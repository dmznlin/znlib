{*******************************************************************************
  作者: dmzn@163.com 2019-09-25
  描述: 阿里图像识别接口驱动

  备注:
  *.驱动输出:
    1.车辆进入和离开地磅事件.
    2.车辆是否单车完全上磅检测.
    3.当前在地磅上的有效车牌号.
*******************************************************************************}
unit UMgrAliVision;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, IdSocketHandle, IdGlobal, IdStack,
  IdUDPBase, IdUDPServer, NativeXml, superobject, UWaitItem, ULibFun,
  USysLoger;

type
  TTruckState = (tsNone, tsNewOn, tsLeave, tsNormal, tsOut);
  //车辆状态:无车,新车上磅,车辆下磅,单车完全上磅,车辆越界(未完全上磅,多车)
  TTruckStates = set of TTruckState;

  TPoundLocation = record
    FLeftTop: TPoint;
    FLeftBottom: TPoint;
    FRightTop: TPoint;
    FRightBottom: TPoint;                         //地磅四角坐标
  end;

  TLineLocation = record
    FStart: TPoint;
    FEnd: TPoint;                                 //检测线起始、结束坐标
  end;

  PCameraItem = ^TCameraItem;
  TCameraItem = record
    FChannel: string;                             //通道编号
    FName: string;                                //相机名称
    FReader: string;                              //对应读卡器

    FResolution: TPoint;                          //解析度(1920x1080)
    FPoundLoc: TPoundLocation;                    //地磅坐标
    FLineLoc: TLineLocation;                      //检测线坐标

    {*以下为图像识别系统上报数据*}
    FTruck: string;                               //当前车牌
    FTruckUpdate: Cardinal;                       //数据更新
    FState: TTruckState;                          //当前状态
    FStateUpdate: Cardinal;                       //数据更新
  end;
  TCameraItems = array of TCameraItem;            //相机列表

  PPoundItem = ^TPoundItem;
  TPoundItem = record
    FID: string;                                  //通道标识
    FName: string;                                //通道名称
    FTruck: string;                               //当前车辆
    FTruckPrev: string;                           //上一辆车

    FLastIn: Cardinal;                            //上次进入
    FLastOut: Cardinal;                           //上次退出
    FStateNow: TTruckState;                       //当前状态
    FStateNew: TTruckStates;                      //更新状态

    FValStr: string;
    FValInt: Integer;
    FValFloat: Double;                            //辅助数据
    FCameras: TCameraItems;                       //相机列表
  end;
  TPoundtems = array of TPoundItem;               //磅站列表

  TTruckManager = class;
  TTruckMonitor = class(TThread)
  private
    FOwner: TTruckManager;
    //拥有者
    FWaiter: TWaitObject;
    //等待对象
  protected
    procedure Execute; override;
    procedure DoExecute;
    //执行业务
  public
    constructor Create(AOwner: TTruckManager);
    destructor Destroy; override;
    //创建释放
    procedure Wakeup;
    procedure StopMe;
    //启停通道
  end;

  TOnTruckStatusProc = procedure (const nPound: PPoundItem);
  TOnTruckStatusEvent = procedure (const nPound: PPoundItem) of Object;

  TTruckManager = class(TObject)
  private
    FPounds: TPoundtems;
    //地磅列表
    FMonitor: TTruckMonitor;
    //守护服务
    FStatusUpdateInerval: Word;
    //状态间隔
    FUDPPort: Word;
    FUDPServer: TIdUDPServer;
    //UDP服务
    FSyncLock: TCriticalSection;
    //同步锁定
    FOnStatusProc: TOnTruckStatusProc;
    FOnStatusEvent: TOnTruckStatusEvent;
    ///事件相关
  protected
    function FindCamera(const nChannel: string; var nPound,nCamera: Integer): Boolean;
    //检索数据
    procedure DoUDPRead(AThread: TIdUDPListenerThread;
      AData: TIdBytes; ABinding: TIdSocketHandle);
    //UDP上报
    function AdjustTruck(const nTruck: string): string;
    //车牌校正
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure LoadConfig(const nFile: string);
    //载入配置
    procedure StartService;
    procedure StopService;
    //启停服务
    function GetPoundData(const nID: string; var nData: TPoundItem): Boolean;
    procedure SetPoundData(const nID: string; const nData: PPoundItem = nil);
    //获取数据
    property UDPPort: Word read FUDPPort write FUDPPort;
    property OnStatusChange: TOnTruckStatusProc read FOnStatusProc write FOnStatusProc;
    property OnStatusChangeEvent: TOnTruckStatusEvent read FOnStatusEvent write FOnStatusEvent;
    //属性相关
  end;

var
  gVisionManager: TTruckManager = nil;
  //全集使用

implementation

type
  TProvinceName = record
    FName: string;
    FPinYin: string;
  end;

const
  cTruckNull = '0'; //空车牌

  cTruckProvince: array[0..32] of TProvinceName = (
    (FName: '皖'; FPinYin: 'Anhui'),
    (FName: '京'; FPinYin: 'Beijing'),
    (FName: '渝'; FPinYin: 'Chongqing'),
    (FName: '闽'; FPinYin: 'Fujian'),
    (FName: '甘'; FPinYin: 'Gansu'),
    (FName: '粤'; FPinYin: 'Guangdong'),
    (FName: '桂'; FPinYin: 'Guangxi'),
    (FName: '黔'; FPinYin: 'Guizhou'),
    (FName: '琼'; FPinYin: 'Hainan'),
    (FName: '冀'; FPinYin: 'Hebei'),
    (FName: '黑'; FPinYin: 'Heilongjiang'),
    (FName: '豫'; FPinYin: 'Henan'),
    (FName: '港'; FPinYin: 'HongKong'),
    (FName: '鄂'; FPinYin: 'Hubei'),
    (FName: '湘'; FPinYin: 'Hunan'),
    (FName: '蒙'; FPinYin: 'InnerMongolia'),
    (FName: '苏'; FPinYin: 'Jiangsu'),
    (FName: '赣'; FPinYin: 'Jiangxi'),
    (FName: '吉'; FPinYin: 'Jilin'),
    (FName: '辽'; FPinYin: 'Liaoning'),
    (FName: '澳'; FPinYin: 'Macau'),
    (FName: '宁'; FPinYin: 'Ningxia'),
    (FName: '青'; FPinYin: 'Qinghai'),
    (FName: '陕'; FPinYin: 'Shaanxi'),
    (FName: '鲁'; FPinYin: 'Shandong'),
    (FName: '沪'; FPinYin: 'Shanghai'),
    (FName: '晋'; FPinYin: 'Shanxi'),
    (FName: '川'; FPinYin: 'Sichuan'),
    (FName: '津'; FPinYin: 'Tianjin'),
    (FName: '藏'; FPinYin: 'Tibet'),
    (FName: '新'; FPinYin: 'Xinjiang'),
    (FName: '滇'; FPinYin: 'Yunnan'),
    (FName: '浙'; FPinYin: 'Zhejiang')
  );

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TTruckManager, 'AliVision', nEvent);
end;

constructor TTruckManager.Create;
begin
  FMonitor := nil;
  FUDPServer := nil;
  FStatusUpdateInerval := 2 * 1000;

  SetLength(FPounds, 0);
  FSyncLock := TCriticalSection.Create;
end;

destructor TTruckManager.Destroy;
begin
  StopService;
  if Assigned(FUDPServer) then
    FreeAndNil(FUDPServer);
  //xxxxx

  FSyncLock.Free;
  inherited;
end;

procedure TTruckManager.StartService;
begin
  if not Assigned(FUDPServer) then
  begin
    FUDPServer := TIdUDPServer.Create;
    FUDPServer.OnUDPRead := DoUDPRead;
  end;

  FUDPServer.Active := False;
  FUDPServer.DefaultPort := FUDPPort;
  FUDPServer.Active := True;

  if not Assigned(FMonitor) then
    FMonitor := TTruckMonitor.Create(Self);
  //xxxxx
end;

procedure TTruckManager.StopService;
begin
  if Assigned(FMonitor) then
    FMonitor.StopMe;
  FMonitor := nil;

  if Assigned(FUDPServer) then
    FUDPServer.Active := False;
  //xxxxx
end;

//Date: 2019-09-26
//Parm: 通道编号;磅站,相机
//Desc: 检索指定相机
function TTruckManager.FindCamera(const nChannel: string; var nPound,
  nCamera: Integer): Boolean;
var i,nIdx: Integer;
begin
  Result := False;
  //init
  
  for i:=Low(FPounds) to High(FPounds) do
   for nIdx:=Low(FPounds[i].FCameras) to High(FPounds[i].FCameras) do
    if CompareText(nChannel, FPounds[i].FCameras[nIdx].FChannel) = 0 then
    begin
      nPound := i;
      nCamera := nIdx;

      Result := True;
      Exit;
    end;
end;

//Date: 2019-11-05
//Parm: 标识
//Desc: 获取nID的当前数据
function TTruckManager.GetPoundData(const nID: string;
  var nData: TPoundItem): Boolean;
var nIdx: Integer;
begin
  Result := False;
  try
    FSyncLock.Enter;
    //lock first

    for nIdx:=Low(FPounds) to High(FPounds) do
     with FPounds[nIdx] do
      if CompareText(nID, FID) = 0 then
      begin
        nData := FPounds[nIdx];
        if nData.FTruck = cTruckNull then
          nData.FTruck := '';
        //xxxxx

        if nData.FTruckPrev = cTruckNull then
          nData.FTruckPrev := '';
        //xxxxxx

        Result := True;
        Break;
      end;
  finally
    FSyncLock.Leave;
  end;   
end;

//Date: 2019-11-05
//Parm: 地磅标识
//Desc: 设置nID的辅助数据
procedure TTruckManager.SetPoundData(const nID: string; const nData: PPoundItem);
var nIdx: Integer;
begin
  try
    FSyncLock.Enter;
    //lock first

    for nIdx:=Low(FPounds) to High(FPounds) do
     with FPounds[nIdx] do
      if CompareText(nID, FID) = 0 then
      begin
        if Assigned(nData) then
        begin
          FValStr := nData.FValStr;
          FValInt := nData.FValInt;
          FValFloat := nData.FValFloat;
        end else
        begin
          FValStr := '';
          FValInt := 0;
          FValFloat := 0;
        end;

        Break;
      end;
  finally
    FSyncLock.Leave;
  end;
end;

procedure TTruckManager.DoUDPRead(AThread: TIdUDPListenerThread;
  AData: TIdBytes; ABinding: TIdSocketHandle);
var nStr: string;
    nPound,nCamera: Integer;
    nRoot,nNode: ISuperObject;
begin
  try
    nStr := BytesToString(AData, Indy8BitEncoding);
    nRoot := SO(nStr);
    nNode := nRoot.R['channel'];

    if not FindCamera(nNode.AsString, nPound, nCamera) then
    begin
      nStr := '通道编号为[ %s ]的相机不存在';
      raise Exception.Create(Format(nStr, [nNode.AsString]));
    end;

    with FPounds[nPound] do
    try
      FSyncLock.Enter;
      nNode := nRoot.O['lr_result'];

      if Assigned(nNode) then //车牌识别
      begin
        nStr := UpperCase(Trim(nNode.AsString));
        if nStr <> cTruckNull then
        begin
          FCameras[nCamera].FTruck := AdjustTruck(nStr);
          FCameras[nCamera].FTruckUpdate := GetTickCount();

          //WriteLog('Truck: ' + nStr);
        end;
      end;

      nNode := nRoot.O['pd_result'];
      if Assigned(nNode) then //过磅检测
      begin
        nStr := nNode.AsString;
        if nStr = '0' then                        //无车
             FCameras[nCamera].FState := tsNone
        else if nStr = '1' then
             FCameras[nCamera].FState := tsNormal //有车且不越界
        else FCameras[nCamera].FState := tsOut;   //越界

        FCameras[nCamera].FStateUpdate := GetTickCount();

        //WriteLog('Status: ' + nStr);
      end;
    finally
      FSyncLock.Leave;
    end;
  except
    on nErr: Exception do
    begin
      WriteLog(nErr.Message);
    end;
  end;
end;

//Date: 2019-11-05
//Parm: 车牌号
//Desc: 校正nTruck中的特定字符
function TTruckManager.AdjustTruck(const nTruck: string): string;
var nIdx: Integer;
begin
  for nIdx:=Low(cTruckProvince) to High(cTruckProvince) do
  with cTruckProvince[nIdx] do
  begin
    if Pos(UpperCase(FPinYin), nTruck) > 0 then
    begin
      Result := StringReplace(nTruck, '<' + FPinYin + '>', FName, [rfIgnoreCase]);
      Exit;
    end;
  end;

  Result := nTruck;
end;

procedure TTruckManager.LoadConfig(const nFile: string);
var nStr: string;
    i,nIdx,nInt: Integer;
    nXML: TNativeXml;
    nRoot,nNode: TXmlNode;

    function ParsePoint(var nPT: TPoint; const nVal: string): Boolean;
    var nPos: Integer;
    begin
      Result := False;
      try
        nPT := Point(0, 0);
        nPos := Pos(',', nVal);
        if nPos < 2 then Exit;

        nStr := Copy(nVal, 1, nPos - 1);
        if not IsNumber(nStr, False) then Exit;
        nPT.X := StrToInt(nStr);

        nStr := Copy(nVal, nPos + 1, Length(nVal) - nPos);
        if not IsNumber(nStr, False) then Exit;
        nPT.Y := StrToInt(nStr);
        
        Result := True;
      finally
        if not Result then
        begin
          nStr := '相机[ %s,%s ]坐标配置无效';
          with FPounds[nInt].FCameras[i] do
            raise Exception.Create(Format(nStr, [FChannel, FName, nStr]));
          //xxxxx
        end;
      end;
    end;
begin
  nXML := TNativeXml.Create;
  try
    nXML.LoadFromFile(nFile);
    //load config
    nRoot := nXML.Root.NodeByNameR('config');
    FUDPPort := nRoot.NodeByNameR('udp_port').ValueAsInteger;
    FStatusUpdateInerval := nRoot.NodeByNameR('status_timeout').ValueAsInteger;

    nRoot := nXML.Root.NodeByNameR('pounds');
    SetLength(FPounds, nRoot.NodeCount);
    nInt := 0;

    for nIdx:=0 to nRoot.NodeCount - 1 do
    begin
      with FPounds[nInt], nRoot.Nodes[nIdx] do
      begin
        FID := AttributeByName['id'];
        FName := AttributeByName['name'];
        SetLength(FCameras, 0);

        FTruck := cTruckNull;
        FTruckPrev := '';
        FLastIn := 0;
        FLastOut := 0;

        FStateNew := [];
        FStateNow := tsNone;
      end;

      Inc(nInt);
    end;

    nRoot := nXML.Root.NodeByNameR('cameras');
    for nIdx:=0 to nRoot.NodeCount - 1 do
    with nRoot.Nodes[nIdx] do
    begin
      nInt := -1;
      nStr := NodeByNameR('pound').ValueAsString;

      for i:=Low(FPounds) to High(FPounds) do
      if CompareText(nStr, FPounds[i].FID) = 0 then
      begin
        nInt := i;
        Break;
      end;

      if nInt < 0 then
      begin
        nStr := '相机[ %s,%s ]所在磅站[ %s ]不存在';
        raise Exception.Create(Format(nStr, [AttributeByName['channel'],
         AttributeByName['name'], nStr]));
      end;

      i := Length(FPounds[nInt].FCameras);
      SetLength(FPounds[nInt].FCameras, i + 1);
      //new camera item

      with FPounds[nInt].FCameras[i] do
      begin
        FChannel := AttributeByName['channel'];
        FName := AttributeByName['name'];
        FReader := NodeByNameR('reader').ValueAsString;

        ParsePoint(FResolution, NodeByNameR('resolution').ValueAsString);
        nNode := NodeByNameR('lineloc');
        ParsePoint(FLineLoc.FStart, nNode.AttributeByName['start']);
        ParsePoint(FLineLoc.FEnd,   nNode.AttributeByName['end']);

        nNode := NodeByNameR('poundloc');
        ParsePoint(FPoundLoc.FLeftTop,     nNode.AttributeByName['LT']);
        ParsePoint(FPoundLoc.FLeftBottom,  nNode.AttributeByName['LB']);
        ParsePoint(FPoundLoc.FRightTop,    nNode.AttributeByName['RT']);
        ParsePoint(FPoundLoc.FRightBottom, nNode.AttributeByName['RB']);
      end;
    end;
  finally
    nXML.Free;
  end;
end;

//------------------------------------------------------------------------------
constructor TTruckMonitor.Create(AOwner: TTruckManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FWaiter := TWaitObject.Create();
  FWaiter.Interval := 500;
end;

destructor TTruckMonitor.Destroy;
begin
  FreeAndNil(FWaiter);
  inherited;
end;

procedure TTruckMonitor.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure TTruckMonitor.Wakeup;
begin
  FWaiter.Wakeup();
end;

procedure TTruckMonitor.Execute;
var nIdx,nInt: Integer;
    nTruckNormal,nTruckExists: Boolean;
begin
  while not Terminated do
  try
    FWaiter.EnterWait;
    if Terminated then Exit;

    with FOwner do
    try
      FSyncLock.Enter;
      //lock first

      for nInt:=Low(FPounds) to High(FPounds) do
      with FPounds[nInt] do
      begin
        if Length(FCameras) < 1 then Continue;
        //未配置相机 或 没车牌
        
        FStateNew := [];
        nTruckNormal := True;
        nTruckExists := False;

        for nIdx:=Low(FCameras) to High(FCameras) do
        begin
          if GetTickCountDiff(FCameras[nIdx].FStateUpdate) > FStatusUpdateInerval then
            FCameras[nIdx].FState := tsNone;
          //更新超时视为无效

          if FCameras[nIdx].FState = tsOut then
          begin
            nTruckNormal := False;
            //未完全上磅
            FStateNew := FStateNew + [tsOut] - [tsNormal];
            //车辆越界
          end;

          if (FCameras[nIdx].FState <> tsNone) or
             (GetTickCountDiff(FCameras[nIdx].FTruckUpdate) < FStatusUpdateInerval) then
          begin
            nTruckExists := True;
            //车辆在磅上,或车牌号刚更新
          end;
        end;

        if not nTruckExists then
        begin
          FTruckPrev := FTruck;
          FTruck := cTruckNull;
          FLastOut := GetTickCount();
          FStateNew := FStateNew + [tsLeave];
        end;

        if nTruckNormal then
        begin
          FStateNew := FStateNew + [tsNormal];
          //车辆完全在磅
        end;

        for nIdx:=Low(FCameras) to High(FCameras) do
        begin
          if (FCameras[nIdx].FTruck <> cTruckNull) and
             (GetTickCountDiff(FCameras[nIdx].FTruckUpdate) < FStatusUpdateInerval) then
          begin
            if FCameras[nIdx].FTruck <> FTruck then
            begin
              FTruck := FCameras[nIdx].FTruck;
              FLastIn := GetTickCount();
              FStateNew := FStateNew + [tsNewOn];
              //新车上磅
            end;
          end;
        end;
      end;
    finally
      FSyncLock.Leave;
    end;

    Doexecute;
  except
    on E:Exception do
    begin
      WriteLog(E.Message);
    end;
  end;
end;

procedure TTruckMonitor.DoExecute;
var nIdx: Integer;

    //触发事件
    procedure DoEvent;
    begin
      if Assigned(FOwner.FOnStatusProc) then
        FOwner.FOnStatusProc(@FOwner.FPounds[nIdx]);
      //xxxxx

      if Assigned(FOwner.FOnStatusEvent) then
        FOwner.FOnStatusEvent(@FOwner.FPounds[nIdx]);
      //xxxxx
    end;
begin
  for nIdx:=Low(FOwner.FPounds) to High(FOwner.FPounds) do
  with FOwner, FOwner.FPounds[nIdx] do
  begin
    if FStateNew = [] then Continue;
    //no event,no truck

    if (tsNewOn in FStateNew) and (FStateNow <> tsNewOn) then //新车上磅
    begin
      FStateNow := tsNewOn;
      if FTruck <> cTruckNull then
        DoEvent;
      //xxxxx
    end;

    if (tsLeave in FStateNew) and (FStateNow <> tsLeave) then //车辆离磅
    begin
      FStateNow := tsLeave;
      if FTruckPrev <> cTruckNull then
        DoEvent;
      //xxxxx
    end;

    if (tsNormal in FStateNew) and (FStateNow <> tsNormal) then //完全上磅
    begin
      FStateNow := tsNormal;
      if FTruck <> cTruckNull then
        DoEvent;
      //xxxxx
    end;

    if (tsOut in FStateNew) and (FStateNow <> tsOut) then //未完全上磅
    begin
      FStateNow := tsOut;
      if FTruck <> cTruckNull then
        DoEvent;
      //xxxxx
    end;
  end;
end;

initialization
  gVisionManager := nil;
finalization
  FreeAndNil(gVisionManager);
end.
