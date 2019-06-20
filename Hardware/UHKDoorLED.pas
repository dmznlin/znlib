{*******************************************************************************
  作者: dmzn@163.com 2017-10-18
  描述: 海康威门岗LED引导屏

  备注:
  *.支持型号: IS-TVL224
*******************************************************************************}
unit UHKDoorLED;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, NativeXml, UHKDoorLED_Head, UWaitItem,
  ULibFun, USysLoger;

type
  PHKCardParam = ^THKCardParam;
  THKCardParam = record
    FID        : string;            //标识
    FName      : string;            //名称
    FHost      : string;            //IP
    FPort      : Integer;           //端口
    
    FColor     : Integer;           //颜色:1,单色;2,双色
    FWidth     : Integer;           //宽
    FHeight    : Integer;           //高

    FTextKeep  : Integer;           //保持时间
    FTextSpeed : Integer;           //播放速度
    FTextSend  : Int64;             //发送时间
    FDefaltTxt : string;            //默认内容
  end;

  THKDataDisplay = (ddNeiMa, ddLnTXT);
  //显示模式: 内码,单行文本

  PHKCardData = ^THKCardData;
  THKCardData = record
    FCard      : string;            //卡标识
    FText      : string;            //内容
    FColor     : Integer;           //颜色
    FDisplay   : THKDataDisplay;    //显示模式
    FDefault   : Boolean;           //默认标识
    FEnabled   : Boolean;           //有效标识
  end;

  THKCardManager = class;
  THKCardSender = class(TThread)
  private
    FOwner: THKCardManager;
    //拥有者
    FWaiter: TWaitObject;
    //等待对象
  protected
    procedure Execute; override;
    procedure Doexecute;
    //执行线程
    procedure SendCardData(const nCard: PHKCardParam; const nData: PHKCardData);
    //发送数据
  public
    constructor Create(AOwner: THKCardManager);
    destructor Destroy; override;
    //创建释放
    procedure WakupMe;
    //唤醒线程
    procedure StopMe;
    //停止线程
  end;

  THKCardManager = class(TObject)
  private
    FEnabled: Boolean;
    //是否启用
    FCards: array of THKCardParam;
    //卡列表
    FTextData: array of THKCardData;
    //待显内容
    FSender: THKCardSender;
    //发送线程
    FSyncLock: TCriticalSection;
    //同步锁定
  protected
    procedure InitDataBuffer;
    //初始缓存
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure LoadConfig(const nFile: string);
    //读取配置
    procedure StartDiaplay;
    procedure StopDiaplay;
    //启停发送
    procedure GetCardList(const nList: TStrings);
    //获取列表
    procedure DisplayText(const nCard,nText: string; const nColor: Integer);
    //显示文本
  end;

var
  gHKCardManager: THKCardManager = nil;
  //全局使用

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(THKCardManager, '海康威视LED屏', nEvent);
end;

constructor THKCardManager.Create;
begin
  FEnabled := True;
  FSender := nil;

  InitDataBuffer;
  FSyncLock := TCriticalSection.Create;
end;

destructor THKCardManager.Destroy;
begin
  StopDiaplay;
  FSyncLock.Free;
  inherited;
end;

procedure THKCardManager.StartDiaplay;
begin
  if not FEnabled then Exit;
  //xxxxx
  
  if Length(FCards) < 1 then
    raise Exception.Create('Display Card List Is Null.');
  //xxxxx

  if not Assigned(FSender) then
    FSender := THKCardSender.Create(Self);
  FSender.WakupMe;
end;

procedure THKCardManager.StopDiaplay;
begin
  if Assigned(FSender) then
    FSender.StopMe;
  //xxxxx
  
  FSender := nil;
  InitDataBuffer;
end;

//Date: 2017-10-18
//Desc: 初始化内容缓冲
procedure THKCardManager.InitDataBuffer;
var nIdx: Integer;
begin
  SetLength(FTextData, 10);
  for nIdx:=Low(FTextData) to High(FTextData) do
    FTextData[nIdx].FEnabled := False;
  //xxxx
end;

//Date: 2017-10-18
//Parm: 卡标识;内容;颜色
//Desc: 在nCard上显示颜色为nColor的nText内容
procedure THKCardManager.DisplayText(const nCard, nText: string;
  const nColor: Integer);
var nIdx,nInt: Integer;
begin
  if not FEnabled then Exit;
  //xxxxx

  if not Assigned(FSender) then
    raise Exception.Create('Card Sender Should Start First.');
  //xxxxx

  FSyncLock.Enter;
  try
    for nIdx:=Low(FTextData) to High(FTextData) do
     with FTextData[nIdx] do
      if FEnabled and (FCard = nCard) then
      begin
        FText := nText;
        FColor := nColor;

        FSender.WakupMe;
        Exit;
      end; //同卡内容合并

    nInt := -1;
    for nIdx:=Low(FTextData) to High(FTextData) do
    if not FTextData[nIdx].FEnabled then
    begin
      nInt := nIdx;
      Break;
    end; //未使用

    if nInt < 0 then
    begin
      nInt := Length(FTextData);
      SetLength(FTextData, nInt + 1);
    end;

    with FTextData[nInt] do
    begin 
      FCard := nCard;
      FText := nText;
      FColor := nColor;

      FEnabled := True;
      FDisplay := ddNeiMa;
      FSender.WakupMe;
    end;
  finally
    FSyncLock.Leave;
  end;   
end;

//Date: 2017-10-18
//Parm: 列表
//Desc: 获取可用卡标识列表
procedure THKCardManager.GetCardList(const nList: TStrings);
var nIdx: Integer;
begin
  nList.Clear;
  for nIdx:=Low(FCards) to High(FCards) do
   with FCards[nIdx] do
    nList.Values[FID] := FName;
  //xxxxx
end;

procedure THKCardManager.LoadConfig(const nFile: string);
var nIdx,nInt: Integer;
    nXML: TNativeXml;
    nNode,nTmp: TXmlNode;
begin
  nXML := TNativeXml.Create;
  try
    SetLength(FCards, 0);
    nXML.LoadFromFile(nFile);
    nTmp := nXML.Root.FindNode('config');

    if Assigned(nTmp) then
    begin
      nIdx := nTmp.NodeByName('enable').ValueAsInteger;
      FEnabled := nIdx = 1;
    end;

    nTmp := nXML.Root.FindNode('cards');
    if Assigned(nTmp) then
    begin
      for nIdx:=0 to nTmp.NodeCount - 1 do
      begin
        nNode := nTmp.Nodes[nIdx];
        if nNode.NodeByName('enable').ValueAsInteger <> 1 then Continue;

        nInt := Length(FCards);
        SetLength(FCards, nInt + 1);

        with FCards[nInt] do
        begin
          FID := nNode.AttributeByName['id'];
          FName := nNode.AttributeByName['name'];

          FHost := nNode.NodeByName('ip').ValueAsString;
          FPort := nNode.NodeByName('port').ValueAsInteger;

          FColor := nNode.NodeByName('color').ValueAsInteger;
          FWidth := nNode.NodeByName('width').ValueAsInteger;
          FHeight := nNode.NodeByName('height').ValueAsInteger;

          FTextSend := GetTickCount();
          FTextKeep := nNode.NodeByName('textkeep').ValueAsInteger;
          FTextSpeed := nNode.NodeByName('textspeed').ValueAsInteger;
          FDefaltTxt := nNode.NodeByName('default').ValueAsString;
        end;
      end;
    end;
  finally
    nXML.Free;
  end;
end;

//------------------------------------------------------------------------------
constructor THKCardSender.Create(AOwner: THKCardManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FWaiter := TWaitObject.Create;
  FWaiter.Interval := 1000;
end;

destructor THKCardSender.Destroy;
begin
  FWaiter.Free;
  inherited;
end;

procedure THKCardSender.WakupMe;
begin
  FWaiter.Wakeup;
end;

procedure THKCardSender.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure THKCardSender.Execute;
begin
  while not Terminated do
  try
    FWaiter.EnterWait;
    if Terminated then Exit;
    Doexecute;
  except
    on nErr: Exception do
    begin
      WriteLog(nErr.Message);
    end;
  end;
end;

procedure THKCardSender.Doexecute;
var nIdx: Integer;
    nData: THKCardData;
begin
  while not Terminated do
  with FOwner do
  begin
    FSyncLock.Enter;
    try
      nData.FEnabled := False;
      //default flag
      
      for nIdx:=Low(FTextData) to High(FTextData) do
      if FTextData[nIdx].FEnabled then
      begin
        nData := FTextData[nIdx];
        nData.FDefault := False;

        FTextData[nIdx].FEnabled := False;
        Break;
      end; //获取待发送内容
    finally
      FSyncLock.Leave;
    end;

    if not nData.FEnabled then
    begin
      for nIdx:=Low(FCards) to High(FCards) do
      with FCards[nIdx] do
      begin
        if (FTextSend = 0) or
           (GetTickCountDiff(FTextSend) < FTextKeep * 1000) then Continue;
        FTextSend := 0;

        with nData do
        begin
          FCard := FID;
          FText := FDefaltTxt;
          FColor := 2;
          FDisplay := ddLnTXT;
          
          FDefault := True;
          FEnabled := True;
        end;

        Break;
      end;
    end; //发送默认内容

    if not nData.FEnabled then Exit;
    //no data

    for nIdx:=Low(FCards) to High(FCards) do
    if CompareText(FCards[nIdx].FID, nData.FCard) = 0 then
    begin
      SendCardData(@FCards[nIdx], @nData);
      nData.FEnabled := False; //send flag
      
      nData.FText := '';
      Break;
    end;

    if nData.FEnabled then
    begin
      nData.FText := '屏卡[ %s ]不存在,显示内容已丢弃.';
      WriteLog(Format(nData.FText, [nData.FCard]));
    end;
  end;
end;

//Date: 2017-10-18
//Parm: 屏卡索引;显示内容
//Desc: 在nCard上显示nData内容
procedure THKCardSender.SendCardData(const nCard: PHKCardParam;
  const nData: PHKCardData);
var nErr: string;
    nFont,nTxt: WideString;
    nInt,nHandle,nColor: Integer;
begin
  nErr := '向[ %s.%s ]发送内容失败';
  nErr := Format(nErr, [nCard.FID, nCard.FName]);
  nErr := nErr + '(call %s return %d).';

  nHandle := -1;
  try
    nHandle := StartSend();
    if nHandle < 1 then
    begin
      WriteLog(Format(nErr, ['StartSend', nHandle]));
      Exit;
    end;

    nInt := SetTransMode(nHandle, 1, 0, 2, 1);
    if nInt <> 1 then
    begin
      WriteLog(Format(nErr, ['SetTransMode', nInt]));
      Exit;
    end;

    nTxt := nCard.FHost;
    nInt := SetNetworkPara(nHandle, 1, PWideChar(nTxt));
    if nInt <> 1 then
    begin
      WriteLog(Format(nErr, ['SetNetworkPara', nInt]));
      Exit;
    end;

    nInt := SendScreenPara(nHandle, nCard.FColor, nCard.FWidth, nCard.FHeight);
    if nInt <> 1 then
    begin
      WriteLog(Format(nErr, ['SendScreenPara', nInt]));
      Exit;
    end;

    nInt := AddControl(nHandle, 1, nCard.FColor);
    if nInt <> 1 then
    begin
      WriteLog(Format(nErr, ['AddControl', nInt]));
      Exit;
    end;

    nInt := AddProgram(nHandle, 1, 0);
    if nInt <> 1 then
    begin
      WriteLog(Format(nErr, ['AddProgram', nInt]));
      Exit;
    end;

    if nData.FDisplay = ddNeiMa then
    begin
      nTxt := nData.FText;
      nInt := AddNeiMaTxtArea1(nHandle, 1, 1, 0, 0, nCard.FWidth, nCard.FHeight,
              PWideChar(nTxt), 16, 0, nData.FColor, 1, 255, nCard.FTextSpeed, 255);
      //xxxxx

      if nInt <> 1 then
      begin
        WriteLog(Format(nErr, ['AddNeiMaTxtArea1', nInt]));
        Exit;
      end;
    end else

    if nData.FDisplay = ddLnTXT then
    begin
      nFont := '宋体';
      nTxt := nData.FText;

      case nData.FColor of
       1: nColor := 255;      //红色
       2: nColor := 65535     //绿色
       else nColor := 65280;  //黄色
      end;

      nInt := AddLnTxtString(nHandle, 1, 1, 0, 0, nCard.FWidth, nCard.FHeight,
              PWideChar(nTxt), PWideChar(nFont), 12, nColor, 0, 0, 0, 32, 20, 0);
      //xxxxx

      if nInt <> 1 then
      begin
        WriteLog(Format(nErr, ['AddLnTxtString', nInt]));
        Exit;
      end;
    end;

    nInt := SendControl(nHandle, 1, 0);
    if nInt <> 1 then
    begin
      WriteLog(Format(nErr, ['SendControl', nInt]));
      Exit;
    end;

    if not nData.FDefault then
      nCard.FTextSend := GetTickCount;
    //发送计时
  finally
    if nHandle > 0 then
      EndSend(nHandle);
    //close handle
  end;
end;

initialization
  gHKCardManager := nil;
finalization
  FreeAndNil(gHKCardManager);
end.
