{*******************************************************************************
  作者: dmzn@163.com 2019-10-09
  描述: 万集激光器WLR-711驱动
*******************************************************************************}
unit UMgrWJLaser;

{.$DEFINE DEBUG}
interface

uses
  Windows, Classes, SysUtils, NativeXml, IdTCPConnection, IdTCPClient, IdGlobal,
  SyncObjs, UWaitItem, ULibFun, USysLoger;

const
  cLaserDataMax             = 720 * 2;
  //雷达可扫描180度,分辨率为0.5时,共360个点,每个点2个字节,共计720个字节.
  //当分辨率为0.25时,数据为720 x 2
  cLaserDataParse           = Trunc(cLaserDataMax / 2);

  cWJLaserMaxThread         = 10;
  //最大线程数

  cWJLaser_Wait_Short       = 2 * 1000;
  cWJLaser_Wait_Long        = 5 * 1000;       //线程等待

type
  TWJLaserData = packed record
    FHead        : array[0..1] of Byte;       //帧头
    FLength      : array[0..1] of Byte;       //有效长度(除去帧头,帧尾)
    FSerial      : array[0..1] of Byte;       //0-65535依次累加
    FTimeStamp   : Integer;                   //时间戳
    FVerifyType  : Byte;                      //校验类型
    FrameType    : Byte;                      //帧类型
    FDeviceType  : array[0..1] of Byte;       //设备类型
    FReserved    : array[0..7] of Byte;       //预留字节
    FCommand     : Byte;                      //主指令号
    FSubCmd      : Byte;                      //子指令号
    FParams      : array[0..1] of Byte;       //命令参数
    FData        : array[0..cLaserDataMax-1] of Byte;//数据
    FVerify      : array[0..1] of Byte;       //校验值
    FFrameEnd    : array[0..1] of Byte;       //帧尾
  end;

const
  cSize_WJLaserData = SizeOf(TWJLaserData);   //数据大小

type
  TWJLaserMatrix = record
    FAngle       : Single;                    //角度
    FDistance    : Word;                      //直线距离
    Fx           : Word;                      //水平距离
    Fy           : Word;                      //垂直距离
    FHigh        : Word;                      //离地距离
  end;

  PWJLaserHost = ^TWJLaserHost;
  TWJLaserHost = record
    FID          : string;                    //标识
    FName        : string;                    //名称
    FHost        : string;                    //地址
    FPort        : Integer;                   //端口
    FTunnel      : string;                    //通道号
    FEnable      : Boolean;                   //是否启用
    FLocked      : Boolean;                   //是否锁定
    FLastActive  : Cardinal;                  //上次活动

    FHighLaser        : Cardinal;             //激光器距离地面(安装)高度
    FHighUnderLaser   : Cardinal;             //激光器正下方高度
    FHighFrontLaser   : Cardinal;             //激光器正下方车头方向高度
    FHighBackLaser    : Cardinal;             //激光器正下方车尾方向高度
    FOffsetFrontLaser : Integer;              //激光器正下方向车头偏移量
    FOffsetBackLaser  : Integer;              //激光器正下方向车尾偏移量
    FTruckHighMin     : Cardinal;             //车厢高度(最小)
    FTruckHigh        : Cardinal;             //当前车高
    FTruckLong        : Cardinal;             //当前车长
    
    FClient      : TIdTCPClient;              //通信链路
    FSerial      : Word;                      //帧序列号
    FData        : TWJLaserData;              //原始数据
    FDataParse   : array[0..cLaserDataParse-1] of TWJLaserMatrix; //解析后数据
    FOptions     : TStrings;                  //附加选项
  end;
  TWJLaserHosts = array of TWJLaserHost;

  TWJLaserThreadType = (ttAll, ttActive);
  //线程模式: 全能;只读活动

  TWJLaserManager = class;
  TWJLaserReader = class(TThread)
  private
    FOwner: TWJLaserManager;
    //拥有者
    FWaiter: TWaitObject;
    //等待对象
    FSingleRequst: TWJLaserData;
    //单帧获取数据
    FActiveHost: PWJLaserHost;
    //当前读头
    FThreadType: TWJLaserThreadType;
    //线程模式
  protected
    procedure DoExecute;
    procedure Execute; override;
    //执行线程
    procedure ScanActiveHost(const nActive: Boolean);
    //扫描可用
    procedure InitSingleRequst;
    //构建指令
    function SendHostCommand(const nHost: PWJLaserHost): Boolean;
    //发送指令
    procedure ParseLaserData(const nHost: PWJLaserHost);
    //解析数据
  public
    constructor Create(AOwner: TWJLaserManager; AType: TWJLaserThreadType);
    destructor Destroy; override;
    //创建释放
    procedure StopMe;
    //停止线程
  end;

  //----------------------------------------------------------------------------
  TWJLaserEventMode = (emThread, emMain);
  //事件模式: 线程;主进程

  TWJLaserProc = procedure (const nItem: PWJLaserHost);
  TWJLaserEvent = procedure (const nItem: PWJLaserHost) of Object;

  TWJLaserManager = class(TObject)
  private
    FEnable: Boolean;
    //是否启用
    FLaserHosts: TList;
    //激光器列表
    FMonitorCount: Integer;
    FThreadCount: Integer;
    //通讯线程数
    FLaserIndex: Integer;
    FLaserActive: Integer;
    //激光器索引
    FSyncLock: TCriticalSection;
    //同步锁定
    FThreads: array[0..cWJLaserMaxThread-1] of TWJLaserReader;
    //通讯对象
    FOnProc: TWJLaserProc;
    FOnEvent: TWJLaserEvent;
    FEventMode: TWJLaserEventMode;
    //事件定义
  protected
    procedure ClearHosts(const nFree: Boolean);
    //清理资源
    procedure CloseHost(const nHost: PWJLaserHost);
    //关闭读头
    function FindHost(const nHost: string): Integer;
    //检索读头
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure LoadConfig(const nFile: string);
    //载入配置
    procedure StartService;
    procedure StopService;
    //启停服务
    property Lasers: TList read FLaserHosts;
    property OnCardProc: TWJLaserProc read FOnProc write FOnProc;
    property OnCardEvent: TWJLaserEvent read FOnEvent write FOnEvent;
    property EventMode: TWJLaserEventMode read FEventMode write FEventMode;
    //属性相关
  end;

var
  gWJLaserManager: TWJLaserManager = nil;
  //全局使用

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TWJLaserManager, '激光装车检测', nEvent);
end;

constructor TWJLaserManager.Create;
var nIdx: Integer;
begin
  FEnable := False;
  FEventMode := emThread;
  FThreadCount := 1;
  FMonitorCount := 1;  

  for nIdx:=Low(FThreads) to High(FThreads) do
    FThreads[nIdx] := nil;
  //xxxxx

  FLaserHosts := TList.Create;
  FSyncLock := TCriticalSection.Create;
end;

destructor TWJLaserManager.Destroy;
begin
  StopService;
  ClearHosts(True);

  FSyncLock.Free;
  inherited;
end;

procedure TWJLaserManager.ClearHosts(const nFree: Boolean);
var nIdx: Integer;
    nItem: PWJLaserHost;
begin
  for nIdx:=FLaserHosts.Count - 1 downto 0 do
  begin
    nItem := FLaserHosts[nIdx];
    nItem.FClient.Free;
    nItem.FClient := nil;

    FreeAndNil(nItem.FOptions);
    Dispose(nItem);
    FLaserHosts.Delete(nIdx);
  end;

  if nFree then
    FLaserHosts.Free;
  //xxxxx
end;

procedure TWJLaserManager.CloseHost(const nHost: PWJLaserHost);
begin
  if Assigned(nHost) and Assigned(nHost.FClient) then
  begin
    nHost.FClient.Disconnect;
    if Assigned(nHost.FClient.IOHandler) then
      nHost.FClient.IOHandler.InputBuffer.Clear;
    //xxxxx
  end;
end;

procedure TWJLaserManager.StartService;
var nIdx,nNum: Integer;
    nType: TWJLaserThreadType;
begin
  if not FEnable then Exit;
  FLaserIndex := 0;
  FLaserActive := 0;

  nNum := 0;
  //init
  
  for nIdx:=Low(FThreads) to High(FThreads) do
  begin
    if (nNum >= FThreadCount) or
       (nNum >= FLaserHosts.Count) then Exit;
    //线程不能超过预定值,或不多余激光器个数

    if nNum < FMonitorCount then
         nType := ttAll
    else nType := ttActive;

    if not Assigned(FThreads[nIdx]) then
      FThreads[nIdx] := TWJLaserReader.Create(Self, nType);
    Inc(nNum);
  end;
end;

procedure TWJLaserManager.StopService;
var nIdx: Integer;
begin
  for nIdx:=Low(FThreads) to High(FThreads) do
   if Assigned(FThreads[nIdx]) then
    FThreads[nIdx].Terminate;
  //设置退出标记

  for nIdx:=Low(FThreads) to High(FThreads) do
  begin
    if Assigned(FThreads[nIdx]) then
      FThreads[nIdx].StopMe;
    FThreads[nIdx] := nil;
  end;

  FSyncLock.Enter;
  try
    for nIdx:=FLaserHosts.Count - 1 downto 0 do
      CloseHost(FLaserHosts[nIdx]);
    //关闭链路
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-10-09
//Parm: 激光器标识
//Desc: 检索nReader的索引
function TWJLaserManager.FindHost(const nHost: string): Integer;
var nIdx: Integer;
begin
  Result := -1;

  for nIdx:=FLaserHosts.Count-1 downto 0 do
  if CompareText(PWJLaserHost(FLaserHosts[nIdx]).FID, nHost) = 0 then
  begin
    Result := nIdx;
    Exit;
  end;
end;

procedure TWJLaserManager.LoadConfig(const nFile: string);
var nIdx: Integer;
    nXML: TNativeXml;
    nHost: PWJLaserHost;
    nDefHost: TWJLaserHost;
    nRoot,nNode,nTmp: TXmlNode;
begin
  FEnable := False;
  if not FileExists(nFile) then Exit;

  nXML := nil;
  try
    nXML := TNativeXml.Create;
    nXML.LoadFromFile(nFile);
    nRoot := nXML.Root.FindNode('config');

    if not Assigned(nRoot) then
      raise Exception.Create('Invalid WJLaser Config File.');
    //xxxxx

    nNode := nRoot.FindNode('enable');
    if Assigned(nNode) then
      Self.FEnable := nNode.ValueAsString <> 'N';
    //xxxxx

    nNode := nRoot.FindNode('thread');
    if Assigned(nNode) then
         FThreadCount := nNode.ValueAsInteger
    else FThreadCount := 1;

    if (FThreadCount < 1) or (FThreadCount > cWJLaserMaxThread) then
      raise Exception.Create('WJLaser Thread-Num Need Between 1-10.');
    //xxxxx

    nNode := nRoot.FindNode('monitor');
    if Assigned(nNode) then
         FMonitorCount := nNode.ValueAsInteger
    else FMonitorCount := 1;

    if (FMonitorCount < 1) or (FMonitorCount > FThreadCount) then
      raise Exception.Create(Format(
        'WJLaser Monitor-Num Need Between 1-%d.', [FThreadCount]));
    //xxxxx

    //--------------------------------------------------------------------------
    nRoot := nXML.Root.FindNode('lasers');
    if not Assigned(nRoot) then Exit;

    FillChar(nDefHost, SizeOf(TWJLaserHost), #0);
    ClearHosts(False);

    for nIdx:=0 to nRoot.NodeCount - 1 do
    begin
      nNode := nRoot.Nodes[nIdx];
      if CompareText(nNode.Name, 'laser') <> 0 then Continue;

      New(nHost);
      FLaserHosts.Add(nHost);
      nHost^ := nDefHost;

      with nNode,nHost^ do
      begin
        FLocked := False;
        FLastActive := GetTickCount;

        FID := AttributeByName['id'];
        FHost := NodeByNameR('ip').ValueAsString;
        FPort := NodeByNameR('port').ValueAsInteger;
        FEnable := NodeByNameR('enable').ValueAsString <> 'N';

        FTunnel := NodeByNameR('tunnel').ValueAsString;
        FHighLaser := NodeByNameR('high').ValueAsInteger;
        FOffsetFrontLaser := NodeByNameR('offsetFront').ValueAsInteger;
        FOffsetBackLaser := NodeByNameR('offsetBack').ValueAsInteger;

        FClient := TIdTCPClient.Create;
        with FClient do
        begin
          Host := FHost;
          Port := FPort;
          ReadTimeout := 3 * 1000;
          ConnectTimeout := 3 * 1000;   
        end;

        nTmp := FindNode('options');
        if Assigned(nTmp) then
        begin
          FOptions := TStringList.Create;
          SplitStr(nTmp.ValueAsString, FOptions, 0, ';');
        end else FOptions := nil;
      end;
    end;
  finally
    nXML.Free;
  end;
end;

//------------------------------------------------------------------------------
constructor TWJLaserReader.Create(AOwner: TWJLaserManager;
  AType: TWJLaserThreadType);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FThreadType := AType;

  FWaiter := TWaitObject.Create;
  FWaiter.Interval := cWJLaser_Wait_Short;
end;

destructor TWJLaserReader.Destroy;
begin       
  FWaiter.Free;
  inherited;
end;

procedure TWJLaserReader.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

//Date: 2019-10-09
//Parm: 活动&不活动激光器
//Desc: 扫描nActive激光器,若可用存入FActiveHost.
procedure TWJLaserReader.ScanActiveHost(const nActive: Boolean);
var nIdx: Integer;
    nReader: PWJLaserHost;
begin
  if nActive then //扫描活动
  with FOwner do
  begin
    if FLaserActive = 0 then
         nIdx := 1
    else nIdx := 0; //从0开始为完整一轮

    while True do
    begin
      if FLaserActive >= FLaserHosts.Count then
      begin
        FLaserActive := 0;
        Inc(nIdx);

        if nIdx >= 2 then Break;
        //扫描一轮,无效退出
      end;

      nReader := FLaserHosts[FLaserActive];
      Inc(FLaserActive);
      if nReader.FLocked or (not nReader.FEnable) then Continue;

      if nReader.FLastActive > 0 then 
      begin
        FActiveHost := nReader;
        FActiveHost.FLocked := True;
        Break;
      end;
    end;
  end else

  with FOwner do //扫描不活动
  begin
    if FLaserIndex = 0 then
         nIdx := 1
    else nIdx := 0; //从0开始为完整一轮

    while True do
    begin
      if FLaserIndex >= FLaserHosts.Count then
      begin
        FLaserIndex := 0;
        Inc(nIdx);

        if nIdx >= 2 then Break;
        //扫描一轮,无效退出
      end;

      nReader := FLaserHosts[FLaserIndex];
      Inc(FLaserIndex);
      if nReader.FLocked or (not nReader.FEnable) then Continue;

      if nReader.FLastActive = 0 then 
      begin
        FActiveHost := nReader;
        FActiveHost.FLocked := True;
        Break;
      end;
    end;
  end;
end;

procedure TWJLaserReader.Execute;
begin
  InitSingleRequst;
  //初始化指令
  
  while not Terminated do
  try
    FWaiter.EnterWait;
    if Terminated then Exit;

    FActiveHost := nil;
    try
      DoExecute;
    finally
      if Assigned(FActiveHost) then
      begin
        FOwner.FSyncLock.Enter;
        FActiveHost.FLocked := False;
        FOwner.FSyncLock.Leave;
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLog(E.Message);
      Sleep(500);
    end;
  end;
end;

procedure TWJLaserReader.DoExecute;
begin
  FOwner.FSyncLock.Enter;
  try
    if FThreadType = ttAll then
    begin
      ScanActiveHost(False);
      //优先扫描不活动

      if not Assigned(FActiveHost) then
        ScanActiveHost(True);
      //辅助扫描活动项
    end else

    if FThreadType = ttActive then //只扫活动线程
    begin
      ScanActiveHost(True);
      //优先扫描活动

      if Assigned(FActiveHost) then
      begin
        FWaiter.Interval := cWJLaser_Wait_Short;
        //有活动,加速
      end else
      begin
        FWaiter.Interval := cWJLaser_Wait_Long;
        //无活动,降速
        ScanActiveHost(False);
        //辅助扫描不活动项
      end;
    end;
  finally
    FOwner.FSyncLock.Leave;
  end;

  if Assigned(FActiveHost) and (not Terminated) then
  try
    if SendHostCommand(FActiveHost) then
    begin
      ParseLaserData(FActiveHost);
      //解析数据
      
      if FThreadType = ttActive then
        FWaiter.Interval := cWJLaser_Wait_Short;
      FActiveHost.FLastActive := GetTickCount;
    end else
    begin
      if (FActiveHost.FLastActive > 0) and
         (GetTickCountDiff(FActiveHost.FLastActive) >= 5 * 1000) then
        FActiveHost.FLastActive := 0;
      //未检测到有效车辆,自动转为不活动
    end;
  except
    on E:Exception do
    begin
      FActiveHost.FLastActive := 0;
      //置为不活动

      WriteLog(Format('Reader:[ %s:%d ] Msg: %s', [FActiveHost.FHost,
        FActiveHost.FPort, E.Message]));
      //xxxxx

      FOwner.CloseHost(FActiveHost);
      //focus reconnect
    end;
  end;
end;

//------------------------------------------------------------------------------
type
  TTwoByte = packed record
    FHi: Byte;
    FLo: Byte;
  end;

procedure FillWord(var nBuf: array of Byte; const nData: Word);
var nTB: TTwoByte;
begin
  nTB := TTwoByte(nData);
  nBuf[0] := nTB.FLo;
  nBuf[1] := nTB.FHi;
end;

function MakeWord(const nLo,nHi: Byte): Word;
var nTB: TTwoByte;
begin
  nTB.FLo := nLo;
  nTB.FHi := nHi;
  Result := Word(nTB);
end;

//Date: 2019-10-10
//Parm: 待校验数据;是否附加校验结果;从索引0开始,校验的数据长度
//Desc: 使用异或算法校验nData数据
function VerifyData(var nData: TIdBytes; const nAppend: Boolean;
  nLen: Integer = -1): Byte;
var nIdx: Integer;
begin
  Result := 0;
  if nLen < 0 then
       nLen := High(nData) - 4 //末4位不参与计算
  else nLen := nLen - 5;
  if nLen < 2 then Exit;

  Result := nData[2];      //两位帧头不参与计算
  for nIdx:=3 to nLen do
    Result := Result xor nData[nIdx];
  //xxxxx

  if nAppend then
  begin
    nData[nLen + 1] := $00;
    nData[nLen + 2] := Result;

    nData[nLen + 3] := $EE;
    nData[nLen + 4] := $EE;
  end;
end;

//Date: 2019-10-10
//Parm: 数据;前缀
//Desc: 打印nData的二进制数据
procedure LogHex(const nData: TIdBytes; const nPrefix: string = '');
var nStr: string;
    nIdx: Integer;
begin
  nStr := '';
  for nIdx:=Low(nData) to High(nData) do
    nStr := nStr + IntToHex(nData[nIdx], 2) + ' ';
  WriteLog(nPrefix + nStr);
end;

//Date: 2019-10-10
//Desc: 构建获取单帧数据指令
procedure TWJLaserReader.InitSingleRequst;
begin
  FillChar(FSingleRequst, cSize_WJLaserData, #0);
  //init

  with FSingleRequst do
  begin
    FHead[0]         := $FF;
    FHead[1]         := $AA;

    FillWord(FLength, $1E);     //总长34,减去帧头帧尾4个字节
    FVerifyType      := $01;    //01,异或校验
    FrameType        := $01;    //01,请求;02,应答
    FDeviceType[0]   := $00;
    FDeviceType[1]   := $04;

    FCommand         := $02;    //02,距离数据
    FSubCmd          := $01;    //01,单帧数据
    FFrameEnd[0]     := $EE;
    FFrameEnd[1]     := $EE;    //帧尾
  end;
end;

//Date: 2019-10-14
//Parm: 激光器
//Desc: 向nHost发送数据查询指令
function TWJLaserReader.SendHostCommand(const nHost: PWJLaserHost): Boolean;
var nBuf: TIdBytes;
    nLen,nInit: Cardinal;
begin
  Result := False;
  nHost.FClient.CheckForGracefulDisconnect(False);
  //check connection

  if not nHost.FClient.Connected then
    nHost.FClient.Connect;
  //make sure connect

  with FSingleRequst do
  begin
    FillWord(FSerial, nHost.FSerial);
    Inc(nHost.FSerial);

    FTimeStamp := DateTimeToTimeStamp(Now).Time;
    nBuf := RawToBytes(FSingleRequst, MakeWord(FLength[0], FLength[1]) + 4);
    VerifyData(nBuf, True); //添加校验
  end;                     

  {$IFDEF DEBUG}
  LogHex(nBuf, 'Send Request ::: >>> ');
  {$ENDIF}

  nHost.FClient.IOHandler.Write(nBuf);
  //write data
  nHost.FClient.IOHandler.CheckForDataOnSource(100);
  //fill the output buffer with a long timeout

  nInit := GetTickCount();
  SetLength(nBuf, 0);

  repeat
    if nHost.FClient.IOHandler.InputBufferIsEmpty then
      nHost.FClient.IOHandler.CheckForDataOnSource(10);
    //fill the output buffer with a short timeout

    if not nHost.FClient.IOHandler.InputBufferIsEmpty then
      nHost.FClient.IOHandler.InputBuffer.ExtractToBytes(nBuf);
    //read data

    nLen := Length(nBuf);
    if (nLen < 5) or ((nBuf[0] <> $FF) or (nBuf[1] <> $AA)) then Continue;
    //invalid header

    if nLen >= MakeWord(nBuf[2], nBuf[3]) + 4 then Break;
    //frame done
  until GetTickCountDiff(nInit) >= nHost.FClient.ReadTimeout;

  if nLen < 5 then Exit;
  //no data

  {$IFDEF DEBUG}
  //LogHex(nBuf, 'Recv Data ::: <<< ');
  {$ENDIF}

  if nLen > cSize_WJLaserData then
    nLen := cSize_WJLaserData;
  //clip data

  BytesToRaw(nBuf, nHost.FData, nLen);
  with nHost.FData do
  begin
    FFrameEnd[1] := nBuf[nLen - 1];
    FFrameEnd[0] := nBuf[nLen - 2];
    FVerify[1]   := nBuf[nLen - 3];
    FVerify[0]   := nBuf[nLen - 4];
    //comine data

    if (FHead[0] <> $FF) or (FHead[1] <> $AA) then Exit; //invalid header
    if (FFrameEnd[0] <> $EE) or (FFrameEnd[1] <> $EE) then Exit; //invalid end

    if (FCommand <> $02) or (FSubCmd <> $01) then Exit;
    //invalid command

    if (FVerify[0] <> $00) or
       (FVerify[1] <> VerifyData(nBuf, False, nLen)) then Exit;
    //invalid verify
  end;

  Result := True;
end;

//Date: 2019-10-14
//Parm: 激光器
//Desc: 解析nHost的FData数据
procedure TWJLaserReader.ParseLaserData(const nHost: PWJLaserHost);
var nIdx,nInt: Integer;
    nDPrecision: Boolean;
begin
  with FOwner, nHost.FData do
  try
    FSyncLock.Enter;
    nIdx := MakeWord(FLength[0], FLength[1]);
    nDPrecision := nIdx > 1000;
    //依据数据长度判定激光器扫描精度(单精度0.5度,双精度0.25度)

    nIdx := 0;
    nInt := 0;
    //init

    while True do
    begin
      with nHost.FDataParse[nInt] do
      begin
        if nDPrecision then
             FAngle := nInt * 0.25
        else FAngle := nInt * 0.5;

        FDistance := MakeWord(FData[nIdx], FData[nIdx + 1]);
        Fx := Trunc(FDistance * Cos(FAngle * Pi / 180));
        Fy := Trunc(FDistance * Sin(FAngle * Pi / 180));

        if nHost.FHighLaser >= Fy then
             FHigh := nHost.FHighLaser - Fy
        else FHigh := 0;
      end;

      Inc(nIdx, 2);
      Inc(nInt);

      if nDPrecision then
      begin
        if nInt >= 720 then //双精度720个点值
        begin
          if nInt < cLaserDataParse then
            nHost.FDataParse[nInt].FAngle := -1;
          Break;
        end;
      end else
      begin
        if nInt >= 360 then //单精度360个点值
        begin
          if nInt < cLaserDataParse then
            nHost.FDataParse[nInt].FAngle := -1;
          Break;
        end;
      end;
    end;
  finally
    FSyncLock.Leave;
  end;
end;

initialization
  gWJLaserManager := nil;
finalization
  FreeAndNil(gWJLaserManager);
end.
