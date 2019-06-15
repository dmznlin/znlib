{*******************************************************************************
  作者: dmzn@163.com 2019-06-11
  描述: 海康威门岗LED引导屏

  备注:
  *.支持型号: DS-TVL224-4-5Y
*******************************************************************************}
unit UHKDoorLED2;

interface

uses
  Classes, SysUtils, SyncObjs, Windows, IdTCPClient, IdGlobal, NativeXml,
  UHKDoorLED_Head, UWaitItem, USysLoger;

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
    FActionStyle : Integer;         //显示方式
  end;

  THKDataDisplay = (ddNeiMa, ddLnTXT);
  //显示模式: 内码,单行文本

  PHKCardData = ^THKCardData;
  THKCardData = record
    FCard      : string;            //卡标识
    FText      : string;            //内容
    FSound     : string;            //声音
    FColor     : Integer;           //颜色
    FDisplay   : THKDataDisplay;    //显示模式
    FEnabled   : Boolean;           //有效标识
  end;

  THKCardManager = class;
  THKCardSender = class(TThread)
  private
    FOwner: THKCardManager;
    //拥有者
    FClient: TIdTCPClient;
    //套接字
    FWaiter: TWaitObject;
    //等待对象
  protected
    procedure Execute; override;
    procedure Doexecute;
    //执行线程
    procedure CloseConnection;
    //关闭连接
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
    procedure DisplayText(const nCard,nText,nSound: string; const nColor: Integer);
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

function ErrorDesc(const nCode: Integer): string;
begin
  case nCode of
   VT_PAR_OK                   : Result := '参数正确';
   VT_PAR_DEV_NOT_INIT_ERROR   : Result := '设备参数未设置';
   VT_PAR_PROGRAM_ID_ERROR     : Result := '节目 ID 号错误';
   VT_PAR_NO_PROGRAM_ERROR     : Result := '节目未初始化';
   VT_PAR_AREA_ID_ERROR        : Result := '区域 ID 号错误';
   VT_PAR_AREA_ERROR           : Result := '区域坐标设置错误';
   VT_PAR_COLOR_ERROR          : Result := '颜色设置错误';
   VT_PAR_ACTION_ERROR         : Result := '动作方式设置错误';
   VT_PAR_FONT_ERROR           : Result := '字体设置错误';
   VT_PAR_SOUND_ONLY_ERROR     : Result := '一个节目中只能含有一条语音';
   VT_PAR_DATA_SIZE_ERROR      : Result := '数据长度设置错误';
   VT_PAR_MEM_ERROR            : Result := '系统缓存错误';
   VT_PAR_FRAME_FLAG_ERROR     : Result := '协议数据帧标志错误';
   VT_PAR_FRAME_SIZE_ERROR     : Result := '协议数据帧长度错误';
   VT_PAR_CMD_ERROR            : Result := '指令错误'
   else
    Result := '未定义错误';
  end;
end;

//------------------------------------------------------------------------------
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
procedure THKCardManager.DisplayText(const nCard, nText, nSound: string;
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
        FSound := nSound;
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
      FSound := nSound;
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
    nNode,nRoot,nTmp: TXmlNode;
begin
  nXML := TNativeXml.Create;
  try
    SetLength(FCards, 0);
    nXML.LoadFromFile(nFile);
    nRoot := nXML.Root.FindNode('config');

    if Assigned(nRoot) then
    begin
      nIdx := nRoot.NodeByName('enable').ValueAsInteger;
      FEnabled := nIdx = 1;
    end;

    nRoot := nXML.Root.FindNode('cards');
    if Assigned(nRoot) then
    begin
      for nIdx:=0 to nRoot.NodeCount - 1 do
      begin
        nNode := nRoot.Nodes[nIdx];
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

          FTextSend := 0;
          FTextKeep := nNode.NodeByName('textkeep').ValueAsInteger;
          FTextSpeed := nNode.NodeByName('textspeed').ValueAsInteger;
          FDefaltTxt := nNode.NodeByName('default').ValueAsString;

          nTmp := nNode.NodeByName('actionstyle');
          if Assigned(nTmp) then
               FActionStyle := StrToInt(nNode.NodeByName('actionstyle').ValueAsString)
          else FActionStyle := VT_ACTION_HOLD;
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

  FClient := TIdTCPClient.Create;
  with FClient do
  begin
    ReadTimeout := 3 * 1000;
    ConnectTimeout := 3 * 1000;
  end;
end;

destructor THKCardSender.Destroy;
begin
  FreeAndNil(FClient);
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
           (GetTickCount - FTextSend < FTextKeep * 1000) then Continue;
        FTextSend := 0;

        with nData do
        begin
          FCard := FID;
          FText := FDefaltTxt;
          FSound := '';
          FColor := VT_COLOR_GREEN;
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

procedure THKCardSender.CloseConnection;
begin
  FClient.Disconnect;
  if Assigned(FClient.IOHandler) then
    FClient.IOHandler.InputBuffer.Clear;
  //xxxxx
end;

//Date: 2017-10-18
//Parm: 屏卡索引;显示内容
//Desc: 在nCard上显示nData内容
procedure THKCardSender.SendCardData(const nCard: PHKCardParam;
  const nData: PHKCardData);
var nErr: string;
    nCode: Integer;
    nBuf,nPack: TIdBytes;
    nLen: vt_uint32_t;
    nProgram,nArea: vt_uint8_t;
begin
  try
    if FClient.Host <> nCard.FHost then
    begin
      FClient.Disconnect;
      FClient.Host := nCard.FHost;
      FClient.Port := nCard.FPort;
    end;

    if not FClient.Connected then
      FClient.Connect;
    //conn remote
  except
    on nE: Exception do
    begin
      nErr := '连接[ %s:%d ]失败: %s';
      WriteLog(Format(nErr, [nCard.FHost, nCard.FPort, nE.Message]));

      CloseConnection();
      Exit;
    end;
  end;

  nErr := '向[ %s.%s ]发送内容失败';
  nErr := Format(nErr, [nCard.FID, nCard.FName]);
  nErr := nErr + '(call %s return %d,%s).';

  SetLength(nBuf, 0);
  try
    nCode := vtInitialize(nCard.FWidth, nCard.FHeight, nCard.FColor, 0);
    if nCode <> VT_PAR_OK then
    begin
      WriteLog(Format(nErr, ['vtInitialize', nCode, ErrorDesc(nCode)]));
      Exit;
    end;

    nProgram := 1;
    nArea := 1;
    nCode := vtAddProgram(nProgram);

    if nCode <> VT_PAR_OK then
    begin
      WriteLog(Format(nErr, ['vtAddProgram', nCode, ErrorDesc(nCode)]));
      Exit;
    end;

    nCode := vtAddTextAreaItem(nProgram, nArea, 0, 0,
      nCard.FWidth, nCard.FHeight, PChar(nData.FText), Length(nData.FText),
      nData.FColor, nCard.FActionStyle,
      VT_FONT_16, nCard.FTextSpeed, nCard.FTextKeep);
    //xxxxx

    if nCode <> VT_PAR_OK then
    begin
      WriteLog(Format(nErr, ['vtAddTextAreaItem', nCode, ErrorDesc(nCode)]));
      Exit;
    end;

    if nData.FSound <> '' then
    begin
      Inc(nArea);
      nCode := vtAddSoundItem(nProgram, nArea, 0, 5, 5, PChar(nData.FSound),
        Length(nData.FSound));
      //xxxxx

      if nCode <> VT_PAR_OK then
      begin
        WriteLog(Format(nErr, ['vtAddSoundItem', nCode, ErrorDesc(nCode)]));
        Exit;
      end;
    end;

    SetLength(nPack, 1024);
    while True do
    begin
      nCode := vtGetProgramPack($01, $01, @nPack[0], @nLen);
      if (nLen < 1) or (nLen > 1024) then Exit; //invalid pack

      AppendBytes(nBuf, nPack, 0, nLen);
      if nCode < 1 then Break;
      //multi packs
    end;
  finally
    vtUninitialize();
    //close handle
  end;

  if Length(nBuf) > 0 then
  try
    FClient.IOHandler.Write(nBuf);
    //send data
    nCard.FTextSend := GetTickCount;

    FClient.IOHandler.CheckForDataOnSource(10);
    //fill the output buffer with a timeout

    if not FClient.IOHandler.InputBufferIsEmpty then
      FClient.IOHandler.InputBuffer.ExtractToBytes(nBuf);
    //read data
  except
    on nE: Exception do
    begin
      nErr := '发送[ %s,%s ]失败: %s';
      WriteLog(Format(nErr, [nCard.FID, nCard.FHost, nE.Message]));
      CloseConnection();
    end;
  end;
end;

initialization
  gHKCardManager := nil;
finalization
  FreeAndNil(gHKCardManager);
end.
