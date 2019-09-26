{*******************************************************************************
  ����: dmzn@163.com 2019-06-11
  ����: �������Ÿ�LED������

  ��ע:
  *.֧���ͺ�: DS-TVL224-4-5Y
*******************************************************************************}
unit UHKDoorLED2;

interface

uses
  Classes, SysUtils, SyncObjs, Windows, IdTCPClient, IdGlobal, NativeXml,
  UHKDoorLED_Head, UWaitItem, ULibFun, USysLoger;

type
  PHKCardParam = ^THKCardParam;
  THKCardParam = record
    FID        : string;            //��ʶ
    FName      : string;            //����
    FHost      : string;            //IP
    FPort      : Integer;           //�˿�

    FColor     : Integer;           //��ɫ:1,��ɫ;2,˫ɫ
    FWidth     : Integer;           //��
    FHeight    : Integer;           //��

    FTextKeep  : Integer;           //����ʱ��
    FTextSpeed : Integer;           //�����ٶ�
    FTextSend  : Int64;             //����ʱ��
    FDefaltTxt : string;            //Ĭ������
    FActionStyle : Integer;         //��ʾ��ʽ
  end;

  THKDataDisplay = (ddNeiMa, ddLnTXT);
  //��ʾģʽ: ����,�����ı�

  PHKCardData = ^THKCardData;
  THKCardData = record
    FCard      : string;            //����ʶ
    FText      : string;            //����
    FSound     : string;            //����
    FColor     : Integer;           //��ɫ
    FDisplay   : THKDataDisplay;    //��ʾģʽ
    FDefault   : Boolean;           //Ĭ�ϱ�ʶ
    FEnabled   : Boolean;           //��Ч��ʶ
  end;

  THKCardManager = class;
  THKCardSender = class(TThread)
  private
    FOwner: THKCardManager;
    //ӵ����
    FClient: TIdTCPClient;
    //�׽���
    FWaiter: TWaitObject;
    //�ȴ�����
  protected
    procedure Execute; override;
    procedure Doexecute;
    //ִ���߳�
    procedure CloseConnection;
    //�ر�����
    procedure SendCardData(const nCard: PHKCardParam; const nData: PHKCardData);
    //��������
  public
    constructor Create(AOwner: THKCardManager);
    destructor Destroy; override;
    //�����ͷ�
    procedure WakupMe;
    //�����߳�
    procedure StopMe;
    //ֹͣ�߳�
  end;

  THKCardManager = class(TObject)
  private
    FEnabled: Boolean;
    //�Ƿ�����
    FCards: array of THKCardParam;
    //���б�
    FTextData: array of THKCardData;
    //��������
    FSender: THKCardSender;
    //�����߳�
    FSyncLock: TCriticalSection;
    //ͬ������
  protected
    procedure InitDataBuffer;
    //��ʼ����
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    procedure LoadConfig(const nFile: string);
    //��ȡ����
    procedure StartDiaplay;
    procedure StopDiaplay;
    //��ͣ����
    procedure GetCardList(const nList: TStrings);
    //��ȡ�б�
    procedure DisplayText(const nCard,nText,nSound: string; const nColor: Integer);
    //��ʾ�ı�
  end;

var
  gHKCardManager: THKCardManager = nil;
  //ȫ��ʹ��
  
implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(THKCardManager, '��������LED��', nEvent);
end;

function ErrorDesc(const nCode: Integer): string;
begin
  case nCode of
   VT_PAR_OK                   : Result := '������ȷ';
   VT_PAR_DEV_NOT_INIT_ERROR   : Result := '�豸����δ����';
   VT_PAR_PROGRAM_ID_ERROR     : Result := '��Ŀ ID �Ŵ���';
   VT_PAR_NO_PROGRAM_ERROR     : Result := '��Ŀδ��ʼ��';
   VT_PAR_AREA_ID_ERROR        : Result := '���� ID �Ŵ���';
   VT_PAR_AREA_ERROR           : Result := '�����������ô���';
   VT_PAR_COLOR_ERROR          : Result := '��ɫ���ô���';
   VT_PAR_ACTION_ERROR         : Result := '������ʽ���ô���';
   VT_PAR_FONT_ERROR           : Result := '�������ô���';
   VT_PAR_SOUND_ONLY_ERROR     : Result := 'һ����Ŀ��ֻ�ܺ���һ������';
   VT_PAR_DATA_SIZE_ERROR      : Result := '���ݳ������ô���';
   VT_PAR_MEM_ERROR            : Result := 'ϵͳ�������';
   VT_PAR_FRAME_FLAG_ERROR     : Result := 'Э������֡��־����';
   VT_PAR_FRAME_SIZE_ERROR     : Result := 'Э������֡���ȴ���';
   VT_PAR_CMD_ERROR            : Result := 'ָ�����'
   else
    Result := 'δ�������';
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

//Date: 2019-06-11
//Desc: ��ʼ�����ݻ���
procedure THKCardManager.InitDataBuffer;
var nIdx: Integer;
begin
  SetLength(FTextData, 10);
  for nIdx:=Low(FTextData) to High(FTextData) do
    FTextData[nIdx].FEnabled := False;
  //xxxx
end;

//Date: 2019-06-11
//Parm: ����ʶ;����;��ɫ
//Desc: ��nCard����ʾ��ɫΪnColor��nText����
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
      end; //ͬ�����ݺϲ�

    nInt := -1;
    for nIdx:=Low(FTextData) to High(FTextData) do
    if not FTextData[nIdx].FEnabled then
    begin
      nInt := nIdx;
      Break;
    end; //δʹ��

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

//Date: 2019-06-11
//Parm: �б�
//Desc: ��ȡ���ÿ���ʶ�б�
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

          FTextSend := GetTickCount();
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
        nData.FDefault := False;
        
        FTextData[nIdx].FEnabled := False;
        Break;
      end; //��ȡ����������
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
          FSound := '';
          FColor := VT_COLOR_GREEN;

          FDefault := True;
          FEnabled := True;
        end;

        Break;
      end;
    end; //����Ĭ������

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
      nData.FText := '����[ %s ]������,��ʾ�����Ѷ���.';
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

//Date: 2019-06-11
//Parm: ��������;��ʾ����
//Desc: ��nCard����ʾnData����
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
      CloseConnection();      
      FClient.Host := nCard.FHost;
      FClient.Port := nCard.FPort;
    end;

    FClient.CheckForGracefulDisconnect(False);
    if not FClient.Connected then
      FClient.Connect;
    //conn remote
  except
    on nE: Exception do
    begin
      nErr := '����[ %s:%d ]ʧ��: %s';
      WriteLog(Format(nErr, [nCard.FHost, nCard.FPort, nE.Message]));

      CloseConnection();
      Exit;
    end;
  end;

  nErr := '��[ %s.%s ]��������ʧ��';
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

    if not nData.FDefault then
      nCard.FTextSend := GetTickCount;
    //reset default content

    FClient.IOHandler.CheckForDataOnSource(10);
    //fill the output buffer with a timeout

    if not FClient.IOHandler.InputBufferIsEmpty then
      FClient.IOHandler.InputBuffer.ExtractToBytes(nBuf);
    //read data

    CloseConnection();
    //disconn for reconn
  except
    on nE: Exception do
    begin
      nErr := '����[ %s,%s ]ʧ��: %s';
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
