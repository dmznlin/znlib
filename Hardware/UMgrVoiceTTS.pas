{*******************************************************************************
  ����: dmzn@163.com 2019-10-24
  ����: ����windows tts�������ϳ�����
*******************************************************************************}
unit UMgrVoiceTTS;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, ActiveX, ComObj, Variants, SpeechLib_TLB,
  NativeXml, UWaitItem, ULibFun, UMemDataPool, USysLoger;

const
  cVoice_FrameInterval  = 10;          //֡���
  cVoice_Status_Busy    = $4E;         //����״̬
  cVoice_Status_Idle    = $4F;         //����״̬

  cVoice_Content_Len    = 4096;        //�ı�����
  cVoice_Content_Keep   = 60 * 1000;   //ͣ����ʱ

type
  PVoiceContentParam = ^TVoiceContentParam;
  TVoiceContentParam = record
    FID       : string;                //���ݱ�ʶ
    FObject   : string;                //�����ʶ
    FSleep    : Integer;               //������
    FText     : string;                //��������
    FTimes    : Integer;               //�ط�����
    FInterval : Integer;               //�ط����
    FRepeat   : Integer;               //�����ظ�
    FReInterval: Integer;              //���μ��
  end;

  PVoiceResource = ^TVoiceResource;
  TVoiceResource = record
    FKey      : string;                //������
    FValue    : string;                //��������
  end;

  PVoiceContentNormal = ^TVoiceContentNormal;
  TVoiceContentNormal = record
    FText     : string;                //�������ı�
    FContent  : string;                //ִ�����ݱ�ʶ
    FAddTime  : Int64;                 //�������ʱ��
  end;

  PVoiceConfig = ^TVoiceConfig;
  TVoiceConfig = record
    FEnable   : Boolean;               //�Ƿ�����
    FShowLog  : Boolean;               //��ʾ��־
    FVoiceName: string;                //��������
    FContent  : TList;                 //��������
    FResource : TList;                 //��Դ����

    FVoiceData: string;                //��������
    FVoiceLast: Cardinal;              //�ϴβ���
    FVoiceTime: Byte;                  //��������
    FParam    : PVoiceContentParam;    //��������
  end;

type
  TVoiceManager = class;
  TVoiceConnector = class(TThread)
  private
    FOwner: TVoiceManager;
    //ӵ����
    FTTSVoicer: ISpeechVoice;
    //���Ŷ���
    FWaiter: TWaitObject;
    //�ȴ�����
    FListA: TStrings;
    //�ַ��б�
  protected
    procedure Execute; override;
    //ִ���߳�
    procedure SetTTSVoice(const nName: string);
    //����������
    function MakeVoiceData: Boolean;
    procedure SendVoiceData;
    //��������
  public
    constructor Create(AOwner: TVoiceManager);
    destructor Destroy; override;
    //�����ͷ�
    procedure WakupMe;
    //�����߳�
    procedure StopMe;
    //ֹͣ�߳�
  end;

  TVoiceManager = class(TObject)
  private
    FConfig: TVoiceConfig;
    //������Ϣ
    FBuffer: TList;
    //���ݻ���
    FIDContent: Word;
    //���ݱ�ʶ
    FVoicer: TVoiceConnector;
    //��������
    FSyncLock: TCriticalSection;
    //ͬ����
  protected
    procedure ClearDataList(const nList: TList; const nFree: Boolean = False);
    //������
    procedure RegisterDataType;
    //ע������
    function FindContentParam(const nID: string): PVoiceContentParam;
    //��������
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    procedure LoadConfig(const nFile: string);
    //��ȡ����
    procedure StartVoice;
    procedure StopVoice;
    //��ͣ��ȡ
    procedure PlayVoice(const nText: string; const nContent: string = '');
    //��������
  end;

var
  gTTSVoiceManager: TVoiceManager = nil;
  //ȫ��ʹ��

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TVoiceManager, 'TTS Voice Manager', nEvent);
end;

procedure OnNew(const nFlag: string; const nType: Word; var nData: Pointer);
var nItem: PVoiceContentNormal;
begin
  if nFlag = 'TTSContent' then
  begin
    New(nItem);
    nData := nItem;
  end;
end;

procedure OnFree(const nFlag: string; const nType: Word; const nData: Pointer);
var nItem: PVoiceContentNormal;
begin
  if nFlag = 'TTSContent' then
  begin
    nItem := nData;
    Dispose(nItem);
  end;
end;

procedure TVoiceManager.RegisterDataType;
begin
  if not Assigned(gMemDataManager) then
    raise Exception.Create('NetVoiceManager Needs MemDataManager Support.');
  //xxxxx

  with gMemDataManager do
    FIDContent := RegDataType('TTSContent', 'TTSVoiceManager', OnNew, OnFree, 2);
  //xxxxx
end;

//------------------------------------------------------------------------------
constructor TVoiceManager.Create;
begin
  RegisterDataType;
  with FConfig do
  begin
    FVoiceTime := MAXBYTE;
    //��ǲ�����
    FVoiceLast := 0;
    //���δ����

    FResource := nil;
    FContent := TList.Create;
  end;

  FBuffer := TList.Create;
  FSyncLock := TCriticalSection.Create;
end;

destructor TVoiceManager.Destroy;
begin
  StopVoice;
  ClearDataList(FBuffer, True);
  ClearDataList(FConfig.FContent, True);
  ClearDataList(FConfig.FResource, True);

  FSyncLock.Free;
  inherited;
end;

//Date: 2015-04-23
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nList�б�
procedure TVoiceManager.ClearDataList(const nList: TList; const nFree: Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx:=nList.Count - 1 downto 0 do
    begin
      if nList = FBuffer then
      begin
        gMemDataManager.UnLockData(nList[nIdx]);
        //unlock
      end else

      if nList = FConfig.FContent then
      begin
        Dispose(PVoiceContentParam(FConfig.FContent[nIdx]));
        //free
      end else

      if nList = FConfig.FResource then
      begin
        Dispose(PVoiceResource(FConfig.FResource[nIdx]));
        //free
      end;
    end;

    if nFree then
         nList.Free
    else nList.Clear;
  end;
end;

procedure TVoiceManager.StartVoice;
begin
  if not FConfig.FEnable then Exit;
  //no use

  if not Assigned(FVoicer) then
    FVoicer := TVoiceConnector.Create(Self);
  FVoicer.WakupMe;
end;

procedure TVoiceManager.StopVoice;
begin
  if Assigned(FVoicer) then
    FVoicer.StopMe;
  FVoicer := nil;

  FConfig.FVoiceTime := MAXBYTE;
  ClearDataList(FBuffer);
  //��������ͻ���
end;

//Date: 2015-04-23
//Parm: ������;���ݱ�ʶ
//Desc: ��nCard�м�����ʶΪnID����������
function TVoiceManager.FindContentParam(const nID: string): PVoiceContentParam;
var nIdx: Integer;
begin
  if FConfig.FContent.Count > 0 then
       Result := FConfig.FContent[0]
  else Result := nil;

  for nIdx:=FConfig.FContent.Count - 1 downto 0 do
  if CompareText(nID, PVoiceContentParam(FConfig.FContent[nIdx]).FID) = 0 then
  begin
    Result := FConfig.FContent[nIdx];
    Break;
  end;
end;

//Date: 2015-04-23
//Parm: �ı�;�������ñ�ʶ
//Desc: ����ʹ��nContent���������nText,д�뻺��ȴ�����
procedure TVoiceManager.PlayVoice(const nText,nContent: string);
var nTxt: string;
    nIdx: Integer;
    nData: PVoiceContentNormal;
begin
  nTxt := Trim(nText);
  if (not FConfig.FEnable) or (nTxt = '') then Exit;
  //invalid text

  if not Assigned(FVoicer) then
    raise Exception.Create('Voice Service Should Start First.');
  //xxxxx

  FSyncLock.Enter;
  try
    for nIdx:=FBuffer.Count-1 downto 0 do
    begin
      nData := FBuffer[nIdx];
      if (nData.FContent = nContent) and (nData.FText = nTxt) then
      begin
        nData.FAddTime := GetTickCount;
        Exit;
      end; //�ϲ���ͬ����
    end;

    nData := gMemDataManager.LockData(FIDContent);
    FBuffer.Add(nData);

    nData.FText := nTxt;
    nData.FContent := nContent;
    nData.FAddTime := GetTickCount;
  finally
    FSyncLock.Leave;
  end;   
end;

//Date: 2015-04-23
//Parm: �����ļ�
//Desc: ��ȡnFile�����ļ�
procedure TVoiceManager.LoadConfig(const nFile: string);
var nIdx: Integer;
    nXML: TNativeXml;
    nRoot,nNode: TXmlNode;
    nRes: PVoiceResource;
    nParam: PVoiceContentParam;
begin
  nXML := TNativeXml.Create;
  try
    nXML.LoadFromFile(nFile);
    nRoot := nXML.Root.NodeByNameR('config');
    with nRoot,FConfig do
    begin
      FEnable := NodeByNameR('enable').ValueAsString <> 'N';
      FVoiceName := Trim(NodeByNameR('voicename').ValueAsString);

      nNode := NodeByName('showlog');
      if Assigned(nNode) then
           FShowLog := nNode.ValueAsString <> 'N'
      else FShowLog := True;
    end;

    nRoot := nXML.Root.NodeByNameR('contents');
    for nIdx:=0 to nRoot.NodeCount - 1 do
    with nRoot.Nodes[nIdx] do
    begin
      New(nParam);
      FConfig.FContent.Add(nParam);

      with nParam^ do
      begin
        FID       := AttributeByName['id'];
        FObject   := NodeByName('object').ValueAsString;
        FSleep    := NodeByName('sleep').ValueAsInteger;
        FText     := NodeByName('text').ValueAsString;

        FTimes    := NodeByName('times').ValueAsInteger;
        FInterval := NodeByName('interval').ValueAsInteger;
        FRepeat   := NodeByName('repeat').ValueAsInteger;
        FReInterval := NodeByName('reinterval').ValueAsInteger;
      end;
    end;

    nRoot := nXML.Root.NodeByName('resource');
    if Assigned(nRoot) then
    begin
      FConfig.FResource := TList.Create;
      //resource

      for nIdx:=nRoot.NodeCount - 1 downto 0 do
      with nRoot.Nodes[nIdx] do
      begin
        New(nRes);
        FConfig.FResource.Add(nRes);

        nRes.FKey   := AttributeByName['key'];
        nRes.FValue := AttributeByName['value'];
      end;
    end else FConfig.FResource := nil;
  finally
    nXML.Free;
  end;
end;

//------------------------------------------------------------------------------
constructor TVoiceConnector.Create(AOwner: TVoiceManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FTTSVoicer := nil;
  FListA := TStringList.Create;
  
  FWaiter := TWaitObject.Create;
  FWaiter.Interval := 1000;
end;

destructor TVoiceConnector.Destroy;
begin
  FWaiter.Free;
  FListA.Free;
  inherited;
end;

procedure TVoiceConnector.WakupMe;
begin
  FWaiter.Wakeup;
end;

procedure TVoiceConnector.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure TVoiceConnector.Execute;
begin
  CoInitialize(nil);
  try
    FTTSVoicer := CoSpVoice.Create;
    SetTTSVoice(FOwner.FConfig.FVoiceName);
    //init voice

    while True do
    begin
      FWaiter.EnterWait;
      if Terminated then Break;

      FOwner.FSyncLock.Enter;
      try
        if not MakeVoiceData then Continue;
        //no data
      finally
        FOwner.FSyncLock.Leave;
      end;

      SendVoiceData;
      //��������
    end;

    FTTSVoicer := nil;
    //free object
  except
    on E:Exception do
    begin
      WriteLog(E.Message);
    end;
  end;

  CoUninitialize();
end;

//Date: 2019-10-31
//Parm: ����������
//Desc: ���õ�ǰ������ΪnName
procedure TVoiceConnector.SetTTSVoice(const nName: string);
var nStr: string;
    nIdx: Integer;
    nSOToken: ISpeechObjectToken;
    nSOTokens: ISpeechObjectTokens;
begin
  nSOTokens := FTTSVoicer.GetVoices('', '');
  for nIdx := 0 to nSOTokens.Count - 1 do
  begin
    nSOToken := nSOTokens.Item(nIdx);
    nStr := nSOToken.GetDescription(0);
    WriteLog(Format('VoiceName %d: %s', [nIdx, nStr]));

    if (nName <> '') and (Pos(nName, nStr) > 0) then
      FTTSVoicer.Voice := nSOToken;
    //set voice
  end;
end;

//Desc: �����ͻ������ݺϲ�������������
function TVoiceConnector.MakeVoiceData: Boolean;
var nStr: string;
    i,nIdx,nLen: Integer;
    nRes: PVoiceResource;
    nParm: PVoiceContentParam;
    nTxt: PVoiceContentNormal;

    //Desc: �ͷŻ�����
    procedure DisposeBufferItem;
    begin
      gMemDataManager.UnLockData(FOwner.FBuffer[nIdx]);
      FOwner.FBuffer.Delete(nIdx);
    end;
begin
  with FOwner do
  begin
    Result := False;
    nIdx := 0;
    
    while nIdx < FBuffer.Count do
    begin
      nTxt := FBuffer[nIdx];
      if GetTickCountDiff(nTxt.FAddTime) > cVoice_Content_Keep then
      begin
        WriteLog('�����������ݳ�ʱ.');
        DisposeBufferItem;
        Continue;
      end;

      nParm := FindContentParam(nTxt.FContent);
      if not Assigned(nParm) then
      begin
        nStr := Format('�������ݱ�ʶ[ %s ]������.', [nTxt.FContent]);
        WriteLog(nStr);

        DisposeBufferItem;
        Continue;
      end;

      //------------------------------------------------------------------------
      SplitStr(nTxt.FText, FListA, 0, #9, False);
      //���: YA001 #9 YA002

      for i:=FListA.Count - 1 downto 0 do
      begin
        FListA[i] := Trim(FListA[i]);
        if FListA[i] = '' then
          FListA.Delete(i);
        //�������
      end;

      if (FListA.Count > 1) or (nTxt.FText[1] = #9) then
      begin
        nStr := '';
        nLen := FListA.Count - 1;

        for i:=0 to nLen do
        if Trim(FListA[i]) <> '' then
        begin
          if nIdx = nLen then
               nStr := nStr + FListA[i]
          else nStr := nStr + FListA[i] + Format('<silence msec="%d"/>', [nParm.FSleep]);
        end;

        nStr := StringReplace(nParm.FText, nParm.FObject, nStr,
                                           [rfReplaceAll, rfIgnoreCase]);
        //text real content
      end else nStr := nTxt.FText;

      for i:=FConfig.FResource.Count - 1 downto 0 do
      begin
        nRes := FConfig.FResource[i];
        nStr := StringReplace(nStr, nRes.FKey, nRes.FValue,
                                    [rfReplaceAll, rfIgnoreCase]);
        //resource replace
      end;

      for i:=2 to nParm.FRepeat do
        nStr := nStr + Format('<silence msec="%d"/>', [nParm.FReInterval]) + nStr;
      //xxxxx

      with FConfig do
      begin
        FVoiceData := nStr;
        FParam := nParm;
        FVoiceLast := 0;
        FVoiceTime := 0;
      end;

      DisposeBufferItem;
      //�������,�ͷ�
      Result := True;
      Exit;
    end;
  end;
end;

//Date: 2015-04-23
//Desc: ���ͻ���������
procedure TVoiceConnector.SendVoiceData;
begin
  with FOwner do
  begin
    if FConfig.FVoiceTime = MAXBYTE then Exit;
    //�����ͱ��
    if FConfig.FVoiceTime >= FConfig.FParam.FTimes then Exit;
    //���ʹ������
    if GetTickCountDiff(FConfig.FVoiceLast) < FConfig.FParam.FInterval * 1000 then Exit;
    //���ͼ��δ��

    try
      FTTSVoicer.Speak(FConfig.FVoiceData, SVSFDefault or SVSFIsXML);
      //voice

      if FConfig.FShowLog then
        WriteLog(FConfig.FVoiceData);
      //loged
    finally
      FConfig.FVoiceLast := GetTickCount;
      FConfig.FVoiceTime := FConfig.FVoiceTime + 1;
      //������
    end;
  end;
end;

initialization
  gTTSVoiceManager := nil;
finalization
  FreeAndNil(gTTSVoiceManager);
end.
