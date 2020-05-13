{*******************************************************************************
  ����: dmzn@163.com 2020-03-16
  ����: DataSky DS-00X wifi̽������
*******************************************************************************}
unit UWiFiProber;

interface

uses
  Windows, Classes, SysUtils, IdCustomHTTPServer, IdHTTPServer, IdContext,
  NativeXml, SyncObjs, superobject, UMemDataPool, UWaitItem, ULibFun,
  USysLoger;

type
  TWiFiVitualType = (vt900, vt02n);
  //��������: 900m,02n

  TWiFiPositon = (wpUnkown, wpIn, wpOut, wpPound, wpZT, wpSZ);
  //��װλ��: δ֪,����,����,�ذ�,ջ̨,ɢװ

  PWiFiProber = ^TWiFiProber;
  TWiFiProber = record
    FEnabled    : Boolean;               //�Ƿ�����
    FID         : string;                //��ʶ
    FHost       : string;                //��ַ
    FPort       : Integer;               //�˿�
    FHostKeep   : Integer;               //������Ч(��)
    FPosition   : TWiFiPositon;          //��װλ��
    FTunnel     : string;                //ͨ����ʶ
    FLastActive : Cardinal;              //�ϴλ

    FVirtual    : Boolean;               //�����ͷ
    FVReader    : string;                //��ͷ��ʶ
    FVReaders   : TDynamicStrArray;      //��ͷ����
    FVInterval  : Integer;               //��ͷ���
    FVRGroup    : string;                //��ͷ����
    FVType      : TWiFiVitualType;       //��������

    FKeepOnce   : Integer;               //���α���
    FKeepPeer   : Boolean;               //����ģʽ
    FOptions    : TStrings;              //����ѡ��
  end;

  PWiFiHost = ^TWiFiHost;
  TWiFiHost = record
    FMAC        : string;                //MAC��ַ
    Frssi       : Integer;               //�ź�ǿ��
    Frange      : Double;                //����(��)
    Fts         : string;                //��ǰ���ӵ�wifi ssid
    Ftmc        : string;                //��ǰ���ӵ�wifi mac
    Ftc         : string;                //�Ƿ���·��������(Y/N)
    Fds         : string;                //�Ƿ�˯��(Y/N)

    FFlag       : string;                //�ر��ʶ
    FCounter    : Cardinal;              //������
    FValid      : Boolean;               //�Ƿ���Ч
    FProber     : PWiFiProber;           //����̽��
    FLastActive : Cardinal;              //�ϴλ
  end;

  TWiFiDataFromProber = record
    FValid      : Boolean;               //�Ƿ���Ч
    FData       : string;                //����̽�������
    FLastActive : Cardinal;              //�ϴ�ʹ��      
  end;

  TWiFiData = array of TWiFiDataFromProber;
  //̽�����ݻ���
  
  TWiFiProberManager = class;
  TWiFiProbeThread = class(TThread)
  private
    FOwner: TWiFiProberManager;
    //ӵ����
    FDataParsed: string;
    //����������
    FActiveHost: TWiFiHost;
    //WiFi����
    FOfflineHost: TList;
    //��������
    FWaiter: TWaitObject;
    //�ȴ�����
  protected
    procedure DoExecute;
    procedure Execute; override;
    //ִ���߳�
    procedure DoOwnerEvent;
    procedure DoEvent;
    //ִ���¼�
  public
    constructor Create(AOwner: TWiFiProberManager);
    destructor Destroy; override;
    //�����ͷ�
    procedure StopMe;
    //ֹͣ�߳�
  end;

  TWiFiProbeThreads = array of TWiFiProbeThread;
  //����߳���

  TWiFiProbeEventMode = (emThread, emMain);
  //�¼�ģʽ: �߳�;������

  TWiFiConfigure = record
    FEnableProbe : Boolean;              //����̽��
    FValidProbe  : Integer;              //��Ч̽����

    FHostBlock   : Boolean;              //�Ƿ���ֹ
    FHostFlag    : string;               //������ʶ
    FHostMAC     : string;               //�����嵥
    FHostList    : TStrings;             //�����б�

    FThreadNum   : Integer;              //����߳���
    FWebPort     : Integer;              //���ݽ��ն˿�
    FHostKeep    : Integer;              //������Ϣ����ʱ��(��)
    FCheckOffline: Cardinal;             //�ϴμ����������ʱ��
  end;

  TWiFiOnNewHost = procedure (const nHost: PWiFiHost);
  TWiFiOnNewHostEvent = procedure (const nHost: PWiFiHost) of object;
  //�¼�����

  TWiFiProberManager = class(TObject)
  private
    FConfig: TWiFiConfigure;
    //���ò���
    FProbers: TList;
    //̽���б�
    FIDHost: Integer;
    FWiFiHost: TList;
    FWiFiData: TWiFiData;
    //̽������
    FWebServer: TIdHTTPServer;
    //Web����
    FProbeThreads: TWiFiProbeThreads;
    //̽���߳�
    FSyncLock: TCriticalSection;
    //ͬ������
    FEventMode: TWiFiProbeEventMode;
    FOnNewHost: TWiFiOnNewHost;
    FOnNewHostEvent: TWiFiOnNewHostEvent;
    //�¼����
  protected
    procedure ClearProberList(const nFree: Boolean = False);
    procedure ClearHostList(var nList: TList; const nFree: Boolean);
    //������Դ
    procedure RegisterDataType;
    //ע������
    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    //��������
    function FindProber(const nID: string): Integer;
    //��������
    function LockHost(const nMAC: string; const nProber: PWiFiProber;
     const nOffline: TList = nil): PWiFiHost;
    //��������
    function InHostList(const nMAC: string): Boolean;
    //���б���
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    procedure LoadConfig(const nFile: string);
    //��������
    procedure StartProber;
    procedure StopProber;
    //��ͣ̽��
    property Config: TWiFiConfigure read FConfig;
    property OnNewHost: TWiFiOnNewHost read FOnNewHost write FOnNewHost;
    property OnNewHostEvent: TWiFiOnNewHostEvent read FOnNewHostEvent write FOnNewHostEvent;
    property EventMode: TWiFiProbeEventMode read FEventMode write FEventMode;
    //�������
  end;

var
  gWiFiProberManager: TWiFiProberManager = nil;
  //ȫ��ʹ��

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TWiFiProberManager, 'WiFi������', nEvent);
end;

constructor TWiFiProberManager.Create;
var nDef: TWiFiConfigure;
begin
  RegisterDataType;
  FEventMode := emThread;
  SetLength(FProbeThreads, 0);

  FillChar(nDef, SizeOf(nDef), #0);
  FConfig := nDef; //init all
  FConfig.FHostList   := TStringList.Create;

  FProbers := TList.Create;
  FWiFiHost := TList.Create;
  FSyncLock := TCriticalSection.Create;
  
  FWebServer := TIdHTTPServer.Create;
  FWebServer.OnCommandGet := DoCommandGet;
end;

destructor TWiFiProberManager.Destroy;
begin
  StopProber();
  ClearHostList(FWiFiHost, True);
  ClearProberList(True);

  FConfig.FHostList.Free;
  FWebServer.Free;
  FSyncLock.Free;
  inherited;
end;

procedure OnNew(const nFlag: string; const nType: Word; var nData: Pointer);
var nHost: PWiFiHost;
begin
  if nFlag = 'WiFiHost' then
  begin
    New(nHost);
    nData := nHost;
  end;
end;

procedure OnFree(const nFlag: string; const nType: Word; const nData: Pointer);
begin
  if nFlag = 'WiFiHost' then
  begin
    Dispose(PWiFiHost(nData));
  end;
end;

//Desc: ע����������
procedure TWiFiProberManager.RegisterDataType;
begin
  if not Assigned(gMemDataManager) then
    raise Exception.Create('WiFiProberManager Needs MemDataManager Support.');
  //xxxxx

  with gMemDataManager do
  begin
    FIDHost:= RegDataType('WiFiHost', 'WiFiProbe', OnNew, OnFree, 2);
  end;
end;

//Date: 2020-03-17
//Parm: �Ƿ��ͷ�
//Desc: ����̽���б�
procedure TWiFiProberManager.ClearProberList(const nFree: Boolean);
var nIdx: Integer;
    nProber: PWiFiProber;
begin
  if Assigned(FProbers) then
  begin
    for nIdx:=FProbers.Count - 1 downto 0 do
    begin
      nProber := FProbers[nIdx];
      nProber.FOptions.Free;
      Dispose(nProber);
    end;
    
    if nFree then
         FreeAndNil(FProbers)
    else FProbers.Clear;
  end;
end;

//Date: 2020-03-19
//Parm: �Ƿ��ͷ�
//Desc: ���������б�
procedure TWiFiProberManager.ClearHostList(var nList: TList; const nFree: Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx:=nList.Count - 1 downto 0 do
      gMemDataManager.UnLockData(nList[nIdx]);
    //xxxxx
    
    if nFree then
         FreeAndNil(nList)
    else nList.Clear;
  end;
end;

//Date: 2020-03-17
//Desc: ��������
procedure TWiFiProberManager.StartProber;
var nIdx: Integer;
begin
  if not FConfig.FEnableProbe then
  begin
    WriteLog('All Probers Are Diabled.');
    Exit;
  end;

  if FConfig.FValidProbe < 1 then
  begin
    WriteLog('No Prober Active.');
    Exit;
  end;

  StopProber(); //stop first
  SetLength(FWiFiData, FConfig.FValidProbe * 10);
  for nIdx:=Low(FWiFiData) to High(FWiFiData) do
    FWiFiData[nIdx].FValid := False;
  //init
         
  SetLength(FProbeThreads, FConfig.FThreadNum);
  for nIdx:=Low(FProbeThreads) to High(FProbeThreads) do
  begin
    if nIdx >= FConfig.FValidProbe then Break;
    //�̲߳����������豸��

    FProbeThreads[nIdx] := TWiFiProbeThread.Create(Self);
    //xxxxx
  end;

  FWebServer.DefaultPort := FConfig.FWebPort;
  FWebServer.Active := True;
  //start web service
end;

//Date: 2020-03-17
//Desc: ֹͣ����
procedure TWiFiProberManager.StopProber;
var nIdx: Integer;
begin
  FWebServer.Active := False;
  //stop web first

  for nIdx:=Low(FProbeThreads) to High(FProbeThreads) do
   if Assigned(FProbeThreads[nIdx]) then
    FProbeThreads[nIdx].Terminate;
  //set exit flag

  for nIdx:=Low(FProbeThreads) to High(FProbeThreads) do
  begin
    if Assigned(FProbeThreads[nIdx]) then
      FProbeThreads[nIdx].StopMe;
    FProbeThreads[nIdx] := nil;
  end;

  ClearHostList(FWiFiHost, False);
  //clear buffer
end;

//Date: 2020-03-18
//Parm: ̽���ʶ
//Desc: ������ʶΪnID��̽��
function TWiFiProberManager.FindProber(const nID: string): Integer;
var nIdx: Integer;
begin
  Result := -1;
  //default

  for nIdx:=FProbers.Count-1 downto 0 do
  if CompareText(nID, PWiFiProber(FProbers[nIdx]).FID) = 0 then
  begin
    Result := nIdx;
    Break;
  end;
end;

//Date: 2020-03-19
//Parm: MAC;̽��;����������
//Desc: ����nProber�ϵ�nMAC����
function TWiFiProberManager.LockHost(const nMAC: string;
 const nProber: PWiFiProber; const nOffline: TList): PWiFiHost;
var nIdx: Integer;
    nHost: PWiFiHost;
begin
  Result := nil;
  for nIdx:=FWiFiHost.Count-1 downto 0 do
  begin
    if nIdx = 0 then
      FConfig.FCheckOffline := GetTickCount();
    //checked all

    nHost := FWiFiHost[nIdx];
    if (nMAC <> '') and (nHost.FMAC = nMAC) then
    begin
      Result := nHost;
      Break;
    end;

    if (not nHost.FValid) or
       (GetTickCountDiff(nHost.FLastActive) >= nHost.FProber.FHostKeep) then
    begin
      if Assigned(nOffline) then
      begin
        nHost.FValid := False;
        nOffline.Add(nHost);
      end else
      begin
        gMemDataManager.UnLockData(nHost);
        //release
      end;

      FWiFiHost.Delete(nIdx);
      //�ڵ㳬ʱ,�Ƴ�
    end;
  end;

  if nMAC = '' then //only for offline
  begin
    FConfig.FCheckOffline := GetTickCount();
    Exit;
  end;
  
  if not Assigned(Result) then
  begin
    Result := gMemDataManager.LockData(FIDHost);
    FWiFiHost.Add(Result);
    Result.FMAC := '';
  end;

  if Result.FMAC <> nMAC then
  begin
    Result.FMAC := nMAC;
    Result.FCounter := 0;
  end;

  Inc(Result.FCounter);
  Result.FValid := True;
  Result.FProber := nProber;
end;

//Date: 2020-03-20
//Parm: MAC
//Desc: �ж�nMAC�Ƿ��ڹ�ע�б���
function TWiFiProberManager.InHostList(const nMAC: string): Boolean;
var nIdx: Integer;
begin
  Result := False;
  if FConfig.FHostMAC = '' then Exit;

  for nIdx:=FConfig.FHostList.Count-1 downto 0 do
  if (FConfig.FHostList[nIdx] = nMAC) or
     (Pos(FConfig.FHostList[nIdx], nMAC) > 0) then
  begin
    Result := True;
    Exit;
  end;
end;

//Date: 2020-03-17
//Desc: ����̽���ϴ�����
procedure TWiFiProberManager.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var nIdx,nFirst: Integer;
begin
  FSyncLock.Enter;
  try
    nFirst := Low(FWiFiData);
    //init first
    
    for nIdx:=nFirst+1 to High(FWiFiData) do
    begin
      if not FWiFiData[nIdx].FValid then
      begin
        nFirst := nIdx; //first idle
        Break;
      end;

      if TickCountRelation(FWiFiData[nIdx].FLastActive,
                           FWiFiData[nFirst].FLastActive) = rtLess then
        nFirst := nIdx;
      //��λ�������Ľڵ�,׼������
    end;

    try
      with FWiFiData[nFirst] do
      begin
        FData := UTF8Decode(ARequestInfo.Params.Values['data']);
        FLastActive := GetTickCount();
        FValid := True;
      end;
    except
      on nErr: Exception do
      begin
        WriteLog('WiFi�����쳣: ' + nErr.Message);
      end;
    end;
  finally
    FSyncLock.Leave;
  end;
end;

procedure TWiFiProberManager.LoadConfig(const nFile: string);
var nStr: string;
    nIdx: Integer;
    nXML: TNativeXml;  
    nProber: PWiFiProber;
    nRoot,nNode,nTmp: TXmlNode;

    //Desc: ��ֶ��ͷ��ʶ
    procedure SplitMultiReaders(nNames: string);
    var i,nPos: Integer;
    begin
      nPos := Pos(',', nNames);
      if nPos < 2 then
      begin
        SetLength(nProber.FVReaders, 1);
        nProber.FVReaders[0] := nNames;
        Exit;
      end;

      i := Length(nNames);
      if Copy(nNames, i, 1) <> ',' then
        nNames := nNames + ',';
      //xxxxx

      nPos := Pos(',', nNames);
      while nPos > 0 do
      begin
        nStr := Trim(Copy(nNames, 1, nPos - 1));
        if nStr <> '' then
        begin
          i := Length(nProber.FVReaders);
          SetLength(nProber.FVReaders, i + 1);
          nProber.FVReaders[i] := nStr;
        end;

        System.Delete(nNames, 1, nPos);
        nPos := Pos(',', nNames);
      end;
    end;
begin
  StopProber;
  ClearProberList(False);
  FConfig.FValidProbe := 0; //for load config

  nXML := TNativeXml.Create;
  try
    nXML.LoadFromFile(nFile);
    //load config

    nRoot := nXML.Root.NodeByNameR('config');
    with FConfig,nRoot do
    begin
      FEnableProbe := NodeByNameR('enable').ValueAsString <> 'N';
      FWebPort := NodeByNameR('localport').ValueAsInteger;

      nNode := NodeByNameR('hostlist');
      FHostBlock := nNode.AttributeByName['block'] = 'Y';
      FHostFlag := nNode.AttributeByName['flag'];
      FHostMAC := Trim(nNode.ValueAsString);

      FHostList.Clear;
      if FHostMAC <> '' then
        SplitStr(FHostMAC, FHostList, 0, ',');
      //xxxxx

      FThreadNum := NodeByNameR('thread').ValueAsInteger;
      if (FThreadNum < 1) or (FThreadNum > 5) then
      begin
        FThreadNum := 1;
        WriteLog('WiFi Probe Thread-Num Need Between 1-5.');
      end;

      FHostKeep := NodeByNameR('hostkeep').ValueAsInteger;
      if FHostKeep < 2 then
      begin
        FHostKeep := 2;
        WriteLog('WiFi Probe Host-Keep >=2 Seconds.');
      end;
      FHostKeep := FHostKeep * 1000;
    end;

    //--------------------------------------------------------------------------
    nRoot := nXML.Root.FindNode('probers');
    if not Assigned(nRoot) then Exit;

    for nIdx:=0 to nRoot.NodeCount - 1 do
    begin
      nNode := nRoot.Nodes[nIdx];
      if CompareText(nNode.Name, 'prober') <> 0 then Continue;

      New(nProber);
      FProbers.Add(nProber);

      with nNode,nProber^ do
      begin
        FLastActive := GetTickCount;

        FID := AttributeByName['id'];
        FHost := NodeByNameR('ip').ValueAsString;
        FPort := NodeByNameR('port').ValueAsInteger;

        FEnabled := NodeByNameR('enable').ValueAsString <> 'N';
        if FEnabled then Inc(FConfig.FValidProbe);
        //counter

        nStr := LowerCase(NodeByNameR('position').ValueAsString);
        if nStr = 'in' then FPosition := wpIn else
        if nStr = 'out' then FPosition := wpOut else
        if nStr = 'pound' then FPosition := wpPound else
        if nStr = 'zt' then FPosition := wpZT else
        if nStr = 'sz' then FPosition := wpSZ else FPosition := wpUnkown;

        nTmp := FindNode('hostkeep');
        if Assigned(nTmp) and (nTmp.ValueAsInteger >= 2) then
             FHostKeep := nTmp.ValueAsInteger * 1000
        else FHostKeep := FConfig.FHostKeep;

        nTmp := FindNode('tunnel');
        if Assigned(nTmp) then
          FTunnel := nTmp.ValueAsString;
        //tunnel

        nTmp := FindNode('virtual');
        if Assigned(nTmp) then
        begin
          FVirtual := nTmp.ValueAsString = 'Y';
          FVReader := nTmp.AttributeByName['reader'];
          FVRGroup := nTmp.AttributeByName['group'];

          if nTmp.AttributeByName['type'] = '900' then
               FVType := vt900
          else FVType := vt02n;

          SplitMultiReaders(FVReader);
          //����������ʱ,�������
          
          nStr := nTmp.AttributeByName['interval'];
          if (nStr <> '') and IsNumber(nStr, False) then
               FVInterval := StrToInt(nStr)
          else FVInterval := 0;
        end else
        begin
          FVirtual := False;
          //Ĭ�ϲ�����
        end;

        nTmp := FindNode('keeponce');
        if Assigned(nTmp) then
        begin
          FKeepOnce := nTmp.ValueAsInteger;
          FKeepPeer := nTmp.AttributeByName['keeppeer'] = 'Y';
        end else
        begin
          FKeepOnce := 0;
          //Ĭ�ϲ��ϲ�
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
constructor TWiFiProbeThread.Create(AOwner: TWiFiProberManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := AOwner;
  
  FOfflineHost := TList.Create;
  FWaiter := TWaitObject.Create;
  FWaiter.Interval := 100;
end;

destructor TWiFiProbeThread.Destroy;
begin
  FOwner.ClearHostList(FOfflineHost, True);
  FWaiter.Free;
  inherited;
end;

procedure TWiFiProbeThread.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure TWiFiProbeThread.Execute;
var nIdx: Integer;
    nHasData: Boolean;
begin
  nHasData := True;
  while not Terminated do
  try
    if not nHasData then
      FWaiter.EnterWait;
    //�����ݼ���

    if Terminated then Exit;
    nHasData := False;
    
    with FOwner do
    begin
      FSyncLock.Enter;
      try
        for nIdx:=Low(FWiFiData) to High(FWiFiData) do
        with FWiFiData[nIdx] do
        begin
          if not FValid then Continue;
          //invalid
          FValid := False;
          
          nHasData := True;
          FDataParsed := FData;
          Break;
        end;
      finally
        FSyncLock.Leave;
      end;
    end;

    if nHasData then
      DoExecute;
    //parse data

    with FOwner do
    begin
      FSyncLock.Enter;
      try
        if GetTickCountDiff(FConfig.FCheckOffline, tdZero) > 5 * 1000 then
          LockHost('', nil, FOfflineHost);
        //get offline hosts
      finally
        FSyncLock.Leave;
      end;

      if FOfflineHost.Count > 0 then //offline event
      begin
        FActiveHost.FValid := False;
        DoEvent;
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

//Date: 2020-03-18
//Desc: ����̽������
procedure TWiFiProbeThread.DoExecute;
var nStr: string;
    nIdx: Integer;
    nHost: PWiFiHost;
    nProber: PWiFiProber;
    nJSON: ISuperObject;
    nHosts: TSuperArray;
begin
  nJSON := SO(FDataParsed);
  nStr := nJSON.S['id'];

  nIdx := FOwner.FindProber(nStr);
  if nIdx < 0 then
  begin
    WriteLog(Format('WiFi %s Is Not Exists.', [nStr]));
    Exit;
  end;
  nProber := FOwner.FProbers[nIdx];

  nHosts := nJSON.A['data'];  
  for nIdx:=nHosts.Length - 1 downto 0 do
  begin
    nStr := Trim(nHosts[nIdx].S['mac']);
    if nStr = '' then Continue;
    //invalid mac

    if FOwner.InHostList(nStr) then
    begin
      if FOwner.FConfig.FHostBlock then Continue; //������      
      FActiveHost.FFlag := FOwner.FConfig.FHostFlag;
    end else
    begin
      FActiveHost.FFlag := '';
      //no flag
    end;

    with FOwner do
    try
      FActiveHost.FValid := False;
      FSyncLock.Enter;
      //get active and offline

      nHost := LockHost(nStr, nProber, FOfflineHost);
      nHost.FFlag := FActiveHost.FFlag;
      //host flag is important

      if nProber.FKeepOnce > 0 then
      begin
        if GetTickCountDiff(nHost.FLastActive) < nProber.FKeepOnce then
        begin
          if not nProber.FKeepPeer then
            nHost.FLastActive := GetTickCount;
          Continue;
        end; //ͬһ������γ���ʱѹ���¼�
      end;

      nHost.Frssi   := nHosts[nIdx].I['rssi'];
      nHost.Frange  := nHosts[nIdx].D['range'];
      nHost.Fts     := nHosts[nIdx].S['ts'];
      nHost.Ftmc    := nHosts[nIdx].S['tmc'];
      nHost.Ftc     := nHosts[nIdx].S['tc'];
      nHost.Fds     := nHosts[nIdx].S['ds'];
      nHost.FLastActive := GetTickCount();

      if Assigned(FOnNewHost) or Assigned(FOnNewHostEvent) then
        FActiveHost := nHost^;
      //bind data
    finally
      FSyncLock.Leave;

      if FActiveHost.FValid or (FOfflineHost.Count > 0) then
        DoEvent;
      //ִ���¼�
    end;
  end;
end;

procedure TWiFiProbeThread.DoEvent;
begin
  if FOwner.FEventMode = emThread then
    DoOwnerEvent;
  //xxxxx

  if FOwner.FEventMode = emMain then
    Synchronize(DoOwnerEvent);
  //xxxxx
end;

procedure TWiFiProbeThread.DoOwnerEvent;
var nIdx: Integer;
begin
  if FActiveHost.FValid then //��������
  begin
    if Assigned(FOwner.FOnNewHost) then
      FOwner.FOnNewHost(@FActiveHost);
    //xxxxx

    if Assigned(FOwner.FOnNewHostEvent) then
      FOwner.FOnNewHostEvent(@FActiveHost);
    //xxxxx
  end;

  if FOfflineHost.Count > 0 then //��������
  begin
    for nIdx:=FOfflineHost.Count-1 downto 0 do
    begin
      if Assigned(FOwner.FOnNewHost) then
        FOwner.FOnNewHost(FOfflineHost[nIdx]);
      //xxxxx

      if Assigned(FOwner.FOnNewHostEvent) then
        FOwner.FOnNewHostEvent(FOfflineHost[nIdx]);
      //xxxxx
    end;

    FOwner.ClearHostList(FOfflineHost, False);
    //clear list
  end;
end;

initialization
  gWiFiProberManager := nil;
finalization
  FreeAndNil(gWiFiProberManager);
end.
