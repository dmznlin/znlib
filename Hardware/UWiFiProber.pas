{*******************************************************************************
  ����: dmzn@163.com 2020-03-16
  ����: DataSky DS-00X wifi̽������
*******************************************************************************}
unit UWiFiProber;

interface

uses
  Windows, Classes, SysUtils, IdCustomHTTPServer, IdHTTPServer, IdContext,
  NativeXml, SyncObjs, superobject, UWaitItem, ULibFun, USysLoger;

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
    Ftms        : string;                //��ǰ���ӵ�wifi mac
    Ftc         : string;                //�Ƿ���·��������(Y/N)
    Fds         : string;                //�Ƿ�˯��(Y/N)

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
    FWiFiHost: TList;
    FActiveHost: TWiFiHost;
    //WiFi����
    FWaiter: TWaitObject;
    //�ȴ�����
  protected
    procedure SyncProbe;
    procedure DoExecute;
    procedure Execute; override;
    //ִ���߳�
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
    FBlockMAC    : string;               //������
    FThreadNum   : Integer;              //����߳���
    FWebPort     : Integer;              //���ݽ��ն˿�
    FHostKeep    : Integer;              //������Ϣ����ʱ��(��)
  end;

  TWiFiProberManager = class(TObject)
  private
    FConfig: TWiFiConfigure;
    //���ò���
    FProbers: TList;
    //̽���б�
    FWiFiData: TWiFiData;
    //̽������
    FWebServer: TIdHTTPServer;
    //Web����
    FProbeThreads: TWiFiProbeThreads;
    //̽���߳�
    FSyncLock: TCriticalSection;
    //ͬ������
  protected
    procedure ClearProberList(const nFree: Boolean = False);
    //������Դ
    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    //��������
    function FindProber(const nID: string): Integer;
    //��������
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    procedure LoadConfig(const nFile: string);
    //��������
    procedure StartProber;
    procedure StopProber;
    //��ͣ̽��
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
begin 
  SetLength(FProbeThreads, 0);
  FProbers := TList.Create;
  FSyncLock := TCriticalSection.Create;

  FWebServer := TIdHTTPServer.Create;
  FWebServer.OnCommandGet := DoCommandGet;
end;

destructor TWiFiProberManager.Destroy;
begin
  StopProber();
  ClearProberList(True);

  FWebServer.Free;
  FSyncLock.Free;
  inherited;
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

//Date: 2020-03-17
//Desc: ��������
procedure TWiFiProberManager.StartProber;
var nIdx,nInt: Integer;
    nProber: PWiFiProber;
begin
  if not FConfig.FEnableProbe then
  begin
    WriteLog('All Probers Are Diabled.');
    Exit;
  end;

  nInt := 0;
  for nIdx:=FProbers.Count-1 downto 0 do
  begin
    nProber := FProbers[nIdx];
    if nProber.FEnabled then
      Inc(nInt);
    //count enable prober
  end;

  if nInt < 1 then
  begin
    WriteLog('No Prober Active.');
    Exit;
  end;

  StopProber(); //stop first
  SetLength(FWiFiData, nInt * 10);
  for nIdx:=Low(FWiFiData) to High(FWiFiData) do
    FWiFiData[nIdx].FValid := False;
  //init
         
  SetLength(FProbeThreads, FConfig.FThreadNum);
  for nIdx:=Low(FProbeThreads) to High(FProbeThreads) do
  begin
    if nIdx >= nInt then Break;
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
  //for load config

  nXML := TNativeXml.Create;
  try
    nXML.LoadFromFile(nFile);
    //load config

    nRoot := nXML.Root.NodeByNameR('config');
    with FConfig,nRoot do
    begin
      FEnableProbe := NodeByNameR('enable').ValueAsString <> 'N';
      FBlockMAC := NodeByNameR('blockmac').ValueAsString;
      FWebPort := NodeByNameR('localport').ValueAsInteger;

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

        nStr := LowerCase(NodeByNameR('position').ValueAsString);
        if nStr = 'in' then FPosition := wpIn else
        if nStr = 'out' then FPosition := wpOut else
        if nStr = 'pound' then FPosition := wpPound else
        if nStr = 'zt' then FPosition := wpZT else
        if nStr = 'sz' then FPosition := wpSZ else FPosition := wpUnkown;

        nTmp := FindNode('hostkeep');
        if Assigned(nTmp) then
        begin
          FHostKeep := nTmp.ValueAsInteger;
          if FHostKeep < 2 then
            FHostKeep := 2;
          //default value
        end else FHostKeep := FConfig.FHostKeep;

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
  FWiFiHost := TList.Create;

  FWaiter := TWaitObject.Create;
  FWaiter.Interval := 100;
end;

destructor TWiFiProbeThread.Destroy;
var nIdx: Integer;
begin
  for nIdx:=FWiFiHost.Count-1 downto 0 do
    Dispose(PWiFiHost(FWiFiHost[nIdx]));
  FWiFiHost.Free;

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
    nJSON: ISuperObject;
    nProber: PWiFiProber;
begin
  nJSON := SO(FDataParsed);
  nStr := nJSON.S['id'];

  nIdx := FOwner.FindProber(nStr);
  if nIdx < 0 then
  begin
    WriteLog(Format('WiFi %s Is Not Exists.', [nStr]));
    Exit;
  end;

  WriteLog(nJSON.s['data']);
end;

procedure TWiFiProbeThread.SyncProbe;
begin

end;

initialization
  gWiFiProberManager := nil;
finalization
  FreeAndNil(gWiFiProberManager);
end.
