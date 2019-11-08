{*******************************************************************************
  ����: dmzn@163.com 2014-06-11
  ����: ��վͨ��������
*******************************************************************************}
unit UMgrPoundTunnels;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, CPort, CPortTypes, IdComponent,
  IdTCPConnection, IdTCPClient, IdUDPServer, IdGlobal, IdSocketHandle, NativeXml, ULibFun,
  UWaitItem, USysLoger;

const
  cPTMaxCameraTunnel = 5;
  //֧�ֵ������ͨ����

  cPTWait_Short = 320;
  cPTWait_Long  = 2 * 1000; //����ͨѶʱˢ��Ƶ��

type
  TPoundTunnelManager = class;
  TPoundTunnelConnector = class;

  PPTPortItem = ^TPTPortItem;
  PPTCameraItem = ^TPTCameraItem;
  PPTTunnelItem = ^TPTTunnelItem;

  TOnTunnelDataEvent = procedure (const nValue: Double) of object;
  TOnTunnelDataEventEx = procedure (const nValue: Double;
    const nPort: PPTPortItem) of object;
  //�¼�����
  
  TPTTunnelItem = record
    FID: string;                     //��ʶ
    FName: string;                   //����
    FPort: PPTPortItem;              //ͨѶ�˿�
    FProber: string;                 //������
    FReader: string;                 //�ſ���ͷ
    FUserInput: Boolean;             //�ֹ�����
    FAutoWeight: Boolean;            //�Զ�����

    FFactoryID: string;              //������ʶ
    FCardInterval: Integer;          //�������
    FSampleNum: Integer;             //��������
    FSampleFloat: Integer;           //��������
    FOptions: TStrings;              //���Ӳ���

    FCamera: PPTCameraItem;          //�����
    FCameraTunnels: array[0..cPTMaxCameraTunnel-1] of Byte;
                                     //����ͨ��                                     
    FOnData: TOnTunnelDataEvent;
    FOnDataEx:TOnTunnelDataEventEx;  //�����¼�
    FOldEventTunnel: PPTTunnelItem;  //ԭ����ͨ��
  end;

  TPTCameraItem = record
    FID: string;                     //��ʶ
    FType: string;                   //����
    FHost: string;                   //������ַ
    FPort: Integer;                  //�˿�
    FUser: string;                   //�û���
    FPwd: string;                    //����
    FPicSize: Integer;               //ͼ���С
    FPicQuality: Integer;            //ͼ������
  end;

  TPTConnType = (ctTCP, ctUDP, ctCOM, ctUser);
  //��·����: ����,����,�ֶ�����
         
  TPTPortItem = record
    FID: string;                     //��ʶ
    FName: string;                   //����
    FType: string;                   //����
    FConn: TPTConnType;              //��·
    FPort: string;                   //�˿�
    FRate: TBaudRate;                //������
    FDatabit: TDataBits;             //����λ
    FStopbit: TStopBits;             //��ͣλ
    FParitybit: TParityBits;         //У��λ
    FParityCheck: Boolean;           //����У��
    FCharBegin: Char;                //��ʼ���
    FCharEnd: Char;                  //�������
    FPackLen: Integer;               //���ݰ���
    FSplitTag: string;               //�ֶα�ʶ
    FSplitPos: Integer;              //��Ч��
    FInvalidBegin: Integer;          //���׳���
    FInvalidEnd: Integer;            //��β����
    FDataMirror: Boolean;            //��������
    FDataByteHex: Boolean;           //16����ֵ
    FDataEnlarge: Single;            //�Ŵ���
    FDataPrecision: Integer;         //���ݾ���
    FMaxValue: Double;               //��վ����
    FMaxValid: Double;               //����ȡֵ
    FMinValue: Double;               //��վ����

    FHostIP: string;
    FHostPort: Integer;              //������·
    FHostNetCheck: Integer;          //������
    FHostLastActive: Cardinal;       //����
    FClient: TIdTCPClient;           //�׽���
    FClientActive: Boolean;          //��·����

    FCOMPort: TComPort;              //��д����
    FCOMBuff: string;                //ͨѶ����
    FCOMData: string;                //ͨѶ����
    FCOMValue: Double;               //��Чֵ
    FCOMDataEx: string;              //ԭʼ����
    
    FEventTunnel: PPTTunnelItem;     //����ͨ��
    FOptions: TStrings;              //���Ӳ���
  end;

  TPoundTunnelConnector = class(TThread)
  private
    FOwner: TPoundTunnelManager;
    //ӵ����
    FActiveClient: TIdTCPClient;
    FActivePort: PPTPortItem;
    FActiveTunnel: PPTTunnelItem;
    //��ǰͨ��
    FWaiter: TWaitObject;
    //�ȴ�����
  protected
    procedure Execute; override;
    //ִ���߳�
    function ReadPound: Boolean;
    //��ȡ����
    procedure DoSyncEvent;
    //�����¼�
  public
    constructor Create(AOwner: TPoundTunnelManager);
    destructor Destroy; override;
    //�����ͷ�
    procedure WakupMe;
    //�����߳�
    procedure StopMe;
    //ֹͣ�߳�
  end;

  TPoundParseWeight = function (const nPort: PPTPortItem): Boolean;
  //�Զ������ݽ���

  TPoundTunnelManager = class(TObject)
  private
    FPorts: TList;
    //�˿��б�
    FCameras: TList;
    //�����
    FTunnels: TList;
    //ͨ���б�
    FStrList: TStrings;
    //�ַ��б�
    FUDPServerUser: Integer;
    FUDPServer: TIdUDPServer;
    //UDP��·
    FSyncLock: TCriticalSection;
    //ͬ������
    FConnector: TPoundTunnelConnector;
    //�׽�����·
    FOnTunnelData: TOnTunnelDataEventEx;
    //���ݴ����¼�
    FOnUserParseWeight: TPoundParseWeight;
    //�Զ������
  protected
    procedure ClearList(const nFree: Boolean);
    //������Դ
    function ParseWeight(const nPort: PPTPortItem): Boolean;
    procedure OnComData(Sender: TObject; Count: Integer);
    //��ȡ����
    procedure DoUDPRead(AThread: TIdUDPListenerThread;
      AData: TIdBytes; ABinding: TIdSocketHandle);
    //UDP��·
    procedure DisconnectClient(const nClient: TIdTCPClient);
    //�ر���·
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    procedure LoadConfig(const nFile: string);
    //��ȡ����
    function ActivePort(const nTunnel: string; const nEvent: TOnTunnelDataEvent;
      const nOpenPort: Boolean = False;
      const nEventEx: TOnTunnelDataEventEx = nil): Boolean;
    procedure ClosePort(const nTunnel: string);
    //��ͣ�˿�
    procedure SetData(const nTunnel: string; const nValue: Double);
    //�ֶ�����
    function GetPort(const nID: string): PPTPortItem;
    function GetCamera(const nID: string): PPTCameraItem;
    function GetTunnel(const nID: string): PPTTunnelItem;
    //��������
    property Tunnels: TList read FTunnels;
    property OnUserParseWeight: TPoundParseWeight read FOnUserParseWeight
      write FOnUserParseWeight;
    property OnData: TOnTunnelDataEventEx read FOnTunnelData write FOnTunnelData;
    //�������
  end;

var
  gPoundTunnelManager: TPoundTunnelManager = nil;
  //ȫ��ʹ��

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TPoundTunnelManager, '��վͨ������', nEvent);
end;

constructor TPoundTunnelManager.Create;
begin
  FConnector := nil;
  FOnTunnelData := nil;
  FUDPServer := nil;
  FUDPServerUser := 0;

  FPorts := TList.Create;
  FCameras := TList.Create;

  FTunnels := TList.Create;
  FStrList := TStringList.Create;
  FSyncLock := TCriticalSection.Create;
end;

destructor TPoundTunnelManager.Destroy;
begin
  if Assigned(FConnector) then
    FConnector.StopMe;
  //xxxxx

  if Assigned(FUDPServer) then
  begin
    FUDPServer.Active := False;
    FreeAndNil(FUDPServer);
  end;
  
  ClearList(True);
  FStrList.Free;
  FSyncLock.Free;
  inherited;
end;

//Date: 2014-06-12
//Parm: �Ƿ��ͷ�
//Desc: �����б���Դ
procedure TPoundTunnelManager.ClearList(const nFree: Boolean);
var nIdx: Integer;
    nPort: PPTPortItem;
    nTunnel: PPTTunnelItem;
begin
  for nIdx:=FPorts.Count - 1 downto 0 do
  begin
    nPort := FPorts[nIdx];
    if Assigned(nPort.FCOMPort) then
    begin
      nPort.FCOMPort.Close;
      nPort.FCOMPort.Free;
    end;

    if Assigned(nPort.FClient) then
    begin
      nPort.FClient.Disconnect;
      nPort.FClient.Free;
    end;

    FreeAndNil(nPort.FOptions);
    Dispose(nPort);
    FPorts.Delete(nIdx);
  end;

  for nIdx:=FCameras.Count - 1 downto 0 do
  begin
    Dispose(PPTCameraItem(FCameras[nIdx]));
    FCameras.Delete(nIdx);
  end;

  for nIdx:=FTunnels.Count - 1 downto 0 do
  begin
    nTunnel := FTunnels[nIdx];
    FreeAndNil(nTunnel.FOptions);
    
    Dispose(nTunnel);
    FTunnels.Delete(nIdx);
  end;

  if nFree then
  begin
    FPorts.Free;
    FCameras.Free;
    FTunnels.Free;
  end;
end;

//Date: 2016-11-26
//Parm: �׽���
//Desc: ����nClient��·
procedure TPoundTunnelManager.DisconnectClient(const nClient: TIdTCPClient);
begin
  if Assigned(nClient) then
  begin
    nClient.Disconnect;
    if Assigned(nClient.IOHandler) then
      nClient.IOHandler.InputBuffer.Clear;
    //xxxxx
  end;
end;

//Date��2014-6-18
//Parm��ͨ��;��ַ�ַ���,����: 1,2,3
//Desc����nStr��,����nTunnel.FCameraTunnels�ṹ��
procedure SplitCameraTunnel(const nTunnel: PPTTunnelItem; const nStr: string);
var nIdx: Integer;
    nList: TStrings;
begin
  nList := TStringList.Create;
  try
    for nIdx:=Low(nTunnel.FCameraTunnels) to High(nTunnel.FCameraTunnels) do
      nTunnel.FCameraTunnels[nIdx] := MAXBYTE;
    //Ĭ��ֵ

    SplitStr(nStr, nList, 0 , ',');
    if nList.Count < 1 then Exit;

    nIdx := nList.Count - 1;
    if nIdx > High(nTunnel.FCameraTunnels) then
      nIdx := High(nTunnel.FCameraTunnels);
    //���߽�

    while nIdx>=Low(nTunnel.FCameraTunnels) do
    begin
      nTunnel.FCameraTunnels[nIdx] := StrToInt(nList[nIdx]);
      Dec(nIdx);
    end;
  finally
    nList.Free;
  end;
end;

//Date: 2014-06-12
//Parm: �����ļ�
//Desc: ����nFile����
procedure TPoundTunnelManager.LoadConfig(const nFile: string);
var nStr: string;
    nIdx: Integer;
    nXML: TNativeXml;
    nNode,nTmp: TXmlNode;
    nPort: PPTPortItem;
    nCamera: PPTCameraItem;
    nTunnel: PPTTunnelItem;
begin
  nXML := TNativeXml.Create;
  try
    nXML.LoadFromFile(nFile);
    nNode := nXML.Root.NodeByNameR('ports');

    for nIdx:=0 to nNode.NodeCount - 1 do
    with nNode.Nodes[nIdx] do
    begin
      New(nPort);
      FPorts.Add(nPort);
      FillChar(nPort^, SizeOf(TPTPortItem), #0);

      with nPort^ do
      begin
        FID := AttributeByName['id'];
        FName := AttributeByName['name'];
        FType := NodeByNameR('type').ValueAsString;

        nTmp := NodeByName('conn');
        if Assigned(nTmp) then
             nStr := nTmp.ValueAsString
        else nStr := 'com';

        if CompareText('tcp', nStr) = 0 then nPort.FConn := ctTCP else
        if CompareText('udp', nStr) = 0 then nPort.FConn := ctUDP else
        if CompareText('user', nStr) = 0 then nPort.FConn := ctUser else
           nPort.FConn := ctCOM;
        //xxxxxx

        FPort := NodeByNameR('port').ValueAsString;
        FRate := StrToBaudRate(NodeByNameR('rate').ValueAsString);
        FDatabit := StrToDataBits(NodeByNameR('databit').ValueAsString);
        FStopbit := StrToStopBits(NodeByNameR('stopbit').ValueAsString);
        FParitybit := StrToParity(NodeByNameR('paritybit').ValueAsString);
        FParityCheck := NodeByNameR('paritycheck').ValueAsString = 'Y';

        FCharBegin := Char(StrToInt(NodeByNameR('charbegin').ValueAsString));
        FCharEnd := Char(StrToInt(NodeByNameR('charend').ValueAsString));
        FPackLen := NodeByNameR('packlen').ValueAsInteger;

        nTmp := NodeByName('invalidlen');
        if Assigned(nTmp) then //ֱ��ָ����ȡ����
        begin
          FInvalidBegin := 0;
          FInvalidEnd := nTmp.ValueAsInteger;
        end else
        begin
          FInvalidBegin := NodeByNameR('invalidbegin').ValueAsInteger;
          FInvalidEnd := NodeByNameR('invalidend').ValueAsInteger;
        end;

        FSplitTag := Char(StrToInt(NodeByNameR('splittag').ValueAsString));
        FSplitPos := NodeByNameR('splitpos').ValueAsInteger;
        FDataMirror := NodeByNameR('datamirror').ValueAsInteger = 1;
        FDataEnlarge := NodeByNameR('dataenlarge').ValueAsFloat;

        nTmp := NodeByName('databytehex');
        if Assigned(nTmp) then
             FDataByteHex := nTmp.ValueAsInteger = 1
        else FDataByteHex := False;

        nTmp := NodeByName('dataprecision');
        if Assigned(nTmp) then
             FDataPrecision := nTmp.ValueAsInteger
        else FDataPrecision := 100;

        nTmp := NodeByName('maxval');
        if Assigned(nTmp) and (nTmp.AttributeByName['enable'] = 'y') then
        begin
          FMaxValue := nTmp.ValueAsFloat;
          FMaxValid := StrToFloat(nTmp.AttributeByName['valid']);

          FMaxValue := Float2Float(FMaxValue, 100, False);
          FMaxValid := Float2Float(FMaxValid, 100, False);
        end else
        begin
          FMaxValue := 0;
          FMaxValid := 0;
        end;

        nTmp := NodeByName('minval');
        if Assigned(nTmp) and (nTmp.AttributeByName['enable'] = 'y') then
             FMinValue := Float2Float(nTmp.ValueAsFloat, 100, False)
        else FMinValue :=0;

        nTmp := NodeByName('hostip');
        if Assigned(nTmp) then
          FHostIP := nTmp.ValueAsString;
        //xxxxx

        nTmp := NodeByName('hostport');
        if Assigned(nTmp) then
          FHostPort := nTmp.ValueAsInteger;
        //xxxxx

        nTmp := NodeByName('hostcheck');
        if Assigned(nTmp) then
             FHostNetCheck := nTmp.ValueAsInteger
        else FHostNetCheck := 0;

        nTmp := NodeByName('options');
        if Assigned(nTmp) then
        begin
          FOptions := TStringList.Create;
          SplitStr(nTmp.ValueAsString, FOptions, 0, ';');
        end else FOptions := nil;
      end;

      nPort.FClient := nil;
      nPort.FClientActive := False;
      //Ĭ������·
      
      nPort.FCOMPort := nil;
      //Ĭ�ϲ�����
      nPort.FEventTunnel := nil;
    end;

    nNode := nXML.Root.NodeByName('cameras');
    if Assigned(nNode) then
    begin
      for nIdx:=0 to nNode.NodeCount - 1 do
      with nNode.Nodes[nIdx] do
      begin
        New(nCamera);
        FCameras.Add(nCamera);
        FillChar(nCamera^, SizeOf(TPTCameraItem), #0);

        with nCamera^ do
        begin
          FID := AttributeByName['id'];
          FHost := NodeByNameR('host').ValueAsString;
          FPort := NodeByNameR('port').ValueAsInteger;
          FUser := NodeByNameR('user').ValueAsString;
          FPwd := NodeByNameR('password').ValueAsString;
          FPicSize := NodeByNameR('picsize').ValueAsInteger;
          FPicQuality := NodeByNameR('picquality').ValueAsInteger;
        end;

        nTmp := NodeByName('type');
        if Assigned(nTmp) then
             nCamera.FType := nTmp.ValueAsString
        else nCamera.FType := 'HKV';
      end;
    end;

    nNode := nXML.Root.NodeByNameR('tunnels');
    for nIdx:=0 to nNode.NodeCount - 1 do
    with nNode.Nodes[nIdx] do
    begin
      New(nTunnel);
      FTunnels.Add(nTunnel);
      FillChar(nTunnel^, SizeOf(TPTTunnelItem), #0);

      nStr := NodeByNameR('port').ValueAsString;
      nTunnel.FPort := GetPort(nStr);
      if not Assigned(nTunnel.FPort) then
        raise Exception.Create(Format('ͨ��[ %s.Port ]��Ч.', [nTunnel.FName]));
      //xxxxxx

      with nTunnel^ do
      begin
        FID := AttributeByName['id'];
        FName := AttributeByName['name'];
        FProber := NodeByNameR('prober').ValueAsString;
        FReader := NodeByNameR('reader').ValueAsString;
        FUserInput := NodeByNameR('userinput').ValueAsString = 'Y';

        FFactoryID := NodeByNameR('factory').ValueAsString;
        FCardInterval := NodeByNameR('cardInterval').ValueAsInteger;
        FSampleNum := NodeByNameR('sampleNum').ValueAsInteger;
        FSampleFloat := NodeByNameR('sampleFloat').ValueAsInteger;

        nTmp := NodeByName('options');
        if Assigned(nTmp) then
        begin
          FOptions := TStringList.Create;
          SplitStr(nTmp.ValueAsString, FOptions, 0, ';');
        end else FOptions := nil;

        nTmp := NodeByName('autoweight');
        if Assigned(nTmp) then
             FAutoWeight := nTmp.ValueAsString = 'Y'
        else FAutoWeight := False; //�Ϸ�����,������Ҫ��

        nTmp := NodeByName('camera');
        if Assigned(nTmp) then
        begin
          nStr := nTmp.AttributeByName['id'];
          FCamera := GetCamera(nStr);
          SplitCameraTunnel(nTunnel, nTmp.ValueAsString);
        end else
        begin
          FCamera := nil;
          //no camera
        end;
      end;
    end;
  finally
    nXML.Free;
  end;   
end;

//------------------------------------------------------------------------------
//Desc: ������ʶΪnID�Ķ˿�
function TPoundTunnelManager.GetPort(const nID: string): PPTPortItem;
var nIdx: Integer;
begin
  Result := nil;

  for nIdx:=FPorts.Count - 1 downto 0 do
  if CompareText(nID, PPTPortItem(FPorts[nIdx]).FID) = 0 then
  begin
    Result := FPorts[nIdx];
    Exit;
  end;
end;

//Desc: ������ʶΪnID�������
function TPoundTunnelManager.GetCamera(const nID: string): PPTCameraItem;
var nIdx: Integer;
begin
  Result := nil;

  for nIdx:=FCameras.Count - 1 downto 0 do
  if CompareText(nID, PPTCameraItem(FCameras[nIdx]).FID) = 0 then
  begin
    Result := FCameras[nIdx];
    Exit;
  end;
end;

//Desc: ������ʶΪnID��ͨ��
function TPoundTunnelManager.GetTunnel(const nID: string): PPTTunnelItem;
var nIdx: Integer;
begin
  Result := nil;

  for nIdx:=FTunnels.Count - 1 downto 0 do
  if CompareText(nID, PPTTunnelItem(FTunnels[nIdx]).FID) = 0 then
  begin
    Result := FTunnels[nIdx];
    Exit;
  end;
end;

//Date: 2014-06-11
//Parm: ͨ����;�����¼�
//Desc: ����nTunnelͨ����д�˿�
function TPoundTunnelManager.ActivePort(const nTunnel: string;
  const nEvent: TOnTunnelDataEvent; const nOpenPort: Boolean;
  const nEventEx: TOnTunnelDataEventEx): Boolean;
var nPT: PPTTunnelItem;
begin
  Result := False;
  //xxxxx

  FSyncLock.Enter;
  try
    nPT := GetTunnel(nTunnel);
    if not Assigned(nPT) then Exit;

    nPT.FOnData := nEvent;
    nPT.FOnDataEx := nEventEx;
    
    nPT.FOldEventTunnel := nPT.FPort.FEventTunnel;
    nPT.FPort.FEventTunnel := nPT;
    
    if nPT.FPort.FConn = ctTCP then
    begin
      if not Assigned(nPT.FPort.FClient) then
      begin
        nPT.FPort.FClient := TIdTCPClient.Create;
        //new socket
        
        with nPT.FPort.FClient do
        begin
          Host := nPT.FPort.FHostIP;
          Port := nPT.fPort.FHostPort;
          
          ReadTimeout := 2 * 1000;
          ConnectTimeout := 2 * 1000;
        end;
      end;

      if not Assigned(FConnector) then
        FConnector := TPoundTunnelConnector.Create(Self);
      FConnector.WakupMe; //����������
                                  
      nPT.FPort.FClientActive := True;
      //�׽�����·
    end else

    if nPT.FPort.FConn = ctCOM then
    begin
      if not Assigned(nPT.FPort.FCOMPort) then
      begin
        nPT.FPort.FCOMPort := TComPort.Create(nil);
        with nPT.FPort.FCOMPort do
        begin
          Tag := FPorts.IndexOf(nPT.FPort);
          OnRxChar := OnComData;

          with Timeouts do
          begin
            ReadTotalConstant := 100;
            ReadTotalMultiplier := 10;
          end;

          with Parity do
          begin
            Bits := nPT.FPort.FParitybit;
            Check := nPT.FPort.FParityCheck;
          end;

          Port := nPT.FPort.FPort;
          BaudRate := nPT.FPort.FRate;
          DataBits := nPT.FPort.FDatabit;
          StopBits := nPT.FPort.FStopbit;
        end;
      end;

      try
        if nOpenPort then
          nPT.FPort.FCOMPort.Open;
        //�����˿�
      except
        on E: Exception do
        begin
          WriteLog(E.Message);
        end;
      end;
    end else

    if nPT.FPort.FConn = ctUDP then
    begin
      try
        if not Assigned(FUDPServer) then
        begin
          FUDPServer := TIdUDPServer.Create;
          FUDPServer.DefaultPort := nPT.FPort.FHostPort;
          FUDPServer.OnUDPRead := DoUDPRead;
        end;

        if not FUDPServer.Active then
          FUDPServer.Active := True;
        Inc(FUDPServerUser); //������·����        
      except
        on E: Exception do
        begin
          WriteLog(E.Message);
        end;
      end;
    end;
    
    Result := True;
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2014-06-11
//Parm: ͨ����
//Desc: �ر�nTunnelͨ����д�˿�
procedure TPoundTunnelManager.ClosePort(const nTunnel: string);
var nPT: PPTTunnelItem;
begin
  FSyncLock.Enter;
  try
    nPT := GetTunnel(nTunnel);
    if not Assigned(nPT) then Exit;

    nPT.FPort.FClientActive := False;
    nPT.FOnData := nil;

    if nPT.FPort.FEventTunnel = nPT then
    begin
      nPT.FPort.FEventTunnel := nPT.FOldEventTunnel;
      //��ԭ����ͨ��

      if Assigned(nPT.FPort.FCOMPort) then
        nPT.FPort.FCOMPort.Close;
      //ͨ��������ر�

      DisconnectClient(nPT.FPort.FClient);
      //�ر���·

      if Assigned(FUDPServer) then
      begin
        if FUDPServerUser > 0 then Dec(FUDPServerUser);
        if FUDPServerUser < 1 then FUDPServer.Active := False;
      end; //�ر�UDP
    end;
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2014-06-11
//Desc: ��ȡ����
procedure TPoundTunnelManager.OnComData(Sender: TObject; Count: Integer);
var nVal: Double;
    nPort: PPTPortItem;
begin
  with TComPort(Sender) do
  begin
    nPort := FPorts[Tag];
    ReadStr(nPort.FCOMBuff, Count);
  end;

  FSyncLock.Enter;
  try
    if not (Assigned(FOnTunnelData) or (Assigned(nPort.FEventTunnel) and
            Assigned(nPort.FEventTunnel.FOnData))) then Exit;
    //�޽����¼�

    nPort.FCOMData := nPort.FCOMData + nPort.FCOMBuff;
    //�ϲ�����

    try
      if ParseWeight(nPort) then
      begin
        nVal := StrToFloat(nPort.FCOMData) * nPort.FDataEnlarge;
        nVal := Float2Float(nVal, nPort.FDataPrecision, False);

        nPort.FCOMValue := nVal;
        nPort.FCOMData := '';
        //clear data

        if Assigned(FOnTunnelData) then
          FOnTunnelData(nPort.FCOMValue, nPort);
        //xxxxx

        if Assigned(nPort.FEventTunnel) then
        begin
          if Assigned(nPort.FEventTunnel.FOnData) then
            nPort.FEventTunnel.FOnData(nPort.FCOMValue);
          //xxxxx


          if Assigned(nPort.FEventTunnel.FOnDataEx) then
            nPort.FEventTunnel.FOnDataEx(nPort.FCOMValue, nPort);
          //xxxxx
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLog(E.Message);
      end;
    end;
  finally
    FSyncLock.Leave;
  end;

  if Length(nPort.FCOMData) >= 5 * nPort.FPackLen then
  begin
    System.Delete(nPort.FCOMData, 1, 4 * nPort.FPackLen);
    WriteLog('��Ч���ݹ���,�Ѳü�.')
  end;
end;

//Date: 2014-06-12
//Parm: �˿�
//Desc: ����nPort�ϵĳ�������
function TPoundTunnelManager.ParseWeight(const nPort: PPTPortItem): Boolean;
var nStr: string;
    nVal: Double;
    i,nIdx,nPos,nEnd: Integer;
begin
  if Assigned(FOnUserParseWeight) then
  begin
    Result := FOnUserParseWeight(nPort);
    Exit;
  end;
  
  Result := False;
  if Length(nPort.FCOMData) < nPort.FPackLen then Exit;
  //���ݲ�����������

  nEnd := -1;
  for nIdx:=Length(nPort.FCOMData) downto 1 do
  begin
    if (nEnd < 1) and (nPort.FCOMData[nIdx] = nPort.FCharEnd) then
    begin
      nEnd := nIdx;
      Continue;
    end;

    if (nEnd < 1) or (nPort.FCOMData[nIdx] <> nPort.FCharBegin) then Continue;
    //�����ݽ������,���ǿ�ʼ���

    nPort.FCOMData := Copy(nPort.FCOMData, nIdx + 1, nEnd - nIdx - 1);
    //�������ͷ������
    nPort.FCOMDataEx := nPort.FCOMData;

    if nPort.FSplitPos > 0 then
    begin
      SplitStr(nPort.FCOMData, FStrList, 0, nPort.FSplitTag);
      //�������

      for nPos:=FStrList.Count - 1 downto 0 do
      begin
        FStrList[nPos] := Trim(FStrList[nPos]);
        if FStrList[nPos] = '' then FStrList.Delete(nPos);
      end; //��������

      if FStrList.Count < nPort.FSplitPos then
      begin
        nPort.FCOMData := '';
        Exit;
      end; //�ֶ�����Խ��

      nPort.FCOMData := FStrList[nPort.FSplitPos - 1];
      //��Ч����
    end;

    if nPort.FInvalidBegin > 0 then
      System.Delete(nPort.FCOMData, 1, nPort.FInvalidBegin);
    //�ײ���Ч����

    if nPort.FInvalidEnd > 0 then
      System.Delete(nPort.FCOMData, Length(nPort.FCOMData)-nPort.FInvalidEnd+1,
                    nPort.FInvalidEnd);
    //β����Ч����

    if nPort.FDataMirror then
      nPort.FCOMData := MirrorStr(nPort.FCOMData);
    //���ݷ�ת

    if nPort.FDataByteHex then
    begin
      nStr := '';
      for i:=1 to Length(nPort.FCOMData) do
        nStr := nStr + IntToHex(Ord(nPort.FCOMData[i]), 2);
      nPort.FCOMData := nStr;
    end; //ÿ�ֽ�16����ƴ��

    nPort.FCOMData := Trim(nPort.FCOMData);
    Result := IsNumber(nPort.FCOMData, True);

    if Result and (nPort.FMaxValue > 0) then
    begin
      nVal := StrToFloat(nPort.FCOMData);
      if FloatRelation(nVal, nPort.FMaxValue, rtGE, 1000) then
        nPort.FCOMData := FloatToStr(nPort.FMaxValid);
      //����ȡ��Чֵ
    end;

    Exit;
    //end loop
  end;
end;

//------------------------------------------------------------------------------
constructor TPoundTunnelConnector.Create(AOwner: TPoundTunnelManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FWaiter := TWaitObject.Create;
  FWaiter.Interval := cPTWait_Short;
end;

destructor TPoundTunnelConnector.Destroy;
begin
  FWaiter.Free;
  inherited;
end;

procedure TPoundTunnelConnector.WakupMe;
begin
  FWaiter.Wakeup;
end;

procedure TPoundTunnelConnector.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure TPoundTunnelConnector.Execute;
var nIdx: Integer;
    nTunnel: PPTTunnelItem;
begin
  while not Terminated do
  try
    FWaiter.EnterWait;
    if Terminated then Exit;

    with FOwner do
    begin
      for nIdx:=FTunnels.Count - 1 downto 0 do
      begin
        nTunnel := FTunnels[nIdx];
        if not nTunnel.FPort.FClientActive then Continue;

        FActiveTunnel := nTunnel;
        FActivePort   := nTunnel.FPort;
        FActiveClient := nTunnel.FPort.FClient;

        ReadPound;
        //��ȡ����
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLog(E.Message);
      Sleep(200);
    end;
  end; 
end;

//Desc: ��ȡ����
function TPoundTunnelConnector.ReadPound: Boolean;
var nVal: Double;
    nBuf: TIdBytes;
begin
  Result := False;
  try
    if not FActiveClient.Connected then
    begin
      FActiveClient.Connect;
      FActivePort.FHostLastActive := GetTickCount();
    end;

    if (FActivePort.FHostNetCheck > 0) and (GetTickCountDiff
       (FActivePort.FHostLastActive) >= FActivePort.FHostNetCheck) then
    begin
      WriteLog(Format('�ذ�[ %s ]������ʱ,���Լ������.', [FActiveTunnel.FID]));
      //xxxxx

      FActivePort.FHostLastActive := GetTickCount();
      nBuf := ToBytes(FActiveClient.IOHandler.ReadByte);
      //��ȡ1���ֽ�,�������쳣������
    end else
    begin
      SetLength(nBuf, 0);
      //init
    end;

    with FActiveClient do
    begin
      nVal := 0;
      while True do
      begin
        IOHandler.CheckForDataOnSource(10);
        //fill the output buffer with a timeout

        if IOHandler.InputBufferIsEmpty then Break;
        nVal := 10;
        IOHandler.InputBuffer.ExtractToBytes(nBuf);
      end;

      if nVal < 1 then Exit;
      FActivePort.FHostLastActive := GetTickCount;
      FActivePort.FCOMBuff := BytesToString(nBuf);
      FActivePort.FCOMData := FActivePort.FCOMData + FActivePort.FCOMBuff;
      //���ݺϲ�
    end;

    if not FOwner.ParseWeight(FActiveTunnel.FPort) then
    begin
      if Length(FActivePort.FCOMData) >= 5 * FActivePort.FPackLen then
      begin
        System.Delete(FActivePort.FCOMData, 1, 4 * FActivePort.FPackLen);
        WriteLog('��Ч���ݹ���,�Ѳü�.')
      end;

      Exit;
    end;

    nVal := StrToFloat(FActivePort.FCOMData) * FActivePort.FDataEnlarge;
    nVal := Float2Float(nVal, FActivePort.FDataPrecision, False);

    FActivePort.FCOMValue := nVal;
    FActivePort.FCOMData := '';
    //clear data

    if Assigned(FOwner.FOnTunnelData) then
      FOwner.FOnTunnelData(FActivePort.FCOMValue, FActivePort);
    //xxxxx

    if Assigned(FActivePort.FEventTunnel) and
       Assigned(FActivePort.FEventTunnel.FOnData) then
      Synchronize(DoSyncEvent);
    Result := True;
  except
    on nErr: Exception do
    begin
      WriteLog(Format('�ذ�[ %s ]����ʧ��,����: %s', [FActiveTunnel.FID,
        nErr.Message]));
      //xxxxx

      FOwner.DisconnectClient(FActiveClient);
      //�ر���·
    end;
  end;
end;

//Desc: �����������¼�
procedure TPoundTunnelConnector.DoSyncEvent;
begin
  if Assigned(FActiveTunnel.FOnData) then
    FActiveTunnel.FOnData(FActivePort.FCOMValue);
  //do event

  if Assigned(FActiveTunnel.FOnDataEx) then
    FActiveTunnel.FOnDataEx(FActivePort.FCOMValue, FActivePort);
  //xxxxx
end;

//Date: 2019-03-09
//Desc: ����UDP���ݰ�,���ⲿ����
procedure TPoundTunnelManager.DoUDPRead(AThread: TIdUDPListenerThread;
  AData: TIdBytes; ABinding: TIdSocketHandle);
var nIdx: Integer;
    nVal: Double;
    nPort: PPTPortItem;
    nTunnel: PPTTunnelItem;
begin
  FSyncLock.Enter;
  try
    for nIdx:=FTunnels.Count-1 downto 0 do
    begin
      nTunnel := FTunnels[nIdx];
      if (nTunnel.FPort.FConn <> ctUDP) or
         (nTunnel.FPort.FHostIP <> ABinding.PeerIP) then Continue;
      //match tunnel

      nPort := nTunnel.FPort;
      try
        if not (Assigned(FOnTunnelData) or (Assigned(nPort.FEventTunnel) and
                Assigned(nPort.FEventTunnel.FOnData))) then Exit;
        //�޽����¼�

        nPort.FCOMBuff := BytesToString(AData, Indy8BitEncoding);
        //udp data

        if ParseWeight(nPort) then
        begin
          nVal := StrToFloat(nPort.FCOMData) * nPort.FDataEnlarge;
          nVal := Float2Float(nVal, nPort.FDataPrecision, False);

          nPort.FCOMValue := nVal;
          nPort.FCOMData := '';
          //clear data

          if Assigned(FOnTunnelData) then
            FOnTunnelData(nPort.FCOMValue, nPort);
          //xxxxx

          if Assigned(nPort.FEventTunnel) then
          begin
            if Assigned(nPort.FEventTunnel.FOnData) then
              nPort.FEventTunnel.FOnData(nPort.FCOMValue);
            //xxxxx
                        
            if Assigned(nPort.FEventTunnel.FOnDataEx) then
              nPort.FEventTunnel.FOnDataEx(nPort.FCOMValue, nPort);
            //xxxxx
          end;
        end;
      except
        on E: Exception do
        begin
          WriteLog(E.Message);
        end;
      end;
    end;
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-07-16
//Parm: ͨ��;ֵ
//Desc: ����nTunnel��ֵ
procedure TPoundTunnelManager.SetData(const nTunnel: string; const nValue: Double);
var nVal: Double;
    nPT: PPTTunnelItem;
begin
  FSyncLock.Enter;
  try
    nPT := GetTunnel(nTunnel);
    if Assigned(nPT) then
    try
      if not (Assigned(FOnTunnelData) or (Assigned(nPT.FPort.FEventTunnel) and
              Assigned(nPT.FPort.FEventTunnel.FOnData))) then Exit;
      //no event

      nVal := nValue * nPT.FPort.FDataEnlarge;
      nVal := Float2Float(nVal, nPT.FPort.FDataPrecision, False);
      //adjust precision

      nPT.FPort.FCOMValue := nVal;
      nPT.FPort.FCOMData := '';
      //clear data

      if Assigned(FOnTunnelData) then
        FOnTunnelData(nVal, nPT.FPort);
      //xxxxx

      if Assigned(nPT.FPort.FEventTunnel) then
      begin
        if Assigned(nPT.FPort.FEventTunnel.FOnData) then
           nPT.FPort.FEventTunnel.FOnData(nVal);
        //xxxxx
                
        if Assigned(nPT.FPort.FEventTunnel.FOnDataEx) then
           nPT.FPort.FEventTunnel.FOnDataEx(nVal, nPT.FPort);
        //xxxxx
      end;
    except
      on nErr: Exception do
      begin
        WriteLog('SetData Error: ' + nErr.Message);
      end;
    end;
  finally
    FSyncLock.Leave;
  end;
end;

initialization
  gPoundTunnelManager := nil;
finalization
  FreeAndNil(gPoundTunnelManager);
end.
