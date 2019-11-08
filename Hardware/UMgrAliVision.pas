{*******************************************************************************
  ����: dmzn@163.com 2019-09-25
  ����: ����ͼ��ʶ��ӿ�����

  ��ע:
  *.�������:
    1.����������뿪�ذ��¼�.
    2.�����Ƿ񵥳���ȫ�ϰ����.
    3.��ǰ�ڵذ��ϵ���Ч���ƺ�.
*******************************************************************************}
unit UMgrAliVision;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, IdSocketHandle, IdGlobal, IdStack,
  IdUDPBase, IdUDPServer, NativeXml, superobject, UWaitItem, ULibFun,
  USysLoger;

type
  TTruckState = (tsNone, tsNewOn, tsLeave, tsNormal, tsOut);
  //����״̬:�޳�,�³��ϰ�,�����°�,������ȫ�ϰ�,����Խ��(δ��ȫ�ϰ�,�೵)
  TTruckStates = set of TTruckState;

  TPoundLocation = record
    FLeftTop: TPoint;
    FLeftBottom: TPoint;
    FRightTop: TPoint;
    FRightBottom: TPoint;                         //�ذ��Ľ�����
  end;

  TLineLocation = record
    FStart: TPoint;
    FEnd: TPoint;                                 //�������ʼ����������
  end;

  PCameraItem = ^TCameraItem;
  TCameraItem = record
    FChannel: string;                             //ͨ�����
    FName: string;                                //�������
    FReader: string;                              //��Ӧ������

    FResolution: TPoint;                          //������(1920x1080)
    FPoundLoc: TPoundLocation;                    //�ذ�����
    FLineLoc: TLineLocation;                      //���������

    {*����Ϊͼ��ʶ��ϵͳ�ϱ�����*}
    FTruck: string;                               //��ǰ����
    FTruckUpdate: Cardinal;                       //���ݸ���
    FState: TTruckState;                          //��ǰ״̬
    FStateUpdate: Cardinal;                       //���ݸ���
  end;
  TCameraItems = array of TCameraItem;            //����б�

  PPoundItem = ^TPoundItem;
  TPoundItem = record
    FID: string;                                  //ͨ����ʶ
    FName: string;                                //ͨ������
    FTruck: string;                               //��ǰ����
    FTruckPrev: string;                           //��һ����

    FLastIn: Cardinal;                            //�ϴν���
    FLastOut: Cardinal;                           //�ϴ��˳�
    FStateNow: TTruckState;                       //��ǰ״̬
    FStateNew: TTruckStates;                      //����״̬

    FValStr: string;
    FValInt: Integer;
    FValFloat: Double;                            //��������
    FCameras: TCameraItems;                       //����б�
  end;
  TPoundtems = array of TPoundItem;               //��վ�б�

  TTruckManager = class;
  TTruckMonitor = class(TThread)
  private
    FOwner: TTruckManager;
    //ӵ����
    FWaiter: TWaitObject;
    //�ȴ�����
  protected
    procedure Execute; override;
    procedure DoExecute;
    //ִ��ҵ��
  public
    constructor Create(AOwner: TTruckManager);
    destructor Destroy; override;
    //�����ͷ�
    procedure Wakeup;
    procedure StopMe;
    //��ͣͨ��
  end;

  TOnTruckStatusProc = procedure (const nPound: PPoundItem);
  TOnTruckStatusEvent = procedure (const nPound: PPoundItem) of Object;

  TTruckManager = class(TObject)
  private
    FPounds: TPoundtems;
    //�ذ��б�
    FMonitor: TTruckMonitor;
    //�ػ�����
    FStatusUpdateInerval: Word;
    //״̬���
    FUDPPort: Word;
    FUDPServer: TIdUDPServer;
    //UDP����
    FSyncLock: TCriticalSection;
    //ͬ������
    FOnStatusProc: TOnTruckStatusProc;
    FOnStatusEvent: TOnTruckStatusEvent;
    ///�¼����
  protected
    function FindCamera(const nChannel: string; var nPound,nCamera: Integer): Boolean;
    //��������
    procedure DoUDPRead(AThread: TIdUDPListenerThread;
      AData: TIdBytes; ABinding: TIdSocketHandle);
    //UDP�ϱ�
    function AdjustTruck(const nTruck: string): string;
    //����У��
  public
    constructor Create;
    destructor Destroy; override;
    //�����ͷ�
    procedure LoadConfig(const nFile: string);
    //��������
    procedure StartService;
    procedure StopService;
    //��ͣ����
    function GetPoundData(const nID: string; var nData: TPoundItem): Boolean;
    procedure SetPoundData(const nID: string; const nData: PPoundItem = nil);
    //��ȡ����
    property UDPPort: Word read FUDPPort write FUDPPort;
    property OnStatusChange: TOnTruckStatusProc read FOnStatusProc write FOnStatusProc;
    property OnStatusChangeEvent: TOnTruckStatusEvent read FOnStatusEvent write FOnStatusEvent;
    //�������
  end;

var
  gVisionManager: TTruckManager = nil;
  //ȫ��ʹ��

implementation

type
  TProvinceName = record
    FName: string;
    FPinYin: string;
  end;

const
  cTruckNull = '0'; //�ճ���

  cTruckProvince: array[0..32] of TProvinceName = (
    (FName: '��'; FPinYin: 'Anhui'),
    (FName: '��'; FPinYin: 'Beijing'),
    (FName: '��'; FPinYin: 'Chongqing'),
    (FName: '��'; FPinYin: 'Fujian'),
    (FName: '��'; FPinYin: 'Gansu'),
    (FName: '��'; FPinYin: 'Guangdong'),
    (FName: '��'; FPinYin: 'Guangxi'),
    (FName: 'ǭ'; FPinYin: 'Guizhou'),
    (FName: '��'; FPinYin: 'Hainan'),
    (FName: '��'; FPinYin: 'Hebei'),
    (FName: '��'; FPinYin: 'Heilongjiang'),
    (FName: 'ԥ'; FPinYin: 'Henan'),
    (FName: '��'; FPinYin: 'HongKong'),
    (FName: '��'; FPinYin: 'Hubei'),
    (FName: '��'; FPinYin: 'Hunan'),
    (FName: '��'; FPinYin: 'InnerMongolia'),
    (FName: '��'; FPinYin: 'Jiangsu'),
    (FName: '��'; FPinYin: 'Jiangxi'),
    (FName: '��'; FPinYin: 'Jilin'),
    (FName: '��'; FPinYin: 'Liaoning'),
    (FName: '��'; FPinYin: 'Macau'),
    (FName: '��'; FPinYin: 'Ningxia'),
    (FName: '��'; FPinYin: 'Qinghai'),
    (FName: '��'; FPinYin: 'Shaanxi'),
    (FName: '³'; FPinYin: 'Shandong'),
    (FName: '��'; FPinYin: 'Shanghai'),
    (FName: '��'; FPinYin: 'Shanxi'),
    (FName: '��'; FPinYin: 'Sichuan'),
    (FName: '��'; FPinYin: 'Tianjin'),
    (FName: '��'; FPinYin: 'Tibet'),
    (FName: '��'; FPinYin: 'Xinjiang'),
    (FName: '��'; FPinYin: 'Yunnan'),
    (FName: '��'; FPinYin: 'Zhejiang')
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
//Parm: ͨ�����;��վ,���
//Desc: ����ָ�����
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
//Parm: ��ʶ
//Desc: ��ȡnID�ĵ�ǰ����
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
//Parm: �ذ���ʶ
//Desc: ����nID�ĸ�������
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
      nStr := 'ͨ�����Ϊ[ %s ]�����������';
      raise Exception.Create(Format(nStr, [nNode.AsString]));
    end;

    with FPounds[nPound] do
    try
      FSyncLock.Enter;
      nNode := nRoot.O['lr_result'];

      if Assigned(nNode) then //����ʶ��
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
      if Assigned(nNode) then //�������
      begin
        nStr := nNode.AsString;
        if nStr = '0' then                        //�޳�
             FCameras[nCamera].FState := tsNone
        else if nStr = '1' then
             FCameras[nCamera].FState := tsNormal //�г��Ҳ�Խ��
        else FCameras[nCamera].FState := tsOut;   //Խ��

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
//Parm: ���ƺ�
//Desc: У��nTruck�е��ض��ַ�
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
          nStr := '���[ %s,%s ]����������Ч';
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
        nStr := '���[ %s,%s ]���ڰ�վ[ %s ]������';
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
        //δ������� �� û����
        
        FStateNew := [];
        nTruckNormal := True;
        nTruckExists := False;

        for nIdx:=Low(FCameras) to High(FCameras) do
        begin
          if GetTickCountDiff(FCameras[nIdx].FStateUpdate) > FStatusUpdateInerval then
            FCameras[nIdx].FState := tsNone;
          //���³�ʱ��Ϊ��Ч

          if FCameras[nIdx].FState = tsOut then
          begin
            nTruckNormal := False;
            //δ��ȫ�ϰ�
            FStateNew := FStateNew + [tsOut] - [tsNormal];
            //����Խ��
          end;

          if (FCameras[nIdx].FState <> tsNone) or
             (GetTickCountDiff(FCameras[nIdx].FTruckUpdate) < FStatusUpdateInerval) then
          begin
            nTruckExists := True;
            //�����ڰ���,���ƺŸո���
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
          //������ȫ�ڰ�
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
              //�³��ϰ�
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

    //�����¼�
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

    if (tsNewOn in FStateNew) and (FStateNow <> tsNewOn) then //�³��ϰ�
    begin
      FStateNow := tsNewOn;
      if FTruck <> cTruckNull then
        DoEvent;
      //xxxxx
    end;

    if (tsLeave in FStateNew) and (FStateNow <> tsLeave) then //�������
    begin
      FStateNow := tsLeave;
      if FTruckPrev <> cTruckNull then
        DoEvent;
      //xxxxx
    end;

    if (tsNormal in FStateNew) and (FStateNow <> tsNormal) then //��ȫ�ϰ�
    begin
      FStateNow := tsNormal;
      if FTruck <> cTruckNull then
        DoEvent;
      //xxxxx
    end;

    if (tsOut in FStateNew) and (FStateNow <> tsOut) then //δ��ȫ�ϰ�
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
