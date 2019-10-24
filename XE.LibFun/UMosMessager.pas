{*******************************************************************************
  ����: dmzn@163.com 2019-08-15
  ����: ����mosquitto����Ϣ����

  ��ע:
  *.MQTT������ʹ��·����ʽ����: Root/Path1/Path2/Path3/Path4...
    һ��Root:Broker����,��: TXYun37,ָ��������Ѷ����IPĩλ��37�ķ�����.
    ����Path:Ӧ������,��: Delivery,ָ��������ϵͳ
    ����Path:Ӧ��ʵ������,��:HuaXin_YX,ָ���¼������¹���
    �ļ�Path:Ӧ�ö����ʶ,��:TMgrOPC,ָOPC�������.
    �弶Path:��������,��:Info,Error,Warn��
    �����弶·��Ϊ��Ҫ·��,�����ѡ,·���������1024�ֽ�.
*******************************************************************************}
unit UMosMessager;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, UMosMQTT, UBaseObject;

type
  TMsgType = (mtInfo, mtWarn, mtError, mtEvent, mtIRC, mtCmd, mtWaitCmd);
  //��Ϣ����: ��Ϣ;����;����;�¼�;��ʱͨѶ;����;����ȴ����
  TMsgTypes = set of TMsgType;

const
  sMsgType: array[TMsgType] of string = ('Info', 'Warn', 'Error', 'Event',
    'IRC', 'Command', 'CmdWait');
  //��Ϣ��������

type
  PMsgPath = ^TMsgPath;
  TMsgPath = record
    FServer   : string;                   //����������
    FAppName  : string;                   //Ӧ������
    FInstance : string;                   //ʵ������
    FObject   : string;                   //�����ʶ
    FType     : TMsgType;                 //��Ϣ����
    FExtend   : string;                   //��չ·��
  end;

  PMsgData = ^TMsgData;
  TMsgData = record
    FPath: TMsgPath;                      //��Ϣ·��
    FData: string;                        //��Ϣ����
  end;

  TMsgCallProc = procedure(const nMsg: PMsgData);
  TMsgCallEvent = procedure(const nMsg: PMsgData) of object;

  PMsgReceiver = ^TMsgReceiver;
  TMsgReceiver = record
    FID   : string;                       //���ձ�ʶ
    FName : string;                       //��������
    FPath : TMsgPath;                     //��ע·��
    FTypes: TMsgTypes;                    //��ע����
    FCallProc: TMsgCallProc;              //�ص�����
    FCallEvent: TMsgCallEvent;            //�ص��¼�
  end;

  TMQTTMessager = class(TManagerBase)
  private
    FChannel: TMQTTClient;
    {*��Ϣͨ��*}
    FDefaultPath: TMsgPath;
    {*Ĭ��·��*}
    FReceivers: TList;
    {*��Ϣ������*}
  protected
    procedure DoRecvMessage(const nTopic,nPayload: string);
    {*������Ϣ*}
    function ParsePath(const nTopic: string; const nPath: PMsgPath): Boolean;
    {*����·��*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    procedure RunBeforApplicationHalt; override;
    {*�ӳ�ִ��*}
    procedure InitPath(const nServer,nAppName,nInstance: string);
    {*����·��*}
    procedure InitReceiver(const nReceiver: PMsgReceiver);
    function MakeTopic(const nReceiver: PMsgReceiver): string;
    {*������Ϣ*}
    property Channel: TMQTTClient read FChannel;
    property DefaultPath: TMsgPath read FDefaultPath;
    {*�������*}
  end;

var
  gMQTTMessager: TMQTTMessager = nil;
  //ȫ��ʹ��

implementation

uses
  UManagerGroup, ULibFun;

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TMQTTMessager, 'ϵͳ��Ϣ����', nEvent);
end;

constructor TMQTTMessager.Create;
begin
  FChannel := nil;
  FReceivers := nil;
  InitPath('Moquitto', 'Delivery', 'Factory');
end;

destructor TMQTTMessager.Destroy;
begin
  //xxx
  inherited;
end;

//Date: 2019-08-15
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TMQTTMessager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TMQTTMessager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TMQTTMessager.Create;
    gMG.FMessageCenter := gMG.FManagers[nIdx].FManager as TMQTTMessager;
  end else
  begin
    gMG.FMessageCenter := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;
end;

//Date: 2019-08-30
//Desc: ����MQTTͨ��
procedure TMQTTMessager.RunAfterRegistAllManager;
begin
  FReceivers := TList.Create;
  FChannel := TMQTTClient.Create;

  with FChannel do
  begin
    EventMode := emThread;
    OnMessageEvent := DoRecvMessage;
  end;
end;

//Date: 2019-08-30
//Desc: ����׼���ر�,�Ͽ�����
procedure TMQTTMessager.RunBeforApplicationHalt;
begin
  FChannel.Disconnect;
end;

//Date: 2019-08-30
//Desc: ������Դ
procedure TMQTTMessager.RunBeforUnregistAllManager;
var nIdx: Integer;
begin
  for nIdx := FReceivers.Count - 1 downto 0 do
  if Assigned(FReceivers[nIdx]) then
  begin
    Dispose(PMsgReceiver(FReceivers[nIdx]));
    FReceivers[nIdx] := nil;
  end;

  FReceivers.Free;
  FChannel.Free;
end;

//Date: 2019-08-15
//Parm: ����;Ӧ��;ʵ��
//Desc: ��ʼ��Ĭ��·��
procedure TMQTTMessager.InitPath(const nServer, nAppName, nInstance: string);
var nDef: TMsgPath;
begin
  FillChar(nDef, SizeOf(TMsgPath), #0);
  FDefaultPath := nDef;

  with FDefaultPath do
  begin
    FServer   := nServer;
    FAppName  := nAppName;
    FInstance := nInstance;
    FType     := mtInfo;
  end;
end;

//Date: 2019-08-30
//Parm: ��Ϣ�������
//Desc: ��ʼ��nReceiver���ݽṹ
procedure TMQTTMessager.InitReceiver(const nReceiver: PMsgReceiver);
var nDef: TMsgReceiver;
begin
  FillChar(nDef, SizeOf(TMsgReceiver), #0);
  nReceiver.FID := gMG.FSerialIDManager.GetSID;
  nReceiver.FPath := FDefaultPath;
end;

//Date: 2019-08-30
//Parm: ��Ϣ�������
//Desc: ����nReceiver������������
function TMQTTMessager.MakeTopic(const nReceiver: PMsgReceiver): string;
begin
  with nReceiver.FPath do
  begin
    if FServer = '' then
         Result := '+/'
    else Result := FServer + '/'; //L1

    if FAppName = '' then
         Result := Result + '+/'
    else Result := Result + FAppName + '/'; //L2

    if FInstance = '' then
         Result := Result + '+/'
    else Result := Result + FInstance + '/'; //L3

    if FObject = '' then
         Result := Result + '+/'
    else Result := Result + FObject + '/'; //L4

    if nReceiver.FTypes = [] then
         Result := Result + '+/'
    else Result := Result + FAppName + '/'; //L5
  end;
end;

//Date: 2019-08-26
//Parm: ����;·��
//Desc: �������ⶨ�����nTopic
function TMQTTMessager.ParsePath(const nTopic: string;
  const nPath: PMsgPath): Boolean;
var nStr: string;
    nType: TMsgType;
    nIdx,nNow,nPos: Integer;
begin
  Result := False;
  //default

  nNow := 1;
  nIdx := 0;
  nPos := Pos('/', nTopic);

  while nPos > 0 do
  begin
    if nIdx < 5 then
    begin
      nStr := Copy(nTopic, nNow, nPos - nNow);
      //section text
    end else
    begin
      nStr := Copy(nTopic, nNow, Length(nTopic) - nNow + 1);
      //all of last
    end;

    case nIdx of
     0: nPath.FServer := nStr;
     1: nPath.FAppName := nStr;
     2: nPath.FInstance := nStr;
     3: nPath.FObject := nStr;
     5: //last section
      begin
        nPath.FExtend := nStr;
        Inc(nIdx);
        Break;
      end;
    end;

    if nIdx = 4 then //message type
    begin
      for nType := Low(TMsgType) to High(TMsgType) do
      if nStr = sMsgType[nType] then
      begin
        nPath.FType := nType;
        Result := True;
        Break;
      end;

      if not Result then
        Exit;
      //invalid path
    end;

    Inc(nIdx);
    nNow := nPos + 1;
    nPos := Pos('/', nTopic, nNow);
  end;

  if (nIdx = 5) and (nNow < Length(nTopic)) then
       nPath.FExtend := Copy(nTopic, nNow, Length(nTopic) - nNow + 1)
  else nPath.FExtend := '';
end;

//Date: 2019-08-26
//Parm: ����;����
//Desc: ������������
procedure TMQTTMessager.DoRecvMessage(const nTopic, nPayload: string);
var nPath: TMsgPath;
begin
  if not ParsePath(nTopic, @nPath) then Exit;
  //invalid path

  WriteLog(nTopic + ' ' + nPath.FExtend);
end;

end.
