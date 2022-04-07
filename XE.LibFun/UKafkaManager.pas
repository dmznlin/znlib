{*******************************************************************************
  ����: dmzn@163.com 2022-04-07
  ����: ʵ��kafka�����ߡ������߹�����
*******************************************************************************}
unit UKafkaManager;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, UBaseObject, UThreadPool, UWaitItem,
  UKafkaTypes;

type
  PKKMessage = ^TKKMessage;
  TKKMessage = record
  private
    FValid          : Boolean;                    //valid flag
  public
    FPartition      : Integer;                    //topic partition
    FKey            : TBytes;                     //message key
    FData           : TBytes;                     //message data
  end;
  TKKMessages = TArray<TKKMessage>;

  PKKConsumer = ^TKKConsumer;
  TKKMessageProcedure = procedure (const nConsumer: PKKConsumer;
    const nMsg: PKKMessage);
  TKKMessageEvent = procedure (const nConsumer: PKKConsumer;
    const nMsg: PKKMessage) of object;
  //�������¼�,�ص�����

  TKKConsumer = record
  private
    FValid          : Boolean;                    //valid flag
    FOwner          : TObject;                    //����
    FIdent          : Cardinal;                   //��ʶ
    FTopicID        : string;                     //��ע��topic

    FConfig         : TKKConfig;                  //kafa config
    FTopic          : TKKTopicConfig;             //topic config
    FClient         : TKKClient;                  //consumer handle
  public
    FOnMsg          : TKKMessageProcedure;        //��Ϣ����
    FOnMsgEvent     : TKKMessageEvent;            //��Ϣ�¼�
    FOnMsgUI        : TKKMessageProcedure;        //������Ϣ����
    FOnMsgEventUI   : TKKMessageEvent;            //������Ϣ�¼�
  public
    property Owner: TObject read FOwner;
    property Ident: Cardinal read FIdent;
    {*�������*}
  end;
  TKKConsumers = TArray<TKKConsumer>;

  TKafkaManager = class(TManagerBase)
  private
    FMessages: TKKMessages;
    {*��Ϣ����*}
    FConsumerBaseID: Cardinal;
    FConsumers: TKKConsumers;
    {*�������б�*}
    FValidMessage: Cardinal;
    FValidConsumer: Cardinal;
    {*��Ч����*}
    FDefaultConfigFile: string;
    {*Ĭ������*}
  protected
    procedure ResetConsumer(const nIdx: Integer);
    {*����������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*�ӳ�ִ��*}
    procedure LoadConfig(nFile: string = '');
    procedure SaveDefault(nFile: string = '');
    {*�����ļ�*}
    function ConsumerInit(const nTopicID: string;
      const nOwner: TObject = nil): TKKConsumer;
    procedure ConsumerAdd(const nConfig: PKKConsumer);
    procedure ConsumerDel(const nOwner: TObject; const nID: Cardinal = 0);
    {*������*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
    procedure WriteLog(const nEvent: string);
    {*��¼��־*}
    property NumMessage: Cardinal read FValidMessage;
    property NumConsumer: Cardinal read FValidConsumer;
    {*�����¼�*}
  end;

var
  gKafkaManager: TKafkaManager = nil;
  //ȫ��ʹ��

implementation

uses
  UManagerGroup, ULibFun, NativeXml;

procedure TKafkaManager.WriteLog(const nEvent: string);
begin
  gMG.WriteLog('TKafkaManager', 'KAFKA������', nEvent);
end;

constructor TKafkaManager.Create;
begin
  FValidMessage := 0;
  FValidConsumer := 0;
  FConsumerBaseID := 1;

  SetLength(FMessages, 0);
  SetLength(FConsumers, 0);
  FDefaultConfigFile := TApplicationHelper.gPath + 'kafka.xml';
end;

destructor TKafkaManager.Destroy;
begin

  inherited;
end;

//Date: 2022-04-07
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TKafkaManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TKafkaManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TKafkaManager.Create;
    gMG.FKafkaManager := gMG.FManagers[nIdx].FManager as TKafkaManager;
  end else
  begin
    gMG.FKafkaManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gKafkaManager := gMG.FKafkaManager;
  //����ȫ�ֱ���
end;

//Date: 2022-04-07
//Desc: ����kafka����
procedure TKafkaManager.RunAfterRegistAllManager;
begin
  inherited;

end;

//Date: 2022-04-07
//Desc: ֹͣkafka����
procedure TKafkaManager.RunBeforUnregistAllManager;
begin
  inherited;

end;

procedure TKafkaManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
begin
    with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      nList.Add('MessageTotal=' + Length(FMessages).ToString);
      nList.Add('MessageValid=' + FValidMessage.ToString);
      nList.Add('ConsumerTotal=' + Length(FConsumers).ToString);
      nList.Add('ConsumerValid=' + FValidConsumer.ToString);
      Exit;
    end;

    nList.Add(FixData('MessageTotal:', Length(FMessages).ToString));
    nList.Add(FixData('MessageValid:', FValidMessage.ToString));
    nList.Add(FixData('ConsumerTotal:', Length(FConsumers).ToString));
    nList.Add(FixData('ConsumerValid:', FValidConsumer.ToString));
  finally
    SyncLeave;
  end;
end;

procedure TKafkaManager.LoadConfig(nFile: string);
begin

end;

//Date: 2022-04-07
//Parm: �ļ�
//Desc: ����Ĭ�����õ�nFile��
procedure TKafkaManager.SaveDefault(nFile: string);
type
  TAddFun = reference to procedure (const nName: string;
    const nType: TKKConfigType);
  //function define
var nXML: TNativeXML;
    nNode,nTmp: TXMLNode;
    nAddNode: TAddFun;
begin
  nXML := TNativeXml.Create(nil);
  with nXML do
  try
    if (nFile <> FDefaultConfigFile) and (not FileExists(nFile)) then
      nFile := FDefaultConfigFile;
    //xxxxx

    if FileExists(nFile) then
    begin
      LoadFromFile(nFile);
      //load config
    end else
    begin
      VersionString := '1.0';
      Charset := 'gb2312';
      Root.Name := 'kafka_service';
    end;

    nNode := Root.NodeFindOrCreate('default');
    //first node
    nNode.Clear;

    nAddNode :=
      procedure (const nName: string; const nType: TKKConfigType)
      var nIdx: Integer;
      begin
        nTmp := nNode.NodeNew(nName);
        with TKafaConfig do
         for nIdx := Low(ConfigKeys) to High(ConfigKeys) do
          if ConfigTypes[nIdx] = nType then
           nTmp.NodeNew(ConfigKeys[nIdx]).AttributeAdd('set', '');
        //global config

        with TTopicConfig do
         for nIdx := Low(ConfigKeys) to High(ConfigKeys) do
          if ConfigTypes[nIdx] = nType then
           nTmp.NodeNew(ConfigKeys[nIdx]).AttributeAdd('set', '');
        //topic config
      end;

    nAddNode('both', B);
    nAddNode('consumer', C);
    nAddNode('producer', P);

    XmlFormat := xfReadable;
    nXML.SaveToFile(nFile);
    //save config
  finally
    nXML.Free;
  end;
end;

//------------------------------------------------------------------------------
//Date: 2022-04-07
//Parm: �����ʶ;��������
//Desc: ��ʼ��һ����nOwnerӵ�е�,��עnTopicID�����������
function TKafkaManager.ConsumerInit(const nTopicID: string;
  const nOwner: TObject): TKKConsumer;
var nInit: TKKConsumer;
begin
  SyncEnter;
  try
    if FConsumerBaseID < High(Cardinal) then
         Inc(FConsumerBaseID)
    else FConsumerBaseID := 1;

    FillChar(nInit, SizeOf(TKKConsumer), #0);
    Result := nInit;
    //copy default

    with Result do
    begin
      FValid := True;
      FOwner := nOwner;
      FIdent := FConsumerBaseID;
      FTopicID := nTopicID;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2022-04-07
//Parm: ����������
//Desc: ���һ��������
procedure TKafkaManager.ConsumerAdd(const nConfig: PKKConsumer);
begin

end;

//Date: 2022-04-07
//Parm: ����;��ʶ
//Desc: ʹ��nOwner��nIDɾ��ָ��������
procedure TKafkaManager.ConsumerDel(const nOwner: TObject; const nID: Cardinal);
var nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FConsumers) to High(FConsumers) do
     with FConsumers[nIdx] do
      if FValid and ((Assigned(nOwner) and (nOwner = FOwner)) or
                    ((nID > 0) and (nID = FIdent))) then
      begin
        ResetConsumer(nIdx);
        Break;
      end;
  finally
    SyncLeave;
  end;
end;

//Date: 2022-04-07
//Parm: ����
//Desc: ����nIdx������
procedure TKafkaManager.ResetConsumer(const nIdx: Integer);
begin
  with FConsumers[nIdx] do
  begin
    if FValid then
    begin
      Dec(FValidConsumer);
      //update counter
      FValid := False;
    end;
  end;
end;

end.
