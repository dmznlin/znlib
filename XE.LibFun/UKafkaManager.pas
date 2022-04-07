{*******************************************************************************
  作者: dmzn@163.com 2022-04-07
  描述: 实现kafka生产者、消费者管理器
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
  //消费者事件,回调函数

  TKKConsumer = record
  private
    FValid          : Boolean;                    //valid flag
    FOwner          : TObject;                    //对象
    FIdent          : Cardinal;                   //标识
    FTopicID        : string;                     //关注的topic

    FConfig         : TKKConfig;                  //kafa config
    FTopic          : TKKTopicConfig;             //topic config
    FClient         : TKKClient;                  //consumer handle
  public
    FOnMsg          : TKKMessageProcedure;        //消息过程
    FOnMsgEvent     : TKKMessageEvent;            //消息事件
    FOnMsgUI        : TKKMessageProcedure;        //界面消息过程
    FOnMsgEventUI   : TKKMessageEvent;            //界面消息事件
  public
    property Owner: TObject read FOwner;
    property Ident: Cardinal read FIdent;
    {*属性相关*}
  end;
  TKKConsumers = TArray<TKKConsumer>;

  TKafkaManager = class(TManagerBase)
  private
    FMessages: TKKMessages;
    {*消息缓冲*}
    FConsumerBaseID: Cardinal;
    FConsumers: TKKConsumers;
    {*消费者列表*}
    FValidMessage: Cardinal;
    FValidConsumer: Cardinal;
    {*有效个数*}
    FDefaultConfigFile: string;
    {*默认配置*}
  protected
    procedure ResetConsumer(const nIdx: Integer);
    {*重置消费者*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*延迟执行*}
    procedure LoadConfig(nFile: string = '');
    procedure SaveDefault(nFile: string = '');
    {*配置文件*}
    function ConsumerInit(const nTopicID: string;
      const nOwner: TObject = nil): TKKConsumer;
    procedure ConsumerAdd(const nConfig: PKKConsumer);
    procedure ConsumerDel(const nOwner: TObject; const nID: Cardinal = 0);
    {*消费者*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
    procedure WriteLog(const nEvent: string);
    {*记录日志*}
    property NumMessage: Cardinal read FValidMessage;
    property NumConsumer: Cardinal read FValidConsumer;
    {*属性事件*}
  end;

var
  gKafkaManager: TKafkaManager = nil;
  //全局使用

implementation

uses
  UManagerGroup, ULibFun, NativeXml;

procedure TKafkaManager.WriteLog(const nEvent: string);
begin
  gMG.WriteLog('TKafkaManager', 'KAFKA管理器', nEvent);
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
//Parm: 是否注册
//Desc: 向系统注册管理器对象
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
  //启用全局变量
end;

//Date: 2022-04-07
//Desc: 启动kafka服务
procedure TKafkaManager.RunAfterRegistAllManager;
begin
  inherited;

end;

//Date: 2022-04-07
//Desc: 停止kafka服务
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
//Parm: 文件
//Desc: 保存默认配置到nFile中
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
//Parm: 主题标识;关联对象
//Desc: 初始化一个由nOwner拥有的,关注nTopicID主题的消费者
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
//Parm: 消费者配置
//Desc: 添加一个消费者
procedure TKafkaManager.ConsumerAdd(const nConfig: PKKConsumer);
begin

end;

//Date: 2022-04-07
//Parm: 对象;标识
//Desc: 使用nOwner或nID删除指定消费者
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
//Parm: 索引
//Desc: 重置nIdx消费者
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
