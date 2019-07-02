{*******************************************************************************
  作者: dmzn@163.com 2019-06-10
  描述: 基于mosquitto的MQTT客户端

  备注:
  *.TMQTTPublishItem.FQos: 消息等级
    level 0: 最多一次的传输,消息可能到达服务器1次,也可能根本不会到达.
    level 1: 至少一次的传输,服务器接收到消息会被确认,通过传输一个PUBACK信息.
      如果有一个可以辨认的传输失败,无论是通讯连接还是发送设备,还是过了一段时间
      确认信息没有收到,发送方都会将消息头的DUP位置1,然后再次发送消息.
    level 2: 只有一次的传输,在level 1上附加的协议流保证了重复的消息不会传送到
      接收的应用.
  *.TMQTTPublishItem.FRetain: 保留消息定义
    Broker会存储每个Topic的最后一条保留消息及其Qos,当订阅该Topic的客户端上线后,
    Broker需要将该消息投递给它.
*******************************************************************************}
unit UMosMQTT;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, Winapi.Windows, Vcl.PostMsg,
  System.AnsiStrings, UMosquitto, UManagerGroup, UThreadPool, ULibFun,
  ULibConst;

type
  PMQTTTopicItem = ^TMQTTTopicItem;
  TMQTTTopicItem = record
    FEnabled: Boolean;                       //有效标识
    FTopic: AnsiString;                      //主题名称
    FChannel: Integer;                       //订阅编号
    FQos: Integer;                           //qos
    FHasSub: Boolean;                        //已订阅
    FLastSub: Cardinal;                      //发起订阅
  end;
  TMQTTTopicItems = array of TMQTTTopicItem;

  PMQTTPublishItem = ^TMQTTPublishItem;
  TMQTTPublishItem = record
    FEnabled: Boolean;                       //有效标识
    FTopic: AnsiString;                      //主题
    FPayload: string;                        //数据
    FQos: Integer;                           //qos
    FRetain: Boolean;                        //是否保留
    FLastPub: Cardinal;                      //发布时间
  end;
  TMQTTPublishItems = array of TMQTTPublishItem;

  PMQTTClientData = ^TMQTTClientData;
  TMQTTClientData = record
    FClient: TObject;                        //客户端对象
    FServerHost: string;
    FServerPort: Integer;                    //Broker
    FLastConnecting: Cardinal;               //连接时间

    FThreadRunning: Boolean;                 //运行中
    FDisconnecting: Boolean;                 //正在断开
    FSubscribeAllTopics: Boolean;            //全部订阅
    FHasNewPublishItem: Boolean;             //有待发消息
  end;

  TMQTTEventType = (etConn, etMsg);
  //事件类型

  PMQTTEventItem = ^TMQTTEventItem;
  TMQTTEventItem = record
    FEnabled: Boolean;                       //有效标识
    FLastUsed: Cardinal;                     //启用时间
    FEventType: TMQTTEventType;              //类型标识

    FConnected: Boolean;                     //连接状态
    FTopic: string;                          //主题
    FPayload: string;                        //数据内容
  end;
  TMQTTEventItems = array of TMQTTEventItem;

  TMQTTOnConnect = procedure (const nServer: string; const nPort: Integer;
    const nConnected: Boolean);
  TMQTTOnMessage = procedure (const nTopic,nPayload: string);
  TMQTTOnConnectEvent = procedure (const nServer: string; const nPort: Integer;
    const nConnected: Boolean) of object;
  TMQTTOnMessageEvent = procedure (const nTopic,nPayload: string) of object;

  TMQTTClient = class(TObject)
  private
    FServerHost: string;
    FServerPort: Integer;
    FUserName: string;
    FPassword: string;
    FKeepAlive: Integer;
    {*服务器*}
    FClientID: string;
    FClient: p_mosquitto;
    FClientData: TMQTTClientData;
    {*客户端*}
    FTopics: TMQTTTopicItems;
    FPublishs: TMQTTPublishItems;
    FNowPublish: TMQTTPublishItem;
    {*订阅发布*}
    FDetailLog: Boolean;
    {*明细日志*}
    FSyncLock: TCriticalSection;
    {*同步锁定*}
    FNowEvent: TMQTTEventItem;
    FEventData: TMQTTEventItems;
    FOnConnect: TMQTTOnConnect;
    FOnMessage: TMQTTOnMessage;
    FOnConnectEvent: TMQTTOnConnectEvent;
    FOnMessageEvent: TMQTTOnMessageEvent;
    {*事件相关*}
  protected
    procedure DoThreadWork(const nConfig: PThreadWorkerConfig;
      const nThread: TThread);
    {*线程业务*}
    procedure SyncEvent(const nEventData: PMQTTEventItem);
    procedure SyncMainThread(nSender: TObject; nMsg: Integer;
      nWParam,nLParam: NativeInt);
    {*同步事件*}
    function FindTopic(const nTopic: AnsiString): Integer;
    {*检索数据*}
    procedure SubscribeTopics();
    {*订阅主题*}
    procedure PublishData(const nThread: TThread);
    {*发布消息*}
    procedure ResetAllTopicStatus();
    {*重置状态*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    procedure ConnectBroker;
    procedure Disconnect;
    {*连接断开*}
    procedure AddTopic(const nTopic: string; const nQos: Integer = MOSQ_QOS_0);
    procedure DelTopic(const nTopic: string);
    {*增减订阅*}
    procedure Publish(const nTopic, nPayload: string;
      const nRetain: Boolean = False; const nQos: Integer = MOSQ_QOS_0);
    {*发布消息*}
    property ServerHost: string read FServerHost write FServerHost;
    property ServerPort: Integer read FServerPort write FServerPort;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property ClientID: string read FClientID write FClientID;
    property KeepAlive: Integer read FKeepAlive write FKeepAlive;
    property DetailLog: Boolean read FDetailLog write FDetailLog;
    property OnConnect: TMQTTOnConnect read FOnConnect write FOnConnect;
    property OnMessage: TMQTTOnMessage read FOnMessage write FOnMessage;
    property OnConnectEvent: TMQTTOnConnectEvent read FOnConnectEvent write FOnConnectEvent;
    property OnMessageEvent: TMQTTOnMessageEvent read FOnMessageEvent write FOnMessageEvent;
    {*属性事件*}
  end;

implementation

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TMQTTClient, 'MQTT-Client', nEvent);
end;

//Date: 2019-06-26
//Desc: 连接Broker结束
procedure on_connect(mosq: p_mosquitto; obj: Pointer; rc: Integer); cdecl;
var nStr: string;
    nClient: TMQTTClient;
    nData: PMQTTClientData;
    nEvent: TMQTTEventItem;
begin
  case rc of
    0: nStr := '成功';
    1: nStr := '连接被拒绝(不支持的协议版本)';
    2: nStr := '连接被决绝(identifier被拒绝)';
    3: nStr := '连接被拒绝(远程服务不可用)'
    else
      nStr := '未知';
  end;

  nData := obj;
  WriteLog(Format('连接远程服务[ %s,%d ]: %s', [nData.FServerHost,
    nData.FServerPort, nStr]));
  //conn log

  if rc = 0 then
  begin
    nClient := nData.FClient as TMQTTClient;
    //client obj

    if Assigned(nClient.FOnConnect) then
      nClient.FOnConnect(nData.FServerHost, nData.FServerPort, True);
    //event

    if Assigned(nClient.FOnConnectEvent) then
    begin
      nEvent.FEventType := etConn;
      nEvent.FConnected := True;
      nClient.SyncEvent(@nEvent);
    end;
  end;
end;

//Date: 2019-06-26
//Desc: 远程Broker断开
procedure on_disconnect(mosq: p_mosquitto; obj: Pointer; rc: Integer); cdecl;
var nStr: string;
    nClient: TMQTTClient;
    nData: PMQTTClientData;
    nEvent: TMQTTEventItem;
begin
  if rc = 0 then
       nStr := '客户端主动断开'
  else nStr := '网络异常断开';

  nData := obj;
  WriteLog(Format('远程服务[ %s,%d ]已断开: %s', [nData.FServerHost,
    nData.FServerPort, nStr]));
  //conn log

  if rc = 0 then
  begin
    nClient := nData.FClient as TMQTTClient;
    //client obj

    if Assigned(nClient.FOnConnect) then
      nClient.FOnConnect(nData.FServerHost, nData.FServerPort, False);
    //event

    if Assigned(nClient.FOnConnectEvent) then
    begin
      nEvent.FEventType := etConn;
      nEvent.FConnected := False;
      nClient.SyncEvent(@nEvent);
    end;
  end;
end;

//Date: 2019-06-26
//Desc: 主题订阅完毕
procedure on_subscribe(mosq: p_mosquitto; obj: Pointer; mid: Integer;
 qos_count: Integer; const granted_qos: PInteger); cdecl;
var nIdx: Integer;
    nClient: TMQTTClient;
    nData: PMQTTClientData;
begin
  nData := obj;
  nClient := nData.FClient as TMQTTClient;
  //client obj

  with nClient do
  begin
    for nIdx := Low(FTopics) to High(FTopics) do
    if FTopics[nIdx].FChannel = mid then
    begin
      FTopics[nIdx].FHasSub := True;
      //sub flag

      WriteLog(Format('远程服务[ %s,%d ]订阅: %s,成功', [nData.FServerHost,
        nData.FServerPort, nClient.FTopics[nIdx].FTopic]));
      //conn log
    end;
  end;
end;

//Date: 2019-06-26
//Desc: 取消主题订阅
procedure on_unsubscribe(mosq: p_mosquitto; obj: Pointer; mid: Integer); cdecl;
var nIdx: Integer;
    nClient: TMQTTClient;
    nData: PMQTTClientData;
begin
  nData := obj;
  nClient := nData.FClient as TMQTTClient;
  //client obj

  with nClient do
  begin
    for nIdx := Low(FTopics) to High(FTopics) do
    if FTopics[nIdx].FChannel = mid then
    begin
      FTopics[nIdx].FHasSub := False;
      //sub flag

      WriteLog(Format('远程服务[ %s,%d ]取消订阅: %s,成功', [nData.FServerHost,
        nData.FServerPort, nClient.FTopics[nIdx].FTopic]));
      //conn log
    end;
  end;
end;

//Date: 2019-06-26
//Desc: 运行明细日志
procedure on_log(mosq: p_mosquitto; obj: Pointer; level: Integer;
 const str: PAnsiChar); cdecl;
begin
  WriteLog(Format('%s,%d: %s', [PMQTTClientData(obj).FServerHost,
    PMQTTClientData(obj).FServerPort, System.AnsiStrings.StrPas(str)]));
  //conn log
end;

//Date: 2019-06-26
//Desc: 发布主题
procedure on_publish(mosq: p_mosquitto; obj: Pointer; mid: Integer); cdecl;
begin
  //nothing
end;

//Date: 2019-06-26
//Desc: 收到消息
procedure on_message(mosq: p_mosquitto; obj: Pointer;
 const msg: p_mosquitto_message); cdecl;
var nTopic,nPayload: string;
    nClient: TMQTTClient;
    nEvent: TMQTTEventItem;
begin
  nClient := PMQTTClientData(obj).FClient as TMQTTClient;
  //client obj

  if Assigned(nClient.FOnMessage) or Assigned(nClient.FOnMessageEvent) then
  begin
    nTopic := UTF8ToUnicodeString(msg.topic);
    nPayload := UTF8ToUnicodeString(PAnsiChar(msg.payload));

    if Assigned(nClient.FOnMessage) then
      nClient.FOnMessage(nTopic, nPayload);
    //xxxxx

    if Assigned(nClient.FOnMessageEvent) then
    begin
      nEvent.FEventType := etMsg;
      nEvent.FTopic := nTopic;
      nEvent.FPayload := nPayload;
      nClient.SyncEvent(@nEvent);
    end;
  end;
end;

//------------------------------------------------------------------------------
constructor TMQTTClient.Create;
var nStr: string;
    nRes: Integer;
    major,minor,revision: Integer;
    nWorker: TThreadWorkerConfig;
begin
  inherited;
  FUserName := '';
  FPassword := '';
  FServerPort := 1883;
  FServerHost := '127.0.0.1';

  FKeepAlive := 600;
  FDetailLog := False;
  SetLength(FTopics, 0);
  SetLength(FPublishs, 0);
  SetLength(FEventData, 0);
  FSyncLock := TCriticalSection.Create;

  FClient := nil;
  FClientID := GUIDToString(TGUID.NewGuid);
  FillChar(FClientData, SizeOf(FClientData), #0);

  mosquitto_lib_version(@major, @minor, @revision);
  if not ((major = LIBMOSQUITTO_MAJOR) and (minor = LIBMOSQUITTO_MINOR) and
          (revision = LIBMOSQUITTO_REVISION)) then
  begin
    nStr := Format('MOS库版本不匹配(%d.%d.%d - %d.%d.%d)', [major, minor,
      revision, LIBMOSQUITTO_MAJOR, LIBMOSQUITTO_MINOR, LIBMOSQUITTO_REVISION]);
    //xxxxx

    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;
  //xxxxx

  nRes := mosquitto_lib_init();
  if nRes <> MOSQ_ERR_SUCCESS then
  begin
    nStr := Format('lib_init error: %d,%s', [nRes, mosquitto_strerror(nRes)]);
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  gMG.FThreadPool.WorkerInit(nWorker);
  with nWorker do
  begin
    FWorkerName   := 'TMQTTClient.DoWork';
    FParentObj    := Self;
    FParentDesc   := 'MQTT-Client';
    FCallTimes    := 0; //暂停
    FCallInterval := 100;
    FProcEvent    := DoThreadWork;
  end;

  gMG.FThreadPool.WorkerAdd(@nWorker);
  //添加线程作业
end;

destructor TMQTTClient.Destroy;
begin
  Disconnect;
  //clear conn first
  mosquitto_lib_cleanup();

  FreeAndNil(FSyncLock);
  SyncPostAbort(Self);
  inherited;
end;

//Date: 2019-06-21
//Parm: 主题
//Desc: 检索nTopic,返回索引
function TMQTTClient.FindTopic(const nTopic: AnsiString): Integer;
var nIdx: Integer;
begin
  Result := -1;
  //default

  for nIdx := Low(FTopics) to High(FTopics) do
  with FTopics[nIdx] do
  begin
    if (not FEnabled) or (FTopic <> nTopic) then Continue;
    //not match

    Result := nIdx;
    Break;
  end;
end;

//Date: 2019-06-21
//Parm: 主题名称;qos
//Desc: 添加nTopic订阅
procedure TMQTTClient.AddTopic(const nTopic: string; const nQos: Integer);
var nIdx,nRes: Integer;
begin
  nRes := mosquitto_sub_topic_check(PAnsiChar(AnsiString(nTopic)));
  if nRes <> MOSQ_ERR_SUCCESS then
  begin
    WriteLog(Format('AddTopic Error: %d,%s', [nRes, mosquitto_strerror(nRes)]));
    Exit;
  end;

  FSyncLock.Enter;
  try
    nIdx := FindTopic(AnsiString(nTopic));
    if nIdx <> -1 then Exit; //exists

    nIdx := Length(FTopics);
    SetLength(FTopics, nIdx + 1);

    with FTopics[nIdx] do
    begin
      FEnabled  := True;
      FTopic    := AnsiString(nTopic);
      FChannel  := nIdx;
      FQos      := nQos;
      FHasSub   := False;
      FLastSub  := 0;
    end;

    FClientData.FSubscribeAllTopics := False;
    //subscribe flag
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-06-21
//Parm: 主题名称
//Desc: 取消nTopic订阅
procedure TMQTTClient.DelTopic(const nTopic: string);
var nIdx: Integer;
begin
  FSyncLock.Enter;
  try
    nIdx := FindTopic(AnsiString(nTopic));
    if nIdx <> -1 then
    begin
      FTopics[nIdx].FEnabled := False;
      if FTopics[nIdx].FHasSub then
        FClientData.FSubscribeAllTopics := False;
      //unsubscribe flag
    end;
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-06-21
//Parm: 主题;内容;是否保留;qos
//Desc: 向nTopic发布一个nPayload消息
procedure TMQTTClient.Publish(const nTopic, nPayload: string;
  const nRetain: Boolean; const nQos: Integer);
var nRes: Integer;
    i,nIdx,nTimeout: Integer;
begin
  nRes := mosquitto_pub_topic_check(PAnsiChar(AnsiString(nTopic)));
  if nRes <> MOSQ_ERR_SUCCESS then
  begin
    WriteLog(Format('Publish Error: %d,%s', [nRes, mosquitto_strerror(nRes)]));
    Exit;
  end;

  FSyncLock.Enter;
  try
    nIdx := -1;
    nTimeout := -1;
    //init

    for i := Low(FPublishs) to High(FPublishs) do
    begin
      if FPublishs[i].FEnabled then
      begin
        if (nTimeout = -1) and (TDateTimeHelper.GetTickCountDiff
           (FPublishs[i].FLastPub) > 10 * 1000) then
          nTimeout := i;
        Continue;
      end;

      nIdx := i; //重复使用无效节点
      Break;
    end;

    if nIdx = -1 then
      nIdx := nTimeout;
    //重复使用超时节点

    if nIdx = -1 then
    begin
      nIdx := Length(FPublishs);
      SetLength(FPublishs, nIdx + 1);
    end;

    with FPublishs[nIdx] do
    begin
      FEnabled := True;
      FTopic   := AnsiString(nTopic);
      FPayload := nPayload;
      FQos     := nQos;
      FRetain  := nRetain;
      FLastPub := GetTickCount();
    end;

    FClientData.FHasNewPublishItem := True;
    //publish flag
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-06-21
//Desc: 将所欲主题设为未订阅状态
procedure TMQTTClient.ResetAllTopicStatus;
var nIdx: Integer;
begin
  FSyncLock.Enter;
  try
    for nIdx := Low(FTopics) to High(FTopics) do
      if FTopics[nIdx].FEnabled then
        FTopics[nIdx].FHasSub := False;
    //set flag

    FClientData.FSubscribeAllTopics := False;
    //subscribe flag
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-06-21
//Desc: 订阅所有主题
procedure TMQTTClient.SubscribeTopics;
var nIdx,nInt,nRes: Integer;
begin
  FSyncLock.Enter;
  try
    nInt := 0;
    //init

    for nIdx := Low(FTopics) to High(FTopics) do
    with FTopics[nIdx] do
    begin
      if not FEnabled then
      begin
        if FHasSub then //取消订阅
        begin
          FHasSub := False;
          nRes := mosquitto_unsubscribe(FClient, @FChannel, PAnsiChar(FTopic));

          if nRes <> MOSQ_ERR_SUCCESS then
          begin
            WriteLog(Format('Unsubscribe Error: %d,%s', [nRes, mosquitto_strerror(nRes)]));
          end;
        end;

        Continue;
      end;

      if FHasSub then Continue;
      //sub done
      Inc(nInt);

      with TDateTimeHelper do
        if GetTickCountDiff(FLastSub, tdNow) < 10 * 1000 then Continue;
      FLastSub := GetTickCount();

      nRes := mosquitto_subscribe(FClient, @FChannel, PAnsiChar(FTopic), FQos);
      if nRes <> MOSQ_ERR_SUCCESS then
      begin
        WriteLog(Format('Subscribe Error: %d,%s', [nRes, mosquitto_strerror(nRes)]));
      end;
    end;

    if nInt = 0 then
      FClientData.FSubscribeAllTopics := True;
    //xxxxx
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-06-26
//Desc: 发布数据
procedure TMQTTClient.PublishData(const nThread: TThread);
var nIdx: Integer;
    nBuf: AnsiString;
begin
  while not nThread.CheckTerminated do
  begin
    FSyncLock.Enter;
    try
      if FClientData.FDisconnecting then
        Exit;
      //try to disconn
      FNowPublish.FEnabled := False;

      for nIdx := Low(FPublishs) to High(FPublishs) do
      with FPublishs[nIdx] do
      begin
        if not FEnabled then Continue;
        //invalid item

        FNowPublish := FPublishs[nIdx];
        FEnabled := False;
        Break;
      end;

      if not FNowPublish.FEnabled then
      begin
        FClientData.FHasNewPublishItem := False;
        Exit;
      end;
    finally
      FSyncLock.Leave;
    end;

    with FNowPublish do
    begin
      nBuf := UTF8EncodeToShortString(FPayload);
      mosquitto_publish(FClient, nil, PAnsiChar(FTopic), Length(nBuf),
        PAnsiChar(nBuf), FQos, FRetain);
      //send message
    end;
  end;
end;

//------------------------------------------------------------------------------
//Date: 2019-06-21
//Desc: 连接远程服务
procedure TMQTTClient.ConnectBroker;
begin
  if not Assigned(FClient) then
  begin
    FClientData.FClient := Self;
    FClientData.FServerHost := FServerHost;
    FClientData.FServerPort := FServerPort;

    FClient := mosquitto_new(PAnsiChar(AnsiString(FClientID)), True, @FClientData);
    //new client

    mosquitto_connect_callback_set(FClient, on_connect);
    mosquitto_disconnect_callback_set(FClient, on_disconnect);
    mosquitto_message_callback_set(FClient, on_message);
    mosquitto_subscribe_callback_set(FClient, on_subscribe);
    mosquitto_unsubscribe_callback_set(FClient, on_unsubscribe);
    mosquitto_publish_callback_set(FClient, on_publish);
    //call back event

    if FDetailLog then
         mosquitto_log_callback_set(FClient, on_log)
    else mosquitto_log_callback_set(FClient, nil);

    if FUserName <> '' then
      mosquitto_username_pw_set(FClient, PAnsiChar(AnsiString(FUserName)),
        PAnsiChar(AnsiString(FPassword)));
    //set credentials
  end;

  FSyncLock.Enter;
  try
    FClientData.FDisconnecting := False;
  finally
    FSyncLock.Leave;
  end;

  mosquitto_loop_start(FClient);
  gMG.FThreadPool.WorkerStart(Self);
  //start thread
end;

//Date: 2019-06-21
//Desc: 断开远程服务
procedure TMQTTClient.Disconnect;
var nClient: p_mosquitto;
begin
  while True do
  begin
    FSyncLock.Enter;
    try
      if not FClientData.FDisconnecting then
        FClientData.FDisconnecting := True;
      //set disconn flag

      if not FClientData.FThreadRunning then
        Break;
      //thread exit
    finally
      FSyncLock.Leave;
    end;

    Sleep(1);
  end;

  if Assigned(FClient) then
  begin
    nClient := FClient;
    FClient := nil;
    //try clear client

    mosquitto_loop_stop(nClient, True);
    mosquitto_disconnect(nClient);
    mosquitto_destroy(nClient);
  end;

  gMG.FThreadPool.WorkerStop(Self);
  //stop thread
end;

procedure TMQTTClient.DoThreadWork(const nConfig: PThreadWorkerConfig;
 const nThread: TThread);
var nRes: Integer;
begin
  try
    FSyncLock.Enter;
    try
      FClientData.FThreadRunning := True;
      //set running flag

      if FClientData.FDisconnecting then
      begin
        nConfig.FCallTimes := 0;
        Exit;
      end;
    finally
      FSyncLock.Leave;
    end;

    nRes := mosquitto_loop(FClient, 100, 1);
    if nRes <> MOSQ_ERR_SUCCESS then //any error
    begin
      if (nRes = MOSQ_ERR_NO_CONN) or (nRes = MOSQ_ERR_CONN_LOST) then
      begin
        with TDateTimeHelper,FClientData do
          if GetTickCountDiff(FLastConnecting, tdNow) < 10 * 1000 then Exit;
        FClientData.FLastConnecting := GetTickCount();

        WriteLog(Format('connect remote broker: %d,%s', [nRes,
          mosquitto_strerror(nRes)]));
        //xxxxx

        ResetAllTopicStatus();
        //set unsubcribe flag

        mosquitto_connect(FClient, PAnsiChar(AnsiString(FClientData.FServerHost)),
          FClientData.FServerPort, FKeepAlive);
        //conn broker
      end
      else
      begin
        WriteLog(Format('mosquitto_loop error: %d,%s', [nRes,
          mosquitto_strerror(nRes)]));
        //xxxxx
      end;

      Exit;
    end;

    if not FClientData.FSubscribeAllTopics then
    begin
      SubscribeTopics();
      Exit;
    end;

    if FClientData.FHasNewPublishItem then
      PublishData(nThread);
    //xxxxx
  finally
    FSyncLock.Enter;
    try
      FClientData.FThreadRunning := False;
    finally
      FSyncLock.Leave;
    end;
  end;
end;

//------------------------------------------------------------------------------
//Date: 2019-07-02
//Parm: 数据
//Desc: 将nEventData同步到主线程
procedure TMQTTClient.SyncEvent(const nEventData: PMQTTEventItem);
var nIdx,nInt,nOldest: Integer;
    nOld,nTD: Cardinal;
begin
  FSyncLock.Enter;
  try
    nInt := -1;
    nOld := 0;
    nOldest := -1;

    for nIdx := Low(FEventData) to High(FEventData) do
    with FEventData[nIdx] do
    begin
      if FEnabled then
      begin
        nTD := TDateTimeHelper.GetTickCountDiff(FLastUsed);
        if nTD > nOld then
        begin
          nOld := nTD;
          nOldest := nIdx;
        end;
      end else
      begin
        nInt := nIdx; //1.优先使用无效项
        Break;
      end;
    end;

    if nInt < 0 then
    begin
      nIdx := Length(FEventData);
      if nIdx < cMessageBufferMax then //2.新建项
      begin
        nInt := nIdx;
        SetLength(FEventData, nInt + 1);
      end;
    end;

    if nInt < 0 then
    begin
      nInt := nOldest; //3.覆盖旧数据
      if nInt <> -1 then
        WriteLog('主线程效率过低,事件数据被覆盖');
      //xxxxx
    end;

    if nInt < 0 then Exit; //数据项无效
    with FEventData[nInt] do
    begin
      FEnabled    := True;
      FLastUsed   := GetTickCount();
      FEventType  := nEventData.FEventType;

      FConnected  := nEventData.FConnected;
      FTopic      := nEventData.FTopic;
      FPayload    := nEventData.FPayload;
    end;
  finally
    FSyncLock.Leave;
  end;

  SyncPostMessage(SyncMainThread, Self, 0, nInt, nInt);
  //主进程同步
end;

//Date: 2019-07-02
//Desc: 在主线程中同步事件
procedure TMQTTClient.SyncMainThread(nSender: TObject; nMsg: Integer; nWParam,
  nLParam: NativeInt);
begin
  FSyncLock.Enter;
  try
    FNowEvent.FEnabled := False;
    if (nWParam = nLParam) and
       (nWParam >= Low(FEventData)) and (nWParam <= High(FEventData)) then
    begin
      FNowEvent := FEventData[nWParam];
      FEventData[nWParam].FEnabled := False;
    end;
  finally
    FSyncLock.Leave;
  end;

  if not FNowEvent.FEnabled then Exit;
  //invalid event

  if Assigned(FOnConnectEvent) and (FNowEvent.FEventType = etConn) then
    FOnConnectEvent(FClientData.FServerHost, FClientData.FServerPort,
      FNowEvent.FConnected);
  //xxxxx

  if Assigned(FOnMessageEvent) and (FNowEvent.FEventType = etMsg) then
    FOnMessageEvent(FNowEvent.FTopic, FNowEvent.FPayload);
  //xxxxx
end;

end.
