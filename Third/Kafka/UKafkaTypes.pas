{*******************************************************************************
  作者: dmzn@163.com 2022-04-01
  描述: 基于Kafka.lib定义结构和常量

  备注:
  *.常量字符串: https://github.com/edenhill/librdkafka.git/CONFIGURATION.md
*******************************************************************************}
unit UKafkaTypes;

interface

uses
  System.Classes, System.SysUtils, Kafka.Lib;

type
  TKKClientType        = rd_kafka_type_t;           //client type
  TKKClient            = Prd_kafka_t;               //client pointer
  TKKConfig            = Prd_kafka_conf_t;          //kafka config
  TKKTopicConfig       = Prd_kafka_topic_conf_t;    //topic config

  TKafkaHelper = class
  public
    type
      TDataBuffer = array[0 .. 512] of AnsiChar;
      {*缓冲区*}
      TErrorData = TDataBuffer;
      {*异常数据*}
      PDataItem = ^TDataItem;
      TDataItem = record
        FName: string; //名称
        FData: string; //数据
      end;
      {*数据项*}
      TDataItems = TArray<TDataItem>;
      {*数据组*}
      TConfigItem = TDataItem;
      {*配置项*}
      TConfigItems = TDataItems;
      {*配置列表*}
      TLogType = (ltInfo, ltError, ltDebug, ltProducer, ltConsumer);
      {*日志类型*}
    const
      cDataBuffer = SizeOf(TDataBuffer);
      {*缓冲大小*}
      cLogType: array[TLogType] of String = ('Info', 'Error', 'Debug',
        'Producer', 'Consumer');
      {*日志类名*}
  public
    class procedure Log(const nText: String; const nType: TLogType = ltInfo);
    {*记录日志*}
    class function Version(): string; static;
    {*SDK版本*}
    class procedure BuildConfig(const nStr: string;
      var nItems: TConfigItems; const nInit: Boolean = True); static;
    {*构建配置*}
    class function ConfigNew(const nItems: TConfigItems = [];
      const nCallback: Boolean = True): TKKConfig; static;
    class procedure ConfigSet(const nConfig: TKKConfig;
      const nKey,nValue: string); overload; static;
    class procedure ConfigSet(const nConfig: TKKConfig;
      const nItem: TConfigItems; const nExcept: Boolean = True); overload; static;
    class procedure ConfigDestroy(const nConfig: TKKConfig); static;
    {*全局参数*}
    class function TopicConfigNew(const nItems: TConfigItems = [
      ]): TKKTopicConfig; static;
    class procedure TopicConfigSet(const nConfig: TKKTopicConfig;
      const nKey,nValue: string); overload; static;
    class procedure TopicConfigSet(const nConfig: TKKTopicConfig;
      const nItem: TConfigItems; const nExcept: Boolean = True); overload; static;
    class procedure TopicConfigDestroy(const nConfig: TKKTopicConfig); static;
    {*主题参数*}
    class function ClientNew(const nType: TKKClientType;
      const nItems: TConfigItems = []): TKKClient; overload; static;
    class function ClientNew(const nType: TKKClientType;
      const nConfig: TKKConfig): TKKClient; overload; static;
    class function ClientType(const nClient: TKKClient): TKKClientType; static;
    class procedure ClientClose(const nClient: TKKClient); static;
    {*客户端*}
  end;

  //----------------------------------------------------------------------------
  TKKConfigType = (B, C, P);
  {*C = Consumer, P = Producer, B = both*}

  TKafaConfig = class
  const
    {*Global configuration properties*}
    BUILTIN_FEATURES                      = 'builtin.features';
    CLIENT_ID                             = 'client.id';
    METADATA_BROKER_LIST                  = 'metadata.broker.list';
    BOOTSTRAP_SERVERS                     = 'bootstrap.servers';
    MESSAGE_MAX_BYTES                     = 'message.max.bytes';
    MESSAGE_COPY_MAX_BYTES                = 'message.copy.max.bytes';
    RECEIVE_MESSAGE_MAX_BYTES             = 'receive.message.max.bytes';
    MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION = 'max.in.flight.requests.per.connection';
    MAX_IN_FLIGHT                         = 'max.in.flight';
    TOPIC_METADATA_REFRESH_INTERVAL_MS    = 'topic.metadata.refresh.interval.ms';
    METADATA_MAX_AGE_MS                   = 'metadata.max.age.ms';
    TOPIC_METADATA_REFRESH_FAST_INTERVAL_MS= 'topic.metadata.refresh.fast.interval.ms';
    TOPIC_METADATA_REFRESH_FAST_CNT       = 'topic.metadata.refresh.fast.cnt';
    TOPIC_METADATA_REFRESH_SPARSE         = 'topic.metadata.refresh.sparse';
    TOPIC_METADATA_PROPAGATION_MAX_MS     = 'topic.metadata.propagation.max.ms';
    TOPIC_BLACKLIST                       = 'topic.blacklist';
    DEBUG                                 = 'debug';
    SOCKET_TIMEOUT_MS                     = 'socket.timeout.ms';
    SOCKET_BLOCKING_MAX_MS                = 'socket.blocking.max.ms';
    SOCKET_SEND_BUFFER_BYTES              = 'socket.send.buffer.bytes';
    SOCKET_RECEIVE_BUFFER_BYTES           = 'socket.receive.buffer.bytes';
    SOCKET_KEEPALIVE_ENABLE               = 'socket.keepalive.enable';
    SOCKET_NAGLE_DISABLE                  = 'socket.nagle.disable';
    SOCKET_MAX_FAILS                      = 'socket.max.fails';
    BROKER_ADDRESS_TTL                    = 'broker.address.ttl';
    BROKER_ADDRESS_FAMILY                 = 'broker.address.family';
    CONNECTIONS_MAX_IDLE_MS               = 'connections.max.idle.ms';
    RECONNECT_BACKOFF_JITTER_MS           = 'reconnect.backoff.jitter.ms';
    RECONNECT_BACKOFF_MS                  = 'reconnect.backoff.ms';
    RECONNECT_BACKOFF_MAX_MS              = 'reconnect.backoff.max.ms';
    STATISTICS_INTERVAL_MS                = 'statistics.interval.ms';
    ENABLED_EVENTS                        = 'enabled_events';
    ERROR_CB                              = 'error_cb';
    THROTTLE_CB                           = 'throttle_cb';
    STATS_CB                              = 'stats_cb';
    LOG_CB                                = 'log_cb';
    LOG_LEVEL                             = 'log_level';
    LOG_QUEUE                             = 'log.queue';
    LOG_THREAD_NAME                       = 'log.thread.name';
    ENABLE_RANDOM_SEED                    = 'enable.random.seed';
    LOG_CONNECTION_CLOSE                  = 'log.connection.close';
    BACKGROUND_EVENT_CB                   = 'background_event_cb';
    SOCKET_CB                             = 'socket_cb';
    CONNECT_CB                            = 'connect_cb';
    CLOSESOCKET_CB                        = 'closesocket_cb';
    OPEN_CB                               = 'open_cb';
    OPAQUE                                = 'opaque';
    DEFAULT_TOPIC_CONF                    = 'default_topic_conf';
    INTERNAL_TERMINATION_SIGNAL           = 'internal.termination.signal';
    API_VERSION_REQUEST                   = 'api.version.request';
    API_VERSION_REQUEST_TIMEOUT_MS        = 'api.version.request.timeout.ms';
    API_VERSION_FALLBACK_MS               = 'api.version.fallback.ms';
    BROKER_VERSION_FALLBACK               = 'broker.version.fallback';
    SECURITY_PROTOCOL                     = 'security.protocol';
    SSL_CIPHER_SUITES                     = 'ssl.cipher.suites';
    SSL_CURVES_LIST                       = 'ssl.curves.list';
    SSL_SIGALGS_LIST                      = 'ssl.sigalgs.list';
    SSL_KEY_LOCATION                      = 'ssl.key.location';
    SSL_KEY_PASSWORD                      = 'ssl.key.password';
    SSL_KEY_PEM                           = 'ssl.key.pem';
    SSL_KEY                               = 'ssl_key';
    SSL_CERTIFICATE_LOCATION              = 'ssl.certificate.location';
    SSL_CERTIFICATE_PEM                   = 'ssl.certificate.pem';
    SSL_CERTIFICATE                       = 'ssl_certificate';
    SSL_CA_LOCATION                       = 'ssl.ca.location';
    SSL_CA_PEM                            = 'ssl.ca.pem';
    SSL_CA                                = 'ssl_ca';
    SSL_CA_CERTIFICATE_STORES             = 'ssl.ca.certificate.stores';
    SSL_CRL_LOCATION                      = 'ssl.crl.location';
    SSL_KEYSTORE_LOCATION                 = 'ssl.keystore.location';
    SSL_KEYSTORE_PASSWORD                 = 'ssl.keystore.password';
    SSL_ENGINE_LOCATION                   = 'ssl.engine.location';
    SSL_ENGINE_ID                         = 'ssl.engine.id';
    SSL_ENGINE_CALLBACK_DATA              = 'ssl_engine_callback_data';
    ENABLE_SSL_CERTIFICATE_VERIFICATION   = 'enable.ssl.certificate.verification';
    SSL_ENDPOINT_IDENTIFICATION_ALGORITHM = 'ssl.endpoint.identification.algorithm';
    SSL_CERTIFICATE_VERIFY_CB             = 'ssl.certificate.verify_cb';
    SASL_MECHANISMS                       = 'sasl.mechanisms';
    SASL_MECHANISM                        = 'sasl.mechanism';
    SASL_KERBEROS_SERVICE_NAME            = 'sasl.kerberos.service.name';
    SASL_KERBEROS_PRINCIPAL               = 'sasl.kerberos.principal';
    SASL_KERBEROS_KINIT_CMD               = 'sasl.kerberos.kinit.cmd';
    SASL_KERBEROS_KEYTAB                  = 'sasl.kerberos.keytab';
    SASL_KERBEROS_MIN_TIME_BEFORE_RELOGIN = 'sasl.kerberos.min.time.before.relogin';
    SASL_USERNAME                         = 'sasl.username';
    SASL_PASSWORD                         = 'sasl.password';
    SASL_OAUTHBEARER_CONFIG               = 'sasl.oauthbearer.config';
    ENABLE_SASL_OAUTHBEARER_UNSECURE_JWT  = 'enable.sasl.oauthbearer.unsecure.jwt';
    OAUTHBEARER_TOKEN_REFRESH_CB          = 'oauthbearer_token_refresh_cb';
    SASL_OAUTHBEARER_METHOD               = 'sasl.oauthbearer.method';
    SASL_OAUTHBEARER_CLIENT_ID            = 'sasl.oauthbearer.client.id';
    SASL_OAUTHBEARER_CLIENT_SECRET        = 'sasl.oauthbearer.client.secret';
    SASL_OAUTHBEARER_SCOPE                = 'sasl.oauthbearer.scope';
    SASL_OAUTHBEARER_EXTENSIONS           = 'sasl.oauthbearer.extensions';
    SASL_OAUTHBEARER_TOKEN_ENDPOINT_URL   = 'sasl.oauthbearer.token.endpoint.url';
    PLUGIN_LIBRARY_PATHS                  = 'plugin.library.paths';
    INTERCEPTORS                          = 'interceptors';
    GROUP_ID                              = 'group.id';
    GROUP_INSTANCE_ID                     = 'group.instance.id';
    PARTITION_ASSIGNMENT_STRATEGY         = 'partition.assignment.strategy';
    SESSION_TIMEOUT_MS                    = 'session.timeout.ms';
    HEARTBEAT_INTERVAL_MS                 = 'heartbeat.interval.ms';
    GROUP_PROTOCOL_TYPE                   = 'group.protocol.type';
    COORDINATOR_QUERY_INTERVAL_MS         = 'coordinator.query.interval.ms';
    MAX_POLL_INTERVAL_MS                  = 'max.poll.interval.ms';
    ENABLE_AUTO_COMMIT                    = 'enable.auto.commit';
    AUTO_COMMIT_INTERVAL_MS               = 'auto.commit.interval.ms';
    ENABLE_AUTO_OFFSET_STORE              = 'enable.auto.offset.store';
    QUEUED_MIN_MESSAGES                   = 'queued.min.messages';
    QUEUED_MAX_MESSAGES_KBYTES            = 'queued.max.messages.kbytes';
    FETCH_WAIT_MAX_MS                     = 'fetch.wait.max.ms';
    FETCH_MESSAGE_MAX_BYTES               = 'fetch.message.max.bytes';
    MAX_PARTITION_FETCH_BYTES             = 'max.partition.fetch.bytes';
    FETCH_MAX_BYTES                       = 'fetch.max.bytes';
    FETCH_MIN_BYTES                       = 'fetch.min.bytes';
    FETCH_ERROR_BACKOFF_MS                = 'fetch.error.backoff.ms';
    OFFSET_STORE_METHOD                   = 'offset.store.method';
    ISOLATION_LEVEL                       = 'isolation.level';
    CONSUME_CB                            = 'consume_cb';
    REBALANCE_CB                          = 'rebalance_cb';
    OFFSET_COMMIT_CB                      = 'offset_commit_cb';
    ENABLE_PARTITION_EOF                  = 'enable.partition.eof';
    CHECK_CRCS                            = 'check.crcs';
    ALLOW_AUTO_CREATE_TOPICS              = 'allow.auto.create.topics';
    CLIENT_RACK                           = 'client.rack';
    TRANSACTIONAL_ID                      = 'transactional.id';
    TRANSACTION_TIMEOUT_MS                = 'transaction.timeout.ms';
    ENABLE_IDEMPOTENCE                    = 'enable.idempotence';
    ENABLE_GAPLESS_GUARANTEE              = 'enable.gapless.guarantee';
    QUEUE_BUFFERING_MAX_MESSAGES          = 'queue.buffering.max.messages';
    QUEUE_BUFFERING_MAX_KBYTES            = 'queue.buffering.max.kbytes';
    QUEUE_BUFFERING_MAX_MS                = 'queue.buffering.max.ms';
    LINGER_MS                             = 'linger.ms';
    MESSAGE_SEND_MAX_RETRIES              = 'message.send.max.retries';
    RETRIES                               = 'retries';
    RETRY_BACKOFF_MS                      = 'retry.backoff.ms';
    QUEUE_BUFFERING_BACKPRESSURE_THRESHOLD= 'queue.buffering.backpressure.threshold';
    COMPRESSION_CODEC                     = 'compression.codec';
    COMPRESSION_TYPE                      = 'compression.type';
    BATCH_NUM_MESSAGES                    = 'batch.num.messages';
    BATCH_SIZE                            = 'batch.size';
    DELIVERY_REPORT_ONLY_ERROR            = 'delivery.report.only.error';
    DR_CB                                 = 'dr_cb';
    DR_MSG_CB                             = 'dr_msg_cb';
    STICKY_PARTITIONING_LINGER_MS         = 'sticky.partitioning.linger.ms';

    ConfigKeys: array[1..145] of string = (BUILTIN_FEATURES, CLIENT_ID,
      METADATA_BROKER_LIST, BOOTSTRAP_SERVERS,
      MESSAGE_MAX_BYTES, MESSAGE_COPY_MAX_BYTES, RECEIVE_MESSAGE_MAX_BYTES,
      MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, MAX_IN_FLIGHT,
      TOPIC_METADATA_REFRESH_INTERVAL_MS, METADATA_MAX_AGE_MS,
      TOPIC_METADATA_REFRESH_FAST_INTERVAL_MS, TOPIC_METADATA_REFRESH_FAST_CNT,
      TOPIC_METADATA_REFRESH_SPARSE, TOPIC_METADATA_PROPAGATION_MAX_MS,
      TOPIC_BLACKLIST, DEBUG, SOCKET_TIMEOUT_MS, SOCKET_BLOCKING_MAX_MS,
      SOCKET_SEND_BUFFER_BYTES, SOCKET_RECEIVE_BUFFER_BYTES,
      SOCKET_KEEPALIVE_ENABLE, SOCKET_NAGLE_DISABLE, SOCKET_MAX_FAILS,
      BROKER_ADDRESS_TTL, BROKER_ADDRESS_FAMILY, CONNECTIONS_MAX_IDLE_MS,
      RECONNECT_BACKOFF_JITTER_MS, RECONNECT_BACKOFF_MS,
      RECONNECT_BACKOFF_MAX_MS, STATISTICS_INTERVAL_MS, ENABLED_EVENTS, ERROR_CB,
      THROTTLE_CB, STATS_CB, LOG_CB, LOG_LEVEL, LOG_QUEUE, LOG_THREAD_NAME,
      ENABLE_RANDOM_SEED, LOG_CONNECTION_CLOSE, BACKGROUND_EVENT_CB, SOCKET_CB,
      CONNECT_CB, CLOSESOCKET_CB, OPEN_CB, OPAQUE, DEFAULT_TOPIC_CONF,
      INTERNAL_TERMINATION_SIGNAL, API_VERSION_REQUEST,
      API_VERSION_REQUEST_TIMEOUT_MS, API_VERSION_FALLBACK_MS,
      BROKER_VERSION_FALLBACK, SECURITY_PROTOCOL, SSL_CIPHER_SUITES,
      SSL_CURVES_LIST, SSL_SIGALGS_LIST, SSL_KEY_LOCATION, SSL_KEY_PASSWORD,
      SSL_KEY_PEM, SSL_KEY, SSL_CERTIFICATE_LOCATION, SSL_CERTIFICATE_PEM,
      SSL_CERTIFICATE, SSL_CA_LOCATION, SSL_CA_PEM, SSL_CA,
      SSL_CA_CERTIFICATE_STORES, SSL_CRL_LOCATION, SSL_KEYSTORE_LOCATION,
      SSL_KEYSTORE_PASSWORD, SSL_ENGINE_LOCATION, SSL_ENGINE_ID,
      SSL_ENGINE_CALLBACK_DATA, ENABLE_SSL_CERTIFICATE_VERIFICATION,
      SSL_ENDPOINT_IDENTIFICATION_ALGORITHM, SSL_CERTIFICATE_VERIFY_CB,
      SASL_MECHANISMS, SASL_MECHANISM, SASL_KERBEROS_SERVICE_NAME,
      SASL_KERBEROS_PRINCIPAL, SASL_KERBEROS_KINIT_CMD, SASL_KERBEROS_KEYTAB,
      SASL_KERBEROS_MIN_TIME_BEFORE_RELOGIN, SASL_USERNAME, SASL_PASSWORD,
      SASL_OAUTHBEARER_CONFIG, ENABLE_SASL_OAUTHBEARER_UNSECURE_JWT,
      OAUTHBEARER_TOKEN_REFRESH_CB, SASL_OAUTHBEARER_METHOD,
      SASL_OAUTHBEARER_CLIENT_ID, SASL_OAUTHBEARER_CLIENT_SECRET,
      SASL_OAUTHBEARER_SCOPE, SASL_OAUTHBEARER_EXTENSIONS,
      SASL_OAUTHBEARER_TOKEN_ENDPOINT_URL, PLUGIN_LIBRARY_PATHS, INTERCEPTORS,
      GROUP_ID, GROUP_INSTANCE_ID, PARTITION_ASSIGNMENT_STRATEGY,
      SESSION_TIMEOUT_MS, HEARTBEAT_INTERVAL_MS, GROUP_PROTOCOL_TYPE,
      COORDINATOR_QUERY_INTERVAL_MS, MAX_POLL_INTERVAL_MS, ENABLE_AUTO_COMMIT,
      AUTO_COMMIT_INTERVAL_MS, ENABLE_AUTO_OFFSET_STORE, QUEUED_MIN_MESSAGES,
      QUEUED_MAX_MESSAGES_KBYTES, FETCH_WAIT_MAX_MS, FETCH_MESSAGE_MAX_BYTES,
      MAX_PARTITION_FETCH_BYTES, FETCH_MAX_BYTES, FETCH_MIN_BYTES,
      FETCH_ERROR_BACKOFF_MS, OFFSET_STORE_METHOD, ISOLATION_LEVEL, CONSUME_CB,
      REBALANCE_CB, OFFSET_COMMIT_CB, ENABLE_PARTITION_EOF, CHECK_CRCS,
      ALLOW_AUTO_CREATE_TOPICS, CLIENT_RACK, TRANSACTIONAL_ID,
      TRANSACTION_TIMEOUT_MS, ENABLE_IDEMPOTENCE, ENABLE_GAPLESS_GUARANTEE,
      QUEUE_BUFFERING_MAX_MESSAGES, QUEUE_BUFFERING_MAX_KBYTES,
      QUEUE_BUFFERING_MAX_MS, LINGER_MS, MESSAGE_SEND_MAX_RETRIES, RETRIES,
      RETRY_BACKOFF_MS, QUEUE_BUFFERING_BACKPRESSURE_THRESHOLD,
      COMPRESSION_CODEC, COMPRESSION_TYPE, BATCH_NUM_MESSAGES, BATCH_SIZE,
      DELIVERY_REPORT_ONLY_ERROR, DR_CB, DR_MSG_CB,
      STICKY_PARTITIONING_LINGER_MS);

    ConfigTypes: array[1..145] of TKKConfigType = (B, B, B, B, B, B, B, B, B, B,
      B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B,
      B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B,
      B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B,
      B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, C, C, C, C,
      C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, C, B, P,
      P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P);
    {*Configuration properties*}
  end;

  TTopicConfig = class
  const
    {*Topic configuration properties*}
    REQUEST_REQUIRED_ACKS                 = 'request.required.acks';
    ACKS                                  = 'acks';
    REQUEST_TIMEOUT_MS                    = 'request.timeout.ms';
    MESSAGE_TIMEOUT_MS                    = 'message.timeout.ms';
    DELIVERY_TIMEOUT_MS                   = 'delivery.timeout.ms';
    QUEUING_STRATEGY                      = 'queuing.strategy';
    PRODUCE_OFFSET_REPORT                 = 'produce.offset.report';
    PARTITIONER                           = 'partitioner';
    PARTITIONER_CB                        = 'partitioner_cb';
    MSG_ORDER_CMP                         = 'msg_order_cmp';
    OPAQUE                                = 'opaque';
    COMPRESSION_CODEC                     = 'compression.codec';
    COMPRESSION_TYPE                      = 'compression.type';
    COMPRESSION_LEVEL                     = 'compression.level';
    AUTO_COMMIT_ENABLE                    = 'auto.commit.enable';
    ENABLE_AUTO_COMMIT                    = 'enable.auto.commit';
    AUTO_COMMIT_INTERVAL_MS               = 'auto.commit.interval.ms';
    AUTO_OFFSET_RESET                     = 'auto.offset.reset';
    OFFSET_STORE_PATH                     = 'offset.store.path';
    OFFSET_STORE_SYNC_INTERVAL_MS         = 'offset.store.sync.interval.ms';
    OFFSET_STORE_METHOD                   = 'offset.store.method';
    CONSUME_CALLBACK_MAX_MESSAGES         = 'consume.callback.max.messages';

    ConfigKeys: array[1..22] of string = (REQUEST_REQUIRED_ACKS, ACKS,
      REQUEST_TIMEOUT_MS, MESSAGE_TIMEOUT_MS,
      DELIVERY_TIMEOUT_MS, QUEUING_STRATEGY, PRODUCE_OFFSET_REPORT, PARTITIONER,
      PARTITIONER_CB, MSG_ORDER_CMP, OPAQUE, COMPRESSION_CODEC, COMPRESSION_TYPE,
      COMPRESSION_LEVEL, AUTO_COMMIT_ENABLE, ENABLE_AUTO_COMMIT,
      AUTO_COMMIT_INTERVAL_MS, AUTO_OFFSET_RESET, OFFSET_STORE_PATH,
      OFFSET_STORE_SYNC_INTERVAL_MS, OFFSET_STORE_METHOD,
      CONSUME_CALLBACK_MAX_MESSAGES);

    ConfigTypes: array[1..22] of TKKConfigType = (P, P, P, P, P, P, P, P, P, P,
      B, P, P, P, C, C, C, C, C, C, C, C);
    {*Configuration properties*}
  end;

implementation

uses
  UManagerGroup, ULibFun;

//Date: 2022-04-01
//Parm: 内容;类型
//Desc: 记录日志
class procedure TKafkaHelper.Log(const nText: String; const nType: TLogType);
begin
  gMG.FLogManager.AddLog(TKafkaHelper, 'KAFKA',
    Format('[%s]', [cLogType[nType]]) + nText);
  //xxxxx
end;

//Date: 2022-04-07
//Desc: 获取sdk版本
class function TKafkaHelper.Version: string;
begin
  Result := string(rd_kafka_version_str());
end;

//Date: 2022-04-01
//Parm: 字符串,格式:key=value #13 key=value
//Desc: 使用nStr填充nItems配置列表
class procedure TKafkaHelper.BuildConfig(const nStr: string;
  var nItems: TConfigItems; const nInit: Boolean);
var nK,nV: string;
    nList: TStrings;
    nIdx,nLen,i: Integer;
begin
  nList := nil;
  try
    if nInit then SetLength(nItems, 0);
    nList := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nList.Text := nStr;

    for nIdx := 0 to nList.Count -1 do
    begin
      nK := Trim(nList.KeyNames[nIdx]);
      nV := Trim(nList.ValueFromIndex[nIdx]);
      if (nK = '')  or (nV = '') then Continue;

      nLen := -1;
      for i := Low(nItems) to High(nItems) do
      if CompareText(nItems[i].FName, nK) = 0 then
      begin
        nLen := i;
        Break;
      end;

      if nLen < 0 then
      begin
        nLen := Length(nItems);
        SetLength(nItems, nLen + 1);
        nItems[nLen].FName := nK;
      end;

      nItems[nLen].FData := nV;
      //set value
    end;
  finally
    gMG.FObjectPool.Release(nList);
  end;
end;

//------------------------------------------------------------------------------
procedure CallbackProducer(rk: prd_kafka_t; rkmessage: prd_kafka_message_t;
  opaque: Pointer); cdecl;
var nStr: string;
begin
  if rkmessage <> nil then
  begin
    nStr := format('Message send result = %d', [Integer(rkmessage.err)]);
    TKafkaHelper.Log(nStr, ltProducer);
  end;
end;
procedure CallbackLog(rk: prd_kafka_t; level: integer; fac: PAnsiChar;
  buf: PAnsiChar); cdecl;
var nStr: string;
begin
  nStr := format('Log_CallBack - fac = %s, buff = %s',
          [string(fac), string(buf)]);
  TKafkaHelper.Log(nStr, ltInfo);
end;
procedure CallBackError(rk: prd_kafka_t; err: integer; reason: PAnsiChar;
  opaque: Pointer); cdecl;
begin
  with TKafkaHelper do
    Log(format('Error =  %s', [String(reason)]), ltError);
  //xxxxx
end;

//Date: 2022-04-01
//Parm: 配置列表;默认回调
//Desc: 创建Kafka配置实例
class function TKafkaHelper.ConfigNew(const nItems: TConfigItems;
  const nCallback: Boolean): TKKConfig;
begin
  Result := rd_kafka_conf_new();
  if Length(nItems) > 0 then
    ConfigSet(Result, nItems);
  //xxxxx

  if nCallback then
  begin
    rd_kafka_conf_set_dr_msg_cb(Result, @CallbackProducer);
    rd_kafka_conf_set_log_cb(Result, @CallbackLog);
    rd_kafka_conf_set_error_cb(Result, @CallBackError);
  end;
end;

//Date: 2022-04-01
//Parm: 配置实例;参数名;参数值
//Desc: 为nConfig增加配置
class procedure TKafkaHelper.ConfigSet(const nConfig: TKKConfig; const nKey,
  nValue: string);
var nStr: string;
    nBuf: TErrorData;
    nRes: rd_kafka_conf_res_t;
begin
  nRes := rd_kafka_conf_set(nConfig, PAnsiChar(AnsiString(nKey)),
    PAnsiChar(AnsiString(nValue)), nBuf, cDataBuffer);
  if nRes = RD_KAFKA_CONF_OK then Exit;

  case nRes of
   RD_KAFKA_CONF_UNKNOWN:
    nStr := Format('unknown config name: %s', [String(nBuf)]);
   RD_KAFKA_CONF_INVALID:
    nStr := Format('invalid config value: %s', [String(nBuf)])
   else
    nStr := Format('unknown config result: %s', [String(nBuf)]);
  end;

  Log(nStr, ltError);
  raise Exception.Create(nStr);
end;

//Date: 2022-04-01
//Parm: 配置实例;配置列表;异常后中止
//Desc: 为nConfig增加配置
class procedure TKafkaHelper.ConfigSet(const nConfig: TKKConfig;
  const nItem: TConfigItems; const nExcept: Boolean);
var nIdx: Integer;
begin
  for nIdx := Low(nItem) to High(nItem) do
  begin
    try
      ConfigSet(nConfig, nItem[nIdx].FName, nItem[nIdx].FData);
    except
      if nExcept then raise;
    end;
  end;
end;

//Date: 2022-04-01
//Parm: kafka配置
//Desc: 销毁nConfig实例
class procedure TKafkaHelper.ConfigDestroy(const nConfig: TKKConfig);
begin
  rd_kafka_conf_destroy(nConfig);
end;

//------------------------------------------------------------------------------
//Date: 2022-04-02
//Parm: 配置列表
//Desc: 创建kafka topic的配置
class function TKafkaHelper.TopicConfigNew(const nItems: TConfigItems
  ): TKKTopicConfig;
begin
  Result := rd_kafka_topic_conf_new();
  if Length(nItems) > 0 then
    TopicConfigSet(Result, nItems);
  //xxxxx
end;

//Date: 2022-04-06
//Parm: topic实例;参数名;参数值
//Desc: 为nConfig增加配置
class procedure TKafkaHelper.TopicConfigSet(const nConfig: TKKTopicConfig;
  const nKey, nValue: string);
var nStr: string;
    nBuf: TErrorData;
    nRes: rd_kafka_conf_res_t;
begin
  nRes := rd_kafka_topic_conf_set(nConfig, PAnsiChar(AnsiString(nKey)),
    PAnsiChar(AnsiString(nValue)), nBuf, cDataBuffer);
  if nRes = RD_KAFKA_CONF_OK then Exit;

  case nRes of
   RD_KAFKA_CONF_UNKNOWN:
    nStr := Format('unknown topic-config name: %s', [String(nBuf)]);
   RD_KAFKA_CONF_INVALID:
    nStr := Format('invalid topic-config value: %s', [String(nBuf)])
   else
    nStr := Format('unknown topic-config result: %s', [String(nBuf)]);
  end;

  Log(nStr, ltError);
  raise Exception.Create(nStr);
end;

//Date: 22022-04-06
//Parm: 配置实例;配置列表;异常后中止
//Desc: 为nConfig增加配置
class procedure TKafkaHelper.TopicConfigSet(const nConfig: TKKTopicConfig;
  const nItem: TConfigItems; const nExcept: Boolean = True);
var nIdx: Integer;
begin
  for nIdx := Low(nItem) to High(nItem) do
  begin
    try
      TopicConfigSet(nConfig, nItem[nIdx].FName, nItem[nIdx].FData);
    except
      if nExcept then raise;
    end;
  end;
end;

//Date: 2022-04-06
//Parm: topic配置
//Desc: 销毁nConfig实例
class procedure TKafkaHelper.TopicConfigDestroy(const nConfig: TKKTopicConfig);
begin
  rd_kafka_topic_conf_destroy(nConfig);
end;

//------------------------------------------------------------------------------
//Date: 2022-04-06
//Parm: client类型;配置列表
//Desc: 创建nType类型的客户端实例
class function TKafkaHelper.ClientNew(const nType: TKKClientType;
  const nItems: TConfigItems): TKKClient;
var nConf: TKafaConfig;
begin
  nConf := nil;
  try
    if Length(nItems) > 0 then
      nConf := ConfigNew(nItems);
    Result := ClientNew(nType, nConf);
  except
    if Assigned(nConf) then
      ConfigDestroy(nConf);
    raise;
  end;
end;

//Date: 2022-04-06
//Parm: client类型;配置项
//Desc: 创建nType类型的客户端实例
class function TKafkaHelper.ClientNew(const nType: TKKClientType;
  const nConfig: TKKConfig): TKKClient;
var nStr: string;
    nBuf: TErrorData;
begin
  Result := rd_kafka_new(nType, nConfig, nBuf, cDataBuffer);
  if not Assigned(Result) then
  begin
    nStr := Format('Unable to create Kafka Handle - %s', [string(nBuf)]);
    Log(nStr);
    raise Exception.Create(nStr);
  end;
end;

//Date: 2022-04-06
//Parm: 客户端实例
//Desc: 返回nClient的类型
class function TKafkaHelper.ClientType(const nClient: TKKClient): TKKClientType;
begin
  Result := rd_kafka_type(nClient);
end;

//Date: 2022-04-06
//Parm: 客户端实例
//Desc: 关闭nClient实例
class procedure TKafkaHelper.ClientClose(const nClient: TKKClient);
begin
  rd_kafka_destroy(nClient);
end;

end.
