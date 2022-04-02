{*******************************************************************************
  作者: dmzn@163.com 2022-04-01
  描述: 基于Kafka.lib定义结构和常量

  备注:
  *.常量字符串: https://github.com/edenhill/librdkafka.git/CONFIGURATION.md
  *.常量前缀描述: C = Consumer, P = Producer, B = both
*******************************************************************************}
unit UKafkaTypes;

interface

uses
  System.Classes, System.SysUtils, UManagerGroup, ULibFun, Kafka.Lib;

type
  TKafkaConfig = Prd_kafka_conf_t;
  TKafkaTopicConfig = Prd_kafka_topic_conf_t;

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
    class procedure BuildConfig(const nStr: string;
      var nItems: TConfigItems; const nInit: Boolean = True); static;
    {*构建配置*}
    class function ConfigNew(const nItems: TConfigItems = [];
      const nCallback: Boolean = True): TKafkaConfig; static;
    class procedure ConfigSet(const nConfig: TKafkaConfig;
      const nKey,nValue: string); overload; static;
    class procedure ConfigSet(const nConfig: TKafkaConfig;
      const nItem: TConfigItems; const nExcept: Boolean = True); overload; static;
    class procedure ConfigDestroy(const nConfig: TKafkaConfig); static;
    {*全局参数*}
    class function TopicConfigNew(const nItems: TConfigItems = [
      ]): TKafkaTopicConfig; static;
    class procedure TopicConfigSet(const nConfig: TKafkaTopicConfig;
      const nKey,nValue: string); overload; static;
    class procedure TopicConfigSet(const nConfig: TKafkaTopicConfig;
      const nItem: TConfigItems; const nExcept: Boolean = True); overload; static;
    class procedure TopicConfigDestroy(const nConfig: TKafkaTopicConfig); static;
    {*主题参数*}
  end;

  //----------------------------------------------------------------------------
  TKafaConfig = class
  const
    {*Global configuration properties*}
    B_BUILTIN_FEATURES                    = 'builtin.features';
    B_CLIENT_ID                           = 'client.id';
    B_METADATA_BROKER_LIST                = 'metadata.broker.list';
    B_BOOTSTRAP_SERVERS                   = 'bootstrap.servers';
    B_MESSAGE_MAX_BYTES                   = 'message.max.bytes';
    B_MESSAGE_COPY_MAX_BYTES              = 'message.copy.max.bytes';
    B_RECEIVE_MESSAGE_MAX_BYTES           = 'receive.message.max.bytes';
    B_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION= 'max.in.flight.requests.per.connection';
    B_MAX_IN_FLIGHT                       = 'max.in.flight';
    B_TOPIC_METADATA_REFRESH_INTERVAL_MS  = 'topic.metadata.refresh.interval.ms';
    B_METADATA_MAX_AGE_MS                 = 'metadata.max.age.ms';
    B_TOPIC_METADATA_REFRESH_FAST_INTERVAL_MS= 'topic.metadata.refresh.fast.interval.ms';
    B_TOPIC_METADATA_REFRESH_FAST_CNT     = 'topic.metadata.refresh.fast.cnt';
    B_TOPIC_METADATA_REFRESH_SPARSE       = 'topic.metadata.refresh.sparse';
    B_TOPIC_METADATA_PROPAGATION_MAX_MS   = 'topic.metadata.propagation.max.ms';
    B_TOPIC_BLACKLIST                     = 'topic.blacklist';
    B_DEBUG                               = 'debug';
    B_SOCKET_TIMEOUT_MS                   = 'socket.timeout.ms';
    B_SOCKET_BLOCKING_MAX_MS              = 'socket.blocking.max.ms';
    B_SOCKET_SEND_BUFFER_BYTES            = 'socket.send.buffer.bytes';
    B_SOCKET_RECEIVE_BUFFER_BYTES         = 'socket.receive.buffer.bytes';
    B_SOCKET_KEEPALIVE_ENABLE             = 'socket.keepalive.enable';
    B_SOCKET_NAGLE_DISABLE                = 'socket.nagle.disable';
    B_SOCKET_MAX_FAILS                    = 'socket.max.fails';
    B_BROKER_ADDRESS_TTL                  = 'broker.address.ttl';
    B_BROKER_ADDRESS_FAMILY               = 'broker.address.family';
    B_CONNECTIONS_MAX_IDLE_MS             = 'connections.max.idle.ms';
    B_RECONNECT_BACKOFF_JITTER_MS         = 'reconnect.backoff.jitter.ms';
    B_RECONNECT_BACKOFF_MS                = 'reconnect.backoff.ms';
    B_RECONNECT_BACKOFF_MAX_MS            = 'reconnect.backoff.max.ms';
    B_STATISTICS_INTERVAL_MS              = 'statistics.interval.ms';
    B_ENABLED_EVENTS                      = 'enabled_events';
    B_ERROR_CB                            = 'error_cb';
    B_THROTTLE_CB                         = 'throttle_cb';
    B_STATS_CB                            = 'stats_cb';
    B_LOG_CB                              = 'log_cb';
    B_LOG_LEVEL                           = 'log_level';
    B_LOG_QUEUE                           = 'log.queue';
    B_LOG_THREAD_NAME                     = 'log.thread.name';
    B_ENABLE_RANDOM_SEED                  = 'enable.random.seed';
    B_LOG_CONNECTION_CLOSE                = 'log.connection.close';
    B_BACKGROUND_EVENT_CB                 = 'background_event_cb';
    B_SOCKET_CB                           = 'socket_cb';
    B_CONNECT_CB                          = 'connect_cb';
    B_CLOSESOCKET_CB                      = 'closesocket_cb';
    B_OPEN_CB                             = 'open_cb';
    B_OPAQUE                              = 'opaque';
    B_DEFAULT_TOPIC_CONF                  = 'default_topic_conf';
    B_INTERNAL_TERMINATION_SIGNAL         = 'internal.termination.signal';
    B_API_VERSION_REQUEST                 = 'api.version.request';
    B_API_VERSION_REQUEST_TIMEOUT_MS      = 'api.version.request.timeout.ms';
    B_API_VERSION_FALLBACK_MS             = 'api.version.fallback.ms';
    B_BROKER_VERSION_FALLBACK             = 'broker.version.fallback';
    B_SECURITY_PROTOCOL                   = 'security.protocol';
    B_SSL_CIPHER_SUITES                   = 'ssl.cipher.suites';
    B_SSL_CURVES_LIST                     = 'ssl.curves.list';
    B_SSL_SIGALGS_LIST                    = 'ssl.sigalgs.list';
    B_SSL_KEY_LOCATION                    = 'ssl.key.location';
    B_SSL_KEY_PASSWORD                    = 'ssl.key.password';
    B_SSL_KEY_PEM                         = 'ssl.key.pem';
    B_SSL_KEY                             = 'ssl_key';
    B_SSL_CERTIFICATE_LOCATION            = 'ssl.certificate.location';
    B_SSL_CERTIFICATE_PEM                 = 'ssl.certificate.pem';
    B_SSL_CERTIFICATE                     = 'ssl_certificate';
    B_SSL_CA_LOCATION                     = 'ssl.ca.location';
    B_SSL_CA_PEM                          = 'ssl.ca.pem';
    B_SSL_CA                              = 'ssl_ca';
    B_SSL_CA_CERTIFICATE_STORES           = 'ssl.ca.certificate.stores';
    B_SSL_CRL_LOCATION                    = 'ssl.crl.location';
    B_SSL_KEYSTORE_LOCATION               = 'ssl.keystore.location';
    B_SSL_KEYSTORE_PASSWORD               = 'ssl.keystore.password';
    B_SSL_ENGINE_LOCATION                 = 'ssl.engine.location';
    B_SSL_ENGINE_ID                       = 'ssl.engine.id';
    B_SSL_ENGINE_CALLBACK_DATA            = 'ssl_engine_callback_data';
    B_ENABLE_SSL_CERTIFICATE_VERIFICATION = 'enable.ssl.certificate.verification';
    B_SSL_ENDPOINT_IDENTIFICATION_ALGORITHM= 'ssl.endpoint.identification.algorithm';
    B_SSL_CERTIFICATE_VERIFY_CB           = 'ssl.certificate.verify_cb';
    B_SASL_MECHANISMS                     = 'sasl.mechanisms';
    B_SASL_MECHANISM                      = 'sasl.mechanism';
    B_SASL_KERBEROS_SERVICE_NAME          = 'sasl.kerberos.service.name';
    B_SASL_KERBEROS_PRINCIPAL             = 'sasl.kerberos.principal';
    B_SASL_KERBEROS_KINIT_CMD             = 'sasl.kerberos.kinit.cmd';
    B_SASL_KERBEROS_KEYTAB                = 'sasl.kerberos.keytab';
    B_SASL_KERBEROS_MIN_TIME_BEFORE_RELOGIN= 'sasl.kerberos.min.time.before.relogin';
    B_SASL_USERNAME                       = 'sasl.username';
    B_SASL_PASSWORD                       = 'sasl.password';
    B_SASL_OAUTHBEARER_CONFIG             = 'sasl.oauthbearer.config';
    B_ENABLE_SASL_OAUTHBEARER_UNSECURE_JWT= 'enable.sasl.oauthbearer.unsecure.jwt';
    B_OAUTHBEARER_TOKEN_REFRESH_CB        = 'oauthbearer_token_refresh_cb';
    B_SASL_OAUTHBEARER_METHOD             = 'sasl.oauthbearer.method';
    B_SASL_OAUTHBEARER_CLIENT_ID          = 'sasl.oauthbearer.client.id';
    B_SASL_OAUTHBEARER_CLIENT_SECRET      = 'sasl.oauthbearer.client.secret';
    B_SASL_OAUTHBEARER_SCOPE              = 'sasl.oauthbearer.scope';
    B_SASL_OAUTHBEARER_EXTENSIONS         = 'sasl.oauthbearer.extensions';
    B_SASL_OAUTHBEARER_TOKEN_ENDPOINT_URL = 'sasl.oauthbearer.token.endpoint.url';
    B_PLUGIN_LIBRARY_PATHS                = 'plugin.library.paths';
    B_INTERCEPTORS                        = 'interceptors';
    C_GROUP_ID                            = 'group.id';
    C_GROUP_INSTANCE_ID                   = 'group.instance.id';
    C_PARTITION_ASSIGNMENT_STRATEGY       = 'partition.assignment.strategy';
    C_SESSION_TIMEOUT_MS                  = 'session.timeout.ms';
    C_HEARTBEAT_INTERVAL_MS               = 'heartbeat.interval.ms';
    C_GROUP_PROTOCOL_TYPE                 = 'group.protocol.type';
    C_COORDINATOR_QUERY_INTERVAL_MS       = 'coordinator.query.interval.ms';
    C_MAX_POLL_INTERVAL_MS                = 'max.poll.interval.ms';
    C_ENABLE_AUTO_COMMIT                  = 'enable.auto.commit';
    C_AUTO_COMMIT_INTERVAL_MS             = 'auto.commit.interval.ms';
    C_ENABLE_AUTO_OFFSET_STORE            = 'enable.auto.offset.store';
    C_QUEUED_MIN_MESSAGES                 = 'queued.min.messages';
    C_QUEUED_MAX_MESSAGES_KBYTES          = 'queued.max.messages.kbytes';
    C_FETCH_WAIT_MAX_MS                   = 'fetch.wait.max.ms';
    C_FETCH_MESSAGE_MAX_BYTES             = 'fetch.message.max.bytes';
    C_MAX_PARTITION_FETCH_BYTES           = 'max.partition.fetch.bytes';
    C_FETCH_MAX_BYTES                     = 'fetch.max.bytes';
    C_FETCH_MIN_BYTES                     = 'fetch.min.bytes';
    C_FETCH_ERROR_BACKOFF_MS              = 'fetch.error.backoff.ms';
    C_OFFSET_STORE_METHOD                 = 'offset.store.method';
    C_ISOLATION_LEVEL                     = 'isolation.level';
    C_CONSUME_CB                          = 'consume_cb';
    C_REBALANCE_CB                        = 'rebalance_cb';
    C_OFFSET_COMMIT_CB                    = 'offset_commit_cb';
    C_ENABLE_PARTITION_EOF                = 'enable.partition.eof';
    C_CHECK_CRCS                          = 'check.crcs';
    C_ALLOW_AUTO_CREATE_TOPICS            = 'allow.auto.create.topics';
    B_CLIENT_RACK                         = 'client.rack';
    P_TRANSACTIONAL_ID                    = 'transactional.id';
    P_TRANSACTION_TIMEOUT_MS              = 'transaction.timeout.ms';
    P_ENABLE_IDEMPOTENCE                  = 'enable.idempotence';
    P_ENABLE_GAPLESS_GUARANTEE            = 'enable.gapless.guarantee';
    P_QUEUE_BUFFERING_MAX_MESSAGES        = 'queue.buffering.max.messages';
    P_QUEUE_BUFFERING_MAX_KBYTES          = 'queue.buffering.max.kbytes';
    P_QUEUE_BUFFERING_MAX_MS              = 'queue.buffering.max.ms';
    P_LINGER_MS                           = 'linger.ms';
    P_MESSAGE_SEND_MAX_RETRIES            = 'message.send.max.retries';
    P_RETRIES                             = 'retries';
    P_RETRY_BACKOFF_MS                    = 'retry.backoff.ms';
    P_QUEUE_BUFFERING_BACKPRESSURE_THRESHOLD= 'queue.buffering.backpressure.threshold';
    P_COMPRESSION_CODEC                   = 'compression.codec';
    P_COMPRESSION_TYPE                    = 'compression.type';
    P_BATCH_NUM_MESSAGES                  = 'batch.num.messages';
    P_BATCH_SIZE                          = 'batch.size';
    P_DELIVERY_REPORT_ONLY_ERROR          = 'delivery.report.only.error';
    P_DR_CB                               = 'dr_cb';
    P_DR_MSG_CB                           = 'dr_msg_cb';
    P_STICKY_PARTITIONING_LINGER_MS       = 'sticky.partitioning.linger.ms';

    ConfigKeys: array[1..145] of string = (B_BUILTIN_FEATURES, B_CLIENT_ID,
      B_METADATA_BROKER_LIST, B_BOOTSTRAP_SERVERS,
      B_MESSAGE_MAX_BYTES, B_MESSAGE_COPY_MAX_BYTES, B_RECEIVE_MESSAGE_MAX_BYTES,
      B_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, B_MAX_IN_FLIGHT,
      B_TOPIC_METADATA_REFRESH_INTERVAL_MS, B_METADATA_MAX_AGE_MS,
      B_TOPIC_METADATA_REFRESH_FAST_INTERVAL_MS,
      B_TOPIC_METADATA_REFRESH_FAST_CNT, B_TOPIC_METADATA_REFRESH_SPARSE,
      B_TOPIC_METADATA_PROPAGATION_MAX_MS, B_TOPIC_BLACKLIST, B_DEBUG,
      B_SOCKET_TIMEOUT_MS, B_SOCKET_BLOCKING_MAX_MS, B_SOCKET_SEND_BUFFER_BYTES,
      B_SOCKET_RECEIVE_BUFFER_BYTES, B_SOCKET_KEEPALIVE_ENABLE,
      B_SOCKET_NAGLE_DISABLE, B_SOCKET_MAX_FAILS, B_BROKER_ADDRESS_TTL,
      B_BROKER_ADDRESS_FAMILY, B_CONNECTIONS_MAX_IDLE_MS,
      B_RECONNECT_BACKOFF_JITTER_MS, B_RECONNECT_BACKOFF_MS,
      B_RECONNECT_BACKOFF_MAX_MS, B_STATISTICS_INTERVAL_MS, B_ENABLED_EVENTS,
      B_ERROR_CB, B_THROTTLE_CB, B_STATS_CB, B_LOG_CB, B_LOG_LEVEL, B_LOG_QUEUE,
      B_LOG_THREAD_NAME, B_ENABLE_RANDOM_SEED, B_LOG_CONNECTION_CLOSE,
      B_BACKGROUND_EVENT_CB, B_SOCKET_CB, B_CONNECT_CB, B_CLOSESOCKET_CB,
      B_OPEN_CB, B_OPAQUE, B_DEFAULT_TOPIC_CONF, B_INTERNAL_TERMINATION_SIGNAL,
      B_API_VERSION_REQUEST, B_API_VERSION_REQUEST_TIMEOUT_MS,
      B_API_VERSION_FALLBACK_MS, B_BROKER_VERSION_FALLBACK, B_SECURITY_PROTOCOL,
      B_SSL_CIPHER_SUITES, B_SSL_CURVES_LIST, B_SSL_SIGALGS_LIST,
      B_SSL_KEY_LOCATION, B_SSL_KEY_PASSWORD, B_SSL_KEY_PEM, B_SSL_KEY,
      B_SSL_CERTIFICATE_LOCATION, B_SSL_CERTIFICATE_PEM, B_SSL_CERTIFICATE,
      B_SSL_CA_LOCATION, B_SSL_CA_PEM, B_SSL_CA, B_SSL_CA_CERTIFICATE_STORES,
      B_SSL_CRL_LOCATION, B_SSL_KEYSTORE_LOCATION, B_SSL_KEYSTORE_PASSWORD,
      B_SSL_ENGINE_LOCATION, B_SSL_ENGINE_ID, B_SSL_ENGINE_CALLBACK_DATA,
      B_ENABLE_SSL_CERTIFICATE_VERIFICATION,
      B_SSL_ENDPOINT_IDENTIFICATION_ALGORITHM, B_SSL_CERTIFICATE_VERIFY_CB,
      B_SASL_MECHANISMS, B_SASL_MECHANISM, B_SASL_KERBEROS_SERVICE_NAME,
      B_SASL_KERBEROS_PRINCIPAL, B_SASL_KERBEROS_KINIT_CMD,
      B_SASL_KERBEROS_KEYTAB, B_SASL_KERBEROS_MIN_TIME_BEFORE_RELOGIN,
      B_SASL_USERNAME, B_SASL_PASSWORD, B_SASL_OAUTHBEARER_CONFIG,
      B_ENABLE_SASL_OAUTHBEARER_UNSECURE_JWT, B_OAUTHBEARER_TOKEN_REFRESH_CB,
      B_SASL_OAUTHBEARER_METHOD, B_SASL_OAUTHBEARER_CLIENT_ID,
      B_SASL_OAUTHBEARER_CLIENT_SECRET, B_SASL_OAUTHBEARER_SCOPE,
      B_SASL_OAUTHBEARER_EXTENSIONS, B_SASL_OAUTHBEARER_TOKEN_ENDPOINT_URL,
      B_PLUGIN_LIBRARY_PATHS, B_INTERCEPTORS, C_GROUP_ID, C_GROUP_INSTANCE_ID,
      C_PARTITION_ASSIGNMENT_STRATEGY, C_SESSION_TIMEOUT_MS,
      C_HEARTBEAT_INTERVAL_MS, C_GROUP_PROTOCOL_TYPE,
      C_COORDINATOR_QUERY_INTERVAL_MS, C_MAX_POLL_INTERVAL_MS,
      C_ENABLE_AUTO_COMMIT, C_AUTO_COMMIT_INTERVAL_MS,
      C_ENABLE_AUTO_OFFSET_STORE, C_QUEUED_MIN_MESSAGES,
      C_QUEUED_MAX_MESSAGES_KBYTES, C_FETCH_WAIT_MAX_MS,
      C_FETCH_MESSAGE_MAX_BYTES, C_MAX_PARTITION_FETCH_BYTES, C_FETCH_MAX_BYTES,
      C_FETCH_MIN_BYTES, C_FETCH_ERROR_BACKOFF_MS, C_OFFSET_STORE_METHOD,
      C_ISOLATION_LEVEL, C_CONSUME_CB, C_REBALANCE_CB, C_OFFSET_COMMIT_CB,
      C_ENABLE_PARTITION_EOF, C_CHECK_CRCS, C_ALLOW_AUTO_CREATE_TOPICS,
      B_CLIENT_RACK, P_TRANSACTIONAL_ID, P_TRANSACTION_TIMEOUT_MS,
      P_ENABLE_IDEMPOTENCE, P_ENABLE_GAPLESS_GUARANTEE,
      P_QUEUE_BUFFERING_MAX_MESSAGES, P_QUEUE_BUFFERING_MAX_KBYTES,
      P_QUEUE_BUFFERING_MAX_MS, P_LINGER_MS, P_MESSAGE_SEND_MAX_RETRIES,
      P_RETRIES, P_RETRY_BACKOFF_MS, P_QUEUE_BUFFERING_BACKPRESSURE_THRESHOLD,
      P_COMPRESSION_CODEC, P_COMPRESSION_TYPE, P_BATCH_NUM_MESSAGES,
      P_BATCH_SIZE, P_DELIVERY_REPORT_ONLY_ERROR, P_DR_CB, P_DR_MSG_CB,
      P_STICKY_PARTITIONING_LINGER_MS);
    {*Configuration properties*}
  end;

  TTopicConfig = class
  const
    {*Topic configuration properties*}
    P_REQUEST_REQUIRED_ACKS               = 'request.required.acks';
    P_ACKS                                = 'acks';
    P_REQUEST_TIMEOUT_MS                  = 'request.timeout.ms';
    P_MESSAGE_TIMEOUT_MS                  = 'message.timeout.ms';
    P_DELIVERY_TIMEOUT_MS                 = 'delivery.timeout.ms';
    P_QUEUING_STRATEGY                    = 'queuing.strategy';
    P_PRODUCE_OFFSET_REPORT               = 'produce.offset.report';
    P_PARTITIONER                         = 'partitioner';
    P_PARTITIONER_CB                      = 'partitioner_cb';
    P_MSG_ORDER_CMP                       = 'msg_order_cmp';
    B_OPAQUE                              = 'opaque';
    P_COMPRESSION_CODEC                   = 'compression.codec';
    P_COMPRESSION_TYPE                    = 'compression.type';
    P_COMPRESSION_LEVEL                   = 'compression.level';
    C_AUTO_COMMIT_ENABLE                  = 'auto.commit.enable';
    C_ENABLE_AUTO_COMMIT                  = 'enable.auto.commit';
    C_AUTO_COMMIT_INTERVAL_MS             = 'auto.commit.interval.ms';
    C_AUTO_OFFSET_RESET                   = 'auto.offset.reset';
    C_OFFSET_STORE_PATH                   = 'offset.store.path';
    C_OFFSET_STORE_SYNC_INTERVAL_MS       = 'offset.store.sync.interval.ms';
    C_OFFSET_STORE_METHOD                 = 'offset.store.method';
    C_CONSUME_CALLBACK_MAX_MESSAGES       = 'consume.callback.max.messages';

    ConfigKeys: array[1..22] of string = (P_REQUEST_REQUIRED_ACKS, P_ACKS,
      P_REQUEST_TIMEOUT_MS, P_MESSAGE_TIMEOUT_MS,
      P_DELIVERY_TIMEOUT_MS, P_QUEUING_STRATEGY, P_PRODUCE_OFFSET_REPORT,
      P_PARTITIONER, P_PARTITIONER_CB, P_MSG_ORDER_CMP, B_OPAQUE,
      P_COMPRESSION_CODEC, P_COMPRESSION_TYPE, P_COMPRESSION_LEVEL,
      C_AUTO_COMMIT_ENABLE, C_ENABLE_AUTO_COMMIT, C_AUTO_COMMIT_INTERVAL_MS,
      C_AUTO_OFFSET_RESET, C_OFFSET_STORE_PATH, C_OFFSET_STORE_SYNC_INTERVAL_MS,
      C_OFFSET_STORE_METHOD, C_CONSUME_CALLBACK_MAX_MESSAGES);
    {*Configuration properties*}
  end;

implementation

//Date: 2022-04-01
//Parm: 内容;类型
//Desc: 记录日志
class procedure TKafkaHelper.Log(const nText: String; const nType: TLogType);
begin
  gMG.FLogManager.AddLog(TKafkaHelper, 'KAFKA',
    Format('[%s]', [cLogType[nType]]) + nText);
  //xxxxx
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
  const nCallback: Boolean): TKafkaConfig;
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
class procedure TKafkaHelper.ConfigSet(const nConfig: TKafkaConfig; const nKey,
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
class procedure TKafkaHelper.ConfigSet(const nConfig: TKafkaConfig;
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
class procedure TKafkaHelper.ConfigDestroy(const nConfig: TKafkaConfig);
begin
  rd_kafka_conf_destroy(nConfig);
end;

//------------------------------------------------------------------------------
//Date: 2022-04-02
//Parm: 配置列表
//Desc:
class function TKafkaHelper.TopicConfigNew(const nItems: TConfigItems
  ): TKafkaTopicConfig;
begin

end;

class procedure TKafkaHelper.TopicConfigSet(const nConfig: TKafkaTopicConfig;
  const nKey, nValue: string);
begin

end;

class procedure TKafkaHelper.TopicConfigSet(const nConfig: TKafkaTopicConfig;
  const nItem: TConfigItems; const nExcept: Boolean = True);
begin

end;

class procedure TKafkaHelper.TopicConfigDestroy(const nConfig: TKafkaTopicConfig);
begin

end;

end.
