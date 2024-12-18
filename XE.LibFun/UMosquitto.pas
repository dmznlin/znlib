{*******************************************************************************
  作者: dmzn@163.com 2019-06-05
  描述: mosquitto开发库头文件
*******************************************************************************}
unit UMosquitto;

interface

const
  LIBMOSQUITTO_MAJOR             = 2;
  LIBMOSQUITTO_MINOR             = 0;
  LIBMOSQUITTO_REVISION          = 20; //库版本号

  LIBMOSQUITTO_VERSION_NUMBER    = LIBMOSQUITTO_MAJOR * 1000000 +
    LIBMOSQUITTO_MINOR * 1000 + LIBMOSQUITTO_REVISION;
  ///* looks like 1002001 for e.g. version 1.2.1 * /

  //Log types
  MOSQ_LOG_NONE			             = $00;
  MOSQ_LOG_INFO	                 = 1 shl 0;
  MOSQ_LOG_NOTICE                = 1 shl 1;
  MOSQ_LOG_WARNING               = 1 shl 2;
  MOSQ_LOG_ERR                   = 1 shl 3;
  MOSQ_LOG_DEBUG                 = 1 shl 4;
  MOSQ_LOG_SUBSCRIBE             = 1 shl 5;
  MOSQ_LOG_UNSUBSCRIBE           = 1 shl 6;
  MOSQ_LOG_WEBSOCKETS            = 1 shl 7;
  MOSQ_LOG_INTERNAL              = $80000000;
  MOSQ_LOG_ALL                   = $7FFFFFFF;

  //Error values
  MOSQ_ERR_AUTH_CONTINUE         = -4;
  MOSQ_ERR_NO_SUBSCRIBERS        = -3;
  MOSQ_ERR_SUB_EXISTS            = -2;
  MOSQ_ERR_CONN_PENDING          = -1;
  MOSQ_ERR_SUCCESS               = 0;
  MOSQ_ERR_NOMEM                 = 1;
  MOSQ_ERR_PROTOCOL              = 2;
  MOSQ_ERR_INVAL                 = 3;
  MOSQ_ERR_NO_CONN               = 4;
  MOSQ_ERR_CONN_REFUSED          = 5;
  MOSQ_ERR_NOT_FOUND             = 6;
  MOSQ_ERR_CONN_LOST             = 7;
  MOSQ_ERR_TLS                   = 8;
  MOSQ_ERR_PAYLOAD_SIZE          = 9;
  MOSQ_ERR_NOT_SUPPORTED         = 10;
  MOSQ_ERR_AUTH                  = 11;
  MOSQ_ERR_ACL_DENIED            = 12;
  MOSQ_ERR_UNKNOWN               = 13;
  MOSQ_ERR_ERRNO                 = 14;
  MOSQ_ERR_EAI                   = 15;
  MOSQ_ERR_PROXY                 = 16;
  MOSQ_ERR_PLUGIN_DEFER          = 17;
  MOSQ_ERR_MALFORMED_UTF8        = 18;
  MOSQ_ERR_KEEPALIVE             = 19;
  MOSQ_ERR_LOOKUP                = 20;
  MOSQ_ERR_MALFORMED_PACKET      = 21;
  MOSQ_ERR_DUPLICATE_PROPERTY    = 22;
  MOSQ_ERR_TLS_HANDSHAKE         = 23;
  MOSQ_ERR_QOS_NOT_SUPPORTED     = 24;
  MOSQ_ERR_OVERSIZE_PACKET       = 25;
  MOSQ_ERR_OCSP                  = 26;

  //Option values
  MOSQ_OPT_PROTOCOL_VERSION      = 1;
  MOSQ_OPT_SSL_CTX               = 2;
  MOSQ_OPT_SSL_CTX_WITH_DEFAULTS = 3;
  MOSQ_OPT_RECEIVE_MAXIMUM       = 4;
  MOSQ_OPT_SEND_MAXIMUM          = 5;
  MOSQ_OPT_TLS_KEYFORM           = 6;
  MOSQ_OPT_TLS_ENGINE            = 7;
  MOSQ_OPT_TLS_ENGINE_KPASS_SHA1 = 8;
  MOSQ_OPT_TLS_OCSP_REQUIRED     = 9;
  MOSQ_OPT_TLS_ALPN              = 10;

  MOSQ_QOS_0                     = 0;
  MOSQ_QOS_1                     = 1;
  MOSQ_QOS_2                     = 2;

  ///* MQTT specification restricts client ids to a maximum of 23 characters */
  MOSQ_MQTT_ID_MAX_LENGTH        = 23;

  MQTT_PROTOCOL_V31              = 3;
  MQTT_PROTOCOL_V311             = 4;
  MQTT_PROTOCOL_V5               = 5;

type
  p_mosquitto_message = ^mosquitto_message;
  mosquitto_message = record
    mid        : Integer;
    topic      : PAnsiChar;
    payload    : Pointer;
    payloadlen : Integer;
    qos        : Integer;
    retain     : Boolean;
  end;
  pp_mosquitto_message = ^p_mosquitto_message;

  p_mosquitto = ^mosquitto;
  mosquitto = record
    //null
  end;

  pw_callback    = function (buf: PAnsiChar; size: Integer; rwflag: Integer;
                   userdata: Pointer): Integer; cdecl;
  on_connect     = procedure (mosq: p_mosquitto; obj: Pointer; rc: Integer); cdecl;
  on_disconnect  = procedure (mosq: p_mosquitto; obj: Pointer; rc: Integer); cdecl;
  on_publish     = procedure (mosq: p_mosquitto; obj: Pointer; mid: Integer); cdecl;
  on_message     = procedure (mosq: p_mosquitto; obj: Pointer;
                   const msg: p_mosquitto_message); cdecl;
  on_subscribe   = procedure (mosq: p_mosquitto; obj: Pointer; mid: Integer;
                   qos_count : Integer; const granted_qos: PInteger); cdecl;
  on_unsubscribe = procedure (mosq: p_mosquitto; obj: Pointer; mid: Integer); cdecl;
  on_log         = procedure (mosq: p_mosquitto; obj: Pointer; level: Integer;
                   const str: PAnsiChar); cdecl;
  //call back functions

  mosq_opt_t     = LongInt;
  size_t         = Cardinal;

  p_PAnsiChar    = ^PAnsiChar;
  pp_PansiChar   = ^p_PAnsiChar;

const
  cMosDLL = 'mosquitto.dll';

{* =============================================================================
 *
 * Section: Library version, init, and cleanup
 *
 * ========================================================================== *}
function mosquitto_lib_version(major,minor,revision: PInteger): Integer;
 cdecl; external cMosDLL;
function mosquitto_lib_init(): Integer; cdecl; external cMosDLL;
function mosquitto_lib_cleanup(): Integer; cdecl; external cMosDLL;

{* =============================================================================
 *
 * Section: Client creation, destruction, and reinitialisation
 *
 * ========================================================================== *}
function mosquitto_new(const id: PAnsiChar; clean_session: Boolean;
  obj: Pointer): p_mosquitto; cdecl; external cMosDLL;
procedure mosquitto_destroy(const mosq: p_mosquitto); cdecl; external cMosDLL;

function mosquitto_reinitialise(mosq: p_mosquitto; const id: PAnsiChar;
  clean_session: Boolean; obj: Pointer): Integer; cdecl; external cMosDLL;
function mosquitto_will_set(mosq: p_mosquitto; const topis: PAnsiChar;
  payloadlen: Integer; const payload: Pointer; qos: Integer;
  retain: Boolean): Integer; cdecl; external cMosDLL;
function mosquitto_will_clear(mosq: p_mosquitto): Integer; cdecl; external cMosDLL;

function mosquitto_username_pw_set(mosq: p_mosquitto;
  const username,password: PAnsiChar): Integer; cdecl; external cMosDLL;
function mosquitto_connect(mosq: p_mosquitto; const host: PAnsiChar;
  port: Integer; keepalive: Integer):Integer; cdecl; external cMosDLL;
function mosquitto_connect_bind(mosq: p_mosquitto; const host: PAnsiChar;
  port: Integer; keepalive: Integer;
  const bind_address: PAnsiChar): Integer; cdecl; external cMosDLL;
function mosquitto_connect_async(mosq: p_mosquitto; const host: PAnsiChar;
  port: Integer; keepalive: Integer):Integer; cdecl; external cMosDLL;
function mosquitto_connect_bind_async(mosq: p_mosquitto; const host: PAnsiChar;
  port: Integer; keepalive: Integer;
  const bind_address: PAnsiChar): Integer; cdecl; external cMosDLL;
function mosquitto_connect_srv(mosq: p_mosquitto; const host: PAnsiChar;
  keepalive: Integer;
  const bind_address: PAnsiChar):Integer; cdecl; external cMosDLL;

function mosquitto_reconnect(mosq: p_mosquitto): Integer; cdecl; external cMosDLL;
function mosquitto_reconnect_async(mosq: p_mosquitto): Integer; cdecl; external cMosDLL;
function mosquitto_disconnect(mosq: p_mosquitto): Integer; cdecl; external cMosDLL;

{* =============================================================================
 *
 * Section: Publishing, subscribing, unsubscribing
 *
 * ========================================================================== *}
function mosquitto_publish(mosq: p_mosquitto; mid: PInteger;
  const topic: PAnsiChar; payloadlen: Integer; const payload: Pointer;
  qos: Integer; retain: Boolean): Integer; cdecl; external cMosDLL;
function mosquitto_subscribe(mosq: p_mosquitto; mid: PInteger;
  const sub: PAnsiChar; qos: Integer):Integer; cdecl; external cMosDLL;
function mosquitto_unsubscribe(mosq: p_mosquitto; mid: PInteger;
  const sub: PAnsiChar):Integer; cdecl; external cMosDLL;

{* =============================================================================
 *
 * Section: Struct mosquitto_message helper functions
 *
 * ========================================================================== *}
function mosquitto_message_copy(dst,src : p_mosquitto_message):Integer;
  cdecl; external cMosDLL;
procedure mosquitto_message_free(msg : pp_mosquitto_message);
  cdecl; external cMosDLL;
procedure mosquitto_message_clear(msg: p_mosquitto_message );
  cdecl; external cMosDLL;
//xxxxx

{* =============================================================================
 *
 * Section: Network loop (managed by libmosquitto)
 *
 * ========================================================================== *}
function mosquitto_loop(mosq: p_mosquitto; timeout: Integer;
  max_packets: Integer): Integer; cdecl; external cMosDLL;
function mosquitto_loop_forever(mosq: p_mosquitto; timeout: Integer;
  max_packets: Integer): Integer; cdecl; external cMosDLL;
function mosquitto_loop_start(mosq: p_mosquitto): Integer; cdecl; external cMosDLL;
function mosquitto_loop_stop(mosq: p_mosquitto; force: Boolean): Integer;
  cdecl; external cMosDLL;
//xxxxx

{* =============================================================================
 *
 * Section: Network loop (for use in other event loops)
 *
 * ========================================================================== *}
function mosquitto_loop_read(mosq: p_mosquitto;
  max_packets: Integer): Integer; cdecl; external cMosDLL;
function mosquitto_loop_write(mosq: p_mosquitto;
  max_packets: Integer): Integer; cdecl; external cMosDLL;
function mosquitto_loop_misc(mosq: p_mosquitto): Integer; cdecl; external cMosDLL;

{* =============================================================================
 *
 * Section: Network loop (helper functions)
 *
 * ========================================================================== *}
function  mosquitto_socket(mosq: p_mosquitto): Integer; cdecl; external cMosDLL;
function  mosquitto_want_write(mosq: p_mosquitto): Boolean; cdecl; external cMosDLL;
function  mosquitto_threaded_set(mosq: p_mosquitto; threaded: Boolean): Integer;
  cdecl; external cMosDLL;
//xxxxx

{* =============================================================================
 *
 * Section: Client options
 *
 * ========================================================================== *}
function  mosquitto_opts_set(mosq: p_mosquitto; option: mosq_opt_t;
  value: Pointer): Integer; cdecl; external cMosDLL;
function mosquitto_int_option(mosq: p_mosquitto; option: mosq_opt_t;
  value: Integer): Integer; cdecl; external cMosDLL;
function mosquitto_void_option(mosq: p_mosquitto; option: mosq_opt_t;
  value: Pointer): Integer; cdecl; external cMosDLL;
function mosquitto_string_option(mosq: p_mosquitto; option: mosq_opt_t;
  const nvalue: PAnsiChar): Integer; cdecl; external cMosDLL;

function mosquitto_reconnect_delay_set(mosq: p_mosquitto;
  reconnect_delay,reconnect_delay_max: Cardinal;
  reconnect_exponential_backoff: Boolean): Integer; cdecl; external cMosDLL;
function mosquitto_max_inflight_messages_set(mosq: p_mosquitto;
  max_inflight_messages: Cardinal):Integer; cdecl; external cMosDLL;
procedure mosquitto_message_retry_set(mosq: p_mosquitto;
  message_retry: Cardinal); cdecl; external cMosDLL;
procedure mosquitto_user_data_set(mosq: p_mosquitto;
  obj: Pointer); cdecl; external cMosDLL;
function mosquitto_userdata(mosq: p_mosquitto): Pointer; cdecl; external cMosDLL;

{* =============================================================================
 *
 * Section: TLS support
 *
 * ========================================================================== *}
function mosquitto_tls_set(mosq: p_mosquitto; cafile: PAnsiChar;
  const capath,certfile,keyfile: PAnsiChar;
  pw_callback: pw_callback): Integer; cdecl; external cMosDLL;
function mosquitto_tls_insecure_set(mosq: p_mosquitto;
  value: Boolean):Integer; cdecl; external cMosDLL;
function mosquitto_tls_opts_set(mosq: p_mosquitto; cert_reqs: Integer;
  const tls_version, ciphers: PAnsiChar): Integer; cdecl; external cMosDLL;
function mosquitto_tls_psk_set(mosq: p_mosquitto;
  const psk,identity,ciphers: PAnsiChar):Integer; cdecl; external cMosDLL;
//xxxxx

{* =============================================================================
 *
 * Section: Callbacks
 *
 * ========================================================================== *}
procedure mosquitto_connect_callback_set(mosq: p_mosquitto;
  on_connect: on_connect); cdecl; external cMosDLL;
procedure mosquitto_disconnect_callback_set(mosq: p_mosquitto;
  on_disconnect: on_disconnect); cdecl; external cMosDLL;
procedure mosquitto_publish_callback_set(mosq: p_mosquitto;
  on_publish: on_publish); cdecl; external cMosDLL;
procedure mosquitto_message_callback_set(mosq: p_mosquitto;
  on_message:  on_message); cdecl; external cMosDLL;
procedure mosquitto_subscribe_callback_set(mosq: p_mosquitto;
  on_subscribe: on_subscribe); cdecl; external cMosDLL;
procedure mosquitto_unsubscribe_callback_set(mosq: p_mosquitto;
  on_unsubscribe: on_unsubscribe); cdecl; external cMosDLL;
procedure mosquitto_log_callback_set(mosq: p_mosquitto;
  on_log: on_log); cdecl; external cMosDLL;

{* =============================================================================
 *
 * Section: SOCKS5 proxy functions
 *
 * ========================================================================== *}
function mosquitto_socks5_set(mosq: p_mosquitto; const host: PAnsiChar;
  port: Integer;
  const username,password: PAnsiChar): Integer; cdecl; external cMosDLL;
//xxxxx

{* =============================================================================
 *
 * Section: Utility functions
 *
 * ========================================================================== *}
function mosquitto_strerror(mosq_errno: Integer): PAnsiChar;
  cdecl; external cMosDLL;
function mosquitto_connack_string(connack_code: Integer): PAnsiChar;
  cdecl; external cMosDLL;
function mosquitto_reason_string(reason_code: Integer): PAnsiChar;
  cdecl; external cMosDLL;
function mosquitto_string_to_command(str: PAnsiChar; cmd: PInteger): Integer;
  cdecl; external cMosDLL;

function mosquitto_sub_topic_tokenise(const subtopic: PAnsiChar;
  topics: pp_PAnsiChar; count: PInteger):Integer; cdecl; external cMosDLL;
function mosquitto_sub_topic_tokens_free(topics: pp_PAnsiChar;
  count: Integer):Integer; cdecl; external cMosDLL;

function mosquitto_topic_matches_sub(const sub,topic: PAnsiChar;
  result: PBoolean):Integer; cdecl; external cMosDLL;
function mosquitto_topic_matches_sub2(const sub: PAnsiChar;
 sublen: size_t; const topic: PAnsiChar; topiclen: size_t;
 result: PBoolean): Integer; cdecl; external cMosDLL;
function mosquitto_pub_topic_check(const topic: PAnsiChar): Integer;
  cdecl; external cMosDLL;
function mosquitto_pub_topic_check2(const topic: PAnsiChar;
  topiclen: size_t): Integer; cdecl; external cMosDLL;
function mosquitto_sub_topic_check(const topic: PAnsiChar): Integer;
  cdecl; external cMosDLL;
function mosquitto_sub_topic_check2(const topic: PAnsiChar;
  topiclen: size_t): Integer; cdecl; external cMosDLL;
//xxxxx

implementation

end.
