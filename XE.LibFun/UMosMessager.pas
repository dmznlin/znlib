{*******************************************************************************
  作者: dmzn@163.com 2019-08-15
  描述: 基于mosquitto的消息中心

  备注:
  *.MQTT的主题使用路径格式定义: Root/Path1/Path2/Path3/Path4...
    一级Root:Broker名称,如: TXYun37,指部署在腾讯云上IP末位是37的服务器.
    二级Path:应用名称,如: Delivery,指物流发货系统
    三级Path:应用实例代码,如:HuaXin_YX,指华新集团阳新工厂
    四级Path:应用对象标识,如:TMgrOPC,指OPC服务对象.
    五级Path:数据类型,如:Info,Error,Warn等
    以上五级路径为必要路径,其余可选,路径最长不超过1024字节.
*******************************************************************************}
unit UMosMessager;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, UMosMQTT, UBaseObject;

type
  TMsgType = (mtInfo, mtWarn, mtError, mtEvent, mtIRC, mtCmd, mtWaitCmd);
  //消息类型: 信息;警告;错误;事件;即时通讯;命令;命令并等待结果
  TMsgTypes = set of TMsgType;

const
  sMsgType: array[0..6] of string = ('Info', 'Warn', 'Error', 'Event', 'IRC',
    'Command', 'CmdWait');
  //消息类型描述

type
  PMsgPath = ^TMsgPath;
  TMsgPath = record
    FServer    : string;                  //服务器名称
    FAppName   : string;                  //应用名称
    FInstance  : string;                  //实例代码
    FObject    : string;                  //对象标识
    FType      : TMsgType;                //消息类型
    FExtend    : string;                  //扩展路径
  end;

  PMsgData = ^TMsgData;
  TMsgData = record
    FPath      : TMsgPath;                //消息路径
    FData      : string;                  //消息数据
  end;

  TMQTTMessager = class(TManagerBase)
  private
    FChannel: TMQTTClient;
    {*消息通道*}
    FDefaultPath: TMsgPath;
    {*默认路径*}
  protected
    procedure DoRecvMessage(const nTopic,nPayload: string);
    {*处理消息*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*延迟执行*}
    procedure InitPath(const nServer,nAppName,nInstance: string);
    {*设置路径*}
    property Channel: TMQTTClient read FChannel;
    property DefaultPath: TMsgPath read FDefaultPath;
    {*属性相关*}
  end;

var
  gMQTTMessager: TMQTTMessager = nil;
  //全局使用

implementation

uses
  UManagerGroup, ULibFun;

constructor TMQTTMessager.Create;
begin
  FChannel := nil;
  InitPath('Moquitto', 'Delivery', 'Factory');
end;

destructor TMQTTMessager.Destroy;
begin
  //xxx
  inherited;
end;

//Date: 2019-08-15
//Parm: 是否注册
//Desc: 向系统注册管理器对象
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

procedure TMQTTMessager.RunAfterRegistAllManager;
begin
  FChannel := TMQTTClient.Create;
  with FChannel do
  begin
    EventMode := emThread;
    OnMessageEvent := DoRecvMessage;
  end;
end;

procedure TMQTTMessager.RunBeforUnregistAllManager;
begin
  FChannel.Free;
end;

//Date: 2019-08-15
//Parm: 服务;应用;实例
//Desc: 初始化默认路径
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

procedure TMQTTMessager.DoRecvMessage(const nTopic, nPayload: string);
begin

end;

end.
