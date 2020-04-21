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
  sMsgType: array[TMsgType] of string = ('Info', 'Warn', 'Error', 'Event',
    'IRC', 'Command', 'CmdWait');
  //消息类型描述

type
  PMsgPath = ^TMsgPath;
  TMsgPath = record
    FServer   : string;                   //服务器名称
    FAppName  : string;                   //应用名称
    FInstance : string;                   //实例代码
    FObject   : string;                   //对象标识
    FType     : TMsgType;                 //消息类型
    FExtend   : string;                   //扩展路径
  end;

  PMsgData = ^TMsgData;
  TMsgData = record
    FPath: TMsgPath;                      //消息路径
    FData: string;                        //消息数据
  end;

  TMsgCallProc = procedure(const nMsg: PMsgData);
  TMsgCallEvent = procedure(const nMsg: PMsgData) of object;

  PMsgReceiver = ^TMsgReceiver;
  TMsgReceiver = record
    FID   : string;                       //接收标识
    FName : string;                       //接收名称
    FPath : TMsgPath;                     //关注路径
    FTypes: TMsgTypes;                    //关注类型
    FCallProc: TMsgCallProc;              //回调过程
    FCallEvent: TMsgCallEvent;            //回调事件
  end;

  TMQTTMessager = class(TManagerBase)
  private
    FChannel: TMQTTClient;
    {*消息通道*}
    FDefaultPath: TMsgPath;
    {*默认路径*}
    FReceivers: TList;
    {*消息处理者*}
  protected
    procedure DoRecvMessage(const nTopic,nPayload: string);
    {*处理消息*}
    function ParsePath(const nTopic: string; const nPath: PMsgPath): Boolean;
    {*解析路径*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    procedure RunBeforApplicationHalt; override;
    {*延迟执行*}
    procedure InitPath(const nServer,nAppName,nInstance: string);
    {*设置路径*}
    procedure InitReceiver(const nReceiver: PMsgReceiver);
    function MakeTopic(const nReceiver: PMsgReceiver): string;
    {*处理消息*}
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

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TMQTTMessager, '系统消息中心', nEvent);
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

  gMQTTMessager := gMG.FMessageCenter;
  //启用全局变量
end;

//Date: 2019-08-30
//Desc: 创建MQTT通道
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
//Desc: 程序准备关闭,断开连接
procedure TMQTTMessager.RunBeforApplicationHalt;
begin
  FChannel.Disconnect;
end;

//Date: 2019-08-30
//Desc: 清理资源
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

//Date: 2019-08-30
//Parm: 消息处理对象
//Desc: 初始化nReceiver数据结构
procedure TMQTTMessager.InitReceiver(const nReceiver: PMsgReceiver);
var nDef: TMsgReceiver;
begin
  FillChar(nDef, SizeOf(TMsgReceiver), #0);
  nReceiver.FID := gMG.FSerialIDManager.GetSID;
  nReceiver.FPath := FDefaultPath;
end;

//Date: 2019-08-30
//Parm: 消息处理对象
//Desc: 依据nReceiver构建订阅主题
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
//Parm: 主题;路径
//Desc: 依据主题定义解析nTopic
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
//Parm: 主题;数据
//Desc: 解析主题数据
procedure TMQTTMessager.DoRecvMessage(const nTopic, nPayload: string);
var nPath: TMsgPath;
begin
  if not ParsePath(nTopic, @nPath) then Exit;
  //invalid path

  WriteLog(nTopic + ' ' + nPath.FExtend);
end;

end.
