{*******************************************************************************
  作者: dmzn@163.com 2019-04-23
  描述: 基于插件模式的模块管理器

  备注:
  *.当一个系统由多个独立的模块组成时,可以使用插件管理器实现隔离.
  *.例如:一个中间件需要同时支持业务服务和硬件服务,只需要编写继承自
    TPlugEventWorkerBase的两个子类,然后将子类注册到插件管理器.
  *.两个子类也可从中间件中分离,作为独立的业务中间件和硬件服务中间件.
*******************************************************************************}
unit UMgrPlugs;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, {$IFDEF MSWin}Winapi.Windows,{$ENDIF}
  UBaseObject;

const
  {*plug event*}
  cPlugEvent_InitSystemObject     = $0001;
  cPlugEvent_RunSystemObject      = $0002;
  cPlugEvent_FreeSystemObject     = $0003;
  cPlugEvent_BeforeStartServer    = $0004;
  cPlugEvent_AfterServerStarted   = $0005;
  cPlugEvent_BeforeStopServer     = $0006;
  cPlugEvent_AfterStopServer      = $0007;
  cPlugEvent_BeforeHaltServer     = $0008;

type
  TPlugModuleInfo = record
    FModuleID       : string;    //标识
    FModuleName     : string;    //名称
    FModuleAuthor   : string;    //作者
    FModuleVersion  : string;    //版本
    FModuleDesc     : string;    //描述
    FModuleFile     : string;    //文件
    FModuleBuildTime: TDateTime; //编译时间
  end;

  TPlugModuleList = array of TPlugModuleInfo;
  //模块信息列表

  PPlugMenuItem = ^TPlugMenuItem;
  TPlugMenuItem = record
    FModule     : string;        //模块标识
    FName       : string;        //菜单名
    FCaption    : string;        //菜单标题
    FFormID     : Integer;       //功能窗体
    FDefault    : Boolean;       //默认启动
  end;

  TPlugMenuItems = array of TPlugMenuItem;
  //模块菜单列表

  TPlugEventWorkerBase = class(TObject)
  private
    FWorkerValid: Boolean;
    //有效标识
  protected
    procedure InitSystemObject(const nData: Pointer); virtual;
    //主程序启动时初始化
    procedure RunSystemObject(const nData: Pointer); virtual;
    //主程序启动后运行
    procedure FreeSystemObject(const nData: Pointer); virtual;
    //主程序退出时释放
    procedure BeforeStartServer(const nData: Pointer); virtual;
    //服务启动之前调用
    procedure AfterServerStarted(const nData: Pointer); virtual;
    //服务启动之后调用
    procedure BeforeStopServer(const nData: Pointer); virtual;
    //服务关闭之前调用
    procedure AfterStopServer(const nData: Pointer); virtual;
    //服务关闭之后调用
    function WhenEventActive(const nEvent: string;
      const nData: Pointer): Boolean; overload; virtual;
    function WhenEventActive(const nEvent: Integer;
      const nData: Pointer): Boolean; overload; virtual;
    //外部触发特定事件
    procedure GetExtendMenu(var nMenus: TPlugMenuItems); virtual;
    //加载扩展菜单项
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    class function ModuleInfo: TPlugModuleInfo; virtual;
    //模块信息
  end;

  TPlugManager = class(TManagerBase)
  private
    FWorkers: array of TPlugEventWorkerBase;
    {*工作对象*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*延迟执行*}
    procedure InitSystemObject(const nData: Pointer = nil);
    procedure RunSystemObject(const nData: Pointer = nil);
    procedure FreeSystemObject(const nData: Pointer = nil);
    {*对象业务*}
    procedure BeforeStartServer(const nData: Pointer = nil);
    procedure AfterServerStarted(const nData: Pointer = nil);
    procedure BeforeStopServer(const nData: Pointer = nil);
    procedure AfterStopServer(const nData: Pointer = nil);
    {*服务起停*}
    procedure ActiveEvent(const nEvent: string; const nData: Pointer = nil;
      const nValidWorker: Boolean = True); overload;
    procedure ActiveEvent(const nEvent: Integer; const nData: Pointer = nil;
      const nValidWorker: Boolean = True); overload;
    {*特定事件*}
    procedure AddEventWorker(const nWorker: TPlugEventWorkerBase);
    procedure DeleteWorker(const nModule: string);
    {*增减对象*}
    function GetMenuItems: TPlugMenuItems;
    {*模块菜单*}
    function GetModuleInfo(const nModule: string): TPlugModuleInfo;
    function GetModuleInfoList: TPlugModuleList;
    {*模块列表*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
  end;

var
  gPlugManager: TPlugManager = nil;
  //全局使用

implementation

uses
  UManagerGroup, ULibFun;

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TPlugManager, '插件管理器', nEvent);
end;

constructor TPlugEventWorkerBase.Create;
begin
  inherited;
  //nothing
end;

destructor TPlugEventWorkerBase.Destroy;
begin
  //nothing
  inherited;
end;

procedure TPlugEventWorkerBase.InitSystemObject;
begin

end;

procedure TPlugEventWorkerBase.RunSystemObject;
begin

end;

procedure TPlugEventWorkerBase.FreeSystemObject;
begin

end;

procedure TPlugEventWorkerBase.BeforeStartServer;
begin

end;

procedure TPlugEventWorkerBase.AfterServerStarted;
begin

end;

procedure TPlugEventWorkerBase.BeforeStopServer;
begin

end;

procedure TPlugEventWorkerBase.AfterStopServer;
begin

end;

procedure TPlugEventWorkerBase.GetExtendMenu(var nMenus: TPlugMenuItems);
begin

end;

function TPlugEventWorkerBase.WhenEventActive(const nEvent: string;
  const nData: Pointer): Boolean;
begin
  Result := False;
end;

function TPlugEventWorkerBase.WhenEventActive(const nEvent: Integer;
  const nData: Pointer): Boolean;
begin
  Result := False;
end;

class function TPlugEventWorkerBase.ModuleInfo: TPlugModuleInfo;
{$IFDEF MSWin}var nBuf: array[0..MAX_PATH-1] of Char;{$ENDIF}
begin
  with Result do
  begin
    FModuleID       := '{0EE5410B-9334-45DE-A186-713C11434392}';
    FModuleName     := '通用框架插件基类';
    FModuleAuthor   := 'dmzn@163.com';
    FModuleVersion  := '2019-01-01';
    FModuleDesc     := '插件通过继承该类,可以获得框架的接口.';
    FModuleBuildTime:= TDateTimeHelper.Str2DateTime('2019-01-01 00:01:01');

    {$IFDEF MSWin}
    FModuleFile := Copy(nBuf, 1, GetModuleFileName(HInstance, nBuf, MAX_PATH));
    //module full file name
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------
constructor TPlugManager.Create;
begin
  inherited;
  SetLength(FWorkers, 0);
end;

destructor TPlugManager.Destroy;
begin

  inherited;
end;

//Date: 2019-04-23
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TPlugManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TPlugManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TPlugManager.Create;
    gMG.FPlugManager := gMG.FManagers[nIdx].FManager as TPlugManager;
  end else
  begin
    gMG.FPlugManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gPlugManager := gMG.FPlugManager;
  //启用全局变量
end;

procedure TPlugManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TPlugManager', ['TLogManager']);
  //检查支持
end;

procedure TPlugManager.RunBeforUnregistAllManager;
var nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
      FreeAndNil(FWorkers[nIdx]);
    SetLength(FWorkers, 0);
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: 事件对象
//Desc: 增加nWorker对象
procedure TPlugManager.AddEventWorker(const nWorker: TPlugEventWorkerBase);
var nIdx: Integer;
begin
  SyncEnter;
  try
    nIdx := Length(FWorkers);
    SetLength(FWorkers, nIdx + 1);

    nWorker.FWorkerValid := True;
    FWorkers[nIdx] := nWorker;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: 模块标识
//Desc: 删除指定对象
procedure TPlugManager.DeleteWorker(const nModule: string);
var nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
     if FWorkers[nIdx].ModuleInfo.FModuleID = nModule then
      FWorkers[nIdx].FWorkerValid := False;
    //xxxxx
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Desc: 获取扩展菜单项
function TPlugManager.GetMenuItems: TPlugMenuItems;
var nIdx: Integer;
begin
  SyncEnter;
  try
    SetLength(Result, 0);
    //init

    for nIdx := Low(FWorkers) to High(FWorkers) do
      FWorkers[nIdx].GetExtendMenu(Result);
    //xxxxx
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: 模块标识
//Desc: 获取nModule的描述
function TPlugManager.GetModuleInfo(const nModule: string): TPlugModuleInfo;
var nIdx: Integer;
    nInit: TPlugModuleInfo;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      Result := FWorkers[nIdx].ModuleInfo;
      if Result.FModuleID = nModule then Exit;
    end;

    FillChar(nInit, SizeOf(nInit), #0);
    Result := nInit;
    //default
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: 无
//Desc: 获取模块列表
function TPlugManager.GetModuleInfoList: TPlugModuleList;
var nIdx,nInt: Integer;
begin
  SyncEnter;
  try
    SetLength(Result, Length(FWorkers));
    nInt := 0;
    //init

    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      Result[nInt] := FWorkers[nIdx].ModuleInfo;
      Inc(nInt);
    end;
  finally
    SyncLeave;
  end;
end;

procedure TPlugManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx,nInt: Integer;
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);
    nInt := 0;

    for nIdx := Low(FWorkers) to High(FWorkers) do
     if FWorkers[nIdx].FWorkerValid then
      Inc(nInt);
    //xxxxx

    if not nFriendly then
    begin
      nList.Add('NumWorker=' + IntToStr(Length(FWorkers)));
      nList.Add('NumWorkerValid=' + IntToStr(nInt));
      Exit;
    end;

    nList.Add(FixData('NumWorker:', IntToStr(Length(FWorkers))));
    nList.Add(FixData('NumWorkerValid:', IntToStr(nInt)));
  finally
    SyncLeave;
  end;
end;

//------------------------------------------------------------------------------
procedure TPlugManager.InitSystemObject(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_InitSystemObject, nData);
end;

procedure TPlugManager.RunSystemObject(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_RunSystemObject, nData);
end;

procedure TPlugManager.FreeSystemObject(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_FreeSystemObject, nData);
end;

procedure TPlugManager.BeforeStartServer(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_BeforeStartServer, nData);
end;

procedure TPlugManager.AfterServerStarted(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_AfterServerStarted, nData);
end;

procedure TPlugManager.BeforeStopServer(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_BeforeStopServer, nData);
end;

procedure TPlugManager.AfterStopServer(const nData: Pointer);
begin
  ActiveEvent(cPlugEvent_AfterStopServer, nData);
end;

//Date: 2019-04-23
//Parm: 事件标识
//Desc: 返回nEventID的描述
function Event2Str(const nEventID: Integer): string;
begin
  case nEventID of
   cPlugEvent_InitSystemObject   : Result := 'InitSystemObject';
   cPlugEvent_RunSystemObject    : Result := 'RunSystemObject';
   cPlugEvent_FreeSystemObject   : Result := 'FreeSystemObject';
   cPlugEvent_BeforeStartServer  : Result := 'BeforeStartServer';
   cPlugEvent_AfterServerStarted : Result := 'AfterServerStarted';
   cPlugEvent_BeforeStopServer   : Result := 'BeforeStopServer';
   cPlugEvent_AfterStopServer    : Result := 'AfterStopServer';
   cPlugEvent_BeforeHaltServer   : Result := 'BeforeHaltServer'
   else Result := Format('Unknown Command(%d)', [nEventID]);
  end;
end;

//Date: 2019-04-23
//Parm: 事件;数据;触发有效
//Desc: 执行nEvent事件
procedure TPlugManager.ActiveEvent(const nEvent: string; const nData: Pointer;
  const nValidWorker: Boolean);
var nStr: string;
    nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      if nValidWorker and (not FWorkers[nIdx].FWorkerValid) then Continue;
      //worker is close

      with FWorkers[nIdx] do
      try
        WhenEventActive(nEvent, nData);
        //default deal
      except
        on nErr: Exception do
        begin
          nStr := '模块[ %s ]执行[ %s ]时错误,描述: %s';
          WriteLog(Format(nStr, [ModuleInfo.FModuleName, nEvent,
            nErr.Message]));
          //xxxxx
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-04-23
//Parm: 事件;数据;触发有效
//Desc: 执行nEvent事件
procedure TPlugManager.ActiveEvent(const nEvent: Integer; const nData: Pointer;
  const nValidWorker: Boolean);
var nStr: string;
    nIdx: Integer;
begin
  SyncEnter;
  try
    for nIdx := Low(FWorkers) to High(FWorkers) do
    begin
      if nValidWorker and (not FWorkers[nIdx].FWorkerValid) then Continue;
      //worker is close

      with FWorkers[nIdx] do
      try
        if WhenEventActive(nEvent, nData) then Continue;
        //default deal

        case nEvent of
         cPlugEvent_InitSystemObject   : InitSystemObject(nData);
         cPlugEvent_RunSystemObject    : RunSystemObject(nData);
         cPlugEvent_FreeSystemObject   : FreeSystemObject(nData);
         cPlugEvent_BeforeStartServer  : BeforeStartServer(nData);
         cPlugEvent_AfterServerStarted : AfterServerStarted(nData);
         cPlugEvent_BeforeStopServer   : BeforeStopServer(nData);
         cPlugEvent_AfterStopServer    : AfterStopServer(nData);

         cPlugEvent_BeforeHaltServer   : //服务退出
          begin
            BeforeStopServer(nData);
            AfterStopServer(nData);
            //关闭服务
            FreeSystemObject(nData);
            //释放对象
          end;
        end;
      except
        on nErr: Exception do
        begin
          nStr := '模块[ %s ]执行[ %s ]时错误,描述: %s';
          WriteLog(Format(nStr, [ModuleInfo.FModuleName, Event2Str(nEvent),
            nErr.Message]));
          //xxxxx
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

end.
