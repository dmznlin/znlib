{*******************************************************************************
  作者: dmzn@163.com 2017-03-27
  描述: 统一管理各种管理器的全局变量
*******************************************************************************}
unit UManagerGroup;

{$I LibFun.Inc}
interface

uses
  System.Rtti, System.SysUtils, System.Classes, UBaseObject, UObjectPool,
  UMemDataPool,
  {$IFDEF EnableDBManager}UDBManager,{$ENDIF}
  {$IFDEF EnableMenuManager}UMenuManager,{$ENDIF}
  {$IFDEF EnableLogManager}UMgrLog,{$ENDIF}
  {$IFDEF EnablePlugManager}UMgrPlugs,{$ENDIF}
  {$IFDEF EnableTaskMonitor}UTaskMonitor,{$ENDIF}
  {$IFDEF EnableThreadPool}UThreadPool,{$ENDIF}
  {$IFDEF EnableChannelManager}UMgrChannel,{$ENDIF}
  {$IFDEF EnableMQTTMessager}UMosMessager,{$ENDIF}
  {$IFDEF EnableParamManager}UParameters,{$ENDIF} ULibFun, UWaitItem;

type
  TSimpleLogger = class(TObject)
  private
    FFileExt: string;
    FLogField: string;
    {*日志特征*}
    FWritePath: string;
    FWriteLock: TCrossProcWaitObject;
    {*同步锁定*}
  public
    constructor Create(const nPath: string; const nExt: string = '';
      const nField: string = ''; const nSyncLock: Boolean = False);
    destructor Destroy; override;
    {*创建释放*}
    procedure WriteLog(const nClass,nDesc,nEvent: string);
    {*记录日志*}
  end;

  TManagersCallMethod = reference to procedure (const nObj: TObject;
    const nInstance: TRttiInstanceType; const nMethod: TRttiMethod);
  //xxxxx

  PManagerGroup = ^TManagerGroup;
  TManagerGroup = record
  public
    const 
      sAllManager = 'ALLManager';
      {*常量定义*}
    type
      TItem = record
        FClass   : TClass;
        FManager : TManagerBase;
      end;
      {*实例定义*}
    var
      FManagers: array of TItem;
      {*实例列表*}
      FSimpleLogger: TSimpleLogger;
      {*简易日志*}
  public
    FObjectPool: TObjectPoolManager;
    //对象缓冲池
    FMemDataManager: TMemDataManager;
    //内存管理器
    FSerialIDManager: TSerialIDManager;
    //编号管理器
    FObjectManager: TCommonObjectManager;
    //对象管理器
    {$IFDEF EnableLogManager}FLogManager: TLogManager;{$ENDIF}
    //日志管理器
    {$IFDEF EnablePlugManager}FPlugManager: TPlugManager;{$ENDIF}
    //插件管理器
    {$IFDEF EnableTaskMonitor}FTaskMonitor: TTaskMonitor;{$ENDIF}
    //任务管理器
    {$IFDEF EnableThreadPool}FThreadPool: TThreadPoolManager;{$ENDIF}
    //线程管理器
    {$IFDEF EnableMQTTMessager}FMessageCenter: TMQTTMessager;{$ENDIF}
    //消息管理器
    {$IFDEF EnableDBManager}FDBManager: TDBManager;{$ENDIF}
    //数据库管理器
    {$IFDEF EnableMenuManager}FMenuManager: TMenuManager;{$ENDIF}
    //菜单管理器
    {$IFDEF EnableParamManager}FParamsManager: TParameterManager;{$ENDIF}
    //参数管理器
    {$IFDEF EnableChannelManager}FChannelManager: TChannelManager;{$ENDIF}
    //RemObjects通道管理器
  private
    procedure InnerLog(const nEvent: string);
    //管理器内部日志
  public
    procedure CallManagersMethod(const nMethodName: string;
      const nObjMethod: Boolean; const nCallback: TManagersCallMethod);
    //枚举所有管理器执行特定方法
    procedure RegistAll(const nReg: Boolean);
    //注册所有
    procedure CheckSupport(const nCallClass,nManagerName: string;
      const nManager: TObject); overload;
    procedure CheckSupport(const nCallClass: string;
      const nManagers: TStringHelper.TStringArray); overload;
    //验证所需管理器是否正常
    procedure RunAfterApplicationStart();
    procedure RunBeforApplicationHalt();
    //关联主进程运行,处理特定资源
    procedure GetManagersStatus(const nList: TStrings);
    //获取有效管理器的当前状态
    procedure WriteLog(const nClass,nDesc,nEvent: string); overload;
    procedure WriteLog(const nClass: TClass; const nDesc,nEvent: string); overload;
    //日志管理器未生效时记录简易日志
  end;

var
  gMG: TManagerGroup;
  //全局使用
  
implementation

constructor TSimpleLogger.Create(const nPath,nExt,nField: string;
  const nSyncLock: Boolean);
begin
  FFileExt := Trim(nExt);
  if FFileExt = '' then
    FFileExt := '.log';
  //xxxxx

  FLogField := Trim(nField);
  if FLogField = '' then
    FLogField := #9;
  //xxxxx

  if not DirectoryExists(nPath) then
    ForceDirectories(nPath);
  FWritePath := nPath;

  if nSyncLock then
       FWriteLock := TCrossProcWaitObject.Create()
  else FWriteLock := nil; //for thread or process sync
end;

destructor TSimpleLogger.Destroy;
begin
  FWriteLock.Free;
  inherited;
end;

//Date: 2021-01-04
//Parm: 日志对象类名;对象描述;事件
//Desc: 向日志文件中写入nClass.nEvent事件
procedure TSimpleLogger.WriteLog(const nClass,nDesc,nEvent: string);
var nStr: string;
    nFile: TextFile;
begin
  if Assigned(FWriteLock) then
    FWriteLock.SyncLockEnter(True);
  //xxxxx
  try
    nStr := FWritePath +  TDateTimeHelper.Date2Str(Now) + FFileExt;
    AssignFile(nFile, nStr);

    if FileExists(nStr) then
         Append(nFile)
    else Rewrite(nFile);

    nStr := FormatDateTime('hh:nn:ss.zzz', Time()) + FLogField +
            Copy(nClass, 1, 32) + FLogField;
    //时间,类名

    if nDesc <> '' then
      nStr := nStr + nDesc + FLogField;            //描述
    WriteLn(nFile, nStr + nEvent);                 //写入事件
  finally
    if Assigned(FWriteLock) then
      FWriteLock.SyncLockLeave(True);
    CloseFile(nFile);
  end;
end;

//------------------------------------------------------------------------------
//Date: 2019-05-27
//Parm: 方法名称;对象方法;回调方法
//Desc: 枚举所有管理,执行nMethodName方法
procedure TManagerGroup.CallManagersMethod(const nMethodName: string;
  const nObjMethod: Boolean; const nCallBack: TManagersCallMethod);
var nObj: TObject;
    nCtx: TRttiContext;
    nType: TRttiType;
    nRF: TRttiField;
    nMethod: TRttiMethod;
    nInstance: TRttiInstanceType;
begin
  nCtx := TRttiContext.Create;
  try
    nType := nCtx.GetType(TypeInfo(TManagerGroup));
    for nRF in nType.GetFields do
    begin
      if nRF.FieldType.TypeKind <> tkClass then Continue;
      nInstance := nRF.FieldType.AsInstance;
      nMethod := nInstance.GetMethod(nMethodName);

      if Assigned(nMethod) then
      begin
        if nObjMethod then
        begin
          nObj := nRF.GetValue(@gMG).AsObject;
          if Assigned(nObj) then
            nCallBack(nObj, nInstance, nMethod);
          //object call
        end else
        begin
          nCallBack(nil, nInstance, nMethod);
          //type call
        end;
      end;
    end;
  finally
    nCtx.Free;
  end;
end;

//Date: 2017-03-27
//Parm: 是否注册
//Desc: 扫描Group中所有Manager,调用Manager的注册方法.
procedure TManagerGroup.RegistAll(const nReg: Boolean);
begin
  if not nReg then
  begin
    CallManagersMethod('RunBeforUnregistAllManager', True,
    procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
      const nMethod: TRttiMethod)
    begin
      nMethod.Invoke(nObj, []);
    end);
  end; //卸载前执行

  CallManagersMethod('RegistMe', False,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nInstance.MetaclassType, [TValue.From(nReg)]);
  end); //执行注册

  if nReg then
  begin
    CallManagersMethod('RunAfterRegistAllManager', True,
    procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
      const nMethod: TRttiMethod)
    begin
      nMethod.Invoke(nObj, []);
    end);
  end; //注册后执行
end;

//Date: 2021-04-11
//Parm: 事件
//Desc: 记录管理器内部事件
procedure TManagerGroup.InnerLog(const nEvent: string);
begin
  WriteLog('TManagerGroup', '管理器组', nEvent);
end;

//Date: 2017-04-18
//Parm: 调用类;变量名;管理器变量
//Desc: 当nCallClass需要nManager支持,但nManager为nil,抛出异常.
procedure TManagerGroup.CheckSupport(const nCallClass,nManagerName: string;
  const nManager: TObject);
var nStr: string;
begin
  if not Assigned(nManager) then
  begin
    nStr := '%s Needs TManagerGroup.%s(nil) Support.';
    nStr := Format(nStr, [nCallClass, nManagerName]);

    InnerLog(nStr);
    raise Exception.Create(nStr);
  end;
end;

//Date: 2017-04-17
//Parm: 调用类;所需管理器变量名
//Desc: 检查nCallClsss所需的nManagers是否存在
procedure TManagerGroup.CheckSupport(const nCallClass: string;
  const nManagers: TStringHelper.TStringArray);
var nStr,nBase: string;
    nBool: Boolean;
    nCtx: TRttiContext;
    nType: TRttiType;
    nRF: TRttiField;    
begin
  nCtx := TRttiContext.Create;
  try
    nType := nCtx.GetType(TypeInfo(TManagerGroup));
    for nBase in nManagers do
    begin
      nBool := False;
      //init flag
      
      for nRF in nType.GetFields do
      begin
        nBool := (nBase = nRF.FieldType.Name) or (nBase = sAllManager);
        if not nBool then Continue;
        
        if nRF.FieldType.TypeKind = tkClass then
        begin
          CheckSupport(nCallClass, nRF.Name, nRF.GetValue(@gMG).AsObject);
          if nBase <> sAllManager then
            Break; 
          //match done  
        end else

        if nBase <> sAllManager then        
        begin
          nStr := '%s: Manager "%s" Is Not Valid Class.';
          nStr := Format(nStr, [nCallClass, nBase]);

          InnerLog(nStr);
          raise Exception.Create(nStr);
        end;  
      end;

      if not nBool then
      begin
        nStr := '%s: Manager "%s" Is Not Exists.';
        nStr := Format(nStr, [nCallClass, nBase]);

        InnerLog(nStr);
        raise Exception.Create(nStr);
      end; //not exits
    end;
  finally
    nCtx.Free;
  end;    
end;

//Date: 2019-04-24
//Parm: 列表
//Desc: 获取当前有效的管理器状态信息
procedure TManagerGroup.GetManagersStatus(const nList: TStrings);
begin
  CallManagersMethod('GetStatus', True,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nObj, [TValue.From(nList), TValue.From(True)]);
  end);
end;

//Date: 2019-05-27
//Desc: 主程序启动完成
procedure TManagerGroup.RunAfterApplicationStart;
begin
  CallManagersMethod('RunAfterApplicationStart', True,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nObj, []);
  end);
end;

//Date: 2019-05-27
//Desc: 主程序主备关闭
procedure TManagerGroup.RunBeforApplicationHalt;
begin
  CallManagersMethod('RunBeforApplicationHalt', True,
  procedure (const nObj: TObject; const nInstance: TRttiInstanceType;
    const nMethod: TRttiMethod)
  begin
    nMethod.Invoke(nObj, []);
  end);
end;

//Date: 2021-04-09
//Parm: 日志对象;对象描述;事件
//Desc: 向日志文件中写入nClass.nEvent事件
procedure TManagerGroup.WriteLog(const nClass: TClass; const nDesc,nEvent: string);
begin
  FSimpleLogger.WriteLog(nClass.ClassName, nDesc, nEvent);
end;

//Date: 2021-04-09
//Parm: 日志对象名称;对象描述;事件
//Desc: 向日志文件中写入nClass.nEvent事件
procedure TManagerGroup.WriteLog(const nClass, nDesc, nEvent: string);
begin
  FSimpleLogger.WriteLog(nClass, nDesc, nEvent);
end;

initialization
  FillChar(gMG, SizeOf(TManagerGroup), #0);
  gMG.FSimpleLogger := TSimpleLogger.Create(TApplicationHelper.gLogPath, '_S.log');
  gMG.RegistAll(True);
finalization
  gMG.RegistAll(False);
  gMG.FSimpleLogger.Free;
end.
