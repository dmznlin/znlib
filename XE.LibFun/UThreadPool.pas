{*******************************************************************************
  作者: dmzn@163.com 2019-01-09
  描述: 线程池管理器

  备注:
  *.使用方法:
    var nWorker: TThreadWorkerConfig;
    //0.声明工作对象
    begin
      WorkerInit(nWorker);
      //1.初始化

      with nWorker do
      begin
        FWorkerName := '高速计时器';
        FParentObj := Self;
        FParentDesc := '主窗口';
        FDataInteger[0] := nIdx;
        FCallInterval := 100;

        //4.工作对象支持3种线程函数,以FProc开头,运行在线程中
        FProcRefer := procedure (const nConfig: PThreadWorkerConfig;
         const nThread: TThread)
        begin
          TWaitTimer.StartHighResolutionTimer;
          Sleep(100);
          nInt := TWaitTimer.GetHighResolutionTimerResult;

          TThread.Synchronize(nThread, procedure
          begin
            case nConfig.FDataInteger[0] of
             0: Form1.Edit1.Text := IntToStr(nInt);
             1: Form1.Edit2.Text := IntToStr(nInt);
             2: Form1.Edit3.Text := IntToStr(nInt);
            end;
          end);
        end;
      end; //2.填写工作对象信息

      WorkerAdd(@nWorker);
      //3.投递工作对象
    end;
    ----------------------------------------------------------------------------
  *.Worker的休眠状态: 如果工作对象是如TCPClient之类阻塞对象业务,在网络异常时
    会执行时间超长,当多个超时业务同时存在,会拖慢线程池的执行效率.而在网络异常时
    高频率的网络通讯已经没有意义.

    设置Worker休眠有两种方式:
      1.TThreadWorkerConfig.FAutoStatus = True: 自动切换休眠状态
        当执行时长超过 TThreadWorkerConfig.FCallMaxTake 视为Worker已休眠.
      2.在Worker的线程函数中设置,如:
        procedure Do(const nConfig: PThreadWorkerConfig; const nThread: TThread);
        begin
          nConfig.FWorkStatus := twsSleep;
          //FAutoStatus = False时有效
        end;

    当Worker被设置为休眠(twsSleep)时,空闲(twsNormal未到执行时间)的线程会执行
    休眠Worker的业务,且线程数不超过 TThreadPoolManager.NumRunSleep,确保有足够的
    线程执行正常业务.
    注意:若TThreadPoolManager.NumRunSleep = 0,则休眠的Worker不会执行.
*******************************************************************************}
unit UThreadPool;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, {$IFDEF MSWin}Winapi.ActiveX,{$ENDIF}
  {$IFDEF POSIX}Posix.Pthread,{$ENDIF}UWaitItem, UBaseObject;

const
  cThreadMin              = 1;                   //最小线程数
  cThreadMax              = 32;                  //最大线程数
  cThreadMaxRunTake       = 10 * 1000;           //默认运行最长耗时
  cThreadMaxWorkInterval  = 1000;                //线程最大扫描间隔

type
  TThreadPoolManager = class;
  PThreadWorkerConfig = ^TThreadWorkerConfig;

  TThreadProcedure = procedure (const nConfig: PThreadWorkerConfig;
    const nThread: TThread);
  TThreadProcEvent = procedure (const nConfig: PThreadWorkerConfig;
    const nThread: TThread) of object;
  TThreadProcRefer = reference to procedure (const nConfig: PThreadWorkerConfig;
    const nThread: TThread);
  {*线程内运行函数*}

  TThreadWorkerEvent = record
    WorkProc  : TThreadProcedure;                //标准函数
    WorkEvent : TThreadProcEvent;                //对象事件
    WorkRefer : TThreadProcRefer;                //匿名函数
  end;

  TThreadWorkerEventName = (twnInit, twnWork, twnStop, twnFree);
  //工作对象事件名称: 初始化,运行中,运行完毕,释放对象

  TThreadWorkerStatus = (twsNormal, twsSleep, twsNull);
  TThreadWorkerStatuses = set of TThreadWorkerStatus;
  {*****************************************************************************
    工作对象状态: 正常,休眠
    休眠状态是指: Worker的业务暂时不正常,可以降低到忽略的程序.
    例如: 1.TCP通讯时网络异常,引起Worker代码执行时间超长.
          2.磁盘空间不足,引起Worker写操作异常.
    休眠状态的Worker过多,会拖慢线程池的执行效率,因此需要特殊处理.
  *****************************************************************************}

  TThreadWorkerConfig = record
  private
  const
    cDim = 2;
    {*数组下标*}
  public
    FWorkerName   : string;                      //对象名称
    FParentObj    : TObject;                     //线程调用方
    FParentDesc   : string;                      //调用方描述
    FDataStr      : array[0..cDim] of string;    //字符串
    FDataInt      : array[0..cDim] of Integer;   //整型值
    FDataFlt      : array[0..cDim] of Double;    //浮点数
    FDataPtr      : array[0..cDim] of Pointer;   //数据指针
    FDataObj      : array[0..cDim] of TObject;   //对象变量

    FCallTimes    : Cardinal;                    //执行次数
    FCallInterval : Cardinal;                    //执行间隔(毫秒)
    FCallMaxTake  : Cardinal;                    //运行耗时(毫秒)
    FFirstInit    : Boolean;                     //执行初始化
    FOnInit       : TThreadWorkerEvent;          //初始化(首次运行)
    FOnWork       : TThreadWorkerEvent;          //业务
    FOnStop       : TThreadWorkerEvent;          //停止
    FOnFree       : TThreadWorkerEvent;          //释放

    FCoInitialize : Boolean;                     //执行COM初始化
    FAutoStatus   : Boolean;                     //自动切换状态
    FWorkStatus   : TThreadWorkerStatus;         //当前工作状态
  end;

  PThreadWorker = ^TThreadWorker;
  TThreadWorker = record
    FWorkerID     : Cardinal;                    //对象标识
    FWorker       : TThreadWorkerConfig;         //工作对象
    FRunner       : Integer;                     //运行对象
    FLastCall     : Cardinal;                    //上次调用
    FStartCall    : Cardinal;                    //开始调用
    FStartDelete  : Cardinal;                    //开始删除
    FWakeupCall   : Cardinal;                    //唤醒计数
  end;

  TThreadNV = record
    FName         : string;                      //对象名称
    FValue        : Cardinal;                    //有效值
    FMemo         : string;                      //描述信息
  end;

  TThreadManagerStatus = record
    FNumWorkers      : Cardinal;                 //工作对象
    FNumWorkerValid  : Cardinal;                 //有效对象
    FNumWorkerSleep  : Cardinal;                 //休眠对象
    FNumRunners      : Cardinal;                 //运行对象
    FNumRunning      : Cardinal;                 //运行中
    FNumRunSleep     : Cardinal;                 //处理休眠线程
    FNumWorkerRun    : UInt64;                   //调用次数
    FNumWorkerMax    : Cardinal;                 //最多工作对象
    FNumRunnerMax    : Cardinal;                 //最多运行对象

    FRunDelayNow     : Cardinal;                 //运行延迟
    FRunDelayMax     : Cardinal;                 //运行最大延迟
    FRunMostFast     : TThreadNV;                //运行最快纪录
    FRunMostSlow     : TThreadNV;                //运行最慢记录
    FRunErrorIndex   : Integer;                  //运行错误索引
    FRunErrors       : array[0..9] of TThreadNV; //运行错误记录
    FMaxWorkInterval : Cardinal;                 //最大扫描间隔
    FMaxWorkIdleLong : Cardinal;                 //空大空闲计时
    FWorkIdleInit    : Cardinal;                 //空闲计时开始
    FWorkIdleCounter : Cardinal;                 //空闲时长累积
  end;

  TThreadRunner = class(TThread)
  private
    FOwner: TThreadPoolManager;
    {*拥有者*}
    FTag: Integer;
    {*线程标识*}
    FActiveWorker: PThreadWorker;
    {*当前运行*}
    FWorkInterval: Cardinal;
    FWorkIdleStart: Cardinal;
    FWorkIdleLast: Cardinal;
    {*空闲计时*}
    FCoInitialize: Boolean;
    {*COM初始化*}
  protected
    procedure Execute; override;
    {*执行业务*}
  public
    constructor Create(AOwner: TThreadPoolManager; ATag: Integer);
    destructor Destroy; override;
    {*创建释放*}
    procedure StopMe;
    {*停止线程*}
  end;

  TThreadMonitor = class(TThread)
  private
    FOwner: TThreadPoolManager;
    {*拥有者*}
    FWaiter: TWaitObject;
    {*等待对象*}
    FLastRunDelayVal: Cardinal;
    FLastRunDelayInc: Cardinal;
    {*状态标记*}
  protected
    procedure DoMonitor;
    procedure Execute; override;
    {*执行守护*}
    procedure AdjustRunner(const nInc: Boolean; const nNum: Word = 1;
      const nIndex: Integer = -1);
    {*调整对象*}
  public
    constructor Create(AOwner: TThreadPoolManager);
    destructor Destroy; override;
    {*创建释放*}
    procedure Wakeup;
    {*唤醒线程*}
    procedure StopMe;
    {*停止线程*}
  end;

  TThreadPoolManager = class(TManagerBase)
  private
    FStatus: TThreadManagerStatus;
    {*工作状态*}
    FWorkerIndex: Integer;
    FWorkers: TList;
    {*工作对象*}
    FMonitor: TThreadMonitor;
    {*守护线程*}
    FRunners: TList;
    FRunnerMin: Word;
    FRunnerMax: Word;
    {*运行对象*}
    FMaxRunSleep: Word;
    FWorkerIndexSleep: Integer;
    {*休眠对象*}
  protected
    procedure StopRunners;
    procedure DeleteWorker(const nIdx: Integer);
    procedure ClearWorkers(const nFree: Boolean);
    {*清理资源*}
    function RunWorkerEvent(const nWorker: PThreadWorker;
      const nEventName: TThreadWorkerEventName;
      const nThread: TThread = nil): Boolean; inline;
    {*执行业务*}
    procedure IncErrorCounter(const nName: string; const nLock: Boolean = True;
      const nMemo: string = '');
    {*错误计数*}
    procedure SetRunnerMin(const nValue: Word);
    procedure SetRunnerMax(const nValue: Word);
    {*设置参数*}
    function MaxWorkInterval(const nIleTime: PCardinal = nil;
      const nLock: Boolean = False): Cardinal;
    {*扫描间隔*}
    function ValidWorkerNumber(const nLock: Boolean = False): Cardinal;
    {*有效对象*}
    function SleepWorkerNumber(const nLock: Boolean = False): Cardinal;
    {*休眠对象*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    procedure RunBeforUnregistAllManager; override;
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; override;
    {*获取状态*}
    procedure WorkerInit(var nWorker: TThreadWorkerConfig);
    function WorkerAdd(const nWorker: PThreadWorkerConfig;
      const nMulti: Boolean = True): Cardinal;
    procedure WorkerDelete(const nWorkerID: Cardinal;
      const nUpdateValid: Boolean = True); overload;
    procedure WorkerDelete(const nParent: TObject); overload;
    {*添加删除*}
    procedure WorkerStart(const nWorkerID: Cardinal;
      const nTimes: Cardinal = INFINITE); overload;
    procedure WorkerStart(const nParent: TObject;
      const nTimes: Cardinal = INFINITE); overload;
    procedure WorkerStop(const nWorkerID: Cardinal;
      const nUpdateValid: Boolean = True); overload;
    procedure WorkerStop(const nParent: TObject); overload;
    {*启动停止*}
    procedure WorkerWakeup(const nWorkerID: Cardinal); overload;
    procedure WorkerWakeup(const nParent: TObject); overload;
    {*唤醒对象*}
    procedure WriteLog(const nEvent: string);
    {*记录日志*}
    property ThreadMin: Word read FRunnerMin write SetRunnerMin;
    property ThreadMax: Word read FRunnerMax write SetRunnerMax;
    property NumRunSleep: Word read FMaxRunSleep write FMaxRunSleep;
    {*属性相关*}
  end;

const
  cThreadWorkerEventNames: array[TThreadWorkerEventName] of string = ('OnInit',
    'OnWork', 'OnStop', 'OnFree');
  //工作对象事件名称描述

var
  gThreadPoolManager: TThreadPoolManager = nil;
  //全局使用

implementation

uses
  UManagerGroup, UMgrLog, ULibFun;

var
  gSimpleLogger: TSimpleLogger = nil;
  //日志记录器

procedure TThreadPoolManager.WriteLog(const nEvent: string);
var nWriter: TLogWriter;
begin
  with nWriter do
  begin
    FDesc := '线程管理器';
    FOjbect := TThreadPoolManager;
  end;

  gSimpleLogger.WriteLog(nWriter, nEvent);
end;

constructor TThreadPoolManager.Create;
begin
  inherited;
  FRunnerMin := cThreadMin;
  FRunnerMax := cThreadMax;

  FWorkerIndex := 0;
  FWorkerIndexSleep := 0;
  FMaxRunSleep := 1;

  FillChar(FStatus, SizeOf(FStatus), #0);
  FStatus.FWorkIdleInit := TDateTimeHelper.GetTickCount();
  FStatus.FRunErrorIndex := Low(FStatus.FRunErrors);

  gSimpleLogger := TSimpleLogger.Create(TApplicationHelper.gLogPath, '_TM.log');
  //enable logger

  FRunners := TList.Create;
  FWorkers := TList.Create;
  FMonitor := TThreadMonitor.Create(Self);
end;

destructor TThreadPoolManager.Destroy;
begin
  if Assigned(FMonitor) then
  begin
    FMonitor.StopMe;
    FMonitor := nil;
  end;

  StopRunners;
  ClearWorkers(True);
  FRunners.Free;

  FreeAndNil(gSimpleLogger);
  inherited;
end;

procedure TThreadPoolManager.StopRunners;
var nIdx: Integer;
begin
  for nIdx := FRunners.Count - 1 downto 0 do
   if Assigned(FRunners[nIdx]) then
    TThreadRunner(FRunners[nIdx]).Terminate;
  //set stop flag

  for nIdx := FRunners.Count - 1 downto 0 do
  if Assigned(FRunners[nIdx]) then
  begin
    TThreadRunner(FRunners[nIdx]).StopMe;
    FRunners[nIdx] := nil;
  end;

  FRunners.Clear;
end;

procedure TThreadPoolManager.DeleteWorker(const nIdx: Integer);
begin
  RunWorkerEvent(FWorkers[nIdx], twnFree);
  Dispose(PThreadWorker(FWorkers[nIdx]));
  FWorkers.Delete(nIdx);

  if FStatus.FNumWorkers > 0 then
    Dec(FStatus.FNumWorkers);
  //xxxxx
end;

procedure TThreadPoolManager.ClearWorkers(const nFree: Boolean);
var nIdx: Integer;
begin
  for nIdx := FWorkers.Count-1 downto 0 do
    DeleteWorker(nIdx);
  //xxxxx

  if nFree then
       FreeAndNil(FWorkers)
  else FWorkers.Clear;
end;

//Date: 2019-01-09
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TThreadPoolManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TThreadPoolManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TThreadPoolManager.Create;
    gMG.FThreadPool := gMG.FManagers[nIdx].FManager as TThreadPoolManager;
  end else
  begin
    gMG.FThreadPool := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gThreadPoolManager := gMG.FThreadPool;
  //启用全局变量
end;

//Date: 2021-01-15
//Desc: 停止线程池
procedure TThreadPoolManager.RunBeforUnregistAllManager;
begin
  if Assigned(FMonitor) then
  begin
    FMonitor.StopMe;
    FMonitor := nil;
  end;

  StopRunners();
end;

//Date: 2021-01-26
//Parm: 工作对象;事件名称;执行线程
//Desc: 在nThread中执行nWorker的nEvent事件
function TThreadPoolManager.RunWorkerEvent(const nWorker: PThreadWorker;
  const nEventName: TThreadWorkerEventName; const nThread: TThread): Boolean;
var nEvent: TThreadWorkerEvent;
begin
  Result := True;
  try
    case nEventName of
     twnInit: nEvent := nWorker.FWorker.FOnInit;
     twnWork: nEvent := nWorker.FWorker.FOnWork;
     twnStop: nEvent := nWorker.FWorker.FOnStop;
     twnFree: nEvent := nWorker.FWorker.FOnFree;
    end;

    if Assigned(nEvent.WorkProc) then
      nEvent.WorkProc(@nWorker.FWorker, nThread);
    //xxxxx

    if Assigned(nEvent.WorkEvent) then
      nEvent.WorkEvent(@nWorker.FWorker, nThread);
    //xxxxx

    if Assigned(nEvent.WorkRefer) then
      nEvent.WorkRefer(@nWorker.FWorker, nThread);
    //xxxxx
  except
    on nErr: Exception do
    begin
      WriteLog(Format('[ %s.%s ]%s', [nWorker.FWorker.FWorkerName,
        cThreadWorkerEventNames[nEventName], nErr.Message]));
      Result := False;
    end;
  end;
end;

procedure TThreadPoolManager.SetRunnerMax(const nValue: Word);
begin
  if (nValue <> FRunnerMax) and (nValue <= cThreadMax) then
  begin
    SyncEnter;
    FRunnerMax := nValue;
    SyncLeave;

    FMonitor.Wakeup;
    //性能平衡
  end;
end;

procedure TThreadPoolManager.SetRunnerMin(const nValue: Word);
begin
  if (nValue <> FRunnerMin) and (nValue >= cThreadMin) then
  begin
    SyncEnter;
    FRunnerMin := nValue;
    SyncLeave;

    FMonitor.Wakeup;
    //性能平衡
  end;
end;

//Date: 2019-01-09
//Parm: 配置项
//Desc: 初始化nConfig
procedure TThreadPoolManager.WorkerInit(var nWorker: TThreadWorkerConfig);
var nInit: TThreadWorkerConfig;
begin
  FillChar(nInit, SizeOf(nInit), #0);
  nWorker := nInit;

  with nWorker do
  begin
    FCallTimes := INFINITE;
    //不限次数
    FCallMaxTake := cThreadMaxRunTake;
    //最大运行耗时
    FFirstInit := True;
    //首次运行时初始化
    FCoInitialize := False;
    //默认不执行COM初始化
    FAutoStatus := False;
    //不自动切换工作状态
    FWorkStatus := twsNormal;
    //默认状态: 正常运行
  end;
end;

//Date: 2019-01-09
//Parm: 工作对象;允许多对象
//Desc: 添加nWorker
function TThreadPoolManager.WorkerAdd(const nWorker: PThreadWorkerConfig;
  const nMulti: Boolean): Cardinal;
var nIdx: Integer;
    nPWorker: PThreadWorker;
begin
  Result := 0;
  gMG.CheckSupport('TThreadPoolManager', ['TSerialIDManager']);

  SyncEnter;
  try
    if not nMulti then
    begin
      for nIdx := FWorkers.Count-1 downto 0 do
      begin
        nPWorker := FWorkers[nIdx];
        if nPWorker.FWorker.FParentObj = nWorker.FParentObj then Exit;
        //调用方相同
      end;
    end;

    New(nPWorker);
    FWorkers.Add(nPWorker);
    FillChar(nPWorker^, SizeOf(TThreadWorker), #0);

    nPWorker.FRunner := -1;
    nPWorker.FWorker := nWorker^;
    nPWorker.FWorkerID := gMG.FSerialIDManager.GetID;

    nPWorker.FLastCall := TDateTimeHelper.GetTickCount - nWorker.FCallInterval;
    //确保立刻执行,并计算出FStatus.FRunDelayNow,延迟过大时增加线程

    if nWorker.FWorkStatus = twsSleep then
      SleepWorkerNumber(False);
    //xxxxx

    Inc(FStatus.FNumWorkers);
    if FStatus.FNumWorkers > FStatus.FNumWorkerMax then
      FStatus.FNumWorkerMax := FStatus.FNumWorkers;
    ValidWorkerNumber(False);
  finally
    SyncLeave;
  end;

  FMonitor.Wakeup;
  //守护线程平衡性能
end;

//Date: 2019-01-10
//Parm: 调用方
//Desc: 删除nParent的所有Worker
procedure TThreadPoolManager.WorkerDelete(const nParent: TObject);
var nIdx,nLen: Integer;
    nWorker: PThreadWorker;
    nWorkers: array of Cardinal;
begin
  SyncEnter;
  try
    SetLength(nWorkers, 0);
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if nWorker.FWorker.FParentObj = nParent then
      begin
        nWorker.FStartDelete := TDateTimeHelper.GetTickCount;
        //删除标记

        nLen := Length(nWorkers);
        SetLength(nWorkers, nLen + 1);
        nWorkers[nLen] := nWorker.FWorkerID;
      end;
    end;
  finally
    SyncLeave;
  end;

  for nIdx := Low(nWorkers) to High(nWorkers) do
    WorkerDelete(nWorkers[nIdx], False);
  ValidWorkerNumber(True);

  SleepWorkerNumber(True);
  //recount sleep worker
end;

//Date: 2019-01-10
//Parm: 对象标识;更新有效Worker
//Desc: 删除标识为nWorker的Worker
procedure TThreadPoolManager.WorkerDelete(const nWorkerID: Cardinal;
 const nUpdateValid: Boolean);
var nIdx: Integer;
    nExists,nInMain: Boolean;
    nWorker: PThreadWorker;
begin
  while True do
  begin
    SyncEnter;
    try
      nExists := False;
      for nIdx := FWorkers.Count-1 downto 0 do
      begin
        nWorker := FWorkers[nIdx];
        if nWorker.FWorkerID <> nWorkerID then Continue;

        if nWorker.FStartDelete < 1 then
        begin
          nWorker.FStartDelete := TDateTimeHelper.GetTickCount;
          //删除标记

          if nUpdateValid then
          begin
            ValidWorkerNumber(False);
            SleepWorkerNumber(False);
          end;
        end;

        {删除时需要等待调用结束:
          1.若删除操作由主线程发起,则主线程处于锁定状态.
          2.若Worker在线程中执行时,使用Synchronize同步VCL操作.
          3.则以上两步操作会死锁,所以主线程调用时延迟清理.}
        nInMain := TThread.Current.ThreadID = MainThreadID;
        if (nWorker.FStartCall = 0) or nInMain then
        begin
          if (nWorker.FStartCall = 0) or (not nInMain) then
            DeleteWorker(nIdx); //主线程调用时延迟清理
          Exit;
        end;

        nExists := True;
        Break;
      end;
    finally
      SyncLeave;
    end;

    if nExists then
         Sleep(10) //调用中等待
    else Exit;
  end;
end;

//Date: 2019-01-10
//Parm: 调用方;调用次数
//Desc: 启动nParent的所有Worker
procedure TThreadPoolManager.WorkerStart(const nParent: TObject;
  const nTimes: Cardinal);
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  SyncEnter;
  try
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      with nWorker.FWorker do
       if FParentObj = nParent then
       begin
         FCallTimes := nTimes;
         nWorker.FLastCall := TDateTimeHelper.GetTickCount - FCallInterval;
       end;
    end;

    ValidWorkerNumber(False);
  finally
    SyncLeave;
  end;

  FMonitor.Wakeup;
  //守护线程平衡性能
end;

//Date: 2019-01-10
//Parm: 对象标识;调用次数
//Desc: 启动标识为nWorkerID的Worker
procedure TThreadPoolManager.WorkerStart(const nWorkerID, nTimes: Cardinal);
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  SyncEnter;
  try
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if nWorker.FWorkerID = nWorkerID then
       with nWorker.FWorker do
       begin
         FCallTimes := nTimes;
         nWorker.FLastCall := TDateTimeHelper.GetTickCount - FCallInterval;
         Break;
       end;
    end;

    ValidWorkerNumber(False);
  finally
    SyncLeave;
  end;

  FMonitor.Wakeup;
  //守护线程平衡性能
end;

//Date: 2019-01-10
//Parm: 调用方
//Desc: 停止nParent的所有Worker
procedure TThreadPoolManager.WorkerStop(const nParent: TObject);
var nIdx,nLen: Integer;
    nWorker: PThreadWorker;
    nWorkers: array of Cardinal;
begin
  SyncEnter;
  try
    SetLength(nWorkers, 0);
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if nWorker.FWorker.FParentObj = nParent then
      begin
        nWorker.FWorker.FCallTimes := 0;
        //停止标记

        nLen := Length(nWorkers);
        SetLength(nWorkers, nLen + 1);
        nWorkers[nLen] := nWorker.FWorkerID;
      end;
    end;
  finally
    SyncLeave;
  end;

  for nIdx := Low(nWorkers) to High(nWorkers) do
    WorkerStop(nWorkers[nIdx], False);
  ValidWorkerNumber(True);
end;

//Date: 2019-01-10
//Parm: 对象标识;更新有效Worker
//Desc: 停止标识为nWorkerID的Worker
procedure TThreadPoolManager.WorkerStop(const nWorkerID: Cardinal;
 const nUpdateValid: Boolean);
var nIdx: Integer;
    nExists: Boolean;
    nWorker: PThreadWorker;
begin
  while True do
  begin
    SyncEnter;
    try
      nExists := False;
      for nIdx := FWorkers.Count-1 downto 0 do
      begin
        nWorker := FWorkers[nIdx];
        if nWorker.FWorkerID <> nWorkerID then Continue;

        if nWorker.FWorker.FCallTimes > 0 then
        begin
          nWorker.FWorker.FCallTimes := 0;
          //停止标记

          if nUpdateValid then
            ValidWorkerNumber(False);
          //xxxxx
        end;

        {停止时需要等待调用结束:
          1.若停止操作由主线程发起,则主线程处于锁定状态.
          2.若Worker在线程中执行时,使用Synchronize同步VCL操作.
          3.则以上两步操作会死锁,所以主线程调用时不等待.}
        if (nWorker.FStartCall = 0) or                     //未被调用
           (TThread.Current.ThreadID = MainThreadID) then  //主线程调用
        begin
          if nWorker.FStartCall = 0 then
            RunWorkerEvent(nWorker, twnStop);
          Exit;
        end;

        nExists := True;
        Break;
      end;
    finally
      SyncLeave;
    end;

    if nExists then
         Sleep(10) //调用中等待
    else Exit;
  end;
end;

//Date: 2019-01-24
//Parm: 调用方
//Desc: 取消nParent所有Worker的等待时间,立即执行
procedure TThreadPoolManager.WorkerWakeup(const nParent: TObject);
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  SyncEnter;
  try
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if nWorker.FWorker.FParentObj = nParent then
      begin
        nWorker.FWakeupCall := TDateTimeHelper.GetTickCount();
        //唤醒
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-24
//Parm: 对象标识
//Desc: 取消nWorkerID的等待时间,立即执行
procedure TThreadPoolManager.WorkerWakeup(const nWorkerID: Cardinal);
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  SyncEnter;
  try
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if nWorker.FWorkerID = nWorkerID then
      begin
        nWorker.FWakeupCall := TDateTimeHelper.GetTickCount(); //唤醒
        Break;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-15
//Parm: 调用方;是否锁定
//Desc: 累加运行错误计数
procedure TThreadPoolManager.IncErrorCounter(const nName: string;
  const nLock: Boolean; const nMemo: string);
begin
  if nLock then SyncEnter;
  try
    with FStatus.FRunErrors[FStatus.FRunErrorIndex] do
    begin
      FName := nName;
      FMemo := nMemo;
      FValue := TDateTimeHelper.GetTickCount();
    end;

    Inc(FStatus.FRunErrorIndex);
    if FStatus.FRunErrorIndex > High(FStatus.FRunErrors) then
      FStatus.FRunErrorIndex := Low(FStatus.FRunErrors);
    //xxxxx
  finally
    if nLock then SyncLeave;
  end;
end;

//Date: 2019-01-11
//Desc: 获取有效的工作对象个数
function TThreadPoolManager.ValidWorkerNumber(const nLock: Boolean): Cardinal;
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  if nLock then SyncEnter;
  try
    Result := 0;
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if (nWorker.FStartDelete < 1) and (nWorker.FWorker.FCallTimes > 0) then
        Inc(Result);
      //valid worker
    end;

    FStatus.FNumWorkerValid := Result;
  finally
    if nLock then SyncLeave;
  end;
end;

//Date: 2021-01-21
//Desc: 获取已休眠的工作对象个数
function TThreadPoolManager.SleepWorkerNumber(const nLock: Boolean): Cardinal;
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  if nLock then SyncEnter;
  try
    Result := 0;
    for nIdx := FWorkers.Count - 1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if (nWorker.FStartDelete < 1) and (nWorker.FWorker.FWorkStatus = twsSleep) then
        Inc(Result);
      //no delete, sleep worker
    end;

    FStatus.FNumWorkerSleep := Result;
  finally
    if nLock then SyncLeave;
  end;
end;

//Date: 2019-01-14
//Parm: 空闲时长;是否锁定
//Desc: 工作线程的最大扫描间隔
function TThreadPoolManager.MaxWorkInterval(const nIleTime: PCardinal;
  const nLock: Boolean): Cardinal;
var nIdx: Integer;
    nVal: Cardinal;
begin
  if nLock then SyncEnter;
  try
    if Assigned(nIleTime) then
      nIleTime^ := 0;
    Result := 0;

    for nIdx := FRunners.Count - 1 downto 0 do
     if Assigned(FRunners[nIdx]) then
      with TThreadRunner(FRunners[nIdx]) do
      begin
        if FWorkInterval > Result then
        begin
          Result := FWorkInterval;
        end;

        if Assigned(nIleTime) and (FWorkIdleStart > 0) then
        begin
          nVal := TDateTimeHelper.GetTickCountDiff(FWorkIdleStart);
          if nVal > nIleTime^ then
            nIleTime^ := nVal;
          //xxxxx
        end;
      end;
  finally
    if nLock then SyncLeave;
  end;
end;

//Date: 2019-01-09
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList
procedure TThreadPoolManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nInt: Integer;
    nVal: Cardinal;
begin
  with TObjectStatusHelper,FStatus do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);
    nInt := MaxWorkInterval(@nVal);

    if not nFriendly then
    begin
      nList.Add('NumWorkerMax=' + FNumWorkerMax.ToString);
      nList.Add('NumWorker=' + FNumWorkers.ToString);
      nList.Add('NumWorkerValid=' + FNumWorkerValid.ToString);
      nList.Add('NumWorkerSleep=' + FNumWorkerSleep.ToString);
      nList.Add('NumThreadMax=' + FNumRunnerMax.ToString);
      nList.Add('NumThread=' + FNumRunners.ToString);
      nList.Add('NumRunning=' + FNumRunning.ToString);
      nList.Add('NumRunSleep=' + FNumRunSleep.ToString);

      nList.Add('NumWorkerRun=' + FNumWorkerRun.ToString);
      nList.Add('NowRunDelay=' + FRunDelayNow.ToString);
      nList.Add('MaxRunDelay=' + FRunDelayMax.ToString);
      nList.Add('WorkerRunMostFast=' + FRunMostFast.FValue.ToString);
      nList.Add('WorkerRunMostSlow=' + FRunMostSlow.FValue.ToString);

      nList.Add('NowWorkInterval=' + nInt.ToString);
      nList.Add('NowWorkIdleLong=' + nVal.ToString);
      nList.Add('MaxWorkInterval=' + FMaxWorkInterval.ToString);
      nList.Add('MaxWorkIdleLong=' + FMaxWorkIdleLong.ToString);
      Exit;
    end;

    nList.Add(FixData('NumWorkerMax:', FNumWorkerMax));
    nList.Add(FixData('NumWorker:', FNumWorkers));
    nList.Add(FixData('NumWorkerValid:', FNumWorkerValid));
    nList.Add(FixData('NumWorkerSleep:', FNumWorkerSleep));
    nList.Add(FixData('NumThreadMax:', FNumRunnerMax));
    nList.Add(FixData('NumThread:', FNumRunners));
    nList.Add(FixData('NumRunning:', FNumRunning));
    nList.Add(FixData('NumRunSleep:', Format('%d/%d', [FNumRunSleep, FMaxRunSleep])));

    nList.Add(FixData('NowWorkInterval:', nInt));
    nList.Add(FixData('MaxWorkInterval:', FMaxWorkInterval));
    nList.Add(FixData('NowWorkIdleLong:', nVal));
    nList.Add(FixData('MaxWorkIdleLong:', FMaxWorkIdleLong));
    nList.Add(FixData('WorkIdleCounter:', FWorkIdleCounter));

    nList.Add(FixData('NowRunDelay:', FRunDelayNow));
    nList.Add(FixData('MaxRunDelay:', FRunDelayMax));
    nList.Add(FixData('NumWorkerRun:', FNumWorkerRun));

    with FStatus.FRunMostFast do
     nList.Add(FixData('WorkerRunMostFast:', FValue.ToString + '(' + FName + ')'));
    //xxxxx

    with FStatus.FRunMostSlow do
     nList.Add(FixData('WorkerRunMostSlow:', FValue.ToString + '(' + FName + ')'));
    //xxxxx

    for nInt := 0 to FWorkers.Count - 1 do
    with PThreadWorker(FWorkers[nInt]).FWorker,TStringHelper do
    begin
       nList.Add(FixData(Format('Worker Item %d:', [nInt + 1]),
        Format('[%s]%s', [StrIF(['R', 'S'], FCallTimes > 0), FWorkerName])));
      //xxxxx
    end;

    for nInt := Low(FStatus.FRunErrors) to High(FStatus.FRunErrors) do
    with FStatus.FRunErrors[nInt] do
    begin
      if FValue < 1 then Continue;
      nList.Add(FixData(Format('Thread RunError %d:', [nInt + 1]),
        Format('%s: %s', [FName, FMemo])));
      //xxxxx
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-09
//Desc: 获取管理器健康度
function TThreadPoolManager.GetHealth(const nList: TStrings): TObjectHealth;
var nStr: string;
    nInt: Integer;
begin
  SyncEnter;
  try
    Result := hlNormal;
    nInt := FRunners.Count;

    if (nInt >= cThreadMax * 0.8) and (Result < hlLow) then
    begin
      if Assigned(nList) then
      begin
        nStr := '线程池对象[Runner: %d]过多.';
        nList.Add(Format(nStr, [nInt]));
      end;

      Result := hlLow;
    end;

    if (nInt >= cThreadMax) and (Result < hlBad) then
    begin
      if Assigned(nList) then
      begin
        nStr := '线程池对象[Runner: %d]达到警戒值.';
        nList.Add(Format(nStr, [nInt]));
      end;

      Result := hlBad;
    end;
  finally
    SyncLeave;
  end;
end;

//------------------------------------------------------------------------------
constructor TThreadMonitor.Create(AOwner: TThreadPoolManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FLastRunDelayVal := 0;
  FLastRunDelayInc := 0;

  FWaiter := TWaitObject.Create();
  FWaiter.Interval := 10 * 1000;
end;

destructor TThreadMonitor.Destroy;
begin
  FreeAndNil(FWaiter);
  inherited;
end;

procedure TThreadMonitor.Wakeup;
begin
  FWaiter.Wakeup();
end;

procedure TThreadMonitor.StopMe;
begin
  Terminate;
  FWaiter.Wakeup();

  WaitFor;
  Free;
end;

procedure TThreadMonitor.Execute;
begin
  while not Terminated do
  try
    FOwner.SyncEnter;
    try
      DoMonitor;
    finally
      FOwner.SyncLeave;
    end;

    if Terminated then Exit;
    FWaiter.EnterWait;
  except
    on nErr: Exception do
    begin
      FOwner.IncErrorCounter('TThreadMonitor', True, nErr.Message);
      //ignor any error
    end;
  end;
end;

//Date: 2019-01-11
//Parm: 增减;调整数量;指定索引
//Desc: 增加或减少运行对象
procedure TThreadMonitor.AdjustRunner(const nInc: Boolean; const nNum: Word;
  const nIndex: Integer);
var nIdx,nInt: Integer;

    //Desc: 新对象索引
    function GetNewRunner: Integer;
    var i: Integer;
    begin
      for i := FOwner.FRunners.Count - 1 downto 0 do
      if not Assigned(FOwner.FRunners[i]) then
      begin
        Result := i;
        FOwner.FRunners[i] := TThreadRunner.Create(FOwner, Result);
        Exit;
      end;

      Result := FOwner.FRunners.Count;
      FOwner.FRunners.Add(TThreadRunner.Create(FOwner, Result));
      //new runner
    end;

    //Desc: 停止运行对象
    procedure StopRunner;
    begin
      FOwner.SyncLeave;
      try
        TThreadRunner(FOwner.FRunners[nIdx]).StopMe;
        //wait and stop
      finally
        FOwner.SyncEnter;
      end;

      FOwner.FRunners[nIdx] := nil;
      Dec(FOwner.FStatus.FNumRunners);
    end;
begin
  with FOwner.FStatus do
  begin
    FRunDelayNow := 0;
    //重新计算延迟
    FWorkIdleInit := TDateTimeHelper.GetTickCount();
    FWorkIdleCounter := 0;
    //重新计算空闲时长计数
  end;

  if nInc then //add
  begin
    if FOwner.FRunnerMax < FOwner.FStatus.FNumWorkerValid  then
         nIdx := FOwner.FRunnerMax
    else nIdx := FOwner.FStatus.FNumWorkerValid;

    nIdx := FOwner.FStatus.FNumRunners + nNum - Word(nIdx);
    if nIdx > 0 then
         nInt := nNum - nIdx
    else nInt := nNum;

    while nInt > 0 do
    begin
      nIdx := GetNewRunner();
      Dec(nInt);
      Inc(FOwner.FStatus.FNumRunners);

      if FOwner.FStatus.FNumRunners > FOwner.FStatus.FNumRunnerMax then
        FOwner.FStatus.FNumRunnerMax := FOwner.FStatus.FNumRunners;
      //xxxxx

      if FOwner.FStatus.FNumRunners >= FOwner.FRunnerMax then
        FOwner.WriteLog(Format('Threads Number Max(%d)', [FOwner.FStatus.FNumRunners]));
      //xxxxx
    end;
  end else //del
  begin
    if nIndex >= 0 then //指定删除索引
    begin
      if (nIndex >= 0) and (nIndex < FOwner.FRunners.Count) and
         (FOwner.FStatus.FNumRunners > FOwner.FRunnerMin) then
      begin      
        nIdx := nIndex;
        StopRunner;
      end;
      
      Exit;
    end;

    nIdx := FOwner.FRunnerMin - (FOwner.FStatus.FNumRunners - nNum);
    if nIdx > 0 then
         nInt := nNum - nIdx
    else nInt := nNum;

    if nInt < 1 then Exit;
    //invalid adjust

    for nIdx := FOwner.FRunners.Count - 1 downto 0 do
    if Assigned(FOwner.FRunners[nIdx]) then
    begin
      StopRunner;
      Dec(nInt);
      if nInt <= 0 then Break;
    end;

    if FOwner.FStatus.FNumRunners <=  FOwner.FRunnerMin then
      FOwner.WriteLog(Format('Threads Number Min(%d)', [FOwner.FStatus.FNumRunners]));
    //xxxxx
  end;
end;

procedure TThreadMonitor.DoMonitor;
var nIdx: Integer;
    nVal: Cardinal;
    nWorker: PThreadWorker;
begin
  with FOwner.FStatus do
  begin
    if FNumRunners > FNumWorkerValid then
    begin
      AdjustRunner(False, FNumRunners - FNumWorkerValid);
      //至多每个Worker独立使用一个线程
    end;

    if (FNumRunners < FOwner.FRunnerMin) and (FNumWorkerValid > 0) then
    begin
      AdjustRunner(True, FOwner.FRunnerMin - FNumRunners);
      //保持最小线程
    end;
  end;

  with FOwner.FStatus,TDateTimeHelper do
  begin
    nVal := GetTickCountDiff(FWorkIdleInit, TTickDefault.tdZero);
    nVal := Trunc(nVal / (60 * 1000)); //分钟数

    if nVal >= 5 then
    begin
      nVal := Trunc(FWorkIdleCounter / (nVal * FNumRunners * 1000));
      if nVal > 30 then
      begin
        AdjustRunner(False, 1);
        //每分钟有30秒空闲
        FOwner.WriteLog(Format('Threads Idle %d Sec,Dec %d Thread', [nVal, 1]));
      end else
      begin
        FWorkIdleInit := TDateTimeHelper.GetTickCount();
        FWorkIdleCounter := 0;
        //重新计时
      end;
    end;
  end;

  for nIdx := FOwner.FWorkers.Count-1 downto 0 do
  begin
    nWorker := FOwner.FWorkers[nIdx];
    if nWorker.FStartCall > 0 then
    begin
      nVal := TDateTimeHelper.GetTickCountDiff(nWorker.FStartCall);
      if (nWorker.FWorker.FCallMaxTake > 0) and
         (nWorker.FWorker.FCallMaxTake <= nVal) then
      begin
        FOwner.IncErrorCounter(nWorker.FWorker.FWorkerName, False, '长时间挂起');
        //Worker长时间挂起,视为异常
        FOwner.WriteLog(Format('[ %s ]Worker Run Timeout %d MS', [nWorker.FWorker.FWorkerName, nVal]));
      end;
    end;

    if (nWorker.FStartDelete > 0) and (nWorker.FStartCall < 1) then
    begin
      FOwner.DeleteWorker(nIdx);
      //清理在主线程调用中被延迟的Worker
    end;
  end;

  with FOwner.FStatus do
  begin
    if (FRunDelayNow >= 1000) and (FNumRunners < FNumWorkerValid) and (
       (FRunDelayNow <> FLastRunDelayVal) or (TDateTimeHelper.
        GetTickCountDiff(FLastRunDelayInc) >= FRunDelayNow)) then
    begin
      FLastRunDelayVal := FRunDelayNow;
      FLastRunDelayInc := TDateTimeHelper.GetTickCount();
      nVal := Trunc(FRunDelayNow / 2000);

      if nVal < 1 then 
        nVal := 1;
      //xxxxx

      FOwner.WriteLog(Format('Run Delay %d MS,Inc %d Thread(s)', [FRunDelayNow, nVal]));
      //log event
      AdjustRunner(True, nVal);
      //运行延迟过大,增加线程
    end;
  end;
end;

//------------------------------------------------------------------------------
constructor TThreadRunner.Create(AOwner: TThreadPoolManager; ATag: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FTag := ATag;
  FCoInitialize := False;

  FWorkInterval := 1;
  FWorkIdleStart := 0;
  FWorkIdleLast := 0;
end;

destructor TThreadRunner.Destroy;
begin

  inherited;
end;

procedure TThreadRunner.StopMe;
begin
  Terminate;
  WaitFor;
  Free;
end;

procedure TThreadRunner.Execute;
var nIdx: Integer;
    nLoop: Boolean;
    nInit,nVal: Cardinal;
    nWorker: PThreadWorker;
    nWorkStatus: TThreadWorkerStatuses;

  //Parm: 索引变量;Worker状态
  //Desc: 使用nWorkerIndex扫描状态为nStatus的工作对象
  procedure ScanActiveWorker(var nWorkerIndex: Integer;
    const nStatus: TThreadWorkerStatuses);
  begin
    nLoop := False;
    if nWorkerIndex >= FOwner.FWorkers.Count then
      nWorkerIndex := 0;
    nIdx := nWorkerIndex;

    while nWorkerIndex < FOwner.FWorkers.Count do
    begin
      if nLoop and (FOwner.FWorkerIndex = nIdx) then Break;
      //新一轮到开始位置,扫描结束
      nWorker := FOwner.FWorkers[nWorkerIndex];
      Inc(nWorkerIndex);

      if nWorkerIndex >= FOwner.FWorkers.Count then
      begin
        nLoop := True;
        nWorkerIndex := 0;
      end; //开始新一轮扫描

      if not (nWorker.FWorker.FWorkStatus in nStatus) then Continue;
      //工作状态不匹配

      if (nWorker.FStartCall > 0) or (nWorker.FStartDelete > 0) or
         (nWorker.FWorker.FCallTimes < 1) then Continue;
      //调用中;删除中;执行完毕

      if (nWorker.FWorker.FCallInterval > 0) and (nWorker.FLastCall > 0) then
      begin
        if nWorker.FWakeupCall > 0 then
        begin
          nVal := TDateTimeHelper.GetTickCountDiff(nWorker.FWakeupCall);
          if nVal > 1000 then
            nWorker.FWakeupCall := 0;
          //被唤醒1秒内可重复执行
        end;

        nVal := TDateTimeHelper.GetTickCountDiff(nWorker.FLastCall);
        if (nVal < nWorker.FWorker.FCallInterval) and
           (nWorker.FWakeupCall < 1) then Continue;
        //未到执行时间或未唤醒

        if (nVal > nWorker.FWorker.FCallInterval) and     //调用超时
           (nWorker.FWorker.FWorkStatus <> twsSleep) then //休眠时延迟无意义
        begin
          nVal := nVal - nWorker.FWorker.FCallInterval;
          //两次调用间隔超过需要的间隔

          if nVal > FOwner.FStatus.FRunDelayNow then
            FOwner.FStatus.FRunDelayNow := nVal;
          //当前最大延迟

          if nVal > FOwner.FStatus.FRunDelayMax then
            FOwner.FStatus.FRunDelayMax := nVal;
          //历史最大延迟
        end;
      end;

      nWorkStatus := [nWorker.FWorker.FWorkStatus]; //原始工作状态
      FActiveWorker := nWorker;
      Break;
    end;

    if Assigned(FActiveWorker) then
    begin
      FWorkInterval := 1;
      FWorkIdleStart := 0;
      FWorkIdleLast := 0;

      FActiveWorker.FRunner := FTag;
      FActiveWorker.FStartCall := TDateTimeHelper.GetTickCount;
      Inc(FOwner.FStatus.FNumRunning);
    end else
    begin
      if FWorkIdleStart = 0 then
      begin
        FWorkIdleStart := TDateTimeHelper.GetTickCount();
        //开始空闲计时
      end else

      if FOwner.FStatus.FNumWorkerValid > 0 then      
      begin
        nVal := TDateTimeHelper.GetTickCountDiff(FWorkIdleStart);
        if nVal > FOwner.FStatus.FMaxWorkIdleLong then
          FOwner.FStatus.FMaxWorkIdleLong := nVal;
        //最长空闲等待

        Inc(FOwner.FStatus.FWorkIdleCounter, nVal - FWorkIdleLast);
        //累计空闲时长
        FWorkIdleLast := nVal;
      end;

      if FWorkInterval < cThreadMaxWorkInterval then
      begin
        Inc(FWorkInterval);
        //空闲时增加等待

        if (FWorkInterval > FOwner.FStatus.FMaxWorkInterval) and
           (FOwner.FStatus.FNumWorkerValid > 0) then
          FOwner.FStatus.FMaxWorkInterval := FWorkInterval;
        //最长等待间隔
      end;
    end;
  end;

  //Desc: 运行后清理
  procedure DoAfterRun();
  var nNewWS: TThreadWorkerStatus;
  begin
    FActiveWorker.FRunner := -1;
    FActiveWorker.FStartCall := 0;
    FActiveWorker.FLastCall := TDateTimeHelper.GetTickCount;

    if FOwner.FStatus.FNumRunning > 0 then
      Dec(FOwner.FStatus.FNumRunning);
    Inc(FOwner.FStatus.FNumWorkerRun);
    //counter

    if (FActiveWorker.FWorker.FCallTimes < INFINITE) and
       (FActiveWorker.FWorker.FCallTimes > 0) then
    begin
      Dec(FActiveWorker.FWorker.FCallTimes);
      //call times
      if FActiveWorker.FWorker.FCallTimes < 1 then
        FOwner.ValidWorkerNumber(False)
      //update valid
    end;

    if FActiveWorker.FWorker.FCallTimes < 1 then
      FOwner.RunWorkerEvent(FActiveWorker, twnStop, Self);
    //worker stop

    if nInit > 0 then
    begin
      with FActiveWorker.FWorker do
      if FAutoStatus and (FCallMaxTake > 0) then //自动状态,超时时长
      begin
        if nInit >= FCallMaxTake then //执行超时
             nNewWS := twsSleep
        else nNewWS := twsNormal;

        if not (nNewWS in nWorkStatus) then
        begin
          FWorkStatus := nNewWS;
          FOwner.SleepWorkerNumber(False);
          //status changed,recount
        end;
      end;

      with FOwner.FStatus.FRunMostFast do
      begin
        if (nInit < FValue) or (FValue < 1) then //最快纪录
        begin
          FValue := nInit;
          FName := FActiveWorker.FWorker.FWorkerName;
        end;
      end;

      with FOwner.FStatus.FRunMostSlow do
      begin
        if nInit > FValue then //最慢纪录
        begin
          FValue := nInit;
          FName := FActiveWorker.FWorker.FWorkerName;
        end;
      end;
    end;
  end;
begin
  while not Terminated do
  try
    nInit := 0;
    nWorkStatus := [];
    //init default

    FActiveWorker := nil;
    try
      FOwner.SyncEnter;
      try
        if (FOwner.FStatus.FNumRunSleep > 0) or  //已有线程处理休眠业务
           (FOwner.FStatus.FNumRunners < 2) then //当前线程池只有一个线程
        begin
          ScanActiveWorker(FOwner.FWorkerIndex, [twsNormal]);
          //①.优先扫描正常工作对象

          if not Assigned(FActiveWorker) then
            nWorkStatus := [twsNull];
          //未扫描到工作对象
        end;

        if (FOwner.FStatus.FNumRunSleep < FOwner.FMaxRunSleep) and //未达上限
           (FOwner.FStatus.FNumWorkerSleep > 0) and                //有休眠对象
           (not Assigned(FActiveWorker)) then                      //①未成功
        begin
          ScanActiveWorker(FOwner.FWorkerIndexSleep, [twsSleep]);
          //②.处理休眠Worker的线程,优先扫描休眠Worker

          if Assigned(FActiveWorker) then
          begin
            Inc(FOwner.FStatus.FNumRunSleep);
            //增加计数,限制处理休眠Worker的线程个数
            nWorkStatus := [twsSleep];
          end;
        end;

        if nWorkStatus = [] then //①未执行,②未成功
          ScanActiveWorker(FOwner.FWorkerIndex, [twsNormal]);
        //扫描正常工作对象
      finally
        FOwner.SyncLeave;
      end;

      if not Assigned(FActiveWorker) then
      begin
        Sleep(FWorkInterval);
        Continue;
      end;

      {$IFDEF MSWin}
      if FActiveWorker.FWorker.FCoInitialize and (not FCoInitialize) then
      begin
        FCoInitialize := CoInitialize(nil) = S_OK;
        //COM init first

        if not FCoInitialize then
          CoUninitialize();
        //配对解除初始化调用
      end;
      {$ENDIF}

      if FActiveWorker.FWorker.FFirstInit then //执行初始化
      begin
        if not FOwner.RunWorkerEvent(FActiveWorker, twnInit, Self) then
          Continue;
        FActiveWorker.FWorker.FFirstInit := False;
      end;

      FOwner.RunWorkerEvent(FActiveWorker, twnWork, Self);
      //运行业务
      nInit := TDateTimeHelper.GetTickCountDiff(FActiveWorker.FStartCall);
      //运行计时
    finally
      if Assigned(FActiveWorker) then
      try
        FOwner.SyncEnter;
        if (twsSleep in nWorkStatus) then
          Dec(FOwner.FStatus.FNumRunSleep);
        DoAfterRun();
      finally
        FOwner.SyncLeave;
      end;
    end;
  except
    on nErr: Exception do
    begin
      FOwner.IncErrorCounter('TThreadRunner', True, nErr.Message);
      //ignor any error
    end;
  end;

  {$IFDEF MSWin}
  if FCoInitialize then
    CoUninitialize();
  //xxxxx
  {$ENDIF}
end;

end.
