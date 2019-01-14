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
*******************************************************************************}
unit UThreadPool;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, UWaitItem, UBaseObject;

const
  cThreadMin              = 1;     //最小线程数
  cThreadMax              = 32;    //最大线程数
  cThreadMaxWorkInterval  = 1000;  //线程最大扫描间隔

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

  TThreadWorkerConfig = record
  private
  const
    cDim = 2;
    {*数组下标*}
  public
    FWorkerName   : string;                      //对象名称
    FParentObj    : TObject;                     //线程调用方
    FParentDesc   : string;                      //调用方描述
    FDataString   : array[0..cDim] of string;    //字符串
    FDataInteger  : array[0..cDim] of Integer;   //整型值
    FDataFloat    : array[0..cDim] of Double;    //浮点数
    FDataPointer  : array[0..cDim] of Pointer;   //数据指针

    FCallTimes    : Cardinal;                    //执行次数
    FCallInterval : Cardinal;                    //执行间隔(毫秒)
    FProcedure    : TThreadProcedure;            //待执行函数
    FProcEvent    : TThreadProcEvent;            //待执行事件
    FProcRefer    : TThreadProcRefer;            //待执行匿名
  end;

  PThreadWorker = ^TThreadWorker;
  TThreadWorker = record
    FWorkerID     : Cardinal;                    //对象标识
    FWorker       : TThreadWorkerConfig;         //工作对象
    FRunner       : Integer;                     //运行对象
    FLastCall     : Cardinal;                    //上次调用
    FStartCall    : Cardinal;                    //开始调用
    FStartDelete  : Cardinal;                    //开始删除
  end;

  TThreadWorkerStatus = record
    FWorkerName   : string;                      //对象名称
    FHistoryValue : Cardinal;                    //历史记录
  end;

  TThreadManagerStatus = record
    FNumWorkers   : Cardinal;                    //工作对象
    FNumRunners   : Cardinal;                    //运行对象
    FNumRunning   : Cardinal;                    //运行中
    FNumWorkerRun : UInt64;                      //调用次数
    FNumWorkerMax : Cardinal;                    //最多工作对象
    FNumRunnerMax : Cardinal;                    //最多运行对象

    FRunDelayNow  : Cardinal;                    //运行延迟
    FRunDelayMax  : Cardinal;                    //运行最大延迟
    FRunMostFast  : TThreadWorkerStatus;         //运行最快纪录
    FRunMostSlow  : TThreadWorkerStatus;         //运行最慢记录
    FMaxWorkInterval: Cardinal;                  //最大扫描间隔
    FMaxWorkIdleLong: Cardinal;                  //空大空闲计时
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
    {*空闲计时*}
  protected
    procedure DoRun;
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
    FRunnerMin: Word;
    FRunnerMax: Word;
    FRunners: array of TThreadRunner;
    {*运行对象*}
  protected
    procedure StopRunners;
    procedure ClearWorkers(const nFree: Boolean);
    {*清理资源*}
    procedure SetRunnerMin(const nValue: Word);
    procedure SetRunnerMax(const nValue: Word);
    {*设置参数*}
    function MaxWorkInterval(const nIleTime: PCardinal = nil;
      const nLock: Boolean = False): Cardinal;
    {*扫描间隔*}
    function ValidWorkerNumber(const nLock: Boolean = False): Cardinal;
    {*有效对象*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; override;
    {*获取状态*}
    procedure WorkerInit(var nWorker: TThreadWorkerConfig);
    function WorkerAdd(const nWorker: PThreadWorkerConfig;
      const nMulti: Boolean = True): Cardinal;
    procedure WorkerDelete(const nWorkerID: Cardinal); overload;
    procedure WorkerDelete(const nParent: TObject); overload;
    {*添加删除*}
    procedure WorkerStart(const nWorkerID: Cardinal;
      const nTimes: Cardinal = INFINITE); overload;
    procedure WorkerStart(const nParent: TObject;
      const nTimes: Cardinal = INFINITE); overload;
    procedure WorkerStop(const nWorkerID: Cardinal); overload;
    procedure WorkerStop(const nParent: TObject); overload;
    {*启动停止*}
    property ThreadMin: Word read FRunnerMin write SetRunnerMin;
    property ThreadMax: Word read FRunnerMax write SetRunnerMax;
  end;

var
  gThreadPoolManager: TThreadPoolManager = nil;
  //全局使用

implementation

uses
  UManagerGroup, ULibFun;

constructor TThreadPoolManager.Create;
begin
  FRunnerMin := cThreadMin;
  FRunnerMax := cThreadMax;

  FWorkerIndex := 0;
  SetLength(FRunners, 0);
  FillChar(FStatus, SizeOf(FStatus), #0);

  FWorkers := TList.Create;
  FMonitor := TThreadMonitor.Create(Self);
end;

destructor TThreadPoolManager.Destroy;
begin
  FMonitor.StopMe;
  FMonitor := nil;
  StopRunners;

  ClearWorkers(True);
  inherited;
end;

procedure TThreadPoolManager.StopRunners;
var nIdx: Integer;
begin
  for nIdx := Low(FRunners) to High(FRunners) do
  if Assigned(FRunners[nIdx]) then
  begin
    FRunners[nIdx].StopMe;
    FRunners[nIdx] := nil;
  end;

  SetLength(FRunners, 0);
end;

procedure TThreadPoolManager.ClearWorkers(const nFree: Boolean);
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  for nIdx := FWorkers.Count-1 downto 0 do
  begin
    nWorker := FWorkers[nIdx];
    Dispose(nWorker);
  end;

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
    if not Assigned(FManagers[nIdx].FManager) then
      FManagers[nIdx].FManager := TThreadPoolManager.Create;
    gMG.FThreadPool := FManagers[nIdx].FManager as TThreadPoolManager;
  end else
  begin
    gMG.FThreadPool := nil;
    FreeAndNil(FManagers[nIdx].FManager);
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

    nPWorker.FWorker := nWorker^;
    nPWorker.FWorkerID := gMG.FSerialIDManager.GetID;
    Inc(FStatus.FNumWorkers);

    if FStatus.FNumWorkers > FStatus.FNumWorkerMax then
      FStatus.FNumWorkerMax := FStatus.FNumWorkers;
    //xxxxx
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
        nWorker.FStartDelete := GetTickCount;
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
    WorkerDelete(nWorkers[nIdx]);
  //xxxxx
end;

//Date: 2019-01-10
//Parm: 对象标识
//Desc: 删除标识为nWorker的Worker
procedure TThreadPoolManager.WorkerDelete(const nWorkerID: Cardinal);
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

        if nWorker.FStartCall = 0 then //未被调用
        begin
          Dispose(nWorker);
          FWorkers.Delete(nIdx);

          if FStatus.FNumWorkers > 0 then
            Dec(FStatus.FNumWorkers);
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
      if nWorker.FWorker.FParentObj = nParent then
        nWorker.FWorker.FCallTimes := nTimes;
      //xxxxx
    end;
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
      begin
        nWorker.FWorker.FCallTimes := nTimes;
        Break;
      end;
    end;
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
        nLen := Length(nWorkers);
        SetLength(nWorkers, nLen + 1);
        nWorkers[nLen] := nWorker.FWorkerID;
      end;
    end;
  finally
    SyncLeave;
  end;

  for nIdx := Low(nWorkers) to High(nWorkers) do
    WorkerStop(nWorkers[nIdx]);
  //xxxxx
end;

//Date: 2019-01-10
//Parm: 对象标识
//Desc: 停止标识为nWorkerID的Worker
procedure TThreadPoolManager.WorkerStop(const nWorkerID: Cardinal);
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

        if nWorker.FStartCall = 0 then //未被调用
        begin
          nWorker.FWorker.FCallTimes := 0;
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

//Date: 2019-01-11
//Desc: 获取有效的工作对象个数
function TThreadPoolManager.ValidWorkerNumber(const nLock: Boolean): Cardinal;
var nIdx: Integer;
    nWorker: PThreadWorker;
begin
  Result := 0;
  if nLock then SyncEnter;
  try
    for nIdx := FWorkers.Count-1 downto 0 do
    begin
      nWorker := FWorkers[nIdx];
      if (nWorker.FStartDelete = 0) and (nWorker.FWorker.FCallTimes > 0) then
        Inc(Result);
      //valid worker
    end;
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
    Result := 0;
    for nIdx := Low(FRunners) to High(FRunners) do
    if Assigned(FRunners[nIdx]) then
    begin
      if FRunners[nIdx].FWorkInterval > Result then
      begin
        Result := FRunners[nIdx].FWorkInterval;
        if Result > FStatus.FMaxWorkInterval then
          FStatus.FMaxWorkInterval := Result;
        //xxxxx
      end;

      if FRunners[nIdx].FWorkIdleStart > 0 then
      begin
        nVal := TDateTimeHelper.GetTickCountDiff(FRunners[nIdx].FWorkIdleStart);
        if nVal > FStatus.FMaxWorkIdleLong then
          FStatus.FMaxWorkIdleLong := nVal;
        //xxxxx

        if Assigned(nIleTime) and (nVal > nIleTime^) then
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
      nList.Add('NumWorkerMax' + FNumWorkerMax.ToString);
      nList.Add('NumWorker=' + FNumWorkers.ToString);
      nList.Add('NumWorkerValid=' + ValidWorkerNumber().ToString);
      nList.Add('NumThreadMax=' + FNumRunnerMax.ToString);
      nList.Add('NumThread=' + FNumRunners.ToString);
      nList.Add('NumRunning=' + FNumRunning.ToString);

      nList.Add('NumWorkerRun=' + FNumWorkerRun.ToString);
      nList.Add('NowRunDelay=' + FRunDelayNow.ToString);
      nList.Add('MaxRunDelay=' + FRunDelayMax.ToString);
      nList.Add('WorkerRunMostFast=' + FRunMostFast.FHistoryValue.ToString);
      nList.Add('WorkerRunMostSlow=' + FRunMostSlow.FHistoryValue.ToString);

      nList.Add('NowWorkInterval=' + nInt.ToString);
      nList.Add('NowWorkIdleLong=' + nVal.ToString);
      nList.Add('MaxWorkInterval=' + FMaxWorkInterval.ToString);
      nList.Add('MaxWorkIdleLong=' + FMaxWorkIdleLong.ToString);
      Exit;
    end;

    nList.Add(FixData('NumWorkerMax:', FStatus.FNumWorkerMax));
    nList.Add(FixData('NumWorker:', FStatus.FNumWorkers));
    nList.Add(FixData('NumWorkerValid:', ValidWorkerNumber()));
    nList.Add(FixData('NumThreadMax:', FStatus.FNumRunnerMax));
    nList.Add(FixData('NumThread:', FStatus.FNumRunners));
    nList.Add(FixData('NumRunning:', FStatus.FNumRunning));

    nList.Add(FixData('NowWorkInterval:', nInt));
    nList.Add(FixData('MaxWorkInterval:', FStatus.FMaxWorkInterval));
    nList.Add(FixData('NowWorkIdleLong:', nVal));
    nList.Add(FixData('MaxWorkIdleLong:', FStatus.FMaxWorkIdleLong));

    nList.Add(FixData('NowRunDelay:', FStatus.FRunDelayNow));
    nList.Add(FixData('MaxRunDelay:', FStatus.FRunDelayMax));
    nList.Add(FixData('NumWorkerRun:', FStatus.FNumWorkerRun));

    with FStatus.FRunMostFast do
      nList.Add(FixData('WorkerRunMostFast:', FHistoryValue.ToString + '(' +
                                        FWorkerName + ')'));
    //xxxxx

    with FStatus.FRunMostSlow do
      nList.Add(FixData('WorkerRunMostSlow:', FHistoryValue.ToString + '(' +
                                        FWorkerName + ')'));
    //xxxxx
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
    nInt := Length(FRunners);

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
    FWaiter.EnterWait;
    if Terminated then Break;

    FOwner.SyncEnter;
    try
      DoMonitor;
    finally
      FOwner.SyncLeave;
    end;
  except
    //ignor any error
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
      for i := Low(FOwner.FRunners) to High(FOwner.FRunners) do
      if not Assigned(FOwner.FRunners[i]) then
      begin
        Result := i;
        Exit;
      end;

      i := Length(FOwner.FRunners);
      SetLength(FOwner.FRunners, i + 1);
      Result := i;
    end;

    //Desc: 停止运行对象
    procedure StopRunner;
    begin
      FOwner.SyncLeave;
      try
        FOwner.FRunners[nIdx].StopMe;
        //wait and stop
      finally
        FOwner.SyncEnter;
      end;

      FOwner.FRunners[nIdx] := nil;
      Dec(FOwner.FStatus.FNumRunners);
    end;
begin
  FOwner.FStatus.FRunDelayNow := 0;
  //调整运行对象,则重新计算延迟

  if nInc then //add
  begin
    nIdx := FOwner.FStatus.FNumRunners + nNum - FOwner.FRunnerMax;
    if nIdx > 0 then
         nInt := nNum - nIdx
    else nInt := nNum;

    while nInt > 0 do
    begin
      nIdx := GetNewRunner();
      FOwner.FRunners[nIdx] := TThreadRunner.Create(FOwner, nIdx);

      Dec(nInt);
      Inc(FOwner.FStatus.FNumRunners);

      if FOwner.FStatus.FNumRunners > FOwner.FStatus.FNumRunnerMax then
        FOwner.FStatus.FNumRunnerMax := FOwner.FStatus.FNumRunners;
      //xxxxx
    end;
  end else //del
  begin
    if (nIndex >= 0) and (nIndex >= Low(FOwner.FRunners)) and
       (nIndex <= High(FOwner.FRunners)) and
       (FOwner.FStatus.FNumRunners > FOwner.FRunnerMin) then //指定删除索引
    begin
      nIdx := nIndex;
      StopRunner;
      Exit;
    end;

    nIdx := FOwner.FRunnerMin - (FOwner.FStatus.FNumRunners - nNum);
    if nIdx > 0 then
         nInt := nNum - nIdx
    else nInt := nNum;

    if nInt < 1 then Exit;
    //invalid adjust

    for nIdx := Low(FOwner.FRunners) to High(FOwner.FRunners) do
    if Assigned(FOwner.FRunners[nIdx]) then
    begin
      StopRunner;
      Dec(nInt);
      if nInt <= 0 then Break;
    end;
  end;
end;

procedure TThreadMonitor.DoMonitor;
var nIdx: Integer;
    nVal: Cardinal;
begin
  nVal := FOwner.ValidWorkerNumber;
  if FOwner.FStatus.FNumRunners > nVal then
  begin
    AdjustRunner(False, FOwner.FStatus.FNumRunners - nVal);
    //至多每个Worker独立使用一个线程
  end;

  if (FOwner.FStatus.FNumRunners < FOwner.FRunnerMin) and (nVal > 0) then
  begin
    AdjustRunner(True, FOwner.FRunnerMin - FOwner.FStatus.FNumRunners);
    //保持最小线程
  end;

  for nIdx := Low(FOwner.FRunners) to High(FOwner.FRunners) do
  if Assigned(FOwner.FRunners[nIdx]) and (
     FOwner.FRunners[nIdx].FWorkIdleStart > 0) then //空闲线程
  begin
    with FOwner.FRunners[nIdx] do
      nVal := TDateTimeHelper.GetTickCountDiff(FWorkIdleStart);
    //xxxxx

    if nVal >= 60 * 1000 then
      AdjustRunner(False, 1, nIdx);
    //空闲超时则关闭
  end;

  if FOwner.FStatus.FRunDelayNow >= 1000 then
  begin
    FOwner.FStatus.FRunDelayNow := 0;
    AdjustRunner(True, 1);
    //运行延迟过大,增加线程
  end;
end;

//------------------------------------------------------------------------------
constructor TThreadRunner.Create(AOwner: TThreadPoolManager; ATag: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FTag := ATag;
  FWorkInterval := 1;
  FWorkIdleStart := 0;
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

  //Desc: 扫描可用工作对象
  procedure ScanActiveWorker;
  begin
    nLoop := False;
    nIdx := FOwner.FWorkerIndex;

    while FOwner.FWorkerIndex < FOwner.FWorkers.Count do
    begin
      if nLoop and (FOwner.FWorkerIndex = nIdx) then Break;
      //新一轮到开始位置,扫描结束
      nWorker := FOwner.FWorkers[FOwner.FWorkerIndex];
      Inc(FOwner.FWorkerIndex);

      if FOwner.FWorkerIndex >= FOwner.FWorkers.Count then
      begin
        nLoop := True;
        FOwner.FWorkerIndex := 0;
      end; //开始新一轮扫描

      if (nWorker.FStartCall > 0) or (nWorker.FStartDelete > 0) or
         (nWorker.FWorker.FCallTimes < 1) then Continue;
      //调用中;删除中;执行完毕
       
      if (nWorker.FWorker.FCallInterval > 0) and (nWorker.FLastCall > 0) then
      begin
        nVal := TDateTimeHelper.GetTickCountDiff(nWorker.FLastCall);
        if nVal < nWorker.FWorker.FCallInterval then Continue;
        //未到执行时间

        nVal := nVal - nWorker.FWorker.FCallInterval;
        if nVal > 0 then //两次调用间隔超过需要的间隔
        begin
          if nVal > FOwner.FStatus.FRunDelayNow then
            FOwner.FStatus.FRunDelayNow := nVal;
          //当前最大延迟

          if nVal > FOwner.FStatus.FRunDelayMax then
            FOwner.FStatus.FRunDelayMax := nVal;
          //历史最大延迟
        end;
      end;
         
      FActiveWorker := nWorker;
      Break;
    end;

    if Assigned(FActiveWorker) then
    begin
      if FWorkIdleStart > 0 then
      begin
        nVal := TDateTimeHelper.GetTickCountDiff(FWorkIdleStart);
        if nVal > FOwner.FStatus.FMaxWorkIdleLong then
          FOwner.FStatus.FMaxWorkIdleLong := nVal;
        //最长空闲等待
      end;

      FWorkInterval := 1;
      FWorkIdleStart := 0;

      FActiveWorker.FRunner := FTag;
      FActiveWorker.FStartCall := GetTickCount;
      Inc(FOwner.FStatus.FNumRunning);
    end else
    begin
      if FWorkInterval = 1 then
        FWorkIdleStart := GetTickCount();
      //开始空闲计时

      if FWorkInterval < cThreadMaxWorkInterval then
      begin
        Inc(FWorkInterval);
        //空闲时增加等待

        if FWorkInterval > FOwner.FStatus.FMaxWorkInterval then
          FOwner.FStatus.FMaxWorkInterval := FWorkInterval;
        //最长等待间隔
      end;
    end;
  end;
begin
  while not Terminated do
  try
    nInit := 0;
    FActiveWorker := nil;
    try
      FOwner.SyncEnter;
      try
        ScanActiveWorker();
      finally
        FOwner.SyncLeave;
      end;

      if not Assigned(FActiveWorker) then
      begin
        Sleep(FWorkInterval);
        Continue;
      end;

      DoRun();
      nInit := TDateTimeHelper.GetTickCountDiff(FActiveWorker.FStartCall);

      if nInit < FWorkInterval then
        Sleep(FWorkInterval - nInit);
      //wait seconds
    finally
      if Assigned(FActiveWorker) then
      try
        FOwner.SyncEnter;
        FActiveWorker.FRunner := 0;
        FActiveWorker.FStartCall := 0;
        FActiveWorker.FLastCall := GetTickCount;

        if (FActiveWorker.FWorker.FCallTimes < INFINITE) and
           (FActiveWorker.FWorker.FCallTimes > 0) then
          Dec(FActiveWorker.FWorker.FCallTimes);
        //call times

        Inc(FOwner.FStatus.FNumWorkerRun);
        Dec(FOwner.FStatus.FNumRunning);

        if nInit > 0 then
        begin
          with FOwner.FStatus.FRunMostFast do
          begin
            if (nInit < FHistoryValue) or (FHistoryValue < 1) then //最快纪录
            begin
              FHistoryValue := nInit;
              FWorkerName := FActiveWorker.FWorker.FWorkerName;
            end;
          end;

          with FOwner.FStatus.FRunMostSlow do
          begin
            if nInit > FHistoryValue then //最慢纪录
            begin
              FHistoryValue := nInit;
              FWorkerName := FActiveWorker.FWorker.FWorkerName;
            end;
          end;
        end;
      finally
        FOwner.SyncLeave;
      end;
    end;
  except
    //ignor any error
  end;
end;

procedure TThreadRunner.DoRun;
begin
  if Assigned(FActiveWorker.FWorker.FProcedure) then
    FActiveWorker.FWorker.FProcedure(@FActiveWorker.FWorker, Self);
  //xxxxx

  if Assigned(FActiveWorker.FWorker.FProcEvent) then
    FActiveWorker.FWorker.FProcEvent(@FActiveWorker.FWorker, Self);
  //xxxxx

  if Assigned(FActiveWorker.FWorker.FProcRefer) then
    FActiveWorker.FWorker.FProcRefer(@FActiveWorker.FWorker, Self);
  //xxxxx
end;

end.
