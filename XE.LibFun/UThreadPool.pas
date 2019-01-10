{*******************************************************************************
  作者: dmzn@163.com 2019-01-09
  描述: 线程池
*******************************************************************************}
unit UThreadPool;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, UWaitItem, UBaseObject;

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
    FLastCall     : Int64;                       //上次调用
    FStartCall    : Int64;                       //开始调用
    FStartDelete  : Int64;                       //开始删除
  end;

  TThreadRunner = class(TThread)
  private
    FOwner: TThreadPoolManager;
    {*拥有者*}
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TThreadPoolManager);
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
    FWorkers: TList;
    {*工作对象*}
    FMonitor: TThreadMonitor;
    {*守护线程*}
    FRunners: array of TThreadRunner;
    {*运行对象*}
  protected
    procedure StopRunners;
    procedure ClearWorkers(const nFree: Boolean);
    {*清理资源*}
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
  end;

var
  gThreadPoolManager: TThreadPoolManager = nil;
  //全局使用

implementation

uses
  UManagerGroup;

constructor TThreadPoolManager.Create;
begin
  FWorkers := TList.Create;
  SetLength(FRunners, 0);
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
    nPWorker.FWorkerID := gMG.FSerialIDManager.GetID;

    nPWorker.FLastCall := 0;
    nPWorker.FStartCall := 0;
    nPWorker.FStartDelete := 0;
    nPWorker.FWorker := nWorker^;
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

//Date: 2019-01-09
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList
procedure TThreadPoolManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      nList.Add('NumThread=' + Length(FRunners).ToString);
      nList.Add('NumWorker=' + FWorkers.Count.ToString);
      Exit;
    end;

    nList.Add(FixData('NumThread:', Length(FRunners)));
    nList.Add(FixData('NumWorker:', FWorkers.Count));
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

    if (nInt >= 20) and (Result < hlLow) then
    begin
      if Assigned(nList) then
      begin
        nStr := '线程池对象[Runner: %d]过多.';
        nList.Add(Format(nStr, [nInt]));
      end;

      Result := hlLow;
    end;

    if (nInt >= 50) and (Result < hlBad) then
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
  FWaiter.Interval := 1000;
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
    if not Terminated then
      DoMonitor;
    //xxxxx
  except
    //ignor any error
  end;
end;

procedure TThreadMonitor.DoMonitor;
begin

end;

//------------------------------------------------------------------------------
constructor TThreadRunner.Create(AOwner: TThreadPoolManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := AOwner;
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
begin
  inherited;

end;

end.
