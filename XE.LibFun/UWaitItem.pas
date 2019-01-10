{*******************************************************************************
  作者: dmzn@163.com 2017-04-16
  描述: 实现等待对象和高性能等待计数器

  描述:
  &.TWaitObject在EnterWait后进入阻塞,直到Wakeup唤醒.
  &.该对象多线程安全,即A线程EnterWait,B线程Wakeup.
  &.TWaitTimer实现微秒级间隔计数.
*******************************************************************************}
unit UWaitItem;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows;

type
  TWaitObject = class(TObject)
  strict private
    const
      cIsIdle    = $02;
      cIsWaiting = $27;
    {*常量定义*}
  private     
    FEvent: THandle;
    {*等待事件*}
    FInterval: Cardinal;
    {*等待间隔*}
    FStatus: Integer;
    {*等待状态*}
    FWaitResult: Cardinal;
    {*等待结果*}
  public
    constructor Create(nEventName: string = '');
    destructor Destroy; override;
    {*创建释放*}
    procedure InitStatus(const nWakeup: Boolean);
    {*初始状态*}
    function EnterWait: Cardinal;
    procedure Wakeup(const nForce: Boolean = False);
    {*等待.唤醒*}
    function IsWaiting: Boolean;
    function IsTimeout: Boolean;
    function IsWakeup: Boolean;
    {*等待状态*}
    property WaitResult: Cardinal read FWaitResult;
    property Interval: Cardinal read FInterval write FInterval;
  end;

  TCrossProcWaitObject = class(TObject)
  private
    FEvent: THandle;
    {*同步事件*}
    FLockStatus: Boolean;
    {*锁定状态*}
  public
    constructor Create(nEventName: string = '');
    destructor Destroy; override;
    {*创建释放*}
    function SyncLockEnter(const nWaitFor: Boolean = False): Boolean;
    procedure SyncLockLeave(const nOnlyMe: Boolean = True);
    {*同步操作*}
  end;

  TWaitTimer = class(TObject)
  strict private
    type
      PTimerItem = ^TTimerItem;
      TTimerItem = record
        FThread: THandle;    //线程句柄
        FLastActive: UInt64; //上次活动
      end;
  private
    FFrequency: Int64;
    {*CPU频率*}
    FFlagFirst: Int64;
    {*起始标记*}
    FTimeResult: Int64;
    {*计时结果*}
  public
    constructor Create;
    class procedure ManageTimer; static;
    {*创建释放*}
    procedure StartTime;
    class procedure StartHighResolutionTimer; static;
    {*开始计时*}    
    function EndTime: Int64;
    class function GetHighResolutionTimerResult: Int64; static;
    {*结束计时*}
    property TimeResult: Int64 read FTimeResult;
    {*属性相关*}
  end;

implementation

uses
  UManagerGroup, ULibFun;

constructor TWaitObject.Create(nEventName: string);
begin
  inherited Create;
  FStatus := cIsIdle;
  FInterval := INFINITE;

  if Trim(nEventName) = '' then
    nEventName := 'evt_waitobj_' + TDateTimeHelper.DateTimeSerial;
  FEvent := CreateEvent(nil, False, False, PChar(nEventName));

  if FEvent = 0 then
    raise Exception.Create('Create TCrossProcWaitObject.FEvent Failure');
  //xxxxx
end;

destructor TWaitObject.Destroy;
begin
  if FEvent > 0 then
    CloseHandle(FEvent);
  inherited;
end;

function TWaitObject.IsWaiting: Boolean;
begin
  Result := FStatus = cIsWaiting;
end;

function TWaitObject.IsTimeout: Boolean;
begin
  if IsWaiting then
       Result := False
  else Result := FWaitResult = WAIT_TIMEOUT;
end;

function TWaitObject.IsWakeup: Boolean;
begin
  if IsWaiting then
       Result := False
  else Result := FWaitResult = WAIT_OBJECT_0;
end;

procedure TWaitObject.InitStatus(const nWakeup: Boolean);
begin
  if nWakeup then
       SetEvent(FEvent)
  else ResetEvent(FEvent);
end;

function TWaitObject.EnterWait: Cardinal;
begin
  InterlockedExchange(FStatus, cIsWaiting);
  Result := WaitForSingleObject(FEvent, FInterval);

  FWaitResult := Result;
  InterlockedExchange(FStatus, cIsIdle);
end;

procedure TWaitObject.Wakeup(const nForce: Boolean);
begin
  if (FStatus = cIsWaiting) or nForce then
    SetEvent(FEvent);
  //xxxxx
end;

//------------------------------------------------------------------------------
constructor TCrossProcWaitObject.Create(nEventName: string);
var nStr: string;
begin
  inherited Create;
  FLockStatus := False;

  if Trim(nEventName) = '' then
    nEventName := 'evt_crosswait_' + TDateTimeHelper.DateTimeSerial;
  FEvent := CreateEvent(nil, False, True, PChar(nEventName));

  if FEvent = 0 then
  begin
    nStr := 'Create TCrossProcWaitObject.FSyncEvent Failure.';
    if GetLastError = ERROR_INVALID_HANDLE then
    begin
      nStr := nStr + #13#10#13#10 +
              'The name of an existing semaphore,mutex,or file-mapping object.';
      //xxxxx
    end;

    raise Exception.Create(nStr);
  end;
end;

destructor TCrossProcWaitObject.Destroy;
begin
  SyncLockLeave(True);
  //unlock
  
  if FEvent > 0 then
    CloseHandle(FEvent);
  inherited;
end;

//Date: 2017-04-16
//Parm: 等待信号
//Desc: 锁定同步信号量,锁定成功返回True
function TCrossProcWaitObject.SyncLockEnter(const nWaitFor: Boolean): Boolean;
begin
  if nWaitFor then
       Result := WaitForSingleObject(FEvent, INFINITE) = WAIT_OBJECT_0
  else Result := WaitForSingleObject(FEvent, 0) = WAIT_OBJECT_0;
  {*
    a.FEvent初始状态为True.
    b.首次WaitFor返回WAIT_OBJECT_0,锁定成功.
    c.FEvent复位方式为False,WaitFor调用成功后自动置为"无信号".
    d.其它WaitFor都会返回WAIT_TIMEOUT,锁定失败.
    e.LockRelease后,解锁.
  *}

  FLockStatus := Result;
  {*是否本对象锁定*}
end;

//Date: 2017-04-16
//Parm: 只解锁本对象锁定信号
//Desc: 解锁同步信号
procedure TCrossProcWaitObject.SyncLockLeave(const nOnlyMe: Boolean);
begin
  if (not nOnlyMe) or FLockStatus then
    SetEvent(FEvent);
  //set event signal
end;

//------------------------------------------------------------------------------
constructor TWaitTimer.Create;
begin
  FTimeResult := 0;
  if not QueryPerformanceFrequency(FFrequency) then
    raise Exception.Create('not support high-resolution performance counter');
  //xxxxx
end;

procedure TWaitTimer.StartTime;
begin
  QueryPerformanceCounter(FFlagFirst);
end;

function TWaitTimer.EndTime: Int64;
var nNow: Int64;
begin
  QueryPerformanceCounter(nNow);
  Result := Trunc((nNow - FFlagFirst) / FFrequency * 1000 * 1000);
  FTimeResult := Result;
end;

//Date: 2017-04-17
//Desc: 注册计时器对象
class procedure TWaitTimer.ManageTimer;
var nItem: PTimerItem;
begin
  gMG.CheckSupport('TWaitTimer', ['TObjectPoolManager']);
  //检查依赖

  gMG.FObjectPool.NewClass(TWaitTimer,
    function(var nData: Pointer):TObject
    begin
      Result := TWaitTimer.Create;
      New(nItem);
      nData := nItem;

      nItem.FThread := 0;
      nItem.FLastActive := 0;
    end,

    procedure(const nObj: TObject; const nData: Pointer)
    begin
      TWaitTimer(nObj).Free;
      Dispose(PTimerItem(nData));
    end);
  //xxxxx
end;

//Date: 2017-04-17
//Desc: 开始一个计数
class procedure TWaitTimer.StartHighResolutionTimer;
var nCurID: THandle;
    nItem: PTimerItem;
    nTimer: TWaitTimer;
begin
  nTimer := nil;
  try
    nCurID := GetCurrentThreadId;
    nTimer := gMG.FObjectPool.Lock(TWaitTimer, nil, @nItem,
      function(const nObj: TObject; const nData: Pointer;
       var nTimes: Integer): Boolean
      begin
        nItem := nData;
        if nTimes = 1 then //首轮扫描
        begin
          Result := (not Assigned(nItem)) or (nItem.FThread = nCurID);
          //相同线程
        end else
        begin
          Result := (not Assigned(nItem)) or ((nItem.FThread = 0) or
                    (GetTickCount - nItem.FLastActive > 60 * 60 * 1000));
          //空闲对象
        end;

        if nTimes = 1 then
          nTimes := 2;
        //扫描2轮
      end) as TWaitTimer;
    //xxxxx

    nTimer.StartTime;
    nItem.FThread := nCurID;
    nItem.FLastActive := GetTickCount64;
  finally
    gMG.FObjectPool.Release(nTimer);
  end;
end;

//Date: 2017-04-17
//Desc: 返回计数结果
class function TWaitTimer.GetHighResolutionTimerResult: Int64;
var nCurID: THandle;
    nItem: PTimerItem;
    nTimer: TWaitTimer;
begin
  nTimer := nil;
  try
    Result := 0;
    nCurID := GetCurrentThreadId;

    nTimer := gMG.FObjectPool.Lock(TWaitTimer, nil, @nItem,
      function(const nObj: TObject; const nData: Pointer;
       var nTimes: Integer): Boolean
      begin
        nItem := nData;
        Result := (not Assigned(nItem)) or (nItem.FThread = nCurID);
      end, True) as TWaitTimer;
    //xxxxx

    if Assigned(nTimer) then
    begin
      Result := nTimer.EndTime;
      nItem.FThread := 0;
      nItem.FLastActive := 0;
    end;
  finally
    gMG.FObjectPool.Release(nTimer);
  end;
end;

initialization
  TWaitTimer.ManageTimer;
finalization
  //nothing
end.
