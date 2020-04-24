{*******************************************************************************
  作者: dmzn@163.com 2017-04-16
  描述: 实现等待对象和高性能等待计数器

  描述:
  &.TWaitObject在EnterWait后进入阻塞,直到Wakeup唤醒.
  &.该对象多线程安全,即A线程EnterWait,B线程Wakeup.
  &.TWaitTimer实现微秒级间隔计数.
  &.TWaitTimer多线程调用时,自动为每个线程分配计数对象;同线程多次调用时,自动匹配
    计数对象.
  &.TWaitTimer同线程调用逻辑:
    TWaitTimer.StartHighResolutionTimer;      //1
    sleep(1);
    TWaitTimer.StartHighResolutionTimer;      //2
    sleep(2);
    TWaitTimer.StartHighResolutionTimer;      //3
    Sleep(3);
    TWaitTimer.GetHighResolutionTimerResult;  //与3配对
    TWaitTimer.GetHighResolutionTimerResult;  //与2配对
    TWaitTimer.GetHighResolutionTimerResult;  //与1配对
*******************************************************************************}
unit UWaitItem;

{$I LibFun.inc}
interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Diagnostics,
  {$IFDEF MSWin}Winapi.Windows,{$ENDIF}{$IFDEF POSIX}Posix.Pthread,{$ENDIF}
  UBaseObject, ULibFun;

type
  TWaitObject = class(TObject)
  strict private
    const
      cIsIdle    = $02;
      cIsWaiting = $27;
    {*常量定义*}
  private     
    FEvent: TSimpleEvent;
    {*等待事件*}
    FInterval: Cardinal;
    {*等待间隔*}
    FStatus: Integer;
    {*等待状态*}
    FWaitResult: TWaitResult;
    {*等待结果*}
  public
    constructor Create(nEventName: string = '');
    destructor Destroy; override;
    {*创建释放*}
    procedure InitStatus(const nWakeup: Boolean);
    {*初始状态*}
    function EnterWait: TWaitResult;
    procedure Wakeup(const nForce: Boolean = False);
    {*等待.唤醒*}
    function IsWaiting: Boolean;
    function IsTimeout: Boolean;
    function IsWakeup: Boolean;
    {*等待状态*}
    property WaitResult: TWaitResult read FWaitResult;
    property Interval: Cardinal read FInterval write FInterval;
  end;

  TCrossProcWaitObject = class(TObject)
  private
    FEvent: TSimpleEvent;
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
        FThread    : THandle;   //线程句柄
        FLastActive: Cardinal;  //上次活动
        FSerialID  : TSerialID; //序列编号
      end;
  class var
    FHasRegist: Boolean;
    {*注册标记*}
  private
    FTimer: TStopwatch;
    {*计时器*}
    FTimeResult: Int64;
    {*计时结果*}
  public
    constructor Create;
    {*创建释放*}
    class procedure ManageTimer(const nInit: Boolean); static;
    {*注册对象*}
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
  UManagerGroup;

constructor TWaitObject.Create(nEventName: string);
begin
  inherited Create;
  FStatus := cIsIdle;
  FInterval := INFINITE;

  {$IFDEF MSWin}
  if Trim(nEventName) = '' then
    nEventName := 'evt_waitobj_' + TDateTimeHelper.DateTimeSerial;
  //xxxxx
  {$ELSE}
  nEventName := '';
  {$ENDIF}

  FEvent := TSimpleEvent.Create(nil, False, False, nEventName);
  {$IFDEF MSWin}
  if FEvent.Handle = 0 then
    raise Exception.Create('Create TCrossProcWaitObject.FEvent Failure');
  //xxxxx
  {$ENDIF}
end;

destructor TWaitObject.Destroy;
begin
  FreeAndNil(FEvent);
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
  else Result := FWaitResult = wrTimeout;
end;

function TWaitObject.IsWakeup: Boolean;
begin
  if IsWaiting then
       Result := False
  else Result := FWaitResult = wrSignaled;
end;

procedure TWaitObject.InitStatus(const nWakeup: Boolean);
begin
  if nWakeup then
       FEvent.SetEvent
  else FEvent.ResetEvent;
end;

function TWaitObject.EnterWait: TWaitResult;
begin
  {$IFDEF MSWin}
  InterlockedExchange(FStatus, cIsWaiting);
  Result := FEvent.WaitFor(FInterval);

  FWaitResult := Result;
  InterlockedExchange(FStatus, cIsIdle);
  {$ELSE}
  FStatus := cIsWaiting;
  Result := FEvent.WaitFor(FInterval);

  FWaitResult := Result;
  FStatus := cIsIdle;
  {$ENDIF}
end;

procedure TWaitObject.Wakeup(const nForce: Boolean);
begin
  if (FStatus = cIsWaiting) or nForce then
    FEvent.SetEvent;
  //xxxxx
end;

//------------------------------------------------------------------------------
constructor TCrossProcWaitObject.Create(nEventName: string);
{$IFDEF MSWin}
var nStr: string;
{$ENDIF}
begin
  inherited Create;
  FLockStatus := False;

  {$IFDEF MSWin}
  if Trim(nEventName) = '' then
    nEventName := 'evt_crosswait_' + TDateTimeHelper.DateTimeSerial;
  //xxxxx
  {$ELSE}
  nEventName := '';
  {$ENDIF}

  FEvent := TSimpleEvent.Create(nil, False, True, nEventName);
  {$IFDEF MSWin}
  if FEvent.Handle = 0 then
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
  {$ENDIF}
end;

destructor TCrossProcWaitObject.Destroy;
begin
  SyncLockLeave(True);
  //unlock
  
  FreeAndNil(FEvent);
  inherited;
end;

//Date: 2017-04-16
//Parm: 等待信号
//Desc: 锁定同步信号量,锁定成功返回True
function TCrossProcWaitObject.SyncLockEnter(const nWaitFor: Boolean): Boolean;
begin
  if nWaitFor then
       Result := FEvent.WaitFor(INFINITE) = wrSignaled
  else Result := FEvent.WaitFor(0) = wrSignaled;
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
    FEvent.SetEvent;
  //set event signal
end;

//------------------------------------------------------------------------------
constructor TWaitTimer.Create;
begin
  FTimeResult := 0;
  FTimer := TStopwatch.Create;
end;

procedure TWaitTimer.StartTime;
begin
  FTimer.Reset;
  FTimer.Start;
end;

function TWaitTimer.EndTime: Int64;
begin
  FTimer.Stop;
  Result := FTimer.ElapsedTicks;
  FTimeResult := Result;
end;

//Date: 2017-04-17
//Desc: 注册计时器对象
class procedure TWaitTimer.ManageTimer(const nInit: Boolean);
var nItem: PTimerItem;
begin
  if nInit then
  begin
    FHasRegist := False;
    Exit;
  end;

  gMG.CheckSupport('TWaitTimer', ['TObjectPoolManager', 'TSerialIDManager']);
  //检查依赖

  gMG.FObjectPool.NewClass(TWaitTimer,
    function(var nData: Pointer):TObject
    begin
      Result := TWaitTimer.Create;
      New(nItem);
      nData := nItem;

      nItem.FThread := 0;
      nItem.FLastActive := 0;
      nItem.FSerialID.FID := 0;
    end,

    procedure(const nObj: TObject; const nData: Pointer)
    begin
      TWaitTimer(nObj).Free;
      Dispose(PTimerItem(nData));
    end);
  //xxxxx

  FHasRegist := True;
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
    if not FHasRegist then
      ManageTimer(False);
    nCurID := TThread.Current.ThreadID;

    nTimer := gMG.FObjectPool.Lock(TWaitTimer, nil, @nItem,
      function(const nObj: TObject; const nData: Pointer; var nTimes: Integer;
        const nUsed: Boolean): Boolean
      begin
        nItem := nData;
        Result := (not Assigned(nItem)) or ((nItem.FThread = 0) or
          (TDateTimeHelper.GetTickCountDiff(nItem.FLastActive) > 60 * 60 * 1000));
        //空闲对象
      end) as TWaitTimer;
    //xxxxx

    nTimer.StartTime;
    nItem.FThread := nCurID;
    nItem.FLastActive := TDateTimeHelper.GetTickCount;
    nItem.FSerialID := gMG.FSerialIDManager.GetSerialID;
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
    nMaxID: TSerialID;
begin
  nTimer := nil;
  try
    Result := 0;
    nMaxID.FID := 0;
    nCurID := TThread.Current.ThreadID;

    nTimer := gMG.FObjectPool.Lock(TWaitTimer, nil, @nItem,
      function(const nObj: TObject; const nData: Pointer; var nTimes: Integer;
        const nUsed: Boolean): Boolean
      begin
        nItem := nData;
        if nTimes = 1 then
        begin
          Result := False;
          if nItem.FThread <> nCurID then Exit;

          if (nMaxID.FID = 0) or gMG.FSerialIDManager.CompareID(nItem.FSerialID,
             nMaxID, srBigger) then
          begin
            nMaxID := nItem.FSerialID;
            //扫描最大序列号
          end;
        end else
        begin
          Result := (nMaxID.FID > 0) and (nItem.FSerialID.FID = nMaxID.FID);
        end;

        if nTimes = 1 then
          nTimes := 2;
        //扫描2轮
      end, False, True) as TWaitTimer;
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
  TWaitTimer.ManageTimer(True);
finalization
  //nothing
end.
