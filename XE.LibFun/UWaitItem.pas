{*******************************************************************************
  ����: dmzn@163.com 2017-04-16
  ����: ʵ�ֵȴ�����͸����ܵȴ�������

  ����:
  &.TWaitObject��EnterWait���������,ֱ��Wakeup����.
  &.�ö�����̰߳�ȫ,��A�߳�EnterWait,B�߳�Wakeup.
  &.TWaitTimerʵ��΢�뼶�������.
  &.TWaitTimer���̵߳���ʱ,�Զ�Ϊÿ���̷߳����������;ͬ�̶߳�ε���ʱ,�Զ�ƥ��
    ��������.
  &.TWaitTimerͬ�̵߳����߼�:
    TWaitTimer.StartHighResolutionTimer;      //1
    sleep(1);
    TWaitTimer.StartHighResolutionTimer;      //2
    sleep(2);
    TWaitTimer.StartHighResolutionTimer;      //3
    Sleep(3);
    TWaitTimer.GetHighResolutionTimerResult;  //��3���
    TWaitTimer.GetHighResolutionTimerResult;  //��2���
    TWaitTimer.GetHighResolutionTimerResult;  //��1���
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
    {*��������*}
  private     
    FEvent: TSimpleEvent;
    {*�ȴ��¼�*}
    FInterval: Cardinal;
    {*�ȴ����*}
    FStatus: Integer;
    {*�ȴ�״̬*}
    FWaitResult: TWaitResult;
    {*�ȴ����*}
  public
    constructor Create(nEventName: string = '');
    destructor Destroy; override;
    {*�����ͷ�*}
    procedure InitStatus(const nWakeup: Boolean);
    {*��ʼ״̬*}
    function EnterWait: TWaitResult;
    procedure Wakeup(const nForce: Boolean = False);
    {*�ȴ�.����*}
    function IsWaiting: Boolean;
    function IsTimeout: Boolean;
    function IsWakeup: Boolean;
    {*�ȴ�״̬*}
    property WaitResult: TWaitResult read FWaitResult;
    property Interval: Cardinal read FInterval write FInterval;
  end;

  TCrossProcWaitObject = class(TObject)
  private
    FEvent: TSimpleEvent;
    {*ͬ���¼�*}
    FLockStatus: Boolean;
    {*����״̬*}
  public
    constructor Create(nEventName: string = '');
    destructor Destroy; override;
    {*�����ͷ�*}
    function SyncLockEnter(const nWaitFor: Boolean = False): Boolean;
    procedure SyncLockLeave(const nOnlyMe: Boolean = True);
    {*ͬ������*}
  end;

  TWaitTimer = class(TObject)
  strict private
    type
      PTimerItem = ^TTimerItem;
      TTimerItem = record
        FThread    : THandle;   //�߳̾��
        FLastActive: Cardinal;  //�ϴλ
        FSerialID  : TSerialID; //���б��
      end;
  class var
    FHasRegist: Boolean;
    {*ע����*}
  private
    FTimer: TStopwatch;
    {*��ʱ��*}
    FTimeResult: Int64;
    {*��ʱ���*}
  public
    constructor Create;
    {*�����ͷ�*}
    class procedure ManageTimer(const nInit: Boolean); static;
    {*ע�����*}
    procedure StartTime;
    class procedure StartHighResolutionTimer; static;
    {*��ʼ��ʱ*}    
    function EndTime: Int64;
    class function GetHighResolutionTimerResult: Int64; static;
    {*������ʱ*}
    property TimeResult: Int64 read FTimeResult;
    {*�������*}
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
//Parm: �ȴ��ź�
//Desc: ����ͬ���ź���,�����ɹ�����True
function TCrossProcWaitObject.SyncLockEnter(const nWaitFor: Boolean): Boolean;
begin
  if nWaitFor then
       Result := FEvent.WaitFor(INFINITE) = wrSignaled
  else Result := FEvent.WaitFor(0) = wrSignaled;
  {*
    a.FEvent��ʼ״̬ΪTrue.
    b.�״�WaitFor����WAIT_OBJECT_0,�����ɹ�.
    c.FEvent��λ��ʽΪFalse,WaitFor���óɹ����Զ���Ϊ"���ź�".
    d.����WaitFor���᷵��WAIT_TIMEOUT,����ʧ��.
    e.LockRelease��,����.
  *}

  FLockStatus := Result;
  {*�Ƿ񱾶�������*}
end;

//Date: 2017-04-16
//Parm: ֻ���������������ź�
//Desc: ����ͬ���ź�
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
//Desc: ע���ʱ������
class procedure TWaitTimer.ManageTimer(const nInit: Boolean);
var nItem: PTimerItem;
begin
  if nInit then
  begin
    FHasRegist := False;
    Exit;
  end;

  gMG.CheckSupport('TWaitTimer', ['TObjectPoolManager', 'TSerialIDManager']);
  //�������

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
//Desc: ��ʼһ������
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
        //���ж���
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
//Desc: ���ؼ������
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
            //ɨ��������к�
          end;
        end else
        begin
          Result := (nMaxID.FID > 0) and (nItem.FSerialID.FID = nMaxID.FID);
        end;

        if nTimes = 1 then
          nTimes := 2;
        //ɨ��2��
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
