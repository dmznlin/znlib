{*******************************************************************************
  ����: dmzn@163.com 2019-01-09
  ����: �̳߳ع�����

  ��ע:
  *.ʹ�÷���:
    var nWorker: TThreadWorkerConfig;
    //0.������������
    begin
      WorkerInit(nWorker);
      //1.��ʼ��

      with nWorker do
      begin
        FWorkerName := '���ټ�ʱ��';
        FParentObj := Self;
        FParentDesc := '������';
        FDataInteger[0] := nIdx;
        FCallInterval := 100;

        //4.��������֧��3���̺߳���,��FProc��ͷ,�������߳���
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
      end; //2.��д����������Ϣ

      WorkerAdd(@nWorker);
      //3.Ͷ�ݹ�������
    end;
    ----------------------------------------------------------------------------
  *.Worker������״̬: ���������������TCPClient֮����������ҵ��,�������쳣ʱ
    ��ִ��ʱ�䳬��,�������ʱҵ��ͬʱ����,�������̳߳ص�ִ��Ч��.���������쳣ʱ
    ��Ƶ�ʵ�����ͨѶ�Ѿ�û������.

    ����Worker���������ַ�ʽ:
      1.TThreadWorkerConfig.FAutoStatus = True: �Զ��л�����״̬
        ��ִ��ʱ������ TThreadWorkerConfig.FCallMaxTake ��ΪWorker������.
      2.��Worker���̺߳���������,��:
        procedure Do(const nConfig: PThreadWorkerConfig; const nThread: TThread);
        begin
          nConfig.FWorkStatus := twsSleep;
          //FAutoStatus = Falseʱ��Ч
        end;

    ��Worker������Ϊ����(twsSleep)ʱ,����(twsNormalδ��ִ��ʱ��)���̻߳�ִ��
    ����Worker��ҵ��,���߳��������� TThreadPoolManager.NumRunSleep,ȷ�����㹻��
    �߳�ִ������ҵ��.
    ע��:��TThreadPoolManager.NumRunSleep = 0,�����ߵ�Worker����ִ��.
*******************************************************************************}
unit UThreadPool;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, {$IFDEF MSWin}Winapi.ActiveX,{$ENDIF}
  {$IFDEF POSIX}Posix.Pthread,{$ENDIF}UWaitItem, UBaseObject;

const
  cThreadMin              = 1;                   //��С�߳���
  cThreadMax              = 32;                  //����߳���
  cThreadMaxRunTake       = 10 * 1000;           //Ĭ���������ʱ
  cThreadMaxWorkInterval  = 1000;                //�߳����ɨ����

type
  TThreadPoolManager = class;
  PThreadWorkerConfig = ^TThreadWorkerConfig;

  TThreadProcedure = procedure (const nConfig: PThreadWorkerConfig;
    const nThread: TThread);
  TThreadProcEvent = procedure (const nConfig: PThreadWorkerConfig;
    const nThread: TThread) of object;
  TThreadProcRefer = reference to procedure (const nConfig: PThreadWorkerConfig;
    const nThread: TThread);
  {*�߳������к���*}

  TThreadWorkerEvent = record
    WorkProc  : TThreadProcedure;                //��׼����
    WorkEvent : TThreadProcEvent;                //�����¼�
    WorkRefer : TThreadProcRefer;                //��������
  end;

  TThreadWorkerEventName = (twnInit, twnWork, twnStop, twnFree);
  //���������¼�����: ��ʼ��,������,�������,�ͷŶ���

  TThreadWorkerStatus = (twsNormal, twsSleep, twsNull);
  TThreadWorkerStatuses = set of TThreadWorkerStatus;
  {*****************************************************************************
    ��������״̬: ����,����
    ����״̬��ָ: Worker��ҵ����ʱ������,���Խ��͵����Եĳ���.
    ����: 1.TCPͨѶʱ�����쳣,����Worker����ִ��ʱ�䳬��.
          2.���̿ռ䲻��,����Workerд�����쳣.
    ����״̬��Worker����,�������̳߳ص�ִ��Ч��,�����Ҫ���⴦��.
  *****************************************************************************}

  TThreadWorkerConfig = record
  private
  const
    cDim = 2;
    {*�����±�*}
  public
    FWorkerName   : string;                      //��������
    FParentObj    : TObject;                     //�̵߳��÷�
    FParentDesc   : string;                      //���÷�����
    FDataStr      : array[0..cDim] of string;    //�ַ���
    FDataInt      : array[0..cDim] of Integer;   //����ֵ
    FDataFlt      : array[0..cDim] of Double;    //������
    FDataPtr      : array[0..cDim] of Pointer;   //����ָ��
    FDataObj      : array[0..cDim] of TObject;   //�������

    FCallTimes    : Cardinal;                    //ִ�д���
    FCallInterval : Cardinal;                    //ִ�м��(����)
    FCallMaxTake  : Cardinal;                    //���к�ʱ(����)
    FFirstInit    : Boolean;                     //ִ�г�ʼ��
    FOnInit       : TThreadWorkerEvent;          //��ʼ��(�״�����)
    FOnWork       : TThreadWorkerEvent;          //ҵ��
    FOnStop       : TThreadWorkerEvent;          //ֹͣ
    FOnFree       : TThreadWorkerEvent;          //�ͷ�

    FCoInitialize : Boolean;                     //ִ��COM��ʼ��
    FAutoStatus   : Boolean;                     //�Զ��л�״̬
    FWorkStatus   : TThreadWorkerStatus;         //��ǰ����״̬
  end;

  PThreadWorker = ^TThreadWorker;
  TThreadWorker = record
    FWorkerID     : Cardinal;                    //�����ʶ
    FWorker       : TThreadWorkerConfig;         //��������
    FRunner       : Integer;                     //���ж���
    FLastCall     : Cardinal;                    //�ϴε���
    FStartCall    : Cardinal;                    //��ʼ����
    FStartDelete  : Cardinal;                    //��ʼɾ��
    FWakeupCall   : Cardinal;                    //���Ѽ���
  end;

  TThreadNV = record
    FName         : string;                      //��������
    FValue        : Cardinal;                    //��Чֵ
    FMemo         : string;                      //������Ϣ
  end;

  TThreadManagerStatus = record
    FNumWorkers      : Cardinal;                 //��������
    FNumWorkerValid  : Cardinal;                 //��Ч����
    FNumWorkerSleep  : Cardinal;                 //���߶���
    FNumRunners      : Cardinal;                 //���ж���
    FNumRunning      : Cardinal;                 //������
    FNumRunSleep     : Cardinal;                 //���������߳�
    FNumWorkerRun    : UInt64;                   //���ô���
    FNumWorkerMax    : Cardinal;                 //��๤������
    FNumRunnerMax    : Cardinal;                 //������ж���

    FRunDelayNow     : Cardinal;                 //�����ӳ�
    FRunDelayMax     : Cardinal;                 //��������ӳ�
    FRunMostFast     : TThreadNV;                //��������¼
    FRunMostSlow     : TThreadNV;                //����������¼
    FRunErrorIndex   : Integer;                  //���д�������
    FRunErrors       : array[0..9] of TThreadNV; //���д����¼
    FMaxWorkInterval : Cardinal;                 //���ɨ����
    FMaxWorkIdleLong : Cardinal;                 //�մ���м�ʱ
    FWorkIdleInit    : Cardinal;                 //���м�ʱ��ʼ
    FWorkIdleCounter : Cardinal;                 //����ʱ���ۻ�
  end;

  TThreadRunner = class(TThread)
  private
    FOwner: TThreadPoolManager;
    {*ӵ����*}
    FTag: Integer;
    {*�̱߳�ʶ*}
    FActiveWorker: PThreadWorker;
    {*��ǰ����*}
    FWorkInterval: Cardinal;
    FWorkIdleStart: Cardinal;
    FWorkIdleLast: Cardinal;
    {*���м�ʱ*}
    FCoInitialize: Boolean;
    {*COM��ʼ��*}
  protected
    procedure Execute; override;
    {*ִ��ҵ��*}
  public
    constructor Create(AOwner: TThreadPoolManager; ATag: Integer);
    destructor Destroy; override;
    {*�����ͷ�*}
    procedure StopMe;
    {*ֹͣ�߳�*}
  end;

  TThreadMonitor = class(TThread)
  private
    FOwner: TThreadPoolManager;
    {*ӵ����*}
    FWaiter: TWaitObject;
    {*�ȴ�����*}
    FLastRunDelayVal: Cardinal;
    FLastRunDelayInc: Cardinal;
    {*״̬���*}
  protected
    procedure DoMonitor;
    procedure Execute; override;
    {*ִ���ػ�*}
    procedure AdjustRunner(const nInc: Boolean; const nNum: Word = 1;
      const nIndex: Integer = -1);
    {*��������*}
  public
    constructor Create(AOwner: TThreadPoolManager);
    destructor Destroy; override;
    {*�����ͷ�*}
    procedure Wakeup;
    {*�����߳�*}
    procedure StopMe;
    {*ֹͣ�߳�*}
  end;

  TThreadPoolManager = class(TManagerBase)
  private
    FStatus: TThreadManagerStatus;
    {*����״̬*}
    FWorkerIndex: Integer;
    FWorkers: TList;
    {*��������*}
    FMonitor: TThreadMonitor;
    {*�ػ��߳�*}
    FRunners: TList;
    FRunnerMin: Word;
    FRunnerMax: Word;
    {*���ж���*}
    FMaxRunSleep: Word;
    FWorkerIndexSleep: Integer;
    {*���߶���*}
  protected
    procedure StopRunners;
    procedure DeleteWorker(const nIdx: Integer);
    procedure ClearWorkers(const nFree: Boolean);
    {*������Դ*}
    function RunWorkerEvent(const nWorker: PThreadWorker;
      const nEventName: TThreadWorkerEventName;
      const nThread: TThread = nil): Boolean; inline;
    {*ִ��ҵ��*}
    procedure IncErrorCounter(const nName: string; const nLock: Boolean = True;
      const nMemo: string = '');
    {*�������*}
    procedure SetRunnerMin(const nValue: Word);
    procedure SetRunnerMax(const nValue: Word);
    {*���ò���*}
    function MaxWorkInterval(const nIleTime: PCardinal = nil;
      const nLock: Boolean = False): Cardinal;
    {*ɨ����*}
    function ValidWorkerNumber(const nLock: Boolean = False): Cardinal;
    {*��Ч����*}
    function SleepWorkerNumber(const nLock: Boolean = False): Cardinal;
    {*���߶���*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    procedure RunBeforUnregistAllManager; override;
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; override;
    {*��ȡ״̬*}
    procedure WorkerInit(var nWorker: TThreadWorkerConfig);
    function WorkerAdd(const nWorker: PThreadWorkerConfig;
      const nMulti: Boolean = True): Cardinal;
    procedure WorkerDelete(const nWorkerID: Cardinal;
      const nUpdateValid: Boolean = True); overload;
    procedure WorkerDelete(const nParent: TObject); overload;
    {*���ɾ��*}
    procedure WorkerStart(const nWorkerID: Cardinal;
      const nTimes: Cardinal = INFINITE); overload;
    procedure WorkerStart(const nParent: TObject;
      const nTimes: Cardinal = INFINITE); overload;
    procedure WorkerStop(const nWorkerID: Cardinal;
      const nUpdateValid: Boolean = True); overload;
    procedure WorkerStop(const nParent: TObject); overload;
    {*����ֹͣ*}
    procedure WorkerWakeup(const nWorkerID: Cardinal); overload;
    procedure WorkerWakeup(const nParent: TObject); overload;
    {*���Ѷ���*}
    procedure WriteLog(const nEvent: string);
    {*��¼��־*}
    property ThreadMin: Word read FRunnerMin write SetRunnerMin;
    property ThreadMax: Word read FRunnerMax write SetRunnerMax;
    property NumRunSleep: Word read FMaxRunSleep write FMaxRunSleep;
    {*�������*}
  end;

const
  cThreadWorkerEventNames: array[TThreadWorkerEventName] of string = ('OnInit',
    'OnWork', 'OnStop', 'OnFree');
  //���������¼���������

var
  gThreadPoolManager: TThreadPoolManager = nil;
  //ȫ��ʹ��

implementation

uses
  UManagerGroup, UMgrLog, ULibFun;

var
  gSimpleLogger: TSimpleLogger = nil;
  //��־��¼��

procedure TThreadPoolManager.WriteLog(const nEvent: string);
var nWriter: TLogWriter;
begin
  with nWriter do
  begin
    FDesc := '�̹߳�����';
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
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
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
  //����ȫ�ֱ���
end;

//Date: 2021-01-15
//Desc: ֹͣ�̳߳�
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
//Parm: ��������;�¼�����;ִ���߳�
//Desc: ��nThread��ִ��nWorker��nEvent�¼�
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
    //����ƽ��
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
    //����ƽ��
  end;
end;

//Date: 2019-01-09
//Parm: ������
//Desc: ��ʼ��nConfig
procedure TThreadPoolManager.WorkerInit(var nWorker: TThreadWorkerConfig);
var nInit: TThreadWorkerConfig;
begin
  FillChar(nInit, SizeOf(nInit), #0);
  nWorker := nInit;

  with nWorker do
  begin
    FCallTimes := INFINITE;
    //���޴���
    FCallMaxTake := cThreadMaxRunTake;
    //������к�ʱ
    FFirstInit := True;
    //�״�����ʱ��ʼ��
    FCoInitialize := False;
    //Ĭ�ϲ�ִ��COM��ʼ��
    FAutoStatus := False;
    //���Զ��л�����״̬
    FWorkStatus := twsNormal;
    //Ĭ��״̬: ��������
  end;
end;

//Date: 2019-01-09
//Parm: ��������;��������
//Desc: ���nWorker
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
        //���÷���ͬ
      end;
    end;

    New(nPWorker);
    FWorkers.Add(nPWorker);
    FillChar(nPWorker^, SizeOf(TThreadWorker), #0);

    nPWorker.FRunner := -1;
    nPWorker.FWorker := nWorker^;
    nPWorker.FWorkerID := gMG.FSerialIDManager.GetID;

    nPWorker.FLastCall := TDateTimeHelper.GetTickCount - nWorker.FCallInterval;
    //ȷ������ִ��,�������FStatus.FRunDelayNow,�ӳٹ���ʱ�����߳�

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
  //�ػ��߳�ƽ������
end;

//Date: 2019-01-10
//Parm: ���÷�
//Desc: ɾ��nParent������Worker
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
        //ɾ�����

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
//Parm: �����ʶ;������ЧWorker
//Desc: ɾ����ʶΪnWorker��Worker
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
          //ɾ�����

          if nUpdateValid then
          begin
            ValidWorkerNumber(False);
            SleepWorkerNumber(False);
          end;
        end;

        {ɾ��ʱ��Ҫ�ȴ����ý���:
          1.��ɾ�����������̷߳���,�����̴߳�������״̬.
          2.��Worker���߳���ִ��ʱ,ʹ��Synchronizeͬ��VCL����.
          3.��������������������,�������̵߳���ʱ�ӳ�����.}
        nInMain := TThread.Current.ThreadID = MainThreadID;
        if (nWorker.FStartCall = 0) or nInMain then
        begin
          if (nWorker.FStartCall = 0) or (not nInMain) then
            DeleteWorker(nIdx); //���̵߳���ʱ�ӳ�����
          Exit;
        end;

        nExists := True;
        Break;
      end;
    finally
      SyncLeave;
    end;

    if nExists then
         Sleep(10) //�����еȴ�
    else Exit;
  end;
end;

//Date: 2019-01-10
//Parm: ���÷�;���ô���
//Desc: ����nParent������Worker
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
  //�ػ��߳�ƽ������
end;

//Date: 2019-01-10
//Parm: �����ʶ;���ô���
//Desc: ������ʶΪnWorkerID��Worker
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
  //�ػ��߳�ƽ������
end;

//Date: 2019-01-10
//Parm: ���÷�
//Desc: ֹͣnParent������Worker
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
        //ֹͣ���

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
//Parm: �����ʶ;������ЧWorker
//Desc: ֹͣ��ʶΪnWorkerID��Worker
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
          //ֹͣ���

          if nUpdateValid then
            ValidWorkerNumber(False);
          //xxxxx
        end;

        {ֹͣʱ��Ҫ�ȴ����ý���:
          1.��ֹͣ���������̷߳���,�����̴߳�������״̬.
          2.��Worker���߳���ִ��ʱ,ʹ��Synchronizeͬ��VCL����.
          3.��������������������,�������̵߳���ʱ���ȴ�.}
        if (nWorker.FStartCall = 0) or                     //δ������
           (TThread.Current.ThreadID = MainThreadID) then  //���̵߳���
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
         Sleep(10) //�����еȴ�
    else Exit;
  end;
end;

//Date: 2019-01-24
//Parm: ���÷�
//Desc: ȡ��nParent����Worker�ĵȴ�ʱ��,����ִ��
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
        //����
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-24
//Parm: �����ʶ
//Desc: ȡ��nWorkerID�ĵȴ�ʱ��,����ִ��
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
        nWorker.FWakeupCall := TDateTimeHelper.GetTickCount(); //����
        Break;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-15
//Parm: ���÷�;�Ƿ�����
//Desc: �ۼ����д������
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
//Desc: ��ȡ��Ч�Ĺ����������
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
//Desc: ��ȡ�����ߵĹ����������
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
//Parm: ����ʱ��;�Ƿ�����
//Desc: �����̵߳����ɨ����
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
//Parm: �б�;�Ƿ��Ѻ���ʾ
//Desc: ��������״̬���ݴ���nList
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
//Desc: ��ȡ������������
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
        nStr := '�̳߳ض���[Runner: %d]����.';
        nList.Add(Format(nStr, [nInt]));
      end;

      Result := hlLow;
    end;

    if (nInt >= cThreadMax) and (Result < hlBad) then
    begin
      if Assigned(nList) then
      begin
        nStr := '�̳߳ض���[Runner: %d]�ﵽ����ֵ.';
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
//Parm: ����;��������;ָ������
//Desc: ���ӻ�������ж���
procedure TThreadMonitor.AdjustRunner(const nInc: Boolean; const nNum: Word;
  const nIndex: Integer);
var nIdx,nInt: Integer;

    //Desc: �¶�������
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

    //Desc: ֹͣ���ж���
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
    //���¼����ӳ�
    FWorkIdleInit := TDateTimeHelper.GetTickCount();
    FWorkIdleCounter := 0;
    //���¼������ʱ������
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
    if nIndex >= 0 then //ָ��ɾ������
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
      //����ÿ��Worker����ʹ��һ���߳�
    end;

    if (FNumRunners < FOwner.FRunnerMin) and (FNumWorkerValid > 0) then
    begin
      AdjustRunner(True, FOwner.FRunnerMin - FNumRunners);
      //������С�߳�
    end;
  end;

  with FOwner.FStatus,TDateTimeHelper do
  begin
    nVal := GetTickCountDiff(FWorkIdleInit, TTickDefault.tdZero);
    nVal := Trunc(nVal / (60 * 1000)); //������

    if nVal >= 5 then
    begin
      nVal := Trunc(FWorkIdleCounter / (nVal * FNumRunners * 1000));
      if nVal > 30 then
      begin
        AdjustRunner(False, 1);
        //ÿ������30�����
        FOwner.WriteLog(Format('Threads Idle %d Sec,Dec %d Thread', [nVal, 1]));
      end else
      begin
        FWorkIdleInit := TDateTimeHelper.GetTickCount();
        FWorkIdleCounter := 0;
        //���¼�ʱ
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
        FOwner.IncErrorCounter(nWorker.FWorker.FWorkerName, False, '��ʱ�����');
        //Worker��ʱ�����,��Ϊ�쳣
        FOwner.WriteLog(Format('[ %s ]Worker Run Timeout %d MS', [nWorker.FWorker.FWorkerName, nVal]));
      end;
    end;

    if (nWorker.FStartDelete > 0) and (nWorker.FStartCall < 1) then
    begin
      FOwner.DeleteWorker(nIdx);
      //���������̵߳����б��ӳٵ�Worker
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
      //�����ӳٹ���,�����߳�
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

  //Parm: ��������;Worker״̬
  //Desc: ʹ��nWorkerIndexɨ��״̬ΪnStatus�Ĺ�������
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
      //��һ�ֵ���ʼλ��,ɨ�����
      nWorker := FOwner.FWorkers[nWorkerIndex];
      Inc(nWorkerIndex);

      if nWorkerIndex >= FOwner.FWorkers.Count then
      begin
        nLoop := True;
        nWorkerIndex := 0;
      end; //��ʼ��һ��ɨ��

      if not (nWorker.FWorker.FWorkStatus in nStatus) then Continue;
      //����״̬��ƥ��

      if (nWorker.FStartCall > 0) or (nWorker.FStartDelete > 0) or
         (nWorker.FWorker.FCallTimes < 1) then Continue;
      //������;ɾ����;ִ�����

      if (nWorker.FWorker.FCallInterval > 0) and (nWorker.FLastCall > 0) then
      begin
        if nWorker.FWakeupCall > 0 then
        begin
          nVal := TDateTimeHelper.GetTickCountDiff(nWorker.FWakeupCall);
          if nVal > 1000 then
            nWorker.FWakeupCall := 0;
          //������1���ڿ��ظ�ִ��
        end;

        nVal := TDateTimeHelper.GetTickCountDiff(nWorker.FLastCall);
        if (nVal < nWorker.FWorker.FCallInterval) and
           (nWorker.FWakeupCall < 1) then Continue;
        //δ��ִ��ʱ���δ����

        if (nVal > nWorker.FWorker.FCallInterval) and     //���ó�ʱ
           (nWorker.FWorker.FWorkStatus <> twsSleep) then //����ʱ�ӳ�������
        begin
          nVal := nVal - nWorker.FWorker.FCallInterval;
          //���ε��ü��������Ҫ�ļ��

          if nVal > FOwner.FStatus.FRunDelayNow then
            FOwner.FStatus.FRunDelayNow := nVal;
          //��ǰ����ӳ�

          if nVal > FOwner.FStatus.FRunDelayMax then
            FOwner.FStatus.FRunDelayMax := nVal;
          //��ʷ����ӳ�
        end;
      end;

      nWorkStatus := [nWorker.FWorker.FWorkStatus]; //ԭʼ����״̬
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
        //��ʼ���м�ʱ
      end else

      if FOwner.FStatus.FNumWorkerValid > 0 then      
      begin
        nVal := TDateTimeHelper.GetTickCountDiff(FWorkIdleStart);
        if nVal > FOwner.FStatus.FMaxWorkIdleLong then
          FOwner.FStatus.FMaxWorkIdleLong := nVal;
        //����еȴ�

        Inc(FOwner.FStatus.FWorkIdleCounter, nVal - FWorkIdleLast);
        //�ۼƿ���ʱ��
        FWorkIdleLast := nVal;
      end;

      if FWorkInterval < cThreadMaxWorkInterval then
      begin
        Inc(FWorkInterval);
        //����ʱ���ӵȴ�

        if (FWorkInterval > FOwner.FStatus.FMaxWorkInterval) and
           (FOwner.FStatus.FNumWorkerValid > 0) then
          FOwner.FStatus.FMaxWorkInterval := FWorkInterval;
        //��ȴ����
      end;
    end;
  end;

  //Desc: ���к�����
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
      if FAutoStatus and (FCallMaxTake > 0) then //�Զ�״̬,��ʱʱ��
      begin
        if nInit >= FCallMaxTake then //ִ�г�ʱ
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
        if (nInit < FValue) or (FValue < 1) then //����¼
        begin
          FValue := nInit;
          FName := FActiveWorker.FWorker.FWorkerName;
        end;
      end;

      with FOwner.FStatus.FRunMostSlow do
      begin
        if nInit > FValue then //������¼
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
        if (FOwner.FStatus.FNumRunSleep > 0) or  //�����̴߳�������ҵ��
           (FOwner.FStatus.FNumRunners < 2) then //��ǰ�̳߳�ֻ��һ���߳�
        begin
          ScanActiveWorker(FOwner.FWorkerIndex, [twsNormal]);
          //��.����ɨ��������������

          if not Assigned(FActiveWorker) then
            nWorkStatus := [twsNull];
          //δɨ�赽��������
        end;

        if (FOwner.FStatus.FNumRunSleep < FOwner.FMaxRunSleep) and //δ������
           (FOwner.FStatus.FNumWorkerSleep > 0) and                //�����߶���
           (not Assigned(FActiveWorker)) then                      //��δ�ɹ�
        begin
          ScanActiveWorker(FOwner.FWorkerIndexSleep, [twsSleep]);
          //��.��������Worker���߳�,����ɨ������Worker

          if Assigned(FActiveWorker) then
          begin
            Inc(FOwner.FStatus.FNumRunSleep);
            //���Ӽ���,���ƴ�������Worker���̸߳���
            nWorkStatus := [twsSleep];
          end;
        end;

        if nWorkStatus = [] then //��δִ��,��δ�ɹ�
          ScanActiveWorker(FOwner.FWorkerIndex, [twsNormal]);
        //ɨ��������������
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
        //��Խ����ʼ������
      end;
      {$ENDIF}

      if FActiveWorker.FWorker.FFirstInit then //ִ�г�ʼ��
      begin
        if not FOwner.RunWorkerEvent(FActiveWorker, twnInit, Self) then
          Continue;
        FActiveWorker.FWorker.FFirstInit := False;
      end;

      FOwner.RunWorkerEvent(FActiveWorker, twnWork, Self);
      //����ҵ��
      nInit := TDateTimeHelper.GetTickCountDiff(FActiveWorker.FStartCall);
      //���м�ʱ
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
