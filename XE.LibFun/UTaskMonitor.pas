{*******************************************************************************
  ����: dmzn@163.com 2019-02-26
  ����: ��������ִ��״̬
*******************************************************************************}
unit UTaskMonitor;

interface

uses
  System.Classes, System.SysUtils, UBaseObject, UThreadPool;

const
  cTaskTimeoutShort = 500;       //�̳�ʱ
  cTaskTimeoutLong  = 5 * 1000;  //����ʱ

type
  PTaskItem = ^TTaskItem;
  TTaskItem = record
    FTaskID   : Cardinal;        //�����ʶ
    FDesc     : string;          //��������
    FStart    : Cardinal;        //��ʼʱ��
    FTimeOut  : Cardinal;        //��ʱʱ��
  end;

  TTaskMonitorStatus = record
    FNumAll   : Integer;         //��������
    FNumMax   : Integer;         //ͬʱִ�������
    FTOIndex  : Integer;         //��ʱ������
    FTimeOut  : array[0..9] of TTaskItem; //��ʱ����
  end;

  TTaskMonitor = class(TManagerBase)
  private
    FItemID: Cardinal;
    {*���ݱ�ʶ*}
    FTasks: TList;
    {*�����б�*}
    FMonitor: TThreadWorkerConfig;
    {*�����߳�*}
    FStatus: TTaskMonitorStatus;
    {*���״̬*}
  protected
    procedure ClearTask(const nFree: Boolean);
    {*������Դ*}
    function GetTask(const nTaskID: Cardinal = 0): Integer;
    {*��������*}
    procedure DoThreadMonitor(const nConfig: PThreadWorkerConfig;
      const nThread: TThread);
    {*���ӷ���*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    function AddTask(const nDesc: string;
      const nTimeout: Cardinal = cTaskTimeoutShort): Cardinal;
    procedure DelTask(const nTaskID: Cardinal; const nLog: Boolean = False);
    {*���ɾ��*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*�ӳ�ִ��*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
  end;

var
  gTaskMonitor: TTaskMonitor = nil;
  //ȫ��ʹ��
  
implementation

uses
  UManagerGroup, ULibFun;

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TTaskMonitor, '�����ط���', nEvent);
end;

constructor TTaskMonitor.Create;
begin
  inherited;
  FTasks := nil;

  FStatus.FTOIndex := Low(FStatus.FTimeOut);
  FillChar(FStatus, SizeOf(TTaskMonitorStatus), #0);
end;

destructor TTaskMonitor.Destroy;
begin
  //call RunBeforUnregistAllManager
  inherited;
end;

//Date: 2019-01-26
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TTaskMonitor.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TTaskMonitor);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TTaskMonitor.Create;
    gMG.FTaskMonitor := gMG.FManagers[nIdx].FManager as TTaskMonitor;
  end else
  begin
    gMG.FTaskMonitor := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gTaskMonitor := gMG.FTaskMonitor;
  //����ȫ�ֱ���
end;

procedure TTaskMonitor.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TTaskMonitor', ['TThreadPoolManager', 'TMemDataManager',
    'TSerialIDManager']);
  //���֧��
  FTasks := TList.Create;

  FItemID := gMG.FMemDataManager.NewType('TTaskMonitor.TaskItem', 'Data',
    function (): Pointer //Desc: �����ڴ�
    var nItem: PTaskItem;
    begin
      New(nItem);
      Result := nItem;
    end,
    procedure (const nData: Pointer) //Desc: �ͷ��ڴ�
    begin
      Dispose(PTaskItem(nData));
    end
  );

  gMG.FThreadPool.WorkerInit(FMonitor);
  with FMonitor do
  begin
    FWorkerName   := 'TTaskMonitor.Monitor';
    FParentObj    := Self;
    FParentDesc   := '�����ط���';
    FCallInterval := 100;
    FProcEvent    := DoThreadMonitor;
  end;

  gMG.FThreadPool.WorkerAdd(@FMonitor);
  //����߳���ҵ
end;

procedure TTaskMonitor.RunBeforUnregistAllManager;
begin
  if Assigned(gMG.FThreadPool) then
    gMG.FThreadPool.WorkerDelete(Self);
  //ֹͣ�߳�

  SyncEnter;
  try
    ClearTask(True);
    //�����б�
  finally
    SyncLeave;
  end;

  if Assigned(gMG.FMemDataManager) then
    gMG.FMemDataManager.DeleteType(FItemID);
  //�ͷ�
end;

procedure TTaskMonitor.ClearTask(const nFree: Boolean);
var nIdx: Integer;
begin
  for nIdx := FTasks.Count-1 downto 0 do
    gMG.FMemDataManager.Release(FTasks[nIdx]);
  //xxxxx

  if nFree then
       FTasks.Free
  else FTasks.Clear;
end;

//Date: 2019-01-26
//Parm: �����ʶ
//Desc: ������ʶΪnTaskID������
function TTaskMonitor.GetTask(const nTaskID: Cardinal): Integer;
var nIdx: Integer;
begin
  Result := -1;

  for nIdx:=FTasks.Count - 1 downto 0 do
  if PTaskItem(FTasks[nIdx]).FTaskID = nTaskID then
  begin
    Result := nIdx;
    Break;
  end;
end;

//Date: 2019-01-26
//Parm: ��������;��ʱʱ��
//Desc: ���һ����������,��nTimeoutû�����ʱ��ӡ��־
function TTaskMonitor.AddTask(const nDesc: string;
  const nTimeout: Cardinal): Cardinal;
var nTask: PTaskItem;
begin
  SyncEnter;
  try
    nTask := gMG.FMemDataManager.LockData(FItemID);
    FTasks.Add(nTask);
    Result := gMG.FSerialIDManager.GetID;

    with nTask^ do
    begin
      FTaskID := Result;
      FDesc := nDesc;
      FTimeOut := nTimeout;
      FStart := TDateTimeHelper.GetTickCount();
    end;

    if FTasks.Count > FStatus.FNumMax then
      FStatus.FNumMax := FTasks.Count;
    Inc(FStatus.FNumAll);
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-26
//Parm: �����ʶ;��¼��־
//Desc: ɾ����ʶΪnTaskID������
procedure TTaskMonitor.DelTask(const nTaskID: Cardinal; const nLog: Boolean);
var nIdx: Integer;
    nTask: PTaskItem;
begin
  SyncEnter;
  try
    nIdx := GetTask(nTaskID);
    if nIdx < 0 then Exit;
    nTask := FTasks[nIdx];

    if nLog then
     WriteLog(Format('��ʱ:%d,����:%s', [TDateTimeHelper.
      GetTickCountDiff(nTask.FStart), nTask.FDesc]));
    //xxxxx

    FTasks.Delete(nIdx);
    gMG.FMemDataManager.Release(nTask);
  finally
    SyncLeave;
  end;
end;

procedure TTaskMonitor.DoThreadMonitor(const nConfig: PThreadWorkerConfig;
  const nThread: TThread);
var nIdx: Integer;
    nInt: Cardinal;
    nTask: PTaskItem;
begin
  try
    SyncEnter;
    try
      for nIdx:=FTasks.Count - 1 downto 0 do
      begin
        nTask := FTasks[nIdx];
        nInt := TDateTimeHelper.GetTickCountDiff(nTask.FStart);

        if nInt > nTask.FTimeOut then
        begin
          with FStatus do
          begin
            FTimeOut[FTOIndex] := nTask^;
            FTimeOut[FTOIndex].FTimeOut := nInt - nTask.FTimeOut;

            Inc(FTOIndex);
            if FTOIndex > High(FTimeOut) then
              FTOIndex := Low(FTimeOut);
            //xxxxx
          end;

          WriteLog(Format('����ʱ,��ʱ: %d,����: %s', [nInt, nTask.FDesc]));
          FTasks.Delete(nIdx);
          gMG.FMemDataManager.Release(nTask);
        end;
      end;
    finally
      SyncLeave;
    end;
  except
    on E: Exception do
    begin
      WriteLog(E.Message);
    end;
  end;
end;

//Date: 2019-01-29
//Parm: �б�;�Ƿ��Ѻ���ʾ
//Desc: ��������״̬���ݴ���nList
procedure TTaskMonitor.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx: Integer;
begin
    with TObjectStatusHelper,FStatus do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      nList.Add('NumAll=' + FStatus.FNumAll.ToString);
      nList.Add('NumMax=' + FStatus.FNumMax.ToString);
      nList.Add('NumNow=' + FTasks.Count.ToString);
      Exit;
    end;

    nList.Add(FixData('NumAll:', FStatus.FNumAll));
    nList.Add(FixData('NumMax:', FStatus.FNumMax));
    nList.Add(FixData('NumNow:', FTasks.Count));

    for nIdx := Low(FStatus.FTimeOut) to High(FStatus.FTimeOut) do
    with FStatus.FTimeOut[nIdx] do
    begin
      if FTaskID < 1 then Continue;
      nList.Add(FixData(Format('Task Timeout %d:', [nIdx + 1]),
        Format('��ʱ:%-5d ����:%s', [FTimeOut, FDesc])));
      //xxxxx
    end;
  finally
    SyncLeave;
  end;
end;

end.
