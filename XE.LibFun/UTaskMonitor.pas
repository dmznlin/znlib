{*******************************************************************************
  作者: dmzn@163.com 2019-02-26
  描述: 监控任务的执行状态
*******************************************************************************}
unit UTaskMonitor;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, UBaseObject, UThreadPool;

const
  cTaskTimeoutShort = 500;       //短超时
  cTaskTimeoutLong  = 5 * 1000;  //长超时

type
  PTaskItem = ^TTaskItem;
  TTaskItem = record
    FTaskID   : Cardinal;        //任务标识
    FDesc     : string;          //任务描述
    FStart    : Cardinal;        //开始时间
    FTimeOut  : Cardinal;        //超时时间
  end;

  TTaskMonitorStatus = record
    FNumAll   : Integer;         //任务总数
    FNumMax   : Integer;         //同时执行最大数
    FTOIndex  : Integer;         //超时项索引
    FTimeOut  : array[0..9] of TTaskItem; //超时任务
  end;

  TTaskMonitor = class(TManagerBase)
  private
    FItemID: Cardinal;
    {*数据标识*}
    FTasks: TList;
    {*任务列表*}
    FMonitor: TThreadWorkerConfig;
    {*监视线程*}
    FStatus: TTaskMonitorStatus;
    {*监控状态*}
  protected
    procedure ClearTask(const nFree: Boolean);
    {*清理资源*}
    function GetTask(const nTaskID: Cardinal = 0): Integer;
    {*检索任务*}
    procedure DoThreadMonitor(const nConfig: PThreadWorkerConfig;
      const nThread: TThread);
    {*监视服务*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    function AddTask(const nDesc: string;
      const nTimeout: Cardinal = cTaskTimeoutShort): Cardinal;
    procedure DelTask(const nTaskID: Cardinal; const nLog: Boolean = False);
    {*添加删除*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*延迟执行*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
  end;

var
  gTaskMonitor: TTaskMonitor = nil;
  //全局使用
  
implementation

uses
  UManagerGroup, ULibFun;

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TTaskMonitor, '任务监控服务', nEvent);
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
//Parm: 是否注册
//Desc: 向系统注册管理器对象
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
end;

procedure TTaskMonitor.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TTaskMonitor', ['TThreadPoolManager', 'TMemDataManager',
    'TSerialIDManager']);
  //检查支持
  FTasks := TList.Create;

  FItemID := gMG.FMemDataManager.NewType('TTaskMonitor.TaskItem', 'Data',
    function (): Pointer //Desc: 申请内存
    var nItem: PTaskItem;
    begin
      New(nItem);
      Result := nItem;
    end,
    procedure (const nData: Pointer) //Desc: 释放内存
    begin
      Dispose(PTaskItem(nData));
    end
  );

  gMG.FThreadPool.WorkerInit(FMonitor);
  with FMonitor do
  begin
    FWorkerName   := 'TTaskMonitor.Monitor';
    FParentObj    := Self;
    FParentDesc   := '任务监控服务';
    FCallInterval := 100;
    FProcEvent    := DoThreadMonitor;
  end;

  gMG.FThreadPool.WorkerAdd(@FMonitor);
  //添加线程作业
end;

procedure TTaskMonitor.RunBeforUnregistAllManager;
begin
  if Assigned(gMG.FThreadPool) then
    gMG.FThreadPool.WorkerDelete(Self);
  //停止线程

  SyncEnter;
  try
    ClearTask(True);
    //清理列表
  finally
    SyncLeave;
  end;

  if Assigned(gMG.FMemDataManager) then
    gMG.FMemDataManager.DeleteType(FItemID);
  //释放
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
//Parm: 任务标识
//Desc: 检索标识为nTaskID的任务
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
//Parm: 任务描述;超时时长
//Desc: 添加一个监视任务,在nTimeout没有完成时打印日志
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
      FStart := GetTickCount();
    end;

    if FTasks.Count > FStatus.FNumMax then
      FStatus.FNumMax := FTasks.Count;
    Inc(FStatus.FNumAll);
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-26
//Parm: 任务标识;记录日志
//Desc: 删除标识为nTaskID的任务
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
     WriteLog(Format('耗时:%d,任务:%s', [TDateTimeHelper.
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

          WriteLog(Format('任务超时,耗时: %d,描述: %s', [nInt, nTask.FDesc]));
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
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList
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
        Format('超时:%-5d 任务:%s', [FTimeOut, FDesc])));
      //xxxxx
    end;
  finally
    SyncLeave;
  end;
end;

end.
