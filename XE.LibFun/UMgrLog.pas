{*******************************************************************************
  作者: dmzn@163.com 2019-01-22
  描述: 实现对日志的缓存和管理
*******************************************************************************}
unit UMgrLog;

interface

uses
  System.Classes, System.SysUtils, UBaseObject, UThreadPool, UWaitItem,
  ULibFun;

type
  TLogType = (ltNull, ltInfo, ltWarn, ltError);
  //日志类型:无,信息,警告,错误
  TLogTag = set of (ltWriteFile, ltWriteDB, ltWriteCMD);
  //日志标记

  TLogWriter = record
    FOjbect  : TClass;             //组件类型
    FDesc    : string;             //描述信息
  end;

  PLogItem = ^TLogItem;
  TLogItem = record
    FWriter  : TLogWriter;         //日志作者
    FLogTag  : TLogTag;            //日志标记
    FType    : TLogType;           //日志类型
    FTime    : TDateTime;          //日志时间
    FEvent   : string;             //日志内容
  end;

  //****************************************************************************
  TLogManager = class;
  TLogEvent = procedure (const nManager: TLogManager;
    const nLogItem: PLogItem) of object;
  TWriteLogProcedure = procedure (const nManager: TLogManager;
    const nLogItems: TList);
  TWriteLogEvent = procedure (const nManager: TLogManager;
    const nLogItems: TList) of object;
  //日志事件,回调函数

  TLogManager = class(TManagerBase)
  public
    class var
      FFileExt: string;
      {*日志扩展名*}
      FLogField: string;
      {*日志分隔符*}
  private
    FItemID: Cardinal;
    {*数据标识*}
    FBuffer: TList;
    FWriterBuffer: TList;
    {*缓冲区*}
    FWritePath: string;
    FWriteLock: TCrossProcWaitObject;
    FWriter: TThreadWorkerConfig;
    {*写日志线程*}
    FOnNewLog: TLogEvent;
    FEvent: TWriteLogEvent;
    FProcedure: TWriteLogProcedure;
    {*事件变量*}
  protected
    procedure ClearList(const nList: TList; const nFree: Boolean = False);
    {*清理资源*}
    procedure DoThreadWrite(const nConfig: PThreadWorkerConfig;
      const nThread: TThread);
    procedure DoWriteFile(const nLogItems: TList);
    {*执行写入*}
    procedure WriteErrorLog(const nList: TList);
    {*写入错误*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*延迟执行*}
    function HasItem: Boolean;
    procedure InitItem(var nItem: TLogItem);
    {*初始化*}
    procedure AddLog(const nItem: PLogItem); overload;
    procedure AddLog(const nEvent: string;
     const nType: TLogType = ltNull); overload;
    procedure AddLog(const nObj: TClass; const nDesc,nEvent: string;
     const nType: TLogType = ltNull); overload;
    {*新日志*}
    procedure StartService(const nPath: string; const nSyncLock: string = '');
    procedure StopService();
    {*启停服务*}
    class function Type2Str(const nType: TLogType;
      const nLong: Boolean = True): string;
    class function Str2Type(const nStr: string;
      const nLong: Boolean = True): TLogType;
    {*日志类型*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; override;
    {*获取状态*}
    property OnNewLog: TLogEvent read FOnNewLog write FOnNewLog;
    property WriteEvent: TWriteLogEvent read FEvent write FEvent;
    property WriteProcedure: TWriteLogProcedure read FProcedure write FProcedure;
    {*属性事件*}
  end;

var
  gLogManager: TLogManager = nil;
  //全局使用

implementation

uses
  UManagerGroup;

constructor TLogManager.Create;
begin
  inherited;
  FWritePath := '';
  FWriteLock := nil;
end;

destructor TLogManager.Destroy;
begin
  FreeAndNil(FWriteLock);
  inherited;
end;

//Date: 2019-01-23
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TLogManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TLogManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TLogManager.Create;
    gMG.FLogManager := gMG.FManagers[nIdx].FManager as TLogManager;
  end else
  begin
    gMG.FLogManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;
end;

procedure TLogManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TLogManager', ['TThreadPoolManager', 'TMemDataManager']);
  //检查支持

  FBuffer := TList.Create;
  FWriterBuffer := TList.Create;
  //创建缓冲区

  FItemID := gMG.FMemDataManager.NewType('TLogManager.LogItem', 'Data',
    function (): Pointer //Desc: 申请内存
    var nItem: PLogItem;
    begin
      New(nItem);
      Result := nItem;
    end,
    procedure (const nData: Pointer) //Desc: 释放内存
    begin
      Dispose(PLogItem(nData));
    end
  );

  gMG.FThreadPool.WorkerInit(FWriter);
  with FWriter do
  begin
    FWorkerName   := 'TLogManager.Writer';
    FParentObj    := Self;
    FParentDesc   := '日志管理器';
    FCallTimes    := 0; //暂停
    FCallInterval := 500;
    FProcEvent    := DoThreadWrite;
  end;

  gMG.FThreadPool.WorkerAdd(@FWriter);
  //添加线程作业
end;

procedure TLogManager.RunBeforUnregistAllManager;
begin
  if Assigned(gMG.FThreadPool) then
    gMG.FThreadPool.WorkerDelete(Self);
  //停止线程

  SyncEnter;
  try
    ClearList(FBuffer, True);
    ClearList(FWriterBuffer, True); //清理列表
  finally
    SyncLeave;
  end;

  if Assigned(gMG.FMemDataManager) then
    gMG.FMemDataManager.DeleteType(FItemID);
  //释放
end;

procedure TLogManager.ClearList(const nList: TList; const nFree: Boolean);
var nIdx: Integer;
begin
  for nIdx := nList.Count-1 downto 0 do
    gMG.FMemDataManager.Release(nList[nIdx]);
  //xxxxx

  if nFree then
       nList.Free
  else nList.Clear;
end;

//Date: 2019-01-24
//Parm: 日志项
//Desc: 初始化nItem
procedure TLogManager.InitItem(var nItem: TLogItem);
var nInit: TLogItem;
begin
  FillChar(nInit, SizeOf(TLogItem), #0);
  nItem := nInit;
  nItem.FLogTag := [];
  nItem.FTime := Now();
end;

//Desc: 添加日志项
procedure TLogManager.AddLog(const nItem: PLogItem);
var nP: PLogItem;
begin
  if Assigned(FOnNewLog) then
    FOnNewLog(Self, nItem);
  //基本处理,不用线程写入

  if nItem.FLogTag = [] then Exit;
  //无需处理

  SyncEnter;
  try
    nP := gMG.FMemDataManager.LockData(FItemID);
    FBuffer.Add(nP);
    nP^ := nItem^;
  finally
    SyncLeave;
  end;

  gMG.FThreadPool.WorkerWakeup(Self);
end;

//Desc: 特定对象日志
procedure TLogManager.AddLog(const nObj: TClass; const nDesc, nEvent: string;
  const nType: TLogType);
var nItem: TLogItem;
begin
  with nItem do
  begin
    FWriter.FOjbect := nObj;
    FWriter.FDesc := nDesc;

    FType := nType;
    FLogTag := [ltWriteFile];
    nItem.FTime := Now();
    FEvent := nEvent;
  end;

  AddLog(@nItem);
  //put-in buffer
end;

//Desc: 默认日志
procedure TLogManager.AddLog(const nEvent: string; const nType: TLogType);
begin
  AddLog(TLogManager, '默认日志对象', nEvent, nType);
end;

function TLogManager.HasItem: Boolean;
var nIdx,nCount: integer;
begin
  SyncEnter;
  try
    if FWriterBuffer.Count < 1 then
    begin
      nCount := FBuffer.Count - 1;
      for nIdx:=0 to nCount do
        FWriterBuffer.Add(FBuffer[nIdx]);
      FBuffer.Clear;
    end;

    Result := FWriterBuffer.Count > 0;
  finally
    SyncLeave;
  end;
end;

//Date: 2019-01-24
//Parm: 日志目录;同步标识
//Desc: 开启日志服务,在nPath中写入日志
procedure TLogManager.StartService(const nPath, nSyncLock: string);
begin
  StopService();
  //stop first

  if nPath = '' then
  begin
    FWritePath := '';
    gMG.FThreadPool.WorkerStart(Self);
    Exit;
  end;

  if not DirectoryExists(nPath) then
    ForceDirectories(nPath);
  FWritePath := nPath;

  if not Assigned(FWriteLock) then
    FWriteLock := TCrossProcWaitObject.Create(nSyncLock);
  //for thread or process sync
  gMG.FThreadPool.WorkerStart(Self);
end;

//Date: 2019-01-24
//Desc: 停止服务
procedure TLogManager.StopService;
begin
  gMG.FThreadPool.WorkerStop(Self);
  //stop thread

  SyncEnter;
  try
    ClearList(FBuffer);
    ClearList(FWriterBuffer);
  finally
    SyncLeave;
  end;
end;

procedure TLogManager.DoThreadWrite(const nConfig: PThreadWorkerConfig;
  const nThread: TThread);
begin
  if not HasItem then Exit;
  //try save all when thread terminated

  if nConfig.FDataInteger[0] > 1 then
  begin
    nConfig.FDataInteger[0] := 0;
    ClearList(FWriterBuffer);
  end;

  if FWriterBuffer.Count > 0 then
  try
    if FWritePath <> '' then
      DoWriteFile(FWriterBuffer);
    //write file

    if Assigned(FEvent) then
       FEvent(Self, FWriterBuffer);
    if Assigned(FProcedure) then
       FProcedure(Self, FWriterBuffer);
    //xxxxx

    nConfig.FDataInteger[0] := 0;
    ClearList(FWriterBuffer);
  except
    if nConfig.FDataInteger[0] = 0 then
      WriteErrorLog(FWriterBuffer);
    Inc(nConfig.FDataInteger[0]);
  end;
end;

//Date: 2019-01-24
//Parm: 日志列表
//Desc: 将nLogItems写入文件
procedure TLogManager.DoWriteFile(const nLogItems: TList);
var nStr: string;
    nFile: TextFile;
    nItem: PLogItem;
    i,nCount: integer;
begin
  FWriteLock.SyncLockEnter(True);
  try
    nStr := FWritePath + TDateTimeHelper.Date2Str(Now) + FFileExt;
    AssignFile(nFile, nStr);

    if FileExists(nStr) then
         Append(nFile)
    else Rewrite(nFile);

    nCount := nLogItems.Count - 1;
    for i:=0 to nCount do
    begin
      nItem := nLogItems[i];
      if not (ltWriteFile in nItem.FLogTag) then Continue;
      //不写文件

      nStr := Copy(nItem.FWriter.FOjbect.ClassName, 1, 15);
      nStr := TDateTimeHelper.DateTime2Str(nItem.FTime) + ' ' +
              TLogManager.Type2Str(nItem.FType, False) + FLogField +
              nStr + FLogField;
      //时间,类名

      if nItem.FWriter.FDesc <> '' then
        nStr := nStr + nItem.FWriter.FDesc + FLogField;      //描述
      nStr := nStr + nItem.FEvent;                           //事件
      WriteLn(nFile, nStr);
    end;
  finally
    CloseFile(nFile);
    FWriteLock.SyncLockLeave(True);
  end;
end;

procedure TLogManager.WriteErrorLog(const nList: TList);
var nItem: PLogItem;
begin
  nItem := gMG.FMemDataManager.LockData(FItemID);
  nList.Insert(0, nItem);

  nItem.FLogTag := [ltWriteFile];
  nItem.FType := ltError;
  nItem.FWriter.FOjbect := TLogManager;
  nItem.FWriter.FDesc := '日志线程';
  nItem.FEvent := Format('有%d笔日志写入失败,再次尝试.', [nList.Count]);
end;

class function TLogManager.Str2Type(const nStr: string;
  const nLong: Boolean): TLogType;
var nU: string;
begin
  nU := UpperCase(Trim(nStr));
  //格式化

  if nLong then
  begin
    if nU = 'INFO' then Result := ltInfo else
    if nU = 'WARN' then Result := ltWarn else
    if nU = 'ERROR' then Result := ltError else Result := ltNull;
  end else
  begin
    if nU = 'I' then Result := ltInfo else
    if nU = 'W' then Result := ltWarn else
    if nU = 'E' then Result := ltError else Result := ltNull;
  end;
end;

//Date: 2019-01-23
//Parm: 类型;长描述
//Desc: 类型转描述
class function TLogManager.Type2Str(const nType: TLogType;
  const nLong: Boolean): string;
begin
  if nLong then
  begin
    case nType of
     ltInfo: Result := 'INFO';
     ltWarn: Result := 'WARN';
     ltError: Result := 'ERROR' else Result := '';
    end;
  end else
  begin
    case nType of
     ltInfo: Result := 'I';
     ltWarn: Result := 'W';
     ltError: Result := 'E' else Result := '';
    end;
  end;
end;

function TLogManager.GetHealth(const nList: TStrings): TObjectHealth;
begin
  SyncEnter;
  try
    Result := hlNormal;
  finally
    SyncLeave;
  end;
end;

procedure TLogManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      //nList.Add('NumWorkerMax' + FNumWorkerMax.ToString);

      Exit;
    end;

    //nList.Add(FixData('NumWorkerMax:', FStatus.FNumWorkerMax));
  finally
    SyncLeave;
  end;
end;

initialization
  with TLogManager do
  begin
    FFileExt := '.log';
    FLogField := #9;
  end;
finalization
  //nothing
end.
