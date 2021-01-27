{*******************************************************************************
  作者: dmzn@163.com 2019-01-22
  描述: 实现对日志的缓存和管理
*******************************************************************************}
unit UMgrLog;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, UBaseObject, UThreadPool, UWaitItem;

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

  TSimpleLogger = class(TObject)
  private
    FFileExt: string;
    FLogField: string;
    {*日志特征*}
    FWritePath: string;
    FWriteLock: TCrossProcWaitObject;
    {*同步锁定*}
  public
    constructor Create(const nPath: string; const nExt: string = '';
      const nField: string = ''; const nSyncLock: Boolean = False);
    destructor Destroy; override;
    {*创建释放*}
    procedure WriteLog(const nWriter: TLogWriter; const nEvent: string);
    {*记录日志*}
  end;

  //****************************************************************************
  TLogManager = class;
  TLogManagerStatus = record
    FNumAll       : Integer;       //总处理日志数
    FNumError     : Integer;       //处理错误次数
    FNumLost      : Integer;       //抛弃日志数
    FNumBufferMax : Integer;       //缓存最大记录数
  end;

  TLogEvent = procedure (const nManager: TObject;
    const nItem: PLogItem) of object;
  TWriteLogProcedure = procedure (const nManager: TObject;
    const nItems: TList);
  TWriteLogEvent = procedure (const nManager: TObject;
    const nItems: TList) of object;
  TWriteLogEventSimple = procedure (const nEvent: string) of Object;
  //日志事件,回调函数

  TLogManager = class(TManagerBase)
  public
    class var
      FFileExt: string;
      {*日志扩展名*}
      FLogField: string;
      {*日志分隔符*}
  private
    FStatus: TLogManagerStatus;
    {*状态数据*}
    FItemID: Cardinal;
    {*数据标识*}
    FBuffer: TList;
    FWriterBuffer: TList;
    {*缓冲区*}
    FWritePath: string;
    FWriteLock: TCrossProcWaitObject;
    FWriter: TThreadWorkerConfig;
    {*写日志线程*}
    FMainUIRun: Boolean;
    FSyncMainUI: Boolean;
    {*同步至界面*}
    FOnNewLog: TLogEvent;
    FEvent: TWriteLogEvent;
    FProcedure: TWriteLogProcedure;
    FSyncEvent: TWriteLogEvent;
    FSyncProc: TWriteLogProcedure;
    FSyncSimple: TWriteLogEventSimple;
    {*事件变量*}
  protected
    procedure ClearList(const nList: TList; const nFree: Boolean = False);
    {*清理资源*}
    function SimpleLog(const nItem: PLogItem): string;
    {*简单日志*}
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
    procedure RunBeforApplicationHalt; override;
    {*关联执行*}
    procedure InitItem(var nItem: TLogItem);
    {*初始化*}
    procedure AddLog(const nItem: PLogItem); overload;
    procedure AddLog(const nEvent: string;
     const nType: TLogType = ltNull); overload;
    procedure AddLog(const nObj: TClass; const nDesc,nEvent: string;
     const nType: TLogType = ltNull); overload;
    {*新日志*}
    procedure StartService(const nPath: string = ''; const nSyncLock: string = '');
    procedure StopService();
    {*启停服务*}
    class function Type2Str(const nType: TLogType;
      const nLong: Boolean = True): string;
    class function Str2Type(const nStr: string;
      const nLong: Boolean = True): TLogType;
    {*日志类型*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
    property OnNewLog: TLogEvent read FOnNewLog write FOnNewLog;
    property WriteEvent: TWriteLogEvent read FEvent write FEvent;
    property WriteProcedure: TWriteLogProcedure read FProcedure write FProcedure;
    property SyncMainUI: Boolean read FSyncMainUI write FSyncMainUI;
    property SyncEvent: TWriteLogEvent read FEvent write FEvent;
    property SyncProcedure: TWriteLogProcedure read FProcedure write FProcedure;
    property SyncSimple: TWriteLogEventSimple read FSyncSimple write FSyncSimple;
    {*属性事件*}
  end;

var
  gLogManager: TLogManager = nil;
  //全局使用

implementation

uses
  UManagerGroup, ULibFun;

constructor TSimpleLogger.Create(const nPath,nExt,nField: string;
  const nSyncLock: Boolean);
begin
  FFileExt := Trim(nExt);
  if FFileExt = '' then
    FFileExt := '.log';
  //xxxxx

  FLogField := Trim(nField);
  if FLogField = '' then
    FLogField := #9;
  //xxxxx

  if not DirectoryExists(nPath) then
    ForceDirectories(nPath);
  FWritePath := nPath;

  if nSyncLock then
       FWriteLock := TCrossProcWaitObject.Create()
  else FWriteLock := nil; //for thread or process sync
end;

destructor TSimpleLogger.Destroy;
begin
  FWriteLock.Free;
  inherited;
end;

//Date: 2021-01-04
//Parm: 日志对象;事件
//Desc: 向日志文件中写入nWriter.nEvent事件
procedure TSimpleLogger.WriteLog(const nWriter: TLogWriter; const nEvent: string);
var nStr: string;
    nFile: TextFile;
begin
  if Assigned(FWriteLock) then
    FWriteLock.SyncLockEnter(True);
  //xxxxx
  try
    nStr := FWritePath +  TDateTimeHelper.Date2Str(Now) + FFileExt;
    AssignFile(nFile, nStr);

    if FileExists(nStr) then
         Append(nFile)
    else Rewrite(nFile);

    nStr := FormatDateTime('hh:nn:ss.zzz', Time()) + FLogField +
            Copy(nWriter.FOjbect.ClassName, 1, 32) + FLogField;
    //时间,类名

    if nWriter.FDesc <> '' then
      nStr := nStr + nWriter.FDesc + FLogField;            //描述
    nStr := nStr + nEvent;                                 //事件

    WriteLn(nFile, nStr);
  finally
    if Assigned(FWriteLock) then
      FWriteLock.SyncLockLeave(True);
    CloseFile(nFile);
  end;
end;

//------------------------------------------------------------------------------
constructor TLogManager.Create;
begin
  inherited;
  FWritePath := '';
  FWriteLock := nil;

  FMainUIRun := True;
  FSyncMainUI := False;
  FillChar(FStatus, SizeOf(TLogManagerStatus), #0);
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
    {$IFDEF EnableLogManager}
    gMG.FLogManager := gMG.FManagers[nIdx].FManager as TLogManager;
    {$ENDIF}
  end else
  begin
    {$IFDEF EnableLogManager}
    gMG.FLogManager := nil;
    {$ENDIF}
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  {$IFDEF EnableLogManager}
  gLogManager := gMG.FLogManager;
  //启用全局变量
  {$ENDIF}
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
    FOnWork.WorkEvent := DoThreadWrite;
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

procedure TLogManager.RunBeforApplicationHalt;
begin
  FMainUIRun := False;
  //主程序退出
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

//Date: 2019-01-24
//Parm: 日志目录;同步标识
//Desc: 开启日志服务,在nPath中写入日志
procedure TLogManager.StartService(const nPath, nSyncLock: string);
begin
  StopService();
  //stop first

  FWritePath := Trim(nPath);
  if FWritePath = '' then
    FWritePath := TApplicationHelper.gLogPath;
  //xxxxx

  if not DirectoryExists(FWritePath) then
    ForceDirectories(FWritePath);
  //xxxxx

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

//Date: 2019-05-24
//Parm: 日志项
//Desc: 返回nItem的简洁描述
function TLogManager.SimpleLog(const nItem: PLogItem): string;
begin
  Result := Copy(nItem.FWriter.FOjbect.ClassName, 1, 15);
  Result := TDateTimeHelper.DateTime2Str(nItem.FTime) + ' ' +
            TLogManager.Type2Str(nItem.FType, False) + FLogField +
            Result + FLogField;
  //时间,类名

  if nItem.FWriter.FDesc <> '' then
    Result := Result + nItem.FWriter.FDesc + FLogField;      //描述
  Result := Result + nItem.FEvent;                           //事件
end;

//Date: 2019-01-25
//Desc: 在线程中执行写入
procedure TLogManager.DoThreadWrite(const nConfig: PThreadWorkerConfig;
  const nThread: TThread);
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

    if FWriterBuffer.Count < 1 then Exit;
    //no data need write
    nCount := FBuffer.Count + FWriterBuffer.Count;

    if nCount > FStatus.FNumBufferMax then
      FStatus.FNumBufferMax := nCount;
    //xxxxx
  finally
    SyncLeave;
  end;

  if nConfig.FDataInt[0] > 1 then
  begin
    SyncEnter;
    FStatus.FNumLost := FStatus.FNumLost + FWriterBuffer.Count;
    SyncLeave;

    nConfig.FDataInt[0] := 0;
    ClearList(FWriterBuffer);
    Exit;//连续写入错误,则清空
  end;

  try
    if FWritePath <> '' then
      DoWriteFile(FWriterBuffer);
    //write file

    if Assigned(FEvent) then
       FEvent(Self, FWriterBuffer);
    if Assigned(FProcedure) then
       FProcedure(Self, FWriterBuffer);
    //xxxxx

    if FMainUIRun and FSyncMainUI and (Assigned(FSyncEvent) or
       Assigned(FSyncProc) or Assigned(FSyncSimple)) then
    begin
      TThread.Synchronize(nThread, procedure ()
      var i,nInt: Integer;
      begin
        if Assigned(FSyncEvent) then
          FSyncEvent(Self, FWriterBuffer);
        //xxxxx

        if Assigned(FSyncProc) then
          FSyncProc(Self, FWriterBuffer);
        //xxxxx

        if Assigned(FSyncSimple) then
        begin
          nInt := FWriterBuffer.Count - 1;
          for i := 0 to nInt do
            FSyncSimple(SimpleLog(FWriterBuffer[i]));
          //xxxxx
        end;
      end);
    end; //run in main-ui thread

    SyncEnter;
    FStatus.FNumAll := FStatus.FNumAll + FWriterBuffer.Count;
    SyncLeave; //count all

    nConfig.FDataInt[0] := 0;
    ClearList(FWriterBuffer);
  except
    if nConfig.FDataInt[0] = 0 then
      WriteErrorLog(FWriterBuffer);
    Inc(nConfig.FDataInt[0]);

    SyncEnter;
    Inc(FStatus.FNumError);
    SyncLeave;
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
      if (ltWriteFile in nItem.FLogTag) then
        WriteLn(nFile, SimpleLog(nItem));
      //xxxxxx
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
  nItem.FTime := Now();
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

procedure TLogManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      nList.Add('NumAll=' + FStatus.FNumAll.ToString);
      nList.Add('NumError=' + FStatus.FNumError.ToString);
      nList.Add('NumLost=' + FStatus.FNumLost.ToString);
      nList.Add('NumBufferMax=' + FStatus.FNumBufferMax.ToString);
      Exit;
    end;

    nList.Add(FixData('NumAll:', FStatus.FNumAll));
    nList.Add(FixData('NumError:', FStatus.FNumError));
    nList.Add(FixData('NumLost:', FStatus.FNumLost));
    nList.Add(FixData('NumBufferMax:', FStatus.FNumBufferMax));
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
