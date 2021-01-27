{*******************************************************************************
  ����: dmzn@163.com 2019-01-22
  ����: ʵ�ֶ���־�Ļ���͹���
*******************************************************************************}
unit UMgrLog;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, UBaseObject, UThreadPool, UWaitItem;

type
  TLogType = (ltNull, ltInfo, ltWarn, ltError);
  //��־����:��,��Ϣ,����,����
  TLogTag = set of (ltWriteFile, ltWriteDB, ltWriteCMD);
  //��־���

  TLogWriter = record
    FOjbect  : TClass;             //�������
    FDesc    : string;             //������Ϣ
  end;

  PLogItem = ^TLogItem;
  TLogItem = record
    FWriter  : TLogWriter;         //��־����
    FLogTag  : TLogTag;            //��־���
    FType    : TLogType;           //��־����
    FTime    : TDateTime;          //��־ʱ��
    FEvent   : string;             //��־����
  end;

  TSimpleLogger = class(TObject)
  private
    FFileExt: string;
    FLogField: string;
    {*��־����*}
    FWritePath: string;
    FWriteLock: TCrossProcWaitObject;
    {*ͬ������*}
  public
    constructor Create(const nPath: string; const nExt: string = '';
      const nField: string = ''; const nSyncLock: Boolean = False);
    destructor Destroy; override;
    {*�����ͷ�*}
    procedure WriteLog(const nWriter: TLogWriter; const nEvent: string);
    {*��¼��־*}
  end;

  //****************************************************************************
  TLogManager = class;
  TLogManagerStatus = record
    FNumAll       : Integer;       //�ܴ�����־��
    FNumError     : Integer;       //����������
    FNumLost      : Integer;       //������־��
    FNumBufferMax : Integer;       //��������¼��
  end;

  TLogEvent = procedure (const nManager: TObject;
    const nItem: PLogItem) of object;
  TWriteLogProcedure = procedure (const nManager: TObject;
    const nItems: TList);
  TWriteLogEvent = procedure (const nManager: TObject;
    const nItems: TList) of object;
  TWriteLogEventSimple = procedure (const nEvent: string) of Object;
  //��־�¼�,�ص�����

  TLogManager = class(TManagerBase)
  public
    class var
      FFileExt: string;
      {*��־��չ��*}
      FLogField: string;
      {*��־�ָ���*}
  private
    FStatus: TLogManagerStatus;
    {*״̬����*}
    FItemID: Cardinal;
    {*���ݱ�ʶ*}
    FBuffer: TList;
    FWriterBuffer: TList;
    {*������*}
    FWritePath: string;
    FWriteLock: TCrossProcWaitObject;
    FWriter: TThreadWorkerConfig;
    {*д��־�߳�*}
    FMainUIRun: Boolean;
    FSyncMainUI: Boolean;
    {*ͬ��������*}
    FOnNewLog: TLogEvent;
    FEvent: TWriteLogEvent;
    FProcedure: TWriteLogProcedure;
    FSyncEvent: TWriteLogEvent;
    FSyncProc: TWriteLogProcedure;
    FSyncSimple: TWriteLogEventSimple;
    {*�¼�����*}
  protected
    procedure ClearList(const nList: TList; const nFree: Boolean = False);
    {*������Դ*}
    function SimpleLog(const nItem: PLogItem): string;
    {*����־*}
    procedure DoThreadWrite(const nConfig: PThreadWorkerConfig;
      const nThread: TThread);
    procedure DoWriteFile(const nLogItems: TList);
    {*ִ��д��*}
    procedure WriteErrorLog(const nList: TList);
    {*д�����*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*�ӳ�ִ��*}
    procedure RunBeforApplicationHalt; override;
    {*����ִ��*}
    procedure InitItem(var nItem: TLogItem);
    {*��ʼ��*}
    procedure AddLog(const nItem: PLogItem); overload;
    procedure AddLog(const nEvent: string;
     const nType: TLogType = ltNull); overload;
    procedure AddLog(const nObj: TClass; const nDesc,nEvent: string;
     const nType: TLogType = ltNull); overload;
    {*����־*}
    procedure StartService(const nPath: string = ''; const nSyncLock: string = '');
    procedure StopService();
    {*��ͣ����*}
    class function Type2Str(const nType: TLogType;
      const nLong: Boolean = True): string;
    class function Str2Type(const nStr: string;
      const nLong: Boolean = True): TLogType;
    {*��־����*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
    property OnNewLog: TLogEvent read FOnNewLog write FOnNewLog;
    property WriteEvent: TWriteLogEvent read FEvent write FEvent;
    property WriteProcedure: TWriteLogProcedure read FProcedure write FProcedure;
    property SyncMainUI: Boolean read FSyncMainUI write FSyncMainUI;
    property SyncEvent: TWriteLogEvent read FEvent write FEvent;
    property SyncProcedure: TWriteLogProcedure read FProcedure write FProcedure;
    property SyncSimple: TWriteLogEventSimple read FSyncSimple write FSyncSimple;
    {*�����¼�*}
  end;

var
  gLogManager: TLogManager = nil;
  //ȫ��ʹ��

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
//Parm: ��־����;�¼�
//Desc: ����־�ļ���д��nWriter.nEvent�¼�
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
    //ʱ��,����

    if nWriter.FDesc <> '' then
      nStr := nStr + nWriter.FDesc + FLogField;            //����
    nStr := nStr + nEvent;                                 //�¼�

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
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
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
  //����ȫ�ֱ���
  {$ENDIF}
end;

procedure TLogManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TLogManager', ['TThreadPoolManager', 'TMemDataManager']);
  //���֧��

  FBuffer := TList.Create;
  FWriterBuffer := TList.Create;
  //����������

  FItemID := gMG.FMemDataManager.NewType('TLogManager.LogItem', 'Data',
    function (): Pointer //Desc: �����ڴ�
    var nItem: PLogItem;
    begin
      New(nItem);
      Result := nItem;
    end,
    procedure (const nData: Pointer) //Desc: �ͷ��ڴ�
    begin
      Dispose(PLogItem(nData));
    end
  );

  gMG.FThreadPool.WorkerInit(FWriter);
  with FWriter do
  begin
    FWorkerName   := 'TLogManager.Writer';
    FParentObj    := Self;
    FParentDesc   := '��־������';
    FCallTimes    := 0; //��ͣ
    FCallInterval := 500;
    FOnWork.WorkEvent := DoThreadWrite;
  end;

  gMG.FThreadPool.WorkerAdd(@FWriter);
  //����߳���ҵ
end;

procedure TLogManager.RunBeforUnregistAllManager;
begin
  if Assigned(gMG.FThreadPool) then
    gMG.FThreadPool.WorkerDelete(Self);
  //ֹͣ�߳�

  SyncEnter;
  try
    ClearList(FBuffer, True);
    ClearList(FWriterBuffer, True); //�����б�
  finally
    SyncLeave;
  end;

  if Assigned(gMG.FMemDataManager) then
    gMG.FMemDataManager.DeleteType(FItemID);
  //�ͷ�
end;

procedure TLogManager.RunBeforApplicationHalt;
begin
  FMainUIRun := False;
  //�������˳�
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
//Parm: ��־��
//Desc: ��ʼ��nItem
procedure TLogManager.InitItem(var nItem: TLogItem);
var nInit: TLogItem;
begin
  FillChar(nInit, SizeOf(TLogItem), #0);
  nItem := nInit;
  nItem.FLogTag := [];
  nItem.FTime := Now();
end;

//Desc: �����־��
procedure TLogManager.AddLog(const nItem: PLogItem);
var nP: PLogItem;
begin
  if Assigned(FOnNewLog) then
    FOnNewLog(Self, nItem);
  //��������,�����߳�д��

  if nItem.FLogTag = [] then Exit;
  //���账��

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

//Desc: �ض�������־
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

//Desc: Ĭ����־
procedure TLogManager.AddLog(const nEvent: string; const nType: TLogType);
begin
  AddLog(TLogManager, 'Ĭ����־����', nEvent, nType);
end;

//Date: 2019-01-24
//Parm: ��־Ŀ¼;ͬ����ʶ
//Desc: ������־����,��nPath��д����־
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
//Desc: ֹͣ����
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
//Parm: ��־��
//Desc: ����nItem�ļ������
function TLogManager.SimpleLog(const nItem: PLogItem): string;
begin
  Result := Copy(nItem.FWriter.FOjbect.ClassName, 1, 15);
  Result := TDateTimeHelper.DateTime2Str(nItem.FTime) + ' ' +
            TLogManager.Type2Str(nItem.FType, False) + FLogField +
            Result + FLogField;
  //ʱ��,����

  if nItem.FWriter.FDesc <> '' then
    Result := Result + nItem.FWriter.FDesc + FLogField;      //����
  Result := Result + nItem.FEvent;                           //�¼�
end;

//Date: 2019-01-25
//Desc: ���߳���ִ��д��
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
    Exit;//����д�����,�����
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
//Parm: ��־�б�
//Desc: ��nLogItemsд���ļ�
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
  nItem.FWriter.FDesc := '��־�߳�';
  nItem.FEvent := Format('��%d����־д��ʧ��,�ٴγ���.', [nList.Count]);
end;

class function TLogManager.Str2Type(const nStr: string;
  const nLong: Boolean): TLogType;
var nU: string;
begin
  nU := UpperCase(Trim(nStr));
  //��ʽ��

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
//Parm: ����;������
//Desc: ����ת����
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
