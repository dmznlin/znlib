{*******************************************************************************
  ����: dmzn@163.com 2021-03-14
  ����: ���ݿ�ADO����
*******************************************************************************}
unit UDBDriverADO;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Data.Win.ADODB,
  Data.DB, UDBManager;

type
  TDBDriverADO = class(TDBDriver)
  private
    function GetCurrentConnection(const nConn: TObject;
      const nConnData: PPDBConnData = nil;
      const nCallName: string = ''; const nEvent: string = ''): TADOConnection;
    {*��ǰ�߳���·*}
  protected
    function LockDBConn(nDB: string = ''): TObject; override;
    procedure ReleaseDBConn(const nConn: TObject); override;
    function CheckDBConn(const nDB: string = ''): string; override;
    {*���ݿ���·*}
    function LockDBQuery(const nDB: string = ''): TDataSet; override;
    procedure ReleaseDBQuery(const nQuery: TObject;
      const nResetConn: Boolean = False); override;
    {*���ݿ����*}
    function DBQuery(const nSQL: string;
      const nQuery: TObject = nil;
      const nDB: string = '';
      const nLockBookmark: Boolean = False): TDataSet; override;
    function DBExecute(const nSQL: string;
      const nCmd: TObject = nil;
      const nDB: string = ''): Integer; overload; override;
    function DBExecute(const nList: TStrings;
      const nCmd: TObject = nil;
      const nDB: string = ''): Integer; overload; override;
    {*���ݿ����*}
    procedure BeginTrans(const nConn: TObject); override;
    procedure CommitTrans(const nConn: TObject); override;
    procedure RollbackTrans(const nConn: TObject); override;
    {*�������*}
  public
    class function DriverInfo: TDBDriverInfo; override;
    {*������Ϣ*}
    constructor Create(AOwner: TDBManager); override;
    {*�����ͷ�*}
  end;

implementation

uses
  ULibFun, UManagerGroup;

//Desc: ������Ϣ
class function TDBDriverADO.DriverInfo: TDBDriverInfo;
begin
  Result := inherited DriverInfo;
  with Result do
  begin
    DrvName    := 'DBDriver.ADO';
    DrvAuthor  := 'dmzn@163.com';
    DrvVersion := '0.0.1';
  end;
end;

constructor TDBDriverADO.Create(AOwner: TDBManager);
var nCD: PDBConnData;
begin
  inherited Create(AOwner);

  with gMG.FObjectPool do
  begin
    NewClass(TADOConnection,
      function(var nData: Pointer): TObject
      begin
        Result := TADOConnection.Create(nil); //new connction
        New(nCD);
        nData := nCD;

        nCD.FConnID := '';
        nCD.FInThread := 0;
        //init value

        ResetTransData(nCD);
        //init trans

        {$IFDEF Debug}
        WriteMultiThreadLog('ADO.LockDBConn', 'TADOConnection.Create');
        {$ENDIF}
      end,

      procedure(const nObj: TObject; const nData: Pointer)
      begin
        nObj.Free;
        Dispose(PDBConnData(nData));
      end);
    //ado conn

    NewClass(TADOQuery,
      function(var nData: Pointer): TObject
      begin
        Result := TADOQuery.Create(nil);
      end);
    //ado query
  end;
end;

//Date: 2020-04-17
//Parm: ���ݿ��ʶ
//Desc: ��ȡnDB�����Ӷ���
function TDBDriverADO.LockDBConn(nDB: string): TObject;
var nStr: string;
    nInThread: Cardinal;
    nCD: PDBConnData;
    nCfg: TDBConnConfig;
begin
  if nDB = '' then
    nDB := FOwner.DefaultDB;
  //set default

  if not FOwner.GetDB(nDB, nCfg) then
  begin
    nStr := Format('���ݿ�[ %s ]������,��������.', [nDB]);
    FOwner.WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  nInThread := TThread.Current.ThreadID;
  //�����������߳�

  Result := gMG.FObjectPool.Lock(TADOConnection, nil, @nCD,
    function(const nObj: TObject; const nData: Pointer; var nTimes: Integer;
      const nUsed: Boolean): Boolean
    var nConn: PDBConnData;
    begin
      nConn := nData;
      //conn config

      if nTimes = 1 then
      begin
        Result := nUsed and Assigned(nConn)  //ͬ����(ʹ����)
          and (nConn.FInThread = nInThread)  //ͬ�߳�
          and (nConn.FConnID = nDB);         //ͬ���ݿ�
        //ͬ�߳�ͬ��������
      end else

      if nTimes = 2 then
      begin
        Result := (not nUsed) and ((not Assigned(nConn)) or (nConn.FConnID = nDB));
        //ͬ������
      end else
      begin
        Result := (not nUsed) and ((not Assigned(nConn)) or
          (TDateTimeHelper.GetTickCountDiff(nConn.FConneLast) > 60 * 1000) or
          (not TADOConnection(nObj).Connected));
        //��������
      end;

      {$IFDEF Debug}
      if Result then
        WriteMultiThreadLog('ADO.LockDBConn', 'Lock Mode:' + nTimes.ToString);
      {$ENDIF}

      if nTimes = 1 then
        nTimes := 3;
      //����ɨ��

      if Result and (not nUsed) then
        ResetTransData(nConn);
      //�����״�ʹ��ʱ�����������
    end, True);
  //xxxxx

  with Result as TADOConnection do
  begin
    if nCD.FInThread <> nInThread then
      nCD.FInThread := nInThread;
    //bind thread

    nCD.FConneLast := TDateTimeHelper.GetTickCount();
    nCD.FConnected := Connected;
    //conn status

    if nCD.FConnID <> nDB then
    begin
      nCD.FConnID := nDB;
      //id

      Connected := False;
      ConnectionString := nCfg.FConn;
      LoginPrompt := False;
    end;
  end;
end;

//Date: 2020-04-17
//Parm: ���Ӷ���
//Desc: �ͷ���·
procedure TDBDriverADO.ReleaseDBConn(const nConn: TObject);
begin
  if Assigned(nConn) then
  begin
    gMG.FObjectPool.Release(nConn);
  end;
end;

//Date: 2020-04-17
//Parm: ���ݿ��ʶ
//Desc: ���nConn�Ƿ�����
function TDBDriverADO.CheckDBConn(const nDB: string): string;
var nQuery: TADOQuery;
begin
  {$IFDEF Debug}
  WriteMultiThreadLog('ADO.CheckDBConn');
  {$ENDIF}

  nQuery := nil;
  try
    Result := '';
    nQuery := LockDBQuery(nDB) as TADOQuery;

    with nQuery do
    try
      Close;
      SQL.Text := 'select 1';
      Open;
    except
      on nErr: Exception do
      begin
        Result := nErr.Message;
        FOwner.WriteLog(Result);
      end;
    end;
  finally
    ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2020-04-17
//Parm: ���ݿ��ʶ
//Desc: ��ȡnDB���ݵ�Query����
function TDBDriverADO.LockDBQuery(const nDB: string): TDataSet;
var nConn: TADOConnection;
begin
  nConn := nil;
  Result := nil;
  try
    nConn := LockDBConn(nDB) as TADOConnection;
    //get connection first
    Result := gMG.FObjectPool.Lock(TADOQuery) as TADOQuery;

    with Result as TADOQuery do
    begin
      Close;
      ParamCheck := False;
      Connection := nConn;
    end;
  except
    if Assigned(Result) then
         ReleaseDBQuery(Result)
    else ReleaseDBConn(nConn);

    raise;
  end;
end;

//Date: 2020-04-17
//Parm: ����;����
//Desc: �ͷ�nQuery����
procedure TDBDriverADO.ReleaseDBQuery(const nQuery: TObject;
  const nResetConn: Boolean);
var nQry: TADOQuery;
    nCD: PDBConnData;
begin
  if Assigned(nQuery) then
  begin
    nQry := nQuery as TADOQuery;
    try
      if nQry.Active then
        nQry.Close;
      //xxxxx

      if nResetConn then
      begin
        nCD := gMG.FObjectPool.GetData(TADOConnection, nQry.Connection);
        if not nCD.FConnected then
          nQry.Connection.Connected := False;
        //restore old status
      end;
    except
      //ignor any error
    end;

    ReleaseDBConn(nQry.Connection);
    gMG.FObjectPool.Release(nQry);
  end;
end;

//Date: 2020-04-17
//Parm: SQL;��ѯ����;������ǩ
//Desc: ��nQuery��ִ�в�ѯ
function TDBDriverADO.DBQuery(const nSQL: string; const nQuery: TObject;
  const nDB: string; const nLockBookmark: Boolean): TDataSet;
var nStep: Integer;
    nQry: TADOQuery;
    nException: string;
    nBookMark: TBookmark;
begin
  nQry := nil;
  try
    if Assigned(nQuery) then
         nQry := nQuery as TADOQuery
    else nQry := LockDBQuery(nDB) as TADOQuery;

    Result := nil;
    nException := '';
    nStep := 0;

    while nStep <= 2 do
    try
      if nStep = 1 then
      begin
        if CheckDBConn(nDB) = '' then
             Break  //connection is ok
        else raise Exception.Create('verify connection failure');
      end else

      if nStep = 2 then
      begin
        nQry.Connection.Close;
        nQry.Connection.Open;
      end; //reconnnect

      if not nQry.Connection.Connected then
        nQry.Connection.Connected := True;
      //xxxxx

      if nLockBookmark then
      begin
        nQry.DisableControls;
        nBookMark := nQry.GetBookmark;
      end; //lock bookmark first

      try
        nQry.Close;
        nQry.SQL.Text := nSQL;
        nQry.Open;

        Result := nQry;
        nException := '';

        if nLockBookmark then
        begin
          if nQry.BookmarkValid(nBookMark) then
            nQry.GotoBookmark(nBookMark);
          //restore booktmark
        end;

        Break;
      finally
        if nLockBookmark then
        begin
          nQry.FreeBookmark(nBookMark);
          nQry.EnableControls;
        end;
      end;
    except
      on nErr: Exception do
      begin
        Inc(nStep);
        nException := nErr.Message;

        if nException = '' then
          nException := 'Unknow Error(Null).';
        FOwner.WriteLog(nException + #13#10 + nSQL);

        if (not FOwner.AutoReconnect) or (nStep > 2) then
        begin
          nQry.Connection.Connected := False;
          Break;
        end;
      end;
    end;
  finally
    if (nException <> '') and (not Assigned(nQuery)) then
      ReleaseDBQuery(nQry);
    //xxxxx
  end;

  if nException <> '' then
    raise Exception.Create(nException);
  //xxxxx
end;

//Date: 2020-04-17
//Parm: SQL;����;���ݿ��ʶ
//Desc: ��nDB��ִ��д�����
function TDBDriverADO.DBExecute(const nSQL: string; const nCmd: TObject;
  const nDB: string): Integer;
var nC: TADOQuery;
    nStep: Integer;
    nException: string;
begin
  nC := nil;
  try
    if Assigned(nCmd) then
         nC := nCmd as TADOQuery
    else nC := LockDBQuery(nDB) as TADOQuery;

    Result := -1;
    nException := '';
    nStep := 0;

    while nStep <= 2 do
    try
      if nStep = 1 then
      begin
        if CheckDBConn(nDB) = '' then
             Break  //connection is ok
        else raise Exception.Create('verify connection failure');
      end else

      if nStep = 2 then
      begin
        nC.Connection.Close;
        nC.Connection.Open;
      end; //reconnnect

      if not nC.Connection.Connected then
        nC.Connection.Connected := True;
      //xxxxx

      nC.Close;
      nC.SQL.Text := nSQL;
      Result := nC.ExecSQL;

      nException := '';
      Break;
    except
      on nErr: Exception do
      begin
        Inc(nStep);
        nException := nErr.Message;

        if nException = '' then
          nException := 'Unknow Error(Null).';
        FOwner.WriteLog(nException + #13#10 + nSQL);

        if (not FOwner.AutoReconnect) or (nStep > 2) then
        begin
          nC.Connection.Connected := False;
          Break;
        end;
      end;
    end;
  finally
    if not Assigned(nCmd) then
      ReleaseDBQuery(nC);
    //xxxxx
  end;

  if nException <> '' then
    raise Exception.Create(nException);
  //xxxxx
end;

//Date: 2020-04-17
//Parm: �б�;����;���ݿ��ʶ
//Desc: ��nDB������ִ��nListд����
function TDBDriverADO.DBExecute(const nList: TStrings; const nCmd: TObject;
  const nDB: string): Integer;
var nIdx: Integer;
    nC: TADOQuery;
    nCtx: TDBTransContext;
begin
  nC := nil;
  try
    if Assigned(nCmd) then
         nC := nCmd as TADOQuery
    else nC := LockDBQuery(nDB) as TADOQuery;

    Result := 0;
    nCtx := FOwner.BeginTrans(nC); //trans start
    try
      for nIdx := 0 to nList.Count-1 do
      with nC do
      begin
        Close;
        SQL.Text := nList[nIdx];
        Result := Result + ExecSQL;
      end;

      nCtx.CommitTrans;
      //commit
    except
      on nErr: Exception do
      begin
        nCtx.RollbackTrans;
        FOwner.WriteLog(nErr.Message);
      end;
    end;
  finally
    if not Assigned(nCmd) then
      ReleaseDBQuery(nC);
    //xxxxx
  end;
end;

//Date: 2021-03-21
//Parm: ���Ӷ��� or ��ѯ����
//Desc: ����nConn������������
function TDBDriverADO.GetCurrentConnection(const nConn: TObject;
  const nConnData: PPDBConnData;
  const nCallName, nEvent: string): TADOConnection;
var nStr: string;
begin
  {$IFDEF Debug}
  WriteMultiThreadLog(nCallName, nEvent);
  {$ENDIF}

  if not Assigned(nConn) then
  begin
    nStr := Format('%s: Connection Object Is Nil.', [nCallName]);
    FOwner.WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if nConn is TADOQuery then
       Result := (nConn as TADOQuery).Connection
  else Result := nConn as TADOConnection;

  if Assigned(nConnData) then
  begin
    nConnData^ := gMG.FObjectPool.GetData(TADOConnection, Result);
    if not Assigned(nConnData^) then
    begin
      nStr := Format('%s: Cann''t Get TADOConnection.PDBConnData.', [nCallName]);
      FOwner.WriteLog(nStr);
      raise Exception.Create(nStr);
    end;
  end;
end;

//Date: 2021-03-17
//Parm: ���Ӷ��� or ��ѯ����
//Desc: ��nConn�Ͽ�������
procedure TDBDriverADO.BeginTrans(const nConn: TObject);
var nStr: string;
    nDat: PDBConnData;
    nCon: TADOConnection;
begin
  nCon := GetCurrentConnection(nConn, @nDat, 'ADO.BeginTrans');
  if nDat.FTransStatus = [] then
  begin
    if nCon.InTransaction then
    begin
      nStr := 'BeginTrans: TADOConnection.InTransaction Is Invalid.';
      FOwner.WriteLog(nStr);
      raise Exception.Create(nStr);
    end;

    if not nCon.Connected then
      nCon.Connected := True;
    nCon.BeginTrans;

    nDat.FTransLevel := 1;
    nDat.FTransStatus := [tsBegin];
  end else
  begin
    Inc(nDat.FTransLevel);
    //���Ӽ���
  end;
end;

//Date: 2021-03-17
//Parm: ���Ӷ��� or ��ѯ����
//Desc: �ύnConn�ϵ�����
procedure TDBDriverADO.CommitTrans(const nConn: TObject);
var nStr: string;
    nDat: PDBConnData;
    nCon: TADOConnection;
begin
  nCon := GetCurrentConnection(nConn, @nDat, 'ADO.CommitTrans');
  if nDat.FTransStatus = [] then //����δ���� �� �ѽ���
  begin
    ResetTransData(nDat);
    Exit;
  end;

  if nDat.FTransLevel > 1 then
  begin
    Dec(nDat.FTransLevel);
    //���ټ���
    if not (tsCommit in nDat.FTransStatus) then
      Include(nDat.FTransStatus, tsCommit);
    Exit;
  end;

  if not nCon.InTransaction then
  begin
    nStr := 'CommitTrans: TADOConnection.InTransaction Is Invalid.';
    FOwner.WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if tsRollback in nDat.FTransStatus then
       nCon.RollbackTrans
  else nCon.CommitTrans;

  ResetTransData(nDat);
  //�ύ���������
end;

//Date: 2021-03-17
//Parm: ���Ӷ��� or ��ѯ����
//Desc: �ع�nConn�ϵ�����
procedure TDBDriverADO.RollbackTrans(const nConn: TObject);
var nStr: string;
    nDat: PDBConnData;
    nCon: TADOConnection;
begin
  nCon := GetCurrentConnection(nConn, @nDat, 'ADO.RollbackTrans');
  if nDat.FTransStatus = [] then //����δ���� �� �ѽ���
  begin
    ResetTransData(nDat);
    Exit;
  end;

  if nDat.FTransLevel > 1 then
  begin
    Dec(nDat.FTransLevel);
    //���ټ���
    if not (tsRollback in nDat.FTransStatus) then
      Include(nDat.FTransStatus, tsRollback);
    Exit;
  end;

  if not nCon.InTransaction then
  begin
    nStr := 'RollbackTrans: TADOConnection.InTransaction Is Invalid.';
    FOwner.WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  nCon.RollbackTrans;
  //�ع����������
  ResetTransData(nDat);
end;

initialization
  TDBManager.RegistDriver(TDBDriverADO);
  //ע������
end.
