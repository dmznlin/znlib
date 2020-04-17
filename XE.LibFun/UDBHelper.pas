{*******************************************************************************
  ����: dmzn@163.com 2020-04-16
  ����: ���ݿ�����������ӳ�

  ��ע:
  *.��ṹ����:�ֶΡ�������Ĭ��ֵ��������,�������Զ����������ڵ�����,�ﵽ
    ϵͳ�����ݿ������.
  *.������֧�ֲ�ͬ���ݿ�Ĳ���,�����ֶΡ�������������ʱ��ȷ�������ݿ�����.
*******************************************************************************}
unit UDBHelper;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Winapi.Windows,
  Data.DB, Data.Win.ADODB, UManagerGroup, ULibFun;

type
  TDBHelper = class;
  //define first

  TDBType = (dtDefault, dtAccess, dtMSSQL, dtMySQL, dtOracle, dtSQLLite,
    dtPostgre);
  //all support database type

  TDBConnType = (ctMain, ctWork);
  //connection: main system; work database

  PDBConnConfig = ^TDBConnConfig;
  TDBConnConfig = record
    FID    : string;                            //���ӱ�ʶ
    FName  : string;                            //��������
    FConn  : string;                            //�����ַ���
    FFitDB : TDBType;                           //�������ݿ�
  end;

  PDBConnData = ^TDBConnData;
  TDBConnData = record
    FConnID    : string;                        //���ӱ�ʶ
    FConnected : Boolean;                       //����״̬
    FConneLast : Int64;                         //�ϴλ
  end;

  PDBData = ^TDBData;
  TDBData = record
    FName  : string;                            //��������
    FData  : string;                            //��������
    FFitDB : TDBType;                           //�������ݿ�
  end;

  PDBField = ^TDBField;
  TDBField = record
    FName   : string;                           //�ֶ����� Ex: L_Man
    FType   : array of TDBData;                 //�������� Ex: varChar(32)
    FMemo   : string;                           //�ֶ����� Ex: Operator Name
    FDefVal : string;                           //Ĭ��ֵ
  end;

  PDBTable = ^TDBTable;
  TDBTable = record
    FManager  : TDBHelper;                      //����������
    FName     : string;                         //������
    FFields   : array of TDBField;                //���ֶ�
    FIndexes  : array of TDBData;               //������
    FTriggers : array of TDBData;               //������
    {*������*}
    function AddF(const nField,nType,nMemo: string;
      const nDefVal: string = '';
      nDBType: TDBType = dtDefault): PDBTable;
    {*�����ֶ�*}
    function AddI(const nName,nIndex: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*��������*}
    function AddT(const nName,nTrigger: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*���Ӵ�����*}
  end;

  TDBHelper = class
  protected
    FDefaultFit: TDBType;
    {*Ĭ����������*}
    FDefaultDB: string;
    {*Ĭ�����ݿ��ʶ*}
    FAutoReconnect: Boolean;
    {*���ݿ��Զ�����*}
    FDBConfig: TDictionary<string, TDBConnConfig>;
    {*�����ֵ�*}
    procedure RegObjectPoolTypes;
    {*ע�����*}
    procedure AddSystemTables(const nList: TList); virtual; abstract;
    procedure AddSystemIndexes(const nList: TList); virtual;
    procedure AddSystemTriggers(const nList: TList); virtual;
    function AddTable(const nTable: string; const nList: TList): PDBTable;
    {*�������*}
    function FindTable(const nTable: string; const nList: TList): PDBTable;
    {*��������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    procedure AddDB(nConfig: TDBConnConfig);
    {*������ݿ�*}
    procedure GetTables(const nList: TList);
    procedure ClearTables(const nList: TList; const nFree: Boolean=True);
    {*��ȡ����Ϣ*}
    function GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
    {*��ȡ���ݿ�*}
    function LockDBConn(nDB: string = ''): TADOConnection;
    procedure ReleaseDBConn(const nConn: TADOConnection);
    function CheckDBConn(const nConn: TADOConnection): string;
    {*���ݿ���·*}
    function LockDBQuery(const nDB: string = '';
      const nBindConnection: Boolean = True): TADOQuery;
    procedure ReleaseDBQuery(const nQuery: TADOQuery;
      const nResetConn: Boolean = False; const nReleaseConn: Boolean = True);
    {*���ݿ����*}
    function DBQuery(const nSQL: string; const nQuery: TADOQuery;
      const nDB: string = ''; const nLockBookmark: Boolean = False): TDataSet;
    function DBExecute(const nSQL: string; const nCmd: TADOQuery = nil;
      const nDB: string = ''): Integer; overload;
    function DBExecute(const nList: TStrings; const nCmd: TADOQuery = nil;
      const nDB: string = ''): Integer; overload;
    {*���ݿ����*}
  end;

implementation

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TDBHelper, '���ݿ���·', nEvent);
end;

//------------------------------------------------------------------------------
//Date: 2020-04-16
//Parm: �ֶ���;�ֶ�����;�ֶ�����;Ĭ��ֵ;�������ݿ�
//Desc: ����һ������nDBType���ݿ�ı��ֶ�
function TDBTable.AddF(const nField, nType, nMemo, nDefVal: string;
  nDBType: TDBType): PDBTable;
var i,nIdx,nInt: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FManager.FDefaultFit;
  //set default

  for nIdx := Low(FFields) to High(FFields) do
  with FFields[nIdx] do
  begin
    if CompareText(nField, FName) = 0 then
    begin
      FMemo := nMemo;
      FDefVal := nDefVal;
      //update memo and default value

      for i := Low(FType) to High(FType) do
       if FType[i].FFitDB = nDBType then Exit;
      //same db, same field

      nInt := Length(FType);
      SetLength(FType, nInt + 1);
      with FType[nInt] do
      begin
        FName := nField;
        FData := nType;
        FFitDB := nDBType;
      end;

      Exit
    end;
  end;

  nInt := Length(FFields);
  SetLength(FFields, nInt + 1);
  //new table field

  with FFields[nInt] do
  begin
    FName := nField;
    FMemo := nMemo;
    FDefVal := nDefVal;

    SetLength(FType, 1);
    with FType[0] do
    begin
      FName := nField;
      FData := nType;
      FFitDB := nDBType;
    end;
  end;
end;

//Date: 2020-04-16
//Parm: ������;��������;�������ݿ�
//Desc: ����һ������nDBType�ı�����
function TDBTable.AddI(const nName,nIndex: string; nDBType: TDBType): PDBTable;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FManager.FDefaultFit;
  //set default

  for nIdx := Low(FIndexes) to High(FIndexes) do
  with FIndexes[nIdx] do
  begin
    if (CompareText(nName, FName) = 0) and (FFitDB = nDBType) then
    begin
      FData := nIndex;
      Exit;
    end; //same db,same index
  end;

  nIdx := Length(FIndexes);
  SetLength(FIndexes, nIdx + 1);
  //new index

  with FIndexes[nIdx] do
  begin
    FName := nName;
    FData := nIndex;
    FFitDB := nDBType;
  end;
end;

//Date: 2020-04-16
//Parm: ��������;����������;�������ݿ�
//Desc: ����һ������nDBType�ı�����
function TDBTable.AddT(const nName,nTrigger: string; nDBType: TDBType): PDBTable;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FManager.FDefaultFit;
  //set default

  for nIdx := Low(FTriggers) to High(FTriggers) do
  with FTriggers[nIdx] do
  begin
    if (CompareText(nName, FName) = 0) and (FFitDB = nDBType) then
    begin
      FData := nTrigger;
      Exit;
    end; //same db,same trigger
  end;

  nIdx := Length(FTriggers);
  SetLength(FTriggers, nIdx + 1);
  //new index

  with FTriggers[nIdx] do
  begin
    FName := nName;
    FData := nTrigger;
    FFitDB := nDBType;
  end;
end;

//------------------------------------------------------------------------------
constructor TDBHelper.Create;
begin
  FDefaultDB := 'main';
  FDefaultFit := dtMSSQL;

  FAutoReconnect := True;
  FDBConfig := TDictionary<string, TDBConnConfig>.Create();

  RegObjectPoolTypes;
  //register pool
end;

destructor TDBHelper.Destroy;
begin
  FDBConfig.Free;
  inherited;
end;

//Date: 2020-04-17
//Desc: ע������
procedure TDBHelper.RegObjectPoolTypes;
var nCD: PDBConnData;
begin
  with gMG.FObjectPool do
  begin
    NewClass(TADOConnection,
      function(var nData: Pointer): TObject
      begin
        Result := TADOConnection.Create(nil);
        //new connction

        New(nCD);
        nData := nCD;
        nCD.FConnID := '';
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

//Date: 2020-04-16
//Parm: ������;�б�
//Desc: ����nList������ΪnTable�ı�
function TDBHelper.FindTable(const nTable: string; const nList:TList): PDBTable;
var nIdx: Integer;
begin
  Result := nil;
  for nIdx := nList.Count - 1 downto 0 do
   if CompareText(nTable, PDBTable(nList[nIdx]).FName) = 0 then
   begin
     Result := nList[nIdx];
     Break;
   end;
end;

//Date: 2020-04-16
//Parm: ������;�б�
//Desc: ���һ����
function TDBHelper.AddTable(const nTable: string; const nList: TList): PDBTable;
begin
  Result := FindTable(nTable, nList);
  if not Assigned(Result) then
  begin
    New(Result);
    nList.Add(Result);
    Result.FName := nTable;
  end;

  Result.FManager := Self;
  //for table config
end;

//Date: 2020-04-16
//Desc: ������ݿ�����
procedure TDBHelper.AddSystemIndexes(const nList: TList);
begin
  //for sub-type
end;

//Date: 2020-04-16
//Desc: ������ݱ�����
procedure TDBHelper.AddSystemTriggers(const nList: TList);
begin
  //for sub-type
end;

//Date: 2020-04-16
//Parm: �б�
//Desc: ��ȡϵͳ����Ϣ
procedure TDBHelper.GetTables(const nList: TList);
begin
  ClearTables(nList, False);
  //init first

  AddSystemTables(nList);
  AddSystemIndexes(nList);
  AddSystemTriggers(nList);
end;

//Date: 2020-04-16
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nList����Ϣ
procedure TDBHelper.ClearTables(const nList: TList;
  const nFree: Boolean);
var nIdx: Integer;
begin
  for nIdx := nList.Count - 1 downto 0 do
    Dispose(PDBTable(nList[nIdx]));
  //xxxxx

  if nFree then
       nList.Free
  else nList.Clear;
end;

//Date: 2020-04-16
//Parm: ���ݿ�����
//Desc: �������ݿ�������
procedure TDBHelper.AddDB(nConfig: TDBConnConfig);
begin
  if (nConfig.FFitDB <= Low(TDBType)) or (nConfig.FFitDB > High(TDBType)) then
    nConfig.FFitDB := FDefaultFit;
  FDBConfig.Add(nConfig.FID, nConfig);
end;

//Date: 2020-04-16
//Parm: ���ݿ��ʶ;����
//Desc: ��ȡ��ʶΪnID����Ϣ
function TDBHelper.GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
begin
  Result := FDBConfig.TryGetValue(nID, nConfig);
end;

//------------------------------------------------------------------------------
//Date: 2020-04-17
//Parm: ���ݿ��ʶ
//Desc: ��ȡnDB�����Ӷ���
function TDBHelper.LockDBConn(nDB: string): TADOConnection;
var nStr: string;
    nCD: PDBConnData;
    nCfg: TDBConnConfig;
begin
  if nDB = '' then
    nDB := FDefaultDB;
  //set default

  if not GetDB(nDB, nCfg) then
  begin
    nStr := Format('���ݿ�[ %s ]������,��������.', [nDB]);
    raise Exception.Create(nStr);
  end;

  Result := gMG.FObjectPool.Lock(TADOConnection, nil, @nCD,
    function(const nObj: TObject; const nData: Pointer;
     var nTimes: Integer): Boolean
    begin
      if nTimes = 1 then
      begin
        Result := (not Assigned(nData)) or
                  (PDBConnData(nData).FConnID = nDB);
        //��ͬ����
      end else
      begin
        Result := (not Assigned(nData)) or (
          (not TADOConnection(nObj).Connected) or
          (TDateTimeHelper.GetTickCountDiff(PDBConnData(nData).FConneLast) > 60 * 1000));
        //��������
      end;

      if nTimes = 1 then
        nTimes := 2;
      //����ɨ��
    end
  ) as TADOConnection;

  with Result do
  begin
    nCD.FConneLast := GetTickCount();
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
procedure TDBHelper.ReleaseDBConn(const nConn: TADOConnection);
begin
  if Assigned(nConn) then
  begin
    gMG.FObjectPool.Release(nConn);
  end;
end;

//Date: 2020-04-17
//Parm: ���ݿ����·
//Desc: ���nConn�Ƿ�����
function TDBHelper.CheckDBConn(const nConn: TADOConnection): string;
var nQuery: TADOQuery;
begin
  nQuery := nil;
  try
    Result := '';
    nQuery := LockDBQuery('', False);

    with nQuery do
    try
      Connection := nConn;
      Close;
      SQL.Text := 'select 1';
      Open;
    except
      on nErr: Exception do
      begin
        Result := nErr.Message;
        WriteLog(Result);
      end;
    end;
  finally
    ReleaseDBQuery(nQuery, False, False);
  end;
end;

//Date: 2020-04-17
//Parm: ���ݿ��ʶ;����·
//Desc: ��ȡnDB���ݵ�Query����
function TDBHelper.LockDBQuery(const nDB: string;
  const nBindConnection: Boolean): TADOQuery;
begin
  Result := gMG.FObjectPool.Lock(TADOQuery) as TADOQuery;
  with Result do
  begin
    Close;
    ParamCheck := False;

    if nBindConnection then
      Connection := LockDBConn(nDB);
    //xxxxx
  end;
end;

//Date: 2020-04-17
//Parm: ����;����
//Desc: �ͷ�nQuery����
procedure TDBHelper.ReleaseDBQuery(const nQuery: TADOQuery;
  const nResetConn, nReleaseConn: Boolean);
var nCD: PDBConnData;
begin
  if Assigned(nQuery) then
  begin
    try
      if nQuery.Active then
        nQuery.Close;
      //xxxxx

      if nResetConn then
      begin
        nCD := gMG.FObjectPool.GetData(TADOConnection, nQuery.Connection);
        if not nCD.FConnected then
          nQuery.Connection.Connected := False;
        //restore old status
      end;
    except
      //ignor any error
    end;

    if nReleaseConn then
      gMG.FObjectPool.Release(nQuery.Connection);
    gMG.FObjectPool.Release(nQuery);
  end;
end;

//Date: 2020-04-17
//Parm: SQL;��ѯ����;������ǩ
//Desc: ��nQuery��ִ�в�ѯ
function TDBHelper.DBQuery(const nSQL: string; const nQuery: TADOQuery;
  const nDB: string; const nLockBookmark: Boolean): TDataSet;
var nStep: Integer;
    nException: string;
    nBookMark: TBookmark;
begin
  Result := nil;
  nException := '';
  nStep := 0;

  while nStep <= 2 do
  try
    if nStep = 1 then
    begin
      if CheckDBConn(nQuery.Connection) = '' then
        Break;
      //connection is ok
    end else

    if nStep = 2 then
    begin
      nQuery.Connection.Close;
      nQuery.Connection.Open;
    end; //reconnnect

    if not nQuery.Connection.Connected then
      nQuery.Connection.Connected := True;
    //xxxxx

    if nLockBookmark then
    begin
      nQuery.DisableControls;
      nBookMark := nQuery.GetBookmark;
    end; //lock bookmark first

    try
      nQuery.Close;
      nQuery.SQL.Text := nSQL;
      nQuery.Open;

      Result := nQuery;
      nException := '';

      if nLockBookmark then
      begin
        if nQuery.BookmarkValid(nBookMark) then
          nQuery.GotoBookmark(nBookMark);
        //restore booktmark
      end;

      Break;
    finally
      if nLockBookmark then
      begin
        nQuery.FreeBookmark(nBookMark);
        nQuery.EnableControls;
      end;
    end;
  except
    on nErr: Exception do
    begin
      Inc(nStep);
      nException := nErr.Message;

      if nException = '' then
        nException := 'Unknow Error(Null).';
      WriteLog(nException);

      if (not FAutoReconnect) or (nStep > 2) then
      begin
        nQuery.Connection.Connected := False;
        Break;
      end;
    end;
  end;

  if nException <> '' then
    raise Exception.Create(nException);
  //xxxxx
end;

//Date: 2020-04-17
//Parm: SQL;����;���ݿ��ʶ
//Desc: ��nDB��ִ��д�����
function TDBHelper.DBExecute(const nSQL: string; const nCmd: TADOQuery;
  const nDB: string): Integer;
var nC: TADOQuery;
    nStep: Integer;
    nException: string;
begin
  nC := nil;
  try
    if Assigned(nCmd) then
         nC := nCmd
    else nC := LockDBQuery(nDB);

    Result := -1;
    nException := '';
    nStep := 0;

    while nStep <= 2 do
    try
      if nStep = 1 then
      begin
        if CheckDBConn(nC.Connection) = '' then
          Break;
        //connection is ok
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
        WriteLog(nException);

        if (not FAutoReconnect) or (nStep > 2) then
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
function TDBHelper.DBExecute(const nList: TStrings; const nCmd: TADOQuery;
  const nDB: string): Integer;
var nIdx: Integer;
    nC: TADOQuery;
begin
  nC := nil;
  try
    if Assigned(nCmd) then
         nC := nCmd
    else nC := LockDBQuery(nDB);

    if not nC.Connection.Connected then
      nC.Connection.Connected := True;
    //xxxxx

    Result := 0;
    try
      nC.Connection.BeginTrans;
      //trans start

      for nIdx := 0 to nList.Count-1 do
      with nC do
      begin
        Close;
        SQL.Text := nList[nIdx];
        Result := Result + ExecSQL;
      end;

      nC.Connection.CommitTrans;
      //commit
    except
      on nErr: Exception do
      begin
        nC.Connection.RollbackTrans;
        nC.Connection.Connected := False;
        raise;
      end;
    end;
  finally
    if not Assigned(nCmd) then
      ReleaseDBQuery(nC);
    //xxxxx
  end;
end;

end.
