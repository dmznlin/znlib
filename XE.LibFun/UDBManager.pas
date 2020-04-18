{*******************************************************************************
  ����: dmzn@163.com 2020-04-16
  ����: ���ݿ�����������ӳ�

  ��ע:
  *.��ṹ����:�ֶΡ�������Ĭ��ֵ��������,�������Զ����������ڵ�����,�ﵽ
    ϵͳ�����ݿ������.
  *.������֧�ֲ�ͬ���ݿ�Ĳ���,�����ֶΡ�������������ʱ��ȷ�������ݿ�����.
*******************************************************************************}
unit UDBManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Data.Win.ADODB,
  Data.DB, UBaseObject;

type
  TDBManager = class;
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
    FInThread  : Cardinal;                      //�����߳�
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
    FManager  : TDBManager;                      //����������
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

  TDBSystemData = procedure (const nList: TList);
  //for external-system fill database.table info

  TDBManager = class(TManagerBase)
  private
    FDefaultFit: TDBType;
    {*Ĭ����������*}
    FDefaultDB: string;
    {*Ĭ�����ݿ��ʶ*}
    FAutoReconnect: Boolean;
    {*���ݿ��Զ�����*}
    FSystemDataList: array of TDBSystemData;
    {*���ݱ�������Ϣ*}
    FDBConfig: TDictionary<string, TDBConnConfig>;
    {*���ݿ������ֵ�*}
  protected
    procedure RegObjectPoolTypes;
    {*ע�����*}
    function FindTable(const nTable: string; const nList: TList): PDBTable;
    {*��������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure AddDB(nConfig: TDBConnConfig);
    {*������ݿ�*}
    procedure AddSystemData(const nData: TDBSystemData);
    function AddTable(const nTable: string; const nList: TList): PDBTable;
    {*�������*}
    procedure GetTables(const nList: TList);
    procedure ClearTables(const nList: TList; const nFree: Boolean=True);
    {*��ȡ����Ϣ*}
    function GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
    {*��ȡ���ݿ�*}
    function LockDBConn(nDB: string = ''): TADOConnection;
    procedure ReleaseDBConn(const nConn: TADOConnection);
    function CheckDBConn(nDB: string = ''): string;
    {*���ݿ���·*}
    function LockDBQuery(const nDB: string = ''): TADOQuery;
    procedure ReleaseDBQuery(const nQuery: TADOQuery;
      const nResetConn: Boolean = False);
    {*���ݿ����*}
    function DBQuery(const nSQL: string; const nQuery: TADOQuery;
      const nDB: string = ''; const nLockBookmark: Boolean = False): TDataSet;
    function DBExecute(const nSQL: string; const nCmd: TADOQuery = nil;
      const nDB: string = ''): Integer; overload;
    function DBExecute(const nList: TStrings; const nCmd: TADOQuery = nil;
      const nDB: string = ''): Integer; overload;
    {*���ݿ����*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
    property DefaultDB: string read FDefaultDB write FDefaultDB;
    property DefaultFit: TDBType read FDefaultFit write FDefaultFit;
    property AutoReconnect: Boolean read FAutoReconnect write FAutoReconnect;
    {*�������*}
  end;

implementation

uses
  ULibFun, UManagerGroup;

procedure WriteLog(const nEvent: string);
begin
  gMG.FLogManager.AddLog(TDBManager, '���ݿ���·', nEvent);
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
constructor TDBManager.Create;
begin
  FDefaultDB := 'main';
  FDefaultFit := dtMSSQL;

  FAutoReconnect := True;
  FDBConfig := TDictionary<string, TDBConnConfig>.Create();

  RegObjectPoolTypes;
  //register pool
end;

destructor TDBManager.Destroy;
begin
  FDBConfig.Free;
  inherited;
end;

//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TDBManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TDBManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TDBManager.Create;
    gMG.FDBManager := gMG.FManagers[nIdx].FManager as TDBManager;
  end else
  begin
    gMG.FDBManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;
end;

//Date: 2020-04-17
//Desc: ע������
procedure TDBManager.RegObjectPoolTypes;
var nCD: PDBConnData;
begin
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
function TDBManager.FindTable(const nTable: string; const nList:TList): PDBTable;
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
function TDBManager.AddTable(const nTable: string; const nList: TList): PDBTable;
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
//Parm: �б�
//Desc: ��ȡϵͳ����Ϣ
procedure TDBManager.GetTables(const nList: TList);
var nIdx: Integer;
begin
  ClearTables(nList, False);
  //init first

  for nIdx := Low(FSystemDataList) to High(FSystemDataList) do
    FSystemDataList[nIdx](nList);
  //xxxxx
end;

//Date: 2020-04-16
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nList����Ϣ
procedure TDBManager.ClearTables(const nList: TList;
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
procedure TDBManager.AddDB(nConfig: TDBConnConfig);
begin
  if (nConfig.FFitDB <= Low(TDBType)) or (nConfig.FFitDB > High(TDBType)) then
    nConfig.FFitDB := FDefaultFit;
  //check default

  if FDBConfig.ContainsKey(nConfig.FID) then
       FDBConfig.Items[nConfig.FID] := nConfig
  else FDBConfig.Add(nConfig.FID, nConfig);
end;

//Date: 2020-04-16
//Parm: ���ݿ��ʶ;����
//Desc: ��ȡ��ʶΪnID����Ϣ
function TDBManager.GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
begin
  Result := FDBConfig.TryGetValue(nID, nConfig);
end;

//Date: 2020-04-18
//Parm: ���÷���
//Desc: �����������÷���
procedure TDBManager.AddSystemData(const nData: TDBSystemData);
var nIdx: Integer;
begin
  for nIdx := Low(FSystemDataList) to High(FSystemDataList) do
    if @FSystemDataList[nIdx] = @nData then Exit;
  //has exists

  nIdx := Length(FSystemDataList);
  SetLength(FSystemDataList, nIdx + 1);
  FSystemDataList[nIdx] := nData;
end;

//Date: 2020-04-18
//Parm: �б�;�Ƿ��Ѻ���ʾ
//Desc: ��������״̬���ݴ���nList
procedure TDBManager.GetStatus(const nList: TStrings; const nFriendly: Boolean);
var nIdx: Integer;
    nConn: TDBConnConfig;
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      nList.Add('DefaultDB=' + FDefaultDB);
      nList.Add('DefaultFit=' + TStringHelper.Enum2Str<TDBType>(FDefaultFit));
      nList.Add('AutoReconnect=' + BoolToStr(FAutoReconnect, True));
      Exit;
    end;

    nList.Add(FixData('DefaultDB:', FDefaultDB));
    nList.Add(FixData('DefaultFit:', TStringHelper.Enum2Str<TDBType>(FDefaultFit)));
    nList.Add(FixData('AutoReconnect:', BoolToStr(FAutoReconnect, True)));

    nIdx := 1;
    for nConn in FDBConfig.Values do
    begin
      nList.Add(FixData(Format('DataBase %d:', [nIdx]),
                        Format('%s.%s', [nConn.FID, nConn.FName])));
      Inc(nIdx);
    end; //get all support database
  finally
    SyncLeave;
  end;
end;

//------------------------------------------------------------------------------
//Date: 2020-04-17
//Parm: ���ݿ��ʶ
//Desc: ��ȡnDB�����Ӷ���
function TDBManager.LockDBConn(nDB: string): TADOConnection;
var nStr: string;
    nInThread: Cardinal;
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
        Result := nUsed and Assigned(nConn) and (nConn.FInThread = nInThread);
        //ͬ�߳�ͬ����
      end else

      if nTimes = 2 then
      begin
        Result := (not nUsed) and (not Assigned(nConn)) or (nConn.FConnID = nDB);
        //ͬ������
      end else
      begin
        Result := (not nUsed) and ((not Assigned(nConn)) or
          (TDateTimeHelper.GetTickCountDiff(nConn.FConneLast) > 60 * 1000) or
          (not TADOConnection(nObj).Connected));
        //��������
      end;

      if nTimes = 1 then
        nTimes := 3;
      //����ɨ��
    end, True) as TADOConnection;
  //xxxxx

  with Result do
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
procedure TDBManager.ReleaseDBConn(const nConn: TADOConnection);
begin
  if Assigned(nConn) then
  begin
    gMG.FObjectPool.Release(nConn);
  end;
end;

//Date: 2020-04-17
//Parm: ���ݿ��ʶ
//Desc: ���nConn�Ƿ�����
function TDBManager.CheckDBConn(nDB: string = ''): string;
var nQuery: TADOQuery;
begin
  nQuery := nil;
  try
    Result := '';
    nQuery := LockDBQuery(nDB);

    with nQuery do
    try
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
    ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2020-04-17
//Parm: ���ݿ��ʶ
//Desc: ��ȡnDB���ݵ�Query����
function TDBManager.LockDBQuery(const nDB: string): TADOQuery;
begin
  Result := gMG.FObjectPool.Lock(TADOQuery) as TADOQuery;
  with Result do
  begin
    Close;
    ParamCheck := False;
    Connection := LockDBConn(nDB);
  end;
end;

//Date: 2020-04-17
//Parm: ����;����
//Desc: �ͷ�nQuery����
procedure TDBManager.ReleaseDBQuery(const nQuery: TADOQuery;
  const nResetConn: Boolean);
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

    ReleaseDBConn(nQuery.Connection);
    gMG.FObjectPool.Release(nQuery);
  end;
end;

//Date: 2020-04-17
//Parm: SQL;��ѯ����;������ǩ
//Desc: ��nQuery��ִ�в�ѯ
function TDBManager.DBQuery(const nSQL: string; const nQuery: TADOQuery;
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
      if CheckDBConn(nDB) = '' then
           Break  //connection is ok
      else raise Exception.Create('verify connection failure');
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
function TDBManager.DBExecute(const nSQL: string; const nCmd: TADOQuery;
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
function TDBManager.DBExecute(const nList: TStrings; const nCmd: TADOQuery;
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
