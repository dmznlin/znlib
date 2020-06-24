{*******************************************************************************
  ����: dmzn@163.com 2020-04-16
  ����: ���ݿ�����������ӳ�

  ��ע:
  *.��ṹ����:�ֶΡ�������Ĭ��ֵ�����������洢����,�������Զ����������ڵ�����,
    �ﵽϵͳ�����ݿ������.
  *.������֧�ֲ�ͬ���ݿ�Ĳ���,�����ֶΡ�������������ʱ��ȷ�������ݿ�����.

  *.ʹ�÷���:
    1.��д���ݿ���������:
      procedure SystemTables(const nList: TList);
      begin
        gMG.FDBManager.AddTable('Sys_Dict', nList).              //��дģʽ
        AddF('D_ID',     'varChar(15)',        '��¼��ʶ').      //��ͨ�ֶ�
        AddF('D_Port',   'Integer',            '�˿�', '80').    //Ĭ��ֵ1
        AddF('D_Serial', 'Integer Default -1', 'װ�ú�', '-1').  //Ĭ��ֵ2
        //field
        Copy('Sys_Dict2').
        //table
        AddI('idx_prog', 'CREATE INDEX idx_prog ON $TB.*(D_ID ASC)').
        //index
        AddT('tr_bi', 'CREATE TRIGGER tr_bi $TB.* AFTER INSERT AS BEGIN END').
        //trigger
        AddP('pro_do', '....').
        //procedure
        AddR('init', 'INSERT INTO $TB.* Values(...)')
        //��ʼ������,����ʱ����һ��
      end;
    2.�������������������:
      gMG.FDBManager.AddTableBuilder(SystemTables);
    3.ʹ�ù�������ʼ�����ݿ�:
      gMG.FDBManager.InitDB();

  *.�ṹ��ͬ�Ķ��ű�,����������:
    1.TDBTable.Copy: ���Ʊ�ṹ.
    2.TDBTable.Add������,��SQL��京�� cDBTables ����,�����б���ִ��.
*******************************************************************************}
unit UDBManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Data.Win.ADODB,
  Data.DB, UBaseObject;

const
  //������ʶ
  sDBTables  = '$TB.*';
  sDBIndex   = '$IDX';

  //С���ֶ�
  sField_Access_Decimal          = 'Float';
  sField_SQLServer_Decimal       = 'Decimal(15, 5)';

  //ͼƬ�ֶ�
  sField_Access_Image            = 'OLEObject';
  sField_SQLServer_Image         = 'Image';

  //�������
  sField_SQLServer_Now           = 'getDate()';

  //�����ֶ�
  sField_Access_AutoInc          = 'Counter';
  sField_SQLServer_AutoInc       = 'Integer IDENTITY (1,1) PRIMARY KEY';

  //���ñ��
  sFlag_Yes                      = 'Y';         //��
  sFlag_No                       = 'N';         //��
  sFlag_Unknow                   = 'U';         //δ֪
  sFlag_Enabled                  = 'Y';         //����
  sFlag_Disabled                 = 'N';         //����

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
    FParmI : Integer;                           //���β���
    FParmB : Boolean;                           //��������
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
    FDefaultFit : TDBType;                      //����(Ĭ��)
    FName       : string;                       //������
    FFields     : array of TDBField;            //���ֶ�
    FIndexes    : array of TDBData;             //������
    FTriggers   : array of TDBData;             //������
    FProcedures : array of TDBData;             //�洢����
    FRecords    : array of TDBData;             //Ĭ�ϼ�¼
    FSameTbs    : array of string;              //ͬ�ṹ��
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
    function AddP(const nName,nProcedure: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*���Ӵ�����*}
    function AddR(const nName,nRecord: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*���ӳ�ʼ����¼*}
    function Copy(const nTable: string): PDBTable;
    {*���Ʊ�ṹ*}
  end;

  TDBTableBuilder = procedure (const nList: TList);
  //for external-system fill database.table info

  TDBManager = class(TManagerBase)
  private
    FDefaultFit: TDBType;
    {*Ĭ����������*}
    FDefaultDB: string;
    {*Ĭ�����ݿ��ʶ*}
    FAutoReconnect: Boolean;
    {*���ݿ��Զ�����*}
    FTableBuilders: array of TDBTableBuilder;
    {*���ݱ�������Ϣ*}
    FDBConfig: TDictionary<string, TDBConnConfig>;
    {*���ݿ������ֵ�*}
  protected
    procedure RegObjectPoolTypes;
    {*ע�����*}
    function FindTable(const nTable: string; const nList: TList): PDBTable;
    {*��������*}
    function InitAccess(const nDB: string; const nTables: TList;
      const nMemo: TStrings): Boolean;
    function InitMSSQL(const nDB: string; const nTables: TList;
      const nMemo: TStrings): Boolean;
    function InitPostgre(const nDB: string; const nTables: TList;
      const nMemo: TStrings): Boolean;
    {*��ʼ�����ݿ�*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    function InitDB(const nDB: string; const nMemo: TStrings = nil): Boolean;
    {*��ʼ�����ݿ�*}
    procedure AddTableBuilder(const nBuilder: TDBTableBuilder);
    procedure AddDB(nConfig: TDBConnConfig);
    function AddTable(const nTable: string; const nList: TList;
      nDBType: TDBType = dtDefault): PDBTable;
    {*�������*}
    procedure GetTables(const nList: TList);
    procedure ClearTables(const nList: TList; const nFree: Boolean = False);
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

var
  gDBManager: TDBManager = nil;
  //ȫ��ʹ��

implementation

uses
  ULibFun, UManagerGroup;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
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
    nDBType := FDefaultFit;
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
var nIdx,nInt: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FDefaultFit;
  nInt := -1;

  for nIdx := Low(FIndexes) to High(FIndexes) do
   with FIndexes[nIdx] do
    if (CompareText(nName, FName) = 0) and (FFitDB = nDBType) then
    begin
      nInt := nIdx;
      Break;
    end; //same db,same index

  if nInt < 0 then
  begin
    nInt := Length(FIndexes);
    SetLength(FIndexes, nInt + 1);
  end; //new index

  with FIndexes[nInt] do
  begin
    FName := nName;
    with TStringHelper do
      FData := MacroValue(nIndex, [MI(sDBIndex, nName)]);
    FFitDB := nDBType;

    FParmI := Pos(sDBTables, nIndex);
    FParmB := FParmI > 0;
  end;
end;

//Date: 2020-04-16
//Parm: ��������;����������;�������ݿ�
//Desc: ����һ������nDBType�ı�����
function TDBTable.AddT(const nName,nTrigger: string; nDBType: TDBType): PDBTable;
var nIdx,nInt: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FDefaultFit;
  nInt := -1;

  for nIdx := Low(FTriggers) to High(FTriggers) do
   with FTriggers[nIdx] do
    if (CompareText(nName, FName) = 0) and (FFitDB = nDBType) then
    begin
      nInt := nIdx;
      Break;
    end; //same db,same trigger

  if nInt < 0 then
  begin
    nInt := Length(FTriggers);
    SetLength(FTriggers, nInt + 1);
  end; //new trigger

  with FTriggers[nInt] do
  begin
    FName := nName;
    FData := nTrigger;
    FFitDB := nDBType;

    FParmI := Pos(sDBTables, nTrigger);
    FParmB := FParmI > 0;
  end;
end;

//Date: 2020-04-20
//Parm: �洢������;�洢���̽ű�;�������ݿ�
//Desc: ����һ������nDBType�Ĵ洢����
function TDBTable.AddP(const nName, nProcedure: string;
  nDBType: TDBType): PDBTable;
var nIdx,nInt: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FDefaultFit;
  nInt := -1;

  for nIdx := Low(FProcedures) to High(FProcedures) do
   with FProcedures[nIdx] do
    if (CompareText(nName, FName) = 0) and (FFitDB = nDBType) then
    begin
      nInt := nIdx;
      Break;
    end; //same db,same procedure

  if nInt < 0 then
  begin
    nInt := Length(FProcedures);
    SetLength(FProcedures, nInt + 1);
  end; //new procedure

  with FProcedures[nInt] do
  begin
    FName := nName;
    FData := nProcedure;
    FFitDB := nDBType;

    FParmI := -1;
    FParmB := False;
  end;
end;

//Date: 2020-04-21
//Parm: SQL�ű�;�������ݿ�
//Desc: ����һ������nDBType��SQL
function TDBTable.AddR(const nName, nRecord: string; nDBType: TDBType): PDBTable;
var nIdx,nInt: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FDefaultFit;
  nInt := -1;

  for nIdx := Low(FRecords) to High(FRecords) do
   with FRecords[nIdx] do
    if (CompareText(nName, FName) = 0) and (FFitDB = nDBType) then
    begin
      nInt := nIdx;
      Break;
    end; //same db,same record

  if nInt < 0 then
  begin
    nInt := Length(FRecords);
    SetLength(FRecords, nInt + 1); //new record
  end;

  with FRecords[nInt] do
  begin
    FName := nName;
    FData := nRecord;
    FFitDB := nDBType;

    FParmI := Pos(sDBTables, nRecord);
    FParmB := FParmI > 0;
  end;
end;

//Date: 2020-04-21
//Parm: ������
//Desc: ���ݵ�ǰ��ṹ����nTable��
function TDBTable.Copy(const nTable: string): PDBTable;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  for nIdx := Low(FSameTbs) to High(FSameTbs) do
    if CompareText(nTable, FSameTbs[nIdx]) = 0 then Exit;
  //has exists

  nIdx := Length(FSameTbs);
  SetLength(FSameTbs, nIdx + 1);
  FSameTbs[nIdx] := nTable;
end;

//------------------------------------------------------------------------------
constructor TDBManager.Create;
begin
  FDefaultDB := 'main';
  FDefaultFit := dtMSSQL;

  FAutoReconnect := True;
  SetLength(FTableBuilders, 0);
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

  gDBManager := gMG.FDBManager;
  //����ȫ�ֱ���
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
function TDBManager.AddTable(const nTable: string; const nList: TList;
  nDBType: TDBType): PDBTable;
begin
  Result := FindTable(nTable, nList);
  if not Assigned(Result) then
  begin
    New(Result);
    nList.Add(Result);
    Result.FName := nTable;

    SetLength(Result.FFields, 0);
    SetLength(Result.FRecords, 0);
    SetLength(Result.FIndexes, 0);
    SetLength(Result.FTriggers, 0);
    SetLength(Result.FProcedures, 0);

    SetLength(Result.FSameTbs, 1);
    Result.FSameTbs[0] := nTable;
    //first table in list
  end;

  if nDBType = dtDefault then
    nDBType := FDefaultFit;
  Result.FDefaultFit := nDBType;
end;

//Date: 2020-04-16
//Parm: �б�
//Desc: ��ȡϵͳ����Ϣ
procedure TDBManager.GetTables(const nList: TList);
var nIdx: Integer;
begin
  ClearTables(nList, False);
  //init first

  for nIdx := Low(FTableBuilders) to High(FTableBuilders) do
    FTableBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2020-04-16
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nList����Ϣ
procedure TDBManager.ClearTables(const nList: TList; const nFree: Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx := nList.Count - 1 downto 0 do
      Dispose(PDBTable(nList[nIdx]));
    //xxxxx

    if nFree then
         nList.Free
    else nList.Clear;
  end;
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
procedure TDBManager.AddTableBuilder(const nBuilder: TDBTableBuilder);
var nIdx: Integer;
begin
  for nIdx := Low(FTableBuilders) to High(FTableBuilders) do
    if @FTableBuilders[nIdx] = @nBuilder then Exit;
  //has exists

  nIdx := Length(FTableBuilders);
  SetLength(FTableBuilders, nIdx + 1);
  FTableBuilders[nIdx] := nBuilder;
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
//Date: 2020-04-20
//Parm: ���ݿ��ʶ
//Desc: ��ʼ��nDB���ݿ�
function TDBManager.InitDB(const nDB: string; const nMemo: TStrings): Boolean;
var nList: TList;
    nCfg: TDBConnConfig;
begin
  Result := False;
  if Assigned(nMemo) then
    nMemo.Clear;
  //xxxxx

  if not GetDB(nDB, nCfg) then
  begin
    WriteLog(Format('���ݿ�[ %s ]������,��������.', [nDB]), nMemo);
    Exit;
  end;

  nList := gMG.FObjectPool.Lock(TList) as TList;
  try
    GetTables(nList);
    //get database data

    try
      case nCfg.FFitDB of
       dtMSSQL   : Result := InitMSSQL(nDB, nList, nMemo);   //SQLServer
       dtAccess  : Result := InitAccess(nDB, nList, nMemo);  //Access
       dtPostgre : Result := InitPostgre(nDB, nList, nMemo); //Postgre
      end;
    except
      on nErr: Exception do
      begin
        WriteLog(nErr.Message, nMemo);
      end;
    end;
  finally
    ClearTables(nList);
    gMG.FObjectPool.Release(nList);
  end;
end;

//Date: 2020-04-20
//Parm: ���ݿ��ʶ;����;��ע��Ϣ
//Desc: ��ʼ��SQLServer���ݿ�nDB
function TDBManager.InitMSSQL(const nDB: string; const nTables: TList;
  const nMemo: TStrings): Boolean;
var nStr: string;
    i,j,k,nIdx,nInt: Integer;
    nTable: PDBTable;
    nQuery: TADOQuery;
    nListA,nListB,nListC: TStrings;
begin
  Result := True;
  nQuery := nil;

  nListA := nil;
  nListB := nil;
  nListC := nil;
  try
    nListA := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListA.Clear;
    nListB := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListB.Clear;
    nListC := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListC.Clear;

    WriteLog('::: �������ݱ� :::', nMemo);
    nStr := 'Select so.name as tName From sysobjects so ' +
            'Where xtype=''U''';
    //query all user tables

    nQuery := LockDBQuery(nDB);
    with DBQuery(nStr, nQuery) do
    begin
      First;
      while not Eof do
      begin
        nListC.Add(FieldByName('tName').AsString);
        Next;
      end;
    end; //enum all user tables

    for nIdx := nTables.Count -1 downto 0 do
    begin
      nInt := 0;
      nTable := nTables[nIdx];
      for k := Low(nTable.FSameTbs) to High(nTable.FSameTbs) do
      begin
        if nListC.IndexOf(nTable.FSameTbs[k]) < 0 then
             Inc(nInt)
        else WriteLog('�Ѵ���: ' + nTable.FSameTbs[k], nMemo);
      end;
      
      if nInt < 1 then Continue; //all tables exists
      nInt := 0;
      nStr := '';

      for i := Low(nTable.FFields) to High(nTable.FFields) do
       with nTable.FFields[i] do
        for j := Low(FType) to High(FType) do
         if FType[j].FFitDB = dtMSSQL then
         begin
           if nInt = 0 then
                nStr := Format('%s %s', [FName, FType[j].FData])
           else nStr := nStr + Format(',%s %s', [FName, FType[j].FData]);

           Inc(nInt);
           Break;
         end;
      //comine all fields

      if nInt < 1 then
      begin
        WriteLog('���ֶ�: ' + nTable.FName, nMemo);
        Continue;
      end;

      for k := Low(nTable.FSameTbs) to High(nTable.FSameTbs) do
      if nListC.IndexOf(nTable.FSameTbs[k]) < 0 then //not exists
      begin
        nListB.Add(Format('Create Table %s(%s)', [nTable.FSameTbs[k], nStr]));
        WriteLog('�Ѵ���: ' + nTable.FSameTbs[k], nMemo);
        //new table

        for i := Low(nTable.FRecords) to High(nTable.FRecords) do
        with nTable.FRecords[i] do
        begin
          if FFitDB <> dtMSSQL then Continue;
          //not fit

          if FParmB then //ƥ�������
          begin
            with TStringHelper do
             nListB.Add(MacroValue(FData, [MI(sDBTables, nTable.FSameTbs[k])]));
            //�滻������
          end else

          if k = 0 then
          begin
            nListB.Add(FData);
            //������ʱ����һ��
          end;
        end; //init table records
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: �޸��ֶ� :::', nMemo);
    nStr := 'Select so.name tName,sc.name fName From sysobjects so ' +
            ' Inner Join syscolumns sc On sc.id=so.id ' +
            'Where so.xtype=''U''';
    //query all table fields

    with DBQuery(nStr, nQuery) do
    begin
      nListA.Clear;
      First;

      while not Eof do
      begin
        nStr := FieldByName('tName').AsString + '.' +  //table
                FieldByName('fName').AsString;         //field
        nListA.Add(nStr);
        Next;
      end;
    end; //enum all table fields

    for nIdx := nTables.Count -1 downto 0 do
    begin
      nTable := nTables[nIdx];
      //table item
      for k := Low(nTable.FSameTbs) to High(nTable.FSameTbs) do
      begin
        if nListC.IndexOf(nTable.FSameTbs[k]) < 0 then Continue;
        //new table will create soon

        for i := Low(nTable.FFields) to High(nTable.FFields) do
        with nTable.FFields[i] do
        begin
          if nListA.IndexOf(nTable.FSameTbs[k] + '.' + FName) >= 0 then Continue;
          //field exists
          nInt := 0;

          for j := Low(FType) to High(FType) do
          if FType[j].FFitDB = dtMSSQL then
          begin
            nStr := Format('Alter Table %s Add %s %s', [nTable.FSameTbs[k],
              FName, FType[j].FData]);
            nListB.Add(nStr);

            WriteLog(Format('���޸�: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Inc(nInt);
            Break;
          end;

          if nInt < 1 then
          begin
            WriteLog(Format('���ֶ�: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
          end;
        end;
      end;
    end;

    if nListB.Count > 0 then
    begin
      DBExecute(nListB, nil, nDB);
      //���ȱ�֤����ֶ�����
      nListB.Clear;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: �޸�Ĭ��ֵ :::', nMemo);
    nStr := 'Select so.name tName,sc.name fName From dbo.sysobjects so ' +
            ' Inner Join dbo.syscolumns sc On sc.id=so.id ' +
            ' Inner Join dbo.syscomments sm On sm.id=sc.cdefault ' +
            'Where so.xtype = ''U''';
    //query all default values

    with DBQuery(nStr, nQuery) do
    begin
      nListA.Clear;
      First;

      while not Eof do
      begin
        nStr := FieldByName('tName').AsString + '.' +  //table
                FieldByName('fName').AsString;         //field
        nListA.Add(nStr);
        Next;
      end;
    end; //enum all default values

    for nIdx := nTables.Count -1 downto 0 do
    begin
      nTable := nTables[nIdx];
      for i := Low(nTable.FFields) to High(nTable.FFields) do
      with nTable.FFields[i] do
      begin
        if FDefVal = '' then Continue;
        //no default value

        for k := Low(nTable.FSameTbs) to High(nTable.FSameTbs) do
        begin
          if nListA.IndexOf(nTable.FSameTbs[k] + '.' + FName) >= 0 then
          begin
            WriteLog(Format('�Ѵ���: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Continue;
          end;

          nStr := 'Alter Table %s Add Default(%s) For %s';
          nStr := Format(nStr, [nTable.FSameTbs[k], FDefVal, FName]);
          nListB.Add(nStr); //����Ĭ��ֵ

          nStr := 'Update %s Set %s=%s Where %s Is Null';
          nStr := Format(nStr, [nTable.FSameTbs[k], FName, FDefVal, FName]);
          nListB.Add(nStr); //���¾����ݵ�Ĭ��ֵ
          WriteLog(Format('���޸�: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
        end;
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: �������� :::', nMemo);
    nStr := 'Select t.name as tName,i.name as iName From sys.indexes i ' +
            ' Inner Join sys.objects t On t.object_id=i.object_id ' +
            'Where t.is_ms_shipped<>1 and i.index_id > 0';
    //query all indexes

    with DBQuery(nStr, nQuery) do
    begin
      nListA.Clear;
      First;

      while not Eof do
      begin
        nStr := FieldByName('tName').AsString + '.' +
                FieldByName('iName').AsString;
        nListA.Add(nStr);
        Next;
      end;
    end; //enum all indexes

    for nIdx := nTables.Count -1 downto 0 do
    begin
      nTable := nTables[nIdx];
      for i := Low(nTable.FIndexes) to High(nTable.FIndexes) do
      with nTable.FIndexes[i] do
      begin
        for k := Low(nTable.FSameTbs) to High(nTable.FSameTbs) do
        begin
          if nListA.IndexOf(nTable.FSameTbs[k] + '.' + FName) >= 0 then
          begin
            WriteLog(Format('�Ѵ���: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Continue;
          end;

          if FFitDB <> dtMSSQL then Continue;
          //not fit

          if FParmB then //ƥ�������
          begin
            with TStringHelper do
             nListB.Add(MacroValue(FData, [MI(sDBTables, nTable.FSameTbs[k])]));
            //�滻������
            WriteLog(Format('�Ѵ���: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
          end else

          if k = 0 then
          begin
            nListB.Add(FData);
            WriteLog(Format('�Ѵ���: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            //������ʱ����һ��
          end;
        end;
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: ���������� :::', nMemo);
    nStr := 'Select t2.name as tName,t1.name as trName From sys.triggers t1 ' +
            'Inner Join sys.tables t2 On t2.object_id= t1.parent_id ' +
            'Where t1.type=''TR''';
    //query all triggers

    with DBQuery(nStr, nQuery) do
    begin
      nListA.Clear;
      First;

      while not Eof do
      begin
        nStr := FieldByName('tName').AsString + '.' +
                FieldByName('trName').AsString;
        nListA.Add(nStr);
        Next;
      end;
    end; //enum all triggers

    for nIdx := nTables.Count -1 downto 0 do
    begin
      nTable := nTables[nIdx];
      for i := Low(nTable.FTriggers) to High(nTable.FTriggers) do
      with nTable.FTriggers[i] do
      begin
        for k := Low(nTable.FSameTbs) to High(nTable.FSameTbs) do
        begin
          if nListA.IndexOf(nTable.FSameTbs[k] + '.' + FName) >= 0 then
          begin
            WriteLog(Format('�Ѵ���: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Continue;
          end;

          if FFitDB <> dtMSSQL then Continue;
          //not fit

          if FParmB then //ƥ�������
          begin
            with TStringHelper do
             nListB.Add(MacroValue(FData, [MI(sDBTables, nTable.FSameTbs[k])]));
            //�滻������
            WriteLog(Format('�Ѵ���: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
          end else

          if k = 0 then
          begin
            nListB.Add(FData);
            WriteLog(Format('�Ѵ���: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            //������ʱ����һ��
          end;
        end;
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: �����洢���� :::', nMemo);
    nStr := 'Select so.object_id as fID,so.name as fName From sys.objects so ' +
            'Where is_ms_shipped<>1 and so.type=''P''';
    //query all procedure

    with DBQuery(nStr, nQuery) do
    begin
      nListA.Clear;
      First;

      while not Eof do
      begin
        nListA.Add(FieldByName('fName').AsString);
        Next;
      end;
    end; //enum all procedure

    for nIdx := nTables.Count -1 downto 0 do
    begin
      nTable := nTables[nIdx];
      for i := Low(nTable.FProcedures) to High(nTable.FProcedures) do
      with nTable.FProcedures[i] do
      begin
        if nListA.IndexOf(FName) >= 0 then
        begin
          WriteLog(Format('�Ѵ���: %s.%s', [nTable.FName, FName]), nMemo);
          Continue;
        end;

        if FFitDB = dtMSSQL then
        begin
          nListB.Add(FData);
          WriteLog(Format('�Ѵ���: %s.%s', [nTable.FName, FName]), nMemo);
        end;
      end;
    end;

    if nListB.Count > 0 then
      DBExecute(nListB, nil, nDB);
    //init db
  finally
    gMG.FObjectPool.Release(nListA);
    gMG.FObjectPool.Release(nListB);
    gMG.FObjectPool.Release(nListC);
    ReleaseDBQuery(nQuery);
  end;
end;

function TDBManager.InitAccess(const nDB: string; const nTables: TList;
  const nMemo: TStrings): Boolean;
begin
  Result := False;
  //to do
end;

function TDBManager.InitPostgre(const nDB: string; const nTables: TList;
  const nMemo: TStrings): Boolean;
begin
  Result := False;
  //to do
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
