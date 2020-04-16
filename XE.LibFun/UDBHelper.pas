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
  System.Classes, System.SysUtils, System.Generics.Collections;

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
    FID   : string;                             //���ӱ�ʶ
    FName : string;                             //��������
    FConn : string;                             //�����ַ���
  end;

  PDBConnData = ^TDBConnData;
  TDBConnData = record
    FConnUser  : string;                        //�û����������ַ���
    FConnStr   : string;                        //ϵͳ��Ч�����ַ���
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
    FDefaultDB: TDBType;
    {*Ĭ�����ݿ�*}
    FDBConfig: TDictionary<string, TDBConnConfig>;
    {*�����ֵ�*}
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
    procedure AddDB(const nConfig: TDBConnConfig);
    {*������ݿ�*}
    procedure GetTables(const nList: TList);
    procedure ClearTables(const nList: TList; const nFree: Boolean=True);
    {*��ȡ����Ϣ*}
    function GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
    {*��ȡ���ݿ�*}
  end;

implementation

constructor TDBHelper.Create;
begin
  FDefaultDB := dtMySQL;
  FDBConfig := TDictionary<string, TDBConnConfig>.Create();
end;

destructor TDBHelper.Destroy;
begin
  FDBConfig.Free;
  inherited;
end;

//Date: 2020-04-16
//Parm: ������;�б�
//Desc: ����nList������ΪnTable�ı�
function TDBHelper.FindTable(const nTable: string; const nList: TList): PDBTable;
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
procedure TDBHelper.AddDB(const nConfig: TDBConnConfig);
begin
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
    nDBType := FManager.FDefaultDB;
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
function TDBTable.AddI(const nName,nIndex: string;
  nDBType: TDBType): PDBTable;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FManager.FDefaultDB;
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
function TDBTable.AddT(const nName, nTrigger: string;
  nDBType: TDBType): PDBTable;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if nDBType = dtDefault then
    nDBType := FManager.FDefaultDB;
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

end.
