{*******************************************************************************
  作者: dmzn@163.com 2020-04-16
  描述: 数据库管理器及连接池

  备注:
  *.表结构包括:字段、索引、默认值、触发器,管理器自动创建不存在的内容,达到
    系统和数据库的适配.
  *.管理器支持不同数据库的差异,新增字段、索引、触发器时明确适配数据库类型.
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
    FID   : string;                             //连接标识
    FName : string;                             //连接名称
    FConn : string;                             //连接字符串
  end;

  PDBConnData = ^TDBConnData;
  TDBConnData = record
    FConnUser  : string;                        //用户设置连接字符串
    FConnStr   : string;                        //系统有效连接字符串
    FConnected : Boolean;                       //连接状态
    FConneLast : Int64;                         //上次活动
  end;

  PDBData = ^TDBData;
  TDBData = record
    FName  : string;                            //数据名称
    FData  : string;                            //数据内容
    FFitDB : TDBType;                           //适配数据库
  end;

  PDBField = ^TDBField;
  TDBField = record
    FName   : string;                           //字段名称 Ex: L_Man
    FType   : array of TDBData;                 //数据类型 Ex: varChar(32)
    FMemo   : string;                           //字段描述 Ex: Operator Name
    FDefVal : string;                           //默认值
  end;

  PDBTable = ^TDBTable;
  TDBTable = record
    FManager  : TDBHelper;                      //所属管理器
    FName     : string;                         //表名称
    FFields   : array of TDBField;                //表字段
    FIndexes  : array of TDBData;               //表索引
    FTriggers : array of TDBData;               //触发器
    {*表属性*}
    function AddF(const nField,nType,nMemo: string;
      const nDefVal: string = '';
      nDBType: TDBType = dtDefault): PDBTable;
    {*增加字段*}
    function AddI(const nName,nIndex: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*增加索引*}
    function AddT(const nName,nTrigger: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*增加触发器*}
  end;

  TDBHelper = class
  protected
    FDefaultDB: TDBType;
    {*默认数据库*}
    FDBConfig: TDictionary<string, TDBConnConfig>;
    {*配置字典*}
    procedure AddSystemTables(const nList: TList); virtual; abstract;
    procedure AddSystemIndexes(const nList: TList); virtual;
    procedure AddSystemTriggers(const nList: TList); virtual;
    function AddTable(const nTable: string; const nList: TList): PDBTable;
    {*添加数据*}
    function FindTable(const nTable: string; const nList: TList): PDBTable;
    {*检索数据*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    procedure AddDB(const nConfig: TDBConnConfig);
    {*添加数据库*}
    procedure GetTables(const nList: TList);
    procedure ClearTables(const nList: TList; const nFree: Boolean=True);
    {*获取表信息*}
    function GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
    {*获取数据库*}
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
//Parm: 表名称;列表
//Desc: 检索nList中名称为nTable的表
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
//Parm: 表名称;列表
//Desc: 添加一个表
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
//Desc: 添加数据库索引
procedure TDBHelper.AddSystemIndexes(const nList: TList);
begin
  //for sub-type
end;

//Date: 2020-04-16
//Desc: 添加数据表触发器
procedure TDBHelper.AddSystemTriggers(const nList: TList);
begin
  //for sub-type
end;

//Date: 2020-04-16
//Parm: 列表
//Desc: 获取系统表信息
procedure TDBHelper.GetTables(const nList: TList);
begin
  ClearTables(nList, False);
  //init first

  AddSystemTables(nList);
  AddSystemIndexes(nList);
  AddSystemTriggers(nList);
end;

//Date: 2020-04-16
//Parm: 列表;是否释放
//Desc: 清理nList表信息
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
//Parm: 数据库配置
//Desc: 新增数据库配置项
procedure TDBHelper.AddDB(const nConfig: TDBConnConfig);
begin
  FDBConfig.Add(nConfig.FID, nConfig);
end;

//Date: 2020-04-16
//Parm: 数据库标识;配置
//Desc: 获取标识为nID的信息
function TDBHelper.GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
begin
  Result := FDBConfig.TryGetValue(nID, nConfig);
end;

//------------------------------------------------------------------------------
//Date: 2020-04-16
//Parm: 字段名;字段类型;字段描述;默认值;适配数据库
//Desc: 新增一个适配nDBType数据库的表字段
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
//Parm: 索引名;索引数据;适配数据库
//Desc: 新增一个适配nDBType的表索引
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
//Parm: 触发器名;触发器数据;适配数据库
//Desc: 新增一个适配nDBType的表触发器
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
