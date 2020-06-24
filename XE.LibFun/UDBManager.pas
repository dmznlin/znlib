{*******************************************************************************
  作者: dmzn@163.com 2020-04-16
  描述: 数据库管理器及连接池

  备注:
  *.表结构包括:字段、索引、默认值、触发器、存储过程,管理器自动创建不存在的内容,
    达到系统和数据库的适配.
  *.管理器支持不同数据库的差异,新增字段、索引、触发器时明确适配数据库类型.

  *.使用方法:
    1.编写数据库描述函数:
      procedure SystemTables(const nList: TList);
      begin
        gMG.FDBManager.AddTable('Sys_Dict', nList).              //连写模式
        AddF('D_ID',     'varChar(15)',        '记录标识').      //普通字段
        AddF('D_Port',   'Integer',            '端口', '80').    //默认值1
        AddF('D_Serial', 'Integer Default -1', '装置号', '-1').  //默认值2
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
        //初始化数据,建表时运行一次
      end;
    2.将描述函数加入管理器:
      gMG.FDBManager.AddTableBuilder(SystemTables);
    3.使用管理器初始化数据库:
      gMG.FDBManager.InitDB();

  *.结构相同的多张表,处理方法如下:
    1.TDBTable.Copy: 复制表结构.
    2.TDBTable.Add方法中,若SQL语句含有 cDBTables 常量,则所有表都会执行.
*******************************************************************************}
unit UDBManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Data.Win.ADODB,
  Data.DB, UBaseObject;

const
  //任意表标识
  sDBTables  = '$TB.*';
  sDBIndex   = '$IDX';

  //小数字段
  sField_Access_Decimal          = 'Float';
  sField_SQLServer_Decimal       = 'Decimal(15, 5)';

  //图片字段
  sField_Access_Image            = 'OLEObject';
  sField_SQLServer_Image         = 'Image';

  //日期相关
  sField_SQLServer_Now           = 'getDate()';

  //自增字段
  sField_Access_AutoInc          = 'Counter';
  sField_SQLServer_AutoInc       = 'Integer IDENTITY (1,1) PRIMARY KEY';

  //常用标记
  sFlag_Yes                      = 'Y';         //是
  sFlag_No                       = 'N';         //否
  sFlag_Unknow                   = 'U';         //未知
  sFlag_Enabled                  = 'Y';         //启用
  sFlag_Disabled                 = 'N';         //禁用

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
    FID    : string;                            //连接标识
    FName  : string;                            //连接名称
    FConn  : string;                            //连接字符串
    FFitDB : TDBType;                           //适配数据库
  end;

  PDBConnData = ^TDBConnData;
  TDBConnData = record
    FConnID    : string;                        //连接标识
    FConnected : Boolean;                       //连接状态
    FConneLast : Int64;                         //上次活动
    FInThread  : Cardinal;                      //所在线程
  end;

  PDBData = ^TDBData;
  TDBData = record
    FName  : string;                            //数据名称
    FData  : string;                            //数据内容
    FFitDB : TDBType;                           //适配数据库
    FParmI : Integer;                           //整形参数
    FParmB : Boolean;                           //布尔参数
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
    FDefaultFit : TDBType;                      //适配(默认)
    FName       : string;                       //表名称
    FFields     : array of TDBField;            //表字段
    FIndexes    : array of TDBData;             //表索引
    FTriggers   : array of TDBData;             //触发器
    FProcedures : array of TDBData;             //存储过程
    FRecords    : array of TDBData;             //默认记录
    FSameTbs    : array of string;              //同结构表
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
    function AddP(const nName,nProcedure: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*增加触发器*}
    function AddR(const nName,nRecord: string;
      nDBType: TDBType = dtDefault): PDBTable;
    {*增加初始化记录*}
    function Copy(const nTable: string): PDBTable;
    {*复制表结构*}
  end;

  TDBTableBuilder = procedure (const nList: TList);
  //for external-system fill database.table info

  TDBManager = class(TManagerBase)
  private
    FDefaultFit: TDBType;
    {*默认适配类型*}
    FDefaultDB: string;
    {*默认数据库标识*}
    FAutoReconnect: Boolean;
    {*数据库自动重连*}
    FTableBuilders: array of TDBTableBuilder;
    {*数据表配置信息*}
    FDBConfig: TDictionary<string, TDBConnConfig>;
    {*数据库配置字典*}
  protected
    procedure RegObjectPoolTypes;
    {*注册对象*}
    function FindTable(const nTable: string; const nList: TList): PDBTable;
    {*检索数据*}
    function InitAccess(const nDB: string; const nTables: TList;
      const nMemo: TStrings): Boolean;
    function InitMSSQL(const nDB: string; const nTables: TList;
      const nMemo: TStrings): Boolean;
    function InitPostgre(const nDB: string; const nTables: TList;
      const nMemo: TStrings): Boolean;
    {*初始化数据库*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    function InitDB(const nDB: string; const nMemo: TStrings = nil): Boolean;
    {*初始化数据库*}
    procedure AddTableBuilder(const nBuilder: TDBTableBuilder);
    procedure AddDB(nConfig: TDBConnConfig);
    function AddTable(const nTable: string; const nList: TList;
      nDBType: TDBType = dtDefault): PDBTable;
    {*添加数据*}
    procedure GetTables(const nList: TList);
    procedure ClearTables(const nList: TList; const nFree: Boolean = False);
    {*获取表信息*}
    function GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
    {*获取数据库*}
    function LockDBConn(nDB: string = ''): TADOConnection;
    procedure ReleaseDBConn(const nConn: TADOConnection);
    function CheckDBConn(nDB: string = ''): string;
    {*数据库链路*}
    function LockDBQuery(const nDB: string = ''): TADOQuery;
    procedure ReleaseDBQuery(const nQuery: TADOQuery;
      const nResetConn: Boolean = False);
    {*数据库对象*}
    function DBQuery(const nSQL: string; const nQuery: TADOQuery;
      const nDB: string = ''; const nLockBookmark: Boolean = False): TDataSet;
    function DBExecute(const nSQL: string; const nCmd: TADOQuery = nil;
      const nDB: string = ''): Integer; overload;
    function DBExecute(const nList: TStrings; const nCmd: TADOQuery = nil;
      const nDB: string = ''): Integer; overload;
    {*数据库操作*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
    property DefaultDB: string read FDefaultDB write FDefaultDB;
    property DefaultFit: TDBType read FDefaultFit write FDefaultFit;
    property AutoReconnect: Boolean read FAutoReconnect write FAutoReconnect;
    {*属性相关*}
  end;

var
  gDBManager: TDBManager = nil;
  //全局使用

implementation

uses
  ULibFun, UManagerGroup;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TDBManager, '数据库链路', nEvent);
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
//Parm: 索引名;索引数据;适配数据库
//Desc: 新增一个适配nDBType的表索引
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
//Parm: 触发器名;触发器数据;适配数据库
//Desc: 新增一个适配nDBType的表触发器
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
//Parm: 存储过程名;存储过程脚本;适配数据库
//Desc: 新增一个适配nDBType的存储过程
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
//Parm: SQL脚本;适配数据库
//Desc: 新增一个适配nDBType的SQL
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
//Parm: 表名称
//Desc: 依据当前表结构创建nTable表
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

//Parm: 是否注册
//Desc: 向系统注册管理器对象
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
  //启用全局变量
end;

//Date: 2020-04-17
//Desc: 注册对象池
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
//Parm: 表名称;列表
//Desc: 检索nList中名称为nTable的表
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
//Parm: 表名称;列表
//Desc: 添加一个表
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
//Parm: 列表
//Desc: 获取系统表信息
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
//Parm: 列表;是否释放
//Desc: 清理nList表信息
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
//Parm: 数据库配置
//Desc: 新增数据库配置项
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
//Parm: 数据库标识;配置
//Desc: 获取标识为nID的信息
function TDBManager.GetDB(const nID: string; var nConfig: TDBConnConfig): Boolean;
begin
  Result := FDBConfig.TryGetValue(nID, nConfig);
end;

//Date: 2020-04-18
//Parm: 配置方法
//Desc: 新增数据配置方法
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
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList
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
//Parm: 数据库标识
//Desc: 初始化nDB数据库
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
    WriteLog(Format('数据库[ %s ]不存在,请先配置.', [nDB]), nMemo);
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
//Parm: 数据库标识;数据;备注信息
//Desc: 初始化SQLServer数据库nDB
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

    WriteLog('::: 创建数据表 :::', nMemo);
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
        else WriteLog('已存在: ' + nTable.FSameTbs[k], nMemo);
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
        WriteLog('无字段: ' + nTable.FName, nMemo);
        Continue;
      end;

      for k := Low(nTable.FSameTbs) to High(nTable.FSameTbs) do
      if nListC.IndexOf(nTable.FSameTbs[k]) < 0 then //not exists
      begin
        nListB.Add(Format('Create Table %s(%s)', [nTable.FSameTbs[k], nStr]));
        WriteLog('已创建: ' + nTable.FSameTbs[k], nMemo);
        //new table

        for i := Low(nTable.FRecords) to High(nTable.FRecords) do
        with nTable.FRecords[i] do
        begin
          if FFitDB <> dtMSSQL then Continue;
          //not fit

          if FParmB then //匹配任意表
          begin
            with TStringHelper do
             nListB.Add(MacroValue(FData, [MI(sDBTables, nTable.FSameTbs[k])]));
            //替换表名称
          end else

          if k = 0 then
          begin
            nListB.Add(FData);
            //主表创建时运行一次
          end;
        end; //init table records
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: 修复字段 :::', nMemo);
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

            WriteLog(Format('已修复: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Inc(nInt);
            Break;
          end;

          if nInt < 1 then
          begin
            WriteLog(Format('空字段: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
          end;
        end;
      end;
    end;

    if nListB.Count > 0 then
    begin
      DBExecute(nListB, nil, nDB);
      //优先保证表和字段完整
      nListB.Clear;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: 修复默认值 :::', nMemo);
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
            WriteLog(Format('已存在: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Continue;
          end;

          nStr := 'Alter Table %s Add Default(%s) For %s';
          nStr := Format(nStr, [nTable.FSameTbs[k], FDefVal, FName]);
          nListB.Add(nStr); //增加默认值

          nStr := 'Update %s Set %s=%s Where %s Is Null';
          nStr := Format(nStr, [nTable.FSameTbs[k], FName, FDefVal, FName]);
          nListB.Add(nStr); //更新旧数据的默认值
          WriteLog(Format('已修复: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
        end;
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: 创建索引 :::', nMemo);
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
            WriteLog(Format('已存在: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Continue;
          end;

          if FFitDB <> dtMSSQL then Continue;
          //not fit

          if FParmB then //匹配任意表
          begin
            with TStringHelper do
             nListB.Add(MacroValue(FData, [MI(sDBTables, nTable.FSameTbs[k])]));
            //替换表名称
            WriteLog(Format('已创建: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
          end else

          if k = 0 then
          begin
            nListB.Add(FData);
            WriteLog(Format('已创建: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            //主表创建时运行一次
          end;
        end;
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: 创建触发器 :::', nMemo);
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
            WriteLog(Format('已存在: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            Continue;
          end;

          if FFitDB <> dtMSSQL then Continue;
          //not fit

          if FParmB then //匹配任意表
          begin
            with TStringHelper do
             nListB.Add(MacroValue(FData, [MI(sDBTables, nTable.FSameTbs[k])]));
            //替换表名称
            WriteLog(Format('已创建: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
          end else

          if k = 0 then
          begin
            nListB.Add(FData);
            WriteLog(Format('已创建: %s.%s', [nTable.FSameTbs[k], FName]), nMemo);
            //主表创建时运行一次
          end;
        end;
      end;
    end;

    //--------------------------------------------------------------------------
    WriteLog('::: 创建存储过程 :::', nMemo);
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
          WriteLog(Format('已存在: %s.%s', [nTable.FName, FName]), nMemo);
          Continue;
        end;

        if FFitDB = dtMSSQL then
        begin
          nListB.Add(FData);
          WriteLog(Format('已创建: %s.%s', [nTable.FName, FName]), nMemo);
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
//Parm: 数据库标识
//Desc: 获取nDB的连接对象
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
    nStr := Format('数据库[ %s ]不存在,请先配置.', [nDB]);
    raise Exception.Create(nStr);
  end;

  nInThread := TThread.Current.ThreadID;
  //调用者所在线程

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
        //同线程同连接
      end else

      if nTimes = 2 then
      begin
        Result := (not nUsed) and (not Assigned(nConn)) or (nConn.FConnID = nDB);
        //同库连接
      end else
      begin
        Result := (not nUsed) and ((not Assigned(nConn)) or
          (TDateTimeHelper.GetTickCountDiff(nConn.FConneLast) > 60 * 1000) or
          (not TADOConnection(nObj).Connected));
        //空闲连接
      end;

      if nTimes = 1 then
        nTimes := 3;
      //三轮扫描
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
//Parm: 连接对象
//Desc: 释放链路
procedure TDBManager.ReleaseDBConn(const nConn: TADOConnection);
begin
  if Assigned(nConn) then
  begin
    gMG.FObjectPool.Release(nConn);
  end;
end;

//Date: 2020-04-17
//Parm: 数据库标识
//Desc: 检测nConn是否正常
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
//Parm: 数据库标识
//Desc: 获取nDB数据的Query对象
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
//Parm: 对象;重置
//Desc: 释放nQuery对象
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
//Parm: SQL;查询对象;锁定书签
//Desc: 在nQuery上执行查询
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
//Parm: SQL;对象;数据库标识
//Desc: 在nDB上执行写入操作
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
//Parm: 列表;对象;数据库标识
//Desc: 在nDB上批量执行nList写操作
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
