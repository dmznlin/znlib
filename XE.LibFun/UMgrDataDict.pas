{*******************************************************************************
  作者: dmzn@163.com 2021-06-08
  描述: 数据字典管理器

  备注:
  &.数据字典主要用于初始化ListView,cxGrid等数据表格,字典管理中维护了一组与之
    相关的配置数据.
  &.字典分两级管理: 程序模块,模块下多个实体,每个实体对应一组数据项.
  &.字典管理器使用ProgID属性,来标识当前所有实体所归属的程序.
  &.字典数据由数据库加载,即用即请求,管理器会缓存,所以每个实体数据只加载一次.
  &.读取时调用LoadEntity,若成功则该实体会被激活,直接读取ActiveEntity就可以了.
*******************************************************************************}
unit UMgrDataDict;

interface

uses
  System.Classes, System.SysUtils, Data.DB, UBaseObject;

type
  TDictFormatStyle = (fsNone, fsFixed, fsSQL, fsCheckBox);
  //格式化方式: 固定数据,数据库数据

  PDictFormatItem = ^TDictFormatItem;
  TDictFormatItem = record
    FStyle    : TDictFormatStyle;                       //方式
    FData     : string;                                 //数据
    FFormat   : string;                                 //格式化
    FExtMemo  : string;                                 //扩展数据
  end;

  PDictDBItem = ^TDictDBItem;
  TDictDBItem = record
    FTable    : string;                                 //表名
    FField    : string;                                 //字段
    FIsKey    : Boolean;                                //主键

    FType     : TFieldType;                             //数据类型
    FWidth    : integer;                                //字段宽度
    FDecimal  : integer;                                //小数位
  end;

  TDictFooterKind = (fkNone, fkSum, fkMin, fkMax, fkCount, fkAverage);
  //统计类型: 无,合计,最小,最大,数目,平均值
  TDictFooterPosition = (fpNone, fpFooter, fpGroup, fpAll);
  //合计位置: 页脚,分组,两者都有

  PDictGroupFooter = ^TDictGroupFooter;
  TDictGroupFooter = record
    FDisplay  : string;                                 //显示文本
    FFormat   : string;                                 //格式化
    FKind     : TDictFooterKind;                        //合计类型
    FPosition : TDictFooterPosition;                    //合计位置
  end;

  PDictItem = ^TDictItem;
  TDictItem = record
    FRecordID : string;                                //记录号
    FTitle    : string;                                 //标题
    FAlign    : TAlignment;                             //对齐
    FWidth    : integer;                                //宽度
    FIndex    : integer;                                //顺序
    FVisible  : Boolean;                                //可见
    FDBItem   : TDictDBItem;                            //数据库
    FFormat   : TDictFormatItem;                        //格式化
    FFooter   : TDictGroupFooter;                       //页脚合计
  end;
  TDictItems = array of TDictItem;

  PEntityItem = ^TEntityItem;
  TEntityItem = record
  private
    FEntity   : string;                                 //实体标记
    FName     : string;                                 //实体名称
    FLang     : string;                                 //语言标识
    FItems    : TDictItems;                             //字典数据(PDictItem)
  public
    function AddDict(const nTitle,nField: string): PEntityItem;
    {*添加字典项*}
  end;

  TDictItemBuilder = procedure (const nList: TList);
  //for external-system fill entity.items info

  TDataDictManager = class(TManagerBase)
  public
    const
      sTable_DataDict = 'Sys_DataDict';                 //数据字典
  private
    FBuilders: array of TDictItemBuilder;
    {*字典配置信息*}
  protected
    function FindEntity(const nEntity,nLang: string;
      const nList: TList): PEntityItem;
    {*检索数据*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    {*延迟执行*}
    procedure AddDictBuilder(const nBuilder: TDictItemBuilder);
    function AddEntity(const nEntity,nName,nLang: string;
      const nList: TList): PEntityItem;
    {*添加数据*}
    procedure GetDictData(const nList: TList);
    procedure ClearDictData(const nList: TList; const nFree: Boolean = False);
    {*字典数据*}
    function BuilDictSQL(const nEntity: PEntityItem;const nIdx:Integer): string;
    procedure InitDict(const nEntity: PEntityItem;
      const nFirstItem: Boolean = True);
    procedure AddDict(const nEntity: PEntityItem; const nIdx: Integer = 0);
    procedure DelDict(const nEntity: PEntityItem; const nIdx: Integer = 0);
    {*字典项*}
    procedure GetEntity(const nEntity,nLang: string; const nData: PEntityItem);
    {*字典数据*}
    function InitDictData(const nLang: string;
      const nMemo: TStrings = nil): Boolean;
    {*初始化数据*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
  end;

var
  gDataDictManager: TDataDictManager = nil;
  //全局使用
  
implementation

uses
  UManagerGroup, UDBManager, ULibFun;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TDataDictManager, '字典管理器', nEvent);
end;

//Desc: 添加管理器所需表
procedure AddDataDictTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TDataDictManager do
  begin
    AddTable(sTable_DataDict, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '记录标识').
      AddF('D_Entity',      'varchar(32)',            '所属实体').
      AddF('D_Title',       'varchar(32)',            '数据标题').
      AddF('D_Align',       'smallint',               '标题对齐').
      AddF('D_Width',       'integer',                '标题宽度').
      AddF('D_Index',       'integer',                '标题顺序').
      AddF('D_Visible',     'smallint',               '是否可见').
      AddF('D_LangID',      'varchar(5)',             '语言标识').
      //normal
      AddF('D_DBTable',     'varchar(32)',            '表名称').
      AddF('D_DBField',     'varchar(32)',            '字段名').
      AddF('D_DBIsKey',     'smallint',               '是否主键').
      AddF('D_DBType',      'smallint',               '数据类型').
      AddF('D_DBWidth',     'smallint',               '字段宽度').
      AddF('D_DBDecimal',   'smallint',               '小数位').
      //database
      AddF('D_FmtStyle',    'smallint',               '格式化方式').
      AddF('D_FmtData',     'varchar(200)',           '格式化数据').
      AddF('D_FmtFormat',   'varchar(100)',           '格式化内容').
      AddF('D_FmtExtMemo',  'varchar(100)',           '格式化扩展').
      //format
      AddF('D_FteDisplay',  'varChar(50)',            '统计显示文本').
      AddF('D_FteFormat',   'varChar(50)',            '统计格式化').
      AddF('D_FteKind',     'smallint',               '统计类型').
      AddF('D_FtePositon',  'smallint',               '统计显示位置').
      //footer
      AddI('idx_entity', 'CREATE INDEX $IDX ON $TBS(D_Entity ASC,D_LangID ASC)');
      //for index;
  end;
end;

//Date: 2021-06-17
//Parm: 标题;字段
//Desc: 添加字典项
function TEntityItem.AddDict(const nTitle, nField: string): PEntityItem;
var nStr: string;
    nIdx: Integer;
    nInit: TDictItem;
begin
  Result := @Self;
  //return self address

  for nIdx := Low(FItems) to High(FItems) do
   if (CompareText(nTitle, FItems[nIdx].FTitle) = 0) and
      (CompareText(nField, FItems[nIdx].FDBItem.FField) = 0) then
   begin
     nStr := 'TDataDictManager.AddDict: %s.%s Has Exists.';
     nStr := Format(nStr, [nField, nTitle]);

     WriteLog(nStr);
     raise Exception.Create(nStr);
   end;

  FillChar(nInit, SizeOf(TDictItem), #0);
  //default item

  nIdx := Length(FItems);
  SetLength(FItems, nIdx + 1);
  FItems[nIdx] := nInit;

  with FItems[nIdx] do
  begin
    FVisible := True;
    FTitle := nTitle;
    FDBItem.FField := nField;
  end;
end;

//------------------------------------------------------------------------------
constructor TDataDictManager.Create;
begin
  inherited;
  SetLength(FBuilders, 0);
end;

destructor TDataDictManager.Destroy;
begin
  //call RunBeforUnregistAllManager
  inherited;
end;

//Date: 2021-06-08
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TDataDictManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TDataDictManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TDataDictManager.Create;
    gMG.FDataDictManager := gMG.FManagers[nIdx].FManager as TDataDictManager;
  end else
  begin
    gMG.FDataDictManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gDataDictManager := gMG.FDataDictManager;
  //启用全局变量
end;

procedure TDataDictManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TDataDictManager', ['TDBManager']);
  //检查支持
  gDBManager.AddTableBuilder(AddDataDictTables);
end;

//Date: 2021-06-17
//Parm: 配置方法
//Desc: 新增字典配置方法
procedure TDataDictManager.AddDictBuilder(const nBuilder: TDictItemBuilder);
var nIdx: Integer;
begin
  for nIdx := Low(FBuilders) to High(FBuilders) do
    if @FBuilders[nIdx] = @nBuilder then Exit;
  //has exists

  nIdx := Length(FBuilders);
  SetLength(FBuilders, nIdx + 1);
  FBuilders[nIdx] := nBuilder;
end;

//Date: 2021-06-17
//Parm: 列表
//Desc: 获取字典数据
procedure TDataDictManager.GetDictData(const nList: TList);
var nIdx: Integer;
begin
  nList.Clear;
  //init first

  for nIdx := Low(FBuilders) to High(FBuilders) do
    FBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2021-06-17
//Parm: 列表;是否释放
//Desc: 清理nList字典数据
procedure TDataDictManager.ClearDictData(const nList:TList;const nFree:Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx := nList.Count - 1 downto 0 do
      Dispose(PEntityItem(nList[nIdx]));
    //xxxxx

    if nFree then
         nList.Free
    else nList.Clear;
  end;
end;

//Date: 2021-06-17
//Parm: 实体名;语言标识;列表
//Desc: 在nList中检索nEntity.nLang实体
function TDataDictManager.FindEntity(const nEntity,nLang: string;
  const nList: TList): PEntityItem;
var nIdx: Integer;
    nEty: PEntityItem;
begin
  Result := nil;
  for nIdx := nList.Count - 1 downto 0 do
  begin
    nEty := nList[nIdx];
    if (CompareText(nEntity, nEty.FEntity) = 0) and
       (CompareText(nLang, nEty.FLang) = 0) then
    begin
      Result := nEty;
      Break;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: 实体标识;名称;语言
//Desc: 在nList中增加//标识为nEntity字典实体
function TDataDictManager.AddEntity(const nEntity, nName, nLang: string;
  const nList: TList): PEntityItem;
begin
  Result := FindEntity(nEntity, nLang, nList);
  if not Assigned(Result) then
  begin
    New(Result);
    nList.Add(Result);

    InitDict(Result, False);
    with Result^ do
    begin
      FEntity := nEntity;
      FName   := nName;
      FLang   := nLang;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: 实体;字典项索引
//Desc: 构建nEntity.FItems[nIdx]的insert,update语句
function TDataDictManager.BuilDictSQL(const nEntity: PEntityItem;
  const nIdx: Integer): string;
var nStr: string;
begin
  if (nIdx < Low(nEntity.FItems)) or (nIdx > High(nEntity.FItems)) then
  begin
    nStr := 'TDataDictManager.BuilItemSQL: Index Out of Bounds.';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  with nEntity.FItems[nIdx], TSQLBuilder do
  begin
    Result := MakeSQLByStr([
      SF('D_Entity', nEntity.FEntity),
      SF('D_Title', nEntity.FName),
      SF('D_Align', Ord(FAlign), sfVal),
      SF('D_Width', FWidth, sfVal),
      SF('D_Index', FIndex, sfVal),
      SF('D_Visible', BoolToStr(FVisible), sfVal),
      SF('D_LangID', nEntity.FLang),
      //normal
      SF('D_DBTable', FDBItem.FTable),
      SF('D_DBField', FDBItem.FField),
      SF('D_DBIsKey', BoolToStr(FDBItem.FIsKey), sfVal),
      SF('D_DBType', Ord(FDBItem.FType), sfVal),
      SF('D_DBWidth', FDBItem.FWidth, sfVal),
      SF('D_DBDecimal', FDBItem.FDecimal, sfVal),
      //database
      SF('D_FmtStyle', Ord(FFormat.FStyle), sfVal),
      SF('D_FmtData', FFormat.FData),
      SF('D_FmtFormat', FFormat.FFormat),
      SF('D_FmtExtMemo', FFormat.FExtMemo),
      //format
      SF('D_FteDisplay', FFooter.FDisplay),
      SF('D_FteFormat', FFooter.FFormat),
      SF('D_FteKind', Ord(FFooter.FKind), sfVal),
      SF('D_FtePositon', Ord(FFooter.FPosition), sfVal)
      //footer
    ], sTable_DataDict, SF('R_ID', FRecordID, sfVal), FRecordID = '');
  end;
end;

//Date: 2021-06-18
//Parm: 语言;输出
//Desc: 初始化数据库中的字典数据
function TDataDictManager.InitDictData(const nLang: string;
  const nMemo: TStrings): Boolean;
begin

end;

//Date: 2021-06-17
//Parm: 实体;首个字典项
//Desc: 初始化nEntity和默认字典项
procedure TDataDictManager.InitDict(const nEntity: PEntityItem;
  const nFirstItem: Boolean);
var nEty: TEntityItem;
    nDict: TDictItem;
begin
  FillChar(nEty, SizeOf(TEntityItem), #0);
  nEntity^ := nEty;

  if nFirstItem then
  begin
    FillChar(nDict, SizeOf(TDictItem), #0);
    with nDict do
    begin
      FVisible := True;
      FIndex   := -1;
    end;

    SetLength(nEntity.FItems, 0);
    nEntity.FItems[0] := nDict;
  end;
end;

//Date: 2021-06-17
//Parm: 字典项
//Desc: 保存字典项
procedure TDataDictManager.AddDict(const nEntity: PEntityItem;
  const nIdx: Integer);
var nStr: string;
    nQuery: TDataSet;
begin
  if (nEntity.FEntity = '') or (nEntity.FLang = '') then
  begin
    nStr := 'TDataDictManager.AddItem: Entity or Lang Is Null';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if (nIdx < Low(nEntity.FItems)) or (nIdx > High(nEntity.FItems)) then
  begin
    nStr := 'TDataDictManager.AddItem: Index Out of Bounds.';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if nEntity.FItems[nIdx].FRecordID <> '' then
  begin
    nStr := BuilDictSQL(nEntity, nIdx);
    gDBManager.DBExecute(nStr);
    Exit;
  end;

  nQuery := nil;
  try
    if nEntity.FItems[nIdx].FIndex < 0 then
    begin
      nStr := 'Select Top 1 D_Index From %s ' +
              'Where D_Entity=''%s'' And D_LangID=''%s'' ' +
              'Order By D_Index DESC';
      nStr := Format(nStr, [sTable_DataDict, nEntity.FEntity, nEntity.FLang]);

      nQuery := gDBManager.DBQuery(nStr);
      if nQuery.RecordCount < 1 then
           nEntity.FItems[nIdx].FIndex := 0
      else nEntity.FItems[nIdx].FIndex := nQuery.Fields[0].AsInteger + 1;
    end;

    nStr := BuilDictSQL(nEntity, nIdx);
    gDBManager.DBExecute(nStr, nQuery);
  finally
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-06-17
//Parm: 字典项
//Desc: 删除字典项
procedure TDataDictManager.DelDict(const nEntity: PEntityItem;
 const nIdx: Integer);
var nStr: string;
begin
  if (nIdx < Low(nEntity.FItems)) or (nIdx > High(nEntity.FItems)) then
  begin
    nStr := 'TDataDictManager.DelItem: Index Out of Bounds.';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if nEntity.FItems[nIdx].FRecordID <> '' then
  begin
    nStr := 'Delete From %s Where R_ID=%s';
    nStr := Format(nStr, [sTable_DataDict, nEntity.FItems[nIdx].FRecordID]);
    gDBManager.DBExecute(nStr);
  end;
end;

//Date: 2021-06-18
//Parm: 实体;语言;数据
//Desc: 载入nEntity.nLang的字典数据
procedure TDataDictManager.GetEntity(const nEntity,nLang: string;
  const nData: PEntityItem);
var nStr: string;
    nIdx: Integer;
    nQuery: TDataSet;
    nDefItem: TDictItem;
begin
  InitDict(nData, False);
  //init
  nQuery := nil;

  with TApplicationHelper,TStringHelper do
  try
    nStr := 'Select * From %s ' +
            'Where D_Entity=''%s'' And D_LangID=''%s'' ' +
            'Order By D_Index ASC';
    nStr := Format(nStr, [sTable_DataDict, nEntity, nLang]);

    nQuery := gDBManager.DBQuery(nStr);
    with nQuery do
    begin
      if RecordCount < 1 then Exit;
      //no data
      SetLength(nData.FItems, RecordCount);

      FillChar(nDefItem, SizeOf(TDictItem), #0);
      nIdx := 0;
      First;

      nData.FEntity := FieldByName('D_Entity').AsString;
      nData.FName   := FieldByName('D_Title').AsString;
      nData.FLang   := FieldByName('D_LangID').AsString;

      while not Eof do
      begin
        nData.FItems[nIdx] := nDefItem;
        //init

        with nData.FItems[nIdx] do
        begin
          FRecordID := FieldByName('').AsString;
          FTitle    := FieldByName('').AsString;
          FAlign    := TAlignment(FieldByName('').AsInteger);
          FWidth    := FieldByName('').AsInteger;
          FIndex    := FieldByName('').AsInteger;
          FVisible  := StrToBool(FieldByName('').AsString);
          //normal
          FDBItem.FTable := FieldByName('D_DBTable').AsString;
          FDBItem.FField := FieldByName('D_DBField').AsString;
          FDBItem.FIsKey := StrToBool(FieldByName('D_DBIsKey').AsString);
          FDBItem.FType  := TFieldType(FieldByName('D_DBType').AsInteger);
          FDBItem.FWidth := FieldByName('D_DBWidth').AsInteger;
          FDBItem.FDecimal := FieldByName('D_DBDecimal').AsInteger;
          //database
          FFormat.FStyle := TDictFormatStyle(FieldByName('D_FmtStyle').AsInteger);
          FFormat.FData  := FieldByName('D_FmtData').AsString;
          FFormat.FFormat:= FieldByName('D_FmtFormat').AsString;
          FFormat.FExtMemo := FieldByName('D_FmtExtMemo').AsString;
          //format
          FFooter.FDisplay := FieldByName('D_FteDisplay').AsString;
          FFooter.FFormat := FieldByName('D_FteFormat').AsString;
          FFooter.FKind := TDictFooterKind(FieldByName('D_FteKind').AsInteger);
          FFooter.FPosition := TDictFooterPosition(FieldByName('D_FtePositon').AsInteger);
          //footer
        end;

        Inc(nIdx);
        Next;
      end;
    end;
  finally
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-06-08
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList
procedure TDataDictManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
begin
    with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      //nList.Add('NumAll=' + FStatus.FNumAll.ToString);
      Exit;
    end;

    //nList.Add(FixData('NumAll:', FStatus.FNumAll));
  finally
    SyncLeave;
  end;
end;

end.
