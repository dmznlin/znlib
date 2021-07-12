{*******************************************************************************
  作者: dmzn@163.com 2021-06-08
  描述: 数据字典管理器

  使用方法:
  1.添加Builder
    procedure SytemDictBuilder(const nList: TList);
    var nEty: PDictEntity;
    begin
      nEty := gDataDictManager.AddEntity('Main_A01', '操作日志', nList);
      nEty.AddDict('R_ID',        '标识').
           AddDict('L_Name',      '名称').
           AddDict('L_Owner',     '拥有人').
           AddDict('L_Index',     '顺序');
      //添加字典项

      with nEty.ByField('R_ID').FFooter do
      begin
        FDisplay  := 'total:';
        FFormat   := '合计: 共 0 条';
        FKind     := fkCount;
        FPosition := fpAll;
      end; //扩展字典项
    end;

    gDataDictManager.AddDictBuilder(SytemDictBuilder);
    //添加至管理器
    
  2.初始化字典项
    gDataDictManager.InitDictData('cn');  //中文字典项
    gDataDictManager.InitDictData('en');  //英文字典项

  3.加载字典项
    var nEntity: TDictEntity;
    gDataDictManager.GetEntity('Main_A01', 'cn', @nEntity);
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
    FRecordID : string;                                 //记录号
    FTitle    : string;                                 //标题
    FAlign    : TAlignment;                             //对齐
    FWidth    : integer;                                //宽度
    FIndex    : integer;                                //顺序
    FVisible  : Boolean;                                //可见
    FDBItem   : TDictDBItem;                            //数据库
    FFormat   : TDictFormatItem;                        //格式化
    FFooter   : TDictGroupFooter;                       //页脚合计
  public
    procedure Init();
    {*初始化*}
  end;
  TDictItems = array of TDictItem;

  PDictEntity = ^TDictEntity;
  TDictEntity = record
    FEntity   : string;                                 //实体标记
    FName     : string;                                 //实体名称
    FLang     : string;                                 //语言标识
    FItems    : TDictItems;                             //字典数据(PDictItem)
  public
    procedure Init(const nFirstItem: Boolean = False);
    {*初始化*}
    function AddDict(const nField,nTitle: string): PDictEntity;
    {*添加字典项*}    
    function ByTitle(const nTitle: string): PDictItem;
    function ByField(const nField: string): PDictItem;
    function FindItem(const nData: string; const nMode: Byte): PDictItem;
    {*检索字典项*} 
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
      const nList: TList): PDictEntity;
    {*检索数据*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    {*延迟执行*}
    procedure AddDictBuilder(const nBuilder: TDictItemBuilder;
      const nIdx: Integer = -1);
    function AddEntity(const nEntity,nName: string;
      const nList: TList): PDictEntity;
    procedure GetEntity(const nEntity,nLang: string; const nData: PDictEntity);
    {*字典数据*}
    procedure GetDictData(const nList: TList);
    procedure ClearDictData(const nList: TList; const nFree: Boolean = False);
    {*字典数据*}
    function BuilDictSQL(const nEntity: PDictEntity;const nIdx:Integer): string;
    procedure AddDict(const nEntity: PDictEntity; const nIdx: Integer = 0);
    procedure DelDict(const nEntity: PDictEntity; const nIdx: Integer = 0);
    {*字典项*}
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
      AddF('D_Entity',      'varchar(32)',            '实体标识').
      AddF('D_Name',        'varchar(32)',            '实体名称').
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

//Date: 2021-07-12
//Desc: 初始化字典项
procedure TDictItem.Init();
var nInit: TDictItem;
begin
  FillChar(nInit, SizeOf(TDictItem), #0);
  Self := nInit;

  FVisible := True;
  FIndex   := -1;
end;

//------------------------------------------------------------------------------
//Date: 2021-07-12
//Parm: 首个字典项
//Desc: 初始化实体和默认字典项
procedure TDictEntity.Init(const nFirstItem: Boolean);
var nInit: TDictEntity;
begin
  FillChar(nInit, SizeOf(TDictEntity), #0);
  Self := nInit;

  if nFirstItem then
  begin
    SetLength(FItems, 1);
    FItems[0].Init;
  end;
end;

//Date: 2021-06-17
//Parm: 字段;标题
//Desc: 添加字典项
function TDictEntity.AddDict(const nField,nTitle: string): PDictEntity;
var nStr: string;
    nIdx: Integer;
begin
  Result := @Self;
  //return self address

  for nIdx := Low(FItems) to High(FItems) do
   if (CompareText(nTitle, FItems[nIdx].FTitle) = 0) and
      (CompareText(nField, FItems[nIdx].FDBItem.FField) = 0) then
   begin
     nStr := 'TDataDictManager.AddDict: %s.%s.%s.%s Has Exists.';
     nStr := Format(nStr, [FEntity, FName, nField, nTitle]);

     WriteLog(nStr);
     raise Exception.Create(nStr);
   end;

  nIdx := Length(FItems);
  SetLength(FItems, nIdx + 1);
  FItems[nIdx].Init;

  with FItems[nIdx] do
  begin
    FIndex := -1;
    FVisible := True;
    FTitle := nTitle;
    FDBItem.FField := nField;
  end;
end;

//Date: 2021-06-21
//Parm: 数据;模式
//Desc: 依据nMode检索字典项
function TDictEntity.FindItem(const nData: string; const nMode: Byte): PDictItem;
var nIdx: Integer;
begin
  Result := nil;
  for nIdx := Low(FItems) to High(FItems) do
  begin
    if nMode = 1 then //title
    begin
      if CompareText(nData, FItems[nIdx].FTitle) = 0 then
      begin
        Result := @FItems[nIdx];
        Break;      
      end;
    end else

    if nMode = 2 then //field
    begin
      if CompareText(nData, FItems[nIdx].FDBItem.FField) = 0 then
      begin
        Result := @FItems[nIdx];
        Break;      
      end;
    end;
  end;
end;

//Date: 2021-06-21
//Parm: 标题
//Desc: 检索标题是nTitle的字典项
function TDictEntity.ByTitle(const nTitle: string): PDictItem;
begin
  Result := FindItem(nTitle, 1);
end;

//Date: 2021-06-21
//Parm: 字段
//Desc: 检索字段为nField的字典项
function TDictEntity.ByField(const nField: string): PDictItem;
begin
  Result := FindItem(nField, 2);
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
//Parm: 配置方法;顺序
//Desc: 新增字典配置方法
procedure TDataDictManager.AddDictBuilder(const nBuilder: TDictItemBuilder;
  const nIdx: Integer);
var i: Integer;
begin
  for i := Low(FBuilders) to High(FBuilders) do
    if @FBuilders[i] = @nBuilder then Exit;
  //has exists

  i := Length(FBuilders);
  SetLength(FBuilders, i + 1);
  FBuilders[i] := nBuilder;

  if (nIdx > -1) and (nIdx < i) then
  begin
    FBuilders[i] := FBuilders[nIdx]; //save old
    FBuilders[nIdx] := nBuilder;     //make new
  end;
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
      Dispose(PDictEntity(nList[nIdx]));
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
  const nList: TList): PDictEntity;
var nIdx: Integer;
    nEty: PDictEntity;
begin
  Result := nil;
  for nIdx := nList.Count - 1 downto 0 do
  begin
    nEty := nList[nIdx];
    if (CompareText(nEntity, nEty.FEntity) = 0) and (
       (nLang = '') or (CompareText(nLang, nEty.FLang) = 0)) then
    begin
      Result := nEty;
      Break;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: 实体标识;名称;语言
//Desc: 在nList中增加//标识为nEntity字典实体
function TDataDictManager.AddEntity(const nEntity, nName: string;
  const nList: TList): PDictEntity;
begin
  Result := FindEntity(nEntity, '', nList);
  if not Assigned(Result) then
  begin
    New(Result);
    nList.Add(Result);
    Result.Init(False);

    with Result^ do
    begin
      FEntity := nEntity;
      FName   := nName;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: 实体;字典项索引
//Desc: 构建nEntity.FItems[nIdx]的insert,update语句
function TDataDictManager.BuilDictSQL(const nEntity: PDictEntity;
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
    if FWidth < 1 then
      FWidth := 120;
    //default width

    Result := MakeSQLByStr([
      SF('D_Entity', nEntity.FEntity),
      SF('D_Name', nEntity.FName),
      SF('D_Title', FTitle),
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
var nStr: string;
    i,nIdx: Integer;
    nDicts: TList;
    nListA,nListB: TStrings;

    nQuery: TDataSet;
    nEntity: PDictEntity;
begin
  Result := False;
  if Assigned(nMemo) then
    nMemo.Clear;
  //xxxxx

  nListA := nil;
  nListB := nil;
  nDicts := nil;
  nQuery := nil; //init

  with gDBManager do
  try
    nQuery := LockDBQuery();

    nListA := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListA.Clear;
    nListB := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListB.Clear;

    nStr := 'Select D_Entity,D_LangID,D_Title,D_DBField From %s ' +
            'Where D_LangID=''%s''';
    nStr := Format(nStr, [sTable_DataDict, nLang]);

    with DBQuery(nStr, nQuery) do
    if RecordCount > 0 then
    begin
      First;
      while not Eof do
      begin
        nStr := FieldByName('D_Entity').AsString + '.' +
                FieldByName('D_LangID').AsString + '.' +
                FieldByName('D_DBField').AsString + '.' +
                FieldByName('D_Title').AsString;
        nListA.Add(nStr);
        Next;
      end;
    end;

    nDicts := gMG.FObjectPool.Lock(TList) as TList;
    GetDictData(nDicts);
    //get dict data

    UMgrDataDict.WriteLog('::: 创建字典数据 :::', nMemo);
    for nIdx := 0 to nDicts.Count -1 do
    begin
      nEntity := nDicts[nIdx];
      nEntity.FLang := nLang;

      UMgrDataDict.WriteLog('创建实体: ' + nEntity.FEntity + '.' +
                                           nEntity.FName, nMemo);
      //xxxxx

      for i := Low(nEntity.FItems) to High(nEntity.FItems) do
      with nEntity.FItems[i],TSQLBuilder,TStringHelper do
      begin
        nStr := nEntity.FEntity + '.' + nLang + '.' +
                FDBItem.FField + '.' + FTitle;
        //xxxxx

        if nListA.IndexOf(nStr) < 0 then
        begin
          if FIndex < 0 then          
            FIndex := i;
          //any property

          nListB.Add(BuilDictSQL(nEntity, i));
          UMgrDataDict.WriteLog('已创建: ' + nStr, nMemo);
        end;
      end;
    end;

    if nListB.Count > 0 then
      DBExecute(nListB);
    //save
  finally
    gMG.FObjectPool.Release(nListA);
    gMG.FObjectPool.Release(nListB);
    gDBManager.ReleaseDBQuery(nQuery);

    ClearDictData(nDicts);
    gMG.FObjectPool.Release(nDicts);
    //menu list
  end;
end;

//Date: 2021-06-17
//Parm: 字典项
//Desc: 保存字典项
procedure TDataDictManager.AddDict(const nEntity: PDictEntity;
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
procedure TDataDictManager.DelDict(const nEntity: PDictEntity;
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
  const nData: PDictEntity);
var nStr: string;
    nIdx: Integer;
    nQuery: TDataSet;
begin
  nData.Init(False);
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
      nIdx := 0;
      First;

      nData.FEntity := FieldByName('D_Entity').AsString;
      nData.FName   := FieldByName('D_Name').AsString;
      nData.FLang   := FieldByName('D_LangID').AsString;

      while not Eof do
      begin
        nData.FItems[nIdx].Init;
        //init

        with nData.FItems[nIdx] do
        begin
          FRecordID := FieldByName('R_ID').AsString;
          FTitle    := FieldByName('D_Title').AsString;
          FAlign    := TAlignment(FieldByName('D_Align').AsInteger);
          FWidth    := FieldByName('D_Width').AsInteger;
          FIndex    := FieldByName('D_Index').AsInteger;
          FVisible  := StrToBool(FieldByName('D_Visible').AsString);
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
var nIdx,nInt: Integer;
    nDicts: TList;
    nEntity: PDictEntity;
begin
  nDicts := nil;
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    nDicts := gMG.FObjectPool.Lock(TList) as TList;
    GetDictData(nDicts);
    //get dict data

    nInt := 0;
    for nIdx := nDicts.Count-1 downto 0 do
    begin
      nEntity := nDicts[nIdx];
      nInt := nInt + Length(nEntity.FItems);
    end;

    if not nFriendly then
    begin
      nList.Add('NumBuilder=' + Length(FBuilders).ToString);
      nList.Add('NumEntity=' + nDicts.Count.ToString);
      nList.Add('NumDictItem=' + nInt.ToString);
      Exit;
    end;

    nList.Add(FixData('NumBuilder:', Length(FBuilders).ToString));
    nList.Add(FixData('NumEntity:', nDicts.Count.ToString));
    nList.Add(FixData('NumDictItem:', nInt.ToString));

    for nIdx := 0 to nDicts.Count-1 do
    begin
      nEntity := nDicts[nIdx];
      nList.Add(FixData(Format('EntityItem %d:', [nIdx+1]),
        nEntity.FEntity + '.' + nEntity.FName));
      //xxxxx
    end;
  finally
    SyncLeave;
    ClearDictData(nDicts);
    gMG.FObjectPool.Release(nDicts);
  end;
end;

end.
