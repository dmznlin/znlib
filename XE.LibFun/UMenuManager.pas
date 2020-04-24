{*******************************************************************************
  作者: dmzn@163.com 2020-04-21
  描述: 菜单管理器

  备注:
  *.多语言支持.
  *.代码内维护菜单项,保证系统菜单完整.原来使用菜单编辑器的方法,会导致部署差异.
  *.程序ProgID: 用于区分菜单所属的系统.
  *.实体Entity: 用于区分同系统内的不同菜单,包括主菜单和快捷菜单.
*******************************************************************************}
unit UMenuManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Data.Win.ADODB,
  UBaseObject;

type
  TMenuItemType = (mtDefault, mtProg, mtEntity, mtParent, mtItem, mtAssist);
  //menu item

const
  sMenuTypeID: array[TMenuItemType] of string = ('df', 'pg', 'et', 'pr',
    'mn', 'as');
  //menu type id

  sMenuTypeText: array[TMenuItemType] of string = ('默认', '程序', '实体',
    '父菜单', '菜单项', '辅助项');
  //menu type desc

  sMenuItemDefault = '#*#';
  //default flag

type
  TMenuData = record
    FID           : string;                              //标识
    FName         : string;                              //名称
    FData         : string;                              //数据
    FFlag         : string;                              //附加参数
  end;
  TMenuDataList = array of TMenuData;                    //数据列表

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    FType         : TMenuItemType;                       //菜单类型
    FProgID       : string;                              //程序标识
    FEntity       : string;                              //实体标识
    FMenuID       : string;                              //菜单标识
    FPMenu        : string;                              //上级菜单
    FTitle        : string;                              //菜单标题
    FImgIndex     : integer;                             //图标索引
    FFlag         : string;                              //附加参数(下划线..)
    FAction       : string;                              //菜单动作
    FFilter       : string;                              //过滤条件
    FLang         : string;                              //语言标识
    FNewOrder     : Single;                              //创建序列
  end;

  TMenuItems = array of TMenuItem;                       //菜单列表

  PMenuEntity = ^TMenuEntity;
  TMenuEntity = record
  private
    FDefaultPMenu : string;                              //默认上级
    FDefaultType  : TMenuItemType;                       //默认类型
    {*辅助信息*}
    FProgID       : string;                              //程序标识
    FEntity       : string;                              //实体标识
    FItems        : TMenuItems;                          //菜单项
    {*菜单属性*}
  public
    function SetParent(const nPMenu: string): PMenuEntity;
    function SetType(const nType: TMenuItemType): PMenuEntity;
    {*设置属性*}
    function AddM(const nID,nTitle: string; const nAction: string = '';
      nPMenu: string = sMenuItemDefault;
      const nFilter: string = ''; nType: TMenuItemType = mtDefault): PMenuEntity;
    {*添加菜单*}
  end;

  TMenuItemBuilder = procedure (const nList: TList);
  //for external-system fill menu.item info

  TMenuManager = class(TManagerBase)
  private
    FMultiLang: TMenuDataList;
    {*多语言列表*}
    FMenuBuilders: array of TMenuItemBuilder;
    {*菜单配置信息*}
  protected
    function FindEntity(const nProg,nEntity: string; const nList: TList): PMenuEntity;
    {*检索数据*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    {*延迟执行*}
    procedure AddLanguage(const nID,nName: string);
    procedure DeleteLanguage(const nID: string);
    procedure LoadLanguage(const nQuery: TADOQuery = nil);
    {*语言管理*}
    procedure AddMenuBuilder(const nBuilder: TMenuItemBuilder);
    function AddEntity(const nProg,nEntity: string; const nList: TList): PMenuEntity;
    {*添加数据*}
    procedure GetMenus(const nList: TList);
    procedure ClearMenus(const nList: TList; const nFree: Boolean = False);
    {*获取表信息*}
    function InitMenus(const nMemo: TStrings = nil): Boolean;
    {*初始化数据*}
    property MultiLanguage: TMenuDataList read FMultiLang;
    {*属性相关*}
  end;

var
  gMenuManager: TMenuManager = nil;
  //全局使用

implementation

uses
  ULibFun, UManagerGroup, UDBManager;

const
  sTableLang        = 'Sys_Lang';         //多语言配置
  sTableMenu        = 'Sys_Menus';        //菜单配置

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TMenuManager, '菜单管理器', nEvent);
end;

//Desc: 添加管理器所需表
procedure AddMenuTables(const nList: TList);
begin
  with gMG.FDBManager,TSQLBuilder do
  AddTable(sTableLang, nList, dtMSSQL).
  AddF('L_ID',          'varChar(5)',             '语言标识').
  AddF('L_Name',        'varChar(32)',            '语言名称').
  AddF('L_Valid',       'Char(1)',                '是否有效', '''Y''').
  AddR('en', MakeSQLByStr([
    SF('L_ID', 'en'),
    SF('L_Name', 'English'),
    SF('L_Valid', sFlag_Disabled)], sTableLang)).
  AddR('cn', MakeSQLByStr([
    SF('L_ID', 'cn'),
    SF('L_Name', '简体中文')], sTableLang));
  //for language

  gMG.FDBManager.AddTable(sTableMenu, nList, dtMSSQL).
  AddF('R_ID',          sField_SQLServer_AutoInc, '记录标识').
  AddF('M_Prog',        'varchar(15)',            '程序标识').
  AddF('M_Entity',      'varchar(15)',            '实体标识').
  AddF('M_PMenu',       'varchar(15)',            '上级菜单').
  AddF('M_MenuID',      'varchar(15)',            '菜单标识').
  AddF('M_Title',       'varchar(50)',            '菜单标题').
  AddF('M_Action',      'varchar(100)',           '菜单动作').
  AddF('M_Flag',        'varchar(20)',            '附加参数').
  AddF('M_Filter',      'varchar(100)',           '过滤条件').
  AddF('M_Type',        'varchar(5)',             '类型标识').
  AddF('M_Lang',        'varchar(5)',             '语言标识').
  AddF('M_ImgIndex',    'integer default -1',     '图标索引', '-1').
  AddF('M_NewOrder',    'float default 0',        '创建序列', '0').
  //for field
  AddI('idx_prog', 'CREATE INDEX idx_prog ON $TB.* (M_Prog ASC)');
  //for index
end;

//------------------------------------------------------------------------------
//Date: 2020-04-24
//Parm: 菜单标识
//Desc: 设置当前默认父级菜单
function TMenuEntity.SetParent(const nPMenu: string): PMenuEntity;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if nPMenu = '' then
  begin
    FDefaultPMenu := nPMenu;
    Exit;
  end;

  for nIdx := Low(FItems) to High(FItems) do
   with FItems[nIdx] do
    if CompareText(nPMenu, FMenuID) = 0 then
    begin
      FDefaultPMenu := nPMenu;
      Exit;
    end;

  raise Exception.Create(Format('SetParent: 父菜单 %s 不存在.', [nPMenu]));
end;

//Date: 2020-04-24
//Parm: 菜单类型
//Desc: 设置默认类型
function TMenuEntity.SetType(const nType: TMenuItemType): PMenuEntity;
begin
  Result := @Self;
  //return self address

  if nType = mtDefault then
       FDefaultType := mtItem
  else FDefaultType := nType;
end;

//Date: 2020-04-22
//Parm: 菜单标识;菜单标题;父菜单
//Desc: 新增菜单项
function TMenuEntity.AddM(const nID, nTitle, nAction: string; nPMenu: string;
  const nFilter: string; nType: TMenuItemType): PMenuEntity;
var nIdx,nInt: Integer;
begin
  Result := @Self;
  //return self address

  if nPMenu = sMenuItemDefault then
    nPMenu := FDefaultPMenu;
  //xxxxx

  if nType = mtDefault then
    nType := FDefaultType;
  nInt := -1;

  for nIdx := Low(FItems) to High(FItems) do
   with FItems[nIdx] do
    if CompareText(nID, FMenuID) = 0 then
    begin
      nInt := nIdx;
      Break;
    end; //same entity,same menuid

  if nInt < 0 then
  begin
    nInt := Length(FItems);
    SetLength(FItems, nInt + 1);
  end; //new menu item

  with FItems[nInt] do
  begin
    FType         := nType;
    FProgID       := FProgID;
    FEntity       := FEntity;

    FMenuID       := nID;
    FPMenu        := nPMenu;
    FTitle        := nTitle;
    FAction       := nAction;
    FFilter       := nFilter;

    FFlag         := '';
    FLang         := '';
    FNewOrder     := 0;
    FImgIndex     := -1;
  end;
end;

//------------------------------------------------------------------------------
constructor TMenuManager.Create;
begin
  SetLength(FMultiLang, 0);
  SetLength(FMenuBuilders, 0);
end;

destructor TMenuManager.Destroy;
begin

  inherited;
end;

//Date: 2020-04-21
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TMenuManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TMenuManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TMenuManager.Create;
    gMG.FMenuManager := gMG.FManagers[nIdx].FManager as TMenuManager;
  end else
  begin
    gMG.FMenuManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gMenuManager := gMG.FMenuManager;
  //启用全局变量
end;

procedure TMenuManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TMenuManager', ['TDBManager']);
  //检查支持
  gMG.FDBManager.AddTableBuilder(AddMenuTables);
end;

//Date: 2020-04-21
//Parm: 语言标识;语言名称;
//Desc: 添加多语言项
procedure TMenuManager.AddLanguage(const nID, nName: string);
var nStr: string;
    nQuery: TADOQuery;
begin
  nQuery := nil;
  try
    with gMG.FDBManager, TSQLBuilder do
    begin
      nStr := 'Select L_ID From %s Where L_ID=''%s''';
      nStr := Format(nStr, [sTableLang, nID]);
      nQuery := LockDBQuery();

      nStr := MakeSQLByStr([
        SF('L_ID', nID),
        SF('L_Name', nName),
        SF('L_Valid', sFlag_Enabled)
      ], sTableLang, SF('L_ID', nID), DBQuery(nStr, nQuery).RecordCount < 1);
      DBExecute(nStr);
    end;
  finally
    gMG.FDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2020-04-24
//Parm: 语言标识
//Desc: 删除标识为nID的语言
procedure TMenuManager.DeleteLanguage(const nID: string);
var nStr: string;
    nList: TStrings;
begin
  nList := gMG.FObjectPool.Lock(TStrings) as TStrings;
  try
    nStr := 'Delete From %s Where L_ID=''%s''';
    nStr := Format(nStr, [sTableLang, nID]);
    nList.Add(nStr);

    nStr := 'Delete From %s Where M_Lang=''%s''';
    nStr := Format(nStr, [sTableMenu, nID]);
    nList.Add(nStr);

    gMG.FDBManager.DBExecute(nList);
    //action
  finally
    gMG.FObjectPool.Release(nList);
  end;
end;

//Date: 2020-04-24
//Desc: 载入多语言列表
procedure TMenuManager.LoadLanguage(const nQuery: TADOQuery);
var nStr: string;
    nIdx: Integer;
    nQry: TADOQuery;
begin
  nQry := nil;
  with gMG.FDBManager do
  try
    if Assigned(nQuery) then
         nQry := nQuery
    else nQry := LockDBQuery();

    nStr := 'Select L_ID,L_Name From %s Where L_Valid=''%s''';
    nStr := Format(nStr, [sTableLang, sFlag_Enabled]);

    with DBQuery(nStr, nQry) do
    begin
      SetLength(FMultiLang, RecordCount);
      nIdx := 0;
      First;

      while not Eof do
      begin
        with FMultiLang[nIdx] do
        begin
          FID   := FieldByName('L_ID').AsString;
          FName := FieldByName('L_Name').AsString;
        end;

        Inc(nIdx);
        Next;
      end;
    end;
  finally
    if not Assigned(nQuery) then
      ReleaseDBQuery(nQry);
    //xxxxx
  end;
end;

//Date: 2020-04-21
//Parm: 配置方法
//Desc: 新增菜单项配置方法
procedure TMenuManager.AddMenuBuilder(const nBuilder: TMenuItemBuilder);
var nIdx: Integer;
begin
  for nIdx := Low(FMenuBuilders) to High(FMenuBuilders) do
    if @FMenuBuilders[nIdx] = @nBuilder then Exit;
  //has exists

  nIdx := Length(FMenuBuilders);
  SetLength(FMenuBuilders, nIdx + 1);
  FMenuBuilders[nIdx] := nBuilder;
end;

//Date: 2020-04-22
//Parm: 程序标识;实体标识;列表
//Desc: 在nList中检索nProg.nEntity实体
function TMenuManager.FindEntity(const nProg, nEntity: string;
  const nList: TList): PMenuEntity;
var nIdx: Integer;
    nME: PMenuEntity;
begin
  Result := nil;
  for nIdx := nList.Count - 1 downto 0 do
  begin
    nME := nList[nIdx];
    if (CompareText(nProg, nME.FProgID) = 0) and
       (CompareText(nEntity, nME.FEntity) = 0) then
    begin
      Result := nME;
      Break;
    end;
  end;
end;

//Date: 2020-04-22
//Parm: 程序标识;实体标识;列表
//Desc: 在nList中//新增nProg.nEntity实体
function TMenuManager.AddEntity(const nProg, nEntity: string;
  const nList: TList): PMenuEntity;
begin
  Result := FindEntity(nProg, nEntity, nList);
  if not Assigned(Result) then
  begin
    New(Result);
    nList.Add(Result);

    Result.FProgID := nProg;
    Result.FEntity := nEntity;
    SetLength(Result.FItems, 0);

    Result.FDefaultPMenu := '';
    Result.FDefaultType := mtItem;
  end;
end;

//Date: 2020-04-22
//Parm: 列表;是否释放
//Desc: 清理nList实体信息
procedure TMenuManager.ClearMenus(const nList: TList; const nFree: Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx := nList.Count - 1 downto 0 do
      Dispose(PMenuEntity(nList[nIdx]));
    //xxxxx

    if nFree then
         nList.Free
    else nList.Clear;
  end;
end;

//Date: 2020-04-22
//Parm: 列表
//Desc: 获取菜单实体信息
procedure TMenuManager.GetMenus(const nList: TList);
var nIdx: Integer;
begin
  ClearMenus(nList);
  //init first

  for nIdx := Low(FMenuBuilders) to High(FMenuBuilders) do
    FMenuBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2020-04-24
//Parm: 输出
//Desc: 在数据中初始化
function TMenuManager.InitMenus(const nMemo: TStrings): Boolean;
var nStr,nSQL: string;
    i,j,nIdx: Integer;
    nMenus: TList;
    nListA,nListB: TStrings;

    nQuery: TADOQuery;
    nEntity: PMenuEntity;
begin
  Result := False;
  if Assigned(nMemo) then
    nMemo.Clear;
  //xxxxx

  nListA := nil;
  nListB := nil;
  nMenus := nil;
  nQuery := nil; //init

  with gMG.FDBManager do
  try
    nQuery := LockDBQuery();
    LoadLanguage(nQuery);

    if Length(FMultiLang) < 1 then
    begin
      WriteLog('InitMenus: 未配置语言', nMemo);
      Exit;
    end;

    nListA := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListA.Clear;
    nListB := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListB.Clear;

    nSQL := 'Select M_Prog,M_Entity,M_MenuID,M_Lang From ' + sTableMenu;
    with DBQuery(nSQL, nQuery) do
    if RecordCount > 0 then
    begin
      First;
      while not Eof do
      begin
        nStr := FieldByName('M_Prog').AsString + '.' +
                FieldByName('M_Entity').AsString + '.' +
                FieldByName('M_MenuID').AsString + '.' +
                FieldByName('M_Lang').AsString;
        nListA.Add(nStr);
        Next;
      end;
    end;

    nMenus := gMG.FObjectPool.Lock(TList) as TList;
    GetMenus(nMenus);
    //get menus data

    WriteLog('::: 创建菜单数据 :::', nMemo);
    for nIdx := 0 to nMenus.Count -1 do
    begin
      nEntity := nMenus[nIdx];
      //entity item
      WriteLog('创建实体: ' + nEntity.FProgID + '.' + nEntity.FEntity);

      for i := Low(FMultiLang) to High(FMultiLang) do //multi language
      begin
        for j := Low(nEntity.FItems) to High(nEntity.FItems) do
        with nEntity.FItems[j] do
        begin
          nStr := nEntity.FProgID + '.' + nEntity.FEntity + '.' +
                  FMenuID + '.' + FMultiLang[i].FID;
          //xxxxx

          if nListA.IndexOf(nStr) >= 0 then Continue;
          //menu exists

          with TSQLBuilder do
          nSQL := TSQLBuilder.MakeSQLByStr([
            SF('M_Prog', nEntity.FProgID),
            SF('M_Entity', nEntity.FEntity),
            SF('M_PMenu', FPMenu),
            SF('M_MenuID', FMenuID),
            SF('M_Title', FTitle),
            SF('M_Action', FAction),
            SF('M_Flag', FFlag),
            SF('M_Filter', FFilter),
            SF('M_Type', sMenuTypeID[FType]),
            SF('M_Lang', FMultiLang[i].FID),
            SF('M_NewOrder', j, sfVal)
            ], sTableMenu);
          //insert sql

          nListB.Add(nSQL);
          WriteLog('已创建: ' + nStr, nMemo);
        end;
      end;
    end;

    if nListB.Count > 0 then
      DBExecute(nListB);
    //save
  finally
    gMG.FObjectPool.Release(nListA);
    gMG.FObjectPool.Release(nListB);
    //string list
    ClearMenus(nMenus);
    gMG.FObjectPool.Release(nMenus);
    //menu list
    gMG.FDBManager.ReleaseDBQuery(nQuery);
  end;
end;

end.
