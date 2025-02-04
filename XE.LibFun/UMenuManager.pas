{*******************************************************************************
  作者: dmzn@163.com 2020-04-21
  描述: 菜单管理器

  备注:
  *.多语言支持.
  *.代码内维护菜单项,保证系统菜单完整.原来使用菜单编辑器的方法,会导致部署差异.
  *.程序ProgID: 用于区分菜单所属的系统.
  *.实体Entity: 用于区分同系统内的不同菜单,包括主菜单和快捷菜单.

  使用方法:
  1.添加Builder
    procedure SytemMenuBuilder(const nList: TList);
    begin
      gMenuManager.AddEntity(sProg_Main, '主程序', 'MAIN', '主菜单', nList).
        SetParent('').SetType(mtItem).                 //1 level
          AddM('A00', '系统').
          AddM('B00', '网络').
          AddM('C00', '参数').
        SetParent('A00').SetType(mtItem).              //A00
          AddM('A01', '切换语言').
          AddM('A02', '修改密码').
          AddM('A03', '动态口令').
          AddM('A04', '退出系统', maExecute, sCMD_Exit).
        SetParent('B00').SetType(mtItem).              //B00
          AddM('B01', 'B01', maNewForm, 'formB01').
          AddM('B02', 'B02').
      //RunSoft.MAIN

      gMenuManager.AddEntity(sProg_Admin, '管理工具', 'MAIN', '主菜单', nList).
        SetParent('').SetType(mtItem).                 //1 level
          AddM('A00', '系统管理').
          AddM('B00', '基础信息').
        SetParent('A00').SetType(mtItem).
          AddM('A01', '数据备份').
          AddM('A02', '数据恢复').
        SetParent('B00').SetType(mtItem).
          AddM('B01', '系统参数').
          AddM('B02', '组织结构').
          AddM('B03', '用户管理').
      //RunAdmin.MAIN
    end;

    gMenuManager.AddMenuBuilder(SytemMenuBuilder);
    //将菜单项提交至菜单管理器

  2.初始化菜单项
    gMenuManager.InitMenus();

  3.加载菜单项
    gMenuManager.GetMenus(FActive.FDeployType, FPrograms[nIdx].FProgram,
        sEntity_Main, FUser.FLangID, FMenus);
    //main menu
*******************************************************************************}
unit UMenuManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, Data.DB,
  UBaseObject, ULibFun;

type
  TMenuItemType = (mtDefault, mtProg, mtEntity, mtItem, mtAssist);
  //menu item

  TMenuAction = (maDefault, maNewForm, maNewFrame, maExecute);
  //菜单动作: 默认,新窗体,新页面,执行命令

const
  sMenuItemType: array[TMenuItemType] of string = ('默认', '程序', '实体',
    '菜单项', '辅助项');
  //menu type desc

  sMenuAction: array[TMenuAction] of string = ('默认操作', '打开窗体', '打开框架',
    '执行程序');
  //menu acton desc

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
    FProgID       : string;                              //程序标识
    FEntity       : string;                              //实体标识
    FMenuID       : string;                              //菜单标识
    FPMenu        : string;                              //上级菜单
    FTitle        : string;                              //菜单标题
    FTitlePy      : string;                              //标题拼音
    FAction       : TMenuAction;                         //菜单动作
    FActionData   : string;                              //动作数据
    FFlag         : string;                              //附加参数(下划线..)
    FImgIndex     : integer;                             //图标索引
    FLang         : string;                              //语言标识
    FNewOrder     : Single;                              //创建序列
    FExpaned      : Boolean;                             //是否展开

    FRecordID     : string;                              //记录编号(DB)
    FUserID       : string;                              //所属用户
    FUniqueID     : string;                              //数据关联标识
    FType         : TMenuItemType;                       //菜单类型
    FDeploy       : TApplicationHelper.TDeployTypes;     //部署类型
    FSubItems     : TList;                               //子菜单列表
  public
    procedure Init();
    {*初始化*}
  end;

  TMenuItems = array of TMenuItem;                       //菜单列表

  PMenuEntity = ^TMenuEntity;
  TMenuEntity = record
  private
    FDefaultPMenu : string;                              //默认上级
    FDefaultType  : TMenuItemType;                       //默认类型
    FDefaultDeploy : TApplicationHelper.TDeployTypes;    //默认部署
    {*辅助信息*}
    FProgID       : string;                              //程序标识
    FEntity       : string;                              //实体标识
    FItems        : TMenuItems;                          //菜单项
    {*菜单属性*}
  public
    function SetParent(const nPMenu: string): PMenuEntity;
    function SetType(const nType: TMenuItemType): PMenuEntity;
    function SetDeploy(const nDeploy: TApplicationHelper.TDeployTypes): PMenuEntity;
    {*设置属性*}
    function AddM(const nID,nTitle: string;
      const nAction: TMenuAction = maDefault;
      const nActionData: string = '';
      nDeploy: TApplicationHelper.TDeployTypes = []): PMenuEntity;
    {*添加菜单*}
  end;

  TMenuItemBuilder = procedure (const nList: TList);
  //for external-system fill menu.item info

  TMenuManager = class(TManagerBase)
  public
    const
      sTable_Lang = 'Sys_Lang';                          //语言配置
      sTable_Menu = 'Sys_Menus';                         //菜单配置
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
    procedure LoadLanguage(const nQuery: TDataSet = nil);
    {*语言管理*}
    procedure AddMenuBuilder(const nBuilder: TMenuItemBuilder);
    function AddEntity(const nProg,nProgName,nEntity,nEntityName: string;
      const nList: TList): PMenuEntity;
    {*添加数据*}
    procedure GetMenuData(const nList: TList);
    procedure ClearMenuData(const nList: TList; const nFree: Boolean = False);
    {*菜单数据*}
    function BuildMenuSQL(const nMenu: PMenuItem): string;
    procedure AddMenu(const nMenu: PMenuItem);
    procedure DeleteMenu(const nMenu: PMenuItem);
    {*菜单项*}
    procedure GetMenus(const nDeploy: TApplicationHelper.TDeployType;
      const nProg,nEntity,nLang: string;
      const nList: TList; const nUser: string = '');
    procedure ClearMenus(const nList: TList; const nFree: Boolean = False);
    {*菜单信息*}
    procedure InitMenus(const nMemo: TStrings = nil);
    {*初始化数据*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
    property MultiLanguage: TMenuDataList read FMultiLang;
    {*属性相关*}
  end;

var
  gMenuManager: TMenuManager = nil;
  //全局使用

implementation

uses
  UManagerGroup, UDBManager, UDBFun;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TMenuManager, '菜单管理器', nEvent);
end;

//Desc: 添加管理器所需表
procedure AddMenuTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TMenuManager do
  begin
    AddTable(sTable_Lang, nList, dtMSSQL).
      AddF('L_ID',          'varChar(5)',             '语言标识').
      AddF('L_Name',        'varChar(32)',            '语言名称').
      AddF('L_Valid',       'Char(1)',                '是否有效', SQM(sFlag_Enabled)).
      AddR('en', MakeSQLByStr([
        SF('L_ID', 'en'),
        SF('L_Name', 'English'),
        SF('L_Valid', sFlag_Disabled)], sTable_Lang)).
      AddR('cn', MakeSQLByStr([
        SF('L_ID', 'cn'),
        SF('L_Name', '简体中文')], sTable_Lang));
    //for language

    AddTable(sTable_Menu, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '记录标识').
      AddF('M_ProgID',      'varchar(15)',            '程序标识').
      AddF('M_Entity',      'varchar(15)',            '实体标识').
      AddF('M_MenuID',      'varchar(15)',            '菜单标识').
      AddF('M_PMenu',       'varchar(15)',            '上级菜单').
      AddF('M_Title',       'varchar(50)',            '菜单标题').
      AddF('M_Action',      'varchar(15)',            '菜单动作').
      AddF('M_Data',        'varchar(320)',           '动作数据').
      AddF('M_Flag',        'varchar(20)',            '附加参数').
      AddF('M_Type',        'varchar(15)',            '菜单类型').
      AddF('M_Lang',        'varchar(5)',             '语言标识').
      AddF('M_UserID',      'varchar(32)',            '用户标识').
      AddF('M_Unique',      'varchar(32)',            '数据关联').
      AddF('M_Deploy',      'varchar(32)',            '部署类型(Desktop,Web)').
      AddF('M_Expand',      'Char(1)',                '默认展开', SQM(sFlag_No)).
      AddF('M_ImgIndex',    'integer default -1',     '图标索引', '-1').
      AddF('M_NewOrder',    'float default 0',        '创建序列', '0').
      //for field
      AddI('idx_prog', 'CREATE INDEX $IDX ON $TBS(M_ProgID ASC,M_Entity ASC)');
      //for index
  end;
end;

//Date: 2021-07-12
//Desc: 初始化菜单项
procedure TMenuItem.Init();
var nInit: TMenuItem;
begin
  FillChar(nInit, SizeOf(TMenuItem), #0);
  Self := nInit;

  FImgIndex := -1;
  FNewOrder := 0;
  FType     := mtItem;
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

//Date: 2021-05-18
//Parm: 部署方式
//Desc: 设置默认部署方式
function TMenuEntity.SetDeploy(
  const nDeploy: TApplicationHelper.TDeployTypes): PMenuEntity;
begin
  Result := @Self;
  //return self address
  FDefaultDeploy := nDeploy;
end;

//Date: 2020-04-22
//Parm: 菜单标识;菜单标题;动作;动作数据;部署方式
//Desc: 新增菜单项
function TMenuEntity.AddM(const nID, nTitle: string; const nAction: TMenuAction;
  const nActionData: string; nDeploy: TApplicationHelper.TDeployTypes): PMenuEntity;
var nStr: string;
    nIdx: Integer;
begin
  Result := @Self;
  //return self address

  for nIdx := Low(FItems) to High(FItems) do
  with FItems[nIdx] do
  begin
    if CompareText(nID, FMenuID) <> 0 then Continue;
    //same entity,same menuid

    nStr := 'TMenuManager.AddM: %s.%s And %s.%s Have The Same MenuID.';
    nStr := Format(nStr, [nID, nTitle, FMenuID, FTitle]);

    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if nDeploy = [] then
    nDeploy := FDefaultDeploy;
  //use default

  nIdx := Length(FItems);
  SetLength(FItems, nIdx + 1);
  FItems[nIdx].Init;

  with FItems[nIdx] do
  begin
    FType         := FDefaultType;
    FProgID       := Result.FProgID;
    FEntity       := Result.FEntity;

    FMenuID       := nID;
    FPMenu        := FDefaultPMenu;
    FTitle        := nTitle;
    FAction       := nAction;
    FActionData   := nActionData;
    FDeploy       := nDeploy;
    FExpaned      := (FType = mtProg) or (FType = mtEntity); //expand 1 level

    FNewOrder     := 0;
    FImgIndex     := -1;
    FSubItems     := nil;
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
  gDBManager.AddTableBuilder(AddMenuTables);
end;

//Date: 2020-04-21
//Parm: 语言标识;语言名称;
//Desc: 添加多语言项
procedure TMenuManager.AddLanguage(const nID, nName: string);
var nStr: string;
    nQuery: TDataSet;
begin
  nQuery := nil;
  try
    with gDBManager, TSQLBuilder do
    begin
      nStr := 'Select L_ID From %s Where L_ID=''%s''';
      nStr := Format(nStr, [sTable_Lang, nID]);
      nQuery := LockDBQuery();

      nStr := MakeSQLByStr([
        SF('L_ID', nID),
        SF('L_Name', nName),
        SF('L_Valid', sFlag_Enabled)
      ], sTable_Lang, SF('L_ID', nID), DBQuery(nStr, nQuery).RecordCount < 1);
      DBExecute(nStr);
    end;
  finally
    gDBManager.ReleaseDBQuery(nQuery);
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
    nStr := Format(nStr, [sTable_Lang, nID]);
    nList.Add(nStr);

    nStr := 'Delete From %s Where M_Lang=''%s''';
    nStr := Format(nStr, [sTable_Lang, nID]);
    nList.Add(nStr);

    gDBManager.DBExecute(nList);
    //action
  finally
    gMG.FObjectPool.Release(nList);
  end;
end;

//Date: 2020-04-24
//Desc: 载入多语言列表
procedure TMenuManager.LoadLanguage(const nQuery: TDataSet);
var nStr: string;
    nIdx: Integer;
    nQry: TDataSet;
begin
  nQry := nil;
  with gDBManager do
  try
    if Assigned(nQuery) then
         nQry := nQuery
    else nQry := LockDBQuery();

    nStr := 'Select L_ID,L_Name From %s Where L_Valid=''%s''';
    nStr := Format(nStr, [sTable_Lang, sFlag_Enabled]);

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
//Desc: 在nList中新增nProg.nEntity实体
function TMenuManager.AddEntity(const nProg,nProgName,nEntity,nEntityName: string;
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

    Result.SetDeploy([]);
    Result.SetParent('');
    Result.SetType(mtProg).AddM(nProg, nProgName);
    Result.SetType(mtEntity).AddM(nEntity, nEntityName);
  end;
end;

//Date: 2020-04-22
//Parm: 列表;是否释放
//Desc: 清理nList实体信息
procedure TMenuManager.ClearMenuData(const nList: TList; const nFree: Boolean);
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
procedure TMenuManager.GetMenuData(const nList: TList);
var nIdx: Integer;
begin
  nList.Clear;
  //init first

  for nIdx := Low(FMenuBuilders) to High(FMenuBuilders) do
    FMenuBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2021-05-27
//Parm: 菜单数据
//Desc: 构建nMenu的insert,update语句
function TMenuManager.BuildMenuSQL(const nMenu: PMenuItem): string;
var nStr: string;
begin
  with nMenu^, TApplicationHelper, TSQLBuilder, TStringHelper do
  begin
    nStr := TStringHelper.Set2Str<TDeployType, TDeployTypes>(FDeploy);
    //deploy

    Result := MakeSQLByStr([
      SF('M_ProgID', FProgID),
      SF('M_PMenu', FPMenu),
      SF('M_Title', FTitle),
      SF('M_Action', Enum2Str(FAction)),
      SF('M_Data', FActionData),
      SF('M_Flag', FFlag),
      SF('M_Type', Enum2Str(FType)),
      SF('M_Deploy', nStr),
      SF('M_Lang', FLang),
      SF('M_Unique', FUniqueID),
      SF('M_ImgIndex', FImgIndex, sfVal),
      SF('M_NewOrder', FNewOrder, sfVal),

      SF_IF(['', SF('M_UserID', FUserID)], FUserID = ''),
      SF_IF([SF('M_Expand', sFlag_Yes),
             SF('M_Expand', sFlag_No)], FExpaned),
      //default expanded

      SF_IF([SF('M_Entity', ''),
             SF('M_Entity', FEntity)], FType = mtProg),
      //program no entity

      SF_IF([SF('M_MenuID', ''),
             SF('M_MenuID', FMenuID)], FType in [mtProg, mtEntity])
      //program and entity no id
    ], sTable_Menu, SF('R_ID', FRecordID, sfVal), FRecordID = '');
  end;
end;

//Date: 2021-05-27
//Parm: 菜单数据
//Desc: 新增 或 覆盖nMenu菜单
procedure TMenuManager.AddMenu(const nMenu: PMenuItem);
var nStr: string;
    nQuery: TDataSet;
    nEntity: TMenuItem;
begin
  if nMenu.FRecordID <> '' then
  begin
    nStr := gMenuManager.BuildMenuSQL(nMenu);
    gMG.FDBManager.DBExecute(nStr);
    Exit;
  end;

  if nMenu.FType <> mtItem then Exit;
  nMenu.FProgID := Trim(nMenu.FProgID);
  nMenu.FEntity := Trim(nMenu.FEntity);

  if (nMenu.FProgID = '') or (nMenu.FEntity = '') then
  begin
    nStr := 'TMenuManager.AddMenu: Program or Entity Is Null';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  nQuery := nil;
  try
    nStr := 'Select Top 1 M_NewOrder From %s ' +
            'Where M_ProgID=''%s'' And M_Entity=''%s'' ' +
            'Order By M_NewOrder DESC';
    nStr := Format(nStr, [sTable_Menu, nMenu.FProgID, nMenu.FEntity]);

    nQuery := gDBManager.DBQuery(nStr);
    if nQuery.RecordCount < 1 then
    begin
      nEntity := nMenu^;
      with nEntity do
      begin
        FTitle    := '我的菜单';
        FImgIndex := -1;
        FNewOrder := 0;
        FExpaned  := True;
        FType     := mtEntity;
      end;

      nStr := gMenuManager.BuildMenuSQL(@nEntity);
      gMG.FDBManager.DBExecute(nStr);
      //add entity
      nMenu.FNewOrder := 1;
    end else nMenu.FNewOrder := nQuery.Fields[0].AsInteger + 1;

    nStr := gMenuManager.BuildMenuSQL(nMenu);
    gMG.FDBManager.DBExecute(nStr, nQuery);
  finally
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-05-27
//Parm: 菜单数据
//Desc: 删除nMenu菜单
procedure TMenuManager.DeleteMenu(const nMenu: PMenuItem);
var nStr: string;
begin
  if nMenu.FRecordID <> '' then
  begin
    nStr := 'Delete From %s Where R_ID=%s';
    nStr := Format(nStr, [sTable_Menu, nMenu.FRecordID]);
    gMG.FDBManager.DBExecute(nStr);
  end;
end;

//Date: 2020-04-24
//Parm: 输出
//Desc: 初始化数据库中的菜单数据
procedure TMenuManager.InitMenus(const nMemo: TStrings);
var nStr: string;
    i,j,nIdx: Integer;
    nMenus: TList;
    nQuery: TDataSet;
    nEntity: PMenuEntity;
    nListA,nListB: TStrings;
begin
  nListA := nil;
  nListB := nil;
  nMenus := nil;
  nQuery := nil; //init

  with gDBManager do
  try
    nQuery := LockDBQuery();
    LoadLanguage(nQuery);

    if Length(FMultiLang) < 1 then
    begin
      UMenuManager.WriteLog('InitMenus: 未配置语言', nMemo);
      Exit;
    end;

    nListA := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListA.Clear;
    nListB := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListB.Clear;

    nStr := 'Select M_ProgID,M_Entity,M_MenuID,M_Lang From %s ' +
            'Where M_UserID Is Null';
    nStr := Format(nStr, [sTable_Menu]);

    with DBQuery(nStr, nQuery) do
    if RecordCount > 0 then
    begin
      First;
      while not Eof do
      begin
        nStr := FieldByName('M_ProgID').AsString + '.' +
                FieldByName('M_Entity').AsString + '.' +
                FieldByName('M_MenuID').AsString + '.' +
                FieldByName('M_Lang').AsString;
        nListA.Add(nStr);
        Next;
      end;
    end;

    nMenus := gMG.FObjectPool.Lock(TList) as TList;
    nMenus.Clear;
    GetMenuData(nMenus); //get menus data

    UMenuManager.WriteLog('::: 创建菜单数据 :::', nMemo);
    for nIdx := 0 to nMenus.Count -1 do
    begin
      nEntity := nMenus[nIdx];
      //entity item
      UMenuManager.WriteLog('创建实体: ' + nEntity.FProgID + '.' + nEntity.FEntity, nMemo);

      for i := Low(FMultiLang) to High(FMultiLang) do //multi language
      begin
        for j := Low(nEntity.FItems) to High(nEntity.FItems) do
        with nEntity.FItems[j],TSQLBuilder,TStringHelper do
        begin
          nStr := nEntity.FProgID + '.' +
            SF_IF(['', nEntity.FEntity], FType = mtProg) + '.' +
            SF_IF(['', FMenuID], FType in [mtProg, mtEntity]) + '.' +
            FMultiLang[i].FID;
          //xxxxx

          if nListA.IndexOf(nStr) >= 0 then Continue;
          //menu exists

          FLang := FMultiLang[i].FID;
          FNewOrder := j;

          if FType = mtItem then
            FUniqueID := TDBCommand.SnowflakeID();
          //xxxxx
          
          nListB.Add(BuildMenuSQL(@nEntity.FItems[j]));
          UMenuManager.WriteLog('已创建: ' + nStr, nMemo);
        end;
      end;
    end;

    if nListB.Count > 0 then
      DBExecute(nListB);
    //xxxxx
  finally
    gMG.FObjectPool.Release(nListA);
    gMG.FObjectPool.Release(nListB);
    gDBManager.ReleaseDBQuery(nQuery);

    ClearMenuData(nMenus);
    gMG.FObjectPool.Release(nMenus);
    //menu list
  end;
end;

//Date: 2021-05-19
//Parm: 菜单项列表
//Desc: 清理nList列表
procedure TMenuManager.ClearMenus(const nList: TList; const nFree: Boolean);
var nIdx: Integer;
    nItem: PMenuItem;
begin
  if Assigned(nList) then
  begin
    for nIdx := nList.Count-1 downto 0 do
    begin
      nItem := nList[nIdx];
      ClearMenus(nItem.FSubItems, True);
      Dispose(nItem);
    end;

    if nFree then
         nList.Free
    else nList.Clear;
  end;
end;

//Date: 2021-05-19
//Parm: 部署方式;程序;实体;用户;语言
//Desc: 载入nUser.nLang对应的nProg.nEntity的树状菜单列表数据
procedure TMenuManager.GetMenus(const nDeploy: TApplicationHelper.TDeployType;
  const nProg, nEntity, nLang: string; const nList: TList; const nUser: string);
var nStr: string;
    nQuery: TDataSet;
    nType: TMenuItemType;
    nItem,nPItem: PMenuItem;
    nDPType: TApplicationHelper.TDeployTypes;

    //Date: 2021-05-19
    //Parm: 父菜单名;节点列表;当前层级
    //Desc: 在nPList中检索名称为nPMenu的菜单项
    function FindParent(const nPName: string; const nPList: TList;
      const nPLevel: Integer = 1): PMenuItem;
    var nIdx: Integer;
        nBool: Boolean;
    begin
      Result := nil;

      for nIdx := nPList.Count - 1 downto 0 do
      begin
        nPItem := nPList[nIdx];
        if CompareText(nPItem.FLang, nLang) <> 0 then Continue;
        //语言匹配
        
        if nType in [mtProg, mtEntity] then //程序和实体: 寻找程序菜单项
        begin
          nBool := (nPItem.FType = mtProg) and
                   (CompareText(nPItem.FProgID, nProg) = 0);
          //xxxxx
        end else

        if nPName = '' then //一级菜单: 寻找实体
        begin
          nBool := (nPItem.FType = mtEntity) and
                   (CompareText(nPItem.FProgID, nProg) = 0) and
                   (CompareText(nPItem.FEntity, nEntity) = 0);
          //xxxxx
        end else //菜单项: 依据MenuID匹配
        begin
          nBool := (CompareText(nPItem.FMenuID, nPName) = 0) and
                   (CompareText(nPItem.FProgID, nProg) = 0) and
                   (CompareText(nPItem.FEntity, nEntity) = 0);
          //xxxxx
        end;

        if nBool then
        begin
          Result := nPItem;
          Exit;
        end;
      end;

      if (nType in [mtProg, mtEntity]) and (nPLevel > 0) then Exit;
      //程序和实体扫描第一级

      for nIdx := nPList.Count - 1 downto 0 do
      begin
        nPItem := nPList[nIdx];
        if CompareText(nPItem.FLang, nLang) <> 0 then Continue;
        //语言匹配

        if Assigned(nPItem.FSubItems) then
        begin
          Result := FindParent(nPName, nPItem.FSubItems, nPLevel + 1);
          if Assigned(Result) then Break;
        end;
      end;
    end;
begin
  nQuery := nil;
  with TApplicationHelper,TStringHelper do
  try
    nStr := 'Select * From %s Where M_ProgID=''%s'' ' +
            'And (M_Entity='''' Or M_Entity=''%s'') And M_Lang=''%s'' ' +
     StrIF(['And M_UserID Is Null ', 'And M_UserID =''%s'' '], nUser = '') +
            'Order By M_NewOrder ASC';
    nStr := Format(nStr, [sTable_Menu, nProg, nEntity, nLang, nUser]);

    nQuery := gDBManager.DBQuery(nStr);
    with nQuery do
    if RecordCount > 0 then
    begin
      First;
      //go to first

      while not Eof do
      try
        nStr := FieldByName('M_Deploy').AsString;
        nDPType := TStringHelper.Str2Set<TDeployType, TDeployTypes>(nStr);

        if (nDeploy <> dtAll) and
           (nDPType <> []) and (not (nDeploy in nDPType)) then Continue;
        //not match deploy

        nStr := FieldByName('M_Type').AsString;
        nType := TStringHelper.Str2Enum<TMenuItemType>(nStr);
        nPItem := FindParent(FieldByName('M_PMenu').AsString, nList);

        if Assigned(nPItem) then
        begin
          if nType = mtProg then Continue;
          //program has exits

          if not Assigned(nPItem.FSubItems) then
            nPItem.FSubItems := TList.Create;
          //xxxxx

          New(nItem);
          nPItem.FSubItems.Add(nItem);
          nItem.FSubItems := nil;
        end else
        begin
          New(nItem);
          nList.Add(nItem);
          nItem.FSubItems := nil;
        end;

        nItem.Init; //init
        with nItem^ do
        begin
          FType         := nType;
          FDeploy       := nDPType;
          FRecordID     := FieldByName('R_ID').AsString;
          FProgID       := FieldByName('M_ProgID').AsString;
          FEntity       := FieldByName('M_Entity').AsString;
          FMenuID       := FieldByName('M_MenuID').AsString;
          FPMenu        := FieldByName('M_PMenu').AsString;
          FTitle        := FieldByName('M_Title').AsString;
          FTitlePy      := TStringHelper.GetPinYin(FTitle);

          nStr          := FieldByName('M_Action').AsString;
          FAction       := TStringHelper.Str2Enum<TMenuAction>(nStr);
          FActionData   := FieldByName('M_Data').AsString;
          FFlag         := FieldByName('M_Flag').AsString;
          FImgIndex     := FieldByName('M_ImgIndex').AsInteger;
          FLang         := FieldByName('M_Lang').AsString;
          FUserID       := FieldByName('M_UserID').AsString;
          FUniqueID     := FieldByName('M_Unique').AsString;
          FExpaned      := FieldByName('M_Expand').AsString = sFlag_Yes;
          FNewOrder     := FieldByName('M_NewOrder').AsInteger;
        end;

      finally
        Next;
        //next record
      end;
    end;
  finally
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

procedure TMenuManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx,nInt: Integer;
    nMenus: TList;
    nEntity: PMenuEntity;
begin
  nMenus := nil;
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    nMenus := gMG.FObjectPool.Lock(TList) as TList;
    GetMenuData(nMenus);
    //get menus data

    nInt := 0;
    for nIdx := nMenus.Count-1 downto 0 do
    begin
      nEntity := nMenus[nIdx];
      nInt := nInt + Length(nEntity.FItems);
    end;

    if not nFriendly then
    begin
      nList.Add('NumBuilder=' + Length(FMenuBuilders).ToString);
      nList.Add('NumEntity=' + nMenus.Count.ToString);
      nList.Add('NumMenuItem=' + nInt.ToString);
      Exit;
    end;

    nList.Add(FixData('NumBuilder:', Length(FMenuBuilders).ToString));
    nList.Add(FixData('NumEntity:', nMenus.Count.ToString));
    nList.Add(FixData('NumMenuItem:', nInt.ToString));

    for nIdx := 0 to nMenus.Count-1 do
    begin
      nEntity := nMenus[nIdx];
      nList.Add(FixData(Format('EntityItem %d:', [nIdx+1]),
        nEntity.FProgID + '.' + nEntity.FEntity));
      //xxxxx
    end;
  finally
    SyncLeave;
    ClearMenuData(nMenus);
    gMG.FObjectPool.Release(nMenus);
  end;
end;

end.
