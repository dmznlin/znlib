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
  System.Classes, System.SysUtils, System.Generics.Collections, UBaseObject;

type
  TMenuItemType = (mtProg, mtEntity, mtParent, mtItem, mtAssist);
  //menu item

const
  sMenuTypeID: array[TMenuItemType] of string = ('pg', 'et', 'pr', 'mn', 'as');
  //menu type id

  sMenuTypeText: array[TMenuItemType] of string = ('程序', '实体', '父菜单',
    '菜单项', '辅助项');
  //menu type desc

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
    FProgID       : string;                              //程序标识
    FEntity       : string;                              //实体标识
    FItems        : TMenuItems;                          //菜单项
    {*菜单属性*}
    function AddM(const nID,nTitle: string; const nPMenu: string = '';
      const nAction: string = ''; const nFilter: string = ''): PMenuEntity;
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
    procedure AddLang(const nID,nName: string);
    procedure AddMenuBuilder(const nBuilder: TMenuItemBuilder);
    function AddEntity(const nProg,nEntity: string; const nList: TList): PMenuEntity;
    {*添加数据*}
    procedure GetMenus(const nList: TList);
    procedure ClearMenus(const nList: TList; const nFree: Boolean = False);
    {*获取表信息*}
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
  sTableMenuItem    = 'Sys_MenuItem';     //菜单项配置

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
  AddR('cn', MakeSQLByStr([
    SF('L_ID', 'cn'),
    SF('L_Name', '简体中文')], sTableLang));
  //for language

  gMG.FDBManager.AddTable(sTableMenuItem, nList, dtMSSQL).
  AddF('R_ID',          sField_SQLServer_AutoInc, '记录标识').
  AddF('M_Prog',        'varchar(15)',            '程序标识').
  AddF('M_Entity',      'varchar(15)',            '实体标识').
  AddF('M_MenuID',      'varchar(15)',            '菜单标识').
  AddF('M_Title',       'varchar(50)',            '菜单标题').
  AddF('M_Flag',        'varchar(20)',            '附加参数').
  AddF('M_Action',      'varchar(100)',           '菜单动作').
  AddF('M_Filter',      'varchar(100)',           '过滤条件').
  AddF('M_Type',        'varchar(5)',             '类型标识').
  AddF('M_Lang',        'varchar(5)',             '语言标识').
  AddF('M_ImgIndex',    'integer default 0',      '图标索引', '0').
  AddF('M_NewOrder',    'float default 0',        '创建序列', '0').
  //for field
  Copy(sTableMenu).
  //for table
  AddI('idx_prog', 'CREATE NONCLUSTERED INDEX idx_prog ON $TB.* (M_Prog ASC)');
  //for index
end;

//Date: 2020-04-22
//Parm: 菜单标识;菜单标题;父菜单
//Desc: 新增菜单项
function TMenuEntity.AddM(const nID, nTitle, nPMenu, nAction,
  nFilter: string): PMenuEntity;
begin

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
procedure TMenuManager.AddLang(const nID, nName: string);
var nIdx: Integer;
begin
  for nIdx := Low(FMultiLang) to High(FMultiLang) do
  if CompareText(nID, FMultiLang[nIdx].FID) = 0 then
  begin
    FMultiLang[nIdx].FName := nName;
    Exit;
  end;

  nIdx := Length(FMultiLang);
  SetLength(FMultiLang, nIdx + 1);
  with FMultiLang[nIdx] do
  begin
    FID := nID;
    FName := nName;
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
  end;
end;

//Date: 2020-04-22
//Parm: 列表;是否释放
//Desc: 清理nList实体信息
procedure TMenuManager.ClearMenus(const nList: TList; const nFree: Boolean);
var nIdx: Integer;
begin
  for nIdx := nList.Count - 1 downto 0 do
    Dispose(PMenuEntity(nList[nIdx]));
  //xxxxx

  if nFree then
       nList.Free
  else nList.Clear;
end;

//Date: 2020-04-22
//Parm: 列表
//Desc: 获取菜单实体信息
procedure TMenuManager.GetMenus(const nList: TList);
var nIdx: Integer;
begin
  ClearMenus(nList, False);
  //init first

  for nIdx := Low(FMenuBuilders) to High(FMenuBuilders) do
    FMenuBuilders[nIdx](nList);
  //xxxxx
end;

end.
