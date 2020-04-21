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
  cMenuItemType: array[TMenuItemType] of string = ('程序', '实体',
    '父菜单', '菜单项', '辅助项');
  //menu item desc

type
  TMenuData = record
    FName         : string;                              //名称
    FData         : string;                              //数据
    FFlag         : string;                              //标识,如:LangID
  end;
  TMenuDatas = array of TMenuData;                       //数据列表

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    FType         : TMenuItemType;                       //菜单类型
    FProgID       : string;                              //程序标识
    FEntity       : string;                              //实体标识
    FMenuID       : string;                              //菜单标识
    FPMenu        : string;                              //上级菜单
    FTitle        : TMenuDatas;                          //菜单标题
    FImgIndex     : integer;                             //图标索引
    FFlag         : string;                              //附加参数(下划线..)
    FAction       : string;                              //菜单动作
    FFilter       : string;                              //过滤条件
    FNewOrder     : Single;                              //创建序列
    FPopedom      : string;                              //权限项
    FSubMenu      : TList;                               //子菜单列表
  end;

  TMenuManager = class(TManagerBase)
  private

  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    {*延迟执行*}
  end;

var
  gMenuManager: TMenuManager = nil;
  //全局使用

implementation

uses
  ULibFun, UManagerGroup;

const
  sTableMenu = 'Sys_Menu';

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TMenuManager, '菜单管理器', nEvent);
end;

//------------------------------------------------------------------------------
constructor TMenuManager.Create;
begin

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

//Desc: 添加管理器所需表
procedure AddMenuTables(const nList: TList);
begin
  gMG.FDBManager.AddTable(sTableMenu, nList).
  AddF('M_MenuID',      'varchar(15)',        '菜单标识').
  AddF('M_ProgID',      'varchar(15)',        '程序标识').
  AddF('M_Entity',      'varchar(15)',        '实体标识').
  AddF('M_PMenu',       'varchar(15)',        '上级菜单').
  AddF('M_Title',       'varchar(50)',        '菜单标题').
  AddF('M_ImgIndex',    'integer default 0',  '图标索引', '0').
  AddF('M_Flag',        'varchar(20)',        '附加参数').
  AddF('M_Action',      'varchar(100)',       '菜单动作').
  AddF('M_Filter',      'varchar(100)',       '过滤条件').
  AddF('M_Popedom',     'varchar(36)',        '权限项').
  AddF('M_NewOrder',    'float default -1',   '创建序列', '-1');
end;

procedure TMenuManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TMenuManager', ['TDBManager']);
  //检查支持
  gMG.FDBManager.AddSystemData(AddMenuTables);
end;

end.
