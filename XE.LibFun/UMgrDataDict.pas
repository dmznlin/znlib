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
    FRecord   : integer;                                //记录号
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

  TDcitItemBuilder = procedure (const nList: TList);
  //for external-system fill entity.items info

  TDataDictManager = class(TManagerBase)
  public
    const
      sTable_DataDict = 'Sys_DataDict';                     //数据字典
  private
    FBuilders: array of TDcitItemBuilder;
    {*字典配置信息*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册对象*}
    procedure RunAfterRegistAllManager; override;
    procedure RunBeforUnregistAllManager; override;
    {*延迟执行*}
    procedure AddDictBuilder(const nBuilder: TDcitItemBuilder);
    function AddEntity(const nEntity,nName: string; nLang: string=''): PEntityItem;
    {*添加数据*}
    function BuilItemSQL(const nItem: PEntityItem): string;
    procedure InitItem(const nItem: PEntityItem);
    procedure AddItem(const nItem: PEntityItem);
    procedure DelItem(const nItem: PEntityItem);
    {*字典项*}
    procedure GetEntity(const nEntity: string;
      const nData: PEntityItem; nLang: string = '');
    {*字典数据*}
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

procedure WriteLog(const nEvent: string);
begin
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

function TEntityItem.AddDict(const nTitle, nField: string): PEntityItem;
begin

end;

//------------------------------------------------------------------------------
constructor TDataDictManager.Create;
begin
  inherited;
  //
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

procedure TDataDictManager.RunBeforUnregistAllManager;
begin

end;

procedure TDataDictManager.AddDictBuilder(const nBuilder: TDcitItemBuilder);
begin

end;

function TDataDictManager.AddEntity(const nEntity, nName: string;
  nLang: string): PEntityItem;
begin

end;

function TDataDictManager.BuilItemSQL(const nItem: PEntityItem): string;
begin

end;

procedure TDataDictManager.AddItem(const nItem: PEntityItem);
begin

end;

procedure TDataDictManager.DelItem(const nItem: PEntityItem);
begin

end;

//Date: 2021-06-08
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList
procedure TDataDictManager.GetEntity(const nEntity: string;
  const nData: PEntityItem; nLang: string);
begin

end;

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

procedure TDataDictManager.InitItem(const nItem: PEntityItem);
begin

end;

end.
