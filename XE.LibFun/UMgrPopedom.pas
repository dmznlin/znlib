{*******************************************************************************
  作者: dmzn@163.com 2021-11-09
  描述: 权限配置管理器
*******************************************************************************}
unit UMgrPopedom;

interface

uses
  System.Classes, System.SysUtils, Data.DB, ULibFun, UBaseObject;

const
  //popedom tag
  sPopedom_Read       = 'A';                         //浏览
  sPopedom_Add        = 'B';                         //添加
  sPopedom_Edit       = 'C';                         //修改
  sPopedom_Delete     = 'D';                         //删除
  sPopedom_Preview    = 'E';                         //预览
  sPopedom_Print      = 'F';                         //打印
  sPopedom_Export     = 'G';                         //导出

  sPopedom_Tags: array[0..6] of TStringHelper.TItemName = (
    (FItem: sPopedom_Read;      FName: '浏览'),
    (FItem: sPopedom_Add;       FName: '添加'),
    (FItem: sPopedom_Edit;      FName: '修改'),
    (FItem: sPopedom_Delete;    FName: '删除'),
    (FItem: sPopedom_Preview;   FName: '预览'),
    (FItem: sPopedom_Print;     FName: '打印'),
    (FItem: sPopedom_Export;    FName: '导出'));
  //popedom tag width name

  //popdom group
  sPopedom_Group      = 'Popedom_Group';             //默认分组
  sPopedom_Forms      = 'Popedom_Forms';             //窗体分组
  sPopedom_Frames     = 'Popedom_Frames';            //框架分组

  sPopedom_Groups: array[0..2] of TStringHelper.TItemName = (
    (FItem: sPopedom_Group;     FName: '系统权限'),
    (FItem: sPopedom_Forms;     FName: '系统窗体(Forms)'),
    (FItem: sPopedom_Frames;    FName: '系统框架(Frames)'));
  //popedom group width name

type
  TPopedomTag = record
    FTag      : string;                              //权限标记
    FName     : string;                              //权限名称
  end;
  TPopedomTags = array of TPopedomTag;

  PPopedomItem = ^TPopedomItem;
  TPopedomItem = record
    FItemID   : string;                              //权限对象
    FName     : string;                              //对象名称
    FPopedom  : TStringHelper.TStringArray;          //权限列表
  end;
  TPopedomItems = array of TPopedomItem;

  PPopedomGroup = ^TPopedomGroup;
  TPopedomGroup = record
    FGroup    : string;                              //分组标识
    FName     : string;                              //分组名称
    FItems    : TPopedomItems;                       //权限项
  public
    function Init(nGroup,nName: string): PPopedomGroup;
    {*初始化*}
    function AddPopdom(const nItem,nName: string;
      const nPopedom: TStringHelper.TStringArray = []): PPopedomGroup;
    {*添加权限项*}
  end;
  TPopedomGroups = array of TPopedomGroup;

  PPopedomPost = ^TPopedomPost;
  TPopedomPost = record
    FPost     : string;                              //工作岗位
    FValidOn  : Cardinal;                            //生效时间
    FItems    : TPopedomItems;                       //权限项
  public
    function Init(const nPost: string): PPopedomPost;
    {*初始化*}
  end;
  TPopedomPosts = array of TPopedomPost;

  TPopedomBuilder = procedure (const nList: TList);
  //for external-system fill popedom info

  TPopedomManager = class(TManagerBase)
  public
    const
      //tables
      sTable_Popedom      = 'Sys_Popedom';           //权限配置
      sTable_PopedomTag   = 'Sys_PopedomTag';        //权限标识
      sTable_PopedomGroup = 'Sys_PopedomGroup';      //权限分组
  private
    FTags: TPopedomTags;
    {*权限标识*}
    FGroups: TPopedomGroups;
    {*权限分组*}
    FBuffered: TPopedomPosts;
    {*权限缓存*}
    FBuilders: array of TPopedomBuilder;
    {*权限配置信息*}
  protected
    function FindTag(const nTag: string): Integer;
    {*检索数据*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册管理器*}
    procedure RunAfterRegistAllManager; override;
    {*延迟执行*}
    procedure AddTag(const nTag: TPopedomTag); overload;
    procedure AddTag(const nTag,nName: string); overload;
    {*权限标识*}
    procedure AddBuilder(const nBuilder: TPopedomBuilder);
    function AddGroup(const nList: TList; const nGroup:string = '';
      const nName: string = ''): PPopedomGroup;
    {*添加数据*}
    function FindTagName(const nTag: string): string;
    function FindGroupName(const nGroup: string): string;
    function FindGroup(const nID: string;const nList: TList=nil): PPopedomGroup;
    {*检索数据*}
    procedure GetPopedoms(const nList: TList);
    procedure ClearPopedoms(const nList: TList; const nFree: Boolean = False);
    {*权限数据*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
    property PopedomTags: TPopedomTags read FTags;
    property PopedomGroups: TPopedomGroups read FGroups;
  end;

var
  gPopedomManager: TPopedomManager = nil;
  //全局使用

implementation

uses
  UManagerGroup, UDBManager;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TPopedomManager, '权限管理器', nEvent);
end;

//Desc: 添加管理器所需表
procedure AddPopedomTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TPopedomManager do
  begin
    AddTable(sTable_PopedomTag, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '记录编号').
      AddF('T_Tag',         'varchar(32)',            '权限标识').
      AddF('T_Memo',        'varchar(80)',            '标识描述');
    //popedom tag

    AddTable(sTable_PopedomGroup, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '记录编号').
      AddF('G_ID',          'varchar(32)',            '分组标识').
      AddF('G_Name',        'varchar(80)',            '分组描述').
      AddF('G_Index',       'Integer',                '分组顺序');
    //popedom group

    AddTable(sTable_Popedom, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '记录编号').
      AddF('P_ID',          'varchar(80)',            '对象标识').
      AddF('P_Name',        'varchar(80)',            '对象名称').
      AddF('P_Group',       'varchar(32)',            '所属分组').
      AddF('P_Post',        'varchar(32)',            '所属岗位').
      AddF('P_Popedom',     'varchar(320)',           '权限值').
      AddF('P_LastModify',  'DateTime',               '修改时间').
      //for field
      AddI('idx_id',        'P_Post ASC,P_ID ASC').
      AddI('idx_post_all',  'P_Post ASC');
    //岗位P_Post对P_ID对象拥有P_Popedom权限
  end;
end;

//------------------------------------------------------------------------------
//Date: 2021-11-17
//Parm: 岗位标识
//Desc: 初始化岗位权限
function TPopedomPost.Init(const nPost: string): PPopedomPost;
var nInit: TPopedomPost;
begin
  FillChar(nInit, SizeOf(nInit), #0);
  Self := nInit;
  Result := @Self;
end;

//Date: 2021-11-17
//Parm: 分组标识;分组名称
//Desc: 初始化权限分组
function TPopedomGroup.Init(nGroup, nName: string): PPopedomGroup;
var nInit: TPopedomGroup;
begin
  FillChar(nInit, SizeOf(TPopedomGroup), #0);
  Self := nInit;
  Result := @Self;

  if nGroup = '' then
       FGroup := sPopedom_Group
  else FGroup := nGroup;

  if nName = '' then
       FName := gPopedomManager.FindGroupName(FGroup)
  else FName := nName;
end;

//Date: 2021-11-17
//Parm: 权限对象;对象名称;可使用的权限
//Desc: 添加一个权限对象
function TPopedomGroup.AddPopdom(const nItem, nName: string;
  const nPopedom: TStringHelper.TStringArray): PPopedomGroup;
var nStr: string;
    i,nIdx,nLen: Integer;
begin
  for nIdx := Low(FItems) to High(FItems) do
   if CompareText(nItem, FItems[nIdx].FItemID) = 0 then
   begin
     nStr := 'UMgrPopedom.AddPopdom: %s.%s Has Exists.';
     nStr := Format(nStr, [nItem, nName]);
     gMG.WriteLog(TPopedomManager, '权限管理器', nStr);
     raise Exception.Create(nStr);
   end;

  Result := @Self;
  nIdx := Length(FItems);
  SetLength(FItems, nIdx + 1);

  with FItems[nIdx] do
  begin
    FItemID   := nItem;
    FName     := nName;

    nLen := Length(nPopedom);
    if nLen > 0 then
    begin
      SetLength(FPopedom, nLen);
      for i := Low(nPopedom) to High(nPopedom) do
        FPopedom[i] := nPopedom[i];
      //xxxxx
    end else
    begin
      nLen := Length(sPopedom_Tags);
      SetLength(FPopedom, nLen);
      for i := Low(sPopedom_Tags) to High(sPopedom_Tags) do
        FPopedom[i] := sPopedom_Tags[i].FItem;
      //xxxxx
    end;
  end;
end;

//------------------------------------------------------------------------------
constructor TPopedomManager.Create;
begin
  inherited;
  SetLength(FTags, 0);
  SetLength(FGroups, 0);
  SetLength(FBuffered, 0);
  SetLength(FBuilders, 0);
end;

destructor TPopedomManager.Destroy;
begin

  inherited;
end;

//Date: 2021-08-15
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TPopedomManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TPopedomManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TPopedomManager.Create;
    gMG.FPopedomManager := gMG.FManagers[nIdx].FManager as TPopedomManager;
  end else
  begin
    gMG.FPopedomManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gPopedomManager := gMG.FPopedomManager;
  //启用全局变量
end;

procedure TPopedomManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TParameterManager', ['TDBManager']);
  //检查支持
  gDBManager.AddTableBuilder(AddPopedomTables);
end;

//Date: 2021-11-16
//Parm: 标识
//Desc: 检索nTag标识的索引
function TPopedomManager.FindTag(const nTag: string): Integer;
var nIdx: Integer;
begin
  Result := -1;
  for nIdx := Low(FTags) to High(FTags) do
   if CompareText(FTags[nIdx].FTag, nTag) = 0 then
   begin
     Result := nIdx;
     Break;
   end;
end;

//Date: 2021-11-17
//Parm: 标识
//Desc: 检索nTag的名称
function TPopedomManager.FindTagName(const nTag: string): string;
var nIdx: Integer;
begin
  for nIdx := Low(FTags) to High(FTags) do
   if CompareText(nTag, FTags[nIdx].FTag) = 0 then
   begin
     Result := FTags[nIdx].FName;
     Exit;
   end;

  Result := '';
  for nIdx := Low(sPopedom_Tags) to High(sPopedom_Tags) do
   if CompareText(nTag, sPopedom_Tags[nIdx].FItem) = 0 then
   begin
     Result := sPopedom_Tags[nIdx].FName;
     Break;
   end;
end;

//Date: 2021-11-16
//Parm: 标识;描述
//Desc: 新增权限标识符
procedure TPopedomManager.AddTag(const nTag, nName: string);
var nPT: TPopedomTag;
begin
  FillChar(nPT, SizeOf(nPT), #0);
  with nPT do
  begin
    FTag := nTag;
    FName := nName;
  end;
  AddTag(nPT);
end;

//Date: 2021-11-16
//Parm: 权限标识
//Desc: 新增nTag标识符
procedure TPopedomManager.AddTag(const nTag: TPopedomTag);
var nIdx: Integer;
begin
  nIdx := FindTag(nTag.FTag);
  if nIdx < 0 then //new tag
  begin
    nIdx := Length(FTags);
    SetLength(FTags, nIdx + 1);
  end;

  FTags[nIdx] := nTag;
end;

//Date: 2021-11-17
//Parm: 配置方法
//Desc: 新增权限项配置方法
procedure TPopedomManager.AddBuilder(const nBuilder: TPopedomBuilder);
var nIdx: Integer;
begin
  for nIdx := Low(FBuilders) to High(FBuilders) do
    if @FBuilders[nIdx] = @nBuilder then Exit;
  //has exists

  nIdx := Length(FBuilders);
  SetLength(FBuilders, nIdx + 1);
  FBuilders[nIdx] := nBuilder;
end;

//Date: 2021-11-17
//Parm: 分组标识
//Desc: 检索标识为nID的权限分组
function TPopedomManager.FindGroup(const nID: string;
  const nList: TList): PPopedomGroup;
var nIdx: Integer;
    nPG: PPopedomGroup;
begin
  Result := nil;
  if Assigned(nList) then
  begin
    for nIdx := nList.Count - 1 downto 0 do
    begin
      nPG := nList[nIdx];
      if CompareText(nID, nPG.FGroup) = 0 then
      begin
        Result := nPG;
        Break;
      end;
    end;

    Exit;
  end else
  begin
    for nIdx := Low(FGroups) to High(FGroups) do
     if CompareText(nID, FGroups[nIdx].FGroup) = 0 then
     begin
       Result := @FGroups[nIdx];
       Break;
     end;
  end;
end;

//Date: 2021-11-17
//Parm: 分组标识
//Desc: 检索nGroup分组的名称
function TPopedomManager.FindGroupName(const nGroup: string): string;
var nIdx: Integer;
begin
  for nIdx := Low(FGroups) to High(FGroups) do
   if CompareText(nGroup, FGroups[nIdx].FGroup) = 0 then
   begin
     Result := FGroups[nIdx].FName;
     Exit;
   end;

  Result := '';
  for nIdx := Low(sPopedom_Groups) to High(sPopedom_Groups) do
   if CompareText(nGroup, sPopedom_Groups[nIdx].FItem) = 0 then
   begin
     Result := sPopedom_Groups[nIdx].FName;
     Break;
   end;
end;

//Date: 2021-11-17
//Parm: 分组标识;名称
//Desc: 添加一个权限分组
function TPopedomManager.AddGroup(const nList: TList;
 const nGroup, nName:string): PPopedomGroup;
begin
  Result := FindGroup(nGroup, nList);
  if Assigned(Result) then
  begin
    if (nName <> '') and (nName <> Result.FName) then
      Result.FName := nName;
    Exit;
  end;

  New(Result);
  nList.Add(Result);
  Result.Init(nGroup, nName);
end;

//Date: 2021-11-17
//Parm: 列表
//Desc: 获取所有的权限项
procedure TPopedomManager.GetPopedoms(const nList: TList);
var nIdx: Integer;
begin
  nList.Clear;
  //init first

  for nIdx := Low(FBuilders) to High(FBuilders) do
    FBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2021-11-17
//Parm: 列表;是否释放
//Desc: 清理nList权限项列表
procedure TPopedomManager.ClearPopedoms(const nList: TList;
  const nFree: Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx := nList.Count - 1 downto 0 do
      Dispose(PPopedomGroup(nList[nIdx]));
    //xxxxx

    if nFree then
         nList.Free
    else nList.Clear;
  end;
end;

procedure TPopedomManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx: Integer;
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin

      Exit;
    end;

  finally
    SyncLeave;
  end;
end;

end.
