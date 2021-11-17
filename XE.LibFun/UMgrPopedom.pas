{*******************************************************************************
  ����: dmzn@163.com 2021-11-09
  ����: Ȩ�����ù�����
*******************************************************************************}
unit UMgrPopedom;

interface

uses
  System.Classes, System.SysUtils, Data.DB, ULibFun, UBaseObject;

const
  //popedom tag
  sPopedom_Read       = 'A';                         //���
  sPopedom_Add        = 'B';                         //���
  sPopedom_Edit       = 'C';                         //�޸�
  sPopedom_Delete     = 'D';                         //ɾ��
  sPopedom_Preview    = 'E';                         //Ԥ��
  sPopedom_Print      = 'F';                         //��ӡ
  sPopedom_Export     = 'G';                         //����

  sPopedom_Tags: array[0..6] of TStringHelper.TItemName = (
    (FItem: sPopedom_Read;      FName: '���'),
    (FItem: sPopedom_Add;       FName: '���'),
    (FItem: sPopedom_Edit;      FName: '�޸�'),
    (FItem: sPopedom_Delete;    FName: 'ɾ��'),
    (FItem: sPopedom_Preview;   FName: 'Ԥ��'),
    (FItem: sPopedom_Print;     FName: '��ӡ'),
    (FItem: sPopedom_Export;    FName: '����'));
  //popedom tag width name

  //popdom group
  sPopedom_Group      = 'Popedom_Group';             //Ĭ�Ϸ���
  sPopedom_Forms      = 'Popedom_Forms';             //�������
  sPopedom_Frames     = 'Popedom_Frames';            //��ܷ���

  sPopedom_Groups: array[0..2] of TStringHelper.TItemName = (
    (FItem: sPopedom_Group;     FName: 'ϵͳȨ��'),
    (FItem: sPopedom_Forms;     FName: 'ϵͳ����(Forms)'),
    (FItem: sPopedom_Frames;    FName: 'ϵͳ���(Frames)'));
  //popedom group width name

type
  TPopedomTag = record
    FTag      : string;                              //Ȩ�ޱ��
    FName     : string;                              //Ȩ������
  end;
  TPopedomTags = array of TPopedomTag;

  PPopedomItem = ^TPopedomItem;
  TPopedomItem = record
    FItemID   : string;                              //Ȩ�޶���
    FName     : string;                              //��������
    FPopedom  : TStringHelper.TStringArray;          //Ȩ���б�
  end;
  TPopedomItems = array of TPopedomItem;

  PPopedomGroup = ^TPopedomGroup;
  TPopedomGroup = record
    FGroup    : string;                              //�����ʶ
    FName     : string;                              //��������
    FItems    : TPopedomItems;                       //Ȩ����
  public
    function Init(nGroup,nName: string): PPopedomGroup;
    {*��ʼ��*}
    function AddPopdom(const nItem,nName: string;
      const nPopedom: TStringHelper.TStringArray = []): PPopedomGroup;
    {*���Ȩ����*}
  end;
  TPopedomGroups = array of TPopedomGroup;

  PPopedomPost = ^TPopedomPost;
  TPopedomPost = record
    FPost     : string;                              //������λ
    FValidOn  : Cardinal;                            //��Чʱ��
    FItems    : TPopedomItems;                       //Ȩ����
  public
    function Init(const nPost: string): PPopedomPost;
    {*��ʼ��*}
  end;
  TPopedomPosts = array of TPopedomPost;

  TPopedomBuilder = procedure (const nList: TList);
  //for external-system fill popedom info

  TPopedomManager = class(TManagerBase)
  public
    const
      //tables
      sTable_Popedom      = 'Sys_Popedom';           //Ȩ������
      sTable_PopedomTag   = 'Sys_PopedomTag';        //Ȩ�ޱ�ʶ
      sTable_PopedomGroup = 'Sys_PopedomGroup';      //Ȩ�޷���
  private
    FTags: TPopedomTags;
    {*Ȩ�ޱ�ʶ*}
    FGroups: TPopedomGroups;
    {*Ȩ�޷���*}
    FBuffered: TPopedomPosts;
    {*Ȩ�޻���*}
    FBuilders: array of TPopedomBuilder;
    {*Ȩ��������Ϣ*}
  protected
    function FindTag(const nTag: string): Integer;
    {*��������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�������*}
    procedure RunAfterRegistAllManager; override;
    {*�ӳ�ִ��*}
    procedure AddTag(const nTag: TPopedomTag); overload;
    procedure AddTag(const nTag,nName: string); overload;
    {*Ȩ�ޱ�ʶ*}
    procedure AddBuilder(const nBuilder: TPopedomBuilder);
    function AddGroup(const nList: TList; const nGroup:string = '';
      const nName: string = ''): PPopedomGroup;
    {*�������*}
    function FindTagName(const nTag: string): string;
    function FindGroupName(const nGroup: string): string;
    function FindGroup(const nID: string;const nList: TList=nil): PPopedomGroup;
    {*��������*}
    procedure GetPopedoms(const nList: TList);
    procedure ClearPopedoms(const nList: TList; const nFree: Boolean = False);
    {*Ȩ������*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
    property PopedomTags: TPopedomTags read FTags;
    property PopedomGroups: TPopedomGroups read FGroups;
  end;

var
  gPopedomManager: TPopedomManager = nil;
  //ȫ��ʹ��

implementation

uses
  UManagerGroup, UDBManager;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TPopedomManager, 'Ȩ�޹�����', nEvent);
end;

//Desc: ��ӹ����������
procedure AddPopedomTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TPopedomManager do
  begin
    AddTable(sTable_PopedomTag, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼���').
      AddF('T_Tag',         'varchar(32)',            'Ȩ�ޱ�ʶ').
      AddF('T_Memo',        'varchar(80)',            '��ʶ����');
    //popedom tag

    AddTable(sTable_PopedomGroup, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼���').
      AddF('G_ID',          'varchar(32)',            '�����ʶ').
      AddF('G_Name',        'varchar(80)',            '��������').
      AddF('G_Index',       'Integer',                '����˳��');
    //popedom group

    AddTable(sTable_Popedom, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼���').
      AddF('P_ID',          'varchar(80)',            '�����ʶ').
      AddF('P_Name',        'varchar(80)',            '��������').
      AddF('P_Group',       'varchar(32)',            '��������').
      AddF('P_Post',        'varchar(32)',            '������λ').
      AddF('P_Popedom',     'varchar(320)',           'Ȩ��ֵ').
      AddF('P_LastModify',  'DateTime',               '�޸�ʱ��').
      //for field
      AddI('idx_id',        'P_Post ASC,P_ID ASC').
      AddI('idx_post_all',  'P_Post ASC');
    //��λP_Post��P_ID����ӵ��P_PopedomȨ��
  end;
end;

//------------------------------------------------------------------------------
//Date: 2021-11-17
//Parm: ��λ��ʶ
//Desc: ��ʼ����λȨ��
function TPopedomPost.Init(const nPost: string): PPopedomPost;
var nInit: TPopedomPost;
begin
  FillChar(nInit, SizeOf(nInit), #0);
  Self := nInit;
  Result := @Self;
end;

//Date: 2021-11-17
//Parm: �����ʶ;��������
//Desc: ��ʼ��Ȩ�޷���
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
//Parm: Ȩ�޶���;��������;��ʹ�õ�Ȩ��
//Desc: ���һ��Ȩ�޶���
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
     gMG.WriteLog(TPopedomManager, 'Ȩ�޹�����', nStr);
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
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
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
  //����ȫ�ֱ���
end;

procedure TPopedomManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TParameterManager', ['TDBManager']);
  //���֧��
  gDBManager.AddTableBuilder(AddPopedomTables);
end;

//Date: 2021-11-16
//Parm: ��ʶ
//Desc: ����nTag��ʶ������
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
//Parm: ��ʶ
//Desc: ����nTag������
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
//Parm: ��ʶ;����
//Desc: ����Ȩ�ޱ�ʶ��
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
//Parm: Ȩ�ޱ�ʶ
//Desc: ����nTag��ʶ��
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
//Parm: ���÷���
//Desc: ����Ȩ�������÷���
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
//Parm: �����ʶ
//Desc: ������ʶΪnID��Ȩ�޷���
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
//Parm: �����ʶ
//Desc: ����nGroup���������
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
//Parm: �����ʶ;����
//Desc: ���һ��Ȩ�޷���
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
//Parm: �б�
//Desc: ��ȡ���е�Ȩ����
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
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nListȨ�����б�
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
