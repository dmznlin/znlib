{*******************************************************************************
  ����: dmzn@163.com 2020-04-21
  ����: �˵�������

  ��ע:
  *.������֧��.
  *.������ά���˵���,��֤ϵͳ�˵�����.ԭ��ʹ�ò˵��༭���ķ���,�ᵼ�²������.
  *.����ProgID: �������ֲ˵�������ϵͳ.
  *.ʵ��Entity: ��������ͬϵͳ�ڵĲ�ͬ�˵�,�������˵��Ϳ�ݲ˵�.
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

  sMenuTypeText: array[TMenuItemType] of string = ('Ĭ��', '����', 'ʵ��',
    '���˵�', '�˵���', '������');
  //menu type desc

  sMenuItemDefault = '#*#';
  //default flag

type
  TMenuData = record
    FID           : string;                              //��ʶ
    FName         : string;                              //����
    FData         : string;                              //����
    FFlag         : string;                              //���Ӳ���
  end;
  TMenuDataList = array of TMenuData;                    //�����б�

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    FType         : TMenuItemType;                       //�˵�����
    FProgID       : string;                              //�����ʶ
    FEntity       : string;                              //ʵ���ʶ
    FMenuID       : string;                              //�˵���ʶ
    FPMenu        : string;                              //�ϼ��˵�
    FTitle        : string;                              //�˵�����
    FImgIndex     : integer;                             //ͼ������
    FFlag         : string;                              //���Ӳ���(�»���..)
    FAction       : string;                              //�˵�����
    FFilter       : string;                              //��������
    FLang         : string;                              //���Ա�ʶ
    FNewOrder     : Single;                              //��������
  end;

  TMenuItems = array of TMenuItem;                       //�˵��б�

  PMenuEntity = ^TMenuEntity;
  TMenuEntity = record
  private
    FDefaultPMenu : string;                              //Ĭ���ϼ�
    FDefaultType  : TMenuItemType;                       //Ĭ������
    {*������Ϣ*}
    FProgID       : string;                              //�����ʶ
    FEntity       : string;                              //ʵ���ʶ
    FItems        : TMenuItems;                          //�˵���
    {*�˵�����*}
  public
    function SetParent(const nPMenu: string): PMenuEntity;
    function SetType(const nType: TMenuItemType): PMenuEntity;
    {*��������*}
    function AddM(const nID,nTitle: string; const nAction: string = '';
      nPMenu: string = sMenuItemDefault;
      const nFilter: string = ''; nType: TMenuItemType = mtDefault): PMenuEntity;
    {*��Ӳ˵�*}
  end;

  TMenuItemBuilder = procedure (const nList: TList);
  //for external-system fill menu.item info

  TMenuManager = class(TManagerBase)
  private
    FMultiLang: TMenuDataList;
    {*�������б�*}
    FMenuBuilders: array of TMenuItemBuilder;
    {*�˵�������Ϣ*}
  protected
    function FindEntity(const nProg,nEntity: string; const nList: TList): PMenuEntity;
    {*��������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    {*�ӳ�ִ��*}
    procedure AddLanguage(const nID,nName: string);
    procedure DeleteLanguage(const nID: string);
    procedure LoadLanguage(const nQuery: TADOQuery = nil);
    {*���Թ���*}
    procedure AddMenuBuilder(const nBuilder: TMenuItemBuilder);
    function AddEntity(const nProg,nEntity: string; const nList: TList): PMenuEntity;
    {*�������*}
    procedure GetMenus(const nList: TList);
    procedure ClearMenus(const nList: TList; const nFree: Boolean = False);
    {*��ȡ����Ϣ*}
    function InitMenus(const nMemo: TStrings = nil): Boolean;
    {*��ʼ������*}
    property MultiLanguage: TMenuDataList read FMultiLang;
    {*�������*}
  end;

var
  gMenuManager: TMenuManager = nil;
  //ȫ��ʹ��

implementation

uses
  ULibFun, UManagerGroup, UDBManager;

const
  sTableLang        = 'Sys_Lang';         //����������
  sTableMenu        = 'Sys_Menus';        //�˵�����

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TMenuManager, '�˵�������', nEvent);
end;

//Desc: ��ӹ����������
procedure AddMenuTables(const nList: TList);
begin
  with gMG.FDBManager,TSQLBuilder do
  AddTable(sTableLang, nList, dtMSSQL).
  AddF('L_ID',          'varChar(5)',             '���Ա�ʶ').
  AddF('L_Name',        'varChar(32)',            '��������').
  AddF('L_Valid',       'Char(1)',                '�Ƿ���Ч', '''Y''').
  AddR('en', MakeSQLByStr([
    SF('L_ID', 'en'),
    SF('L_Name', 'English'),
    SF('L_Valid', sFlag_Disabled)], sTableLang)).
  AddR('cn', MakeSQLByStr([
    SF('L_ID', 'cn'),
    SF('L_Name', '��������')], sTableLang));
  //for language

  gMG.FDBManager.AddTable(sTableMenu, nList, dtMSSQL).
  AddF('R_ID',          sField_SQLServer_AutoInc, '��¼��ʶ').
  AddF('M_Prog',        'varchar(15)',            '�����ʶ').
  AddF('M_Entity',      'varchar(15)',            'ʵ���ʶ').
  AddF('M_PMenu',       'varchar(15)',            '�ϼ��˵�').
  AddF('M_MenuID',      'varchar(15)',            '�˵���ʶ').
  AddF('M_Title',       'varchar(50)',            '�˵�����').
  AddF('M_Action',      'varchar(100)',           '�˵�����').
  AddF('M_Flag',        'varchar(20)',            '���Ӳ���').
  AddF('M_Filter',      'varchar(100)',           '��������').
  AddF('M_Type',        'varchar(5)',             '���ͱ�ʶ').
  AddF('M_Lang',        'varchar(5)',             '���Ա�ʶ').
  AddF('M_ImgIndex',    'integer default -1',     'ͼ������', '-1').
  AddF('M_NewOrder',    'float default 0',        '��������', '0').
  //for field
  AddI('idx_prog', 'CREATE INDEX idx_prog ON $TB.* (M_Prog ASC)');
  //for index
end;

//------------------------------------------------------------------------------
//Date: 2020-04-24
//Parm: �˵���ʶ
//Desc: ���õ�ǰĬ�ϸ����˵�
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

  raise Exception.Create(Format('SetParent: ���˵� %s ������.', [nPMenu]));
end;

//Date: 2020-04-24
//Parm: �˵�����
//Desc: ����Ĭ������
function TMenuEntity.SetType(const nType: TMenuItemType): PMenuEntity;
begin
  Result := @Self;
  //return self address

  if nType = mtDefault then
       FDefaultType := mtItem
  else FDefaultType := nType;
end;

//Date: 2020-04-22
//Parm: �˵���ʶ;�˵�����;���˵�
//Desc: �����˵���
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
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
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
  //����ȫ�ֱ���
end;

procedure TMenuManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TMenuManager', ['TDBManager']);
  //���֧��
  gMG.FDBManager.AddTableBuilder(AddMenuTables);
end;

//Date: 2020-04-21
//Parm: ���Ա�ʶ;��������;
//Desc: ��Ӷ�������
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
//Parm: ���Ա�ʶ
//Desc: ɾ����ʶΪnID������
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
//Desc: ����������б�
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
//Parm: ���÷���
//Desc: �����˵������÷���
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
//Parm: �����ʶ;ʵ���ʶ;�б�
//Desc: ��nList�м���nProg.nEntityʵ��
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
//Parm: �����ʶ;ʵ���ʶ;�б�
//Desc: ��nList��//����nProg.nEntityʵ��
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
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nListʵ����Ϣ
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
//Parm: �б�
//Desc: ��ȡ�˵�ʵ����Ϣ
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
//Parm: ���
//Desc: �������г�ʼ��
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
      WriteLog('InitMenus: δ��������', nMemo);
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

    WriteLog('::: �����˵����� :::', nMemo);
    for nIdx := 0 to nMenus.Count -1 do
    begin
      nEntity := nMenus[nIdx];
      //entity item
      WriteLog('����ʵ��: ' + nEntity.FProgID + '.' + nEntity.FEntity);

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
          WriteLog('�Ѵ���: ' + nStr, nMemo);
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
