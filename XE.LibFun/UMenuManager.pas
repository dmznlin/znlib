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
  System.Classes, System.SysUtils, System.Generics.Collections, UBaseObject;

type
  TMenuItemType = (mtProg, mtEntity, mtParent, mtItem, mtAssist);
  //menu item

const
  sMenuTypeID: array[TMenuItemType] of string = ('pg', 'et', 'pr', 'mn', 'as');
  //menu type id

  sMenuTypeText: array[TMenuItemType] of string = ('����', 'ʵ��', '���˵�',
    '�˵���', '������');
  //menu type desc

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
    FProgID       : string;                              //�����ʶ
    FEntity       : string;                              //ʵ���ʶ
    FItems        : TMenuItems;                          //�˵���
    {*�˵�����*}
    function AddM(const nID,nTitle: string; const nPMenu: string = '';
      const nAction: string = ''; const nFilter: string = ''): PMenuEntity;
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
    procedure AddLang(const nID,nName: string);
    procedure AddMenuBuilder(const nBuilder: TMenuItemBuilder);
    function AddEntity(const nProg,nEntity: string; const nList: TList): PMenuEntity;
    {*�������*}
    procedure GetMenus(const nList: TList);
    procedure ClearMenus(const nList: TList; const nFree: Boolean = False);
    {*��ȡ����Ϣ*}
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
  sTableMenuItem    = 'Sys_MenuItem';     //�˵�������

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
  AddR('cn', MakeSQLByStr([
    SF('L_ID', 'cn'),
    SF('L_Name', '��������')], sTableLang));
  //for language

  gMG.FDBManager.AddTable(sTableMenuItem, nList, dtMSSQL).
  AddF('R_ID',          sField_SQLServer_AutoInc, '��¼��ʶ').
  AddF('M_Prog',        'varchar(15)',            '�����ʶ').
  AddF('M_Entity',      'varchar(15)',            'ʵ���ʶ').
  AddF('M_MenuID',      'varchar(15)',            '�˵���ʶ').
  AddF('M_Title',       'varchar(50)',            '�˵�����').
  AddF('M_Flag',        'varchar(20)',            '���Ӳ���').
  AddF('M_Action',      'varchar(100)',           '�˵�����').
  AddF('M_Filter',      'varchar(100)',           '��������').
  AddF('M_Type',        'varchar(5)',             '���ͱ�ʶ').
  AddF('M_Lang',        'varchar(5)',             '���Ա�ʶ').
  AddF('M_ImgIndex',    'integer default 0',      'ͼ������', '0').
  AddF('M_NewOrder',    'float default 0',        '��������', '0').
  //for field
  Copy(sTableMenu).
  //for table
  AddI('idx_prog', 'CREATE NONCLUSTERED INDEX idx_prog ON $TB.* (M_Prog ASC)');
  //for index
end;

//Date: 2020-04-22
//Parm: �˵���ʶ;�˵�����;���˵�
//Desc: �����˵���
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
  end;
end;

//Date: 2020-04-22
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nListʵ����Ϣ
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
//Parm: �б�
//Desc: ��ȡ�˵�ʵ����Ϣ
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
