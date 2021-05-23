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
  System.Classes, System.SysUtils, System.Generics.Collections, Data.DB,
  UBaseObject, ULibFun;

type
  TMenuItemType = (mtDefault, mtProg, mtEntity, mtItem, mtAssist);
  //menu item

const
  sMenuTypeText: array[TMenuItemType] of string = ('Ĭ��', '����', 'ʵ��',
    '�˵���', '������');
  //menu type desc

type
  TMenuAction = (maDefault, maNewForm, maNewFrame, maExecute);
  //�˵�����: Ĭ��,�´���,��ҳ��,ִ������

  TMenuData = record
    FID           : string;                              //��ʶ
    FName         : string;                              //����
    FData         : string;                              //����
    FFlag         : string;                              //���Ӳ���
  end;
  TMenuDataList = array of TMenuData;                    //�����б�

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    FProgID       : string;                              //�����ʶ
    FEntity       : string;                              //ʵ���ʶ
    FMenuID       : string;                              //�˵���ʶ
    FPMenu        : string;                              //�ϼ��˵�
    FTitle        : string;                              //�˵�����
    FAction       : TMenuAction;                         //�˵�����
    FActionData   : string;                              //��������
    FFlag         : string;                              //���Ӳ���(�»���..)
    FImgIndex     : integer;                             //ͼ������
    FLang         : string;                              //���Ա�ʶ
    FNewOrder     : Single;                              //��������

    FRecordID     : string;                              //��¼���(DB)
    FUserID       : string;                              //�����û�
    FType         : TMenuItemType;                       //�˵�����
    FDeploy       : TApplicationHelper.TDeployTypes;     //��������
    FSubItems     : TList;                               //�Ӳ˵��б�
  end;

  TMenuItems = array of TMenuItem;                       //�˵��б�

  PMenuEntity = ^TMenuEntity;
  TMenuEntity = record
  private
    FDefaultPMenu : string;                              //Ĭ���ϼ�
    FDefaultType  : TMenuItemType;                       //Ĭ������
    FDefaultDeploy : TApplicationHelper.TDeployTypes;    //Ĭ�ϲ���
    {*������Ϣ*}
    FProgID       : string;                              //�����ʶ
    FEntity       : string;                              //ʵ���ʶ
    FItems        : TMenuItems;                          //�˵���
    {*�˵�����*}
  public
    function SetParent(const nPMenu: string): PMenuEntity;
    function SetType(const nType: TMenuItemType): PMenuEntity;
    function SetDeploy(const nDeploy: TApplicationHelper.TDeployTypes): PMenuEntity;
    {*��������*}
    function AddM(const nID,nTitle: string;
      const nAction: TMenuAction = maDefault;
      const nActionData: string = '';
      nDeploy: TApplicationHelper.TDeployTypes = []): PMenuEntity;
    {*��Ӳ˵�*}
  end;

  TMenuItemBuilder = procedure (const nList: TList);
  //for external-system fill menu.item info

  TMenuManager = class(TManagerBase)
  public
    const
      sTable_Lang = 'Sys_Lang';                          //��������
      sTable_Menu = 'Sys_Menus';                         //�˵�����
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
    procedure LoadLanguage(const nQuery: TDataSet = nil);
    {*���Թ���*}
    procedure AddMenuBuilder(const nBuilder: TMenuItemBuilder);
    function AddEntity(const nProg,nProgName,nEntity,nEntityName: string;
      const nList: TList): PMenuEntity;
    {*�������*}
    procedure GetMenuData(const nList: TList);
    procedure ClearMenuData(const nList: TList; const nFree: Boolean = False);
    {*�˵�����*}
    procedure GetMenus(const nDeploy: TApplicationHelper.TDeployType;
      const nProg,nEntity,nLang: string;
      const nList: TList; const nUser: string = '');
    procedure ClearMenus(const nList: TList; const nFree: Boolean = False);
    {*�˵���Ϣ*}
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
  UManagerGroup, UDBManager;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TMenuManager, '�˵�������', nEvent);
end;

//Desc: ��ӹ����������
procedure AddMenuTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TMenuManager do
  begin
    AddTable(sTable_Lang, nList, dtMSSQL).
      AddF('L_ID',          'varChar(5)',             '���Ա�ʶ').
      AddF('L_Name',        'varChar(32)',            '��������').
      AddF('L_Valid',       'Char(1)',                '�Ƿ���Ч', SQM(sFlag_Enabled)).
      AddR('en', MakeSQLByStr([
        SF('L_ID', 'en'),
        SF('L_Name', 'English'),
        SF('L_Valid', sFlag_Disabled)], sTable_Lang)).
      AddR('cn', MakeSQLByStr([
        SF('L_ID', 'cn'),
        SF('L_Name', '��������')], sTable_Lang));
    //for language

    AddTable(sTable_Menu, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼��ʶ').
      AddF('M_ProgID',      'varchar(15)',            '�����ʶ').
      AddF('M_Entity',      'varchar(15)',            'ʵ���ʶ').
      AddF('M_MenuID',      'varchar(15)',            '�˵���ʶ').
      AddF('M_PMenu',       'varchar(15)',            '�ϼ��˵�').
      AddF('M_Title',       'varchar(50)',            '�˵�����').
      AddF('M_Action',      'varchar(15)',            '�˵�����').
      AddF('M_Data',        'varchar(320)',           '��������').
      AddF('M_Flag',        'varchar(20)',            '���Ӳ���').
      AddF('M_Type',        'varchar(15)',            '�˵�����').
      AddF('M_Lang',        'varchar(5)',             '���Ա�ʶ').
      AddF('M_UserID',      'varchar(32)',            '�û���ʶ').
      AddF('M_Deploy',      'varchar(32)',            '��������(Desktop,Web)').
      AddF('M_ImgIndex',    'integer default -1',     'ͼ������', '-1').
      AddF('M_NewOrder',    'float default 0',        '��������', '0').
      //for field
      AddI('idx_prog', 'CREATE INDEX $IDX ON $TBS(M_ProgID ASC,M_Entity ASC)');
      //for index
  end;
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

//Date: 2021-05-18
//Parm: ����ʽ
//Desc: ����Ĭ�ϲ���ʽ
function TMenuEntity.SetDeploy(
  const nDeploy: TApplicationHelper.TDeployTypes): PMenuEntity;
begin
  Result := @Self;
  //return self address
  FDefaultDeploy := nDeploy;
end;

//Date: 2020-04-22
//Parm: �˵���ʶ;�˵�����;����;��������;����ʽ
//Desc: �����˵���
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
  //new menu item

  with FItems[nIdx] do
  begin
    FType         := FDefaultType;
    FProgID       := FProgID;
    FEntity       := FEntity;

    FMenuID       := nID;
    FPMenu        := FDefaultPMenu;
    FTitle        := nTitle;
    FAction       := nAction;
    FActionData   := nActionData;
    FDeploy       := nDeploy;

    FFlag         := '';
    FLang         := '';
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
  gDBManager.AddTableBuilder(AddMenuTables);
end;

//Date: 2020-04-21
//Parm: ���Ա�ʶ;��������;
//Desc: ��Ӷ�������
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
//Parm: ���Ա�ʶ
//Desc: ɾ����ʶΪnID������
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
//Desc: ����������б�
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
//Desc: ��nList������nProg.nEntityʵ��
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
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nListʵ����Ϣ
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
//Parm: �б�
//Desc: ��ȡ�˵�ʵ����Ϣ
procedure TMenuManager.GetMenuData(const nList: TList);
var nIdx: Integer;
begin
  nList.Clear;
  //init first

  for nIdx := Low(FMenuBuilders) to High(FMenuBuilders) do
    FMenuBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2020-04-24
//Parm: ���
//Desc: �������г�ʼ��
function TMenuManager.InitMenus(const nMemo: TStrings): Boolean;
var nStr,nSQL,nTmp: string;
    i,j,nIdx: Integer;
    nMenus: TList;
    nListA,nListB: TStrings;

    nQuery: TDataSet;
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

  with gDBManager do
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

    nSQL := 'Select M_ProgID,M_Entity,M_MenuID,M_Lang From %s ' +
            'Where M_UserID Is Null';
    nSQL := Format(nSQL, [sTable_Menu]);

    with DBQuery(nSQL, nQuery) do
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
    GetMenuData(nMenus);
    //get menus data

    WriteLog('::: �����˵����� :::', nMemo);
    for nIdx := 0 to nMenus.Count -1 do
    begin
      nEntity := nMenus[nIdx];
      //entity item
      WriteLog('����ʵ��: ' + nEntity.FProgID + '.' + nEntity.FEntity, nMemo);

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

          with TApplicationHelper do
           nTmp := TStringHelper.Set2Str<TDeployType, TDeployTypes>(FDeploy);
          //deploy

          nSQL := MakeSQLByStr([
            SF('M_ProgID', nEntity.FProgID),
            SF('M_PMenu', FPMenu),
            SF('M_Title', FTitle),
            SF('M_Action', Enum2Str(FAction)),
            SF('M_Data', FActionData),
            SF('M_Flag', FFlag),
            SF('M_Type', Enum2Str(FType)),
            SF('M_Deploy', nTmp),
            SF('M_Lang', FMultiLang[i].FID),
            SF('M_NewOrder', j, sfVal),

            SF_IF([SF('M_Entity', ''),
                   SF('M_Entity', nEntity.FEntity)], FType = mtProg),
            //program no entity

            SF_IF([SF('M_MenuID', ''),
                   SF('M_MenuID', FMenuID)], FType in [mtProg, mtEntity])
            //program and entity no id
          ], sTable_Menu);
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
    gDBManager.ReleaseDBQuery(nQuery);

    ClearMenuData(nMenus);
    gMG.FObjectPool.Release(nMenus);
    //menu list
  end;
end;

//Date: 2021-05-19
//Parm: �˵����б�
//Desc: ����nList�б�
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
//Parm: ����ʽ;����;ʵ��;�û�;����
//Desc: ����nUser.nLang��Ӧ��nProg.nEntity����״�˵��б�����
procedure TMenuManager.GetMenus(const nDeploy: TApplicationHelper.TDeployType;
  const nProg, nEntity, nLang: string; const nList: TList; const nUser: string);
var nStr: string;
    nQuery: TDataSet;
    nType: TMenuItemType;
    nItem,nPItem: PMenuItem;
    nDPType: TApplicationHelper.TDeployTypes;

    //Date: 2021-05-19
    //Parm: ���˵���;�ڵ��б�;��ǰ�㼶
    //Desc: ��nPList�м�������ΪnPMenu�Ĳ˵���
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
        //����ƥ��
        
        if nType in [mtProg, mtEntity] then //�����ʵ��: Ѱ�ҳ���˵���
        begin
          nBool := (nPItem.FType = mtProg) and
                   (CompareText(nPItem.FProgID, nProg) = 0);
          //xxxxx
        end else

        if nPName = '' then //һ���˵�: Ѱ��ʵ��
        begin
          nBool := (nPItem.FType = mtEntity) and
                   (CompareText(nPItem.FProgID, nProg) = 0) and
                   (CompareText(nPItem.FEntity, nEntity) = 0);
          //xxxxx
        end else //�˵���: ����MenuIDƥ��
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
      //�����ʵ��ɨ���һ��

      for nIdx := nPList.Count - 1 downto 0 do
      begin
        nPItem := nPList[nIdx];
        if CompareText(nPItem.FLang, nLang) <> 0 then Continue;
        //����ƥ��

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
        if (nDPType <> []) and (not (nDeploy in nDPType)) then Continue;
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

          nStr          := FieldByName('M_Action').AsString;
          FAction       := TStringHelper.Str2Enum<TMenuAction>(nStr);
          FActionData   := FieldByName('M_Data').AsString;
          FFlag         := FieldByName('M_Flag').AsString;
          FImgIndex     := FieldByName('M_ImgIndex').AsInteger;
          FLang         := FieldByName('M_Lang').AsString;
          FUserID       := FieldByName('M_UserID').AsString;
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

end.
