{*******************************************************************************
  ����: dmzn@163.com 2021-06-08
  ����: �����ֵ������

  ��ע:
  &.�����ֵ���Ҫ���ڳ�ʼ��ListView,cxGrid�����ݱ��,�ֵ������ά����һ����֮
    ��ص���������.
  &.�ֵ����������: ����ģ��,ģ���¶��ʵ��,ÿ��ʵ���Ӧһ��������.
  &.�ֵ������ʹ��ProgID����,����ʶ��ǰ����ʵ���������ĳ���.
  &.�ֵ����������ݿ����,���ü�����,�������Ỻ��,����ÿ��ʵ������ֻ����һ��.
  &.��ȡʱ����LoadEntity,���ɹ����ʵ��ᱻ����,ֱ�Ӷ�ȡActiveEntity�Ϳ�����.
*******************************************************************************}
unit UMgrDataDict;

interface

uses
  System.Classes, System.SysUtils, Data.DB, UBaseObject;

type
  TDictFormatStyle = (fsNone, fsFixed, fsSQL, fsCheckBox);
  //��ʽ����ʽ: �̶�����,���ݿ�����

  PDictFormatItem = ^TDictFormatItem;
  TDictFormatItem = record
    FStyle    : TDictFormatStyle;                       //��ʽ
    FData     : string;                                 //����
    FFormat   : string;                                 //��ʽ��
    FExtMemo  : string;                                 //��չ����
  end;

  PDictDBItem = ^TDictDBItem;
  TDictDBItem = record
    FTable    : string;                                 //����
    FField    : string;                                 //�ֶ�
    FIsKey    : Boolean;                                //����

    FType     : TFieldType;                             //��������
    FWidth    : integer;                                //�ֶο��
    FDecimal  : integer;                                //С��λ
  end;

  TDictFooterKind = (fkNone, fkSum, fkMin, fkMax, fkCount, fkAverage);
  //ͳ������: ��,�ϼ�,��С,���,��Ŀ,ƽ��ֵ
  TDictFooterPosition = (fpNone, fpFooter, fpGroup, fpAll);
  //�ϼ�λ��: ҳ��,����,���߶���

  PDictGroupFooter = ^TDictGroupFooter;
  TDictGroupFooter = record
    FDisplay  : string;                                 //��ʾ�ı�
    FFormat   : string;                                 //��ʽ��
    FKind     : TDictFooterKind;                        //�ϼ�����
    FPosition : TDictFooterPosition;                    //�ϼ�λ��
  end;

  PDictItem = ^TDictItem;
  TDictItem = record
    FRecordID : string;                                //��¼��
    FTitle    : string;                                 //����
    FAlign    : TAlignment;                             //����
    FWidth    : integer;                                //���
    FIndex    : integer;                                //˳��
    FVisible  : Boolean;                                //�ɼ�
    FDBItem   : TDictDBItem;                            //���ݿ�
    FFormat   : TDictFormatItem;                        //��ʽ��
    FFooter   : TDictGroupFooter;                       //ҳ�źϼ�
  end;
  TDictItems = array of TDictItem;

  PEntityItem = ^TEntityItem;
  TEntityItem = record
  private
    FEntity   : string;                                 //ʵ����
    FName     : string;                                 //ʵ������
    FLang     : string;                                 //���Ա�ʶ
    FItems    : TDictItems;                             //�ֵ�����(PDictItem)
  public
    function AddDict(const nTitle,nField: string): PEntityItem;
    {*����ֵ���*}
  end;

  TDictItemBuilder = procedure (const nList: TList);
  //for external-system fill entity.items info

  TDataDictManager = class(TManagerBase)
  public
    const
      sTable_DataDict = 'Sys_DataDict';                 //�����ֵ�
  private
    FBuilders: array of TDictItemBuilder;
    {*�ֵ�������Ϣ*}
  protected
    function FindEntity(const nEntity,nLang: string;
      const nList: TList): PEntityItem;
    {*��������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    {*�ӳ�ִ��*}
    procedure AddDictBuilder(const nBuilder: TDictItemBuilder);
    function AddEntity(const nEntity,nName,nLang: string;
      const nList: TList): PEntityItem;
    {*�������*}
    procedure GetDictData(const nList: TList);
    procedure ClearDictData(const nList: TList; const nFree: Boolean = False);
    {*�ֵ�����*}
    function BuilDictSQL(const nEntity: PEntityItem;const nIdx:Integer): string;
    procedure InitDict(const nEntity: PEntityItem;
      const nFirstItem: Boolean = True);
    procedure AddDict(const nEntity: PEntityItem; const nIdx: Integer = 0);
    procedure DelDict(const nEntity: PEntityItem; const nIdx: Integer = 0);
    {*�ֵ���*}
    procedure GetEntity(const nEntity,nLang: string; const nData: PEntityItem);
    {*�ֵ�����*}
    function InitDictData(const nLang: string;
      const nMemo: TStrings = nil): Boolean;
    {*��ʼ������*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
  end;

var
  gDataDictManager: TDataDictManager = nil;
  //ȫ��ʹ��
  
implementation

uses
  UManagerGroup, UDBManager, ULibFun;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TDataDictManager, '�ֵ������', nEvent);
end;

//Desc: ��ӹ����������
procedure AddDataDictTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TDataDictManager do
  begin
    AddTable(sTable_DataDict, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼��ʶ').
      AddF('D_Entity',      'varchar(32)',            '����ʵ��').
      AddF('D_Title',       'varchar(32)',            '���ݱ���').
      AddF('D_Align',       'smallint',               '�������').
      AddF('D_Width',       'integer',                '������').
      AddF('D_Index',       'integer',                '����˳��').
      AddF('D_Visible',     'smallint',               '�Ƿ�ɼ�').
      AddF('D_LangID',      'varchar(5)',             '���Ա�ʶ').
      //normal
      AddF('D_DBTable',     'varchar(32)',            '������').
      AddF('D_DBField',     'varchar(32)',            '�ֶ���').
      AddF('D_DBIsKey',     'smallint',               '�Ƿ�����').
      AddF('D_DBType',      'smallint',               '��������').
      AddF('D_DBWidth',     'smallint',               '�ֶο��').
      AddF('D_DBDecimal',   'smallint',               'С��λ').
      //database
      AddF('D_FmtStyle',    'smallint',               '��ʽ����ʽ').
      AddF('D_FmtData',     'varchar(200)',           '��ʽ������').
      AddF('D_FmtFormat',   'varchar(100)',           '��ʽ������').
      AddF('D_FmtExtMemo',  'varchar(100)',           '��ʽ����չ').
      //format
      AddF('D_FteDisplay',  'varChar(50)',            'ͳ����ʾ�ı�').
      AddF('D_FteFormat',   'varChar(50)',            'ͳ�Ƹ�ʽ��').
      AddF('D_FteKind',     'smallint',               'ͳ������').
      AddF('D_FtePositon',  'smallint',               'ͳ����ʾλ��').
      //footer
      AddI('idx_entity', 'CREATE INDEX $IDX ON $TBS(D_Entity ASC,D_LangID ASC)');
      //for index;
  end;
end;

//Date: 2021-06-17
//Parm: ����;�ֶ�
//Desc: ����ֵ���
function TEntityItem.AddDict(const nTitle, nField: string): PEntityItem;
var nStr: string;
    nIdx: Integer;
    nInit: TDictItem;
begin
  Result := @Self;
  //return self address

  for nIdx := Low(FItems) to High(FItems) do
   if (CompareText(nTitle, FItems[nIdx].FTitle) = 0) and
      (CompareText(nField, FItems[nIdx].FDBItem.FField) = 0) then
   begin
     nStr := 'TDataDictManager.AddDict: %s.%s Has Exists.';
     nStr := Format(nStr, [nField, nTitle]);

     WriteLog(nStr);
     raise Exception.Create(nStr);
   end;

  FillChar(nInit, SizeOf(TDictItem), #0);
  //default item

  nIdx := Length(FItems);
  SetLength(FItems, nIdx + 1);
  FItems[nIdx] := nInit;

  with FItems[nIdx] do
  begin
    FVisible := True;
    FTitle := nTitle;
    FDBItem.FField := nField;
  end;
end;

//------------------------------------------------------------------------------
constructor TDataDictManager.Create;
begin
  inherited;
  SetLength(FBuilders, 0);
end;

destructor TDataDictManager.Destroy;
begin
  //call RunBeforUnregistAllManager
  inherited;
end;

//Date: 2021-06-08
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
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
  //����ȫ�ֱ���
end;

procedure TDataDictManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TDataDictManager', ['TDBManager']);
  //���֧��
  gDBManager.AddTableBuilder(AddDataDictTables);
end;

//Date: 2021-06-17
//Parm: ���÷���
//Desc: �����ֵ����÷���
procedure TDataDictManager.AddDictBuilder(const nBuilder: TDictItemBuilder);
var nIdx: Integer;
begin
  for nIdx := Low(FBuilders) to High(FBuilders) do
    if @FBuilders[nIdx] = @nBuilder then Exit;
  //has exists

  nIdx := Length(FBuilders);
  SetLength(FBuilders, nIdx + 1);
  FBuilders[nIdx] := nBuilder;
end;

//Date: 2021-06-17
//Parm: �б�
//Desc: ��ȡ�ֵ�����
procedure TDataDictManager.GetDictData(const nList: TList);
var nIdx: Integer;
begin
  nList.Clear;
  //init first

  for nIdx := Low(FBuilders) to High(FBuilders) do
    FBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2021-06-17
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nList�ֵ�����
procedure TDataDictManager.ClearDictData(const nList:TList;const nFree:Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx := nList.Count - 1 downto 0 do
      Dispose(PEntityItem(nList[nIdx]));
    //xxxxx

    if nFree then
         nList.Free
    else nList.Clear;
  end;
end;

//Date: 2021-06-17
//Parm: ʵ����;���Ա�ʶ;�б�
//Desc: ��nList�м���nEntity.nLangʵ��
function TDataDictManager.FindEntity(const nEntity,nLang: string;
  const nList: TList): PEntityItem;
var nIdx: Integer;
    nEty: PEntityItem;
begin
  Result := nil;
  for nIdx := nList.Count - 1 downto 0 do
  begin
    nEty := nList[nIdx];
    if (CompareText(nEntity, nEty.FEntity) = 0) and
       (CompareText(nLang, nEty.FLang) = 0) then
    begin
      Result := nEty;
      Break;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: ʵ���ʶ;����;����
//Desc: ��nList������//��ʶΪnEntity�ֵ�ʵ��
function TDataDictManager.AddEntity(const nEntity, nName, nLang: string;
  const nList: TList): PEntityItem;
begin
  Result := FindEntity(nEntity, nLang, nList);
  if not Assigned(Result) then
  begin
    New(Result);
    nList.Add(Result);

    InitDict(Result, False);
    with Result^ do
    begin
      FEntity := nEntity;
      FName   := nName;
      FLang   := nLang;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: ʵ��;�ֵ�������
//Desc: ����nEntity.FItems[nIdx]��insert,update���
function TDataDictManager.BuilDictSQL(const nEntity: PEntityItem;
  const nIdx: Integer): string;
var nStr: string;
begin
  if (nIdx < Low(nEntity.FItems)) or (nIdx > High(nEntity.FItems)) then
  begin
    nStr := 'TDataDictManager.BuilItemSQL: Index Out of Bounds.';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  with nEntity.FItems[nIdx], TSQLBuilder do
  begin
    Result := MakeSQLByStr([
      SF('D_Entity', nEntity.FEntity),
      SF('D_Title', nEntity.FName),
      SF('D_Align', Ord(FAlign), sfVal),
      SF('D_Width', FWidth, sfVal),
      SF('D_Index', FIndex, sfVal),
      SF('D_Visible', BoolToStr(FVisible), sfVal),
      SF('D_LangID', nEntity.FLang),
      //normal
      SF('D_DBTable', FDBItem.FTable),
      SF('D_DBField', FDBItem.FField),
      SF('D_DBIsKey', BoolToStr(FDBItem.FIsKey), sfVal),
      SF('D_DBType', Ord(FDBItem.FType), sfVal),
      SF('D_DBWidth', FDBItem.FWidth, sfVal),
      SF('D_DBDecimal', FDBItem.FDecimal, sfVal),
      //database
      SF('D_FmtStyle', Ord(FFormat.FStyle), sfVal),
      SF('D_FmtData', FFormat.FData),
      SF('D_FmtFormat', FFormat.FFormat),
      SF('D_FmtExtMemo', FFormat.FExtMemo),
      //format
      SF('D_FteDisplay', FFooter.FDisplay),
      SF('D_FteFormat', FFooter.FFormat),
      SF('D_FteKind', Ord(FFooter.FKind), sfVal),
      SF('D_FtePositon', Ord(FFooter.FPosition), sfVal)
      //footer
    ], sTable_DataDict, SF('R_ID', FRecordID, sfVal), FRecordID = '');
  end;
end;

//Date: 2021-06-18
//Parm: ����;���
//Desc: ��ʼ�����ݿ��е��ֵ�����
function TDataDictManager.InitDictData(const nLang: string;
  const nMemo: TStrings): Boolean;
begin

end;

//Date: 2021-06-17
//Parm: ʵ��;�׸��ֵ���
//Desc: ��ʼ��nEntity��Ĭ���ֵ���
procedure TDataDictManager.InitDict(const nEntity: PEntityItem;
  const nFirstItem: Boolean);
var nEty: TEntityItem;
    nDict: TDictItem;
begin
  FillChar(nEty, SizeOf(TEntityItem), #0);
  nEntity^ := nEty;

  if nFirstItem then
  begin
    FillChar(nDict, SizeOf(TDictItem), #0);
    with nDict do
    begin
      FVisible := True;
      FIndex   := -1;
    end;

    SetLength(nEntity.FItems, 0);
    nEntity.FItems[0] := nDict;
  end;
end;

//Date: 2021-06-17
//Parm: �ֵ���
//Desc: �����ֵ���
procedure TDataDictManager.AddDict(const nEntity: PEntityItem;
  const nIdx: Integer);
var nStr: string;
    nQuery: TDataSet;
begin
  if (nEntity.FEntity = '') or (nEntity.FLang = '') then
  begin
    nStr := 'TDataDictManager.AddItem: Entity or Lang Is Null';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if (nIdx < Low(nEntity.FItems)) or (nIdx > High(nEntity.FItems)) then
  begin
    nStr := 'TDataDictManager.AddItem: Index Out of Bounds.';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if nEntity.FItems[nIdx].FRecordID <> '' then
  begin
    nStr := BuilDictSQL(nEntity, nIdx);
    gDBManager.DBExecute(nStr);
    Exit;
  end;

  nQuery := nil;
  try
    if nEntity.FItems[nIdx].FIndex < 0 then
    begin
      nStr := 'Select Top 1 D_Index From %s ' +
              'Where D_Entity=''%s'' And D_LangID=''%s'' ' +
              'Order By D_Index DESC';
      nStr := Format(nStr, [sTable_DataDict, nEntity.FEntity, nEntity.FLang]);

      nQuery := gDBManager.DBQuery(nStr);
      if nQuery.RecordCount < 1 then
           nEntity.FItems[nIdx].FIndex := 0
      else nEntity.FItems[nIdx].FIndex := nQuery.Fields[0].AsInteger + 1;
    end;

    nStr := BuilDictSQL(nEntity, nIdx);
    gDBManager.DBExecute(nStr, nQuery);
  finally
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-06-17
//Parm: �ֵ���
//Desc: ɾ���ֵ���
procedure TDataDictManager.DelDict(const nEntity: PEntityItem;
 const nIdx: Integer);
var nStr: string;
begin
  if (nIdx < Low(nEntity.FItems)) or (nIdx > High(nEntity.FItems)) then
  begin
    nStr := 'TDataDictManager.DelItem: Index Out of Bounds.';
    WriteLog(nStr);
    raise Exception.Create(nStr);
  end;

  if nEntity.FItems[nIdx].FRecordID <> '' then
  begin
    nStr := 'Delete From %s Where R_ID=%s';
    nStr := Format(nStr, [sTable_DataDict, nEntity.FItems[nIdx].FRecordID]);
    gDBManager.DBExecute(nStr);
  end;
end;

//Date: 2021-06-18
//Parm: ʵ��;����;����
//Desc: ����nEntity.nLang���ֵ�����
procedure TDataDictManager.GetEntity(const nEntity,nLang: string;
  const nData: PEntityItem);
var nStr: string;
    nIdx: Integer;
    nQuery: TDataSet;
    nDefItem: TDictItem;
begin
  InitDict(nData, False);
  //init
  nQuery := nil;

  with TApplicationHelper,TStringHelper do
  try
    nStr := 'Select * From %s ' +
            'Where D_Entity=''%s'' And D_LangID=''%s'' ' +
            'Order By D_Index ASC';
    nStr := Format(nStr, [sTable_DataDict, nEntity, nLang]);

    nQuery := gDBManager.DBQuery(nStr);
    with nQuery do
    begin
      if RecordCount < 1 then Exit;
      //no data
      SetLength(nData.FItems, RecordCount);

      FillChar(nDefItem, SizeOf(TDictItem), #0);
      nIdx := 0;
      First;

      nData.FEntity := FieldByName('D_Entity').AsString;
      nData.FName   := FieldByName('D_Title').AsString;
      nData.FLang   := FieldByName('D_LangID').AsString;

      while not Eof do
      begin
        nData.FItems[nIdx] := nDefItem;
        //init

        with nData.FItems[nIdx] do
        begin
          FRecordID := FieldByName('').AsString;
          FTitle    := FieldByName('').AsString;
          FAlign    := TAlignment(FieldByName('').AsInteger);
          FWidth    := FieldByName('').AsInteger;
          FIndex    := FieldByName('').AsInteger;
          FVisible  := StrToBool(FieldByName('').AsString);
          //normal
          FDBItem.FTable := FieldByName('D_DBTable').AsString;
          FDBItem.FField := FieldByName('D_DBField').AsString;
          FDBItem.FIsKey := StrToBool(FieldByName('D_DBIsKey').AsString);
          FDBItem.FType  := TFieldType(FieldByName('D_DBType').AsInteger);
          FDBItem.FWidth := FieldByName('D_DBWidth').AsInteger;
          FDBItem.FDecimal := FieldByName('D_DBDecimal').AsInteger;
          //database
          FFormat.FStyle := TDictFormatStyle(FieldByName('D_FmtStyle').AsInteger);
          FFormat.FData  := FieldByName('D_FmtData').AsString;
          FFormat.FFormat:= FieldByName('D_FmtFormat').AsString;
          FFormat.FExtMemo := FieldByName('D_FmtExtMemo').AsString;
          //format
          FFooter.FDisplay := FieldByName('D_FteDisplay').AsString;
          FFooter.FFormat := FieldByName('D_FteFormat').AsString;
          FFooter.FKind := TDictFooterKind(FieldByName('D_FteKind').AsInteger);
          FFooter.FPosition := TDictFooterPosition(FieldByName('D_FtePositon').AsInteger);
          //footer
        end;

        Inc(nIdx);
        Next;
      end;
    end;
  finally
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-06-08
//Parm: �б�;�Ƿ��Ѻ���ʾ
//Desc: ��������״̬���ݴ���nList
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

end.
