{*******************************************************************************
  ����: dmzn@163.com 2021-06-08
  ����: �����ֵ������

  ʹ�÷���:
  1.���Builder
    procedure SytemDictBuilder(const nList: TList);
    var nEty: PDictEntity;
    begin
      nEty := gDataDictManager.AddEntity('Main_A01', '������־', nList);
      nEty.AddDict('R_ID',        '��ʶ').
           AddDict('L_Name',      '����').
           AddDict('L_Owner',     'ӵ����').
           AddDict('L_Index',     '˳��');
      //����ֵ���

      with nEty.ByField('R_ID').FFooter do
      begin
        FDisplay  := 'total:';
        FFormat   := '�ϼ�: �� 0 ��';
        FKind     := fkCount;
        FPosition := fpAll;
      end; //��չ�ֵ���
    end;

    gDataDictManager.AddDictBuilder(SytemDictBuilder);
    //�����������
    
  2.��ʼ���ֵ���
    gDataDictManager.InitDictData('cn');  //�����ֵ���
    gDataDictManager.InitDictData('en');  //Ӣ���ֵ���

  3.�����ֵ���
    var nEntity: TDictEntity;
    gDataDictManager.GetEntity('Main_A01', 'cn', @nEntity);
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
    FRecordID : string;                                 //��¼��
    FTitle    : string;                                 //����
    FAlign    : TAlignment;                             //����
    FWidth    : integer;                                //���
    FIndex    : integer;                                //˳��
    FVisible  : Boolean;                                //�ɼ�
    FDBItem   : TDictDBItem;                            //���ݿ�
    FFormat   : TDictFormatItem;                        //��ʽ��
    FFooter   : TDictGroupFooter;                       //ҳ�źϼ�
  public
    procedure Init();
    {*��ʼ��*}
  end;
  TDictItems = array of TDictItem;

  PDictEntity = ^TDictEntity;
  TDictEntity = record
    FEntity   : string;                                 //ʵ����
    FName     : string;                                 //ʵ������
    FLang     : string;                                 //���Ա�ʶ
    FItems    : TDictItems;                             //�ֵ�����(PDictItem)
  public
    procedure Init(const nFirstItem: Boolean = False);
    {*��ʼ��*}
    function AddDict(const nField,nTitle: string): PDictEntity;
    {*����ֵ���*}    
    function ByTitle(const nTitle: string): PDictItem;
    function ByField(const nField: string): PDictItem;
    function FindItem(const nData: string; const nMode: Byte): PDictItem;
    {*�����ֵ���*} 
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
      const nList: TList): PDictEntity;
    {*��������*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�����*}
    procedure RunAfterRegistAllManager; override;
    {*�ӳ�ִ��*}
    procedure AddDictBuilder(const nBuilder: TDictItemBuilder;
      const nIdx: Integer = -1);
    function AddEntity(const nEntity,nName: string;
      const nList: TList): PDictEntity;
    procedure GetEntity(const nEntity,nLang: string; const nData: PDictEntity);
    {*�ֵ�����*}
    procedure GetDictData(const nList: TList);
    procedure ClearDictData(const nList: TList; const nFree: Boolean = False);
    {*�ֵ�����*}
    function BuilDictSQL(const nEntity: PDictEntity;const nIdx:Integer): string;
    procedure AddDict(const nEntity: PDictEntity; const nIdx: Integer = 0);
    procedure DelDict(const nEntity: PDictEntity; const nIdx: Integer = 0);
    {*�ֵ���*}
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
      AddF('D_Entity',      'varchar(32)',            'ʵ���ʶ').
      AddF('D_Name',        'varchar(32)',            'ʵ������').
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

//Date: 2021-07-12
//Desc: ��ʼ���ֵ���
procedure TDictItem.Init();
var nInit: TDictItem;
begin
  FillChar(nInit, SizeOf(TDictItem), #0);
  Self := nInit;

  FVisible := True;
  FIndex   := -1;
end;

//------------------------------------------------------------------------------
//Date: 2021-07-12
//Parm: �׸��ֵ���
//Desc: ��ʼ��ʵ���Ĭ���ֵ���
procedure TDictEntity.Init(const nFirstItem: Boolean);
var nInit: TDictEntity;
begin
  FillChar(nInit, SizeOf(TDictEntity), #0);
  Self := nInit;

  if nFirstItem then
  begin
    SetLength(FItems, 1);
    FItems[0].Init;
  end;
end;

//Date: 2021-06-17
//Parm: �ֶ�;����
//Desc: ����ֵ���
function TDictEntity.AddDict(const nField,nTitle: string): PDictEntity;
var nStr: string;
    nIdx: Integer;
begin
  Result := @Self;
  //return self address

  for nIdx := Low(FItems) to High(FItems) do
   if (CompareText(nTitle, FItems[nIdx].FTitle) = 0) and
      (CompareText(nField, FItems[nIdx].FDBItem.FField) = 0) then
   begin
     nStr := 'TDataDictManager.AddDict: %s.%s.%s.%s Has Exists.';
     nStr := Format(nStr, [FEntity, FName, nField, nTitle]);

     WriteLog(nStr);
     raise Exception.Create(nStr);
   end;

  nIdx := Length(FItems);
  SetLength(FItems, nIdx + 1);
  FItems[nIdx].Init;

  with FItems[nIdx] do
  begin
    FIndex := -1;
    FVisible := True;
    FTitle := nTitle;
    FDBItem.FField := nField;
  end;
end;

//Date: 2021-06-21
//Parm: ����;ģʽ
//Desc: ����nMode�����ֵ���
function TDictEntity.FindItem(const nData: string; const nMode: Byte): PDictItem;
var nIdx: Integer;
begin
  Result := nil;
  for nIdx := Low(FItems) to High(FItems) do
  begin
    if nMode = 1 then //title
    begin
      if CompareText(nData, FItems[nIdx].FTitle) = 0 then
      begin
        Result := @FItems[nIdx];
        Break;      
      end;
    end else

    if nMode = 2 then //field
    begin
      if CompareText(nData, FItems[nIdx].FDBItem.FField) = 0 then
      begin
        Result := @FItems[nIdx];
        Break;      
      end;
    end;
  end;
end;

//Date: 2021-06-21
//Parm: ����
//Desc: ����������nTitle���ֵ���
function TDictEntity.ByTitle(const nTitle: string): PDictItem;
begin
  Result := FindItem(nTitle, 1);
end;

//Date: 2021-06-21
//Parm: �ֶ�
//Desc: �����ֶ�ΪnField���ֵ���
function TDictEntity.ByField(const nField: string): PDictItem;
begin
  Result := FindItem(nField, 2);
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
//Parm: ���÷���;˳��
//Desc: �����ֵ����÷���
procedure TDataDictManager.AddDictBuilder(const nBuilder: TDictItemBuilder;
  const nIdx: Integer);
var i: Integer;
begin
  for i := Low(FBuilders) to High(FBuilders) do
    if @FBuilders[i] = @nBuilder then Exit;
  //has exists

  i := Length(FBuilders);
  SetLength(FBuilders, i + 1);
  FBuilders[i] := nBuilder;

  if (nIdx > -1) and (nIdx < i) then
  begin
    FBuilders[i] := FBuilders[nIdx]; //save old
    FBuilders[nIdx] := nBuilder;     //make new
  end;
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
      Dispose(PDictEntity(nList[nIdx]));
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
  const nList: TList): PDictEntity;
var nIdx: Integer;
    nEty: PDictEntity;
begin
  Result := nil;
  for nIdx := nList.Count - 1 downto 0 do
  begin
    nEty := nList[nIdx];
    if (CompareText(nEntity, nEty.FEntity) = 0) and (
       (nLang = '') or (CompareText(nLang, nEty.FLang) = 0)) then
    begin
      Result := nEty;
      Break;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: ʵ���ʶ;����;����
//Desc: ��nList������//��ʶΪnEntity�ֵ�ʵ��
function TDataDictManager.AddEntity(const nEntity, nName: string;
  const nList: TList): PDictEntity;
begin
  Result := FindEntity(nEntity, '', nList);
  if not Assigned(Result) then
  begin
    New(Result);
    nList.Add(Result);
    Result.Init(False);

    with Result^ do
    begin
      FEntity := nEntity;
      FName   := nName;
    end;
  end;
end;

//Date: 2021-06-17
//Parm: ʵ��;�ֵ�������
//Desc: ����nEntity.FItems[nIdx]��insert,update���
function TDataDictManager.BuilDictSQL(const nEntity: PDictEntity;
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
    if FWidth < 1 then
      FWidth := 120;
    //default width

    Result := MakeSQLByStr([
      SF('D_Entity', nEntity.FEntity),
      SF('D_Name', nEntity.FName),
      SF('D_Title', FTitle),
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
var nStr: string;
    i,nIdx: Integer;
    nDicts: TList;
    nListA,nListB: TStrings;

    nQuery: TDataSet;
    nEntity: PDictEntity;
begin
  Result := False;
  if Assigned(nMemo) then
    nMemo.Clear;
  //xxxxx

  nListA := nil;
  nListB := nil;
  nDicts := nil;
  nQuery := nil; //init

  with gDBManager do
  try
    nQuery := LockDBQuery();

    nListA := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListA.Clear;
    nListB := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListB.Clear;

    nStr := 'Select D_Entity,D_LangID,D_Title,D_DBField From %s ' +
            'Where D_LangID=''%s''';
    nStr := Format(nStr, [sTable_DataDict, nLang]);

    with DBQuery(nStr, nQuery) do
    if RecordCount > 0 then
    begin
      First;
      while not Eof do
      begin
        nStr := FieldByName('D_Entity').AsString + '.' +
                FieldByName('D_LangID').AsString + '.' +
                FieldByName('D_DBField').AsString + '.' +
                FieldByName('D_Title').AsString;
        nListA.Add(nStr);
        Next;
      end;
    end;

    nDicts := gMG.FObjectPool.Lock(TList) as TList;
    GetDictData(nDicts);
    //get dict data

    UMgrDataDict.WriteLog('::: �����ֵ����� :::', nMemo);
    for nIdx := 0 to nDicts.Count -1 do
    begin
      nEntity := nDicts[nIdx];
      nEntity.FLang := nLang;

      UMgrDataDict.WriteLog('����ʵ��: ' + nEntity.FEntity + '.' +
                                           nEntity.FName, nMemo);
      //xxxxx

      for i := Low(nEntity.FItems) to High(nEntity.FItems) do
      with nEntity.FItems[i],TSQLBuilder,TStringHelper do
      begin
        nStr := nEntity.FEntity + '.' + nLang + '.' +
                FDBItem.FField + '.' + FTitle;
        //xxxxx

        if nListA.IndexOf(nStr) < 0 then
        begin
          if FIndex < 0 then          
            FIndex := i;
          //any property

          nListB.Add(BuilDictSQL(nEntity, i));
          UMgrDataDict.WriteLog('�Ѵ���: ' + nStr, nMemo);
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

    ClearDictData(nDicts);
    gMG.FObjectPool.Release(nDicts);
    //menu list
  end;
end;

//Date: 2021-06-17
//Parm: �ֵ���
//Desc: �����ֵ���
procedure TDataDictManager.AddDict(const nEntity: PDictEntity;
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
procedure TDataDictManager.DelDict(const nEntity: PDictEntity;
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
  const nData: PDictEntity);
var nStr: string;
    nIdx: Integer;
    nQuery: TDataSet;
begin
  nData.Init(False);
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
      nIdx := 0;
      First;

      nData.FEntity := FieldByName('D_Entity').AsString;
      nData.FName   := FieldByName('D_Name').AsString;
      nData.FLang   := FieldByName('D_LangID').AsString;

      while not Eof do
      begin
        nData.FItems[nIdx].Init;
        //init

        with nData.FItems[nIdx] do
        begin
          FRecordID := FieldByName('R_ID').AsString;
          FTitle    := FieldByName('D_Title').AsString;
          FAlign    := TAlignment(FieldByName('D_Align').AsInteger);
          FWidth    := FieldByName('D_Width').AsInteger;
          FIndex    := FieldByName('D_Index').AsInteger;
          FVisible  := StrToBool(FieldByName('D_Visible').AsString);
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
var nIdx,nInt: Integer;
    nDicts: TList;
    nEntity: PDictEntity;
begin
  nDicts := nil;
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    nDicts := gMG.FObjectPool.Lock(TList) as TList;
    GetDictData(nDicts);
    //get dict data

    nInt := 0;
    for nIdx := nDicts.Count-1 downto 0 do
    begin
      nEntity := nDicts[nIdx];
      nInt := nInt + Length(nEntity.FItems);
    end;

    if not nFriendly then
    begin
      nList.Add('NumBuilder=' + Length(FBuilders).ToString);
      nList.Add('NumEntity=' + nDicts.Count.ToString);
      nList.Add('NumDictItem=' + nInt.ToString);
      Exit;
    end;

    nList.Add(FixData('NumBuilder:', Length(FBuilders).ToString));
    nList.Add(FixData('NumEntity:', nDicts.Count.ToString));
    nList.Add(FixData('NumDictItem:', nInt.ToString));

    for nIdx := 0 to nDicts.Count-1 do
    begin
      nEntity := nDicts[nIdx];
      nList.Add(FixData(Format('EntityItem %d:', [nIdx+1]),
        nEntity.FEntity + '.' + nEntity.FName));
      //xxxxx
    end;
  finally
    SyncLeave;
    ClearDictData(nDicts);
    gMG.FObjectPool.Release(nDicts);
  end;
end;

end.
