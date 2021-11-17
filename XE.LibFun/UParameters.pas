{*******************************************************************************
  ����: dmzn@163.com 2021-08-15
  ����: �������ù�����

  ��ע:
  *.������:TParamItem
    *.FGroup: ��������
    *.FID: ������ʶ.ͬһ��������FID�����ظ�,��ͬ����FID������ͬ.
    *.FOptionOnly: ָ��������ĳЩ���͵Ĳ���ʱ,������������,ֻ��ѡ��.
    *.FOwner: ������Ӧ����֯�ܹ���ʶ.
    *.FEffect: ������Ч��ʽ,�ο�һ��"��֯�ܹ��������Ĺ�ϵ"�Ľ���.
  *.ÿһ������� FOwner.FGroup.FID Ψһ��Լ��,��: ÿ��FOwner��
    Ψһ�Ĳ�����(FGroup.FID)

  *.��֯�ܹ��������Ĺ�ϵ,�Լ��š����򡢹��������ܹ�Ϊ��:
    1.��֯�ܹ��е�ÿһ��,������ӵ��һ�����(1:1),���߲����ò���(0:1).
    2.��֯�ܹ�����һ��,������ͬ�Ĳ���(��ͬGroup��ID)�����Ч.����:
      a.���������¼���Ч(etLower),�����������������,���������Լ��Ĳ���Ϊ׼.
      b.���������ϼ���Ч(etHigher),���������򡢹����������,���Լ���Ϊ׼.
      c.������δ����,�����������¼���Ч(etLower),�������Լ��Ĳ���Ϊ׼.
      d.������δ����,�����������ϼ���Ч(etHigher),����������Ĳ���Ϊ׼.
    3.����һ��û�����ò���,���Ա����Ĳ���Ϊ׼.
    4.������û�����ò���,����Ĭ��ֵΪ׼.

  ʹ�÷���:
  1.���Builder
    procedure SystemParams(const nList: TList);
    begin
      gMG.FParamsManager.Default.Init('System', 'ϵͳͨ�ò���').
        SetEffect(etLower).
        SetOptionOnly([ptStr]);
      //���ò���Ĭ��ֵ

      gMG.FParamsManager.AddParam('001', '��һ����', nList).
        AddS('a1', '', True).
        AddI(11, '', True).
        AddF(11.1);
      //first

      gMG.FParamsManager.AddParam('002', '�ڶ�����', nList).
        AddS('a2', '', True).
        AddI(22, '', True).
        AddF(22.2);
      //second
    end;

    gMG.FParamsManager.AddBuilder(SystemParams);
    //�����������

  2.��ʼ��������
    gMG.FParamsManager.InitParameters('Owner01', 'admin');
    //��ʼ��Owner01�Ĳ�����

  3.��ȡ������
    var nP: TParamItem;
    gMG.FParamsManager.GetParam('system', '001', ['Owner01'], nP)
    //��ȡ��֯�ܹ���Owner01�ڷ���system�б��Ϊ001������
*******************************************************************************}
unit UParameters;

interface

uses
  System.Classes, System.SysUtils, Data.DB, ULibFun, UBaseObject;

type
  TParamEffectType = (etLower, etHigher);
  //effect type

const
  cParamDefaultValue = High(Word);
  //value for return

  sParamDataType: array[TCommandParam.TParamType] of string = ('�ַ�', '����',
    '����', 'ָ��', '����', '����');
  //data type desc

  sParamEffectType:array[TParamEffectType] of string = ('�¼���Ч', '�ϼ���Ч');
  //effect type desc

type
  PParamItem = ^TParamItem;
  TParamItem = record
    FEnabled     : Boolean;                        //״̬���
    FRecord      : string;                         //��¼��ʶ
    FGroup       : string;                         //��������
    FGrpName     : string;                         //��������
    FID          : string;                         //������ʶ
    FName        : string;                         //��������
    FValue       : TCommandParam;                  //����ֵ
    FOptionOnly  : TCommandParam.TParamTypes;      //ֻʹ�ÿ�ѡ
    FOwner       : string;                         //ӵ����id
    FEffect      : TParamEffectType;               //��Ч��ʽ
  private
  public
    function Init(const nGroup,nName: string): PParamItem;
    {*��ʼ��*}
    function SetGroup(nGroup,nName: string): PParamItem;
    function SetEffect(const nEffect: TParamEffectType): PParamItem;
    function SetOptionOnly(const nOnly: TCommandParam.TParamTypes): PParamItem;
    {*��������*}
    function AddS(const nStr: string; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    function AddI(const nInt: Integer; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    function AddF(const nFlt: Double; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    function AddD(const nDate: TDateTime; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    {*��Ӳ���ֵ*}
  end;

  TParamItemBuilder = procedure (const nList: TList);
  //for external-system fill param info

  TParameterManager = class(TManagerBase)
  public
    const
      //tables
      sTable_SysDict = 'Sys_Dict';                 //��������
      sTable_DictExt = 'Sys_DictExt';              //��չ����

      //group
      sParamDefGroup = 'SysParam';                 //Ĭ�Ϸ���
      sParamDefGName = 'ϵͳ����';                 //Ĭ������
  private
    FParamOptions: TList;
    {*�������б�*}
    FDefaultItem: TParamItem;
    {*Ĭ������*}
    FBuilders: array of TParamItemBuilder;
    {*����������Ϣ*}
  public
    constructor Create;
    destructor Destroy; override;
    {*�����ͷ�*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*ע�������*}
    procedure RunAfterRegistAllManager; override;
    {*�ӳ�ִ��*}
    function Default: PParamItem;
    {*Ĭ������*}
    procedure AddBuilder(const nBuilder: TParamItemBuilder);
    function AddParam(const nID,nName: string; const nList: TList): PParamItem;
    {*�������*}
    function FindParam(const nGroup,nID: string; nList: TList=nil): PParamItem;
    {*��������*}
    procedure GetParamData(const nList: TList);
    procedure ClearParamData(const nList: TList; const nFree: Boolean = False);
    {*��������*}
    procedure BuildSQL(const nParam: PParamItem; const nEditor: string;
      const nList: TStrings);
    procedure InitParameters(const nOwner,nEditor: string;
      const nMemo: TStrings = nil);
    {*��ʼ������*}
    procedure SaveParam(const nParam: PParamItem; const nEditor: string);
    procedure DeleteParam(const nParam: PParamItem);
    {*���������*}
    function GetParam(const nGroup, nID: string; const nOwner: TArray<string>;
      var nParam: TParamItem): Boolean;
    function GetParam2(const nRecord: string; var nParam: TParamItem;
      nQuery: TDataSet = nil): Boolean;
    {*��ȡ������*}
    procedure LoadFromFile(const nFile: string);
    procedure SaveToFile(const nFile: string);
    {*�ļ��־û�*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*��ȡ״̬*}
    property ParamItems: TList read FParamOptions;
    {*�������*}
  end;

var
  gParamsManager: TParameterManager = nil;
  //ȫ��ʹ��

implementation

uses
  UManagerGroup, UDBManager, UDBFun, NativeXml;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TParameterManager, '����������', nEvent);
end;

//Desc: ��ӹ����������
procedure AddParameterTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TParameterManager do
  begin
    AddTable(sTable_SysDict, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼���').
      AddF('D_Record',      'varchar(32)',            '��¼��ʶ').
      AddF('D_Group',       'varchar(32)',            '��������').
      AddF('D_GrpName',     'varchar(80)',            '��������').
      AddF('D_ID',          'varchar(32)',            '������ʶ').
      AddF('D_Name',        'varchar(80)',            '��������').
      AddF('D_Str',         'varchar(200)',           '�ַ�ֵ').
      AddF('D_Int',         'integer',                '����ֵ').
      AddF('D_Double',      sField_SQLServer_Decimal, '����ֵ').
      AddF('D_Date',        'DateTime',               '����ֵ').
      AddF('D_Effect',      'varchar(16)',            '��Ч��ʽ').
      AddF('D_Owner',       'varchar(32)',            '������֯').
      AddF('D_Editor',      'varchar(32)',            '�޸���').
      AddF('D_EditTime',    'DateTime',               '�޸�ʱ��').
      //for field
      AddI('idx_id',        'D_Group ASC,D_ID ASC').
      AddI('idx_record',    'D_Record ASC').
      AddI('idx_owner',     'D_Owner ASC');
      //for index

    AddTable(sTable_DictExt, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '��¼��ʶ').
      AddF('V_ID',          'varchar(32)',            '������ʶ').
      AddF('V_Desc',        'varchar(80)',            '��������').
      AddF('V_Type',        'varchar(16)',            '��������').
      AddF('V_Str',         'varchar(200)',           '�ַ�ֵ').
      AddF('V_Int',         'integer',                '����ֵ').
      AddF('V_Double',      sField_SQLServer_Decimal, '����ֵ').
      AddF('V_Date',        'DateTime',               '����ֵ').
      //for field
      AddI('idx_id',        'V_ID ASC');
      //for index
  end;
end;

//------------------------------------------------------------------------------
//Date: 2021-08-16
//Parm: ����;��������
//Desc: ��ʼ��������
function TParamItem.Init(const nGroup,nName: string): PParamItem;
var nInit: TParamItem;
begin
  FillChar(nInit, SizeOf(TParamItem), #0);
  Self := nInit;
  Result := @Self;

  FEnabled := True;
  FEffect  := etLower;
  FOptionOnly := [];

  SetGroup(nGroup, nName);
  FValue.AllowRepeat(False);
end;

//Date: 2021-08-16
//Parm: ����;��������
//Desc: ���÷���
function TParamItem.SetGroup(nGroup,nName: string): PParamItem;
begin
  Result := @Self;
  //return self address

  nGroup := Trim(nGroup);
  if nGroup = '' then
    nGroup := TParameterManager.sParamDefGroup;
  FGroup := nGroup;

  nName := Trim(nName);
  if nName = '' then
    nName := TParameterManager.sParamDefGName;
  FGrpName := nName;
end;

//Date: 2021-08-16
//Parm: ��Ч��ʽ
//Desc: ������Ч��ʽ
function TParamItem.SetEffect(const nEffect: TParamEffectType): PParamItem;
begin
  Result := @Self;
  FEffect := nEffect;
end;

//Date: 2021-08-16
//Parm: ֻʹ�ÿ�ѡ���ݵ�����
//Desc: ����nOnlyָ��������ֻʹ��FOptions�������,ֻ��ѡ��������.
function TParamItem.SetOptionOnly(const nOnly: TCommandParam.TParamTypes): PParamItem;
begin
  Result := @Self;
  FOptionOnly := nOnly;
end;

//Date: 2021-08-16
//Parm: �ַ���;����;Ĭ��
//Desc: �����ַ���ֵ
function TParamItem.AddS(const nStr,nDesc: string;
  const nDef: Boolean): PParamItem;
begin
  Result := @Self;
  //return self address
  FValue.AddS(nStr, nDesc, nDef);
end;

//Date: 2021-08-16
//Parm: ����;����;Ĭ��
//Desc: ��������ֵ
function TParamItem.AddI(const nInt: Integer; const nDesc: string;
  const nDef: Boolean): PParamItem;
begin
  Result := @Self;
  //return self address
  FValue.AddI(nInt, nDesc, nDef);
end;

//Date: 2021-08-16
//Parm: ����;����;Ĭ��
//Desc: ��������ֵ
function TParamItem.AddF(const nFlt: Double; const nDesc: string;
  const nDef: Boolean): PParamItem;
begin
  Result := @Self;
  //return self address
  FValue.AddF(nFlt, nDesc, nDef);
end;

//Date: 2021-08-16
//Parm: ����;����;Ĭ��
//Desc: ��������ʱ��ֵ
function TParamItem.AddD(const nDate: TDateTime; const nDesc: string;
  const nDef: Boolean): PParamItem;
begin
  Result := @Self;
  //return self address
  FValue.AddD(nDate, nDesc, nDef);
end;

//------------------------------------------------------------------------------
constructor TParameterManager.Create;
begin
  inherited;
  SetLength(FBuilders, 0);
  FDefaultItem.Init('', '');

  FParamOptions := TList.Create;
  //�������б�
end;

destructor TParameterManager.Destroy;
begin
  ClearParamData(FParamOptions, True);
  inherited;
end;

//Date: 2021-08-15
//Parm: �Ƿ�ע��
//Desc: ��ϵͳע�����������
class procedure TParameterManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TParameterManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TParameterManager.Create;
    gMG.FParamsManager := gMG.FManagers[nIdx].FManager as TParameterManager;
  end else
  begin
    gMG.FParamsManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;

  gParamsManager := gMG.FParamsManager;
  //����ȫ�ֱ���
end;

procedure TParameterManager.RunAfterRegistAllManager;
begin
  gMG.CheckSupport('TParameterManager', ['TDBManager']);
  //���֧��
  gDBManager.AddTableBuilder(AddParameterTables);
end;

//Date: 2021-08-15
//Parm: ���÷���
//Desc: �������������÷���
procedure TParameterManager.AddBuilder(const nBuilder: TParamItemBuilder);
var nIdx: Integer;
begin
  for nIdx := Low(FBuilders) to High(FBuilders) do
    if @FBuilders[nIdx] = @nBuilder then Exit;
  //has exists

  nIdx := Length(FBuilders);
  SetLength(FBuilders, nIdx + 1);
  FBuilders[nIdx] := nBuilder;

  if Assigned(FParamOptions) then
    nBuilder(FParamOptions);
  //xxxxx
end;

//Date: 2021-08-16
//Desc: Ĭ��
function TParameterManager.Default: PParamItem;
begin
  Result := @FDefaultItem;
end;

//Date: 2021-08-16
//Parm: ����;��ʶ
//Desc: ����nGroup.nID������
function TParameterManager.FindParam(const nGroup, nID: string;
  nList: TList): PParamItem;
var nIdx: Integer;
    nPI: PParamItem;
begin
  Result := nil;
  if not Assigned(nList) then
    nList := FParamOptions;
  //xxxxx

  for nIdx := nList.Count - 1 downto 0 do
  begin
    nPI := nList[nIdx];
    if (CompareText(nID, nPI.FID) = 0) and
       (CompareText(nGroup, nPI.FGroup) = 0) then
    begin
      Result := nPI;
      Break;
    end;
  end;
end;

//Date: 2021-08-16
//Parm: ����;��ʶ;����
//Desc: ��Ӳ�����
function TParameterManager.AddParam(const nID, nName: string;
  const nList: TList): PParamItem;
begin
  Result := FindParam(FDefaultItem.FGroup, nID, nList);
  if Assigned(Result) then
  begin
    if nName <> '' then
      Result.FName := nName;
    Exit;
  end;

  New(Result);
  nList.Add(Result);
  Result.Init(FDefaultItem.FGroup, FDefaultItem.FGrpName);

  with Result^ do
  begin
    FID          := nID;
    FName        := nName;
    FOptionOnly  := FDefaultItem.FOptionOnly;
    FEffect      := FDefaultItem.FEffect;
  end;
end;

//Date: 2021-08-16
//Parm: �б�
//Desc: ��ȡ���еĲ���������
procedure TParameterManager.GetParamData(const nList: TList);
var nIdx: Integer;
begin
  nList.Clear;
  //init first

  for nIdx := Low(FBuilders) to High(FBuilders) do
    FBuilders[nIdx](nList);
  //xxxxx
end;

//Date: 2021-08-16
//Parm: �б�;�Ƿ��ͷ�
//Desc: ����nList���������б�
procedure TParameterManager.ClearParamData(const nList: TList;
  const nFree: Boolean);
var nIdx: Integer;
begin
  if Assigned(nList) then
  begin
    for nIdx := nList.Count - 1 downto 0 do
      Dispose(PParamItem(nList[nIdx]));
    //xxxxx

    if nFree then
         nList.Free
    else nList.Clear;
  end;
end;

//Date: 2021-08-16
//Parm: ������;�޸���
//Desc: ����nEntity.FItems[nIdx]��insert,update���,����nList��
procedure TParameterManager.BuildSQL(const nParam: PParamItem;
  const nEditor: string; const nList: TStrings);
var nStr,nID: string;
    nIdx: Integer;
    nBool: Boolean;
    nExt: TCommandParam.PParamExtend;

    //Desc: �Ƿ�Ϊ��һ��Ĭ��ֵ
    function IsFirstDefault(): Boolean;
    begin
      Result := False;
      if Assigned(nExt) then
      begin
        if nExt.FDefault and (not nBool) then
        begin
           nBool := True;
           Result := True;
        end else nStr := nExt.FDesc;
      end else nStr := '';
    end;
begin
  with nParam^, TSQLBuilder,TDateTimeHelper,TStringHelper do
  begin
    nBool := FRecord = '';
    if nBool then
      nID := TDBCommand.SnowflakeID();
    //��¼���к�

    nStr := MakeSQLByStr([SF_IF([SF('D_Record', nID), ''], nBool),
      SF('D_Group',    FGroup),
      SF('D_GrpName',  FGrpName),
      SF('D_ID',       FID),
      SF('D_Name',     FName),
      SF('D_Effect',   Enum2Str(FEffect)),
      SF('D_Owner',    FOwner),
      SF('D_Editor',   nEditor),
      SF('D_EditTime', sField_SQLServer_Now, sfVal),

      SF('D_Str',      FValue.DefaultS()),
      SF('D_Int',      FValue.DefaultI(cParamDefaultValue), sfVal),
      SF('D_Double',   FValue.DefaultF(cParamDefaultValue), sfVal),
      SF('D_Date',     DateTime2Str(FValue.DefaultD()))
    ], sTable_SysDict, SF('D_Record', FRecord), nBool);

    nList.Add(nStr);
    //����Ĭ��ֵ
    if nBool then Exit;

    nStr := 'Delete From %s Where V_ID=''%s''';
    nStr := Format(nStr, [sTable_DictExt, FRecord]);
    nList.Add(nStr); //clear extend parameters

    with FValue do
    begin
      nBool := False;
      for nIdx := Low(Str) to High(Str) do
      begin
        nExt := GetExt(ptStr, nIdx);
        if IsFirstDefault() then Continue; //��һ��Ĭ��ֵ�Ѵ������

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', nStr),
          SF('V_Type', Enum2Str(ptStr)),
          SF('V_Str', Str[nIdx])
          ], sTable_DictExt, '', True);
        nList.Add(nStr);
      end;

      nBool := False;
      for nIdx := Low(Int) to High(Int) do
      begin
        nExt := GetExt(ptInt, nIdx);
        if IsFirstDefault() then Continue;

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', nStr),
          SF('V_Type', Enum2Str(ptInt)),
          SF('V_Int', Int[nIdx], sfVal)
          ], sTable_DictExt, '', True);
        nList.Add(nStr);
      end;

      nBool := False;
      for nIdx := Low(Flt) to High(Flt) do
      begin
        nExt := GetExt(ptFlt, nIdx);
        if IsFirstDefault() then Continue;

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', nStr),
          SF('V_Type', Enum2Str(ptFlt)),
          SF('V_Double', Flt[nIdx], sfVal)
          ], sTable_DictExt, '', True);
        nList.Add(nStr);
      end;

      nBool := False;
      for nIdx := Low(Dat) to High(Dat) do
      begin
        nExt := GetExt(ptDate, nIdx);
        if IsFirstDefault() then Continue;

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', nStr),
          SF('V_Type', Enum2Str(ptDate)),
          SF('V_Date', DateTime2Str(Dat[nIdx]))
          ], sTable_DictExt, '', True);
        nList.Add(nStr);
      end;
    end;
  end;
end;

//Date: 2021-08-17
//Parm: ����ӵ����;�޸���id
//Desc: ��ʼ��nOwner�����в�����
procedure TParameterManager.InitParameters(const nOwner,nEditor: string;
  const nMemo: TStrings);
var nStr: string;
    nIdx: Integer;
    nQuery: TDataSet;
    nPItem: PParamItem;
    nListA,nListB: TStrings;
begin
  nListA := nil;
  nListB := nil;
  nQuery := nil; //init

  with gDBManager do
  try
    nListA := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListA.Clear;
    nListB := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nListB.Clear;

    nQuery := LockDBQuery();
    nStr := 'Select D_Group,D_ID From %s Where D_Owner=''%s''';
    nStr := Format(nStr, [sTable_SysDict, nOwner]);

    with DBQuery(nStr, nQuery) do
    if RecordCount > 0 then
    begin
      First;
      while not Eof do
      begin
        nStr := FieldByName('D_Group').AsString + '.' +
                FieldByName('D_ID').AsString;
        nListA.Add(nStr);
        Next;
      end;
    end;

    UParameters.WriteLog('::: �������ò��� :::', nMemo);
    for nIdx := 0 to FParamOptions.Count -1 do
    begin
      nPItem := FParamOptions[nIdx];
      nStr := nPItem.FGroup + '.' + nPItem.FID;
      if nListA.IndexOf(nStr) < 0 then
      begin
        nPItem.FOwner := nOwner;
        BuildSQL(nPItem, nEditor, nListB);
        UParameters.WriteLog('�Ѵ���: ' + nStr, nMemo);
      end;
    end;

    if nListB.Count > 0 then
      DBExecute(nListB);
    //xxxxx
  finally
    gMG.FObjectPool.Release(nListA);
    gMG.FObjectPool.Release(nListB);
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-08-19
//Parm: ������;�༭��
//Desc: ����nParam�����ݿ�
procedure TParameterManager.SaveParam(const nParam: PParamItem;
  const nEditor: string);
var nStr: string;
    nList: TStrings;
    nQuery: TDataSet;
begin
  if (nParam.FGroup = '') or (nParam.FID = '') or (nParam.FOwner = '') then
  begin
    nStr := 'UParameters.SaveParam: Fields(FGroup,FID,FOwner) Is Null.';
    WriteLog(nStr);
    Exit;
  end;

  nList := nil;
  nQuery := nil;
  try
    if nParam.FRecord = '' then
    begin
      nStr := 'Select D_Record From %s ' +
              'Where D_Group=''%s'' And D_ID=''%s'' And D_Owner=''%s''';
      nStr := Format(nStr, [sTable_SysDict, nParam.FGroup, nParam.FID,
              nParam.FOwner]);
      //xxxxx

      nQuery := gDBManager.DBQuery(nStr);
      if nQuery.RecordCount > 0 then
        nParam.FRecord := nQuery.Fields[0].AsString;
      //get old record,try to override
    end;

    nList := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nList.Clear;
    BuildSQL(nParam, nEditor, nList);

    if nList.Count > 0 then
      gDBManager.DBExecute(nList);
    //xxxxx
  finally
    gMG.FObjectPool.Release(nList);
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-08-19
//Parm: ������
//Desc: ɾ��nParam
procedure TParameterManager.DeleteParam(const nParam: PParamItem);
var nStr: string;
    nList: TStrings;
begin
  if nParam.FRecord = '' then
  begin
    nStr := 'UParameters.DeleteParam: Fields(FRecord) Is Null.';
    WriteLog(nStr);
    Exit;
  end;

  nList := nil;
  try
    nList := gMG.FObjectPool.Lock(TStrings) as TStrings;
    nList.Clear;

    nStr := 'Delete From %s Where D_Record=''%s''';
    nStr := Format(nStr, [sTable_SysDict, nParam.FRecord]);
    nList.Add(nStr);

    nStr := 'Delete From %s Where V_ID=''%s''';
    nStr := Format(nStr, [sTable_DictExt, nParam.FRecord]);
    nList.Add(nStr);

    if nList.Count > 0 then
      gDBManager.DBExecute(nList);
    //xxxxx
  finally
    gMG.FObjectPool.Release(nList);
  end;
end;

//Date: 2021-08-20
//Parm: ��¼���
//Desc: ��ȡnRecord�Ĳ�������
function TParameterManager.GetParam2(const nRecord: string;
  var nParam: TParamItem; nQuery: TDataSet): Boolean;
var nStr: string;
    nBool: Boolean;
    nType: TCommandParam.TParamType;
begin
  Result := False;
  nParam.Init('', '');
  nBool := Assigned(nQuery);
  try
    if not nBool then
    begin
      nStr := 'Select * From %s Where D_Record=''%s''';
      nStr := Format(nStr, [sTable_SysDict, nRecord]);
      nQuery := gDBManager.DBQuery(nStr);

      if nQuery.RecordCount < 1 then
      begin
        nStr := Format('���Ϊ[ %s ]�ļ�¼������', [nRecord]);
        WriteLog(nStr);
        Exit;
      end;
    end;

    with nQuery, nParam do
    begin
      FRecord  := FieldByName('D_Record').AsString;
      FGroup   := FieldByName('D_Group').AsString;
      FGrpName := FieldByName('D_GrpName').AsString;
      FID      := FieldByName('D_ID').AsString;
      FName    := FieldByName('D_Name').AsString;
      FOwner   := FieldByName('D_Owner').AsString;

      nStr     := FieldByName('D_Effect').AsString;
      FEffect  := TStringHelper.Str2Enum<TParamEffectType>(nStr);

      if FieldByName('D_Str').AsString <> '' then
        FValue.AddS(FieldByName('D_Str').AsString, '', True);
      //xxxxx

      if FieldByName('D_Int').AsInteger <> cParamDefaultValue then
        FValue.AddI(FieldByName('D_Int').AsInteger, '', True);
      //xxxxx

      if FieldByName('D_Double').AsFloat <> cParamDefaultValue then
        FValue.AddF(FieldByName('D_Double').AsFloat, '', True);
      //xxxxx

      if FieldByName('D_Date').AsDateTime > 0 then
        FValue.AddD(FieldByName('D_Date').AsDateTime, '', True);
      //xxxxx
    end;

    nStr := 'Select * From %s Where V_ID=''%s''';
    nStr := Format(nStr, [sTable_DictExt, nParam.FRecord]);
    gDBManager.DBQuery(nStr, nQuery);

    with nQuery, nParam, TStringHelper do
    if RecordCount > 0 then    
    begin
      First;
      //xxxxx
      
      while not Eof do
      begin
        nStr := FieldByName('V_Type').AsString;
        nType := TStringHelper.Str2Enum<TCommandParam.TParamType>(nStr);
        nStr := FieldByName('V_Desc').AsString;
        
        case nType of
         ptStr : FValue.AddS(FieldByName('V_Str').AsString, nStr);
         ptInt : FValue.AddI(FieldByName('V_Int').AsInteger, nStr);
         ptFlt : FValue.AddF(FieldByName('V_Double').AsFloat, nStr);
         ptDate: FValue.AddD(FieldByName('V_Date').AsDateTime, nStr);
        end;
      
        Next;
      end;
    end;

    Result := True;
  finally
    if not nBool then
      gDBManager.ReleaseDBQuery(nQuery);
    //xxxxx
  end;
end;

//Date: 2021-08-20
//Parm: ����;��ʶ;ӵ���߼ܹ�(�ӵ͵���)
//Desc: ��ȡnOwner�Ĳ���nGroup.nID����,����nParam��
function TParameterManager.GetParam(const nGroup, nID: string;
  const nOwner: TArray<string>; var nParam: TParamItem): Boolean;
var nStr,nSQL: string;
    nBool: Boolean;
    nIdx,nLow: Integer;
    nQuery: TDataSet;
    nEffect: TParamEffectType;
begin
  Result := False;
  nParam.Init('', '');
  //init first

  nSQL := '';
  for nIdx := Low(nOwner) to High(nOwner) do
  begin
    nStr := Trim(nOwner[nIdx]);
    if nStr <> '' then
    begin
      nStr := TSQLBuilder.SQM(nStr);
      //'str','str'

      if nSQL = '' then
           nSQL := nStr
      else nSQL := nSQL + ',' + nStr;
    end; //owner list
  end;

  if nSQL = '' then
  begin
    WriteLog('UParameters.GetParam: Owner Is Null');
    Exit;
  end;

  nQuery := nil;
  try
    nStr := 'Select * From %s ' +
            'Where D_Group=''%s'' And D_ID=''%s'' And D_Owner In (%s)';
    nStr := Format(nStr, [sTable_SysDict, nGroup, nID, nSQL]);

    nQuery := gDBManager.DBQuery(nStr);
    if nQuery.RecordCount < 1 then
    begin
      nStr := Format('������[ %s.%s(%s) ]������', [nGroup, nID, nSQL]);
      WriteLog(nStr);
      Exit;
    end;

    {---------------------------------------------------------------------------
     *.��֯�ܹ��Ͳ�������Ч��ʽ,��ο���Ԫͷ˵��.
     *.nOwner�еĲ㼶˳��Ϊ: ����,�ϼ�..,���ϼ�
     *.����ʱ����ID��������,���Բ������ϼ�
    ---------------------------------------------------------------------------}
    nBool := False;
    nLow := Low(nOwner); //����id

    for nIdx := High(nOwner) downto nLow do //�������
    begin
      nQuery.First;
      //cursor first
      while not nQuery.Eof do
      begin
        nStr := nQuery.FieldByName('D_Owner').AsString;
        if CompareText(nStr, nOwner[nIdx]) = 0 then
        begin
          nStr := nQuery.FieldByName('D_Effect').AsString;
          nEffect := TStringHelper.Str2Enum<TParamEffectType>(nStr);

          if (nEffect = etHigher) or (nIdx = nLow) then
          begin
            nStr := nQuery.FieldByName('D_Record').AsString;
            Result := GetParam2(nStr, nParam, nQuery);
            Exit; //�ϼ���Ч,ֱ��ȡ�ϼ�����
          end;

          nBool := True; //�ҵ��ϼ�
          Break;
        end;

        nQuery.Next;
        //cursor next
      end;

      if nBool then
        Break;
      //xxxxx
    end;

    nQuery.First;
    //cursor first  
    while not nQuery.Eof do
    begin
      nStr := nQuery.FieldByName('D_Owner').AsString;
      if CompareText(nStr, nOwner[nLow]) = 0 then
      begin
        nStr := nQuery.FieldByName('D_Record').AsString;
        Result := GetParam2(nStr, nParam, nQuery);
        Exit;
      end;

      nQuery.Next;
      //cursor next
    end;
  finally
    gDBManager.ReleaseDBQuery(nQuery);
  end;
end;

//Date: 2021-08-23
//Parm: �洢�ڵ�;������;��ȡ or д��
//Desc: ����nRoot�ڵ�Ĳ���������
procedure ParamWithXML(const nRoot: TXmlNode; const nParam: PParamItem;
  const nLoad: Boolean);
var nStr: string;
    nIdx,nInt: Integer;
    nNode: TXmlNode;
    nType: TCommandParam.TParamType;
    nExt: TCommandParam.PParamExtend;

    //Desc: д����չ��Ϣ
    procedure WriteExtXML();
    begin
      if Assigned(nExt) then
      begin
        nNode.AttributeAdd('default', BoolToStr(nExt.FDefault, True));
        nNode.AttributeAdd('desc', nExt.FDesc);
      end else
      begin
        nNode.AttributeAdd('default', BoolToStr(False, True));
        nNode.AttributeAdd('desc', '');
      end;
    end;
begin
  if nLoad then
  begin
    nStr := nRoot.AttributeValueByName['effect'];
    nParam.SetEffect(TStringHelper.Str2Enum<TParamEffectType>(nStr));

    nStr := nRoot.AttributeValueByName['options'];
    nParam.SetOptionOnly(TStringHelper.Str2Set<TCommandParam.TParamType,
      TCommandParam.TParamTypes>(nStr));
    //xxxxx

    nInt := nRoot.NodeCount - 1;
    for nIdx := 0 to nInt do
    begin
      nNode := nRoot.Nodes[nIdx];
      if CompareText('data', nNode.Name) <> 0 then Continue;
      //must be data node

      nType := TStringHelper.Str2Enum<TCommandParam.TParamType>(
        nNode.AttributeValueByName['type']);
      //xxxxx

      case nType of
       ptStr:
        nParam.AddS(nNode.AttributeValueByName['value'],
                    nNode.AttributeValueByName['desc'],
          StrToBool(nNode.AttributeValueByName['default']));
       ptInt:
        nParam.AddI(StrToInt(
                    nNode.AttributeValueByName['value']),
                    nNode.AttributeValueByName['desc'],
          StrToBool(nNode.AttributeValueByName['default']));
       ptFlt:
        nParam.AddF(StrToFloat(
                    nNode.AttributeValueByName['value']),
                    nNode.AttributeValueByName['desc'],
          StrToBool(nNode.AttributeValueByName['default']));
       ptDate:
        nParam.AddD(TDateTimeHelper.Str2DateTime(
                    nNode.AttributeValueByName['value']),
                    nNode.AttributeValueByName['desc'],
          StrToBool(nNode.AttributeValueByName['default']));
      end;
    end;

    Exit;
  end;

  with nParam.FValue do
  begin
    nRoot.AttributeAdd('id', nParam.FID);
    nRoot.AttributeAdd('name', nParam.FName);
    nRoot.AttributeAdd('effect', TStringHelper.Enum2Str(nParam.FEffect));

    nRoot.AttributeAdd('options', TStringHelper.Set2Str<TCommandParam.TParamType,
      TCommandParam.TParamTypes>(nParam.FOptionOnly));
    //xxxxx

    for nIdx := Low(Str) to High(Str) do
    begin
      nNode := nRoot.NodeNew('data');
      nNode.AttributeAdd('type', TStringHelper.Enum2Str(ptStr));
      nNode.AttributeAdd('value', Str[nIdx]);

      nExt := GetExt(ptStr, nIdx);
      WriteExtXML();
    end;

    for nIdx := Low(Int) to High(Int) do
    begin
      nNode := nRoot.NodeNew('data');
      nNode.AttributeAdd('type', TStringHelper.Enum2Str(ptInt));
      nNode.AttributeAdd('value', IntToStr(Int[nIdx]));

      nExt := GetExt(ptInt, nIdx);
      WriteExtXML();
    end;

    for nIdx := Low(Flt) to High(Flt) do
    begin
      nNode := nRoot.NodeNew('data');
      nNode.AttributeAdd('type', TStringHelper.Enum2Str(ptFlt));
      nNode.AttributeAdd('value', FloatToStr(Flt[nIdx]));

      nExt := GetExt(ptFlt, nIdx);
      WriteExtXML();
    end;

    for nIdx := Low(Dat) to High(Dat) do
    begin
      nNode := nRoot.NodeNew('data');
      nNode.AttributeAdd('type', TStringHelper.Enum2Str(ptDate));
      nNode.AttributeAdd('value', TDateTimeHelper.DateTime2Str(Dat[nIdx]));

      nExt := GetExt(ptDate, nIdx);
      WriteExtXML();
    end;
  end;
end;

//Date: 2021-08-23
//Parm: �����ļ�
//Desc: ��nFile�м������ò���
procedure TParameterManager.LoadFromFile(const nFile: string);
var nIdx,j: Integer;
    nParam: PParamItem;
    nXML: TNativeXml;
    nRoot,nNode: TXmlNode;
begin
  if not FileExists(nFile) then Exit;
  //invalid file

  nXML := TNativeXml.Create(nil);
  try
    nXML.LoadFromFile(nFile);
    for nIdx := nXML.Root.NodeCount - 1 downto 0 do
    begin
      nRoot := nXML.Root.Nodes[nIdx];
      if CompareText('paramGroup', nRoot.Name) <> 0 then Continue;
      //must be group node

      FDefaultItem.SetGroup(nRoot.AttributeValueByName['id'],
                            nRoot.AttributeValueByName['name']);
      //set group property

      for j := nRoot.NodeCount-1 downto 0 do
      begin
        nNode := nRoot.Nodes[j];
        if CompareText('param', nNode.Name) = 0 then
        begin
          nParam := AddParam(nNode.AttributeValueByName['id'],
                             nNode.AttributeValueByName['name'], FParamOptions);
          //add new param

          ParamWithXML(nNode, nParam, True);
          //load param data
        end;
      end;
    end;
  finally
    nXML.Free;
  end;
end;

//Date: 2021-08-23
//Parm: �����ļ�
//Desc: ����������õ�nFile��
procedure TParameterManager.SaveToFile(const nFile: string);
var nIdx,j: Integer;
    nXML: TNativeXml;
    nNode: TXmlNode;
    nParam,nPNext: PParamItem;

    //Desc: ����״̬
    procedure ResetStatus();
    var i: Integer;
    begin
      for i := FParamOptions.Count-1 downto 0 do
        PParamItem(FParamOptions[i]).FEnabled := True;
      //xxxxx
    end;
begin
  nXML := TNativeXml.Create(nil);
  try
    with nXML do
    begin
      Charset := 'utf-8';
      VersionString := '1.0';
      XmlFormat := xfReadable;      

      Root.Name := 'parameters';
      Root.AttributeAdd('author', TApplicationHelper.GetCPUIDStr());
      Root.AttributeAdd('date', TDateTimeHelper.DateTime2Str(Now()));
    end;

    ResetStatus();
    //init status

    for nIdx := FParamOptions.Count-1 downto 0 do
    begin
      nParam := FParamOptions[nIdx];
      if not nParam.FEnabled then Continue;

      nNode := nXML.Root.NodeNew('paramGroup');
      nNode.AttributeAdd('id', nParam.FGroup);
      nNode.AttributeAdd('name', nParam.FGrpName);

      ParamWithXML(nNode.NodeNew('param'), nParam, False);
      //write xml
      nParam.FEnabled := False; //written flag

      for j := nIdx-1 downto 0 do
      begin
        nPNext := FParamOptions[j];
        if (nPNext.FEnabled) and
           (CompareText(nPNext.FGroup, nParam.FGroup) = 0) then //��ͬ����
        begin
          ParamWithXML(nNode.NodeNew('param'), nPNext, False);
          //write xml
          nPNext.FEnabled := False; //written flag
        end;
      end;
    end;

    nXML.SaveToFile(nFile);
    //save data
  finally
    nXML.Free;
    ResetStatus();
  end;
end;

procedure TParameterManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx: Integer;
    nParam: PParamItem;
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);

    if not nFriendly then
    begin
      nList.Add('NumBuilder=' + Length(FBuilders).ToString);
      nList.Add('NumParam=' + FParamOptions.Count.ToString);
      Exit;
    end;

    nList.Add(FixData('NumBuilder:', Length(FBuilders).ToString));
    nList.Add(FixData('NumParam:', FParamOptions.Count.ToString));

    for nIdx := 0 to FParamOptions.Count-1 do
    begin
      nParam := FParamOptions[nIdx];
      nList.Add(FixData(Format('Param %d:', [nIdx + 1]),
        Format('[ %s.%s ]%s.%s', [nParam.FGroup, nParam.FID,
        nParam.FGrpName, nParam.FName])));
      //xxxxx
    end;
  finally
    SyncLeave;
  end;
end;

end.
