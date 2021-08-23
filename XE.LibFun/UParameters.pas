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
        SetOptionOnly([dtStr]);
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
  System.Classes, System.SysUtils, Data.DB, UBaseObject;

type
  TParamDataType = (dtStr, dtInt, dtFlt, dtDateTime);
  //data type
  TParamDataTypes = set of TParamDataType;

  TParamEffectType = (etLower, etHigher);
  //effect type

const
  cParamDefaultValue = High(Word);
  //value for return

  sParamDataType: array[TParamDataType] of string = ('�ַ�', '����', '����',
    '����');
  //data type desc

  sParamEffectType:array[TParamEffectType] of string = ('�¼���Ч', '�ϼ���Ч');
  //effect type desc

type
  TParamData<T> = record
    FData    : T;                                  //����
    FDesc    : string;                             //����
    FDefault : Boolean;                            //Ĭ��ֵ
  end;

  TParamDataItem = record
    Str: array of TParamData<String>;              //�ַ�ֵ
    Int: array of TParamData<Integer>;             //����ֵ
    Flt: array of TParamData<Double>;              //����ֵ
    Date: array of TParamData<TDateTime>;          //����ֵ
  public
    function IsValid(const nType: TParamDataType;
      const nNum: Integer = 1): Boolean;
    {*��֤������Ч*}
    procedure AddS(const nStr: string; const nDesc: string = '';
      const nDef: Boolean = False);
    procedure AddI(const nInt: Integer; const nDesc: string = '';
      const nDef: Boolean = False);
    procedure AddF(const nFlt: Double; const nDesc: string = '';
      const nDef: Boolean = False);
    procedure AddD(const nDate: TDateTime; const nDesc: string = '';
      const nDef: Boolean = False);
    {*��Ӳ���ֵ*}
  end;

  PParamItem = ^TParamItem;
  TParamItem = record
    FEnabled     : Boolean;                        //״̬���
    FRecord      : string;                         //��¼��ʶ
    FGroup       : string;                         //��������
    FGrpName     : string;                         //��������
    FID          : string;                         //������ʶ
    FName        : string;                         //��������
    FValue       : TParamDataItem;                 //����ֵ
    FOptionOnly  : TParamDataTypes;                //ֻʹ�ÿ�ѡ
    FOwner       : string;                         //ӵ����id
    FEffect      : TParamEffectType;               //��Ч��ʽ
  private
  public
    function Init(const nGroup,nName: string): PParamItem;
    {*��ʼ��*}
    function SetGroup(nGroup,nName: string): PParamItem;
    function SetEffect(const nEffect: TParamEffectType): PParamItem;
    function SetOptionOnly(const nOnly: TParamDataTypes): PParamItem;
    {*��������*}
    function DefValue<T>(const nItems: array of TParamData<T>;
      const nDefault: T): T;
    {*��ȡĬ��ֵ*}
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
  TParamItems = array of TParamItem;

  TParamItemBuilder = procedure (const nList: TList);
  //for external-system fill param info

  TParameterManager = class(TManagerBase)
  public
    const
      sTable_SysDict = 'Sys_Dict';                 //��������
      sTable_DictExt = 'Sys_DictExt';              //��չ����
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

implementation

uses
  UManagerGroup, UDBManager, UDBFun, NativeXml, ULibFun;

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
function TParamItem.SetOptionOnly(const nOnly: TParamDataTypes): PParamItem;
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

procedure TParamDataItem.AddS(const nStr, nDesc: string; const nDef: Boolean);
var nIdx: Integer;
begin
  for nIdx := Low(Str) to High(Str) do
   if CompareText(nStr, Str[nIdx].FData) = 0 then
   begin
     if nDef then
       Str[nIdx].FDefault := True;
     //xxxxx

     if nDesc <> '' then
       Str[nIdx].FDesc := nDesc;
     Exit;
   end;

  nIdx := Length(Str);
  SetLength(Str, nIdx + 1);

  with Str[nIdx] do
  begin
    FData := nStr;
    FDesc := nDesc;
    FDefault := nDef;
  end;
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

procedure TParamDataItem.AddI(const nInt: Integer; const nDesc: string;
  const nDef: Boolean);
var nIdx: Integer;
begin
  for nIdx := Low(Int) to High(Int) do
   if nInt = Int[nIdx].FData then
   begin
     if nDef then
       Int[nIdx].FDefault := True;
     //xxxxx

     if nDesc <> '' then
       Int[nIdx].FDesc := nDesc;
     Exit;
   end;

  nIdx := Length(Int);
  SetLength(Int, nIdx + 1);

  with Int[nIdx] do
  begin
    FData := nInt;
    FDesc := nDesc;
    FDefault := nDef;
  end;
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

procedure TParamDataItem.AddF(const nFlt: Double; const nDesc: string;
  const nDef: Boolean);
var nIdx: Integer;
begin
  for nIdx := Low(Flt) to High(Flt) do
   if nFlt = Flt[nIdx].FData then
   begin
     if nDef then
       Flt[nIdx].FDefault := True;
     //xxxxx

     if nDesc <> '' then
       Flt[nIdx].FDesc := nDesc;
     Exit;
   end;

  nIdx := Length(Flt);
  SetLength(Flt, nIdx + 1);

  with Flt[nIdx] do
  begin
    FData := nFlt;
    FDesc := nDesc;
    FDefault := nDef;
  end;
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

procedure TParamDataItem.AddD(const nDate: TDateTime; const nDesc: string;
  const nDef: Boolean);
var nIdx: Integer;
begin
  for nIdx := Low(Date) to High(Date) do
   if nDate = Date[nIdx].FData then
   begin
     if nDef then
        Date[nIdx].FDefault := True;
     //xxxxx

     if nDesc <> '' then
       Date[nIdx].FDesc := nDesc;
     Exit;
   end;

  nIdx := Length(Date);
  SetLength(Date, nIdx + 1);

  with Date[nIdx] do
  begin
    FData := nDate;
    FDesc := nDesc;
    FDefault := nDef;
  end;
end;

//Date: 2021-08-16
//Parm: ��������;��Чֵ����
//Desc: ���nType���������Ƿ���nNum����Чֵ
function TParamDataItem.IsValid(const nType: TParamDataType;
  const nNum: Integer): Boolean;
begin
  Result := nNum < 1;
  if Result then Exit;
  //check input param

  case nType of
   dtStr      : Result := Length(Str) >= nNum;
   dtInt      : Result := Length(Int) >= nNum;
   dtFlt      : Result := Length(Flt) >= nNum;
   dtDateTime : Result := Length(Date) >= nNum;
  end;
end;

//Date: 2021-08-18
//Parm: ����;Ĭ��ֵ
//Desc: ��nItems�м���Ĭ��ֵ
function TParamItem.DefValue<T>(const nItems: array of TParamData<T>;
  const nDefault: T): T;
var nIdx: Integer;
begin
  for nIdx := Low(nItems) to High(nItems) do
   if nItems[nIdx].FDefault then
   begin
     Result := nItems[nIdx].FData;
     Exit;
   end;

  Result := nDefault;
  //return default
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

      SF('D_Str',      DefValue<string>(FValue.Str, '')),
      SF('D_Int',      DefValue<Integer>(FValue.Int, cParamDefaultValue), sfVal),
      SF('D_Double',   DefValue<Double>(FValue.Flt, cParamDefaultValue), sfVal),
      SF('D_Date',     DateTime2Str(DefValue<TDateTime>(FValue.Date, 0)))
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
        if Str[nIdx].FDefault then //��һ��Ĭ��ֵ�Ѵ������
        begin
          if nBool then
               Continue
          else nBool := True;
        end;

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', Str[nIdx].FDesc),
          SF('V_Type', Enum2Str(dtStr)),
          SF('V_Str', Str[nIdx].FData)
          ], sTable_DictExt, '', True);
        nList.Add(nStr);
      end;

      nBool := False;
      for nIdx := Low(Int) to High(Int) do
      begin
        if Int[nIdx].FDefault then
        begin
          if nBool then
               Continue
          else nBool := True;
        end;

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', Int[nIdx].FDesc),
          SF('V_Type', Enum2Str(dtInt)),
          SF('V_Int', Int[nIdx].FData, sfVal)
          ], sTable_DictExt, '', True);
        nList.Add(nStr);
      end;

      nBool := False;
      for nIdx := Low(Flt) to High(Flt) do
      begin
        if Flt[nIdx].FDefault then
        begin
          if nBool then
               Continue
          else nBool := True;
        end;

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', Flt[nIdx].FDesc),
          SF('V_Type', Enum2Str(dtFlt)),
          SF('V_Double', Flt[nIdx].FData, sfVal)
          ], sTable_DictExt, '', True);
        nList.Add(nStr);
      end;

      nBool := False;
      for nIdx := Low(Date) to High(Date) do
      begin
        if Date[nIdx].FDefault then
        begin
          if nBool then
               Continue
          else nBool := True;
        end;

        nStr := MakeSQLByStr([
          SF('V_ID', FRecord),
          SF('V_Desc', Date[nIdx].FDesc),
          SF('V_Type', Enum2Str(dtDateTime)),
          SF('V_Date', DateTime2Str(Date[nIdx].FData))
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
    nType: TParamDataType;
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
        nType := TStringHelper.Str2Enum<TParamDataType>(nStr);
        nStr := FieldByName('V_Desc').AsString;
        
        case nType of
         dtStr : FValue.AddS(FieldByName('V_Str').AsString, nStr);
         dtInt : FValue.AddI(FieldByName('V_Int').AsInteger, nStr);
         dtFlt : FValue.AddF(FieldByName('V_Double').AsFloat, nStr);
         dtDateTime : FValue.AddD(FieldByName('V_Date').AsDateTime, nStr);
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
    nDT: TParamDataType;
begin
  if nLoad then
  begin
    nStr := nRoot.AttributeValueByName['effect'];
    nParam.SetEffect(TStringHelper.Str2Enum<TParamEffectType>(nStr));

    nStr := nRoot.AttributeValueByName['options'];
    nParam.SetOptionOnly(TStringHelper.Str2Set<TParamDataType,
      TParamDataTypes>(nStr));
    //xxxxx

    nInt := nRoot.NodeCount - 1;
    for nIdx := 0 to nInt do
    begin
      nNode := nRoot.Nodes[nIdx];
      if CompareText('data', nNode.Name) <> 0 then Continue;
      //must be data node

      nDT := TStringHelper.Str2Enum<TParamDataType>(
        nNode.AttributeValueByName['type']);
      //xxxxx

      case nDT of
       dtStr:
        nParam.AddS(nNode.AttributeValueByName['value'],
                    nNode.AttributeValueByName['desc'],
          StrToBool(nNode.AttributeValueByName['default']));
       dtInt:
        nParam.AddI(StrToInt(
                    nNode.AttributeValueByName['value']),
                    nNode.AttributeValueByName['desc'],
          StrToBool(nNode.AttributeValueByName['default']));
       dtFlt:
        nParam.AddF(StrToFloat(
                    nNode.AttributeValueByName['value']),
                    nNode.AttributeValueByName['desc'],
          StrToBool(nNode.AttributeValueByName['default']));
       dtDateTime:
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

    nRoot.AttributeAdd('options', TStringHelper.Set2Str<TParamDataType,
      TParamDataTypes>(nParam.FOptionOnly));
    //xxxxx

    for nIdx := Low(Str) to High(Str) do
    with nRoot.NodeNew('data') do
    begin
      AttributeAdd('type', TStringHelper.Enum2Str<TParamDataType>(dtStr));
      AttributeAdd('value', Str[nIdx].FData);       
      AttributeAdd('default', BoolToStr(Str[nIdx].FDefault, True));
      AttributeAdd('desc', Str[nIdx].FDesc);
    end;

    for nIdx := Low(Int) to High(Int) do
    with nRoot.NodeNew('data') do
    begin
      AttributeAdd('type', TStringHelper.Enum2Str<TParamDataType>(dtint));
      AttributeAdd('value', IntToStr(Int[nIdx].FData));       
      AttributeAdd('default', BoolToStr(Int[nIdx].FDefault, True));
      AttributeAdd('desc', Int[nIdx].FDesc);
    end;

    for nIdx := Low(Flt) to High(Flt) do
    with nRoot.NodeNew('data') do
    begin
      AttributeAdd('type', TStringHelper.Enum2Str<TParamDataType>(dtFlt));
      AttributeAdd('value', FloatToStr(Flt[nIdx].FData));       
      AttributeAdd('default', BoolToStr(Flt[nIdx].FDefault, True));
      AttributeAdd('desc', Flt[nIdx].FDesc);
    end;

    for nIdx := Low(Date) to High(Date) do
    with nRoot.NodeNew('data') do
    begin
      AttributeAdd('type', TStringHelper.Enum2Str<TParamDataType>(dtDateTime));
      AttributeAdd('value', TDateTimeHelper.DateTime2Str(Date[nIdx].FData));       
      AttributeAdd('default', BoolToStr(Date[nIdx].FDefault, True));
      AttributeAdd('desc', Date[nIdx].FDesc);
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
