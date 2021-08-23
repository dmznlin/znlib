{*******************************************************************************
  作者: dmzn@163.com 2021-08-15
  描述: 参数配置管理器

  备注:
  *.参数项:TParamItem
    *.FGroup: 参数分组
    *.FID: 参数标识.同一个分组内FID不能重复,不同分组FID可以相同.
    *.FOptionOnly: 指定在设置某些类型的参数时,不能自由输入,只能选择.
    *.FOwner: 参数对应的组织架构标识.
    *.FEffect: 参数生效方式,参考一下"组织架构与参数项的关系"的解释.
  *.每一项参数由 FOwner.FGroup.FID 唯一性约束,即: 每个FOwner有
    唯一的参数项(FGroup.FID)

  *.组织架构与参数项的关系,以集团、区域、工厂三级架构为例:
    1.组织架构中的每一级,都可以拥有一组参数(1:1),或者不配置参数(0:1).
    2.组织架构中上一级,决定相同的参数(相同Group、ID)如何生效.例如:
      a.集团配置下级生效(etLower),则无论区域如何配置,工厂都以自己的参数为准.
      b.集团配置上级生效(etHigher),则无论区域、工厂如何配置,都以集团为准.
      c.若集团未配置,则区域配置下级生效(etLower),工厂以自己的参数为准.
      d.若集团未配置,则区域配置上级生效(etHigher),工厂以区域的参数为准.
    3.若上一级没有配置参数,则以本级的参数为准.
    4.若本级没有配置参数,则以默认值为准.

  使用方法:
  1.添加Builder
    procedure SystemParams(const nList: TList);
    begin
      gMG.FParamsManager.Default.Init('System', '系统通用参数').
        SetEffect(etLower).
        SetOptionOnly([dtStr]);
      //设置参数默认值

      gMG.FParamsManager.AddParam('001', '第一参数', nList).
        AddS('a1', '', True).
        AddI(11, '', True).
        AddF(11.1);
      //first

      gMG.FParamsManager.AddParam('002', '第二参数', nList).
        AddS('a2', '', True).
        AddI(22, '', True).
        AddF(22.2);
      //second
    end;

    gMG.FParamsManager.AddBuilder(SystemParams);
    //添加至管理器

  2.初始化参数项
    gMG.FParamsManager.InitParameters('Owner01', 'admin');
    //初始化Owner01的参数项

  3.获取参数项
    var nP: TParamItem;
    gMG.FParamsManager.GetParam('system', '001', ['Owner01'], nP)
    //获取组织架构中Owner01在分组system中编号为001参数项
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

  sParamDataType: array[TParamDataType] of string = ('字符', '整数', '浮点',
    '日期');
  //data type desc

  sParamEffectType:array[TParamEffectType] of string = ('下级生效', '上级生效');
  //effect type desc

type
  TParamData<T> = record
    FData    : T;                                  //数据
    FDesc    : string;                             //描述
    FDefault : Boolean;                            //默认值
  end;

  TParamDataItem = record
    Str: array of TParamData<String>;              //字符值
    Int: array of TParamData<Integer>;             //整数值
    Flt: array of TParamData<Double>;              //浮点值
    Date: array of TParamData<TDateTime>;          //日期值
  public
    function IsValid(const nType: TParamDataType;
      const nNum: Integer = 1): Boolean;
    {*验证数据有效*}
    procedure AddS(const nStr: string; const nDesc: string = '';
      const nDef: Boolean = False);
    procedure AddI(const nInt: Integer; const nDesc: string = '';
      const nDef: Boolean = False);
    procedure AddF(const nFlt: Double; const nDesc: string = '';
      const nDef: Boolean = False);
    procedure AddD(const nDate: TDateTime; const nDesc: string = '';
      const nDef: Boolean = False);
    {*添加参数值*}
  end;

  PParamItem = ^TParamItem;
  TParamItem = record
    FEnabled     : Boolean;                        //状态标记
    FRecord      : string;                         //记录标识
    FGroup       : string;                         //参数分组
    FGrpName     : string;                         //分组名称
    FID          : string;                         //参数标识
    FName        : string;                         //参数名称
    FValue       : TParamDataItem;                 //参数值
    FOptionOnly  : TParamDataTypes;                //只使用可选
    FOwner       : string;                         //拥有者id
    FEffect      : TParamEffectType;               //生效方式
  private
  public
    function Init(const nGroup,nName: string): PParamItem;
    {*初始化*}
    function SetGroup(nGroup,nName: string): PParamItem;
    function SetEffect(const nEffect: TParamEffectType): PParamItem;
    function SetOptionOnly(const nOnly: TParamDataTypes): PParamItem;
    {*设置属性*}
    function DefValue<T>(const nItems: array of TParamData<T>;
      const nDefault: T): T;
    {*获取默认值*}
    function AddS(const nStr: string; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    function AddI(const nInt: Integer; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    function AddF(const nFlt: Double; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    function AddD(const nDate: TDateTime; const nDesc: string = '';
      const nDef: Boolean = False): PParamItem;
    {*添加参数值*}
  end;
  TParamItems = array of TParamItem;

  TParamItemBuilder = procedure (const nList: TList);
  //for external-system fill param info

  TParameterManager = class(TManagerBase)
  public
    const
      sTable_SysDict = 'Sys_Dict';                 //参数配置
      sTable_DictExt = 'Sys_DictExt';              //扩展数据
      sParamDefGroup = 'SysParam';                 //默认分组
      sParamDefGName = '系统配置';                 //默认名称
  private
    FParamOptions: TList;
    {*参数项列表*}
    FDefaultItem: TParamItem;
    {*默认配置*}
    FBuilders: array of TParamItemBuilder;
    {*参数配置信息*}
  public
    constructor Create;
    destructor Destroy; override;
    {*创建释放*}
    class procedure RegistMe(const nReg: Boolean); override;
    {*注册管理器*}
    procedure RunAfterRegistAllManager; override;
    {*延迟执行*}
    function Default: PParamItem;
    {*默认配置*}
    procedure AddBuilder(const nBuilder: TParamItemBuilder);
    function AddParam(const nID,nName: string; const nList: TList): PParamItem;
    {*添加数据*}
    function FindParam(const nGroup,nID: string; nList: TList=nil): PParamItem;
    {*检索数据*}
    procedure GetParamData(const nList: TList);
    procedure ClearParamData(const nList: TList; const nFree: Boolean = False);
    {*参数数据*}
    procedure BuildSQL(const nParam: PParamItem; const nEditor: string;
      const nList: TStrings);
    procedure InitParameters(const nOwner,nEditor: string;
      const nMemo: TStrings = nil);
    {*初始化参数*}
    procedure SaveParam(const nParam: PParamItem; const nEditor: string);
    procedure DeleteParam(const nParam: PParamItem);
    {*保存参数项*}
    function GetParam(const nGroup, nID: string; const nOwner: TArray<string>;
      var nParam: TParamItem): Boolean;
    function GetParam2(const nRecord: string; var nParam: TParamItem;
      nQuery: TDataSet = nil): Boolean;
    {*获取参数项*}
    procedure LoadFromFile(const nFile: string);
    procedure SaveToFile(const nFile: string);
    {*文件持久化*}
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    {*获取状态*}
    property ParamItems: TList read FParamOptions;
    {*属性相关*}
  end;

implementation

uses
  UManagerGroup, UDBManager, UDBFun, NativeXml, ULibFun;

procedure WriteLog(const nEvent: string; const nMemo: TStrings = nil);
begin
  if Assigned(nMemo) then
    nMemo.Add(TDateTimeHelper.Time2Str(Now(), True, True) + #9 + nEvent);
  gMG.FLogManager.AddLog(TParameterManager, '参数管理器', nEvent);
end;

//Desc: 添加管理器所需表
procedure AddParameterTables(const nList: TList);
begin
  with gDBManager,TSQLBuilder,TParameterManager do
  begin
    AddTable(sTable_SysDict, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '记录编号').
      AddF('D_Record',      'varchar(32)',            '记录标识').
      AddF('D_Group',       'varchar(32)',            '所属分组').
      AddF('D_GrpName',     'varchar(80)',            '分组名称').
      AddF('D_ID',          'varchar(32)',            '参数标识').
      AddF('D_Name',        'varchar(80)',            '参数名称').
      AddF('D_Str',         'varchar(200)',           '字符值').
      AddF('D_Int',         'integer',                '整数值').
      AddF('D_Double',      sField_SQLServer_Decimal, '浮点值').
      AddF('D_Date',        'DateTime',               '日期值').
      AddF('D_Effect',      'varchar(16)',            '生效方式').
      AddF('D_Owner',       'varchar(32)',            '所属组织').
      AddF('D_Editor',      'varchar(32)',            '修改人').
      AddF('D_EditTime',    'DateTime',               '修改时间').
      //for field
      AddI('idx_id',        'D_Group ASC,D_ID ASC').
      AddI('idx_record',    'D_Record ASC').
      AddI('idx_owner',     'D_Owner ASC');
      //for index

    AddTable(sTable_DictExt, nList, dtMSSQL).
      AddF('R_ID',          sField_SQLServer_AutoInc, '记录标识').
      AddF('V_ID',          'varchar(32)',            '参数标识').
      AddF('V_Desc',        'varchar(80)',            '参数描述').
      AddF('V_Type',        'varchar(16)',            '数据类型').
      AddF('V_Str',         'varchar(200)',           '字符值').
      AddF('V_Int',         'integer',                '整数值').
      AddF('V_Double',      sField_SQLServer_Decimal, '浮点值').
      AddF('V_Date',        'DateTime',               '日期值').
      //for field
      AddI('idx_id',        'V_ID ASC');
      //for index
  end;
end;

//------------------------------------------------------------------------------
//Date: 2021-08-16
//Parm: 分组;分组名称
//Desc: 初始化参数项
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
//Parm: 分组;分组名称
//Desc: 设置分组
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
//Parm: 生效方式
//Desc: 设置生效方式
function TParamItem.SetEffect(const nEffect: TParamEffectType): PParamItem;
begin
  Result := @Self;
  FEffect := nEffect;
end;

//Date: 2021-08-16
//Parm: 只使用可选数据的类型
//Desc: 设置nOnly指定的类型只使用FOptions里的数据,只能选择不能输入.
function TParamItem.SetOptionOnly(const nOnly: TParamDataTypes): PParamItem;
begin
  Result := @Self;
  FOptionOnly := nOnly;
end;

//Date: 2021-08-16
//Parm: 字符串;描述;默认
//Desc: 新增字符串值
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
//Parm: 整数;描述;默认
//Desc: 新增整数值
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
//Parm: 浮点;描述;默认
//Desc: 新增浮点值
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
//Parm: 日期;描述;默认
//Desc: 新增日期时间值
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
//Parm: 参数类型;有效值个数
//Desc: 检测nType参数组内是否有nNum个有效值
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
//Parm: 数组;默认值
//Desc: 在nItems中检索默认值
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
  //参数项列表
end;

destructor TParameterManager.Destroy;
begin
  ClearParamData(FParamOptions, True);
  inherited;
end;

//Date: 2021-08-15
//Parm: 是否注册
//Desc: 向系统注册管理器对象
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
  //检查支持
  gDBManager.AddTableBuilder(AddParameterTables);
end;

//Date: 2021-08-15
//Parm: 配置方法
//Desc: 新增参数项配置方法
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
//Desc: 默认
function TParameterManager.Default: PParamItem;
begin
  Result := @FDefaultItem;
end;

//Date: 2021-08-16
//Parm: 分组;标识
//Desc: 检索nGroup.nID参数项
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
//Parm: 分组;标识;名称
//Desc: 添加参数项
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
//Parm: 列表
//Desc: 获取所有的参数配置项
procedure TParameterManager.GetParamData(const nList: TList);
var nIdx: Integer;
begin
  nList.Clear;
  //init first

  for nIdx := Low(FBuilders) to High(FBuilders) do
    FBuilders[nIdx](nList);
  //xxxxx
end;

//Parm: 列表;是否释放
//Desc: 清理nList参数配置列表
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
//Parm: 参数项;修改人
//Desc: 构建nEntity.FItems[nIdx]的insert,update语句,存入nList中
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
    //记录序列号

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
    //参数默认值
    if nBool then Exit;

    nStr := 'Delete From %s Where V_ID=''%s''';
    nStr := Format(nStr, [sTable_DictExt, FRecord]);
    nList.Add(nStr); //clear extend parameters

    with FValue do
    begin
      nBool := False;
      for nIdx := Low(Str) to High(Str) do
      begin
        if Str[nIdx].FDefault then //第一个默认值已存放主表
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
//Parm: 参数拥有者;修改人id
//Desc: 初始化nOwner的所有参数项
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

    UParameters.WriteLog('::: 创建配置参数 :::', nMemo);
    for nIdx := 0 to FParamOptions.Count -1 do
    begin
      nPItem := FParamOptions[nIdx];
      nStr := nPItem.FGroup + '.' + nPItem.FID;
      if nListA.IndexOf(nStr) < 0 then
      begin
        nPItem.FOwner := nOwner;
        BuildSQL(nPItem, nEditor, nListB);
        UParameters.WriteLog('已创建: ' + nStr, nMemo);
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
//Parm: 参数项;编辑人
//Desc: 保存nParam到数据库
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
//Parm: 参数项
//Desc: 删除nParam
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
//Parm: 记录编号
//Desc: 获取nRecord的参数数据
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
        nStr := Format('编号为[ %s ]的记录不存在', [nRecord]);
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
//Parm: 分组;标识;拥有者架构(从低到高)
//Desc: 获取nOwner的参数nGroup.nID数据,存入nParam中
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
      nStr := Format('参数项[ %s.%s(%s) ]不存在', [nGroup, nID, nSQL]);
      WriteLog(nStr);
      Exit;
    end;

    {---------------------------------------------------------------------------
     *.组织架构和参数的生效方式,请参考单元头说明.
     *.nOwner中的层级顺序为: 本级,上级..,最上级
     *.调用时本级ID必须设置,可以不设置上级
    ---------------------------------------------------------------------------}
    nBool := False;
    nLow := Low(nOwner); //本级id

    for nIdx := High(nOwner) downto nLow do //倒序检索
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
            Exit; //上级生效,直接取上级参数
          end;

          nBool := True; //找到上级
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
//Parm: 存储节点;参数项;读取 or 写入
//Desc: 处理nRoot节点的参数项数据
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
//Parm: 参数文件
//Desc: 从nFile中加载外置参数
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
//Parm: 参数文件
//Desc: 保存参数配置到nFile中
procedure TParameterManager.SaveToFile(const nFile: string);
var nIdx,j: Integer;
    nXML: TNativeXml;
    nNode: TXmlNode;
    nParam,nPNext: PParamItem;

    //Desc: 重置状态
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
           (CompareText(nPNext.FGroup, nParam.FGroup) = 0) then //相同分组
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
