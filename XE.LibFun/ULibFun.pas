{*******************************************************************************
  作者: dmzn@163.com 2017-02-20
  描述: 项目通用函数定义单元
*******************************************************************************}
unit ULibFun;

{$I LibFun.inc}
interface

uses
  {$IFDEF JsonSerializers}System.JSON.Serializers, System.JSON.Types,{$ENDIF}
  {$IFDEF MSWin}Winapi.Windows, UNetwork,{$ENDIF}
  {$IFDEF HasVCL}Vcl.Forms, Vcl.Controls,{$ENDIF}
  {$IFDEF HasFMX}FMX.Forms, FMX.Controls,{$ENDIF}
  {$IFDEF EnableThirdANSI}UByteString,{$ENDIF}
  {$IFDEF EnableThirdDEC}DECCipherBase, DECCiphers, DECFormat,{$ENDIF}
  System.Classes, System.UITypes, System.SysUtils, System.NetEncoding,
  System.Rtti, System.TypInfo, System.Hash, System.Variants, System.IniFiles,
  System.Math;

type
  ///<summary>命令调用时传递数据的结构</summary>
  PCommandParam = ^TCommandParam;
  TCommandParam = record
  public
    type
      TParamType = (ptStr, ptInt, ptFlt, ptPtr, ptObj, ptDate);
      TParamTypes = set of TParamType;
      {*参数类型*}
      PParamExtend = ^TParamExtend;
      TParamExtend = record
        FType     : TParamType;                          //类型
        FIndex    : Integer;                             //索引
        FDesc     : string;                              //描述
        FDefault  : Boolean;                             //默认值
      end;{*参数扩展*}
  public
    Command: Integer;                                    //命令
    Str: array of string;                                //字符
    Int: array of Integer;                               //整数
    Flt: array of Double;                                //浮点
    Ptr: array of Pointer;                               //指针
    Obj: array of TObject;                               //对象
    Dat: array of TDateTime;                             //时间
    Ext: array of TParamExtend;                          //扩展数据
  private
    FAllowRepeat: Boolean;
    {*允许数据重复*}
    procedure AddExt(const nType: TParamType; const nIdx: Integer;
      const nDesc: string; const nDef: Boolean);
    function AddNew(const nType: TParamType; const nNum: Integer = 1): Integer;
    {*新增数据*}
  public
    function Init(const nCmd: Integer = -1): PCommandParam;
    {*初始化*}
    procedure AllowRepeat(const nAllowed: Boolean);
    {*是否允许重复*}
    function AddS(const nS: string; const nDesc: string = '';
      const nDef: Boolean = False): PCommandParam; overload;
    function AddI(const nI: Integer; const nDesc: string = '';
      const nDef: Boolean = False): PCommandParam; overload;
    function AddF(const nF: Double; const nDesc: string = '';
      const nDef: Boolean = False): PCommandParam; overload;
    function AddP(const nP: Pointer; const nDesc: string = '';
      const nDef: Boolean = False): PCommandParam; overload;
    function AddO(const nO: TObject; const nDesc: string = '';
      const nDef: Boolean = False): PCommandParam; overload;
    function AddD(const nD: TDateTime; const nDesc: string = '';
      const nDef: Boolean = False): PCommandParam; overload;
    {*添加单个数据*}
    function AddS(const nS: array of string): PCommandParam; overload;
    function AddI(const nI: array of Integer): PCommandParam; overload;
    function AddF(const nF: array of Double): PCommandParam; overload;
    function AddP(const nP: array of Pointer): PCommandParam; overload;
    function AddO(const nO: array of TObject): PCommandParam; overload;
    function AddD(const nD: array of TDateTime): PCommandParam; overload;
    {*添加多个数据*}
    function GetExt(const nType: TParamType; const nIdx: Integer):PParamExtend;
    {*检索数据*}
    function DefaultS(const nDef: string = ''): string;
    function DefaultI(const nDef: Integer = 0): Integer;
    function DefaultF(const nDef: Double = 0): Double;
    function DefaultP(const nDef: Pointer = nil): Pointer;
    function DefaultO(const nDef: TObject = nil): TObject;
    function DefaultD(const nDef: TDateTime = 0): TDateTime;
    {*获取默认值*}
    function IsValid(const nType: TParamType; const nNum: Integer = 1): Boolean;
    {*验证数据有效*}
  end;

  TApplicationHelper = class
  public
    const
      sVerifyCode   = ';Verify:';
      //id verify
      sVerifyIgnore = '@@@';
      //ignore prefix when verify
      sDefaultKey   = 'sysadmin';
      //the key for encrypt data
      sDefaultLang  = 'cn';
      //language id

    type
      TCPUID = array[1..4] of Longint;
      //id record

      TDeployType = (Desktop, Web, Mobile, dtAll);
      TDeployTypes = set of TDeployType;
      //部署类型: Desktop-Client,PC-Web,Mobile-Web

      TOrganizationStructure = (osGroup, osArea, osFactory);
      TOrganizationStructures = set of TOrganizationStructure;
      //组织架构: 集团,区域,工厂

      TProgramConfig = record
        FProgram    : string;                            //程序标识
        FTitleApp   : string;                            //状态栏名称
        FTitleMain  : string;                            //主窗体名称
        FCopyRight  : string;                            //程序版权声明
        FDeployName : string;                            //部署单位名称
        FDeployType : TDeployType;                       //部署类型(方式)

        //for server mode
        FPort       : Integer;                           //服务端口
        FFavicon    : string;                            //收藏夹显示图标
      end;

      TAppParam = record
        FGroupID    : string;                            //所属集团
        FFactory    : string;                            //所属工厂
        FLocalIP    : string;                            //本机IP
        FLocalMAC   : string;                            //本机MAC
        FLocalName  : string;                            //本机名称
        FAdminKey   : string;                            //管理员密钥

        FActive     : TProgramConfig;                    //活动程序
        FPrograms   : TArray<TProgramConfig>;            //程序参数
      end;

    class var
      gPath         : string;                            //系统所在路径
      gSysConfig    : string;                            //系统配置文件
      gFormConfig   : string;                            //窗体配置文件
      gDBConfig     : string;                            //数据库配置文件
      gLogPath      : string;                            //日志所在目录

  public
    class function GetCPUIDStr: string; static;
    //处理器标识
    class procedure AddExpireDate(const nFile,nDate: string;
      const nInit: Boolean); static;
    class function IsSystemExpire(const nFile: string): Boolean; static;
    //系统日期限制
    class procedure AddVerifyData(const nFile,nSeed: string); static;
    //为nFile添加校验信息
    class function IsValidConfigFile(const nFile,nSeed: string):Boolean;static;
    //校验nFile是否合法配置文件
    {$IFDEF HasVCL}
    class procedure EnumSubCtrlList(const nPCtrl: TWinControl;
      const nList: TList); static;
    //枚举nPCtrl的所有子控件
    {$ENDIF}
    class procedure LoadFormConfig(const nForm: TForm;
      const nIniF: TIniFile = nil; const nFile: string = ''); static;
    //载入窗体信息
    class procedure SaveFormConfig(const nForm: TForm;
      const nIniF: TIniFile = nil; const nFile: string = ''); static;
    //存储窗体信息
    class function ReplaceGlobalPath(const nPath: string): string; static;
    //替换nPath中的$Path宏定义
    class procedure LoadParameters(var nParam: TAppParam; nIni: TIniFile = nil;
      const nExtend: Boolean = False); static;
    //载入系统配置参数
  end;

  TStringHelper = class
  public
    const
      cFI = Low(String); //FirstIndex
      //字符首字母索引
    type
      TFillPos = (fpLeft, fpMid, fpRight);
      //填充位置:左,中间,右
      TTrimPos = (tpNo, tpLTrim, tpRTrim, tpTrim);
      //修剪方式:不修剪,左侧,右侧,两侧
      TStringArray = array of string;
      //字符串动态数组

      PMacroItem = ^TMacroItem;
      TMacroItem = record
        FMacro: string;                                  //宏定义
        FValue: string;                                  //宏取值
      end;
      TDynamicMacroArray = array of TMacroItem;          //宏定义数组

      PDictionaryItem = ^TDictionaryItem;
      TDictionaryItem = record
        FKey     : string;                               //标识
        FValue   : string;                               //取值
        FParam   : string;                               //附加
      end;
      TDictionaryItems = array of TDictionaryItem;       //字典列表

  public
    class function CopyLeft(const nStr: string; nSize: Word): string; static;
    class function CopyRight(const nStr: string; nSize: Word): string; static;
    class function CopyNoLeft(const nStr: string; nSize: Word): string; static;
    class function CopyNoRight(const nStr: string; nSize: Word): string; static;
    //复制两端字符串
    class function MI(const nMacro,nValue: string): TMacroItem; static;
    class function MacroValue(const nData: string;
      const nMacro: TDynamicMacroArray): string; static;
    //处理宏定义
    class function MS(const nData: array of string;
      const nIdx: Integer): string; overload; static;
    class function MS(const nData: array of string;
      const nFirst: Boolean): string; overload; static;
    //多个字符串中选择一个(multi select)
    class function StrListIndex(const nStr: string; const nList: TStrings;
      const nSection: Integer; const nFlag: string = ''): Integer;
    class function StrArrayIndex(const nStr: string; const nArray: TStringArray;
      const nIgnoreCase: Boolean = True): integer; static;
    //字符串索引
    class function Combine(const nList: TStrings;
      nFlag: string = '';
      const nFlagEnd: Boolean = True): string; overload; static;
    class function Combine(const nStrArray: array of string; 
      nFlag: string = '';      
      const nFlagEnd: Boolean = True): string; overload; static;
    class function Split(const nStr: string; const nList: TStrings;
      nFlag: string = ''; const nTrim: TTrimPos = tpNo;
      const nNum: Word = 0; const nFlagEnd: Boolean = True): Boolean; static;
    class function SplitArray(const nStr: string; var nArray: TStringArray;
      nFlag: string = ''; const nTrim: TTrimPos = tpNo;
      const nNum: Word = 0; const nFlagEnd: Boolean = True): Boolean; static;
    //合并,拆分字符串
    class function AdjustFormat(const nItems,nSymbol: string; 
      const nAdd: Boolean; nFlag: string = ''; 
      const nFlagEnd: Boolean = True): string; overload; static;
    class function AdjustFormat(const nList: TStrings; 
      const nSymbol: string; const nAdd: Boolean;
      nFlag: string = ''; const nFlagEnd: Boolean = True;      
      const nListYet: Boolean = True): string; overload; static;
    //格式化列表字符串
    class function FixWidth(const nStr: string;
      const nWidth: Word; const nStyle: TFillPos = fpRight;
      const nFixChar: Char = #32; const nClip: Boolean = False;
      const nAnsiLen: Boolean = False): string; static;
    //定长字符串
    class function SplitValue(const nStr: string; 
      const nList: TStrings): Boolean; static;
    class function SplitInt(const nStr: string; 
      const nDef: Integer = 0): Integer; static;
    class function SplitFloat(const nStr: string;
      const nDef: Double = 0): Double; static;
    //拆分出数值
    {$IFNDEF EnableThirdANSI}
    class function GetPinYin(const nCH: string): string; static;
    //获取nChinese的拼音简写
    {$ENDIF}
    class function MirrorStr(const nStr: string): string; static;
    //镜像反转nStr字符串
    class function IsNumber(const nStr: string;
      const nFloat: Boolean = True): Boolean; static;
    //是否数值
    class function CountChar(const nChar: Char; const nStr: string;
      const nRepeatValid: Boolean): Cardinal; static;
    //统计字符在字符串中的个数
    class function ByHex(const nHex: string): string; static;
    //使用特定数据构建字符串
    class function HexStr(const nStr: string): string; static;
    class function HexBytes(const nBytes: TBytes): string; static;
    //转换为十六进制
    class function Str2Bytes(const nStr: string): TBytes; static;
    class function Bytes2Str(const nBytes: TBytes): string; static;
    //按单字节转换字符串
    class function StrIF(const nData: array of string;
      const nIdx: Integer): string; overload; static;
    class function StrIF(const nData: array of string;
      const nFirst: Boolean): string; overload; static;
    //按条件选择字符串
    class function Enum2Str<T>(const nEnum: T): string; static;
    class function Str2Enum<T>(const nEnum: string): T; static;
    class procedure EnumItems<T>(const nList: TStrings;
      const nSerial: Boolean = False); static;
    //获取枚举类型字符串描述
    class function Set2Str<T,ST>(const nSet: ST): string; static;
    class function Str2Set<T,ST>(const nSet: string): ST; static;
    //获取集合类型的字符串描述
    class procedure FillList(const nList: TStrings;
      const nArray: array of string;
      const nClearFirst: Boolean = True; const nSerial: Boolean = False); static;
    //使用数据填充列表
    class function Ansi_UTF8(const nStr: string): string; static;
    class function Ansi_Unicode(const nStr: string): string; static;
    class function UTF8_Unicode(const nStr: string): string; static;
    class function UTF8_Ansi(const nStr: string): string; static;
    class function Unicode_UTF8(const nStr: string): string; static;
    class function Unicode_Ansi(const nStr: string): string; static;
    //字符串编码转换
  end;

  TSQLBuilder = class
  public
    type
      TFieldType = (sfStr, sfVal, sfDate, sfTime, sfDateTime);
      //string, date, time, value

    class function SF(const nField: string; const nValue: Variant;
      const nType: TFieldType = sfStr): string; static;
    class function SF_IF(const nData: array of string;
      const nIdx: Integer): string; overload; static;
    class function SF_IF(const nData: array of string;
      const nFirst: Boolean): string; overload; static;
    //make sql field
    class function MakeSQLByStr(const nData: array of string;
      const nTable: string; const nWhere: string = '';
      const nIsNew: Boolean = True): string; static;
    //make insert,update sql
    class function SQM(const nData: string): string; static;
    //add single quotation marks
  end;

  TFloatHelper = class
  public
    type
      TFloatRelationType = (rtGreater, rtGE, rtEqual, rtLE, rtLess);
      //浮点关系:>, >=, =, <=, <
  
    class function Float2PInt(const nValue: Double; 
      const nPrecision: Integer = 100;
      const nRound: Boolean = True): Int64; static;
    class function Float2Float(const nValue: Double; 
      const nPrecision: Integer = 100;
      const nRound: Boolean = True): Double; static;
    //按精度转换浮点
    class function FloatRelation(const nA,nB: Double; 
      const nType: TFloatRelationType;
      const nPrecision: Integer = 100): Boolean; static;
    //浮点关系判定
  end;

  TBitHelper = class
  public
    type
      TBitCount = (Bit_8 = 8, Bit_16 = 16, Bit_32 = 32);
      //可操作字节数

    class function GetBit(const nNum: Integer; const nBit: Byte;
      const nBitCount: TBitCount): Byte; static;
    //查询指定位是0或1
    class function SetBit(const nNum: Integer; const nBit,nValue: Byte;
      const nBitCount: TBitCount): Integer; static;
    //设置指定位的值为0或1
    class function Int2Bin(const nValue: LongInt;
      const nBitCount: TBitCount): string;
    //十进制转二进制显示
    class function Bin2Int(const nValue: string): LongInt;
    //二进制转十进制数值
  end;

  TEncodeHelper = class
  public
    class function EncodeBase32(const nData: string): string; static;
    class function DecodeBase32(const nData: string): string; static;
    //base32
    class function EncodeBase64(const nData: string;
      const nLineBreak: Boolean = False): string; static;
    class function DecodeBase64(const nData: string): string; static;
    //base64
    class function EncodeMD5(const nData: string): string; static;
    //md5
    class function Encode_1DES(const nData,nKey: string): string; static;
    class function Decode_1DES(const nData,nKey: string): string; static;
    class function Encode_3DES(const nData,nKey: string): string; static;
    class function Decode_3DES(const nData,nKey: string): string; static;
    //DES en&decode
    {$IFDEF JsonSerializers}
    class function EncodeRecord<T: record>(const nRecord: T): string; static;
    class function DecodeRecord<T: record>(const nRecord: string): T; static;
    //serialize record
    {$ENDIF}
  end;

  TDateTimeHelper = class
  public
    type
      TTickDefault = (tdZero, tdNow);
      //value when tick=0
    class function Str2Date(const nStr: string): TDate; static;
    //change nStr to date value
    class function Str2Time(const nStr: string): TTime; static;
    //change nStr to time value
    class function Date2Str(const nDate: TDateTime;
      const nSeparator: Boolean = True): string; static;
    //change nDate to string value
    class function Time2Str(const nTime: TDateTime;
      const nSeparator: Boolean = True;
      const nMSec: Boolean = False): string; static;
    //change nTime to string value
    class function DateTime2Str(const nDT: TDateTime): string; static;
    //change nDT to string value
    class function Str2DateTime(const nStr: string): TDateTime; static;
    //change nStr to datetime value
    class function Date2CH(const nDate: TDate): string; static;
    //change nDate to chinese string
    class function Time2CH(const nTime: TTime): string; static;
    //change nTime to chinese string
    class function Date2Week(nPrefix: string = '';
      nDate: TDateTime = 0): string; static;
    //get the week of nDate
    class function TimeLong2CH(const nTime: Int64): string; static;
    //change time long to chinese string
    class function DateTimeSerial(const nSeparator: Boolean = False): string; static;
    //serial id by date
    class function GetTickCount: Cardinal; static;
    //tick counter
    class function GetTickCountDiff(const nCount: Cardinal;
      const nDefault: TTickDefault = tdNow): Int64; static;
    //result = gettickcount - nCount
  end;

implementation

//------------------------------------------------------------------------------
//Date: 2021-07-06
//Desc: 初始化
function TCommandParam.Init(const nCmd: Integer): PCommandParam;
var nInit: TCommandParam;
begin
  FillChar(nInit, SizeOf(TCommandParam), #0);
  Self := nInit;
  Result := @Self;

  if nCmd >= 0 then
    Command := nCmd;
  FAllowRepeat := True;
end;

//Date: 2021-08-24
//Parm: 是否允许
//Desc: 设置是否允许数据重复
procedure TCommandParam.AllowRepeat(const nAllowed: Boolean);
begin
  FAllowRepeat := nAllowed;
end;

//Date: 2021-07-06
//Parm: 参数类型;新增个数
//Desc: 新增nNum个nType参数项
function TCommandParam.AddNew(const nType: TParamType;
  const nNum: Integer): Integer;
begin
  Result := -1;
  //init first

  case nType of
   ptStr:
    begin
      Result := Length(Str);
      SetLength(Str, Result + nNum);
    end;
   ptInt:
    begin
      Result := Length(Int);
      SetLength(Int, Result + nNum);
    end;
   ptFlt:
    begin
      Result := Length(Flt);
      SetLength(Flt, Result + nNum);
    end;
   ptPtr:
    begin
      Result := Length(Ptr);
      SetLength(Ptr, Result + nNum);
    end;
   ptObj:
    begin
      Result := Length(Obj);
      SetLength(Obj, Result + nNum);
    end;
   ptDate:
    begin
      Result := Length(Dat);
      SetLength(Dat, Result + nNum);
    end;
  end;

  if Result < 0 then
    raise Exception.Create('ULibFun.AddNew: Invalid Param Type.');
  //xxxxx
end;

//Date: 2021-08-24
//Parm: 参数类型;索引;描述;默认值
//Desc: 添加nType.nIdx的扩展数据
procedure TCommandParam.AddExt(const nType: TParamType; const nIdx: Integer;
  const nDesc: string; const nDef: Boolean);
var nInt: Integer;
    nExt: PParamExtend;
begin
  if nDef or (nDesc <> '') then //valid data
  begin
    nExt := GetExt(nType, nIdx);
    if Assigned(nExt) then
    begin
      if nDef then
        nExt.FDefault := True;
      //xxxxx

      if nDesc <> '' then
        nExt.FDesc := nDesc;
      Exit;
    end;

    nInt := Length(Ext);
    SetLength(Ext, nInt + 1);
    //add extend

    with Ext[nInt] do
    begin
      FType    := nType;
      FIndex   := nIdx;
      FDesc    := nDesc;
      FDefault := nDef;
    end;
  end;
end;

//Date: 2021-08-24
//Parm: 参数类型;索引
//Desc: 获取nType.nIdx的扩展数据
function TCommandParam.GetExt(const nType: TParamType;
  const nIdx: Integer): PParamExtend;
var nI: Integer;
begin
  for nI := Low(Ext) to High(Ext) do
   with Ext[nI] do
    if (nType = FType) and (nIdx = FIndex) then
    begin
      Result := @Ext[nI];
      Exit;
    end;
  //xxxxx

  Result := nil;
end;

//Date: 2021-07-06
//Parm: 字符串
//Desc: 添加nS到结构
function TCommandParam.AddS(const nS, nDesc: string;
  const nDef: Boolean): PCommandParam;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if not FAllowRepeat then
  begin
    for nIdx := Low(Str) to High(Str) do
     if CompareText(nS, Str[nIdx]) = 0 then
     begin
       AddExt(ptStr, nIdx, nDesc, nDef);
       Exit;
     end;
  end;

  nIdx := AddNew(ptStr);
  Str[nIdx] := nS;
  AddExt(ptStr, nIdx, nDesc, nDef);
end;

//Date: 2021-07-06
//Parm: 字符串数组
//Desc: 添加nS到结构
function TCommandParam.AddS(const nS: array of string): PCommandParam;
var i,nIdx: Integer;
begin
  i := AddNew(ptStr, Length(nS));
  for nIdx := Low(nS) to High(nS) do
    Str[nIdx + i] := nS[nIdx];
  Result := @Self;
end;

//Date: 2021-08-24
//Parm: 默认字符串
//Desc: 获取默认字符串
function TCommandParam.DefaultS(const nDef: string): string;
var nIdx: Integer;
begin
  for nIdx := Low(Ext) to High(Ext) do
   with Ext[nIdx] do
    if (FType = ptStr) and FDefault then
    begin
      Result := Str[FIndex];
      Exit;
    end;
  //xxxxx

  Result := nDef;
end;

//Date: 2021-07-06
//Parm: 整数
//Desc: 添加nI到结构
function TCommandParam.AddI(const nI: Integer; const nDesc: string;
  const nDef: Boolean): PCommandParam;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if not FAllowRepeat then
  begin
    for nIdx := Low(Int) to High(Int) do
     if nI = Int[nIdx] then
     begin
       AddExt(ptInt, nIdx, nDesc, nDef);
       Exit;
     end;
  end;

  nIdx := AddNew(ptInt);
  Int[nIdx] := nI;
  AddExt(ptInt, nIdx, nDesc, nDef);
end;

//Date: 2021-07-06
//Parm: 整数数组
//Desc: 添加nI到结构
function TCommandParam.AddI(const nI: array of Integer): PCommandParam;
var i,nIdx: Integer;
begin
  i := AddNew(ptInt, Length(nI));
  for nIdx := Low(nI) to High(nI) do
    Int[nIdx + i] := nI[nIdx];
  Result := @Self;
end;

//Date: 2021-08-24
//Parm: 默认整数
//Desc: 获取默认整数值
function TCommandParam.DefaultI(const nDef: Integer): Integer;
var nIdx: Integer;
begin
  for nIdx := Low(Ext) to High(Ext) do
   with Ext[nIdx] do
    if (FType = ptInt) and FDefault then
    begin
      Result := Int[FIndex];
      Exit;
    end;
  //xxxxx

  Result := nDef;
end;

//Date: 2021-07-06
//Parm: 浮点
//Desc: 添加nF到结构
function TCommandParam.AddF(const nF: Double; const nDesc: string;
  const nDef: Boolean): PCommandParam;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if not FAllowRepeat then
  begin
    for nIdx := Low(Flt) to High(Flt) do
     if nF = Flt[nIdx] then
     begin
       AddExt(ptFlt, nIdx, nDesc, nDef);
       Exit;
     end;
  end;

  nIdx := AddNew(ptFlt);
  Flt[nIdx] := nF;
  AddExt(ptFlt, nIdx, nDesc, nDef);
end;

//Date: 2021-07-06
//Parm: 浮点数组
//Desc: 添加nF到结构
function TCommandParam.AddF(const nF: array of Double): PCommandParam;
var i,nIdx: Integer;
begin
  i := AddNew(ptFlt, Length(nF));
  for nIdx := Low(nF) to High(nF) do
    Flt[nIdx + i] := nF[nIdx];
  Result := @Self;
end;

//Date: 2021-08-24
//Parm: 默认浮点
//Desc: 获取默认浮点值
function TCommandParam.DefaultF(const nDef: Double): Double;
var nIdx: Integer;
begin
  for nIdx := Low(Ext) to High(Ext) do
   with Ext[nIdx] do
    if (FType = ptFlt) and FDefault then
    begin
      Result := Flt[FIndex];
      Exit;
    end;
  //xxxxx

  Result := nDef;
end;

//Date: 2021-07-06
//Parm: 指针
//Desc: 添加nP到结构
function TCommandParam.AddP(const nP: Pointer; const nDesc: string;
  const nDef: Boolean): PCommandParam;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if not FAllowRepeat then
  begin
    for nIdx := Low(Ptr) to High(Ptr) do
     if nP = Ptr[nIdx] then
     begin
       AddExt(ptPtr, nIdx, nDesc, nDef);
       Exit;
     end;
  end;

  nIdx := AddNew(ptPtr);
  Ptr[nIdx] := nP;
  AddExt(ptPtr, nIdx, nDesc, nDef);
end;

//Date: 2021-07-06
//Parm: 指针数组
//Desc: 添加nP到结构
function TCommandParam.AddP(const nP: array of Pointer): PCommandParam;
var i,nIdx: Integer;
begin
  i := AddNew(ptPtr, Length(nP));
  for nIdx := Low(nP) to High(nP) do
    Ptr[nIdx + i] := nP[nIdx];
  Result := @Self;
end;

//Date: 2021-08-24
//Parm: 默认指针
//Desc: 获取默认指针
function TCommandParam.DefaultP(const nDef: Pointer): Pointer;
var nIdx: Integer;
begin
  for nIdx := Low(Ext) to High(Ext) do
   with Ext[nIdx] do
    if (FType = ptPtr) and FDefault then
    begin
      Result := Ptr[FIndex];
      Exit;
    end;
  //xxxxx

  Result := nDef;
end;

//Date: 2021-07-06
//Parm: 对象
//Desc: 添加nO到结构
function TCommandParam.AddO(const nO: TObject; const nDesc: string;
  const nDef: Boolean): PCommandParam;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if not FAllowRepeat then
  begin
    for nIdx := Low(Obj) to High(Obj) do
     if nO = Obj[nIdx] then
     begin
       AddExt(ptObj, nIdx, nDesc, nDef);
       Exit;
     end;
  end;

  nIdx := AddNew(ptObj);
  Obj[nIdx] := nO;
  AddExt(ptObj, nIdx, nDesc, nDef);
end;

//Date: 2021-07-06
//Parm: 对象数组
//Desc: 添加nO到结构
function TCommandParam.AddO(const nO: array of TObject): PCommandParam;
var i,nIdx: Integer;
begin
  i := AddNew(ptObj, Length(nO));
  for nIdx := Low(nO) to High(nO) do
    Obj[nIdx + i] := nO[nIdx];
  Result := @Self;
end;

//Date: 2021-08-24
//Parm: 默认对象
//Desc: 获取默认对象
function TCommandParam.DefaultO(const nDef: TObject): TObject;
var nIdx: Integer;
begin
  for nIdx := Low(Ext) to High(Ext) do
   with Ext[nIdx] do
    if (FType = ptObj) and FDefault then
    begin
      Result := Obj[FIndex];
      Exit;
    end;
  //xxxxx

  Result := nDef;
end;

//Date: 2021-08-24
//Parm: 日期时间
//Desc: 添加nD到结构
function TCommandParam.AddD(const nD: TDateTime; const nDesc: string;
  const nDef: Boolean): PCommandParam;
var nIdx: Integer;
begin
  Result := @Self;
  //return self address

  if not FAllowRepeat then
  begin
    for nIdx := Low(Dat) to High(Dat) do
     if nD = Dat[nIdx] then
     begin
       AddExt(ptDate, nIdx, nDesc, nDef);
       Exit;
     end;
  end;

  nIdx := AddNew(ptDate);
  Dat[nIdx] := nD;
  AddExt(ptDate, nIdx, nDesc, nDef);
end;

//Date: 2021-08-24
//Parm: 日期数组
//Desc: 添加nD到结构
function TCommandParam.AddD(const nD: array of TDateTime): PCommandParam;
var i,nIdx: Integer;
begin
  i := AddNew(ptDate, Length(nD));
  for nIdx := Low(nD) to High(nD) do
    Dat[nIdx + i] := nD[nIdx];
  Result := @Self;
end;

//Date: 2021-08-24
//Parm: 默认日期
//Desc: 获取默认日期
function TCommandParam.DefaultD(const nDef: TDateTime): TDateTime;
var nIdx: Integer;
begin
  for nIdx := Low(Ext) to High(Ext) do
   with Ext[nIdx] do
    if (FType = ptDate) and FDefault then
    begin
      Result := Dat[FIndex];
      Exit;
    end;
  //xxxxx

  Result := nDef;
end;

//Date: 2021-07-07
//Parm: 参数类型;有效值个数
//Desc: 检测nType参数组内是否有nNum个有效值
function TCommandParam.IsValid(const nType: TParamType;
  const nNum: Integer): Boolean;
begin
  Result := nNum < 1;
  if Result then Exit;
  //check input param

  case nType of
   ptStr: Result := Length(Str) >= nNum;
   ptInt: Result := Length(Int) >= nNum;
   ptFlt: Result := Length(Flt) >= nNum;
   ptPtr: Result := Length(Ptr) >= nNum;
   ptObj: Result := Length(Obj) >= nNum;
   ptDate:Result := Length(Dat) >= nNum;
  end;
end;

//------------------------------------------------------------------------------
//Date: 2018-03-15
//Desc: CPU标识字符串
class function TApplicationHelper.GetCPUIDStr: string;
var nIdx: Integer;
    nCPU: TCPUID;

    {$IFDEF UseASM}
    function GetCPUID: TCPUID; assembler; register;
    asm
      PUSH    EBX         {Save affected register}
      PUSH    EDI
      MOV     EDI,EAX     {@Resukt}
      MOV     EAX,1
      DW      $A20F       {CPUID Command}
      STOSD                {CPUID[1]}
      MOV     EAX,EBX
      STOSD               {CPUID[2]}
      MOV     EAX,ECX
      STOSD               {CPUID[3]}
      MOV     EAX,EDX
      STOSD               {CPUID[4]}
      POP     EDI         {Restore registers}
      POP     EBX
    end;
    {$ENDIF}
begin
  try
    for nIdx:=Low(nCPU) to High(nCPU) do
      nCPU[nIdx] := -1;
    //xxxxx

    {$IFDEF UseASM}
    nCPU := GetCPUID;
    {$ENDIF}
    Result := Format('%.8x', [nCPU[1]]);
  except
    Result := 'unknown';
  end;
end;

//Date: 2018-03-15
//Parm: 配置文件;日期;是否初始化
//Desc: 添加过期日期限制
class procedure TApplicationHelper.AddExpireDate(const nFile, nDate: string;
  const nInit: Boolean);
var nStr,nEn: string;
begin
  with TEncodeHelper,TDateTimeHelper,TIniFile.Create(nFile) do
  try
    if nInit then
    begin
      nStr := ReadString('System', 'Local', '');
      if nStr = '' then
           nEn := 'N'
      else nEn := 'Y';

      WriteString('System', 'Unlock', nEn);
      //has id,then unlock

      nStr := EncodeBase64(nDate);
      WriteString('System', 'Expire', nStr);

      nEn := TEncodeHelper.EncodeBase64(Date2Str(Now));
      nStr := nStr + nEn;
      WriteString('System', 'DateBase', nEn);
      WriteString('System', 'DateUpdate', nEn);

      nStr := 'run_' + nStr + ReadString('System', 'Unlock', '');
      WriteString('System', 'DateVerify', EncodeMD5(nStr));
    end else
    begin
      nEn := ReadString('System', 'Local', '');
      if nEn = '' then
      begin
        nEn := EncodeMD5('id:' + GetCPUIDStr);
        WriteString('System', 'Unlock', 'N');
        WriteString('System', 'Local', nEn);
      end; //no id,add them

      nEn := ReadString('System', 'DateBase', '');
      nEn := DecodeBase64(nEn);

      if Date2Str(Now) <> nEn then
      begin
        if Date() < Str2Date(nEn) then
        begin
          nStr := ReadString('System', 'DateUpdate', '');
          nStr := DecodeBase64(nStr);

          if Date() = Str2Date(nStr) then Exit;
          nEn := Date2Str(Str2Date(nEn) + 1)
        end else nEn := Date2Str(Now);

        nEn := EncodeBase64(nEn);
        WriteString('System', 'DateBase', nEn);

        nStr := ReadString('System', 'Expire', '') + nEn +
                ReadString('System', 'Unlock', '');
        WriteString('System', 'DateVerify', EncodeMD5('run_' + nStr));

        nEn := EncodeBase64(Date2Str(Now));
        WriteString('System', 'DateUpdate', nEn);
      end;
    end;
  finally
    Free;
  end;
end;

//Date: 2018-03-15
//Parm: 配置文件
//Desc: 验证nFile文件配置的日期是否过期
class function TApplicationHelper.IsSystemExpire(const nFile: string): Boolean;
var nStr,nEn,nLock: string;
begin
  with TEncodeHelper,TDateTimeHelper,TIniFile.Create(nFile) do
  try
    Result := True;
    AddExpireDate(nFile, '', False);

    nStr := EncodeMD5('id:' + GetCPUIDStr);
    if nStr <> ReadString('System', 'Local', '') then Exit;
    //id invalid

    nLock := ReadString('System', 'Unlock', '');
    if nLock <> 'Y' then Exit;
    //unlock version

    nStr := ReadString('System', 'Expire', '');
    nEn := ReadString('System', 'DateBase', '');

    if ReadString('System', 'DateVerify', '') =
       EncodeMD5('run_' + nStr + nEn + nLock) then
    begin
      nStr := DecodeBase64(nStr);
      nEn := DecodeBase64(nEn);
      Result := Str2Date(nStr) <= Str2Date(nEn);
    end;
  finally
    Free;
  end;
end;

//Date: 2021-05-06
//Parm: 列表;种子
//Desc: 将nList中需要忽略的内容,替换为nSeed
procedure VerifyIgnore(const nList: TStrings; const nSeed: string);
var nIdx: Integer;
begin
  with TApplicationHelper do
  begin
    for nIdx := nList.Count - 1 downto 1 do
     if Copy(nList[nIdx], TStringHelper.cFI, 3) = sVerifyIgnore then
      nList[nIdx] := nSeed;
  end;
end;

//Date: 2018-03-15
//Parm: 文件全路径;加密种子
//Desc: 为nFile添加校验信息
class procedure TApplicationHelper.AddVerifyData(const nFile, nSeed: string);
var nStr: string;
    nList,nTmp: TStrings;
begin
  if not FileExists(nFile) then Exit;
  nTmp := nil;
  nList := TStringList.Create;
  try
    nList.LoadFromFile(nFile);
    if (nList.Count > 0) and (Pos(sVerifyCode, nList[0]) = 1) then
         nList[0] := nSeed
    else nList.Insert(0, nSeed);

    nTmp := TStringList.Create;
    nTmp.AddStrings(nList);
    VerifyIgnore(nTmp, nSeed);

    nStr := sVerifyCode + TEncodeHelper.EncodeMD5(nTmp.Text);
    nList[0] := nStr;
    nList.SaveToFile(nFile);
  finally
    nTmp.Free;
    nList.Free;
  end;
end;

//Date: 2018-03-15
//Parm: 文件全路径;加密种子
//Desc: 校验nFile是否合法配置文件
class function TApplicationHelper.IsValidConfigFile(const nFile,
  nSeed: string): Boolean;
var nStr: string;
    nList: TStrings;
begin
  Result := False;
  if not FileExists(nFile) then Exit;

  nStr := ExtractFilePath(nFile);
  nStr := nStr + nSeed + '.run';

  if FileExists(nStr) then
  begin
    AddVerifyData(nFile, nSeed); Result := True; Exit;
  end;

  nList := TStringList.Create;
  try
    nList.LoadFromFile(nFile);
    if (nList.Count > 0) and (Pos(sVerifyCode, nList[0]) = 1) then
    begin
      nStr := nList[0];
      System.Delete(nStr, 1, Length(sVerifyCode));

      nList[0] := nSeed;
      VerifyIgnore(nList, nSeed);
      Result := TEncodeHelper.EncodeMD5(nList.Text) = nStr;
    end;
  finally
    nList.Free;
  end;
end;

{$IFDEF HasVCL}
//Date: 2018-05-07
//Parm: 父容器控件;列表
//Desc: 枚举nPCtrl的所有子控件,放入nList中
class procedure TApplicationHelper.EnumSubCtrlList(const nPCtrl: TWinControl;
  const nList: TList);
var i,nCount: integer;
begin
  nCount := nPCtrl.ControlCount - 1;
  for i:=0 to nCount do
  begin
    nList.Add(nPCtrl.Controls[i]);
    if nPCtrl.Controls[i] is TWinControl then
      EnumSubCtrlList(nPCtrl.Controls[i] as TWinControl, nList);
    //enum sub ctrls
  end;
end;
{$ENDIF}

//Date: 2018-10-31
//Parm: 窗体;文件对象;文件名
//Desc: 从nIniF或nFile中载入nForm的配置信息
class procedure TApplicationHelper.LoadFormConfig(const nForm: TForm;
  const nIniF: TIniFile; const nFile: string);
var nStr: string;
    nIni: TIniFile;
    nValue,nMax: integer;
begin
  if not Assigned(nIniF) then
  begin
    if nFile = '' then
         nStr := gFormConfig
    else nStr := nFile;

    if nStr = '' then
      raise Exception.Create('ULibFun.LoadFormConfig: Invalidate ConfigFile!');
    nIni := TIniFile.Create(nStr);
  end else nIni := nIniF;

  try
    {$IFDEF HasFMX}
    with nForm do
    begin
      nMax := High(integer);
      nValue := nIni.ReadInteger(Name, 'FormTop', nMax);

      if nValue < nMax then
      begin
        Top := nValue;
      end else
      begin
        if Position = TFormPosition.Designed then
          Position := TFormPosition.DesktopCenter;
        //初次加载时居中,避免设计时分辨率不同越界
      end;

      nValue := nIni.ReadInteger(Name, 'FormLeft', nMax);
      if nValue < nMax then Left := nValue;

      if BorderStyle = BorderStyle.bsSizeable then
      begin
        nValue := nIni.ReadInteger(Name, 'FormWidth', nMax);
        if nValue < nMax then Width := nValue;

        nValue := nIni.ReadInteger(Name, 'FormHeight', nMax);
        if nValue < nMax then Height := nValue;
      end; //载入窗体位置和宽高

      if nIni.ReadBool(Name, 'Maximized', False) = True then
         WindowState := TWindowState.wsMaximized;
      //最大化状态
    end;
    //--------------------------------------------------------------------------
    {$ELSE}
    with nForm do
    begin
      nMax := High(integer);
      nValue := nIni.ReadInteger(Name, 'FormTop', nMax);

      if nValue < nMax then
      begin
        Top := nValue;
      end else
      begin
        if (Position = poDesigned) or (Position = poDefaultPosOnly) then
          Position := poDesktopCenter;
        //初次加载时居中,避免设计时分辨率不同越界
      end;

      nValue := nIni.ReadInteger(Name, 'FormLeft', nMax);
      if nValue < nMax then Left := nValue;

      if BorderStyle = bsSizeable then
      begin
        nValue := nIni.ReadInteger(Name, 'FormWidth', nMax);
        if nValue < nMax then Width := nValue;

        nValue := nIni.ReadInteger(Name, 'FormHeight', nMax);
        if nValue < nMax then Height := nValue;
      end; //载入窗体位置和宽高

      if nIni.ReadBool(Name, 'Maximized', False) = True then
         WindowState := wsMaximized;
      //最大化状态
    end;
    {$ENDIF}
  finally
    if not Assigned(nIniF) then nIni.Free;
  end;
end;

class procedure TApplicationHelper.SaveFormConfig(const nForm: TForm;
  const nIniF: TIniFile; const nFile: string);
var nStr: string;
    nIni: TIniFile;
    nBool: Boolean;
begin
  if not Assigned(nIniF) then
  begin
    if nFile = '' then
         nStr := gFormConfig
    else nStr := nFile;

    if nStr = '' then
      raise Exception.Create('ULibFun.SaveFormConfig: Invalidate ConfigFile!');
    nIni := TIniFile.Create(nStr);
  end else nIni := nIniF;

  nBool := False;
  try
    with nForm do
    begin
      {$IFDEF HasFMX}
      nBool := WindowState = TWindowState.wsMaximized;
      {$ELSE}
      nBool := WindowState = wsMaximized;
      {$ENDIF}

      if nBool then
      begin
        {$IFDEF HasVCL}
        LockWindowUpdate(nForm.Handle);
        {$ENDIF}

        {$IFDEF HasFMX}
        WindowState := TWindowState.wsNormal;
        {$ELSE}
        WindowState := wsNormal;
        {$ENDIF}
        //还原,记录正常位置宽高
      end;

      nIni.WriteInteger(Name, 'FormTop', Top);
      nIni.WriteInteger(Name, 'FormLeft', Left);
      nIni.WriteInteger(Name, 'FormWidth', Width);
      nIni.WriteInteger(Name, 'FormHeight', Height);
      nIni.WriteBool(Name, 'Maximized', nBool);
      //保存窗体位置和宽高
    end;
  finally
    if nBool then
    begin
      {$IFDEF HasFMX}
      nForm.WindowState := TWindowState.wsMaximized;
      {$ELSE}
      nForm.WindowState := wsMaximized;
      {$ENDIF}

      {$IFDEF HasVCL}
      LockWindowUpdate(0);
      {$ENDIF}
    end;

    if not Assigned(nIniF) then
      nIni.Free;
    //xxxxx
  end;
end;

//Date: 2021-04-15
//Parm: 文件路径
//Desc: 替换nPath中的全局路径
class function TApplicationHelper.ReplaceGlobalPath(const nPath: string): string;
begin
  Result := TStringHelper.CopyRight(gPath, 1);
  if (Result = '\') or (Result = '/') then
       Result := TStringHelper.CopyNoRight(gPath, 1)
  else Result := gPath;

  Result := StringReplace(nPath, '$Path', Result, [rfReplaceAll, rfIgnoreCase]);
end;

//Date: 2021-04-15
//Parm: 配置参数;文件对象
//Desc: 加载基础配置参数到nParam中
class procedure TApplicationHelper.LoadParameters(var nParam: TAppParam;
  nIni: TIniFile; const nExtend: Boolean);
const sMain = 'Config';
var nStr: string;
    nIdx: Integer;
    nBool: Boolean;
    nList: TStrings;
begin
  nList := nil;
  nBool := Assigned(nIni);

  if not nBool then
    nIni := TIniFile.Create(gSysConfig);
  //xxxxx

  with nParam,nIni do
  try
    FGroupID := ReadString(sMain, 'GroupID', 'group');       //集团代码
    FFactory := ReadString(sMain, 'FactoryID', 'factory');   //工厂代码
    nStr     := ReadString(sMain, 'ProgID', 'runsoft');      //系统代码

    if Trim(nStr) = '' then
      raise Exception.Create('ULibFun.LoadParameters: Invalid "ProgID" Config');
    //xxxxx

    FAdminKey := ReadString(sMain, sVerifyIgnore + 'AdminKey', ''); //管理员密钥
    if FAdminKey = '' then
         FAdminKey := sDefaultKey
    else FAdminKey := TEncodeHelper.Decode_3DES(FAdminKey, sDefaultKey);

    nList := TStringList.Create;
    TStringHelper.Split(nStr, nList, ',', tpTrim);           //id,id,id
    SetLength(FPrograms, nList.Count);

    FActive.FProgram := '';
    nStr := ReadString(sMain, 'Active', '');                 //默认系统代码

    for nIdx := 0 to nList.Count - 1 do
    with FPrograms[nIdx] do
    begin
      FProgram    := nList[nIdx];
      FTitleApp   := ReadString(FProgram, 'AppTitle', 'application');
      FTitleMain  := ReadString(FProgram, 'MainTitle', 'main');
      FCopyRight  := ReadString(FProgram, 'CopyRight', 'copyright');

      FDeployName := ReadString(FProgram, 'DeployName', 'deploy');
      FDeployType := TStringHelper.Str2Enum<TDeployType>(
                     ReadString(FProgram, sVerifyIgnore + 'DeployType', ''));
      //xxxxx

      //for server mode
      FPort       := ReadInteger(FProgram, 'ServerPort', 8080);
      FFavicon    := ReplaceGlobalPath(ReadString(FProgram, 'Favicon', ''));

      if CompareText(FProgram, nStr) = 0 then
        FActive := FPrograms[nIdx];
      //set default
    end;

    if FActive.FProgram = '' then
      FActive := FPrograms[0];
    //first as default
  finally
    nList.Free;
    if not nBool then nIni.Free;
  end;

  if nBool then
       nStr := nIni.FileName
  else nStr := gSysConfig;

  if not IsValidConfigFile(nStr, nParam.FActive.FProgram) then
    raise Exception.Create('ULibFun.LoadParameters: Invalid Config File.');
  //invalid config

  if nExtend then
  begin
    {$IFDEF MSWin}
    with nParam do
    begin
      FLocalMAC := MakeActionID_MAC();
      GetLocalIPConfig(FLocalName, FLocalIP);
    end;
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------
//Date: 2020-06-24
//Parm: 字符串;待复制大小
//Desc: 复制nStr左侧nSize字符
class function TStringHelper.CopyLeft(const nStr: string; nSize: Word): string;
var nLen: Integer;
begin
  nLen := Length(nStr);
  if nSize > nLen then
    nSize := nLen;
  Result := Copy(nStr, cFI, nSize);
end;

//Date: 2020-06-24
//Parm: 字符串;复制时去除
//Desc: 复制nStr左侧去除nSize字符
class function TStringHelper.CopyNoLeft(const nStr: string; nSize: Word): string;
var nLen: Integer;
begin
  nLen := Length(nStr);
  if nSize > nLen then
    nSize := nLen;
  Result := Copy(nStr, nSize + cFI, nLen - nSize);
end;

//Date: 2020-06-24
//Parm: 字符串;待复制大小
//Desc: 复制nStr右侧nSize字符
class function TStringHelper.CopyRight(const nStr: string; nSize: Word): string;
var nLen: Integer;
begin
  nLen := Length(nStr);
  if nSize > nLen then
    nSize := nLen;
  Result := Copy(nStr, nLen - nSize + cFI, nSize);
end;

//Date: 2020-06-24
//Parm: 字符串;复制时去除
//Desc: 复制nStr右侧去除nSize字符
class function TStringHelper.CopyNoRight(const nStr: string; nSize: Word): string;
var nLen: Integer;
begin
  nLen := Length(nStr);
  if nSize > nLen then
    nSize := nLen;
  Result := Copy(nStr, cFI, nLen - nSize);
end;

//Desc: 宏定义项
class function TStringHelper.MI(const nMacro,nValue: string): TMacroItem;
begin
  Result.FMacro := nMacro;
  Result.FValue := nValue;
end;

//Date: 2008-8-8
//Parm: 带宏的字符串;宏内容
//Desc: 依据nMacro的数据,替换nData中所有的宏定义
class function TStringHelper.MacroValue(const nData: string;
  const nMacro: TDynamicMacroArray): string;
var nIdx,nLen: integer;
begin
  Result := nData;
  nLen := High(nMacro);

  for nIdx:=Low(nMacro) to nLen do
  begin
    Result := StringReplace(Result, nMacro[nIdx].FMacro,
                            nMacro[nIdx].FValue, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

//Date: 2021-05-06
//Parm: 字段内容;索引
//Desc: 使用nData的nIdx项内容
class function TStringHelper.MS(const nData: array of string;
  const nIdx: Integer): string;
begin
  if (nIdx > High(nData)) or (nIdx < Low(nData)) then
    raise Exception.Create('TStringHelper.MS: Data out of range.');
  Result := nData[nIdx];
end;

//Date: 2021-05-06
//Parm: 字段内容;选项
//Desc: 依据nBool使用nData的内容
class function TStringHelper.MS(const nData: array of string;
  const nFirst: Boolean): string;
begin
    if Length(nData) < 2 then
    raise Exception.Create('TStringHelper.MS: Data out of range.');
  //xxxxx

  if nFirst then
       Result := nData[0]
  else Result := nData[1];
end;

//Date: 2018-05-03
//Parm: 字符串;列表;分段位置;分隔符
//Desc: 检索使用nFlag分割的nList列表的nSection中是否有nStr字符串
class function TStringHelper.StrListIndex(const nStr: string;
  const nList: TStrings; const nSection: Integer; const nFlag: string): Integer;
var nTmp: TStrings;
    nIdx: Integer;
begin
  nTmp := nil;
  try
    Result := -1;
    nTmp := TStringList.Create;

    for nIdx := nList.Count-1 downto 0 do
    begin
      Split(nList[nIdx], nTmp, nFlag);
      if (nTmp.Count > nSection) and 
         (CompareText(nStr, nTmp[nSection]) = 0) then
      begin
        Result := nIdx;
        Exit;
      end;      
    end;
  finally
    nTmp.Free;
  end;
end;

//Date: 2018-05-03
//Parm: 字符串;数组;忽略大小写
//Desc: 检索nStr在nArray中的索引位置
class function TStringHelper.StrArrayIndex(const nStr: string;
  const nArray: TStringArray; const nIgnoreCase: Boolean): integer;
var nIdx: integer;
    nRes: Boolean;
begin
  Result := -1;
  for nIdx:=Low(nArray) to High(nArray) do
  begin
    if nIgnoreCase then
         nRes := CompareText(nStr, nArray[nIdx]) = 0
    else nRes := nStr = nArray[nIdx];

    if nRes then
    begin
      Result := nIdx;
      Exit;
    end;
  end;
end;

//Date: 2017-03-17
//Parm: 字符串数组;分隔符; 
//Desc: 使用nFlag将nStrArray合并为字符串
class function TStringHelper.Combine(const nStrArray: array of string;
  nFlag: string; const nFlagEnd: Boolean): string;
var nIdx,nLen: integer;
begin
  if nFlag = '' then
    nFlag := ';';
  //def flag

  Result := '';
  nLen := High(nStrArray);

  for nIdx:=Low(nStrArray) to nLen do
   if (nIdx <> nLen) or nFlagEnd then
        Result := Result + nStrArray[nIdx] + nFlag
   else Result := Result + nStrArray[nIdx];
end;

//Date: 2017-03-17
//Parm: 字符串列表;分隔符;
//Desc: 使用nFlag将nList合并为字符串
class function TStringHelper.Combine(const nList: TStrings; nFlag: string;
  const nFlagEnd: Boolean): string;
var nIdx,nCount: integer;
begin
  if nFlag = '' then
    nFlag := ';';
  //def flag

  Result := '';
  nCount := nList.Count - 1;

  for nIdx:=0 to nCount do
   if (nIdx <> nCount) or nFlagEnd then
        Result := Result + nList[nIdx] + nFlag
   else Result := Result + nList[nIdx];
end;

//Date: 2017-03-17
//Parm: 字符串;结果列表;分隔符;结果个数
//Desc: 使用nFlag将nStr拆分,结果存入nList.
class function TStringHelper.Split(const nStr: string; const nList: TStrings;
  nFlag: string; const nTrim: TTrimPos;
  const nNum: Word; const nFlagEnd: Boolean): Boolean;
var nTxt: string;
    nPos,nNow,nLen: integer;

    procedure TrimStr;
    begin
      case nTrim of
       tpLTrim : nTxt := TrimLeft(nTxt);
       tpRTrim : nTxt := TrimRight(nTxt);
       tpTrim  : nTxt := Trim(nTxt);
      end;

      nList.Add(nTxt);
    end;
begin
  if nFlag = '' then
    nFlag := ';';
  //def flag

  nList.Clear;
  nlen := Length(nFlag);
  nPos := Pos(nFlag, nStr, cFI);

  nNow := 1;
  while nPos > 0 do
  begin
    nTxt := Copy(nStr, nNow, nPos - nNow);
    TrimStr;

    nNow := nPos + nLen;
    nPos := Pos(nFlag, nStr, nNow);
  end;

  nLen := Length(nStr);
  if nNow <= nLen then
  begin
    nTxt := Copy(nStr, nNow, nLen - nNow + 1);
    TrimStr;
  end;

  if (not nFlagEnd) and (nNow = nLen + 1) then
  begin
    nLen := Length(nFlag);
    if Copy(nStr, nNow - nLen, nLen) = nFlag then
      nList.Add('');
    //if nStr not end by flag,but the end is flag,append blank
  end; 

  if nNum > 0 then
       Result := nList.Count = nNum
  else Result := nList.Count > 0;
end;

//Date: 2021-05-22
//Parm: 字符串;数组;分隔符;结果个数
//Desc: 使用nFlag将nStr拆分,结果存入nResult.
class function TStringHelper.SplitArray(const nStr: string;
  var nArray: TStringArray; nFlag: string; const nTrim: TTrimPos;
  const nNum: Word; const nFlagEnd: Boolean): Boolean;
var nTxt: string;
    nPos,nNow,nLen,nIdx: integer;

    procedure TrimStr;
    begin
      case nTrim of
       tpLTrim : nTxt := TrimLeft(nTxt);
       tpRTrim : nTxt := TrimRight(nTxt);
       tpTrim  : nTxt := Trim(nTxt);
      end;

      nIdx := Length(nArray);
      SetLength(nArray, nIdx + 1);
      nArray[nIdx] := nTxt;
    end;
begin
  if nFlag = '' then
    nFlag := ';';
  //def flag

  SetLength(nArray, 0);
  nlen := Length(nFlag);
  nPos := Pos(nFlag, nStr, cFI);

  nNow := 1;
  while nPos > 0 do
  begin
    nTxt := Copy(nStr, nNow, nPos - nNow);
    TrimStr;

    nNow := nPos + nLen;
    nPos := Pos(nFlag, nStr, nNow);
  end;

  nLen := Length(nStr);
  if nNow <= nLen then
  begin
    nTxt := Copy(nStr, nNow, nLen - nNow + 1);
    TrimStr;
  end;

  if (not nFlagEnd) and (nNow = nLen + 1) then
  begin
    nLen := Length(nFlag);
    if Copy(nStr, nNow - nLen, nLen) = nFlag then
    begin
      nTxt := '';
      TrimStr;
    end; //if nStr not end by flag,but the end is flag,append blank
  end;

  if nNum > 0 then
       Result := Length(nArray) = nNum
  else Result := Length(nArray) > 0;
end;

//Date: 2017-03-17
//Parm: 字符串;列表
//Desc: 解析nStr中的数值,存入nList中
class function TStringHelper.SplitValue(const nStr: string;
  const nList: TStrings): Boolean;
var nVal: string;
    nIdx,nLen: integer;
begin
  nList.Clear;
  nVal := '';
  nLen := Length(nStr);

  for nIdx:=1 to nLen do
  begin
    if CharInSet(nStr[nIdx], ['0'..'9', '.', '-']) then
    begin
      nVal := nVal + nStr[nIdx];
    end else

    if nVal <> '' then
    begin
      if IsNumber(nVal, True) then
        nList.Add(nVal);
      nVal := '';
    end;
  end;

  if (nVal <> '') and IsNumber(nVal, True) then
    nList.Add(nVal);
  Result := nList.Count > 0;
end;

//Date: 2017-03-17
//Parm: 字符串;默认值
//Desc: 解析nStr中的整型数值
class function TStringHelper.SplitInt(const nStr: string;
  const nDef: Integer): Integer;
var nVal: string;
    nIdx,nLen: integer;
begin
  Result := nDef;
  nVal := '';
  nLen := Length(nStr);

  for nIdx:=1 to nLen do
  begin
    if CharInSet(nStr[nIdx], ['0'..'9', '.', '-']) then
    begin
      nVal := nVal + nStr[nIdx];
    end else

    if nVal <> '' then    
    begin
      if IsNumber(nVal, False) then 
        Break;
      nVal := '';
    end;
  end;

  if IsNumber(nVal, False) then
    Result := StrToInt(nVal);
  //xxxxx
end;

//Date: 2017-03-17
//Parm: 字符串;默认值
//Desc: 解析nStr中的浮点数值
class function TStringHelper.SplitFloat(const nStr: string;
  const nDef: Double): Double;
var nVal: string;
    nIdx,nLen: integer;
begin
  Result := nDef;
  nVal := '';
  nLen := Length(nStr);

  for nIdx:=1 to nLen do
  begin
    if CharInSet(nStr[nIdx], ['0'..'9', '.', '-']) then
    begin
      nVal := nVal + nStr[nIdx];
    end else

    if nVal <> '' then    
    begin
      if IsNumber(nVal, True) then 
        Break;
      nVal := '';
    end;
  end;

  if IsNumber(nVal, True) then
    Result := StrToFloat(nVal);
  //xxxxx
end;

//Date: 2014-09-17
//Parm: 内容;符号;是否添加;分隔符
//Desc: 在nList的所有项前后,添加或删除nSymbol符号
class function TStringHelper.AdjustFormat(const nItems, nSymbol: string;
  const nAdd: Boolean; nFlag: string; const nFlagEnd: Boolean): string;
var nList: TStrings;
begin
  nList := TStringList.Create;
  try
    if nFlag = '' then nFlag := ';';
    Split(nItems, nList, nFlag, tpNo, 0, nFlagEnd);
    Result := AdjustFormat(nList, nSymbol, nAdd, nFlag, False);
  finally
    nList.Free;
  end;
end;

//Date: 2014-09-17
//Parm: 内容;符号;是否添加;分隔符
//Desc: 在nList的所有项前后,添加或删除nSymbol符号
class function TStringHelper.AdjustFormat(const nList: TStrings;
  const nSymbol: string; const nAdd: Boolean; nFlag: string; const nFlagEnd,
  nListYet: Boolean): string;
var nStr,nBak: string;
    nIdx,nLen,nSLen: Integer;
begin
  if nFlag = '' then
     nFlag := ';';
  nSLen := Length(nSymbol);

  if not nListYet then
    nBak := nList.Text;
  //备份内容

  for nIdx:=0 to nList.Count - 1 do
  begin
    nStr := nList[nIdx];
    nLen := Length(nStr);

    if nAdd then
    begin
      if Copy(nStr, 1, nSLen) <> nSymbol then
        nStr := nSymbol + nStr;
      //xxxxx

      if Copy(nStr, nLen - nSLen + 1, nSLen) <> nSymbol then
        nStr := nStr + nSymbol;
      //xxxxx
    end else
    begin
      if Copy(nStr, 1, nSLen) = nSymbol then
        nStr := Copy(nStr, nSLen + 1, nLen - nSLen);
      //xxxxx

      if Copy(nStr, nLen - nSLen + 1, nSLen) = nSymbol then
        nStr := Copy(nStr, 1, nLen - nSLen);
      //xxxxx
    end;

    nList[nIdx] := nStr;
    //change
  end;

  Result := Combine(nList, nFlag, nFlagEnd);
  //合并

  if not nListYet then
    nList.Text := nBak;
  //还原内容
end;

//Date: 2017-03-17
//Parm: 字符串;长度;填充方式;填充符;是否裁剪;单字节长度
//Desc: 使用nFixChar填充nStr,使其保持nWidth定长.
class function TStringHelper.FixWidth(const nStr: string; const nWidth: Word;
const nStyle: TFillPos; const nFixChar: Char; const nClip,nAnsiLen: Boolean): string;
var nAnsi: AnsiString;
    nLen,nInt: Integer;
begin
  if nAnsiLen then
  begin
    {$IFDEF EnableThirdANSI}
    nAnsi := AnsiString.Create(nStr, TEncoding.ANSI);
    nLen := nAnsi.Length;
    {$ELSE}
    nAnsi := AnsiString(nStr);
    nLen := Length(nAnsi);
    {$ENDIF}
  end else
  begin
    nLen := Length(nStr);
    //wide string
  end;

  if nLen >= nWidth then
  begin
    if nClip and (nLen > nWidth) then
    begin
      if nAnsiLen then
      begin
        {$IFDEF EnableThirdANSI}
        Result := string(AnsiCopy(nAnsi, cFI, nWidth));
        {$ELSE}
        Result := string(Copy(nAnsi, cFI, nWidth));
        {$ENDIF}

        nInt := Length(Result);
        if Result[nInt] <> nStr[nInt] then
          Result[nInt] := nStr[nInt];
        //restore dword charactor
      end else
      begin
        Result := Copy(nStr, cFI, nWidth);
      end;
    end else
    begin
      Result := nStr;
      //default
    end;

    Exit;
  end;

  nLen := nWidth - nLen;
  //not enough length

  case nStyle of
   fpLeft:
    begin
      Result := StringOfChar(nFixChar, nLen) + nStr;
    end;
   fpMid:
    begin 
      nInt := Trunc(nLen / 2);
      Result := StringOfChar(nFixChar, nInt) + nStr +
                StringOfChar(nFixChar, nLen - nInt);
    end;
   fpRight: 
    begin
      Result := nStr + StringOfChar(nFixChar, nLen);      
    end;
  end;
end;

//Date: 2017-03-17
//Parm: 字符;是否浮点
//Desc: 判断nStr是否为数值
class function TStringHelper.IsNumber(const nStr: string;
  const nFloat: Boolean): Boolean;
begin
  Result := False;
  if nStr <> '' then  
  try
    if nFloat then
         StrToFloat(nStr)
    else StrToInt64(nStr);
    Result := True;
  except
    //ignor any error
  end;
end;

//Date: 2019-06-03
//Parm: 字符;字符串;重复有效
//Desc: 统计nChar在nStr中出现的次数,重复出现只统计一次.
class function TStringHelper.CountChar(const nChar: Char; const nStr: string;
  const nRepeatValid: Boolean): Cardinal;
var nIdx,nInt: Integer;
    nPre: Char;
begin
  Result := 0;
  nInt := High(nStr);
  if nInt < cFI then Exit;

  nPre := nStr[cFI];
  if nPre = nChar then
    Inc(Result);
  //xxxxx

  for nIdx := cFI + 1 to nInt do
  begin
    if nStr[nIdx] = nChar then
    begin
      if (nStr[nIdx] <> nPre) or nRepeatValid then
        Inc(Result);
      //xxxxx
    end;

    nPre := nStr[nIdx];
    //current char
  end;
end;

//Date: 2019-06-03
//Parm: 以空格分隔的十六进制字符串
//Desc: 使用nHex构建字符串
class function TStringHelper.ByHex(const nHex: string): string;
const cChars = [#32, #13, #10]; //spit char
var nStr: string;
    nIdx,nHi,nPos: Integer;
begin
  Result := '';
  nHi := High(nHex);
  if nHi < cFI then Exit;

  nPos := cFI;
  for nIdx:=cFI to nHi do
  begin
    if not CharInSet(nHex[nIdx], ['0'..'9','a'..'f', 'A'..'F'] + cChars) then
    begin
      nPos := nIdx + 1;
      Continue;
    end;

    if CharInSet(nHex[nIdx], cChars) or (nIdx = nHi) then
    begin
      if not CharInSet(nHex[nPos], cChars) then
      begin
        if CharInSet(nHex[nIdx], cChars) then
             nStr := Copy(nHex, nPos, nIdx - nPos)
        else nStr := Copy(nHex, nPos, nIdx - nPos + 1);

        if Length(nStr) = 2 then
          Result := Result + Char(StrToInt('$' + nStr));
        //xxxxx
      end;

      nPos := nIdx;
    end else
    begin
      if CharInSet(nHex[nPos], cChars) then
        nPos := nIdx;
      //xxxxx
    end;
  end;
end;

//Date: 2019-05-27
//Parm: 字符串
//Desc: 将nStr转换为十六进制字符串
class function TStringHelper.HexStr(const nStr: string): string;
var nIdx,nLen: Integer;
begin
  Result := '';
  nLen := High(nStr);

  for nIdx := cFI to nLen do
  begin
    Result := Result + IntToHex(Ord(nStr[nIdx]), 2);
    if nIdx < nLen then
      Result := Result + ' ';
    //xxxxx
  end;
end;

//Date: 2019-06-01
//Parm: 字节数组
//Desc: 将nBytes转为十六进制字符串
class function TStringHelper.HexBytes(const nBytes: TBytes): string;
var nIdx,nLen: Integer;
begin
  Result := '';
  nLen := High(nBytes);

  for nIdx := Low(nBytes) to nLen do
  begin
    Result := Result + IntToHex(nBytes[nIdx], 2);
    if nIdx < nLen then
      Result := Result + ' ';
    //xxxxx
  end;
end;

//Date: 2019-06-01
//Parm: 字符串
//Desc: 按单字节将nStr转为字节数组
class function TStringHelper.Str2Bytes(const nStr: string): TBytes;
var nIdx,nInt: Integer;
begin
  nInt := Length(nStr);
  SetLength(Result, nInt);

  if nInt > 0 then
  begin
    nInt := cFI + (0 - Low(Result));
    for nIdx := Low(Result) to High(Result) do
      Result[nIdx] := Ord(nStr[nIdx + nInt]);
    //xxxxx
  end;
end;

//Date: 2019-06-01
//Parm: 字节数组
//Desc: 按单字节将nBytes转为字符串
class function TStringHelper.Bytes2Str(const nBytes: TBytes): string;
var nIdx,nInt: Integer;
begin
  nInt := Length(nBytes);
  SetLength(Result, nInt);

  if nInt > 0 then
  begin
    nInt := cFI + (0 - Low(nBytes));
    for nIdx := Low(nBytes) to High(nBytes) do
      Result[nIdx + nInt] := Char(nBytes[nIdx]);
    //xxxxx
  end;
end;

//Date: 2019-05-25
//Parm: 字段内容;选项
//Desc: 依据nBool使用nData的内容
class function TStringHelper.StrIF(const nData: array of string;
  const nIdx: Integer): string;
begin
  if (nIdx > High(nData)) or (nIdx < Low(nData)) then
    raise Exception.Create('TStringHelper.StrIF: Data out of range.');
  Result := nData[nIdx];
end;

//Date: 2019-05-25
//Parm: 字段内容;选项
//Desc: 依据nBool使用nData的内容
class function TStringHelper.StrIF(const nData: array of string;
  const nFirst: Boolean): string;
begin
  if Length(nData) < 2 then
    raise Exception.Create('TStringHelper.StrIF: Data out of range.');
  //xxxxx

  if nFirst then
       Result := nData[0]
  else Result := nData[1];
end;

//Date: 2019-05-23
//Parm: 枚举值
//Desc: 返回nEnum的字符串描述
class function TStringHelper.Enum2Str<T>(const nEnum: T): string;
var nType: TRttiType;
    nTEnum: TRttiEnumerationType;
begin
  with TRttiContext.Create do
  try
    nType := GetType(TypeInfo(T));
    if not (nType is TRttiEnumerationType) then
      raise Exception.Create('TStringHelper.Enum2Str: Invalid EnumType.');
    //xxxxx

    nTEnum := nType as TRttiEnumerationType;
    Result := nTEnum.GetName(nEnum);
  finally
    Free;
  end;
end;

//Date: 2019-05-23
//Parm: 枚举值描述
//Desc: 返回nEnum描述对应的值
class function TStringHelper.Str2Enum<T>(const nEnum: string): T;
var nStr,nDef: string;
    nType: TRttiType;
    nTEnum: TRttiEnumerationType;
begin
  with TRttiContext.Create do
  try
    nType := GetType(TypeInfo(T));
    if not (nType is TRttiEnumerationType) then
      raise Exception.Create('TStringHelper.Str2Enum: Invalid EnumType.');
    //xxxxx

    nDef := '';
    nTEnum := nType as TRttiEnumerationType;
    
    for nStr in nTEnum.GetNames do
    begin
      if nDef = '' then
        nDef := nStr;
      //default value
    
      if CompareText(nStr, nEnum) = 0 then
      begin
        Result := nTEnum.GetValue<T>(nEnum);
        Exit;
      end;
    end;

    Result := nTEnum.GetValue<T>(nDef);
  finally
    Free;
  end;
end;

//Date: 2021-05-18
//Parm: 集合
//Desc: 返回nSet的字符串描述
class function TStringHelper.Set2Str<T, ST>(const nSet: ST): string;
var nStr: string;
    nIdx: Integer;
    nVal: set of 0..255;
    nNames: TArray<string>;

    nType: TRttiType;
    nTSet: TRttiSetType;
    nTEnum: TRttiEnumerationType;
begin
  with TRttiContext.Create do
  try
    nType := GetType(TypeInfo(T));
    if not (nType is TRttiEnumerationType) then
    begin
      nStr := 'TStringHelper.Set2Str: %s Is Invalid EnumType.';
      nStr := Format(nStr, [nType.Name]);
      raise Exception.Create(nStr);
    end;

    nTEnum := nType as TRttiEnumerationType;
    case nTEnum.OrdType of
     otSByte,otUByte: ;
     else
      begin
        nStr := 'TStringHelper.Set2Str: SizeOf(%s) > 1.';
        nStr := Format(nStr, [nType.Name]);
        raise Exception.Create(nStr);
      end;
    end;

    nType := GetType(TypeInfo(ST));
    if not (nType is TRttiSetType) then
    begin
      nStr := 'TStringHelper.Set2Str: %s Is Invalid SetType.';
      nStr := Format(nStr, [nType.Name]);
      raise Exception.Create(nStr);
    end;

    nTSet := nType as TRttiSetType;
    if (nTSet.ElementType.Handle <> nTEnum.Handle) then
    begin
      nStr := 'TStringHelper.Set2Str: %s No-Match %s Type.';
      nStr := Format(nStr, [nTSet.Name, nTEnum.Name]);
      raise Exception.Create(nStr);
    end;

    if SizeOf(ST) > SizeOf(nVal) then
    begin
      nStr := 'TStringHelper.Set2Str: SizeOf(%s) > 8 byte.';
      nStr := Format(nStr, [nTSet.Name]);
      raise Exception.Create(nStr);
    end;

    FillChar(nVal, SizeOf(nVal), 0);
    Move(nSet, nVal, SizeOf(ST));
    nNames := nTEnum.GetNames();

    Result := '';
    for nIdx := nTEnum.MinValue to nTEnum.MaxValue do
     if nIdx in nVal then
      if Result = '' then
           Result := nNames[nIdx]
      else Result := Result + ',' + nNames[nIdx];
  finally
    Free;
  end;
end;

//Date: 2021-05-18
//Parm: 集合字符串
//Desc: 返回nSet对应的集合
class function TStringHelper.Str2Set<T, ST>(const nSet: string): ST;
var nStr: string;
    nIdx: Integer;
    nList: TStrings;
    nVal: set of 0..255;
    nNames: TArray<string>;

    nType: TRttiType;
    nTSet: TRttiSetType;
    nTEnum: TRttiEnumerationType;
begin
  nList := nil;
  with TRttiContext.Create do
  try
    nType := GetType(TypeInfo(T));
    if not (nType is TRttiEnumerationType) then
    begin
      nStr := 'TStringHelper.Set2Str: %s Is Invalid EnumType.';
      nStr := Format(nStr, [nType.Name]);
      raise Exception.Create(nStr);
    end;

    nTEnum := nType as TRttiEnumerationType;
    case nTEnum.OrdType of
     otSByte,otUByte: ;
     else
      begin
        nStr := 'TStringHelper.Set2Str: SizeOf(%s) > 1.';
        nStr := Format(nStr, [nType.Name]);
        raise Exception.Create(nStr);
      end;
    end;

    nType := GetType(TypeInfo(ST));
    if not (nType is TRttiSetType) then
    begin
      nStr := 'TStringHelper.Set2Str: %s Is Invalid SetType.';
      nStr := Format(nStr, [nType.Name]);
      raise Exception.Create(nStr);
    end;

    nTSet := nType as TRttiSetType;
    if (nTSet.ElementType.Handle <> nTEnum.Handle) then
    begin
      nStr := 'TStringHelper.Set2Str: %s No-Match %s Type.';
      nStr := Format(nStr, [nTSet.Name, nTEnum.Name]);
      raise Exception.Create(nStr);
    end;

    if SizeOf(ST) > SizeOf(nVal) then
    begin
      nStr := 'TStringHelper.Set2Str: SizeOf(%s) > 8 byte.';
      nStr := Format(nStr, [nTSet.Name]);
      raise Exception.Create(nStr);
    end;

    nList := TStringList.Create;
    Split(nSet, nList, ',', tpTrim);
    //get all items

    FillChar(nVal, SizeOf(nVal), 0);
    nNames := nTEnum.GetNames();
    //get all names

    for nIdx := nTEnum.MinValue to nTEnum.MaxValue do
     if nList.IndexOf(nNames[nIdx]) >= 0 then
      System.Include(nVal, nIdx);
    //set values

    Move(nVal, Result, SizeOf(ST));
    //combine data
  finally
    Free;
    nList.Free;
  end;
end;

//Date: 2020-04-21
//Parm: 列表;显示序号
//Desc: 将T的所有项名称存入nList
class procedure TStringHelper.EnumItems<T>(const nList: TStrings;
  const nSerial: Boolean);
var nStr: string;
    nID: Integer;
    nType: TRttiType;
    nTEnum: TRttiEnumerationType;
begin
  with TRttiContext.Create do
  try
    nList.Clear;
    nType := GetType(TypeInfo(T));
    if not (nType is TRttiEnumerationType) then
      raise Exception.Create('TStringHelper.EnumItems: Invalid EnumType.');
    //xxxxx

    nID := 0;
    nTEnum := nType as TRttiEnumerationType;
    for nStr in nTEnum.GetNames do
    begin
      if nSerial then
      begin
        nList.Add(IntToStr(nID) + '.' + nStr);
        Inc(nID);
      end else
      begin
        nList.Add(nStr);
      end;
    end;
  finally
    Free;
  end;
end;

//Date: 2021-06-01
//Parm: 列表;数组
//Desc: 使用nArray填充nList
class procedure TStringHelper.FillList(const nList: TStrings;
  const nArray: array of string; const nClearFirst,nSerial: Boolean);
var nIdx,nID: Integer;
begin
  with nList do
  try
    BeginUpdate;
    if nClearFirst then
      Clear;
    //xxxxx

    if nSerial then
         nID := 1 - Low(nArray) //from 1
    else nID := 0;

    for nIdx := Low(nArray) to High(nArray) do
      if nSerial then
           nList.AddObject(IntToStr(nIdx + nID)+'.'+nArray[nIdx], Pointer(nIdx))
      else nList.AddObject(nArray[nIdx], Pointer(nIdx));
  finally
    EndUpdate;
  end;
end;

//Date: 2017-03-17
//Parm: 字符串
//Desc: 将nStr镜像反转
class function TStringHelper.MirrorStr(const nStr: string): string;
var nIdx,nLen: Integer;
begin
  nLen := High(nStr);
  SetLength(Result, nLen - cFI + 1);

  for nIdx:=cFI to nLen do
    Result[nIdx] := nStr[nLen - nIdx + 1];
  //convert
end;

//Date: 2019-06-06
//Parm: 字符串
//Desc: 将nStr编码为UTF-8并使用Ansi单字节字符串
class function TStringHelper.Ansi_UTF8(const nStr: string): string;
var nPtr: TPtrWrapper;
begin
  nPtr := TPtrWrapper.Create(nil);
  try
    nPtr := TMarshal.AllocStringAsUtf8(nStr);
    Result := TMarshal.ReadStringAsAnsi(nPtr);
  finally
    TMarshal.FreeMem(nPtr);
  end;
end;

//Date: 2019-06-06
//Parm: 字符串
//Desc: 将nStr编码为Unicode并使用Ansi单字节字符串
class function TStringHelper.Ansi_Unicode(const nStr: string): string;
var nPtr: TPtrWrapper;
begin
  nPtr := TPtrWrapper.Create(nil);
  try
    nPtr := TMarshal.AllocStringAsUnicode(nStr);
    Result := TMarshal.ReadStringAsAnsi(nPtr);
  finally
    TMarshal.FreeMem(nPtr);
  end;
end;

//Date: 2019-06-06
//Parm: 字符串
//Desc: 将nStr编码为Unicode并使用UTF-8字符串
class function TStringHelper.UTF8_Unicode(const nStr: string): string;
var nPtr: TPtrWrapper;
begin
  nPtr := TPtrWrapper.Create(nil);
  try
    nPtr := TMarshal.AllocStringAsUnicode(nStr);
    Result := TMarshal.ReadStringAsUtf8(nPtr);
  finally
    TMarshal.FreeMem(nPtr);
  end;
end;

//Date: 2019-06-06
//Parm: 字符串
//Desc: 将nStr编码为Ansi并使用UTF-8字符串
class function TStringHelper.UTF8_Ansi(const nStr: string): string;
var nPtr: TPtrWrapper;
begin
  nPtr := TPtrWrapper.Create(nil);
  try
    nPtr := TMarshal.AllocStringAsAnsi(nStr);
    Result := TMarshal.ReadStringAsUtf8(nPtr);
  finally
    TMarshal.FreeMem(nPtr);
  end;
end;

//Date: 2019-06-06
//Parm: 字符串
//Desc: 将nStr编码为UTF8并使用Unicode字符串
class function TStringHelper.Unicode_UTF8(const nStr: string): string;
var nPtr: TPtrWrapper;
begin
  nPtr := TPtrWrapper.Create(nil);
  try
    nPtr := TMarshal.AllocStringAsUtf8(nStr);
    Result := TMarshal.ReadStringAsUnicode(nPtr);
  finally
    TMarshal.FreeMem(nPtr);
  end;
end;

//Date: 2019-06-06
//Parm: 字符串
//Desc: 将nStr编码为Ansi并使用Unicode字符串
class function TStringHelper.Unicode_Ansi(const nStr: string): string;
var nPtr: TPtrWrapper;
begin
  nPtr := TPtrWrapper.Create(nil);
  try
    nPtr := TMarshal.AllocStringAsAnsi(nStr);
    Result := TMarshal.ReadStringAsUnicode(nPtr);
  finally
    TMarshal.FreeMem(nPtr);
  end;
end;

//------------------------------------------------------------------------------
{$IFNDEF EnableThirdANSI}
const
  cPYData: array[216..247] of AnsiString = (
  {216}'CJWGNSPGCGNESYPB' + 'TYYZDXYKYGTDJNMJ' + 'QMBSGZSCYJSYYZPG' +
  {216}'KBZGYCYWYKGKLJSW' + 'KPJQHYZWDDZLSGMR' + 'YPYWWCCKZNKYDG',
  {217}'TTNJJEYKKZYTCJNM' + 'CYLQLYPYQFQRPZSL' + 'WBTGKJFYXJWZLTBN' +
  {217}'CXJJJJZXDTTSQZYC' + 'DXXHGCKBPHFFSSYY' + 'BGMXLPBYLLLHLX',
  {218}'SPZMYJHSOJNGHDZQ' + 'YKLGJHXGQZHXQGKE' + 'ZZWYSCSCJXYEYXAD' +
  {218}'ZPMDSSMZJZQJYZCD' + 'JEWQJBDZBXGZNZCP' + 'WHKXHQKMWFBPBY',
  {219}'DTJZZKQHYLYGXFPT' + 'YJYYZPSZLFCHMQSH' + 'GMXXSXJJSDCSBBQB' +
  {219}'EFSJYHXWGZKPYLQB' + 'GLDLCCTNMAYDDKSS' + 'NGYCSGXLYZAYBN',
  {220}'PTSDKDYLHGYMYLCX' + 'PYCJNDQJWXQXFYYF' + 'JLEJBZRXCCQWQQSB' +
  {220}'ZKYMGPLBMJRQCFLN' + 'YMYQMSQYRBCJTHZT' + 'QFRXQHXMJJCJLX',
  {221}'QGJMSHZKBSWYEMYL' + 'TXFSYDSGLYCJQXSJ' + 'NQBSCTYHBFTDCYZD' +
  {221}'JWYGHQFRXWCKQKXE' + 'BPTLPXJZSRMEBWHJ' + 'LBJSLYYSMDXLCL',
  {222}'QKXLHXJRZJMFQHXH' + 'WYWSBHTRXXGLHQHF' + 'NMCYKLDYXZPWLGGS' +
  {222}'MTCFPAJJZYLJTYAN' + 'JGBJPLQGDZYQYAXB' + 'KYSECJSZNSLYZH',
  {223}'ZXLZCGHPXZHZNYTD' + 'SBCJKDLZAYFMYDLE' + 'BBGQYZKXGLDNDNYS' +
  {223}'KJSHDLYXBCGHXYPK' + 'DQMMZNGMMCLGWZSZ' + 'XZJFZNMLZZTHCS',
  {224}'YDBDLLSCDDNLKJYK' + 'JSYCJLKOHQASDKNH' + 'CSGANHDAASHTCPLC' +
  {224}'PQYBSDMPJLPCJOQL' + 'CDHJJYSPRCHNKNNL' + 'HLYYQYHWZPTCZG',
  {225}'WWMZFFJQQQQYXACL' + 'BHKDJXDGMMYDJXZL' + 'LSYGXGKJRYWZWYCL' +
  {225}'ZMSSJZLDBYDCPCXY' + 'HLXCHYZJQSQQAGMN' + 'YXPFRKSSBJLYXY',
  {226}'SYGLNSCMHCWWMNZJ' + 'JLXXHCHSYD CTXRY' + 'CYXBYHCSMXJSZNPW' +
  {226}'GPXXTAYBGAJCXLYS' + 'DCCWZOCWKCCSBNHC' + 'PDYZNFCYYTYCKX',
  {227}'KYBSQKKYTQQXFCWC' + 'HCYKELZQBSQYJQCC' + 'LMTHSYWHMKTLKJLY' +
  {227}'CXWHEQQHTQHZPQSQ' + 'SCFYMMDMGBWHWLGS' + 'LLYSDLMLXPTHMJ',
  {228}'HWLJZYHZJXHTXJLH' + 'XRSWLWZJCBXMHZQX' + 'SDZPMGFCSGLSXYMJ' +
  {228}'SHXPJXWMYQKSMYPL' + 'RTHBXFTPMHYXLCHL' + 'HLZYLXGSSSSTCL',
  {229}'SLDCLRPBHZHXYYFH' + 'BBGDMYCNQQWLQHJJ' + 'ZYWJZYEJJDHPBLQX' +
  {229}'TQKWHLCHQXAGTLXL' + 'JXMSLXHTZKZJECXJ' + 'CJNMFBYCSFYWYB',
  {230}'JZGNYSDZSQYRSLJP' + 'CLPWXSDWEJBJCBCN' + 'AYTWGMPABCLYQPCL' +
  {230}'ZXSBNMSGGFNZJJBZ' + 'SFZYNDXHPLQKZCZW' + 'ALSBCCJXJYZHWK',
  {231}'YPSGXFZFCDKHJGXD' + 'LQFSGDSLQWZKXTMH' + 'SBGZMJZRGLYJBPML' +
  {231}'MSXLZJQQHZSJCZYD' + 'JWBMJKLDDPMJEGXY' + 'HYLXHLQYQHKYCW',
  {232}'CJMYYXNATJHYCCXZ' + 'PCQLBZWWYTWBQCML' + 'PMYRJCCCXFPZNZZL' +
  {232}'JPLXXYZTZLGDLDCK' + 'LYRLZGQTGJHHGJLJ' + 'AXFGFJZSLCFDQZ',
  {233}'LCLGJDJCSNCLLJPJ' + 'QDCCLCJXMYZFTSXG' + 'CGSBRZXJQQCTZHGY' +
  {233}'QTJQQLZXJYLYLBCY' + 'AMCSTYLPDJBYREGK' + 'JZYZHLYSZQLZNW',
  {234}'CZCLLWJQJJJKDGJZ' + 'OLBBZPPGLGHTGZXY' + 'GHZMYCNQSYCYHBHG' +
  {234}'XKAMTXYXNBSKYZZG' + 'JZLQJDFCJXDYGJQJ' + 'JPMGWGJJJPKQSB',
  {235}'GBMMCJSSCLPQPDXC' + 'DYYKYWCJDDYYGYWR' + 'HJRTGZNYQLDKLJSZ' +
  {235}'ZGZQZJGDYKSHPZMT' + 'LCPWNJAFYZDJCNMW' + 'ESCYGLBTZCGMSS',
  {236}'LLYXQSXSBSJSBBGG' + 'GHFJLYPMZJNLYYWD' + 'QSHZXTYYWHMCYHYW' +
  {236}'DBXBTLMSYYYFSXJC' + 'SDXXLHJHF SXZQHF' + 'ZMZCZTQCXZXRTT',
  {237}'DJHNNYZQQMNQDMMG' + 'LYDXMJGDHCDYZBFF' + 'ALLZTDLTFXMXQZDN' +
  {237}'GWQDBDCZJDXBZGSQ' + 'QDDJCMBKZFFXMKDM' + 'DSYYSZCMLJDSYN',
  {238}'SPRSKMKMPCKLGDBQ' + 'TFZSWTFGGLYPLLJZ' + 'HGJJGYPZLTCSMCNB' +
  {238}'TJBQFKTHBYZGKPBB' + 'YMTDSSXTBNPDKLEY' + 'CJNYCDYKZDDHQH',
  {239}'SDZSCTARLLTKZLGE' + 'CLLKJLQJAQNBDKKG' + 'HPJTZQKSECSHALQF' +
  {239}'MMGJNLYJBBTMLYZX' + 'DCJPLDLPCQDHZYCB' + 'ZSCZBZMSLJFLKR',
  {240}'ZJSNFRGJHXPDHYJY' + 'BZGDLJCSEZGXLBLH' + 'YXTWMABCHECMWYJY' +
  {240}'ZLLJJYHLGBDJLSLY' + 'GKDZPZXJYYZLWCXS' + 'ZFGWYYDLYHCLJS',
  {241}'CMBJHBLYZLYCBLYD' + 'PDQYSXQZBYTDKYYJ' + 'YYCNRJMPDJGKLCLJ' +
  {241}'BCTBJDDBBLBLCZQR' + 'PPXJCGLZCSHLTOLJ' + 'NMDDDLNGKAQHQH',
  {242}'JHYKHEZNMSHRP QQ' + 'JCHGMFPRXHJGDYCH' + 'GHLYRZQLCYQJNZSQ' +
  {242}'TKQJYMSZSWLCFQQQ' + 'XYFGGYPTQWLMCRNF' + 'KKFSYYLQBMQAMM',
  {243}'MYXCTPSHCPTXXZZS' + 'MPHPSHMCLMLDQFYQ' + 'XSZYJDJJZZHQPDSZ' +
  {243}'GLSTJBCKBXYQZJSG' + 'PSXQZQZRQTBDKYXZ' + 'KHHGFLBCSMDLDG',
  {244}'DZDBLZYYCXNNCSYB' + 'ZBFGLZZXSWMSCCMQ' + 'NJQSBDQSJTXXMBLT' +
  {244}'XZCLZSHZCXRQJGJY' + 'LXZFJPHYXZQQYDFQ' + 'JJLZZNZJCDGZYG',
  {245}'CTXMZYSCTLKPHTXH' + 'TLBJXJLXSCDQXCBB' + 'TJFQZFSLTJBTKQBX' +
  {245}'XJJLJCHCZDBZJDCZ' + 'JDCPRNPQCJPFCZLC' + 'LZXBDMXMPHJSGZ',
  {246}'GSZZQLYLWTJPFSYA' + 'SMCJBTZYYCWMYTCS' + 'JJLQCQLWZMALBXYF' +
  {246}'BPNLSFHTGJWEJJXX' + 'GLLJSTGSHJQLZFKC' + 'GNNDSZFDEQFHBS',
  {247}'AQTGYLBXMMYGSZLD' + 'YDQMJJRGBJTKGDHG' + 'KBLQKBDMBYLXWCXY' +
  {247}'TTYBKMRTJZXQJBHL' + 'MHMJJZMQASLDCYXY' + 'QDLQCAFYWYXQHZ'  );

//Desc: 汉字转拼音
function HZ2PY(const nValue: array of AnsiChar): AnsiChar;
begin
  Result := #0;

  case Word(nValue[0]) shl 8 + Word(nValue[1]) of
    $B0A1..$B0C4: Result := 'A';
    $B0C5..$B2C0: Result := 'B';
    $B2C1..$B4ED: Result := 'C';
    $B4EE..$B6E9: Result := 'D';
    $B6EA..$B7A1: Result := 'E';
    $B7A2..$B8C0: Result := 'F';
    $B8C1..$B9FD: Result := 'G';
    $B9FE..$BBF6: Result := 'H';
    $BBF7..$BFA5: Result := 'J';
    $BFA6..$C0AB: Result := 'K';
    $C0AC..$C2E7: Result := 'L';
    $C2E8..$C4C2: Result := 'M';
    $C4C3..$C5B5: Result := 'N';
    $C5B6..$C5BD: Result := 'O';
    $C5BE..$C6D9: Result := 'P';
    $C6DA..$C8BA: Result := 'Q';
    $C8BB..$C8F5: Result := 'R';
    $C8F6..$CBF9: Result := 'S';
    $CBFA..$CDD9: Result := 'T';
    $CDDA..$CEF3: Result := 'W';
    $CEF4..$D1B8: Result := 'X';
    $D1B9..$D4D0: Result := 'Y';
    $D4D1..$D7F9: Result := 'Z';
  end; //一级汉字

  if Result = #0 then
   case Byte(nValue[0]) of
    216..247: Result := cPYData[Byte(nValue[0])][Byte(nValue[1]) - 160];
   end;
end;

//Desc: 拼音有效字符(可见字符)
function PYValidChar(const nChar: AnsiChar): Boolean;
begin
  Result := (Ord(nChar) >= Ord(' ')) and (Ord(nChar) <= Ord('~'));
end;

//Date: 2017-03-17
//Parm: 中文字符串
//Desc: 获取nCH的拼音首字母 
class function TStringHelper.GetPinYin(const nCH: string): string;
var nChar: AnsiChar;
    nStr: AnsiString;
    nIdx,nLen: integer;
    nArray: array[0..1] of AnsiChar;
begin
  Result := '';
  nIdx := 0;
  nLen := Length(nCH); 

  while nIdx < nLen do
  begin
    Inc(nIdx);
    nStr := AnsiString(nCH[nIdx]);
    
    if Length(nStr) < 2 then
    begin
      if PYValidChar(nStr[1]) then
        Result := Result + string(nStr);
      //xxxxx
    end else
    begin
      nArray[0] := nStr[1];
      nArray[1] := nStr[2];

      nChar := HZ2PY(nArray);
      if PYValidChar(nChar) then
        Result := Result + string(nChar);
      //xxxxx
    end;
  end;

  Result := LowerCase(Result);
end;
{$ENDIF}
//------------------------------------------------------------------------------
//Date: 2017-12-19
//Parm: 字段;值;类型
//Desc: 构建MakeSQLByStr所需的数据
class function TSQLBuilder.SF(const nField: string; const nValue: Variant;
  const nType: TFieldType): string;
var nVal: string;
begin
  if nType = sfDateTime then
  begin
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', nValue);
    Result := Format('%s=''%s''', [nField, Result]);
    Exit;
  end;

  nVal := VarToStr(nValue);
  //convert type

  with TDateTimeHelper do
  begin
    case nType of
     sfStr:
      Result := Format('%s=''%s''', [nField, nVal]);
     sfDate:
      Result := Format('%s=''%s''', [nField, Date2Str(Str2DateTime(nVal))]);
     sfTime:
      Result := Format('%s=''%s''', [nField, Time2Str(Str2DateTime(nVal))]);
     sfVal:
      begin
        if nVal = '' then
             Result := Format('%s=%d', [nField, 0])
        else Result := Format('%s=%s', [nField, nVal]);
      end
     else Result := '';
    end;
  end;
end;

//Date: 2017-12-19
//Parm: 字段内容;索引
//Desc: 使用nData的nIdx项内容
class function TSQLBuilder.SF_IF(const nData: array of string;
  const nIdx: Integer): string;
begin
  if (nIdx > High(nData)) or (nIdx < Low(nData)) then
    raise Exception.Create('TSQLBuilder.SF_IF: Data out of range.');
  Result := nData[nIdx];
end;

//Date: 2017-12-19
//Parm: 字段内容;选项
//Desc: 依据nBool使用nData的内容
class function TSQLBuilder.SF_IF(const nData: array of string;
  const nFirst: Boolean): string;
begin
    if Length(nData) < 2 then
    raise Exception.Create('TSQLBuilder.SF_IF: Data out of range.');
  //xxxxx

  if nFirst then
       Result := nData[0]
  else Result := nData[1];
end;

//Date: 2017-12-19
//Parm: 字段内容;表名;条件;Insert or Update
//Desc: 构建SQL语句
class function TSQLBuilder.MakeSQLByStr(const nData: array of string;
  const nTable, nWhere: string; const nIsNew: Boolean): string;
var nIdx,nPos: integer;
    nStr,nTmp,nPrefix,nMain,nPostfix: string;
begin
  Result := '';
  //init

  if nIsNew then
  begin
    nPrefix := 'Insert into ' + nTable + '(';
    nPostfix := '';
  end else
  begin
    nPrefix := 'Update ' + nTable + ' Set ';
    nPostfix := ' Where ' + nWhere;
  end;

  for nIdx := Low(nData) to High(nData) do
  begin
    nPos := Pos('=', nData[nIdx]);
    if nPos < 2 then Continue;
    nStr := nData[nIdx];

    if nIsNew then
    begin
      nTmp := Copy(nStr, 1, nPos - 1);
      System.Delete(nStr, 1, nPos);

      if nMain = '' then
           nMain := nTmp
      else nMain := nMain + ',' + nTmp;

      if nPostfix = '' then
           nPostfix := nStr
      else nPostfix := nPostfix + ',' + nStr;
    end else
    begin
      if nMain = '' then
           nMain := nStr
      else nMain := nMain + ',' + nStr;
    end;
  end;

  if nIsNew then
  begin
    nMain := nMain + ') Values(';
    nPostfix := nPostfix + ')';
  end;

  Result := nPrefix + nMain + nPostfix;
  //full sql
end;

//Date: 2021-05-23
//Parm: 字符串
//Desc: 在nData两侧添加单引号
class function TSQLBuilder.SQM(const nData: string): string;
begin
  Result := '''' + nData + '''';
end;

//------------------------------------------------------------------------------
//Date: 2017-03-17
//Parm: 值;精度;四舍五入
//Desc: 将nValue放大nPrecision,然后取整,小数位四舍五入
class function TFloatHelper.Float2PInt(const nValue: Double;
  const nPrecision: Integer; const nRound: Boolean): Int64;
var nStr,nVal: string;
    nInt: Integer;
begin
  nInt := Length(IntToStr(nPrecision)) - 1;
  //放大倍数(10,100)包含0的个数

  nStr := '#.' + StringOfChar('0', nInt + 2);
  //多补两位,防止函数自动四舍五入

  nStr := FormatFloat(nStr, StrToFloat(FloatToStr(nValue)));
  //防止浮点运算的误差

  if nStr = '' then
  begin
    Result := 0;
    Exit;
  end;

  nVal := Copy(nStr, 1, Length(nStr)-2);
  //去掉多补的两位
  nVal := StringReplace(nVal, '.', '', []);
  //去掉小数点
  Result := StrToInt64(nVal);
  //转为整型

  if nRound then
  begin
    nStr := Copy(nStr, Length(nStr)-1, 2);
    if StrToInt(nStr) < 50 then Exit;

    if Result >= 0 then
         Inc(Result)
    else Dec(Result);
  end;
end;

//Date: 2017-03-17
//Parm: 值;精度;四舍五入
//Desc: 将nValue放大nPrecision,取整,然后换回小数
class function TFloatHelper.Float2Float(const nValue: Double;
  const nPrecision: Integer; const nRound: Boolean): Double;
begin
  Result := Float2PInt(nValue, nPrecision, nRound) / nPrecision;
end;

//Date: 2017-03-17
//Parm: 浮点数A,B;待判定关系;判定精度
//Desc: 按nPrecision精度,判定nA、nB是否满足nType关系
class function TFloatHelper.FloatRelation(const nA, nB: Double;
  const nType: TFloatRelationType; const nPrecision: Integer): Boolean;
var nIA,nIB: Int64;
begin
  Result := False;
  nIA := Float2PInt(nA, nPrecision, False);
  nIB := Float2PInt(nB, nPrecision, False);

  case nType of
   rtGreater: Result := nIA > nIB;
   rtGE: Result := nIA >= nIB;
   rtEqual: Result := nIA = nIB;
   rtLE: Result := nIA <= nIB;
   rtLess: Result := nIA < nIB;
  end;
end;

//------------------------------------------------------------------------------
//Date: 2019-03-20
//Parm: 待查询值;待查询位;nNum位数
//Desc: 查询nNum第nBit位的bit值
class function TBitHelper.GetBit(const nNum: Integer; const nBit: Byte;
  const nBitCount: TBitCount): Byte;
begin
  if nBit > Byte(nBitCount) then
       Result := nNum
  else Result := Byte((nNum shr (nBit - 1)) and 1);
end;

//Date: 2019-03-20
//Parm: 待设置值;待设置位;待设置值;nNum位数
//Desc: 设置nNum第nBit位的值为nValue
class function TBitHelper.SetBit(const nNum: Integer; const nBit, nValue: Byte;
  const nBitCount: TBitCount): Integer;
begin
  if nBit > Byte(nBitCount) then
  begin
    Result := nNum;
  end else
  begin
    if nValue = 0 then
         Result := nNum and not(1 shl (nBit - 1))
    else Result := nNum or (1 shl (nBit - 1));
  end;
end;

//Date: 2019-06-24
//Parm: 待解析值;位数
//Desc: 解析nValue的二进制显示
class function TBitHelper.Int2Bin(const nValue: LongInt;
  const nBitCount: TBitCount): string;
var nIdx,nLen: Integer;
begin
  nLen := Byte(nBitCount);
  SetLength(Result, nLen);

  for nIdx:=nLen - 1 downto 0 do
  begin
    if nValue and (1 shl nIdx) = 0 then
         Result[nLen - nIdx + TStringHelper.cFI - 1] := '0'
    else Result[nLen - nIdx + TStringHelper.cFI - 1] := '1';
  end;
end;

//Date: 2019-06-24
//Parm: 二进制字符串;位数
//Desc: 将nValue拼接为十进制数值
class function TBitHelper.Bin2Int(const nValue: string): LongInt;
var nIdx,nLen: Integer;
begin
  Result := 0;
  nLen := High(nValue);

  for nIdx:=nLen downto TStringHelper.cFI do
  begin
    if nValue[nIdx] = '1' then
      Result := Result + (1 shl (nLen - nIdx));
    //xxxxx
  end;
end;

//------------------------------------------------------------------------------
//Date: 2020-10-14
//Parm: 待编码数据
//Desc: 对nData执行Base32编码
class function TEncodeHelper.EncodeBase32(const nData: string): string;
begin
  Result := '';
  //Result := SZEncodeBase32(nData);
  //unit: Third\CodeBaseX\SZCodeBaseX.pas
end;

//Date: 2020-10-14
//Parm: 待解码数据
//Desc: 对nData知行Base32解码
class function TEncodeHelper.DecodeBase32(const nData: string): string;
begin
  Result := '';
  //Result := SZDecodeBase32(nData);
  //unit: Third\CodeBaseX\SZCodeBaseX.pas
end;

//Date: 2017-03-17
//Parm: 待编码数据;是否换行
//Desc: 对nData执行base64编码
class function TEncodeHelper.EncodeBase64(const nData: string;
  const nLineBreak: Boolean): string;
var nCoder: TBase64Encoding;
begin
  nCoder := nil;
  try
    if nLineBreak then
         nCoder := TBase64Encoding.Create
    else nCoder := TBase64Encoding.Create(MaxInt);

    Result := nCoder.Encode(nData);
  finally
    nCoder.Free;
  end;
end;

//Date: 2017-03-17
//Parm: 待解码数据
//Desc: 对nData执行base64解码
class function TEncodeHelper.DecodeBase64(const nData: string): string;
begin
  with TBase64Encoding.Create do
  try
    Result := Decode(nData);
  finally
    Free;
  end;
end;

//Date: 2017-03-17
//Parm: 待编码数据
//Desc: 对nData执行md5编码 
class function TEncodeHelper.EncodeMD5(const nData: string): string;
begin
  Result := THashMD5.GetHashString(nData);
end;

//Date: 2021-03-26
//Parm: 待编码数据;密钥
//Desc: 对nData执行1DES编码,输出Base64密文
class function TEncodeHelper.Encode_1DES(const nData, nKey: string): string;
{$IFDEF EnableThirdDEC}
var nCipher: TCipher_1DES;
{$ENDIF}
begin
  {$IFDEF EnableThirdDEC}
  nCipher := TCipher_1DES.Create;
  try
    nCipher.Init(RawByteString(nKey));
    nCipher.Mode := cmCBCx;
    Result := nCipher.EncodeStringToString(nData, TFormat_Base64);
  finally
    nCipher.Free;
  end;
  {$ELSE}
  raise Exception.Create('LibFun: DEC Library Is Disabled.');
  {$ENDIF}
end;

//Date: 2021-03-26
//Parm: 待解码数据;密钥
//Desc: 对nData执行1DES解码
class function TEncodeHelper.Decode_1DES(const nData, nKey: string): string;
{$IFDEF EnableThirdDEC}
var nCipher: TCipher_1DES;
{$ENDIF}
begin
  {$IFDEF EnableThirdDEC}
  nCipher := TCipher_1DES.Create;
  try
    nCipher.Init(RawByteString(nKey));
    nCipher.Mode := cmCBCx;
    Result := nCipher.DecodeStringToString(nData, TFormat_Base64);
  finally
    nCipher.Free;
  end;
  {$ELSE}
  raise Exception.Create('LibFun: DEC Library Is Disabled.');
  {$ENDIF}
end;

//Date: 2021-03-26
//Parm: 待编码数据;密钥
//Desc: 对nData执行3DES编码,输出Base64密文
class function TEncodeHelper.Encode_3DES(const nData, nKey: string): string;
{$IFDEF EnableThirdDEC}
var nCipher: TCipher_3DES;
{$ENDIF}
begin
  {$IFDEF EnableThirdDEC}
  nCipher := TCipher_3DES.Create;
  try
    nCipher.Init(RawByteString(nKey));
    nCipher.Mode := cmCBCx;
    Result := nCipher.EncodeStringToString(nData, TFormat_Base64);
  finally
    nCipher.Free;
  end;
  {$ELSE}
  raise Exception.Create('LibFun: DEC Library Is Disabled.');
  {$ENDIF}
end;

//Date: 2021-03-26
//Parm: 待解码数据;密钥
//Desc: 对nData执行1DES解码
class function TEncodeHelper.Decode_3DES(const nData, nKey: string): string;
{$IFDEF EnableThirdDEC}
var nCipher: TCipher_3DES;
{$ENDIF}
begin
  {$IFDEF EnableThirdDEC}
  nCipher := TCipher_3DES.Create;
  try
    nCipher.Init(RawByteString(nKey));
    nCipher.Mode := cmCBCx;
    Result := nCipher.DecodeStringToString(nData, TFormat_Base64);
  finally
    nCipher.Free;
  end;
  {$ELSE}
  raise Exception.Create('LibFun: DEC Library Is Disabled.');
  {$ENDIF}
end;

{$IFDEF JsonSerializers}
//Date: 2017-12-20
//Parm: 结构数据
//Desc: 序列化nRecord为json字符串
class function TEncodeHelper.EncodeRecord<T>(const nRecord: T): string;
var nSeriallizer: TJsonSerializer;
begin
  nSeriallizer := nil;
  try
    nSeriallizer := TJsonSerializer.Create;
    with nSeriallizer do
    begin
      Formatting := TJsonFormatting.None;
      DateParseHandling := TJsonDateParseHandling.None;
      DateTimeZoneHandling := TJsonDateTimeZoneHandling.Utc;
      DateFormatHandling := TJsonDateFormatHandling.FormatSettings;
    end;

    Result := nSeriallizer.Serialize<T>(nRecord);
    //do serial
  finally
    nSeriallizer.Free;
  end;
end;

//Date: 2017-12-20
//Parm: 序列化json字符串
//Desc: 将nRecord反序列化为T结构
class function TEncodeHelper.DecodeRecord<T>(const nRecord: string): T;
var nSeriallizer: TJsonSerializer;
begin
  nSeriallizer := nil;
  try
    nSeriallizer := TJsonSerializer.Create;
    with nSeriallizer do
    begin
      Formatting := TJsonFormatting.None;
      DateParseHandling := TJsonDateParseHandling.None;
      DateTimeZoneHandling := TJsonDateTimeZoneHandling.Utc;
      DateFormatHandling := TJsonDateFormatHandling.FormatSettings;
    end;

    Result := nSeriallizer.Deserialize<T>(nRecord);
    //do serial
  finally
    nSeriallizer.Free;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
//Desc: 本地化格式
function LocalDTSettings: TFormatSettings;
begin
  Result := TFormatSettings.Create('en-US');
  //default config  

  with Result do
  begin
    ShortDateFormat:='yyyy-MM-dd';
    DateSeparator  :='-';
    LongTimeFormat :='hh:mm:ss';
    TimeSeparator  :=':';
  end;
end;

//Desc: 转换为日期型
class function TDateTimeHelper.Str2Date(const nStr: string): TDate;
begin
  try
    Result := StrToDate(nStr, LocalDTSettings);
  except
    Result := Date();
  end;
end;

//Desc: 日期转字符串
class function TDateTimeHelper.Date2Str(const nDate: TDateTime;
  const nSeparator: Boolean): string;
begin
  if nSeparator then
       Result := FormatDateTime('YYYY-MM-DD', nDate)
  else Result := FormatDateTime('YYYYMMDD', nDate);
end;

//Desc: 转换为时间型
class function TDateTimeHelper.Str2Time(const nStr: string): TTime;
begin
  try
    Result := StrToTime(nStr, LocalDTSettings);
  except
    Result := Time();
  end;
end;

class function TDateTimeHelper.Time2Str(const nTime: TDateTime;
  const nSeparator,nMSec: Boolean): string;
begin
  if nSeparator then
  begin
    Result := 'HH:MM:SS';
    if nMSec then
      Result := Result + ':ZZZ';
    //Milliseconds
  end else
  begin
    Result := 'HHMMSS';
    if nMSec then
      Result := Result + 'ZZZ';
    //Milliseconds
  end;

  Result := FormatDateTime(Result, nTime);
end;

class function TDateTimeHelper.Str2DateTime(const nStr: string): TDateTime;
begin
  try
    Result := StrToDateTime(nStr, LocalDTSettings);
  except
    Result := Now();
  end;
end;

//Desc: 日期转字符串
class function TDateTimeHelper.DateTime2Str(const nDT: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:mm:ss', nDT);
end;

//Desc: 将nDate转为yyyy年MM月dd日
class function TDateTimeHelper.Date2CH(const nDate: TDate): string;
begin
  Result := Date2Str(nDate, False);
  Result := Copy(Result, 1, 4) + '年' +
            Copy(Result, 5, 2) + '月' +
            Copy(Result, 7, 2) + '日';
  //combine
end;

//Desc: 将hhMMss的字符串转为hh时MM分ss秒
class function TDateTimeHelper.Time2CH(const nTime: TTime): string;
begin
  Result := Time2Str(nTime, False);
  Result := Copy(Result, 1, 2) + '时' +
            Copy(Result, 3, 2) + '分' +
            Copy(Result, 5, 2) + '秒';
  //combine
end;

//Date: 2013-07-21
//Parm: 前缀;日期
//Desc: 获取nDate的周天描述
class function TDateTimeHelper.Date2Week(nPrefix: string;
  nDate: TDateTime): string;
begin
  if nDate = 0 then
    nDate := Date();
  //default is now

  if nPrefix = '' then
    nPrefix := '星期';
  //default prefix

  case DayOfWeek(nDate) of
   1: Result := '天';
   2: Result := '一';
   3: Result := '二';
   4: Result := '三';
   5: Result := '四';
   6: Result := '五';
   7: Result := '六' else Result := '';
  end;

  Result := nPrefix + Result;
end;

//Date: 2017-04-14
//Parm: 时间长度(单位毫秒)
//Desc: 描述nTime毫秒是多长时间(天,小时,分,秒,毫秒)
class function TDateTimeHelper.TimeLong2CH(const nTime: Int64): string;
var nD,nH,nM,nS,nMS: Int64;
begin
  nD := Trunc(nTime / (1000 * 3600 * 24));
  nMS := nTime - nD * 1000 * 3600 * 24;
   
  nH := Trunc(nMS / (1000 * 3600));
  nMS := nMS - nH * 1000 * 3600;

  nM := Trunc(nMS / (1000 * 60));
  nMS := nMS - nM * 1000 * 60;

  nS := Trunc(nMS / 1000);
  nMS := nMS - nS * 1000;   
  Result := Format('%d 天 %d 时 %d 分 %d 秒 %d 毫秒', [nD, nH, nM, nS, nMS]);
end;

//Date: 2017-11-20
//Desc: 使用日期生产唯一串号
class function TDateTimeHelper.DateTimeSerial(const nSeparator: Boolean): string;
begin
  Sleep(1); //must be
  if nSeparator then
       Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now())
  else Result := FormatDateTime('yyyymmddhhnnsszzz', Now());
end;

//Date: 2019-04-01
//Desc: 短间隔计数器
class function TDateTimeHelper.GetTickCount: Cardinal;
begin
  Result := TThread.GetTickCount();
end;

//Date: 2019-01-11
//Parm: 上一次调用GetTickCount的值;默认值
//Desc: 计算GetTickCount - nCount的差值,需校正溢出归零问题
class function TDateTimeHelper.GetTickCountDiff(const nCount:Cardinal;
  const nDefault: TTickDefault): Int64;
begin
  if nCount = 0 then
  begin
    Result := 0;
    //default

    case nDefault of
     tdZero : Exit;
     tdNow  : Result := TDateTimeHelper.GetTickCount();
    end;
  end else
  begin
    Result := TDateTimeHelper.GetTickCount();
    //default

    if Result >= nCount then
         Result := Result - nCount
    else Result := Result + High(Cardinal) - nCount + 1;
  end;
end;

//Date: 2021-01-04
//Desc: 初始化库
procedure InitLibrary;
var nSymbol: string;
begin
  with TApplicationHelper do
  begin
    gPath := ExtractFilePath(ParamStr(0));
    nSymbol := TStringHelper.CopyRight(gPath, 1);
    if (nSymbol <> '\') and (nSymbol <> '/') then
    begin
      nSymbol := PathDelim;
      gPath := gPath + nSymbol;
    end;

    gSysConfig  := gPath + 'Config.Ini';
    gFormConfig := gPath + 'Forms.Ini';
    gDBConfig   := gPath + 'DBConn.Ini';
    gLogPath    := gPath + 'Logs' + nSymbol;
  end;
end;

initialization
  InitLibrary();
finalization
  //nothing
end.
