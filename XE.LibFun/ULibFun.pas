{*******************************************************************************
  作者: dmzn@163.com 2017-02-20
  描述: 项目通用函数定义单元
*******************************************************************************}
unit ULibFun;

{$I LibFun.inc}
interface

uses
  System.Classes, System.SysUtils, System.NetEncoding, System.Hash, Vcl.Controls,
  {$IFDEF JsonSerializers}System.JSON.Serializers, System.JSON.Types,{$ENDIF}
  System.Variants, System.IniFiles;

type
  TApplicationHelper = class
  strict private
  const
    sVerifyCode = ';Verify:';
    //id verify
  type
     TCPUID = array[1..4] of Longint;
    //id record

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
    class procedure EnumSubCtrlList(const nPCtrl: TWinControl;
      const nList: TList); static;
    //枚举nPCtrl的所有子控件
  end;

  TStringHelper = class
  public
    const
      {$IFNDEF XE2_UP}
        cFirstIndex = 1;
      {$ELSE}
        cFirstIndex = Low(String);
      {$ENDIF}
    type
      TFillPos = (fpLeft, fpMid, fpRight);
      //填充位置:左,中间,右
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

    class function MI(const nMacro,nValue: string): TMacroItem; static;
    class function MacroValue(const nData: string;
      const nMacro: TDynamicMacroArray): string; static;
    //处理宏定义
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
      const nNum: Word = 0; nFlag: string = ''; 
      const nFlagEnd: Boolean = True): Boolean; static;
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
      const nWidth: Byte; const nStyle: TFillPos = fpRight;  
      const nFixChar: Char = #32): string; static;
    //定长字符串
    class function SplitValue(const nStr: string; 
      const nList: TStrings): Boolean; static;
    class function SplitInt(const nStr: string; 
      const nDef: Integer = 0): Integer; static;
    class function SplitFloat(const nStr: string; 
      const nDef: Double = 0): Double; static;
    //拆分出数值
    class function GetPinYin(const nCH: string): string; static;
    //获取nChinese的拼音简写
    class function MirrorStr(const nStr: string): string; static;
    //镜像反转nStr字符串
    class function IsNumber(const nStr: string;
      const nFloat: Boolean = True): Boolean; static;
    //是否数值
  end;

  TSQLBuilder = class
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

  TEncodeHelper = class
  public
    class function EncodeBase64(const nData: string;
      const nLineBreak: Boolean = False): string; static;
    class function DecodeBase64(const nData: string): string; static;
    //base64
    class function EncodeMD5(const nData: string): string; static;
    //md5
    {$IFDEF JsonSerializers}
    class function EncodeRecord<T: record>(const nRecord: T): string; static;
    class function DecodeRecord<T: record>(const nRecord: string): T; static;
    //serialize record
    {$ENDIF}
  end;

  TDateTimeHelper = class
  public
    class function Str2Date(const nStr: string): TDate; static;
    //change nStr to date value
    class function Str2Time(const nStr: string): TTime; static;
    //change nStr to time value
    class function Date2Str(const nDate: TDateTime;
      const nSeparator: Boolean = True): string; static;
    //change nDate to string value
    class function Time2Str(const nTime: TDateTime;
      const nSeparator: Boolean = True): string; static;
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
    class function DateTimeSerial: string; static;
    //serial id by date
  end;

implementation

//------------------------------------------------------------------------------
//Date: 2018-03-15
//Desc: CPU标识字符串
class function TApplicationHelper.GetCPUIDStr: string;
var nIdx: Integer;
    nCPU: TCPUID;

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
begin
  try
    for nIdx:=Low(nCPU) to High(nCPU) do
      nCPU[nIdx] := -1;
    //xxxxx

    nCPU := GetCPUID;
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
//Desc: 验证nFile文件配置的日期是否过期s
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

//Date: 2018-03-15
//Parm: 文件全路径;加密种子
//Desc: 为nFile添加校验信息
class procedure TApplicationHelper.AddVerifyData(const nFile, nSeed: string);
var nStr: string;
    nList: TStrings;
begin
  if not FileExists(nFile) then Exit;
  nList := TStringList.Create;
  try
    nList.LoadFromFile(nFile);
    if (nList.Count > 0) and (Pos(sVerifyCode, nList[0]) = 1) then
         nList[0] := nSeed
    else nList.Insert(0, nSeed);

    nStr := TEncodeHelper.EncodeMD5(nList.Text);
    nStr := sVerifyCode + nStr;
    nList[0] := nStr;
    nList.SaveToFile(nFile);
  finally
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
      Result := TEncodeHelper.EncodeMD5(nList.Text) = nStr;
    end;
  finally
    nList.Free;
  end;
end;

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

//------------------------------------------------------------------------------
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
      Split(nList[nIdx], nTmp, 0, nFlag);
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
//Parm: 字符串;结果列表;结果个数;分隔符
//Desc: 使用nFlag将nStr拆分,结果存入nList.
class function TStringHelper.Split(const nStr: string; const nList: TStrings;
  const nNum: Word; nFlag: string; const nFlagEnd: Boolean): Boolean;
var nPos,nNow,nLen: integer;
begin
  if nFlag = '' then
    nFlag := ';';
  //def flag
  
  nList.Clear;
  nlen := Length(nFlag);
  nPos := Pos(nFlag, nStr, 1);

  nNow := 1;
  while nPos > 0 do
  begin
    nList.Add(Copy(nStr, nNow, nPos - nNow));
    nNow := nPos + nLen;
    nPos := Pos(nFlag, nStr, nNow);
  end;

  nLen := Length(nStr);
  if nNow <= nLen then
    nList.Add(Copy(nStr, nNow, nLen - nNow + 1));
  //xxxxx

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
    Split(nItems, nList, 0, nFlag, nFlagEnd);
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

  if nAdd and (not nListYet) then
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
        nStr := Copy(nStr, 2, nLen - 1);
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

  if nAdd and (not nListYet) then
    nList.Text := nBak;
  //还原内容
end;

//Date: 2017-03-17
//Parm: 字符串;长度;填充方式;填充负责
//Desc: 使用nFixChar填充nStr,使其保持nWidth定长.
class function TStringHelper.FixWidth(const nStr: string; const nWidth: Byte;
const nStyle: TFillPos; const nFixChar: Char): string;
var nLen,nHalf: Integer;
begin
  nLen := Length(nStr);
  if nLen >= nWidth then
  begin
    Result := nStr; 
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
      nHalf := Trunc(nLen / 2);
      Result := StringOfChar(nFixChar, nHalf) + nStr +
                StringOfChar(nFixChar, nLen - nHalf);
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

//Date: 2017-03-17
//Parm: 字符串
//Desc: 将nStr镜像反转
class function TStringHelper.MirrorStr(const nStr: string): string ;
var nIdx,nLen: Integer;
begin
  nLen := Length(nStr);
  SetLength(Result, nLen);

  for nIdx:=1 to nLen do
    Result[nIdx] := nStr[nLen - nIdx + 1];
  //convert
end;

//------------------------------------------------------------------------------
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
    raise Exception.Create('function "SF_IF" out of range.');
  Result := nData[nIdx];
end;

//Date: 2017-12-19
//Parm: 字段内容;选项
//Desc: 依据nBool使用nData的内容
class function TSQLBuilder.SF_IF(const nData: array of string;
  const nFirst: Boolean): string;
begin
    if Length(nData) < 2 then
    raise Exception.Create('function "SF_IF" out of range.');
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
  const nSeparator: Boolean): string;
begin
  if nSeparator then
       Result := FormatDateTime('HH:MM:SS', nTime)
  else Result := FormatDateTime('HHMMSS', nTime);
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
class function TDateTimeHelper.DateTimeSerial: string;
begin
  Sleep(1); //must be
  Result := FormatDateTime('yyyymmddhhnnsszzz', Now());
end;

end.
