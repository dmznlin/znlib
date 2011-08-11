{*******************************************************************************
  ����: dmzn@163.com 2007-10-09
  ����: ��Ŀͨ�ú������嵥Ԫ

  ��ע:
  &.ͨ�ú�������һ�鹦�ܵ�Ԫ���,ʹ��ǰ�����InitGlobalVariant��ʼ��ȫ�ֱ���.
*******************************************************************************}
unit ULibFun;

{$I LibFun.Inc}
interface

uses
  Windows, Classes, Controls, Forms, Messages, SysUtils, IniFiles, ShellAPI,
  ZnMd5, UMgrVar, ULibRes;

//------------------------------------------------------------------------------
type
  TFloatRelationType = (rtGreater, rtGE, rtEqual, rtLE, rtLess);
  //�����ϵ(>, >=, =, <=, <)

  PMacroItem = ^TMacroItem;
  TMacroItem = record
    FMacro: string;                                  //�궨��
    FValue: string;                                  //��ȡֵ
  end;

  TDynamicMacroArray = array of TMacroItem;
  //�궨������
  TDynamicStrArray = array of string;
  //�ַ�������

//------------------------------------------------------------------------------
function MI(const nMacro,nValue: string): TMacroItem;
function MacroValue(const nData: string; const nMacro: array of TMacroItem): string;
//�����궨��

function DSA(const nStr: array of string): TDynamicStrArray;
//�ַ������װ
function StrArrayIndex(const nStr: string; const nArray: TDynamicStrArray;
  const nIgnoreCase: Boolean = True): integer;
//�ַ�������

//------------------------------------------------------------------------------
procedure InitGlobalVariant(const nAppPath,nSysConfig,nFormConfig: string;
  const nDBConfig: string = cVarEmpty; const nDlgMsg: string = cVarEmpty;
  const nHint: string = cVarEmpty; const nAsk: string = cVarEmpty;
  const nWarn: string = cVarEmpty);
//���û�������

procedure PopMsgOnOff(const nOn: Boolean = True);
procedure PopMsgBackImage(const nIdx: integer = -1);
//��������

//------------------------------------------------------------------------------
procedure ShowMsg(const nMsg,nTitle: string);
procedure ShowHintMsg(const nMsg,nTitle: string; const nHwnd: integer = -1);
procedure ShowDlg(const nMsg,nTitle: string; const nHwnd: integer = -1);
function QueryDlg(const nMsg,nTitle: string; const nHwnd: integer = -1): Boolean;
//��ʾ��,ѯ�ʿ�

function CombinStr(const nList: TStrings;
 const nFlag: string = ''): string; overload;
function CombinStr(const nStrs: array of string;
 const nFlag: string = ''): string; overload;
function SplitStr(const nStr: string; const nList: TStrings; const nNum: Word;
 const nFlag: string = ''): Boolean;
//�ϲ�,����ַ���

function StrWithWidth(const nStr: string; const nWidth,nStyle: Byte;
  const nFixChar: Char = #32): string;
//�����ַ���

function SplitValue(const nStr: string; const nList: TStrings): Boolean;
function SplitIntValue(const nStr: string; const nDef: Integer = 0): Integer;
function SplitFloatValue(const nStr: string; const nDef: Double = 0): Double;
//��ֳ���ֵ

function Float2PInt(const nValue: Double; const nPrecision: Integer;
 const nRound: Boolean = True): Int64;
function Float2Float(const nValue: Double; const nPrecision: Integer;
 const nRound: Boolean = True): Double;
//������ת������
function FloatRelation(const nA,nB: Double; const nType: TFloatRelationType;
 const nPrecision: Integer = 100): Boolean;
//�����ϵ�ж�

//------------------------------------------------------------------------------
function GetFileVersionStr(const nFile: string): string;
//��ȡ�ļ��汾
function IsNumber(const nStr: string; const nFloat: Boolean): Boolean;
//�Ƿ���ֵ
procedure SwitchFocusCtrl(const nCtrl: TControl; const nDown: Boolean);
//�л�����

procedure LoadFormConfig(const nForm: TForm; const nIniF: TIniFile = nil;
 const nFile: string = '');
//���봰����Ϣ
procedure SaveFormConfig(const nForm: TForm; const nIniF: TIniFile = nil;
 const nFile: string = '');
//�洢������Ϣ

function IsValidConfigFile(const nFile,nSeed: string): Boolean;
//У��nFile�Ƿ�Ϸ������ļ�
procedure AddVerifyData(const nFile,nSeed: string);
//ΪnFile����У����Ϣ

//------------------------------------------------------------------------------
function GetPinYinOfStr(const nChinese: string): string;
//��ȡnChinese��ƴ����д

function Str2Date(const nStr: string): TDate;
//change nStr to date value
function Str2Time(const nStr: string): TTime;
//change nStr to time value
function Date2Str(const nDate: TDateTime): string;
//change nDate to string value
function Time2Str(const nTime: TDateTime): string;
//change nTime to string value
function DateTime2Str(const nDT: TDateTime): string;
//change nDT to string value
function Str2DateTime(const nStr: string): TDateTime;
//change nStr to datetime value
function Date2CH(const nDate: string): string;
//change nDate to chinese string
function Time2CH(const nTime: string): string;
//change nTime to chinese string

implementation

//------------------------------------------------------------------------------
type
  TPopMsg_Init = procedure (const nApp: TApplication; const nScreen: TScreen;
    const nBackImg: integer = -1); stdcall;
  TPopMsg_Free = procedure; stdcall;
  TPopMsg_ShowMsg = procedure (const nMsg,nTitle: PChar); stdcall;

var
  gPopMsg_Hwnd: THandle = 0;
  gPopMsg_Init: TPopMsg_Init = nil;
  gPopMsg_Free: TPopMsg_Free = nil;
  gPopMsg_ShowMsg: TPopMsg_ShowMsg = nil;
  gPopMsg_BackImg: integer = 0;

//Desc: ��ʼ��������
function InitPopMsgLibrary: Boolean;
var nStr: string;
begin
  Result := False;
  nStr := gVariantManager.VarStr(sVar_DlgMsg, sVar_DlgMsgDef);

  gPopMsg_Hwnd := LoadLibrary(PChar(nStr));
  if gPopMsg_Hwnd < 32 then Exit;

  @gPopMsg_Init := GetProcAddress(gPopMsg_Hwnd, 'PopMsg_Init');
  @gPopMsg_Free := GetProcAddress(gPopMsg_Hwnd, 'PopMsg_Free');
  @gPopMsg_ShowMsg := GetProcAddress(gPopMsg_Hwnd, 'PopMsg_ShowMsg');

  Result := Assigned(gPopMsg_Init) and
            Assigned(gPopMsg_Free) and
            Assigned(gPopMsg_ShowMsg);
  if Result then gPopMsg_Init(Application, Screen, gPopMsg_BackImg);
end;

//Desc: ��ʾ��Ϣ
procedure ShowMsg(const nMsg,nTitle: string);
begin
  if not (Assigned(gPopMsg_ShowMsg) or InitPopMsgLibrary) then
    raise Exception.Create('Load PopMsg Library Error!');
  gPopMsg_ShowMsg(PChar(nMsg), PChar(nTitle));
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//Desc:��ʼ��ȫ�ֱ���
procedure InitGlobalVariant;
begin
  if nAppPath <> cVarEmpty then
    gVariantManager.AddVarStr(sVar_AppPath, nAppPath);
  if nSysConfig <> cVarEmpty then
    gVariantManager.AddVarStr(sVar_SysConfig, nSysConfig);
  if nFormConfig <> cVarEmpty then
    gVariantManager.AddVarStr(sVar_FormConfig, nFormConfig);
  if nDBConfig <> cVarEmpty then
    gVariantManager.AddVarStr(sVar_ConnDBConfig, nDBConfig);
  if nDlgMsg <> cVarEmpty then
    gVariantManager.AddVarStr(sVar_DlgMsg, nDlgMsg);

  if nHint <> cVarEmpty then gVariantManager.AddVarStr(sVar_DlgHintStr, nHint);
  if nAsk <> cVarEmpty then gVariantManager.AddVarStr(sVar_DlgAskStr, nAsk);
  if nWarn <> cVarEmpty then gVariantManager.AddVarStr(sVar_DlgWarnStr, nWarn);
end;

//Desc: ����ʽ��ʾ�򿪹�
procedure PopMsgOnOff(const nOn: Boolean = True);
begin
  if nOn then
       gVariantManager.DelVar(sVar_DlgMsgLocked)
  else gVariantManager.AddVarStr(sVar_DlgMsgLocked, sVar_DlgMsgLockFlag);
end;

//Desc: ����ʽ��ʾ�򱳾�ͼƬ
procedure PopMsgBackImage(const nIdx: integer = -1);
begin
  gPopMsg_BackImg := nIdx;
end;

//------------------------------------------------------------------------------
//Desc: ��ʾ��Ϣ
procedure ShowDlg(const nMsg,nTitle: string; const nHwnd: integer = -1);
var nStr: string;
    nHandle: THandle;
begin
  if nTitle = '' then
       nStr := gVariantManager.VarStr(sVar_DlgHintStr, sVar_DlgHintStrDef)
  else nStr := nTitle;

  if nHwnd < 0 then
       nHandle := GetActiveWindow
  else nHandle := nHwnd;

  Messagebox(nHandle, PChar(nMsg), PChar(nStr), mb_Ok + Mb_IconInformation);
end;

//Desc: ���ݱ༭����ѡ��ͬ����Ϣ��
procedure ShowHintMsg(const nMsg,nTitle: string; const nHwnd: integer = -1);
begin
  if gVariantManager.VarStr(sVar_DlgMsgLocked) = sVar_DlgMsgLockFlag then
       ShowDlg(nMsg, nTitle, nHwnd)
  else ShowMsg(nMsg, nTitle);
end;

//Desc: ѯ�ʶԻ���
function QueryDlg(const nMsg,nTitle: string; const nHwnd: integer = -1): Boolean;
var nStr: string;
    nHandle: THandle;
begin
  if nTitle = '' then
       nStr := gVariantManager.VarStr(sVar_DlgAskStr, sVar_DlgAskStrDef)
  else nStr := nTitle;

  if nHwnd < 0 then
       nHandle := GetActiveWindow
  else nHandle := nHwnd;

  Result := Messagebox(nHandle, PChar(nMsg),
            PChar(nStr), Mb_YesNo + MB_ICONQUESTION) = IDYes;
end;

//Desc: �ж�nStr�Ƿ�Ϊ��ֵ,nFloat�趨�Ƿ�����С��
function IsNumber(const nStr: string; const nFloat: Boolean): Boolean;
begin
  Result := False;
  try
    if nStr <> '' then
    begin
      if nFloat then
           StrToFloat(nStr)
      else StrToInt(nStr);
      Result := True;
    end;
  except
    //ignor any error
  end;
end;

//Desc: �궨����
function MI(const nMacro,nValue: string): TMacroItem;
begin
  Result.FMacro := nMacro;
  Result.FValue := nValue;
end;

//Date: 2008-8-8
//Parm: ������ַ���;������
//Desc: ����nMacro������,�滻nData�����еĺ궨��
function MacroValue(const nData: string; const nMacro: array of TMacroItem): string;
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

//Date: 2010-3-9
//Parm: �ַ�����
//Desc: ��nStr��װΪ��̬�ַ�����
function DSA(const nStr: array of string): TDynamicStrArray;
var nIdx: Integer;
begin
  SetLength(Result, Length(nStr));
  for nIdx:=Low(nStr) to High(nStr) do
   Result[nIdx] := nStr[nIdx];
end;

//Date: 2010-3-5
//Parm: �ַ���;����;���Դ�Сд
//Desc: ����nStr��nArray�е�����λ��
function StrArrayIndex(const nStr: string; const nArray: TDynamicStrArray;
  const nIgnoreCase: Boolean = True): integer;
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
      Result := nIdx; Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
//Desc: �ϲ�nList���ַ���
function CombinStr(const nList: TStrings; const nFlag: string = ''): string;
var nStr: string;
    i,nCount: integer;
begin
  if nFlag = '' then
       nStr := ';'
  else nStr := nFlag;

  Result := '';
  nCount := nList.Count - 1;
  
  for i:=0 to nCount do
   if i = nCount then
        Result := Result + nList[i]
   else Result := Result + nList[i] + nStr;
end;

//Desc: �ϲ�nStrs���ַ���
function CombinStr(const nStrs: array of string; const nFlag: string = ''): string;
var nStr: string;
    i,nLen: integer;
begin
  if nFlag = '' then
       nStr := ';'
  else nStr := nFlag;

  Result := '';
  nLen := High(nStrs);

  for i:=Low(nStrs) to nLen do
   if i = nLen then
        Result := Result + nStrs[i]
   else Result := Result + nStrs[i] + nStr;
end;

//Desc: ��nStartλ������nSub���ַ�����nStr�е�����
function Q_PosStr(const nSub, nStr: string; const nStart: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        SUB     EDX,ECX
        JNG     @@qt0
        XCHG    EAX,EDX
        ADD     EDI,ECX
        MOV     ECX,EAX
        JMP     @@nx
@@fr:   INC     EDI
        DEC     ECX
        JE      @@qt0
@@nx:   MOV     EBX,EDX
        MOV     AL,BYTE PTR [ESI]
@@lp1:  CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JE      @@qt0
        CMP     AL,BYTE PTR [EDI]
        JE      @@uu
        INC     EDI
        DEC     ECX
        JNE     @@lp1
@@qt0:  XOR     EAX,EAX
@@qt:   POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
        RET
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JE      @@fd
        MOV     AL,BYTE PTR [ESI+EBX]
        CMP     AL,BYTE PTR [EDI+EBX]
        JNE     @@fr
        DEC     EBX
        JNE     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        SUB     EAX,[ESP]
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;

//Desc: ���nStrΪnNum��,����nList��
function SplitStr(const nStr: string; const nList: TStrings; const nNum: Word;
  const nFlag: string = ''): Boolean;
var nSF: string;
    nPos,nNow,nLen: integer;
begin
  if nFlag = '' then
       nSF := ';'
  else nSF := nFlag;

  nList.Clear;
  nlen := Length(nSF);
  nPos := Q_PosStr(nSF, nStr, 1);

  nNow := 1;
  while nPos > 0 do
  begin
    nList.Add(Copy(nStr, nNow, nPos - nNow));
    nNow := nPos + nLen;
    nPos := Q_PosStr(nSF, nStr, nNow);
  end;

  nLen := Length(nStr);
  if nNow <= nLen then
    nList.Add(Copy(nStr, nNow, nLen - nNow + 1));
  if nNum > 0 then
       Result := nList.Count = nNum
  else Result := nList.Count > 0;
end;

//Desc: �����ַ���,��������nFixChar���
function StrWithWidth(const nStr: string; const nWidth,nStyle: Byte;
 const nFixChar: Char = #32): string;
var nLen,nHalf: Integer;
begin
  nLen := Length(nStr);
  if nLen >= nWidth then
  begin
    Result := nStr; Exit;
  end;

  nLen := nWidth - nLen;
  //not enough length

  case nStyle of
   1: Result := nStr + StringOfChar(nFixChar, nLen);
   2: Result := StringOfChar(nFixChar, nLen) + nStr;
   3: begin
        nHalf := Trunc(nLen / 2);
        Result := StringOfChar(nFixChar, nHalf) + nStr +
                  StringOfChar(nFixChar, nLen - nHalf)
      end else Result := nStr;
  end;
end;

//Desc: ��nStr�в�ֳ���ֵ,���б�����nList��
function SplitValue(const nStr: string; const nList: TStrings): Boolean;
var nVal: string;
    i,nLen: integer;
begin
  nList.Clear;
  nVal := '';
  nLen := Length(nStr);

  for i:=1 to nLen do
  if nStr[i] in ['0'..'9', '.'] then
  begin
    nVal := nVal + nStr[i];
  end else

  if nVal <> '' then
  begin
    if IsNumber(nVal, True) then
      nList.Add(nVal);
    nVal := '';
  end;

  if (nVal <> '') and IsNumber(nVal, True) then
    nList.Add(nVal);
  Result := nList.Count > 0;
end;

//Desc: ��nStr�в��������ֵ
function SplitIntValue(const nStr: string; const nDef: Integer): Integer;
var nVal: string;
    i,nLen: integer;
begin
  Result := nDef;
  nVal := '';
  nLen := Length(nStr);

  for i:=1 to nLen do
  if nStr[i] in ['0'..'9'] then
    nVal := nVal + nStr[i]
  else if nVal <> '' then Break;

  if (nVal <> '') and IsNumber(nVal, False) then
    Result := StrToInt(nVal);
  //xxxxx
end;

//Desc: ��nStr�в�ָ�����ֵ
function SplitFloatValue(const nStr: string; const nDef: Double): Double;
var nVal: string;
    i,nLen: integer;
begin
  Result := nDef;
  nVal := '';
  nLen := Length(nStr);

  for i:=1 to nLen do
  if nStr[i] in ['0'..'9', '.'] then
    nVal := nVal + nStr[i]
  else if nVal <> '' then Break;

  if (nVal <> '') and IsNumber(nVal, True) then
    Result := StrToFloat(nVal);
  //xxxxx
end;

//Date: 2010-3-9
//Parm: ֵ;����;��������
//Desc: ��nValue�Ŵ�nPrecision,Ȼ��ȡ��,С��λ��������
function Float2PInt(const nValue: Double; const nPrecision: Integer;
 const nRound: Boolean): Int64;
var nStr: string;
    nPos: integer;
begin
  nStr := FloatToStr(nValue * nPrecision);
  nPos := Pos('.', nStr);

  if nPos > 1 then
  begin
    Result := StrToInt64(Copy(nStr, 1, nPos - 1));
    if nRound then
    begin
      System.Delete(nStr, 1, nPos);
      nStr := Copy(nStr, 1, 1);
      if StrToInt(nStr) >= 5 then Inc(Result);
    end;
  end else Result := StrToInt64(nStr);
end;

//Date: 2010-4-21
//Parm: ֵ;����;��������
//Desc: ��nValue�Ŵ�nPrecision,ȡ��,Ȼ�󻻻�С��
function Float2Float(const nValue: Double; const nPrecision: Integer;
 const nRound: Boolean = True): Double;
begin
  Result := Float2PInt(nValue, nPrecision, nRound) / nPrecision;
end;

//Date: 2010-7-14
//Parm: ������A,B;���ж���ϵ;�ж�����
//Desc: ��nPrecision����,�ж�nA��nB�Ƿ�����nType��ϵ
function FloatRelation(const nA,nB: Double; const nType: TFloatRelationType;
 const nPrecision: Integer = 100): Boolean;
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
//Date: 2006-09-21
//Desc: ���������ļ�����nForm����Ϣ
procedure LoadFormConfig;
var nStr: string;
    nIni: TIniFile;
    nValue,nMax: integer;
begin
  if Assigned(nIniF) then
     nIni := nIniF else
  begin
    if nFile = '' then
         nStr := gVariantManager.VarStr(sVar_FormConfig)
    else nStr := nFile;

    if not FileExists(nStr) then Exit;
    nIni := TIniFile.Create(nStr);
  end;

  try
    with nForm do
    begin
      if nIni.ReadBool(Name, 'Maximized', False) = True then
         WindowState := wsMaximized else
      //���״̬
      begin
        nMax := High(integer);
        nValue := nIni.ReadInteger(Name, 'FormTop', nMax);
        if nValue < nMax then Top := nValue;

        nValue := nIni.ReadInteger(Name, 'FormLeft', nMax);
        if nValue < nMax then Left := nValue;

        if BorderStyle = bsSizeable then
        begin
          nValue := nIni.ReadInteger(Name, 'FormWidth', nMax);
          if nValue < nMax then Width := nValue;

          nValue := nIni.ReadInteger(Name, 'FormHeight', nMax);
          if nValue < nMax then Height := nValue;
        end;
      end;
    end;
    //���봰��λ�úͿ���
  finally
    if not Assigned(nIniF) then nIni.Free;
  end;
end;

//Date: 2006-09-21
//Desc: �洢nForm����Ϣ���������ļ�
procedure SaveFormConfig;
var nStr: string;
    nIni: TIniFile;
begin
  if Assigned(nIniF) then
     nIni := nIniF else
  begin
    if nFile = '' then
         nStr := gVariantManager.VarStr(sVar_FormConfig)
    else nStr := nFile;

    if nStr = '' then
         raise Exception.Create('Invalidate ConfigFile!')
    else nIni := TIniFile.Create(nStr);
  end;

  try
    with nForm do
    begin
      nIni.WriteInteger(Name, 'FormTop', Top);
      nIni.WriteInteger(Name, 'FormLeft', Left);
      nIni.WriteInteger(Name, 'FormWidth', Width);
      nIni.WriteInteger(Name, 'FormHeight', Height);
      nIni.WriteBool(Name, 'Maximized', WindowState = wsMaximized);
      //���洰��λ�úͿ���
    end;
  finally
    if not Assigned(nIniF) then nIni.Free;
  end;
end;

//------------------------------------------------------------------------------
//Desc: ��ȡ�ļ��汾
function GetFileVersionStr(const nFile: string): string;
var nLen: UInt;
    nStr: string;
    nSize: DWord;
    nHwnd: THandle;
    nBuf: PChar;
    nTmp: Pointer;
    nName: array [0..Max_Path - 1] of Char;
begin
  Result := '';
  nSize := GetFileVersionInfoSize(PChar(nFile), nHwnd);
  if nSize = 0 then Exit;

  nBuf := AllocMem(nSize);
  try
    if GetFileVersionInfo(PChar(nFile), nHwnd, nSize, nBuf) then
    begin
      nTmp := nil;
      VerQueryValue(nBuf, '\VarFileInfo\Translation', nTmp, nLen);

      if nTmp <> nil then
        nStr := IntToHex(MakeLong(HiWord(Longint(nTmp^)), LoWord(Longint(nTmp^))), 8);
      StrPCopy(@nName[0], '\StringFileInfo\' + nStr + '\FileVersion');
      if VerQueryValue(nBuf, nName, nTmp, nLen) then Result := StrPas(PChar(nTmp));
    end;
  finally
    FreeMem(nBuf, nSize);
  end;
end;

//Desc: ����,�����л�nCtrl�ϵĽ���λ��
procedure SwitchFocusCtrl(const nCtrl: TControl; const nDown: Boolean);
begin
  if nDown then
       nCtrl.Perform(WM_NEXTDLGCTL, 0, 0)
  else nCtrl.Perform(WM_NEXTDLGCTL, 1, 0);
end;

//------------------------------------------------------------------------------
//Date: 2008-8-20
//Parm: �ļ�ȫ·��;��������
//Desc: У��nFile�Ƿ�Ϸ������ļ�
function IsValidConfigFile(const nFile,nSeed: string): Boolean;
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
      Result := MD5Print(MD5String(nList.Text)) = nStr;
    end;
  finally
    nList.Free;
  end;

end;

//Date: 2008-8-20
//Parm: �ļ�ȫ·��;��������
//Desc: ΪnFile����У����Ϣ
procedure AddVerifyData(const nFile,nSeed: string);
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

    nStr := MD5Print(MD5String(nList.Text));
    nStr := sVerifyCode + nStr;
    nList[0] := nStr;
    nList.SaveToFile(nFile);
  finally
    nList.Free;
  end;
end;

//------------------------------------------------------------------------------
const
  cPYData: array[216..247] of string = (
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

//Desc: ����תƴ��
function HZ2PY(const nValue: array of Char): Char;
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
  end; //һ������

  if Result = #0 then
   case Byte(nValue[0]) of
    216..247: Result := cPYData[Byte(nValue[0])][Byte(nValue[1]) - 160];
   end;
end;

//Desc: ��ȡnChinese��ƴ����д
function GetPinYinOfStr(const nChinese: string): string;
var nChar: Char;
    nIdx,nLen: integer;
    nArray: array[0..1] of Char;
begin
  Result := '';
  nLen := Length(nChinese);

  nIdx := 1;
  while nIdx < nLen do
  begin
    if nChinese[nIdx] < #160 then
    begin
      Result := Result + nChinese[nIdx];
      Inc(nIdx);
    end else
    begin
      nArray[0] := nChinese[nIdx];
      nArray[1] := nChinese[nIdx + 1];

      nChar := HZ2PY(nArray);
      if nChar <> #0 then
        Result := Result + nChar;
      Inc(nIdx, 2);
    end;
  end;

  if nIdx = nLen then
    Result := Result + nChinese[nIdx];
  Result := LowerCase(Result);
end;

//------------------------------------------------------------------------------ 
//Desc: ����ת�ַ���
function Date2Str(const nDate: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DD', nDate);
end;

//Desc: ʱ��ת�ַ���
function Time2Str(const nTime: TDateTime): string;
begin
  Result := FormatDateTime('HH:MM:SS', nTime);
end;

//Desc: ����ת�ַ���
function DateTime2Str(const nDT: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:mm:ss', nDT);
end;

//Desc: ת��Ϊ������
function Str2DateTime(const nStr: string): TDateTime;
begin
  try
    Result := StrToDateTime(nStr);
  except
    Result := Now;
  end;
end;

//Desc: ת��Ϊ������
function Str2Date(const nStr: string): TDate;
begin
  try
    Result := StrToDate(nStr);
  except
    Result := Date;
  end;
end;

//Desc: ת��Ϊʱ����
function Str2Time(const nStr: string): TTime;
begin
  try
    Result := StrToTime(nStr);
  except
    Result := Time;
  end;
end;

//Desc: ��yyyMMdd���ַ���תΪyyyy��MM��dd��
function Date2CH(const nDate: string): string;
var nLen: integer;
begin
  Result := '';
  nLen := Length(nDate);

  if nLen > 3 then Result := Copy(nDate, 1, 4) + '��';
  if nLen > 5 then Result := Result + Copy(nDate, 5, 2) + '��';
  if nLen > 7 then Result := Result + Copy(nDate, 7, 2) + '��';
end;

//Desc: ��hhMMss���ַ���תΪhhʱMM��ss��
function Time2CH(const nTime: string): string;
var nLen: integer;
begin
  Result := '';
  nLen := Length(nTime);

  if nLen > 1 then Result := Copy(nTime, 1, 2) + 'ʱ';
  if nLen > 3 then Result := Result + Copy(nTime, 3, 2) + '��';
  if nLen > 5 then Result := Result + Copy(nTime, 5, 2) + '��';
end;

initialization

finalization
  if gPopMsg_Hwnd > 0 then
    FreeLibrary(gPopMsg_Hwnd);
end.

