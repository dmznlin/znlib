{*******************************************************************************
  作者: dmzn@163.com 2018-05-03
  描述: 业务对象数据封装器
*******************************************************************************}
unit UBusinessPacker;

interface

uses
  Windows, Classes, SyncObjs, SysUtils, UBase64, ULibFun;

type
  PBWWorkerInfo = ^TBWWorkerInfo;
  TBWWorkerInfo = record
    FUser   : string;              //发起人
    FIP     : string;              //IP地址
    FMAC    : string;              //主机标识
    FTime   : TDateTime;           //发起时间
    FKpLong : Int64;               //消耗时长
  end;

  TBWWorkerInfoType = (itFrom, itVia, itFinal);
  //信息类型

  PBWDataBase = ^TBWDataBase;
  TBWDataBase = record
    FWorker   : string;            //封装者
    FFrom     : TBWWorkerInfo;     //源
    FVia      : TBWWorkerInfo;     //经由
    FFinal    : TBWWorkerInfo;     //到达

    FMsgNO    : string;            //消息号
    FKey      : string;            //记录标记
    FParam    : string;            //扩展参数

    FResult   : Boolean;           //执行结果
    FErrCode  : string;            //错误代码
    FErrDesc  : string;            //错误描述
  end;

  TBusinessPackerBase = class(TObject)
  protected
    FEnabled: Boolean;
    //可用标记
    FStrBuilder: TStrings;
    //字符构建器
    FCodeEnable: Boolean;
    //启用编码
    procedure DoInitIn(const nData: Pointer); virtual;
    procedure DoInitOut(const nData: Pointer); virtual;
    procedure DoPackIn(const nData: Pointer); virtual;
    procedure DoUnPackIn(const nData: Pointer); virtual;
    procedure DoPackOut(const nData: Pointer); virtual;
    procedure DoUnPackOut(const nData: Pointer); virtual;
    //子类实现
    function PackerEncode(const nStr: string): string; overload;
    function PackerEncode(const nDT: TDateTime): string; overload;
    function PackerEncode(const nVal: Boolean): string; overload;
    procedure PackerDecode(const nStr: string; var nValue: string); overload;
    procedure PackerDecode(const nStr: string; var nValue: Boolean); overload;
    procedure PackerDecode(const nStr: string; var nValue: Integer); overload;
    procedure PackerDecode(const nStr: string; var nValue: Cardinal); overload;
    procedure PackerDecode(const nStr: string; var nValue: Int64); overload;
    procedure PackerDecode(const nStr: string; var nValue: Double); overload;
    procedure PackerDecode(const nStr: string; var nValue: TDateTime); overload;
    //打包函数
    procedure PackWorkerInfo(const nBuilder: TStrings; var nInfo: TBWWorkerInfo;
      const nPrefix: string; const nEncode: Boolean = True);
    //打包基数据
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    class function PackerName: string; virtual;
    //函数名
    procedure InitData(const nData: Pointer; const nIn: Boolean;
      const nSub: Boolean = True; const nBase: Boolean = True);
    //初始化
    function PackIn(const nData: Pointer; nCode: Boolean = True): string;
    procedure UnPackIn(const nStr: string; const nData: Pointer;
      nCode: Boolean = True);
    //入参处理
    function PackOut(const nData: Pointer; nCode: Boolean = True): string;
    procedure UnPackOut(const nStr: string; const nData: Pointer;
      nCode: Boolean = True);
    //出参处理
    property StrBuilder: TStrings read FStrBuilder;
    //属性相关
  end;

function PackerEncodeStr(const nStr: string): string;
function PackerDecodeStr(const nStr: string): string;
//字符编码

implementation

function PackerEncodeStr(const nStr: string): string;
begin
  Result := EncodeBase64(nStr);
end;

function PackerDecodeStr(const nStr: string): string;
begin
  Result := DecodeBase64(nStr);
end;

//------------------------------------------------------------------------------
constructor TBusinessPackerBase.Create;
begin
  FEnabled := True;
  FStrBuilder := TStringList.Create;
end;

destructor TBusinessPackerBase.Destroy;
begin
  FStrBuilder.Free;
  inherited;
end;

class function TBusinessPackerBase.PackerName: string;
begin
  Result := '';
end;

//Date: 2012-3-14
//Parm: 参数;入参;子类;基类
//Desc: 按需求初始化nData数据
procedure TBusinessPackerBase.InitData(const nData: Pointer;
 const nIn,nSub,nBase: Boolean);
var nBW: TBWDataBase;
begin
  if nBase then
  begin
    FillChar(nBW, SizeOf(nBW), #0);
    nBW.FMsgNO := PBWDataBase(nData).FMsgNO;
    nBW.FKey := PBWDataBase(nData).FKey;
    PBWDataBase(nData)^ := nBW;

    with PBWDataBase(nData)^ do
    begin
      FFrom.FTime := Now;
      FVia.FTime := Now;
      FFinal.FTime := Now;
      FResult := False;
    end;
  end;

  if nSub then
  begin
    if nIn then
         DoInitIn(nData)
    else DoInitOut(nData);
  end;
end;

//Date: 2012-3-7
//Parm: 参数数据;是否编码
//Desc: 对输入数据nData打包处理
function TBusinessPackerBase.PackIn(const nData: Pointer; nCode: Boolean): string;
begin
  FStrBuilder.Clear;
  FCodeEnable := nCode;

  DoPackIn(nData);
  Result := FStrBuilder.Text;
end;

//Date: 2012-3-7
//Parm: 字符数据;解码
//Desc: 对nStr拆包处理
procedure TBusinessPackerBase.UnPackIn(const nStr: string; const nData: Pointer;
  nCode: Boolean);
begin
  FStrBuilder.Text := nStr;
  FCodeEnable := nCode;
  DoUnPackIn(nData);
end;

//Date: 2012-3-7
//Parm: 结构数据;是否编码
//Desc: 对结构数据nData打包处理
function TBusinessPackerBase.PackOut(const nData: Pointer; nCode: Boolean): string;
begin
  FStrBuilder.Clear;
  FCodeEnable := nCode;

  DoPackOut(nData);
  Result := FStrBuilder.Text;
end;

//Date: 2012-3-7
//Parm: 字符数据
//Desc: 对nStr拆包处理
procedure TBusinessPackerBase.UnPackOut(const nStr: string;
 const nData: Pointer; nCode: Boolean);
begin
  FStrBuilder.Text := nStr;
  FCodeEnable := nCode;
  DoUnPackOut(nData);
end;

//Date: 2012-3-7
//Parm: 构建器;信息;前缀;是否编码
//Desc: 处理nInfo的信息
procedure TBusinessPackerBase.PackWorkerInfo(const nBuilder: TStrings;
  var nInfo: TBWWorkerInfo; const nPrefix: string; const nEncode: Boolean);
begin
  with nBuilder,nInfo do
  begin
    if nEncode  then
    begin
      Values[nPrefix + '_User']    := PackerEncode(FUser);
      Values[nPrefix + '_MAC']     := PackerEncode(FMAC);
      Values[nPrefix + '_IP']      := PackerEncode(FIP);
      Values[nPrefix + '_Time']    := PackerEncode(FTime);
      Values[nPrefix + '_KpLong']  := IntToStr(FKpLong);
    end else
    begin
      PackerDecode(Values[nPrefix + '_User'], FUser);
      PackerDecode(Values[nPrefix + '_IP'], FIP);
      PackerDecode(Values[nPrefix + '_MAC'], FMAC);
      PackerDecode(Values[nPrefix + '_Time'], FTime);
      PackerDecode(Values[nPrefix + '_KpLong'], FKpLong);
    end;
  end;
end;

//------------------------------------------------------------------------------
//Desc: 对nStr编码
function TBusinessPackerBase.PackerEncode(const nStr: string): string;
begin
  if FCodeEnable then
       Result := PackerEncodeStr(nStr)
  else Result := nStr;
end;

//Desc: 日期型
function TBusinessPackerBase.PackerEncode(const nDT: TDateTime): string;
begin
  with TDateTimeHelper do
  try
    Result := DateTime2Str(nDT);
  except
    Result := DateTime2Str(Now);
  end;
end;

//Desc: 布尔型
function TBusinessPackerBase.PackerEncode(const nVal: Boolean): string;
begin
  if nVal then
       Result := 'Y'
  else Result := 'N';
end;

//Desc: 字符串
procedure TBusinessPackerBase.PackerDecode(const nStr: string;
 var nValue: string);
begin
  if nStr = '' then
  begin
    nValue := '';
  end else

  if FCodeEnable then
       nValue := PackerDecodeStr(nStr)
  else nValue := nStr;
end;

//Desc: 布尔型
procedure TBusinessPackerBase.PackerDecode(const nStr: string;
 var nValue: Boolean);
begin
  nValue := nStr = 'Y';
end;

//Desc: 有符号整数
procedure TBusinessPackerBase.PackerDecode(const nStr: string;
 var nValue: Integer); 
begin
  if nStr = '' then
       nValue := 0
  else nValue := StrToInt(nStr)
end;

//Desc: 无符号整数
procedure TBusinessPackerBase.PackerDecode(const nStr: string;
 var nValue: Cardinal);
begin
  if nStr = '' then
       nValue := 0
  else nValue := StrToInt(nStr)
end;

//Desc: 64符号整数
procedure TBusinessPackerBase.PackerDecode(const nStr: string;
 var nValue: Int64);
begin
  if nStr = '' then
       nValue := 0
  else nValue := StrToIntDef(nStr, 0)
end;

//Desc: 浮点数
procedure TBusinessPackerBase.PackerDecode(const nStr: string;
 var nValue: Double);
begin
  if nStr = '' then
       nValue := 0
  else nValue := StrToFloat(nStr);
end;

//Desc: 日期
procedure TBusinessPackerBase.PackerDecode(const nStr: string;
 var nValue: TDateTime);
begin
  with TDateTimeHelper do
  begin
    if nStr = '' then
         nValue := 0
    else nValue := Str2DateTime(nStr);
  end;
end;

//------------------------------------------------------------------------------  
procedure TBusinessPackerBase.DoInitIn(const nData: Pointer);
begin

end;

procedure TBusinessPackerBase.DoInitOut(const nData: Pointer);
begin

end;

procedure TBusinessPackerBase.DoPackIn(const nData: Pointer);
begin

end;

procedure TBusinessPackerBase.DoPackOut(const nData: Pointer);
begin

end;

procedure TBusinessPackerBase.DoUnPackIn(const nData: Pointer);
begin

end;

procedure TBusinessPackerBase.DoUnPackOut(const nData: Pointer);
begin

end;

end.


