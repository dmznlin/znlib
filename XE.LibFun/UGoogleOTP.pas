{*******************************************************************************
  作者: dmzn@163.com 2020-10-16
  描述: 基于Google Authenticator的动态口令

  描述:
  *.A simple unit to provide calculation and verification of Time Based One Time
    Passwords (TOTP) compatile with Google Authenticator.
*******************************************************************************}
unit UGoogleOTP;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.Math,
  IdGlobal, IdHMACSHA1, SZCodeBaseX, UManagerGroup, ULibFun;

type
  TGoogleOTP = record
  public
    const
      OTPLength = 6;
      // How many digits are there in a TOTP. Google Authenticator uses 6 digits.
      KeyRegeneration = 30;
      // Time step for TOTP generation. Google Authenticator uses 30 seconds.
    type
      TAuthority = (totp, hotp);
      //Time-based One-Time Password,HMAC-based One-Time Password
  public
    class procedure InitOPT; static;
    //init environment
    class function EncodeBase32(const nSecret: string): string; static;
    class function DecodeBase32(const nSecret: string): string; static;
    //base32 coding
    class function MakeSecret(nLen: Integer = OTPLength;
      const nEncode: Boolean = True): string; static;
    //make secret for calculate
    class function Calculate(const nSecret: string; const nCounter: Integer = -1;
      const nAcceptTime: PInteger = nil): Integer; static;
    class function CalAsString(const nSecret: string;
      const nCounter: Integer = -1): string; static;
    //Calculates the TOTP for a given secret and time segment
    class function Validate(const nSecret: string; const nToken: integer;
      const nWindowSize: integer = 3): boolean; static;
    //Checks if the provided TOTP (Token) is valid
    class function TimeRemain(const nAcceptTime: Integer): Integer; static;
    //the remaining validity time in seconds of the created password
    class function MakeURI(const nAccount,nSecret:string;
      const nParams: string = '';
      const nAuthority: TAuthority = totp): string; static;
    //QR-Code URI
  end;

implementation

//Date: 2020-10-17
//Parm: 密钥;数据
//Desc: 使用nKey编码nData
function HMACSHA1(const nKey: TIdBytes; const nData: TIdBytes): TIdBytes;
var nSHA: TIdHMACSHA1;
begin
  nSHA := nil;
  try
    nSHA := gMG.FObjectPool.Lock(TIdHMACSHA1) as TIdHMACSHA1;
    with nSHA do
    begin
      Key := nKey;
      Result := HashValue(nData);
    end;
  finally
    gMG.FObjectPool.Release(nSHA);
  end;
end;

//Date: 2020-10-17
//Parm: 时间戳
//Desc: 编码时间戳
function Time2Bytes(const nTime: Int64): TIdBytes;
var nIdx,nHigh: Integer;
    nBuf: TIdBytes;
begin
  nBuf := ToBytes(nTime);
  SetLength(Result, Length(nBuf));
  nHigh := High(nBuf);

  for nIdx := Low(nBuf) to nHigh do
    Result[nHigh - nIdx] := nBuf[nIdx];
  //Reverses TIdBytes (from low->high to high->low)
end;

class procedure TGoogleOTP.InitOPT;
begin
  gMG.FObjectPool.NewClass(TIdHMACSHA1,
    function(var nData: Pointer): TObject
    begin
      Result := TIdHMACSHA1.Create;
    end);
  //xxxxxx
end;

//Date: 2020-10-22
//Parm: 长度;是否编码
//Desc: 随机生成长度为nLen的secret key
class function TGoogleOTP.MakeSecret(nLen: Integer;
  const nEncode: Boolean): string;
const
  cValidChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ23456789';
  cValidLenth = Length(cValidChars);

var nIdx: Integer;
begin
  if nLen < 1 then nLen := 1
  else
  if nLen > 32 then nLen := 32;

  SetLength(Result, nLen);
  while nLen > 0 do
  begin
    nIdx := Random(cValidLenth + 1);
    if nIdx < 1 then nIdx := 1
    else
    if nIdx > cValidLenth then nIdx := cValidLenth;

    Result[nLen] := cValidChars[nIdx];
    //fill char
    Dec(nLen);
  end;

  if nEncode then
    Result := SZEncodeBase32(Result);
  //xxxxx
end;

//Date: 2020-10-22
//Parm: 待编码key
//Desc: 编码nSecret
class function TGoogleOTP.EncodeBase32(const nSecret: string): string;
begin
  Result := SZEncodeBase32(nSecret);
end;

//Date: 2020-10-22
//Parm: 待解码key
//Desc: 解码nSecret
class function TGoogleOTP.DecodeBase32(const nSecret: string): string;
begin
  Result := SZDecodeBase32(nSecret);
end;

{
  Calculates the TOTP for a given secret and time segment. You need to
  format the integer into six digit representation.

  @param(Secret is the Base32-encoded secret key for TOTP calculation)
  @param(Counter is the 30-second time segment since the start of the UNIX epoch
    you want to calculate an OTP for)
  @returns(The TOTP as an integer)
}
class function TGoogleOTP.Calculate(const nSecret: string;
  const nCounter: Integer; const nAcceptTime: PInteger): Integer;
var nSKey,nHash: string;
    nTime,nIKey: Integer;
    nOffset,nP1,nP2,nP3,nP4: Integer;
begin
  if nCounter <> -1 then
       nTime := nCounter
  else nTime := DateTimeToUnix(Now(), False) div KeyRegeneration;

  if Assigned(nAcceptTime) then
    nAcceptTime^ := nTime;
  //xxxxx

  nSKey := SZDecodeBase32(nSecret);
  nHash := BytesToStringRaw(HMACSHA1(ToBytes(nSKey, IndyTextEncoding_ASCII), Time2Bytes(nTime)));

  nOffset := (Ord(nHash[20]) and $0F) + 1;
  nP1 := (Ord(nHash[nOffset + 0]) and $7F);
  nP2 := (Ord(nHash[nOffset + 1]) and $FF);
  nP3 := (Ord(nHash[nOffset + 2]) and $FF);
  nP4 := (Ord(nHash[nOffset + 3]) and $FF);

  nIKey := (nP1 shl 24) or (nP2 shl 16) or (nP3 shl 8) or (nP4);
  Result := nIKey mod Trunc(IntPower(10, OTPLength));
end;

class function TGoogleOTP.CalAsString(const nSecret: string;
  const nCounter: Integer): string;
begin
  Result := Format('%.6d', [Calculate(nSecret, nCounter)]);
end;

{
  Checks if the provided TOTP (Token) is valid by comparing it against a freshly
  generated TOTP using the Base32-encoded Secret. Since there is time drift
  between devices, application latency and the user taking their time to enter
  the code and submit it to the application we use a time window of +/- WindowSize
  time steps. A WindowSize of -1 means that a Token is accepted if it matches
  the TOTP of the previous time step (30 seconds ago), now and the next time
  step (30 seconds in the future). You are advised not to increase the
  WindowSize over 2. The bigger the WindowSize the easier a replay attack becomes.

  @param(Secret is the Base32-encoded secret key for TOTP calculation)
  @param(Token is the TOTP you want to validate)
  @param(WindowSize is the time window you consider a TOTP valid)
  @return(@true if Token is a valid TOTP, @false otherwise)
}
class function TGoogleOTP.Validate(const nSecret: string; const nToken,
  nWindowSize: Integer): boolean;
var nTime: Integer;
    nValue: Integer;
begin
  Result := False;
  nTime := DateTimeToUnix(Now(), False) div KeyRegeneration;

  for nValue := nTime - nWindowSize to nTime + nWindowSize do
  if Calculate(nSecret, nValue) = nToken then
  begin
    Result := True;
    Break;
  end;
end;

//Date: 2020-10-17
//Parm: 上次计算密码时的时间戳
//Desc: 计算nAcceptTime的剩余有效时长,单位秒
class function TGoogleOTP.TimeRemain(const nAcceptTime: Integer): Integer;
var nUnix: Int64;
    nTime: Integer;
begin
  nUnix := DateTimeToUnix(Now(), False);
  nTime := nUnix div KeyRegeneration;

  if nTime = nAcceptTime then
       Result := KeyRegeneration - nUnix mod KeyRegeneration
  else Result := 0;
end;

//Date: 2020-10-25
//Parm: 账户名称;密钥;附加参数;算法类型
//Desc: 生成Google Authenticator能识别的二维码内容
class function TGoogleOTP.MakeURI(const nAccount, nSecret, nParams: string;
  const nAuthority: TAuthority): string;
begin
  Result := 'otpauth://$auth/$account?secret=$secret';
  with TStringHelper do
  begin
    if nAuthority = hotp then
         Result := MacroValue(Result, [MI('$auth', 'hotp')])
    else Result := MacroValue(Result, [MI('$auth', 'totp')]);

    if nAccount = '' then
         Result := MacroValue(Result, [MI('$account', 'user@here')])
    else Result := MacroValue(Result, [MI('$account', nAccount)]);

    if nSecret = '' then
         Result := MacroValue(Result, [MI('$secret', MakeSecret())])
    else Result := MacroValue(Result, [MI('$secret', nSecret)]);

    if nParams <> '' then
      Result := Result + '&' + nParams;
    //xxxxx
  end;
end;

end.
