{*******************************************************************************
  作者: dmzn@163.com 2014-10-24
  描述: 深圳市中科华益科技有限公司 RFID102读取器驱动
*******************************************************************************}
unit UMgrRFID102;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, NativeXml, UWaitItem, 
  USysLoger, IdTCPClient, IdGlobal, ULibFun;

const
  cHYReader_Wait_Short     = 5;
  cHYReader_Wait_Long      = 2 * 1000;

type
  TReadCmdType = (
    tCmd_Err_Cmd                  = $00,
    //未识别命令
    
    tCmd_G2_Seek                  = $01,
    tCmd_G2_ReadData              = $02,
    tCmd_G2_WriteData             = $03,
    tCmd_G2_WriteEPCID            = $04,
    tCmd_G2_Destory               = $05,
    tCmd_G2_SetMemRWProtect       = $06,
    tCmd_G2_EreaseArea            = $07,
    tCmd_G2_InstalReadProtect     = $08,
    tCmd_G2_SetReadProtect        = $09,
    tCmd_G2_UnlockRProtect        = $0A,
    tCmd_G2_ChargeRProtect        = $0B,
    tCmd_G2_SetEASWarn            = $0C,
    tCmd_G2_ChargeEASWarn         = $0D,
    tCmd_G2_UseAreaLock           = $0E,
    tCmd_G2_SeekSingle            = $0F,
    tCmd_G2_WriteArea             = $10,
    //以上EPC C1G2命令 范围0x01-0x10
    //1	0x01	询查标签
    //2	0x02	读数据
    //3	0x03	写数据
    //4	0x04	写EPC号
    //5	0x05	销毁标签
    //6	0x06	设定存储区读写保护状态
    //7	0x07	块擦除
    //8	0x08	根据EPC号设定读保护设置
    //9	0x09	不需要EPC号读保护设定
    //10	0x0a	解锁读保护
    //11	0x0b	测试标签是否被设置读保护
    //12	0x0c	EAS报警设置
    //13	0x0d	EAS报警探测
    //14	0x0e	user区块锁
    //15	0x0f	询查单标签
    //16	0x10	块写

    
    tCmd_6B_SeekSingle             = $50,
    tCmd_6B_SeekMulti              = $51,
    tCmd_6B_ReadData               = $52,
    tCmd_6B_WriteData              = $53,
    tCmd_6B_ChargeLock             = $54,
    tCmd_6B_Lock                   = $55,
    //以上1800-68命令 范围0x50-0x55
    //1	0x50	询查命令(单张)。这个命令每次只能询查一张电子标签。不带条件询查。
    //2	0x51	条件询查命令(多张)。这个命令根据给定的条件进行询查标签，返回符合条件的电子标签的UID。可以同时询查多张电子标签。
    //3	0x52	读数据命令。这个命令读取电子标签的数据，一次最多可以读32个字节。
    //4	0x53	写数据命令。写入数据到电子标签中，一次最多可以写32个字节。
    //5	0x54	检测锁定命令。检测某个存储单元是否已经被锁定。
    //6	0x55	锁定命令。锁定某个尚未被锁定的电子标签。
    
    
    tCmd_Reader_ReadInfo              = $21,
    tCmd_Reader_SetWorkrate           = $22,
    tCmd_Reader_SetAddr               = $24,
    tCmd_Reader_SetSeekTimeOut        = $25,
    tCmd_Reader_SetBoundrate          = $28,
    tCmd_Reader_SetOutweight          = $2F,
    tCmd_Reader_SetRoundAndRight      = $33,
    tCmd_Reader_SetWGParam            = $34,
    tCmd_Reader_SetWorkmode           = $35,
    tCmd_Reader_ReadWorkmode          = $36,
    tCmd_Reader_SetEASweight          = $37,
    tCmd_Reader_SetSyris485TimeOut    = $38,
    tCmd_Reader_SetReplyTimeOut       = $3B
    //读写器自定义命令

    //1	0x21	读取读写器信息
    //2	0x22	设置读写器工作频率
    //3	0x24	设置读写器地址
    //4	0x25	设置读写器询查时间
    //5	0x28	设置读写器的波特率
    //6	0x2F	调整读写器输出功率
    //7	0x33	声光控制命令
    //8	0x34	韦根参数设置命令
    //9	0x35	工作模式设置命令
    //10	0x36	读取工作模式参数命令
    //11	0x37	EAS测试精度设置命令
    //12	0x38	设置Syris485响应偏执时间
    //13	0x3b	设置触发有效时间
  );

  PRFIDReaderCmd = ^TRFIDReaderCmd;
  TRFIDReaderCmd = record
    FLen :Char;
    //指令长度命令数据块的长度，但不包括Len本身。
    //即数据块的长度等于4加Data[]的长度。Len允许的最大值为96，最小值为4

    FAddr:Char;
    //读写器地址。地址范围：0x00~0xFE，0xFF为广播地址，
    //读写器只响应和自身地址相同及地址为0xFF的命令。读写器出厂时地址为0x00

    FCmd :TReadCmdType;
    //命令代码。

    FStatus: Char;
    //命令执行结果状态值。

    FData:string;
    //参数域。在实际命令中，可以不存在。

    FLSB, FMSB:Char;
    //CRC16低字节和高字节。CRC16是从Len到Data[]的CRC16值
  end;

  TRFIDReaderClass = class(TObject)
  private 
    function Crc16Calc(const nStrSrc: string;
      const nStart,nEnd: Integer; nCrcValue: Word=$FFFF;
      nGenPoly: Word=$8408): Word;
    //华益电子标签Crc16算法

    function AsciConvertBuf(const nTxt: string; var nBuf: TIdBytes): Integer;
    function BufConvertAsci(var nTxt: string; const nBuf: TIdBytes): Integer;
  public    
    function PackSendData(var nStrDest: string; nItem:TRFIDReaderCmd): Boolean;
    function UnPackRecvData(var nItem:TRFIDReaderCmd; nStrSrc: string): Boolean;
    //封装与解析电子标签协议
  end;

//------------------------------------------------------------------------------

  PHYReaderItem = ^THYReaderItem;
  THYReaderItem = record
    FID     : string;          //读头标识
    FHost   : string;          //地址
    FPort   : Integer;         //端口

    FCard   : string;          //卡号
    FTunnel : string;          //通道号
    FEnable : Boolean;         //是否启用
    FLocked : Boolean;         //是否锁定
    FLastActive: Int64;        //上次活动

    FClient : TIdTCPClient;       //通信方式
  end;

  THYReaderManager = class;
  THYRFIDReader = class(TThread)
  private
    FOwner: THYReaderManager;
    //拥有者
    FWaiter: TWaitObject;
    //等待对象
    FActiveReader: PHYReaderItem;
    //当前读头

    FSendItem, FRecvItem: TRFIDReaderCmd;
    //发送指令，返回指令
    FRFIDReader: TRFIDReaderClass;
    //电子标签读卡器对象
    
    FEPCList: TStrings;
    //电子标签
  protected
    procedure DoExecute;
    procedure Execute; override;
    function ReadCard(const nReader: PHYReaderItem): Boolean;
    //执行线程
  public
    constructor Create(AOwner: THYReaderManager);
    destructor Destroy; override;
    //创建释放
    procedure StopMe;
    //停止线程
  end;

//------------------------------------------------------------------------------

  THYReaderProc = procedure (const nItem: PHYReaderItem);
  THYReaderEvent = procedure (const nItem: PHYReaderItem) of Object;

  THYReaderManager = class(TObject)
  private
    FEnable: Boolean;
    //是否启用
    FReaderIndex: Integer;
    FReaders: TList;
    //读头列表
    FThreads: array[0..2] of THYRFIDReader;
    //读卡对象
    FSyncLock: TCriticalSection;
    //同步锁定
    FOnProc: THYReaderProc;
    FOnEvent: THYReaderEvent;
    //事件定义

    FThreadCount: Int64;
    //读卡线程数
  protected
    procedure ClearReaders(const nFree: Boolean);
    //清理资源
    procedure CloseReader(const nReader: PHYReaderItem);
    //关闭读头
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure LoadConfig(const nFile: string);
    //载入配置
    procedure StartReader;
    procedure StopReader;
    //启停读头
    property OnCardProc: THYReaderProc read FOnProc write FOnProc;
    property OnCardEvent: THYReaderEvent read FOnEvent write FOnEvent;
    //属性相关
  end;

var
  gHYReaderManager: THYReaderManager = nil;
  //全局使用
  
implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(THYReaderManager, '华益RFID读卡器', nEvent);
end;
//------------------------------------------------------------------------------
constructor THYReaderManager.Create;
var nIdx: Integer;
begin
  for nIdx:=Low(FThreads) to High(FThreads) do
    FThreads[nIdx] := nil;
  //xxxxx
  
  FEnable := False;
  FReaders := TList.Create;
  FSyncLock := TCriticalSection.Create;
end;

destructor THYReaderManager.Destroy;
begin
  StopReader;
  ClearReaders(True);

  FSyncLock.Free;
  inherited;
end;

procedure THYReaderManager.ClearReaders(const nFree: Boolean);
var nIdx: Integer;
begin
  for nIdx:=FReaders.Count - 1 downto 0 do
  begin
    Dispose(PHYReaderItem(FReaders[nIdx]));
    FReaders.Delete(nIdx);
  end;

  if nFree then
    FReaders.Free;
  //xxxxx
end;

procedure THYReaderManager.StartReader;
var nIdx,nNum: Integer;
begin
  if not FEnable then Exit;
  nNum := 0;
  FReaderIndex := 0;

  for nIdx:=Low(FThreads) to High(FThreads) do
   if Assigned(FThreads[nIdx]) then
    Inc(nNum);
  //xxxxx

  for nIdx:=Low(FThreads) to High(FThreads) do
  begin
    if (nNum > 0) and (FReaders.Count < 2) then Exit;
    //一个读头单线程
    if nNum >= FThreadCount then Exit;
    //线程不能超过预定值

    if not Assigned(FThreads[nIdx]) then
    begin
      FThreads[nIdx] := THYRFIDReader.Create(Self);
      Inc(nNum);
    end;
  end;
end;

procedure THYReaderManager.CloseReader(const nReader: PHYReaderItem);
begin
  if Assigned(nReader) and Assigned(nReader.FClient) then
  begin 
    nReader.FClient.Disconnect;
    if Assigned(nReader.FClient.IOHandler) then
      nReader.FClient.IOHandler.InputBuffer.Clear;
  end;
end;

procedure THYReaderManager.StopReader;
var nIdx: Integer;
begin
  for nIdx:=Low(FThreads) to High(FThreads) do
   if Assigned(FThreads[nIdx]) then
    FThreads[nIdx].Terminate;
  //设置退出标记

  for nIdx:=Low(FThreads) to High(FThreads) do
  if Assigned(FThreads[nIdx]) then
  begin
    FThreads[nIdx].StopMe;
    FThreads[nIdx] := nil;
  end;

  FSyncLock.Enter;
  try
    for nIdx:=FReaders.Count - 1 downto 0 do
      CloseReader(FReaders[nIdx]);
    //关闭读头
  finally
    FSyncLock.Leave;
  end;
end;

procedure THYReaderManager.LoadConfig(const nFile: string);
var nIdx: Integer;
    nXML: TNativeXml;
    nNode,nTmp: TXmlNode;
    nReader: PHYReaderItem;
begin
  FEnable := False;
  if not FileExists(nFile) then Exit;

  nXML := nil;
  try
    nXML := TNativeXml.Create;
    nXML.LoadFromFile(nFile);

    nNode := nXML.Root.FindNode('readers');
    if not Assigned(nNode) then Exit;
    ClearReaders(False);

    nTmp := nNode.FindNode('threadcount');
    if Assigned(nTmp) then
         FThreadCount := nTmp.ValueAsInteger
    else FThreadCount := 1;

    for nIdx:=0 to nNode.NodeCount - 1 do
    begin
      nTmp := nNode.Nodes[nIdx];
      if CompareText(nTmp.Name, 'reader') <> 0 then Continue;

      New(nReader);
      FReaders.Add(nReader);

      with nTmp,nReader^ do
      begin
        FLocked := False;
        FLastActive := GetTickCount;

        FID := AttributeByName['id'];
        FHost := NodeByName('ip').ValueAsString;
        FPort := NodeByName('port').ValueAsInteger;
        FEnable := NodeByName('enable').ValueAsString <> 'N';

        if FEnable then
          Self.FEnable := True;
        //有效节点

        nTmp := FindNode('tunnel');
        if Assigned(nTmp) then
          FTunnel := nTmp.ValueAsString;
        //通道号

        FClient := TIdTCPClient.Create;
        with FClient do
        begin
          Host := FHost;
          Port := FPort;

          ConnectTimeout := cHYReader_Wait_Long;   
        end;  
      end;
    end;
  finally
    nXML.Free;
  end;
end;

//------------------------------------------------------------------------------
constructor THYRFIDReader.Create(AOwner: THYReaderManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FWaiter := TWaitObject.Create;
  FWaiter.Interval := cHYReader_Wait_Short;

  FEPCList:=TStringList.Create;
  FRFIDReader := TRFIDReaderClass.Create;
end;

destructor THYRFIDReader.Destroy;
begin
  FreeAndNil(FEPCList);
  FreeAndNil(FRFIDReader);
  FWaiter.Free;
  inherited;
end;

procedure THYRFIDReader.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure THYRFIDReader.Execute;
begin
  while not Terminated do
  try
    FWaiter.EnterWait;
    if Terminated then Exit;

    DoExecute;
    //执行读卡
  except
    on E: Exception do
    begin
      WriteLog(E.Message);
      Sleep(500);
    end;
  end;
end;

procedure THYRFIDReader.DoExecute;
var nIdx: Integer;
    nStr: string;
    nReader: PHYReaderItem;
begin
  FActiveReader := nil;
  //init

  with FOwner do
  try
    FSyncLock.Enter;
    try
      if FThreadCount>1 then  //启动多个线程时，有卡号的读头优先
      for nIdx:=FReaders.Count - 1 downto 0 do
      begin
        nReader := FReaders[nIdx];
        if nReader.FEnable and (not nReader.FLocked) and
           (GetTickCount - nReader.FLastActive < cHYReader_Wait_Long) then
        //有卡号的读头优先
        begin
          FActiveReader := nReader;
          FActiveReader.FLocked := True;
          Break;
        end;
      end;

      if not Assigned(FActiveReader) then
      begin
        nIdx := 0;
        //init

        while True do
        begin
          if FReaderIndex >= FReaders.Count then
          begin
            FReaderIndex := 0;
            Inc(nIdx);

            if nIdx > 1 then Break;
            //扫描一轮,无效退出
          end;

          nReader := FReaders[FReaderIndex];
          Inc(FReaderIndex);
          if nReader.FLocked or (not nReader.FEnable) then Continue;

          FActiveReader := nReader;
          FActiveReader.FLocked := True;
          Break;
        end;
      end;
    finally
      FSyncLock.Leave;
    end;

    if Assigned(FActiveReader) and (not Terminated) then
    try
      if ReadCard(FActiveReader) then
      begin
        FWaiter.Interval := cHYReader_Wait_Short;
        FActiveReader.FLastActive := GetTickCount;
      end else
      begin
        if (FActiveReader.FLastActive > 0) and
           (GetTickCount - FActiveReader.FLastActive >= 3 * 1000) then
        begin
          FActiveReader.FLastActive := 0;

          if FThreadCount>1 then
            FWaiter.Interval := cHYReader_Wait_Long;
          //多个读卡线程时，读卡超时后，延长时间
        end;
      end;
    except
      on E: Exception do
      begin
        nStr := '读卡器[%s:%s.%d]操作失败，错误信息[%s]';
        nStr := Format(nStr, [FActiveReader.FID, FActiveReader.FHost,
                FActiveReader.FPort, E.Message]);
        //xxxx
                                     
        CloseReader(FActiveReader);
        raise Exception.Create(nStr);
      end;
    end;
  finally
    if Assigned(FActiveReader) then
      FActiveReader.FLocked := False;
    //unlock
  end;
end;

function getStr(pStr: pchar; len: Integer): string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to len - 1 do
    result := result + (pStr + i)^;
end;

function getHexStr(sBinStr: string): string; //获得十六进制字符串
var
  i: Integer;
begin
  result := '';
  for i := 1 to Length(sBinStr) do
    result := result + IntToHex(ord(sBinStr[i]), 2);
end;

function THYRFIDReader.ReadCard(const nReader: PHYReaderItem): Boolean;
var nSendData, nRecvData,nEPC: string;
    nBuf, nRecv: TIdBytes;
    nStart, nLen: Integer;
    nInt, nIdx: Integer;   
begin
  Result := False;

  FOwner.FSyncLock.Enter;
  try
    with FRFIDReader, nReader^ do
    begin
      with FSendItem do
      begin
        FCmd  := tCmd_G2_Seek;
        FAddr := Chr($FF);
        FData := '' ;
      end;  

      if not PackSendData(nSendData, FSendItem) then Exit;

      try
        if not FClient.Connected then
          FClient.Connect;

        AsciConvertBuf(nSendData, nBuf);
        FClient.IOHandler.Write(nBuf);

        Sleep(150);
        //Wait for

        FClient.IOHandler.CheckForDataOnSource;
        if FClient.IOHandler.InputBufferIsEmpty then  Exit;
        //No Data Recv

        nInt := FClient.IOHandler.InputBuffer.Size;
        FClient.IOHandler.ReadBytes(nRecv, nInt, False);
        BufConvertAsci(nRecvData, nRecv);

        if not UnPackRecvData(FRecvItem, nRecvData) then Exit;
        //Unpack Error

        if FRecvItem.FCmd <> FSendItem.FCmd then Exit;
        //not sample cmd

        if (FRecvItem.FStatus <> #01) and (FRecvItem.FStatus <> #02)
        and (FRecvItem.FStatus <> #03) and (FRecvItem.FStatus <> #04)
        then Exit;

        nStart:=1;
        nInt := Ord(FRecvItem.FData[1]);
        for nIdx:=0 to nInt-1 do
        begin
          nLen := Ord(FRecvItem.FData[nStart+1]);
          nEPC := getHexStr(Copy(FRecvItem.FData, nStart+2, nLen));

          nStart := nStart + nLen + 1;
          FEPCList.Add(nEPC);
        end;  
      except
        on E: Exception do
        begin
          raise;
        end;
      end;
    end;
    
    if Terminated then Exit;
    //thread exit

    nReader.FCard := CombinStr(FEPCList, ',', False);
    FEPCList.Clear;
    //xxxxx
  finally
    FOwner.FSyncLock.Leave;
  end;

  if Assigned(FOwner.FOnProc) then
    FOwner.FOnProc(nReader);
  //xxxxx

  if Assigned(FOwner.FOnEvent) then
    FOwner.FOnEvent(nReader);
  //xxxxx

  Result := True;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//Date: 2015/7/8
//Parm: 目标字符串；发送数据格式
//Desc: 华益通信协议封装
function TRFIDReaderClass.PackSendData(var nStrDest: string;
    nItem:TRFIDReaderCmd):Boolean;
var nCRC: Word;
    nTmpSend: string;
    nTmpCmd: TRFIDReaderCmd;
begin
  nTmpSend := '';
  nTmpCmd  := nItem;

  with nTmpCmd do
  begin
    FLen := Chr(4 + Length(FData));

    nTmpSend := FLen + FAddr + Chr(Ord(FCmd)) + FData;
    nCRC := Crc16Calc(nTmpSend, 1, Length(nTmpSend));

    FLSB := Chr(nCRC mod 256);
    FMSB := Chr(nCRC div 256);

    nTmpSend := nTmpSend + FLSB + FMSB;
  end;

  nStrDest := nTmpSend;
  Result := True;
end;
//------------------------------------------------------------------------------
//Date: 2015/7/8
//Parm: 目标协议结构；原始字符串
//Desc: 华益通信协议解析
function TRFIDReaderClass.UnPackRecvData(var nItem:TRFIDReaderCmd;
  nStrSrc: string): Boolean;
var nLen, nLenSrc: Integer;
    nRecvCRC: Word;
begin
  Result := False;

  nLenSrc:=Length(nStrSrc);
  if nLenSrc<0 then Exit;
  //数据为空

  nLen := Ord(nStrSrc[1]);
  if nLen>(nLenSrc-1) then Exit;
  //数据未接收完全

  nRecvCRC := Crc16Calc(nStrSrc, 1, nLen-1);
  if (Ord(nStrSrc[nLen]) <> (nRecvCRC mod 256)) or
     (Ord(nStrSrc[nLen+1]) <> (nRecvCRC div 256)) then Exit;
  //CRC Error

  with nItem do
  begin
    FLen      := Chr(nLen);
    FAddr     := nStrSrc[2];
    FCmd      := TReadCmdType(Ord(nStrSrc[3]));
    FStatus   := nStrSrc[4];

    FData     := Copy(nStrSrc, 5, nLen-5);
    FLSB      := nStrSrc[nLen];
    FMSB      := nStrSrc[nLen + 1];
  end;

  if (nItem.FCmd=tCmd_Err_Cmd) then Exit;
  //Err Command type
  
  Result := True;
end;

//Date: 2015/2/8
//Parm: 字符串信息;字符数组
//Desc: 字符串转数组
function TRFIDReaderClass.AsciConvertBuf(const nTxt: string;
  var nBuf: TIdBytes): Integer;
var nIdx: Integer;
    nC: char;
begin
  Result := 0;
  SetLength(nBuf, Length(nTxt));
  //xxxxx

  for nIdx:=1 to Length(nTxt) do
  begin
    nC := nTxt[nIdx];
    nBuf[Result] := Ord(nC);

    Inc(Result);
  end;
end;
//------------------------------------------------------------------------------
//Date: 2015/7/8
//Parm: 目标字符串;原始字符数组
//Desc: 数组转字符串
function TRFIDReaderClass.BufConvertAsci(var nTxt: string;
  const nBuf: TIdBytes): Integer;
var nIdx: Integer;
begin
  Result := 0;
  nTxt   := '';

  for nIdx:=0 to Length(nBuf)-1 do
  begin
    nTxt := nTxt + Chr(nBuf[nIdx]);
    Inc(Result);
  end;
end;

//Date: 2015/6/19
//Parm: 原始数据(16进制);校验起始索引;校验终止索引；初始CRC；多项式
//Desc: 中科华益电子标签CRC16校验算法
function TRFIDReaderClass.Crc16Calc(const nStrSrc: string;
    const nStart,nEnd: Integer; nCrcValue: Word=$FFFF;
    nGenPoly: Word=$8408): Word;
var nIdx,nInt: Integer;
    nCrcTmp: Word;
begin
  Result := 0;
  if (nStart > nEnd) or (nEnd < 1) then Exit;

  nCrcTmp := nCrcValue;
  for nIdx:=nStart to nEnd do
  begin
    nCrcTmp := nCrcTmp xor Ord(nStrSrc[nIdx]);

    for nInt:=0 to 7 do
    if (nCrcTmp and $0001)<>0 then
         nCrcTmp := (nCrcTmp shr 1) xor nGenPoly
    else nCrcTmp := nCrcTmp shr 1;
  end;

  Result := nCrcTmp;
end;

initialization
  gHYReaderManager := nil;
finalization
  FreeAndNil(gHYReaderManager);
end.
