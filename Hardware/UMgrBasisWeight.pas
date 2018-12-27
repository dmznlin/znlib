{*******************************************************************************
  作者: dmzn@163.com 2018-12-26
  描述: 基于地磅的定量装车业务
*******************************************************************************}
unit UMgrBasisWeight;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, UMgrPoundTunnels, ULibFun,
  UWaitItem, USysLoger;

type
  TBWStatus = (bsInit, bsNew, bsStart, bsProcess, bsStable, bsDone, bsClose);
  //状态: 初始化;新添加;开始;装车中;平稳;完成;关闭

  PBWTunnel = ^TBWTunnel;
  TBWTunnel = record
    FID           : string;            //通道标识
    FBill         : string;            //交货单据
    FValue        : Double;            //待装量
    FValHas       : Double;            //已装量
    FValMax       : Double;            //最大数据
    FValTunnel    : Double;            //通道数据
    FValUpdate    : Int64;             //通道更新

    FStatusNow    : TBWStatus;         //当前状态
    FStatusNew    : TBWStatus;         //新状态
    FStableDone   : Boolean;           //平稳状态

    //TimeOut
    FTONoData     : Integer;           //长时间无数据
    FTONoWeight   : Integer;           //长时间未上磅
    FInitWeight   : Int64;             //开始业务计时
    FTOProceFresh : Integer;           //装车进度刷新
    FInitFresh    : Int64;             //进度刷新计时
    FValFresh     : Double;            //进度刷新数据

    FTunnel       : PPTTunnelItem;     //通道参数
    FParams       : TStrings;          //参数项
    FSampleIndex  : Integer;           //采样索引
    FValSamples   : array of Double;   //数据采样
  end;

  TBasisWeightManager = class;
  TBasisWeightWorker = class(TThread)
  private
    FOwner: TBasisWeightManager;
    //拥有者
    FActive: PBWTunnel;
    //当前通道
    FWaiter: TWaitObject;
    //等待对象
  protected
    procedure DoExecute;
    procedure Execute; override;
    //执行业务
    function IsValidSamaple(const nCheckMin: Boolean): Boolean;
    //验证采样
  public
    constructor Create(AOwner: TBasisWeightManager);
    destructor Destroy; override;
    //创建释放
    procedure WakupMe;
    //唤醒线程
    procedure StopMe;
    //停止线程
  end;

  TBWStatusChange = procedure (const nTunnel: PBWTunnel);
  TBWStatusChangeEvent = procedure (const nTunnel: PBWTunnel) of object;
  //事件定义

  TBasisWeightManager = class(TObject)
  private
    FTunnelManager: TPoundTunnelManager;
    FTunnels: TList;
    //通道列表
    FWorker: TBasisWeightWorker;
    //扫描对象
    FChangeProc: TBWStatusChange;
    FchangeEvent: TBWStatusChangeEvent;
    //事件定义
    FSyncLock: TCriticalSection;
    //同步锁定
  protected
    procedure ClearTunnels(const nFree: Boolean);
    //清理通道
    procedure OnTunnelData(const nValue: Double; const nPort: PPTPortItem);
    //通道数据
    procedure InitTunnelData(const nTunnel: PBWTunnel; nSimpleOnly: Boolean);
    //初始化
    function FindTunnel(const nTunnel: string; nLoged: Boolean): Integer;
    //检索通道
    procedure DoChangeEvent(const nTunnel: PBWTunnel; nNewStatus: TBWStatus);
    //响应事件
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure LoadConfig(const nFile: string);
    //读取配置
    procedure StartService;
    procedure StopService;
    //起停服务
    function IsTunnelBusy(const nTunnel: string): Boolean;
    //通道忙
    procedure StartWeight(const nTunnel,nBill: string; const nValue: Double;
     const nHasVal: Double = 0; const nParams: string = '');
    procedure StopWeight(const nTunnel: string);
    //起停称重
    property OnStatusChange: TBWStatusChange read FChangeProc write FChangeProc;
    property OnStatusEvent: TBWStatusChangeEvent read FchangeEvent write FchangeEvent;
    //属性相关
  end;

var
  gBasisWeightManager: TBasisWeightManager = nil;
  //全局使用

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TBasisWeightManager, '定量装车管理', nEvent);
end;

constructor TBasisWeightManager.Create;
begin
  FWorker := nil;
  FTunnels := TList.Create;
  FSyncLock := TCriticalSection.Create;

  FTunnelManager := TPoundTunnelManager.Create;
  FTunnelManager.OnData := OnTunnelData;
end;

destructor TBasisWeightManager.Destroy;
begin
  if Assigned(FWorker) then
  begin
    FWorker.StopMe;
    FWorker := nil;
  end;

  ClearTunnels(True);
  FreeAndNil(FTunnelManager);
  FreeAndNil(FSyncLock);
  inherited;
end;

procedure TBasisWeightManager.ClearTunnels(const nFree: Boolean);
var nIdx: Integer;
    nTunnel: PBWTunnel;
begin
  for nIdx:=FTunnels.Count-1 downto 0 do
  begin
    nTunnel := FTunnels[nIdx];
    FreeAndNil(nTunnel.FParams);

    Dispose(nTunnel);
    FTunnels.Delete(nIdx);
  end;

  if nFree then
    FreeAndNil(FTunnels);
  //xxxxx
end;

procedure TBasisWeightManager.DoChangeEvent(const nTunnel: PBWTunnel;
 nNewStatus: TBWStatus);
begin
  try
    nTunnel.FStatusNew := nNewStatus;
    if Assigned(FChangeProc) then FChangeProc(nTunnel);
    if Assigned(FChangeEvent) then FchangeEvent(nTunnel);
  except
    on nErr: Exception do
    begin
      WriteLog(nErr.Message);
    end;
  end;

  nTunnel.FStatusNow := nNewStatus;
  //apply status
end;

procedure TBasisWeightManager.StartService;
var nIdx: Integer;
begin
  if FTunnels.Count < 1 then
    raise Exception.Create('TBasisWeightManager Need LoadConfig() First.');
  //xxxxx

  if not Assigned(FWorker) then
    FWorker := TBasisWeightWorker.Create(Self);
  //xxxxx

  for nIdx:=FTunnels.Count-1 downto 0 do
    FTunnelManager.ActivePort(PBWTunnel(FTunnels[nIdx]).FID, nil, True);
  //启动端口
end;

procedure TBasisWeightManager.StopService;
var nIdx: Integer;
begin
  if Assigned(FWorker) then
  begin
    FWorker.StopMe;
    FWorker := nil;
  end;

  for nIdx:=FTunnels.Count-1 downto 0 do
    FTunnelManager.ClosePort(PBWTunnel(FTunnels[nIdx]).FID);
  //启动端口
end;

//Desc: 初始化通道数据
procedure TBasisWeightManager.InitTunnelData(const nTunnel: PBWTunnel;
 nSimpleOnly: Boolean);
var nIdx: Integer;
begin
  if not nSimpleOnly then
  begin
    nTunnel.FBill := '';
    nTunnel.FParams.Clear;
    nTunnel.FStableDone := False;

    nTunnel.FValue := 0;
    nTunnel.FValHas := 0;
    nTunnel.FValMax := 0;
    nTunnel.FValTunnel := 0;
    nTunnel.FValUpdate := GetTickCount;

    nTunnel.FValFresh := -1;
    nTunnel.FInitFresh := 0;
    nTunnel.FInitWeight := GetTickCount;
  end;

  for nIdx:=Low(nTunnel.FValSamples) to High(nTunnel.FValSamples) do
    nTunnel.FValSamples[nIdx] := 0;
  nTunnel.FSampleIndex := 0;
end;

//Date: 2018-12-27
//Parm: 通道号;记录日志
//Desc: 检索nTunnel索引
function TBasisWeightManager.FindTunnel(const nTunnel: string;
 nLoged: Boolean): Integer;
var nIdx: Integer;
begin
  Result := -1;
  //default
  
  for nIdx:=FTunnels.Count-1 downto 0 do
  if CompareText(nTunnel, PBWTunnel(FTunnels[nIdx]).FID) = 0 then
  begin
    Result := nIdx;
    Break;
  end;

  if (Result < 0) and nLoged then
    WriteLog(Format('通道[ %s ]不存在.', [nTunnel]));
  //xxxxx
end;

//Date: 2018-12-27
//Parm: 通道号
//Desc: 判断nTunnel是否空闲
function TBasisWeightManager.IsTunnelBusy(const nTunnel: string): Boolean;
var nIdx: Integer;
    nPT: PBWTunnel;
begin
  Result := False;
  nIdx := FindTunnel(nTunnel, True);
  if nIdx < 0 then Exit;

  FSyncLock.Enter;
  try
    nPT := FTunnels[nIdx];
    Result := (nPT.FBill <> '') and
              (nPT.FValue > 0) and (nPT.FValue < nPT.FValHas);
    //未装完
  finally
    FSyncLock.Leave;
  end;   
end;

//Date: 2018-12-27
//Parm: 通道号;交货单;应装;已装;参数
//Desc: 开启新的称重业务
procedure TBasisWeightManager.StartWeight(const nTunnel, nBill: string;
  const nValue, nHasVal: Double; const nParams: string);
var nIdx: Integer;
    nPT: PBWTunnel;
begin
  nIdx := FindTunnel(nTunnel, True);
  if nIdx < 0 then Exit;

  FSyncLock.Enter;
  try
    nPT := FTunnels[nIdx];
    if nPT.FBill <> nBill then
    begin
      InitTunnelData(nPT, False);
      nPT.FBill := nBill;
      nPT.FValue := nValue;
    end;

    if nParams <> '' then
      SplitStr(nParams, nPT.FParams, 0, ';');
    DoChangeEvent(nPT, bsNew);
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2018-12-27
//Parm: 通道号
//Desc: 停止称重业务
procedure TBasisWeightManager.StopWeight(const nTunnel: string);
var nIdx: Integer;
begin
  nIdx := FindTunnel(nTunnel, True);
  if nIdx < 0 then Exit;

  FSyncLock.Enter;
  try
    InitTunnelData(FTunnels[nIdx], False);
    DoChangeEvent(FTunnels[nIdx], bsClose);
  finally
    FSyncLock.Leave;
  end;
end;

procedure TBasisWeightManager.LoadConfig(const nFile: string);
var nStr: string;
    nIdx: Integer;
    nTunnel: PBWTunnel;
begin
  if FTunnels.Count > 0 then
    ClearTunnels(False);
  FTunnelManager.LoadConfig(nFile);

  for nIdx:=FTunnelManager.Tunnels.Count-1 downto 0 do
  begin
    New(nTunnel);
    FTunnels.Add(nTunnel);
    nTunnel.FTunnel := FTunnelManager.Tunnels[nIdx];
    
    nTunnel.FID := nTunnel.FTunnel.FID;
    nTunnel.FParams := TStringList.Create;
    SetLength(nTunnel.FValSamples, nTunnel.FTunnel.FSampleNum);

    if Assigned(nTunnel.FTunnel.FOptions) then
    begin
      nStr := nTunnel.FTunnel.FOptions.Values['NoDataTimeOut'];
      if IsNumber(nStr, False) then
           nTunnel.FTONoData := StrToInt(nStr) * 1000
      else nTunnel.FTONoData := 10 * 1000;

      nStr := nTunnel.FTunnel.FOptions.Values['EmptyIdleLong'];
      if IsNumber(nStr, False) then
           nTunnel.FTONoWeight := StrToInt(nStr) * 1000
      else nTunnel.FTONoWeight := 60 * 1000;

      nStr := nTunnel.FTunnel.FOptions.Values['ProceFresh'];
      if IsNumber(nStr, False) then
           nTunnel.FTOProceFresh := StrToInt(nStr) * 1000
      else nTunnel.FTOProceFresh := 1 * 1000;
    end;

    InitTunnelData(nTunnel, False);
    nTunnel.FStatusNow := bsInit;
    DoChangeEvent(nTunnel, bsInit);
  end;
end;

//Date: 2018-12-26
//Parm: 通道值;通道端口
//Desc: 处理从nPort发送来的数据
procedure TBasisWeightManager.OnTunnelData(const nValue: Double;
  const nPort: PPTPortItem);
var nIdx: Integer;
    nTunnel: PBWTunnel;
begin
  FSyncLock.Enter;
  try
    for nIdx:=FTunnels.Count-1 downto 0 do
    begin
      nTunnel := FTunnels[nIdx];
      if nTunnel.FTunnel <> nPort.FEventTunnel then Continue;
      nTunnel.FValUpdate := GetTickCount;

      if (nPort.FMinValue > 0) and (nValue <= nPort.FMinValue) then
           nTunnel.FValTunnel := 0
      else nTunnel.FValTunnel := nValue;

      if nTunnel.FValTunnel > nTunnel.FValMax then
        nTunnel.FValMax := nTunnel.FValTunnel;
      Break;
    end;
  finally
    FSyncLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
constructor TBasisWeightWorker.Create(AOwner: TBasisWeightManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FWaiter := TWaitObject.Create;
  FWaiter.Interval := 200;
end;

destructor TBasisWeightWorker.Destroy;
begin
  FWaiter.Free;
  inherited;
end;

procedure TBasisWeightWorker.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure TBasisWeightWorker.WakupMe;
begin
  FWaiter.Wakeup;
end;

procedure TBasisWeightWorker.Execute;
var nIdx: Integer;
    nItv: Int64;
begin
  while not Terminated do
  try
    FWaiter.EnterWait;
    if Terminated then Exit;

    for nIdx:=FOwner.FTunnels.Count-1 downto 0 do
    try
      FOwner.FSyncLock.Enter;
      FActive := FOwner.FTunnels[nIdx];
      if (FActive.FBill = '') or (FActive.FValue <= 0) then Continue; //无业务

      nItv := GetTickCount - FActive.FInitFresh;
      if nItv >= FActive.FTOProceFresh then
      begin
        if FActive.FValFresh <> FActive.FValTunnel then
        begin
          if (FActive.FValFresh = 0) and (FActive.FValTunnel > 0) then
            FOwner.DoChangeEvent(FActive, bsStart);
          //开始称重

          FActive.FValFresh := FActive.FValTunnel;
          FOwner.DoChangeEvent(FActive, bsProcess);
        end;

        FActive.FInitFresh := GetTickCount;
        //刷新仪表数值
      end;

      nItv := GetTickCount - FActive.FInitWeight;
      if (FActive.FValMax <= 0) and (nItv >= FActive.FTONoWeight) then
      begin
        FOwner.DoChangeEvent(FActive, bsClose);
        FOwner.InitTunnelData(FActive, False);

        WriteLog(Format('通道[ %s.%s ]业务超时,已退出.', [
          FActive.FID, FActive.FTunnel.FName]));
        Continue;
      end;
      
      nItv := GetTickCount - FActive.FValUpdate;
      if nItv >= FActive.FTONoData then //地磅故障
      begin
        FOwner.DoChangeEvent(FActive, bsClose);
        FOwner.InitTunnelData(FActive, False);

        WriteLog(Format('通道[ %s.%s ]故障,无数据应答.', [
          FActive.FID, FActive.FTunnel.FName]));
        Continue;
      end;

      if (nItv >= 1500) or (nItv < 300) then Continue;
      //更新超时 或 更新过频 

      DoExecute;
      if Terminated then Break;
    finally
      FOwner.FSyncLock.Leave;
    end;
  except
    on E: Exception do
    begin
      WriteLog(E.Message);
      Sleep(500);
    end;
  end;
end;

//Date: 2018-12-27
//Parm: 是否验证最小值
//Desc: 验证采样是否稳定
function TBasisWeightWorker.IsValidSamaple(const nCheckMin: Boolean): Boolean;
var nIdx: Integer;
    nVal: Integer;
begin
  Result := False;
  //default

  for nIdx:=High(FActive.FValSamples) downto 1 do
  begin
    if nCheckMin and (FActive.FValSamples[nIdx] < 0.02) then Exit;
    //样本不完整

    nVal := Trunc(FActive.FValSamples[nIdx] * 1000 -
                  FActive.FValSamples[nIdx-1] * 1000);
    if Abs(nVal) >= FActive.FTunnel.FSampleFloat then Exit; //浮动值过大
  end;
  
  Result := True;
end;

procedure TBasisWeightWorker.DoExecute;
begin
  FActive.FValSamples[FActive.FSampleIndex] := FActive.FValTunnel;
  Inc(FActive.FSampleIndex);

  if FActive.FSampleIndex >= FActive.FTunnel.FSampleNum then
    FActive.FSampleIndex := Low(FActive.FValSamples);
  //循环索引

  if FActive.FStableDone then
    FActive.FStableDone := FActive.FValHas = FActive.FValTunnel;
  //平稳数据变更

  if not FActive.FStableDone then
  begin
    {**************************** FActive.FStableDone **************************
    标记位用法:
     1.正常情况下,若通道数据FValTunnel没变化,则DoChangeEvent只调用一次.
     2.若DoChangeEvent由于某些原因,比如光栅判定失败,则需要再次触发业务,可手动
       设置FStableDone=False.
    ***************************************************************************}

    if IsValidSamaple(True) then //有效数据平稳
    begin
      FActive.FStableDone := True;
      FActive.FValHas := FActive.FValTunnel;
      FOwner.DoChangeEvent(FActive, bsStable);

      if not FActive.FStableDone then
        FOwner.InitTunnelData(FActive, True);
      //重置样本
    end;

    if (FActive.FValTunnel = 0) and
       (FActive.FValMax > 0) and IsValidSamaple(False) then //装完下磅
    begin
      if FActive.FValHas <= 0 then
        FActive.FValHas := FActive.FValMax;
      //使用最大值

      FActive.FStableDone := True;
      FOwner.DoChangeEvent(FActive, bsDone);

      if FActive.FStableDone then
        FOwner.InitTunnelData(FActive, False);
      //关闭业务
    end;
  end;
end;

initialization
  gBasisWeightManager := nil;
finalization
  FreeAndNil(gBasisWeightManager);
end.
