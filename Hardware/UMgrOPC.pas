{*******************************************************************************
  作者: dmzn@163.com 2019-07-04
  描述: 基于OPC的数据读写单元
*******************************************************************************}
unit UMgrOPC;

interface

uses
  Windows, Classes, SysUtils, Variants, SyncObjs, dOPCDA, dOPC, NativeXml, 
  UWaitItem, UMemDataPool, USysLoger, ULibFun;

const
  OPC_QUALITY_GOOD           = $C0; //good
  OPC_QUALITY_LOCAL_OVERRIDE = $D8; //值被覆盖,输入失去连接和手动被强制
  OPC_QUALITY_UNCERTAIN      = $40; //没有指定原因说明值为什么不确定
  OPC_QUALITY_LAST_USABLE    = $44; //最后的可用值
  OPC_QUALITY_SENSOR_CAL     = $50; //传感器达到了它的一个限值或者超过了它的量程
  OPC_QUALITY_EGU_EXCEEDED   = $54; //返回值越限
  OPC_QUALITY_SUB_NORMAL     = $58; //值有几个源,并且可用的源少于规定的品质好的源

  OPC_QUALITY_BAD            = $00; //值为坏的,没有标明原因
  OPC_QUALITY_CONFIG_ERROR   = $04; //服务器特定的配置问题
  OPC_QUALITY_NOT_CONNECTED  = $08; //输入没有可用的连接
  OPC_QUALITY_DEVICE_FAILURE = $0C; //设备故障
  OPC_QUALITY_SENSOR_FAILURE = $10; //传感器故障
  OPC_QUALITY_LAST_KNOWN     = $14; //通讯失败,最后的值是可用的
  OPC_QUALITY_COMM_FAILURE   = $18; //通讯失败,最后的值不可用
  OPC_QUALITY_OUT_OF_SERVICE = $1C; //块脱离扫描或者被锁

type
  POPCGroup = ^TOPCGroup;
  TOPCWantMode = (wmNone, wmSync, wmASync);
  //值写入方式: 无,同步,异步

  TOPCWantData = record
    FGroup      : string;                    //分组名称
    FItem       : string;                    //项目名称
    FValue      : OleVariant;                //待写入值
    FMode       : TOPCWantMode;              //写入方式
  end;
  TOPCWantDataItems = array of TOPCWantData;

  POPCItem = ^TOPCItem;
  TOPCItem = record
    FID         : string;                    //节点编号
    FName       : string;                    //项目名称
    FOPCItem    : TdOPCItem;                 //项目对象
    FGroup      : POPCGroup;                 //所属分组
    FServerItem : Integer;                   //服务数据项

    FValue      : string;                    //当前字符值
    FValueWant  : OleVariant;                //待设置值
    FWantMode   : TOPCWantMode;              //写入方式
    FLastUpdate : Cardinal;                  //当前值更新时间

    FFlagEvent  : Boolean;                   //更新后需触发事件
    FFlagStr    : string;                    //字符标识
    FFlagInt    : Integer;                   //整型标识
    FFlagPtr    : Pointer;                   //指针标识
  end;
  TOPCItems = array of TOPCItem;

  TOPCGroup = record
    FID         : string;                    //分组编号
    FName       : string;                    //分组名称
    FGroupItem  : TdOPCGroup;                //分组对象

    FActive     : Boolean;                   //是否启用
    FDeadBand   : Integer;                   //死区参数
    FUpdateRate : Integer;                   //刷新频率
    FOptions    : TStrings;                  //附加参数
    FItems      : TOPCItems;                 //组成员
  end;
  TOPCGroups = array of TOPCGroup;

  POPCServer = ^TOPCServer;
  TOPCServer = record
    FEnabled    : Boolean;                   //启用标识
    FID         : string;                    //服务编号
    FName       : string;                    //服务名称
    FServerID   : string;                    //服务标识
    FServerObj  : TdOPCServer;               //服务对象

    FLastUpdate : Cardinal;                  //最后更新
    FItems      : TOPCItems;                 //数据项
  end;

  POPCManagerStatus = ^TOPCManagerStatus;
  TOPCManagerStatus = record
    FConnected  : Boolean;                   //已连接
    FLastConn   : Cardinal;                  //上次连接
    FNewWant    : Boolean;                   //有待写入值
    FEventTimer : Cardinal;                  //事件触发计时
  end;

  TOPCManager = class;
  TOPCThread = class(TThread)
  private
    FOwner: TOPCManager;
    //拥有者
    FASyncList: TdOPCItemList;
    //异步列表
    FWaiter: TWaitObject;
    //等待对象
  protected
    procedure Execute; override;
    //执行线程
    procedure DoConnectServer();
    //连接服务
    procedure DoWriteOPCData();
    //执行写入
  public
    constructor Create(AOwner: TOPCManager);
    destructor Destroy; override;
    //创建释放
    procedure Wakeup;
    procedure StopMe;
    //启停线程
  end;

  TOPCEventMode = (emMain, emThread, emThreadTime);
  //事件运行: 主线程,子线程,子线程定时
  
  TOPCOnDataChange = procedure (const nServer: POPCServer) of object;
  TOPCOnDataChangeProc = procedure (const nServer: POPCServer);
  TOPCOnItemChange = procedure (const nItem: POPCItem) of object;
  TOPCOnItemChangeProc = procedure (const nItem: POPCItem);
  //事件定义

  TOPCManager = class(TObject)
  private
    FServer: TOPCServer;
    //服务对象
    FGroups: TOPCGroups;
    //数据分组
    FThread: TOPCThread;
    //工作线程
    FStatus: TOPCManagerStatus;
    //工作状态
    FSyncLock: TCriticalSection;
    //同步锁定
    FEventMode: TOPCEventMode;
    FEventInterval: Cardinal;
    FOnDataChange: TOPCOnDataChange;
    FOnDataChangeProc: TOPCOnDataChangeProc;
    FOnItemChange: TOPCOnItemChange;
    FOnItemChangeProc: TOPCOnItemChangeProc;
    //事件相关
  protected
    function FindPoint(const nID: string): Integer;
    function FindGroup(const nGroup: string; const nPoint: string = '';
      const nPIdx: PInteger = nil): Integer;
    //检索数据
    function ReadData(const nGroup,nPoint: string; var nSData: string;
      var nOData: OleVariant; const nReadStr: Boolean;
      const nTimeout: Integer): Boolean;
    //读取数据
    procedure OnServerConnect(Sender: TObject);
    procedure OnServerDisconnect(Sender: TObject);
    procedure OnServerDatachange(Sender: TObject; nList: TdOPCItemList);
    procedure OnServer_Shutdown(Sender: TObject; nReason: string);
    procedure OnServerTimeout(Sender: TObject);
    //OPC事件
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure LoadConfig(const nFile: string);
    //读取配置
    procedure StartService;
    procedure StopService;
    //启停服务
    function ReadOPC(const nPoint: string; var nData: string;
      const nTimeout: Integer = -1; const nGroup: string = ''): Boolean; overload;
    function ReadOPC(const nPoint: string; const nTimeout: Integer = -1;
      const nGroup: string = ''): string; overload;
    function ReadOPC(const nPoint: string; var nData: OleVariant;
      const nTimeout: Integer = -1; const nGroup: string = ''): Boolean; overload;
    //读取数据
    procedure WriteOPC(const nPoint,nData: string; const nGroup: string = '';
      const nMode: TOPCWantMode = wmSync); overload;
    procedure WriteOPC(const nData: TOPCWantDataItems); overload;
    //写入数据
    property EventMode: TOPCEventMode read FEventMode write FEventMode;
    property EventInterval: Cardinal read FEventInterval write FEventInterval;
    property OnDataChange: TOPCOnDataChange read FOnDataChange write FOnDataChange;
    property OnDataChangeProc: TOPCOnDataChangeProc read FOnDataChangeProc
     write FOnDataChangeProc;
    property OnItemChange: TOPCOnItemChange read FOnItemChange write FOnItemChange;
    property OnItemChangeProc: TOPCOnItemChangeProc read FOnItemChangeProc
     write FOnItemChangeProc;
    //事件相关
  end;

var
  gOPCManager: TOPCManager = nil;
  //全局使用

implementation

procedure WriteLog(const nEvent: string);
begin
  gSysLoger.AddLog(TOPCManager, 'OPC数据服务', nEvent);
end;

constructor TOPCManager.Create;
begin
  FThread := nil;
  FEventMode := emMain;
  FEventInterval := 1000;
  
  FServer.FEnabled := False;
  FServer.FServerObj := nil;
  SetLength(FServer.FItems, 0);
  
  SetLength(FGroups, 0);
  FillChar(FStatus, SizeOf(FStatus), #0);
  FSyncLock := TCriticalSection.Create;
end;

destructor TOPCManager.Destroy;
var nIdx: Integer;
begin
  StopService;
  //stop first

  for nIdx:=Low(FGroups) to High(FGroups) do
  begin
    if Assigned(FGroups[nIdx].FOptions) then
      FreeAndNil(FGroups[nIdx].FOptions);
    //xxxxx
  end;

  FServer.FServerObj.Free;
  FSyncLock.Free;
  inherited;
end;

procedure TOPCManager.StartService;
begin
  if not FServer.FEnabled then Exit;
  //no service

  if not Assigned(FThread) then
    FThread := TOPCThread.Create(Self);
  FThread.Wakeup;
end;

procedure TOPCManager.StopService;
begin
  if Assigned(FThread) then
    FThread.StopMe;
  FThread := nil;

  if Assigned(FServer.FServerObj) then
  begin
    FServer.FServerObj.OPCGroups.Clear;
    FServer.FServerObj.Active := False;
  end;
  FStatus.FConnected := False;
end;

//Date: 2019-07-12
//Parm: 接入点标识
//Desc: 检索标识为nID的接入点,返回索引
function TOPCManager.FindPoint(const nID: string): Integer;
var nIdx: Integer;
begin
  Result := -1;

  for nIdx:=Low(FServer.FItems) to High(FServer.FItems) do
  if CompareText(nID, FServer.FItems[nIdx].FID) = 0 then
  begin
    Result := nIdx;
    Break;
  end;
end;

//Date: 2019-07-15
//Parm: 组标识;接入点;接入点索引
//Desc: 检索标识为nID的组,返回索引
function TOPCManager.FindGroup(const nGroup,nPoint: string;
 const nPIdx: PInteger): Integer;
var i,nIdx: Integer;
begin
  Result := -1;
  if Assigned(nPIdx) then
    nPIdx^ := -1;
  //default

  if nGroup <> '' then
  begin
    for nIdx:=Low(FGroups) to High(FGroups) do
    if CompareText(nGroup, FGroups[nIdx].FID) = 0 then
    begin
      Result := nIdx;
      Break;
    end;
  end;

  if nPoint <> '' then
  begin
    if Result < 0 then
    begin
      for nIdx:=Low(FGroups) to High(FGroups) do
       for i:=Low(FGroups[nIdx].FItems) to High(FGroups[nIdx].FItems) do
        if CompareText(nPoint, FGroups[nIdx].FItems[i].FID) = 0 then
        begin
          Result := nIdx;
          if Assigned(nPIdx) then
            nPIdx^ := i;
          Break;
        end;
      //使用接入点检索
    end else

    if Assigned(nPIdx) then
    begin
      for i:=Low(FGroups[Result].FItems) to High(FGroups[Result].FItems) do
      if CompareText(nPoint, FGroups[Result].FItems[i].FID) = 0 then
      begin
        nPIdx^ := i; //检索接入点索引
        Break;
      end;
    end;
  end;
end;

//Date: 2019-07-12
//Desc: 连接Server成功
procedure TOPCManager.OnServerConnect(Sender: TObject);
begin
  FStatus.FConnected := True;
  WriteLog('Server Connected');
end;

//Date: 2019-07-12
//Desc: 与Server连接断开
procedure TOPCManager.OnServerDisconnect(Sender: TObject);
begin
  FStatus.FConnected := False;
  FStatus.FLastConn := GetTickCount(); //delay
  WriteLog('Server Disconnect');
end;

//Date: 2019-07-15
//Desc: 服务关闭
procedure TOPCManager.OnServer_Shutdown(Sender: TObject; nReason: string);
begin
  FStatus.FConnected := False;
  FStatus.FLastConn := GetTickCount(); //delay
  WriteLog('Server Shutdown(' + nReason + ')');
end;

procedure TOPCManager.OnServerTimeout(Sender: TObject);
begin
  FStatus.FConnected := False;
  FStatus.FLastConn := GetTickCount(); //delay
  WriteLog('Server TimeOut');
end;

//Date: 2019-07-12
//Desc: 数据变更,更新数据项
procedure TOPCManager.OnServerDatachange(Sender: TObject; nList: TdOPCItemList);
var nIdx: Integer;
    nItem: POPCItem;
begin
  FServer.FLastUpdate := GetTickCount();
  //time-stamp

  for nIdx:=nList.Count - 1 downto 0 do
  begin
    nItem := nList[nIdx].Data;
    try
      FSyncLock.Enter;
      try
        nItem.FValue := nItem.FOPCItem.ValueStr;
        nItem.FLastUpdate := FServer.FLastUpdate;

        with FServer.FItems[nItem.FServerItem] do
        begin
          FOPCItem    := nItem.FOPCItem;
          FValue      := nItem.FValue;
          FGroup      := nItem.FGroup;

          FLastUpdate := FServer.FLastUpdate;
          FFlagEvent  := True;
        end;
      finally
        FSyncLock.Leave;
      end; 

      if Assigned(FOnItemChange) then
        FOnItemChange(nItem);
      //xxxxx

      if Assigned(FOnItemChangeProc) then
        FOnItemChangeProc(nItem);
      //xxxxx
    except
      on nErr: Exception do
      begin
        WriteLog(Format('Item %s.%s OnChange Error: %s', [nItem.FID,
          nItem.FName, nErr.Message]));
        //xxxxx
      end;
    end;
  end;

  if FEventMode = emMain then
  try
    if Assigned(FOnDataChange) then
      FOnDataChange(@FServer);
    //xxxxx

    if Assigned(FOnDataChangeProc) then
      FOnDataChangeProc(@FServer);
    //xxxxx

    for nIdx:=Low(FServer.FItems) to High(FServer.FItems) do
     if FServer.FItems[nIdx].FFlagEvent then
      FServer.FItems[nIdx].FFlagEvent := False;
    //reset event flag
  except
    on nErr: Exception do
    begin
      WriteLog(Format('Server %s.%s OnChange Error: %s', [FServer.FID,
        FServer.FName, nErr.Message]));
      //xxxxx
    end;
  end;
end;

//Date: 2019-07-15
//Parm: 组标识;接入点;字符数据;复合变量;读取字符数据;数据超时
//Desc: 读取nGroup.nPoint的数据
function TOPCManager.ReadData(const nGroup,nPoint: string;
  var nSData: string; var nOData: OleVariant;
  const nReadStr: Boolean; const nTimeout: Integer): Boolean;
var nIdx,nInt: Integer;
begin
  if not FServer.FEnabled then
  begin
    Result := False;
    Exit;
  end; //no service

  FSyncLock.Enter;
  try
    Result := False;
    if nGroup = '' then
    begin
      nIdx := FindPoint(nPoint);
      if nIdx < 0 then Exit;

      with FServer.FItems[nIdx] do
      begin
        if (nTimeout > 0) and (GetTickCountDiff(FLastUpdate) >= nTimeout) then
        begin
          if (not Assigned(FOPCItem)) or (FOPCItem.Quality <> OPC_QUALITY_GOOD) then
            Exit;
          //更新超时,且连接质量不是良好
        end;

        Result := nReadStr or Assigned(FOPCItem);
        if Result then
        begin
          if nReadStr then
               nSData := FValue
          else nOData := FOPCItem.Value;
        end;
      end;
      
      Exit; //接入点最新数据
    end;

    nIdx := FindGroup(nGroup, nPoint, @nInt);
    if (nIdx < 0) or (nInt < 0) then Exit;
    if not FGroups[nIdx].FActive then Exit;

    with FGroups[nIdx].FItems[nInt] do
    begin
      if (nTimeout > 0) and (GetTickCountDiff(FLastUpdate) >= nTimeout) then
      begin
        if (not Assigned(FOPCItem)) or (FOPCItem.Quality <> OPC_QUALITY_GOOD) then
          Exit;
        //更新超时,且连接质量不是良好
      end;

      Result := nReadStr or Assigned(FOPCItem);
      if Result then
      begin
        if nReadStr then
             nSData := FValue
        else nOData := FOPCItem.Value;
      end;
    end;                           
  finally
    FSyncLock.Leave;
  end;   
end;

function TOPCManager.ReadOPC(const nPoint: string; var nData: string;
  const nTimeout: Integer; const nGroup: string): Boolean;
var nOle: OleVariant;
begin
  Result := ReadData(nGroup, nPoint, nData, nOle, True, nTimeout);
end;

function TOPCManager.ReadOPC(const nPoint: string; const nTimeout: Integer;
  const nGroup: string): string;
var nOle: OleVariant;
begin
  if not ReadData(nGroup, nPoint, Result, nOle, True, nTimeout) then
    Result := '';
  //xxxxx
end;

function TOPCManager.ReadOPC(const nPoint: string; var nData: OleVariant;
  const nTimeout: Integer; const nGroup: string): Boolean;
var nStr: string;
begin
  Result := ReadData(nGroup, nPoint, nStr, nData, False, nTimeout);
end;

//Date: 2019-07-16
//Parm: 待写入项
//Desc: 将nData写入OPC
procedure TOPCManager.WriteOPC(const nData: TOPCWantDataItems);
var i,nIdx,nInt: Integer;
begin
  if not FServer.FEnabled then Exit;
  //no service
  
  FSyncLock.Enter;
  try
    for nIdx:=Low(nData) to High(nData) do
    begin
      nInt := FindGroup(nData[nIdx].FGroup, nData[nIdx].FItem, @i);
      if (nInt < 0) or (i < 0) then Continue;

      if FGroups[nInt].FActive then
      with FGroups[nInt].FItems[i] do
      begin
        FValueWant := nData[nIdx].FValue;
        FWantMode  := nData[nIdx].FMode;
      end;
    end;

    FStatus.FNewWant := True;
    //write flag
  finally
    FSyncLock.Leave;
  end;

  if Assigned(FThread) then
    FThread.Wakeup;
  //write quick
end;

//Date: 2019-07-16
//Parm: 接入点;数据;分组;模式
//Desc: 将nData写入nGroup.nPoint
procedure TOPCManager.WriteOPC(const nPoint, nData, nGroup: string;
  const nMode: TOPCWantMode);
var nItems: TOPCWantDataItems;
begin
  SetLength(nItems, 1);
  with nItems[0] do
  begin
    FGroup := nGroup;
    FItem  := nPoint;
    FValue := nData;
    FMode  := nMode;
  end;

  WriteOPC(nItems);
end;

//Date: 2019-07-12
//Parm: 配置文件
//Desc: 加载nFile配置项
procedure TOPCManager.LoadConfig(const nFile: string);
var nList: TStrings;
    nXML: TNativeXml;
    i,j,k,nIdx,nLen: Integer;
    nRoot,nNode,nTmp: TXmlNode;
begin
  nList := nil;
  nXML := TNativeXml.Create;
  try
    nXML.LoadFromFile(nFile);
    //load config
    nRoot := nXML.Root.NodeByNameR('enable');
    FServer.FEnabled := nRoot.ValueAsString <> 'N';

    nRoot := nXML.Root.NodeByNameR('server');
    with FServer do
    begin
      FID := nRoot.AttributeByName['id'];
      FName := nRoot.AttributeByName['name'];
      FServerID := nRoot.ValueAsString;
    end;

    nRoot := nXML.Root.NodeByNameR('points');
    SetLength(FServer.FItems, nRoot.NodeCount);
    nIdx := Low(FServer.FItems);

    for i:=0 to nRoot.NodeCount - 1 do
    begin
      nNode := nRoot.Nodes[i];
      with FServer.FItems[nIdx] do
      begin
        FID         := nNode.AttributeByName['id'];
        FName       := nNode.AttributeByName['name'];
        FValue      := nNode.AttributeByName['default'];

        FWantMode   := wmNone;
        FOPCItem    := nil;
        FLastUpdate := 0;

        FFlagEvent  := False;
        FFlagStr    := '';
        FFlagInt    := -1;
        FFlagPtr    := nil;
      end;

      Inc(nIdx);
    end;

    nList := TStringList.Create;
    nRoot := nXML.Root.NodeByNameR('groups');
    SetLength(FGroups, nRoot.NodeCount);
    nIdx := Low(FGroups);

    for i:=0 to nRoot.NodeCount - 1 do
    begin
      nNode := nRoot.Nodes[i];
      with FGroups[nIdx] do
      begin
        FID         := nNode.AttributeByName['id'];
        FName       := nNode.AttributeByName['name'];
        FGroupItem  := nil;
        
        FActive     := nNode.NodeByNameR('active').ValueAsString <> 'N';
        FDeadBand   := nNode.NodeByNameR('deadband').ValueAsInteger;
        FUpdateRate := nNode.NodeByNameR('updaterate').ValueAsInteger;

        nTmp := nNode.NodeByName('options');
        if Assigned(nTmp) then
        begin
          FOptions := TStringList.Create;
          SplitStr(nTmp.ValueAsString, FOptions, 0, ';');
        end else FOptions := nil;

        SetLength(FItems, 0);
        SplitStr(nNode.NodeByNameR('points').ValueAsString, nList, 0, ',');

        for j:=nList.Count-1 downto 0 do
        begin
          k := FindPoint(nList[j]);
          if k < 0 then
          begin
            WriteLog(Format('组节点[ %s.%s ]不存在', [FID, nList[j]]));
            Continue;
          end;

          nLen := Length(FItems);
          SetLength(FItems, nLen + 1);
          FItems[nLen] := FServer.FItems[k];
          FItems[nLen].FServerItem := k;
        end;
      end;

      Inc(nIdx);
    end;
  finally
    nList.Free;
    nXML.Free;
  end;
end;

//------------------------------------------------------------------------------
constructor TOPCThread.Create(AOwner: TOPCManager);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
  FASyncList := TdOPCItemList.Create;
  FWaiter := TWaitObject.Create;
  FWaiter.Interval := 300;
end;

destructor TOPCThread.Destroy;
begin
  FASyncList.Free;
  FWaiter.Free;
  inherited;
end;

procedure TOPCThread.Wakeup;
begin
  FWaiter.Wakeup;
end;

procedure TOPCThread.StopMe;
begin
  Terminate;
  FWaiter.Wakeup;

  WaitFor;
  Free;
end;

procedure TOPCThread.Execute;
var nIdx,nInt: Integer;
begin
  while not Terminated do
  try
    FWaiter.EnterWait;
    if Terminated then Break;

    with FOwner.FStatus do
    begin
      if (not FConnected) and (GetTickCountDiff(FLastConn) > 3 * 1000) then
        Synchronize(DoConnectServer);
      //reconn

      if FNewWant and FConnected then
        Synchronize(DoWriteOPCData);
      //xxxxx
    end;

    with FOwner do
    begin
      if (FEventMode = emThreadTime) and
         (Assigned(FOnDataChange) or Assigned(FOnDataChangeProc)) and
         (GetTickCountDiff(FStatus.FEventTimer, tdNow) >= FEventInterval) then
      try
        FStatus.FEventTimer := GetTickCount();
        FSyncLock.Enter;
        //lock first

        if Assigned(FOnDataChange) then
          FOnDataChange(@FServer);
        //xxxxx

        if Assigned(FOnDataChangeProc) then
          FOnDataChangeProc(@FServer);
        //xxxxx
      finally
        FSyncLock.Leave;
      end;

      if (FEventMode = emThread) and
         (Assigned(FOnDataChange) or Assigned(FOnDataChangeProc)) then
      try
        FSyncLock.Enter;
        //lock first
        nInt := -1;

        for nIdx:=Low(FServer.FItems) to High(FServer.FItems) do
        if FServer.FItems[nIdx].FFlagEvent then
        begin
          if Assigned(FOnDataChange) then
            FOnDataChange(@FServer);
          //xxxxx

          if Assigned(FOnDataChangeProc) then
            FOnDataChangeProc(@FServer);
          //xxxxx

          nInt := nIdx;
          //event item
          Break;
        end;

        if nInt >= 0 then
        begin
          for nIdx:=Low(FServer.FItems) to High(FServer.FItems) do
           if FServer.FItems[nIdx].FFlagEvent then
            FServer.FItems[nIdx].FFlagEvent := False;
          //reset event flag
        end;
      finally
        FSyncLock.Leave;
      end;
    end;
  except
    on nErr: Exception do
    begin
      WriteLog(Format('OPC-Service Error: %s', [nErr.Message]));
      //log any error
    end;
  end;
end;

//Date: 2019-07-12
//Desc: 连接服务器,创建分组和接入点
procedure TOPCThread.DoConnectServer;
var i,nIdx: Integer;
    nItem: TdOPCItem;
begin
  with FOwner do
  try
    FSyncLock.Enter;
    //lock first

    WriteLog(Format('Connect OPC-Server(%s.%s)', [FServer.FID, FServer.FName]));
    //logged
    FStatus.FLastConn := GetTickCount();

    if Assigned(FServer.FServerObj) then
    begin
      FServer.FServerObj.Active := False;
      //stop first

      for nIdx:=Low(FGroups) to High(FGroups) do
       with FGroups[nIdx] do
        for i:=Low(FItems) to High(FItems) do
         FItems[i].FOPCItem := nil;
      //reset group's item

      for nIdx:=Low(FServer.FItems) to High(FServer.FItems) do
        FServer.FItems[nIdx].FOPCItem := nil;
      //reset server's item
    end else
    begin
      FServer.FServerObj := TdOPCServer.Create(nil);
      with FServer.FServerObj do
      begin
        KeepAlive := 3200;
        //check the OPC server accessibility

        OnConnect := OnServerConnect;
        OnDisconnect := OnServerDisconnect;
        OnDataChange := OnServerDatachange;
        OnServerShutdown := OnServer_Shutdown;
      end;
    end;

    with FServer.FServerObj do
    begin
      OPCGroups.Clear;
      ServerName := FServer.FServerID;
      Active := True;
    end;

    for nIdx:=Low(FGroups) to High(FGroups) do
    with FGroups[nIdx] do
    begin
      with FServer.FServerObj do
      begin
        FGroupItem := OPCGroups.GetOPCGroup(FID);
        if not Assigned(FGroupItem) then
          FGroupItem := OPCGroups.Add(FID);
        //xxxxx
      end;

      with FGroupItem do
      begin
        OPCItems.RemoveAll;
        FGroupItem.Name := FGroups[nIdx].FID;

        UpdateRate := FUpdateRate;
        DeadBand   := FDeadBand;
        IsActive   := FActive;
      end;

      for i:=Low(FItems) to High(FItems) do
      try
        FItems[i].FGroup := @FGroups[nIdx];
        FItems[i].FOPCItem := nil;
        nItem := FGroupItem.OPCItems.AddItem(FItems[i].FName);

        FItems[i].FOPCItem := nItem;
        FItems[i].FOPCItem.Data := @FItems[i];
      except
        on nErr: Exception do
        begin
          WriteLog(Format('New Group %s.%s Item %s.%s Error: %s', [
            FID, FName, FItems[i].FID, FItems[i].FName, nErr.Message]));
          //xxxxx
        end;
      end;
    end;
  finally
    FSyncLock.Leave;
  end;
end;

//Date: 2019-07-16
//Desc: 执行数据写入
procedure TOPCThread.DoWriteOPCData;
var i,nIdx: Integer;
begin
  with FOwner do
  try
    FSyncLock.Enter;
    //lock first
    
    for nIdx:=Low(FGroups) to High(FGroups) do
    try
      FASyncList.Clear;
      //default

      for i:=Low(FGroups[nIdx].FItems) to High(FGroups[nIdx].FItems) do
      with FGroups[nIdx].FItems[i] do
      begin
        if FWantMode = wmSync then
          FOPCItem.WriteSync(FValueWant);
        //xxxxx

        if FWantMode = wmASync then
        begin
          FOPCItem.WantValue := FValueWant;
          FASyncList.Add(FOPCItem);
        end;

        FWantMode := wmNone;
        //erase flag
      end;

      if FASyncList.Count > 0 then
        FGroups[nIdx].FGroupItem.AsyncWrite(FASyncList);
      //xxxxx
    except
      on nErr: Exception do
      begin
        WriteLog('WriteOPCData Error: ' + nErr.Message);
      end;
    end;

    FStatus.FNewWant := False;
    //erase write flag
  finally
    FSyncLock.Leave;
  end;
end;

initialization
  gOPCManager := nil;
finalization
  FreeAndNil(gOPCManager);
end.
