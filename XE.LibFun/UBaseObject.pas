{*******************************************************************************
  作者: dmzn@163.com 2017-03-21
  描述: 注册管理系统对象的运行状态

  备注:
  *.TObjectBase.DataS,DataP,Health属性,调用时需要用SyncEnter锁定,避免多线程操作
    时写脏数据.
*******************************************************************************}
unit UBaseObject;

interface

uses   
  System.Classes, System.SysUtils, System.SyncObjs, ULibFun
  {$IF defined(MSWINDOWS)},Winapi.Windows{$ENDIF};

type
  TObjectHealth = (hlHigh, hlNormal, hlLow, hlBad);
  //对象状态

  TObjectStatusHelper = class
  public
    class var
      shTitle: Word;
      shData: Word;
      //标题,数据长度

    class procedure AddTitle(const nList: TStrings;
      const nClass: string);  static;
    //添加标题   
    class function FixData(const nTitle: string;
      const nData: string): string; overload; static;
    class function FixData(const nTitle: string;
      const nData: Double): string; overload; static; 
    //格式化数据    
  end;

  TObjectBase = class(TObject)  
  public
    type
      TDataDim = 0..2;
      TDataS = array [TDataDim] of string;
      TDataP = array [TDataDim] of Pointer;      
    var
      DataS: TDataS;
      DataP: TDataP;
      //状态数据 
  strict private
    FSyncLock: TCriticalSection;
    //同步锁定
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放     
    procedure SyncEnter;
    procedure SyncLeave;
    //同步锁定      
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); virtual;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; virtual;
    //对象状态
  end;

  TObjectBaseClass = class of TObjectBase;
  //对象类

  TManagerBase = class
  private
    FSyncLock: TCriticalSection;
    //同步锁定
  protected
    class function GetMe(const nClass: TClass;
      const nAutoNew: Boolean = True): Integer; static;
    class procedure RegistMe(const nReg: Boolean); virtual; abstract;
    //注册管理器    
  public 
    constructor Create;
    destructor Destroy; override;
    //创建释放    
    procedure SyncEnter;
    procedure SyncLeave;
    //同步锁定
    procedure RunAfterRegistAllManager; virtual;
    procedure RunBeforUnregistAllManager; virtual;
    //延迟运行
    procedure RunAfterApplicationStart; virtual;
    procedure RunBeforApplicationHalt; virtual;
    //关联运行
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); virtual;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; virtual;
    //对象状态 
    class function GetManager(const nClass: TClass): TManagerBase; static;
    //检索管理器    
  end;

  TManagerClass = class of TManagerBase;
  //管理器类
  
  TCommonObjectManager = class(TManagerBase)
  private  
    FObjects: TList;
    //对象列表
  public       
    constructor Create;
    destructor Destroy; override;
    //创建释放
    class procedure RegistMe(const nReg: Boolean); override;
    //注册管理器
    procedure AddObject(const nObj: TObject);
    procedure DelObject(const nObj: TObject);
    //添加删除
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    //获取状态
  end;

  PSerialID = ^TSerialID;
  TSerialID = record
    FID   : Cardinal; //编码基数
    FLoop : Cardinal; //循环次数
  end;

  TSerialIDRelation = (srSame, srBigger, srSmaller);
  //关系:相同,大于,小于

  TSerialIDManager = class(TManagerBase)
  private
    FBase: TSerialID;
    //编码基数
    FTimeStamp: string;
    //时间戳
  public
    constructor Create;
    //创建释放
    class procedure RegistMe(const nReg: Boolean); override;
    //注册管理器    
    function GetID: Cardinal;
    function GetSID: string;
    function GetSerialID: TSerialID;
    //获取标识
    function RandomID(const nLen: Integer): string;
    //获取随机标识
    function CompareID(const nA,nB: TSerialID;
      const nRelation: TSerialIDRelation): Boolean;
    //对比标识
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    //获取状态
  end;

implementation

uses
  UManagerGroup;

//Date: 2017-04-14
//Parm: 列表;类名
//Desc: 添加一个类的标题到列表
class procedure TObjectStatusHelper.AddTitle(const nList: TStrings;
  const nClass: string);
var nLen: Integer;
begin
  if nList.Count > 0 then     
      nList.Add('');
  //xxxxx
  
  nLen := Trunc((shTitle - Length(nClass)) / 2);
  nList.Add(StringOfChar('+', nLen) + ' ' + nClass + ' ' +
            StringOfChar('+', nLen));
  //title
end;

//Date: 2017-04-10
//Parm: 前缀标题;数据
//Desc: 格式化数据,格式为: nTitle(定长) nData
class function TObjectStatusHelper.FixData(const nTitle, nData: string): string;
begin
  Result := ULibFun.TStringHelper.FixWidth(nTitle, shData) + nData;
end;

class function TObjectStatusHelper.FixData(const nTitle: string;
  const nData: Double): string;
begin
  Result := FixData(nTitle, nData.ToString);
end;

//------------------------------------------------------------------------------
constructor TObjectBase.Create;
begin
  FSyncLock := nil;  
  if Assigned(gMG.FObjectManager) then
    gMG.FObjectManager.AddObject(Self);
  //xxxxx
end;

destructor TObjectBase.Destroy;
begin
  if Assigned(gMG.FObjectManager) then
    gMG.FObjectManager.DelObject(Self);
  //xxxxx
  
  FSyncLock.Free;
  inherited;
end;

procedure TObjectBase.SyncEnter;
begin
  if not Assigned(FSyncLock) then   
    FSyncLock := TCriticalSection.Create;
  FSyncLock.Enter;
end;

procedure TObjectBase.SyncLeave;
begin
  if Assigned(FSyncLock) then
    FSyncLock.Leave;
  //xxxxx
end;

//Desc: 对象健康度
function TObjectBase.GetHealth(const nList: TStrings): TObjectHealth;
begin
  Result := hlNormal;
end;

//Desc: 对象状态
procedure TObjectBase.GetStatus(const nList: TStrings; const nFriendly: Boolean);
begin
  TObjectStatusHelper.AddTitle(nList, ClassName);
end;

//------------------------------------------------------------------------------
constructor TManagerBase.Create;
begin
  inherited;
  FSyncLock := nil;
end;

destructor TManagerBase.Destroy;
begin
  FSyncLock.Free;
  inherited;
end;

procedure TManagerBase.SyncEnter;
begin
  if not Assigned(FSyncLock) then   
    FSyncLock := TCriticalSection.Create;
  FSyncLock.Enter;
end;

procedure TManagerBase.SyncLeave;
begin
  if Assigned(FSyncLock) then
    FSyncLock.Leave;
  //xxxxx
end;

//Date: 2017-03-23
//Parm: 类别;自动添加
//Desc: 检索nClass在管理器列表中的位置
class function TManagerBase.GetMe(const nClass: TClass;
  const nAutoNew: Boolean): Integer;
var nIdx: Integer;
begin
  for nIdx := Low(gMG.FManagers) to High(gMG.FManagers) do
  if gMG.FManagers[nIdx].FClass = nClass then
  begin
    Result := nIdx;
    Exit;
  end;
    
  Result := -1;
  if not nAutoNew then Exit;

  Result := Length(gMG.FManagers);
  nIdx := Result; 
  SetLength(gMG.FManagers, nIdx + 1);

  with gMG.FManagers[nIdx] do
  begin
    FClass := nClass;
    FManager := nil;
  end;    
end;

//Desc: 全部管理器注册完毕后运行
procedure TManagerBase.RunAfterRegistAllManager;
begin
  //sub-type do
end;

//Desc: 卸载管理器之前运行
procedure TManagerBase.RunBeforUnregistAllManager;
begin
  //sub-type do
end;

//Desc: 主进程启动后运行
procedure TManagerBase.RunAfterApplicationStart;
begin
  //sub-type do
end;

//Desc: 主进程准备关闭
procedure TManagerBase.RunBeforApplicationHalt;
begin
  //sub-type do
end;

//Desc: 对象状态
procedure TManagerBase.GetStatus(const nList: TStrings; 
  const nFriendly: Boolean);
begin
  TObjectStatusHelper.AddTitle(nList, ClassName);
end;

//Desc: 对象健康度
function TManagerBase.GetHealth(const nList: TStrings): TObjectHealth;
begin
  Result := hlNormal;
end;

//Date: 2017-04-15
//Parm: 类名
//Desc: 检索类名为nClass的管理器
class function TManagerBase.GetManager(const nClass: TClass): TManagerBase;
var nIdx: Integer;
begin
  nIdx := GetMe(nClass, False);
  if nIdx < 0 then
       Result := nil
  else Result := gMG.FManagers[nIdx].FManager;
end;

//------------------------------------------------------------------------------
constructor TCommonObjectManager.Create;
begin
  inherited;
  FObjects := TList.Create;
end;

destructor TCommonObjectManager.Destroy;
begin
  FObjects.Free;
  inherited;
end;

//Date: 2017-03-23
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TCommonObjectManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TCommonObjectManager);
  if nReg then
  begin     
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TCommonObjectManager.Create;
    gMG.FObjectManager := gMG.FManagers[nIdx].FManager as TCommonObjectManager;
  end else
  begin
    gMG.FObjectManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;
end;

procedure TCommonObjectManager.AddObject(const nObj: TObject);
begin
  if not (nObj is TObjectBase) then
    raise Exception.Create(ClassName + ': Object Is Not Support.');
  //xxxxx

  SyncEnter;
  FObjects.Add(nObj);
  SyncLeave;
end;

procedure TCommonObjectManager.DelObject(const nObj: TObject);
var nIdx: Integer;
begin
  SyncEnter;
  try
    nIdx := FObjects.IndexOf(nObj);
    if nIdx > -1 then
      FObjects.Delete(nIdx);
    //xxxxx
  finally
    SyncLeave;
  end;
end;

//Date: 2017-04-15
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList中
procedure TCommonObjectManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx: Integer;
begin
  with TObjectStatusHelper do
  try  
    SyncEnter;     
    if not nFriendly then
    begin
      inherited GetStatus(nList, nFriendly);
      nList.Add('NumObject=' + FObjects.Count.ToString);
      Exit;
    end;
    
    for nIdx:=0 to FObjects.Count - 1 do
    with TObjectBase(FObjects[nIdx]) do
    begin
      TObjectStatusHelper.AddTitle(nList, ClassName);
      GetStatus(nList, nFriendly);
    end;
  finally
    SyncLeave;
  end;
end;

//------------------------------------------------------------------------------
constructor TSerialIDManager.Create;
begin
  inherited;
  FBase.FID := 0;
  FBase.FLoop := 0;

  with TDateTimeHelper do
  begin
    FTimeStamp := DateTime2Str(Now());
    {$IF defined(MSWINDOWS)}
    FTimeStamp := FTimeStamp + ' Win-OS: ' + IntToStr(GetTickCount);
    {$ENDIF}
  end;
end;

//Date: 2017-03-23
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TSerialIDManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TSerialIDManager);
  if nReg then
  begin
    if not Assigned(gMG.FManagers[nIdx].FManager) then
      gMG.FManagers[nIdx].FManager := TSerialIDManager.Create;
    gMG.FSerialIDManager := gMG.FManagers[nIdx].FManager as TSerialIDManager;
  end else
  begin
    gMG.FSerialIDManager := nil;
    FreeAndNil(gMG.FManagers[nIdx].FManager);
  end;
end;

function TSerialIDManager.GetSerialID: TSerialID;
begin
  SyncEnter;
  try
    if FBase.FID < High(Cardinal) then
    begin
      Inc(FBase.FID);
    end else
    begin
      FBase.FID := 1;
      if FBase.FLoop < High(Cardinal) then
           Inc(FBase.FLoop)
      else FBase.FLoop := 1;
    end;

    Result := FBase;
  finally
    SyncLeave;
  end;
end;

function TSerialIDManager.GetID: Cardinal;
begin
  Result := GetSerialID.FID;
end;

function TSerialIDManager.GetSID: string;
begin
  Result := GetSerialID.FID.ToString;
end;

//Date: 2024-12-25
//Parm: 标识长度
//Desc: 依据时间生成nLen长度的随机标识
function TSerialIDManager.RandomID(const nLen: Integer): string;
var nL,nPos: Integer;
begin
  Result := TDateTimeHelper.DateTimeSerial(False) + IntToStr(Random(999));
  Result := TEncodeHelper.EncodeMD5(Result);

  nL := Length(Result);
  nPos := nL;

  if nL > nLen then
    nL := nLen;
  //xxxxx

  nPos := Random(nPos - nL + 1);
  if nPos < 1 then
    nPos := 1;
  Result := Copy(Result, nPos, nL);
end;

//Date: 2019-01-22
//Parm: 标识A,B;关系项
//Desc: 判断nA,nB是否具有nRelation
function TSerialIDManager.CompareID(const nA, nB: TSerialID;
  const nRelation: TSerialIDRelation): Boolean;
begin
  case nRelation of
   srSame    : Result := (nA.FLoop = nB.FLoop) and (nA.FID = nB.FID);
   srBigger  : Result := (nA.FLoop > nB.FLoop) or ((
                         (nA.FLoop = nB.FLoop)) and (nA.FID > nB.FID));
   srSmaller : Result := (nA.FLoop < nB.FLoop) or ((
                         (nA.FLoop = nB.FLoop)) and (nA.FID < nB.FID))
   else        Result := False;
  end;
end;

//Date: 2017-04-15
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList中
procedure TSerialIDManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nStr: string;
begin
  with TObjectStatusHelper do
  try  
    SyncEnter;
    inherited GetStatus(nList, nFriendly);
    
    if not nFriendly then
    begin
      nList.Add('Base=' + FBase.FID.ToString);
      nList.Add('Loop=' + FBase.FLoop.ToString);
      Exit;
    end;
    
    with TDateTimeHelper do
    begin
      nStr := DateTime2Str(Now());
      {$IF defined(MSWINDOWS)}
      nStr := nStr + ' Win-OS: ' + IntToStr(GetTickCount);
      {$ENDIF}
    end;
  
    nList.Add(FixData('Base:', FBase.FID));
    nList.Add(FixData('Loop:', FBase.FLoop));
    nList.Add(FixData('Start On:',  FTimeStamp));
    nList.Add(FixData('Service Now:',  nStr));
  finally
    SyncLeave;
  end;
end;

initialization
  TObjectStatusHelper.shTitle := 85;
  TObjectStatusHelper.shData  := 42;
finalization
  //nothing
end.
