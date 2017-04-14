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
  System.Classes, System.SysUtils, System.SyncObjs, ULibFun;

type
  TObjectStatusHelper = class
  public  
    class procedure AddTitle(const nList: TStrings; const nClass: string);
    //添加标题   
    class function FixData(const nTitle: string;
      const nData: string): string; overload;
    class function FixData(const nTitle: string;
      const nData: Double): string; overload; 
    //格式化数据    
  end;

  TObjectBase = class(TObject)
  strict private
    FSyncLock: TCriticalSection;
    //同步锁定
  public
    type
      TDataDim = 0..2;
      TDataS = array [TDataDim] of string;
      TDataP = array [TDataDim] of Pointer; 
      THealth = (hlHigh, hlNormal, hlLow, hlBad);
    var
      DataS: TDataS;
      DataP: TDataP;
      Health: THealth;
      //状态数据
            
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure GetStatus(const nList: TStrings); virtual;
    //对象状态
    procedure SyncEnter;
    procedure SyncLeave;
    //同步锁定    
  end;

  TManagerBase = class
  strict private
    FSyncLock: TCriticalSection;
    //同步锁定
  strict protected
    type
      TItem = record
        FClass: TClass;
        FManager: TObject;
      end;
    class var
      FManagers: array of TItem;
      //管理器列表
  protected
    class function GetMe(const nClass: TClass): Integer;
    class procedure RegistMe(const nReg: Boolean = True); virtual; abstract;
    //注册管理器  
    procedure GetStatus(const nList: TStrings); virtual;
    //对象状态
  public       
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure SyncEnter;
    procedure SyncLeave;
    //同步锁定 
  end;

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
    procedure GetStatus(const nList: TStrings); override;
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
  
  nLen := Trunc((85 - Length(nClass)) / 2);
  nList.Add(StringOfChar('+', nLen) + ' ' + nClass + ' ' +
            StringOfChar('+', nLen));
  //title
end;

//Date: 2017-04-10
//Parm: 前缀标题;数据
//Desc: 格式化数据,格式为: nTitle(定长) nData
class function TObjectStatusHelper.FixData(const nTitle, nData: string): string;
begin
  Result := ULibFun.TStringHelper.FixWidth(nTitle, 32) + nData;
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
  Health := hlNormal;
  
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

//Desc: 添加标题行
procedure TObjectBase.GetStatus(const nList: TStrings);
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
//Parm: 类别
//Desc: 检索nClass在管理器列表中的位置
class function TManagerBase.GetMe(const nClass: TClass): Integer;
var nIdx: Integer;
begin
  for nIdx := Low(FManagers) to High(FManagers) do
  if FManagers[nIdx].FClass = nClass then
  begin
    Result := nIdx;
    Exit;
  end;
    
  Result := Length(FManagers); 
  nIdx := Result; 
  SetLength(FManagers, nIdx + 1);

  with FManagers[nIdx] do
  begin
    FClass := nClass;
    FManager := nil;
  end;    
end;

//Desc: 添加标题行
procedure TManagerBase.GetStatus(const nList: TStrings);
begin
  TObjectStatusHelper.AddTitle(nList, ClassName);
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
    if not Assigned(FManagers[nIdx].FManager) then
      FManagers[nIdx].FManager := TCommonObjectManager.Create;
    gMG.FObjectManager := FManagers[nIdx].FManager as TCommonObjectManager; 
  end else
  begin
    gMG.FObjectManager := nil;
    FreeAndNil(FManagers[nIdx].FManager);    
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

procedure TCommonObjectManager.GetStatus(const nList: TStrings);
var nIdx,nLen: Integer;
begin
  SyncEnter;
  try
    for nIdx:=0 to FObjects.Count - 1 do
    with TObjectBase(FObjects[nIdx]) do
    begin
      TObjectStatusHelper.AddTitle(nList, ClassName);
      GetStatus(nList);
    end;
  finally
    SyncLeave;
  end;
end;

initialization
  //nothing
finalization
  TCommonObjectManager.RegistMe(False);
end.
