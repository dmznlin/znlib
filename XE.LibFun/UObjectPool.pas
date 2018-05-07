{*******************************************************************************
  作者: dmzn@163.com 2017-03-21
  描述: 使已经创建的对象可以重复使用的对象池

  备注: 
  *.线程安全.
  *.TObjectPoolManager.Lock和Release必须配对使用.
*******************************************************************************}
unit UObjectPool;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, UBaseObject, ULibFun;

type
  TObjectNewOne = reference to function(var nData: Pointer): TObject;
  TObjectFreeOne = reference to procedure(const nObject: TObject;
    const nData: Pointer);
  //对象创建释放
  TObjectResetOne = reference to procedure(const nObject: TObject;
    const nData: Pointer);
  //对象重置数据

  PObjectPoolItem = ^TObjectPoolItem;
  TObjectPoolItem = record
    FObject: TObject;             //对象
    FData: Pointer;               //附加
    FUsed: Boolean;               //使用中
  end;

  PObjectPoolClass = ^TObjectPoolClass;
  TObjectPoolClass = record 
    FClass: TClass;               //类名
    FNewOne: TObjectNewOne;       //生成
    FFreeOne: TObjectFreeOne;     //释放
    FResetOne: TObjectResetOne;   //重置
      
    FNumLocked: Integer;          //已锁定
    FNumLockAll: Int64;           //请求次数
    FItems: TList;                //对象列表
  end;

  TObjectLockFilter = reference to function(const nObject: TObject;
    const nData: Pointer): Boolean;
  //锁定时筛选

  TObjectPoolManager = class(TManagerBase)
  private
    FPool: array of TObjectPoolClass;
    //对象池
    FNumLocked: Integer;
    FNumLockAll: Int64;
    //锁定对象
    FSrvClosed: Integer;
    //服务关闭 
  protected
    procedure ClearPool(const nFree: Boolean);
    //清理资源
    function FindPool(const nClass: TClass): Integer;
    //检索内容
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    class procedure RegistMe(const nReg: Boolean); override;
    //注册管理器
    function NewClass(const nClass: TClass; const nNew: TObjectNewOne;
      const nFree: TObjectFreeOne = nil;
      const nReset: TObjectResetOne = nil): Integer;
    procedure NewNormalClass;
    //注册类型
    function Lock(const nClass: TClass; const nNew: TObjectNewOne = nil;
      const nData: PPointer = nil;
      const nFilter: TObjectLockFilter = nil): TObject;
    procedure Release(const nObject: TObject; const nReset: Boolean = True);
    //锁定释放
    function GetData(const nClass: TClass; const nObj: TObject): Pointer;
    function SetData(const nClass: TClass; const nObj: TObject;
      const nData: Pointer): Boolean;
    //扩展数据
    procedure GetStatus(const nList: TStrings;
      const nFriendly: Boolean = True); override;
    function GetHealth(const nList: TStrings = nil): TObjectHealth; override;
    //获取状态
  end;

var
  gObjectPoolManager: TObjectPoolManager = nil;
  //全局使用
  
implementation

uses
  UManagerGroup;
  
const
  cYes  = $0002;
  cNo   = $0005;

//------------------------------------------------------------------------------
constructor TObjectPoolManager.Create;
begin
  inherited;
  FNumLocked := 0;
  FSrvClosed := cNo;
  
  NewNormalClass;
  //reg normal 
end;

destructor TObjectPoolManager.Destroy;
begin
  SyncEnter;
  FSrvClosed := cYes; //set close flag  
  SyncLeave;

  if FNumLocked > 0 then
  begin
    while FNumLocked > 0 do
      Sleep(1);
    //wait for relese
  end;
    
  ClearPool(True);
  inherited;
end;

//Date: 2017-03-23
//Parm: 是否注册
//Desc: 向系统注册管理器对象
class procedure TObjectPoolManager.RegistMe(const nReg: Boolean);
var nIdx: Integer;
begin
  nIdx := GetMe(TObjectPoolManager);
  if nReg then
  begin     
    if not Assigned(FManagers[nIdx].FManager) then
      FManagers[nIdx].FManager := TObjectPoolManager.Create;
    gMG.FObjectPool := FManagers[nIdx].FManager as TObjectPoolManager; 
  end else
  begin
    gMG.FObjectPool := nil;
    FreeAndNil(FManagers[nIdx].FManager);    
  end;
end;

//Desc: 清理对象池
procedure TObjectPoolManager.ClearPool(const nFree: Boolean);
var nIdx,i: Integer;
    nItem: PObjectPoolItem;
begin
  for nIdx := Low(FPool) to High(FPool) do
  with FPool[nIdx] do
  begin
    if Assigned(FItems) then
    begin
      for i := FItems.Count - 1 downto 0 do
      begin
        nItem := FItems[i];
        if Assigned(FFreeOne) then
        begin
          FFreeOne(nItem.FObject, nItem.FData);
          nItem.FObject := nil;
          nItem.FData := nil;
        end else
        begin
          FreeAndNil(nItem.FObject);
          //free default
        end;

        Dispose(nItem);
        FItems.Delete(i);
      end;

      FreeAndNil(FItems);
    end;
  end;

  if nFree then  
    SetLength(FPool, 0);
  //clear all
end;

//Date: 2017-03-23
//Parm: 类型 
//Desc: 检索nClass在对象池中的位置
function TObjectPoolManager.FindPool(const nClass: TClass): Integer;
var nIdx: Integer;
begin
  Result := -1;

  for nIdx := Low(FPool) to High(FPool) do
  if FPool[nIdx].FClass = nClass then
  begin
    Result := nIdx;
    Exit;
  end;
end;

//Date: 2017-03-23
//Parm: 类型;创建方法;释放方法;重置方法
//Desc: 注册nClass类到对象池
function TObjectPoolManager.NewClass(const nClass: TClass;
  const nNew: TObjectNewOne; const nFree: TObjectFreeOne;
  const nReset: TObjectResetOne): Integer;
begin
  SyncEnter;
  try
    Result := FindPool(nClass);
    if Result < 0 then
    begin
      Result := Length(FPool);
      SetLength(FPool, Result + 1);
      FillChar(FPool[Result], SizeOf(TObjectPoolClass), #0);
    end;

    with FPool[Result] do
    begin
      FClass := nClass;
      FNewOne := nNew;
      FFreeOne := nFree;
      FResetOne := nReset;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2017-03-23
//Desc: 注册常用类 
procedure TObjectPoolManager.NewNormalClass;
var nNewOne: TObjectNewOne;
    nResetOne: TObjectResetOne;
begin
  nNewOne :=
   function(var nData: Pointer):TObject begin Result := TStringList.Create; end;
  //create

  nResetOne := procedure(const nObject: TObject; const nData: Pointer)
    begin TStringList(nObject).Clear; end;
  //reset

  NewClass(TStrings, nNewOne, nil, nResetOne);
  NewClass(TStringList, nNewOne, nil, nResetOne);

  //----------------------------------------------------------------------------
  nNewOne :=
   function(var nData: Pointer):TObject begin Result := TList.Create; end;
  //xxxxx

  nResetOne := procedure(const nObject: TObject; const nData: Pointer)
    begin TList(nObject).Clear; end;
  //reset
  NewClass(TList, nNewOne, nil, nResetOne);
end;

//Date: 2017-03-23
//Parm: 对象类型;创建方法;扩展数据;筛选策略
//Desc: 返回nClass的对象指针
function TObjectPoolManager.Lock(const nClass:TClass; const nNew:TObjectNewOne;
  const nData: PPointer; const nFilter: TObjectLockFilter): TObject;
var nIdx,i: Integer;
    nItem: PObjectPoolItem;
begin
  SyncEnter;
  try    
    Result := nil; 
    if FSrvClosed = cYes then
      raise Exception.Create(ClassName + ': Not Support "Lock" When Closing.');
    //pool will close
                                
    nIdx := FindPool(nClass);
    if (not Assigned(nNew)) and ((nIdx < 0) or 
       (not Assigned(FPool[nIdx].FNewOne))) then
      raise Exception.Create(ClassName + ': Lock Object Need "Create" Method.');
    //xxxxx

    if nIdx < 0 then
      nIdx := NewClass(nClass, nNew);
    //xxxxx

    with FPool[nIdx] do
    begin
      if not Assigned(FItems) then
        FItems := TList.Create;
      //xxxxx

      if Assigned(nNew) and (not Assigned(FNewOne)) then
        FNewOne := nNew;
      //xxxxx

      for i := FItems.Count - 1 downto 0 do
      begin
        nItem := FItems[i];
        if (not nItem.FUsed) and (
           (not Assigned(nFilter)) or nFilter(nItem.FObject, nItem.FData)) then
        begin
          Result := nItem.FObject;
          nItem.FUsed := True;

          if Assigned(nData) then
            nData^ := nItem.FData;
          Break;
        end;
      end;

      if not Assigned(Result) then
      begin
        New(nItem);
        FItems.Add(nItem);
        FillChar(nItem^, SizeOf(TObjectPoolItem), #0);
        
        if Assigned(nNew) then         
             nItem.FObject := nNew(nItem.FData)
        else nItem.FObject := FNewOne(nItem.FData);

        Result := nItem.FObject;
        nItem.FUsed := True;

        if Assigned(nData) then
          nData^ := nItem.FData;
        //xxxxx
      end;
    end;

    Inc(FPool[nIdx].FNumLocked);
    Inc(Self.FNumLocked);
    //inc counter

    if FPool[nIdx].FNumLockAll < High(Int64) then      
         Inc(FPool[nIdx].FNumLockAll)
    else FPool[nIdx].FNumLockAll := 0;

    if Self.FNumLockAll < High(Int64) then
         Inc(Self.FNumLockAll)
    else Self.FNumLockAll := 0;    
  finally
    SyncLeave;
  end;
end;

//Date: 2017-03-23
//Parm: 对象;重置
//Desc: 释放对象
procedure TObjectPoolManager.Release(const nObject: TObject;
  const nReset: Boolean);
var nIdx,i: Integer;
    nItem: PObjectPoolItem; 
begin
  if not Assigned(nObject) then Exit;
  //nothing
  
  SyncEnter;
  try     
    for nIdx := Low(FPool) to High(FPool) do
    with FPool[nIdx] do
    begin
      if not (nObject is FClass) then Continue;
      //not match
            
      if Assigned(FItems) then         
      begin       
        for i := FItems.Count - 1 downto 0 do
        begin
          nItem := FItems[i];
          if nItem.FObject = nObject then
          begin
            Dec(FPool[nIdx].FNumLocked);
            Dec(Self.FNumLocked);
            nItem.FUsed := False;

            if nReset and Assigned(FResetOne) then
              FResetOne(nItem.FObject, nItem.FData);
            Exit;
          end;
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2018-05-31
//Parm: 对象类型;对象实例
//Desc: 获取nClass.nObj的扩展数据
function TObjectPoolManager.GetData(const nClass: TClass;
  const nObj: TObject): Pointer;
var nIdx,i: Integer;
    nItem: PObjectPoolItem;
begin
  SyncEnter;
  try
    Result := nil;
    nIdx := FindPool(nClass);
    if nIdx < 0 then Exit;

    with FPool[nIdx] do
    begin
      for i := FItems.Count - 1 downto 0 do
      begin
        nItem := FItems[i];
        if nItem.FObject = nObj then
        begin
          Result := nItem.FData;
          Break;
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2018-05-31
//Parm: 对象类型;对象实例;扩展数据
//Desc: 设置nClass.nObj的扩展数据
function TObjectPoolManager.SetData(const nClass: TClass; const nObj: TObject;
  const nData: Pointer): Boolean;
var nIdx,i: Integer;
    nItem: PObjectPoolItem;
begin
  SyncEnter;
  try
    Result := False;
    nIdx := FindPool(nClass);
    if nIdx < 0 then Exit;

    with FPool[nIdx] do
    begin
      for i := FItems.Count - 1 downto 0 do
      begin
        nItem := FItems[i];
        if nItem.FObject = nObj then
        begin
          nItem.FData := nData;
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2017-04-15
//Parm: 列表;是否友好显示
//Desc: 将管理器状态数据存入nList中
procedure TObjectPoolManager.GetStatus(const nList: TStrings;
  const nFriendly: Boolean);
var nIdx,nLen: Integer;    
begin
  with TObjectStatusHelper do
  try
    SyncEnter;
    inherited GetStatus(nList, nFriendly);
    
    if not nFriendly then
    begin
      nList.Add('NumPool=' + Length(FPool).ToString);
      nList.Add('NumLocked=' +  FNumLocked.ToString);
      nList.Add('NumLockAll=' + FNumLockAll.ToString);
      Exit;
    end;
                           
    nList.Add(FixData('NumPool:', Length(FPool)));
    nList.Add(FixData('NumLocked:', FNumLocked));
    nList.Add(FixData('NumLockAll:', FNumLockAll));
                                  
    for nIdx := Low(FPool) to High(FPool) do
    with FPool[nIdx] do
    begin           
      if Assigned(FItems) then
           nLen := FItems.Count
      else nLen := 0;
      
      nList.Add('');
      nList.Add(FixData(FClass.ClassName + '.NumAll:', nLen));
      nList.Add(FixData(FClass.ClassName + '.NumLocked:', FNumLocked));
      nList.Add(FixData(FClass.ClassName + '.NumLockAll:', FNumLockAll));
    end;
  finally
    SyncLeave;
  end;
end;

//Date: 2017-04-16
//Desc: 获取管理器健康度
function TObjectPoolManager.GetHealth(const nList: TStrings): TObjectHealth;
var nStr: string;
begin
  SyncEnter;
  try
    Result := hlNormal;
    if (FNumLocked >= 1000) and (Result < hlLow) then
    begin
      if Assigned(nList) then
      begin
        nStr := '已锁定对象[NumLocked: %d]过多,等待释放.';
        nList.Add(Format(nStr, [FNumLocked]));
      end;

      Result := hlLow;
    end;

    if (FNumLocked >= 5000) and (Result < hlBad) then
    begin
      if Assigned(nList) then
      begin
        nStr := '已锁定对象[NumLocked: %d]达到警戒值,请检查释放逻辑.';
        nList.Add(Format(nStr, [FNumLocked]));
      end;

      Result := hlBad;
    end;    
  finally
    SyncLeave;
  end;
end;

initialization
  //nothing
finalization
  //nothing
end.
