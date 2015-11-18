{*******************************************************************************
  作者: dmzn@163.com 2015-08-06
  描述: 注册管理系统对象的运行状态
*******************************************************************************}
unit UBaseObject;

interface

uses
  Windows, Classes, SysUtils, SyncObjs;

type
  TCommonObjectBase = class(TObject)
  private
    FStatusA: array [0..2] of string;
    FStatusB: array [0..2] of Pointer;
    //状态数据
  protected
    procedure GetStatus(const nList: TStrings); virtual; abstract;
    //对象状态
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    property StatusA0: string read FStatusA[0] write FStatusA[0];
    property StatusA1: string read FStatusA[1] write FStatusA[1];
    property StatusA2: string read FStatusA[2] write FStatusA[2];
    property StatusB0: Pointer read FStatusB[0] write FStatusB[0];
    property StatusB1: Pointer read FStatusB[1] write FStatusB[1];
    property StatusB2: Pointer read FStatusB[2] write FStatusB[2];
    //属性相关
  end;

  TCommonObjectManager = class(TObject)
  private
    FObjects: TList;
    //对象列表
    FSyncLock: TCriticalSection;
    //同步锁定
  public
    constructor Create;
    destructor Destroy; override;
    //创建释放
    procedure AddObject(const nObj: TObject);
    procedure DelObject(const nObj: TObject);
    //添加删除
    procedure GetStatus(const nList: TStrings);
    //获取状态
  end;

var
  gCommonObjectManager: TCommonObjectManager = nil;
  //全局使用

implementation

constructor TCommonObjectBase.Create;
begin
  if Assigned(gCommonObjectManager) then
    gCommonObjectManager.AddObject(Self);
  //xxxxx
end;

destructor TCommonObjectBase.Destroy;
begin
  if Assigned(gCommonObjectManager) then
    gCommonObjectManager.DelObject(Self);
  inherited;
end;

//------------------------------------------------------------------------------
constructor TCommonObjectManager.Create;
begin
  FObjects := TList.Create;
  FSyncLock := TCriticalSection.Create;
end;

destructor TCommonObjectManager.Destroy;
begin
  FObjects.Free;
  FSyncLock.Free;
  inherited;
end;

procedure TCommonObjectManager.AddObject(const nObj: TObject);
begin
  if not (nObj is TCommonObjectBase) then
    raise Exception.Create(ClassName + ': Object Is Not Support.');
  //xxxxx

  FSyncLock.Enter;
  FObjects.Add(nObj);
  FSyncLock.Leave;
end;

procedure TCommonObjectManager.DelObject(const nObj: TObject);
var nIdx: Integer;
begin
  FSyncLock.Enter;
  try
    nIdx := FObjects.IndexOf(nObj);
    if nIdx > -1 then
      FObjects.Delete(nIdx);
    //xxxxx
  finally
    FSyncLock.Leave;
  end;
end;

procedure TCommonObjectManager.GetStatus(const nList: TStrings);
var nIdx,nLen: Integer;
begin
  FSyncLock.Enter;
  try
    nList.BeginUpdate;
    nList.Clear;
    //init

    for nIdx:=0 to FObjects.Count - 1 do
    with TCommonObjectBase(FObjects[nIdx]) do
    begin
      if nIdx <> 0 then
        nList.Add('');
      //xxxxx

      nLen := Trunc((85 - Length(ClassName)) / 2);
      nList.Add(StringOfChar('+', nLen) + ' ' + ClassName + ' ' +
                StringOfChar('+', nLen));
      GetStatus(nList);
    end;
  finally
    nList.EndUpdate;
    FSyncLock.Leave;
  end;
end;

initialization
  gCommonObjectManager := nil;
finalization
  FreeAndNil(gCommonObjectManager);
end.
