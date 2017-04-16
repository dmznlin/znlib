{*******************************************************************************
  作者: dmzn@163.com 2017-03-23
  描述: 统一管理各种管理器的全局变量
*******************************************************************************}
unit UManagerGroup;

interface

uses
  System.Rtti, UBaseObject, UObjectPool, UMemDataPool;

type
  PManagerGroup = ^TManagerGroup;
  TManagerGroup = record
  public
    FSerialIDManager: TSerialIDManager;
    //编号管理器
    FObjectManager: TCommonObjectManager;
    //对象管理器
    FObjectPool: TObjectPoolManager;
    //对象缓冲池
    FMemDataManager: TMemDataManager;
    //内存管理器  
  public
    procedure RegistAll(const nReg: Boolean);
    //注册所有
  end;

var
  gMG: TManagerGroup;
  //全局使用
  
implementation

//Date: 2017-03-23
//Parm: 是否注册
//Desc: 扫描Group中所有Manager,调用Manager的注册方法.
procedure TManagerGroup.RegistAll(const nReg: Boolean);
var nCtx: TRttiContext;
    nType: TRttiType;
    nRF: TRttiField;
    nMethod: TRttiMethod;
    nInstance: TRttiInstanceType;
begin    
  nCtx := TRttiContext.Create;
  try
    nType := nCtx.GetType(TypeInfo(TManagerGroup));
    for nRF in nType.GetFields do
     if nRF.FieldType.TypeKind = tkClass then   
      begin
        nInstance := nRF.FieldType.AsInstance; 
        nMethod := nInstance.GetMethod('RegistMe');
        
        if Assigned(nMethod) then
          nMethod.Invoke(nInstance.MetaclassType, [TValue.From(nReg)]);
        //call function
      end;    
  finally
    nCtx.Free;
  end;
end;

initialization
  FillChar(gMG, SizeOf(TManagerGroup), #0);
  gMG.RegistAll(True);
finalization
  gMG.RegistAll(False);
end.
