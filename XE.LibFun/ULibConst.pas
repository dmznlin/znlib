{*******************************************************************************
  作者: dmzn@163.com 2019-07-01
  描述: 通用函数库公共定义
*******************************************************************************}
unit ULibConst;

{$I LibFun.Inc}
interface

uses
  System.Classes, System.SysUtils;

//------------------------------------------------------------------------------
//*** 以下定义用于: Vcl.PostMsg,FMX.PostMsg
const
  cMessageBufferMax = 100;
  //max buffer record

type
  TMessageHandle = procedure (nSender: TObject; nMsg: Integer;
    nWParam,nLParam: NativeInt) of object;
  TMessageHandleRef = reference to procedure (nSender: TObject; nMsg: Integer;
    nWParam,nLParam: NativeInt);
  //call back function

  PMessageItem = ^TMessageItem;
  TMessageItem = record
    FEnabled   : Boolean;                              //有效标识
    FLastUsed  : Cardinal;                             //启用时间
    FHandle    : TMessageHandle;                       //消息事件
    FHandleRef : TMessageHandleRef;                    //匿名事件

    FSender    : TObject;                              //发起方
    FMsg       : Integer;                              //消息号
    FWParam    : NativeInt;
    FLParam    : NativeInt;                            //高低参数

    FIsFirst   : Boolean;                              //是否首项
    FNextItem  : Integer;                              //下项索引
  end;
  TMessageItems = array of TMessageItem;

implementation

end.
