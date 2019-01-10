{$DEFINE XE.LibFun}
//用于XE的库标识

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24}
    {$DEFINE XE2_UP}   
  {$ENDIF}
{$ENDIF}
//特定平台定义,用于特定处理

{$IF CompilerVersion >= 32}
  {$DEFINE JsonSerializers}
  //10.2 tokyo delphi支持序列化
{$ENDIF}

{-------------------------------------------------------------------------------
 以下功能模块,需要在项目中定义编译条件,操作方法(以Delphi_10.2为例):
 1.菜单 - Project - Options - Delphi Compiler - Compiling
 2.Target - All configurations - Conditional defines
 3.添加RemObjects开启RO支持,添加ThreadPool开启线程池等
-------------------------------------------------------------------------------}
{$IFNDEF HasFMX}
  {$DEFINE HasVCL}
{$ENDIF}
//组件类别

{$IFDEF RemObjects}
  {.$DEFINE RO_v90}
  //使用ver9.0的RemObjects
  {$DEFINE EnableChannelManager}
  //启用通道管理器
{$ENDIF}

{$IFDEF ThreadPool}
  {$DEFINE EnableThreadPool}
  //启用线程池
{$ENDIF}
