{$DEFINE XE.LibFun}
//用于XE的库标识

{$IF CompilerVersion >= 32}
  {$DEFINE JsonSerializers}
  //10.2 tokyo delphi支持序列化
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE MSWin}
  //平台标记
  {$IFDEF WIN32}
    {$DEFINE UseASM}
    //使用汇编语法
  {$ENDIF}
{$ENDIF}
//WIN平台定义

{$IFDEF NEXTGEN}
  {$DEFINE Mobile}
  //平台标识
  {$DEFINE EnableThirdANSI}
  //使用三方ansi字符串
  {$ZEROBASEDSTRINGS OFF}
  //关闭字符串0索引,从1开始
{$ENDIF}
//移动平台定义

{-------------------------------------------------------------------------------
 以下功能模块,需要在项目中定义编译条件,操作方法(以Delphi_10.2为例):
 1.菜单 - Project - Options - Delphi Compiler
 2.Target - All configurations - Conditional defines
 3.添加RemObjects开启RO支持,添加ThreadPool开启线程池等
-------------------------------------------------------------------------------}
{$IFNDEF NoLinkConfig}
  {$Include Link.Inc}
{$ENDIF}
//不使用项目配置文件

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

{$IFDEF AutoParams}
  {$DEFINE EnableParamManager}
  //启用参数管理器
{$ENDIF}

{$IFDEF TaskMonitor}
  {$DEFINE LogManager}
  //启用日志管理器
  {$DEFINE ThreadPool}
  //启用线程池
  {$DEFINE EnableTaskMonitor}
  //启用任务监控服务
{$ENDIF}

{$IFDEF LogManager}
  {$DEFINE ThreadPool}
  //启用线程池
  {$DEFINE EnableLogManager}
  //启用日志管理器
{$ENDIF}

{$IFDEF ThreadPool}
  {$DEFINE EnableThreadPool}
  //启用线程池
{$ENDIF}
