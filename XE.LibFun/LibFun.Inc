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

{$DEFINE RemObjects}
{$IFDEF RemObjects}
  {.$DEFINE RO_v90}
  //使用ver9.0的RemObjects
  {$DEFINE EnableChannelManager}
  //启用通道管理器
{$ENDIF}
