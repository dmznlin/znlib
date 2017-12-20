//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
