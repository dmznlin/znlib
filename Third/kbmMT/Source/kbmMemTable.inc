{$ifdef NEXTGEN}
 {$LEGACYIFEND ON}
{$endif}


{$ifdef FPC}         // FreePascal
  {$I kbmFPC.inc}
  {$define KBMMEMTABLE_DONT_SUPPORT_TIMESTAMPPOPULATION}
  {$define KBMMEMTABLE_SUPPORT_FT_FIXEDWIDECHAR}
  {$define KBMMEMTABLE_SUPPORT_FT_WIDEMEMO}
  {$define KBMMEMTABLE_DONT_SUPPORT_PUBLISHED_RECORDS}
  {$define KBMMEMTABLE_PUBLIC_FIELDDATA_SETTER_GETTER}
  {$define LEVEL5}
  {$define LEVEL6}
  {$define LEVEL7}
  {$define LEVEL9}
  {$define LEVEL10}
  {$define LEVEL11}
  {$define LEVEL12}
  {$define LEVEL13}
  {$define LEVEL14}
  {$define LEVEL15}
  {$define DELPHI}
{$endif}

{$ifdef VER100}      // Delphi 3
  {$define LEVEL3}
  {$define DELPHI}
  {$define KBMMEMTABLE_SUPPORT_BDE}
{$endif}

{$ifdef VER110}      // C++ Builder 3
  {$define LEVEL3}
  {$define BCB}
{$endif}

{$ifdef VER120}      // Delphi 4
  {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
  {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
  {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
  {$define KBMMEMTABLE_SUPPORT_BDE}
  {$define LEVEL4}
  {$define DELPHI}
{$endif}

{$ifdef VER125}      // C++ Builder 4
  {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
  {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
  {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
  {$define KBMMEMTABLE_SUPPORT_BDE}
  {$define LEVEL4}
  {$define BCB}
{$endif}

{$ifdef VER130}      // Delphi 5 or Builder 5
  {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
  {$define KBMMEMTABLE_SUPPORT_EXPRESSIONPARSER}
  {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
  {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
  {$define KBMMEMTABLE_PUBLIC_FIELDDATA_SETTER_GETTER}
  {$define KBMMEMTABLE_SUPPORT_BDE}
  {$define LEVEL4}
  {$define LEVEL5}
  {$ifndef BCB}        // Delphi 5
    {$define DELPHI}
  {$else}              // Builder 5
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
{$endif}

{$ifdef VER140}      // Delphi/BCB 6 & Kylix 1/2/3.
  {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
  {$define KBMMEMTABLE_SUPPORT_EXPRESSIONPARSER}
  {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
  {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
  {$define LEVEL4}
  {$define LEVEL5}
  {$define LEVEL6}
  {$ifndef BCB}        // Delphi 6
    {$define DELPHI}
  {$else}              // Builder 6
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$ifdef LINUX}
    {$ifdef CONDITIONALEXPRESSIONS}  // Figure out which Kylix.
      {$if Declared(CompilerVersion) and (CompilerVersion=14.10)}
        {$define KYLIX2}
	{$ifend}
      {$if Declared(CompilerVersion) and (CompilerVersion=14.50)}
        {$define KYLIX3}
	{$ifend}
    {$ELSE}
      {$define KYLIX1}
    {$endif}
  {$else}
   {$define KBMMEMTABLE_SUPPORT_BDE}
  {$endif}
{$endif}

{$ifdef VER150}      // Delphi 7.
  {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
  {$define KBMMEMTABLE_SUPPORT_EXPRESSIONPARSER}
  {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
  {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
  {$define LEVEL4}
  {$define LEVEL5}
  {$define LEVEL6}
  {$define LEVEL7}
  {$ifndef BCB}        // Delphi 7
    {$define DELPHI}
  {$else}              // Builder 7
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$define KBMMEMTABLE_SUPPORT_BDE}
  {$ifdef LINUX}
    Sorry this level of Kylix not yet supported.
  {$endif}
{$endif}

{$ifdef VER170}      // Delphi 9.
  {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
  {$define KBMMEMTABLE_SUPPORT_EXPRESSIONPARSER}
  {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
  {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
  {$define LEVEL4}
  {$define LEVEL5}
  {$define LEVEL6}
  {$define LEVEL7}
  {$define LEVEL9}
  {$ifndef BCB}        // Delphi 9/BDS 2005
    {$define DELPHI}
  {$else}
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$define KBMMEMTABLE_SUPPORT_BDE}
  {$ifdef LINUX}
    Sorry this level of Kylix not yet supported.
  {$endif}
{$endif}

{$ifdef VER180}      // Delphi 10/BDS 2006.
  {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
  {$define KBMMEMTABLE_SUPPORT_EXPRESSIONPARSER}
  {$define KBMMEMTABLE_SUPPORT_FT_FIXEDWIDECHAR}
  {$define KBMMEMTABLE_SUPPORT_FT_WIDEMEMO}
  {$define KBMMEMTABLE_SUPPORT_FT_ORAINTERVAL}
  {$define KBMMEMTABLE_SUPPORT_FT_ORATIMESTAMP}
  {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
  {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
  {$define KBMMEMTABLE_USE_WSTRLEN}
  {$define LEVEL4}
  {$define LEVEL5}
  {$define LEVEL6}
  {$define LEVEL7}
  {$define LEVEL9}
  {$define LEVEL10}
  {$ifndef BCB}        // Delphi 10/BDS 2006
    {$define DELPHI}
  {$else}
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  {$define KBMMEMTABLE_SUPPORT_BDE}
  {$ifdef LINUX}
    Sorry this level of Kylix not yet supported.
  {$endif}
{$endif}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 18.5} // Spacely / Delphi 2007 for Win32
    {$DEFINE SPACELY_UP}
    {$define KBMMEMTABLE_SUPPORT_NESTEDFIELDS}
    {$define KBMMEMTABLE_SUPPORT_EXPRESSIONPARSER}
    {$define KBMMEMTABLE_SUPPORT_FT_FIXEDWIDECHAR}
    {$define KBMMEMTABLE_SUPPORT_FT_WIDEMEMO}
    {$define KBMMEMTABLE_SUPPORT_FT_ORAINTERVAL}
    {$define KBMMEMTABLE_SUPPORT_FT_ORATIMESTAMP}
    {$define KBMMEMTABLE_SUPPORT_SETBLOCKREADSIZE}
    {$define KBMMEMTABLE_SUPPORT_FIELDFULLNAME}
    {$define KBMMEMTABLE_USE_WSTRLEN}
    {$define KBMMEMTABLE_SUPPORT_INLINE}
    {$define LEVEL4}
    {$define LEVEL5}
    {$define LEVEL6}
    {$define LEVEL7}
    {$define LEVEL9}
    {$define LEVEL10}
    {$DEFINE LEVEL11}
  {$IFEND}
  {$IF CompilerVersion >= 19.0} // Highlander / CodeGear RAD Studio 2007 for Win32/.Net
    {$DEFINE HIGHLANDER_UP}
    {$DEFINE LEVEL12}
  {$IFEND}
  {$IF CompilerVersion >= 20}   // Tiburon / Delphi 2009
    {$DEFINE TIBURON_UP}
    {$DEFINE LEVEL12}
    {$DEFINE LEVEL13}
    {$DEFINE KBMMEMTABLE_SUPPORT_ASBYTES}
    {$DEFINE KBMMEMTABLE_SUPPORT_UNICODESTRINGS}
    {$DEFINE KBMMEMTABLE_USE_TRECORDBUFFER}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_LONGWORD}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_SHORTINT}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_BYTE}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_EXTENDED}
   {$IFNDEF NEXTGEN}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_UNICODE}
   {$ENDIF}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_CONNECTION}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_STREAM}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_PARAMS}
  {$IFEND}
  {$IF CompilerVersion >= 21}   // Weaver / Delphi 2010
    {$DEFINE WEAVER_UP}
    {$DEFINE LEVEL12}
    {$DEFINE LEVEL13}
    {$DEFINE LEVEL14}
    {$DEFINE KBMMEMTABLE_SUPPORT_UNICODESTRINGS}
    {$DEFINE KBMMEMTABLE_USE_TRECORDBUFFER}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_TIMESTAMPOFFSET}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_OBJECT}
    {$DEFINE KBMMEMTABLE_SUPPORT_FT_SINGLE}
  {$IFEND}
  {$IF CompilerVersion >= 22}   // Fulcrum / Delphi XE
    {$DEFINE FULCRUM_UP}
    {$DEFINE LEVEL15}
  {$IFEND}
  {$IF CompilerVersion >= 23}   // Pulsar / Delphi XE2
    {$DEFINE PULSAR_UP}
    {$DEFINE LEVEL16}
  {$IFEND}
  {$IF CompilerVersion >= 24}   // Waterdragon / Delphi XE3
    {$DEFINE WATERDRAGON_UP}
    {$DEFINE LEVEL17}
    {$DEFINE KBMMEMTABLE_USE_TVALUEBUFFER}
    {$DEFINE KBMMEMTABLE_USE_TBOOKMARK}
  {$IFEND}
  {$IF CompilerVersion >= 25}   // Quintessence / Delphi XE4
    {$DEFINE QUINTESSENCE_UP}
    {$DEFINE LEVEL18}
    {$DEFINE KBMMEMTABLE_USE_VAR_GETFIELDDATA}
  {$IFEND}
  {$IF CompilerVersion >= 26}   // Zephyr / Delphi XE5
    {$DEFINE ZEPHYR_UP}
    {$DEFINE LEVEL19}
  {$IFEND}
  {$IF CompilerVersion >= 27}   // Proteus / Delphi XE6
    {$DEFINE PROTEUS_UP}
    {$DEFINE LEVEL21} // LEVEL20 is reserved to AppMethod v1
  {$IFEND}
  {$IF CompilerVersion >= 28}   // Carpathia / Delphi XE7
    {$DEFINE CARPATHIA_UP}
    {$DEFINE LEVEL22} 
  {$IFEND}
  {$IF CompilerVersion >= 29}   // Elbrus / Delphi XE8
    {$DEFINE ELBRUS_UP}
    {$DEFINE LEVEL23}
  {$IFEND}
  {$IF CompilerVersion >= 30}   // Seattle / Delphi 10
    {$DEFINE SEATTLE_UP}
    {$DEFINE LEVEL24}
  {$IFEND}
  {$IF CompilerVersion >= 31}   // Berlin / Delphi 10.1
    {$DEFINE BERLIN_UP}
    {$DEFINE LEVEL25}
  {$IFEND}
  {$IF CompilerVersion >= 32}   // Tokyo / Delphi 10.2
    {$DEFINE TOKYO_UP}
    {$DEFINE LEVEL26}
  {$IFEND}
  {$ifndef BCB}
    {$define DELPHI}
  {$else}
    {$undef DELPHI}	     // ( Ensure Delphi off )
  {$endif}
  
  {$IF CompilerVersion<28} // From XE7/Carpathia, BDE is no longer part of primary installer.
    {$define KBMMEMTABLE_SUPPORT_BDE}
  {$IFEND}
{$ENDIF}

{$ifdef CLR}
 {$define DOTNET}
  .Net is not supported with current version of kbmMemTable.
  Please use an earlier version.
{$ENDIF}

//***********************************************************************

{$DEFINE KBMMEMTABLE_USE_DSDESIGN}
{$ifdef LEVEL7}
 {$WARN UNSAFE_CODE OFF}
 {$WARN UNSAFE_TYPE OFF}
 {$WARN UNSAFE_CAST OFF}
{$endif}
{$ifdef LEVEL6}
 {$WARN SYMBOL_PLATFORM OFF}
{$endif}

{$B-}    // Enable short circuit evaluation.

{$ifdef BCB}
{$ObjExportAll On}
{$ASSERTIONS ON}
{$endif}


