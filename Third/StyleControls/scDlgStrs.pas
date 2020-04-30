{*******************************************************************}
{                                                                   }
{       Almediadev Visual Component Library                         }
{       StyleControls                                               }
{       Version 4.62                                                }
{                                                                   }
{       Copyright (c) 2014-2020 Almediadev                          }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{       Home:  http://www.almdev.com                                }
{       Support: support@almdev.com                                 }
{                                                                   }
{*******************************************************************}

unit scDlgStrs;

interface

  uses
    Winapi.Windows, Winapi.Messages, System.Classes; 

var

  SC_S_Msg_Ok: String = 'Ok';
  SC_S_Msg_Cancel: String = 'Cancel';
  SC_S_Msg_Yes: String = 'Yes';
  SC_S_Msg_No: String = 'No';
  SC_S_Msg_Retry: String = 'Retry';
  SC_S_Msg_Ignore: String = 'Ignore';
  SC_S_Msg_Abort: String = 'Abort';
  SC_S_Msg_Help: String = 'Help';
  SC_S_Msg_Close: String = 'Close';
  
  function LoadStringFromDLL(ALib: HModule; AIdent: Integer; ADefValue: String): String;

implementation
  uses
   System.Types, System.SysUtils, System.UITypes;

  function LoadStringFromDLL(ALib: HModule; AIdent: Integer; ADefValue: String): String;
  var
    ResStringRec: TResStringRec;
  begin
    ResStringRec.Module := @ALib;
    ResStringRec.Identifier := AIdent;
    Result := LoadResString(@ResStringRec);
    if Result = '' then
      Result := ADefValue;
  end;

var
  Lib: HMODULE = 0;

initialization

  Lib := LoadLibrary(user32);
  if Lib <> 0 then
  begin
    SC_S_Msg_Ok := LoadStringFromDll(Lib, 800, SC_S_Msg_Ok);
    SC_S_Msg_Cancel := LoadStringFromDll(Lib, 801, SC_S_Msg_Cancel);
    SC_S_Msg_Yes := LoadStringFromDll(Lib, 805, SC_S_Msg_Yes);
    SC_S_Msg_No := LoadStringFromDll(Lib, 806, SC_S_Msg_No);
    SC_S_Msg_Retry := LoadStringFromDll(Lib, 803, SC_S_Msg_Retry);
    SC_S_Msg_Ignore := LoadStringFromDll(Lib, 804, SC_S_Msg_Ignore);
    SC_S_Msg_Abort := LoadStringFromDll(Lib, 802, SC_S_Msg_Abort);
    SC_S_Msg_Help := LoadStringFromDll(Lib, 904, SC_S_Msg_Help);
    SC_S_Msg_Close := LoadStringFromDll(Lib, 905, SC_S_Msg_Close);
    FreeLibrary(Lib);
  end;

end.
