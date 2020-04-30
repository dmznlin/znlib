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

unit scMsgDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Themes, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scControls, Vcl.StdCtrls,
  scStyledForm, scGPFontControls;

type
  TscMsgDlgForm = class(TForm)
    scStyledForm1: TscStyledForm;
    Image: TscGPCharImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

end.
