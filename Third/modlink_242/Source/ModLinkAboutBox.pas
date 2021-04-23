{*****************************************************************}
{                                                                 }
{ ModLink                                                         }
{ Copyright (C) 2002 - 2013 Ing. Ivo Bauer                        }
{ All Rights Reserved.                                            }
{                                                                 }
{ Web site: http://www.ozm.cz/ivobauer/modlink/                   }
{ E-mail:   bauer@ozm.cz                                          }
{                                                                 }
{ For a detailed information regarding the distribution and use   }
{ of this software product, please refer to the License Agreement }
{ embedded in the accompanying online documentation (ModLink.chm) }
{                                                                 }
{*****************************************************************}

unit ModLinkAboutBox;

{$I ModLink.inc}

interface

//--------------------------------------------------------------------------------------------------

uses
  { Windows } Windows, Messages,
  { Delphi  } SysUtils, {$IFDEF COMPILER_6_UP} Variants , {$ENDIF COMPILER_6_UP}
              Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls;

//--------------------------------------------------------------------------------------------------

type
  TModLinkAboutForm = class(TForm)
    LogoImage: TImage;
    VersionLabel: TLabel;
    UnregisteredLabel: TLabel;
    CopyrightLabel: TLabel;
    CopyrightLabel2: TLabel;
    HomepageLabel: TLabel;
    EmailAuthorLabel2: TLabel;
    InformationLabel: TLabel;
    OKButton: TButton;
    HomepageTitleLabel: TLabel;
    EmailAuthorTitleLabel: TLabel;
    procedure HomepageLabelClick(Sender: TObject);
    procedure EmailAuthorLabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//--------------------------------------------------------------------------------------------------

var
  ModLinkAboutForm: TModLinkAboutForm;

//--------------------------------------------------------------------------------------------------

procedure ShowModLinkAboutBox;

//--------------------------------------------------------------------------------------------------

implementation

//--------------------------------------------------------------------------------------------------

uses
  { Windows } ShellAPI,
  { ModLink } ModLink;

//--------------------------------------------------------------------------------------------------

{$R *.dfm}

//--------------------------------------------------------------------------------------------------

procedure ShowModLinkAboutBox;
begin
  with TModLinkAboutForm.Create(nil) do
    try
      VersionLabel.Caption := Format('ModLink %s', [ModLinkVersion]);
      {$IFDEF MODLINK_SHAREWARE_VERSION}
      UnregisteredLabel.Visible := True;
      {$ELSE}
      UnregisteredLabel.Visible := False;
      {$ENDIF MODLINK_SHAREWARE_VERSION}
      ShowModal;
    finally
      Free;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure GoFor(const S: string);
begin
  if ShellExecute(0, 'open', PChar(S), PChar(nil), PChar(nil), 0) <= 32 then
    {$IFDEF COMPILER_6_UP} RaiseLastOSError {$ELSE} RaiseLastWin32Error {$ENDIF COMPILER_6_UP} ;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkAboutForm.HomepageLabelClick(Sender: TObject);
begin
  GoFor((Sender as TLabel).Caption);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkAboutForm.EmailAuthorLabelClick(Sender: TObject);
begin
  GoFor('mailto:' + (Sender as TLabel).Caption + '?subject=ModLink Inquiry');
end;

//--------------------------------------------------------------------------------------------------

end.
