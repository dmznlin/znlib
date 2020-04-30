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

unit scFontDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Themes, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scControls, Vcl.StdCtrls, scExtControls,
  scStyledForm, scColorDialog, scDrawUtils;

type
  TscFontDlgFrm = class(TForm)
    NameComboBox: TscFontComboBox;
    OKButton: TscButton;
    CancelButton: TscButton;
    scStyledForm1: TscStyledForm;
    ColorButton: TscColorButton;
    scColorDialog1: TscColorDialog;
    ExamplePanel: TscPanel;
    BoldCheckBox: TscCheckBox;
    ItalicCheckBox: TscCheckBox;
    UnderLineCheckBox: TscCheckBox;
    StrikeOutCheckBox: TscCheckBox;
    SizeComboBox: TscFontSizeComboBox;
    procedure FormCreate(Sender: TObject);
    procedure NameComboBoxClick(Sender: TObject);
    procedure BoldCheckBoxClick(Sender: TObject);
    procedure ItalicCheckBoxClick(Sender: TObject);
    procedure UnderLineCheckBoxClick(Sender: TObject);
    procedure StrikeOutCheckBoxClick(Sender: TObject);
    procedure ColorButtonChangeColor(Sender: TObject);
    procedure SizeComboBoxChange(Sender: TObject);
    procedure scStyledForm1ChangeScale(AScaleFactor: Double);
  private
    { Private declarations }
    FSetting: Boolean;
  public
    { Public declarations }
    FFontHeight: Integer;
    procedure SetDialogFont(AFont: TFont);
    procedure CheckColor;
  end;

  TscFontDialog = class(TComponent)
  private
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FTitle: String;
    FFont: TFont;
    FDlgFrm: TscFontDlgFrm;
    FScaled: Boolean;
    FAnimationOnControls: Boolean;
    FFontListOptions: TscFontListOptions;
    FHideSizeBox: Boolean;
    FHideColorButton: Boolean;
    FColorDialog: TColorDialog;
    FColorDialogCustom: TscColorDialog;
    FPreviewText: String;
    procedure SetFont(Value: TFont);
  protected
    procedure SetAnimation;
    procedure Change;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
  public
    CustomColors: TscCustomColorValues;
    CustomButtonColors: TscCustomColorButtonValues;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property ColorDialog: TColorDialog read FColorDialog write FColorDialog;
    property ColorDialogCustom: TscColorDialog read FColorDialogCustom write FColorDialogCustom;
    property Title: String read FTitle write FTitle;
    property PreviewText: String read FPreviewText write FPreviewText;
    property Scaled: Boolean read FScaled write FScaled;
    property AnimationOnControls: Boolean
      read FAnimationOnControls write FAnimationOnControls;
    property FontListOptions: TscFontListOptions
      read FFontListOptions write FFontListOptions default [foTrueTypeOnly];
    property Font: TFont read FFont write SetFont;
    property HideSizeBox: Boolean
      read FHideSizeBox write FHideSizeBox default False;
    property HideColorButton: Boolean
      read FHideColorButton write FHideColorButton default False;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

var
  SC_S_MoreColor: String = 'Custom';

implementation
  uses
    scDlgStrs;

{$R *.dfm}

var
  Lib: HModule = 0;

procedure TscFontDlgFrm.FormCreate(Sender: TObject);
begin
  if SC_RTLMODE then
    BidiMode := bdRightToLeft;

  ColorButton.MoreCaption := SC_S_MoreColor + '...';
  OkButton.Caption := SC_S_Msg_OK;
  CancelButton.Caption := SC_S_Msg_Cancel;
end;

procedure TscFontDlgFrm.ItalicCheckBoxClick(Sender: TObject);
begin
 if FSetting then Exit;

 if ItalicCheckBox.Checked then
    ExamplePanel.Font.Style := ExamplePanel.Font.Style + [fsItalic]
  else
    ExamplePanel.Font.Style := ExamplePanel.Font.Style - [fsItalic];
end;

procedure TscFontDlgFrm.NameComboBoxClick(Sender: TObject);
begin
  if FSetting then Exit;

  if SizeComboBox <> nil then
  begin
    SizeComboBox.FontName := NameComboBox.FontName;
    ExamplePanel.Font.Name := NameComboBox.FontName;
  end;
end;

procedure TscFontDlgFrm.scStyledForm1ChangeScale(AScaleFactor: Double);
begin
  ExamplePanel.Font.Height := FFontHeight;
end;

procedure TscFontDlgFrm.CheckColor;
var
  C: TColor;
begin
  C := ColorToRGB(ExamplePanel.Font.Color);
  if C = GetStyleColor(clBtnFace) then
  begin
    C := GetStyleColor(clBtnFace);
    if IsDarkStyle then
      C := LighterColor(C, 30)
    else
      C := DarkerColor(C, 30);
    ExamplePanel.StyleElements := ExamplePanel.StyleElements - [seClient];
    ExamplePanel.Color := C;
    ExamplePanel.LightBorderColor := GetStyleColor(clBtnHighLight);
    ExamplePanel.ShadowBorderColor := GetStyleColor(clBtnShadow);
  end
  else
  begin
    if not (seClient in ExamplePanel.StyleElements) then
    begin
      ExamplePanel.StyleElements := ExamplePanel.StyleElements + [seClient];
      ExamplePanel.Color := clBtnFace;
      ExamplePanel.LightBorderColor := clBtnHighLight;
      ExamplePanel.ShadowBorderColor := clBtnShadow;
    end;
  end;
end;

procedure TscFontDlgFrm.ColorButtonChangeColor(Sender: TObject);
begin
  if FSetting then Exit;
  ExamplePanel.Font.Color := ColorButton.ColorValue;
  CheckColor;
end;

procedure TscFontDlgFrm.BoldCheckBoxClick(Sender: TObject);
begin
  if FSetting then Exit;

  if BoldCheckBox.Checked then
    ExamplePanel.Font.Style := ExamplePanel.Font.Style + [fsBold]
  else
    ExamplePanel.Font.Style := ExamplePanel.Font.Style - [fsBold];
end;

procedure TscFontDlgFrm.SizeComboBoxChange(Sender: TObject);
begin
  if FSetting then Exit;
   ExamplePanel.Font.Size := SizeComboBox.SizeValue;
end;

procedure TscFontDlgFrm.StrikeOutCheckBoxClick(Sender: TObject);
begin
  if FSetting then Exit;

  if StrikeOutCheckBox.Checked then
    ExamplePanel.Font.Style := ExamplePanel.Font.Style + [fsStrikeOut]
  else
    ExamplePanel.Font.Style := ExamplePanel.Font.Style - [fsStrikeOut];
end;

procedure TscFontDlgFrm.UnderLineCheckBoxClick(Sender: TObject);
begin
  if FSetting then Exit;
  
  if UnderLineCheckBox.Checked then
    ExamplePanel.Font.Style := ExamplePanel.Font.Style + [fsUnderLine]
  else
    ExamplePanel.Font.Style := ExamplePanel.Font.Style - [fsUnderLine];
end;

procedure TscFontDlgFrm.SetDialogFont(AFont: TFont);
begin
  FSetting := True;
  ExamplePanel.Font.Assign(AFont);
  NameComboBox.FontName := ExamplePanel.Font.Name;
  SizeComboBox.FontName := ExamplePanel.Font.Name;
  SizeComboBox.Text := IntToStr( ExamplePanel.Font.Size);
  BoldCheckBox.Checked := fsBold in ExamplePanel.Font.Style;
  ItalicCheckBox.Checked := fsItalic in ExamplePanel.Font.Style;
  UnderLineCheckBox.Checked := fsUnderLine in ExamplePanel.Font.Style;
  StrikeOutCheckBox.Checked := fsStrikeOut in ExamplePanel.Font.Style;
  ColorButton.ColorValue := ExamplePanel.Font.Color;
  FSetting := False;
  CheckColor;
end;

constructor TscFontDialog.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FColorDialog := nil;
  FColorDialogCustom := nil;
  FFontListOptions := [foTrueTypeOnly];
  FAnimationOnControls := False;
  FHideSizeBox := False;
  FHideColorButton := False;
  FScaled := True;
  FTitle := '';
  FFont := TFont.Create;
  FPreviewText := 'AaBbYyZz';
  with FFont do
  begin
    Name := 'Tahoma';
    Style := [];
    Size := 8;
  end;
  for i := 1 to 12 do
    CustomColors[I] := clNone;
  for i := 1 to 8 do
    CustomButtonColors[I] := clWhite;
end;

destructor TscFontDialog.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TscFontDialog.Notification;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FColorDialog)
  then
    FColorDialog := nil;
  if (Operation = opRemove) and (AComponent = FColorDialogCustom)
  then
    FColorDialogCustom := nil;
end;

procedure TscFontDialog.SetFont;
begin
  FFont.Assign(Value);
end;

procedure TscFontDialog.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscFontDialog.SetAnimation;
var
  I: Integer;
begin
  if FDlgFrm = nil then Exit;
  for I := 0 to FDlgFrm.ComponentCount - 1 do
  begin
    if FDlgFrm.Components[I] is TscButton then
      TscButton(FDlgFrm.Components[I]).Animation := FAnimationOnControls
    else
    if FDlgFrm.Components[I] is TscCheckBox then
      TscCheckBox(FDlgFrm.Components[I]).Animation := FAnimationOnControls;
  end;
end;

function TscFontDialog.Execute: Boolean;
var
  IsChange: Boolean;
  I: Integer;
begin
  if Assigned(FOnShow) then FOnShow(Self);
  FDlgFrm := TscFontDlgFrm.Create(Application);
  SetAnimation;
  FDlgFrm.scColorDialog1.Scaled := FScaled;
  FDlgFrm.NameComboBox.Options := FontListOptions;
  FDlgFrm.ColorButton.Visible := not FHideColorButton;
  FDlgFrm.ColorButton.ColorDialog := FColorDialog;
  FDlgFrm.ColorButton.ColorDialogCustom := FColorDialogCustom;
  FDlgFrm.SizeComboBox.Visible := not FHideSizeBox;
  FDlgFrm.FFontHeight := Self.Font.Height;
  if FPreviewText <> '' then
    FDlgFrm.ExamplePanel.Caption := FPreviewText;
  if FTitle <> '' then
    FDlgFrm.Caption := FTitle;
  IsChange := False;
  try
    FDlgFrm.SetDialogFont(Self.Font);
    for I := 1 to 12 do
      FDlgFrm.scColorDialog1.CustomColors[I] := Self.CustomColors[I];
    for I := 1 to 8 do
      FDlgFrm.ColorButton.CustomColors[I] := Self.CustomButtonColors[I];
    Result := (FDlgFrm.ShowModal = mrOk);
    if Result then
    begin
      Self.Font.Assign(FDlgFrm.ExamplePanel.Font);
      IsChange := True;
      for I := 1 to 12 do
        Self.CustomColors[I] := FDlgFrm.scColorDialog1.CustomColors[I];
      for I := 1 to 8 do
        Self.CustomButtonColors[I] := FDlgFrm.ColorButton.CustomColors[I];
    end;
  finally
     FDlgFrm.Free;
     if IsChange then
       Change;
     if Assigned(FOnClose) then
       FOnClose(Self);
  end;
end;


initialization

  Lib := LoadLibrary('comdlg32.dll');
  if Lib <> 0 then
  begin
    SC_S_MoreColor := LoadStringFromDll(Lib, 1056, SC_S_MoreColor);
    FreeLibrary(Lib);
  end;

end.
