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

unit scColorDialog;

interface

{$R-}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scDrawUtils, scControls, Vcl.StdCtrls,
  Vcl.Mask, scStyledForm, scColorControls;

type
  TscCustomColorValues = array[1..12] of TColor;

type
  TscColorDlgFrm = class(TForm)
    OKButton: TscButton;
    CancelButton: TscButton;
    scLabel1: TscLabel;
    scLabel2: TscLabel;
    scLabel3: TscLabel;
    scLabel4: TscLabel;
    scLabel5: TscLabel;
    scLabel6: TscLabel;
    scButton1: TscButton;
    scStyledForm1: TscStyledForm;
    ColorsPanel: TscPanel;
    ColorViewer: TscPanel;
    OldColorViewer: TscPanel;
    HEdit: TscTrackEdit;
    SEdit: TscTrackEdit;
    LEdit: TscTrackEdit;
    REdit: TscTrackEdit;
    GEdit: TscTrackEdit;
    BEdit: TscTrackEdit;
    scCustomColorGrid1: TscCustomColorGrid;
    scColorGrid1: TscColorGrid;
    scLColorPicker1: TscLColorPicker;
    scHSColorPicker1: TscHSColorPicker;
    procedure FormCreate(Sender: TObject);
    procedure scButton1Click(Sender: TObject);
  protected
    RGBStopCheck, HSLStopCheck,
    HSPickerStopCheck, LPickerStopCheck: Boolean;
    function GetColorValue: TColor;
    procedure SetColorValue(Value: TColor);
    procedure RGBEditChange(Sender: TObject);
    procedure HSLEditChange(Sender: TObject);
    procedure LEditChange(Sender: TObject);
    procedure ColorGridChange(Sender: TObject);
    procedure CustomColorGridChange(Sender: TObject);
    procedure HSColorPickerChange(Sender: TObject);
    procedure ColorPickerChange(Sender: TObject);
  end;

  TscColorDialog = class(TComponent)
  private
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FTitle: String;
    FColor: TColor;
    FDlgFrm: TscColorDlgFrm;
    FScaled: Boolean;
    FAnimationOnControls: Boolean;
  protected
    procedure SetAnimation;
    procedure Change;
  public
    CustomColors: TscCustomColorValues;
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  published
    property Title: String read FTitle write FTitle;
    property Scaled: Boolean read FScaled write FScaled;
    property AnimationOnControls: Boolean
      read FAnimationOnControls write FAnimationOnControls;
    property Color: TColor read FColor write FColor;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

var
  SC_S_ColorDlg_Title: String = 'Color';
  SC_S_ColorDlg_Add: String = 'Add';

implementation

Uses System.Math, System.Types, System.UITypes, scDlgStrs;

{$R *.dfm}

procedure DrawSelCross(x, y: Integer; ACanvas: TCanvas; Color: TColor);
const
  w = 5;
  h = 3;
  o = 8;
var
  R: TRect;
begin
  R := Rect(x - 10, y - 10, x + 9, y + 9);
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(Rect(R.Left, R.Top + o, R.Left + w, R.Top + o + h));
  ACanvas.FillRect(Rect(R.Left + o, R.Top, R.Left + o + h, R.Top + w));
  ACanvas.FillRect(Rect(R.Right - w, R.Top + o, R.Right, R.Top + o + h));
  ACanvas.FillRect(Rect(R.Left + o, R.Bottom - w, R.Left + o + h, R.Bottom));
end;

procedure TscColorDlgFrm.FormCreate(Sender: TObject);
begin
  if SC_RTLMODE then
    BidiMode := bdRightToLeft;

  Caption := SC_S_ColorDlg_Title;
  OKButton.Caption := SC_S_Msg_Ok;
  CancelButton.Caption := SC_S_Msg_Cancel;
  scCustomColorGrid1.OnChange := CustomColorGridChange;
  scColorGrid1.OnChange := ColorGridChange;
  scHSColorPicker1.OnChange := HSColorPickerChange;
  scLColorPicker1.OnChange := ColorPickerChange;
  REdit.OnChange := RGBEditChange;
  GEdit.OnChange := RGBEditChange;
  BEdit.OnChange := RGBEditChange;
  HEdit.OnChange := HSLEditChange;
  SEdit.OnChange := HSLEditChange;
  LEdit.OnChange := LEditChange;
end;

procedure TscColorDlgFrm.RGBEditChange(Sender: TObject);
var
  R, G, B: Byte;
  H, S, L: Integer;
begin
  if RGBStopCheck then Exit;
  R := REdit.Value;
  G := GEdit.Value;
  B := BEdit.Value;
  ColorViewer.Color := RGB(R, G, B);
  HSLStopCheck := True;
  RGBToHSLRange(ColorViewer.Color, H, S, L);
  HEdit.Value := H;
  SEdit.Value := S;
  LEdit.Value := L;
  HSLStopCheck := False;
  if not LPickerStopCheck then
  begin
    LPickerStopCheck := True;
    scLColorPicker1.SetSelectedColor(ColorViewer.Color);
    LPickerStopCheck := False;
  end;
  if not HSPickerStopCheck then
  begin
    HSPickerStopCheck := True;
    scHSColorPicker1.SetSelectedColor(ColorViewer.Color);
    HSPickerStopCheck := False;
  end;
end;

procedure TscColorDlgFrm.scButton1Click(Sender: TObject);
begin
  scCustomColorGrid1.AddColor(ColorViewer.Color);
end;

function TscColorDlgFrm.GetColorValue: TColor;
begin
  Result := ColorViewer.Color;
end;

procedure TscColorDlgFrm.SetColorValue(Value: TColor);
begin
  scHSColorPicker1.SetSelectedColor(Value);
  scColorGrid1.ColorValue := Value;
end;

procedure TscColorDlgFrm.HSColorPickerChange(Sender: TObject);
var
  R, G, B: Byte;
  H, S, L: Integer;
begin
  if HSPickerStopCheck then Exit;
  ColorViewer.Color := scHSColorPicker1.GetSelectedColor;

  ColorToR_G_B(ColorViewer.Color, R, G, B);
  RGBToHSLRange(ColorViewer.Color, H, S, L);

  HSLStopCheck := True;
  HEdit.Value := H;
  LEdit.Value := L;
  SEdit.Value := S;
  HSLStopCheck := False;

  RGBStopCheck := True;
  REdit.Value := R;
  GEdit.Value := G;
  BEdit.Value := B;
  RGBStopCheck := False;

  LPickerStopCheck := True;
  scLColorPicker1.SetSelectedColor(ColorViewer.Color);
  LPickerStopCheck := False;
end;

procedure TscColorDlgFrm.ColorPickerChange(Sender: TObject);
var
  R, G, B: Byte;
  H, S, L: Integer;
begin
  if LPickerStopCheck then Exit;

  ColorViewer.Color := scLColorPicker1.GetSelectedColor;

  ColorToR_G_B(ColorViewer.Color, R, G, B);
  RGBToHSLRange(ColorViewer.Color, H, S, L);

  HSLStopCheck := True;
  HEdit.Value := H;
  LEdit.Value := L;
  SEdit.Value := S;
  HSLStopCheck := False;

  RGBStopCheck := True;
  REdit.Value := R;
  GEdit.Value := G;
  BEdit.Value := B;
  RGBStopCheck := False;
end;

procedure TscColorDlgFrm.LEditChange(Sender: TObject);
begin
  if HSLStopCheck then Exit;
  scLColorPicker1.Luminance := LEdit.Value;
end;

procedure TscColorDlgFrm.HSLEditChange(Sender: TObject);
var
  R, G, B: Byte;
  H, S, L: Integer;
begin
  if HSLStopCheck then Exit;
  H := HEdit.Value;
  L := LEdit.Value;
  S := SEdit.Value;
  ColorViewer.Color := HSLRangeToRGB(H, S, L);
  ColorToR_G_B(ColorViewer.Color, R, G, B);
  RGBStopCheck := True;
  REdit.Value := R;
  GEdit.Value := G;
  BEdit.Value := B;
  RGBStopCheck := False;
  if not LPickerStopCheck then
  begin
    LPickerStopCheck := True;
    scLColorPicker1.SetSelectedColor(ColorViewer.Color);
    LPickerStopCheck := False;
  end;
  if not HSPickerStopCheck then
  begin
    HSPickerStopCheck := True;
    scHSColorPicker1.SetSelectedColor(ColorViewer.Color);
    HSPickerStopCheck := False;
  end;
end;

procedure TscColorDlgFrm.ColorGridChange(Sender: TObject);
var
  R, G, B: Byte;
  H, S, L: Integer;
begin
  ColorToR_G_B(scColorGrid1.ColorValue, R, G, B);
  RGBStopCheck := True;
  REdit.Value := R;
  GEdit.Value := G;
  BEdit.Value := B;
  RGBStopCheck := False;
  ColorViewer.Color := scColorGrid1.ColorValue;
  RGBtoHSLRange(ColorViewer.Color, H, S, L);
  HSLStopCheck := True;
  HEdit.Value := H;
  SEdit.Value := S;
  LEdit.Value := L;
  if not LPickerStopCheck then
  begin
    LPickerStopCheck := True;
    scLColorPicker1.SetSelectedColor(ColorViewer.Color);
    LPickerStopCheck := False;
  end;
  if not HSPickerStopCheck then
  begin
    HSPickerStopCheck := True;
    scHSColorPicker1.SetSelectedColor(ColorViewer.Color);
    HSPickerStopCheck := False;
  end;
  HSLStopCheck := False;
end;

procedure TscColorDlgFrm.CustomColorGridChange(Sender: TObject);
var
  R, G, B: Byte;
  H, S, L: Integer;
begin
  ColorToR_G_B(scCustomColorGrid1.ColorValue, R, G, B);
  RGBStopCheck := True;
  REdit.Value := R;
  GEdit.Value := G;
  BEdit.Value := B;
  RGBStopCheck := False;
  ColorViewer.Color := scCustomColorGrid1.ColorValue;
  RGBtoHSLRange(ColorViewer.Color, H, S, L);
  HSLStopCheck := True;
  HEdit.Value := H;
  SEdit.Value := S;
  LEdit.Value := L;
  if not LPickerStopCheck then
  begin
    LPickerStopCheck := True;
    scLColorPicker1.SetSelectedColor(ColorViewer.Color);
    LPickerStopCheck := False;
  end;
  if not HSPickerStopCheck then
  begin
    HSPickerStopCheck := True;
    scHSColorPicker1.SetSelectedColor(ColorViewer.Color);
    HSPickerStopCheck := False;
  end;
  HSLStopCheck := False;
end;

constructor TscColorDialog.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FAnimationOnControls := False;
  FScaled := True;
  FTitle := '';
  FColor := clBlack;
  for I := 1 to 12 do
    CustomColors[I] := clNone;
end;

procedure TscColorDialog.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscColorDialog.SetAnimation;
var
  I: Integer;
begin
  if FDlgFrm = nil then Exit;
  for I := 0 to FDlgFrm.ComponentCount - 1 do
  begin
    if FDlgFrm.Components[I] is TscButton then
      TscButton(FDlgFrm.Components[I]).Animation := FAnimationOnControls;
  end;
end;

function TscColorDialog.Execute: Boolean;
var
  IsChange: Boolean;
  I: Integer;
begin
  if Assigned(FOnShow) then FOnShow(Self);
  FDlgFrm := TscColorDlgFrm.Create(Application);
  SetAnimation;
  if FTitle <> '' then
    FDlgFrm.Caption := FTitle;
  IsChange := False;
  try
    FDlgFrm.OldColorViewer.Color := Self.Color;
    FDlgFrm.SetColorValue(Self.Color);
    for I := 1 to 12 do
      FDlgFrm.scCustomColorGrid1.CustomColorValues[I] := Self.CustomColors[I];
    Result := (FDlgFrm.ShowModal = mrOk);
    if Result then
    begin
      for I := 1 to 12 do
         CustomColors[I] := FDlgFrm.scCustomColorGrid1.CustomColorValues[I];
      if FColor <> FDlgFrm.GetColorValue then
      begin
        FColor := FDlgFrm.GetColorValue;
        IsChange := True;
      end;
    end;
  finally
    FDlgFrm.Free;
    if IsChange then
      Change;
    if Assigned(FOnClose) then
      FOnClose(Self);
  end;
end;


var
  Lib: HModule = 0;

initialization

  Lib := LoadLibrary('compstui.dll');
  if Lib <> 0 then
  begin
    SC_S_ColorDlg_Title := LoadStringFromDll(Lib, 64744, SC_S_ColorDlg_Title);
    FreeLibrary(Lib);
  end;

finalization

end.
