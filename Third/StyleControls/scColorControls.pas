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

unit scColorControls;

interface

{$R-}
{$I scdefine.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scDrawUtils, scControls, Vcl.StdCtrls,
  Vcl.Mask;

type
  TscCustomColorValues = array[1..12] of TColor;

type

  TscCustomColorGrid = class(TscPanel)
  private
    FColorValue: TColor;
    FOnChange: TNotifyEvent;
    FColCount, FRowCount: Integer;
    FColorIndex: Integer;
    procedure SetColCount(Value: Integer);
    procedure SetRowCount(Value: Integer);
  protected
    procedure DrawCursor(ACanvas: TCanvas; ARect: TRect);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure PaintGrid(ACanvas: TCanvas);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    CustomColorValues: TscCustomColorValues;
    FColorsCount: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddColor(AColor: TColor);
  published
    property RowCount: Integer read FRowCount write SetRowCount;
    property ColCount: Integer read FColCount write SetColCount;
    property ColorValue: TColor read FColorValue;
    property OnChange: TNotifyEvent  read FOnChange write FOnChange;
  end;

  TscColorGrid = class(TscPanel)
  private
    FColorValue: TColor;
    FOnChange: TNotifyEvent;
    FColCount, FRowCount: Integer;
    procedure SetColCount(Value: Integer);
    procedure SetRowCount(Value: Integer);
    procedure SetColorValue(Value: TColor);
  protected
    procedure DrawCursor(ACanvas: TCanvas; ARect: TRect);
    function CheckColor(Value: TColor): boolean;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure PaintGrid(ACanvas: TCanvas);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RowCount: Integer read FRowCount write SetRowCount;
    property ColCount: Integer read FColCount write SetColCount;
    property ColorValue: TColor read FColorValue write SetColorValue;
    property OnChange: TNotifyEvent  read FOnChange write FOnChange;
  end;

 TscHSColorPicker = class(TscCustomControl)
 private
   FSelected: TColor;
   FHSLBmp: TBitmap;
   FOnChange: TNotifyEvent;
   FHue, FSaturation, FLuminance: Integer;
   FLum: Integer;
   FMousePos: TPoint;
 protected
   procedure SetHValue(h: Integer);
   procedure SetSValue(s: Integer);
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure DrawMarker(ACanvas: TCanvas; X, Y: Integer);
   procedure CreateHSLGradient;
   procedure Resize; override;
   procedure CreateWnd; override;
   procedure CorrectPosition(var X, Y: Integer);
   procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   function GetColorAtPoint(x, y: Integer): TColor;
   procedure SetSelectedColor(Value: TColor);
   function GetSelectedColor: TColor;
   property Lum: Integer read FLum write FLum default 120;
 published
   property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
   property HueValue: Integer read FHue write SetHValue default 0;
   property SaturationValue: Integer read FSaturation write SetSValue default 240;
   property OnChange: TNotifyEvent read FOnChange write FOnChange;
 end;

 type

 TscColorTrackBarKind = (scctkHorizontal, scctkVertical);
 TscColorSliderPos = (sccspBefore, sccspAfter, sccspBoth);
 TscSelIndicatorKind = (scsiArrows, scsiRect);

 TscTrackBarPicker = class(TscCustomControl)
 private
   FMousePos: TPoint;
   FOnChange: TNotifyEvent;
   FIncrement: Integer;
   FKind: TscColorTrackBarKind;
   FPlacement: TscColorSliderPos;
   TrackW, TrackH: Integer;
   FDoChange: boolean;
   FSelIndicator: TscSelIndicatorKind;
   FWebSafe: boolean;
   function XToArrowPos(p: Integer): Integer;
   function YToArrowPos(p: Integer): Integer;
   procedure SetKind(Value: TscColorTrackBarKind);
   procedure SetPlacement(Value: TscColorSliderPos);
   procedure DrawMarker(ACanvas: TCanvas; p: Integer);
   procedure SetSelIndicatorKind(Value: TscSelIndicatorKind);
   procedure CalcPickRect;
 protected
   FArrowPos: Integer;
   FChange: boolean;
   FPickRect: TRect;
   FLimit: Integer;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure Resize; override;
   procedure CreateWnd; override;
   procedure DoMouseMove; virtual;
   procedure DoMouseUp; virtual;
   procedure DoMouseDown; virtual;
   procedure DoResize; virtual;
   procedure DoInitBmp; virtual;
   function GetArrowPos: Integer; dynamic;
   function GetSelectedValue: Integer; virtual; abstract;
   procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
   procedure PaintTrackBar(ACanvas: TCanvas); virtual;
 public
   constructor Create(AOwner: TComponent); override;
 published
   property Increment: Integer read FIncrement write FIncrement default 1;
   property Kind: TscColorTrackBarKind read FKind write SetKind default scctkHorizontal;
   property ArrowPlacement: TscColorSliderPos read FPlacement write SetPlacement default sccspAfter;
   property SelectionIndicator: TscSelIndicatorKind read FSelIndicator write SetSelIndicatorKind default scsiArrows;
   property ShowHint;
   property Anchors;
   property Align;
   property Visible;
   property Enabled;
   property PopupMenu;
   property TabOrder;
   property DragCursor;
   property DragMode;
   property DragKind;
   property Constraints;
   property OnChange: TNotifyEvent read FOnChange write FOnChange;
   property OnContextPopup;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnMouseWheel;
   property OnMouseWheelUp;
   property OnMouseWheelDown;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnResize;
   property OnStartDrag;
 end;

 TscLColorPicker = class(TscTrackBarPicker)
 private
  FHue, FSat, FLuminance: Integer;
  FLBmp: TBitmap;
  function ArrowPosFromLum(l: Integer): Integer;
  function LumFromArrowPos(p: Integer): Integer;
  procedure CreateLGradient;
  procedure SetHue(h: Integer);
  procedure SetSat(s: Integer);
  procedure SetLuminance(l: Integer);
 protected
  procedure CreateWnd; override;
  function GetArrowPos: Integer; override;
  function GetSelectedValue: Integer; override;
  procedure PaintTrackBar(ACanvas: TCanvas); override;

  procedure DoMouseMove; override;
  procedure DoMouseUp; override;
  procedure DoMouseDown; override;
  procedure DoResize; override;
  procedure DoInitBmp; override;

 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure SetSelectedColor(c: TColor);
  function GetSelectedColor: TColor;
 published
  property Hue: Integer read FHue write SetHue default 0;
  property Saturation: Integer read FSat write SetSat default 240;
  property Luminance: Integer read FLuminance write SetLuminance default 120;
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clRed;
  property Kind default scctkVertical;
 end;

 function HSLtoRGBColor(H, S, L: Double): TColor;
 procedure RGBtoHSLRange(RGB: TColor; var H1, S1, L1 : Integer);
 procedure ColorToR_G_B(C: TColor; var R, G, B: Byte);
 function HSLRangeToRGB(H, S, L : Integer): TColor;

implementation

  Uses System.Math, System.Types, System.UITypes;
type
  TRGBTripleArray = array [0..0] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBQuadArray = array [0..0]  of TRGBQuad;
  pRGBQuadArray = ^TRGBQuadArray;

const
  ColorValues: array[1..48] of TColor =
   (0, 64, 128, 4210816, 255, 8421631, 32896, 16512, 33023, 4227327, 65535, 8454143,
    4227200, 16384, 32768, 65280, 65408, 8454016, 8421504, 4210688, 4227072, 8421376, 4259584, 8453888,
    8421440, 8388608, 16711680, 8404992, 16776960, 16777088, 12632256, 4194304, 10485760, 16744576, 12615680, 16744448,
    4194368, 5194368, 8388736, 4194432, 12615808, 12615935, 16777215, 8388672, 16711808, 8388863, 16711935, 16744703);

var
  MaxHue: Integer = 239;
  MaxSat: Integer = 240;
  MaxLum: Integer = 240;


function HSLtoRGBColor(H, S, L: Double): TColor;
var
 M1, M2: Double;

  function HueToColorValue(Hue: Double): byte;
  var
   V : Double;
  begin
   if Hue < 0 then
    Hue := Hue + 1
   else
    if Hue > 1 then
     Hue := Hue - 1;
   if 6 * Hue < 1 then
    V := M1 + (M2 - M1) * Hue * 6
   else
    if 2 * Hue < 1 then
     V := M2
    else
     if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2/3 - Hue) * 6
     else
      V := M1;
   Result := round (255 * V)
  end;

var
 R, G, B: byte;
begin
 if S = 0 then
  begin
   R := round (MaxLum * L);
   G := R;
   B := R
  end
 else
  begin
   if L <= 0.5 then
    M2 := L * (1 + S)
   else
    M2 := L + S - L * S;
   M1 := 2 * L - M2;
   R := HueToColorValue (H + 1/3);
   G := HueToColorValue (H);
   B := HueToColorValue (H - 1/3)
  end;
 Result := RGB (R, G, B)
end;

function HSLRangeToRGB(H, S, L : Integer): TColor;
begin
 if s > MaxSat then s := MaxSat;
 if s < 0 then s := 0;
 if l > MaxLum then l := MaxLum;
 if l < 0 then l := 0;
 Result := HSLToRGBColor(H / MaxHue, S / MaxSat, L / MaxLum);
end;

procedure RGBtoHSLRange(RGB: TColor; var H1, S1, L1 : Integer);
var
  R, G, B, D, Cmax, Cmin, h, s, l: Double;
begin
 R := GetRValue (RGB) / 255;
 G := GetGValue (RGB) / 255;
 B := GetBValue (RGB) / 255;
 Cmax := Max (R, Max (G, B));
 Cmin := Min (R, Min (G, B));
 L := (Cmax + Cmin) / 2;
 if Cmax = Cmin then
  begin
   H := 0;
   S := 0;
  end
 else
  begin
   D := Cmax - Cmin;
   //calc L
   if L < 0.5 then
    S := D / (Cmax + Cmin)
   else
    S := D / (2 - Cmax - Cmin);
   //calc H
   if R = Cmax then
    H := (G - B) / D
   else
    if G = Cmax then
     H  := 2 + (B - R) /D
    else
     H := 4 + (R - G) / D;
   H := H / 6;
   if H < 0 then
    H := H + 1;
  end;
 H1 := round (H * MaxHue);
 S1 := round (S * MaxSat);
 L1 := round (L * MaxLum);
end;

function RGBtoRGBQuad(R, G, B: byte): TRGBQuad; overload;
begin
 with Result do
  begin
   rgbRed := R;
   rgbGreen := G;
   rgbBlue := B;
   rgbReserved := 0;
  end
end;

function RGBToRGBQuad(c: TColor): TRGBQuad; overload;
begin
 with Result do
  begin
   rgbRed := GetRValue(c);
   rgbGreen := GetGValue(c);
   rgbBlue := GetBValue(c);
   rgbReserved := 0
  end;
end;

function RGBQuadToRGB(q: TRGBQuad): TColor;
begin
 Result := RGB(q.rgbRed, q.rgbGreen, q.rgbBlue);
end;

function RGBTripleToTColor(RGBTriple: TRGBTriple): TColor;
begin
 Result := RGBTriple.rgbtBlue shl 16 + RGBTriple.rgbtGreen shl 8 + RGBTriple.rgbtRed;
end;

procedure ColorToR_G_B(C: TColor; var R, G, B: Byte);
begin
  R := C and $FF;
  G := (C shr 8) and $FF;
  B := (C shr 16) and $FF;
end;

procedure DrawSelCross(x, y: Integer; ACanvas: TCanvas; Color: TColor; AScaleFactor: Double);
var
  R: TRect;
  w, h, o, s: Integer;
begin
  w := 5;
  h := 3;
  o := 8;
  s := 10;
  if AScaleFactor > 1.5 then
  begin
    w := w * 2;
    h := h * 2;
    o := o * 2;
    s := s * 2;
  end;
  R := Rect(x - s, y - s, x + s - 1, y + s - 1);
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(Rect(R.Left, R.Top + o, R.Left + w, R.Top + o + h));
  ACanvas.FillRect(Rect(R.Left + o, R.Top, R.Left + o + h, R.Top + w));
  ACanvas.FillRect(Rect(R.Right - w, R.Top + o, R.Right, R.Top + o + h));
  ACanvas.FillRect(Rect(R.Left + o, R.Bottom - w, R.Left + o + h, R.Bottom));
end;


constructor TscCustomColorGrid.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FCanEmpty := False;
  StyleKind := scpsTransparent;
  BorderStyle := scpbsNone;
  Width := 280;
  Height := 115;
  FColorValue := 0;
  FColCount := 6;
  FRowCount := 2;
  for i := 1 to 12 do
    CustomColorValues[I] := clNone;
  FColorsCount := 0;
  FColorIndex := 0;
end;

destructor TscCustomColorGrid.Destroy;
begin
  inherited;
end;

procedure TscCustomColorGrid.AddColor(AColor: TColor);
var
  I: Integer;
  B: Boolean;
begin
  B := False;
  for I := 1 to 12 do
    if CustomColorValues[I] = AColor then
    begin
      Exit;
    end
    else
    if CustomColorValues[I] = clNone then
    begin
      FColorsCount := I;
      B := True;
      Break;
    end;
  if not B then
    Inc(FColorsCount);
  if FColorsCount > 12 then FColorsCount := 1;
  CustomColorValues[FColorsCount] := AColor;
  RePaintControl;
end;

procedure TscCustomColorGrid.SetColCount(Value: Integer);
begin
  if Value < 1 then Exit;
  FColCount := Value;
  RePaintControl;
end;

procedure TscCustomColorGrid.SetRowCount(Value: Integer);
begin
  FRowCount := Value;
  RePaintControl;
end;

procedure TscCustomColorGrid.DrawCursor;
var
  C: TColor;
begin
  C := scDrawUtils.GetStyleColor(clBtnText);
  scDrawUtils.Frm3D(ACanvas, ARect, C, C);
  scDrawUtils.Frm3D(ACanvas, ARect, C, C);
end;

procedure TscCustomColorGrid.PaintGrid(ACanvas: TCanvas);
var
  RX, RY, X, Y, CW, CH, i, j, k: Integer;
  R, R1, Rct: TRect;
  C: TColor;
begin
  R := Rect(0, 0, Width, Height);
  AdjustClientRect(R);
  CW := (R.Width - ColCount * 2) div ColCount;
  CH := (R.Height - RowCount * 2) div RowCount;

  R1 := Rect(0, 0, (CW + 2) * ColCount, (CH + 2) * RowCount);
  RX := R.Left + R.Width div 2 - R1.Width div 2;
  RY := R.Top + R.Height div 2 - R1.Height div 2;
  R := Rect(RX, RY, RX + R1.Width, R1.Height);

  Y := R.Top + 1;
  k := 0;
  for i := 1 to RowCount do
  begin
    X := R.Left + 1;
    for j := 1 to ColCount do
    begin
      Inc(k);
      with ACanvas do
      begin
        if CustomColorValues[k] = clNone then
          Brush.Color := clWhite
        else
          Brush.Color := CustomColorValues[k];

        Rct := Rect(X - 1, Y -1 , X + CW + 1, Y + CH + 1);

        if (k = FColorIndex)
        then
          begin
            DrawCursor(ACanvas, Rct);
          end;

        if (k = FColorIndex)
        then
          Rct := Rect(X + 3, Y + 3 , X + CW - 3, Y + CH - 3)
        else
          Rct := Rect(X + 2, Y + 2 , X + CW - 2, Y + CH - 2);

        InflateRect(Rct, -1, -1);
        FillRect(Rct);
        C :=  scDrawUtils.GetStyleColor(clBtnShadow);
        scDrawUtils.Frm3D(ACanvas, Rct, C, C);
        InflateRect(Rct, 1, 1);
      end;
      Inc(X, CW + 2);
    end;
    Inc(Y, CH + 2);
  end;
end;

procedure TscCustomColorGrid.Draw;
begin
  inherited;
  PaintGrid(ACanvas);
end;

procedure TscCustomColorGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                     X, Y: Integer);
var
  RX, RY, X1, Y1, CW, CH, i, j, k: Integer;
  R, R1, Rct: TRect;
begin
  inherited;
  R := Rect(0, 0, Width, Height);
  AdjustClientRect(R);
  CW := (R.Width - ColCount * 2) div ColCount;
  CH := (R.Height - RowCount * 2) div RowCount;

  R1 := Rect(0, 0, (CW + 2) * ColCount, (CH + 2) * RowCount);
  RX := R.Left + R.Width div 2 - R1.Width div 2;
  RY := R.Top + R.Height div 2 - R1.Height div 2;
  R := Rect(RX, RY, RX + R1.Width, R1.Height);

  Y1 := R.Top + 1;
  k := 0;
  for i := 1 to RowCount do
  begin
    X1 := R.Left + 1;
    for j := 1 to ColCount do
    begin
      Inc(k);
      Rct := Rect(X1, Y1, X1 + CW, Y1 + CH);
      if PtInRect(Rct, Point(X, Y))
      then
        begin
          if CustomColorValues[k] = clNone then
            FColorValue := clWhite
          else
            FColorValue := CustomColorValues[k];
          FColorIndex := k;
          RePaintControl;
          if Assigned(FOnChange) then FOnChange(Self);
          Break;
        end;
      Inc(X1, CW + 2);
    end;
    Inc(Y1, CH + 2);
  end;
end;

constructor TscColorGrid.Create(AOwner: TComponent);
begin
  inherited;
  FCanEmpty := False;
  StyleKind := scpsTransparent;
  BorderStyle := scpbsNone;
  Width := 280;
  Height := 115;
  FColorValue := 0;
  FColCount := 12;
  FRowCount := 4;
end;

destructor TscColorGrid.Destroy;
begin
  inherited;
end;

procedure TscColorGrid.SetColCount(Value: Integer);
begin
  if Value < 1 then Exit;
  FColCount := Value;
  RePaintControl;
end;

procedure TscColorGrid.SetRowCount(Value: Integer);
begin
  FRowCount := Value;
  RePaintControl;
end;

procedure TscColorGrid.DrawCursor;
var
  C: TColor;
begin
  C :=  scDrawUtils.GetStyleColor(clBtnText);
  scDrawUtils.Frm3D(ACanvas, ARect, C, C);
  scDrawUtils.Frm3D(ACanvas, ARect, C, C);
end;

procedure TscColorGrid.PaintGrid(ACanvas: TCanvas);
var
  RX, RY, X, Y, CW, CH, i, j, k: Integer;
  R, R1, Rct: TRect;
  C: TCOlor;
begin
  R := Rect(0, 0, Width, Height);
  AdjustClientRect(R);
  CW := (R.Width - ColCount * 2) div ColCount;
  CH := (R.Height - RowCount * 2) div RowCount;
  R1 := Rect(0, 0, (CW + 2) * ColCount, (CH + 2) * RowCount);
  RX := R.Left + R.Width div 2 - R1.Width div 2;
  RY := R.Top + R.Height div 2 - R1.Height div 2;
  R := Rect(RX, RY, RX + R1.Width, R1.Height);
  Y := R.Top + 1;
  k := 0;
  for i := 1 to RowCount do
  begin
    X := R.Left + 1;
    for j := 1 to ColCount do
    begin
      Inc(k);
      with ACanvas do
      begin
        Rct := Rect(X - 1, Y - 1, X + CW + 1, Y + CH + 1);

        if FColorValue = ColorValues[k]
        then
          begin
            DrawCursor(ACanvas, Rct);
          end;

        Brush.Color := ColorValues[k];

        if FColorValue = ColorValues[k]
        then
          Rct := Rect(X + 3, Y + 3 , X + CW - 3, Y + CH - 3)
        else
          Rct := Rect(X + 2, Y + 2 , X + CW - 2, Y + CH - 2);

        InflateRect(Rct, -1, -1);
        FillRect(Rct);
        C :=  scDrawUtils.GetStyleColor(clBtnShadow);
        scDrawUtils.Frm3D(ACanvas, Rct, C, C);
        InflateRect(Rct, 1, 1);
      end;
      Inc(X, CW + 2);
    end;
    Inc(Y, CH + 2);
  end;
end;

procedure TscColorGrid.Draw;
begin
  inherited;
  PaintGrid(ACanvas);
end;

function TscColorGrid.CheckColor(Value: TColor): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to 48 do
    if ColorValues[I] = Value
    then
      begin
        Result := True;
        Break;
      end;
end;

procedure TscColorGrid.SetColorValue(Value: TColor);
begin
  FColorValue := Value;
  if CheckColor(FColorValue)
  then
    begin
      if Assigned(FOnChange) then FOnChange(Self);
      RePaintControl;
    end;
end;

procedure TscColorGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                     X, Y: Integer);
var
  RX, RY, X1, Y1, CW, CH, i, j, k: Integer;
  R, R1, Rct: TRect;
begin
  inherited;
  R := Rect(0, 0, Width, Height);
  AdjustClientRect(R);
  CW := (R.Width - ColCount * 2) div ColCount;
  CH := (R.Height - RowCount * 2) div RowCount;

  R1 := Rect(0, 0, (CW + 2) * ColCount, (CH + 2) * RowCount);
  RX := R.Left + R.Width div 2 - R1.Width div 2;
  RY := R.Top + R.Height div 2 - R1.Height div 2;
  R := Rect(RX, RY, RX + R1.Width, R1.Height);

  Y1 := R.Top + 1;
  k := 0;
  for i := 1 to RowCount do
  begin
    X1 := R.Left + 1;
    for j := 1 to ColCount do
    begin
      Inc(k);
      Rct := Rect(X1, Y1, X1 + CW, Y1 + CH);
      if PtInRect(Rct, Point(X, Y))
      then
        begin
          ColorValue := ColorValues[k];
          Break;
        end;
      Inc(X1, CW + 2);
    end;
    Inc(Y1, CH + 2);
  end;
end;

constructor TscHSColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FTransparentBackground := False;
  FHSLBmp := TBitmap.Create;
  FHSLBmp.PixelFormat := pf32bit;
  FHSLBmp.SetSize(240, 241);
  Width := 240;
  Height := 240;
  FHue := 0;
  FSaturation := 240;
  FLuminance := 120;
  FSelected := clRed;
  FLum := 120;
  FMousePos.X := 0;
  FMousePos.Y := 0;
end;

destructor TscHSColorPicker.Destroy;
begin
  FHSLBmp.Free;
  inherited Destroy;
end;

procedure TscHSColorPicker.CreateWnd;
begin
  inherited;
  CreateHSLGradient;
end;

procedure TscHSColorPicker.CreateHSLGradient;
var
  Hue, Sat : Integer;
  row: pRGBQuadArray;
begin
 if FHSLBmp = nil then
  begin
   FHSLBmp := TBitmap.Create;
   FHSLBmp.PixelFormat := pf32bit;
   FHSLBmp.Width := 239;
   FHSLBmp.Height := 240;
  end;
 for Hue := 0 to 239 do
  for Sat := 0 to 240 do
   begin
     row := FHSLBmp.ScanLine[240 - Sat];
     row[Hue] := RGBToRGBQuad(HSLRangeToRGB(Hue, Sat, 120));
   end;
end;

procedure TscHSColorPicker.CorrectPosition(var X, Y: Integer);
begin
 if X < 0 then X := 0;
 if Y < 0 then y := 0;
 if X > Width - 1 then X := Width - 1;
 if Y > Height - 1 then Y := Height - 1;
end;

procedure TscHSColorPicker.DrawMarker(ACanvas: TCanvas; X, Y: Integer);
begin
  CorrectPosition(X, Y);
  RGBtoHSLRange(FSelected, FHue, FSaturation, FLuminance);
  if Assigned(FOnChange) then
    FOnChange(Self);
  DrawSelCross(X, Y, ACanvas, clBlack, FScaleFactor);
end;

function TscHSColorPicker.GetSelectedColor: TColor;
begin
  Result := FSelected;
end;

procedure TscHSColorPicker.SetSelectedColor(Value: TColor);
begin
  RGBtoHSLRange(Value, FHue, FSaturation, FLuminance);
  FSelected := Value;
  FMousePos.X := Round(FHue*(Width/239));
  FMousePos.Y := Round((240-FSaturation)*(Height/240));
  RePaintControl;
end;

procedure TscHSColorPicker.Draw;
begin
  ACanvas.StretchDraw(ClientRect, FHSLBmp);
  CorrectPosition(FMousePos.X, FMousePos.Y);
  DrawMarker(ACanvas, FMousePos.X, FMousePos.Y);
end;

procedure TscHSColorPicker.Resize;
begin
  SetSelectedColor(FSelected);
  inherited;
end;

procedure TscHSColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  FMousePos.X := X;
  FMousePos.Y := Y;
  if Button = mbLeft then
  begin
    R := ClientRect;
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    FSelected := GetColorAtPoint(X, Y);
    RePaintControl;
  end;
end;

procedure TscHSColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMousePos.X := X;
  FMousePos.Y := Y;
  FSelected := GetColorAtPoint(X, Y);
  RePaintControl;
end;

procedure TscHSColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (ssLeft in Shift)then
  begin
    FMousePos.X := X;
    FMousePos.Y := Y;
    RePaintControl;
    FSelected := GetColorAtPoint(x, y);
  end;
end;

procedure TscHSColorPicker.SetHValue(h: Integer);
begin
  if h > 239 then h := 239;
  if h < 0 then h := 0;
  FHue := h;
  SetSelectedColor(HSLRangeToRGB(FHue, FSaturation, 120));
end;

procedure TscHSColorPicker.SetSValue(s: Integer);
begin
  if s > 240 then s := 240;
  if s < 0 then s := 0;
  FSaturation := s;
  SetSelectedColor(HSLRangeToRGB(FHue, FSaturation, 120));
end;

function TscHSColorPicker.GetColorAtPoint(X, Y: Integer): TColor;
begin
  CorrectPosition(X, Y);
  Result := Canvas.Pixels[x, y];
end;


constructor TscTrackBarPicker.Create(AOwner: TComponent);
begin
  inherited;
  FTransparentBackground := True;
  Width := 267;
  Height := 22;
  ParentShowHint := True;
  FMousePos.X := 0;
  FMousePos.Y := 0;
  FIncrement := 1;
  FArrowPos := GetArrowPos;
  FChange := True;
  FKind := scctkHorizontal;
  TrackW := 6;
  TrackH := 10;
  FPlacement := sccspAfter;
  FPickRect := Rect(TrackW, 0, Width - TrackW, Height - TrackH);
  FDoChange := False;
  FSelIndicator := scsiArrows;
  FLimit := 7;
  FWebSafe := False;
end;

procedure TscTrackBarPicker.CreateWnd;
begin
  inherited;
  CalcPickRect;
end;

procedure TscTrackBarPicker.CalcPickRect;
var
 f: Integer;
begin
 case FSelIndicator of
  scsiArrows:
  begin
    f := 0;
    TrackW := Round(6 * FScaleFactor);
    TrackH := Round(10 * FScaleFactor);
    FLimit := Round(7 * FScaleFactor);
  end;
 scsiRect:
 begin
   f := 0;
   TrackW := 4;
   TrackH := 5;
   FLimit := 3;
 end
 else
   f := 0;
 end;

 case FKind of
  scctkHorizontal:
   case FSelIndicator of
    scsiArrows:
     case FPlacement of
        sccspAfter: FPickRect := Rect(TrackW, 0, Width - TrackW, Height - TrackH - f);
        sccspBefore: FPickRect := Rect(TrackW, TrackH + f, Width - TrackW, Height);
        sccspBoth: FPickRect := Rect(TrackW, TrackH + f, Width - TrackW, Height - TrackH - f);
     end;
     scsiRect: FPickRect := Rect(TrackW, TrackH, width - 2 * TrackW + 1, height - TrackH);
   end;
  scctkVertical:
   case FSelIndicator of
    scsiArrows:
     case FPlacement of
        sccspAfter: FPickRect := Rect(0, TrackW, Width - TrackH - f, Height - TrackW);
        sccspBefore: FPickRect := Rect(TrackH + f, TrackW, Width, Height - TrackW);
        sccspBoth: FPickRect := Rect(TrackH + f, TrackW, Width - TrackH - f, Height - TrackW);
     end;
     scsiRect: FPickRect := Rect(TrackH, TrackW, width - 5, height - 2 * TrackW + 1);
   end;
 end;
end;

procedure TscTrackBarPicker.PaintTrackBar(ACanvas: TCanvas);
begin
end;

procedure TscTrackBarPicker.Draw;
begin
  CalcPickRect;
  FArrowPos := GetArrowPos;
  PaintTrackBar(ACanvas);
  DrawMarker(ACanvas, FArrowPos);
  if FDoChange then
  begin
    if Assigned(FOnChange) then FOnChange(Self);
    FDoChange := False;
  end;
end;

procedure TscTrackBarPicker.DrawMarker(ACanvas: TCanvas; p: Integer);
var
 x, y, w: Integer;
 R: TRect;
begin
  case FSelIndicator of
  scsiRect:
   begin
    case FKind of
     scctkHorizontal:
      begin
       p := p + TrackW;
       R := Rect(p - 2, 2, p + 3, Height - 2);
      end;
     scctkVertical:
      begin
       p := p + TrackW;
       R := Rect(2, p - 2, Width - 2, p + 3);
      end;
    end;
    ACanvas.Pen.Mode := pmNot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(R);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Pen.Mode := pmCopy;
   end;
  scsiArrows:
   begin
     ACanvas.Brush.Color := scDrawUtils.GetStyleColor(clBtnText);
     ACanvas.Pen.Color := scDrawUtils.GetStyleColor(clBtnText);
     if FKind = scctkHorizontal then
     begin
      x := p + TrackW;
      if x < TrackW then x := TrackW;
      if x > Width - TrackW then x := Width - TrackW;
      w := Round(5 * FScaleFactor);
      case FPlacement of
       sccspAfter:
        begin
          y := Height - TrackW - Round(2 * FScaleFactor);
          ACanvas.Polygon([Point(x, y), Point(x - w, y + w), Point(x + w, y + w)]);
        end;
       sccspBefore:
        begin
          y := TrackW + Round(2 * FScaleFactor) - 1;
          ACanvas.Polygon([Point(x, y), Point(x - w, y - w), Point(x + w, y - w)]);
        end;
       sccspBoth:
        begin
          y := Height - TrackW - Round(2 * FScaleFactor);
          ACanvas.Polygon([Point(x, y), Point(x - w, y + w), Point(x + w, y + w)]);
          y := TrackW + Round(2 * FScaleFactor) - 1;
          ACanvas.Polygon([Point(x, y), Point(x - w, y - w), Point(x + w, y - w)]);
        end;
      end;
     end
    else
     begin
      y := p + TrackW;
      if y < TrackW then y := TrackW;
      if y > Height - TrackW - 1 then y := Height - TrackW - 1;
      w := Round(5 * FScaleFactor);
      case FPlacement of
       sccspAfter:
        begin
          x := width - TrackW - Round(2 * FScaleFactor);
          ACanvas.Polygon([Point(x, y), Point(x + w, y - w), Point(x + w, y + w)]);
        end;
       sccspBefore:
        begin
          x := TrackW + Round(2 * FScaleFactor) - 1;
          ACanvas.Polygon([Point(x, y), Point(x - w, y - w), Point(x - w, y + w)]);
        end;
       sccspBoth:
        begin
          x := width - TrackW - Round(2 * FScaleFactor);
          ACanvas.Polygon([Point(x, y), Point(x + w, y - w), Point(x + w, y + w)]);
          x := TrackW + Round(2 * FScaleFactor) - 1;
          ACanvas.Polygon([Point(x, y), Point(x - w, y - w), Point(x - w, y + w)]);
        end;
      end;
     end;
    end;
  end;
end;

procedure TscTrackBarPicker.Resize;
begin
  inherited;
  FChange := False;
  DoResize;
  FChange := True;
end;

function TscTrackBarPicker.XToArrowPos(p: Integer): Integer;
var
  pos: Integer;
begin
  pos := p - TrackW;
  if pos < 0 then pos := 0;
  if pos > Width - TrackW - 1 then pos := Width - TrackW - 1;
  Result := pos;
end;

function TscTrackBarPicker.YToArrowPos(p: Integer): Integer;
var
  pos: Integer;
begin
  pos := p - TrackW;
  if pos < 0 then pos := 0;
  if pos > Height - TrackW - 1 then pos := Height - TrackW - 1;
  Result := pos;
end;

procedure TscTrackBarPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 R: TRect;
begin
  if ssLeft in shift then
  begin
    R := ClientRect;
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    FMousePos.X := X;
    FMousePos.Y := Y;
    if FKind = scctkHorizontal then
      FArrowPos := XToArrowPos(x)
     else
      FArrowPos := YToArrowPos(y);
     DoMouseMove;
     FDoChange := True;
     RePaintControl;
  end;
  inherited;
end;

procedure TscTrackBarPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  FMousePos.X := X;
  FMousePos.Y := Y;
  if FKind = scctkHorizontal then
    FArrowPos := XToArrowPos(X)
  else
    FArrowPos := YToArrowPos(Y);
  DoMouseDown;
  FDoChange := True;
  RePaintControl;
  inherited;
end;

procedure TscTrackBarPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  FMousePos.X := X;
  FMousePos.Y := Y;
  if FKind = scctkHorizontal then
    FArrowPos := XToArrowPos(X)
   else
    FArrowPos := YToArrowPos(Y);
  DoMouseUp;
  FDoChange := True;
  RePaintControl;
  inherited;
end;

procedure TscTrackBarPicker.SetKind(Value: TscColorTrackBarKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    DoInitBMP;
    RePaintControl;
  end;
end;

procedure TscTrackBarPicker.SetPlacement(Value: TscColorSliderPos);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    RePaintControl;
   end;
end;

procedure TscTrackBarPicker.SetSelIndicatorKind(Value: TscSelIndicatorKind);
begin
  if FSelIndicator <> Value then
  begin
    FSelIndicator := Value;
    RePaintControl;
  end;
end;

procedure TscTrackBarPicker.DoMouseMove;
begin

end;

procedure TscTrackBarPicker.DoMouseUp;
begin

end;

procedure TscTrackBarPicker.DoMouseDown;
begin

end;

procedure TscTrackBarPicker.DoResize;
begin

end;

procedure TscTrackBarPicker.DoInitBmp;
begin

end;

function TscTrackBarPicker.GetArrowPos: Integer;
begin
  Result := 0;
end;

constructor TscLColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FLBmp := TBitmap.Create;
  FLBmp.PixelFormat := pf32bit;
  Width := 22;
  Height := 252;
  Kind := scctkVertical;
  FHue := 0;
  FSat := MaxSat;
  FArrowPos := ArrowPosFromLum(MaxLum div 2);
  FChange := False;
  SetLuminance(MaxLum div 2);
  FChange := True;
end;

destructor TscLColorPicker.Destroy;
begin
  FLBmp.Free;
  inherited Destroy;
end;

procedure TscLColorPicker.CreateWnd;
begin
  inherited;
  CreateLGradient;
end;

procedure TscLColorPicker.CreateLGradient;
var
  i,j: Integer;
  row: pRGBQuadArray;
begin
  if FLBmp = nil then
  begin
    FLBmp := TBitmap.Create;
    FLBmp.PixelFormat := pf32bit;
  end;
  if Kind = scctkHorizontal then
  begin
    FLBmp.width := MaxLum;
    FLBmp.height := 12;
    for i := 0 to MaxLum - 1 do
     for j := 0 to 11 do
      begin
        row := FLBmp.Scanline[j];
        row[i] := RGBToRGBQuad(HSLRangeToRGB(FHue, FSat, i));
      end;
   end
  else
  begin
    FLBmp.width := 12;
    FLBmp.height := MaxLum;
    for i := 0 to MaxLum - 1 do
     begin
      row := FLBmp.Scanline[i];
      for j := 0 to 11 do
        row[j] := RGBToRGBQuad(HSLRangeToRGB(FHue, FSat, MaxLum - i));
     end;
   end;
end;

procedure TscLColorPicker.SetHue(h: Integer);
begin
  if h > MaxHue then h := MaxHue;
  if h < 0 then h := 0;
  if FHue <> h then
  begin
    FHue := h;
    CreateLGradient;
    Invalidate;
    if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TscLColorPicker.SetSat(s: Integer);
begin
  if s > MaxSat then s := MaxSat;
  if s < 0 then s := 0;
  if FSat <> s then
  begin
    FSat := s;
    CreateLGradient;
    Invalidate;
    if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TscLColorPicker.ArrowPosFromLum(l: Integer): Integer;
var
  a: Integer;
begin
  if Kind = scctkHorizontal then
  begin
    a := Round(((Width - 12)/MaxLum)*l);
    if a > Width - FLimit then a := Width - FLimit;
  end
  else
  begin
    l := MaxLum - l;
    a := Round(((Height - 12)/MaxLum)*l);
   if a > Height - FLimit then a := Height - FLimit;
  end;
  if a < 0 then a := 0;
  Result := a;
end;

function TscLColorPicker.LumFromArrowPos(p: Integer): Integer;
var
  r: Integer;
begin
 if Kind = scctkHorizontal then
   r := Round(p/((Width - 12)/MaxLum))
 else
   r := Round(MaxLum - p/((Height - 12)/MaxLum));
  if r < 0 then r := 0;
  if r > MaxLum then r := MaxLum;
  Result := r;
end;

procedure TscLColorPicker.SetLuminance(l: Integer);
begin
  if l < 0 then l := 0;
  if l > MaxLum then l := MaxLum;
  if FLuminance <> l then
  begin
    FLuminance := l;
    FArrowPos := ArrowPosFromLum(l);
    Invalidate;
    if FChange then
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TscLColorPicker.GetSelectedColor: TColor;
begin
  Result := HSLRangeToRGB(FHue, FSat, FLuminance);
end;

function TscLColorPicker.GetSelectedValue: Integer;
begin
  Result := FLuminance;
end;

procedure TscLColorPicker.SetSelectedColor(c: TColor);
var
  h1, s1, l1: Integer;
begin
  RGBtoHSLRange(c, h1, s1, l1);
  FChange := False;
  SetHue(h1);
  SetSat(s1);
  SetLuminance(l1);
  FChange := True;
  if FChange then
  if Assigned(OnChange) then OnChange(Self);
end;

function TscLColorPicker.GetArrowPos: Integer;
begin
  Result := ArrowPosFromLum(FLuminance);
end;

procedure TscLColorPicker.PaintTrackBar(ACanvas: TCanvas);
begin
  ACanvas.StretchDraw(FPickRect, FLBmp);
end;

procedure TscLColorPicker.DoMouseMove;
begin
  Fluminance := LumFromArrowPos(FArrowPos);
end;

procedure TscLColorPicker.DoMouseUp;
begin
  Fluminance := LumFromArrowPos(FArrowPos);
end;

procedure TscLColorPicker.DoMouseDown;
begin
  Fluminance := LumFromArrowPos(FArrowPos);
end;

procedure TscLColorPicker.DoResize;
begin
  SetLuminance(FLuminance);
end;

procedure TscLColorPicker.DoInitBmp;
begin
  CreateLGradient;
end;

end.
