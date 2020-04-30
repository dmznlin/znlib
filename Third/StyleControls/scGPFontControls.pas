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

unit scGPFontControls;

{$I scdefine.inc}
{$R-}

interface

uses
  Winapi.Windows, System.Classes, WinApi.Messages,
  WinApi.GdipObj, WinApi.GdipApi,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics, scDrawUtils, scControls,
  scGPControls, scGPUtils, Vcl.ExtCtrls;

{$R scAwesomeFont.res}

type

  TscGPFontAwesome = class
  private
    FPrivateFontCollection : TGPPrivateFontCollection;
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; AColor: TColor;
      AColorAlpha: Byte; ARect: TRect; AFontHeight: Integer; AIndex: Integer); overload;
    procedure Draw(AGraphics: TGPGraphics; AColor: Cardinal; ARect: TGPRectF;
      AFontHeight: Integer; AIndex: Integer); overload;
  end;


  TscGPCharGlyphOptions = class(TPersistent)
  private
    FNormalColor: TColor;
    FHotColor: TColor;
    FPressedColor: TColor;
    FFocusedColor: TColor;
    FDisabledColor: TColor;

    FNormalColorAlpha: Byte;
    FHotColorAlpha: Byte;
    FPressedColorAlpha: Byte;
    FFocusedColorAlpha: Byte;
    FDisabledColorAlpha: Byte;

    FStyleColors: Boolean;
    FState: TscsCtrlState;
    FOnChange: TNotifyEvent;

    FIndex: Integer;
    FSize: Integer;
    FMargin: Integer;

    procedure SetSize(Value: Integer);
    procedure SetMargin(Value: Integer);

    function GetNormalColor: TColor;
    function GetHotColor: TColor;
    function GetPressedColor: TColor;
    function GetFocusedColor: TColor;
    function GetDisabledColor: TColor;

    function GetColor: TColor;
    function GetColorAlpha: Byte;

    procedure SetNormalColor(Value: TColor);
    procedure SetHotColor(Value: TColor);
    procedure SetPressedColor(Value: TColor);
    procedure SetFocusedColor(Value: TColor);
    procedure SetDisabledColor(Value: TColor);

    procedure SetNormalColorAlpha(Value: Byte);
    procedure SetHotColorAlpha(Value: Byte);
    procedure SetPressedColorAlpha(Value: Byte);
    procedure SetFocusedColorAlpha(Value: Byte);
    procedure SetDisabledColorAlpha(Value: Byte);

    procedure SetStyleColors(Value: Boolean);
    procedure SetIndex(Value: Integer);

    procedure Changed;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property State: TscsCtrlState read FState write FState;

    property Color: TColor read GetColor;
    property ColorAlpha: Byte read GetColorAlpha;
  published
    property NormalColor: TColor read GetNormalColor write SetNormalColor;
    property HotColor: TColor read GetHotColor write SetHotColor;
    property PressedColor: TColor read GetPressedColor write SetPressedColor;
    property FocusedColor: TColor read GetFocusedColor write SetFocusedColor;
    property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;

    property NormalColorAlpha: Byte read FNormalColorAlpha write SetNormalColorAlpha;
    property HotColorAlpha: Byte read FHotColorAlpha write SetHotColorAlpha;
    property PressedColorAlpha: Byte read FPressedColorAlpha write SetPressedColorAlpha;
    property FocusedColorAlpha: Byte read FFocusedColorAlpha write SetFocusedColorAlpha;
    property DisabledColorAlpha: Byte read FDisabledColorAlpha write SetDisabledColorAlpha;

    property Index: Integer
      read FIndex write SetIndex;

    property Margin: Integer read FMargin write SetMargin;
    property Size: Integer read FSize write SetSize;
    property StyleColors: Boolean read FStyleColors write SetStyleColors;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscGPCharGlyphButton = class(TscCustomButtonControl)
  private
    FBadge: TscGPButtonBadge;
    FOptions: TscGPButtonOptions;
    FGlyphOptions: TscGPCharGlyphOptions;
    FScaleFrameWidth: Boolean;
    FWidthWithCaption: Integer;
    FWidthWithoutCaption: Integer;
    FShowCaption: Boolean;
    FActive: Boolean;
    FDefault: Boolean;
    FCancel: Boolean;
    FModalResult: TModalResult;
    FModalSetting: Boolean;
    FTextMargin: Integer;

    FMouseX: Integer;
    FMouseY: Integer;
    FMouseLDown: Boolean;
    FFluentLightEffect: Boolean;
    FFluentLightPressedEffectAmount: Byte;

    procedure SetShowCaption(Value: Boolean);
    procedure SetTextMargin(Value: Integer);
    procedure OnOptionsChange(Sender: TObject);
  protected
    FMinMargins: Boolean;
    procedure DoDialogChar; override;
    function GetCtrlState: TscsCtrlState; override;
    procedure ButtonClick; override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function CanAnimateFocusedState: Boolean; override;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CheckGroupIndex; override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure DrawButton(ACanvas: TCanvas; ACtrlState: TscsCtrlState; ADrawContent: Boolean; ADrawDivider: Boolean);
    procedure Loaded; override;
    procedure DrawBadge(ACanvas: TCanvas);

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
       AHeight: Integer); override;
  published
    property Action;
    property ArrowDirection;
    property ArrowPosition;
    property Animation;
    property Badge: TscGPButtonBadge
        read FBadge write FBadge;
    property Caption;
    property CanFocused;
    property CustomDropDown;
    property DrawTextMode;
    property FluentLightEffect: Boolean read FFluentLightEffect write FFluentLightEffect;
    property Layout;
    property TransparentBackground;
    property Default: Boolean
      read FDefault write FDefault default False;
    property Cancel: Boolean
      read FCancel write FCancel default False;
    property Options: TscGPButtonOptions
      read FOptions write FOptions;
    property GlyphOptions: TscGPCharGlyphOptions
      read FGlyphOptions write FGlyphOptions;
    property ModalResult: TModalResult read
      FModalResult write FModalResult default mrNone;
    property ModalSetting: Boolean read
      FModalSetting write FModalSetting default False;
    property TextMargin: Integer read
      FTextMargin write SetTextMargin;
    property ScaleFrameWidth: Boolean
      read FScaleFrameWidth write FScaleFrameWidth default True;
    property WidthWithCaption: Integer read FWidthWithCaption write FWidthWithCaption;
    property WidthWithoutCaption: Integer read FWidthWithoutCaption write FWidthWithoutCaption;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default False;
    property SplitButton;
    property RepeatClick;
    property RepeatClickInterval;
    property DropDownMenu;
    property GalleryMenu;
    property ShowGalleryMenuFromTop;
    property ShowGalleryMenuFromRight;
    property ShowMenuArrow;
    property ShowFocusRect;
    property Down;
    property GroupIndex;
    property AllowAllUp;
    property WordWrap;
    property OnDropDown;
    property OnCloseUp;
    property OnClick;
  end;


  TscGPCharImage = class(TscPanel)
  private
    FTransparent: Boolean;
    FFrame: TscGPImageClipFrame;
    FFrameRadius: Integer;
    FFrameFillColor: TColor;
    FFrameFillColorAlpha: Byte;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FRotationAngle: Integer;
    FAnimationTimer: TTimer;
    FRotateAnimation: Boolean;
    FImageIndex: Integer;
    FImageSize: Integer;
    FAnimationAcceleration: Boolean;
    FImageColor: TColor;
    FImageColorAlpha: Byte;

    procedure SetImageColor(Value: TColor);
    procedure SetImageColorAlpha(Value: Byte);

    procedure SetImageSize(Value: Integer);
    procedure SetImageIndex(Value: Integer);
    procedure SetRotateAnimation(Value: Boolean);
    procedure SetRotationAngle(Value: Integer);
    procedure SetFrame(Value: TscGPImageClipFrame);
    procedure SetFrameRadius(Value: Integer);
    procedure SetFrameFillColor(Value: TColor);
    procedure SetFrameFillColorAlpha(Value: Byte);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameWidth(Value: Integer);

    procedure SetTransparent(Value: Boolean);
  protected

    procedure StartAnimation;
    procedure StopAnimation;
    procedure OnAnimationTimer(Sender: TObject);

    procedure DrawAdditionalContent(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; var ADrawCaption: Boolean); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property DrawTextMode;
    property ImageColor: TColor
      read FImageColor write SetImageColor;
    property ImageColorAlpha: Byte
      read FImageColorAlpha write SetImageColorAlpha;
    property ImageSize: Integer
      read FImageSize write SetImageSize;
    property ImageIndex: Integer
      read FImageIndex write SetImageIndex;

    property Frame: TscGPImageClipFrame
      read FFrame write SetFrame;
    property FrameRadius: Integer
      read FFrameRadius write SetFrameRadius;
    property FrameFillColor: TColor
      read FFrameFillColor write SetFrameFillColor;
    property FrameFillColorAlpha: Byte
      read FFrameFillColorAlpha write SetFrameFillColorAlpha;
    property FrameColor: TColor read
      FFrameColor write SetFrameColor;
    property FrameWidth: Integer
      read FFrameWidth write SetFrameWidth;

    property RotationAngle: Integer
      read FRotationAngle write SetRotationAngle;
    property AnimationAcceleration: Boolean
      read FAnimationAcceleration write FAnimationAcceleration;

    property RotateAnimation: Boolean
      read FRotateAnimation write SetRotateAnimation;

    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Touch;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

var
  SC_FontAwesome:  TscGPFontAwesome;

implementation

uses
  System.Math, Vcl.Forms, Vcl.Buttons;

constructor TscGPFontAwesome.Create;
begin
  inherited;
  FPrivateFontCollection := nil;
  Init;
end;

destructor TscGPFontAwesome.Destroy;
begin
  if FPrivateFontCollection <> nil then
   FPrivateFontCollection.Free;
  inherited;
end;

procedure TscGPFontAwesome.Init;
var
  RStream: TResourceStream;
begin
  RStream := TResourceStream.Create(HInstance, 'fontawesome', RT_RCDATA);
  try
    FPrivateFontCollection := TGPPrivateFontCollection.Create;
    FPrivateFontCollection.AddMemoryFont(RStream.Memory, RStream.Size);
  finally
    RStream.Free;
  end;
end;

procedure TscGPFontAwesome.Draw(ACanvas: TCanvas; AColor: TColor; AColorAlpha: Byte; ARect: TRect; AFontHeight: Integer; AIndex: Integer);
var
  LGraphics: TGPGraphics;
  R: TGPRectF;
  LColor: Cardinal;
begin
  LGraphics := TGPGraphics.Create(ACanvas.Handle);
  LGraphics.SetSmoothingMode(SmoothingModeHighQuality);
  LGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
  try
    LColor := ColorToGPColor(AColor, AColorAlpha);
    R := RectToGPRect(ARect);
    Draw(LGraphics, LColor, R, AFontHeight, AIndex);
  finally
    LGraphics.Free;
  end;
end;

procedure TscGPFontAwesome.Draw(AGraphics: TGPGraphics; AColor: Cardinal; ARect: TGPRectF; AFontHeight: Integer; AIndex: Integer);
var
  LFont: TGPFont;
  LBrush: TGPSolidBrush;
  LGPStringFormat: TGPStringFormat;
  LChar: Char;
begin
  if AFontHeight = 0 then
    AFontHeight := Trunc(Min(Trunc(ARect.Width), Trunc(ARect.Height)) * 0.9);

  AGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
  LFont := TGPFont.Create('FontAwesome', AFontHeight, FontStyleRegular, UnitPixel, FPrivateFontCollection);
  try
    LBrush := TGPSolidBrush.Create(AColor);
    try
      LGPStringFormat := TGPStringFormat.Create;
      try
        LGPStringFormat.SetFormatFlags(StringFormatFlagsNoClip);
        LGPStringFormat.SetAlignment(StringAlignment.StringAlignmentCenter);
        ARect.Y := ARect.Y + (ARect.Height - AFontHeight) / 2;
        ARect.Height := AFontHeight;
        LChar := Chr(61440 + AIndex);
        AGraphics.DrawString(LChar, -1, LFont, ARect, LGPStringFormat, LBrush);
      finally
        LGPStringFormat.Free;
      end;
    finally
      LBrush.Free;
    end;
  finally
    LFont.Free;
  end;
end;

constructor TscGPCharGlyphOptions.Create;
begin
  inherited;
  FSize := 0;
  FIndex := 2;
  FNormalColor := clBtnText;
  FHotColor := clBtnText;
  FPressedColor := clBtnText;
  FFocusedColor := clBtnText;
  FDisabledColor := clBtnText;
  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;
  FNormalColorAlpha := 200;
  FHotColorAlpha := 255;
  FPressedColorAlpha := 255;
  FFocusedColorAlpha := 255;
  FDisabledColorAlpha := 100;
end;

procedure TscGPCharGlyphOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPCharGlyphOptions then
  begin
    FNormalColor := TscGPCharGlyphOptions(Source).FNormalColor;
    FHotColor := TscGPCharGlyphOptions(Source).FHotColor;
    FPressedColor := TscGPCharGlyphOptions(Source).FPressedColor;
    FFocusedColor := TscGPCharGlyphOptions(Source).FFocusedColor;
    FDisabledColor := TscGPCharGlyphOptions(Source).FDisabledColor;
    FNormalColorAlpha := TscGPCharGlyphOptions(Source).FNormalColorAlpha;
    FHotColorAlpha := TscGPCharGlyphOptions(Source).FHotColorAlpha;
    FPressedColorAlpha := TscGPCharGlyphOptions(Source).FPressedColorAlpha;
    FFocusedColorAlpha := TscGPCharGlyphOptions(Source).FFocusedColorAlpha;
    FDisabledColorAlpha := TscGPCharGlyphOptions(Source).FDisabledColorAlpha;
    FStyleColors := TscGPCharGlyphOptions(Source).FStyleColors;
    FIndex := TscGPCharGlyphOptions(Source).Index;
  end
  else
    inherited Assign(Source);
end;

 function TscGPCharGlyphOptions.GetColorAlpha: Byte;
 begin
   Result := FNormalColorAlpha;
   case FState of
     scsHot: Result := FHotColorAlpha;
     scsPressed: Result := FPressedColorAlpha;
     scsFocused: Result := FFocusedColorAlpha;
     scsDisabled: Result := FDisabledColorAlpha;
   end;
 end;

function TscGPCharGlyphOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := PressedColor;
    scsFocused: Result := FocusedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscGPCharGlyphOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscGPCharGlyphOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscGPCharGlyphOptions.GetPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscGPCharGlyphOptions.GetFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscGPCharGlyphOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

procedure TscGPCharGlyphOptions.SetSize(Value: Integer);
begin
  if (Value >= 0) and (FSize <> Value) then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetMargin(Value: Integer);
begin
  if (Value >= 0) and (FMargin <> Value) then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure  TscGPCharGlyphOptions.SetIndex(Value: Integer);
begin
  if FIndex <> Value then
  begin
    FIndex := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetHotColorAlpha(Value: Byte);
begin
  if FHotColorAlpha <> Value then
  begin
    FHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetPressedColorAlpha(Value: Byte);
begin
  if FPressedColorAlpha <> Value then
  begin
    FPressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetFocusedColorAlpha(Value: Byte);
begin
  if FFocusedColorAlpha <> Value then
  begin
    FFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGPCharGlyphOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

type
  TscGPButtonOptionsClass = class(TscGPButtonOptions);

constructor TscGPCharGlyphButton.Create(AOwner: TComponent);
begin
  inherited;
  FFluentLightEffect := False;
  FMargin := 0;
  FMouseX := 0;
  FMouseY := 0;
  FMouseLDown := False;
  FFluentLightPressedEffectAmount := 0;

  FBadge := TscGPButtonBadge.Create;
  FBadge.OnChange := OnOptionsChange;

  FOptions := TscGPButtonOptions.Create;
  FOptions.OnChange := OnOptionsChange;
  FGlyphOptions := TscGPCharGlyphOptions.Create;
  FGlyphOptions.OnChange := OnOptionsChange;

  TscGPButtonOptionsClass(FOptions).FFrameWidth := 2;
  TscGPButtonOptionsClass(FOptions).FNormalColor := clBtnText;
  TscGPButtonOptionsClass(FOptions).FHotColor := clBtnText;
  TscGPButtonOptionsClass(FOptions).FPressedColor := clBtnText;
  TscGPButtonOptionsClass(FOptions).FDisabledColor := clBtnText;
  TscGPButtonOptionsClass(FOptions).FNormalColorAlpha := 10;
  TscGPButtonOptionsClass(FOptions).FHotColorAlpha := 20;
  TscGPButtonOptionsClass(FOptions).FPressedColorAlpha := 30;
  TscGPButtonOptionsClass(FOptions).FDisabledColorAlpha := 5;

  TscGPButtonOptionsClass(FOptions).FFrameNormalColor := clBtnText;
  TscGPButtonOptionsClass(FOptions).FFrameHotColor := clBtnText;
  TscGPButtonOptionsClass(FOptions).FFramePressedColor := clBtnText;
  TscGPButtonOptionsClass(FOptions).FFrameDisabledColor := clBtnText;

  TscGPButtonOptionsClass(FOptions).FFrameNormalColorAlpha := 70;
  TscGPButtonOptionsClass(FOptions).FFrameHotColorAlpha := 100;
  TscGPButtonOptionsClass(FOptions).FFramePressedColorAlpha := 150;
  TscGPButtonOptionsClass(FOptions).FFrameDisabledColorAlpha := 30;

  FMinMargins := False;
  FShowCaption := False;
  FScaleFrameWidth := True;
  FActive := False;
  FCancel := False;
  FDefault := False;

  FModalResult := mrNone;
  FModalSetting := False;
  FWidthWithCaption := 0;
  FWidthWithoutCaption := 0;
  FGlowBuffer := nil;
  FGlowImageBuffer := nil;

  FTextMargin := -1;

  Width := 50;
  Height := 50;
end;

destructor TscGPCharGlyphButton.Destroy;
begin
  FOptions.Free;
  FGlyphOptions.Free;
  FBadge.Free;
  inherited;
end;

procedure TscGPCharGlyphButton.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if FFluentLightEffect then
    if Message.TimerID = 51 then
    begin
      if FFluentLightPressedEffectAmount <= SC_FluentPressedEffectSteps then
      begin
        RePaintControl;
        Inc(FFluentLightPressedEffectAmount);
      end
      else
      begin
        FMouseLDown := False;
        KillTimer(Handle, 51);
        RePaintControl;
      end;
  end;
end;

procedure TscGPCharGlyphButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FFluentLightEffect and (Button = mbLeft) and not FDropDownCall then
  begin
    FFluentLightPressedEffectAmount := 0;
    FMouseLDown := True;
    FMouseY := Y;
    RePaintControl;
    SetTimer(Handle, 51, 20, nil);
  end;
end;

procedure TscGPCharGlyphButton.DoMouseLeave;
begin
  inherited;
  if FMouseLDown then
  begin
    KillTimer(Handle, 51);
    FMouseLDown := False;
    RePaintControl;
  end;
end;

procedure TscGPCharGlyphButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FFluentLightEffect then
  begin
    FMouseX := X;
    if not (FMouseLDown and (FFluentLightPressedEffectAmount > SC_FluentPressedEffectSteps)) then
      RePaintControl;
  end;
end;

procedure TscGPCharGlyphButton.DrawBadge(ACanvas: TCanvas);
var
  FColor: TColor;
  FillR: TGPRectF;
  BW, BH: Integer;
  FillPath: TGPGraphicsPath;
  l, t, w, h, d: Single;
  FillColor: Cardinal;
  B: TGPSolidBrush;
  G: TGPGraphics;
  TX, TY: Integer;
begin
  ACanvas.Font := Badge.Font;
  if FOptions.StyleColors then
  begin
    FColor := GetStyleColor(Badge.Color);
    ACanvas.Font.Color := GetStyleColor(Badge.Font.Color);
  end
  else
    FColor := Badge.Color;
  BW := ACanvas.TextWidth(FBadge.Text);
  BH := ACanvas.TextHeight('Qq');
  BW := BW + BH div 2;
  BH := BH + BH div 2;
  if BW < BH then
    BW := BH;

  if BidiMode <> bdRightToLeft then
    Badge.ControlRect := Rect(Width - BW, 0, Width, BH)
  else
    Badge.ControlRect := Rect(0, 0, BW, BH);

  ACanvas.Brush.Color := FColor;
  FillColor := ColorToGPColor(FColor, Badge.ColorAlpha);
  G := TGPGraphics.Create(ACanvas.Handle);
  FillPath := TGPGraphicsPath.Create;
  B := TGPSolidBrush.Create(FillColor);
  try
    G.SetSmoothingMode(SmoothingModeHighQuality);
    G.SetPixelOffsetMode(PixelOffsetModeHalf);
    FillR := RectToGPRect(Badge.ControlRect);
    l := FillR.X;
    t := FillR.y;
    w := FillR.Width;
    h := FillR.Height;
    d := FillR.Height;
    FillPath.StartFigure;
    FillPath.AddArc(l, t, d, d, 180, 90);
    FillPath.AddArc(l + w - d, t, d, d, 270, 90);
    FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
    FillPath.AddArc(l, t + h - d, d, d, 90, 90);
    FillPath.CloseFigure;
    G.FillPath(B, FillPath);
  finally
    B.Free;
    FillPath.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;


  if FDrawTextMode = scdtmGDI then
  begin
    ACanvas.Brush.Style := bsClear;
    BW := ACanvas.TextWidth(FBadge.Text);
    BH := ACanvas.TextHeight(FBadge.Text);
    TX := FBadge.ControlRect.Left + (FBadge.ControlRect.Width - BW) div 2;
    TY := FBadge.ControlRect.Top + (FBadge.ControlRect.Height - BH) div 2;
    ACanvas.TextOut(TX, TY, FBadge.Text);
  end
  else
    GPDrawText (G, nil, ACanvas, FBadge.ControlRect, FBadge.Text, DT_CENTER or DT_VCENTER);

  if G <> nil then
    G.Free;
end;

procedure TscGPCharGlyphButton.SetTextMargin(Value: Integer);
begin
  if Value <> FTextMargin then
  begin
    FTextMargin := Value;
    RePaintControl;
  end;
end;

procedure TscGPCharGlyphButton.Loaded;
begin
  inherited;
  if (FOptions.ShapeStyle = scgpRounded) and (Width <> Height) then
  begin
    SetBounds(Left, Top, Width, Height);
  end;
end;

function TscGPCharGlyphButton.GetCtrlState: TscsCtrlState;
begin
  if FDown then
  begin
    if FMouseIn and FOptions.PressedHotColors then
      Result := scsHot
    else
      Result := scsPressed;
  end
  else
  if not Enabled then
    Result := scsDisabled else
  if FMenuDroppedDown then
    Result := scsHot
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn or (FActive and FDefault and CanFocused) then
    Result := scsHot
  else
  if Focused and CanFocused then
    Result := scsFocused
  else
    Result := scsNormal;
end;

procedure TscGPCharGlyphButton.CMFocusChanged(var Message: TCMFocusChanged);
var
  OldActive: Boolean;
begin
  if CanFocused and FDefault then
  begin
    OldActive := FActive;
    with Message do
    if Sender is TscGPCharGlyphButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
    if OldActive <> FActive then
      RePaintControl;
  end;
  inherited;
end;

procedure TscGPCharGlyphButton.CMDialogKey(var Message: TCMDialogKey);
begin
  if not CanFocused then
  begin
    inherited;
    Exit;
  end;
  with Message do
   if FActive and (CharCode = VK_RETURN) and Enabled
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
   if (CharCode = VK_ESCAPE) and FCancel and CanFocused and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
     inherited;
end;

procedure TscGPCharGlyphButton.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscGPCharGlyphButton.ButtonClick;
var
  Form: TCustomForm;
begin
  if FDisableClick then Exit;
  if FModalSetting then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := FModalResult;
  end;
  inherited;
end;

procedure TscGPCharGlyphButton.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  if (FOptions <> nil) and (FOptions.ShapeStyle = scgpRounded) then
    AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscGPCharGlyphButton.SetShowCaption(Value: Boolean);
begin
  if Value <> FShowCaption then
  begin
    FShowCaption := Value;
    if (FWidthWithCaption > 0) and FShowCaption then
      Width := FWidthWithCaption
    else
    if (FWidthWithoutCaption > 0) and not FShowCaption then
      Width := FWidthWithoutCaption
    else
      RePaintControl;
  end;
end;

procedure TscGPCharGlyphButton.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  if FScaleFrameWidth then
    TscGPButtonOptionsClass(FOptions).FFrameWidth := MulDiv(TscGPButtonOptionsClass(FOptions).FFrameWidth, M, D);

  if FGlyphOptions.FSize <> 0 then
   FGlyphOptions.FSize := MulDiv(FGlyphOptions.FSize, M, D);
  if FGlyphOptions.FMargin <> 0 then
   FGlyphOptions.FMargin := MulDiv(FGlyphOptions.FMargin, M, D);

  TscGPButtonOptionsClass(FOptions).FShapeCornerRadius := MulDiv(TscGPButtonOptionsClass(FOptions).FShapeCornerRadius, M, D);
  TscGPButtonOptionsClass(FOptions).FArrowSize := MulDiv(TscGPButtonOptionsClass(FOptions).FArrowSize, M, D);
  TscGPButtonOptionsClass(FOptions).FArrowAreaSize := MulDiv(TscGPButtonOptionsClass(FOptions).FArrowAreaSize, M, D);
  if TscGPButtonOptionsClass(FOptions).ArrowThicknessScaled then
    TscGPButtonOptionsClass(FOptions).FArrowThickness := MulDiv(TscGPButtonOptionsClass(FOptions).FArrowThickness, M, D);

  FBadge.Font.Height := MulDiv(FBadge.Font.Height, M, D);

  if FWidthWithCaption > 0 then
    FWidthWithCaption := MulDiv(FWidthWithCaption, M, D);
  if FWidthWithoutCaption > 0 then
    FWidthWithoutCaption := MulDiv(FWidthWithoutCaption, M, D);

  inherited;
end;

function TscGPCharGlyphButton.CanAnimateFocusedState: Boolean;
begin
  Result := CanFocused;
end;

procedure TscGPCharGlyphButton.CheckGroupIndex;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Parent.ControlCount - 1 do
   if (Parent.Controls[I] is TscGPCharGlyphButton) and (GroupIndex = TscGPCharGlyphButton(Parent.Controls[I]).GroupIndex)
     and TscGPCharGlyphButton(Parent.Controls[I]).Down and (Parent.Controls[I] <> Self)  then
       TscGPCharGlyphButton(Parent.Controls[I]).Down := False;
end;

procedure TscGPCharGlyphButton.DrawButton(ACanvas: TCanvas; ACtrlState: TscsCtrlState; ADrawContent: Boolean; ADrawDivider: Boolean);
var
  R, TR, ArrowR: TRect;
  G: TGPGraphics;
  B: TGPBrush;
  P: TGPPen;
  ArrowBrush: TGPSolidBrush;
  FramePath, FillPath, ArrowPath: TGPGraphicsPath;
  FillR, FrameR, R1, ArrowRGP: TGPRectF;
  FrameColor, FillColor: Cardinal;
  l, t, w, h, d: Single;
  GlyphR: TGPRectF;
  C1, C2, GlyphColor, ArrowColor: Cardinal;
  GS: Integer;
  FInternalLayout: TButtonLayout;
  SaveState: TscsCtrlState;
begin
  R := Rect(0, 0, Width, Height);
  TR := R;
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;

  FInternalLayout := Layout;
  if BidiMode = bdRightToLeft then
    if FInternalLayout = blGlyphLeft then
      FInternalLayout := blGlyphRight
    else
    if FInternalLayout = blGlyphRight then
      FInternalLayout := blGlyphLeft;


  FOptions.State := ACtrlState;

  FGlyphOptions.State := ACtrlState;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  P := TGPPen.Create(0, FOptions.FrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  ArrowPath := nil;
  // colors
  if FMouseIn and FDown and FOptions.PressedHotColors then
  begin
    SaveState := FOptions.State;
    FOptions.State := scsPressed;
    FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
    FOptions.State := SaveState;
  end
  else
    FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
  //
  if ADrawDivider then
    FillColor := ColorToGPColor(Options.FontColor, 50)
  else
    FillColor := ColorToGPColor(Options.Color, Options.ColorAlpha);
  GlyphColor := ColorToGPColor(FGlyphOptions.Color, FGlyphOptions.ColorAlpha);

  ArrowColor := ColorToGPColor(Options.ArrowColor, Options.ArrowColorAlpha);

  P.SetColor(FrameColor);
  // rects
  FillR := RectToGPRect(R);
  FrameR := RectToGPRect(R);
  InflateGPRect(FrameR, -FOptions.FrameWidth / 2, -FOptions.FrameWidth / 2);
  if FrameColor <> 0 then
  begin
    if FOptions.FrameColorAlpha = 255 then
      FillR := FrameR
    else
      InflateGPRect(FillR, -FOptions.FrameWidth, - FOptions.FrameWidth);
  end;
  if ADrawDivider then
    FrameColor := 0;
  if (FOptions.ShapeFillStyle = scgpsfColor) or ADrawDivider then
    B := TGPSolidBrush.Create(FillColor)
  else
  begin
    if FOptions.Color2 <> clNone then
    begin
      C1 := ColorToGPColor(FOptions.Color, Options.ColorAlpha);
      C2 := ColorToGPColor(FOptions.Color2, Options.Color2Alpha);
    end
    else
    begin
      C1 := ColorToGPColor(LighterColor(FOptions.Color, FOptions.ShapeFillGradientColorOffset), Options.ColorAlpha);
      C2 := ColorToGPColor(DarkerColor(FOptions.Color, FOptions.ShapeFillGradientColorOffset), Options.ColorAlpha);
    end;
    R1 := FillR;
    if (FOptions.ShapeStyle = scgpTabTop) or (FOptions.ShapeStyle = scgpTabBottom) then
      InflateGPRect(R1, FOptions.FrameWidth, FOptions.FrameWidth)
    else
      InflateGPRect(R1, 1, 1);
    if ACtrlState = scsPressed then
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.ShapeFillGradientPressedAngle)
    else
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.ShapeFillGradientAngle);
  end;
  // draw
  try
   case FOptions.ShapeStyle of
      scgpLeftLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, P.GetWidth, FrameR.X, Height - P.GetWidth);
        end;
       scgpLeftLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
         if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, 0, FrameR.X, Height);
        end;
        scgpLeftRightLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X, 0, FrameR.X, Height);
          G.DrawLine(P, FrameR.X + FrameR.Width, 0, FrameR.X + FrameR.Width, Height);
        end;
      scgpRightLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X + FrameR.Width, P.GetWidth, FrameR.X + FrameR.Width, Height - P.GetWidth);
        end;
      scgpRightLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, FrameR.X + FrameR.Width, 0, FrameR.X + FrameR.Width, Height);
        end;
      scgpTopLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, P.GetWidth, FrameR.Y, Width - P.GetWidth, FrameR.Y);
        end;
      scgpTopLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y, Width, FrameR.Y);
        end;
       scgpTopBottomLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y, Width, FrameR.Y);
          G.DrawLine(P, 0, FrameR.Y + FrameR.Height, Width, FrameR.Y + FrameR.Height);
        end;
      scgpBottomLineMargins:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, P.GetWidth, FrameR.Y + FrameR.Height, Width - P.GetWidth, FrameR.Y + FrameR.Height);
        end;
      scgpBottomLine:
        begin
          FillR := RectToGPRect(R);
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawLine(P, 0, FrameR.Y + FrameR.Height, Width, FrameR.Y + FrameR.Height);
        end;
      scgpRect:
        begin
          G.FillRectangle(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
          end;
          G.DrawRectangle(P, FrameR);
        end;
      scgpRounded, scgpEllipse:
        begin
          G.FillEllipse(B, FillR);
          if FFluentLightEffect and not FMenuTracking then
          begin
            FillPath.AddEllipse(FillR);
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          G.DrawEllipse(P, FrameR);
        end;
      scgpRoundedRect, scgpRoundedLeftRight:
        begin
          // fill
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if Options.ShapeStyle = scgpRoundedLeftRight
          then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
           if Options.ShapeStyle = scgpRoundedLeftRight
          then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedMiddle:
      begin
        if FOptions.FrameColorAlpha = 255 then
        begin
          FillR.X := 0;
          FillR.Width := R.Width;
        end
        else
        begin
          FillR.X := FOptions.FrameWidth / 2;
          FillR.Width := R.Width - FOptions.FrameWidth;
        end;
        FrameR.X := FrameR.X - FOptions.FrameWidth / 2;
        FrameR.Width := FrameR.Width + FOptions.FrameWidth;
        G.FillRectangle(B, FillR);
        if FFluentLightEffect and not FMenuTracking then
        begin
          if FMouseLDown then
            DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
          if FMouseIn then
            DrawFluentLightHotEffect(G, FillR, FMouseX);
        end;
        G.DrawRectangle(P, FrameR);
      end;
      scgpTabTop:
        begin
           // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            FillR.Height := FillR.Height + FOptions.FrameWidth * 2
          else
            FillR.Height := FillR.Height + FOptions.FrameWidth;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height),
           MakePoint(FillR.X, FillR.Y + FillR.Height));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          FrameR.Height := FrameR.Height + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height),
           MakePoint(FrameR.X, FrameR.Y + FrameR.Height));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpTabBottom:
        begin
           // fill
          FillR.Y := - FOptions.FrameWidth;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            FillR.Height := FillR.Height + FOptions.FrameWidth * 1.5
          else
            FillR.Height := FillR.Height + FOptions.FrameWidth;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
          begin
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth;
            if d < 1 then d := 1;
          end
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.AddLine(MakePoint(FillR.X, FillR.Y),
           MakePoint(FillR.X +  FillR.Width, FillR.Y));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          FrameR.Y := - FOptions.FrameWidth;
          FrameR.Height := FrameR.Height + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y),
           MakePoint(FrameR.X +  FrameR.Width, FrameR.Y));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedLeft, scgpTabLeft, scgpSegmentedLeftRounded, scgpTabLeftRounded:
        begin
           // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             (FOptions.ShapeStyle <> scgpTabLeft) and (FOptions.ShapeStyle <> scgpTabLeftRounded)
           then
            FillR.Width := R.Width - FOptions.FrameWidth * 1.5
          else
            FillR.Width := R.Width;
          l := FillR.X;
          t := FillR.y;
          h := FillR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedLeftRounded) or
             (FOptions.ShapeStyle = scgpTabLeftRounded) then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l, t + h - d, d, d, 90, 90);
          FillPath.AddArc(l, t, d, d, 180, 90);
          FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y),
            MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          if (Options.ShapeStyle = scgpSegmentedLeft) or
             (Options.ShapeStyle = scgpSegmentedLeftRounded)
          then
            FrameR.Width := FrameR.Width + FOptions.FrameWidth / 2
          else
            FrameR.Width := FrameR.Width + FOptions.FrameWidth;
          l := FrameR.X;
          t := FrameR.y;
          h := FrameR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedLeftRounded) or
             (FOptions.ShapeStyle = scgpTabLeftRounded) then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l, t + h - d, d, d, 90, 90);
          FramePath.AddArc(l, t, d, d, 180, 90);
          FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y),
            MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height));
            FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
      scgpSegmentedRight, scgpTabRight,scgpSegmentedRightRounded, scgpTabRightRounded:
        begin
          // fill
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             (FOptions.ShapeStyle <> scgpTabRight) and (FOptions.ShapeStyle <> scgpTabRightRounded)
          then
            FillR.X := FOptions.FrameWidth / 2
          else
            FillR.X := 0;
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             ((FOptions.ShapeStyle = scgpSegmentedRight) or (FOptions.ShapeStyle = scgpSegmentedRightRounded))
          then
            w := w + FOptions.FrameWidth / 2
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) and
             ((FOptions.ShapeStyle = scgpTabRight) or (FOptions.ShapeStyle = scgpTabRightRounded))
          then
            w := w + FOptions.FrameWidth;

          h := FillR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedRightRounded) or
             (FOptions.ShapeStyle = scgpTabRightRounded) then
            d := FillR.Height
          else
          if (FrameColor <> 0) and (FOptions.FrameColorAlpha < 255) then
            d := Options.ShapeCornerRadius * 2 - FOptions.FrameWidth
          else
            d := Options.ShapeCornerRadius * 2;
          FillPath.StartFigure;
          FillPath.AddArc(l + w - d, t, d, d, 270, 90);
          FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FillPath.AddLine(MakePoint(FillR.X, FillR.Y + FillR.Height),
           MakePoint(FillR.X, FillR.Y));
          FillPath.CloseFigure;
          G.FillPath(B, FillPath);
          if FFluentLightEffect and not FMenuTracking then
          begin
            G.SetClip(FillPath);
            if FMouseLDown then
              DrawFluentLightPressedEffect(G, FillR, FMouseX, FMouseY, FFluentLightPressedEffectAmount);
            if FMouseIn then
              DrawFluentLightHotEffect(G, FillR, FMouseX);
            G.ResetClip;
          end;
          // frame
          if (Options.ShapeStyle = scgpSegmentedRight) or
             (Options.ShapeStyle = scgpSegmentedRightRounded) then
          begin
            FrameR.X := FrameR.X - FOptions.FrameWidth / 2;
            FrameR.Width := FrameR.Width + FOptions.FrameWidth / 2;
          end
          else
          begin
            FrameR.X := FrameR.X - FOptions.FrameWidth;
            FrameR.Width := FrameR.Width + FOptions.FrameWidth;
          end;
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          if (FOptions.ShapeStyle = scgpSegmentedRightRounded) or
             (FOptions.ShapeStyle = scgpTabRightRounded) then
            d := FrameR.Height
          else
            d := Options.ShapeCornerRadius * 2;
          FramePath.StartFigure;
          FramePath.AddArc(l + w - d, t, d, d, 270, 90);
          FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
          FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y + FrameR.Height),
           MakePoint(FrameR.X, FrameR.Y));
          FramePath.CloseFigure;
          G.DrawPath(P, FramePath);
        end;
    end;

    // draw arrow

    if ShowMenuArrow and not ADrawDivider and ((DropDownMenu <> nil) or (GalleryMenu <> nil) or CustomDropDown) then
    begin
     if FArrowPosition = scapRight then
      begin
        if Options.ArrowAreaSize = 0 then
        begin
          if BidiMode <> bdRightToLeft then
          begin
            Dec(R.Right, Options.ArrowSize * 2);
            ArrowR := Rect(R.Right, Height div 2 - FOptions.ArrowSize div 2 - 1,
              R.Right + Options.ArrowSize, Height div 2 - FOptions.ArrowSize div 2 + FOptions.ArrowSize - 1);
          end
          else
          begin
            Inc(R.Left, Options.ArrowSize * 2);
            ArrowR := Rect(R.Left - Options.ArrowSize, Height div 2 - FOptions.ArrowSize div 2 - 1,
              R.Left, Height div 2 - FOptions.ArrowSize div 2 + FOptions.ArrowSize - 1);
          end;
        end
        else
        begin
          if BidiMode <> bdRightToLeft then
          begin
            Dec(R.Right, Options.ArrowAreaSize);
            ArrowR := Rect(R.Right + (Options.ArrowAreaSize - Options.ArrowSize) div 2, Height div 2 - FOptions.ArrowSize div 2 - 1,
              R.Right + (Options.ArrowAreaSize - Options.ArrowSize) div 2 + Options.ArrowSize,
                Height div 2 - FOptions.ArrowSize div 2 + FOptions.ArrowSize - 1);
          end
          else
          begin
            Inc(R.Left, Options.ArrowAreaSize);
            ArrowR := Rect(R.Left - (Options.ArrowAreaSize + Options.ArrowSize) div 2, Height div 2 - FOptions.ArrowSize div 2 - 1,
              R.Left - (Options.ArrowAreaSize + Options.ArrowSize) div 2 + Options.ArrowSize,
                Height div 2 - FOptions.ArrowSize div 2 + FOptions.ArrowSize - 1);
          end;
        end;
        if FOptions.ShapeStyle = scgpTabBottom then
          OffsetRect(ArrowR, 0, -FOptions.FrameWidth);
      end
      else
      begin
        if Options.ArrowAreaSize = 0 then
        begin
          Dec(R.Bottom, Options.ArrowSize * 2);
          ArrowR := Rect(Width div 2 - FOptions.ArrowSize div 2,
            R.Bottom,
            Width div 2 - FOptions.ArrowSize div 2 + FOptions.ArrowSize,
            R.Bottom + FOptions.ArrowSize);
         if FOptions.ShapeStyle = scgpRounded then
          Inc(R.Bottom, Options.ArrowSize * 2);
        end
        else
        begin
          Dec(R.Bottom, Options.ArrowAreaSize);
          ArrowR := Rect(Width div 2 - FOptions.ArrowSize div 2,
            R.Bottom + Options.ArrowAreaSize div 2 - Round(FOptions.ArrowSize / 1.4),
            Width div 2 - FOptions.ArrowSize div 2 + FOptions.ArrowSize,
              R.Bottom + Options.ArrowAreaSize div 2 - Round(FOptions.ArrowSize / 1.4) + FOptions.ArrowSize);
          if FOptions.ShapeStyle = scgpRounded then
            Inc(R.Bottom, Options.ArrowAreaSize div 2);
        end;
      end;

      if FOptions.ArrowType = scgpatModern then
      begin
        if FArrowDirection = scadDefault then
          OffsetRect(ArrowR, 0, -1)
        else
          OffsetRect(ArrowR, 0, 1);
      end;

      if FOptions.ArrowType = scgpatDefault then
      begin
        ArrowRGP := RectToGPRect(ArrowR);
        ArrowPath := TGPGraphicsPath.Create;
        ArrowPath.StartFigure;
        if FArrowDirection = scadDefault then
        begin
          ArrowPath.AddLine(ArrowRGP.X, ArrowRGP.Y + Round(ArrowRGP.Height / 2),
            ArrowRGP.X + ArrowRGP.Width / 2, ArrowRGP.Y + ArrowRGP.Height);
          ArrowPath.AddLine(ArrowRGP.X + ArrowRGP.Width / 2, ArrowRGP.Y + ArrowRGP.Height,
            ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2));
        end
        else
        begin
          ArrowPath.AddLine(ArrowRGP.X + Round(ArrowRGP.Width / 2), ArrowRGP.Y,
            ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2));
          ArrowPath.AddLine(ArrowRGP.X + ArrowRGP.Width, ArrowRGP.Y + Round(ArrowRGP.Height / 2),
            ArrowRGP.X + Round(ArrowRGP.Width / 2), ArrowRGP.Y + ArrowRGP.Height);
        end;
        ArrowPath.CloseFigure;
        ArrowBrush := TGPSolidBrush.Create(ArrowColor);
        try
          G.FillPath(ArrowBrush, ArrowPath);
        finally
          ArrowBrush.Free;
        end;
      end
      else
      begin
        if FArrowDirection = scadDefault then
          GPDrawDropDownButtonGlyph(G, RectToGPRect(ArrowR), ArrowColor, 1, FOptions.ArrowThickness)
        else
          GPDrawRightGlyph(G, RectToGPRect(ArrowR), ArrowColor, 1, FOptions.ArrowThickness);
      end;
    end;

   if SplitButton and ((DropDownMenu <> nil) or CustomDropDown) then
  begin
    if FOptions.ArrowAreaSize = 0 then
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          Dec(R.Right, FOptions.ArrowSize)
        else
          Inc(R.Left, FOptions.ArrowSize);
      end
      else
        Dec(R.Bottom, FOptions.ArrowSize);
  end;

  TR := R;

  // draw button glyph
  if FShowCaption then
  begin
    GS := Min(R.Width, R.Height);
    case FInternalLayout of
      blGlyphLeft:
        R.Right := R.Left + GS;
      blGlyphRight:
        R.Left := R.Right - GS;
      blGlyphTop:
        R.Bottom := R.Top + GS - GS div 3;
      blGlyphBottom:
        R.Top := R.Bottom - GS + GS div 3;
    end;
  end;

  if (R.Width < R.Height) and (FInternalLayout = blGlyphLeft) then
    R.Right := R.Left + R.Height
  else
  if (R.Width < R.Height) and (FInternalLayout = blGlyphRight) then
    R.Left := R.Right - R.Height;


  if R.Width >= R.Height then
  begin
    InflateRect(R, -FOptions.FrameWidth , -FOptions.FrameWidth);
    R.Left := R.Left + R.Width div 2 - R.Height div 2;
    R.Right := R.Left + R.Height;
  end
  else
  begin
    InflateRect(R,  -FOptions.FrameWidth , -FOptions.FrameWidth);
    R.Top := R.Top + R.Height div 2 - R.Width div 2;
    R.Bottom := R.Top + R.Width;
  end;

  if FGlyphOptions.Margin > 0 then
   case FInternalLayout of
      blGlyphLeft:
        OffsetRect(R, FGlyphOptions.Margin, 0);
     blGlyphRight:
        OffsetRect(R, -FGlyphOptions.Margin, 0);
      blGlyphTop:
        OffsetRect(R, 0, FGlyphOptions.Margin);
      blGlyphBottom:
        OffsetRect(R, 0, - FGlyphOptions.Margin);
    end;

  if FShowCaption then
  case FInternalLayout of
    blGlyphLeft:
      TR.Left := R.Right;
   blGlyphRight:
      TR.Right := R.Left;
    blGlyphTop:
      TR.Top := R.Bottom;
    blGlyphBottom:
      TR.Bottom := R.Top;
  end;

  GlyphR := RectToGPRect(R);

  if  ADrawContent  then
    SC_FontAwesome.Draw(G, GlyphColor, GlyphR, FGlyphOptions.Size, FGlyphOptions.Index);
  
  finally
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
    if ArrowPath <> nil then
      ArrowPath.Free;
    if FDrawTextMode = scdtmGDI then
    begin
      G.Free;
      G := nil;
    end;
  end;

  // draw button text
  if FShowCaption and ADrawContent then
  begin
    if FTextMargin >= 0 then
    begin
      case FInternalLayout of
        blGlyphLeft:
          TR.Left := Self.Height;
        blGlyphRight:
          TR.Right := Width - Self.Height;
        blGlyphTop: ;
        blGlyphBottom: ;
      end;
    end;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Assign(Font);
    ACanvas.Font.Color := FOptions.FontColor;
    if FDrawTextMode = scdtmGDIPlus then
      GPDrawImageAndText(G, ACanvas, TR, FTextMargin, 0,
        FInternalLayout, Caption, -1, nil,
        ACtrlState <> scsDisabled, False,
        IsRightToLeft, False, FScaleFactor, WordWrap)
    else
    if (FInternalLayout = blGlyphLeft) or (FInternalLayout = blGlyphRight) then
      DrawImageAndText2(ACanvas, TR, FTextMargin, 0,
        FInternalLayout, Caption, -1, nil,
        ACtrlState <> scsDisabled, False, clBlack, False,
        IsRightToLeft, False, FScaleFactor, WordWrap)
    else
      DrawImageAndText(ACanvas, TR, FTextMargin, 0,
        FInternalLayout, Caption, -1, nil,
        ACtrlState <> scsDisabled, False, clBlack, False,
        IsRightToLeft, False, FScaleFactor, WordWrap);
  end;

  if G <> nil then
    G.Free;
end;

procedure TscGPCharGlyphButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  SaveIndex: Integer;
begin
  if FMenuDroppedDown and SplitButton then
  begin
    if FOptions.ArrowAreaSize = 0 then
      FSplitWidth := Options.ArrowSize * 3 - Options.FrameWidth
    else
      FSplitWidth := FOptions.ArrowAreaSize;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, 0, 0,  Width - FSplitWidth, Height)
        else
          IntersectClipRect(ACanvas.Handle, FSplitWidth, 0,  Width, Height);
      end
     else
       IntersectClipRect(ACanvas.Handle, 0, 0, Width, Height - FSplitWidth);

     DrawButton(ACanvas, scsHot, True, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, 0, Width, Height)
        else
          IntersectClipRect(ACanvas.Handle, 0, 0, FSplitWidth, Height);
      end
     else
        IntersectClipRect(ACanvas.Handle, 0, Height - FSplitWidth, Width, Height);

      DrawButton(ACanvas, scsPressed, False, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
     if FArrowPosition = scapRight then
     begin
       if BidiMode <> bdRightToLeft then
         IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, Options.FrameWidth,
            Width - FSplitWidth + Max(1, FOptions.FrameWidth div 2), Height - Options.FrameWidth)
       else
         IntersectClipRect(ACanvas.Handle, FSplitWidth - Max(1, FOptions.FrameWidth div 2), Options.FrameWidth,
            FSplitWidth, Height - Options.FrameWidth);
     end
      else
        IntersectClipRect(ACanvas.Handle, Options.FrameWidth, Height - FSplitWidth, Width - Options.FrameWidth,
          Height - FSplitWidth + Max(1, FOptions.FrameWidth div 2));

      DrawButton(ACanvas, ACtrlState, False, True);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

  end
  else
  if SplitButton and ((DropDownMenu <> nil) or CustomDropDown) and (ACtrlState = scsHot) then
  begin
    DrawButton(ACanvas, ACtrlState, True, False);

    if FOptions.ArrowAreaSize = 0 then
      FSplitWidth := Options.ArrowSize * 3 - Options.FrameWidth
    else
      FSplitWidth := FOptions.ArrowAreaSize;

    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, Options.FrameWidth,
            Width - FSplitWidth + Max(1, FOptions.FrameWidth div 2), Height - Options.FrameWidth)
        else
          IntersectClipRect(ACanvas.Handle, FSplitWidth - Max(1, FOptions.FrameWidth div 2), Options.FrameWidth,
           FSplitWidth, Height - Options.FrameWidth);
      end
      else
        IntersectClipRect(ACanvas.Handle, Options.FrameWidth, Height - FSplitWidth, Width - Options.FrameWidth,
         Height - FSplitWidth + Max(1, FOptions.FrameWidth div 2));

       DrawButton(ACanvas, ACtrlState, False, True);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

  end
  else
    DrawButton(ACanvas, ACtrlState, True, False);

  if FBadge.Visible and (FBadge.Text <> '') then
    DrawBadge(ACanvas);
end;


procedure TscGPCharGlyphButton.OnOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;


constructor TscGPCharImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csPannable];
  StyleKind := scpsTransparent;
  FCanEmpty := False;
  FImageColor := clBtnText;
  FImageColorAlpha := 255;
  FImageIndex := 2;
  FImageSize := 0;
  FAnimationTimer := nil;
  FRotationAngle := 0;
  FRotateAnimation := False;
  FAnimationAcceleration := False;
  FFrame := scgpcfNone;
  FFrameFillColor := clWindow;
  FFrameFillColorAlpha := 255;
  FFrameColor := clBtnFace;
  FFrameWidth := 2;
  FFrameRadius := 10;
  Height := 105;
  Width := 105;
end;

destructor TscGPCharImage.Destroy;
begin
  if FAnimationTimer <> nil then
    FAnimationTimer.Free;
  inherited Destroy;
end;

procedure TscGPCharImage.Loaded;
begin
  inherited;
end;

procedure TscGPCharImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TscGPCharImage.OnAnimationTimer(Sender: TObject);
var
  Angle: Integer;
  I: Integer;
begin
  if  FAnimationAcceleration then
  begin
    I := Abs(Round(30 * (FRotationAngle / 360)));
    if I < 5 then I := 5;
    if I > 20 then
    begin
      Angle := FRotationAngle + 5;
      if Angle > 355 then
        Angle := 0;
    end
    else
    begin
      Angle := FRotationAngle + 10;
      if Angle > 350 then
        Angle := 0;
    end;

    if FAnimationTimer <> nil  then
      FAnimationTimer.Interval := I;
  end
  else
  begin
     Angle := FRotationAngle + 10;
    if (FAnimationTimer <> nil) and (FAnimationTimer.Interval <> 40) then
      FAnimationTimer.Interval := 40;
    if Angle > 350 then
      Angle := 0;
  end;
  RotationAngle := Angle;
end;


procedure TscGPCharImage.SetImageColor(Value: TColor);
begin
  if (Value <> FImageColor) then
  begin
    FImageColor := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPCharImage.SetImageColorAlpha(Value: Byte);
begin
  if (Value <> FImageColorAlpha) then
  begin
    FImageColorAlpha := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPCharImage.SetImageSize(Value: Integer);
begin
  if (Value <> FImageSize) and (Value >= 0) then
  begin
    FImageSize := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPCharImage.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPCharImage.StartAnimation;
begin
  if FAnimationTimer <> nil then Exit;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := 40;
  FAnimationTimer.Enabled := True;
end;

procedure TscGPCharImage.StopAnimation;
begin
  if FAnimationTimer = nil then Exit;
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Free;
  FAnimationTimer := nil;
end;

procedure TscGPCharImage.SetRotateAnimation(Value: Boolean);
begin
  if FRotateAnimation <> Value then
  begin
    FRotateAnimation := Value;
    if FRotateAnimation then
       StartAnimation
     else
       StopAnimation;
  end;
end;

procedure TscGPCharImage.SetRotationAngle;
begin
  if (Value >= -360) and (Value <= 360) and
    (FRotationAngle <> Value) then
  begin
    FRotationAngle := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPCharImage.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  FFrameWidth := MulDiv(FFrameWidth, M, D);
  FFrameRadius := MulDiv(FFrameRadius, M, D);
  if FImageSize <> 0 then
    FImageSize := MulDiv(FImageSize, M, D);
  inherited;
end;

procedure TscGPCharImage.SetFrame(Value: TscGPImageClipFrame);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPCharImage.SetFrameRadius(Value: Integer);
begin
  if (Value >= 0) and (FFrameRadius <> Value) then
  begin
    FFrameRadius := Value;
    if FFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPCharImage.SetFrameFillColor(Value: TColor);
begin
  if (FFrameFillColor <> Value) then
  begin
    FFrameFillColor := Value;
    if FFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPCharImage.SetFrameFillColorAlpha(Value: Byte);
begin
  if (FFrameFillColorAlpha <> Value) then
  begin
    FFrameFillColorAlpha := Value;
    if FFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPCharImage.SetFrameColor(Value: TColor);
begin
  if (FFrameColor <> Value) then
  begin
    FFrameColor := Value;
    if FFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPCharImage.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) then
  begin
    FFrameWidth := Value;
    if FFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPCharImage.DrawAdditionalContent(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; var ADrawCaption: Boolean);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R: TRect;
  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR: TGPRectF;
  C: Cardinal;
  l, t, w, h, d: Single;
  M: TGPMatrix;
  CenterP: TGPPointF;
  IColor: Cardinal;
  S: String;
  Flags: Longint;
begin
  M := nil;
  IColor := ColorToGPColor(GetStyleColor(FImageColor), FImageColorAlpha);

  if FFrame = scgpcfNone then
  begin
    G := TGPGraphics.Create(ACanvas.Handle);
    G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
    G.SetInterpolationMode(InterpolationModeHighQuality);
    try
      if (FRotationAngle <> 0) then
      begin
        M := TGPMatrix.Create;
        CenterP.X := Self.Width / 2;
        CenterP.Y := Self.Height / 2;
        if FRotateAnimation then
          M.RotateAt(FRotationAngle + 1, CenterP)
        else
          M.RotateAt(FRotationAngle, CenterP);
        G.SetTransform(M);
      end;

      FillR := RectToGPRect(ARect);
      SC_FontAwesome.Draw(G, IColor, FillR, FImageSize, FImageIndex);

    finally
      if M <> nil then
        M.Free;
      G.Free;
    end;
    Exit;
  end;

  R := ARect;
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  G.SetInterpolationMode(InterpolationModeHighQuality);

  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, FFrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  InflateRect(R, -1, -1);
  FillR := RectToGPRect(R);
  InflateRect(R, 1, 1);
  FrameR := RectToGPRect(R);
  if FFrameWidth > 0 then
    InflateGPRect(FrameR, -FFrameWidth / 2, -FFrameWidth / 2);
  try
    // fill frame
    if (FFrameRadius = 0) or (FFrame = scgpcfEllipse) then
    begin
      C := ColorToGPColor(GetStyleColor(FFrameFillColor), FFrameFillColorAlpha);
      B.SetColor(C);
      if FFrame = scgpcfEllipse then
        FillPath.AddEllipse(FillR)
      else
        FillPath.AddRectangle(FillR);
      G.FillPath(B, FillPath);
    end
    else
    begin
      C := ColorToGPColor(GetStyleColor(FFrameFillColor), FFrameFillColorAlpha);
      B.SetColor(C);
      l := FillR.X;
      t := FillR.y;
      w := FillR.Width;
      h := FillR.Height;
      d := FFrameRadius * 2;
      FillPath.StartFigure;
      FillPath.AddArc(l, t, d, d, 180, 90);
      FillPath.AddArc(l + w - d, t, d, d, 270, 90);
      FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
      FillPath.AddArc(l, t + h - d, d, d, 90, 90);
      FillPath.CloseFigure;
      G.FillPath(B, FillPath);
    end;

    try
      if (FRotationAngle <> 0) then
      begin
        M := TGPMatrix.Create;
        CenterP.X := Self.Width / 2;
        CenterP.Y := Self.Height / 2;
        if FRotateAnimation then
          M.RotateAt(FRotationAngle + 1, CenterP)
        else
          M.RotateAt(FRotationAngle, CenterP);
        G.SetTransform(M);
      end;

      // draw char
      if FFrameWidth > 0 then
        InflateGPRect(FillR, -FFrameWidth * 2, -FFrameWidth * 2);

      SC_FontAwesome.Draw(G, IColor, FillR, FImageSize, FImageIndex);

    finally
       if FRotationAngle <> 0 then
       G.ResetTransform;
    end;

    // darw frame
    if (FFrameRadius = 0) or (FFrame = scgpcfEllipse) then
    begin
      if FFrameColor <> clNone then
      begin
        C := ColorToGPColor(GetStyleColor(FFrameColor), 255);
        P.SetColor(C);
        if FFrame = scgpcfEllipse then
          G.DrawEllipse(P, FrameR)
        else
          G.DrawRectangle(P, FrameR);
      end;
    end
    else
    begin
      if (FFrameColor <> clNone) then
      begin
        C := ColorToGPColor(GetStyleColor(FFrameColor), 255);
        P.SetColor(C);
        l := FrameR.X;
        t := FrameR.y;
        w := FrameR.Width;
        h := FrameR.Height;
        d := FFrameRadius * 2;
        FramePath.StartFigure;
        FramePath.AddArc(l, t, d, d, 180, 90);
        FramePath.AddArc(l + w - d, t, d, d, 270, 90);
        FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FramePath.AddArc(l, t + h - d, d, d, 90, 90);
        FramePath.CloseFigure;
        G.DrawPath(P, FramePath);
      end;
    end;

    if (FDrawTextMode = scdtmGDIPlus) and FShowCaption and (GetCaptionText <> '') then
    begin
      ADrawCaption := True;
      S := GetCaptionText;
      InflateRect(R, -3, -3);
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := GetCaptionColor;
      ACanvas.Brush.Style := bsClear;
      Flags := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or Alignments[FAlignment];
      R := Rect(0, 0, Width, Height);
      InflateRect(R, -3, -3);
      GPDrawText(G, nil, ACanvas, R, S, Flags);
    end;

  finally
    if M <> nil then
      M.Free;
    P.Free;
    B.Free;
    G.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPCharImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    RePaintControl;
  end;
end;


initialization

  SC_FontAwesome := TscGPFontAwesome.Create;

finalization

  SC_FontAwesome.Free;

end.