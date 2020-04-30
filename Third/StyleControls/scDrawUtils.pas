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

unit scDrawUtils;

{$R-}
{$I scdefine.inc}

interface
  uses
    Winapi.Windows, Winapi.Messages, System.Classes,
    Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.Themes,
    Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.ImgList,
    Vcl.Imaging.PngImage;

  type
    {$IFDEF VER230}
    TStyleElements = set of (seFont, seClient, seBorder);
    {$ENDIF}

    TscCanChangePageEvent = procedure(ATabIndex: Integer; var ACanChange: Boolean) of object;

    TscsCtrlState = (scsNormal, scsHot, scsPressed, scsFocused, scsDisabled);
    TscsCtrlStates = set of TscsCtrlState;
    TscDropDownPosition = (scdpRight, scdpLeft);

    PscColor = ^TscColor;
    TscColor = type Cardinal;

    PscColorRec = ^TscColorRec;
    TscColorRec = packed record
    case Cardinal of
      0: (Color: Cardinal);
      2: (HiWord, LoWord: Word);
      3: (B, G, R, A: Byte);
    end;

    PscColorArray = ^TscColorArray;
    TscColorArray = array [0..0] of TscColor;

    PscColorRecArray = ^TscColorRecArray;
    TscColorRecArray = array [0..0] of TscColorRec;

    TscAccentPolicy = packed record
      AccentState: Integer;
      AccentFlags: Integer;
      GradientColor: Integer;
      AnimationId: Integer;
    end;

    TscWinCompAttrData = packed record
      attribute: THandle;
      pData: Pointer;
      dataSize: ULONG;
    end;

   SC_BP_ANIMATIONPARAMS = record
     cbSize: DWORD;
     dwFlags: DWORD;
     style: Cardinal;
     dwDuration: DWORD;
   end;

   _SC_BP_ANIMATIONPARAMS = SC_BP_ANIMATIONPARAMS;
   TscBPAnimationParams = _SC_BP_ANIMATIONPARAMS;
   PscBPAnimationParams = ^TscBPAnimationParams;

   SC_BP_PAINTPARAMS = record
     cbSize: DWORD;
     dwFlags: DWORD;
     prcExclude: PRect;
     pBlendFunction: PBLENDFUNCTION;
   end;
   _SC_BP_PAINTPARAMS = SC_BP_PAINTPARAMS;
   TscBPPaintParams = _SC_BP_PAINTPARAMS;
   PscBPPaintParams = ^TscBPPaintParams;

   SC_HANIMATIONBUFFER = THandle;

   SC_MARGINS = packed record
    cxLeftWidth    : Integer;
    cxRightWidth   : Integer;
    cyTopHeight    : Integer;
    cyBottomHeight : Integer;
  end;

  PscMargins = ^SC_MARGINS;
  TscMargins = SC_MARGINS;

   TscCustomImageList = class(TCustomImageList);

   TscGlowEffect = class(TPersistent)
   private
     FEnabled: Boolean;
     FColor: TColor;
     FAlphaValue: Byte;
     FGlowSize: Byte;
     FIntensive: Boolean;
     FOffset: Byte;
     FStyleColors: Boolean;
     FOnChange: TNotifyEvent;
     procedure SetEnabled(Value: Boolean);
     procedure SetColor(Value: TColor);
     procedure SetAlphaValue(Value: Byte);
     procedure SetGlowSize(Value: Byte);
     procedure SetOffset(Value: Byte);
     procedure SetStyleColors(Value: Boolean);
     procedure SetIntensive(Value: Boolean);
     procedure Changed;
     function GetColor: TColor;
     function GetAlphaValue: Byte;
   public
     constructor Create; virtual;
     procedure Assign(Source: TPersistent); override;
   published
     property Enabled: Boolean read FEnabled write SetEnabled;
     property Color: TColor read GetColor write SetColor;
     property AlphaValue: Byte read GetAlphaValue write SetAlphaValue;
     property GlowSize: Byte read FGlowSize write SetGlowSize;
     property Offset: Byte read FOffset write SetOffset;
     property Intensive: Boolean read FIntensive write SetIntensive;
     property StyleColors: Boolean read FStyleColors write SetStyleColors;
     property OnChange: TNotifyEvent read FOnChange write FOnChange;
   end;

   TscButtonGlowEffect = class(TscGlowEffect)
   private
     FHotColor,
     FPressedColor,
     FFocusedColor: TColor;
     FPressedAlphaValue: Byte;
     FPressedGlowSize: Byte;
     FStates: TscsCtrlStates;
     function GetHotColor: TColor;
     function GetPressedColor: TColor;
     function GetFocusedColor: TColor;
     procedure SetHotColor(Value: TColor);
     procedure SetPressedColor(Value: TColor);
     procedure SetFocusedColor(Value: TColor);
     procedure SetPressedAlphaValue(Value: Byte);
     procedure SetPressedGlowSize(Value: Byte);
     procedure SetStates(Value: TscsCtrlStates);
     function GetPressedAlphaValue: Byte;
   public
     constructor Create; override;
     procedure Assign(Source: TPersistent); override;
   published
     property HotColor: TColor read GetHotColor write SetHotColor;
     property PressedColor: TColor read GetPressedColor write SetPressedColor;
     property FocusedColor: TColor read GetFocusedColor write SetFocusedColor;
     property PressedGlowSize: Byte read FPressedGlowSize write SetPressedGlowSize;
     property PressedAlphaValue: Byte read GetPressedAlphaValue write SetPressedAlphaValue;
     property States: TscsCtrlStates read FStates write SetStates;
   end;

  TscBitmapOptions = class(TPersistent)
  private
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FTopMargin: Integer;
    FBottomMargin: Integer;
    FAlphaBlend: Boolean;
    FAlphaBlendBorder: Boolean;
    FAlphaBlendCorners: Boolean;
    FStretch: Boolean;
    FStretchBorder: Boolean;
    FDrawOnlyBorder: Boolean;
    FDrawInClipRect: Boolean;
    FOnChange: TNotifyEvent;
    FContentLeftMargin: Integer;
    FContentRightMargin: Integer;
    FContentTopMargin: Integer;
    FContentBottomMargin: Integer;
    procedure SetLeftMargin(Value: Integer);
    procedure SetRightMargin(Value: Integer);
    procedure SetTopMargin(Value: Integer);
    procedure SetBottomMargin(Value: Integer);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin;
    property TopMargin: Integer read FTopMargin write SetTopMargin;
    property RightMargin: Integer read FRightMargin write SetRightMargin;
    property BottomMargin: Integer read FBottomMargin write SetBottomMargin;
    property AlphaBlend: Boolean read FAlphaBlend write FAlphaBlend;
    property AlphaBlendBorder: Boolean read FAlphaBlendBorder write FAlphaBlendBorder;
    property AlphaBlendCorners: Boolean read FAlphaBlendCorners write FAlphaBlendCorners;
    property Stretch: Boolean read FStretch write FStretch;
    property StretchBorder: Boolean read FStretchBorder write FStretchBorder;
    property DrawOnlyBorder: Boolean read FDrawOnlyBorder write FDrawOnlyBorder;
    property DrawInClipRect: Boolean read FDrawInClipRect write FDrawInClipRect;
    property ContentLeftMargin: Integer
      read FContentLeftMargin write FContentLeftMargin;
    property ContentRightMargin: Integer
      read FContentRightMargin write FContentRightMargin;
    property ContentTopMargin: Integer
      read FContentTopMargin write FContentTopMargin;
    property ContentBottomMargin: Integer
      read FContentBottomMargin write FContentBottomMargin;
  end;

  // system
  procedure GetWindowsVersion(var Major, Minor: Integer);
  function IsWindowsXP: Boolean;
  function IsWindows7: Boolean;
  function IsWindows8: Boolean;
  function IsWindows10: Boolean;
  // style
  function IsWindowsModernStyle: Boolean;
  function IsDarkStyle: Boolean;
  function IsDarkWindowStyle: Boolean;
  function IsCustomStyle: Boolean;
  // draw controls
  procedure DrawBorder(ACanvas: TCanvas; ARect: TRect);
  procedure DrawEditBorder(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawSingleBorder(ACanvas: TCanvas; ARect: TRect);
  procedure DrawButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ADrawFocusRect: Boolean; AScaleDrawFactor: Double = 1);
  procedure DrawSegmentedButton(ACanvas: TCanvas; ARect: TRect;
    ACtrlState: TscsCtrlState; ADrawFocusRect: Boolean; ADrawCode: Integer; AScaleDrawFactor: Double = 1);
  procedure DrawSegmentedToolButton(ACanvas: TCanvas; ARect: TRect;
    ACtrlState: TscsCtrlState; ADrawCode: Integer; AScaleDrawFactor: Double = 1);
  procedure DrawSplitButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
     ADroppedDown: Boolean; AChevronWidth: Integer);
  procedure DrawSplitToolButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
    ADroppedDown: Boolean; AChevronWidth: Integer);
  procedure DrawToolButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawToolButton2(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawUpSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawDownSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawLeftSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawRightSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  function GetCheckBoxSize(ACanvas: TCanvas; AScaleFactor: Double = 1): Integer;
  procedure DrawTreeCheckBox(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);
  procedure DrawCheckBox(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);
  procedure DrawCheckBoxInCenter(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);
  procedure DrawRadioButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);
  procedure DrawDropDownButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ANewStyle: Boolean; ARightToLeft: Boolean; AScaleFactor: Double = 1);
  procedure DrawGroupBoxFrame(ACanvas: TCanvas; ARect: TRect);
  procedure DrawHeaderSection(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawCategoryHeader(ACanvas: TCanvas; ARect: TRect);
  procedure DrawSelection(ACanvas: TCanvas; ARect: TRect; AFocused, AFocusedSelection: Boolean);
  procedure DrawSelectionWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
  procedure FillRectWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
  procedure DrawToolBar(ACanvas: TCanvas; ARect: TRect);
  procedure DrawTab(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
    ATabPosition: TTabPosition; AFirst, ALast: Boolean);
  procedure DrawSimpleTab(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawTabFrame(ACanvas: TCanvas; ARect: TRect);
  procedure DrawPopupWindow(ACanvas: TCanvas; ARect: TRect);
  procedure DrawPopupSelectionWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
  procedure DrawTabWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
  procedure DrawPopupItem(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawFormBackground(ACanvas: TCanvas; ARect: TRect);
  procedure DrawProgressBar(ACanvas: TCanvas; ARect: TRect; AMin, AMax, AValue: Integer);
  procedure DrawProgressBarBorder(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean);
  procedure DrawProgressBarChunk(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean);
  procedure DrawTrackBarTrack(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean);
  procedure DrawTrackBarThumb(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawStatusPanel(ACanvas: TCanvas; ARect: TRect);
  procedure DrawStatusBar(ACanvas: TCanvas; ARect: TRect);
  // scroll bar
  procedure DrawUpScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawDownScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawLeftScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawRightScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
  procedure DrawHScrollThumb(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawHScrollFrame(ACanvas: TCanvas; ARect: TRect);
  procedure DrawVScrollThumb(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
  procedure DrawVScrollFrame(ACanvas: TCanvas; ARect: TRect);
  // get text colors
  function GetActiveTextColor: TColor;
  function GetEditTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetEditBrushColor(ACtrlState: TscsCtrlState): TColor;
  function GetButtonTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetToolButtonTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetCheckBoxTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetRadioButtonTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetGroupBoxTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetHeaderTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetCategoryHeaderTextColor: TColor;
  function GetStyleColor(AColor: TColor): TColor;
  function GetSelectionTextColor: TColor;
  function GetTabTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetSimpleTabTextColor(ACtrlState: TscsCtrlState): TColor;
  function GetPopupItemTextColor(ACtrlState: TscsCtrlState): TColor;
 function GetCaptionTextColor(ACtrlState: TscsCtrlState): TColor;
  // draw text and image
  procedure DrawTabbedString(S: String; TW: TStrings; C: TCanvas; R: TRect; Offset: Integer);
  procedure AngleDrawText(Canvas: TCanvas; Angle: Integer; X, Y: Integer; const Text: string; AColor: TColor);
  procedure scDrawText(ACanvas: TCanvas; AText: String; ARect: TRect; ARightToLeft: Boolean; ANoPrefix: Boolean);
  procedure scDrawClipText(ACanvas: TCanvas; AText: String; ARect: TRect; ARightToLeft: Boolean; ANoPrefix: Boolean);
  function  scDrawTextBiDiModeFlags(Flags: Longint; UseRightToLeftAlignment: Boolean): Longint;
  procedure CorrectTextbyWidth(ACanvas: TCanvas; var AText: String; AWidth: Integer);

  procedure CalcLCoord(ALayout: TButtonLayout; ARect: TRect; gw, gh: Integer; tw, th: Integer;
    ASpacing, AMargin: Integer; var tx, ty, gx, gy: Integer);

  procedure DrawTextLeftMultiLine(ACanvas: TCanvas; AText: String; ARect: TRect);
  procedure DrawTextCenterMultiLine(ACanvas: TCanvas; AText: String; ARect: TRect; AModify: Boolean);
  function DrawTextMultiLine(ACanvas: TCanvas; AText: String; ARect: TRect; ALeft, ARightToLeft: Boolean; ANoPrefix: Boolean): Boolean;
  procedure DrawWordWrapText(ADC: HDC; AText: String; ARect: TRect);
  procedure DrawTextAlignment(ACanvas: TCanvas; AText: String; ARect: TRect; AAlignment: TAlignment);
  procedure DrawTextAlignmentNoPrefix(ACanvas: TCanvas; AText: String; ARect: TRect; AAlignment: TAlignment; ARightToLeft: Boolean);

  procedure DrawImageAndTextLeft(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ARightToLeft: Boolean; ANoPrefix: Boolean);

  procedure DrawImageAndTextLeft2(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ARightToLeft: Boolean; ANoPrefix: Boolean);

  procedure DrawImageAndText(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor; ADrawFocusRect: Boolean;
            ARightToLeft: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);

  procedure DrawImageAndText2(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor; ADrawFocusRect: Boolean;
            ARightToLeft: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);

  function DrawImageAndTextEllipses(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ARightToLeft: Boolean; ANoPrefix: Boolean;
            AScaleDrawFactor: Double = 1): Boolean;

  function DrawImageAndTextEllipsesWithGlow(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ARightToLeft: Boolean; ALeft: Boolean;
            AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
            AImageGlow: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1): Boolean;

  procedure DrawImageWithGlow(ACanvas: TCanvas; AX, AY: Integer; AImageList: TCustomImageList; AImageIndex: Integer;
    AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AGlowAlpha: Byte);

  procedure DrawImageWithGlowBuffer(var ABuffer: TBitmap; AUpdateBuffer: Boolean;
    ACanvas: TCanvas; AX, AY: Integer; AImageList: TCustomImageList; AImageIndex: Integer;
    AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AGlowAlpha: Byte);

  procedure DrawTextWithGlow(ACanvas: TCanvas; ARect: TRect; AText: string; AFlags: cardinal;
      AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
      ARightToLeft: Boolean; ANoPrefix: Boolean);

  procedure DrawTextWithGlowBuffer(var ABuffer: TBitmap; AUpdateBuffer: Boolean; ACanvas: TCanvas; ARect: TRect; AText: string; AFlags: cardinal;
      AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
      ARightToLeft: Boolean; ANoPrefix: Boolean);

  function DrawTextMultiLineGlow(ACanvas: TCanvas; AText: String; ARect: TRect;
    ALeft, ARightToLeft: Boolean;
    AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AIntensiveGlow: Boolean; AGlowAlpha: Byte; ANoPrefix: Boolean): Boolean;

  procedure DrawImageAndTextWithGlow(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
            AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
            AImageGlow: Boolean;
            ADrawFocusRect: Boolean;
            ARightToLeft: Boolean;
            ANoPrefix: Boolean; AScaleDrawFactor: Double = 1);

  procedure DrawImageAndTextWithGlow2(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
            AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
            AImageGlow: Boolean;
            ADrawFocusRect: Boolean;
            ARightToLeft: Boolean;
            ANoPrefix: Boolean; AScaleDrawFactor: Double = 1);

  procedure DrawImageAndTextWithGlowBuffer(var ABuffer, AImageBuffer: TBitmap; AUpdateBuffer: Boolean;
            ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
            AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
            AImageGlow: Boolean;
            ADrawFocusRect: Boolean;
            ARightToLeft: Boolean;
            ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);

  procedure DrawImageAndTextWithGlowBuffer2(var ABuffer, AImageBuffer: TBitmap; AUpdateBuffer: Boolean;
            ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
            AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
            AImageGlow: Boolean;
            ADrawFocusRect: Boolean;
            ARightToLeft: Boolean;
            ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);
  // cursor
  function GetCursorHeightMargin: Integer;
  // bitmap effects
  function scColor(AColor: TColor; AAlpha: Byte): TscColor;
  function Bitmap_CheckAlpha(ABitmap: TBitmap): Boolean;
  function Bitmap_CheckAlpha0(ABitmap: TBitmap): Boolean;
  function Bitmap_CheckAlphaRect(ABitmap: TBitmap; ARect: TRect): Boolean;
  procedure Bitmap_MakeCopyFromPng(ABitmap: TBitMap; APngObject: TPngImage);
  procedure Bitmap_CheckOptions(ABitmap: TBitmap; AOptions: TscBitmapOptions);
  procedure Bitmap_DrawWithOptions(ABitmap: TBitmap; AOptions: TscBitmapOptions; ACanvas: TCanvas; ARect: TRect; Alpha: Byte = 255);
  procedure Bitmap_Clear(ABitmap: TBitmap; AColor: TscColor);
  procedure Bitmap_FillColor(ABitmap: TBitmap; Color: TColor);
  procedure Bitmap_ClearAlpha(ABitmap: TBitmap; Alpha: Byte);
  procedure Bitmap_DivAlpha(ABitmap: TBitmap; ADivider: Byte);
  procedure Bitmap_DrawAlpha(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect; Alpha: Byte);
  procedure Bitmap_DrawAlpha_Tile(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect; Alpha: Byte);
  procedure Bitmap_Draw(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect);
  procedure Bitmap_Draw_Tile(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect);
  procedure Bitmap_DrawAlpha_XY(ABitmap: TBitmap; ACanvas: TCanvas; AX, AY: integer; Alpha: Byte);
  procedure Bitmap_DrawScaleAlpha_XY(ABitmap: TBitmap; ACanvas: TCanvas; AX, AY: integer; Alpha: Byte; AScaleFactor: Double); overload;
  procedure Bitmap_DrawScaleAlpha_XY(ABitmap: TBitmap; ACanvas: TCanvas; AX, AY: integer; Alpha: Byte; ANewWidth, ANewHeight: Integer); overload;
  procedure Bitmap_Rotate90_1(ABitmap, ADest: TBitmap);
  procedure Bitmap_Rotate90_2(ABitmap, ADest: TBitmap);
  procedure Bitmap_FlipVert(ABitmap: TBitmap);
  procedure Bitmap_Reflection(ABitmap: TBitmap);
  procedure Bitmap_Blur(ABitmap: TBitmap; const Radius: integer);
  procedure Bitmap_AlphaScale(Src: TBitmap; Dst: TBitmap);
  function Bitmap_GetFromImageList(IL: TCustomImageList; AIndex: Integer;
    B: TBitmap): Boolean;
  function Bitmap_GetFromImageListNoPremultiplied(IL: TCustomImageList; AIndex: Integer;
    B: TBitmap; ACheckAlpha: Boolean): Boolean;
  procedure CreatePngFromBitmap(B: TBitMap; P: TPngImage);
  procedure CreateGlowBitmapFromPng(B: TBitMap; P: TPngImage; GlowSize: Integer; GlowColor: TColor);
  procedure CreateGlowBitmapFromBitmap(B, SourceB: TBitMap; GlowSize: Integer; GlowColor: TColor);
  procedure CreateGlowBitmapFromImageList(B: TBitMap; IL: TCustomImageList; IIndex: Integer; GlowSize: Integer; GlowColor: TColor);
  procedure CreateGlowBitmapFromMask(B: TBitMap; GlowSize: Integer; GlowColor: TColor);
  procedure DrawBitmapFromImageList(ACanvas: TCanvas; X, Y: Integer; IL: TCustomImageList; IIndex: Integer; Alpha: Byte);
  procedure DrawBitmapFromImageListToRect(ACanvas: TCanvas; ARect: TRect; IL: TCustomImageList; IIndex: Integer; Alpha: Byte);
  procedure GetBitmapFromImageList(ABitmap: TBitmap; IL: TCustomImageList; IIndex: Integer);
  procedure DrawStrecthBitmap(ALeftOffset, ATopOffset, ARightOffset, ABottomOffset: Integer;
    ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect);
  // other
  function CreateNullRgn: HRGN;
  procedure DrawParentBackground(AControl: TWinControl; ADest: TCanvas);
  procedure Frm3D(ACanvas: TCanvas; var ARect: TRect; ATopColor, ABottomColor: TColor);
  function EqPoints(const Pt1, Pt2: TPoint): Boolean;
  function PointInRect(ARect: TRect; APoint: TPoint): Boolean;
  function RectInRect(AR1, AR2: TRect): Boolean;
  function RectToRect(AR1, AR2: TRect): Boolean;
  function RectToCenter(var ARect: TRect; ABounds: TRect): TRect;

  procedure DrawMenuRadioImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);
  procedure DrawMenuCheckImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);
  procedure DrawMenuArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; ACode: Integer; AScaleFactor: Double = 1);

  procedure DrawToolGroupArrowImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);

  procedure DrawArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; ACode: Integer; AScaleFactor: Double = 1);
  procedure DrawButtonArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
  procedure DrawButtonModernArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
  procedure DrawComboArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
  procedure DrawDownArrowImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);
  procedure DrawModernArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
  procedure DrawModernMenuArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; ACode: Integer; AScaleFactor: Double = 1);

  procedure DrawTreeArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AColor2: TColor; ACode: Integer; AScaleFactor: Double = 1; AModern: Boolean = False);
  procedure DrawTreeExpandImage(ACanvas: TCanvas; ARect: TRect; AExpanded: Boolean; ABorderColor: TColor; AScaleFactor: Double = 1);
  procedure DrawTreeExpandImageColor(ACanvas: TCanvas; ARect: TRect; AExpanded: Boolean; AColor, AGlyphColor: TColor; AScaleFactor: Double = 1);

  procedure DrawTabCloseImage(C: TCanvas; R: TRect; Color: TColor; AScaleFactor: Double = 1);

  procedure scDrawFocusRect(ACanvas: TCanvas; ARect: TRect; AScaleFactor: Double = 1);
  procedure scDrawFocusRect2(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
  procedure scDrawColorFocusRect(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
  procedure DrawPixelLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer; AColor: TColor);

  procedure DrawHorzSplitter(ACanvas: TCanvas; ARect: TRect);
  procedure DrawVertSplitter(ACanvas: TCanvas; ARect: TRect; AIsRightToLeft: Boolean = False);

  function MiddleColor(AColor1, AColor2: TColor): TColor;
  function DarkerColor(Color:TColor; Percent: Byte): TColor;
  function LighterColor(Color:TColor; Percent: Byte): TColor;
  function AlternateColor(AColor: TColor): TColor;
  function IsLightColor(AColor: TColor): Boolean;
  function HexStringToColor(HexString: string; var Color: Integer): Boolean;
  function DecStringToColor(DecString: String; var Color: Integer): Boolean;
  function NameStringToColor(NameString: String; var Color: Integer): Boolean;

  function SC_DwmCompositionEnabled: Boolean;

  var

  SC_BeginBufferedAnimation: function (hwnd: HWND; hdcTarget: HDC;
    var prcTarget: TRect; dwFormat: DWORD; pPaintParams: PscBPPaintParams;
    var pAnimationParams: TscBPAnimationParams; var phdcFrom: HDC;
    var phdcTo: HDC): SC_HANIMATIONBUFFER; stdcall;

  SC_EndBufferedAnimation: function (hbpAnimation: SC_HANIMATIONBUFFER;
    fUpdateTarget: BOOL): HResult; stdcall;

  SC_BufferedPaintRenderAnimation: function (hwnd: HWND; hdcTarget: HDC): BOOL; stdcall;

  SC_BufferedPaintStopAllAnimations: function (hwnd: HWND): HResult; stdcall;

  SC_DwmIsCompositionEnabled: function(out pfEnabled: BOOL): HRESULT; stdcall;
  SC_DwmExtendFrameIntoClientArea: function (hWnd: HWND; const pMarInset: TscMargins): HResult; stdcall;
  SC_SetWindowCompositionAttribute: function (hWnd: HWND; const AttrData: TscWinCompAttrData): BOOL; stdcall = nil;
  SC_DwmSetWindowAttribute: function (hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HResult; stdcall;


  const
     DisabledImageAlphaValue = 80;

     SC_BPAS_NONE = 0;
     SC_BPAS_LINEAR = 1;
     SC_BPAS_CUBIC = 2;
     SC_BPAS_SINE = 3;
     SC_BPBF_COMPATIBLEBITMAP = 0;

     // SmartEffects messages
     CM_SEPAINT = $B000 + 456;
     CM_SENCPAINT = $B000 + 457;
     SE_RESULT = $3233;

     SC_WCA_ACCENT_POLICY = 19;
     SC_ACCENT_ENABLE_BLURBEHIND = 3;
     SC_ACCENT_ENABLE_ACRYLICBLURBEHIND = 4;


     // scale styles elements (checkbox, radiobutton, trackbar thumb...)
   var
     SC_SCALESTYLES: Boolean = True;
     SC_SCALETHEMES: Boolean = False;
     SC_SCALERESOURCES: Boolean = True;
     SC_SCALEFORMBORDER: Boolean = True;
     SC_StopSetMenuRedraw: Boolean = False;
     SC_MODERNARROWS: Boolean = False;
     SC_MENUCOLORSELECTION: Boolean = False;
     SC_RTLMODE: Boolean = False;

implementation

 uses
   System.Types, System.SysUtils, System.Math, System.UITypes, WinApi.CommCtrl{$IFDEF GPDRAWING},scGPUtils{$ENDIF};
 var
   FOSMajor, FOSMinor: Integer;

 type
   TParentControl = class(TWinControl);


constructor TscGlowEffect.Create;
begin
  FEnabled := False;
  FColor := clHighLight;
  FAlphaValue := 255;
  FOffset := 0;
  FGlowSize := 7;
  FIntensive := True;
  FStyleColors := True;
end;

procedure TscGlowEffect.Assign(Source: TPersistent);
begin
  if Source is TscGlowEffect then
  begin
    FEnabled := TscGlowEffect(Source).FEnabled;
    FColor := TscGlowEffect(Source).FColor;
    FAlphaValue := TscGlowEffect(Source).FAlphaValue;
    FOffset := TscGlowEffect(Source).FOffset;
    FGlowSize := TscGlowEffect(Source).FGlowSize;
    FIntensive := TscGlowEffect(Source).FIntensive;
    FStyleColors := TscGlowEffect(Source).FStyleColors;
  end
  else
    inherited Assign(Source);
end;

procedure TscGlowEffect.Changed;
begin
  if FEnabled then
    if Assigned(FOnChange) then FOnChange(Self);
end;

function TscGlowEffect.GetAlphaValue: Byte;
begin
  Result := FAlphaValue;
  if (FColor = clHighLight) and (Result > 170) and
     not IsCustomStyle and not StyleServices.Enabled then
    Result := Result - 80;
end;

function TscGlowEffect.GetColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FColor)
  else
    Result := FColor;
end;

procedure TscGlowEffect.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGlowEffect.SetIntensive(Value: Boolean);
begin
  if FIntensive <> Value then
  begin
    FIntensive := Value;
    Changed;
  end;
end;

procedure TscGlowEffect.SetOffset(Value: Byte);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Changed;
  end;
end;

procedure TscGlowEffect.SetGlowSize(Value: Byte);
begin
  if FGlowSize <> Value then
  begin
    FGlowSize := Value;
    Changed;
  end;
end;

procedure TscGlowEffect.SetAlphaValue(Value: Byte);
begin
  if FAlphaValue <> Value then
  begin
    FAlphaValue := Value;
    Changed;
  end;
end;

procedure TscGlowEffect.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
procedure TscGlowEffect.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

function TscButtonGlowEffect.GetHotColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscButtonGlowEffect.GetPressedColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscButtonGlowEffect.GetFocusedColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscButtonGlowEffect.GetPressedAlphaValue: Byte;
begin
  Result := FPressedAlphaValue;
  if (FPressedColor = clHighLight) and (Result > 200) and
     not IsCustomStyle and not StyleServices.Enabled then
    Result := Result - 50;
end;

procedure TscButtonGlowEffect.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscButtonGlowEffect.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

constructor TscButtonGlowEffect.Create;
begin
  inherited;
  FHotColor := clNone;
  FPressedColor := clNone;
  FFocusedColor := clNone;
  FPressedAlphaValue := 255;
  FPressedGlowSize := 7;
  FStates := [scsHot, scsPressed, scsFocused];
end;

procedure TscButtonGlowEffect.Assign(Source: TPersistent);
begin
  if Source is TscButtonGlowEffect then
  begin
    inherited Assign(Source);
    FHotColor := TscButtonGlowEffect(Source).FHotColor;
    FPressedColor := TscButtonGlowEffect(Source).FPressedColor;
    FFocusedColor := TscButtonGlowEffect(Source).FFocusedColor;
    FPressedAlphaValue := TscButtonGlowEffect(Source).FPressedAlphaValue;
    FPressedGlowSize := TscButtonGlowEffect(Source).FPressedGlowSize;
    FStates := TscButtonGlowEffect(Source).FStates;
  end
  else
    inherited Assign(Source);
end;

procedure TscButtonGlowEffect.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscButtonGlowEffect.SetPressedAlphaValue(Value: Byte);
begin
  if FPressedAlphaValue <> Value then
  begin
    FPressedAlphaValue := Value;
    Changed;
  end;
end;

procedure TscButtonGlowEffect.SetPressedGlowSize(Value: Byte);
begin
  if FPressedGlowSize <> Value then
  begin
    FPressedGlowSize := Value;
    Changed;
  end;
end;

procedure TscButtonGlowEffect.SetStates(Value: TscsCtrlStates);
begin
  if FStates <> Value then
  begin
    FStates := Value;
    Changed;
  end;
end;

constructor TscBitmapOptions.Create;
begin
  FLeftMargin := 0;
  FRightMargin := 0;
  FTopMargin := 0;
  FBottomMargin := 0;
  FAlphaBlend := False;
  FAlphaBlendBorder := False;
  FAlphaBlendCorners := False;
  FDrawOnlyBorder := False;
  FStretch := True;
  FStretchBorder := False;
  FDrawInClipRect := False;
  FContentLeftMargin := 0;
  FContentRightMargin := 0;
  FContentTopMargin := 0;
  FContentBottomMargin := 0;
end;

procedure TscBitmapOptions.Assign(Source: TPersistent);
begin
  if Source is TscBitmapOptions then
  begin
    FLeftMargin := TscBitmapOptions(Source).FLeftMargin;
    FRightMargin := TscBitmapOptions(Source).FRightMargin;
    FTopMargin := TscBitmapOptions(Source).FTopMargin;
    FBottomMargin := TscBitmapOptions(Source).FBottomMargin;
    FAlphaBlend := TscBitmapOptions(Source).FAlphaBlend;
    FAlphaBlendBorder := TscBitmapOptions(Source).FAlphaBlendBorder;
    FAlphaBlendCorners := TscBitmapOptions(Source).FAlphaBlendCorners;
    FStretch := TscBitmapOptions(Source).FStretch;
    FStretchBorder := TscBitmapOptions(Source).FStretchBorder;
    FDrawOnlyBorder := TscBitmapOptions(Source).FDrawOnlyBorder;
  end
  else
    inherited Assign(Source);
end;

procedure TscBitmapOptions.SetLeftMargin(Value: Integer);
begin
  FLeftMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscBitmapOptions.SetRightMargin(Value: Integer);
begin
  FRightMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscBitmapOptions.SetTopMargin(Value: Integer);
begin
  FTopMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscBitmapOptions.SetBottomMargin(Value: Integer);
begin
  FBottomMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;


type
  WKSTA_INFO_100 = record
    wki100_platform_id: DWORD;
    wki100_computername: LPWSTR;
    wki100_langroup: LPWSTR;
    wki100_ver_major: DWORD;
    wki100_ver_minor: DWORD;
  end;
  LPWKSTA_INFO_100 = ^WKSTA_INFO_100;

function NetWkstaGetInfo(ServerName: LPWSTR; Level: DWORD; BufPtr: LPBYTE): Longint; stdcall;
  external 'netapi32.dll' name 'NetWkstaGetInfo';

function GetNetWkstaMajorMinor(var MajorVersion, MinorVersion: DWORD): Boolean;
var
  LBuf: LPWKSTA_INFO_100;
begin
  Result := NetWkstaGetInfo(nil, 100, @LBuf) = 0;
  if Result then
  begin
    MajorVersion := LBuf^.wki100_ver_major;
    MinorVersion := LBuf^.wki100_ver_minor;
  end;
end;

procedure GetWindowsVersion(var Major, Minor: Integer);
var
  Ver : Longint;
  MajorNum: DWord;
  MinorNum: DWord;
begin
  Ver := GetVersion;
  Major := LoByte(LoWord(Ver));
  Minor := HiByte(LoWord(Ver));
  if (Major > 6) or ((Major = 6) and (Minor > 1)) then
  begin
    if GetNetWkstaMajorMinor(MajorNum, MinorNum) then
    begin
      Major := MajorNum;
      Minor := MinorNum;
    end;
  end;
end;

function IsWindows10: Boolean;
begin
  Result := FOSMajor >= 10;
end;

function IsWindows8: Boolean;
begin
  Result := (FOSMajor = 6) and (FOSMinor >= 2);
end;

function IsWindows7: Boolean;
begin
  Result := (FOSMajor = 6) and (FOSMinor = 1);
end;

function IsWindowsXP: Boolean;
begin
  Result := (FOSMajor = 5) and (FOSMinor = 1);
end;

procedure RGBToHSL1(AR, AV, AB: Byte; var H, S, L: Double);
var
  R,G,B,D,
  Cmax, Cmin: Double;
begin
  R := AR / 255;
  G := AV / 255;
  B := AB / 255;
  Cmax := Max (R, Max (G, B));
  Cmin := Min (R, Min (G, B));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin
  then
    begin
      H := 0;
      S := 0
    end
  else
    begin
      D := Cmax - Cmin;
      if L < 0.5 then S := D / (Cmax + Cmin) else S := D / (2 - Cmax - Cmin);
      if R = Cmax
      then
        H := (G - B) / D
      else
        if G = Cmax then H  := 2 + (B - R) /D else H := 4 + (R - G) / D;
      H := H / 6;
      if H < 0 then  H := H + 1;
    end;
end;

procedure RGBToHSL2(AR, AG, AB: Byte; var H, S, L: Integer);
var
  RGB: array[0..2] of Double;
  MinIndex, MaxIndex: Integer;
  Range: Double;
  H1 : Double;
begin
  RGB[0]:= AR;
  RGB[1]:= AG;
  RGB[2]:= AB;

  MinIndex:= 0;
  if AG < AR then MinIndex:= 1;
  if AB < RGB[MinIndex] then MinIndex:= 2;

  MaxIndex:= 0;
  if AG > AR then MaxIndex:= 1;
  if AB > RGB[MaxIndex] then MaxIndex:= 2;
  Range:= RGB[MaxIndex] - RGB[MinIndex];

  if Range = 0
  then
    begin
      S := 0;
      L := Round(100 * AR / 255);
    end
  else
    begin
      H1 := MaxIndex * 2 + (AR - AG) / Range;
      S := Round(Range / RGB[MaxIndex] * 100);
      L :=  Round(100 * (RGB[MaxIndex] / 255));
      H1 := H1 / 6;
      if H1 < 0 then H1 := H1 + 1;
      H := Round(H1 * 359);
    end;
end;

procedure RGBToHSL(AR, AG, AB: Byte; var RH, RS, RL: Integer);
var
  H, S, L: Double;
begin
  RGBToHSL1(AR, AG, AB, H, S, L);
  RGBToHSL2(AR, AG, AB, RH, RS, RL);
  if RS <> 0 then RH := Round(H * 359);
end;

procedure HSLToRGB(var R, G, B: Byte; RH, RS, RL: Integer);
const
  SectionSize = 60/360;
var
  Section: Double;
  SectionIndex: Integer;
  f, p, q, t, H, S, L: Double;
begin
  H := RH / 360;
  S := RS / 100;
  L := (255 * RL / 100);
  if S = 0
  then
    begin
      R := Round(L);
      G := R;
      B := R;
    end
  else
   begin
     Section := H / SectionSize;
     SectionIndex := Floor(Section);
     f := Section - SectionIndex;
     p := L * ( 1 - S );
     q := L * ( 1 - S * f );
     t := L * ( 1 - S * ( 1 - f ) );
     case SectionIndex of
      0:
        begin
          R := Round(L);
          G := Round(t);
          B := Round(p);
        end;
      1:
        begin
          R := Round(q);
          G := Round(L);
          B := Round(p);
        end;
      2:
        begin
          R := Round(p);
          G := Round(L);
          B := Round(t);
        end;
      3:
        begin
          R := Round(p);
          G := Round(q);
          B := Round(L);
        end;
      4:
        begin
          R := Round(t);
          G := Round(p);
          B := Round(L);
        end;
    else
      R := Round(L);
      G := Round(p);
      B := Round(q);
    end;
  end;
end;

function DarkerColor(Color: TColor; Percent: Byte): TColor;
var
  r, g, b: Byte;
begin
  if Percent > 100 then
    Percent := 100;
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := Byte(r - MulDiv(r, Percent, 100));
  g := Byte(g - MulDiv(g, Percent, 100));
  b := Byte(b - MulDiv(b, Percent, 100));
  Result := RGB(r, g, b);
end;

function LighterColor(Color: TColor; Percent: Byte): TColor;
var
  r, g, b: Byte;
begin
  if Percent > 100 then
    Percent := 100;
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := Byte(r + MulDiv(255 - r, Percent, 100));
  g := Byte(g + MulDiv(255 - g, Percent, 100));
  b := Byte(b + MulDiv(255 - b, Percent, 100));
  Result := RGB(r, g, b);
end;

function RectVCenter(var ARect: TRect; ABounds: TRect): TRect;
begin
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  OffsetRect(ARect, 0, (ABounds.Height - ARect.Height) div 2);
  OffsetRect(ARect, ABounds.Left, ABounds.Top);
  Result := ARect;
end;

function IsLightColor(AColor: TColor): Boolean;
var
  r, g, b: Byte;
  FColor: TColor;
begin
  FColor := GetStyleColor(AColor);
  FColor := ColorToRGB(FColor);
  r := GetRValue(FColor);
  g := GetGValue(FColor);
  b := GetBValue(FColor);
  if (r + g + b) div 3 >= 127
  then
    Result := True
  else
    Result := False;
end;

function IsDarkStyle: Boolean;
var
  r, g, b: Byte;
  FColor: TColor;
begin
  FColor := GetStyleColor(clBtnFace);
  FColor := ColorToRGB(FColor);
  r := GetRValue(FColor);
  g := GetGValue(FColor);
  b := GetBValue(FColor);
  if (r + g + b) div 3 <= 127  then
    Result := True
  else
    Result := False;
end;

function IsDarkWindowStyle: Boolean;
var
  r, g, b: Byte;
  FColor: TColor;
begin
  FColor := GetStyleColor(clWindow);
  FColor := ColorToRGB(FColor);
  r := GetRValue(FColor);
  g := GetGValue(FColor);
  b := GetBValue(FColor);
  if (r + g + b) div 3 <= 127  then
    Result := True
  else
    Result := False;
end;

function AlternateColor(AColor: TColor): TColor;
var
  r, g, b: Byte;
  H, S, L: Integer;
begin
  AColor := ColorToRGB(AColor);
  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  RGBTOHSL(r, g, b, h, s, l);
  if l < 50
  then
    Result := LighterColor(AColor, 10)
  else
    Result := DarkerColor(AColor, 10);
end;

function MiddleColor(AColor1, AColor2: TColor): TColor;
var
  r1, g1, b1, r2, g2, b2: Byte;
begin
  AColor1 := ColorToRGB(AColor1);
  AColor2 := ColorToRGB(AColor2);
  r1 := GetRValue(AColor1);
  g1 := GetGValue(AColor1);
  b1 := GetBValue(AColor1);
  r2 := GetRValue(AColor2);
  g2 := GetGValue(AColor2);
  b2 := GetBValue(AColor2);
  r1 := (r1 + r2) div 2;
  g1 := (g1 + g2) div 2;
  b1 := (b1 + b2) div 2;
  Result := RGB(r1, g1, b1);
end;

procedure CorrectTextbyWidth(ACanvas: TCanvas; var AText: String; AWidth: Integer);
var
  j: Integer;
begin
  j := Length(AText);
  with ACanvas do  begin
    if TextWidth(AText) > AWidth then
    begin
      repeat
        Delete(AText, j, 1);
        Dec(j);
      until (TextWidth(AText + '...') <= AWidth) or (AText = '');
      AText := AText + '...';
     end;
  end;
end;

procedure DrawWordWrapText(ADC: HDC; AText: String; ARect: TRect);
var
  I: Integer;
  S1, S2: String;
  R: TRect;
  FModify: Boolean;
begin
  if AText = '' then Exit;
  S1 := '';
  S2 := '';
  FModify := False;
  for I := 1 to Length(AText) do
  begin
    S1 := S1 + AText[I];
    R := Rect(0, 0, 0, 0);
    WinApi.Windows.DrawText(ADC, PChar(S1), Length(S1), R, DT_LEFT or
      DT_CALCRECT or DT_NOPREFIX);
    if (R.Width > ARect.Width) and (AText[I] <> ' ') then
    begin
      FModify := True;
      S2 := S2 + ' ' + AText[I];
      S1 := AText[I];
    end
    else
    begin
      S2 := S2 + AText[I];
      if AText[I] = ' ' then S1 := '';
    end;
  end;
  if FModify then
    WinApi.Windows.DrawText(ADC, PChar(S2), Length(S2), ARect,
      DT_WORDBREAK or DT_CENTER or DT_NOPREFIX)
  else
    WinApi.Windows.DrawText(ADC, PChar(AText), Length(AText), ARect,
      DT_LEFT or DT_NOPREFIX);
end;

function DrawTextMultiLine(ACanvas: TCanvas; AText: String; ARect: TRect; ALeft,
  ARightToLeft: Boolean;  ANoPrefix: Boolean): Boolean;
var
  S1, S2: String;
  TempR, TR: TRect;
  I, TY: Integer;
  Flags: Longint;
begin
  if AText = '' then Exit(False);
  Result := False;
  Flags := DT_WORDBREAK;
  if ALeft then
    Flags := Flags or DT_LEFT
  else
    Flags := Flags or DT_CENTER;
  if ANoPrefix then
    Flags := Flags or DT_NOPREFIX;
  Flags := scDrawTextBidiModeFlags(Flags, ARightToLeft);
  S1 := '';
  S2 := '';
  if ARect.Width <= 0 then Exit;
  if ARect.Height < ACanvas.TextHeight('Wq') then
    ARect.Bottom := ARect.Top + ACanvas.TextHeight('Wq');
  TempR := Rect(0, 0, ARect.Width, ARect.Height);
  SetLength(S2, Length(AText));
  S2 := Copy(AText, 1, Length(AText));
  DrawText(ACanvas.Handle, PChar(S2), Length(S2), TempR,
     Flags or DT_CALCRECT);
  if (TempR.Height <= ARect.Height) and (TempR.Width <= ARect.Width) then
  begin
    TY := ARect.Top + ARect.Height div 2 - TempR.Height div 2;
    TR := Rect(ARect.Left, TY, ARect.Right, TY + TempR.Height);
    DrawText(ACanvas.Handle, PChar(S2), Length(S2), TR,
       Flags);
    Exit;
  end;
  Result := True;
  repeat
    Delete(S2, Length(S2), 1);
    TempR := Rect(0, 0, ARect.Width, ARect.Height);
    DrawText(ACanvas.Handle, PChar(S2), Length(S2), TempR,
    Flags or DT_CALCRECT);
  until ((TempR.Height <= ARect.Height) and (TempR.Width <= ARect.Width)) or (S2 = '');
  if Length(S2) > 3 then
  begin
    I := Length(s2);
    S2[I - 2] := '.';
    S2[I - 1] := '.';
    S2[I] := '.';
  end
  else
  if S2 <> '' then
  begin
    S2 := S2[1] + '...';
    TempR := Rect(0, 0, ARect.Width, ARect.Height);
    DrawText(ACanvas.Handle, PChar(S2), Length(S2), TempR,
    Flags or DT_CALCRECT);
    if not ((TempR.Height <= ARect.Height) and (TempR.Width <= ARect.Width)) then
    S2 := '...';
  end
  else
    S2 := '...';
  TY := ARect.Top + ARect.Height div 2 - TempR.Height div 2;
  TR := Rect(ARect.Left, TY, ARect.Right, TY + TempR.Height);
  DrawText(ACanvas.Handle, PChar(S2), Length(S2), TR, Flags);
end;

procedure DrawTextCenterMultiLine(ACanvas: TCanvas; AText: String; ARect: TRect; AModify: Boolean);
var
  S1, S2, S3, S4: String;
  TempR: TRect;
  I: Integer;
begin
  if AText = '' then Exit;
  S1 := '';
  S2 := '';
  if not AModify
  then
    begin
      TempR := Rect(0, 0, RectWidth(ARect), RectHeight(ARect));
      SetLength(S2, Length(AText));
      S2 := Copy(AText, 1, Length(AText));
      DrawText(ACanvas.Handle, PChar(S2), Length(S2), TempR,
        DT_WORDBREAK or DT_CENTER or DT_CALCRECT or DT_NOPREFIX);
      if (TempR.Height <= ARect.Height) and
         (TempR.Width <= ARect.Width) then
      begin
        DrawText(ACanvas.Handle, PChar(S2), Length(S2), ARect,
          DT_WORDBREAK or DT_CENTER or DT_NOPREFIX);
        Exit;
      end;
      S1 := '';
      S4 := '';
      for I := 1 to Length(S2) do
      begin
        S4 := S1;
        S1 := S1 + S2[I];
        S3 := S1 + '...';
        TempR := Rect(0, 0, ARect.Width, ARect.Height);
        DrawText(ACanvas.Handle, PChar(S3), Length(S3), TempR,
          DT_WORDBREAK or DT_CENTER or DT_CALCRECT or DT_NOPREFIX);
        if (TempR.Height > ARect.Height) or
           (TempR.Width > ARect.Width) then
          Break;
      end;
      S4 := S4 + '...';
      DrawText(ACanvas.Handle, PChar(S4), Length(S4), ARect,
        DT_WORDBREAK or DT_CENTER or DT_NOPREFIX);
    end
  else
  begin
    for I := 1 to Length(AText) do
    begin
      S1 := S1 + AText[I];
      TempR := Rect(0, 0, 0, 0);
      DrawText(ACanvas.Handle, PChar(S1), Length(S1), TempR, DT_LEFT or DT_CALCRECT or DT_NOPREFIX);
      if (TempR.Width > ARect.Width) and (AText[I] <> ' ')
      then
        begin
         S2 := S2 + ' ' + AText[I];
         S1 := AText[I];
       end
      else
        begin
         S2 := S2 + AText[I];
         if AText[I] = ' ' then S1 := '';
       end;
     end;
    DrawText(ACanvas.Handle, PChar(S2), Length(S2), ARect,
     DT_WORDBREAK or DT_CENTER or DT_NOPREFIX);
  end;
end;

procedure DrawTextLeftMultiLine(ACanvas: TCanvas; AText: String; ARect: TRect);
var
  S: String;
  TempR: TRect;
begin
  if AText = '' then Exit;
  S := '';
  TempR := Rect(0, 0, ARect.Width, ARect.Height);
  SetLength(S, Length(AText));
  S := Copy(AText, 1, Length(AText));
  DrawText(ACanvas.Handle, PChar(S), Length(S), TempR,
     DT_LEFT or  DT_SINGLELINE or DT_VCENTER or DT_CALCRECT or DT_NOPREFIX);
  if (TempR.Height <= ARect.Height) and (TempR.Width <= ARect.Width) then
  begin
    DrawText(ACanvas.Handle, PChar(S), Length(S), ARect,
       DT_LEFT or DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    Exit;
  end;
  TempR := Rect(0, 0, ARect.Width, ARect.Height);
  DrawText(ACanvas.Handle, PChar(S), Length(S), TempR,
     DT_WORDBREAK or DT_LEFT or DT_CALCRECT or DT_NOPREFIX);
  if (TempR.Height <= ARect.Height) and (TempR.Width <= ARect.Width) then
  begin
    DrawText(ACanvas.Handle, PChar(S), Length(S), ARect,
       DT_WORDBREAK or DT_LEFT or DT_VCENTER or DT_NOPREFIX);
    Exit;
  end
  else
    DrawText(ACanvas.Handle, PChar(S), Length(S), ARect,
      DT_LEFT or DT_VCENTER or  DT_SINGLELINE or DT_MODIFYSTRING or
      DT_END_ELLIPSIS or DT_NOPREFIX);
end;


function scDrawTextBiDiModeFlags(Flags: Longint; UseRightToLeftAlignment: Boolean): Longint;
begin
  Result := Flags;
  if UseRightToLeftAlignment then
    if Result and DT_RIGHT = DT_RIGHT then
      Result := Result and not DT_RIGHT
    else if not (Result and DT_CENTER = DT_CENTER) then
      Result := Result or DT_RIGHT;
  if UseRightToLeftAlignment then
    Result := Result or DT_RTLREADING;
end;

function IsWindowsModernStyle: Boolean;
begin
  if IsCustomStyle then
    Result := Pos('windows', LowerCase(TStyleManager.ActiveStyle.Name)) <> 0
  else
    Result := False;
end;

function IsCustomStyle: Boolean;
begin
  Result := TStyleManager.IsCustomStyleActive;
end;

procedure DrawDownArrowImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);
var
  i: Integer;
  X, Y: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  ACanvas.Pen.Color := Color;
  if AScaleFactor <> 1 then
    AScaleFactor := AScaleFactor + 0.1;
  X := ARect.Left + ARect.Width div 2;
  Y := ARect.Top + ARect.Height div 2 + Round(3 * AScaleFactor);
  for i := Round(3 * AScaleFactor) downto 0 do
  with ACanvas do
  begin
    MoveTo(X - i, Y - i);
    LineTo(X + i + 1, Y - i);
  end;
  for i := Round(AScaleFactor) downto 1 do
  with ACanvas do
  begin
    MoveTo(X - Round(3 * AScaleFactor), Y - Round(4 * AScaleFactor) - i - 1);
    LineTo(X + Round(3 * AScaleFactor) + 1, Y - Round(4 * AScaleFactor) - i - 1);
  end;
end;

procedure DrawToolGroupArrowImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);
var
  i: Integer;
  X, Y, W, O: Integer;
begin
  ACanvas.Pen.Color := Color;
  W := Round(7 * AScaleFactor);
  if not Odd(W) then W := W + 1;

  O := Trunc(AScaleFactor);
  X := ARect.Left + ARect.Width div 2 - W div 2;
  Y := ARect.Top + ARect.Height div 2 - W div 2;
  ARect := Rect(X, Y, X + W, Y + W);
  X := ARect.Right;
  Y := ARect.Bottom - 1;
  for i := 0 to Round(4 * AScaleFactor) do
    with ACanvas do
    begin
      MoveTo(X - i, Y);
      LineTo(X, Y - i);
    end;
  X := ARect.Left;
  Y := ARect.Top;
  with ACanvas do
  begin
    MoveTo(X, Y);
    LineTo(X, ARect.Bottom - O);
    MoveTo(X, Y);
    LineTo(X + ARect.Width - O, Y);
  end;
  X := ARect.Right - 1;
  Y := ARect.Bottom - 1;
  O := W div 2 + O;
  with ACanvas do
  begin
    MoveTo(X, Y);
    LineTo(X - O, Y - O);
  end;
end;

procedure DrawModernArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
var
  i: Integer;
  X, Y: Integer;
  Offset: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  ACanvas.Pen.Color := AColor;
  Offset := Round(4 * AScaleFactor);
  X := ARect.Left + ARect.Width div 2;
  Y := ARect.Top + ARect.Height div 2 + Round(2 * AScaleFactor) - 1;
  with ACanvas do
  begin
    for I := 1 to Round(AScaleFactor + 1) do
    begin
      if I = 1 then
      begin
        MoveTo(X, Y);
        LineTo(X - Offset + 1, Y - Offset + 1);
        MoveTo(X, Y);
        LineTo(X + Offset - 1, Y - Offset + 1);
      end
      else
      begin
        MoveTo(X, Y);
        LineTo(X - Offset, Y - Offset);
        MoveTo(X, Y);
        LineTo(X + Offset, Y - Offset);
      end;
      Inc(Y);
    end;
  end;
end;

procedure DrawComboArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
var
  i: Integer;
  X, Y: Integer;
  Offset: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  ACanvas.Pen.Color := AColor;
  if (IsWindows10 and not IsCustomStyle) or (IsCustomStyle and (IsWindowsModernStyle or SC_MODERNARROWS)) then
  begin
    Offset := Round(5 * AScaleFactor);
    X := ARect.Left + ARect.Width div 2;
    Y := ARect.Top + ARect.Height div 2 + Round(2 * AScaleFactor) - 1;
    with ACanvas do
    begin
      for I := 1 to Round(AScaleFactor + 1) do
      begin
        if I = 1 then
        begin
          MoveTo(X, Y);
          LineTo(X - Offset + 1, Y - Offset + 1);
          MoveTo(X, Y);
          LineTo(X + Offset - 1, Y - Offset + 1);
        end
        else
        begin
          MoveTo(X, Y);
          LineTo(X - Offset, Y - Offset);
          MoveTo(X, Y);
          LineTo(X + Offset, Y - Offset);
        end;
        Inc(Y);
      end;
    end;
  end
  else
  if IsWindows8 and not IsCustomStyle then
  begin
    X := ARect.Left + ARect.Width div 2;
    Y := ARect.Top + ARect.Height div 2 + Round(2 * AScaleFactor) - 1;
    Offset := Round(4 * AScaleFactor);
    with ACanvas do
    begin
      for I := 1 to Round(3 * AScaleFactor) do
      begin
        MoveTo(X, Y);
        LineTo(X - Offset, Y - Offset);
        MoveTo(X, Y);
        LineTo(X + Offset, Y - Offset);
        Inc(Y);
      end;
    end;
  end
  else
  begin
    if AScaleFactor <> 1 then
      AScaleFactor := AScaleFactor + 0.1;
    X := ARect.Left + ARect.Width div 2;
    Y := ARect.Top + ARect.Height div 2 + Round(2 * AScaleFactor);
    for i := Round(3 * AScaleFactor) downto 0 do
    with ACanvas do
    begin
      MoveTo(X - i, Y - i);
      LineTo(X + i + 1, Y - i);
    end;
  end;
end;

procedure DrawTreeArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AColor2: TColor; ACode: Integer; AScaleFactor: Double = 1; AModern: Boolean = False);
var
  i: Integer;
  X, Y: Integer;
  Offset: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  if IsWindows10 or AModern then
  with ACanvas do
  begin
    if AScaleFactor > 1.5 then
    begin
      AScaleFactor := AScaleFactor - 0.5;
      if AScaleFactor < 1.5 then
        AScaleFactor := 1.5;
    end;
    Pen.Color := AColor;
    case ACode of
      1:
        begin
          X := ARect.Left + ARect.Width div 2 + Round(AScaleFactor) - 1;
          Y := ARect.Top + ARect.Height div 2;
          Offset := Round(5 * AScaleFactor);
          with ACanvas do
          begin
            for I := 1 to Round(3 * AScaleFactor) do
            begin
             if I = 1 then
              begin
                MoveTo(X, Y);
                LineTo(X - Offset + 1, Y - Offset + 1);
                MoveTo(X, Y);
                LineTo(X - Offset + 1, Y + Offset - 1);
              end
              else
              begin
                MoveTo(X, Y);
                LineTo(X - Offset, Y - Offset);
                MoveTo(X, Y);
                LineTo(X - Offset, Y + Offset);
              end;
              Inc(X);
           end;
         end;
        end;
      2:
        begin
          X := ARect.Left + ARect.Width div 2;
          Y := ARect.Top + ARect.Height div 2 + Round(AScaleFactor) - 1;
          Offset := Round(5 * AScaleFactor);
          with ACanvas do
          begin
            for I := 1 to Round(3 * AScaleFactor) do
            begin
              if I = 1 then
              begin
                MoveTo(X, Y);
                LineTo(X - Offset + 1, Y - Offset + 1);
                MoveTo(X, Y);
                LineTo(X + Offset - 1, Y - Offset + 1);
              end
              else
              begin
                MoveTo(X, Y);
                LineTo(X - Offset, Y - Offset);
                MoveTo(X, Y);
                LineTo(X + Offset, Y - Offset);
              end;
              Inc(Y);
           end;
         end;
        end;
    end;
  end
  else
  with ACanvas do
  begin
    if AScaleFactor <> 1 then
      AScaleFactor := AScaleFactor + 0.1;
    Pen.Color := AColor;
    case ACode of
      1:
        begin
          X := ARect.Left + ARect.Width div 2 + Round(2 * AScaleFactor);
          Y := ARect.Top + ARect.Height div 2;
          for i := Round(3 * AScaleFactor + 1) downto 0 do
           begin
             if i = Round(3 * AScaleFactor + 1) then Pen.Color := AColor2 else Pen.Color := AColor;
             MoveTo(X - i, Y + i);
             LineTo(X - i, Y - i - 1);
             if i <> Round(3 * AScaleFactor + 1)then
             begin
               ACanvas.Pixels[X - i, Y + i] := AColor2;
               ACanvas.Pixels[X - i, Y - i] := AColor2;
             end;
           end;
        end;
      2:
        begin
          X := ARect.Left + ARect.Width div 2 + Round(2 * AScaleFactor);
          Y := ARect.Top + ARect.Height div 2 - Round(2 * AScaleFactor) + 2;
          for i := Round(5 * AScaleFactor + 1) downto 0 do
          begin
            if i = Round(5 * AScaleFactor + 1) then Pen.Color := AColor2 else Pen.Color := AColor;
            MoveTo(X - i, Y - (4 - i));
            LineTo(X, Y - (4 - i));
            if I <  Round(5 * AScaleFactor + 1) then
             begin
               ACanvas.Pixels[X - i - 1, Y - (4 - i) + 1] := AColor2;
               ACanvas.Pixels[X - 1, Y - (4 - i) + 1] := AColor2;
             end;
          end;
        end;
    end;
  end;
end;


procedure DrawButtonModernArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
var
  SaveIndex, X, Y, i: Integer;
  FOldPen: HPEN;
  Offset: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(AColor));
    Offset := Round(4 * AScaleFactor);
    X := ARect.Left + ARect.Width div 2;
    Y := ARect.Top + ARect.Height div 2 + Round(2 * AScaleFactor) - 1;
    with ACanvas do
    begin
      for I := 1 to Round(AScaleFactor + 1) do
      begin
        if I = 1 then
        begin
          MoveTo(X, Y);
          LineTo(X - Offset + 1, Y - Offset + 1);
          MoveTo(X, Y);
          LineTo(X + Offset - 1, Y - Offset + 1);
        end
        else
        begin
          MoveTo(X, Y);
          LineTo(X - Offset, Y - Offset);
          MoveTo(X, Y);
          LineTo(X + Offset, Y - Offset);
        end;
        Inc(Y);
      end;
    end;
    SelectObject(ACanvas.Handle, FOldPen);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure DrawButtonArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
var
  SaveIndex, X, Y, i: Integer;
  FOldPen: HPEN;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(AColor));
    if AScaleFactor <> 1 then
      AScaleFactor := AScaleFactor + 0.1;
    X := ARect.Left + ARect.Width div 2;
    Y := ARect.Top + ARect.Height div 2 + Round(2 * AScaleFactor);
    for i := Round(3 * AScaleFactor) downto 0 do
    with ACanvas do
    begin
      MoveTo(X - i, Y - i);
      LineTo(X + i + 1, Y - i);
    end;
    SelectObject(ACanvas.Handle, FOldPen);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure DrawArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; ACode: Integer; AScaleFactor: Double = 1);
var
  i: Integer;
  X, Y: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  if Round(AScaleFactor) <> Trunc(AScaleFactor) then
    AScaleFactor := Round(AScaleFactor);
  with ACanvas do
  begin
    Pen.Color := AColor;
    case ACode of
      1:
        begin
          X := ARect.Left + ARect.Width div 2 - Round(2 * AScaleFactor);
          Y := ARect.Top + ARect.Height div 2;
          for i := 0 to Round(2 * AScaleFactor + 1) do
          begin
            MoveTo(X + i, Y - i);
            LineTo(X + i, Y + i + 1);
          end;
        end;
      2:
        begin
          X := ARect.Left + ARect.Width div 2 + Round(2 * AScaleFactor);
          Y := ARect.Top + ARect.Height div 2;
          for i := Round(2 * AScaleFactor + 1) downto 0 do
           begin
             MoveTo(X - i, Y + i);
             LineTo(X - i, Y - i - 1);
           end;
        end;
      3:
        begin
          X := ARect.Left + ARect.Width div 2;
          Y := ARect.Top + ARect.Height div 2 - Round(2 * AScaleFactor);
          for i := 0 to Round(2 * AScaleFactor + 1) do
          begin
            MoveTo(X - i, Y + i);
            LineTo(X + i + 1, Y + i);
          end;
        end;
      4:
        begin
          X := ARect.Left + ARect.Width div 2;
          Y := ARect.Top + ARect.Height div 2 + Round(2 * AScaleFactor);
          for i := Round(2 * AScaleFactor + 1) downto 0 do
          begin
            MoveTo(X - i, Y - i);
            LineTo(X + i + 1, Y - i);
          end;
        end;
    end;
  end;
end;


procedure DrawModernMenuArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; ACode: Integer; AScaleFactor: Double = 1);
var
  i: Integer;
  X, Y: Integer;
  SaveIndex: Integer;
  FOldPen: HPEN;
  Offset: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  if Round(AScaleFactor) <> Trunc(AScaleFactor) then
    AScaleFactor := Round(AScaleFactor);
  Offset := Round(4 * AScaleFactor);
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(AColor));
    with ACanvas do
    begin
      case ACode of
      1:
        begin
          X := ARect.Left + ARect.Width div 2 + Round(2 * AScaleFactor) - 1;
          Y := ARect.Top + ARect.Height div 2;
          for I := 1 to Trunc(AScaleFactor + 1) do
          begin
            MoveTo(X, Y);
            LineTo(X - Offset, Y - Offset);
            MoveTo(X, Y);
            LineTo(X - Offset, Y + Offset);
            Dec(X);
          end;
        end;
      2:
        begin
          X := ARect.Left + ARect.Width div 2 - Round(2 * AScaleFactor) - 1;
          Y := ARect.Top + ARect.Height div 2;
          for I := 1 to Trunc(AScaleFactor + 1) do
          begin
            MoveTo(X, Y);
            LineTo(X + Offset, Y - Offset);
            MoveTo(X, Y);
            LineTo(X + Offset, Y + Offset);
            Dec(X);
          end;
        end;
      end;
    end;
    SelectObject(ACanvas.Handle, FOldPen);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure DrawMenuArrowImage(ACanvas: TCanvas; ARect: TRect; AColor: TColor; ACode: Integer; AScaleFactor: Double = 1);
var
  i: Integer;
  X, Y: Integer;
  SaveIndex: Integer;
  FOldPen: HPEN;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  if Round(AScaleFactor) <> Trunc(AScaleFactor) then
    AScaleFactor := Round(AScaleFactor);
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(AColor));
    with ACanvas do
    begin
      case ACode of
      1:
        begin
          X := ARect.Left + ARect.Width div 2 - Round(2 * AScaleFactor);
          Y := ARect.Top + ARect.Height div 2;
          for i := 0 to Round(2 * AScaleFactor + 1) do
          begin
            MoveTo(X + i, Y - i);
            LineTo(X + i, Y + i + 1);
          end;
        end;
      2:
        begin
          X := ARect.Left + ARect.Width div 2 + Round(2 * AScaleFactor);
          Y := ARect.Top + ARect.Height div 2;
          for i := Round(2 * AScaleFactor + 1) downto 0 do
           begin
             MoveTo(X - i, Y + i);
             LineTo(X - i, Y - i - 1);
           end;
        end;
      end;
    end;
    SelectObject(ACanvas.Handle, FOldPen);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure DrawMenuRadioImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);
var
  SaveIndex: Integer;
  FOldPen: HPEN;
  X, Y: Integer;
  Size, Size2, I: Integer;
  Offset: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  AScaleFactor := AScaleFactor + 0.1;
  X := ARect.Left + ARect.Width div 2;
  Y := ARect.Top + ARect.Height div 2;
  Size := Round(6 * AScaleFactor);
  Size2 := Size div 2;
  Size := Size2 * 2;
  Y := Y - Size2;
  X := X - Size2;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(Color));
    for I := 1 to Size2 do
     with ACanvas do
     begin
       Offset := Size div 2 - I - 1;
       if Offset < 0 then Offset := 0;
       MoveTo(X + Offset, Y);
       LineTo(X + Size - Offset, Y);
       Inc(Y);
     end;
     Y := Y + Size2 - 1;
     for I := 1 to Size2 do
     with ACanvas do
     begin
       Offset := Size div 2 - I - 1;
       if Offset < 0 then Offset := 0;
       MoveTo(X + Offset, Y);
       LineTo(X + Size - Offset, Y);
       Dec(Y);
     end;
    SelectObject(ACanvas.Handle, FOldPen);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure DrawMenuCheckImage(ACanvas: TCanvas; ARect: TRect; Color: TColor; AScaleFactor: Double = 1);
var
  SaveIndex: Integer;
  FOldPen: HPEN;
  X, Y: Integer;
  Size, Size2, Size3: Integer;
  i: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(Color));
    Size := Round(10 * AScaleFactor);
    Size2 := Size div 2;
    Size := Size2 * 2;
    Size3 := Round(2 * AScaleFactor);
    X := ARect.Left + ARect.Width div 2 + Size3 div 2 - 1;
    Y := ARect.Top + ARect.Height div 2 + Size3 div 2 - 1;
    X := X - Size2;
    with ACanvas do
    begin
      for I := 1 to Round(AScaleFactor) * 2 do
      begin
        MoveTo(X, Y);
        LineTo(X + Size2 - Size3, Y + Size2 - Size3);
        LineTo(X + Size, Y - Size3 * 2);
        Dec(Y);
        MoveTo(X + 1, Y + 1);
        LineTo(X + Size2 - Size3, Y + Size2 - Size3);
        LineTo(X + Size - 1, Y - Size3 * 2 + 1);
      end;
    end;
    SelectObject(ACanvas.Handle, FOldPen);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure DrawUpSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  SaveIndex: Integer;
  R, R1: TRect;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    if not IsCustomStyle then
     case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsUpNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsUpHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsUpPressed);
        scsDisabled:
          Details := StyleServices.GetElementDetails(tsUpDisabled);
      end
    else
      case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsArrowBtnUpNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsArrowBtnUpHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsArrowBtnUpPressed);
        scsDisabled:
          Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
      end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
    if not IsCustomStyle and (ARect.Height <= 9) then
    begin
      R := ARect;
      R.Bottom := R.Top + 2;
      R1 := ARect;
      R1.Bottom := R1.Top + 10;
      SaveIndex := SaveDC(ACanvas.Handle);
      try
        IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        StyleServices.DrawElement(ACanvas.Handle, Details, R1{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
      finally
        RestoreDC(ACanvas.Handle, SaveIndex);
      end;
    end;
  end
  else
    with ACanvas do
    begin
      if ACtrlState = scsDisabled then
       DrawFrameControl(Handle, ARect, DFC_SCROLL,  DFCS_SCROLLUP or DFCS_INACTIVE)
      else
      if ACtrlState = scsPressed then
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLUP or DFCS_PUSHED)
      else
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLUP);
    end;
end;

procedure DrawDownSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    if not IsCustomStyle then
      case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsDownNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsDownHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsDownPressed);
        scsDisabled:
           Details := StyleServices.GetElementDetails(tsDownDisabled);
       end
     else
      case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsArrowBtnDownNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsArrowBtnDownHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsArrowBtnDownPressed);
        scsDisabled:
          Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
      end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
  end
  else
    with ACanvas do
    begin
      if ACtrlState = scsDisabled then
        DrawFrameControl(Handle, ARect, DFC_SCROLL,  DFCS_SCROLLDOWN or DFCS_INACTIVE)
      else
      if ACtrlState = scsPressed then
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLDOWN or DFCS_PUSHED)
      else
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLDOWN);
    end;
end;

procedure DrawRightSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    if not IsCustomStyle then
     case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsUpHorzNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsUpHorzHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsUpHorzPressed);
        scsDisabled:
           Details := StyleServices.GetElementDetails(tsUpHorzDisabled);
      end
    else
      case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsArrowBtnRightNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsArrowBtnRightHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsArrowBtnRightPressed);
        scsDisabled:
          Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
      end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
  end
  else
    with ACanvas do
    begin
      if ACtrlState = scsDisabled then
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLRIGHT or DFCS_INACTIVE)
      else
      if ACtrlState = scsPressed then
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLRIGHT or DFCS_PUSHED)
     else
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLRIGHT);
    end;
end;

procedure DrawLeftSpinButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    if not IsCustomStyle then
      case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsDownHorzNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsDownHorzHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsDownHorzPressed);
        scsDisabled:
          Details := StyleServices.GetElementDetails(tsDownHorzDisabled);
      end
    else
      case ACtrlState of
        scsNormal:
           Details := StyleServices.GetElementDetails(tsArrowBtnLeftNormal);
        scsHot, scsFocused:
           Details := StyleServices.GetElementDetails(tsArrowBtnLeftHot);
        scsPressed:
           Details := StyleServices.GetElementDetails(tsArrowBtnLeftPressed);
        scsDisabled:
          Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
      end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
  end
  else
    with ACanvas do
    begin
      if ACtrlState = scsDisabled then
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLLEFT or DFCS_INACTIVE)
      else
      if ACtrlState = scsPressed then
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLLEFT or DFCS_PUSHED)
      else
        DrawFrameControl(Handle, ARect, DFC_SCROLL, DFCS_SCROLLLEFT);
    end;
end;

procedure DrawSegmentedButton(ACanvas: TCanvas; ARect: TRect;
  ACtrlState: TscsCtrlState; ADrawFocusRect: Boolean; ADrawCode: Integer; AScaleDrawFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  R1, R2: TRect;
begin
  R1 := ARect;
  R2 := ARect;
  Dec(R2.Bottom, 2);
  Inc(R2.Top, 2);
  case ADrawCode of
    1: Inc(R1.Right, 20);
    2:
      begin
        Dec(R1.Left, 20);
        Inc(R1.Right, 20);
      end;
    3: Dec(R1.Left, 20);
  end;
  if ACtrlState = scsFocused then
    ACtrlState := scsHot;
  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(tbPushButtonNormal);
      scsHot:
         Details := StyleServices.GetElementDetails(tbPushButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tbPushButtonPressed);
      scsFocused:
         Details := StyleServices.GetElementDetails(tbPushButtonDefaulted);
      scsDisabled:
         Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, R1);
    if StyleServices.IsSystemStyle and ADrawFocusRect then
    begin
      InflateRect(ARect, -3, -3);
      if ADrawFocusRect then scDrawFocusRect(ACanvas, ARect, AScaleDrawFactor);
    end;
  end
  else
  with ACanvas do
  begin
    if ACtrlState = scsPressed then
      DrawFrameControl(Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
    else
      DrawFrameControl(Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
    if ADrawFocusRect and not IsCustomStyle then
    begin
      InflateRect(ARect, -3, -3);
      DrawFocusRect(ARect);
    end;
  end;
  R1 := R2;
  case ADrawCode of
    1:
      begin
        R1.Left := R1.Right - 1;
        Inc(R1.Right);
        DrawVertSplitter(ACanvas, R1);
      end;
    2:
      begin
        R1.Left := R1.Right - 1;
        Inc(R1.Right);
        DrawVertSplitter(ACanvas, R1);
        R1 := R2;
        R1.Right := R1.Left;
        Dec(R1.Left);
        DrawVertSplitter(ACanvas, R1);
      end;
    3:
      begin
        R1.Right := R1.Left;
        Dec(R1.Left);
        DrawVertSplitter(ACanvas, R1);
      end;
  end;
end;

procedure DrawSegmentedToolButton(ACanvas: TCanvas; ARect: TRect;
  ACtrlState: TscsCtrlState; ADrawCode: Integer; AScaleDrawFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  R1, R2: TRect;
begin
  R1 := ARect;
  R2 := ARect;
  Dec(R2.Bottom, 2);
  Inc(R2.Top, 2);
  case ADrawCode of
    1: Inc(R1.Right, 20);
    2:
      begin
        Dec(R1.Left, 20);
        Inc(R1.Right, 20);
      end;
    3: Dec(R1.Left, 20);
  end;
  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(ttbButtonNormal);
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(ttbButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(ttbButtonPressed);
      scsDisabled:
         Details := StyleServices.GetElementDetails(ttbButtonDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, R1);
  end
  else
  with ACanvas do
  begin
    if ACtrlState = scsPressed then
      Frm3D(ACanvas, R1, clBtnShadow, clBtnHighlight)
    else
    if (ACtrlState = scsHot) or (ACtrlState = scsFocused) then
      Frm3D(ACanvas, R1, clBtnHighlight, clBtnShadow);
  end;
  R1 := R2;
  case ADrawCode of
    1:
      begin
        R1.Left := R1.Right - 1;
        Inc(R1.Right);
        DrawVertSplitter(ACanvas, R1);
      end;
    2:
      begin
        R1.Left := R1.Right - 1;
        Inc(R1.Right);
        DrawVertSplitter(ACanvas, R1);
        R1 := R2;
        R1.Right := R1.Left;
        Dec(R1.Left);
        DrawVertSplitter(ACanvas, R1);
      end;
    3:
      begin
        R1.Right := R1.Left;
        Dec(R1.Left);
        DrawVertSplitter(ACanvas, R1);
      end;
  end;
end;

procedure DrawButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
  ADrawFocusRect: Boolean; AScaleDrawFactor: Double = 1);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(tbPushButtonNormal);
      scsHot:
         Details := StyleServices.GetElementDetails(tbPushButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tbPushButtonPressed);
      scsFocused:
         Details := StyleServices.GetElementDetails(tbPushButtonDefaulted);
      scsDisabled:
         Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    if StyleServices.IsSystemStyle and ADrawFocusRect then
    begin
      InflateRect(ARect, -3, -3);
      if ADrawFocusRect then scDrawFocusRect(ACanvas, ARect, AScaleDrawFactor);
    end;
  end
  else
  with ACanvas do
  begin
    if ACtrlState = scsPressed then
      DrawFrameControl(Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
    else
      DrawFrameControl(Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
    if ADrawFocusRect and not IsCustomStyle then
    begin
      InflateRect(ARect, -3, -3);
      DrawFocusRect(ARect);
    end;
  end;
end;

procedure DrawSplitButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
   ADroppedDown: Boolean; AChevronWidth: Integer);
var
  R, CR: TRect;
  SaveIndex: Integer;
begin
  R := ARect;
  Dec(R.Right, AChevronWidth);
  CR := ARect;
  CR.Left := CR.Right - AChevronWidth;
  if not ADroppedDown then
    DrawButton(ACanvas, ARect, ACtrlState, False)
  else
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      DrawButton(ACanvas, ARect, scsHot, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, CR.Left, CR.Top, CR.Right, CR.Bottom);
      DrawButton(ACanvas, ARect, scsPressed, False);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;
  Inc(CR.Top, 2);
  Dec(CR.Bottom, 2);
  DrawVertSplitter(ACanvas, CR);
end;

procedure DrawSplitToolButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
   ADroppedDown: Boolean; AChevronWidth: Integer);
var
  R, CR: TRect;
  SaveIndex: Integer;
begin
  R := ARect;
  Dec(R.Right, AChevronWidth);
  CR := ARect;
  CR.Left := CR.Right - AChevronWidth;
  if not ADroppedDown then
    DrawToolButton(ACanvas, ARect, ACtrlState)
  else
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      DrawToolButton(ACanvas, ARect, scsHot);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, CR.Left, CR.Top, CR.Right, CR.Bottom);
      DrawToolButton(ACanvas, ARect, scsPressed);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;
  if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
  begin
    Inc(CR.Top, 2);
    Dec(CR.Bottom, 2);
    DrawVertSplitter(ACanvas, CR);
  end;
end;

procedure DrawToolButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(ttbButtonNormal);
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(ttbButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(ttbButtonPressed);
      scsDisabled:
         Details := StyleServices.GetElementDetails(ttbButtonDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
    begin
      if ACtrlState = scsPressed then
        Frm3D(ACanvas, ARect, clBtnShadow, clBtnHighlight)
      else
      if (ACtrlState = scsHot) or (ACtrlState = scsFocused) then
        Frm3D(ACanvas, ARect, clBtnHighlight, clBtnShadow);
    end;
end;

procedure DrawToolButton2(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(ttbButtonNormal);
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(ttbButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(ttbButtonPressed);
      scsDisabled:
         Details := StyleServices.GetElementDetails(ttbButtonDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
    begin
      Frm3D(ACanvas, ARect, clBtnFace, clBtnFace);
      if ACtrlState = scsPressed then
        Frm3D(ACanvas, ARect, clBtnShadow, clBtnHighlight)
      else
      if (ACtrlState = scsHot) or (ACtrlState = scsFocused) then
        Frm3D(ACanvas, ARect, clBtnHighlight, clBtnShadow);
    end;
end;

function GetCheckBoxSize(ACanvas: TCanvas; AScaleFactor: Double = 1): Integer;
var
  LRect: TRect;
  ElementSize: TElementSize;
  BoxSize: TSize;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  {$IFDEF VER330_UP}
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}
  if StyleServices.Enabled then
  begin
    ElementSize := esActual;
    if StyleServices.GetElementSize(ACanvas.Handle,
         StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         LRect, ElementSize, BoxSize{$IFDEF VER330_UP},FDPI{$ENDIF}) then
    begin
      Result := BoxSize.cx;
      {$IFNDEF VER330_UP}
      if SC_SCALETHEMES and
      not IsCustomStyle and (Abs(BoxSize.cx - Round(13 * AScaleFactor)) > AScaleFactor) then
        Result := Round(13 * AScaleFactor);
      {$ENDIF}
    end
    else
      Result := 13;
  end
  else
    Result := 13;
  if (AScaleFactor > 1) and (IsCustomStyle or not StyleServices.Enabled) then
    Result := Round(Result * AScaleFactor);
end;

procedure DrawTreeCheckBox(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
  ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);

procedure CheckScaleFactor(AWidth: Integer);
var
  W, NewW: Integer;
begin
  if AScaleFactor > 1.25 then
  begin
    W := Round(AWidth * AScaleFactor);
    NewW := Round(13 * AScaleFactor);
    if W > NewW + 2 then
    begin
      repeat
        AScaleFactor := AScaleFactor - 0.1;
        W := Round(AWidth * AScaleFactor);
      until (W < NewW + 2) or (AScaleFactor < 1);
    end;
  end;
  if AScaleFactor < 1 then AScaleFactor := 1;
end;


var
  Details:  TThemedElementDetails;
  R, R1: TRect;
  SaveIndex, Size: Integer;
  BoxSize, ScaleBoxSize: TSize;
  ElementSize: TElementSize;
  Buffer: TBitmap;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  if AScaleFactor < 1 then AScaleFactor := 1;

  {$IFDEF VER330_UP}
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         begin
           case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
          end
         end;
      scsHot:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedHot);
          end
        end;
      scsPressed:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
          end
        end;
      scsDisabled:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
          end
        end;
    end;
    R := ARect;
    ElementSize := esActual;
    if not StyleServices .GetElementSize(ACanvas.Handle,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, ElementSize, BoxSize {$IFDEF VER330_UP},FDPI{$ENDIF}) then
    begin
      BoxSize.cx := 13;
      BoxSize.cy := 13;
    end;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if {$IFDEF VER330_UP}False{$ELSE}SC_SCALETHEMES{$ENDIF} and
        not IsCustomStyle and
        (Abs(BoxSize.cx - Round(13 * AScaleFactor)) > AScaleFactor) then
      begin
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(13 * AScaleFactor);
        ScaleBoxSize.cy := Round(13 * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          R := Rect(0, 0, ScaleBoxSize.cx, ScaleBoxSize.cy);
          RectVCenter(R, ARect);
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas,
            R.Right - ScaleBoxSize.cx, R.Top,
            255, Round(13 * AScaleFactor), Round(13 * AScaleFactor));
        finally
          Buffer.Free;
        end;
      end
      else
      if not (IsCustomStyle and (AScaleFactor >= 1.25) and SC_SCALESTYLES) then
      begin
        if IsCustomStyle and (AScaleFactor >= 1.25)  then
        begin
          R := Rect(0, 0, Round(BoxSize.cx * AScaleFactor), Round(BoxSize.cy * AScaleFactor));
          RectVCenter(R, ARect);
          R1 := Rect(R.CenterPoint.X - BoxSize.cx div 2, R.CenterPoint.Y - BoxSize.cy div 2,
            R.CenterPoint.X - BoxSize.cx div 2 + BoxSize.cx,
            R.CenterPoint.Y - BoxSize.cy div 2 + BoxSize.cy);
          StyleServices.DrawElement(ACanvas.Handle, Details, R1);
        end
        else
        begin
          R := Rect(0, 0, BoxSize.cx, BoxSize.cy);
          RectVCenter(R, ARect);
          if R.Right > ARect.Right then
            OffsetRect(R, -(R.Right - ARect.Right), 0);
          StyleServices.DrawElement(ACanvas.Handle, Details, R{$IFDEF VER330_UP}, nil, FDPI{$ENDIF});
        end;
      end
      else
      begin
        CheckScaleFactor(BoxSize.cx);
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(BoxSize.cx * AScaleFactor);
        ScaleBoxSize.cy := Round(BoxSize.cy * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          R := Rect(0, 0, ScaleBoxSize.cx, ScaleBoxSize.cy);
          RectVCenter(R, ARect);
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas, R.Right - ScaleBoxSize.cx, R.Top, 255, AScaleFactor);
        finally
          Buffer.Free;
        end;
      end;
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end
  else
  begin
    Size := Round(13 * AScaleFactor);
    R := Rect(0, 0, Size, Size);
    RectVCenter(R, ARect);
    case ACtrlState of
      scsNormal, scsHot:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED);
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE);
        end;
      scsPressed:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_PUSHED);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_PUSHED);
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE or DFCS_PUSHED);
        end;
      scsDisabled:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_INACTIVE);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE );
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE);
        end;
     end;
  end;
end;

procedure DrawCheckBox(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
  ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);

procedure CheckScaleFactor(AWidth: Integer);
var
  W, NewW: Integer;
begin
  if AScaleFactor > 1.25 then
  begin
    W := Round(AWidth * AScaleFactor);
    NewW := Round(13 * AScaleFactor);
    if W > NewW + 2 then
    begin
      repeat
        AScaleFactor := AScaleFactor - 0.1;
        W := Round(AWidth * AScaleFactor);
      until (W < NewW + 2) or (AScaleFactor < 1);
    end;
  end;
  if AScaleFactor < 1 then AScaleFactor := 1;
end;


var
  Details:  TThemedElementDetails;
  R, R1: TRect;
  SaveIndex, Size: Integer;
  BoxSize, ScaleBoxSize: TSize;
  ElementSize: TElementSize;
  Buffer: TBitmap;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  if AScaleFactor < 1 then AScaleFactor := 1;

  {$IFDEF VER330_UP}
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         begin
           case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
          end
         end;
      scsHot:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedHot);
          end
        end;
      scsPressed:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
          end
        end;
      scsDisabled:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
          end
        end;
    end;
    R := ARect;
    ElementSize := esActual;
    if not StyleServices.GetElementSize(ACanvas.Handle,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, ElementSize, BoxSize {$IFDEF VER330_UP}, FDPI{$ENDIF}) then
    begin
      BoxSize.cx := 13;
      BoxSize.cy := 13;
    end;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if {$IFDEF VER330_UP}False{$ELSE}SC_SCALETHEMES{$ENDIF} and
        not IsCustomStyle and
        (Abs(BoxSize.cx - Round(13 * AScaleFactor)) > AScaleFactor) then
      begin
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(13 * AScaleFactor);
        ScaleBoxSize.cy := Round(13 * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          R := Rect(0, 0, ScaleBoxSize.cx, ScaleBoxSize.cy);
          RectVCenter(R, ARect);
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas,
            R.Left, R.Top,
            255, Round(13 * AScaleFactor), Round(13 * AScaleFactor));
        finally
          Buffer.Free;
        end;
      end
      else
      if not (IsCustomStyle and (AScaleFactor >= 1.25) and SC_SCALESTYLES) then
      begin
        if IsCustomStyle and (AScaleFactor >= 1.25)  then
        begin
          R := Rect(0, 0, Round(BoxSize.cx * AScaleFactor), Round(BoxSize.cy * AScaleFactor));
          RectVCenter(R, ARect);
          R1 := Rect(R.CenterPoint.X - BoxSize.cx div 2, R.CenterPoint.Y - BoxSize.cy div 2,
            R.CenterPoint.X - BoxSize.cx div 2 + BoxSize.cx,
            R.CenterPoint.Y - BoxSize.cy div 2 + BoxSize.cy);
          StyleServices.DrawElement(ACanvas.Handle, Details, R1);
        end
        else
        begin
          R := Rect(0, 0, BoxSize.cx, BoxSize.cy);
          RectVCenter(R, ARect);
          StyleServices.DrawElement(ACanvas.Handle, Details, R{$IFDEF VER330_UP}, nil, FDPI{$ENDIF});
        end;
      end
      else
      begin
        CheckScaleFactor(BoxSize.cx);
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(BoxSize.cx * AScaleFactor);
        ScaleBoxSize.cy := Round(BoxSize.cy * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          R := Rect(0, 0, ScaleBoxSize.cx, ScaleBoxSize.cy);
          RectVCenter(R, ARect);
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas, R.Left, R.Top, 255, AScaleFactor);
        finally
          Buffer.Free;
        end;
      end;
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end
  else
  begin
    Size := Round(13 * AScaleFactor);
    R := Rect(0, 0, Size, Size);
    RectVCenter(R, ARect);
    case ACtrlState of
      scsNormal, scsHot:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED);
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE);
        end;
      scsPressed:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_PUSHED);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_PUSHED);
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE or DFCS_PUSHED);
        end;
      scsDisabled:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_INACTIVE);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE );
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE);
        end;
     end;
  end;
end;

procedure DrawCheckBoxInCenter(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
  ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);

procedure CheckScaleFactor(AWidth: Integer);
var
  W, NewW: Integer;
begin
  if AScaleFactor > 1.25 then
  begin
    W := Round(AWidth * AScaleFactor);
    NewW := Round(13 * AScaleFactor);
    if W > NewW + 2 then
    begin
      repeat
        AScaleFactor := AScaleFactor - 0.1;
        W := Round(AWidth * AScaleFactor);
      until (W < NewW + 2) or (AScaleFactor < 1);
    end;
  end;
  if AScaleFactor < 1 then AScaleFactor := 1;
end;

var
  Details:  TThemedElementDetails;
  R: TRect;
  SaveIndex, Size: Integer;
  BoxSize, ScaleBoxSize: TSize;
  ElementSize: TElementSize;
  Buffer: TBitmap;

  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}

begin
  if AScaleFactor < 1 then AScaleFactor := 1;

  {$IFDEF VER330_UP}
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         begin
           case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
          end
         end;
      scsHot:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedHot);
          end
        end;
      scsPressed:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
          end
        end;
      scsDisabled:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
            cbChecked: Details := StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
            cbGrayed: Details := StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
          end
        end;
    end;
    R := ARect;
    ElementSize := esActual;
    if not StyleServices.GetElementSize(ACanvas.Handle,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, ElementSize, BoxSize{$IFDEF VER330_UP}, FDPI{$ENDIF}) then
    begin
      BoxSize.cx := 13;
      BoxSize.cy := 13;
    end;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if {$IFDEF VER330_UP}False{$ELSE}SC_SCALETHEMES{$ENDIF} and
         not IsCustomStyle and (Abs(BoxSize.cx - Round(13 * AScaleFactor)) > AScaleFactor) then
      begin
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(13 * AScaleFactor);
        ScaleBoxSize.cy := Round(13 * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas,
            ARect.CenterPoint.X - ScaleBoxSize.cx div 2,
            ARect.CenterPoint.Y - ScaleBoxSize.cy div 2,
            255, Round(13 * AScaleFactor), Round(13 * AScaleFactor));
        finally
          Buffer.Free;
        end;
      end
      else
      if not (IsCustomStyle and (AScaleFactor >= 1.25) and SC_SCALESTYLES) then
      begin
        R := Rect(ARect.CenterPoint.X - BoxSize.cx div 2,
             ARect.CenterPoint.Y - BoxSize.cx div 2,
             ARect.CenterPoint.X - BoxSize.cx div 2 + BoxSize.cx,
             ARect.CenterPoint.Y - BoxSize.cy div 2 + BoxSize.cy);
        StyleServices.DrawElement(ACanvas.Handle, Details, R{$IFDEF VER330_UP}, nil, FDPI{$ENDIF});
      end
      else
      begin
        CheckScaleFactor(BoxSize.cx);
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(BoxSize.cx * AScaleFactor);
        ScaleBoxSize.cy := Round(BoxSize.cy * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas,
            ARect.CenterPoint.X - ScaleBoxSize.cx div 2,
            ARect.CenterPoint.Y - ScaleBoxSize.cy div 2,
            255, AScaleFactor);
        finally
          Buffer.Free;
        end;
      end;
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end
  else
  begin
    Size := Round(6 * AScaleFactor);
    R := Rect(ARect.CenterPoint.X - Size, ARect.CenterPoint.Y - Size, ARect.CenterPoint.X + Size + 1,
      ARect.CenterPoint.Y + Size + 1);
    case ACtrlState of
      scsNormal, scsHot:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED);
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE);
        end;
      scsPressed:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_PUSHED);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_PUSHED);
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE or DFCS_PUSHED);
        end;
      scsDisabled:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_INACTIVE);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE );
          cbGrayed: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_INACTIVE);
        end;
     end;
  end;
end;

procedure DrawRadioButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState; AScaleFactor: Double = 1);

procedure CheckScaleFactor(AWidth: Integer);
var
  W, NewW: Integer;
begin
  if AScaleFactor > 1.25 then
  begin
    W := Round(AWidth * AScaleFactor);
    NewW := Round(13 * AScaleFactor);
    if W > NewW + 2 then
    begin
      repeat
        AScaleFactor := AScaleFactor - 0.1;
        W := Round(AWidth * AScaleFactor);
      until (W < NewW + 2) or (AScaleFactor < 1);
    end;
  end;
  if AScaleFactor < 1 then AScaleFactor := 1;
end;


var
  Details:  TThemedElementDetails;
  R, R1: TRect;
  BoxSize, ScaleBoxSize: TSize;
  ElementSize: TElementSize;
  SaveIndex, Size: Integer;
  Buffer: TBitmap;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  if AScaleFactor < 1 then AScaleFactor := 1;

  {$IFDEF VER330_UP}
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         begin
           case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
            cbChecked: Details := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
          end
         end;
      scsHot:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
            cbChecked: Details := StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
          end
        end;
      scsPressed:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
            cbChecked: Details := StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
          end
        end;
      scsDisabled:
        begin
          case ACheckBoxState of
            cbUnChecked: Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
            cbChecked: Details := StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
          end
        end;
    end;
    R := ARect;
    ElementSize := esActual;
    if not StyleServices.GetElementSize(ACanvas.Handle,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, ElementSize, BoxSize {$IFDEF VER330_UP},FDPI{$ENDIF}) then
    begin
      BoxSize.cx := 13;
      BoxSize.cy := 13;
    end;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if {$IFDEF VER330_UP}False{$ELSE}SC_SCALETHEMES{$ENDIF} and
        not IsCustomStyle and (Abs(BoxSize.cx - Round(13 * AScaleFactor)) > AScaleFactor) then
      begin
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(13 * AScaleFactor);
        ScaleBoxSize.cy := Round(13 * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          R := Rect(0, 0, ScaleBoxSize.cx, ScaleBoxSize.cy);
          RectVCenter(R, ARect);
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas,
            R.Left, R.Top,
            255, Round(13 * AScaleFactor), Round(13 * AScaleFactor));
        finally
          Buffer.Free;
        end;
      end
      else
      if not (IsCustomStyle and (AScaleFactor >= 1.25) and SC_SCALESTYLES) then
      begin
        if IsCustomStyle and (AScaleFactor = 1.25)  then
        begin
          R := Rect(0, 0, Round(BoxSize.cx * 1.25), Round(BoxSize.cy * 1.25));
          RectVCenter(R, ARect);
          R1 := Rect(R.CenterPoint.X - BoxSize.cx div 2, R.CenterPoint.Y - BoxSize.cy div 2,
            R.CenterPoint.X - BoxSize.cx div 2 + BoxSize.cx,
            R.CenterPoint.Y - BoxSize.cy div 2 + BoxSize.cy);
          Inc(R1.Top);
          StyleServices.DrawElement(ACanvas.Handle, Details, R1);
        end
        else
        begin
          R := Rect(0, 0, BoxSize.cx, BoxSize.cy);
          RectVCenter(R, ARect);
          StyleServices.DrawElement(ACanvas.Handle, Details, R {$IFDEF VER330_UP},nil, FDPI{$ENDIF});
        end;
      end
      else
      begin
        CheckScaleFactor(BoxSize.cx);
        Inc(BoxSize.cx, 2);
        Inc(BoxSize.cy, 2);
        ScaleBoxSize.cx := Round(BoxSize.cx * AScaleFactor);
        ScaleBoxSize.cy := Round(BoxSize.cy * AScaleFactor);
        Buffer := TBitmap.Create;
        Buffer.Width := BoxSize.cx;
        Buffer.Height := BoxSize.cy;
        Buffer.PixelFormat := pf32bit;
        try
          Bitmap_ClearAlpha(Buffer, 0);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          R := Rect(0, 0, ScaleBoxSize.cx, ScaleBoxSize.cy);
          RectVCenter(R, ARect);
          Inc(R.Top);
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas, R.Left, R.Top, 255, AScaleFactor);
        finally
          Buffer.Free;
        end;
      end;
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end
  else
  begin
    Size := Round(13 * AScaleFactor);
    R := Rect(0, 0, Size, Size);
    RectVCenter(R, ARect);
    case ACtrlState of
      scsNormal, scsHot:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONRADIO);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_CHECKED);
        end;
      scsPressed:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_PUSHED);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_CHECKED or DFCS_PUSHED);
        end;
      scsDisabled:
        case ACheckBoxState of
          cbUnChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_INACTIVE);
          cbChecked: DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_INACTIVE);
        end;
    end;
  end;
end;

procedure DrawSelectionWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
var
  B: TBitmap;
  R: TRect;
  Details:  TThemedElementDetails;
begin
  if ARect.Width * ARect.Height <= 0 then Exit;

  B := TBitmap.Create;
  B.Width := ARect.Width;
  B.Height := ARect.Height;
  B.PixelFormat := pf32bit;
  R := Rect(0, 0, B.Width, B.Height);
  try
    if (StyleServices.Enabled and not IsWindowsXP) or IsCustomStyle then
    begin
      Bitmap_ClearAlpha(B, 0);
      B.AlphaFormat := afPremultiplied;
      if not IsCustomStyle then
        Details := StyleServices.GetElementDetails(tmcGridCellBackgroundSelected)
      else
        Details :=  StyleServices.GetElementDetails(tgCellSelected);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);
    end
    else
    begin
      with B.Canvas do
      begin
        Brush.Color := clHighLight;
        Brush.Style := bsSolid;
        FillRect(R);
      end;
      Bitmap_ClearAlpha(B, 255);
    end;
    Bitmap_DrawAlpha_XY(B, ACanvas, ARect.Left, ARect.Top, AAlpha);
  finally
    B.Free;
  end;
end;

procedure FillRectWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
var
  B: TBitmap;
  R: TRect;
  C: TColor;
begin
  if ARect.Width * ARect.Height <= 0 then Exit;

  B := TBitmap.Create;
  B.Width := ARect.Width;
  B.Height := ARect.Height;
  B.PixelFormat := pf32bit;
  R := Rect(0, 0, B.Width, B.Height);
  C := ACanvas.Brush.Color;
  try
    with B.Canvas do
    begin
      Brush.Color := ColorToRGB(C);
      Brush.Style := bsSolid;
      FillRect(R);
    end;
    Bitmap_ClearAlpha(B, 255);
    Bitmap_DrawAlpha_XY(B, ACanvas, ARect.Left, ARect.Top, AAlpha);
  finally
    B.Free;
  end;
end;

procedure DrawSelection(ACanvas: TCanvas; ARect: TRect; AFocused, AFocusedSelection: Boolean);
var
  Details:  TThemedElementDetails;
  SaveIndex: Integer;
begin
  if ARect.Width * ARect.Height <= 0 then Exit;

  if IsCustomStyle and not AFocused and not AFocusedSelection then
  begin
    DrawSelectionWithAlpha(ACanvas, ARect, 175);
  end
  else
  if StyleServices.Enabled and not (IsWindowsXP and not IsCustomStyle) then
  begin
    if not IsCustomStyle then
    begin
      if not AFocused and not AFocusedSelection then
        Details := StyleServices.GetElementDetails(tmcGridCellBackgroundSelectedNotFocused)
      else
        Details := StyleServices.GetElementDetails(tmcGridCellBackgroundSelected);
    end
    else
      Details := StyleServices.GetElementDetails(tgCellSelected);
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end
  else
  with ACanvas do
  begin
    Brush.Color := clHighLight;
    Brush.Style := bsSolid;
    if not AFocused and not AFocusedSelection then
      FillRectWithAlpha(ACanvas, ARect, 200)
    else
      FillRect(ARect);
  end;
end;

procedure DrawCategoryHeader(ACanvas: TCanvas; ARect: TRect);
var
  Details:  TThemedElementDetails;
  SaveIndex: Integer;
begin
  if IsCustomStyle then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    Details := StyleServices.GetElementDetails(tcpThemedHeader);
    try
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
   end;
  end
  else
  with ACanvas do
  begin
    Brush.Color := DarkerColor(clBtnFace, 10);
    Brush.Style := bsSolid;
    FillRect(ARect);
  end;
end;

procedure DrawHeaderSection(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  Details:  TThemedElementDetails;
  SaveIndex: Integer;
begin
  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal, scsDisabled:
         Details := StyleServices.GetElementDetails(tgFixedCellNormal);
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(tgFixedCellHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tgFixedCellPressed);
    end;
   SaveIndex := SaveDC(ACanvas.Handle);
   try
     StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    finally
     RestoreDC(ACanvas.Handle, SaveIndex);
   end;
  end
  else
  with ACanvas do
  begin
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    FillRect(ARect);
    if ACtrlState = scsPressed then
      Frm3D(ACanvas, ARect, clBtnShadow, clBtnHighlight)
    else
      Frm3D(ACanvas, ARect, clBtnHighlight, clBtnShadow);
  end;
end;

procedure DrawBorder(ACanvas: TCanvas; ARect: TRect);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(teEditTextNormal);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  begin
    Frm3D(ACanvas, ARect, clBtnShadow, clBtnHighLight);
    Frm3D(ACanvas, ARect, cl3DDkShadow, cl3DLight);
  end;
end;

procedure DrawEditBorder(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  Details:  TThemedElementDetails;
  C: TColor;
  R: TRect;
begin
  if StyleServices.Enabled then
  begin
    R := ARect;
    C := GetStyleColor(clBtnFace);
    Frm3D(ACanvas, R, C, C);
    Frm3D(ACanvas, R, C, C);
    case ACtrlState of
      scsNormal: Details := StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
      scsHot: Details := StyleServices.GetElementDetails(teEditBorderNoScrollHot);
      scsFocused:
       if IsCustomStyle then
         Details := StyleServices.GetElementDetails(teEditBorderNoScrollHot)
       else
         Details := StyleServices.GetElementDetails(teEditBorderNoScrollFocused);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  begin
    Frm3D(ACanvas, ARect, clBtnShadow, clBtnHighLight);
    Frm3D(ACanvas, ARect, cl3DDkShadow, cl3DLight);
  end;
end;

procedure DrawSingleBorder(ACanvas: TCanvas; ARect: TRect);
var
  C: TColor;
begin
  if IsCustomStyle then
    C := StyleServices.GetSystemColor(clBtnShadow)
  else
    C:= clBtnShadow;
  Frm3D(ACanvas, ARect, C, C);
  if IsCustomStyle then
    C := StyleServices.GetSystemColor(clWindow)
  else
    C:= clWindow;
  Frm3D(ACanvas, ARect, C, C);
end;

procedure DrawDropDownButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
  ANewStyle: Boolean; ARightToLeft: Boolean; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if ANewStyle and not ARightToLeft and
    not IsCustomStyle and StyleServices.Enabled and CheckWin32Version(6, 0) then
  begin
    Inc(ARect.Right);
    Inc(ARect.Bottom);
    Dec(ARect.Top);
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(tcDropDownButtonRightNormal);
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(tcDropDownButtonRightHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tcDropDownButtonRightPressed);
      scsDisabled:
         Details := StyleServices.GetElementDetails(tcDropDownButtonRightDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP}, nil, FDPI{$ENDIF});
  end
  else
  if ANewStyle and ARightToLeft and
     not IsCustomStyle and StyleServices.Enabled and CheckWin32Version(6, 0) then
  begin
    Dec(ARect.Left);
    Inc(ARect.Bottom);
    Dec(ARect.Top);
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(tcDropDownButtonLeftNormal);
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(tcDropDownButtonLeftHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tcDropDownButtonLeftPressed);
      scsDisabled:
         Details := StyleServices.GetElementDetails(tcDropDownButtonLeftDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP}, nil, FDPI{$ENDIF});
  end
  else
  if StyleServices.Enabled then
  begin
    case ACtrlState of
      scsNormal:
         Details := StyleServices.GetElementDetails(tcDropDownButtonNormal);
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(tcDropDownButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tcDropDownButtonPressed);
      scsDisabled:
         Details := StyleServices.GetElementDetails(tcDropDownButtonDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP}, nil, FDPI{$ENDIF});
  end
  else
  begin
   if ACtrlState = scsPressed then
      DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
    else
      DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
    DrawArrowImage(ACanvas, ARect, clBtnText, 4);
  end;
end;

procedure DrawGroupBoxFrame(ACanvas: TCanvas; ARect: TRect);
var
  Details:  TThemedElementDetails;
  R: TRect;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tbGroupBoxNormal);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  begin
    R := ARect;
    Inc(R.Left);
    Inc(R.Top);
    Frm3D(ACanvas, R, clBtnHighLight, clBtnHighLight);
    R := ARect;
    Dec(R.Right);
    Dec(R.Bottom);
    Frm3D(ACanvas, R, clBtnShadow, clBtnShadow);
  end;
end;

procedure DrawTrackBarTrack(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    if AVertical then
    begin
      Details := StyleServices.GetElementDetails(ttbTrackVert);
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    end
    else
    begin
      Details := StyleServices.GetElementDetails(ttbTrack);
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    end;
  end
  else
  begin
    Frm3D(ACanvas, ARect, clBtnShadow, clBtnHighLight);
  end;
end;

procedure DrawTrackBarThumb(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details: TThemedElementDetails;
  R: TRect;
  Buffer: TBitmap;
  IX, IY: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(ttbThumbNormal);
    if AVertical then
    begin
      case ACtrlState of
        scsNormal: Details := StyleServices.GetElementDetails(ttbThumbVertNormal);
        scsHot: Details := StyleServices.GetElementDetails(ttbThumbVertHot);
        scsPressed: Details := StyleServices.GetElementDetails(ttbThumbVertPressed);
        scsDisabled: Details := StyleServices.GetElementDetails(ttbThumbVertDisabled);
      end;
    end
    else
      case ACtrlState of
        scsNormal: Details := StyleServices.GetElementDetails(ttbThumbNormal);
        scsHot: Details := StyleServices.GetElementDetails(ttbThumbHot);
        scsPressed: Details := StyleServices.GetElementDetails(ttbThumbPressed);
        scsDisabled: Details := StyleServices.GetElementDetails(ttbThumbDisabled);
      end;
    if IsCustomStyle and (AScaleFactor >= 1.25) and SC_SCALESTYLES
    then
      begin
        R := Rect(0, 0, 23, 23);
        Buffer := TBitmap.Create;
        Buffer.PixelFormat := pf32bit;
        Buffer.Width := R.Width;
        Buffer.Height := R.Height;
        Bitmap_ClearAlpha(Buffer, 0);
        try
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details,
            Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
          IX := ARect.Left + ARect.Width div 2 - Round(Buffer.Width * AScaleFactor) div 2;
          IY := ARect.Top + ARect.Height div 2 - Round(Buffer.Height * AScaleFactor) div 2;
          Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas, IX, IY, 255, AScaleFactor);
        finally
          Buffer.Free;
        end;
      end
    else
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  begin
    DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
  end;
end;

procedure DrawProgressBarBorder(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean);
var
  Details: TThemedElementDetails;
  SaveIndex: Integer;
  FOldPen: HPEN;
begin
  SaveIndex := SaveDC(ACanvas.Handle);
  if StyleServices.Enabled then
  begin
    if AVertical then
      Details := StyleServices.GetElementDetails(tpBarVert)
    else
      Details := StyleServices.GetElementDetails(tpBar);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(clBtnShadow));
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Rectangle(ARect);
    SelectObject(ACanvas.Handle, FOldPen);
  end;
  RestoreDC(ACanvas.Handle, SaveIndex);
end;

procedure DrawProgressBarChunk(ACanvas: TCanvas; ARect: TRect; AVertical: Boolean);
var
  Details: TThemedElementDetails;
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Handle);
  if StyleServices.Enabled then
  begin
    if AVertical then
      Details := StyleServices.GetElementDetails(tpChunkVert)
    else
      Details := StyleServices.GetElementDetails(tpChunk);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Color := clGreen;
    Brush.Style := bsSolid;
    FillRect(ARect);
  end;
  RestoreDC(ACanvas.Handle, SaveIndex);
end;

procedure DrawProgressBar(ACanvas: TCanvas; ARect: TRect; AMin, AMax, AValue: Integer);
var
  PW: Integer;
  Kf: Single;
  R: TRect;
  Details: TThemedElementDetails;
  SaveIndex: Integer;
  FOldPen: HPEN;
begin
  SaveIndex := SaveDC(ACanvas.Handle);
  if (AMax - AMin <> 0) and (AValue >= AMin) then
    kf := (AValue - AMin) / (AMax - AMin)
  else
    kf := 0;
  PW := Round((ARect.Width - 2) * kf);
  R := ARect;
  InflateRect(R, -1, -1);
  R.Right := R.Left + PW;
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tpBar);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    Details := StyleServices.GetElementDetails(tpChunk);
    if R.Width > 0 then
      StyleServices.DrawElement(ACanvas.Handle, Details, R);
  end
  else
  with ACanvas do
  begin
    FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(ACanvas.Handle, ColorToRGB(clBtnShadow));
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Rectangle(ARect);
    Brush.Color := clGreen;
    FillRect(R);
    SelectObject(ACanvas.Handle, FOldPen);
  end;
  RestoreDC(ACanvas.Handle, SaveIndex);
end;

procedure DrawToolBar(ACanvas: TCanvas; ARect: TRect);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details.State := 0;
    Details.Part := 0;
    Details.Element := teToolBar;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(ARect);
  end;
end;

procedure DrawSimpleTab(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
begin
  if ACtrlState = scsPressed then
    ACtrlState := scsFocused;
  if ACtrlState = scsFocused then
    Inc(ARect.Bottom, 2);

  if (ACtrlState <> scsFocused) and not StyleServices.Enabled
  then
  begin
    Inc(ARect.Top, 2);
  end;

  if ACtrlState = scsDisabled then
    DrawTabWithAlpha(ACanvas, ARect, 150)
  else
    DrawTab(ACanvas, ARect, ACtrlState, tpTop, False, False);
end;

procedure DrawTab(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;
  ATabPosition: TTabPosition; AFirst, ALast: Boolean);
var
  Details:  TThemedElementDetails;
  DrawCtrlState: TThemedTab;
  Buffer: TBitmap;
  R: TRect;
  SaveIndex: Integer;
begin
  if StyleServices.Enabled then
  begin
    DrawCtrlState := ttTabDontCare;
    case ATabPosition of
    tpTop:
      begin
        if ACtrlState = scsFocused then
        begin
          if not IsCustomStyle and AFirst then
            DrawCtrlState := ttTabItemLeftEdgeSelected
          else
          if not IsCustomStyle and ALast then
            DrawCtrlState := ttTabItemRightEdgeSelected
          else
            DrawCtrlState := ttTabItemSelected;
        end
        else
        if (ACtrlState <> scsFocused) and IsWindows10 and AFirst and not IsCustomStyle then
        begin
          case ACtrlState of
            scsNormal:
              DrawCtrlState := ttTabItemLeftEdgeNormal;
            scsHot:
              DrawCtrlState := ttTabItemLeftEdgeHot;
            scsDisabled:
              DrawCtrlState := ttTabItemLeftEdgeDisabled;
          end;
        end
        else if ACtrlState = scsHot then
          DrawCtrlState := ttTabItemHot
        else
          DrawCtrlState := ttTabItemNormal;
      end;
    tpLeft:
      begin
        if ACtrlState = scsFocused then
          DrawCtrlState := ttTabItemLeftEdgeSelected
        else if ACtrlState = scsHot then
          DrawCtrlState := ttTabItemLeftEdgeHot
        else
          DrawCtrlState := ttTabItemLeftEdgeNormal;
      end;
    tpBottom:
      begin
        if ACtrlState = scsFocused then
          DrawCtrlState := ttTabItemBothEdgeSelected
        else if ACtrlState = scsHot then
          DrawCtrlState := ttTabItemBothEdgeHot
        else
          DrawCtrlState := ttTabItemBothEdgeNormal;
      end;
    tpRight:
      begin
        if ACtrlState = scsFocused then
          DrawCtrlState := ttTabItemRightEdgeSelected
        else if ACtrlState = scsHot then
          DrawCtrlState := ttTabItemRightEdgeHot
        else
          DrawCtrlState := ttTabItemRightEdgeNormal;
      end;
    end;
    Details := StyleServices.GetElementDetails(DrawCtrlState);
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
   end;
  end
  else
  begin
    Buffer := TBitmap.Create;
    Buffer.SetSize(ARect.Width, ARect.Height);
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    case ATabPosition of
      tpTop:
        with Buffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          FillRect(R);
          Pen.Color := clBtnHighLight;
          MoveTo(R.Left, R.Bottom);
          LineTo(R.Left, R.Top);
          LineTo(R.Right - 1, R.Top);
          Pen.Color := cl3DDkShadow;
          LineTo(R.Right - 1, R.Bottom);
          Pen.Color := cl3DLight;
          InflateRect(R, -1, -1);
          Pen.Color := cl3DLight;
          MoveTo(R.Left, R.Bottom);
          LineTo(R.Left, R.Top);
          LineTo(R.Right - 1, R.Top);
          Pen.Color := clBtnShadow;
          LineTo(R.Right - 1, R.Bottom);
        end;
      tpBottom:
        with Buffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          FillRect(R);
          Pen.Color := clBtnHighLight;
          MoveTo(R.Left, R.Top);
          LineTo(R.Left, R.Bottom - 1);
          Pen.Color := cl3DDkShadow;
          LineTo(R.Right - 1, R.Bottom - 1);
          if not ALast then
            LineTo(R.Right - 1, R.Top)
          else
            LineTo(R.Right - 1, R.Top - 1);
          InflateRect(R, -1, -1);
          Pen.Color := cl3DLight;
          MoveTo(R.Left, R.Top - 1);
          LineTo(R.Left, R.Bottom - 1);
          Pen.Color := clBtnShadow;
          LineTo(R.Right - 1, R.Bottom - 1);
          LineTo(R.Right - 1, R.Top - 1);
        end;
      tpLeft:
        with Buffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          FillRect(R);
          Pen.Color := clBtnHighLight;
          MoveTo(R.Right, R.Top);
          LineTo(R.Left, R.Top);
          LineTo(R.Left, R.Bottom - 1);
          Pen.Color := cl3DDkShadow;
          LineTo(R.Right, R.Bottom - 1);
          InflateRect(R, -1, -1);
          Pen.Color := cl3DLight;
          MoveTo(R.Right, R.Top);
          LineTo(R.Left, R.Top);
          LineTo(R.Left, R.Bottom - 1);
          Pen.Color := clBtnShadow;
          LineTo(R.Right, R.Bottom - 1);
        end;
      tpRight:
      with Buffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          FillRect(R);
          Pen.Color := clBtnHighLight;
          MoveTo(R.Left, R.Top);
          LineTo(R.Right - 1, R.Top);
          Pen.Color := cl3DDkShadow;
          LineTo(R.Right - 1, R.Bottom - 1);
          LineTo(R.Left, R.Bottom - 1);
          InflateRect(R, -1, -1);
          Pen.Color := cl3DLight;
          MoveTo(R.Left, R.Top);
          LineTo(R.Right - 1, R.Top);
          Pen.Color := clBtnShadow;
          LineTo(R.Right - 1, R.Bottom - 1);
          LineTo(R.Left, R.Bottom - 1);
        end;
    end;
    ACanvas.Draw(ARect.Left, ARect.Top, Buffer);
    Buffer.Free;
  end;
end;

procedure DrawTabFrame(ACanvas: TCanvas; ARect: TRect);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(ttPane);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(ARect);
    Frm3D(ACanvas, ARect, clBtnHighLight, cl3DDkShadow);
    Frm3D(ACanvas, ARect, cl3DLight, clBtnShadow);
  end;
end;

procedure DrawFormBackground(ACanvas: TCanvas; ARect: TRect);
var
  Details: TThemedElementDetails;
begin
  if IsCustomStyle then
  begin
    Details.Element := teWindow;
    Details.Part := 0;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(ARect);
  end;
end;

procedure DrawTabWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
var
  B: TBitmap;
  R: TRect;
  Details:  TThemedElementDetails;
begin
  B := TBitmap.Create;
  B.Width := ARect.Width;
  B.Height := ARect.Height;
  B.PixelFormat := pf32bit;
  R := Rect(0, 0, B.Width, B.Height);
  try
    if (StyleServices.Enabled and not IsWindowsXP) or IsCustomStyle then
    begin
      Bitmap_ClearAlpha(B, 0);
      B.AlphaFormat := afPremultiplied;
      Details :=  StyleServices.GetElementDetails(ttTabItemNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);
    end;
    Bitmap_DrawAlpha_XY(B, ACanvas, ARect.Left, ARect.Top, AAlpha);
  finally
    B.Free;
  end;
end;

procedure DrawPopupSelectionWithAlpha(ACanvas: TCanvas; ARect: TRect; AAlpha: Integer);
var
  B: TBitmap;
  R: TRect;
  Details:  TThemedElementDetails;
begin
  B := TBitmap.Create;
  B.Width := ARect.Width;
  B.Height := ARect.Height;
  B.PixelFormat := pf32bit;
  R := Rect(0, 0, B.Width, B.Height);
  try
    if (StyleServices.Enabled and not IsWindowsXP) or IsCustomStyle then
    begin
      Bitmap_ClearAlpha(B, 0);
      B.AlphaFormat := afPremultiplied;
      Details :=  StyleServices.GetElementDetails(tmPopupItemHot);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);
    end
    else
    begin
      with B.Canvas do
      begin
        Brush.Color := clHighLight;
        Brush.Style := bsSolid;
        FillRect(R);
      end;
      Bitmap_ClearAlpha(B, 255);
    end;
    Bitmap_DrawAlpha_XY(B, ACanvas, ARect.Left, ARect.Top, AAlpha);
  finally
    B.Free;
  end;
end;

procedure DrawStatusBar(ACanvas: TCanvas; ARect: TRect);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsStatusRoot);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(ARect);
  end;
end;

procedure DrawStatusPanel(ACanvas: TCanvas; ARect: TRect);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsPane);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(ARect);
    Frm3D(ACanvas, ARect, clBtnShadow, clBtnHighLight);
  end;
end;

procedure DrawPopupWindow(ACanvas: TCanvas; ARect: TRect);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tmPopupBorders);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(ARect);
    Frm3D(ACanvas, ARect, cl3DLight, cl3DDkShadow);
    Frm3D(ACanvas, ARect, clBtnHighLight, clBtnShadow);
  end;
end;

procedure DrawPopupItem(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tmPopupItemNormal);
    if ACtrlState = scsHot then
      Details := StyleServices.GetElementDetails(tmPopupItemHot)
    else
    if ACtrlState = scsDisabled then
       Details := StyleServices.GetElementDetails(tmPopupItemDisabled);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    if ACtrlState = scsHot then
      Brush.Color := clHighLight
    else
      Brush.Color := clBtnFace;
    FillRect(ARect);
  end;
end;

function GetEditBrushColor(ACtrlState: TscsCtrlState): TColor;
const
  ColorStates: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
begin
  if IsCustomStyle then
    Result := StyleServices.GetStyleColor(ColorStates[ACtrlState = scsNormal])
  else
    Result := clWindow;
end;

function GetActiveTextColor: TColor;
var
  C: TColor;
begin
  C := GetStyleColor(clHighLight);
  if not IsDarkStyle then
    Result := DarkerColor(C, 30)
  else
    Result := LighterColor(C, 30);
end;

function GetEditTextColor(ACtrlState: TscsCtrlState): TColor;
const
  FontColorStates: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
begin
  if IsCustomStyle then
    Result := StyleServices.GetStyleFontColor(FontColorStates[ACtrlState = scsNormal])
  else
  if ACtrlState = scsDisabled then
    Result := clGrayText
  else
    Result := clWindowText;
end;

function GetButtonTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  if ACtrlState = scsDisabled then
    FColor := clGrayText
  else
    FColor := clBtnText;
  if IsCustomStyle then
  begin
    Details := StyleServices.GetElementDetails(tbPushButtonNormal);
    case ACtrlState of
      scsHot:
         Details := StyleServices.GetElementDetails(tbPushButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tbPushButtonPressed);
      scsFocused:
         Details := StyleServices.GetElementDetails(tbPushButtonDefaulted);
      scsDisabled:
         Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
    end;
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnFace;
  end;
  Result := FColor;
end;

function GetToolButtonTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  if ACtrlState = scsDisabled then
    FColor := clGrayText
  else
    FColor := clBtnText;
  if IsCustomStyle then
  begin
   Details := StyleServices.GetElementDetails(ttbButtonNormal);
   case ACtrlState of
      scsHot, scsFocused:
         Details := StyleServices.GetElementDetails(ttbButtonHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(ttbButtonPressed);
      scsDisabled:
         Details := StyleServices.GetElementDetails(ttbButtonDisabled);
    end;
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnFace;
  end;
  Result := FColor;
end;

function GetCheckBoxTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  if ACtrlState = scsDisabled then
    FColor := clGrayText
  else
    FColor := clBtnText;
  if IsCustomStyle then
  begin
    Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    case ACtrlState of
      scsHot, scsFocused:
        Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
      scsPressed:
        Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
      scsDisabled:
        Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
    end;
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnText;
  end;
  Result := FColor;
end;

function GetRadioButtonTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
   if ACtrlState = scsDisabled then
    FColor := clGrayText
  else
    FColor := clBtnText;
  if IsCustomStyle then
  begin
    Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
    case ACtrlState of
      scsHot:
        Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
      scsPressed:
        Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
      scsDisabled:
        Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
     end;
     if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
       FColor := clBtnFace;
  end;
  Result := FColor;
end;

function GetGroupBoxTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  if ACtrlState = scsDisabled then
    FColor := clGrayText
  else
    FColor := clBtnText;
  if IsCustomStyle then
  begin
    if ACtrlState = scsDisabled then
      Details := StyleServices.GetElementDetails(tbGroupBoxDisabled)
    else
      Details := StyleServices.GetElementDetails(tbGroupBoxNormal);
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnFace;
  end;
  Result := FColor;
end;

function GetCategoryHeaderTextColor: TColor;
var
  Details:  TThemedElementDetails;
  FColor: TColor;
begin
  Result := clBtnText;
  if IsCustomStyle then
  begin
    Details := StyleServices.GetElementDetails(tcpThemedHeader);
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
       FColor := clBtnFace;
    Result := FColor;
  end;
end;

function GetHeaderTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  if ACtrlState = scsDisabled then
    FColor := clGrayText
  else
    FColor := clBtnText;
  if IsCustomStyle then
  begin
    Details := StyleServices.GetElementDetails(tgFixedCellNormal);
    case ACtrlState of
      scsDisabled:
        Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
      scsHot:
         Details := StyleServices.GetElementDetails(tgFixedCellHot);
      scsPressed:
         Details := StyleServices.GetElementDetails(tgFixedCellPressed);
    end;
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnFace;
  end;
  Result := FColor;
end;

function GetStyleColor(AColor: TColor): TColor;
begin
  if IsCustomStyle then
    Result := StyleServices.GetSystemColor(AColor)
  else
    Result := AColor;
end;

function GetSelectionTextColor: TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  FColor := clHighLightText;
  if StyleServices.Enabled and not (IsWindowsXP and not IsCustomStyle) then
  begin
    Details := StyleServices.GetElementDetails(tgCellSelected);
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clHighLightText;
  end;
  Result := FColor;
end;

function GetSimpleTabTextColor(ACtrlState: TscsCtrlState): TColor;
begin
  if ACtrlState = scsPressed then
     ACtrlState := scsFocused;
  Result := GetTabTextColor(ACtrlState);
end;

function GetTabTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  if ACtrlState = scsDisabled then
    FColor := clGrayText
  else
    FColor := clBtnText;
  if IsCustomStyle then
  begin
    Details := StyleServices.GetElementDetails(ttTabItemNormal);
    case ACtrlState of
      scsFocused:
        Details := StyleServices.GetElementDetails(ttTabItemSelected);
      scsHot:
        Details := StyleServices.GetElementDetails(ttTabItemHot);
      scsDisabled:
        Details := StyleServices.GetElementDetails(ttTabItemDisabled);
    end;
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnFace;
  end;
  Result := FColor;
end;

function GetCaptionTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  if IsCustomStyle then
  begin
    if ACtrlState = scsDisabled then
      Details := StyleServices.GetElementDetails(twCaptionInActive)
    else
      Details := StyleServices.GetElementDetails(twCaptionActive);
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnText;
    Result := FColor;
  end
  else
  begin
    if ACtrlState = scsDisabled then
      Result := clInActiveCaptionText
    else
      Result := clCaptionText;
  end;
end;

function GetPopupItemTextColor(ACtrlState: TscsCtrlState): TColor;
var
  Details: TThemedElementDetails;
  FColor: TColor;
begin
  FColor := clBtnText;
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tmPopupItemNormal);
    if ACtrlState = scsHot then
      Details := StyleServices.GetElementDetails(tmPopupItemHot)
    else
    if ACtrlState = scsDisabled then
       Details := StyleServices.GetElementDetails(tmPopupItemDisabled);
    if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
      FColor := clBtnFace;
  end
  else
  begin
    if ACtrlState = scsHot then
      FColor := clHighLightText
    else
    if ACtrlState = scsDisabled then
      FColor := clGrayText;
  end;
  Result := FColor;
end;

procedure CalcLCoord(ALayout: TButtonLayout; ARect: TRect; gw, gh: Integer; tw, th: Integer;
    ASpacing, AMargin: Integer; var tx, ty, gx, gy: Integer);
var
  H, W, H1, W1: Integer;
begin
 H := ARect.Top + ARect.Height div 2;
 W := ARect.Left + ARect.Width div 2;
 if gw = 0 then ASpacing := 0;
 if AMargin = -1
 then
   begin
     W1 := (tw + gw + ASpacing) div 2;
     H1 := (th + gh + ASpacing) div 2;
     case ALayout of
       blGlyphRight:
         begin
           tx := W - W1;
           ty := H - th div 2;
           gx := W + W1 - gw;
           gy := H - gh div 2;
         end;
      blGlyphLeft:
         begin
           gx := W - W1;
           gy := H - gh div 2;
           tx := W + W1 - tw;
           ty := H - th div 2;
         end;
      blGlyphTop:
         begin
           tx := W - tw div 2;
           ty := H + H1 - th;
           gx := W - gw div 2;
           gy := H - H1;
        end;
     blGlyphBottom:
        begin
          gx := W - gw div 2;
          gy := H + H1 - gh;
          tx := W - tw div 2;
          ty := H - H1;
       end;
     end;
   end
 else
   begin
     case ALayout of
       blGlyphRight:
         begin
           gy := H - gh div 2;
           gx := ARect.Right - gw - AMargin;
           if ASpacing < -100 then
           begin
             tx := gx - tw;
             tx := tx - (ARect.Width - gw) div 2 + tw div 2;
           end
           else
             tx := gx - ASpacing - tw;
           ty := H - th div 2;
         end;
       blGlyphLeft:
         begin
           gy := H - gh div 2;
           gx := ARect.Left + AMargin;
           if ASpacing < -100 then
           begin
             tx := gx + gw;
             tx := tx + (ARect.Right - tx) div 2 - tw div 2;
           end
           else
             tx := gx + gw + ASpacing;
           ty := H - th div 2;
         end;
       blGlyphTop:
          begin
            gy := ARect.Top +  AMargin;
            gx := W - gw div 2;
            if ASpacing < -100 then
            begin
              ty := gy + gh;
              ty := ty + (ARect.Height - ty) div 2 - th div 2;
            end
            else
              ty := gy + gh + ASpacing;

            tx := W - tw div 2;
          end;
      blGlyphBottom:
          begin
            gy := ARect.Bottom - gh - AMargin;
            gx := W - gw div 2;
            if ASpacing < -100 then
            begin
              ty := gy - th;
              ty := ty - (ARect.Height - gh) div 2 + th div 2;
            end
            else
              ty := gy - ASpacing - th;
            tx := W - tw div 2;
         end;
       end;
    end;
end;

procedure CalcLCoord2(ALayout: TButtonLayout; ARect: TRect; gw, gh: Integer;
    var tw, th: Integer;
    ASpacing, AMargin: Integer; var tx, ty, gx, gy: Integer);
var
  H, W, H1, W1: Integer;
begin
 H := ARect.Top + ARect.Height div 2;
 W := ARect.Left + ARect.Width div 2;
 if gw = 0 then ASpacing := 0;
 if AMargin = -1
 then
   begin
     W1 := (tw + gw + ASpacing) div 2;
     H1 := (th + gh + ASpacing) div 2;
     case ALayout of
       blGlyphRight:
         begin
           tx := W - W1;
           ty := H - th div 2;
           gx := W + W1 - gw;
           gy := H - gh div 2;
           if gw > 0 then
           begin
             if gw + gx > ARect.Right then
             begin
               gx := ARect.Right - gw;
               tx := gx - ASpacing - tw;
               if tx < ARect.Left then
               begin
                 tx := ARect.Left;
                 tw := ARect.Width - gw - ASpacing;
               end;
             end;
             if ty < ARect.Top then
             begin
               ty := ARect.Top;
               th := ARect.Height;
             end;
           end
           else
           if (tw > 0) and (tx < ARect.Left) then
           begin
             tx := ARect.Left;
             tw := ARect.Width;
           end;
         end;
      blGlyphLeft:
         begin
           gx := W - W1;
           gy := H - gh div 2;
           tx := W + W1 - tw;
           ty := H - th div 2;
           if gw > 0 then
           begin
             if gx < ARect.Left then
             begin
               gx := ARect.Left;
               tx := gx + gw + ASpacing;
               tw := ARect.Width - gw - ASpacing;
             end;
             if ty < ARect.Top then
             begin
               ty := ARect.Top;
               th := ARect.Height;
             end;
           end
           else
           if (tw > 0) and (tx + tw > ARect.Right) then
           begin
             tx := ARect.Left;
             tw := ARect.Width;
           end;
         end;
      blGlyphTop:
         begin
           tx := W - tw div 2;
           ty := H + H1 - th;
           gx := W - gw div 2;
           gy := H - H1;
           if tx < ARect.Left then
           begin
             tx := ARect.Left;
             tw := ARect.Width;
           end;
           if gw > 0 then
           begin
             if gy < ARect.Top then
             begin
               gy := ARect.Top;
               ty := gy + gh + ASpacing;
               th := ARect.Height - gh - ASpacing;
             end;
           end
           else
           if (th > 0) and (ty + th > ARect.Bottom) then
           begin
             ty := ARect.Top;
             tw := ARect.Height;
           end;
        end;
     blGlyphBottom:
        begin
          gx := W - gw div 2;
          gy := H + H1 - gh;
          tx := W - tw div 2;
          ty := H - H1;
          if tx < ARect.Left then
          begin
            tx := ARect.Left;
            tw := ARect.Width;
          end;
          if gw > 0 then
           begin
             if gh + gy > ARect.Bottom then
             begin
               gy := ARect.Bottom - gw;
               ty := gy - ASpacing - th;
               if ty < ARect.Top then
               begin
                 ty := ARect.Top;
                 th := ARect.Height - gh - ASpacing;
               end;
             end;
           end
           else
           if (th > 0) and (ty < ARect.Top) then
           begin
             ty := ARect.Top;
             th := ARect.Height;
           end;
       end;
     end;
   end
 else
   begin
     case ALayout of
       blGlyphRight:
         begin
           gy := H - gh div 2;
           gx := ARect.Right - gw - AMargin;
           tx := ARect.Left;
           ty := ARect.Top;
           th := ARect.Height;
           tw := ARect.Width - gw - ASpacing - AMargin;
         end;
       blGlyphLeft:
         begin
           gy := H - gh div 2;
           gx := ARect.Left + AMargin;
           tx := gx + gw + ASpacing;
           ty := ARect.Top;
           tw := ARect.Width - gw - ASpacing - AMargin;
           th := ARect.Height;
         end;
       blGlyphTop:
          begin
            gy := ARect.Top +  AMargin;
            gx := W - gw div 2;
            ty := gy + gh + ASpacing;
            tx := ARect.Left;
            th := ARect.Height - gh - ASpacing - AMargin;
            tw := ARect.Width;
          end;
      blGlyphBottom:
          begin
            gy := ARect.Bottom - gh - AMargin;
            gx := W - gw div 2;
            ty := ARect.Top;
            tx := ARect.Left;
            th := ARect.Height - gh - ASpacing - AMargin;
            tw := ARect.Width;
          end;
       end;
    end;
end;

procedure DrawImageAndTextLeft(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
    AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
    AEnabled: Boolean; ARightToLeft: Boolean; ANoPrefix: Boolean);
begin
  if AImageIndex = -1 then
   DrawImageAndText(ACanvas, ARect, AMargin, ASpacing, blGlyphTop,
    AText, AImageIndex, AImageList, AEnabled,
    False, clNone, False, ARightToLeft, ANoPrefix)
  else
  DrawImageAndText(ACanvas, ARect, AMargin, ASpacing, blGlyphLeft,
    AText, AImageIndex, AImageList, AEnabled,
    False, clNone, False, ARightToLeft, ANoPrefix);
end;

procedure DrawImageAndTextLeft2(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
    AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
    AEnabled: Boolean; ARightToLeft: Boolean; ANoPrefix: Boolean);
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY: Integer;
  TR: TRect;
  Flags: Longint;
begin
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        Dec(TR.Right, gw + ASpacing);
        if AMargin > 0 then Dec(TR.Right, AMargin);
        Flags := DT_EXPANDTABS or DT_CALCRECT;
        Flags := Flags or DT_NOPREFIX;
        DrawText(Handle, PChar(AText), Length(AText), TR, Flags);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(blGlyphLeft, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      Flags := DT_EXPANDTABS or DT_VCENTER or DT_CENTER or DT_NOCLIP;
      if ANoPrefix then
         Flags := Flags or DT_NOPREFIX;
      OffsetRect(TR, 0, -1);
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), TR,
        scDrawTextBidiModeFlags(Flags, ARightToLeft));
    end;
  if gw <> 0
  then
    begin
      if AImageList.ShareImages then
        ImageList_DrawEx(AImageList.Handle, AImageIndex, ACanvas.Handle, GX, GY, AImageList.Width, AImageList.Height,
         CLR_NONE, CLR_NONE, ILD_NORMAL)
      else
      if AEnabled then
         AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
      else
      if AImageList is TscCustomImageList then
        AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
      else
         DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
    end;
end;

procedure DrawImageAndText(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
          ALayout: TButtonLayout;
          AText: String;
          AImageIndex: Integer; AImageList: TCustomImageList;
          AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor; ADrawFocusRect: Boolean; ARightToLeft: Boolean;
          ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY, I: Integer;
  TR: TRect;
  Flags: Longint;
begin
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        Flags := DT_EXPANDTABS or DT_CALCRECT;
        if ANoPrefix then
          Flags := Flags or DT_NOPREFIX;
        if AWordWrap then
          Flags := Flags or DT_WORDBREAK;
        DrawText(Handle, PChar(AText), Length(AText), TR, Flags);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      Flags := DT_EXPANDTABS or DT_VCENTER or DT_CENTER or DT_NOCLIP;
      if ANoPrefix then
         Flags := Flags or DT_NOPREFIX;
      if AWordWrap then
         Flags := Flags or DT_WORDBREAK;
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), TR,
        scDrawTextBidiModeFlags(Flags, ARightToLeft));
      if ADrawFocusRect then
        scDrawFocusRect(ACanvas, Rect(TR.Left - 2, TR.Top - 2, TR.Right + 2, TR.Bottom + 2), AScaleDrawFactor);
    end;
  if gw <> 0
  then
    begin
      if AImageList.ShareImages then
        ImageList_DrawEx(AImageList.Handle, AImageIndex, ACanvas.Handle, GX, GY, AImageList.Width, AImageList.Height,
         CLR_NONE, CLR_NONE, ILD_NORMAL)
      else
      if AEnabled then
         AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
      else
      if AImageList is TscCustomImageList then
        AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
      else
        DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
      if ADrawColorMarker
      then
        with ACanvas do
        begin
          Pen.Color := AColorMarkerValue;
          for I := 0 to Trunc(2 * AScaleDrawFactor) do
          begin
            MoveTo(GX, GY + AImageList.Height + I);
            LineTo(GX + AImageList.Width, GY + AImageList.Height + I);
          end;
        end;
      if ADrawFocusRect and (AText = '') then
        scDrawFocusRect(ACanvas, Rect(GX - 2, GY - 2, GX + AImageList.Width + 2, GY + AImageList.Height + 2),
         AScaleDrawFactor);
    end
  else
    if ADrawColorMarker
    then
      with ACanvas do
      begin
        if AText <> ''
        then
          begin
            Pen.Color := AColorMarkerValue;
            for I := 0 to Trunc(2 * AScaleDrawFactor) do
            begin
              MoveTo(TR.Left, TR.Bottom + I);
              LineTo(TR.Right, TR.Bottom + I);
            end;
          end
        else
          begin
            Brush.Color := AColorMarkerValue;
            Brush.Style := bsSolid;
            FillRect(Rect(ARect.Left + 4, ARect.Top + 4, ARect.Right - 2, ARect.Bottom - 4));
          end;
     end;
end;

procedure DrawImageAndText2(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
          ALayout: TButtonLayout;
          AText: String;
          AImageIndex: Integer; AImageList: TCustomImageList;
          AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor; ADrawFocusRect: Boolean;
          ARightToLeft: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY, I: Integer;
  TR: TRect;
  Flags: Longint;
begin
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        Flags := DT_EXPANDTABS or DT_CALCRECT;
        if ANoPrefix then
          Flags := Flags or DT_NOPREFIX;
        if AWordWrap then
          Flags := Flags or DT_WORDBREAK;
        DrawText(Handle, PChar(AText), Length(AText), TR, Flags);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      Flags := DT_EXPANDTABS or DT_VCENTER or DT_NOCLIP;
      if ANoPrefix then
         Flags := Flags or DT_NOPREFIX;
      if AWordWrap then
         Flags := Flags or DT_WORDBREAK;
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), TR,
          scDrawTextBidiModeFlags(Flags, ARightToLeft));
      if ADrawFocusRect then
        scDrawFocusRect(ACanvas, Rect(TR.Left - 2, TR.Top - 2, TR.Right + 2, TR.Bottom + 2), AScaleDrawFactor);
    end;
  if gw <> 0
  then
    begin
      if AEnabled then
         AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
      else
      if AImageList is TscCustomImageList then
        AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
      else
        DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
      if ADrawColorMarker
      then
        with ACanvas do
        begin
          Pen.Color := AColorMarkerValue;
          for I := 0 to Trunc(2 * AScaleDrawFactor) do
          begin
            MoveTo(GX, GY + AImageList.Height + I);
            LineTo(GX + AImageList.Width, GY + AImageList.Height + I);
          end;
        end;
      if ADrawFocusRect and (AText = '') then
        scDrawFocusRect(ACanvas, Rect(GX - 2, GY - 2, GX + AImageList.Width + 2, GY + AImageList.Height + 2), AScaleDrawFactor);
    end
  else
    if ADrawColorMarker
    then
      with ACanvas do
      begin
        if AText <> ''
        then
          begin
            Pen.Color := AColorMarkerValue;
            for I := 0 to Trunc(2 * AScaleDrawFactor) do
            begin
              MoveTo(TR.Left, TR.Bottom + I);
              LineTo(TR.Right, TR.Bottom + I);
            end;
          end
        else
          begin
            Brush.Color := AColorMarkerValue;
            Brush.Style := bsSolid;
            FillRect(Rect(ARect.Left + 4, ARect.Top + 4, ARect.Right - 2, ARect.Bottom - 4));
          end;
     end;
end;

function DrawImageAndTextEllipses(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ARightToLeft: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1): Boolean;

var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY: Integer;
  TR: TRect;
begin
  Result := False;
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        if ANoPrefix then
          DrawText(Handle, PChar(AText), Length(AText), TR,
             DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT or DT_NOPREFIX)
        else
          DrawText(Handle, PChar(AText), Length(AText), TR,
             DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord2(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom) then
        Result := DrawTextMultiLine(ACanvas, AText, TR, False, ARightToLeft, ANoPrefix)
      else
        Result :=  DrawTextMultiLine(ACanvas, AText, TR, True, ARightToLeft, ANoPrefix);
    end;
  if gw <> 0
  then
    begin
      if AEnabled then
         AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
      else
      if AImageList is TscCustomImageList then
        AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
      else
         DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
    end;
end;

procedure DrawTabbedString(S: String; TW: TStrings; C: TCanvas; R: TRect; Offset: Integer);

function GetNum(AText: String): Integer;
const
  EditChars = '01234567890';
var
  i: Integer;
  S: String;
  IsNum: Boolean;
begin
  S := EditChars;
  Result := 0;
  if (AText = '') then Exit;
  IsNum := True;
  for i := 1 to Length(AText) do
  begin
    if Pos(AText[i], S) = 0
    then
      begin
        IsNum := False;
        Break;
      end;
  end;
  if IsNum then Result := StrToInt(AText) else Result := 0;
end;

var
  i, Max: Integer;
  TWValue: array[0..9] of Integer;
  X, Y: Integer;
begin
  for i := 0 to TW.Count - 1 do
  begin
    if i < 10 then TWValue[i] := GetNum(TW[i]);
  end;
  Max := TW.Count;
  if Max > 10 then Max := 10;
  X := R.Left + Offset + 2;
  Y := R.Top + RectHeight(R) div 2 - C.TextHeight(S) div 2;
  TabbedTextOut(C.Handle, X, Y, PChar(S), Length(S), Max, TWValue, 0);
end;

procedure AngleDrawText(Canvas: TCanvas; Angle: Integer; X, Y: Integer; const Text: string; AColor: TColor);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(Canvas.Handle);
  try
    Canvas.Font.Orientation := Angle;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    SetTextColor(Canvas.Handle, AColor);
    Canvas.TextOut(X, Y, Text);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

procedure scDrawText(ACanvas: TCanvas; AText: String; ARect: TRect; ARightToLeft: Boolean; ANoPrefix: Boolean);
begin
  if AText = '' then Exit;
  if ANoPrefix then
    DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,
      scDrawTextBidiModeFlags(DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_NOCLIP or DT_NOPREFIX, ARightToLeft))
  else
    DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,
     scDrawTextBidiModeFlags(DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_NOCLIP, ARightToLeft));
end;

procedure scDrawClipText(ACanvas: TCanvas; AText: String; ARect: TRect; ARightToLeft: Boolean; ANoPrefix: Boolean);
begin
  if AText = '' then Exit;
  if ANoPrefix then
    DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,
      scDrawTextBidiModeFlags(DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_NOPREFIX, ARightToLeft))
  else
    DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,
     scDrawTextBidiModeFlags(DT_VCENTER or DT_SINGLELINE or DT_LEFT, ARightToLeft));
end;

procedure DrawTextAlignmentNoPrefix(ACanvas: TCanvas; AText: String; ARect: TRect; AAlignment: TAlignment; ARightToLeft: Boolean);
begin
  if AText = '' then Exit;
  case AAlignment of
    taLeftJustify:
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,
        scDrawTextBidiModeFlags(DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_NOPREFIX or DT_NOCLIP, ARightToLeft));
    taRightJustify:
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,
        scDrawTextBidiModeFlags(DT_VCENTER or DT_SINGLELINE or DT_RIGHT or DT_NOPREFIX or DT_NOCLIP, ARightToLeft));
    taCenter:
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,
       scDrawTextBidiModeFlags(DT_VCENTER or DT_SINGLELINE or DT_CENTER or DT_NOPREFIX or DT_NOCLIP, ARightToLeft));
  end;
end;


procedure DrawTextAlignment(ACanvas: TCanvas; AText: String; ARect: TRect; AAlignment: TAlignment);
begin
  if AText = '' then Exit;
  case AAlignment of
    taLeftJustify:
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,  DT_VCENTER or DT_SINGLELINE or DT_LEFT);
    taRightJustify:
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,  DT_VCENTER or DT_SINGLELINE or DT_RIGHT);
    taCenter:
      DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect,  DT_VCENTER or DT_SINGLELINE or DT_CENTER);
  end;
end;

function PointInRect(ARect: TRect; APoint: TPoint): Boolean;
begin
  Result := (APoint.X >= ARect.Left) and (APoint.Y >= ARect.Top) and
            (APoint.X <= ARect.Right) and (APoint.Y <= ARect.Bottom);

end;

function RectInRect(AR1, AR2: TRect): Boolean;
begin
  Result := PtInRect(AR2, AR1.TopLeft) and PtInRect(AR2, AR1.BottomRight)
end;

function RectToRect(AR1, AR2: TRect): Boolean;
begin
  AR1.Left := AR2.Left + 1;
  AR1.Right := AR2.Right - 1;
  Result := PtInRect(AR2, AR1.TopLeft) or PtInRect(AR2, AR1.BottomRight);
end;


function RectToCenter(var ARect: TRect; ABounds: TRect): TRect;
begin
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  OffsetRect(ARect, (ABounds.Width - ARect.Width)
   div 2, (ABounds.Height - ARect.Height) div 2);
  OffsetRect(ARect, ABounds.Left, ABounds.Top);
  Result := ARect;
end;

function Bitmap_GetFromImageList(IL: TCustomImageList; AIndex: Integer;
  B: TBitmap): Boolean;
begin
  Result := (B <> nil) and
    IL.HandleAllocated and (AIndex > -1) and (AIndex < IL.Count);
  if Result then
    if (IL.ColorDepth <> cd32Bit) then
      Result := IL.GetBitmap(AIndex, B)
    else
    begin
      B.SetSize(IL.Width, IL.Height);
      B.PixelFormat := pf32bit;
      B.AlphaFormat := afIgnored;
      Bitmap_ClearAlpha(B, 0);
      IL.GetBitmap(AIndex, B);
      B.AlphaFormat := afPremultiplied;
    end;
end;

function Bitmap_GetFromImageListNoPremultiplied(IL: TCustomImageList; AIndex: Integer;
  B: TBitmap; ACheckAlpha: Boolean): Boolean;
begin
  Result := (B <> nil) and
    IL.HandleAllocated and (AIndex > -1) and (AIndex < IL.Count);
  if Result then
    if (IL.ColorDepth <> cd32Bit) then
      Result := IL.GetBitmap(AIndex, B)
    else
    begin
      B.SetSize(IL.Width, IL.Height);
      B.PixelFormat := pf32bit;
      B.AlphaFormat := afIgnored;
      Bitmap_ClearAlpha(B, 0);
      Result := IL.GetBitmap(AIndex, B);
      if ACheckAlpha then
        if not Bitmap_CheckAlpha0(B) then
          Bitmap_ClearAlpha(B, 255);
    end;
end;

function scColor(AColor: TColor; AAlpha: Byte): TscColor;
begin
  TscColorRec(Result).R := GetRValue(ColorToRGB(AColor));
  TscColorRec(Result).G := GetGValue(ColorToRGB(AColor));
  TscColorRec(Result).B := GetBValue(ColorToRGB(AColor));
  TscColorRec(Result).A := AAlpha;
end;

procedure Bitmap_MakeCopyFromPng(ABitmap: TBitMap; APngObject: TPngImage);
begin
  ABitmap.AlphaFormat := afIgnored;
  ABitmap.Assign(APngObject);
  ABitmap.AlphaFormat := afPremultiplied;
end;

procedure FillAlpha(Src: Pointer; Count: Integer; Alpha: byte);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    PscColorRecArray(Src)[I].A := Alpha;
end;

procedure DivAlpha(Src: Pointer; Count: Integer; ADivider: Byte);
var
  I: Integer;
begin
  if ADivider <= 0 then
   ADivider := 1;
  for I := 0 to Count - 1 do
    PscColorRecArray(Src)[I].A := PscColorRecArray(Src)[I].A div ADivider;
end;

procedure FillColor(Src: Pointer; Count: Integer; Color: TColor);
var
  I: Integer;
  R, G, B: Byte;
begin
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  for I := 0 to Count - 1 do
  begin
    PscColorRecArray(Src)[I].R := R;
    PscColorRecArray(Src)[I].G := G;
    PscColorRecArray(Src)[I].B := B;
  end;
end;

procedure Bitmap_DivAlpha(ABitmap: TBitmap; ADivider: Byte);
begin
  with ABitmap do
    DivAlpha(Scanline[Height - 1], Width * Height, ADivider);
end;

procedure Bitmap_ClearAlpha(ABitmap: TBitmap; Alpha: Byte);
begin
  with ABitmap do
    FillAlpha(Scanline[Height - 1], Width * Height, Alpha);
end;

procedure Bitmap_FillColor(ABitmap: TBitmap; Color: TColor);
begin
  with ABitmap do
    FillColor(Scanline[Height - 1], Width * Height, Color);
end;

procedure FillLongword(ASrc: Pointer; ACount: Integer; AValue: Longword);
var
  I: Integer;
  S: PscColorArray;
begin
  if AValue = 0 then FillChar(ASrc^, ACount * 4, 0)
  else if AValue = $FFFFFFFF then
    FillChar(ASrc^, ACount * 4, $FF)
  else
  begin
    S := PscColorArray(ASrc);
    for I := 0 to ACount - 1 do S[I] := AValue;
  end;
end;

procedure Bitmap_Clear(ABitmap: TBitmap; AColor: TscColor);
begin
  with ABitmap do
    FillLongword(Scanline[Height - 1], Width * Height, AColor);
end;

procedure Bitmap_Draw_Tile(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect);
var
  I, J: integer;
  R, R1, SrcR: TRect;
  XCnt, YCnt: integer;
  W, H, DW, DH: integer;
begin
  W := ASrcRect.Width;
  H := ASrcRect.Height;
  if W * H = 0 then Exit;

  SrcR := Rect(0, 0, W, H);
  OffsetRect(SrcR, ADestRect.Left, ADestRect.Top);

  XCnt := ADestRect.Width div W;
  if XCnt = 0 then XCnt := 1;
  YCnt := ADestRect.Height div H;
  if YCnt = 0 then YCnt := 1;

  for I := 0 to XCnt do
    for J := 0 to YCnt do
    begin
      R := SrcR;
      OffsetRect(R, I * W, J * H);
      IntersectRect(R, R, ADestRect);
      DW := R.Width;
      DH := R.Height;
      if (DW = 0) or (DH = 0) then Break;
      R1 := ASrcRect;
      if (DW <> W) or (DH <> H) then
      begin
        R1.Right := R1.Left + DW;
        R1.Bottom := R1.Top + DH;
      end;
      Bitmap_Draw(ABitmap, ACanvas, R1, R);
    end;
end;

procedure Bitmap_DrawAlpha_Tile(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect; Alpha: Byte);
var
  I, J: integer;
  R, R1, SrcR: TRect;
  XCnt, YCnt: integer;
  W, H, DW, DH: integer;
begin
  W := ASrcRect.Width;
  H := ASrcRect.Height;
  if W * H = 0 then Exit;

  SrcR := Rect(0, 0, W, H);
  OffsetRect(SrcR, ADestRect.Left, ADestRect.Top);

  XCnt := ADestRect.Width div W;
  if XCnt = 0 then XCnt := 1;
  YCnt := ADestRect.Height div H;
  if YCnt = 0 then YCnt := 1;

  for I := 0 to XCnt do
    for J := 0 to YCnt do
    begin
      R := SrcR;
      OffsetRect(R, I * W, J * H);
      IntersectRect(R, R, ADestRect);
      DW := R.Width;
      DH := R.Height;
      if (DW = 0) or (DH = 0) then Break;
      R1 := ASrcRect;
      if (DW <> W) or (DH <> H) then
      begin
        R1.Right := R1.Left + DW;
        R1.Bottom := R1.Top + DH;
      end;
      Bitmap_DrawAlpha(ABitmap, ACanvas, R1, R, 255);
    end;
end;

procedure Bitmap_DrawAlpha(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect; Alpha: Byte);
var
  BF: TBlendFunction;
begin
  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := Alpha;
  BF.AlphaFormat := AC_SRC_ALPHA;
  WinAPI.Windows.AlphaBlend(ACanvas.Handle,
   ADestRect.Left, ADestRect.Top, ADestRect.Right - ADestRect.Left, ADestRect.Bottom - ADestRect.Top,
    ABitmap.Canvas.Handle,
      ASrcRect.Left, ASrcRect.Top, ASrcRect.Right - ASrcRect.Left, ASrcRect.Bottom - ASrcRect.Top, BF);
end;

procedure Bitmap_DrawAlpha_XY(ABitmap: TBitmap; ACanvas: TCanvas; AX, AY: integer; Alpha: Byte);
begin
  Bitmap_DrawAlpha(ABitmap, ACanvas, Rect(0, 0, ABitmap.Width, ABitmap.Height),
    Rect(AX, AY, AX + ABitmap.Width, AY + ABitmap.Height), Alpha);
end;

procedure Bitmap_Rotate90_1(ABitmap, ADest: TBitmap);
var
 x, y: Integer;
 P1, P2: PscColor;
begin
  for y := 0 to ABitmap.Height - 1 do
    for x := 0 to ABitmap.Width - 1 do
    begin
      P1 := PscColor(@PscColorArray(ADest.Scanline[ABitmap.Width - 1 - x])[y]);
      P2 := PscColor(@PscColorArray(ABitmap.Scanline[y])[x]);
      P1^ := P2^;
    end;
end;

procedure Bitmap_Rotate90_2(ABitmap, ADest: TBitmap);
var
 x, y: Integer;
 P1, P2: PscColor;
begin
  for y := 0 to ABitmap.Height - 1 do
    for x := 0 to ABitmap.Width - 1 do
    begin
      P1 := PscColor(@PscColorArray(ADest.Scanline[x])[ABitmap.Height - 1 - y]);
      P2 := PscColor(@PscColorArray(ABitmap.Scanline[y])[x]);
      P1^ := P2^;
    end;
end;

procedure Bitmap_FlipVert(ABitmap: TBitmap);
var
 J, J2: Integer;
 Buffer: PscColorArray;
 P1, P2: PscColor;
begin
  J2 := ABitmap.Height - 1;
  GetMem(Buffer, ABitmap.Width shl 2);
  for J := 0 to ABitmap.Height div 2 - 1 do
  begin
    P1 := ABitmap.Scanline[J];
    P2 := ABitmap.Scanline[J2];
    Move(P1^, PscColor(Buffer)^, ABitmap.Width * 4);
    Move(P2^, P1^, ABitmap.Width * 4);
    Move(PscColor(Buffer)^, P2^, ABitmap.Width * 4);
    Dec(J2);
  end;
  FreeMem(Buffer);
end;

procedure Bitmap_Reflection(ABitmap: TBitmap);
var
  SLine: PscColorArray;
  I, J: Integer;
  Alpha: Integer;
  Kf, Step: Double;
begin
  ABitmap.AlphaFormat := afIgnored;
  Bitmap_FlipVert(ABitmap);
  with ABitmap do
  begin
    Step := (1 / Height) *  2;
    Kf := 1;
    for J := 0 to Height - 1 do
    begin
      SLine := Scanline[J];
      for I := 0 to Width - 1 do
      begin
        if TscColorRec(SLine^[I]).A <> 0 then
        begin
          Alpha := Round(TscColorRec(SLine^[i]).A * Kf) div 3;
          if Alpha < 0 then Alpha := 0;
          TscColorRec(SLine^[i]).A := Alpha;
        end;
      end;
      Kf := Kf - Step;
    end;
  end;
  ABitmap.AlphaFormat := afPremultiplied;
end;

type
  PIntArray = ^TIntArray;
  TIntArray = array [0..0] of Integer;

function Max(A, B: Longint): Longint;
begin
  if A > B then Result := A
  else Result := B;
end;

function Min(A, B: Longint): Longint;
begin
  if A < B then Result := A
  else Result := B;
end;

procedure Bitmap_Blur(ABitmap: TBitmap; const Radius: integer);
var
  pix: PscColorArray;
  w, h, wm, hm, wh, vdiv: integer;
  rsum,gsum,bsum,x,y,i,yp,yi,yw: integer;
  P: cardinal;
  divsum: integer;
  stackpointer, stackstart: integer;
  sir: PscColorRec;
  rbs, r1, routsum, goutsum, boutsum, rinsum, ginsum, binsum: integer;
  dv: PIntArray;
  vmin: PIntArray;
  r, g, b: PIntArray;
  stack: PscColorArray;
begin
  if (radius < 1) then Exit;

  w := ABitmap.width;
  h := ABitmap.height;
  wm := w - 1;
  hm := h - 1;
  pix := ABitmap.Scanline[hm];
  wh := w * h;
  vdiv := radius + radius + 1;

  GetMem(r, wh * SizeOf(Integer));
  GetMem(g, wh * SizeOf(Integer));
  GetMem(b, wh * SizeOf(Integer));
  GetMem(vmin, max(w, h) * SizeOf(Integer));
  divsum := (vdiv + 1) shr 1;
  divsum := divsum * divsum;
  GetMem(dv, 256 * divsum * SizeOf(Integer));
  for i := 0 to 256 * divsum - 1 do
    dv[i] := (i div divsum);

  yw := 0;
  yi := 0;

  GetMem(stack, vdiv * SizeOf(TscColor));

  r1 := radius + 1;

  for y := 0 to h - 1 do
  begin
    rinsum := 0;
    ginsum := 0;
    binsum := 0;
    routsum := 0;
    goutsum := 0;
    boutsum := 0;
    rsum := 0;
    gsum := 0;
    bsum :=0;
    for i := -radius to radius do
    begin
      p := pix[yi + min(wm,max(i,0))];
      sir := @stack[i + radius];
      sir.Color := p;
      rbs := r1-abs(i);
      rsum := rsum + (sir.r*rbs);
      gsum := gsum + (sir.g*rbs);
      bsum := bsum + (sir.b*rbs);
      if (i > 0) then
      begin
        rinsum := rinsum + sir.r;
        ginsum := ginsum + sir.g;
        binsum := binsum + sir.b;
      end else
      begin
        routsum := routsum + sir.r;
        goutsum := goutsum + sir.g;
        boutsum := boutsum + sir.b;
      end;
    end;
    stackpointer := radius;

    for x := 0 to w - 1 do
    begin
      r[yi] := dv[rsum];
      g[yi] := dv[gsum];
      b[yi] := dv[bsum];

      rsum := rsum - routsum;
      gsum := gsum - goutsum;
      bsum := bsum - boutsum;

      stackstart := stackpointer-radius+vdiv;
      sir := @stack[stackstart mod vdiv];

      routsum := routsum - sir.r;
      goutsum := goutsum - sir.g;
      boutsum := boutsum - sir.b;

      if (y=0)then
      begin
        vmin[x] := min(x+radius+1,wm);
      end;

      p := pix[yw + vmin[x]];

      sir.color := p;

      rinsum := rinsum + sir.r;
      ginsum := ginsum + sir.g;
      binsum := binsum + sir.b;

      rsum := rsum + rinsum;
      gsum := gsum + ginsum;
      bsum := bsum + binsum;

      stackpointer :=(stackpointer+1) mod vdiv;
      sir := @stack[(stackpointer) mod vdiv];

      routsum := routsum + sir.r;
      goutsum := goutsum + sir.g;
      boutsum := boutsum + sir.b;

      rinsum := rinsum - sir.r;
      ginsum := ginsum - sir.g;
      binsum := binsum - sir.b;

      yi := yi + 1;
    end;
    yw := yw + w;
  end;

  for x := 0 to w - 1 do
  begin
    rinsum := 0;
    ginsum := 0;
    binsum := 0;
    routsum := 0;
    goutsum := 0;
    boutsum := 0;
    rsum := 0;
    gsum := 0;
    bsum :=0;
    yp := -radius * w;
    for i := -radius to radius do
    begin
      yi := max(0,yp) + x;

      sir := @stack[i+radius];

      sir.r := r[yi];
      sir.g := g[yi];
      sir.b := b[yi];

      rbs := r1 - abs(i);

      rsum := rsum + (r[yi]*rbs);
      gsum := gsum + (g[yi]*rbs);
      bsum := bsum + (b[yi]*rbs);

      if (i > 0)then
      begin
        rinsum := rinsum + sir.r;
        ginsum := ginsum + sir.g;
        binsum := binsum + sir.b;
      end else
      begin
        routsum := routsum + sir.r;
        goutsum := goutsum + sir.g;
        boutsum := boutsum + sir.b;
      end;

      if (i < hm) then
      begin
        yp := yp + w;
      end
    end;
    yi := x;
    stackpointer := radius;
    for y := 0 to h - 1 do
    begin
      pix[yi] := Integer($FF000000) or (dv[rsum] shl 16) or (dv[gsum] shl 8) or dv[bsum];

      rsum := rsum - routsum;
      gsum := gsum - goutsum;
      bsum := bsum - boutsum;

      stackstart := stackpointer-radius+vdiv;
      sir := @stack[stackstart mod vdiv];

      routsum := routsum - sir.r;
      goutsum := goutsum - sir.g;
      boutsum := boutsum - sir.b;

      if (x = 0) then
      begin
        vmin[y] := min(y+r1,hm)*w;
      end;
      p := x + vmin[y];

      sir.r := r[p];
      sir.g := g[p];
      sir.b := b[p];

      rinsum := rinsum + sir.r;
      ginsum := ginsum + sir.g;
      binsum := binsum + sir.b;

      rsum := rsum + rinsum;
      gsum := gsum + ginsum;
      bsum := bsum + binsum;

      stackpointer := (stackpointer + 1) mod vdiv;
      sir := @stack[stackpointer];

      routsum := routsum + sir.r;
      goutsum := goutsum + sir.g;
      boutsum := boutsum + sir.b;

      rinsum := rinsum - sir.r;
      ginsum := ginsum - sir.g;
      binsum := binsum - sir.b;

      yi := yi + w;
    end;
  end;
  FreeMem(stack, vdiv * SizeOf(TscColor));
  FreeMem(dv, 256 * divsum * SizeOf(Integer));
  FreeMem(vmin, max(w, h) * SizeOf(Integer));
  FreeMem(r, wh * SizeOf(Integer));
  FreeMem(g, wh * SizeOf(Integer));
  FreeMem(b, wh * SizeOf(Integer));
end;

procedure Bitmap_AlphaScale(Src: TBitmap; Dst: TBitmap);
var
  ix, iy, x, y: Integer;
  xdif, ydif,
  xp, yp, wy, wyi, wx,
  w11, w21, w12, w22: Int64;
  sBits, sLine1, sLine2, dLine: PByteArray;
  sLineDif, dLineDif: Int64;
  c1, c2, c3, c4, c5, c6, c7: Int64;
  Limit: Integer;
 begin
   if (Src.Width = Dst.Width) and (Src.Height = Dst.Height) then
   begin
     Dst.Assign(Src);
     Exit;
   end;

   y := 0;
   if Src.Width < Dst.Width then
   begin
     xdif := (Src.Width  shl 16) div (Dst.Width + Dst.Width div Src.Width - 1);
     ydif := (Src.Height shl 16) div (Dst.Height + Dst.Width div Src.Width - 1);
   end
   else
   begin
     xdif := (Src.Width  shl 16) div Dst.Width;
     ydif := (Src.Height shl 16) div Dst.Height;
   end;

   sBits := Src.ScanLine[0];

   if Src.Height > 1 then
     sLineDif := Int64(Src.ScanLine[1]) - Int64(sBits)
   else
     sLineDif := 0;

   dLine := Dst.ScanLine[0];
   if Dst.Height > 1 then
     dLineDif := Int64(Dst.ScanLine[1]) - Int64(dLine) - 4 * Dst.Width
   else
     dLineDif := 0;

   for iy := 0 to Dst.Height - 1 do
   begin
     yp := y shr 16;
     Pointer(sLine1) := Pointer(Int64(sBits) + sLineDif * yp);
     if yp < Src.Height - 1 then
        Pointer(sLine2) := Pointer(Int64(sLine1) + sLineDif)
     else
        sLine2 := sLine1;

      x := 0;
      wy := y  and $FFFF;
      wyi := (not y) and $FFFF;

      for ix := 0 to Dst.Width - 1 do
      begin
        xp  := (x shr 16) * 4;
        wx  := x and $FFFF;
        w21 := (wyi * wx) shr 16;
        w11 := wyi - w21;
        w22 := (wy  * wx) shr 16;
        w12 := wy  - w22;

        Limit := Abs(sLineDif) - 1;

        if xp > Limit then
          xp := Limit;
        c1 := xp + 1;
        if c1 > Limit then c1 := Limit;
        c2 := xp + 2;
        if c2 > Limit then c2 := Limit;
        c3 := xp + 3;
        if c3 > Limit then c3 := Limit;
        c4 := xp + 4;
        if c4 > Limit then c4 := Limit;
        c5 := xp + 5;
        if c5 > Limit then c5 := Limit;
        c6 := xp + 6;
        if c6 > Limit then c6 := Limit;
        c7 := xp + 7;
        if c7 > Limit then c7 := Limit;

        dLine^[0] := (sLine1[xp] * w11 + sLine1[c4] * w21 + sLine2[xp] * w12 + sLine2[c4] * w22) shr 16;
        dLine^[1] := (sLine1[c1] * w11 + sLine1[c5] * w21 + sLine2[c1] * w12 + sLine2[c5] * w22) shr 16;
        dLine^[2] := (sLine1[c2] * w11 + sLine1[c6] * w21 + sLine2[c2] * w12 + sLine2[c6] * w22) shr 16;
        dLine^[3] := (sLine1[c3] * w11 + sLine1[c7] * w21 + sLine2[c3] * w12 + sLine2[c7] * w22) shr 16;

        dLine := Pointer(Int64(dLine) + 4);
        Inc(x, xdif);
      end;
      dLine := Pointer(Int64(dLine) + dLineDif);
      Inc(y, ydif);
   end;
end;

procedure Bitmap_DrawScaleAlpha_XY(ABitmap: TBitmap; ACanvas: TCanvas; AX, AY: integer; Alpha: Byte; AScaleFactor: Double);
var
  {$IFDEF GPDRAWING}
  R: TRect;
  {$ELSE}
  Buffer: TBitmap;
  {$ENDIF}
begin
  if AScaleFactor = 1 then
  begin
    Bitmap_DrawAlpha_XY(ABitmap, ACanvas, AX, AY, Alpha);
    Exit;
  end;
  {$IFDEF GPDRAWING}
  R := Rect(AX, AY, AX + Round(ABitmap.Width * AScaleFactor), AY + Round(ABitmap.Height * AScaleFactor));
  GPDrawBitmapSmooth(ACanvas.Handle, R, ABitmap);
  {$ELSE}
  Buffer := TBitmap.Create;
  Buffer.PixelFormat := pf32bit;
  try
    Buffer.Width := Round(ABitmap.Width * AScaleFactor);
    Buffer.Height := Round(ABitmap.Height * AScaleFactor);
    Bitmap_AlphaScale(ABitmap, Buffer);
    Bitmap_DrawAlpha_XY(Buffer, ACanvas, AX, AY, Alpha);
  finally
    Buffer.Free;
  end;
  {$ENDIF}
end;

procedure Bitmap_DrawScaleAlpha_XY(ABitmap: TBitmap; ACanvas: TCanvas; AX, AY: integer; Alpha: Byte; ANewWidth, ANewHeight: Integer);
var
  {$IFDEF GPDRAWING}
  R: TRect;
  {$ELSE}
  Buffer: TBitmap;
  {$ENDIF}
begin
  if (ABitmap.Width = ANewWidth) and (ABitmap.Height = ANewHeight) then
  begin
    Bitmap_DrawAlpha_XY(ABitmap, ACanvas, AX, AY, Alpha);
    Exit;
  end;

  if ANewWidth * ANewHeight = 0 then Exit;
  {$IFDEF GPDRAWING}
  R := Rect(AX, AY, AX + ANewWidth, AY + ANewHeight);
  GPDrawBitmapSmooth(ACanvas.Handle, R, ABitmap);
  {$ELSE}
  Buffer := TBitmap.Create;
  Buffer.PixelFormat := pf32bit;
  try
    Buffer.Width := ANewWidth;
    Buffer.Height := ANewHeight;
    Bitmap_AlphaScale(ABitmap, Buffer);
    Bitmap_DrawAlpha_XY(Buffer, ACanvas, AX, AY, Alpha);
  finally
    Buffer.Free;
  end;
  {$ENDIF}
end;

procedure CreatePngFromBitmap(B: TBitMap; P: TPngImage);
var
  X, Y: Integer;
  Line: Vcl.Imaging.PngImage.pByteArray;
  scColor: PscColor;
  Buffer: TBitmap;
begin
  if B.PixelFormat <> pf32Bit then
  begin
    P.Assign(B);
    Exit;
  end;
  Buffer := TBitmap.Create;
  Buffer.SetSize(B.Width, B.Height);
  Buffer.PixelFormat := pf32bit;
  Bitmap_ClearAlpha(Buffer, 0);
  Buffer.AlphaFormat := afDefined;
  Bitmap_DrawAlpha_XY(B, Buffer.Canvas, 0, 0, 255);
  Buffer.AlphaFormat := afIgnored;
  P.Assign(Buffer);
  if B.PixelFormat = pf32Bit then
  begin
    P.CreateAlpha;
    for Y := 0 to Buffer.Height - 1 do
    begin
      Line := P.AlphaScanline[Y];
      for X := 0 to Buffer.Width - 1 do
      begin
        scColor := PscColor(@PscColorArray(Buffer.Scanline[Y])[X]);
        Line^[X] := PscColorRec(scColor)^.A;
      end;
    end;
  end;
  Buffer.Free;
end;

procedure CreateGlowBitmapFromPng(B: TBitMap; P: TPngImage; GlowSize: Integer; GlowColor: TColor);
var
  X, Y: Integer;
  Line: Vcl.Imaging.PngImage.pByteArray;
  C: TscColor;
  DstP: PscColor;
begin
  B.SetSize(P.Width + GlowSize * 2, P.Height + GlowSize * 2);
  B.PixelFormat := pf32bit;
  Bitmap_Clear(B, scColor(clBlack, 255));
  C := scColor(clWhite, 255);
  for Y := 0 to P.Height - 1 do
   begin
     Line := P.AlphaScanline[Y];
     for X := 0 to P.Width - 1 do
     begin
       DstP := PscColor(@PscColorArray(B.Scanline[y + GlowSize])[x + GlowSize]);
       if Line^[X] <> 0
       then
         DstP^ := C
     end;
   end;
  Bitmap_Blur(B, GlowSize);
  B.AlphaFormat := afIgnored;
  for Y := 0 to B.Height - 1 do
  begin
    for X := 0 to B.Width - 1 do
    begin
      DstP := PscColor(@PscColorArray(B.Scanline[y])[x]);
      DstP^ := scColor(GlowColor, TscColorRec(DstP^).R);
    end;
  end;
  B.AlphaFormat := afPremultiplied;
end;

procedure CreateGlowBitmapFromMask(B: TBitMap; GlowSize: Integer; GlowColor: TColor);
var
  X, Y: Integer;
  DstP: PscColor;
begin
  Bitmap_Blur(B, GlowSize);
  B.AlphaFormat := afIgnored;
  for Y := 0 to B.Height - 1 do
  begin
    for X := 0 to B.Width - 1 do
    begin
      DstP := PscColor(@PscColorArray(B.Scanline[y])[x]);
      DstP^ := scColor(GlowColor, TscColorRec(DstP^).R);
    end;
  end;
  B.AlphaFormat := afPremultiplied;
end;

procedure CreateGlowBitmapFromBitmap(B, SourceB: TBitMap; GlowSize: Integer; GlowColor: TColor);
var
  X, Y: Integer;
  SrcP, DstP, TrC: PscColor;
  C: TscColor;
  Buffer: TBitmap;
begin
  B.SetSize(SourceB.Width + GlowSize * 2, SourceB.Height + GlowSize * 2);
  B.PixelFormat := pf32bit;
  Bitmap_Clear(B, scColor(clBlack, 255));
  C := scColor(clWhite, 255);
  if (SourceB.PixelFormat = pf32bit) and Bitmap_CheckAlpha(SourceB) then
  begin
    for Y := 0 to SourceB.Height - 1 do
    begin
      for X := 0 to SourceB.Width - 1 do
      begin
        SrcP := PscColor(@PscColorArray(SourceB.Scanline[y])[x]);
        DstP := PscColor(@PscColorArray(B.Scanline[y + GlowSize])[x + GlowSize]);
        if PscColorRec(SrcP).A <> 0 then DstP^ := C;
      end;
    end;
  end
  else
  begin
    Buffer := TBitmap.Create;
    Buffer.Assign(SourceB);
    Buffer.PixelFormat := pf32bit;
    Bitmap_ClearAlpha(Buffer, 255);
    Trc := PscColor(@PscColorArray(Buffer.Scanline[0])[0]);
    for Y := 0 to Buffer.Height - 1 do
    begin
      for X := 0 to Buffer.Width - 1 do
      begin
        SrcP := PscColor(@PscColorArray(Buffer.Scanline[y])[x]);
        DstP := PscColor(@PscColorArray(B.Scanline[y + GlowSize])[x + GlowSize]);
        if Trc^ <> SrcP^ then DstP^ := C;
      end;
    end;
    Buffer.Free;
  end;
  Bitmap_Blur(B, GlowSize);
  B.AlphaFormat := afIgnored;
  for Y := 0 to B.Height - 1 do
  begin
    for X := 0 to B.Width - 1 do
    begin
      DstP := PscColor(@PscColorArray(B.Scanline[y])[x]);
      DstP^ := scColor(GlowColor, TscColorRec(DstP^).R);
    end;
  end;
  B.AlphaFormat := afPremultiplied;
end;

procedure Bitmap_Draw(ABitmap: TBitmap; ACanvas: TCanvas; ASrcRect, ADestRect: TRect);
begin
  StretchBlt(ACanvas.Handle, ADestRect.Left, ADestRect.Top, ADestRect.Right - ADestRect.Left,
    ADestRect.Bottom - ADestRect.Top,
    ABitmap.Canvas.Handle, ASrcRect.Left, ASrcRect.Top, ASrcRect.Right - ASrcRect.Left,
    ASrcRect.Bottom - ASrcRect.Top, SRCCOPY);
end;

procedure Bitmap_DrawWithOptions(ABitmap: TBitmap; AOptions: TscBitmapOptions; ACanvas: TCanvas; ARect: TRect; Alpha: Byte = 255);
var
  CornerR1, CornerR2, CornerR3, CornerR4: TRect;
  BorderR1, BorderR2, BorderR3, BorderR4: TRect;
  ClientR: TRect;
  CornerR1_Dest, CornerR2_Dest, CornerR3_Dest, CornerR4_Dest: TRect;
  BorderR1_Dest, BorderR2_Dest, BorderR3_Dest, BorderR4_Dest: TRect;
  ClientR_Dest: TRect;
  X, Y: Integer;
  R: TRect;
begin
  if (AOptions.LeftMargin = 0) and (AOptions.RightMargin = 0) and
     (AOptions.TopMargin = 0) and (AOptions.BottomMargin = 0)
  then
  begin
    X := ARect.Left + ARect.Width div 2 - ABitmap.Width div 2;
    Y := ARect.Top + ARect.Height div 2 - ABitmap.Height div 2;
    if X < ARect.Left then X := ARect.Left;
    if Y < ARect.Top then Y := ARect.Top;
    if ABitmap.PixelFormat = pf32Bit then
      Bitmap_DrawAlpha_XY(ABitmap, ACanvas, X, Y, Alpha)
    else
      ACanvas.Draw(X, Y, ABitmap);
  end
  else
  if (AOptions.LeftMargin > 0) and (AOptions.RightMargin > 0) and
     (AOptions.TopMargin > 0) and (AOptions.BottomMargin > 0)
  then
  begin
    CornerR1 := Rect(0, 0, AOptions.LeftMargin, AOptions.TopMargin);
    CornerR2 := Rect(ABitmap.Width - AOptions.RightMargin, 0,
                     ABitmap.Width, AOptions.TopMargin);
    CornerR3 := Rect(0, ABitmap.Height - AOptions.BottomMargin,
                     AOptions.LeftMargin, ABitmap.Height);
    CornerR4 := Rect(ABitmap.Width - AOptions.RightMargin, 
                     ABitmap.Height - AOptions.BottomMargin, 
                     ABitmap.Width, ABitmap.Height);
    BorderR1 := Rect(AOptions.LeftMargin, 0,
                     ABitmap.Width - AOptions.RightMargin,
                     AOptions.TopMargin);
    BorderR2 := Rect(AOptions.LeftMargin, ABitmap.Height - AOptions.BottomMargin,
                     ABitmap.Width - AOptions.RightMargin, ABitmap.Height);
    BorderR3 := Rect(0, AOptions.TopMargin, AOptions.LeftMargin,
                     ABitmap.Height - AOptions.BottomMargin);
    BorderR4 := Rect(ABitmap.Width - AOptions.RightMargin,
                     AOptions.TopMargin,
                     ABitmap.Width,
                     ABitmap.Height - AOptions.BottomMargin);
    ClientR := Rect(AOptions.LeftMargin, AOptions.TopMargin,
                    ABitmap.Width - AOptions.RightMargin,
                    ABitmap.Height - AOptions.BottomMargin);
                    
    X := ARect.Width - ABitmap.Width;
    Y := ARect.Height - ABitmap.Height;
    // calc client
    ClientR_Dest := ClientR;
    OffsetRect(ClientR_Dest, ARect.Left, ARect.Top);
    ClientR_Dest.Right := ClientR_Dest.Right + X;
    ClientR_Dest.Bottom := ClientR_Dest.Bottom + Y;
    // calc corners
    CornerR1_Dest := CornerR1;
    OffsetRect(CornerR1_Dest, ARect.Left, ARect.Top);
    CornerR2_Dest := CornerR2;
    OffsetRect(CornerR2_Dest, ARect.Left, ARect.Top);
    OffsetRect(CornerR2_Dest, X, 0);
    CornerR3_Dest := CornerR3;
    OffsetRect(CornerR3_Dest, ARect.Left, ARect.Top);
    OffsetRect(CornerR3_Dest, 0, Y);
    CornerR4_Dest := CornerR4;
    OffsetRect(CornerR4_Dest, ARect.Left, ARect.Top);
    OffsetRect(CornerR4_Dest, X, Y);
    // calc border
    BorderR1_Dest := BorderR1;
    OffsetRect(BorderR1_Dest, ARect.Left, ARect.Top);
    BorderR1_Dest.Right := BorderR1_Dest.Right + X;
    BorderR2_Dest := BorderR2;
    OffsetRect(BorderR2_Dest, ARect.Left, ARect.Top);
    BorderR2_Dest.Right := BorderR2_Dest.Right + X;
    OffsetRect(BorderR2_Dest, 0, Y);
    BorderR3_Dest := BorderR3;
    OffsetRect(BorderR3_Dest, ARect.Left, ARect.Top);
    BorderR3_Dest.Bottom := BorderR3_Dest.Bottom + Y;
    BorderR4_Dest := BorderR4;
    OffsetRect(BorderR4_Dest, ARect.Left, ARect.Top);
    BorderR4_Dest.Bottom := BorderR4_Dest.Bottom + Y;
    OffsetRect(BorderR4_Dest, X, 0);
    // check corners
    if CornerR1_Dest.Right > ARect.Right then
    begin
      Dec(CornerR1.Right, CornerR1_Dest.Right - ARect.Right);
      Dec(CornerR1_Dest.Right, CornerR1_Dest.Right - ARect.Right);
    end;
    if CornerR2_Dest.Left < CornerR1_Dest.Right then
    begin
      Inc(CornerR2.Left, CornerR1_Dest.Right - CornerR2_Dest.Left);
      Inc(CornerR2_Dest.Left, CornerR1_Dest.Right - CornerR2_Dest.Left);
    end;
    if CornerR3_Dest.Right > ARect.Right then
    begin
      Dec(CornerR3.Right, CornerR3_Dest.Right - ARect.Right);
      Dec(CornerR3_Dest.Right, CornerR3_Dest.Right - ARect.Right);
    end;
    if CornerR4_Dest.Left < CornerR3_Dest.Right then
    begin
      Inc(CornerR4.Left, CornerR3_Dest.Right - CornerR4_Dest.Left);
      Inc(CornerR4_Dest.Left, CornerR3_Dest.Right - CornerR4_Dest.Left);
    end;
    //
    if CornerR1_Dest.Bottom > ARect.Bottom then
    begin
      Dec(CornerR1.Bottom, CornerR1_Dest.Bottom - ARect.Bottom);
      Dec(CornerR1_Dest.Bottom, CornerR1_Dest.Bottom - ARect.Bottom);
    end;
    if CornerR2_Dest.Bottom > ARect.Bottom then
    begin
      Dec(CornerR2.Bottom, CornerR2_Dest.Bottom - ARect.Bottom);
      Dec(CornerR2_Dest.Bottom, CornerR2_Dest.Bottom - ARect.Bottom);
    end;
    if CornerR3_Dest.Top < CornerR1_Dest.Bottom then
    begin
      Inc(CornerR3.Top, CornerR1_Dest.Bottom - CornerR3_Dest.Top);
      Inc(CornerR3_Dest.Top, CornerR1_Dest.Bottom - CornerR3_Dest.Top);
    end;
    if CornerR4_Dest.Top < CornerR2_Dest.Bottom then
    begin
      Inc(CornerR4.Top, CornerR2_Dest.Bottom - CornerR4_Dest.Top);
      Inc(CornerR4_Dest.Top, CornerR2_Dest.Bottom - CornerR4_Dest.Top);
    end;
    // check borders
    if BorderR3_Dest.Right > ARect.Right then
    begin
      Dec(BorderR3.Right, BorderR3_Dest.Right - ARect.Right);
      Dec(BorderR3_Dest.Right, BorderR3_Dest.Right - ARect.Right);
    end;
    if BorderR4_Dest.Left < BorderR3_Dest.Right then
    begin
      Inc(BorderR4.Left, BorderR3_Dest.Right - BorderR4_Dest.Left);
      Inc(BorderR4_Dest.Left, BorderR3_Dest.Right - BorderR4_Dest.Left);
    end;
    if BorderR1_Dest.Bottom > ARect.Bottom then
    begin
      Dec(BorderR1.Bottom, BorderR1_Dest.Bottom - ARect.Bottom);
      Dec(BorderR1_Dest.Bottom, BorderR1_Dest.Bottom - ARect.Bottom);
    end;
    if BorderR2_Dest.Top < BorderR1_Dest.Bottom then
    begin
      Inc(BorderR2.Top, BorderR1_Dest.Bottom - BorderR2_Dest.Top);
      Inc(BorderR2_Dest.Top, BorderR1_Dest.Bottom - BorderR2_Dest.Top);
    end;
    // draw corners
    if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlendCorners then
    begin
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR1, CornerR1_Dest, Alpha);
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR2, CornerR2_Dest, Alpha);
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR3, CornerR3_Dest, Alpha);
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR4, CornerR4_Dest, Alpha);
    end
    else
    begin
      ACanvas.CopyRect(CornerR1_Dest, ABitmap.Canvas, CornerR1);
      ACanvas.CopyRect(CornerR2_Dest, ABitmap.Canvas, CornerR2);
      ACanvas.CopyRect(CornerR3_Dest, ABitmap.Canvas, CornerR3);
      ACanvas.CopyRect(CornerR4_Dest, ABitmap.Canvas, CornerR4);
    end;
    // draw border
    if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlendBorder then
    begin
      if AOptions.StretchBorder then
      begin
        if BorderR1_Dest.Width > 0 then
          Bitmap_DrawAlpha(ABitmap, ACanvas, BorderR1, BorderR1_Dest, Alpha);
        if BorderR2_Dest.Width > 0 then
          Bitmap_DrawAlpha(ABitmap, ACanvas, BorderR2, BorderR2_Dest, Alpha);
        if BorderR3_Dest.Height > 0 then
          Bitmap_DrawAlpha(ABitmap, ACanvas, BorderR3, BorderR3_Dest, Alpha);
        if BorderR4_Dest.Height > 0 then
          Bitmap_DrawAlpha(ABitmap, ACanvas, BorderR4, BorderR4_Dest, Alpha);
      end
      else
      begin
        if BorderR1_Dest.Width > 0 then
          Bitmap_DrawAlpha_Tile(ABitmap, ACanvas, BorderR1, BorderR1_Dest, Alpha);
        if BorderR2_Dest.Width > 0 then
          Bitmap_DrawAlpha_Tile(ABitmap, ACanvas, BorderR2, BorderR2_Dest, Alpha);
        if BorderR3_Dest.Height > 0 then
          Bitmap_DrawAlpha_Tile(ABitmap, ACanvas, BorderR3, BorderR3_Dest, Alpha);
        if BorderR4_Dest.Height > 0 then
          Bitmap_DrawAlpha_Tile(ABitmap, ACanvas, BorderR4, BorderR4_Dest, Alpha);
      end;
    end
    else
    begin
      if AOptions.StretchBorder then
      begin
        if BorderR1_Dest.Width > 0 then
          Bitmap_Draw(ABitmap, ACanvas, BorderR1, BorderR1_Dest);
        if BorderR2_Dest.Width > 0 then
          Bitmap_Draw(ABitmap, ACanvas, BorderR2, BorderR2_Dest);
        if BorderR3_Dest.Height > 0 then
          Bitmap_Draw(ABitmap, ACanvas, BorderR3, BorderR3_Dest);
        if BorderR4_Dest.Height > 0 then
          Bitmap_Draw(ABitmap, ACanvas, BorderR4, BorderR4_Dest);
      end
      else
      begin
        if BorderR1_Dest.Width > 0 then
          Bitmap_Draw_Tile(ABitmap, ACanvas, BorderR1, BorderR1_Dest);
        if BorderR2_Dest.Width > 0 then
          Bitmap_Draw_Tile(ABitmap, ACanvas, BorderR2, BorderR2_Dest);
        if BorderR3_Dest.Height > 0 then
          Bitmap_Draw_Tile(ABitmap, ACanvas, BorderR3, BorderR3_Dest);
        if BorderR4_Dest.Height > 0 then
          Bitmap_Draw_Tile(ABitmap, ACanvas, BorderR4, BorderR4_Dest);
      end;
    end;
    // draw client
    if ((ClientR_Dest.Width > 0) and (ClientR_Dest.Height > 0)) and not AOptions.DrawOnlyBorder then
    begin
      if AOptions.Stretch then
      begin
        if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlend then
          Bitmap_DrawAlpha(ABitmap, ACanvas, ClientR, ClientR_Dest, Alpha)
        else
          Bitmap_Draw(ABitmap, ACanvas, ClientR, ClientR_Dest);
      end
      else
      begin
        if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlend then
          Bitmap_DrawAlpha_Tile(ABitmap, ACanvas, ClientR, ClientR_Dest, Alpha)
        else
          Bitmap_Draw_Tile(ABitmap, ACanvas, ClientR, ClientR_Dest);
      end;
    end;
  end
  else
  if (AOptions.LeftMargin > 0) and (AOptions.RightMargin > 0)
  then
  begin
    R := ARect;
    R.Bottom := R.Top + ABitmap.Height;
    Y := ARect.Height div 2 - R.Height div 2;
    if Y < 0 then Y := 0;
    OffsetRect(R, 0, Y);
    CornerR1 := Rect(0, 0, AOptions.LeftMargin, ABitmap.Height);
    CornerR2 := Rect(ABitmap.Width - AOptions.RightMargin, 0,
                     ABitmap.Width, ABitmap.Height);
    ClientR := Rect(AOptions.LeftMargin, 0,
                    ABitmap.Width - AOptions.RightMargin,
                    ABitmap.Height);
    X := R.Width - ABitmap.Width;
    ClientR_Dest := ClientR;
    OffsetRect(ClientR_Dest, R.Left, R.Top);
    ClientR_Dest.Right := ClientR_Dest.Right + X;
    CornerR1_Dest := CornerR1;
    OffsetRect(CornerR1_Dest, R.Left, R.Top);
    CornerR2_Dest := CornerR2;
    OffsetRect(CornerR2_Dest, R.Left, R.Top);
    OffsetRect(CornerR2_Dest, X, 0);
    if CornerR1_Dest.Right > ARect.Right then
    begin
      Dec(CornerR1.Right, CornerR1_Dest.Right - ARect.Right);
      Dec(CornerR1_Dest.Right, CornerR1_Dest.Right - ARect.Right);
    end;
    if CornerR2_Dest.Left < CornerR1_Dest.Right then
    begin
      Inc(CornerR2.Left, CornerR1_Dest.Right - CornerR2_Dest.Left);
      Inc(CornerR2_Dest.Left, CornerR1_Dest.Right - CornerR2_Dest.Left);
    end;
    // draw corners
    if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlendCorners then
    begin
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR1, CornerR1_Dest, Alpha);
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR2, CornerR2_Dest, Alpha);
    end
    else
    begin
      ACanvas.CopyRect(CornerR1_Dest, ABitmap.Canvas, CornerR1);
      ACanvas.CopyRect(CornerR2_Dest, ABitmap.Canvas, CornerR2);
    end;
    // draw client
    if (ClientR_Dest.Width > 0) and (ClientR_Dest.Height > 0) then
    begin
      if AOptions.Stretch then
      begin
        if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlend then
          Bitmap_DrawAlpha(ABitmap, ACanvas, ClientR, ClientR_Dest, Alpha)
        else
          Bitmap_Draw(ABitmap, ACanvas, ClientR, ClientR_Dest);
      end
      else
      begin
        if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlend then
          Bitmap_DrawAlpha_Tile(ABitmap, ACanvas, ClientR, ClientR_Dest, Alpha)
        else
          Bitmap_Draw_Tile(ABitmap, ACanvas, ClientR, ClientR_Dest);
      end;
    end;
  end
  else
  if (AOptions.TopMargin > 0) and (AOptions.BottomMargin > 0)
  then
  begin
    R := ARect;
    R.Right := R.Left + ABitmap.Width;
    X := ARect.Width div 2 - R.Width div 2;
    if X < 0 then X := 0;
    OffsetRect(R, X, 0);
    CornerR1 := Rect(0, 0, ABitmap.Width, AOptions.TopMargin);
    CornerR2 := Rect(0, ABitmap.Height - AOptions.BottomMargin,
                     ABitmap.Width, ABitmap.Height);
    ClientR := Rect(0, AOptions.TopMargin,
                    ABitmap.Width,
                    ABitmap.Height - AOptions.BottomMargin);
    Y := R.Height - ABitmap.Height;
    ClientR_Dest := ClientR;
    OffsetRect(ClientR_Dest, R.Left, R.Top);
    ClientR_Dest.Bottom := ClientR_Dest.Bottom + Y;
    CornerR1_Dest := CornerR1;
    OffsetRect(CornerR1_Dest, R.Left, R.Top);
    CornerR2_Dest := CornerR2;
    OffsetRect(CornerR2_Dest, R.Left, R.Top);
    OffsetRect(CornerR2_Dest, 0, Y);
    if CornerR1_Dest.Bottom > ARect.Bottom then
    begin
      Dec(CornerR1.Bottom, CornerR1_Dest.Bottom - ARect.Bottom);
      Dec(CornerR1_Dest.Bottom, CornerR1_Dest.Bottom - ARect.Bottom);
    end;
    if CornerR2_Dest.Top < CornerR1_Dest.Bottom then
    begin
      Inc(CornerR2.Top, CornerR1_Dest.Bottom - CornerR2_Dest.Top);
      Inc(CornerR2_Dest.Top, CornerR1_Dest.Bottom - CornerR2_Dest.Top);
    end;
    // draw corners
    if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlendCorners then
    begin
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR1, CornerR1_Dest, Alpha);
      Bitmap_DrawAlpha(ABitmap, ACanvas, CornerR2, CornerR2_Dest, Alpha);
    end
    else
    begin
      ACanvas.CopyRect(CornerR1_Dest, ABitmap.Canvas, CornerR1);
      ACanvas.CopyRect(CornerR2_Dest, ABitmap.Canvas, CornerR2);
    end;
    // draw client
    if (ClientR_Dest.Width > 0) and (ClientR_Dest.Height > 0) then
    begin
      if AOptions.Stretch then
      begin
        if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlend then
          Bitmap_DrawAlpha(ABitmap, ACanvas, ClientR, ClientR_Dest, Alpha)
        else
          Bitmap_Draw(ABitmap, ACanvas, ClientR, ClientR_Dest);
      end
      else
      begin
        if (ABitmap.PixelFormat = pf32bit) and AOptions.AlphaBlend then
          Bitmap_DrawAlpha_Tile(ABitmap, ACanvas, ClientR, ClientR_Dest, Alpha)
        else
          Bitmap_Draw_Tile(ABitmap, ACanvas, ClientR, ClientR_Dest);
      end;
    end;
  end;
end;
  
procedure Bitmap_CheckOptions(ABitmap: TBitmap; AOptions: TscBitmapOptions);
var
  CornerR1, CornerR2, CornerR3, CornerR4: TRect;
  BorderR1, BorderR2, BorderR3, BorderR4: TRect;
  ClientR: TRect;
begin
  if ABitmap.Empty or (ABitmap.PixelFormat = pf24Bit) then
  begin
    AOptions.AlphaBlend := False;
    AOptions.AlphaBlendBorder := False;
    AOptions.AlphaBlendCorners := False;
    Exit;
  end;
  if (AOptions.LeftMargin = 0) and (AOptions.RightMargin = 0) and
     (AOptions.TopMargin = 0) and (AOptions.BottomMargin = 0)
  then
  begin
    AOptions.AlphaBlend := Bitmap_CheckAlpha(ABitmap);
    AOptions.AlphaBlendBorder := False;
    AOptions.AlphaBlendCorners := False;
  end
  else
  if (AOptions.LeftMargin > 0) and (AOptions.RightMargin > 0) and
     (AOptions.TopMargin > 0) and (AOptions.BottomMargin > 0)
  then
  begin
    CornerR1 := Rect(0, 0, AOptions.LeftMargin, AOptions.TopMargin);
    CornerR2 := Rect(ABitmap.Width - AOptions.RightMargin, 0,
                     ABitmap.Width - 1, AOptions.TopMargin);
    CornerR3 := Rect(0, ABitmap.Height - AOptions.BottomMargin,
                     AOptions.LeftMargin, ABitmap.Height - 1);
    CornerR4 := Rect(ABitmap.Width - AOptions.RightMargin, 
                     ABitmap.Height - AOptions.BottomMargin, 
                     ABitmap.Width - 1, ABitmap.Height - 1);
    BorderR1 := Rect(AOptions.LeftMargin + 1, 0, 
                     ABitmap.Width - AOptions.RightMargin - 1, 
                     AOptions.TopMargin);
    BorderR2 := Rect(AOptions.LeftMargin, ABitmap.Height - AOptions.BottomMargin,
                     ABitmap.Width - AOptions.RightMargin - 1, ABitmap.Height - 1);
    BorderR3 := Rect(0, AOptions.TopMargin + 1, AOptions.LeftMargin, 
                     ABitmap.Height - AOptions.BottomMargin - 1);
    BorderR4 := Rect(ABitmap.Width - AOptions.RightMargin,
                     AOptions.TopMargin + 1,
                     ABitmap.Width - 1,
                     ABitmap.Height - AOptions.BottomMargin - 1);
    ClientR := Rect(AOptions.LeftMargin, AOptions.TopMargin,
                    ABitmap.Width - AOptions.RightMargin,
                    ABitmap.Height - AOptions.BottomMargin);

    AOptions.AlphaBlend := Bitmap_CheckAlphaRect(ABitmap, ClientR);

    AOptions.AlphaBlendBorder :=
      Bitmap_CheckAlphaRect(ABitmap, BorderR1) or
      Bitmap_CheckAlphaRect(ABitmap, BorderR2) or
      Bitmap_CheckAlphaRect(ABitmap, BorderR3) or
      Bitmap_CheckAlphaRect(ABitmap, BorderR4);

    AOptions.AlphaBlendCorners :=
      Bitmap_CheckAlphaRect(ABitmap, CornerR1) or
      Bitmap_CheckAlphaRect(ABitmap, CornerR2) or
      Bitmap_CheckAlphaRect(ABitmap, CornerR3) or
      Bitmap_CheckAlphaRect(ABitmap, CornerR4);
  end
  else
  if (AOptions.LeftMargin > 0) and (AOptions.RightMargin > 0) then 
  begin
    CornerR1 := Rect(0, 0, AOptions.LeftMargin, ABitmap.Height - 1);
    CornerR2 := Rect(ABitmap.Width - AOptions.RightMargin, 0, 
                ABitmap.Width - 1, ABitmap.Height - 1);
    ClientR := Rect(AOptions.LeftMargin + 1, 0, 
                    ABitmap.Width - AOptions.RightMargin - 1,
                    ABitmap.Height - 1);
    AOptions.AlphaBlend := Bitmap_CheckAlphaRect(ABitmap, ClientR);
    AOptions.AlphaBlendBorder := AOptions.AlphaBlend; 
    AOptions.AlphaBlendCorners :=
      Bitmap_CheckAlphaRect(ABitmap, CornerR1) or
      Bitmap_CheckAlphaRect(ABitmap, CornerR2);
                      
  end
  else  
  if (AOptions.TopMargin > 0) and (AOptions.BottomMargin > 0) then 
  begin
    CornerR1 := Rect(0, 0, ABitmap.Width - 1, AOptions.TopMargin);
    CornerR2 := Rect(0, ABitmap.Height - AOptions.BottomMargin, 
                ABitmap.Width - 1, ABitmap.Height - 1);
    ClientR := Rect(0, AOptions.TopMargin + 1, 
                    ABitmap.Width - 1,
                    ABitmap.Height - AOptions.BottomMargin - 1);
    AOptions.AlphaBlend := Bitmap_CheckAlphaRect(ABitmap, ClientR);
    AOptions.AlphaBlendBorder := AOptions.AlphaBlend; 
    AOptions.AlphaBlendCorners :=
      Bitmap_CheckAlphaRect(ABitmap, CornerR1) or
      Bitmap_CheckAlphaRect(ABitmap, CornerR2);
  end;
end;

function Bitmap_CheckAlphaRect(ABitmap: TBitmap; ARect: TRect): Boolean;
var
  X, Y: Integer;
  C: PscColor;
begin
  Result := False;
  for Y := ARect.Top to ARect.Bottom do
  begin
    for X := ARect.Left to ARect.Right do
    begin
      C := PscColor(@PscColorArray(ABitmap.Scanline[y])[x]);
      if TscColorRec(C^).A < 255 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function Bitmap_CheckAlpha0(ABitmap: TBitmap): Boolean;
var
  X, Y: Integer;
  C: PscColor;
begin
  Result := False;
  for Y := 0 to ABitmap.Height - 1 do
  begin
    for X := 0 to ABitmap.Width - 1 do
    begin
      C := PscColor(@PscColorArray(ABitmap.Scanline[y])[x]);
      if TscColorRec(C^).A > 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function Bitmap_CheckAlpha(ABitmap: TBitmap): Boolean;
var
  X, Y: Integer;
  C: PscColor;
begin
  Result := False;
  for Y := 0 to ABitmap.Height - 1 do
  begin
    for X := 0 to ABitmap.Width - 1 do
    begin
      C := PscColor(@PscColorArray(ABitmap.Scanline[y])[x]);
      if TscColorRec(C^).A < 255 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure CreateGlowBitmapFromImageList(B: TBitMap; IL: TCustomImageList;
  IIndex: Integer; GlowSize: Integer; GlowColor: TColor);
var
  B1: TBitmap;
begin
  B1 := TBitmap.Create;
  B1.SetSize(IL.Width, IL.Height);
  B1.PixelFormat := pf32bit;
  Bitmap_ClearAlpha(B1, 0);
  IL.GetBitmap(IIndex, B1);
  CreateGlowBitmapFromBitmap(B, B1, GlowSize, GlowColor);
  B1.Free;
end;

procedure DrawBitmapFromImageList(ACanvas: TCanvas; X, Y: Integer;
   IL: TCustomImageList; IIndex: Integer; Alpha: Byte);
var
  B, B1: TBitmap;
  I, J: Integer;
  SrcP, DstP, TrC: PscColor;
begin
  B := TBitmap.Create;
  B.SetSize(IL.Width, IL.Height);
  B.PixelFormat := pf32bit;
  B.AlphaFormat := afIgnored;
  if (IL.ColorDepth = cd32bit) or (IL is TscCustomImageList) then
  begin
    Bitmap_ClearAlpha(B, 0);
    IL.GetBitmap(IIndex, B);
  end
  else
  begin
    Bitmap_ClearAlpha(B, 0);
    B1 := TBitmap.Create;
    B1.PixelFormat := pf32bit;
    B1.SetSize(IL.Width, IL.Height);
    IL.GetBitmap(IIndex, B1);
    Trc := PscColor(@PscColorArray(B1.Scanline[0])[0]);
    for J := 0 to B1.Height - 1 do
    begin
      for I := 0 to B1.Width - 1 do
      begin
        SrcP := PscColor(@PscColorArray(B1.Scanline[J])[I]);
        DstP := PscColor(@PscColorArray(B.Scanline[J])[I]);
        if Trc^ <> SrcP^ then
        begin
          DstP^ := SrcP^;
          TscColorRec(DstP^).A := 255;
        end;
      end;
    end;
    B1.Free;
  end;
  B.AlphaFormat := afPremultiplied;
  Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, Alpha);
  B.Free;
end;

procedure DrawBitmapFromImageListToRect(ACanvas: TCanvas; ARect: TRect;
   IL: TCustomImageList; IIndex: Integer; Alpha: Byte);
var
  B, B1: TBitmap;
  I, J: Integer;
  SrcP, DstP, TrC: PscColor;
begin
  B := TBitmap.Create;
  B.SetSize(IL.Width, IL.Height);
  B.PixelFormat := pf32bit;
  B.AlphaFormat := afIgnored;
  if (IL.ColorDepth = cd32bit) or (IL is TscCustomImageList) then
  begin
    Bitmap_ClearAlpha(B, 0);
    IL.GetBitmap(IIndex, B);
  end
  else
  begin
    Bitmap_ClearAlpha(B, 0);
    B1 := TBitmap.Create;
    B1.PixelFormat := pf32bit;
    B1.SetSize(IL.Width, IL.Height);
    IL.GetBitmap(IIndex, B1);
    Trc := PscColor(@PscColorArray(B1.Scanline[0])[0]);
    for J := 0 to B1.Height - 1 do
    begin
      for I := 0 to B1.Width - 1 do
      begin
        SrcP := PscColor(@PscColorArray(B1.Scanline[J])[I]);
        DstP := PscColor(@PscColorArray(B.Scanline[J])[I]);
        if Trc^ <> SrcP^ then
        begin
          DstP^ := SrcP^;
          TscColorRec(DstP^).A := 255;
        end;
      end;
    end;
    B1.Free;
  end;
  B.AlphaFormat := afPremultiplied;
  Bitmap_DrawAlpha(B, ACanvas, Rect(0, 0, B.Width, B.Height), ARect, Alpha);
  B.Free;
end;

procedure DrawPixelLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer; AColor: TColor);
var
  B: Boolean;
  I: Integer;
begin
  ACanvas.Pen.Color := ACanvas.Font.Color;
  if X1 = X2 then
  begin
    B := True;
    for I := Y1 to Y2 do
    begin
     if B then
        ACanvas.Pixels[X1, I] := AColor;
      B := not B;
    end;
  end
  else
  if Y1 = Y2 then
  begin
    B := True;
    for I := X1 to X2 do
    begin
      if B then
        ACanvas.Pixels[I, Y1] := AColor;
      B := not B;
    end;
  end;
end;

procedure scDrawFocusRect2(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
var
  i, j: Integer;
  B1: Boolean;
  FOldPen: HPen;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  Dec(ARect.Right);
  Dec(ARect.Bottom);
  FOldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));
  SetDCPenColor(ACanvas.Handle, ColorToRGB(AColor));
  B1 := ((ARect.Width div 2) = (ARect.Width / 2)) and
        ((ARect.Height div 2) = (ARect.Height / 2));
  if AScaleFactor >= 1.5 then
    AScaleFactor := 2
  else
    AScaleFactor := 1;
  for j := 1 to Round(AScaleFactor) do
  begin
  for i := ARect.Left to ARect.Right  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[i, ARect.Top] := AColor;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Top to ARect.Bottom  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[ARect.Right, i] := AColor;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Right downto ARect.Left  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[i, ARect.Bottom] := AColor;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Bottom downto ARect.Top  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[ARect.Left, i] := AColor;
  end;
  InflateRect(ARect, -1, -1);
  end;
  SelectObject(ACanvas.Handle, FOldPen);
end;

procedure scDrawFocusRect(ACanvas: TCanvas; ARect: TRect; AScaleFactor: Double = 1);
var
  i, j: Integer;
  B1: Boolean;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  Dec(ARect.Right);
  Dec(ARect.Bottom);
  B1 := ((ARect.Width div 2) = (ARect.Width / 2)) and
        ((ARect.Height div 2) = (ARect.Height / 2));
  ACanvas.Pen.Color := ACanvas.Font.Color;
  if AScaleFactor >= 1.5 then
    AScaleFactor := 2
  else
    AScaleFactor := 1;
  for j := 1 to Round(AScaleFactor) do
  begin
  for i := ARect.Left to ARect.Right  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[i, ARect.Top] := ACanvas.Font.Color;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Top to ARect.Bottom  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[ARect.Right, i] := ACanvas.Font.Color;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Right downto ARect.Left  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[i, ARect.Bottom] := ACanvas.Font.Color;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Bottom downto ARect.Top  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[ARect.Left, i] := ACanvas.Font.Color;
  end;
  InflateRect(ARect, -1, -1);
  end;
end;

procedure scDrawColorFocusRect(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AScaleFactor: Double = 1);
var
  i, j: Integer;
  B1: Boolean;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  Dec(ARect.Right);
  Dec(ARect.Bottom);
  B1 := ((ARect.Width div 2) = (ARect.Width / 2)) and
        ((ARect.Height div 2) = (ARect.Height / 2));
  ACanvas.Pen.Color := AColor;
  if AScaleFactor >= 1.5 then
    AScaleFactor := 2
  else
    AScaleFactor := 1;
  for j := 1 to Round(AScaleFactor) do
  begin
  for i := ARect.Left to ARect.Right  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[i, ARect.Top] := ACanvas.Font.Color;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Top to ARect.Bottom  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[ARect.Right, i] := ACanvas.Font.Color;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Right downto ARect.Left  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[i, ARect.Bottom] := ACanvas.Font.Color;
  end;
  if not B1 then B1 := True else B1 := False;
  for i := ARect.Bottom downto ARect.Top  do
  begin
    B1 := not B1;
    if B1 then ACanvas.Pixels[ARect.Left, i] := ACanvas.Font.Color;
  end;
  InflateRect(ARect, -1, -1);
  end;
end;

procedure GetBitmapFromImageList(ABitmap: TBitmap; IL: TCustomImageList; IIndex: Integer);
var
  B1: TBitmap;
  I, J: Integer;
  SrcP, DstP, TrC: PscColor;
begin
  ABitmap.SetSize(IL.Width, IL.Height);
  ABitmap.PixelFormat := pf32bit;
  ABitmap.AlphaFormat := afIgnored;
  if (IL.ColorDepth = cd32bit) or (IL is TscCustomImageList) then
  begin
    Bitmap_ClearAlpha(ABitmap, 0);
    IL.GetBitmap(IIndex, ABitmap);
  end
  else
  begin
    Bitmap_ClearAlpha(ABitmap, 0);
    B1 := TBitmap.Create;
    B1.PixelFormat := pf32bit;
    B1.SetSize(IL.Width, IL.Height);
    IL.GetBitmap(IIndex, B1);
    Trc := PscColor(@PscColorArray(B1.Scanline[0])[0]);
    for J := 0 to B1.Height - 1 do
    begin
      for I := 0 to B1.Width - 1 do
      begin
        SrcP := PscColor(@PscColorArray(B1.Scanline[J])[I]);
        DstP := PscColor(@PscColorArray(ABitmap.Scanline[J])[I]);
        if Trc^ <> SrcP^ then
        begin
          DstP^ := SrcP^;
          TscColorRec(DstP^).A := 255;
        end;
      end;
    end;
    B1.Free;
  end;
  ABitmap.AlphaFormat := afPremultiplied;
end;

function DrawTextMultiLineGlow(ACanvas: TCanvas; AText: String; ARect: TRect;
  ALeft, ARightToLeft: Boolean;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AIntensiveGlow: Boolean;
   AGlowAlpha: Byte; ANoPrefix: Boolean): Boolean;
var
  R, R1: TRect;
  B: TBitmap;
begin
  if AText = '' then Exit(False);
  if ARect.Width < AGlowSize then ARect.Right := ARect.Left + AGlowSize;
  if ARect.Height < AGlowSize then ARect.Bottom := ARect.Top + AGlowSize;
  B := TBitmap.Create;
  B.SetSize(ARect.Width + AGlowSize * 2, ARect.Height + AGlowSize * 2);
  B.PixelFormat := pf32bit;
  B.Canvas.Font.Assign(ACanvas.Font);
  B.Canvas.Font.Color := clWhite;
  Bitmap_Clear(B, scColor(clBlack, 255));
  R := Rect(AGlowSize, AGlowSize, B.Width - AGlowSize, B.Height - AGlowSize);
  B.Canvas.Brush.Style := bsClear;
  if AIntensiveGlow then
  begin
    R1 := R;
    OffsetRect(R1, -1, 0);
    DrawTextMultiLine(B.Canvas, AText, R1, ALeft, ARightToLeft, ANoPrefix);
    OffsetRect(R1, 2, 0);
    DrawTextMultiLine(B.Canvas, AText, R1, ALeft, ARightToLeft, ANoPrefix);
    OffsetRect(R1, -1, -1);
    DrawTextMultiLine(B.Canvas, AText, R1, ALeft, ARightToLeft, ANoPrefix);
    OffsetRect(R1, 0, 2);
    DrawTextMultiLine(B.Canvas, AText, R1, ALeft, ARightToLeft, ANoPrefix);
  end
  else
    DrawTextMultiLine(B.Canvas, AText, R, ALeft, ARightToLeft, ANoPrefix);
  Bitmap_ClearAlpha(B, 255);
  CreateGlowBitmapFromMask(B, AGlowSize, AGlowColor);
  Bitmap_DrawAlpha_XY(B, ACanvas,
    ARect.Left + AGlowDistance - AGlowSize, ARect.Top + AGlowDistance - AGlowSize, AGlowAlpha);
  ACanvas.Brush.Style := bsClear;
  Result := DrawTextMultiLine(ACanvas, AText, ARect, ALeft, ARightToLeft, ANoPrefix);
  B.Free;
end;

procedure DrawTextWithGlowBuffer(var ABuffer: TBitmap; AUpdateBuffer: Boolean; ACanvas: TCanvas; ARect: TRect; AText: string; AFlags: cardinal;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
  ARightToLeft: Boolean; ANoPrefix: Boolean);
var
  R, R1: TRect;
begin
  if ANoPrefix then
    AFlags := AFlags or DT_NOPREFIX;
  if AUpdateBuffer then
  begin
    if ABuffer <> nil then
    begin
      ABuffer.Free;
      ABuffer := nil;
    end;
  end;
  if ABuffer = nil then
  begin
    if ARect.Width < AGlowSize * 2 then ARect.Right := ARect.Left + AGlowSize * 2;
    if ARect.Height < AGlowSize * 2 then ARect.Bottom := ARect.Top + AGlowSize * 2;
    ABuffer := TBitmap.Create;
    ABuffer.SetSize(ARect.Width + AGlowSize * 2, ARect.Height + AGlowSize * 2);
    ABuffer.PixelFormat := pf32bit;
    ABuffer.Canvas.Font.Assign(ACanvas.Font);
    ABuffer.Canvas.Font.Color := clWhite;
    Bitmap_Clear(ABuffer, scColor(clBlack, 255));
    R := Rect(AGlowSize, AGlowSize, ABuffer.Width - AGlowSize, ABuffer.Height - AGlowSize);
    ABuffer.Canvas.Brush.Style := bsClear;
    if AIntensiveGlow then
    begin
      R1 := R;
      OffsetRect(R1, -1, 0);
      DrawText(ABuffer.Canvas.Handle, PChar(AText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
      OffsetRect(R1, 2, 0);
      DrawText(ABuffer.Canvas.Handle, PChar(AText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
      OffsetRect(R1, -1, -1);
      DrawText(ABuffer.Canvas.Handle, PChar(AText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
      OffsetRect(R1, 0, 2);
      DrawText(ABuffer.Canvas.Handle, PChar(aText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
    end
    else
      DrawText(ABuffer.Canvas.Handle, PChar(AText), Length(AText), R, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
    Bitmap_ClearAlpha(ABuffer, 255);
    CreateGlowBitmapFromMask(ABuffer, AGlowSize, AGlowColor);
  end;
  Bitmap_DrawAlpha_XY(ABuffer, ACanvas,
   ARect.Left + AGlowDistance - AGlowSize, ARect.Top + AGlowDistance - AGlowSize, AGlowAlpha);
  ACanvas.Brush.Style := bsClear;
  DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
end;


procedure DrawTextWithGlow(ACanvas: TCanvas; ARect: TRect; AText: string; AFlags: cardinal;
     AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
     ARightToLeft: Boolean; ANoPrefix: Boolean);
var
  R, R1: TRect;
  B: TBitmap;
begin
  if ARect.Width < AGlowSize * 2 then ARect.Right := ARect.Left + AGlowSize * 2;
  if ARect.Height < AGlowSize * 2 then ARect.Bottom := ARect.Top + AGlowSize * 2;

  if ANoPrefix then
    AFlags := AFlags or DT_NOPREFIX;

  B := TBitmap.Create;
  B.SetSize(ARect.Width + AGlowSize * 2, ARect.Height + AGlowSize * 2);
  B.PixelFormat := pf32bit;
  B.Canvas.Font.Assign(ACanvas.Font);
  B.Canvas.Font.Color := clWhite;
  Bitmap_Clear(B, scColor(clBlack, 255));
  R := Rect(AGlowSize, AGlowSize, B.Width - AGlowSize, B.Height - AGlowSize);
  B.Canvas.Brush.Style := bsClear;
  if AIntensiveGlow then
  begin
    R1 := R;
    OffsetRect(R1, -1, 0);
    DrawText(B.Canvas.Handle, PChar(AText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
    OffsetRect(R1, 2, 0);
    DrawText(B.Canvas.Handle, PChar(AText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
    OffsetRect(R1, -1, -1);
    DrawText(B.Canvas.Handle, PChar(AText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
    OffsetRect(R1, 0, 2);
    DrawText(B.Canvas.Handle, PChar(aText), Length(AText), R1, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
  end
  else
    DrawText(B.Canvas.Handle, PChar(AText), Length(AText), R, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
  Bitmap_ClearAlpha(B, 255);
  CreateGlowBitmapFromMask(B, AGlowSize, AGlowColor);
  Bitmap_DrawAlpha_XY(B, ACanvas,
    ARect.Left + AGlowDistance - AGlowSize, ARect.Top + AGlowDistance - AGlowSize, AGlowAlpha);
  ACanvas.Brush.Style := bsClear;
  DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect, scDrawTextBidiModeFlags(AFlags, ARightToLeft));
  B.Free;
end;

procedure DrawImageWithGlow(ACanvas: TCanvas; AX, AY: Integer; AImageList: TCustomImageList; AImageIndex: Integer;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AGlowAlpha: Byte);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  CreateGlowBitmapFromImageList(B, AImageList, AImageIndex, AGlowSize, AGlowColor);
  Bitmap_DrawAlpha_XY(B, ACanvas,
    AX + AGlowDistance - AGlowSize, AY + AGlowDistance - AGlowSize, AGlowAlpha);
  AImageList.Draw(ACanvas, AX, AY, AImageIndex, True);
  B.Free;
end;

procedure DrawImageWithGlowBuffer(var ABuffer: TBitmap; AUpdateBuffer: Boolean;
  ACanvas: TCanvas; AX, AY: Integer; AImageList: TCustomImageList; AImageIndex: Integer;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Integer; AGlowAlpha: Byte);
begin
  if AUpdateBuffer then
  begin
    if ABuffer <> nil then
    begin
      ABuffer.Free;
      ABuffer := nil;
    end;
  end;
  if ABuffer = nil then
  begin
    ABuffer := TBitmap.Create;
    CreateGlowBitmapFromImageList(ABuffer, AImageList, AImageIndex, AGlowSize, AGlowColor);
  end;
  Bitmap_DrawAlpha_XY(ABuffer, ACanvas,
    AX + AGlowDistance - AGlowSize, AY + AGlowDistance - AGlowSize, AGlowAlpha);
  AImageList.Draw(ACanvas, AX, AY, AImageIndex, True);
end;

function DrawImageAndTextEllipsesWithGlow(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
            ALayout: TButtonLayout;
            AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
            AEnabled: Boolean; ARightToLeft: Boolean; ALeft: Boolean;
            AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
            AImageGlow: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1): Boolean;
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY: Integer;
  TR: TRect;
begin
  Result := False;
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        if ANoPrefix then
          DrawText(Handle, PChar(AText), Length(AText), TR,
             DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT or DT_NOPREFIX)
        else
          DrawText(Handle, PChar(AText), Length(AText), TR,
             DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord2(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      Result := DrawTextMultiLineGlow(ACanvas, AText, TR, ALeft, ARightToLeft,
         AGlowDistance, AGlowColor, AGlowSize, AIntensiveGlow, AGlowAlpha, ANoPrefix);
    end;
  if gw <> 0
  then
    begin
      if AImageGlow then
        DrawImageWithGlow(ACanvas, GX, GY, AImageList, AImageIndex,
           AGlowDistance, AGlowColor, AGlowSize, AGlowAlpha)
      else
        if AEnabled then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
        else
        if AImageList is TscCustomImageList then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
        else
          DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
    end;
end;

procedure DrawImageAndTextWithGlow(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
  ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
  AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
  AImageGlow: Boolean;
  ADrawFocusRect: Boolean;
  ARightToLeft: Boolean;  ANoPrefix: Boolean; AScaleDrawFactor: Double = 1);
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY, I: Integer;
  TR: TRect;
begin
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        if ANoPrefix then
          DrawText(Handle, PChar(AText), Length(AText), TR,
            DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT or DT_NOPREFIX)
        else
           DrawText(Handle, PChar(AText), Length(AText), TR,
            DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      DrawTextWithGlow(ACanvas, TR, AText, DT_EXPANDTABS or DT_VCENTER or DT_CENTER or DT_WORDBREAK or DT_NOCLIP,
         AGlowDistance, AGlowColor, AGlowSize, AIntensiveGlow, AGlowAlpha, ARightToLeft, ANoPrefix);
      if ADrawFocusRect then
        scDrawFocusRect(ACanvas, Rect(TR.Left - 2, TR.Top - 2, TR.Right + 2, TR.Bottom + 2), AScaleDrawFactor);
    end;
  if gw <> 0
  then
    begin
      if AImageGlow then
        DrawImageWithGlow(ACanvas, GX, GY, AImageList, AImageIndex,
           AGlowDistance, AGlowColor, AGlowSize, AGlowAlpha)
      else
        if AEnabled then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
        else
        if AImageList is TscCustomImageList then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
        else
          DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
      if ADrawColorMarker
      then
        with ACanvas do
        begin
          Pen.Color := AColorMarkerValue;
          for I := 0 to Trunc(2 * AScaleDrawFactor) do
          begin
            MoveTo(GX, GY + AImageList.Height + I);
            LineTo(GX + AImageList.Width, GY + AImageList.Height + I);
          end;
        end;
      if ADrawFocusRect and (AText = '') then
        scDrawFocusRect(ACanvas, Rect(GX - 2, GY - 2, GX + AImageList.Width + 2, GY + AImageList.Height + 2), AScaleDrawFactor);
    end
  else
    if ADrawColorMarker
    then
      with ACanvas do
      begin
        if AText <> ''
        then
          begin
            Pen.Color := AColorMarkerValue;
            for I := 0 to Trunc(2 * AScaleDrawFactor) do
            begin
              MoveTo(TR.Left, TR.Bottom + I);
              LineTo(TR.Right, TR.Bottom + I);
            end;
          end
        else
          begin
            Brush.Color := AColorMarkerValue;
            Brush.Style := bsSolid;
            FillRect(Rect(ARect.Left + 4, ARect.Top + 4, ARect.Right - 2, ARect.Bottom - 4));
          end;
     end;
end;

procedure DrawImageAndTextWithGlowBuffer(var ABuffer, AImageBuffer: TBitmap; AUpdateBuffer: Boolean;
  ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
  ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
  AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
  AImageGlow: Boolean;
  ADrawFocusRect: Boolean;
  ARightToLeft: Boolean;  ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY, I: Integer;
  TR: TRect;
  Flags: Longint;
begin
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        Flags := DT_EXPANDTABS or DT_CALCRECT;
        if ANoPrefix then
          Flags := Flags or DT_NOPREFIX;
        if AWordWrap then
          Flags := Flags or DT_WORDBREAK;
        DrawText(Handle, PChar(AText), Length(AText), TR, Flags);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      Flags := DT_EXPANDTABS or DT_VCENTER or DT_CENTER or DT_NOCLIP;
      if ANoPrefix then
         Flags := Flags or DT_NOPREFIX;
      if AWordWrap then
         Flags := Flags or DT_WORDBREAK;
      DrawTextWithGlowBuffer(ABuffer, AUpdateBuffer, ACanvas, TR, AText, Flags,
         AGlowDistance, AGlowColor, AGlowSize, AIntensiveGlow, AGlowAlpha, ARightToLeft, ANoPrefix);
      if ADrawFocusRect then
        scDrawFocusRect(ACanvas, Rect(TR.Left - 2, TR.Top - 2, TR.Right + 2, TR.Bottom + 2),
        AScaleDrawFactor);
    end;
  if gw <> 0
  then
    begin
      if AImageGlow then
        DrawImageWithGlowBuffer(AImageBuffer, AUpdateBuffer,
           ACanvas, GX, GY, AImageList, AImageIndex,
           AGlowDistance, AGlowColor, AGlowSize, AGlowAlpha)
      else
        if AEnabled then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
        else
        if AImageList is TscCustomImageList then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
        else
          DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
      if ADrawColorMarker
      then
        with ACanvas do
        begin
          Pen.Color := AColorMarkerValue;
          for I := 0 to Trunc(2 * AScaleDrawFactor) do
          begin
            MoveTo(GX, GY + AImageList.Height + I);
            LineTo(GX + AImageList.Width, GY + AImageList.Height + I);
          end;
        end;
      if ADrawFocusRect and (AText = '') then
        scDrawFocusRect(ACanvas, Rect(GX - 2, GY - 2, GX + AImageList.Width + 2, GY + AImageList.Height + 2),
        AScaleDrawFactor);
    end
  else
    if ADrawColorMarker
    then
      with ACanvas do
      begin
        if AText <> ''
        then
          begin
            Pen.Color := AColorMarkerValue;
            for I := 0 to Trunc(2 * AScaleDrawFactor) do
            begin
              MoveTo(TR.Left, TR.Bottom + I);
              LineTo(TR.Right, TR.Bottom + I);
            end;
          end
        else
          begin
            Brush.Color := AColorMarkerValue;
            Brush.Style := bsSolid;
            FillRect(Rect(ARect.Left + 4, ARect.Top + 4, ARect.Right - 2, ARect.Bottom - 4));
          end;
     end;
end;

procedure DrawImageAndTextWithGlowBuffer2(var ABuffer, AImageBuffer: TBitmap; AUpdateBuffer: Boolean;
  ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
  ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
  AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
  AImageGlow: Boolean;
  ADrawFocusRect: Boolean;
  ARightToLeft: Boolean; ANoPrefix: Boolean; AScaleDrawFactor: Double = 1; AWordWrap: Boolean = True);
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY, I: Integer;
  TR: TRect;
  Flags: Longint;
begin
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        Flags := DT_EXPANDTABS or DT_CALCRECT;
        if ANoPrefix then
          Flags := Flags or DT_NOPREFIX;
        if AWordWrap then
          Flags := Flags or DT_WORDBREAK;
        DrawText(Handle, PChar(AText), Length(AText), TR, Flags);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      Flags := DT_VCENTER  or DT_NOCLIP;
      if ANoPrefix then
         Flags := Flags or DT_NOPREFIX;
      if AWordWrap then
         Flags := Flags or DT_WORDBREAK;
      DrawTextWithGlowBuffer(ABuffer, AUpdateBuffer, ACanvas, TR, AText, Flags,
         AGlowDistance, AGlowColor, AGlowSize, AIntensiveGlow, AGlowAlpha, ARightToLeft, ANoPrefix);
      if ADrawFocusRect then
       scDrawFocusRect(ACanvas, Rect(TR.Left - 2, TR.Top - 2, TR.Right + 2, TR.Bottom + 2),
         AScaleDrawFactor);
    end;
  if gw <> 0
  then
    begin
      if AImageGlow then
        DrawImageWithGlowBuffer(AImageBuffer, AUpdateBuffer, ACanvas, GX, GY, AImageList, AImageIndex,
           AGlowDistance, AGlowColor, AGlowSize, AGlowAlpha)
      else
        if AEnabled then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
        else
        if AImageList is TscCustomImageList then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
       else
          DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
      if ADrawColorMarker
      then
        with ACanvas do
        begin
          Pen.Color := AColorMarkerValue;
          for I := 0 to Trunc(2 * AScaleDrawFactor) do
          begin
            MoveTo(GX, GY + AImageList.Height + I);
            LineTo(GX + AImageList.Width, GY + AImageList.Height + I);
          end;
        end;
     if ADrawFocusRect and (AText = '') then
       scDrawFocusRect(ACanvas, Rect(GX - 2, GY - 2, GX + AImageList.Width + 2, GY + AImageList.Height + 2),
       AScaleDrawFactor);
    end
  else
    if ADrawColorMarker
    then
      with ACanvas do
      begin
        if AText <> ''
        then
          begin
            Pen.Color := AColorMarkerValue;
            for I := 0 to Trunc(2 * AScaleDrawFactor) do
            begin
              MoveTo(TR.Left, TR.Bottom + I);
              LineTo(TR.Right, TR.Bottom + I);
            end;
          end
        else
          begin
            Brush.Color := AColorMarkerValue;
            Brush.Style := bsSolid;
            FillRect(Rect(ARect.Left + 4, ARect.Top + 4, ARect.Right - 2, ARect.Bottom - 4));
          end;
     end;
end;

procedure DrawImageAndTextWithGlow2(ACanvas: TCanvas; ARect: TRect; AMargin, ASpacing: Integer;
  ALayout: TButtonLayout; AText: String; AImageIndex: Integer; AImageList: TCustomImageList;
  AEnabled: Boolean; ADrawColorMarker: Boolean; AColorMarkerValue: TColor;
  AGlowDistance: Integer; AGlowColor: TColor; AGlowSize: Byte; AIntensiveGlow: Boolean; AGlowAlpha: Byte;
  AImageGlow: Boolean;
  ADrawFocusRect: Boolean;
  ARightToLeft: Boolean;  ANoPrefix: Boolean; AScaleDrawFactor: Double = 1);
var
  gw, gh: Integer;
  tw, th: Integer;
  TX, TY, GX, GY, I: Integer;
  TR: TRect;
begin
  if (AImageIndex < 0) or (AImageList = nil) or (AImageIndex >= AImageList.Count)
  then
    begin
      gw := 0;
      gh := 0;
      ASpacing := 0;
    end
  else
    begin
      gw := AImageList.Width;
      gh := AImageList.Height;
    end;
  with ACanvas do
  begin
    if AText = ''
    then
      begin
        tw := 0;
        th := 0;
        ASpacing := 0;
      end
    else
      begin
        TR := Rect(0, 0, ARect.Width, ARect.Height);
        if (ALayout = blGlyphLeft) or (ALayout = blGlyphRight)
        then
          begin
            Dec(TR.Right, gw + ASpacing);
            if AMargin > 0 then Dec(TR.Right, AMargin);
          end
        else
        if (ALayout = blGlyphTop) or (ALayout = blGlyphBottom)
        then
          begin
            Dec(TR.Bottom, gh + ASpacing);
          end;
        if ANoPrefix then
          DrawText(Handle, PChar(AText), Length(AText), TR,
           DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT or DT_NOPREFIX)
        else
           DrawText(Handle, PChar(AText), Length(AText), TR,
             DT_EXPANDTABS or DT_WORDBREAK or DT_CALCRECT);
        tw := TR.Width;
        th := TR.Height;
      end;
    Brush.Style := bsClear;
  end;
  CalcLCoord(ALayout, ARect, gw, gh, tw, th, ASpacing, AMargin, TX, TY, GX, GY);
  if AText <> ''
  then
    begin
      TR := Rect(TX, TY, TX + tw, TY + th);
      DrawTextWithGlow(ACanvas, TR, AText, DT_EXPANDTABS or DT_VCENTER  or DT_WORDBREAK or DT_NOCLIP,
         AGlowDistance, AGlowColor, AGlowSize, AIntensiveGlow, AGlowAlpha, ARightToLeft,  ANoPrefix);
      if ADrawFocusRect then
       scDrawFocusRect(ACanvas, Rect(TR.Left - 2, TR.Top - 2, TR.Right + 2, TR.Bottom + 2),
       AScaleDrawFactor);
    end;
  if gw <> 0
  then
    begin
      if AImageGlow then
        DrawImageWithGlow(ACanvas, GX, GY, AImageList, AImageIndex,
           AGlowDistance, AGlowColor, AGlowSize, AGlowAlpha)
      else
        if AEnabled then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, True)
        else
        if AImageList is TscCustomImageList then
          AImageList.Draw(ACanvas, GX, GY, AImageIndex, False)
        else
           DrawBitmapFromImageList(ACanvas, GX, GY,  AImageList, AImageIndex, DisabledImageAlphaValue);
      if ADrawColorMarker
      then
        with ACanvas do
        begin
          Pen.Color := AColorMarkerValue;
          for I := 0 to Trunc(2 * AScaleDrawFactor) do
          begin
            MoveTo(GX, GY + AImageList.Height + I);
            LineTo(GX + AImageList.Width, GY + AImageList.Height + I);
          end;
        end;
     if ADrawFocusRect and (AText = '') then
       scDrawFocusRect(ACanvas, Rect(GX - 2, GY - 2, GX + AImageList.Width + 2, GY + AImageList.Height + 2),
       AScaleDrawFactor);
    end
  else
    if ADrawColorMarker
    then
      with ACanvas do
      begin
        if AText <> ''
        then
          begin
            Pen.Color := AColorMarkerValue;
            for I := 0 to Trunc(2 * AScaleDrawFactor) do
            begin
              MoveTo(TR.Left, TR.Bottom + I);
              LineTo(TR.Right, TR.Bottom + I);
            end;
          end
        else
          begin
            Brush.Color := AColorMarkerValue;
            Brush.Style := bsSolid;
            FillRect(Rect(ARect.Left + 4, ARect.Top + 4, ARect.Right - 2, ARect.Bottom - 4));
          end;
     end;
end;

procedure DrawHorzSplitter(ACanvas: TCanvas; ARect: TRect);
var
  Buffer: TBitmap;
  C1, C2: TColor;
begin
  Buffer := TBitmap.Create;
  Buffer.SetSize(ARect.Width, 2);
  Buffer.PixelFormat := pf32Bit;
  if StyleServices.Enabled then
  begin
    C1 := StyleServices.GetSystemColor(clBtnShadow);
    C2 := StyleServices.GetSystemColor(clBtnHighLight);
  end
  else
  begin
    C1 := clBtnShadow;
    C2 := clBtnHighLight;
  end;
  with Buffer.Canvas do
  begin
    Pen.Color := C1;
    MoveTo(0, 0);
    LineTo(Buffer.Width, 0);
    Pen.Color := C2;
    MoveTo(0, 1);
    LineTo(Buffer.Width, 1);
  end;
  Bitmap_ClearAlpha(Buffer, 255);
  Bitmap_DrawAlpha_XY(Buffer, ACanvas, ARect.Left, ARect.Top, 125);
  Buffer.Free;
end;

procedure DrawVertSplitter(ACanvas: TCanvas; ARect: TRect; AIsRightToLeft: Boolean = False);
var
  Buffer: TBitmap;
  C1, C2: TColor;
begin
  Buffer := TBitmap.Create;
  Buffer.SetSize(2, ARect.Height);
  Buffer.PixelFormat := pf32Bit;
  if StyleServices.Enabled then
  begin
    C1 := StyleServices.GetSystemColor(clBtnShadow);
    C2 := StyleServices.GetSystemColor(clBtnHighLight);
  end
  else
  begin
    C1 := clBtnShadow;
    C2 := clBtnHighLight;
  end;
  with Buffer.Canvas do
  begin
    Pen.Color := C1;
    MoveTo(0, 0);
    LineTo(0, Buffer.Height);
    Pen.Color := C2;
    MoveTo(1, 0);
    LineTo(1, Buffer.Height);
  end;
  Bitmap_ClearAlpha(Buffer, 255);
  if not AIsRightToLeft then
    Bitmap_DrawAlpha_XY(Buffer, ACanvas, ARect.Left, ARect.Top, 125)
  else
    Bitmap_DrawAlpha_XY(Buffer, ACanvas, ARect.Right - 2, ARect.Top, 125);

  Buffer.Free;
end;

procedure DrawVertSplitter2(ACanvas: TCanvas; ARect: TRect);
var
  Buffer: TBitmap;
  C1, C2: TColor;
begin
  Buffer := TBitmap.Create;
  Buffer.SetSize(2, ARect.Height);
  Buffer.PixelFormat := pf32Bit;
  if StyleServices.Enabled then
  begin
    C1 := StyleServices.GetSystemColor(clBtnShadow);
    C2 := StyleServices.GetSystemColor(clBtnHighLight);
  end
  else
  begin
    C1 := clBtnShadow;
    C2 := clBtnHighLight;
  end;
  with Buffer.Canvas do
  begin
    Pen.Color := C1;
    MoveTo(0, 0);
    LineTo(0, Buffer.Height);
    Pen.Color := C2;
    MoveTo(1, 0);
    LineTo(1, Buffer.Height);
  end;
  Bitmap_ClearAlpha(Buffer, 255);
  Bitmap_DrawAlpha_XY(Buffer, ACanvas, ARect.Left, ARect.Top, 180);
  Buffer.Free;
end;

procedure DrawTabCloseImage(C: TCanvas; R: TRect; Color: TColor; AScaleFactor: Double = 1);
var
  FOldPen: HPEN;
  X, Y, I: Integer;
  Size: Integer;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  FOldPen := SelectObject(C.Handle, GetStockObject(DC_PEN));
  SetDCPenColor(C.Handle, ColorToRGB(Color));
  Size := Round(8 * AScaleFactor);
  X := R.Left + R.Width div 2 - Size div 2 - 1;
  Y := R.Top + R.Height div 2 - Size div 2;
  with C do
  begin
    for I := 1 to Round(AScaleFactor * 2) do
    begin
      MoveTo(X, Y); LineTo(X + Size + 1 , Y + Size + 1);
      MoveTo(X + Size, Y); LineTo(X - 1, Y + Size + 1);
      Inc(X);
    end;
  end;
  SelectObject(C.Handle, FOldPen);
end;

function CreateNullRgn: HRGN;
begin
  Result := CreateRectRgn(-1, -1, -1, -1);
end;

procedure DrawStrecthBitmap(ALeftOffset, ATopOffset, ARightOffset, ABottomOffset: Integer;
  ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect);
var
  LTPt, RTPt, LBPt, RBPt: TPoint;
  NewLTPt, NewRTPt, NewLBPt, NewRBPt: TPoint;
  ClRect, NewClRect: TRect;
  R, R1, R2, R3: TRect;
  Buffer: TBitmap;
begin
  ClRect := Rect(ALeftOffset, ATopOffset, ABitmap.Width - ARightOffset, ABitmap.Height - ABottomOffset);
  LtPt := Point(ClRect.Left, ClRect.Top);
  RtPt := Point(ClRect.Right, ClRect.Top);
  LBPt := Point(ClRect.Left, ClRect.Bottom);
  RBPt := Point(ClRect.Right, ClRect.Bottom);

  NewClRect := ClRect;
  NewClRect.Right := ARect.Width - (ABitmap.Width - ClRect.Right);
  NewClRect.Bottom := ARect.Height - (ABitmap.Height - ClRect.Bottom);

  OffsetRect(NewClRect, ARect.Left, ARect.Top);

  NewLtPt := Point(NewClRect.Left, NewClRect.Top);
  NewRtPt := Point(NewClRect.Right, NewClRect.Top);
  NewLBPt := Point(NewClRect.Left, NewClRect.Bottom);
  NewRBPt := Point(NewClRect.Right, NewClRect.Bottom);

  R := Rect(0, 0, ABitmap.Width, ABitmap.Height);

  R1 := Rect(NewLTPt.X, ARect.Top, NewRTPt.X, NewClRect.Top);
  R2 := Rect(R.Left + LTPt.X, R.Top, R.Left + RTPt.X, R.Top + ClRect.Top);
  Buffer := TBitMap.Create;
  Buffer.Width := R2.Width;
  Buffer.Height := R2.Height;
  R3 := Rect(0, 0, Buffer.Width, Buffer.Height);
  Buffer.Canvas.CopyRect(R3, ABitmap.Canvas, R2);
  if R1.Width * R1.Height > 0 then
    ACanvas.StretchDraw(R1, Buffer);
  Buffer.Free;

  R1 := Rect(NewLBPt.X, NewClRect.Bottom, NewRBPt.X, ARect.Bottom);
  R2 := Rect(R.Left + LBPt.X, R.Top + ClRect.Bottom, R.Left + RBPt.X, R.Bottom);
  Buffer := TBitMap.Create;
  Buffer.Width := R2.Width;
  Buffer.Height := R2.Height;
  R3 := Rect(0, 0, Buffer.Width, Buffer.Height);
  Buffer.Canvas.CopyRect(R3, ABitmap.Canvas, R2);
  if R1.Width * R1.Height > 0 then
    ACanvas.StretchDraw(R1, Buffer);
  Buffer.Free;

  R1 := Rect(ARect.Left, NewLTPt.Y, NewClRect.Left, NewLBPt.Y);
  R2 := Rect(R.Left, R.Top + LtPt.Y, R.Left + ClRect.Left, R.Top + LBPt.Y);
  Buffer := TBitMap.Create;
  Buffer.Width := R2.Width;
  Buffer.Height := R2.Height;
  R3 := Rect(0, 0, Buffer.Width, Buffer.Height);
  Buffer.Canvas.CopyRect(R3, ABitmap.Canvas, R2);
  if R1.Width * R1.Height > 0 then
    ACanvas.StretchDraw(R1, Buffer);
  Buffer.Free;

  R1 := Rect(NewClRect.Right, NewRTPt.Y, ARect.Right, NewRBPt.Y);
  R2 := Rect(R.Left + ClRect.Right, R.Top + RtPt.Y, R.Right, R.Top + RBPt.Y);
  Buffer := TBitMap.Create;
  Buffer.Width := R2.Width;
  Buffer.Height := R2.Height;
  R3 := Rect(0, 0, Buffer.Width, Buffer.Height);
  Buffer.Canvas.CopyRect(R3, ABitmap.Canvas, R2);
  if R1.Width * R1.Height > 0 then
    ACanvas.StretchDraw(R1, Buffer);
  Buffer.Free;

  ACanvas.CopyRect(Rect(ARect.Left, ARect.Top, NewLTPt.X, NewLTPt.Y),
    ABitmap.Canvas, Rect(R.Left, R.Top,
      R.Left + LTPt.X, R.Top + LTPt.Y));

  ACanvas.CopyRect(Rect(ARect.Left, NewLBPt.Y, NewLBPt.X, ARect.Bottom),
    ABitmap.Canvas, Rect(R.Left, LBPt.Y, LBPt.X, R.Bottom));

  ACanvas.CopyRect(Rect(NewRTPt.X, ARect.Top, ARect.Right, NewRTPt.Y),
    ABitmap.Canvas, Rect(RTPt.X, R.Top, R.Right, RTPt.Y));

  ACanvas.CopyRect(Rect(NewRBPt.X, NewRBPt.Y, ARect.Right, ARect.Bottom),
    ABitmap.Canvas, Rect(RBPt.X, RBPt.Y, R.Right, R.Bottom));

  Buffer := TBitMap.Create;
  Buffer.Width := ClRect.Width;
  Buffer.Height := ClRect.Height;
  Buffer.Canvas.CopyRect(Rect(0, 0, Buffer.Width, Buffer.Height),
    ABitMap.Canvas, Rect(R.Left + ClRect.Left, R.Top + ClRect.Top,
    R.Left + ClRect.Right, R.Top + ClRect.Bottom));
  if NewClRect.Width * NewClRect.Height > 0 then
    ACanvas.StretchDraw(NewClRect, Buffer);
  Buffer.Free;
end;

function EqPoints(const Pt1, Pt2: TPoint): Boolean;
begin
  Result := (Pt1.X = Pt2.X) and (Pt1.Y = Pt2.Y);
end;

procedure Frm3D(ACanvas: TCanvas; var ARect: TRect; ATopColor, ABottomColor: TColor);
var
  R: TRect;

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
    OldPen: HPEN;
  begin
    with ACanvas, R do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;

      OldPen := SelectObject(ACanvas.Handle, GetStockObject(DC_PEN));

      SetDCPenColor(ACanvas.Handle, ColorToRGB(ATopColor));
      PolyLine([BottomLeft, TopLeft, TopRight]);

      Dec(BottomLeft.X);
      SetDCPenColor(ACanvas.Handle, ColorToRGB(ABottomColor));
      PolyLine([TopRight, BottomRight, BottomLeft]);

      SelectObject(ACanvas.Handle, OldPen);
    end;
  end;

begin
  ACanvas.Pen.Width := 1;
  R := ARect;
  Dec(R.Bottom);
  Dec(R.Right);
  DoRect;
  InflateRect(ARect, -1, -1);
end;

procedure DrawParentBackground(AControl: TWinControl; ADest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
  FCanvas: TCanvas;
  P1, P2: TPoint;
begin
  if (AControl = nil) or (AControl.Parent = nil) then Exit;

  if (AControl.Parent.ClassName = 'TscStatusBar') then
  begin
    R := Rect(0, 0, AControl.Width, AControl.Height);
    Dec(R.Top, AControl.Top);
    Dec(R.Left, 5);
    Inc(R.Right, 5);
    Inc(R.Bottom, AControl.Parent.Height - AControl.Top - AControl.Height);
    DrawStatusBar(ADest, R);
    DrawStatusPanel(ADest, R);
    Exit;
  end;

  if (AControl.Parent.ClassName = 'TTabPage') and (AControl.Parent.Parent <> nil) and
     (AControl.Parent.Parent.ClassName = 'TTabbedNotebook') then
  begin
    DC := ADest.Handle;
    FCanvas := TCanvas.Create;
    FCanvas.Handle := DC;
    try
      DrawTabFrame(FCanvas, Rect(-AControl.Parent.Left, -AControl.Parent.Top,
        AControl.Parent.ClientWidth + AControl.Parent.Left,
        AControl.Parent.ClientHeight + AControl.Parent.Top));
    finally
      FCanvas.Handle := 0;
      FCanvas.Free;
    end;
    Exit;
  end;

  if IsCustomStyle then
  begin
    StyleServices.DrawParentBackground(AControl.Handle, ADest.Handle, nil, False);
    Exit;
  end;

  Count := AControl.Parent.ControlCount;
  DC := ADest.Handle;
  with AControl.Parent do ControlState := ControlState + [csPaintCopy];
  try
    with AControl do begin
      SelfR := Bounds(Left, Top, Width, Height);
      X := -Left; Y := -Top;
      P1 := Point(0, 0);
      P2 := Point(AControl.Left, AControl.Top);
      P1 := AControl.ClientToScreen(P1);
      P2 := AControl.Parent.ClientToScreen(P2);
      P1.X := P1.X - P2.X;
      P1.Y := P1.Y - P2.Y;
      X := X - P1.X;
      Y := Y - P1.Y;
    end;
    SaveIndex := SaveDC(DC);
     try
       SetViewportOrgEx(DC, X, Y, nil);
       IntersectClipRect(DC, 0, 0, AControl.Parent.ClientWidth,
         AControl.Parent.ClientHeight);
      if (AControl.Parent.ClassName = 'TscTabSheet') then
      begin
        AControl.Parent.Perform(WM_ERASEBKGND, DC, 0);
      end
      else
      if ((AControl.Parent is TTabSheet) and StyleServices.Enabled and
         (csDesigning in AControl.ComponentState) and
         (TTabSheet(AControl.Parent).PageControl <> nil) and
         (TTabSheet(AControl.Parent).PageControl.Style = tsTabs)) or
         ((AControl.Parent.ClassName = 'TTabPage') and StyleServices.Enabled) then
      begin
        FCanvas := TCanvas.Create;
        FCanvas.Handle := DC;
        try
          DrawTabFrame(FCanvas, Rect(-AControl.Parent.Left, -AControl.Parent.Top,
            AControl.Parent.ClientWidth + AControl.Parent.Left,
            AControl.Parent.ClientHeight + AControl.Parent.Top));
        finally
          FCanvas.Handle := 0;
          FCanvas.Free;
        end;
      end
      else
      if (AControl.Parent is TFrame) and TFrame(AControl.Parent).ParentBackground
      then
      begin
        if AControl.Parent.Parent <> nil then
        begin
          SetViewportOrgEx(DC,
            X - AControl.Parent.Left,
             Y - AControl.Parent.Top, nil);
          AControl.Parent.Parent.Perform(WM_ERASEBKGND, DC, 0);
          SetViewportOrgEx(DC, X, Y, nil);
        end;
      end
      else
      begin
        with TParentControl(AControl.Parent) do begin
          Perform(WM_ERASEBKGND, DC, 0);
         if not (AControl.Parent is TCustomForm) then PaintWindow(DC);
        end;
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;

    for I := 0 to Count - 1 do
    if AControl.Parent.Controls[I] <> AControl
    then
    begin
      if (AControl.Parent.Controls[I] <> nil) and
         (AControl.Parent.Controls[I] is TGraphicControl)
      then
      begin
        with TGraphicControl(AControl.Parent.Controls[I]) do begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SaveIndex := SaveDC(DC);
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
              IntersectClipRect(DC, 0, 0, Width, Height);
              Perform(WM_PAINT, DC, 0);
            finally
              RestoreDC(DC, SaveIndex);
              ControlState := ControlState - [csPaintCopy];
            end;
          end;
        end;
      end
    end
    else
      Break;
  finally
    with AControl.Parent do ControlState := ControlState - [csPaintCopy];
  end;
end;

procedure DrawTreeExpandImageColor(ACanvas: TCanvas; ARect: TRect; AExpanded: Boolean; AColor, AGlyphColor: TColor; AScaleFactor: Double = 1);
var
  Size: Integer;
  Buffer: TBitmap;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  Size := Round(8 * AScaleFactor) + 1;
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Size, Size);
    with Buffer.Canvas do
    begin
      Brush.Color := AColor;
      Pen.Color := AlternateColor(AColor);
      Rectangle(0, 0, Buffer.Width, Buffer.Height);
      Pen.Color := AGlyphColor;
      MoveTo(2, Size div 2);
      LineTo(Buffer.Width - 2, Size div 2);
      if not AExpanded then
      begin
        MoveTo(Size div 2, 2);
        LineTo(Size div 2, Buffer.Height - 2);
      end;
      if AScaleFactor >=2 then
      begin
        MoveTo(2, Size div 2 - 1);
        LineTo(Buffer.Width - 2, Size div 2 - 1);
        MoveTo(2, Size div 2 + 1);
        LineTo(Buffer.Width - 2, Size div 2 + 1);
        if not AExpanded then
        begin
          MoveTo(Size div 2 - 1, 2);
          LineTo(Size div 2 - 1, Buffer.Height - 2);
          MoveTo(Size div 2 + 1, 2);
          LineTo(Size div 2 + 1, Buffer.Height - 2);
        end;
      end;
    end;
    ACanvas.Draw(ARect.Left + ARect.Width div 2 - Size div 2,
      ARect.Top + ARect.Height div 2 - Size div 2 + 1, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure DrawTreeExpandImage(ACanvas: TCanvas; ARect: TRect; AExpanded: Boolean; ABorderColor: TColor; AScaleFactor: Double = 1);
var
  Size: Integer;
  Buffer: TBitmap;
  C: TColor;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  Size := Round(8 * AScaleFactor) + 1;
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Size, Size);
    with Buffer.Canvas do
    begin
      Brush.Color := GetStyleColor(clBtnFace);
      Pen.Color := MiddleColor(ABorderColor, GetStyleColor(clBtnFace));
      Rectangle(0, 0, Buffer.Width, Buffer.Height);
      Pen.Color := GetStyleColor(clBtnHighLight);
      MoveTo(1,1); LineTo(Buffer.Width - 1, 1);
      C := MiddleColor(GetStyleColor(clBtnText), GetStyleColor(clBtnFace));
      Pen.Color := MiddleColor(GetStyleColor(clBtnText), C);
      MoveTo(2, Size div 2);
      LineTo(Buffer.Width - 2, Size div 2);
      if not AExpanded then
      begin
        MoveTo(Size div 2, 2);
        LineTo(Size div 2, Buffer.Height - 2);
      end;
      if AScaleFactor >=2 then
      begin
        MoveTo(2, Size div 2 - 1);
        LineTo(Buffer.Width - 2, Size div 2 - 1);
        MoveTo(2, Size div 2 + 1);
        LineTo(Buffer.Width - 2, Size div 2 + 1);
        if not AExpanded then
        begin
          MoveTo(Size div 2 - 1, 2);
          LineTo(Size div 2 - 1, Buffer.Height - 2);
          MoveTo(Size div 2 + 1, 2);
          LineTo(Size div 2 + 1, Buffer.Height - 2);
        end;
      end;
    end;
    ACanvas.Draw(ARect.Left + ARect.Width div 2 - Size div 2,
      ARect.Top + ARect.Height div 2 - Size div 2 + 1, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure DrawUpScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsArrowBtnUpNormal);
    case ACtrlState of
      scsHot:  Details := StyleServices.GetElementDetails(tsArrowBtnUpHot);
      scsPressed: Details := StyleServices.GetElementDetails(tsArrowBtnUpPressed);
      scsDisabled:  Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
  end
  else
    DrawUpSpinButton(ACanvas, ARect, ACtrlState);
end;

procedure DrawDownScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsArrowBtnDownNormal);
    case ACtrlState of
      scsHot: Details := StyleServices.GetElementDetails(tsArrowBtnDownHot);
      scsPressed: Details := StyleServices.GetElementDetails(tsArrowBtnDownPressed);
      scsDisabled: Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
  end
  else
    DrawDownSpinButton(ACanvas, ARect, ACtrlState);
end;

procedure DrawLeftScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}

  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsArrowBtnLeftNormal);
    case ACtrlState of
      scsHot:  Details := StyleServices.GetElementDetails(tsArrowBtnLeftHot);
      scsPressed: Details := StyleServices.GetElementDetails(tsArrowBtnLeftPressed);
      scsDisabled:  Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
  end
  else
    DrawLeftSpinButton(ACanvas, ARect, ACtrlState);
end;

procedure DrawRightScrollButton(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AScaleFactor: Double = 1);
var
  Details:  TThemedElementDetails;
  {$IFDEF VER330_UP}
  FDPI: Integer;
  {$ENDIF}
begin
  {$IFDEF VER330_UP}
  if AScaleFactor < 1 then AScaleFactor := 1;
  FDPI := Round(96 * AScaleFactor);
  {$ENDIF}
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsArrowBtnRightNormal);
    case ACtrlState of
      scsHot:  Details := StyleServices.GetElementDetails(tsArrowBtnRightHot);
      scsPressed: Details := StyleServices.GetElementDetails(tsArrowBtnRightPressed);
      scsDisabled:  Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect{$IFDEF VER330_UP},nil, FDPI{$ENDIF});
  end
  else
    DrawRightSpinButton(ACanvas, ARect, ACtrlState);
end;

procedure DrawHScrollThumb(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsThumbBtnHorzNormal);
    case ACtrlState of
      scsHot:  Details := StyleServices.GetElementDetails(tsThumbBtnHorzHot);
      scsPressed: Details := StyleServices.GetElementDetails(tsThumbBtnHorzPressed);
      scsDisabled:  Details := StyleServices.GetElementDetails(tsThumbBtnHorzDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    if not IsCustomStyle and (ARect.Width > 17) then
    begin
      Details := StyleServices.GetElementDetails(tsGripperHorzNormal);
      case ACtrlState of
        scsHot:  Details := StyleServices.GetElementDetails(tsGripperHorzHot);
        scsPressed: Details := StyleServices.GetElementDetails(tsGripperHorzPressed);
        scsDisabled:  Details := StyleServices.GetElementDetails(tsGripperHorzDisabled);
      end;
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    end;
  end
  else
    DrawButton(ACanvas, ARect, ACtrlState, False);
end;

procedure DrawVScrollThumb(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsThumbBtnVertNormal);
    case ACtrlState of
      scsHot:  Details := StyleServices.GetElementDetails(tsThumbBtnVertHot);
      scsPressed: Details := StyleServices.GetElementDetails(tsThumbBtnVertPressed);
      scsDisabled:  Details := StyleServices.GetElementDetails(tsThumbBtnVertDisabled);
    end;
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    if not IsCustomStyle and (ARect.Height > 17) then
    begin
      Details := StyleServices.GetElementDetails(tsGripperVertNormal);
      case ACtrlState of
        scsHot:  Details := StyleServices.GetElementDetails(tsGripperVertHot);
        scsPressed: Details := StyleServices.GetElementDetails(tsGripperVertPressed);
        scsDisabled:  Details := StyleServices.GetElementDetails(tsGripperVertDisabled);
      end;
      StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
    end;
  end
  else
    DrawButton(ACanvas, ARect, ACtrlState, False);
end;

procedure DrawHScrollFrame(ACanvas: TCanvas; ARect: TRect);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsUpperTrackHorzNormal);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(ARect);
  end;
end;

procedure DrawVScrollFrame(ACanvas: TCanvas; ARect: TRect);
var
  Details:  TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
    StyleServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(ARect);
  end;
end;

function GetCursorHeightMargin: Integer;
var
  IconInfo: TIconInfo;
  BitmapInfoSize, BitmapBitsSize, ImageSize: DWORD;
  Bitmap: PBitmapInfoHeader;
  Bits: Pointer;
  BytesPerScanline: Integer;
  {$IFDEF VER300_UP}
  M, D: Integer;
  {$ENDIF}
 
function FindScanline(Source: Pointer; MaxLen: Cardinal; Value: Cardinal): Cardinal;
var
  Ptr: PByte;
begin
  Result := MaxLen;
  if Result > 0 then
    Dec(Result);
  Ptr := Source;
  while (Result > 0) and (Ptr^ = Value) do
  begin
    Inc(Ptr);
    Dec(Result);
  end;
end;

begin
  Result := GetSystemMetrics(SM_CYCURSOR);
  if GetIconInfo(GetCursor, IconInfo) then
  try
    GetDIBSizes(IconInfo.hbmMask, BitmapInfoSize, BitmapBitsSize);
    Bitmap := AllocMem(DWORD(BitmapInfoSize) + BitmapBitsSize);
    try
      Bits := Pointer(DWORD(Bitmap) + BitmapInfoSize);
      if GetDIB(IconInfo.hbmMask, 0, Bitmap^, Bits^) and
        (Bitmap^.biBitCount = 1) then
      begin
        with Bitmap^ do
        begin
          BytesPerScanline := ((biWidth * biBitCount + 31) and not 31) div 8;
          ImageSize := biWidth * BytesPerScanline;
          Bits := Pointer(DWORD(Bits) + BitmapBitsSize - ImageSize);
          Result := FindScanline(Bits, ImageSize, $FF);
          if (Result = 0) and (biHeight >= 2 * biWidth) then
            Result := FindScanline(Pointer(DWORD(Bits) - ImageSize),
            ImageSize, $00);
          Result := Result div BytesPerScanline;
        end;
        Dec(Result, IconInfo.yHotSpot);
      end;
      finally
        FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
      end;
    finally
      if IconInfo.hbmColor <> 0 then DeleteObject(IconInfo.hbmColor);
      if IconInfo.hbmMask <> 0 then DeleteObject(IconInfo.hbmMask);
    end;

  {$IFDEF VER300_UP}
  if Screen.ActiveCustomForm <> nil then
  begin
    D := Screen.PixelsPerInch;
    M := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).PixelsPerInch;
    Result := Muldiv(Result, M, D);
  end;
  {$ENDIF}
end;

function HexStringToColor(HexString: string; var Color: Integer): Boolean;
var
  I: byte;
begin
  if HexString = '' then
  begin
    Result := False;
    Color := 0;
    Exit;
  end;

  if HexString[1] = '#' then
    Delete(HexString, 1, 1);

  HexString := UpperCase(HexString);

  if HexString[Length(HexString)] = 'H' then
    Delete(HexString, Length(HexString), 1);

  Color := 0;
  Result := True;

  for i := 1 to length(HexString) do
  begin
    Color := Color shl 4;
    if CharInSet(HexString[i], ['0'..'9'])
    then
      Color := Color + (Byte(HexString[i]) - 48)
    else
      if CharInSet(HexString[i], ['A'..'F'])
      then
        Color := Color + (Byte(HexString[i]) - 55)
      else
      begin
        Result := False;
        Color := 0;
        Break;
      end;
  end;
end;

function DecStringToColor(DecString: String; var Color: Integer): Boolean;
begin
  Result := False;
  Color := 0;
  if length(DecString) > 8 then Exit;
  Result := TryStrToInt(DecString, Color);
  Color := Color and $FFFFFF;
end;

function NameStringToColor(NameString: String; var Color: Integer): Boolean;
begin
  Result := True;
  NameString := LowerCase(NameString);
  if NameString = 'black' then Color := clBlack
  else if NameString = 'maroon' then
    Color := clMaroon
  else if NameString = 'green' then
    Color := clGreen
  else if NameString = 'olive' then
    Color := clOlive
  else if NameString = 'navy' then
    Color := clNavy
  else if NameString = 'purple' then
    Color := clPurple
  else if NameString = 'teal' then
    Color := clTeal
  else if NameString = 'gray' then
    Color := clGray
  else if NameString = 'silver' then
    Color := clSilver
  else if NameString = 'red' then
    Color := clRed
  else if NameString = 'lime' then
    Color := clLime
  else if NameString = 'yellow' then
    Color := clYellow
  else if NameString = 'blue' then
    Color := clBlue
  else if NameString = 'fuchsia' then
    Color := clFuchsia
  else if NameString = 'magenta' then
    Color := clFuchsia
  else if NameString = 'aqua' then
    Color := clAqua
  else if NameString = 'ltgray' then
    Color := clLtGray
  else if NameString = 'dkgray' then
    Color := clDkGray
  else if NameString = 'white' then
    Color := clWhite
  else if NameString = 'moneygreen' then
    Color := clMoneyGreen
  else if NameString = 'skyblue' then
    Color := clSkyBlue
  else if NameString = 'cream' then
    Color := clCream
  else if NameString = 'medgray' then
    Color := clMedGray
  else if NameString = 'brown' then
    Color := $17335C
  else if NameString = 'orange' then
    Color := $008CFF
  else if NameString = 'pink' then
    Color := $9314FF
  else
    Result := False;
end;

var
  UxThemeDLL: HMODULE = 0;
  DWMDLL: HMODULE = 0;
  User32DLL: HMODULE = 0;

procedure SetupUxTheme;
begin
  UxThemeDLL := LoadLibrary('uxtheme.dll');
  if uxThemeDLL <> 0 then
  begin
    @SC_BeginBufferedAnimation := GetProcAddress(uxThemeDLL, 'BeginBufferedAnimation');
    @SC_EndBufferedAnimation := GetProcAddress(uxThemeDLL, 'EndBufferedAnimation');
    @SC_BufferedPaintRenderAnimation := GetProcAddress(uxThemeDLL, 'BufferedPaintRenderAnimation');
    @SC_BufferedPaintStopAllAnimations := GetProcAddress(uxThemeDLL, 'BufferedPaintStopAllAnimations');
  end;
end;

procedure SetupDWM;
begin
  DWMDLL := LoadLibrary('dwmapi.dll');
  if DWMDLL <> 0 then
  begin
    @SC_DwmIsCompositionEnabled := GetProcAddress(DWMDLL, 'DwmIsCompositionEnabled');
    @SC_DwmExtendFrameIntoClientArea := GetProcAddress(DWMDLL, 'DwmExtendFrameIntoClientArea');
    @SC_DwmSetWindowAttribute := GetProcAddress(DWMDLL, 'DwmSetWindowAttribute');
  end;
end;

procedure SetupUser32;
begin
  User32DLL := LoadLibrary('user32.dll');
  if User32DLL <> 0 then
  begin
    @SC_SetWindowCompositionAttribute := GetProcAddress(User32DLL, 'SetWindowCompositionAttribute');
  end;
end;

function SC_DwmCompositionEnabled: Boolean;
var
  LEnabled: BOOL;
begin
  Result := (FOSMajor >= 6) and (@SC_DwmIsCompositionEnabled <> nil) and
    (SC_DwmIsCompositionEnabled(LEnabled) = S_OK) and LEnabled;
end;

initialization

  GetWindowsVersion(FOSMajor, FOSMinor);
  SetupuxTheme;
  SetupDWM;
  SetupUser32;
  
finalization

 if UxThemeDLL <> 0 then
   FreeLibrary(uxThemeDLL);
 if DWMDLL <> 0 then
   FreeLibrary(DWMDLL);
 if User32DLL <> 0 then
   FreeLibrary(User32DLL);

end.

