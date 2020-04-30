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

unit scGPUtils;

interface

uses
   System.Classes, System.Types, System.SysUtils,
   WinApi.Windows, WinApi.GDIPApi, WinApi.GDIPObj, Vcl.Graphics;

const
  SC_FluentPressedEffectSteps = 10;

type
  TscGPShapeFillStyle = (scgpsfColor, scgpsfGradient);
  TscGPImageClipFrame = (scgpcfNone, scgpcfRoundedRect, scgpcfEllipse);

  TscGPBitmapOptions = class(TPersistent)
  private
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FTopMargin: Integer;
    FBottomMargin: Integer;
    FStretch: Boolean;
    FStretchBorder: Boolean;
    FDrawOnlyBorder: Boolean;
    FDrawOnlyClient: Boolean;
    FOnChange: TNotifyEvent;
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
    property Stretch: Boolean read FStretch write FStretch;
    property StretchBorder: Boolean read FStretchBorder write FStretchBorder;
    property DrawOnlyBorder: Boolean read FDrawOnlyBorder write FDrawOnlyBorder;
    property DrawOnlyClient: Boolean read FDrawOnlyClient write FDrawOnlyClient;
  end;

  procedure GPBitmap_DrawWithOptions(AGraphics: TGPGraphics; ABitmap: TGPBitmap;
     AOptions: TscGPBitmapOptions; ARect: TGPRectF; AVirtualWidth, AVirtualHeight: Integer; AScaleFactor: Double);

  function ColorToGPColor(AColor: TColor; Alpha: Byte = 255): Cardinal;
  function RectToGPRect(ARect: TRect): TGPRectF;
  procedure InflateGPRect(var ARect: TGPRectF; DX, DY: Single);
  procedure GPDrawBitmapSmooth(ADC: HDC; ARect: TRect; ABitmap: TBitmap);
  procedure GPDrawBitmapSmooth2(ADC: HDC; ARect: TRect; ABitmap: TBitmap);
  function GraphicToGPBitmap(G: TGraphic): TGPBitmap;
  function PictureToGPBitmap(Picture: TPicture): TGPBitmap;

  procedure GPDrawShadowForEllipse(G: TGPGraphics; X, Y, W, H: Single; ShadowSize: Integer; ShadowAlpha: Byte = 255);
  procedure GPDrawPathAsShadow(G: TGPGraphics; Path: TGPGraphicsPath; CenterPoint: TGPPointF; ShadowAlpha: Byte = 255);

  procedure GPDrawDownGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPasswordGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawDropDownButtonGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawUpGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawRightGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawClearGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMoreGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawDetailsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double);
  procedure GPDrawPriorGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawNextGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawSearchGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawDetailsGlyph2(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPlusGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMinusGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawOptionsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawCheckGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawRefreshGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawRefreshGlyph2(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawEditGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFirstGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawLastGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPlayGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPauseGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawRectGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawExitGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawInBoxGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawOutBoxGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawHomeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawShutDownGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawCheckOptionsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawDetailPointsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawInfoGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawDownloadGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawUpLoadGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMinimizeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMaximizeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawRestoreGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFileNewGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFileSaveGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFileSaveAsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFolderGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFileOpenGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPrintGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMicGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMicGlyph2(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMicMuteGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawCircleGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawCircleFillGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPhotoGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawVideoGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawLockGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawUnLockGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawUserGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawTrashGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPinGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawColorMarker(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AColorValue: TColor; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawFontColorMarker(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AColorValue: TColor; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFillColorMarker(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AColorValue: TColor; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawRewindGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawForwardGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawSkipToStartGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawSkipToEndGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawVolumeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMuteGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawVolumePlusGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawVolumeMinusGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawZoomPlusGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawZoomMinusGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawViewGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFlagGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFilterGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawObjectsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawReplyGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawUndoGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawRedoGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawClockGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawCalendarGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawChartGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawFontGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawFontIncSizeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFontDecSizeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawFillGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawMessageGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawMessageReadGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawSendGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawTextAlignLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawTextAlignRightGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawTextAlignCenterGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawTextAlignJustifiedGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawCopyGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawCutGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawPasteGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawHelpGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawReplaceGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawShareGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawCallGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawCallEndGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawMapGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawMapMarkerGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawGearGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawGearGlyphAngle(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; Angle: Single; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawGlobeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawStarGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawNaviTopGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawNaviBottomGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawNaviTopLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawNaviTopRightGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawNaviBottomLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawNaviBottomRightGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawSizeFullGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawSizeCompactGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawShopBasketGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawShopBasketInGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
  procedure GPDrawShopBasketOutGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawToolDialogGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure GPDrawResizeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

  procedure scGPDrawFocus(G: TGPGraphics; ARect: TGPRectF;
     AColor: Cardinal; AScaleFactor: Double);

  procedure DrawFluentLightHotEffect(G: TGPGraphics; AFillR: TGPRectF; AX: Integer);
  procedure DrawFluentLightPressedEffect(G: TGPGraphics; AFillR: TGPRectF; AX: Integer; AY: Integer; Amount: Byte);

implementation
   Uses Vcl.Imaging.PngImage;

function GPRotate(P, CP: TGPPointF; Angle: Single; RightDirection: Boolean): TGPPointF;
var
  X, Y: Single;
  Rad: Single;
begin
  X := P.X - CP.X;
  Y := P.Y - CP.Y;
  Rad := (pi / 180) * Angle;
  if RightDirection then
  begin
    Result.X := X * Cos(Rad) - Y * Sin(Rad) + CP.X;
    Result.Y := Y * Cos(Rad) + X * Sin(Rad) + CP.Y;
  end
  else
  begin
    Result.X := X * Cos(Rad) + Y * Sin(Rad) + P.X;
    Result.Y := Y * Cos(Rad) - X * Sin(Rad) + P.Y;
  end;
end;

 function ColorToGPColor(AColor: TColor; Alpha: Byte = 255): Cardinal;
 var
   C: Cardinal;
 begin
   if AColor = clNone then
   begin
     Result := MakeColor(0, 0, 0, 0);
     Exit;
   end;
   C := ColorToRGB(AColor);
   Result := ((C shl 16) and $00FF0000) or ((C shr 16) and $000000FF) or
    (C and $0000FF00) or (Alpha shl 24);
end;

function RectToGPRect(ARect: TRect): TGPRectF;
begin
  Result.X := ARect.Left;
  Result.Y := ARect.Top;
  Result.Width := ARect.Width;
  Result.Height := ARect.Height;
end;

procedure InflateGPRect(var ARect: TGPRectF; DX, DY: Single);
begin
  ARect.X := ARect.X - DX;
  ARect.Y := ARect.Y - DY;
  ARect.Width := ARect.Width + DX * 2;
  ARect.Height := ARect.Height + DY * 2;
end;

procedure GPDrawRectGlyph2(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawRectangle(P, ARect);
  P.Free;
end;

procedure GPDrawDropDownButtonGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width / 2, ARect.Y + ARect.Height,
    ARect.X + ARect.Width, ARect.Y + ARect.Height / 2);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawDownGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X,
    ARect.Y + ARect.Height / 4,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height - ARect.Height / 4);
  GPath.AddLine(ARect.X + ARect.Width / 2, ARect.Y + ARect.Height - ARect.Height / 4,
    ARect.X + ARect.Width, ARect.Y + ARect.Height / 4);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawUpGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X,
    ARect.Y + ARect.Height - ARect.Height / 4,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height / 4);
  GPath.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height / 4,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height - ARect.Height / 4);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
   if AThickness < 1 then
    AThickness := 1;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width - ARect.Width / 4, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width - ARect.Width / 4,
    ARect.Y + ARect.Height);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawPlayGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 4, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 4, ARect.Y + ARect.Height,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2);
  GPath.CloseFigure;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawRightCloseGlyph(GPath: TGPGraphicsPath; ARect: TGPRectF);
begin
  GPath.StartFigure;
  GPath.AddLine(ARect.X,
    ARect.Y,
    ARect.X, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X, ARect.Y + ARect.Height,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2);
  GPath.CloseFigure;
end;

procedure GPDrawLeftCloseGlyph(GPath: TGPGraphicsPath; ARect: TGPRectF);
begin
  GPath.StartFigure;
  GPath.AddLine(ARect.X,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width, ARect.Y,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height);
  GPath.CloseFigure;
end;

procedure GPDrawRewindGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R1, R2: TGPRectF;
  GPath: TGPGraphicsPath;
  P: TGPPen;
begin
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R1 := ARect;
  R1.Width := R1.Height / 2.5;
  R1.Height := R1.Height / 1.5;
  R1.Y := ARect.Y + ARect.Height / 2 - R1.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPDrawLeftCloseGlyph(GPath, R1);
  R2 := R1;
  R2.X := R1.X + R1.Width + P.GetWidth / 2;;
  GPDrawLeftCloseGlyph(GPath, R2);
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;


procedure GPDrawForwardGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R1, R2: TGPRectF;
  GPath: TGPGraphicsPath;
  P: TGPPen;
begin
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R1 := ARect;
  R1.Width := R1.Height / 2.5;
  R1.Height := R1.Height / 1.5;
  R1.X := R1.X + R1.Width * 0.4;
  R1.Y := ARect.Y + ARect.Height / 2 - R1.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPDrawRightCloseGlyph(GPath, R1);
  R2 := R1;
  R2.X := R1.X + R1.Width + P.GetWidth / 2;
  GPDrawRightCloseGlyph(GPath, R2);
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;

procedure GPDrawSkipToStartGlyph(G: TGPGraphics; ARect: TGPRectF;
   AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R1: TGPRectF;
  GPath: TGPGraphicsPath;
  P: TGPPen;
begin
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R1 := ARect;
  R1.Width := R1.Height / 2.5;
  R1.Height := R1.Height / 1.5;
  R1.X := ARect.X + ARect.Width / 2 - R1.Width / 2;
  R1.Y := ARect.Y + ARect.Height / 2 - R1.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPDrawLeftCloseGlyph(GPath, R1);
  GPath.AddLine(R1.X - P.GetWidth, R1.Y, R1.X - P.GetWidth, R1.Y + R1.Height);
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;

procedure GPDrawSkipToEndGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R1: TGPRectF;
  GPath: TGPGraphicsPath;
  P: TGPPen;
begin
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R1 := ARect;
  R1.Width := R1.Height / 2.5;
  R1.Height := R1.Height / 1.5;
  R1.X := R1.X + ARect.Width / 2 - R1.Width / 2;
  R1.Y := ARect.Y + ARect.Height / 2 - R1.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPDrawRightCloseGlyph(GPath, R1);
  GPath.AddLine(R1.X + R1.Width + P.GetWidth, R1.Y,
    R1.X + R1.Width + P.GetWidth, R1.Y + R1.Height);
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;

procedure GPDrawFirstGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
   if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  ARect.X := ARect.X + P.GetWidth * 1.25;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width - ARect.Width / 4, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width - ARect.Width / 4,
    ARect.Y + ARect.Height);
  GPath.CloseFigure;
  GPath.AddLine(ARect.X + ARect.Width / 4 - P.GetWidth * 2, ARect.Y,
  ARect.X + ARect.Width / 4 - P.GetWidth * 2, ARect.Y + ARect.Height);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawLastGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
   if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  ARect.X := ARect.X - P.GetWidth * 1.25;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 4, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 4, ARect.Y + ARect.Height);
  GPath.CloseFigure;
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 4 + P.GetWidth * 2, ARect.Y,
  ARect.X + ARect.Width - ARect.Width / 4 + P.GetWidth * 2,
  ARect.Y + ARect.Height);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawRightGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 4, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 4,
    ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 4, ARect.Y + ARect.Height);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawNaviTopGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y,
   ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawNaviBottomGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y,
   ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawNaviTopLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X,
    ARect.Y + W,
    ARect.X, ARect.Y);
  GPath.AddLine(ARect.X + W,
    ARect.Y,
    ARect.X, ARect.Y);
  GPath2.AddLine(ARect.X,
    ARect.Y,
   ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;


procedure GPDrawNaviTopRightGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + W,
    ARect.X + ARect.Width, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width - W,
    ARect.Y,
    ARect.X + ARect.Width, ARect.Y);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y,
   ARect.X, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;
procedure GPDrawNaviBottomLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X,
    ARect.Y + ARect.Height - W,
    ARect.X, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + W,
    ARect.Y + ARect.Height,
    ARect.X, ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y,
   ARect.X, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawResizeGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width - W,
    ARect.Y + ARect.Height,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X,
    ARect.Y,
   ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath3.AddLine(ARect.X,
    ARect.Y + W,
    ARect.X, ARect.Y);
  GPath3.AddLine(ARect.X + W,
    ARect.Y,
    ARect.X, ARect.Y);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);

  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;

procedure GPDrawNaviBottomRightGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width - W,
    ARect.Y + ARect.Height,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X,
    ARect.Y,
   ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;


procedure GPDrawToolDialogGlyph(G: TGPGraphics; ARect: TGPRectF;

  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
  SMode: SmoothingMode;
  OMode: PixelOffsetMode;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width - W,
    ARect.Y + ARect.Height,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X + ARect.Width * 0.5,
    ARect.Y + ARect.Height * 0.5,
   ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);

  G.DrawPath(P, GPath);  // arrow

  SMode := G.GetSmoothingMode;
  OMode := G.GetPixelOffsetMode;

  G.SetSmoothingMode(SmoothingMode.SmoothingModeNone);
  G.SetPixelOffsetMode(PixelOffsetMode.PixelOffsetModeNone);

  ARect.X := Round(ARect.X);
  ARect.Y := Round(ARect.Y);
  ARect.Width := Round(ARect.Width);
  ARect.Height := Round(ARect.Height);

  GPath3.AddLine(ARect.X, ARect.Y, ARect.X + ARect.Width * 0.9, ARect.Y);
  GPath3.AddLine(ARect.X, ARect.Y, ARect.X, ARect.Y + + ARect.Height * 0.9);

  G.DrawPath(P, GPath3); // lines

  G.SetSmoothingMode(SMode);
  G.SetPixelOffsetMode(OMode);

  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;



procedure GPDrawPriorGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + W,
    ARect.Y + ARect.Height / 2 - W,
    ARect.X, ARect.Y + ARect.Height / 2);
  GPath.AddLine(ARect.X,
    ARect.Y + ARect.Height / 2,
    ARect.X + W, ARect.Y + ARect.Height / 2 + W);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2,
    ARect.X + P.GetWidth - P.GetWidth / 4, ARect.Y + ARect.Height / 2);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawNextGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3;
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width - W,
    ARect.Y + ARect.Height / 2 - W,
    ARect.X + ARect.Width, ARect.Y + ARect.Height / 2);
  GPath.AddLine(ARect.X + ARect.Width - W,
    ARect.Y + ARect.Height / 2 + W,
    ARect.X + ARect.Width, ARect.Y + ARect.Height / 2);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2,
    ARect.X, ARect.Y + ARect.Height / 2);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawDownloadGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3;
  ARect.Y := ARect.Y - P.GetWidth;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y,
   ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  ARect.Y := ARect.Y + P.GetWidth;
  G.DrawLine(P, ARect.X + ARect.Width / 2 - W,
    ARect.Y + ARect.Height,
    ARect.X + ARect.Width / 2 + W,
    ARect.Y + ARect.Height);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawUpLoadGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3;
  ARect.Y := ARect.Y + P.GetWidth;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y,
   ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  ARect.Y := ARect.Y - P.GetWidth;
  G.DrawLine(P, ARect.X + ARect.Width / 2 - W,
    ARect.Y,
    ARect.X + ARect.Width / 2 + W,
    ARect.Y);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;


procedure GPDrawExitGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  R: TGPRectF;
  P1, P2, P3, P4, P5, P6: TGPPointF;
begin
  R := ARect;
  W := ARect.Height / 6;
  InflateGPRect(R, -W, -W);
  R.X := R.X + W * 2;
  GPDrawNextGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);
  P1.X := ARect.X + ARect.Width - W;
  P1.Y := ARect.Y + ARect.Height / 4;
  P2.X := ARect.X + ARect.Width - W;
  P2.Y := ARect.Y;
  P3.X := ARect.X;
  P3.Y := ARect.Y;
  P4.X := ARect.X;
  P4.Y := ARect.Y + ARect.Height;
  P5.X := ARect.X + ARect.Width - W;
  P5.Y := ARect.Y + ARect.Height;
  P6.X := ARect.X + ARect.Width - W;
  P6.Y := ARect.Y + ARect.Height - ARect.Height / 4;
  GPath := TGPGraphicsPath.Create;
  GPath.AddLine(P1, P2);
  GPath.AddLine(P2, P3);
  GPath.AddLine(P3, P4);
  GPath.AddLine(P4, P5);
  GPath.AddLine(P5, P6);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;

procedure GPDrawGearGlyphAngle(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; Angle: Single; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;

  CP: TGPPointF;
  P: array of TGPPointF;
  Count: Integer;
  R1, R2: TGPRectF;
  P1, P2: TGPPointF;
  I, J: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  InflateGPRect(ARect, -GPen.GetWidth / 2, -GPen.GetWidth / 2);
  Count := 32;
  SetLength(P, Count);
  CP.X := ARect.X + ARect.Width / 2;
  CP.Y := ARect.Y + ARect.Height / 2;
  R1 := ARect;
  R2 := R1;
  InflateGPRect(R2, -ARect.Width / 8, -ARect.Height / 8);

  P1.X := CP.X;
  P1.Y := R1.Y;
  P2.X := CP.X;
  P2.Y := R2.Y;

  P[0] := GPRotate(P2, CP, 345 + Angle, True);
  P[1] := GPRotate(P1, CP, 352 + Angle, True);
  P[2] := GPRotate(P1, CP, 8 + Angle, True);
  P[3] := GPRotate(P2, CP, 15 + Angle, True);

  J := 1;

  for I := 1 to 7 do
  begin
    P[J + 3] := GPRotate(P[J - 1], CP, 45, True);
    P[J + 4] := GPRotate(P[J], CP, 45, True);
    P[J + 5] := GPRotate(P[J + 1], CP, 45, True);
    P[J + 6] := GPRotate(P[J + 2], CP, 45, True);
    Inc(J, 4);
  end;

  GPath.AddPolygon(PGPPointF(P), Count);

  InflateGPRect(R2, -ARect.Width / 5, -ARect.Height / 5);
  GPath.AddEllipse(R2);

  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawGearGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;

  CP: TGPPointF;
  P: array of TGPPointF;
  Count: Integer;
  R1, R2: TGPRectF;
  P1, P2: TGPPointF;
  I, J: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  Count := 32;
  SetLength(P, Count);
  CP.X := ARect.X + ARect.Width / 2;
  CP.Y := ARect.Y + ARect.Height / 2;
  R1 := ARect;
  R2 := R1;
  InflateGPRect(R2, -ARect.Width / 8, -ARect.Height / 8);

  P1.X := CP.X;
  P1.Y := R1.Y;
  P2.X := CP.X;
  P2.Y := R2.Y;

  P[0] := GPRotate(P2, CP, 345, True);
  P[1] := GPRotate(P1, CP, 352, True);
  P[2] := GPRotate(P1, CP, 8, True);
  P[3] := GPRotate(P2, CP, 15, True);

  J := 1;

  for I := 1 to 7 do
  begin
    P[J + 3] := GPRotate(P[J - 1], CP, 45, True);
    P[J + 4] := GPRotate(P[J], CP, 45, True);
    P[J + 5] := GPRotate(P[J + 1], CP, 45, True);
    P[J + 6] := GPRotate(P[J + 2], CP, 45, True);
    Inc(J, 4);
  end;

  GPath.AddPolygon(PGPPointF(P), Count);

  InflateGPRect(R2, -ARect.Width / 5, -ARect.Height / 5);
  GPath.AddEllipse(R2);

  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawGlobeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;

  R: TGPRectF;
  CP: TGPPointF;
  P: array of TGPPointF;
  Count: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;
  Count := 4;
  SetLength(P, Count);
  R := ARect;
  CP.X := ARect.X + ARect.Width / 2;
  CP.Y := ARect.Y + ARect.Height / 2;
  GPath.AddEllipse(R);
  R.Width := ARect.Width / 3;
  R.X := CP.X - R.Width / 2;
  GPath.AddEllipse(R);
  GPath.AddLine(ARect.X, CP.Y,
    ARect.X + ARect.Width, CP.Y);

  P[0].X := CP.X;
  P[0].Y := ARect.Y;
  P[3] := P[0];
  P[0] := GPRotate(P[0], CP, 45, True);
  P[3] := GPRotate(P[3], CP, 315, True);
  P[1].X := CP.X;
  P[1].Y := ARect.Y + ARect.Height / 5;
  P[2].X := CP.X;
  P[2].Y := ARect.Y + ARect.Height / 5;
  P[1] := GPRotate(P[1], CP, 45, True);
  P[2] := GPRotate(P[2], CP, 315, True);
  GPath2.AddBeziers(PGPPointF(P), Count);

  P[0].X := CP.X;
  P[0].Y := ARect.Y;
  P[3] := P[0];
  P[0] := GPRotate(P[0], CP, 135, True);
  P[3] := GPRotate(P[3], CP, 225, True);
  P[1].X := CP.X;
  P[1].Y := ARect.Y + ARect.Height / 5;
  P[2].X := CP.X;
  P[2].Y := ARect.Y + ARect.Height / 5;
  P[1] := GPRotate(P[1], CP, 135, True);
  P[2] := GPRotate(P[2], CP, 225, True);
  GPath3.AddBeziers(PGPPointF(P), Count);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPen.Free;
end;

procedure GPDrawStarGlyph(G: TGPGraphics; ARect: TGPRectF;
   AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  GPen: TGPPen;
  GPath: TGPGraphicsPath;

  CP: TGPPointF;
  P: array of TGPPointF;
  Count: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  Count := 10;
  SetLength(P, Count);
  CP.X := ARect.X + ARect.Width / 2;
  CP.Y := ARect.Y + ARect.Height / 2;

  P[0].X := CP.X;
  P[0].Y := ARect.Y;
  P[2] := GPRotate(P[0], CP, 72, True);
  P[4] := GPRotate(P[0], CP, 144, True);
  P[6] := GPRotate(P[0], CP, 216, True);
  P[8] := GPRotate(P[0], CP, 288, True);

  InflateGPRect(ARect, -ARect.Width / 4, -ARect.Height / 4);

  P[1].X := CP.X;
  P[1].Y := ARect.Y;
  P[1] := GPRotate(P[1], CP, 36, True);
  P[3] := GPRotate(P[1], CP, 72, True);
  P[5] := GPRotate(P[1], CP, 144, True);
  P[7] := GPRotate(P[1], CP, 216, True);
  P[9] := GPRotate(P[1], CP, 288, True);

  GPath.AddPolygon(PGPPointF(P), Count);

  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawMapGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Width / 3;
  Count := 8;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y + W / 2;
  P[1].X := P[0].X + W;
  P[1].Y := ARect.Y;
  P[2].X := P[1].X + W;
  P[2].Y := P[0].Y;
  P[3].X := P[2].X + W;
  P[3].Y := ARect.Y;
  P[4].X := P[3].X;
  P[4].Y := ARect.Y + ARect.Height - W / 2;
  P[5].X := P[4].X - W;
  P[5].Y := ARect.Y + ARect.Height;
  P[6].X := P[5].X - W;
  P[6].Y := P[4].Y;
  P[7].X := P[6].X - W;
  P[7].Y := P[5].Y;
  GPath.AddPolygon(PGPPointF(P), Count);
  GPath.AddLine(P[1], P[6]);
  GPath.AddLine(P[5], P[2]);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawMapMarkerGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  P: TGPPen;

  GPath: TGPGraphicsPath;

  R: TGPRectF;

begin

  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  R := ARect;
  R.Width := ARect.Width - ARect.Width / 3;
  R.X := ARect.X + ARect.Width / 2 - R.Width / 2;
  R.Height := R.Width;
  GPath.AddArc(R, 0, -180);
  GPath.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height, R.X, R.Y + R.Height / 2);
  GPath.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height, R.X + R.Width, R.Y + R.Height / 2);
  InflateGPRect(R, -R.Width / 3, -R.Width / 3);
  GPath.AddEllipse(R);
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;

procedure GPDrawHomeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 6;
  InflateGPRect(ARect, -W / 2, -W / 2);
  Count := 12;
  SetLength(P, Count);
  ARect.Y := ARect.Y + W;
  ARect.Height := ARect.Height - W;
  P[0].X := ARect.X;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X;
  P[1].Y := ARect.Y + ARect.Height;
  P[2].X := ARect.X + ARect.Width / 3;
  P[2].Y := ARect.Y + ARect.Height;
  P[3].X := P[2].X;
  P[3].Y := P[2].Y - ARect.Height / 2;
  P[4].X := P[3].X + ARect.Width / 3;
  P[4].Y := P[3].Y;
  P[5].X := P[4].X;
  P[5].Y := P[1].Y;
  P[6].X := ARect.X + ARect.Width;
  P[6].Y := P[2].Y;
  P[7].X := P[6].X;
  P[7].Y := ARect.Y;
  P[8].X := P[0].X - W / 2;
  P[8].Y := P[0].Y;
  P[9].X := ARect.X + ARect.Width / 2;
  P[9].Y := ARect.Y - W * 2;
  P[10].X := ARect.X + ARect.Width + W / 2;
  P[10].Y := ARect.Y;
  P[11].X := ARect.X + ARect.Width;
  P[11].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawClearGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X, ARect.Y,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width, ARect.Y,
    ARect.X,
    ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawMinusGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawLine(P, ARect.X, ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2);
  P.Free;
end;

procedure GPDrawRectGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 6;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  InflateGPRect(ARect, -W, -W);
  G.DrawRectangle(P, ARect);
  P.Free;
end;

procedure GPDrawRestoreGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R: TGPRectF;
  Offset: Single;
  SMode: SmoothingMode;
  OMode: PixelOffsetMode;
begin
  ARect.Height := ARect.Height - 1;
  ARect.Width := ARect.Width - 1;
  R := ARect;
  InflateGPRect(R, -ARect.Height / 5, -ARect.Height / 5);
  Offset := ARect.Height / 12;
  R.X := R.X - Offset;
  R.Y := R.Y + Offset;
  SMode := G.GetSmoothingMode;
  OMode := G.GetPixelOffsetMode;
  G.SetSmoothingMode(SmoothingMode.SmoothingModeNone);
  G.SetPixelOffsetMode(PixelOffsetMode.PixelOffsetModeNone);

  R.X := Round(R.X);
  R.Y := Round(R.Y);
  R.Width := Round(R.Width);
  R.Height := Round(R.Height);
  GPDrawRectGlyph2(G, R, AGlyphColor, AScaleFactor, AThickness);

  G.ExcludeClip(R);
  R := ARect;
  InflateGPRect(R, -ARect.Height / 5, -ARect.Height / 5);
  R.X := R.X + Offset;
  R.Y := R.Y - Offset;

  R.X := Round(R.X);
  R.Y := Round(R.Y);
  R.Width := Round(R.Width);
  R.Height := Round(R.Height);
  GPDrawRectGlyph2(G, R, AGlyphColor, AScaleFactor, AThickness);

  G.ResetClip;
  G.SetSmoothingMode(SMode);
  G.SetPixelOffsetMode(OMode);
end;

procedure GPDrawMaximizeGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
  SMode: SmoothingMode;
  OMode: PixelOffsetMode;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := Round(ARect.Height / 6);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  InflateGPRect(ARect, -W, -W);
  ARect.Y := ARect.Y - 1;
  SMode := G.GetSmoothingMode;
  OMode := G.GetPixelOffsetMode;
  G.SetSmoothingMode(SmoothingMode.SmoothingModeNone);
  G.SetPixelOffsetMode(PixelOffsetMode.PixelOffsetModeNone);

  ARect.X := Round(ARect.X);
  ARect.Y := Round(ARect.Y);
  ARect.Width := Round(ARect.Width);
  ARect.Height := Round(ARect.Height);

  G.DrawRectangle(P, ARect);
  G.SetSmoothingMode(SMode);
  G.SetPixelOffsetMode(OMode);
  P.Free;
end;

procedure GPDrawMinimizeGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  Mode: SmoothingMode;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  Mode := G.GetSmoothingMode;
  G.SetSmoothingMode(SmoothingMode.SmoothingModeNone);
  G.DrawLine(P, ARect.X + ARect.Width / 6, ARect.Y + ARect.Height - ARect.Height / 2,
    ARect.X + ARect.Width - ARect.Width / 6,
    ARect.Y + ARect.Height - ARect.Height / 2);
  G.SetSmoothingMode(Mode);
  P.Free;
end;

procedure GPDrawCheckGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  ARect.Y := Round(ARect.Y - ARect.Height / 10);
  GPath.AddLine(ARect.X, ARect.Y + ARect.Height - ARect.Height / 3,
     ARect.X + ARect.Width / 3, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width / 3, ARect.Y + ARect.Height,
     ARect.X + ARect.Width, ARect.Y +  ARect.Height / 4);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawPlusGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddLine(ARect.X + ARect.Width / 2, ARect.Y,
    ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X, ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawFileNewGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  W := ARect.Height / 6;
  ARect.Y := ARect.Y + W / 2;
  ARect.Height := ARect.Height - W;
  Count := 5;
  SetLength(P, Count);
  P[0].X := ARect.X + W;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X + W;
  P[1].Y := ARect.Y + ARect.Height;
  P[2].X := ARect.X + ARect.Width - W;
  P[2].Y := ARect.Y + ARect.Height;
  P[3].X := ARect.X + ARect.Width - W;
  P[3].Y := ARect.Y + W * 1.5;
  P[4].X := ARect.X + ARect.Width - W - W * 1.5;
  P[4].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width - W, ARect.Y + W * 1.5,
    ARect.X + ARect.Width - W - W * 1.5, ARect.Y + W * 1.5 );
  GPath2.AddLine(ARect.X + ARect.Width - W - W * 1.5, ARect.Y + W * 1.5,
    ARect.X + ARect.Width - W - W * 1.5, ARect.Y);
  GPath.AddPath(GPath2, True);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPen.Free;
end;

procedure GPDrawShopBasketGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
  R: TGPRectF;
begin
  Count := 6;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y + ARect.Height / 6;
  P[1].X := ARect.X + ARect.Width / 6;
  P[1].Y := ARect.Y + ARect.Height / 6;
  P[2].X := P[1].X + ARect.Width / 4;
  P[2].Y := P[1].Y + ARect.Width / 2;
  P[3].X := P[2].X + ARect.Width / 2.5;
  P[3].Y := P[2].Y;
  P[4].X := ARect.X + ARect.Width;
  P[4].Y := P[2].Y - ARect.Height / 2.5;
  P[5].X := P[4].X - ARect.Width / 2;
  P[5].Y := P[4].Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLines(PGPPointF(P), Count);

  R.Width := ARect.Width / 8;
  R.Height := ARect.Width / 8;
  R.X := P[2].X - R.Width / 2;
  R.Y := P[2].Y + GPen.GetWidth * 2;
  GPath.AddEllipse(R);

  R.X := P[3].X - R.Width / 2;
  R.Y := P[3].Y + GPen.GetWidth * 2;

  GPath.AddEllipse(R);

  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawShopBasketInGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
  R: TGPRectF;
begin
  Count := 6;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y + ARect.Height / 6;
  P[1].X := ARect.X + ARect.Width / 6;
  P[1].Y := ARect.Y + ARect.Height / 6;
  P[2].X := P[1].X + ARect.Width / 4;
  P[2].Y := P[1].Y + ARect.Width / 2;
  P[3].X := P[2].X + ARect.Width / 2.5;
  P[3].Y := P[2].Y;
  P[4].X := ARect.X + ARect.Width;
  P[4].Y := P[2].Y - ARect.Height / 2.5;
  P[5].X := P[4].X - ARect.Width / 4;
  P[5].Y := P[4].Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLines(PGPPointF(P), Count);

  R.Width := ARect.Width / 8;
  R.Height := ARect.Width / 8;
  R.X := P[2].X - R.Width / 2;
  R.Y := P[2].Y + GPen.GetWidth * 2;
  GPath.AddEllipse(R);

  R.X := P[3].X - R.Width / 2;
  R.Y := P[3].Y + GPen.GetWidth * 2;

  GPath.AddEllipse(R);
  G.DrawPath(GPen, GPath);

  R := ARect;
  InflateGPRect(R, -ARect.Width / 3, -ARect.Width / 3);
  R.Y := R.Y - R.Height;

  GPDrawNaviBottomGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);

  GPath.Free;
  GPen.Free;
end;

procedure GPDrawShopBasketOutGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
  R: TGPRectF;
begin
  Count := 6;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y + ARect.Height / 6;
  P[1].X := ARect.X + ARect.Width / 6;
  P[1].Y := ARect.Y + ARect.Height / 6;
  P[2].X := P[1].X + ARect.Width / 4;
  P[2].Y := P[1].Y + ARect.Width / 2;
  P[3].X := P[2].X + ARect.Width / 2.5;
  P[3].Y := P[2].Y;
  P[4].X := ARect.X + ARect.Width;
  P[4].Y := P[2].Y - ARect.Height / 2.5;
  P[5].X := P[4].X - ARect.Width / 4;
  P[5].Y := P[4].Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLines(PGPPointF(P), Count);

  R.Width := ARect.Width / 8;
  R.Height := ARect.Width / 8;
  R.X := P[2].X - R.Width / 2;
  R.Y := P[2].Y + GPen.GetWidth * 2;
  GPath.AddEllipse(R);

  R.X := P[3].X - R.Width / 2;
  R.Y := P[3].Y + GPen.GetWidth * 2;

  GPath.AddEllipse(R);
  G.DrawPath(GPen, GPath);

  R := ARect;
  InflateGPRect(R, -ARect.Width / 3, -ARect.Width / 3);
  R.Y := R.Y - R.Height;

  GPDrawNaviTopGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);

  GPath.Free;
  GPen.Free;
end;

procedure GPDrawFlagGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
begin
  Count := 6;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y + ARect.Height;
  P[1].X := ARect.X;
  P[1].Y := ARect.Y;
  P[2].X := ARect.X + ARect.Width;
  P[2].Y := ARect.Y;
  P[3].X := ARect.X + ARect.Width - ARect.Width / 4;
  P[3].Y := ARect.Y + ARect.Height / 4;
  P[4].X := ARect.X + ARect.Width;
  P[4].Y := ARect.Y + ARect.Height / 2;
  P[5].X := ARect.X;
  P[5].Y := ARect.Y + ARect.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLines(PGPPointF(P), Count);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawFilterGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  W := ARect.Height / 10;
  InflateGPRect(ARect, -W, -W);
  ARect.Y := ARect.Y + W / 2;
  Count := 6;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X + ARect.Width / 3;
  P[1].Y := ARect.Y + ARect.Height / 2;
  P[2].X := ARect.X + ARect.Width / 3;
  P[2].Y := ARect.Y + ARect.Height;
  P[3].X := P[2].X + ARect.Width / 4;
  P[3].Y := ARect.Y + ARect.Height - W / 2;
  P[4].X := P[3].X;
  P[4].Y := P[1].Y;
  P[5].X := ARect.X + ARect.Width;
  P[5].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  G.DrawPath(GPen, GPath);
  GPath.Free;
end;


procedure GPDrawVolumeGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  W := ARect.Height / 10;
  InflateGPRect(ARect, -W, -W);
  ARect.X := ARect.X - W / 2;
  Count := 8;
  SetLength(P, Count);
  P[0].X := ARect.X + ARect.Width / 2;
  P[0].Y := ARect.Y + ARect.Height / 4;
  P[1].X := ARect.X;
  P[1].Y := ARect.Y + ARect.Height / 4;
  P[2].X := ARect.X;
  P[2].Y := P[0].Y + ARect.Height / 2;
  P[3].X := ARect.X + ARect.Width / 2;
  P[3].Y := P[0].Y + ARect.Height / 2;
  P[4].X := P[0].X;
  P[4].Y := P[0].Y;
  P[5].X := ARect.X + ARect.Width;
  P[5].Y := ARect.Y;
  P[6].X := ARect.X + ARect.Width;
  P[6].Y := ARect.Y + ARect.Height;
  P[7].X := P[3].X;
  P[7].Y := P[3].Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLines(PGPPointF(P), Count);
  G.DrawPath(GPen, GPath);
  GPath.Free;
end;

procedure GPDrawMuteGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  W := ARect.Height / 10;
  InflateGPRect(ARect, -W, -W);
  ARect.X := ARect.X - W / 2;
  Count := 8;
  SetLength(P, Count);
  P[0].X := ARect.X + ARect.Width / 2;
  P[0].Y := ARect.Y + ARect.Height / 4;
  P[1].X := ARect.X;
  P[1].Y := ARect.Y + ARect.Height / 4;
  P[2].X := ARect.X;
  P[2].Y := P[0].Y + ARect.Height / 2;
  P[3].X := ARect.X + ARect.Width / 2;
  P[3].Y := P[0].Y + ARect.Height / 2;
  P[4].X := P[0].X;
  P[4].Y := P[0].Y;
  P[5].X := ARect.X + ARect.Width;
  P[5].Y := ARect.Y;
  P[6].X := ARect.X + ARect.Width;
  P[6].Y := ARect.Y + ARect.Height;
  P[7].X := P[3].X;
  P[7].Y := P[3].Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLines(PGPPointF(P), Count);
  GPath2 := TGPGraphicsPath.Create;
  InflateGPRect(ARect, W / 2, W / 2);
  GPath2.AddLine(ARect.X + W, ARect.Y, ARect.X + ARect.Width - W, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawVolumePlusGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R: TGPRectF;
begin
  R := ARect;
  R.Width := ARect.Width - ARect.Width / 3;
  R.Height := ARect.Height - ARect.Height / 3;
  R.Y := ARect.X + ARect.Height / 2 - R.Height / 2;
  GPDrawVolumeGlyph(G, R, AGlyphColor,
    AScaleFactor, AThickness);
  R := ARect;
  R.Width := ARect.Width / 3;
  R.Height := ARect.Height / 3;
  R.X := ARect.X + ARect.Width - R.Width;
  R.Y := ARect.Y + ARect.Height / 2 - R.Height / 2;
  GPDrawPlusGlyph(G, R, AGlyphColor,
    AScaleFactor, AThickness);
end;

procedure GPDrawVolumeMinusGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  R: TGPRectF;
begin
  R := ARect;
  R.Width := ARect.Width - ARect.Width / 3;
  R.Height := ARect.Height - ARect.Height / 3;
  R.Y := ARect.X + ARect.Height / 2 - R.Height / 2;
  GPDrawVolumeGlyph(G, R, AGlyphColor,
    AScaleFactor, AThickness);
  R := ARect;
  R.Width := ARect.Width / 3;
  R.Height := ARect.Height / 3;
  R.X := ARect.X + ARect.Width - R.Width;
  R.Y := ARect.Y + ARect.Height / 2 - R.Height / 2;
  GPDrawMinusGlyph(G, R, AGlyphColor,
    AScaleFactor, AThickness);
end;

procedure GPDrawFileSaveGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2, GPath3, GPath4: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  W := ARect.Height / 6;
  Count := 5;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X;
  P[1].Y := ARect.Y + ARect.Height;
  P[2].X := ARect.X + ARect.Width ;
  P[2].Y := ARect.Y + ARect.Height;
  P[3].X := ARect.X + ARect.Width;
  P[3].Y := ARect.Y + W * 1.5;
  P[4].X := ARect.X + ARect.Width - W * 1.5;
  P[4].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width - W * 1.5, ARect.Y,
    ARect.X + ARect.Width - W * 1.5, ARect.Y + W * 2);
  GPath2.AddLine(ARect.X + ARect.Width - W * 1.5, ARect.Y + W * 2,
   ARect.X + W * 1.5, ARect.Y + W * 2);
  GPath2.AddLine(ARect.X + W * 1.5, ARect.Y + W * 2,
   ARect.X + W * 1.5, ARect.Y);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(ARect.X + ARect.Width - W * 1.5, ARect.Y + ARect.Height,
    ARect.X + ARect.Width - W * 1.5, ARect.Y + ARect.Height - ARect.Height / 2.5);
  GPath3.AddLine(ARect.X + ARect.Width - W * 1.5, ARect.Y + ARect.Height - ARect.Height / 2.5,
    ARect.X + W * 1.5, ARect.Y + ARect.Height - ARect.Height / 2.5);
  GPath3.AddLine(ARect.X + W * 1.5, ARect.Y + ARect.Height - ARect.Height / 2.5,
    ARect.X + W * 1.5, ARect.Y + ARect.Height);

  GPath4 := TGPGraphicsPath.Create;
  GPath4.AddLine(ARect.X + ARect.Width - W * 2.5, ARect.Y,
   ARect.X + ARect.Width - W * 2.5, ARect.Y + W);
  GPath4.AddLine(ARect.X + ARect.Width - W * 2.5, ARect.Y + W,
   ARect.X + ARect.Width - W * 2.8, ARect.Y + W);
  GPath4.AddLine(ARect.X + ARect.Width - W * 2.8, ARect.Y + W,
   ARect.X + ARect.Width - W * 2.8, ARect.Y);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  GPath.AddPath(GPath4, False);


  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPath4.Free;
  GPen.Free;
end;

procedure GPDrawFileSaveAsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R: TGPRectF;
  W: Single;
begin
  W := ARect.Width / 2;
  R.X := ARect.X + ARect.Width - W;
  R.Y := ARect.Y + ARect.Height - W;
  R.Width := W - Trunc(AThickness * AScaleFactor) * 2;
  R.Height := W - W / 4;
  G.ExcludeClip(R);
  GPDrawFileSaveGlyph(G, ARect,
    AGlyphColor, AScaleFactor, AThickness);
  G.ResetClip;
  R.X := R.X - W / 6;
  R.Y := R.Y - W / 6;
  R.Width := W;
  R.Height := W;
  GPDrawEditGlyph(G, R, AGlyphColor, AScaleFactor, AThickness);
end;

procedure GPDrawUpArrowGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y,
   ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawUpArrowGlyph2(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2.5;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + W,
    ARect.X + ARect.Width / 2, ARect.Y);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y,
   ARect.X + ARect.Width / 2, ARect.Y + ARect.Height + ARect.Height / 6);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawDownArrowGlyph2(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 2.5;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + ARect.Height - W,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y - ARect.Height / 6,
   ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawOutBoxGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  R, R1: TGPRectF;
begin
  R := ARect;
  InflateGPRect(R, -ARect.Width / 4, -ARect.Width / 4);
  R.Y := R.Y - ARect.Height / 4;
  GPDrawUpArrowGlyph2(G, R,
    AGlyphColor, AScaleFactor, AThickness);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R := ARect;
  R.Y := R.Y + ARect.Height / 8;
  InflateGPRect(R, -ARect.Width / 5, -ARect.Width / 5);
  R1 := R;
  R1.X := R1.X + R1.Width / 4;
  R1.Width := R1.Width / 2;
  R1.Y := R1.Y - P.GetWidth * 2;
  R1.Height := P.GetWidth * 4;
  G.ExcludeClip(R1);
  G.DrawRectangle(P, R);
  G.ResetClip;
  P.Free;
end;

procedure GPDrawInBoxGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  R, R1: TGPRectF;
begin
  R := ARect;
  InflateGPRect(R, -ARect.Width / 4, -ARect.Width / 4);
  R.Y := R.Y - ARect.Height / 8;
  GPDrawDownArrowGlyph2(G, R,
    AGlyphColor, AScaleFactor, AThickness);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R := ARect;
  R.Y := R.Y + ARect.Height / 8;
  InflateGPRect(R, -ARect.Width / 5, -ARect.Width / 5);
  R1 := R;
  R1.X := R1.X + R1.Width / 4;
  R1.Width := R1.Width / 2;
  R1.Y := R1.Y - P.GetWidth * 2;
  R1.Height := P.GetWidth * 4;
  G.ExcludeClip(R1);
  G.DrawRectangle(P, R);
  G.ResetClip;
  P.Free;
end;

procedure GPDrawFileOpenGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R, R1: TGPRectF;
  W: Single;
begin
  R1.X := ARect.X + ARect.Width / 2 + ARect.Width / 10;
  R1.Y := ARect.Y;
  R1.Width := ARect.Width / 5;
  R1.Height := ARect.Height / 3;
  G.ExcludeClip(R1);
  InflateGPRect(ARect, -ARect.Height / 10, -ARect.Height / 10);
  GPDrawFolderGlyph(G, ARect,
    AGlyphColor, AScaleFactor, AThickness);
  G.ResetClip;
  W := ARect.Width / 2;
  R.X := R1.X - W / 4;
  R.Y := R1.Y - Trunc(AThickness * AScaleFactor);
  R.Width := W;
  R.Height := W - Trunc(AThickness * AScaleFactor);
  GPDrawUpArrowGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);
end;

procedure GPDrawFolderGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  W := ARect.Height / 6;
  Count := 6;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X;
  P[1].Y := ARect.Y + ARect.Height;
  P[2].X := ARect.X + ARect.Width ;
  P[2].Y := ARect.Y + ARect.Height;
  P[3].X := ARect.X + ARect.Width;
  P[3].Y := ARect.Y + W;
  P[4].X := ARect.X + ARect.Width / 2;
  P[4].Y := ARect.Y + W;
  P[5].X := ARect.X + ARect.Width / 2 - W;
  P[5].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X, ARect.Y + W * 2.2,
    ARect.X + ARect.Width, ARect.Y + W * 2.2);
  GPath.AddPath(GPath2, False);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPen.Free;
end;

procedure GPDrawPrintGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
  B: TGPSolidBrush;
  R: TGPRectF;
begin
  W := ARect.Height / 14;
  Count := 11;
  SetLength(P, Count);
  ARect.Y := ARect.Y + ARect.Height / 3;
  ARect.Height := ARect.Height - ARect.Height / 2;
  P[0].X := ARect.X + W;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X;
  P[1].Y := ARect.Y + W;
  P[2].X := ARect.X;
  P[2].Y := ARect.Y + W;
  P[3].X := ARect.X;
  P[3].Y := ARect.Y + ARect.Height;
  P[4].X := ARect.X + ARect.Width / 4;
  P[4].Y := ARect.Y + ARect.Height;
  P[5].X := ARect.X + ARect.Width / 4;
  P[5].Y := ARect.Y + ARect.Height / 2;
  P[6].X := ARect.X + ARect.Width - ARect.Width / 4;
  P[6].Y := ARect.Y + ARect.Height / 2;
  P[7].X := ARect.X + ARect.Width - ARect.Width / 4;
  P[7].Y := ARect.Y + ARect.Height;
  P[8].X := ARect.X + ARect.Width;
  P[8].Y := ARect.Y + ARect.Height;
  P[9].X := ARect.X + ARect.Width;
  P[9].Y := ARect.Y + W;
  P[10].X := ARect.X + ARect.Width - W;
  P[10].Y := ARect.Y;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));

  GPath := TGPGraphicsPath.Create;
  GPath.AddPolygon(PGPPointF(P), Count);

  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(P[4].X, P[4].Y, P[4].X, P[4].Y + W * 2);
  GPath2.AddLine(P[4].X, P[4].Y + W * 2, P[6].X, P[4].Y + W * 2);
  GPath2.AddLine(P[6].X, P[7].Y + W * 2, P[6].X, P[7].Y);

  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(P[4].X, P[0].Y, P[4].X, P[0].Y - W * 3);
  GPath3.AddLine(P[4].X, P[0].Y - W * 3, P[7].X, P[0].Y - W * 3);
  GPath3.AddLine(P[7].X, P[0].Y - W * 3, P[7].X, P[0].Y);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  G.DrawPath(GPen, GPath);

  G.DrawLine(GPen, P[4].X + W, P[5].Y + W * 1.75, P[7].X - W, P[5].Y + W * 1.75);
  G.DrawLine(GPen, P[4].X + W, P[5].Y + W * 3.8, P[7].X - W, P[5].Y + W * 3.8);

  B := TGPSolidBrush.Create(AGlyphColor);
  R.X := P[0].X;
  R.Y := P[1].Y;
  R.Width := W * 1.5;
  R.Height := W * 1.5;
  G.FillEllipse(B, R);

  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPen.Free;
  B.Free;
end;

procedure GPDrawMessageGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 5;
  ARect.Y := ARect.Y + W / 2;
  ARect.Height := ARect.Height - W;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddRectangle(ARect);
  GPath.AddLine(ARect.X, ARect.Y, ARect.X + ARect.Width / 2, ARect.Y + W * 2);
  GPath.AddLine(ARect.X + ARect.Width / 2, ARect.Y + W * 2, ARect.X + ARect.Width, ARect.Y);
  G.DrawPath(GPen, GPath);
  GPen.Free;
  GPath.Free;
end;

procedure GPDrawMessageReadGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 5;
  ARect.Y := ARect.Y + W / 2;
  ARect.Height := ARect.Height - W;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  Count := 5;
  SetLength(P, Count);
  P[0].X := ARect.X;
  P[0].Y := ARect.Y;
  P[1].X := P[0].X;
  P[1].Y := ARect.Y + ARect.Height;
  P[2].X := ARect.X + ARect.Width;
  P[2].Y := ARect.Y + ARect.Height;
  P[3].X := ARect.X + ARect.Width;
  P[3].Y := ARect.Y;
  P[4].X := ARect.X + ARect.Width / 2;
  P[4].Y := ARect.Y - W * 2;
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X, ARect.Y, ARect.X + ARect.Width / 2, ARect.Y + W * 2);
  GPath2.AddLine(ARect.X + ARect.Width / 2, ARect.Y + W * 2, ARect.X + ARect.Width, ARect.Y);
  GPath.AddPolygon(PGPPointF(P), Count);
  GPath.AddPath(GPath2, False);
  G.DrawPath(GPen, GPath);
  GPen.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawSendGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  Count := 7;
  SetLength(P, Count);
  W := ARect.Height / 12;
  ARect.X := ARect.X + W;
  P[0].X := ARect.X;
  P[0].Y := ARect.Y;
  P[1].X := P[0].X;
  P[1].Y := ARect.Y + ARect.Height / 2 - W;
  P[2].X := ARect.X + ARect.Width / 2;
  P[2].Y := P[1].Y;
  P[3].X := P[2].X;
  P[3].Y := P[1].Y + W * 2;
  P[4].X := ARect.X;
  P[4].Y := P[3].Y;
  P[5].X := ARect.X;
  P[5].Y := ARect.Y + ARect.Height;
  P[6].X := ARect.X + ARect.Width;
  P[6].Y := ARect.Y + ARect.Height / 2;
  GPath.AddPolygon(PGPPointF(P), Count);
  G.DrawPath(GPen, GPath);
  GPen.Free;
  GPath.Free;
end;

procedure GPDrawPhotoGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
  R: TGPRectF;
  B: TGPSolidBrush;
begin
   if AThickness < 1 then
    AThickness := 1;

  W := ARect.Height / 6;
  Count := 8;
  SetLength(P, Count);

  ARect.Y := ARect.Y + W / 2;
  ARect.Height := ARect.Height - W;

  P[0].X := ARect.X;
  P[0].Y := ARect.Y + W;
  P[1].X := ARect.X + ARect.Width / 4;
  P[1].Y := ARect.Y + W;
  P[2].X := ARect.X + ARect.Width / 3;
  P[2].Y := ARect.Y;
  P[3].X := ARect.X + ARect.Width - ARect.Width / 3;
  P[3].Y := ARect.Y;
  P[4].X := ARect.X + ARect.Width - ARect.Width / 4;
  P[4].Y := ARect.Y + W;
  P[5].X := ARect.X + ARect.Width;
  P[5].Y := ARect.Y + W;
  P[6].X := ARect.X + ARect.Width;
  P[6].Y := ARect.Y + ARect.Height;
  P[7].X := ARect.X;
  P[7].Y := ARect.Y + ARect.Height;

  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  G.DrawPath(GPen, GPath);

  R.X := ARect.X + ARect.Width / 2 - W * 1.25;
  R.Y := ARect.Y + W * 1.7;
  R.Width := W * 2.5;
  R.Height := W * 2.5;
  G.DrawEllipse(GPen, R);

  B := TGPSolidBrush.Create(AGlyphColor);
  R.X := P[0].X + W / 2;
  R.Y := P[0].Y + W / 2;
  R.Width := W;
  R.Height := W;
  G.FillEllipse(B, R);

  GPath.Free;
  GPen.Free;
  B.Free;
end;

procedure GPDrawVideoGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
begin
   if AThickness < 1 then
    AThickness := 1;

  W := ARect.Height / 6;

  ARect.Y := ARect.Y + W;
  ARect.Height := ARect.Height - W * 2;
  ARect.Width := ARect.Width - W;
  ARect.X := ARect.X - W / 4;

  GPath := TGPGraphicsPath.Create;
  GPath.AddRectangle(ARect);
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 - W / 2,
    ARect.X + ARect.Width + W * 1.5, ARect.Y + ARect.Height / 2 - W * 1.5);
  GPath.AddLine(ARect.X + ARect.Width + W * 1.5, ARect.Y + ARect.Height / 2 - W * 1.5,
   ARect.X + ARect.Width + W * 1.5, ARect.Y + ARect.Height / 2 + W * 1.5);

  GPath.AddLine(ARect.X + ARect.Width + W * 1.5, ARect.Y + ARect.Height / 2 + W * 1.5,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2);

  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));

  G.DrawPath(GPen, GPath);

  GPath.Free;
  GPen.Free;
end;

procedure GPDrawColorMarker(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AColorValue: TColor; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
  B: TGPSolidBrush;
  C: Cardinal;
  FillR, FrameR: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 10;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  InflateGPRect(ARect, -W, -W);
  FillR := ARect;
  FrameR := ARect;
  W := P.GetWidth;
  InflateGPRect(FrameR, -W / 2, -W / 2);
  InflateGPRect(FillR, -W * 0.9, - W * 0.9);
  C := ColorToGPColor(AColorValue, 255);
  B := TGPSolidBrush.Create(C);
  G.FillEllipse(B, FillR);
  G.DrawEllipse(P, FrameR);
  B.Free;
  P.Free;
end;

procedure GPDrawFillColorMarker(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AColorValue: TColor; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
  B: TGPSolidBrush;
  C: Cardinal;
  FillR, FrameR, R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 4;

  R := ARect;
  R.Height := R.Height - W;
  R.Width := R.Height;
  R.X := R.X + W / 2;
  GPDrawFillGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);

  W := ARect.Height / 10;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R.Y := R.Y + R.Height + P.GetWidth;
  R.Height := W * 2;
  if R.Height < P.GetWidth * 3 then R.Height := P.GetWidth * 3;

  FillR := R;
  FrameR := R;
  W := P.GetWidth / 2;
  if W < 1 then W := 1;
  P.SetWidth(W);
  InflateGPRect(FrameR, -W / 2, -W / 2);
  InflateGPRect(FillR, -W * 0.9, - W * 0.9);
  C := ColorToGPColor(AColorValue, 255);
  B := TGPSolidBrush.Create(C);
  G.FillRectangle(B, FillR);
  G.DrawRectangle(P, FrameR);
  B.Free;
  P.Free;
end;
procedure GPDrawFontColorMarker(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AColorValue: TColor; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
  B: TGPSolidBrush;
  C: Cardinal;
  FillR, FrameR, R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 4;

  R := ARect;
  R.Height := R.Height - W;
  R.Width := R.Height;
  R.X := R.X + W / 2;
  GPDrawFontGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);

  W := ARect.Height / 10;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R.Y := R.Y + R.Height + P.GetWidth;
  R.Height := W * 2;
  if R.Height < P.GetWidth * 3 then R.Height := P.GetWidth * 3;

  FillR := R;
  FrameR := R;
  W := P.GetWidth / 2;
  if W < 1 then W := 1;
  P.SetWidth(W);
  InflateGPRect(FrameR, -W / 2, -W / 2);
  InflateGPRect(FillR, -W * 0.9, - W * 0.9);
  C := ColorToGPColor(AColorValue, 255);
  B := TGPSolidBrush.Create(C);
  G.FillRectangle(B, FillR);
  G.DrawRectangle(P, FrameR);
  B.Free;
  P.Free;
end;

procedure GPDrawClockGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
  GPath, GPath2: TGPGraphicsPath;
  CX, CY: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.AddEllipse(ARect);
  CX := ARect.X + ARect.Width / 2;
  CY := ARect.Y + ARect.Height / 2;
  W := ARect.Width / 10;
  ARect.X := CX - W;
  ARect.Y := CY - W;
  ARect.Width := W * 2;
  ARect.Height := W * 2;
  GPath.AddLine(CX + W - P.GetWidth, CY + W - P.GetWidth, CX + W + ARect.Width * 0.8, CY + W + ARect.Height * 0.8);
  GPath.AddArc(ARect, 45, 359.99);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(CX, CY - W + P.GetWidth / 2, CX, CY - W - ARect.Height * 1.6);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  GPath.Free;
  GPath2.Free;
  P.Free;
end;


procedure GPDrawFillGlyph(G: TGPGraphics; ARect: TGPRectF;

  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

var

  GPen: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
  W: Single;
  R: TGPRectF;
begin
  Count := 5;
  SetLength(P, Count);
  W := ARect.Width / 6;
  ARect.X := ARect.X + W / 2;
  P[0].X := ARect.X;
  P[0].Y := ARect.Y + ARect.Height / 2;
  P[1].X := ARect.X + W;
  P[1].Y := ARect.Y + ARect.Height / 2;
  P[2].X := ARect.X + ARect.Width / 2 + W / 2;
  P[2].Y := ARect.Y + ARect.Height - W / 2;
  P[3].X := ARect.X + ARect.Width;
  P[3].Y := ARect.Y + ARect.Height / 2;
  P[4].X := ARect.X + ARect.Width / 2;
  P[4].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  GPath2 := TGPGraphicsPath.Create;
  R.Width := W;
  R.Height := W;
  R.Y := P[0].Y + W * 1.5;
  R.X := P[0].X - W * 0.8;
  GPath2.AddArc(R, 0, 180);
  GPath2.AddLine(R.X, R.Y + W / 2, R.X + R.Width / 2, R.Y - W / 2);
  GPath2.AddLine(R.X + R.Width, R.Y + W / 2, R.X + R.Width / 2, R.Y - W / 2);
  GPath.AddPath(GPath2, False);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPen.Free;
end;


procedure GPDrawFontGlyph(G: TGPGraphics; ARect: TGPRectF;

  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

var

  GPen: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
begin
  Count := 4;
  SetLength(P, Count);
  ARect.Y := ARect.Y - ARect.Height / 12;
  P[0].X := ARect.X + ARect.Width / 2;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X + ARect.Width / 2 - ARect.Width / 4;
  P[1].Y := ARect.Y + ARect.Height * 0.6;
  P[2].X := ARect.X + ARect.Width / 2 + ARect.Width / 4;
  P[2].Y := ARect.Y + ARect.Height * 0.6;
  P[3].X := ARect.X + ARect.Width / 2;
  P[3].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLines(PGPPointF(P), Count);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(P[2].X, P[2].Y, P[2].X + ARect.Width * 0.165, ARect.Y + ARect.Height);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(P[1].X - ARect.Width * 0.165, ARect.Y + ARect.Height, P[1].X, P[1].Y);
  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPen.Free;
end;


procedure GPDrawFontIncSizeGlyph(G: TGPGraphics; ARect: TGPRectF;

    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R: TGPRectF;
  W: Single;
begin
  W := ARect.Width / 8;
  R := ARect;
  R.X := R.X - W;
  InflateGPRect(R, -W, -W);
  GPDrawFontGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);
  R := ARect;
  R.Width := W * 3;
  R.Height := W * 3;
  R.X := ARect.X + ARect.Width - R.Width;
  GPDrawPlusGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);
end;

procedure GPDrawFontDecSizeGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R: TGPRectF;
  W: Single;
begin
  W := ARect.Width / 8;
  R := ARect;
  R.X := R.X - W;
  InflateGPRect(R, -W * 1.6, -W * 1.6);
  GPDrawFontGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);
  R := ARect;
  R.Width := W * 3;
  R.Height := W * 3;
  R.X := ARect.X + ARect.Width - R.Width;
  GPDrawMinusGlyph(G, R,
    AGlyphColor, AScaleFactor, AThickness);
end;



procedure GPDrawChartGlyph(G: TGPGraphics; ARect: TGPRectF;

    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

var

  P: TGPPen;

  R: TGPRectF;
  GPath: TGPGraphicsPath;

  W: Single;

begin

 if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  R := ARect;
  W := ARect.Width / 4;
  R.Width := W;
  R.Y := R.Y + W;
  R.Height := R.Height - W;
  GPath.AddRectangle(R);
  R.Y := R.Y + W;
  R.Height := R.Height - W;
  R.X := R.X + W * 1.5;
  GPath.AddRectangle(R);
  R := ARect;
  R.Y := ARect.Y + W / 6;
  R.Height := ARect.Height - W / 6;
  R.X := R.X + W * 3;
  R.Width := W;
  GPath.AddRectangle(R);
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;


procedure GPDrawSizeFullGlyph(G: TGPGraphics; ARect: TGPRectF;

    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3, GPath4: TGPGraphicsPath;
  W: Single;
begin
 if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;
  GPath4 := TGPGraphicsPath.Create;
  W := ARect.Width / 8;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W, ARect.Y,
    ARect.X, ARect.Y);
  GPath.AddLine(ARect.X, ARect.Y + ARect.Height / 2 - W,
    ARect.X, ARect.Y);

  GPath2.AddLine(ARect.X + ARect.Width / 2 + W, ARect.Y,
    ARect.X + ARect.Width, ARect.Y);
  GPath2.AddLine(ARect.X + ARect.Width, ARect.Y + ARect.Height / 2 - W,
    ARect.X + ARect.Width, ARect.Y);

  GPath3.AddLine(ARect.X + ARect.Width / 2 + W, ARect.Y + ARect.Height,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);
  GPath3.AddLine(ARect.X + ARect.Width, ARect.Y + ARect.Height / 2 + W,
    ARect.X + ARect.Width, ARect.Y + ARect.Height);

  GPath4.AddLine(ARect.X + ARect.Width / 2 - W, ARect.Y + ARect.Height,
    ARect.X, ARect.Y + ARect.Height);
  GPath4.AddLine(ARect.X, ARect.Y + ARect.Height / 2 + W,
    ARect.X, ARect.Y + ARect.Height);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  GPath.AddPath(GPath4, False);
  G.DrawPath(P, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPath4.Free;
  P.Free;
end;

procedure GPDrawSizeCompactGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);

var

  P: TGPPen;
  GPath, GPath2, GPath3, GPath4: TGPGraphicsPath;
  W: Single;
begin
 if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;
  GPath4 := TGPGraphicsPath.Create;
  W := ARect.Width / 6;
  GPath.AddLine(ARect.X + ARect.Width / 2 - W, ARect.Y,
    ARect.X + ARect.Width / 2 - W, ARect.Y + W * 2);
  GPath.AddLine(ARect.X, ARect.Y + ARect.Height / 2 - W,
    ARect.X + ARect.Width / 2 - W, ARect.Y + W * 2);

  GPath2.AddLine(ARect.X + ARect.Width / 2 + W, ARect.Y,
    ARect.X + ARect.Width / 2 + W, ARect.Y + W * 2);
  GPath2.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y +  W * 2,
    ARect.X + ARect.Width, ARect.Y + W * 2);

  GPath3.AddLine(ARect.X + ARect.Width / 2 + W, ARect.Y + ARect.Height,
    ARect.X + ARect.Width / 2 + W, ARect.Y + ARect.Height - W * 2);
  GPath3.AddLine(ARect.X + ARect.Width / 2 + W,
    ARect.Y + ARect.Height - W * 2,
    ARect.X + ARect.Width, ARect.Y + ARect.Height - W * 2);

  GPath4.AddLine(ARect.X + ARect.Width / 2 - W, ARect.Y + ARect.Height,
    ARect.X + ARect.Width / 2 - W, ARect.Y + ARect.Height - W * 2);
  GPath4.AddLine(ARect.X + ARect.Width / 2 - W,
    ARect.Y + ARect.Height - W * 2,
    ARect.X, ARect.Y + ARect.Height - W * 2);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  GPath.AddPath(GPath4, False);

  G.DrawPath(P, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPath4.Free;
  P.Free;
end;


procedure GPDrawCalendarGlyph(G: TGPGraphics; ARect: TGPRectF;

    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  P: TGPPen;
  R: TGPRectF;
  GPath, GPath2, GPath3, GPath4: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  R := ARect;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.AddRectangle(R);
  GPath.AddLine(R.X, R.Y + R.Height / 4,
  R.X + R.Width, R.Y + R.Height / 4);

  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(R.X + R.Width / 4,
    R.Y - R.Height / 10,
    R.X + R.Width / 4, R.Y + R.Height / 10);

  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(R.X + R.Width - R.Width / 4,
    R.Y - R.Height / 10,
    R.X + R.Width - R.Width / 4, R.Y + R.Height / 10);

  GPath4 := TGPGraphicsPath.Create;
  R := ARect;
  R.Y := R.Y + R.Height / 4;
  R.Height := R.Height - R.Height / 4;
  InflateGPRect(R, -R.Height / 4, -R.Height / 4);
  GPath4.AddRectangle(R);
  R := ARect;
  R.Y := R.Y + R.Height / 4;
  R.Height := R.Height - R.Height / 4;
  InflateGPRect(R, -R.Height / 4, -R.Height / 4);
  R.X := R.X + R.Width / 3;
  R.Width := R.Width / 3;
  GPath4.AddRectangle(R);
  R := ARect;
  R.Y := R.Y + R.Height / 4;
  R.Height := R.Height - R.Height / 4;
  InflateGPRect(R, -R.Height / 4, -R.Height / 4);
  R.Y := R.Y + R.Height / 3;
  R.Height := R.Height / 3;
  GPath4.AddRectangle(R);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  GPath.AddPath(GPath4, False);
  G.DrawPath(P, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPath4.Free;
  P.Free;
end;


procedure GPDrawCircleGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 6;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  InflateGPRect(ARect, -W, -W);
  G.DrawEllipse(P, ARect);
  P.Free;
end;

procedure GPDrawCircleFillGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  W: Single;
  B: TGPSolidBrush;
begin
  W := ARect.Height / 6;
  InflateGPRect(ARect, -W, -W);
  B := TGPSolidBrush.Create(AGlyphColor);
  G.FillEllipse(B, ARect);
  B.Free;
end;

procedure GPDrawLockGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPath, GPath2: TGPGraphicsPath;
  P: TGPPen;
  W: Single;
  R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 8;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  InflateGPRect(ARect, -W, -W);
  ARect.Y := ARect.Y + W * 2;
  ARect.Height := ARect.Height - W;
  GPath.AddRectangle(ARect);
  R.X := ARect.X + ARect.Width / 6;
  R.Width := ARect.Width - ARect.Width / 3;
  R.Y := ARect.Y - R.Width * 0.8;
  R.Height := R.Width;
  GPath.AddArc(R, 270, 90);
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 6, ARect.Y - R.Width * 0.2,
    ARect.X + ARect.Width - ARect.Width / 6, ARect.Y);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddArc(R, 270, -90);
  GPath2.AddLine(ARect.X + ARect.Width / 6, ARect.Y,
    ARect.X + ARect.Width / 6, ARect.Y - R.Width * 0.2);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawUnLockGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPath, GPath2: TGPGraphicsPath;
  P: TGPPen;
  W: Single;
  R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 8;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  InflateGPRect(ARect, -W, -W);
  ARect.Y := ARect.Y + W * 2;
  ARect.Height := ARect.Height - W;
  GPath.AddRectangle(ARect);
  R.X := ARect.X + ARect.Width / 6;
  R.Width := ARect.Width - ARect.Width / 3;
  R.Y := ARect.Y - R.Width * 0.8;
  R.Height := R.Width;
  GPath.AddArc(R, 270, 45);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddArc(R, 270, -90);
  GPath2.AddLine(ARect.X + ARect.Width / 6, ARect.Y,
    ARect.X + ARect.Width / 6, ARect.Y - R.Width * 0.2);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawUserGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R: TGPRectF;
  P: TGPPen;
  GPath: TGPGraphicsPath;
begin
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  R := ARect;
  R.Y := R.Y + ARect.Height * 0.6;
  GPath := TGPGraphicsPath.Create;
  GPath.AddArc(R, 0, -180);
  R.Width := ARect.Width * 0.7;
  R.Height := R.Width;
  R.X := ARect.X + ARect.Width / 2 - R.Width / 2;
  R.Y := ARect.Y - R.Height * 0.2;
  GPath.AddEllipse(R);
  G.DrawPath(P, GPath);
  GPath.Free;
  P.Free;
end;

procedure GPDrawTrashGlyph(G: TGPGraphics; ARect: TGPRectF;
   AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
begin
  W := ARect.Height / 5;
  ARect.Y := ARect.Y + W;
  ARect.Height := ARect.Height - W * 1.5;
  ARect.X := ARect.X + W;
  ARect.Width := ARect.Width - W * 2;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddRectangle(ARect);
  GPath.AddLine(ARect.X, ARect.Y, ARect.X - W / 1.5, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width, ARect.Y, ARect.X +  ARect.Width + W / 1.5, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 4,
    ARect.Y, ARect.X + ARect.Width / 4, ARect.Y - W * 0.7);
  GPath.AddLine(ARect.X + ARect.Width / 4, ARect.Y - W * 0.7,
    ARect.X + ARect.Width - ARect.Width / 4, ARect.Y - W * 0.7);
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 4, ARect.Y - W * 0.7,
    ARect.X + ARect.Width - ARect.Width / 4, ARect.Y);
  G.DrawPath(GPen, GPath);

 G.DrawLine(GPen, ARect.X + ARect.Width / 2, ARect.Y + W * 0.7,
    ARect.X + ARect.Width / 2, ARect.Y + ARect.Height - W * 0.7);
  G.DrawLine(GPen, ARect.X + ARect.Width / 4, ARect.Y + W * 0.7,
    ARect.X + ARect.Width / 4, ARect.Y + ARect.Height - W * 0.7);
  G.DrawLine(GPen, ARect.X + ARect.Width - ARect.Width / 4, ARect.Y + W * 0.7,
    ARect.X + ARect.Width - ARect.Width / 4, ARect.Y + ARect.Height - W * 0.7);
  GPath.Free;
  GPen.Free;
end;

procedure GPDrawPinGlyph(G: TGPGraphics; ARect: TGPRectF;
   AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
begin
  InflateGPRect(ARect, -ARect.Width / 6, 0);
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddLine(ARect.X + ARect.Width / 4, ARect.Y,
    ARect.X + ARect.Width / 4 + ARect.Width / 2, ARect.Y);
  GPath.AddLine(ARect.X + ARect.Width / 4 + ARect.Width / 2, ARect.Y,
    ARect.X + ARect.Width - ARect.Width / 6, ARect.Y + ARect.Height / 2);
  GPath.AddLine(ARect.X + ARect.Width - ARect.Width / 6, ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 6, ARect.Y + ARect.Height / 2);
  GPath.AddLine(ARect.X + ARect.Width / 6, ARect.Y + ARect.Height / 2,
    ARect.X + ARect.Width / 4, ARect.Y);

  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X,
    ARect.Y + ARect.Height / 2, ARect.X + ARect.Width / 4, ARect.Y + ARect.Height / 2);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2, ARect.X + ARect.Width - ARect.Width / 4, ARect.Y + ARect.Height / 2);
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height / 2, ARect.X + ARect.Width / 2, ARect.Y + ARect.Height);

  GPath.AddPath(GPath2, False);
  G.DrawPath(GPen, GPath);

  GPath.Free;
  GPen.Free;
end;


procedure GPDrawCallGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
  W: Single;
  R: TGPRectF;
begin
   if AThickness < 1 then
    AThickness := 1;
  Count := 4;
  W := ARect.Width / 8;
  ARect.Y := ARect.Y + W / 2;
  ARect.X := ARect.X - W / 2;
  SetLength(P, Count);
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  R := ARect;
  InflateGPRect(R, R.Width / 2, R.Width / 2);
  R.X := R.X + R.Width / 4;
  R.Y := R.Y - R.Height / 4;
  P[0].X := ARect.X + W;
  P[0].Y := ARect.Y + W;
  P[1].X := ARect.X + ARect.Width / 3 - W * 2;
  P[1].Y := ARect.Y + ARect.Height / 3;
  P[2].X := ARect.X + ARect.Width - ARect.Width / 3;
  P[2].Y := ARect.Y + ARect.Height - ARect.Height / 3 + W * 2;
  P[3].X := ARect.X + ARect.Width - W;
  P[3].Y := ARect.Y + ARect.Height - W;
  GPath.AddBeziers(PGPPointF(P), Count);

  GPath2.AddLine(P[0].X, P[0].Y, P[0].X + W * 1.5, P[0].Y - W * 1.5);
  GPath2.AddLine(P[0].X + W * 1.5, P[0].Y - W * 1.5,
    P[0].X + W * 3, P[0].Y);
  GPath2.AddLine(P[0].X + W * 3, P[0].Y,
    P[0].X + W * 2, P[0].Y + W);
  GPath2.AddLine(P[3].X - W, P[3].Y - W * 3 + W,
    P[3].X, P[3].Y - W * 3);
  GPath2.AddLine(P[3].X + W * 1.5, P[3].Y - W * 1.5,
    P[3].X, P[3].Y);

  GPath.AddPath(GPath2, False);

  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPen.Free;
end;

procedure GPDrawCallEndGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  GPen: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  P: array of TGPPointF;
  Count: Integer;
  W: Single;
  R: TGPRectF;
begin
   if AThickness < 1 then
    AThickness := 1;
  Count := 4;
  W := ARect.Width / 8;
  ARect.Y := ARect.Y + W / 2;
  ARect.X := ARect.X - W / 2;
  SetLength(P, Count);
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;

  R := ARect;
  InflateGPRect(R, R.Width / 2, R.Width / 2);
  R.X := R.X + R.Width / 4;
  R.Y := R.Y - R.Height / 4;
  P[0].X := ARect.X + W;
  P[0].Y := ARect.Y + W;
  P[1].X := ARect.X + ARect.Width / 3 - W * 2;
  P[1].Y := ARect.Y + ARect.Height / 3;
  P[2].X := ARect.X + ARect.Width - ARect.Width / 3;
  P[2].Y := ARect.Y + ARect.Height - ARect.Height / 3 + W * 2;
  P[3].X := ARect.X + ARect.Width - W;
  P[3].Y := ARect.Y + ARect.Height - W;
  GPath.AddBeziers(PGPPointF(P), Count);

  GPath2.AddLine(P[0].X, P[0].Y, P[0].X + W * 1.5, P[0].Y - W * 1.5);
  GPath2.AddLine(P[0].X + W * 1.5, P[0].Y - W * 1.5,
    P[0].X + W * 3, P[0].Y);
  GPath2.AddLine(P[0].X + W * 3, P[0].Y,
    P[0].X + W * 2, P[0].Y + W);
  GPath2.AddLine(P[3].X - W, P[3].Y - W * 3 + W,
    P[3].X, P[3].Y - W * 3);
  GPath2.AddLine(P[3].X + W * 1.5, P[3].Y - W * 1.5,
    P[3].X, P[3].Y);

  GPath.AddPath(GPath2, False);

  GPath3.AddLine(ARect.X + ARect.Width, ARect.Y,
    ARect.X + W, ARect.Y + ARect.Height - W);

  GPath.AddPath(GPath3, False);

  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPen.Free;
end;


procedure GPDrawCutGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  R: TGPRectF;
  GPath, GPath2, GPath3, GPath4: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Width / 3;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  R.X := ARect.X;
  R.Y := ARect.Y;
  R.Width := W;
  R.Height := W;
  GPath.AddLine(ARect.X + ARect.Width, ARect.Y + ARect.Height,
    ARect.X + W, ARect.Y + W);
  GPath.AddArc(R, 45, 359.99);
  R.X := ARect.X;
  R.Y := ARect.Y + ARect.Height - W;
  R.Width := W;
  R.Height := W;
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width, ARect.Y,
    ARect.X + W, ARect.Y + ARect.Height - W);
  GPath2.AddArc(R, -45, 359.99);
  GPath.AddPath(GPath2, False);
  GPath3 := TGPGraphicsPath.Create;
  Count := 3;
  SetLength(P, Count);
  P[0].X := ARect.X + ARect.Width;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X + ARect.Width - ARect.Width / 3;
  P[1].Y := ARect.Y + ARect.Height / 6;
  P[2].X := ARect.X + ARect.Width / 2;
  P[2].Y := ARect.Y + ARect.Height / 2;
  GPath3.AddCurve(PGPPointF(P),  Count);

  GPath4 := TGPGraphicsPath.Create;
  P[0].X := ARect.X + ARect.Width;
  P[0].Y := ARect.Y + ARect.Height;
  P[1].X := ARect.X + ARect.Width - ARect.Width / 3;
  P[1].Y := ARect.Y + ARect.Height - ARect.Height / 6;
  P[2].X := ARect.X + ARect.Width / 2;
  P[2].Y := ARect.Y + ARect.Height / 2;
  GPath4.AddCurve(PGPPointF(P),  Count);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  GPath.AddPath(GPath4, False);

  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPath4.Free;
  GPen.Free;
end;


procedure GPDrawHelpGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: array of TGPPointF;
  Count: Integer;
  GPen: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  R: TGPRectF;
begin

  if AThickness < 1 then

    AThickness := 1;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;

  Count := 3;
  SetLength(P, Count);
  R := ARect;
  InflateGPRect(R, -ARect.Width / 5, -ARect.Width / 5);
  R.Y := R.Y - ARect.Width / 4;
  GPath.AddArc(R, 60, -225);
  R.Y := R.Y + R.Height;
  R.X := R.X + R.Width / 2;
  R.Width := ARect.Width / 4;
  R.Height := ARect.Width / 4;
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddArc(R, 180, 60);
  GPath2.AddPath(GPath, True);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(R.X,
  R.Y + R.Height / 2,
  R.X, ARect.Y + ARect.Height - ARect.Height / 5);
  GPath2.AddPath(GPath3, False);
  R.X := ARect.X + ARect.Width / 2 - ARect.Height / 20;
  R.Y := ARect.Y + ARect.Height - ARect.Height / 20;
  R.Width := ARect.Height / 10;
  R.Height := ARect.Height / 10;
  GPath2.AddEllipse(R);
  G.DrawPath(GPen, GPath2);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPen.Free;
end;

procedure GPDrawPasteGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  GPen: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
  P: array of TGPPointF;
  Count: Integer;
begin
  W := ARect.Height / 6;
  ARect.Y := ARect.Y + W / 2;
  ARect.Height := ARect.Height - W;
  Count := 10;
  SetLength(P, Count);
  P[0].X := ARect.X + W;
  P[0].Y := ARect.Y;
  P[1].X := ARect.X + W;
  P[1].Y := ARect.Y + ARect.Height;
  P[2].X := ARect.X + ARect.Width / 2;
  P[2].Y := ARect.Y + ARect.Height;
  P[3].X := ARect.X + ARect.Width / 2;
  P[3].Y := ARect.Y + ARect.Height / 2;
  P[4].X := ARect.X + ARect.Width - W;
  P[4].Y := ARect.Y + ARect.Height / 2;
  P[5].X := ARect.X + ARect.Width - W;
  P[5].Y := ARect.Y;
  P[6].X := ARect.X + ARect.Width - ARect.Width * 0.4;
  P[6].Y := ARect.Y;
  P[7].X := ARect.X + ARect.Width - ARect.Width * 0.4;
  P[7].Y := ARect.Y - W / 2;
  P[8].X := ARect.X + ARect.Width * 0.4;
  P[8].Y := ARect.Y - W / 2;
  P[9].X := ARect.X + ARect.Width * 0.4;
  P[9].Y := ARect.Y;
  GPath := TGPGraphicsPath.Create;
  GPen := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath.AddPolygon(PGPPointF(P), Count);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(P[0].X + W * 0.7, P[0].Y,
   P[0].X + W * 0.7, P[0].Y + W);
  GPath2.AddLine(P[5].X - W * 0.7, P[0].Y + W,
   P[5].X - W * 0.7, P[0].Y);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(P[4].X, P[4].Y,
   P[4].X + W / 2, P[4].Y);
  GPath3.AddLine(P[4].X + W / 2, P[4].Y,
    P[4].X + W / 2, P[1].Y + W / 2);
  GPath3.AddLine(P[4].X + W / 2, P[1].Y + W / 2,
    P[2].X, P[2].Y + W / 2);
  GPath3.AddLine(P[2].X, P[2].Y + W / 2,
    P[2].X, P[2].Y);
  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);
  G.DrawPath(GPen, GPath);
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPen.Free;
end;

procedure GPDrawCopyGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  R: TGPRectF;
  Offset: Single;
  W: Single;
begin
  W := ARect.Height / 6;
  ARect.X := ARect.X - W / 3;
  R := ARect;
  InflateGPRect(R, -W, -W);
  R.X := R.X + W / 2;
  R.Width := R.Width  - W / 2;
  Offset := ARect.Height / 10;
  R.X := R.X - Offset;
  R.Y := R.Y + Offset;
  GPDrawRectGlyph2(G, R, AGlyphColor, AScaleFactor, AThickness);
  G.ExcludeClip(R);
  R := ARect;
  InflateGPRect(R, -W, -W);
  R.X := R.X + W / 2;
  R.Width := R.Width  - W / 2;
  R.X := R.X + Offset;
  R.Y := R.Y - Offset;
  GPDrawRectGlyph2(G, R, AGlyphColor, AScaleFactor, AThickness);
  G.ResetClip;
end;


procedure GPDrawCheckGlyph2(GPath: TGPGraphicsPath; ARect: TGPRectF);
begin
  ARect.Y := Round(ARect.Y - ARect.Height / 10);
  GPath.AddLine(ARect.X - ARect.Width / 8, ARect.Y + ARect.Height - ARect.Height / 3,
     ARect.X + ARect.Width / 3, ARect.Y + ARect.Height);
  GPath.AddLine(ARect.X + ARect.Width / 3, ARect.Y + ARect.Height,
     ARect.X + ARect.Width, ARect.Y +  ARect.Height / 4);
end;

procedure GPDrawCheckOptionsGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
  GPath, GPath2: TGPGraphicsPath;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 4;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  InflateGPRect(ARect, -W, -W);
  GPath := TGPGraphicsPath.Create;
  GPath.AddRectangle(ARect);
  InflateGPRect(ARect, W * 0.7, W * 0.7);
  ARect.X := ARect.X + W / 2;
  ARect.Y := ARect.Y - W;
  GPath2 := TGPGraphicsPath.Create;
  GPDrawCheckGlyph2(GPath2, ARect);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);
  GPath.Free;
  GPath2.Free;
  P.Free;
end;

procedure GPDrawPauseGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 4;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawLine(P, ARect.X + ARect.Width / 2 - W, ARect.Y,
    ARect.X + ARect.Width / 2 - W,
    ARect.Y + ARect.Height);
  G.DrawLine(P, ARect.X + ARect.Width / 2 + W, ARect.Y,
    ARect.X + ARect.Width / 2 + W,
    ARect.Y + ARect.Height);
  P.Free;
end;

procedure GPDrawSearchGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  CX, CY: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  CX := ARect.X + ARect.Width * 0.65;
  CY := ARect.Y + ARect.Height * 0.65;
  GPath := TGPGraphicsPath.Create;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height,
    CX, CY);
  ARect.Width := CX - ARect.X + P.GetWidth / 2;
  ARect.Height := CY - ARect.Y + P.GetWidth / 2;
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddArc(ARect, -315, 359.99);
  GPath.AddPath(GPath2, True);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawZoomPlusGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  CX, CY: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  CX := ARect.X + ARect.Width * 0.65;
  CY := ARect.Y + ARect.Height * 0.65;
  GPath := TGPGraphicsPath.Create;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height,
    CX, CY);
  ARect.Width := CX - ARect.X + P.GetWidth / 2;
  ARect.Height := CY - ARect.Y + P.GetWidth / 2;
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddArc(ARect, -315, 359.99);
  GPath.AddPath(GPath2, True);
  G.DrawPath(P, GPath);
  CX := ARect.Width / 6;
  InflateGPRect(ARect, -CX, -CX);
  GPDrawPlusGlyph(G, ARect,
    AGlyphColor, AScaleFactor, AThickness);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawZoomMinusGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  CX, CY: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  CX := ARect.X + ARect.Width * 0.65;
  CY := ARect.Y + ARect.Height * 0.65;
  GPath := TGPGraphicsPath.Create;
  GPath.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height,
    CX, CY);
  ARect.Width := CX - ARect.X + P.GetWidth / 2;
  ARect.Height := CY - ARect.Y + P.GetWidth / 2;
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddArc(ARect, -315, 359.99);
  GPath.AddPath(GPath2, True);
  G.DrawPath(P, GPath);
  CX := ARect.Width / 6;
  InflateGPRect(ARect, -CX, -CX);
  GPDrawMinusGlyph(G, ARect,
    AGlyphColor, AScaleFactor, AThickness);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawShutDownGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawArc(P, ARect, -60, 300);
  G.DrawLine(P, ARect.X + ARect.Width / 2, ARect.Y,
    ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height / 2);
  P.Free;
end;

procedure GPDrawInfoGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  B: TGPSolidBrush;
  R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));

  R.X := ARect.X + ARect.Width / 2 - P.GetWidth;
  R.Y := ARect.Y + ARect.Height / 6;
  R.Width := P.GetWidth * 2;
  R.Height := R.Width;
  B := TGPSolidBrush.Create(AGlyphColor);
  G.FillEllipse(B, R);

  G.DrawEllipse(P, ARect);
  G.DrawLine(P, ARect.X + ARect.Width / 2, R.Y + R.Height + ARect.Height / 8,
    ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height - ARect.Height / 5);

  P.Free;
  B.Free;
end;

procedure GPDrawMicGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2: TGPGraphicsPath;
  W: Single;
  LArc, RArc, R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Width / 4;
  R.X := ARect.X + ARect.Width / 2 - W / 2;
  R.Y := ARect.Y;
  R.Width := W;
  R.Height := ARect.Height / 5;
  GPath := TGPGraphicsPath.Create;

  LArc.X := R.X;
  LArc.Y := R.Y;
  LArc.Width := R.Width;
  LArc.Height := LArc.Width;
  RArc.X := R.X;
  RArc.Y := R.Y + R.Height * 2;
  RArc.Width := R.Width;
  RArc.Height := RArc.Width;
  GPath.StartFigure;
  GPath.AddArc(LArc, -180, 180);
  GPath.AddArc(RArc, 0, 180);
  GPath.CloseFigure;

  InflateGPRect(RArc, W / 2, W / 2);
  GPath.AddArc(RArc, 0, 180);

  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    RArc.Y + RArc.Height, ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X + ARect.Width / 2 - R.Width / 2,
    ARect.Y + ARect.Height, ARect.X + ARect.Width / 2 + R.Width / 2,
    ARect.Y + ARect.Height);
  GPath.AddPath(GPath2, False);

  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawMicGlyph2(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
  W: Single;
  LArc, RArc, R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Width / 2.5;
  R.X := ARect.X + ARect.Width / 2 - W / 2;
  R.Y := ARect.Y;
  R.Width := W;
  R.Height := ARect.Height / 5;
  GPath := TGPGraphicsPath.Create;

  LArc.X := R.X;
  LArc.Y := R.Y;
  LArc.Width := R.Width;
  LArc.Height := LArc.Width;
  RArc.X := R.X;
  RArc.Y := R.Y + R.Height * 2;
  RArc.Width := R.Width;
  RArc.Height := RArc.Width;
  GPath.StartFigure;
  GPath.AddArc(LArc, -180, 180);
  GPath.AddArc(RArc, 0, 180);
  GPath.CloseFigure;

  InflateGPRect(RArc, W / 2, W / 2);
  GPath.AddArc(RArc, 0, 180);

  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawMicMuteGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
  LArc, RArc, R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Width / 4;
  R.X := ARect.X + ARect.Width / 2 - W / 2;
  R.Y := ARect.Y;
  R.Width := W;
  R.Height := ARect.Height / 5;
  GPath := TGPGraphicsPath.Create;

  LArc.X := R.X;
  LArc.Y := R.Y;
  LArc.Width := R.Width;
  LArc.Height := LArc.Width;
  RArc.X := R.X;
  RArc.Y := R.Y + R.Height * 2;
  RArc.Width := R.Width;
  RArc.Height := RArc.Width;
  GPath.StartFigure;
  GPath.AddArc(LArc, -180, 180);
  GPath.AddArc(RArc, 0, 180);
  GPath.CloseFigure;

  InflateGPRect(RArc, W / 2, W / 2);
  GPath.AddArc(RArc, 0, 180);

  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width / 2,
    RArc.Y + RArc.Height, ARect.X + ARect.Width / 2,
    ARect.Y + ARect.Height);
  GPath2.AddLine(ARect.X + ARect.Width / 2 - R.Width / 2,
    ARect.Y + ARect.Height, ARect.X + ARect.Width / 2 + R.Width / 2,
    ARect.Y + ARect.Height);

  GPath3 := TGPGraphicsPath.Create;
  InflateGPRect(ARect, - W / 2, -W / 2);
  GPath3.AddLine(ARect.X, ARect.Y, ARect.X + ARect.Width, ARect.Y + ARect.Height);

  GPath.AddPath(GPath2, False);
  GPath.AddPath(GPath3, False);

  G.DrawPath(P, GPath);

  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;

procedure GPDrawRefreshGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 12;
  InflateGPRect(ARect, -W, -W);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddArc(ARect, -315, 315);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width + W * 1.5,
    ARect.Y + ARect.Height / 2 + W / 2 - W * 1.5);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width - W * 1.5,
    ARect.Y + ARect.Height / 2 + W / 2 - W * 1.5);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2);
  GPath3.AddPath(GPath2, False);
  GPath.AddPath(GPath3, True);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;

procedure GPDrawRefreshGlyph2(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  W := ARect.Height / 8;
  GPath.StartFigure;
  GPath.AddArc(ARect, -315, 315);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width + W * 1.5,
    ARect.Y + ARect.Height / 2 + W / 2 - W * 1.5);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width - W * 1.5,
    ARect.Y + ARect.Height / 2 + W / 2 - W * 1.5);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2);
  GPath3.AddPath(GPath2, False);
  GPath.AddPath(GPath3, True);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;

procedure GPDrawReplaceGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
  R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 12;
  InflateGPRect(ARect, -W, -W);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;

  GPath.AddArc(ARect, -135, 135);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width + W * 1.5,
    ARect.Y + ARect.Height / 2 + W / 2 - W * 1.5);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width - W * 1.5,
    ARect.Y + ARect.Height / 2 + W / 2 - W * 1.5);
  GPath3.AddLine(ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2 + W / 2,
    ARect.X + ARect.Width,
    ARect.Y + ARect.Height / 2);
  GPath3.AddPath(GPath2, False);
  GPath.AddPath(GPath3, True);
  G.DrawPath(P, GPath);

  GPath.Reset;
  GPath2.Reset;
  GPath3.Reset;

  GPath.AddArc(ARect, 45, 135);
  GPath2.AddLine(ARect.X,
    ARect.Y + ARect.Height / 2 - W / 2,
    ARect.X + W * 1.5,
    ARect.Y + ARect.Height / 2 - W / 2 + W * 1.5);
  GPath2.AddLine(ARect.X,
    ARect.Y + ARect.Height / 2 - W / 2,
    ARect.X - W * 1.5,
    ARect.Y + ARect.Height / 2 - W / 2 + W * 1.5);
  GPath3.AddLine(ARect.X,
    ARect.Y + ARect.Height / 2,
    ARect.X,
    ARect.Y + ARect.Height / 2 - W / 2);
  GPath3.AddPath(GPath2, False);
  GPath.AddPath(GPath3, True);
  G.DrawPath(P, GPath);

  R.X := ARect.X;
  R.Y := ARect.Y + ARect.Height / 2 - W * 2.7;
  R.Width := W;
  R.Height := W;
  G.DrawEllipse(P, R);

  R.X := ARect.X + ARect.Width - W * 1.1;
  R.Y := ARect.Y + ARect.Height / 2 + W * 1.6;
  R.Width := W;
  R.Height := W;
  G.DrawEllipse(P, R);

  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;

procedure GPDrawRedoGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 4;
  InflateGPRect(ARect, -W / 2, -W / 2);
  ARect.Y := ARect.Y + W / 2;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddArc(ARect, 90, 180);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y,
    ARect.X + ARect.Width - W,
    ARect.Y + W);
  GPath2.AddLine(ARect.X + ARect.Width,
    ARect.Y,
    ARect.X + ARect.Width - W,
    ARect.Y - W);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(ARect.X +ARect.Width,
    ARect.Y,
    ARect.X + ARect.Width - ARect.Width / 3,
    ARect.Y);
  GPath3.AddPath(GPath2, False);
  GPath.AddPath(GPath3, True);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;

procedure GPDrawUndoGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 4;
  InflateGPRect(ARect, -W / 2, -W / 2);
  ARect.Y := ARect.Y + W / 2;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddArc(ARect, 90, -180);
  GPath2 := TGPGraphicsPath.Create;
  GPath2.AddLine(ARect.X,
    ARect.Y,
    ARect.X + W,
    ARect.Y + W);
  GPath2.AddLine(ARect.X,
    ARect.Y,
    ARect.X + W,
    ARect.Y - W);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(ARect.X,
    ARect.Y,
    ARect.X + ARect.Width / 3,
    ARect.Y);
  GPath3.AddPath(GPath2, False);
  GPath.AddPath(GPath3, True);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;

procedure GPDrawReplyGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath, GPath2, GPath3: TGPGraphicsPath;
  W: Single;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 4;
  ARect.X := ARect.X - W / 2;
  ARect.Y := ARect.Y + W;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  GPath := TGPGraphicsPath.Create;
  GPath.StartFigure;
  GPath.AddArc(ARect, 0, -90);
  GPath2 := TGPGraphicsPath.Create;
  ARect.X := ARect.X + W;
  GPath2.AddLine(ARect.X,
    ARect.Y,
    ARect.X + W,
    ARect.Y + W);
  GPath2.AddLine(ARect.X,
    ARect.Y,
    ARect.X + W,
    ARect.Y - W);
  GPath3 := TGPGraphicsPath.Create;
  GPath3.AddLine(ARect.X,
    ARect.Y,
    ARect.X + ARect.Width / 6,
    ARect.Y);
  GPath3.AddPath(GPath2, False);
  GPath.AddPath(GPath3, True);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
end;


procedure GPDrawViewGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
  CX, CY: Single;
  W: Single;
  R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  CX := ARect.X + ARect.Width / 2;
  CY := ARect.Y + ARect.Height / 2;
  W := ARect.Width / 4;
  GPath := TGPGraphicsPath.Create;
  R := ARect;
  R.X := CX - W;
  R.Y := CY - W;
  R.Width := W * 2;
  R.Height := W * 2;
  GPath.AddEllipse(R);
  InflateGPRect(R, -W / 1.5, -W / 1.5);
  ARect.Y := ARect.Y + W / 2;
  GPath.AddArc(ARect, -180, 180);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawObjectsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
  CX, CY: Single;
  W: Single;
  R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  CX := ARect.X + ARect.Width / 2;
  CY := ARect.Y + ARect.Height / 2;
  W := ARect.Width / 4;
  GPath := TGPGraphicsPath.Create;
  R := ARect;
  R.X := CX - W / 2;
  R.Y := CY - W / 2;
  R.Width := W * 2;
  R.Height := W * 2;
  R.X := R.X + W / 2;
   GPath.AddRectangle(R);
  R.X := R.X - W * 2;
   GPath.AddLine(R.X + R.Width / 2, R.Y,
    R.X, R.Y + R.Height);
  GPath.AddLine(R.X + R.Width / 2, R.Y,
    R.X + R.Width, R.Y + R.Height);
   GPath.AddLine(R.X, R.Y + R.Height,
    R.X + R.Width, R.Y + R.Height);

  R.Y := R.Y - W * 2;
  R.X := R.X + W / 2;
  GPath.AddEllipse(R);

  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawPasswordGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  GPath: TGPGraphicsPath;
  CX, CY: Single;
  W: Single;
  R: TGPRectF;
begin
  if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  CX := ARect.X + ARect.Width / 2;
  CY := ARect.Y + ARect.Height * 0.65;
  W := ARect.Width / 6;
  GPath := TGPGraphicsPath.Create;
  R := ARect;
  R.X := CX - W;
  R.Y := CY - W;
  R.Width := W * 2;
  R.Height := W * 2;
  GPath.AddEllipse(R);
  ARect.Y := ARect.Y + W;
  GPath.AddArc(ARect, -180, 180);
  G.DrawPath(P, GPath);
  P.Free;
  GPath.Free;
end;

procedure GPDrawMoreGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  B: TGPSolidBrush;
  R: TGPRectF;
  W: Integer;
  CX, CY: Single;
begin
  B := TGPSolidBrush.Create(AGlyphColor);
  W := Round(ARect.Width / 6);
  CX := ARect.X + ARect.Width / 2 - W / 2;
  CY := ARect.Y + ARect.Height / 2 - W / 2;
  R.X := CX;
  R.Y := CY;
  R.Width := W;
  R.Height := W;
  G.FillEllipse(B, R);
  R.X := CX - R.Width - W;
  G.FillEllipse(B, R);
  R.X := CX + R.Width + W;
  G.FillEllipse(B, R);
  B.Free;
end;

procedure GPDrawDetailPointsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  B: TGPSolidBrush;
  R: TGPRectF;
  W: Integer;
  CX, CY: Single;
begin
  B := TGPSolidBrush.Create(AGlyphColor);
  W := Round(ARect.Width / 6);
  CX := ARect.X + ARect.Width / 2 - W / 2;
  CY := ARect.Y + ARect.Height / 2 - W / 2;
  R.X := CX;
  R.Y := CY;
  R.Width := W;
  R.Height := W;
  G.FillEllipse(B, R);
  R.Y := CY - R.Height - W;
  G.FillEllipse(B, R);
  R.Y := CY + R.Height + W;
  G.FillEllipse(B, R);
  B.Free;
end;

procedure GPDrawDetailsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double);
var
  B: TGPSolidBrush;
  R: TGPRectF;
  W: Integer;
  CX, CY: Single;
begin
  B := TGPSolidBrush.Create(AGlyphColor);
  W := Round(ARect.Height / 4);
  CX := ARect.X;
  CY := Round(ARect.Y + ARect.Height / 2 - 1);
  R.X := CX;
  R.Y := CY;
  R.Width := ARect.Width;
  if AScaleFactor > 1.5 then
    R.Height := 2
  else
    R.Height := 1;
  G.FillRectangle(B, R);
  R.Y := CY - W;
  G.FillRectangle(B, R);
  R.Y := CY + W;
  G.FillRectangle(B, R);
  B.Free;
end;

procedure GPDrawTextAlignLeftGlyph(G: TGPGraphics; ARect: TGPRectF;
   AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P1, P2, P3: TGPPointF;
  W: Single;
  P: TGPPen;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 5;
  ARect.Y := ARect.Y + W / 2;
  P1.X := ARect.X;
  P1.Y := ARect.Y;
  P2.X := ARect.X + ARect.Width - ARect.Width / 3;
  P2.Y := ARect.Y;
  P3.X := ARect.X + ARect.Width;
  P3.Y := ARect.Y;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawLine(P, P1, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P1, P2);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P1, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P1, P2);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P1, P3);
  P.Free;
end;

procedure GPDrawTextAlignRightGlyph(G: TGPGraphics; ARect: TGPRectF;
   AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P1, P2, P3: TGPPointF;
  W: Single;
  P: TGPPen;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 5;
  ARect.Y := ARect.Y + W / 2;
  P1.X := ARect.X;
  P1.Y := ARect.Y;
  P2.X := ARect.X + ARect.Width / 3;
  P2.Y := ARect.Y;
  P3.X := ARect.X + ARect.Width;
  P3.Y := ARect.Y;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawLine(P, P1, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P2, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P1, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P2, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  G.DrawLine(P, P1, P3);
  P.Free;
end;

procedure GPDrawTextAlignCenterGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P1, P2, P3, P4: TGPPointF;
  W: Single;
  P: TGPPen;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 5;
  ARect.Y := ARect.Y + W / 2;
  P1.X := ARect.X;
  P1.Y := ARect.Y;
  P2.X := ARect.X + ARect.Width / 4;
  P2.Y := ARect.Y;
  P3.X := ARect.X + ARect.Width;
  P3.Y := ARect.Y;
  P4.X := ARect.X + ARect.Width - ARect.Width / 4;
  P4.Y := ARect.Y;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawLine(P, P1, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  P4.Y := P4.Y + W;
  G.DrawLine(P, P2, P4);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  P4.Y := P4.Y + W;
  G.DrawLine(P, P1, P3);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  P4.Y := P4.Y + W;
  G.DrawLine(P, P2, P4);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  P3.Y := P3.Y + W;
  P4.Y := P4.Y + W;
  G.DrawLine(P, P1, P3);
  P.Free;
end;

procedure GPDrawTextAlignJustifiedGlyph(G: TGPGraphics; ARect: TGPRectF;
  AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var

  P1, P2: TGPPointF;
  W: Single;
  P: TGPPen;
begin
  if AThickness < 1 then
    AThickness := 1;
  W := ARect.Height / 5;
  ARect.Y := ARect.Y + W / 2;
  P1.X := ARect.X;
  P1.Y := ARect.Y;
  P2.X := ARect.X + ARect.Width;
  P2.Y := ARect.Y;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  G.DrawLine(P, P1, P2);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  G.DrawLine(P, P1, P2);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  G.DrawLine(P, P1, P2);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  G.DrawLine(P, P1, P2);
  P1.Y := P1.Y + W;
  P2.Y := P2.Y + W;
  G.DrawLine(P, P1, P2);
  P.Free;
end;

procedure GPDrawDetailsGlyph2(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  B: TGPSolidBrush;
  R: TGPRectF;
  W: Integer;
  CX, CY: Single;
begin
 if AThickness < 1 then
    AThickness := 1;
  B := TGPSolidBrush.Create(AGlyphColor);
  W := Round(ARect.Height / 4);
  CX := ARect.X;
  CY := Round(ARect.Y + ARect.Height / 2 - Trunc(AThickness * AScaleFactor) / 2);
  R.X := CX;
  R.Y := CY;
  R.Width := ARect.Width;
  R.Height := Trunc(AThickness * AScaleFactor);
  G.FillRectangle(B, R);
  R.Y := CY - W;
  G.FillRectangle(B, R);
  R.Y := CY + W;
  G.FillRectangle(B, R);
  B.Free;
end;

procedure GPDrawOptionsGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Integer;
  R: TGPRectF;
  GPath, GPath2: TGPGraphicsPath;
begin
 if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := Round(ARect.Height / 4);
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  R.Width := W;
  R.Height := W;

  R.X := ARect.X;
  R.Y := ARect.Y + W * 0.7;
  GPath.AddEllipse(R);
  GPath.AddLine(R.X + W / 2, ARect.Y, R.X + W / 2, R.Y);
  GPath2.AddLine(R.X + W / 2, ARect.Y + ARect.Height, R.X + W / 2, R.Y + R.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);

  GPath.Reset;
  GPath2.Reset;

  R.X := ARect.X + ARect.Width / 2 - W / 2;
  R.Y := ARect.Y + ARect.Height - W * 1.7;
  GPath.AddEllipse(R);
  GPath.AddLine(R.X + W / 2, ARect.Y, R.X + W / 2, R.Y);
  GPath2.AddLine(R.X + W / 2, ARect.Y + ARect.Height, R.X + W / 2, R.Y + R.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);

  GPath.Reset;
  GPath2.Reset;

  R.X := ARect.X + ARect.Width - W;
  R.Y := ARect.Y + W * 0.7;
  GPath.AddEllipse(R);
  GPath.AddLine(R.X + W / 2, ARect.Y, R.X + W / 2, R.Y);
  GPath2.AddLine(R.X + W / 2, ARect.Y + ARect.Height, R.X + W / 2, R.Y + R.Height);
  GPath.AddPath(GPath2, False);
  G.DrawPath(P, GPath);

  P.Free;
  GPath.Free;
  GPath2.Free;
end;

procedure GPDrawShareGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  P: TGPPen;
  W: Single;
  R: TGPRectF;
  GPath, GPath2, GPath3, GPath4: TGPGraphicsPath;
begin
 if AThickness < 1 then
    AThickness := 1;
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 3.5;
  GPath := TGPGraphicsPath.Create;
  GPath2 := TGPGraphicsPath.Create;
  GPath3 := TGPGraphicsPath.Create;
  GPath4 := TGPGraphicsPath.Create;

  ARect.X := ARect.X - W / 3;

  R.Width := W;
  R.Height := W;

  R.X := ARect.X;
  R.Y := ARect.Y + ARect.Height / 2 - W / 2;
  GPath.AddArc(R, -30, 359.99);

  R.X := ARect.X + ARect.Width - W;
  R.Y := ARect.Y;
  GPath2.AddArc(R, 160, 359.99);

  R.X := ARect.X + ARect.Width - W;
  R.Y := ARect.Y + ARect.Height - W;
  GPath3.AddArc(R, 205, 359.99);

  R.X := ARect.X;
  R.Y := ARect.Y + ARect.Height / 2 - W / 2;
  GPath4.AddArc(R, 30, 359.99);
  GPath3.AddPath(GPath4, True);

  GPath.AddPath(GPath2, True);
  GPath.AddPath(GPath3, False);

  G.DrawPath(P, GPath);

  P.Free;
  GPath.Free;
  GPath2.Free;
  GPath3.Free;
  GPath4.Free;
end;

procedure GPDrawEditGlyph(G: TGPGraphics; ARect: TGPRectF;
    AGlyphColor: Cardinal; AScaleFactor: Double; AThickness: Byte = 2);
var
  B: TGPSolidBrush;
  P: TGPPen;
  W: Single;
  GPath: TGPGraphicsPath;
  P1, P2, P3, P4, P5: TGPPointF;
begin
  if AThickness < 1 then
    AThickness := 1;
  B := TGPSolidBrush.Create(AGlyphColor);
  P := TGPPen.Create(AGlyphColor, Trunc(AThickness * AScaleFactor));
  W := ARect.Height / 8;
  InflateGPRect(ARect, -W, -W);
  GPath := TGPGraphicsPath.Create;
  P1.X := ARect.X + ARect.Width - W;
  P1.Y := ARect.Y;
  P2.X := ARect.X + W / 2;
  P2.Y := ARect.Y + ARect.Height - W - W / 2;
  P3.X := ARect.X + ARect.Width;
  P3.Y := ARect.Y + W;
  P4.X := ARect.X + W + W / 2;
  P4.Y := ARect.Y + ARect.Height - W / 2;
  P5.X := ARect.X;
  P5.Y := ARect.Y + ARect.Height;
  GPath.StartFigure;
  GPath.AddLine(P1, P2);
  GPath.AddLine(P4, P3);
  GPath.CloseFigure;
  GPath.StartFigure;
  GPath.AddLine(P2, P5);
  GPath.AddLine(P4, P5);
  GPath.CloseFigure;
  P1.X := P1.X - W;
  P1.Y := P1.Y + W;
  P3.X := P3.X - W;
  P3.Y := P3.Y + W;
  GPath.AddLine(P1, P3);
  G.DrawPath(P, GPath);
  GPath.Free;
  B.Free;
  P.Free;
end;

procedure GPDrawBitmapSmooth2(ADC: HDC; ARect: TRect; ABitmap: TBitmap);
var
  G: TGPGraphics;
  B: TGPBitmap;
begin
  B := nil;
  G := TGPGraphics.Create(ADC);
  try
    G.SetSmoothingMode(SmoothingModeHighQuality);
    G.SetInterpolationMode(InterpolationModeHighQuality);
    G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
    if ABitmap.PixelFormat = pf32bit then
    begin
      B := TGPBitmap.Create(ABitmap.Width, ABitmap.Height, -ABitmap.Width * 4,
        PixelFormat32bppARGB, ABitmap.ScanLine[0]);
    end
    else
      B := TGPBitmap.Create(ABitmap.Handle, ABitmap.Palette);
    G.DrawImage(B, RectToGPRect(ARect));
  finally
    G.Free;
    B.Free;
  end;
end;

procedure GPDrawBitmapSmooth(ADC: HDC; ARect: TRect; ABitmap: TBitmap);
var
  G: TGPGraphics;
  B: TGPBitmap;
begin
  B := nil;
  G := TGPGraphics.Create(ADC);
  try
    G.SetSmoothingMode(SmoothingModeHighQuality);
    G.SetInterpolationMode(InterpolationModeHighQuality);
    if ABitmap.PixelFormat = pf32bit then
    begin
      B := TGPBitmap.Create(ABitmap.Width, ABitmap.Height, -ABitmap.Width * 4,
        PixelFormat32bppARGB, ABitmap.ScanLine[0]);
    end
    else
      B := TGPBitmap.Create(ABitmap.Handle, ABitmap.Palette);
    G.DrawImage(B, RectToGPRect(ARect));
  finally
    G.Free;
    B.Free;
  end;
end;

function GraphicToGPBitmap(G: TGraphic): TGPBitmap;
var
  MemStream: TMemoryStream;
  B: TBitmap;
begin
  Result := nil;
  if G = nil then Exit;
  if (G is TIcon) or (G is TPngImage) then
  begin
    MemStream := TMemoryStream.Create;
    G.SaveToStream(MemStream);
    try
      MemStream.Position := 0;
      Result := TGPBitmap.Create(TStreamAdapter.Create(MemStream));
    finally
      FreeAndNil(MemStream);
    end;
  end
  else
  begin
    B := TBitmap.Create;
    try
      B.Assign(G);
      Result := TGPBitmap.Create(B.Handle, B.Palette);
    finally
      B.Free;
    end;
  end;
end;

function PictureToGPBitmap(Picture: TPicture): TGPBitmap;
begin
  Result :=  GraphicToGPBitmap(Picture.Graphic);
end;

constructor TscGPBitmapOptions.Create;
begin
  FLeftMargin := 0;
  FRightMargin := 0;
  FTopMargin := 0;
  FBottomMargin := 0;
  FDrawOnlyBorder := False;
  FDrawOnlyClient := False;
  FStretch := True;
  FStretchBorder := False;
end;

procedure TscGPBitmapOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPBitmapOptions then
  begin
    FLeftMargin := TscGPBitmapOptions(Source).FLeftMargin;
    FRightMargin := TscGPBitmapOptions(Source).FRightMargin;
    FTopMargin := TscGPBitmapOptions(Source).FTopMargin;
    FBottomMargin := TscGPBitmapOptions(Source).FBottomMargin;
    FStretch := TscGPBitmapOptions(Source).FStretch;
    FStretchBorder := TscGPBitmapOptions(Source).FStretchBorder;
    FDrawOnlyBorder := TscGPBitmapOptions(Source).FDrawOnlyBorder;
    FDrawOnlyClient := TscGPBitmapOptions(Source).FDrawOnlyClient;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPBitmapOptions.SetLeftMargin(Value: Integer);
begin
  FLeftMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPBitmapOptions.SetRightMargin(Value: Integer);
begin
  FRightMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPBitmapOptions.SetTopMargin(Value: Integer);
begin
  FTopMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPBitmapOptions.SetBottomMargin(Value: Integer);
begin
  FBottomMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure GPBitmap_DrawWithOptions(AGraphics: TGPGraphics; ABitmap: TGPBitmap;
  AOptions: TscGPBitmapOptions; ARect: TGPRectF; AVirtualWidth, AVirtualHeight: Integer; AScaleFactor: Double);
var
  X, Y: Double;
  W, H, Wkf, Hkf: Double;

  CornerR1, CornerR2, CornerR3, CornerR4: TGPRectF;
  BorderR1, BorderR2, BorderR3, BorderR4: TGPRectF;
  ClientR: TGPRectF;
  ClipR: TGPRectF;
  Clip: Boolean;

  CornerR1_Dest, CornerR2_Dest, CornerR3_Dest, CornerR4_Dest: TGPRectF;
  BorderR1_Dest, BorderR2_Dest, BorderR3_Dest, BorderR4_Dest: TGPRectF;
  ClientR_Dest: TGPRectF;

  L, T, R, B: Integer;
  XCnt, YCnt, I, J: Integer;
begin
  if (AOptions.LeftMargin > 0) and (AOptions.RightMargin > 0) and
     (AOptions.TopMargin > 0) and (AOptions.BottomMargin > 0)
  then
  begin
    W := ABitmap.GetWidth;
    H := ABitmap.GetHeight;

    L := AOptions.LeftMargin;
    T := AOptions.TopMargin;
    R := AOptions.RightMargin;
    B := AOptions.BottomMargin;

    CornerR1.X := 0;
    CornerR1.Y := 0;
    CornerR1.Width := L;
    CornerR1.Height := T;

    CornerR2.X := W - R;
    CornerR2.Y := 0;
    CornerR2.Width := R;
    CornerR2.Height := T;

    CornerR3.X := 0;
    CornerR3.Y := H - B;
    CornerR3.Width := L;
    CornerR3.Height := B;

    CornerR4.X := W - B;;
    CornerR4.Y := H - B;
    CornerR4.Width := R;
    CornerR4.Height := B;

    BorderR1.X := L;
    BorderR1.Y := 0;
    BorderR1.Height := T;
    BorderR1.Width := W - R - L;

    BorderR2.X := L;
    BorderR2.Y := H - B;
    BorderR2.Height := B;
    BorderR2.Width := W - R - L;

    BorderR3.X := 0;
    BorderR3.Y := T;
    BorderR3.Height := H - T - B;
    BorderR3.Width := L;

    BorderR4.X := W - R;
    BorderR4.Y := T;
    BorderR4.Height := H - T - B;
    BorderR4.Width := R;

    ClientR.X := L;
    ClientR.Y := T;
    ClientR.Width := W - L - R;
    ClientR.Height := H - T - B;

    L := Round(L * AScaleFactor);
    T := Round(T * AScaleFactor);
    R := Round(R * AScaleFactor);
    B := Round(B * AScaleFactor);

    Wkf := 1;
    Hkf := 1;

    if (AVirtualWidth > 0) and (AVirtualHeight > 0) then
    begin
      Wkf := AVirtualWidth / W;
      Hkf := AVirtualHeight / H;
      L := Round(L * Wkf);
      T := Round(T * Hkf);
      R := Round(R * Wkf);
      B := Round(B * Hkf);
    end;

    X := ARect.X;
    Y := ARect.Y;
    W := ARect.Width;
    H := ARect.Height;

    CornerR1_Dest.X := X;
    CornerR1_Dest.Y := Y;
    CornerR1_Dest.Width := L;
    CornerR1_Dest.Height := T;

    CornerR2_Dest.X := X + W - R;
    CornerR2_Dest.Y := Y;
    CornerR2_Dest.Width := R;
    CornerR2_Dest.Height := T;

    CornerR3_Dest.X := X;
    CornerR3_Dest.Y := Y + H - B;
    CornerR3_Dest.Width := L;
    CornerR3_Dest.Height := B;

    CornerR4_Dest.X := X + W - R;
    CornerR4_Dest.Y := Y + H - B;
    CornerR4_Dest.Width := R;
    CornerR4_Dest.Height := B;

    BorderR1_Dest.X := X + L;
    BorderR1_Dest.Y := Y;
    BorderR1_Dest.Height := T;
    BorderR1_Dest.Width := W - R - L;

    BorderR2_Dest.X := X + L;
    BorderR2_Dest.Y := Y + H - B;
    BorderR2_Dest.Height := B;
    BorderR2_Dest.Width := W - R - L;

    BorderR3_Dest.X := X;
    BorderR3_Dest.Y := Y + T;
    BorderR3_Dest.Height := H - T - B;
    BorderR3_Dest.Width := L;

    BorderR4_Dest.X := X + W - R;
    BorderR4_Dest.Y := Y + T;
    BorderR4_Dest.Height := H - T - B;
    BorderR4_Dest.Width := R;

    if AOptions.DrawOnlyClient then
      ClientR_Dest := ARect
    else
    begin
      ClientR_Dest.X := X + L;
      ClientR_Dest.Y := Y + T;
      ClientR_Dest.Width := W - L - R;
      ClientR_Dest.Height := H - T - B;
    end;

    if not AOptions.DrawOnlyClient then   // not draw client only
    begin

    ClipR := CornerR1_Dest;
    Clip := False;
    if CornerR1_Dest.X + CornerR1_Dest.Width > X + W then
    begin
      ClipR.Width := ClipR.Width - ((CornerR1_Dest.X + CornerR1_Dest.Width) - (X + W));
      Clip := True;
    end;
    if CornerR1_Dest.Y + CornerR1_Dest.Height > Y + H then
    begin
      ClipR.Height := ClipR.Height - ((CornerR1_Dest.Y + CornerR1_Dest.Height) - (Y + H));
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR1_Dest, CornerR1.X, CornerR1.Y,
      CornerR1.Width, CornerR1.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;

    ClipR := CornerR2_Dest;
    Clip := False;
    if CornerR2_Dest.X < CornerR1_Dest.X + CornerR1_Dest.Width then
    begin
      ClipR.X := CornerR1_Dest.X + CornerR1_Dest.Width;
      ClipR.Width := ClipR.Width - ((CornerR1_Dest.X + CornerR1_Dest.Width) - CornerR2_Dest.X);
      Clip := True;
    end;
    if ClipR.Y + ClipR.Height > Y + H then
    begin
      ClipR.Height := ClipR.Height - ((ClipR.Y + ClipR.Height) - (Y + H));
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR2_Dest, CornerR2.X, CornerR2.Y,
      CornerR2.Width, CornerR2.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;


    ClipR := CornerR3_Dest;
    Clip := False;
    if CornerR3_Dest.X + CornerR3_Dest.Width > X + W then
    begin
      ClipR.Width := ClipR.Width - ((CornerR3_Dest.X + CornerR3_Dest.Width) - (X + W));
      Clip := True;
    end;
    if CornerR3_Dest.Y < CornerR1_Dest.Y + CornerR1_Dest.Height then
    begin
      ClipR.Y := CornerR1_Dest.Y + CornerR1_Dest.Height;
      ClipR.Height := ClipR.Height - ((CornerR1_Dest.Y + CornerR1_Dest.Height) - CornerR3_Dest.Y);
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR3_Dest, CornerR3.X, CornerR3.Y,
      CornerR3.Width, CornerR3.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;

    ClipR := CornerR4_Dest;
    Clip := False;
    if CornerR4_Dest.X < CornerR3_Dest.X + CornerR3_Dest.Width then
    begin
      ClipR.X := CornerR3_Dest.X + CornerR3_Dest.Width;
      ClipR.Width := ClipR.Width - ((CornerR3_Dest.X + CornerR3_Dest.Width) - CornerR4_Dest.X);
      Clip := True;
    end;
    if CornerR4_Dest.Y < CornerR2_Dest.Y + CornerR2_Dest.Height then
    begin
      ClipR.Y := CornerR2_Dest.Y + CornerR2_Dest.Height;
      ClipR.Height := ClipR.Height - ((CornerR2_Dest.Y + CornerR2_Dest.Height) - CornerR4_Dest.Y);
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR4_Dest, CornerR4.X, CornerR4.Y,
      CornerR4.Width, CornerR4.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;

    if BorderR1_Dest.Width > 0 then
    begin
      ClipR := BorderR1_Dest;
      Clip := not AOptions.StretchBorder;
      if BorderR1_Dest.Y + BorderR1_Dest.Height > Y + H then
      begin
        ClipR.Height := ClipR.Height - ((BorderR1_Dest.Y + BorderR1_Dest.Height) - (Y + H));
        Clip := True;
      end;
      if Clip then
        AGraphics.IntersectClip(ClipR);
      if AOptions.StretchBorder then
        AGraphics.DrawImage(ABitmap, BorderR1_Dest, BorderR1.X, BorderR1.Y,
          BorderR1.Width, BorderR1.Height, UnitPixel)
      else
      begin
        W := Round(BorderR1.Width * AScaleFactor * Wkf);
        BorderR1_Dest.Width := W;
        XCnt := Round(ClipR.Width /BorderR1_Dest.Width);
        for I := 0 to XCnt do
        begin
          BorderR1_Dest.X := ClipR.X + I * W;
          AGraphics.DrawImage(ABitmap, BorderR1_Dest, BorderR1.X, BorderR1.Y,
            BorderR1.Width, BorderR1.Height, UnitPixel);
        end;
      end;
      if Clip then
        AGraphics.ResetClip;
    end;

    if BorderR2_Dest.Width > 0 then
    begin
      ClipR := BorderR2_Dest;
      Clip := not AOptions.StretchBorder;
      if BorderR2_Dest.Y < BorderR1_Dest.Y + BorderR1_Dest.Height then
      begin
        ClipR.Y := BorderR1_Dest.Y + BorderR1_Dest.Height;
        ClipR.Height := ClipR.Height - ((BorderR1_Dest.Y + BorderR1_Dest.Height) -  BorderR2_Dest.Y);
        Clip := True;
      end;
      if Clip then
        AGraphics.IntersectClip(ClipR);
      if AOptions.StretchBorder then
        AGraphics.DrawImage(ABitmap, BorderR2_Dest, BorderR2.X, BorderR2.Y,
          BorderR2.Width, BorderR2.Height, UnitPixel)
      else
      begin
        W := Round(BorderR2.Width * AScaleFactor * Wkf);
        BorderR2_Dest.Width := W;
        XCnt := Round(ClipR.Width /BorderR2_Dest.Width);
        for I := 0 to XCnt do
        begin
          BorderR2_Dest.X := ClipR.X + I * W;
          AGraphics.DrawImage(ABitmap, BorderR2_Dest, BorderR2.X, BorderR2.Y,
            BorderR2.Width, BorderR2.Height, UnitPixel);
        end;
      end;
      if Clip then
        AGraphics.ResetClip;
    end;

    if BorderR3_Dest.Height > 0 then
    begin
      ClipR := BorderR3_Dest;
      Clip := not AOptions.StretchBorder;
      if BorderR3_Dest.X + BorderR3_Dest.Width > X + W then
      begin
        ClipR.Width := ClipR.Width - ((BorderR3_Dest.X + BorderR3_Dest.Width) - (X + W));
        Clip := True;
      end;
      if Clip then
        AGraphics.IntersectClip(ClipR);
      if AOptions.StretchBorder then
        AGraphics.DrawImage(ABitmap, BorderR3_Dest, BorderR3.X, BorderR3.Y,
          BorderR3.Width, BorderR3.Height, UnitPixel)
      else
      begin
        H := Round(BorderR3.Height * AScaleFactor * Hkf);
        BorderR3_Dest.Height := H;
        YCnt := Round(ClipR.Height / BorderR3_Dest.Height);
        for J := 0 to YCnt  do
        begin
          BorderR3_Dest.Y := ClipR.Y + J * H;
          AGraphics.DrawImage(ABitmap, BorderR3_Dest, BorderR3.X, BorderR3.Y,
            BorderR3.Width, BorderR3.Height, UnitPixel)
        end;
      end;
      if Clip then
        AGraphics.ResetClip;
    end;

    if BorderR4_Dest.Height > 0 then
    begin
      ClipR := BorderR4_Dest;
      Clip := not AOptions.StretchBorder;
      if BorderR4_Dest.X < BorderR3_Dest.X + BorderR3_Dest.Width then
      begin
        ClipR.X := BorderR3_Dest.X + BorderR3_Dest.Width;
        ClipR.Width := ClipR.Width - ((BorderR3_Dest.X + BorderR3_Dest.Width) - BorderR4_Dest.X);
        Clip := True;
      end;
      if Clip then
        AGraphics.IntersectClip(ClipR);
      if AOptions.StretchBorder then
        AGraphics.DrawImage(ABitmap, BorderR4_Dest, BorderR4.X, BorderR4.Y,
          BorderR4.Width, BorderR4.Height, UnitPixel)
      else
      begin
        H := Round(BorderR4.Height * AScaleFactor * Hkf);
        BorderR4_Dest.Height := H;
        YCnt := Round(ClipR.Height / BorderR4_Dest.Height);
        for J := 0 to YCnt  do
        begin
          BorderR4_Dest.Y := ClipR.Y + J * H;
          AGraphics.DrawImage(ABitmap, BorderR4_Dest, BorderR4.X, BorderR4.Y,
            BorderR4.Width, BorderR4.Height, UnitPixel)
        end;
      end;
      if Clip then
        AGraphics.ResetClip;
    end;

    end; // not draw client only

    if (ClientR_Dest.Height > 0) and (ClientR_Dest.Width > 0) and not
      AOptions.DrawOnlyBorder
    then
    begin
      if AOptions.Stretch then
        AGraphics.DrawImage(ABitmap, ClientR_Dest, ClientR.X, ClientR.Y,
          ClientR.Width, ClientR.Height, UnitPixel)
      else
      begin
        ClipR := ClientR_Dest;
        AGraphics.IntersectClip(ClipR);
        W := Round(ClientR.Width * AScaleFactor * Wkf);
        H := Round(ClientR.Height * AScaleFactor * Hkf);
        ClientR_Dest.Width := W;
        ClientR_Dest.Height := H;
        XCnt := Round(ClipR.Width / ClientR_Dest.Width);
        YCnt := Round(ClipR.Height / ClientR_Dest.Height);
        for I := 0 to XCnt do
        begin
          for J := 0 to YCnt  do
          begin
            ClientR_Dest.X := ClipR.X + I * W;
            ClientR_Dest.Y := ClipR.Y + J * H;
            AGraphics.DrawImage(ABitmap, ClientR_Dest, ClientR.X, ClientR.Y,
              ClientR.Width, ClientR.Height, UnitPixel)
          end;
        end;
        AGraphics.ResetClip;
      end;
    end;
  end
  else
  if (AOptions.LeftMargin > 0) and (AOptions.RightMargin > 0)
  then
  begin
    W := ABitmap.GetWidth;
    H := ABitmap.GetHeight;

    L := AOptions.LeftMargin;
    R := AOptions.RightMargin;

    CornerR1.X := 0;
    CornerR1.Y := 0;
    CornerR1.Width := L;
    CornerR1.Height := H;

    CornerR2.X := W - R;
    CornerR2.Y := 0;
    CornerR2.Width := R;
    CornerR2.Height := H;

    ClientR.X := L;
    ClientR.Y := 0;
    ClientR.Width := W - L - R;
    ClientR.Height := H;

    L := Round(L * AScaleFactor);
    R := Round(R * AScaleFactor);

    Wkf := 1;

    if (AVirtualWidth > 0) and (AVirtualHeight > 0) then
    begin
      Wkf := AVirtualWidth / W;
      L := Round(L * Wkf);
      R := Round(R * Wkf);
    end;

    X := ARect.X;
    Y := ARect.Y;
    W := ARect.Width;
    H := ARect.Height;

    CornerR1_Dest.X := X;
    CornerR1_Dest.Y := Y;
    CornerR1_Dest.Width := L;
    CornerR1_Dest.Height := H;

    CornerR2_Dest.X := X + W - R;
    CornerR2_Dest.Y := Y;
    CornerR2_Dest.Width := R;
    CornerR2_Dest.Height := H;

    if AOptions.DrawOnlyClient then
      ClientR_Dest := ARect
    else
    begin
      ClientR_Dest.X := X + L;
      ClientR_Dest.Y := Y;
      ClientR_Dest.Width := W - L - R;
      ClientR_Dest.Height := H;
    end;

    if not AOptions.DrawOnlyClient then  // not draw only client
    begin

    ClipR := CornerR1_Dest;
    Clip := False;
    if CornerR1_Dest.X + CornerR1_Dest.Width > X + W then
    begin
      ClipR.Width := ClipR.Width - ((CornerR1_Dest.X + CornerR1_Dest.Width) - (X + W));
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR1_Dest, CornerR1.X, CornerR1.Y,
      CornerR1.Width, CornerR1.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;

    ClipR := CornerR2_Dest;
    Clip := False;
    if CornerR2_Dest.X < CornerR1_Dest.X + CornerR1_Dest.Width then
    begin
      ClipR.X := CornerR1_Dest.X + CornerR1_Dest.Width;
      ClipR.Width := ClipR.Width - ((CornerR1_Dest.X + CornerR1_Dest.Width) - CornerR2_Dest.X);
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR2_Dest, CornerR2.X, CornerR2.Y,
      CornerR2.Width, CornerR2.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;

    end; // not draw only client

   if (ClientR_Dest.Width > 0) and not
      AOptions.DrawOnlyBorder
    then
    begin
      if AOptions.Stretch then
        AGraphics.DrawImage(ABitmap, ClientR_Dest, ClientR.X, ClientR.Y,
          ClientR.Width, ClientR.Height, UnitPixel)
      else
      begin
        ClipR := ClientR_Dest;
        AGraphics.IntersectClip(ClipR);
        W := Round(ClientR.Width * AScaleFactor * Wkf);
        ClientR_Dest.Width := W;
        XCnt := Round(ClipR.Width / ClientR_Dest.Width);
        for I := 0 to XCnt do
        begin
          ClientR_Dest.X := ClipR.X + I * W;
          AGraphics.DrawImage(ABitmap, ClientR_Dest, ClientR.X, ClientR.Y,
           ClientR.Width, ClientR.Height, UnitPixel)
        end;
        AGraphics.ResetClip;
      end;
    end;
  end
  else
  if (AOptions.TopMargin > 0) and (AOptions.BottomMargin > 0)
  then
  begin
    W := ABitmap.GetWidth;
    H := ABitmap.GetHeight;

    T := AOptions.TopMargin;
    B := AOptions.BottomMargin;

    CornerR3.X := 0;
    CornerR3.Y := 0;
    CornerR3.Width := W;
    CornerR3.Height := T;

    CornerR4.X := 0;;
    CornerR4.Y := H - B;
    CornerR4.Width := W;
    CornerR4.Height := B;

    ClientR.X := 0;
    ClientR.Y := T;
    ClientR.Width := W;
    ClientR.Height := H - T - B;

    T := Round(T * AScaleFactor);
    B := Round(B * AScaleFactor);

    Hkf := 1;

    if (AVirtualWidth > 0) and (AVirtualHeight > 0) then
    begin
      Hkf := AVirtualHeight / H;
      T := Round(T * Hkf);
      B := Round(B * Hkf);
    end;

    X := ARect.X;
    Y := ARect.Y;
    W := ARect.Width;
    H := ARect.Height;

    CornerR3_Dest.X := X;
    CornerR3_Dest.Y := Y;
    CornerR3_Dest.Width := W;
    CornerR3_Dest.Height := T;

    CornerR4_Dest.X := X;
    CornerR4_Dest.Y := Y + H - B;
    CornerR4_Dest.Width := W;
    CornerR4_Dest.Height := B;

    if AOptions.DrawOnlyClient then
      ClientR_Dest := ARect
    else
    begin
      ClientR_Dest.X := X;
      ClientR_Dest.Y := Y + T;
      ClientR_Dest.Width := W;
      ClientR_Dest.Height := H - T - B;
    end;

    if not AOptions.DrawOnlyClient then // not draw only client
    begin

    ClipR := CornerR3_Dest;
    Clip := False;
    if CornerR3_Dest.Y + CornerR1_Dest.Height > Y + H then
    begin
      ClipR.Height := ClipR.Height -((CornerR3_Dest.Y + CornerR1_Dest.Height) - (Y + H));
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR3_Dest, CornerR3.X, CornerR3.Y,
      CornerR3.Width, CornerR3.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;

    ClipR := CornerR4_Dest;
    Clip := False;
    if CornerR4_Dest.Y < CornerR3_Dest.Y + CornerR3_Dest.Height then
    begin
      ClipR.Y := CornerR3_Dest.Y + CornerR3_Dest.Height;
      ClipR.Height := ClipR.Height - ((CornerR3_Dest.Y + CornerR3_Dest.Height) - CornerR4_Dest.Y);
      Clip := True;
    end;
    if Clip then
      AGraphics.IntersectClip(ClipR);
    AGraphics.DrawImage(ABitmap, CornerR4_Dest, CornerR4.X, CornerR4.Y,
      CornerR4.Width, CornerR4.Height, UnitPixel);
    if Clip then
      AGraphics.ResetClip;

    end; // not draw only client

   if (ClientR_Dest.Height > 0) and not
      AOptions.DrawOnlyBorder
    then
    begin
      if AOptions.Stretch then
        AGraphics.DrawImage(ABitmap, ClientR_Dest, ClientR.X, ClientR.Y,
          ClientR.Width, ClientR.Height, UnitPixel)
      else
      begin
        ClipR := ClientR_Dest;
        AGraphics.IntersectClip(ClipR);
        H := Round(ClientR.Height * AScaleFactor * Hkf);
        ClientR_Dest.Height := H;
        YCnt := Round(ClipR.Height / ClientR_Dest.Height);
        for I := 0 to YCnt do
        begin
          ClientR_Dest.Y := ClipR.Y + I * H;
          AGraphics.DrawImage(ABitmap, ClientR_Dest, ClientR.X, ClientR.Y,
           ClientR.Width, ClientR.Height, UnitPixel)
        end;
        AGraphics.ResetClip;
      end;
    end;
  end
  else
  if (AOptions.LeftMargin = 0) and (AOptions.RightMargin = 0) and
     (AOptions.TopMargin = 0) and (AOptions.BottomMargin = 0)
  then
  begin
    if (AVirtualWidth > 0) and (AVirtualHeight > 0) then
    begin
      W := Round(AVirtualWidth * AScaleFactor);
      H := Round(AVirtualHeight * AScaleFactor);
    end
    else
    begin
      W := Round(ABitmap.GetWidth * AScaleFactor);
      H := Round(ABitmap.GetHeight * AScaleFactor);
    end;
    X := Round(ARect.X + ARect.Width / 2 - W / 2);
    Y := Round(ARect.Y + ARect.Height / 2 - H / 2);
    if X < ARect.X then X := ARect.X;
    if Y < ARect.Y then Y := ARect.Y;
    AGraphics.DrawImage(ABitmap, X, Y, W, H);
  end;
end;

procedure GPDrawShadowForEllipse(G: TGPGraphics; X, Y, W, H: Single; ShadowSize: Integer; ShadowAlpha: Byte = 255);
var
  Path: TGPGraphicsPath;
  Brush: TGPPathGradientBrush;
  SurroundColors: array [0..0] of Cardinal;
  ColorCount: Integer;
  P: TGPPen;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddEllipse(X - ShadowSize / 2, Y - ShadowSize / 2, W + ShadowSize, H + ShadowSize * 2);
  Brush := TGPPathGradientBrush.Create(Path);
  Brush.SetCenterColor(MakeColor(ShadowAlpha, 0, 0, 0));
  SurroundColors[0] := MakeColor(0, 0, 0, 0);
  ColorCount := 1;
  Brush.SetSurroundColors(@SurroundColors[0], ColorCount);
  P := TGPPen.Create(Brush, ShadowSize * 4);
  G.DrawEllipse(P, X - ShadowSize * 2, Y - ShadowSize * 2, W + ShadowSize * 4, H + ShadowSize * 4);
  Path.Free;
  Brush.Free;
  P.Free;
end;

procedure GPDrawPathAsShadow(G: TGPGraphics; Path: TGPGraphicsPath; CenterPoint: TGPPointF; ShadowAlpha: Byte = 255);
var
  Brush: TGPPathGradientBrush;
  SurroundColors: array [0..0] of Cardinal;
  ColorCount: Integer;
begin
  Brush := TGPPathGradientBrush.Create(Path);
  Brush.SetCenterColor(MakeColor(ShadowAlpha, 0, 0, 0));
  Brush.SetCenterPoint(CenterPoint);
  SurroundColors[0] := MakeColor(0, 0, 0, 0);
  ColorCount := 1;
  Brush.SetSurroundColors(@SurroundColors[0], ColorCount);
  G.FillPath(Brush, Path);
  Brush.Free;
end;

procedure scGPDrawFocus(G: TGPGraphics; ARect: TGPRectF;
    AColor: Cardinal; AScaleFactor: Double);
var
  P: TGPPen;
begin
  InflateGPRect(ARect, -1, -1);
  if AScaleFactor < 2 then
    P := TGPPen.Create(0, 1)
  else
    P := TGPPen.Create(0, 2);
  P.SetColor(AColor);
  P.SetDashStyle(DashStyle.DashStyleDot);
  G.DrawRectangle(P, ARect);
  P.Free;
end;


procedure GPDrawLightCircle(G: TGPGraphics; AFillR: TGPRectF; LightAlpha: Byte = 255);
var
  Brush: TGPPathGradientBrush;
  SurroundColors: array [0..0] of Cardinal;
  ColorCount: Integer;
  CenterPoint: TGPPointF;
  FillPath: TGPGraphicsPath;
  GR: TGPRectF;
begin
  FillPath := TGPGraphicsPath.Create;
  CenterPoint.X := AFillR.X + AFillR.Width / 2;
  CenterPoint.Y := AFillR.Y + AFillR.Height / 2;
  GR := AFillR;
  GR.Height := AFillR.Width;
  GR.X := CenterPoint.X - GR.Width / 2;
  GR.Y := CenterPoint.Y - GR.Height / 2;
  FillPath.AddEllipse(GR);
  Brush := TGPPathGradientBrush.Create(FillPath);
  Brush.SetCenterColor(MakeColor(LightAlpha, 255, 255, 255));
  Brush.SetCenterPoint(CenterPoint);
  SurroundColors[0] := MakeColor(0, 255, 255, 255);
  ColorCount := 1;
  Brush.SetSurroundColors(@SurroundColors[0], ColorCount);
  G.FillPath(Brush, FillPath);
  FillPath.Free;
  Brush.Free;
end;

procedure DrawFluentLightHotEffect(G: TGPGraphics; AFillR: TGPRectF; AX: Integer);
var
  GR: TGPRectF;
begin
  GR := AFillR;
  GR.X := AX - GR.Width / 2;
  GPDrawLightCircle(G, GR, 50);
end;

procedure DrawFluentLightPressedEffect(G: TGPGraphics; AFillR: TGPRectF; AX: Integer; AY: Integer; Amount: Byte);
var
  GR: TGPRectF;
  kf: Double;
begin
  if Amount > SC_FluentPressedEffectSteps then
    Exit;
  kf := (SC_FluentPressedEffectSteps - Amount) / SC_FluentPressedEffectSteps;
  GR := AFillR;
  GR.Width := GR.Width / 2 + GR.Width / 2 * (1 - kf);
  GR.X := AX - GR.Width / 2;
  GR.Y := AY - GR.Height / 2;
  GPDrawLightCircle(G, GR, Round(70 * kf));
end;

end.
