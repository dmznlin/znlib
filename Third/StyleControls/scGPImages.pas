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

unit scGPImages;

{$I scdefine.inc}
{$R-}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Types, System.UITypes, Vcl.ImgList,
  Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  scDrawUtils, scGPUtils, scControls, scImageCollection,
  Vcl.Imaging.PngImage,
  WinApi.GdipObj, WinApi.GdipApi;

type
  TscGPImagePosition = (scgpipLeft, scgpipRight, scgpipCenter);
  TscGPInterpolationMode =(scgppimDefault, scgppimHighQualityBilinear);

  TscGPImage = class(TscPanel)
  private
    FPicture: TPicture;
    FPngImage: TPngImage;
    FOnProgress: TProgressEvent;
    FStretch: Boolean;
    FPosition: TscGPImagePosition;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FDrawing: Boolean;
    FProportional: Boolean;
    GPB: TGPBitmap;
    FClipFrame: TscGPImageClipFrame;
    FClipFrameRadius: Integer;
    FClipFrameFillColor: TColor;
    FClipFrameFillColorAlpha: Byte;
    FClipFrameColor: TColor;
    FClipFrameWidth: Integer;
    FRotationAngle: Integer;
    FAnimationTimer: TTimer;
    FRotateAnimation: Boolean;
    FImages: TscCustomImageCollection;
    FImageIndex: Integer;
    FAnimationAcceleration: Boolean;
    FInterpolationMode: TscGPInterpolationMode;
    procedure SetInterpolationMode(Value: TscGPInterpolationMode);
    procedure SetImageIndex(Value: Integer);
    procedure SetPngImage(Value: TPngImage);
    procedure SetImages(Value: TscCustomImageCollection);
    procedure SetRotateAnimation(Value: Boolean);
    procedure SetRotationAngle(Value: Integer);
    procedure SetClipFrame(Value: TscGPImageClipFrame);
    procedure SetClipFrameRadius(Value: Integer);
    procedure SetClipFrameFillColor(Value: TColor);
    procedure SetClipFrameFillColorAlpha(Value: Byte);
    procedure SetClipFrameColor(Value: TColor);
    procedure SetClipFrameWidth(Value: Integer);

    procedure PictureChanged(Sender: TObject);
    procedure SetPosition(Value: TscGPImagePosition);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);
  protected
    function GetPictureSource: TGraphic;

    procedure StartAnimation;
    procedure StopAnimation;
    procedure OnAnimationTimer(Sender: TObject);

    function CanObserve(const ID: Integer): Boolean; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect(ARect: TRect): TRect;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure DrawAdditionalContent(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; var ADrawCaption: Boolean); override;
    procedure CustomUpdateControl; override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImageChanged;
  published
    property Align;
    property Anchors;
    property AutoSize;

    property Images: TscCustomImageCollection read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property InterpolationMode: TscGPInterpolationMode
      read FInterpolationMode write SetInterpolationMode;

    property ClipFrame: TscGPImageClipFrame
      read FClipFrame write SetClipFrame;
    property ClipFrameRadius: Integer
      read FClipFrameRadius write SetClipFrameRadius;
    property ClipFrameFillColor: TColor
      read FClipFrameFillColor write SetClipFrameFillColor;
    property ClipFrameFillColorAlpha: Byte
      read FClipFrameFillColorAlpha write SetClipFrameFillColorAlpha;
    property ClipFrameColor: TColor read
      FClipFrameColor write SetClipFrameColor;
    property ClipFrameWidth: Integer
      read FClipFrameWidth write SetClipFrameWidth;

    property DrawTextMode;

    property Position: TscGPImagePosition read FPosition write SetPosition default scgpipLeft;
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
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    property PngImage: TPngImage read FPngImage write SetPngImage;
    property PopupMenu;
    property Proportional: Boolean read FProportional write SetProportional default false;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
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
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPImageCollectionItem = class(TCollectionItem)
  private
    FBitmap: TBitmap;
    FPngImage: TPngImage;
    FOptions: TscGPBitmapOptions;
    FDrawStyle: TscImageDrawStyle;
    FTileOffsetX: Integer;
    FTileOffsetY: Integer;
    FDescription: String;
    FContentLeftMargin: Integer;
    FContentRightMargin: Integer;
    FContentTopMargin: Integer;
    FContentBottomMargin: Integer;
    FColor: TColor;
    FScaled: Boolean;
    FProportional: Boolean;
    FVirtualWidth, FVirtualHeight: Integer;
    FPixelOffsetModeHighQuality: Boolean;
    FInterpolationModeHighQuality: Boolean;
    function GetTransparent: Boolean;
    procedure SetBitmap(Value: TBitmap);
    procedure SetPngImage(Value: TPngImage);
    function GetFullUpdate: Boolean;
    procedure SetColor(AValue: TColor);
    procedure OnBitmapChange(Sender: TObject);
    procedure PngChanged;
  protected
    function GetDisplayName: string; override;
  public
    GPB: TGPBitmap;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadPngFromResourceName(Instance: THandle;
      const ResName: String);
    procedure LoadPngFromFile(AFileName: String);
    property NeedFullUpdate: Boolean read GetFullUpdate;
    property Transparent: Boolean read GetTransparent;
    property Color: TColor read FColor write SetColor;
    procedure ImageChanged;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property PngImage: TPngImage read FPngImage write SetPngImage;
    property ContentLeftMargin: Integer
      read FContentLeftMargin write FContentLeftMargin;
    property ContentRightMargin: Integer
      read FContentRightMargin write FContentRightMargin;
    property ContentTopMargin: Integer
      read FContentTopMargin write FContentTopMargin;
    property ContentBottomMargin: Integer
      read FContentBottomMargin write FContentBottomMargin;
    property Options: TscGPBitmapOptions
      read FOptions write FOptions;
    property DrawStyle: TscImageDrawStyle read FDrawStyle write FDrawStyle;
    property VirtualWidth: Integer
      read FVirtualWidth write FVirtualWidth;
    property VirtualHeight: Integer
      read FVirtualHeight write FVirtualHeight;
    property TileOffsetX: Integer read FTileOffsetX write FTileOffsetX;
    property TileOffsetY: Integer read FTileOffsetY write FTileOffsetY;
    property Proportional: Boolean
     read FProportional write FProportional;
    property Description: String read FDescription write FDescription;
    property Scaled: Boolean read FScaled write FScaled;
    property PixelOffsetModeHighQuality: Boolean
      read FPixelOffsetModeHighQuality write FPixelOffsetModeHighQuality;
    property InterpolationModeHighQuality: Boolean
      read FInterpolationModeHighQuality write FInterpolationModeHighQuality;
  end;

  TscGPImageCollection = class;

  TscGPImageCollectionItems = class(TCollection)
  private
    function GetItem(Index: Integer): TscGPImageCollectionItem;
    procedure SetItem(Index: Integer; Value: TscGPImageCollectionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    FImageCollection: TscGPImageCollection;
    constructor Create(ImageList: TscGPImageCollection);
    function Add: TscGPImageCollectionItem;
    property Items[Index: Integer]: TscGPImageCollectionItem read GetItem write SetItem; default;
  end;

  TscGPImageCollection = class(TscCustomImageCollection)
  private
    FImages: TscGPImageCollectionItems;
  protected
    function GetCount: Integer; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetWidth(AIndex: Integer; AScaleFactor: Double = 1): Integer; override;
    function GetHeight(AIndex: Integer; AScaleFactor: Double = 1): Integer; override;
    function GetMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect; override;
    function GetContentMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect; override;
    function NeedFullUpdate(AIndex: Integer): Boolean; override;
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function IsSolidDrawing(AIndex: Integer): Boolean; override;
    function IsStretchTileDrawing(AIndex: Integer): Boolean; override;
    procedure DrawToGraphic(G: TGPGraphics; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1);
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1); override;
    procedure DrawRect(ACanvas: TCanvas; ADestRect, ASrcRect: TRect; AIndex: Integer);
  published
    property Images: TscGPImageCollectionItems read FImages write FImages;
  end;

  TscGPPixelOffsetMode = (scgppomDefault, scgppomHighQuality);

  TscGPVirtualImageList = class(TscCustomImageList)
  private
    FLoading: Boolean;
    FSourceImageList: TImageList;
    FSourceImageCollection: TscGPImageCollection;
    FDefaultWidth: Integer;
    FDefaultHeight: Integer;
    FPixelOffsetMode: TscGPPixelOffsetMode;
    FInterpolationMode: TscGPInterpolationMode;
    FDirectDraw: Boolean;
    procedure SetDirectDraw(Value: Boolean);
    procedure SetSourceImageList(Value: TImageList);
    procedure SetSourceImageCollection(Value: TscGPImageCollection);
    procedure InsertBitMaps;
  protected
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    procedure DoDrawInternal(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style:
      Cardinal; Enabled: Boolean = True);
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style:
      Cardinal; Enabled: Boolean = True); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetScaleFactor(Value: Double);
    procedure Change; override;
    procedure Update;
  published
    property ColorDepth;
    property DirectDraw: Boolean
      read FDirectDraw write SetDirectDraw;
    property PixelOffsetMode: TscGPPixelOffsetMode
      read FPixelOffsetMode write FPixelOffsetMode;
    property InterpolationMode: TscGPInterpolationMode
      read FInterpolationMode write FInterpolationMode;
    property Width;
    property Height;
    property DefaultWidth: Integer
      read FDefaultWidth write FDefaultWidth default 16;
    property DefaultHeight: Integer
      read FDefaultHeight write FDefaultHeight default 16;
    property SourceImageList: TImageList read FSourceImageList write SetSourceImageList;
    property SourceImageCollection: TscGPImageCollection read
      FSourceImageCollection write SetSourceImageCollection;
  end;

implementation

uses
  System.Math, Vcl.Forms, Vcl.Consts, Vcl.Themes, scGPControls;

constructor TscGPImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csPannable];
  FCanEmpty := False;
  FInterpolationMode := scgppimDefault;
  GPB := nil;
  FAnimationTimer := nil;
  FImages := nil;
  FImageIndex := -1;
  FRotationAngle := 0;
  FRotateAnimation := False;
  FPicture := TPicture.Create;
  FPngImage := TPngImage.Create;
  FAnimationAcceleration := False;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := Progress;
  FClipFrame := scgpcfNone;
  FClipFrameFillColor := clWindow;
  FClipFrameFillColorAlpha := 255;
  FClipFrameColor := clBtnFace;
  FClipFrameWidth := 2;
  FClipFrameRadius := 10;
  Height := 105;
  Width := 105;
end;

destructor TscGPImage.Destroy;
begin
  if FAnimationTimer <> nil then
    FAnimationTimer.Free;
  if GPB <> nil then
    GPB.Free;
  FPicture.Free;
  FPngImage.Free;
  inherited Destroy;
end;

procedure TscGPImage.Loaded;
begin
  inherited;
  if not FPngImage.Empty then
    PictureChanged(Self);
end;

function TscGPImage.GetPictureSource: TGraphic;
begin
  if not FPngImage.Empty then
    Result := FPngImage
  else
  if (FImages <> nil) and FImages.IsIndexAvailable(FImageIndex) and
     (FImages is TscImageCollection) and
     (TscImageCollection(FImages).Images[FImageIndex].Picture.Graphic <> nil)
  then
    Result := TscImageCollection(FImages).Images[FImageIndex].Picture.Graphic
  else
    Result := Self.Picture.Graphic;
end;

procedure TscGPImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TscGPImage.OnAnimationTimer(Sender: TObject);
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

procedure TscGPImage.SetInterpolationMode(Value: TscGPInterpolationMode);
begin
  if Value <> FInterpolationMode then
  begin
    FInterpolationMode := Value;
    RePaintControl;
  end;
end;

procedure TscGPImage.SetImages(Value: TscCustomImageCollection);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    PictureChanged(Self);
  end;
end;

procedure TscGPImage.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    PictureChanged(Self);
  end;
end;

procedure TscGPImage.StartAnimation;
begin
  if FAnimationTimer <> nil then Exit;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := 40;
  FAnimationTimer.Enabled := True;
end;

procedure TscGPImage.StopAnimation;
begin
  if FAnimationTimer = nil then Exit;
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Free;
  FAnimationTimer := nil;
end;

procedure TscGPImage.SetRotateAnimation(Value: Boolean);
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

procedure TscGPImage.SetRotationAngle;
begin
  if (Value >= -360) and (Value <= 360) and
    (FRotationAngle <> Value) then
  begin
    FRotationAngle := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPImage.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FClipFrameWidth := MulDiv(FClipFrameWidth, M, D);
  FClipFrameRadius := MulDiv(FClipFrameRadius, M, D);
end;

function TscGPImage.GetPalette: HPALETTE;
var
  G: TGraphic;
begin
  Result := 0;
  G := GetPictureSource;
  if G <> nil then
    Result := G.Palette;
end;

function TscGPImage.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditLinkID then
    Result := True;
end;

procedure TscGPImage.SetClipFrame(Value: TscGPImageClipFrame);
begin
  if FClipFrame <> Value then
  begin
    FClipFrame := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPImage.SetClipFrameRadius(Value: Integer);
begin
  if (Value >= 0) and (FClipFrameRadius <> Value) then
  begin
    FClipFrameRadius := Value;
    if FClipFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPImage.SetClipFrameFillColor(Value: TColor);
begin
  if (FClipFrameFillColor <> Value) then
  begin
    FClipFrameFillColor := Value;
    if FClipFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPImage.SetClipFrameFillColorAlpha(Value: Byte);
begin
  if (FClipFrameFillColorAlpha <> Value) then
  begin
    FClipFrameFillColorAlpha := Value;
    if FClipFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPImage.SetClipFrameColor(Value: TColor);
begin
  if (FClipFrameColor <> Value) then
  begin
    FClipFrameColor := Value;
    if FClipFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPImage.SetClipFrameWidth(Value: Integer);
begin
  if (FClipFrameWidth <> Value) then
  begin
    FClipFrameWidth := Value;
    if FClipFrame <> scgpcfNone then
      RePaintControl;
  end;
end;

procedure TscGPImage.DrawAdditionalContent(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState;  var ADrawCaption: Boolean);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R: TRect;
  Save: Boolean;
  G: TGPGraphics;
  B: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR: TGPRectF;
  C: Cardinal;
  l, t, w, h, d: Single;
  Rgn: TGPRegion;
  M: TGPMatrix;
  CenterP: TGPPointF;
  ImagesGPB: TGPBitmap;
  S: String;
  Flags: Longint;
begin
  M := nil;
  if  (FImages <> nil) and (FImages is TscGPImageCollection) and
      FImages.IsIndexAvailable(FImageIndex) and
      (TscGPImageCollection(FImages).FImages[FImageIndex].GPB <> nil)
  then
    ImagesGPB := TscGPImageCollection(FImages).FImages[FImageIndex].GPB
  else
    ImagesGPB := nil;
  if FClipFrame = scgpcfNone then
  begin
    Save := FDrawing;
    FDrawing := True;
    if (ImagesGPB <> nil) or (GPB <> nil) then
    begin
      G := TGPGraphics.Create(ACanvas.Handle);
      G.SetPixelOffsetMode(PixelOffsetModeHighQuality);

      case FInterpolationMode of
        scgppimDefault:
          G.SetInterpolationMode(InterpolationModeHighQuality);
        scgppimHighQualityBilinear:
          G.SetInterpolationMode(InterpolationModeHighQualityBilinear);
      end;

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
      try
        if (ImagesGPB <> nil) and (FImages <> nil) then
          TscGPImageCollection(FImages).DrawToGraphic(G, ARect, FImageIndex, FScaleFactor)
        else
        begin
          R := DestRect(ARect);
          G.DrawImage(GPB, RectToGPRect(R));
        end;
      finally
        if M <> nil then
          M.Free;
        G.Free;
        FDrawing := Save;
      end;
    end
    else
      FDrawing := Save;
    Exit;
  end;
  R := ARect;
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
  case FInterpolationMode of
    scgppimDefault:
      G.SetInterpolationMode(InterpolationModeHighQuality);
    scgppimHighQualityBilinear:
      G.SetInterpolationMode(InterpolationModeHighQualityBilinear);
  end;
  G.SetInterpolationMode(InterpolationModeHighQuality);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, FClipFrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  InflateRect(R, -1, -1);
  FillR := RectToGPRect(R);
  InflateRect(R, 1, 1);
  FrameR := RectToGPRect(R);
  if FClipFrameWidth > 0 then
    InflateGPRect(FrameR, -FClipFrameWidth / 2, -FClipFrameWidth / 2);
  try
    // fill frame
    if (FClipFrameRadius = 0) or (FClipFrame = scgpcfEllipse) then
    begin
      C := ColorToGPColor(GetStyleColor(FClipFrameFillColor), FClipFrameFillColorAlpha);
      B.SetColor(C);
      if FClipFrame = scgpcfEllipse then
        FillPath.AddEllipse(FillR)
      else
        FillPath.AddRectangle(FillR);
      G.FillPath(B, FillPath);
    end
    else
    begin
      C := ColorToGPColor(GetStyleColor(FClipFrameFillColor), FClipFrameFillColorAlpha);
      B.SetColor(C);
      l := FillR.X;
      t := FillR.y;
      w := FillR.Width;
      h := FillR.Height;
      d := FClipFrameRadius * 2;
      FillPath.StartFigure;
      FillPath.AddArc(l, t, d, d, 180, 90);
      FillPath.AddArc(l + w - d, t, d, d, 270, 90);
      FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
      FillPath.AddArc(l, t + h - d, d, d, 90, 90);
      FillPath.CloseFigure;
      G.FillPath(B, FillPath);
    end;
    // draw image
    Save := FDrawing;
    FDrawing := True;
    if (GPB <> nil) or (ImagesGPB <> nil) then
    begin
      try
        Rgn := TGPRegion.Create(FillPath);
        try
          G.SetClip(Rgn);
          if FRotationAngle <> 0 then
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
          if (ImagesGPB <> nil) and (FImages <> nil) then
            TscGPImageCollection(FImages).DrawToGraphic(G, ARect, FImageIndex, FScaleFactor)
          else
          begin
            R := DestRect(ARect);
            G.DrawImage(GPB, RectToGPRect(R));
          end;
          if FRotationAngle <> 0 then
            G.ResetTransform;
        finally
          G.ResetClip;
          Rgn.Free;
        end;
      finally
        FDrawing := Save;
      end;
    end
    else
      FDrawing := Save;
    // darw frame
    if (FClipFrameRadius = 0) or (FClipFrame = scgpcfEllipse) then
    begin
      if FClipFrameColor <> clNone then
      begin
        C := ColorToGPColor(GetStyleColor(FClipFrameColor), 255);
        P.SetColor(C);
        if FClipFrame = scgpcfEllipse then
          G.DrawEllipse(P, FrameR)
        else
          G.DrawRectangle(P, FrameR);
      end;
    end
    else
    begin
      if (FClipFrameColor <> clNone) then
      begin
        C := ColorToGPColor(GetStyleColor(FClipFrameColor), 255);
        P.SetColor(C);
        l := FrameR.X;
        t := FrameR.y;
        w := FrameR.Width;
        h := FrameR.Height;
        d := FClipFrameRadius * 2;
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

function TscGPImage.DestRect(ARect: TRect): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
  G: TGraphic;
begin
  G := GetPictureSource;
  if G <> nil then
  begin
    w := G.Width;
    h := G.Height;
  end
  else
  begin
    w := 0;
    h := 0;
  end;
  cw := ARect.Width;
  ch := ARect.Height;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if FPosition = scgpipCenter then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2)
  else
  if FPosition = scgpipRight then
    OffsetRect(Result, cw - w, 0);
  OffsetRect(Result, ARect.Left, ARect.Top);
end;

function TscGPImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
  G: TGraphic;
begin
  Result := False;
  G := GetPictureSource;
  Tmp := G;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
    (Tmp.PaletteModified) then
  begin
    if (Tmp.Palette = 0) then
      Tmp.PaletteModified := False
    else
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
      begin
        if FDrawing then
          ParentForm.Perform(wm_QueryNewPalette, 0, 0)
        else
          PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
        Result := True;
        Tmp.PaletteModified := False;
      end;
    end;
  end;
end;

procedure TscGPImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
    if DoPaletteChange then Update
    else Paint;
  end;
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TscGPImage.SetPosition(Value: TscGPImagePosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    PictureChanged(Self);
  end;
end;

procedure TscGPImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TscGPImage.SetPngImage(Value: TPngImage);
begin
  FPngImage.Assign(Value);
  PictureChanged(Self);
end;

procedure TscGPImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    PictureChanged(Self);
  end;
end;

procedure TscGPImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    PictureChanged(Self);
  end;
end;

procedure TscGPImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    PictureChanged(Self);
  end;
end;

procedure TscGPImage.ImageChanged;
var
  G: TGraphic;
  MemStream: TMemoryStream;
  W, H: Integer;
begin
  G := GetPictureSource;

  if AutoSize and (G <> nil) and (G.Width > 0) and (G.Height > 0) then
  	SetBounds(Left, Top, G.Width, G.Height);

  if AutoSize and (FImages <> nil) and (FImages is TscGPImageCollection) and
     FImages.IsIndexAvailable(FImageIndex)
  then
  begin
    W := FImages.GetWidth(FImageIndex, FScaleFactor);
    H := FImages.GetHeight(FImageIndex, FScaleFactor);
    if (W > 0) and (H > 0) then
      SetBounds(Left, Top, W, H);
  end;

  if not PngImage.Empty then
  begin
    if GPB <> nil then
      FreeAndNil(GPB);
    if not PngImage.Empty then
    begin
      MemStream := TMemoryStream.Create;
      FPngImage.SaveToStream(MemStream);
      try
        MemStream.Position := 0;
        GPB := TGPBitmap.Create(TStreamAdapter.Create(MemStream));
      finally
        FreeAndNil(MemStream);
      end;
    end;
  end
  else
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := FTransparent;
    if DoPaletteChange and FDrawing then Update;
    if GPB <> nil then
      FreeAndNil(GPB);
    GPB := scGPUtils.GraphicToGPBitmap(G);
  end
  else
  begin
    if GPB <> nil then
      FreeAndNil(GPB);
  end;

  if not FDrawing then
  begin
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPImage.PictureChanged(Sender: TObject);
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkEdit(Observers) then
      TLinkObservers.EditLinkModified(Observers);

  ImageChanged;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkIsEditing(Observers) then
      TLinkObservers.EditLinkUpdate(Observers);
end;

function TscGPImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  G: TGraphic;
begin
  Result := True;
  G := GetPictureSource;
  if G <> nil then
  begin
    if not (csDesigning in ComponentState) or (G.Width > 0) and
      (G.Height > 0) then
    begin
      if Align in [alNone, alLeft, alRight] then
        NewWidth := G.Width;
      if Align in [alNone, alTop, alBottom] then
        NewHeight := G.Height;
    end;
  end
  else
  if (FImages <> nil) and (FImages is TscGPImageCollection) and
      FImages.IsIndexAvailable(FImageIndex)
  then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := FImages.GetWidth(FImageIndex, FScaleFactor);
    if Align in [alNone, alTop, alBottom] then
      NewHeight := FImages.GetHeight(FImageIndex, FScaleFactor);
  end;
end;

procedure TscGPImage.CMStyleChanged(var Message: TMessage);
var
  G: TGraphic;
begin
  inherited;
  if Transparent then
  begin
    G := GetPictureSource;
    if (G <> nil) and not ((G is TMetaFile) or (G is TIcon)) and G.Transparent then
    begin
      G.Transparent := False;
      G.Transparent := True;
    end;
  end;
end;

procedure TscGPImage.CustomUpdateControl;
begin
  if Picture.Graphic <> nil then
  begin
    if FStretch or FProportional or (FPosition = scgpipCenter) or
       (FPosition = scgpipRight) then
      UpdateControls;
  end;
end;

constructor TscGPVirtualImageList.Create(AOwner: TComponent);
begin
  inherited;
  FLoading := False;
  FDirectDraw := True;
  FDefaultWidth := 16;
  FDefaultHeight := 16;
  FSourceImageList := nil;
  FPixelOffsetMode := scgppomDefault;
  FInterpolationMode := scgppimDefault;
end;

procedure TscGPVirtualImageList.Loaded;
begin
  inherited;
  if not FDirectDraw then
    InsertBitmaps;
end;

procedure TscGPVirtualImageList.SetDirectDraw(Value: Boolean);
begin
  if FDirectDraw <> Value then
  begin
    FDirectDraw := Value;
    if not FDirectDraw then
      ColorDepth := cd32bit;
    InsertBitmaps;
  end;
end;

procedure TscGPVirtualImageList.Change;
begin
  inherited;

  if (Count = 0) and not FLoading then
  begin
    if not (csDestroying in ComponentState) then
      InsertBitMaps;
  end;

  if csDesigning in ComponentState then
  begin
    FDefaultWidth := Width;
    FDefaultHeight := Height;
  end;

end;

procedure TscGPVirtualImageList.ReadData(Stream: TStream);
begin

end;

procedure TscGPVirtualImageList.WriteData(Stream: TStream);
begin

end;

procedure TscGPVirtualImageList.SetScaleFactor(Value: Double);
begin
  if Value >= 0 then
    SetSize(Round(FDefaultWidth * Value), Round(FDefaultHeight * Value));
end;

procedure TscGPVirtualImageList.Update;
var
  I, J, ICount: Integer;
  B: TBitmap;
begin
  if (FSourceImageList = nil) and (FSourceImageCollection = nil) then
  begin
    Clear;
    Exit;
  end;

  if not FDirectDraw then
  begin
    InsertBitmaps;
    Exit;
  end;

  if FLoading then Exit;

  if (FSourceImageCollection <> nil) and (FSourceImageCollection.Count <> Self.Count) then
    ICount := FSourceImageCollection.Count
  else
  if (FSourceImageList <> nil) and (FSourceImageList.Count <> Self.Count) then
    ICount := FSourceImageList.Count
  else
    ICount := 0;

  if ICount = 0 then Exit;

  FLoading := True;

  if Self.Count < ICount then
  begin
    J := ICount - Self.Count;
    for I := 1 to J do
    begin
      B := TBitMap.Create;
      B.PixelFormat := pf1bit;
      B.Width := Self.Width;
      B.height := Self.Height;
      Add(B, nil);
      B.Free;
    end;
  end
  else
  begin
    J := Self.Count - ICount;
    for I := 1 to J do
      if Self.Count > 0 then
        Delete(Self.Count - 1);
  end;

  FLoading := False;
end;

procedure TscGPVirtualImageList.InsertBitMaps;
var
  B: TBitMap;
  I, ICount: Integer;
begin
  if (FSourceImageList = nil) and (FSourceImageCollection = nil) then Exit;
  if (FSourceImageList <> nil) and (FSourceImageList.Count = 0) then Exit;
  if (FSourceImageCollection <> nil) and (FSourceImageCollection.Count = 0) then Exit;
  if FLoading then Exit;
  
  FLoading := True;

  if Count <> 0 then
    Clear;

  if FSourceImageList <> nil then
    ICount := FSourceImageList.Count
  else
  if FSourceImageCollection <> nil then
    ICount := FSourceImageCollection.Count
  else
    ICount := 0;

  for I := 0 to ICount - 1 do
  begin
    B := TBitMap.Create;
    B.Width := Self.Width;
    B.Height := Self.Height;
    if DirectDraw then
      B.PixelFormat := pf1bit
    else
    begin
      B.PixelFormat := pf32bit;
      Bitmap_ClearAlpha(B, 0);
      B.AlphaFormat := afPremultiplied;
      DoDrawInternal(I, B.Canvas, 0, 0, 0, True);
      B.AlphaFormat := afIgnored;
    end;
    Add(B, nil);
    B.Free;
  end;

  FLoading := False;
end;

destructor TscGPVirtualImageList.Destroy;
begin
  inherited;
end;

procedure TscGPVirtualImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style:
      Cardinal; Enabled: Boolean = True);
var
  B: TBitmap;
begin
  if (FSourceImageList = nil) and (FSourceImageCollection = nil) then Exit;
  if Count = 0 then Exit;
  if DirectDraw then
    DoDrawInternal(Index, Canvas, X, Y, Style, Enabled)
  else
  if not Enabled then
  begin
    B := TBitmap.Create;
    try
     if Bitmap_GetFromImageList(Self, Index, B) then
       Bitmap_DrawAlpha_XY(B, Canvas, X, Y, 125);
    finally
      B.Free;
    end;
  end
  else
    inherited;
end;

procedure TscGPVirtualImageList.DoDrawInternal(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  B: TBitmap;
  R: TRect;
  G: TGPGraphics;
begin
  if (FSourceImageList = nil) and (FSourceImageCollection = nil) then Exit;

  if FSourceImageCollection <> nil then
  begin
    if FSourceImageCollection.IsIndexAvailable(Index) then
    begin
      if Enabled then
      begin
        R := Rect(X, Y, X + Self.Width, Y + Self.Height);
        G := TGPGraphics.Create(Canvas.Handle);
        try
        G.SetSmoothingMode(SmoothingModeHighQuality);

        case FInterpolationMode of
          scgppimDefault:
            G.SetInterpolationMode(InterpolationModeHighQuality);
          scgppimHighQualityBilinear:
            G.SetInterpolationMode(InterpolationModeHighQualityBilinear);
        end;

        if Self.PixelOffsetMode = scgppomHighQuality then
          G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
        G.DrawImage(FSourceImageCollection.Images[Index].GPB, RectToGPRect(R));
        finally
          G.Free;
        end;
      end
      else
      begin
        B := TBitmap.Create;
        B.Width := Self.Width;
        B.Height := Self.Height;
        B.PixelFormat := pf32bit;
        Bitmap_ClearAlpha(B, 0);
        R := Rect(0, 0, Self.Width, Self.Height);
        G := TGPGraphics.Create(B.Canvas.Handle);
        try
          G.SetSmoothingMode(SmoothingModeHighQuality);

          case FInterpolationMode of
          scgppimDefault:
            G.SetInterpolationMode(InterpolationModeHighQuality);
          scgppimHighQualityBilinear:
            G.SetInterpolationMode(InterpolationModeHighQualityBilinear);
          end;

          if Self.PixelOffsetMode = scgppomHighQuality then
            G.SetPixelOffsetMode(PixelOffsetModeHighQuality);
          B.AlphaFormat := afIgnored;
          G.DrawImage(FSourceImageCollection.Images[Index].GPB, RectToGPRect(R));
          B.AlphaFormat := afPremultiplied;
          scDrawUtils.Bitmap_DrawAlpha(B, Canvas, Rect(0, 0, B.Width, B.Height),
            Rect(X, Y, X + B.Width, Y + B.Height), 125);
        finally
          B.Free;
          G.Free;
        end;
      end;
    end;
    Exit;
  end;

  B := TBitmap.Create;
  try
    if Bitmap_GetFromImageListNoPremultiplied(FSourceImageList, Index, B, not FDirectDraw) then
    begin
      R := Rect(X, Y, X + Self.Width, Y + Self.Height);
      if (B.PixelFormat = pf32bit) and not Enabled then
         Bitmap_DivAlpha(B, 4);
      if FPixelOffsetMode = scgppomDefault then
        GPDrawBitmapSmooth(Canvas.Handle, R, B)
      else
        GPDrawBitmapSmooth2(Canvas.Handle, R, B);
    end;
  finally
    B.Free;
  end;
end;

procedure TscGPVirtualImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSourceImageList) then
    FSourceImageList := nil;
  if (Operation = opRemove) and (AComponent = FSourceImageCollection) then
    FSourceImageCollection := nil;
end;

procedure TscGPVirtualImageList.SetSourceImageList(Value: TImageList);
begin
  if (Value = nil) or ((Value <> nil) and (Value.ColorDepth = cd32Bit)) then
  begin
    FSourceImageList := Value;
    Clear;
  end;
end;

procedure TscGPVirtualImageList.SetSourceImageCollection(Value: TscGPImageCollection);
begin
  if Value <> FSourceImageCollection then
  begin
    FSourceImageCollection := Value;
    Clear;
  end;
end;

constructor TscGPImageCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  GPB := nil;
  FVirtualWidth := 0;
  FVirtualHeight := 0;
  FProportional := False;
  FOptions := TscGPBitmapOptions.Create;
  FScaled := True;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := OnBitmapChange;
  FPngImage := TPngImage.Create;
  FTileOffsetX := 0;
  FTileOffsetY := 0;
  FDescription := '';
  FColor := clFuchsia;
  FScaled := True;
  FContentLeftMargin := 0;
  FContentRightMargin := 0;
  FContentTopMargin := 0;
  FContentBottomMargin := 0;
  FPixelOffsetModeHighQuality := True;
  FInterpolationModeHighQuality := True;
end;

destructor TscGPImageCollectionItem.Destroy;
begin
  if GPB <> nil then
  begin
    GPB.Free;
    GPB := nil;
  end;
  FPngImage.Free;
  FBitmap.Free;
  FOptions.Free;
  inherited;
end;

procedure TscGPImageCollectionItem.PngChanged;
var
  MemStream: TMemoryStream;
begin
  if GPB <> nil then
  begin
    GPB.Free;
    GPB := nil;
  end;
  if FPngImage = nil then Exit;
  if FPngImage.Empty then Exit;
  if FPngImage.Width * FPngImage.Height = 0 then Exit;
  MemStream := TMemoryStream.Create;
  FPngImage.SaveToStream(MemStream);
  try
    MemStream.Position := 0;
    GPB := TGPBitmap.Create(TStreamAdapter.Create(MemStream));
  finally
    FreeAndNil(MemStream);
  end;
end;

procedure TscGPImageCollectionItem.ImageChanged;
begin
  if not FBitmap.Empty then
    OnBitmapChange(Self)
  else
  if not FPngImage.Empty then
    PngChanged;
end;

procedure TscGPImageCollectionItem.OnBitmapChange(Sender: TObject);
begin
  if GPB <> nil then
  begin
    GPB.Free;
    GPB := nil;
  end;
  if FBitmap = nil then Exit;
  if FBitmap.Empty then Exit;
  if FBitmap.PixelFormat = pf32bit then
  begin
    FBitmap.OnChange := nil;
    FBitmap.AlphaFormat := afDefined;
    GPB := TGPBitmap.Create(FBitmap.Width, FBitmap.Height, -FBitmap.Width * 4,
      PixelFormat32bppARGB, FBitmap.ScanLine[0]);
    FBitmap.AlphaFormat := afIgnored;
    FBitmap.OnChange := OnBitmapChange;
  end
  else
    GPB := TGPBitmap.Create(FBitmap.Handle, FBitmap.Palette);
end;

procedure TscGPImageCollectionItem.SetColor(AValue: TColor);
var
  R, G, B, A: Byte;
  C: TColor;
  GPC: TGPColor;
  X, Y: Integer;
begin
  if ColorToRGB(FColor) <> ColorToRGB(AValue) then
  begin
    FColor := AValue;
    if Transparent and (GPB <> nil) and (GPB.GetWidth > 0) and (GPB.GetHeight > 0) then
    begin
      C := ColorToRGB(FColor);
      R := GetRValue(C);
      G := GetGValue(C);
      B := GetBValue(C);
      for X := 0 to GPB.GetWidth - 1 do
        for Y := 0 to GPB.GetHeight - 1 do
        begin
          GPB.GetPixel(X, Y, GPC);
          A := GetAlpha(GPC);
          GPC := MakeColor(A, R, G, B);
          GPB.SetPixel(X, Y, GPC);
        end;
    end;
  end;
end;

procedure TscGPImageCollectionItem.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TscGPImageCollectionItem.SetPngImage(Value: TPngImage);
begin
  FPngImage.Assign(Value);
  PngChanged;
end;

function TscGPImageCollectionItem.GetFullUpdate: Boolean;
begin
  Result := FDrawStyle in
    [idsStretch, idsCenter,
     idsTopRight, idsBottomLeft, idsBottomRight,
     idsRightTile, idsBottomTile,
     idsTopStretch, idsLeftStretch, idsRightStretch, idsBottomStretch,
     idsHorzStretchTile, idsVertStretchTile, idsHorzCenterStretch, idsVertCenterStretch,
     idsLeftCenter, idsTopCenter, idsRightCenter, idsBottomCenter];
  if not Result and (FDrawStyle = idsTile) and
     (Options.LeftMargin > 0) and (Options.TopMargin > 0) and
     (Options.RightMargin > 0) and (Options.BottomMargin > 0)
   then
     Result := True;
end;

procedure TscGPImageCollectionItem.LoadPngFromFile(AFileName: String);
begin
  FPngImage.LoadFromFile(AFileName);
  PngChanged;
end;

procedure TscGPImageCollectionItem.LoadPngFromResourceName(Instance: THandle;
  const ResName: String);
begin
  FPngImage.LoadFromResourceName(Instance, ResName);
  PngChanged;
end;

procedure TscGPImageCollectionItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

function TscGPImageCollectionItem.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
end;

function TscGPImageCollectionItem.GetTransparent: Boolean;
begin
  Result := False;
  if not FBitmap.Empty then
    Result := FBitmap.PixelFormat = pf32bit
  else
  if not FPngImage.Empty then
    Result := FPngImage.TransparencyMode = ptmPartial;
end;

constructor TscGPImageCollectionItems.Create(ImageList: TscGPImageCollection);
begin
  inherited Create(TscGPImageCollectionItem);
  FImageCollection := ImageList;
end;

function TscGPImageCollectionItems.GetItem(Index: Integer): TscGPImageCollectionItem;
begin
  Result := TscGPImageCollectionItem(inherited GetItem(Index));
end;

procedure TscGPImageCollectionItems.SetItem(Index: Integer; Value: TscGPImageCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TscGPImageCollectionItems.GetOwner: TPersistent;
begin
  Result := FImageCollection;
end;

procedure TscGPImageCollectionItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

function TscGPImageCollectionItems.Add: TscGPImageCollectionItem;
begin
  Result := TscGPImageCollectionItem(inherited Add);
end;

constructor TscGPImageCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaled := True;
  FImages := TscGPImageCollectionItems.Create(Self);
end;

destructor TscGPImageCollection.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

function TscGPImageCollection.GetMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not IsIndexAvailable(AIndex) then Exit;
  if not FScaled or not FImages[AIndex].Scaled then AScaleFactor := 1;
  Result.Left := Round(FImages[AIndex].Options.LeftMargin * AScaleFactor);
  Result.Top := Round(FImages[AIndex].Options.TopMargin * AScaleFactor);
  Result.Right := Round(FImages[AIndex].Options.RightMargin * AScaleFactor);
  Result.Bottom := Round(FImages[AIndex].Options.BottomMargin * AScaleFactor);
end;

function TscGPImageCollection.GetContentMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not IsIndexAvailable(AIndex) then Exit;
  if not FScaled or not FImages[AIndex].Scaled then AScaleFactor := 1;
  Result.Left := Round(FImages[AIndex].ContentLeftMargin * AScaleFactor);
  Result.Top := Round(FImages[AIndex].ContentTopMargin * AScaleFactor);
  Result.Right := Round(FImages[AIndex].ContentRightMargin * AScaleFactor);
  Result.Bottom := Round(FImages[AIndex].ContentBottomMargin * AScaleFactor);
end;

procedure TscGPImageCollection.Loaded;
var
  I: Integer;
begin
  inherited;
  if Count = 0 then Exit;
  for I := 0 to FImages.Count - 1 do
   if FImages[I].GPB = nil then
   begin
     if not FImages[I].PngImage.Empty then
       FImages[I].PngChanged
     else
     if not FImages[I].FBitmap.Empty then
       FImages[I].OnBitmapChange(Self);
  end;
end;

function TscGPImageCollection.GetCount: Integer;
begin
  Result := FImages.Count;
end;

function TscGPImageCollection.IsStretchTileDrawing(AIndex: Integer): Boolean;
begin
  Result := IsIndexAvailable(AIndex) and
     (FImages[AIndex].DrawStyle in [idsTile, idsStretch]);
end;

function TscGPImageCollection.IsSolidDrawing(AIndex: Integer): Boolean;
begin
  Result := IsIndexAvailable(AIndex) and not FImages[AIndex].Transparent and
     (FImages[AIndex].DrawStyle in [idsTile, idsStretch]);
end;

function TscGPImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FImages.Count) and
   (not FImages[AIndex].FBitmap.Empty or not FImages[AIndex].FPngImage.Empty);
end;

function TscGPImageCollection.NeedFullUpdate(AIndex: Integer): Boolean;
begin
  Result := IsIndexAvailable(AIndex) and FImages.Items[AIndex].NeedFullUpdate;
end;

function TscGPImageCollection.GetWidth(AIndex: Integer; AScaleFactor: Double = 1): Integer;
begin
  Result := 0;
  if not FScaled then
    AScaleFactor := 1;
  if IsIndexAvailable(AIndex) and (FImages[AIndex].GPB <> nil) then
  begin
    if not FImages[AIndex].Scaled then AScaleFactor := 1;
    if FImages[AIndex].FVirtualWidth > 0 then
      Result := Round(FImages[AIndex].FVirtualWidth * AScaleFactor)
    else
      Result := Round(FImages[AIndex].GPB.GetWidth * AScaleFactor);
  end;
end;

function TscGPImageCollection.GetHeight(AIndex: Integer; AScaleFactor: Double = 1): Integer;
begin
  Result := 0;
  if not FScaled then
    AScaleFactor := 1;
  if IsIndexAvailable(AIndex) and (FImages[AIndex].GPB <> nil) then
  begin
    if not FImages[AIndex].Scaled then AScaleFactor := 1;
    if FImages[AIndex].FVirtualHeight > 0 then
      Result := Round(FImages[AIndex].FVirtualHeight * AScaleFactor)
    else
      Result := Round(FImages[AIndex].GPB.GetHeight * AScaleFactor);
  end;
end;

procedure TscGPImageCollection.DrawRect(ACanvas: TCanvas; ADestRect, ASrcRect: TRect; AIndex: Integer);
var
  G: TGPGraphics;
  GPB: TGPBitmap;
  R1, R2: TGPRectF;
begin
  if not IsIndexAvailable(AIndex) then Exit;
  if ASrcRect.Width = 0 then Exit;
  if ASrcRect.Height = 0 then Exit;
  if FImages[AIndex].GPB = nil then
  begin
    if not FImages[AIndex].PngImage.Empty then
      FImages[AIndex].PngChanged
    else
    if not FImages[AIndex].FBitmap.Empty then
      FImages[AIndex].OnBitmapChange(Self);
  end;
  GPB := FImages[AIndex].GPB;
  if GPB.GetWidth = 0 then Exit;
  if GPB.GetHeight = 0 then Exit;
  R1 := RectToGPRect(ADestRect);
  R2 := RectToGPRect(ASrcRect);
  G := TGPGraphics.Create(ACanvas.Handle);
  try
    G.SetSmoothingMode(SmoothingModeHighQuality);
    if Images[AIndex].InterpolationModeHighQuality then
      G.SetInterpolationMode(InterpolationModeHighQuality);
    G.DrawImage(GPB, R1, R2.X, R2.Y,
      R2.Width, R2.Height, UnitPixel);
  finally
    G.Free;
  end;
end;

procedure TscGPImageCollection.DrawToGraphic(G: TGPGraphics; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1);
var
  R: TGPRectF;
  X, Y: Double;
  W, H, IH, IW, RIH, RIW, xyaspect: Double;
  GPB: TGPBitmap;
  XCnt, YCnt, I, J: Integer;

  procedure CheckProportional;
  var
    cw, ch: Double;
  begin
    cw := ARect.Width;
    ch := ARect.Height;
    if (W > 0) and (H > 0) then
    begin
      if W <> IW then
      begin
        xyaspect := W / IW;
        H := Trunc(H * xyaspect);
      end;
      if H <> IH then
      begin
        xyaspect := H / IH;
        W := Trunc(W * xyaspect);
      end;
    end
    else
    begin
      W := cw;
      H := ch;
    end;
   end;


  procedure CheckProportional2;
  var
    cw, ch: Double;
  begin
    cw := ARect.Width;
    ch := ARect.Height;
    xyaspect := w / h;
    if (w > 0) and (h > 0) then
    begin
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
   end;

begin
  if not IsIndexAvailable(AIndex) then Exit;
  if not FImages[AIndex].Scaled then AScaleFactor := 1;
  if FImages[AIndex].GPB = nil then
  begin
    if not FImages[AIndex].PngImage.Empty then
      FImages[AIndex].PngChanged
    else
    if not FImages[AIndex].FBitmap.Empty then
      FImages[AIndex].OnBitmapChange(Self);
  end;
  if FImages[AIndex].GPB = nil then Exit;
  if FImages[AIndex].GPB.GetWidth = 0 then Exit;
  if FImages[AIndex].GPB.GetHeight = 0 then Exit;
  try
    GPB := FImages[AIndex].GPB;
    R := RectToGPRect(ARect);
    W := GetWidth(AIndex, AScaleFactor);
    H := GetHeight(AIndex, AScaleFactor);
    IH := H;
    IW := W;
    RIW := 0;
    RIH := 0;
    if not FImages[AIndex].Transparent then
    begin
      RIW := Round(IW / FImages[AIndex].GPB.GetWidth);
      RIH := Round(IH / FImages[AIndex].GPB.GetHeight);
    end;
    if  (FImages[AIndex].Options.LeftMargin > 0) and
        (FImages[AIndex].Options.RightMargin > 0) and
        (FImages[AIndex].Options.TopMargin > 0) and
        (FImages[AIndex].Options.RightMargin > 0) then
    begin
      GPBitmap_DrawWithOptions(G, GPB,
        FImages[AIndex].Options, R,
        FImages[AIndex].VirtualWidth,
        FImages[AIndex].VirtualHeight,
        AScaleFactor);
    end
    else
    if (FImages[AIndex].Options.LeftMargin > 0) and
       (FImages[AIndex].Options.RightMargin > 0)
    then
    begin
      case FImages[AIndex].DrawStyle of
        idsTopLeft, idsTopRight, idsTopStretch, idsTopTile:
          R.Height := H;
        idsBottomLeft, idsBottomRight, idsBottomStretch, idsBottomTile:
        begin
          R.Height := H;
          R.Y := ARect.Bottom - H;
        end;
        idsCenter, idsHorzCenterStretch:
        begin
          R.Height := H;
          R.Y := Round(ARect.Top + ARect.Height div 2 - H / 2);
        end;
      end;
      GPBitmap_DrawWithOptions(G, GPB,
        FImages[AIndex].Options, R,
        FImages[AIndex].VirtualWidth,
        FImages[AIndex].VirtualHeight,
        AScaleFactor);
    end
    else
    if (FImages[AIndex].Options.TopMargin > 0) and
       (FImages[AIndex].Options.BottomMargin > 0)
    then
    begin
      case FImages[AIndex].DrawStyle of
        idsTopLeft, idsBottomLeft, idsLeftStretch, idsLeftTile:
          R.Height := H;
        idsTopRight, idsBottomRight, idsRightStretch, idsRightTile:
        begin
          R.Width := W;
          R.X := ARect.Right - W;
        end;
        idsCenter, idsVertCenterStretch:
        begin
          R.Width := W;
          R.X := Round(ARect.Left + ARect.Width div 2 - W / 2);
        end;
      end;
      GPBitmap_DrawWithOptions(G, GPB,
        FImages[AIndex].Options, R,
        FImages[AIndex].VirtualWidth,
        FImages[AIndex].VirtualHeight,
        AScaleFactor);
    end
    else
    case FImages[AIndex].DrawStyle of
      idsStretch:
      begin
        if FImages[AIndex].FProportional then
        begin
          CheckProportional2;
          R.Width := W;
          R.Height := H;
          R.X := Round(ARect.Left + ARect.Width / 2 - W / 2);
          R.Y := Round(ARect.Top + ARect.Height / 2 - H / 2);
        end;
        G.DrawImage(GPB, R);
      end;
      idsTopStretch:
      begin
        R.Height := H;
        if FImages[AIndex].FProportional then
        begin
          W := R.Width;
          CheckProportional;
          R.Height := H;
        end;
        G.DrawImage(GPB, R);
      end;
      idsLeftStretch:
      begin
        R.Width := W;
        if FImages[AIndex].FProportional then
        begin
          H := R.Height;
          CheckProportional;
          R.Width := W;
        end;
        G.DrawImage(GPB, R);
      end;
      idsRightStretch:
      begin
        R.Width := W;
        if FImages[AIndex].FProportional then
        begin
          H := R.Height;
          CheckProportional;
          R.Width := W;
        end;
        R.X := ARect.Right - W;
        G.DrawImage(GPB, R);
      end;
      idsBottomStretch:
      begin
        R.Height := H;
        if FImages[AIndex].FProportional then
        begin
          W := R.Width;
          CheckProportional;
          R.Height := H;
        end;
        R.Y := ARect.Bottom - H;
        G.DrawImage(GPB, R);
      end;
      idsHorzCenterStretch:
      begin
        R.Y := Round(R.Y + R.Height / 2 - H / 2);
        R.Height := H;
        if FImages[AIndex].FProportional then
        begin
          W := R.Width;
          CheckProportional;
          R.Height := H;
          R.Y := Round(ARect.Top + ARect.Height / 2 - H / 2);
        end;
        G.DrawImage(GPB, R);
      end;
      idsVertCenterStretch:
      begin
        R.X := Round(R.X + R.Width / 2 - W / 2);
        R.Width := W;
        if FImages[AIndex].FProportional then
        begin
          H := R.Height;
          CheckProportional;
          R.Width := W;
          R.X := Round(ARect.Left + ARect.Width / 2 - W / 2);
        end;
        G.DrawImage(GPB, R);
      end;
      idsCenter:
      begin
        X := Round(R.X + R.Width / 2 - W / 2);
        Y := Round(R.Y + R.Height / 2 - H / 2);
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsTopCenter:
      begin
        X := Round(R.X + R.Width / 2 - W / 2);
        Y := R.Y;
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsTopLeft:
      begin
        X := R.X;
        Y := R.Y;
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsTopRight:
      begin
        X := ARect.Right - W;
        Y := R.Y;
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsBottomCenter:
      begin
        X := Round(R.X + R.Width / 2 - W / 2);
        Y := ARect.Bottom - H;
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsBottomLeft:
      begin
        X := R.X;
        Y := ARect.Bottom - H;
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsBottomRight:
      begin
        X := ARect.Right - W;
        Y := ARect.Bottom - H;
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsLeftCenter:
      begin
        X := R.X;
        Y := Round(R.Y + R.Height / 2 - H / 2);
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsRightCenter:
      begin
        X := ARect.Right - W;
        Y := Round(R.Y + R.Height / 2 - H / 2);
        G.DrawImage(GPB, X, Y, W, H);
      end;
      idsTile:
      begin
        XCnt := ARect.Width div Round(W + FImages[AIndex].TileOffsetX * AScaleFactor);
        YCnt := ARect.Height div Round(H + FImages[AIndex].TileOffsetY * AScaleFactor);
        for I := 0 to XCnt do
          for J := 0 to YCnt do
          begin
            X := R.X + I * W + Round(FImages[AIndex].TileOffsetX * AScaleFactor);
            Y := R.Y + J * H + Round(FImages[AIndex].TileOffsetY * AScaleFactor);
            G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
          end;
      end;
      idsLeftTile:
      begin
        YCnt := ARect.Height div Round(H + FImages[AIndex].TileOffsetY * AScaleFactor);
        X := R.X;
        for J := 0 to YCnt do
        begin
          Y := R.Y + J * H + Round(FImages[AIndex].TileOffsetY * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
      idsRightTile:
      begin
        YCnt := ARect.Height div Round(H + FImages[AIndex].TileOffsetY * AScaleFactor);
        X := ARect.Right - W;
        for J := 0 to YCnt do
        begin
          Y := R.Y + J * H + Round(FImages[AIndex].TileOffsetY * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
      idsTopTile:
      begin
        XCnt := ARect.Width div Round(W + FImages[AIndex].TileOffsetX * AScaleFactor);
        Y := R.Y;
        for I := 0 to XCnt do
        begin
          X := R.X + I * W + Round(FImages[AIndex].TileOffsetX * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
      idsBottomTile:
      begin
        XCnt := ARect.Width div Round(W + FImages[AIndex].TileOffsetX * AScaleFactor);
        Y := ARect.Bottom - H;
        for I := 0 to XCnt do
        begin
          X := R.X + I * W + Round(FImages[AIndex].TileOffsetX * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
      idsVertStretchTile:
      begin
        YCnt := ARect.Height div Round(H + FImages[AIndex].TileOffsetY * AScaleFactor);
        X := R.X;
        W := R.Width;
        for J := 0 to YCnt do
        begin
          Y := R.Y + J * H + Round(FImages[AIndex].TileOffsetY * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
      idsHorzStretchTile:
      begin
        XCnt := ARect.Width div Round(W + FImages[AIndex].TileOffsetX * AScaleFactor);
        Y := R.Y;
        H := R.Height;
        for I := 0 to XCnt do
        begin
          X := R.X + I * W + Round(FImages[AIndex].TileOffsetX * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
      idsHorzCenterTile:
      begin
        XCnt := ARect.Width div Round(W + FImages[AIndex].TileOffsetX * AScaleFactor);
        Y := Round(R.Y + R.Height / 2 - H / 2);
        for I := 0 to XCnt do
        begin
          X := R.X + I * W + Round(FImages[AIndex].TileOffsetX * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
      idsVertCenterTile:
      begin
        YCnt := ARect.Height div Round(H + FImages[AIndex].TileOffsetY * AScaleFactor);
        X := Round(R.X + R.Width / 2 - W / 2);
        for J := 0 to YCnt do
        begin
          Y := R.Y + J * H + Round(FImages[AIndex].TileOffsetY * AScaleFactor);
          G.DrawImage(GPB, X, Y, W + RIW, H + RIH);
        end;
      end;
    end;
  finally
  end;
end;

procedure TscGPImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1);
var
  G: TGPGraphics;
begin
  if not IsIndexAvailable(AIndex) then Exit;
  G := TGPGraphics.Create(ACanvas.Handle);

  if Images[AIndex].PixelOffsetModeHighQuality then
    G.SetPixelOffsetMode(PixelOffsetModeHighQuality)
  else
    G.SetPixelOffsetMode(PixelOffsetModeDefault);

  if Images[AIndex].InterpolationModeHighQuality then
    G.SetInterpolationMode(InterpolationModeHighQuality)
  else
    G.SetInterpolationMode(InterpolationModeDefault);

  try
    DrawToGraphic(G, ARect, AIndex, AScaleFactor);
  finally
    G.Free;
  end;
end;

end.


