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

unit scImageCollection;

{$R-}

interface

uses
   Winapi.Windows,  Winapi.Messages, Vcl.Graphics, Vcl.Controls,
    Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
    System.SysUtils, System.Classes, scDrawUtils;

type

  TscImageCollection = class;
  TscImageDrawStyle = (idsTile, idsStretch, idsCenter,
    idsTopLeft, idsTopRight, idsBottomLeft, idsBottomRight,
    idsTopTile, idsLeftTile, idsRightTile, idsBottomTile,
    idsTopStretch, idsLeftStretch, idsRightStretch, idsBottomStretch,
    idsHorzStretchTile, idsVertStretchTile, idsHorzCenterStretch, idsVertCenterStretch,
    idsLeftCenter, idsTopCenter, idsRightCenter, idsBottomCenter,
    idsHorzCenterTile, idsVertCenterTile);

  TscCustomImageCollection = class(TComponent)
  protected
    FScaled: Boolean;
    procedure Change; virtual;
    function GetCount: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetWidth(AIndex: Integer; AScaleFactor: Double = 1): Integer; virtual;
    function GetHeight(AIndex: Integer; AScaleFactor: Double = 1): Integer; virtual;
    function GetContentMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect; virtual;
    function GetMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect; virtual;
    function NeedFullUpdate(AIndex: Integer): Boolean; virtual;
    function IsIndexAvailable(AIndex: Integer): Boolean; virtual;
    function IsSolidDrawing(AIndex: Integer): Boolean; virtual;
    function IsStretchTileDrawing(AIndex: Integer): Boolean; virtual;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1); virtual;
    property Count: Integer read GetCount;
  end;

  TscImageCollectionItem = class(TCollectionItem)
  private
    FPicture: TPicture;
    FBitmap: TBitmap;
    FBitmapOptions: TscBitmapOptions;
    FDrawStyle: TscImageDrawStyle;
    FTileOffsetX: Integer;
    FTileOffsetY: Integer;
    FDescription: String;
    FColor: TColor;
    function GetTransparent: Boolean;
    procedure SetPicture(Value: TPicture);
    procedure SetBitmap(Value: TBitmap);
    function GetFullUpdate: Boolean;
    procedure OnBitmapOptionsChange(Sender: TObject);
    procedure SetColor(AValue: TColor);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CheckBitmapOptions;
    procedure LoadFromResourceName(Instance: THandle;
      const ResName: String; AScaleFactor: Double = 1);
    procedure LoadPngFromResourceName(Instance: THandle;
      const ResName: String; AScaleFactor: Double = 1);
    procedure LoadPngFromFile(AFileName: String);
    property NeedFullUpdate: Boolean read GetFullUpdate;
    property Transparent: Boolean read GetTransparent;
    property Color: TColor read FColor write SetColor;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BitmapOptions: TscBitmapOptions
      read FBitmapOptions write FBitmapOptions;
    property Picture: TPicture read FPicture write SetPicture;
    property DrawStyle: TscImageDrawStyle read FDrawStyle write FDrawStyle;
    property TileOffsetX: Integer read FTileOffsetX write FTileOffsetX;
    property TileOffsetY: Integer read FTileOffsetY write FTileOffsetY;
    property Description: String read FDescription write FDescription;
  end;

  TscImageCollectionItems = class(TCollection)
  private
    function GetItem(Index: Integer): TscImageCollectionItem;
    procedure SetItem(Index: Integer; Value: TscImageCollectionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    FImageCollection: TscImageCollection;
    constructor Create(ImageList: TscImageCollection);
    function Add: TscImageCollectionItem;
    property Items[Index: Integer]: TscImageCollectionItem read GetItem write SetItem; default;
  end;

  TscImageCollection = class(TscCustomImageCollection)
  private
    FImages: TscImageCollectionItems;
  protected
    function GetCount: Integer; override;
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
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1); override;
    procedure DrawBitmap(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AAlphaValue: Byte = 255); virtual;
    procedure DrawBitmapRect(ACanvas: TCanvas; ADestRect, ASrcRect: TRect; AIndex: Integer); virtual;
  published
    property Images: TscImageCollectionItems read FImages write FImages;
  end;

implementation
    Uses Vcl.Imaging.PngImage, Vcl.Imaging.JPeg, scGPUtils;


constructor TscCustomImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FScaled := False;
end;

function TscCustomImageCollection.IsStretchTileDrawing(AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TscCustomImageCollection.IsSolidDrawing(AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TscCustomImageCollection.GetMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TscCustomImageCollection.GetContentMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

procedure TscCustomImageCollection.Change;
begin
end;

function TscCustomImageCollection.GetWidth(AIndex: Integer; AScaleFactor: Double = 1): Integer;
begin
  Result := 0;
end;

function TscCustomImageCollection.GetHeight(AIndex: Integer; AScaleFactor: Double = 1): Integer;
begin
  Result := 0;
end;

function TscCustomImageCollection.GetCount: Integer;
begin
  Result := 0;
end;

function TscCustomImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := False;
end;

procedure TscCustomImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1);
begin
end;

function TscCustomImageCollection.NeedFullUpdate(AIndex: Integer): Boolean;
begin
  Result := False;
end;

constructor TscImageCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TPicture.Create;
  FBitmap := TBitmap.Create;
  FBitmapOptions := TscBitmapOptions.Create;
  FBitmapOptions.OnChange := OnBitmapOptionsChange;
  FTileOffsetX := 0;
  FTileOffsetY := 0;
  FDescription := '';
  FColor := clFuchsia;
end;

destructor TscImageCollectionItem.Destroy;
begin
  FPicture.Free;
  FBitmap.Free;
  FBitmapOptions.Free;
  inherited;
end;

procedure TscImageCollectionItem.SetColor(AValue: TColor);
begin
  if ColorToRGB(FColor) <> ColorToRGB(AValue) then
  begin
    FColor := AValue;
    FBitmap.AlphaFormat := afIgnored;
    if not FBitmap.Empty and (FBitmap.PixelFormat = pf32bit) and
       (FColor <> clFuchsia) then
      Bitmap_FillColor(FBitmap, ColorToRGB(FColor));
    FBitmap.AlphaFormat := afPremultiplied;
  end;
end;

procedure TscImageCollectionItem.LoadPngFromFile(AFileName: String);
var
  Picture: TPngImage;
begin
  FColor := clFuchsia;
  Picture := TPngImage.Create;
  Picture.LoadFromFile(AFileName);
  if Picture.TransparencyMode = ptmPartial then
  begin
    FBitmap.AlphaFormat := afIgnored;
    FBitmap.Assign(Picture);
    FBitmap.AlphaFormat := afPremultiplied;
  end
  else
  begin
    FBitmap.PixelFormat := pf24bit;
    FBitmap.Width := Picture.Width;
    FBitmap.Height := Picture.Height;
    FBitmap.Canvas.StretchDraw(
      Rect(0, 0, FBitmap.Width, FBitmap.Height), Picture);
  end;
  Picture.Free;
end;


procedure TscImageCollectionItem.LoadPngFromResourceName(Instance: THandle;
      const ResName: String; AScaleFactor: Double = 1);
var
  Picture: TPngImage;
  Buffer: TBitmap;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  FColor := clFuchsia;
  Picture := TPngImage.Create;
  Picture.LoadFromResourceName(Instance, ResName);
  if Picture.TransparencyMode = ptmPartial then
  begin
    FBitmap.AlphaFormat := afIgnored;
    if AScaleFactor = 1 then
      FBitmap.Assign(Picture)
    else
    begin
      Buffer := TBitmap.Create;
      Buffer.Assign(Picture);
      FBitmap.PixelFormat := pf32bit;
      FBitmap.Width := Round(Buffer.Width * AScaleFactor);
      FBitmap.Height := Round(Buffer.Height * AScaleFactor);
      try
        Bitmap_ClearAlpha(FBitmap, 0);
        Bitmap_AlphaScale(Buffer, FBitmap);
      finally
        Buffer.Free;
      end;
    end;
    FBitmap.AlphaFormat := afPremultiplied;
  end
  else
  begin
    FBitmap.PixelFormat := pf24bit;
    FBitmap.Width := Round(Picture.Width * AScaleFactor);
    FBitmap.Height := Round(Picture.Height * AScaleFactor);
    FBitmap.Canvas.StretchDraw(
      Rect(0, 0, FBitmap.Width, FBitmap.Height), Picture);
  end;
  Picture.Free;
end;

procedure TscImageCollectionItem.LoadFromResourceName(Instance: THandle;
  const ResName: String; AScaleFactor: Double = 1);
var
  Buffer: TBitmap;
begin
  if AScaleFactor < 1 then AScaleFactor := 1;
  FColor := clFuchsia;
  if AScaleFactor = 1 then
  begin
    FBitmap.AlphaFormat := afIgnored;
    FBitmap.LoadFromResourceName(Instance, ResName);
    FBitmap.AlphaFormat := afPremultiplied;
  end
  else
  begin
    Buffer := TBitmap.Create;
    Buffer.LoadFromResourceName(Instance, ResName);
    FBitmap.Width := Round(Buffer.Width * AScaleFactor);
    FBitmap.Height := Round(Buffer.Height * AScaleFactor);
    if Buffer.PixelFormat = pf32bit then
    begin
      FBitmap.PixelFormat := pf32bit;
      FBitmap.AlphaFormat := afIgnored;
      Bitmap_ClearAlpha(FBitmap, 0);
      Bitmap_AlphaScale(Buffer, FBitmap);
      FBitmap.AlphaFormat := afPremultiplied;
    end
    else
    begin
      FBitmap.Canvas.StretchDraw(
        Rect(0, 0, FBitmap.Width, FBitmap.Height), Buffer);
    end;
    Buffer.Free;
  end;
end;

procedure TscImageCollectionItem.CheckBitmapOptions;
begin
  Bitmap_CheckOptions(FBitmap, FBitmapOptions);
end;

procedure TscImageCollectionItem.OnBitmapOptionsChange(Sender: TObject);
begin
  CheckBitmapOptions;
end;

procedure TscImageCollectionItem.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

function TscImageCollectionItem.GetFullUpdate: Boolean;
begin
  Result := FDrawStyle in
    [idsStretch, idsCenter,
     idsTopRight, idsBottomLeft, idsBottomRight,
     idsRightTile, idsBottomTile,
     idsTopStretch, idsLeftStretch, idsRightStretch, idsBottomStretch,
     idsHorzStretchTile, idsVertStretchTile, idsHorzCenterStretch, idsVertCenterStretch,
     idsLeftCenter, idsTopCenter, idsRightCenter, idsBottomCenter];
  if not Result and (FDrawStyle = idsTile) and
     (BitmapOptions.LeftMargin > 0) and (BitmapOptions.TopMargin > 0) and
     (BitmapOptions.RightMargin > 0) and (BitmapOptions.BottomMargin > 0)
   then
     Result := True;
end;


procedure TscImageCollectionItem.Assign(Source: TPersistent);
begin
   if (Source is TscImageCollectionItem) then
     FPicture.Assign(TscImageCollectionItem(Source).FPicture);
  inherited Assign(Source);
end;

function TscImageCollectionItem.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
end;

function TscImageCollectionItem.GetTransparent: Boolean;
begin
  Result := False;
  if not FBitmap.Empty and (FBitmap.PixelFormat = pf32bit) then
    Result := True
  else
  if (FPicture.Graphic <> nil) and not FPicture.Graphic.Empty and
     (FPicture.Graphic is TPngImage) and (TPngImage(FPicture.Graphic).Transparent)
  then
    Result := True;
end;

procedure TscImageCollectionItem.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

constructor TscImageCollectionItems.Create(ImageList: TscImageCollection);
begin
  inherited Create(TscImageCollectionItem);
  FImageCollection := ImageList;
end;

function TscImageCollectionItems.GetItem(Index: Integer): TscImageCollectionItem;
begin
  Result := TscImageCollectionItem(inherited GetItem(Index));
end;

procedure TscImageCollectionItems.SetItem(Index: Integer; Value: TscImageCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TscImageCollectionItems.GetOwner: TPersistent;
begin
  Result := FImageCollection;
end;

procedure TscImageCollectionItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

function TscImageCollectionItems.Add: TscImageCollectionItem;
begin
  Result := TscImageCollectionItem(inherited Add);
end;

constructor TscImageCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImages := TscImageCollectionItems.Create(Self);
end;

destructor TscImageCollection.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

function TscImageCollection.GetMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not IsIndexAvailable(AIndex) then Exit;
  if not FScaled then AScaleFactor := 1;
  Result.Left := Round(FImages[AIndex].BitmapOptions.LeftMargin * AScaleFactor);
  Result.Top := Round(FImages[AIndex].BitmapOptions.TopMargin * AScaleFactor);
  Result.Right := Round(FImages[AIndex].BitmapOptions.RightMargin * AScaleFactor);
  Result.Bottom := Round(FImages[AIndex].BitmapOptions.BottomMargin * AScaleFactor);
end;

function TscImageCollection.GetContentMargins(AIndex: Integer; AScaleFactor: Double = 1): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not IsIndexAvailable(AIndex) then Exit;
  if not FScaled then AScaleFactor := 1;
  Result.Left := Round(FImages[AIndex].BitmapOptions.ContentLeftMargin * AScaleFactor);
  Result.Top := Round(FImages[AIndex].BitmapOptions.ContentTopMargin * AScaleFactor);
  Result.Right := Round(FImages[AIndex].BitmapOptions.ContentRightMargin * AScaleFactor);
  Result.Bottom := Round(FImages[AIndex].BitmapOptions.ContentBottomMargin * AScaleFactor);
end;

function TscImageCollection.GetCount: Integer;
begin
  Result := FImages.Count;
end;

function TscImageCollection.IsStretchTileDrawing(AIndex: Integer): Boolean;
begin
  Result := IsIndexAvailable(AIndex) and
     (FImages[AIndex].DrawStyle in [idsTile, idsStretch]);
end;

function TscImageCollection.IsSolidDrawing(AIndex: Integer): Boolean;
begin
  Result := IsIndexAvailable(AIndex) and not FImages[AIndex].Transparent and
     (FImages[AIndex].DrawStyle in [idsTile, idsStretch]);
end;

function TscImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FImages.Count) and
   (not FImages[AIndex].FBitmap.Empty or
   ((FImages[AIndex].Picture.Graphic <> nil) and not FImages[AIndex].Picture.Graphic.Empty));
end;

function TscImageCollection.NeedFullUpdate(AIndex: Integer): Boolean;
begin
  Result := IsIndexAvailable(AIndex) and FImages.Items[AIndex].NeedFullUpdate;
end;

procedure TscImageCollection.DrawBitmapRect(ACanvas: TCanvas;
  ADestRect, ASrcRect: TRect; AIndex: Integer);
begin
  Bitmap_DrawAlpha(FImages[AIndex].Bitmap, ACanvas, ASrcRect, ADestRect, 255);
end;

function TscImageCollection.GetWidth(AIndex: Integer; AScaleFactor: Double = 1): Integer;
begin
  Result := 0;
  if not FScaled then
    AScaleFactor := 1;
  if IsIndexAvailable(AIndex) then
  begin
    if not FImages[AIndex].Bitmap.Empty then
      Result := Round(FImages[AIndex].Bitmap.Width * AScaleFactor)
    else
    if FImages[AIndex].Picture.Graphic <> nil then
      Result := Round(FImages[AIndex].Picture.Width * AScaleFactor)
  end;
end;

function TscImageCollection.GetHeight(AIndex: Integer; AScaleFactor: Double = 1): Integer;
begin
  Result := 0;
  if not FScaled then
    AScaleFactor := 1;
  if IsIndexAvailable(AIndex) then
  begin
    if not FImages[AIndex].Bitmap.Empty then
      Result := Round(FImages[AIndex].Bitmap.Height * AScaleFactor)
    else
    if FImages[AIndex].Picture.Graphic <> nil then
      Result := Round(FImages[AIndex].Picture.Height * AScaleFactor)
  end;
end;

procedure TscImageCollection.DrawBitmap(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AAlphaValue: Byte = 255);
begin
  if not FImages[AIndex].Bitmap.Empty then
    Bitmap_DrawWithOptions(FImages[AIndex].Bitmap, FImages[AIndex].BitmapOptions, ACanvas, ARect, AAlphaValue);
end;

procedure TscImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AScaleFactor: Double = 1);
var
  SaveIndex, X, Y, XCnt, YCnt: Integer;
  P: TPicture;
  B: TBitmap;
  R, SrcRect: TRect;
begin
  if not IsIndexAvailable(AIndex) then Exit;

  if not FImages[AIndex].Bitmap.Empty then
  begin
    B := FImages[AIndex].Bitmap;
    SrcRect := Rect(0, 0, B.Width, B.Height);
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FImages[AIndex].BitmapOptions.DrawInClipRect then
        IntersectClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      if (FImages[AIndex].BitmapOptions.LeftMargin > 0) and
         (FImages[AIndex].BitmapOptions.RightMargin > 0) and
         (FImages[AIndex].BitmapOptions.TopMargin > 0) and
         (FImages[AIndex].BitmapOptions.RightMargin > 0)
      then
      begin
        Bitmap_DrawWithOptions (B, FImages[AIndex].BitmapOptions, ACanvas, ARect);
      end
      else
      if (FImages[AIndex].BitmapOptions.LeftMargin > 0) and
         (FImages[AIndex].BitmapOptions.RightMargin > 0)
      then
      begin
        R := ARect;
        case FImages[AIndex].DrawStyle of
          idsTopLeft, idsTopRight, idsTopStretch, idsTopTile:
            R.Bottom := R.Top + B.Height;
          idsBottomLeft, idsBottomRight, idsBottomStretch, idsBottomTile:
            R.Top := R.Bottom - B.Height;
        end;
        if FImages[AIndex].DrawStyle  = idsVertStretchTile then
        begin
          YCnt := ARect.Height div (B.Height + FImages[AIndex].TileOffsetY);
          for Y := 0 to YCnt do
          begin
            R := Rect(ARect.Left,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), ARect.Right,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY)  + B.Height);
            Bitmap_DrawWithOptions(B, FImages[AIndex].BitmapOptions, ACanvas, R);
          end;
        end
        else
          Bitmap_DrawWithOptions(B, FImages[AIndex].BitmapOptions, ACanvas, R);
      end
      else
      if (FImages[AIndex].BitmapOptions.TopMargin > 0) and
         (FImages[AIndex].BitmapOptions.BottomMargin > 0)
      then
      begin
        R := ARect;
        case FImages[AIndex].DrawStyle of
          idsTopLeft, idsBottomLeft, idsLeftStretch, idsLeftTile:
            R.Right := R.Left + B.Width;
          idsTopRight, idsBottomRight, idsRightStretch, idsRightTile:
            R.Left := R.Right - B.Width;
        end;
        if FImages[AIndex].DrawStyle  = idsHorzStretchTile then
        begin
          XCnt := ARect.Width div (B.Width + FImages[AIndex].TileOffsetX);
          for X := 0 to XCnt do
          begin
            R := Rect(ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                 ARect.Top,
                 ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX) + B.Width,
                 ARect.Bottom);
            Bitmap_DrawWithOptions(B, FImages[AIndex].BitmapOptions, ACanvas, R);
          end;
        end
        else
          Bitmap_DrawWithOptions(B, FImages[AIndex].BitmapOptions, ACanvas, R);
      end
      else
      case FImages[AIndex].DrawStyle of
        idsTile:
        begin
          XCnt := ARect.Width div (B.Width + FImages[AIndex].TileOffsetX);
          YCnt := ARect.Height div (B.Height + FImages[AIndex].TileOffsetY);
          for X := 0 to XCnt do
             for Y := 0 to YCnt do
             begin
               if B.PixelFormat = pf32Bit then
                 Bitmap_DrawAlpha_XY(B, ACanvas,
                   ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                   ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), 255)
               else
                 ACanvas.Draw(ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                   ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), B);
             end;
        end;
        idsStretch:
        begin
          if B.PixelFormat = pf32Bit then
            Bitmap_DrawAlpha(B, ACanvas, SrcRect, ARect, 255)
          else
            ACanvas.StretchDraw(ARect, B);
        end;
        idsCenter:
        begin
          X := ARect.Left + ARect.Width div 2 - B.Width div 2;
          Y := ARect.Top + ARect.Height div 2 - B.Height div 2;
          if B.PixelFormat = pf32Bit then
            Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
          else
            ACanvas.Draw(X, Y, B);
        end;
        idsTopCenter:
        begin
          X := ARect.Left + ARect.Width div 2 - B.Width div 2;
          Y := ARect.Top;
          if B.PixelFormat = pf32Bit then
            Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
          else
            ACanvas.Draw(X, Y, B);
        end;
        idsBottomCenter:
         begin
           X := ARect.Left + ARect.Width div 2 - B.Width div 2;
           Y := ARect.Bottom - B.Height;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
           else
             ACanvas.Draw(X, Y, B);
         end;
         idsLeftCenter:
         begin
           X := ARect.Left;
           Y := ARect.Top + ARect.Height div 2 - B.Height div 2;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
           else
             ACanvas.Draw(X, Y, B);
         end;
         idsRightCenter:
         begin
           X := ARect.Right - B.Width;
           Y := ARect.Top + ARect.Height div 2 - B.Height div 2;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
           else
             ACanvas.Draw(X, Y, B);
         end;
         idsTopLeft:
         begin
           X := ARect.Left;
           Y := ARect.Top;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
           else
             ACanvas.Draw(X, Y, B);
         end;
         idsBottomLeft:
         begin
           X := ARect.Left;
           Y := ARect.Bottom - B.Height;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
           else
             ACanvas.Draw(X, Y, B);
         end;
         idsTopRight:
         begin
           X := ARect.Right - B.Width;
           Y := ARect.Top;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
           else
             ACanvas.Draw(X, Y, B);
         end;
         idsBottomRight:
         begin
           X := ARect.Right - B.Width;
           Y := ARect.Bottom - B.Height;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha_XY(B, ACanvas, X, Y, 255)
           else
             ACanvas.Draw(X, Y, B);
         end;
        idsTopTile:
         begin
           XCnt := ARect.Width div (B.Width + FImages[AIndex].TileOffsetX);
           for X := 0 to XCnt do
             if B.PixelFormat = pf32Bit then
               Bitmap_DrawAlpha_XY(B, ACanvas, ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX), ARect.Top, 255)
             else
               ACanvas.Draw(ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                 ARect.Top, B);
         end;
         idsLeftTile:
         begin
           YCnt := ARect.Height div (B.Height + FImages[AIndex].TileOffsetY);
           for Y := 0 to YCnt do
             if B.PixelFormat = pf32Bit then
               Bitmap_DrawAlpha_XY(B, ACanvas, ARect.Left,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), 255)
             else
               ACanvas.Draw(ARect.Left,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), B);
         end;
         idsRightTile:
         begin
           YCnt := ARect.Height div (B.Height + FImages[AIndex].TileOffsetY);
           for Y := 0 to YCnt do
             if B.PixelFormat = pf32Bit then
               Bitmap_DrawAlpha_XY(B, ACanvas, ARect.Right - B.Width,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), 255)
             else
               ACanvas.Draw(ARect.Right - B.Width,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), B);
         end;
         idsBottomTile:
         begin
           XCnt := ARect.Width div (B.Width + FImages[AIndex].TileOffsetX);
           for X := 0 to XCnt do
             if B.PixelFormat = pf32Bit then
              Bitmap_DrawAlpha_XY(B, ACanvas,
                 ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                 ARect.Bottom - B.Height, 255)
             else
               ACanvas.Draw(ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                 ARect.Bottom - B.Height, B);
         end;
         idsTopStretch:
         begin
           R := ARect;
           R.Bottom := R.Top + B.Height;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha(B, ACanvas, SrcRect, R, 255)
           else
             ACanvas.StretchDraw(R, B);
         end;
         idsLeftStretch:
         begin
           R := ARect;
           R.Right := R.Left + B.Width;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha(B, ACanvas, SrcRect, R, 255)
           else
             ACanvas.StretchDraw(R, B);
         end;
         idsRightStretch:
         begin
           R := ARect;
           R.Left := R.Right - B.Width;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha(B, ACanvas, SrcRect, R, 255)
           else
             ACanvas.StretchDraw(R, B);
         end;
         idsBottomStretch:
         begin
           R := ARect;
           R.Top := R.Bottom - B.Height;
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha(B, ACanvas, SrcRect, R, 255)
           else
             ACanvas.StretchDraw(R, B);
         end;
         idsHorzStretchTile:
         begin
           XCnt := ARect.Width div (B.Width + FImages[AIndex].TileOffsetX);
           for X := 0 to XCnt do
             if B.PixelFormat = pf32Bit then
               Bitmap_DrawAlpha(B, ACanvas, SrcRect, Rect(ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                 ARect.Top,
                 ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX) + B.Width,
                 ARect.Bottom), 255)
             else
               ACanvas.StretchDraw(Rect(ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX),
                 ARect.Top,
                 ARect.Left + X * (B.Width + FImages[AIndex].TileOffsetX) + B.Width, ARect.Bottom), B);
         end;
         idsVertStretchTile:
         begin
           YCnt := ARect.Height div (B.Height + FImages[AIndex].TileOffsetY);
           for Y := 0 to YCnt do
             if B.PixelFormat = pf32Bit then
               Bitmap_DrawAlpha(B, ACanvas, SrcRect, Rect(ARect.Left,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), ARect.Right,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY)  + B.Height), 255)
             else
               ACanvas.StretchDraw(Rect(ARect.Left,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY), ARect.Right,
                 ARect.Top + Y * (B.Height + FImages[AIndex].TileOffsetY)  + B.Height), B);
         end;
         idsHorzCenterStretch:
         begin
           R := Rect(ARect.Left, ARect.Top + ARect.Height div 2 - B.Height div 2,
                     ARect.Right, ARect.Top + ARect.Height div 2 - B.Height div 2 + B.Height);
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha(B, ACanvas, SrcRect, R, 255)
           else
             ACanvas.StretchDraw(R, B);
         end;
         idsVertCenterStretch:
         begin
           R := Rect(ARect.Left + ARect.Width div 2 - B.Width div 2, ARect.Top,
                     ARect.Left + ARect.Width div 2 - B.Width div 2 + B.Width, ARect.Bottom);
           if B.PixelFormat = pf32Bit then
             Bitmap_DrawAlpha(B, ACanvas, SrcRect, R, 255)
           else
             ACanvas.StretchDraw(R, B);
         end;
       end;
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    Exit;
  end;

  if FImages[AIndex].FPicture.Graphic = nil then Exit;
  if FImages[AIndex].FPicture.Graphic.Empty then Exit;

 SaveIndex := SaveDC(ACanvas.Handle);
 try
   IntersectClipRect(ACanvas.Handle,
     ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
   P := FImages[AIndex].FPicture;
   case FImages[AIndex].DrawStyle of
     idsTile:
       begin
         XCnt := ARect.Width div (P.Width + FImages[AIndex].TileOffsetX);
         YCnt := ARect.Height div (P.Height + FImages[AIndex].TileOffsetY);
         for X := 0 to XCnt do
           for Y := 0 to YCnt do
             ACanvas.Draw(ARect.Left + X * (P.Width + FImages[AIndex].TileOffsetX),
             ARect.Top + Y * (P.Height + FImages[AIndex].TileOffsetY), P.Graphic);
       end;
     idsStretch:
       begin
         ACanvas.StretchDraw(ARect, P.Graphic);
       end;
     idsCenter:
       begin
         X := ARect.Left + ARect.Width div 2 - FImages[AIndex].FPicture.Graphic.Width div 2;
         Y := ARect.Top + ARect.Height div 2 - FImages[AIndex].FPicture.Graphic.Height div 2;
         ACanvas.Draw(X, Y, P.Graphic);
       end;
      idsTopCenter:
       begin
         X := ARect.Left + ARect.Width div 2 - FImages[AIndex].FPicture.Graphic.Width div 2;
         Y := ARect.Top;
         ACanvas.Draw(X, Y, P.Graphic);
       end;
      idsBottomCenter:
       begin
         X := ARect.Left + ARect.Width div 2 - FImages[AIndex].FPicture.Graphic.Width div 2;
         Y := ARect.Bottom - P.Height;
         ACanvas.Draw(X, Y, P.Graphic);
       end;
     idsLeftCenter:
       begin
         X := ARect.Left;
         Y := ARect.Top + ARect.Height div 2 - FImages[AIndex].FPicture.Graphic.Height div 2;
         ACanvas.Draw(X, Y, P.Graphic);
       end;
     idsRightCenter:
       begin
         X := ARect.Right - P.Width;
         Y := ARect.Top + ARect.Height div 2 - FImages[AIndex].FPicture.Graphic.Height div 2;
         ACanvas.Draw(X, Y, P.Graphic);
       end;
     idsTopLeft:
       begin
         ACanvas.Draw(ARect.Left, ARect.Top, P.Graphic);
       end;
     idsBottomLeft:
       begin
         ACanvas.Draw(ARect.Left, ARect.Bottom - P.Height, P.Graphic);
       end;
     idsTopRight:
       begin
         ACanvas.Draw(ARect.Right - P.Width, ARect.Top, P.Graphic);
       end;
     idsBottomRight:
       begin
         ACanvas.Draw(ARect.Right - P.Width, ARect.Bottom - P.Height, P.Graphic);
       end;
     idsTopTile:
       begin
         XCnt := ARect.Width div (P.Width + FImages[AIndex].TileOffsetX);
         for X := 0 to XCnt do
          ACanvas.Draw(ARect.Left + X * (P.Width + FImages[AIndex].TileOffsetX),
             ARect.Top, P.Graphic);
       end;
     idsLeftTile:
       begin
         YCnt := ARect.Height div (P.Height + FImages[AIndex].TileOffsetY);
         for Y := 0 to YCnt do
           ACanvas.Draw(ARect.Left,
             ARect.Top + Y * (P.Height + FImages[AIndex].TileOffsetY), P.Graphic);
       end;
     idsRightTile:
       begin
         YCnt := ARect.Height div (P.Height + FImages[AIndex].TileOffsetY);
         for Y := 0 to YCnt do
           ACanvas.Draw(ARect.Right - P.Width,
             ARect.Top + Y * (P.Height + FImages[AIndex].TileOffsetY), P.Graphic);
       end;
     idsBottomTile:
       begin
         XCnt := ARect.Width div (P.Width + FImages[AIndex].TileOffsetX);
         for X := 0 to XCnt do
          ACanvas.Draw(ARect.Left + X * (P.Width + FImages[AIndex].TileOffsetX),
             ARect.Bottom - P.Height, P.Graphic);
       end;
     idsTopStretch:
       begin
         R := ARect;
         R.Bottom := R.Top + P.Height;
         ACanvas.StretchDraw(R, P.Graphic);
       end;
     idsLeftStretch:
       begin
         R := ARect;
         R.Right := R.Left + P.Width;
         ACanvas.StretchDraw(R, P.Graphic);
       end;
     idsRightStretch:
       begin
         R := ARect;
         R.Left := R.Right - P.Width;
         ACanvas.StretchDraw(R, P.Graphic);
       end;
     idsBottomStretch:
       begin
         R := ARect;
         R.Top := R.Bottom - P.Height;
         ACanvas.StretchDraw(R, P.Graphic);
       end;
     idsHorzStretchTile:
       begin
         XCnt := ARect.Width div (P.Width + FImages[AIndex].TileOffsetX);
         for X := 0 to XCnt do
          ACanvas.StretchDraw(Rect(ARect.Left + X * (P.Width + FImages[AIndex].TileOffsetX),
             ARect.Top, ARect.Left + X * (P.Width + FImages[AIndex].TileOffsetX) + P.Width, ARect.Bottom),
              P.Graphic);
       end;
     idsVertStretchTile:
       begin
         YCnt := ARect.Height div (P.Height + FImages[AIndex].TileOffsetY);
         for Y := 0 to YCnt do
           ACanvas.StretchDraw(Rect(ARect.Left,
             ARect.Top + Y * (P.Height + FImages[AIndex].TileOffsetY), ARect.Right,
              ARect.Top + Y * (P.Height + FImages[AIndex].TileOffsetY)  + P.Height), P.Graphic);
       end;
     idsHorzCenterStretch:
        begin
          R := Rect(ARect.Left, ARect.Top + ARect.Height div 2 - P.Height div 2,
                    ARect.Right, ARect.Top + ARect.Height div 2 - P.Height div 2 + P.Height);
          ACanvas.StretchDraw(R, P.Graphic);
        end;
     idsVertCenterStretch:
       begin
         R := Rect(ARect.Left + ARect.Width div 2 - P.Width div 2, ARect.Top,
                   ARect.Left + ARect.Width div 2 - P.Width div 2 + P.Width, ARect.Bottom);
         ACanvas.StretchDraw(R, P.Graphic);
       end;
     idsHorzCenterTile:
       begin
         XCnt := ARect.Width div (P.Width + FImages[AIndex].TileOffsetX);
         Y := ARect.Top + ARect.Height div 2 - FImages[AIndex].FPicture.Graphic.Height div 2;
         for X := 0 to XCnt do
          ACanvas.Draw(ARect.Left + X * (P.Width + FImages[AIndex].TileOffsetX),
             Y, P.Graphic);
       end;
     idsVertCenterTile:
       begin
         YCnt := ARect.Height div (P.Height + FImages[AIndex].TileOffsetY);
         X := ARect.Left + ARect.Width div 2 - FImages[AIndex].FPicture.Graphic.Width div 2;
         for Y := 0 to YCnt do
           ACanvas.Draw(X,
             ARect.Top + Y * (P.Height + FImages[AIndex].TileOffsetY), P.Graphic);
       end;
   end;
 finally
   RestoreDC(ACanvas.Handle, SaveIndex);
 end;
end;

end.
