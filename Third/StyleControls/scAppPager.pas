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

{$I scdefine.inc}

unit scAppPager;

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
    Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Graphics, Vcl.Themes, Vcl.ImgList,
    scDrawUtils, scImageCollection, scControls, scExtControls, Vcl.Buttons;

  type

  TscAppPager = class;
  TscAppPagerItem = class;
  TscAppPagerPage = class;

  TscAppPagerItem = class(TCollectionItem)
  protected
    FOnClick: TNotifyEvent;
    FPage: TscAppPagerPage;
    FVisible: Boolean;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FCaption: String;
    FDivider: Boolean;
    procedure SetPage(const Value: TscAppPagerPage);
    procedure SetCaption(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetDivider(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    Active: Boolean;
    ItemRect: TRect;
    IsVisible: Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Page: TscAppPagerPage read FPage write SetPage;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: String read FCaption write SetCaption;
    property Divider: Boolean read FDivider write SetDivider;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscAppPagerItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscAppPagerItem;
    procedure SetItem(Index: Integer; Value:  TscAppPagerItem);
  protected
    AppPager: TscAppPager;
    DestroyPage: TscAppPagerPage;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AAppPager: TscAppPager);
    property Items[Index: Integer]: TscAppPagerItem read GetItem write SetItem; default;
  end;

  TscAppPagerPage = class(TscScrollBox)
  private
    FFirstActiveControl: TWinControl;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    AppPager: TscAppPager;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property FirstActiveControl: TWinControl
      read FFirstActiveControl write FFirstActiveControl;
  end;

  TscAppPagerSelectionStyle = (scapsColor, scapsStyled, scapsModern, scapsCustomImage,
    scapsCustomImageOverColor);

  TscAppPagerCanChangePageEvent = procedure(APage: TscAppPagerPage; var ACanChange: Boolean) of object;

  TscAppPager = class(TscCustomControl)
  private
    FMouseWheelSupport: Boolean;
    FChangePageWithItemSelection: Boolean;
    FSelectionStyle: TscAppPagerSelectionStyle;
    FCaptionFont: TFont;
    FScrollOffset: Integer;
    ScrollBar: TscScrollBar;
    FOldHeight: Integer;
    FMouseIn: Boolean;
    FItemIndex: Integer;
    FItems: TscAppPagerItems;
    FLeftOffset, FRightOffset: Integer;
    FOldItemActive, FItemActive: Integer;
    FActivePage: TscAppPagerPage;
    FItemWidth: Integer;
    FItemHeight: Integer;
    FItemImages: TCustomImageList;

    FOnChangePage: TNotifyEvent;
    FOnChangingPage: TNotifyEvent;
    FOnCanChangePage: TscAppPagerCanChangePageEvent;

    FItemSpacing,
    FItemMargin: Integer;

    FItemBackgroundColor: TColor;
    FItemHotColor: TColor;
    FItemSelectedColor: TColor;
    FItemSelectedFocusedColor: TColor;

    FItemFontHotColor: TColor;
    FItemFontSelectedColor: TColor;
    FItemFontSelectedFocusedColor: TColor;

    FItemHotImageIndex: Integer;
    FItemSelectedImageIndex: Integer;
    FItemSelectedFocusedImageIndex: Integer;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageIndex: Integer;
    FCustomOverContentImageIndex: Integer;
    FShowCaption: Boolean;
    FCaptionHeight: Integer;
    FItemGlowEffect: TscButtonGlowEffect;
    FCaptionGlowEffect: TscGlowEffect;
    FCaptionImages: TCustomImageList;
    FCaptionImageIndex: Integer;
    FCaptionLayout: TButtonLayout;
    FOnCaptionClick: TNotifyEvent;
    FCaptionRect: TRect;
    FCaptionDown: Boolean;
    FTouchBegin, FTouchEnd: Integer;
    FCaptionCursor, FSaveCursor: TCursor;
    FCaptionCursorEnabled: Boolean;
    FDividerColor: TColor;
    FItemsTopOffset: Integer;

    procedure SetItemsTopOffset(Value: Integer);
    procedure SetDividerColor(Value: TColor);
    procedure SetCaptionlayout(Value: TButtonLayout);
    procedure SetCaptionImages(Value: TCustomImageList);
    procedure SetCaptionImageIndex(Value: Integer);
    procedure SetCaptionHeight(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetItemMargin(Value: Integer);
    procedure SetCaptionFont(Value: TFont);
    procedure SetShowCaption(Value: Boolean);

    procedure SetItemFontHotColor(Value: TColor);
    procedure SetItemFontSelectedColor(Value: TColor);
    procedure SetItemFontSelectedFocusedColor(Value: TColor);

    procedure SetItemHotColor(Value: TColor);
    procedure SetItemSelectedColor(Value: TColor);
    procedure SetItemSelectedFocusedColor(Value: TColor);
    procedure SetItemHotImageIndex(Value: Integer);
    procedure SetItemSelectedImageIndex(Value: Integer);
    procedure SetItemSelectedFocusedImageIndex(Value: Integer);

    procedure SetSelectionStyle(Value: TscAppPagerSelectionStyle);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomOverContentImageIndex(Value: Integer);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetItemBackgroundColor(Value: TColor);
    procedure SetItems(AValue: TscAppPagerItems);
    procedure SetActivePage(const Value: TscAppPagerPage);
    function GetPageIndex(Value: TscAppPagerPage): Integer;
    function GetPageBoundsRect: TRect;
    function ItemFromPoint(P: TPoint): Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetItemWidth(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemImages(Value: TCustomImageList);
    procedure OnControlChange(Sender: TObject);
  protected
    FStopCheckItemIndex: Boolean;
    FChangeFromKeyboard: Boolean;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CMGesture(var Message: TCMGesture); message CM_GESTURE;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL); message WM_MOUSEWHEEL;
    procedure TestActive(X, Y: Integer);
    procedure Loaded; override;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure CalcItemRects;
    procedure DrawItem(ACanvas: TCanvas; Index: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure GetScrollInfo(var AMin, AMax, APage: Integer);

    procedure ShowScrollbar;
    procedure HideScrollBar;
    procedure AdjustScrollBar;
    procedure SBChange(Sender: TObject);
    procedure Scroll(AScrollOffset: Integer);
    procedure ScrollToItem(Index: Integer);
    function GetTopOffset: Integer;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure InitTouch;
  public
    procedure Paint; override;

    procedure FindNextItem;
    procedure FindPriorItem;
    procedure FindFirstItem;
    procedure FindLastItem;
    procedure UpdateScrollInfo;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreatePage;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  published
    property Align;
    property Font;
    property Caption;
    property Color;
    property CaptionCursor: TCursor
     read FCaptionCursor write FCaptionCursor;
    property CaptionCursorEnabled: Boolean
      read FCaptionCursorEnabled write FCaptionCursorEnabled;
    property CaptionHeight: Integer
      read FCaptionHeight write SetCaptionHeight;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionGlowEffect: TscGlowEffect
      read FCaptionGlowEffect write FCaptionGlowEffect;
    property ChangePageWithItemSelection: Boolean
      read FChangePageWithItemSelection write FChangePageWithItemSelection;

    property DividerColor: TColor
      read FDividerColor write SetDividerColor;
    property ItemsTopOffset: Integer read
      FItemsTopOffset write SetItemsTopOffset;

    property ShowCaption: Boolean
      read FShowCaption write SetShowCaption;
    property CaptionImages: TCustomImageList
      read FCaptionImages write SetCaptionImages;
    property CaptionImageIndex: Integer
      read FCaptionImageIndex write SetCaptionImageIndex;
    property CaptionLayout: TButtonLayout
      read FCaptionLayout write SetCaptionlayout;
    property ItemGlowEffect: TscButtonGlowEffect read FItemGlowEffect write FItemGlowEffect;
    property SelectionStyle: TscAppPagerSelectionStyle
      read FSelectionStyle write SetSelectionStyle;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageIndex: Integer
      read FCustomBackgroundImageIndex write SetCustomBackgroundImageIndex;

   property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;

    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing;
    property ItemMargin: Integer read FItemMargin write SetItemMargin;

    property ItemBackgroundColor: TColor
      read FItemBackgroundColor write SetItemBackgroundColor;

    property ItemFontHotColor: TColor
      read FItemFontHotColor write SetItemFontHotColor;
    property ItemFontSelectedColor: TColor
      read FItemFontSelectedColor write SetItemFontSelectedColor;
    property ItemFontSelectedFocusedColor: TColor
      read FItemFontSelectedFocusedColor write SetItemFontSelectedFocusedColor;

    property ItemHotColor: TColor
      read FItemHotColor write SetItemHotColor;
    property ItemSelectedColor: TColor
      read FItemSelectedColor write SetItemSelectedColor;
    property ItemSelectedFocusedColor: TColor
      read FItemSelectedFocusedColor write SetItemSelectedFocusedColor;

    property ItemHotImageIndex: Integer
      read FItemHotImageIndex write SetItemHotImageIndex;
    property ItemSelectedImageIndex: Integer
      read FItemSelectedImageIndex write SetItemSelectedImageIndex;
    property ItemSelectedFocusedImageIndex: Integer
      read FItemSelectedFocusedImageIndex write SetItemSelectedFocusedImageIndex;

    property CustomOverContentImageIndex: Integer
      read FCustomOverContentImageIndex write SetCustomOverContentImageIndex;
    property ItemWidth: Integer read FItemWidth write SetItemWidth;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property Items: TscAppPagerItems read FItems write SetItems;
    property ActivePage: TscAppPagerPage read FActivePage write SetActivePage;
    property ItemImages: TCustomImageList
      read FItemImages write SetItemImages;

    property OnChangingPage: TNotifyEvent read FOnChangingPage write FOnChangingPage;
    property OnChangePage: TNotifyEvent read FOnChangePage write FOnChangePage;
    property OnCanChangePage: TscAppPagerCanChangePageEvent
      read FOnCanChangePage write FOnCanChangePage;

    property OnCaptionClick: TNotifyEvent read FOnCaptionClick write FOnCaptionClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

implementation
   Uses System.UITypes;

const
  DefAppPagerItemWidth = 200;
  DefAppPagerItemHeight = 50;
  DefDividerItemHeight = 20;

constructor TscAppPagerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Active := False;
  FDivider := False;
  FEnabled := True;
  FVisible := True;
  FPage := nil;
  FCaption := 'TscAppPagerItem' + IntToStr(Index);
  FImageIndex := -1;
end;

destructor TscAppPagerItem.Destroy;
begin
  inherited;
end;

procedure TscAppPagerItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TscAppPagerItem
  then
    begin
      FPage := TscAppPagerItem(Source).Page;
      FCaption := TscAppPagerItem(Source).Caption;
      FImageIndex := TscAppPagerItem(Source).ImageIndex;
      FVisible := TscAppPagerItem(Source).Visible;
      FEnabled := TscAppPagerItem(Source).Enabled;
    end
end;

procedure TscAppPagerItem.SetDivider(Value: Boolean);
begin
  if FDivider <> Value then
  begin
    FDivider := Value;
    Changed(False);
  end;
end;

procedure TscAppPagerItem.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscAppPagerItem.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value
  then
    begin
      FEnabled := Value;
      Changed(False);
    end;
end;


procedure TscAppPagerItem.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value
  then
    begin
      FImageIndex := Value;
      Changed(False);
    end;
end;


procedure TscAppPagerItem.SetVisible(Value: Boolean);
begin
  if FVisible <> Value
  then
    begin
      FVisible := Value;
      Changed(False);
    end;
end;


procedure TscAppPagerItem.SetPage(const Value: TscAppPagerPage);
begin
  if FPage <> Value then
  begin
    FPage := Value;
    if (FPage <> nil) and (FPage.AppPager <> nil) then
      FPage.AppPager.ActivePage := FPage;
  end;
end;

constructor TscAppPagerItems.Create;
begin
  inherited Create(TscAppPagerItem);
  AppPager := AAppPager;
  DestroyPage := nil;
end;

function TscAppPagerItems.GetOwner: TPersistent;
begin
  Result := AppPager;
end;

procedure TscAppPagerItems.Update(Item: TCollectionItem);
begin
  inherited;
  if (csDesigning in AppPager.ComponentState) or AppPager.Visible
  then
    AppPager.UpdateScrollInfo;
  AppPager.RePaintControl;
end;

function TscAppPagerItems.GetItem(Index: Integer):  TscAppPagerItem;
begin
  Result := TscAppPagerItem(inherited GetItem(Index));
end;

procedure TscAppPagerItems.SetItem(Index: Integer; Value:  TscAppPagerItem);
begin
  inherited SetItem(Index, Value);
end;

constructor TscAppPagerPage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  BorderStyle := bsNone;
  ParentFont := False;
  ParentColor := False;
end;

destructor TscAppPagerPage.Destroy;
begin
  inherited;
end;

procedure TscAppPagerPage.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FFirstActiveControl)
  then
    FFirstActiveControl := nil;
end;

procedure TscAppPagerPage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if AppPager = nil then
    if (Parent <> nil) and (Parent is TscAppPager) then
      AppPager := TscAppPager(Parent);
  if (Parent <> nil) and (AppPager <> nil)
  then
    begin
      R := AppPager.GetPageBoundsRect;
      inherited SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    end
  else
    inherited;
end;

constructor TscAppPager.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FItems := TscAppPagerItems.Create(Self);
  FCaptionCursor := crDefault;
  FSaveCursor := crDefault;
  FCaptionCursorEnabled := False;

  FDividerColor := clNone;

  FMouseWheelSupport := False;
  FChangePageWithItemSelection := True;
  FChangeFromKeyboard := False;

  FCaptionFont := TFont.Create;
  FCaptionFont.Assign(Font);
  FCaptionFont.Size := 14;
  FCaptionFont.OnChange := OnControlChange;

  FItemGlowEffect := TscButtonGlowEffect.Create;
  FItemGlowEffect.OnChange := OnControlChange;
  FCaptionGlowEffect := TscGlowEffect.Create;
  FCaptionGlowEffect.OnChange := OnControlChange;

  FItemMargin := 10;
  FItemSpacing := 10;
  FCaptionHeight := 50;
  FItemsTopOffset := 10;
  FCaptionImageIndex := -1;
  FCaptionLayout := blGlyphTop;

  FItemWidth := DefAppPagerItemWidth;
  FItemHeight := DefAppPagerItemHeight;
  FShowCaption := False;
  FWallpaperIndex := -1;
  FWallPapers := nil;
  FCustomImages := nil;
  FCaptionImages := nil;
  FItemImages := nil;
  FCustomBackgroundImageIndex := -1;
  FCustomOverContentImageIndex := -1;
  FTransparentBackground := False;
  FStopCheckItemIndex := False;

  FItemBackgroundColor := clWindow;
  FItemSelectedColor := clHighLight;
  FItemSelectedFocusedColor := clHighLight;
  FItemHotColor := clBtnFace;

  FItemFontSelectedColor := clHighLightText;
  FItemFontSelectedFocusedColor := clHighLightText;
  FItemFontHotColor := clBtnText;

  FItemHotImageIndex := -1;
  FItemSelectedImageIndex := -1;
  FItemSelectedFocusedImageIndex := -1;
  FMouseIn := False;
  ScrollBar := nil;
  FScrollOffset := 0;
  FItemIndex := -1;
  FLeftOffset := 6;
  FRightOffset := 5;
  Width := 200;
  Height := 200;
  FOldItemActive := -1;
  FItemActive := -1;
  FOldHeight := -1;
  FTouchBegin := 0;
  FTouchEnd := 0;
  InitTouch;
end;

destructor TscAppPager.Destroy;
begin
  FCaptionGlowEffect.Free;
  FItemGlowEffect.Free;
  FCaptionFont.Free;
  FItems.Free;
  FItems := nil;
  if ScrollBar <> nil then
  begin
    ScrollBar.Free;
    ScrollBar := nil;
  end;
  inherited;
end;

procedure TscAppPager.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FItemsTopOffset := MulDiv(FItemsTopOffset, M, D);
  FItemHeight := MulDiv(FItemHeight, M, D);
  FItemSpacing := MulDiv(FItemSpacing, M, D);
  FItemMargin := MulDiv(FItemMargin, M, D);
  FCaptionHeight := MulDiv(FCaptionHeight, M, D);
  FCaptionFont.Height := MulDiv(FCaptionFont.Height, M, D);
  ItemWidth := MulDiv(ItemWidth, M, D);
end;

procedure TscAppPager.InitTouch;
begin
  Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions + [igoPanSingleFingerVertical, igoPanInertia];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [igPan, igPressAndTap];
end;

procedure TscAppPager.CMGesture(var Message: TCMGesture);
var
  Offset: Integer;
begin
  inherited;
  if (ScrollBar <> nil) and ScrollBar.Visible then
  begin
    if gfBegin in Message.Info^.Flags
    then
      FTouchBegin := Message.Info^.Location.Y
    else
    begin
      FTouchEnd := Message.Info^.Location.Y;
      Offset := FTouchEnd - FTouchBegin;
      FTouchBegin := FTouchEnd;
      ScrollBar.Position := ScrollBar.Position - Offset;
    end;
  end;
end;

function TscAppPager.GetTopOffset: Integer;
begin
  if FShowCaption then
    Result := FCaptionHeight +  FItemsTopOffset
  else
    Result := FItemsTopOffset;
end;

procedure TscAppPager.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscAppPager.SetDividerColor;
begin
  if FDividerColor <> Value then
  begin
    FDividerColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetCaptionHeight(Value: Integer);
begin
  if (FCaptionHeight <> Value) and (Value > 20) then
  begin
    FCaptionHeight := Value;
    if FShowCaption then
      RePaintControl;
  end;
end;

procedure TscAppPager.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  RePaintControl;
end;

procedure TscAppPager.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetSelectionStyle(Value: TscAppPagerSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    Invalidate;
  end;
end;

procedure TscAppPager.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetCustomOverContentImageIndex(Value: Integer);
begin
  if FCustomOverContentImageIndex <> Value then
  begin
    FCustomOverContentImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.Paint;
begin
  if (FItemWidth = 0) and (FActivePage <> nil) then
    Exit
  else
    inherited;
end;

procedure TscAppPager.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  ItemsRect: TRect;
  PageRect: TRect;
  I: Integer;
begin
  if (FItemWidth = 0) and (FActivePage <> nil) then
    Exit;

  if BidiMode <> bdRightToLeft then
  begin
    ItemsRect := Rect(0, 0, FItemWidth, Height);
    PageRect := Rect(FItemWidth, 0, Width, Height);
  end
  else
  begin
    ItemsRect := Rect(Width - FItemWidth, 0, Width, Height);
    PageRect := Rect(0, 0, Width - FItemWidth, Height);
  end;

  with ACanvas do
  begin
    if seClient in StyleElements then
      Brush.Color := GetStyleColor(Color)
    else
      Brush.Color := Color;
    FillRect(PageRect);
    if seClient in StyleElements then
      Brush.Color := GetStyleColor(FItemBackgroundColor)
    else
      Brush.Color := FItemBackgroundColor;
    FillRect(ItemsRect);
  end;

  if FItemWidth = 0 then Exit;

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
  then
    FCustomImages.Draw(ACanvas, ItemsRect, FCustomBackgroundImageIndex, FScaleFactor);
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
    FWallpapers.Draw(ACanvas, ItemsRect, FWallpaperIndex, FScaleFactor);
  // draw caption
  if FShowCaption then
  begin
    ACanvas.Font := FCaptionFont;
    if seClient in StyleElements  then
      ACanvas.Font.Color := GetStyleColor(ACanvas.Font.Color);
    FCaptionRect := ItemsRect;
    FCaptionRect.Bottom := FCaptionRect.Top + FCaptionHeight;
    OffsetRect(FCaptionRect, 0, -FScrollOffset);
    if FCaptionGlowEffect.Enabled then
    begin
      if (FCaptionLayout = blGlyphLeft) or (FCaptionLayout = blGlyphRight)
      then
        DrawImageAndTextWithGlow2(ACanvas, FCaptionRect, -1, FItemSpacing,
        FCaptionLayout, Caption, FCaptionImageIndex, FCaptionImages,
        ACtrlState <> scsDisabled, False, clBlack,
        FCaptionGlowEffect.Offset, FCaptionGlowEffect.Color,
        FCaptionGlowEffect.GlowSize,
        FCaptionGlowEffect.Intensive,
        FCaptionGlowEffect.AlphaValue, True, False, IsRightToLeft, True, FScaleFactor)
     else
       DrawImageAndTextWithGlow(ACanvas, FCaptionRect, -1, FItemSpacing,
        FCaptionLayout, Caption, FCaptionImageIndex, FCaptionImages,
        ACtrlState <> scsDisabled, False, clBlack,
        FCaptionGlowEffect.Offset, FCaptionGlowEffect.Color,
        FCaptionGlowEffect.GlowSize,
        FCaptionGlowEffect.Intensive,
        FCaptionGlowEffect.AlphaValue, True, False, IsRightToLeft, True, FScaleFactor)
    end
    else
    if (FCaptionLayout = blGlyphLeft) or (FCaptionLayout = blGlyphRight) then
      DrawImageAndText2(ACanvas, FCaptionRect, -1, FItemSpacing,
       FCaptionLayout, Caption, FCaptionImageIndex, FCaptionImages,
        ACtrlState <> scsDisabled, False, clBlack, False, IsRightToLeft, True, FScaleFactor)
    else
      DrawImageAndText(ACanvas, FCaptionRect, -1, FItemSpacing,
       FCaptionLayout, Caption, FCaptionImageIndex, FCaptionImages,
        ACtrlState <> scsDisabled, False, clBlack, False, IsRightToLeft, True, FScaleFactor)
  end;
  // draw items
  CalcItemRects;
  for I := 0 to FItems.Count - 1  do
  begin
    if FItems[I].Visible and FItems[I].IsVisible then
      DrawItem(ACanvas, I);
  end;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomOverContentImageIndex)
  then
    FCustomImages.Draw(ACanvas, ItemsRect, FCustomOverContentImageIndex, FScaleFactor);
end;

procedure TscAppPager.ShowScrollbar;
begin
  if FItemWidth = 0 then Exit;
  if ScrollBar = nil
  then
    begin
      ScrollBar := TscScrollBar.Create(Self);
      ScrollBar.Visible := False;
      ScrollBar.Parent := Self;
      ScrollBar.SmallChange := FItemHeight;
      ScrollBar.LargeChange := FItemHeight;
      ScrollBar.Kind := sbVertical;
      ScrollBar.OnChange := SBChange;
      AdjustScrollBar;
      ScrollBar.Visible := True;
      RePaintControl;
      if FActivePage <> nil then
        FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
        FActivePage.Width, FActivePage.Height);
    end;
end;

procedure TscAppPager.ScrollToItem(Index: Integer);
var
  R: TRect;
  VOff: Integer;
begin
  if FItemWidth = 0 then Exit;
  if Index = -1 then Exit;
  R := Items[Index].ItemRect;
  if R.Top < 0
  then
    VOff := R.Top - FScrollOffset
  else
    VOff := R.Bottom - FScrollOffset;
  if (ScrollBar <> nil) and (ScrollBar.Visible) and
     ((R.Top < 0) or (R.Bottom > Self.Height))
  then
    begin
      if Index = Items.Count -1
      then
        ScrollBar.Position := ScrollBar.Max
      else
        ScrollBar.Position := ScrollBar.Position + VOff;
    end
  else
  if (ScrollBar <> nil) and (ScrollBar.Visible) and
     (Index = Items.Count -1) and (ScrollBar.Position <> ScrollBar.Max)
  then
    begin
      ScrollBar.Position := ScrollBar.Max;
    end
  else
  if (ScrollBar <> nil) and (ScrollBar.Visible) and
     (Index = 0) and (ScrollBar.Position <> ScrollBar.Min)
  then
    begin
      ScrollBar.Position := ScrollBar.Min;
    end;

  if Items[Index].Page <> nil
  then
    begin
      R := Items[Index].ItemRect;
      if R.Top < GetTopOffset
      then
        begin
          VOff := GetTopOffset - R.Top;
          ScrollBar.Position := ScrollBar.Position - VOff;
        end
      else
      if R.Bottom > Height - GetTopOffset
      then
        begin
          VOff := R.Bottom - (Height - GetTopOffset);
          ScrollBar.Position := ScrollBar.Position + VOff;
        end;
    end;
end;

procedure TscAppPager.Scroll(AScrollOffset: Integer);
begin
  if FItemWidth = 0 then Exit;
  FScrollOffset := AScrollOffset;
  RePaint;
end;

procedure TscAppPager.SBChange(Sender: TObject);
begin
  if (ScrollBar <> nil) and ScrollBar.HandleAllocated then
  begin
   if ScrollBar.Position <= ScrollBar.Max - ScrollBar.PageSize + 1  then
     Scroll(ScrollBar.Position)
   else
     Scroll(ScrollBar.Max - ScrollBar.PageSize + 1);
  end;
end;

procedure TscAppPager.HideScrollBar;
begin
  if ScrollBar = nil then Exit;
  ScrollBar.Visible := False;
  ScrollBar.Free;
  ScrollBar := nil;
  RePaintControl;
  if FActivePage <> nil then
    FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
    FActivePage.Width, FActivePage.Height);
end;

procedure TscAppPager.AdjustScrollBar;
var
  R: TRect;
  ScrollSize: Integer;
begin
  ScrollSize := GetSystemMetrics(SM_CYHSCROLL);
  if FItemWidth = 0 then Exit;
  if ScrollBar = nil then Exit;
  R := Rect(0, 0, Width, Height);
  Dec(R.Right, ScrollSize);
  if BidiMode <> bdRightToLeft then
    ScrollBar.SetBounds(FItemWidth, R.Top, ScrollSize, RectHeight(R))
  else
    ScrollBar.SetBounds(Width - FItemWidth - ScrollSize, R.Top, ScrollSize, RectHeight(R));
end;

procedure TscAppPager.SetCaptionLayout(Value: TButtonLayout);
begin
  if FCaptionLayout <> Value then
  begin
    FCaptionlayout := Value;
    if FShowCaption then
      RePaintControl;
  end;
end;

procedure TscAppPager.SetCaptionImageIndex(Value: Integer);
begin
  if FCaptionImageIndex <> Value then
  begin
    FCaptionImageIndex := Value;
    if FShowCaption then
      RePaintControl;
  end;
end;

procedure TscAppPager.SetItemHotColor(Value: TColor);
begin
  if FItemHotColor <> Value then
  begin
    FItemHotColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemSelectedColor(Value: TColor);
begin
  if FItemSelectedColor <> Value then
  begin
    FItemSelectedColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemSelectedFocusedColor(Value: TColor);
begin
  if FItemSelectedFocusedColor <> Value then
  begin
    FItemSelectedFocusedColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemFontHotColor(Value: TColor);
begin
  if FItemFontHotColor <> Value then
  begin
    FItemFontHotColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemFontSelectedColor(Value: TColor);
begin
  if FItemFontSelectedColor <> Value then
  begin
    FItemFontSelectedColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemFontSelectedFocusedColor(Value: TColor);
begin
  if FItemFontSelectedFocusedColor <> Value then
  begin
    FItemFontSelectedFocusedColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemHotImageIndex(Value: Integer);
begin
  if FItemHotImageIndex<> Value then
  begin
    FItemHotImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemSelectedImageIndex(Value: Integer);
begin
  if FItemSelectedImageIndex <> Value then
  begin
    FItemSelectedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemSelectedFocusedImageIndex(Value: Integer);
begin
  if FItemSelectedFocusedImageIndex <> Value then
  begin
    FItemSelectedFocusedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemBackgroundColor(Value: TColor);
begin
  if FItemBackgroundColor <> Value then
  begin
    FItemBackgroundColor := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetCaptionImages(Value: TCustomImageList);
begin
  if FCaptionImages <> Value then
  begin
    FCaptionImages := Value;
    if FShowCaption then
      RePaintControl;
  end;
end;

procedure TscAppPager.SetItemImages(Value: TCustomImageList);
begin
  if FItemImages <> Value then
  begin
    FItemImages := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemWidth;
var
  I: Integer;
  R: TRect;
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    R := GetPageBoundsRect;
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TscAppPagerPage then
      Controls[I].SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemHeight;
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemsTopOffset(Value: Integer);
begin
  if (Value >= 0) and (FItemsTopOffset <> Value) then
  begin
    FItemsTopOffset := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemMargin(Value: Integer);
begin
  if (Value > 0) and (FItemMargin <> Value) then
  begin
    FItemMargin := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemSpacing(Value: Integer);
begin
  if (Value > 0) and (FItemSpacing <> Value) then
  begin
    FItemSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscAppPager.SetItemIndex;
var
  B: Boolean;
begin
  if Assigned(FOnCanChangePage) and not (csLoading in ComponentState) and
    (Value >= 0) and (Value < Items.Count) and (Items[Value].Page <> nil)
  then
  begin
    B := True;
    FOnCanChangePage(Items[Value].Page, B);
    if not B then Exit;
  end;
  FItemIndex := Value;
  if (ItemIndex >= 0) and (FItemIndex < Items.Count)
  then
    begin
      if (Items[FItemIndex].Page <> nil) and (ActivePage <> Items[FItemIndex].Page)
      then
      begin
        if Assigned(FItems[FItemIndex].OnClick) then
          FItems[FItemIndex].OnClick(Self);
        if FChangePageWithItemSelection or not FChangeFromKeyboard then
          ActivePage := Items[FItemIndex].Page;
      end;
    end;
  if ScrollBar <> nil then ScrollToItem(FItemIndex);
  RePaintControl;
end;

function TscAppPager.ItemFromPoint;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Items.Count -1 do
    if Items[i].Visible and PtInRect(Items[i].ItemRect, P)
    then
      begin
        Result := i;
        Break;
      end;
end;

procedure TscAppPager.CMDesignHitTest;
var
  P: TPoint;
  I: Integer;
begin
  inherited;
  try
    P := SmallPointToPoint(Message.Pos);
    if (Message.Keys = MK_LBUTTON) and (ItemFromPoint(P) <> -1) then
    begin
      I := ItemFromPoint(P);
      if (Items[I].Page <> nil) and (Items[I].Page <> ActivePage)
      then ActivePage := Items[I].Page;
        GetParentForm(Self).Designer.Modified;
    end;
  except
  end;
end;


procedure TscAppPager.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscAppPager.Notification(AComponent: TComponent;
      Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TscAppPagerPage) and (FItems <>  nil) then
  begin
    I := GetPageIndex(TscAppPagerPage(AComponent));
    if I <> -1 then
    begin
      Items[I].FPage := nil;
      RePaintControl;
    end;
  end;
  if (Operation = opRemove) and (AComponent = FActivePage) then
    FActivePage := nil;
  if (Operation = opRemove) and (AComponent = FItemImages) then
    FItemImages := nil;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
  if (Operation = opRemove) and (AComponent = FCaptionImages) then
    FCaptionImages := nil;
end;

procedure TscAppPager.FindNextItem;
var
  i, j, k: Integer;
begin
  j := ItemIndex;
  if (j = -1) or (j = Items.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Items.Count - 1 do
  begin
    if Items[i].Visible and Items[i].Enabled and not FItems[i].Divider
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then ItemIndex := k;
end;

procedure TscAppPager.FindPriorItem;
var
  i, j, k: Integer;
begin
  j := ItemIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Items[i].Visible and Items[i].Enabled and not FItems[i].Divider
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then ItemIndex := k;
end;

procedure TscAppPager.FindFirstItem;
var
  i, k: Integer;
begin
  k := -1;
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Visible and Items[i].Enabled and not FItems[i].Divider
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then ItemIndex := k;
end;

procedure TscAppPager.FindLastItem;
var
  i, k: Integer;
begin
  k := -1;
  for i := Items.Count - 1 downto 0 do
  begin
    if Items[i].Visible and Items[i].Enabled and not FItems[i].Divider
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then ItemIndex := k;
end;

procedure TscAppPager.WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL);
var
  P: TPoint;
  Wnd: HWnd;
begin
  inherited;
  if (FItemWidth = 0) or not FMouseWheelSupport then Exit;
  GetCursorPos(P);
  Wnd := WindowFromPoint(P);
  if (Wnd = Handle) or
     ((ScrollBar <> nil) and (Wnd = ScrollBar.Handle)) then
    if Message.WheelDelta < 0 then FindNextItem else FindPriorItem;
end;

procedure TscAppPager.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FItemWidth = 0 then Exit;
  if (Key = VK_RETURN) or (Key = VK_SPACE)
  then
    begin
      if (ItemIndex <> -1) and (Items[ItemIndex].Page = nil)
      then
        begin
          if Assigned(FItems[FItemIndex].OnClick) then
            FItems[FItemIndex].OnClick(Self);
        end
      else
      begin
        if (ItemIndex <> -1) and (Items[ItemIndex].Page <> nil)
        then
          begin
            if not FChangePageWithItemSelection and (Items[ItemIndex].Page <> ActivePage) then
              ActivePage := Items[ItemIndex].Page;

            if (Items[ItemIndex].Page.FirstActiveControl <> nil) and
               (Items[ItemIndex].Page.FirstActiveControl.Parent = Items[ItemIndex].Page) and
               (Items[ItemIndex].Page.FirstActiveControl.Visible) and
               (Items[ItemIndex].Page.FirstActiveControl.Enabled)
            then
              Items[ItemIndex].Page.FirstActiveControl.SetFocus;
         end;
      end;
    end
  else
  if (Key = VK_NEXT)
  then
  begin
    FChangeFromKeyboard := True;
    FindLastItem;
    FChangeFromKeyboard := False;
  end
  else
  if (Key = VK_PRIOR)
  then
  begin
    FChangeFromKeyboard := True;
    FindFirstItem;
    FChangeFromKeyboard := False;
  end
  else
  if (Key = VK_LEFT) or (Key = VK_UP)
  then
  begin
    FChangeFromKeyboard := True;
    FindPriorItem;
    FChangeFromKeyboard := False;
  end
  else
  if (Key = VK_RIGHT) or (Key = VK_DOWN)
  then
  begin
    FChangeFromKeyboard := True;
    FindNextItem;
    FChangeFromKeyboard := False;
  end;
end;

procedure TscAppPager.WMGetDlgCode;
begin
  if FItemWidth = 0 then Exit;
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscAppPager.WMSETFOCUS;
begin
  inherited;
  if FItemWidth = 0 then Exit;
  RePaintControl;
end;

procedure TscAppPager.WMKILLFOCUS;
begin
  inherited;
  if FItemWidth = 0 then Exit;
  RePaintControl;
end;

procedure TscAppPager.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FItemWidth = 0 then Exit;
  if not FMouseIn then FMouseIn := True;
  TestActive(X, Y);
end;

procedure TscAppPager.MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);

var
  WasFocused: Boolean;
begin
  inherited;
  if FItemWidth = 0 then Exit;
  if Button <> mbLeft then Exit;
  if FShowCaption and Assigned(FOnCaptionClick) and
     PtInRect(FCaptionRect, Point(X, Y)) then
    FCaptionDown := True
  else
    FCaptionDown := False;
  TestActive(X, Y);

  WasFocused := Focused;

  if FItemActive <> ItemIndex then ItemIndex := FItemActive;

  if not WasFocused then SetFocus;
end;

procedure TscAppPager.MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
begin
  inherited;
  if FItemWidth = 0 then Exit;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  if (ItemIndex <> -1) and (Items[ItemIndex].Page = nil) and
     (ItemIndex = FItemActive) then
  begin
    if Assigned(FItems[FItemIndex].OnClick) then
        FItems[FItemIndex].OnClick(Self);
  end;
  if FShowCaption and Assigned(FOnCaptionClick) and
     PtInRect(FCaptionRect, Point(X, Y)) and FCaptionDown then
  begin
    FCaptionDown := False;
    FOnCaptionClick(Self);
  end;
end;

procedure TscAppPager.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FItemWidth = 0 then Exit;
  TestActive(-1, -1);
end;

procedure TscAppPager.TestActive(X, Y: Integer);
var
  i: Integer;
begin
  if FItemWidth = 0 then Exit;
  if Items.Count = 0 then Exit;
  FOldItemActive:= FItemActive;
  FItemActive := -1;

  if FCaptionCursorEnabled then
  begin
    if FCaptionRect.Contains(Point(X, Y)) and (FCaptionCursor <> Cursor) then
    begin
      FSaveCursor := Cursor;
      Cursor := FCaptionCursor;
    end
    else
    if not FCaptionRect.Contains(Point(X, Y)) and (Cursor <> FSaveCursor) then
      Cursor := FSaveCursor;
  end;

  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Visible and not Items[i].Divider and Items[i].Enabled and PtInRect(Items[i].ItemRect, Point(X, Y))
    then
      begin
        FItemActive := i;
        Break;
      end;
  end;

  if (FItemActive <> FOldItemActive)
  then
    begin
      if (FOldItemActive <> - 1)
      then
        Items[FOldItemActive].Active := False;
      if (FItemActive <> - 1)
      then
        Items[FItemActive].Active := True;
      RePaintControl;
    end;
end;

procedure TscAppPager.GetScrollInfo(var AMin, AMax, APage: Integer);
var
  I, Y: Integer;
begin
  Y := GetTopOffset;
  for I := 0 to Items.Count - 1 do
    if Items[I].Visible then
    begin
      if Items[I].Divider then
        Y := Y + DefDividerItemHeight
      else
        Y := Y + FItemHeight;
    end;
  AMax := Y - 1;
  AMin := 0;
  APage := Height;
end;

procedure TscAppPager.CalcItemRects;
var
  I, X, Y: Integer;
begin
  if BidiMode <> bdRightToLeft then
    X := 0
  else
    X := Width - FItemWidth;

  Y := GetTopOffset - FScrollOffset;
  for I := 0 to Items.Count - 1 do
    if Items[I].Visible then
    begin
      if Items[I].Divider then
      begin
        Items[I].ItemRect := Rect(X, Y, X + FItemWidth, Y + DefDividerItemHeight);
        Y := Y + DefDividerItemHeight;
      end
      else
      begin
        Items[I].ItemRect := Rect(X, Y, X + FItemWidth, Y + FItemHeight);
        Y := Y + FItemHeight;
      end;
      Items[I].IsVisible := (Items[I].ItemRect.Top + Items[I].ItemRect.Height >= 0) and
        (Items[I].ItemRect.Bottom - Items[I].ItemRect.Height <= Height);
    end;
end;

procedure TscAppPager.SetItems(AValue: TscAppPagerItems);
begin
  FItems.Assign(AValue);
  RePaintControl;
end;

procedure TscAppPager.SetActivePage(const Value: TscAppPagerPage);
var
  OldActivePage: TscAppPagerPage;
begin
  OldActivePage := FActivePage;
  FActivePage := Value;
  if (Value <> nil) and not (csLoading in ComponentState)
  then
    begin
      RePaintControl;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangingPage) then FOnChangingPage(Self);
      with FActivePage do
      begin
        Visible := False;
        Parent := Self;
        SetBounds(Left, Top, Width, Height);
        Visible := True;
        BringToFront;
        FullRedraw;
      end;
      if (OldActivePage <> FActivePage) and Assigned(FOnChangePage) and
        not (csLoading in ComponentState)
      then FOnChangePage(Self);
    end;
  if (OldActivePage <> nil) and (OldActivePage <> FActivePage) then
    OldActivePage.Visible := False;
end;

function TscAppPager.GetPageIndex(Value: TscAppPagerPage): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
    if Items[i].Page = Value
    then
       begin
         Result := i;
         Break;
       end;
end;

procedure TscAppPager.Loaded;
var
  i: Integer;
begin
  inherited;
  if Items.Count > 0 then
    for i := 0 to Items.Count - 1 do
    if Items[i].Page <> nil then
    begin
      Items[i].Page.AppPager := Self;
      if Items[i].Page = FActivePage
      then
        Items[i].Page.Visible := True
      else
        Items[i].Page.Visible := False;
    end;
  UpdateScrollInfo;
  if FActivePage <> nil then
    FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
      FActivePage.Width, FActivePage.Height);
end;

function TscAppPager.GetPageBoundsRect: TRect;
begin
  Result.Top := 0;
  Result.Bottom := Height;
  if BidiMode <> bdRightToLeft then
  begin
    Result.Left := FItemWidth;
    Result.Right := Width - FItemWidth;
    if ScrollBar <> nil then
    begin
      Result.Right := Result.Right - ScrollBar.Width;
      Result.Left := Result.Left + ScrollBar.Width;
    end;
  end
  else
  begin
    Result.Left := 0;
    Result.Right := Width - FItemWidth;
    if ScrollBar <> nil then
      Result.Right := Result.Right - ScrollBar.Width;
  end;
end;

procedure TscAppPager.UpdateScrollInfo;
var
  FMin, FMax, FPage: Integer;
begin
  if FItemWidth = 0 then Exit;
  GetScrollInfo(FMin, FMax, FPage);
  if (FMax > FPage) and (ScrollBar = nil)
  then
    ShowScrollBar
  else
    if (FMax <= FPage) and (ScrollBar <> nil)
    then
      HideScrollBar;
  if ScrollBar <> nil
  then
     begin
       ScrollBar.SetParams(FScrollOffset, FMin, FMax);
       ScrollBar.PageSize := FPage;
       ScrollBar.LargeChange := FPage;
     end;
end;

procedure TscAppPager.WMSIZE(var Message: TWMSIZE);
var
  FMin, FMax, FPage: Integer;
begin
  inherited;
  if (FOldHeight <> Height) and (FOldHeight <> -1)
  then
    begin
      GetScrollInfo(FMin, FMax, FPage);
      if FScrollOffset > 0
      then
        FScrollOffset := FScrollOffset - (Height - FOldHeight);
      if FScrollOffset < 0 then FScrollOffset := 0;
      if (FMax > FPage) and (ScrollBar = nil)
      then
        ShowScrollBar
      else
      if (FMax <= FPage) and (ScrollBar <> nil)
      then
        HideScrollBar;
      if ScrollBar <> nil
      then
        begin
          ScrollBar.SetParams(FScrollOffset, FMin, FMax);
          ScrollBar.PageSize := FPage;
          ScrollBar.LargeChange := FPage;
        end;
    end;
  if ScrollBar <> nil then
    AdjustScrollBar;
  if (ActivePage <> nil)
  then
    with ActivePage do
      SetBounds(Left, Top, Width, Height);
  if FItemWidth > 0 then
    RePaintControl;
  FOldHeight := Height;
end;

procedure TscAppPager.DrawItem(ACanvas: TCanvas; Index: Integer);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  R, R1, MR: TRect;
  FC, BGC, DivC: TColor;
  IIndex, TX, TY: Integer;
  ItemState: TscsCtrlState;
  FGlowColor: TColor;
  FSelectedColor, FFocusedColor, FHotColor: TColor;
  FSelectedFontColor, FFocusedFontColor, FHotFontColor: TColor;
  SaveIndex: Integer;
begin
  if (Items[Index].Page = ActivePage) and (ActivePage <> nil) and
      Items[Index].Enabled and Items[Index].Visible and (FItemIndex = -1)
  then
    FItemIndex := Index;
  R := FItems[Index].ItemRect;

  ACanvas.Font := Self.Font;
  if IsCustomStyle and (seClient in StyleElements) then
  begin
    FC := GetStyleColor(clWindowText);
    BGC := GetStyleColor(clWindow);
    FSelectedColor := GetStyleColor(clHighLight);
    FFocusedColor := GetStyleColor(clHighLight);
    FHotColor := GetStyleColor(clHighLight);
    FSelectedFontColor := GetSelectionTextColor;
    FFocusedFontColor := FSelectedFontColor;
    FHotFontColor := FSelectedFontColor;
  end
 else
 begin
   FC := GetStyleColor(Self.Font.Color);
   BGC := GetStyleColor(FItemBackgroundColor);
   FSelectedColor := GetStyleColor(FItemSelectedColor);
   FFocusedColor := GetStyleColor(FItemSelectedFocusedColor);
   FHotColor := GetStyleColor(FItemHotColor);
   if FSelectionStyle = scapsStyled then
   begin
     FSelectedFontColor := GetSelectionTextColor;
     FFocusedFontColor := FSelectedFontColor;
     FHotFontColor := FSelectedFontColor;
   end
   else
   begin
     FSelectedFontColor := GetStyleColor(FItemFontSelectedColor);
     FFocusedFontColor := GetStyleColor(FItemFontSelectedFocusedColor);
     FHotFontColor := GetStyleColor(FItemFontHotColor);
   end;
 end;

 if FDividerColor = clNone then
 begin
   DivC := MiddleColor(FC, BGC);
   DivC := MiddleColor(DivC, BGC);
 end
 else
   DivC := GetStyleColor(FDividerColor);

 if Items[Index].Divider then
 begin
   with ACanvas do
   begin
     Pen.Color := DivC;
     MoveTo(FItems[Index].ItemRect.Left + FItemMargin,
       FItems[Index].ItemRect.Top + FItems[Index].ItemRect.Height div 2);
     LineTo(FItems[Index].ItemRect.Right - FItemMargin,
       FItems[Index].ItemRect.Top + FItems[Index].ItemRect.Height div 2);
   end;
   Exit;
 end;

 if FItems[Index].Enabled then
   ItemState := scsNormal
 else
   ItemState := scsDisabled;

 with ACanvas do
 begin
   if (Items[Index].Page = ActivePage) and (ActivePage <> nil) and
      Items[Index].Enabled and Items[Index].Visible
   then
   begin
     if Focused then
       ItemState := scsFocused
     else
       ItemState := scsPressed;
     if Focused then
     begin
       Brush.Color := FFocusedColor;
       FC := FFocusedFontColor;
     end
     else
     begin
       Brush.Color := FSelectedColor;
       FC := FSelectedFontColor;
     end;
     if FSelectionStyle = scapsModern then
     begin
       R1 := Rect(R.Left, R.Top, R.Left + Trunc(5 * FScaleFactor), R.Bottom);
       if Focused or (FItemGlowEffect.Enabled and (scsFocused in FItemGlowEffect.States)) then
         FillRect(R1)
       else
         FillRectWithAlpha(ACanvas, R1, 200);  
       FC := Brush.Color;
     end
     else
      if (FSelectionStyle = scapsCustomImageOverColor) or
         (FSelectionStyle = scapsColor)
      then
      begin
        if Focused or (FItemGlowEffect.Enabled and (scsFocused in FItemGlowEffect.States)) then
         FillRect(R)
        else
          FillRectWithAlpha(ACanvas, R, 200);
      end
      else
      if FSelectionStyle = scapsStyled then
      begin
        if not Focused then
        begin
          if IsCustomStyle then
            DrawSelectionWithAlpha(ACanvas, R, 200)
          else
            DrawSelection(ACanvas, R, False, False);
        end
        else
          DrawSelection(ACanvas, R, Focused, False);
      end;
   end
   else                                                           
   if ((FItems[Index].Active) or ((Index = ItemIndex) and Focused)) and
      not (csDesigning in ComponentState)
   then
   begin
     ItemState := scsHot;
     Brush.Color := FHotColor;
     FC := FHotFontColor;
     if FSelectionStyle = scapsModern then
     begin
       Brush.Color := GetStyleColor(Self.Font.Color);
       FC := GetStyleColor(Self.Font.Color);
       if (FItemIndex = Index) and Focused then
       begin
         SaveIndex := SaveDC(ACanvas.Handle);
         try
           ExcludeClipRect(ACanvas.Handle,
             R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2);
           FillRectWithAlpha(ACanvas, R, 80);
         finally
           RestoreDC(ACanvas.Handle, SaveIndex);
         end;
       end
       else
       begin
         if FItems[Index].Active then
           FillRectWithAlpha(ACanvas, R, 30);
       end;
     end
     else
     if (FSelectionStyle = scapsCustomImageOverColor) or
        (FSelectionStyle = scapsColor)
      then
      begin
        if FSelectedColor = FHotColor then
        begin
          if FItemIndex = Index then
            FillRectWithAlpha(ACanvas, R, 150)
          else
          begin
            FC := GetStyleColor(Self.Font.Color);
            FillRectWithAlpha(ACanvas, R, 100);
          end;
        end
        else
          FillRect(R);
      end
      else
      if FSelectionStyle = scapsStyled then
      begin
        if not IsCustomStyle then
          DrawSelectionWithAlpha(ACanvas, R, 180)
        else
          DrawSelectionWithAlpha(ACanvas, R, 150);
      end;
   end;
 end;
 // draw cusotm image over item
 if (FSelectionStyle = scapsCustomImageOverColor) or
    (FSelectionStyle = scapsCustomImage)
 then
 begin
   IIndex := -1;
   case ItemState of
     scsHot: IIndex := FItemHotImageIndex;
     scsPressed: IIndex := FItemSelectedImageIndex;
     scsFocused: IIndex := FItemSelectedFocusedImageIndex;
   end;
   if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(IIndex)
   then
   begin
     R1 := R;
     MR := FCustomImages.GetContentMargins(IIndex);
     if MR.Top > 0 then
       Dec(R1.Top, MR.Top);
     if MR.Bottom > 0 then
       Inc(R1.Bottom, MR.Bottom);
     FCustomImages.Draw(ACanvas, R1, IIndex, FScaleFactor);
   end;
 end;
 // draw image and text
 FGlowColor := FItemGlowEffect.Color;
 case ItemState of
   scsHot: FGlowColor := FItemGlowEffect.HotColor;
   scsPressed: FGlowColor := FItemGlowEffect.PressedColor;
   scsFocused: FGlowColor := FItemGlowEffect.FocusedColor;
 end;
 if FGlowColor = clNone then
   FGlowColor := FItemGlowEffect.Color;

 ACanvas.Brush.Style := bsClear;
 ACanvas.Font.Color := FC;
 if not FItems[Index].Enabled then
   ACanvas.Font.Color := MiddleColor(FC, BGC)
 else
   ACanvas.Font.Color := FC;

 if BidiMode <> bdRightToLeft then
   Inc(R.Left, FItemMargin)
 else
   Dec(R.Right, FItemMargin);

 IIndex := FItems[Index].ImageIndex;
 if (FItemImages <> nil) and (IIndex >= 0) and
    (IIndex < FItemImages.Count) then
 begin
   if FItemGlowEffect.Enabled and (ItemState in FItemGlowEffect.States) then
      DrawImageAndTextWithGlow2(ACanvas, R, 0, FItemSpacing, GlyphLayout[IsRightToLeft],
        FItems[Index].Caption, IIndex, FItemImages,
        FItems[Index].Enabled and Self.Enabled, False, clBlack,
        FItemGlowEffect.Offset, FGlowColor,
        FItemGlowEffect.GlowSize, FItemGlowEffect.Intensive,
        FItemGlowEffect.AlphaValue, True,
        False, IsRightToLeft, True, FScaleFactor)
    else
      DrawImageAndText2(ACanvas, R, 0, FItemSpacing, GlyphLayout[IsRightToLeft],
        FItems[Index].Caption, IIndex, FItemImages,
        FItems[Index].Enabled and Self.Enabled, False, clBlack, False, IsRightToLeft, True,
        FScaleFactor)
  end
  else
  begin
    if FItemImages <> nil then Inc(R.Left, FItemImages.Width + 5);
    R1 := Rect(0, 0, R.Width, R.Height);
    DrawText(ACanvas.Handle, PChar(FItems[Index].Caption),
      Length(FItems[Index].Caption), R1,
      DT_LEFT or DT_CALCRECT or DT_WORDBREAK);
    TX := R.Left;
    TY := R.Top + R.Height div 2 - R1.Height div 2;
    if TY < R.Top then TY := R.Top;
    R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
    if FItemGlowEffect.Enabled and (ItemState in FItemGlowEffect.States) then
      DrawTextWithGlow(ACanvas, R, FItems[Index].Caption, DT_WORDBREAK or DT_LEFT,
        FItemGlowEffect.Offset, FGlowColor, FItemGlowEffect.GlowSize,
        FItemGlowEffect.Intensive, FItemGlowEffect.AlphaValue, IsRightToLeft, True)
    else
      DrawText(ACanvas.Handle, PChar(FItems[Index].Caption),
        Length(FItems[Index].Caption), R,
        scDrawTextBidiModeFlags(DT_WORDBREAK or DT_LEFT, BidiMode = bdRightToLeft));
  end;
end;

procedure TscAppPager.CreatePage;

  function GetUniqueName(const Name: string; const AComponent: TComponent): string;
  var
    LIdx: Integer;
  begin
    LIdx := 1;
    Result := Format(Name, [LIdx]);
    if AComponent <> nil then
    begin
      while AComponent.FindComponent(Result) <> nil do
      begin
        Inc(LIdx);
        Result := Format(Name, [LIdx]);
      end;
    end;
  end;

var
  LPage: TscAppPagerPage;
  R: TRect;
begin
  LPage := TscAppPagerPage.Create(GetParentForm(Self));
  LPage.Parent := Self;
  LPage.AppPager := Self;
  R := GetPageBoundsRect;
  LPage.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
  LPage.Name := GetUniqueName('scAppPagerPage%d', GetParentForm(Self));
  ActivePage := LPage;
  RePaintControl;
end;

initialization

  TCustomStyleEngine.RegisterStyleHook(TscAppPagerPage, TscScrollBoxStyleHook);

finalization

  {$IFNDEF VER230}
  TCustomStyleEngine.UnRegisterStyleHook(TscAppPagerPage, TscScrollBoxStyleHook);
  {$ENDIF}

end.

