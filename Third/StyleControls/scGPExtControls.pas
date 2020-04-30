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

unit scGPExtControls;

{$I scdefine.inc}
{$R-}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Types, System.UITypes, Vcl.Forms, Vcl.Mask, Vcl.Menus,
  Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList,
  scDrawUtils, scGPUtils, scControls, scExtControls, scImageCollection,
  WinApi.GdipObj, WinApi.GdipApi, scGPControls, scCalendar, scHint;

type
  TscGPScrollingControl = class;

  TscGPScrollBarPosition = (scgpsbDefault, scgpsbOverContent);

  TscGPControlScrollBarOptions = class(TPersistent)
  private
    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FThumbColor: TColor;
    FThumbColorAlpha: Byte;
    FThumbColorHotAlpha: Byte;
    FThumbColorPressedAlpha: Byte;
    FThumbRounded: Boolean;

    FBorderWidth: Integer;
    FSize: Integer;
    FPosition: TscGPScrollBarPosition;

    FOnChange: TNotifyEvent;

    procedure SetPosition(Value: TscGPScrollBarPosition);
    procedure SetBorderWidth(Value: Integer);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetThumbColor(Value: TColor);
    procedure SetThumbColorAlpha(Value: Byte);
    procedure SetThumbColorHotAlpha(Value: Byte);
    procedure SetThumbColorPressedAlpha(Value: Byte);
    procedure SetThumbRounded(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderWidth: Integer
      read FBorderWidth write SetBorderWidth;
    property FillColor: TColor
      read FFillColor write SetFillColor;
    property FillColorAlpha: Byte
      read FFillColorAlpha write SetFillColorAlpha;
    property ThumbColor: TColor
      read FThumbColor write SetThumbColor;
    property ThumbColorAlpha: Byte
      read FThumbColorAlpha write SetThumbColorALpha;
    property ThumbColorHotAlpha: Byte
      read FThumbColorHotAlpha write SetThumbColorHotALpha;
    property ThumbColorPressedAlpha: Byte
      read FThumbColorPressedAlpha write SetThumbColorPressedALpha;
    property ThumbRounded: Boolean
      read FThumbRounded write SetThumbRounded;
    property Position: TscGPScrollBarPosition read
      FPosition write SetPosition;
    property Size: Integer
      read FSize write SetSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscGPControlScrollBar = class(TPersistent)
  protected
    FMin, FMax, FPosition, FPageSize: Integer;
    FControl: TWinControl;
    FVertical: Boolean;
    FBoundsRect: TRect;
    ThumbRect, TrackRect: TRect;
    OMPos, ThumbPos, ScrollMPos: Integer;
    FDownPosition: Integer;
    FScrollTimer: TTimer;
    FOnChange: TNotifyEvent;
    function GetScrollBarRect: TRect;
    procedure OnScrollTimer(Sender: TObject);
    function GetVisible: Boolean;
    function GetOptions: TscGPControlScrollBarOptions; virtual;
  public
    FDown: Boolean;
    FThumbState: TscsCtrlState;
    constructor Create(AControl: TWinControl; AVertical: Boolean);
    destructor Destroy; override;
    procedure SetParams(AMin, AMax, APageSize, APosition: Integer; AUpdate: Boolean);
    procedure MouseLeave;
    procedure MouseMove(X, Y: Integer);
    procedure MouseUp(X, Y: Integer);
    procedure MouseDown(X, Y: Integer);
    procedure SetBoundsRect(ARect: TRect);
    procedure SetPosition(APosition: Integer; AUpdate: Boolean);
    procedure Draw(G: TGPGraphics);
    procedure RePaint; virtual;
    property Visible: Boolean read GetVisible;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;
    property PageSize: Integer read FPageSize write FPageSize;
    property Position: Integer read FPosition write FPosition;
    property ScrollBarRect: TRect read GetScrollBarRect;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscGPScrollingControlOptions = class(TPersistent)
  protected
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FFrameColorAlpha: Byte;
    FFrameScaleWidth: Boolean;

    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FFillColor2: TColor;
    FFillColor2Alpha: Byte;

    FFillStyle: TscGPShapeFillStyle;
    FFillGradientAngle: Integer;
    FOnChange: TNotifyEvent;

    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFillGradientAngle(Value: Integer);

    procedure SetFrameColorAlpha(Value: Byte);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFrameWidth(Value: Integer);

    procedure SetFillColor(Value: TColor);
    procedure SetFillColor2(Value: TColor);
    procedure SetFillColor2Alpha(Value: Byte);
    procedure SetFrameColor(Value: TColor);

    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColor2: TColor read FFillColor2 write SetFillColor2;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillColor2Alpha: Byte read FFillColor2Alpha write SetFillColor2Alpha;
    property FillStyle: TscGPShapeFillStyle
      read FFillStyle write SetFillStyle default scgpsfColor;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameColorAlpha: Byte read FFrameColorAlpha write SetFrameColorAlpha;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
    property FrameScaleWidth: Boolean
      read  FFrameScaleWidth write FFrameScaleWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscGPScrollingControl = class(TscCustomControl)
  protected
    FScrollBarOptions: TscGPControlScrollBarOptions;
    FVertScrollBar: TscGPControlScrollBar;
    FHorzScrollBar: TscGPControlScrollBar;

    FHTouchBegin, FHTouchEnd: Integer;
    FVTouchBegin, FVTouchEnd: Integer;

    FOptions: TscGPScrollingControlOptions;

    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomImageIndex: Integer;

    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);

    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure OnScrollOptionsChange(Sender: TObject);
    procedure OnControlChange(Sender: TObject);
    procedure GetScrollInfo; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;

    procedure CMGesture(var Message: TCMGesture); message CM_GESTURE;
    procedure InitTouch; virtual;

    procedure DrawContent(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect); virtual;
    function GetContentRect: TRect;

    function IsScrollBarCaptured: Boolean;
    function MouseInScrollBar(X, Y: Integer): Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomImageIndex: Integer read FCustomImageIndex write SetCustomImageIndex;
    property Options:  TscGPScrollingControlOptions
      read FOptions write FOptions;
    property ScrollBarOptions: TscGPControlScrollBarOptions
      read FScrollBarOptions write FScrollBarOptions;
    property Color;
    property TransparentBackground;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
  end;

  TscGPSelectionOptions = class(TPersistent)
  private
    FColor: TColor;
    FColorAlpha: Byte;
    FFillStyle: TscGPShapeFillStyle;
    FGradientAngle: Integer;
    FFocusedColor: TColor;
    FFocusedColorAlpha: Byte;
    FFocusedFillStyle: TscGPShapeFillStyle;
    FFocusedGradientAngle: Integer;
    FFontColor: TColor;
    FFocusedFontColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetFontColor(Value: TColor);
    procedure SetFocusedFontColor(Value: TColor);
    procedure SetColor(Value: TColor);
    procedure SetColorAlpha(Value: Byte);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetGradientAngle(Value: Integer);
    procedure SetFocusedColor(Value: TColor);
    procedure SetFocusedColorAlpha(Value: Byte);
    procedure SetFocusedFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFocusedGradientAngle(Value: Integer);
  protected
    procedure Changed;
  public
    Focused: Boolean;
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function GetColor: TColor;
    function GetColorAlpha: Byte;
    function GetFillStyle: TscGPShapeFillStyle;
    function GetGradientAngle: Integer;
    function GetFontColor: TColor;
  published
    property Color: TColor read FColor write SetColor;
    property ColorAlpha: Byte read FColorAlpha write SetColorAlpha;
    property FillStyle: TscGPShapeFillStyle read FFillStyle write SetFillStyle;
    property GradientAngle: Integer read FGradientAngle write SetGradientAngle;
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor;
    property FocusedColorAlpha: Byte read FFocusedColorAlpha write SetFocusedColorAlpha;
    property FocusedFillStyle: TscGPShapeFillStyle read FFocusedFillStyle write SetFocusedFillStyle;
    property FocusedGradientAngle: Integer read FFocusedGradientAngle write SetFocusedGradientAngle;
    property FontColor: TColor read FFontColor write SetFontColor;
    property FocusedFontColor: TColor read FFocusedFontColor write SetFocusedFontColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscGPHeaderOptions = class(TPersistent)
  private
    FColor: TColor;
    FColorAlpha: Byte;
    FFillStyle: TscGPShapeFillStyle;
    FGradientAngle: Integer;
    FMargin: Integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetColorAlpha(Value: Byte);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetGradientAngle(Value: Integer);
    procedure SetMargin(Value: Integer);
  protected
    procedure OnChanged(Sender: TObject);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor;
    property ColorAlpha: Byte read FColorAlpha write SetColorAlpha;
    property FillStyle: TscGPShapeFillStyle read FFillStyle write SetFillStyle;
    property GradientAngle: Integer read FGradientAngle write SetGradientAngle;
    property Margin: Integer read FMargin write SetMargin;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscGPListCheckBoxOptions = class(TPersistent)
    private
      FNormalColor: TColor;
      FNormalColorAlpha: Byte;
      FDisabledColor: TColor;
      FDisabledColorAlpha: Byte;

      FFrameNormalColor: TColor;
      FFrameDisabledColor: TColor;
      FFrameNormalColorAlpha: Byte;
      FFrameDisabledColorAlpha: Byte;

      FCheckedNormalColor: TColor;
      FCheckedNormalColorAlpha: Byte;
      FCheckedDisabledColor: TColor;
      FCheckedDisabledColorAlpha: Byte;

      FCheckedFrameNormalColor: TColor;
      FCheckedFrameDisabledColor: TColor;
      FCheckedFrameNormalColorAlpha: Byte;
      FCheckedFrameDisabledColorAlpha: Byte;

      FShapeSize: Integer;
      FFrameWidth: Integer;
      FCheckMarkNormalColor: TColor;
      FCheckMarkDisabledColor: TColor;
      FCheckMarkThickness: Integer;
      FCheckMarkNormalColorAlpha: Byte;
      FCheckMarkDisabledColorAlpha: Byte;

      FState: TscsCtrlState;
      FChecked: Boolean;

      FScaleCheckMarkThickness: Boolean;
      FScaleFrameWidth: Boolean;

      FOnChange: TNotifyEvent;

      procedure SetShapeSize(Value: Integer);
      procedure SetCheckMarkThickness(Value: Integer);

      function GetNormalColor: TColor;
      function GetDisabledColor: TColor;

      function GetFrameNormalColor: TColor;
      function GetFrameDisabledColor: TColor;

      function GetCheckedNormalColor: TColor;
      function GetCheckedDisabledColor: TColor;

      function GetCheckedFrameNormalColor: TColor;
      function GetCheckedFrameDisabledColor: TColor;

      function GetCheckMarkNormalColor: TColor;
      function GetCheckMarkDisabledColor: TColor;

      function GetColor: TColor;
      function GetFrameColor: TColor;
      function GetCheckMarkColor: TColor;
      function GetColorAlpha: Byte;
      function GetFrameColorAlpha: Byte;
      function GetCheckMarkColorAlpha: Byte;

      procedure SetNormalColor(Value: TColor);
      procedure SetDisabledColor(Value: TColor);
      procedure SetNormalColorAlpha(Value: Byte);
      procedure SetDisabledColorAlpha(Value: Byte);
      procedure SetFrameNormalColor(Value: TColor);
      procedure SetFrameDisabledColor(Value: TColor);
      procedure SetFrameNormalColorAlpha(Value: Byte);
      procedure SetFrameDisabledColorAlpha(Value: Byte);

      procedure SetCheckedNormalColor(Value: TColor);
      procedure SetCheckedDisabledColor(Value: TColor);
      procedure SetCheckedNormalColorAlpha(Value: Byte);
      procedure SetCheckedDisabledColorAlpha(Value: Byte);
      procedure SetCheckedFrameNormalColor(Value: TColor);
      procedure SetCheckedFrameDisabledColor(Value: TColor);
      procedure SetCheckedFrameNormalColorAlpha(Value: Byte);
      procedure SetCheckedFrameDisabledColorAlpha(Value: Byte);

      procedure SetCheckMarkNormalColorAlpha(Value: Byte);
      procedure SetCheckMarkDisabledColorAlpha(Value: Byte);

      procedure SetCheckMarkNormalColor(Value: TColor);
      procedure SetCheckMarkDisabledColor(Value: TColor);

      procedure SetFrameWidth(Value: Integer);

      procedure Changed;
    public
      constructor Create; virtual;
      procedure Assign(Source: TPersistent); override;

      property State: TscsCtrlState read FState write FState;
      property Checked: Boolean read FChecked write FChecked;

      property Color: TColor read GetColor;
      property FrameColor: TColor read GetFrameColor;
      property CheckMarkColor: TColor read GetCheckMarkColor;
      property ColorAlpha: Byte read GetColorAlpha;
      property FrameColorAlpha: Byte read GetFrameColorAlpha;
      property CheckMarkColorAlpha: Byte read GetCheckMarkColorAlpha;
    published
      property NormalColor: TColor read GetNormalColor write SetNormalColor;
      property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;
      property NormalColorAlpha: Byte read FNormalColorAlpha write SetNormalColorAlpha;
      property DisabledColorAlpha: Byte read FDisabledColorAlpha write SetDisabledColorAlpha;
      property FrameNormalColor: TColor read GetFrameNormalColor write SetFrameNormalColor;
      property FrameDisabledColor: TColor read GetFrameDisabledColor write SetFrameDisabledColor;
      property FrameNormalColorAlpha: Byte read FFrameNormalColorAlpha write SetFrameNormalColorAlpha;
      property FrameDisabledColorAlpha: Byte read FFrameDisabledColorAlpha write SetFrameDisabledColorAlpha;

      property CheckedNormalColor: TColor read GetCheckedNormalColor write SetCheckedNormalColor;
      property CheckedDisabledColor: TColor read GetCheckedDisabledColor write SetCheckedDisabledColor;
      property CheckedNormalColorAlpha: Byte read FCheckedNormalColorAlpha write SetCheckedNormalColorAlpha;
      property CheckedDisabledColorAlpha: Byte read FCheckedDisabledColorAlpha write SetCheckedDisabledColorAlpha;

      property CheckedFrameNormalColor: TColor read GetCheckedFrameNormalColor write SetCheckedFrameNormalColor;
      property CheckedFrameDisabledColor: TColor read GetCheckedFrameDisabledColor write SetCheckedFrameDisabledColor;
      property CheckedFrameNormalColorAlpha: Byte read FCheckedFrameNormalColorAlpha write SetCheckedFrameNormalColorAlpha;
      property CheckedFrameDisabledColorAlpha: Byte read FCheckedFrameDisabledColorAlpha write SetCheckedFrameDisabledColorAlpha;

      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

      property CheckMarkNormalColor: TColor read GetCheckMarkNormalColor write SetCheckMarkNormalColor;
      property CheckMarkDisabledColor: TColor read GetCheckMarkDisabledColor write SetCheckMarkDisabledColor;

      property CheckMarkNormalColorAlpha: Byte read FCheckMarkNormalColorAlpha write SetCheckMarkNormalColorAlpha;
      property CheckMarkDisabledColorAlpha: Byte read FCheckMarkDisabledColorAlpha write SetCheckMarkDisabledColorAlpha;

      property ShapeSize: Integer
        read FShapeSize write SetShapeSize;
      property CheckMarkThickness: Integer
        read FCheckMarkThickness write SetCheckMarkThickness;

      property ScaleCheckMarkThickness: Boolean
        read FScaleCheckMarkThickness write FScaleCheckMarkThickness;

       property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth;

      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  TscGPListBoxItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: String;
    FDetail: String;
    FEnabled: Boolean;
    FData: TCustomData;
    FHeader: Boolean;
    FChecked: Boolean;
    FIndent: Integer;
  protected
    FCustomParam: String;
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetDetail(const Value: String); virtual;
    procedure SetCaption(const Value: String); virtual;
    procedure SetData(const Value: TCustomData); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetHeader(Value: Boolean); virtual;
    procedure SetIndent(Value: Integer);
    procedure SetChecked(Value: Boolean); virtual;
    procedure SetCustomColor(Value: TColor);
    procedure SetCustomColorAlpha(Value: Byte);
    procedure SetCustomTextColor(Value: TColor);
    procedure SetCustomDetailTextColor(Value: TColor);
  public
    ItemRect: TRect;
    CheckRect: TRect;
    IsVisible: Boolean;
    IsFilterVisible: Boolean;
    Active: Boolean;
    FCustomColor: TColor;
    FCustomColorAlpha: Byte;
    FCustomTextColor: TColor;
    FCustomDetailTextColor: TColor;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Data: TCustomData read FData write SetData;
    procedure Assign(Source: TPersistent); override;
  published
    property Header: Boolean read FHeader write SetHeader;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Caption: String read FCaption write SetCaption;
    property CustomColor: TColor read FCustomColor write SetCustomColor;
    property CustomColorAlpha: Byte read FCustomColorAlpha write SetCustomColorAlpha;
    property CustomTextColor: TColor read FCustomTextColor write SetCustomTextColor;
    property CustomDetailTextColor: TColor read FCustomDetailTextColor write SetCustomDetailTextColor;
    property Detail: String read FDetail write SetDetail;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property Checked: Boolean  read FChecked write SetChecked;
    property Indent: Integer read FIndent write SetIndent default 0;
  end;

  TscGPCustomListBox = class;

  TscGPListBoxItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscGPListBoxItem;
    procedure SetItem(Index: Integer; Value:  TscGPListBoxItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    ListBox: TscGPCustomListBox;
    constructor Create(AListBox: TscGPCustomListBox);
    property Items[Index: Integer]: TscGPListBoxItem read GetItem write SetItem; default;
    function Add: TscGPListBoxItem;
    function Insert(Index: Integer): TscGPListBoxItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscGPListBoxDetailPosition = (scgplbdBottom, scgplbdRight);

  TscGPCustomListBox = class(TscGPScrollingControl)
  protected
    FAutoComplete: Boolean;
    FSearchString: String;
    FSearchTimerEnabled: Boolean;
    FIndentMargin: Integer;
    FItemMargin: Integer;
    FSelectionOptions: TscGPSelectionOptions;
    FHeaderOptions: TscGPHeaderOptions;
    FCheckBoxOptions: TscGPListCheckBoxOptions;
    FItems: TscGPListBoxItems;
    FTimerMode: Integer;
    FDetailFont: TFont;
    FDetailPosition: TscGPListBoxDetailPosition;
    FDetailWordWrap: Boolean;
    FHeaderFont: TFont;
    FLineColor: TColor;
    FLineColorAlpha: Byte;
    FCheckOffset: Integer;
    FShowCheckBoxes: Boolean;
    FInUpdateItems: Boolean;
    FMouseMoveChangeIndex: Boolean;
    FShowLines: Boolean;
    FClicksDisabled: Boolean;
    FMouseDown: Boolean;
    FMouseActive: Integer;
    FMax: Integer;
    FRealMax: Integer;
    FItemsRect: TRect;
    FScrollOffset: Integer;
    FImages: TCustomImageList;
    FShowItemDetails: Boolean;
    FItemHeight: Integer;
    FItemSpacing: Integer;
    FHeaderHeight: Integer;
    FOldHeight: Integer;
    FItemIndex: Integer;
    FItemWordWrap: Boolean;
    FItemShowEllipsis: Boolean;
    FOnItemClick: TNotifyEvent;
    FOnItemCheckClick: TNotifyEvent;
    FFilter: String;
    FPopupMode: Boolean;

    procedure SetIndentMargin(Value: Integer);
    procedure SetItemMargin(Value: Integer);

    procedure SetItemWordWrap(Value: Boolean);
    procedure SetItemShowEllipsis(Value: Boolean);

    procedure SetItems(Value: TscGPListBoxItems);
    procedure SetFilter(Value: String);
    procedure SetItemSpacing(Value: Integer);
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;
    procedure OnVertScrollBarChange(Sender: TObject);
    procedure SetHeaderFont(Value: TFont);
    procedure SetDetailFont(Value: TFont);
    procedure SetDetailPosition(Value: TscGPListBoxDetailPosition);
    procedure SetDetailWordWrap(Value: Boolean);
    procedure SkinDrawCheckImage(X, Y: Integer; Cnvs: TCanvas; IR: TRect; DestCnvs: TCanvas);
    procedure SetShowCheckBoxes(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetItemIndex(Value: Integer);
    procedure SetItemActive(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetShowItemDetails(Value: Boolean);
    procedure SetLineColor(Value: TColor);
    procedure SetLineColorAlpha(Value: Byte);
    function CanDrawContent: Boolean;
    procedure OptionsChanged(Sender: TObject);
    procedure DrawContent(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect); override;

    procedure DrawItem(Index: Integer; Cnvs: TCanvas; G: TGPGraphics); virtual;
    procedure DrawHeaderItem(Index: Integer; Cnvs: TCanvas; G: TGPGraphics);
    procedure DrawSelection(G: TGPGraphics; ARect: TRect);
    procedure DrawCheckBox(G: TGPGraphics; ARect: TRect; AChecked: Boolean; AEnabled: Boolean);

    procedure CalcItemRects;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure Loaded; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure WndProc(var Message: TMessage); override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure FindUp;
    procedure FindDown;
    procedure FindPageUp;
    procedure FindPageDown;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function CalcHeight(AItemCount: Integer): Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Change; virtual;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure GetScrollInfo; override;

    property Items:  TscGPListBoxItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemMargin: Integer read FItemMargin write SetItemMargin;
    property IndentMargin: Integer read FIndentMargin write SetIndentMargin;
    property HeaderOptions: TscGPHeaderOptions
      read FHeaderOptions write FHeaderOptions;
    property CheckBoxOptions: TscGPListCheckBoxOptions
      read  FCheckBoxOptions write FCheckBoxOptions;
    property ShowCheckBoxes: Boolean
      read FShowCheckBoxes write SetShowCheckBoxes;
    property Images: TCustomImageList read FImages write SetImages;
    property ShowItemDetails: Boolean
      read FShowItemDetails write SetShowItemDetails;
    property ItemSpacing: Integer
      read FItemSpacing write SetItemSpacing;
    property ItemHeight: Integer
      read FItemHeight write SetItemHeight;
    property HeaderHeight: Integer
      read FHeaderHeight write SetHeaderHeight;
    property ItemWordWrap: Boolean
      read FItemWordWrap write SetItemWordWrap;
    property ItemShowEllipsis: Boolean
      read FItemShowEllipsis write SetItemShowEllipsis;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollToItem(Index: Integer);
    procedure Scroll(AScrollOffset: Integer);
    procedure BeginUpdateItems;
    procedure EndUpdateItems;

    procedure Add(const Item: String); overload;
    procedure Add(Items: TStrings); overload;
    procedure Delete(Index: Integer);
    procedure Clear;

    function ItemAtPos(X, Y: Integer): Integer;
    function NextIndex(const S: string): Integer;
    function IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
    function IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
    procedure InitItemIndex(Value: Integer);
    procedure Sort;
    property Filter: String read FFilter write SetFilter;
  published
    property AutoComplete: Boolean
      read FAutoComplete write FAutoComplete;
    property DrawTextMode;
    property SelectionOptions: TscGPSelectionOptions
      read FSelectionOptions write FSelectionOptions;
    property ShowLines: Boolean read FShowLines write SetShowLines;
    property LineColor: TColor read FLineColor write SetLineColor;
    property LineColorAlpha: Byte read FLineColorAlpha write SetLineColorAlpha;
    property MouseMoveChangeIndex: Boolean
      read FMouseMoveChangeIndex write FMouseMoveChangeIndex;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property DetailFont: TFont read FDetailFont write SetDetailFont;
    property DetailPosition: TscGPListBoxDetailPosition
      read FDetailPosition write SetDetailPosition;
    property DetailWordWrap: Boolean
      read FDetailWordWrap write SetDetailWordWrap;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property TabStop;
    property Font;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnItemClick: TNotifyEvent
      read FOnItemClick write FOnItemClick;
    property OnItemCheckClick: TNotifyEvent
      read FOnItemCheckClick write FOnItemCheckClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPListBox = class(TscGPCustomListBox)
  published
    property Items;
    property ItemIndex;
    property ItemMargin;
    property ItemWordWrap;
    property ItemShowEllipsis;
    property IndentMargin;
    property CheckBoxOptions;
    property ShowCheckBoxes;
    property Images;
    property ShowItemDetails;
    property ItemSpacing;
    property ItemHeight;
    property HeaderOptions;
    property HeaderHeight;
  end;

  TscGPPopupListBox = class(TscGPCustomListBox)
  private
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property Items;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Hide;
    procedure Show(Origin: TPoint);
  end;

  TscGPComboShapeStyle = (scgpcssRect, scgpcssRoundedRect, scgpcssRoundedLeftRight,
    scgpcssLine, scgpcssNone);

   TscGPComboOptions = class(TPersistent)
    private
      FNormalColor: TColor;
      FHotColor: TColor;
      FPressedColor: TColor;
      FFocusedColor: TColor;
      FDisabledColor: TColor;
      FFrameNormalColor: TColor;
      FFrameHotColor: TColor;
      FFramePressedColor: TColor;
      FFrameFocusedColor: TColor;
      FFrameDisabledColor: TColor;
      FFrameWidth: Integer;
      FFontNormalColor: TColor;
      FFontHotColor: TColor;
      FFontPressedColor: TColor;
      FFontFocusedColor: TColor;
      FFontDisabledColor: TColor;

      FNormalColorAlpha: Byte;
      FHotColorAlpha: Byte;
      FPressedColorAlpha: Byte;
      FFocusedColorAlpha: Byte;
      FDisabledColorAlpha: Byte;

      FFrameNormalColorAlpha: Byte;
      FFrameHotColorAlpha: Byte;
      FFramePressedColorAlpha: Byte;
      FFrameFocusedColorAlpha: Byte;
      FFrameDisabledColorAlpha: Byte;

      FStyleColors: Boolean;
      FState: TscsCtrlState;
      FOnChange: TNotifyEvent;

      FShapeCornerRadius: Integer;
      FShapeStyle: TscGPComboShapeStyle;
      FShapeFillStyle: TscGPShapeFillStyle;
      FShapeFillGradientAngle: Integer;
      FShapeFillGradientPressedAngle: Integer;

      FArrowSize: Integer;
      FArrowAreaWidth: Integer;

      FScaleFrameWidth: Boolean;

      procedure SetArrowAreaWidth(Value: Integer);
      procedure SetArrowSize(Value: Integer);
      procedure SetShapeFillStyle(Value: TscGPShapeFillStyle);
      procedure SetShapeFillGradientAngle(Value: Integer);
      procedure SetShapeFillGradientPressedAngle(Value: Integer);

      procedure SetShapeStyle(Value: TscGPComboShapeStyle);
      procedure SetShapeCornerRadius(Value: Integer);

      function GetNormalColor: TColor;
      function GetHotColor: TColor;
      function GetPressedColor: TColor;
      function GetFocusedColor: TColor;
      function GetDisabledColor: TColor;

      function GetFrameNormalColor: TColor;
      function GetFrameHotColor: TColor;
      function GetFramePressedColor: TColor;
      function GetFrameFocusedColor: TColor;
      function GetFrameDisabledColor: TColor;

      function GetFontNormalColor: TColor;
      function GetFontHotColor: TColor;
      function GetFontPressedColor: TColor;
      function GetFontFocusedColor: TColor;
      function GetFontDisabledColor: TColor;

      function GetColor: TColor;
      function GetFrameColor: TColor;
      function GetFontColor: TColor;
      function GetColorAlpha: Byte;
      function GetFrameColorAlpha: Byte;

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

      procedure SetFrameNormalColor(Value: TColor);
      procedure SetFrameHotColor(Value: TColor);
      procedure SetFramePressedColor(Value: TColor);
      procedure SetFrameFocusedColor(Value: TColor);
      procedure SetFrameDisabledColor(Value: TColor);

      procedure SetFrameNormalColorAlpha(Value: Byte);
      procedure SetFrameHotColorAlpha(Value: Byte);
      procedure SetFramePressedColorAlpha(Value: Byte);
      procedure SetFrameFocusedColorAlpha(Value: Byte);
      procedure SetFrameDisabledColorAlpha(Value: Byte);

      procedure SetFontNormalColor(Value: TColor);
      procedure SetFontHotColor(Value: TColor);
      procedure SetFontPressedColor(Value: TColor);
      procedure SetFontFocusedColor(Value: TColor);
      procedure SetFontDisabledColor(Value: TColor);

      procedure SetFrameWidth(Value: Integer);
      procedure SetStyleColors(Value: Boolean);

      procedure Changed;
    public
      constructor Create; virtual;
      procedure Assign(Source: TPersistent); override;

      property State: TscsCtrlState read FState write FState;

      property Color: TColor read GetColor;
      property FrameColor: TColor read GetFrameColor;
      property FontColor: TColor read GetFontColor;
      property ColorAlpha: Byte read GetColorAlpha;
      property FrameColorAlpha: Byte read GetFrameColorAlpha;
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

      property FrameNormalColor: TColor read GetFrameNormalColor write SetFrameNormalColor;
      property FrameHotColor: TColor read GetFrameHotColor write SetFrameHotColor;
      property FramePressedColor: TColor read GetFramePressedColor write SetFramePressedColor;
      property FrameFocusedColor: TColor read GetFrameFocusedColor write SetFrameFocusedColor;
      property FrameDisabledColor: TColor read GetFrameDisabledColor write SetFrameDisabledColor;
      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

      property FrameNormalColorAlpha: Byte read FFrameNormalColorAlpha write SetFrameNormalColorAlpha;
      property FrameHotColorAlpha: Byte read FFrameHotColorAlpha write SetFrameHotColorAlpha;
      property FramePressedColorAlpha: Byte read FFramePressedColorAlpha write SetFramePressedColorAlpha;
      property FrameFocusedColorAlpha: Byte read FFrameFocusedColorAlpha write SetFrameFocusedColorAlpha;
      property FrameDisabledColorAlpha: Byte read FFrameDisabledColorAlpha write SetFrameDisabledColorAlpha;

      property FontNormalColor: TColor read GetFontNormalColor write SetFontNormalColor;
      property FontHotColor: TColor read GetFontHotColor write SetFontHotColor;
      property FontPressedColor: TColor read GetFontPressedColor write SetFontPressedColor;
      property FontFocusedColor: TColor read GetFontFocusedColor write SetFontFocusedColor;
      property FontDisabledColor: TColor read GetFontDisabledColor write SetFontDisabledColor;

      property ShapeFillStyle: TscGPShapeFillStyle
        read FShapeFillStyle write SetShapeFillStyle default scgpsfColor;
      property ShapeFillGradientAngle: Integer
        read FShapeFillGradientAngle write SetShapeFillGradientAngle;
      property ShapeFillGradientPressedAngle: Integer
        read FShapeFillGradientPressedAngle write SetShapeFillGradientPressedAngle;

      property ShapeCornerRadius: Integer
        read FShapeCornerRadius write SetShapeCornerRadius;
      property ShapeStyle: TscGPComboShapeStyle
        read FShapeStyle write SetShapeStyle;

      property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth;

      property ArrowSize: Integer
        read FArrowSize write SetArrowSize;
      property ArrowAreaWidth: Integer
        read FArrowAreaWidth write SetArrowAreaWidth;

      property StyleColors: Boolean read FStyleColors write SetStyleColors;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  TscGPCustomCombo = class(TscCustomActiveControl)
  private
    FOptions: TscGPComboOptions;
    FDropDownPosition: TscDropDownPosition;
    FContentMargin: Integer;
    procedure SetContentMargin(Value: Integer);
    procedure OnOptionsChange(Sender: TObject);
  protected
    FDropDown: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DrawComboItem(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect); virtual;
    function CanDrawItem: Boolean; virtual;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    property Options: TscGPComboOptions read FOptions write FOptions;
    property DropDownPosition: TscDropDownPosition
     read FDropDownPosition write FDropDownPosition default scdpRight;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ContentMargin: Integer
      read FContentMargin write SetContentMargin;
  end;

  TscGPCustomComboBox = class(TscGPCustomCombo)
  protected
    FAutoComplete: Boolean;
    FSearchString: String;
    FSearchTimerEnabled: Boolean;
    FShowItemImage: Boolean;
    FShowItemText: Boolean;
    FShowItemDetail: Boolean;
    FDropDownCount: Integer;
    WasInLB: Boolean;
    FTimerMode: Integer;
    FListBoxWidth: Integer;
    FListBoxHeight: Integer;
    FLastTime: Cardinal;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOldItemIndex: Integer;
    FLBDown: Boolean;
    FListBox: TscGPPopupListBox;
    FListBoxWindowProc: TWndMethod;
    FCheckedListMode: Boolean;
    FCheckedListWrap: Boolean;
    FOnItemCheckClick: TNotifyEvent;
    FMouseWheelSupport: Boolean;

    function GetItemWordWrap: Boolean;
    procedure SetItemWordWrap(Value: Boolean);
    function GetItemShowEllipsis: Boolean;
    procedure SetItemShowEllipsis(Value: Boolean);

    procedure SetCheckedListWrap(Value: Boolean);
    procedure SetCheckedListMode(Value: Boolean);
    function CanAnimate: Boolean; override;
    procedure SetShowItemDetail(Value: Boolean);
    procedure SetShowItemImage(Value: Boolean);
    procedure SetShowItemText(Value: Boolean);
    procedure ListBoxWindowProcHook(var Message: TMessage);
    procedure ProcessListBox;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
    procedure CheckButtonClick(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TscGPListBoxItems);
    function GetItems: TscGPListBoxItems;
    procedure SetDropDownCount(Value: Integer);
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure WMSETFOCUS(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMMouseHookCancelMode(var Message: TMessage); message WM_MOUSEHOOKCANCELMODE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure DrawComboItem(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect); override;
    function CanDrawItem: Boolean; override;
    procedure Change; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ComboKeyUp(AChange: Boolean);
    procedure ComboKeyDown(AChange: Boolean);
    procedure ComboPageUp(AChange: Boolean);
    procedure ComboPageDown(AChange: Boolean);

    function GetDetailPosition: TscGPListBoxDetailPosition;
    procedure SetDetailPosition(Value: TscGPListBoxDetailPosition);
    function GetDetailWordWrap: Boolean;
    procedure SetDetailWordWrap(Value: Boolean);

    function GetListBoxHeaderOptions: TscGPHeaderOptions;
    procedure SetListBoxHeaderOptions(Value: TscGPHeaderOptions);

    function GetListBoxSelectionOptions: TscGPSelectionOptions;
    procedure SetListBoxSelectionOptions(Value: TscGPSelectionOptions);

    function GetListBoxOptions: TscGPScrollingControlOptions;
    procedure SetListBoxOptions(Value: TscGPScrollingControlOptions);

    function GetListBoxScrollBarOptions: TscGPControlScrollBarOptions;
    procedure SetListBoxScrollBarOptions(Value: TscGPControlScrollBarOptions);

    function GetListBoxHeaderFont: TFont;
    procedure SetListBoxHeaderFont(Value: TFont);

    function GetDetailFont: TFont;
    procedure SetDetailFont(Value: TFont);

    function GetListBoxItemSpacing: Integer;
    procedure SetListBoxItemSpacing(Value: Integer);

    function GetListBoxItemMargin: Integer;
    procedure SetListBoxItemMargin(Value: Integer);

    function GetListBoxIndentMargin: Integer;
    procedure SetListBoxIndentMargin(Value: Integer);

    function GetListBoxLineColor: TColor;
    procedure SetListBoxLineColor(Value: TColor);

    function GetListBoxLineColorAlpha: TColor;
    procedure SetListBoxLineColorAlpha(Value: TColor);

    function GetListBoxShowLines: Boolean;
    procedure SetListBoxShowLines(Value: Boolean);
    function GetListBoxItemHeight: Integer;
    procedure SetListBoxItemHeight(Value: Integer);
    function GetListBoxHeaderHeight: Integer;
    procedure SetListBoxHeaderHeight(Value: Integer);
    function GetListBoxShowItemDetails: Boolean;
    procedure SetListBoxShowItemDetails(Value: Boolean);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort;
    procedure BeginUpdateItems;
    procedure EndUpdateItems;

    procedure Add(const Item: String); overload;
    procedure Add(Items: TStrings); overload;
    procedure Delete(Index: Integer);
    procedure Clear;

    function IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
    function IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
    function NextIndex(const S: string): Integer;
    procedure InitItemIndex(Value: Integer);
    procedure CloseUp(Value: Boolean);
    procedure DropDown; virtual;
    function IsPopupVisible: Boolean;
    function CanCancelDropDown: Boolean;

    property AutoComplete: Boolean
      read FAutoComplete write FAutoComplete;
    property CheckedListMode: Boolean
     read FCheckedListMode write SetCheckedListMode;
    property CheckedListWrap: Boolean
      read FCheckedListWrap write SetCheckedListWrap;

    property ShowItemImage: Boolean read FShowItemImage write SetShowItemImage;
    property ShowItemText: Boolean read FShowItemText write SetShowItemText;
    property ShowItemDetail: Boolean read FShowItemDetail write SetShowItemDetail;

    property ListBoxLineColor: TColor
      read GetListBoxLineColor write SetListBoxLineColor;
    property ListBoxLineColorAlpha: TColor
      read GetListBoxLineColorAlpha write SetListBoxLineColorAlpha;

    property ListBoxWidth: Integer read FListBoxWidth write FListBoxWidth;
    property ListBoxHeight: Integer read FListBoxHeight write FListBoxHeight;

    property ListBoxIndentMargin: Integer
      read GetListBoxIndentMargin write SetListBoxIndentMargin;

    property ListBoxItemSpacing: Integer
      read GetListBoxItemSpacing write SetListBoxItemSpacing;


    property ListBoxItemMargin: Integer
      read GetListBoxItemMargin write SetListBoxItemMargin;

    property ListBoxShowItemDetails: Boolean
      read GetListBoxShowItemDetails write SetListBoxShowItemDetails;

    property ListBoxShowLines: Boolean
      read GetListBoxShowLines write SetListBoxShowLines;
    property ListBoxItemHeight: Integer
      read GetListBoxItemHeight write SetListBoxItemHeight;
    property ListBoxHeaderHeight: Integer
      read GetListBoxHeaderHeight write SetListBoxHeaderHeight;

    property ListBoxHeaderOptions: TscGPHeaderOptions
      read GetListBoxHeaderOptions write SetListBoxHeaderOptions;

    property ListBoxSelectionOptions: TscGPSelectionOptions
      read GetListBoxSelectionOptions write SetListBoxSelectionOptions;

    property ListBoxOptions: TscGPScrollingControlOptions
      read GetListBoxOptions write SetListBoxOptions;

     property ListBoxScrollBarOptions: TscGPControlScrollBarOptions
      read GetListBoxScrollBarOptions write SetListBoxScrollBarOptions;

    property ListBox: TscGPPopupListBox read
      FListBox;

    property DetailPosition: TscGPListBoxDetailPosition
      read GetDetailPosition write SetDetailPosition;

    property DetailWordWrap: Boolean
      read GetDetailWordWrap write SetDetailWordWrap;

    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Align;
    property Items: TscGPListBoxItems read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;

    property ItemWordWrap: Boolean read
      GetItemWordWrap write SetItemWordWrap;
    property ItemShowEllipsis: Boolean read
      GetItemShowEllipsis write SetItemShowEllipsis;

    property Images: TCustomImageList read GetImages write SetImages;
    property ListBoxHeaderFont: TFont
      read GetListBoxHeaderFont write SetListBoxHeaderFont;
    property DetailFont: TFont
      read GetDetailFont write SetDetailFont;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;

    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
     property OnItemCheckClick: TNotifyEvent
      read FOnItemCheckClick write FOnItemCheckClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
  end;

  TscGPComboBox = class(TscGPCustomComboBox)
  published
    property AutoComplete;
    property Animation;
    property CheckedListMode;
    property CheckedListWrap;
    property DetailPosition;
    property DetailWordWrap;
    property DrawTextMode;
    property Images;
    property Items;
    property ItemIndex;
    property ItemWordWrap;
    property ItemShowEllipsis;
    property DropDownCount;
    property DropDownPosition;
    property Options;
    property ShowItemImage;
    property ShowItemText;
    property ShowItemDetail;
    property ListBoxHeaderFont;
    property ListBoxHeaderOptions;
    property DetailFont;
    property ListBoxLineColor;
    property ListBoxWidth;
    property ListBoxHeight;
    property ListBoxShowItemDetails;
    property ListBoxShowLines;
    property ListBoxItemHeight;
    property ListBoxHeaderHeight;
    property ListBoxItemSpacing;
    property ListBoxItemMargin;
    property ListBoxIndentMargin;
    property ListBoxScrollBarOptions;
    property ListBoxOptions;

    property MouseWheelSupport;

    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    property Align;
    property Font;
    property Color;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnDropDown;
    property OnItemCheckClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
  end;


   TscGPEditShapeStyle = (scgpessRect, scgpessRoundedRect, scgpessRoundedLeftRight,
    scgpessLine, scgpessNone);

   TscGPEditOptions = class(TPersistent)
    private
      FNormalColor: TColor;
      FHotColor: TColor;
      FFocusedColor: TColor;
      FDisabledColor: TColor;
      FFrameNormalColor: TColor;
      FFrameHotColor: TColor;
      FFrameFocusedColor: TColor;
      FFrameDisabledColor: TColor;
      FFrameWidth: Integer;
      FFontNormalColor: TColor;
      FFontHotColor: TColor;
      FFontFocusedColor: TColor;
      FFontDisabledColor: TColor;

      FNormalColorAlpha: Byte;
      FHotColorAlpha: Byte;
      FFocusedColorAlpha: Byte;
      FDisabledColorAlpha: Byte;

      FFrameNormalColorAlpha: Byte;
      FFrameHotColorAlpha: Byte;
      FFrameFocusedColorAlpha: Byte;
      FFrameDisabledColorAlpha: Byte;

      FStyleColors: Boolean;
      FState: TscsCtrlState;
      FOnChange: TNotifyEvent;

      FShapeCornerRadius: Integer;
      FShapeStyle: TscGPEditShapeStyle;
      FShapeFillStyle: TscGPShapeFillStyle;
      FShapeFillGradientAngle: Integer;

      FScaleFrameWidth: Boolean;

      procedure SetShapeFillStyle(Value: TscGPShapeFillStyle);
      procedure SetShapeFillGradientAngle(Value: Integer);

      procedure SetShapeStyle(Value: TscGPEditShapeStyle);
      procedure SetShapeCornerRadius(Value: Integer);

      function GetNormalColor: TColor;
      function GetHotColor: TColor;
      function GetFocusedColor: TColor;
      function GetDisabledColor: TColor;

      function GetFrameNormalColor: TColor;
      function GetFrameHotColor: TColor;
      function GetFrameFocusedColor: TColor;
      function GetFrameDisabledColor: TColor;

      function GetFontNormalColor: TColor;
      function GetFontHotColor: TColor;
      function GetFontFocusedColor: TColor;
      function GetFontDisabledColor: TColor;

      function GetColor: TColor;
      function GetFrameColor: TColor;
      function GetFontColor: TColor;
      function GetColorAlpha: Byte;
      function GetFrameColorAlpha: Byte;

      procedure SetNormalColor(Value: TColor);
      procedure SetHotColor(Value: TColor);
      procedure SetFocusedColor(Value: TColor);
      procedure SetDisabledColor(Value: TColor);

      procedure SetNormalColorAlpha(Value: Byte);
      procedure SetHotColorAlpha(Value: Byte);
      procedure SetFocusedColorAlpha(Value: Byte);
      procedure SetDisabledColorAlpha(Value: Byte);

      procedure SetFrameNormalColor(Value: TColor);
      procedure SetFrameHotColor(Value: TColor);
      procedure SetFrameFocusedColor(Value: TColor);
      procedure SetFrameDisabledColor(Value: TColor);

      procedure SetFrameNormalColorAlpha(Value: Byte);
      procedure SetFrameHotColorAlpha(Value: Byte);
      procedure SetFrameFocusedColorAlpha(Value: Byte);
      procedure SetFrameDisabledColorAlpha(Value: Byte);

      procedure SetFontNormalColor(Value: TColor);
      procedure SetFontHotColor(Value: TColor);
      procedure SetFontFocusedColor(Value: TColor);
      procedure SetFontDisabledColor(Value: TColor);

      procedure SetFrameWidth(Value: Integer);
      procedure SetStyleColors(Value: Boolean);

      procedure Changed;
    public
      constructor Create; virtual;
      procedure Assign(Source: TPersistent); override;

      property State: TscsCtrlState read FState write FState;

      property Color: TColor read GetColor;
      property FrameColor: TColor read GetFrameColor;
      property FontColor: TColor read GetFontColor;
      property ColorAlpha: Byte read GetColorAlpha;
      property FrameColorAlpha: Byte read GetFrameColorAlpha;
    published
      property NormalColor: TColor read GetNormalColor write SetNormalColor;
      property HotColor: TColor read GetHotColor write SetHotColor;
      property FocusedColor: TColor read GetFocusedColor write SetFocusedColor;
      property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;

      property NormalColorAlpha: Byte read FNormalColorAlpha write SetNormalColorAlpha;
      property HotColorAlpha: Byte read FHotColorAlpha write SetHotColorAlpha;
      property FocusedColorAlpha: Byte read FFocusedColorAlpha write SetFocusedColorAlpha;
      property DisabledColorAlpha: Byte read FDisabledColorAlpha write SetDisabledColorAlpha;

      property FrameNormalColor: TColor read GetFrameNormalColor write SetFrameNormalColor;
      property FrameHotColor: TColor read GetFrameHotColor write SetFrameHotColor;
      property FrameFocusedColor: TColor read GetFrameFocusedColor write SetFrameFocusedColor;
      property FrameDisabledColor: TColor read GetFrameDisabledColor write SetFrameDisabledColor;
      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

      property FrameNormalColorAlpha: Byte read FFrameNormalColorAlpha write SetFrameNormalColorAlpha;
      property FrameHotColorAlpha: Byte read FFrameHotColorAlpha write SetFrameHotColorAlpha;
      property FrameFocusedColorAlpha: Byte read FFrameFocusedColorAlpha write SetFrameFocusedColorAlpha;
      property FrameDisabledColorAlpha: Byte read FFrameDisabledColorAlpha write SetFrameDisabledColorAlpha;

      property FontNormalColor: TColor read GetFontNormalColor write SetFontNormalColor;
      property FontHotColor: TColor read GetFontHotColor write SetFontHotColor;
      property FontFocusedColor: TColor read GetFontFocusedColor write SetFontFocusedColor;
      property FontDisabledColor: TColor read GetFontDisabledColor write SetFontDisabledColor;

      property ShapeFillStyle: TscGPShapeFillStyle
        read FShapeFillStyle write SetShapeFillStyle default scgpsfColor;
      property ShapeFillGradientAngle: Integer
        read FShapeFillGradientAngle write SetShapeFillGradientAngle;

      property ShapeCornerRadius: Integer
        read FShapeCornerRadius write SetShapeCornerRadius;
      property ShapeStyle: TscGPEditShapeStyle
        read FShapeStyle write SetShapeStyle;

      property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth;

      property StyleColors: Boolean read FStyleColors write SetStyleColors;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  TscGPEditButtonKind =
    (scgpebCustom, scgpebDropDown, scgpebClear,
     scgpebMore, scgpebDetails,
     scgpebLeftArrow, scgpebRightArrow,
     scgpebUpArrow, scgpebDownArrow,
     scgpebNext, scgpebPrior, scgpebSearch,
     scgpebRefresh, scgpebMic);

  TscGPEditButton = class(TPersistent)
  private
    FRepeatClick: Boolean;
    FRepeatClickInterval: Integer;
    FEnabled: Boolean;
    FVisible: Boolean;
    FImageIndex: Integer;
    FImageHotIndex: Integer;
    FImagePressedIndex: Integer;
    FOnChange: TNotifyEvent;
    FOnVisibleChange: TNotifyEvent;
    FDropDownMenu: TPopupMenu;
    FKind: TscGPEditButtonKind;
    FGlyphColor: TColor;
    FGlyphColorAlpha: Byte;
    FGlyphColorHotAlpha: Byte;
    FGlyphColorPressedAlpha: Byte;
    FGlyphThickness: Byte;
    FGlyphSize: Integer;
    FHint: String;
    FShowHint: Boolean;
    FWidth: Integer;

    procedure SetWidth(Value: Integer);
    procedure SetGlyphSize(Value: Integer);
    procedure SetGlyphColor(Value: TColor);
    procedure SetGlyphColorAlpha(Value: Byte);
    procedure SetGlyphThickness(Value: Byte);
    procedure SetKind(Value: TscGPEditButtonKind);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure Changed;
  public
    FDropDown: Boolean;
    ButtonRect: TRect;
    MouseIn: Boolean;
    Down: Boolean;
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Kind: TscGPEditButtonKind
      read FKind write SetKind;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property ImageHotIndex: Integer read FImageHotIndex write FImageHotIndex;
    property ImagePressedIndex: Integer read FImagePressedIndex write FImagePressedIndex;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property RepeatClick: Boolean read FRepeatClick write FRepeatClick;
    property RepeatClickInterval: Integer read FRepeatClickInterval write FRepeatClickInterval;
    property Hint: String read FHint write FHint;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property GlyphColor: TColor
      read FGlyphColor write SetGlyphColor;
    property GlyphColorAlpha: Byte
      read FGlyphColorAlpha write SetGlyphColorAlpha;
    property GlyphColorHotAlpha: Byte
      read FGlyphColorHotAlpha write FGlyphColorHotAlpha;
    property GlyphColorPressedAlpha: Byte
      read FGlyphColorPressedAlpha write FGlyphColorPressedAlpha;
    property GlyphThickness: Byte
      read FGlyphThickness write SetGlyphThickness;
    property GlyphSize: Integer
      read FGlyphSize write SetGlyphSize;
    property Width: Integer
      read FWidth write SetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnVisibleChange: TNotifyEvent read FOnVisibleChange write FOnVisibleChange;
  end;

  TscGPCustomEdit = class(TCustomMaskEdit)
  private
   {$IFDEF VER230}
    FStyleElements: TStyleElements;
   {$ENDIF}
    FHintComponent: TscHint;
    FButtonImages: TCustomImageList;
    FLeftButton: TscGPEditButton;
    FRightButton: TscGPEditButton;
    FOptions: TscGPEditOptions;
    FRepeatClickTimer: TTimer;
    FTextColor: TColor;
    FMouseIn: Boolean;
    FTransparent: Boolean;
    ParentBGBuffer: TBitmap;
    FFrameColor: TColor;
    FFrameActiveColor: TColor;
    FContentMarginLeft: Integer;
    FContentMarginRight: Integer;
    FContentMarginTop: Integer;
    FContentMarginBottom: Integer;
    FPromptText: String;
    FPromptTextColor: TColor;
    FHidePromptTextIfFocused: Boolean;
    FOnLeftButtonClick: TNotifyEvent;
    FOnRightButtonClick: TNotifyEvent;
    FGlyphSize: Integer;
    FOnDrawBackgroundEvent: TscPaintControlEvent;
    FCustomDraw: Boolean;
    // Windows 10 Fluient UI
    FFluentUIOpaque: Boolean;
    procedure SetFluentUIOpaque(Value: Boolean);
    //
    procedure SetButtonImages(Value: TCustomImageList);
    procedure SetTransparent(Value: Boolean);
    procedure SetPromptText(Value: String);
    procedure SetPromptTextColor(Value: TColor);
    procedure SetContentMarginLeft(Value: Integer);
    procedure SetContentMarginTop(Value: Integer);
    procedure SetContentMarginRight(Value: Integer);
    procedure SetContentMarginBottom(Value: Integer);
  protected
    FStopDraw: Boolean;
    FInheritedKeys: Boolean;
    FStopGetParentBG: Boolean;
    FMenuTracking: Boolean;
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    procedure RepeatClickTimerProc(Sender: TObject);
    procedure WaitClickTimerProc(Sender: TObject);
    procedure StartRepeatClick;
    procedure StopRepeatClick;

    function IsFluentUIOpaque: Boolean;

    procedure OptionsChanged(Sender: TObject);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure DrawEditBackground(ACanvas: TCanvas);
    procedure DrawEditShape(ACanvas: TCanvas);
    procedure DrawEditButton(ACanvas: TCanvas; G: TGPGraphics; AButton: TscGPEditButton);
    function IsCustomDraw(ADC: HDC): Boolean; virtual;
    function GetTextColor: TColor;
    procedure DrawPromptText(ACanvas: TCanvas);
    function GetTextRect: TRect; virtual;
    procedure AdjustHeight;
    function GetEditHeight: Integer;
    procedure AdjustTextRect;
    procedure GetParentBG;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSetFocus(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure WMCHAR(var Message:TWMCHAR); message WM_CHAR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
    procedure WMNCLBUTTONDOWN(var Message: TWMNCLBUTTONDOWN); message WM_NCLBUTTONDOWN;
    procedure WMNCLBUTTONDBCLK(var Message: TWMNCLBUTTONDOWN); message WM_NCLBUTTONDBLCLK;
    procedure WMNCLBUTTONUP(var Message: TWMNCLBUTTONUP); message WM_NCLBUTTONUP;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure WndProc(var Message: TMessage); override;
    procedure WMNCHITTEST(var Message: TWMNCHITTEST); message WM_NCHITTEST;
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoPaint;
    procedure DoPaint2(DC: HDC);
    procedure EditMouseEnter;
    procedure EditMouseLeave;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Change; override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure OnButtonChange(Sender: TObject);
    procedure OnButtonVisibleChange(Sender: TObject);
    property LeftButton: TscGPEditButton read FLeftButton write FLeftButton;
    property RightButton: TscGPEditButton read FRightButton write FRightButton;
    property ButtonImages: TCustomImageList read FButtonImages write SetButtonImages;
    property CustomDraw: Boolean read FCustomDraw write FCustomDraw;
    property OnLeftButtonClick: TNotifyEvent
      read FOnLeftButtonClick write FOnLeftButtonClick;
    property OnRightButtonClick: TNotifyEvent
      read FOnRightButtonClick write FOnRightButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintToDC(DC: HDC; X, Y: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property AutoSize;
    property FluentUIOpaque: Boolean
      read FFluentUIOpaque write SetFluentUIOpaque;
    {$IFDEF VER230}
    property StyleElements: TStyleElements
      read  FStyleElements write FStyleElements;
    {$ENDIF}
    property HintComponent: TscHint
      read FHintComponent write FHintComponent;
    property Options: TscGPEditOptions
      read FOptions write FOptions;
    property ContentMarginLeft: Integer read FContentMarginLeft write SetContentMarginLeft;
    property ContentMarginRight: Integer read FContentMarginRight write SetContentMarginRight;
    property ContentMarginTop: Integer read FContentMarginTop write SetContentMarginTop;
    property ContentMarginBottom: Integer read FContentMarginBottom write SetContentMarginBottom;
    property PromptText: String read FPromptText write SetPromptText;
    property HidePromptTextIfFocused: Boolean
      read FHidePromptTextIfFocused write FHidePromptTextIfFocused;
    property PromptTextColor: TColor read FPromptTextColor write SetPromptTextColor;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property OnDrawBackground: TscPaintControlEvent
      read FOnDrawBackgroundEvent write FOnDrawBackgroundEvent;
  end;

 TscGPEdit = class(TscGPCustomEdit)
 published
    property LeftButton;
    property RightButton;
    property ButtonImages;
    property CustomDraw;
    property Transparent;
    property EditMask;
    property Text;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumbersOnly;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TextHint;
    property Touch;
    property Visible;
    property StyleElements;
    property OnLeftButtonClick;
    property OnRightButtonClick;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPNumericEdit = class(TscGPCustomEdit)
  private
    FDisplayType: TscNumEditDisplayType;
    StopCheck, FromEdit: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FDecimal: Byte;
    FValueType: TscValueType;
    FIncrement: Double;
    FSupportUpDownKeys: Boolean;
    FSupportMouseWheel: Boolean;
    FCurrencyString: String;
    FDisplayFormat: String;
    procedure SetDisplayType(Value: TscNumEditDisplayType);
    procedure SetValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetDecimal(NewValue: Byte);
    procedure SetValueType(NewType: TscValueType);
    procedure SetCurrencyString(Value: String);
    procedure SetDisplayFormat(Value: String);
  protected
    function CheckValue(NewValue: Double): Double;
    function GetValueAsInt: Integer;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function IsValidChar(Key: Char): Boolean;
    procedure Change; override;
    property Text;
    procedure WMKILLFOCUS(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    function IsCustomDraw(ADC: HDC): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsNumText(AText: String): Boolean;
    property ValueAsInt: Integer read GetValueAsInt;
  published
    property CurrencyString: String
      read FCurrencyString write SetCurrencyString;
    property DisplayFormat: String
      read  FDisplayFormat write SetDisplayFormat;
    property Increment: Double read FIncrement write FIncrement;
    property SupportUpDownKeys: Boolean
      read FSupportUpDownKeys write FSupportUpDownKeys;
    property SupportMouseWheel: Boolean
      read FSupportMouseWheel write FSupportMouseWheel;
    property Alignment;
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property ValueType: TscValueType read FValueType write SetValueType;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property DisplayType: TscNumEditDisplayType
      read FDisplayType write SetDisplayType;
    property LeftButton;
    property RightButton;
    property Transparent;
    property ButtonImages;
    property EditMask;
    property Align;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumbersOnly;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TextHint;
    property Touch;
    property Visible;
    property StyleElements;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnLeftButtonClick;
    property OnRightButtonClick;
  end;

  TscGPComboEdit = class(TscGPCustomEdit)
  protected
    FIsModified: Boolean;
    FDropDownPosition: TscDropDownPosition;
    FListBoxWallpapers: TscCustomImageCollection;
    FListBoxWallpaperIndex: Integer;
    FShowItemTitle: Boolean;
    FShowItemImage: Boolean;
    FShowItemText: Boolean;
    FDropDownCount: Integer;
    WasInLB: Boolean;
    TimerMode: Integer;
    FListBoxWidth: Integer;
    FListBoxHeight: Integer;
    FHideSelection: Boolean;
    FLastTime: Cardinal;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOldItemIndex: Integer;
    FLBDown: Boolean;
    FListBox: TscGPPopupListBox;
    FListBoxWindowProc: TWndMethod;
    FUseFilter: Boolean;
    FStopUseFilter: Boolean;
    FMouseWheelSupport: Boolean;
    procedure RightButtonClick(Sender: TObject);
    procedure ListBoxWindowProcHook(var Message: TMessage);
    procedure ProcessListBox;
    procedure StartTimer;
    procedure StopTimer;
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
    procedure CheckButtonClick(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TscGPListBoxItems);
    function GetItems: TscGPListBoxItems;
    procedure SetDropDownCount(Value: Integer);
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure WMSETFOCUS(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure WMMouseHookCancelMode(var Message: TMessage); message WM_MOUSEHOOKCANCELMODE;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

     procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure Change; override;
    procedure ComboKeyUp(AChange: Boolean);
    procedure ComboKeyDown(AChange: Boolean);
    procedure ComboPageUp(AChange: Boolean);
    procedure ComboPageDown(AChange: Boolean);

    function GetListBoxItemWordWrap: Boolean;
    procedure SetListBoxItemWordWrap(Value: Boolean);
    function GetListBoxItemShowEllipsis: Boolean;
    procedure SetListBoxItemShowEllipsis(Value: Boolean);

    function GetListBoxHeaderOptions: TscGPHeaderOptions;
    procedure SetListBoxHeaderOptions(Value: TscGPHeaderOptions);

    function GetListBoxSelectionOptions: TscGPSelectionOptions;
    procedure SetListBoxSelectionOptions(Value: TscGPSelectionOptions);

    function GetListBoxOptions: TscGPScrollingControlOptions;
    procedure SetListBoxOptions(Value: TscGPScrollingControlOptions);

    function GetListBoxScrollBarOptions: TscGPControlScrollBarOptions;
    procedure SetListBoxScrollBarOptions(Value: TscGPControlScrollBarOptions);

    function GetListBoxHeaderFont: TFont;
    procedure SetListBoxHeaderFont(Value: TFont);

    function GetListBoxDetailFont: TFont;
    procedure SetListBoxDetailFont(Value: TFont);

    function GetListBoxItemSpacing: Integer;
    procedure SetListBoxItemSpacing(Value: Integer);

    function GetListBoxItemMargin: Integer;
    procedure SetListBoxItemMargin(Value: Integer);

    function GetListBoxIndentMargin: Integer;
    procedure SetListBoxIndentMargin(Value: Integer);

    function GetListBoxLineColor: TColor;
    procedure SetListBoxLineColor(Value: TColor);

    function GetListBoxLineColorAlpha: TColor;
    procedure SetListBoxLineColorAlpha(Value: TColor);

    function GetListBoxShowLines: Boolean;
    procedure SetListBoxShowLines(Value: Boolean);
    function GetListBoxItemHeight: Integer;
    procedure SetListBoxItemHeight(Value: Integer);
    function GetListBoxHeaderHeight: Integer;
    procedure SetListBoxHeaderHeight(Value: Integer);
    function GetListBoxShowItemDetails: Boolean;
    procedure SetListBoxShowItemDetails(Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdateItems;
    procedure EndUpdateItems;

    procedure Add(const Item: String); overload;
    procedure Add(Items: TStrings); overload;
    procedure Delete(Index: Integer);

    procedure CloseUp(Value: Boolean);
    procedure DropDown; virtual;
    function IsPopupVisible: Boolean;
    function IndexOf(const S: string; AStartOff: Boolean = False): Integer;
    property ListBox: TscGPPopupListBox read FListBox;
    function IsModified: Boolean;
  published
    property DropDownPosition: TscDropDownPosition
      read FDropDownPosition write FDropDownPosition default scdpRight;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property LeftButton;
    property RightButton;
    property ButtonImages;
    property CustomDraw;
    property Transparent;
    property UseFilter: Boolean read FUseFilter write FUseFilter;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Align;
    property Items: TscGPListBoxItems read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Images: TCustomImageList read GetImages write SetImages;

    property ListBoxItemWordWrap: Boolean read
      GetListBoxItemWordWrap write SetListBoxItemWordWrap;
    property ListBoxItemShowEllipsis: Boolean read
      GetListBoxItemShowEllipsis write SetListBoxItemShowEllipsis;

    property ListBoxHeaderFont: TFont
      read GetListBoxHeaderFont write SetListBoxHeaderFont;

    property ListBoxDetailFont: TFont
      read GetListBoxDetailFont write SetListBoxDetailFont;

    property ListBoxLineColor: TColor
      read GetListBoxLineColor write SetListBoxLineColor;
    property ListBoxLineColorAlpha: TColor
      read GetListBoxLineColorAlpha write SetListBoxLineColorAlpha;

    property ListBoxWidth: Integer read FListBoxWidth write FListBoxWidth;
    property ListBoxHeight: Integer read FListBoxHeight write FListBoxHeight;

    property ListBoxIndentMargin: Integer
      read GetListBoxIndentMargin write SetListBoxIndentMargin;

    property ListBoxItemSpacing: Integer
      read GetListBoxItemSpacing write SetListBoxItemSpacing;

    property ListBoxItemMargin: Integer
      read GetListBoxItemMargin write SetListBoxItemMargin;

    property ListBoxShowItemDetails: Boolean
      read GetListBoxShowItemDetails write SetListBoxShowItemDetails;

    property ListBoxShowLines: Boolean
      read GetListBoxShowLines write SetListBoxShowLines;
    property ListBoxItemHeight: Integer
      read GetListBoxItemHeight write SetListBoxItemHeight;
    property ListBoxHeaderHeight: Integer
      read GetListBoxHeaderHeight write SetListBoxHeaderHeight;

    property ListBoxHeaderOptions: TscGPHeaderOptions
      read GetListBoxHeaderOptions write SetListBoxHeaderOptions;

    property ListBoxSelectionOptions: TscGPSelectionOptions
      read GetListBoxSelectionOptions write SetListBoxSelectionOptions;

    property ListBoxOptions: TscGPScrollingControlOptions
      read GetListBoxOptions write SetListBoxOptions;

     property ListBoxScrollBarOptions: TscGPControlScrollBarOptions
      read GetListBoxScrollBarOptions write SetListBoxScrollBarOptions;

    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;
    property EditMask;
    property Text;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumbersOnly;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property ReadOnly;
    property TextHint;
    property Touch;
    property StyleElements;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnLeftButtonClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;


  TscGPSpinEdit = class(TscGPCustomEdit)
  private
    FDisplayType: TscNumEditDisplayType;
    FUpDownKind: TscUpDownKind;
    StopCheck, FromEdit: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FDecimal: Byte;
    FValueType: TscValueType;
    FIncrement: Double;
    FIsModified: Boolean;
    FMouseWheelSupport: Boolean;

    function GetArrowGlyphColor: TColor;
    function GetArrowGlyphColorAlpha: Byte;
    function GetArrowGlyphColorHotAlpha: Byte;
    function GetArrowGlyphColorPressedAlpha: Byte;

    procedure SetArrowGlyphColor(Value: TColor);
    procedure SetArrowGlyphColorAlpha(Value: Byte);
    procedure SetArrowGlyphColorHotAlpha(Value: Byte);
    procedure SetArrowGlyphColorPressedAlpha(Value: Byte);

    procedure SetUpDownKind(Value: TscUpDownKind);
    procedure SetDisplayType(Value: TscNumEditDisplayType);
    procedure SetValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetDecimal(NewValue: Byte);
    procedure SetValueType(NewType: TscValueType);
    procedure UpButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure ShowUpDownButtons;
  protected
    function GetValueAsInt: Integer;
    function CheckValue(NewValue: Double): Double;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function IsValidChar(Key: Char): Boolean;
    procedure Change; override;
    property Text;
    procedure WMKILLFOCUS(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    function IsCustomDraw(ADC: HDC): Boolean; override;
    function GetTextRect: TRect; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsNumText(AText: String): Boolean;
    property ValueAsInt: Integer read GetValueAsInt;
    function IsModified: Boolean;
  published
    property Increment: Double read FIncrement write FIncrement;
    property UpDownKind: TscUpDownKind read FUpDownKind write SetUpDownKind;
    property Alignment;
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property ValueType: TscValueType read FValueType write SetValueType;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property Value: Double read FValue write SetValue;
    property DisplayType: TscNumEditDisplayType
      read FDisplayType write SetDisplayType;
    property ArrowGlyphColor: TColor
      read GetArrowGlyphColor write SetArrowGlyphColor;
    property ArrowGlyphColorAlpha: Byte
      read GetArrowGlyphColorAlpha write SetArrowGlyphColorAlpha;
    property ArrowGlyphColorHotAlpha: Byte
      read GetArrowGlyphColorHotAlpha write SetArrowGlyphColorHotAlpha;
    property ArrowGlyphColorPressedAlpha: Byte
      read GetArrowGlyphColorPressedAlpha write SetArrowGlyphColorPressedAlpha;
    property Transparent;
    property ButtonImages;
    property EditMask;
    property Align;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumbersOnly;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TextHint;
    property Touch;
    property Visible;
    property StyleElements;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPTimeEdit = class(TscGPCustomEdit)
  private
    FShowMSec: Boolean;
    FShowSec: Boolean;
    FShowUpDown: Boolean;
    FTimeFormat: TscTimeFormat;

    function GetArrowGlyphColor: TColor;
    function GetArrowGlyphColorAlpha: Byte;
    function GetArrowGlyphColorHotAlpha: Byte;
    function GetArrowGlyphColorPressedAlpha: Byte;

    procedure SetArrowGlyphColor(Value: TColor);
    procedure SetArrowGlyphColorAlpha(Value: Byte);
    procedure SetArrowGlyphColorHotAlpha(Value: Byte);
    procedure SetArrowGlyphColorPressedAlpha(Value: Byte);

    function GetIncIndex: Integer;
    procedure UpButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure SetShowMilliseconds(const Value: Boolean);
    procedure SetShowSeconds(const Value: Boolean);
    procedure SetMilliseconds(const Value: Integer);
    function GetMilliseconds: Integer;
    procedure SetTime(const Value: string);
    function GetTime: string;
    function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
    function IsValidChar(Key: Char): Boolean;
    procedure CheckSpace(var S: String);
    procedure SetValidTime(var H, M, S, MS: Word);
    function ValidateParameter(S: String; MustLen: Integer): String;
    procedure SetShowUpDown(Value: Boolean);
    procedure ShowUpDownButtons;
    procedure HideUpDownButtons;
    procedure SetTimeFormat(Value: TscTimeFormat);
    function GetTimeValue: TTime;
    procedure SetTimeValue(Value: TTime);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function GetTextRect: TRect; override;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ValidateEdit; override;
    procedure DecodeTime(var Hour, Min, Sec, MSec: Word);
    procedure EncodeTime(Hour, Min, Sec, MSec: Word);
    property Milliseconds: Integer read GetMilliseconds write SetMilliseconds;
    property Time: string read GetTime write SetTime;
    property TimeValue: TTime read GetTimeValue write SetTimeValue;
  published
    property ShowUpDown: Boolean read FShowUpDown write SetShowUpDown;
    property ShowMSec: Boolean read FShowMSec write SetShowMilliseconds;
    property ShowSec: Boolean read FShowSec write SetShowSeconds;
    property TimeFormat: TscTimeFormat read
      FTimeFormat write SetTimeFormat default sctf24Hour;
    property ArrowGlyphColor: TColor
      read GetArrowGlyphColor write SetArrowGlyphColor;
    property ArrowGlyphColorAlpha: Byte
      read GetArrowGlyphColorAlpha write SetArrowGlyphColorAlpha;
    property ArrowGlyphColorHotAlpha: Byte
      read GetArrowGlyphColorHotAlpha write SetArrowGlyphColorHotAlpha;
    property ArrowGlyphColorPressedAlpha: Byte
      read GetArrowGlyphColorPressedAlpha write SetArrowGlyphColorPressedAlpha;
    property Transparent;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumbersOnly;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Touch;
    property Visible;
    property StyleElements;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPPasswordEditButton = class(TPersistent)
  private
    FVisible: Boolean;
    FGlyphColor: TColor;
    FGlyphColorAlpha: Byte;
    FGlyphColorHotAlpha: Byte;
    FGlyphColorPressedAlpha: Byte;
    FOnChange: TNotifyEvent;
    procedure SetGlyphColor(Value: TColor);
    procedure SetGlyphColorAlpha(Value: Byte);
    procedure SetVisible(Value: Boolean);
  protected
    procedure Changed;
  public
    FDropDown: Boolean;
    ButtonRect: TRect;
    MouseIn: Boolean;
    Down: Boolean;
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property GlyphColor: TColor
      read FGlyphColor write SetGlyphColor;
    property GlyphColorAlpha: Byte
      read FGlyphColorAlpha write SetGlyphColorAlpha;
    property GlyphColorHotAlpha: Byte
      read FGlyphColorHotAlpha write FGlyphColorHotAlpha;
    property GlyphColorPressedAlpha: Byte
      read FGlyphColorPressedAlpha write FGlyphColorPressedAlpha;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscGPPasswordEdit = class(TscCustomControl)
  private
    FOptions: TscGPEditOptions;
    FTransparent: Boolean;
    FMouseIn: Boolean;
    FText: String;
    FLMouseSelecting: Boolean;
    FCaretPosition: Integer;
    FSelStart: Integer;
    FSelLength: Integer;
    FFVChar: Integer;
    FAutoSelect: Boolean;
    FCharCase: TEditCharCase;
    FHideSelection: Boolean;
    FMaxLength: Integer;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FTextAlignment: TAlignment;
    FContentMarginLeft: Integer;
    FContentMarginRight: Integer;
    FContentMarginTop: Integer;
    FContentMarginBottom: Integer;
    FPromptText: String;
    FPromptTextColor: TColor;
    FShowingText: Boolean;
    FPasswordButton: TscGPPasswordEditButton;
    FGlyphSize: Integer;
    FHidePromptTextIfFocused: Boolean;
    procedure SetTransparent(Value: Boolean);
    procedure SetPromptText(Value: String);
    procedure SetPromptTextColor(Value: TColor);
    procedure SetContentMarginLeft(Value: Integer);
    procedure SetContentMarginTop(Value: Integer);
    procedure SetContentMarginRight(Value: Integer);
    procedure SetContentMarginBottom(Value: Integer);
    procedure UpdateFVC;
    procedure UpdateCaretePosition;
    procedure UpdateCarete;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    function GetSelText: String;
    function GetVisibleSelText: String;
    function GetNextWPos(StartPosition: Integer): Integer;
    function GetPrivWPos(StartPosition: Integer): Integer;
    function GetSelStart: Integer;
    function GetSelLength: Integer;
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetCaretPosition(const Value: Integer);
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetAutoSelect(const Value: Boolean);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetMaxLength(const Value: Integer);
    procedure SetTextAlignment(const Value: TAlignment);
  protected
    procedure Loaded; override;
    function GetEditRect: TRect; virtual;
    function GetPasswordFigureWidth: Integer;
    function GetCharX(A: Integer): Integer;
    function GetCPos(x: Integer): Integer;
    function GetSelRect: TRect; virtual;
    function GetAlignmentFlags: Integer;
    procedure PaintButton(G: TGPGraphics; Cnv: TCanvas);
    procedure PaintText(G: TGPGraphics; Cnv: TCanvas);
    procedure PaintSelectedText(G: TGPGraphics; Cnv: TCanvas);
    procedure DrawPasswordChar(G: TGPGraphics; SymbolRect: TRect; Selected: Boolean; Cnv: TCanvas);
    function ValidText(NewText: String): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SelectWord;
    procedure Change; dynamic;
    property CaretPosition: Integer read FCaretPosition write SetCaretPosition;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetPaintText: String; virtual;
    procedure DrawPromptText(ACanvas: TCanvas);
    procedure DrawPasswordText(ACanvas: TCanvas);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure OptionsChanged(Sender: TObject);
    procedure DrawEditShape(G: TGPGraphics; ACanvas: TCanvas);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PasteFromClipboard;
    procedure ShowCaret; virtual;
    procedure HideCaret; virtual;
    procedure ClearSelection;
    procedure SelectAll;
    procedure Clear;
    procedure InsertChar(Ch: Char);
    procedure InsertText(AText: String);
    procedure InsertAfter(Position: Integer; S: String; Selected: Boolean);
    procedure DeleteFrom(Position, Length : Integer; MoveCaret : Boolean);
    procedure ShowPasswordText(AShow: Boolean);
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: String read GetSelText;
  published
    property Anchors;
    property AutoSelect: Boolean read FAutoSelect write SetAutoSelect default True;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default TEditCharCase.ecNormal;
    property Constraints;
    property Color default clWindow;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
     property Options: TscGPEditOptions
      read FOptions write FOptions;
    property PasswordButton: TscGPPasswordEditButton
      read FPasswordButton write FPasswordButton;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property ContentMarginLeft: Integer read FContentMarginLeft write SetContentMarginLeft;
    property ContentMarginRight: Integer read FContentMarginRight write SetContentMarginRight;
    property ContentMarginTop: Integer read FContentMarginTop write SetContentMarginTop;
    property ContentMarginBottom: Integer read FContentMarginBottom write SetContentMarginBottom;
    property PromptText: String read FPromptText write SetPromptText;
    property PromptTextColor: TColor read FPromptTextColor write SetPromptTextColor;
    property HidePromptTextIfFocused: Boolean
      read FHidePromptTextIfFocused write FHidePromptTextIfFocused;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property ParentFont;
    property ParentShowHint;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Text: String read GetText write SetText;
    property TextAlignment : TAlignment read FTextAlignment write SetTextAlignment default taLeftJustify;
    property Visible;
    property StyleElements;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPCalButton = class(TscGPButton)
  protected
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    DrawCode: Integer;
    constructor Create(AOwner: TComponent); override;
  end;

  TscGPMonthCalendar = class(TscGPPanel)
  protected
    FShowMonthMenu: Boolean;
    FMenuPopup: Boolean;
    FMonthMenu: TPopupMenu;
    FIsChina: Boolean;
    FWeekNumbers: Boolean;
    FShowToday: Boolean;
    FTodayDefault: Boolean;
    BevelTop, CellW, CellH, BottomOffset: Integer;
    FBtns: array[0..3] of TscGPCalButton;
    FDate: TDate;
    FFirstDayOfWeek: TscDaysOfWeek;
    CalFontColor: TColor;
    CalGrayFontColor: TColor;
    FOnNumberClick: TNotifyEvent;
    FBoldDays: Boolean;
    FTodayR: TRect;
    FInTodayR: Boolean;
    procedure MonthMenuClick(Sender: TObject);
    procedure Loaded; override;
    procedure CreateMonthMenu;
    procedure SetTodayDefault(Value: Boolean);
    procedure OffsetMonth(AOffset: Integer);
    procedure OffsetYear(AOffset: Integer);
    procedure SetFirstDayOfWeek(Value: TscDaysOfWeek);
    procedure UpdateCalendar;
    procedure ArangeControls;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure SetDate(Value: TDate);
    procedure DrawCalendar(Cnvs: TCanvas);
    function DaysThisMonth: Integer;
    function GetMonthOffset: Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DayNumFromPoint(X, Y: Integer): Word;
    procedure NextMButtonClick(Sender: TObject);
    procedure PriorMButtonClick(Sender: TObject);
    procedure NextYButtonClick(Sender: TObject);
    procedure PriorYButtonClick(Sender: TObject);
    procedure SetBoldDays(Value: Boolean);
    procedure SetWeekNumbers(Value: Boolean);
    procedure SetShowToday(Value: Boolean);
    procedure DrawFrame(R: TRect; C: TCanvas; G: TGPGraphics);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Date: TDate read FDate write SetDate;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers;
    property ShowToday: Boolean read FShowToday write SetShowToday;
    property TodayDefault: Boolean read FTodayDefault write SetTodayDefault;
    property FirstDayOfWeek: TscDaysOfWeek read FFirstDayOfWeek write SetFirstDayOfWeek;
    property OnNumberClick: TNotifyEvent
      read FOnNumberClick write FOnNumberClick;
    property BoldDays: Boolean read FBoldDays write SetBoldDays;
    property ShowMonthMenu: Boolean read FShowMonthMenu write FShowMonthMenu;
  end;

  TscGPPopupMonthCalendar = class(TscGPMonthCalendar)
  protected
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscGPDateEdit = class(TscGPCustomEdit)
  private
    StopCheck: Boolean;
    FTodayDefault: Boolean;
    FBlanksChar: Char;
    FDateSelected: Boolean;
  protected
    FMonthCalendar: TscGPPopupMonthCalendar;
    FOnDateChange: TNotifyEvent;
    FOldDateValue: TDateTime;
    function IsCalendarVisible: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMMouseHookCancelMode(var Message: TMessage); message WM_MOUSEHOOKCANCELMODE;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function FourDigitYear: Boolean;
    function GetDateOrder(const DateFormat: string): TscDateOrder;
    function DefDateFormat(FourDigitYear: Boolean): string;
    function DefDateMask(BlanksChar: Char; FourDigitYear: Boolean): string;
    function MonthFromName(const S: string; MaxLen: Byte): Byte;
    procedure ExtractMask(const Format, S: string; Ch: Char; Cnt: Integer;
       var I: Integer; Blank, Default: Integer);
    function ScanDateStr(const Format, S: string; var D, M, Y: Integer): Boolean;
    function CurrentYear: Word;
    function ExpandYear(Year: Integer): Integer;
    function IsValidDate(Y, M, D: Word): Boolean;
    function ScanDate(const S, DateFormat: string; var Pos: Integer;
      var Y, M, D: Integer): Boolean;
    function GetDateMask: String;
    procedure Loaded; override;

    function GetShowToday: Boolean;
    procedure SetShowToday(Value: Boolean);
    function GetWeekNumbers: Boolean;
    procedure SetWeekNumbers(Value: Boolean);

    procedure SetTodayDefault(Value: Boolean);
    function GetCalendarFont: TFont;
    procedure SetCalendarFont(Value: TFont);
    function GetCalendarWidth: Integer;
    procedure SetCalendarWidth(Value: Integer);

    function GetCalendarBoldDays: Boolean;
    procedure SetCalendarBoldDays(Value: Boolean);

    function GetCalendarHeight: Integer;
    procedure SetCalendarHeight(Value: Integer);

    function GetDate: TDate;
    procedure SetDate(Value: TDate);
    procedure DropDown; virtual;
    procedure CloseUp(AcceptValue: Boolean);
    procedure CalendarClick(Sender: TObject);
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WndProc(var Message: TMessage); override;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CheckValidDate;
    procedure Change; override;

    function GetFirstDayOfWeek: TscDaysOfWeek;
    procedure SetFirstDayOfWeek(Value: TscDaysOfWeek);
    function IsValidText(S: String): Boolean;
    function IsOnlyNumbers(S: String): Boolean;
    function MyStrToDate(S: String): TDate;
    function MyDateToStr(Date: TDate): String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsDateInput: Boolean;
    procedure ValidateEdit; override;
    procedure ButtonClick(Sender: TObject);
  published
    property Date: TDate read GetDate write SetDate;
    property TodayDefault: Boolean read FTodayDefault write SetTodayDefault;

    property CalendarWidth: Integer read GetCalendarWidth write SetCalendarWidth;
    property CalendarHeight: Integer read GetCalendarHeight write SetCalendarHeight;
    property CalendarFont: TFont read GetCalendarFont write SetCalendarFont;
    property CalendarBoldDays: Boolean read GetCalendarBoldDays write SetCalendarBoldDays;

    property FirstDayOfWeek: TscDaysOfWeek
      read GetFirstDayOfWeek write SetFirstDayOfWeek;

    property WeekNumbers: Boolean
      read GetWeekNumbers write SetWeekNumbers;

    property ShowToday: Boolean
      read GetShowToday write SetShowToday;

    property OnDateChange: TNotifyEvent
      read FOnDateChange write FOnDateChange;

    property LeftButton;
    property RightButton;
    property Transparent;
    property ButtonImages;

    property Align;
    property ReadOnly;
    property Font;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property CharCase;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnLeftButtonClick;
  end;

   TscGPMemoOptions = class(TPersistent)
    private
      FNormalColor: TColor;
      FHotColor: TColor;
      FFocusedColor: TColor;
      FDisabledColor: TColor;
      FFrameNormalColor: TColor;
      FFrameHotColor: TColor;
      FFrameFocusedColor: TColor;
      FFrameDisabledColor: TColor;
      FFrameWidth: Integer;
      FFontNormalColor: TColor;
      FFontHotColor: TColor;
      FFontFocusedColor: TColor;
      FFontDisabledColor: TColor;

      FNormalColorAlpha: Byte;
      FHotColorAlpha: Byte;
      FFocusedColorAlpha: Byte;
      FDisabledColorAlpha: Byte;

      FFrameNormalColorAlpha: Byte;
      FFrameHotColorAlpha: Byte;
      FFrameFocusedColorAlpha: Byte;
      FFrameDisabledColorAlpha: Byte;

      FStyleColors: Boolean;
      FState: TscsCtrlState;
      FOnChange: TNotifyEvent;

      FShapeCornerRadius: Integer;
      FShapeFillStyle: TscGPShapeFillStyle;
      FShapeFillGradientAngle: Integer;

      FShapeFillGradientBeginColorOffset: Byte;
      FShapeFillGradientEndColorOffset: Byte;

      FScaleFrameWidth: Boolean;

      procedure SetShapeFillGradientBeginColorOffset(Value: Byte);
      procedure SetShapeFillGradientEndColorOffset(Value: Byte);

      procedure SetShapeFillStyle(Value: TscGPShapeFillStyle);
      procedure SetShapeFillGradientAngle(Value: Integer);

      procedure SetShapeCornerRadius(Value: Integer);

      function GetNormalColor: TColor;
      function GetHotColor: TColor;
      function GetFocusedColor: TColor;
      function GetDisabledColor: TColor;

      function GetFrameNormalColor: TColor;
      function GetFrameHotColor: TColor;
      function GetFrameFocusedColor: TColor;
      function GetFrameDisabledColor: TColor;

      function GetFontNormalColor: TColor;
      function GetFontHotColor: TColor;
      function GetFontFocusedColor: TColor;
      function GetFontDisabledColor: TColor;

      function GetColor: TColor;
      function GetFrameColor: TColor;
      function GetFontColor: TColor;
      function GetColorAlpha: Byte;
      function GetFrameColorAlpha: Byte;

      procedure SetNormalColor(Value: TColor);
      procedure SetHotColor(Value: TColor);
      procedure SetFocusedColor(Value: TColor);
      procedure SetDisabledColor(Value: TColor);

      procedure SetNormalColorAlpha(Value: Byte);
      procedure SetHotColorAlpha(Value: Byte);
      procedure SetFocusedColorAlpha(Value: Byte);
      procedure SetDisabledColorAlpha(Value: Byte);

      procedure SetFrameNormalColor(Value: TColor);
      procedure SetFrameHotColor(Value: TColor);
      procedure SetFrameFocusedColor(Value: TColor);
      procedure SetFrameDisabledColor(Value: TColor);

      procedure SetFrameNormalColorAlpha(Value: Byte);
      procedure SetFrameHotColorAlpha(Value: Byte);
      procedure SetFrameFocusedColorAlpha(Value: Byte);
      procedure SetFrameDisabledColorAlpha(Value: Byte);

      procedure SetFontNormalColor(Value: TColor);
      procedure SetFontHotColor(Value: TColor);
      procedure SetFontFocusedColor(Value: TColor);
      procedure SetFontDisabledColor(Value: TColor);

      procedure SetFrameWidth(Value: Integer);
      procedure SetStyleColors(Value: Boolean);

      procedure Changed;
    public
      constructor Create; virtual;
      procedure Assign(Source: TPersistent); override;

      property State: TscsCtrlState read FState write FState;

      property Color: TColor read GetColor;
      property FrameColor: TColor read GetFrameColor;
      property FontColor: TColor read GetFontColor;
      property ColorAlpha: Byte read GetColorAlpha;
      property FrameColorAlpha: Byte read GetFrameColorAlpha;
    published
      property NormalColor: TColor read GetNormalColor write SetNormalColor;
      property HotColor: TColor read GetHotColor write SetHotColor;
      property FocusedColor: TColor read GetFocusedColor write SetFocusedColor;
      property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;

      property NormalColorAlpha: Byte read FNormalColorAlpha write SetNormalColorAlpha;
      property HotColorAlpha: Byte read FHotColorAlpha write SetHotColorAlpha;
      property FocusedColorAlpha: Byte read FFocusedColorAlpha write SetFocusedColorAlpha;
      property DisabledColorAlpha: Byte read FDisabledColorAlpha write SetDisabledColorAlpha;

      property FrameNormalColor: TColor read GetFrameNormalColor write SetFrameNormalColor;
      property FrameHotColor: TColor read GetFrameHotColor write SetFrameHotColor;
      property FrameFocusedColor: TColor read GetFrameFocusedColor write SetFrameFocusedColor;
      property FrameDisabledColor: TColor read GetFrameDisabledColor write SetFrameDisabledColor;
      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

      property FrameNormalColorAlpha: Byte read FFrameNormalColorAlpha write SetFrameNormalColorAlpha;
      property FrameHotColorAlpha: Byte read FFrameHotColorAlpha write SetFrameHotColorAlpha;
      property FrameFocusedColorAlpha: Byte read FFrameFocusedColorAlpha write SetFrameFocusedColorAlpha;
      property FrameDisabledColorAlpha: Byte read FFrameDisabledColorAlpha write SetFrameDisabledColorAlpha;

      property FontNormalColor: TColor read GetFontNormalColor write SetFontNormalColor;
      property FontHotColor: TColor read GetFontHotColor write SetFontHotColor;
      property FontFocusedColor: TColor read GetFontFocusedColor write SetFontFocusedColor;
      property FontDisabledColor: TColor read GetFontDisabledColor write SetFontDisabledColor;

      property ShapeFillStyle: TscGPShapeFillStyle
        read FShapeFillStyle write SetShapeFillStyle default scgpsfColor;
      property ShapeFillGradientAngle: Integer
        read FShapeFillGradientAngle write SetShapeFillGradientAngle;
                                                            
      property ShapeFillGradientBeginColorOffset: Byte
        read FShapeFillGradientBeginColorOffset write SetShapeFillGradientBeginColorOffset;
      property ShapeFillGradientEndColorOffset: Byte
        read FShapeFillGradientEndColorOffset write SetShapeFillGradientEndColorOffset;
      
      property ShapeCornerRadius: Integer
        read FShapeCornerRadius write SetShapeCornerRadius;

      property ScaleFrameWidth: Boolean
        read FScaleFrameWidth write FScaleFrameWidth;

      property StyleColors: Boolean read FStyleColors write SetStyleColors;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  TscGPMemoScrollBar = class(TscGPControlScrollBar)
  protected
    function GetOptions: TscGPControlScrollBarOptions; override;
  public
    procedure RePaint; override;
  end;

  TscGPMemo = class(TCustomMemo)
  private
    {$IFDEF VER230}
    FStyleElements: TStyleElements;
    {$ENDIF}
    FTransparent: Boolean;
    ParentBGBuffer: TBitmap;
    FSelLen, FSelPos: Integer;
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    FContentMarginLeft: Integer;
    FContentMarginRight: Integer;
    FContentMarginTop: Integer;
    FContentMarginBottom: Integer;
    FOptions: TscGPMemoOptions;
    FScrollBarOptions: TscGPControlScrollBarOptions;
    FMouseIn: Boolean;
    FScrollCaptured: Boolean;
    FSizeChanging: Boolean;
    // Windows 10 Fluient UI
    FFluentUIOpaque: Boolean;
    procedure SetFluentUIOpaque(Value: Boolean);
    //
    procedure SetTransparent(Value: Boolean);
    procedure SetRedraw(Value: Boolean);
    procedure SetContentMarginLeft(Value: Integer);
    procedure SetContentMarginTop(Value: Integer);
    procedure SetContentMarginRight(Value: Integer);
    procedure SetContentMarginBottom(Value: Integer);
  protected
    FVertScrollBar: TscGPMemoScrollBar;
    FHorzScrollBar: TscGPMemoScrollBar;
    FStopDraw: Boolean;
    FStopGetParentBG: Boolean;
    FStopRedraw: Boolean;
    procedure GetParentBG;
    function IsFluentUIOpaque: Boolean;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMVSCROLL(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHSCROLL(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd);  message WM_ERASEBKGND;
    procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
    procedure WMCOMMAND(var Message: TWMCOMMAND); message CN_COMMAND;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure DrawBackGround(ACanvas: TCanvas);
    procedure DrawMemoShape(ACanvas: TCanvas);
    procedure DoPaint;
    procedure DoPaint2(DC: HDC);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMNCPAINT(var Message: TWMNCPAINT); message WM_NCPAINT;
    procedure WMNCCALCSIZE(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure OptionsChanged(Sender: TObject);
    procedure ScrollOptionsChanged(Sender: TObject);
    function GetTextRect: TRect; virtual;
    procedure AdjustTextRect;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure MemoMouseEnter;
    procedure MemoMouseLeave;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCHITTEST(var Message: TWMNCHITTEST); message WM_NCHITTEST;
    procedure GetMemoScrollInfo; virtual;
    procedure OnHorzScrollBarChanged(Sender: TObject);
    procedure OnVertScrollBarChanged(Sender: TObject);
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
    procedure CMSEPaint(var Message: TMessage); message CM_SEPAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FluentUIOpaque: Boolean
      read FFluentUIOpaque write SetFluentUIOpaque;
    property Options: TscGPMemoOptions
      read FOptions write FOptions;
    property ScrollBarOptions: TscGPControlScrollBarOptions
      read FScrollBarOptions write FScrollBarOptions;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property ContentMarginLeft: Integer read FContentMarginLeft write SetContentMarginLeft;
    property ContentMarginRight: Integer read FContentMarginRight write SetContentMarginRight;
    property ContentMarginTop: Integer read FContentMarginTop write SetContentMarginTop;
    property ContentMarginBottom: Integer read FContentMarginBottom write SetContentMarginBottom;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    {$IFDEF VER230}
    property StyleElements: TStyleElements
      read  FStyleElements write FStyleElements;
    {$ENDIF}
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscGPGearActivityIndicatorKind = (scgpgkOneGear, scgpgkTwoGears);

  TscGPGearActivityIndicator = class(TscCustomControl)
  protected
    FGlyphColor: TColor;
    FGlyphColorAlpha: Byte;
    FGlyphThickness: Byte;
    FAnimationTimer: TTimer;
    FAnimationAngle: Double;
    FBigAnimationAngle: Double;
    FActive: Boolean;
    FKind: TscGPGearActivityIndicatorKind;
    FAnimationAcceleration: Boolean;
    procedure SetGlyphThickness(Value: Byte);
    procedure SetKind(Value: TscGPGearActivityIndicatorKind);
    procedure SetGlyphColor(Value: TColor);
    procedure SetGlyphColorAlpha(Value: Byte);
    procedure SetActive(Value: Boolean);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure StartAnimation;
    procedure StopAnimation;
    procedure OnAnimationTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure Activate;
    procedure Deactivate;
  published
    property Active: Boolean
      read FActive write SetActive;
    property AnimationAcceleration: Boolean
      read FAnimationAcceleration write FAnimationAcceleration;
    property GlyphColor: TColor
      read FGlyphColor write SetGlyphColor;
    property GlyphColorAlpha: Byte
      read FGlyphColorAlpha write SetGlyphColorAlpha;
    property GlyphThickness: Byte
      read FGlyphThickness write SetGlyphThickness;
    property Kind: TscGPGearActivityIndicatorKind
      read FKind write SetKind;
    property TransparentBackground;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Align;
    property Color;
    property Enabled;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;


  TscGPScrollPanelBGStyleKind = (scgspbFormBackground, scgspbPanel,
    scgspbTransparent);

  TscGPScrollPanelHorzDirection = (scgspsdLeftToRight, scgspsdRightToLeft);
  TscGPScrollPanelVertDirection = (scgspsdTopToBottom, scgspsdBottomToTop);

  TscGPScrollPanel = class(TscCustomControl)
  private
    FFillStyle: TscGPShapeFillStyle;
    FFillColor: TColor;
    FFillColorAlpha: Byte;
    FFillColor2: TColor;
    FFillGradientAngle: Integer;
    FFillGradientBeginAlpha: Byte;
    FFillGradientEndAlpha: Byte;
    FFillGradientBeginColorOffset: Byte;
    FFillGradientEndColorOffset: Byte;

    FBGStyleKind: TscGPScrollPanelBGStyleKind;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomImageIndex: Integer;
    FCapturedButton: Integer;
    FMouseTimerActive: Boolean;
    FHotScroll: Boolean;
    TimerMode: Integer;
    FStartScroll: Boolean;
    SMax, SPosition, SPage, SOldPosition: Integer;
    FAutoSize: Boolean;
    FScrollType: TscScrollType;
    FScrollTimerInterval: Integer;
    FTouchBegin, FTouchEnd: Integer;
    FTouchScroll: Boolean;
    FScrollButtonWidth: Integer;
    FScrollButtonArrowWidth: Integer;

    FHorzScrollDirection: TscGPScrollPanelHorzDirection;
    FVertScrollDirection: TscGPScrollPanelVertDirection;

    procedure SetHorzScrollDirection(Value: TscGPScrollPanelHorzDirection);
    procedure SetVertScrollDirection(Value: TscGPScrollPanelVertDirection);

    procedure SetScrollButtonArrowWidth(Value: Integer);
    procedure SetFillColor(Value: TColor);
    procedure SetFillColorAlpha(Value: Byte);
    procedure SetFillColor2(Value: TColor);
    procedure SetFillStyle(Value: TscGPShapeFillStyle);
    procedure SetFillGradientAngle(Value: Integer);
    procedure SetFillGradientBeginAlpha(Value: Byte);
    procedure SetFillGradientEndAlpha(Value: Byte);
    procedure SetFillGradientBeginColorOffset(Value: Byte);
    procedure SetFillGradientEndColorOffset(Value: Byte);

    procedure SetScrollButtonWidth(Value: Integer);

    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetBGStyleKind(Value: TscGPScrollPanelBGStyleKind);

    procedure SetScrollType(Value: TscScrollType);
    procedure SetScrollOffset(Value: Integer);
    procedure SetScrollTimerInterval(Value: Integer);
    procedure DrawButton(Cnvs: TCanvas; i: Integer);
    procedure SetPosition(const Value: integer);
    procedure SetTouchScroll(Value: Boolean);
    procedure UpdateNC;
  protected
    Buttons: array[0..1] of TscControlButton;
    FCanScroll: Boolean;
    FUpdatingScrollInfo: Boolean;
    FVSizeOffset: Integer;
    FHSizeOffset: Integer;
    FScrollOffset: Integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMGesture(var Message: TCMGesture); message CM_GESTURE;
    procedure WMSETCURSOR(var Message: TWMSetCursor); message WM_SetCursor;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure WMNCCALCSIZE(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPAINT(var Message: TMessage); message WM_NCPAINT;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure WMDESTROY(var Message: TMessage); message WM_DESTROY;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure DoMouseLeave;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure WndProc(var Message: TMessage); override;
    procedure SetButtonsVisible(AVisible: Boolean);
    procedure ButtonClick(I: Integer);
    procedure ButtonDown(I: Integer);
    procedure ButtonUp(I: Integer);
    procedure GetHRange;
    procedure GetVRange;
    procedure GetHRangeRTL;
    procedure GetVRangeBTT;
    procedure VScrollControls(AOffset: Integer);
    procedure HScrollControls(AOffset: Integer);
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure StartTimer;
    procedure StopTimer;
    procedure Loaded; override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure InitTouch;
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
    procedure DrawBackground(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    procedure UpDateSize;
    procedure GetScrollInfo;
    property Position: integer read SPosition write SetPosition;
  published
    property CanScroll: Boolean read FCanScroll write FCanScroll;
    property FillStyle: TscGPShapeFillStyle
      read FFillStyle write SetFillStyle default scgpsfColor;
    property FillGradientAngle: Integer
      read FFillGradientAngle write SetFillGradientAngle;
    property FillGradientBeginAlpha: Byte
      read FFillGradientBeginAlpha write SetFillGradientBeginAlpha;
    property FillGradientEndAlpha: Byte
      read FFillGradientEndAlpha write SetFillGradientEndAlpha;
    property FillGradientBeginColorOffset: Byte
      read FFillGradientBeginColorOffset write SetFillGradientBeginColorOffset;
    property FillGradientEndColorOffset: Byte
      read FFillGradientEndColorOffset write SetFillGradientEndColorOffset;
    property FillColor: TColor read FFillColor write SetFillColor;
    property FillColorAlpha: Byte read FFillColorAlpha write SetFillColorAlpha;
    property FillColor2: TColor read FFillColor2 write SetFillColor2;

    property Color;
    property BGStyleKind: TscGPScrollPanelBGStyleKind read FBGStyleKind write SetBGStyleKind;
    property ScrollButtonWidth: Integer
      read FScrollButtonWidth write SetScrollButtonWidth;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomImageIndex: Integer read FCustomImageIndex write SetCustomImageIndex;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property HotScroll: Boolean read FHotScroll write FHotScroll;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property ScrollType: TscScrollType read FScrollType write SetScrollType;
    property ScrollOffset: Integer read FScrollOffset write SetScrollOffset;
    property ScrollTimerInterval: Integer
      read FScrollTimerInterval write SetScrollTimerInterval;
    property ScrollButtonArrowWidth: Integer
      read FScrollButtonArrowWidth write SetScrollButtonArrowWidth;
    property HorzScrollDirection: TscGPScrollPanelHorzDirection
      read FHorzScrollDirection write SetHorzScrollDirection;
    property VertScrollDirection: TscGPScrollPanelVertDirection
      read FVertScrollDirection write SetVertScrollDirection;
    property TouchScroll: Boolean read
      FTouchScroll write SetTouchScroll;
    property StorePaintBuffer;
  end;

implementation

uses
  System.Math, Vcl.Consts, Vcl.Themes, Vcl.Buttons, Vcl.Clipbrd;

const
  HTEDITBUTTONL = HTSIZE + 100;
  HTEDITBUTTONR = HTSIZE + 101;
  HTUPDOWN = HTSIZE + 102;

  HTBUTTON1 = HTOBJECT + 100;
  HTBUTTON2 = HTOBJECT + 101;
  ScrollButtonSize = 15;

  // calendar
  BSize = 30;
  RepeatInt = 100;
  CenturyOffset = 60;

constructor TscGPControlScrollBarOptions.Create;
begin
  inherited;

  FFillColor := clBtnFace;
  FFillColorAlpha := 100;
  FThumbColor := clWindowText;
  FThumbColorAlpha := 50;
  FThumbColorHotAlpha := 100;
  FThumbColorPressedAlpha := 150;
  FThumbRounded := False;
  FSize := 11;
  FPosition := scgpsbDefault;
end;

procedure TscGPControlScrollBarOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPControlScrollBarOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPControlScrollBarOptions then
  begin
    FFillColor := TscGPControlScrollBarOptions(Source).FillColor;
    FFillColorAlpha := TscGPControlScrollBarOptions(Source).FillColorAlpha;
    FThumbColor := TscGPControlScrollBarOptions(Source).ThumbColor;
    FThumbColorAlpha := TscGPControlScrollBarOptions(Source).ThumbColorAlpha;
    FThumbColorHotAlpha := TscGPControlScrollBarOptions(Source).ThumbColorHotAlpha;
    FThumbColorPressedAlpha := TscGPControlScrollBarOptions(Source).ThumbColorPressedAlpha;
    FThumbRounded := TscGPControlScrollBarOptions(Source).ThumbRounded;
    FSize := TscGPControlScrollBarOptions(Source).Size;
    FBorderWidth := TscGPControlScrollBarOptions(Source).BorderWidth;
    FPosition := TscGPControlScrollBarOptions(Source).Position;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPControlScrollBarOptions.SetPosition(Value: TscGPScrollBarPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetBorderWidth(Value: Integer);
begin
  if (FBorderWidth <> Value) and (Value >= 0) then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetThumbColor(Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetThumbColorAlpha(Value: Byte);
begin
  if FThumbColorAlpha <> Value then
  begin
    FThumbColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetThumbColorHotAlpha(Value: Byte);
begin
  if FThumbColorHotAlpha <> Value then
  begin
    FThumbColorHotAlpha := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetThumbColorPressedAlpha(Value: Byte);
begin
  if FThumbColorPressedAlpha <> Value then
  begin
    FThumbColorPressedAlpha := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetThumbRounded(Value: Boolean);
begin
  if FThumbRounded <> Value then
  begin
    FThumbRounded := Value;
    Changed;
  end;
end;

procedure TscGPControlScrollBarOptions.SetSize(Value: Integer);
begin
  if (FSize <> Value) and (Value > 6) then
  begin
    FSize := Value;
    Changed;
  end;
end;

constructor TscGPControlScrollBar.Create(AControl: TWinControl; AVertical: Boolean);
begin
  FControl := AControl;
  FDown := False;
  FScrollTimer := TTimer.Create(FControl);
  FScrollTimer.Enabled := False;
  FScrollTimer.OnTimer := OnScrollTimer;
  FScrollTimer.Interval := 500;
  FThumbState := scsNormal;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FPageSize := 0;
  FVertical := AVertical;
  FBoundsRect := Rect(0, 0, 0, 0);
end;

destructor TscGPControlScrollBar.Destroy;
begin
  FScrollTimer.Free;
  inherited;
end;

function TscGPControlScrollBar.GetOptions: TscGPControlScrollBarOptions;
begin
  Result := nil;
  if FControl is TscGPScrollingControl then
    Result := TscGPScrollingControl(FControl).ScrollBarOptions;
end;

procedure TscGPControlScrollBar.OnScrollTimer(Sender: TObject);
var
  Offset: Integer;
begin
  if not FDown then 
  begin
    FScrollTimer.Enabled := False;
    Exit;
  end;
  Offset := FPageSize;
  if Offset = 0 then
    Offset := 1;
  if FVertical then
  begin
    if ScrollMPos > ThumbRect.Bottom then
      SetPosition(FPosition + Offset, True)
    else
    if ScrollMPos < ThumbRect.Top then
      SetPosition(FPosition - Offset, True)
    else
    begin
      FScrollTimer.Enabled := False;
      FDown := False;
      Exit;
    end;
  end
  else
  begin
    if ScrollMPos > ThumbRect.Right then
      SetPosition(FPosition + Offset, True)
    else
    if ScrollMPos < ThumbRect.Left then
      SetPosition(FPosition - Offset, True)
    else
    begin
      FScrollTimer.Enabled := False;
      FDown := False;
      Exit;
    end;
  end;
  FScrollTimer.Interval := 100;
end;

function TscGPControlScrollBar.GetVisible: Boolean;
begin
  Result := FMax > FMin;
end;

procedure TscGPControlScrollBar.SetBoundsRect(ARect: TRect);
begin
  FBoundsRect := ARect;
end;

procedure TscGPControlScrollBar.SetParams(AMin, AMax, APageSize, APosition: Integer; AUpdate: Boolean);
begin
  FMin := AMin;
  FMax := AMax;
  if FMax < FMin then
    FMax := FMin;
  FPageSize := APageSize;
  if FPageSize > FMax - FMin then
    FPageSize := 0;
  FPosition := APosition;
  if FPosition > FMax - FPageSize + 1 then
    FPosition := FMax - FPageSize + 1;
  if AUpdate then
    RePaint;
end;

procedure TscGPControlScrollBar.SetPosition(APosition: Integer; AUpdate: Boolean);
begin
  if APosition < FMin then
    APosition := FMin;
  if APosition > FMax - FPageSize + 1 then
    APosition := FMax - FPageSize + 1;
  if FPosition <> APosition then
  begin
    FPosition := APosition;
    if AUpdate and Visible then
    begin
      if Assigned(FOnChange) then
        FOnChange(Self);
      RePaint;
    end;
  end;
end;

procedure TscGPControlScrollBar.MouseLeave;
begin
  if FThumbState = scsHot then
  begin
    FThumbState := scsNormal;
    RePaint;
  end;
end;

procedure TscGPControlScrollBar.MouseMove(X, Y: Integer);
var
  Off, Pos, TX, TY: Integer;
  kf: Double;
begin
  if FDown then
  begin
    if FVertical then
      ScrollMPos := Y
    else
      ScrollMPos := X;
    FScrollTimer.Enabled := ScrollBarRect.Contains(Point(X, Y));
  end
  else
  if FThumbState = scsPressed then
  begin
    if FVertical
    then
    begin
      Off := Y - OMPos;
      TY := ThumbPos + Off;
      if TY < TrackRect.Top then
        TY := TrackRect.Top;
      if TY + ThumbRect.Height > TrackRect.Bottom then
        TY := TrackRect.Bottom - ThumbRect.Height;
      kf := (TY - TrackRect.Top) / (TrackRect.Height - ThumbRect.Height);
      Pos := Round((FMax - FMin - FPageSize + 1) * kf);
      SetPosition(Pos, True);
    end
    else
    begin
      Off := X - OMPos;
      TX := ThumbPos + Off;
      if TX < TrackRect.Left then
        TX := TrackRect.Left;
      if TX + ThumbRect.Width > TrackRect.Right then
        TX := TrackRect.Right - ThumbRect.Width;
      kf := (TX - TrackRect.Left) / (TrackRect.Width - ThumbRect.Width);
      Pos := Round((FMax - FMin - FPageSize + 1) * kf);
      SetPosition(Pos, True);
    end;
  end
  else
  if ThumbRect.Contains(Point(X, Y)) and (FThumbState = scsNormal) then
  begin
    FThumbState := scsHot;
    RePaint;
  end
  else
  if not ThumbRect.Contains(Point(X, Y)) and (FThumbState = scsHot) then
  begin
    FThumbState := scsNormal;
    RePaint;
  end;
end;

procedure TscGPControlScrollBar.MouseUp(X, Y: Integer);
begin
  if FThumbState = scsPressed then
  begin
    if ThumbRect.Contains(Point(X, Y)) then
      FThumbState := scsHot
    else
      FThumbState := scsNormal;
    FDown := False;
    if (FScrollTimer <> nil) and FScrollTimer.Enabled then
      FScrollTimer.Enabled := False;
    RePaint;
  end
  else
  if FDown then
  begin
    OnScrollTimer(Self);
    FDown := False;
    FScrollTimer.Enabled := False;
  end;
end;

procedure TscGPControlScrollBar.MouseDown(X, Y: Integer);
begin
  if ThumbRect.Contains(Point(X, Y)) then
  begin
    FThumbState := scsPressed;
    if FVertical then
    begin
      OMPos := Y;
      ThumbPos := ThumbRect.Top;
    end
    else
    begin
      OMPos := X;
      ThumbPos := ThumbRect.Left;
    end;
    FDownPosition := Position;
    RePaint;
  end
  else
  if ScrollBarRect.Contains(Point(X, Y)) then
  begin
    if FVertical then
      ScrollMPos := Y
    else
      ScrollMPos := X;
    FDown := True;
    FScrollTimer.Enabled := True;
    FScrollTimer.Interval := 500;
  end;
end;

function TscGPControlScrollBar.GetScrollBarRect: TRect;
begin
  Result := Rect(FBoundsRect.Left, FBoundsRect.Top,
    FBoundsRect.Left + FBoundsRect.Right,
    FBoundsRect.Top + FBoundsRect.Bottom);
end;
    
procedure TscGPControlScrollBar.Draw(G: TGPGraphics);
var
  R, ThumbR: TGPRectF;
  Options: TscGPControlScrollBarOptions;
  B: TGPSolidBrush;
  LArc, RArc: TGPRectF;
  FillPath: TGPGraphicsPath;
  C: Cardinal;
  Kf: Double;
  I, J, XMax: Double;
begin
  if not Visible then Exit;
  if FBoundsRect.Right * FBoundsRect.Bottom <= 0  then Exit;
  if FControl = nil then Exit;
  R := RectToGPRect(Rect(FBoundsRect.Left, FBoundsRect.Top, 
    FBoundsRect.Left + FBoundsRect.Right,
    FBoundsRect.Top + FBoundsRect.Bottom));
  Options := GetOptions;
  if Options = nil then
    Exit;
  B := TGPSolidBrush.Create(0);
  // fill background
  if (Options.FillColor <> clNone) and (Options.FillColorAlpha > 0) then
  begin
    C := ColorToGPColor(GetStyleColor(Options.FillColor), Options.FillColorAlpha);
    B.SetColor(C);
    G.FillRectangle(B, R);
  end;
  InflateGPRect(R, -Options.BorderWidth, -Options.BorderWidth);
  TrackRect := Rect(Round(R.X), Round(R.Y),
    Round(R.X + R.Width), Round(R.Y + R.Height));
  ThumbRect := Rect(0, 0, 0, 0);
  // calc thumb rect
  if FVertical then
  begin
    I := R.Height;
    J := FMax - FMin + 1;
    if J = 0 then kf := 0 else kf := FPageSize / J;
    J := Round(I * kf);
    if J < R.Width * 2 then j := R.Width * 2;
    XMax := FMax - FPageSize + 1;
    if XMax - FMin > 0 then
      kf := (FPosition - FMin) / (XMax - FMin)
    else
      kf := 0;
    I := Round((R.Height - J) * kf);
    ThumbR.X := R.X;
    ThumbR.Y := I + R.Y;
    ThumbR.Width := R.Width;
    ThumbR.Height := J;
    //
    ThumbRect := Rect(Round(ThumbR.X), Round(ThumbR.Y),
      Round(ThumbR.X + ThumbR.Width),
      Round(ThumbR.Y + ThumbR.Height));
  end
  else
  begin
    I := R.Width;
    J := FMax - FMin + 1;
    if J = 0 then kf := 0 else kf := FPageSize / J;
    J := Round(I * kf);
    if J < R.Height * 2 then j := R.Height * 2;
    XMax := FMax - FPageSize + 1;
    if XMax - FMin > 0 then
      kf := (FPosition - FMin) / (XMax - FMin)
    else
      kf := 0;
    I := Round((R.Width - J) * kf);
    ThumbR.X := I + R.X;
    ThumbR.Y := R.Y;
    ThumbR.Width := J;
    ThumbR.Height := R.Height;
    //
    ThumbRect := Rect(Round(ThumbR.X), Round(ThumbR.Y),
      Round(ThumbR.X + ThumbR.Width),
      Round(ThumbR.Y + ThumbR.Height));
  end;
  // draw thumb
  C := ColorToGPColor(GetStyleColor(Options.ThumbColor), Options.ThumbColorAlpha);
  case FThumbState of
    scsHot:
      C := ColorToGPColor(GetStyleColor(Options.ThumbColor), Options.ThumbColorHotAlpha);
    scsPressed:
      C := ColorToGPColor(GetStyleColor(Options.ThumbColor), Options.ThumbColorPressedAlpha);
  end;
  B.SetColor(C);
  if not Options.ThumbRounded then
  begin
    G.FillRectangle(B, ThumbR);
  end
  else
  begin
    FillPath := TGPGraphicsPath.Create();
    if FVertical then
    begin
      LArc.X := ThumbR.X;
      LArc.Y := ThumbR.Y;
      LArc.Width := ThumbR.Width;
      LArc.Height := LArc.Width;
      RArc.X := ThumbR.X;
      RArc.Y := ThumbR.Y + ThumbR.Height - ThumbR.Width;
      RArc.Width := ThumbR.Width;
      RArc.Height := RArc.Width;
      FillPath.StartFigure;
      FillPath.AddArc(LArc, -180, 180);
      FillPath.AddArc(RArc, 0, 180);
      FillPath.CloseFigure;
    end
    else
    begin
      LArc.X := ThumbR.X;
      LArc.Y := ThumbR.Y;
      LArc.Height := ThumbR.Height;
      LArc.Width := LArc.Height;
      RArc.X := ThumbR.X + ThumbR.Width - ThumbR.Height;
      RArc.Y := ThumbR.Y;
      RArc.Height := ThumbR.Height;
      RArc.Width := RArc.Height;
      FillPath.StartFigure;
      FillPath.AddArc(LArc, 90, 180);
      FillPath.AddArc(RArc, -90, 180);
      FillPath.CloseFigure;
    end;
    G.FillPath(B, FillPath);
    FillPath.Free;
  end;
  B.Free; 
end;

procedure TscGPControlScrollBar.RePaint;
begin
  if (FControl <> nil) and FControl.Visible and Visible then
  begin
    if FControl is TscGPScrollingControl then
      TscGPScrollingControl(FControl).RePaintControl
    else
      FControl.Invalidate;
  end;
end;

constructor TscGPScrollingControlOptions.Create;
begin
  inherited;
  FFrameColorAlpha := 255;
  FFillColorAlpha := 255;
  FFillColor2Alpha := 255;
  FFrameWidth := 2;
  FFillColor := clWindow;
  FFillColor2 := clWindow;
  FFrameColor := clBtnShadow;
  FFrameScaleWidth := False;
  FFillStyle := scgpsfColor;
  FFillGradientAngle := 90;
end;

procedure TscGPScrollingControlOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPScrollingControlOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPScrollingControlOptions then
  begin
    FFrameColorAlpha := TscGPScrollingControlOptions(Source).FrameColorAlpha;
    FFillColorAlpha :=  TscGPScrollingControlOptions(Source).FillColorAlpha;
    FFillColor2Alpha := TscGPScrollingControlOptions(Source).FillColor2Alpha;
    FFrameWidth := TscGPScrollingControlOptions(Source).FrameWidth;
    FFillColor := TscGPScrollingControlOptions(Source).FillColor;
    FFillColor2 := TscGPScrollingControlOptions(Source).FillColor2;
    FFrameColor := TscGPScrollingControlOptions(Source).FrameColor;
    FFrameScaleWidth := TscGPScrollingControlOptions(Source).FrameScaleWidth;
    FFillStyle := TscGPScrollingControlOptions(Source).FillStyle;
    FFillGradientAngle := TscGPScrollingControlOptions(Source).FillGradientAngle;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPScrollingControlOptions.SetFrameColorAlpha(Value: Byte);
begin
  if FFrameColorAlpha <> Value then
  begin
    FFrameColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFillColor2Alpha(Value: Byte);
begin
  if FFillColor2Alpha <> Value then
  begin
    FFillColor2Alpha := Value;
    Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value >= 0) then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFillColor2(Value: TColor);
begin
  if FFillColor2 <> Value then
  begin
    FFillColor2 := Value;
    Changed;
  end;
end;

procedure TscGPScrollingControlOptions.SetFrameColor(Value: TColor);
begin
   if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Changed;
  end;
end;

constructor TscGPScrollingControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csDoubleClicks, csReplicatable, csPannable, csGestures];
  FOptions := TscGPScrollingControlOptions.Create;
  FOptions.OnChange := OnControlChange;
  FScrollBarOptions := TscGPControlScrollBarOptions.Create;
  FScrollBarOptions.OnChange := OnScrollOptionsChange;
  FTransparentBackground := True;

  FWallpapers := nil;
  FCustomImages := nil;
  FCustomImageIndex := -1;
  FWallpaperIndex := -1;

  Color := clBtnFace;

  FHTouchBegin := 0;
  FHTouchEnd := 0;
  FVTouchBegin := 0;
  FVTouchEnd := 0;

  InitTouch;
end;

destructor TscGPScrollingControl.Destroy;
begin
  if FVertScrollBar <> nil then
  begin
    FVertScrollBar.Free;
    FVertScrollBar := nil;
  end;
  if FHorzScrollBar <> nil then
  begin
    FHorzScrollBar.Free;
    FHorzScrollBar := nil;
  end;
  FOptions.Free;
  FScrollBarOptions.Free;
  inherited;
end;

procedure TscGPScrollingControl.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscGPScrollingControl.SetCustomImageIndex(Value: Integer);
begin
  if FCustomImageIndex <> Value then
  begin
    FCustomImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGPScrollingControl.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscGPScrollingControl.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
  end;
end;

procedure TscGPScrollingControl.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGPScrollingControl.InitTouch;
begin
  Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions + [igoPanSingleFingerVertical,
    igoPanSingleFingerHorizontal, igoPanInertia];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [igPan, igPressAndTap];
end;

procedure TscGPScrollingControl.CMGesture(var Message: TCMGesture);
var
  Offset: Integer;
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
  begin
    if gfBegin in Message.Info^.Flags
    then
      FVTouchBegin := Message.Info^.Location.Y
    else
    begin
      FVTouchEnd := Message.Info^.Location.Y;
      Offset := FVTouchEnd - FVTouchBegin;
      FVTouchBegin := FVTouchEnd;
      FVertScrollBar.SetPosition(FVertScrollBar.Position - Offset, True);
    end;
  end;
  if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
  begin
    if gfBegin in Message.Info^.Flags
    then
      FHTouchBegin := Message.Info^.Location.X
    else
    begin
      FHTouchEnd := Message.Info^.Location.X;
      Offset := FHTouchEnd - FHTouchBegin;
      FHTouchBegin := FHTouchEnd;
      FHorzScrollBar.SetPosition(FHorzScrollBar.Position - Offset, True);
    end;
  end;
end;

function TscGPScrollingControl.GetContentRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
  InflateRect(Result, -FOptions.FFrameWidth, -FOptions.FFrameWidth);
  if FScrollBarOptions.Position = scgpsbDefault then
  begin
    if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
      Dec(Result.Bottom, ScrollBarOptions.Size);
    if (FVertScrollBar <> nil) and FVertScrolLBar.Visible then
    begin
      if BidiMode <> bdRightToLeft then
        Dec(Result.Right, ScrollBarOptions.Size)
      else
        Inc(Result.Left, ScrollBarOptions.Size);
    end;
  end;
end;

function TscGPScrollingControl.MouseInScrollBar(X, Y: Integer): Boolean;
begin
  Result := False;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
    Result := FVertScrollbar.ScrollBarRect.Contains(Point(X, Y));
  if not Result then
    if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
      Result := FHorzScrollbar.ScrollBarRect.Contains(Point(X, Y));
end;

function TscGPScrollingControl.IsScrollBarCaptured: Boolean;
begin
  Result := False;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
    Result := FVertScrollBar.FDown or (FVertScrollBar.FThumbState = scsPressed);
  if not Result then
    if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
    Result := FHorzScrollBar.FDown or (FHorzScrollBar.FThumbState = scsPressed);
end;

procedure TscGPScrollingControl.GetScrollInfo;
begin
end;

procedure TscGPScrollingControl.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPScrollingControl.OnScrollOptionsChange(Sender: TObject);
begin
  if ((FVertScrollBar <> nil) and (FVertScrollBar.Visible)) or
     ((FHorzScrollBar <> nil) and (FHorzScrollBar.Visible))
  then
    RePaintControl;
end;

procedure TscGPScrollingControl.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  if FOptions.FFrameScaleWidth and (FOptions.FFrameWidth > 0) then
    FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);
  if FScrollBarOptions <> nil then
  begin
    FScrollBarOptions.FSize := MulDiv(FScrollBarOptions.FSize, M, D);
    FScrollBarOptions.FBorderWidth := MulDiv(FScrollBarOptions.FBorderWidth, M, D);
  end;
end;

procedure TscGPScrollingControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not
      (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscGPScrollingControl.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPScrollingControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
    FVertScrollBar.MouseLeave;
  if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
    FHorzScrollBar.MouseLeave;
end;

procedure TscGPScrollingControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TscGPScrollingControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
    FVertScrollBar.MouseDown(X, Y);
  if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
    FHorzScrollBar.MouseDown(X, Y);
end;

procedure TscGPScrollingControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
    FVertScrollBar.MouseUp(X, Y);
  if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
    FHorzScrollBar.MouseUp(X, Y);
end;

procedure TscGPScrollingControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
    FVertScrollBar.MouseMove(X, Y);
  if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
    FHorzScrollBar.MouseMove(X, Y);
end;

procedure TscGPScrollingControl.DrawContent(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect);
begin

end;

procedure TscGPScrollingControl.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, ClRect: TRect;
  G: TGPGraphics;
  B: TGPBrush;
  B1: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, R2: TGPRectF;
  C, C2: Cardinal;
  Col: TColor;
begin
  R := Rect(0, 0, Width, Height);

  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  P := TGPPen.Create(0, FOptions.FFrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  FillR := RectToGPRect(R);
  FrameR := RectToGPRect(R);
  ClRect := R;
  if FOptions.FFrameWidth > 0 then
  begin
    InflateGPRect(FrameR, -FOptions.FFrameWidth / 2, -FOptions.FFrameWidth / 2);
    InflateGPRect(FillR, -FOptions.FFrameWidth, -FOptions.FFrameWidth);
    InflateRect(ClRect,  -FOptions.FFrameWidth, -FOptions.FFrameWidth);
  end;
 
  B := nil;

  if (FOptions.FFillColor <> clNone) and (FOptions.FFillColorAlpha <> 0) then
  begin
    if FOptions.FFillStyle = scgpsfColor then
    begin
      C := ColorToGPColor(GetStyleColor(FOptions.FFillColor), FOptions.FFillColorAlpha);
      B := TGPSolidBrush.Create(C);
    end
    else
    begin
      Col := GetStyleColor(FOptions.FFillColor);
      C := ColorToGPColor(Col,FOptions.FFillColorAlpha);
      Col := GetStyleColor(FOptions.FFillColor2);
      C2 := ColorToGPColor(Col, FOptions.FFillColor2Alpha);
      R2 := FillR;
      InflateGPRect(R2, 1, 1);
      B := TGPLinearGradientBrush.Create(R2, C, C2, FOptions.FFillGradientAngle);
    end;
  end;

  try
    if (FOptions.FFillColor <> clNone) and (FOptions.FFillColorAlpha <> 0) and (B <> nil) then
    begin
      G.FillRectangle(B, FillR);
    end;
    if (FOptions.FFrameWidth > 0) and (FOptions.FFrameColor <> clNone) and (FOptions.FFrameColorAlpha <> 0) then
    begin
      C := ColorToGPColor(GetStyleColor(FOptions.FFrameColor), FOptions.FFrameColorAlpha);
      P.SetColor(C);
      G.DrawRectangle(P, FrameR);
    end;
    GetScrollInfo;
    // draw wallpaper
    if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
    begin
      R := Rect(0, 0, Width, Height);
      InflateRect(R, -FOptions.FrameWidth, -FOptions.FrameWidth);
      FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
    end;
    // draw content
    DrawContent(ACanvas, G, GetContentRect);
    // draw scrollbars
    if BidiMode <> bdRightToLeft then
    begin
      if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
      begin
       if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
          FVertScrollBar.BoundsRect := Rect(ClRect.Right - FScrollBarOptions.Size, ClRect.Top,
          FScrollBarOptions.Size, ClRect.Height -  FScrollBarOptions.Size)
       else
         FVertScrollBar.BoundsRect := Rect(ClRect.Right - FScrollBarOptions.Size, ClRect.Top,
           FScrollBarOptions.Size, ClRect.Height);
       FVertScrollBar.Draw(G);
      end;
      if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
      begin
        if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
          FHorzScrollBar.BoundsRect := Rect(ClRect.Left, ClRect.Bottom - FScrollBarOptions.Size,
            ClRect.Width - FScrollBarOptions.Size, FScrollBarOptions.Size)
        else
          FHorzScrollBar.BoundsRect := Rect(ClRect.Left, ClRect.Bottom - FScrollBarOptions.Size,
            ClRect.Width, FScrollBarOptions.Size);
        FHorzScrollBar.Draw(G);
      end;
    end
    else
    begin
      if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
      begin
       if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
          FVertScrollBar.BoundsRect := Rect(ClRect.Left, ClRect.Top,
          FScrollBarOptions.Size, ClRect.Height -  FScrollBarOptions.Size)
       else
         FVertScrollBar.BoundsRect := Rect(ClRect.Left, ClRect.Top,
           FScrollBarOptions.Size, ClRect.Height);
       FVertScrollBar.Draw(G);
      end;
      if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
      begin
        if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
          FHorzScrollBar.BoundsRect := Rect(ClRect.Left + FScrollBarOptions.Size, ClRect.Bottom - FScrollBarOptions.Size,
            ClRect.Width, FScrollBarOptions.Size)
        else
          FHorzScrollBar.BoundsRect := Rect(ClRect.Left, ClRect.Bottom - FScrollBarOptions.Size,
            ClRect.Width, FScrollBarOptions.Size);
        FHorzScrollBar.Draw(G);
      end;
    end;
    // sizebox
    if (FVertScrollBar <> nil) and FVertScrollBar.Visible and
       (FHorzScrollBar <> nil) and FHorzScrollBar.Visible and
       (FScrollBarOptions.FillColor <> clNone) and (FScrollBarOptions.FillColorAlpha > 0)
    then
    begin
      C := ColorToGPColor(GetStyleColor(FScrollBarOptions.FillColor), FScrollBarOptions.FillColorAlpha); 
      B1 := TGPSolidBrush.Create(C);
      FillR.X := FVertScrollBar.BoundsRect.Left;
      FillR.Y := FVertScrollBar.BoundsRect.Top + FVertScrollBar.BoundsRect.Bottom;
      FillR.Width := FVertScrollBar.BoundsRect.Right;
      FillR.Height := FillR.Width; 
      G.FillRectangle(B1, FillR);
      B1.Free;
    end;
  finally
    P.Free;
    if B <> nil then
      B.Free;
    G.Free;
    FramePath.Free;
    FillPath.Free;
  end;

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomImageIndex) then
  begin
    R := Rect(0, 0, Width, Height);
    InflateRect(R, -FOptions.FrameWidth, -FOptions.FrameWidth);
    FCustomImages.Draw(ACanvas, R, FCustomImageIndex, FScaleFactor);
  end;
end;

procedure TscGPScrollingControl.WMSIZE(var Msg: TMessage);
begin
  inherited;
  RePaint;
end;

constructor TscGPSelectionOptions.Create;
begin
  inherited;
  Focused := False;
  FColor := clHighLight;
  FColorAlpha := 220;
  FFillStyle := scgpsfColor;
  FGradientAngle := 90;
  FFocusedColor := clHighLight;
  FFocusedColorAlpha := 255;
  FFocusedFillStyle := scgpsfColor;
  FFocusedGradientAngle := 90;
  FFontColor := clHighLightText;
  FFocusedFontColor := clHighLightText;
end;

procedure TscGPSelectionOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPSelectionOptions then
  begin
    FColor := TscGPSelectionOptions(Source).Color;
    FColorAlpha := TscGPSelectionOptions(Source).ColorAlpha;
    FFillStyle := TscGPSelectionOptions(Source).FillStyle;
    FGradientAngle := TscGPSelectionOptions(Source).GradientAngle;
    FFocusedColor := TscGPSelectionOptions(Source).FocusedColor;
    FFocusedColorAlpha := TscGPSelectionOptions(Source).FocusedColorAlpha;
    FFocusedFillStyle := TscGPSelectionOptions(Source).FocusedFillStyle;
    FFocusedGradientAngle := TscGPSelectionOptions(Source).FocusedGradientAngle;
    FFontColor := TscGPSelectionOptions(Source).FontColor;
    FFocusedFontColor := TscGPSelectionOptions(Source).FocusedFontColor;
  end
  else
    inherited Assign(Source);
end;

function TscGPSelectionOptions.GetColor: TColor;
begin
  if Focused then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := GetStyleColor(FColor);
end;

function TscGPSelectionOptions.GetColorAlpha: Byte;
begin
  if Focused then
    Result := FFocusedColorAlpha
  else
    Result := FColorAlpha;
end;

function TscGPSelectionOptions.GetFillStyle: TscGPShapeFillStyle;
begin
  if Focused then
    Result := FFocusedFillStyle
  else
    Result := FFillStyle;
end;

function TscGPSelectionOptions.GetGradientAngle: Integer;
begin
  if Focused then
    Result := FFocusedGradientAngle
  else
    Result := FGradientAngle;
end;

function TscGPSelectionOptions.GetFontColor: TColor;
begin
  if Focused then
    Result := GetStyleColor(FFocusedFontColor)
  else
    Result := GetStyleColor(FFontColor);
end;

procedure TscGPSelectionOptions.SetFontColor(Value: TColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetFocusedFontColor(Value: TColor);
begin
  if FFocusedFontColor <> Value then
  begin
    FFocusedFontColor := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetColorAlpha(Value: Byte);
begin
  if FColorAlpha <> Value then
  begin
    FColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetGradientAngle(Value: Integer);
begin
  if FGradientAngle <> Value then
  begin
    FGradientAngle := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetFocusedColorAlpha(Value: Byte);
begin
  if FFocusedColorAlpha <> Value then
  begin
    FFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetFocusedFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFocusedFillStyle <> Value then
  begin
    FFocusedFillStyle := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.SetFocusedGradientAngle(Value: Integer);
begin
  if FFocusedGradientAngle <> Value then
  begin
    FFocusedGradientAngle := Value;
    Changed;
  end;
end;

procedure TscGPSelectionOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPHeaderOptions.Create;
begin
  inherited;
  FColor := clBtnFace;
  FColorAlpha := 220;
  FFillStyle := scgpsfColor;
  FGradientAngle := 90;
  FMargin := 5;
end;

procedure TscGPHeaderOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPHeaderOptions then
  begin
    FColor := TscGPHeaderOptions(Source).Color;
    FColorAlpha := TscGPHeaderOptions(Source).ColorAlpha;
    FFillStyle := TscGPHeaderOptions(Source).FillStyle;
    FGradientAngle := TscGPHeaderOptions(Source).GradientAngle;
    FMargin := TscGPHeaderOptions(Source).Margin;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPHeaderOptions.OnChanged(Sender: TObject);
begin
  Changed;
end;

procedure TscGPHeaderOptions.SetMargin(Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TscGPHeaderOptions.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TscGPHeaderOptions.SetColorAlpha(Value: Byte);
begin
  if FColorAlpha <> Value then
  begin
    FColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPHeaderOptions.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    Changed;
  end;
end;

procedure TscGPHeaderOptions.SetGradientAngle(Value: Integer);
begin
  if FGradientAngle <> Value then
  begin
    FGradientAngle := Value;
    Changed;
  end;
end;

procedure TscGPHeaderOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPListCheckBoxOptions.Create;
begin
  inherited;

  FNormalColor := clWindow;
  FDisabledColor := clWindow;
  FFrameNormalColor := clWindowText;
  FFrameDisabledColor := clWindowText;

  FCheckedNormalColor := clWindow;
  FCheckedDisabledColor := clWindow;
  FCheckedFrameNormalColor := clWindowText;
  FCheckedFrameDisabledColor := clWindowText;

  FCheckMarkNormalColor := clWindowText;
  FCheckMarkDisabledColor := clWindowText;

  FOnChange := nil;
  FState := scsNormal;
  FChecked := False;

  FNormalColorAlpha := 200;
  FDisabledColorAlpha := 125;

  FCheckedNormalColorAlpha := 200;
  FCheckedDisabledColorAlpha := 125;

  FFrameNormalColorAlpha := 100;
  FFrameDisabledColorAlpha := 50;

  FCheckedFrameNormalColorAlpha := 100;
  FCheckedFrameDisabledColorAlpha := 50;

  FCheckMarkNormalColorAlpha := 255;
  FCheckMarkDisabledColorAlpha := 125;

  FShapeSize := 20;
  FFrameWidth := 2;
  FCheckMarkThickness := 2;

  FScaleCheckMarkThickness := True;
  FScaleFrameWidth := False;
end;

procedure TscGPListCheckBoxOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPListCheckBoxOptions then
  begin
    FNormalColor := TscGPListCheckBoxOptions(Source).FNormalColor;
    FDisabledColor := TscGPListCheckBoxOptions(Source).FDisabledColor;
    FFrameNormalColor := TscGPListCheckBoxOptions(Source).FFrameNormalColor;
    FFrameDisabledColor := TscGPListCheckBoxOptions(Source).FFrameDisabledColor;
    FNormalColorAlpha := TscGPListCheckBoxOptions(Source).FNormalColorAlpha;
    FDisabledColorAlpha := TscGPListCheckBoxOptions(Source).FDisabledColorAlpha;
    FFrameNormalColorAlpha := TscGPListCheckBoxOptions(Source).FFrameNormalColorAlpha;
    FFrameDisabledColorAlpha := TscGPListCheckBoxOptions(Source).FFrameDisabledColorAlpha;

    FCheckedNormalColor := TscGPListCheckBoxOptions(Source).FCheckedNormalColor;
    FCheckedDisabledColor := TscGPListCheckBoxOptions(Source).FCheckedDisabledColor;
    FCheckedFrameNormalColor := TscGPListCheckBoxOptions(Source).FCheckedFrameNormalColor;
    FCheckedFrameDisabledColor := TscGPListCheckBoxOptions(Source).FCheckedFrameDisabledColor;
    FCheckedNormalColorAlpha := TscGPListCheckBoxOptions(Source).FCheckedNormalColorAlpha;
    FCheckedDisabledColorAlpha := TscGPListCheckBoxOptions(Source).FCheckedDisabledColorAlpha;
    FCheckedFrameNormalColorAlpha := TscGPListCheckBoxOptions(Source).FCheckedFrameNormalColorAlpha;
    FCheckedFrameDisabledColorAlpha := TscGPListCheckBoxOptions(Source).FCheckedFrameDisabledColorAlpha;

    FFrameWidth := TscGPListCheckBoxOptions(Source).FFrameWidth;
    FCheckMarkNormalColor := TscGPListCheckBoxOptions(Source).FCheckMarkNormalColor;
    FCheckMarkDisabledColor := TscGPListCheckBoxOptions(Source).FCheckMarkDisabledColor;

    FCheckMarkNormalColorAlpha := TscGPListCheckBoxOptions(Source).FCheckMarkNormalColorAlpha;
    FCheckMarkDisabledColorAlpha := TscGPListCheckBoxOptions(Source).FCheckMarkDisabledColorAlpha;
    FShapeSize := TscGPListCheckBoxOptions(Source).FShapeSize;
    FCheckMarkThickness := TscGPListCheckBoxOptions(Source).FCheckMarkThickness;

    FScaleCheckMarkThickness := TscGPListCheckBoxOptions(Source).FScaleCheckMarkThickness;
    FScaleFrameWidth := TscGPListCheckBoxOptions(Source).FScaleFrameWidth;

  end
  else
    inherited Assign(Source);
end;

 function TscGPListCheckBoxOptions.GetColorAlpha: Byte;
 begin
   if not FChecked then
   begin
     if FState = scsDisabled then
       Result := FDisabledColorAlpha
     else
       Result := FNormalColorAlpha;
   end
   else
   begin
     if FState = scsDisabled then
       Result := FCheckedDisabledColorAlpha
     else
       Result := FCheckedNormalColorAlpha;
   end;
 end;

 function TscGPListCheckBoxOptions.GetCheckMarkColorAlpha: Byte;
 begin
   if FState = scsDisabled then
     Result := FCheckMarkDisabledColorAlpha
   else
     Result := FCheckMarkNormalColorAlpha;
 end;

 function TscGPListCheckBoxOptions.GetFrameColorAlpha: Byte;
 begin
   if not FChecked then
   begin
     if FState = scsDisabled then
       Result := FFrameDisabledColorAlpha
     else
       Result := FFrameNormalColorAlpha;
   end
   else
   begin
     if FState = scsDisabled then
       Result := FCheckedFrameDisabledColorAlpha
     else
       Result := FCheckedFrameNormalColorAlpha;
   end;
 end;

procedure TscGPListCheckBoxOptions.SetCheckMarkThickness(Value: Integer);
begin
  if (FCheckMarkThickness <> Value) and (Value >= 1) then
  begin
    FCheckMarkThickness := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetShapeSize(Value: Integer);
begin
  if (FShapeSize <> Value) and (Value >= 10) then
  begin
    FShapeSize := Value;
    Changed;
  end;
end;

function TscGPListCheckBoxOptions.GetColor: TColor;
begin
  if not FChecked then
  begin
    if FState = scsDisabled then
      Result := DisabledColor
    else
      Result := NormalColor;
  end
  else
  begin
    if FState = scsDisabled then
      Result := CheckedDisabledColor
    else
      Result := CheckedNormalColor;
  end;
end;

function TscGPListCheckBoxOptions.GetFrameColor: TColor;
begin
  if not FChecked then
  begin
    if FState = scsDisabled then
      Result := FrameDisabledColor
    else
      Result := FrameNormalColor;
  end
  else
  begin
    if FState = scsDisabled then
      Result := CheckedFrameDisabledColor
    else
      Result := CheckedFrameNormalColor;
  end;
end;

function TscGPListCheckBoxOptions.GetCheckMarkColor: TColor;
begin
  if FState = scsDisabled then
    Result := CheckMarkDisabledColor
  else
    Result := CheckMarkNormalColor;
end;

function TscGPListCheckBoxOptions.GetNormalColor: TColor;
begin
  Result := GetStyleColor(FNormalColor);
end;

function TscGPListCheckBoxOptions.GetDisabledColor: TColor;
begin
  Result := GetStyleColor(FDisabledColor);
end;

function TscGPListCheckBoxOptions.GetFrameNormalColor: TColor;
begin
  Result := GetStyleColor(FFrameNormalColor);
end;

function TscGPListCheckBoxOptions.GetFrameDisabledColor: TColor;
begin
  Result := GetStyleColor(FFrameDisabledColor);
end;

function TscGPListCheckBoxOptions.GetCheckedNormalColor: TColor;
begin
  Result := GetStyleColor(FCheckedNormalColor);
end;

function TscGPListCheckBoxOptions.GetCheckedDisabledColor: TColor;
begin
  Result := GetStyleColor(FCheckedDisabledColor);
end;

function TscGPListCheckBoxOptions.GetCheckedFrameNormalColor: TColor;
begin
  Result := GetStyleColor(FCheckedFrameNormalColor);
end;

function TscGPListCheckBoxOptions.GetCheckedFrameDisabledColor: TColor;
begin
  Result := GetStyleColor(FCheckedFrameDisabledColor);
end;


function TscGPListCheckBoxOptions.GetCheckMarkNormalColor: TColor;
begin
  Result := GetStyleColor(FCheckMarkNormalColor);
end;

function TscGPListCheckBoxOptions.GetCheckMarkDisabledColor: TColor;
begin
  Result := GetStyleColor(FCheckMarkDisabledColor);
end;

procedure TscGPListCheckBoxOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetFrameNormalColor(Value: TColor);
begin
  if FFrameNormalColor <> Value then
  begin
    FFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetFrameDisabledColor(Value: TColor);
begin
  if FFrameDisabledColor <> Value then
  begin
    FFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedNormalColor(Value: TColor);
begin
  if FCheckedNormalColor <> Value then
  begin
    FCheckedNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedDisabledColor(Value: TColor);
begin
  if FCheckedDisabledColor <> Value then
  begin
    FCheckedDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedNormalColorAlpha(Value: Byte);
begin
   if FCheckedNormalColorAlpha <> Value then
  begin
    FCheckedNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedDisabledColorAlpha(Value: Byte);
begin
  if FCheckedDisabledColorAlpha <> Value then
  begin
    FCheckedDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedFrameNormalColor(Value: TColor);
begin
  if FCheckedFrameNormalColor <> Value then
  begin
    FCheckedFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedFrameDisabledColor(Value: TColor);
begin
  if FCheckedFrameDisabledColor <> Value then
  begin
    FCheckedFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetFrameNormalColorAlpha(Value: Byte);
begin
   if FFrameNormalColorAlpha <> Value then
  begin
    FFrameNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetFrameDisabledColorAlpha(Value: Byte);
begin
  if FFrameDisabledColorAlpha <> Value then
  begin
    FFrameDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedFrameNormalColorAlpha(Value: Byte);
begin
   if FCheckedFrameNormalColorAlpha <> Value then
  begin
    FCheckedFrameNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckedFrameDisabledColorAlpha(Value: Byte);
begin
  if FCheckedFrameDisabledColorAlpha <> Value then
  begin
    FCheckedFrameDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckMarkNormalColorAlpha(Value: Byte);
begin
   if FCheckMarkNormalColorAlpha <> Value then
  begin
    FCheckMarkNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckMarkDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckMarkNormalColor(Value: TColor);
begin
  if FCheckMarkNormalColor <> Value then
  begin
    FCheckMarkNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetCheckMarkDisabledColor(Value: TColor);
begin
  if FCheckMarkDisabledColor <> Value then
  begin
    FCheckMarkDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPListCheckBoxOptions.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value > 0) then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;


procedure TscGPListCheckBoxOptions.Changed;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPListBoxItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FIndent := 0;
  IsFilterVisible := True;
  FHeader := False;
  FImageIndex := -1;
  FCaption := '';
  FDetail := '';
  FEnabled := True;
  FChecked := False;
  Active := False;
  ItemRect := Rect(0, 0, 0, 0);
  CheckRect := ItemRect;
  FCustomColor := clNone;
  FCustomColorAlpha := 255;
  FCustomTextColor := clNone;
  FCustomDetailTextColor := clNone;
end;

destructor TscGPListBoxItem.Destroy;
begin
  inherited;
end;

procedure TscGPListBoxItem.Assign(Source: TPersistent);
begin
  if Source is TscGPListBoxItem then
  begin
    FImageIndex := TscGPListBoxItem(Source).ImageIndex;
    FCaption := TscGPListBoxItem(Source).Caption;
    FEnabled := TscGPListBoxItem(Source).Enabled;
    FChecked := TscGPListBoxItem(Source).Checked;
    FHeader := TscGPListBoxItem(Source).Header;
    FCustomTextColor := TscGPListBoxItem(Source).CustomTextColor;
    FCustomDetailTextColor := TscGPListBoxItem(Source).CustomDetailTextColor;
    FCustomColor := TscGPListBoxItem(Source).CustomColor;
    FCustomColorAlpha := TscGPListBoxItem(Source).CustomColorAlpha;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPListBoxItem.SetIndent;
begin
  if (FIndent <> Value) then
  begin
    if Value >= 0 then
      FIndent := Value;
    Changed(False);
  end;
end;


procedure TscGPListBoxItem.SetCustomColor(Value: TColor);
begin
  if Value <> FCustomColor then
  begin
    FCustomColor := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscGPListBoxItem.SetCustomColorAlpha(Value: Byte);
begin
  if Value <> FCustomColorAlpha then
  begin
    FCustomColorAlpha := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscGPListBoxItem.SetCustomTextColor(Value: TColor);
begin
  if Value <> FCustomTextColor then
  begin
    FCustomTextColor := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscGPListBoxItem.SetCustomDetailTextColor(Value: TColor);
begin
  if Value <> FCustomDetailTextColor then
  begin
    FCustomDetailTextColor := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscGPListBoxItem.SetChecked;
begin
  FChecked := Value;
  Changed(False);
end;

procedure TscGPListBoxItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed(False);
end;

procedure TscGPListBoxItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TscGPListBoxItem.SetHeader(Value: Boolean);
begin
  FHeader := Value;
  Changed(False);
end;

procedure TscGPListBoxItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  Changed(False);
end;

procedure TscGPListBoxItem.SetDetail(const Value: String);
begin
  FDetail := Value;
  Changed(False);
end;

procedure TscGPListBoxItem.SetData(const Value: TCustomData);
begin
  FData := Value;
end;

constructor TscGPListBoxItems.Create;
begin
  inherited Create(TscGPListBoxItem);
  ListBox := AListBox;
end;

function TscGPListBoxItems.GetOwner: TPersistent;
begin
  Result := ListBox;
end;

procedure  TscGPListBoxItems.Update(Item: TCollectionItem);
begin
  ListBox.RePaintControl;
end;

function TscGPListBoxItems.GetItem(Index: Integer):  TscGPListBoxItem;
begin
  Result := TscGPListBoxItem(inherited GetItem(Index));
end;

procedure TscGPListBoxItems.SetItem(Index: Integer; Value:  TscGPListBoxItem);
begin
  inherited SetItem(Index, Value);
  ListBox.RePaintControl;
end;

function TscGPListBoxItems.Add: TscGPListBoxItem;
begin
  Result := TscGPListBoxItem(inherited Add);
  ListBox.RePaintControl;
end;

function TscGPListBoxItems.Insert(Index: Integer): TscGPListBoxItem;
begin
  Result := TscGPListBoxItem(inherited Insert(Index));
  ListBox.RePaintControl;
end;

procedure TscGPListBoxItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
  ListBox.RePaintControl;
end;

procedure TscGPListBoxItems.Clear;
begin
  inherited Clear;
  ListBox.FItemIndex := -1;
  ListBox.RePaintControl;
end;

constructor TscGPCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVertScrollBar := TscGPControlScrollBar.Create(Self, True);
  FVertScrollBar.OnChange := OnVertScrollBarChange;
  FAutoComplete := True;

  FItemWordWrap := True;
  FItemShowEllipsis := False;

  FFilter := '';
  FSearchString := '';
  FSearchTimerEnabled := False;
  FPopupMode := False;
  FTimerMode := 0;
  FItems := TscGPListBoxItems.Create(Self);
  FSelectionOptions := TscGPSelectionOptions.Create;
  FSelectionOptions.OnChange := OptionsChanged;
  FHeaderOptions := TscGPHeaderOptions.Create;
  FHeaderOptions.OnChange := OptionsChanged;
  FCheckBoxOptions := TscGPListCheckBoxOptions.Create;
  FCheckBoxOptions.OnChange := OptionsChanged;

  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(Font);
  FHeaderFont.Style := [fsBold];
  FHeaderFont.OnChange := OptionsChanged;

  FDetailFont := TFont.Create;
  FDetailFont.Assign(Font);
  FDetailFont.Style := [];
  FDetailFont.Color := clWindowText;
  FDetailFont.OnChange := OptionsChanged;

  FDetailPosition := scgplbdBottom;
  FDetailWordWrap := False;

  FItemSpacing := 5;
  FItemMargin := 5;
  FIndentMargin := 10;

  FCheckOffset := 0;
  FShowCheckBoxes := False;
  FInUpdateItems := False;
  FClicksDisabled := False;
  FMouseMoveChangeIndex := False;
  FMouseDown := False;
  FShowLines := False;
  FMouseActive := -1;
  FScrollOffset := 0;
  FImages := nil;
  Width := 150;
  Height := 150;
  TabStop := True;
  FItemHeight := 30;
  FHeaderHeight := 20;
  FShowItemDetails := False;
  FMax := 0;
  FRealMax := 0;
  FOldHeight := -1;
  FItemIndex := -1;
  FLineColor := clWindowText;
  FLineColorAlpha := 20;
end;

destructor TscGPCustomListBox.Destroy;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 1);
  end;
  FItems.Free;
  FItems := nil;
  FSelectionOptions.Free;
  FHeaderOptions.Free;
  FCheckBoxOptions.Free;
  FHeaderFont.Free;
  FDetailFont.Free;
  inherited Destroy;
end;


procedure TscGPCustomListBox.Add(const Item: String);
var
  FItem: TscGPListBoxItem;
begin
  FItem := FItems.Add;
  FItem.Caption := Item;
end;

procedure TscGPCustomListBox.Add(Items: TStrings);
var
  I: Integer;
begin
  BeginUpdateItems;
  try
    for I := 0 to Items.Count - 1 do
      Add(Items[I]);
  finally
    EndUpdateItems;
  end;
end;

procedure TscGPCustomListBox.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure TscGPCustomListBox.Clear;
begin
  BeginUpdateItems;
  try
    FItems.Clear;
  finally
    EndUpdateItems;
  end;
end;


procedure TscGPCustomListBox.SetItemWordWrap(Value: Boolean);
begin
  if FItemWordWrap <> Value then
  begin
    FItemWordWrap := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetItemShowEllipsis(Value: Boolean);
begin
   if FItemShowEllipsis <> Value then
  begin
    FItemShowEllipsis := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetItemMargin(Value: Integer);
begin
  if (Value >= 5) and (FItemMargin <> Value) then
  begin
    FItemMargin := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetIndentMargin(Value: Integer);
begin
  if (Value >= 5) and (FIndentMargin <> Value) then
  begin
    FIndentMargin := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetItems(Value: TscGPListBoxItems);
begin
  FItems.Assign(Value);
  RePaintControl;
end;

procedure TscGPCustomListBox.OptionsChanged(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPCustomListBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FItemHeight := MulDiv(FItemHeight, M, D);
  FIndentMargin := MulDiv(FIndentMargin, M, D);
  FItemMargin := MulDiv(FItemMargin, M, D);
  FItemSpacing := MulDiv(FItemSpacing, M, D);
  FHeaderFont.Height := MulDiv(FHeaderFont.Height, M, D);
  FDetailFont.Height := MulDiv(FDetailFont.Height, M, D);
  FHeaderHeight := MulDiv(FHeaderHeight, M, D);
  FHeaderOptions.FMargin := MulDiv(FHeaderOptions.FMargin, M, D);
  FCheckBoxOptions.FShapeSize := MulDiv(FCheckBoxOptions.FShapeSize, M, D);
  if FCheckBoxOptions.ScaleCheckMarkThickness then
    FCheckBoxOptions.FCheckMarkThickness := MulDiv(FCheckBoxOptions.FCheckMarkThickness, M, D);
   if FCheckBoxOptions.ScaleFrameWidth then
    FCheckBoxOptions.FFrameWidth := MulDiv(FCheckBoxOptions.FFrameWidth, M, D);
end;

procedure TscGPCustomListBox.SetFilter(Value: String);
begin
  if FFilter <> Value then
  begin
    FFilter := Value;
    FScrollOffset := 0;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.Sort;
var
  I, J: Integer;
begin
  if FItems.Count < 2 then Exit;
  for I := 0 to FItems.Count - 2 do
    for J := I + 1 to FItems.Count - 1 do
    if FItems[I].Caption > FItems[J].Caption  then
      FItems[J].SetIndex(I);
end;

function TscGPCustomListBox.NextIndex(const S: string): Integer;
var
  I, J: Integer;
  FS, TS: String;
begin
  Result := -1;
  if FItems.Count = 0 then Exit;

  J := FItemIndex + 1;
  if J < 0 then J := 0;
  if J > FItems.Count - 1 then
    J := 0;

  FS := LowerCase(S);

  for I := J to FItems.Count - 1 do
    if not FItems[I].Header and FItems[I].Enabled then
    begin
      TS := LowerCase(FItems[I].Caption);
      if (Pos(FS, TS) = 1) then
      begin
        Result := I;
        Break;
      end
    end;

  if (Result = -1) and (J > 0) then
    for I := 0 to J - 1 do
    begin
      if not FItems[I].Header and FItems[I].Enabled then
      begin
        TS := LowerCase(FItems[I].Caption);
        if (Pos(FS, TS) = 1) then
        begin
          Result := I;
          Break;
        end
      end;
    end;
end;

function TscGPCustomListBox.IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
var
  I: Integer;
  FS, TS: String;
begin
  Result := -1;
  FS := LowerCase(S);
  for I := 0 to FItems.Count - 1 do
    if not FItems[I].Header then
    begin
      TS := LowerCase(FItems[I].Caption);
      if (not AStartOff and (TS = FS)) or
         (AStartOff and (Pos(FS, TS) = 1)) then
      begin
        Result := I;
        Break;
      end
    end;
end;

function TscGPCustomListBox.IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
var
  I: Integer;
  FS: String;
  TS: String;
begin
  Result := -1;
  FS := LowerCase(S);
  for I := 0 to FItems.Count - 1 do
  begin
    TS := LowerCase(FItems[I].Detail);
    if (not AStartOff and (TS = FS)) or
       (AStartOff and (Pos(FS, TS) = 1)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TscGPCustomListBox.EnableScrollTimer(Value: Integer);
begin
  if FTimerMode = 0 then
  begin
    FTimerMode := Value;
    KillTimer(Handle, 2);
    SetTimer(Handle, 2, 50, nil);
  end
  else
    FTimerMode := Value;
end;

procedure TscGPCustomListBox.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscGPCustomListBox.WMTimer;
begin
  inherited;
  if Message.TimerID = 7 then
  begin
    FSearchString := '';
    KillTimer(Handle, 7);
    FSearchTimerEnabled := False;
  end
  else
  if Message.TimerID = 2 then
  begin
    case FTimerMode of
      1: FindUp;
      2: FindDown;
    end;
  end;
end;

procedure TscGPCustomListBox.SetDetailWordWrap(Value: Boolean);
begin
  if FDetailWordWrap <> Value then
  begin
    FDetailWordWrap:= Value;
    if FShowItemDetails and (DetailPosition = scgplbdRight) then
      RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetDetailPosition(Value: TscGPListBoxDetailPosition);
begin
  if FDetailPosition <> Value then
  begin
    FDetailPosition := Value;
    if FShowItemDetails then
      RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetDetailFont(Value: TFont);
begin
  FDetailFont.Assign(Value);
  if FShowItemDetails then
    RePaintControl;
end;

procedure TscGPCustomListBox.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  RePaintControl;
end;

procedure TscGPCustomListBox.SetLineColorAlpha(Value: Byte);
begin
  if FLineColorAlpha <> Value then
  begin
    FLineColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.BeginUpdateItems;
begin
  FInUpdateItems := True;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TscGPCustomListBox.EndUpdateItems;
begin
  FInUpdateItems := False;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
  RePaintControl;
end;

procedure TscGPCustomListBox.SkinDrawCheckImage(X, Y: Integer; Cnvs: TCanvas; IR: TRect; DestCnvs: TCanvas);
begin
end;

procedure TscGPCustomListBox.SetShowCheckBoxes;
begin
  if FShowCheckBoxes <> Value then
  begin
    FShowCheckBoxes := Value;
    RePaintControl;
  end;
end;

function TscGPCustomListBox.CalcHeight;
begin
  if AItemCount > FItems.Count then AItemCount := FItems.Count;
  Result := AItemCount * ItemHeight;
  Result := Result + Height - GetContentRect.Height;
end;

procedure TscGPCustomListBox.SetShowLines;
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetItemSpacing(Value: Integer);
begin
  if FItemSpacing <> Value then
  begin
    FItemSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.SetHeaderHeight(Value: Integer);
begin
  if FHeaderHeight <> Value
  then
    begin
      FHeaderHeight := Value;
      RePaintControl;
    end;
end;

procedure TscGPCustomListBox.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TscGPCustomListBox.SetShowItemDetails(Value: Boolean);
begin
  if FShowItemDetails <> Value then
  begin
    FShowItemDetails := Value;
    RePaintControl;
  end;
end;

function TscGPCustomListBox.CanDrawContent: Boolean;
begin
  Result := Assigned(FItems) and (FItems.Count > 0);
end;

procedure TscGPCustomListBox.DrawContent(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect);
var
  I, SaveIndex: Integer;
begin
  CalcItemRects;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,
      FItemsRect.Left, FItemsRect.Top, FItemsRect.Right, FItemsRect.Bottom);
    for I := 0 to FItems.Count - 1 do
      if FItems[I].IsVisible and FItems[I].IsFilterVisible then DrawItem(I, ACanvas, G);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end; end;

procedure TscGPCustomListBox.CalcItemRects;
var
  I: Integer;
  X, Y, W, H: Integer;
  AColor: Boolean;
  FilterS: String;

function InFilter(Index: Integer): Boolean;
var
  S: String;
begin
  Result := True;
  if FItems[Index].Header or (FFilter = '') then
  begin
    FItems[Index].IsFilterVisible := True;
    Exit;
  end;
  S := LowerCase(FItems[Index].Caption);
  Result := Pos(FilterS, S) <> 0;
  if FShowItemDetails and not Result then
  begin
    S := LowerCase(FItems[Index].Detail);
    Result := Pos(FilterS, S) <> 0;
  end;
  FItems[Index].IsFilterVisible := Result;
end;

begin
  FilterS := '';
  if FFilter <> '' then
   FilterS := LowerCase(FFilter);
  FRealMax := 0;
  FItemsRect := GetContentRect;
  X := FItemsRect.Left;
  Y := FItemsRect.Top;
  W := FItemsRect.Width;
  AColor := False;
  for I := 0 to FItems.Count - 1 do
    with TscGPListBoxItem(FItems[I]) do
    if InFilter(I) then
    begin
      if not Header then H := ItemHeight else H := HeaderHeight;
      ItemRect := Rect(X, Y, X + W, Y + H);
      OffsetRect(ItemRect, 0, - FScrollOffset);
      IsVisible := RectToRect(ItemRect, FItemsRect);
      if not IsVisible and (ItemRect.Top <= FItemsRect.Top) and
        (ItemRect.Bottom >= FItemsRect.Bottom)
      then
        IsVisible := True;
      if IsVisible then FRealMax := ItemRect.Bottom;
      Y := Y + H;
      AColor := not AColor;
      if Header then AColor := False;
    end
    else
    begin
      IsVisible := False;
      ItemRect := Rect(0, 0, 0, 0);
    end;
  FMax := Y;
end;

procedure TscGPCustomListBox.Scroll(AScrollOffset: Integer);
begin
  FScrollOffset := AScrollOffset;
  RePaintControl;
end;

procedure TscGPCustomListBox.GetScrollInfo;
var
  AMin, AMax, APage, APosition: Integer;
begin
  CalcItemRects;
  AMin := 0;
  AMax := FMax - FItemsRect.Top - 1;
  APage := FItemsRect.Height;
  if AMax <= APage then
  begin
    APage := 0;
    AMax := 0;
  end;
  APosition := FScrollOffset;
  if FVertScrollBar <> nil then
    FVertScrollBar.SetParams(AMin, AMax, APage, APosition, False);
end;

procedure TscGPCustomListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if FOldHeight <> Height then
  begin
    CalcItemRects;
    if (FRealMax <= FItemsRect.Bottom) and (FScrollOffset > 0) then
    begin
      FScrollOffset := FScrollOffset - (FItemsRect.Bottom - FRealMax);
      if FScrollOffset < 0 then FScrollOffset := 0;
      CalcItemRects;
      RePaintControl;
     end;
  end;
  FOldHeight := Height;
end;

procedure TscGPCustomListBox.ScrollToItem(Index: Integer);
var
  R, R1: TRect;
begin
  if (Index < 0) or (Index > Items.Count - 1) then Exit;
  CalcItemRects;
  R1 := FItems[Index].ItemRect;
  R := R1;
  OffsetRect(R, 0, FScrollOffset);
  if (R1.Top <= FItemsRect.Top) then
  begin
    if (Index = 1) and FItems[Index - 1].Header then
      FScrollOffset := 0
    else
      FScrollOffset := R.Top - FItemsRect.Top;
    CalcItemRects;
    RePaintControl;
  end
  else
  if R1.Bottom >= FItemsRect.Bottom then
  begin
    FScrollOffset := R.Top;
    FScrollOffset := FScrollOffset - FItemsRect.Height + R.Height -
      Height + FItemsRect.Bottom;
    CalcItemRects;
    RePaintControl;
  end;
end;

procedure TscGPCustomListBox.OnVertScrollBarChange(Sender: TObject);
begin
  if (FVertScrollBar <> nil) then
  begin
   if FVertScrollBar.Position <= FVertScrollBar.Max - FVertScrollBar.PageSize + 1  then
     Scroll(FVertScrollBar.Position)
   else
     Scroll(FVertScrollBar.Max - FVertScrollBar.PageSize + 1);
  end;
end;

procedure TscGPCustomListBox.DrawHeaderItem(Index: Integer; Cnvs: TCanvas; G: TGPGraphics);
var
  R, R1: TGPRectF;
  C, C1, C2: Cardinal;
  HC: TColor;
  B: TGPBrush;
  TR: TRect;
begin
  if (FHeaderOptions.Color <> clNone) and (FHeaderOptions.ColorAlpha > 0) then
  begin
    R := RectToGPRect(FItems[Index].ItemRect);
    HC := GetStyleColor(FHeaderOptions.Color);
    C := ColorToGPColor(HC, FHeaderOptions.ColorAlpha);
    if FHeaderOptions.FillStyle = scgpsfColor then
      B := TGPSolidBrush.Create(C)
    else
    begin
      C1 := ColorToGPColor(LighterColor(HC, 25), FHeaderOptions.ColorAlpha);
      C2 := ColorToGPColor(DarkerColor(HC, 25), FHeaderOptions.ColorAlpha);
      R1 := R;
      InflateGPRect(R1, 1, 1);
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FHeaderOptions.GradientAngle);
    end;
    G.FillRectangle(B, R);
    B.Free;
  end;
  Cnvs.Font := FHeaderFont;
  Cnvs.Brush.Style := bsClear;
  Cnvs.Font.Color := GetStyleColor(FHeaderFont.Color);
  TR := FItems[Index].ItemRect;
  InflateRect(TR, -FHeaderOptions.Margin, 0);
  if FDrawTextMode = scdtmGDI then
    DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), TR,
      scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft))
  else
    GPDrawText(G, nil, Cnvs, TR, FItems[Index].Caption,
      scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft))
end;

procedure TscGPCustomListBox.DrawSelection(G: TGPGraphics; ARect: TRect);
var
  R, R1: TGPRectF;
  C, C1, C2: Cardinal;
  B: TGPBrush;
begin
  R := RectToGPRect(ARect);
  FSelectionOptions.Focused := Self.Focused;
  C := ColorToGPColor(FSelectionOptions.GetColor, FSelectionOptions.GetColorAlpha);
  if FSelectionOptions .GetFillStyle = scgpsfColor then
    B := TGPSolidBrush.Create(C)
  else
  begin
    C1 := ColorToGPColor(LighterColor(FSelectionOptions.GetColor, 25), FSelectionOptions.GetColorAlpha);
    C2 := ColorToGPColor(DarkerColor(FSelectionOptions.GetColor, 25), FSelectionOptions.GetColorAlpha);
    R1 := R;
    InflateGPRect(R1, 1, 1);
    B := TGPLinearGradientBrush.Create(R1, C1, C2, FSelectionOptions.GetGradientAngle);
  end;
  G.FillRectangle(B, R);
  B.Free;
end;

procedure TscGPCustomListBox.DrawCheckBox(G: TGPGraphics; ARect: TRect; AChecked: Boolean; AEnabled: Boolean);
var
  CR: TRect;
  FrameColor, FillColor, CheckColor: Cardinal;
  B: TGPSolidBrush;
  P: TGPPen;
  W: Integer;
  CheckPath: TGPGraphicsPath;
  FillR, FrameR: TGPRectF;
begin
  W := FCheckBoxOptions.ShapeSize;
  CR := Rect(ARect.Left + ARect.Width div 2 - W div 2,
    ARect.Top + ARect.Height div 2 - W div 2,
    ARect.Left + ARect.Width div 2 - W div 2 + W,
    ARect.Top + ARect.Height div 2 - W div 2 + W);
  FCheckBoxOptions.Checked := AChecked;
  if AEnabled then
    FCheckBoxOptions.State := scsNormal
  else
    FCheckBoxOptions.State := scsDisabled;
  FrameColor := ColorToGPColor(FCheckBoxOptions.FrameColor, FCheckBoxOptions.FrameColorAlpha);
  FillColor := ColorToGPColor(FCheckBoxOptions.Color, FCheckBoxOptions.ColorAlpha);
  CheckColor := ColorToGPColor(FCheckBoxOptions.CheckMarkColor, FCheckBoxOptions.CheckMarkColorAlpha);
  B := TGPSolidBrush.Create(0);
  P := TGPPen.Create(0, FCheckBoxOptions.FrameWidth);
  FillR := RectToGPRect(CR);
  FrameR := RectToGPRect(CR);
  InflateGPRect(FrameR, -FCheckBoxOptions.FrameWidth / 2, -FCheckBoxOptions.FrameWidth / 2);
  if FrameColor <> 0 then
  begin
    InflateGPRect(FillR, -FCheckBoxOptions.FrameWidth, -FCheckBoxOptions.FrameWidth);
  end;
  B.SetColor(FillColor);
  G.FillRectangle(B, FillR);
  P.SetColor(FrameColor);
  G.DrawRectangle(P, FrameR);
  if AChecked then
  begin
    W := FCheckBoxOptions.CheckMarkThickness;
    P.SetWidth(W);
    P.SetColor(CheckColor);
    InflateGPRect(FrameR,
      -FCheckBoxOptions.FrameWidth / 2 - W / 2,
       -FCheckBoxOptions.FrameWidth / 2 - W / 2);
    FrameR.Y:= FrameR.Y - W / 2 - 1;
    CheckPath := TGPGraphicsPath.Create;
    CheckPath.StartFigure;
    CheckPath.AddLine(FrameR.X, FrameR.Y + FrameR.Height - FrameR.Height / 3,
      FrameR.X + FrameR.Width / 3, FrameR.Y + FrameR.Height);
    CheckPath.AddLine(FrameR.X + FrameR.Width / 3, FrameR.Y + FrameR.Height,
      FrameR.X + FrameR.Width, FrameR.Y +  FrameR.Height / 4);
    G.DrawPath(P, CheckPath);
    CheckPath.Free;
  end;
end;

procedure TscGPCustomListBox.DrawItem(Index: Integer; Cnvs: TCanvas; G: TGPGraphics);
var
  R, R1, CR: TRect;
  CheckOffset: Integer;
  S: String;
  X, Y: Integer;
  C: TColor;
  PC: Cardinal;
  B: TGPSolidBrush;
  Flags: Cardinal;
begin
  // draw header
  if FItems[Index].Header then
  begin
    DrawHeaderItem(Index, Cnvs, G);
    Exit;
  end;
  R := FItems[Index].ItemRect;

  if (FItems[Index].FCustomColor <> clNone) and not FItems[Index].Active then
  begin
    C := GetStyleColor(FItems[Index].FCustomColor);
    PC := ColorToGPColor(C, FItems[Index].FCustomColorAlpha);
    B := TGPSolidBrush.Create(PC);
    G.FillRectangle(B,
      R.Left, R.Top, R.Width, R.Height);
    B.Free;
  end;

  if FShowLines and
    ((Index < FItems.Count - 1) or not ((FVertScrollBar <> nil) and FVertScrollBar.Visible))
  then
  begin
    C := GetStyleColor(FLineColor);
    PC := ColorToGPColor(C, FLineColorAlpha);
    B := TGPSolidBrush.Create(PC);
    G.FillRectangle(B,
      R.Left, R.Bottom - 1, R.Width, 1);
    B.Free;
  end;

  // draw selection
  if FItems[Index].Active and FItems[Index].Enabled then
    DrawSelection(G,  FItems[Index].ItemRect);

  if BidiMode = bdRightToLeft then
  begin
    Dec(R.Right, FItemMargin);
    Inc(R.Left, Round(2 * FScaleFactor));
  end
  else
  begin
    Inc(R.Left, FItemMargin);
    Dec(R.Right, Round(2 * FScaleFactor));
  end;

  if FItems[Index].Indent > 0 then
  begin
    if BidiMode <> bdRightToLeft then
      Inc(R.Left, FItems[Index].Indent * FIndentMargin)
    else
      Dec(R.Right, FItems[Index].Indent * FIndentMargin);
  end;
  Y := R.Bottom;
  // draw checkbox
  if FShowCheckBoxes then
  begin
    CR := R;
    CheckOffset := Self.FCheckBoxOptions.ShapeSize;
    if BidiMode <> bdRightToLeft then
      CR.Right := CR.Left + CheckOffset
    else
      CR.Left := CR.Right - CheckOffset;
    DrawCheckBox(G, CR, FItems[Index].Checked, FItems[Index].Enabled);
    FItems[Index].CheckRect := CR;
    if BidiMode <> bdRightToLeft then
    begin
      FCheckOffset := CR.Right + FItemSpacing;
      R.Left := FCheckOffset;
    end
    else
    begin
      FCheckOffset := Width - CR.Left + FItemSpacing;
      R.Right := Width - FCheckOffset;
    end;
  end;
  // draw detail
  if FShowItemDetails then
  begin
    Cnvs.Font := Self.DetailFont;
    Cnvs.Brush.Style := bsClear;

    if (FItems[Index].FCustomDetailTextColor <> clNone) and FItems[Index].Enabled and not FItems[Index].Active then
      C := GetStyleColor(Items[Index].FCustomDetailTextColor)
    else
    if not FItems[Index].Enabled then
    begin
      C := GetStyleColor(DetailFont.Color);
      if FDrawTextMode = scdtmGDI then
        if IsLightColor(C) then
          C := DarkerColor(C, 70)
        else
          C := LighterColor(C, 70);
    end
    else
    if FItems[Index].Active then
    begin
      if (FSelectionOptions.FFontColor = clHighLightText) and IsCustomStyle
      then
        C := GetSelectionTextColor // fix for some vcl styles
      else
        C := GetStyleColor(SelectionOptions.FontColor);

      if FDrawTextMode = scdtmGDI then
        if IsLightColor(C) then
          C := DarkerColor(C, 10)
        else
          C := LighterColor(C, 10);
    end
    else
    begin
      if seFont in StyleElements then
      begin
        C := GetStyleColor(DetailFont.Color);
        if FDrawTextMode = scdtmGDI then
          if IsLightColor(C) then
            C := DarkerColor(C, 40)
          else
            C := LighterColor(C, 35);
      end
      else
        C := DetailFont.Color;
    end;
    Cnvs.Font.Color := C;
    R1 := R;
    S := FItems[Index].Detail;
    if FDetailPosition = scgplbdBottom then
    begin
      R1.Top := R1.Bottom - Cnvs.TextHeight('Wq') - Round(5 * FScaleFactor);
      R.Bottom := R1.Top;
      Inc(R1.Left);
      if FImages <> nil then
      begin
        if BidiMode <> bdRightToLeft then
          Inc(R1.Left, FImages.Width + FItemSpacing)
        else
          Dec(R1.Right, FImages.Width + FItemSpacing)
      end;
      if S <> '' then
        if FDrawTextMode = scdtmGDI then
          DrawText(Cnvs.Handle, PChar(S), Length(S), R1,
            scDrawTextBidiModeFlags(DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft))
        else
        if FItems[Index].Enabled then
          GPDrawText(G, nil, Cnvs, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft), 160)
        else
          GPDrawText(G, nil, Cnvs, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft), 125);
    end
    else
    if S <> '' then
    begin
      if not FDetailWordWrap then
      begin
        if BidiMode <> bdRightToLeft then
          R1.Left := R1.Right - Cnvs.TextWidth(S) - Round(5 * FScaleFactor)
        else
          R1.Right := R1.Left + Cnvs.TextWidth(S) + Round(5 * FScaleFactor);
        if FDrawTextMode = scdtmGDI then
          scDrawText(Cnvs, S, R1, BidiMode = bdRightToLeft, True)
        else
        if FItems[Index].Enabled then
          GPDrawText(G, nil, Cnvs, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_NOPREFIX, BidiMode = bdRightToLeft), 160)
        else
          GPDrawText(G, nil, Cnvs, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_NOPREFIX, BidiMode = bdRightToLeft), 125);
      end
      else
      begin
        CR := Rect(0, 0, R1.Width div 3, R1.Height);
        if FDrawTextMode = scdtmGDI then
          DrawText(Cnvs.Handle, PChar(S), Length(S), CR,
              DT_LEFT or DT_NOPREFIX or DT_WORDBREAK or DT_CALCRECT)
        else
         GPDrawText(G, nil, Cnvs, CR, S,
              DT_LEFT or DT_NOPREFIX or DT_WORDBREAK or DT_CALCRECT);

        if BidiMode <> bdRightToLeft then
        begin
          R1.Right :=  R1.Right - Round(5 * FScaleFactor);
          R1.Left := R1.Right - CR.Width;
        end
        else
        begin
          R1.Left := R1.Left + Round(5 * FScaleFactor);
          R1.Right := R1.Left + CR.Width;
        end;
        R1.Top := R1.Top + R1.Height div 2 - CR.Height div 2;
        if FDrawTextMode = scdtmGDI then
          DrawText(Cnvs.Handle, PChar(S), Length(S), R1,
            scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK, BidiMode = bdRightToLeft))
        else
        if FItems[Index].Enabled then
          GPDrawText(G, nil, Cnvs, R1, S, scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK or DT_NOPREFIX,
            BidiMode = bdRightToLeft), 160)
        else
          GPDrawText(G, nil, Cnvs, R1, S, scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK or DT_NOPREFIX,
            BidiMode = bdRightToLeft), 125);
      end;

      if BidiMode <> bdRightToLeft then
        R.Right := R1.Left
      else
        R.Left := R1.Right;
    end;
  end;
  // draw image
  if FImages <> nil then
  begin
    R1 := R;
    R1.Bottom := Y;
    if BidiMode <> bdRightToLeft then
    begin
      R1.Right := R.Left + FImages.Width;
      Inc(R.Left, FItemSpacing + FImages.Width);
    end
    else
    begin
      R1.Left := R1.Right - FImages.Width;
      Dec(R.Right, FItemSpacing + FImages.Width);
    end;
    if (FItems[Index].ImageIndex >= 0) and
       (FItems[Index].ImageIndex < FImages.Count) then
    begin
      X := R1.Left;
      Y := R1.Top + R1.Height div 2 - FImages.Height div 2;
      if FItems[Index].Enabled then
         FImages.Draw(Cnvs, X, Y, FItems[Index].ImageIndex, True)
      else
      if FImages is TscCustomImageList then
        FImages.Draw(Cnvs, X, Y, FItems[Index].ImageIndex, False)
      else
        DrawBitmapFromImageList(Cnvs, X, Y, FImages,
          FItems[Index].ImageIndex, DisabledImageAlphaValue);
    end;
  end;

  // draw text
  Flags := DT_LEFT or DT_NOPREFIX;
  if FItemWordWrap then
   Flags := Flags or DT_WORDBREAK;
  if FItemShowEllipsis then
   Flags := Flags or DT_END_ELLIPSIS;

  Cnvs.Font := Self.Font;
  if (FItems[Index].FCustomTextColor <> clNone) and FItems[Index].Enabled and not FItems[Index].Active then
    Cnvs.Font.Color := GetStyleColor(Items[Index].FCustomTextColor)
  else
  if not FItems[Index].Enabled then
  begin
    C := GetStyleColor(Self.Font.Color);
    if FDrawTextMode = scdtmGDI then
      if IsLightColor(C) then
        C := DarkerColor(C, 50)
      else
        C := LighterColor(C, 50);
    Cnvs.Font.Color := C;
  end
  else
  if FItems[Index].Active then
  begin
    if (FSelectionOptions.FFontColor = clHighLightText) and IsCustomStyle
    then
      Cnvs.Font.Color := GetSelectionTextColor // fix for some vcl styles
    else
      Cnvs.Font.Color := GetStyleColor(FSelectionOptions.FFontColor);
  end
  else
    Cnvs.Font.Color := GetStyleColor(Self.Font.Color);

  S := FItems[Index].Caption;

  Cnvs.Brush.Style := bsClear;

  R1 := Rect(0, 0, R.Width, R.Height);
  if FDrawTextMode = scdtmGDI then
    DrawText(Cnvs.Handle, PChar(S), Length(S), R1, Flags or DT_CALCRECT)
  else
    GPDrawText(G, nil, Cnvs, R1, S, Flags or DT_CALCRECT);

  if R1.Height <= R.Height  then
  begin
    X := R.Left;
    Y := R.Top + R.Height div 2 - R1.Height div 2;
    if Y < R.Top then Y := R.Top;
    R := Rect(X, Y, R.Right - 2, Y + R1.Height);
  end
  else
  begin
    if FItemWordWrap and  FItemShowEllipsis then
      Flags := Flags or DT_EDITCONTROL;
  end;

  if FDrawTextMode = scdtmGDI then
    DrawText(Cnvs.Handle, PChar(S), Length(S), R,
      scDrawTextBidiModeFlags(Flags, BidiMode = bdRightToLeft))
  else
    if  FItems[Index].Enabled then
      GPDrawText(G, nil, Cnvs, R, S,
        scDrawTextBidiModeFlags(Flags or DT_VCENTER, BidiMode = bdRightToLeft))
    else
      GPDrawText(G, nil, Cnvs, R, S,
        scDrawTextBidiModeFlags(Flags or DT_VCENTER, BidiMode = bdRightToLeft), 125);
end;

procedure TscGPCustomListBox.InitItemIndex(Value: Integer);
var
  I: Integer;
begin
  if Value < 0 then
  begin
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
      FItems[FItemIndex].Active := False;
    FItemIndex := Value;
  end
  else
  if (Value >= 0) and (Value < FItems.Count) and ((FItems[Value].Header) or not FItems[Value].Enabled) then
  begin
    Exit;
  end
  else
  begin
    FItemIndex := Value;
    for I := 0 to FItems.Count - 1 do
      with FItems[I] do
      begin
        if I = FItemIndex then
          Active := True
        else
          Active := False;
      end;
    ScrollToItem(FItemIndex);
  end;
end;

procedure TscGPCustomListBox.SetItemIndex(Value: Integer);
var
  I: Integer;
  IsFind: Boolean;
begin
  if Value < 0 then
  begin
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
      FItems[FItemIndex].Active := False;
    FItemIndex := Value;
    RePaintControl;
  end
  else
  if (Value >= 0) and (Value < FItems.Count) and ((FItems[Value].Header) or not FItems[Value].Enabled) then
  begin
    Exit;
  end
  else
  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    IsFind := False;
    for I := 0 to FItems.Count - 1 do
      with FItems[I] do
      begin
        if I = FItemIndex then
        begin
          Active := True;
          IsFind := True;
        end
        else
          Active := False;
      end;
    RePaintControl;
    ScrollToItem(FItemIndex);
    if IsFind and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not FMouseDown then
    begin
       if Assigned(FOnItemClick) then
        FOnItemClick(Self);
      Change;
    end;
  end;
end;

procedure TscGPCustomListBox.Change;
begin

end;

procedure TscGPCustomListBox.Loaded;
begin
  inherited;
end;

function TscGPCustomListBox.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (ScrollBarOptions.Position = scgpsbOverContent) and
     (FVertScrollBar <> nil) and (FVertScrollBar.Visible) and
     FVertScrollBar.GetScrollBarRect.Contains(Point(X, Y))
  then
    Exit;

  for I := 0 to FItems.Count - 1 do
    if FItems[I].IsFilterVisible and FItems[I].IsVisible and
       PtInRect(FItems[I].ItemRect, Point(X, Y)) and (FItems[I].Enabled)
    then
      begin
        Result := I;
        Break;
      end;
end;

procedure TscGPCustomListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

procedure TscGPCustomListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft) then
  begin
    SetItemActive(I);
    FMouseDown := True;
    FMouseActive := I;
  end;
end;

procedure TscGPCustomListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
  CanClick: Boolean;
begin
  inherited;
  CanClick := FMouseDown;
  FMouseDown := False;
  StopScrollTimer;

  I := ItemAtPos(X, Y);
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft) then ItemIndex := I;

  if CanClick and Assigned(FOnItemClick) then
    FOnItemClick(Self);

  if FShowCheckBoxes and (I <> -1) and (I = ItemIndex) and
     FItems[I].CheckRect.Contains(Point(X, Y)) then
  begin
    Items[I].Checked := not Items[I].Checked;
    if Assigned(FOnItemCheckClick) then FOnItemCheckClick(Self);
  end;

  if CanClick then
    Change;
end;

procedure TscGPCustomListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  R: TRect;
begin
  inherited;
  if FItems.Count = 0 then Exit;
  R := GetContentRect;
  if PtInRect(R, Point(X, Y)) then
  begin
    StopScrollTimer;
    I := ItemAtPos(X, Y);
    if (I <> -1) and not (FItems[I].Header) and
       (FMouseDown or (FMouseMoveChangeIndex and not IsScrollBarCaptured))
       and (I <> FMouseActive) then
    begin
      SetItemActive(I);
      FMouseActive := I;
    end;
  end
  else
  if FMouseDown then
  begin
    if Y < R.Top then
      EnableScrollTimer(1)
    else
    if Y > R.Bottom then
      EnableScrollTimer(2)
    else
      StopScrollTimer;
  end;
end;

procedure TscGPCustomListBox.SetItemActive(Value: Integer);
var
  I: Integer;
begin
  FItemIndex := Value;
  for I := 0 to FItems.Count - 1 do
  with FItems[I] do
   if I = Value then Active := True else Active := False;
  RePaintControl;
  ScrollToItem(Value);
end;

procedure TscGPCustomListBox.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FVertScrollBar = nil then Exit;
  if not FVertScrollBar.Visible then Exit;
  if TWMMOUSEWHEEL(Message).WheelDelta < 0 then
    FVertScrollBar.SetPosition(FVertScrollBar.Position + FItemHeight, True)
  else
    FVertScrollBar.SetPosition(FVertScrollBar.Position - FItemHeight, True);
end;

procedure TscGPCustomListBox.WMSETFOCUS(var Message: TWMSETFOCUS);
begin
  inherited;
  FUpdateParentBuffer := True;
  if DrawTextMode = scdtmGDIPlus then
   Invalidate
  else
    RePaint;
end;

procedure TscGPCustomListBox.WMKILLFOCUS(var Message: TWMKILLFOCUS);
begin
  inherited;
  RePaint;
end;

procedure TscGPCustomListBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and not Focused and not FPopupMode then
      begin
        FClicksDisabled := True;
        WinApi.Windows.SetFocus(Handle);
        FClicksDisabled := False;
        if not Focused then Exit;
      end;
    CN_COMMAND:
      if FClicksDisabled then Exit;
  end;
  inherited WndProc(Message);
end;

procedure TscGPCustomListBox.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscGPCustomListBox.FindUp;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Exit;
  Start := FItemIndex - 1;
  if Start < 0 then Exit;
  for I := Start downto 0 do
  begin
    if (not FItems[I].Header) and FItems[I].Enabled and FItems[I].IsFilterVisible then
    begin
      ItemIndex := I;
      Exit;
    end;
  end;
end;

procedure TscGPCustomListBox.FindDown;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Start := 0 else Start := FItemIndex + 1;
  if Start > FItems.Count - 1 then Exit;
  for I := Start to FItems.Count - 1 do
  begin
    if (not FItems[I].Header) and FItems[I].Enabled and FItems[I].IsFilterVisible then
    begin
      ItemIndex := I;
      Exit;
    end;
  end;
end;

procedure TscGPCustomListBox.FindPageUp;
var
  I, J, Start: Integer;
  PageCount: Integer;
  FindHeader: Boolean;
begin
  if ItemIndex <= -1 then Exit;
  Start := FItemIndex - 1;
  if Start < 0 then Exit;
  PageCount := FItemsRect.Height div FItemHeight;
  if PageCount = 0 then PageCount := 1;
  PageCount := Start - PageCount;
  if PageCount < 0 then PageCount := 0;
  FindHeader := False;
  J := -1;
  for I := Start downto PageCount do
  begin
    if not FItems[I].Header and FindHeader and FItems[I].Enabled and FItems[I].IsFilterVisible then
    begin
      ItemIndex := I;
      Exit;
    end
    else
    if FItems[I].Header then
    begin
      FindHeader := True;
      Continue;
    end
    else
    if not FItems[I].Header and FItems[I].Enabled and FItems[I].IsFilterVisible then
    begin
      J := I;
    end;
  end;
  if J <> -1 then ItemIndex := J;
end;

procedure TscGPCustomListBox.FindPageDown;
var
  I, J, Start: Integer;
  PageCount: Integer;
  FindHeader: Boolean;
begin
  if ItemIndex <= -1 then Start := 0 else Start := FItemIndex + 1;
  if Start > FItems.Count - 1 then Exit;
  PageCount := FItemsRect.Height div FItemHeight;
  if PageCount = 0 then PageCount := 1;
  PageCount := Start + PageCount;
  if PageCount > FItems.Count - 1 then PageCount := FItems.Count - 1;
  FindHeader := False;
  J := -1;
  for I := Start to PageCount do
  begin
    if not FItems[I].Header and FindHeader and FItems[I].Enabled and FItems[I].IsFilterVisible then
    begin
      ItemIndex := I;
      Exit;
    end
    else
    if FItems[I].Header then
    begin
      FindHeader := True;
      Continue;
    end
    else
    if not FItems[I].Header and FItems[I].Enabled and FItems[I].IsFilterVisible then
    begin
      J := I;
    end;
  end;
  if J <> -1 then ItemIndex := J;
end;

procedure TscGPCustomListBox.KeyPress(var Key: Char);
var
  I: Integer;
  S: String;
begin
  inherited;
  if Ord(Key) >= 34 then
  if FAutoComplete then
  begin
    if FSearchTimerEnabled then
      KillTimer(Handle, 7);
    FSearchString := FSearchString + Key;
    I := IndexOfCaption(FSearchString, True);
    if I <> -1 then
      ItemIndex := I;
    SetTimer(Handle, 7, 1000, nil);
    FSearchTimerEnabled := True;
  end
  else
  begin
    S := Key;
    I := NextIndex(S);
    if I <> -1 then
      ItemIndex := I;
  end;
end;

procedure TscGPCustomListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FShowCheckBoxes and (Key = 32) and (ItemIndex <> -1) then
  begin
    Items[ItemIndex].Checked := not Items[ItemIndex].Checked;
    if Assigned(FOnItemCheckClick) then FOnItemCheckClick(Self);
  end;
end;

procedure TscGPCustomListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited KeyDown(Key, Shift);
 case Key of
   VK_NEXT:  FindPageDown;
   VK_PRIOR: FindPageUp;
   VK_UP, VK_LEFT: FindUp;
   VK_DOWN, VK_RIGHT: FindDown;
 end;
end;

constructor TscGPPopupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FPopupMode := True;
  DrawOnBackground := False;
  FTransparentBackground := False;
  Visible := False;
  TabStop := False;
end;

procedure TscGPPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscGPPopupListBox.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscGPPopupListBox.Hide;
begin
  FMouseActive := -1;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TscGPPopupListBox.Show(Origin: TPoint);
begin
  FMouseActive := -1;
  FScrollOffset := 0;
  ScrollToItem(ItemIndex);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

constructor TscGPComboOptions.Create;
begin
  inherited;

  FShapeFillStyle := scgpsfColor;
  FShapeFillGradientAngle := 90;
  FShapeFillGradientPressedAngle := -90;

  FNormalColor := clBtnFace;
  FHotColor := clBtnFace;
  FPressedColor := clBtnShadow;
  FFocusedColor := clBtnFace;
  FDisabledColor := clBtnFace;
  FFrameNormalColor := clBtnShadow;
  FFrameHotColor := clHighLight;
  FFramePressedColor := clHighLight;
  FFrameFocusedColor := clHighLight;
  FFrameDisabledColor := clBtnShadow;
  FFrameWidth := 1;
  FFontNormalColor := clBtnText;
  FFontHotColor := clBtnText;
  FFontPressedColor := clBtnText;
  FFontFocusedColor := clBtnText;
  FFontDisabledColor := clBtnShadow;
  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;

  FNormalColorAlpha := 255;
  FHotColorAlpha := 255;
  FPressedColorAlpha := 255;
  FFocusedColorAlpha := 255;
  FDisabledColorAlpha := 255;

  FFrameNormalColorAlpha := 255;
  FFrameHotColorAlpha := 255;
  FFramePressedColorAlpha := 255;
  FFrameFocusedColorAlpha := 255;
  FFrameDisabledColorAlpha := 255;

  FArrowSize := 9;
  FArrowAreaWidth := 0;

  FShapeCornerRadius := 10;
  FShapeStyle := scgpcssRect;
  FScaleFrameWidth := False;
end;

procedure TscGPComboOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPComboOptions then
  begin
    FNormalColor := TscGPComboOptions(Source).FNormalColor;
    FHotColor := TscGPComboOptions(Source).FHotColor;
    FPressedColor := TscGPComboOptions(Source).FPressedColor;
    FFocusedColor := TscGPComboOptions(Source).FFocusedColor;
    FDisabledColor := TscGPComboOptions(Source).FDisabledColor;
    FFrameNormalColor := TscGPComboOptions(Source).FFrameNormalColor;
    FFrameHotColor := TscGPComboOptions(Source).FFrameHotColor;
    FFramePressedColor := TscGPComboOptions(Source).FFramePressedColor;
    FFrameFocusedColor := TscGPComboOptions(Source).FFrameFocusedColor;
    FFrameDisabledColor := TscGPComboOptions(Source).FFrameDisabledColor;
    FFrameWidth := TscGPComboOptions(Source).FFrameWidth;
    FFontNormalColor := TscGPComboOptions(Source).FFontNormalColor;
    FFontHotColor := TscGPComboOptions(Source).FFontHotColor;
    FFontPressedColor := TscGPComboOptions(Source).FFontPressedColor;
    FFontFocusedColor := TscGPComboOptions(Source).FFontFocusedColor;
    FFontDisabledColor := TscGPComboOptions(Source).FFontDisabledColor;
    FNormalColorAlpha := TscGPComboOptions(Source).FNormalColorAlpha;
    FHotColorAlpha := TscGPComboOptions(Source).FHotColorAlpha;
    FPressedColorAlpha := TscGPComboOptions(Source).FPressedColorAlpha;
    FFocusedColorAlpha := TscGPComboOptions(Source).FFocusedColorAlpha;
    FDisabledColorAlpha := TscGPComboOptions(Source).FDisabledColorAlpha;
    FFrameNormalColorAlpha := TscGPComboOptions(Source).FFrameNormalColorAlpha;
    FFrameHotColorAlpha := TscGPComboOptions(Source).FFrameHotColorAlpha;
    FFramePressedColorAlpha := TscGPComboOptions(Source).FFramePressedColorAlpha;
    FFrameFocusedColorAlpha := TscGPComboOptions(Source).FFrameFocusedColorAlpha;
    FFrameDisabledColorAlpha := TscGPComboOptions(Source).FFrameDisabledColorAlpha;
    FShapeStyle := TscGPComboOptions(Source).ShapeStyle;
    FShapeCornerRadius := TscGPComboOptions(Source).FShapeCornerRadius;
    FArrowSize :=  TscGPComboOptions(Source).FArrowSize;
    FShapeFillStyle :=  TscGPComboOptions(Source).FShapeFillStyle;
    FStyleColors := TscGPComboOptions(Source).FStyleColors;
  end
  else
    inherited Assign(Source);
end;

 function TscGPComboOptions.GetColorAlpha: Byte;
 begin
   Result := FNormalColorAlpha;
   case FState of
     scsHot: Result := FHotColorAlpha;
     scsPressed: Result := FPressedColorAlpha;
     scsFocused: Result := FFocusedColorAlpha;
     scsDisabled: Result := FDisabledColorAlpha;
   end;
 end;

 function TscGPComboOptions.GetFrameColorAlpha: Byte;
 begin
   Result := FFrameNormalColorAlpha;
   case FState of
     scsHot: Result := FFrameHotColorAlpha;
     scsPressed: Result := FFramePressedColorAlpha;
     scsFocused: Result := FFrameFocusedColorAlpha;
     scsDisabled: Result := FFrameDisabledColorAlpha;
   end;
 end;

procedure TscGPComboOptions.SetShapeFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FShapeFillGradientAngle <> Value) then
  begin
    FShapeFillGradientAngle := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPComboOptions.SetShapeFillGradientPressedAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
    (FShapeFillGradientPressedAngle <> Value) then
  begin
    FShapeFillGradientPressedAngle := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPComboOptions.SetShapeFillStyle(Value: TscGPShapeFillStyle);
begin
  if FShapeFillStyle <> Value then
  begin
    FShapeFillStyle := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetArrowAreaWidth(Value: Integer);
begin
   if (FArrowAreaWidth <> Value) and (Value >= 0) then
  begin
    FArrowAreaWidth := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetArrowSize(Value: Integer);
begin
  if (FArrowSize <> Value) and (Value >= 7) then
  begin
    FArrowSize := Value;
    Changed;
  end;
end;

function TscGPComboOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := PressedColor;
    scsFocused: Result := FocusedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscGPComboOptions.GetFrameColor: TColor;
begin
  Result := FrameNormalColor;
  case FState of
    scsHot: Result := FrameHotColor;
    scsPressed: Result := FramePressedColor;
    scsFocused: Result := FrameFocusedColor;
    scsDisabled: Result := FrameDisabledColor;
  end;
end;

function TscGPComboOptions.GetFontColor: TColor;
begin
  Result := FontNormalColor;
  case FState of
    scsHot: Result := FontHotColor;
    scsPressed: Result := FontPressedColor;
    scsFocused: Result := FontFocusedColor;
    scsDisabled: Result := FontDisabledColor;
  end;
end;

function TscGPComboOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscGPComboOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscGPComboOptions.GetPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscGPComboOptions.GetFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscGPComboOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

function TscGPComboOptions.GetFrameNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameNormalColor)
  else
    Result := FFrameNormalColor;
end;

function TscGPComboOptions.GetFrameHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameHotColor)
  else
    Result := FFrameHotColor;
end;

function TscGPComboOptions.GetFramePressedColor: TColor;
begin
 if FStyleColors then
    Result := GetStyleColor(FFramePressedColor)
  else
    Result := FFramePressedColor;
end;

function TscGPComboOptions.GetFrameFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameFocusedColor)
  else
    Result := FFrameFocusedColor;
end;

function TscGPComboOptions.GetFrameDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameDisabledColor)
  else
    Result := FFrameDisabledColor;
end;

function TscGPComboOptions.GetFontNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontNormalColor)
  else
    Result := FFontNormalColor;
end;

function TscGPComboOptions.GetFontHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontHotColor)
  else
    Result := FFontHotColor;
end;

function TscGPComboOptions.GetFontPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontPressedColor)
  else
    Result := FFontPressedColor;
end;

function TscGPComboOptions.GetFontFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontFocusedColor)
  else
    Result := FFontFocusedColor;
end;

function TscGPComboOptions.GetFontDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontDisabledColor)
  else
    Result := FFontDisabledColor;
end;

procedure TscGPComboOptions.SetShapeStyle(Value: TscGPComboShapeStyle);
begin
  if FShapeStyle <> Value then
  begin
    FShapeStyle := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetShapeCornerRadius(Value: Integer);
begin
  if (FShapeCornerRadius <> Value) and (Value > 0) then
  begin
    FShapeCornerRadius := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetHotColorAlpha(Value: Byte);
begin
  if FHotColorAlpha <> Value then
  begin
    FHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetPressedColorAlpha(Value: Byte);
begin
  if FPressedColorAlpha <> Value then
  begin
    FPressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFocusedColorAlpha(Value: Byte);
begin
  if FFocusedColorAlpha <> Value then
  begin
    FFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameNormalColor(Value: TColor);
begin
  if FFrameNormalColor <> Value then
  begin
    FFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameHotColor(Value: TColor);
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFramePressedColor(Value: TColor);
begin
  if FFramePressedColor <> Value then
  begin
    FFramePressedColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameFocusedColor(Value: TColor);
begin
  if FFrameFocusedColor <> Value then
  begin
    FFrameFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameDisabledColor(Value: TColor);
begin
  if FFrameDisabledColor <> Value then
  begin
    FFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameNormalColorAlpha(Value: Byte);
begin
   if FFrameNormalColorAlpha <> Value then
  begin
    FFrameNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameHotColorAlpha(Value: Byte);
begin
  if FFrameHotColorAlpha <> Value then
  begin
    FFrameHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFramePressedColorAlpha(Value: Byte);
begin
  if FFramePressedColorAlpha <> Value then
  begin
    FFramePressedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameFocusedColorAlpha(Value: Byte);
begin
  if FFrameFocusedColorAlpha <> Value then
  begin
    FFrameFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameDisabledColorAlpha(Value: Byte);
begin
  if FFrameDisabledColorAlpha <> Value then
  begin
    FFrameDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFontNormalColor(Value: TColor);
begin
  if FFontNormalColor <> Value then
  begin
    FFontNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFontHotColor(Value: TColor);
begin
  if FFontHotColor <> Value then
  begin
    FFontHotColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFontPressedColor(Value: TColor);
begin
  if FFontPressedColor <> Value then
  begin
    FFontPressedColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFontFocusedColor(Value: TColor);
begin
  if FFontFocusedColor <> Value then
  begin
    FFontFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFontDisabledColor(Value: TColor);
begin
  if FFontDisabledColor <> Value then
  begin
    FFontDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value > 0) then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGPComboOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPCustomCombo.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TscGPComboOptions.Create;
  FOptions.OnChange := OnOptionsChange;
  FDropDownPosition := scdpRight;
  FContentMargin := 0;
end;

destructor TscGPCustomCombo.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure TscGPCustomCombo.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  if FOptions.FScaleFrameWidth and (FOptions.FFrameWidth > 0) then
    FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);
  FOptions.FShapeCornerRadius := MulDiv(FOptions.FShapeCornerRadius, M, D);
  FOptions.FArrowSize := MulDiv(FOptions.FArrowSize, M, D);
  if FOptions.FArrowAreaWidth > 0 then
    FOptions.FArrowAreaWidth := MulDiv(FOptions.FArrowAreaWidth, M, D);
  FContentMargin := MulDiv(FContentMargin, M, D);
end;

procedure TscGPCustomCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TscGPCustomCombo.SetContentMargin(Value: Integer);
begin
  if (FContentMargin <> Value) and (Value >= 0) then
  begin
    FContentMargin := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomCombo.OnOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPCustomCombo.DoMouseEnter;
begin
  if FDropDown then Exit;
  inherited;
end;

procedure TscGPCustomCombo.DoMouseLeave;
begin
  if FDropDown then Exit;
  inherited;
end;

procedure TscGPCustomCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

function TscGPCustomCombo.CanDrawItem: Boolean;
begin
  Result := True;
end;

procedure TscGPCustomCombo.DrawComboItem(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect);
begin

end;

procedure TscGPCustomCombo.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, TR: TRect;
  ArrowColor: Cardinal;
  G: TGPGraphics;
  B: TGPBrush;
  LB: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath, ArrowPath: TGPGraphicsPath;
  FillR, FrameR, R1: TGPRectF;
  FrameColor, FillColor: Cardinal;
  l, t, w, h, d: Single;
  ArrowR: TRect;
  C1, C2: Cardinal;
begin
  if FDropDown then
    ACtrlState := scsPressed
  else
  if Focused then
    ACtrlState := scsFocused;
  R := Rect(0, 0, Width, Height);
  TR := R;
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;
  FOptions.State := ACtrlState;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  P := TGPPen.Create(0, FOptions.FrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  ArrowPath := nil;
  // colors
  FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
  FillColor := ColorToGPColor(Options.Color, Options.ColorAlpha);
  ArrowColor := ColorToGPColor(Options.FontColor, 200);
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
  if FOptions.ShapeFillStyle = scgpsfColor then
    B := TGPSolidBrush.Create(FillColor)
  else
  begin
    C1 := ColorToGPColor(LighterColor(FOptions.Color, 25), Options.ColorAlpha);
    C2 := ColorToGPColor(DarkerColor(FOptions.Color, 25), Options.ColorAlpha);
    R1 := FillR;
    InflateGPRect(R1, 1, 1);
    if ACtrlState = scsPressed then
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientPressedAngle)
    else
      B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientAngle);
  end;
  // draw
  try
    case FOptions.ShapeStyle of
      scgpcssLine:
        begin
          LB := TGPSolidBrush.Create(FrameColor);
          FrameR.Height := FOptions.FFrameWidth;
          FrameR.Y := Height - FOptions.FFrameWidth;
          G.FillRectangle(LB, FrameR);
          LB.Free;
        end;
      scgpcssRect:
        begin
          G.FillRectangle(B, FillR);
          G.DrawRectangle(P, FrameR);
        end;
      scgpcssRoundedRect, scgpcssRoundedLeftRight:
        begin
          // fill
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if Options.ShapeStyle = scgpcssRoundedLeftRight
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
          // frame
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          if Options.ShapeStyle = scgpcssRoundedLeftRight
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
    end;
    // draw arrow
    if BidiMode <> bdRightToLeft then
    begin
      if FOptions.FArrowAreaWidth > 0 then
        Dec(R.Right, FOptions.FArrowAreaWidth)
      else  
      if FContentMargin > 0 then
        Dec(R.Right, FContentMargin + Options.FArrowSize)
      else
      if FOptions.ShapeStyle = scgpcssRoundedLeftRight then
        Dec(R.Right, Options.FArrowSize + Height div 4)
      else
        Dec(R.Right, Options.FArrowSize + Max(Height div 10, FOptions.FFrameWidth * 4));

      if FOptions.FArrowAreaWidth > 0 then
        ArrowR := Rect(R.Right + FOptions.FArrowAreaWidth div 2 -  FOptions.FArrowSize div 2,
          Height div 2 - FOptions.FArrowSize div 2 - 2,
          R.Right + FOptions.FArrowAreaWidth div 2 -  FOptions.FArrowSize div 2 + FOptions.FArrowSize,
          Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 2)
      else
        ArrowR := Rect(R.Right, Height div 2 - FOptions.FArrowSize div 2 - 2,
          R.Right + Options.FArrowSize,
          Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 2);

    end
    else
    begin
      if FOptions.FArrowAreaWidth > 0 then
        Inc(R.Left, FOptions.FArrowAreaWidth)
      else  
      if FContentMargin > 0 then
        Inc(R.Left, FContentMargin + Options.FArrowSize)
      else
      if FOptions.ShapeStyle = scgpcssRoundedLeftRight then
        Inc(R.Left, Options.FArrowSize + Height div 4)
      else
        Inc(R.Left, Options.FArrowSize + Max(Height div 10, FOptions.FFrameWidth * 4));
      if FOptions.FArrowAreaWidth > 0 then
        ArrowR := Rect(R.Left - Options.FArrowSize div 2 - FOptions.FArrowAreaWidth div 2,
          Height div 2 - FOptions.FArrowSize div 2 - 1,
          R.Left - Options.FArrowSize div 2 - FOptions.FArrowAreaWidth div 2 + Options.FArrowSize,
          Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1)
      else
        ArrowR := Rect(R.Left - Options.FArrowSize, Height div 2 - FOptions.FArrowSize div 2 - 1,
          R.Left,
          Height div 2 - FOptions.FArrowSize div 2 + FOptions.FArrowSize - 1);
    end;
    // draw drop down arrow
    OffsetRect(ArrowR, 0, -1);
    GPDrawDropDownButtonGlyph(G, RectToGPRect(ArrowR), ArrowColor, FScaleFactor);
    // draw combo item
    ACanvas.Font := Self.Font;
    ACanvas.Font.Color := FOptions.FontColor;
    if BidiMode <> bdRightToLeft then
    begin
      if FContentMargin > 0 then
        Inc(R.Left, FContentMargin)
      else
      if FOptions.ShapeStyle = scgpcssRoundedLeftRight then
        Inc(R.Left, Height div 4)
      else
        Inc(R.Left, FOptions.FFrameWidth * 2 + Round(2 * FScaleFactor));
    end
    else
    begin
      if FContentMargin > 0 then
        Dec(R.Right, FContentMargin)
      else
      if FOptions.ShapeStyle = scgpcssRoundedLeftRight then
        Dec(R.Right, Height div 4)
      else
        Dec(R.Right, FOptions.FFrameWidth * 2 + Round(2 * FScaleFactor));
    end;
    DrawComboItem(ACanvas, G, R);
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
    if ArrowPath <> nil then
      ArrowPath.Free;
  end;
end;


constructor TscGPCustomComboBox.Create;
begin
  inherited Create(AOwner);
  FListBox := TscGPPopupListBox.Create(Self);
  FMouseWheelSupport := True;
  FCheckedListMode := False;
  FCheckedListWrap := True;
  FAutoComplete := False;
  FSearchString := '';
  FSearchTimerEnabled := False;
  FShowItemImage := True;
  FShowItemText := True;
  FShowItemDetail := False;
  FDropDown := False;
  TabStop := True;
  FLBDown := False;
  WasInLB := False;
  FTimerMode := 0;
  Width := 120;
  Height := 41;
  FListBoxWindowProc := FlistBox.WindowProc;
  FlistBox.WindowProc := ListBoxWindowProcHook;
  FListBox.Visible := False;
  FlistBox.MouseMoveChangeIndex := True;
  if not (csDesigning in ComponentState) then
    FlistBox.Parent := Self;
  FLBDown := False;
  FLastTime := 0;
  FListBoxWidth := 0;
  FListBoxHeight := 0;
  FDropDownCount := 7;
end;

procedure TscGPCustomComboBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FListBoxWidth := MulDiv(FListBoxWidth, M, D);
  FListBoxHeight := MulDiv(FListBoxHeight, M, D);
end;

function TscGPCustomComboBox.CanAnimate: Boolean;
begin
  Result := inherited CanAnimate;
  if Result then
     Result := not FListBox.Visible;
end;

procedure TscGPCustomComboBox.Sort;
begin
  FListBox.Sort;
end;

procedure TscGPCustomComboBox.Add(const Item: String);
begin
  FListBox.Add(Item);
end;

procedure TscGPCustomComboBox.Add(Items: TStrings);
begin
  FListBox.Add(Items);
end;

procedure TscGPCustomComboBox.Delete(Index: Integer);
begin
  FListBox.Delete(Index);
end;

procedure TscGPCustomComboBox.Clear;
begin
  FListBox.Clear;
  RePaintControl;
end;

function TscGPCustomComboBox.NextIndex(const S: string): Integer;
begin
  Result := FListBox.NextIndex(S);
end;

function TscGPCustomComboBox.IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FListBox.IndexOfCaption(S, AStartOff);
end;

function TscGPCustomComboBox.IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FListBox.IndexOfDetail(S, AStartOff);
end;

procedure TscGPCustomComboBox.BeginUpdateItems;
begin
  FListBox.BeginUpdateItems;
end;

procedure TscGPCustomComboBox.EndUpdateItems;
begin
  FListBox.EndUpdateItems;
end;

function TscGPCustomComboBox.CanDrawItem: Boolean;
var
  Index: Integer;
begin
  if FDropDown then Index := FOldItemIndex else Index := ItemIndex;
  if FCheckedListMode then
    Result := True
  else
    Result := (Index >= 0) and (Index < Items.Count);
end;

procedure TscGPCustomComboBox.DrawComboItem(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect);
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  R, R1, CR: TRect;
  S: String;
  Index: Integer;
  X, Y, I: Integer;
  C: TColor;
  Flags: Cardinal;
begin
  if not Assigned(Items) then Exit;

  if FDropDown then Index := FOldItemIndex else Index := ItemIndex;

  if FCheckedListMode then
  begin
    if Items.Count > 0 then
    begin
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := GetStyleColor(FOptions.FontColor);
      ACanvas.Brush.Style := bsClear;
      R := ARect;
      S := '';
      for I := 0 to Items.Count - 1 do
      begin
        if Items[I].Checked and (Items[I].Caption <> '')  then
         if S <> '' then
           S := S + '; ' + Items[I].Caption
         else
           S := Items[I].Caption;
      end;
      if S <> '' then
      begin
        R1 := Rect(0, 0, R.Width, R.Height);
        if FDrawTextMode = scdtmGDI then
          DrawText(ACanvas.Handle, PChar(S), Length(S), R1,
            DT_LEFT or DT_CALCRECT or DT_NOPREFIX or WordWraps[FCheckedListWrap])
        else
          GPDrawText(G, nil, ACanvas, R1, S,
            DT_LEFT or DT_CALCRECT or DT_NOPREFIX or WordWraps[FCheckedListWrap]);

        X := R.Left;
        Y := R.Top + R.Height div 2 - R1.Height div 2;
        if Y < R.Top then Y := R.Top;
        R := Rect(X, Y, R.Right - 2, Y + R1.Height);
        if FDrawTextMode = scdtmGDI then
          DrawText(ACanvas.Handle, PChar(S), Length(S), R,
            scDrawTextBidiModeFlags(WordWraps[FCheckedListWrap] or DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft))
        else
          GPDrawText(G, nil, ACanvas, R, S,
            scDrawTextBidiModeFlags(WordWraps[FCheckedListWrap] or DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft));
      end;
    end;
    Exit;
  end;

  R := ARect;
  if (Index < 0) or (Index > Items.Count - 1) then Exit;
  // draw image
  if (Images <> nil) and FShowItemImage then
  begin
    R1 := R;
    if BidiMode <> bdRightToLeft then
    begin
      R1.Right := R.Left + Images.Width;
      Inc(R.Left, ListboxItemSpacing + Images.Width);
    end
    else
    begin
      R1.Left := R1.Right - Images.Width;
      Dec(R.Right, ListBoxItemSpacing  + Images.Width);
    end;
    if (Items[Index].ImageIndex >= 0) and
       (Items[Index].ImageIndex < Images.Count) then
    begin
      X := R1.Left;
      Y := R1.Top + R1.Height div 2 - Images.Height div 2;
      if Items[Index].Enabled then
         Images.Draw(ACanvas, X, Y, Items[Index].ImageIndex, True)
      else
      if Images is TscCustomImageList then
        Images.Draw(ACanvas, X, Y, Items[Index].ImageIndex, False)
      else
        DrawBitmapFromImageList(ACanvas, X, Y, Images,
          Items[Index].ImageIndex, DisabledImageAlphaValue);
    end;
  end;
   // draw detail
  if FShowItemDetail then
  begin
    ACanvas.Font := Self.DetailFont;
    ACanvas.Brush.Style := bsClear;

    if (Items[Index].CustomDetailTextColor <> clNone) and Items[Index].Enabled then
      C := GetStyleColor(Items[Index].FCustomDetailTextColor)
    else
    begin
      C := GetStyleColor(FOptions.FontColor);
      if FDrawTextMode = scdtmGDI then
        if IsLightColor(C) then
          C := DarkerColor(C, 40)
        else
          C := LighterColor(C, 35);
    end;
    ACanvas.Font.Color := C;

    R1 := R;

    S := Items[Index].Detail;
    if DetailPosition = scgplbdBottom then
    begin
      R1.Top := R1.Bottom - ACanvas.TextHeight('Wq') - Round(4 * FScaleFactor) - FOptions.FFrameWidth;
      Inc(R1.Left);
      if S <> '' then
       if FDrawTextMode = scdtmGDI then
         DrawText(ACanvas.Handle, PChar(S), Length(S), R1,
           scDrawTextBidiModeFlags(DT_LEFT, BidiMode = bdRightToLeft))
       else
        if Enabled then
          GPDrawText(G, nil, ACanvas, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft), 160)
        else
          GPDrawText(G, nil, ACanvas, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft), 125);
      R.Bottom := R1.Top;
    end
    else
    if S <> '' then
    begin
      if not DetailWordWrap then
      begin
        if BidiMode <> bdRightToLeft then
          R1.Left := R1.Right - ACanvas.TextWidth(S) - Round(4 * FScaleFactor) - FOptions.FFrameWidth
        else
          R1.Right := R1.Left + ACanvas.TextWidth(S) + Round(4 * FScaleFactor) + FOptions.FFrameWidth;
        if FDrawTextMode = scdtmGDI then
          scDrawText(ACanvas, S, R1, BidiMode = bdRightToLeft, True)
        else
        if Enabled then
          GPDrawText(G, nil, ACanvas, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_NOPREFIX, BidiMode = bdRightToLeft), 160)
        else
          GPDrawText(G, nil, ACanvas, R1, S, scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_NOPREFIX, BidiMode = bdRightToLeft), 125);
      end
      else
      begin
        CR := Rect(0, 0, R1.Width div 3, R1.Height);
        if FDrawTextMode = scdtmGDI then
          DrawText(ACanvas.Handle, PChar(S), Length(S), CR,
              DT_LEFT or DT_NOPREFIX or DT_WORDBREAK or DT_CALCRECT)
        else
          GPDrawText(G, nil, ACanvas, CR, S,
               DT_LEFT or DT_NOPREFIX or DT_WORDBREAK or DT_CALCRECT);

        if BidiMode <> bdRightToLeft then
        begin
          R1.Right :=  R1.Right - Round(5 * FScaleFactor);
          R1.Left := R1.Right - CR.Width;
        end
        else
        begin
          R1.Left := R1.Left + Round(5 * FScaleFactor);
          R1.Right := R1.Left + CR.Width;
        end;

        R1.Top := R1.Top + R1.Height div 2 - CR.Height div 2;

        if FDrawTextMode = scdtmGDI then
          DrawText(ACanvas.Handle, PChar(S), Length(S), R1,
            scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK, BidiMode = bdRightToLeft))
        else
        if Enabled then
          GPDrawText(G, nil, ACanvas, R1, S, scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK or DT_NOPREFIX,
            BidiMode = bdRightToLeft), 160)
        else
          GPDrawText(G, nil, ACanvas, R1, S, scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK or DT_NOPREFIX,
            BidiMode = bdRightToLeft), 125);

      end;
      if BidiMode <> bdRightToLeft then
        R.Right := R1.Left
      else
        R.Left := R1.Right;
    end;

  end;
  // draw text
  ACanvas.Font := Self.Font;

  if (Items[Index].CustomTextColor <> clNone) and Items[Index].Enabled then
    ACanvas.Font.Color := GetStyleColor(Items[Index].FCustomTextColor)
  else
    ACanvas.Font.Color := GetStyleColor(FOptions.FontColor);

  Flags := DT_LEFT or DT_NOPREFIX;
  if ItemWordWrap then
   Flags := Flags or DT_WORDBREAK;
  if ItemShowEllipsis then
   Flags := Flags or DT_END_ELLIPSIS;

  S := Items[Index].Caption;

  ACanvas.Brush.Style := bsClear;
  R1 := Rect(0, 0, R.Width, R.Height);

  if FDrawTextMode = scdtmGDI then
    DrawText(ACanvas.Handle, PChar(S), Length(S), R1,
              Flags or DT_CALCRECT)
  else
    GPDrawText(G, nil, ACanvas, R1, S, Flags or DT_CALCRECT);

  if R1.Height <= R.Height then
  begin
    X := R.Left;
    Y := R.Top + R.Height div 2 - R1.Height div 2;
    if Y < R.Top then Y := R.Top;
    R := Rect(X, Y, R.Right - 2, Y + R1.Height);
  end
  else
  begin
    if ItemWordWrap and ItemShowEllipsis then
      Flags := Flags or DT_EDITCONTROL;
  end;

  if FDrawTextMode = scdtmGDI then
    DrawText(ACanvas.Handle, PChar(S), Length(S), R,
      scDrawTextBidiModeFlags(Flags, BidiMode = bdRightToLeft))
  else
  if Enabled then
    GPDrawText(G, nil, ACanvas, R, S,
      scDrawTextBidiModeFlags(Flags or DT_VCENTER, BidiMode = bdRightToLeft))
  else
    GPDrawText(G, nil, ACanvas, R, S,
      scDrawTextBidiModeFlags(Flags or DT_VCENTER, BidiMode = bdRightToLeft), 125);
end;

procedure TscGPCustomComboBox.ListBoxWindowProcHook(var Message: TMessage);
var
  FOld: Boolean;
begin
  FOld := True;
  case Message.Msg of
     WM_LBUTTONDOWN:
       begin
         FOLd := False;
         FLBDown := True;
         WasInLB := True;
         if FListBox.Visible then
            FListBox.MouseDown(mbLeft, [],
              TWMLButtonDown(Message).XPos, TWMLButtonDown(Message).YPos);
         SetCapture(Self.Handle);
       end;
     WM_LBUTTONUP, WM_LBUTTONDBLCLK,
     WM_RBUTTONDOWN, WM_RBUTTONUP,
     WM_MBUTTONDOWN, WM_MBUTTONUP:
       begin
         FOLd := False;
       end;
     WM_MOUSEACTIVATE:
      begin
        Message.Result := MA_NOACTIVATE;
      end;
  end;
  if FOld then FListBoxWindowProc(Message);
end;

procedure TscGPCustomComboBox.KeyPress(var Key: Char);
var
  I: Integer;
  S: String;
begin
  inherited;
  if Ord(Key) >= 34 then
  if FAutoComplete then
  begin
    if FSearchTimerEnabled then
      KillTimer(Handle, 7);
    FSearchString := FSearchString + Key;
    I := IndexOfCaption(FSearchString, True);
    if I <> -1 then
      ItemIndex := I;
    SetTimer(Handle, 7, 1000, nil);
    FSearchTimerEnabled := True;
  end
  else
  begin
    S := Key;
    I := NextIndex(S);
    if I <> -1 then
      ItemIndex := I;
  end;
end;

destructor TscGPCustomComboBox.Destroy;
begin
  FListBox.Free;
  FListBox := nil;
  inherited;
end;

procedure TscGPCustomComboBox.CMEnabledChanged;
begin
  inherited;
  RePaintControl;
end;

function TscGPCustomComboBox.GetItemWordWrap: Boolean;
begin
  Result := FListBox.ItemWordWrap;
end;

procedure TscGPCustomComboBox.SetItemWordWrap(Value: Boolean);
begin
  FListBox.ItemWordWrap := Value;
end;

function TscGPCustomComboBox.GetItemShowEllipsis: Boolean;
begin
  Result := FListBox.ItemShowEllipsis;
end;

procedure TscGPCustomComboBox.SetItemShowEllipsis(Value: Boolean);
begin
  FListBox.ItemShowEllipsis := Value;
end;

procedure TscGPCustomComboBox.SetCheckedListWrap(Value: Boolean);
begin
  if FCheckedListWrap <> Value then
  begin
    FCheckedListWrap := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomComboBox.SetCheckedListMode(Value: Boolean);
begin
  if FCheckedListMode <> Value then
  begin
    FCheckedListMode := Value;
    FListBox.ShowCheckBoxes := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

function TscGPCustomComboBox.GetImages: TCustomImageList;
begin
  if FListBox <> nil
  then
    Result := FListBox.Images
   else
    Result := nil;
end;

procedure TscGPCustomComboBox.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (Message.wParam <> FListBox.Handle)
  then
    CloseUp(False);
end;

procedure TscGPCustomComboBox.CMCancelMode;
begin
  inherited;
  if (Message.Sender = nil) or
     (Message.Sender <> Self)
  then
    CloseUp(False);
end;

procedure TscGPCustomComboBox.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGPCustomComboBox.CheckButtonClick;
begin
  CloseUp(True);
end;

procedure TscGPCustomComboBox.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  case Msg.CharCode of
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:  Msg.Result := 1;
  end;
end;

procedure TscGPCustomComboBox.KeyDown;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP, VK_LEFT:
      if ssAlt in Shift
      then
        begin
          if FListBox.Visible then CloseUp(False);
        end
      else
        ComboKeyUp(True);
    VK_DOWN, VK_RIGHT:
      if ssAlt in Shift
      then
        begin
          if not FListBox.Visible then DropDown;
        end
      else
        ComboKeyDown(True);
    VK_SPACE:
      if FCheckedListMode and FListBox.Visible and
        (FListBox.ItemIndex >=0) and (FListBox.ItemIndex < FListBox.Items.Count)
      then
      begin
        FListBox.Items[FListBox.ItemIndex].Checked := not FListBox.Items[FListBox.ItemIndex].Checked;
        RePaintControl;
        if Assigned(FOnItemCheckClick) then
           FOnItemCheckClick(Self);
      end;
    VK_NEXT: ComboPageDown(True);
    VK_PRIOR: ComboPageUp(True);
    VK_ESCAPE: if FListBox.Visible then CloseUp(False);
    VK_RETURN: if FListBox.Visible then CloseUp(True);
  end;
end;

procedure TscGPCustomComboBox.WMMOUSEWHEEL;
begin
  inherited;
  if not FMouseWheelSupport then Exit;
  
  if not FCheckedListMode then
    if TWMMOUSEWHEEL(Message).WheelDelta > 0
    then
      ComboKeyUp(not FListBox.Visible)
    else
      ComboKeyDown(not FListBox.Visible);
end;

procedure TscGPCustomComboBox.WMSETFOCUS;
begin
  inherited;
  FUpdateParentBuffer := True;
  if DrawTextMode = scdtmGDIPlus then
    Invalidate
  else
    RePaint;
end;

procedure TscGPCustomComboBox.WMKILLFOCUS;
begin
  inherited;
  if FListBox.Visible then CloseUp(False);
  RePaint;
end;

function TscGPCustomComboBox.GetItemIndex;
begin
  Result := FListBox.ItemIndex;
end;

procedure TscGPCustomComboBox.InitItemIndex(Value: Integer);
begin
  FListBox.InitItemIndex(Value);
  FOldItemIndex := FListBox.ItemIndex;
end;

procedure TscGPCustomComboBox.SetItemIndex;
begin
  FListBox.ItemIndex := Value;
  FOldItemIndex := FListBox.ItemIndex;
  RePaintControl;
  if not (csDesigning in ComponentState) and
     not (csLoading in ComponentState)
  then
    begin
      if Assigned(FOnClick) then FOnClick(Self);
      Change;
    end;
end;

function TscGPCustomComboBox.IsPopupVisible: Boolean;
begin
  Result := FListBox.Visible;
end;

function TscGPCustomComboBox.CanCancelDropDown;
begin
  Result := FListBox.Visible and not FMouseIn;
end;

procedure TscGPCustomComboBox.EnableScrollTimer(Value: Integer);
begin
  if FTimerMode = 0 then
  begin
    FTimerMode := Value;
    KillTimer(Handle, 2);
    SetTimer(Handle, 2, 50, nil);
  end
  else
    FTimerMode := Value;
end;

procedure TscGPCustomComboBox.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscGPCustomComboBox.WMTimer;
begin
  inherited;
  if Message.TimerID = 7 then
  begin
    FSearchString := '';
    KillTimer(Handle, 7);
    FSearchTimerEnabled := False;
  end
  else
  if Message.TimerID = 2 then
  begin
    case FTimerMode of
      1: FListBox.FindUp;
      2: FListBox.FindDown;
    end;
  end;
end;

procedure TscGPCustomComboBox.ProcessListBox;
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := FListBox.ScreenToClient(P);
  if (P.Y < 0) and (FListBox.FVertScrollBar <> nil) and FListBox.FVertScrollBar.Visible and WasInLB
     and not ListBox.IsScrollBarCaptured and not ListBox.MouseInScrollBar(P.X, P.Y) and FLBDown
  then
    EnableScrollTimer(1)
  else
  if (P.Y > FListBox.Height) and (FListBox.FVertScrollBar <> nil) and FListBox.FVertScrollBar.Visible and WasInLB
     and not ListBox.IsScrollBarCaptured and not ListBox.MouseInScrollBar(P.X, P.Y) and FLBDown
  then
    EnableScrollTimer(2)
  else
    if (P.Y >= 0) and (P.Y <= FListBox.Height)
    then
      begin
        StopScrollTimer;
        FListBox.MouseMove([], P.X, P.Y);
        WasInLB := True;
        if not FLBDown then
        begin
          FLBDown := True;
          WasInLB := False;
        end;
     end;
end;

procedure TscGPCustomComboBox.SetDropDownCount(Value: Integer);
begin
  if Value >= 0
  then
    FDropDownCount := Value;
end;

procedure TscGPCustomComboBox.SetItems;
begin
  FListBox.Items.Assign(Value);
end;

function TscGPCustomComboBox.GetItems;
begin
  Result := FListBox.Items;
end;

procedure TscGPCustomComboBox.MouseDown;
begin
  inherited;
  if not Focused then SetFocus;
  if Button <> mbLeft then Exit;
  RePaintControl;
  if FListBox.Visible then CloseUp(False)
  else
    begin
      WasInLB := False;
      FLBDown := True;
      DropDown;
    end;
end;

procedure TscGPCustomComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
  P: TPoint;
begin
  if FListBox.Visible then
  begin
    GetCursorPos(P);
    P := FListBox.ScreenToClient(P);
  end
  else
    P := Point(X, Y);
  if FListBox.Visible and
     (FListBox.IsScrollBarCaptured or FListBox.MouseInScrollBar(P.X, P.Y)) then
  begin
    WasInLB := False;
    ReleaseCapture;
    FLBDown := False;
    FListBox.MouseUp(Button, [], P.X, P.Y);
    FListBox.FMouseDown := False;
  end
  else
  if FLBDown and WasInLB
  then
    begin
      ReleaseCapture;
      FListBox.FMouseDown := False;
      FLBDown := False;
      if FCheckedListMode and (FListBox.ItemIndex >=0) and (FListBox.ItemIndex < FListBox.Items.Count) then
      begin
        FListBox.Items[FListBox.ItemIndex].Checked := not FListBox.Items[FListBox.ItemIndex].Checked;
        RePaintControl;
        if Assigned(FOnItemCheckClick) then
           FOnItemCheckClick(Self);
      end
      else
      begin
        GetCursorPos(P);
        if WindowFromPoint(P) = FListBox.Handle
        then
          CloseUp(True)
        else
          CloseUp(False);
      end;
    end
  else
     FLBDown := False;
  inherited;
end;

procedure TscGPCustomComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FListBox.Visible then ProcessListBox;
end;

procedure TscGPCustomComboBox.CloseUp;
begin
  SC_UnHookMouseMessages;
  ReleaseCapture;
  if FTimerMode <> 0 then StopScrollTimer;
  if not FListBox.Visible then Exit;
  FListBox.StopScrollTimer;
  FListBox.Hide;
  FDropDown := False;
  if FCheckedListMode or ((FListBox.ItemIndex >= 0) and
     (FListBox.ItemIndex < FListBox.Items.Count) and Value) then
  begin
    RePaintControl;
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
    if Assigned(FOnClick) then FOnClick(Self);
    Change;
  end
  else
  begin
    FListBox.ItemIndex := FOldItemIndex;
    RePaintControl;
  end;
end;

procedure TscGPCustomComboBox.DropDown;
var
  P: TPoint;
  WorkArea: TRect;
begin
  if Animation then StopAnimation;
  if Items.Count = 0 then Exit;
  WasInLB := False;
  if FTimerMode <> 0 then StopScrollTimer;
  if Assigned(FOnDropDown) then FOnDropDown(Self);

  if FListBoxWidth = 0
  then
    FListBox.Width := Width
  else
    FListBox.Width := FListBoxWidth;

  FListBox.Font.Assign(Font);
  FListBox.DrawTextMode := FDrawTextMode;
  FListBox.Color := Color;
  FListBox.BidiMode := Self.BiDiMode;
  FListBox.StyleElements := StyleElements;

  FOldItemIndex := FListBox.ItemIndex;
  FDropDown := True;
  RePaintControl;

  if FListBoxHeight > 0 then
    FListBox.Height := FListBoxHeight
  else
  if FDropDownCount > 0 then
    FListBox.Height := FListBox.CalcHeight(FDropDownCount);

  if FDropDownPosition = scdpRight then
    P := Point(Left + Width - FListBox.Width, Top + Height)
  else
    P := Point(Left, Top + Height);

  P := Parent.ClientToScreen(P);
  WorkArea := Screen.MonitorFromWindow(Handle).WorkAreaRect;
  if P.Y + FListBox.Height > WorkArea.Bottom then
    P.Y := P.Y - Height - FListBox.Height;
  FListBox.Show(P);
  SC_HookMouseMessages(Self);
end;

procedure TscGPCustomComboBox.ComboPageUp;
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindPageUp;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscGPCustomComboBox.ComboPageDown(AChange: Boolean);
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindPageDown;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscGPCustomComboBox.ComboKeyUp;
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindUp;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscGPCustomComboBox.ComboKeyDown;
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindDown;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscGPCustomComboBox.SetImages(Value: TCustomImageList);
begin
  if FListBox.Images <> Value then
  begin
    FListBox.Images := Value;
    RePaintControl;
  end;
end;

function TscGPCustomComboBox.GetDetailPosition: TscGPListBoxDetailPosition;
begin
  Result := FListBox.DetailPosition;
end;

procedure TscGPCustomComboBox.SetDetailPosition(Value: TscGPListBoxDetailPosition);
begin
  if FListBox.DetailPosition <> Value then
  begin
    FListBox.DetailPosition := Value;
    RePaintControl;
  end;
end;

function TscGPCustomComboBox.GetDetailWordWrap: Boolean;
begin
  Result := FListBox.DetailWordWrap;
end;

procedure TscGPCustomComboBox.SetDetailWordWrap(Value: Boolean);
begin
  if FListBox.DetailWordWrap <> Value then
  begin
    FListBox.DetailWordWrap := Value;
    RePaintControl;
  end;
end;

function TscGPCustomComboBox.GetListBoxHeaderFont: TFont;
begin
  Result := FListBox.HeaderFont;
end;

procedure TscGPCustomComboBox.SetListBoxHeaderFont(Value: TFont);
begin
  FListBox.HeaderFont.Assign(Value);
end;

function TscGPCustomComboBox.GetDetailFont: TFont;
begin
  Result := FListBox.DetailFont;
end;

procedure TscGPCustomComboBox.SetDetailFont(Value: TFont);
begin
  FListBox.DetailFont.Assign(Value);
end;

function TscGPCustomComboBox.GetListBoxItemSpacing: Integer;
begin
  Result := FListBox.ItemSpacing;
end;

function TscGPCustomComboBox.GetListBoxItemMargin: Integer;
begin
  Result := FListBox.ItemMargin;
end;

function TscGPCustomComboBox.GetListBoxScrollBarOptions: TscGPControlScrollBarOptions;
begin
  Result := FListBox.ScrollBarOptions;
end;

procedure TscGPCustomComboBox.SetListBoxScrollBarOptions(Value: TscGPControlScrollBarOptions);
begin
  FListBox.ScrollBarOptions.Assign(Value);
end;

function TscGPCustomComboBox.GetListBoxOptions: TscGPScrollingControlOptions;
begin
  Result := FListBox.Options;
end;

procedure TscGPCustomComboBox.SetListBoxOptions(Value: TscGPScrollingControlOptions);
begin
   FListBox.Options.Assign(Value);
end;

function TscGPCustomComboBox.GetListBoxHeaderOptions: TscGPHeaderOptions;
begin
  Result := FListBox.HeaderOptions;
end;

procedure TscGPCustomComboBox.SetListBoxHeaderOptions(Value: TscGPHeaderOptions);
begin
  FListBox.HeaderOptions.Assign(Value);
end;

function TscGPCustomComboBox.GetListBoxSelectionOptions: TscGPSelectionOptions;
begin
  Result := FListBox.SelectionOptions;
end;

procedure TscGPCustomComboBox.SetListBoxSelectionOptions(Value: TscGPSelectionOptions);
begin
  FListBox.SelectionOptions.Assign(Value);
end;

procedure TscGPCustomComboBox.SetListBoxItemMargin(Value: Integer);
begin
  FListBox.ItemMargin := Value;
end;

procedure TscGPCustomComboBox.SetListBoxItemSpacing(Value: Integer);
begin
  FListBox.ItemSpacing := Value;
end;

function TscGPCustomComboBox.GetListBoxIndentMargin: Integer;
begin
  Result := FListBox.IndentMargin;
end;

procedure TscGPCustomComboBox.SetListBoxIndentMargin(Value: Integer);
begin
  FListBox.IndentMargin := Value;
end;

function TscGPCustomComboBox.GetListBoxLineColorAlpha: TColor;
begin
  Result := FListBox.LineColorAlpha;
end;

procedure TscGPCustomComboBox.SetListBoxLineColorAlpha(Value: TColor);
begin
  FListBox.LineColorAlpha := Value;
end;

function TscGPCustomComboBox.GetListBoxLineColor: TColor;
begin
  Result := FListBox.LineColor;
end;

procedure TscGPCustomComboBox.SetListBoxLineColor(Value: TColor);
begin
  FListBox.LineColor := Value;
end;

function TscGPCustomComboBox.GetListBoxShowItemDetails: Boolean;
begin
  Result := FListBox.ShowItemDetails;
end;

procedure TscGPCustomComboBox.SetListBoxShowItemDetails(Value: Boolean);
begin
  FListBox.ShowItemDetails := Value;
end;

function TscGPCustomComboBox.GetListBoxShowLines: Boolean;
begin
  Result := FListBox.ShowLines;
end;

procedure TscGPCustomComboBox.SetListBoxShowLines(Value: Boolean);
begin
  FListBox.ShowLines := Value;
end;

function TscGPCustomComboBox.GetListBoxItemHeight: Integer;
begin
  Result := FlistBox.ItemHeight;
end;

procedure TscGPCustomComboBox.SetListBoxItemHeight(Value: Integer);
begin
  FlistBox.ItemHeight := Value;
end;

function TscGPCustomComboBox.GetListBoxHeaderHeight: Integer;
begin
  Result := FListBox.HeaderHeight;
end;

procedure TscGPCustomComboBox.SetListBoxHeaderHeight(Value: Integer);
begin
  FListBox.HeaderHeight := Value;
end;

procedure TscGPCustomComboBox.SetShowItemImage(Value: Boolean);
begin
  if FShowItemImage <> Value then
  begin
    FShowItemImage := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomComboBox.SetShowItemDetail(Value: Boolean);
begin
  if FShowItemDetail <> Value then
  begin
    FShowItemDetail := Value;
    RePaintControl;
  end;
end;

procedure TscGPCustomComboBox.SetShowItemText(Value: Boolean);
begin
  if FShowItemText <> Value then
  begin
    FShowItemText := Value;
    RePaintControl;
  end;
end;

constructor TscGPEditOptions.Create;
begin
  inherited;

  FShapeFillStyle := scgpsfColor;
  FShapeFillGradientAngle := 90;

  FNormalColor := clWindow;
  FHotColor := clWindow;
  FFocusedColor := clWindow;
  FDisabledColor := clWindow;
  FFrameNormalColor := clBtnText;
  FFrameHotColor := clHighLight;
  FFrameFocusedColor := clHighLight;
  FFrameDisabledColor := clBtnText;
  FFrameWidth := 1;
  FFontNormalColor := clWindowText;
  FFontHotColor := clWindowText;
  FFontFocusedColor := clWindowText;
  FFontDisabledColor := clGrayText;
  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;

  FNormalColorAlpha := 200;
  FHotColorAlpha := 255;
  FFocusedColorAlpha := 255;
  FDisabledColorAlpha := 150;

  FFrameNormalColorAlpha := 100;
  FFrameHotColorAlpha := 255;
  FFrameFocusedColorAlpha := 255;
  FFrameDisabledColorAlpha := 50;

  FShapeCornerRadius := 10;
  FShapeStyle := scgpessRect;
  FScaleFrameWidth := False;
end;

procedure TscGPEditOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPEditOptions then
  begin
    FNormalColor := TscGPEditOptions(Source).FNormalColor;
    FHotColor := TscGPEditOptions(Source).FHotColor;
    FFocusedColor := TscGPEditOptions(Source).FFocusedColor;
    FDisabledColor := TscGPEditOptions(Source).FDisabledColor;
    FFrameNormalColor := TscGPEditOptions(Source).FFrameNormalColor;
    FFrameHotColor := TscGPEditOptions(Source).FFrameHotColor;
    FFrameFocusedColor := TscGPEditOptions(Source).FFrameFocusedColor;
    FFrameDisabledColor := TscGPEditOptions(Source).FFrameDisabledColor;
    FFrameWidth := TscGPEditOptions(Source).FFrameWidth;
    FFontNormalColor := TscGPEditOptions(Source).FFontNormalColor;
    FFontHotColor := TscGPEditOptions(Source).FFontHotColor;
    FFontFocusedColor := TscGPEditOptions(Source).FFontFocusedColor;
    FFontDisabledColor := TscGPEditOptions(Source).FFontDisabledColor;
    FNormalColorAlpha := TscGPEditOptions(Source).FNormalColorAlpha;
    FHotColorAlpha := TscGPEditOptions(Source).FHotColorAlpha;
    FFocusedColorAlpha := TscGPEditOptions(Source).FFocusedColorAlpha;
    FDisabledColorAlpha := TscGPEditOptions(Source).FDisabledColorAlpha;
    FFrameNormalColorAlpha := TscGPEditOptions(Source).FFrameNormalColorAlpha;
    FFrameHotColorAlpha := TscGPEditOptions(Source).FFrameHotColorAlpha;
    FFrameFocusedColorAlpha := TscGPEditOptions(Source).FFrameFocusedColorAlpha;
    FFrameDisabledColorAlpha := TscGPEditOptions(Source).FFrameDisabledColorAlpha;
    FShapeStyle := TscGPEditOptions(Source).ShapeStyle;
    FShapeCornerRadius := TscGPEditOptions(Source).FShapeCornerRadius;
    FShapeFillStyle :=  TscGPEditOptions(Source).FShapeFillStyle;
    FStyleColors := TscGPEditOptions(Source).FStyleColors;
  end
  else
    inherited Assign(Source);
end;

 function TscGPEditOptions.GetColorAlpha: Byte;
 begin
   Result := FNormalColorAlpha;
   case FState of
     scsHot: Result := FHotColorAlpha;
     scsPressed: Result := FFocusedColorAlpha;
     scsFocused: Result := FFocusedColorAlpha;
     scsDisabled: Result := FDisabledColorAlpha;
   end;
 end;

 function TscGPEditOptions.GetFrameColorAlpha: Byte;
 begin
   Result := FFrameNormalColorAlpha;
   case FState of
     scsHot: Result := FFrameHotColorAlpha;
     scsPressed: Result := FFrameFocusedColorAlpha;
     scsFocused: Result := FFrameFocusedColorAlpha;
     scsDisabled: Result := FFrameDisabledColorAlpha;
   end;
 end;

procedure TscGPEditOptions.SetShapeFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FShapeFillGradientAngle <> Value) then
  begin
    FShapeFillGradientAngle := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPEditOptions.SetShapeFillStyle(Value: TscGPShapeFillStyle);
begin
  if FShapeFillStyle <> Value then
  begin
    FShapeFillStyle := Value;
    Changed;
  end;
end;

function TscGPEditOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := FocusedColor;
    scsFocused: Result := FocusedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscGPEditOptions.GetFrameColor: TColor;
begin
  Result := FrameNormalColor;
  case FState of
    scsHot: Result := FrameHotColor;
    scsPressed: Result := FrameFocusedColor;
    scsFocused: Result := FrameFocusedColor;
    scsDisabled: Result := FrameDisabledColor;
  end;
end;

function TscGPEditOptions.GetFontColor: TColor;
begin
  Result := FontNormalColor;
  case FState of
    scsHot: Result := FontHotColor;
    scsPressed: Result := FontFocusedColor;
    scsFocused: Result := FontFocusedColor;
    scsDisabled: Result := FontDisabledColor;
  end;
end;

function TscGPEditOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscGPEditOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscGPEditOptions.GetFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscGPEditOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

function TscGPEditOptions.GetFrameNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameNormalColor)
  else
    Result := FFrameNormalColor;
end;

function TscGPEditOptions.GetFrameHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameHotColor)
  else
    Result := FFrameHotColor;
end;

function TscGPEditOptions.GetFrameFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameFocusedColor)
  else
    Result := FFrameFocusedColor;
end;

function TscGPEditOptions.GetFrameDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameDisabledColor)
  else
    Result := FFrameDisabledColor;
end;

function TscGPEditOptions.GetFontNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontNormalColor)
  else
    Result := FFontNormalColor;
end;

function TscGPEditOptions.GetFontHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontHotColor)
  else
    Result := FFontHotColor;
end;

function TscGPEditOptions.GetFontFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontFocusedColor)
  else
    Result := FFontFocusedColor;
end;

function TscGPEditOptions.GetFontDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontDisabledColor)
  else
    Result := FFontDisabledColor;
end;

procedure TscGPEditOptions.SetShapeStyle(Value: TscGPEditShapeStyle);
begin
  if FShapeStyle <> Value then
  begin
    FShapeStyle := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetShapeCornerRadius(Value: Integer);
begin
  if (FShapeCornerRadius <> Value) and (Value > 0) then
  begin
    FShapeCornerRadius := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetHotColorAlpha(Value: Byte);
begin
  if FHotColorAlpha <> Value then
  begin
    FHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFocusedColorAlpha(Value: Byte);
begin
  if FFocusedColorAlpha <> Value then
  begin
    FFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameNormalColor(Value: TColor);
begin
  if FFrameNormalColor <> Value then
  begin
    FFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameHotColor(Value: TColor);
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameFocusedColor(Value: TColor);
begin
  if FFrameFocusedColor <> Value then
  begin
    FFrameFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameDisabledColor(Value: TColor);
begin
  if FFrameDisabledColor <> Value then
  begin
    FFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameNormalColorAlpha(Value: Byte);
begin
   if FFrameNormalColorAlpha <> Value then
  begin
    FFrameNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameHotColorAlpha(Value: Byte);
begin
  if FFrameHotColorAlpha <> Value then
  begin
    FFrameHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameFocusedColorAlpha(Value: Byte);
begin
  if FFrameFocusedColorAlpha <> Value then
  begin
    FFrameFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameDisabledColorAlpha(Value: Byte);
begin
  if FFrameDisabledColorAlpha <> Value then
  begin
    FFrameDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFontNormalColor(Value: TColor);
begin
  if FFontNormalColor <> Value then
  begin
    FFontNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFontHotColor(Value: TColor);
begin
  if FFontHotColor <> Value then
  begin
    FFontHotColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFontFocusedColor(Value: TColor);
begin
  if FFontFocusedColor <> Value then
  begin
    FFontFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFontDisabledColor(Value: TColor);
begin
  if FFontDisabledColor <> Value then
  begin
    FFontDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value > 0) then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGPEditOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPEditButton.Create;
begin
  FEnabled := True;
  FVisible := False;
  FHint := '';
  FShowHint := False;
  FImageIndex := -1;
  FGlyphThickness := 2;
  FImageHotIndex := -1;
  FImagePressedIndex := -1;
  FDropDownMenu := nil;
  FRepeatClick := False;
  FRepeatClickInterval := 200;
  FKind := scgpebCustom;
  FGlyphColor := clWindowText;
  FGlyphColorAlpha := 180;
  FGlyphColorHotAlpha := 240;
  FGlyphColorPressedAlpha := 150;
  FGlyphSize := 0;
  FWidth := 0;
end;

procedure TscGPEditButton.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TscGPEditButton.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    FWidth := Value;
    if Assigned(FOnVisibleChange) then FOnVisibleChange(Self);
  end;
end;

procedure TscGPEditButton.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnVisibleChange) then FOnVisibleChange(Self);
  end;
end;

procedure TscGPEditButton.SetGlyphThickness(Value: Byte);
begin
  if (FGlyphThickness <> Value) and (Value >= 1) then
  begin
    FGlyphThickness := Value;
    Changed;
  end;
end;

procedure TscGPEditButton.SetGlyphSize(Value: Integer);
begin
  if FGlyphSize <> Value then
  begin
    FGlyphSize := Value;
    Changed;
  end;
end;

procedure TscGPEditButton.SetGlyphColor(Value: TColor);
begin
  if FGlyphColor <> Value then
  begin
    FGlyphColor := Value;
    Changed;
  end;
end;

procedure TscGPEditButton.SetGlyphColorAlpha(Value: Byte);
begin
  if FGlyphColorAlpha <> Value then
  begin
    FGlyphColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPEditButton.SetKind(Value: TscGPEditButtonKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TscGPEditButton.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TscGPEditButton.Assign(Source: TPersistent);
begin
  if Source is TscGPEditButton then
  begin
    FEnabled := TscGPEditButton(Source).Enabled;
    FVisible := TscGPEditButton(Source).Visible;
    FImageIndex := TscGPEditButton(Source).ImageIndex;
    FImageHotIndex := TscGPEditButton(Source).ImageHotIndex;
    FImagePressedIndex := TscGPEditButton(Source).ImagePressedIndex;
    FDropDownMenu := TscGPEditButton(Source).DropDownMenu;
    FKind := TscGPEditButton(Source).Kind;
    FGlyphColor := TscGPEditButton(Source).GlyphColor;
    FGlyphColorAlpha := TscGPEditButton(Source).GlyphColorAlpha;
    FGlyphColorHotAlpha := TscGPEditButton(Source).GlyphColorHotAlpha;
    FGlyphColorPressedAlpha := TscGPEditButton(Source).GlyphColorPressedAlpha;
    FHint := TscGPEditButton(Source).Hint;
    FShowHint := TscGPEditButton(Source).ShowHint;
    FGlyphSize := TscGPEditButton(Source).GlyphSize;
    FWidth :=  TscGPEditButton(Source).Width;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPEditButton.Changed;
begin
  if FVisible then
    if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPCustomEdit.Create(AOwner: TComponent);
begin
  FContentMarginLeft := 5;
  FContentMarginRight := 5;
  FContentMarginTop := 5;
  FContentMarginBottom := 5;
  inherited;
  FFluentUIOpaque := False;
  FCustomDraw := False;
  FMenuTracking := False;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FOptions := TscGPEditOptions.Create;
  FOptions.OnChange := OptionsChanged;
  FGlyphSize := 16;
  FButtonImages := nil;
  FLeftButton := TscGPEditButton.Create;
  FLeftButton.OnChange := OnButtonChange;
  FLeftButton.OnVisibleChange := OnButtonVisibleChange;
  FRightButton := TscGPEditButton.Create;
  FRightButton.OnChange := OnButtonChange;
  FRightButton.OnVisibleChange := OnButtonVisibleChange;
  FTransparent := True;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FPromptText := '';
  FHidePromptTextIfFocused := False;
  FPromptTextColor := clNone;
  FInheritedKeys := False;
  FStopGetParentBG := False;
  ParentBGBuffer := nil;
  BorderStyle := bsNone;
  FFrameColor := clBtnShadow;
  FFrameActiveColor := clHighLight;
  FRepeatClickTimer := nil;
  {$IFDEF VER230}
  FStyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
  Color := clBtnFace;
end;

procedure TscGPCustomEdit.OptionsChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TscGPCustomEdit.OnButtonVisibleChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.OnButtonChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TscGPCustomEdit.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
  if FOptions.FScaleFrameWidth then
     FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);
  FOptions.FShapeCornerRadius :=  MulDiv(FOptions.FShapeCornerRadius, M, D);

  FContentMarginLeft := MulDiv(FContentMarginLeft, M, D);
  FContentMarginTop := MulDiv(FContentMarginTop, M, D);
  FContentMarginRight := MulDiv(FContentMarginRight, M, D);
  FContentMarginBottom := MulDiv(FContentMarginBottom, M, D);

  if FLeftButton.FGlyphSize <> 0 then
    FLeftButton.FGlyphSize := MulDiv(FLeftButton.FGlyphSize, M, D);
  if FRightButton.FGlyphSize <> 0 then
    FRightButton.FGlyphSize := MulDiv(FRightButton.FGlyphSize, M, D);

  if FLeftButton.FWidth <> 0 then
     FLeftButton.FWidth := MulDiv(FLeftButton.FWidth, M, D);

  if FRightButton.FWidth <> 0 then
     FRightButton.FWidth := MulDiv(FRightButton.FWidth, M, D);

  inherited;
  if not (csLoading in ComponentState) then
    SetTimer(Handle, 100, 10, nil);
end;

procedure TscGPCustomEdit.SetButtonImages(Value: TCustomImageList);
begin
  if FButtonImages <> Value then
  begin
    FButtonImages := Value;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.SetPromptText(Value: String);
begin
  if FPromptText <> Value then
  begin
    FPromptText := Value;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.SetPromptTextColor(Value: TColor);
begin
  if FPromptTextColor <> Value then
  begin
    FPromptTextColor := Value;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.WMCHAR;
var
  Key: Char;
begin
  if FInheritedKeys then
  begin
    inherited;
    Exit;
  end;
  if Message.CharCode in [VK_ESCAPE]
  then
    begin
      Key := #27;
      if Assigned(OnKeyPress) then OnKeyPress(Self, Key);
      KeyPress(Key);
      if GetParentForm(Self) <> nil
      then
        GetParentForm(Self).Perform(CM_DIALOGKEY, Message.CharCode, Message.KeyData);
    end
  else
  if Message.CharCode in [VK_RETURN]
  then
    begin
      Key := #13;
      if Assigned(OnKeyPress) then OnKeyPress(Self, Key);
      if GetParentForm(Self) <> nil
      then
        GetParentForm(Self).Perform(CM_DIALOGKEY, Message.CharCode, Message.KeyData);
    end
  else
    inherited;
end;

destructor TscGPCustomEdit.Destroy;
begin
  if FRepeatClickTimer <> nil then
    StopRepeatClick;
  if ParentBGBuffer <> nil then
    ParentBGBuffer.Free;
  FOptions.Free;
  FLeftButton.Free;
  FRightButton.Free;
  inherited;
end;

procedure TscGPCustomEdit.RepeatClickTimerProc(Sender: TObject);
begin
  if LeftButton.Visible and LeftButton.Down then
    if Assigned(FOnLeftButtonClick) then FOnLeftButtonClick(Self);
  if RightButton.Visible and RightButton.Down then
    if Assigned(FOnRightButtonClick) then FOnRightButtonClick(Self);
end;

procedure TscGPCustomEdit.WaitClickTimerProc(Sender: TObject);
begin
  FRepeatClickTimer.Enabled := False;
  FRepeatClickTimer.OnTimer := RepeatClickTimerProc;
  if LeftButton.Visible and LeftButton.Down then
    FRepeatClickTimer.Interval := LeftButton.RepeatClickInterval
  else
  if RightButton.Visible and RightButton.Down then
    FRepeatClickTimer.Interval := RightButton.RepeatClickInterval;
  FRepeatClickTimer.Enabled := True;
end;

procedure TscGPCustomEdit.StartRepeatClick;
begin
  if FRepeatClickTimer <> nil then FreeAndNil(FRepeatClickTimer);
  FRepeatClickTimer := TTimer.Create(Self);
  FRepeatClickTimer.Enabled := False;
  FRepeatClickTimer.OnTimer := WaitClickTimerProc;
  FRepeatClickTimer.Interval := 500;
  FRepeatClickTimer.Enabled := True;
end;

procedure TscGPCustomEdit.StopRepeatClick;
begin
  if FRepeatClickTimer <> nil then
  begin
    FRepeatClickTimer.Enabled := False;
    FreeAndNil(FRepeatClickTimer);
  end;
end;

function TscGPCustomEdit.GetEditHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  I := Self.ContentMarginTop + Self.ContentMarginBottom;
  Result := Metrics.tmHeight + I;
end;

procedure TscGPCustomEdit.AdjustHeight;
begin
  Height := GetEditHeight;
end;

procedure TscGPCustomEdit.WMSetFocus(var Message: TWMSETFOCUS);
begin
  inherited;
  AdjustTextRect;
  if AutoSelect then SelectAll;
end;

function TscGPCustomEdit.IsCustomDraw(ADC: HDC): Boolean;
begin
  Result := FCustomDraw;
end;

procedure TscGPCustomEdit.SetContentMarginLeft(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginLeft <> Value) then
  begin
    FContentMarginLeft := Value;
    if AutoSize then
      AdjustHeight;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.SetContentMarginTop(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginTop <> Value) then
  begin
    FContentMarginTop := Value;
     if AutoSize then
      AdjustHeight;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.SetContentMarginRight(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginRight <> Value) then
  begin
    FContentMarginRight := Value;
     if AutoSize then
      AdjustHeight;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.SetContentMarginBottom(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginBottom <> Value) then
  begin
    FContentMarginBottom := Value;
    if AutoSize then
      AdjustHeight;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (AHeight <> Height) and AutoSize and not (csLoading in ComponentState) then
    AHeight := GetEditHeight;
  inherited;
end;

procedure TscGPCustomEdit.WMSIZE(var Msg: TMessage);
begin
  FStopGetParentBG := False;
  inherited;
  if AutoSize and (csLoading in ComponentState) then
    AdjustHeight;
  AdjustTextRect;
end;

procedure TscGPCustomEdit.CreateWnd;
begin
  inherited;
  AdjustTextRect;
  if IsFluentUIOpaque then
  begin
    FTransparent := False;
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
       SetTimer(Handle, 101, 10, nil);
  end;
end;

procedure TscGPCustomEdit.Loaded;
begin
  inherited;
  if AutoSize then
    AdjustHeight;
  AdjustTextRect;
end;

procedure TscGPCustomEdit.GetParentBG;
var
  R: TRect;
begin
  if FStopGetParentBG then Exit;
  if ParentBGBuffer = nil then
      ParentBGBuffer := TBitmap.Create;
  if Width * Height <> 0 then
  begin
    if (ParentBGBuffer.Width <> Width) or (ParentBGBuffer.Height <> Height) then
    begin
      ParentBGBuffer.Width := Width;
      ParentBGBuffer.Height := Height;
    end;
    if FTransparent then
    begin
      if (Parent <> nil) and (Parent is TscCustomControl) then
        TscCustomControl(Parent).FGetControlBG := True;
      try
        if (Parent <> nil) and (Parent is TscCustomScrollBox) and
           TscCustomScrollBox(Parent).StorePaintBuffer and
           (TscCustomScrollBox(Parent).FPaintBuffer <> nil) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Width > 0) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Height > 0) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Width = TscCustomScrollBox(Parent).ClientWidth) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Height = TscCustomScrollBox(Parent).ClientHeight)
        then
        begin
          R.Left := Self.Left;
          R.Top := Self.Top;
          R.Right := R.Left + Self.Width;
          R.Bottom := R.Top + Self.Height;
          ParentBGBuffer.Canvas.CopyRect(Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height),
            TscCustomScrollBox(Parent).FPaintBuffer.Canvas, R);
        end
        else
        if (Parent is TscCustomControl) and TscCustomControl(Parent).StorePaintBuffer and
           (TscCustomControl(Parent).FPaintBuffer <> nil) and
           (TscCustomControl(Parent).FPaintBuffer.Width > 0) and
           (TscCustomControl(Parent).FPaintBuffer.Height > 0) and
           (TscCustomControl(Parent).FPaintBuffer.Width = TscCustomControl(Parent).Width) and
           (TscCustomControl(Parent).FPaintBuffer.Height = TscCustomControl(Parent).Height)
        then
        begin
          R.Left := Self.Left;
          R.Top := Self.Top;
          R.Right := R.Left + Self.Width;
          R.Bottom := R.Top + Self.Height;
          ParentBGBuffer.Canvas.CopyRect(Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height),
            TscCustomControl(Parent).FPaintBuffer.Canvas, R);
        end
        else
          DrawParentBackground(Self, ParentBGBuffer.Canvas);
      finally
       if (Parent <> nil) and (Parent is TscCustomControl) then
         TscCustomControl(Parent).FGetControlBG := False;
      end;
    end
    else
      with ParentBGBuffer.Canvas do
      begin
        R := Rect(0, 0, Width, Height);
        if (seClient in StyleElements) and IsCustomStyle then
          Brush.Color := ColorToRGB(GetEditBrushColor(scsNormal))
        else
          Brush.Color := ColorToRGB(Self.Color);
        FillRect(R);
      end;
  end;
end;

procedure TscGPCustomEdit.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    GetParentBG;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.WMMove(var Msg: TMessage);
begin
  inherited;
  if FTransparent then
  begin
    FStopGetParentBG := False;
    DoPaint;
  end;
end;

procedure TscGPCustomEdit.DoPaint;
var
  MyDC: HDC;
  TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  if not HandleAllocated then Exit;
  FStopDraw := True;
  HideCaret(Handle);
  try
    MyDC := GetDC(Handle);
    try
      TempDC := CreateCompatibleDC(MyDC);
      try
        TempBmp := CreateCompatibleBitmap(MyDC, Succ(Width), Succ(Height));
        try
          OldBmp := SelectObject(TempDC, TempBmp);
          SendMessage(Handle, WM_ERASEBKGND, TempDC, 0);
          SendMessage(Handle, WM_PAINT, TempDC, 0);
          BitBlt(MyDC, 0, 0, Width, Height, TempDC, 0, 0, SRCCOPY);
          SelectObject(TempDC, OldBmp);
        finally
          DeleteObject(TempBmp);
        end;
      finally
        DeleteDC(TempDC);
      end;
    finally
      ReleaseDC(Handle, MyDC);
    end;
  finally
    ShowCaret(Handle);
    FStopDraw := False;
  end;
end;

procedure TscGPCustomEdit.DoPaint2(DC: HDC);
var
  MyDC: HDC;
  TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  if not HandleAllocated then Exit;
  FStopDraw := True;
  try
    MyDC := DC;
    try
      TempDC := CreateCompatibleDC(MyDC);
      try
        TempBmp := CreateCompatibleBitmap(MyDC, Succ(Width), Succ(Height));
        try
          OldBmp := SelectObject(TempDC, TempBmp);
          SendMessage(Handle, WM_ERASEBKGND, TempDC, 0);
          SendMessage(Handle, WM_PAINT, TempDC, 0);
          BitBlt(MyDC, 0, 0, Width, Height, TempDC, 0, 0, SRCCOPY);
          SelectObject(TempDC, OldBmp);
        finally
          DeleteObject(TempBmp);
        end;
      finally
        DeleteDC(TempDC);
      end;
    finally
      ReleaseDC(Handle, MyDC);
    end;
  finally
    FStopDraw := False;
  end;
end;

procedure TscGPCustomEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if AutoSize then AdjustHeight;
  AdjustTextRect;
end;

procedure TscGPCustomEdit.CMSENCPaint(var Message: TMessage);
begin
  Message.Result := SE_RESULT;
end;

procedure TscGPCustomEdit.PaintToDC(DC: HDC; X, Y: Integer);
var
  SaveIndex: Integer;
  FStoredMouseIn: Boolean;
begin
  ControlState := ControlState + [csPaintCopy];
  SaveIndex := SaveDC(DC);
  try
    MoveWindowOrg(DC, X, Y);
    IntersectClipRect(DC, 0, 0, Width, Height);
    FStoredMouseIn := FMouseIn;
    FMouseIn := False;
    IntersectClipRect(DC, 0, 0, ClientWidth, ClientHeight);
    Perform(WM_PAINT, DC, 0);
    FMouseIn := FStoredMouseIn;
  finally
    RestoreDC(DC, SaveIndex);
  end;
  ControlState := ControlState - [csPaintCopy];
end;

procedure TscGPCustomEdit.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  if not FStopDraw then
  begin
    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    try
      DoPaint2(DC);
      ShowCaret(Handle);
    finally
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end
  else
    begin
      if Focused then
        inherited
      else
      begin
        if not IsCustomDraw(Message.DC) then
         inherited;
      end;
    end;
end;

procedure TscGPCustomEdit.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

function TscGPCustomEdit.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscGPCustomEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or ES_MULTILINE;
    if IsFluentUIOpaque then
      ExStyle := Exstyle or WS_EX_LAYERED;
  end;
end;

procedure TscGPCustomEdit.DrawEditButton(ACanvas: TCanvas; G: TGPGraphics; AButton: TscGPEditButton);
var
  I, X, Y: Integer;
  C: Cardinal;
  R: TGPRectF;
  R1: TRect;
  T: Byte;
  FIntGlyphSize: Integer;
begin
  if AButton = nil then Exit;
  if not AButton.Visible then Exit;

  if AButton.Kind = scgpebCustom then
  begin
    if (FButtonImages <> nil) and (AButton.ImageIndex >= 0) and (AButton.ImageIndex < FButtonImages.Count) then
    begin
       Y := AButton.ButtonRect.Top + AButton.ButtonRect.Height div 2 -
         FButtonImages.Height div 2;
      // if Y < AButton.ButtonRect.Top then Y := AButton.ButtonRect.Top;
         X := AButton.ButtonRect.Left + AButton.ButtonRect.Width div 2 -
         FButtonImages.Width div 2;
      // if X < AButton.ButtonRect.Left then X := AButton.ButtonRect.Left;
       I := AButton.ImageIndex;
       if AButton.Down and AButton.MouseIn and
         (AButton.ImagePressedIndex >= 0) and (AButton.ImagePressedIndex < FButtonImages.Count)
       then
         I := AButton.ImagePressedIndex
       else
       if AButton.MouseIn and (AButton.ImageHotIndex >= 0) and
        (AButton.ImageHotIndex < FButtonImages.Count)
       then
         I := AButton.ImageHotIndex;
       if Enabled and AButton.Enabled then
       begin
         if True then
         if AButton.Down and AButton.MouseIn and
           (AButton.ImagePressedIndex = AButton.ImageIndex)
         then
           DrawBitmapFromImageList(ACanvas, X, Y, FButtonImages, I, 200)
         else
           FButtonImages.Draw(ACanvas, X, Y, I, Enabled)
       end
       else
         DrawBitmapFromImageList(ACanvas, X, Y, FButtonImages,
           I, DisabledImageAlphaValue);
    end;
    Exit;
  end;

  FIntGlyphSize := FGlyphSize;

  if AButton.GlyphSize > 0 then
    FIntGlyphSize := AButton.GlyphSize;

  if AButton.Down then
    C := ColorToGPColor(GetStyleColor(AButton.GlyphColor), AButton.GlyphColorPressedAlpha)
  else
  if AButton.MouseIn then
    C := ColorToGPColor(GetStyleColor(AButton.GlyphColor), AButton.GlyphColorHotAlpha)
  else
  if AButton.Enabled and Self.Enabled then
    C := ColorToGPColor(GetStyleColor(AButton.GlyphColor), AButton.GlyphColorAlpha)
  else
    C := ColorToGPColor(GetStyleColor(AButton.GlyphColor), AButton.GlyphColorAlpha div 2);
  R1 := AButton.ButtonRect;
  R1.Top := R1.Top + R1.Height div 2 - FIntGlyphSize div 2;
  R1.Bottom := R1.Top + FIntGlyphSize;
  R1.Left := R1.Left + R1.Width div 2 - FIntGlyphSize div 2;
  R1.Right := R1.Left + FIntGlyphSize;
  R := RectToGPRect(R1);
  if AButton.Kind = scgpebMore then
    InflateGPRect(R, 2 * FScaleFactor, 2 * FScaleFactor)
  else
  if AButton.Kind = scgpebSearch then
    InflateGPRect(R, FScaleFactor, FScaleFactor);

  T := AButton.GlyphThickness;

  case AButton.Kind of
    scgpebDropDown, scgpebDownArrow:
    begin
      GPDrawDownGlyph(G, R, C, FScaleFactor, T);
    end;
    scgpebUpArrow:
    begin
      GPDrawUpGlyph(G, R, C, FScaleFactor, T);
    end;
    scgpebLeftArrow:
    begin
      GPDrawLeftGlyph(G, R, C, FScaleFactor, T);
    end;
    scgpebRightArrow:
    begin
      GPDrawRightGlyph(G, R, C, FScaleFactor, T);
    end;
    scgpebClear:
    begin
      GPDrawClearGlyph(G, R, C, FScaleFactor, T);
    end;
    scgpebMore:
      GPDrawMoreGlyph(G, R, C, FScaleFactor, T);
    scgpebDetails:
      GPDrawDetailsGlyph(G, R, C, FScaleFactor);
    scgpebNext:
      GPDrawNextGlyph(G, R, C, FScaleFactor, T);
    scgpebPrior:
      GPDrawPriorGlyph(G, R, C, FScaleFactor, T);
    scgpebSearch:
      GPDrawSearchGlyph(G, R, C, FScaleFactor, T);
    scgpebRefresh:
      GPDrawRefreshGlyph2(G, R, C, FScaleFactor, T);
    scgpebMic:
      GPDrawMicGlyph2(G, R, C, FScaleFactor, T);
  end;
end;

procedure TscGPCustomEdit.DrawEditShape(ACanvas: TCanvas);
var
  R, TR: TRect;
  G: TGPGraphics;
  B: TGPBrush;
  LB: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, R1: TGPRectF;
  FrameColor, FillColor: Cardinal;
  l, t, w, h, d: Single;
  C1, C2: Cardinal;
  FCtrlState: TscsCtrlState;
begin
  R := Rect(0, 0, Width, Height);
  TR := R;
  if not FTransparent then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;
  FCtrlState := scsNormal;
  if not Enabled then
    FCtrlState := scsDisabled
  else
  if Focused then
    FCtrlState := scsFocused
  else
  if FMouseIn then
    FCtrlState := scsHot;
  FOptions.State := FCtrlState;
  // draw button shape
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  P := TGPPen.Create(0, FOptions.FrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  // colors
  FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
  FillColor := ColorToGPColor(Options.Color, Options.ColorAlpha);
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
  if FOptions.ShapeFillStyle = scgpsfColor then
    B := TGPSolidBrush.Create(FillColor)
  else
  begin
    C1 := ColorToGPColor(LighterColor(FOptions.Color, 25), Options.ColorAlpha);
    C2 := ColorToGPColor(DarkerColor(FOptions.Color, 25), Options.ColorAlpha);
    R1 := FillR;
    InflateGPRect(R1, 1, 1);
    B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientAngle);
  end;
  // draw
  try
    case FOptions.ShapeStyle of
      scgpessLine:
        begin
          LB := TGPSolidBrush.Create(FrameColor);
          FrameR.Height := FOptions.FFrameWidth;
          FrameR.Y := Height - FOptions.FFrameWidth;
          G.FillRectangle(LB, FrameR);
          LB.Free;
        end;
      scgpessRect:
        begin
          G.FillRectangle(B, FillR);
          G.DrawRectangle(P, FrameR);
        end;
      scgpessRoundedRect, scgpessRoundedLeftRight:
        begin
          // fill
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if Options.ShapeStyle = scgpessRoundedLeftRight
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
          // frame
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          if Options.ShapeStyle = scgpessRoundedLeftRight
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
    end;
    DrawEditButton(ACanvas, G, FLeftButton);
    DrawEditButton(ACanvas, G, FRightButton);
  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;

  if Assigned(FOnDrawBackgroundEvent) then
    FOnDrawBackgroundEvent(ACanvas, GetTextRect, FCtrlState);
end;

procedure TscGPCustomEdit.DrawEditBackground(ACanvas: TCanvas);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    if not FTransparent then
    begin
      GetParentBG;
      if ParentBGBuffer <> nil then
        ACanvas.Draw(0, 0, ParentBGBuffer);
    end;
    DrawEditShape(ACanvas);
    DrawPromptText(ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscGPCustomEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  FCanvas: TCanvas;
begin
  if not FStopDraw then
    DoPaint
  else
    begin
      GetParentBG;
      if ParentBGBuffer <> nil then
      begin
        FCanvas := TCanvas.Create;
        try
          FCanvas.Handle := Message.DC;
          if ParentBGBuffer <> nil then
            FCanvas.Draw(0, 0, ParentBGBuffer);
          DrawEditShape(FCanvas);
          DrawPromptText(FCanvas);
        finally
          FCanvas.Handle := 0;
          FCanvas.Free;
        end;
      end;
    end;
end;

function TscGPCustomEdit.GetTextColor: TColor;
begin
  Result := Self.Font.Color;
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if Enabled then
      Result := ColorToRGB(GetStyleColor(Self.Font.Color))
    else
    if FTransparent then
      Result := ColorToRGB(GetCheckBoxTextColor(scsDisabled))
    else
      Result := ColorToRGB(GetEditTextColor(scsDisabled));
  end
  else
  if not Enabled then
    Result := clGrayText;
end;

procedure TscGPCustomEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CHAR:
    begin
      if Char(TWMChar(Message).CharCode) = ^A then
        SelectAll
      else
        inherited;
    end;

    WM_KEYDOWN:
    begin
      if TWMKeyDown(Message).CharCode <> VK_TAB then
        inherited;
    end;

    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        FTextColor := GetTextColor;
        SetBkMode(Message.WParam, WinApi.Windows.Transparent);
        if (seFont in StyleElements) and IsCustomStyle then
          SetTextColor(Message.WParam, FTextColor)
        else
        if Enabled then
        begin
          SetTextColor(Message.WParam, ColorToRGB(Font.Color));
          FTextColor := Font.Color;
        end
        else
        begin
          SetTextColor(Message.WParam, ColorToRGB(clGrayText));
          FTextColor := clGrayText;
        end;
        Message.Result := GetStockObject(NULL_BRUSH);
      end;
    else
      inherited;
  end;

    case Message.Msg of
      WM_NCPAINT:
        DoPaint;
      WM_CHAR:
      begin
        FStopGetParentBG := True;
        DoPaint;
        FStopGetParentBG := False;
      end;
      WM_CREATE:
      begin
        FMenuTracking := False;
      end;
      WM_TIMER:
        if (TWMTimer(Message).TimerID = 22) and FMenuTracking then
        begin
          KillTimer(Handle, 22);
          FMenuTracking := False;
          Invalidate;
        end
        else
        if (TWMTimer(Message).TimerID = 101) and IsFluentUIOpaque then
        begin
          KillTimer(Handle, 101);
          RedrawWindow(Handle, nil, 0,
            RDW_INVALIDATE + RDW_ERASE + RDW_FRAME + RDW_ALLCHILDREN + RDW_UPDATENOW);
        end;
    end;
end;

procedure TscGPCustomEdit.Change;
begin
  inherited;
  FStopGetParentBG := True;
  DoPaint;
  FStopGetParentBG := False;
end;

procedure TscGPCustomEdit.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  FStopGetParentBG := False;
  DoPaint;
end;

function TscGPCustomEdit.GetTextRect: TRect;
var
  R, R1: TRect;
  W: Integer;
  Buffer: TBitmap;
begin
  Result := ClientRect;
  Inc(Result.Left, FContentMarginLeft);
  Inc(Result.Top, FContentMarginTop);
  Dec(Result.Right, FContentMarginRight);
  Dec(Result.Bottom, FContentMarginBottom);
  if not Assigned(FLeftButton) or not Assigned(FRightButton) then Exit;
  R := Result;
  R1 := R;
  if FOptions.ScaleFrameWidth then
    W := FOptions.FFrameWidth
  else
    W := Round(FOptions.FrameWidth * FScaleFactor);
  R1.Top := W * 2;
  R1.Bottom := Height - W * 2;
  FLeftButton.ButtonRect := Rect(0, 0, 0, 0);
  FRightButton.ButtonRect := Rect(0, 0, 0, 0);
  FGlyphSize := R.Height;
  if BidiMode = bdRightToLeft then
  begin
   if FRightButton.Visible then
   begin
     if FRightButton.Width > 0 then
       FRightButton.ButtonRect := Rect(R.Left, R1.Top, R.Left + FRightButton.Width, R1.Bottom)
     else
       FRightButton.ButtonRect := Rect(R.Left, R1.Top, R.Left + R1.Height, R1.Bottom);
     Inc(R.Left, FRightButton.ButtonRect.Width + Round(4 * FScaleFactor));
   end;
   if FLeftButton.Visible then
   begin
     if FLeftButton.Width > 0 then
       FLeftButton.ButtonRect := Rect(R.Right - FLeftButton.Width, R1.Top, R.Right, R1.Bottom)
     else
       FLeftButton.ButtonRect := Rect(R.Right - R1.Height, R1.Top, R.Right, R1.Bottom);
     Dec(R.Right, FLeftButton.ButtonRect.Width + Round(4 * FScaleFactor));
   end;
  end
  else
  begin
    if FLeftButton.Visible then
    begin
      if FLeftButton.Width > 0 then
        FLeftButton.ButtonRect := Rect(R.Left, R1.Top, R.Left + FLeftButton.Width, R1.Bottom)
      else
        FLeftButton.ButtonRect := Rect(R.Left, R1.Top, R.Left + R1.Height, R1.Bottom);

      Inc(R.Left, LeftButton.ButtonRect.Width + Round(4 * FScaleFactor));
    end;
    if FRightButton.Visible then
    begin
      if FRightButton.Width > 0 then
        FRightButton.ButtonRect := Rect(R.Right - FRightButton.Width, R1.Top, R.Right, R1.Bottom)
      else
        FRightButton.ButtonRect := Rect(R.Right - R1.Height, R1.Top, R.Right, R1.Bottom);

      Dec(R.Right, RightButton.ButtonRect.Width + Round(4 * FScaleFactor));
    end;
  end;

  Buffer := TBitmap.Create;
  try
    Buffer.Canvas.Font := Self.Font;
    if R.Width < Buffer.Canvas.TextWidth('W') then
      R.Width := Buffer.Canvas.TextWidth('W');
    if R.Height < Buffer.Canvas.TextHeight('W') then
     R.Height := Buffer.Canvas.TextHeight('W');
  finally
    Buffer.Free;
  end;

  Result := R;
end;

procedure TscGPCustomEdit.AdjustTextRect;
var
  R: TRect;
begin
  if not HandleAllocated then Exit;
  R := GetTextRect;
  Perform(EM_SETRECTNP, 0, Longint(@R));
end;

procedure TscGPCustomEdit.Notification;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FButtonImages) then
  begin
    FButtonImages := nil;
    if HandleAllocated then Invalidate;
  end;
  if (Operation = opRemove) and (FLeftButton <> nil) and (AComponent = FLeftButton.DropDownMenu) then
   FLeftButton.DropDownMenu := nil;
 if (Operation = opRemove) and (FRightButton <> nil) and (AComponent = FRightButton.DropDownMenu) then
   FRightButton.DropDownMenu := nil;
 if (Operation = opRemove) and (AComponent = FHintComponent) then
    FHintComponent := nil;
end;

procedure TscGPCustomEdit.DrawPromptText(ACanvas: TCanvas);
var
  R: TRect;
  C: TColor;
begin
  if (FPromptText = '') or (Text <> '') then Exit;

  if Focused and FHidePromptTextIfFocused then
    Exit;

  R := GetTextRect;
  if BorderStyle <> bsNone then
    InflateRect(R, -1, -1);
  ACanvas.Font := Self.Font;
  R.Bottom := R.Top + ACanvas.TextHeight('0');
  if FPromptTextColor = clNone then
  begin
    C := clGrayText;
    if (seFont in StyleElements) and IsCustomStyle then
    begin
      if Transparent then
        C := ColorToRGB(GetCheckBoxTextColor(scsDisabled))
      else
        C := ColorToRGB(GetEditTextColor(scsDisabled));
    end;
  end
  else
    C := ColorToRGB(GetStyleColor(FPromptTextColor));
  ACanvas.Font.Color := C;
  ACanvas.Brush.Style := bsClear;
  scDrawUtils.DrawTextAlignmentNoPrefix(ACanvas, FPromptText, R,
    Alignment, IsRightToLeft);
end;

procedure TscGPCustomEdit.WMNCHITTEST(var Message: TWMNCHITTEST);
var
  P: TPoint;    
  R: TRect;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  if not FMouseIn and not (csDesigning in ComponentState) then
    EditMouseEnter;

  if FLeftButton.Visible or FRightButton.Visible then
  begin
    P.X := Message.XPos;
    P.Y := Message.YPos;
    P := ScreenToClient(P);
    if FLeftButton.Visible and PtInRect(LeftButton.ButtonRect, P) then
    begin
      Message.Result := HTEDITBUTTONL;
      if not FLeftButton.MouseIn and FLeftButton.Enabled then
      begin
        FLeftButton.MouseIn := True;
        FRightButton.MouseIn := False;
        Invalidate;
        if (FLeftButton.Hint <> '') and (FLeftButton.ShowHint) and (FHintComponent <> nil) then
          FHintComponent.ActivateHint(FLeftButton.Hint);
       end;
      Exit;
    end
    else
    if FRightButton.Visible and PtInRect(RightButton.ButtonRect, P) then
    begin
      Message.Result := HTEDITBUTTONR;
      if not FRightButton.MouseIn and FRightButton.Enabled  then
      begin
        FRightButton.MouseIn := True;
        FLeftButton.MouseIn := False;
        Invalidate;
        if (FRightButton.Hint <> '') and (FRightButton.ShowHint) and (FHintComponent <> nil) then
          FHintComponent.ActivateHint(FRightButton.Hint);
      end;
      Exit;
    end
    else
    if FLeftButton.Visible and FRightButton.Visible and
       (FLeftButton.ButtonRect.Left = FRightButton.ButtonRect.Left) and
       (P.X >= FLeftButton.ButtonRect.Left) and (P.X <= FLeftButton.ButtonRect.Right)
       and (P.Y = FRightButton.ButtonRect.Bottom)
    then
      Message.Result := HTUPDOWN;
  end;

  if (FHintComponent <> nil) then
      FHintComponent.HideHint;

  if FLeftButton.Visible and FLeftButton.Enabled and FLeftButton.MouseIn then
  begin
    LeftButton.MouseIn := False;
    if LeftButton.Down and (LeftButton.DropDownMenu = nil) then
    begin
      LeftButton.Down := False;
      if LeftButton.RepeatClick then StopRepeatClick;
    end;
    Invalidate;
  end;
  if FRightButton.Visible and FRightButton.Enabled and FRightButton.MouseIn then
  begin
    RightButton.MouseIn := False;
    if RightButton.Down and (RightButton.DropDownMenu = nil) then
    begin
      RightButton.Down := False;
      if RightButton.RepeatClick then StopRepeatClick;
    end;
    Invalidate;
  end;

  if LeftButton.Visible then
  begin 
    R := Self.GetTextRect;
    if P.X < R.Left then
    begin
      Message.Result := HTBORDER;
      Exit;
    end;
  end;

  if RightButton.Visible then
  begin 
    R := Self.GetTextRect;
    if P.X > R.Right then
    begin
      Message.Result := HTBORDER;
      Exit;
    end;
  end;

  
  if Message.Result <> HTUPDOWN then
    inherited;
end;


procedure TscGPCustomEdit.WMNCLBUTTONDBCLK;
var
  P: TPoint;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  if (FHintComponent <> nil) then
     FHintComponent.HideHint;

  if FLeftButton.Visible and FLeftButton.Enabled and (Message.HitTest = HTEDITBUTTONL) and not FMenuTracking then
  begin
    FLeftButton.Down := True;
    if not Focused then SetFocus;
    Invalidate;
    if LeftButton.RepeatClick then StartRepeatClick;
    if FLeftButton.FDropDown then
       if Assigned(FOnLeftButtonClick) then FOnLeftButtonClick(Self);
    if FLeftButton.DropDownMenu <> nil then
    begin
      FMenuTracking := True;
      P := ClientToScreen(Point(0, Self.Height));
      FLeftButton.DropDownMenu.Popup(P.X, P.Y);
      FLeftButton.Down := False;
//      Invalidate;
      SetTimer(Handle, 22, 10, nil);
    end;
  end
  else
  if FRightButton.Visible and FRightButton.Enabled and (Message.HitTest = HTEDITBUTTONR) and not FMenuTracking then
  begin
    FRightButton.Down := True;
    if not Focused then SetFocus;
    Invalidate;

    if RightButton.RepeatClick then StartRepeatClick;
     if FRightButton.Kind = scgpebDropDown then
       if Assigned(FOnRightButtonClick) then FOnRightButtonClick(Self);
    if FRightButton.DropDownMenu <> nil then
    begin
      FMenuTracking := True;
      P := ClientToScreen(Point(0, Self.Height));
      FRightButton.DropDownMenu.Popup(P.X, P.Y);
      FRightButton.Down := False;
     // Invalidate;
      SetTimer(Handle, 22, 10, nil);
    end;
  end
  else
    inherited;
end;

procedure TscGPCustomEdit.WMNCLBUTTONDOWN;
var
  P: TPoint;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  if (FHintComponent <> nil) then
    FHintComponent.HideHint;

  if FLeftButton.Visible and FLeftButton.Enabled and (Message.HitTest = HTEDITBUTTONL) and not FMenuTracking then
  begin
    FLeftButton.Down := True;
    if not Focused then SetFocus;
    Invalidate;
    if LeftButton.RepeatClick then StartRepeatClick;
    if FLeftButton.FDropDown then
       if Assigned(FOnLeftButtonClick) then FOnLeftButtonClick(Self);
    if (FLeftButton.DropDownMenu <> nil) then
    begin
      FMenuTracking := True;
      P := ClientToScreen(Point(0, Self.Height));
      FLeftButton.DropDownMenu.Popup(P.X, P.Y);
      FLeftButton.Down := False;
      //Invalidate;
      SetTimer(Handle, 22, 10, nil);
    end;
  end
  else
  if FRightButton.Visible and FRightButton.Enabled and (Message.HitTest = HTEDITBUTTONR) and not FMenuTracking then
  begin
    FRightButton.Down := True;
    if not Focused then SetFocus;
    Invalidate;

    if RightButton.RepeatClick then StartRepeatClick;
     if FRightButton.Kind = scgpebDropDown then
       if Assigned(FOnRightButtonClick) then FOnRightButtonClick(Self);
    if FRightButton.DropDownMenu <> nil then
    begin
      FMenuTracking := True;
      P := ClientToScreen(Point(0, Self.Height));
      FRightButton.DropDownMenu.Popup(P.X, P.Y);
      FRightButton.Down := False;
      //Invalidate;
      SetTimer(Handle, 22, 10, nil);
    end;
  end
  else
    inherited;
end;

procedure TscGPCustomEdit.WMNCLBUTTONUP;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;
  if FLeftButton.Visible and FLeftButton.Enabled and (Message.HitTest = HTEDITBUTTONL) and LeftButton.Down then
  begin
   LeftButton.Down := False;
   Invalidate;
   if LeftButton.RepeatClick then StopRepeatClick;
   if not (LeftButton.Kind = scgpebDropDown) then
    if Assigned(FOnLeftButtonClick) then FOnLeftButtonClick(Self);
   if FLeftButton.Kind = scgpebClear then
     Text := '';
  end
  else
  if FRightButton.Visible and FRightButton.Enabled and (Message.HitTest = HTEDITBUTTONR) and RightButton.Down then
  begin
    RightButton.Down := False;
    Invalidate;

    if RightButton.RepeatClick then StopRepeatClick;
    if not (RightButton.Kind = scgpebDropDown) then
      if Assigned(FOnRightButtonClick) then FOnRightButtonClick(Self);

    if FRightButton.Kind = scgpebClear then
     Text := '';  
  end
  else
    inherited;
end;

procedure TscGPCustomEdit.WMTimer(var Message: TWMTimer);
var
  P: TPoint;
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    GetCursorPos(P);
    if WindowFromPoint(P) <> Handle then
    begin
      KillTimer(Handle, 1);
      if FMouseIn then
        EditMouseLeave;
    end;
  end
  else
  if Message.TimerID = 100 then
  begin
    KillTimer(Handle, 100);
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPCustomEdit.CMMouseLeave(var Message: TMessage);
var
  P: TPoint;
begin
  inherited;
  GetCursorPos(P);
  if WindowFromPoint(P) <> Handle then
    EditMouseLeave;
end;

procedure TscGPCustomEdit.EditMouseEnter;
begin
  FMouseIn := True;
  DoPaint;
  SetTimer(Handle, 1, 200, nil);
end;

procedure TscGPCustomEdit.EditMouseLeave;
begin
  FMouseIn := False;

  if (FHintComponent <> nil) then
     FHintComponent.HideHint;

  if FLeftButton.Visible and LeftButton.MouseIn then
  begin
    LeftButton.MouseIn := False;
    if LeftButton.Down and (LeftButton.DropDownMenu = nil) then
      LeftButton.Down := False;
  end;
  if FRightButton.Visible and RightButton.MouseIn then
  begin
    RightButton.MouseIn := False;
    if RightButton.Down and (RightButton.DropDownMenu = nil) then
      RightButton.Down := False;
  end;
  DoPaint;
end;

constructor TscGPNumericEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCurrencyString := '';
  FDisplayFormat := '';
  FSupportUpdownKeys := False;
  FSupportMouseWheel := False;
  FIncrement := 1;
  FMinValue := 0;
  FMaxValue := 0;
  FValue := 0;
  StopCheck := True;
  FromEdit := False;
  Text := '0';
  StopCheck := False;
  FDecimal := 2;
end;

destructor TscGPNumericEdit.Destroy;
begin
  inherited;
end;

procedure TscGPNumericEdit.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FSupportMouseWheel then
    if TWMMOUSEWHEEL(Message).WheelDelta > 0 then
      Value := FValue + FIncrement
    else
      Value := FValue - FIncrement;
end;

function TscGPNumericEdit.IsCustomDraw(ADC: HDC): Boolean;
var
  R: TRect;
  S: String;
  FCanvas: TControlCanvas;
  DC: HDC;
  PS: TPaintStruct;
begin
  if (FDisplayType = scedtNumeric) and (FDisplayFormat = '') then
    Result := inherited IsCustomDraw(ADC)
  else
    begin
      Result := True;
      R := GetTextRect;
      if FDisplayFormat <> '' then
        S := FormatFloat(FDisplayFormat, Value)
      else
      if FCurrencyString <> '' then
        S := FormatFloat(',0.00' + ' ' + FCurrencyString, Value)
      else
        S := FormatFloat(',0.00', Value);
      DC := ADC;
      if DC = 0 then DC := BeginPaint(Handle, PS);
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
      FCanvas.Handle := DC;
      try
        with FCanvas do
        begin
          Font := Self.Font;
          if not Enabled then
          begin
            if (seFont in StyleElements) and IsCustomStyle then
              Font.Color := ColorToRGB(GetEditTextColor(scsDisabled))
            else
              Font.Color := clGrayText;
          end
          else
          begin
            if (seFont in StyleElements) and IsCustomStyle then
             Font.Color := GetEditTextColor(scsNormal);
          end;
          Brush.Style := bsClear;
        end;
        if BorderStyle <> bsNone then
          Inc(R.Top);
        R.Bottom := R.Top + FCanvas.TextHeight(S);
        DrawTextAlignment(FCanvas, S, R, taRightJustify);
      finally
        FCanvas.Handle := 0;
        FCanvas.Free;
        if ADC = 0 then EndPaint(Handle, PS)
      end;
    end;
end;

procedure TscGPNumericEdit.SetCurrencyString(Value: String);
begin
  if FCurrencyString <> Value then
  begin
    FCurrencyString := Value;
    Invalidate;
  end;
end;

procedure TscGPNumericEdit.SetDisplayFormat(Value: String);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Invalidate;
  end;
end;

procedure TscGPNumericEdit.SetDisplayType(Value: TscNumEditDisplayType);
begin
  if FDisplayType <> Value then
  begin
    FDisplayType := Value;
    if FDisplayType = scedtCurrency then
    begin
      FDecimal := 2;
      ValueType := scvtFloat;
    end;
    Invalidate;
  end;
end;

procedure TscGPNumericEdit.WMKILLFOCUS(var Message: TMessage);
begin
  inherited;
  StopCheck := True;
  if ValueType = scvtFloat
  then Text := FloatToStrF(FValue, ffFixed, 15, FDecimal)
  else Text := IntToStr(Round(FValue));
  StopCheck := False;
  if (ValueType = scvtFloat) and (FValue <> StrToFloat(Text)) then
  begin
    FValue := StrToFloat(Text);
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TscGPNumericEdit.GetValueAsInt: Integer;
begin
  Result := Round(FValue);
end;

procedure TscGPNumericEdit.SetValueType(NewType: TscValueType);
begin
  if FValueType <> NewType then
  begin
    FValueType := NewType;
  end;
end;

procedure TscGPNumericEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then
  begin
    FDecimal := NewValue;
  end;
end;

function TscGPNumericEdit.CheckValue;
begin
  Result := NewValue;

  if ValueType = scvtInteger then
  begin
    if Result > MaxInt then
      Result := MaxInt
    else
    if Result < -MaxInt then
      Result := -MaxInt;
  end;

  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
    Result := FMinValue
    else if NewValue > FMaxValue then
    Result := FMaxValue;
  end
  else
  if FMaxValue > 0 then
    Result := FMaxValue;
end;

procedure TscGPNumericEdit.SetMinValue;
begin
  FMinValue := AValue;
end;

procedure TscGPNumericEdit.SetMaxValue;
begin
  FMaxValue := AValue;
end;

function TscGPNumericEdit.IsNumText;

function GetMinus: Boolean;
var
  i: Integer;
  S: String;
begin
  S := AText;
  i := Pos('-', S);
  if i > 1 then
    Result := False
  else
    begin
      Delete(S, i, 1);
      Result := Pos('-', S) = 0;
    end;
end;

function GetP: Boolean;
var
  i: Integer;
  S: String;
begin
  S := AText;
  i := Pos(FormatSettings.DecimalSeparator, S);
  if i = 1
  then
    Result := False
  else
    begin
      Delete(S, i, 1);
      Result := Pos(FormatSettings.DecimalSeparator, S) = 0;
    end;
end;

const
  EditChars = '01234567890-';
var
  i: Integer;
  S: String;
begin
  S := EditChars;
  Result := True;
  if ValueType = scvtFloat
  then
    S := S + FormatSettings.DecimalSeparator;
  if (Text = '') or (Text = '-')
  then
    begin
      Result := False;
      Exit;
    end;

  for i := 1 to Length(Text) do
  begin
    if Pos(Text[i], S) = 0
    then
      begin
        Result := False;
        Break;
      end;
  end;

  Result := Result and GetMinus;

  if ValueType = scvtFloat
  then
    Result := Result and GetP;

end;

procedure TscGPNumericEdit.Change;
var
  NewValue, TmpValue: Double;

function CheckInput: Boolean;
begin
  if (NewValue < 0) and (TmpValue < 0)
  then
    Result := NewValue > TmpValue
  else
    Result := NewValue < TmpValue;

  if not Result and ( ((FMinValue > 0) and (TmpValue < 0))
    or ((FMaxValue < 0) and (TmpValue > 0)))
  then
    Result := True;
end;

begin
  if FromEdit then Exit;
  if not StopCheck and IsNumText(Text)
  then
    begin
      if ValueType = scvtFloat
      then
        TmpValue := StrToFloat(Text)
      else
      begin
        TmpValue := StrToFloat(Text);
        if TmpValue > MaxInt then
        begin
          TmpValue := MaxInt;
          FromEdit := True;
          Text := IntToStr(MaxInt);
          FromEdit := False;
        end
        else
        if TmpValue < -MaxInt then
        begin
          TmpValue := -MaxInt;
          FromEdit := True;
          Text := IntToStr(-MaxInt);
          FromEdit := False;
        end
        else
          TmpValue := StrToInt(Text);
      end;
      NewValue := CheckValue(TmpValue);
      if NewValue <> FValue
      then
        begin
          FValue := NewValue;
        end;
      if CheckInput
      then
        begin
          FromEdit := True;
          if ValueType = scvtFloat
          then Text := FloatToStrF(NewValue, ffFixed, 15, FDecimal)
          else Text := IntToStr(Round(FValue));
          FromEdit := False;
        end;
    end;
  inherited;
end;

procedure TscGPNumericEdit.SetValue;
begin
  FValue := CheckValue(AValue);
  StopCheck := True;
  if ValueType = scvtFloat
  then
    Text := FloatToStrF(CheckValue(AValue), ffFixed, 15, FDecimal)
  else
    Text := IntToStr(Round(CheckValue(AValue)));
  StopCheck := False;
end;

procedure TscGPNumericEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FSupportUpDownKeys
  then
    begin
      if Key = VK_UP
      then
        Value := Value + FIncrement
      else
      if Key = VK_DOWN
      then
        Value := Value - FIncrement;
     end
end;

procedure TscGPNumericEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key)
  then
    begin
      Key := #0;
      MessageBeep(0)
    end;
  inherited KeyPress(Key);
end;

function TscGPNumericEdit.IsValidChar(Key: Char): Boolean;
var
  S: String;
  DecPos: Integer;
begin
  if ValueType = scvtFloat
  then
    Result := CharInSet(Key, [FormatSettings.DecimalSeparator, '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)))
  else
    Result := CharInSet(Key, ['-', '0'..'9']) or
     ((Key < #32) and (Key <> Chr(VK_RETURN)));

  if (Key = FormatSettings.DecimalSeparator) and (Pos(FormatSettings.DecimalSeparator, Text) <> 0)  then
    Result := False
  else
  if (Key = '-') and (SelStart = 0) and (SelLength > 0) then
     Result := True
  else
  if (Key = '-') and (SelStart <> 0)  then
     Result := False
  else
  if (Key = '-') and (Pos('-', Text) <> 0) then
    Result := False;

  if ReadOnly and Result and ((Key >= #32) or
     (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)))
  then
    Result := False;

  if Result and (Key <> Char(VK_BACK)) and (Key <> Char(VK_DELETE)) then
  begin
    S := EditText;
    DecPos := Pos(FormatSettings.DecimalSeparator, S);
    if (DecPos > 0) and (SelText = '') and (SelStart >= DecPos) and (Length(S) - DecPos >= Decimal) then
      Exit(False);
  end;
end;

constructor TscGPComboEdit.Create;
begin
  inherited Create(AOwner);
  FMouseWheelSupport := True;
  FIsModified := False;
  FDropDownPosition := scdpRight;
  RightButton.Visible := True;
  RightButton.Kind := scgpebDropDown;
  FUseFilter := False;
  FStopUseFilter := False;
  OnRightButtonClick := RightButtonClick;
  FListBox := TscGPPopupListBox.Create(Self);
  FListBox.ItemHeight := 16;
  FListBoxWallpaperIndex := -1;
  FListBoxWallpapers := nil;
  FShowItemTitle := True;
  FShowItemImage := True;
  FShowItemText := True;
  TabStop := True;
  FLBDown := False;
  WasInLB := False;
  TimerMode := 0;
  Width := 120;
  Height := 41;
  FListBoxWindowProc := FlistBox.WindowProc;
  FlistBox.WindowProc := ListBoxWindowProcHook;
  FListBox.Visible := False;
  FlistBox.MouseMoveChangeIndex := True;
  if not (csDesigning in ComponentState) then
    FlistBox.Parent := Self;
  FLBDown := False;
  FLastTime := 0;
  FListBoxWidth := 0;
  FListBoxHeight := 0;
  FDropDownCount := 7;
end;

function TscGPComboEdit.IsModified: Boolean;
begin
  Result := Modified or FIsModified;
end;

procedure TscGPComboEdit.BeginUpdateItems;
begin
  FListBox.BeginUpdateItems;
end;

procedure TscGPComboEdit.EndUpdateItems;
begin
  FListBox.EndUpdateItems;
end;

procedure TscGPComboEdit.Add(const Item: String);
begin
  FListBox.Add(Item);
end;

procedure TscGPComboEdit.Add(Items: TStrings);
begin
  FListBox.Add(Items);
end;

procedure TscGPComboEdit.Delete(Index: Integer);
begin
  FListBox.Delete(Index);
end;

function TscGPComboEdit.IndexOf(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FListBox.IndexOfCaption(S, AStartOff);
end;

procedure TscGPComboEdit.ListBoxWindowProcHook(var Message: TMessage);
var
  FOld: Boolean;
begin
  FOld := True;
  case Message.Msg of
      WM_LBUTTONDOWN:
       begin
         FOLd := False;
         FLBDown := True;
         WasInLB := True;
         if FListBox.Visible then
            FListBox.MouseDown(mbLeft, [],
              TWMLButtonDown(Message).XPos, TWMLButtonDown(Message).YPos);
         SetCapture(Self.Handle);
       end;
     WM_LBUTTONUP,
     WM_RBUTTONDOWN, WM_RBUTTONUP,
     WM_MBUTTONDOWN, WM_MBUTTONUP:
       begin
         FOLd := False;
       end;
     WM_MOUSEACTIVATE:
      begin
        Message.Result := MA_NOACTIVATE;
      end;
  end;
  if FOld then FListBoxWindowProc(Message);
end;

procedure TscGPComboEdit.KeyPress;
begin
  inherited;
end;

destructor TscGPComboEdit.Destroy;
begin
  FlistBox.Free;
  FlistBox := nil;
  inherited;
end;

function TscGPComboEdit.GetListBoxItemWordWrap: Boolean;
begin
  Result := FListBox.ItemWordWrap;
end;

procedure TscGPComboEdit.SetListBoxItemWordWrap(Value: Boolean);
begin
  FListBox.ItemWordWrap := Value;
end;

function TscGPComboEdit.GetListBoxItemShowEllipsis: Boolean;
begin
  Result := FListBox.ItemShowEllipsis;
end;

procedure TscGPComboEdit.SetListBoxItemShowEllipsis(Value: Boolean);
begin
  FListBox.ItemShowEllipsis := Value;
end;

procedure TscGPComboEdit.CMEnabledChanged;
begin
  inherited;
  Invalidate;
end;

procedure TscGPComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
  if (Operation = opRemove) and (AComponent = FListBoxWallpapers) then
   FListBoxWallpapers := nil;
end;

function TscGPComboEdit.GetImages: TCustomImageList;
begin
  if FListBox <> nil
  then
    Result := FListBox.Images
   else
    Result := nil;
end;

procedure TscGPComboEdit.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (Message.wParam <> FListBox.Handle)
  then
    CloseUp(False);
end;

procedure TscGPComboEdit.CMCancelMode;
begin
  inherited;
  if (Message.Sender = nil) or
     (Message.Sender <> Self.FListBox)
  then
    CloseUp(False);
end;

procedure TscGPComboEdit.CheckButtonClick;
begin
  CloseUp(True);
end;

procedure TscGPComboEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  case Msg.CharCode of
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:  Msg.Result := 1;
  end;
end;

procedure TscGPComboEdit.KeyDown;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      if ssAlt in Shift
      then
        begin
          if FListBox.Visible then CloseUp(False);
        end
      else
        ComboKeyUp(True);
    VK_DOWN:
      if ssAlt in Shift
      then
        begin
          if not FListBox.Visible then DropDown;
        end
      else
        ComboKeyDown(True);

    VK_NEXT: ComboPageDown(True);
    VK_PRIOR: ComboPageUp(True);
    VK_ESCAPE: if FListBox.Visible then CloseUp(False);
    VK_RETURN: if FListBox.Visible then CloseUp(True);
  end;
end;

procedure TscGPComboEdit.MouseDown;
begin
  inherited;
  if FListBox.Visible then CloseUp(False);
end;

procedure TscGPComboEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
  P: TPoint;
begin
  if FListBox.Visible then
  begin
    GetCursorPos(P);
    P := FListBox.ScreenToClient(P);
  end
  else
    P := Point(X, Y);
  if FListBox.Visible and
     (FListBox.IsScrollBarCaptured or FListBox.MouseInScrollBar(P.X, P.Y)) then
  begin
    WasInLB := False;
    ReleaseCapture;
    FLBDown := False;
    FListBox.MouseUp(Button, [], P.X, P.Y);
    FListBox.FMouseDown := False;
  end
  else
  if FLBDown and WasInLB
  then
    begin
      ReleaseCapture;
      FLBDown := False;
      GetCursorPos(P);
      if WindowFromPoint(P) = FListBox.Handle
      then
        CloseUp(True)
      else
        CloseUp(False);
    end
  else
     FLBDown := False;
  inherited;
end;

procedure TscGPComboEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FListBox.Visible then ProcessListBox;
end;

procedure TscGPComboEdit.WMMOUSEWHEEL;
begin
  inherited;

  if not FMouseWheelSupport then Exit;

  if TWMMOUSEWHEEL(Message).WheelDelta > 0
  then
    ComboKeyUp(not FListBox.Visible)
  else
    ComboKeyDown(not FListBox.Visible);
end;

procedure TscGPComboEdit.WMSETFOCUS;
begin
  inherited;
  Invalidate;
end;

procedure TscGPComboEdit.WMKILLFOCUS;
begin
  inherited;
  if FListBox.Visible  then CloseUp(False);
  Invalidate;
end;

function TscGPComboEdit.GetItemIndex;
begin
  Result := FListBox.ItemIndex;
end;

procedure TscGPComboEdit.SetItemIndex;
begin
  FListBox.ItemIndex := Value;
  FOldItemIndex := FListBox.ItemIndex;
  Invalidate;
  if not (csLoading in ComponentState) then
  begin
    if (FListBox.ItemIndex >= 0) and
       (FListBox.ItemIndex < FListbox.Items.Count) then
      Text := FListBox.Items[FListBox.ItemIndex].Caption;
  end;
end;

function TscGPComboEdit.IsPopupVisible: Boolean;
begin
  Result := FListBox.Visible;
end;

procedure TscGPComboEdit.StartTimer;
begin
  KillTimer(Handle, 2);
  SetTimer(Handle, 2, 25, nil);
end;

procedure TscGPComboEdit.StopTimer;
begin
  KillTimer(Handle, 2);
  TimerMode := 0;
end;

procedure TscGPComboEdit.WMTimer;
begin
  inherited;
  case TimerMode of
    1: FListBox.FindUp;
    2: FListBox.FindDown;
  end;
end;

procedure TscGPComboEdit.ProcessListBox;
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := FListBox.ScreenToClient(P);
   if (P.Y < 0) and (FListBox.FVertScrollBar <> nil) and FListBox.FVertScrollBar.Visible and WasInLB
     and not ListBox.IsScrollBarCaptured and not ListBox.MouseInScrollBar(P.X, P.Y)
  then
    begin
      if (TimerMode <> 1)
      then
        begin
          TimerMode := 1;
          StartTimer;
        end;
    end
  else
  if (P.Y > FListBox.Height) and (FListBox.FVertScrollBar <> nil) and FListBox.FVertScrollBar.Visible and WasInLB
    and not ListBox.IsScrollBarCaptured and not ListBox.MouseInScrollBar(P.X, P.Y)
  then
    begin
      if (TimerMode <> 2)
      then
        begin
          TimerMode := 2;
          StartTimer;
        end
    end
  else
    if (P.Y >= 0) and (P.Y <= FListBox.Height)
    then
      begin
        if TimerMode <> 0 then StopTimer;
        FListBox.MouseMove([], P.X, P.Y);
        WasInLB := True;
        if not FLBDown
        then
          begin
            FLBDown := True;
            WasInLB := False;
          end;
      end;
end;

procedure TscGPComboEdit.SetDropDownCount(Value: Integer);
begin
  if Value >= 0
  then
    FDropDownCount := Value;
end;

procedure TscGPComboEdit.SetItems;
begin
  FListBox.Items.Assign(Value);
end;

function TscGPComboEdit.GetItems;
begin
  Result := FListBox.Items;
end;

procedure TscGPComboEdit.CloseUp;
begin
  SC_UnHookMouseMessages;
  if TimerMode <> 0 then StopTimer;
  if not FListBox.Visible then Exit;
  FListBox.StopScrollTimer;
  FListBox.Hide;
  if (FListBox.ItemIndex >= 0) and
     (FListBox.ItemIndex < FListBox.Items.Count) and Value
  then
    begin
      FIsModified := True;
      Text := Self.Items[ItemIndex].Caption;
     end
  else
    FListBox.ItemIndex := FOldItemIndex;
  Invalidate;
  if Value then
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  FIsModified := False;
end;

procedure TscGPComboEdit.RightButtonClick(Sender: TObject);
begin
  if not FListBox.Visible then DropDown else CloseUp(False);
end;

procedure TscGPComboEdit.Change;
begin
  inherited;
  if IsPopupVisible and FUseFilter and not FStopUseFilter then
    FListBox.Filter := Self.Text
  else
  if IsPopupVisible and not FStopUseFilter then
  begin
    FListBox.ItemIndex := FListBox.IndexOfCaption(Text, True);
    if FListBox.ItemIndex = -1 then
      FListBox.ScrollToItem(0);
  end;
end;

procedure TscGPComboEdit.DropDown;
var
  P: TPoint;
  WorkArea: TRect;
  I: Integer;
begin
  if Items.Count = 0 then Exit;
  WasInLB := False;
  if TimerMode <> 0 then StopTimer;
  if Assigned(FOnDropDown) then FOnDropDown(Self);

  if FListBoxWidth = 0
  then
    FListBox.Width := Width
  else
    FListBox.Width := FListBoxWidth;

  FListBox.Font.Assign(Font);
  FListBox.Color := Color;
  FListBox.BidiMode := Self.BiDiMode;
  FListBox.StyleElements := StyleElements;

  if FDropDownPosition = scdpRight then
    P := Point(Left + Width - FListBox.Width, Top + Height)
  else
    P := Point(Left, Top + Height);

  P := Parent.ClientToScreen(P);

  WorkArea := Screen.MonitorFromWindow(Handle).WorkAreaRect;

  FOldItemIndex := FListBox.ItemIndex;

  Invalidate;
  if FListBoxHeight > 0  then
    FListBox.Height := FListBoxHeight
  else
  if FDropDownCount > 0 then
    FListBox.Height := FListBox.CalcHeight(FDropDownCount);

  if P.Y + FListBox.Height > WorkArea.Bottom
  then
    P.Y := P.Y - Height - FListBox.Height;

  // set index
  if Text <> '' then
  begin
    I := FListBox.IndexOfCaption(Text);
    if (I <> -1) then
    begin
      if (I <> FListBox.ItemIndex) then
        FListBox.ItemIndex := I
      else
        FListBox.ScrollToItem(I);
    end
    else
    if (FListBox.ItemIndex <> -1) then
    begin
      FListBox.ItemIndex := -1;
      FListBox.ScrollToItem(0);
    end;
  end
  else
  begin
    FListBox.ItemIndex := -1;
    FListBox.ScrollToItem(0);
  end;
  //
  if FUseFilter then
    FListBox.Filter := Self.Text;
  //
  FListBox.Show(P);
  SC_HookMouseMessages(Self);
end;

procedure TscGPComboEdit.ComboPageUp;
begin
  FListBox.FindPageUp;
  FStopUseFilter := True;
  if AChange then
  begin
    FIsModified := True;
    ItemIndex := FListBox.ItemIndex;
    FIsModified := False;
  end;
  FStopUseFilter := False;
end;

procedure TscGPComboEdit.ComboPageDown(AChange: Boolean);
begin
  FListBox.FindPageDown;
  FStopUseFilter := True;
  if AChange then
  begin
    FIsModified := True;
    ItemIndex := FListBox.ItemIndex;
    FIsModified := False;
  end;
  FStopUseFilter := False;
end;

procedure TscGPComboEdit.ComboKeyUp;
begin
  FListBox.FindUp;
  FStopUseFilter := True;
  if AChange then
  begin
    FIsModified := True;
    ItemIndex := FListBox.ItemIndex;
    FIsModified := False;
  end;
  FStopUseFilter := False;
end;

procedure TscGPComboEdit.ComboKeyDown;
begin
  FListBox.FindDown;
  FStopUseFilter := True;
  if AChange then
  begin
    FIsModified := True;
    ItemIndex := FListBox.ItemIndex;
    FIsModified := False;
  end;
  FStopUseFilter := False;
end;

procedure TscGPComboEdit.SetImages(Value: TCustomImageList);
begin
  if FListBox.Images <> Value then
  begin
    FListBox.Images := Value;
    Invalidate;
  end;
end;

function TscGPComboEdit.GetListBoxHeaderFont: TFont;
begin
  Result := FListBox.HeaderFont;
end;

procedure TscGPComboEdit.SetListBoxHeaderFont(Value: TFont);
begin
  FListBox.HeaderFont.Assign(Value);
end;

function TscGPComboEdit.GetListBoxDetailFont: TFont;
begin
  Result := FListBox.DetailFont;
end;

procedure TscGPComboEdit.SetListBoxDetailFont(Value: TFont);
begin
  FListBox.DetailFont.Assign(Value);
end;

function TscGPComboEdit.GetListBoxItemSpacing: Integer;
begin
  Result := FListBox.ItemSpacing;
end;

function TscGPComboEdit.GetListBoxItemMargin: Integer;
begin
  Result := FListBox.ItemMargin;
end;

function TscGPComboEdit.GetListBoxScrollBarOptions: TscGPControlScrollBarOptions;
begin
  Result := FListBox.ScrollBarOptions;
end;

procedure TscGPComboEdit.SetListBoxScrollBarOptions(Value: TscGPControlScrollBarOptions);
begin
  FListBox.ScrollBarOptions.Assign(Value);
end;

function TscGPComboEdit.GetListBoxOptions: TscGPScrollingControlOptions;
begin
  Result := FListBox.Options;
end;

procedure TscGPComboEdit.SetListBoxOptions(Value: TscGPScrollingControlOptions);
begin
   FListBox.Options.Assign(Value);
end;

function TscGPComboEdit.GetListBoxHeaderOptions: TscGPHeaderOptions;
begin
  Result := FListBox.HeaderOptions;
end;

procedure TscGPComboEdit.SetListBoxHeaderOptions(Value: TscGPHeaderOptions);
begin
  FListBox.HeaderOptions.Assign(Value);
end;

function TscGPComboEdit.GetListBoxSelectionOptions: TscGPSelectionOptions;
begin
  Result := FListBox.SelectionOptions;
end;

procedure TscGPComboEdit.SetListBoxSelectionOptions(Value: TscGPSelectionOptions);
begin
  FListBox.SelectionOptions.Assign(Value);
end;

procedure TscGPComboEdit.SetListBoxItemMargin(Value: Integer);
begin
  FListBox.ItemMargin := Value;
end;

procedure TscGPComboEdit.SetListBoxItemSpacing(Value: Integer);
begin
  FListBox.ItemSpacing := Value;
end;

function TscGPComboEdit.GetListBoxIndentMargin: Integer;
begin
  Result := FListBox.IndentMargin;
end;

procedure TscGPComboEdit.SetListBoxIndentMargin(Value: Integer);
begin
  FListBox.IndentMargin := Value;
end;

function TscGPComboEdit.GetListBoxLineColorAlpha: TColor;
begin
  Result := FListBox.LineColorAlpha;
end;

procedure TscGPComboEdit.SetListBoxLineColorAlpha(Value: TColor);
begin
  FListBox.LineColorAlpha := Value;
end;

function TscGPComboEdit.GetListBoxLineColor: TColor;
begin
  Result := FListBox.LineColor;
end;

procedure TscGPComboEdit.SetListBoxLineColor(Value: TColor);
begin
  FListBox.LineColor := Value;
end;

function TscGPComboEdit.GetListBoxShowItemDetails: Boolean;
begin
  Result := FListBox.ShowItemDetails;
end;

procedure TscGPComboEdit.SetListBoxShowItemDetails(Value: Boolean);
begin
  FListBox.ShowItemDetails := Value;
end;

function TscGPComboEdit.GetListBoxShowLines: Boolean;
begin
  Result := FListBox.ShowLines;
end;

procedure TscGPComboEdit.SetListBoxShowLines(Value: Boolean);
begin
  FListBox.ShowLines := Value;
end;

function TscGPComboEdit.GetListBoxItemHeight: Integer;
begin
  Result := FlistBox.ItemHeight;
end;

procedure TscGPComboEdit.SetListBoxItemHeight(Value: Integer);
begin
  FlistBox.ItemHeight := Value;
end;

function TscGPComboEdit.GetListBoxHeaderHeight: Integer;
begin
  Result := FListBox.HeaderHeight;
end;

procedure TscGPComboEdit.SetListBoxHeaderHeight(Value: Integer);
begin
  FListBox.HeaderHeight := Value;
end;

constructor TscGPSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  FMouseWheelSupport := True;
  FIsModified := False;
  FIncrement := 1;
  FMinValue := 0;
  FMaxValue := 0;
  FValue := 0;
  StopCheck := True;
  FromEdit := False;
  Text := '0';
  StopCheck := False;
  Width := 120;
  Height := 20;
  FDecimal := 2;
  ShowUpDownButtons;
end;

destructor TscGPSpinEdit.Destroy;
begin
  inherited;
end;

function  TscGPSpinEdit.IsModified: Boolean;
begin
  Result := Modified or FIsModified;
end;

function TscGPSpinEdit.GetArrowGlyphColor: TColor;
begin
  Result := FLeftButton.GlyphColor;
end;

function TscGPSpinEdit.GetArrowGlyphColorAlpha: Byte;
begin
  Result := FLeftButton.GlyphColorAlpha;
end;

function TscGPSpinEdit.GetArrowGlyphColorHotAlpha: Byte;
begin
  Result := FLeftButton.GlyphColorHotAlpha
end;

function TscGPSpinEdit.GetArrowGlyphColorPressedAlpha: Byte;
begin
  Result := FLeftButton.GlyphColorPressedAlpha
end;

procedure TscGPSpinEdit.SetArrowGlyphColor(Value: TColor);
begin
  FLeftButton.GlyphColor := Value;
  FRightButton.GlyphColor := Value;
end;

procedure TscGPSpinEdit.SetArrowGlyphColorAlpha(Value: Byte);
begin
  FLeftButton.GlyphColorAlpha := Value;
  FRightButton.GlyphColorAlpha := Value;
end;

procedure TscGPSpinEdit.SetArrowGlyphColorHotAlpha(Value: Byte);
begin
  FLeftButton.GlyphColorHotAlpha := Value;
  FRightButton.GlyphColorHotAlpha := Value;
end;

procedure TscGPSpinEdit.SetArrowGlyphColorPressedAlpha(Value: Byte);
begin
  FLeftButton.GlyphColorPressedAlpha := Value;
  FRightButton.GlyphColorPressedAlpha := Value;
end;

procedure TscGPSpinEdit.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FMouseWheelSupport then
  begin
    FIsModified := True;
    if TWMMOUSEWHEEL(Message).WheelDelta > 0 then
      Value := Value + FIncrement
    else
      Value := Value - FIncrement;
    FIsModified := False;
  end;
end;

procedure TscGPSpinEdit.SetUpDownKind(Value: TscUpDownKind);
begin
  if FUpDownKind <> Value then
  begin
    FUpDownKind := Value;
    ShowUpDownButtons;
    AdjustTextRect;
  end;
end;

procedure TscGPSpinEdit.UpButtonClick(Sender: TObject);
begin
  FIsModified := True;
  Value := Value + FIncrement;
  FIsModified := False;
end;

procedure TscGPSpinEdit.DownButtonClick(Sender: TObject);
begin
  FIsModified := True;
  Value := Value - FIncrement;
  FIsModified := False;
end;

function TscGPSpinEdit.GetTextRect: TRect;
var
  R, R1: TRect;
  W: Integer;
  Buffer: TBitmap;
begin
  if (not Assigned(LeftButton) or not Assigned(RightButton)) or
     (not LeftButton.Visible and not RightButton.Visible) then
  begin
    Result := inherited GetTextRect;
    Exit;
  end;
  if FUpDownKind = scupkLeftRight then
  begin
    Result := inherited GetTextRect;
    if BidiMode = bdRightToLeft then
    begin
      if LeftButton.FKind <> scgpebRightArrow then
        LeftButton.FKind := scgpebRightArrow;
      if RightButton.FKind <> scgpebLeftArrow then
        RightButton.FKind := scgpebLeftArrow;
    end
    else
    begin
      if LeftButton.FKind <> scgpebLeftArrow then
        LeftButton.FKind := scgpebLeftArrow;
      if RightButton.FKind <> scgpebRightArrow then
        RightButton.FKind := scgpebRightArrow;
    end;
    Exit;
  end;
  // for updown
  Result := ClientRect;
  R := Result;
  LeftButton.ButtonRect := Rect(0, 0, 0, 0);
  RightButton.ButtonRect := Rect(0, 0, 0, 0);
  Result := ClientRect;
  Inc(Result.Left, FContentMarginLeft);
  Inc(Result.Top, FContentMarginTop);
  Dec(Result.Right, FContentMarginRight);
  Dec(Result.Bottom, FContentMarginBottom);
  if not Assigned(FLeftButton) or not Assigned(FRightButton) then Exit;
  R := Result;
  R1 := R;
  if FOptions.ScaleFrameWidth then
    W := FOptions.FFrameWidth
  else
    W := Round(FOptions.FrameWidth * FScaleFactor);
  R1.Top := W * 2;
  R1.Bottom := Height - W * 2;
  FLeftButton.ButtonRect := Rect(0, 0, 0, 0);
  FRightButton.ButtonRect := Rect(0, 0, 0, 0);
  FGlyphSize := R.Height - R.Height div 2;
  W := R1.Height - R1.Height div 2;
  if BidiMode = bdRightToLeft then
  begin
   if FLeftButton.Visible then
   begin
     FLeftButton.ButtonRect := Rect(R.Left, R1.Top, R.Left + W, R1.Top + R1.Height div 2);
   end;
   if FRightButton.Visible then
   begin
     FRightButton.ButtonRect := Rect(R.Left, R1.Bottom - R1.Height div 2, R.Left + W, R1.Bottom);
     Inc(R.Left, FRightButton.ButtonRect.Width + Round(4 * FScaleFactor));
   end;
  end
  else
  begin
    if FLeftButton.Visible then
    begin
      FLeftButton.ButtonRect := Rect(R.Right - W, R1.Top, R.Right, R1.Top + R1.Height div 2);
    end;
    if FRightButton.Visible then
    begin
      FRightButton.ButtonRect := Rect(R.Right - W, R1.Bottom - R1.Height div 2, R.Right, R1.Bottom);
      Dec(R.Right, RightButton.ButtonRect.Width + Round(4 * FScaleFactor));
    end;
  end;

  Buffer := TBitmap.Create;
  try
    Buffer.Canvas.Font := Self.Font;
    if R.Width < Buffer.Canvas.TextWidth('W') then
      R.Width := Buffer.Canvas.TextWidth('W');
    if R.Height < Buffer.Canvas.TextHeight('W') then
     R.Height := Buffer.Canvas.TextHeight('W');
  finally
    Buffer.Free;
  end;

  Result := R;
end;

procedure TscGPSpinEdit.ShowUpDownButtons;
begin
  if FUpDownKind = scupkLeftRight then
    LeftButton.Kind := scgpebLeftArrow
  else
    LeftButton.Kind := scgpebUpArrow;
  LeftButton.Visible := True;
  LeftButton.RepeatClick := True;
  LeftButton.RepeatClickInterval := 100;
  if FUpDownKind = scupkLeftRight then
    RightButton.Kind := scgpebRightArrow
  else
    RightButton.Kind := scgpebDownArrow;
  RightButton.Visible := True;
  RightButton.RepeatClick := True;
  RightButton.RepeatClickInterval := 100;
  if FUpDownKind = scupkLeftRight then
  begin
    OnRightButtonClick := UpButtonClick;
    OnLeftButtonClick := DownButtonClick;
  end
  else
  begin
    OnRightButtonClick := DownButtonClick;
    OnLeftButtonClick := UpButtonClick;
  end;
end;

function TscGPSpinEdit.IsCustomDraw(ADC: HDC): Boolean;
var
  R: TRect;
  S: String;
  FCanvas: TControlCanvas;
  DC: HDC;
  PS: TPaintStruct;
begin
  if FDisplayType = scedtNumeric then
    Result := inherited IsCustomDraw(ADC)
  else
    begin
      Result := True;
      R := GetTextRect;
      S := FormatFloat(',0.00', Value);
      DC := ADC;
      if DC = 0 then DC := BeginPaint(Handle, PS);
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
      FCanvas.Handle := DC;
      try
        with FCanvas do
        begin
          Font := Self.Font;
          if not Enabled then
          begin
            if (seFont in StyleElements) and IsCustomStyle then
              Font.Color := ColorToRGB(GetEditTextColor(scsDisabled))
            else
              Font.Color := clGrayText;
          end
          else
          begin
            if (seFont in StyleElements) and IsCustomStyle then
             Font.Color := GetEditTextColor(scsNormal);
          end;
          Brush.Style := bsClear;
         end;
         if BorderStyle <> bsNone then
           Inc(R.Top);
         R.Bottom := R.Top + FCanvas.TextHeight(S);
         DrawTextAlignment(FCanvas, S, R, taRightJustify);
      finally
        FCanvas.Handle := 0;
        FCanvas.Free;
        if ADC = 0 then EndPaint(Handle, PS)
      end;
    end;
end;

procedure TscGPSpinEdit.SetDisplayType(Value: TscNumEditDisplayType);
begin
  if FDisplayType <> Value then
  begin
    FDisplayType := Value;
    if FDisplayType = scedtCurrency then
      FDecimal := 2;
    Invalidate;
  end;
end;

procedure TscGPSpinEdit.WMKILLFOCUS(var Message: TMessage);
begin
  inherited;
  StopCheck := True;
  if ValueType = scvtFloat
  then Text := FloatToStrF(FValue, ffFixed, 15, FDecimal)
  else Text := IntToStr(Round(FValue));
  StopCheck := False;
  if (ValueType = scvtFloat) and (FValue <> StrToFloat(Text)) then
  begin
    FValue := StrToFloat(Text);
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TscGPSpinEdit.GetValueAsInt: Integer;
begin
  Result := Round(FValue);
end;

procedure TscGPSpinEdit.SetValueType(NewType: TscValueType);
begin
  if FValueType <> NewType then
  begin
    FValueType := NewType;
  end;
end;

procedure TscGPSpinEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then
  begin
    FDecimal := NewValue;
  end;
end;

function TscGPSpinEdit.CheckValue;
begin
  Result := NewValue;

  if ValueType = scvtInteger then
  begin
    if Result > MaxInt then
      Result := MaxInt
    else
    if Result < -MaxInt then
      Result := -MaxInt;
  end;

  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
    Result := FMinValue
    else if NewValue > FMaxValue then
    Result := FMaxValue;
  end
  else
  if FMaxValue > 0 then
    Result := FMaxValue;
end;

procedure TscGPSpinEdit.SetMinValue;
begin
  FMinValue := AValue;
end;

procedure TscGPSpinEdit.SetMaxValue;
begin
  FMaxValue := AValue;
end;

function TscGPSpinEdit.IsNumText;

function GetMinus: Boolean;
var
  i: Integer;
  S: String;
begin
  S := AText;
  i := Pos('-', S);
  if i > 1 then
    Result := False
  else
    begin
      Delete(S, i, 1);
      Result := Pos('-', S) = 0;
    end;
end;

function GetP: Boolean;
var
  i: Integer;
  S: String;
begin
  S := AText;
  i := Pos(FormatSettings.DecimalSeparator, S);
  if i = 1
  then
    Result := False
  else
    begin
      Delete(S, i, 1);
      Result := Pos(FormatSettings.DecimalSeparator, S) = 0;
    end;
end;

const
  EditChars = '01234567890-';
var
  i: Integer;
  S: String;
begin
  S := EditChars;
  Result := True;
  if ValueType = scvtFloat
  then
    S := S + FormatSettings.DecimalSeparator;
  if (Text = '') or (Text = '-')
  then
    begin
      Result := False;
      Exit;
    end;

  for i := 1 to Length(Text) do
  begin
    if Pos(Text[i], S) = 0
    then
      begin
        Result := False;
        Break;
      end;
  end;

  Result := Result and GetMinus;

  if ValueType = scvtFloat
  then
    Result := Result and GetP;

end;

procedure TscGPSpinEdit.Change;
var
  NewValue, TmpValue: Double;

function CheckInput: Boolean;
begin
  if (NewValue < 0) and (TmpValue < 0)
  then
    Result := NewValue > TmpValue
  else
    Result := NewValue < TmpValue;

  if not Result and ( ((FMinValue > 0) and (TmpValue < 0))
    or ((FMaxValue < 0) and (TmpValue > 0)))
  then
    Result := True;
end;

begin
  if FromEdit then Exit;
  if not StopCheck and IsNumText(Text)
  then
    begin
       if ValueType = scvtFloat
      then
        TmpValue := StrToFloat(Text)
      else
      begin
        TmpValue := StrToFloat(Text);
        if TmpValue > MaxInt then
        begin
          TmpValue := MaxInt;
          FromEdit := True;
          Text := IntToStr(MaxInt);
          FromEdit := False;
        end
        else
        if TmpValue < -MaxInt then
        begin
          TmpValue := -MaxInt;
          FromEdit := True;
          Text := IntToStr(-MaxInt);
          FromEdit := False;
        end
        else
          TmpValue := StrToInt(Text);
      end;
      NewValue := CheckValue(TmpValue);
      if NewValue <> FValue
      then
        begin
          FValue := NewValue;
        end;
      if CheckInput
      then
        begin
          FromEdit := True;
          if ValueType = scvtFloat
          then Text := FloatToStrF(NewValue, ffFixed, 15, FDecimal)
          else Text := IntToStr(Round(FValue));
          FromEdit := False;
        end;
    end;
  inherited;
end;

procedure TscGPSpinEdit.SetValue;
var
  OldValue: Double;
begin
  OldValue := FValue;
  FValue := CheckValue(AValue);
  if OldValue = FValue then
    FIsModified := False;
  StopCheck := True;
  if ValueType = scvtFloat
  then
    Text := FloatToStrF(CheckValue(AValue), ffFixed, 15, FDecimal)
  else
    Text := IntToStr(Round(CheckValue(AValue)));
  StopCheck := False;
end;

procedure TscGPSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  FIsModified := True;
  if Key = VK_UP then
    Value := Value + FIncrement
  else
  if Key = VK_DOWN then
    Value := Value - FIncrement;
  FIsModified := False;
end;

procedure TscGPSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key)
  then
    begin
      Key := #0;
      MessageBeep(0)
    end;
  inherited KeyPress(Key);
end;

function TscGPSpinEdit.IsValidChar(Key: Char): Boolean;
var
  S: String;
  DecPos: Integer;
begin
  if ValueType = scvtFloat
  then
    Result := CharInSet(Key, [FormatSettings.DecimalSeparator, '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)))
  else
    Result := CharInSet(Key, ['-', '0'..'9']) or
     ((Key < #32) and (Key <> Chr(VK_RETURN)));

  if (Key = FormatSettings.DecimalSeparator) and (Pos(FormatSettings.DecimalSeparator, Text) <> 0)  then
    Result := False
  else
  if (Key = '-') and (SelStart = 0) and (SelLength > 0) then
     Result := True
  else
  if (Key = '-') and (SelStart <> 0)  then
     Result := False
  else
  if (Key = '-') and (Pos('-', Text) <> 0) then
    Result := False;

  if ReadOnly and Result and ((Key >= #32) or
     (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)))
  then
    Result := False;

  if Result and (Key <> Char(VK_BACK)) and (Key <> Char(VK_DELETE)) then
  begin
    S := EditText;
    DecPos := Pos(FormatSettings.DecimalSeparator, S);
    if (DecPos > 0) and (SelText = '') and (SelStart >= DecPos) and (Length(S) - DecPos >= Decimal) then
      Exit(False);
  end;
end;

constructor TscGPTimeEdit.Create(AOwner: TComponent);
begin
  inherited;
  FShowUpDown := False;
  CharCase := TEditCharCase.ecUpperCase;
  FShowMSec := False;
  FShowSec := True;
  FTimeFormat := sctf24Hour;
  EditMask := '!90:00:00;1; ';
  Text := '00' + FormatSettings.TimeSeparator + '00' + FormatSettings.TimeSeparator + '00';
end;

destructor TscGPTimeEdit.Destroy;
begin
  inherited;
end;

function TscGPTimeEdit.GetArrowGlyphColor: TColor;
begin
  Result := FLeftButton.GlyphColor;
end;

function TscGPTimeEdit.GetArrowGlyphColorAlpha: Byte;
begin
  Result := FLeftButton.GlyphColorAlpha;
end;

function TscGPTimeEdit.GetArrowGlyphColorHotAlpha: Byte;
begin
  Result := FLeftButton.GlyphColorHotAlpha
end;

function TscGPTimeEdit.GetArrowGlyphColorPressedAlpha: Byte;
begin
  Result := FLeftButton.GlyphColorPressedAlpha
end;

procedure TscGPTimeEdit.SetArrowGlyphColor(Value: TColor);
begin
  FLeftButton.GlyphColor := Value;
  FRightButton.GlyphColor := Value;
end;

procedure TscGPTimeEdit.SetArrowGlyphColorAlpha(Value: Byte);
begin
  FLeftButton.GlyphColorAlpha := Value;
  FRightButton.GlyphColorAlpha := Value;
end;

procedure TscGPTimeEdit.SetArrowGlyphColorHotAlpha(Value: Byte);
begin
  FLeftButton.GlyphColorHotAlpha := Value;
  FRightButton.GlyphColorHotAlpha := Value;
end;

procedure TscGPTimeEdit.SetArrowGlyphColorPressedAlpha(Value: Byte);
begin
  FLeftButton.GlyphColorPressedAlpha := Value;
  FRightButton.GlyphColorPressedAlpha := Value;
end;

function  TscGPTimeEdit.GetTextRect: TRect;
var
  R, R1: TRect;
  W: Integer;
  Buffer: TBitmap;
begin
  Result := ClientRect;
  Inc(Result.Left, FContentMarginLeft);
  Inc(Result.Top, FContentMarginTop);
  Dec(Result.Right, FContentMarginRight);
  Dec(Result.Bottom, FContentMarginBottom);
  if not Assigned(FLeftButton) or not Assigned(FRightButton) then Exit;
  R := Result;
  R1 := R;
  if FOptions.ScaleFrameWidth then
    W := FOptions.FFrameWidth
  else
    W := Round(FOptions.FrameWidth * FScaleFactor);
  R1.Top := W * 2;
  R1.Bottom := Height - W * 2;
  FLeftButton.ButtonRect := Rect(0, 0, 0, 0);
  FRightButton.ButtonRect := Rect(0, 0, 0, 0);
  FGlyphSize := R.Height - R.Height div 2;
  W := R1.Height - R1.Height div 2;
  if BidiMode = bdRightToLeft then
  begin
   if FLeftButton.Visible then
   begin
     FLeftButton.ButtonRect := Rect(R.Left, R1.Top, R.Left + W, R1.Top + R1.Height div 2);
   end;
   if FRightButton.Visible then
   begin
     FRightButton.ButtonRect := Rect(R.Left, R1.Bottom - R1.Height div 2, R.Left + W, R1.Bottom);
     Inc(R.Left, FRightButton.ButtonRect.Width + Round(4 * FScaleFactor));
   end;
  end
  else
  begin
    if FLeftButton.Visible then
    begin
      FLeftButton.ButtonRect := Rect(R.Right - W, R1.Top, R.Right, R1.Top + R1.Height div 2);
    end;
    if FRightButton.Visible then
    begin
      FRightButton.ButtonRect := Rect(R.Right - W, R1.Bottom - R1.Height div 2, R.Right, R1.Bottom);
      Dec(R.Right, RightButton.ButtonRect.Width + Round(4 * FScaleFactor));
    end;
  end;

  Buffer := TBitmap.Create;
  try
    Buffer.Canvas.Font := Self.Font;
    if R.Width < Buffer.Canvas.TextWidth('W') then
      R.Width := Buffer.Canvas.TextWidth('W');
    if R.Height < Buffer.Canvas.TextHeight('W') then
     R.Height := Buffer.Canvas.TextHeight('W');
  finally
    Buffer.Free;
  end;

  Result := R;
end;

procedure TscGPTimeEdit.WMMOUSEWHEEL(var Message: TMessage);
begin
  if TWMMOUSEWHEEL(Message).WheelDelta > 0
  then
    UpButtonClick(Self)
  else
    DownButtonClick(Self);
end;

function TscGPTimeEdit.GetIncIndex: Integer;
var
  i, j, k: Integer;
  S: String;
begin
  j := Self.SelStart;
  if (FTimeFormat = sctf12Hour) and (J >= Length(Text) - 2) then
  begin
    Result := 4;
    Exit;
  end;
  k := 0;
  S := Text;
  for i := 1 to j do
  begin
    if S[i] = FormatSettings.TimeSeparator then inc(k);
    if S[i] = '.' then k := 3;
  end;
  Result := k;
end;


procedure TscGPTimeEdit.UpButtonClick(Sender: TObject);
var
  k, i: Integer;
  Hour, Min, Sec, MSec: Word;
  S: String;
begin
  if not Focused then SetFocus;
  DecodeTime(Hour, Min, Sec, MSec);
  k := GetIncIndex;
  i := SelStart;
  case k of
    0:
      begin
        if Hour = 23 then Hour := 0 else Inc(Hour);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
    1:
      begin
        if Min = 59 then Min := 0 else Inc(Min);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
    2:
      if FShowSec then
      begin
        if Sec = 59 then Sec := 0 else Inc(Sec);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
    3:
      if FShowMSec then
      begin
        if MSec = 999 then MSec := 0 else Inc(MSec);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
    4: begin
         S := Text;
         if S[Length(Text) - 1] = 'A' then
           S[Length(Text) - 1] := 'P'
         else
           S[Length(Text) - 1] := 'A';
         S[Length(Text)] := 'M';
         Text := S;
       end;
   end;
  SelStart := i;
end;

procedure TscGPTimeEdit.DownButtonClick(Sender: TObject);
var
  k, i: Integer;
  Hour, Min, Sec, MSec: Word;
  S: String;
begin
  if not Focused then SetFocus;
  DecodeTime(Hour, Min, Sec, MSec);
  k := GetIncIndex;
  i := SelStart;
  case k of
    0:
      begin
        if Hour = 0 then Hour := 23 else  Dec(Hour);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
    1:
      begin
        if Min = 0 then Min := 59 else Dec(Min);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
    2:
      if FShowSec then
      begin
        if Sec = 0 then Sec := 59 else Dec(Sec);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
    3:
      if FShowMSec then
      begin
        if MSec = 0 then MSec := 999 else Dec(MSec);
        EncodeTime(Hour, Min, Sec, MSec);
      end;
     4:
       if FTimeFormat = sctf12Hour then
       begin
         S := Text;
         if S[Length(Text) - 1] = 'A' then
           S[Length(Text) - 1] := 'P'
         else
           S[Length(Text) - 1] := 'A';
         S[Length(Text)] := 'M';
         Text := S;
       end;
  end;
  SelStart := i;
end;

procedure TscGPTimeEdit.ShowUpDownButtons;
begin
  LeftButton.Kind := scgpebUpArrow;
  LeftButton.Visible := True;
  LeftButton.RepeatClick := True;
  LeftButton.RepeatClickInterval := 100;
  RightButton.Kind := scgpebDownArrow;
  RightButton.Visible := True;
  RightButton.RepeatClick := True;
  RightButton.RepeatClickInterval := 100;
  OnRightButtonClick := DownButtonClick;
  OnLeftButtonClick := UpButtonClick;
end;

procedure TscGPTimeEdit.HideUpDownButtons;
begin
  LeftButton.Visible := False;
  RightButton.Visible := False;
  OnRightButtonClick := nil;
  OnLeftButtonClick := nil;
end;

procedure TscGPTimeEdit.SetTimeFormat(Value: TscTimeFormat);
begin
  FTimeFormat := Value;
  if FTimeFormat = sctf24Hour then
  begin
     if FShowSec
      then
        begin
          if FShowMSec
          then
            begin
              EditMask := '!90:00:00.000;1; ';
              Text := '00:00:00.000';
            end
          else
           begin
             EditMask := '!90:00:00;1; ';
             Text := '00:00:00';
           end;
        end
      else
        begin
          EditMask := '!90:00;1; ';
          Text := '00:00';
        end;
  end
  else
  begin
    if FShowSec
      then
        begin
          if FShowMSec
          then
            begin
              EditMask := '!90:00:00.000 LL;1; ';
              Text := '12:00:00.000 AM';
            end
          else
           begin
             EditMask := '!90:00:00 LL;1; ';
             Text := '12:00:00 AM';
           end;
        end
      else
        begin
          EditMask := '!90:00 LL;1; ';
          Text := '12:00 AM';
        end;
  end;
end;

procedure TscGPTimeEdit.SetShowUpDown;
begin
  FShowUpDown := Value;
  if FShowUpDown then ShowUpDownButtons else HideUpDownButtons;
end;

procedure TscGPTimeEdit.ValidateEdit;
var
  Str: string;
  Pos: Integer;
begin
  Str := EditText;
  if IsMasked and Modified then
  begin
    if not Validate(Str, Pos) then
    begin
    end;
  end;
end;

procedure TscGPTimeEdit.CheckSpace(var S: String);
var
  i: Integer;
begin
  for i := 0 to Length(S) do
  begin
    if S[i] = ' ' then S[i] := '0';
  end;
end;

procedure TscGPTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      UpButtonClick(Self);
    VK_DOWN:
      DownButtonClick(Self);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TscGPTimeEdit.KeyPress(var Key: Char);
var
  TimeStr: string;
  aHour, aMinute, aSecond, aMillisecond: Word;
  aHourSt, aMinuteSt, aSecondSt, aMillisecondSt: string;
begin
   if (Key <> #13) and (Key <> #8)
   then
   begin
   TimeStr := Text;
   if FTimeFormat = sctf12Hour then
   begin
     if SelStart = Length(TimeStr) - 2 then
     begin
       if Key = 'a' then Key := 'A';
       if Key = 'p' then Key := 'P';
       if (Key <> 'A') and (Key <> 'P') then
         Key := #0;
       inherited KeyPress(Key);
       Exit;
     end
     else
     if SelStart = Length(TimeStr) - 1 then
     begin
       if Key = 'm' then Key := 'M';
       if (Key <> 'M') then
         Key := #0;
       inherited KeyPress(Key);
       Exit;
     end;
   end;
   if SelLength > 1 then SelLength := 1;
   if IsValidChar(Key)
   then
     begin
       Delete(TimeStr,SelStart + 1, 1);
       Insert(string(Key), TimeStr, SelStart + 1);
     end;
      try
         aHourSt := Copy(TimeStr, 1, 2);
         CheckSpace(aHourSt);

         aMinuteSt := Copy(TimeStr, 4, 2);
         CheckSpace(aMinuteSt);

         if FShowSec
         then
           aSecondSt := Copy(TimeStr, 7, 2)
         else
           aSecondSt := '0';
         CheckSpace(aSecondSt);

         if fShowMSec then begin
            aMillisecondSt := Copy(TimeStr, 10, 3);
         end else begin
            aMillisecondSt := '0';
         end;
         CheckSpace(aMillisecondSt);

         aHour := StrToInt(aHourSt);
         aMinute := StrToInt(aMinuteSt);
         aSecond := StrToInt(aSecondSt);
         aMillisecond := StrToInt(aMillisecondSt);

         if not IsValidTime(aHour, aMinute, aSecond, aMillisecond) then begin
            Key := #0;
         end;

         if (FTimeFormat = sctf12Hour) and ((aHour = 0) or (aHour > 12)) then
         begin
           Key := #0;
         end;

      except
         Key := #0;
      end;
   end;
  inherited KeyPress(Key);
end;

procedure TscGPTimeEdit.SetShowSeconds(const Value: Boolean);
begin
  if FShowSec <> Value
  then
    begin
      FShowSec := Value;
      SetTimeFormat(FTimeFormat);
    end;
end;

procedure TscGPTimeEdit.SetShowMilliseconds(const Value: Boolean);
begin
   if FShowMSec <> Value
   then
     begin
       FShowMSec := Value;
       SetTimeFormat(FTimeFormat);
     end;
end;

procedure TscGPTimeEdit.SetMilliseconds(const Value: Integer);
var
   aHour, aMinute, aSecond, aMillisecond: Integer;
   St, S: string;
begin
  if not FShowMSec then
    Exit;

  aSecond := Value div 1000;
  aMillisecond := Value mod 1000;
  aMinute := aSecond div 60;
  aSecond := aSecond mod 60;
  aHour := aMinute div 60;
  aMinute := aMinute mod 60;
  St := Format('%2.2d:%2.2d:%2.2d.%3.3d', [aHour, aMinute, aSecond, aMillisecond]);
  try
    S := '';
    if FTimeFormat = sctf12Hour then
    begin
      S := Text;
      if S[Length(S) - 1] = 'A' then
       S := ' AM'
     else
       S := ' PM';
    end;
    Text := St + S;
  except
    if FTimeFormat = sctf24Hour then
       Text := '00:00:00.000'
     else
       Text := '12:00:00.000' + S;
  end;
end;

function TscGPTimeEdit.GetMilliseconds: Integer;
var
   TimeStr: string;
   aHour, aMinute, aSecond, aMillisecond: Integer;
   aHourSt, aMinuteSt, aSecondSt, aMillisecondSt: string;
begin
  if not FShowMSec then
  begin
    Result := 0;
    Exit;
  end;

   TimeStr := Text;
   try
      aHourSt := Copy(TimeStr, 1, 2);
      CheckSpace(aHourSt);
      aMinuteSt := Copy(TimeStr, 4, 2);
      CheckSpace(aMinuteSt);
      aSecondSt := Copy(TimeStr, 7, 2);
      CheckSpace(aSecondSt);
      aMillisecondSt := Copy(TimeStr, 10, 3);
      CheckSpace(aMillisecondSt);
      aHour := StrToInt(aHourSt);
      aMinute := StrToInt(aMinuteSt);
      aSecond := StrToInt(aSecondSt);
      aMillisecond := StrToInt(aMillisecondSt);
      Result := ((((aHour * 60) + aMinute) * 60) + aSecond) * 1000 + aMillisecond;
   except
      Result := 0;
   end;
end;

procedure TscGPTimeEdit.SetTime(const Value: string);
var
   TimeStr: string;
   aHour, aMinute, aSecond, aMillisecond: Integer;
   aHourSt, aMinuteSt, aSecondSt, aMillisecondSt: string;
   S: String;
begin

   TimeStr := Value;
   try
      aHourSt := Copy(TimeStr, 1, 2);
      CheckSpace(aHourSt);
      aMinuteSt := Copy(TimeStr, 4, 2);
      CheckSpace(aMinuteSt);
      if FShowSec
      then
        begin
          aSecondSt := Copy(TimeStr, 7, 2);
          CheckSpace(aSecondSt);
        end
      else
        aSecondSt := '0';

      aHour := StrToInt(aHourSt);
      aMinute := StrToInt(aMinuteSt);
      aSecond := StrToInt(aSecondSt);
      S := '';

      if FTimeFormat = sctf12Hour then
      begin
        S := Value;
        if Pos('M', S) = 0  then
        begin
          S := ' AM';
          if (aHour > 12) or ((aHour = 12) and (aMinute > 0)) then
          begin
            if aHour > 12 then
                aHour := aHour - 12;
            S := ' PM';
          end;
          if aHour = 0 then
          begin
            aHour := 12;
            S := ' AM';
          end;
        end
        else
        begin
          if S[Length(S) - 1] = 'A' then
            S := ' AM'
          else
            S := ' PM';
        end;
      end;

      if fShowMSec and (Length(TimeStr) > 11) and (Pos('.', TimeStr) <> 0)
      then
        begin
          aMillisecondSt := Copy(TimeStr, 10, 3);
          CheckSpace(aMillisecondSt);
          aMillisecond := StrToInt(aMillisecondSt);
          Text := Format('%2.2d:%2.2d:%2.2d.%3.3d', [aHour, aMinute, aSecond, aMillisecond]) + S;
        end
      else
        begin
          if FShowSec
          then
            Text := Format('%2.2d:%2.2d:%2.2d', [aHour, aMinute, aSecond]) + S
          else
            Text := Format('%2.2d:%2.2d', [aHour, aMinute]) + S;
        end;
   except
     SetTimeFormat(FTimeFormat);
   end;
end;

function TscGPTimeEdit.GetTime: string;
begin
  Result := Text;
end;

function TscGPTimeEdit.IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  Result := ((AHour < 24) and (AMinute < 60) and
            (ASecond < 60) and (AMilliSecond < 1000));
end;

function TscGPTimeEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := CharInSet(Key, ['0'..'9']);
end;

procedure TscGPTimeEdit.SetValidTime(var H, M, S, MS: Word);
begin
  if H > 23 then H := 23;
  if M > 59 then M := 59;
  if S > 59 then S := 59;
  if MS > 999 then MS := 999;
end;

function TscGPTimeEdit.ValidateParameter;
var
  I: Integer;

begin
  Result := S;
  if Length(S) <> MustLen
  then
    begin
      for i := 1 to MustLen do S[i] := '0';
      Exit;
    end;
  for I := 1 to Length(s) do
    if not IsValidChar(S[I])
    then
      begin
        Result := '00';
        Break;
      end;
end;

function TscGPTimeEdit.GetTimeValue: TTime;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Hour, Min, Sec, MSec);
  Result := System.SysUtils.EncodeTime(Hour, Min, Sec, MSec);
end;

procedure TscGPTimeEdit.SetTimeValue(Value: TTime);
var
  Hour, Min, Sec, MSec: Word;
begin
  System.SysUtils.DecodeTime(Value, Hour, Min, Sec, MSec);
  Self.EncodeTime(Hour, Min, Sec, MSec);
end;

procedure TscGPTimeEdit.DecodeTime(var Hour, Min, Sec, MSec: Word);
var
  TimeStr: string;
  aHourSt, aMinuteSt, aSecondSt, aMillisecondSt: string;
begin
  TimeStr := Text;
  aHourSt := Copy(TimeStr, 1, 2);
  CheckSpace(aHourSt);
  aMinuteSt := Copy(TimeStr, 4, 2);
  CheckSpace(aMinuteSt);
  if FShowSec
  then
    aSecondSt := Copy(TimeStr, 7, 2)
  else
    aSecondSt := '00';

  CheckSpace(aSecondSt);

  aHourSt := ValidateParameter(aHourSt, 2);
  aMinuteSt := ValidateParameter(aMinuteSt, 2);
  aSecondSt := ValidateParameter(aSecondSt, 2);

  Hour := StrToInt(aHourSt);
  Min := StrToInt(aMinuteSt);
  Sec := StrToInt(aSecondSt);

  if FTimeFormat = sctf12Hour then
  begin
    if TimeStr[Length(TimeStr) - 1] = 'P' then
    begin
      if Hour < 12 then
        Hour := Hour + 12;
    end
    else
    begin
      if Hour = 12 then
        Hour := 0;
    end;
  end;

  if fShowMSec
  then
    aMillisecondSt := Copy(TimeStr, 10, 3)
  else
    aMillisecondSt := '000';

  CheckSpace(aMillisecondSt);
  aMillisecondSt := ValidateParameter(aMillisecondSt, 3);
  Msec := StrToInt(aMillisecondSt);
  SetValidTime(Hour, Min, Sec, MSec);
end;


procedure TscGPTimeEdit.EncodeTime(Hour, Min, Sec, MSec: Word);
var
  FAMPM: String;
begin
  FAMPM := '';
  if FTimeFormat = sctf12Hour then
  begin
    FAMPM := 'AM';
    if (Hour > 12) or ((Hour = 12) and (Min > 0)) then
    begin
      if Hour > 12 then
        Hour := Hour - 12;
      FAMPM := 'PM';
    end;
    if Hour = 0 then
    begin
      Hour := 12;
      FAMPM := 'AM';
    end;
  end;
  if FAMPM <> '' then
    FAMPM := ' ' + FAMPM;
  if not IsValidTime(Hour, Min, Sec, MSec) then Exit;
  try
    if fShowMSec
    then
      Text := Format('%2.2d:%2.2d:%2.2d.%3.3d', [Hour, Min, Sec, MSec])  + FAMPM
    else
      if FShowSec
      then
        Text := Format('%2.2d:%2.2d:%2.2d', [Hour, Min, Sec]) + FAMPM
      else
        Text := Format('%2.2d:%2.2d', [Hour, Min]) + FAMPM;
  except
    if fShowMSec
    then
      Text := '00:00:00.000' + FAMPM
    else
      if FShowSec
      then
        Text := '00:00:00' + FAMPM
      else
        Text := '00:00' + FAMPM;
  end;
end;

constructor TscGPPasswordEditButton.Create;
begin
  FVisible := False;
  FGlyphColor := clWindowText;
  FGlyphColorAlpha := 180;
  FGlyphColorHotAlpha := 240;
  FGlyphColorPressedAlpha := 150;
end;

procedure TscGPPasswordEditButton.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TscGPPasswordEditButton.SetGlyphColor(Value: TColor);
begin
  if FGlyphColor <> Value then
  begin
    FGlyphColor := Value;
    Changed;
  end;
end;

procedure TscGPPasswordEditButton.SetGlyphColorAlpha(Value: Byte);
begin
  if FGlyphColorAlpha <> Value then
  begin
    FGlyphColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPPasswordEditButton.Assign(Source: TPersistent);
begin
  if Source is TscGPPasswordEditButton then
  begin
    FVisible := TscGPPasswordEditButton(Source).Visible;
    FGlyphColor := TscGPPasswordEditButton(Source).GlyphColor;
    FGlyphColorAlpha := TscGPPasswordEditButton(Source).GlyphColorAlpha;
    FGlyphColorHotAlpha := TscGPPasswordEditButton(Source).GlyphColorHotAlpha;
    FGlyphColorPressedAlpha := TscGPPasswordEditButton(Source).GlyphColorPressedAlpha;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPPasswordEditButton.Changed;
begin
  if FVisible then
    if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscGPPasswordEdit.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse] - [csSetCaption];
  FGlyphSize := 16;
  FPasswordButton := TscGPPasswordEditButton.Create;
  FPasswordButton.OnChange := OptionsChanged;
  FShowingText := False;
  FOptions := TscGPEditOptions.Create;
  FOptions.OnChange := OptionsChanged;
  FPromptText := '';
  FPromptTextColor := clNone;
  FHidePromptTextIfFocused := False;
  TransparentBackground := True;
  ParentColor := False;
  Color := clWindow;
  Text := '';
  FMouseIn := False;
  Width := 121;
  Height := 21;
  TabStop := True;
  FTextAlignment := taLeftJustify;
  FAutoSelect := True;
  FCharCase := TEditCharCase.ecNormal;
  FHideSelection := True;
  FMaxLength := 0;
  FReadOnly := False;
  FLMouseSelecting := False;
  FCaretPosition := 0;
  FSelStart := 0;
  FSelLength := 0;
  FFVChar := 1;
  FTransparent := True;
  FContentMarginLeft := 5;
  FContentMarginRight := 5;
  FContentMarginTop := 5;
  FContentMarginBottom := 5;
end;

destructor TscGPPasswordEdit.Destroy;
begin
  FOptions.Free;
  FPasswordButton.Free;
  inherited;
end;

procedure TscGPPasswordEdit.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  if FOptions.FScaleFrameWidth then
     FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);
  FOptions.ShapeCornerRadius :=  MulDiv(FOptions.ShapeCornerRadius, M, D);
  FContentMarginLeft := MulDiv(FContentMarginLeft, M, D);
  FContentMarginTop := MulDiv(FContentMarginTop, M, D);
  FContentMarginRight := MulDiv(FContentMarginRight, M, D);
  FContentMarginBottom := MulDiv(FContentMarginBottom, M, D);
end;


procedure TscGPPasswordEdit.OptionsChanged(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPPasswordEdit.SetPromptText(Value: String);
begin
  if FPromptText <> Value then
  begin
    FPromptText := Value;
    RePaintControl;
  end;
end;

procedure TscGPPasswordEdit.SetPromptTextColor(Value: TColor);
begin
  if FPromptTextColor <> Value then
  begin
    FPromptTextColor := Value;
    Invalidate;
  end;
end;

procedure TscGPPasswordEdit.SetContentMarginLeft(Value: Integer);
begin
  if (FContentMarginLeft >= 0) and (FContentMarginLeft <> Value) then
  begin
    FContentMarginLeft := Value;
    Invalidate;
  end;
end;

procedure TscGPPasswordEdit.SetContentMarginTop(Value: Integer);
begin
  if (FContentMarginTop >= 0) and (FContentMarginTop <> Value) then
  begin
    FContentMarginTop := Value;
    Invalidate;
  end;
end;

procedure TscGPPasswordEdit.SetContentMarginRight(Value: Integer);
begin
  if (FContentMarginRight >= 0) and (FContentMarginRight <> Value) then
  begin
    FContentMarginRight := Value;
    Invalidate;
  end;
end;

procedure TscGPPasswordEdit.SetContentMarginBottom(Value: Integer);
begin
  if (FContentMarginBottom >= 0) and (FContentMarginBottom <> Value) then
  begin
    FContentMarginBottom := Value;
    Invalidate;
  end;
end;

procedure TscGPPasswordEdit.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    FTransparentBackground := FTransparent;
    GetParentBG;
    RePaintControl;
  end;
end;

function TscGPPasswordEdit.GetPaintText;
begin
  Result := Text;
end;

procedure TscGPPasswordEdit.PasteFromClipboard;
var
  Data: THandle;
  Insertion: WideString;
begin
  if ReadOnly then Exit;

  if Clipboard.HasFormat(CF_UNICODETEXT)
  then
    begin
      Data := Clipboard.GetAsHandle(CF_UNICODETEXT);
      try
        if Data <> 0
        then
          Insertion := PWideChar(GlobalLock(Data));
      finally
        if Data <> 0 then GlobalUnlock(Data);
      end;
    end
  else
    Insertion := Clipboard.AsText;

  InsertText(Insertion);
end;

procedure TscGPPasswordEdit.ShowPasswordText(AShow: Boolean);
begin
  if AShow and not FShowingText then
  begin
    FShowingText := True;
    if Focused then
      HideCaret;
    RePaintControl;
  end
  else
  if not AShow and FShowingText then
  begin
    FShowingText := False;
    RePaintControl;
     if Focused then
      ShowCaret;
  end;
end;

procedure TscGPPasswordEdit.DrawEditShape(G: TGPGraphics; ACanvas: TCanvas);
var
  R, TR: TRect;
  B: TGPBrush;
  LB: TGPSolidBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, R1: TGPRectF;
  FrameColor, FillColor: Cardinal;
  l, t, w, h, d: Single;
  C1, C2: Cardinal;
  FCtrlState: TscsCtrlState;
begin
  R := Rect(0, 0, Width, Height);
  TR := R;
  if not FTransparent then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;
  FCtrlState := scsNormal;
  if not Enabled then
    FCtrlState := scsDisabled
  else
  if Focused then
    FCtrlState := scsFocused
  else
  if FMouseIn then
    FCtrlState := scsHot;
  FOptions.State := FCtrlState;
  // draw button shape
  P := TGPPen.Create(0, FOptions.FrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  // colors
  FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
  FillColor := ColorToGPColor(Options.Color, Options.ColorAlpha);
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
  if FOptions.ShapeFillStyle = scgpsfColor then
    B := TGPSolidBrush.Create(FillColor)
  else
  begin
    C1 := ColorToGPColor(LighterColor(FOptions.Color, 25), Options.ColorAlpha);
    C2 := ColorToGPColor(DarkerColor(FOptions.Color, 25), Options.ColorAlpha);
    R1 := FillR;
    InflateGPRect(R1, 1, 1);
    B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientAngle);
  end;
  // draw
  try
    case FOptions.ShapeStyle of
      scgpessLine:
        begin
          LB := TGPSolidBrush.Create(FrameColor);
          FrameR.Height := FOptions.FFrameWidth;
          FrameR.Y := Height - FOptions.FFrameWidth;
          G.FillRectangle(LB, FrameR);
          LB.Free;
        end;
      scgpessRect:
        begin
          G.FillRectangle(B, FillR);
          G.DrawRectangle(P, FrameR);
        end;
      scgpessRoundedRect, scgpessRoundedLeftRight:
        begin
          // fill
          l := FillR.X;
          t := FillR.y;
          w := FillR.Width;
          h := FillR.Height;
          if Options.ShapeStyle = scgpessRoundedLeftRight
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
          // frame
          l := FrameR.X;
          t := FrameR.y;
          w := FrameR.Width;
          h := FrameR.Height;
          if Options.ShapeStyle = scgpessRoundedLeftRight
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
    end;
  finally
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPPasswordEdit.DrawPasswordText(ACanvas: TCanvas);
var
  R: TRect;
  C: TColor;
begin
  R := GetEditRect;
  InflateRect(R, -1, -1);
  Dec(R.Bottom);
  ACanvas.Font := Self.Font;
  C := ColorToRGB(GetStyleColor(Self.Font.Color));
  ACanvas.Font.Color := C;
  ACanvas.Brush.Style := bsClear;
  scDrawUtils.DrawTextAlignmentNoPrefix(ACanvas, Text, R,
    taLeftJustify, IsRightToLeft);
end;

procedure TscGPPasswordEdit.DrawPromptText(ACanvas: TCanvas);
var
  R: TRect;
  C: TColor;
begin
  if Focused and FHidePromptTextIfFocused then
    Exit;

  R := GetEditRect;
  InflateRect(R, -1, -1);
  Dec(R.Bottom);
  ACanvas.Font := Self.Font;
  if FPromptTextColor = clNone then
  begin
    C := GetStyleColor(Self.Font.Color);
    if IsLightColor(C) then
      C := DarkerColor(C, 40)
    else
      C := LighterColor(C, 40);
  end
  else
    C := ColorToRGB(GetStyleColor(FPromptTextColor));
  ACanvas.Font.Color := C;
  ACanvas.Brush.Style := bsClear;
  scDrawUtils.DrawTextAlignmentNoPrefix(ACanvas, FPromptText, R,
    taLeftJustify, IsRightToLeft);
end;

procedure TscGPPasswordEdit.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  G: TGPGraphics;
begin
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  DrawEditShape(G, ACanvas);
  if (FPromptText <> '') and (Text = '') then
    DrawPromptText(ACanvas);
  if FShowingText then
    DrawPasswordText(ACanvas)
  else
  begin
    PaintText(G, ACanvas);
    if Focused or not HideSelection
    then
      with ACanvas do
      begin
        Brush.Color := clHighLight;
        FillRect(GetSelRect);
      end;
    if Focused or not HideSelection then
      PaintSelectedText(G, ACanvas);
  end;
  if FPasswordButton.Visible then
    PaintButton(G, ACanvas);
  G.Free;
end;

procedure TscGPPasswordEdit.Loaded;
begin
  inherited;
end;

procedure TscGPPasswordEdit.WMSIZE(var Msg: TMessage);
begin
  inherited;
  UpdateCarete;
end;

procedure TscGPPasswordEdit.WMSETFOCUS(var Message: TWMSETFOCUS);
begin
  inherited;
  UpdateCarete;
  CaretPosition := 0;
  if AutoSelect and not FLMouseSelecting then
    SelectAll;
end;

procedure TscGPPasswordEdit.WMKILLFOCUS(var Message: TWMKILLFOCUS);
begin
  inherited;
  DestroyCaret;
  RePaintControl;
end;

function TscGPPasswordEdit.GetCharX(a: Integer): Integer;
var
  WTextWidth : Integer;
  ERWidth : Integer;
begin
  Result := GetEditRect.Left;
  WTextWidth := Length(Text) * GetPasswordFigureWidth;
  if a > 0
  then
    begin
      if a <= Length(Text)
      then Result := Result + (a - FFVChar + 1) * GetPasswordFigureWidth
      else Result := Result + (Length(Text) - FFVChar + 1) * GetPasswordFigureWidth;
  end;
  ERWidth := GetEditRect.Right - GetEditRect.Left;
  if WTextWidth < ERWidth
  then
    case TextAlignment of
      taRightJustify : Result := Result + (ERWidth - WTextWidth);
      taCenter : Result := Result + ((ERWidth - WTextWidth) div 2);
    end;
end;

function TscGPPasswordEdit.GetCPos(x: Integer): Integer;
var
  TmpX,
  WTextWidth,
  ERWidth : Integer;
begin
  Result := FFVChar - 1;
  if Length(Text) = 0 then  Exit;
  WTextWidth := Length(Text) * GetPasswordFigureWidth;

  ERWidth := GetEditRect.Right - GetEditRect.Left;
  TmpX := x;

  if WTextWidth < ERWidth
  then
    case TextAlignment of
      taRightJustify : TmpX := x - (ERWidth - WTextWidth);
      taCenter : TmpX := x - ((ERWidth - WTextWidth) div 2);
    end;

  Result := Result + (TmpX - GetEditRect.Left) div GetPasswordFigureWidth;
  if Result < 0
  then
    Result := 0
  else
    if Result > Length(Text)
    then
      Result := Length(Text);
end;

function TscGPPasswordEdit.GetEditRect: TRect;
var
  R, R1: TRect;
  W: Integer;
begin
  Result := Rect(0, 0, Width, Height);
  Inc(Result.Left, FContentMarginLeft);
  Inc(Result.Top, FContentMarginTop);
  Dec(Result.Right, FContentMarginRight);
  Dec(Result.Bottom, FContentMarginBottom);
  if FPasswordButton.Visible then
  begin
    R := Result;
    R1 := R;
    if FOptions.ScaleFrameWidth then
      W := FOptions.FFrameWidth
    else
      W := Round(FOptions.FrameWidth * FScaleFactor);
    R1.Top := W * 2;
    R1.Bottom := Height - W * 2;
    FPasswordButton.ButtonRect := Rect(0, 0, 0, 0);
    FGlyphSize := R.Height;
    if BidiMode = bdRightToLeft then
    begin
      FPasswordButton.ButtonRect := Rect(R.Left, R1.Top, R.Left + R1.Height, R1.Bottom);
      Inc(R.Left, FPasswordButton.ButtonRect.Width + Round(4 * FScaleFactor));
    end
    else
    begin
      FPasswordButton.ButtonRect := Rect(R.Right - R1.Height, R1.Top, R.Right, R1.Bottom);
      Dec(R.Right, FPasswordButton.ButtonRect.Width + Round(4 * FScaleFactor));
    end;
    Result := R;
  end;
end;

function TscGPPasswordEdit.GetAlignmentFlags: Integer;
begin
  case FTextAlignment of
    taCenter: Result := DT_CENTER;
    taRightJustify: Result := DT_RIGHT;
  else
    Result := DT_LEFT;
  end;
end;

procedure TscGPPasswordEdit.KeyDown(var Key: word; Shift: TShiftState);
var
  TmpS: String;
  OldCaretPosition: Integer;
begin
  inherited KeyDown(Key, Shift);
  OldCaretPosition := CaretPosition;
  case Key of

    Ord('v'), Ord('V'):
      if Shift = [ssCtrl] then PasteFromClipboard;

    VK_INSERT:
      if Shift = [ssShift] then PasteFromClipboard;

    VK_END: CaretPosition := Length(Text);

    VK_HOME: CaretPosition := 0;

    VK_LEFT:
      if ssCtrl in Shift then
        CaretPosition := GetPrivWPos(CaretPosition)
      else
        CaretPosition := CaretPosition - 1;

    VK_RIGHT:
      if ssCtrl in Shift
      then
        CaretPosition := GetNextWPos(CaretPosition)
      else
        CaretPosition := CaretPosition + 1;

    VK_DELETE, 8:
      if not ReadOnly
      then
        begin
          if SelLength <> 0
          then
            ClearSelection
          else
            begin
              TmpS := Text;
              if TmpS <> ''
              then
                if Key = VK_DELETE
                then
                  Delete(TmpS, CaretPosition + 1, 1)
                else
                  begin
                    Delete(TmpS, CaretPosition, 1);
                    CaretPosition := CaretPosition - 1;
                  end;
               Text := TmpS;
            end;
        end;
  end;

  if Key in [VK_END, VK_HOME, VK_LEFT, VK_RIGHT]
  then
    begin
      if ssShift in Shift
      then
        begin
          if SelLength = 0
          then
            FSelStart := OldCaretPosition;
          FSelStart := CaretPosition;
          FSelLength := FSelLength - (CaretPosition - OldCaretPosition);
        end
      else
        FSelLength := 0;
     RePaintControl;
   end;
  UpdateCaretePosition;
end;

procedure TscGPPasswordEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Ord(Key) >= 32) and not ReadOnly then InsertChar(Key);
end;

procedure TscGPPasswordEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft
  then
    FLMouseSelecting := True;
  if not Focused then
  begin
    SetFocus;
  end;
  if Button = mbLeft
  then
    begin
      CaretPosition := GetCPos(x);
      SelLength := 0;
    end;

  if (Button = mbLeft) and FPasswordButton.Visible and
     FPasswordButton.ButtonRect.Contains(Point(X, Y))
     and not FPasswordButton.Down then
  begin
    FPasswordButton.Down := True;
    ShowPasswordText(True);
  end;

end;

procedure TscGPPasswordEdit.UpdateFVC;
var
  LEditRect: TRect;
begin
  if FFVChar >= (FCaretPosition + 1)
  then
    begin
      FFVChar := FCaretPosition;
      if FFVChar < 1 then FFVChar := 1;
    end
  else
    begin
      LEditRect := GetEditRect;
      while ((FCaretPosition - FFVChar + 1) * GetPasswordFigureWidth >
        LEditRect.Right - LEditRect.Left) and (FFVChar < Length(Text)) do
        Inc(FFVChar)
      end;
  RePaintControl;
end;

procedure TscGPPasswordEdit.MouseMove(Shift: TShiftState; x, y: Integer);
var
  OldCaretPosition: Integer;
  TmpNewPosition : Integer;
  R: TRect;
begin
  inherited;
  if FPasswordButton.Visible and FPasswordButton.ButtonRect.Contains(Point(X, Y)) and not FPasswordButton.MouseIn then
  begin
    FPasswordButton.MouseIn := True;
    Invalidate;
  end
  else
  if FPasswordButton.Visible and not FPasswordButton.ButtonRect.Contains(Point(X, Y)) and FPasswordButton.MouseIn then
  begin
    FPasswordButton.MouseIn := False;
    Invalidate;
  end;
  // check cursor
  R := GetEditRect;
  if R.Contains(Point(X, Y)) and (Cursor <> crIBeam) then
    Cursor := crIBeam
  else
  if not R.Contains(Point(X, Y)) and (Cursor <> crDefault) then
    Cursor := crDefault;
  //
  if FLMouseSelecting
  then
    begin
      TmpNewPosition := GetCPos(x);
      OldCaretPosition := CaretPosition;
      if (x > GetEditRect.Right)
      then
        CaretPosition := TmpNewPosition + 1
      else
        CaretPosition := TmpNewPosition;
      if SelLength = 0 then FSelStart := OldCaretPosition;
      FSelStart := CaretPosition;
      FSelLength := FSelLength - (CaretPosition - OldCaretPosition);
    end;
end;

procedure TscGPPasswordEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: Integer);
begin
  inherited;
  if FPasswordButton.Visible and FPasswordButton.Down then
  begin
    FPasswordButton.Down := False;
    ShowPasswordText(False);
  end;
  FLMouseSelecting := False;
end;

procedure TscGPPasswordEdit.PaintButton(G: TGPGraphics; Cnv: TCanvas);
var
  C: Cardinal;
  R: TGPRectF;
  R1: TRect;
begin
  if not FPasswordButton.Visible then Exit;
  if FPasswordButton.Down then
    C := ColorToGPColor(GetStyleColor(FPasswordButton.GlyphColor), FPasswordButton.GlyphColorPressedAlpha)
  else
  if FPasswordButton.MouseIn then
    C := ColorToGPColor(GetStyleColor(FPasswordButton.GlyphColor), FPasswordButton.GlyphColorHotAlpha)
  else
  if Self.Enabled then
    C := ColorToGPColor(GetStyleColor(FPasswordButton.GlyphColor), FPasswordButton.GlyphColorAlpha)
  else
    C := ColorToGPColor(GetStyleColor(FPasswordButton.GlyphColor), FPasswordButton.GlyphColorAlpha div 2);
  R1 := FPasswordButton.ButtonRect;
  R1.Top := R1.Top + R1.Height div 2 - FGlyphSize div 2;
  R1.Bottom := R1.Top + FGlyphSize;
  R1.Left := R1.Left + R1.Width div 2 - FGlyphSize div 2;
  R1.Right := R1.Left + FGlyphSize;
  R := RectToGPRect(R1);
  GPDrawPasswordGlyph(G, R, C, FScaleFactor);
end;

procedure TscGPPasswordEdit.PaintText;
var
  TmpRect, CR: TRect;
  CurChar: Integer;
  LPWCharWidth: Integer;
  S: String;
begin
  TmpRect := GetEditRect;
  LPWCharWidth := GetPasswordFigureWidth;
  if csPaintCopy in ControlState
  then
    begin
      S := GetPaintText
    end
  else
    S := Text;
  for CurChar := 0 to Length(S) - FFVChar do
  begin
    CR := Rect(CurChar * LPWCharWidth + GetCharX(0), TmpRect.Top,
     (CurChar + 1) * LPWCharWidth + GetCharX(0), TmpRect.Bottom);
    if CR.Right <= TmpRect.Right
    then
      DrawPasswordChar(G, CR, False, Cnv);
  end;
end;

procedure TscGPPasswordEdit.PaintSelectedText;
var
  TmpRect, CR, R1: TRect;
  CurChar: Integer;
  LPWCharWidth: Integer;
begin
  R1 := GetEditRect;
  TmpRect := GetSelRect;
  TmpRect.Top := R1.Top;
  TmpRect.Bottom := R1.Bottom;
  LPWCharWidth := GetPasswordFigureWidth;
  for CurChar := 0 to Length(GetVisibleSelText) - 1 do
  begin
    CR := Rect(CurChar * LPWCharWidth + TmpRect.Left,
      TmpRect.Top, (CurChar + 1) * LPWCharWidth + TmpRect.Left, TmpRect.Bottom);
    if CR.Right <= TmpRect.Right
    then
      DrawPasswordChar(G, CR, True, Cnv);
  end;
end;

function TscGPPasswordEdit.GetVisibleSelText: String;
begin
  if SelStart + 1 >= FFVChar
  then Result := SelText
  else Result := Copy(SelText, FFVChar - SelStart, Length(SelText) - (FFVChar - SelStart) + 1);
end;

function TscGPPasswordEdit.GetNextWPos(StartPosition: Integer): Integer;
var
  SpaceFound,
    WordFound: Boolean;
begin
  Result := StartPosition;
  SpaceFound := False;
  WordFound := False;
  while (Result + 2 <= Length(Text)) and
    ((not ((Text[Result + 1] <> ' ') and SpaceFound))
    or not WordFound) do
  begin
    if Text[Result + 1] = ' ' then
      SpaceFound := True;
    if Text[Result + 1] <> ' ' then begin
      WordFound := True;
      SpaceFound := False;
    end;

    Result := Result + 1;
  end;
  if not SpaceFound then
    Result := Result + 1;
end;

function TscGPPasswordEdit.GetPrivWPos(StartPosition: Integer): Integer;
var
  WordFound: Boolean;
begin
  Result := StartPosition;
  WordFound := False;
  while (Result > 0) and
    ((Text[Result] <> ' ') or not WordFound) do
  begin
    if Text[Result] <> ' ' then
      WordFound := True;
    Result := Result - 1;
  end;
end;

procedure TscGPPasswordEdit.ClearSelection;
var
  TmpS: String;
begin
  if ReadOnly then Exit;
  TmpS := Text;
  Delete(TmpS, SelStart + 1, SelLength);
  Text := TmpS;
  CaretPosition := SelStart;
  SelLength := 0;
end;

procedure TscGPPasswordEdit.SelectAll;
begin
  SetCaretPosition(Length(Text));
  SelStart := 0;
  SelLength := Length(Text);
  RePaintControl;
end;

procedure TscGPPasswordEdit.DrawPasswordChar(G: TGPGraphics; SymbolRect: TRect; Selected: Boolean; Cnv: TCanvas);
var
  R: TGPRectF;
  B: TGPSolidBrush;
  C: Cardinal;
begin
  R := RectToGPRect(SymbolRect);
  R.Y := SymbolRect.Top + SymbolRect.Height / 2 - SymbolRect.Width / 2;
  R.Height := R.Width;
  InflateGPRect(R, -2 * FScaleFactor, -2 * FScaleFactor);
  if not Selected then
  begin
    if Enabled then
      C := ColorToGPColor(ColorToRGB(GetStyleColor(Self.Font.Color)), 255)
    else  
      C := ColorToGPColor(ColorToRGB(GetStyleColor(Self.Font.Color)), 150);
  end
  else
  begin
    C := ColorToGPColor(ColorToRGB(clHighLightText), 255);
  end;
  B := TGPSolidBrush.Create(C);
  G.FillEllipse(B, R);
  B.Free;
end;

procedure TscGPPasswordEdit.SelectWord;
begin
  SelStart := GetPrivWPos(CaretPosition);
  SelLength := GetNextWPos(SelStart) - SelStart;
  CaretPosition := SelStart + SelLength;
end;

procedure TscGPPasswordEdit.UpdateCarete;
begin
  CreateCaret(Handle, 0, 0, Height - FContentMarginTop - FContentMarginBottom);
  CaretPosition := FCaretPosition;
  ShowCaret;
end;

procedure TscGPPasswordEdit.HideCaret;
begin
  WinApi.Windows.HideCaret(Handle);
end;

procedure TscGPPasswordEdit.ShowCaret;
begin
  if not (csDesigning in ComponentState) and Focused
  then
    WinApi.Windows.ShowCaret(Handle);
end;

function TscGPPasswordEdit.GetPasswordFigureWidth: Integer;
begin
  Result := GetEditRect.Height div 2;
end;

procedure TscGPPasswordEdit.Change;
begin
  inherited Changed;
  if Enabled and HandleAllocated then SetCaretPosition(CaretPosition);
  if Assigned(FOnChange) then  FOnChange(Self);
end;

procedure TscGPPasswordEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TscGPPasswordEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  FLMouseSelecting := False;
  SelectWord;
end;


procedure TscGPPasswordEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Font.Assign(Font);
  UpdateCarete;
end;

function TscGPPasswordEdit.GetText: String;
begin
  Result := FText;
end;

procedure TscGPPasswordEdit.SetText(const Value: String);
var
  S, S1: String;
begin
  if not ValidText(Value) then Exit;
  S := Value;
  S1 := Text;
  if (Value <> '') and (CharCase <> TEditCharCase.ecNormal)
  then
    case CharCase of
      TEditCharCase.ecUpperCase: FText := AnsiUpperCase(S);
      TEditCharCase.ecLowerCase: FText := AnsiLowerCase(S);
    end
  else
    FText := S;
  RePaintControl;
  if S <> S1 then Change;
end;

procedure TscGPPasswordEdit.SetCaretPosition(const Value: Integer);
begin
  if Value < 0
  then
    FCaretPosition := 0
  else
    if Value > Length(Text)
    then
      FCaretPosition := Length(Text)
    else
      FCaretPosition := Value;
  UpdateFVC;
  if SelLength <= 0 then FSelStart := Value;
  if Focused then SetCaretPos(GetCharX(FCaretPosition), GetEditRect.Top);
end;

procedure TscGPPasswordEdit.SetSelLength(const Value: Integer);
begin
  if FSelLength <> Value
  then
    begin
      FSelLength := Value;
      RePaintControl;
    end;
end;

procedure TscGPPasswordEdit.SetSelStart(const Value: Integer);
begin
  if FSelStart <> Value
  then
    begin
      SelLength := 0;
      FSelStart := Value;
      CaretPosition := FSelStart;
      RePaintControl;
    end;
end;

procedure TscGPPasswordEdit.SetAutoSelect(const Value: Boolean);
begin
  if FAutoSelect <> Value then FAutoSelect := Value;
end;

function TscGPPasswordEdit.GetSelStart: Integer;
begin
  if FSelLength > 0
  then
    Result := FSelStart
  else
    if FSelLength < 0
    then Result := FSelStart + FSelLength
    else Result := CaretPosition;
end;

function TscGPPasswordEdit.GetSelRect: TRect;
begin
  Result := GetEditRect;
  Result.Left := GetCharX(SelStart);
  Result.Right := GetCharX(SelStart + SelLength);
  IntersectRect(Result, Result, GetEditRect);
  Inc(Result.Top);
  Dec(Result.Bottom);
end;

function TscGPPasswordEdit.GetSelLength: Integer;
begin
  Result := Abs(FSelLength);
end;

function TscGPPasswordEdit.GetSelText: String;
begin
  Result := Copy(Text, SelStart + 1, SelLength);
end;

procedure TscGPPasswordEdit.SetCharCase(const Value: TEditCharCase);
var
  S: String;
begin
  if FCharCase <> Value
  then
    begin
      FCharCase := Value;
      if Text <> ''
      then
        begin
          S := Text;
          case Value of
            TEditCharCase.ecUpperCase: Text := AnsiUpperCase(S);
            TEditCharCase.ecLowerCase: Text := AnsiLowerCase(S);
          end;
        end;
    end;
end;

procedure TscGPPasswordEdit.SetHideSelection(const Value: Boolean);
begin
  if FHideSelection <> Value
  then
    begin
      FHideSelection := Value;
      RePaintControl;
    end;
end;

procedure TscGPPasswordEdit.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then FMaxLength := Value;
end;


function TscGPPasswordEdit.ValidText(NewText: String): Boolean;
begin
  Result := True;
end;

procedure TscGPPasswordEdit.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value
  then
    begin
      FTextAlignment := Value;
      RePaintControl;
    end;
end;

procedure TscGPPasswordEdit.UpdateCaretePosition;
begin
  SetCaretPosition(CaretPosition);
end;

procedure TscGPPasswordEdit.InsertText(AText: String);
var
  S: String;
begin
  if ReadOnly then Exit;
  S := Text;
  Delete(S, SelStart + 1, SelLength);
  Insert(AText, S, SelStart + 1);
  if (MaxLength <= 0) or (Length(S) <= MaxLength)
  then
    begin
      Text := S;
      CaretPosition := SelStart + Length(AText);
    end;
  SelLength := 0;
end;

procedure TscGPPasswordEdit.InsertChar(Ch: Char);
begin
  if ReadOnly then Exit;
  InsertText(Ch);
end;

procedure TscGPPasswordEdit.InsertAfter(Position: Integer; S: String;
  Selected: Boolean);
var
  S1: String;
  Insertion : String;
begin
  S := Text;
  Insertion := S;
  if MaxLength > 0
  then
    Insertion := Copy(Insertion, 1, MaxLength - Length(S1));
  Insert(Insertion, S1, Position+1);
  Text := S1;
  if Selected
  then
    begin
      SelStart := Position;
      SelLength := Length(Insertion);
      CaretPosition := SelStart + SelLength;
    end;
end;

procedure TscGPPasswordEdit.DeleteFrom(Position, Length: Integer; MoveCaret : Boolean);
var
  TmpS: String;
begin
  TmpS := Text;
  Delete(TmpS,Position,Length);
  Text := TmpS;
  if MoveCaret
  then
    begin
      SelLength := 0;
      SelStart := Position-1;
    end;
end;

procedure TscGPPasswordEdit.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  FText := inherited Text;
  SelLength := 0;
  RePaintControl;
end;

procedure TscGPPasswordEdit.Clear;
begin
  Text := '';
end;

procedure TscGPPasswordEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if HandleAllocated then RePaintControl;
end;

procedure TscGPPasswordEdit.CMMouseEnter;
begin
  inherited;
  FMouseIn := True;
  if (not Focused) then RePaintControl;
end;

procedure TscGPPasswordEdit.CMMouseLeave;
begin
  inherited;
  FMouseIn := False;
  if FPasswordButton.Visible and
    FPasswordButton.MouseIn then
  begin
    FPasswordButton.MouseIn := False;
    RePaintControl;
  end;
  if not Focused then RePaintControl;
end;

constructor TscGPCalButton.Create(AOwner: TComponent);
begin
  inherited;
  Animation := True;
  Options.ShapeStyle := scgpRounded;
  Options.FrameNormalColor := clNone;
  Options.FrameNormalColorAlpha := 0;
  Options.FrameNormalColor := clNone;
  Options.FrameNormalColorAlpha := 0;
  Options.FrameHotColor := clNone;
  Options.FrameHotColorAlpha := 0;
  Options.FramePressedColor := clNone;
  Options.FramePressedColorAlpha := 0;
  Options.NormalColor := clNone;
  Options.NormalColorAlpha := 0;
  Options.HotColor := clWindowText;
  Options.HotColorAlpha := 30;
  Options.PressedColor := clWindowText;
  Options.PressedColorAlpha := 50;
  Options.FontNormalColor := clWindowText;
  Options.FontHotColor := clWindowText;
  Options.FontPressedColor := clWindowText;
  DrawCode := 0;
end;

procedure TscGPCalButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  G: TGPGraphics;
  C: Cardinal;
  R: TGPRectF;
begin
  inherited;
  C := ColorToGPColor(ACanvas.Font.Color, 150);
  G := TGPGraphics.Create(ACanvas.Handle);
  R.X := 0;
  R.Y := 0;
  R.Width := Width;
  R.Height := Height;
  InflateGPRect(R, - Round(8 * FScaleFactor), - Round(8 * FScaleFactor));
  try
   case DrawCode of
    0:
    begin
      scGPUtils.GPDrawLeftGlyph(G, R, C, FScaleFactor);
    end;
    1:
    begin
      scGPUtils.GPDrawRightGlyph(G, R, C, FScaleFactor);
    end;
    2:
    begin
      R.X := R.X - Round(6 * FScaleFactor);
      scGPUtils.GPDrawLeftGlyph(G, R, C, FScaleFactor);
      R.X := R.X + Round(8 * FScaleFactor);
      scGPUtils.GPDrawLeftGlyph(G, R, C, FScaleFactor);
    end;
    3:
    begin
      R.X := R.X + Round(6 * FScaleFactor);
      scGPUtils.GPDrawRightGlyph(G, R, C, FScaleFactor);
      R.X := R.X - Round(8 * FScaleFactor);
      scGPUtils.GPDrawRightGlyph(G, R, C, FScaleFactor);
    end;
  end;
  finally
    G.Free;
  end;
end;

constructor TscGPMonthCalendar.Create;
begin
  inherited;
  FIsChina := (GetSystemDefaultLangID and $3FF) = $04;
  FScaleFrameWidth := False;
  FillColor := clWindow;
  FMenuPopup := False;
  FWeekNumbers := False;
  FMonthMenu := TPopupMenu.Create(Self);
  FShowToday := False;
  FInTodayR := False;
  FTodayR := Rect(0, 0, 0, 0);
  BottomOffset := 0;
  FBtns[0] := TscGPCalButton.Create(Self);
  FShowMonthMenu := True;
  with FBtns[0] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt;
    Width := BSize;
    Height := BSize;
    DrawCode := 0;
    OnClick := PriorMButtonClick;
    Parent := Self;
  end;

  FBtns[1] := TscGPCalButton.Create(Self);
  with FBtns[1] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt;
    Width := BSize;
    Height := BSize;
    DrawCode := 1;
    OnClick := NextMButtonClick;
    Parent := Self;
  end;

  FBtns[2] := TscGPCalButton.Create(Self);
  with FBtns[2] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt - 50;
    Width := BSize;
    Height := BSize;
    DrawCode := 2;
    OnClick := PriorYButtonClick;
    Parent := Self;
  end;

  FBtns[3] := TscGPCalButton.Create(Self);
  with FBtns[3] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt - 50;
    Width := BSize;
    Height := BSize;
    DrawCode := 3;
    OnClick := NextYButtonClick;
    Parent := Self;
  end;

  Width := 250;
  Height := 200;

  Date := Now;
  FTodayDefault := False;
  FBoldDays := False;
end;

destructor TscGPMonthCalendar.Destroy;
begin
  FMonthMenu.Free;
  inherited;
end;

procedure TscGPMonthCalendar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  ArangeControls;
end;

procedure TscGPMonthCalendar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
begin
  inherited;
  DrawCalendar(ACanvas);
end;

procedure TscGPMonthCalendar.DrawFrame;
var
  B: TGPSolidBrush;
  Col: Cardinal;
  GPR: TGPRectF;
  W: Integer;
begin
  Col := ColorToGPColor(GetStyleCOlor(clHighLight), 200);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  InflateRect(R, Round(2 * FScaleFactor), Round(2 * FScaleFactor));
  B := TGPSolidBrush.Create(Col);
  try
    W := R.Height;
    GPR.X := R.CenterPoint.X - W / 2;
    GPR.Y := R.Top;
    GPR.Width := W;
    GPR.Height := W;
    G.FillEllipse(B, GPR);
  finally
    B.Free;
  end;
  C.Font.Color := GetStyleColor(clHighLightText);
end;

procedure TscGPMonthCalendar.SetWeekNumbers(Value: Boolean);
begin
  if FWeekNumbers <> Value
  then
    begin
      FWeekNumbers := Value;
      RePaintControl;
    end;
end;

procedure TscGPMonthCalendar.SetShowToday(Value: Boolean);
begin
  if FShowToday <> Value
  then
    begin
      FShowToday := Value;
      RePaintControl;
    end;
end;

procedure TscGPMonthCalendar.SetBoldDays(Value: Boolean);
begin
  FBoldDays := Value;
  RePaintControl;
end;

procedure TscGPMonthCalendar.SetTodayDefault;
begin
  FTodayDefault := Value;
  if FTodayDefault then Date := Now;
end;

procedure TscGPMonthCalendar.NextMButtonClick(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  if AMonth = 12 then OffsetYear(1);
  OffsetMonth(1);
  Click;
end;

procedure TscGPMonthCalendar.PriorMButtonClick(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  if AMonth = 1 then OffsetYear(-1);
  OffsetMonth(-1);
  Click;
end;

procedure TscGPMonthCalendar.NextYButtonClick(Sender: TObject);
begin
  OffsetYear(1);
  Click;
end;

procedure TscGPMonthCalendar.PriorYButtonClick(Sender: TObject);
begin
  OffsetYear(-1);
  Click;
end;

procedure TscGPMonthCalendar.OffsetMonth(AOffset: Integer);
var
  AYear, AMonth, ADay: Word;
  TempDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  AMonth := AMonth + AOffset;
  if AMonth > 12 then AMonth := 1 else
  if AMonth <= 0 then AMonth := 12;
  if ADay > DaysPerMonth(AYear, AMonth)
  then ADay := DaysPerMonth(AYear, AMonth);
  TempDate := EncodeDate(AYear, AMonth, ADay);
  Date := TempDate;
end;

procedure TscGPMonthCalendar.OffsetYear(AOffset: Integer);
var
  AYear, AMonth, ADay: Word;
  TempDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  AYear := AYear + AOffset;
  if AYear <= 1760 then Exit else
    if AYear > 9999 then Exit;
  if ADay > DaysPerMonth(AYear, AMonth)
  then ADay := DaysPerMonth(AYear, AMonth);
  TempDate := EncodeDate(AYear, AMonth, ADay);
  Date := TempDate;
end;

procedure TscGPMonthCalendar.SetFirstDayOfWeek(Value: TscDaysOfWeek);
begin
  FFirstDayOfWeek := Value;
  UpdateCalendar;
end;

procedure TscGPMonthCalendar.ArangeControls;
var
  R: TRect;
  BSizeScale: Integer;
begin
  R := Rect(0, 0, Width, Height);
  InflateRect(R, -FFrameWidth-1, -FFrameWidth - 1);
  BSizeScale := Round(BSize * FScaleFactor);
  if FBtns[0] = nil then Exit;
  with FBtns[2] do SetBounds(R.Left + 2, R.Top + 2, Width, Height);
  with FBtns[0] do SetBounds(FBtns[2].Left + FBtns[2].Width + 1,
    R.Top + 2, Width, Height);
  with FBtns[3] do SetBounds(R.Right - BSizeScale - 2, R.Top + 2, Width, Height);
  with FBtns[1] do SetBounds(FBtns[3].Left - FBtns[3].Width - 1 , R.Top + 2, Width, Height);

  if FDrawTextMode = scdtmGDIPlus then
  begin
    if FBtns[0].Animation then
      FBtns[0].Animation := False;
    if FBtns[1].Animation then
      FBtns[1].Animation := False;
    if FBtns[2].Animation then
      FBtns[2].Animation := False;
    if FBtns[3].Animation then
      FBtns[3].Animation := False;
  end
  else
  begin
    if not FBtns[0].Animation then
      FBtns[0].Animation := True;
    if not FBtns[1].Animation then
      FBtns[1].Animation := True;
    if not FBtns[2].Animation then
      FBtns[2].Animation := True;
    if not FBtns[3].Animation then
      FBtns[3].Animation := True;
  end;
end;

procedure TscGPMonthCalendar.WMSIZE;
begin
  inherited;
  ArangeControls;
end;

procedure TscGPMonthCalendar.SetDate(Value: TDate);
begin
  FDate := Value;
  UpdateCalendar;
  RePaintControl;
end;

procedure TscGPMonthCalendar.UpdateCalendar;
begin
  RePaintControl;
end;

function TscGPMonthCalendar.GetMonthOffset: Integer;
var
  AYear, AMonth, ADay: Word;
  FirstDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  FirstDate := EncodeDate(AYear, AMonth, 1);
  Result := 2 - ((DayOfWeek(FirstDate) - Ord(FirstDayOfWeek) + 7) mod 7);
  if Result = 2 then Result := -5;
end;

procedure TscGPMonthCalendar.DrawCalendar(Cnvs: TCanvas);

function WeekOfTheYear(Dat: TDateTime): Word;
var
  Day, Month, Year: Word;
  FirstDate: TDateTime;
  DateDiff: Integer;
begin
  Day := DayOfWeek(Dat) - 1;
  Dat := Dat + 3 -((6 + day) mod 7);
  DecodeDate(Dat, Year, Month, Day);
  FirstDate := EncodeDate(Year,1,1);
  DateDiff := Trunc(Dat - FirstDate);
  Result := 1 + (DateDiff div 7);
end;

var
  R: TRect;
  I, J: Integer;
  FMonthOffset, X, Y, X2, Y2: Integer;
  S: String;
  ADay, DayNum: Integer;
  CDate: TDateTime;
  AYear, AMonth, ADay_: Word;
  Week, OldWeek: Integer;
  G: TGPGraphics;
  C: Cardinal;
  GR: TGPRectF;
  B: TGPSolidBrush;
  GFont: TGPFont;
  TR: TRect;
begin
  G := TGPGraphics.Create(Cnvs.Handle);
  GFont := nil;
  Cnvs.Brush.Style := bsClear;
  CalFontColor := GetStyleColor(Self.Font.Color);
  CalGrayFontColor := CalFontColor;
  if IsLightColor(CalGrayFontColor) then
    CalGrayFontColor := DarkerColor(CalGrayFontColor, 40)
  else
    CalGrayFontColor := LighterColor(CalGrayFontColor, 40);
  C := ColorToGPColor(CalFontColor, 50);
  B := TGPSolidBrush.Create(C);
  R := Rect(0, 0, Width, Height);
  InflateRect(R, -FFrameWidth, -FFrameWidth);
  if FWeekNumbers
  then
    Inc(R.Left, Width div 8);
  with Cnvs do
  begin
    Font := Self.Font;
    if FShowToday
    then
      begin
        BottomOffset := TextHeight('Wq') + 7;
        Dec(R.Bottom, BottomOffset);
      end
    else
      BottomOffset := 0;
    Brush.Style := bsClear;
    // draw caption
    S := FormatDateTime('MMMM, YYYY', FDate);
    Y := R.Top + Round(BSize * FScaleFactor) div 2 - TextHeight(S) div 2 + 1;
    Font.Style := [fsBold];
    X := Self.Width div 2 - TextWidth(S) div 2;
    Font.Color := CalFontColor;
    if FDrawTextMode  = scdtmGDIPlus then
    begin
      GFont := TGPFont.Create(Cnvs.Handle, Cnvs.Font.Handle);
      GPDrawTextXY(G, GFont, Cnvs, X, Y, S, IsRightToLeft);
    end
    else
      TextOut(X, Y, S);

    Font.Color := CalFontColor;
    CellW := (R.Width - 2) div 7;
    // draw week days
    X := R.Left + 1;
    Y := R.Top + Round(BSize * FScaleFactor) + 10;
    for I := 0 to 6 do
    begin
      S := FormatSettings.ShortDayNames[(Ord(FirstDayOfWeek) + I) mod 7 + 1];
      if FIsChina
      then
        begin
          if Length(S) > 2 then S := Copy(S, 3, 1)
        end
      else
        if Length(S) > 4 then S := Copy(S, 1, 4);

      X2 := X + CellW div 2 - TextWidth(S) div 2;

      if (FDrawTextMode  = scdtmGDIPlus) and (GFont <> nil) then
        GPDrawTextXY(G, GFont, Cnvs, X2, Y, S, IsRightToLeft)
      else
        TextOut(X2, Y, S);

      X := X + CellW;
    end;
    // draw bevel
    BevelTop := Y + TextHeight('Wq') + 1;
    GR.X := R.Left + 5;
    GR.Y := BevelTop;
    GR.Width := R.Width - 10;
    GR.Height := 1;
    G.FillRectangle(B, GR);
    if FWeekNumbers
    then
      begin
        GR.X := R.Left - 1;
        GR.Y := BevelTop + 5;
        GR.Width := 1;
        GR.Height := R.Height - GR.Y;
        G.FillRectangle(B, GR);
      end;

    if FBoldDays then Font.Style := [fsBold] else Font.Style := [];

    if GFont <> nil then
    begin
      GFont.Free;
      GFont := TGPFont.Create(Cnvs.Handle, Cnvs.Font.Handle);
    end;

    // draw today
    if FShowToday
    then
      begin
        if WeekNumbers then
          X := R.Left + 10
        else
          X := R.Left + 14;
        Y := R.Bottom + 2;
        S := DateToStr(Now);
        if FInTodayR then
          Font.Color := scDrawUtils.GetActiveTextColor
        else
          Font.Color := CalFontColor;
        if (FDrawTextMode  = scdtmGDIPlus) and (GFont <> nil) then
          GPDrawTextXY(G, GFont, Cnvs, X, Y, S, False)
        else
          TextOut(X, Y, S);
        FTodayR := Rect(X, Y, X + TextWidth(S), Y + TextHeight(S));
        InflateRect(FTodayR, Round(FScaleFactor), Round(FScaleFactor));
      end;
    // draw month numbers
    if FShowToday then
      CellH := (R.Bottom - BevelTop - Round(7 * FScaleFactor)) div 6
    else
      CellH := (R.Bottom - BevelTop - Round(5 * FScaleFactor)) div 6;
    Font.Color := CalFontColor;
    FMonthOffset := GetMonthOffset;
    ADay := ExtractDay(FDate);
    Y := BevelTop + Round(4 * FScaleFactor);
    OldWeek := -2;
    for J := 0 to 6 do
    begin
      X := R.Left + 1;
      if FWeekNumbers
      then
        begin
          Week := -1;
          for I := 0 to 6 do
          begin
            DayNum := FMonthOffset + I + (J - 1) * 7;
            if (DayNum > 0) and (DayNum <= DaysThisMonth)
            then
              begin
                DecodeDate(FDate, AYear, AMonth, ADay_);
                CDate := EncodeDate(AYear, AMonth, DayNum);
                Week := WeekOfTheYear(CDate);
                if FirstDayOfWeek <> Sun
                then
                  Break;
              end;
          end;
          if Week <> -1
          then
            begin
              if (OldWeek = Week)
              then
                begin
                  Week := OldWeek + 1;
                  if Week > 52 then Week := 52;
                end;

              OldWeek := Week;
              S := IntToStr(Week);

              Font.Color := CalGrayFontColor;
              Font.Style := Font.Style - [fsBold];
              if (FDrawTextMode  = scdtmGDIPlus) and (GFont <> nil) then
              begin
                TR := Rect(0, 0, 0, 0);
                GPDrawText (G, GFont, Cnvs, TR, S, DT_LEFT or DT_CALCRECT or DT_NOPREFIX);
                X2 := X + CellW div 2 - TR.Width div 2 - CellW - 2;
                Y2 := Y - CellH div 2 - TR.Height div 2;
                GPDrawTextXY(G, GFont, Cnvs, X2, Y2, S, False);
              end
              else
              begin
                X2 := X + CellW div 2 - TextWidth(S) div 2 - CellW - 2;
                Y2 := Y - CellH div 2 - TextHeight(S) div 2;
                TextOut(X2, Y2, S)
              end;
              Font.Color := CalFontColor;
            end;
         end;
      //
      for I := 0 to 6 do
      begin
        DayNum := FMonthOffset + I + (J - 1) * 7;
        if (DayNum < 1) or (DayNum > DaysThisMonth) then S := ''
        else S := IntToStr(DayNum);
        if DayNum = ADay
        then
          Font.Style := Font.Style + [fsBold]
        else
          Font.Style := Font.Style - [fsBold];

        if DayNum = ADay
        then
          DrawFrame(Rect(X, Y - CellH, X + CellW, Y + 1), Cnvs, G);
         if S <> '' then
          if (FDrawTextMode  = scdtmGDIPlus) and (GFont <> nil) then
          begin
            TR := Rect(0, 0, 0, 0);
            if DayNum = ADay then
              GPDrawText (G, nil, Cnvs, TR, S, DT_LEFT or DT_CALCRECT or DT_NOPREFIX)
            else
              GPDrawText (G, GFont, Cnvs, TR, S, DT_LEFT or DT_CALCRECT or DT_NOPREFIX);

            X2 := X + CellW div 2 - TR.Width div 2;
            Y2 := Y - CellH div 2 - TR.Height div 2;
            if DayNum = ADay then
              GPDrawTextXY(G, nil, Cnvs, X2,  Y2, S, False)
            else
              GPDrawTextXY(G, GFont, Cnvs, X2, Y2, S, False);
          end
          else
          begin
            X2 := X + CellW div 2 - TextWidth(S) div 2;
            Y2 := Y - CellH div 2 - TextHeight(S) div 2;
            TextOut(X2, Y2, S);
          end;
        Font.Color := CalFontColor;
        X := X + CellW;
      end;
      Y := Y + CellH;
    end;
  end;
  B.Free;
  if GFont <> nil then
    GFont.Free;
  G.Free;
end;

function TscGPMonthCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(ExtractYear(FDate), ExtractMonth(FDate));
end;

function TscGPMonthCalendar.DayNumFromPoint;
var
  R, R1: TRect;
  FMonthOffset, X1, Y1, I, J: Integer;
begin
  Result := 0;
  R := Rect(0, 0, Width, Height);
  if FWeekNumbers
  then
    begin
      Inc(R.Left, Width div 8);
    end;
  if FShowToday
  then
    begin
      Dec(R.Bottom, BottomOffset);
    end;
  if not PtInRect(R, Point(X, Y)) then Exit;
  FMonthOffset := GetMonthOffset;
  Y1 := BevelTop + Round(4 * FScaleFactor);
  for J := 0 to 6 do
  begin
    X1 := R.Left + 1;
    for I := 0 to 6 do
    begin
      R1 := Rect(X1, Y1 - CellH, X1 + CellW, Y1);
      if PtInRect(R1, Point(X, Y))
      then
        begin
          Result := FMonthOffset + I + (J - 1) * 7;
          if (Result < 1) or (Result > DaysThisMonth) then Result := 0;
          Break;
        end;
      X1 := X1 + CellW;
    end;
    Y1 := Y1 + CellH;
  end;
end;

procedure  TscGPMonthCalendar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FInTodayR then
  begin
    FInTodayR := False;
    RePaintControl;
  end;
end;

procedure TscGPMonthCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FShowToday then
  begin
    if FTodayR.Contains(Point(X, Y)) and not FInTodayR then
    begin
      FInTodayR := True;
      RePaintControl;
    end
    else
    if not FTodayR.Contains(Point(X, Y)) and FInTodayR then
    begin
      FInTodayR := False;
      RePaintControl;
    end;
  end;
end;

procedure TscGPMonthCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  DayNum, AYear, AMonth, ADay: Word;
  TempDate: TDate;
begin
  inherited;
  if Button <> mbLeft then Exit;
  if FShowMonthMenu and (X > FBtns[0].Left + FBtns[0].Width) and (X < Self.FBtns[1].Left) and
     (Y > 2) and (Y <= Self.FBtns[0].Top + Self.FBtns[0].Height) then
  begin
    GetCursorPos(P);
    CreateMonthMenu;
    FMenuPopup := True;
    FMonthMenu.Popup(P.X, P.Y);
    FMenuPopup := False;
  end
  else
  begin
    DayNum := DayNumFromPoint(X, Y);
    if DayNum <> 0
    then
    begin
      DecodeDate(FDate, AYear, AMonth, ADay);
      ADay := DayNum;
      TempDate := EncodeDate(AYear, AMonth, ADay);
      Date := TempDate;
      if Assigned(FOnNumberClick) then FOnNumberClick(Self);
    end
    else
    if FShowToday and FTodayR.Contains(Point(X, Y)) then
    begin
      Date := Now;
      if Assigned(FOnNumberClick) then FOnNumberClick(Self);
    end;
  end;
end;

procedure  TscGPMonthCalendar.MonthMenuClick(Sender: TObject);
var
  AYear, AMonth, ADay, DaysPM: Word;
  TempDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  AMonth := TMenuItem(Sender).MenuIndex + 1;
  DaysPM := DaysPerMonth(AYear, AMonth);
  if ADay > DaysPM then ADay := DaysPM;
  TempDate := EncodeDate(AYear, AMonth, ADay);
  Date := TempDate;
end;

procedure TscGPMonthCalendar.CreateMonthMenu;
var
  I: Integer;
  MI: TMenuItem;
begin
  if FMonthMenu.Items.Count > 0 then Exit;
  for I := 1 to 12 do
  begin
    MI := TMenuItem.Create(Self);
    MI.Caption := FormatSettings.LongMonthNames[I];
    MI.OnClick := MonthMenuClick;
    FMonthMenu.Items.Add(MI);
  end;
end;

procedure TscGPMonthCalendar.Loaded;
begin
  inherited;
  ArangeControls;
  if FTodayDefault then Date := Now;
end;


constructor TscGPPopupMonthCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FFillColor := clWindow;
  FFillColorAlpha := 255;
  TransparentBackground := False;
  DrawOnBackground := False;
  if FBtns[0] <> nil then
  begin
    FBtns[0].DrawOnBackground := False;
    FBtns[1].DrawOnBackground := False;
    FBtns[2].DrawOnBackground := False;
    FBtns[3].DrawOnBackground := False;
  end;
end;

procedure  TscGPPopupMonthCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscGPPopupMonthCalendar.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

constructor TscGPDateEdit.Create(AOwner: TComponent);
begin
  inherited;
  FBlanksChar := ' ';
  EditMask := GetDateMask;
  FMonthCalendar := TscGPPopupMonthCalendar.Create(Self);
  FMonthCalendar.FillColorAlpha := 255;
  if not (csDesigning in ComponentState) then
    FMonthCalendar.Parent := Self;
  FMonthCalendar.Visible := False;
  FMonthCalendar.OnNumberClick := CalendarClick;
  FMonthCalendar.OnMouseUp := CalendarMouseUp;
  FMonthCalendar.Date := 0;
  OnRightButtonClick := ButtonClick;
  FTodayDefault := False;
  RightButton.Visible := True;
  RightButton.FKind := scgpebDropDown;
end;

destructor TscGPDateEdit.Destroy;
begin
  FMonthCalendar.Free;
  FMonthCalendar := nil;
  inherited;
end;

procedure TscGPDateEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

function TscGPDateEdit.GetShowToday: Boolean;
begin
  Result := FMonthCalendar.ShowToday;
end;

procedure TscGPDateEdit.SetShowToday(Value: Boolean);
begin
  FMonthCalendar.ShowToday := Value;
end;

function TscGPDateEdit.GetWeekNumbers: Boolean;
begin
  Result := FMonthCalendar.WeekNumbers;
end;

procedure TscGPDateEdit.SetWeekNumbers(Value: Boolean);
begin
  FMonthCalendar.WeekNumbers := Value;
end;

function TscGPDateEdit.GetCalendarBoldDays: Boolean;
begin
  Result := FMonthCalendar.BoldDays;
end;

procedure TscGPDateEdit.SetCalendarBoldDays(Value: Boolean);
begin
  FMonthCalendar.BoldDays := Value;
end;

procedure TscGPDateEdit.ValidateEdit;
var
  Str: string;
  Pos: Integer;
begin
  Str := EditText;
  if IsMasked and Modified
  then
    begin
      if not Validate(Str, Pos) then
      begin
      end;
    end;
end;

function TscGPDateEdit.IsDateInput: Boolean;
begin
  Result := IsValidText(Text);
end;

function TscGPDateEdit.MonthFromName(const S: string; MaxLen: Byte): Byte;
begin
  if Length(S) > 0 then
    for Result := 1 to 12 do begin
      if (Length(FormatSettings.LongMonthNames[Result]) > 0) and
        (CompareText(Copy(S, 1, MaxLen),
        Copy(FormatSettings.LongMonthNames[Result], 1, MaxLen)) = 0) then Exit;
    end;
  Result := 0;
end;

function MakeStr(C: Char; N: Integer): String;
var
  S: String;
begin
  if N < 1 then Result := ''
  else
  begin
    S := StringOfChar(C, N);
    Result := S;
  end;
end;

procedure TscGPDateEdit.ExtractMask(const Format, S: string; Ch: Char; Cnt: Integer;
  var I: Integer; Blank, Default: Integer);
var
  Tmp: string;
  J, L: Integer;
  S1: String;
begin
  I := Default;
  Ch := UpCase(Ch);
  L := Length(Format);
  if Length(S) < L then L := Length(S)
  else if Length(S) > L then Exit;
  S1 := MakeStr(Ch, Cnt);
  J := Pos(S1, UpperCase(Format));
  if J <= 0 then Exit;
  Tmp := '';
  while (UpCase(Format[J]) = Ch) and (J <= L) do begin
    if S[J] <> ' ' then Tmp := Tmp + S[J];
    Inc(J);
  end;
  if Tmp = '' then I := Blank
  else if Cnt > 1 then begin
    I := MonthFromName(Tmp, Length(Tmp));
    if I = 0 then I := -1;
  end
  else I := StrToIntDef(Tmp, -1);
end;

function TscGPDateEdit.CurrentYear: Word;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := SystemTime.wYear;
end;

function TscGPDateEdit.ExpandYear(Year: Integer): Integer;
var
  N: Longint;
begin
  Result := Year;
  if Result < 100 then begin
    N := CurrentYear - CenturyOffset;
    Inc(Result, N div 100 * 100);
    if (CenturyOffset > 0) and (Result < N) then
      Inc(Result, 100);
  end;
end;

function TscGPDateEdit.IsValidDate(Y, M, D: Word): Boolean;
begin
  Result := (Y >= 1) and (Y <= 9999) and (M >= 1) and (M <= 12) and
    (D >= 1) and (D <= DaysPerMonth(Y, M));
end;

function TscGPDateEdit.ScanDate(const S, DateFormat: string; var Pos: Integer;
  var Y, M, D: Integer): Boolean;
var
  DateOrder: TscDateOrder;
  N1, N2, N3: Longint;
begin
  Result := False;
  Y := 0; M := 0; D := 0;
  DateOrder := GetDateOrder(DateFormat);
  if not (SCScanNumber(S, MaxInt, Pos, N1) and SCScanChar(S, Pos, FormatSettings.DateSeparator) and
    SCScanNumber(S, MaxInt, Pos, N2)) then Exit;
  if SCScanChar(S, Pos, FormatSettings.DateSeparator) then begin
    if not SCScanNumber(S, MaxInt, Pos, N3) then Exit;
    case DateOrder of
      scdoMDY: begin Y := N3; M := N1; D := N2; end;
      scdoDMY: begin Y := N3; M := N2; D := N1; end;
      scdoYMD: begin Y := N1; M := N2; D := N3; end;
    end;
    Y := ExpandYear(Y);
  end
  else begin
    Y := CurrentYear;
    if DateOrder = scdoDMY then begin
      D := N1; M := N2;
    end
    else begin
      M := N1; D := N2;
    end;
  end;
  SCScanChar(S, Pos, FormatSettings.DateSeparator);
  SCScanBlanks(S, Pos);
  Result := IsValidDate(Y, M, D) and (Pos > Length(S));
end;

function TscGPDateEdit.ScanDateStr(const Format, S: string; var D, M, Y: Integer): Boolean;
var
  Pos: Integer;
begin
  ExtractMask(Format, S, 'm', 3, M, -1, 0);
  if M = 0 then ExtractMask(Format, S, 'm', 1, M, -1, 0);
  ExtractMask(Format, S, 'd', 1, D, -1, 1);
  ExtractMask(Format, S, 'y', 1, Y, -1, CurrentYear);
  Y := ExpandYear(Y);
  Result := IsValidDate(Y, M, D);
  if not Result then
  begin
    Pos := 1;
    Result := ScanDate(S, Format, Pos, Y, M, D);
  end;
end;

function TscGPDateEdit.MyStrToDate(S: String): TDate;
var
  D, M, Y: Integer;
  B: Boolean;
begin
  Result := 0;
  if S <> '' then
  begin
    B := ScanDateStr(DefDateFormat(FourDigitYear), S, D, M, Y);
    if B then
    try
      Result := EncodeDate(Y, M, D);
    except
      Result := 0;
    end;
  end;
end;

function TscGPDateEdit.MyDateToStr(Date: TDate): String;
begin
  Result := FormatDateTime(DefDateFormat(FourDigitYear), Date);
end;

function TscGPDateEdit.IsOnlyNumbers;
const
  DateSymbols = '0123456789';
var
  i: Integer;
  S1: String;
begin
  Result := True;
  S1 := DateSymbols;
  S1 := S1 + FBlanksChar;
  for i := 1 to Length(S) do
  begin
    if (Pos(S[i], S1) = 0) and (S[i] <> FormatSettings.DateSeparator)
    then
      begin
        Result := False;
        Break;
      end;
  end;
end;

function TscGPDateEdit.FourDigitYear: Boolean;
begin
  Result := Pos('YYYY', UpperCase(FormatSettings.ShortDateFormat)) > 0;
end;

function TscGPDateEdit.GetDateOrder(const DateFormat: string): TscDateOrder;
var
  I: Integer;
begin
  Result := scdoMDY;
  I := 1;
  while I <= Length(DateFormat) do begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'Y': Result := scdoYMD;
      'M': Result := scdoMDY;
      'D': Result := scdoDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
end;


function TscGPDateEdit.DefDateFormat(FourDigitYear: Boolean): string;
begin
  if FourDigitYear then begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY: Result := 'MM/DD/YYYY';
      scdoDMY: Result := 'DD/MM/YYYY';
      scdoYMD: Result := 'YYYY/MM/DD';
    end;
  end
  else begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY: Result := 'MM/DD/YY';
      scdoDMY: Result := 'DD/MM/YY';
      scdoYMD: Result := 'YY/MM/DD';
    end;
  end;
end;

function TscGPDateEdit.DefDateMask(BlanksChar: Char; FourDigitYear: Boolean): string;
begin
  if FourDigitYear then begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY, scdoDMY: Result := '!99/99/9999;1;';
      scdoYMD: Result := '!9999/99/99;1;';
    end;
  end
  else begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY, scdoDMY: Result := '!99/99/99;1;';
      scdoYMD: Result := '!99/99/99;1;';
    end;
  end;
  if Result <> '' then Result := Result + BlanksChar;
end;

function TscGPDateEdit.GetDateMask: String;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

procedure TscGPDateEdit.Loaded;
begin
  inherited;
  EditMask := GetDateMask;
  if FTodayDefault then Date := Now;
end;

procedure TscGPDateEdit.SetTodayDefault;
begin
  FTodayDefault := Value;
  if FTodayDefault then Date := Now;
end;

function TscGPDateEdit.GetCalendarFont;
begin
  Result := FMonthCalendar.Font;
end;

procedure TscGPDateEdit.SetCalendarFont;
begin
  FMonthCalendar.Font.Assign(Value);
end;

function TscGPDateEdit.GetCalendarWidth: Integer;
begin
  Result := FMonthCalendar.Width;
end;

procedure TscGPDateEdit.SetCalendarWidth(Value: Integer);
begin
  FMonthCalendar.Width := Value;
end;

function TscGPDateEdit.GetCalendarHeight: Integer;
begin
  Result := FMonthCalendar.Height;
end;

procedure TscGPDateEdit.SetCalendarHeight(Value: Integer);
begin
  FMonthCalendar.Height := Value;
end;

function TscGPDateEdit.GetDate: TDate;
begin
  Result := FMonthCalendar.Date;
end;

procedure TscGPDateEdit.SetDate(Value: TDate);
begin
  FMonthCalendar.Date := Value;
  StopCheck := True;
  if not (csLoading in ComponentState) or FTodayDefault
  then
    begin
      Text := MyDateToStr(Value);
    end;
  StopCheck := False;
  if Assigned(FOnDateChange) then FOnDateChange(Self);
end;

function TscGPDateEdit.IsValidText;
var
  D, M, Y: Integer;
  DF: String;
begin
  Result := IsOnlyNumbers(S);
  if Result
  then
    begin
      DF := DefDateFormat(FourDigitYear);
      Result := ScanDateStr(DF, S, D, M, Y);
    end;
end;

procedure TscGPDateEdit.Change;
begin
  inherited;
  if not StopCheck
  then
    if IsValidText(Text)
    then CheckValidDate;
end;

procedure TscGPDateEdit.CheckValidDate;
begin
  if FMonthCalendar = nil then Exit;
  FMonthCalendar.Date := MyStrToDate(Text);
  if Assigned(FOnDateChange) then FOnDateChange(Self);
end;

procedure TscGPDateEdit.CMCancelMode;
begin
 if (Message.Sender <> FMonthCalendar) and (FMonthCalendar <> nil) and
     not FMonthCalendar.ContainsControl(Message.Sender)
 then
   CloseUp(False);
end;

function TscGPDateEdit.IsCalendarVisible: Boolean;
begin
  Result := (FMonthCalendar <> nil) and FMonthCalendar.Visible;
end;

procedure TscGPDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if not IsCalendarVisible then
  case Key of
    VK_DOWN, VK_RIGHT:
      if ssAlt in Shift then
        DropDown;
  end
  else
  case Key of
    VK_UP, VK_LEFT:
      if ssAlt in Shift then
        CloseUP(False);
  end;
end;

procedure TscGPDateEdit.WndProc;
var
  Offset: Integer;
begin
  case message.Msg of
    WM_KILLFOCUS:
      begin
        inherited;
        if FMonthCalendar <> nil then
        begin
          if not FMonthCalendar.Visible and FTodayDefault then
          begin
            StopCheck := True;
            Text := MyDateToStr(FMonthCalendar.Date);
            StopCheck := False;
          end
          else if message.wParam <> FMonthCalendar.Handle then
            CloseUp(False);
        end;
      end;

    WM_CHAR:
      if not IsCalendarVisible then
        inherited;

    WM_KEYUP:
      begin
        inherited;
        if IsCalendarVisible then
          case TWMKeyUp(message).CharCode of
            VK_SPACE:
              FMonthCalendar.Date := Now;
          end;
      end;

    WM_KEYDOWN:
      if IsCalendarVisible then
      begin
        Offset := 0;
        case TWMKeyDown(message).CharCode of
          VK_LEFT:
            Offset := -1;
          VK_RIGHT:
            Offset := 1;
          VK_UP:
            Offset := -7;
          VK_DOWN:
            Offset := 7;
          VK_RETURN:
            CloseUp(True);
          VK_ESCAPE:
            CloseUp(False);
          VK_NEXT:
             FMonthCalendar.NextMButtonClick(Self);
          VK_PRIOR:
             FMonthCalendar.PriorMButtonClick(Self);
        end;

        if Offset <> 0 then
          FMonthCalendar.Date := FMonthCalendar.Date + Offset;
      end
      else
        inherited;
  else
    inherited;
  end;
end;


procedure TscGPDateEdit.DropDown;
var
  P: TPoint;
  X, Y: Integer;
  WorkArea: TRect;
begin
  if ReadOnly then Exit;

  FDateSelected := False;
  if not FTodayDefault
  then
    begin
      FOldDateValue := FMonthCalendar.Date;
      if (FMonthCalendar.Date = 0) and (Pos(' ', Text) <> 0) then FMonthCalendar.Date := Now;
    end;
  FMonthCalendar.FrameWidth := FOptions.FrameWidth;
  P := Parent.ClientToScreen(Point(Left, Top));
  X := P.X;
  Y := P.Y + Height;
  WorkArea := Screen.MonitorFromWindow(Handle).WorkAreaRect;
  if Y + FMonthCalendar.Height > WorkArea.Bottom then
    Y := Y - Height - FMonthCalendar.Height;
  if X + FMonthCalendar.Width > WorkArea.Right then
    Dec(X, X + FMonthCalendar.Width - WorkArea.Right);
  if X < WorkArea.Left then
    X := WorkArea.Left;
  SetWindowPos(FMonthCalendar.Handle, HWND_TOP, X, Y,
   0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FMonthCalendar.Visible := True;
  SC_HookMouseMessages(Self);
end;

procedure TscGPDateEdit.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (Message.wParam <> FMonthCalendar.Handle) and
     (GetParent(Message.wParam) <> FMonthCalendar.Handle) and
     not FMonthCalendar.FMenuPopup
  then
    CloseUp(False);
end;

procedure TscGPDateEdit.CloseUp(AcceptValue: Boolean);
begin
  SC_UnHookMouseMessages;
  if (FMonthCalendar <> nil) and FMonthCalendar.Visible
  then
    begin
      SetWindowPos(FMonthCalendar.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
      FMonthCalendar.Visible := False;
      if AcceptValue
      then
        begin
          StopCheck := True;
          Text := MyDateToStr(FMonthCalendar.Date);
          if Assigned(FOnDateChange) then FOnDateChange(Self);
          StopCheck := False;
        end
      else
        if not FTodayDefault then FMonthCalendar.Date := FOldDateValue;
   end;
end;

procedure TscGPDateEdit.ButtonClick(Sender: TObject);
begin
  if FMonthCalendar.Visible
  then
    CloseUp(False)
  else
    DropDown;
end;

procedure TscGPDateEdit.CalendarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDateSelected then CloseUp(True);
end;

procedure TscGPDateEdit.CalendarClick;
begin
  FDateSelected := True;
end;

function TscGPDateEdit.GetFirstDayOfWeek: TscDaysOfWeek;
begin
  Result := FMonthCalendar.FirstDayOfWeek;
end;

procedure TscGPDateEdit.SetFirstDayOfWeek(Value: TscDaysOfWeek);
begin
  FMonthCalendar.FirstDayOfWeek := Value;
end;

///
 constructor TscGPMemoOptions.Create;
begin
  inherited;

  FShapeFillStyle := scgpsfColor;
  FShapeFillGradientAngle := -90;

  FNormalColor := clWindow;
  FHotColor := clWindow;
  FFocusedColor := clWindow;
  FDisabledColor := clWindow;
  FFrameNormalColor := clBtnText;
  FFrameHotColor := clHighLight;
  FFrameFocusedColor := clHighLight;
  FFrameDisabledColor := clBtnText;
  FFrameWidth := 1;
  FFontNormalColor := clWindowText;
  FFontHotColor := clWindowText;
  FFontFocusedColor := clWindowText;
  FFontDisabledColor := clGrayText;
  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;

  FNormalColorAlpha := 200;
  FHotColorAlpha := 255;
  FFocusedColorAlpha := 255;
  FDisabledColorAlpha := 150;

  FFrameNormalColorAlpha := 100;
  FFrameHotColorAlpha := 255;
  FFrameFocusedColorAlpha := 255;
  FFrameDisabledColorAlpha := 50;

  FShapeFillGradientBeginColorOffset := 25;
  FShapeFillGradientEndColorOffset := 25;

  FShapeCornerRadius := 0;
  FScaleFrameWidth := False;
end;

procedure TscGPMemoOptions.Assign(Source: TPersistent);
begin
  if Source is TscGPMemoOptions then
  begin
    FNormalColor := TscGPMemoOptions(Source).FNormalColor;
    FHotColor := TscGPMemoOptions(Source).FHotColor;
    FFocusedColor := TscGPMemoOptions(Source).FFocusedColor;
    FDisabledColor := TscGPMemoOptions(Source).FDisabledColor;
    FFrameNormalColor := TscGPMemoOptions(Source).FFrameNormalColor;
    FFrameHotColor := TscGPMemoOptions(Source).FFrameHotColor;
    FFrameFocusedColor := TscGPMemoOptions(Source).FFrameFocusedColor;
    FFrameDisabledColor := TscGPMemoOptions(Source).FFrameDisabledColor;
    FFrameWidth := TscGPMemoOptions(Source).FFrameWidth;
    FFontNormalColor := TscGPMemoOptions(Source).FFontNormalColor;
    FFontHotColor := TscGPMemoOptions(Source).FFontHotColor;
    FFontFocusedColor := TscGPMemoOptions(Source).FFontFocusedColor;
    FFontDisabledColor := TscGPMemoOptions(Source).FFontDisabledColor;
    FNormalColorAlpha := TscGPMemoOptions(Source).FNormalColorAlpha;
    FHotColorAlpha := TscGPMemoOptions(Source).FHotColorAlpha;
    FFocusedColorAlpha := TscGPMemoOptions(Source).FFocusedColorAlpha;
    FDisabledColorAlpha := TscGPMemoOptions(Source).FDisabledColorAlpha;
    FFrameNormalColorAlpha := TscGPMemoOptions(Source).FFrameNormalColorAlpha;
    FFrameHotColorAlpha := TscGPMemoOptions(Source).FFrameHotColorAlpha;
    FFrameFocusedColorAlpha := TscGPMemoOptions(Source).FFrameFocusedColorAlpha;
    FFrameDisabledColorAlpha := TscGPMemoOptions(Source).FFrameDisabledColorAlpha;
    FShapeCornerRadius := TscGPMemoOptions(Source).FShapeCornerRadius;
    FShapeFillStyle :=  TscGPMemoOptions(Source).FShapeFillStyle;

    FShapeFillGradientBeginColorOffset := TscGPMemoOptions(Source).FShapeFillGradientBeginColorOffset;
    FShapeFillGradientEndColorOffset := TscGPMemoOptions(Source).FShapeFillGradientEndColorOffset;

    FStyleColors := TscGPMemoOptions(Source).FStyleColors;
  end
  else
    inherited Assign(Source);
end;

procedure TscGPMemoOptions.SetShapeFillGradientBeginColorOffset(Value: Byte);
begin
  if (Value <= 100) and (FShapeFillGradientBeginColorOffset <> Value) then
  begin
    FShapeFillGradientBeginColorOffset := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPMemoOptions.SetShapeFillGradientEndColorOffset(Value: Byte);
begin
  if (Value <= 100) and (FShapeFillGradientEndColorOffset <> Value) then
  begin
    FShapeFillGradientEndColorOffset := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

 function TscGPMemoOptions.GetColorAlpha: Byte;
 begin
   Result := FNormalColorAlpha;
   case FState of
     scsHot: Result := FHotColorAlpha;
     scsPressed: Result := FFocusedColorAlpha;
     scsFocused: Result := FFocusedColorAlpha;
     scsDisabled: Result := FDisabledColorAlpha;
   end;
 end;

 function TscGPMemoOptions.GetFrameColorAlpha: Byte;
 begin
   Result := FFrameNormalColorAlpha;
   case FState of
     scsHot: Result := FFrameHotColorAlpha;
     scsPressed: Result := FFrameFocusedColorAlpha;
     scsFocused: Result := FFrameFocusedColorAlpha;
     scsDisabled: Result := FFrameDisabledColorAlpha;
   end;
 end;

procedure TscGPMemoOptions.SetShapeFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FShapeFillGradientAngle <> Value) then
  begin
    FShapeFillGradientAngle := Value;
    if FShapeFillStyle = scgpsfGradient then
      Changed;
  end;
end;

procedure TscGPMemoOptions.SetShapeFillStyle(Value: TscGPShapeFillStyle);
begin
  if FShapeFillStyle <> Value then
  begin
    FShapeFillStyle := Value;
    Changed;
  end;
end;

function TscGPMemoOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := FocusedColor;
    scsFocused: Result := FocusedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscGPMemoOptions.GetFrameColor: TColor;
begin
  Result := FrameNormalColor;
  case FState of
    scsHot: Result := FrameHotColor;
    scsPressed: Result := FrameFocusedColor;
    scsFocused: Result := FrameFocusedColor;
    scsDisabled: Result := FrameDisabledColor;
  end;
end;

function TscGPMemoOptions.GetFontColor: TColor;
begin
  Result := FontNormalColor;
  case FState of
    scsHot: Result := FontHotColor;
    scsPressed: Result := FontFocusedColor;
    scsFocused: Result := FontFocusedColor;
    scsDisabled: Result := FontDisabledColor;
  end;
end;

function TscGPMemoOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscGPMemoOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscGPMemoOptions.GetFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscGPMemoOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

function TscGPMemoOptions.GetFrameNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameNormalColor)
  else
    Result := FFrameNormalColor;
end;

function TscGPMemoOptions.GetFrameHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameHotColor)
  else
    Result := FFrameHotColor;
end;

function TscGPMemoOptions.GetFrameFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameFocusedColor)
  else
    Result := FFrameFocusedColor;
end;

function TscGPMemoOptions.GetFrameDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameDisabledColor)
  else
    Result := FFrameDisabledColor;
end;

function TscGPMemoOptions.GetFontNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontNormalColor)
  else
    Result := FFontNormalColor;
end;

function TscGPMemoOptions.GetFontHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontHotColor)
  else
    Result := FFontHotColor;
end;

function TscGPMemoOptions.GetFontFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontFocusedColor)
  else
    Result := FFontFocusedColor;
end;

function TscGPMemoOptions.GetFontDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontDisabledColor)
  else
    Result := FFontDisabledColor;
end;

procedure TscGPMemoOptions.SetShapeCornerRadius(Value: Integer);
begin
  if (FShapeCornerRadius <> Value) and (Value >= 0) then
  begin
    FShapeCornerRadius := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetNormalColorAlpha(Value: Byte);
begin
   if FNormalColorAlpha <> Value then
  begin
    FNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetHotColorAlpha(Value: Byte);
begin
  if FHotColorAlpha <> Value then
  begin
    FHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFocusedColorAlpha(Value: Byte);
begin
  if FFocusedColorAlpha <> Value then
  begin
    FFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetDisabledColorAlpha(Value: Byte);
begin
  if FDisabledColorAlpha <> Value then
  begin
    FDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameNormalColor(Value: TColor);
begin
  if FFrameNormalColor <> Value then
  begin
    FFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameHotColor(Value: TColor);
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameFocusedColor(Value: TColor);
begin
  if FFrameFocusedColor <> Value then
  begin
    FFrameFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameDisabledColor(Value: TColor);
begin
  if FFrameDisabledColor <> Value then
  begin
    FFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameNormalColorAlpha(Value: Byte);
begin
   if FFrameNormalColorAlpha <> Value then
  begin
    FFrameNormalColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameHotColorAlpha(Value: Byte);
begin
  if FFrameHotColorAlpha <> Value then
  begin
    FFrameHotColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameFocusedColorAlpha(Value: Byte);
begin
  if FFrameFocusedColorAlpha <> Value then
  begin
    FFrameFocusedColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameDisabledColorAlpha(Value: Byte);
begin
  if FFrameDisabledColorAlpha <> Value then
  begin
    FFrameDisabledColorAlpha := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFontNormalColor(Value: TColor);
begin
  if FFontNormalColor <> Value then
  begin
    FFontNormalColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFontHotColor(Value: TColor);
begin
  if FFontHotColor <> Value then
  begin
    FFontHotColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFontFocusedColor(Value: TColor);
begin
  if FFontFocusedColor <> Value then
  begin
    FFontFocusedColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFontDisabledColor(Value: TColor);
begin
  if FFontDisabledColor <> Value then
  begin
    FFontDisabledColor := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetFrameWidth(Value: Integer);
begin
  if (FFrameWidth <> Value) and (Value > 0) then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscGPMemoOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;


function TscGPMemoScrollBar.GetOptions: TscGPControlScrollBarOptions;
begin
  Result := nil;
  if FControl is TscGPMemo then
    Result := TscGPMemo(FControl).ScrollBarOptions;
end;


procedure TscGPMemoScrollBar.RePaint;
begin
  if (FControl <> nil) and FControl.Visible and Visible then
  begin
    if FControl is TscGPMemo then
      TscGPMemo(FControl).DoPaint
    else
      FControl.Invalidate;
  end;
end;

constructor TscGPMemo.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FFluentUIOpaque := False;
  FOptions := TscGPMemoOptions.Create;
  FOptions.OnChange := OptionsChanged;
  FScrollBarOptions := TscGPControlScrollBarOptions.Create;
  FScrollBarOptions.OnChange := ScrollOptionsChanged;
  FVertScrollBar := TscGPMemoScrollBar.Create(Self, True);
  FVertScrollBar.OnChange := OnVertScrollBarChanged;
  FHorzScrollBar := TscGPMemoScrollBar.Create(Self, False);
  FHorzScrollBar.OnChange := OnHorzScrollBarChanged;
  FTransparent := False;
  FScrollCaptured := False;
  FSizeChanging := False;
  FMouseIn := False;
  FContentMarginLeft := 5;
  FContentMarginRight := 5;
  FContentMarginTop := 5;
  FContentMarginBottom := 5;
  BorderStyle := bsNone;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FStopRedraw := False;
  FStopGetParentBG := False;
  FSelLen := 0;
  FSelPos := 0;
  ParentBGBuffer := nil;
  Color := clBtnFace;
  {$IFDEF VER230}
  FStyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
end;

destructor TscGPMemo.Destroy;
begin
  if ParentBGBuffer <> nil then
    ParentBGBuffer.Free;
  FOptions.Free;
  FScrollBarOptions.Free;
  FVertScrollBar.Free;
  FHorzScrollBar.Free;
  inherited;
end;

procedure TscGPMemo.OnHorzScrollBarChanged(Sender: TObject);
begin
  SendMessage(Handle, WM_HSCROLL,
    MakeWParam(SB_THUMBPOSITION, FHorzScrollBar.Position), 0);
end;

procedure TscGPMemo.OnVertScrollBarChanged(Sender: TObject);
begin
  SendMessage(Handle, WM_VSCROLL,
    MakeWParam(SB_THUMBPOSITION, FVertScrollBar.Position), 0);
end;

procedure TscGPMemo.WMTimer(var Message: TWMTimer);
var
  P: TPoint;
begin
  inherited;
  if Message.TimerID = 10 then
  begin
    GetCursorPos(P);
    if WindowFromPoint(P) <> Handle then
    begin
      KillTimer(Handle, 10);
      if FMouseIn then
        MemoMouseLeave;
    end;
  end
  else
  if Message.TimerID = 100 then
  begin
    KillTimer(Handle, 100);
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPMemo.WMNCHITTEST(var Message: TWMNCHITTEST);
var
  R: TRect;
  P: TPoint;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  if not FMouseIn and not (csDesigning in ComponentState) then
     MemoMouseEnter;

  if ScrollBars <> ssNone then
  begin
    R := GetTextRect;
    P.X := Message.XPos;
    P.Y := Message.YPos;
    P := ScreenToClient(P);
    if R.Contains(P) then
      Message.Result := HTCLIENT
    else
      Message.Result := HTBORDER;
  end
  else
    inherited;
end;

procedure TscGPMemo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustTextRect;
end;

procedure TscGPMemo.CMMouseLeave(var Message: TMessage);
var
  P: TPoint;
begin
  inherited;
  GetCursorPos(P);
  if WindowFromPoint(P) <> Handle then
    MemoMouseLeave;
end;

procedure TscGPMemo.MemoMouseEnter;
begin
  FMouseIn := True;
  DoPaint;
  SetTimer(Handle, 10, 200, nil);
end;

procedure TscGPMemo.MemoMouseLeave;
begin
  FMouseIn := False;
  if FVertScrollBar.Visible then
    FVertScrollBar.MouseLeave;
  if FHorzScrollBar.Visible then
    FHorzScrollBar.MouseLeave;
  DoPaint;
end;

procedure TscGPMemo.CreateWnd;
begin
  inherited;
  AdjustTextRect;
   if IsFluentUIOpaque then
  begin
    FTransparent := False;
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
       SetTimer(Handle, 101, 10, nil);
  end;
end;

procedure TscGPMemo.Loaded;
begin
  inherited;
  AdjustTextRect;
end;


function TscGPMemo.GetTextRect: TRect;
var
  Offset: Integer;
begin
  Result := ClientRect;
  Inc(Result.Left, FContentMarginLeft);
  Inc(Result.Top, FContentMarginTop);
  Dec(Result.Right, FContentMarginRight);
  Dec(Result.Bottom, FContentMarginBottom);
  Offset := Abs(Font.Height) * 2;
  case ScrollBars of
    ssHorizontal:
      begin
        Dec(Result.Bottom, FScrollBarOptions.Size);
      end;
    ssVertical:
      begin
        if BidiMode <> bdRightToLeft then
          Dec(Result.Right, FScrollBarOptions.Size)
        else
          Inc(Result.Left, FScrollBarOptions.Size);
      end;
    ssBoth:
      begin
        if BidiMode <> bdRightToLeft then
        begin
          Dec(Result.Bottom, FScrollBarOptions.Size);
          Dec(Result.Right, FScrollBarOptions.Size);
        end
        else
        begin
          Inc(Result.Left, FScrollBarOptions.Size);
          Dec(Result.Bottom, FScrollBarOptions.Size);
        end;
      end;
  end;
  if Result.Right < Result.Left + Offset then
    Result.Right := Result.Left + Offset;
  if Result.Bottom < Result.Top + Offset then
    Result.Bottom := Result.Top + Offset;
end;

procedure TscGPMemo.AdjustTextRect;
var
  R: TRect;
begin
  if not HandleAllocated then Exit;
  R := GetTextRect;
  Perform(EM_SETRECTNP, 0, Longint(@R));
end;

procedure TscGPMemo.OptionsChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TscGPMemo.ScrollOptionsChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPMemo.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
  if FOptions.FScaleFrameWidth then
     FOptions.FFrameWidth := MulDiv(FOptions.FFrameWidth, M, D);
  FOptions.ShapeCornerRadius :=  MulDiv(FOptions.ShapeCornerRadius, M, D);
  FContentMarginLeft := MulDiv(FContentMarginLeft, M, D);
  FContentMarginTop := MulDiv(FContentMarginTop, M, D);
  FContentMarginRight := MulDiv(FContentMarginRight, M, D);
  FContentMarginBottom := MulDiv(FContentMarginBottom, M, D);
  FScrollBarOptions.FSize := MulDiv(FScrollBarOptions.FSize, M, D);
  FScrollBarOptions.FBorderWidth := MulDiv(FScrollBarOptions.FBorderWidth, M, D);
  inherited;
   if not (csLoading in ComponentState) then
    SetTimer(Handle, 100, 10, nil);
end;

function TscGPMemo.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscGPMemo.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TscGPMemo.SetContentMarginLeft(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginLeft <> Value) then
  begin
    FContentMarginLeft := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPMemo.SetContentMarginTop(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginTop <> Value) then
  begin
    FContentMarginTop := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPMemo.SetContentMarginRight(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginRight <> Value) then
  begin
    FContentMarginRight := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPMemo.SetContentMarginBottom(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginBottom <> Value) then
  begin
    FContentMarginBottom := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscGPMemo.GetParentBG;
var
  R: TRect;
begin
  if FStopGetParentBG  then Exit;

  if FTransparent then
  begin
    if ParentBGBuffer = nil then
    begin
      ParentBGBuffer := TBitmap.Create;
    end;
    if Width * Height <> 0 then
    begin
      if (ParentBGBuffer.Width <> Width) or (ParentBGBuffer.Height <> Height) then
      begin
        ParentBGBuffer.Width := Width;
        ParentBGBuffer.Height := Height;
      end;
      if (Parent <> nil) and (Parent is TscCustomControl) then
        TscCustomControl(Parent).FGetControlBG := True;
      try
        if (Parent <> nil) and (Parent is TscCustomScrollBox) and
           TscCustomScrollBox(Parent).StorePaintBuffer and
           (TscCustomScrollBox(Parent).FPaintBuffer <> nil) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Width > 0) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Height > 0) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Width = TscCustomScrollBox(Parent).ClientWidth) and
           (TscCustomScrollBox(Parent).FPaintBuffer.Height = TscCustomScrollBox(Parent).ClientHeight)
        then
        begin
          R.Left := Self.Left;
          R.Top := Self.Top;
          R.Right := R.Left + Self.Width;
          R.Bottom := R.Top + Self.Height;
          ParentBGBuffer.Canvas.CopyRect(Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height),
            TscCustomScrollBox(Parent).FPaintBuffer.Canvas, R);
        end
        else
        if (Parent is TscCustomControl) and TscCustomControl(Parent).StorePaintBuffer and
           (TscCustomControl(Parent).FPaintBuffer <> nil) and
           (TscCustomControl(Parent).FPaintBuffer.Width > 0) and
           (TscCustomControl(Parent).FPaintBuffer.Height > 0) and
           (TscCustomControl(Parent).FPaintBuffer.Width = TscCustomControl(Parent).Width) and
           (TscCustomControl(Parent).FPaintBuffer.Height = TscCustomControl(Parent).Height)
        then
        begin
          R.Left := Self.Left;
          R.Top := Self.Top;
          R.Right := R.Left + Self.Width;
          R.Bottom := R.Top + Self.Height;
          ParentBGBuffer.Canvas.CopyRect(Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height),
            TscCustomControl(Parent).FPaintBuffer.Canvas, R);
        end
        else
          DrawParentBackground(Self, ParentBGBuffer.Canvas);
      finally
        if (Parent <> nil) and (Parent is TscCustomControl) then
          TscCustomControl(Parent).FGetControlBG := False;
      end;
    end;
  end
  else
  begin
    if ParentBGBuffer <> nil then
    begin
      ParentBGBuffer.Free;
      ParentBGBuffer := nil;
    end;
  end;
end;

procedure TscGPMemo.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    FStopGetParentBG := False;
    GetParentBG;
    DoPaint;
  end;
end;

procedure TscGPMemo.SetRedraw(Value: Boolean);
begin
  if HandleAllocated and Visible and not FSizeChanging then
    if Value and FStopRedraw then
    begin
      FStopRedraw := False;
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
    end
    else
    if not Value and not FStopRedraw then
    begin
      FStopRedraw := True;
      SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;
end;

procedure TscGPMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TscGPMemo.WMSIZE(var Msg: TMessage);
begin
  if FStopRedraw then
  begin
    FStopRedraw := False;
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
  end;
  FSizeChanging := True;
  inherited;
  AdjustTextRect;
  FSizeChanging := False;
end;

procedure TscGPMemo.WMMove(var Msg: TMessage);
begin
  inherited;
  if FTransparent then
  begin
    FStopGetParentBG := False;
    DoPaint;
  end;
end;

procedure TscGPMemo.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  DC: HDC;
  FCanvas: TCanvas;
begin
  DC := Message.DC;
  FCanvas := TControlCanvas.Create;
  FCanvas.Handle := DC;
  try
    if not FStopDraw then DoPaint else DrawBackground(FCanvas);
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
end;

procedure TscGPMemo.DoPaint;
var
  MyDC: HDC;
  TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  if not HandleAllocated then Exit;
  FStopDraw := True;
  begin
    HideCaret(Handle);
    try
      MyDC := GetDC(Handle);
      try
        TempDC := CreateCompatibleDC(MyDC);
        try
          TempBmp := CreateCompatibleBitmap(MyDC, Succ(Width), Succ(Height));
          try
            OldBmp := SelectObject(TempDC, TempBmp);
            SendMessage(Handle, WM_ERASEBKGND, TempDC, 0);
            SendMessage(Handle, WM_PAINT, TempDC, 0);
            BitBlt(MyDC, 0, 0, Width, Height, TempDC, 0, 0, SRCCOPY);
            SelectObject(TempDC, OldBmp);
          finally
            DeleteObject(TempBmp);
          end;
        finally
          DeleteDC(TempDC);
        end;
      finally
        ReleaseDC(Handle, MyDC);
      end;
    finally
      ShowCaret(Handle);
    end;
  end;
  FStopDraw := False;
end;

procedure TscGPMemo.DoPaint2(DC: HDC);
var
  MyDC: HDC;
  TempDC: HDC;
  OldBmp, TempBmp: HBITMAP;
begin
  if not HandleAllocated then Exit;
  FStopDraw := True;
  try
    MyDC := DC;
    try
      TempDC := CreateCompatibleDC(MyDC);
      try
        TempBmp := CreateCompatibleBitmap(MyDC, Succ(Width), Succ(Height));
        try
          OldBmp := SelectObject(TempDC, TempBmp);
          SendMessage(Handle, WM_ERASEBKGND, TempDC, 0);
          SendMessage(Handle, WM_PAINT, TempDC, 0);
          BitBlt(MyDC, 0, 0, Width, Height, TempDC, 0, 0, SRCCOPY);
          SelectObject(TempDC, OldBmp);
        finally
          DeleteObject(TempBmp);
        end;
      finally
        DeleteDC(TempDC);
      end;
    finally
      ReleaseDC(Handle, MyDC);
    end;
  finally
    FStopDraw := False;
  end;
end;

procedure TscGPMemo.GetMemoScrollInfo;
var
  SF: ScrollInfo;
  sMin, SMax, SPos, sPage: Integer;
begin
  // vertical
  if ((ScrollBars = ssBoth) or (Self.ScrollBars = ssVertical)) and (FVertScrollBar <> nil) then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Self.Handle, SB_VERT, SF);
    SMin := 0;
    SMax := SF.nMax;
    SPos := SF.nPos;
    SPage := SF.nPage;
    if SMax + 1 > SPage then
      FVertScrollBar.SetParams(SMin, SMax, SPage, SPos, False)
    else
      FVertScrollBar.SetParams(0, 0, 0, 0, False);
  end;
  if ((ScrollBars = ssBoth) or (Self.ScrollBars = ssHorizontal)) and (FHorzScrollBar <> nil) then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Self.Handle, SB_HORZ, SF);
    SMin := 0;
    SMax := SF.nMax;
    SPos := SF.nPos;
    SPage := SF.nPage;
    if SMax + 1 > SPage then
      FHorzScrollBar.SetParams(SMin, SMax, SPage, SPos, False)
    else
      FHorzScrollBar.SetParams(0, 0, 0, 0, False);
  end;
end;

procedure TscGPMemo.DrawMemoShape(ACanvas: TCanvas);
var
  R, TR, BR: TRect;
  G: TGPGraphics;
  B: TGPBrush;
  P: TGPPen;
  FramePath, FillPath: TGPGraphicsPath;
  FillR, FrameR, R1: TGPRectF;
  FrameColor, FillColor: Cardinal;
  l, t, w, h, d: Single;
  C1, C2: Cardinal;
  FCtrlState: TscsCtrlState;
  SH, SV: Boolean;
  B1: TGPSolidBrush;
  Offset: Integer;
begin
  R := Rect(0, 0, Width, Height);
  TR := R;
  if not FTransparent then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;
  FCtrlState := scsNormal;
  if not Enabled then
    FCtrlState := scsDisabled
  else
  if Focused then
    FCtrlState := scsFocused
  else
  if FMouseIn then
    FCtrlState := scsHot;

  FOptions.State := FCtrlState;

  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  P := TGPPen.Create(0, FOptions.FrameWidth);
  FramePath := TGPGraphicsPath.Create;
  FillPath := TGPGraphicsPath.Create;
  // colors
  FrameColor := ColorToGPColor(Options.FrameColor, Options.FrameColorAlpha);
  FillColor := ColorToGPColor(Options.Color, Options.ColorAlpha);
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
  if FOptions.ShapeFillStyle = scgpsfColor then
    B := TGPSolidBrush.Create(FillColor)
  else
  begin
    C1 := ColorToGPColor(LighterColor(FOptions.Color, FOptions.FShapeFillGradientBeginColorOffset), 
      Options.ColorAlpha);
    C2 := ColorToGPColor(DarkerColor(FOptions.Color, FOptions.FShapeFillGradientEndColorOffset),
      Options.ColorAlpha);
    R1 := FillR;
    InflateGPRect(R1, 1, 1);
    B := TGPLinearGradientBrush.Create(R1, C1, C2, FOptions.FShapeFillGradientAngle);
  end;
  // draw
  try
    if FOPtions.ShapeCornerRadius = 0 then
    begin
      G.FillRectangle(B, FillR);
      if (FOptions.FrameWidth <> 0) then
        G.DrawRectangle(P, FrameR)
    end
    else
    begin
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
      FillPath.AddArc(l, t, d, d, 180, 90);
      FillPath.AddArc(l + w - d, t, d, d, 270, 90);
      FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
      FillPath.AddArc(l, t + h - d, d, d, 90, 90);
      FillPath.CloseFigure;
      G.FillPath(B, FillPath);
      // frame
      l := FrameR.X;
      t := FrameR.y;
      w := FrameR.Width;
      h := FrameR.Height;
      d := Options.ShapeCornerRadius * 2;
      FramePath.StartFigure;
      FramePath.AddArc(l, t, d, d, 180, 90);
      FramePath.AddArc(l + w - d, t, d, d, 270, 90);
      FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
      FramePath.AddArc(l, t + h - d, d, d, 90, 90);
      FramePath.CloseFigure;
      G.DrawPath(P, FramePath);
    end;


    // draw scrollbars
    GetMemoScrollInfo;

    R := Rect(0, 0, Width, Height);
    if FOptions.FrameWidth > 0 then
      InflateRect(R, -FOptions.FrameWidth, -FOptions.FrameWidth);

    Offset := Max(FScrollBarOptions.Size, FOptions.ShapeCornerRadius);

    SH := False;
    SV := False;

    if BidiMode <> bdRightToLeft then
    begin
      if ((ScrollBars = ssBoth) or (Self.ScrollBars = ssVertical)) and (FVertScrollBar <> nil) and FVertScrollBar.Visible then
      begin
       if ScrollBars = ssBoth then
         BR := Rect(
           R.Right - FScrollBarOptions.Size,
           R.Top + FOptions.ShapeCornerRadius,
           FScrollBarOptions.Size,
           R.Height - Offset - FOptions.ShapeCornerRadius)
       else
         BR := Rect(
           R.Right - FScrollBarOptions.Size,
           R.Top + FOptions.ShapeCornerRadius,
           FScrollBarOptions.Size,
           R.Height - FOptions.ShapeCornerRadius * 2);
       FVertScrollBar.BoundsRect := BR;
       FVertScrollBar.Draw(G);
       SV := True;
      end;

      if ((ScrollBars = ssBoth) or (Self.ScrollBars = ssHorizontal)) and (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
      begin
        if ScrollBars = ssBoth then
          BR := Rect
            (R.Left + FOptions.ShapeCornerRadius,
             R.Bottom - FScrollBarOptions.Size,
             R.Width - Offset - FOptions.ShapeCornerRadius,
             FScrollBarOptions.Size)
        else
          BR := Rect(
            R.Left + FOptions.ShapeCornerRadius,
            R.Bottom - FScrollBarOptions.Size,
            R.Width - FOptions.ShapeCornerRadius * 2,
            FScrollBarOptions.Size);

        FHorzScrollBar.BoundsRect := BR;
        FHorzScrollBar.Draw(G);
        SH := True;
      end;
    end
    else
    begin
      if ((ScrollBars = ssBoth) or (Self.ScrollBars = ssVertical)) and (FVertScrollBar <> nil) and FVertScrollBar.Visible then
      begin
       if ScrollBars = ssBoth then
         BR := Rect(
           R.Left,
           R.Top + FOptions.ShapeCornerRadius,
           FScrollBarOptions.Size,
           R.Height - Offset - FOptions.ShapeCornerRadius)
       else
         BR := Rect(
           R.Left,
           R.Top + FOptions.ShapeCornerRadius,
           FScrollBarOptions.Size,
           R.Height - FOptions.ShapeCornerRadius * 2);
       FVertScrollBar.BoundsRect := BR;
       FVertScrollBar.Draw(G);
       SV := True;
      end;

      if ((ScrollBars = ssBoth) or (Self.ScrollBars = ssHorizontal)) and (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
      begin
        if ScrollBars = ssBoth then
          BR := Rect
            (R.Left + Max(FScrollBarOptions.Size, FOptions.ShapeCornerRadius),
             R.Bottom - FScrollBarOptions.Size,
             R.Width - Offset - FOptions.ShapeCornerRadius,
             FScrollBarOptions.Size)
        else
          BR := Rect(
            R.Left + FOptions.ShapeCornerRadius,
            R.Bottom - FScrollBarOptions.Size,
            R.Width - FOptions.ShapeCornerRadius * 2,
            FScrollBarOptions.Size);

        FHorzScrollBar.BoundsRect := BR;
        FHorzScrollBar.Draw(G);
        SH := True;
      end;
    end;

    if SV and SH and (FOptions.ShapeCornerRadius = 0) and
       (FScrollBarOptions.FillColor <> clNone) and
       (FScrollBarOptions.FillColorAlpha > 0)
    then
    begin
      C1 := ColorToGPColor(GetStyleColor(FScrollBarOptions.FillColor),
        FScrollBarOptions.FillColorAlpha);
      B1 := TGPSolidBrush.Create(C1);
      FillR.X := FVertScrollBar.BoundsRect.Left;
      FillR.Y := FVertScrollBar.BoundsRect.Top + FVertScrollBar.BoundsRect.Bottom;
      FillR.Width := FVertScrollBar.BoundsRect.Right;
      FillR.Height := FillR.Width;
      G.FillRectangle(B1, FillR);
      B1.Free;
    end;

  finally
    G.Free;
    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;
end;

procedure TscGPMemo.DrawBackGround(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Brush.Color := GetStyleColor(Self.Color);
    if FTransparent then
    begin
      GetParentBG;
      if ParentBGBuffer <> nil then
        ACanvas.Draw(0, 0, ParentBGBuffer);
    end
    else
      FillRect(ClientRect);
    DrawMemoShape(ACanvas);
  end;
end;

procedure TscGPMemo.WMPaint(var Message: TWMPaint);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R: TRect;
  S: string;
  FCanvas: TControlCanvas;
  DC: HDC;
  PS: TPaintStruct;
  VisibleLines: Integer;
  i, P: Integer;
  LineHeight: Integer;

function GetVisibleLines: Integer;
var
  R: TRect;
  C: TCanvas;
  DC: HDC;
begin
  C := TCanvas.Create;
  C.Font.Assign(Font);
  DC := GetDC(0);
  C.Handle := DC;
  R := GetClientRect;
  LineHeight := C.TextHeight('Wq');
  if LineHeight <> 0
  then
    Result := R.Height div LineHeight
  else
    Result := 1;
  ReleaseDC(0, DC);
  C.Free;
end;

begin
  if Enabled
  then
    begin
      if not FStopDraw
      then
        begin
          DC := Message.DC;
          if DC = 0 then DC := BeginPaint(Handle, PS);
          DoPaint2(DC);
          ShowCaret(Handle);
          if Message.DC = 0 then EndPaint(Handle, PS);
        end
      else
       inherited;
    end
  else
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
      DC := Message.DC;
      if DC = 0 then DC := BeginPaint(Handle, PS);
      FCanvas.Handle := DC;
      //
      with FCanvas do
      begin
        Font := Self.Font;
        if (seFont in StyleElements) and IsCustomStyle then
          Font.Color := ColorToRGB(GetEditTextColor(scsDisabled))
        else
          Font.Color := clGrayText;
        Brush.Style := bsClear;
      end;
      // draw text
      VisibleLines := GetVisibleLines;
      P := SendMessage(Self.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
      R := GetTextRect;
      for i := P  to P + VisibleLines - 2 do
       if i < Lines.Count
       then
         begin
           S := Lines[i];
           DrawText(FCanvas.Handle, PChar(S), Length(S), R, Alignments[Alignment]);
           Inc(R.Top, LineHeight);
         end;
      FCanvas.Handle := 0;
      FCanvas.Free;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
end;

procedure TscGPMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    ExStyle := Exstyle and not WS_EX_Transparent;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
    if IsFluentUIOpaque then
      ExStyle := Exstyle or WS_EX_LAYERED;
  end;
end;

procedure TscGPMemo.WMVSCROLL(var Message: TWMVScroll);
begin
  SetRedraw(False);
  inherited;
  SetRedraw(True);
  DoPaint;
end;

procedure TscGPMemo.WMHSCROLL(var Message: TWMHScroll);
begin
  SetRedraw(False);
  inherited;
  SetRedraw(True);
  DoPaint;
end;

procedure TscGPMemo.Change;
begin
  inherited;
  SetRedraw(True);
  FStopGetParentBG := True;
  DoPaint;
  FStopGetParentBG := False;
end;

procedure TscGPMemo.WMCOMMAND;
begin
  inherited;
  if (Message.NotifyCode = EN_HSCROLL) or
     (Message.NotifyCode = EN_VSCROLL)
  then
    begin
      DoPaint;
      SetRedraw(False);
    end;
end;

procedure TscGPMemo.WndProc(var Message: TMessage);
var
  WParam: Integer;
  P: TPoint;
begin
  if Message.Msg = WM_SIZE then
    FStopGetParentBG := False;
  case Message.Msg of
    WM_MOUSEWHEEL:
      begin
        if TWMMOUSEWHEEL(Message).WheelDelta > 0
        then
          WParam := MakeWParam(SB_LINEUP, 0)
        else
          WParam := MakeWParam(SB_LINEDOWN, 0);
        SendMessage(Handle, WM_VSCROLL, WParam, 0);
      end;
    CN_CtlColorStatic:
    with TWMCtlColorStatic(Message) do
    begin
      SetBkMode(ChildDC, WinApi.Windows.Transparent);
      if (seFont in StyleElements) and IsCustomStyle then
      begin
        if Enabled then
          SetTextColor(ChildDC,ColorToRGB(GetStyleColor(Self.Font.Color)))
        else
        if Transparent then
          SetTextColor(ChildDC,
            ColorToRGB(scDrawUtils.GetCheckBoxTextColor(scsNormal)))
        else
          SetTextColor(ChildDC, ColorToRGB(GetStyleColor(clWindowText)));
      end
      else
        SetTextColor(ChildDC, ColorToRGB(Font.Color));
      Result := GetStockObject(NULL_BRUSH);
    end;
    CN_CtlColorEdit:
    with TWMCTLCOLOREDIT(Message) do
    begin
      SetBkMode(ChildDC, WinApi.Windows.Transparent);
      if (seFont in StyleElements) and IsCustomStyle then
      begin
        if Enabled then
          SetTextColor(ChildDC,ColorToRGB(GetStyleColor(Self.Font.Color)))
        else
        if Transparent then
          SetTextColor(ChildDC,
            ColorToRGB(scDrawUtils.GetCheckBoxTextColor(scsNormal)))
        else
          SetTextColor(ChildDC, ColorToRGB(GetStyleColor(clWindowText)));
      end
      else
        SetTextColor(ChildDC, ColorToRGB(Font.Color));
      Result := GetStockObject(NULL_BRUSH);
    end
    else
      inherited;
  end;
  case Message.Msg of
    WM_SETFOCUS:
    begin
      if FScrollCaptured then
      begin
        ReleaseCapture;
        FScrollCaptured := False;
        if FVertScrollBar.Visible then
          FVertScrollBar.MouseUp(-1, -1);
        if FHorzScrollBar.Visible then
          FHorzScrollBar.MouseUp(-1, -1);
      end;
    end;
    WM_NCMOUSEMOVE:
    begin
      P.X := TWMNCLBUTTONDOWN(Message).XCursor;
      P.Y := TWMNCLBUTTONDOWN(Message).YCursor;
      P := ScreenToClient(P);
      if FVertScrollBar.Visible then
        FVertScrollBar.MouseMove(P.X, P.Y);
       if FHorzScrollBar.Visible then
        FHorzScrollBar.MouseMove(P.X, P.Y);
    end;
    WM_NCLBUTTONDOWN:
    begin
      P.X := TWMNCLBUTTONDOWN(Message).XCursor;
      P.Y := TWMNCLBUTTONDOWN(Message).YCursor;
      P := ScreenToClient(P);
      if FVertScrollBar.Visible and
        FVertScrollBar.ScrollBarRect.Contains(P)
      then
      begin
        FVertScrollBar.MouseDown(P.X, P.Y);
        FScrollCaptured := True;
        SetCapture(Handle);
      end
      else
      if FHorzScrollBar.Visible and
        FHorzScrollBar.ScrollBarRect.Contains(P)
      then
      begin
        FHorzScrollBar.MouseDown(P.X, P.Y);
        FScrollCaptured := True;
        SetCapture(Handle);
      end;
    end;
    WM_LBUTTONUP:
    begin
      if FScrollCaptured then
      begin
        ReleaseCapture;
        FScrollCaptured := False;
        P.X := TWMLBUTTONUP(Message).XPos;
        P.Y := TWMLBUTTONUP(Message).YPos;
        if FVertScrollBar.Visible then
          FVertScrollBar.MouseUp(P.X, P.Y);
        if FHorzScrollBar.Visible then
          FHorzScrollBar.MouseUp(P.X, P.Y);
      end;
      FSelPos := 0;
      FSelLen := 0;
    end;
    WM_MOUSEMOVE:
    begin
      if FScrollCaptured then
      begin
        P.X := TWMMOUSEMOVE(Message).XPos;
        P.Y := TWMMOUSEMOVE(Message).YPos;
        if FVertScrollBar.Visible then
          FVertScrollBar.MouseMove(P.X, P.Y);
        if FHorzScrollBar.Visible then
          FHorzScrollBar.MouseMove(P.X, P.Y);
      end
      else
      if (WinApi.Windows.GetCapture = Handle) and
         ((FSelPos <> SelStart) or (FSelLen <> SelLength))
      then
      begin
        FSelPos := SelStart;
        FSelLen := SelLength;
        SetRedraw(True);
        FStopGetParentBG := True;
        DoPaint;
        FStopGetParentBG := False;
      end;
    end;
    WM_CHAR, WM_KEYDOWN, WM_KEYUP, WM_NCPAINT:
    begin
      SetRedraw(True);
      if Message.Msg <> WM_NCPAINT then
      begin
        FStopGetParentBG := True;
      end;
      DoPaint;
      FStopGetParentBG := False;
    end;
    WM_TIMER:
      if (TWMTimer(Message).TimerID = 101) and IsFluentUIOpaque then
      begin
        KillTimer(Handle, 101);
        RedrawWindow(Handle, nil, 0,
          RDW_INVALIDATE + RDW_ERASE + RDW_FRAME + RDW_ALLCHILDREN + RDW_UPDATENOW);
      end;
  end;
end;

procedure TscGPMemo.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  if FTransparent then
  begin
    FStopGetParentBG := False;
    DoPaint;
  end;
end;

procedure TscGPMemo.WMNCPAINT(var Message: TWMNCPAINT);
begin
  Message.Result := 1;
end;

procedure TscGPMemo.WMNCCALCSIZE(var Message: TWMNCCalcSize);
begin
  Message.Result := 0;
end;

procedure TscGPMemo.CMSENCPaint(var Message: TMessage);
begin
  Message.Result := SE_RESULT;
end;

procedure TscGPMemo.CMSEPaint(var Message: TMessage);
begin
  SendMessage(Handle, WM_PAINT, Message.WParam, 0);
end;

constructor TscGPGearActivityIndicator.Create;
begin
  inherited;
  FAnimationTimer := nil;
  FAnimationAngle := 0;
  FAnimationAcceleration := False;
  FBigAnimationAngle := 0;
  FActive := False;
  FKind := scgpgkOneGear;
  FGlyphColor := clBtnText;
  FGlyphColorAlpha := 200;
  FGlyphThickness := 2;
  Width := 100;
  Height := 100;
end;

destructor TscGPGearActivityIndicator.Destroy;
begin
  if FAnimationTimer <> nil then
    FAnimationTimer.Free;
  inherited;
end;

procedure TscGPGearActivityIndicator.OnAnimationTimer(Sender: TObject);
var
  I: Integer;
begin
  if FAnimationAcceleration then
  begin
    FBigAnimationAngle := FBigAnimationAngle + 5;
    if FBigAnimationAngle >= 360 then
     FBigAnimationAngle := 0;
    I := Abs(Round(40 * (FBigAnimationAngle / 360)));
    if I < 5 then I := 5;

    if FAnimationTimer <> nil  then
      FAnimationTimer.Interval := I;

    FAnimationAngle := FAnimationAngle + 5;
  end
  else
  begin
    if (FAnimationTimer <> nil) and (FAnimationTimer.Interval <> 40) then
      FAnimationTimer.Interval := 40;
    FAnimationAngle := FAnimationAngle + 5;
  end;

  if FAnimationAngle >= 45 then
    FAnimationAngle := 0;

  RePaintControl;
end;

procedure TscGPGearActivityIndicator.StartAnimation;
begin
  if FAnimationTimer <> nil then Exit;
  FAnimationAngle := 0;
  FBigAnimationAngle := 0;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := 40;
  FAnimationTimer.Enabled := True;
end;

procedure TscGPGearActivityIndicator.StopAnimation;
begin
  if FAnimationTimer = nil then Exit;
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Free;
  FAnimationTimer := nil;
end;

procedure TscGPGearActivityIndicator.SetGlyphThickness(Value: Byte);
begin
  if (FGlyphThickness <> Value) and (Value > 0) then
  begin
    FGlyphThickness := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearActivityIndicator.SetKind(Value: TscGPGearActivityIndicatorKind);
begin
  if FKind <> Value  then
  begin
    FKind := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearActivityIndicator.SetGlyphColor(Value: TColor);
begin
  if FGlyphColor <> Value  then
  begin
    FGlyphColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearActivityIndicator.SetGlyphColorAlpha(Value: Byte);
begin
  if FGlyphColorAlpha <> Value  then
  begin
    FGlyphColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPGearActivityIndicator.SetActive;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      StartAnimation
    else
      StopAnimation;
    RePaintControl;
  end;
end;

procedure TscGPGearActivityIndicator.Activate;
begin
  Active := True;
end;

procedure TscGPGearActivityIndicator.Deactivate;
begin
  Active := False;
end;

procedure TscGPGearActivityIndicator.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TscGPGearActivityIndicator.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  G: TGPGraphics;
  R: TGPRectF;
  GlyphColor: Cardinal;
begin
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  R.X := 0;
  R.Y := 0;
  R.Width := Height;
  R.Height := Height;
  if Enabled then
    GlyphColor := ColorToGPColor(GetStyleColor(FGlyphColor), FGlyphColorAlpha)
  else
    GlyphColor := ColorToGPColor(GetStyleColor(FGlyphColor), FGlyphColorAlpha div 2);
  case FKind of
    scgpgkOneGear:
      begin
        scGPUtils.GPDrawGearGlyphAngle(G, R, GlyphColor, FAnimationAngle, FScaleFactor, FGlyphThickness);
      end;
    scgpgkTwoGears:
      begin
        R.Width := R.Width * 0.6;
        R.Height := R.Width;
        R.Y := Height - R.Height;
        scGPUtils.GPDrawGearGlyphAngle(G, R, GlyphColor, FAnimationAngle, FScaleFactor, FGlyphThickness);
        R.X := Width - R.Width;
        R.Y := 0;
        scGPUtils.GPDrawGearGlyphAngle(G, R, GlyphColor, -FAnimationAngle - 22, FScaleFactor, FGlyphThickness);
      end;
  end;

  G.Free;
end;

procedure TscGPGearActivityIndicator.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPGearActivityIndicator.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

constructor TscGPScrollPanel.Create;
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FCanScroll := True;
  FHorzScrollDirection := scgspsdLeftToRight;
  FVertScrollDirection := scgspsdTopToBottom;
  FScrollButtonArrowWidth := 2;
  FFillStyle := scgpsfColor;
  FFillGradientAngle := 90;
  FFillGradientBeginAlpha := 255;
  FFillGradientEndAlpha := 255;
  FFillGradientBeginColorOffset := 25;
  FFillGradientEndColorOffset := 25;
  FFillColor := clNone;
  FFillColorAlpha := 255;
  FFillColor2 := clNone;
  FBGStyleKind := scgspbFormBackground;

  FScrollButtonWidth := 17;
  FTransparentBackground := False;
  FCapturedButton := 0;
  FWallpapers := nil;
  FCustomImages := nil;
  FCustomImageIndex := -1;
  FWallpaperIndex := -1;
  FHotScroll := False;
  TimerMode := 0;
  FScrollOffset := 0;
  FScrollTimerInterval := 50;
  Width := 150;
  Height := 30;
  Buttons[0].Visible := False;
  Buttons[1].Visible := False;
  FVSizeOffset := 0;
  FHSizeOffset := 0;
  SMax := 0;
  SPosition := 0;
  SOldPosition := 0;
  SPage := 0;
  FAutoSize := False;
  FTouchScroll := False;
end;

procedure TscGPScrollPanel.DrawBackground(ACanvas: TCanvas);
begin
  if Buttons[0].Visible then
  begin
    if FScrollType = scstHorizontal then
      FBackgroundLeft := -Buttons[0].Rect.Width
    else
      FBackgroundTop := -Buttons[0].Rect.Height;
  end
  else
  begin
    FBackgroundLeft := 0;
    FBackgroundTop := 0;
  end;
  inherited;
end;

procedure TscGPScrollPanel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FScrollButtonArrowWidth := MulDiv(FScrollButtonArrowWidth , M, D);
  ScrollButtonWidth := MulDiv(FScrollButtonWidth, M, D);
  if FScrollOffset > 0 then
    FScrollOffset := MulDiv(FScrollOffset , M, D);
end;

procedure TscGPScrollPanel.SetScrollButtonArrowWidth(Value: Integer);
begin
  if (FScrollButtonArrowWidth <> Value) and (Value > 0) then
  begin
    FScrollButtonArrowWidth := Value;
    UpdateNC;
  end;
end;

procedure TscGPScrollPanel.InitTouch;
begin
  if FTouchScroll then
  begin
    Touch.InteractiveGestures := Touch.InteractiveGestures + [igPan, igPressAndTap];
    case FScrollType of
      scstHorizontal:
      begin
        Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions +
          [igoPanSingleFingerHorizontal, igoPanInertia] - [igoPanSingleFingerVertical];
      end;
      scstVertical:
      begin
        Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions +
          [igoPanSingleFingerVertical, igoPanInertia] - [igoPanSingleFingerHorizontal];
      end;
    end;
  end
  else
  begin
    Touch.InteractiveGestures := Touch.InteractiveGestures - [igPan];
    Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions -
      [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia];
  end;
end;

procedure TscGPScrollPanel.SetTouchScroll(Value: Boolean);
begin
  if FTouchScroll <> Value then
  begin
    FTouchScroll := Value;
    InitTouch;
  end;
end;

procedure TscGPScrollPanel.WMDESTROY(var Message: TMessage);
begin
  if TimerMode <> 0 then
    KillTimer(Handle, 1);
  if FMouseTimerActive then
    KillTimer(Handle, 2);
  inherited;
end;

procedure TscGPScrollPanel.AlignControls(AControl: TControl; var ARect: TRect);
begin
  inherited;
  if not FUpdatingScrollInfo then
    GetScrollInfo;
end;

procedure TscGPScrollPanel.WMSETCURSOR(var Message: TWMSetCursor);
var
  HitTest: SmallInt;
begin
  if (Message .HitTest = HTBUTTON1) or
     (Message.HitTest = HTBUTTON2)
  then
    begin
      HitTest := Message.HitTest;
      Message.HitTest :=  HTCLIENT;
    end
  else
    HitTest := 0;
  inherited;
  if HitTest <> 0 then
    Message.HitTest := HitTest;
end;

procedure TscGPScrollPanel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
  G: TGPGraphics;
  B: TGPBrush;
  C, C2, Col: Cardinal;
  FillR, R2: TGPRectF;
begin
  R := ClientRect;
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex)
     and FWallpapers.IsSolidDrawing(FWallpaperIndex) then
  begin
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
    if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomImageIndex)
    then
      FCustomImages.Draw(ACanvas, R, FCustomImageIndex, FScaleFactor);
  end
  else
  begin
    case FBGStyleKind of
      scgspbPanel:
      with ACanvas do
      begin
        if (seClient in StyleElements) and IsCustomStyle then
          Brush.Color := GetStyleColor(Self.Color)
        else
          Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(R);
      end;
      scgspbFormBackground:
      begin
        DrawFormBackground(ACanvas, R);
      end;
    end;
  end;

  // fill
  if (FFillColor <> clNone) and (FFillColorAlpha <> 0) then
  begin
    FillR := RectToGPRect(R);
    G := TGPGraphics.Create(ACanvas.Handle);
    G.SetSmoothingMode(SmoothingModeHighQuality);
    G.SetPixelOffsetMode(PixelOffsetModeHalf);
    if FFillStyle = scgpsfColor then
    begin
      C := ColorToGPColor(GetStyleColor(FFillColor), FFillColorAlpha);
      B := TGPSolidBrush.Create(C);
    end
    else
    begin
      Col := GetStyleColor(FFillColor);
      C := ColorToGPColor(LighterColor(Col, FFillGradientBeginColorOffset),
        FFillGradientBeginAlpha);
      if FFillColor2 <> clNone then
         Col := GetStyleColor(FFillColor2);
      C2 := ColorToGPColor(DarkerColor(Col, FFillGradientEndColorOffset),
        FFillGradientEndAlpha);
      R2 := FillR;
      InflateGPRect(R2, 1, 1);
      B := TGPLinearGradientBrush.Create(R2, C, C2, FFillGradientAngle);
    end;
    G.FillRectangle(B, FillR);
    B.Free;
    G.Free;
  end;
  //

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomImageIndex)
  then
    FCustomImages.Draw(ACanvas, R, FCustomImageIndex, FScaleFactor);

  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex)
  then
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
end;

procedure TscGPScrollPanel.SetHorzScrollDirection(Value: TscGPScrollPanelHorzDirection);
begin
  if Value <> FHorzScrollDirection then
  begin
    FHorzScrollDirection := Value;
    GetScrollInfo;
    UpdateNC;
    RePaintControl;
  end;
end;

procedure TscGPScrollPanel.SetVertScrollDirection(Value: TscGPScrollPanelVertDirection);
begin
  if Value <> FVertScrollDirection then
  begin
    FVertScrollDirection := Value;
    GetScrollInfo;
    UpdateNC;
    RePaintControl;
  end;
end;

procedure TscGPScrollPanel.SetScrollButtonWidth(Value: Integer);
begin
  if Value >= ScrollButtonSize then
  begin
    FScrollButtonWidth := Value;
    UpdateNC;
    RePaintControl;
  end;
end;

procedure TscGPScrollPanel.SetCustomImageIndex(Value: Integer);
begin
  if FCustomImageIndex <> Value then
  begin
    FCustomImageIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.CMSENCPaint(var Message: TMessage);
var
  C: TCanvas;
begin
  if (Message.wParam <> 0) and  Buttons[0].Visible then
  begin
    C := TCanvas.Create;
    C.Handle := Message.WParam;
    try
      DrawButton(C, 0);
      DrawButton(C, 1);
    finally
      C.Handle := 0;
      C.Free;
    end;
    Message.Result := SE_RESULT;
  end;
end;

procedure TscGPScrollPanel.CMGesture(var Message: TCMGesture);
var
  Offset: Integer;
begin
  inherited;
  if not FCanScroll then Exit;
  if not Self.Buttons[0].Visible then Exit;
  if FScrollType = scstVertical then
  begin
    if gfBegin in Message.Info^.Flags
    then
      FTouchBegin := Message.Info^.Location.Y
    else
    begin
      FTouchEnd := Message.Info^.Location.Y;
      Offset := FTouchEnd - FTouchBegin;
      if FVertScrollDirection = scgspsdBottomToTop then
        Offset := -Offset;

      SPosition := SPosition - Offset;
      if Offset > 0 then
      begin
        if SPosition < 0 then SPosition := 0;
        if (SPosition - SOldPosition <> 0)
        then
          begin
            VScrollControls(SPosition - SOldPosition);
            GetVRange;
          end
      end
      else
      begin
        if SPosition > SMax - SPage + 1 then
          SPosition := SMax - SPage + 1;
        if (SPosition - SOldPosition <> 0)
        then
          begin
            VScrollControls(SPosition - SOldPosition);
            GetVRange;
          end
      end;
      FTouchBegin := FTouchEnd;
    end;
  end
  else
  begin
    if gfBegin in Message.Info^.Flags
    then
      FTouchBegin := Message.Info^.Location.X
    else
    begin
      FTouchEnd := Message.Info^.Location.X;
      Offset := FTouchEnd - FTouchBegin;
      if FHorzScrollDirection = scgspsdRightToLeft then
        Offset := -Offset;

      SPosition := SPosition - Offset;
      if Offset > 0 then
      begin
        if SPosition < 0 then SPosition := 0;
        if (SPosition - SOldPosition <> 0)
        then
          begin
            HScrollControls(SPosition - SOldPosition);
            GetHRange;
          end
      end
      else
      begin
        if SPosition > SMax - SPage + 1 then
          SPosition := SMax - SPage + 1;
        if (SPosition - SOldPosition <> 0)
        then
          begin
            HScrollControls(SPosition - SOldPosition);
            GetHRange;
          end
      end;
      FTouchBegin := FTouchEnd;
    end;
  end;
end;

procedure TscGPScrollPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscGPScrollPanel.SetBGStyleKind(Value: TscGPScrollPanelBGStyleKind);
begin
  if FBGStyleKind <> Value then
  begin
    if FBGStyleKind = scgspbTransparent then
    begin
      FTransparentBackground := False;
      GetParentBG;
    end;
    FBGStyleKind := Value;
    if FBGStyleKind = scgspbTransparent then
    begin
      FTransparentBackground := True;
      GetParentBG;
    end;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetPosition(const Value: integer);
begin
  if Value <> SPosition
  then
    begin
      SPosition := Value;
      GetScrollInfo;
    end;
end;

procedure TscGPScrollPanel.Loaded;
begin
  inherited Loaded;
  if FAutoSize then UpDateSize;
  if FCanScroll then
    GetScrollInfo;
end;

destructor TscGPScrollPanel.Destroy;
begin
  inherited;
end;

procedure TscGPScrollPanel.UpDateSize;
begin
  SetBounds(Left, Top, Width, Height);
end;

procedure TscGPScrollPanel.StartTimer;
begin
  KillTimer(Handle, 1);
  if not FStartScroll and not FHotScroll then
    SetTimer(Handle, 1, 300, nil)
  else
    SetTimer(Handle, 1, Self.ScrollTimerInterval, nil);
end;

procedure TscGPScrollPanel.StopTimer;
begin
  KillTimer(Handle, 1);
  TimerMode := 0;
end;

procedure TscGPScrollPanel.WMTimer;
var
  P: TPoint;
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    if not FStartScroll and not FHotScroll then
    begin
      FStartScroll := True;
      StartTimer;
    end
    else
    case TimerMode of
      1: ButtonClick(0);
      2: ButtonClick(1);
    end;
  end;
  if Message.TimerID = 2 then
  begin
    GetCursorPos(P);
    if (WindowFromPoint(P) <> Handle) or (FCapturedButton > 0) then
    begin
      FMouseTimerActive := False;
      KillTimer(Handle, 2);
      if FCapturedButton = 0 then
        DoMouseLeave;
    end;
  end;
end;

procedure TscGPScrollPanel.AdjustClientRect(var Rect: TRect);
var
  RLeft, RTop, VMax, HMax: Integer;
begin
  if FScrollType = scstHorizontal then
  begin
    RTop := 0;
    if FHorzScrollDirection = scgspsdLeftToRight then
    begin
      RLeft := - SPosition;
      HMax := Max(SMax, ClientWidth);
    end
    else
    begin
      RLeft := SPosition;
      HMax := ClientWidth;
    end;
    VMax := ClientHeight;
  end
  else
  begin
    RLeft := 0;
    if FVertScrollDirection = scgspsdTopToBottom then
    begin
      RTop := - SPosition;
      VMax := Max(SMax, ClientHeight);
    end
    else
    begin
      RTop := SPosition;
      VMax := ClientHeight;
    end;

    HMax := ClientWidth;
  end;
  Rect := Bounds(RLeft, RTop,  HMax, VMax);
  inherited AdjustClientRect(Rect);
end;

procedure TscGPScrollPanel.VScrollControls(AOffset: Integer);
begin
  if FVertScrollDirection = scgspsdTopToBottom then
    ScrollBy(0, -AOffset)
  else
    ScrollBy(0, AOffset);
end;

procedure TscGPScrollPanel.ScrollBy(DeltaX, DeltaY: Integer);
begin
  FUpdatingScrollInfo := True;
  Perform(WM_SETREDRAW, 0, 0);
  inherited;
  Perform(WM_SETREDRAW, 1, 0);
  RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or
    RDW_ALLCHILDREN or RDW_UPDATENOW or RDW_FRAME);
  FUpdatingScrollInfo := False;
end;

procedure TscGPScrollPanel.HScrollControls(AOffset: Integer);
begin
  if FHorzScrollDirection = scgspsdLeftToRight then
    ScrollBy(-AOffset, 0)
  else
    ScrollBy(AOffset, 0);
end;

procedure TscGPScrollPanel.SetBounds;
var
  i, MaxHeight, MaxWidth, OldHeight, OldWidth: Integer;
begin
  OldWidth := Width;
  OldHeight := Height;
  FUpdatingScrollInfo := True;
  if FAutoSize
  then
    if FScrollType = scstHorizontal
    then
      begin
        MaxHeight := 0;
        if ControlCount > 0
        then
          for i := 0 to ControlCount - 1 do
          with Controls[i] do
          begin
            if Visible
            then
            if Top + Height > MaxHeight then MaxHeight := Top + Height;
          end;
        if MaxHeight <> 0 then AHeight := MaxHeight;
      end
    else
      begin
        MaxWidth := 0;
        if ControlCount > 0
        then
          for i := 0 to ControlCount - 1 do
          with Controls[i] do
          begin
            if Visible
            then
            if Left + Width > MaxWidth then MaxWidth := Left + Width;
          end;
        if MaxWidth <> 0 then AWidth := MaxWidth;
      end;
  inherited;
  if (OldWidth <> Width)
  then
    begin
      if (OldWidth < Width) and (OldWidth <> 0)
      then FHSizeOffset := Width - OldWidth
      else FHSizeOffset := 0;
    end
  else
    FHSizeOffset := 0;
  if (OldHeight <> Height)
  then
    begin
      if (OldHeight < Height) and (OldHeight <> 0)
      then FVSizeOffset := Height - OldHeight
      else FVSizeOffset := 0;
    end
  else
    FVSizeOffset := 0;

  GetScrollInfo;
  FUpdatingScrollInfo := False;
end;


procedure TscGPScrollPanel.GetHRangeRTL;
var
  i, FMax, W, MaxRight: Integer;
begin
  MaxRight := 0;
  W := ClientWidth;
  if ControlCount > 0
  then
  for i := 0 to ControlCount - 1 do
  with Controls[i] do
  begin
   if Visible 
   then
     if Abs(Left - W) > MaxRight then MaxRight := Abs(Left - W);
  end;
  if MaxRight = 0
  then
    begin
      if Buttons[1].Visible then SetButtonsVisible(False);
      Exit;
    end;
  FMax := MaxRight + SPosition;
  if (FMax > W)
  then
    begin
      if not Buttons[1].Visible then SetButtonsVisible(True);
      if (SPosition > 0) and (MaxRight < W) and (FHSizeOffset > 0)
      then
        begin
          if FHSizeOffset > SPosition then FHSizeOffset := SPosition;
          SMax := FMax - 1;
          SPosition := SPosition - FHSizeOffset;
          SPage := W;
          HScrollControls(-FHSizeOffset);
          SOldPosition := SPosition;
        end
     else
       begin
         if (FHSizeOffset = 0) and ((FMax - 1) < SMax) and (SPosition > 0) and
            (MaxRight < W)
         then
           begin
             SMax := FMax - 1;
             SPosition := SPosition - (W - MaxRight);
             SPage := W;
             HScrollControls(MaxRight - W);
             FHSizeOffset := 0;
             SOldPosition := SPosition;
           end
         else
           begin
             SMax := FMax - 1;
             SPage := W;
           end;
          FHSizeOffset := 0;
          SOldPosition := SPosition;
        end;
    end
  else
    begin
      if SPosition > 0 then
        HScrollControls(-SPosition);
      FHSizeOffset := 0;
      SMax := 0;
      SPosition := 0;
      SPage := 0;
      if Buttons[1].Visible then SetButtonsVisible(False);
   end;
end;

procedure TscGPScrollPanel.GetHRange;
var
  i, FMax, W, MaxRight: Integer;
begin
  if FHorzScrollDirection = scgspsdRightToLeft then
  begin
    GetHRangeRTL;
    Exit;
  end;

  MaxRight := 0;
  if ControlCount > 0
  then
  for i := 0 to ControlCount - 1 do
  with Controls[i] do
  begin
   if Visible
   then
     if Left + Width > MaxRight then MaxRight := left + Width;
  end;
  if MaxRight = 0
  then
    begin
      if Buttons[1].Visible then SetButtonsVisible(False);
      Exit;
    end;
  W := ClientWidth;
  FMax := MaxRight + SPosition;
  if (FMax > W)
  then
    begin
      if not Buttons[1].Visible then  SetButtonsVisible(True);
      if (SPosition > 0) and (MaxRight < W) and (FHSizeOffset > 0)
      then
        begin
          if FHSizeOffset > SPosition then FHSizeOffset := SPosition;
          SMax := FMax - 1;
          SPosition := SPosition - FHSizeOffset;
          SPage := W;
          HScrollControls(-FHSizeOffset);
          SOldPosition := SPosition;
        end
     else
       begin
         if (FHSizeOffset = 0) and ((FMax - 1) < SMax) and (SPosition > 0) and
            (MaxRight < W)
         then
           begin
             SMax := FMax - 1;
             SPosition := SPosition - (W - MaxRight);
             SPage := W;
             HScrollControls(MaxRight - W);
             FHSizeOffset := 0;
             SOldPosition := SPosition;
           end
         else
           begin
             SMax := FMax - 1;
             SPage := W;
           end;
          FHSizeOffset := 0;
          SOldPosition := SPosition;
        end;
    end
  else
    begin
      if SPosition > 0 then HScrollControls(-SPosition);
      FHSizeOffset := 0;
      SMax := 0;
      SPosition := 0;
      SPage := 0;
      if Buttons[1].Visible then SetButtonsVisible(False);
   end;
end;

procedure TscGPScrollPanel.GetVRangeBTT;
var
  i, MaxBottom, H: Integer;
  FMax: Integer;
begin
  MaxBottom := 0;
  H := ClientHeight;
  if ControlCount > 0
  then
    for i := 0 to ControlCount - 1 do
    with Controls[i] do
    begin
      if Visible
   then
     if Abs(Top - H) > MaxBottom then MaxBottom := Abs(Top - H);
    end;
  if MaxBottom = 0
  then
    begin
      if Buttons[1].Visible then SetButtonsVisible(False);
      Exit;
    end;
  FMax := MaxBottom + SPosition;
  if FMax > H
  then
    begin
      if not Buttons[1].Visible then SetButtonsVisible(True);

      if (SPosition > 0) and (MaxBottom < H) and (FVSizeOffset > 0)
      then
        begin
          if FVSizeOffset > SPosition then FVSizeOffset := SPosition;
          SMax := FMax - 1;
          SPosition := SPosition - FVSizeOffset;
          SPage := H;
          VScrollControls(- FVSizeOffset);
          FVSizeOffset := 0;
          SOldPosition := SPosition;
        end
      else
        begin
          if (FVSizeOffset = 0) and ((FMax - 1) < SMax) and (SPosition > 0) and
             (MaxBottom < H)
            then
              begin
                SMax := FMax - 1;
                SPosition := SPosition - (H - MaxBottom);
                SPage := H;
                VScrollControls(MaxBottom - H);
                FVSizeOffset := 0;
                SOldPosition := SPosition;
              end
            else
              begin
                SMax := FMax - 1;
                SPage := H;
              end;
          FVSizeOffset := 0;
          SOldPosition := SPosition;
        end;
    end
   else
     begin
       if SPosition > 0 then VScrollControls(-SPosition);
       FVSizeOffset := 0;
       SOldPosition := 0;
       SMax := 0;
       SPage := 0;
       SPosition := 0;
       if Buttons[1].Visible then SetButtonsVisible(False);
     end;
end;

procedure TscGPScrollPanel.GetVRange;
var
  i, MaxBottom, H: Integer;
  FMax: Integer;
begin
  if FVertScrollDirection = scgspsdBottomToTop then
  begin
    GetVRangeBTT;
    Exit;
  end;

  MaxBottom := 0;
  if ControlCount > 0
  then
    for i := 0 to ControlCount - 1 do
    with Controls[i] do
    begin
      if Visible
      then
        if Top + Height > MaxBottom then MaxBottom := Top + Height;
    end;
  if MaxBottom = 0
  then
    begin
      if Buttons[1].Visible then SetButtonsVisible(False);
      Exit;
    end;
  H := ClientHeight;
  FMax := MaxBottom + SPosition;
  if FMax > H
  then
    begin
      if not Buttons[1].Visible then SetButtonsVisible(True);

      if (SPosition > 0) and (MaxBottom < H) and (FVSizeOffset > 0)
      then
        begin
          if FVSizeOffset > SPosition then FVSizeOffset := SPosition;
          SMax := FMax - 1;
          SPosition := SPosition - FVSizeOffset;
          SPage := H;
          VScrollControls(- FVSizeOffset);
          FVSizeOffset := 0;
          SOldPosition := SPosition;
        end
      else
        begin
          if (FVSizeOffset = 0) and ((FMax - 1) < SMax) and (SPosition > 0) and
             (MaxBottom < H)
            then
              begin
                SMax := FMax - 1;
                SPosition := SPosition - (H - MaxBottom);
                SPage := H;
                VScrollControls(MaxBottom - H);
                FVSizeOffset := 0;
                SOldPosition := SPosition;
              end
            else
              begin
                SMax := FMax - 1;
                SPage := H;
              end;
          FVSizeOffset := 0;
          SOldPosition := SPosition;
        end;
    end
   else
     begin
       if SPosition > 0 then VScrollControls(-SPosition);
       FVSizeOffset := 0;
       SOldPosition := 0;
       SMax := 0;
       SPage := 0;
       SPosition := 0;
       if Buttons[1].Visible then SetButtonsVisible(False);
     end;
end;

procedure TscGPScrollPanel.GetScrollInfo;
begin
  if not FCanScroll then
  begin
    Buttons[0].Visible := False;
    Buttons[1].Visible := False;
    Exit;
  end;

  if FScrollType = scstHorizontal
  then
    GetHRange
  else
    GetVRange;
end;

procedure TscGPScrollPanel.UpdateNC;
begin
  SetWindowPos(Handle, 0, Self.Left, Self.Top, Self.Width, Self.Height,
    SWP_FRAMECHANGED or SWP_DRAWFRAME or
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);
end;

procedure TscGPScrollPanel.SetButtonsVisible;
begin
  if Buttons[0].Visible <> AVisible
  then
    begin
      Buttons[0].Visible := AVisible;
      Buttons[1].Visible := AVisible;
      UpdateNC;
    end;
end;

procedure TscGPScrollPanel.ButtonDown(I: Integer);
begin
  case I of
    0:
      begin
        TimerMode := 1;
        StartTimer;
      end;
    1:
      begin
        TimerMode := 2;
        StartTimer;
      end;
  end;
end;

procedure TscGPScrollPanel.ButtonUp(I: Integer);
begin

  case I of
    0:
      begin
        StopTimer;
        TimerMode := 0;
        ButtonClick(0);
      end;
    1:
      begin
        StopTimer;
        TimerMode := 0;
        ButtonClick(1);
      end;
  end;
end;

procedure TscGPScrollPanel.ButtonClick;
var
  SOffset: Integer;
begin
  if FScrollType = scstHorizontal then
  begin
    if FScrollOffset = 0
    then
      SOffset := ClientWidth
    else
      SOffset := FScrollOffset;
  end
  else
  begin
    if FScrollOffset = 0
    then
      SOffset := ClientHeight
    else
      SOffset := FScrollOffset;
  end;

  if ((FScrollType = scstHorizontal) and(FHorzScrollDirection = scgspsdRightToLeft)) or
     ((FScrollType = scstVertical) and(FVertScrollDirection = scgspsdBottomToTop))
   then
    if I = 0 then
      I := 1
    else
      I := 0;
  
   case I of
    0:
      if FScrollType = scstHorizontal
      then
        begin
          SPosition := SPosition - SOffset;
          if SPosition < 0 then SPosition := 0;
          if (SPosition - SOldPosition <> 0)
          then
            begin
              HScrollControls(SPosition - SOldPosition);
              GetHRange;
            end
          else
            StopTimer;
        end
      else
        begin
          SPosition := SPosition - SOffset;
          if SPosition < 0 then SPosition := 0;
          if (SPosition - SOldPosition <> 0)
          then
            begin
              VScrollControls(SPosition - SOldPosition);
              GetVRange;
            end
          else
            StopTimer;
        end;
    1:
      if FScrollType = scstHorizontal
      then
        begin
          SPosition := SPosition + SOffset;
          if SPosition > SMax - SPage + 1 then SPosition := SMax - SPage + 1;
          if (SPosition - SOldPosition <> 0)
          then
            begin
              HScrollControls(SPosition - SOldPosition);
              GetHRange;
            end
          else
            StopTimer;
        end
      else
        begin
          SPosition := SPosition + SOffset;
          if SPosition > SMax - SPage + 1 then SPosition := SMax - SPage + 1;
          if (SPosition - SOldPosition <> 0)
          then
            begin
              VScrollControls(SPosition - SOldPosition);
              GetVRange;
            end
          else
            StopTimer;
        end;
  end;
end;

procedure TscGPScrollPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  DoMouseLeave;
end;

procedure TscGPScrollPanel.DoMouseLeave;
begin
  if Buttons[0].MouseIn and Buttons[0].Visible then
  begin
    if TimerMode <> 0 then StopTimer;
    Buttons[0].MouseIn := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
  if Buttons[1].MouseIn and Buttons[1].Visible then
  begin
    if TimerMode <> 0 then StopTimer;
    Buttons[1].MouseIn := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TscGPScrollPanel.WndProc;
var
  B: Boolean;
  P: TPoint;
begin
  B := True;
  case Message.Msg of
    WM_NCHITTEST:
      if not (csDesigning in ComponentState) then
      begin
        P.X := LoWord(Message.lParam);
        P.Y := HiWord(Message.lParam);
        P := ScreenToClient(P);
        if (((FScrollType = scstVertical) and (P.Y < 0)) or
           ((FScrollType = scstHorizontal) and (P.X < 0))) and Buttons[0].Visible
        then
          begin
            Message.Result := HTBUTTON1;
            if not FMouseTimerActive and (FCapturedButton = 0) then
            begin
              FMouseTimerActive := True;
              SetTimer(Handle, 2, 100, nil);
            end;
            B := False;
          end
        else
        if (((FScrollType = scstVertical) and (P.Y > ClientHeight)) or
           ((FScrollType = scstHorizontal) and (P.X > ClientWidth))) and Buttons[1].Visible
        then
          begin
            Message.Result := HTBUTTON2;
            if not FMouseTimerActive and (FCapturedButton = 0) then
            begin
              FMouseTimerActive := True;
              SetTimer(Handle, 2, 100, nil);
            end;
           B := False;
          end;
      end;

    WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK:
       begin
         if (Message.wParam = HTBUTTON1) and Buttons[0].Enabled
         then
           begin
             Buttons[0].Down := True;
             SendMessage(Handle, WM_NCPAINT, 0, 0);
             ButtonDown(0);
             FCapturedButton := 1;
             SetCapture(Handle);
           end
         else
         if (Message.wParam = HTBUTTON2) and Buttons[1].Enabled
         then
           begin
             Buttons[1].Down := True;
             SendMessage(Handle, WM_NCPAINT, 0, 0);
             ButtonDown(1);
             FCapturedButton := 2;
             SetCapture(Handle);
           end;
       end;
    WM_NCLBUTTONUP:
      begin
        FCapturedButton := 0;
      end;
    WM_LBUTTONUP:
     if FCapturedButton > 0 then
     begin
       FStartScroll := False;
       ReleaseCapture;
       Buttons[FCapturedButton - 1].Down := False;
       SendMessage(Handle, WM_NCPAINT, 0, 0);
       P.X := TWMLButtonUp(Message).XPos;
       P.Y := TWMLButtonUp(Message).YPos;
       if (((FScrollType = scstVertical) and (P.Y < 0)) or
           ((FScrollType = scstHorizontal) and (P.X < 0))) and Buttons[0].Visible and
           Buttons[0].MouseIn and (FCapturedButton = 1)
       then
         ButtonUp(0)
       else
       if (((FScrollType = scstVertical) and (P.Y > ClientHeight)) or
          ((FScrollType = scstHorizontal) and (P.X > ClientWidth))) and Buttons[1].Visible and
          Buttons[1].MouseIn and (FCapturedButton = 2)
       then
         ButtonUp(1);
       FCapturedButton := 0;
     end;
     WM_NCMOUSEMOVE:
       begin
         if (Message.wParam = HTBUTTON1) and (not Buttons[0].MouseIn) and
            Buttons[0].Enabled
         then
           begin
             Buttons[0].MouseIn := True;
             Buttons[1].MouseIn := False;
             SendMessage(Handle, WM_NCPAINT, 0, 0);
             if FHotScroll
             then
               begin
                 TimerMode := 1;
                 StartTimer;
               end;
           end
         else
         if (Message.wParam = HTBUTTON2) and (not Buttons[1].MouseIn) and
            Buttons[1].Enabled
         then
           begin
             Buttons[1].MouseIn := True;
             Buttons[0].MouseIn := False;
             SendMessage(Handle, WM_NCPAINT, 0, 0);
             if FHotScroll
             then
               begin
                 TimerMode := 2;
                 StartTimer;
               end;
           end;
       end;

      WM_MOUSEMOVE:
      begin
        if FCapturedButton > 0 then
          GetCursorPos(P);
        if (FCapturedButton > 0) and (WindowFromPoint(P) = Handle) then
        begin
          P.X := TWMMouseMove(Message).XPos;
          P.Y := TWMMouseMove(Message).YPos;
          if (((FScrollType = scstVertical) and (P.Y < 0)) or
           ((FScrollType = scstHorizontal) and (P.X < 0))) and Buttons[0].Visible and
           not Buttons[0].MouseIn and (FCapturedButton = 1)
          then
          begin
            Buttons[0].MouseIn := True;
            Buttons[1].MouseIn := False;
            SendMessage(Handle, WM_NCPAINT, 0, 0);
            if FHotScroll or FStartScroll then
            begin
              TimerMode := 1;
              StartTimer;
            end;
          end
          else
          if (((FScrollType = scstVertical) and (P.Y > ClientHeight)) or
             ((FScrollType = scstHorizontal) and (P.X > ClientWidth))) and Buttons[1].Visible and
             not Buttons[1].MouseIn and (FCapturedButton = 2)
          then
          begin
            Buttons[1].MouseIn := True;
            Buttons[0].MouseIn := False;
            SendMessage(Handle, WM_NCPAINT, 0, 0);
            if FHotScroll or FStartScroll then
            begin
              TimerMode := 2;
              StartTimer;
            end;
          end
          else
          if PtInRect(ClientRect, P) then
          begin
            if Buttons[0].MouseIn and Buttons[0].Visible then
            begin
              if TimerMode <> 0 then StopTimer;
              Buttons[0].MouseIn := False;
              SendMessage(Handle, WM_NCPAINT, 0, 0);
            end;
            if Buttons[1].MouseIn and Buttons[1].Visible then
            begin
              if TimerMode <> 0 then StopTimer;
              Buttons[1].MouseIn := False;
              SendMessage(Handle, WM_NCPAINT, 0, 0);
            end;
          end;
        end
        else
        begin
          if FMouseTimerActive then
          begin
            FMouseTimerActive := False;
            KillTimer(Handle, 2);
          end;
          if Buttons[0].MouseIn and Buttons[0].Visible then
          begin
            if TimerMode <> 0 then StopTimer;
            Buttons[0].MouseIn := False;
            SendMessage(Handle, WM_NCPAINT, 0, 0);
          end;
          if Buttons[1].MouseIn and Buttons[1].Visible then
          begin
            if TimerMode <> 0 then StopTimer;
            Buttons[1].MouseIn := False;
            SendMessage(Handle, WM_NCPAINT, 0, 0);
          end;
        end;
      end;
  end;
  if B then inherited;
end;

procedure TscGPScrollPanel.SetScrollOffset;
begin
  if Value >= 0 then FScrollOffset := Value;
end;

procedure TscGPScrollPanel.SetScrollType(Value: TscScrollType);
begin
  if FScrollType <> Value
  then
    begin
      FScrollType := Value;
      SMax := 0;
      SPosition := 0;
      SOldPosition := 0;
      SPage := 0;
      Buttons[0].Visible := False;
      Buttons[1].Visible := False;
      UpdateNC;
      InitTouch;
    end;
end;

procedure TscGPScrollPanel.WMNCCALCSIZE;
begin
  with TWMNCCALCSIZE(Message).CalcSize_Params^.rgrc[0] do
  begin
    if FScrollType = scstHorizontal
    then
      begin
        if Buttons[0].Visible then Inc(Left, FScrollButtonWidth);
        if Buttons[1].Visible then Dec(Right, FScrollButtonWidth);
      end
    else
      begin
        if Buttons[0].Visible then Inc(Top, FScrollButtonWidth);
        if Buttons[1].Visible then Dec(Bottom, FScrollButtonWidth);
      end;
  end;
end;

procedure TscGPScrollPanel.WMNCPaint;
var
  Cnvs: TCanvas;
  DC: HDC;
begin
  if Buttons[0].Visible or Buttons[1].Visible
  then
    begin
      DC := GetWindowDC(Handle);
      Cnvs := TCanvas.Create;
      Cnvs.Handle := DC;
      if Buttons[0].Visible then DrawButton(Cnvs, 0);
      if Buttons[1].Visible then DrawButton(Cnvs, 1);
      Cnvs.Handle := 0;
      ReleaseDC(Handle, DC);
      Cnvs.Free;
    end;
end;

procedure TscGPScrollPanel.WMSIZE;
begin
  FUpdatingScrollInfo := True;
  inherited;
  if ScrollType = scstHorizontal
  then
    begin
      Buttons[0].Rect := Rect(0, 0, FScrollButtonWidth, Height);
      Buttons[1].Rect := Rect(Width - FScrollButtonWidth, 0, Width, Height);
    end
  else
    begin
      Buttons[0].Rect := Rect(0, 0, Width, FScrollButtonWidth);
      Buttons[1].Rect := Rect(0, Height - FScrollButtonWidth, Width, Height);
    end;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
  FUpdatingScrollInfo := False;
end;

procedure TscGPScrollPanel.SetScrollTimerInterval;
begin
  if Value > 0 then FScrollTimerInterval := Value;
end;

procedure TscGPScrollPanel.DrawButton(Cnvs: TCanvas; I: Integer);
var
  B: TBitmap;
  R: TRect;
  R1: TGPRectF;
  GPBrush: TGPSolidBrush;
  G: TGPGraphics;
  BC, GC: Cardinal;
  BCAlpha, GCAlpha: Byte;
begin
  if not Buttons[I].Visible then Exit;
  if ((HorzScrollDirection = scgspsdRightToLeft) and (ScrollType = scstHorizontal)) or
     ((VertScrollDirection = scgspsdBottomToTop) and (ScrollType = scstVertical))
  then
  begin
    if I = 0 then
      Buttons[I].Enabled := (SPosition = 0) or (SPosition <= SMax - SPage)
    else
      Buttons[I].Enabled := Position <> 0;
  end
  else
    if I = 0 then
      Buttons[I].Enabled := Position <> 0
    else
      Buttons[I].Enabled := (SPosition = 0) or (SPosition <= SMax - SPage);
  if Buttons[I].Enabled then
  begin
    BCAlpha := 15;
    GCAlpha := 200;
    if Buttons[I].MouseIn and Buttons[I].Down then
    begin
      BCAlpha := 60;
      GCAlpha := 255;
    end
    else
     if Buttons[I].MouseIn then
     begin
       BCAlpha := 40;
       GCAlpha := 255;
     end;
  end
  else
  begin
    if FCapturedButton = I + 1 then
      FCapturedButton := 0;
    Buttons[I].MouseIn := False;
    Buttons[I].Down := False;
    BCAlpha := 5;
    GCAlpha := 100;
  end;
  BC := ColorToGPColor(GetStyleColor(clBtnText), BCAlpha);
  GC := ColorToGPColor(GetStyleColor(clBtnText), GCAlpha);
  B := TBitmap.Create;
  B.Width := Buttons[i].Rect.Width;
  B.Height := Buttons[i].Rect.Height;
  R := Rect(0, 0, B.Width, B.Height);
  G := TGPGraphics.Create(B.Canvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);
  R1 := RectToGPRect(R);
  GPBrush := TGPSolidBrush.Create(ColorToGPColor(GetStyleColor(clBtnFace), 255));
  G.FillRectangle(GPBrush, R1);
  GPBrush.Free;

  GPBrush := TGPSolidBrush.Create(BC);
  G.FillRectangle(GPBrush, R1);

  if Self.ScrollType = scstHorizontal then
  begin
    InflateGPRect(R1, -R1.Width / 20, -R1.Width / 20);
    R1.Y := R1.Y + R1.Height / 2 - R1.Width / 2;
    R1.Height := R1.Width;
    if I = 0 then
      scGPUtils.GPDrawLeftGlyph(G, R1, GC, FScaleFactor, FScrollButtonArrowWidth)
    else
      scGPUtils.GPDrawRightGlyph(G, R1, GC, FScaleFactor, FScrollButtonArrowWidth);
  end
  else
  begin
    InflateGPRect(R1, -R1.Height / 20, -R1.Height / 20);
    R1.X := R1.X + R1.Width / 2 - R1.Height / 2;
    R1.Width := R1.Height;
    if I = 0 then
      scGPUtils.GPDrawUpGlyph(G, R1, GC, FScaleFactor, FScrollButtonArrowWidth)
    else
      scGPUtils.GPDrawDownGlyph(G, R1, GC, FScaleFactor, FScrollButtonArrowWidth);
  end;
  G.Free;
  GPBrush.Free;
  Cnvs.Draw(Buttons[i].Rect.Left, Buttons[i].Rect.Top, B);
  B.Free;
end;

procedure TscGPScrollPanel.SetFillStyle(Value: TscGPShapeFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillGradientAngle(Value: Integer);
begin
  if (Value >= -360) and (Value <= 360) and
     (FFillGradientAngle <> Value) then
  begin
    FFillGradientAngle := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillGradientBeginAlpha(Value: Byte);
begin
  if FFillGradientBeginAlpha <> Value then
  begin
    FFillGradientBeginAlpha := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillGradientEndAlpha(Value: Byte);
begin
  if FFillGradientEndAlpha <> Value then
  begin
    FFillGradientEndAlpha := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillGradientBeginColorOffset(Value: Byte);
begin
  if (Value <= 100) and (FFillGradientBeginColorOffset <> Value) then
  begin
    FFillGradientBeginColorOffset := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillGradientEndColorOffset(Value: Byte);
begin
  if (Value <= 100) and (FFillGradientEndColorOffset <> Value) then
  begin
    FFillGradientEndColorOffset := Value;
    if FFillStyle = scgpsfGradient then
      RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillColorAlpha(Value: Byte);
begin
  if FFillColorAlpha <> Value then
  begin
    FFillColorAlpha := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillColor(Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPScrollPanel.SetFillColor2(Value: TColor);
begin
  if FFillColor2 <> Value then
  begin
    FFillColor2 := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

end.


