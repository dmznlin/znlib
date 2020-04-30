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
{       Support: support@almde.v.com                                }
{                                                                   }
{*******************************************************************}

unit scAdvancedControls;

{$I scdefine.inc}
{$R-}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types,
    Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Graphics, Vcl.Themes, Vcl.ImgList,
    Vcl.Buttons, Vcl.ComCtrls, WinApi.CommCtrl, Vcl.Menus,
    scDrawUtils, scImageCollection, scControls, scHint;

type
  TscBorderStyle = (scbsSingle, scbsNone);
  TscBackgroundStyle = (scbgsColor, scbgsFormBackground, scbgsTransparent);
  TscAdvancedSelectionStyle = (scastStyled, scastColor,
    scastGlow, scastCustomImage, scastCustomImageWithGlow);

  TscAdvancedCustomControl = class(TscCustomControl)
  private
    FBalloonHint: TBalloonHint;
    FHintComponent: TscHint;
    FBorderStyle: TscBorderStyle;
    FBackgroundStyle: TscBackgroundStyle;
    FCustomImages: TscCustomImageCollection;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FSelectionStyle: TscAdvancedSelectionStyle;
    FSelectionGlow: TscGlowEffect;
    FShowFocusRect: Boolean;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FCustomBackgroundImageIndex: Integer;
    FCustomOverContentImageIndex: Integer;
    FCustomSelectionImageIndex: Integer;
    FCustomFocusedSelectionImageIndex: Integer;
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetCustomOverContentImageIndex(Value: Integer);
    procedure SetCustomSelectionImageIndex(Value: Integer);
    procedure SetCustomFocusedSelectionImageIndex(Value: Integer);
    procedure SetBalloonHint(Value: TBalloonHint);
    procedure SetSelectionColor(Value: TColor);
    procedure SetSelectionTextColor(Value: TColor);
    procedure SetBackgroundStyle(Value: TscBackgroundStyle);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetSelectionStyle(Value: TscAdvancedSelectionStyle);
  protected
    FHintItemIndex: Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ShowBalloonHint(S: String; R: TRect);
    procedure ShowCustomHint(S: String);
    procedure SetBorderStyle(Value: TscBorderStyle); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    function CanDrawContent: Boolean; virtual;
    procedure DrawContent(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect); virtual;
    function GetContentRect: TRect; virtual;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnControlChange(Sender: TObject); virtual;
    property SelectionStyle: TscAdvancedSelectionStyle read FSelectionStyle write SetSelectionStyle;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
    property SelectionGlow: TscGlowEffect read FSelectionGlow write FSelectionGlow;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
    property BalloonHint: TBalloonHint read FBalloonHint write SetBalloonHint;
    property HintComponent: TscHint read FHintComponent write FHintComponent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageIndex: Integer read FCustomBackgroundImageIndex write SetCustomBackgroundImageIndex;
    property CustomOverContentImageIndex: Integer
      read FCustomOverContentImageIndex write SetCustomOverContentImageIndex;
    property CustomSelectionImageIndex: Integer read FCustomSelectionImageIndex write SetCustomSelectionImageIndex;
    property CustomFocusedSelectionImageIndex: Integer read FCustomFocusedSelectionImageIndex write SetCustomFocusedSelectionImageIndex;
    property BorderStyle: TscBorderStyle read FBorderStyle write SetBorderStyle;
    property BackgroundStyle: TscBackgroundStyle read FBackgroundStyle write SetBackgroundStyle;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property Color;
  end;

  TscAdvancedScrollingControl = class(TscAdvancedCustomControl)
  protected
    FHorzScrollBar: TScrollBar;
    FVertScrollBar: TScrollBar;
    FScrollGrip: TscPanel;
    FChangingScrollInfo: Boolean;
    FTouchBegin, FTouchEnd: Integer;
    FPopupMode: Boolean;
    procedure ShowScrollGrip;
    procedure HideScrollGrip;
    procedure ShowVertScrollBar;
    procedure ShowHorzScrollBar;
    procedure HideVertScrollBar;
    procedure HideHorzScrollBar;
    function GetContentRect: TRect; override;
    procedure AdjustScrollBars;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure SetBorderStyle(Value: TscBorderStyle); override;
    procedure UpdateHorzScrollPos(APosition: Integer);
    procedure UpdateVertScrollPos(APosition: Integer);
    procedure UpdateHorzScrollBar(AMin, AMax, APosition, APageSize: Integer);
    procedure UpdateVertScrollBar(AMin, AMax, APosition, APageSize: Integer);
    procedure OnHorzScrollBarChange(Sender: TObject); virtual;
    procedure OnVertScrollBarChange(Sender: TObject); virtual;
    procedure CMGesture(var Message: TCMGesture); message CM_GESTURE;
    procedure Loaded; override;
    procedure InitTouch; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TscListButtonStyleKind = (sclbsPushButton, sclbsTransparent, sclbsCustomImage);

  TscListBoxButton = class(TPersistent)
  private
    FCaption: String;
    FEnabled: Boolean;
    FVisible: Boolean;
    FStyleKind: TscListButtonStyleKind;
    FWidth: Integer;
    FHeight: Integer;
    FImageIndex: Integer;
    FImageHotIndex: Integer;
    FImagePressedIndex: Integer;
    FOnChange: TNotifyEvent;
    FOnVisibleChange: TNotifyEvent;
    procedure SetCaption(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetStyleKind(Value: TscListButtonStyleKind);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetImageIndex(Value: Integer);
  protected
    ButtonRect: TRect;
    MouseIn: Boolean;
    Down: Boolean;
    procedure Changed;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: String read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    property StyleKind: TscListButtonStyleKind read FStyleKind write SetStyleKind;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property ImageHotIndex: Integer read FImageHotIndex write FImageHotIndex;
    property ImagePressedIndex: Integer read FImagePressedIndex write FImagePressedIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnVisibleChange: TNotifyEvent read FOnVisibleChange write FOnVisibleChange;
  end;

  TscListBoxProgressBar = class(TPersistent)
  private
    FMin: Integer;
    FMax: Integer;
    FValue: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetValue(AValue: Integer);
  protected
    procedure Changed;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Value: Integer read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscAdvancedListItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FTitle: String;
    FCaption: String;
    FDetail: String;
    FEnabled: Boolean;
    FData: TCustomData;
    FHeader: Boolean;
    FOnClick: TNotifyEvent;
    FOnButtonClick: TNotifyEvent;
    FOnCheckClick: TNotifyEvent;
    FChecked: Boolean;
    FButton: TscListBoxButton;
    FProgressBar: TscListBoxProgressBar;
    FIndent: Integer;
  protected
    AlternateColor: Boolean;
    procedure OnItemChange(Sender: TObject);
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetDetail(const Value: String); virtual;
    procedure SetTitle(const Value: String); virtual;
    procedure SetCaption(const Value: String); virtual;
    procedure SetData(const Value: TCustomData); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetHeader(Value: Boolean); virtual;
    procedure SetIndent(Value: Integer);
    procedure SetChecked(Value: Boolean); virtual;
    procedure SetCustomColor(Value: TColor); virtual;
    procedure SetCustomColorAlpha(Value: Byte); virtual;
    procedure SetCustomTextColor(Value: TColor); virtual;
    procedure SetCustomDetailTextColor(Value: TColor); virtual;
  public
    ItemRect: TRect;
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
    property Button: TscListBoxButton read FButton write FButton;
    property CustomColor: TColor read FCustomColor write SetCustomColor;
    property CustomColorAlpha: Byte read FCustomColorAlpha write SetCustomColorAlpha;
    property CustomTextColor: TColor read FCustomTextColor write SetCustomTextColor;
    property CustomDetailTextColor: TColor read FCustomDetailTextColor write SetCustomDetailTextColor;
    property ProgressBar: TscListBoxProgressBar read FProgressBar write FProgressBar;
    property Header: Boolean read FHeader write SetHeader;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Title: String read FTitle write SetTitle;
    property Caption: String read FCaption write SetCaption;
    property Detail: String read FDetail write SetDetail;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property Checked: Boolean  read FChecked write SetChecked;
    property Indent: Integer read FIndent write SetIndent default 0;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCheckClick: TNotifyEvent read FOnCheckClick write FOnCheckClick;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

  TscAdvancedListBox = class;

  TscAdvancedListItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscAdvancedListItem;
    procedure SetItem(Index: Integer; Value:  TscAdvancedListItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    AdvancedListBox: TscAdvancedListBox;
    constructor Create(AListBox: TscAdvancedListBox);
    property Items[Index: Integer]: TscAdvancedListItem read GetItem write SetItem; default;
    function Add: TscAdvancedListItem;
    function Insert(Index: Integer): TscAdvancedListItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscDrawAdvancedListItemEvent = procedure(Cnvs: TCanvas; Index: Integer;
    TextRct: TRect) of object;

  TscAdvancedHeaderStyle = (scahsDefault, scahsBottomLine, scahsCustomImage);
  TscAdvancedListBoxStyle = (scalbsPlain, scalbsGroup, scalbsTransparentGroup);
  TscListBoxDetailPosition = (sclbdBottom, sclbdRight);

  TscAdvancedListBox = class(TscAdvancedScrollingControl)
  protected
    FAutoComplete: Boolean;
    FDetailPosition: TscListBoxDetailPosition;
    FDetailWordWrap: Boolean;
    FSearchString: String;
    FSearchTimerEnabled: Boolean;
    FIndentMargin: Integer;
    FStyle: TscAdvancedListBoxStyle;
    FCustomCheckedImageIndex: Integer;
    FCustomUnCheckedImageIndex: Integer;
    FCustomCheckedDisabledImageIndex: Integer;
    FCustomUnCheckedDisabledImageIndex: Integer;
    FCustomButtonImageNormalIndex: Integer;
    FCustomButtonImageHotIndex: Integer;
    FCustomButtonImagePressedIndex: Integer;
    FCustomButtonImageDisabledIndex: Integer;
    FCustomHeaderImageIndex: Integer;
    FGroupBackgroundCustomImageIndex: Integer;
    FGroupBackgroundColor: TColor;
    FTimerMode: Integer;
    FTitleFont: TFont;
    FDetailFont: TFont;
    FButtonFont: TFont;
    FHeaderFont: TFont;
    FHeaderStyle: TscAdvancedHeaderStyle;
    FHeaderUseStyleColor: Boolean;
    FPressedButtonIndex: Integer;
    FLineColor: TColor;
    FCheckOffset: Integer;
    FShowCheckBoxes: Boolean;
    FInUpdateItems: Boolean;
    FOnDrawItem: TscDrawAdvancedListItemEvent;
    FMouseMoveChangeIndex: Boolean;
    FDisabledFontColor: TColor;
    FShowLines: Boolean;
    FClicksDisabled: Boolean;
    FMouseDown: Boolean;
    FMouseActive: Integer;
    FOldButtonActive: Integer;
    FMax: Integer;
    FRealMax: Integer;
    FItemsRect: TRect;
    FScrollOffset: Integer;
    FItems: TscAdvancedListItems;
    FImages: TCustomImageList;
    FButtonImages: TCustomImageList;
    FShowItemTitles: Boolean;
    FShowItemDetails: Boolean;
    FShowItemProgressBars: Boolean;
    FItemProgressBarWidth: Integer;
    FItemProgressBarHeight: Integer;
    FItemHeight: Integer;
    FItemSpacing: Integer;
    FHeaderHeight: Integer;
    FOldHeight: Integer;
    FItemIndex: Integer;
    FOnItemClick: TNotifyEvent;
    FOnItemCheckClick: TNotifyEvent;
    FOnItemButtonClick: TNotifyEvent;
    FAlternateRow: Boolean;
    FFilter: String;
    procedure SetFilter(Value: String);
    procedure SetIndentMargin(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetGroupBackgroundCustomImageIndex(Value: Integer);
    procedure SetCustomCheckedImageIndex(Value: Integer);
    procedure SetCustomUnCheckedImageIndex(Value: Integer);
    procedure SetCustomCheckedDisabledImageIndex(Value: Integer);
    procedure SetCustomUnCheckedDisabledImageIndex(Value: Integer);
    procedure SetCustomButtonImageNormalIndex(Value: Integer);
    procedure SetCustomButtonImageHotIndex(Value: Integer);
    procedure SetCustomButtonImagePressedIndex(Value: Integer);
    procedure SetCustomButtonImageDisabledIndex(Value: Integer);
    procedure SetCustomHeaderImageIndex(Value: Integer);
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;
    procedure OnVertScrollBarChange(Sender: TObject); override;
    procedure SetHeaderStyle(Value: TscAdvancedHeaderStyle);
    procedure SetStyle(Value: TscAdvancedListBoxStyle);
    procedure SetGroupBackgroundColor(Value: TColor);
    procedure SetTitleFont(Value: TFont);
    procedure SetHeaderFont(Value: TFont);
    procedure SetDetailFont(Value: TFont);
    procedure SetDetailPosition(Value: TscListBoxDetailPosition);
    procedure SetDetailWordWrap(Value: Boolean);
    procedure SetButtonFont(Value: TFont);
    procedure SetAlternateRow(Value: Boolean);
    procedure SkinDrawCheckImage(X, Y: Integer; Cnvs: TCanvas; IR: TRect; DestCnvs: TCanvas);
    procedure SetShowCheckBoxes(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetItemIndex(Value: Integer);
    procedure SetItemActive(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetItemProgressBarWidth(Value: Integer);
    procedure SetItemProgressBarHeight(Value: Integer);
    procedure SetItems(Value: TscAdvancedListItems);
    procedure SetImages(Value: TCustomImageList);
    procedure SetButtonImages(Value: TCustomImageList);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetShowItemDetails(Value: Boolean);
    procedure SetShowItemProgressBars(Value: Boolean);
    procedure SetShowItemTitles(Value: Boolean);
    procedure SetLineColor(Value: TColor);
    function CanDrawContent: Boolean; override;
    procedure DrawContent(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawItem(Index: Integer; Cnvs: TCanvas); virtual;
    procedure DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
    procedure CalcItemRects;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure UpdateScrollInfo;
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
    procedure DrawListButton(ACanvas: TCanvas; AIndex: Integer; ADefaultTextColor: TColor);
    function GetCustomCheckBoxSize: TPoint;
    procedure DrawCustomCheckBox(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
    procedure Change; virtual;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(const Item: String); overload;
    procedure Add(Items: TStrings); overload;
    procedure Delete(Index: Integer);
    procedure Clear;

    procedure ScrollToItem(Index: Integer);
    procedure Scroll(AScrollOffset: Integer);
    procedure GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
    procedure BeginUpdateItems;
    procedure EndUpdateItems;
    function ItemAtPos(X, Y: Integer): Integer;
    function NextIndex(const S: string): Integer;
    function IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
    function IndexOfTitle(const S: string; AStartOff: Boolean = False): Integer;
    function IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
    procedure InitItemIndex(Value: Integer);
    procedure Sort;
    property Filter: String read FFilter write SetFilter;
  published
    property AutoComplete: Boolean
      read FAutoComplete write FAutoComplete;
    property CustomCheckedImageIndex: Integer read FCustomCheckedImageIndex write SetCustomCheckedImageIndex;
    property CustomUnCheckedImageIndex: Integer read FCustomUnCheckedImageIndex write SetCustomUnCheckedImageIndex;
    property CustomCheckedDisabledImageIndex: Integer read FCustomCheckedDisabledImageIndex write SetCustomCheckedDisabledImageIndex;
    property CustomUnCheckedDisabledImageIndex: Integer read FCustomUnCheckedDisabledImageIndex write SetCustomUnCheckedDisabledImageIndex;
    property CustomButtonImageNormalIndex: Integer read FCustomButtonImageNormalIndex write SetCustomButtonImageNormalIndex;
    property CustomButtonImageHotIndex: Integer read FCustomButtonImageHotIndex write SetCustomButtonImageHotIndex;
    property CustomButtonImagePressedIndex: Integer read FCustomButtonImagePressedIndex write SetCustomButtonImagePressedIndex;
    property CustomButtonImageDisabledIndex: Integer read FCustomButtonImageDisabledIndex write SetCustomButtonImageDisabledIndex;
    property CustomHeaderImageIndex: Integer read FCustomHeaderImageIndex write SetCustomHeaderImageIndex;
    property GroupBackgroundColor: TColor
      read FGroupBackgroundColor write SetGroupBackgroundColor;
    property GroupBackgroundCustomImageIndex: Integer
      read FGroupBackgroundCustomImageIndex write SetGroupBackgroundCustomImageIndex;
    property SelectionStyle;
    property SelectionColor;
    property SelectionTextColor;
    property ShowFocusRect;
    property SelectionGlow;
    property Style: TscAdvancedListBoxStyle
      read FStyle write SetStyle;
    property AlternateRow: Boolean
      read FAlternateRow write SetAlternateRow;
    property ShowCheckBoxes: Boolean
      read FShowCheckBoxes write SetShowCheckBoxes;
    property Items:  TscAdvancedListItems read FItems write SetItems;
    property Images: TCustomImageList read FImages write SetImages;
    property ButtonImages: TCustomImageList read FButtonImages write SetButtonImages;
    property ShowItemDetails: Boolean
      read FShowItemDetails write SetShowItemDetails;
    property ShowItemProgressBars: Boolean
      read FShowItemProgressBars write SetShowItemProgressBars;
    property ItemProgressBarWidth: Integer
      read FItemProgressBarWidth write SetItemProgressBarWidth;
    property ItemProgressBarHeight: Integer
      read FItemProgressBarHeight write SetItemProgressBarHeight;
    property ItemSpacing: Integer
      read FItemSpacing write SetItemSpacing;
    property ShowItemTitles: Boolean
      read FShowItemTitles write SetShowItemTitles;
    property ItemHeight: Integer
      read FItemHeight write SetItemHeight;
    property HeaderHeight: Integer
      read FHeaderHeight write SetHeaderHeight;
    property IndentMargin: Integer read FIndentMargin write SetIndentMargin;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ShowLines: Boolean read FShowLines write SetShowLines;
    property LineColor: TColor read FLineColor write SetLineColor;
    property MouseMoveChangeIndex: Boolean
      read FMouseMoveChangeIndex write FMouseMoveChangeIndex;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property DetailFont: TFont read FDetailFont write SetDetailFont;
    property DetailPosition: TscListBoxDetailPosition
      read FDetailPosition write SetDetailPosition;
    property DetailWordWrap: Boolean
      read FDetailWordWrap write SetDetailWordWrap;
    property ButtonFont: TFont read FButtonFont write SetButtonFont;
    property HeaderStyle: TscAdvancedHeaderStyle read FHeaderStyle write SetHeaderStyle;
    property HeaderUseStyleColor: Boolean read FHeaderUseStyleColor write FHeaderUseStyleColor;
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
    property OnDrawItem: TscDrawAdvancedListItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnItemClick: TNotifyEvent
      read FOnItemClick write FOnItemClick;
    property OnItemCheckClick: TNotifyEvent
      read FOnItemCheckClick write FOnItemCheckClick;
   property OnItemButtonClick: TNotifyEvent
      read FOnItemButtonClick write FOnItemButtonClick;
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

  TscAdvancedPopupListBox = class(TscAdvancedListBox)
  private
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect); override;
    property Items;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Hide;
    procedure Show(Origin: TPoint);
  end;

  TscPopupPanel = class(TscPanel)
  private
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Hide;
    procedure Show(Origin: TPoint);
  end;

  TscCBItem = record
    R: TRect;
    State: TOwnerDrawState;
  end;

  TscCBButton = record
    R: TRect;
    MouseIn: Boolean;
    Down: Boolean;
  end;

  TscAdvancedComboStyle = (sccbCombo, sccbPushButton, sccbToolButton,
    sccbPushButtonTransparent, sccbToolButtonTransparent, sccbTransparent,
    sccbTransparentBottomLine, sccbTransparentActiveBottomLine, sccbColorCombo,
    sccbCustomImage, sccbCustomImageOverContent);

  TscAdvancedCustomCombo = class(TscCustomActiveControl)
  private
    FStyle: TscAdvancedComboStyle;
    FShowFocusRect: Boolean;
    FBottomLineColor: TColor;
    FBottomActiveLineColor: TColor;
    FSelectionStyle: TscAdvancedSelectionStyle;
    FSelectionGlow: TscGlowEffect;
    FHideSelection: Boolean;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FColorOptions: TscButtonColorOptions;
    FCustomImages: TscCustomImageCollection;
    FCustomImageNormalIndex: Integer;
    FCustomImageHotIndex: Integer;
    FCustomImagePressedIndex: Integer;
    FCustomImageFocusedIndex: Integer;
    FCustomImageDisabledIndex: Integer;
    FCustomArrowImageNormalIndex: Integer;
    FCustomArrowImageHotIndex: Integer;
    FCustomArrowImagePressedIndex: Integer;
    FCustomArrowImageFocusedIndex: Integer;
    FCustomArrowImageDisabledIndex: Integer;
    FDropDownPosition: TscDropDownPosition;
    FUseFontColorToStyleColor: Boolean;
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetCustomImageNormalIndex(Value: Integer);
    procedure SetCustomImageHotIndex(Value: Integer);
    procedure SetCustomImagePressedIndex(Value: Integer);
    procedure SetCustomImageFocusedIndex(Value: Integer);
    procedure SetCustomImageDisabledIndex(Value: Integer);
    procedure SetCustomArrowImageNormalIndex(Value: Integer);
    procedure SetCustomArrowImageHotIndex(Value: Integer);
    procedure SetCustomArrowImagePressedIndex(Value: Integer);
    procedure SetCustomArrowImageFocusedIndex(Value: Integer);
    procedure SetCustomArrowImageDisabledIndex(Value: Integer);
    procedure OnColorOptionsChange(Sender: TObject);
    procedure SetSelectionColor(Value: TColor);
    procedure SetSelectionTextColor(Value: TColor);
    procedure SetStyle(Value: TscAdvancedComboStyle);
    procedure SetBottomLineColor(Value: TColor);
  protected
    CBItem: TscCBItem;
    Button: TscCBButton;
    FDropDown: Boolean;
    function GetCustomArrowWidth: Integer;
    procedure DrawArrow(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AColor: TColor);
    procedure DrawCustomArrow(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DrawComboItem(ACanvas: TCanvas; ARect: TRect); virtual;
    function CanDrawItem: Boolean; virtual;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    property Style: TscAdvancedComboStyle read FStyle write SetStyle;
    property SelectionStyle: TscAdvancedSelectionStyle read FSelectionStyle write FSelectionStyle;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
    property SelectionGlow: TscGlowEffect read FSelectionGlow write FSelectionGlow;
    property HideSelection: Boolean read FHideSelection write FHideSelection;
    property BottomLineColor: TColor read FBottomLineColor write SetBottomLineColor;
    property BottomActiveLineColor: TColor read FBottomActiveLineColor write FBottomActiveLineColor;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
    property ColorOptions: TscButtonColorOptions read FColorOptions write FColorOptions;
    property CustomImages: TscCustomImageCollection read
      FCustomImages write SetCustomImages;
    property CustomImageNormalIndex: Integer read
      FCustomImageNormalIndex write SetCustomImageNormalIndex;
    property CustomImageHotIndex: Integer read
      FCustomImageHotIndex write SetCustomImageHotIndex;
    property CustomImagePressedIndex: Integer read
      FCustomImagePressedIndex write SetCustomImagePressedIndex;
    property CustomImageDisabledIndex: Integer read
      FCustomImageDisabledIndex write SetCustomImageDisabledIndex;
    property CustomImageFocusedIndex: Integer read
      FCustomImageFocusedIndex write SetCustomImageFocusedIndex;
    property CustomArrowImageNormalIndex: Integer read
      FCustomArrowImageNormalIndex write SetCustomArrowImageNormalIndex;
    property CustomArrowImageHotIndex: Integer read
      FCustomArrowImageHotIndex write SetCustomArrowImageHotIndex;
    property CustomArrowImagePressedIndex: Integer read
      FCustomArrowImagePressedIndex write SetCustomArrowImagePressedIndex;
    property CustomArrowImageDisabledIndex: Integer read
      FCustomArrowImageDisabledIndex write SetCustomArrowImageDisabledIndex;
    property CustomArrowImageFocusedIndex: Integer read
      FCustomArrowImageFocusedIndex write SetCustomArrowImageFocusedIndex;
    property DropDownPosition: TscDropDownPosition
     read FDropDownPosition write FDropDownPosition default scdpRight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property UseFontColorToStyleColor: Boolean
      read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
  end;

  TscAdvancedCustomComboBox = class(TscAdvancedCustomCombo)
  protected
    FAutoComplete: Boolean;
    FSearchString: String;
    FSearchTimerEnabled: Boolean;
    FOnDrawItem: TscDrawAdvancedListItemEvent;
    FListBoxWallpapers: TscCustomImageCollection;
    FListBoxWallpaperIndex: Integer;
    FShowItemTitle: Boolean;
    FShowItemImage: Boolean;
    FShowItemText: Boolean;
    FShowItemDetail: Boolean;
    FDropDownCount: Integer;
    WasInLB: Boolean;
    FTimerMode: Integer;
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
    FListBox: TscAdvancedPopupListBox;
    FListBoxWindowProc: TWndMethod;
    FCheckedListMode: Boolean;
    FCheckedListWrap: Boolean;
    FOnItemCheckClick: TNotifyEvent;
    FOnListBoxDrawItem: TscDrawAdvancedListItemEvent;
    procedure SetCheckedListWrap(Value: Boolean);
    procedure SetCheckedListMode(Value: Boolean);
    function CanAnimate: Boolean; override;
    procedure SetShowItemTitle(Value: Boolean);
    procedure SetShowItemDetail(Value: Boolean);
    procedure SetShowItemImage(Value: Boolean);
    procedure SetShowItemText(Value: Boolean);
    procedure ListBoxWindowProcHook(var Message: TMessage);
    procedure ProcessListBox;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;
    function GetAlternateRow: Boolean;
    procedure SetAlternateRow(Value: Boolean);
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
    procedure CheckButtonClick(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TscAdvancedListItems);
    function GetItems: TscAdvancedListItems;
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
    procedure DrawComboItem(ACanvas: TCanvas; ARect: TRect); override;
    function CanDrawItem: Boolean; override;
    procedure Change; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ComboKeyUp(AChange: Boolean);
    procedure ComboKeyDown(AChange: Boolean);
    procedure ComboPageUp(AChange: Boolean);
    procedure ComboPageDown(AChange: Boolean);

    function GetTitleFont: TFont;
    procedure SetTitleFont(Value: TFont);
    function GetHeaderFont: TFont;
    procedure SetHeaderFont(Value: TFont);
    function GetDetailFont: TFont;
    procedure SetDetailFont(Value: TFont);

    function GetDetailPosition: TscListBoxDetailPosition;
    procedure SetDetailPosition(Value: TscListBoxDetailPosition);
    function GetDetailWordWrap: Boolean;
    procedure SetDetailWordWrap(Value: Boolean);

    function GetListBoxIndentMargin: Integer;
    procedure SetListBoxIndentMargin(Value: Integer);

    function GetListBoxCustomHeaderImageIndex: Integer;
    procedure SetListBoxCustomHeaderImageIndex(Value: Integer);
    function GetListBoxCustomSelectionImageIndex: Integer;
    procedure SetListBoxCustomSelectionImageIndex(Value: Integer);

    procedure SetListBoxWallpapers(Value: TscCustomImageCollection);
    procedure SetListBoxWallpaperIndex(Value: Integer);
    function GetListBoxHeaderUseStyleColor: Boolean;
    procedure SetListBoxHeaderUseStyleColor(Value: Boolean);
    function GetListBoxLineColor: TColor;
    procedure SetListBoxLineColor(Value: TColor);
    function GetListBoxHeaderStyle: TscAdvancedHeaderStyle;
    procedure SetListBoxHeaderStyle(Value: TscAdvancedHeaderStyle);
    function GetListBoxSelectionStyle: TscAdvancedSelectionStyle;
    procedure SetListBoxSelectionStyle(Value: TscAdvancedSelectionStyle);

    function GetListBoxSelectionColor: TColor;
    procedure SetListBoxSelectionColor(Value: TColor);
    function GetListBoxSelectionTextColor: TColor;
    procedure SetListBoxSelectionTextColor(Value: TColor);

    function GetListBoxShowLines: Boolean;
    procedure SetListBoxShowLines(Value: Boolean);
    function GetListBoxItemHeight: Integer;
    procedure SetListBoxItemHeight(Value: Integer);
    function GetListBoxHeaderHeight: Integer;
    procedure SetListBoxHeaderHeight(Value: Integer);
    function GetListBoxShowItemTitles: Boolean;
    procedure SetListBoxShowItemTitles(Value: Boolean);
    function GetListBoxShowItemDetails: Boolean;
    procedure SetListBoxShowItemDetails(Value: Boolean);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdateItems;
    procedure EndUpdateItems;

    procedure Add(const Item: String); overload;
    procedure Add(Items: TStrings); overload;
    procedure Delete(Index: Integer);
    procedure Clear;

    function NextIndex(const S: string): Integer;
    function IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
    function IndexOfTitle(const S: string; AStartOff: Boolean = False): Integer;
    function IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
    procedure Sort;
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

    property DetailPosition: TscListBoxDetailPosition
      read GetDetailPosition write SetDetailPosition;
    property DetailWordWrap: Boolean
      read GetDetailWordWrap write SetDetailWordWrap;

    property ShowItemTitle: Boolean read FShowItemTitle write SetShowItemTitle;
    property ShowItemImage: Boolean read FShowItemImage write SetShowItemImage;
    property ShowItemText: Boolean read FShowItemText write SetShowItemText;
    property ShowItemDetail: Boolean read FShowItemDetail write SetShowItemDetail;
    property AlternateRow: Boolean read GetAlternateRow write SetAlternateRow;
    property BottomLineColor;
    property BottomActiveLineColor;
    property ListBoxHeaderUseStyleColor: Boolean
     read GetListBoxHeaderUseStyleColor write SetListBoxHeaderUseStyleColor;
    property ListBoxWallpapers: TscCustomImageCollection
      read FListBoxWallpapers write SetListBoxWallpapers;
    property ListBoxWallpaperIndex: Integer
      read FListBoxWallpaperIndex write SetListBoxWallpaperIndex;
    property ListBoxLineColor: TColor
      read GetListBoxLineColor write SetListBoxLineColor;
    property ListBoxHeaderStyle: TscAdvancedHeaderStyle
      read GetListBoxHeaderStyle write SetListBoxHeaderStyle;
    property ListBoxWidth: Integer read FListBoxWidth write FListBoxWidth;
    property ListBoxHeight: Integer read FListBoxHeight write FListBoxHeight;
    property ListBoxSelectionStyle:  TscAdvancedSelectionStyle
      read GetListBoxSelectionStyle write SetListBoxSelectionStyle;

    property ListBoxIndentMargin: Integer
      read GetListBoxIndentMargin write SetListBoxIndentMargin;

    property ListBoxSelectionColor: TColor
      read GetListBoxSelectionColor  write SetListBoxSelectionColor;

    property ListBoxSelectionTextColor: TColor
      read GetListBoxSelectionTextColor  write SetListBoxSelectionTextColor;

    property ListBoxShowItemTitles: Boolean
      read GetListBoxShowItemTitles write SetListBoxShowItemTitles;

    property ListBoxShowItemDetails: Boolean
      read GetListBoxShowItemDetails write SetListBoxShowItemDetails;

    property ListBoxShowLines: Boolean
      read GetListBoxShowLines write SetListBoxShowLines;
    property ListBoxItemHeight: Integer
      read GetListBoxItemHeight write SetListBoxItemHeight;
    property ListBoxHeaderHeight: Integer
      read GetListBoxHeaderHeight write SetListBoxHeaderHeight;

    property ListBoxCustomHeaderImageIndex: Integer
      read GetListBoxCustomHeaderImageIndex write SetListBoxCustomHeaderImageIndex;
    property ListBoxCustomSelectionImageIndex: Integer
      read GetListBoxCustomSelectionImageIndex write SetListBoxCustomSelectionImageIndex;

    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Align;
    property Items: TscAdvancedListItems read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Images: TCustomImageList read GetImages write SetImages;
    property TitleFont: TFont
      read GetTitleFont write SetTitleFont;
    property HeaderFont: TFont
      read GetHeaderFont write SetHeaderFont;
    property DetailFont: TFont
      read GetDetailFont write SetDetailFont;
    property ListBox: TscAdvancedPopupListBox read FListBox;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;
    property OnDrawItem: TscDrawAdvancedListItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnItemCheckClick: TNotifyEvent
      read FOnItemCheckClick write FOnItemCheckClick;
    property OnListBoxDrawItem: TscDrawAdvancedListItemEvent
      read FOnListBoxDrawItem write FOnListBoxDrawItem;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
  end;

  TscAdvancedComboBox = class(TscAdvancedCustomComboBox)
  published
    property AutoComplete;
    property Animation;
    property CheckedListMode;
    property CheckedListWrap;
    property DetailPosition;
    property DetailWordWrap;
    property Images;
    property Items;
    property ItemIndex;
    property DropDownCount;
    property DropDownPosition;
    property Style;
    property HideSelection;
    property SelectionStyle;
    property SelectionColor;
    property SelectionTextColor;
    property SelectionGlow;
    property ShowFocusRect;
    property AlternateRow;
    property ShowItemTitle;
    property ShowItemImage;
    property ShowItemText;
    property ShowItemDetail;
    property TitleFont;
    property HeaderFont;
    property DetailFont;
    property ColorOptions;
    property CustomImages;
    property CustomImageNormalIndex;
    property CustomImageHotIndex;
    property CustomImagePressedIndex;
    property CustomImageDisabledIndex;
    property CustomImageFocusedIndex;
    property CustomArrowImageNormalIndex;
    property CustomArrowImageHotIndex;
    property CustomArrowImagePressedIndex;
    property CustomArrowImageDisabledIndex;
    property CustomArrowImageFocusedIndex;
    property ListBoxHeaderUseStyleColor;
    property ListBoxWallpapers;
    property ListBoxWallpaperIndex;
    property ListBoxHeaderStyle;
    property ListBoxLineColor;
    property ListBoxWidth;
    property ListBoxHeight;
    property ListBoxShowItemTitles;
    property ListBoxShowItemDetails;
    property ListBoxShowLines;
    property ListBoxItemHeight;
    property ListBoxHeaderHeight;
    property ListBoxSelectionStyle;
    property ListBoxSelectionColor;
    property ListBoxSelectionTextColor;
    property ListBoxCustomSelectionImageIndex;
    property ListBoxCustomHeaderImageIndex;

    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    property Align;
    property Font;
    property Color;

    property OnDrawItem;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnDropDown;
    property OnItemCheckClick;
    property OnListBoxDrawItem;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
  end;

  TscLinkBarItem = class(TCollectionItem)
  private
    FData: TCustomData;
    FImageIndex: Integer;
    FActiveImageIndex: Integer;
    FUseCustomGlowColor: Boolean;
    FCustomGlowColor: TColor;
    FCaption: String;
    FEnabled: Boolean;
    FHeader: Boolean;
    FOnClick: TNotifyEvent;
  protected
    procedure SetData(const Value: TCustomData); virtual;
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetActiveImageIndex(const Value: Integer); virtual;
    procedure SetCaption(const Value: String); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetHeader(Value: Boolean); virtual;
  public
    ItemRect: TRect;
    IsVisible: Boolean;
    Active: Boolean;
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Data: TCustomData read FData write SetData;
  published
    property Header: Boolean read FHeader write SetHeader;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Caption: String read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property ActiveImageIndex: Integer read FActiveImageIndex
      write SetActiveImageIndex default -1;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property UseCustomGlowColor: Boolean
      read FUseCustomGlowColor write FUseCustomGlowColor;
    property CustomGlowColor: TColor
      read FCustomGlowColor write FCustomGlowColor;
  end;

  TscLinkBar = class;

  TscLinkBarItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscLinkBarItem;
    procedure SetItem(Index: Integer; Value:  TscLinkBarItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    LinkBar: TscLinkBar;
    constructor Create(AListBox: TscLinkBar);
    property Items[Index: Integer]: TscLinkBarItem read GetItem write SetItem; default;
    function Add: TscLinkBarItem;
    function Insert(Index: Integer): TscLinkBarItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscLinkBar = class(TscAdvancedScrollingControl)
  protected
    FUseHandPointCursor: Boolean;
    FHeaderFont: TFont;
    FHeaderStyle: TscAdvancedHeaderStyle;
    FHeaderUseStyleColor: Boolean;
    FCustomHeaderImageIndex: Integer;
    FShowTextUnderLine: Boolean;
    FHoldSelectedItem: Boolean;
    FSpacing: Integer;
    FClicksDisabled: Boolean;
    FMouseDown: Boolean;
    FMouseActive: Integer;
    FMax: Integer;
    FRealMax: Integer;
    FItemsRect: TRect;
    FScrollOffset: Integer;
    FItems: TscLinkBarItems;
    FImages: TCustomImageList;
    FItemHeight: Integer;
    FHeaderHeight: Integer;
    FOldHeight: Integer;
    FItemIndex: Integer;
    FOnItemClick: TNotifyEvent;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    function CanDrawContent: Boolean; override;
    procedure DrawContent(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawItem(Index: Integer; Cnvs: TCanvas); virtual;
    procedure DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
    procedure OnVertScrollBarChange(Sender: TObject); override;
    procedure SetHoldSelectedItem(Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetHeaderFont(Value: TFont);
    procedure SetHeaderStyle(Value: TscAdvancedHeaderStyle);
    procedure SetItemIndex(Value: Integer);
    procedure SetItemActive(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetItems(Value: TscLinkBarItems);
    procedure SetImages(Value: TCustomImageList);
    procedure SetCustomHeaderImageIndex(Value: Integer);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CalcItemRects;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure UpdateScrollInfo;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;
    function CalcHeight(AItemCount: Integer): Integer;
    procedure SetSpacing(Value: Integer);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure ScrollToItem(Index: Integer);
    procedure Scroll(AScrollOffset: Integer);
    procedure GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
  published
    property SelectionGlow;
    property UseHandPointCursor: Boolean
      read FUseHandPointCursor write FUseHandPointCursor;
    property HeaderStyle: TscAdvancedHeaderStyle read FHeaderStyle write SetHeaderStyle;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HeaderUseStyleColor: Boolean read FHeaderUseStyleColor write FHeaderUseStyleColor;
    property ShowTextUnderLine: Boolean
      read FShowTextUnderLine write FShowTextUnderLine;
    property HoldSelectedItem: Boolean
      read FHoldSelectedItem write SetHoldSelectedItem;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Spacing: Integer
     read FSpacing write SetSpacing;
    property Items:  TscLinkBarItems read FItems write SetItems;
    property Images: TCustomImageList read FImages write SetImages;
    property ItemHeight: Integer
      read FItemHeight write SetItemHeight;
    property HeaderHeight: Integer
      read FHeaderHeight write SetHeaderHeight;
    property CustomHeaderImageIndex: Integer read FCustomHeaderImageIndex write SetCustomHeaderImageIndex;
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

  TscGridViewItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: String;
    FEnabled: Boolean;
    FData: TCustomData;
    FHeader: Boolean;
    FOnClick: TNotifyEvent;
  protected
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetCaption(const Value: String); virtual;
    procedure SetData(const Value: TCustomData); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetHeader(Value: Boolean); virtual;
  public
    ItemRect: TRect;
    FShowEllipses: Boolean;
    IsVisible: Boolean;
    Active: Boolean;
    constructor Create(Collection: TCollection); override;
    property Data: TCustomData read FData write SetData;
    procedure Assign(Source: TPersistent); override;
  published
    property Header: Boolean read FHeader write SetHeader;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Caption: String read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscGridView = class;

  TscGridViewItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscGridViewItem;
    procedure SetItem(Index: Integer; Value:  TscGridViewItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    GridView: TscGridView;
    constructor Create(AGridView: TscGridView);
    property Items[Index: Integer]: TscGridViewItem read GetItem write SetItem; default;
    function Add: TscGridViewItem;
    function Insert(Index: Integer): TscGridViewItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscGridView = class(TscAdvancedScrollingControl)
  protected
    FAutoComplete: Boolean;
    FSearchString: String;
    FSearchTimerEnabled: Boolean;
    FTimerMode: Integer;
    FCustomHeaderImageIndex: Integer;
    FHeaderFont: TFont;
    FHeaderStyle: TscAdvancedHeaderStyle;
    FHeaderUseStyleColor: Boolean;
    FMouseMoveChangeIndex: Boolean;
    FItemLayout: TButtonLayout;
    FItemMargin: Integer;
    FItemSpacing: Integer;
    RowCount, ColCount: Integer;
    FInUpdateItems: Boolean;
    FOnDrawItem: TscDrawAdvancedListItemEvent;
    FDisabledFontColor: TColor;
    FClicksDisabled: Boolean;
    FMouseDown: Boolean;
    FMouseActive: Integer;
    FMax: Integer;
    FRealMax: Integer;
    FItemsRect: TRect;
    FScrollOffset: Integer;
    FItems: TscGridViewItems;
    FImages: TCustomImageList;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FHeaderHeight: Integer;
    FOldHeight: Integer;
    FItemIndex: Integer;
    FOnItemClick: TNotifyEvent;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;
    procedure SetCustomHeaderImageIndex(Value: Integer);
    procedure SetHeaderFont(Value: TFont);
    procedure SetHeaderStyle(Value: TscAdvancedHeaderStyle);
    procedure SetItemLayout(Value: TButtonLayout);
    procedure SetItemMargin(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItemActive(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemWidth(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetItems(Value: TscGridViewItems);
    procedure SetImages(Value: TCustomImageList);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CalcItemRects;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure UpdateScrollInfo;
    procedure Loaded; override;

    procedure OnVertScrollBarChange(Sender: TObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;

    procedure WndProc(var Message: TMessage); override;

    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure FindUp;
    procedure FindDown;
    procedure FindLeft;
    procedure FindRight;
    procedure FindPageUp;
    procedure FindPageDown;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function CalcHeight(AItemCount: Integer): Integer;
    function CanDrawContent: Boolean; override;
    procedure DrawContent(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawItem(Index: Integer; Cnvs: TCanvas); virtual;
    procedure DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure ScrollToItem(Index: Integer);
    procedure Scroll(AScrollOffset: Integer);
    procedure GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
    procedure BeginUpdateItems;
    procedure EndUpdateItems;
    function IndexOf(const S: string; AStartOff: Boolean = False): Integer;
    function NextIndex(const S: string): Integer;
    procedure InitItemIndex(Value: Integer);
  published
    property AutoComplete: Boolean
      read FAutoComplete write FAutoComplete;
    property BalloonHint;
    property HintComponent;
    property SelectionStyle;
    property SelectionGlow;
    property SelectionColor;
    property SelectionTextColor;
    property ShowFocusRect;
    property ItemLayout: TButtonLayout read FItemLayout write SetItemLayout;
    property ItemMargin: Integer read FItemMargin write SetItemMargin;
    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing;
    property Items:  TscGridViewItems read FItems write SetItems;
    property Images: TCustomImageList read FImages write SetImages;
    property ItemHeight: Integer
      read FItemHeight write SetItemHeight;
    property ItemWidth: Integer
      read FItemWidth write SetItemWidth;
    property HeaderHeight: Integer
      read FHeaderHeight write SetHeaderHeight;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property MouseMoveChangeIndex: Boolean
      read FMouseMoveChangeIndex write FMouseMoveChangeIndex;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HeaderStyle: TscAdvancedHeaderStyle read FHeaderStyle write SetHeaderStyle;
    property HeaderUseStyleColor: Boolean read FHeaderUseStyleColor write FHeaderUseStyleColor;
    property CustomHeaderImageIndex: Integer read FCustomHeaderImageIndex write SetCustomHeaderImageIndex;
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
    property OnDrawItem: TscDrawAdvancedListItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnItemClick: TNotifyEvent
      read FOnItemClick write FOnItemClick;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscPopupGridView = class(TscGridView)
  private
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect); override;
    property Items;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Hide;
    procedure Show(Origin: TPoint);
  end;

  TscHorzListBox = class;

  TscHorzItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: String;
    FEnabled: Boolean;
    FData: TCustomData;
    FOnClick: TNotifyEvent;
  protected
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetCaption(const Value: String); virtual;
    procedure SetData(const Value: TCustomData); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
  public
    ItemRect: TRect;
    IsVisible: Boolean;
    Active: Boolean;
    FShowEllipses: Boolean;
    constructor Create(Collection: TCollection); override;
    property Data: TCustomData read FData write SetData;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Caption: String read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscHorzItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscHorzItem;
    procedure SetItem(Index: Integer; Value:  TscHorzItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    HorzListBox: TscHorzListBox;
    constructor Create(AListBox: TscHorzListBox);
    property Items[Index: Integer]: TscHorzItem read GetItem write SetItem; default;
    function Add: TscHorzItem;
    function Insert(Index: Integer): TscHorzItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscHorzListBox = class(TscAdvancedScrollingControl)
  protected
    FAutoComplete: Boolean;
    FSearchString: String;
    FSearchTimerEnabled: Boolean;
    FTimerMode: Integer;
    FInUpdateItems: Boolean;
    FOnDrawItem: TscDrawAdvancedListItemEvent;
    FShowGlow: Boolean;
    FItemLayout: TButtonLayout;
    FItemMargin: Integer;
    FItemSpacing: Integer;
    FMouseMoveChangeIndex: Boolean;
    FDisabledFontColor: TColor;
    FClicksDisabled: Boolean;
    FMouseDown: Boolean;
    FMouseActive: Integer;
    FMax: Integer;
    FRealMax: Integer;
    FItemsRect: TRect;
    FScrollOffset: Integer;
    FItems: TscHorzItems;
    FImages: TCustomImageList;
    FItemWidth: Integer;
    FOldWidth: Integer;
    FItemIndex: Integer;
    FOnItemClick: TNotifyEvent;
    procedure SetShowGlow(Value: Boolean);
    procedure SetItemLayout(Value: TButtonLayout);
    procedure SetItemMargin(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItemActive(Value: Integer);
    procedure SetItemWidth(Value: Integer);
    procedure SetItems(Value: TscHorzItems);
    procedure SetImages(Value: TCustomImageList);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure CalcItemRects;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure UpdateScrollInfo;
    procedure OnHorzScrollBarChange(Sender: TObject); override;
    procedure Loaded; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WndProc(var Message: TMessage); override;

    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure FindUp;
    procedure FindDown;
    procedure FindPageUp;
    procedure FindPageDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function CalcWidth(AItemCount: Integer): Integer;
    function CanDrawContent: Boolean; override;
    procedure DrawContent(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawItem(Index: Integer; Cnvs: TCanvas); virtual;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;
    procedure InitTouch; override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollToItem(Index: Integer);
    function ItemAtPos(X, Y: Integer): Integer;
    procedure Scroll(AScrollOffset: Integer);
    procedure GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
    procedure BeginUpdateItems;
    procedure EndUpdateItems;
    function IndexOf(const S: string; AStartOff: Boolean = False): Integer;
    function NextIndex(const S: string): Integer;
  published
    property AutoComplete: Boolean
      read FAutoComplete write FAutoComplete;
    property BalloonHint;
    property HintComponent;
    property SelectionStyle;
    property SelectionColor;
    property SelectionGlow;
    property SelectionTextColor;
    property ShowFocusRect;
    property ShowGlow: Boolean read FShowGlow write SetShowGlow;
    property ItemLayout: TButtonLayout read FItemLayout write SetItemLayout;
    property ItemMargin: Integer read FItemMargin write SetItemMargin;
    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing;
    property Items:  TscHorzItems read FItems write SetItems;
    property Images: TCustomImageList read FImages write SetImages;
    property ItemWidth: Integer
      read FItemWidth write SetItemWidth;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property MouseMoveChangeIndex: Boolean
      read FMouseMoveChangeIndex write FMouseMoveChangeIndex;
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
    property OnDrawItem: TscDrawAdvancedListItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnItemClick: TNotifyEvent
      read FOnItemClick write FOnItemClick;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscGridViewCustomComboBox = class(TscAdvancedCustomCombo)
  protected
    FAutoComplete: Boolean;
    FSearchString: String;
    FSearchTimerEnabled: Boolean;
    FOnDrawItem: TscDrawAdvancedListItemEvent;
    FGridViewWallpapers: TscCustomImageCollection;
    FGridViewWallpaperIndex: Integer;
    FShowItemImage: Boolean;
    FShowItemText: Boolean;
    WasInLB: Boolean;
    FTimerMode: Integer;
    FGridViewWidth: Integer;
    FGridViewHeight: Integer;
    FHideSelection: Boolean;
    FLastTime: Cardinal;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOldItemIndex: Integer;
    FLBDown: Boolean;
    FGridView: TscPopupGridView;
    FGridViewWindowProc: TWndMethod;
    function CanAnimate: Boolean; override;
    procedure SetShowItemImage(Value: Boolean);
    procedure SetShowItemText(Value: Boolean);
    procedure GridViewWindowProcHook(var Message: TMessage);
    procedure ProcessGridView;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
    procedure CheckButtonClick(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TscGridViewItems);
    function GetItems: TscGridViewItems;
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
    procedure DrawComboItem(ACanvas: TCanvas; ARect: TRect); override;
    function CanDrawItem: Boolean; override;
    procedure Change; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ComboKeyUp(AChange: Boolean);
    procedure ComboKeyDown(AChange: Boolean);
    procedure ComboKeyLeft(AChange: Boolean);
    procedure ComboKeyRight(AChange: Boolean);
    procedure ComboPageUp(AChange: Boolean);
    procedure ComboPageDown(AChange: Boolean);

    procedure SetGridViewWallpapers(Value: TscCustomImageCollection);
    procedure SetGridViewWallpaperIndex(Value: Integer);
    function GetGridViewHeaderUseStyleColor: Boolean;
    procedure SetGridViewHeaderUseStyleColor(Value: Boolean);
    function GetGridViewHeaderStyle: TscAdvancedHeaderStyle;
    procedure SetGridViewHeaderStyle(Value: TscAdvancedHeaderStyle);
    function GetGridViewSelectionStyle: TscAdvancedSelectionStyle;
    procedure SetGridViewSelectionStyle(Value: TscAdvancedSelectionStyle);
    function GetGridViewItemHeight: Integer;
    procedure SetGridViewItemHeight(Value: Integer);
    function GetGridViewItemWidth: Integer;
    procedure SetGridViewItemWidth(Value: Integer);

    function GetGridViewBalloonHint: TBalloonHint;
    procedure SetGridViewBalloonHint(Value: TBalloonHint);
    function GetGridViewHintComponent: TscHint;
    procedure SetGridViewHintComponent(Value: TscHint);

    function GetGridViewHeaderHeight: Integer;
    procedure SetGridViewHeaderHeight(Value: Integer);
    function GetGridViewSelectionColor: TColor;
    procedure SetGridViewSelectionColor(Value: TColor);
    function GetGridViewSelectionTextColor: TColor;
    procedure SetGridViewSelectionTextColor(Value: TColor);

    function GetGridViewCustomHeaderImageIndex: Integer;
    procedure SetGridViewCustomHeaderImageIndex(Value: Integer);
    function GetGridViewCustomSelectionImageIndex: Integer;
    procedure SetGridViewCustomSelectionImageIndex(Value: Integer);

    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    ActiveSkinRect: TRect;
    ActiveFontColor: TColor;
    FontName: String;
    FontStyle: TFontStyles;
    FontHeight: Integer;
    SItemRect, FocusItemRect, ActiveItemRect: TRect;
    ItemLeftOffset, ItemRightOffset: Integer;
    ItemTextRect: TRect;
    FontColor, FocusFontColor: TColor;
    ButtonRect,
    ActiveButtonRect,
    DownButtonRect, UnEnabledButtonRect: TRect;
    GridViewName: String;
    ItemStretchEffect, FocusItemStretchEffect: Boolean;
    ShowFocus: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdateItems;
    procedure EndUpdateItems;
    function IndexOf(const S: string; AStartOff: Boolean = False): Integer;
    function NextIndex(const S: string): Integer;
    procedure InitItemIndex(Value: Integer);
    procedure CloseUp(Value: Boolean);
    procedure DropDown; virtual;
    function IsPopupVisible: Boolean;
    function CanCancelDropDown: Boolean;

    property AutoComplete: Boolean
      read FAutoComplete write FAutoComplete;

    property ShowItemImage: Boolean read FShowItemImage write SetShowItemImage;
    property ShowItemText: Boolean read FShowItemText write SetShowItemText;

    property GridViewBalloonHint: TBalloonHint
     read GetGridViewBalloonHint write SetGridViewBalloonHint;

    property GridViewHintComponent: TscHint
     read GetGridViewHintComponent write SetGridViewHintComponent;

    property GridViewHeaderUseStyleColor: Boolean
     read GetGridViewHeaderUseStyleColor write SetGridViewHeaderUseStyleColor;
    property GridViewWallpapers: TscCustomImageCollection
      read FGridViewWallpapers write SetGridViewWallpapers;
    property GridViewWallpaperIndex: Integer
      read FGridViewWallpaperIndex write SetGridViewWallpaperIndex;
    property GridViewHeaderStyle: TscAdvancedHeaderStyle
      read GetGridViewHeaderStyle write SetGridViewHeaderStyle;
    property GridViewWidth: Integer read FGridViewWidth write FGridViewWidth;
    property GridViewHeight: Integer read FGridViewHeight write FGridViewHeight;
    property GridViewSelectionStyle:  TscAdvancedSelectionStyle
      read GetGridViewSelectionStyle write SetGridViewSelectionStyle;
    property GridViewItemHeight: Integer
      read GetGridViewItemHeight write SetGridViewItemHeight;
    property GridViewItemWidth: Integer
      read GetGridViewItemWidth write SetGridViewItemWidth;
    property GridViewHeaderHeight: Integer
      read GetGridViewHeaderHeight write SetGridViewHeaderHeight;
    property GridViewSelectionColor: TColor
      read GetGridViewSelectionColor  write SetGridViewSelectionColor;
    property GridViewSelectionTextColor: TColor
      read GetGridViewSelectionTextColor  write SetGridViewSelectionTextColor;

    property GridViewCustomHeaderImageIndex: Integer
      read GetGridViewCustomHeaderImageIndex write SetGridViewCustomHeaderImageIndex;
    property GridViewCustomSelectionImageIndex: Integer
      read GetGridViewCustomSelectionImageIndex write SetGridViewCustomSelectionImageIndex;

    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Align;
    property Items: TscGridViewItems read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Images: TCustomImageList read GetImages write SetImages;
    property GridView: TscPopupGridView read FGridView;
    property OnDrawItem: TscDrawAdvancedListItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
  end;

  TscGridViewComboBox = class(TscGridViewCustomComboBox)
  published
    property AutoComplete;
    property Animation;
    property DropDownPosition;
    property Images;
    property Items;
    property ItemIndex;
    property Style;
    property HideSelection;
    property SelectionStyle;
    property SelectionColor;
    property SelectionTextColor;
    property SelectionGlow;
    property ShowFocusRect;
    property ShowItemImage;
    property ShowItemText;

    property ColorOptions;
    property CustomImages;
    property CustomImageNormalIndex;
    property CustomImageHotIndex;
    property CustomImagePressedIndex;
    property CustomImageDisabledIndex;
    property CustomImageFocusedIndex;
    property CustomArrowImageNormalIndex;
    property CustomArrowImageHotIndex;
    property CustomArrowImagePressedIndex;
    property CustomArrowImageDisabledIndex;
    property CustomArrowImageFocusedIndex;

    property GridViewBalloonHint;
    property GridViewHintComponent;

    property GridViewHeaderUseStyleColor;
    property GridViewWallpapers;
    property GridViewWallpaperIndex;
    property GridViewHeaderStyle;
    property GridViewWidth;
    property GridViewHeight;
    property GridViewItemHeight;
    property GridViewItemWidth;
    property GridViewHeaderHeight;
    property GridViewSelectionStyle;
    property GridViewSelectionColor;
    property GridViewSelectionTextColor;

    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    property Align;
    property Font;
    property Color;

    property OnDrawItem;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnDropDown;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEnter;
    property OnExit;
  end;

  TscGalleryDropDownButton = class(TscButton)
  protected
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscGallery = class;

  TscGalleryItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: String;
    FData: TCustomData;
    FOnClick: TNotifyEvent;
  protected
    ItemRect: TRect;
    IsVisible: Boolean;
    Active: Boolean;
    MouseIn: Boolean;
    PopupItemRect: TRect;
    PopupIsVisible: Boolean;
    PopupActive: Boolean;
    PopupMouseIn: Boolean;
    FShowEllipses: Boolean;
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetCaption(const Value: String); virtual;
    procedure SetData(const Value: TCustomData); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
  public
    constructor Create(Collection: TCollection); override;
    property Data: TCustomData read FData write SetData;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: String read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscGalleryItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscGalleryItem;
    procedure SetItem(Index: Integer; Value:  TscGalleryItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    FGallery: TscGallery;
    constructor Create(AFGallery: TscGallery);
    property Items[Index: Integer]: TscGalleryItem read GetItem write SetItem; default;
    function Add: TscGalleryItem;
    function Insert(Index: Integer): TscGalleryItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscPopupGallery = class;

  TscGallery = class(TscAdvancedCustomControl)
  protected
    FTouchBegin, FTouchEnd: Integer;
    FScrollPanel: TscPanel;
    FPopupGallery: TscPopupGallery;
    FPopupMaxRowCount: Integer;
    FMouseInItem, FOldMouseInItem: Integer;
    FStartIndex: Integer;
    FOnDrawItem: TscDrawAdvancedListItemEvent;
    FItemLayout: TButtonLayout;
    FItemMargin: Integer;
    FItemSpacing: Integer;
    FClicksDisabled: Boolean;
    FMouseDown: Boolean;
    FMouseActive: Integer;
    FItemsRect: TRect;
    RowCount, ColCount, ItemsInPage: Integer;
    FItems: TscGalleryItems;
    FImages: TCustomImageList;
    FItemWidth: Integer;
    FItemHeight: Integer;
    FOldWidth: Integer;
    UpDown: TscUpDown;
    DownButton: TscGalleryDropDownButton;
    FItemIndex: Integer;
    FOnItemClick: TNotifyEvent;
    function GetContentRect: TRect; override;
    procedure TestActive(MouseInIndex: Integer);
    procedure SetItemLayout(Value: TButtonLayout);
    procedure SetItemMargin(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItemActive(Value: Integer);
    procedure SetItemWidth(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetItems(Value: TscGalleryItems);
    procedure SetImages(Value: TCustomImageList);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CalcItemRects;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure ShowUpDown;
    procedure HideUpDown;
    procedure UpdateScrollInfo;
    procedure AdjustButtons;
    procedure OnButtonClick(Sender: TObject);
    procedure OnUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure Loaded; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMMouseHookCancelMode(var Message: TMessage); message WM_MOUSEHOOKCANCELMODE;

    procedure WndProc(var Message: TMessage); override;

    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure FindUp;
    procedure FindDown;
    procedure FindLeft;
    procedure FindRight;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure ShowPopupGallery;
    procedure HidePopupGallery;

    procedure BeginUpdateItems;
    procedure EndUpdateItems;
    function CanDrawContent: Boolean; override;
    procedure DrawContent(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawItem(Index: Integer; Cnvs: TCanvas); virtual;

    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;

    procedure InitTouch;
    procedure CMGesture(var Message: TCMGesture); message CM_GESTURE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure ScrollToItem(Index: Integer);
    procedure Scroll(AScrollOffset: Integer);
    procedure GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
  published
    property BalloonHint;
    property HintComponent;

    property SelectionStyle;
    property SelectionColor;
    property SelectionTextColor;
    property ShowFocusRect;
    property SelectionGlow;
    property PopupMaxRowCount: Integer
      read FPopupMaxRowCount write FPopupMaxRowCount;
    property ItemLayout: TButtonLayout read FItemLayout write SetItemLayout;
    property ItemMargin: Integer read FItemMargin write SetItemMargin;
    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing;
    property Items:  TscGalleryItems read FItems write SetItems;
    property Images: TCustomImageList read FImages write SetImages;
    property ItemWidth: Integer
      read FItemWidth write SetItemWidth;
    property ItemHeight: Integer
      read FItemHeight write SetItemHeight;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property OnDrawItem: TscDrawAdvancedListItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnItemClick: TNotifyEvent
      read FOnItemClick write FOnItemClick;
  end;

  TscPopupGallery = class(TscAdvancedScrollingControl)
  protected
    FTimerMode: Integer;
    FInUpdateItems: Boolean;
    FScrollOffset: Integer;
    FGallery: TscGallery;
    FMouseInItem, FOldMouseInItem: Integer;
    FStartIndex: Integer;
    FMouseDown: Boolean;
    FMouseActive: Integer;
    FMax: Integer;
    FRealMax: Integer;
    FItemsRect: TRect;
    RowCount, ColCount, ItemsInPage: Integer;
    FOldWidth: Integer;
    FItemIndex: Integer;

    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure EnableScrollTimer(Value: Integer);
    procedure StopScrollTimer;

    procedure TestActive(MouseInIndex: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItemActive(Value: Integer);
    procedure CalcItemRects;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure UpdateScrollInfo;
    procedure OnVertScrollBarChange(Sender: TObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;

    procedure WndProc(var Message: TMessage); override;

    procedure FindUp;
    procedure FindDown;
    procedure FindLeft;
    procedure FindRight;
    procedure FindPageUp;
    procedure FindPageDown;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetItems: TscGalleryItems;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    function CanDrawContent: Boolean; override;
    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawContent(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawItem(Index: Integer; Cnvs: TCanvas); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure ScrollToItem(Index: Integer);
    procedure Scroll(AScrollOffset: Integer);
    procedure GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
    procedure Hide;
    procedure Show(Origin: TPoint);
  published
    property Items:  TscGalleryItems read GetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  end;

  TscAdvancedComboEdit = class(TscCustomEdit)
  protected
    FIsModified: Boolean;
    FOnDrawItem: TscDrawAdvancedListItemEvent;
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
    FListBox: TscAdvancedPopupListBox;
    FListBoxWindowProc: TWndMethod;
    FUseFilter: Boolean;
    FStopUseFilter: Boolean;
    FMouseWheelSupport: Boolean;
    procedure RightButtonClick(Sender: TObject);
    procedure ListBoxWindowProcHook(var Message: TMessage);
    procedure ProcessListBox;
    procedure StartTimer;
    procedure StopTimer;
    function GetAlternateRow: Boolean;
    procedure SetAlternateRow(Value: Boolean);
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
    procedure CheckButtonClick(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TscAdvancedListItems);
    function GetItems: TscAdvancedListItems;
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

    function GetTitleFont: TFont;
    procedure SetTitleFont(Value: TFont);
    function GetHeaderFont: TFont;
    procedure SetHeaderFont(Value: TFont);
    function GetDetailFont: TFont;
    procedure SetDetailFont(Value: TFont);

    function GetListBoxDetailPosition: TscListBoxDetailPosition;
    procedure SetListBoxDetailPosition(Value: TscListBoxDetailPosition);
    function GetListBoxDetailWordWrap: Boolean;
    procedure SetListBoxDetailWordWrap(Value: Boolean);


    procedure SetListBoxWallpapers(Value: TscCustomImageCollection);
    procedure SetListBoxWallpaperIndex(Value: Integer);
    function GetListBoxHeaderUseStyleColor: Boolean;
    procedure SetListBoxHeaderUseStyleColor(Value: Boolean);
    function GetListBoxLineColor: TColor;
    procedure SetListBoxLineColor(Value: TColor);
    function GetListBoxHeaderStyle: TscAdvancedHeaderStyle;
    procedure SetListBoxHeaderStyle(Value: TscAdvancedHeaderStyle);
    function GetListBoxSelectionStyle: TscAdvancedSelectionStyle;
    procedure SetListBoxSelectionStyle(Value: TscAdvancedSelectionStyle);
    function GetListBoxShowLines: Boolean;
    procedure SetListBoxShowLines(Value: Boolean);
    function GetListBoxItemHeight: Integer;
    procedure SetListBoxItemHeight(Value: Integer);
    function GetListBoxHeaderHeight: Integer;
    procedure SetListBoxHeaderHeight(Value: Integer);
    function GetListBoxShowItemTitles: Boolean;
    procedure SetListBoxShowItemTitles(Value: Boolean);
    function GetListBoxShowItemDetails: Boolean;
    procedure SetListBoxShowItemDetails(Value: Boolean);
    function GetListBoxSelectionColor: TColor;
    procedure SetListBoxSelectionColor(Value: TColor);
    function GetListBoxSelectionTextColor: TColor;
    procedure SetListBoxSelectionTextColor(Value: TColor);
    function GetListBoxIndentMargin: Integer;
    procedure SetListBoxIndentMargin(Value: Integer);
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
    procedure Sort;
    function IsModified: Boolean;
  published
    property CustomDraw;
    property DropDownPosition: TscDropDownPosition
      read FDropDownPosition write FDropDownPosition default scdpRight;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property LeftButton;
    property RightButton;
    property ButtonImages;
    property Transparent;
    property BorderKind;
    property FrameColor;
    property FrameActiveColor;
    property AlternateRow: Boolean read GetAlternateRow write SetAlternateRow;

    property ListBoxDetailPosition: TscListBoxDetailPosition
      read GetListBoxDetailPosition write SetListBoxDetailPosition;
    property ListBoxDetailWordWrap: Boolean
      read GetListBoxDetailWordWrap write SetListBoxDetailWordWrap;

    property ListBoxIndentMargin: Integer
      read GetListBoxIndentMargin write SetListBoxIndentMargin;
    property ListBoxHeaderUseStyleColor: Boolean
     read GetListBoxHeaderUseStyleColor write SetListBoxHeaderUseStyleColor;
    property ListBoxWallpapers: TscCustomImageCollection
      read FListBoxWallpapers write SetListBoxWallpapers;
    property ListBoxWallpaperIndex: Integer
      read FListBoxWallpaperIndex write SetListBoxWallpaperIndex;
    property ListBoxLineColor: TColor
      read GetListBoxLineColor write SetListBoxLineColor;
    property ListBoxHeaderStyle: TscAdvancedHeaderStyle
      read GetListBoxHeaderStyle write SetListBoxHeaderStyle;
    property ListBoxWidth: Integer read FListBoxWidth write FListBoxWidth;
    property ListBoxHeight: Integer read FListBoxHeight write FListBoxHeight;
    property ListBoxSelectionStyle:  TscAdvancedSelectionStyle
      read GetListBoxSelectionStyle write SetListBoxSelectionStyle;
    property ListBoxShowItemTitles: Boolean
      read GetListBoxShowItemTitles write SetListBoxShowItemTitles;
     property ListBoxShowItemDetails: Boolean
      read GetListBoxShowItemDetails write SetListBoxShowItemDetails;
    property ListBoxShowLines: Boolean
      read GetListBoxShowLines write SetListBoxShowLines;
    property ListBoxItemHeight: Integer
      read GetListBoxItemHeight write SetListBoxItemHeight;
    property ListBoxHeaderHeight: Integer
      read GetListBoxHeaderHeight write SetListBoxHeaderHeight;
    property ListBoxSelectionColor: TColor
      read GetListBoxSelectionColor  write SetListBoxSelectionColor;
    property ListBoxSelectionTextColor: TColor
      read GetListBoxSelectionTextColor  write SetListBoxSelectionTextColor;
    property UseFilter: Boolean read FUseFilter write FUseFilter;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Align;
    property Items: TscAdvancedListItems read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Images: TCustomImageList read GetImages write SetImages;
    property TitleFont: TFont
      read GetTitleFont write SetTitleFont;
    property HeaderFont: TFont
      read GetHeaderFont write SetHeaderFont;
    property DetailFont: TFont
      read GetDetailFont write SetDetailFont;
    property ListBox: TscAdvancedPopupListBox read FListBox;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;
    property EditMask;
    property Text;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
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
    property OnDrawItem: TscDrawAdvancedListItemEvent
      read FOnDrawItem write FOnDrawItem;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

  TscButtonsBar = class;
  TscButtonBarSection = class;
  TscButtonBarItems = class;

  TscButtonBarItem = class(TCollectionItem)
  private
    FCaption: String;
    FImageIndex: Integer;
    FOnClick: TNotifyEvent;
    FTag: Integer;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FSpacing: Integer;
    FHint: String;
    FEnabled: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    procedure ItemClick(const Value: TNotifyEvent);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetSpacing(Value: Integer);
  protected
    function GetDisplayName: string; override;
    procedure Click;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write FHint;
    property ImageIndex:integer read FImageIndex write SetImageIndex;
    property Tag: Integer read FTag write FTag;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property Margin: Integer read FMargin write SetMargin;
    property Spacing: Integer read FSpacing write SetSpacing;
    property OnClick:TNotifyEvent read FonClick write ItemClick;
  end;

  TscButtonBarItems = class(TCollection)
  private
    FSection: TscButtonBarSection;
    function GetItem(Index: Integer): TscButtonBarItem;
    procedure SetItem(Index: Integer; Value: TscButtonBarItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Section: TscButtonBarSection);
    function Add: TscButtonBarItem;
    property Items[Index: Integer]: TscButtonBarItem read GetItem write SetItem; default;
  end;

  TscButtonBarSection = class(TCollectionItem)
  private
    FCaption: string;
    FItems: TscButtonBarItems;
    FOnClick: TNotifyEvent;
    FImageIndex: Integer;
    FTag: Integer;
    FHint: String;
    FMargin: Integer;
    FSpacing: Integer;
    FEnabled: Boolean;
    FBGColor: TColor;
    FItemFontColor: TColor;
    FItemGlowColor: TColor;
    FCustomColorOptions: TscButtonColorOptions;
    FCustomColors: Boolean;
    procedure SetCustomColors(Value: Boolean);
    procedure SetItemGlowColor(const Value: TColor);
    procedure SetItemFontColor(const Value: TColor);
    procedure SetBGColor(const Value: TColor);
    procedure SetCaption(const Value: string);
    procedure SetItems(const Value: TscButtonBarItems);
    procedure SectionClick(const Value: TNotifyEvent);
    procedure SetImageIndex(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure OnColorOptionsChange(Sender: TObject);
  protected
    function GetDisplayName: string; override;
    procedure Click;
  public
    constructor Create(Collection: TCollection); override;
    destructor  Destroy;override;
    procedure Assign(Source: TPersistent); override;
  published
    property CustomColorOptions: TscButtonColorOptions
        read FCustomColorOptions write FCustomColorOptions;
    property CustomColors: Boolean
      read FCustomColors write SetCustomColors;
    property BGColor: TColor
      read FBGColor write SetBGColor;
    property ItemFontColor: TColor
      read FItemFontColor write SetItemFontColor;
    property ItemGlowColor: TColor
      read FItemGlowColor write SetItemGlowColor;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write FHint;
    property Items: TscButtonBarItems read FItems write SetItems;
    property Tag: Integer read FTag write FTag;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Margin: Integer read FMargin write SetMargin;
    property Spacing: Integer read FSpacing write SetSpacing;
    property OnClick:TNotifyEvent read FOnClick write SectionClick;
  end;

  TscButtonBarSections = class(TCollection)
  private
    FButtonsBar: TscButtonsBar;
    function GetItem(Index: Integer): TscButtonBarSection;
    procedure SetItem(Index: Integer; Value: TscButtonBarSection);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    function GetButtonsBar: TscButtonsBar;
    constructor Create(ButtonsBar: TscButtonsBar);
    function Add: TscButtonBarSection;
    property Items[Index: Integer]: TscButtonBarSection read GetItem write SetItem; default;
  end;

  TscSectionButton = class(TscButton)
  private
    FItemIndex: Integer;
    FButtonsBar: TscButtonsBar;
  protected
    function GetCtrlState: TscsCtrlState; override;
  public
    constructor CreateEx(AOwner: TComponent; AButtonsBar: TscButtonsBar; AIndex: Integer);
    procedure ButtonClick; override;
  end;

  TscSectionItem = class(TscButton)
  private
    FItemIndex: Integer;
    FButtonsBar: TscButtonsBar;
    FSectionIndex: Integer;
  protected
    FStopClick: Boolean;
  public
    constructor CreateEx(AOwner: TComponent; AButtonsBar: TscButtonsBar; ASectionIndex, AIndex: Integer);
    procedure ButtonClick; override;
  end;

  TscbbItemStyle = (scbiToolButton, scbiLink, scbiTransparent);
  TscbbItemPanelStyle = (scbpPanel, scbpFormBackground);

  TscButtonsBar = class(TscAdvancedCustomControl)
  private
    FSelectFirstItem: Boolean;
    FButtonHeight: Integer;
    FButtonStyle: TscbbButtonStyle;
    FItemImages: TCustomImageList;
    FItemHeight: Integer;
    FItemImageGlow: Boolean;
    FItemPanelStyleColor: Boolean;
    FItemPanelColor: TColor;
    FItemPanelStyle: TscbbItemPanelStyle;
    FItemPanelWallpaperIndex: Integer;
    FItemStyle: TscbbItemStyle;
    FItemAnimation: Boolean;
    FItemGlowEffect: TscButtonGlowEffect;
    FShowSelectedItem: Boolean;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FShowItemHint: Boolean;
    FShowButtons: Boolean;
    FItemFont: TFont;
    FUpButton, FDownButton: TscButton;
    TopIndex: Integer;
    VisibleCount: Integer;
    FItemPanel: TscPanel;
    FSections: TscButtonBarSections;
    FSectionIndex: Integer;
    FSectionImages: TCustomImageList;
    FAllButtonsOnTop: Boolean;
    FReorderButtons: Boolean;
    procedure SetReorderButtons(Value: Boolean);
    procedure SetAllButtonsOnTop(Value: Boolean);
    procedure SetItemPanelWallpaperIndex(Value: Integer);
    procedure SetItemPanelColor(Value: TColor);
    procedure SetItemPanelStyle(Value: TscbbItemPanelStyle);
    procedure SetButtonStyle(Value: TscbbButtonStyle);
    procedure SetItemStyle(Value: TscbbItemStyle);
    procedure SetShowButtons(Value: Boolean);
    procedure SetButtonHeight(Value: Integer);
    procedure SetItemFont(Value: TFont);
    procedure SetItemHeight(Value: Integer);
    procedure SetSections(Value: TscButtonBarSections);
    procedure UpdateSection(Index: Integer);
    procedure SetSectionIndex(const Value: integer);
    procedure SetItemImages(const Value: TCustomImageList);
    procedure SetSectionImages(const Value: TCustomImageList);
    procedure CheckVisibleItems;
    procedure OnItemPanelResize(Sender: TObject);
    procedure OnItemsChange(Sender: TObject);
  protected
    FItemIndex: Integer;
    procedure CreateWnd; override;
    procedure ClearSections;
    procedure ClearItems;
    procedure ArangeItems;
    procedure ShowUpButton;
    procedure ShowDownButton;
    procedure HideUpButton;
    procedure HideDownButton;
    procedure UpButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure SetItemIndex2(ASectionIndex, AItemIndex: Integer);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure AdjustControls;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
  public
    FSectionButtons: TList;
    FSectionItems: TList;
    procedure OpenSection(Index: Integer);
    procedure ScrollUp;
    procedure ScrollDown;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent:TComponent; Operation:TOperation);override;
    procedure UpDateSectionButtons;
    procedure UpdateSections;
    procedure RePaintSections;
    procedure UpdateItems;
    function GetItemIndex: Integer;
    procedure SetItemIndex(ASectionIndex, AItemIndex: Integer);
    procedure AddItem(ASectionIndex: Integer;
                      AItem: TscButtonBarItem);
    procedure InsertItem(AItemIndex, ASectionIndex: Integer;
                         AItem: TscButtonBarItem);
    procedure DeleteItem(AItemIndex, ASectionIndex: Integer);
  published
    property AllButtonsOnTop: Boolean
      read FAllButtonsOnTop write SetAllButtonsOnTop;
    property SelectFirstItem: Boolean
     read FSelectFirstItem write FSelectFirstItem;
    property ItemImageGlow: Boolean
      read FItemImageGlow write FItemImageGlow;
    property ItemPanelStyleColor: Boolean
      read FItemPanelStyleColor write FItemPanelStyleColor;
    property ItemPanelWallpaperIndex: Integer
     read FItemPanelWallpaperIndex write SetItemPanelWallpaperIndex;
    property ItemPanelColor: TColor
      read FItemPanelColor write SetItemPanelColor;
    property ItemPanelStyle: TscbbItemPanelStyle
      read FItemPanelstyle write SetItemPanelStyle;
    property ButtonStyle: TscbbButtonStyle read FButtonStyle
     write SetButtonStyle;
    property ItemStyle: TscbbItemStyle read FItemStyle
     write SetItemStyle;
    property ItemAnimation: Boolean
      read FItemAnimation write FItemAnimation;
    property ItemGlowEffect: TscButtonGlowEffect read
      FItemGlowEffect write FItemGlowEffect;
    property ShowSelectedItem: Boolean
      read FShowSelectedItem write FShowSelectedItem;
    property ShowItemHint: Boolean read FShowItemHint write FShowItemHint;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons;
    property ButtonHeight: Integer
      read FButtonHeight write SetButtonHeight;
    property ItemFont: TFont read FItemFont write SetItemFont;
    property Align default alLeft;
    property Enabled;
    property ReorderButtons: Boolean
      read FReorderButtons write SetReorderButtons;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property ItemImages: TCustomImageList read FItemImages write SetItemImages;
    property SectionImages: TCustomImageList read FSectionImages write SetSectionImages;
    property Sections: TscButtonBarSections read FSections write SetSections;
    property SectionIndex:integer read FSectionIndex write SetSectionIndex;
    property PopupMenu;
    property ShowHint;
    property Hint;
    property Visible;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;


implementation
 uses
   System.UITypes, Vcl.ExtCtrls, System.SysUtils;

constructor TscAdvancedCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents,
    csOpaque, csDoubleClicks, csReplicatable, csPannable, csGestures];
  FBalloonHint := nil;
  FHintComponent := nil;
  FHintItemIndex := -1;
  TransparentBackground := False;
  FWallpapers := nil;
  FCustomImages := nil;
  FShowFocusRect := True;
  FSelectionGlow := TscGlowEffect.Create;
  FSelectionGlow.Enabled := True;
  FSelectionGlow.OnChange := OnControlChange;
  FWallpaperIndex := -1;
  FCustomFocusedSelectionImageIndex := -1;
  FCustomSelectionImageIndex := -1;
  FCustomBackgroundImageIndex := -1;
  FCustomOverContentImageIndex := -1;
  ParentColor := False;
  Color := clWindow;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
end;

destructor TscAdvancedCustomControl.Destroy;
begin
  FSelectionGlow.Free;
  inherited;
end;

procedure TscAdvancedCustomControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if (FBalloonHint <> nil) and FBalloonHint.ShowingHint
  then
    FBalloonHint.HideHint;
  if FHintComponent <> nil
  then
    FHintComponent.HideHint;
end;

procedure TscAdvancedCustomControl.SetCustomOverContentImageIndex(Value: Integer);
begin
  if FCustomOverContentImageIndex <> Value then
  begin
    FCustomOverContentImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.SetCustomFocusedSelectionImageIndex(Value: Integer);
begin
  if FCustomFocusedSelectionImageIndex <> Value then
  begin
    FCustomFocusedSelectionImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.SetCustomSelectionImageIndex(Value: Integer);
begin
  if FCustomSelectionImageIndex <> Value then
  begin
    FCustomSelectionImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.SetBalloonHint(Value: TBalloonHint);
begin
  if FBalloonHint <> Value then
  begin
    FBalloonHint := Value;
    if FBalloonHint <> nil then
      FBalloonHint.HideAfter := 2000;
  end;
end;

procedure TscAdvancedCustomControl.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscAdvancedCustomControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
  end;
end;

procedure TscAdvancedCustomControl.ShowCustomHint(S: String);
begin
  if FHintComponent <> nil then
    FHintComponent.ActivateHint(S);
end;

procedure TscAdvancedCustomControl.ShowBalloonHint(S: String; R: TRect);
var
  I: Integer;
  P: TPoint;
begin
  if FBalloonHint = nil then Exit;
  for I := 1 to Length(S) do
   if S[I] = ' ' then S[I] := #13;
  FBalloonHint.Title := '';
  FBalloonHint.Description := S;
  P.X := R.Left;
  P.Y := R.Top;
  P := ClientToScreen(P);
  R := Rect(P.X, P.Y, P.X + R.Width, P.Y + R.Height);
  if FBalloonHint is TscBalloonHint then
    TscBalloonHint(FBalloonHint).ShowHint(R)
  else
    BalloonHint.ShowHint(R);
end;

procedure TscAdvancedCustomControl.SetBorderStyle(Value: TscBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscAdvancedCustomControl.SetBackgroundStyle(Value: TscBackgroundStyle);
begin
  if FBackgroundStyle <> Value then
  begin
    if FBackgroundStyle = scbgsTransparent then
    begin
      TransparentBackground := False;
      GetParentBG;
    end;
    FBackgroundStyle := Value;
    if FBackgroundStyle = scbgsTransparent then
    begin
      TransparentBackground := True;
      GetParentBG;
    end;
    RePaintControl;
  end;
end;

function TscAdvancedCustomControl.GetContentRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
  if BorderStyle = scbsSingle then
    InflateRect(Result, -2, -2);
end;

function TscAdvancedCustomControl.CanDrawContent: Boolean;
begin
  Result := True;
end;

procedure TscAdvancedCustomControl.DrawContent(ACanvas: TCanvas; ARect: TRect);
begin

end;

procedure TscAdvancedCustomControl.DrawBorder(ACanvas: TCanvas; ARect: TRect);
var
  SaveIndex: Integer;
begin
  if BorderStyle = scbsSingle then
  begin
    if IsCustomStyle or IsWindows10 or IsWindows8 then
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      ExcludeClipRect(ACanvas.Handle, ARect.Left + 2, ARect.Top + 2,
         ARect.Right - 2, ARect.Bottom - 2);
      try
        DrawEditBorder (ACanvas, ARect, scsNormal);
      finally
        RestoreDC(ACanvas.Handle, SaveIndex);
      end;
    end
    else
      DrawSingleBorder(ACanvas, ARect);
  end;
end;

procedure TscAdvancedCustomControl.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, CR: TRect;
begin
  R := Rect(0, 0, Width, Height);
  CR := GetContentRect;
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) and
      FWallpapers.IsSolidDrawing(FWallpaperIndex)
  then
  begin
    FWallpapers.Draw(ACanvas, CR, FWallpaperIndex, FScaleFactor);
  end
  else
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex) and
      FCustomImages.IsSolidDrawing(FCustomBackgroundImageIndex)
  then
  begin
    FCustomImages.Draw(ACanvas, CR, FCustomBackgroundImageIndex, FScaleFactor);
  end
  else
  begin
    case FBackgroundStyle of
      scbgsColor:
        with ACanvas do
        begin
          if (seClient in StyleElements) and IsCustomStyle then
            Brush.Color :=  GetEditBrushColor(scsNormal)
          else
            Brush.Color := Self.Color;
          Brush.Style := bsSolid;
         FillRect(R);
       end;
      scbgsFormBackground:
      begin
        DrawFormBackground(ACanvas, R);
      end;
    end;

    if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
    then
      FCustomImages.Draw(ACanvas, CR, FCustomBackgroundImageIndex, FScaleFactor);

    if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
      FWallpapers.Draw(ACanvas, CR, FWallpaperIndex, FScaleFactor);
  end;
  DrawBorder(ACanvas, R);
  if CanDrawContent then
    DrawContent(ACanvas, CR);

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomOverContentImageIndex) 
  then
    FCustomImages.Draw(ACanvas, CR, FCustomOverContentImageIndex);
end;

procedure TscAdvancedCustomControl.SetSelectionStyle(Value: TscAdvancedSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomControl.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscAdvancedCustomControl.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscAdvancedCustomControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FBalloonHint) then
    FBalloonHint := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscAdvancedCustomControl.AdjustClientRect(var Rect: TRect);
begin
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := Width;
  Rect.Bottom := Height;
  if FBorderStyle = scbsSingle then
    InflateRect(Rect, -2, -2);
end;

constructor TscAdvancedScrollingControl.Create(AOwner: TComponent);
begin
  inherited;
  FPopupMode := False;
  FHorzScrollBar := nil;
  FVertScrollBar := nil;
  FScrollGrip := nil;
  FChangingScrollInfo := False;
  FTouchBegin := 0;
  FTouchEnd := 0;
  InitTouch;
end;

destructor TscAdvancedScrollingControl.Destroy;
begin
  HideVertScrollBar;
  HideHorzScrollBar;
  HideScrollGrip;
  inherited;
end;

procedure TscAdvancedScrollingControl.InitTouch;
begin
  Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions + [igoPanSingleFingerVertical, igoPanInertia];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [igPan, igPressAndTap];
end;

procedure TscAdvancedScrollingControl.Loaded;
begin
  inherited;
end;

procedure TscAdvancedScrollingControl.CMGesture(var Message: TCMGesture);
var
  Offset: Integer;
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
  begin
    if gfBegin in Message.Info^.Flags
    then
      FTouchBegin := Message.Info^.Location.Y
    else
    begin  
      FTouchEnd := Message.Info^.Location.Y;
      Offset := FTouchEnd - FTouchBegin;
      FTouchBegin := FTouchEnd;
      FVertScrollBar.Position := FVertScrollBar.Position - Offset;
    end;
  end;
  if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
  begin
    if gfBegin in Message.Info^.Flags
    then
      FTouchBegin := Message.Info^.Location.X
    else
    begin
      FTouchEnd := Message.Info^.Location.X;
      Offset := FTouchEnd - FTouchBegin;
      FTouchBegin := FTouchEnd;
      FHorzScrollBar.Position := FHorzScrollBar.Position - Offset;
    end;
  end;
end;

procedure TscAdvancedScrollingControl.SetBorderStyle(Value: TscBorderStyle);
begin
  inherited;
  AdjustScrollBars;
end;

function TscAdvancedScrollingControl.GetContentRect: TRect;
begin
  Result := inherited GetContentRect;
  if (FVertScrollBar <> nil) and FVertScrollBar.Visible then
  begin
    if bidiMode = bdRightToLeft then
      Inc(Result.Left, FVertScrollBar.Width)
    else
      Dec(Result.Right, FVertScrollBar.Width)
  end;
  if (FHorzScrollBar <> nil) and FHorzScrollBar.Visible then
  begin
    Dec(Result.Bottom, FHorzScrollBar.Height);
  end;
end;

procedure TscAdvancedScrollingControl.ShowScrollGrip;
begin
  if FScrollGrip <> nil then Exit;
  FScrollGrip := TscPanel.Create(Self);
  with FScrollGrip do
  begin
    Height := GetSystemMetrics(SM_CYHSCROLL);
    Width := GetSystemMetrics(SM_CYHSCROLL);
    Visible := False;
    Parent := Self;
  end;
end;

procedure TscAdvancedScrollingControl.HideScrollGrip;
begin
  if FScrollGrip <> nil then
  begin
    FScrollGrip.Free;
    FScrollGrip := nil;
  end;
end;

procedure TscAdvancedScrollingControl.ShowVertScrollBar;
begin
  if FVertScrollBar <> nil then Exit;
  FVertScrollBar := TscScrollbar.Create(Self);
  with FVertScrollBar do
  begin
    Kind := sbVertical;
    Width := GetSystemMetrics(SM_CYHSCROLL);
    Visible := False;
    Parent := Self;
    OnChange := OnVertScrollBarChange;
  end;
  AdjustScrollBars;
  FVertScrollBar.Visible := True;
  RePaintControl;
end;

procedure TscAdvancedScrollingControl.ShowHorzScrollBar;
begin
  if FHorzScrollBar <> nil then Exit;
  FHorzScrollBar := TscScrollbar.Create(Self);
  with FHorzScrollBar do
  begin
    Height := GetSystemMetrics(SM_CYHSCROLL);
    Visible := False;
    Parent := Self;
    OnChange := OnHorzScrollBarChange;
  end;
  AdjustScrollBars;
  FHorzScrollBar.Visible := True;
  RePaintControl;
end;

procedure TscAdvancedScrollingControl.HideVertScrollBar;
begin
  if FVertScrollBar <> nil then
  begin
    FVertScrollBar.Free;
    FVertScrollBar := nil;
  end;
end;

procedure TscAdvancedScrollingControl.HideHorzScrollBar;
begin
  if FHorzScrollBar <> nil then
  begin
    FHorzScrollBar.Free;
    FHorzScrollBar := nil;
  end;
end;

procedure TscAdvancedScrollingControl.AdjustScrollBars;
var
  R: TRect;
  Bottom: Integer;
  ScrollSize: Integer;
begin
  AdjustClientRect(R);
  ScrollSize := GetSystemMetrics(SM_CYHSCROLL);
  {$IFDEF VER330_UP}
  if FPopupMode and (Owner is TWinControl) then
    ScrollSize := TWinControl(Owner).GetSystemMetrics(SM_CYHSCROLL);
  {$ENDIF}
  Bottom := R.Bottom;
  if FHorzScrollBar <> nil then
    Dec(Bottom, ScrollSize);
  if FVertScrollBar <> nil then
  begin
    if BidiMode = bdRightToLeft then
      FVertScrollBar.SetBounds(R.Left, R.Top,
        ScrollSize, Bottom - R.Top)
     else
      FVertScrollBar.SetBounds(R.Right - ScrollSize, R.Top,
        ScrollSize, Bottom - R.Top)
  end;
  if FHorzScrollBar <> nil then
  begin
    if FVertScrollBar <> nil then
    begin
      if BidiMode = bdRightToLeft then
        FHorzScrollBar.SetBounds(R.Left + ScrollSize,
          R.Bottom - ScrollSize,
          R.Right - R.Left - ScrollSize, ScrollSize)
      else
        FHorzScrollBar.SetBounds(R.Left, R.Bottom - ScrollSize,
          R.Right - R.Left - ScrollSize, ScrollSize);
    end
    else
    begin
      FHorzScrollBar.SetBounds(R.Left, R.Bottom - ScrollSize,
        R.Width, ScrollSize);
    end;
  end;
  if (FHorzScrollBar <> nil) and (FVertScrollBar <> nil) then
  begin
    ShowScrollGrip;
    if BidiMode = bdRightToLeft then
      FScrollGrip.SetBounds(R.Left, R.Bottom - ScrollSize,
        ScrollSize, ScrollSize)
    else
      FScrollGrip.SetBounds(R.Right - ScrollSize,
       R.Bottom - ScrollSize,
        ScrollSize, ScrollSize);
    FScrollGrip.Visible := True;
  end
  else
    HideScrollGrip;
end;

procedure TscAdvancedScrollingControl.WMSIZE(var Msg: TMessage);
begin
  inherited;
  AdjustScrollBars;
end;

procedure TscAdvancedScrollingControl.UpdateHorzScrollPos(APosition: Integer);
begin
  FChangingScrollInfo := True;
  FHorzScrollBar.Position := APosition;
  FChangingScrollInfo := False;
end;

procedure TscAdvancedScrollingControl.UpdateVertScrollPos(APosition: Integer);
begin
  FChangingScrollInfo := True;
  FVertScrollBar.Position := APosition;
  FChangingScrollInfo := False;
end;


procedure TscAdvancedScrollingControl.UpdateHorzScrollBar(AMin, AMax, APosition, APageSize: Integer);
begin
  if FHorzScrollBar = nil then Exit;
  FChangingScrollInfo := True;
  if FHorzScrollBar.PageSize > AMax then
    FHorzScrollBar.PageSize := 0;
  FHorzScrollBar.SetParams(APosition, AMin, AMax);
  FHorzScrollBar.PageSize := APageSize;
  if IsCustomStyle and not (csLoading in ComponentState) and
     not (csReading in ComponentState)
  then
    FHorzScrollBar.Repaint;
  FChangingScrollInfo := False;
end;

procedure TscAdvancedScrollingControl.UpdateVertScrollBar(AMin, AMax, APosition, APageSize: Integer);
begin
  if FVertScrollBar = nil then Exit;
  FChangingScrollInfo := True;
  if FVertScrollBar.PageSize > AMax then
    FVertScrollBar.PageSize := 0;
  FVertScrollBar.SetParams(APosition, AMin, AMax);
  FVertScrollBar.PageSize := APageSize;
  if IsCustomStyle and not (csLoading in ComponentState) and
    not (csReading in ComponentState)
  then
    FVertScrollBar.Repaint;
  FChangingScrollInfo := False;
end;

procedure TscAdvancedScrollingControl.OnHorzScrollBarChange(Sender: TObject);
begin
  if FChangingScrollInfo then Exit;
end;

procedure TscAdvancedScrollingControl.OnVertScrollBarChange(Sender: TObject);
begin
  if FChangingScrollInfo then Exit;
end;

constructor TscListBoxButton.Create;
begin
  FEnabled := True;
  FVisible := False;
  FImageIndex := -1;
  FImageHotIndex := -1;
  FImagePressedIndex := -1;
  FWidth := 50;
  FHeight := 25;
  FCaption := '';
end;

procedure TscListBoxButton.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TscListBoxButton.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TscListBoxButton.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnVisibleChange) then FOnVisibleChange(Self);
  end;
end;

procedure TscListBoxButton.SetStyleKind(Value: TscListButtonStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    Changed;
  end;
end;

procedure TscListBoxButton.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value > 10) then
  begin
    FWidth := Value;
    if FVisible then
      if Assigned(FOnVisibleChange) then FOnVisibleChange(Self);
  end;
end;

procedure TscListBoxButton.SetHeight(Value: Integer);
begin
  if (FHeight <> Value) and (Value > 10) then
  begin
    FHeight := Value;
    if FVisible then
      if Assigned(FOnVisibleChange) then FOnVisibleChange(Self);
  end;
end;

procedure TscListBoxButton.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TscListBoxButton.Assign(Source: TPersistent);
begin
  if Source is TscListBoxButton then
  begin
    FCaption := TscListBoxButton(Source).Caption;
    FEnabled := TscListBoxButton(Source).Enabled;
    FVisible := TscListBoxButton(Source).Visible;
    FImageIndex := TscListBoxButton(Source).ImageIndex;
    FImageHotIndex := TscListBoxButton(Source).ImageHotIndex;
    FImagePressedIndex := TscListBoxButton(Source).ImagePressedIndex;
    FWidth := TscListBoxButton(Source).Width;
    FHeight := TscListBoxButton(Source).Height;
  end
  else
    inherited Assign(Source);
end;

procedure TscListBoxButton.Changed;
begin
  if FVisible then
    if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscListBoxProgressBar.Create;
begin
  FMin := 0;
  FMax := 100;
  FValue := 0;
end;

procedure TscListBoxProgressBar.SetValue(AValue: Integer);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TscListBoxProgressBar.SetMin(Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TscListBoxProgressBar.SetMax(Value: Integer);
begin
  if FMax > Value then
  begin
    FMax := Value;
   if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TscListBoxProgressBar.Assign(Source: TPersistent);
begin
  if Source is TscListBoxProgressBar then
  begin
    FMin := TscListBoxProgressBar(Source).Min;
    FMax := TscListBoxProgressBar(Source).Max;
    FValue := TscListBoxProgressBar(Source).Value;
  end
  else
    inherited Assign(Source);
end;

procedure TscListBoxProgressBar.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscAdvancedListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FIndent := 0;
  FCustomColor := clNone;
  FCustomColorAlpha := 255;
  FCustomTextColor := clNone;
  FCustomDetailTextColor := clNone;
  IsFilterVisible := True;
  FHeader := False;
  FImageIndex := -1;
  FCaption := '';
  FTitle := '';
  FDetail := '';
  FEnabled := True;
  FChecked := False;
  FProgressBar := TscListBoxProgressBar.Create;
  FProgressBar.OnChange := OnItemChange;
  FButton := TscListBoxButton.Create;
  FButton.OnChange := OnItemChange;
  FButton.OnVisibleChange := OnItemChange;
  if TscAdvancedListItems(Collection).AdvancedListBox.ItemIndex = Self.Index
  then
    Active := True
  else
    Active := False;
end;

destructor TscAdvancedListItem.Destroy;
begin
  FProgressBar.Free;
  FProgressBar := nil;
  FButton.Free;
  FButton := nil;
  inherited;
end;

procedure TscAdvancedListItem.Assign(Source: TPersistent);
begin
  if Source is TscAdvancedListItem then
  begin
    FImageIndex := TscAdvancedListItem(Source).ImageIndex;
    FCaption := TscAdvancedListItem(Source).Caption;
    FTitle := TscAdvancedListItem(Source).Title;
    FEnabled := TscAdvancedListItem(Source).Enabled;
    FChecked := TscAdvancedListItem(Source).Checked;
    FHeader := TscAdvancedListItem(Source).Header;
    FCustomTextColor :=  TscAdvancedListItem(Source).CustomTextColor;
    FCustomDetailTextColor :=  TscAdvancedListItem(Source).CustomDetailTextColor;
    FCustomColor :=  TscAdvancedListItem(Source).CustomColor;
    FCustomColorAlpha :=  TscAdvancedListItem(Source).CustomColorAlpha;
  end
  else
    inherited Assign(Source);
end;

procedure TscAdvancedListItem.OnItemChange(Sender: TObject);
begin
  Changed(False);
end;

procedure TscAdvancedListItem.SetCustomColor(Value: TColor);
begin
  if Value <> FCustomColor then
  begin
    FCustomColor := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscAdvancedListItem.SetCustomColorAlpha(Value: Byte);
begin
  if Value <> FCustomColorAlpha then
  begin
    FCustomColorAlpha := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscAdvancedListItem.SetCustomTextColor(Value: TColor);
begin
  if Value <> FCustomTextColor then
  begin
    FCustomTextColor := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscAdvancedListItem.SetCustomDetailTextColor(Value: TColor);
begin
  if Value <> FCustomDetailTextColor then
  begin
    FCustomDetailTextColor := Value;
    if IsVisible then
      Changed(False);
  end;
end;

procedure TscAdvancedListItem.SetIndent;
begin
  if (FIndent <> Value) then
  begin
    if Value >= 0 then
      FIndent := Value;
    Changed(False);
  end;
end;

procedure TscAdvancedListItem.SetChecked;
begin
  FChecked := Value;
  Changed(False);
end;

procedure TscAdvancedListItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed(False);
end;

procedure TscAdvancedListItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TscAdvancedListItem.SetHeader(Value: Boolean);
begin
  FHeader := Value;
  Changed(False);
end;

procedure TscAdvancedListItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  Changed(False);
end;

procedure TscAdvancedListItem.SetTitle(const Value: String);
begin
  FTitle := Value;
  Changed(False);
end;

procedure TscAdvancedListItem.SetDetail(const Value: String);
begin
  FDetail := Value;
  Changed(False);
end;

procedure TscAdvancedListItem.SetData(const Value: TCustomData);
begin
  FData := Value;
end;

constructor TscAdvancedListItems.Create;
begin
  inherited Create(TscAdvancedListItem);
  AdvancedListBox := AListBox;
end;

function TscAdvancedListItems.GetOwner: TPersistent;
begin
  Result := AdvancedListBox;
end;

procedure  TscAdvancedListItems.Update(Item: TCollectionItem);
begin
  AdvancedListBox.RePaintControl;
  AdvancedListBox.UpdateScrollInfo;
end;

function TscAdvancedListItems.GetItem(Index: Integer):  TscAdvancedListItem;
begin
  Result := TscAdvancedListItem(inherited GetItem(Index));
end;

procedure TscAdvancedListItems.SetItem(Index: Integer; Value:  TscAdvancedListItem);
begin
  inherited SetItem(Index, Value);
  AdvancedListBox.RePaintControl;
end;

function TscAdvancedListItems.Add: TscAdvancedListItem;
begin
  Result := TscAdvancedListItem(inherited Add);
  AdvancedListBox.RePaintControl;
end;

function TscAdvancedListItems.Insert(Index: Integer): TscAdvancedListItem;
begin
  Result := TscAdvancedListItem(inherited Insert(Index));
  AdvancedListBox.RePaintControl;
end;

procedure TscAdvancedListItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
  AdvancedListBox.RePaintControl;
end;

procedure TscAdvancedListItems.Clear;
begin
  inherited Clear;
  AdvancedListBox.FItemIndex := -1;
  AdvancedListBox.RePaintControl;
end;

constructor TscAdvancedListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoComplete := True;
  FFilter := '';
  FSearchString := '';
  FSearchTimerEnabled := False;

  FTimerMode := 0;
  FPopupMode := False;
  FItems := TscAdvancedListItems.Create(Self);

  FTitleFont := TFont.Create;
  FTitleFont.Assign(Font);
  FTitleFont.Style := [fsBold];
  FTitleFont.OnChange := OnControlChange;

  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(Font);
  FHeaderFont.Style := [fsBold];
  FHeaderFont.OnChange := OnControlChange;

  FDetailFont := TFont.Create;
  FDetailFont.Assign(Font);
  FDetailFont.Style := [];
  FDetailFont.Color := clGrayText;
  FDetailFont.OnChange := OnControlChange;
  FDetailPosition := sclbdBottom;
  FDetailWordWrap := False;

  FButtonFont := TFont.Create;
  FButtonFont.Assign(Font);
  FButtonFont.Style := [];
  FButtonFont.OnChange := OnControlChange;

  FItemProgressBarWidth := 0;
  FItemProgressBarHeight := 15;
  FItemSpacing := 5;
  FIndentMargin := 10;

  FHeaderUseStyleColor := True;
  FCheckOffset := 0;
  FShowCheckBoxes := False;
  FInUpdateItems := False;
  FClicksDisabled := False;
  FMouseMoveChangeIndex := False;
  FMouseDown := False;
  FShowLines := False;
  FMouseActive := -1;
  FOldButtonActive := -1;
  FPressedButtonIndex := -1;
  FScrollOffset := 0;
  FImages := nil;
  FButtonImages := nil;
  FGroupBackgroundCustomImageIndex := -1;
  FCustomCheckedImageIndex := -1;
  FCustomUnCheckedImageIndex := -1;
  FCustomCheckedDisabledImageIndex := -1;
  FCustomUnCheckedDisabledImageIndex := -1;
  FCustomButtonImageNormalIndex := -1;
  FCustomButtonImageHotIndex := -1;
  FCustomButtonImagePressedIndex := -1;
  FCustomButtonImageDisabledIndex := -1;
  FCustomHeaderImageIndex := -1;
  Width := 150;
  Height := 150;
  TabStop := True;
  FItemHeight := 30;
  FHeaderHeight := 20;
  FShowItemTitles := True;
  FShowItemDetails := False;
  FShowItemProgressBars := False;
  FMax := 0;
  FRealMax := 0;
  FOldHeight := -1;
  FItemIndex := -1;
  FDisabledFontColor := clGray;
  FGroupBackgroundColor := clWindow;
  FAlternateRow := False;
  FLineColor := clBtnFace;
end;

destructor TscAdvancedListBox.Destroy;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 1);
  end;
  FItems.Free;
  FItems := nil;
  FTitleFont.Free;
  FHeaderFont.Free;
  FDetailFont.Free;
  FButtonFont.Free;
  inherited Destroy;
end;

procedure TscAdvancedListBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FItemHeight := MulDiv(FItemHeight, M, D);
  FIndentMargin := MulDiv(FIndentMargin, M, D);
  FItemSpacing := MulDiv(FItemSpacing, M, D);
  FHeaderFont.Height := MulDiv(FHeaderFont.Height, M, D);
  FButtonFont.Height := MulDiv(FButtonFont.Height, M, D);
  FTitleFont.Height := MulDiv(FTitleFont.Height, M, D);
  FDetailFont.Height := MulDiv(FDetailFont.Height, M, D);
  FHeaderHeight := MulDiv(FHeaderHeight, M, D);
  FItemProgressBarWidth := MulDiv(FItemProgressBarWidth, M, D);
  FItemProgressBarHeight := MulDiv(FItemProgressBarHeight, M, D);
end;

procedure TscAdvancedListBox.Add(const Item: String);
var
  FItem: TscAdvancedListItem;
begin
  FItem := FItems.Add;
  FItem.Caption := Item;
end;

procedure TscAdvancedListBox.Add(Items: TStrings);
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

procedure TscAdvancedListBox.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure TscAdvancedListBox.Clear;
begin
  BeginUpdateItems;
  try
    FItems.Clear;
  finally
    EndUpdateItems;
  end;
end;

procedure TscAdvancedListBox.SetFilter(Value: String);
begin
  if FFilter <> Value then
  begin
    FFilter := Value;
    FScrollOffset := 0;
    RePaintControl;
    UpdateScrollInfo;
  end;
end;

procedure TscAdvancedListBox.SetIndentMargin(Value: Integer);
begin
  if (Value >= 5) and (FIndentMargin <> Value) then
  begin
    FIndentMargin := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.Sort;
var
  I, J: Integer;
begin
  if FItems.Count < 2 then Exit;
  for I := 0 to FItems.Count - 2 do
    for J := I + 1 to FItems.Count - 1 do
    if FItems[I].Caption > FItems[J].Caption  then
      FItems[J].SetIndex(I);
end;

function TscAdvancedListBox.NextIndex(const S: string): Integer;
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


function TscAdvancedListBox.IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
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

function TscAdvancedListBox.IndexOfTitle(const S: string; AStartOff: Boolean = False): Integer;
var
  I: Integer;
  FS, TS: String;
begin
  Result := -1;
  FS := LowerCase(S);
  for I := 0 to FItems.Count - 1 do
  begin
    TS := LowerCase(FItems[I].Title);
    if (not AStartOff and (TS = FS)) or
       (AStartOff and (Pos(FS, TS) = 1)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TscAdvancedListBox.IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
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

procedure TscAdvancedListBox.SetCustomHeaderImageIndex(Value: Integer);
begin
  if FCustomHeaderImageIndex <> Value then
  begin
    FCustomHeaderImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetCustomButtonImageNormalIndex(Value: Integer);
begin
  if FCustomButtonImageNormalIndex <> Value then
  begin
    FCustomButtonImageNormalIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetCustomButtonImageHotIndex(Value: Integer);
begin
  FCustomButtonImageHotIndex := Value;
end;

procedure TscAdvancedListBox.SetCustomButtonImagePressedIndex(Value: Integer);
begin
  FCustomButtonImagePressedIndex := Value;
end;

procedure TscAdvancedListBox.SetCustomButtonImageDisabledIndex(Value: Integer);
begin
  if FCustomButtonImageDisabledIndex <> Value then
  begin
    FCustomButtonImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetCustomCheckedImageIndex(Value: Integer);
begin
  if FCustomCheckedImageIndex <> Value then
  begin
    FCustomCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetCustomUnCheckedImageIndex(Value: Integer);
begin
  if FCustomUnCheckedImageIndex <> Value then
  begin
    FCustomUnCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetCustomCheckedDisabledImageIndex(Value: Integer);
begin
  if FCustomCheckedDisabledImageIndex <> Value then
  begin
    FCustomCheckedDisabledImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetCustomUnCheckedDisabledImageIndex(Value: Integer);
begin
  if FCustomUnCheckedDisabledImageIndex <> Value then
  begin
    FCustomUnCheckedDisabledImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.EnableScrollTimer(Value: Integer);
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

procedure TscAdvancedListBox.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscAdvancedListBox.WMTimer;
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

procedure TscAdvancedListBox.SetGroupBackgroundColor(Value: TColor);
begin
  if FGroupBackgroundColor <> Value then
  begin
    FGroupBackgroundColor := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetGroupBackgroundCustomImageIndex(Value: Integer);
begin
  if FGroupBackgroundCustomImageIndex <> Value then
  begin
    FGroupBackgroundCustomImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetStyle(Value: TscAdvancedListBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetHeaderStyle(Value: TscAdvancedHeaderStyle);
begin
  if FHeaderStyle <> Value then
  begin
    FHeaderStyle := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
  RePaintControl;
end;

procedure TscAdvancedListBox.SetDetailWordWrap(Value: Boolean);
begin
  if FDetailWordWrap <> Value then
  begin
    FDetailWordWrap:= Value;
    if FShowItemDetails and (DetailPosition = sclbdRight) then
      RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetDetailPosition(Value: TscListBoxDetailPosition);
begin
  if FDetailPosition <> Value then
  begin
    FDetailPosition := Value;
    if FShowItemDetails then
      RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetDetailFont(Value: TFont);
begin
  FDetailFont.Assign(Value);
  RePaintControl;
end;

procedure TscAdvancedListBox.SetButtonFont(Value: TFont);
begin
  FButtonFont.Assign(Value);
  RePaintControl;
end;

procedure TscAdvancedListBox.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  RePaintControl;
end;

procedure TscAdvancedListBox.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetAlternateRow(Value: Boolean);
begin
  if Value <> FAlternateRow then
  begin
    FAlternateRow := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.BeginUpdateItems;
begin
  FInUpdateItems := True;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TscAdvancedListBox.EndUpdateItems;
begin
  FInUpdateItems := False;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscAdvancedListBox.SkinDrawCheckImage(X, Y: Integer; Cnvs: TCanvas; IR: TRect; DestCnvs: TCanvas);
begin
end;

procedure TscAdvancedListBox.SetShowCheckBoxes;
begin
  if FShowCheckBoxes <> Value then
  begin
    FShowCheckBoxes := Value;
    RePaintControl;
  end;
end;

function TscAdvancedListBox.CalcHeight;
begin
  if AItemCount > FItems.Count then AItemCount := FItems.Count;
  Result := AItemCount * ItemHeight;
  Result := Result + Height - GetContentRect.Height;
end;

procedure TscAdvancedListBox.SetShowLines;
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetItemSpacing(Value: Integer);
begin
  if FItemSpacing <> Value then
  begin
    FItemSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    RePaintControl;
    UpdateScrollInfo;
  end;
end;

procedure TscAdvancedListBox.SetHeaderHeight(Value: Integer);
begin
  if FHeaderHeight <> Value
  then
    begin
      FHeaderHeight := Value;
      RePaintControl;
      UpdateScrollInfo;
    end;
end;

procedure TscAdvancedListBox.SetItems(Value: TscAdvancedListItems);
begin
  FItems.Assign(Value);
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscAdvancedListBox.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetButtonImages(Value: TCustomImageList);
begin
  if FButtonImages <> Value then
  begin
    FButtonImages := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  if (Operation = opRemove) and (AComponent = FButtonImages) then
    FButtonImages := nil;
end;

procedure TscAdvancedListBox.SetShowItemTitles(Value: Boolean);
begin
  if FShowItemTitles <> Value then
  begin
    FShowItemTitles := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetShowItemProgressBars(Value: Boolean);
begin
  if FShowItemProgressBars <> Value then
  begin
    FShowItemProgressBars := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetItemProgressBarWidth(Value: Integer);
begin
  if Value <> FItemProgressBarWidth then
  begin
    FItemProgressBarWidth := Value;
    if FShowItemProgressBars then RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetItemProgressBarHeight(Value: Integer);
begin
  if Value <> FItemProgressBarHeight then
  begin
    FItemProgressBarHeight := Value;
    if FShowItemProgressBars then RePaintControl;
  end;
end;

procedure TscAdvancedListBox.SetShowItemDetails(Value: Boolean);
begin
  if FShowItemDetails <> Value then
  begin
    FShowItemDetails := Value;
    RePaintControl;
  end;
end;

function TscAdvancedListBox.CanDrawContent: Boolean;
begin
  Result := Assigned(FItems) and (FItems.Count > 0);
end;

procedure TscAdvancedListBox.DrawContent(ACanvas: TCanvas; ARect: TRect);
var
  I, SaveIndex: Integer;
begin
  CalcItemRects;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,
      FItemsRect.Left, FItemsRect.Top, FItemsRect.Right, FItemsRect.Bottom);
    for I := 0 to FItems.Count - 1 do
      if FItems[I].IsVisible and FItems[I].IsFilterVisible then DrawItem(I, ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscAdvancedListBox.CalcItemRects;
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
  if FShowItemTitles and not Result then
  begin
    S := LowerCase(FItems[Index].Title);
    Result := Pos(FilterS, S) <> 0;
  end;
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
    with TscAdvancedListItem(FItems[I]) do
    if InFilter(I) then
    begin
      AlternateColor := AColor;
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

procedure TscAdvancedListBox.Scroll(AScrollOffset: Integer);
begin
  FScrollOffset := AScrollOffset;
  RePaintControl;
  UpdateScrollInfo;
end;

function TscAdvancedListBox.GetCustomCheckBoxSize: TPoint;
begin
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomCheckedImageIndex) then
  begin
    Result.X := FCustomImages.GetWidth(FCustomCheckedImageIndex, FScaleFactor);
    Result.Y := FCustomImages.GetHeight(FCustomUnCheckedImageIndex, FScaleFactor);
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

procedure TscAdvancedListBox.GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
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
end;

procedure TscAdvancedListBox.WMSize(var Msg: TWMSize);
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
  UpdateScrollInfo;
  FOldHeight := Height;
end;

procedure TscAdvancedListBox.ScrollToItem(Index: Integer);
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
  UpdateScrollInfo;
end;

procedure TscAdvancedListBox.UpdateScrollInfo;
var
  SMin, SMax, SPage, SPos: Integer;
begin
  if not HandleAllocated then Exit;
  if FInUpdateItems then Exit;
  GetScrollInfo(SMin, SMax, SPage, SPos);
  if SMax <> 0 then
  begin
    ShowVertScrollBar;
    UpdateVertScrollBar(SMin, SMax, SPos, SPage);
    if FVertScrollBar <> nil then
    begin
      FVertScrollBar.LargeChange := SPage;
      FVertScrollBar.SmallChange := FItemHeight;
    end;
  end
  else
  if (SMax = 0) and (FVertScrollBar <> nil) then
  begin
    HideVertScrollBar;
    RePaintControl;
  end;
end;

procedure TscAdvancedListBox.OnVertScrollBarChange(Sender: TObject);
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.HandleAllocated then
  begin
   if FVertScrollBar.Position <= FVertScrollBar.Max - FVertScrollBar.PageSize + 1  then
     Scroll(FVertScrollBar.Position)
   else
     Scroll(FVertScrollBar.Max - FVertScrollBar.PageSize + 1);
  end;
end;

procedure TscAdvancedListBox.DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
var
  R, R1: TRect;
  C, FC: TColor;
begin
  R := FItems[Index].ItemRect;
  if Style <> scalbsPlain then
  begin
    if (HeaderStyle = scahsCustomImage) and
       FCustomImages.IsIndexAvailable(FCustomHeaderImageIndex)
    then
      FCustomImages.Draw(Cnvs, R, FCustomHeaderImageIndex, FScaleFactor);

    if (seClient in StyleElements) and IsCustomStyle then
      C := GetEditBrushColor(scsNormal)
    else
      C := FGroupBackgroundColor;
    if IsCustomStyle and (seFont in StyleElements) then
    begin
      if BackgroundStyle = scbgsColor then
        FC := GetEditTextColor(scsNormal)
       else
        FC := GetCheckBoxTextColor(scsNormal)
    end
    else
      FC := Font.Color;
    C := MiddleColor(C, FC);
    Cnvs.Pen.Color := C;
    Cnvs.MoveTo(R.Left, R.Bottom - 1);
    Cnvs.LineTo(R.Right, R.Bottom - 1);
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
    begin
      C := GetCheckBoxTextColor(scsNormal);
      C := MiddleColor(C, GetStyleColor(clBtnFace));
      Cnvs.Font.Color := C;
    end;
  end
  else
  if HeaderStyle = scahsCustomImage then
  begin
    if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomHeaderImageIndex)
    then
      FCustomImages.Draw(Cnvs, R, FCustomHeaderImageIndex, FScaleFactor);
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
      Cnvs.Font.Color := GetStyleColor(HeaderFont.Color);
  end
  else
  if HeaderStyle = scahsDefault then
  begin
    DrawHeaderSection(Cnvs, FItems[Index].ItemRect, scsNormal);
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
      Cnvs.Font.Color := GetHeaderTextColor(scsNormal);
  end
  else
  begin
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
    begin
      C := GetCheckBoxTextColor(scsNormal);
      C := MiddleColor(C, GetStyleColor(clBtnFace));
      Cnvs.Font.Color := C;
    end
    else
      C := MiddleColor(clWindowText, clBtnFace);
    Cnvs.Pen.Color := C;
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 1);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 1);
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 2);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 2);
  end;
  Inc(R.Left, 5);
  Dec(R.Right, 5);
  Cnvs.Brush.Style := bsClear;
  if (HeaderStyle = scahsCustomImage) and (Style = scalbsPlain) then
    DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft))
  else
  if (HeaderStyle = scahsDefault) and (Style = scalbsPlain) then
    DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft))
  else
    begin
      R1 := Rect(0, 0, 0, 0);
      DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R1,
       DT_LEFT or DT_SINGLELINE or DT_CALCRECT);
      if Style = scalbsPlain then
        R.Top := R.Bottom - R1.Height - 6
      else
        R.Top := R.Bottom - R1.Height - 15;
      DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft));
    end;
end;

procedure TscAdvancedListBox.DrawListButton(ACanvas: TCanvas; AIndex: Integer; ADefaultTextColor: TColor);
var
  ButtonState: TscsCtrlState;
  R: TRect;
  TextColor: TColor;
  IX: Integer;
  S: String;
  SaveIndex: Integer;
  IIndex: Integer;
begin
  if not FItems[AIndex].Button.Visible then Exit;
  if (FItems[AIndex].Button.Down) and (FItems[AIndex].Button.MouseIn) then
    ButtonState := scsPressed
  else
  if FItems[AIndex].Button.MouseIn then
    ButtonState := scsHot
  else
  if (FItems[AIndex].Button.Enabled) and FItems[AIndex].Enabled then
    ButtonState := scsNormal
  else
    ButtonState := scsDisabled;

  R := FItems[AIndex].Button.ButtonRect;

  ACanvas.Font := FButtonFont;
  TextColor := FButtonFont.Color;
  S := FItems[AIndex].Button.Caption;
  case FItems[AIndex].Button.StyleKind of
   sclbsCustomImage:
     begin
       IIndex := FCustomButtonImageNormalIndex;
       case ButtonState of
         scsHot: IIndex := FCustomButtonImageHotIndex;
         scsPressed: IIndex := FCustomButtonImagePressedIndex;
         scsDisabled: IIndex := FCustomButtonImageDisabledIndex;
       end;
       if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(IIndex)
       then
         FCustomImages.Draw(ACanvas, R, IIndex);
       TextColor := GetStyleColor(TextColor);
     end;
   sclbsPushButton:
     begin
       SaveIndex := SaveDC(ACanvas.Handle);
       try
         DrawButton(ACanvas, R, ButtonState, False);
       finally
         RestoreDC(ACanvas.Handle, SaveIndex);
       end;
       TextColor := GetButtonTextColor(ButtonState);
     end;
   sclbsTransparent:
     begin
       if (IsCustomStyle and (seFont in StyleElements)) or (ButtonState = scsDisabled) then
        TextColor := ADefaultTextColor;
      end;
   end;

   ACanvas.Font.Color := TextColor;
   ACanvas.Brush.Style := bsClear;
   IX := FItems[AIndex].Button.ImageIndex;

   if (FButtonImages <> nil) and (IX >= 0) and
      (IX < FButtonImages.Count) then
   begin
     if FItems[AIndex].Button.Down and FItems[AIndex].Button.MouseIn and
         (FItems[AIndex].Button.ImagePressedIndex >= 0) and
         (FItems[AIndex].Button.ImagePressedIndex < FButtonImages.Count)
      then
        IX := Items[AIndex].Button.ImagePressedIndex
      else
      if Items[AIndex].Button.MouseIn and (Items[AIndex].Button.ImageHotIndex >= 0) and
        (Items[AIndex].Button.ImageHotIndex < FButtonImages.Count)
      then
        IX := Items[AIndex].Button.ImageHotIndex;
     InflateRect(R, -2, -2);
     if IsRightToLeft then
        DrawImageAndText(ACanvas, R, -1, 4,
        blGlyphRight, S, IX, FButtonImages,
         ButtonState <> scsDisabled, False, clBlack, False, True, False, FScaleFactor)
     else
       DrawImageAndText(ACanvas, R, -1, 4,
          blGlyphLeft, S, IX, FButtonImages,
         ButtonState <> scsDisabled, False, clBlack, False, False, False, FScaleFactor);
   end
   else
    if (S <> '') then
    begin
      DrawText(ACanvas.Handle,
       PChar(S), Length(S), R,
        scDrawTextBidiModeFlags(DT_VCENTER or DT_CENTER or DT_SINGLELINE,
          BidiMode = bdRightToLeft));
   end;
end;


procedure TscAdvancedListBox.DrawCustomCheckBox(ACanvas: TCanvas;
  ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
var
  CIndex: Integer;
begin
  CIndex := -1;
  case ACtrlState of
    scsNormal:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageIndex;
        cbChecked: CIndex := FCustomCheckedImageIndex;
      end;
    end;
    scsDisabled:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedDisabledImageIndex;
        cbChecked: CIndex := FCustomCheckedDisabledImageIndex;
      end;
    end;
  end;
  if CIndex = -1 then
  begin
    case ACheckBoxState of
      cbUnChecked: CIndex := FCustomUnCheckedImageIndex;
      cbChecked: CIndex := FCustomCheckedImageIndex;
    end;
  end;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIndex)
  then
    FCustomImages.Draw(ACanvas, ARect, CIndex, FScaleFactor);
end;

procedure TscAdvancedListBox.DrawItem(Index: Integer; Cnvs: TCanvas);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  R, IR, R1, CR, PR: TRect;
  C, FC, C1: TColor;
  TX, TY: Integer;
  SaveIndex: Integer;
  FGlowAlpha: Byte;
  Offset, CheckOffset: Integer;
  S: TPoint;
  FHasCustomBackground: Boolean;
begin
  if FItems[Index].Header then
  begin
    DrawHeaderItem(Index, Cnvs);
    Exit;
  end;

  if (Style = scalbsGroup) and
     (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FGroupBackgroundCustomImageIndex)
  then
    begin
      FCustomImages.Draw(Cnvs, FItems[Index].ItemRect, FGroupBackgroundCustomImageIndex, FScaleFactor);
      FHasCustomBackground := True;
    end
  else
    FHasCustomBackground := False;

  if (FItems[Index].FCustomColor <> clNone) and not FItems[Index].Active then
  begin
    Cnvs.Brush.Style := bsSolid;
    Cnvs.Brush.Color := GetStyleColor(FItems[Index].FCustomColor);
    if FItems[Index].FCustomColorAlpha = 255 then
      Cnvs.FillRect(FItems[Index].ItemRect)
    else
      FillRectWithAlpha(Cnvs, FItems[Index].ItemRect, FItems[Index].FCustomColorAlpha);
  end;

  if (seClient in StyleElements) and IsCustomStyle then
    C := GetEditBrushColor(scsNormal)
  else
  if Style <> scalbsPlain then
    C := Self.FGroupBackgroundColor
  else
    C := Self.Color;

  if (BackgroundStyle = scbgsColor) or (Style = scalbsGroup) then
  begin
    Cnvs.Brush.Style := bsSolid;
    if (Style = scalbsGroup) and not FHasCustomBackground then
    begin
      Cnvs.Brush.Color := C;
      Cnvs.FillRect(FItems[Index].ItemRect);
    end
    else
    if (FItems[Index].AlternateColor) and AlternateRow then
    begin
      Cnvs.Brush.Color := AlternateColor(C);
      Cnvs.FillRect(FItems[Index].ItemRect);
    end;
  end;

  if (FItems[Index].Active and FItems[Index].Enabled) and (SelectionStyle <> scastGlow) then
  begin
    if SelectionStyle = scastColor
    then
    begin
      if FSelectionColor <> clNone then
        Cnvs.Brush.Color := FSelectionColor
      else
        Cnvs.Brush.Color := GetStyleColor(clHighLight);

      if not Focused and not FShowFocusRect then
        FillRectWithAlpha(Cnvs, FItems[Index].ItemRect, 200)
      else
        Cnvs.FillRect(FItems[Index].ItemRect);

      if FSelectionColor <> clNone then
        FC := FSelectionTextColor
      else
      if IsCustomStyle then
        FC := GetSelectionTextColor
      else
        FC := clHighLightText;
    end
    else
    begin
      R := FItems[Index].ItemRect;
      if FShowLines or (Style <> scalbsPlain) then Dec(R.Bottom);
      if ((SelectionStyle = scastCustomImage) or (SelectionStyle = scastCustomImageWithGlow)) and
         (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomSelectionImageIndex) and
          FCustomImages.IsIndexAvailable(FCustomFocusedSelectionImageIndex)
      then
      begin
        if Self.Focused then
          FCustomImages.Draw(Cnvs, R, FCustomFocusedSelectionImageIndex, FScaleFactor)
        else
          FCustomImages.Draw(Cnvs, R, FCustomSelectionImageIndex, FScaleFactor);
        FC := GetStyleColor(FSelectionTextColor);
      end
      else
      begin
        DrawSelection(Cnvs, R, Self.Focused, FShowFocusRect);
        FC := GetSelectionTextColor;
      end;
    end
  end
  else
  begin
    if IsCustomStyle and (seFont in StyleElements) then
    begin
      if BackgroundStyle = scbgsColor then
        FC := GetEditTextColor(scsNormal)
      else
        FC := GetCheckBoxTextColor(scsNormal)
    end
    else
      FC := Font.Color;
    if (not FItems[Index].Enabled) or not Self.Enabled then
    begin
      if IsCustomStyle and (seFont in StyleElements) then
      begin
        if BackgroundStyle = scbgsColor then
          FC := GetEditTextColor(scsDisabled)
        else
          FC := GetCheckBoxTextColor(scsDisabled)
      end
      else
        FC := clGrayText;
    end;
  end;

  if not FItems[Index].Active and FItems[Index].Enabled and (FItems[Index].CustomTextColor <> clNone) then
    FC := GetStyleColor(FItems[Index].CustomTextColor);

  Cnvs.Font := Self.Font;
  Cnvs.Font.Color := FC;
  Cnvs.Brush.Color := C;
  Cnvs.Brush.Style := bsClear;

  R := FItems[Index].ItemRect;
  IR := R;

  SaveIndex := SaveDC(Cnvs.Handle);
  try
    IntersectClipRect(Cnvs.Handle, R.Left, R.Top, R.Right, R.Bottom);

  if FItems[Index].Indent > 0 then
  begin
    if BidiMode <> bdRightToLeft then
      Inc(R.Left, FItems[Index].Indent * FIndentMargin)
    else
      Dec(R.Right, FItems[Index].Indent * FIndentMargin);
  end;

  InflateRect(R, -2, -2);
  if Style <> scalbsPlain then
  begin
    if BidiMode = bdRightToLeft then
       Dec(R.Right, 8)
    else
      Inc(R.Left, 8);
  end;

  if FSelectionStyle = scastGlow then
  begin
    Inc(R.Left, FSelectionGlow.GlowSize div 2);
    Dec(R.Right, FSelectionGlow.GlowSize div 2);
  end
  else
  begin
    Inc(R.Left, 2);
    Dec(R.Right, 2);
  end;

  with FItems[Index] do
  begin
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Cnvs, Index, FItems[Index].ItemRect)
    else
      begin
        if FShowCheckBoxes then
        begin
          CR := R;
          S := GetCustomCheckBoxSize;
          if S.X = 0 then
            CheckOffset := GetCheckBoxSize(Cnvs, FScaleFactor) + 7
          else
            CheckOffset := S.X + 5;

          if CheckOffset < 20 then CheckOffset := 20;

          if BidiMode <> bdRightToLeft then
          begin
            Inc(CR.Left, 2);
            CR.Right := R.Left + CheckOffset;
            Inc(R.Left, CheckOffset + 2);
          end
          else
          begin
            Dec(CR.Right, 2);
            CR.Left := R.Right - CheckOffset;
            Dec(R.Right, CheckOffset +2);
          end;

          if S.X = 0 then
          begin
            if FItems[Index].Enabled and Self.Enabled then
            begin
              if Checked then
                DrawCheckBox(Cnvs, CR, scsNormal, cbChecked, FScaleFactor)
              else
                DrawCheckBox(Cnvs, CR, scsNormal, cbUnChecked, FScaleFactor);
            end
            else
            begin
              if Checked then
                DrawCheckBox(Cnvs, CR, scsDisabled, cbChecked, FScaleFactor)
              else
                DrawCheckBox(Cnvs, CR, scsDisabled, cbUnChecked, FScaleFactor);
            end;
          end
          else
          begin
            if FItems[Index].Enabled and Self.Enabled then
            begin
              if Checked then
                DrawCustomCheckBox(Cnvs, CR, scsNormal, cbChecked)
              else
                DrawCustomCheckBox(Cnvs, CR, scsNormal, cbUnChecked);
            end
            else
            begin
              if Checked then
                DrawCustomCheckBox(Cnvs, CR, scsDisabled, cbChecked)
              else
                DrawCustomCheckBox(Cnvs, CR, scsDisabled, cbUnChecked);
            end;
          end;
          if FStyle <> scalbsPlain then
            FCheckOffset := CheckOffset + 10
          else
            FCheckOffset := CheckOffset + 2;
        end;
        if (Title <> '') and FShowItemTitles then
        begin
          R1 := R;
          Cnvs.Font := TitleFont;
          if not Enabled or not Self.Enabled then Cnvs.Font.Color := FC;

          if IsCustomStyle and (seFont in StyleElements) or
            ((FItems[Index].Active and FItems[Index].Enabled) and (SelectionStyle <> scastGlow)) then
            Cnvs.Font.Color := FC;
          if not Self.Enabled then Cnvs.Font.Color := FC;

          R1.Bottom := R1.Top + Cnvs.TextHeight('Wq');
          DrawText(Cnvs.Handle, PChar(Title), Length(FTitle), R1,
            scDrawTextBidiModeFlags(DT_LEFT, BidiMode = bdRightToLeft));
          R.Top := R1.Bottom;
          Cnvs.Font := Self.Font;
          Cnvs.Font.Color := FC;
        end;
        if FShowItemProgressBars then
        begin
          R1 := R;
          R1.Top := R1.Bottom - FItemProgressBarHeight - 6;
          PR := R1;
          InflateRect(PR, -3, -3);
          if FItemProgressBarWidth > 0 then
            PR.Right := PR.Left + FItemProgressBarWidth;
          DrawProgressBar(Cnvs, PR, FItems[Index].ProgressBar.Min,
            FItems[Index].ProgressBar.Max, FItems[Index].ProgressBar.Value);
          R.Bottom := R1.Top;
        end;

        if FItems[Index].Button.Visible then
        begin
          R1 := R;
          R1.Top := IR.Top;
          R1.Bottom := IR.Bottom;
          if BidiMode <> bdRightToLeft then
            R1.Left := R.Right - Round(FItems[Index].Button.Width * FScaleFactor) - 2
          else
            R1.Right := R.Left + Round(FItems[Index].Button.Width * FScaleFactor) + 2;

          R1.Top := R1.Top + R1.Height div 2 - Round(FItems[Index].Button.Height * FScaleFactor) div 2;
          FItems[Index].Button.ButtonRect := Rect(R1.Left, R1.Top,
           R1.Left + Round(FItems[Index].Button.Width * FScaleFactor),
           R1.Top + Round(FItems[Index].Button.Height * FScaleFactor));
          if BidiMode <> bdRightToLeft then
            R.Right := R1.Left - 2
          else
            R.Left := R1.Right + 2;
        end;

        if FShowItemDetails  then
        begin
          R1 := R;
          Cnvs.Font := FDetailFont;
          if not Enabled or not Self.Enabled then Cnvs.Font.Color := FC;

          if IsCustomStyle and (seFont in StyleElements) or
            ((FItems[Index].Active and FItems[Index].Enabled) and (SelectionStyle <> scastGlow)) then
            Cnvs.Font.Color := FC;
          if not Self.Enabled then Cnvs.Font.Color := FC;

          if not FItems[Index].Active and FItems[Index].Enabled and (FItems[Index].CustomDetailTextColor <> clNone) then
            Cnvs.Font.Color := GetStyleColor(FItems[Index].CustomDetailTextColor)
          else
          if IsCustomStyle and (seFont in StyleElements) and
            not FItems[Index].Active then
          begin
            Cnvs.Font.Color := MiddleColor(C, Cnvs.Font.Color);
            Cnvs.Font.Color := MiddleColor(FC, Cnvs.Font.Color);
          end;

          if FDetailPosition = sclbdBottom then
          begin
            if Images <> nil then
              if BidiMode <> bdRightToLeft then
                Inc(R1.Left, Images.Width + FItemSpacing)
              else
                Dec(R1.Right, Images.Width + FItemSpacing);

            R1.Top := R1.Bottom - Cnvs.TextHeight('Wq') - 4;
            if Detail <> '' then
              DrawText(Cnvs.Handle, PChar(Detail), Length(FDetail), R1,
                scDrawTextBidiModeFlags(DT_LEFT, BidiMode = bdRightToLeft));
            R.Bottom := R1.Top;
          end
          else
          begin
            if not FDetailWordWrap then
            begin
              if BidiMode <> bdRightToLeft then
                R1.Left := R1.Right - Cnvs.TextWidth(Detail) - Round(5 * FScaleFactor)
              else
                R1.Right := R1.Left + Cnvs.TextWidth(Detail) + Round(5 * FScaleFactor);
              scDrawText(Cnvs, Detail, R1, BidiMode = bdRightToLeft, True);
            end
            else
            begin
              CR := Rect(0, 0, R1.Width div 3, R1.Height);
              DrawText(Cnvs.Handle, PChar(Detail), Length(Detail), CR,
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
              DrawText(Cnvs.Handle, PChar(Detail), Length(Detail), R1,
                scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK, BidiMode = bdRightToLeft));
            end;
            if BidiMode <> bdRightToLeft then
              R.Right := R1.Left
            else
              R.Left := R1.Right;
          end;

          Cnvs.Font := Self.Font;
          Cnvs.Font.Color := FC;
        end;

        if (FImages <> nil) and (ImageIndex >= 0) and
           (ImageIndex < FImages.Count) then
         begin
           FGlowAlpha := FSelectionGlow.AlphaValue;
           if not FShowFocusRect and not Self.Focused then
           begin
             if FGlowAlpha > 60 then
               FGlowAlpha := FGlowAlpha - 60;
           end;
           if ((SelectionStyle = scastGlow) or (SelectionStyle = scastCustomImageWithGlow)) and
           (FItems[Index].Active and FItems[Index].Enabled) then
            DrawImageAndTextWithGlow2(Cnvs, R, 0, FItemSpacing, GlyphLayout[IsRightToLeft],
              Caption, FImageIndex, FImages,
                FItems[Index].Enabled and Self.Enabled, False, clBlack,
                FSelectionGlow.Offset, FSelectionGlow.Color,
                 FSelectionGlow.GlowSize, FSelectionGlow.Intensive, FGlowAlpha, True,
                False, IsRightToLeft, True, FScaleFactor)
           else
             DrawImageAndText2(Cnvs, R, 0, FItemSpacing, GlyphLayout[IsRightToLeft],
              Caption, FImageIndex, FImages,
                FItems[Index].Enabled and Self.Enabled, False, clBlack, False, IsRightToLeft, True, FScaleFactor)
         end
         else
         begin
           if Images <> nil then Inc(R.Left, Images.Width + 5);
           R1 := Rect(0, 0, R.Width, R.Height);
           DrawText(Cnvs.Handle, PChar(Caption), Length(Caption), R1,
             DT_LEFT or DT_CALCRECT or DT_WORDBREAK);
           TX := R.Left;
           TY := R.Top + R.Height div 2 - R1.Height div 2;
           if TY < R.Top then TY := R.Top;
           R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
           FGlowAlpha := FSelectionGlow.AlphaValue;
           if not FShowFocusRect and not Self.Focused then
           begin
             if FGlowAlpha > 60 then
               FGlowAlpha := FGlowAlpha - 60;
           end;
           if ((SelectionStyle = scastGlow) or (SelectionStyle = scastCustomImageWithGlow))
              and (FItems[Index].Active and FItems[Index].Enabled) then
             DrawTextWithGlow(Cnvs, R, Caption, DT_WORDBREAK or DT_LEFT or DT_NOPREFIX,
              FSelectionGlow.Offset, FSelectionGlow.Color, FSelectionGlow.GlowSize,
                FSelectionGlow.Intensive, FGlowAlpha, IsRightToLeft, True)
           else
             DrawText(Cnvs.Handle, PChar(Caption), Length(Caption), R,
               scDrawTextBidiModeFlags(DT_WORDBREAK or DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft));
         end;
      end;

      if FItems[Index].Button.Visible then
      begin
        if not FItems[Index].Button.Enabled then
        begin
          if IsCustomStyle and (seFont in StyleElements) then
          begin
            if BackgroundStyle = scbgsColor then
              FC := GetEditTextColor(scsDisabled)
            else
              FC := GetCheckBoxTextColor(scsDisabled)
          end
          else
            FC := clGrayText;
        end;
        DrawListButton(Cnvs, Index, FC);
        Cnvs.Font := Self.Font;
        Cnvs.Font.Color := FC;
      end;

      if FStyle <> scalbsPlain then
      begin
        if (seClient in StyleElements) and IsCustomStyle then
          C := GetEditBrushColor(scsNormal)
        else
          C := FGroupBackgroundColor;
        if IsCustomStyle and (seFont in StyleElements) then
        begin
          if BackgroundStyle = scbgsColor then
           FC := GetEditTextColor(scsNormal)
          else
           FC := GetCheckBoxTextColor(scsNormal)
        end
        else
          FC := Font.Color;
        C1 := C;
        C := MiddleColor(C, FC);
        if (Index < FItems.Count - 1) and not FItems[Index + 1].Header then
          C := MiddleColor(C, C1);
        FC := Cnvs.Pen.Color;
        Cnvs.Pen.Color := C;
        Offset := 10;
        if ShowCheckBoxes then
          Inc(Offset, 15);
        if (Index < FItems.Count - 1) and not FItems[Index + 1].Header
        then
        begin
          if FShowLines and
             not ((SelectionStyle = scastColor) and (FItems[Index].Active)) then
          if Self.BiDiMode = bdRightToLeft then
          begin
            Cnvs.MoveTo(FItems[Index].ItemRect.Left, FItems[Index].ItemRect.Bottom - 1);
            Cnvs.LineTo(FItems[Index].ItemRect.Right - Offset, FItems[Index].ItemRect.Bottom - 1);
          end
          else
          begin
            Cnvs.MoveTo(FItems[Index].ItemRect.Left + Offset, FItems[Index].ItemRect.Bottom - 1);
            Cnvs.LineTo(FItems[Index].ItemRect.Right, FItems[Index].ItemRect.Bottom - 1);
          end;
        end
        else
        begin
          Cnvs.MoveTo(FItems[Index].ItemRect.Left, FItems[Index].ItemRect.Bottom - 1);
          Cnvs.LineTo(FItems[Index].ItemRect.Right, FItems[Index].ItemRect.Bottom - 1);
        end;
        Cnvs.Pen.Color := FC;
      end
      else
      if FShowLines
      then
        begin
          C := Cnvs.Pen.Color;
          if (seClient in StyleElements) and IsCustomStyle then
          begin
            C1 := GetStyleColor(clWindow);
            FC := GetStyleColor(clWindowText);
            C1 := MiddleColor(C1, FC);
            FC := GetStyleColor(clWindow);
            C1 := MiddleColor(C1, FC);
            Cnvs.Pen.Color := C1;
          end
          else
            Cnvs.Pen.Color := FLineColor;
          Cnvs.MoveTo(FItems[Index].ItemRect.Left, FItems[Index].ItemRect.Bottom - 1);
          Cnvs.LineTo(FItems[Index].ItemRect.Right, FItems[Index].ItemRect.Bottom - 1);
          Cnvs.Pen.Color := C;
        end;
    end;

  if (Self.ItemIndex = -1) and Focused and (Index = 0) and not FItems[Index].Header then
  begin
    R := FItems[Index].ItemRect;
    if FShowLines then Dec(R.Bottom);
    if FShowFocusRect then scDrawFocusRect(Cnvs, R, FScaleFactor);
  end
  else
  if (Self.ItemIndex = -1) and Focused and (Index = 1) and not FItems[Index].Header then
  begin
    R := FItems[Index].ItemRect;
    if FShowLines then Dec(R.Bottom);
    if FShowFocusRect then scDrawFocusRect(Cnvs, R, FScaleFactor);
  end
  else
  if FItems[Index].Active and Focused then
  begin
    R := FItems[Index].ItemRect;
    if FShowLines then Dec(R.Bottom);
    if FShowFocusRect then
    begin
      if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
        and (FSelectionStyle = scastStyled) then
        InflateRect(R, -1, -1);
      scDrawFocusRect(Cnvs, R, FScaleFactor);
    end;
  end;

  finally
    RestoreDC(Cnvs.Handle, SaveIndex);
  end;
end;

procedure TscAdvancedListBox.InitItemIndex(Value: Integer);
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

procedure TscAdvancedListBox.SetItemIndex(Value: Integer);
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
  if Value <> FItemIndex then
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
      if Assigned(FItems[FItemIndex].OnClick) then
        FItems[FItemIndex].OnClick(Self);
       if Assigned(FOnItemClick) then
        FOnItemClick(Self);
      Change;
    end;
  end;
end;

procedure TscAdvancedListBox.Change;
begin

end;

procedure TscAdvancedListBox.Loaded;
begin
  inherited;
end;

function TscAdvancedListBox.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].IsFilterVisible and FItems[I].IsVisible and
       PtInRect(FItems[I].ItemRect, Point(X, Y)) and (FItems[I].Enabled)
    then
      begin
        Result := I;
        Break;
      end;
end;

procedure TscAdvancedListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
   if (FOldButtonActive <> -1) and (FOldButtonActive < FItems.Count) and
     FItems[FOldButtonActive].Button.MouseIn then
  begin
    FItems[FOldButtonActive].Button.MouseIn := False;
    FOldButtonActive := -1;
    RePaintControl;
  end;
  if (FOldButtonActive <> -1) and (FOldButtonActive > FItems.Count - 1) then
    FOldButtonActive := -1;
end;

procedure TscAdvancedListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  FPressedButtonIndex := -1;
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft) then
  begin
    SetItemActive(I);
    FMouseDown := True;
    FMouseActive := I;
    if FItems[I].Button.Visible and FItems[I].Button.Enabled and
      PtInRect(FItems[I].Button.ButtonRect, Point(X, Y)) then
    begin
      FItems[I].Button.Down := True;
      FPressedButtonIndex := I;
      RePaintControl;
    end;
  end;
end;

procedure TscAdvancedListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
  P: TPoint;
  CanClick: Boolean;
begin
  inherited;
  CanClick := FMouseDown;
  FMouseDown := False;
  StopScrollTimer;

  I := ItemAtPos(X, Y);
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft) then ItemIndex := I;

  if CanClick then
  begin
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) and
       Assigned(FItems[FItemIndex].OnClick) then
      FItems[FItemIndex].OnClick(Self);

    if Assigned(FOnItemClick) then
      FOnItemClick(Self);

    Change;
  end;

  if FShowCheckBoxes and (I <> -1) and (I = ItemIndex) then
  begin
    if BidiMode <> bdRightToLeft then
    begin
      P.X := FItems[I].ItemRect.Left + FItems[I].Indent * FIndentMargin;
      if (X <= P.X + FCheckOffset) and (X >= P.X) then
      begin
        Items[I].Checked := not Items[I].Checked;
        if Assigned(Items[I].OnCheckClick) then Items[I].OnCheckClick(Self);
        if Assigned(FOnItemCheckClick) then FOnItemCheckClick(Self);
      end;
    end
    else
    begin
      P.X := FItems[I].ItemRect.Right - FItems[I].Indent * 10;
      if (X >= P.X - FCheckOffset) and (X <= P.X) then
      begin
        Items[I].Checked := not Items[I].Checked;
        if Assigned(Items[I].OnCheckClick) then Items[I].OnCheckClick(Self);
        if Assigned(FOnItemCheckClick) then FOnItemCheckClick(Self);
      end;
    end;
  end;
  if (I <> -1) and FItems[I].Button.Visible and FItems[I].Button.Enabled and
      PtInRect(FItems[I].Button.ButtonRect, Point(X, Y)) then
  begin
    FItems[I].Button.Down := False;
    RePaintControl;
    if Assigned(Items[I].OnButtonClick) then Items[I].OnButtonClick(Self);
    if Assigned(FOnItemButtonClick) then FOnItemButtonClick(Self);
  end
  else
  if (FPressedButtonIndex <> -1) and (FPressedButtonIndex < Items.Count) and
    FItems[FPressedButtonIndex].Button.Down then
  begin
    FItems[FPressedButtonIndex].Button.Down := False;
    RePaintControl;
  end;
  FPressedButtonIndex := -1;
end;

procedure TscAdvancedListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  R: TRect;
  FTempButtonIndex: Integer;
  FNeedUpdate: Boolean;
begin
  inherited;
  if FItems.Count = 0 then Exit;
  R := GetContentRect;
  FTempButtonIndex := -1;
  if PtInRect(R, Point(X, Y)) then
  begin
    StopScrollTimer;
    I := ItemAtPos(X, Y);
    if (I <> -1) and not (FItems[I].Header) and (FMouseDown or FMouseMoveChangeIndex)
       and (I <> FMouseActive) then
    begin
      SetItemActive(I);
      FMouseActive := I;
    end;
    if (I <> -1) and FItems[I].Button.Visible and FItems[I].Button.Enabled and
      PtInRect(FItems[I].Button.ButtonRect, Point(X, Y)) then
    begin
      FNeedUpdate := not FItems[I].Button.MouseIn;
      FItems[I].Button.MouseIn := True;

      if (FOldButtonActive <> -1) and (FOldButtonActive < FItems.Count) and
         (FOldButtonActive <> I) and
        FItems[FOldButtonActive].Button.MouseIn
      then
      begin
        FItems[FOldButtonActive].Button.MouseIn := False;
        FItems[FOldButtonActive].Button.Down := False;
      end;
      FTempButtonIndex := I;
      FOldButtonActive := I;
      if FNeedUpdate then RePaintControl;
    end
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
  if (FOldButtonActive <> -1) and (FOldButtonActive < FItems.Count) and
     (FOldButtonActive <> FTempButtonIndex) and
      FItems[FOldButtonActive].Button.MouseIn then
  begin
    FItems[FOldButtonActive].Button.MouseIn := False;
    FOldButtonActive := -1;
    RePaintControl;
  end;
  if (FOldButtonActive <> -1) and (FOldButtonActive > FItems.Count - 1) then
    FOldButtonActive := -1;
end;

procedure TscAdvancedListBox.SetItemActive(Value: Integer);
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

procedure TscAdvancedListBox.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FVertScrollBar = nil then Exit;
  if TWMMOUSEWHEEL(Message).WheelDelta < 0 then
    FVertScrollBar.Position :=  FVertScrollBar.Position + FVertScrollBar.SmallChange
  else
    FVertScrollBar.Position :=  FVertScrollBar.Position - FVertScrollBar.SmallChange;
end;

procedure TscAdvancedListBox.WMSETFOCUS(var Message: TWMSETFOCUS);
begin
  inherited;
  FUpdateParentBuffer := True;
  RePaint;
end;

procedure TscAdvancedListBox.WMKILLFOCUS(var Message: TWMKILLFOCUS);
begin
  inherited;
  RePaint;
end;

procedure TscAdvancedListBox.WndProc(var Message: TMessage);
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

procedure TscAdvancedListBox.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscAdvancedListBox.FindUp;
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

procedure TscAdvancedListBox.FindDown;
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

procedure TscAdvancedListBox.FindPageUp;
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

procedure TscAdvancedListBox.FindPageDown;
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

procedure TscAdvancedListBox.KeyPress(var Key: Char);
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

procedure TscAdvancedListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FShowCheckBoxes and (Key = 32) and (ItemIndex <> -1) then
  begin
    Items[ItemIndex].Checked := not Items[ItemIndex].Checked;
    if Assigned(FOnItemCheckClick) then FOnItemCheckClick(Self);
  end;
end;

procedure TscAdvancedListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited KeyDown(Key, Shift);
 case Key of
   VK_NEXT:  FindPageDown;
   VK_PRIOR: FindPageUp;
   VK_UP, VK_LEFT: FindUp;
   VK_DOWN, VK_RIGHT: FindDown;
 end;
end;

constructor TscPopupPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  DrawOnBackground := False;
  Visible := False;
end;

procedure TscPopupPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscPopupPanel.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscPopupPanel.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TscPopupPanel.Show(Origin: TPoint);
begin
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

constructor TscAdvancedPopupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FPopupMode := True;
  TabStop := False;
  DrawOnBackground := False;
  Visible := False;
end;

procedure TscAdvancedPopupListBox.DrawBorder(ACanvas: TCanvas; ARect: TRect);
begin
  DrawSingleBorder(ACanvas, ARect);
end;

procedure TscAdvancedPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscAdvancedPopupListBox.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscAdvancedPopupListBox.Hide;
begin
  FMouseActive := -1;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TscAdvancedPopupListBox.Show(Origin: TPoint);
begin
  if not IsCustomStyle and StyleServices.Enabled and
    (FVertScrollBar <> nil) and (FVertScrollBar.Visible) then
  begin
    FVertScrollBar.Perform(CM_RECREATEWND, 0, 0);
    AdjustScrollBars;
  end;
  FMouseActive := -1;
  FScrollOffset := 0;
  ScrollToItem(ItemIndex);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

constructor TscAdvancedCustomCombo.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptions := TscButtonColorOptions.Create;
  FColorOptions.OnChange := OnColorOptionsChange;
  FUseFontColorToStyleColor := False;
  FDropDownPosition := scdpRight;
  ParentColor := False;
  Color := clWindow;
  FSelectionGlow := TscGlowEffect.Create;
  FShowFocusRect := True;
  FBottomLineColor := clBtnShadow;
  FBottomActiveLineColor := clHighLight;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
  FCustomImageNormalIndex := -1;
  FCustomImageHotIndex := -1;
  FCustomImagePressedIndex := -1;
  FCustomImageFocusedIndex := -1;
  FCustomImageDisabledIndex := -1;
  FCustomArrowImageNormalIndex := -1;
  FCustomArrowImageHotIndex := -1;
  FCustomArrowImagePressedIndex := -1;
  FCustomArrowImageFocusedIndex := -1;
  FCustomArrowImageDisabledIndex := -1;
end;

destructor TscAdvancedCustomCombo.Destroy;
begin
  FColorOptions.Free;
  FSelectionGlow.Free;
  inherited;
end;

function TscAdvancedCustomCombo.GetCustomArrowWidth: Integer;
begin
  Result := 0;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomArrowImageNormalIndex)
  then
    Result := FCustomImages.GetWidth(FCustomArrowImageNormalIndex, FScaleFactor);
end;

procedure TscAdvancedCustomCombo.DrawArrow(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; AColor: TColor);
begin
  if GetCustomArrowWidth > 0 then
    DrawCustomArrow(ACanvas, ARect, ACtrlState)
  else
  begin
    if (Style = sccbToolButton) or (Style = sccbToolButtonTransparent) then
      DrawArrowImage(ACanvas, ARect, AColor, 4, FScaleFactor)
    else
      DrawComboArrowImage(ACanvas, ARect, AColor, FScaleFactor);
  end;
end;

procedure TscAdvancedCustomCombo.DrawCustomArrow(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState);
var
  IIndex: Integer;
begin
  IIndex := FCustomArrowImageNormalIndex;
  case ACtrlState of
    scsHot: IIndex := FCustomArrowImageHotIndex;
    scsPressed: IIndex := FCustomArrowImagePressedIndex;
    scsDisabled: IIndex := FCustomArrowImageDisabledIndex;
  end;
  if IIndex = -1 then
    IIndex := FCustomArrowImageNormalIndex;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(IIndex)
  then
    FCustomImages.Draw(ACanvas, ARect, IIndex, FScaleFactor);
end;

procedure TscAdvancedCustomCombo.SetCustomImageNormalIndex(Value: Integer);
begin
  if FCustomImageNormalIndex <> Value then
  begin
    FCustomImageNormalIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomImageHotIndex(Value: Integer);
begin
  if FCustomImageHotIndex <> Value then
  begin
    FCustomImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomImagePressedIndex(Value: Integer);
begin
  if FCustomImagePressedIndex <> Value then
  begin
    FCustomImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomImageDisabledIndex(Value: Integer);
begin
  if FCustomImageDisabledIndex <> Value then
  begin
    FCustomImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomImageFocusedIndex(Value: Integer);
begin
  if FCustomImageFocusedIndex <> Value then
  begin
    FCustomImageFocusedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomArrowImageNormalIndex(Value: Integer);
begin
  if FCustomArrowImageNormalIndex <> Value then
  begin
    FCustomArrowImageNormalIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomArrowImageHotIndex(Value: Integer);
begin
  if FCustomArrowImageHotIndex <> Value then
  begin
    FCustomArrowImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomArrowImagePressedIndex(Value: Integer);
begin
  if FCustomArrowImagePressedIndex <> Value then
  begin
    FCustomArrowImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomArrowImageDisabledIndex(Value: Integer);
begin
  if FCustomArrowImageDisabledIndex <> Value then
  begin
    FCustomArrowImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetCustomArrowImageFocusedIndex(Value: Integer);
begin
  if FCustomArrowImageFocusedIndex <> Value then
  begin
    FCustomArrowImageFocusedIndex := Value;
    RePaintControl;
  end;
end;


procedure TscAdvancedCustomCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscAdvancedCustomCombo.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.OnColorOptionsChange(Sender: TObject);
begin
  if (Style =  sccbColorCombo) or (Style =  sccbCustomImage) then
    RePaintControl;
end;

procedure TscAdvancedCustomCombo.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.DoMouseEnter;
begin
  if FDropDown then Exit;
  inherited;
end;

procedure TscAdvancedCustomCombo.DoMouseLeave;
begin
  if FDropDown then Exit;
  Button.MouseIn := False;
  inherited;
end;

procedure TscAdvancedCustomCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(Button.R, Point(X, Y)) and not Button.MouseIn then
  begin
    Button.MouseIn := True;
    RePaintControl;
  end
  else
  if not PtInRect(Button.R, Point(X, Y)) and Button.MouseIn then
  begin
    Button.MouseIn := False;
    RePaintControl;
  end;
end;

function TscAdvancedCustomCombo.CanDrawItem: Boolean;
begin
  Result := True;
end;

procedure TscAdvancedCustomCombo.DrawComboItem(ACanvas: TCanvas; ARect: TRect);
begin
end;

procedure TscAdvancedCustomCombo.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, R1, MR: TRect;
  ButtonState: TscsCtrlState;
  C, FC: TColor;
  SaveIndex, CIIndex, W, AW: Integer;
  FMarginLeft, FMarginTop, FMarginRight, FMarginBottom: Integer;
begin                             R := Rect(0, 0, Width, Height);
  Button.R := R;

  if FStyle = sccbCombo then
    W := GetSystemMetrics(SM_CXVSCROLL)
  else
    W := Round(17 * FScaleFactor);

  AW := GetCustomArrowWidth;
  if AW > W then W := AW;
  if IsCustomStyle then
    InflateRect(Button.R, -2, -2)
  else
    InflateRect(Button.R, -1, -1);
  if BiDiMode <> bdRightToLeft then
  begin
    Button.R.Left := Button.R.Right - W - 1;
    CBItem.R := Rect(2, 2, Button.R.Left - 1 , Height -  2);
  end
  else
  begin
    Button.R.Right := Button.R.Left + W + 1;
    CBItem.R := Rect(Button.R.Right + 1, 2, Width - 2, Height - 2);
  end;
  FC := Self.Font.Color;
  if (FStyle <> sccbCombo) and (FStyle <> sccbTransparent) then
    InflateRect(CBItem.R, -1, -1);
  case FStyle of
    sccbCombo:
    begin
      if Focused then
      begin
        DrawEditBorder(ACanvas, R, scsFocused);
        case SelectionStyle of
          scastStyled: FC := GetSelectionTextColor;
          scastColor:
            if FSelectionColor <> clNone then
              FC := FSelectionTextColor
            else
              FC := GetStyleColor(clHighLightText);
          scastGlow: FC := GetEditTextColor(scsNormal);
        end;
      end
      else
      begin
        if Enabled then
          DrawEditBorder(ACanvas, R, ACtrlState)
        else
          DrawEditBorder(ACanvas, R, scsNormal);
        if Enabled then
          FC := GetEditTextColor(scsNormal)
        else
          FC := GetEditTextColor(scsDisabled);
      end;
      with ACanvas do
      begin
        if (seClient in StyleElements) and IsCustomStyle then
          Brush.Color := GetEditBrushColor(scsNormal)
        else
          Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        InflateRect(R, -2, -2);
        FillRect(R);
      end;
      if Button.Down or FDropDown then
       ButtonState := scsPressed
      else
      if Button.MouseIn then
        ButtonState := scsHot
      else
      if Enabled then
        ButtonState := scsNormal
      else
        ButtonState := scsDisabled;
      DrawDropDownButton(ACanvas, Button.R, ButtonState, True,
        BidiMode = bdRightToLeft, FScaleFactor);
    end;
    sccbCustomImageOverContent:
    begin
      CIIndex := FCustomImageNormalIndex;
      case ACtrlState of
        scsHot: CIIndex := FCustomImageHotIndex;
        scsDisabled: CIIndex := FCustomImageDisabledIndex;
      end;
      if Focused and Enabled then
         CIIndex := FCustomImageFocusedIndex;
      if FDropDown then
        CIIndex := FCustomImagePressedIndex;
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIIndex)
      then
      begin
        MR := FCustomImages.GetContentMargins(CIIndex, FScaleFactor);
        FMarginLeft := MR.Left;
        FMarginTop := MR.Top;
        FMarginRight := MR.Right;
        FMarginBottom := MR.Bottom;
        if (FMarginLeft > 0) or (FMarginTop > 0) or
           (FMarginRight > 0) or (FMarginBottom > 0)
        then
        begin
          R1 := Rect(0, 0, Width, Height);
          Inc(R1.Left, FMarginLeft);
          Inc(R1.Top, FMarginTop);
          Dec(R1.Right, FMarginRight);
          Dec(R1.Bottom, FMarginBottom);
          Button.R := R1;
          W := GetSystemMetrics(SM_CXVSCROLL);
          AW := GetCustomArrowWidth;
          if AW > W then W := AW;
          if BiDiMode <> bdRightToLeft then
          begin
            Button.R.Right := Width - FMarginRight div 2;
            Button.R.Left := Button.R.Right - W - 1;
            CBItem.R := Rect(R1.Left, R1.Top, Button.R.Left - 1, R1.Bottom);
          end
          else
          begin
            Button.R.Left := FMarginLeft div 2;
            Button.R.Right := Button.R.Left + W + 1;
            CBItem.R := Rect(Button.R.Right + 1, R1.Top, R1.Right, R1.Bottom);
          end;
        end
      end;
      FColorOptions.State := ACtrlState;
      FC := FColorOptions.FontColor;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbCustomImage:
    begin
      CIIndex := FCustomImageNormalIndex;
      case ACtrlState of
        scsHot: CIIndex := FCustomImageHotIndex;
        scsDisabled: CIIndex := FCustomImageDisabledIndex;
      end;
      if Focused and Enabled then
          CIIndex := FCustomImageFocusedIndex;
      if FDropDown then
        CIIndex := FCustomImagePressedIndex;
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIIndex)
      then
      begin
        FCustomImages.Draw(ACanvas, R, CIIndex, FScaleFactor);
        MR := FCustomImages.GetMargins(CIIndex, FScaleFactor);
        FMarginLeft := MR.Left;
        FMarginTop := MR.Top;
        FMarginRight := MR.Right;
        FMarginBottom := MR.Bottom;
        if (FMarginLeft > 0) or (FMarginTop > 0) or
           (FMarginRight > 0) or (FMarginBottom > 0)
        then
        begin
          R1 := Rect(0, 0, Width, Height);
          Inc(R1.Left, FMarginLeft);
          Inc(R1.Top, FMarginTop);
          Dec(R1.Right, FMarginRight);
          Dec(R1.Bottom, FMarginBottom);
          Button.R := R1;
          W := GetSystemMetrics(SM_CXVSCROLL);
          AW := GetCustomArrowWidth;
          if AW > W then W := AW;
          if BiDiMode <> bdRightToLeft then
          begin
            Button.R.Right := Width - FMarginRight div 2;
            Button.R.Left := Button.R.Right - W - 1;
            CBItem.R := Rect(R1.Left, R1.Top, Button.R.Left - 1, R1.Bottom);
          end
          else
          begin
            Button.R.Left := FMarginLeft div 2;
            Button.R.Right := Button.R.Left + W + 1;
            CBItem.R := Rect(Button.R.Right + 1, R1.Top, R1.Right, R1.Bottom);
          end;
        end
      end;
      FColorOptions.State := ACtrlState;
      FC := FColorOptions.FontColor;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbColorCombo:
    begin
      FColorOptions.State := ACtrlState;
      if FColorOptions.Color <> clNone then
       begin
         ACanvas.Brush.Color := FColorOptions.Color;
         ACanvas.FillRect(R);
      end;
      if (FColorOptions.FrameColor <> clNone) and (FColorOptions.FrameWidth > 0) then
      begin
        R1 := R;
        ACanvas.Pen.Color := FColorOptions.FrameColor;
        Frame3D(ACanvas, R1, FColorOptions.FrameColor, FColorOptions.FrameColor,
          FColorOptions.FrameWidth);
      end;
      FC := FColorOptions.FontColor;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbPushButton:
    begin
      if FDropDown then
        ButtonState := scsPressed
      else
      if Focused then
        ButtonState := scsHot
      else
        ButtonState := ACtrlState;
      DrawButton(ACanvas, R, ButtonState, False);
      C := GetButtonTextColor(ButtonState);
      FC := C;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbToolButton:
    begin
      if FDropDown then
        ButtonState := scsPressed
      else
        ButtonState := ACtrlState;
      DrawToolButton (ACanvas, R, ButtonState);
      C := GetToolButtonTextColor(ButtonState);
      FC := C;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbPushButtonTransparent:
    begin
      if FDropDown then
        ButtonState := scsPressed
      else
      if Focused then
        ButtonState := scsHot
      else
        ButtonState := ACtrlState;
      if (ButtonState = scsPressed) or (ButtonState = scsHot) then
      begin
        DrawButton(ACanvas, R, ButtonState, False);
        C := GetButtonTextColor(ButtonState);
      end
      else
      begin
        if FUseFontColorToStyleColor and Enabled then
          C := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          C := GetCheckBoxTextColor(ButtonState);
      end;
      FC := C;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbToolButtonTransparent:
    begin
      if FDropDown then
        ButtonState := scsPressed
      else
        ButtonState := ACtrlState;
      if (ButtonState = scsPressed) or (ButtonState = scsHot) then
      begin
        DrawToolButton(ACanvas, R, ButtonState);
        C := GetToolButtonTextColor(ButtonState);
      end
      else
      begin
        if FUseFontColorToStyleColor and Enabled then
          C := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          C := GetCheckBoxTextColor(ButtonState);
      end;
      FC := C;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbTransparent:
    begin
      if FUseFontColorToStyleColor and Enabled then
        C := ColorToRGB(GetStyleColor(Self.Font.Color))
      else
        C := GetCheckBoxTextColor(ACtrlState);
      FC := C;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
    end;
    sccbTransparentBottomLine, sccbTransparentActiveBottomLine:
    begin
      if FUseFontColorToStyleColor and Enabled then
        C := ColorToRGB(GetStyleColor(Self.Font.Color))
      else
        C := GetCheckBoxTextColor(ACtrlState);
      FC := C;
      DrawArrow(ACanvas, Button.R, ACtrlState, FC);
      if ((ACtrlState = scsHot) or Focused) and (FStyle = sccbTransparentActiveBottomLine) then
        C := GetStyleColor(FBottomActiveLineColor)
      else
      if (seBorder in StyleElements) and IsCustomStyle then
      begin
        C := MiddleColor(GetStyleColor(clBtnFace), GetCheckBoxTextColor(scsNormal));
        C := MiddleColor(GetStyleColor(clBtnFace), C);
      end
      else
        C := FBottomLineColor;
      ACanvas.Pen.Color := C;
      ACanvas.MoveTo(0, Height - 1);
      ACanvas.LineTo(Width, Height - 1);
    end
  end;
  SaveIndex := SaveDC(ACanvas.Handle);
  IntersectClipRect(ACanvas.Handle,
    CBItem.R.Left, CBItem.R.Top, CBItem.R.Right, CBItem.R.Bottom);
  try
    if CanDrawItem then
    begin
      if (Style = sccbCombo) and Focused and (SelectionStyle <> scastGlow) and not FHideSelection then
      begin
        if SelectionStyle = scastStyled then
          DrawSelection(ACanvas, CBItem.R, True, FShowFocusRect)
        else
        with ACanvas do
        begin
          if FSelectionColor <> clNone then
            Brush.Color := FSelectionColor
          else
            Brush.Color := GetStyleColor(clHighLight);
          FillRect(CBItem.R);
        end;
        if ((FSelectionColor <> clNone) and (FSelectionStyle = scastColor)) or
           ((FSelectionStyle = scastColor) and not IsCustomStyle)
         then
          FC := FSelectionTextColor
        else
          FC := GetSelectionTextColor;
      end;
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := FC;
      DrawComboItem(ACanvas, CBItem.R);
    end;
    if FShowFocusRect and Focused then
      scDrawFocusRect(ACanvas, CBItem.R, FScaleFactor);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
  case FStyle of
    sccbCustomImageOverContent:
    begin
      R := Rect(0, 0, Width, Height);
       CIIndex := FCustomImageNormalIndex;
      case ACtrlState of
        scsHot: CIIndex := FCustomImageHotIndex;
        scsDisabled: CIIndex := FCustomImageDisabledIndex;
      end;
      if Focused and Enabled then
          CIIndex := FCustomImageFocusedIndex;
      if FDropDown then
        CIIndex := FCustomImagePressedIndex;
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIIndex)
      then
        FCustomImages.Draw(ACanvas, R, CIIndex, FScaleFactor);
    end;
  end;
end;

procedure TscAdvancedCustomCombo.SetBottomLineColor;
begin
  if FBottomLineColor <> Value then
  begin
    FBottomLineColor := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomCombo.SetStyle(Value: TscAdvancedComboStyle);
begin
  if FStyle <> Value then
  begin
    if FStyle <> sccbCombo then
    begin
      TransparentBackground := False;
      GetParentBG;
    end;
    FStyle := Value;
    if FStyle <> sccbCombo then
    begin
      TransparentBackground := True;
      GetParentBG;
    end;
    RePaintControl;
  end;
end;

constructor TscAdvancedCustomComboBox.Create;
begin
  inherited Create(AOwner);
  FListBox := TscAdvancedPopupListBox.Create(Self);
  FCheckedListMode := False;
  FCheckedListWrap := True;
  FAutoComplete := False;
  FSearchString := '';
  FSearchTimerEnabled := False;
  FListBoxWallpaperIndex := -1;
  FListBoxWallpapers := nil;
  FShowItemTitle := True;
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

procedure TscAdvancedCustomComboBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FListBoxWidth := MulDiv(FListBoxWidth, M, D);
  FListBoxHeight := MulDiv(FListBoxHeight, M, D);
end;

function TscAdvancedCustomComboBox.CanAnimate: Boolean;
begin
  Result := inherited CanAnimate;
  if Result then
     Result := not FListBox.Visible;
end;

procedure TscAdvancedCustomComboBox.Sort;
begin
  FListBox.Sort;
end;

procedure TscAdvancedCustomComboBox.Add(const Item: String);
begin
  FListBox.Add(Item);
end;

procedure TscAdvancedCustomComboBox.Add(Items: TStrings);
begin
  FListBox.Add(Items);
end;

procedure TscAdvancedCustomComboBox.Delete(Index: Integer);
begin
  FListBox.Delete(Index);
end;

procedure TscAdvancedCustomComboBox.Clear;
begin
  FListBox.Clear;
  RePaintControl;
end;

function TscAdvancedCustomComboBox.NextIndex(const S: string): Integer;
begin
  Result := FListBox.NextIndex(S);
end;

function TscAdvancedCustomComboBox.IndexOfCaption(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FListBox.IndexOfCaption(S, AStartOff);
end;

function TscAdvancedCustomComboBox.IndexOfTitle(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FListBox.IndexOfTitle(S, AStartOff);
end;

function TscAdvancedCustomComboBox.IndexOfDetail(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FListBox.IndexOfDetail(S, AStartOff);
end;

procedure TscAdvancedCustomComboBox.BeginUpdateItems;
begin
  FListBox.BeginUpdateItems;
end;

procedure TscAdvancedCustomComboBox.EndUpdateItems;
begin
  FListBox.EndUpdateItems;
end;

function TscAdvancedCustomComboBox.CanDrawItem: Boolean;
var
  Index: Integer;
begin
  if FDropDown then Index := FOldItemIndex else Index := ItemIndex;
  if FCheckedListMode then
    Result := True
  else
    Result := (Index >= 0) and (Index < Items.Count);
end;

procedure TscAdvancedCustomComboBox.DrawComboItem(ACanvas: TCanvas; ARect: TRect);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  R, R1, CR: TRect;
  FC: TColor;
  TX, TY: Integer;
  S: String;
  Index: Integer;
  I: Integer;
begin
  R := ARect;
  InflateRect(R, -2, -2);
  Inc(R.Left, 2);
  Dec(R.Right, 2);
  if not Assigned(Items) then Exit;

  if FDropDown then Index := FOldItemIndex else Index := ItemIndex;

  if FCheckedListMode then
  begin
    if Assigned(FOnDrawItem) then
    begin
      FOnDrawItem(ACanvas, Index, R);
      Exit;
    end;
    if Items.Count > 0 then
    begin
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
        ACanvas.Brush.Style := bsClear;
        R1 := Rect(0, 0, R.Width, R.Height);
        DrawText(ACanvas.Handle, PChar(S), Length(S), R1,
          DT_LEFT or DT_CALCRECT or DT_NOPREFIX or WordWraps[FCheckedListWrap]);
        TX := R.Left;
        TY := R.Top + R.Height div 2 - R1.Height div 2;
        if TY < R.Top then TY := R.Top;
        R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
        DrawText(ACanvas.Handle, PChar(S), Length(S), R,
          scDrawTextBidiModeFlags(WordWraps[FCheckedListWrap] or DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft));
      end;
    end;
    Exit;
  end;

  if not ((Index >= 0) and (ItemIndex < Items.Count)) then Exit;

  ACanvas.Brush.Style := bsClear;

  if Assigned(FOnDrawItem) then
  begin
    FOnDrawItem(ACanvas, Index, R);
    Exit;
  end;


  with Items[Index] do
  begin
    if (Title <> '') and FShowItemTitle then
    begin
      R1 := R;
      FC := ACanvas.Font.Color;
      ACanvas.Font := TitleFont;
      if ((IsCustomStyle and (seFont in StyleElements)) or not Self.Enabled) or
         Focused
       then
        ACanvas.Font.Color := FC;
      R1.Bottom := R1.Top + ACanvas.TextHeight(Title);
      DrawText(ACanvas.Handle, PChar(Title), Length(FTitle), R1,
        scDrawTextBidiModeFlags(DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft));
      R.Top := R1.Bottom;
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := FC;
    end;

    if FShowItemDetail and ListBoxShowItemDetails then
    begin
      R1 := R;
      FC := ACanvas.Font.Color;
      ACanvas.Font := DetailFont;

      if ((IsCustomStyle and (seFont in StyleElements))
         or not Self.Enabled) or Focused
      then
        ACanvas.Font.Color := FC;

      if (Items[Index].CustomDetailTextColor <> clNone) and Items[Index].Enabled then
        ACanvas.Font.Color := GetStyleColor(Items[Index].FCustomDetailTextColor)
      else
      if IsCustomStyle and (seFont in StyleElements) and
          not Focused then
      begin
        ACanvas.Font.Color := MiddleColor(FC, GetStyleColor(clWindow));
        ACanvas.Font.Color := MiddleColor(FC, ACanvas.Font.Color);
      end;

      if DetailPosition = sclbdBottom then
      begin
        if (Images <> nil) and FShowItemImage then
          if BidiMode <> bdRightToLeft then
            Inc(R1.Left, Images.Width + 5)
          else
            Dec(R1.Right, Images.Width + 5);

        R1.Top := R1.Bottom - ACanvas.TextHeight('Wq') - 4;

        DrawText(ACanvas.Handle, PChar(Detail), Length(FDetail), R1,
           scDrawTextBidiModeFlags(DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft));
        R.Bottom := R1.Top;
      end
      else
        if FDetail <> '' then
        begin
          if not DetailWordWrap then
          begin
            if BidiMode <> bdRightToLeft then
              R1.Left := R1.Right - ACanvas.TextWidth(FDetail) - Round(4 * FScaleFactor) - 4
            else
              R1.Right := R1.Left + ACanvas.TextWidth(FDetail) + Round(4 * FScaleFactor) + 4;

            scDrawText(ACanvas, FDetail, R1, BidiMode = bdRightToLeft, True);
          end
          else
          begin
            CR := Rect(0, 0, R1.Width div 3, R1.Height);
            DrawText(ACanvas.Handle, PChar(FDetail), Length(FDetail), CR,
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
            DrawText(ACanvas.Handle, PChar(FDetail), Length(FDetail), R1,
              scDrawTextBidiModeFlags(DT_RIGHT or DT_NOPREFIX or DT_WORDBREAK, BidiMode = bdRightToLeft));
          end;

        if BidiMode <> bdRightToLeft then
          R.Right := R1.Left
        else
          R.Left := R1.Right;
      end;
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := FC;
    end;

    if (Items[Index].CustomTextColor <> clNone) and Items[Index].Enabled then
        ACanvas.Font.Color := GetStyleColor(Items[Index].FCustomTextColor);

    if (Images <> nil) and (ImageIndex >= 0) and
       (ImageIndex < Images.Count) and FShowItemImage then
    begin
      S := Caption;
      if not FShowItemText then S := '';
      if not HideSelection and (SelectionStyle = scastGlow) and (Focused and Self.Enabled) then
        DrawImageAndTextWithGlow2(ACanvas, R, 0, 5, GlyphLayout[IsRightToLeft],
          S, FImageIndex, Images, Self.Enabled, False, clBlack,
          FSelectionGlow.Offset, FSelectionGlow.Color,
          FSelectionGlow.GlowSize, FSelectionGlow.Intensive,
          FSelectionGlow.AlphaValue, True, False, IsRightToLeft, True, FScaleFactor)
      else
        DrawImageAndText2(ACanvas, R, 0, 5, GlyphLayout[IsRightToLeft],
          S, FImageIndex, Images,
          Self.Enabled, False, clBlack, False, IsRightToLeft, True, FScaleFactor)
    end
    else
    if FShowItemText then
    begin
      if (Images <> nil) and FShowItemImage then Inc(R.Left, Images.Width + 5);
      R1 := Rect(0, 0, R.Width, R.Height);
      DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R1,
        DT_LEFT or DT_CALCRECT or DT_WORDBREAK  or DT_NOPREFIX);
      TX := R.Left;
      TY := R.Top + R.Height div 2 - R1.Height div 2;
      if TY < R.Top then TY := R.Top;
      R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
      if not HideSelection and (SelectionStyle = scastGlow) and (Focused and Self.Enabled) then
       DrawTextWithGlow(ACanvas, R, Caption, DT_WORDBREAK or DT_LEFT,
          FSelectionGlow.Offset, FSelectionGlow.Color, FSelectionGlow.GlowSize,
          FSelectionGlow.Intensive,FSelectionGlow.AlphaValue, IsRightToLeft, True)
      else
        DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R,
        scDrawTextBidiModeFlags(DT_WORDBREAK or DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft));
    end;
  end;
end;


procedure TscAdvancedCustomComboBox.ListBoxWindowProcHook(var Message: TMessage);
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

procedure TscAdvancedCustomComboBox.KeyPress;
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

destructor TscAdvancedCustomComboBox.Destroy;
begin
  FlistBox.Free;
  FlistBox := nil;
  inherited;
end;

procedure TscAdvancedCustomComboBox.CMEnabledChanged;
begin
  inherited;
  RePaintControl;
end;

function TscAdvancedCustomComboBox.GetAlternateRow: Boolean;
begin
  Result := FListBox.AlternateRow;
end;

procedure TscAdvancedCustomComboBox.SetCheckedListWrap(Value: Boolean);
begin
  if FCheckedListWrap <> Value then
  begin
    FCheckedListWrap := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomComboBox.SetCheckedListMode(Value: Boolean);
begin
  if FCheckedListMode <> Value then
  begin
    FCheckedListMode := Value;
    FListBox.ShowCheckBoxes := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomComboBox.SetAlternateRow(Value: Boolean);
begin
  FListBox.AlternateRow := Value;
end;

procedure TscAdvancedCustomComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
  if (Operation = opRemove) and (AComponent = FListBoxWallpapers) then
   FListBoxWallpapers := nil;
end;

function TscAdvancedCustomComboBox.GetImages: TCustomImageList;
begin
  if FListBox <> nil
  then
    Result := FListBox.Images
   else
    Result := nil;
end;

procedure TscAdvancedCustomComboBox.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (Message.wParam <> FListBox.Handle) and
      not ((FListBox.FVertScrollBar <>  nil) and
        (Message.wParam = FListBox.FVertScrollBar.Handle))
  then
    CloseUp(False);
end;

procedure TscAdvancedCustomComboBox.CMCancelMode;
begin
  inherited;
  if (Message.Sender = nil) or (
     (Message.Sender <> Self) and
     (Message.Sender <> Self.FListBox) and
     (Message.Sender <> Self.FListBox.FVertScrollBar))
  then
    CloseUp(False);
end;

procedure TscAdvancedCustomComboBox.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscAdvancedCustomComboBox.CheckButtonClick;
begin
  CloseUp(True);
end;

procedure TscAdvancedCustomComboBox.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  case Msg.CharCode of
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:  Msg.Result := 1;
  end;
end;

procedure TscAdvancedCustomComboBox.KeyDown;
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

procedure TscAdvancedCustomComboBox.WMMOUSEWHEEL;
begin
  inherited;
  if not FCheckedListMode then
    if TWMMOUSEWHEEL(Message).WheelDelta > 0
    then
      ComboKeyUp(not FListBox.Visible)
    else
      ComboKeyDown(not FListBox.Visible);
end;

procedure TscAdvancedCustomComboBox.WMSETFOCUS;
begin
  inherited;
  FUpdateParentBuffer := True;
  RePaint;
end;

procedure TscAdvancedCustomComboBox.WMKILLFOCUS;
begin
  inherited;
  if FListBox.Visible  then CloseUp(False);
  RePaint;
end;

function TscAdvancedCustomComboBox.GetItemIndex;
begin
  Result := FListBox.ItemIndex;
end;

procedure TscAdvancedCustomComboBox.InitItemIndex(Value: Integer);
begin
  FListBox.InitItemIndex(Value);
  FOldItemIndex := FListBox.ItemIndex;
end;

procedure TscAdvancedCustomComboBox.SetItemIndex;
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

function TscAdvancedCustomComboBox.IsPopupVisible: Boolean;
begin
  Result := FListBox.Visible;
end;

function TscAdvancedCustomComboBox.CanCancelDropDown;
begin
  Result := FListBox.Visible and not FMouseIn;
end;

procedure TscAdvancedCustomComboBox.EnableScrollTimer(Value: Integer);
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

procedure TscAdvancedCustomComboBox.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscAdvancedCustomComboBox.WMTimer;
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

procedure TscAdvancedCustomComboBox.ProcessListBox;
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := FListBox.ScreenToClient(P);
  if (P.Y < 0) and (FListBox.FVertScrollBar <> nil) and WasInLB and FLBDown
  then
    EnableScrollTimer(1)
  else
  if (P.Y > FListBox.Height) and (FListBox.FVertScrollBar <> nil) and WasInLB and FLBDown
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

procedure TscAdvancedCustomComboBox.SetDropDownCount(Value: Integer);
begin
  if Value >= 0
  then
    FDropDownCount := Value;
end;

procedure TscAdvancedCustomComboBox.SetItems;
begin
  FListBox.Items.Assign(Value);
end;

function TscAdvancedCustomComboBox.GetItems;
begin
  Result := FListBox.Items;
end;

procedure TscAdvancedCustomComboBox.MouseDown;
begin
  inherited;
  if not Focused then SetFocus;
  if Button <> mbLeft then Exit;
  if Self.Button.MouseIn or
     PtInRect(CBItem.R, Point(X, Y)) or (FStyle <> sccbCombo)
  then
    begin
      Self.Button.Down := True;
      RePaintControl;
      if FListBox.Visible then CloseUp(False)
      else
        begin
          WasInLB := False;
          FLBDown := True;
          DropDown;
        end;
    end
  else
    if FListBox.Visible then CloseUp(False);
end;

procedure TscAdvancedCustomComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
  P: TPoint;
begin
  if FLBDown and WasInLB
  then
    begin
      ReleaseCapture;
      FLBDown := False;
      if FCheckedListMode and (FListBox.ItemIndex >=0) and (FListBox.ItemIndex < FListBox.Items.Count) then
      begin
        FListBox.Items[FListBox.ItemIndex].Checked := not FListBox.Items[FListBox.ItemIndex].Checked;
        if Assigned(FOnItemCheckClick) then
           FOnItemCheckClick(Self);
        RePaintControl;
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
  if Self.Button.Down
  then
    begin
      Self.Button.Down := False;
      RePaintControl;
    end;
end;

procedure TscAdvancedCustomComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FListBox.Visible then ProcessListBox;
end;

procedure TscAdvancedCustomComboBox.CloseUp;
begin
  SC_UnHookMouseMessages;
  if FTimerMode <> 0 then StopScrollTimer;
  if not FListBox.Visible then Exit;
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

procedure TscAdvancedCustomComboBox.DropDown;
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

  if Assigned(FOnListBoxDrawItem) then
    FListBox.OnDrawItem := Self.OnListBoxDrawItem
  else
    FListBox.OnDrawItem := Self.OnDrawItem;

  FListBox.Font.Assign(Font);
  FListBox.Color := Color;
  FListBox.BidiMode := Self.BiDiMode;
  FListBox.StyleElements := StyleElements;
  FListBox.Wallpapers := ListBoxWallpapers;
  FListBox.CustomImages := Self.CustomImages;
  FListBox.WallpaperIndex := ListBoxWallpaperIndex;

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

procedure TscAdvancedCustomComboBox.ComboPageUp;
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindPageUp;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscAdvancedCustomComboBox.ComboPageDown(AChange: Boolean);
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindPageDown;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscAdvancedCustomComboBox.ComboKeyUp;
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindUp;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscAdvancedCustomComboBox.ComboKeyDown;
begin
  if FCheckedListMode and not FListBox.Visible then Exit;
  FListBox.FindDown;
  if AChange then ItemIndex := FListBox.ItemIndex;
end;

procedure TscAdvancedCustomComboBox.SetImages(Value: TCustomImageList);
begin
  if FListBox.Images <> Value then
  begin
    FListBox.Images := Value;
    RePaintControl;
  end;
end;

function TscAdvancedCustomComboBox.GetHeaderFont: TFont;
begin
  Result := FListBox.HeaderFont;
end;

procedure TscAdvancedCustomComboBox.SetHeaderFont(Value: TFont);
begin
  FListBox.HeaderFont.Assign(Value);
end;

function TscAdvancedCustomComboBox.GetDetailFont: TFont;
begin
  Result := FListBox.DetailFont;
end;

procedure TscAdvancedCustomComboBox.SetDetailFont(Value: TFont);
begin
  FListBox.DetailFont.Assign(Value);
end;

function TscAdvancedCustomComboBox.GetTitleFont: TFont;
begin
  Result := FListBox.TitleFont;
end;

procedure TscAdvancedCustomComboBox.SetTitleFont(Value: TFont);
begin
  FListBox.TitleFont.Assign(Value);
end;

procedure TscAdvancedCustomComboBox.SetListBoxWallpapers(Value: TscCustomImageCollection);
begin
  FListBoxWallpapers := Value;
end;

procedure TscAdvancedCustomComboBox.SetListBoxWallpaperIndex(Value: Integer);
begin
  FListBoxWallpaperIndex := Value;
end;

function TscAdvancedCustomComboBox.GetDetailPosition: TscListBoxDetailPosition;
begin
  Result := FListBox.DetailPosition;
end;

procedure TscAdvancedCustomComboBox.SetDetailPosition(Value: TscListBoxDetailPosition);
begin
  if FListBox.DetailPosition <> Value then
  begin
    FListBox.DetailPosition := Value;
    RePaintControl;
  end;
end;

function TscAdvancedCustomComboBox.GetDetailWordWrap: Boolean;
begin
  Result := FListBox.DetailWordWrap;
end;

procedure TscAdvancedCustomComboBox.SetDetailWordWrap(Value: Boolean);
begin
  if FListBox.DetailWordWrap <> Value then
  begin
    FListBox.DetailWordWrap := Value;
    RePaintControl;
  end;
end;

function TscAdvancedCustomComboBox.GetListBoxIndentMargin: Integer;
begin
  Result := FListBox.IndentMargin;
end;

procedure TscAdvancedCustomComboBox.SetListBoxIndentMargin(Value: Integer);
begin
  FListBox.IndentMargin := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxCustomHeaderImageIndex: Integer;
begin
  Result := FListBox.CustomHeaderImageIndex;
end;

procedure TscAdvancedCustomComboBox.SetListBoxCustomHeaderImageIndex(Value: Integer);
begin
  FListBox.CustomHeaderImageIndex := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxCustomSelectionImageIndex: Integer;
begin
  Result := FListBox.CustomSelectionImageIndex;
end;

procedure TscAdvancedCustomComboBox.SetListBoxCustomSelectionImageIndex(Value: Integer);
begin
  FListBox.CustomSelectionImageIndex := Value;
  FListBox.CustomFocusedSelectionImageIndex := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxHeaderUseStyleColor: Boolean;
begin
  Result := FListBox.HeaderUseStyleColor;
end;

procedure TscAdvancedCustomComboBox.SetListBoxHeaderUseStyleColor(Value: Boolean);
begin
  FListBox.HeaderUseStyleColor := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxLineColor: TColor;
begin
  Result := FListBox.LineColor;
end;

procedure TscAdvancedCustomComboBox.SetListBoxLineColor(Value: TColor);
begin
  FListBox.LineColor := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxHeaderStyle: TscAdvancedHeaderStyle;
begin
  Result := FListBox.HeaderStyle;
end;

procedure TscAdvancedCustomComboBox.SetListBoxHeaderStyle(Value: TscAdvancedHeaderStyle);
begin
  FListBox.HeaderStyle := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxSelectionColor: TColor;
begin
  Result := FListBox.SelectionColor;
end;

procedure TscAdvancedCustomComboBox.SetListBoxSelectionColor(Value: TColor);
begin
  FListBox.SelectionColor := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxSelectionTextColor: TColor;
begin
  Result := FListBox.SelectionTextColor;
end;

procedure TscAdvancedCustomComboBox.SetListBoxSelectionTextColor(Value: TColor);
begin
  FListBox.SelectionTextColor := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxSelectionStyle: TscAdvancedSelectionStyle;
begin
  Result := FListBox.SelectionStyle;
end;

procedure TscAdvancedCustomComboBox.SetListBoxSelectionStyle(Value: TscAdvancedSelectionStyle);
begin
  FListBox.SelectionStyle := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxShowItemDetails: Boolean;
begin
  Result := FListBox.ShowItemDetails;
end;

procedure TscAdvancedCustomComboBox.SetListBoxShowItemDetails(Value: Boolean);
begin
  FListBox.ShowItemDetails := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxShowItemTitles: Boolean;
begin
  Result := FListBox.ShowItemTitles;
end;

procedure TscAdvancedCustomComboBox.SetListBoxShowItemTitles(Value: Boolean);
begin
  FListBox.ShowItemTitles := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxShowLines: Boolean;
begin
  Result := FListBox.ShowLines;
end;

procedure TscAdvancedCustomComboBox.SetListBoxShowLines(Value: Boolean);
begin
  FListBox.ShowLines := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxItemHeight: Integer;
begin
  Result := FlistBox.ItemHeight;
end;

procedure TscAdvancedCustomComboBox.SetListBoxItemHeight(Value: Integer);
begin
  FlistBox.ItemHeight := Value;
end;

function TscAdvancedCustomComboBox.GetListBoxHeaderHeight: Integer;
begin
  Result := FListBox.HeaderHeight;
end;

procedure TscAdvancedCustomComboBox.SetListBoxHeaderHeight(Value: Integer);
begin
  FListBox.HeaderHeight := Value;
end;

procedure TscAdvancedCustomComboBox.SetShowItemDetail(Value: Boolean);
begin
  if FShowItemDetail <> Value then
  begin
    FShowItemDetail := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomComboBox.SetShowItemTitle(Value: Boolean);
begin
  if FShowItemTitle <> Value then
  begin
    FShowItemTitle := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomComboBox.SetShowItemImage(Value: Boolean);
begin
  if FShowItemImage <> Value then
  begin
    FShowItemImage := Value;
    RePaintControl;
  end;
end;

procedure TscAdvancedCustomComboBox.SetShowItemText(Value: Boolean);
begin
  if FShowItemText <> Value then
  begin
    FShowItemText := Value;
    RePaintControl;
  end;
end;


constructor TscLinkBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FHeader := False;
  FImageIndex := -1;
  FActiveImageIndex := -1;
  FCaption := '';
  FEnabled := True;
  if TscLinkBarItems(Collection).LinkBar.ItemIndex = Self.Index
  then
    Active := True
  else
    Active := False;
  FUseCustomGlowColor := False;
  FCustomGlowColor := clHighLight;
end;

procedure TscLinkBarItem.Assign(Source: TPersistent);
begin
  if Source is TscLinkBarItem then
  begin
    FImageIndex := TscLinkBarItem(Source).ImageIndex;
    FActiveImageIndex := TscLinkBarItem(Source).ActiveImageIndex;
    FCaption := TscLinkBarItem(Source).Caption;
    FEnabled := TscLinkBarItem(Source).Enabled;
    FHeader := TscLinkBarItem(Source).Header;
    FUseCustomGlowColor := TscLinkBarItem(Source).UseCustomGlowColor;
    FCustomGlowColor := TscLinkBarItem(Source).CustomGlowColor;
  end
  else
    inherited Assign(Source);
end;

procedure TscLinkBarItem.SetData(const Value: TCustomData);
begin
  FData := Value;
end;

procedure TscLinkBarItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed(False);
end;

procedure TscLinkBarItem.SetActiveImageIndex(const Value: Integer);
begin
  FActiveImageIndex := Value;
  Changed(False);
end;

procedure TscLinkBarItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TscLinkBarItem.SetHeader(Value: Boolean);
begin
  FHeader := Value;
  Changed(False);
end;

procedure TscLinkBarItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  Changed(False);
end;

constructor TscLinkBarItems.Create;
begin
  inherited Create(TscLinkBarItem);
  LinkBar := AListBox;
end;

function TscLinkBarItems.GetOwner: TPersistent;
begin
  Result := LinkBar;
end;

procedure  TscLinkBarItems.Update(Item: TCollectionItem);
begin
  LinkBar.RepaintControl;
  LinkBar.UpdateScrollInfo;
end;

function TscLinkBarItems.GetItem(Index: Integer):  TscLinkBarItem;
begin
  Result := TscLinkBarItem(inherited GetItem(Index));
end;

procedure TscLinkBarItems.SetItem(Index: Integer; Value:  TscLinkBarItem);
begin
  inherited SetItem(Index, Value);
  LinkBar.RePaintControl;
end;

function TscLinkBarItems.Add: TscLinkBarItem;
begin
  Result := TscLinkBarItem(inherited Add);
  LinkBar.RePaintControl;
end;

function TscLinkBarItems.Insert(Index: Integer): TscLinkBarItem;
begin
  Result := TscLinkBarItem(inherited Insert(Index));
  LinkBar.RePaintControl;
end;

procedure TscLinkBarItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
  LinkBar.RePaintControl;
end;

procedure TscLinkBarItems.Clear;
begin
  inherited Clear;
  LinkBar.FItemIndex := -1;
  LinkBar.RePaintControl;
end;

constructor TscLinkBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(Font);
  FHeaderFont.Style := [fsBold];
  FHeaderFont.OnChange := OnControlChange;
  FHeaderUseStyleColor := True;
  FUseHandPointCursor := True;
  FShowTextUnderLine := True;
  FHoldSelectedItem := False;
  FClicksDisabled := False;
  FSpacing := 5;
  FMouseDown := False;
  FMouseActive := -1;
  FScrollOffset := 0;
  FItems := TscLinkBarItems.Create(Self);
  FImages := nil;
  Width := 150;
  Height := 150;
  FItemHeight := 30;
  FHeaderHeight := 20;
  FMax := 0;
  FRealMax := 0;
  FOldHeight := -1;
  FItemIndex := -1;
  FCustomHeaderImageIndex := -1;
end;

destructor TscLinkBar.Destroy;
begin
  FItems.Free;
  FItems := nil;
  FHeaderFont.Free;
  inherited Destroy;
end;

procedure TscLinkBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FItemHeight := MulDiv(FItemHeight, M, D);
  FSpacing := MulDiv(FSpacing, M, D);
  FHeaderFont.Height := MulDiv(FHeaderFont.Height, M, D);
  FHeaderHeight := MulDiv(FHeaderHeight, M, D);
end;

procedure TscLinkBar.SetCustomHeaderImageIndex(Value: Integer);
begin
  if FCustomHeaderImageIndex <> Value then
  begin
    FCustomHeaderImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscLinkBar.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FVertScrollBar = nil then Exit;
  if TWMMOUSEWHEEL(Message).WheelDelta < 0 then
    FVertScrollBar.Position :=  FVertScrollBar.Position + FVertScrollBar.SmallChange
  else
    FVertScrollBar.Position :=  FVertScrollBar.Position - FVertScrollBar.SmallChange;
end;

procedure TscLinkBar.SetHeaderStyle(Value: TscAdvancedHeaderStyle);
begin
  if FHeaderStyle <> Value then
  begin
    FHeaderStyle := Value;
    RePaintControl;
  end;
end;

procedure TscLinkBar.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  RePaintControl;
end;

function TscLinkBar.CanDrawContent: Boolean;
begin
  Result := Assigned(FItems) and (FItems.Count > 0);
end;

procedure TscLinkBar.DrawContent(ACanvas: TCanvas; ARect: TRect);
var
  I, SaveIndex: Integer;
begin
  CalcItemRects;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,
      FItemsRect.Left, FItemsRect.Top, FItemsRect.Right, FItemsRect.Bottom);
    for I := 0 to FItems.Count - 1 do
      if FItems[I].IsVisible then DrawItem(I, ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscLinkBar.DrawItem(Index: Integer; Cnvs: TCanvas);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);

function IsItemActive: Boolean;
begin
  Result :=  (FItems[Index].Active and FItems[Index].Enabled) or
             (FHoldSelectedItem and (FItemIndex = Index));
end;

var
  R, R1: TRect;
  FC: TColor;
  TX, TY: Integer;
  SaveIndex: Integer;
  FGlowColor: TColor;
  IIndex: Integer;
begin
  if FItems[Index].Header then
  begin
    DrawHeaderItem(Index, Cnvs);
    Exit;
  end;

  if IsCustomStyle and (seFont in StyleElements) then
  begin
    if BackgroundStyle = scbgsColor then
      FC := GetEditTextColor(scsNormal)
    else
      FC := GetCheckBoxTextColor(scsNormal)
  end
  else
    FC := Font.Color;

  if (not FItems[Index].Enabled) or not Self.Enabled then
  begin
    if IsCustomStyle and (seFont in StyleElements) then
    begin
      if BackgroundStyle = scbgsColor then
        FC := GetEditTextColor(scsDisabled)
      else
        FC := GetCheckBoxTextColor(scsDisabled)
    end
    else
      FC := clGrayText;
  end;

  Cnvs.Font := Self.Font;
  if not FSelectionGlow.Enabled and IsItemActive then
  begin
    if (seFont in StyleElements) and IsCustomStyle then
      Cnvs.Font.Color := GetStyleColor(clHighLight)
    else
      Cnvs.Font.Color := clHighLight;
  end
  else
    Cnvs.Font.Color := FC;

  if IsItemActive and ShowTextUnderLine then
    Cnvs.Font.Style := Cnvs.Font.Style + [fsUnderLine];
  Cnvs.Brush.Style := bsClear;

  R := FItems[Index].ItemRect;

 if FItems[Index].UseCustomGlowColor then
   FGlowColor := FItems[Index].CustomGlowColor
 else
   FGlowColor := FSelectionGlow.Color;

 SaveIndex := SaveDC(Cnvs.Handle);
 try
   IntersectClipRect(Cnvs.Handle, R.Left, R.Top, R.Right, R.Bottom);

   InflateRect(R, -2, -2);
   if BidiMode = bdRightToLeft then
     Dec(R.Right, 5)
   else
     Inc(R.Left, 5);
   with FItems[Index] do
   begin
     if IsItemActive and (ActiveImageIndex <> -1) then
       IIndex := ActiveImageIndex
     else
       IIndex := ImageIndex;

     if (FImages <> nil) and (IIndex >= 0) and
        (IIndex < FImages.Count) then
     begin
       if (SelectionGlow.Enabled) and IsItemActive then
         DrawImageAndTextWithGlow2(Cnvs, R, 0, FSpacing, GlyphLayout[IsRightToLeft],
          Caption, IIndex, FImages,
          FItems[Index].Enabled and Self.Enabled, False, clBlack,
          FSelectionGlow.Offset, FGlowColor,
          FSelectionGlow.GlowSize, FSelectionGlow.Intensive, FSelectionGlow.AlphaValue, True,
          False, IsRightToLeft, True, FScaleFactor)
      else
        DrawImageAndText2(Cnvs, R, 0, FSpacing, GlyphLayout[IsRightToLeft],
          Caption, IIndex, FImages,
          FItems[Index].Enabled and Self.Enabled, False, clBlack, False, IsRightToLeft, True, FScaleFactor)
    end
    else
      begin
        if Images <> nil then Inc(R.Left, Images.Width + 5);
        R1 := Rect(0, 0, R.Width, R.Height);
        DrawText(Cnvs.Handle, PChar(Caption), Length(Caption), R1,
          DT_LEFT or DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
        TX := R.Left;
        TY := R.Top + R.Height div 2 - R1.Height div 2;
        if TY < R.Top then TY := R.Top;
        R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
        if (SelectionGlow.Enabled) and IsItemActive then
           DrawTextWithGlow(Cnvs, R, Caption, DT_WORDBREAK or DT_LEFT,
           FSelectionGlow.Offset, FGlowColor, FSelectionGlow.GlowSize,
           FSelectionGlow.Intensive, FSelectionGlow.AlphaValue, IsRightToLeft, True)
        else
          DrawText(Cnvs.Handle, PChar(Caption), Length(Caption), R,
            scDrawTextBidiModeFlags(DT_WORDBREAK or DT_LEFT or DT_NOPREFIX, BidiMode = bdRightToLeft));
      end;
  end;
 finally
   RestoreDC(Cnvs.Handle, SaveIndex);
 end;
end;

procedure TscLinkBar.DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
var
  R, R1: TRect;
  C: TColor;
begin
  R := FItems[Index].ItemRect;
  if HeaderStyle = scahsCustomImage then
  begin
    if (HeaderStyle = scahsCustomImage) and
       FCustomImages.IsIndexAvailable(FCustomHeaderImageIndex)
    then
      FCustomImages.Draw(Cnvs, R, FCustomHeaderImageIndex, FScaleFactor);
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
      Cnvs.Font.Color := GetStyleColor(HeaderFont.Color);
  end
  else
  if HeaderStyle = scahsDefault then
  begin
    DrawHeaderSection(Cnvs, FItems[Index].ItemRect, scsNormal);
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
      Cnvs.Font.Color := GetHeaderTextColor(scsNormal);
  end
  else
  begin
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
    begin
      C := GetCheckBoxTextColor(scsNormal);
      C := MiddleColor(C, GetStyleColor(clBtnFace));
      Cnvs.Font.Color := C;
    end
    else
      C := MiddleColor(clWindowText, clBtnFace);
    Cnvs.Pen.Color := C;
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 1);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 1);
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 2);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 2);
  end;
  Inc(R.Left, 5);
  Dec(R.Right, 5);
  Cnvs.Brush.Style := bsClear;
  if HeaderStyle = scahsDefault then
    DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX, BidiMode = bdRightToLeft))
  else
    begin
      R1 := Rect(0, 0, 0, 0);
      DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R1,
       DT_LEFT or DT_SINGLELINE or DT_CALCRECT or DT_NOPREFIX);
      R.Top := R.Bottom - R1.Height - 6;
      DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX, BidiMode = bdRightToLeft));
    end;
end;

procedure TscLinkBar.SetHoldSelectedItem(Value: Boolean);
begin
  FHoldSelectedItem := Value;
  if not FHoldSelectedItem then FItemIndex := -1;
end;

procedure TscLinkBar.SetSpacing;
begin
  if FSpacing <> Value
  then
    begin
      FSpacing := Value;
      RePaintControl;
    end;
end;

function TscLinkBar.CalcHeight;
begin
  if AItemCount > FItems.Count then AItemCount := FItems.Count;
  Result := AItemCount * ItemHeight;
  Result := Result + Height - GetContentRect.Height;
end;

procedure TscLinkBar.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value
  then
    begin
      FItemHeight := Value;
      RePaintControl;
      UpdateScrollInfo;
    end;
end;

procedure TscLinkBar.SetHeaderHeight(Value: Integer);
begin
  if FHeaderHeight <> Value
  then
    begin
      FHeaderHeight := Value;
      RePaintControl;
      UpdateScrollInfo;
    end;
end;

procedure TscLinkBar.SetItems(Value: TscLinkBarItems);
begin
  FItems.Assign(Value);
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscLinkBar.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    RePaintControl;
  end;
end;

procedure TscLinkBar.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
   FImages := nil;
end;

procedure TscLinkBar.CalcItemRects;
var
  I: Integer;
  X, Y, W, H: Integer;
begin
  FRealMax := 0;
  FItemsRect := GetContentRect;
  Y := FItemsRect.Top;
  W := FItemsRect.Width - 2;
  for I := 0 to FItems.Count - 1 do
    with TscLinkBarItem(FItems[I]) do
    begin
      if not Header then H := ItemHeight else H := HeaderHeight;
      if FSelectionGlow.Enabled and not Header then X := FItemsRect.Left + FSelectionGlow.GlowSize - 5
      else X := FItemsRect.Left;
      if X < FItemsRect.Left then X := FItemsRect.Left;
      ItemRect := Rect(X, Y, X + W, Y + H);
      OffsetRect(ItemRect, 0, - FScrollOffset);
      IsVisible := RectToRect(ItemRect, FItemsRect);
      if not IsVisible and (ItemRect.Top <= FItemsRect.Top) and
        (ItemRect.Bottom >= FItemsRect.Bottom)
      then
        IsVisible := True;
      if IsVisible then FRealMax := ItemRect.Bottom;
      Y := Y + H;
    end;
  FMax := Y;
end;

procedure TscLinkBar.Scroll(AScrollOffset: Integer);
begin
  FScrollOffset := AScrollOffset;
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscLinkBar.GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
begin
  CalcItemRects;
  AMin := 0;
  AMax := FMax - FItemsRect.Top - 1;
  APage := FItemsRect.Height;
  if AMax <= APage
  then
    begin
      APage := 0;
      AMax := 0;
    end;
  APosition := FScrollOffset;
end;

procedure TscLinkBar.CMMouseLeave;
begin
  inherited;
  SetItemActive(-1);
  FMouseActive := -1;
  if FUseHandPointCursor then
    Cursor := crDefault;
end;

procedure TscLinkBar.WMSize(var Msg: TWMSize);
begin
  inherited;
  if FOldHeight <> Height
  then
    begin
      CalcItemRects;
      if (FRealMax <= FItemsRect.Bottom) and (FScrollOffset > 0)
      then
        begin
          FScrollOffset := FScrollOffset - (FItemsRect.Bottom - FRealMax);
          if FScrollOffset < 0 then FScrollOffset := 0;
          CalcItemRects;
          RePaintControl;
        end;
    end;
  UpdateScrollInfo;
  FOldHeight := Height;
end;

procedure TscLinkBar.ScrollToItem(Index: Integer);
var
  R, R1: TRect;
begin
  CalcItemRects;
  R1 := FItems[Index].ItemRect;
  R := R1;
  OffsetRect(R, 0, FScrollOffset);
  if (R1.Top <= FItemsRect.Top)
  then
    begin
      if (Index = 1) and FItems[Index - 1].Header
      then
        FScrollOffset := 0
      else
        FScrollOffset := R.Top - FItemsRect.Top;
      CalcItemRects;
      RePaintControl;
    end
  else
  if R1.Bottom >= FItemsRect.Bottom
  then
    begin
      FScrollOffset := R.Top;
      FScrollOffset := FScrollOffset - FItemsRect.Height + R.Height -
        Height + FItemsRect.Bottom;
      CalcItemRects;
      RePaintControl;
    end;
  UpdateScrollInfo;
end;

procedure TscLinkBar.UpdateScrollInfo;
var
  SMin, SMax, SPage, SPos: Integer;
begin
  if not HandleAllocated then Exit;

  GetScrollInfo(SMin, SMax, SPage, SPos);
  if SMax <> 0
  then
    begin
      ShowVertScrollBar;
      UpdateVertScrollBar(SMin, SMax, SPos, SPage);
      if FVertScrollBar <> nil then
      begin
        FVertScrollBar.LargeChange := SPage;
        FVertScrollBar.SmallChange := FItemHeight;
      end;
    end
  else
  if (SMax = 0) and (FVertScrollBar <> nil)
  then
    begin
      HideVertScrollBar;
      RePaintControl;
    end;
end;

procedure TscLinkBar.OnVertScrollBarChange(Sender: TObject);
begin
  inherited;
  if (FVertScrollBar <> nil) and FVertScrollBar.HandleAllocated then
  begin
   if FVertScrollBar.Position <= FVertScrollBar.Max - FVertScrollBar.PageSize + 1  then
     Scroll(FVertScrollBar.Position)
   else
     Scroll(FVertScrollBar.Max - FVertScrollBar.PageSize + 1);
  end;
end;

procedure TscLinkBar.SetItemIndex(Value: Integer);
var
  I: Integer;
  IsFind: Boolean;
  FOldItemIndex: Integer;
begin
  if (csDesigning in ComponentState) and not FHoldSelectedItem
  then
    begin
      FItemIndex := -1;
      Exit;
    end;

  FOldItemIndex := FItemIndex;

  FItemIndex := Value;

  if FItemIndex < 0
  then
    begin
      if (FOldItemIndex >= 0) and (FOldItemIndex < FItems.Count) then
        FItems[FOldItemIndex].Active := False;
      FItemIndex := -1;
      RePaintControl;
      Exit;
    end;

    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) and not
       (csDesigning in ComponentState)
    then
     begin
      IsFind := False;
      for I := 0 to FItems.Count - 1 do
        with FItems[I] do
        begin
          if I = FItemIndex
          then
            begin
              Active := True;
              IsFind := True;
            end
          else
             Active := False;
        end;
      RePaintControl;
      ScrollToItem(FItemIndex);
      if IsFind then
      begin
        if Assigned(FItems[FItemIndex].OnClick) then
          FItems[FItemIndex].OnClick(Self);
        if Assigned(FOnItemClick) then
          FOnItemClick(Self);
      end;
    end;
end;

procedure TscLinkBar.Loaded;
begin
  inherited;
end;

function TscLinkBar.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if PtInRect(FItems[I].ItemRect, Point(X, Y)) and (FItems[I].Enabled)
    then
      begin
        Result := I;
        Break;
      end;
end;

procedure TscLinkBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft)
  then
    begin
      SetItemActive(I);
      if FHoldSelectedItem
      then
        begin
          ItemIndex := I;
        end;
      FMouseDown := True;
      FMouseActive := I;
    end;
end;

procedure TscLinkBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  FMouseDown := False;
  I := ItemAtPos(X, Y);
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft) and
     not FHoldSelectedItem
  then
    ItemIndex := I;
end;

procedure TscLinkBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  if (I <> -1)and FItems[I].Header then I := -1;
  if (I <> -1) and (I <> FMouseActive)
  then
    begin
      SetItemActive(I);
      FMouseActive := I;
      if FUseHandPointCursor then
        Cursor := crHandPoint;
    end
  else
  if (I = -1) and (I <> FMouseActive)
  then
    begin
      SetItemActive(-1);
      if FUseHandPointCursor then
        Cursor := crDefault;
      FMouseActive := -1;
    end;
end;

procedure TscLinkBar.SetItemActive(Value: Integer);
var
  I: Integer;
begin
  if not FHoldSelectedItem then FItemIndex := Value;

  if not FHoldSelectedItem and (FItemIndex = -1)
  then
    begin
      for I := 0 to FItems.Count - 1 do FItems[I].Active := False;
      RePaintControl;
      Exit;
    end;

  for I := 0 to FItems.Count - 1 do
  with FItems[I] do
   if I = Value then Active := True else Active := False;
  RePaintControl;
end;

procedure TscLinkBar.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
end;

constructor TscGridViewItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FShowEllipses := False;
  FHeader := False;
  FImageIndex := -1;
  FCaption := '';
  FEnabled := True;
  if TscGridViewItems(Collection).GridView.ItemIndex = Self.Index
  then
    Active := True
  else
    Active := False;
end;

procedure TscGridViewItem.Assign(Source: TPersistent);
begin
  if Source is TscGridViewItem then
  begin
    FImageIndex := TscGridViewItem(Source).ImageIndex;
    FCaption := TscGridViewItem(Source).Caption;
    FEnabled := TscGridViewItem(Source).Enabled;
    FHeader := TscGridViewItem(Source).Header;
  end
  else
    inherited Assign(Source);
end;

procedure TscGridViewItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed(False);
end;

procedure TscGridViewItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TscGridViewItem.SetHeader(Value: Boolean);
begin
  FHeader := Value;
  Changed(False);
end;

procedure TscGridViewItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  Changed(False);
end;

procedure TscGridViewItem.SetData(const Value: TCustomData);
begin
  FData := Value;
end;

constructor TscGridViewItems.Create;
begin
  inherited Create(TscGridViewItem);
  GridView := AGridView;
end;

function TscGridViewItems.GetOwner: TPersistent;
begin
  Result := GridView;
end;

procedure  TscGridViewItems.Update(Item: TCollectionItem);
begin
  GridView.Repaint;
  GridView.UpdateScrollInfo;
end;

function TscGridViewItems.GetItem(Index: Integer):  TscGridViewItem;
begin
  Result := TscGridViewItem(inherited GetItem(Index));
end;

procedure TscGridViewItems.SetItem(Index: Integer; Value:  TscGridViewItem);
begin
  inherited SetItem(Index, Value);
  GridView.RePaintControl;
end;

function TscGridViewItems.Add: TscGridViewItem;
begin
  Result := TscGridViewItem(inherited Add);
  GridView.RePaintControl;
end;

function TscGridViewItems.Insert(Index: Integer): TscGridViewItem;
begin
  Result := TscGridViewItem(inherited Insert(Index));
  GridView.RePaintControl;
end;

procedure TscGridViewItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
  GridView.RePaintControl;
end;

procedure TscGridViewItems.Clear;
begin
  inherited Clear;
  GridView.FItemIndex := -1;
  GridView.RePaintControl;
end;

constructor TscGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoComplete := True;
  FSearchString := '';
  FPopupMode := False;
  FMouseMoveChangeIndex := False;
  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(Font);
  FHeaderFont.Style := [fsBold];
  FHeaderFont.OnChange := OnControlChange;
  FHeaderUseStyleColor := True;
  TabStop := True;
  RowCount := 1;
  ColCount := 1;
  FInUpdateItems := False;
  FClicksDisabled := False;
  FMouseDown := False;
  FMouseActive := -1;
  FScrollOffset := 0;
  FItems := TscGridViewItems.Create(Self);
  FItemMargin := -1;
  FItemSpacing := 1;
  FItemLayout := blGlyphTop;
  FImages := nil;
  Width := 150;
  Height := 150;
  FItemHeight := 30;
  FItemWidth := 30;
  FHeaderHeight := 20;
  FMax := 0;
  FRealMax := 0;
  FOldHeight := -1;
  FItemIndex := -1;
  FCustomHeaderImageIndex := -1;
  FDisabledFontColor := clGray;
end;

destructor TscGridView.Destroy;
begin
  FHeaderFont.Free;
  FItems.Free;
  FItems := nil;
  inherited Destroy;
end;

procedure TscGridView.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FItemWidth := MulDiv(FItemWidth, M, D);
  FItemHeight := MulDiv(FItemHeight, M, D);
  FItemSpacing := MulDiv(FItemSpacing, M, D);
  FHeaderFont.Height := MulDiv(FHeaderFont.Height, M, D);
  FHeaderHeight := MulDiv(FHeaderHeight, M, D);
end;

function TscGridView.NextIndex(const S: string): Integer;
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

function TscGridView.IndexOf(const S: string; AStartOff: Boolean = False): Integer;
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

procedure TscGridView.SetCustomHeaderImageIndex(Value: Integer);
begin
  if FCustomHeaderImageIndex <> Value then
  begin
    FCustomHeaderImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGridView.SetHeaderStyle(Value: TscAdvancedHeaderStyle);
begin
  if FHeaderStyle <> Value then
  begin
    FHeaderStyle := Value;
    RePaintControl;
  end;
end;

procedure TscGridView.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  RePaintControl;
end;

procedure TscGridView.SetItemLayout(Value: TButtonLayout);
begin
  FItemLayout := Value;
  RePaintControl;
end;

procedure TscGridView.SetItemMargin(Value: Integer);
begin
  FItemMargin := Value;
  RePaintControl;
end;

procedure TscGridView.SetItemSpacing(Value: Integer);
begin
  FItemSpacing := Value;
  RePaintControl;
end;

procedure TscGridView.BeginUpdateItems;
begin
  FInUpdateItems := True;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TscGridView.EndUpdateItems;
begin
  FInUpdateItems := False;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
  RePaintControl;
  UpdateScrollInfo;
end;

function TscGridView.CalcHeight;
begin
  if AItemCount > FItems.Count then AItemCount := FItems.Count;
  Result := AItemCount * ItemHeight;
  Result := Result + Height - GetContentRect.Height;
end;

procedure TscGridView.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value
  then
    begin
      FItemHeight := Value;
      RePaintControl;
      UpdateScrollInfo;
    end;
end;

procedure TscGridView.SetItemWidth(Value: Integer);
begin
  if FItemWidth <> Value
  then
    begin
      FItemWidth := Value;
      RePaintControl;
      UpdateScrollInfo;
    end;
end;

procedure TscGridView.SetHeaderHeight(Value: Integer);
begin
  if FHeaderHeight <> Value
  then
    begin
      FHeaderHeight := Value;
      RePaintControl;
      UpdateScrollInfo;
    end;
end;

procedure TscGridView.SetItems(Value: TscGridViewItems);
begin
  FItems.Assign(Value);
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscGridView.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    RePaintControl;
  end;
end;

procedure TscGridView.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
   FImages := nil;
end;

procedure TscGridView.CalcItemRects;
var
  I, Col: Integer;
  X, Y: Integer;
begin
  FItemsRect := GetContentRect;
  RowCount := FItemsRect.Height div ItemHeight;
  ColCount := FItemsRect.Width div ItemWidth;
  if RowCount = 0 then RowCount := 1;
  if ColCount = 0 then ColCount := 1;
  FRealMax := 0;
  Y := FItemsRect.Top;
  X := FItemsRect.Left;
  Col := 0;
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Header
    then
      begin
        X := FItemsRect.Left;
        if i <> 0 then Y := Y + ItemHeight;
        Items[i].ItemRect := Rect(X, Y, X + FItemsRect.Width, Y + HeaderHeight);
        Y := Y + HeaderHeight;
        Col := 0;
      end
    else
    if Col = ColCount
    then
      begin
        X := FItemsRect.Left;
        Y := Y + ItemHeight;
        Col := 0
      end;
    if not Items[i].Header
    then
      Items[i].ItemRect := Rect(X, Y, X + ItemWidth, Y + ItemHeight);
    OffsetRect(Items[i].ItemRect, 0, - FScrollOffset);
    Items[i].IsVisible := RectToRect(Items[i].ItemRect, FItemsRect);
    if not Items[i].IsVisible and (Items[i].ItemRect.Top <= FItemsRect.Top) and
       (Items[i].ItemRect.Bottom >= FItemsRect.Bottom)
    then
      Items[i].IsVisible := True;
    if Items[i].IsVisible then FRealMax := Items[i].ItemRect.Bottom;

    if not Items[i].Header
    then
      begin
        X := X + ItemWidth;
        Inc(Col);
      end;
  end;
  FMax := Y + ItemHeight;
end;


procedure TscGridView.Scroll(AScrollOffset: Integer);
begin
  FScrollOffset := AScrollOffset;
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscGridView.GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
begin
  CalcItemRects;
  AMin := 0;
  AMax := FMax - FItemsRect.Top - 1;
  APage := FItemsRect.Height;
  if AMax <= APage
  then
    begin
      APage := 0;
      AMax := 0;
    end;
  APosition := FScrollOffset;
end;

procedure TscGridView.EnableScrollTimer(Value: Integer);
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

procedure TscGridView.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscGridView.WMTimer;
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

procedure TscGridView.WMSize(var Msg: TWMSize);
begin
  inherited;
  if FOldHeight <> Height
  then
    begin
      CalcItemRects;
      if (FRealMax <= FItemsRect.Bottom) and (FScrollOffset > 0)
      then
        begin
          FScrollOffset := FScrollOffset - (FItemsRect.Bottom - FRealMax);
          if FScrollOffset < 0 then FScrollOffset := 0;
          CalcItemRects;
          RePaintControl;
        end;
    end;
  UpdateScrollInfo;
  FOldHeight := Height;
end;

procedure TscGridView.ScrollToItem(Index: Integer);
var
  R, R1: TRect;
begin
  CalcItemRects;
  R1 := FItems[Index].ItemRect;
  R := R1;
  OffsetRect(R, 0, FScrollOffset);
  if (R1.Top <= FItemsRect.Top)
  then
    begin
      if (Index = 1) and FItems[Index - 1].Header
      then
        FScrollOffset := 0
      else
        FScrollOffset := R.Top - FItemsRect.Top;
      CalcItemRects;
      RePaintControl;
    end
  else
  if (R1.Bottom >= FItemsRect.Bottom)
  then
    begin
      FScrollOffset := R.Top;
      FScrollOffset := FScrollOffset - FItemsRect.Height + R.Height -
        Height + FItemsRect.Bottom;
      CalcItemRects;
      RePaintControl;
    end;
  UpdateScrollInfo;
end;

procedure TscGridView.UpdateScrollInfo;
var
  SMin, SMax, SPage, SPos: Integer;
begin
  if not HandleAllocated then Exit;
  if FInUpdateItems then Exit;
  GetScrollInfo(SMin, SMax, SPage, SPos);
  if SMax <> 0
  then
    begin
      if FVertScrollBar = nil then ShowVertScrollBar;
      Self.UpdateVertScrollBar(SMin, SMax, SPos, SPage);
      if FVertScrollBar <> nil then
      begin
        FVertScrollBar.LargeChange := SPage;
        FVertScrollBar.SmallChange := FItemHeight;
      end;
    end
  else
  if (SMax = 0) and (FVertScrollBar <> nil)
  then
  begin
    HideVertScrollBar;
    RePaintControl;
  end;
end;

procedure TscGridView.OnVertScrollBarChange(Sender: TObject);
begin
  inherited;
   if (FVertScrollBar <> nil) and FVertScrollBar.HandleAllocated then
  begin
   if FVertScrollBar.Position <= FVertScrollBar.Max - FVertScrollBar.PageSize + 1  then
     Scroll(FVertScrollBar.Position)
   else
     Scroll(FVertScrollBar.Max - FVertScrollBar.PageSize + 1);
  end;
end;

procedure TscGridView.InitItemIndex(Value: Integer);
var
  I: Integer;
begin
  if Value < 0
  then
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
          if I = FItemIndex
          then
            Active := True
          else
            Active := False;
        end;
      ScrollToItem(FItemIndex);
    end;
end;

procedure TscGridView.SetItemIndex(Value: Integer);
var
  I: Integer;
  IsFind: Boolean;
begin
  if Value < 0
  then
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
          if I = FItemIndex
          then
            begin
              Active := True;
              IsFind := True;
            end
          else
             Active := False;
        end;
      RePaintControl;
      ScrollToItem(FItemIndex);
      if IsFind and not (csDesigning in ComponentState)
         and not (csLoading in ComponentState) and not FMouseDown
      then
      begin
        if Assigned(FItems[FItemIndex].OnClick) then
          FItems[FItemIndex].OnClick(Self);
        if Assigned(FOnItemClick) then
          FOnItemClick(Self);
      end;
    end;
end;

procedure TscGridView.Loaded;
begin
  inherited;
end;

function TscGridView.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if PtInRect(FItems[I].ItemRect, Point(X, Y)) and (FItems[I].Enabled)
    then
      begin
        Result := I;
        Break;
      end;
end;


procedure TscGridView.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft)
  then
    begin
      SetItemActive(I);
      FMouseDown := True;
      FMouseActive := I;
    end;
end;

procedure TscGridView.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
  CanClick: Boolean;
begin
  inherited;
  CanClick := FMouseDown;
  FMouseDown := False;
  StopScrollTimer;

  if CanClick then
  begin
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) and
       Assigned(FItems[FItemIndex].OnClick) then
      FItems[FItemIndex].OnClick(Self);

    if Assigned(FOnItemClick) then
      FOnItemClick(Self);
  end;

  I := ItemAtPos(X, Y);
  if (I <> -1) and not (FItems[I].Header) and (Button = mbLeft) then
    ItemIndex := I;
end;

procedure TscGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
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
    if (I <> -1) and not (FItems[I].Header) and (FMouseDown or FMouseMoveChangeIndex)
       and (I <> FMouseActive) then
    begin
      SetItemActive(I);
      FMouseActive := I;
    end;
    if (FBalloonHint <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      if FItems[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        ShowBalloonHint(FItems[I].Caption, FItems[I].ItemRect);
      end
      else
      begin
        FHintItemIndex := -1;
        if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      end;
    end;
    if (FHintComponent <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if FItems[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        FHintComponent.ActivateHint(FItems[I].Caption);
      end
      else
      begin
        FHintItemIndex := -1;
        FHintComponent.HideHint;
      end;
    end;
    if (I = -1) and (FBalloonHint <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
    end;
    if (I = -1) and (FHintComponent <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      FHintComponent.HideHint;
    end;
  end
  else
  if FMouseDown then
  begin
    if Y < R.Top then
      EnableScrollTimer(1)
    else
      EnableScrollTimer(2);
  end;
end;

procedure TscGridView.SetItemActive(Value: Integer);
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

procedure TscGridView.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  if (FBalloonHint <> nil) and FBalloonHint.ShowingHint
  then
    FBalloonHint.HideHint;
  if FHintComponent <> nil
  then
    FHintComponent.HideHint;
end;

procedure TscGridView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHintItemIndex <> -1 then
  begin
    FHintItemIndex := -1;
    if (FBalloonHint <> nil) and (FBalloonHint.ShowingHint) then
      FBalloonHint.HideHint;
    if FHintComponent <> nil
    then
      FHintComponent.HideHint;
  end;
end;

procedure TscGridView.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FVertScrollBar = nil then Exit;
  if TWMMOUSEWHEEL(Message).WheelDelta < 0
  then
    FVertScrollBar.Position :=  FVertScrollBar.Position + FVertScrollBar.SmallChange
  else
    FVertScrollBar.Position :=  FVertScrollBar.Position - FVertScrollBar.SmallChange;
end;

procedure TscGridView.WMSETFOCUS(var Message: TWMSETFOCUS);
begin
  inherited;
  FUpdateParentBuffer := True;
  RePaint;
end;

procedure TscGridView.WMKILLFOCUS(var Message: TWMKILLFOCUS);
begin
  inherited;
  RePaint;
end;

procedure TscGridView.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and not Focused  and not FPopupMode then
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

procedure TscGridView.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscGridView.FindUp;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Exit;
  Start := FItemIndex - ColCount;
  if Start < 0 then Start := 0;
  for I := FItemIndex downto Start do
    if FItems[I].Header
    then
      begin
        Start := I;
        Break;
      end;
  for I := Start downto 0 do
  begin
    if (not FItems[I].Header) and FItems[I].Enabled and (ItemIndex <> I)
    then
      begin
        ItemIndex := I;
        Exit;
      end;
  end;
end;

procedure TscGridView.FindDown;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Start := 0 else Start := FItemIndex + ColCount;
  if Start > FItems.Count - 1 then Start := FItems.Count - 1;
  if FItemIndex <> -1 then
    for I := FItemIndex to Start do
      if FItems[I].Header
      then
        begin
          Start := I;
          Break;
        end;
  for I := Start to FItems.Count - 1 do
  begin
    if (not FItems[I].Header) and FItems[I].Enabled and (ItemIndex <> I)
    then
      begin
        ItemIndex := I;
        Exit;
      end;
  end;
end;

procedure TscGridView.FindLeft;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Exit;
  Start := FItemIndex - 1;
  if Start < 0 then Exit;
  for I := Start downto 0 do
  begin
    if (not FItems[I].Header) and FItems[I].Enabled
    then
      begin
        ItemIndex := I;
        Exit;
      end;
  end;
end;

procedure TscGridView.FindRight;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Start := 0 else Start := FItemIndex + 1;
  if Start > FItems.Count - 1 then Exit;
  for I := Start to FItems.Count - 1 do
  begin
    if (not FItems[I].Header) and FItems[I].Enabled
    then
      begin
        ItemIndex := I;
        Exit;
      end;
  end;
end;

procedure TscGridView.FindPageUp;
var
  I, J, Start: Integer;
  PageCount: Integer;
  FindHeader: Boolean;
begin
  if ItemIndex <= -1 then Exit;
  Start := FItemIndex - 1;
  if Start < 0 then Exit;
  PageCount := (FItemsRect.Height div FItemHeight) * ColCount;
  if PageCount = 0 then PageCount := 1;
  PageCount := Start - PageCount;
  if PageCount < 0 then PageCount := 0;
  FindHeader := False;
  J := -1;
  for I := Start downto PageCount do
  begin
    if not FItems[I].Header and FindHeader and FItems[I].Enabled
    then
      begin
        ItemIndex := I;
        Exit;
      end
    else
    if FItems[I].Header
    then
      begin
        FindHeader := True;
        Continue;
      end
    else
    if not FItems[I].Header and FItems[I].Enabled
    then
      begin
        J := I;
      end;
  end;
  if J <> -1 then ItemIndex := J;
end;


procedure TscGridView.FindPageDown;
var
  I, J, Start: Integer;
  PageCount: Integer;
  FindHeader: Boolean;
begin
  if ItemIndex <= -1 then Start := 0 else Start := FItemIndex + 1;
  if Start > FItems.Count - 1 then Exit;
  PageCount := (FItemsRect.Height div FItemHeight) * ColCount;
  if PageCount = 0 then PageCount := 1;
  PageCount := Start + PageCount;
  if PageCount > FItems.Count - 1 then PageCount := FItems.Count - 1;
  FindHeader := False;
  J := -1;
  for I := Start to PageCount do
  begin
    if not FItems[I].Header and FindHeader  and FItems[I].Enabled
    then
      begin
        ItemIndex := I;
        Exit;
      end
    else
    if FItems[I].Header
    then
      begin
        FindHeader := True;
        Continue;
      end
    else
    if not FItems[I].Header  and FItems[I].Enabled
    then
      begin
        J := I;
      end;
  end;
  if J <> -1 then ItemIndex := J;
end;

procedure TscGridView.KeyPress(var Key: Char);
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
    I := IndexOf(FSearchString, True);
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

procedure TscGridView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;


procedure TscGridView.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited KeyDown(Key, Shift);
 case Key of
   VK_NEXT:  FindPageDown;
   VK_PRIOR: FindPageUp;
   VK_LEFT: FindLeft;
   VK_UP: FindUp;
   VK_DOWN: FindDown;
   VK_RIGHT: FindRight;
 end;
end;

function TscGridView.CanDrawContent: Boolean;
begin
  Result := Assigned(FItems) and (FItems.Count > 0);
end;

procedure TscGridView.DrawContent(ACanvas: TCanvas; ARect: TRect);
var
  I, SaveIndex: Integer;
begin
  CalcItemRects;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,
      FItemsRect.Left, FItemsRect.Top, FItemsRect.Right, FItemsRect.Bottom);
    for I := 0 to FItems.Count - 1 do
      if FItems[I].IsVisible then DrawItem(I, ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscGridView.DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
var
  R, R1: TRect;
  C: TColor;
begin
  R := FItems[Index].ItemRect;
  if HeaderStyle = scahsCustomImage then
  begin
   if (HeaderStyle = scahsCustomImage) and
      FCustomImages.IsIndexAvailable(FCustomHeaderImageIndex)
    then
      FCustomImages.Draw(Cnvs, R, FCustomHeaderImageIndex, FScaleFactor);
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
      Cnvs.Font.Color := GetStyleColor(HeaderFont.Color);
  end
  else
  if HeaderStyle = scahsDefault then
  begin
    DrawHeaderSection(Cnvs, FItems[Index].ItemRect, scsNormal);
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
      Cnvs.Font.Color := GetHeaderTextColor(scsNormal);
  end
  else
  begin
    Cnvs.Font := HeaderFont;
    if IsCustomStyle and FHeaderUseStyleColor then
    begin
      C := GetCheckBoxTextColor(scsNormal);
      C := MiddleColor(C, GetStyleColor(clBtnFace));
      Cnvs.Font.Color := C;
    end
    else
      C := MiddleColor(clWindowText, clBtnFace);
    Cnvs.Pen.Color := C;
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 1);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 1);
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 2);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 2);
  end;
  Inc(R.Left, 5);
  Dec(R.Right, 5);
  Cnvs.Brush.Style := bsClear;
  if HeaderStyle = scahsDefault then
    DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft))
  else
    begin
      R1 := Rect(0, 0, 0, 0);
      DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R1,
       DT_LEFT or DT_SINGLELINE or DT_CALCRECT);
      R.Top := R.Bottom - R1.Height - 6;
      DrawText(Cnvs.Handle, PChar(FItems[Index].Caption), Length(FItems[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft));
    end;
end;

procedure TscGridView.DrawItem(Index: Integer; Cnvs: TCanvas);
var
  R: TRect;
  FC: TColor;
  SaveIndex: Integer;
  FGlowAlpha: Byte;
  FMargin: Integer;
begin
  if FItems[Index].Header then
  begin
    DrawHeaderItem(Index, Cnvs);
    Exit;
  end;

  if (FItems[Index].Active and FItems[Index].Enabled) and (SelectionStyle <> scastGlow) then
  begin
    if SelectionStyle = scastColor
    then
    begin
      if FSelectionColor <> clNone then
        Cnvs.Brush.Color := FSelectionColor
      else
        Cnvs.Brush.Color := GetStyleColor(clHighLight);
      if not Focused and not FShowFocusRect then
        FillRectWithAlpha(Cnvs, FItems[Index].ItemRect, 200)
      else
        Cnvs.FillRect(FItems[Index].ItemRect);
      if FSelectionColor <> clNone then
        FC := FSelectionTextColor
      else
      if IsCustomStyle then
        FC := GetSelectionTextColor
      else
        FC := clHighLightText;
    end
    else
    begin
      R := FItems[Index].ItemRect;
      if ((SelectionStyle = scastCustomImage) or (SelectionStyle = scastCustomImageWithGlow)) and
         (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomSelectionImageIndex) and
          FCustomImages.IsIndexAvailable(FCustomFocusedSelectionImageIndex)
      then
      begin
        if Self.Focused then
          FCustomImages.Draw(Cnvs, R, FCustomFocusedSelectionImageIndex, FScaleFactor)
        else
          FCustomImages.Draw(Cnvs, R, FCustomSelectionImageIndex, FScaleFactor);
        FC := GetStyleColor(FSelectionTextColor);
      end
      else
      begin
        DrawSelection(Cnvs, R, Self.Focused, FShowFocusRect);
        FC := GetSelectionTextColor;
      end;
    end
  end
  else
  begin
    if IsCustomStyle and (seFont in StyleElements) then
    begin
      if BackgroundStyle = scbgsColor then
        FC := GetEditTextColor(scsNormal)
      else
        FC := GetCheckBoxTextColor(scsNormal)
    end
    else
      FC := Font.Color;
    if (not FItems[Index].Enabled) or not Self.Enabled then
    begin
      if IsCustomStyle and (seFont in StyleElements) then
      begin
        if BackgroundStyle = scbgsColor then
          FC := GetEditTextColor(scsDisabled)
        else
          FC := GetCheckBoxTextColor(scsDisabled)
      end
      else
        FC := clGrayText;
    end;
  end;

  Cnvs.Font := Self.Font;
  Cnvs.Font.Color := FC;
  Cnvs.Brush.Style := bsClear;

  R := FItems[Index].ItemRect;

  SaveIndex := SaveDC(Cnvs.Handle);
  try
  IntersectClipRect(Cnvs.Handle, R.Left, R.Top, R.Right, R.Bottom);

  FMargin := FItemMargin;
  if FSelectionStyle = scastGlow then
  begin
    InflateRect(R, -FSelectionGlow.GlowSize, -FSelectionGlow.GlowSize);
    if FMargin > 0 then
    begin
      FMargin := FMargin - FSelectionGlow.GlowSize + 2;
      if FMargin < 0 then FMargin := 0;
    end;
  end
  else
    InflateRect(R, -2, -2);
  with FItems[Index] do
  begin
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Cnvs, Index, FItems[Index].ItemRect)
    else
      begin
        FGlowAlpha := FSelectionGlow.AlphaValue;
        if not FShowFocusRect and not Self.Focused then
        begin
          if FGlowAlpha > 60 then
           FGlowAlpha := FGlowAlpha - 60;
        end;
        if ((SelectionStyle = scastGlow) or (SelectionStyle = scastCustomImageWithGlow)) and
           (FItems[Index].Active and FItems[Index].Enabled) then
         FItems[Index].FShowEllipses := DrawImageAndTextEllipsesWithGlow(Cnvs, R,
            FMargin, FItemSpacing,
            FItemLayout,
            Caption, FImageIndex, FImages,
            FItems[Index].Enabled and Self.Enabled, IsRightToLeft,
            (FItemLayout = blGlyphLeft) or (FItemLayout = blGlyphRight),
            FSelectionGlow.Offset, FSelectionGlow.Color,FSelectionGlow.GlowSize,
            FSelectionGlow.Intensive, FGlowAlpha, True, True, FScaleFactor)
         else
          FItems[Index].FShowEllipses := DrawImageAndTextEllipses(Cnvs, R,
            FMargin, FItemSpacing, FItemLayout,
            Caption, FImageIndex, FImages,
            FItems[Index].Enabled and Self.Enabled, IsRightToLeft, True, FScaleFactor);
      end;
   end;

  if (Self.ItemIndex = -1) and Focused and (Index = 0) and not FItems[Index].Header then
  begin
    R := FItems[Index].ItemRect;
    if FShowFocusRect then scDrawFocusRect(Cnvs, R);
  end
  else
  if (Self.ItemIndex = -1) and Focused and (Index = 1) and not FItems[Index].Header then
  begin
    R := FItems[Index].ItemRect;
    if FShowFocusRect then scDrawFocusRect(Cnvs, R, FScaleFactor);
  end
  else
  if FItems[Index].Active and Focused then
  begin
    R := FItems[Index].ItemRect;
    if FShowFocusRect then
    begin
      if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
        and (FSelectionStyle = scastStyled) then
        InflateRect(R, -1, -1);
      scDrawFocusRect(Cnvs, R, FScaleFactor);
    end;
  end;

  finally
    RestoreDC(Cnvs.Handle, SaveIndex);
  end;
end;

constructor TscHorzItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FShowEllipses := False;
  FImageIndex := -1;
  FCaption := '';
  FEnabled := True;
  if TscHorzItems(Collection).HorzListBox.ItemIndex = Self.Index
  then
    Active := True
  else
    Active := False;
end;

procedure TscHorzItem.Assign(Source: TPersistent);
begin
  if Source is TscHorzItem then
  begin
    FImageIndex := TscHorzItem(Source).ImageIndex;
    FCaption := TscHorzItem(Source).Caption;
    FEnabled := TscHorzItem(Source).Enabled;
  end
  else
    inherited Assign(Source);
end;

procedure TscHorzItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed(False);
end;

procedure TscHorzItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TscHorzItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  Changed(False);
end;

procedure TscHorzItem.SetData(const Value: TCustomData);
begin
  FData := Value;
end;

constructor TscHorzItems.Create;
begin
  inherited Create(TscHorzItem);
  HorzListBox := AListBox;
end;

function TscHorzItems.GetOwner: TPersistent;
begin
  Result := HorzListBox;
end;

procedure  TscHorzItems.Update(Item: TCollectionItem);
begin
  HorzListBox.Repaint;
  HorzListBox.UpdateScrollInfo;
end;

function TscHorzItems.GetItem(Index: Integer):  TscHorzItem;
begin
  Result := TscHorzItem(inherited GetItem(Index));
end;

procedure TscHorzItems.SetItem(Index: Integer; Value:  TscHorzItem);
begin
  inherited SetItem(Index, Value);
  HorzListBox.RePaintControl;
end;

function TscHorzItems.Add: TscHorzItem;
begin
  Result := TscHorzItem(inherited Add);
  HorzListBox.RePaintControl;
end;

function TscHorzItems.Insert(Index: Integer): TscHorzItem;
begin
  Result := TscHorzItem(inherited Insert(Index));
  HorzListBox.RePaintControl;
end;

procedure TscHorzItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
  HorzListBox.RePaintControl;
end;

procedure TscHorzItems.Clear;
begin
  inherited Clear;
  HorzListBox.FItemIndex := -1;
  HorzListBox.RePaintControl;
end;

constructor TscHorzListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoComplete := True;
  FSearchString := '';
  FSearchTimerEnabled := False;
  FTimerMode := 0;
  TabStop := True;
  FInUpdateItems := False;
  FShowGlow := False;
  FItemMargin := -1;
  FItemSpacing := 1;
  FItemLayout := blGlyphTop;
  FClicksDisabled := False;
  FMouseMoveChangeIndex := False;
  FMouseDown := False;
  FMouseActive := -1;
  FScrollOffset := 0;
  FItems := TscHorzItems.Create(Self);
  FImages := nil;
  Width := 250;
  Height := 100;
  FItemWidth := 70;
  FMax := 0;
  FRealMax := 0;
  FOldWidth := -1;
  FItemIndex := -1;
  FDisabledFontColor := clGray;
end;

destructor TscHorzListBox.Destroy;
begin
  FItems.Free;
  Fitems := nil;
  inherited Destroy;
end;

procedure TscHorzListBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FItemWidth := MulDiv(FItemWidth, M, D);
end;

procedure TscHorzListBox.InitTouch;
begin
  Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions + [igoPanSingleFingerHorizontal, igoPanInertia];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [igPan, igPressAndTap];
end;

function TscHorzListBox.NextIndex(const S: string): Integer;
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
    if  FItems[I].Enabled then
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
      if FItems[I].Enabled then
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

function TscHorzListBox.IndexOf(const S: string; AStartOff: Boolean = False): Integer;
var
  I: Integer;
  FS, TS: String;
begin
  Result := -1;
  FS := LowerCase(S);
  for I := 0 to FItems.Count - 1 do
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

procedure TscHorzListBox.Loaded;
begin
  inherited;
  if FItemIndex <> -1 then ScrollToItem(FItemIndex);
end;

procedure TscHorzListBox.BeginUpdateItems;
begin
  FInUpdateItems := True;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TscHorzListBox.EndUpdateItems;
begin
  FInUpdateItems := False;
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
  RePaintControl;
  UpdateScrollInfo;
end;


procedure TscHorzListBox.SetShowGlow;
begin
  if FShowGlow <> Value
  then
    begin
      FShowGlow := Value;
      RePaintControl;
    end;
end;

procedure TscHorzListBox.SetItemLayout(Value: TButtonLayout);
begin
  FItemLayout := Value;
  RePaintControl;
end;

procedure TscHorzListBox.SetItemMargin(Value: Integer);
begin
  FItemMargin := Value;
  RePaintControl;
end;


procedure TscHorzListBox.SetItemSpacing(Value: Integer);
begin
  FItemSpacing := Value;
  RePaintControl;
end;

function TscHorzListBox.CalcWidth;
begin
  if AItemCount > FItems.Count then AItemCount := FItems.Count;
  Result := AItemCount * ItemWidth;
  Result := Result + Width - GetContentRect.Width;
end;

procedure TscHorzListBox.SetItemWidth(Value: Integer);
begin
  if FItemWidth <> Value
  then
    begin
      FItemWidth := Value;
      RePaintControl;
      UpdateScrollInfo;
    end;
end;

procedure TscHorzListBox.SetItems(Value: TscHorzItems);
begin
  FItems.Assign(Value);
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscHorzListBox.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    RePaintControl;
  end;
end;

procedure TscHorzListBox.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
   FImages := nil;
end;

procedure TscHorzListBox.CalcItemRects;
var
  I: Integer;
  X, Y, W, H: Integer;
begin
  FRealMax := 0;
  FItemsRect := GetContentRect;
  X := FItemsRect.Left;
  Y := FItemsRect.Top;
  H:= FItemsRect.Height;
  for I := 0 to FItems.Count - 1 do
    with TscHorzItem(FItems[I]) do
    begin
      W := ItemWidth;
      ItemRect := Rect(X, Y, X + W, Y + H);
      OffsetRect(ItemRect, - FScrollOffset, 0);
      IsVisible := RectToRect(ItemRect, FItemsRect);
      if not IsVisible and (ItemRect.Left <= FItemsRect.Left) and
        (ItemRect.Right >= FItemsRect.Right)
      then
        IsVisible := True;
      if IsVisible then FRealMax := ItemRect.Right;
      X := X + W;
    end;
  FMax := X;
end;

procedure TscHorzListBox.Scroll(AScrollOffset: Integer);
begin
  FScrollOffset := AScrollOffset;
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscHorzListBox.GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
begin
  CalcItemRects;
  AMin := 0;
  AMax := FMax - FItemsRect.Left;
  APage := FItemsRect.Width;
  if AMax <= APage
  then
    begin
      APage := 0;
      AMax := 0;
    end;
  APosition := FScrollOffset;
end;

procedure TscHorzListBox.EnableScrollTimer(Value: Integer);
begin
  if FTimerMode = 0 then
  begin
    FTimerMode := Value;
    SetTimer(Handle, 2, 50, nil);
  end
  else
    FTimerMode := Value;
end;

procedure TscHorzListBox.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscHorzListBox.WMTimer;
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

procedure TscHorzListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if (FOldWidth <> Height) and (FOldWidth <> -1)
  then
    begin
      CalcItemRects;
      if (FRealMax <= FItemsRect.Right) and (FScrollOffset > 0)
      then
        begin
          FScrollOffset := FScrollOffset - (FItemsRect.Right - FRealMax);
          if FScrollOffset < 0 then FScrollOffset := 0;
          CalcItemRects;
          RePaintControl;
        end;
    end;
  UpdateScrollInfo;
  FOldWidth := Width;
end;

procedure TscHorzListBox.ScrollToItem(Index: Integer);
var
  R, R1: TRect;
begin
  if FItems.Count = 0 then Exit;
  CalcItemRects;
  R1 := FItems[Index].ItemRect;
  R := R1;
  OffsetRect(R, FScrollOffset, 0);
  if (R1.Left <= FItemsRect.Left)
  then
    begin
      FScrollOffset := R.Left - FItemsRect.Left;
      CalcItemRects;
      RePaintControl;
    end
  else
  if R1.Right >= FItemsRect.Right
  then
    begin
      FScrollOffset := R.Left;
      FScrollOffset := FScrollOffset - FItemsRect.Width + R.Width -
        Width + FItemsRect.Right;
      CalcItemRects;
      RePaintControl;
    end;
  UpdateScrollInfo;
end;

procedure TscHorzListBox.UpdateScrollInfo;
var
  SMin, SMax, SPage, SPos: Integer;
begin
  if not HandleAllocated then Exit;
  if FInUpdateItems then Exit;
  GetScrollInfo(SMin, SMax, SPage, SPos);
  if SMax <> 0
  then
    begin
      if FHorzScrollBar = nil then ShowHorzScrollBar;
      Self.UpdateHorzScrollBar(SMin, SMax, SPos, SPage);
      if FHorzScrollBar <> nil then
      begin
        FHorzScrollBar.LargeChange := SPage;
        FHorzScrollBar.SmallChange := FItemWidth;
      end;
    end
  else
  if (SMax = 0) and (FHorzScrollBar <> nil) then
    HideHorzScrollBar;
end;

procedure TscHorzListBox.OnHorzScrollBarChange(Sender: TObject);
begin
  inherited;
  if FHorzScrollBar <> nil then
  begin
   if FHorzScrollBar.Position <= FHorzScrollBar.Max - FHorzScrollBar.PageSize + 1  then
     Scroll(FHorzScrollBar.Position)
   else
     Scroll(FHorzScrollBar.Max - FHorzScrollBar.PageSize + 1);
  end;
end;

procedure TscHorzListBox.SetItemIndex(Value: Integer);
var
  I: Integer;
  IsFind: Boolean;
begin
  if Value < 0
  then
    begin
      if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
        FItems[FItemIndex].Active := False;
      FItemIndex := Value;
      RePaintControl;
    end
  else
  if (Value >= 0) and (Value < FItems.Count) and not FItems[Value].Enabled then
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
          if I = FItemIndex
          then
            begin
              Active := True;
              IsFind := True;
            end
          else
             Active := False;
        end;
      RePaintControl;
      ScrollToItem(FItemIndex);
      if IsFind and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not FMouseDown
      then
      begin
        if Assigned(FItems[FItemIndex].OnClick) then
          FItems[FItemIndex].OnClick(Self);
        if Assigned(FOnItemClick) then
          FOnItemClick(Self);
      end;
    end;
end;

function TscHorzListBox.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if PtInRect(FItems[I].ItemRect, Point(X, Y)) and (FItems[I].Enabled)
    then
      begin
        Result := I;
        Break;
      end;
end;


procedure TscHorzListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  if (I <> -1) and (Button = mbLeft)
  then
    begin
      SetItemActive(I);
      FMouseDown := True;
      FMouseActive := I;
    end;
end;

procedure TscHorzListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
  R: TRect;
  CanClick: Boolean;
begin
  inherited;
  CanClick := FMouseDown;
  FMouseDown := False;
  StopScrollTimer;

  if FItems.Count = 0 then Exit;
  R := GetContentRect;
  if PtInRect(R, Point(X, Y)) then
  begin
    I := ItemAtPos(X, Y);
    if (I <> -1) and  (Button = mbLeft) then ItemIndex := I;
  end;

  if CanClick then
  begin
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) and
       Assigned(FItems[FItemIndex].OnClick) then
      FItems[FItemIndex].OnClick(Self);

    if Assigned(FOnItemClick) then
      FOnItemClick(Self);
  end;
end;

procedure TscHorzListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
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
    if (I <> -1) and (FMouseDown or FMouseMoveChangeIndex)
     and (I <> FMouseActive) then
    begin
      SetItemActive(I);
      FMouseActive := I;
    end;
   if (FBalloonHint <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      if FItems[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        ShowBalloonHint(FItems[I].Caption, FItems[I].ItemRect);
      end
      else
      begin
        FHintItemIndex := -1;
        if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      end;
    end;
    if (FHintComponent <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if FItems[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        FHintComponent.ActivateHint(FItems[I].Caption);
      end
      else
      begin
        FHintItemIndex := -1;
        FHintComponent.HideHint;
      end;
    end;
    if (I = -1) and (FBalloonHint <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
    end;
    if (I = -1) and (FHintComponent <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      FHintComponent.HideHint;
    end;
  end
  else
  if FMouseDown then
  begin
    if X < R.Left then
      EnableScrollTimer(1)
    else
    if X > R.Right then
      EnableScrollTimer(2)
    else
      StopScrollTimer;
  end;
end;

procedure TscHorzListBox.SetItemActive(Value: Integer);
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

procedure TscHorzListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHintItemIndex <> -1 then
  begin
    FHintItemIndex := -1;
    if (FBalloonHint <> nil) and (FBalloonHint.ShowingHint) then
      FBalloonHint.HideHint;
    if (FHintComponent <> nil) then
      FHintComponent.HideHint;
  end;
end;

procedure TscHorzListBox.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FHorzScrollBar = nil then Exit;
  if TWMMOUSEWHEEL(Message).WheelDelta < 0
  then
    FHorzScrollBar.Position :=  FHorzScrollBar.Position + FHorzScrollBar.SmallChange
  else
    FHorzScrollBar.Position :=  FHorzScrollBar.Position - FHorzScrollBar.SmallChange;
end;

procedure TscHorzListBox.WMSETFOCUS(var Message: TWMSETFOCUS);
begin
  inherited;
  FUpdateParentBuffer := True;
  RePaint;
end;

procedure TscHorzListBox.WMKILLFOCUS(var Message: TWMKILLFOCUS);
begin
  inherited;
  RePaint;
end;

procedure TscHorzListBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and not Focused then
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

procedure TscHorzListBox.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscHorzListBox.FindUp;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Exit;
  Start := FItemIndex - 1;
  if Start < 0 then Exit;
  for I := Start downto 0 do
  begin
    if FItems[I].Enabled
    then
      begin
        ItemIndex := I;
        Exit;
      end;
  end;
end;

procedure TscHorzListBox.FindDown;
var
  I, Start: Integer;
begin
  if ItemIndex <= -1 then Start := 0 else Start := FItemIndex + 1;
  if Start > FItems.Count - 1 then Exit;
  for I := Start to FItems.Count - 1 do
  begin
    if FItems[I].Enabled
    then
      begin
        ItemIndex := I;
        Exit;
      end;
  end;
end;

procedure TscHorzListBox.FindPageUp;
var
  I, J, Start: Integer;
  PageCount: Integer;
begin
  if ItemIndex <= -1 then Exit;
  Start := FItemIndex - 1;
  if Start < 0 then Exit;
  PageCount := FItemsRect.Width div FItemWidth;
  if PageCount = 0 then PageCount := 1;
  PageCount := Start - PageCount;
  if PageCount < 0 then PageCount := 0;
  J := -1;
  for I := Start downto PageCount do
  begin
    if FItems[I].Enabled
    then
      begin
        J := I;
      end;
  end;
  if J <> -1 then ItemIndex := J;
end;


procedure TscHorzListBox.FindPageDown;
var
  I, J, Start: Integer;
  PageCount: Integer;
begin
  if ItemIndex <= -1 then Start := 0 else Start := FItemIndex + 1;
  if Start > FItems.Count - 1 then Exit;
  PageCount := FItemsRect.Width div FItemWidth;
  if PageCount = 0 then PageCount := 1;
  PageCount := Start + PageCount;
  if PageCount > FItems.Count - 1 then PageCount := FItems.Count - 1;
  J := -1;
  for I := Start to PageCount do
  begin
    if FItems[I].Enabled
    then
      begin
        J := I;
      end;
  end;
  if J <> -1 then ItemIndex := J;
end;

procedure TscHorzListBox.KeyPress(var Key: Char);
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
    I := IndexOf(FSearchString, True);
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

procedure TscHorzListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited KeyDown(Key, Shift);
 case Key of
   VK_NEXT:  FindPageDown;
   VK_PRIOR: FindPageUp;
   VK_UP, VK_LEFT: FindUp;
   VK_DOWN, VK_RIGHT: FindDown;
 end;
end;

function TscHorzListBox.CanDrawContent: Boolean;
begin
  Result := Assigned(FItems) and (FItems.Count > 0);
end;

procedure TscHorzListBox.DrawContent(ACanvas: TCanvas; ARect: TRect);
var
  I, SaveIndex: Integer;
begin
  CalcItemRects;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,
      FItemsRect.Left, FItemsRect.Top, FItemsRect.Right, FItemsRect.Bottom);
    for I := 0 to FItems.Count - 1 do
      if FItems[I].IsVisible then DrawItem(I, ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscHorzListBox.DrawItem(Index: Integer; Cnvs: TCanvas);
var
  R: TRect;
  FC: TColor;
  SaveIndex: Integer;
  FGlowAlpha: Byte;
  FMargin: Integer;
begin
  if (FItems[Index].Active and FItems[Index].Enabled) and (SelectionStyle <> scastGlow) then
  begin
    if (SelectionStyle = scastColor)
    then
    begin
       if FSelectionColor <> clNone then
        Cnvs.Brush.Color := FSelectionColor
      else
        Cnvs.Brush.Color := GetStyleColor(clHighLight);
      if not Focused and not FShowFocusRect then
        FillRectWithAlpha(Cnvs, FItems[Index].ItemRect, 200)
      else
        Cnvs.FillRect(FItems[Index].ItemRect);
      if FSelectionColor <> clNone then
        FC := FSelectionTextColor
      else
      if IsCustomStyle then
        FC := GetSelectionTextColor
      else
        FC := clHighLightText;
    end
    else
    begin
      R := FItems[Index].ItemRect;
      if ((SelectionStyle = scastCustomImage) or (SelectionStyle = scastCustomImageWithGlow)) and
         (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomSelectionImageIndex) and
          FCustomImages.IsIndexAvailable(FCustomFocusedSelectionImageIndex)
      then
      begin
        if Self.Focused then
          FCustomImages.Draw(Cnvs, R, FCustomFocusedSelectionImageIndex, FScaleFactor)
        else
          FCustomImages.Draw(Cnvs, R, FCustomSelectionImageIndex, FScaleFactor);
        FC := GetStyleColor(FSelectionTextColor);
      end
      else
      begin
        DrawSelection(Cnvs, R, Self.Focused, FShowFocusRect);
        FC := GetSelectionTextColor;
      end;
    end
  end
  else
  begin
    if IsCustomStyle and (seFont in StyleElements) then
    begin
      if BackgroundStyle = scbgsColor then
        FC := GetEditTextColor(scsNormal)
      else
        FC := GetCheckBoxTextColor(scsNormal)
    end
    else
      FC := Font.Color;
    if (not FItems[Index].Enabled) or not Self.Enabled then
    begin
      if IsCustomStyle and (seFont in StyleElements) then
      begin
        if BackgroundStyle = scbgsColor then
          FC := GetEditTextColor(scsDisabled)
        else
          FC := GetCheckBoxTextColor(scsDisabled)
      end
      else
        FC := clGrayText;
    end;
  end;

  Cnvs.Font := Self.Font;
  Cnvs.Font.Color := FC;
  Cnvs.Brush.Style := bsClear;

  R := FItems[Index].ItemRect;

  SaveIndex := SaveDC(Cnvs.Handle);
  try
  IntersectClipRect(Cnvs.Handle, R.Left, R.Top, R.Right, R.Bottom);

  FMargin := FItemMargin;
  if FSelectionStyle = scastGlow then
  begin
    InflateRect(R, -FSelectionGlow.GlowSize, -FSelectionGlow.GlowSize);
    if FMargin > 0 then
    begin
      FMargin := FMargin - FSelectionGlow.GlowSize + 2;
      if FMargin < 0 then FMargin := 0;
    end;
  end
  else
    InflateRect(R, -2, -2);

  with FItems[Index] do
  begin
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Cnvs, Index, FItems[Index].ItemRect)
    else
      begin
        FGlowAlpha := FSelectionGlow.AlphaValue;
        if not FShowFocusRect and not Self.Focused then
        begin
          if FGlowAlpha > 60 then
           FGlowAlpha := FGlowAlpha - 60;
        end;
        if ((SelectionStyle = scastGlow) or (SelectionStyle = scastCustomImageWithGlow)) and (FItems[Index].Active and FItems[Index].Enabled) then
          FItems[Index].FShowEllipses := DrawImageAndTextEllipsesWithGlow(Cnvs, R, FMargin, FItemSpacing,
            FItemLayout,
            Caption, FImageIndex, FImages,
            FItems[Index].Enabled and Self.Enabled, IsRightToLeft,
            (FItemLayout = blGlyphLeft) or (FItemLayout = blGlyphRight),
            FSelectionGlow.Offset, FSelectionGlow.Color,FSelectionGlow.GlowSize,
            FSelectionGlow.Intensive, FGlowAlpha, True, True, FScaleFactor)
         else
           FItems[Index].FShowEllipses := DrawImageAndTextEllipses(Cnvs, R, FMargin, FItemSpacing, FItemLayout,
             Caption, FImageIndex, FImages,
               FItems[Index].Enabled and Self.Enabled, IsRightToLeft, True, FScaleFactor);
      end;
   end;

  if (Self.ItemIndex = -1) and Focused and (Index = 0) then
  begin
    R := FItems[Index].ItemRect;
    if FShowFocusRect then scDrawFocusRect(Cnvs, R, FScaleFactor);
  end
  else
  if FItems[Index].Active and Focused then
  begin
    R := FItems[Index].ItemRect;
    if FShowFocusRect then
    begin
      if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
        and (FSelectionStyle = scastStyled)
      then
        InflateRect(R, -1, -1);
      scDrawFocusRect(Cnvs, R, FScaleFactor);
    end;
  end;

  finally
    RestoreDC(Cnvs.Handle, SaveIndex);
  end;
end;

constructor TscPopupGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FPopupMode := True;
  DrawOnBackground := False;
  TabStop := False;
  Visible := False;
end;

procedure TscPopupGridView.DrawBorder(ACanvas: TCanvas; ARect: TRect);
begin
  DrawSingleBorder(ACanvas, ARect);
end;

procedure TscPopupGridView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscPopupGridView.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscPopupGridView.Hide;
begin
  FMouseActive := -1;
  if FHintItemIndex <> -1 then
  begin
    FHintItemIndex := -1;
    if (FBalloonHint <> nil) and (FBalloonHint.ShowingHint) then
      FBalloonHint.HideHint;
    if FHintComponent <> nil then
      FHintComponent.HideHint;
  end;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TscPopupGridView.Show(Origin: TPoint);
begin
  if not IsCustomStyle and StyleServices.Enabled and
    (FVertScrollBar <> nil) and (FVertScrollBar.Visible) then
  begin
    FVertScrollBar.Perform(CM_RECREATEWND, 0, 0);
  end;
  FMouseActive := -1;
  FScrollOffset := 0;
  ScrollToItem(ItemIndex);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;


constructor TscGridViewCustomComboBox.Create;
begin
  inherited Create(AOwner);
  FAutoComplete := False;
  FSearchString := '';
  FSearchTimerEnabled := False;
  FGridView := TscPopupGridView.Create(Self);
  FGridViewWallpaperIndex := -1;
  FGridView.Width := 200;
  FGridView.Height := 200;
  FGridViewWallpapers := nil;
  FShowItemImage := True;
  FShowItemText := True;
  FDropDown := False;
  TabStop := True;
  FLBDown := False;
  WasInLB := False;
  FTimerMode := 0;
  Width := 120;
  Height := 41;
  FGridViewWindowProc := FGridView.WindowProc;
  FGridView.WindowProc := GridViewWindowProcHook;
  FGridView.Visible := False;
  FGridView.MouseMoveChangeIndex := True;
  if not (csDesigning in ComponentState) then
    FGridView.Parent := Self;
  FLBDown := False;
  FLastTime := 0;
  FGridViewWidth := 200;
  FGridViewHeight := 200;
end;

procedure TscGridViewCustomComboBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FGridViewWidth := MulDiv(FGridViewWidth, M, D);
  FGridViewHeight := MulDiv(FGridViewHeight, M, D);
end;

procedure TscGridViewCustomComboBox.BeginUpdateItems;
begin
  FGridView.BeginUpdateItems;
end;

function TscGridViewCustomComboBox.NextIndex(const S: string): Integer;
begin
  Result := FGridView.NextIndex(S);
end;

function TscGridViewCustomComboBox.IndexOf(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FGridView.IndexOf(S, AStartOff);
end;

procedure TscGridViewCustomComboBox.EndUpdateItems;
begin
  FGridView.EndUpdateItems;
end;

function TscGridViewCustomComboBox.CanAnimate: Boolean;
begin
  Result := inherited CanAnimate;
  if Result then
     Result := not FGridView.Visible;
end;

function TscGridViewCustomComboBox.GetGridViewCustomHeaderImageIndex: Integer;
begin
  Result := FGridView.CustomHeaderImageIndex;
end;

procedure TscGridViewCustomComboBox.SetGridViewCustomHeaderImageIndex(Value: Integer);
begin
  FGridView.CustomHeaderImageIndex := Value;
end;

function TscGridViewCustomComboBox.GetGridViewCustomSelectionImageIndex: Integer;
begin
  Result := FGridView.CustomSelectionImageIndex;
end;

procedure TscGridViewCustomComboBox.SetGridViewCustomSelectionImageIndex(Value: Integer);
begin
  FGridView.CustomSelectionImageIndex := Value;
  FGridView.CustomFocusedSelectionImageIndex := Value;
end;

function TscGridViewCustomComboBox.CanDrawItem: Boolean;
var
  Index: Integer;
begin
  if FDropDown then Index := FOldItemIndex else Index := ItemIndex;
  Result := (Index >= 0) and (Index < Items.Count);
end;

procedure TscGridViewCustomComboBox.DrawComboItem(ACanvas: TCanvas; ARect: TRect);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  R, R1: TRect;
  TX, TY: Integer;
  S: String;
  Index: Integer;
begin
  R := ARect;
  InflateRect(R, -2, -2);
  if FDropDown then Index := FOldItemIndex else Index := ItemIndex;
  if not ((Index >= 0) and (ItemIndex < Items.Count)) then Exit;
  ACanvas.Brush.Style := bsClear;

  if Assigned(FOnDrawItem) then
  begin
    FOnDrawItem(ACanvas, Index, R);
    Exit;
  end;

  with Items[Index] do
  begin
    if (Images <> nil) and (ImageIndex >= 0) and
       (ImageIndex < Images.Count) and FShowItemImage then
    begin
      S := Caption;
      if not FShowItemText then S := '';
      if not HideSelection and (SelectionStyle = scastGlow) and (Focused and Self.Enabled) then
        DrawImageAndTextWithGlow2(ACanvas, R, 0, 5, GlyphLayout[IsRightToLeft],
          S, FImageIndex, Images, Self.Enabled, False, clBlack,
          FSelectionGlow.Offset, FSelectionGlow.Color,
          FSelectionGlow.GlowSize, FSelectionGlow.Intensive,
          FSelectionGlow.AlphaValue, True, False, IsRightToLeft, True)
      else
        DrawImageAndText2(ACanvas, R, 0, 5, GlyphLayout[IsRightToLeft],
          S, FImageIndex, Images,
          Self.Enabled, False, clBlack, False, IsRightToLeft, True)
    end
    else
    if FShowItemText then
    begin
      if (Images <> nil) and FShowItemImage then Inc(R.Left, Images.Width + 5);
      R1 := Rect(0, 0, R.Width, R.Height);
      DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R1,
        DT_LEFT or DT_CALCRECT or DT_WORDBREAK);
      TX := R.Left;
      TY := R.Top + R.Height div 2 - R1.Height div 2;
      if TY < R.Top then TY := R.Top;
      R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
      if not HideSelection and (SelectionStyle = scastGlow) and (Focused and Self.Enabled) then
       DrawTextWithGlow(ACanvas, R, Caption, DT_WORDBREAK or DT_LEFT,
          FSelectionGlow.Offset, FSelectionGlow.Color, FSelectionGlow.GlowSize,
          FSelectionGlow.Intensive,FSelectionGlow.AlphaValue, IsRightToLeft, True)
      else
        DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R,
        scDrawTextBidiModeFlags(DT_WORDBREAK or DT_LEFT, BidiMode = bdRightToLeft));
    end;
  end;
end;

procedure TscGridViewCustomComboBox.GridViewWindowProcHook(var Message: TMessage);
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
  if FOld then FGridViewWindowProc(Message);
end;

procedure TscGridViewCustomComboBox.KeyPress;
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
    I := IndexOf(FSearchString, True);
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

destructor TscGridViewCustomComboBox.Destroy;
begin
  FGridView.Free;
  FGridView := nil;
  inherited;
end;

procedure TscGridViewCustomComboBox.CMEnabledChanged;
begin
  inherited;
  RePaintControl;
end;

procedure TscGridViewCustomComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
  if (Operation = opRemove) and (AComponent = FGridViewWallpapers) then
  begin
    if (FGridView <> nil) and (FGridView.Wallpapers = FGridViewWallpapers) then
      FGridView.Wallpapers := nil;
    FGridViewWallpapers := nil;
  end;
  if (Operation = opRemove) and (FGridView <> nil) and (AComponent = GridViewBalloonHint) then
   GridViewBalloonHint := nil;
end;

function TscGridViewCustomComboBox.GetImages: TCustomImageList;
begin
  if FGridView <> nil
  then
    Result := FGridView.Images
   else
    Result := nil;
end;

procedure TscGridViewCustomComboBox.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (Message.wParam <> FGridView.Handle) and
     not ((FGridView.FVertScrollBar <>  nil) and
        (Message.wParam = FGridView.FVertScrollBar.Handle))

  then
    CloseUp(False);
end;

procedure TscGridViewCustomComboBox.CMCancelMode;
begin
  inherited;
  if (Message.Sender = nil) or (
     (Message.Sender <> Self) and
     (Message.Sender <> Self.FGridView) and
     (Message.Sender <> Self.FGridView.FVertScrollBar))
  then
    CloseUp(False);
end;

procedure TscGridViewCustomComboBox.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscGridViewCustomComboBox.CheckButtonClick;
begin
  CloseUp(True);
end;

procedure TscGridViewCustomComboBox.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  case Msg.CharCode of
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:  Msg.Result := 1;
  end;
end;

procedure TscGridViewCustomComboBox.KeyDown;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP, VK_LEFT:
      if ssAlt in Shift
      then
        begin
          if FGridView.Visible then CloseUp(False);
        end
      else
      begin
        if Key = VK_Left then
          ComboKeyLeft(True)
        else
          ComboKeyUp(True);
      end;
    VK_DOWN, VK_RIGHT:
      if ssAlt in Shift
      then
        begin
          if not FGridView.Visible then DropDown;
        end
      else
      begin
        if Key = VK_RIGHT then
          ComboKeyRight(True)
        else
          ComboKeyDown(True);
      end;

    VK_NEXT: ComboPageDown(True);
    VK_PRIOR: ComboPageUp(True);
    VK_ESCAPE: if FGridView.Visible then CloseUp(False);
    VK_RETURN: if FGridView.Visible then CloseUp(True);
  end;
end;

procedure TscGridViewCustomComboBox.WMMOUSEWHEEL;
begin
  inherited;
  if TWMMOUSEWHEEL(Message).WheelDelta > 0
  then
    ComboKeyUp(not FGridView.Visible)
  else
    ComboKeyDown(not FGridView.Visible);
end;

procedure TscGridViewCustomComboBox.WMSETFOCUS;
begin
  inherited;
  FUpdateParentBuffer := True;
  RePaint;
end;

procedure TscGridViewCustomComboBox.WMKILLFOCUS;
begin
  inherited;
  if FGridView.Visible  then CloseUp(False);
  RePaint;
end;

function TscGridViewCustomComboBox.GetItemIndex;
begin
  Result := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.InitItemIndex(Value: Integer);
begin
  FGridView.InitItemIndex(Value);
  FOldItemIndex := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.SetItemIndex;
begin
  FGridView.ItemIndex := Value;
  FOldItemIndex := FGridView.ItemIndex;
  RePaintControl;
  if not (csDesigning in ComponentState) and
     not (csLoading in ComponentState)
  then
    begin
      if Assigned(FOnClick) then FOnClick(Self);
      Change;
    end;
end;

function TscGridViewCustomComboBox.IsPopupVisible: Boolean;
begin
  Result := FGridView.Visible;
end;

function TscGridViewCustomComboBox.CanCancelDropDown;
begin
  Result := FGridView.Visible and not FMouseIn;
end;

procedure TscGridViewCustomComboBox.EnableScrollTimer(Value: Integer);
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

procedure TscGridViewCustomComboBox.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscGridViewCustomComboBox.WMTimer;
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
      1: FGridView.FindUp;
      2: FGridView.FindDown;
    end;
  end;
end;

procedure TscGridViewCustomComboBox.ProcessGridView;
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := FGridView.ScreenToClient(P);
  if (P.Y < 0) and (FGridView.FVertScrollBar <> nil) and WasInLB and FLBDown
  then
    EnableScrollTimer(1)
  else
  if (P.Y > FGridView.Height) and (FGridView.FVertScrollBar <> nil) and WasInLB and FLBDown
  then
    EnableScrollTimer(2)
  else
    if (P.Y >= 0) and (P.Y <= FGridView.Height)
    then
      begin
        StopScrollTimer;
        FGridView.MouseMove([], P.X, P.Y);
        WasInLB := True;
        if not FLBDown
        then
          begin
            FLBDown := True;
            WasInLB := False;
          end;
      end;
end;

function TscGridViewCustomComboBox.GetItems;
begin
  Result := FGridView.Items;
end;

procedure TscGridViewCustomComboBox.SetItems;
begin
  FGridView.Items.Assign(Value);
end;

procedure TscGridViewCustomComboBox.MouseDown;
begin
  inherited;
  if not Focused then SetFocus;
  if Button <> mbLeft then Exit;
  if Self.Button.MouseIn or
     PtInRect(CBItem.R, Point(X, Y)) or (FStyle <> sccbCombo)
  then
    begin
      Self.Button.Down := True;
      RePaintControl;
      if FGridView.Visible then CloseUp(False)
      else
        begin
          WasInLB := False;
          FLBDown := True;
          DropDown;
        end;
    end
  else
    if FGridView.Visible then CloseUp(False);
end;

procedure TscGridViewCustomComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
  P: TPoint;
begin
  if FLBDown and WasInLB
  then
    begin
      ReleaseCapture;
      FLBDown := False;
      GetCursorPos(P);
      if WindowFromPoint(P) = FGridView.Handle
      then
        CloseUp(True)
      else
        CloseUp(False);
    end
  else
     FLBDown := False;
  inherited;
  if Self.Button.Down
  then
    begin
      Self.Button.Down := False;
      RePaintControl;
    end;
end;

procedure TscGridViewCustomComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FGridView.Visible then ProcessGridView;
end;

procedure TscGridViewCustomComboBox.CloseUp;
begin
  SC_UnHookMouseMessages;
  StopScrollTimer;
  if not FGridView.Visible then Exit;
  FGridView.Hide;
  FDropDown := False;
  if (FGridView.ItemIndex >= 0) and
     (FGridView.ItemIndex < FGridView.Items.Count) and Value
  then
    begin
       RePaintControl;
       if Assigned(FOnCloseUp) then FOnCloseUp(Self);
       if Assigned(FOnClick) then FOnClick(Self);
       Change;
     end
  else
  begin
    FGridView.ItemIndex := FOldItemIndex;
    RePaintControl;
  end;
end;

procedure TscGridViewCustomComboBox.DropDown;
var
  P: TPoint;
  WorkArea: TRect;
begin
  if Animation then StopAnimation;
  if Items.Count = 0 then Exit;
  WasInLB := False;
  StopScrollTimer;
  if Assigned(FOnDropDown) then FOnDropDown(Self);

  if FGridViewWidth = 0
  then
    FGridView.Width := Width
  else
    FGridView.Width := FGridViewWidth;

  FGridView.OnDrawItem := Self.OnDrawItem;
  FGridView.Font.Assign(Font);
  FGridView.Color := Color;
  FGridView.BidiMode := Self.BiDiMode;
  FGridView.StyleElements := StyleElements;
  FGridView.Wallpapers := GridViewWallpapers;
  FGridView.CustomImages := Self.CustomImages;
  FGridView.WallpaperIndex := GridViewWallpaperIndex;
  FGridView.Width := FGridViewWidth;
  FGridView.Height := FGridViewHeight;

  if FDropDownPosition = scdpRight then
    P := Point(Left + Width - FGridView.Width, Top + Height)
  else
    P := Point(Left, Top + Height);

  P := Parent.ClientToScreen(P);

  WorkArea := Screen.MonitorFromWindow(Handle).WorkAreaRect;

  if P.Y + FGridView.Height > WorkArea.Bottom
  then
    P.Y := P.Y - Height - FGridView.Height;

  FOldItemIndex := FGridView.ItemIndex;

  FDropDown := True;
  RePaintControl;
  FGridView.Show(P);
  SC_HookMouseMessages(Self);
end;

procedure TscGridViewCustomComboBox.ComboPageUp;
begin
  FGridView.FindPageUp;
  if AChange then ItemIndex := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.ComboPageDown(AChange: Boolean);
begin
  FGridView.FindPageDown;
  if AChange then ItemIndex := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.ComboKeyLeft;
begin
  FGridView.FindLeft;
  if AChange then ItemIndex := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.ComboKeyRight;
begin
  FGridView.FindRight;
  if AChange then ItemIndex := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.ComboKeyUp;
begin
  FGridView.FindLeft;
  if AChange then ItemIndex := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.ComboKeyDown;
begin
  FGridView.FindRight;
  if AChange then ItemIndex := FGridView.ItemIndex;
end;

procedure TscGridViewCustomComboBox.SetImages(Value: TCustomImageList);
begin
  if FGridView.Images <> Value then
  begin
    FGridView.Images := Value;
    RePaintControl;
  end;
end;

procedure TscGridViewCustomComboBox.SetGridViewWallpapers(Value: TscCustomImageCollection);
begin
  FGridViewWallpapers := Value;
end;

procedure TscGridViewCustomComboBox.SetGridViewWallpaperIndex(Value: Integer);
begin
  FGridViewWallpaperIndex := Value;
end;

function TscGridViewCustomComboBox.GetGridViewHintComponent: TscHint;
begin
  Result := FGridView.HintComponent;
end;

procedure TscGridViewCustomComboBox.SetGridViewHintComponent(Value: TscHint);
begin
  GridView.HintComponent := Value;
end;

function TscGridViewCustomComboBox.GetGridViewBalloonHint: TBalloonHint;
begin
  Result := FGridView.BalloonHint;
end;

procedure TscGridViewCustomComboBox.SetGridViewBalloonHint(Value: TBalloonHint);
begin
  FGridView.BalloonHint := Value;
end;

function TscGridViewCustomComboBox.GetGridViewSelectionColor: TColor;
begin
  Result := FGridView.SelectionColor;
end;

procedure TscGridViewCustomComboBox.SetGridViewSelectionColor(Value: TColor);
begin
  FGridView.SelectionColor := Value;
end;

function TscGridViewCustomComboBox.GetGridViewSelectionTextColor: TColor;
begin
  Result := FGridView.SelectionTextColor;
end;

procedure TscGridViewCustomComboBox.SetGridViewSelectionTextColor(Value: TColor);
begin
  FGridView.SelectionTextColor := Value;
end;

function TscGridViewCustomComboBox.GetGridViewHeaderUseStyleColor: Boolean;
begin
  Result := FGridView.HeaderUseStyleColor;
end;

procedure TscGridViewCustomComboBox.SetGridViewHeaderUseStyleColor(Value: Boolean);
begin
  FGridView.HeaderUseStyleColor := Value;
end;

function TscGridViewCustomComboBox.GetGridViewHeaderStyle: TscAdvancedHeaderStyle;
begin
  Result := FGridView.HeaderStyle;
end;

procedure TscGridViewCustomComboBox.SetGridViewHeaderStyle(Value: TscAdvancedHeaderStyle);
begin
  FGridView.HeaderStyle := Value;
end;

function TscGridViewCustomComboBox.GetGridViewSelectionStyle: TscAdvancedSelectionStyle;
begin
  Result := FGridView.SelectionStyle;
end;

procedure TscGridViewCustomComboBox.SetGridViewSelectionStyle(Value: TscAdvancedSelectionStyle);
begin
  FGridView.SelectionStyle := Value;
end;

function TscGridViewCustomComboBox.GetGridViewItemWidth: Integer;
begin
  Result := FGridView.ItemWidth;
end;

procedure TscGridViewCustomComboBox.SetGridViewItemWidth(Value: Integer);
begin
  FGridView.ItemWidth := Value;
end;

function TscGridViewCustomComboBox.GetGridViewItemHeight: Integer;
begin
  Result := FGridView.ItemHeight;
end;

procedure TscGridViewCustomComboBox.SetGridViewItemHeight(Value: Integer);
begin
  FGridView.ItemHeight := Value;
end;

function TscGridViewCustomComboBox.GetGridViewHeaderHeight: Integer;
begin
  Result := FGridView.HeaderHeight;
end;

procedure TscGridViewCustomComboBox.SetGridViewHeaderHeight(Value: Integer);
begin
  FGridView.HeaderHeight := Value;
end;

procedure TscGridViewCustomComboBox.SetShowItemImage(Value: Boolean);
begin
  if FShowItemImage <> Value then
  begin
    FShowItemImage := Value;
    RePaintControl;
  end;
end;

procedure TscGridViewCustomComboBox.SetShowItemText(Value: Boolean);
begin
  if FShowItemText <> Value then
  begin
    FShowItemText := Value;
    RePaintControl;
  end;
end;


constructor TscGalleryItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FCaption := '';
  MouseIn := False;
  PopupMouseIn := False;
  if TscGalleryItems(Collection).FGallery.ItemIndex = Self.Index
  then
    begin
      Active := True;
      PopupActive := True;
    end
  else
    begin
      Active := False;
      PopupActive := False;
    end;
end;

procedure TscGalleryItem.Assign(Source: TPersistent);
begin
  if Source is TscGalleryItem then
  begin
    FImageIndex := TscGalleryItem(Source).ImageIndex;
    FCaption := TscGalleryItem(Source).Caption;
  end
  else
    inherited Assign(Source);
end;

procedure TscGalleryItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed(False);
end;

procedure TscGalleryItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TscGalleryItem.SetEnabled(Value: Boolean);
begin
  Changed(False);
end;

procedure TscGalleryItem.SetData(const Value: TCustomData);
begin
  FData := Value;
end;

constructor TscGalleryItems.Create;
begin
  inherited Create(TscGalleryItem);
  FGallery := AFGallery;
end;

function TscGalleryItems.GetOwner: TPersistent;
begin
  Result := FGallery;
end;

procedure  TscGalleryItems.Update(Item: TCollectionItem);
begin
  FGallery.Repaint;
  FGallery.UpdateScrollInfo;
end;

function TscGalleryItems.GetItem(Index: Integer):  TscGalleryItem;
begin
  Result := TscGalleryItem(inherited GetItem(Index));
end;

procedure TscGalleryItems.SetItem(Index: Integer; Value:  TscGalleryItem);
begin
  inherited SetItem(Index, Value);
  FGallery.RePaintControl;
end;

function TscGalleryItems.Add: TscGalleryItem;
begin
  Result := TscGalleryItem(inherited Add);
  FGallery.RePaintControl;
end;

function TscGalleryItems.Insert(Index: Integer): TscGalleryItem;
begin
  Result := TscGalleryItem(inherited Insert(Index));
  FGallery.RePaintControl;
end;

procedure TscGalleryItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
  FGallery.RePaintControl;
end;

procedure TscGalleryItems.Clear;
begin
  inherited Clear;
  FGallery.FItemIndex := -1;
  FGallery.RePaintControl;
end;

constructor TscGalleryDropDownButton.Create(AOwner: TComponent);
begin
  inherited;
  TransparentBackground := False;
  CanFocused := False;
  TabStop := False;
end;

procedure TscGalleryDropDownButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  C: TColor;
  R: TRect;
begin
  inherited;
  C := ACanvas.Font.Color;
  R := Rect(2, 2, Width - 2, Height - 2);
  DrawDownArrowImage(ACanvas, R, C, FScaleFactor);
end;

constructor TscGallery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  FScrollPanel := nil;
  FPopupGallery := TscPopupGallery.Create(Self);
  FPopupGallery.Visible := False;
  if not (csDesigning in ComponentState) then
    FPopupGallery.Parent := Self;
  ItemsInPage := 0;
  FPopupMaxRowCount := 3;
  FMouseInItem := -1;
  FOldMouseInItem := -1;
  FStartIndex := 0;
  FItemMargin := -1;
  FItemSpacing := 1;
  FItemLayout := blGlyphTop;
  FClicksDisabled := False;
  FMouseDown := False;
  FMouseActive := -1;
  UpDown := nil;
  DownButton := nil;
  FItems := TscGalleryItems.Create(Self);
  FImages := nil;
  Width := 250;
  Height := 100;
  FItemWidth := 45;
  FItemHeight := 45;
  FOldWidth := -1;
  FItemIndex := -1;
  FTouchBegin := 0;
  FTouchEnd := 0;
  InitTouch;
  ShowUpDown;
end;

destructor TscGallery.Destroy;
begin
  HideUpdown;
  FPopupGallery.Free;
  FItems.Free;
  FItems := nil;
  inherited Destroy;
end;

procedure TscGallery.CMGesture(var Message: TCMGesture);
begin
  inherited;
  if gfBegin in Message.Info^.Flags
  then
    FTouchBegin := Message.Info^.Location.Y
  else
  if (gfInertia in Message.Info^.Flags) and (FTouchBegin <> FTouchEnd) then
  begin
    FTouchEnd := Message.Info^.Location.Y;
    if FTouchEnd > FTouchBegin then
      OnUpDownClick(Self, btNext)
    else
      OnUpDownClick(Self, btPrev);
    FTouchBegin := FTouchEnd;
  end;
end;

procedure TscGallery.InitTouch;
begin
  Touch.InteractiveGestureOptions := Touch.InteractiveGestureOptions + [igoPanSingleFingerVertical, igoPanInertia];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [igPan, igPressAndTap];
end;

procedure TscGallery.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FItemHeight := MulDiv(FItemHeight, M, D);
  FItemWidth := MulDiv(FItemWidth, M, D);
  FItemSpacing := MulDiv(FItemSpacing, M, D);
end;

function TscGallery.GetContentRect: TRect;
begin
  Result := inherited GetContentRect;
  if Bidimode = bdRightToLeft then
    Inc(Result.Left, GetSystemMetrics(SM_CYHSCROLL))
  else
    Dec(Result.Right, GetSystemMetrics(SM_CYHSCROLL));
end;

function TscGallery.CanDrawContent: Boolean;
begin
  Result := Assigned(FItems) and (FItems.Count > 0);
end;

procedure TscGallery.DrawContent(ACanvas: TCanvas; ARect: TRect);
var
  I, SaveIndex: Integer;
begin
  CalcItemRects;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,
      FItemsRect.Left, FItemsRect.Top, FItemsRect.Right, FItemsRect.Bottom);
    for I := 0 to FItems.Count - 1 do
      if FItems[I].IsVisible then DrawItem(I, ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscGallery.DrawItem(Index: Integer; Cnvs: TCanvas);
var
  R: TRect;
  FC: TColor;
  SaveIndex: Integer;
  FGlowAlpha: Byte;
  FMargin: Integer;
begin
  if FItems[Index].Active and (SelectionStyle <> scastGlow) then
  begin
    if SelectionStyle = scastColor
    then
    begin
      if FSelectionColor <> clNone then
        Cnvs.Brush.Color := FSelectionColor
      else
        Cnvs.Brush.Color := GetStyleColor(clHighLight);

      if not Focused and not FShowFocusRect then
        FillRectWithAlpha(Cnvs, FItems[Index].ItemRect, 200)
      else
        Cnvs.FillRect(FItems[Index].ItemRect);

      if FSelectionColor <> clNone then
        FC := FSelectionTextColor
      else
      if IsCustomStyle then
        FC := GetSelectionTextColor
      else
        FC := clHighLightText;
    end
    else
    begin
      R := FItems[Index].ItemRect;
     if ((SelectionStyle = scastCustomImage) or (SelectionStyle = scastCustomImageWithGlow)) and
         (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomSelectionImageIndex) and
          FCustomImages.IsIndexAvailable(FCustomFocusedSelectionImageIndex)
      then
      begin
        if Self.Focused then
          FCustomImages.Draw(Cnvs, R, FCustomFocusedSelectionImageIndex, FScaleFactor)
        else
          FCustomImages.Draw(Cnvs, R, FCustomSelectionImageIndex, FScaleFactor);
        FC := GetStyleColor(FSelectionTextColor);
      end
      else
      begin
        DrawSelection(Cnvs, R, Self.Focused, FShowFocusRect);
        FC := GetSelectionTextColor;
      end;
    end
  end
  else
  begin
    if IsCustomStyle and (seFont in StyleElements) then
    begin
      if BackgroundStyle = scbgsColor then
        FC := GetEditTextColor(scsNormal)
      else
        FC := GetCheckBoxTextColor(scsNormal)
    end
    else
      FC := Font.Color;
    if not Self.Enabled then
    begin
      if IsCustomStyle and (seFont in StyleElements) then
      begin
        if BackgroundStyle = scbgsColor then
          FC := GetEditTextColor(scsDisabled)
        else
          FC := GetCheckBoxTextColor(scsDisabled)
      end
      else
        FC := clGrayText;
    end;
  end;

  Cnvs.Font := Self.Font;
  Cnvs.Font.Color := FC;
  Cnvs.Brush.Style := bsClear;

  R := FItems[Index].ItemRect;

  SaveIndex := SaveDC(Cnvs.Handle);
  try
  IntersectClipRect(Cnvs.Handle, R.Left, R.Top, R.Right, R.Bottom);

  FMargin := FItemMargin;
  if FSelectionStyle = scastGlow then
  begin
    InflateRect(R, -FSelectionGlow.GlowSize, -FSelectionGlow.GlowSize);
    if FMargin > 0 then
    begin
      FMargin := FMargin - FSelectionGlow.GlowSize + 2;
      if FMargin < 0 then FMargin := 0;
    end;
  end
  else
    InflateRect(R, -2, -2);

  with FItems[Index] do
  begin
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Cnvs, Index, FItems[Index].ItemRect)
    else
      begin
        FGlowAlpha := FSelectionGlow.AlphaValue;
        if not FShowFocusRect and not Self.Focused then
        begin
          if FGlowAlpha > 60 then
           FGlowAlpha := FGlowAlpha - 60;
        end;
       if ((SelectionStyle = scastGlow) or (SelectionStyle = scastCustomImageWithGlow)) and (FItems[Index].Active and Self.Enabled) then
        FItems[Index].FShowEllipses := DrawImageAndTextEllipsesWithGlow(Cnvs, R, FMargin, FItemSpacing,
            FItemLayout,
            Caption, FImageIndex, FImages,
            Self.Enabled, IsRightToLeft,
            (FItemLayout = blGlyphLeft) or (FItemLayout = blGlyphRight),
            FSelectionGlow.Offset, FSelectionGlow.Color,FSelectionGlow.GlowSize,
            FSelectionGlow.Intensive, FGlowAlpha, True, True, FScaleFactor)
         else
          FItems[Index].FShowEllipses := DrawImageAndTextEllipses(Cnvs, R, FMargin, FItemSpacing, FItemLayout,
             Caption, FImageIndex, FImages,
             Self.Enabled, IsRightToLeft, True, FScaleFactor);
      end;
   end;

  if (Self.ItemIndex = -1) and Focused and (Index = 0) then
  begin
    R := FItems[Index].ItemRect;
    if FShowFocusRect then scDrawFocusRect(Cnvs, R);
  end
  else
  if FItems[Index].Active and Focused then
  begin
    R := FItems[Index].ItemRect;
    if FShowFocusRect then
    begin
      if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
        and (FSelectionStyle = scastStyled) then
        InflateRect(R, -1, -1);
      scDrawFocusRect(Cnvs, R);
    end;
  end;

  finally
    RestoreDC(Cnvs.Handle, SaveIndex);
  end;
end;


procedure TscGallery.BeginUpdateItems;
begin
  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TscGallery.EndUpdateItems;
begin
  if Visible then
   SendMessage(Handle, WM_SETREDRAW, 1, 0);
  CalcItemRects;
  RePaintControl;
end;

procedure TscGallery.WMMouseHookCancelMode(var Message: TMessage);
begin
  if FPopupGallery.Visible
  then
    if (Message.wParam <> Handle) and
       (Message.wParam <> FPopupGallery.Handle) and
       not ((FPopupGallery.FVertScrollBar <>  nil) and
        (Message.wParam = FPopupGallery.FVertScrollBar.Handle))
    then
      HidePopupGallery;
end;

procedure TscGallery.CMCancelMode;
begin
  inherited;
  if FPopupGallery.Visible
  then
    if (Message.Sender = nil) or (
       (Message.Sender <> Self) and
       (Message.Sender <> Self.FPopupGallery) and
       (Message.Sender <> Self.FPopupGallery.FVertScrollBar))
    then
      HidePopupGallery;
end;

procedure TscGallery.ShowPopupGallery;
var
  P: TPoint;
  RCount: Integer;
begin
  P.X := 0;
  P.Y := 0;
  P := ClientToScreen(P);
  FPopupGallery.FScrollOffset := 0;
  FPopupGallery.Width := Self.Width;
  FPopupGallery.BalloonHint := BalloonHint;
  FPopupGallery.HintComponent := HintComponent;
  FPopupGallery.Wallpapers := Wallpapers;
  FPopupGallery.CustomImages := CustomImages;
  FPopupGallery.CustomBackgroundImageIndex := CustomBackgroundImageIndex;
  FPopupGallery.WallpaperIndex := WallpaperIndex;
  FPopupGallery.SelectionStyle := SelectionStyle;
  FPopupGallery.SelectionGlow.Assign(SelectionGlow);
  RCount := Items.Count div ColCount + 1;
  if RCount > FPopupMaxRowCount then RCount := FPopupMaxRowCount;
  FPopupGallery.Height := RCount * ItemHeight + Height - GetContentRect.Height;
  if FPopupGallery.Height < Height then FPopupGallery.Height := Height;
  FPopupGallery.CalcItemRects;
  FPopupGallery.ItemIndex := ItemIndex;
  FPopupGallery.Show(P);
  SC_HookMouseMessages(Self);
end;

procedure TscGallery.HidePopupGallery;
begin
  FPopupGallery.Hide;
end;

procedure TscGallery.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TestActive(-1);
  if FHintItemIndex <> -1 then
  begin
    FHintItemIndex := -1;
    if (FBalloonHint <> nil) and (FBalloonHint.ShowingHint) then
      FBalloonHint.HideHint;
     if FHintComponent <> nil then
      FHintComponent.HideHint;
  end;
end;

procedure TscGallery.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TscGallery.OnUpDownClick(Sender: TObject; Button: TUDBtnType);
var
  I: Integer;
begin
  CalcItemRects;
  if not Focused then SetFocus;
  if Button = btNext then
  begin
    I := FStartIndex - ItemsInPage;
    if I < 0 then I := 0;
    if I <> FStartIndex then
    begin
      FStartIndex := I;
      CalcItemRects;
      RePaintControl;
    end;
  end
  else
  begin
    if FStartIndex + ItemsInPage - 1 >= FItems.Count - 1 then Exit;
    I := FStartIndex + ItemsInPage;
    if I > FItems.Count - 1 then I := FItems.Count - 1;
    if I <> FStartIndex then
    begin
      FStartIndex := I;
      CalcItemRects;
      RePaintControl;
    end;
  end;
end;

procedure TscGallery.OnButtonClick(Sender: TObject);
begin
  if not Focused then SetFocus;
  ShowPopupGallery;
end;

procedure TscGallery.SetItemLayout(Value: TButtonLayout);
begin
  FItemLayout := Value;
  RePaintControl;
end;

procedure TscGallery.SetItemMargin(Value: Integer);
begin
  FItemMargin := Value;
  RePaintControl;
end;


procedure TscGallery.SetItemSpacing(Value: Integer);
begin
  FItemSpacing := Value;
  RePaintControl;
end;

procedure TscGallery.SetItemWidth(Value: Integer);
begin
  if FItemWidth <> Value
  then
    begin
      FItemWidth := Value;
      RePaintControl;
    end;
end;

procedure TscGallery.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value
  then
    begin
      FItemHeight := Value;
      RePaintControl;
    end;
end;

procedure TscGallery.SetItems(Value: TscGalleryItems);
begin
  FItems.Assign(Value);
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscGallery.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    RePaintControl;
  end;
end;

procedure TscGallery.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    FImages := nil;

  if (Operation = opRemove) and (AComponent = Wallpapers) and
     (FPopupGallery <> nil) and (FPopupGallery.Wallpapers = Wallpapers) then
    FPopupGallery.Wallpapers := nil;

  if (Operation = opRemove) and (AComponent = BalloonHint) and
    (FPopupGallery <> nil) and (FPopupGallery.BalloonHint = BalloonHint) then
     FPopupGallery.BalloonHint := nil;
end;

procedure TscGallery.CalcItemRects;
var
  I, J, K: Integer;
  X, Y: Integer;
begin
  if FItems.Count = 0
  then
    begin
      ItemsInPage := 0;
      RowCount := 1;
      ColCount := 1;
      UpDown.Enabled := False;
      DownButton.Enabled := UpDown.Enabled;
      FItemsRect := GetContentRect;
      Exit;
    end
  else
  begin
    UpDown.Min := -FItems.Count;
    UpDown.Max := FItems.Count;
  end;

  FItemsRect := GetContentRect;

  RowCount := FItemsRect.Height div FItemHeight;
  ColCount := FItemsRect.Width div FItemWidth;
  if RowCount = 0 then RowCount := 1;
  if ColCount = 0 then ColCount := 1;
  ItemsInPage := RowCount * ColCount;

  for I := 0 to FItems.Count - 1 do
    TscGalleryItem(FItems[I]).IsVisible := (I >= FStartIndex)
     and (I <= FStartIndex + ItemsInPage - 1);

  Y := FItemsRect.Top;
  k := FStartIndex;
  for I := 1 to RowCount do
  begin
    X := FItemsRect.Left;
    for J := 1 to ColCount do
    begin
      if k < FItems.Count then
      begin
        FItems[k].ItemRect := Rect(X, Y, X + FItemWidth, Y + FItemHeight);
        X := X + FItemWidth;
      end;
      Inc(k);
    end;
    Y := Y + FItemHeight;
  end;
  UpDown.Enabled := FItems.Count > ItemsInPage;
  DownButton.Enabled := UpDown.Enabled;
end;

procedure TscGallery.Scroll(AScrollOffset: Integer);
begin
end;

procedure TscGallery.GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
begin
end;

procedure TscGallery.WMSize(var Msg: TWMSize);
var
  OldItemsInPage: Integer;
  I: Integer;
begin
  inherited;
  OldItemsInPage := ItemsInPage;
  CalcItemRects;
  if (OldItemsInPage < ItemsInPage) and (FItems.Count <> 0)
  then
    begin
      I := FStartIndex - (ItemsInPage - OldItemsInPage);
      if I < 0 then I := 0;
      if I > FItems.Count - 1 then I := FItems.Count - 1;
      FStartIndex := I;
      CalcItemRects;
      RePaintControl;
    end;
  AdjustButtons;
end;

procedure TscGallery.ScrollToItem(Index: Integer);
var
  I: Integer;
begin
  if FItems.Count = 0 then Exit;
  if ItemsInPage = 0 then Exit;
  if Index = -1 then Exit;
  if Index < FStartIndex
  then
    begin
      I := FStartIndex;
      repeat
        I := I - ItemsInPage;
      until (I <= 0) or ((Index >= I) and (Index <= I + ItemsInPage - 1));
      if I >= 0
      then
        begin
         FStartIndex := I;
         CalcItemRects;
         RePaintControl;
       end;
    end
  else
  if Index > FStartIndex + ItemsInPage  - 1
  then
    begin
      I := FStartIndex;
      repeat
        I := I + ItemsInPage;
      until (I >= FItems.Count) or ((Index >= I) and (Index <= I + ItemsInPage - 1));
      if I <= FItems.Count - 1
      then
        begin
          FStartIndex := I;
          CalcItemRects;
          RePaintControl;
       end;
    end;
end;

procedure TscGallery.ShowUpDown;
begin
  if FScrollPanel = nil then
  begin
    FScrollPanel := TscPanel.Create(Self);
    FScrollPanel.Width := GetSystemMetrics(SM_CYHSCROLL);
    FScrollPanel.Visible := False;
    FScrollPanel.Parent := Self;

    DownButton := TscGalleryDropDownButton.Create(Self);
    DownButton.Visible := True;
    DownButton.Parent := FScrollPanel;
    DownButton.Height := GetSystemMetrics(SM_CYHSCROLL) + 5;
    DownButton.CanFocused := False;
    DownButton.Caption := '';
    DownButton.OnClick := OnButtonClick;
    DownButton.Align := alBottom;

    UpDown := TscUpDown.Create(Self);
    UpDown.Visible := True;
    UpDown.Parent := FScrollPanel;
    UpDown.Orientation := udVertical;
    UpDown.OnClick := OnUpDownClick;
    UpDown.Align := alClient;

    AdjustButtons;
    FScrollPanel.Visible := True;
  end;
end;

procedure TscGallery.HideUpDown;
begin
  FScrollPanel.Free;
  FScrollPanel := nil;
  UpDown := nil;
  DownButton := nil;
end;

procedure TscGallery.UpdateScrollInfo;
begin
end;

procedure TscGallery.AdjustButtons;
var
  R: TRect;
  PW: Integer;
begin
  if FScrollPanel = nil then Exit;
  R := GetContentRect;
  PW := GetSystemMetrics(SM_CYHSCROLL);
  if Bidimode = bdRightToLeft then
  begin
    R.Right := R.Left;
    Dec(R.Left, PW);
  end
  else
  begin
    R.Left := R.Right;
    Inc(R.Right, PW);
  end;
  FScrollPanel.SetBounds(R.Left, R.Top, PW, R.Height);
end;

procedure TscGallery.SetItemIndex(Value: Integer);
var
  I: Integer;
  IsFind: Boolean;
begin
  if Value < 0
  then
    begin
      if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
        FItems[FItemIndex].Active := False;
      FItemIndex := Value;
      RePaintControl;
    end
  else
    begin
      FItemIndex := Value;
      IsFind := False;
      for I := 0 to FItems.Count - 1 do
        with FItems[I] do
        begin
          if I = FItemIndex
          then
            begin
              Active := True;
              IsFind := True;
            end
          else
             Active := False;
        end;
      RePaintControl;
      ScrollToItem(FItemIndex);
      if IsFind and not (csDesigning in ComponentState) and not (csLoading in ComponentState)
      then
      begin
        if Assigned(FItems[FItemIndex].OnClick) then
          FItems[FItemIndex].OnClick(Self);
        if Assigned(FOnItemClick) then
          FOnItemClick(Self);
      end;
    end;
end;

procedure TscGallery.Loaded;
begin
  inherited;
  if FItemIndex <> -1 then ScrollToItem(FItemIndex);
end;

function TscGallery.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if PtInRect(FItems[I].ItemRect, Point(X, Y)) and (FItems[I].IsVisible)
    then
      begin
        Result := I;
        Break;
      end;
end;


procedure TscGallery.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  if (I <> -1) and (Button = mbLeft)
  then
    begin
      SetItemActive(I);
      FMouseDown := True;
      FMouseActive := I;
    end;
end;

procedure TscGallery.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  FMouseDown := False;
  I := ItemAtPos(X, Y);
  if (I <> -1) and  (Button = mbLeft) then ItemIndex := I;
end;

procedure TscGallery.TestActive(MouseInIndex: Integer);
begin
  if (MouseInIndex = -1) and (FOldMouseInItem <> -1)
  then
    begin
      FItems[FOldMouseInItem].MouseIn := False;
      FOldMouseInItem := -1;
      FMouseInItem := -1;
      RePaintControl;
    end
  else
  if (MouseInIndex <> -1) and (MouseInIndex <> FMouseInItem)
  then
    begin
      FMouseInItem := MouseInIndex;
      if FOldMouseInItem <> - 1 then FItems[FOldMouseInItem].MouseIn := False;
      FItems[FMouseInItem].MouseIn := True;
      FOldMouseInItem := FMouseInItem;
      RePaintControl;
    end;
end;

procedure TscGallery.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  TestActive(I);
  if (I <> -1) and FMouseDown and (I <> FMouseActive)
  then
    begin
      SetItemActive(I);
      FMouseActive := I;
    end;
  if (FBalloonHint <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      if FItems[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        ShowBalloonHint(FItems[I].Caption, FItems[I].ItemRect);
      end
      else
      begin
        FHintItemIndex := -1;
        if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      end;
    end;
    if (FHintComponent <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if FItems[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        FHintComponent.ActivateHint(FItems[I].Caption);
      end
      else
      begin
        FHintItemIndex := -1;
        FHintComponent.HideHint;
      end;
    end;
    if (I = -1) and (FBalloonHint <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
    end;
    if (I = -1) and (FHintComponent <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      FHintComponent.HideHint;
    end;
end;

procedure TscGallery.SetItemActive(Value: Integer);
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

procedure TscGallery.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FPopupGallery.Visible
  then
    begin
      SendMessage(FPopupGallery.Handle, WM_MOUSEWHEEL, Message.WParam, Message.LParam)
    end
  else
    if TWMMOUSEWHEEL(Message).WheelDelta > 0
    then OnUpDownClick(Self, btNext)
    else OnUpDownClick(Self, btPrev);
end;

procedure TscGallery.WMSETFOCUS(var Message: TWMSETFOCUS);
begin
  inherited;
  FUpdateParentBuffer := True;
  RePaint;
end;

procedure TscGallery.WMKILLFOCUS(var Message: TWMKILLFOCUS);
begin
  inherited;
  if FPopupGallery.Visible then HidePopupGallery;
  RePaint;
end;

procedure TscGallery.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and not Focused then
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

procedure TscGallery.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscGallery.FindLeft;
var
  I: Integer;
begin
  if ItemIndex <= -1 then Exit;
  I := FItemIndex - 1;
  if (ItemIndex < FStartIndex) or (ItemIndex > FStartIndex + ItemsInPage - 1)
  then
    begin
      I := FStartIndex + ItemsInPage - 1;
      if I > FItems.Count - 1 then I := FItems.Count - 1;
    end;
  if I >= 0 then ItemIndex := I;
end;

procedure TscGallery.FindRight;
var
  I: Integer;
begin
  if ItemIndex <= -1 then I := 0 else I := FItemIndex + 1;
  if (ItemIndex < FStartIndex) or (ItemIndex > FStartIndex + ItemsInPage - 1) then I := FStartIndex;
  if I <= FItems.Count - 1 then ItemIndex := I;
end;

procedure TscGallery.FindDown;
var
  i: Integer;
begin
  i := ItemIndex + ColCount;
  if (ItemIndex < FStartIndex) or (ItemIndex > FStartIndex + ItemsInPage - 1) then I := FStartIndex;
  if i > FItems.Count - 1 then i := FItems.Count - 1;
  if i <> ItemIndex then ItemIndex := i;
end;

procedure TscGallery.FindUp;
var
  i: Integer;
begin
  i := ItemIndex - ColCount;
  if (ItemIndex < FStartIndex) or (ItemIndex > FStartIndex + ItemsInPage - 1)
  then
    begin
      I := FStartIndex + ItemsInPage - 1;
      if I > FItems.Count - 1 then I := FItems.Count - 1;
    end;
  if i < 0 then i := 0;
  if i <> ItemIndex then ItemIndex := i;
end;

procedure TscGallery.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited KeyDown(Key, Shift);
 if FPopupGallery.Visible
 then
   begin
     FPopupGallery.KeyDown(Key, Shift);
   end
  else
    case Key of
      VK_NEXT: OnUpDownClick(Self, btNext);
      VK_PRIOR: OnUpDownClick(Self, btPrev);
      VK_UP: FindUp;
      VK_LEFT: FindLeft;
      VK_DOWN: if (ssAlt in Shift) then ShowPopupGallery else FindDown;
      VK_RIGHT: if (ssAlt in Shift) then ShowPopupGallery else FindRight;
    end;
end;

constructor TscPopupGallery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  DrawOnBackground := False;
  TabStop := False;
  FTimerMode := 0;
  Visible := False;
  FInUpdateItems := False;
  FScrollOffset := 0;
  FGallery := TscGallery(AOwner);
  FMouseInItem := -1;
  FOldMouseInItem := -1;
  FStartIndex := 0;
  FMouseDown := False;
  FMouseActive := -1;
  Width := 250;
  Height := 100;
  FMax := 0;
  FRealMax := 0;
  FOldWidth := -1;
  FItemIndex := -1;
end;

destructor TscPopupGallery.Destroy;
begin
  inherited Destroy;
end;

procedure TscPopupGallery.EnableScrollTimer(Value: Integer);
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

procedure TscPopupGallery.StopScrollTimer;
begin
  if FTimerMode <> 0 then
  begin
    FTimerMode := 0;
    KillTimer(Handle, 2);
  end;
end;

procedure TscPopupGallery.WMTimer;
begin
  inherited;
  if Message.TimerID = 2 then
  begin
    case FTimerMode of
      1: FindUp;
      2: FindDown;
    end;
  end;
end;

procedure TscPopupGallery.DrawBorder(ACanvas: TCanvas; ARect: TRect);
begin
  DrawSingleBorder(ACanvas, ARect);
end;

function TscPopupGallery.CanDrawContent: Boolean;
begin
  Result := Assigned(Items) and (Items.Count > 0);
end;

procedure TscPopupGallery.DrawContent(ACanvas: TCanvas; ARect: TRect);
var
  I, SaveIndex: Integer;
begin
  CalcItemRects;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,
      FItemsRect.Left, FItemsRect.Top, FItemsRect.Right, FItemsRect.Bottom);
    for I := 0 to Items.Count - 1 do
      if Items[I].PopupIsVisible then DrawItem(I, ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscPopupGallery.DrawItem(Index: Integer; Cnvs: TCanvas);
var
  R: TRect;
  FC: TColor;
  SaveIndex: Integer;
  FMargin: Integer;
begin
  if (seFont in StyleElements) and IsCustomStyle then
    FC := GetEditTextColor(scsNormal)
  else
    FC := Self.Font.Color;

  if (Items[Index].PopupActive or (Items[Index].PopupMouseIn))  and (SelectionStyle <> scastGlow) then
  begin
    R := Items[Index].PopupItemRect;
    if ((FGallery.SelectionStyle = scastCustomImage) or (FGallery.SelectionStyle = scastCustomImageWithGlow)) then
    begin
      if (FGallery.FCustomImages <> nil) and FGallery.FCustomImages.IsIndexAvailable(FGallery.FCustomSelectionImageIndex) and
          FGallery.FCustomImages.IsIndexAvailable(FGallery.FCustomFocusedSelectionImageIndex)
      then
      begin
        if Items[Index].Active then
          FGallery.FCustomImages.Draw(Cnvs, R, FGallery.FCustomFocusedSelectionImageIndex, FScaleFactor)
        else
          FGallery.FCustomImages.Draw(Cnvs, R, FGallery.FCustomSelectionImageIndex, FScaleFactor);
        FC := GetStyleColor(FGallery.FSelectionTextColor);
      end;
    end
    else
    if FGallery.SelectionStyle = scastStyled then
    begin
      R := Items[Index].PopupItemRect;
      if Items[Index].Active then
        DrawSelection(Cnvs, R, True, False)
      else
        DrawSelection(Cnvs, R, False, False);
      FC := GetSelectionTextColor;
    end
    else
    if FGallery.SelectionStyle = scastColor then
    begin
      if FGallery.SelectionColor <> clNone then
        Cnvs.Brush.Color := FGallery.SelectionColor
      else
        Cnvs.Brush.Color := GetStyleColor(clHighLight);
      if Items[Index].Active then
        Cnvs.FillRect(Items[Index].PopupItemRect)
      else
        FillRectWithAlpha(Cnvs, Items[Index].PopupItemRect, 200);
      if FGallery.SelectionColor <> clNone then
        FC := FGallery.SelectionTextColor
      else
      if IsCustomStyle then
        FC := GetSelectionTextColor
      else
        FC := clHighLightText;
    end
  end
  else
  begin
    if IsCustomStyle and (seFont in FGallery.StyleElements) then
    begin
      if BackgroundStyle = scbgsColor then
        FC := GetEditTextColor(scsNormal)
      else
        FC := GetCheckBoxTextColor(scsNormal)
    end
    else
      FC := Font.Color;
  end;

  Cnvs.Font := Self.Font;
  Cnvs.Font.Color := FC;
  Cnvs.Brush.Style := bsClear;

  R := Items[Index].PopupItemRect;

  SaveIndex := SaveDC(Cnvs.Handle);
  try
  IntersectClipRect(Cnvs.Handle, R.Left, R.Top, R.Right, R.Bottom);

  FMargin := FGallery.ItemMargin;
  if FGallery.SelectionStyle = scastGlow then
  begin
    InflateRect(R, -FGallery.SelectionGlow.GlowSize, -FGallery.SelectionGlow.GlowSize);
    if FMargin > 0 then
    begin
      FMargin := FMargin - FGallery.SelectionGlow.GlowSize + 2;
      if FMargin < 0 then FMargin := 0;
    end;
  end
  else
    InflateRect(R, -2, -2);

  with Items[Index] do
  begin
    if Assigned(Self.FGallery.OnDrawItem) then
      Self.FGallery.OnDrawItem(Cnvs, Index, Items[Index].PopupItemRect)
    else
      begin
         if ((SelectionStyle = scastGlow) or (SelectionStyle = scastCustomImageWithGlow)) and (Items[Index].PopupActive or (Items[Index].PopupMouseIn)) then
          Items[Index].FShowEllipses :=  DrawImageAndTextEllipsesWithGlow(Cnvs, R, FMargin, FGallery.ItemSpacing,
             FGallery.ItemLayout,
             Caption, FImageIndex, FGallery.Images,
             True, FGallery.IsRightToLeft,
             (FGallery.ItemLayout = blGlyphLeft) or (FGallery.ItemLayout = blGlyphRight),
             FSelectionGlow.Offset, FSelectionGlow.Color,FSelectionGlow.GlowSize,
             FSelectionGlow.Intensive,  FGallery.SelectionGlow.AlphaValue, True, True,
             FScaleFactor)
         else
          Items[Index].FShowEllipses := DrawImageAndTextEllipses(Cnvs, R, FMargin, FGallery.ItemSpacing, FGallery.ItemLayout,
             Caption, FImageIndex, Fgallery.Images,
               True, FGallery.IsRightToLeft, True, FScaleFactor);
      end;
   end;
  finally
    RestoreDC(Cnvs.Handle, SaveIndex);
  end;
end;


procedure TscPopupGallery.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscPopupGallery.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscPopupGallery.Hide;
begin
  SC_UnHookMouseMessages;
  FMouseActive := -1;
  if FHintItemIndex <> -1 then
  begin
    FHintItemIndex := -1;
    if (FBalloonHint <> nil) and (FBalloonHint.ShowingHint) then
      FBalloonHint.HideHint;
    if FHintComponent <> nil then
      FHintComponent.HideHint;
  end;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TscPopupGallery.Show(Origin: TPoint);
begin
  if not IsCustomStyle and StyleServices.Enabled and
    (FVertScrollBar <> nil) and (FVertScrollBar.Visible) then
  begin
    FVertScrollBar.Perform(CM_RECREATEWND, 0, 0);
  end;
  FMouseActive := -1;
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  UpdateScrollInfo;
  Visible := True;
end;

function TscPopupGallery.GetItems;
begin
  Result := FGallery.Items;
end;

procedure TscPopupGallery.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TestActive(-1);
  if FHintItemIndex <> -1 then
  begin
    FHintItemIndex := -1;
    if (FBalloonHint <> nil) and (FBalloonHint.ShowingHint) then
      FBalloonHint.HideHint;
    if FHintComponent <> nil then
      FHintComponent.HideHint;
  end;
end;

procedure TscPopupGallery.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TscPopupGallery.OnVertScrollBarChange(Sender: TObject);
begin
  inherited;
   if (FVertScrollBar <> nil) and FVertScrollBar.HandleAllocated then
  begin
   if FVertScrollBar.Position <= FVertScrollBar.Max - FVertScrollBar.PageSize + 1  then
     Scroll(FVertScrollBar.Position)
   else
     Scroll(FVertScrollBar.Max - FVertScrollBar.PageSize + 1);
  end;
end;

procedure TscPopupGallery.CalcItemRects;
var
  I, Col: Integer;
  X, Y: Integer;
begin
  FItemsRect := GetContentRect;
  RowCount := FItemsRect.Height div FGallery.ItemHeight;
  ColCount := FItemsRect.Width div FGallery.ItemWidth;
  if (Items.Count div ColCount) = (Items.Count / ColCount) then
    RowCount := Items.Count div ColCount;
  if RowCount = 0 then RowCount := 1;
  if ColCount = 0 then ColCount := 1;
  ItemsInPage := RowCount * ColCount;

  FRealMax := 0;
  Y := FItemsRect.Top;
  X := FItemsRect.Left;
  Col := 0;
  for i := 0 to Items.Count - 1 do
  begin
    if Col = ColCount
    then
      begin
        X := FItemsRect.Left;
        Y := Y + FGallery.ItemHeight;
        Col := 0;
      end;

    Items[i].PopupItemRect := Rect(X, Y, X + FGallery.ItemWidth, Y + FGallery.ItemHeight);

    OffsetRect(Items[i].PopupItemRect, 0, - FScrollOffset);
    Items[i].PopupIsVisible := RectToRect(Items[i].PopupItemRect, FItemsRect);
    if not Items[i].PopupIsVisible and (Items[i].PopupItemRect.Top <= FItemsRect.Top) and
       (Items[i].PopupItemRect.Bottom >= FItemsRect.Bottom)
    then
      Items[i].PopupIsVisible := True;
    if Items[i].PopupIsVisible then FRealMax := Items[i].PopupItemRect.Bottom;

    X := X + FGallery.ItemWidth;
    Inc(Col);
  end;
  FMax := Y + FGallery.ItemHeight;
end;

procedure TscPopupGallery.Scroll(AScrollOffset: Integer);
begin
  FScrollOffset := AScrollOffset;
  RePaintControl;
  UpdateScrollInfo;
end;

procedure TscPopupGallery.GetScrollInfo(var AMin, AMax, APage, APosition: Integer);
begin
  CalcItemRects;
  AMin := 0;
  AMax := FMax - FItemsRect.Top;
  APage := FItemsRect.Height;
  if AMax <= APage
  then
    begin
      APage := 0;
      AMax := 0;
    end;
  APosition := FScrollOffset;
end;

procedure TscPopupGallery.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateScrollInfo;
end;

procedure TscPopupGallery.ScrollToItem(Index: Integer);
var
  R, R1: TRect;
begin
  CalcItemRects;
  R1 := Items[Index].PopupItemRect;
  R := R1;
  OffsetRect(R, 0, FScrollOffset);
  if (R1.Top <= FItemsRect.Top)
  then
    begin
      if Index = 0
      then
        FScrollOffset := 0
      else
        FScrollOffset := R.Top - FItemsRect.Top;
      CalcItemRects;
      RePaintControl;
    end
  else
  if R1.Bottom >= FItemsRect.Bottom
  then
    begin
      FScrollOffset := R.Top;
      FScrollOffset := FScrollOffset - FItemsRect.Height + R.Height -
        Height + FItemsRect.Bottom;
      CalcItemRects;
      RePaintControl;
    end;
  UpdateScrollInfo;
end;

procedure TscPopupGallery.UpdateScrollInfo;
var
  SMin, SMax, SPage, SPos: Integer;
begin
  if not HandleAllocated then Exit;

  if FInUpdateItems then Exit;
  GetScrollInfo(SMin, SMax, SPage, SPos);
  if SMax <> 0
  then
    begin
      ShowVertScrollBar;
      UpdateVertScrollBar(SMin, SMax, SPos, SPage);
      if FVertScrollBar <> nil then
      begin
        FVertScrollBar.LargeChange := SPage;
        FVertScrollBar.SmallChange := Self.FGallery.ItemHeight;
      end;
    end
  else
    if (SMax = 0) and (FVertScrollBar <> nil) then
    begin
      HideVertScrollBar;
      RePaintControl;
    end;
end;

procedure TscPopupGallery.SetItemIndex(Value: Integer);
var
  I: Integer;
begin
  if Value < 0
  then
    begin
      if (FItemIndex >= 0) and (FItemIndex < Items.Count) then
        Items[FItemIndex].PopupActive := False;
      FItemIndex := Value;
      RePaintControl;
    end
  else
    begin
      FItemIndex := Value;
      for I := 0 to Items.Count - 1 do
        with Items[I] do
        begin
          if I = FItemIndex
          then
            PopupActive := True
          else
            PopupActive := False;
        end;
      RePaintControl;
      ScrollToItem(FItemIndex);
    end;
end;

function TscPopupGallery.ItemAtPos(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
    if PtInRect(Items[I].PopupItemRect, Point(X, Y)) and (Items[I].PopupIsVisible)
    then
      begin
        Result := I;
        Break;
      end;
end;


procedure TscPopupGallery.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  I := ItemAtPos(X, Y);
  if (I <> -1) and (Button = mbLeft)
  then
    begin
      SetItemActive(I);
      FMouseDown := True;
      FMouseActive := I;
    end;
end;

procedure TscPopupGallery.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  StopScrollTimer;
  FMouseDown := False;
  I := ItemAtPos(X, Y);
  if (I <> -1) and  (Button = mbLeft)
  then
    begin
      ItemIndex := I;
      Hide;
      if FGallery.ItemIndex <> ItemIndex then FGallery.ItemIndex := ItemIndex;
    end;
end;

procedure TscPopupGallery.TestActive(MouseInIndex: Integer);
begin
  if (MouseInIndex = -1) and (FOldMouseInItem <> -1)
  then
    begin
      Items[FOldMouseInItem].PopupMouseIn := False;
      FOldMouseInItem := -1;
      FMouseInItem := -1;
      RePaintControl;
    end
  else
  if (MouseInIndex <> -1) and (MouseInIndex <> FMouseInItem)
  then
    begin
      FMouseInItem := MouseInIndex;
      if FOldMouseInItem <> - 1 then Items[FOldMouseInItem].PopupMouseIn := False;
      Items[FMouseInItem].PopupMouseIn := True;
      FOldMouseInItem := FMouseInItem;
      RePaintControl;
    end;
end;

procedure TscPopupGallery.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  R: TRect;
begin
  inherited;
  if Items.Count = 0 then Exit;
  R := GetContentRect;
  if PtInRect(R, Point(X, Y)) then
  begin
    StopScrollTimer;
    I := ItemAtPos(X, Y);
    TestActive(I);
    if (I <> -1) and FMouseDown and (I <> FMouseActive) then
    begin
      SetItemActive(I);
      FMouseActive := I;
    end;
    if (FBalloonHint <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      if Items[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        ShowBalloonHint(Items[I].Caption, Items[I].PopupItemRect);
      end
      else
      begin
        FHintItemIndex := -1;
        if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
      end;
    end;
    if (FHintComponent <> nil) and (I <> -1) and
       (I <> FHintItemIndex) then
    begin
      if Items[I].FShowEllipses and not FMouseDown then
      begin
        FHintItemIndex := I;
        FHintComponent.ActivateHint(Items[I].Caption);
      end
      else
      begin
        FHintItemIndex := -1;
        FHintComponent.HideHint;
      end;
    end;
    if (I = -1) and (FBalloonHint <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      if FBalloonHint.ShowingHint then FBalloonHint.HideHint;
    end;
    if (I = -1) and (FHintComponent <> nil) and (FHintItemIndex <> -1) then
    begin
      FHintItemIndex := -1;
      FHintComponent.HideHint;
    end;
  end
  else
  if FMouseDown then
  begin
    if Y < R.Top then
      EnableScrollTimer(1)
    else
      EnableScrollTimer(2);
  end;
end;

procedure TscPopupGallery.SetItemActive(Value: Integer);
var
  I: Integer;
begin
  FItemIndex := Value;
  for I := 0 to Items.Count - 1 do
  with Items[I] do
   if I = Value then PopupActive := True else PopupActive := False;
  RePaintControl;
  ScrollToItem(Value);
end;

procedure TscPopupGallery.FindPageUp;
begin
  if FVertScrollBar <> nil
  then
    FVertScrollBar.Position := FVertScrollBar.Position - FVertScrollBar.SmallChange;
end;

procedure TscPopupGallery.FindPageDown;
begin
  if FVertScrollBar <> nil
  then
    FVertScrollBar.Position := FVertScrollBar.Position + FVertScrollBar.SmallChange;
end;

procedure TscPopupGallery.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FVertScrollBar <> nil
  then
    begin
      if TWMMOUSEWHEEL(Message).WheelDelta > 0
      then
        FVertScrollBar.Position := FVertScrollBar.Position - FVertScrollBar.SmallChange
      else
        FVertScrollBar.Position := FVertScrollBar.Position + FVertScrollBar.SmallChange;
    end;
end;

procedure TscPopupGallery.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
end;

procedure TscPopupGallery.FindLeft;
var
  I: Integer;
begin
  if ItemIndex <= -1 then Exit;
  I := FItemIndex - 1;
  if I >= 0 then ItemIndex := I;
end;

procedure TscPopupGallery.FindRight;
var
  I: Integer;
begin
  if ItemIndex <= -1 then I := 0 else I := FItemIndex + 1;
  if I <= Items.Count - 1 then ItemIndex := I;
end;

procedure TscPopupGallery.FindDown;
var
  i: Integer;
begin
  i := ItemIndex + ColCount;
  if i > Items.Count - 1 then i := Items.Count - 1;
  if i <> ItemIndex then ItemIndex := i;
end;

procedure TscPopupGallery.FindUp;
var
  i: Integer;
begin
  i := ItemIndex - ColCount;
  if i < 0 then i := 0;
  if i <> ItemIndex then ItemIndex := i;
end;

procedure TscPopupGallery.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_NEXT: FindPageDown;
    VK_PRIOR: FindPageUp;
    VK_UP: FindUp;
    VK_LEFT: FindLeft;
    VK_DOWN: FindDown;
    VK_RIGHT: FindRight;
    VK_RETURN, VK_SPACE:
      begin
        Hide;
        if FGallery.ItemIndex <> ItemIndex
        then
          FGallery.ItemIndex := ItemIndex;
      end;
    VK_ESCAPE:  Hide;
  end;
end;

type
  TscEditButtonClass = class(TscEditButton);

constructor TscAdvancedComboEdit.Create;
begin
  inherited Create(AOwner);
  FIsModified := False;
  FMouseWheelSupport := True;
  FDropDownPosition := scdpRight;
  RightButton.Visible := True;
  RightButton.ComboButton := True;
  FUseFilter := False;
  FStopUseFilter := False;
  OnRightButtonClick := RightButtonClick;
  RightButton.Width := GetSystemMetrics(SM_CYHSCROLL);
  FListBox := TscAdvancedPopupListBox.Create(Self);
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

procedure TscAdvancedComboEdit.BeginUpdateItems;
begin
  FListBox.BeginUpdateItems;
end;

procedure TscAdvancedComboEdit.EndUpdateItems;
begin
  FListBox.EndUpdateItems;
end;

procedure TscAdvancedComboEdit.Add(const Item: String);
begin
  FListBox.Add(Item);
end;

procedure TscAdvancedComboEdit.Add(Items: TStrings);
begin
  FListBox.Add(Items);
end;

procedure TscAdvancedComboEdit.Delete(Index: Integer);
begin
  FListBox.Delete(Index);
end;

function TscAdvancedComboEdit.IsModified: Boolean;
begin
  Result := Modified or FIsModified;
end;

procedure TscAdvancedComboEdit.Sort;
begin
  FListBox.Sort;
end;

function TscAdvancedComboEdit.IndexOf(const S: string; AStartOff: Boolean = False): Integer;
begin
  Result := FListBox.IndexOfCaption(S, AStartOff);
end;

procedure TscAdvancedComboEdit.ListBoxWindowProcHook(var Message: TMessage);
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

procedure TscAdvancedComboEdit.KeyPress;
begin
  inherited;
end;

destructor TscAdvancedComboEdit.Destroy;
begin
  FlistBox.Free;
  FlistBox := nil;
  inherited;
end;

procedure TscAdvancedComboEdit.CMEnabledChanged;
begin
  inherited;
  Invalidate;
end;

function TscAdvancedComboEdit.GetAlternateRow: Boolean;
begin
  Result := FListBox.AlternateRow;
end;

procedure TscAdvancedComboEdit.SetAlternateRow(Value: Boolean);
begin
  FListBox.AlternateRow := Value;
end;

procedure TscAdvancedComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
  if (Operation = opRemove) and (AComponent = FListBoxWallpapers) then
   FListBoxWallpapers := nil;
end;

function TscAdvancedComboEdit.GetImages: TCustomImageList;
begin
  if FListBox <> nil
  then
    Result := FListBox.Images
   else
    Result := nil;
end;

procedure TscAdvancedComboEdit.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (Message.wParam <> FListBox.Handle) and
      not ((FListBox.FVertScrollBar <>  nil) and
        (Message.wParam = FListBox.FVertScrollBar.Handle))
  then
    CloseUp(False);
end;

procedure TscAdvancedComboEdit.CMCancelMode;
begin
  inherited;
  if (Message.Sender = nil) or (
     (Message.Sender <> Self.FListBox) and
     (Message.Sender <> Self.FListBox.FVertScrollBar))
  then
    CloseUp(False);
end;

procedure TscAdvancedComboEdit.CheckButtonClick;
begin
  CloseUp(True);
end;

procedure TscAdvancedComboEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  case Msg.CharCode of
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:  Msg.Result := 1;
  end;
end;

procedure TscAdvancedComboEdit.KeyDown;
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

procedure TscAdvancedComboEdit.MouseDown;
begin
  inherited;
  if FListBox.Visible then CloseUp(False);
end;

procedure TscAdvancedComboEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
  P: TPoint;
begin
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

procedure TscAdvancedComboEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FListBox.Visible then ProcessListBox;
end;

procedure TscAdvancedComboEdit.WMMOUSEWHEEL;
begin
  inherited;
  if FMouseWheelSupport then
    if TWMMOUSEWHEEL(Message).WheelDelta > 0
    then
      ComboKeyUp(not FListBox.Visible)
    else
      ComboKeyDown(not FListBox.Visible);
end;

procedure TscAdvancedComboEdit.WMSETFOCUS;
begin
  inherited;
  Invalidate;
end;

procedure TscAdvancedComboEdit.WMKILLFOCUS;
begin
  inherited;
  if FListBox.Visible  then CloseUp(False);
  Invalidate;
end;

function TscAdvancedComboEdit.GetItemIndex;
begin
  Result := FListBox.ItemIndex;
end;

procedure TscAdvancedComboEdit.SetItemIndex;
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

function TscAdvancedComboEdit.IsPopupVisible: Boolean;
begin
  Result := FListBox.Visible;
end;

procedure TscAdvancedComboEdit.StartTimer;
begin
  KillTimer(Handle, 2);
  SetTimer(Handle, 2, 25, nil);
end;

procedure TscAdvancedComboEdit.StopTimer;
begin
  KillTimer(Handle, 2);
  TimerMode := 0;
end;

procedure TscAdvancedComboEdit.WMTimer;
begin
  inherited;
  case TimerMode of
    1: FListBox.FindUp;
    2: FListBox.FindDown;
  end;
end;

procedure TscAdvancedComboEdit.ProcessListBox;
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := FListBox.ScreenToClient(P);
  if (P.Y < 0) and (FListBox.FVertScrollBar <> nil) and WasInLB and FLBDown
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
  if (P.Y > FListBox.Height) and (FListBox.FVertScrollBar <> nil) and WasInLB and FLBDown
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

procedure TscAdvancedComboEdit.SetDropDownCount(Value: Integer);
begin
  if Value >= 0
  then
    FDropDownCount := Value;
end;

procedure TscAdvancedComboEdit.SetItems;
begin
  FListBox.Items.Assign(Value);
end;

function TscAdvancedComboEdit.GetItems;
begin
  Result := FListBox.Items;
end;

procedure TscAdvancedComboEdit.CloseUp;
begin
  SC_UnHookMouseMessages;
  if TimerMode <> 0 then StopTimer;
  if not FListBox.Visible then Exit;
  FListBox.Hide;
  TscEditButtonClass(RightButton).FDropDown := False;
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
  UpdateComboButton;
  if Value then
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  FIsModified := False;
end;

procedure TscAdvancedComboEdit.RightButtonClick(Sender: TObject);
begin
  if not FListBox.Visible then DropDown else CloseUp(False);
end;

procedure TscAdvancedComboEdit.Change;
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

procedure TscAdvancedComboEdit.DropDown;
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

  FListBox.OnDrawItem := Self.OnDrawItem;
  FListBox.Font.Assign(Font);
  FListBox.Color := Color;
  FListBox.BidiMode := Self.BiDiMode;
  FListBox.StyleElements := StyleElements;
  FListBox.Wallpapers := ListBoxWallpapers;
  FListBox.WallpaperIndex := ListBoxWallpaperIndex;

  if FDropDownPosition = scdpRight then
    P := Point(Left + Width - FListBox.Width, Top + Height)
  else
    P := Point(Left, Top + Height);

  P := Parent.ClientToScreen(P);

  WorkArea := Screen.MonitorFromWindow(Handle).WorkAreaRect;

  FOldItemIndex := FListBox.ItemIndex;

  TscEditButtonClass(RightButton).FDropDown := True;
  Invalidate;
  UpdateComboButton;
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

procedure TscAdvancedComboEdit.ComboPageUp;
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

procedure TscAdvancedComboEdit.ComboPageDown(AChange: Boolean);
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

procedure TscAdvancedComboEdit.ComboKeyUp;
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

procedure TscAdvancedComboEdit.ComboKeyDown;
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

procedure TscAdvancedComboEdit.SetImages(Value: TCustomImageList);
begin
  if FListBox.Images <> Value then
  begin
    FListBox.Images := Value;
    Invalidate;
  end;
end;

function TscAdvancedComboEdit.GetHeaderFont: TFont;
begin
  Result := FListBox.HeaderFont;
end;

procedure TscAdvancedComboEdit.SetHeaderFont(Value: TFont);
begin
  FListBox.HeaderFont.Assign(Value);
end;

function TscAdvancedComboEdit.GetDetailFont: TFont;
begin
  Result := FListBox.DetailFont;
end;

procedure TscAdvancedComboEdit.SetDetailFont(Value: TFont);
begin
  FListBox.DetailFont.Assign(Value);
end;

function TscAdvancedComboEdit.GetTitleFont: TFont;
begin
  Result := FListBox.TitleFont;
end;

procedure TscAdvancedComboEdit.SetTitleFont(Value: TFont);
begin
  FListBox.TitleFont.Assign(Value);
end;

procedure TscAdvancedComboEdit.SetListBoxWallpapers(Value: TscCustomImageCollection);
begin
  FListBoxWallpapers := Value;
end;

procedure TscAdvancedComboEdit.SetListBoxWallpaperIndex(Value: Integer);
begin
  FListBoxWallpaperIndex := Value;
end;

function TscAdvancedComboEdit.GetListBoxDetailPosition: TscListBoxDetailPosition;
begin
  Result := FListBox.DetailPosition;
end;

procedure TscAdvancedComboEdit.SetListBoxDetailPosition(Value: TscListBoxDetailPosition);
begin
  FListBox.DetailPosition := Value;
end;

function TscAdvancedComboEdit.GetListBoxDetailWordWrap: Boolean;
begin
  Result := FListBox.DetailWordWrap;
end;

procedure TscAdvancedComboEdit.SetListBoxDetailWordWrap(Value: Boolean);
begin
  FListBox.DetailWordWrap := Value;
end;

function TscAdvancedComboEdit.GetListBoxIndentMargin: Integer;
begin
  Result := FListBox.IndentMargin;
end;

procedure TscAdvancedComboEdit.SetListBoxIndentMargin(Value: Integer);
begin
  FListBox.IndentMargin := Value;
end;

function TscAdvancedComboEdit.GetListBoxSelectionColor: TColor;
begin
  Result := FListBox.SelectionColor;
end;

procedure TscAdvancedComboEdit.SetListBoxSelectionColor(Value: TColor);
begin
  FListBox.SelectionColor := Value;
end;

function TscAdvancedComboEdit.GetListBoxSelectionTextColor: TColor;
begin
  Result := FListBox.SelectionTextColor;
end;

procedure TscAdvancedComboEdit.SetListBoxSelectionTextColor(Value: TColor);
begin
  FListBox.SelectionTextColor := Value;
end;

function TscAdvancedComboEdit.GetListBoxHeaderUseStyleColor: Boolean;
begin
  Result := FListBox.HeaderUseStyleColor;
end;

procedure TscAdvancedComboEdit.SetListBoxHeaderUseStyleColor(Value: Boolean);
begin
  FListBox.HeaderUseStyleColor := Value;
end;

function TscAdvancedComboEdit.GetListBoxLineColor: TColor;
begin
  Result := FListBox.LineColor;
end;

procedure TscAdvancedComboEdit.SetListBoxLineColor(Value: TColor);
begin
  FListBox.LineColor := Value;
end;

function TscAdvancedComboEdit.GetListBoxHeaderStyle: TscAdvancedHeaderStyle;
begin
  Result := FListBox.HeaderStyle;
end;

procedure TscAdvancedComboEdit.SetListBoxHeaderStyle(Value: TscAdvancedHeaderStyle);
begin
  FListBox.HeaderStyle := Value;
end;

function TscAdvancedComboEdit.GetListBoxSelectionStyle: TscAdvancedSelectionStyle;
begin
  Result := FListBox.SelectionStyle;
end;

procedure TscAdvancedComboEdit.SetListBoxSelectionStyle(Value: TscAdvancedSelectionStyle);
begin
  FListBox.SelectionStyle := Value;
end;

function TscAdvancedComboEdit.GetListBoxShowItemDetails: Boolean;
begin
  Result := FListBox.ShowItemDetails;
end;

procedure TscAdvancedComboEdit.SetListBoxShowItemDetails(Value: Boolean);
begin
  FListBox.ShowItemDetails := Value;
end;

function TscAdvancedComboEdit.GetListBoxShowItemTitles: Boolean;
begin
  Result := FListBox.ShowItemTitles;
end;

procedure TscAdvancedComboEdit.SetListBoxShowItemTitles(Value: Boolean);
begin
  FListBox.ShowItemTitles := Value;
end;

function TscAdvancedComboEdit.GetListBoxShowLines: Boolean;
begin
  Result := FListBox.ShowLines;
end;

procedure TscAdvancedComboEdit.SetListBoxShowLines(Value: Boolean);
begin
  FListBox.ShowLines := Value;
end;

function TscAdvancedComboEdit.GetListBoxItemHeight: Integer;
begin
  Result := FlistBox.ItemHeight;
end;

procedure TscAdvancedComboEdit.SetListBoxItemHeight(Value: Integer);
begin
  FlistBox.ItemHeight := Value;
end;

function TscAdvancedComboEdit.GetListBoxHeaderHeight: Integer;
begin
  Result := FListBox.HeaderHeight;
end;

procedure TscAdvancedComboEdit.SetListBoxHeaderHeight(Value: Integer);
begin
  FListBox.HeaderHeight := Value;
end;

procedure TscButtonBarItem.Assign(Source: TPersistent);
begin
  if Source is TscButtonBarItem then
  begin
    Caption := TscButtonBarItem(Source).Caption;
    ImageIndex := TscButtonBarItem(source).ImageIndex;
    OnClick:= TscButtonBarItem(source).OnClick;
    Tag := TscButtonBarItem(source).Tag;
    Layout := TscButtonBarItem(source).Layout;
    Margin := TscButtonBarItem(source).Margin;
    Spacing := TscButtonBarItem(source).Spacing;
    Enabled := TscButtonBarItem(source).Enabled;
  end
  else inherited Assign(Source);
end;

procedure TscButtonBarItem.Click;
begin
  if Assigned(OnClick) then OnClick(Self);
end;

constructor TscButtonBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FTag := 0;
  FLayout := blGlyphTop;
  FMargin := -1;
  FSpacing := 1;
  FHint := '';
end;

function TscButtonBarItem.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;


procedure TscButtonBarItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex<>value then
  begin
    FImageIndex := Value;
    changed(false)
  end;
end;

procedure TscButtonBarItem.ItemClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TscButtonBarItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscButtonBarItem.SetLayout;
begin
  FLayout := Value;
  Changed(False);
end;

procedure TscButtonBarItem.SetMargin;
begin
  FMargin := Value;
  Changed(False);
end;

procedure TscButtonBarItem.SetSpacing;
begin
  FSpacing := Value;
  Changed(False);
end;

function TscButtonBarItems.Add: TscButtonBarItem;
begin
  Result := TscButtonBarItem(inherited Add);
end;

constructor TscButtonBarItems.Create(Section: TscButtonBarSection);
begin
  inherited Create(TscButtonBarItem);
  FSection := Section;
end;

function TscButtonBarItems.GetItem(Index: Integer): TscButtonBarItem;
begin
  Result := TscButtonBarItem(inherited GetItem(Index));
end;

function TscButtonBarItems.GetOwner: TPersistent;
begin
  Result := FSection;
end;

procedure TscButtonBarItems.SetItem(Index: Integer; Value: TscButtonBarItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TscButtonBarItems.Update(Item: TCollectionItem);
begin
  FSection.Changed(False);
end;

constructor TscSectionButton.CreateEx;
begin
  inherited Create(AOwner);
  TransparentBackground := False;
  FButtonsBar := AButtonsBar;
  Height := FButtonsBar.ButtonHeight;
  FItemIndex := AIndex;
  if FButtonsBar.ButtonStyle = scbbPushButton then
    StyleKind := scbsPushButton
  else
    StyleKind := scbsHeaderSection;
  CanFocused := False;
  Animation := FButtonsBar.ItemAnimation;
end;

procedure TscSectionButton.ButtonClick;
begin
  FButtonsBar.Sections[FItemIndex].Click;
  FButtonsBar.OpenSection(FItemIndex);
  inherited;
end;

function TscSectionButton.GetCtrlState: TscsCtrlState;
begin
  if (FButtonsBar <> nil) and (FButtonsBar.AllButtonsOnTop)
     and (not FButtonsBar.ReorderButtons) and (FItemIndex = FButtonsBar.SectionIndex)
  then
    Result := scsPressed
  else
    Result := inherited GetCtrlState;
end;

constructor TscSectionItem.CreateEx;
var
  C: TColor;
begin
  inherited Create(AOwner);
  ParentFont := False;
  CanFocused := False;
  FButtonsBar := AButtonsBar;
  case FButtonsBar.ItemStyle of
    scbiToolButton: StyleKind := scbsToolButtonTransparent;
    scbiLink:
      begin
        StyleKind := scbsLink;
        Cursor := crHandPoint;
      end;
    scbiTransparent: StyleKind := scbsTransparent;
  end;
  StyleElements := FButtonsBar.StyleElements;
  Animation := FButtonsBar.ItemAnimation;
  GlowEffect.Assign(FButtonsBar.ItemGlowEffect);
  ImageGlow := FButtonsBar.ItemImageGlow;
  Font.Assign(FButtonsBar.ItemFont);
  if FButtonsBar.Sections.Items[ASectionIndex].ItemGlowColor <> clNone then
  begin
    C := FButtonsBar.Sections.Items[ASectionIndex].ItemGlowColor;
    GlowEffect.PressedColor := C;
    GlowEffect.HotColor := C;
    GlowEffect.Color := C;
    GlowEffect.FocusedColor := C;
  end;
  if FButtonsBar.Sections.Items[ASectionIndex].ItemFontColor <> clNone then
  begin
    StyleElements := StyleElements - [seFont];
    Font.Color := FButtonsBar.Sections.Items[ASectionIndex].ItemFontColor;
  end;
  FItemIndex := AIndex;
  FSectionIndex := ASectionIndex;
  Spacing := 5;
end;

procedure TscSectionItem.ButtonClick;
begin
  FButtonsBar.FItemIndex := FItemIndex;
  if not FStopClick then
    FButtonsBar.Sections[FSectionIndex].Items[FItemIndex].Click;
  inherited;
end;

constructor TscButtonBarSection.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FItems := TscButtonBarItems.Create(self);
  FCustomColorOptions := TscButtonColorOptions.Create;
  FCustomColorOptions.OnChange := OnColorOptionsChange;
  FCustomColors := False;
  FHint := '';
  FBGColor := clNone;
  FItemFontColor := clNone;
  FItemGlowColor := clNone;
  FTag := 0;
  FSpacing := 1;
  FMargin := -1;
  FEnabled := True;
end;

procedure TscButtonBarSection.OnColorOptionsChange(Sender: TObject);
begin
  if FCustomColors then
    Changed(False);
end;

procedure TscButtonBarSection.SetCustomColors(Value: Boolean);
begin
  if FCustomColors <> Value then
  begin
    FCustomColors := Value;
    Changed(False);
  end;
end;

procedure TscButtonBarSection.SetItemFontColor(const Value: TColor);
begin
  if FItemFontColor <> Value then
  begin
    FItemFontColor := Value;
    Changed(False);
  end;
end;

procedure TscButtonBarSection.SetItemGlowColor(const Value: TColor);
begin
  if FItemGlowColor <> Value then
  begin
    FItemGlowColor := Value;
    Changed(False);
  end;
end;

procedure TscButtonBarSection.SetBGColor(const Value: TColor);
begin
  if FBGColor <> Value then
  begin
    FBGColor := Value;
    Changed(False);
  end;
end;

procedure TscButtonBarSection.SetMargin;
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Changed(False)
  end;
end;

procedure TscButtonBarSection.SetSpacing;
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed(False);
  end;
end;

procedure TscButtonBarSection.Assign(Source: TPersistent);
begin
  if Source is TscButtonBarSection then
  begin
    FBGColor := TscButtonBarSection(Source).BGColor;
    FItemFontColor := TscButtonBarSection(Source).ItemFontColor;
    FCaption := TscButtonBarSection(Source).Caption;
    FImageIndex := TscButtonBarSection(Source).ImageIndex;
    FTag := TscButtonBarSection(Source).Tag;
    FOnClick := TscButtonBarSection(source).OnClick;
    fMargin := TscButtonBarSection(source).Margin;
    FSpacing := TscButtonBarSection(source).Spacing;
    FEnabled := TscButtonBarSection(source).Enabled;
    FCustomColors := TscButtonBarSection(source).CustomColors;
    FCustomColorOptions.Assign(TscButtonBarSection(source).CustomColorOptions);
  end
  else
    inherited Assign(Source);
end;

function TscButtonBarSection.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;


procedure TscButtonBarSection.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscButtonBarSection.SetItems(const Value: TscButtonBarItems);
begin
  FItems.assign(Value);
end;

destructor TscButtonBarSection.Destroy;
begin
  FItems.Free;
  FCustomColorOptions.Free;
  inherited;
end;

procedure TscButtonBarSection.SectionClick(const Value: TNotifyEvent);
begin
  FonClick := Value;
end;

procedure TscButtonBarSection.Click;
begin
  if assigned(onClick) then
    onclick(self);
end;

procedure TscButtonBarSection.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

constructor TscButtonBarSections.Create(ButtonsBar: TscButtonsBar);
begin
  inherited Create(TscButtonBarSection);
  FButtonsBar := ButtonsBar;
end;

function TscButtonBarSections.GetButtonsBar: TscButtonsBar;
begin
  Result := FButtonsBar;
end;

function TscButtonBarSections.Add: TscButtonBarSection;
begin
  Result := TscButtonBarSection(inherited Add);
end;

function TscButtonBarSections.GetItem(Index: Integer): TscButtonBarSection;
begin
  Result := TscButtonBarSection(inherited GetItem(Index));
end;

function TscButtonBarSections.GetOwner: TPersistent;
begin
  Result := FButtonsBar;
end;

procedure TscButtonBarSections.SetItem(Index: Integer; Value: TscButtonBarSection);
begin
  inherited SetItem(Index, Value);
end;

procedure TscButtonBarSections.Update(Item: TCollectionItem);
begin
  if Item = nil
  then FButtonsBar.UpdateSections
  else FButtonsBar.UpdateSection(Item.Index);
  if Count = 0 then FButtonsBar.ClearSections;
end;

constructor TscButtonsBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReorderButtons := False;
  FAllButtonsOnTop := False;
  FSelectFirstItem := False;
  Color := clBtnFace;
  FItemPanelStyleColor := True;
  FItemImageGlow := True;
  FItemPanelColor := Color;
  FItemAnimation := False;
  FItemGlowEffect := TscButtonGlowEffect.Create;
  FItemGlowEffect.OnChange := OnItemsChange;
  FItemPanel := TscPanel.Create(Self);
  FItemPanelWallpaperIndex := -1;
  with FItemPanel do
  begin
    TransparentBackground := False;
    FItemPanel.StyleKind := scpsPanel;
    OnResize := OnItemPanelResize;
  end;
  FItemIndex := -1;

  FShowSelectedItem := False;

  FShowButtons := True;
  FShowItemHint := True;

  FItemFont := TFont.Create;
  FItemFont.OnChange := OnItemsChange;

  FUpButton := nil;
  FDownButton := nil;

  Width := 150;
  FButtonHeight := 25;
  FItemHeight := 60;
  Align := alLeft;
  FSectionButtons := TList.Create;
  FSectionItems := TList.Create ;
  FSections := TscButtonBarSections.Create(Self);
  AdjustControls;
end;

destructor TscButtonsBar.Destroy;
begin
  FItemGlowEffect.Free;
  FItemFont.Free;
  ClearItems;
  ClearSections;
  FSectionButtons.Free;
  FSectionItems.Free;
  FItemPanel.Free;
  FItemPanel := nil;
  FSections.Free;
  inherited Destroy;
end;

procedure TscButtonsBar.WMSIZE(var Msg: TMessage);
begin
  inherited;
  if HandleAllocated then
    AdjustControls;
end;

procedure TscButtonsBar.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  if FShowSelectedItem then
    SetItemIndex2(SectionIndex, GetItemIndex);
end;

procedure TscButtonsBar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  with ACanvas do
  begin
    Brush.Color := GetStyleColor(clBtnFace);
    FillRect(R);
  end;
  DrawBorder(ACanvas, R);
end;

procedure TscButtonsBar.OnItemsChange(Sender: TObject);
begin
  UpdateItems;
end;

procedure TscButtonsBar.SetAllButtonsOnTop(Value: Boolean);
begin
  if FAllButtonsOnTop <> Value then
  begin
    FAllButtonsOnTop := Value;
    AdjustControls;
    RePaintSections;
  end;
end;

procedure TscButtonsBar.SetReorderButtons(Value: Boolean);
begin
  if FReorderButtons <> Value then
  begin
    FReorderButtons := Value;
    AdjustControls;
  end;
end;

procedure TscButtonsBar.SetItemPanelWallpaperIndex(Value: Integer);
begin
  if FItemPanelWallpaperIndex <> Value then
  begin
    FItemPanelWallpaperIndex := Value;
    UpdateItems;
  end;
end;

procedure TscButtonsBar.SetItemPanelColor(Value: TColor);
begin
  if FItemPanelColor <> Value then
  begin
    FItemPanelColor := Value;
    UpdateItems;
  end;
end;

procedure TscButtonsBar.SetItemPanelStyle(Value: TscbbItemPanelStyle);
begin
  if FItemPanelStyle <> Value then
  begin
    FItemPanelStyle := Value;
    UpdateItems;
  end;
end;

procedure TscButtonsBar.SetButtonStyle(Value: TscbbButtonStyle);
begin
  if FButtonStyle <> Value then
  begin
    FButtonStyle := Value;
    UpdateSections;
  end;
end;

procedure TscButtonsBar.SetItemStyle(Value: TscbbItemStyle);
begin
  if FItemStyle <> Value then
  begin
    FItemStyle := Value;
    UpdateItems;
  end;
end;

procedure TscButtonsBar.AddItem(ASectionIndex: Integer;
                                    AItem: TscButtonBarItem);
var
  I: TscButtonBarItem;
begin
  I := TscButtonBarItem(Sections[ASectionIndex].Items.Add);
  if (I <> nil) and (AItem <> nil) then I.Assign(AItem);
  if SectionIndex = ASectionIndex then UpdateSection(ASectionIndex);
  if AItem <> nil then AItem.Free;
end;

procedure TscButtonsBar.InsertItem(AItemIndex, ASectionIndex: Integer;
                                       AItem: TscButtonBarItem);
var
  I: TscButtonBarItem;
begin
  I := TscButtonBarItem(Sections[ASectionIndex].Items.Insert(AItemIndex));
  if (I <> nil) and (AItem <> nil) then I.Assign(AItem);
  if SectionIndex = ASectionIndex then UpdateSection(ASectionIndex);
  if AItem <> nil then AItem.Free;
end;

procedure TscButtonsBar.DeleteItem(AItemIndex, ASectionIndex: Integer);
begin
  Sections[ASectionIndex].Items.Delete(AItemIndex);
  if SectionIndex = ASectionIndex then UpdateSection(ASectionIndex);
end;

function TscButtonsBar.GetItemIndex;
begin
  Result := FItemIndex;
end;

procedure TscButtonsBar.SetItemIndex;
begin
  if (ASectionIndex >= 0) and (ASectionIndex < Self.Sections.Count) then
    OpenSection(ASectionIndex);
  if (AItemIndex >= 0) and (AItemIndex < Sections[ASectionIndex].Items.Count) then
    with TscSectionItem(FSectionItems[AItemIndex]) do
      if FShowSelectedItem then SetDown(True);
end;

procedure TscButtonsBar.SetItemIndex2;
begin
  if (AItemIndex >= 0) and (AItemIndex < Sections[ASectionIndex].Items.Count) then
    with TscSectionItem(FSectionItems[AItemIndex]) do
    if FShowSelectedItem then
    begin
      FStopClick := True;
      SetDown(True);
      FStopClick := False;
    end;
end;

procedure TscButtonsBar.SetShowButtons;
begin
  if FShowButtons <> Value then
  begin
    FShowButtons := Value;
    UpdateSections;
  end;
end;

procedure TscButtonsBar.OnItemPanelResize(Sender: TObject);
begin
  CheckVisibleItems;
end;

procedure TscButtonsBar.SetButtonHeight(Value: Integer);
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    UpDateSectionButtons;
  end;
end;

procedure TscButtonsBar.SetItemFont;
begin
  FItemFont.Assign(Value);
end;

procedure TscButtonsBar.ShowUpButton;
begin
  FUpButton := TscButton.Create(Self);
  with FUpButton do
  begin
    TransparentBackground  := False;
    CanFocused := False;
    Visible := False;
    Parent := FItemPanel;
    CanFocused := False;
    Width := GetSystemMetrics(SM_CYHSCROLL);
    Height := GetSystemMetrics(SM_CYHSCROLL);
    Spacing := 0;
    StyleKind := scbsUpSpinButton;
    RepeatClick := True;
    RepeatClickInterval := 150;
    Caption := '';
    OnClick := UpButtonClick;
    Top := - Height;
    Visible := True;
  end;
end;

procedure TscButtonsBar.ShowDownButton;
begin
  FDownButton := TscButton.Create(Self);
  with FDownButton do
  begin
    TransparentBackground := False;
    CanFocused := False;
    Visible := False;
    Parent := FItemPanel;
    CanFocused:= False;
    Width := GetSystemMetrics(SM_CYHSCROLL);
    Height := GetSystemMetrics(SM_CYHSCROLL);
    Spacing := 0;
    StyleKind := scbsDownSpinButton;
    RepeatClick := True;
    RepeatClickInterval := 150;
    Caption := '';
    OnClick := DownButtonClick;
    Top := - Height;
    Visible := True;
  end;
end;

procedure TscButtonsBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FButtonHeight := MulDiv(FButtonHeight, M, D);
  FItemFont.Height := MulDiv(FItemFont.Height, M, D);
  FItemHeight :=  MulDiv(FItemHeight, M, D);
  UpdateItems;
  if (FSectionIndex >= 0) and (FItemIndex >= 0)  then
    SetItemIndex2(FSectionIndex, FItemIndex);
  AdjustControls;
end;

procedure TscButtonsBar.HideUpButton;
begin
  FUpButton.Free;
  FUpButton := nil;
end;

procedure TscButtonsBar.HideDownButton;
begin
  FDownButton.Free;
  FDownButton := nil;
end;

procedure TscButtonsBar.UpButtonClick(Sender: TObject);
begin
  ScrollUp;
end;

procedure TscButtonsBar.DownButtonClick(Sender: TObject);
begin
  ScrollDown;
end;

procedure TscButtonsBar.ArangeItems;
var
  I, J, Offset: Integer;
begin
  if (TopIndex > 0) and (FUpButton = nil) then
    ShowUpButton
  else
    if (TopIndex = 0) and (FUpButton <> nil) then HideUpButton;

  if (TopIndex + VisibleCount < FSectionItems.Count) and (FDownButton = nil) then
    ShowDownButton
  else
  if (TopIndex + VisibleCount >= FSectionItems.Count) and (FDownButton <> nil) then
    HideDownButton;

  if FUpButton <> nil then
    with FUpButton do
      SetBounds(FItemPanel.Width - Width, 0, Width, Height);

  if FDownButton <> nil then
    with FDownButton do
      SetBounds(FItemPanel.Width - Width, FItemPanel.Height - Height, Width, Height);

  J := 0;
  Offset := GetSystemMetrics(SM_CYHSCROLL) + 3;
  SendMessage(FItemPanel.Handle, WM_SETREDRAW, 0, 0);
  for I := 0 to FSectionItems.Count - 1 do
  with TscSectionItem(FSectionItems.Items[I]) do
  if Visible then
  begin
    if Parent <> nil then
      SendMessage(Handle, WM_SETREDRAW, 0, 0);
    {$IFDEF VER310_UP}
    if Parent <> FItemPanel then
    begin
      Parent := FItemPanel;
      Font.Assign(ItemFont);
    end;
    {$ENDIF}
    if (Left <> Offset) or (Top <> J) or (Width <> FItemPanel.Width - Offset * 2) then
      SetBounds(Offset, J, FItemPanel.Width - Offset * 2, FItemHeight);
    Inc(J, FItemHeight);
    {$IFNDEF VER310_UP}
    if Parent <> FItemPanel then Parent := FItemPanel;
    {$ENDIF}
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
  end;
  SendMessage(FItemPanel.Handle, WM_SETREDRAW, 1, 0);
  RedrawWindow(FItemPanel.Handle, nil, 0,
    RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_UPDATENOW);
end;

procedure TscButtonsBar.CheckVisibleItems;
var
  I: Integer;
  OldVisibleCount, OldTopIndex: Integer;
  CanVisible: Boolean;
begin
  OldVisibleCount := VisibleCount;
  OldTopIndex := TopIndex;
  VisibleCount := FItemPanel.Height div FItemHeight;

  if VisibleCount > FSectionItems.Count
  then VisibleCount := FSectionItems.Count;

  if VisibleCount = FSectionItems.Count then
    TopIndex := 0
  else
    if (TopIndex + VisibleCount > FSectionItems.Count) and (TopIndex > 0) then
     begin
       TopIndex := TopIndex - (VisibleCount - OldVisibleCount);
       if TopIndex < 0 then TopIndex := 0;
     end;

  SendMessage(FItemPanel.Handle, WM_SETREDRAW, 0, 0);
  for I := 0 to FSectionItems.Count - 1 do
  with TscSectionItem(FSectionItems.Items[I]) do
  begin
    CanVisible := (I >= TopIndex) and (I <= TopIndex + VisibleCount - 1);
    if CanVisible and not Visible then
    begin
      if I < OldTopIndex then
      begin
        Top := 0;
        Visible := CanVisible;
      end
      else
      begin
        Top := FItemPanel.Height;
        Visible := CanVisible;
      end;
    end
    else
    begin
      Visible := CanVisible;
      if not Visible then Parent := nil;
    end;
  end;
  SendMessage(FItemPanel.Handle, WM_SETREDRAW, 1, 0);
  ArangeItems;
end;

procedure TscButtonsBar.ScrollUp;
begin
  if (TopIndex = 0) or (VisibleCount = 0) then Exit;
  TscSectionItem(FSectionItems.Items[TopIndex + VisibleCount - 1]).Visible := False;
  Dec(TopIndex);
  TscSectionItem(FSectionItems.Items[TopIndex]).Visible := True;
  ArangeItems;
end;

procedure TscButtonsBar.ScrollDown;
begin
  if VisibleCount = 0 then Exit;
  if TopIndex + VisibleCount >= FSectionItems.Count then Exit;
  TscSectionItem(FSectionItems.Items[TopIndex]).Visible := False;
  Inc(TopIndex);
  TscSectionItem(FSectionItems.Items[TopIndex + VisibleCount - 1]).Visible := True;
  ArangeItems;
end;

procedure TscButtonsBar.SetItemHeight;
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    UpdateItems;
  end;
end;

procedure TscButtonsBar.UpDateSectionButtons;
var
  I: Integer;
begin
  if Sections.Count = 0 then Exit;
  for I := 0 to Sections.Count - 1 do UpdateSection(I);
  AdjustControls;
end;

procedure TscButtonsBar.OpenSection(Index: Integer);
begin
  if FSectionIndex = Index then Exit;

  FItemIndex := -1;

  if not (csDesigning in ComponentState) and not (csLoading in ComponentState)
  then
    if Assigned(FOnChanging) then FOnChanging(Self);

  FSectionIndex := Index;

  AdjustControls;

  if AllButtonsOnTop and not ReorderButtons then
    RePaintSections;

  UpdateItems;

  if Index <> -1 then Sections[Index].Click;

  if SelectFirstItem then SetItemIndex(FSectionIndex, 0);

  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscButtonsBar.ClearItems;
var
  I: Integer;
begin
  if FSectionItems = nil then Exit;
  if FSectionItems.Count = 0 then Exit;
  for I := FSectionItems.Count - 1 downto 0 do
    TscSectionItem(FSectionItems.Items[I]).Free;
  FSectionItems.Clear;
end;

procedure TscButtonsBar.ClearSections;
var
  I: Integer;
begin
  if FSectionButtons = nil then Exit;
  if FSectionButtons.Count = 0 then Exit;
  for I := 0 to FSectionButtons.Count - 1 do
    TscSectionButton(FSectionButtons.Items[I]).Free;
  FSectionButtons.Clear;
end;

procedure TscButtonsBar.CreateWnd;
begin
  inherited CreateWnd;
  UpdateSections;
  UpdateItems;
end;

procedure TscButtonsBar.SetSections(Value: TscButtonBarSections);
begin
  FSections.Assign(Value);
end;

procedure TscButtonsBar.UpdateSection(Index: Integer);
var
  S: TscButtonBarSection;
  I: Integer;
  B: Boolean;
begin
  if not HandleAllocated then Exit;
  if FSections.Count = 0 then Exit;
  if not FShowButtons then
  begin
    UpdateItems;
    Exit;
  end;
  S := TscButtonBarSection(Sections.Items[Index]);
  for I := 0 to FSectionButtons.Count - 1 do
  with TscSectionButton(FSectionButtons.Items[I]) do
  if FItemIndex = Index then
  begin
    if S.CustomColors then
    begin
      StyleKind := scbsColorButton;
      ColorOptions.Assign(S.CustomColorOptions);
    end
    else
    if ButtonStyle = scbbPushButton then
      StyleKind := scbsPushButton
    else
      StyleKind := scbsHeaderSection;  
    Hint := S.Hint;
    Margin := S.Margin;
    Spacing := S.Spacing;
    ShowHint := Self.ShowItemHint;
    B := Caption <> S.Caption;
    Enabled := S.Enabled;
    if B then Caption := S.Caption;
    if (S.ImageIndex <> -1) and (FSectionImages <> nil) and (S.ImageIndex < FSectionImages.Count)
    then
      begin
        ImageIndex := S.ImageIndex;
        Images := FSectionImages;
      end
    else
      begin
        ImageIndex := -1;
        Images := nil;
      end;
    RePaintControl;
    if (FSectionIndex = Index) and not B then UpdateItems;
    Break;
  end;
end;

procedure TscButtonsBar.AdjustControls;
var
  I: Integer;
  R, R1: TRect;
  Y1, Y2, BW: Integer;
begin
  if not HandleAllocated then Exit;
  if FSections.Count = 0 then Exit;
  if Self.BorderStyle = scbsSingle then
    BW := 2
  else
    BW := 0;
  Y1 := BW;
  Y2 := ClientHeight - BW;

  if FShowButtons and (FSectionIndex <> -1) and (FSectionButtons.Count > 0) and not FAllButtonsOnTop
  then
    begin
      R := Rect(BW, BW, ClientWidth - BW, FButtonHeight);
      R1 := Rect(BW, ClientHeight - FButtonHeight - BW, ClientWidth - BW , ClientHeight - BW);
      for I := 0 to FSectionIndex do
        with TscSectionButton(FSectionButtons.Items[I]) do
        begin
          Inc(Y1, FButtonHeight);
          if Parent <> Self then
          begin
            Visible := False;
            Parent := Self;
            SetBounds(R.Left, R.Top, R.Width, FButtonHeight);
            Visible := True;
          end
          else
            SetBounds(R.Left, R.Top, R.Width, FButtonHeight);
          OffsetRect(R, 0, FButtonHeight);
        end;

      for I := FSectionButtons.Count - 1 downto FSectionIndex + 1 do
        with TscSectionButton(FSectionButtons.Items[I]) do
        begin
          Dec(Y2, FButtonHeight);
          if Parent <> Self then
          begin
            Visible := False;
            Parent := Self;
            SetBounds(R1.Left, R1.Top, R1.Width, FButtonHeight);
            Visible := True;
          end
          else
            SetBounds(R1.Left, R1.Top, R1.Width, FButtonHeight);
          OffsetRect(R1, 0, -FButtonHeight);
        end;
    end
  else
  if FShowButtons and ((FSectionIndex = -1) or (Self.FAllButtonsOnTop))
    and (FSectionButtons.Count > 0) then
  begin
    R := Rect(BW, BW, ClientWidth - BW, FButtonHeight);
    for I := 0 to FSectionButtons.Count - 1 do
      if (I <> FSectionIndex) or not FReorderButtons then
       with TscSectionButton(FSectionButtons.Items[I]) do
       begin
         Inc(Y1, FButtonHeight);
         if Parent <> Self then
         begin
           Visible := False;
           Parent := Self;
           SetBounds(R.Left, R.Top, R.Width, FButtonHeight);
           Visible := True;
         end
         else
           SetBounds(R.Left, R.Top, R.Width, FButtonHeight);
         OffsetRect(R, 0, FButtonHeight);
       end;
    if (FSectionIndex <> -1) and FReorderButtons then
    with TscSectionButton(FSectionButtons.Items[FSectionIndex]) do
    begin
      Inc(Y1, FButtonHeight);
      if Parent <> Self then
      begin
        Visible := False;
        Parent := Self;
        SetBounds(R.Left, R.Top, R.Width, FButtonHeight);
        Visible := True;
      end
      else
        SetBounds(R.Left, R.Top, R.Width, FButtonHeight);
    end;
  end;
  if FItemPanel <> nil then
  with FItemPanel do
  begin
    if Parent <> Self then
    begin
      Visible := False;
      Parent := Self;
      SetBounds(BW, Y1, Self.ClientWidth - BW * 2, Y2 - Y1);
      Visible := True;
    end
    else
      SetBounds(BW, Y1, Self.ClientWidth - BW * 2, Y2 - Y1);
  end;
end;

procedure TscButtonsBar.RePaintSections;
var
  I: Integer;
begin
  if  FSectionButtons.Count = 0 then Exit;
  if csLoading in ComponentState then Exit;
  for I := 0 to FSectionButtons.Count - 1  do
  begin
   if FAllButtonsOnTop and
      (FSectionIndex <> TscSectionButton(FSectionButtons[I]).FItemIndex)
   then
      TscSectionButton(FSectionButtons[I]).FOldCtrlState := scsNormal;
    TscSectionButton(FSectionButtons[I]).RePaintControl;
  end;
end;

procedure TscButtonsBar.UpdateSections;
var
  I: Integer;
  S: TscButtonBarSection;
  WantUpdateItems: Boolean;
begin
  if not HandleAllocated then Exit;
  if FSections.Count = 0 then Exit;

  WantUpdateItems := False;

  if FSectionIndex > FSections.Count - 1 then
  begin
    FSectionIndex := FSections.Count - 1;
    WantUpdateItems := True;
  end;

  ClearSections;

  if not FShowButtons then
  begin
    CheckVisibleItems;
    Exit;
  end;

  for I := 0 to Sections.Count - 1  do
  begin
    S := TscButtonBarSection(Sections.Items[I]);
    FSectionButtons.Add(TscSectionButton.CreateEx(Self, Self, I));
    with TscSectionButton(FSectionButtons.Items[FSectionButtons.Count - 1]) do
    begin
      if S.CustomColors then
      begin
        StyleKind := scbsColorButton;
        ColorOptions.Assign(S.CustomColorOptions);
      end;
      Caption := S.Caption;
      Hint := S.Hint;
      Margin := S.Margin;
      Spacing := S.Spacing;
      ShowHint := Self.ShowItemHint;
      Enabled := S.Enabled;
      if (S.ImageIndex <> -1) and (FSectionImages <> nil) and (S.ImageIndex < FSectionImages.Count) then
      begin
        ImageIndex := S.ImageIndex;
        Images := FSectionImages;
      end
      else
      begin
        ImageIndex := -1;
        Images := nil;
      end;
    end;
  end;

  AdjustControls;

  if WantUpdateItems then UpdateItems else CheckVisibleItems;
end;

procedure TscButtonsBar.UpdateItems;
var
  I: Integer;
  It: TscButtonBarItem;
begin
  if not HandleAllocated then Exit;
  if FSections.Count = 0 then Exit;
  if FShowButtons and (FSectionButtons.Count = 0) then Exit;
  ClearItems;

  if (FSectionIndex <> -1) and (FSections[FSectionIndex].BGColor <> clNone) then
  begin
    FItemPanel.StyleKind := scpsPanel;
    FItemPanel.Color := FSections[FSectionIndex].BGColor;
  end
  else
  begin
    if FItemPanelStyleColor then
      FItemPanel.Color := GetStyleColor(FItemPanelColor)
    else
      FItemPanel.Color := FItemPanelColor;
    if FItemPanelStyle = scbpPanel then
      FItemPanel.StyleKind := scpsPanel
    else
     FItemPanel.StyleKind := scpsFormBackground;
  end;

  FItemPanel.Wallpapers := FWallpapers;
  FItemPanel.WallpaperIndex := FItemPanelWallpaperIndex;


  if FSectionIndex = -1 then
  begin
    if FUpButton <> nil then HideUpButton;
    if FDownButton <> nil then HideDownButton;
    Exit;
  end;

  if FUpButton <> nil then HideUpButton;
  if FDownButton <> nil then HideDownButton;
  if FSections.Items[FSectionIndex].Items.Count = 0 then Exit;
  TopIndex := 0;
  for I := 0 to FSections.Items[FSectionIndex].Items.Count - 1 do
  begin
    It := TscButtonBarItem(FSections.Items[FSectionIndex].Items[I]);
    FSectionItems.Add(TscSectionItem.CreateEx(FItemPanel, Self, FSectionIndex, I));
    with TscSectionItem(FSectionItems.Items[FSectionItems.Count - 1]) do
    begin
      Caption := It.Caption;
      Hint := It.Hint;
      Enabled := It.Enabled;
      ShowHint := Self.ShowItemHint;
      if FShowSelectedItem then GroupIndex := 1;
      if (It.ImageIndex <> -1) and (FItemImages <> nil) and (It.ImageIndex < FitemImages.Count) then
      begin
        ImageIndex := It.ImageIndex;
        Images := FItemImages;
      end
      else
      begin
        ImageIndex := -1;
        Images := nil;
      end;
      Layout := It.Layout;
      Margin := It.Margin;
      Spacing := It.Spacing;
    end;
  end;
  CheckVisibleItems;
end;

procedure TscButtonsBar.SetSectionIndex(const Value: integer);
begin
  if (Value >= -1) and (Value <> FSectionIndex) and (Value < Sections.Count) then
    OpenSection(Value);
end;

procedure TscButtonsBar.SetItemImages(const Value: TCustomImageList);
begin
  if FItemImages <> Value then
  begin
    FItemImages := Value;
    UpdateItems;
  end;
end;

procedure TscButtonsBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (operation=opremove) and (Acomponent = FItemImages) then
    SetItemImages(nil);
  if (operation=opremove) and (Acomponent=FSectionImages) then
    SetSectionImages(nil);
  if (operation=opremove) and (Acomponent=Wallpapers) and (FItemPanel <> nil) then
    FItemPanel.Wallpapers := nil;
end;

procedure TscButtonsBar.SetSectionImages(const Value: TCustomImageList);
begin
  if FSectionImages <> Value then
  begin
    FSectionImages := Value;
    UpDateSectionButtons;
  end;
end;

end.
