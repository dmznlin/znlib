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

unit scGPVertPagers;

{$I scdefine.inc}
{$R-}

interface
  uses System.Variants, Winapi.Windows, System.SysUtils, Winapi.Messages,
     Vcl.Controls, System.Classes, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.Themes,
     Vcl.ImgList, Vcl.Mask, Vcl.Buttons,
     scDrawUtils, scGPUtils, scControls, scGPControls,  scGPExtControls,
     scGPPagers, scImageCollection,
     WinApi.GdipObj, WinApi.GdipApi;

  type

  TscGPVertPageControl = class;
  TscGPVertPageControlTab = class;
  TscGPVertPageControlPage = class;

  TscGPVertPageControlTab = class(TCollectionItem)
  protected
    FOnClick: TNotifyEvent;
    FPage: TscGPVertPageControlPage;
    FVisible: Boolean;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FCaption: String;
    FCustomOptions: TscGPTabOptions;
    FUseCustomOptions: Boolean;
    FCustomFrameColor: TColor;
    FCustomFrameColorAlpha: Byte;
    FCustomGlowEffect: TscButtonGlowEffect;
    FShowCloseButton: Boolean;
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetPage(const Value: TscGPVertPageControlPage);
    procedure SetCaption(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetUseCustomOptions(Value: Boolean);
    procedure SetCustomFrameColor(Value: TColor);
    procedure SetCustomFrameColorAlpha(Value: Byte);
    procedure OnOptionsChange(Sender: TObject);
  public
    Active: Boolean;
    TabRect: TRect;
    CloseButtonRect: TRect;
    CloseButtonMouseIn, CloseButtonMouseDown:Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Page: TscGPVertPageControlPage read FPage write SetPage;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: String read FCaption write SetCaption;
    property CustomOptions: TscGPTabOptions
      read FCustomOptions write FCustomOptions;
    property CustomGlowEffect: TscButtonGlowEffect read
      FCustomGlowEffect write FCustomGlowEffect;
    property CustomFrameColor: TColor
      read FCustomFrameColor write SetCustomFrameColor;
    property CustomFrameColorAlpha: Byte
      read FCustomFrameColorAlpha write SetCustomFrameColorAlpha;
    property ShowCloseButton: Boolean
      read FShowCloseButton write SetShowCloseButton;
    property UseCustomOptions: Boolean
      read FUseCustomOptions write SetUseCustomOptions;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscGPVertTabScrollButton = class(TscGPGlyphButton)
  protected
    FBottom: Boolean;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscGPVertPageControlTabs = class(TCollection)
  private
    function GetItem(Index: Integer):  TscGPVertPageControlTab;
    procedure SetItem(Index: Integer; Value:  TscGPVertPageControlTab);
  protected
    Pager: TscGPVertPageControl;
    DestroyPage: TscGPVertPageControlPage;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(APager: TscGPVertPageControl);
    function Add: TscGPVertPageControlTab;
    function Insert(Index: Integer): TscGPVertPageControlTab;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TscGPVertPageControlTab read GetItem write SetItem; default;
  end;

  TscGPVertPageControlPage = class(TscCustomScrollBox)
  protected
    FDestroyFromPager: Boolean;
    FBGStyle: TscGPPageBGStyle;
    procedure SetBGStyle(Value: TscGPPageBGStyle);
  public
    Pager: TscGPVertPageControl;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property AutoScroll default False;
    property BGStyle: TscGPPageBGStyle
      read FBGStyle write SetBGStyle;
    property FullUpdate default False;
    property Wallpapers;
    property WallpaperIndex;
    property CustomImages;
    property CustomBackgroundImageIndex;
  end;

  TscGPVertTabControlBorderStyle = (scgpvbsFrame, scgpvbsLine, scgpvbsLineLeftRight, scgpvbsNone);

  TscGPVertPageControl = class(TscCustomControl)
  private
    FTabsScaling: Boolean;
    FTabsBGFillColor: TColor;
    FTabsBGFillColorAlpha: Byte;
    FFrameWidth: Integer;
    FFrameScaleWidth: Boolean;
    FFrameColor: TColor;
    FFrameColorAlpha: Byte;
    FTabOptions: TscGPTabOptions;
    FShowCloseButtons: Boolean;
    FBorderStyle: TscGPVertTabControlBorderStyle;
    FMouseWheelSupport: Boolean;
    FShowInActiveTab: Boolean;
    FScrollOffset: Integer;
    FScrollVisible: Boolean;
    FOldHeight: Integer;
    FMouseIn: Boolean;
    FTabIndex: Integer;
    FTabIndexBeforeFocused, FTabIndexAfterFocused: Integer;
    FTabs: TscGPVertPageControlTabs;
    FLeftOffset, FRightOffset: Integer;
    FOldTabActive, FTabActive: Integer;
    FActivePage: TscGPVertPageControlPage;
    FTabWidth: Integer;
    FTabHeight: Integer;
    FTabImages: TCustomImageList;
    FOnChangePage: TNotifyEvent;
    FOnChangingPage: TNotifyEvent;
    FOnCanChangePage: TscCanChangePageEvent;

    FTabSpacing,
    FTabMargin: Integer;

    FTabGlowEffect: TscButtonGlowEffect;

    FTabsLeftOffset,
    FTabsBottomOffset, FTabsTopOffset: Integer;
    FTopScrollButton, FBottomScrollButton: TscGPVertTabScrollButton;
    FTopTabIndex, FBottomTabIndex: Integer;
    FShowFocusRect: Boolean;
    FFreeOnClose: Boolean;
    FOnClose: TscTabCloseEvent;
    FOnGetTabDrawParams: TscGPGetAdvTabDrawParamsEvent;
    FScrollButtonHeight: Integer;
    FCloseButtonSize: Integer;

    FTabsWallpapers: TscCustomImageCollection;
    FTabsWallpaperIndex: Integer;
    FTabsWallpaper2Index: Integer;

    procedure SetTabsWallpaperIndex(Value: Integer);
    procedure SetTabsWallpaper2Index(Value: Integer);
    procedure SetTabsWallpapers(Value: TscCustomImageCollection);

    procedure SetTabsBGFillColor(Value: TColor);
    procedure SetTabsBGFillColorAlpha(Value: Byte);

    procedure SetFrameWidth(Value: Integer);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameColorAlpha(Value: Byte);

    procedure SetScrollButtonHeight(Value: Integer);
    procedure SetShowCloseButtons(Value: Boolean);
    procedure SetBorderStyle(Value: TscGPVertTabControlBorderStyle);
    procedure SetShowInActiveTab(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetTabsTopOffset(Value: Integer);
    procedure SetTabsLeftOffset(Value: Integer);
    procedure SetTabsBottomOffset(Value: Integer);
    procedure SetTabSpacing(Value: Integer);
    procedure SetTabMargin(Value: Integer);
    procedure SetTabs(AValue: TscGPVertPageControlTabs);
    procedure SetActivePage(const Value: TscGPVertPageControlPage);
    function GetPageIndex(Value: TscGPVertPageControlPage): Integer;
    function GetPageBoundsRect: TRect;
    function TabFromPoint(P: TPoint): Integer;
    procedure SetTabIndex(Value: Integer);
    procedure SetTabHeight(Value: Integer);
    procedure SetTabWidth(Value: Integer);
    procedure SetTabImages(Value: TCustomImageList);
    procedure OnControlChange(Sender: TObject);
    procedure ShowScrollButtons;
    procedure HideScrollButtons;
    procedure OnTopScrollButtonClick(Sender: TObject);
    procedure OnBottomScrollButtonClick(Sender: TObject);
    procedure AdjustScrollButtons;
  protected
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure DrawCloseButton(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect; AIndex: Integer; AColor: TColor);
    procedure ScrollToTop;
    procedure ScrollToBottom;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure UpdateControls; override;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL); message WM_MOUSEWHEEL;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure TestActive(X, Y: Integer);
    procedure Loaded; override;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure CalcTabRects;
    procedure DrawTab(ACanvas: TCanvas; G: TGPGraphics; Index: Integer; AFirst: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure GetScrollInfo;

    procedure ScrollToTab(AIndex: Integer);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;

    function FindDrawNextTabFromIndex(AIndex: Integer): Integer;
    function FindDrawPriorTabFromIndex(AIndex: Integer): Integer;

  public
    procedure DoClose;
    procedure FindNextTab;
    procedure FindPriorTab;
    procedure FindFirstTab;
    procedure FindLastTab;
    function FindNextTabFromIndex(AIndex: Integer): Integer;
    function FindPriorTabFromIndex(AIndex: Integer): Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePage: TscGPVertPageControlPage;
    procedure UpdateTabs;
  published
    property Align;
    property Font;
    property Color;
    property DrawTextMode;
    property TabsBGFillColor: TColor
      read FTabsBGFillColor write SetTabsBGFillColor;
    property TabsBGFillColorAlpha: Byte
      read FTabsBGFillColorAlpha write SetTabsBGFillColorAlpha;

    property TransparentBackground;
    property FrameWidth: Integer
      read FFrameWidth write SetFrameWidth;
    property FrameScaleWidth: Boolean
      read FFrameScaleWidth write FFrameScaleWidth;
    property FrameColor: TColor
      read FFrameColor write SetFrameColor;
    property FrameColorAlpha: Byte
      read FFrameColorAlpha write SetFrameColorAlpha;
    property BorderStyle: TscGPVertTabControlBorderStyle
      read FBorderStyle write SetBorderStyle;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;
    property ShowInActiveTab: Boolean read FShowInactiveTab write SetShowInActiveTab;
    property ShowCloseButtons: Boolean read FShowCloseButtons write SetShowCloseButtons;
    property TabsLeftOffset: Integer
      read FTabsLeftOffset write SetTabsLeftOffset;
    property TabsBottomOffset: Integer
      read FTabsBottomOffset write SetTabsBottomOffset;
    property TabsTopOffset: Integer
      read FTabsTopOffset write SetTabsTopOffset;
    property TabGlowEffect: TscButtonGlowEffect read FTabGlowEffect write FTabGlowEffect;
    property TabOptions: TscGPTabOptions read FTabOptions write FTabOptions;

    property TabSpacing: Integer read FTabSpacing write SetTabSpacing;
    property TabMargin: Integer read FTabMargin write SetTabMargin;

    property ScrollButtonHeight: Integer
      read FScrollButtonHeight write SetScrollButtonHeight;

    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property TabHeight: Integer read FTabHeight write SetTabHeight;
    property Tabs: TscGPVertPageControlTabs read FTabs write SetTabs;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property ActivePage: TscGPVertPageControlPage read FActivePage write SetActivePage;
    property TabImages: TCustomImageList
      read FTabImages write SetTabImages;

    property StorePaintBuffer;

    property FreeOnClose: Boolean read FFreeOnClose write FFreeOnClose;

    property TabsWallpapers: TscCustomImageCollection read FTabsWallpapers write SetTabsWallpapers;
    property TabsWallpaperIndex: Integer read FTabsWallpaperIndex write SetTabsWallpaperIndex;
    property TabsWallpaper2Index: Integer read FTabsWallpaper2Index write SetTabsWallpaper2Index;

    property OnChangingPage: TNotifyEvent read FOnChangingPage write FOnChangingPage;
    property OnChangePage: TNotifyEvent read FOnChangePage write FOnChangePage;
    property OnCanChangePage: TscCanChangePageEvent
      read FOnCanChangePage write FOnCanChangePage;
    property OnClose: TscTabCloseEvent read FOnClose write FOnClose;
    property OnGetTabDrawParams: TscGPGetAdvTabDrawParamsEvent
      read FOnGetTabDrawParams write FOnGetTabDrawParams;
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

  TscGPVertTabControl = class;

  TscGPVertTabControlTab = class(TCollectionItem)
  protected
    FOnClick: TNotifyEvent;
    FTabIndex: Integer;
    FVisible: Boolean;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FCaption: String;
    FCustomOptions: TscGPTabOptions;
    FUseCustomOptions: Boolean;
    FCustomFrameColor: TColor;
    FCustomFrameColorAlpha: Byte;
    FCustomGlowEffect: TscButtonGlowEffect;
    FShowCloseButton: Boolean;
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetCaption(Value: String);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetUseCustomOptions(Value: Boolean);
    procedure SetCustomFrameColor(Value: TColor);
    procedure SetCustomFrameColorAlpha(Value: Byte);
    procedure OnOptionsChange(Sender: TObject);
  public
    Active: Boolean;
    TabRect: TRect;
    CloseButtonRect: TRect;
    CloseButtonMouseIn, CloseButtonMouseDown:Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: String read FCaption write SetCaption;
    property CustomOptions: TscGPTabOptions
      read FCustomOptions write FCustomOptions;
    property CustomGlowEffect: TscButtonGlowEffect read
      FCustomGlowEffect write FCustomGlowEffect;
    property CustomFrameColor: TColor
      read FCustomFrameColor write SetCustomFrameColor;
    property CustomFrameColorAlpha: Byte
      read FCustomFrameColorAlpha write SetCustomFrameColorAlpha;
    property ShowCloseButton: Boolean
      read FShowCloseButton write SetShowCloseButton;
    property UseCustomOptions: Boolean
      read FUseCustomOptions write SetUseCustomOptions;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscGPVertTabControlTabs = class(TCollection)
  private
    function GetItem(Index: Integer):  TscGPVertTabControlTab;
    procedure SetItem(Index: Integer; Value:  TscGPVertTabControlTab);
  protected
    TabControl: TscGPVertTabControl;
    DestroyTab: TscGPVertTabControlTab;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ATabControl: TscGPVertTabControl);
    function Add: TscGPVertTabControlTab;
    function Insert(Index: Integer): TscGPVertTabControlTab;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TscGPVertTabControlTab read GetItem write SetItem; default;
  end;


  TscGPVertTabControl = class(TscCustomControl)
  private
    FTabsScaling: Boolean;
    FTabsBGFillColor: TColor;
    FTabsBGFillColorAlpha: Byte;
    FFrameWidth: Integer;
    FFrameScaleWidth: Boolean;
    FFrameColor: TColor;
    FFrameColorAlpha: Byte;
    FTabOptions: TscGPTabOptions;
    FShowCloseButtons: Boolean;
    FBorderStyle: TscGPVertTabControlBorderStyle;
    FMouseWheelSupport: Boolean;
    FShowInActiveTab: Boolean;
    FScrollOffset: Integer;
    FScrollVisible: Boolean;
    FOldHeight: Integer;
    FMouseIn: Boolean;
    FTabIndex: Integer;
    FTabIndexBeforeFocused, FTabIndexAfterFocused: Integer;
    FTabs: TscGPVertTabControlTabs;
    FLeftOffset, FRightOffset: Integer;
    FOldTabActive, FTabActive: Integer;
    FTabWidth: Integer;
    FTabHeight: Integer;
    FTabImages: TCustomImageList;
    FOnChangeTab: TNotifyEvent;
    FOnChangingTab: TNotifyEvent;
    FOnCanChangeTab: TscCanChangePageEvent;

    FTabSpacing,
    FTabMargin: Integer;

    FTabGlowEffect: TscButtonGlowEffect;

    FTabsLeftOffset,
    FTabsBottomOffset, FTabsTopOffset: Integer;
    FTopScrollButton, FBottomScrollButton: TscGPVertTabScrollButton;
    FTopTabIndex, FBottomTabIndex: Integer;
    FShowFocusRect: Boolean;
    FDeleteOnClose: Boolean;
    FOnClose: TscTabCloseEvent;
    FOnGetTabDrawParams: TscGPGetAdvTabDrawParamsEvent;
    FScrollButtonHeight: Integer;
    FCloseButtonSize: Integer;

    FTabsWallpapers: TscCustomImageCollection;
    FTabsWallpaperIndex: Integer;
    FTabsWallpaper2Index: Integer;

    procedure SetTabsWallpaperIndex(Value: Integer);
    procedure SetTabsWallpaper2Index(Value: Integer);
    procedure SetTabsWallpapers(Value: TscCustomImageCollection);

    procedure SetTabsBGFillColor(Value: TColor);
    procedure SetTabsBGFillColorAlpha(Value: Byte);

    procedure SetFrameWidth(Value: Integer);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameColorAlpha(Value: Byte);

    procedure SetScrollButtonHeight(Value: Integer);
    procedure SetShowCloseButtons(Value: Boolean);
    procedure SetBorderStyle(Value: TscGPVertTabControlBorderStyle);
    procedure SetShowInActiveTab(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetTabsTopOffset(Value: Integer);
    procedure SetTabsLeftOffset(Value: Integer);
    procedure SetTabsBottomOffset(Value: Integer);
    procedure SetTabSpacing(Value: Integer);
    procedure SetTabMargin(Value: Integer);
    procedure SetTabs(AValue: TscGPVertTabControlTabs);
    function GetPageBoundsRect: TRect;
    function TabFromPoint(P: TPoint): Integer;
    procedure SetTabIndex(Value: Integer);
    procedure SetTabHeight(Value: Integer);
    procedure SetTabWidth(Value: Integer);
    procedure SetTabImages(Value: TCustomImageList);
    procedure OnControlChange(Sender: TObject);
    procedure ShowScrollButtons;
    procedure HideScrollButtons;
    procedure OnTopScrollButtonClick(Sender: TObject);
    procedure OnBottomScrollButtonClick(Sender: TObject);
    procedure AdjustScrollButtons;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure DrawCloseButton(ACanvas: TCanvas; G: TGPGraphics; ARect: TRect; AIndex: Integer; AColor: TColor);
    procedure ScrollToTop;
    procedure ScrollToBottom;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure UpdateControls; override;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL); message WM_MOUSEWHEEL;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure TestActive(X, Y: Integer);
    procedure Loaded; override;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure CalcTabRects;
    procedure DrawTab(ACanvas: TCanvas; G: TGPGraphics; Index: Integer; AFirst: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure GetScrollInfo;

    procedure ScrollToTab(AIndex: Integer);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;

    function FindDrawNextTabFromIndex(AIndex: Integer): Integer;
    function FindDrawPriorTabFromIndex(AIndex: Integer): Integer;

  public
    procedure DoClose;
    procedure FindNextTab;
    procedure FindPriorTab;
    procedure FindFirstTab;
    procedure FindLastTab;
    function FindNextTabFromIndex(AIndex: Integer): Integer;
    function FindPriorTabFromIndex(AIndex: Integer): Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTabs;
  published
    property Align;
    property Font;
    property Color;
    property DrawTextMode;
    property TabsBGFillColor: TColor
      read FTabsBGFillColor write SetTabsBGFillColor;
    property TabsBGFillColorAlpha: Byte
      read FTabsBGFillColorAlpha write SetTabsBGFillColorAlpha;

    property TransparentBackground;
    property FrameWidth: Integer
      read FFrameWidth write SetFrameWidth;
    property FrameScaleWidth: Boolean
      read FFrameScaleWidth write FFrameScaleWidth;
    property FrameColor: TColor
      read FFrameColor write SetFrameColor;
    property FrameColorAlpha: Byte
      read FFrameColorAlpha write SetFrameColorAlpha;
    property BorderStyle: TscGPVertTabControlBorderStyle
      read FBorderStyle write SetBorderStyle;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;
    property ShowInActiveTab: Boolean read FShowInactiveTab write SetShowInActiveTab;
    property ShowCloseButtons: Boolean read FShowCloseButtons write SetShowCloseButtons;
    property TabsLeftOffset: Integer
      read FTabsLeftOffset write SetTabsLeftOffset;
    property TabsBottomOffset: Integer
      read FTabsBottomOffset write SetTabsBottomOffset;
    property TabsTopOffset: Integer
      read FTabsTopOffset write SetTabsTopOffset;
    property TabGlowEffect: TscButtonGlowEffect read FTabGlowEffect write FTabGlowEffect;
    property TabOptions: TscGPTabOptions read FTabOptions write FTabOptions;

    property TabSpacing: Integer read FTabSpacing write SetTabSpacing;
    property TabMargin: Integer read FTabMargin write SetTabMargin;

    property ScrollButtonHeight: Integer
      read FScrollButtonHeight write SetScrollButtonHeight;

    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property TabHeight: Integer read FTabHeight write SetTabHeight;
    property Tabs: TscGPVertTabControlTabs read FTabs write SetTabs;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property TabImages: TCustomImageList
      read FTabImages write SetTabImages;

    property StorePaintBuffer;

    property DeleteOnClose: Boolean read FDeleteOnClose write FDeleteOnClose;

    property TabsWallpapers: TscCustomImageCollection read FTabsWallpapers write SetTabsWallpapers;
    property TabsWallpaperIndex: Integer read FTabsWallpaperIndex write SetTabsWallpaperIndex;
    property TabsWallpaper2Index: Integer read FTabsWallpaper2Index write SetTabsWallpaper2Index;

    property OnChangingTab: TNotifyEvent read FOnChangingTab write FOnChangingTab;
    property OnChangeTab: TNotifyEvent read FOnChangeTab write FOnChangeTab;
    property OnCanChangeTab: TscCanChangePageEvent
      read FOnCanChangeTab write FOnCanChangeTab;
    property OnClose: TscTabCloseEvent read FOnClose write FOnClose;
    property OnGetTabDrawParams: TscGPGetAdvTabDrawParamsEvent
      read FOnGetTabDrawParams write FOnGetTabDrawParams;
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

  Uses System.Types, System.Math, System.UITypes, Vcl.Clipbrd,
    Vcl.Dialogs, Vcl.Menus;

const
  DefPagerTabWidth = 150;
  DefPagerTabHeight = 40;
  DefDividerTabHeight = 20;
  TAB_CLOSE_SIZE = 16;

constructor TscGPVertPageControlTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCustomOptions := TscGPTabOptions.Create;
  FCustomOptions.OnChange := OnOptionsChange;
  FCustomGlowEffect := TscButtonGlowEffect.Create;
  FCustomGlowEffect.States := [scsFocused];
  FCustomGlowEffect.OnChange := OnOptionsChange;
  FShowCloseButton := True;
  FUseCustomOptions := False;
  FCustomFrameColor := clNone;
  FCustomFrameColorAlpha := 255;
  Active := False;
  CloseButtonMouseIn := False;
  CloseButtonMouseDown := False;
  CloseButtonRect := Rect(0, 0, 0, 0);
  FEnabled := True;
  FVisible := True;
  FPage := nil;
  FCaption := 'TscGPVertPageControlTab' + IntToStr(Index + 1);
  FImageIndex := -1;
  if (TscGPVertPageControlTabs(Collection).Pager <> nil) and
     (csDesigning in  TscGPVertPageControlTabs(Collection).Pager.ComponentState) and
      not (csLoading in TscGPVertPageControlTabs(Collection).Pager.ComponentState)
  then
  begin
    FPage := TscGPVertPageControlTabs(Collection).Pager.CreatePage;
    TscGPVertPageControlTabs(Collection).Pager.ActivePage := FPage;
  end;
end;

destructor TscGPVertPageControlTab.Destroy;
begin
  if (TscGPVertPageControlTabs(Collection).Pager <> nil)
     and (csDesigning in  TscGPVertPageControlTabs(Collection).Pager.ComponentState)
     and not (csLoading in  TscGPVertPageControlTabs(Collection).Pager.ComponentState)
     and (FPage <> nil)
     and not (csDestroying in TscGPVertPageControlTabs(Collection).Pager.ComponentState)
  then
    TscGPVertPageControlTabs(Collection).DestroyPage := FPage;
  FCustomOptions.Free;
  FCustomGlowEffect.Free;
  inherited;
end;

procedure TscGPVertPageControlTab.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TscGPVertPageControlTab
  then
    begin
      FPage := TscGPVertPageControlTab(Source).Page;
      FCaption := TscGPVertPageControlTab(Source).Caption;
      FImageIndex := TscGPVertPageControlTab(Source).ImageIndex;
      FVisible := TscGPVertPageControlTab(Source).Visible;
      FEnabled := TscGPVertPageControlTab(Source).Enabled;
      FShowCloseButton := TscGPVertPageControlTab(Source).ShowCloseButton;
      FUseCustomOptions := TscGPVertPageControlTab(Source).UseCustomOptions;
      FCustomOptions.Assign(TscGPVertPageControlTab(Source).CustomOptions);
      FCustomFrameColor := TscGPVertPageControlTab(Source).CustomFrameColor;
      FCustomFrameColorAlpha := TscGPVertPageControlTab(Source).CustomFrameColorAlpha;
    end
end;

procedure TscGPVertPageControlTab.OnOptionsChange(Sender: TObject);
begin
  Changed(False);
end;

procedure TscGPVertPageControlTab.SetShowCloseButton(Value: Boolean);
begin
  if FShowCloseButton <> Value then
  begin
    FShowCloseButton := Value;
    Changed(False);
  end;
end;

procedure TscGPVertPageControlTab.SetCustomFrameColor(Value: TColor);
begin
  if FCustomFrameColor <> Value then
  begin
    FCustomFrameColor := Value;
    Changed(False);
  end;
end;

procedure TscGPVertPageControlTab.SetCustomFrameColorAlpha(Value: Byte);
begin
  if FCustomFrameColorAlpha <> Value then
  begin
    FCustomFrameColorAlpha := Value;
    Changed(False);
  end;
end;

procedure TscGPVertPageControlTab.SetUseCustomOptions(Value: Boolean);
begin
  if FUseCustomOptions <> Value then
  begin
    FUseCustomOptions := Value;
    Changed(False);
  end;
end;

procedure TscGPVertPageControlTab.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscGPVertPageControlTab.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value
  then
    begin
      FEnabled := Value;
      Changed(False);
    end;
end;

procedure TscGPVertPageControlTab.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value
  then
    begin
      FImageIndex := Value;
      Changed(False);
    end;
end;

procedure TscGPVertPageControlTab.SetVisible(Value: Boolean);
var
  B: Boolean;
  i, j: Integer;
  FPager: TscGPVertPageControl;
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
    FPager := TscGPVertPageControlTabs(Collection).Pager;
    if (FPager <> nil) and not FVisible
       and not (csLoading in FPager.ComponentState)
    then
    begin
      j := Index;
      B := False;
      if j < FPager.Tabs.Count then
        for i := j to FPager.Tabs.Count - 1 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
             begin
               FPager.FTabIndex := -1;
               FPager.TabIndex := i;
               B := True;
               Break;
             end;
         end;

      if not B and (j >= 0) then
        for i := j downto 0 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
            begin
              FPager.FTabIndex := -1;
              FPager.TabIndex := i;
              Break;
            end;
        end;
       if FPage <> nil then FPage.Visible := False;
       FPager.AdjustScrollButtons;
     end;
  end;
end;

procedure TscGPVertPageControlTab.SetPage(const Value: TscGPVertPageControlPage);
begin
  if FPage <> Value then
  begin
    FPage := Value;
    if (FPage <> nil) and (FPage.Pager <> nil) then
      FPage.Pager.ActivePage := FPage;
  end;
end;

constructor TscGPVertPageControlTabs.Create;
begin
  inherited Create(TscGPVertPageControlTab);
  Pager := APager;
  DestroyPage := nil;
end;

function TscGPVertPageControlTabs.GetOwner: TPersistent;
begin
  Result := Pager;
end;

function TscGPVertPageControlTabs.Add: TscGPVertPageControlTab;
begin
  Result := TscGPVertPageControlTab(inherited Add);
  if (Pager <> nil)
     and not (csDesigning in Pager.ComponentState)
     and not (csLoading in Pager.ComponentState)
  then
  begin
    Result.Page := Pager.CreatePage;
    Pager.ActivePage := Result.Page;
  end;

  if (Pager <> nil) and
     not (csLoading in Pager.ComponentState) then
  begin
    Pager.FScrollOffset := 0;
    Pager.GetScrollInfo;
    Pager.AdjustScrollButtons;
    Pager.ScrollToTab(Pager.TabIndex);
  end;
end;

function TscGPVertPageControlTabs.Insert(Index: Integer): TscGPVertPageControlTab;
begin
  Result := TscGPVertPageControlTab(inherited Insert(Index));
  if (Pager <> nil)
     and not (csDesigning in Pager.ComponentState)
     and not (csLoading in Pager.ComponentState)
  then
  begin
    Result.Page := Pager.CreatePage;
    Pager.FScrollOffset := 0;
    Pager.GetScrollInfo;
    Pager.AdjustScrollButtons;
  end;
end;

procedure TscGPVertPageControlTabs.Delete(Index: Integer);
begin
   if (Pager <> nil)
      and not (csDesigning in Pager.ComponentState)
      and not (csLoading in Pager.ComponentState)
      and (Items[Index].FPage <> nil)
  then
    FreeAndNil(Items[Index].FPage);
  inherited Delete(Index);
  if (Pager <> nil) and
     not (csLoading in Pager.ComponentState) then
  begin
    if Pager.TabIndex > Index then
      Dec(Pager.FTabIndex);
    Pager.FScrollOffset := 0;
    Pager.CalcTabRects;
    Pager.GetScrollInfo;
    Pager.ScrollToTab(Pager.TabIndex);
  end;
end;

procedure TscGPVertPageControlTabs.Update(Item: TCollectionItem);
var
  F: TCustomForm;
begin
  inherited;
  if Pager = nil then
    Exit;

  if (DestroyPage <> nil) and
     (csDesigning in Pager.ComponentState) and
     not (csLoading in  Pager.ComponentState) and
     not (csDestroying in Pager.ComponentState)
  then
  begin
    FreeAndNil(DestroyPage);
    F := GetParentForm(Pager);
    if F <> nil then
      F.Designer.Modified;
  end;

  Pager.UpdateTabs;
end;

function TscGPVertPageControlTabs.GetItem(Index: Integer):  TscGPVertPageControlTab;
begin
  Result := TscGPVertPageControlTab(inherited GetItem(Index));
end;

procedure TscGPVertPageControlTabs.SetItem(Index: Integer; Value:  TscGPVertPageControlTab);
begin
  inherited SetItem(Index, Value);
end;

constructor TscGPVertPageControlPage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  Color := clBtnFace;
  BackgroundStyle := scsbsFormBackground;
  FBGStyle := scgppsForm;
  FDestroyFromPager := False;
  BorderStyle := bsNone;
  AutoScroll := False;
  FullUpdate := False;
  ParentFont := False;
  ParentColor := False;
end;

destructor TscGPVertPageControlPage.Destroy;
var
  i, j: Integer;
  B: Boolean;
begin
  if (Pager <> nil) and not FDestroyFromPager
     and not (csLoading in Pager.ComponentState)
     and not (csDestroying in Pager.ComponentState)
  then
    begin
      j := Pager.GetPageIndex(Self);
      if j <> -1
      then
        begin
          Pager.Tabs[j].Page := nil;
          Pager.Tabs.Delete(j);
          if Pager.TabIndex = j
          then
            begin
              B := False;
              if j < Pager.Tabs.Count then
              for i := j to Pager.Tabs.Count - 1 do
              begin
                if (i >= 0) and (i < Pager.Tabs.Count) then
                if Pager.Tabs[i].Visible and Pager.Tabs[i].Enabled
                then
                  begin
                    Pager.FTabIndex := -1;
                    Pager.TabIndex := i;
                    B := True;
                    Break;
                  end;
              end;
              if not B and (j >= 0)
              then
                for i := j downto 0 do
                begin
                  if (i >= 0) and (i < Pager.Tabs.Count) then
                  if Pager.Tabs[i].Visible and Pager.Tabs[i].Enabled
                  then
                    begin
                      Pager.FTabIndex := -1;
                      Pager.TabIndex := i;
                      Break;
                    end;
                end;
            end;
          Pager.FScrollOffset := 0;
          Pager.CalcTabRects;
          Pager.AdjustScrollButtons;
          Pager.ScrollToTab(Pager.TabIndex);
          Pager.RePaintControl;
        end
      else
        begin
          if Pager.TabIndex > Pager.Tabs.Count - 1
          then
            Pager.TabIndex := Pager.Tabs.Count - 1
          else
            Pager.TabIndex := Pager.TabIndex;
          Pager.FScrollOffset := 0;
          Pager.CalcTabRects;
          Pager.AdjustScrollButtons;
          Pager.ScrollToTab(Pager.TabIndex);
          Pager.RePaintControl;
        end;
    end;
  inherited;
end;

procedure TscGPVertPageControlPage.SetBGStyle(Value: TscGPPageBGStyle);
begin
  if Value <> FBGStyle then
  begin
    FBGStyle := Value;
    case FBGStyle of
      scgppsForm:
        BackgroundStyle := scsbsFormBackground;
      scgppsColor:
        BackgroundStyle := scsbsPanel;
    end;
  end;
end;

procedure TscGPVertPageControlPage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if (Parent <> nil) and (Pager <> nil)
  then
    begin
      R := Pager.GetPageBoundsRect;
      inherited SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    end
  else
    inherited;
end;

constructor TscGPVertTabScrollButton.Create(AOwner: TComponent);
begin
  inherited;
  FMinMargins := True;
  FTransparentBackground := False;
  Options.FrameNormalColor := clNone;
  Options.FrameNormalColorAlpha := 0;
  Options.FrameNormalColor := clNone;
  Options.FrameNormalColorAlpha := 0;
  Options.FrameHotColor := clNone;
  Options.FrameHotColorAlpha := 0;
  Options.FramePressedColor := clNone;
  Options.FramePressedColorAlpha := 0;
  Options.NormalColor := clBtnText;
  Options.NormalColorAlpha := 15;
  Options.HotColor := clBtnText;
  Options.HotColorAlpha := 40;
  Options.PressedColor := clBtnText;
  Options.PressedColorAlpha := 60;
end;

procedure TscGPVertTabScrollButton.CMDesignHitTest;
begin
  Message.Result := 1;
end;

procedure TscGPVertTabScrollButton.WMLButtonUp(var Msg: TWMMouse);
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    if Parent is TscGPVertTabControl then
    begin
      if FBottom then
        TscGPVertTabControl(Parent).ScrollToBottom
      else
        TscGPVertTabControl(Parent).ScrollToTop;
    end;
  end;
end;

constructor TscGPVertPageControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FTabsScaling := False;
  Color := clBtnFace;
  FTabsBGFillColor := clNone;
  FTabsBGFillColorAlpha := 255;
  FTabsWallpapers := nil;
  FTabsWallpaperIndex := -1;
  FTabsWallpaper2Index := -1;
  ParentBackground := False;
  ParentColor := False;
  FTabs := TscGPVertPageControlTabs.Create(Self);
  FTabOptions := TscGPTabOptions.Create;
  FTabOptions.GradientColorOffset := 5;
  FTabOptions.ShapeFillGradientAngle := 0;
  FTabOptions.OnChange := OnControlChange;
  FFrameWidth := 2;
  FFrameScaleWidth := False;
  FFrameColor := clBtnText;
  FFrameColorAlpha := 80;
  FTabIndex := -1;
  FScrollButtonHeight := 20;
  FCloseButtonSize := TAB_CLOSE_SIZE;
  FBorderStyle := scgpvbsFrame;
  FShowInactiveTab := True;
  FTabGlowEffect := TscButtonGlowEffect.Create;
  FTabGlowEffect.States := [scsFocused];
  FTabGlowEffect.OnChange := OnControlChange;
  FMouseWheelSupport := True;
  FShowCloseButtons := False;
  FTabMargin := 10;
  FTabSpacing := 10;
  FFreeOnClose := False;
  FTabWidth := DefPagerTabWidth;
  FTabHeight := DefPagerTabHeight;
  FTabImages := nil;
  FTransparentBackground := False;

  FMouseIn := False;
  FScrollOffset := 0;
  FLeftOffset := 6;
  FRightOffset := 5;
  Width := 300;
  Height := 150;
  FOldTabActive := -1;
  FTabActive := -1;
  FOldHeight := -1;
  FTabsTopOffset := 15;
  FTabsLeftOffset := 0;
  FTabsBottomOffset := 15;
  FTopScrollButton := nil;
  FBottomScrollButton := nil;
  FShowFocusRect := True;
end;

destructor TscGPVertPageControl.Destroy;
begin
  FTabOptions.Free;
  FTabGlowEffect.Free;
  FTabs.Free;
  FTabs := nil;
  inherited;
end;

procedure TscGPVertPageControl.SetTabsWallpapers(Value: TscCustomImageCollection);
begin
  if FTabsWallpapers <> Value then
  begin
    FTabsWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertPageControl.SetTabsWallpaperIndex(Value: Integer);
begin
  if FTabsWallpaperIndex <> Value then
  begin
    FTabsWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertPageControl.SetTabsWallpaper2Index(Value: Integer);
begin
  if FTabsWallpaper2Index <> Value then
  begin
    FTabsWallpaper2Index := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertPageControl.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  if FFrameScaleWidth then
  begin
    FFrameWidth := MulDiv(FFrameWidth, M, D);
    FTabOptions.FrameWidth := MulDiv(FTabOptions.FrameWidth, M, D);
  end;
  FTabOptions.ShapeCornerRadius := MulDiv(FTabOptions.ShapeCornerRadius, M, D);
  FCloseButtonSize := MulDiv(FCloseButtonSize, M, D);
  FTabMargin := MulDiv(FTabMargin, M, D);
  FScrollButtonHeight := MulDiv(FScrollButtonHeight, M, D);
  FTabHeight := MulDiv(FTabHeight, M, D);
  TabWidth := MulDiv(FTabWidth, M, D);
  FTabsTopOffset := MulDiv(FTabsTopOffset, M, D);
  FTabsLeftOffset := MulDiv(FTabsLeftOffset, M, D);
  FTabsBottomOffset := MulDiv(FTabsBottomOffset, M, D);

  if not (csLoading in ComponentState) then
    FTabsScaling := True;

  inherited;
  SetTimer(Handle, 1, 100, nil);
end;

procedure TscGPVertPageControl.SetTabsBGFillColor(Value: TColor);
begin
  if FTabsBGFillColor <> Value then
  begin
    FTabsBGFillColor := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertPageControl.SetTabsBGFillColorAlpha(Value: Byte);
begin
  if FTabsBGFillColorAlpha <> Value then
  begin
    FTabsBGFillColorAlpha := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertPageControl.SetScrollButtonHeight(Value: Integer);
begin
  if (Value >= 20) and (FScrollButtonHeight <> Value) then
  begin
    FScrollButtonHeight := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.DoClose;
var
  FPage: TscGPVertPageControlPage;
  CanClose: Boolean;
  P: TPoint;
  PIndex: Integer;
begin
  FPage := FActivePage;
  if FPage = nil then Exit;
  CanClose := True;
  if Assigned(FOnClose) then FOnClose(FPage, CanClose);
  if CanClose then
  begin
    PIndex := GetPageIndex(FPage);
    FScrollOffset := 0;
    FTabs[PIndex].Visible := False;
    if FFreeOnClose then
    begin
      FTabs[PIndex].Page.FDestroyFromPager := True;
      FTabs.Delete(PIndex);
    end;
  end;
  if CanClose = False then
  begin
    GetCursorPos(P);
    if (WinApi.Windows.WindowFromPoint(P) <> Self.Handle) and (FTabActive <> -1) then
    begin
      FTabs[FTabActive].CloseButtonMouseIn := False;
      FTabs[FTabActive].CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;
end;

procedure TscGPVertPageControl.DrawCloseButton(ACanvas: TCanvas;
  G: TGPGraphics;
  ARect: TRect; AIndex: Integer;  AColor: TColor);
var
  X, Y: Integer;
  ButtonR: TRect;
  GlyphColor, FillColor: Cardinal;
  R: TGPRectF;
  B: TGPSolidBrush;
begin
  X := ARect.Left + ARect.Width div 2 - FCloseButtonSize div 2;
  Y := ARect.Top + ARect.Height div 2 - FCloseButtonSize div 2 + FFrameWidth;
  ButtonR := Rect(X, Y, X + FCloseButtonSize, Y + FCloseButtonSize);
  R := RectToGPRect(ButtonR);
  Tabs[AIndex].CloseButtonRect := ButtonR;
  FillColor := 0;
  if not Tabs[AIndex].Enabled then
  begin
    GlyphColor := ColorToGPColor(GetStyleColor(clBtnText), 100);
  end
  else
  if Tabs[AIndex].CloseButtonMouseDown then
  begin
    FillColor := ColorToGPColor(clRed, 200);
    GlyphColor := ColorToGPColor(clWhite, 200);
  end
  else
  if Tabs[AIndex].CloseButtonMouseIn then
  begin
    FillColor := ColorToGPColor(clRed, 220);
    GlyphColor := ColorToGPColor(clWhite, 255);
  end
  else
    GlyphColor := ColorToGPColor(GetStyleColor(clBtnText), 200);
  if FillColor <> 0 then
  begin
    B := TGPSolidBrush.Create(FillColor);
    G.FillEllipse(B, R);
    B.Free;
  end;
  InflateGPRect(R, -FCloseButtonSize div 4, -FCloseButtonSize div 4);
  scGPUtils.GPDrawClearGlyph
    (G, R, GlyphColor, FScaleFactor, 2);
end;

procedure TscGPVertPageControl.SetShowCloseButtons(Value: Boolean);
begin
  if FShowCloseButtons <> Value then
  begin
    FShowCloseButtons := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.SetFrameWidth(Value: Integer);
begin
  if Value <> FFrameWidth then
  begin
    FFrameWidth := Value;
    RePaintControl;
    if FActivePage <> nil then
      FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
        FActivePage.Width, FActivePage.Height);
  end;
end;

procedure TscGPVertPageControl.SetFrameColor(Value: TColor);
begin
  if Value <> FFrameColor then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.SetFrameColorAlpha(Value: Byte);
begin
  if Value <> FFrameColorAlpha then
  begin
    FFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.SetTabsTopOffset(Value: Integer);
begin
  if Value <> FTabsTopOffset then
  begin
    FTabsTopOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscGPVertPageControl.SetBorderStyle(Value: TscGPVertTabControlBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RePaintControl;
    if FActivePage <> nil then
      FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
        FActivePage.Width, FActivePage.Height);
  end;
end;

procedure TscGPVertPageControl.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[i] is TWinControl) and not (Controls[i] is TscGPVertPageControlPage)
    then
      SendMessage(TWinControl(Controls[I]).Handle, WM_CHECKPARENTBG, 0, 0)
    else
    if Controls[i] is TGraphicControl
     then
       TGraphicControl(Controls[I]).Perform(WM_CHECKPARENTBG, 0, 0);
  end;
end;

procedure TscGPVertPageControl.SetTabsLeftOffset(Value: Integer);
begin
  if (Value <> FTabsLeftOffset) and (Value >= 0) then
  begin
    FTabsLeftOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
    if FActivePage <> nil then
      FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
        FActivePage.Width, FActivePage.Height);
  end;
end;

procedure TscGPVertPageControl.SetShowInActiveTab(Value: Boolean);
begin
  if Value <> FShowInActiveTab then
  begin
    FShowInActiveTab := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.SetTabsBottomOffset(Value: Integer);
begin
  if (Value <> FTabsBottomOffset) and (Value >= 0) then
  begin
    FTabsBottomOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscGPVertPageControl.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPVertPageControl.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  TabsRect: TRect;
  PageRect: TRect;
  FFirst: Boolean;
  FFirstVisible: Boolean;
  I: Integer;
  SaveIndex: Integer;
  G: TGPGraphics;
  P: TGPPen;
  C: Cardinal;
  R, R1: TGPRectF;
  H: Integer;
  B: TGPSolidBrush;
begin
  if BidiMode <> bdRightToLeft then
  begin
    TabsRect := Rect(0, 0, FTabWidth + FTabsLeftOffset, Height);
    PageRect := Rect(FTabWidth + FTabsLeftOffset, 0, Width, Height);
  end
  else
  begin
    TabsRect := Rect(Width - FTabWidth - FTabsLeftOffset, 0, Width, Height);
    PageRect := Rect(0, 0, Width - FTabWidth - FTabsLeftOffset, Height);
  end;

  // draw background
  if not FTransparentBackground then
    with ACanvas do
    begin
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(PageRect);
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(TabsRect);
    end;

  if (FTabsWallpapers <> nil) and  (FTabsWallpaperIndex <> -1) and
     FTabsWallpapers.IsIndexAvailable(FTabsWallpaperIndex) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle,
        TabsRect.Left, TabsRect.Top,
        TabsRect.Right, TabsRect.Bottom);
      FTabsWallpapers.Draw(ACanvas, TabsRect, FTabsWallpaperIndex, FScaleFactor);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  // draw border
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  if (FTabsBGFillColor <> clNone) and (FTabsBGFillColorAlpha > 0) then
  begin
    C := ColorToGPColor(GetStyleCOlor(FTabsBGFillColor), FTabsBGFillColorAlpha);
    B := TGPSolidBrush.Create(C);
    R := RectToGPRect(TabsRect);
    G.FillRectangle(B, R);
    B.Free;
  end;

  if (FTabsWallpapers <> nil) and (FTabsWallpaper2Index <> -1)  and
    FTabsWallpapers.IsIndexAvailable(FTabsWallpaper2Index) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle,
        TabsRect.Left, TabsRect.Top,
        TabsRect.Right, TabsRect.Bottom);
      FTabsWallpapers.Draw(ACanvas, TabsRect, FTabsWallpaper2Index, FScaleFactor);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  if (FTabIndex <> -1) and (FTabIndex < FTabs.Count) and (FTabs[FTabIndex].CustomFrameColor <> clNone) then
    C := ColorToGPColor(GetStyleColor(FTabs[FTabIndex].CustomFrameColor), FTabs[FTabIndex].CustomFrameColorAlpha)
  else
    C := ColorToGPColor(GetStyleColor(FFrameColor), FFrameColorAlpha);

  P := TGPPen.Create(C, FFrameWidth);
  if not FGetControlBG then
    CalcTabRects;

  if (FBorderStyle <> scgpvbsNone) and not FGetControlBG then
  begin
    if (FTabIndex <> -1) and (FTabIndex < FTabs.Count) and FTabs[FTabIndex].Visible then
    begin
      if FScrollVisible then
        H := FTabsTopOffset + FScrollButtonHeight
      else
        H := FTabsTopOffset;
      R1 := RectToGPRect(FTabs[FTabIndex].TabRect);

      if FBorderStyle <> scgpvbsNone then
      begin
        R1.Width := R1.Width + FFrameWidth;
        if BidiMode = bdRightToLeft then
          R1.X := R1.X - FFrameWidth;
      end;

      if R1.Y < H then
      begin
        R1.Height := R1.Height - (H - R1.Y);
        R1.Y := H;
      end;

      if FScrollVisible then
        H := FTabsBottomOffset + FScrollButtonHeight
      else
        H := FTabsBottomOffset;

      if R1.Y + R1.Height > Height - H then
      begin
        R1.Height := R1.Height - (R1.Y + R1.Height - (Height - H));
      end;
      if (R1.Height > 0) and (R1.Y <= Height - H) then
        G.ExcludeClip(R1);
    end;
    R := RectToGPRect(PageRect);
    InflateGPRect(R, -FFrameWidth / 2, -FFrameWidth / 2);
    if (FBorderStyle = scgpvbsLine) or (FBorderStyle = scgpvbsLineLeftRight) then
    begin
      R.Y := 0;
      R.Height := Height;
    end;
    if FBorderStyle = scgpvbsFrame then
      G.DrawRectangle(P, R)
    else
    if (FBorderStyle = scgpvbsLine) or (FBorderStyle = scgpvbsLineLeftRight) then
    begin
      if BidiMode <> bdRightToLeft then
      begin
        G.DrawLine(P, R.X, R.Y, R.X, R.Y + R.Height);
        if FBorderStyle = scgpvbsLineLeftRight then
          G.DrawLine(P, R.X + R.Width, R.Y, R.X + R.Width, R.Y + R.Height);
      end
      else
      begin
        G.DrawLine(P, R.X + R.Width, R.Y, R.X + R.Width, R.Y + R.Height);
        if FBorderStyle = scgpvbsLineLeftRight then
          G.DrawLine(P, R.X, R.Y, R.X, R.Y + R.Height);
      end;
    end;
    if (FTabIndex <> -1) and (R1.Height > 0) and (R1.Y <= Height - FTabsBottomOffset) then
      G.ResetClip;
  end;

  // draw items

  FTabIndexBeforeFocused := FindDrawPriorTabFromIndex(FTabIndex);
  FTabIndexAfterFocused := FindDrawNextTabFromIndex(FTabIndex);

  SaveIndex := SaveDC(ACanvas.Handle);
  try
    if not FGetControlBG then
    begin
      if BidiMode <> bdRightToLeft then
        IntersectClipRect(ACanvas.Handle,
          FTabsLeftOffset, FTabsTopOffset, FTabsLeftOffset + FTabWidth + 2,  Height - FTabsBottomOffset)
      else
        IntersectClipRect(ACanvas.Handle,
          Width - FTabsLeftOffset - FTabWidth - 2, FTabsTopOffset,
            Width - FTabsLeftOffset,  Height - FTabsBottomOffset);

      FFirstVisible := False;
      for I := 0 to FTabs.Count - 1  do
      begin
        if FTabs[I].Visible then
        begin
          FFirst := (FTabsLeftOffset = 0) and (FTabs[I].TabRect.Left = 0);
          if not FFirstVisible and (I = FTabIndex) and not FTabs[I].Enabled then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FFirstVisible and (I <> FTabIndex) then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FShowInActiveTab then
            FFirst := (I <> FTabIndex);
          if (I = TabIndex) and FTabs[I].Enabled then
            FFirstVisible := True;
          DrawTab(ACanvas, G, I, FFirst);
        end;
      end;
    end;
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;

  G.Free;
  P.Free;
end;

procedure TscGPVertPageControl.SetTabImages(Value: TCustomImageList);
begin
  if FTabImages <> Value then
  begin
    FTabImages := Value;
    UpdateTabs;
  end;
end;

procedure TscGPVertPageControl.SetTabWidth;
var
  I: Integer;
  R: TRect;
begin
  if FTabWidth <> Value then
  begin
    FTabWidth := Value;
    R := GetPageBoundsRect;
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TscGPVertPageControlPage then
      Controls[I].SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    RePaintControl;
    AdjustScrollButtons;
  end;
end;

procedure TscGPVertPageControl.SetTabHeight;
begin
  if FTabHeight <> Value then
  begin
    FTabHeight := Value;
    RePaintControl;
    AdjustScrollButtons;
  end;
end;

procedure TscGPVertPageControl.SetTabMargin(Value: Integer);
begin
  if (Value > 0) and (FTabMargin <> Value) then
  begin
    FTabMargin := Value;
    UpdateTabs;
  end;
end;

procedure TscGPVertPageControl.SetTabSpacing(Value: Integer);
begin
  if (Value > 0) and (FTabSpacing <> Value) then
  begin
    FTabSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.SetTabIndex;
var
  LPage: TscGPVertPageControlPage;
  LPrevTabIndex: Integer;
  B: Boolean;
begin
  if (Value < 0) or (Value > Tabs.Count - 1) then
    Exit;

  if Assigned(FOnCanChangePage) and not (csLoading in ComponentState) then
  begin
    B := True;
    FOnCanChangePage(Value, B);
    if not B then Exit;
  end;

  if not Tabs[Value].FVisible then Tabs[Value].FVisible := True;

  if (FTabIndex <> Value) and (Value >= 0) and (Value < Tabs.Count)
  then
    begin
      LPrevTabIndex := FTabIndex;
      FTabIndex := Value;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangingPage) then FOnChangingPage(Self);
      if (FTabIndex > -1) and (FTabs[FTabIndex].Page <> nil)
      then
        begin
          LPage := FTabs[FTabIndex].Page;
          LPage.Parent := Self;
          LPage.SetBounds(LPage.Left, LPage.Top, LPage.Width, LPage.Height);
          LPage.Visible := True;
          LPage.BringToFront;
          FActivePage := LPage;
          if FScrollVisible then
            ScrollToTab(FTabIndex);
        end;
      if (LPrevTabIndex > -1) and (FTabs.Count > LPrevTabIndex) and
         (FTabs[LPrevTabIndex].Page <> nil) and
         (FTabs[LPrevTabIndex].Page <> nil)
      then
        FTabs[LPrevTabIndex].Page.Visible := False;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangePage) then FOnChangePage(Self);
    end
  else
    begin
      if Tabs[Value].FPage <> nil
      then
      begin
        if not Tabs[Value].FPage.Visible then
          Tabs[Value].FPage.Visible := True;
        FActivePage := Tabs[Value].FPage;
      end;
    end;
  RePaintControl;
end;

function TscGPVertPageControl.TabFromPoint;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Tabs.Count -1 do
    if Tabs[i].Visible and PtInRect(Tabs[i].TabRect, P)
    then
      begin
        Result := i;
        Break;
      end;
end;

procedure  TscGPVertPageControl.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  AdjustScrollButtons;
end;

procedure TscGPVertPageControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateTabs;
end;

procedure TscGPVertPageControl.CMDesignHitTest;
var
  P: TPoint;
  I: Integer;
begin
  inherited;
  P := SmallPointToPoint(Message.Pos);
  if (Message.Keys = MK_LBUTTON) and (TabFromPoint(P) <> -1)
  then
    begin
      I := TabFromPoint(P);
      if Tabs[I].Page <> nil then ActivePage := Tabs[I].Page;
      GetParentForm(Self).Designer.Modified;
    end;
end;

procedure TscGPVertPageControl.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscGPVertPageControl.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FActivePage) then
    FActivePage := nil;
  if (Operation = opRemove) and (AComponent = FTabImages) then
    FTabImages := nil;
  if (Operation = opRemove) and (AComponent = FTabsWallpapers) then
    FTabsWallpapers := nil;
end;

function TscGPVertPageControl.FindNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscGPVertPageControl.FindPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscGPVertPageControl.FindDrawNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscGPVertPageControl.FindDrawPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

procedure TscGPVertPageControl.FindNextTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertPageControl.FindPriorTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertPageControl.FindFirstTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertPageControl.FindLastTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := Tabs.Count - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertPageControl.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    FTabsScaling := False;
    UpdateTabs;
    KillTimer(Handle, 1);
  end;
end;

procedure TscGPVertPageControl.WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL);
begin
  if FMouseWheelSupport then
    if Message.WheelDelta < 0 then FindNextTab else FindPriorTab;
end;

procedure TscGPVertPageControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) or (Key = VK_SPACE)
  then
    begin
      if (TabIndex <> -1) and (Tabs[TabIndex].Page = nil)
      then
        begin
          if Assigned(FTabs[FTabIndex].OnClick) then
            FTabs[FTabIndex].OnClick(Self);
        end;
    end
  else
  if (Key = VK_NEXT)
  then
    FindLastTab
  else
  if (Key = VK_PRIOR)
  then
   FindFirstTab
  else
  if (Key = VK_LEFT) or (Key = VK_UP)
  then
    FindPriorTab
  else
  if (Key = VK_RIGHT) or (Key = VK_DOWN)
  then
    FindNextTab;
end;

procedure TscGPVertPageControl.WMGetDlgCode;
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscGPVertPageControl.WMSETFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    if not FTransparentBackground then
      RePaintControl
    else
    begin
      FUpdateParentBuffer := True;
      if DrawTextMode = scdtmGDIPlus then
        Invalidate
      else
        RePaint;
    end;
end;

procedure TscGPVertPageControl.WMKILLFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    RePaintControl;
end;

procedure TscGPVertPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TestActive(X, Y);
end;

procedure TscGPVertPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
var
  WasFocused: Boolean;
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  WasFocused := Focused;

  if FTabActive <> TabIndex then TabIndex := FTabActive;

  if not WasFocused then SetFocus;

  if (FTabActive <> -1) and FShowCloseButtons and FTabs[FTabActive].ShowCloseButton
    and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := True;
        RePaintControl;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscGPVertPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  if (TabIndex <> -1) and (Tabs[TabIndex].Page = nil) and
     (TabIndex = FTabActive) then
  begin
    if Assigned(FTabs[FTabIndex].OnClick) then
        FTabs[FTabIndex].OnClick(Self);
  end;

  if (FTabActive <> -1) and FShowCloseButtons and FTabs[FTabActive].ShowCloseButton
    and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := False;
        RePaintControl;
        DoClose;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscGPVertPageControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TestActive(-1, -1);
end;

procedure TscGPVertPageControl.TestActive(X, Y: Integer);
var
  i: Integer;
begin
  if Tabs.Count = 0 then Exit;

  FOldTabActive:= FTabActive;
  FTabActive := -1;

  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled and PtInRect(Tabs[i].TabRect, Point(X, Y)) and
       (X >= FTabsLeftOffset) and (X < Width - FTabsBottomOffset)
    then
      begin
        FTabActive := i;
        Break;
      end;
  end;

  if (FTabActive <> FOldTabActive)
  then
    begin
      if (FOldTabActive <> - 1) and (FOldTabActive < Tabs.Count)
      then
      begin
        Tabs[FOldTabActive].Active := False;
        Tabs[FOldTabActive].CloseButtonMouseIn := False;
        Tabs[FOldTabActive].CloseButtonMouseDown := False;
      end;
      if (FTabActive <> - 1)
      then
        Tabs[FTabActive].Active := True;
      RePaintControl;
    end;

  if (FTabActive <> -1) and FShowCloseButtons and Tabs[FTabActive].ShowCloseButton then
  with Tabs[FTabActive] do
  begin
    if PtInRect(CloseButtonRect, Point(X, Y)) and not CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := True;
      RePaintControl;
    end
    else
    if not PtInRect(CloseButtonRect, Point(X, Y)) and CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := False;
      CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;

end;

procedure TscGPVertPageControl.ScrollToTab(AIndex: Integer);
var
  R: TRect;
  Offset, SH: Integer;
begin
  if (AIndex < 0) or (AIndex > Tabs.Count - 1) then Exit;

  R := Tabs[AIndex].TabRect;
  if FScrollVisible then
    SH := FScrollButtonHeight
  else
    SH := 0;
  if R.Top < FTabsTopOffset + SH then
  begin
    Offset := Abs(FTabsTopOffset - R.Top);
    Inc(Offset, SH);
    FScrollOffset := FScrollOffset - Offset;
    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end
  else
  if R.Bottom > Height - FTabsBottomOffset - SH then
  begin
    Offset := R.Bottom - (Height - FTabsBottomOffset);
    Inc(Offset, SH);
    FScrollOffset := FScrollOffset + Offset;
    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.ScrollToTop;
begin
  CalcTabRects;
  if FTopTabIndex >= 0 then
    ScrollToTab(FTopTabIndex);
end;

procedure TscGPVertPageControl.ScrollToBottom;
begin
  CalcTabRects;
  if FBottomTabIndex >= 0 then
    ScrollToTab(FBottomTabIndex);
end;

procedure TscGPVertPageControl.OnTopScrollButtonClick(Sender: TObject);
begin
  ScrollToTop;
end;

procedure TscGPVertPageControl.OnBottomScrollButtonClick(Sender: TObject);
begin
  ScrollToBottom;
end;

procedure TscGPVertPageControl.ShowScrollButtons;
var
  B: Boolean;
begin
  B := False;
  if FTopScrollButton = nil then
  begin
    FTopScrollButton := TscGPVertTabScrollButton.Create(Self);
    FTopScrollButton.Visible := False;
    FTopScrollButton.OnClick := OnTopScrollButtonClick;
    FTopScrollButton.RepeatClick := True;
    FTopScrollButton.CanFocused := False;
    FTopScrollButton.TransparentBackground := False;
    FTopScrollButton.GlyphOptions.Kind := scgpbgkUpArrow;
    FTopScrollButton.Parent := Self;
    if BidiMode <> bdRightToLeft then
      FTopScrollButton.SetBounds(FTabsLeftOffset, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight)
    else
    begin
      FTopScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight)
    end;
    FTopScrollButton.Visible := True;
    B := True;
  end
  else
  begin
    if BidiMode <> bdRightToLeft then
      FTopScrollButton.SetBounds(FTabsLeftOffset, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight)
    else
      FTopScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight);
  end;

  if FBottomScrollButton = nil then
  begin
    FBottomScrollButton := TscGPVertTabScrollButton.Create(Self);
    FBottomScrollButton.Visible := False;
    FBottomScrollButton.FBottom := True;
    FBottomScrollButton.OnClick := OnBottomScrollButtonClick;
    FBottomScrollButton.RepeatClick := True;
    FBottomScrollButton.CanFocused := False;
    FBottomScrollButton.TransparentBackground := False;
    FBottomScrollButton.GlyphOptions.Kind := scgpbgkDownArrow;
    FBottomScrollButton.Parent := Self;
    if BidiMode <> bdRightToLeft then
      FBottomScrollButton.SetBounds(FTabsLeftOffset, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight)
    else
      FBottomScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight);

    FBottomScrollButton.Visible := True;
    B := True;
  end
  else
  begin
   if BidiMode <> bdRightToLeft then
      FBottomScrollButton.SetBounds(FTabsLeftOffset, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight)
   else
     FBottomScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight)
  end;

  if B and not(csLoading in ComponentState) then
    RePaintControl;
end;

procedure TscGPVertPageControl.HideScrollButtons;
begin
  if FTopScrollButton <> nil then
  begin
    FTopScrollButton.Visible := False;
    FTopScrollButton.Free;
    FTopScrollButton := nil;
  end;
  if FBottomScrollButton <> nil then
  begin
    FBottomScrollButton.Visible := False;
    FBottomScrollButton.Free;
    FBottomScrollButton := nil;
  end;
end;

procedure TscGPVertPageControl.AdjustScrollButtons;
begin
  if FTabsScaling then
    Exit;

  if FScrollVisible then
    ShowScrollButtons
  else
    HideScrollButtons;
end;

procedure TscGPVertPageControl.GetScrollInfo;
var
  I, Y: Integer;
begin
  Y := FTabsTopOffset;
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      Y := Y + FTabHeight;
    end;
  FScrollVisible := Y > Height - FTabsBottomOffset;
end;

procedure TscGPVertPageControl.UpdateTabs;
begin
  if not (csLoading in ComponentState) and
     not (csDestroying in ComponentState) then
  begin
    FScrollOffset := 0;
    CalcTabRects;
    GetScrollInfo;
    ScrollToTab(FTabIndex);
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscGPVertPageControl.CalcTabRects;
var
  I, X, Y, ScrollH: Integer;
begin
  GetScrollInfo;
  Y := FTabsTopOffset - FScrollOffset;
  if FScrollVisible then
    Inc(Y, FScrollButtonHeight);

  if BidiMode <> bdRightToLeft then
    X := FTabsLeftOffset
  else
    X := Width - FTabWidth - FTabsLeftOffset;
  
  Canvas.Font := Self.Font;
  FTopTabIndex := -1;
  FBottomTabIndex := -1;

  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin

      Tabs[I].TabRect := Rect(X, Y, X + FTabWidth, Y + FTabHeight);
      Y := Y + FTabHeight;
      if FScrollVisible then
        ScrollH := FScrollButtonHeight
      else
        ScrollH := 0;
      if Tabs[I].TabRect.Top < FTabsTopOffset + ScrollH
      then
        FTopTabIndex := I;
      if (Tabs[I].TabRect.Bottom > Height - FTabsBottomOffset - ScrollH) and
         (FBottomTabIndex = -1)
      then
        FBottomTabIndex := I;
    end;
end;

procedure TscGPVertPageControl.SetTabs(AValue: TscGPVertPageControlTabs);
begin
  FTabs.Assign(AValue);
  RePaintControl;
end;

procedure TscGPVertPageControl.SetActivePage(const Value: TscGPVertPageControlPage);
var
  i: Integer;
begin
  if Value <> nil
  then
    begin
      i := GetPageIndex(Value);
      if (i <> -1) and not (Tabs[i].FVisible) then Tabs[i].FVisible := True;
      TabIndex := i;
    end;
end;

function TscGPVertPageControl.GetPageIndex(Value: TscGPVertPageControlPage): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Tabs.Count - 1 do
    if Tabs[i].Page = Value
    then
       begin
         Result := i;
         Break;
       end;
end;

procedure TscGPVertPageControl.Loaded;
var
  i: Integer;
begin
  inherited;
  if Tabs.Count > 0 then
    for i := 0 to Tabs.Count - 1 do
    if Tabs[i].Page <> nil then
    begin
      Tabs[i].Page.Pager := Self;
      if Tabs[i].Page = FActivePage
      then
        Tabs[i].Page.Visible := True
      else
        Tabs[i].Page.Visible := False;
    end;

  CalcTabRects;
  GetScrollInfo;
  AdjustScrollButtons;
  ScrollToTab(FTabIndex);

  if FActivePage <> nil then
    FActivePage.SetBounds(FActivePage.Left, FActivePage.Top,
      FActivePage.Width, FActivePage.Height);
end;

function TscGPVertPageControl.GetPageBoundsRect: TRect;
begin
  if BidiMode <> bdRightToLeft then
    Result.Left := FTabsLeftOffset + FTabWidth
  else
    Result.Left := 0;
    
  Result.Top := 0;
  Result.Bottom := Height;
  Result.Right := Width -  FTabWidth - FTabsLeftOffset;

  if (FBorderStyle = scgpvbsLine) or (FBorderStyle = scgpvbsLineLeftRight) then
  begin
    if BidiMode <> bdRightToLeft then
      Inc(Result.Left, FFrameWidth);
    Dec(Result.Right, FFrameWidth);
    if FBorderStyle = scgpvbsLineLeftRight then
    begin
      Dec(Result.Right, FFrameWidth);
      if BidiMode = bdRightToLeft then  
        Inc(Result.Left, FFrameWidth);
    end;
  end
  else
  if FBorderStyle = scgpvbsFrame then
  begin
    Inc(Result.Top, FFrameWidth);
    Inc(Result.Left, FFrameWidth);
    Dec(Result.Bottom, FFrameWidth * 2);
    Dec(Result.Right, FFrameWidth * 2);
  end;
end;

procedure TscGPVertPageControl.WMSIZE(var Message: TWMSIZE);
var
  B: Boolean;
begin
  B := FScrollVisible;

  inherited;

  if (FOldHeight <> Height) and (FOldHeight <> -1)
  then
    begin
      GetScrollInfo;
      AdjustScrollButtons;
      if FScrollOffset > 0
      then
        FScrollOffset := FScrollOffset - (Height - FOldHeight);
      if FScrollOffset < 0 then FScrollOffset := 0;
    end;

  if ActivePage <> nil
  then
    with ActivePage do
    begin
      SetBounds(Left, Top, Width, Height);
    end;

  FOldHeight := Height;

  if B <> FScrollVisible then
  begin
    FScrollOffset := 0;
    ScrollToTab(FTabIndex);
  end;

  RePaintControl;
end;

procedure TscGPVertPageControl.DrawTab(ACanvas: TCanvas; G: TGPGraphics; Index: Integer; AFirst: Boolean);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  FC: TColor;
  TabState: TscsCtrlState;
  R, R1: TRect;
  IIndex, TX, TY: Integer;
  FGlowColor: TColor;
  FillR, FrameR, GPR, TabR: TGPRectF;
  TabFillC, TabFrameC, C1, C2: Cardinal;
  FramePath, FillPath: TGPGraphicsPath;
  P: TGPPen;
  l, t, h, w, d: Single;
  B: TGPBrush;
  SH: Integer;
  FOptions: TscGPTabOptions;
  FGlowEffect: TscButtonGlowEffect;
begin

  R := FTabs[Index].TabRect;

  if Index = FTabIndex then
  begin
    if BidiMode <> bdRightToLeft then
      Inc(R.Right, FFrameWidth)
    else
      Dec(R.Left, FFrameWidth);
  end;

  TabR := RectToGPRect(R);

  if FTopScrollButton <> nil then
     SH := FTabsTopOffset + FScrollButtonHeight
   else
     SH := FTabsTopOffset;
  if TabR.Y < SH then
  begin
    TabR.Height := TabR.Height - (SH - TabR.Y);
    TabR.Y := SH;
  end;
  if FBottomScrollButton <> nil then
    SH := FTabsBottomOffset + FScrollButtonHeight
  else
    SH := FTabsBottomOffset;
  if TabR.Y + TabR.Height > Height - SH then
  begin
    TabR.Height := TabR.Height - (TabR.Y + TabR.Height - (Height - SH));
  end;

  if TabR.Height <= 0 then Exit;

  if Tabs[Index].UseCustomOptions then
    FOptions := Tabs[Index].CustomOptions
  else
    FOptions := FTabOptions;

  if BidiMode <> bdRightToLeft then
    R.Right := R.Right + FFrameWidth * 2
  else
    R.Left := R.Left - FFrameWidth * 2;

  if (Tabs[Index].Page = ActivePage) and (ActivePage <> nil) and
      Tabs[Index].Enabled and Tabs[Index].Visible
  then
  begin
    if Focused then
      TabState := scsFocused
    else
      TabState := scsPressed;
  end
  else
  if FTabs[Index].Active then
    TabState := scsHot
  else
  if FTabs[Index].Enabled then
    TabState := scsNormal
  else
    TabState := scsDisabled;

  FOptions.State := TabState;
  FC := FOptions.FontColor;

  if TabState = scsDisabled then
  begin
    if IsLightColor(FC) then
      FC := DarkerColor(FC, 40)
    else
      FC := LighterColor(FC, 40);
  end;


  // draw tab shape
  if not (((TabState = scsNormal) or (TabState = scsDisabled)) and not FShowInActiveTab) then
  begin
    FillR := RectToGPRect(R);
    FrameR := RectToGPRect(R);

    if TabState = scsDisabled then
    begin
      TabFillC := ColorToGPColor(FOptions.Color, FOptions.ColorAlpha div 2);
      TabFrameC := ColorToGPColor(FOptions.FrameColor, FOptions.FrameColorAlpha div 2);
    end
    else
    begin
      TabFillC := ColorToGPColor(FOptions.Color, FOptions.ColorAlpha);
      TabFrameC := ColorToGPColor(FOptions.FrameColor, FOptions.FrameColorAlpha);
    end;
    if FOptions.ShapeFillStyle = scgpsfColor then
       B := TGPSolidBrush.Create(TabFillC)
    else
    begin
      C1 := ColorToGPColor(LighterColor(FOptions.Color, FOptions.GradientColorOffset), FOptions.ColorAlpha);
      C2 := TabFillC;
      GPR := FillR;
      InflateGPRect(GPR, FOptions.FrameWidth, FOptions.FrameWidth);
      B := TGPLinearGradientBrush.Create(GPR, C1, C2, FOptions.ShapeFillGradientAngle);
    end;

    P := TGPPen.Create(TabFrameC, FTabOptions.FrameWidth);
    FramePath := TGPGraphicsPath.Create;
    FillPath := TGPGraphicsPath.Create;
    if FTabOptions.FrameWidth > 0 then
    begin
      InflateGPRect(FillR, -FTabOptions.FrameWidth + 0.2, -FTabOptions.FrameWidth + 0.2);
      InflateGPRect(FrameR, -FTabOptions.FrameWidth / 2, -FTabOptions.FrameWidth / 2);
    end;

    if FTabOptions.ShapeCornerRadius = 0 then
    begin
      FillPath.AddRectangle(FillR);
      FramePath.AddRectangle(FrameR);
    end
    else
    begin
      l := FillR.X;
      t := FillR.y;
      h := FillR.Height;
      w := FillR.Width;
      if FTabOptions.ShapeCornerRadius + FFrameWidth >= FTabHeight div 2 then
        d := h
      else
        d := FTabOptions.ShapeCornerRadius * 2 - FOptions.FrameWidth;
      if d < 1 then d := 1;

      FillPath.StartFigure;

      if BidiMode <> bdRightToLeft then
      begin
        FillPath.AddArc(l, t + h - d, d, d, 90, 90);
        FillPath.AddArc(l, t, d, d, 180, 90);
        FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y),
            MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height));
      end
      else
      begin
        FillPath.AddArc(l + w - d, t, d, d, 270, 90);
        FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FillPath.AddLine(MakePoint(FillR.X, FillR.Y + FillR.Height),
          MakePoint(FillR.X, FillR.Y));
      end;

      FillPath.CloseFigure;

      l := FrameR.X;
      t := FrameR.y;
      h := FrameR.Height;
      w := FrameR.Width;

      if FTabOptions.ShapeCornerRadius + FFrameWidth >= FTabHeight div 2 then
        d := h
      else
        d := FTabOptions.ShapeCornerRadius * 2;

      FramePath.StartFigure;

      if BidiMode <> bdRightToLeft then
      begin
        FramePath.AddArc(l, t + h - d, d, d, 90, 90);
        FramePath.AddArc(l, t, d, d, 180, 90);
        FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y),
          MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height));
      end
      else
      begin
        FramePath.AddArc(l + w - d, t, d, d, 270, 90);
        FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y + FrameR.Height),
          MakePoint(FrameR.X, FrameR.Y));
      end;

      FramePath.CloseFigure;
    end;

    G.IntersectClip(TabR);
    G.FillPath(B, FillPath);
    G.DrawPath(P, FramePath);
    G.ResetClip;

    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;

  // draw image and text
  ACanvas.Font := Self.Font;
  ACanvas.Font.Color := FC;

 if Tabs[Index].CustomGlowEffect.Enabled then
   FGlowEffect := Tabs[Index].CustomGlowEffect
 else
   FGlowEffect := FTabGlowEffect;

 FGlowColor := FGlowEffect.Color;
 case TabState of
   scsHot: FGlowColor := FGlowEffect.HotColor;
   scsPressed: FGlowColor := FGlowEffect.PressedColor;
   scsFocused: FGlowColor := FGlowEffect.FocusedColor;
 end;
 if FGlowColor = clNone then
   FGlowColor := FGlowEffect.Color;

 ACanvas.Brush.Style := bsClear;
 ACanvas.Font.Color := FC;

 R := FTabs[Index].TabRect;
 Inc(R.Top, FFrameWidth);

 if IsRightToLeft then
   Dec(R.Right, FTabMargin)
 else
   Inc(R.Left, FTabMargin);

 IIndex := FTabs[Index].ImageIndex;

 if not Focused and (TabState = scsFocused) then
    TabState := scsPressed;

 if Assigned(FOnGetTabDrawParams) then
   FOnGetTabDrawParams(Index, TabState, ACanvas);

 if FDrawTextMode = scdtmGDIPlus then
 begin
   if (FTabImages <> nil) and (IIndex >= 0) and  (IIndex < FTabImages.Count) then
     GPDrawImageAndText(G, ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, IsRightToLeft, True)
   else
   begin
     R1 := Rect(0, 0, R.Width, R.Height);
     GPDrawText(G, nil, ACanvas, R1, FTabs[Index].Caption, DT_LEFT or DT_CALCRECT);
     TX := R.Left;
     TY := R.Top + R.Height div 2 - R1.Height div 2;
     if TY < R.Top then TY := R.Top;
     R := Rect(TX, TY, R.Right, TY + R1.Height);
     GPDrawText(G, nil, ACanvas, R, FTabs[Index].Caption, scDrawTextBidiModeFlags(DT_LEFT, BidiMode = bdRightToLeft));
   end;
 end
 else
 if (FTabImages <> nil) and (IIndex >= 0) and
    (IIndex < FTabImages.Count) then
 begin
   if FGlowEffect.Enabled and (TabState in FGlowEffect.States) then
      DrawImageAndTextWithGlow2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack,
        FGlowEffect.Offset, FGlowColor,
        FGlowEffect.GlowSize, FGlowEffect.Intensive,
        FGlowEffect.AlphaValue, True,
        False, IsRightToLeft, True)
    else
      DrawImageAndText2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack, False, IsRightToLeft, True)
  end
  else
  begin
    R1 := Rect(0, 0, R.Width, R.Height);
    DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
      Length(FTabs[Index].Caption), R1,
      DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
    TX := R.Left;
    TY := R.Top + R.Height div 2 - R1.Height div 2;
    if TY < R.Top then TY := R.Top;
    R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
    if FGlowEffect.Enabled and (TabState in FGlowEffect.States) then
      DrawTextWithGlow(ACanvas, R, FTabs[Index].Caption, DT_LEFT or DT_WORDBREAK,
        FGlowEffect.Offset, FGlowColor, FGlowEffect.GlowSize,
        FGlowEffect.Intensive, FGlowEffect.AlphaValue, IsRightToLeft, True)
    else
      DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
        Length(FTabs[Index].Caption), R,
        scDrawTextBidiModeFlags(DT_LEFT or DT_WORDBREAK, BidiMode = bdRightToLeft));
  end;

  if FShowCloseButtons and FTabs[Index].ShowCloseButton then
  begin
    R := FTabs[Index].TabRect;
    if IsRightToLeft then
      R.Right := R.Left + FCloseButtonSize + 15
    else
      R.Left := R.Right - FCloseButtonSize - 15;
    DrawCloseButton(ACanvas, G, R, Index, FC);
  end;

end;

function TscGPVertPageControl.CreatePage: TscGPVertPageControlPage;

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
  LPage: TscGPVertPageControlPage;
  R: TRect;
begin
  LPage := TscGPVertPageControlPage.Create(GetParentForm(Self));
  LPage.Parent := Self;
  LPage.Pager := Self;
  R := GetPageBoundsRect;
  LPage.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
  LPage.Name := GetUniqueName('scGPVertPageControlPage%d', GetParentForm(Self));
  ActivePage := LPage;
  Result := LPage;

  if not (csLoading in ComponentState) then
  begin
    FScrollOffset := 0;
    GetScrollInfo;
    AdjustScrollButtons;
    FTabIndex := GetPageIndex(FActivePage);
    ScrollToTab(FTabIndex);
  end;

  RePaintControl;
end;

constructor TscGPVertTabControlTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCustomOptions := TscGPTabOptions.Create;
  FCustomOptions.OnChange := OnOptionsChange;
  FCustomGlowEffect := TscButtonGlowEffect.Create;
  FCustomGlowEffect.States := [scsFocused];
  FCustomGlowEffect.OnChange := OnOptionsChange;
  FShowCloseButton := True;
  FUseCustomOptions := False;
  FCustomFrameColor := clNone;
  FCustomFrameColorAlpha := 255;
  Active := False;
  CloseButtonMouseIn := False;
  CloseButtonMouseDown := False;
  CloseButtonRect := Rect(0, 0, 0, 0);
  FEnabled := True;
  FVisible := True;
  FCaption := 'TscGPVertTabControlTab' + IntToStr(Index + 1);
  FImageIndex := -1;
end;

destructor TscGPVertTabControlTab.Destroy;
begin
  FCustomOptions.Free;
  FCustomGlowEffect.Free;
  inherited;
end;

procedure TscGPVertTabControlTab.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TscGPVertTabControlTab
  then
    begin
      FCaption := TscGPVertTabControlTab(Source).Caption;
      FImageIndex := TscGPVertTabControlTab(Source).ImageIndex;
      FVisible := TscGPVertTabControlTab(Source).Visible;
      FEnabled := TscGPVertTabControlTab(Source).Enabled;
      FShowCloseButton :=  TscGPVertTabControlTab(Source).ShowCloseButton;
      FUseCustomOptions := TscGPVertTabControlTab(Source).UseCustomOptions;
      FCustomOptions.Assign(TscGPVertTabControlTab(Source).CustomOptions);
      FCustomFrameColor := TscGPVertTabControlTab(Source).CustomFrameColor;
      FCustomFrameColorAlpha := TscGPVertTabControlTab(Source).CustomFrameColorAlpha;
    end
end;

procedure TscGPVertTabControlTab.OnOptionsChange(Sender: TObject);
begin
  Changed(False);
end;

procedure TscGPVertTabControlTab.SetShowCloseButton(Value: Boolean);
begin
  if FShowCloseButton <> Value then
  begin
    FShowCloseButton := Value;
    Changed(False);
  end;
end;

procedure TscGPVertTabControlTab.SetCustomFrameColor(Value: TColor);
begin
  if FCustomFrameColor <> Value then
  begin
    FCustomFrameColor := Value;
    Changed(False);
  end;
end;

procedure TscGPVertTabControlTab.SetCustomFrameColorAlpha(Value: Byte);
begin
  if FCustomFrameColorAlpha <> Value then
  begin
    FCustomFrameColorAlpha := Value;
    Changed(False);
  end;
end;

procedure TscGPVertTabControlTab.SetUseCustomOptions(Value: Boolean);
begin
  if FUseCustomOptions <> Value then
  begin
    FUseCustomOptions := Value;
    Changed(False);
  end;
end;

procedure TscGPVertTabControlTab.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscGPVertTabControlTab.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value
  then
    begin
      FEnabled := Value;
      Changed(False);
    end;
end;

procedure TscGPVertTabControlTab.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value
  then
    begin
      FImageIndex := Value;
      Changed(False);
    end;
end;

procedure TscGPVertTabControlTab.SetVisible(Value: Boolean);
var
  B: Boolean;
  i, j: Integer;
  FPager: TscGPVertTabControl;
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
    FPager := TscGPVertTabControlTabs(Collection).TabControl;
    if (FPager <> nil) and not FVisible
       and not (csLoading in FPager.ComponentState)
    then
    begin
      j := Index;
      B := False;
      if j < FPager.Tabs.Count then
        for i := j to FPager.Tabs.Count - 1 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
             begin
               FPager.FTabIndex := -1;
               FPager.TabIndex := i;
               B := True;
               Break;
             end;
         end;

      if not B and (j >= 0) then
        for i := j downto 0 do
        begin
          if (i >= 0) and (i < FPager.Tabs.Count) then
            if FPager.Tabs[i].Visible and FPager.Tabs[i].Enabled then
            begin
              FPager.FTabIndex := -1;
              FPager.TabIndex := i;
              Break;
            end;
        end;
       FPager.AdjustScrollButtons;
     end;
  end;
end;

constructor TscGPVertTabControlTabs.Create;
begin
  inherited Create(TscGPVertTabControlTab);
  TabControl := ATabControl;
  DestroyTab := nil;
end;

function TscGPVertTabControlTabs.GetOwner: TPersistent;
begin
  Result := TabControl;
end;

function TscGPVertTabControlTabs.Add: TscGPVertTabControlTab;
begin
  Result := TscGPVertTabControlTab(inherited Add);
  if (TabControl <> nil) and
     not (csLoading in TabControl.ComponentState) then
  begin
    TabControl.FScrollOffset := 0;
    TabControl.RePaintControl;
    TabControl.GetScrollInfo;
    TabControl.AdjustScrollButtons;
  end;
end;

function TscGPVertTabControlTabs.Insert(Index: Integer): TscGPVertTabControlTab;
begin
  Result := TscGPVertTabControlTab(inherited Insert(Index));
  if (TabControl <> nil)
     and not (csDesigning in TabControl.ComponentState)
     and not (csLoading in TabControl.ComponentState)
  then
  begin
    TabControl.FScrollOffset := 0;
    TabControl.RePaintControl;
    TabControl.GetScrollInfo;
    TabControl.AdjustScrollButtons;
  end;
end;

procedure TscGPVertTabControlTabs.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if (TabControl <> nil) and
     not (csLoading in TabControl.ComponentState) then
  begin
    TabControl.FScrollOffset := 0;
    TabControl.RePaintControl;
    TabControl.GetScrollInfo;
    TabControl.AdjustScrollButtons;
    //
    if TabControl.TabIndex > Index then
      Dec(TabControl.FTabIndex);
    //
    if TabControl.TabIndex > Count - 1 then
      TabControl.TabIndex := Count - 1
    else
      TabControl.ScrollToTab(TabControl.TabIndex);
    //
  end;
end;

procedure TscGPVertTabControlTabs.Update(Item: TCollectionItem);
var
  F: TCustomForm;
begin
  inherited;
  if TabControl = nil then
    Exit;

  if (csDesigning in TabControl.ComponentState) and
     not (csLoading in  TabControl.ComponentState) and
     not (csDestroying in TabControl.ComponentState)
  then
  begin
    F := GetParentForm(TabControl);
    if F <> nil then
      F.Designer.Modified;
  end;

  TabControl.UpdateTabs;
end;

function TscGPVertTabControlTabs.GetItem(Index: Integer):  TscGPVertTabControlTab;
begin
  Result := TscGPVertTabControlTab(inherited GetItem(Index));
end;

procedure TscGPVertTabControlTabs.SetItem(Index: Integer; Value:  TscGPVertTabControlTab);
begin
  inherited SetItem(Index, Value);
end;

constructor TscGPVertTabControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FTabsScaling := False;
  Color := clBtnFace;
  FTabsBGFillColor := clNone;
  FTabsBGFillColorAlpha := 255;
  FTabsWallpapers := nil;
  FTabsWallpaperIndex := -1;
  FTabsWallpaper2Index := -1;

  ParentBackground := False;
  ParentColor := False;
  FTabs := TscGPVertTabControlTabs.Create(Self);
  FTabOptions := TscGPTabOptions.Create;
  FTabOptions.GradientColorOffset := 5;
  FTabOptions.ShapeFillGradientAngle := 0;
  FTabOptions.OnChange := OnControlChange;
  FFrameWidth := 2;
  FFrameScaleWidth := False;
  FFrameColor := clBtnText;
  FFrameColorAlpha := 80;
  FTabIndex := -1;
  FScrollButtonHeight := 20;
  FCloseButtonSize := TAB_CLOSE_SIZE;
  FBorderStyle := scgpvbsFrame;
  FShowInactiveTab := True;
  FTabGlowEffect := TscButtonGlowEffect.Create;
  FTabGlowEffect.States := [scsFocused];
  FTabGlowEffect.OnChange := OnControlChange;
  FMouseWheelSupport := True;
  FShowCloseButtons := False;
  FTabMargin := 10;
  FTabSpacing := 10;
  FDeleteOnClose := False;
  FTabWidth := DefPagerTabWidth;
  FTabHeight := DefPagerTabHeight;
  FTabImages := nil;
  FTransparentBackground := False;

  FMouseIn := False;
  FScrollOffset := 0;
  FLeftOffset := 6;
  FRightOffset := 5;
  Width := 300;
  Height := 150;
  FOldTabActive := -1;
  FTabActive := -1;
  FOldHeight := -1;
  FTabsTopOffset := 15;
  FTabsLeftOffset := 0;
  FTabsBottomOffset := 15;
  FTopScrollButton := nil;
  FBottomScrollButton := nil;
  FShowFocusRect := True;
end;

destructor TscGPVertTabControl.Destroy;
begin
  FTabOptions.Free;
  FTabGlowEffect.Free;
  FTabs.Free;
  FTabs := nil;
  inherited;
end;

procedure TscGPVertTabControl.SetTabsWallpapers(Value: TscCustomImageCollection);
begin
  if FTabsWallpapers <> Value then
  begin
    FTabsWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertTabControl.SetTabsWallpaperIndex(Value: Integer);
begin
  if FTabsWallpaperIndex <> Value then
  begin
    FTabsWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertTabControl.SetTabsWallpaper2Index(Value: Integer);
begin
  if FTabsWallpaper2Index <> Value then
  begin
    FTabsWallpaper2Index := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertTabControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := GetPageBoundsRect;
end;

procedure TscGPVertTabControl.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  if FFrameScaleWidth then
  begin
    FFrameWidth := MulDiv(FFrameWidth, M, D);
    FTabOptions.FrameWidth := MulDiv(FTabOptions.FrameWidth, M, D);
  end;
  FTabOptions.ShapeCornerRadius := MulDiv(FTabOptions.ShapeCornerRadius, M, D);
  FCloseButtonSize := MulDiv(FCloseButtonSize, M, D);
  FTabMargin := MulDiv(FTabMargin, M, D);
  FScrollButtonHeight := MulDiv(FScrollButtonHeight, M, D);
  FTabHeight := MulDiv(FTabHeight, M, D);
  TabWidth := MulDiv(FTabWidth, M, D);
  FTabsTopOffset := MulDiv(FTabsTopOffset, M, D);
  FTabsLeftOffset := MulDiv(FTabsLeftOffset, M, D);
  FTabsBottomOffset := MulDiv(FTabsBottomOffset, M, D);

  if not (csLoading in ComponentState) then
    FTabsScaling := True;

  inherited;
  SetTimer(Handle, 1, 100, nil);
end;

procedure TscGPVertTabControl.SetTabsBGFillColor(Value: TColor);
begin
  if FTabsBGFillColor <> Value then
  begin
    FTabsBGFillColor := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertTabControl.SetTabsBGFillColorAlpha(Value: Byte);
begin
  if FTabsBGFillColorAlpha <> Value then
  begin
    FTabsBGFillColorAlpha := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscGPVertTabControl.SetScrollButtonHeight(Value: Integer);
begin
  if (Value >= 20) and (FScrollButtonHeight <> Value) then
  begin
    FScrollButtonHeight := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.DoClose;
var
  CanClose: Boolean;
  P: TPoint;
begin
  if (FTabIndex < 0) or (FTabIndex >= Tabs.Count) then
    Exit;

  CanClose := True;
  if Assigned(FOnClose) then FOnClose(Self, CanClose);

  if CanClose then
  begin
    FScrollOffset := 0;
    if FDeleteOnClose then
      FTabs.Delete(FTabIndex)
    else
      FTabs[FTabIndex].Visible := False;
  end;

  if CanClose = False then
  begin
    GetCursorPos(P);
    if (WinApi.Windows.WindowFromPoint(P) <> Self.Handle) and (FTabActive <> -1) then
    begin
      FTabs[FTabActive].CloseButtonMouseIn := False;
      FTabs[FTabActive].CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;

end;

procedure TscGPVertTabControl.DrawCloseButton(ACanvas: TCanvas;
  G: TGPGraphics;
  ARect: TRect; AIndex: Integer;  AColor: TColor);
var
  X, Y: Integer;
  ButtonR: TRect;
  GlyphColor, FillColor: Cardinal;
  R: TGPRectF;
  B: TGPSolidBrush;
begin
  X := ARect.Left + ARect.Width div 2 - FCloseButtonSize div 2;
  Y := ARect.Top + ARect.Height div 2 - FCloseButtonSize div 2 + FFrameWidth;
  ButtonR := Rect(X, Y, X + FCloseButtonSize, Y + FCloseButtonSize);
  R := RectToGPRect(ButtonR);
  Tabs[AIndex].CloseButtonRect := ButtonR;
  FillColor := 0;
  if not Tabs[AIndex].Enabled then
  begin
    GlyphColor := ColorToGPColor(GetStyleColor(clBtnText), 100);
  end
  else
  if Tabs[AIndex].CloseButtonMouseDown then
  begin
    FillColor := ColorToGPColor(clRed, 200);
    GlyphColor := ColorToGPColor(clWhite, 200);
  end
  else
  if Tabs[AIndex].CloseButtonMouseIn then
  begin
    FillColor := ColorToGPColor(clRed, 220);
    GlyphColor := ColorToGPColor(clWhite, 255);
  end
  else
    GlyphColor := ColorToGPColor(GetStyleColor(clBtnText), 200);
  if FillColor <> 0 then
  begin
    B := TGPSolidBrush.Create(FillColor);
    G.FillEllipse(B, R);
    B.Free;
  end;
  InflateGPRect(R, -FCloseButtonSize div 4, -FCloseButtonSize div 4);
  scGPUtils.GPDrawClearGlyph
    (G, R, GlyphColor, FScaleFactor, 2);
end;

procedure TscGPVertTabControl.SetShowCloseButtons(Value: Boolean);
begin
  if FShowCloseButtons <> Value then
  begin
    FShowCloseButtons := Value;
    GetScrollInfo;
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.SetFrameWidth(Value: Integer);
begin
  if Value <> FFrameWidth then
  begin
    FFrameWidth := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPVertTabControl.SetFrameColor(Value: TColor);
begin
  if Value <> FFrameColor then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.SetFrameColorAlpha(Value: Byte);
begin
  if Value <> FFrameColorAlpha then
  begin
    FFrameColorAlpha := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.SetTabsTopOffset(Value: Integer);
begin
  if Value <> FTabsTopOffset then
  begin
    FTabsTopOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscGPVertTabControl.SetBorderStyle(Value: TscGPVertTabControlBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGPVertTabControl.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[i] is TWinControl)
    then
      SendMessage(TWinControl(Controls[I]).Handle, WM_CHECKPARENTBG, 0, 0)
    else
    if Controls[i] is TGraphicControl
     then
       TGraphicControl(Controls[I]).Perform(WM_CHECKPARENTBG, 0, 0);
  end;
end;

procedure TscGPVertTabControl.SetTabsLeftOffset(Value: Integer);
begin
  if (Value <> FTabsLeftOffset) and (Value >= 0) then
  begin
    FTabsLeftOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
    ReAlign;
  end;
end;

procedure TscGPVertTabControl.SetShowInActiveTab(Value: Boolean);
begin
  if Value <> FShowInActiveTab then
  begin
    FShowInActiveTab := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.SetTabsBottomOffset(Value: Integer);
begin
  if (Value <> FTabsBottomOffset) and (Value >= 0) then
  begin
    FTabsBottomOffset := Value;
    RePaintControl;
    AdjustScrollButtons;
    ScrollToTab(FTabIndex);
  end;
end;

procedure TscGPVertTabControl.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscGPVertTabControl.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  TabsRect: TRect;
  PageRect: TRect;
  FFirst: Boolean;
  FFirstVisible: Boolean;
  I: Integer;
  SaveIndex: Integer;
  G: TGPGraphics;
  P: TGPPen;
  C: Cardinal;
  R, R1: TGPRectF;
  H: Integer;
  B: TGPSolidBrush;
begin
  if BidiMode <> bdRightToLeft then
  begin
    TabsRect := Rect(0, 0, FTabWidth + FTabsLeftOffset, Height);
    PageRect := Rect(FTabWidth + FTabsLeftOffset, 0, Width, Height);
  end
  else
  begin
    TabsRect := Rect(Width - FTabWidth - FTabsLeftOffset, 0, Width, Height);
    PageRect := Rect(0, 0, Width - FTabWidth - FTabsLeftOffset, Height);
  end;

  // draw background
  if not FTransparentBackground then
    with ACanvas do
    begin
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(PageRect);
      if seClient in StyleElements then
        Brush.Color := GetStyleColor(Color)
      else
        Brush.Color := Color;
      FillRect(TabsRect);
    end;

  if (FTabsWallpapers <> nil) and  (FTabsWallpaperIndex <> -1) and
     FTabsWallpapers.IsIndexAvailable(FTabsWallpaperIndex) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle,
        TabsRect.Left, TabsRect.Top,
        TabsRect.Right, TabsRect.Bottom);
      FTabsWallpapers.Draw(ACanvas, TabsRect, FTabsWallpaperIndex, FScaleFactor);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  // draw border
  G := TGPGraphics.Create(ACanvas.Handle);
  G.SetSmoothingMode(SmoothingModeHighQuality);
  G.SetPixelOffsetMode(PixelOffsetModeHalf);

  if (FTabsBGFillColor <> clNone) and (FTabsBGFillColorAlpha > 0) then
  begin
    C := ColorToGPColor(GetStyleCOlor(FTabsBGFillColor), FTabsBGFillColorAlpha);
    B := TGPSolidBrush.Create(C);
    R := RectToGPRect(TabsRect);
    G.FillRectangle(B, R);
    B.Free;
  end;

  if (FTabsWallpapers <> nil) and (FTabsWallpaper2Index <> -1)  and
    FTabsWallpapers.IsIndexAvailable(FTabsWallpaper2Index) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle,
        TabsRect.Left, TabsRect.Top,
        TabsRect.Right, TabsRect.Bottom);
      FTabsWallpapers.Draw(ACanvas, TabsRect, FTabsWallpaper2Index, FScaleFactor);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  if (FTabIndex <> -1) and (FTabIndex < FTabs.Count) and (FTabs[FTabIndex].CustomFrameColor <> clNone) then
    C := ColorToGPColor(GetStyleColor(FTabs[FTabIndex].CustomFrameColor), FTabs[FTabIndex].CustomFrameColorAlpha)
  else
    C := ColorToGPColor(GetStyleColor(FFrameColor), FFrameColorAlpha);

  P := TGPPen.Create(C, FFrameWidth);
  if not FGetControlBG then
    CalcTabRects;

  if (FBorderStyle <> scgpvbsNone) and not FGetControlBG then
  begin
    if (FTabIndex <> -1) and (FTabIndex < FTabs.Count) and FTabs[FTabIndex].Visible then
    begin
      if FScrollVisible then
        H := FTabsTopOffset + FScrollButtonHeight
      else
        H := FTabsTopOffset;
      R1 := RectToGPRect(FTabs[FTabIndex].TabRect);

      if FBorderStyle <> scgpvbsNone then
      begin
        R1.Width := R1.Width + FFrameWidth;
        if BidiMode = bdRightToLeft then
          R1.X := R1.X - FFrameWidth;
      end;

      if R1.Y < H then
      begin
        R1.Height := R1.Height - (H - R1.Y);
        R1.Y := H;
      end;

      if FScrollVisible then
        H := FTabsBottomOffset + FScrollButtonHeight
      else
        H := FTabsBottomOffset;

      if R1.Y + R1.Height > Height - H then
      begin
        R1.Height := R1.Height - (R1.Y + R1.Height - (Height - H));
      end;
      if (R1.Height > 0) and (R1.Y <= Height - H) then
        G.ExcludeClip(R1);
    end;
    R := RectToGPRect(PageRect);
    InflateGPRect(R, -FFrameWidth / 2, -FFrameWidth / 2);
    if (FBorderStyle = scgpvbsLine) or (FBorderStyle = scgpvbsLineLeftRight) then
    begin
      R.Y := 0;
      R.Height := Height;
    end;
    if FBorderStyle = scgpvbsFrame then
      G.DrawRectangle(P, R)
    else
    if (FBorderStyle = scgpvbsLine) or (FBorderStyle = scgpvbsLineLeftRight) then
    begin
      if BidiMode <> bdRightToLeft then
      begin
        G.DrawLine(P, R.X, R.Y, R.X, R.Y + R.Height);
        if FBorderStyle = scgpvbsLineLeftRight then
          G.DrawLine(P, R.X + R.Width, R.Y, R.X + R.Width, R.Y + R.Height);
      end
      else
      begin
        G.DrawLine(P, R.X + R.Width, R.Y, R.X + R.Width, R.Y + R.Height);
        if FBorderStyle = scgpvbsLineLeftRight then
          G.DrawLine(P, R.X, R.Y, R.X, R.Y + R.Height);
      end;
    end;
    if (FTabIndex <> -1) and (R1.Height > 0) and (R1.Y <= Height - FTabsBottomOffset) then
      G.ResetClip;
  end;

  // draw items

  FTabIndexBeforeFocused := FindDrawPriorTabFromIndex(FTabIndex);
  FTabIndexAfterFocused := FindDrawNextTabFromIndex(FTabIndex);

  SaveIndex := SaveDC(ACanvas.Handle);
  try
    if not FGetControlBG then
    begin
      if BidiMode <> bdRightToLeft then
        IntersectClipRect(ACanvas.Handle,
          FTabsLeftOffset, FTabsTopOffset, FTabsLeftOffset + FTabWidth + 2,  Height - FTabsBottomOffset)
      else
        IntersectClipRect(ACanvas.Handle,
          Width - FTabsLeftOffset - FTabWidth - 2, FTabsTopOffset,
            Width - FTabsLeftOffset,  Height - FTabsBottomOffset);

      FFirstVisible := False;
      for I := 0 to FTabs.Count - 1  do
      begin
        if FTabs[I].Visible then
        begin
          FFirst := (FTabsLeftOffset = 0) and (FTabs[I].TabRect.Left = 0);
          if not FFirstVisible and (I = FTabIndex) and not FTabs[I].Enabled then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FFirstVisible and (I <> FTabIndex) then
          begin
            FFirst := True;
            FFirstVisible := True;
          end
          else
          if not FShowInActiveTab then
            FFirst := (I <> FTabIndex);
          if (I = TabIndex) and FTabs[I].Enabled then
            FFirstVisible := True;
          DrawTab(ACanvas, G, I, FFirst);
        end;
      end;
    end;
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;

  G.Free;
  P.Free;
end;

procedure TscGPVertTabControl.SetTabImages(Value: TCustomImageList);
begin
  if FTabImages <> Value then
  begin
    FTabImages := Value;
    UpdateTabs;
  end;
end;

procedure TscGPVertTabControl.SetTabWidth;
begin
  if FTabWidth <> Value then
  begin
    FTabWidth := Value;
    ReAlign;
    RePaintControl;
    AdjustScrollButtons;
  end;
end;

procedure TscGPVertTabControl.SetTabHeight;
begin
  if FTabHeight <> Value then
  begin
    FTabHeight := Value;
    RePaintControl;
    AdjustScrollButtons;
  end;
end;

procedure TscGPVertTabControl.SetTabMargin(Value: Integer);
begin
  if (Value > 0) and (FTabMargin <> Value) then
  begin
    FTabMargin := Value;
    UpdateTabs;
  end;
end;

procedure TscGPVertTabControl.SetTabSpacing(Value: Integer);
begin
  if (Value > 0) and (FTabSpacing <> Value) then
  begin
    FTabSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.SetTabIndex;
var
  B: Boolean;
begin
  if (Value < 0) or (Value > Tabs.Count - 1) then
  begin
    if csLoading in ComponentState then
     FTabIndex := Value;
    Exit;
  end;

  if Assigned(FOnCanChangeTab) and not (csLoading in ComponentState) then
  begin
    B := True;
    FOnCanChangeTab(Value, B);
    if not B then Exit;
  end;

  if not Tabs[Value].FVisible then Tabs[Value].FVisible := True;

  if (FTabIndex <> Value) and (Value >= 0) and (Value < Tabs.Count)
  then
    begin
      FTabIndex := Value;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangingTab) then FOnChangingTab(Self);

      if FScrollVisible then
         ScrollToTab(FTabIndex);

      if not (csLoading in ComponentState) then
        if Assigned(FOnChangeTab) then FOnChangeTab(Self);
    end;

  RePaintControl;
end;

function TscGPVertTabControl.TabFromPoint;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Tabs.Count -1 do
    if Tabs[i].Visible and PtInRect(Tabs[i].TabRect, P)
    then
      begin
        Result := i;
        Break;
      end;
end;

procedure TscGPVertTabControl.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  AdjustScrollButtons;
end;

procedure TscGPVertTabControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateTabs;
end;

procedure TscGPVertTabControl.CMDesignHitTest;
var
  P: TPoint;
  I: Integer;
begin
  inherited;
  P := SmallPointToPoint(Message.Pos);
  I := TabFromPoint(P);
  if (Message.Keys = MK_LBUTTON) and (I <> -1) then
  begin
    TabIndex := I;
    GetParentForm(Self).Designer.Modified;
  end;
end;

procedure TscGPVertTabControl.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscGPVertTabControl.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTabImages) then
    FTabImages := nil;
  if (Operation = opRemove) and (AComponent = FTabsWallpapers) then
    FTabsWallpapers := nil;
end;

function TscGPVertTabControl.FindNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscGPVertTabControl.FindPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscGPVertTabControl.FindDrawNextTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

function TscGPVertTabControl.FindDrawPriorTabFromIndex(AIndex: Integer): Integer;
var
  i, j, k: Integer;
begin
  Result := -1;
  j := AIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then Result := k;
end;

procedure TscGPVertTabControl.FindNextTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = Tabs.Count - 1) then Exit;
  k := -1;
  for i := j + 1 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertTabControl.FindPriorTab;
var
  i, j, k: Integer;
begin
  j := TabIndex;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertTabControl.FindFirstTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertTabControl.FindLastTab;
var
  i, k: Integer;
begin
  k := -1;
  for i := Tabs.Count - 1 downto 0 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled
    then
      begin
        k := i;
        Break;
      end;
  end;
  if k <> -1 then TabIndex := k;
end;

procedure TscGPVertTabControl.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    FTabsScaling := False;
    UpdateTabs;
    KillTimer(Handle, 1);
  end;
end;

procedure TscGPVertTabControl.WMMOUSEWHEEL(var Message: TWMMOUSEWHEEL);
begin
  if FMouseWheelSupport then
    if Message.WheelDelta < 0 then FindNextTab else FindPriorTab;
end;

procedure TscGPVertTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) or (Key = VK_SPACE)
  then
    begin
      if (TabIndex <> -1) then
      begin
        if Assigned(FTabs[FTabIndex].OnClick) then
          FTabs[FTabIndex].OnClick(Self);
      end;
    end
  else
  if (Key = VK_NEXT)
  then
    FindLastTab
  else
  if (Key = VK_PRIOR)
  then
   FindFirstTab
  else
  if (Key = VK_LEFT) or (Key = VK_UP)
  then
    FindPriorTab
  else
  if (Key = VK_RIGHT) or (Key = VK_DOWN)
  then
    FindNextTab;
end;


procedure TscGPVertTabControl.WMGetDlgCode;
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TscGPVertTabControl.WMSETFOCUS;
begin
  inherited;
  if not (csLoading in ComponentState) then
    if not FTransparentBackground then
      RePaintControl
    else
    begin
      FUpdateParentBuffer := True;
      if DrawTextMode = scdtmGDIPlus then
        Invalidate
      else
        RePaint;
    end;
end;

procedure TscGPVertTabControl.WMKILLFOCUS;
begin
  inherited;
   if not (csLoading in ComponentState) then
      RePaintControl;
end;

procedure TscGPVertTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TestActive(X, Y);
end;

procedure TscGPVertTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);

var
  WasFocused: Boolean;
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  WasFocused := Focused;

  if FTabActive <> TabIndex then TabIndex := FTabActive;

  if not WasFocused then SetFocus;

  if (FTabActive <> -1) and FShowCloseButtons and FTabs[FTabActive].ShowCloseButton
  and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := True;
        RePaintControl;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscGPVertTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  TestActive(X, Y);
  if (FTabIndex >= 0) and (FTabIndex < Tabs.Count) and
     (FTabIndex = FTabActive) then
  begin
    if Assigned(FTabs[FTabIndex].OnClick) then
        FTabs[FTabIndex].OnClick(Self);
  end;

  if (FTabActive >= 0) and (FTabActive < FTabs.Count) and FShowCloseButtons
      and FTabs[FTabActive].ShowCloseButton  and not (csDesigning in ComponentState)
  then
    with FTabs[FTabActive] do
    begin
      if PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := False;
        RePaintControl;
        DoClose;
      end
      else
      if not PtInRect(CloseButtonRect, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscGPVertTabControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TestActive(-1, -1);
end;

procedure TscGPVertTabControl.TestActive(X, Y: Integer);
var
  i: Integer;
begin
  if Tabs.Count = 0 then Exit;

  FOldTabActive:= FTabActive;
  FTabActive := -1;

  for i := 0 to Tabs.Count - 1 do
  begin
    if Tabs[i].Visible and Tabs[i].Enabled and PtInRect(Tabs[i].TabRect, Point(X, Y)) and
       (X >= FTabsLeftOffset) and (X < Width - FTabsBottomOffset)
    then
      begin
        FTabActive := i;
        Break;
      end;
  end;

  if (FTabActive <> FOldTabActive)
  then
    begin
      if (FOldTabActive <> - 1) and (FOldTabActive < Tabs.Count)
      then
      begin
        Tabs[FOldTabActive].Active := False;
        Tabs[FOldTabActive].CloseButtonMouseIn := False;
        Tabs[FOldTabActive].CloseButtonMouseDown := False;
      end;
      if (FTabActive <> - 1)
      then
        Tabs[FTabActive].Active := True;
      RePaintControl;
    end;

  if (FTabActive <> -1) and FShowCloseButtons and Tabs[FTabActive].ShowCloseButton then
  with Tabs[FTabActive] do
  begin
    if PtInRect(CloseButtonRect, Point(X, Y)) and not CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := True;
      RePaintControl;
    end
    else
    if not PtInRect(CloseButtonRect, Point(X, Y)) and CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := False;
      CloseButtonMouseDown := False;
      RePaintControl;
    end;
  end;

end;

procedure TscGPVertTabControl.ScrollToTab(AIndex: Integer);
var
  R: TRect;
  Offset, SH: Integer;
begin
  if (AIndex < 0) or (AIndex > Tabs.Count - 1) then Exit;

  R := Tabs[AIndex].TabRect;
  if FScrollVisible then
    SH := FScrollButtonHeight
  else
    SH := 0;
  if R.Top < FTabsTopOffset + SH then
  begin
    Offset := Abs(FTabsTopOffset - R.Top);
    Inc(Offset, SH);
    FScrollOffset := FScrollOffset - Offset;
    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end
  else
  if R.Bottom > Height - FTabsBottomOffset - SH then
  begin
    Offset := R.Bottom - (Height - FTabsBottomOffset);
    Inc(Offset, SH);
    FScrollOffset := FScrollOffset + Offset;
    if FScrollOffset < 0 then FScrollOffset := 0;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.ScrollToTop;
begin
  CalcTabRects;
  if FTopTabIndex >= 0 then
    ScrollToTab(FTopTabIndex);
end;

procedure TscGPVertTabControl.ScrollToBottom;
begin
  CalcTabRects;
  if FBottomTabIndex >= 0 then
    ScrollToTab(FBottomTabIndex);
end;

procedure TscGPVertTabControl.OnTopScrollButtonClick(Sender: TObject);
begin
  ScrollToTop;
end;

procedure TscGPVertTabControl.OnBottomScrollButtonClick(Sender: TObject);
begin
  ScrollToBottom;
end;

procedure TscGPVertTabControl.ShowScrollButtons;
var
  B: Boolean;
begin
  B := False;
  if FTopScrollButton = nil then
  begin
    FTopScrollButton := TscGPVertTabScrollButton.Create(Self);
    FTopScrollButton.Visible := False;
    FTopScrollButton.OnClick := OnTopScrollButtonClick;
    FTopScrollButton.RepeatClick := True;
    FTopScrollButton.CanFocused := False;
    FTopScrollButton.TransparentBackground := False;
    FTopScrollButton.GlyphOptions.Kind := scgpbgkUpArrow;
    FTopScrollButton.Parent := Self;
    if BidiMode <> bdRightToLeft then
      FTopScrollButton.SetBounds(FTabsLeftOffset, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight)
    else
    begin
      FTopScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight)
    end;
    FTopScrollButton.Visible := True;
    B := True;
  end
  else
  begin
    if BidiMode <> bdRightToLeft then
      FTopScrollButton.SetBounds(FTabsLeftOffset, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight)
    else
      FTopScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, FTabsTopOffset,
        FTabWidth, FScrollButtonHeight);
  end;

  if FBottomScrollButton = nil then
  begin
    FBottomScrollButton := TscGPVertTabScrollButton.Create(Self);
    FBottomScrollButton.Visible := False;
    FBottomScrollButton.FBottom := True;
    FBottomScrollButton.OnClick := OnBottomScrollButtonClick;
    FBottomScrollButton.RepeatClick := True;
    FBottomScrollButton.CanFocused := False;
    FBottomScrollButton.TransparentBackground := False;
    FBottomScrollButton.GlyphOptions.Kind := scgpbgkDownArrow;
    FBottomScrollButton.Parent := Self;
    if BidiMode <> bdRightToLeft then
      FBottomScrollButton.SetBounds(FTabsLeftOffset, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight)
    else
      FBottomScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight);

    FBottomScrollButton.Visible := True;
    B := True;
  end
  else
  begin
   if BidiMode <> bdRightToLeft then
      FBottomScrollButton.SetBounds(FTabsLeftOffset, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight)
   else
     FBottomScrollButton.SetBounds(Width - FTabsLeftOffset - FTabWidth, Height - FTabsBottomOffset - FScrollButtonHeight,
        FTabWidth, FScrollButtonHeight)
  end;

  if B and not(csLoading in ComponentState) then
    RePaintControl;
end;

procedure TscGPVertTabControl.HideScrollButtons;
begin
  if FTopScrollButton <> nil then
  begin
    FTopScrollButton.Visible := False;
    FTopScrollButton.Free;
    FTopScrollButton := nil;
  end;
  if FBottomScrollButton <> nil then
  begin
    FBottomScrollButton.Visible := False;
    FBottomScrollButton.Free;
    FBottomScrollButton := nil;
  end;
end;

procedure TscGPVertTabControl.AdjustScrollButtons;
begin
  if FTabsScaling then
    Exit;

  if FScrollVisible then
    ShowScrollButtons
  else
    HideScrollButtons;
end;

procedure TscGPVertTabControl.GetScrollInfo;
var
  I, Y: Integer;
begin
  Y := FTabsTopOffset;
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      Y := Y + FTabHeight;
    end;
  FScrollVisible := Y > Height - FTabsBottomOffset;
end;

procedure TscGPVertTabControl.UpdateTabs;
begin
  if not (csLoading in ComponentState) and
     not (csDestroying in ComponentState) then
  begin
    FScrollOffset := 0;
    CalcTabRects;
    GetScrollInfo;
    ScrollToTab(FTabIndex);
    AdjustScrollButtons;
    RePaintControl;
  end;
end;

procedure TscGPVertTabControl.CalcTabRects;
var
  I, X, Y, ScrollH: Integer;
begin
  GetScrollInfo;
  Y := FTabsTopOffset - FScrollOffset;
  if FScrollVisible then
    Inc(Y, FScrollButtonHeight);

  if BidiMode <> bdRightToLeft then
    X := FTabsLeftOffset
  else
    X := Width - FTabWidth - FTabsLeftOffset;

  Canvas.Font := Self.Font;
  FTopTabIndex := -1;
  FBottomTabIndex := -1;

  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin

      Tabs[I].TabRect := Rect(X, Y, X + FTabWidth, Y + FTabHeight);
      Y := Y + FTabHeight;
      if FScrollVisible then
        ScrollH := FScrollButtonHeight
      else
        ScrollH := 0;
      if Tabs[I].TabRect.Top < FTabsTopOffset + ScrollH
      then
        FTopTabIndex := I;
      if (Tabs[I].TabRect.Bottom > Height - FTabsBottomOffset - ScrollH) and
         (FBottomTabIndex = -1)
      then
        FBottomTabIndex := I;
    end;
end;

procedure TscGPVertTabControl.SetTabs(AValue: TscGPVertTabControlTabs);
begin
  FTabs.Assign(AValue);
  RePaintControl;
end;

procedure TscGPVertTabControl.Loaded;
begin
  inherited;
  CalcTabRects;
  GetScrollInfo;
  AdjustScrollButtons;
  ScrollToTab(FTabIndex);
end;

function TscGPVertTabControl.GetPageBoundsRect: TRect;
begin
  if BidiMode <> bdRightToLeft then
  begin
    Result.Left := FTabsLeftOffset + FTabWidth;
    Result.Right := Width;
  end
  else
  begin
    Result.Left := 0;
    Result.Right := Width - FTabWidth;
  end;
  Result.Top := 0;
  Result.Bottom := Height;
  if (FBorderStyle = scgpvbsLine) or (FBorderStyle = scgpvbsLineLeftRight) then
  begin
    if BidiMode <> bdRightToLeft then
      Inc(Result.Left, FFrameWidth);
    Dec(Result.Right, FFrameWidth);
    if FBorderStyle = scgpvbsLineLeftRight then
    begin
      Dec(Result.Right, FFrameWidth);
      if BidiMode = bdRightToLeft then
        Inc(Result.Left, FFrameWidth);
    end;
  end
  else
  if FBorderStyle = scgpvbsFrame then
  begin
    Inc(Result.Top, FFrameWidth);
    Inc(Result.Left, FFrameWidth);
    Dec(Result.Bottom, FFrameWidth);
    Dec(Result.Right, FFrameWidth);
  end;
end;


procedure TscGPVertTabControl.WMSIZE(var Message: TWMSIZE);
var
  B: Boolean;
begin
  B := FScrollVisible;

  inherited;

  if (FOldHeight <> Height) and (FOldHeight <> -1)
  then
    begin
      GetScrollInfo;
      AdjustScrollButtons;
      if FScrollOffset > 0
      then
        FScrollOffset := FScrollOffset - (Height - FOldHeight);
      if FScrollOffset < 0 then FScrollOffset := 0;
    end;

  FOldHeight := Height;

  if B <> FScrollVisible then
  begin
    FScrollOffset := 0;
    ScrollToTab(FTabIndex);
  end;

  RePaintControl;
end;

procedure TscGPVertTabControl.DrawTab(ACanvas: TCanvas; G: TGPGraphics; Index: Integer; AFirst: Boolean);
const
  GlyphLayout: array[Boolean] of TButtonLayout = (blGlyphLeft, blGlyphRight);
var
  FC: TColor;
  TabState: TscsCtrlState;
  R, R1: TRect;
  IIndex, TX, TY: Integer;
  FGlowColor: TColor;
  FillR, FrameR, GPR, TabR: TGPRectF;
  TabFillC, TabFrameC, C1, C2: Cardinal;
  FramePath, FillPath: TGPGraphicsPath;
  P: TGPPen;
  l, t, h, w, d: Single;
  B: TGPBrush;
  SH: Integer;
  FOptions: TscGPTabOptions;
  FGlowEffect: TscButtonGlowEffect;
begin

  R := FTabs[Index].TabRect;

  if Index = FTabIndex then
  begin
    if BidiMode <> bdRightToLeft then
      Inc(R.Right, FFrameWidth)
    else
      Dec(R.Left, FFrameWidth);
  end;

  TabR := RectToGPRect(R);

  if FTopScrollButton <> nil then
     SH := FTabsTopOffset + FScrollButtonHeight
   else
     SH := FTabsTopOffset;
  if TabR.Y < SH then
  begin
    TabR.Height := TabR.Height - (SH - TabR.Y);
    TabR.Y := SH;
  end;
  if FBottomScrollButton <> nil then
    SH := FTabsBottomOffset + FScrollButtonHeight
  else
    SH := FTabsBottomOffset;
  if TabR.Y + TabR.Height > Height - SH then
  begin
    TabR.Height := TabR.Height - (TabR.Y + TabR.Height - (Height - SH));
  end;

  if TabR.Height <= 0 then Exit;

  if Tabs[Index].UseCustomOptions then
    FOptions := Tabs[Index].CustomOptions
  else
    FOptions := FTabOptions;

  if BidiMode <> bdRightToLeft then
    R.Right := R.Right + FFrameWidth * 2
  else
    R.Left := R.Left - FFrameWidth * 2;

  if (TabIndex = Index) and Tabs[Index].Enabled and Tabs[Index].Visible
  then
  begin
    if Focused then
      TabState := scsFocused
    else
      TabState := scsPressed;
  end
  else
  if FTabs[Index].Active then
    TabState := scsHot
  else
  if FTabs[Index].Enabled then
    TabState := scsNormal
  else
    TabState := scsDisabled;

  FOptions.State := TabState;
  FC := FOptions.FontColor;

  if TabState = scsDisabled then
  begin
    if IsLightColor(FC) then
      FC := DarkerColor(FC, 40)
    else
      FC := LighterColor(FC, 40);
  end;


  // draw tab shape
  if not (((TabState = scsNormal) or (TabState = scsDisabled)) and not FShowInActiveTab) then
  begin
    FillR := RectToGPRect(R);
    FrameR := RectToGPRect(R);

    if TabState = scsDisabled then
    begin
      TabFillC := ColorToGPColor(FOptions.Color, FOptions.ColorAlpha div 2);
      TabFrameC := ColorToGPColor(FOptions.FrameColor, FOptions.FrameColorAlpha div 2);
    end
    else
    begin
      TabFillC := ColorToGPColor(FOptions.Color, FOptions.ColorAlpha);
      TabFrameC := ColorToGPColor(FOptions.FrameColor, FOptions.FrameColorAlpha);
    end;
    if FOptions.ShapeFillStyle = scgpsfColor then
       B := TGPSolidBrush.Create(TabFillC)
    else
    begin
      C1 := ColorToGPColor(LighterColor(FOptions.Color, FOptions.GradientColorOffset), FOptions.ColorAlpha);
      C2 := TabFillC;
      GPR := FillR;
      InflateGPRect(GPR, FOptions.FrameWidth, FOptions.FrameWidth);
      B := TGPLinearGradientBrush.Create(GPR, C1, C2, FOptions.ShapeFillGradientAngle);
    end;

    P := TGPPen.Create(TabFrameC, FTabOptions.FrameWidth);
    FramePath := TGPGraphicsPath.Create;
    FillPath := TGPGraphicsPath.Create;
    if FTabOptions.FrameWidth > 0 then
    begin
      InflateGPRect(FillR, -FTabOptions.FrameWidth + 0.2, -FTabOptions.FrameWidth + 0.2);
      InflateGPRect(FrameR, -FTabOptions.FrameWidth / 2, -FTabOptions.FrameWidth / 2);
    end;

    if FTabOptions.ShapeCornerRadius = 0 then
    begin
      FillPath.AddRectangle(FillR);
      FramePath.AddRectangle(FrameR);
    end
    else
    begin
      l := FillR.X;
      t := FillR.y;
      h := FillR.Height;
      w := FillR.Width;
      if FTabOptions.ShapeCornerRadius + FFrameWidth >= FTabHeight div 2 then
        d := h
      else
        d := FTabOptions.ShapeCornerRadius * 2 - FOptions.FrameWidth;
      if d < 1 then d := 1;

      FillPath.StartFigure;

      if BidiMode <> bdRightToLeft then
      begin
        FillPath.AddArc(l, t + h - d, d, d, 90, 90);
        FillPath.AddArc(l, t, d, d, 180, 90);
        FillPath.AddLine(MakePoint(FillR.X + FillR.Width, FillR.Y),
            MakePoint(FillR.X + FillR.Width, FillR.Y + FillR.Height));
      end
      else
      begin
        FillPath.AddArc(l + w - d, t, d, d, 270, 90);
        FillPath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FillPath.AddLine(MakePoint(FillR.X, FillR.Y + FillR.Height),
          MakePoint(FillR.X, FillR.Y));
      end;

      FillPath.CloseFigure;

      l := FrameR.X;
      t := FrameR.y;
      h := FrameR.Height;
      w := FrameR.Width;

      if FTabOptions.ShapeCornerRadius + FFrameWidth >= FTabHeight div 2 then
        d := h
      else
        d := FTabOptions.ShapeCornerRadius * 2;

      FramePath.StartFigure;

      if BidiMode <> bdRightToLeft then
      begin
        FramePath.AddArc(l, t + h - d, d, d, 90, 90);
        FramePath.AddArc(l, t, d, d, 180, 90);
        FramePath.AddLine(MakePoint(FrameR.X + FrameR.Width, FrameR.Y),
          MakePoint(FrameR.X + FrameR.Width, FrameR.Y + FrameR.Height));
      end
      else
      begin
        FramePath.AddArc(l + w - d, t, d, d, 270, 90);
        FramePath.AddArc(l + w - d, t + h - d, d, d, 0, 90);
        FramePath.AddLine(MakePoint(FrameR.X, FrameR.Y + FrameR.Height),
          MakePoint(FrameR.X, FrameR.Y));
      end;

      FramePath.CloseFigure;
    end;

    G.IntersectClip(TabR);
    G.FillPath(B, FillPath);
    G.DrawPath(P, FramePath);
    G.ResetClip;

    B.Free;
    P.Free;
    FramePath.Free;
    FillPath.Free;
  end;

  // draw image and text
  ACanvas.Font := Self.Font;
  ACanvas.Font.Color := FC;

 if Tabs[Index].CustomGlowEffect.Enabled then
   FGlowEffect := Tabs[Index].CustomGlowEffect
 else
   FGlowEffect := FTabGlowEffect;

 FGlowColor := FGlowEffect.Color;
 case TabState of
   scsHot: FGlowColor := FGlowEffect.HotColor;
   scsPressed: FGlowColor := FGlowEffect.PressedColor;
   scsFocused: FGlowColor := FGlowEffect.FocusedColor;
 end;
 if FGlowColor = clNone then
   FGlowColor := FGlowEffect.Color;

 ACanvas.Brush.Style := bsClear;
 ACanvas.Font.Color := FC;

 R := FTabs[Index].TabRect;
 Inc(R.Top, FFrameWidth);

 if IsRightToLeft then
   Dec(R.Right, FTabMargin)
 else
   Inc(R.Left, FTabMargin);

 IIndex := FTabs[Index].ImageIndex;

 if not Focused and (TabState = scsFocused) then
    TabState := scsPressed;

 if Assigned(FOnGetTabDrawParams) then
   FOnGetTabDrawParams(Index, TabState, ACanvas);

 if FDrawTextMode = scdtmGDIPlus then
 begin
   if (FTabImages <> nil) and (IIndex >= 0) and  (IIndex < FTabImages.Count) then
     GPDrawImageAndText(G, ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, IsRightToLeft, True)
   else
   begin
     R1 := Rect(0, 0, R.Width, R.Height);
     GPDrawText(G, nil, ACanvas, R1, FTabs[Index].Caption, DT_LEFT or DT_CALCRECT);
     TX := R.Left;
     TY := R.Top + R.Height div 2 - R1.Height div 2;
     if TY < R.Top then TY := R.Top;
     R := Rect(TX, TY, R.Right, TY + R1.Height);
     GPDrawText(G, nil, ACanvas, R, FTabs[Index].Caption, scDrawTextBidiModeFlags(DT_LEFT, BidiMode = bdRightToLeft));
   end;
 end
 else
 if (FTabImages <> nil) and (IIndex >= 0) and
    (IIndex < FTabImages.Count) then
 begin
   if FGlowEffect.Enabled and (TabState in FGlowEffect.States) then
      DrawImageAndTextWithGlow2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack,
        FGlowEffect.Offset, FGlowColor,
        FGlowEffect.GlowSize, FGlowEffect.Intensive,
        FGlowEffect.AlphaValue, True,
        False, IsRightToLeft, True)
    else
      DrawImageAndText2(ACanvas, R, 0, FTabSpacing, GlyphLayout[IsRightToLeft],
        FTabs[Index].Caption, IIndex, FTabImages,
        FTabs[Index].Enabled and Self.Enabled, False, clBlack, False, IsRightToLeft, True)
  end
  else
  begin
    R1 := Rect(0, 0, R.Width, R.Height);
    DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
      Length(FTabs[Index].Caption), R1,
      DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
    TX := R.Left;
    TY := R.Top + R.Height div 2 - R1.Height div 2;
    if TY < R.Top then TY := R.Top;
    R := Rect(TX, TY, R.Right - 2, TY + R1.Height);
    if FGlowEffect.Enabled and (TabState in FGlowEffect.States) then
      DrawTextWithGlow(ACanvas, R, FTabs[Index].Caption, DT_LEFT or DT_WORDBREAK,
        FGlowEffect.Offset, FGlowColor, FGlowEffect.GlowSize,
        FGlowEffect.Intensive, FGlowEffect.AlphaValue, IsRightToLeft, True)
    else
      DrawText(ACanvas.Handle, PChar(FTabs[Index].Caption),
        Length(FTabs[Index].Caption), R,
        scDrawTextBidiModeFlags(DT_LEFT or DT_WORDBREAK, BidiMode = bdRightToLeft));
  end;

  if FShowCloseButtons and FTabs[Index].ShowCloseButton then
  begin
    R := FTabs[Index].TabRect;
    if IsRightToLeft then
      R.Right := R.Left + FCloseButtonSize + 15
    else
      R.Left := R.Right - FCloseButtonSize - 15;
    DrawCloseButton(ACanvas, G, R, Index, FC);
  end;

end;

initialization

  TCustomStyleEngine.RegisterStyleHook(TscGPVertPageControlPage, TscScrollBoxStyleHook);

finalization

  {$IFNDEF VER230}
  TCustomStyleEngine.UnRegisterStyleHook(TscGPVertPageControlPage, TscScrollBoxStyleHook);
  {$ENDIF}


end.

