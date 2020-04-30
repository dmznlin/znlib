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

unit scStyledForm;

{$I scdefine.inc}
{$R-}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
    Vcl.Controls, Vcl.Graphics, Vcl.Themes, Vcl.ImgList, Vcl.Menus, Vcl.Forms,
    Vcl.Styles, scImageCollection, System.UITypes,
    Vcl.StdCtrls, scDrawUtils, scHint;

type
  TscStyledForm = class;
  TscNCObject = class;

  TscFormStyleHook = class(TMouseTrackControlStyleHook)
  strict private type
    {$REGION 'TscMainMenuBarStyleHook'}
    TscMainMenuBarStyleHook = class
    strict private type
      TMenuBarButton = record
        Index: Integer;
        State: TThemedWindow;
        ItemRect: TRect;
      end;
    public type
      TMenuBarItem = record
        Index: Integer;
        State: TThemedMenu;
        MenuItem: TMenuItem;
        ItemRect: TRect;
      end;
    strict private
      class var FCurrentMenuItem: TMenuItem;
      class var FMenuBarHook: TscMainMenuBarStyleHook;
      class function PopupMenuHook(Code: Integer; WParam: WPARAM; var Msg: TMsg): LRESULT; stdcall; static;
    strict private
      FActiveItem: Integer;
      FBoundsRect: TRect;
      FEnterWithKeyboard: Boolean;
      FFormHook: TscFormStyleHook;
      FIcon: TIcon;
      FIconHandle: HICON;
      FInMenuLoop: Boolean;
      FItemCount: Integer;
      FItems: array of TMenuBarItem;
      FHotMDIButton, FOldMDIHotButton: Integer;
      FMDIButtons: array[0..2] of TMenuBarButton;
      FMDIChildSysMenuActive: Boolean;
      FMDIChildSystemMenuTracking: Boolean;
      FMenuActive: Boolean;
      FMenuHook: HHOOK;
      FMenuPush: Boolean;
      FMouseInMainMenu: Boolean;
      FMustActivateMDIChildSysMenu: Boolean;
      FMustActivateMenuItem: Boolean;
      FMustActivateSysMenu: Boolean;
      FOldActiveItem: Integer;
      FOldCursorPos: TPoint;
      FPressedMDIButton: Integer;
      FShowMDIButtons: Boolean;
      FSelectFirstItem: Boolean;
      FSysMenuActive: Boolean;
      FSystemMenuTracking: Boolean;
      FLastItemHasSubMenu: Boolean;
      function CanFindPriorItem(AMenuItem: TMenuItem): Boolean;
      function CanFindNextItem(AMenuItem: TMenuItem): Boolean;
      function CanTrackMDISystemMenu: Boolean;
      function CanTrackSystemMenu: Boolean;
      procedure DrawItem(AItem: TMenuBarItem; ACanvas: TCanvas);
      function FindFirstMenuItem(AUpdateMenu: Boolean): Integer;
      function FindFirstRightMenuItem(AUpdateMenu: Boolean): Integer;
      function FindHotKeyItem(CharCode: Integer; AUpdateMenu: Boolean): Integer;
      function FindItem(Value: NativeUInt; Kind: TFindItemKind): TMenuItem;
      function FindNextMenuItem(AUpdateMenu: Boolean): Integer;
      function FindPriorMenuItem(AUpdateMenu: Boolean): Integer;
      function GetIcon: TIcon;
      function GetIconFast: TIcon;
      function GetMenuItemWidth(AMenuItem: TMenuItem; ACanvas: TCanvas): Integer;
      function GetTrackMenuPos(AItem: TMenuBarItem): TPoint;
      procedure HookMenus;
      function IsSubMenuItem(AMenuItem: TMenuItem): Boolean;
      function ItemFromCursorPos: Integer;
      function ItemFromPoint(X, Y: Integer): Integer;
      function MainMenu: TMainMenu;
      procedure MenuExit;
      function MDIButtonFromPoint(X, Y: Integer): Integer;
      procedure MDIChildClose;
      procedure MDIChildMinimize;
      procedure MDIChildRestore;
      procedure SetBoundsRect(const ABoundsRect: TRect);
      procedure SetShowMDIButtons(Value: Boolean);
      procedure TrackMenuFromItem;
      procedure UnHookMenus;
    public
      constructor Create(FormHook: TscFormStyleHook);
      destructor Destroy; override;
      function CheckHotKeyItem(ACharCode: Word): Boolean;
      function GetMenuHeight(AWidth: Integer): Integer;
      procedure Invalidate;
      procedure MenuEnter(ATrackMenu: Boolean);
      procedure MouseDown(X, Y: Integer);
      procedure MouseMove(X, Y: Integer);
      procedure MouseUp(X, Y: Integer);
      procedure Paint(Canvas: TCanvas);
      procedure ProcessMenuLoop(ATrackMenu: Boolean);
      procedure TrackSystemMenu;
      procedure TrackMDIChildSystemMenu;
      property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
      property InMenuLoop: Boolean read FInMenuLoop write FInMenuLoop;
      property EnterWithKeyboard: Boolean read FEnterWithKeyboard write FEnterWithKeyboard;
      property MenuActive: Boolean read FMenuActive write FMenuActive;
      property MustActivateMDIChildSysMenu: Boolean read FMustActivateMDIChildSysMenu write FMustActivateMDIChildSysMenu;
      property MustActivateSysMenu: Boolean read FMustActivateSysMenu write FMustActivateSysMenu;
      property MustActivateMenuItem: Boolean read FMustActivateMenuItem write FMustActivateMenuItem;
      property ShowMDIButtons: Boolean read FShowMDIButtons write SetShowMDIButtons;
      property MouseInMainMenu: Boolean read FMouseInMainMenu;
    end;
    {$ENDREGION}
  strict private const
    WM_NCUAHDRAWCAPTION = $00AE;
  strict private
    FCaptionRect: TRect;
    FChangeSizeCalled: Boolean;
    FChangeVisibleChildHandle: HWND;
    FCloseButtonRect: TRect;
    FFormActive: Boolean;
    FHotButton: Integer;
    FHeight: Integer;
    FHelpButtonRect: TRect;
    FIcon: TIcon;
    FIconHandle: HICON;
    FMainMenuBarHook: TscMainMenuBarStyleHook;
    FMaxButtonRect: TRect;
    FMDIClientInstance: Pointer;
    FMDIHorzScrollBar: TWinControl;
    FMDIPrevClientProc: Pointer;
    FMDIScrollSizeBox: TWinControl;
    FMDIStopHorzScrollBar: Boolean;
    FMDIStopVertScrollBar: Boolean;
    FMDIVertScrollBar: TWinControl;
    FMinButtonRect: TRect;
    FLeft: Integer;
    FNeedsUpdate: Boolean;
    FOldHorzSrollBarPosition: Integer;
    FOldVertSrollBarPosition: Integer;
    FPressedButton: Integer;
    FRegion: HRGN;
    FStopCheckChildMove: Boolean;
    FSysMenuButtonRect: TRect;
    FTop: Integer;
    FWidth: Integer;
    FCaptionEmulation: Boolean;
    FRestoring, FSelfRestoring: Boolean;
    FRestoringConstraints: TSizeConstraints;
    FYOffset, FXOffset: Integer;
    FChildRestoring: Boolean;
    procedure AdjustMDIScrollBars;
    procedure ChangeSize;
    function IsStyleBorder: Boolean;
    function GetBorderSize: TRect;
    function GetForm: TCustomForm; inline;
    function GetIconFast: TIcon;
    function GetIcon: TIcon;
    function GetHitTest(P: TPoint): Integer;
    procedure GetMDIScrollInfo(SetRange: Boolean);
    function GetMDIWorkArea: TRect;
    function GetRegion: HRgn;
    procedure InitMDIScrollBars;
    function MDIChildMaximized: Boolean;
    procedure MDIHorzScroll(Offset: Integer);
    procedure MDIVertScroll(Offset: Integer);
    function NormalizePoint(P: TPoint): TPoint;
    procedure UpdateForm;
    procedure OnMDIHScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnMDIVScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure CMDialogChar(var Message: TWMKey); message CM_DIALOGCHAR;
    procedure CMMenuChanged(var Message: TMessage); message CM_MENUCHANGED;
    procedure WMInitMenu(var Message: TMessage); message WM_INITMENU;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCActivate(var Message: TMessage); message WM_NCACTIVATE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMSize(var Message: TWMSIZE); message WM_SIZE;
    procedure WMMove(var Message: TWMMOVE); message WM_MOVE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCMouseMove(var Message: TWMNCHitMessage); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonDown(var Message: TWMNCHitMessage); message WM_NCLBUTTONDOWN;
    procedure WMNCRButtonDown(var Message: TWMNCHitMessage); message WM_NCRBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCHitMessage); message WM_NCLBUTTONUP;
    procedure WMNCRButtonUp(var Message: TWMNCHitMessage); message WM_NCRBUTTONUP;
    procedure WMNCLButtonDblClk(var Message: TWMNCHitMessage); message WM_NCLBUTTONDBLCLK;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCUAHDrawCaption(var Message: TMessage); message WM_NCUAHDRAWCAPTION;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
    procedure WMMDIChildMove(var Message: TMessage); message WM_MDICHILDMOVE;
    procedure WMMDIChildClose(var Message: TMessage); message WM_MDICHILDCLOSE;
    procedure WMSysCommand(var Message: TMessage); message WM_SYSCOMMAND;
    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;
  strict protected
    FStyledForm: TscStyledForm;
    // nc objects
    FOldNCObject: TscNCObject;
    FActiveNCObject: TscNCObject;
    FCapturedNCObject: TscNCObject;
    FStyledCaptionHeight: Integer;
    //
    function GetStyledForm: TscStyledForm;
    procedure Close; virtual;
    procedure Help; virtual;
    procedure Maximize; virtual;
    procedure MDIClientWndProc(var Message: TMessage); virtual;
    procedure Minimize; virtual;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure Restore; virtual;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    property Handle;
    property Form: TCustomForm read GetForm;
    property StyledForm: TscStyledForm read GetStyledForm;
  end;

  TscNCObject = class(TObject)
  protected
    Parent: TCollectionItem;
    function GetStyledForm: TscStyledForm; virtual;
    function GetObjectWidth(AWidth: Integer; ACanvas: TCanvas): Integer; virtual;
  public
    FMouseIn: Boolean;
    FDown: Boolean;
    ObjectRect: TRect;
    constructor Create(AParent: TCollectionItem);
    procedure MouseDown(X, Y: Integer); virtual;
    procedure MouseUp(X, Y: Integer); virtual;
    procedure MouseMove(X, Y: Integer); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure Invalidate; virtual;
     property StyledForm: TscStyledForm read GetStyledForm;
  end;

  TscNCButtonItem = class;
  TscNCTabItem = class;

  TscNCButtonObject = class(TscNCObject)
  protected
    ButtonItem: TscNCButtonItem;
    function GetStyledForm: TscStyledForm; override;
    function GetObjectWidth(AWidth: Integer; ACanvas: TCanvas): Integer; override;
    procedure TrackPopupMenu;
    procedure TrackCustomWindow;
    procedure CloseUp(AAcceptChanges: Boolean);
  public
    FDroppedDown: Boolean;
    FAllowAllUpCheck: Boolean;
    constructor Create(AParent: TCollectionItem);
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TscNCTabObject = class(TscNCObject)
  protected
    TabItem: TscNCTabItem;
    function GetStyledForm: TscStyledForm; override;
    function GetObjectWidth(AWidth: Integer; ACanvas: TCanvas): Integer; override;
  public
    constructor Create(AParent: TCollectionItem);
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TscBlurAmount = 0..20;

  TscClientInActivePanel = class(TCustomControl)
  private
    Buffer: TBitmap;
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure ShowPanel(AForm: TCustomForm; AColor: TColor; AColorAlpha: Byte;
      ABlurAmount: TscBlurAmount);
    procedure HidePanel;
  end;

  TscNCButtonStyle = (scncPushButton, scncToolButton,
    scncPushButtonTransparent, scncToolButtonTransparent, scncTransparent,
    scncLink);

  TscNCButtonPosition = (scbpLeft, scbpRight);

  TscPaintNCButtonEvent = procedure (AIndex: Integer;
    ACanvas: TCanvas; ARect: TRect; AMouseIn, ADown, ADroppedDown: Boolean) of object;

  TscPaintParamsNCButtonEvent = procedure (AIndex: Integer;
    ACanvas: TCanvas; ARect: TRect; AMouseIn, ADown, ADroppedDown: Boolean; var AHandled: Boolean) of object;

  TscNCMouseXYEvent = procedure(AIndex: Integer; X, Y: Integer) of object;
  TscNCMouseEvent = procedure(AIndex: Integer) of object;

  TscNCButtonCloseUpEvent = procedure(AIndex: Integer; AAcceptChanges: Boolean) of object;
  TscNCButtonDropDownEvent = procedure(AIndex: Integer; X, Y: Integer) of object;

  TscNCButtonItem = class(TCollectionItem)
  protected
    FImageIndex: Integer;
    FHotImageIndex: Integer;
    FPressedImageIndex: Integer;
    FEnabled: Boolean;
    FVisible: Boolean;
    FPopupMenu: TPopupMenu;
    FSplitButton: Boolean;
    FCaption: String;
    FStyle: TscNCButtonStyle;
    FHeight: Integer;
    FWidth: Integer;
    FMargin: Integer;
    FContentMargin: Integer;
    FMarginLeft, FMarginRight,
    FMarginTop, FMarginBottom: Integer;
    FPosition: TscNCButtonPosition;
    FSpacing: Integer;
    FGroupIndex: Integer;
    FDown: Boolean;
    FAllowAllUp: Boolean;
    FHint: String;
    FCustomDropDown: Boolean;

    FOnGetPaintParams: TscPaintParamsNCButtonEvent;
    FOnClick: TNotifyEvent;
    FOnPaint: TscPaintNCButtonEvent;
    FOnMouseUp,
    FOnMouseDown,
    FOnMouseMove: TscNCMouseXYEvent;
    FOnMouseEnter,
    FOnMouseLeave: TscNCMouseEvent;

    FOnCloseUp: TscNCButtonCloseUpEvent;
    FOnDropDown: TscNCButtonDropDownEvent;

    procedure SetImageIndex(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetCaption(Value: String);
    procedure SetSplitButton(Value: Boolean);
    procedure SetStyle(Value: TscNCButtonStyle);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetMarginLeft(Value: Integer);
    procedure SetMarginRight(Value: Integer);
    procedure SetMarginTop(Value: Integer);
    procedure SetMarginBottom(Value: Integer);
    procedure SetPosition(Value: TscNCButtonPosition);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure SetContentMargin(Value: Integer);
    procedure SetDown(Value: Boolean);
  public
    NCObject: TscNCButtonObject;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AllowAllUp: Boolean
      read FAllowAllUp write FAllowAllUp;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property HotImageIndex: Integer read FHotImageIndex
      write FHotImageIndex default -1;
    property PressedImageIndex: Integer read FPressedImageIndex
      write FPressedImageIndex default -1;
    property Down: Boolean read FDown write SetDown;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property Visible: Boolean read FVisible write SetVisible;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Caption: String read FCaption write SetCaption;
    property SplitButton: Boolean read FSplitButton write SetSplitButton;
    property Style: TscNCButtonStyle read FStyle write SetStyle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
    property MarginRight: Integer read FMarginRight write SetMarginRight;
    property MarginBottom: Integer read FMarginBottom write SetMarginBottom;
    property Position: TscNCButtonPosition read FPosition write SetPosition;
    property Spacing: Integer read FSpacing write SetSpacing;
    property Margin: Integer read FMargin write SetMargin;
    property ContentMargin: Integer read FContentMargin write SetContentMargin;
    property CustomDropDown: Boolean read
      FCustomDropDown write FCustomDropDown;
    property Hint: String read FHint write FHint;
    property OnPaint: TscPaintNCButtonEvent read FOnPaint write FOnPaint;
    property OnGetPaintParams: TscPaintParamsNCButtonEvent
      read FOnGetPaintParams write FOnGetPaintParams;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseEnter: TscNCMouseEvent read
      FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TscNCMouseEvent read
      FOnMouseLeave write FOnMouseLeave;
    property OnMouseUp: TscNCMouseXYEvent read
      FOnMouseUp write FOnMouseUp;
    property OnMouseDown: TscNCMouseXYEvent read
      FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TscNCMouseXYEvent read
      FOnMouseMove write FOnMouseMove;
    property OnCloseUp: TscNCButtonCloseUpEvent
      read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TscNCButtonDropDownEvent
      read FOnDropDown write FOnDropDown;
  end;

  TscNCButtonItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscNCButtonItem;
    procedure SetItem(Index: Integer; Value:  TscNCButtonItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    StyledForm: TscStyledForm;
    constructor Create(AStyledForm: TscStyledForm);
    property Items[Index: Integer]: TscNCButtonItem read GetItem write SetItem; default;
    function Add: TscNCButtonItem;
    function Insert(Index: Integer): TscNCButtonItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscPaintNCTabEvent = procedure(AIndex: Integer;
    ACanvas: TCanvas; ARect: TRect; AMouseIn, AActive: Boolean) of object;

  TscPaintParamsNCTabEvent = procedure(AIndex: Integer;
    ACanvas: TCanvas; ARect: TRect; AMouseIn, AActive: Boolean; var AHandled: Boolean) of object;

  TscNCHitTestEvent = procedure (APoint: TPoint; var AResult: Integer) of object;

  TscNCTabItem = class(TCollectionItem)
  protected
    FImageIndex: Integer;
    FVisible: Boolean;
    FEnabled: Boolean;
    FCaption: String;
    FHeight: Integer;
    FWidth: Integer;
    FMarginTop: Integer;
    FContentMargin: Integer;
    FSpacing: Integer;
    FHint: String;
    FOnClick: TNotifyEvent;
    FOnPaint: TscPaintNCTabEvent;
    FOnGetPaintParams: TscPaintParamsNCTabEvent;
    FOnMouseUp,
    FOnMouseDown,
    FOnMouseMove: TscNCMouseXYEvent;
    FOnMouseEnter,
    FOnMouseLeave: TscNCMouseEvent;
    procedure SetContentMargin(Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    procedure SetCaption(Value: String);
    procedure SetVisible(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetMarginTop(Value: Integer);
    procedure SetSpacing(Value: Integer);
  public
    NCObject: TscNCTabObject;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Caption: String read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
    property ContentMargin: Integer
      read FContentMargin write SetContentMargin;
    property Spacing: Integer read FSpacing write SetSpacing;
     property Hint: String read FHint write FHint;
    property OnPaint: TscPaintNCTabEvent
      read FOnPaint write FOnPaint;
    property OnGetPaintParams: TscPaintParamsNCTabEvent
      read FOnGetPaintParams write FOnGetPaintParams;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseEnter: TscNCMouseEvent read
      FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TscNCMouseEvent read
      FOnMouseLeave write FOnMouseLeave;
    property OnMouseUp: TscNCMouseXYEvent read
      FOnMouseUp write FOnMouseUp;
    property OnMouseDown: TscNCMouseXYEvent read
      FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TscNCMouseXYEvent read
      FOnMouseMove write FOnMouseMove;
  end;

  TscNCTabItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscNCTabItem;
    procedure SetItem(Index: Integer; Value:  TscNCTabItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    StyledForm: TscStyledForm;
    constructor Create(AStyledForm: TscStyledForm);
    property Items[Index: Integer]: TscNCTabItem read GetItem write SetItem; default;
    function Add: TscNCTabItem;
    function Insert(Index: Integer): TscNCTabItem;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TscNCTabsPosition = (sctpLeft, sctpRight);

  TscOnChangeScaleEvent = procedure(AScaleFactor: Double) of object;
  TscOnFormDropDownEvent = procedure(ADropDownForm: TCustomForm; ADropDownControl: TControl) of object;
  TscOnFormCloseUpEvent = procedure(ADropDownForm: TCustomForm; ADropDownControl: TControl; AAcceptChanges: Boolean) of object;

  TscHitTestWnd = class(TForm)
  protected
    FStyledForm: TscStyledForm;
    FRgn: HRGN;
    function NormalizePoint(P: TPoint): TPoint;
    function GetHitTest(P: TPoint): Integer;
    procedure UpdateRegion(W, H: Integer);
    procedure WndProc(var Message: TMessage); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    FBorderSize: Integer;
    FDragging: Boolean;
    constructor CreateEx(AOwner: TComponent; AStyledForm: TscStyledForm);
    procedure UpdatePosition;
    procedure UpdateFormPosition(var X, Y, W, H: Integer);
  end;

  TscFluentUIBackground = (scfuibNone, scfuibBlur, scfuibAcrylic);

  TscStyledForm = class(TComponent)
  private
    FPPI: Integer;
    FShowStylesMenu: Boolean;
    FStylesMenuSorted: Boolean;
    FDropDownBorderColor: TColor;
    FStylesMenu: HMenu;
    FOldWndProc: TWndMethod;
    FStyleCount: Integer;
    FSubMenuIndex: Integer;
    FStylesMenuCaption: String;
    FClientWidth, FClientHeight: Integer;
    FFormLeft, FFormTop, FFormWidth, FFormHeight: Integer;
    FSizeStored: Boolean;
    FHintComponent: TscHint;
    FShowHints: Boolean;
    FOnStyleChanged: TNotifyEvent;
    FOnHitTest: TscNCHitTestEvent;
    FWasMinimized: Boolean;
    FStopUpdateFluentColor: Boolean;
    // Windows 10 Fluient UI
    FFluentUIBackground: TscFluentUIBackground;
    FFluentUIAcrylicColor: TColor;
    FFluentUIAcrylicColorAlpha: Byte;
    FFluentUIBorder: Boolean;
    FFluentUIInactiveAcrylicColorOpaque: Boolean;
    // nc objects
    FButtons: TscNCButtonItems;
    FButtonImages: TCustomImageList;
    FButtonFont: TFont;
    FCaptionFont: TFont;
    FShowButtons: Boolean;
    FTabs: TscNCTabItems;
    FTabFont: TFont;
    FTabImages: TCustomImageList;
    FTabIndex: Integer;
    FTabsPosition: TscNCTabsPosition;
    FOnTabChanged: TNotifyEvent;
    FShowTabs: Boolean;
    FShowIcon: Boolean;
    FShowInactiveTab: Boolean;
    //
    FUpdating: Boolean;
    FCaptionAlignment: TAlignment;
    FShowed: Boolean;
    FOnBeforeChangeScale: TNotifyEvent;
    FOnChangeScale: TscOnChangeScaleEvent;
    FScaleFactor: Double;
    FScalePercent: Integer;
    FDropDownForm: Boolean;
    FDropDownAnimation: Boolean;
    FDropDownButtonItem: TscNCButtonItem;
    FOnDropDown: TscOnFormDropDownEvent;
    FOnCloseUp: TscOnFormCloseUpEvent;
    FOnChangeActive: TNotifyEvent;
    //
    FInActivePanel: TscClientInActivePanel;
    FInActiveClientColor: TColor;
    FInActiveClientColorAlpha: Byte;
    FInActiveClientBlurAmount: TscBlurAmount;
    //
    FDWMClientShadow: Boolean;
    FDWMClientShadowHitTest: Boolean;

    FDWMHitTestWnd: TscHitTestWnd;
    //
    FDWMClientNormalRect: TRect;
    FDWMClientDown: Boolean;
    FDWMClientDownPoint: TPoint;
    FOnDWMClientMaximize: TNotifyEvent;
    FOnDWMClientRestore: TNotifyEvent;
    //
    FCaptionWallpapers: TscCustomImageCollection;
    FCaptionWallpaperIndex: Integer;
    FCaptionWallpaperInActiveIndex: Integer;

    FCaptionWallpaperLeftMargin: Integer;
    FCaptionWallpaperTopMargin: Integer;
    FCaptionWallpaperRightMargin: Integer;
    FCaptionWallpaperBottomMargin: Integer;
    //
    FStartUp: Boolean;

    procedure SetFluentUIBackground(Value: TscFluentUIBackground);
    procedure SetFluentUIAcrylicColor(Value: TColor);
    procedure SetFluentUIAcrylicColorAlpha(Value: Byte);
    procedure SetFluentUIBorder(Value: Boolean);

    procedure SetCaptionWallpaperLeftMargin(Value: Integer);
    procedure SetCaptionWallpaperTopMargin(Value: Integer);
    procedure SetCaptionWallpaperRightMargin(Value: Integer);
    procedure SetCaptionWallpaperBottomMargin(Value: Integer);

    procedure SetCaptionWallpaperIndex(Value: Integer);
    procedure SetCaptionWallpapers(Value: TscCustomImageCollection);

    procedure SetDWMClientShadow(Value: Boolean);
    procedure SetDWMClientShadowHitTest(Value: Boolean);
    procedure SetDropDownForm(Value: Boolean);
    procedure SetShowInactiveTab(Value: Boolean);
    procedure SetShowButtons(Value: Boolean);
    procedure SetShowTabs(Value: Boolean);
    procedure SetButtonFont(Value: TFont);
    procedure SetCaptionFont(Value: TFont);
    procedure SetTabFont(Value: TFont);
    procedure SetButtons(Value: TscNCButtonItems);
    procedure SetTabs(Value: TscNCTabItems);
    procedure SetShowIcon(Value: Boolean);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabsPosition(Value: TscNCTabsPosition);
    procedure SetCaptionAlignment(Value: TAlignment);
    // nc objects
    function FindNCObject(AObject: TscNCObject): Boolean;
    function FindNCObjectFromPoint(P: TPoint): TscNCObject;
    function IsNCObjectEnabled(AObject: TscNCObject): Boolean;
    //
    procedure NewWndProc(var Message: TMessage);
    procedure CreateSystemStyleMenu;
    procedure DeleteSystemStyleMenu;
    procedure SetStyleByIndex(AIndex: Integer);
    procedure SetShowStylesMenu(Value: Boolean);
    procedure SetStylesMenuCaption(Value: String);
    procedure SetClientWidth(Value: Integer);
    procedure SetClientHeight(Value: Integer);
    procedure CheckMDIChilds;
    function GetBorderScaleFactor: Double;
  protected
    FForm: TCustomForm;
    FDropDownControl: TControl;
    FMaximized, FStyleChanged, FMinimized, FNCDrag: Boolean;
    FDWMClientMaximized: Boolean;
    FStopActivateEvents: Boolean;
    FHitTestWndShowing: Boolean;
    FStyleChanging: Boolean;
    FWasDWMClientShadow: Boolean;
    FHitTestWndDown: Boolean;
    FMenuTracking: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure ShowDWMHitTestWnd;
    procedure HideDWMHitTestWnd;
    procedure SetDWMFrame;
    procedure UpdateFluentWindow;
  public
    FDPIChanged: Boolean;
    function IsFluentUIEnabled: Boolean;
    function IsFluentUIAvailable: Boolean;
    procedure UpDateFormNC;
    procedure RestoreClientSize;
    procedure BeginUpdateItems;
    procedure EndUpdateItems(AUpdate: Boolean);
    procedure DropDown(ADropDownForm: TCustomForm; X, Y: Integer); overload;
    procedure DropDown(ADropDownControl: TControl; X, Y: Integer); overload;
    procedure DropDown(ADropDownControl: TControl; AFromRight: Boolean = False; AIsRightToLeft: Boolean = False); overload;
    procedure DropDown(ADropDownForm: TCustomForm; ANCButton: TscNCButtonItem; X, Y: Integer; AIsRightToLeft: Boolean = False); overload;
    procedure CloseUp(AAcceptChanges: Boolean);
    procedure ShowClientInActiveEffect;
    procedure HideClientInActiveEffect;
    procedure DWMClientMaximize;
    procedure DWMClientRestore;
    function IsDWMClientMaximized: Boolean;

    procedure DWMClientStartDrag;
    procedure DWMClientDrag;
    procedure DWMClientEndDrag;
    function IsDWMClientDragging: Boolean;

    function ScaleInt(AValue: Integer): Integer;
    function ScaleDouble(AValue: Double): Double;

    procedure SetRedraw(AValue: Boolean; AUpdate: Boolean = True);

    procedure UpdateFluentAcrylicColor;

    function GetMaximizeBounds: TRect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ScaleFactor: Double read FScaleFactor;
    property BorderScaleFactor: Double read GetBorderScaleFactor;
    property DWMClientNormalRect: TRect read FDWMClientNormalRect write FDWMClientNormalRect;

  published
    property FluentUIBackground: TscFluentUIBackground
      read FFluentUIBackground write SetFluentUIBackground;
    property FluentUIAcrylicColor: TColor
      read FFluentUIAcrylicColor write SetFluentUIAcrylicColor;
    property FluentUIAcrylicColorAlpha: Byte
      read FFluentUIAcrylicColorAlpha write SetFluentUIAcrylicColorAlpha;
    property FluentUIBorder: Boolean
      read FFluentUIBorder write SetFluentUIBorder;
    property FluentUIInactiveAcrylicColorOpaque: Boolean
      read FFluentUIInactiveAcrylicColorOpaque write FFluentUIInactiveAcrylicColorOpaque;
    property DWMClientShadow: Boolean
      read FDWMClientShadow write SetDWMClientShadow;
    property DWMClientShadowHitTest: Boolean
      read FDWMClientShadowHitTest write SetDWMClientShadowHitTest;
    property DropDownForm: Boolean
      read FDropDownForm write SetDropDownForm;
    property DropDownAnimation: Boolean
      read FDropDownAnimation write FDropDownAnimation;
    property DropDownBorderColor: TColor
      read FDropDownBorderColor write FDropDownBorderColor;
    property StylesMenuSorted: Boolean read FStylesMenuSorted write FStylesMenuSorted;
    property ShowStylesMenu: Boolean read FShowStylesMenu write SetShowStylesMenu;
    property StylesMenuCaption: String read FStylesMenuCaption write SetStylesMenuCaption;
    property ClientWidth: Integer read FClientWidth write SetClientWidth;
    property ClientHeight: Integer read FClientHeight write SetClientHeight;
    property HintComponent: TscHint
      read FHintComponent write FHintComponent;
    property ShowHints: Boolean
      read FShowHints write FShowHints;
    // nc objects
    property Buttons: TscNCButtonItems read FButtons write SetButtons;
    property ButtonImages: TCustomImageList read
      FButtonImages write FButtonImages;
    property ButtonFont: TFont read FButtonFont write SetButtonFont;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionAlignment: TAlignment
      read FCaptionAlignment write SetCaptionAlignment;
    property InActiveClientColor: TColor
      read FInActiveClientColor write FInActiveClientColor;
    property InActiveClientColorAlpha: Byte
      read FInActiveClientColorAlpha write FInActiveClientColorAlpha;
    property InActiveClientBlurAmount: TscBlurAmount
      read FInActiveClientBlurAmount write FInActiveClientBlurAmount;
    property Tabs: TscNCTabItems read FTabs write SetTabs;
    property TabImages: TCustomImageList read
      FTabImages write FTabImages;
    property TabFont: TFont read FTabFont write SetTabFont;
    property ShowIcon: Boolean read FShowIcon write SetShowIcon default True;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons;
    property ShowTabs: Boolean read FShowTabs write SetShowTabs;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property TabsPosition: TscNCTabsPosition read FTabsPosition write SetTabsPosition;
    property ShowInactiveTab: Boolean
      read FShowInactiveTab write SetShowInactiveTab;

    property CaptionWallpapers: TscCustomImageCollection read FCaptionWallpapers write SetCaptionWallpapers;
    property CaptionWallpaperIndex: Integer read FCaptionWallpaperIndex write SetCaptionWallpaperIndex;
    property CaptionWallpaperInActiveIndex: Integer read FCaptionWallpaperInActiveIndex write FCaptionWallpaperInActiveIndex;
    property CaptionWallpaperLeftMargin: Integer
      read FCaptionWallpaperLeftMargin write SetCaptionWallpaperLeftMargin;
    property CaptionWallpaperTopMargin: Integer
      read FCaptionWallpaperTopMargin write SetCaptionWallpaperTopMargin;
    property CaptionWallpaperRightMargin: Integer
      read FCaptionWallpaperRightMargin write SetCaptionWallpaperRightMargin;
    property CaptionWallpaperBottomMargin: Integer
      read FCaptionWallpaperBottomMargin write SetCaptionWallpaperBottomMargin;

    property OnTabChanged: TNotifyEvent read FOnTabChanged write FOnTabChanged;
    property OnStyleChanged: TNotifyEvent
      read FOnStyleChanged write FOnStyleChanged;
    property OnHitTest: TscNCHitTestEvent
      read FOnHitTest write FOnHitTest;
    property OnChangeActive: TNotifyEvent
      read FOnChangeActive write FOnChangeActive;
    property OnBeforeChangeScale: TNotifyEvent
      read FOnBeforeChangeScale write FOnBeforeChangeScale;
    property OnChangeScale: TscOnChangeScaleEvent
      read FOnChangeScale write FOnChangeScale;
    property OnDropDown: TscOnFormDropDownEvent
      read FOnDropDown write FOnDropDown;
    property OnCloseUp: TscOnFormCloseUpEvent
      read FOnCloseUp write FOnCloseUp;
    property OnDWMClientMaximize: TNotifyEvent
      read FOnDWMClientMaximize write FOnDWMClientMaximize;
    property OnDWMClientRestore: TNotifyEvent
      read FOnDWMClientRestore write FOnDWMClientRestore;
  end;

implementation
  uses System.Math, Winapi.UxTheme, Vcl.GraphUtil, WinApi.CommCtrl;

const
  WM_DPICHANGED = $02E0;
  DWMWA_EXCLUDED_FROM_PEEK = 12;
  DrawLeftBorderFlag = $20;
  DrawTopBorderFlag = $40;
  DrawRightBorderFlag = $80;
  DrawBottomBorderFlag = $100;

type
  THookForm = class(TCustomForm);
  TStyledFormHooksList = TStringList;
var
  StyledFormHookList: TStyledFormHooksList = nil;
  FParentDropDownForm: TCustomForm = nil;
  FActiveDropDownForm: TCustomForm = nil;

const
  WM_POSTSTYLECHANGED = WM_USER + 494;
  WM_CHECKMDICHILDS = WM_USER + 495;
  WM_CHILDRESTORING = WM_USER + 496;
  WM_CHILDRESTORED = WM_USER + 497;
  WM_RESTOREPOSITION = WM_USER + 498;
  WM_RESTORECLIENTSIZE = WM_USER + 499;

  SystemStylesMenu = WM_USER + 500;

procedure AddStyledFormHookList(AClassName: String);
begin
  StyledFormHookList.Add(AClassName);
end;

function FindStyledFormHookList(AClassName: String): Boolean;
begin
  Result := StyledFormHookList.IndexOf(AClassName) >= 0;
end;

function InsertStyleMenuItem(hMenu: HMENU; uPosition: UINT; uIDNewItem:
  UINT_PTR; lpNewItem, IconName: LPCWSTR) : BOOL;
var
  FMenuInfo: TMenuItemInfo;
begin
  FillChar(FMenuInfo, SizeOf(FMenuInfo), 0);
  FMenuInfo.cbSize := SizeOf(TMenuItemInfo);
  FMenuInfo.fMask := MIIM_FTYPE or MIIM_ID or MIIM_BITMAP or MIIM_STRING;
  FMenuInfo.fType := MFT_STRING;
  FMenuInfo.wID := uIDNewItem;
  FMenuInfo.dwTypeData := lpNewItem;
  Result := InsertMenuItem(hMenu, uPosition, True, FMenuInfo);
end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

function TscFormStyleHook.GetStyledForm: TscStyledForm;
var
  I: Integer;
begin
  Result := FStyledForm;
  if Result = nil then
    for I := 0 to Form.ComponentCount-1 do
     if Form.Components[i] is TscStyledForm then
      begin
        FStyledForm := TscStyledForm(Form.Components[I]);
        Result := FStyledForm;
        Break;
      end;
end;

constructor TscFormStyleHook.TscMainMenuBarStyleHook.Create(FormHook: TscFormStyleHook);
begin
  FFormHook := FormHook;
  FBoundsRect := Rect(0, 0, 0, 0);
  FIcon := nil;
  FItemCount := 0;
  FMenuActive := False;
  FMenuPush := False;
  FActiveItem := -1;
  FOldActiveItem := -1;
  FMouseInMainMenu := False;
  FMenuBarHook := nil;
  FOldCursorPos := Point(-1, -1);
  FEnterWithKeyboard := False;
  FSystemMenuTracking := False;
  FMDIChildSystemMenuTracking := False;
  FShowMDIButtons := False;
  FHotMDIButton := -1;
  FPressedMDIButton := -1;
  FOldMDIHotButton := -1;
  FMustActivateSysMenu := False;
  FMustActivateMenuItem := False;
  FMustActivateMDIChildSysMenu := False;
  FSysMenuActive := False;
  FMDIChildSysMenuActive := False;
end;

destructor TscFormStyleHook.TscMainMenuBarStyleHook.Destroy;
begin
  if FIcon <> nil then
    FreeAndNil(FIcon);
  inherited;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.GetIconFast: TIcon;
begin
  if (FIcon = nil) or (FIconHandle = 0) then
    Result := GetIcon
  else
    Result := FIcon;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.GetIcon: TIcon;
var
  IconX, IconY : Integer;
  TmpHandle: Integer;
  ChildForm: TCustomForm;
  B: Boolean;
begin
  if not CanTrackMDISystemMenu then
  begin
    Result := nil;
    Exit;
  end;

  ChildForm := THookForm(FFormHook.Form).ActiveMDIChild;
  if ChildForm = nil then
  begin
    Result := nil;
    Exit;
  end;

  if FIcon = nil then
   FIcon := TIcon.Create;

  B := False;
  if THookForm(ChildForm).Icon.Handle <> 0 then
    TmpHandle := THookForm(ChildForm).Icon.Handle
  else
  if Application.Icon.Handle <> 0 then
    TmpHandle := Application.Icon.Handle
  else
  begin
    TmpHandle := LoadIcon(0, IDI_APPLICATION);
    B := True;
  end;

  IconX := {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CXSMICON);
  if IconX = 0 then
    IconX := {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CXSIZE);
  IconY := {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYSMICON);
  if IconY = 0 then
    IconY := {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYSIZE);

  if (FFormHook.StyledForm <> nil) and not THookForm(FFormHook.Form).Scaled  then
  begin
    if IconX > 16 then IconX := 16;
    if IconY > 16 then IconY := 16;
  end;

  FIcon.Handle := CopyImage(TmpHandle, IMAGE_ICON, IconX, IconY, LR_COPYFROMRESOURCE);

  if B then
    DestroyIcon(TmpHandle);

  Result := FIcon;
end;


function TscFormStyleHook.TscMainMenuBarStyleHook.CanTrackMDISystemMenu: Boolean;
begin
  Result := (THookForm(FFormHook.Form).FormStyle = fsMDIForm) and
            (THookForm(FFormHook.Form).ActiveMDIChild <> nil) and
            (biSystemMenu in THookForm(FFormHook.Form).ActiveMDIChild.BorderIcons);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.CanTrackSystemMenu: Boolean;
begin
  Result := (biSystemMenu in THookForm(FFormHook.Form).BorderIcons) and
    (FFormHook.Form.BorderStyle <> bsNone);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.SetShowMDIButtons(Value: Boolean);
begin
  if FShowMDIButtons <> Value then
  begin
    FShowMDIButtons := Value;
    FHotMDIButton := -1;
    FPressedMDIButton := -1;
    FOldMDIHotButton := -1;
    if not Value and (FIcon <> nil) then
      FreeAndNil(FIcon);
    Invalidate;
  end;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.IsSubMenuItem(AMenuItem: TMenuItem): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FItemCount - 1 do
   if AMenuItem.Parent = FItems[I].MenuItem then
     Exit(False);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.CanFindPriorItem(AMenuItem: TMenuItem): Boolean;
begin
  Result := (AMenuItem = nil) or not IsSubMenuItem(AMenuItem);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.CanFindNextItem(AMenuItem: TMenuItem): Boolean;
begin
  Result := (AMenuItem = nil) or (AMenuItem.Count = 0);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.FindItem(Value: NativeUInt; Kind: TFindItemKind): TMenuItem;
begin
  Result := MainMenu.FindItem(Value, Kind);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.MenuEnter;
begin
  HideCaret(0);
  FMDIChildSysMenuActive := False;
  FSysMenuActive := False;
  if not ATrackMenu then
    FindFirstMenuItem(True);
  ProcessMenuLoop(ATrackMenu);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.MenuExit;
begin
  ShowCaret(0);
  FInMenuLoop := False;
  FMenuPush := False;
  FMenuActive := False;
  FEnterWithKeyboard := False;
  FMDIChildSysMenuActive := False;
  FSysMenuActive := False;
  if (FActiveItem <> -1) and
     (WindowFromPoint(Mouse.CursorPos) = FFormHook.Handle) and
     (ItemFromCursorPos <> -1) then
  begin
    FActiveItem := ItemFromCursorPos;
    FOldActiveItem := FActiveItem;
  end
  else
  begin
    FActiveItem := -1;
    FOldActiveItem := -1;
  end;
  Invalidate;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.CheckHotKeyItem(ACharCode: Word): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := FindHotKeyItem(ACharCode, True);
  if (I <> -1) and (FActiveItem = I) then
  begin
    Result := True;
    if FItems[FActiveItem].MenuItem.Count = 0 then
    begin
      MenuExit;
      if FItems[I].MenuItem.GetParentMenu <> nil then
        FItems[I].MenuItem.GetParentMenu.DispatchCommand(FItems[I].MenuItem.Command);
    end
    else
    begin
      FEnterWithKeyboard := True;
      TrackMenuFromItem;
    end;
  end;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.ProcessMenuLoop;
var
  Msg: TMsg;
  FDispatchMessage: Boolean;
  I: Integer;
begin
  if FInMenuLoop then
    Exit;
  FInMenuLoop := True;

  repeat
    if ATrackMenu then
      TrackMenuFromItem;

    FDispatchMessage := False;

    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
      case Msg.message of
        WM_MOUSEMOVE:
          begin
          end;
        WM_SYSKEYDOWN:
         begin
           if Msg.wParam = VK_MENU then
           begin
             FInMenuLoop := False;
             FDispatchMessage := True;
           end;
         end;
        WM_QUIT:
          begin
            FInMenuLoop := False;
            PostQuitMessage(Msg.wParam);
          end;
        WM_CLOSE, CM_RELEASE:
          begin
            FInMenuLoop := False;
            FDispatchMessage := True;
          end;
        WM_KEYDOWN:
          begin
            if not FEnterWithKeyboard then
            begin
              FEnterWithKeyboard := True;
              Invalidate;
            end;
            I := FindHotKeyItem(Msg.WParam, True);
            if (I <> -1) and (FActiveItem = I) then
            begin
              if FItems[FActiveItem].MenuItem.Count = 0 then
              begin
                MenuExit;
                if FItems[I].MenuItem.GetParentMenu <> nil then
                  FItems[I].MenuItem.GetParentMenu.DispatchCommand(FItems[I].MenuItem.Command);
              end
              else
                TrackMenuFromItem;
            end
            else
              case Msg.WParam of
                VK_ESCAPE:
                  MenuExit;
                VK_RIGHT:
                  if FFormHook.Control.BiDiMode = bdRightToLeft then
                    FindPriorMenuItem(True)
                  else
                    FindNextMenuItem(True);
                VK_LEFT:
                  if FFormHook.Control.BiDiMode = bdRightToLeft then
                    FindNextMenuItem(True)
                  else
                    FindPriorMenuItem(True);
                VK_RETURN, VK_DOWN:
                  if FMDIChildSysMenuActive then
                  begin
                    MenuExit;
                    TrackMDIChildSystemMenu;
                  end
                  else if FSysMenuActive then
                  begin
                    MenuExit;
                    TrackSystemMenu;
                  end
                  else if FActiveItem <> -1 then
                  begin
                    if FItems[FActiveItem].MenuItem.Count = 0 then
                    begin
                      I := FActiveItem;
                      MenuExit;
                      if FItems[I].MenuItem.GetParentMenu <> nil then
                        FItems[I].MenuItem.GetParentMenu.DispatchCommand(FItems[I].MenuItem.Command);
                    end
                    else
                      TrackMenuFromItem;
                  end;
              end;
            end;
        WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN,
        WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN,
        WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP,
        WM_NCLBUTTONUP, WM_NCRBUTTONUP, WM_NCMBUTTONUP,
        WM_ACTIVATE, WM_NCACTIVATE, WM_SETFOCUS, WM_KILLFOCUS,
        WM_CANCELMODE:
          begin
            FInMenuLoop := False;
            FDispatchMessage := True;
          end;
         else
           DispatchMessage(Msg);
      end;

  until not FInMenuLoop;

  if not FMustActivateMenuItem then
  begin
    MenuExit;
    if (Msg.Message = WM_NCLBUTTONDOWN) and (FActiveItem >= 0) then
      FDispatchMessage := False;
  end;
  if FDispatchMessage then
    DispatchMessage(Msg);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.FindFirstMenuItem;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItemCount - 1 do
  begin
    if FItems[I].MenuItem.Visible and FItems[I].MenuItem.Enabled then
    begin
      Result := I;
      if AUpdateMenu then
      begin
        FActiveItem := I;
        Invalidate;
      end;
      Break;
    end;
  end;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.FindFirstRightMenuItem;
var
  I: Integer;
begin
  Result := -1;
  for I := FItemCount - 1 downto 0 do
  begin
    if FItems[I].MenuItem.Visible and FItems[I].MenuItem.Enabled then
    begin
      Result := I;
      if AUpdateMenu then
      begin
        FActiveItem := I;
        Invalidate;
      end;
      Break;
    end;
  end;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.FindHotKeyItem;
var
  i: Integer;
begin
  Result := -1;
  for I := 0 to FItemCount - 1 do
  begin
    if FItems[I].MenuItem.Visible and FItems[I].MenuItem.Enabled and
       IsAccel(CharCode, FItems[I].MenuItem.Caption) then
    begin
      Result := I;
      if AUpdateMenu then
      begin
        FActiveItem := I;
        Invalidate;
      end;
      Break;
    end;
  end;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.FindNextMenuItem;
var
  I, J: Integer;
begin
  Result := -1;
  if FActiveItem = -1 then J := 0 else  J := FActiveItem + 1;
  for I := J to FItemCount - 1 do
  begin
    if FItems[I].MenuItem.Visible and FItems[I].MenuItem.Enabled then
    begin
      Result := I;
      if AUpdateMenu then
      begin
        FActiveItem := I;
        Invalidate;
      end;
      Break;
    end;
  end;

  if (Result = -1) and not CanTrackSystemMenu then
    Result := FindFirstMenuItem(AUpdateMenu)
  else if (Result = -1) and CanTrackSystemMenu and not FMenuPush then
  begin
    if not FSysMenuActive and not FMDIChildSysMenuActive then
    begin
      FSysMenuActive := True;
      FMDIChildSysMenuActive := False;
      if AUpdateMenu then
        Invalidate;
    end
    else if CanTrackMDISystemMenu and not FMDIChildSysMenuActive then
    begin
      FSysMenuActive := False;
      FMDIChildSysMenuActive := True;
      if AUpdateMenu then
        Invalidate;
    end
    else
    begin
      FSysMenuActive := False;
      FMDIChildSysMenuActive := False;
      Result := FindFirstMenuItem(AUpdateMenu);
    end;
  end
  else if (Result = -1) and FMenuPush then
  begin
    if CanTrackSystemMenu and AUpdateMenu then
    begin
      MenuExit;
      TrackSystemMenu;
    end
    else if CanTrackMDISystemMenu and AUpdateMenu then
    begin
      MenuExit;
      TrackMDIChildSystemMenu;
    end
    else if FMenuHook = 0 then
      Result := FindFirstMenuItem(AUpdateMenu);
  end;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.FindPriorMenuItem;
var
  I, J: Integer;
begin
  Result := -1;
  if FActiveItem = -1 then
    J := FItemCount
  else
    J := FActiveItem - 1;

  for I := J downto 0 do
  begin
    if FItems[I].MenuItem.Visible and FItems[I].MenuItem.Enabled then
    begin
      Result := I;
      if AUpdateMenu then
      begin
        FActiveItem := I;
        Invalidate;
      end;
      Break;
    end;
  end;

  if (Result = -1) and not CanTrackSystemMenu then
    Result := FindFirstRightMenuItem(AUpdateMenu)
  else if (Result = -1) and CanTrackSystemMenu and not FMenuPush then
  begin
    if CanTrackMDISystemMenu and not FMDIChildSysMenuActive
       and not FSysMenuActive then
    begin
      FSysMenuActive := False;
      FMDIChildSysMenuActive := True;
      if AUpdateMenu then
        Invalidate;
    end
    else if not FSysMenuActive then
    begin
      FSysMenuActive := True;
      FMDIChildSysMenuActive := False;
      if AUpdateMenu then
        Invalidate;
    end
    else
    begin
      FSysMenuActive := False;
      FMDIChildSysMenuActive := False;
      Result := FindFirstRightMenuItem(AUpdateMenu);
    end;
  end
  else if (Result = -1) and FMenuPush then
  begin
    if CanTrackMDISystemMenu and AUpdateMenu then
    begin
      MenuExit;
      TrackMDIChildSystemMenu;
    end
    else if CanTrackSystemMenu and AUpdateMenu then
    begin
      MenuExit;
      TrackSystemMenu;
    end
    else if FMenuHook = 0 then
      Result := FindFirstRightMenuItem(AUpdateMenu);
  end;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.GetTrackMenuPos(AItem: TMenuBarItem): TPoint;
var
  RightPoint: TPoint;
begin
  Result := Point(AItem.ItemRect.Left, AItem.ItemRect.Top + AItem.ItemRect.Height);
  Result.X := Result.X + FFormHook.FLeft + FBoundsRect.Left;
  Result.Y := Result.Y + FFormHook.FTop + FBoundsRect.Top;
  RightPoint := Point(Result.X + AItem.ItemRect.Width, Result.Y);
  if Screen.MonitorFromPoint(Result) <> Screen.MonitorFromPoint(RightPoint)
  then
    begin
      if FFormHook.Control.BiDiMode <> bdRightToLeft then
        Result.X := Screen.MonitorFromPoint(RightPoint).WorkareaRect.Left
      else
        Result.X := Screen.MonitorFromPoint(Result).WorkareaRect.Right -
          AItem.ItemRect.Width - 1;
    end;
  if FFormHook.Control.BiDiMode = bdRightToLeft then
    Result.X := Result.X + AItem.ItemRect.Width;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.HookMenus;
begin
  FSelectFirstItem := True;
  FMenuBarHook := Self;
  FCurrentMenuItem := nil;
  if FMenuHook = 0 then
    FMenuHook := SetWindowsHookEx(WH_MSGFILTER, @PopupMenuHook, 0,
      GetCurrentThreadID);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.UnHookMenus;
begin
  if FMenuHook <> 0 then
    UnhookWindowsHookEx(FMenuHook);
  FMenuBarHook := nil;
  FCurrentMenuItem := nil;
  FMenuHook := 0;
  FSelectFirstItem := False;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.ItemFromCursorPos: Integer;
var
  P: TPoint;
begin
  P := Mouse.CursorPos;
  P.X := P.X - FFormHook.FLeft - FBoundsRect.Left;
  P.Y := P.Y - FFormHook.FTop - FBoundsRect.Top;
  Result := ItemFromPoint(P.X, P.Y);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.MDIChildClose;
begin
  if (THookForm(FFormHook.Form).ActiveMDIChild <> nil) then
    SendMessage(THookForm(FFormHook.Form).ActiveMDIChild.Handle,
      WM_SYSCOMMAND, SC_CLOSE, 0);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.MDIChildRestore;
begin
  if (THookForm(FFormHook.Form).ActiveMDIChild <> nil) then
    SendMessage(THookForm(FFormHook.Form).ActiveMDIChild.Handle,
      WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.MDIChildMinimize;
begin
  if (THookForm(FFormHook.Form).ActiveMDIChild <> nil) then
    SendMessage(THookForm(FFormHook.Form).ActiveMDIChild.Handle,
      WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.MDIButtonFromPoint(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to 2 do
    if FMDIButtons[I].ItemRect.Contains(Point(X, Y)) then
      Exit(FMDIButtons[I].Index);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.ItemFromPoint(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItemCount - 1 do
    if FItems[I].MenuItem.Visible and FItems[I].MenuItem.Enabled and
       FItems[I].ItemRect.Contains(Point(X, Y)) then
      Exit(FItems[I].Index);
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.Invalidate;
begin
  FFormHook.InvalidateNC;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.MainMenu: TMainMenu;
begin
  if THookForm(FFormHook.Form).FormStyle = fsMDIChild then
  begin
    Result := nil;
    Exit;
  end;
  Result := THookForm(FFormHook.Form).Menu;
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.GetMenuHeight(AWidth: Integer): Integer;

function GetItemCount(AMenu, AChildMenu: TMainMenu): Integer;

procedure Insert(APos: Integer; var ACount: Integer; AItem: TMenuItem);
var
  I: Integer;
begin
  Inc(ACount);
  if APos = ACount - 1 then
    FItems[APos].MenuItem := AItem
  else
  begin
    for I := ACount - 1 downto APos + 1 do
      FItems[I].MenuItem := FItems[I - 1].MenuItem;
    FItems[APos].MenuItem := AItem;
  end;
end;

function CanAddItem(AItem: TMenuItem): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to AChildMenu.Items.Count - 1 do
    if AItem.GroupIndex = AChildMenu.Items[I].GroupIndex then
    begin
      Result := False;
      Break;
    end;
end;

var
  I, J, Count, Index: Integer;
begin
  if AMenu = nil then
    Exit(0);

  if AChildMenu <> nil then
  begin
    Count := AMenu.Items.Count + AChildMenu.Items.Count;
    SetLength(FItems, Count);
    Result := AChildMenu.Items.Count;
    {add items from child menu}
    for I := 0 to Result - 1 do
      FItems[I].MenuItem := AChildMenu.Items[I];
    {add items from menu}
    for I := AMenu.Items.Count - 1 downto 0 do
      if CanAddItem(AMenu.Items[I]) then
      begin
        Index := -1;
        for J := 0 to Result - 1 do
          if AMenu.Items[I].GroupIndex <= FItems[J].MenuItem.GroupIndex then
          begin
            Index := J;
            Break;
          end;
        if Index = -1 then Index := Result;
        Insert(Index, Result, AMenu.Items[I]);
      end;
  end
  else
  begin
    {add items from menu}
    Result := AMenu.Items.Count;
    SetLength(FItems, Result);
    for I := 0 to Result - 1 do
      FItems[I].MenuItem := AMenu.Items[I];
  end;
end;

var
  Buffer: TBitmap;
  I, LHeight: Integer;
  LWidth, LButtonWidth: Integer;
  LIconDraw: Boolean;
  FMenu, FChildMenu: TMainMenu;
begin
  Result := {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU);
  if MainMenu = nil then
    Exit;

  if FShowMDIButtons then
    LButtonWidth := Result * 3
  else
    LButtonWidth := 0;

  {get menu}
  FMenu := MainMenu;
  {get mdi child menu}
  FChildMenu := nil;
  if THookForm(FFormHook.Form).FormStyle = fsMDIForm then
    with THookForm(FFormHook.Form) do
      if (ActiveMDIChild <> nil) and (ActiveMDIChild.Menu <> nil) and
         (ActiveMDIChild.Menu.Items.Count > 0) and
         (ActiveMDIChild.Handle <> FFormHook.FChangeVisibleChildHandle) then
        FChildMenu := ActiveMDIChild.Menu;

  {initialize array of items}
  FItemCount := GetItemCount(FMenu, FChildMenu);

  {calculation sizes}
  Buffer := TBitMap.Create;
  try
    Buffer.Canvas.Font.Assign(Screen.MenuFont);
    {$IFDEF VER330_UP}
    if CheckPerMonitorV2SupportForWindow(FFormHook.Form.Handle) then
      Buffer.Canvas.Font.Height := MulDiv(Buffer.Canvas.Font.Height,
        FFormHook.Form.CurrentPPI, Screen.PixelsPerInch);
    {$ENDIF}
    LIconDraw := FShowMDIButtons and CanTrackMDISystemMenu;
    if LIconDraw then
      LHeight := {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU)
    else
      LHeight := 0;
    for I := 0 to FItemCount  - 1 do
    begin
      LWidth := GetMenuItemWidth(FItems[I].MenuItem, Buffer.Canvas);
      LHeight := LHeight + LWidth;
      if (LHeight > AWidth) and (LHeight <> 0) then
      begin
        LHeight := LWidth;
        Result := Result + {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU);
      end;
    end;
  finally
    Buffer.Free;
  end;
  if (LButtonWidth <> 0) and (LHeight + LButtonWidth > AWidth) then
    Result := Result + {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU);
end;

function TscFormStyleHook.TscMainMenuBarStyleHook.GetMenuItemWidth(AMenuItem: TMenuItem; ACanvas: TCanvas): Integer;
var
  R: TRect;
begin
  if (AMenuItem.GetParentMenu = nil) or not AMenuItem.Visible then
    Exit(0);

  R := Rect(0, 0, 0, 0);
  DrawText(ACanvas.Handle, PChar(AMenuItem.Caption), Length(AMenuItem.Caption), R, DT_CALCRECT);
  Result := R.Width + 10;
  if (AMenuItem.GetParentMenu.Images <> nil) and (AMenuItem.ImageIndex >= 0) and
     (AMenuItem.ImageIndex < AMenuItem.GetParentMenu.Images.Count) then
    Result := Result + MainMenu.Images.Width + 6;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.DrawItem(AItem: TMenuBarItem; ACanvas: TCanvas);
var
  Details: TThemedElementDetails;
  SaveIndex: Integer;
  LWidth, LHeight: Integer;
  R: TRect;
  LTextColor: TColor;
  ItemMainMenu: TMenu;
  LStyle: TCustomStyleServices;
  LItemState: TOwnerDrawState;
begin
  if AItem.MenuItem.GetParentMenu = nil then
    Exit;

  LStyle := StyleServices;
  ItemMainMenu := AItem.MenuItem.GetParentMenu;
  {check item state}
  if FActiveItem = AItem.Index then
  begin
    if FMenuPush then
      AItem.State := tmMenuBarItemPushed
    else if not FSysMenuActive and not FMDIChildSysMenuActive then
      AItem.State := tmMenuBarItemHot
    else
      AItem.State := tmMenuBarItemNormal;
  end
  else if AItem.MenuItem.Enabled then
    AItem.State := tmMenuBarItemNormal
  else
    AItem.State := tmMenuBarItemDisabled;

    if Assigned(AItem.MenuItem.OnDrawItem) or Assigned(AItem.MenuItem.OnAdvancedDrawItem) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if Assigned(AItem.MenuItem.OnDrawItem) then
        AItem.MenuItem.OnDrawItem(AItem.MenuItem, ACanvas, AItem.ItemRect,
         (AItem.State = tmMenuBarItemPushed) or (AItem.State = tmMenuBarItemHot));

      if Assigned(AItem.MenuItem.OnAdvancedDrawItem) then
      begin
        if (FMenuPush or FMenuActive) and FEnterWithKeyboard then
         LItemState := []
        else
         LItemState := [odNoAccel];
        case AItem.State of
          tmMenuBarItemDisabled:
          LItemState := LItemState + [odDisabled];
          tmMenuBarItemPushed,
          tmMenuBarItemHot:
          LItemState := LItemState + [odSelected];
        end;
        AItem.MenuItem.OnAdvancedDrawItem(AItem.MenuItem, ACanvas, AItem.ItemRect,
          LItemState);
      end;
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    Exit;
  end;

  Details := LStyle.GetElementDetails(AItem.State);
  {draw item body}
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    LStyle.DrawElement(ACanvas.Handle, Details, AItem.ItemRect);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
  R := AItem.ItemRect;
  if FFormHook.Control.BiDiMode <> bdRightToLeft then
    Inc(R.Left, 5)
  else
    Dec(R.Right, 5);
  {draw item image}
  if (ItemMainMenu.Images <> nil) and (AItem.MenuItem.ImageIndex >= 0) and
     (AItem.MenuItem.ImageIndex < MainMenu.Images.Count) then
  begin
    if FFormHook.Control.BiDiMode <> bdRightToLeft then
      LWidth := R.Left
    else
      LWidth := R.Right - ItemMainMenu.Images.Width;
    LHeight := R.Top + R.Height div  2 - ItemMainMenu.Images.Height div 2;
    ImageList_Draw(MainMenu.Images.Handle, AItem.MenuItem.ImageIndex,
      ACanvas.Handle, LWidth, LHeight, ILD_TRANSPARENT);
    if FFormHook.Control.BiDiMode <> bdRightToLeft then
      R.Left := R.Left + ItemMainMenu.Images.Width + 3
    else
      R.Right := R.Right - ItemMainMenu.Images.Width - 3;
  end;
  {draw item text}
  if LStyle.GetElementColor(Details, ecTextColor, LTextColor) then
    ACanvas.Font.Color := TColor(LTextColor);
  if (FMenuPush or FMenuActive) and FEnterWithKeyboard then
    DrawText(ACanvas.Handle, PChar(AItem.MenuItem.Caption), Length(AItem.MenuItem.Caption),
      R, FFormHook.Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE))
  else
    DrawText(ACanvas.Handle, PChar(AItem.MenuItem.Caption), Length(AItem.MenuItem.Caption),
      R, FFormHook.Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_HIDEPREFIX or DT_SINGLELINE));
end;


type
  TMenuItemClass = class(TMenuItem);

procedure TscFormStyleHook.TscMainMenuBarStyleHook.Paint(Canvas: TCanvas);

function GetItemCount(AMenu, AMergedMenu: TMainMenu): Integer;

procedure Insert(APos: Integer; var ACount: Integer; AItem: TMenuItem);
var
  I: Integer;
begin
  Inc(ACount);
  if APos = ACount - 1 then
    FItems[APos].MenuItem := AItem
  else
  begin
    for I := ACount - 1 downto APos + 1 do
      FItems[I].MenuItem := FItems[I - 1].MenuItem;
    FItems[APos].MenuItem := AItem;
  end;
end;

function CanAddItem(AItem: TMenuItem): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to AMergedMenu.Items.Count - 1 do
    if AItem.GroupIndex = AMergedMenu.Items[I].GroupIndex then
    begin
      Result := False;
      Break;
    end;
end;

var
  I, J, Count, Index: Integer;
begin
  if AMenu = nil then
    Exit(0);

  if AMergedMenu <> nil then
  begin
    Count := AMenu.Items.Count + AMergedMenu.Items.Count;
    SetLength(FItems, Count);
    Result := AMergedMenu.Items.Count;
    {add items from child menu}
    for I := 0 to Result - 1 do
      FItems[I].MenuItem := AMergedMenu.Items[I];
    {add items from menu}
    for I := AMenu.Items.Count - 1 downto 0 do
      if CanAddItem(AMenu.Items[I]) then
      begin
        Index := -1;
        for J := 0 to Result - 1 do
          if AMenu.Items[I].GroupIndex <= FItems[J].MenuItem.GroupIndex then
          begin
            Index := J;
            Break;
          end;
        if Index = -1 then Index := Result;
        Insert(Index, Result, AMenu.Items[I]);
      end;
  end
  else
  begin
    {add items from menu}
    Result := AMenu.Items.Count;
    SetLength(FItems, Result);
    for I := 0 to Result - 1 do
      FItems[I].MenuItem := AMenu.Items[I];
  end;
end;

function IsRightJustify(AMenu: HMenu; AIndex: Integer): Boolean;
var
  Info: TMenuItemInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TMenuItemInfo);
  Info.fMask := MIIM_TYPE;
  GetMenuItemInfo(AMenu, FItems[AIndex].MenuItem.Command, False, Info);
  Result := Info.fType and MFT_RIGHTJUSTIFY = MFT_RIGHTJUSTIFY;
end;

var
  Details: TThemedElementDetails;
  Buffer, Buf: TBitMap;
  FMenu, FMergedMenu: TMainMenu;
  I, X, Y, W, BW: Integer;
  R, R2: TRect;
  SaveIndex: Integer;
  FIconDraw, FRightAlign: Boolean;
  LStyle: TCustomStyleServices;
  FMerged: TMenuItem;
  RX: Integer;
  FPrevIndex: Integer;
begin
  if (FBoundsRect.Width = 0) or (FBoundsRect.Height = 0) then
    Exit;

  LStyle := StyleServices;
  if not LStyle.Available then
    Exit;

  {get main menu}
  FMenu := MainMenu;
  if FMenu = nil then
    Exit;

  {get merged menu}
  FMergedMenu := nil;

  {$IFNDEF VER230}
  FMerged := TMenuItemClass(THookForm(FFormHook.Form).Menu.Items).Merged;
  {$ELSE}
  FMerged := nil;
  {$ENDIF}


  if (FMerged <> nil) and (FMerged.Count > 0) and (FMerged.GetParentMenu is TMainMenu) then
    FMergedMenu := TMainMenu(FMerged.GetParentMenu);

  Buffer := TBitMap.Create;
  try
    Buffer.SetSize(FBoundsRect.Width, FBoundsRect.Height);
    {draw menu bar}
    SaveIndex := SaveDC(Buffer.Canvas.Handle);
    try
      Details := LStyle.GetElementDetails(tmMenuBarBackgroundActive);
      LStyle.DrawElement(Buffer.Canvas.Handle, Details,
      Rect(0, 0, Buffer.Width, Buffer.Height));
    finally
      RestoreDC(Buffer.Canvas.Handle, SaveIndex);
    end;
    Buffer.Canvas.Font.Assign(Screen.MenuFont);
    {$IFDEF VER330_UP}
    if CheckPerMonitorV2SupportForWindow(FFormHook.Form.Handle) then
      Buffer.Canvas.Font.Height := MulDiv(Buffer.Canvas.Font.Height,
        FFormHook.Form.CurrentPPI, Screen.PixelsPerInch);
    {$ENDIF}
    Buffer.Canvas.Brush.Style := bsClear;
    {draw mdi child icon}
    FIconDraw := FShowMDIButtons and CanTrackMDISystemMenu;
    if FIconDraw then
      DrawIconEx(Buffer.Canvas.Handle, 2, 2, GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);

    {initialize array of items}
    FItemCount := GetItemCount(FMenu, FMergedMenu);

    {draw items}
    FRightAlign := FFormHook.Control.BiDiMode = bdRightToLeft;
    BW := {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU);
    Y := 0;
    if FShowMDIButtons then
      RX := FBoundsRect.Width - BW * 3
    else
      RX := FBoundsRect.Width;
    if not FRightAlign then
    begin
      if FIconDraw then
        X := BW
      else
        X := 0;
    end
    else
      X := RX;

    for I := 0 to FItemCount - 1 do
    begin
      FItems[I].Index := I;
      W := GetMenuItemWidth(FItems[I].MenuItem, Buffer.Canvas);
      if W = 0 then
      begin
        FItems[I].ItemRect := Rect(0, 0, 0, 0);
        Continue;
      end;
      if not FRightAlign then
      begin
        FItems[I].ItemRect.Left := X;
        FItems[I].ItemRect.Right := FItems[I].ItemRect.Left + W;
        if (FItems[I].ItemRect.Right > FBoundsRect.Width) and (X <> 0) then
        begin
          Y := Y + {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU);
          FItems[I].ItemRect.Left := 0;
          FItems[I].ItemRect.Right := W;
        end;
        X := FItems[I].ItemRect.Right;
      end
      else
      begin
        FItems[I].ItemRect.Left := X - W;
        FItems[I].ItemRect.Right := FItems[I].ItemRect.Left + W;
        if (FItems[I].ItemRect.Left < 0) and (X <> 0) then
        begin
          Y := Y + {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU);
          if FShowMDIButtons then
            FItems[I].ItemRect.Right := FBoundsRect.Width - BW * 3
          else
            FItems[I].ItemRect.Right := FBoundsRect.Width;
          FItems[I].ItemRect.Left := FItems[I].ItemRect.Right - W;
        end;
        X := FItems[I].ItemRect.Left;
      end;
      FItems[I].ItemRect.Top := Y;
      FItems[I].ItemRect.Bottom := FItems[I].ItemRect.Top + {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU);
    end;

    if not FRightAlign then
    begin
      FPrevIndex := -1;
      for I := FItemCount - 1 downto 0 do
        if IsRightJustify(FMenu.Handle, I) then
        begin
          FItems[I].Index := I;
          W := FItems[I].ItemRect.Width;
          if (W > 0) and ((FPrevIndex = -1) or
             ((FPrevIndex >= 0) and (FItems[I].ItemRect.Top = FItems[FPrevIndex].ItemRect.Top))) then
          begin
            FItems[I].ItemRect.Left := RX - W;
            FItems[I].ItemRect.Right := RX;
            RX := FItems[I].ItemRect.Left;
            FPrevIndex := I;
          end;
        end;
    end;

    for I := 0 to FItemCount - 1 do
      if FItems[I].ItemRect.Width > 0 then
        DrawItem(FItems[I], Buffer.Canvas);

    {draw mdi buttons}
    X := Buffer.Width;
    Y := Buffer.Height - BW;
    if FShowMDIButtons then
    begin
      for I := 0 to 2 do
      begin
        FMDIButtons[I].Index := I;
        case I of
          0:
            begin
              if (I = FHotMDIButton) and (I = FPressedMDIButton) then
                FMDIButtons[I].State := twMDICloseButtonPushed
              else if (I = FHotMDIButton) then
                FMDIButtons[I].State := twMDICloseButtonHot
              else
                FMDIButtons[I].State := twMDICloseButtonNormal;
            end;
          1:
            begin
              if (I = FHotMDIButton) and (I = FPressedMDIButton) then
                FMDIButtons[I].State := twMDIRestoreButtonPushed
              else if (I = FHotMDIButton) then
                FMDIButtons[I].State := twMDIRestoreButtonHot
              else
                FMDIButtons[I].State := twMDIRestoreButtonNormal;
            end;

         2:
            begin
              if (I = FHotMDIButton) and (I = FPressedMDIButton) then
                FMDIButtons[I].State := twMDIMinButtonPushed
              else if (I = FHotMDIButton) then
                FMDIButtons[I].State := twMDIMinButtonHot
              else
                FMDIButtons[I].State := twMDIMinButtonNormal;
            end;
        end;
        FMDIButtons[I].ItemRect := Rect(X - BW, Y, X, Y + BW);
        Details := LStyle.GetElementDetails(FMDIButtons[I].State);
        // mdib
        if (FFormHook.StyledForm <> nil) and (FFormHook.StyledForm.BorderScaleFactor > 1) and
           (FMDIButtons[I].ItemRect.Height > 22)
        then
        begin
          R := FMDIButtons[I].ItemRect;
          Buf := TBitmap.Create;
          SaveIndex := SaveDC(Buffer.Canvas.Handle);
          try
            Buf.Width := 20;
            Buf.Height := 20;
            Buf.PixelFormat := pf32bit;
            Bitmap_ClearAlpha(Buf, 0);
            R2 := Rect(1, 1, Buf.Width - 1, Buf.Height - 1);
            Buf.AlphaFormat := afPremultiplied;
            LStyle.DrawElement(Buf.Canvas.Handle, Details, R2);
            Bitmap_DrawScaleAlpha_XY(Buf, Buffer.Canvas,
              R.Left - 1, R.Top - 1, 255, FFormHook.StyledForm.BorderScaleFactor);
          finally
            RestoreDC(Buffer.Canvas.Handle, SaveIndex);
            Buf.Free;
          end;
      end
      else
        LStyle.DrawElement(Buffer.Canvas.Handle, Details,
          FMDIButtons[I].ItemRect);
        //
        X := X - BW;
      end;
    end;
    {draw buffer}
    Canvas.Draw(FBoundsRect.Left, FBoundsRect.Top, Buffer);
  finally
    Buffer.Free;
  end;
end;

class function TscFormStyleHook.TscMainMenuBarStyleHook.PopupMenuHook(Code: Integer; WParam: WPARAM; var Msg: TMsg): LRESULT;
var
  FItem: Integer;
  FFindItemKind: TFindItemKind;
  P: TPoint;
  FOldActiveItem: Integer;
  I: Integer;
  CanFindItem: Boolean;
begin
  if (FMenuBarHook = nil) or
     ((FMenuBarHook <> nil) and (FMenuBarHook.MainMenu = nil)) then
    Exit(0);
  Result := CallNextHookEx(FMenuBarHook.FMenuHook, Code, WParam, IntPtr(@Msg));
  if Result <> 0 then
    Exit;

  if FMenuBarHook.FSelectFirstItem then
  begin
    FMenuBarHook.FSelectFirstItem := False;
    if GetKeyState(VK_LBUTTON) < 0 then
      PostMessage(Msg.Hwnd, $01EE, 0, 0);
  end;

  if Code = MSGF_MENU then
    case Msg.Message of
       WM_MOUSEMOVE:
        if (WindowFromPoint(Mouse.CursorPos) = FMenuBarHook.FFormHook.Handle) and
           not FMenuBarHook.FMustActivateMenuItem then
        begin
          P := Mouse.CursorPos;
          P.X := P.X - FMenuBarHook.FFormHook.Control.Left -
            FMenuBarHook.FBoundsRect.Left;
          P.Y := P.Y - FMenuBarHook.FFormHook.Control.Top -
            FMenuBarHook.FBoundsRect.Top;
          FOldActiveItem := FMenuBarHook.FActiveItem;
          FMenuBarHook.MouseMove(P.X, P.Y);
          if (FOldActiveItem <> FMenuBarHook.FActiveItem) and
             (FMenuBarHook.FActiveItem <> -1) then
          begin
            P := Mouse.CursorPos;
            FMenuBarHook.FMustActivateMenuItem := True;
            PostMessage(FMenuBarHook.FFormHook.Handle, WM_NCLBUTTONDOWN, MK_LBUTTON,
              Integer(PointToSmallPoint(P))); // 64-bit safe Integer cast
          end;
        end;
      WM_SYSKEYDOWN:
        if Msg.wParam = VK_MENU then
        begin
           FMenuBarHook.FMustActivateMenuItem := False;
           FMenuBarHook.MenuExit;
         end;
      WM_MENUSELECT:
        begin
          FFindItemKind := fkCommand;
          if (Msg.WParam shr 16) and MF_POPUP <> 0 then
            FFindItemKind := fkHandle;
          if FFindItemKind = fkHandle then
            FItem := GetSubMenu(HMENU(Msg.LParam), LoWord(Msg.WParam))
          else
            FItem := LoWord(Msg.WParam);
          FMenuBarHook.FLastItemHasSubMenu :=
            GetSubMenu(HMENU(Msg.LParam), LoWord(Msg.WParam)) > 0;
          FCurrentMenuItem := FMenuBarHook.FindItem(FItem, FFindItemKind);
        end;
      WM_KEYDOWN:
        begin
          if  FMenuBarHook.FFormHook.Control.BidiMode = bdRightToLeft then
          begin
            if Msg.WParam = VK_RIGHT then Msg.WParam := VK_LEFT else
              if Msg.WParam = VK_LEFT then Msg.WParam := VK_RIGHT;
          end;

          CanFindItem := False;

          if Msg.WParam = VK_RIGHT then
          begin
            if FMenuBarHook.FLastItemHasSubMenu and (FCurrentMenuItem = nil) then
              CanFindItem := False
            else
              CanFindItem := FMenuBarHook.CanFindNextItem(FCurrentMenuItem);
          end
          else if Msg.WParam = VK_LEFT then
          begin
            CanFindItem := FMenuBarHook.CanFindPriorItem(FCurrentMenuItem);
          end;

          case Msg.WParam of
            VK_RIGHT:
             if CanFindItem then
             begin
               FMenuBarHook.FEnterWithKeyboard := True;
               if FMenuBarHook.FSystemMenuTracking and
                  FMenuBarHook.CanTrackMDISystemMenu then
               begin
                 P := Mouse.CursorPos;
                 FMenuBarHook.FMustActivateMDIChildSysMenu := True;
                 EndMenu;
                 PostMessage(FMenuBarHook.FFormHook.Handle, WM_NCLBUTTONDOWN, MK_LBUTTON,
                   Integer(PointToSmallPoint(P))); // 64-bit safe Integer cast
                 Exit;
               end
               else
                 if not FMenuBarHook.FSystemMenuTracking then
                   I := FMenuBarHook.FindNextMenuItem(False)
                 else
                   I := FMenuBarHook.FindFirstMenuItem(False);

               if I <> -1 then
               begin
                 FMenuBarHook.FActiveItem := I;
                 P := Mouse.CursorPos;
                 FMenuBarHook.FMustActivateMenuItem := True;
                 EndMenu;
                 PostMessage(FMenuBarHook.FFormHook.Handle, WM_NCLBUTTONDOWN, MK_LBUTTON,
                   Integer(PointToSmallPoint(P)));
               end
               else if not FMenuBarHook.FSystemMenuTracking then
               begin
                 P := Mouse.CursorPos;
                 FMenuBarHook.FMustActivateSysMenu := True;
                 EndMenu;
                 PostMessage(FMenuBarHook.FFormHook.Handle, WM_NCLBUTTONDOWN, MK_LBUTTON,
                   Integer(PointToSmallPoint(P))); // 64-bit safe Integer cast
               end;
             end;
           VK_LEFT:
           if CanFindItem then
             begin
               FMenuBarHook.FEnterWithKeyboard := True;
               if FMenuBarHook.FMDIChildSystemMenuTracking
               then
                 I := -1
               else if not FMenuBarHook.FSystemMenuTracking then
                 I := FMenuBarHook.FindPriorMenuItem(False)
               else
                 I := FMenuBarHook.FindFirstRightMenuItem(False);

               if I <> -1 then
               begin
                 FMenuBarHook.FActiveItem := I;
                 P := Mouse.CursorPos;
                 FMenuBarHook.FMustActivateMenuItem := True;
                 EndMenu;
                 PostMessage(FMenuBarHook.FFormHook.Handle, WM_NCLBUTTONDOWN, MK_LBUTTON,
                   Integer(PointToSmallPoint(P))); // 64-bit safe Integer cast
               end
               else if FMenuBarHook.CanTrackMDISystemMenu and
                    not FMenuBarHook.FMDIChildSystemMenuTracking then
               begin
                 P := Mouse.CursorPos;
                 FMenuBarHook.FMustActivateMDIChildSysMenu := True;
                 EndMenu;
                 PostMessage(FMenuBarHook.FFormHook.Handle, WM_NCLBUTTONDOWN, MK_LBUTTON,
                   Integer(PointToSmallPoint(P))); // 64-bit safe Integer cast
               end
               else if not FMenuBarHook.FSystemMenuTracking then
               begin
                 P := Mouse.CursorPos;
                 FMenuBarHook.FMustActivateSysMenu := True;
                 EndMenu;
                 PostMessage(FMenuBarHook.FFormHook.Handle, WM_NCLBUTTONDOWN, MK_LBUTTON,
                   Integer(PointToSmallPoint(P))); // 64-bit safe Integer cast
               end;
             end;
         end;
       end;
    end;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.SetBoundsRect(const ABoundsRect: TRect);
begin
  FBoundsRect := ABoundsRect;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.MouseUp(X, Y: Integer);
begin
  FActiveItem := ItemFromPoint(X, Y);
  if FActiveItem <> -1 then
  begin
    Invalidate;
    if FItems[FActiveItem].MenuItem.Count = 0 then
      MainMenu.DispatchCommand(FItems[FActiveItem].MenuItem.Command);
  end;

  if FShowMDIButtons then
  begin
    FHotMDIButton := MDIButtonFromPoint(X, Y);
    if (FHotMDIButton <> -1) and (FPressedMDIButton = FHotMDIButton) then
    begin
      FPressedMDIButton := -1;
      Invalidate;
      case FMDIButtons[FHotMDIButton].Index of
        0: MDIChildClose;
        1: MDIChildRestore;
        2: MDIChildMinimize;
      end;
    end
    else
      FPressedMDIButton := -1;
  end;
end;


procedure TscFormStyleHook.TscMainMenuBarStyleHook.MouseDown(X, Y: Integer);
begin

  FActiveItem := ItemFromPoint(X, Y);
  if FActiveItem <> -1 then
    MenuEnter(True)
  else
  begin
    if FShowMDIButtons and CanTrackMDISystemMenu and Rect(0, 0,
       {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU), {$IFDEF VER330_UP}FFormHook.{$ENDIF}GetSystemMetrics(SM_CYMENU)).Contains(Point(X, Y)) then
      TrackMDIChildSystemMenu;
  end;

  if FShowMDIButtons then
  begin
    FHotMDIButton := MDIButtonFromPoint(X, Y);
    FPressedMDIButton := FHotMDIButton;
    if FPressedMDIButton <> -1 then
      Invalidate;
  end;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.MouseMove(X, Y: Integer);
begin
  if FMustActivateMenuItem  or ((FOldCursorPos = Mouse.CursorPos) and (FMenuActive or FMenuPush)) then
    Exit;

  FOldCursorPos := Mouse.CursorPos;

  FMouseInMainMenu := not ((X < 0) or (Y < 0));
  if FMenuPush then
  begin
    if ItemFromPoint(X, Y) <> -1 then
      FActiveItem := ItemFromPoint(X, Y);
  end
  else
    FActiveItem := ItemFromPoint(X, Y);

  if FActiveItem <> FOldActiveItem then
  begin
    Invalidate;
    FOldActiveItem := FActiveItem;
    if FMenuPush and  (FMenuHook = 0) and (FItems[FActiveItem].MenuItem.Count <> 0) then
      TrackMenuFromItem;
  end;

  if FShowMDIButtons then
  begin
    FHotMDIButton := MDIButtonFromPoint(X, Y);
    if FHotMDIButton <> FOldMDIHotButton then
    begin
      Invalidate;
      FOldMDIHotButton := FHotMDIButton;
    end;
    if FHotMDIButton = -1 then FPressedMDIButton := -1;
  end;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.TrackMDIChildSystemMenu;
var
  X, Y: Integer;
  Child: TCustomForm;
  P: TPoint;
  R: TRect;
  CR: TRect;
begin
  FMDIChildSysMenuActive := False;
  FSysMenuActive := False;
  if THookForm(FFormHook.Form).FormStyle <> fsMDIForm then
    Exit;

  Child := THookForm(FFormHook.Form).ActiveMDIChild;
  if Child = nil then
    Exit;

  FMDIChildSystemMenuTracking := True;
  if Child.WindowState = wsMaximized then
  begin
    X := FFormHook.FLeft + FBoundsRect.Left;
    Y := FFormHook.FTop + FBoundsRect.Bottom;
  end
  else
  begin
    P := FFormHook.Control.ClientToScreen(Point(0, 0));
    R := FFormHook.GetMDIWorkArea;
    if IsIconic(Child.Handle) then
    begin
      GetWindowRect(Child.Handle, CR);
      X := CR.Left;
      Y := CR.Bottom;
    end
    else
    begin
      X := P.X + R.Left + Child.Left + FBoundsRect.Left;
      Y := P.Y + R.Top + Child.Top + FBoundsRect.Top;
    end;
  end;
  HookMenus;
  SendMessage(Child.Handle, $313, 0, MakeLong(X, Y));
  UnHookMenus;
  FMDIChildSystemMenuTracking := False;
  Invalidate;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.TrackSystemMenu;
var
  X, Y: Integer;
  LeftPoint, RightPoint: TPoint;
begin
  FMDIChildSysMenuActive := False;
  FSysMenuActive := False;
  FSystemMenuTracking := True;
  X := FFormHook.FLeft + FBoundsRect.Left;
  Y := FFormHook.FTop + FBoundsRect.Top;
  LeftPoint := Point(X, Y);
  RightPoint := Point(X + 50, Y);
  if Screen.MonitorFromPoint(LeftPoint) <> Screen.MonitorFromPoint(RightPoint)
  then
    X := Screen.MonitorFromPoint(RightPoint).WorkareaRect.Left;
  HookMenus;
  SendMessage(FFormHook.Handle, $313, 0, MakeLong(X, Y));
  UnHookMenus;
  FSystemMenuTracking := False;
  Invalidate;
end;

procedure TscFormStyleHook.TscMainMenuBarStyleHook.TrackMenuFromItem;
var
  P: TPoint;
  Cmd: Bool;
  FItem: TMenuItem;
begin
  FMDIChildSysMenuActive := False;
  FSysMenuActive := False;
  FMenuPush := True;
  Invalidate;
  if FItems[FActiveItem].MenuItem.Count = 0 then
    Exit;
  P := GetTrackMenuPos(FItems[FActiveItem]);

  HookMenus;

  if (FFormHook.Form <> nil) and (THookForm(FFormHook.Form).FormStyle = fsMDIForm)
  then
    SC_StopSetMenuRedraw := True;

  if FFormHook.Control.BiDiMode <> bdRightToLeft then
    Cmd := TrackPopupMenu(FItems[FActiveItem].MenuItem.Handle,
      TPM_LEFTBUTTON  or TPM_RIGHTBUTTON or TPM_RETURNCMD or TPM_NOANIMATION,
        P.X, P.Y, 0, FFormHook.Handle, nil)
  else
    Cmd := TrackPopupMenu(FItems[FActiveItem].MenuItem.Handle,
      TPM_LEFTBUTTON  or TPM_RIGHTBUTTON or TPM_RETURNCMD or TPM_NOANIMATION or TPM_RIGHTALIGN,
        P.X, P.Y, 0, FFormHook.Handle, nil);
  UnHookMenus;

  SC_StopSetMenuRedraw := False;

  FMenuPush := False;

  if Cmd then
  begin
    FItem := FindItem(IntPtr(Cmd), fkCommand);
    if FItem <> nil then
      FItem.GetParentMenu.DispatchCommand(FItem.Command)
    else
      PostMessage(FFormHook.Handle, WM_COMMAND, WParam(Cmd), 0);
    MenuExit;
  end
  else if not FMustActivateMenuItem then
  begin
    FMenuActive := True;
    FInMenuLoop := False;
    ProcessMenuLoop(False);
  end;

  Invalidate;
end;

{ TscFormStyleHook }

constructor TscFormStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  FocusUpdate := False;
  {$IFNDEF VER230}
  if seClient in Form.StyleElements then
  {$ENDIF}
    OverrideEraseBkgnd := True;

  if IsStyleBorder then
    OverridePaintNC := True;
  FStyledCaptionHeight := 0;
  FMainMenuBarHook := nil;
  FMDIHorzScrollBar := nil;
  FMDIVertScrollBar := nil;
  FMDIScrollSizeBox := nil;
  FMDIClientInstance := nil;
  FMDIPrevClientProc := nil;
  FChangeVisibleChildHandle := 0;
  FStopCheckChildMove := False;
  FOldHorzSrollBarPosition := 0;
  FOldVertSrollBarPosition := 0;
  FXOffset := 0;
  FYOffset := 0;

  FMDIStopHorzScrollBar := False;
  FMDIStopVertScrollBar := False;

  MouseInNCArea := True;
  FFormActive := False;
  FChangeSizeCalled := False;
  FRegion := 0;
  FLeft := Control.Left;
  FTop := Control.Top;
  FWidth := Control.Width;
  FHeight := Control.Height;
  FNeedsUpdate := True;
  FIcon := nil;
  FIconHandle := 0;
  FHotButton := 0;
  FPressedButton := 0;
  FCaptionEmulation := False;
  FRestoring := False;
  // nc objects
  FActiveNCObject := nil;
  FOldNCObject := nil;
  FCapturedNCObject := nil;
end;

destructor TscFormStyleHook.Destroy;
begin
  if FIcon <> nil then
    FreeAndNil(FIcon);

  if FMDIClientInstance <> nil then
  begin
    SetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC, IntPtr(FMDIPrevClientProc));
    FreeObjectInstance(FMDIClientInstance);
  end;

  if FMainMenuBarHook <> nil then
    FreeAndNil(FMainMenuBarHook);
  if FMDIHorzScrollBar <> nil then
    FreeAndNil(FMDIHorzScrollBar);
  if FMDIVertScrollBar <> nil then
    FreeAndNil(FMDIVertScrollBar);
  if FMDIScrollSizeBox <> nil then
    FreeAndNil(FMDIScrollSizeBox);
  inherited;
end;

function TscFormStyleHook.IsStyleBorder: Boolean;
begin
  {$IFNDEF VER230}
  Result := (TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements);
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

procedure TscFormStyleHook.Invalidate;
begin
  // Prevent ancestor's Invalidate from executing
end;

procedure TscFormStyleHook.MDIHorzScroll(Offset: Integer);
var
  I: Integer;
  F: TForm;
  R: TRect;
  P: TPoint;
begin
  FStopCheckChildMove := True;
  try
    for I := 0 to THookForm(Form).MDIChildCount -1 do
      if THookForm(Form).MDIChildren[I].Visible then
      begin
        if THookForm(Form).MDIChildren[I].WindowState <> wsMinimized then
          THookForm(Form).MDIChildren[I].Left := THookForm(Form).MDIChildren[I].Left + Offset
        else
        begin
          F := THookForm(Form).MDIChildren[I];
          GetWindowRect(F.Handle, R);
          P := Point(0, 0);
          WinApi.Windows.ClientToScreen(THookForm(Form).ClientHandle, P);
          OffsetRect(R, -P.X, -P.Y);
          SetWindowPos(F.Handle, HWND_TOPMOST, R.Left + Offset, R.Top, 0, 0,
            SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
        end;
      end;
  finally
    FStopCheckChildMove := False;
  end;
  GetMDIScrollInfo(False);
end;

procedure TscFormStyleHook.MDIVertScroll(Offset: Integer);
var
  I: Integer;
  F: TForm;
  R: TRect;
  P: TPoint;
begin
  FStopCheckChildMove := True;
  try
    for I := 0 to THookForm(Form).MDIChildCount -1 do
      if THookForm(Form).MDIChildren[I].Visible then
      begin
        if THookForm(Form).MDIChildren[I].WindowState <> wsMinimized then
          THookForm(Form).MDIChildren[I].Top := THookForm(Form).MDIChildren[I].Top + Offset
        else
        begin
          F := THookForm(Form).MDIChildren[I];
          GetWindowRect(F.Handle, R);
          P := Point(0, 0);
          WinApi.Windows.ClientToScreen(THookForm(Form).ClientHandle, P);
          OffsetRect(R, -P.X, -P.Y);
          SetWindowPos(F.Handle, HWND_TOPMOST, R.Left, R.Top + Offset, 0, 0,
            SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
        end;
      end;
  finally
    FStopCheckChildMove := False;
  end;
  GetMDIScrollInfo(False);
end;

procedure TscFormStyleHook.OnMDIHScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  Offset: Integer;
begin
  if (FMDIStopHorzScrollBar) or (ScrollCode <>  TScrollCode.scEndScroll) then
    Exit;

  Offset := TScrollBar(FMDIHorzScrollBar).Position - FOldHorzSrollBarPosition;
  if Offset <> 0 then
    MDIHorzScroll(-Offset);
  FOldHorzSrollBarPosition := TScrollBar(FMDIHorzScrollBar).Position;
end;

procedure TscFormStyleHook.OnMDIVScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  Offset: Integer;
begin
  if (FMDIStopVertScrollBar) or (ScrollCode <> scEndScroll) then
    Exit;

  Offset := TScrollBar(FMDIVertScrollBar).Position - FOldVertSrollBarPosition;
  if Offset <> 0 then
    MDIVertScroll(-Offset);
  FOldVertSrollBarPosition := TScrollBar(FMDIVertScrollBar).Position;
end;

function TscFormStyleHook.MDIChildMaximized: Boolean;
begin
  Result := (THookForm(Form).ActiveMDIChild <> nil) and
    (THookForm(Form).ActiveMDIChild.WindowState = wsMaximized);
end;

procedure TscFormStyleHook.GetMDIScrollInfo(SetRange: Boolean);
var
  I, MinX, MinY, MaxX, MaxY, HPage, VPage: Integer;
  R, MDIR, MDICLR: TRect;
  ReCalcInfo: Boolean;
  LHorzScrollVisible, LVertScrollVisible: Boolean;
  LMDIHorzScrollBar: TScrollBar;
  LMDIVertScrollBar: TScrollBar;
begin
  if FChildRestoring then Exit;

  LMDIHorzScrollBar := TScrollBar(FMDIHorzScrollBar);
  LMDIVertScrollBar := TScrollBar(FMDIVertScrollBar);
  if (LMDIHorzScrollBar = nil) or (LMDIVertScrollBar = nil) then
    Exit;

  if (not (LMDIVertScrollBar.HandleAllocated)) or
     (not LMDIHorzScrollBar.HandleAllocated) then
    Exit;

  if MDIChildMaximized then
  begin
    if IsWindowVisible(LMDIHorzScrollBar.Handle) then
      ShowWindow(LMDIHorzScrollBar.Handle, SW_HIDE);
    if IsWindowVisible(LMDIVertScrollBar.Handle) then
      ShowWindow(LMDIVertScrollBar.Handle, SW_HIDE);
    if IsWindowVisible(FMDIScrollSizeBox.Handle) then
      ShowWindow(FMDIScrollSizeBox.Handle, SW_HIDE);
    Exit;
  end;

  ReCalcInfo := False;
  R := GetMDIWorkArea;

  MinX := MaxInt;
  MinY := MaxInt;
  MaxX := -MaxInt;
  MaxY := -MaxInt;

  for I := 0 to THookForm(Form).MDIChildCount -1 do
   if (THookForm(Form).MDIChildren[I].Visible) and
      (THookForm(Form).MDIChildren[I].Handle <> FChangeVisibleChildHandle) then
     with Form do
     begin
       GetWindowRect(THookForm(Form).MDIChildren[I].Handle, MDIR);
       GetWindowRect(TForm(Control).ClientHandle, MDICLR);
       OffsetRect(MDIR, -MDICLR.Left, -MDICLR.Top);
       if MinX > MDIR.Left then
         MinX := MDIR.Left;
       if MinY > MDIR.Top then
         MinY := MDIR.Top;
       if MaxX < MDIR.Left + MDIR.Width then
         MaxX := MDIR.Left + MDIR.Width;
       if MaxY < MDIR.Top + MDIR.Height then
         MaxY := MDIR.Top + MDIR.Height;
     end;

  LHorzScrollVisible := (MinX < 0) or (MaxX > R.Width);
  LVertScrollVisible := (MinY < 0) or (MaxY > R.Height);

  if LVertScrollVisible and not LHorzScrollVisible then
    LHorzScrollVisible := (MinX < 0) or (MaxX > R.Width - LMDIVertScrollBar.Width);

  if LHorzScrollVisible and not LVertScrollVisible then
    LVertScrollVisible := (MinY < 0) or (MaxY > R.Height - LMDIHorzScrollBar.Height);

  if LHorzScrollVisible and not IsWindowVisible(LMDIHorzScrollBar.Handle) then
  begin
    SetWindowPos(LMDIHorzScrollBar.Handle, HWND_TOP,
      R.Left, R.Bottom - LMDIHorzScrollBar.Height,
      R.Width, LMDIHorzScrollBar.Height, SWP_SHOWWINDOW);
    ShowWindow(LMDIHorzScrollBar.Handle, SW_SHOW);
    ReCalcInfo := True;
  end
  else if not LHorzScrollVisible and IsWindowVisible(LMDIHorzScrollBar.Handle) then
  begin
    ShowWindow(LMDIHorzScrollBar.Handle, SW_HIDE);
    ReCalcInfo := True;
  end;

  if LVertScrollVisible and not IsWindowVisible(LMDIVertScrollBar.Handle) then
  begin
    if LHorzScrollVisible
    then
      SetWindowPos(LMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - LMDIVertScrollBar.Width,
        R.Top, LMDIVertScrollBar.Width, R.Height - LMDIHorzScrollBar.Height, SWP_SHOWWINDOW)
    else
      SetWindowPos(LMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - LMDIVertScrollBar.Width,
        R.Top, LMDIVertScrollBar.Width, R.Height, SWP_SHOWWINDOW);
    ShowWindow(LMDIVertScrollBar.Handle, SW_SHOW);
    ReCalcInfo := True;
  end
  else if not LVertScrollVisible and IsWindowVisible(LMDIVertScrollBar.Handle) then
  begin
    ShowWindow(LMDIVertScrollBar.Handle, SW_HIDE);
    ReCalcInfo := True;
  end;

  HPage := R.Width;
  VPage := R.Height;

  AdjustMDIScrollBars;

  if IsWindowVisible(LMDIHorzScrollBar.Handle) then
  begin
    if MinX > 0 then
      MinX := 0;
    if MaxX < R.Width then
      MaxX := R.Width;
    if SetRange then
    begin
      FMDIStopHorzScrollBar := True;
      if IsWindowVisible(LMDIVertScrollBar.Handle) then
        LMDIHorzScrollBar.PageSize := HPage - LMDIVertScrollBar.Width
      else
        LMDIHorzScrollBar.PageSize := HPage;
      LMDIHorzScrollBar.SetParams(-MinX, 0, MaxX - MinX - 1);
      FOldHorzSrollBarPosition := LMDIHorzScrollBar.Position;
      FMDIStopHorzScrollBar := False;
    end;
    LMDIHorzScrollBar.LargeChange := LMDIHorzScrollBar.PageSize;
  end;

  if IsWindowVisible(LMDIVertScrollBar.Handle) then
  begin
    if MinY > 0 then
      MinY := 0;
    if MaxY < R.Height then
      MaxY := R.Height;
    if SetRange then
    begin
      FMDIStopVertScrollBar := True;
      if IsWindowVisible(LMDIHorzScrollBar.Handle) then
        LMDIVertScrollBar.PageSize := VPage - LMDIHorzScrollBar.Height
      else
        LMDIVertScrollBar.PageSize := VPage;
      LMDIVertScrollBar.SetParams(-MinY, 0, MaxY - MinY - 1);
      FOldVertSrollBarPosition := LMDIVertScrollBar.Position;
      FMDIStopVertScrollBar := False;
    end;
    LMDIVertScrollBar.LargeChange := LMDIVertScrollBar.PageSize;
  end;

  if (not IsWindowVisible(LMDIHorzScrollBar.Handle)) and
     (not IsWindowVisible(LMDIVertScrollBar.Handle)) then ReCalcInfo := False;

  if IsWindowVisible(LMDIHorzScrollBar.Handle) and IsWindowVisible(LMDIVertScrollBar.Handle) and
     not IsWindowVisible(FMDIScrollSizeBox.Handle) then
  begin
    SetWindowPos(FMDIScrollSizeBox.Handle, HWND_TOP,
      R.Right - LMDIVertScrollBar.Width, R.Bottom - LMDIHorzScrollBar.Height,
      LMDIVertScrollBar.Width, LMDIHorzScrollBar.Height, SWP_SHOWWINDOW);
    ShowWindow(FMDIScrollSizeBox.Handle, SW_SHOW);
  end
  else if not IsWindowVisible(LMDIHorzScrollBar.Handle) or not IsWindowVisible(LMDIVertScrollBar.Handle) and
     IsWindowVisible(FMDIScrollSizeBox.Handle) then
    ShowWindow(FMDIScrollSizeBox.Handle, SW_HIDE);

  if ReCalcInfo then
    GetMDIScrollInfo(SetRange);
end;

procedure TscFormStyleHook.InitMDIScrollBars;
begin
  if FMDIHorzScrollBar = nil then
  begin
    FMDIHorzScrollBar := TScrollBar.CreateParented(Control.Handle);
    with TScrollBar(FMDIHorzScrollBar) do
    begin
      Kind := sbHorizontal;
      OnScroll := OnMDIHScroll;
      SetWindowPos(FMDIHorzScrollBar.Handle, HWND_TOP,
        0, 0, 0, GetSystemMetrics(SM_CYHSCROLL), SWP_NOREDRAW);
      ShowWindow(FMDIHorzScrollBar.Handle, SW_HIDE);
    end;
  end;

  if FMDIVertScrollBar = nil then
  begin
    FMDIVertScrollBar := TScrollBar.CreateParented(Control.Handle);
    with TScrollBar(FMDIVertScrollBar) do
    begin
      Kind := sbVertical;
      OnScroll := OnMDIVScroll;
      SetWindowPos(FMDIVertScrollBar.Handle, HWND_TOP,
        0, 0, GetSystemMetrics(SM_CXVSCROLL), 0, SWP_NOREDRAW);
      ShowWindow(FMDIVertScrollBar.Handle, SW_HIDE);
    end;
  end;

  if FMDIScrollSizeBox = nil
  then
    begin
      FMDIScrollSizeBox := TScrollBarStyleHook.TScrollWindow.CreateParented(Control.Handle);
      with TScrollBarStyleHook.TScrollWindow(FMDIScrollSizeBox) do
      begin
        SizeBox := True;
        SetWindowPos(FMDIScrollSizeBox.Handle, HWND_TOP,
          0, 0, GetSystemMetrics(SM_CXVSCROLL), GetSystemMetrics(SM_CYHSCROLL), SWP_NOREDRAW);
        ShowWindow(FMDIScrollSizeBox.Handle, SW_HIDE);
      end;
    end;
end;

procedure TscFormStyleHook.AdjustMDIScrollBars;
var
  R: TRect;
  W, H: Integer;
begin
  R := GetMDIWorkArea;
  W := GetSystemMetrics(SM_CXVSCROLL);
  H := GetSystemMetrics(SM_CYHSCROLL);

  if (FMDIHorzScrollBar <> nil) and IsWindowVisible(FMDIHorzScrollBar.Handle)
  then
    begin
      if (FMDIVertScrollBar <> nil) and IsWindowVisible(FMDIVertScrollBar.Handle) then
        SetWindowPos(FMDIHorzScrollBar.Handle, HWND_TOP, R.Left,
          R.Bottom - H, R.Width - W, H, SWP_SHOWWINDOW)
      else
        SetWindowPos(FMDIHorzScrollBar.Handle, HWND_TOP, R.Left,
          R.Bottom - H, R.Width, H, SWP_SHOWWINDOW);
    end;

  if (FMDIVertScrollBar <> nil) and IsWindowVisible(FMDIVertScrollBar.Handle) then
  begin
    if (FMDIHorzScrollBar <> nil) and IsWindowVisible(FMDIHorzScrollBar.Handle)
    then
      SetWindowPos(FMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - W,
        R.Top, W, R.Height - H, SWP_SHOWWINDOW)
    else
      SetWindowPos(FMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - W,
        R.Top, W, R.Height, SWP_SHOWWINDOW)
  end;

  if (FMDIScrollSizeBox <> nil) and IsWindowVisible(FMDIScrollSizeBox.Handle) and
     (FMDIVertScrollBar <> nil) and IsWindowVisible(FMDIVertScrollBar.Handle) and
     (FMDIHorzScrollBar <> nil) and IsWindowVisible(FMDIHorzScrollBar.Handle) then
    SetWindowPos(FMDIScrollSizeBox.Handle, HWND_TOP,
      R.Right - W, R.Bottom - H,
      W, H, SWP_SHOWWINDOW);
end;

function TscFormStyleHook.GetMDIWorkArea: TRect;
var
  P: TPoint;
begin
  Result := Control.ClientRect;
  if TForm(Control).ClientHandle <> 0 then
  begin
    GetWindowRect(TForm(Control).ClientHandle, Result);
    P := Control.ClientToScreen(Point(0, 0));
    OffsetRect(Result, -P.X, -P.Y);
  end;
end;

procedure TscFormStyleHook.MDIClientWndProc(var Message: TMessage);
var
  FCallOldProc: Boolean;
  R: TRect;
  Details: TThemedElementDetails;
begin
  FCallOldProc := True;
  case Message.Msg of
    WM_NCACTIVATE:
      begin
        if TForm(Control).ActiveMDIChild <> nil then
          SendMessage(TForm(Control).ActiveMDIChild.Handle,
            Message.Msg, Message.wParam, Message.LParam);
        FCallOldProc := False;
        Message.Result := 1;
      end;
    WM_NCCALCSIZE:
      FCallOldProc := False;
   WM_NCPAINT:
      FCallOldProc := False;
    WM_ERASEBKGND:
      if (StyleServices.Available) {$IFNDEF VER230}and (seClient in Form.StyleElements) {$ENDIF} then
      begin
        Details.Element := teWindow;
        Details.Part := 0;
        R := Rect(0, 0, TForm(Control).ClientWidth, TForm(Control).ClientHeight);
        if StyleServices.Available then
          StyleServices.DrawElement(Message.wParam, Details, R);
        FCallOldProc := False;
      end;
  end;

  if FCallOldProc then
    with Message do
      Result := CallWindowProc(FMDIPrevClientProc, THookForm(Form).ClientHandle,
        Msg, wParam, lParam);
end;

procedure TscFormStyleHook.PaintBackground(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  R: TRect;
begin
  if StyleServices.Available then
  begin
    R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
    Details.Element := teWindow;
    Details.Part := 0;
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
end;

function TscFormStyleHook.GetBorderSize: TRect;
var
  Size: TSize;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
  FW, FH, FCH: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if Form.BorderStyle = bsNone then Exit;

  if not StyleServices.Available then Exit;
  {caption height}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  FStyledCaptionHeight := Size.cy;
  if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
    Size.cy := Round(Size.cy * FStyledForm.BorderScaleFactor);
  Result.Top := Size.cy;
  {left border width}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twFrameLeftActive
  else
    Detail := twSmallFrameLeftActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Left := Size.cx;
  {right border width}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twFrameRightActive
  else
    Detail := twSmallFrameRightActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Right := Size.cx;
  {bottom border height}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twFrameBottomActive
  else
    Detail := twSmallFrameBottomActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Bottom := Size.cy;
  FYOffset := 0;
  FXOffset := 0;
  if (Form.WindowState = wsMaximized) and (Form.BorderStyle <> bsNone) then
  begin
    if (Form.BorderStyle = bsSingle) or (Form.BorderStyle = bsDialog) then
    begin
      FW := WinApi.Windows.GetSystemMetrics(SM_CXDLGFRAME);
      FH := WinApi.Windows.GetSystemMetrics(SM_CYDLGFRAME);
    end
    else
    begin
      FW := WinApi.Windows.GetSystemMetrics(SM_CXFRAME);
      FH := WinApi.Windows.GetSystemMetrics(SM_CYFRAME);
    end;
    FCH := WinApi.Windows.GetSystemMetrics(SM_CYCAPTION);
    {$IFDEF VER330_UP}
    if CheckPerMonitorV2SupportForWindow(Form.Handle) and
      (Screen.PixelsPerInch <> Form.CurrentPPI) then
    begin
      FW := Round(5 * Form.CurrentPPI / 96 + 0.01) + 3;
      FH := FW;
      FCH := MulDiv(FCH, Form.CurrentPPI, Screen.PixelsPerInch);
    end;
   {$ENDIF}
    if Result.Left < FW then
      Inc(Result.Left, FW - Result.Left);
    if Result.Right < FW then
      Inc(Result.Right, FW - Result.Right);
    if Result.Bottom < FH then
      Inc(Result.Bottom, FH - Result.Bottom);
    if THookForm(Form).FormStyle = fsMDIChild then
    begin
      Result.Top := FCH + FH;
    end
    else
    begin
      FYOffset := Result.Top;
      Result.Top := Result.Top + FH - 1;
      FYOffset := Result.Top - FYOffset;
      if FYOffset < 0 then FYOffset := 0;
      FXOffset := FW;
    end;
  end;
end;

function TscFormStyleHook.GetForm: TCustomForm;
begin
  Result := TCustomForm(Control);
end;

function TscFormStyleHook.NormalizePoint(P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
  HandleParent: HWnd;
begin
  if (THookForm(Form).FormStyle = fsMDIChild) or (Form.Parent <> nil) then
  begin
    if THookForm(Form).FormStyle = fsMDIChild then
      HandleParent := GetParent(Control.Handle)
    else
      HandleParent := Form.Parent.Handle;
    WindowPos := Point(FLeft, FTop);
    ClientToScreen(HandleParent, WindowPos);
    ClientPos := Point(0, 0);
    ClientToScreen(Handle, ClientPos);
    Result := P;
    ScreenToClient(Handle, Result);
    Inc(Result.X, ClientPos.X - WindowPos.X);
    Inc(Result.Y, ClientPos.Y - WindowPos.Y);
  end
  else
  begin
    WindowPos := Point(FLeft, FTop);
    ClientPos := Point(0, 0);
    ClientToScreen(Handle, ClientPos);
    Result := P;
    ScreenToClient(Handle, Result);
    Inc(Result.X, ClientPos.X - WindowPos.X);
    Inc(Result.Y, ClientPos.Y - WindowPos.Y);
  end;
end;

function TscFormStyleHook.GetHitTest(P: TPoint): Integer;
var
  FBorderSize: TRect;
  FTopLeftRect,  FTopRightRect,
  FBottomLeftRect, FBottomRightRect,
  FTopRect, FLeftRect, FRightRect, FBottomRect, FHitCaptionRect: TRect;
  R: TRect;
begin
  Result := HTCLIENT;
  if Form.BorderStyle = bsNone then
  begin
    if (FMainMenuBarHook <> nil) and FMainMenuBarHook.BoundsRect.Contains(P) then
      Exit(HTMENU)
    else
      Exit;
  end;

  FBorderSize := GetBorderSize;
  FHitCaptionRect := FCaptionRect;
  FHitCaptionRect.Top := FBorderSize.Left;
  FBorderSize.Top := FHitCaptionRect.Top;

  {check buttons and menu}
  if (FMainMenuBarHook <> nil) and FMainMenuBarHook.BoundsRect.Contains(P) then
  begin
    R := FMainMenuBarHook.BoundsRect;
    if R.Bottom > FHeight - FBorderSize.Bottom then
      R.Bottom := FHeight - FBorderSize.Bottom;
    if R.Contains(P) then
      Exit(HTMENU);
  end
  else if FHitCaptionRect.Contains(P) then
    Exit(HTCAPTION)
  else if FCloseButtonRect.Contains(P) then
    Exit(HTCLOSE)
  else if FMaxButtonRect.Contains(P) then
    Exit(HTMAXBUTTON)
  else if FMinButtonRect.Contains(P) then
    Exit(HTMINBUTTON)
  else if FHelpButtonRect.Contains(P) then
    Exit(HTHELP)
  else if FSysMenuButtonRect.Contains(P) then
    Exit(HTSYSMENU);

  {check nc objects}
  if (StyledForm <> nil) and (StyledForm.ShowButtons or StyledForm.ShowTabs) then
  begin
    if StyledForm.FindNCObjectFromPoint(P) <> nil then
      Exit(HTCAPTION);
  end;

  {check window state}
  if (Form.WindowState = wsMaximized) or
     (Form.WindowState = wsMinimized) then
    Exit;

  {check border}
  if (Form.BorderStyle = bsDialog) or
     (Form.BorderStyle = bsSingle) or
     (Form.BorderStyle = bsToolWindow) then
  begin
    if Rect(FBorderSize.Left, FBorderSize.Top,
       FWidth - FBorderSize.Right, FHeight - FBorderSize.Bottom).Contains(P) then
      Exit(HTCLIENT)
    else
      Exit(HTBORDER);
  end;

  FTopLeftRect := Rect(0, 0, FBorderSize.Left, FBorderSize.Top);
  FTopRightRect := Rect(FWidth - FBorderSize.Right, 0, FWidth, FBorderSize.Top);
  FBottomLeftRect := Rect(0, FHeight - FBorderSize.Bottom, FBorderSize.Left, FHeight);
  FBottomRightRect := Rect(FWidth - FBorderSize.Right, FHeight - FBorderSize.Bottom,
    FWidth, FHeight);
  FTopRect := Rect(FTopLeftRect.Right, 0, FTopRightRect.Left, FBorderSize.Top);
  FLeftRect := Rect(0, FTopLeftRect.Bottom, FBorderSize.Left, FBottomLeftRect.Top);
  FRightRect := Rect(FWidth - FBorderSize.Right, FTopRightRect.Bottom, FWidth, FBottomRightRect.Top);
  FBottomRect := Rect(FBottomLeftRect.Right, FHeight - FBorderSize.Bottom, FBottomRightRect.Left, FHeight);

  if FTopLeftRect.Contains(P) then
    Result := HTTOPLEFT
  else if FTopRightRect.Contains(P) then
    Result := HTTOPRIGHT
  else if FBottomLeftRect.Contains(P) then
    Result := HTBOTTOMLEFT
   else if FBottomRightRect.Contains(P) then
    Result := HTBOTTOMRIGHT
  else if FLeftRect.Contains(P) then
    Result := HTLEFT
  else if FRightRect.Contains(P) then
    Result := HTRIGHT
  else if FBottomRect.Contains(P) then
    Result := HTBOTTOM
  else if FTopRect.Contains(P) then
    Result := HTTOP;

end;

procedure TscFormStyleHook.CMDialogChar(var Message: TWMKey);

function FindHotKeyItem: Boolean;
var
  I: Integer;
  Menu: TMainMenu;
begin
  Result := False;
  if (Application.MainForm <> nil) and
     (Application.MainForm.Menu <> nil) then
  begin
    Menu := Application.MainForm.Menu;
    for I := 0 to Menu.Items.Count - 1 do
      if IsAccel(Message.CharCode, Menu.Items[I].Caption) then
      begin
        Result := True;
        Break;
      end;
  end;
  if not Result and (Form.Menu <> nil) then
  begin
    Menu := Form.Menu;
    for I := 0 to Menu.Items.Count - 1 do
      if IsAccel(Message.CharCode, Menu.Items[I].Caption) then
      begin
        Result := True;
        Break;
      end;
  end;
end;

begin
 if (FMainMenuBarHook <> nil) and
     (KeyDataToShiftState(Message.KeyData) = [ssAlt]) and
     FMainMenuBarHook.CheckHotKeyItem(Message.CharCode) then
  begin
    Message.Result := 1;
    Handled := True;
  end
  else
  if (THookForm(Form).FormStyle = fsMDIChild) and
     (KeyDataToShiftState(Message.KeyData) = [ssAlt]) then
  begin
    CallDefaultProc(TMessage(Message));
    if (Message.Result = 0) and FindHotKeyItem then
    begin
      SendMessage(Application.MainForm.Handle, CM_DIALOGCHAR,
        TMessage(Message).wParam, TMessage(Message).lParam);
    end;
    Handled := True;
  end;
end;

procedure TscFormStyleHook.WMSetText(var Message: TMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;
  Form.DefaultHandler(Message);
  InvalidateNC;
  Handled := True;
end;

procedure TscFormStyleHook.WMMDIChildClose(var Message: TMessage);

function IsAnyMDIChildMaximized: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to THookForm(Form).MDIChildCount - 1 do
    if (FChangeVisibleChildHandle <> THookForm(Form).MDIChildren[I].Handle) and
       (THookForm(Form).MDIChildren[I].Visible) and
       (THookForm(Form).MDIChildren[I].WindowState = wsMaximized) then
    begin
      Result := True;
      Break;
    end;
end;

begin
  FChangeVisibleChildHandle := Message.WParam;
  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) then
  begin
    if IsAnyMDIChildMaximized and not FMainMenuBarHook.ShowMDIButtons then
      FMainMenuBarHook.ShowMDIButtons := True
    else if not IsAnyMDIChildMaximized and FMainMenuBarHook.ShowMDIButtons then
      FMainMenuBarHook.ShowMDIButtons := False;
    InvalidateNC;
  end;
  GetMDIScrollInfo(True);
end;

procedure TscFormStyleHook.WMDestroy(var Message: TMessage);
begin
  if not (csRecreating in Form.ControlState) and (THookForm(Form).FormStyle = fsMDIChild) then
    PostMessage(Application.MainForm.Handle, WM_MDICHILDCLOSE, 0, 0);
end;

procedure TscFormStyleHook.WMSysCommand(var Message: TMessage);
begin
  if IsStyleBorder then
    case Message.WParam  of
      SC_RESTORE:
        begin
          FRestoring := True;
          FRestoringConstraints := Form.Constraints;
          if THookForm(Form).FormStyle = fsMDIChild then
          begin
            FSelfRestoring := True;
            SendMessage(Application.MainForm.Handle, WM_CHILDRESTORING,
              Winapi.Windows.WPARAM(Form.Handle), 0);
          end;
        end;
      SC_CLOSE:
        if THookForm(Form).FormStyle = fsMDIChild then
        begin
          PostMessage(Application.MainForm.Handle, WM_MDICHILDCLOSE,
            Winapi.Windows.WPARAM(Form.Handle), 0);
        end;
      SC_MINIMIZE:
      begin
        if THookForm(Form).FormStyle = fsMDIChild then
        begin
          Form.DefaultHandler(Message);
          FFormActive := THookForm(Form).Active;
          SendMessage(Form.Handle, WM_NCPAINT, 0, 0);
        end;
      end;
      SC_KEYMENU:
       begin
         if TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil then
         begin
           if TWMSYSCOMMAND(Message).Key = VK_SPACE then
             FMainMenuBarHook.TrackSystemMenu
           else
           begin
             FMainMenuBarHook.MenuActive := True;
             FMainMenuBarHook.EnterWithKeyboard := True;
             FMainMenuBarHook.MenuEnter(False);
           end;
           Handled := True;
         end;
       end;
    end;
end;

procedure TscFormStyleHook.WMInitMenu(var Message: TMessage);
begin
  if (WPARAM(GetMenu(Control.Handle)) = Message.wParam) and IsStyleBorder then
    SetMenu(Control.Handle, 0);
end;

procedure TscFormStyleHook.CMMenuChanged(var Message: TMessage);
begin
  if IsStyleBorder then
  begin
    if GetMenu(Control.Handle) <> 0 then
      SetMenu(Control.Handle, 0);
     Handled := True;
  end;
end;

procedure TscFormStyleHook.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  FResult: Integer;
begin
  if IsStyleBorder then
  begin
    P := NormalizePoint(Point(Message.XPos, Message.YPos));
    Message.Result := GetHitTest(P);
    if (FStyledForm <> nil) and Assigned(FStyledForm.FOnHitTest) then
    begin
      FResult := Message.Result;
      FStyledForm.FOnHitTest(P, FResult);
      Message.Result := FResult;
    end;
    Handled := True;
  end;
end;

procedure TscFormStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  Params: PNCCalcSizeParams;
  R, MenuRect: TRect;
  MenuHeight: Integer;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;
  {check menu info}
  if (THookForm(Form).FormStyle = fsMDIChild) then
  begin
    if FMainMenuBarHook <> nil then
      FreeAndNil(FMainMenuBarHook);
  end
  else if (Form.Menu <> nil) and not Form.Menu.AutoMerge and
          (Form.Menu.Items.Count > 0) and (FMainMenuBarHook = nil) then
    FMainMenuBarHook := TscFormStyleHook.TscMainMenuBarStyleHook.Create(Self)
  else if ((Form.Menu = nil) or
          ((Form.Menu <> nil) and (Form.Menu.Items.Count = 0))) and
          (FMainMenuBarHook <> nil) then
    FreeAndNil(FMainMenuBarHook);

  if (Form.WindowState = wsMaximized) and (THookForm(Form).FormStyle = fsMDIChild) then
  begin
    Handled := False;
    Exit;
  end;

  {calc NC info}
  if (Message.CalcValidRects and (Form.BorderStyle <> bsNone)) or
     ((Form.BorderStyle = bsNone) and (FMainMenuBarHook <> nil))
  then
  begin
    R := GetBorderSize;

    if FMainMenuBarHook <> nil then
    begin
      MenuHeight := FMainMenuBarHook.GetMenuHeight(FWidth - R.Left - R.Right);
      MenuRect := Rect(R.Left, R.Top, FWidth - R.Right, R.Top + MenuHeight);
      FMainMenuBarHook.BoundsRect := MenuRect;
    end
    else
      MenuHeight := 0;
    Params := Message.CalcSize_Params;
    with Params^.rgrc[0] do
    begin
      Inc(Left, R.Left);
      Inc(Top, R.Top + MenuHeight);
      Dec(Right, R.Right);
      Dec(Bottom, R.Bottom);
      if THookForm(Form).BorderWidth <> 0 then
      begin
        Inc(Left, THookForm(Form).BorderWidth);
        Inc(Top, THookForm(Form).BorderWidth);
        Dec(Right, THookForm(Form).BorderWidth);
        Dec(Bottom, THookForm(Form).BorderWidth);
      end;
    end;
    Handled := True;
  end
  else
  if Message.CalcValidRects and (Form.BorderStyle = bsNone) and
     (THookForm(Form).FormStyle = fsMDIChild) and (Form.WindowState <> wsMaximized) then
    Handled := True;
end;

function TscFormStyleHook.GetIconFast: TIcon;
begin
  if (FIcon = nil) or (FIconHandle = 0) then
    Result := GetIcon
  else
    Result := FIcon;
end;

function TscFormStyleHook.GetIcon: TIcon;
var
  IconX, IconY: Integer;
  TmpHandle: THandle;
  B: Boolean;
begin
  if FIcon = nil then
   FIcon := TIcon.Create;
  B := False;
  if THookForm(Form).Icon.Handle <> 0 then
    TmpHandle := THookForm(Form).Icon.Handle
  else
  if Application.Icon.Handle <> 0 then
    TmpHandle := Application.Icon.Handle
  else
  begin
    TmpHandle := LoadIcon(0, IDI_APPLICATION);
    B := True;
  end;

  IconX := GetSystemMetrics(SM_CXSMICON);
  if IconX = 0 then
    IconX := GetSystemMetrics(SM_CXSIZE);
  IconY := GetSystemMetrics(SM_CYSMICON);
  if IconY = 0 then
    IconY := GetSystemMetrics(SM_CYSIZE);

   if (StyledForm <> nil) and (not THookForm(Form).Scaled or (StyledForm.BorderScaleFactor = 1)) then
   begin
     if IconX > 16 then IconX := 16;
     if IconY > 16 then IconY := 16;
   end;

  FIcon.Handle := CopyImage(TmpHandle, IMAGE_ICON, IconX, IconY, LR_COPYFROMRESOURCE);

  if B then
    DestroyIcon(TmpHandle);

  Result := FIcon;
end;


procedure TscFormStyleHook.PaintNC(Canvas: TCanvas);
var
  Details, CaptionDetails, IconDetails: TThemedElementDetails;
  Detail: TThemedWindow;
  R, R1, R2, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer, Buffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
  TempRect: TRect;
  SaveIndex, IX, IY, Margin: Integer;
  I, L, T, W, H: Integer;
  FIconVisible, FIconCanVisible: Boolean;
  ScaleF: Double;
  PrevButtonRect: TRect;
  IIndex: Integer;
  IRect: TRect;

  procedure CustomDrawCaption(ACanvas: TCanvas; ARect: TRect; AText: String);
  var
    TextColor: TColor;
    S: String;
  begin
    ACanvas.Font := StyledForm.CaptionFont;
    ACanvas.Font.Height := Round(StyledForm.CaptionFont.Height *
      ScaleF);
    if FFormActive then
      TextColor := scDrawUtils.GetCaptionTextColor(scsNormal)
    else
      TextColor := scDrawUtils.GetCaptionTextColor(scsDisabled);
    S := AText;
    ACanvas.Font.Color := TextColor;
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    CorrectTextbyWidth(ACanvas, S, ARect.Width);
    ARect.Top := ARect.Top - FYOffset;
    DrawTextAlignmentNoPrefix(ACanvas, S, ARect, StyledForm.CaptionAlignment, Form.BidiMode = bdRightToLeft);
  end;

  procedure CorrectRightButtonRect(var AButtonRect: TRect);
  begin
    if (THookForm(Form).WindowState = wsMaximized) and
       (THookForm(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0) then
    begin
      if FYOffset > 0 then
        OffsetRect(AButtonRect, -FXOffset + 1, 0)
      else
        OffsetRect(AButtonRect, -FXOffset, 0);
    end;
  end;

  procedure CorrectLeftButtonRect(var AButtonRect: TRect);
  begin
    if (Form.WindowState = wsMaximized) and
       (THookForm(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0) then
      OffsetRect(AButtonRect, FXOffset, 0);
  end;

  procedure DrawBorder;

    function CheckRectInRect(const R1: TRect; var R2: TRect): Boolean;
    begin
      if R2.Top < R1.Top then R2.Top := R1.Top;
      if R2.Bottom > R1.Bottom then R2.Bottom := R1.Bottom;
      if R2.Left < R1.Left then R2.Left := R1.Left;
      if R2.Bottom > R1.Bottom then R2.Bottom := R1.Bottom;
      Result := (R2.Top <= R2.Bottom) and (R2.Left <= R2.Right);
    end;

  var
    BSize: TRect;
  begin

    if THookForm(Form).BorderWidth = 0 then
      Exit;

    Canvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
    Canvas.Brush.Style := bsSolid;
    if Form.BorderStyle = bsNone then
      BSize := Rect(0, 0, 0, 0)
    else
      BSize := GetBorderSize;
    R := Rect(BSize.Left, BSize.Top, FWidth - BSize.Right, FHeight - BSize.Bottom);
    if TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil then
      Inc(R.Top, FMainMenuBarHook.GetMenuHeight(FWidth - BSize.Left - BSize.Right));

    DrawRect := Rect(R.Left, R.Top, R.Left + THookForm(Form).BorderWidth, R.Bottom);
    if CheckRectInRect(R, DrawRect) then Canvas.FillRect(DrawRect);

    DrawRect := Rect(R.Right - THookForm(Form).BorderWidth, R.Top, R.Right, R.Bottom);
    if CheckRectInRect(R, DrawRect) then Canvas.FillRect(DrawRect);

    DrawRect := Rect(R.Left + THookForm(Form).BorderWidth, R.Top,
                     R.Right - THookForm(Form).BorderWidth, R.Top + THookForm(Form).BorderWidth);
    if CheckRectInRect(R, DrawRect) then Canvas.FillRect(DrawRect);

    DrawRect := Rect(R.Left + THookForm(Form).BorderWidth, R.Bottom - THookForm(Form).BorderWidth,
                     R.Right - THookForm(Form).BorderWidth, R.Bottom);
    if CheckRectInRect(R, DrawRect) then Canvas.FillRect(DrawRect);
  end;

begin
  if Form.BorderStyle = bsNone then
  begin
    if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) then
      FMainMenuBarHook.Paint(Canvas);
    DrawBorder;
    Exit;
  end;

  if (THookForm(Form).FormStyle = fsMDIChild) and IsZoomed(Form.Handle) then
    Exit;

  if (Form = Application.MainForm) and IsIconic(Form.Handle) then
    Exit;

  ScaleF := 1;
  if StyledForm <> nil then
  begin
    ScaleF := StyledForm.FScaleFactor;
    if StyledForm.BorderScaleFactor = 1 then
      if ScaleF > 1.25 then ScaleF := 1.25;
  end;

  {init some parameters}
  FCloseButtonRect := Rect(0, 0, 0, 0);
  FMaxButtonRect := Rect(0, 0, 0, 0);
  FMinButtonRect := Rect(0, 0, 0, 0);
  FHelpButtonRect := Rect(0, 0, 0, 0);
  FSysMenuButtonRect := Rect(0, 0, 0, 0);
  FCaptionRect := Rect(0, 0, 0, 0);
  PrevButtonRect := Rect(0, 0, 0, 0);
  if not StyleServices.Available then
    Exit;
  R := GetBorderSize;

  {draw caption}

  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twCaptionActive
    else
      Detail := twCaptionInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallCaptionActive
    else
      Detail := twSmallCaptionInActive
  end;
  CaptionBuffer := TBitmap.Create;
  CaptionBuffer.SetSize(FWidth, R.Top);

  {draw caption border}
  DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
  OffsetRect(DrawRect, 0, FYOffset);
  Details := StyleServices.GetElementDetails(Detail);
  TempRect := DrawRect;
  if FYOffset > 0 then
  begin
    Dec(TempRect.Left, R.Left);
    Inc(TempRect.Right, R.Right);
    if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
    begin
      if TempRect.Top < FYOffset then
        TempRect.Top := FYOffset;
      if TempRect.Bottom <> CaptionBuffer.Height then
        TempRect.Bottom := CaptionBuffer.Height;
    end;
  end;

  if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
  begin
    R1 := Rect(0, 0, CaptionBuffer.Width, FStyledCaptionHeight);
    Buffer := TBitmap.Create;
    try
      Buffer.Width := R1.Width;
      Buffer.Height := R1.Height;
      Buffer.Canvas.Brush.Color := GetStyleColor(clBtnShadow);
      Buffer.Canvas.FillRect(R1);
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R1);
      Margin := R1.Height div 3;
      DrawStrecthBitmap(Margin, Margin, Margin, Margin, Buffer, CaptionBuffer.Canvas, TempRect);
    finally
      Buffer.Free;
    end;
  end
  else
    StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, TempRect);

  if (StyledForm <> nil) and (StyledForm.FCaptionWallpapers <> nil) then
  begin
    IIndex := StyledForm.CaptionWallpaperIndex;
    if not FFormActive and (StyledForm.CaptionWallpaperInActiveIndex >= 0) then
      IIndex := StyledForm.CaptionWallpaperInActiveIndex;
    IRect := TempRect;
    Inc(IRect.Left, StyledForm.CaptionWallpaperLeftMargin);
    Inc(IRect.Top, StyledForm.CaptionWallpaperTopMargin);
    Dec(IRect.Right, StyledForm.CaptionWallpaperRightMargin);
    Dec(IRect.Bottom, StyledForm.CaptionWallpaperBottomMargin);
    if StyledForm.FCaptionWallpapers.IsIndexAvailable(IIndex) then
    begin
      SaveIndex := SaveDC(CaptionBuffer.Canvas.Handle);
      try
        IntersectClipRect(CaptionBuffer.Canvas.Handle,
          IRect.Left, IRect.Top, IRect.Right, IRect.Bottom);
        StyledForm.CaptionWallpapers.Draw(CaptionBuffer.Canvas, IRect, IIndex,
          StyledForm.BorderScaleFactor);
      finally
        RestoreDC(CaptionBuffer.Canvas.Handle, SaveIndex);
      end;
    end;
  end;

  TextRect := DrawRect;
  CaptionDetails := Details;

  {draw icon}
  FIconVisible := False;
  FIconCanVisible := biSystemMenu in THookForm(Form).BorderIcons;
  if (StyledForm <> nil) and StyledForm.ShowIcon and FIconCanVisible and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin)
  then
  begin
    if (biSystemMenu in THookForm(Form).BorderIcons) and
       (Form.BorderStyle <> bsDialog) and
       (Form.BorderStyle <> bsToolWindow) and
       (Form.BorderStyle <> bsSizeToolWin) then
    begin
      IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
      if not StyleServices.GetElementContentRect(0, IconDetails, DrawRect, ButtonRect) then
        ButtonRect := Rect(0, 0, 0, 0);
      ButtonRect.Top := 0;
      ButtonRect.Bottom := FStyledCaptionHeight;
      OffsetRect(ButtonRect, 0, FYOffset);
      {$IFNDEF VER230}
      if not StyleServices.HasElementFixedPosition(Details) then
      {$ENDIF}
        CorrectLeftButtonRect(ButtonRect);
      IX := GetSystemMetrics(SM_CXSMICON);
      IY :=  GetSystemMetrics(SM_CYSMICON);
      if (StyledForm <> nil) and (not THookForm(Form).Scaled or (StyledForm.BorderScaleFactor = 1)) then
      begin
        if IX > 16 then IX := 16;
        if IY > 16 then IY := 16;
      end;
      R1 := Rect(0, 0, IX, IY);
      if ButtonRect.Width > 0 then
      begin
        if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
        begin
          R2 := ButtonRect;
          R2.Right := R2.Left + Round(ButtonRect.Width * StyledForm.BorderScaleFactor);
          R2.Bottom := R2.Top + Round(ButtonRect.Height * StyledForm.BorderScaleFactor);
          ButtonRect := R2;
        end;
        IY := ButtonRect.Top + ButtonRect.Height div 2 - R1.Height div 2;
        IX := ButtonRect.Left + ButtonRect.Width div 2 - R1.Width div 2;
        DrawIconEx(CaptionBuffer.Canvas.Handle, IX, IY, GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
        FIconVisible := True;
        Inc(TextRect.Left, ButtonRect.Width + 5);
      end;
      FSysMenuButtonRect := ButtonRect;
    end;
  end
  else
    if (Form.WindowState <> wsMaximized) or not FIconCanVisible
    then
    begin
      Inc(TextRect.Left, R.Left);
      if not FIconCanVisible and (Form.WindowState = wsMaximized) then
        Inc(TextRect.Left, 5);
    end;

  {draw buttons}
  if (biSystemMenu in THookForm(Form).BorderIcons) then
  begin
    if (Form.BorderStyle <> bsToolWindow) and
       (Form.BorderStyle <> bsSizeToolWin) then
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else
        if FFormActive then
          FButtonState := twCloseButtonNormal
        else
          FButtonState := twCloseButtonDisabled;
     end
    else
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else
        if FFormActive then
          FButtonState := twSmallCloseButtonNormal
        else
          FButtonState := twSmallCloseButtonDisabled;
    end;

    Details := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    OffsetRect(ButtonRect, 0, FYOffset);
    {$IFNDEF VER230}
    if not StyleServices.HasElementFixedPosition(Details) then
    {$ENDIF}
     CorrectRightButtonRect(ButtonRect);

    if ButtonRect.Width > 0 then
    begin
      if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
      begin
        R1 := ButtonRect;
        R1.Left := R1.Right - Round(ButtonRect.Width * StyledForm.BorderScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * StyledForm.BorderScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBuffer.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R2);
          if R1.Left - 1 + Round(Buffer.Width * StyledForm.BorderScaleFactor) >
            CaptionBuffer.Width - 1
          then
            IntersectClipRect(CaptionBuffer.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * StyledForm.BorderScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBuffer.Canvas,
            R1.Left - 1, R1.Top - 1, 255, StyledForm.BorderScaleFactor);
        finally
          RestoreDC(CaptionBuffer.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    end;
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FCloseButtonRect := ButtonRect;
    PrevButtonRect := FCloseButtonRect;
  end;

  if (biMaximize in THookForm(Form).BorderIcons) and
     (biSystemMenu in THookForm(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if Form.WindowState = wsMaximized then
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else
      if FFormActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else
      if FFormActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;

    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    OffsetRect(ButtonRect, 0, FYOffset);
    {$IFNDEF VER230}
    if not StyleServices.HasElementFixedPosition(Details) then
    {$ENDIF}
     CorrectRightButtonRect(ButtonRect);

    if ButtonRect.Width > 0 then
    begin
      if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
      begin
        R1 := ButtonRect;
        if PrevButtonRect.Width > 0 then
          R1.Right := PrevButtonRect.Left
        else
          R1.Right := CaptionBuffer.Width - Round((CaptionBuffer.Width - R1.Right) * StyledForm.BorderScaleFactor) + 2;
        R1.Left := R1.Right - Round(ButtonRect.Width * StyledForm.BorderScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * StyledForm.BorderScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBuffer.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R2);
          if R1.Left - 1 + Round(Buffer.Width * StyledForm.BorderScaleFactor) >
            CaptionBuffer.Width - 1
          then
            IntersectClipRect(CaptionBuffer.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * StyledForm.BorderScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBuffer.Canvas,
            R1.Left - 1, R1.Top - 1, 255, StyledForm.BorderScaleFactor);
        finally
          RestoreDC(CaptionBuffer.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    end;
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FMaxButtonRect := ButtonRect;
    PrevButtonRect := FMaxButtonRect;
  end;

  if (biMinimize in THookForm(Form).BorderIcons) and
     (biSystemMenu in THookForm(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin

    if (Form.WindowState = wsMinimized) and
       ((THookForm(Form).FormStyle = fsMDIChild) or (Form <> Application.MainForm)) then
    begin
      if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMINBUTTON then
        FButtonState := twRestoreButtonHot
      else
      if FFormActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
        FButtonState := twMinButtonPushed
      else if FHotButton = HTMINBUTTON then
        FButtonState := twMinButtonHot
      else
        if FFormActive then
          FButtonState := twMinButtonNormal
        else
          FButtonState := twMinButtonDisabled;
    end;

    Details := StyleServices.GetElementDetails(twMinButtonNormal);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    Details := StyleServices.GetElementDetails(FButtonState);

    OffsetRect(ButtonRect, 0, FYOffset);
    {$IFNDEF VER230}
    if not StyleServices.HasElementFixedPosition(Details) then
    {$ENDIF}
      CorrectRightButtonRect(ButtonRect);

    if ButtonRect.Width > 0 then
    begin
      if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
      begin
        R1 := ButtonRect;
        if PrevButtonRect.Width > 0 then
          R1.Right := PrevButtonRect.Left
        else
          R1.Right := CaptionBuffer.Width - Round((CaptionBuffer.Width - R1.Right) * StyledForm.BorderScaleFactor);
        R1.Left := R1.Right - Round(ButtonRect.Width * StyledForm.BorderScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * StyledForm.BorderScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBuffer.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R2);
          if R1.Left - 1 + Round(Buffer.Width * StyledForm.BorderScaleFactor) >
            CaptionBuffer.Width - 1
          then
            IntersectClipRect(CaptionBuffer.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * StyledForm.BorderScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBuffer.Canvas,
            R1.Left - 1, R1.Top - 1, 255, StyledForm.BorderScaleFactor);
        finally
          RestoreDC(CaptionBuffer.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    end;
    if ButtonRect.Left > 0 then TextRect.Right := ButtonRect.Left;
    FMinButtonRect := ButtonRect;
    PrevButtonRect := FMinButtonRect;
  end;

  if (biHelp in THookForm(Form).BorderIcons) and (biSystemMenu in THookForm(Form).BorderIcons) and
     ((not (biMaximize in THookForm(Form).BorderIcons) and
     not (biMinimize in THookForm(Form).BorderIcons)) or (Form.BorderStyle = bsDialog))
  then
  begin
    if (FPressedButton = HTHELP) and (FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else
    if FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    OffsetRect(ButtonRect, 0, FYOffset);
    {$IFNDEF VER230}
    if not StyleServices.HasElementFixedPosition(Details) then
    {$ENDIF}
      CorrectRightButtonRect(ButtonRect);

    if ButtonRect.Width > 0 then
    begin
      if (StyledForm <> nil) and (StyledForm.BorderScaleFactor > 1) then
      begin
        R1 := ButtonRect;
        if PrevButtonRect.Width > 0 then
          R1.Right := PrevButtonRect.Left
        else
        R1.Right := CaptionBuffer.Width - Round((CaptionBuffer.Width - R1.Right) * StyledForm.BorderScaleFactor);
        R1.Left := R1.Right - Round(ButtonRect.Width * StyledForm.BorderScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * StyledForm.BorderScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBuffer.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R2);
          if R1.Left - 1 + Round(Buffer.Width * StyledForm.BorderScaleFactor) >
            CaptionBuffer.Width - 1
          then
            IntersectClipRect(CaptionBuffer.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * StyledForm.BorderScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBuffer.Canvas,
            R1.Left - 1, R1.Top - 1, 255, StyledForm.BorderScaleFactor);
        finally
          RestoreDC(CaptionBuffer.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    end;

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FHelpButtonRect := ButtonRect;
  end;

  if (Form.WindowState = wsMaximized) and (THookForm(Form).FormStyle <> fsMDIChild) and
     (FYOffset <> 0) and (biSystemMenu in THookForm(Form).BorderIcons) then
    Inc(TextRect.Left, R.Left);

  FCaptionRect := TextRect;

  if FIconVisible then
    Inc(TextRect.Left, 5);

  {draw right buttons}
  if (StyledForm <> nil) and (StyledForm.Buttons.Count > 0) and StyledForm.ShowButtons then
  begin
    CaptionBuffer.Canvas.Font := StyledForm.ButtonFont;
    CaptionBuffer.Canvas.Font.Height := Round(StyledForm.ButtonFont.Height *
      ScaleF);
    for I := 0 to StyledForm.Buttons.Count - 1 do
      if StyledForm.Buttons[I].Visible and (StyledForm.Buttons[I].Position = scbpRight) then
      begin
        if StyledForm.Buttons[I].Height > 0 then
          H := StyledForm.Buttons[I].Height
        else
          H := TextRect.Height - FYOffset - StyledForm.Buttons[I].MarginTop - StyledForm.Buttons[I].MarginBottom;
        if StyledForm.Buttons[I].Width > 0 then
          W := Round(StyledForm.Buttons[I].Width * ScaleF)
        else
          W := H;
        W := StyledForm.Buttons[I].NCObject.GetObjectWidth(W, CaptionBuffer.Canvas);    
        L := TextRect.Right - W - StyledForm.Buttons[I].MarginRight;
        T := TextRect.Top + StyledForm.Buttons[I].MarginTop;
        StyledForm.Buttons[I].NCObject.ObjectRect := Rect(L, T, L + W, T + H);

        if StyledForm.Buttons[I].NCObject.ObjectRect.Left < TextRect.Left then
          StyledForm.Buttons[I].NCObject.ObjectRect := Rect(0, 0, 0, 0)
        else
        begin
          TextRect.Right := L - StyledForm.Buttons[I].MarginLeft;
          StyledForm.Buttons[I].NCObject.Draw(CaptionBuffer.Canvas);
        end;
      end
      else
      if not StyledForm.Buttons[I].Visible then
        StyledForm.Buttons[I].NCObject.FMouseIn := False;
    Dec(TextRect.Right, 5);
  end;
  {draw right tabs}
  if (StyledForm <> nil) and (StyledForm.Tabs.Count > 0) and (StyledForm.TabsPosition = sctpRight)
      and StyledForm.ShowTabs
  then
  begin
    Dec(TextRect.Right, 5);
    CaptionBuffer.Canvas.Font := StyledForm.TabFont;
    CaptionBuffer.Canvas.Font.Height := Round(StyledForm.ButtonFont.Height *
      ScaleF);
    for I := StyledForm.Tabs.Count - 1 downto 0 do
      if StyledForm.Tabs[I].Visible then
      begin
        if StyledForm.Tabs[I].Height > 0 then
          H := StyledForm.Tabs[I].Height
        else
          H := TextRect.Height - FYOffset - StyledForm.Tabs[I].MarginTop;
        if StyledForm.Tabs[I].Width > 0 then
          W := Round(StyledForm.Tabs[I].Width * ScaleF)
        else
          W := H;
        W := StyledForm.Tabs[I].NCObject.GetObjectWidth(W, CaptionBuffer.Canvas);
        L := TextRect.Right - W;
        T := TextRect.Top + StyledForm.Tabs[I].MarginTop;
        StyledForm.Tabs[I].NCObject.ObjectRect := Rect(L, T, L + W, T + H);
        if StyledForm.Tabs[I].NCObject.ObjectRect.Left < TextRect.Left then
          StyledForm.Tabs[I].NCObject.ObjectRect := Rect(0, 0, 0, 0)
        else
        begin
          TextRect.Right := L;
          StyledForm.Tabs[I].NCObject.Draw(CaptionBuffer.Canvas);
        end;
      end;
    Dec(TextRect.Right, 5);
  end;
  {draw left buttons}
  if (StyledForm <> nil) and (StyledForm.Buttons.Count > 0) and StyledForm.ShowButtons then
  begin
    CaptionBuffer.Canvas.Font := StyledForm.ButtonFont;
    CaptionBuffer.Canvas.Font.Height := Round(StyledForm.ButtonFont.Height *
      ScaleF);
    for I := 0 to StyledForm.Buttons.Count - 1 do
      if StyledForm.Buttons[I].Visible and (StyledForm.Buttons[I].Position = scbpLeft) then
      begin
        if StyledForm.Buttons[I].Height > 0 then
          H := StyledForm.Buttons[I].Height
        else
          H := TextRect.Height - FYOffset - StyledForm.Buttons[I].MarginTop - StyledForm.Buttons[I].MarginBottom;
        if StyledForm.Buttons[I].Width > 0 then
          W := Round(StyledForm.Buttons[I].Width * ScaleF)
        else
          W := H;
        W := StyledForm.Buttons[I].NCObject.GetObjectWidth(W, CaptionBuffer.Canvas);
        L := TextRect.Left + StyledForm.Buttons[I].MarginLeft;
        T := TextRect.Top + StyledForm.Buttons[I].MarginTop;
        StyledForm.Buttons[I].NCObject.ObjectRect := Rect(L, T, L + W, T + H);
        if StyledForm.Buttons[I].NCObject.ObjectRect.Right > TextRect.Right then
          StyledForm.Buttons[I].NCObject.ObjectRect := Rect(0, 0, 0, 0)
        else
        begin
          TextRect.Left := TextRect.Left + W +
            StyledForm.Buttons[I].MarginLeft + StyledForm.Buttons[I].MarginRight;
          StyledForm.Buttons[I].NCObject.Draw(CaptionBuffer.Canvas);
        end;
      end;
    Inc(TextRect.Left, 5);
  end;
  {draw left tabs}
  if (StyledForm <> nil) and (StyledForm.Tabs.Count > 0) and (StyledForm.TabsPosition = sctpLeft)
     and StyledForm.ShowTabs
  then
  begin
    Inc(TextRect.Left, 5);
    CaptionBuffer.Canvas.Font := StyledForm.TabFont;
    CaptionBuffer.Canvas.Font.Height := Round(StyledForm.ButtonFont.Height *
      ScaleF);
    for I := 0 to StyledForm.Tabs.Count - 1 do
      if StyledForm.Tabs[I].Visible then
      begin
        if StyledForm.Tabs[I].Height > 0 then
          H := StyledForm.Tabs[I].Height
        else
          H := TextRect.Height - FYOffset - StyledForm.Tabs[I].MarginTop;
        if StyledForm.Tabs[I].Width > 0 then
          W := Round(StyledForm.Tabs[I].Width * ScaleF)
        else
          W := H;
        W := StyledForm.Tabs[I].NCObject.GetObjectWidth(W, CaptionBuffer.Canvas);
        L := TextRect.Left;
        T := TextRect.Top + StyledForm.Tabs[I].MarginTop;
        StyledForm.Tabs[I].NCObject.ObjectRect := Rect(L, T, L + W, T + H);
        if StyledForm.Tabs[I].NCObject.ObjectRect.Right > TextRect.Right then
          StyledForm.Tabs[I].NCObject.ObjectRect := Rect(0, 0, 0, 0)
        else
        begin
          TextRect.Left := TextRect.Left + W;
          StyledForm.Tabs[I].NCObject.Draw(CaptionBuffer.Canvas);
        end;
      end;
     Inc(TextRect.Left, 5);
   end;

  {draw text}
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if Control.UseRightToLeftReading then
    Include(TextFormat, tfRtlReading);

  if (Form.BorderStyle = bsDialog) and (biSystemMenu in THookForm(Form).BorderIcons) then
   Inc(TextRect.Left, 10);

  LText := Text;

  if (Form.WindowState = wsMaximized) and (THookForm(Form).FormStyle <> fsMDIChild) and
     (FYOffset <> 0) and (biSystemMenu in THookForm(Form).BorderIcons) then
  begin
    if StyledForm <> nil then
    begin
      CustomDrawCaption(CaptionBuffer.Canvas, TextRect, LText)
    end
    else
    begin
      MoveWindowOrg(CaptionBuffer.Canvas.Handle, 0, FYOffset);
      StyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
      MoveWindowOrg(CaptionBuffer.Canvas.Handle, 0, -FYOffset);
    end;
  end
  else
  begin
    if StyledForm <> nil then
      CustomDrawCaption(CaptionBuffer.Canvas, TextRect, LText)
    else
      StyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
  end;

  {draw caption buffer}

  Canvas.Draw(0, 0, CaptionBuffer);
  CaptionBuffer.Free;

  {draw menubar}
  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and
     (FMainMenuBarHook.BoundsRect.Right < FWidth - R.Right) then
     FMainMenuBarHook.BoundsRect :=
       Rect(FMainMenuBarHook.BoundsRect.Left, FMainMenuBarHook.BoundsRect.Top,
       FWidth - R.Right, FMainMenuBarHook.BoundsRect.Bottom);
  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) then
    FMainMenuBarHook.Paint(Canvas);

  {draw left border}

  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameLeftActive
    else
      Detail := twFrameLeftInActive
  end
  else
  begin
    if FFormActive then
      Detail := twSmallFrameLeftActive
    else
      Detail := twSmallFrameLeftInActive
  end;
  DrawRect := Rect(0, R.Top, R.Left, FHeight - R.Bottom);
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  {draw right border}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameRightActive
    else
      Detail := twFrameRightInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallFrameRightActive
    else
      Detail := twSmallFrameRightInActive
  end;
  DrawRect := Rect(FWidth - R.Right, R.Top, FWidth, FHeight - R.Bottom);
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  {draw Bottom border}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameBottomActive
    else
      Detail := twFrameBottomInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallFrameBottomActive
    else
      Detail := twSmallFrameBottomInActive;
  end;

  if IsIconic(Form.Handle) and (Form <> Application.MainForm) then
  begin
    GetWindowRect(Form.Handle, R1);
    DrawRect := Rect(-R.Left, R1.Height - R.Bottom, R1.Width + R.Right, R1.Height);
    R2 := Rect(1, R1.Height - 3, R1.Width - 1, R1.Height);
    Details := StyleServices.GetElementDetails(Detail);
    SaveIndex := SaveDC(Canvas.Handle);
    IntersectClipRect(Canvas.Handle, R2.Left, R2.Top, R2.Right, R2.Bottom);
    try
     if DrawRect.Bottom - DrawRect.Top > 0 then
       StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end
  else
  begin
    DrawRect := Rect(0, FHeight - R.Bottom, FWidth, FHeight);
    Details := StyleServices.GetElementDetails(Detail);
    if DrawRect.Bottom - DrawRect.Top > 0 then
      StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
  end;
  DrawBorder;
end;

procedure TscFormStyleHook.WMNCACTIVATE(var Message: TMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  FFormActive := Message.WParam > 0;

  if StyledForm <> nil then
  begin
    if (FActiveNCObject <> nil) and (StyledForm.FindNCObject(FActiveNCObject)) then
    begin
      FActiveNCObject.FMouseIn := False;
    end;
  end;

  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and FMainMenuBarHook.InMenuLoop then
    FMainMenuBarHook.InMenuLoop := False;

  if (THookForm(Form).FormStyle = fsMDIChild) then
  begin
    if (THookForm(Form).FormStyle = fsMDIChild) and (Win32MajorVersion >=6) then
      SetRedraw(False);

    CallDefaultProc(Message);

    if (THookForm(Form).FormStyle = fsMDIChild) and (Win32MajorVersion >=6) then
    begin
      SetRedraw(True);
      if not (csDestroying in Control.ComponentState) and
         not (csLoading in Control.ComponentState) then
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE + RDW_ALLCHILDREN + RDW_UPDATENOW);
    end;
  end
  else
    Message.Result := 1;

  if THookForm(Form).ClientHandle <> 0 then
    PostMessage(TForm(Control).ClientHandle, WM_NCACTIVATE, Message.WParam, Message.LParam);

  if (Form.BorderStyle <> bsNone) and
     not ((THookForm(Form).FormStyle = fsMDIChild) and
     (Form.WindowState = wsMaximized)) then
    InvalidateNC;

  Handled := True;
end;

function TscFormStyleHook.GetRegion: HRgn;
var
  R, R1, ScrR: TRect;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
  P: TPoint;
  LeftOffset, TopOffset, RightOffset, BottomOffset: Integer;
begin
  Result := 0;
  if not StyleServices.Available then
    Exit;
  R := Rect(0, 0, FWidth, FHeight);
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementRegion(Details, R, Result);
  if (Form.WindowState = wsMaximized) and (THookForm(Form).FormStyle <> fsMDIChild) and
     (Form.BorderStyle <> bsNone) then
  begin
    P.X := FLeft + FWidth div 2;
    P.Y := FTop + FHeight div 2;
    R := Screen.MonitorFromPoint(P).WorkareaRect;
    R1 := Screen.MonitorFromPoint(P).BoundsRect;
    ScrR := Rect(R1.Left, R1.Top, R1.Left + R1.Right, R1.Top + R1.Bottom);
    LeftOffset := 0;
    TopOffset := 0;
    RightOffset := 0;
    BottomOffset := 0;

    if (R.Top <> R1.Top) and (R.Top > 0) and (FTop < 0) and (Abs(FTop) < 100) then
      TopOffset := Abs(FTop)
    else
    if (FTop < R.Top) and (R.Top - FTop < 100) then
      TopOffset := R.Top - FTop
    else
    if (FTop < ScrR.Top) and (ScrR.Top - FTop < 100) then
      TopOffset := ScrR.Top - FTop;

    if (R.Left <> R1.Left) and (R.Left > 0) and (FLeft < 0) and (Abs(FLeft) < 100)  then
      LeftOffset := Abs(FLeft)
    else
    if (FLeft < R.Left) and (R.Left - FLeft < 100) then
      LeftOffset := R.Left - FLeft
    else
    if (FLeft < ScrR.Left) and (ScrR.Left - FLeft < 100) then
      LeftOffset := ScrR.Left - FLeft;

    if (FLeft + FWidth > R.Right) and (FLeft + FWidth - R.Right < 100) then
      RightOffset := FLeft + FWidth - R.Right
    else
    if (FLeft + FWidth > ScrR.Right) and (FLeft + FWidth - ScrR.Right < 100) then
      RightOffset := FLeft + FWidth - ScrR.Right;

    if (FTop + FHeight > R.Bottom) and (FTop + FHeight - R.Bottom < 100) then
      BottomOffset := FTop + FHeight - R.Bottom
    else
    if (FTop + FHeight > ScrR.Bottom) and (FTop + FHeight - ScrR.Bottom < 100) then
      BottomOffset := FTop + FHeight - ScrR.Bottom;

     if (LeftOffset <> 0) or (RightOffset <> 0) or
        (TopOffset <> 0) or (BottomOffset <> 0)
     then
       SetRectRgn(Result, LeftOffset, TopOffset,
         FWidth - RightOffset, FHeight - BottomOffset);
  end;
end;

procedure TscFormStyleHook.ChangeSize;
var
  R: TRect;
begin
  FChangeSizeCalled := True;
  try
    if IsIconic(Handle) then
    begin
      R := GetBorderSize;
      FHeight := R.Top + R.Bottom;
    end;
    if Form.BorderStyle <> bsNone then
    begin
      FRegion := GetRegion;
      SetWindowRgn(Handle, FRegion, True);
    end
    else
      if (FStyledForm <> nil) and FStyledForm.DWMClientShadow then
      begin
      end
      else
      if (Form.BorderStyle = bsNone) and (THookForm(Form).FormStyle = fsMDIChild)
         and (Form.WindowState <> wsMaximized) then
      begin
        FRegion := CreateRectRgn(0, 0, FWidth, FHeight);
        SetWindowRgn(Handle, FRegion, True);
      end
      else
      if (Form.BorderStyle = bsNone) and (FRegion <> 0) then
      begin
        SetWindowRgn(Handle, 0, True);
        FRegion := 0;
      end;
  finally
    FChangeSizeCalled := False;
  end;
end;

procedure TscFormStyleHook.WMMove(var Message: TWMMOVE);
begin
  if THookForm(Form).FormStyle = fsMDIChild then
  begin
    CallDefaultProc(TMessage(Message));
    SendMessage(Application.MainForm.Handle, WM_MDICHILDMOVE, 0, 0);
    Handled := True;
  end;
end;

procedure TscFormStyleHook.WMMDIChildMove(var Message: TMessage);
begin
  if (THookForm(Form).FormStyle = fsMDIForm) and not FStopCheckChildMove  then
  begin
    FChangeVisibleChildHandle := Message.WParam;
    GetMDIScrollInfo(True);
    FChangeVisibleChildHandle := 0;
    if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and IsStyleBorder then
    begin
      //TODO -cStyles: Optimize
      if MDIChildMaximized and not FMainMenuBarHook.ShowMDIButtons then
        FMainMenuBarHook.ShowMDIButtons := True
      else if not MDIChildMaximized and FMainMenuBarHook.ShowMDIButtons then
        FMainMenuBarHook.ShowMDIButtons := False;
    end;
    Handled := True;
  end;
end;

procedure TscFormStyleHook.WMSize(var Message: TWMSize);
begin
  if IsIconic(Handle) and (Application.MainForm.Handle <> Handle) and IsStyleBorder then
    InvalidateNC;

  if (FMDIClientInstance <> nil) then
  begin
    CallDefaultProc(TMessage(Message));
    GetMDIScrollInfo(True);
    Handled := True;
    Exit;
  end;

  if THookForm(Form).FormStyle = fsMDIChild then
  begin
    CallDefaultProc(TMessage(Message));
    SendMessage(Application.MainForm.Handle, WM_MDICHILDMOVE, 0, 0);
    if IsIconic(Handle) and IsStyleBorder then
      InvalidateNC;
    Handled := True;
  end;

end;

procedure TscFormStyleHook.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  Changed: Boolean;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  CallDefaultProc(TMessage(Message));

  if (Message.WindowPos^.flags and SWP_SHOWWINDOW <> 0) and FNeedsUpdate then
  begin
    FNeedsUpdate := False;
    if (Control is TForm) and (TForm(Control).FormStyle = fsMDIForm) and (FMDIClientInstance = nil) then
    begin
      FMDIPrevClientProc := Pointer(GetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC));
      FMDIClientInstance := MakeObjectInstance(MDIClientWndProc);
      SetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC, IntPtr(FMDIClientInstance));
      InitMDIScrollBars;
      AdjustMDIScrollBars;
    end;
    if IsStyleBorder and not TStyleManager.SystemStyle.Enabled and (GetWindowLong(Handle, GWL_STYLE) and WS_CAPTION <> 0) and
       not (THookForm(Form).FormStyle = fsMDIChild) then
    begin
      FCaptionEmulation := True;
      SetWindowLong(Handle, GWL_STYLE,
        GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION);
    end;
  end;

  Handled := True;
  Changed := False;

  if FChangeSizeCalled then
  begin
    if FRestoring then
    begin
      FRestoring := False;
      if (FRestoringConstraints.MinHeight <> 0) or
         (FRestoringConstraints.MinWidth <> 0) or
         (FRestoringConstraints.MaxWidth <> 0) or
         (FRestoringConstraints.MaxHeight <> 0) then
        Form.Constraints := FRestoringConstraints;
    end;
    Exit;
  end;

  if (Message.WindowPos^.flags and SWP_NOSIZE = 0) or
     (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
  begin
    if (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
    begin
      FLeft := Message.WindowPos^.x;
      FTop := Message.WindowPos^.y;
    end;
    if (Message.WindowPos^.flags and SWP_NOSIZE = 0) then
    begin
      Changed := ((Message.WindowPos^.cx <> FWidth) or (Message.WindowPos^.cy <> FHeight)) and
                 (Message.WindowPos^.flags and SWP_NOSIZE = 0);
      FWidth := Message.WindowPos^.cx;
      FHeight := Message.WindowPos^.cy;
    end;
  end;

  if (Message.WindowPos^.flags and SWP_FRAMECHANGED  <> 0) then
    Changed := True;

  if Changed then
  begin
    ChangeSize;
    if Form.BorderStyle <> bsNone then
      InvalidateNC;
  end;
end;

procedure TscFormStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_STYLECHANGED:
    begin
      if THookForm(Form).FormStyle = fsMDIForm then
      begin
        GetMDIScrollInfo(True);
      end;
    end;
    WM_CHILDRESTORING:
      begin
        FChildRestoring := True;
      end;
    WM_CHILDRESTORED:
    begin
      if FChildRestoring  then
      begin
        FChildRestoring := False;
        SendMessage(Application.MainForm.Handle, WM_MDICHILDMOVE, 0, 0);
      end;
    end;
    WM_WINDOWPOSCHANGED:
    begin
      if (THookForm(Form).FormStyle = fsMDIChild) and FSelfRestoring then
      begin
        FSelfRestoring := False;
        PostMessage(Application.MainForm.Handle, WM_CHILDRESTORED,
          Winapi.Windows.WPARAM(Form.Handle), 0);
      end;

      if IsStyleBorder and (Form.WindowState = wsMaximized) then
        with TWMWindowPosChanged(Message) do
          if (WindowPos^.flags and SWP_NOSIZE = 0) or
             (WindowPos^.flags and SWP_NOMOVE = 0) then
          begin
            if (WindowPos^.flags and SWP_NOMOVE = 0) then
            begin
              FLeft := WindowPos^.x;
              FTop := WindowPos^.y;
            end;
            if (WindowPos^.flags and SWP_NOSIZE = 0) then
            begin
              FWidth := WindowPos^.cx;
              FHeight := WindowPos^.cy;
            end;
          end;
    end;
  end;
end;

procedure TscFormStyleHook.UpdateForm;
begin
  if Form.BorderStyle = bsNone then Exit;
  Control.Width := Control.Width - 1;
  Control.Width := Control.Width + 1;
end;

procedure TscFormStyleHook.WMNCMouseMove(var Message: TWMNCHitMessage);
var
  P: TPoint;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  inherited;

  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and (Message.HitTest = HTMENU) then
  begin
    P := NormalizePoint(Point(Message.XCursor, Message.YCursor));
    P.X := P.X - FMainMenuBarHook.BoundsRect.Left;
    P.Y := P.Y - FMainMenuBarHook.BoundsRect.Top;
    FMainMenuBarHook.MouseMove(P.X, P.Y);
    Handled := True;
  end
  else if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and FMainMenuBarHook.MouseInMainMenu and (Message.HitTest <> HTMENU) then
    FMainMenuBarHook.MouseMove(-1, -1);


  if (Message.HitTest = HTCLOSE) or (Message.HitTest = HTMAXBUTTON) or
     (Message.HitTest = HTMINBUTTON) or (Message.HitTest = HTHELP) then
  begin
    if FHotButton <> Message.HitTest then
    begin
      FHotButton := Message.HitTest;
      InvalidateNC;
    end;
    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end
  else if FHotButton <> 0 then
   begin
     FHotButton := 0;
     InvalidateNC;
   end;

  // nc objects
  if (StyledForm <> nil) and (StyledForm.ShowButtons or FStyledForm.ShowTabs) then
  begin
    P := NormalizePoint(Point(Message.XCursor, Message.YCursor));
    FActiveNCObject := StyledForm.FindNCObjectFromPoint(P);
    if (FActiveNCObject <> nil) and (FActiveNCObject <> FOldNCObject) then
    begin
      if (FOldNCObject <> nil) and StyledForm.FindNCObject(FOldNCObject) and
         StyledForm.IsNCObjectEnabled(FOldNCObject)
      then
        FOldNCObject.MouseLeave;
      if StyledForm.IsNCObjectEnabled(FActiveNCObject) then
      begin
        FActiveNCObject.MouseEnter;
        FActiveNCObject.MouseMove(P.X, P.Y);
      end;
    end
    else
    if (FActiveNCObject <> nil) and (FActiveNCObject = FOldNCObject) then
    begin
      if StyledForm.IsNCObjectEnabled(FActiveNCObject) then
        FActiveNCObject.MouseMove(P.X, P.Y);
    end
    else
    if (FActiveNCObject = nil) and (FOldNCObject <> nil) and
        StyledForm.FindNCObject(FOldNCObject) then
    begin
      if StyledForm.IsNCObjectEnabled(FOldNCObject) then
        FOldNCObject.MouseLeave;
    end;
    FOldNCObject := FActiveNCObject;
  end;
end;

procedure TscFormStyleHook.WMNCRButtonDown(var Message: TWMNCHitMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;
  inherited;
  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and (Message.HitTest = HTMENU) then
    Handled := True;
   if (StyledForm <> nil) and (StyledForm.HintComponent <> nil) and StyledForm.ShowHints
   then
     StyledForm.HintComponent.HideHint;
end;

procedure TscFormStyleHook.WMNCLButtonDown(var Message: TWMNCHitMessage);
var
  P: TPoint;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  inherited;

  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and FMainMenuBarHook.MustActivateMDIChildSysMenu then
  begin
    FMainMenuBarHook.InMenuLoop := False;
    FMainMenuBarHook.MustActivateMDIChildSysMenu := False;
    FMainMenuBarHook.TrackMDIChildSystemMenu;
    Handled := True;
    Exit;
  end;

  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and FMainMenuBarHook.MustActivateSysMenu then
  begin
    FMainMenuBarHook.InMenuLoop := False;
    FMainMenuBarHook.MustActivateSysMenu := False;
    FMainMenuBarHook.TrackSystemMenu;
    Handled := True;
    Exit;
  end;

  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and FMainMenuBarHook.MustActivateMenuItem then
  begin
    FMainMenuBarHook.InMenuLoop := False;
    FMainMenuBarHook.MustActivateMenuItem := False;
    FMainMenuBarHook.ProcessMenuLoop(True);
    Handled := True;
    Exit;
  end;

  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and (Message.HitTest = HTMENU) then
  begin
    P := NormalizePoint(Point(Message.XCursor, Message.YCursor));
    P.X := P.X - FMainMenuBarHook.BoundsRect.Left;
    P.Y := P.Y - FMainMenuBarHook.BoundsRect.Top;
    FMainMenuBarHook.MouseDown(P.X, P.Y);
    Handled := True;
  end;

  if (Message.HitTest = HTCLOSE) or (Message.HitTest = HTMAXBUTTON) or
     (Message.HitTest = HTMINBUTTON) or (Message.HitTest = HTHELP) then
  begin
    FPressedButton := Message.HitTest;
    InvalidateNC;
    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end;

  // nc objects
  if (StyledForm <> nil) and (StyledForm.ShowButtons or FStyledForm.ShowTabs) then
  begin
    if (StyledForm.HintComponent <> nil) and StyledForm.ShowHints
    then
      StyledForm.HintComponent.HideHint;
    P := NormalizePoint(Point(Message.XCursor, Message.YCursor));
    FActiveNCObject := StyledForm.FindNCObjectFromPoint(P);
    if FActiveNCObject <> nil then
    begin
      if not FFormActive then
         Form.SetFocus;
      if StyledForm.IsNCObjectEnabled(FActiveNCObject) then
        FActiveNCObject.MouseDown(P.X, P.Y);
      FCapturedNCObject := FActiveNCObject;
      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
    end;
  end;
end;

procedure TscFormStyleHook.WMNCRButtonUp(var Message: TWMNCHitMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  // call system menu
  if (Message.HitTest = HTCAPTION) and FCaptionEmulation then
  begin
    SendMessage(Handle, $313, 0,
      MakeLong(Message.XCursor, Message.YCursor));
  end;
end;

procedure TscFormStyleHook.WMNCLButtonUp(var Message: TWMNCHitMessage);
var
  FWasPressedButton: Integer;
  P: TPoint;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  FWasPressedButton := FPressedButton;

  if FPressedButton <> 0 then
  begin
    FPressedButton := 0;
    InvalidateNC;
  end;

  if (TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil) and (Message.HitTest = HTMENU) then
  begin
    P := NormalizePoint(Point(Message.XCursor, Message.YCursor));
    P.X := P.X - FMainMenuBarHook.BoundsRect.Left;
    P.Y := P.Y - FMainMenuBarHook.BoundsRect.Top;
    FMainMenuBarHook.MouseUp(P.X, P.Y);
    Handled := True;
  end;

  // nc objects
  if (StyledForm <> nil) and (StyledForm.ShowButtons or FStyledForm.ShowTabs) then
  begin
    P := NormalizePoint(Point(Message.XCursor, Message.YCursor));
    FActiveNCObject := StyledForm.FindNCObjectFromPoint(P);
    if (FActiveNCObject <> nil) and (FCapturedNCObject <> nil) and
       (FCapturedNCObject = FActiveNCObject) then
    begin
      if StyledForm.IsNCObjectEnabled(FActiveNCObject) then
        FActiveNCObject.MouseUp(P.X, P.Y);
      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
    end;
    FCapturedNCObject := nil;
    Handled := True;
  end;

  if (Message.HitTest = HTTOP) or (Message.HitTest = HTBOTTOM) or (Message.HitTest = HTLEFT) or
     (Message.HitTest = HTRIGHT) or (Message.HitTest = HTCAPTION) or (Message.HitTest = HTTOPLEFT) or
     (Message.HitTest = HTTOPRIGHT) or (Message.HitTest = HTBOTTOMRIGHT) or
     (Message.HitTest = HTBOTTOMLEFT) or (Message.HitTest = HTSYSMENU) then
  begin
    Exit;
  end;

  if FWasPressedButton = FHotButton then
    if Message.HitTest = HTCLOSE then
      Close
    else if (Message.HitTest = HTMAXBUTTON) and (biMaximize in THookForm(Form).BorderIcons) then
    begin
      if Form.WindowState <> wsMaximized then
        Maximize
      else
        Restore;
    end
    else if (Message.HitTest = HTMINBUTTON) and (biMinimize in THookForm(Form).BorderIcons) then
    begin
      if Form.WindowState <> wsMinimized then
        Minimize
      else
        Restore;
    end
    else if (Message.HitTest = HTHELP) and (biHelp in THookForm(Form).BorderIcons) then
      Help;

  Message.Result := 0;
  Message.Msg := WM_NULL;
  Handled := True;
end;

procedure TscFormStyleHook.WMNCLButtonDblClk(var Message: TWMNCHitMessage);
var
  P: TPoint;
begin
  inherited;

  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  // nc objects
  if (Message.HitTest = HTCAPTION) and (StyledForm <> nil) and (StyledForm.ShowButtons or FStyledForm.ShowTabs) then
  begin
    P := NormalizePoint(Point(Message.XCursor, Message.YCursor));
    FActiveNCObject := StyledForm.FindNCObjectFromPoint(P);
    if FActiveNCObject <> nil then
    begin
      if StyledForm.IsNCObjectEnabled(FActiveNCObject) then
        FActiveNCObject.MouseDown(P.X, P.Y);
      FCapturedNCObject := FActiveNCObject;
      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
      Exit;
    end;
  end;
  //

  if (Message.HitTest = HTTOP) or (Message.HitTest = HTBOTTOM) or (Message.HitTest = HTLEFT) or
     (Message.HitTest = HTRIGHT) or (Message.HitTest = HTCAPTION) or (Message.HitTest = HTTOPLEFT) or
     (Message.HitTest = HTTOPRIGHT) or (Message.HitTest = HTBOTTOMRIGHT) or (Message.HitTest = HTBOTTOMLEFT) then
  begin
    Exit;
  end;

  Message.Result := 0;
  Message.Msg := WM_NULL;
  Handled := True;
end;

procedure TscFormStyleHook.MouseEnter;
begin
  inherited;
  FPressedButton := 0;
  FCapturedNCObject := nil;
end;

procedure TscFormStyleHook.MouseLeave;
begin
  inherited;
  if (FActiveNCObject <> nil) and
     (StyledForm <> nil) and (StyledForm.FindNCObject(FActiveNCObject)) then
  begin
    if StyledForm.IsNCObjectEnabled(FActiveNCObject) then
      FActiveNCObject.MouseLeave;
    FActiveNCObject := nil;
    FOldNCObject := nil;
    FCapturedNCObject := nil;
  end;

  if FHotButton <> 0 then
  begin
    FHotButton := 0;
    FPressedButton := 0;
    if Form.BorderStyle <> bsNone then
      InvalidateNC;
  end;
  if TscMainMenuBarStyleHook(FMainMenuBarHook) <> nil then
    FMainMenuBarHook.MouseMove(-1, -1);
end;

procedure TscFormStyleHook.WMActivate(var Message: TWMActivate);
begin
  if IsStyleBorder then
  begin
    CallDefaultProc(TMessage(Message));
    FFormActive := Message.Active > 0;
    Handled := True;
  end;
end;

 procedure TscFormStyleHook.WMNCUAHDrawCaption(var Message: TMessage);
 begin
   if IsStyleBorder then
   begin
     InvalidateNC;
     Handled := True;
   end;
 end;

procedure TscFormStyleHook.Close;
begin
  if Handle <> 0 then
    SendMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

procedure TscFormStyleHook.Restore;
begin
  FPressedButton := 0;
  FHotButton := 0;

  if Handle <> 0 then
    SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TscFormStyleHook.Maximize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;

    if IsZoomed(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  end;
end;

procedure TscFormStyleHook.Minimize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;
    if IsIconic(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
   end;
end;

procedure TscFormStyleHook.Help;
begin
  SendMessage(Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0)
end;

procedure TscFormStyleHook.WMShowWindow(var Message: TWMShowWindow);
begin
  if Message.Show and FNeedsUpdate then
  begin
    FNeedsUpdate := False;
    if (Control is TForm) and (TForm(Control).FormStyle = fsMDIForm) and (FMDIClientInstance = nil) then
    begin
      FMDIPrevClientProc := Pointer(GetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC));
      FMDIClientInstance := MakeObjectInstance(MDIClientWndProc);
      SetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC, IntPtr(FMDIClientInstance));
      InitMDIScrollBars;
      AdjustMDIScrollBars;
    end;
    if IsStyleBorder and not TStyleManager.SystemStyle.Enabled and
      (GetWindowLong(Handle, GWL_STYLE) and WS_CAPTION <> 0) and
       not (THookForm(Form).FormStyle = fsMDIChild) then
    begin
      FCaptionEmulation := True;
      SetWindowLong(Handle, GWL_STYLE,
           GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION);
    end;
    UpdateForm;
  end;
end;

procedure TscFormStyleHook.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  R: TRect;
  MM: PMinMaxInfo;
begin
  if IsStyleBorder then
  begin
    CallDefaultProc(TMessage(Message));
    R := GetBorderSize;
    if R.Top + R.Bottom > Form.Constraints.MinHeight then
    begin
      MM := Message.MinMaxInfo;
      MM^.ptMinTrackSize.y := R.Top + R.Bottom;
    end;
    Handled := True;
  end;
end;


{TscStyledForm}

procedure CheckDropDownForms(AForm: TCustomForm);
var
  I: Integer;
begin
  for I := 0 to AForm.ComponentCount - 1 do
    if AForm.Components[I] is TscStyledForm then
      TscStyledForm(AForm.Components[I]).CloseUp(False);
end;

constructor TscHitTestWnd.CreateEx(AOwner: TComponent; AStyledForm: TscStyledForm);
begin
  inherited CreateNew(AOwner);
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FBorderSize := 5;
  Parent := nil;
  Visible := False;
  BorderStyle := bsNone;
  AlphaBlend := True;
  AlphaBlendValue := 0;
  FStyledForm := AStyledForm;
end;

procedure TscHitTestWnd.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin

end;

procedure TscHitTestWnd.UpdateFormPosition(var X, Y, W, H: Integer);
begin
  if (FStyledForm.FForm.Constraints.MinWidth > 0) and
     (W < FStyledForm.FForm.Constraints.MinWidth) and (X <> FStyledForm.FForm.Left) then
  begin
    X := FStyledForm.FForm.Left + FStyledForm.FForm.Width - FStyledForm.FForm.Constraints.MinWidth;
    W := FStyledForm.FForm.Constraints.MinWidth;
  end;

  if (FStyledForm.FForm.Constraints.MaxWidth > 0) and
     (W > FStyledForm.FForm.Constraints.MaxWidth) and (X <> FStyledForm.FForm.Left) then
  begin
    X := FStyledForm.FForm.Left + FStyledForm.FForm.Width - FStyledForm.FForm.Constraints.MaxWidth;
    W := FStyledForm.FForm.Constraints.MaxWidth;
  end;

  if (FStyledForm.FForm.Constraints.MinHeight > 0) and
     (H < FStyledForm.FForm.Constraints.MinHeight) and (Y <> FStyledForm.FForm.Top) then
  begin
    Y := FStyledForm.FForm.Top + FStyledForm.FForm.Height - FStyledForm.FForm.Constraints.MinHeight;
    H := FStyledForm.FForm.Constraints.MinHeight;
  end;

  if (FStyledForm.FForm.Constraints.MaxHeight > 0) and
     (H > FStyledForm.FForm.Constraints.MaxHeight) and (Y <> FStyledForm.FForm.Top) then
  begin
    Y := FStyledForm.FForm.Top + FStyledForm.FForm.Height - FStyledForm.FForm.Constraints.MaxHeight;
    H := FStyledForm.FForm.Constraints.MaxHeight;
  end;

  FStyledForm.FForm.SetBounds(X, Y, W, H);

  X := FStyledForm.FForm.Left;
  Y := FStyledForm.FForm.Top;
  W := FStyledForm.FForm.Width;
  H := FStyledForm.FForm.Height;
end;

procedure TscHitTestWnd.UpdatePosition;
begin
  if not FDragging and FStyledForm.FForm.Visible then
  begin
    SetBounds(
      FStyledForm.FForm.Left - FBorderSize,
      FStyledForm.FForm.Top - FBorderSize,
      FStyledForm.FForm.Width + FBorderSize * 2,
      FStyledForm.FForm.Height + FBorderSize * 2);

    if not Visible then
      Visible := True;

    if FStyledForm.FForm.Active then
     SetWindowPos(Handle, FStyledForm.FForm.Handle, 0, 0, 0, 0,
       SWP_NOMOVE or SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE or SWP_NOOWNERZORDER)
    else
      SetWindowPos(Handle, FStyledForm.FForm.Handle, 0, 0, 0, 0,
       SWP_NOMOVE or SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE or SWP_NOOWNERZORDER or SWP_NOZORDER);
  end;
end;

function TscHitTestWnd.GetHitTest(P: TPoint): Integer;
var
  FTopLeftRect,  FTopRightRect,
  FBottomLeftRect, FBottomRightRect,
  FTopRect, FLeftRect, FRightRect, FBottomRect: TRect;
begin
  FTopLeftRect := Rect(0, 0, FBorderSize, FBorderSize);
  FTopRightRect := Rect(Width - FBorderSize, 0, Width, FBorderSize);
  FBottomLeftRect := Rect(0, Height - FBorderSize, FBorderSize, Height);
  FBottomRightRect := Rect(Width - FBorderSize, Height - FBorderSize, Width, Height);
  FTopRect := Rect(FBorderSize, 0, Width - FBorderSize, FBorderSize);
  FLeftRect := Rect(0, FBorderSize, FBorderSize, Height - FBorderSize);
  FRightRect := Rect(Width - FBorderSize, FBorderSize, Width, Height - FBorderSize);
  FBottomRect := Rect(FBorderSize, Height - FBorderSize, Width - FBorderSize, Height);

  if FTopLeftRect.Contains(P) then
    Result := HTTOPLEFT
  else if FTopRightRect.Contains(P) then
    Result := HTTOPRIGHT
  else if FBottomLeftRect.Contains(P) then
    Result := HTBOTTOMLEFT
   else if FBottomRightRect.Contains(P) then
    Result := HTBOTTOMRIGHT
  else if FLeftRect.Contains(P) then
    Result := HTLEFT
  else if FRightRect.Contains(P) then
    Result := HTRIGHT
  else if FBottomRect.Contains(P) then
    Result := HTBOTTOM
  else if FTopRect.Contains(P) then
    Result := HTTOP
  else
   Result := HTNOWHERE;
end;

function TscHitTestWnd.NormalizePoint(P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
begin
  WindowPos := Point(Left, Top);
  ClientPos := Point(0, 0);
  ClientToScreen(ClientPos);
  Result := P;
  ScreenToClient(Result);
  Inc(Result.X, ClientPos.X - WindowPos.X);
  Inc(Result.Y, ClientPos.Y - WindowPos.Y);
end;

procedure TscHitTestWnd.WndProc(var Message: TMessage);
var
  DC: HDC;
  FCanvas: TCanvas;
  Params: PNCCalcSizeParams;
  P: TPoint;
  FHandled: Boolean;
  X, Y, W, H: Integer;
begin
  FHandled := False;
  case Message.Msg of

    WM_NCLBUTTONDOWN:
    begin
      FDragging := True;
      FStyledForm.FStopActivateEvents := True;
      FStyledForm.FHitTestWndDown := True;
    end;

    WM_TIMER:
    if TWMTimer(Message).TimerID = 1 then
    begin
      KillTimer(Handle, 1);
      UpdatePosition;
    end;

    WM_CAPTURECHANGED:
    begin
      FDragging := False;
      SetTimer(Handle, 1, 50, nil);
    end;

    WM_NCCALCSIZE:
    begin
      Params := TWMNCCALCSIZE(Message).CalcSize_Params;
      with Params^.rgrc[0] do
      begin
        Inc(Left, FBorderSize);
        Inc(Top, FBorderSize);
        Dec(Right, FBorderSize);
        Dec(Bottom, FBorderSize);
      end;
      FHandled := True;
    end;

    WM_ERASEBKGND:
    begin
      Message.Result := 1;
      FHandled := True;
    end;

    WM_NCPAINT:
    begin
      DC := GetWindowDC(Handle);
      FCanvas := TCanvas.Create;
      try
        FCanvas.Handle := DC;
        FCanvas.Brush.Color := clBlack;
        FCanvas.FillRect(Rect(0, 0, Width, Height));
      finally
        FCanvas.Handle := 0;
        FCanvas.Free;
        ReleaseDC(Handle, DC);
      end;
       FHandled := True;
    end;

    WM_NCHITTEST:
    begin
      P := NormalizePoint(Point(TWMNCHITTEST(Message).XPos, TWMNCHITTEST(Message).YPos));
      Message.Result := GetHitTest(P);
      FHandled := True;
    end;

    WM_MOUSEACTIVATE:
    begin
      Message.Result := MA_NOACTIVATE;
      if not FStyledForm.FForm.Active and FStyledForm.FForm.Visible then
        FStyledForm.FForm.Show;
      FHandled := True;
    end;

    WM_SETFOCUS:
    begin
      if not FStyledForm.FForm.Active and FStyledForm.FForm.Visible then
        FStyledForm.FForm.Show;
      Message.Result := 1;
      FHandled := True;
    end;

    WM_WINDOWPOSCHANGING:
    begin
      if (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0) and FDragging then
      begin
        X := TWMWindowPosChanging(Message).WindowPos^.x + FBorderSize;
        Y := TWMWindowPosChanging(Message).WindowPos^.y + FBorderSize;
        W := TWMWindowPosChanging(Message).WindowPos^.cx - FBorderSize * 2;
        H := TWMWindowPosChanging(Message).WindowPos^.cy - FBorderSize * 2;

        UpdateFormPosition(X, Y, W, H);

        TWMWindowPosChanging(Message).WindowPos^.x := X - FBorderSize;
        TWMWindowPosChanging(Message).WindowPos^.y := Y - FBorderSize;
        TWMWindowPosChanging(Message).WindowPos^.cx := W + FBorderSize * 2;
        TWMWindowPosChanging(Message).WindowPos^.cy := H + FBorderSize * 2;
      end;
    end;

  end;

  if not FHandled then
    inherited;

  case Message.Msg of
    WM_SIZE:
    begin
      UpdateRegion(Width, Height);
    end;
  end;
end;

procedure TscHitTestWnd.UpdateRegion;
var
  TempRgn, Rgn2: HRGN;
begin
  if W * H = 0 then Exit;
  TempRgn := FRgn;
  FRgn := CreateRectRgn(0, 0, W, H);
  Rgn2 := CreateRectRgn(FBorderSize, FBorderSize, W - FBorderSize, H - FBorderSize);
  CombineRgn(FRgn, FRgn, Rgn2, RGN_XOR);
  SetWindowRgn(Handle, FRgn, True);
  if TempRgn <> 0 then
    DeleteObject(TempRgn);
  DeleteObject(Rgn2);
end;

constructor TscStyledForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFluentUIInactiveAcrylicColorOpaque := False;
  FMenuTracking := False;
  FWasMinimized := True;
  FHitTestWndDown := False;
  FStartUp := True;
  FFluentUIBackground := scfuibNone;
  FFluentUIAcrylicColor := clBtnFace;
  FFluentUIAcrylicColorAlpha := 100;
  FFluentUIBorder := True;
  FHitTestWndShowing := False;
  FStopUpdateFluentColor := False;
  FStopActivateEvents := False;
  FWasDWMClientShadow := False;
  FStyleChanging := False;
  FDWMHitTestWnd := nil;
  FInActiveClientColor := clWindow;
  FDWMClientShadow := False;
  FDWMClientShadowHitTest := False;
  FDWMClientMaximized := False;
  FDropDownBorderColor := clBtnShadow;
  FInActiveClientBlurAmount := 5;
  FInActiveClientColorAlpha := 100;
  FStylesMenuCaption := 'Styles';
  FStylesMenuSorted := False;
  FDPIChanged := False;
  FShowed := False;
  FUpdating := False;
  FStyleCount := 0;
  FSubMenuIndex := -1;
  FClientWidth := 0;
  FClientHeight := 0;
  FMaximized := False;
  FFormLeft := 0;
  FFormTop := 0;
  FFormWidth := 0;
  FFormHeight := 0;
  FHintComponent := nil;
  FInActivePanel := nil;
  FShowHints := True;
  FDropDownControl := nil;
  FDropDownAnimation := False;
  // nc objects
  FButtonFont := TFont.Create;
  FCaptionFont := TFont.Create;
  FCaptionFont.Style := [fsBold];
  FTabFont := TFont.Create;
  FButtons := TscNCButtonItems.Create(Self);
  FTabs := TscNCTabItems.Create(Self);
  FButtonImages := nil;
  FTabImages := nil;
  FShowIcon := True;
  FTabIndex := 0;
  FTabsPosition := sctpLeft;
  FShowButtons := True;
  FShowTabs := True;
  FShowInactiveTab := True;
  FScaleFactor := 1;
  FScalePercent := 100;
  FDropDownForm := False;
  FDropDownButtonItem := nil;

  FCaptionWallpapers := nil;
  FCaptionWallpaperIndex := -1;
  FCaptionWallpaperInActiveIndex := -1;
  FCaptionWallpaperLeftMargin := 1;
  FCaptionWallpaperTopMargin := 1;
  FCaptionWallpaperRightMargin := 1;
  FCaptionWallpaperBottomMargin := 1;

  if AOwner is TCustomForm then
  begin
    FForm := TCustomForm(AOwner);
    FPPI := THookForm(FForm).PixelsPerInch;
    if not (csDesigning in ComponentState) and (StyledFormHookList <> nil) then
    begin
      if not FindStyledFormHookList(AOwner.ClassName) then
      begin
        AddStyledFormHookList(AOwner.ClassName);
        TStyleManager.Engine.RegisterStyleHook(AOwner.ClassType, TscFormStyleHook);
      end;
      if IsCustomStyle then
         FForm.Perform(CM_RECREATEWND, 0, 0);
      FOldWndProc := FForm.WindowProc;
      FForm.WindowProc := NewWndProc;
    end
  end
  else
    FForm := nil;
end;

destructor TscStyledForm.Destroy;
begin
  if not (csDesigning in ComponentState) and (FForm <> nil) then
  begin
    DeleteSystemStyleMenu;
    FForm.WindowProc := FOldWndProc;
  end;

  // nc objects
  FCaptionFont.Free;
  FButtonFont.Free;
  FTabFont.Free;
  FButtons.Free;
  FButtons := nil;
  FTabs.Free;
  FTabs := nil;

  // dwm shadow hittest wnd
  if FDWMHitTestWnd <> nil then
    FDWMHitTestWnd.Free;

  inherited;
end;

procedure TscStyledForm.ShowDWMHitTestWnd;
begin
  FStopUpdateFluentColor := True;
  if not FWasMinimized then
    FHitTestWndShowing := True;
  FWasMinimized := False;
  if FDWMHitTestWnd = nil then
    FDWMHitTestWnd := TscHitTestWnd.CreateEx(Self, Self);
  FDWMHitTestWnd.UpdatePosition;
  SetTimer(FForm.Handle, 30, 100, nil);
end;

procedure TscStyledForm.HideDWMHitTestWnd;
begin
  if FDWMHitTestWnd <> nil then
  begin
    FStopUpdateFluentColor := True;
    FWasMinimized := IsIconic(FForm.Handle);
    FHitTestWndShowing := False;
    FStopActivateEvents := True;
    FDWMHitTestWnd.Visible := False;
    FDWMHitTestWnd.AlphaBlendValue := 0;
    FStopUpdateFluentColor := False;
  end;
end;

function TscStyledForm.IsDWMClientMaximized: Boolean;
begin
  Result := FDWMClientMaximized;
end;

procedure TscStyledForm.DWMClientStartDrag;
begin
  GetCursorPos(FDWMClientDownPoint);
  FDWMClientDown := True;
end;

function TscStyledForm.IsDWMClientDragging: Boolean;
begin
  Result := FDWMClientDown;
end;

procedure TscStyledForm.DWMClientDrag;
var
  L, T, W, H: Integer;
  P: TPoint;
begin
  if not FDWMClientDown then
    Exit;

  GetCursorPos(P);

  if FDWMClientMaximized and ((P.X <> FDWMClientDownPoint.X) or (P.Y <> FDWMClientDownPoint.Y)) then
  begin
    W := FDWMClientNormalRect.Width;
    H := FDWMClientNormalRect.Height;
    L := Round(W * (P.X - FForm.Left) / FForm.Width);
    T := P.Y - FForm.Top;
    FDWMClientNormalRect.Left := P.X - L;
    FDWMClientNormalRect.Top := P.Y - T;
    FDWMClientNormalRect.Right := FDWMClientNormalRect.Left + W;
    FDWMClientNormalRect.Bottom := FDWMClientNormalRect.Top + H;
    DWMClientRestore;
  end
  else
  begin
    L := FForm.Left + (P.X - FDWMClientDownPoint.X);
    T := FForm.Top + (P.Y - FDWMClientDownPoint.Y);
    FForm.SetBounds(L, T, FForm.Width, FForm.Height);
    FDWMClientDownPoint.X := P.X;
    FDWMClientDownPoint.Y := P.Y;
  end;
end;

procedure TscStyledForm.DWMClientEndDrag;
begin
  FDWMClientDown := False;
end;

procedure TscStyledForm.DWMClientMaximize;
begin
  if not FDWMClientMaximized then
  begin
    FHitTestWndShowing := True;
    DWMClientEndDrag;
    FDWMClientNormalRect := FForm.BoundsRect;
    FForm.BoundsRect := GetMaximizeBounds;
    FDWMClientMaximized := True;
    if Assigned(OnDWMClientMaximize) then
      FOnDWMClientMaximize(Self);
    if FDWMHitTestWnd <> nil then
      HideDWMHitTestWnd;
    FHitTestWndShowing := False;
  end;
end;

procedure TscStyledForm.DWMClientRestore;
begin
  if FDWMClientMaximized then
  begin
    FHitTestWndShowing := True;
    DWMClientEndDrag;
    FDWMClientMaximized := False;
    FForm.BoundsRect := FDWMClientNormalRect;
    if Assigned(OnDWMClientRestore) then
      FOnDWMClientRestore(Self);
    FHitTestWndShowing := False;
  end;
end;

procedure TscStyledForm.ShowClientInActiveEffect;
begin
  if FInActivePanel <> nil then HideClientInActiveEffect;
  FInActivePanel := TscClientInActivePanel.Create(Self);
  FInActivePanel.ShowPanel(FForm, GetStyleColor(Self.FInActiveClientColor),
    FInActiveClientColorAlpha,
    FInActiveClientBlurAmount);
end;

function TscStyledForm.ScaleInt(AValue: Integer): Integer;
begin
  Result := Round(AValue * FScaleFactor);
end;

function TscStyledForm.ScaleDouble(AValue: Double): Double;
begin
  Result := AValue * FScaleFactor;
end;

procedure TscStyledForm.SetFluentUIBorder(Value: Boolean);
begin
  FFluentUIBorder := Value;
end;

procedure TscStyledForm.SetFluentUIBackground(Value: TscFluentUIBackground);
begin
  if FFluentUIBackground <> Value then
  begin
    FFluentUIBackground := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and (FForm <> nil) then
      FForm.Perform(CM_RECREATEWND, 0, 0);
    {$IFNDEF VER230}
    if (csDesigning in ComponentState) and (FFluentUIBackground <> scfuibNone) then
      FForm.StyleElements := [seFont];
    {$ENDIF}
  end;
end;

procedure TscStyledForm.SetFluentUIAcrylicColor(Value: TColor);
begin
  if FFluentUIAcrylicColor <> Value then
  begin
    FFluentUIAcrylicColor := Value;
    UpdateFluentAcrylicColor;
  end;
end;

procedure TscStyledForm.SetFluentUIAcrylicColorAlpha(Value: Byte);
begin
  if FFluentUIAcrylicColorAlpha <> Value then
  begin
    FFluentUIAcrylicColorAlpha := Value;
    UpdateFluentAcrylicColor;
  end;
end;

procedure TscStyledForm.SetRedraw(AValue: Boolean; AUpdate: Boolean = True);
begin
  if AValue then
  begin
    SendMessage(FForm.Handle, WM_SETREDRAW, 1, 0);
    if AUpdate then
      RedrawWindow(FForm.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or
        RDW_ALLCHILDREN or RDW_UPDATENOW);
  end
  else
    SendMessage(FForm.Handle, WM_SETREDRAW, 0, 0);
end;

function TscStyledForm.GetMaximizeBounds: TRect;

function GetTaskBarBounds : TRect;
begin
  GetWindowRect( FindWindow('Shell_TrayWnd', '' ), Result );
end;

var
  R, R1, R2: TRect;
  P: TPoint;
  M: TMonitor;
begin
  P.X := FForm.Left + FForm.Width div 2;
  P.Y := FForm.Top + FForm.Height div 2;
  M := Screen.MonitorFromPoint(P);
  R := M.WorkareaRect;
  if (M.WorkareaRect = M.BoundsRect) and (M = Screen.PrimaryMonitor) then
  begin
    R1 := GetTaskBarBounds;
    R2 := R;
    InflateRect(R2, -10, -10);
    if R1.Top > R2.Bottom then
      Dec(R.Bottom, 2)
    else
    if R1.Top < R2.Top then
      Inc(R.Top, 2)
    else
    if R1.Left < R2.Left then
      Inc(R.Left, 2)
    else
    if R1.Left > R2.Right then
      Dec(R.Right, 2);
  end;
  Result := R;
end;

procedure TscStyledForm.HideClientInActiveEffect;
begin
  if FInActivePanel <> nil
  then
    begin
      FInActivePanel.HidePanel;
      FInActivePanel.Free;
      FInActivePanel := nil;
    end;
end;

function TscStyledForm.GetBorderScaleFactor: Double;
begin
  Result := FScaleFactor;
  if not SC_SCALEFORMBORDER or not THookForm(FForm).Scaled then
    Result := 1;
end;

function TscStyledForm.IsFluentUIAvailable: Boolean;
begin
  Result := IsWindows10 and FDWMClientShadow and
   {$IFNDEF VER230} (TOSVersion.Build >= 17134) and {$ENDIF} (@SC_SetWindowCompositionAttribute <> nil);
end;

function TscStyledForm.IsFluentUIEnabled: Boolean;
begin
  Result := (FFluentUIBackground <> scfuibNone) and IsFluentUIAvailable;
end;

procedure TscStyledForm.UpdateFluentAcrylicColor;
var
  Accent: TscAccentPolicy;
  CompAttrData: TscWinCompAttrData;
begin
  if (FFluentUIBackground <>  scfuibAcrylic) or not IsFluentUIEnabled or
     (csLoading in ComponentState) then
    Exit;

  Accent.AccentState := SC_ACCENT_ENABLE_ACRYLICBLURBEHIND;
  if FFluentUIBorder then
    Accent.AccentFlags := DrawLeftBorderFlag or DrawTopBorderFlag or DrawRightBorderFlag or DrawBottomBorderFlag
  else
    Accent.AccentFlags := 0;
  if FFluentUIInactiveAcrylicColorOpaque and not FForm.Active then
    Accent.GradientColor := (255 SHL 24) or ColorToRGB(GetStyleColor(FFluentUIAcrylicColor))
  else
    Accent.GradientColor := (FFluentUIAcrylicColorAlpha SHL 24) or ColorToRGB(GetStyleColor(FFluentUIAcrylicColor));
  CompAttrData.Attribute := SC_WCA_ACCENT_POLICY;
  CompAttrData.dataSize := SizeOf(Accent);
  CompAttrData.pData := @Accent;
  SC_SetWindowCompositionAttribute(FForm.Handle, CompAttrData);
end;

procedure TscStyledForm.SetDWMFrame;
var
  Margins: TscMargins;
  Accent: TscAccentPolicy;
  CompAttrData: TscWinCompAttrData;
  FFluentBorder: Boolean;
begin
  if not SC_DwmCompositionEnabled then
  begin
    FDWMClientShadow := False;
    Exit;
  end;
  if @SC_DwmExtendFrameIntoClientArea = nil then Exit;
  FFluentBorder := False;
  SetWindowLong(FForm.Handle, GWL_STYLE,
    GetWindowLong(FForm.Handle, GWL_STYLE) or WS_CAPTION or WS_THICKFRAME or WS_MINIMIZEBOX);
  // fluent ui
  if IsFluentUIEnabled then
  begin
    FForm.Color := clBlack;
    case FFluentUIBackground of
      scfuibBlur:
        begin
          Accent.AccentState := SC_ACCENT_ENABLE_BLURBEHIND;
          if FFluentUIBorder then
          begin
            Accent.AccentFlags := DrawLeftBorderFlag or DrawTopBorderFlag or DrawRightBorderFlag or DrawBottomBorderFlag;
            FFluentBorder := True;
          end
          else
          begin
            Accent.AccentFlags := 0;
            FFluentBorder := False;
          end;
          Accent.GradientColor := 0;
        end;
      scfuibAcrylic:
        begin
          Accent.AccentState := SC_ACCENT_ENABLE_ACRYLICBLURBEHIND;
          if FFluentUIBorder then
            Accent.AccentFlags := DrawLeftBorderFlag or DrawTopBorderFlag or DrawRightBorderFlag or DrawBottomBorderFlag
          else
            Accent.AccentFlags := 0;
          Accent.GradientColor := (FFluentUIAcrylicColorAlpha SHL 24) or ColorToRGB(GetStyleColor(FFluentUIAcrylicColor));
          FFluentBorder := True;
        end;
    end;
    CompAttrData.Attribute := SC_WCA_ACCENT_POLICY;
    CompAttrData.dataSize := SizeOf(Accent);
    CompAttrData.pData := @Accent;
    SC_SetWindowCompositionAttribute(FForm.Handle, CompAttrData);
  end;
  // dwm shadow
  if not FFluentBorder or (FFluentUIBackground = scfuibNone) then
  begin
    Margins.cyTopHeight := 1;
    Margins.cxLeftWidth := 0;
    Margins.cxRightWidth := 0;
    Margins.cyBottomHeight := 0;
    SC_DwmExtendFrameIntoClientArea(FForm.Handle, Margins);
  end;
end;

procedure TscStyledForm.SetDWMClientShadowHitTest(Value: Boolean);
begin
  FDWMClientShadowHitTest := Value;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    if FDWMClientShadowHitTest then
      ShowDWMHitTestWnd
    else
      HideDWMHitTestWnd;
  end;
end;

procedure TscStyledForm.SetDWMClientShadow(Value: Boolean);
begin
  if FDWMClientShadow = Value then
    Exit;
  FDWMClientShadow := Value;
  if FDWMClientShadow then
  begin
    if FForm <> nil then
    begin
      FForm.BorderStyle := bsNone;
      {$IFNDEF VER230}
      if seBorder in FForm.StyleElements then
         FForm.StyleElements := FForm.StyleElements - [seBorder];
      {$ENDIF}
      if not (csDesigning in ComponentState) then
      begin
        if not IsWindowsXP then
          SetDwmFrame
        else
          FDWMClientShadow := False;
      end;
    end;
  end
  else
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not IsWindowsXP then
  begin
    SetWindowLong(FForm.Handle, GWL_STYLE,
      GetWindowLong(FForm.Handle, GWL_STYLE) and not WS_CAPTION and not WS_THICKFRAME and not WS_MINIMIZEBOX);
  end;
end;

procedure TscStyledForm.SetDropDownForm(Value: Boolean);
begin
  FDropDownForm := Value;
  if FDropDownForm then
  begin
    if FForm <> nil then
    begin
      FForm.BorderStyle := bsNone;
      SetClassLong(FForm.Handle, GCL_STYLE,
        GetClassLong(FForm.Handle, GCL_STYLE) or CS_DROPSHADOW);
    end;
  end;
end;

procedure TscStyledForm.BeginUpdateItems;
begin
  FUpdating := True;
end;

procedure TscStyledForm.EndUpdateItems(AUpdate: Boolean);
begin
  FUpdating := False;
  if AUpdate then UpDateFormNC;
end;

function TscStyledForm.FindNCObjectFromPoint(P: TPoint): TscNCObject;
var
  I: Integer;
begin
  Result := nil;
  if FShowButtons and (FButtons.Count > 0) then
    for I := 0 to FButtons.Count - 1 do
    begin
      if (FButtons[I].NCObject.ObjectRect.Contains(P)) and (FButtons[I].Visible) then
      begin
        Result := FButtons[I].NCObject;
        Break;
      end;
    end;
  if (Result = nil) and FShowTabs and (FTabs.Count > 0) then
    for I := 0 to FTabs.Count - 1 do
    begin
      if (FTabs[I].NCObject.ObjectRect.Contains(P)) and (FTabs[I].Visible) then
      begin
        Result := FTabs[I].NCObject;
        Break;
      end;
    end;
end;

function TscStyledForm.IsNCObjectEnabled(AObject: TscNCObject): Boolean;
begin
  Result := False;
  if AObject.Parent <> nil then
  begin
    if AObject.Parent is TscNCButtonItem then
      Result := TscNCButtonItem(AObject.Parent).Enabled
    else
   if AObject.Parent is TscNCTabItem then
      Result := TscNCTabItem(AObject.Parent).Enabled;
  end;
end;

function TscStyledForm.FindNCObject(AObject: TscNCObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FShowButtons and (FButtons.Count > 0) then
    for I := 0 to FButtons.Count - 1 do
    begin
      if (FButtons[I].NCObject = AObject) and (FButtons[I].Visible) then
      begin
        Result := True;
        Break;
      end;
    end;
  if not Result and FShowTabs and (FTabs.Count > 0) then
    for I := 0 to FTabs.Count - 1 do
    begin
      if (FTabs[I].NCObject = AObject) and (FTabs[I].Visible) then
      begin
        Result := True;
        Break;
      end;
    end;
end;

procedure TscStyledForm.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCaptionWallpapers) then
    FCaptionWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FHintComponent) then
    FHintComponent := nil;
  if (Operation = opRemove) and (AComponent = FButtonImages) then
    FButtonImages := nil;
   if (Operation = opRemove) and (AComponent = FTabImages) then
    FTabImages := nil;
  if (Operation = opRemove) and (AComponent is TPopupMenu) and (FButtons <> nil) and
     (FButtons.Count > 0) then
  begin
    for I := 0 to FButtons.Count - 1 do
      if (FButtons[I].FPopupMenu <> nil) and (FButtons[I].FPopupMenu = AComponent) then
        FButtons[I].FPopupMenu := nil;
  end
end;

procedure TscStyledForm.RestoreClientSize;
begin
  if ((THookForm(FForm).BorderStyle = bsSingle) or
     (THookForm(FForm).BorderStyle = bsDialog) or
     (THookForm(FForm).BorderStyle = bsToolWindow)) and
     ((FClientWidth > 0) and (FClientHeight > 0))
  then
  begin
    if FClientWidth > 0 then
     begin
       THookForm(FForm).ClientWidth := Round(FClientWidth * FScaleFactor) - 1;
       THookForm(FForm).ClientWidth := Round(FClientWidth * FScaleFactor);
     end;
     if FClientHeight > 0 then
     begin
       THookForm(FForm).ClientHeight := Round(FClientHeight * FScaleFactor) - 1;
       THookForm(FForm).ClientHeight := Round(FClientHeight * FScaleFactor) + 1;
     end;
  end;
end;

procedure TscStyledForm.UpdateFluentWindow;
var
  LMargins: TscMargins;
begin
  LMargins.cxLeftWidth := 0;
  LMargins.cxRightWidth := 0;
  LMargins.cyTopHeight := 0;
  LMargins.cyBottomHeight := 0;
  SC_DwmExtendFrameIntoClientArea(FForm.Handle, LMargins);
end;

procedure TscStyledForm.NewWndProc(var Message: TMessage);
var
  FCmdType: NativeUInt;
  StyleIndex: Integer;
  FHandled: Boolean;
  FCanvas: TCanvas;
  DC: HDC;
  C: TColor;
  R: TRect;
  OldScaleFactor: Double;
  FScreenPPI: Integer;
  B: BOOL;
begin
  FHandled := False;
  if (Message.Msg = WM_SIZE) and FDWMClientShadow and IsIconic(FForm.Handle) then
    Exit;
  case Message.Msg of
    WM_CREATE:
      begin
        FMenuTracking := False;
        FUpdating := False;
      end;
    WM_DPICHANGED:
    begin
      if Assigned(FOnBeforeChangeScale) then
        FOnBeforeChangeScale(Self);
    end;
    WM_DWMNCRENDERINGCHANGED:
      begin
        if FDWMClientShadow then
          FHandled := True;
      end;
    WM_NCPAINT:
    if FDropDownForm and (FForm.BorderStyle = bsNone) then
    begin
      FHandled := True;
      DC := GetWindowDC(FForm.Handle);
      FCanvas := TCanvas.Create;
      try
        FCanvas.Handle := DC;
        C := GetStyleColor(FDropDownBorderColor);
        R := Rect(0, 0, FForm.Width, FForm.Height);
        Frm3D(FCanvas, R, C, C);
      finally
        FCanvas.Handle := 0;
        FCanvas.Free;
        ReleaseDC(FForm.Handle, DC);
      end;
    end;
    WM_NCCALCSIZE:
    begin
      if FDWMClientShadow then
      begin
        Message.Result := 0;
        FHandled := True;
      end
      else
      if FDropDownForm and (FForm.BorderStyle = bsNone) then
      begin
        FHandled := True;
        with TWMNCCALCSIZE(Message).CalcSize_Params^.rgrc[0] do
        begin
          Inc(Left);
          Inc(Top);
          Dec(Right);
          Dec(Bottom);
        end;
      end;
    end;
    WM_ACTIVATE:
    begin
      if FStopActivateEvents and (Message.WParam < 1) and FForm.Visible then
      begin
        Message.WParam := 1;
        FStopActivateEvents := False;
        FHandled := True;
      end
      else
        FStopActivateEvents := False;
      if (FForm = FParentDropDownForm) then
      begin
        Message.WParam := 1;
      end;
    end;
    WM_NCACTIVATE:
    begin
      if FStopActivateEvents and (Message.WParam < 1) and FForm.Visible then
      begin
        Message.WParam := 1;
        FStopActivateEvents := False;
        if FHitTestWndDown then
        begin
          FHandled := True;
          FHitTestWndDown := False;
        end;
      end
      else
        FStopActivateEvents := False;
      if (FForm = FParentDropDownForm) then
      begin
        Message.WParam := 1;
      end;
      if FDropDownForm and (Message.WParam < 1) and FForm.Visible then
      begin
        CloseUp(False);
      end;
    end;
    WM_SHOWWINDOW:
    begin
      FStartUp := False;

      if Message.WParam > 0 then
      begin
        if THookForm(FForm).Scaled and
           (THookForm(FForm).PixelsPerInch <> FPPI)
        then
        begin
          FScalePercent := MulDiv(FScalePercent,
            THookForm(FForm).PixelsPerInch, FPPI);
          FScaleFactor := FScalePercent / 100;
          if FScaleFactor < 1 then FScaleFactor := 1;
          if Assigned(FOnChangeScale) then
            FOnChangeScale(FScaleFactor);
          FPPI := THookForm(FForm).PixelsPerInch;
          {$IFDEF VER300_UP}
          if IsCustomStyle and SC_SCALEFORMBORDER and ((FClientWidth > 0) or (FClientHeight > 0)) then
          begin
            SetWindowPos(FForm.Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
              SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
            if FClientWidth > 0 then
              FForm.ClientWidth := Round(FClientWidth * FScaleFactor);
            if FClientHeight > 0 then
              FForm.ClientHeight := Round(FClientHeight * FScaleFactor);
          end;
          {$ENDIF}
        end
        else
        begin
          {$IFDEF VER300_UP}
          FScreenPPI := Screen.MonitorFromWindow(FForm.Handle).PixelsPerInch;
          {$ELSE}
          FScreenPPI := Screen.PixelsPerInch;
          {$ENDIF}
           if THookForm(FForm).Scaled and
            (THookForm(FForm).PixelsPerInch <> FScreenPPI) then
          begin
            OldScaleFactor := FScaleFactor;
            FScaleFactor :=  FScreenPPI / THookForm(FForm).PixelsPerInch;
            if FScaleFactor < 1 then FScaleFactor := 1;
            FScalePercent := Round(FScaleFactor * 100);
            if OldScaleFactor <> FScaleFactor then
            begin
              if Assigned(FOnChangeScale) then
                FOnChangeScale(FScaleFactor);
            end;
            FPPI := THookForm(FForm).PixelsPerInch;
            {$IFDEF VER300_UP}
            if IsCustomStyle and SC_SCALEFORMBORDER and ((FClientWidth > 0) or (FClientHeight > 0)) then
            begin
              SetWindowPos(FForm.Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
                SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
              if FClientWidth > 0 then
                 FForm.ClientWidth := Round(FClientWidth * FScaleFactor);
              if FClientHeight > 0 then
                FForm.ClientHeight := Round(FClientHeight * FScaleFactor);
            end;
            {$ENDIF}
          end
          else
          if THookForm(FForm).Scaled and
            (FScreenPPI <> 96) then
          begin
            OldScaleFactor := FScaleFactor;
            FScaleFactor :=  FScreenPPI / 96;
            if FScaleFactor < 1 then FScaleFactor := 1;
            FScalePercent := Round(FScaleFactor * 100);
            if OldScaleFactor <> FScaleFactor then
            begin
              if Assigned(FOnChangeScale) then
                FOnChangeScale(FScaleFactor);
            end;
            FPPI := THookForm(FForm).PixelsPerInch;
            {$IFDEF VER300_UP}
            if IsCustomStyle and SC_SCALEFORMBORDER and ((FClientWidth > 0) or (FClientHeight > 0)) then
            begin
              SetWindowPos(FForm.Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
                SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
              if FClientWidth > 0 then
                 FForm.ClientWidth := Round(FClientWidth * FScaleFactor);
              if FClientHeight > 0 then
                FForm.ClientHeight := Round(FClientHeight * FScaleFactor);
            end;
            {$ENDIF}
          end;
        end;
      end;
    end;
    WM_NCLBUTTONDOWN:
      begin
        FNCDrag := True;
      end;
    WM_NCLBUTTONUP, WM_NCLBUTTONDBLCLK:
      begin
        FNCDrag := False;
      end;
    WM_SYSCOMMAND:
    begin
      if IsCustomStyle and (Message.WParam = SC_CLOSE) and
         (((THookForm(FForm).FormStyle = fsMDIChild)and
          not IsZoomed(FForm.Handle)) or
         (THookForm(FForm).FormStyle = fsMDIForm))
      then
      begin
        if (THookForm(FForm).FormStyle = fsMDIForm) then
        begin
          if THookForm(FForm).CloseQuery then
            ShowWindow(FForm.Handle, SW_HIDE)
          else
            FHandled := True;
        end
        else
          ShowWindow(FForm.Handle, SW_HIDE);
      end
      else
      if FDropDownForm and (Message.WParam = SC_CLOSE) then
      begin
        FHandled := True;
      end
      else
      if (Message.WParam = SC_MINIMIZE) then
      begin
        if not FSizeStored and not FMaximized then
        begin
          FFormLeft := FForm.Left;
          FFormTop := FForm.Top;
          FFormWidth := FForm.Width;
          FFormHeight := FForm.Height;
          FSizeStored := True;
        end;
      end
      else
      if (Message.WParam = SC_MAXIMIZE) and not FSizeStored and
         (FForm.WindowState <> wsMaximized) then
      begin
        FFormLeft := FForm.Left;
        FFormTop := FForm.Top;
        FFormWidth := FForm.Width;
        FFormHeight := FForm.Height;
        FSizeStored := True;
      end;
    end;
    WM_WINDOWPOSCHANGING:
    begin
      if FDWMClientMaximized then
      begin
        TWMWindowPosChanging(Message).WindowPos^.x := FForm.Left;
        TWMWindowPosChanging(Message).WindowPos^.y := FForm.Top;
        TWMWindowPosChanging(Message).WindowPos^.cx := FForm.Width;
        TWMWindowPosChanging(Message).WindowPos^.cy := FForm.Height;
      end;
    end;

    WM_WINDOWPOSCHANGED:
    begin
      if (FForm.WindowState <> wsNormal) and FStartUp then
      begin
        FStartUp := False;
         if THookForm(FForm).Scaled and
           (THookForm(FForm).PixelsPerInch <> FPPI)
        then
        begin
          FScalePercent := MulDiv(FScalePercent,
            THookForm(FForm).PixelsPerInch, FPPI);
          FScaleFactor := FScalePercent / 100;
          if FScaleFactor < 1 then FScaleFactor := 1;
          if Assigned(FOnChangeScale) then
            FOnChangeScale(FScaleFactor);
          FPPI := THookForm(FForm).PixelsPerInch;
          {$IFDEF VER300_UP}
          if (FForm.WindowState = wsMinimized) or IsCustomStyle and
            SC_SCALEFORMBORDER and ((FClientWidth > 0) or (FClientHeight > 0)) then
          begin
            if FClientWidth > 0 then
              FForm.ClientWidth := Round(FClientWidth * FScaleFactor);
            if FClientHeight > 0 then
              FForm.ClientHeight := Round(FClientHeight * FScaleFactor);
          end;
          {$ENDIF}
        end
        else
        begin
          {$IFDEF VER300_UP}
          FScreenPPI := Screen.MonitorFromWindow(FForm.Handle).PixelsPerInch;
          {$ELSE}
          FScreenPPI := Screen.PixelsPerInch;
          {$ENDIF}
           if THookForm(FForm).Scaled and
            (THookForm(FForm).PixelsPerInch <> FScreenPPI) then
          begin
            OldScaleFactor := FScaleFactor;
            FScaleFactor :=  FScreenPPI / THookForm(FForm).PixelsPerInch;
            if FScaleFactor < 1 then FScaleFactor := 1;
            FScalePercent := Round(FScaleFactor * 100);
            if OldScaleFactor <> FScaleFactor then
            begin
              if Assigned(FOnChangeScale) then
                FOnChangeScale(FScaleFactor);
            end;
            FPPI := THookForm(FForm).PixelsPerInch;
            {$IFDEF VER300_UP}
            if (FForm.WindowState = wsMinimized) or IsCustomStyle and
              SC_SCALEFORMBORDER and ((FClientWidth > 0) or (FClientHeight > 0)) then
            begin
              if FClientWidth > 0 then
                FForm.ClientWidth := Round(FClientWidth * FScaleFactor);
              if FClientHeight > 0 then
                FForm.ClientHeight := Round(FClientHeight * FScaleFactor);
            end;
            {$ENDIF}
          end
          else
          if THookForm(FForm).Scaled and
            (FScreenPPI <> 96) then
          begin
            OldScaleFactor := FScaleFactor;
            FScaleFactor :=  FScreenPPI / 96;
            if FScaleFactor < 1 then FScaleFactor := 1;
            FScalePercent := Round(FScaleFactor * 100);
            if OldScaleFactor <> FScaleFactor then
            begin
              if Assigned(FOnChangeScale) then
                FOnChangeScale(FScaleFactor);
            end;
            FPPI := THookForm(FForm).PixelsPerInch;
            {$IFDEF VER300_UP}
            if (FForm.WindowState = wsMinimized) or IsCustomStyle and
              SC_SCALEFORMBORDER and ((FClientWidth > 0) or (FClientHeight > 0)) then
            begin
              if FClientWidth > 0 then
                FForm.ClientWidth := Round(FClientWidth * FScaleFactor);
              if FClientHeight > 0 then
                FForm.ClientHeight := Round(FClientHeight * FScaleFactor);
            end;
            {$ENDIF}
          end;
        end;
      end;

      if THookForm(FForm).FormStyle <> fsMDIChild then
      begin
        if (FForm.WindowState = wsMaximized) and not FMaximized and not FSizeStored then
        begin
          FMaximized := True;
          {FFormLeft := FForm.Left;
          FFormTop := FForm.Top;
          FFormWidth := FForm.Width;
          FFormHeight := FForm.Height;
          FSizeStored := True; }
        end
        else
        if (FForm.WindowState = wsNormal) and FSizeStored
           and FMaximized and FStyleChanged and not FMinimized then
        begin
          FStyleChanged := False;
          FSizeStored := False;
          FMaximized := False;
          PostMessage(FForm.Handle, WM_RESTOREPOSITION, 0, 0);
        end;
      end
      else
      if (FForm.WindowState = wsMaximized) and not FMaximized and not FSizeStored then
      begin
        FMaximized := True;
        FFormLeft := FForm.Left;
        FFormTop := FForm.Top;
        FFormWidth := FForm.Width;
        FFormHeight := FForm.Height;
        FSizeStored := True;
      end;
    end;
  end;

  case Message.Msg of
    CM_RECREATEWND:
      begin
        FStyleChanging := True;
        if FShowStylesMenu then
        begin
          DeleteSystemStyleMenu;
          FOldWndProc(Message);
          CreateSystemStyleMenu;
          FHandled := True;
        end;
      end;
    WM_SYSCOMMAND:
      if FShowStylesMenu and
         (TWMSysCommand(Message).CmdType >= NativeUInt(SystemStylesMenu)) and
         (TWMSysCommand(Message).CmdType <= NativeUInt(SystemStylesMenu + FStyleCount)) then
      begin
        FCmdType := TWMSysCommand(Message).CmdType;
        StyleIndex := Integer(FCmdType - SystemStylesMenu) - 1;
        if StyleIndex < 0 then StyleIndex := 0;
        if StyleIndex > FStyleCount then StyleIndex := FStyleCount - 1;
        SetStyleByIndex(StyleIndex);
        FHandled := True;
      end;
   end;

  if not FHandled then
     FOldWndProc(Message);

  // restore client size
  case Message.Msg of

    CM_RECREATEWND:
    begin
      FStyleChanging := False;
      FMenuTracking := False;
      FUpdating := False;
    end;

    WM_RESTOREPOSITION:
    begin
      if FNCDrag and (THookForm(FForm).FormStyle <> fsMDIChild) then
      begin
        if (THookForm(FForm).BorderStyle = bsSizeAble) and (FFormWidth > 0) and
         (FFormHeight > 0) then
        begin
          FFormWidth := 0;
          FFormHeight := 0;
        end
        else
        begin
          FFormWidth := 0;
          FFormHeight := 0;
          if (FClientWidth > 0) or (FClientHeight > 0) then
            PostMessage(FForm.Handle, WM_RESTORECLIENTSIZE, 0, 0);
        end;
      end
      else
      begin
        if (THookForm(FForm).FormStyle = fsMDIChild) then
        begin
          if FFormLeft < 0 then FFormLeft := 0;
          if FFormLeft < 0 then FFormTop := 0;
        end;

        if (THookForm(FForm).BorderStyle = bsSizeAble) and (FFormWidth > 0) and
           (FFormHeight > 0) then
        begin
          if THookForm(FForm).WindowState = wsNormal then
            FForm.SetBounds(FFormLeft, FFormTop, FFormWidth, FFormHeight);
        end
        else
          begin
            FForm.Left := FFormLeft;
            FForm.Top := FFormTop;
            FFormWidth := 0;
            FFormHeight := 0;
            if (FClientWidth > 0) or (FClientHeight > 0) then
              PostMessage(FForm.Handle, WM_RESTORECLIENTSIZE, 0, 0);
          end;
      end;
    end;

    WM_RESTORECLIENTSIZE:
    begin
      RestoreClientSize;
      FSizeStored := False;
    end;

    WM_DPICHANGED:
    begin
      FStartUp := False;
      if THookForm(FForm).Scaled and
         (THookForm(FForm).PixelsPerInch <> FPPI) then
      begin
        FScalePercent := MulDiv(FScalePercent,
          THookForm(FForm).PixelsPerInch, FPPI);
        FScaleFactor := FScalePercent / 100;
        if FScaleFactor < 1 then FScaleFactor := 1;
        FPPI := THookForm(FForm).PixelsPerInch;
      end
      else
      begin
        {$IFDEF VER300_UP}
        FScreenPPI := Screen.MonitorFromWindow(FForm.Handle).PixelsPerInch;
        {$ELSE}
        FScreenPPI := Screen.PixelsPerInch;
        {$ENDIF}
        if THookForm(FForm).Scaled then
        begin
          OldScaleFactor := FScaleFactor;
          if FScreenPPI = THookForm(FForm).PixelsPerInch then
            FScaleFactor := FScreenPPI / 96
          else
            FScaleFactor :=  FScreenPPI / THookForm(FForm).PixelsPerInch;
          if FScaleFactor < 1 then FScaleFactor := 1;
          FScalePercent := Round(FScaleFactor * 100);
          if OldScaleFactor <> FScaleFactor then
          begin
            if Assigned(FOnChangeScale) then
              FOnChangeScale(FScaleFactor);
          end;
          FPPI := THookForm(FForm).PixelsPerInch;
        end;
      end;
      FDPIChanged := True;
      if Assigned(FOnChangeScale) then
        FOnChangeScale(FScaleFactor);
      if IsCustomStyle and SC_SCALEFORMBORDER and ((FClientWidth > 0) or (FClientHeight > 0)) then
      begin
        SetWindowPos(FForm.Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
          SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
        if FClientWidth > 0 then
           FForm.ClientWidth := Round(FClientWidth * FScaleFactor);
        if FClientHeight > 0 then
          FForm.ClientHeight := Round(FClientHeight * FScaleFactor);
      end;
    end;

    WM_SYSCOMMAND:
    begin
      case Message.WParam of
        SC_CLOSE:
        begin
          if IsCustomStyle and (THookForm(FForm).FormStyle = fsMDIChild) then
          begin
            if IsIconic(FForm.Handle) and
               not (Application.MainForm.ActiveMDIChild = FForm)
            then
              SendMessage(FForm.Handle, WM_NCACTIVATE, 0, 0);
          end;
        end;
        SC_MINIMIZE:
        begin
          FMinimized := True;
        end;
        SC_MAXIMIZE:
        begin
          FMaximized := True;
        end;
        SC_RESTORE:
        begin
          FMaximized := False;
          if (THookForm(FForm).WindowState = wsNormal) and FSizeStored and FStyleChanged then
          begin
            FMaximized := False;
            FMinimized := False;
            FStyleChanged := False;
            FSizeStored := False;
            SendMessage(FForm.Handle, WM_RESTOREPOSITION, 0, 0);
          end
          else
          if THookForm(FForm).WindowState = wsNormal then
            FSizeStored := False;
        end;
     end;
    end;

    WM_POSTSTYLECHANGED:
    begin
      if Assigned(FOnStyleChanged) then
        FOnStyleChanged(Self);
    end;

    WM_TIMER:
    if (TWMTimer(Message).TimerID = 22) and FMenuTracking then
    begin
      KillTimer(FForm.Handle, 22);
      FMenuTracking := False;
      FUpdating := False;
      UpDateFormNC;
    end
    else
    if TWMTimer(Message).TimerID = 20 then
    begin
      KillTimer(FForm.Handle, 20);
      SendMessage(FForm.Handle, WM_NCPAINT, 0, 0);
    end
    else
    if TWMTimer(Message).TimerID = 30 then
    begin
      KillTimer(FForm.Handle, 30);
      if (FDWMHitTestWnd <> nil) and FDWMHitTestWnd.Visible then
      begin
        FDWMHitTestWnd.AlphaBlendValue := 1;
        if SC_DwmCompositionEnabled and (IsWindows7 or IsWindows8 or IsWindows10) then
        begin
          B := True;
          SC_DwmSetWindowAttribute(FDWMHitTestWnd.Handle, DWMWA_EXCLUDED_FROM_PEEK, @B, SizeOf(B));
        end;
      end;
      FHitTestWndShowing := False;
      FStopUpdateFluentColor := False;
    end;

    WM_STYLECHANGED:
    begin
      FStyleChanged := True;
      if not FMaximized and not FMinimized then
      begin
        FStyleChanged := False;
        {$IFNDEF VER300_UP}
//         if (FClientWidth > 0) and (FClientHeight > 0) and IsWindowVisible(FForm.Handle) then
        {$ENDIF}
        RestoreClientSize;
      end;
      if (FForm = Application.MainForm) and (THookForm(FForm).FormStyle = fsMDIForm)
      then
        PostMessage(FForm.Handle,  WM_CHECKMDICHILDS, 0, 0);
      PostMessage(FForm.Handle,  WM_POSTSTYLECHANGED, 0, 0);
      if IsCustomStyle and IsZoomed(FForm.Handle) and FForm.Visible and
         (FForm.BorderStyle <> bsNone) and (THookForm(FForm).FormStyle <> fsMDIChild)
      then
        SetTimer(FForm.Handle, 20, 100, nil);

      if FDWMClientShadow and not IsWindowsXP then
        SetDWMFrame;

      if (FDWMHitTestWnd <> nil) and FDWMHitTestWnd.Visible and SC_DwmCompositionEnabled and
         (IsWindows7 or IsWindows8 or IsWindows10) then
      begin
        B := True;
        SC_DwmSetWindowAttribute(FDWMHitTestWnd.Handle, DWMWA_EXCLUDED_FROM_PEEK, @B, SizeOf(B));
      end;
    end;

    WM_CHECKMDICHILDS:
     begin
       CheckMDIChilds;
     end;

     WM_SETTINGCHANGE:
       if not IsWindowsXP and FDWMClientShadow and not SC_DwmCompositionEnabled then
       begin
         FWasDWMClientShadow := True;
         FDWMClientShadow := False;
         SetWindowLong(FForm.Handle, GWL_STYLE,
           GetWindowLong(FForm.Handle, GWL_STYLE) and not WS_CAPTION and not WS_THICKFRAME and not WS_MINIMIZEBOX);
         if FDWMHitTestWnd <> nil then
           HideDWMHitTestWnd;
       end
       else
       if not IsWindowsXP and FWasDWMClientShadow and SC_DwmCompositionEnabled then
       begin
         FWasDWMClientShadow := False;
         FDWMClientShadow := True;
         SetDWMFrame;
         if FDWMClientShadowHitTest and FForm.Visible then
           ShowDWMHitTestWnd;
       end;

    WM_ACTIVATE:
    begin
      if IsFluentUIEnabled and (FFluentUIBackground = scfuibAcrylic) and not FStopActivateEvents and not FHitTestWndShowing and
         not (csDestroying in FForm.ComponentState) and FFluentUIInactiveAcrylicColorOpaque and not FStopUpdateFluentColor then
           UpdateFluentAcrylicColor;

      if Assigned(FOnChangeActive) and not FStopActivateEvents and not FStyleChanging and not FHitTestWndShowing and
         not (csDestroying in FForm.ComponentState) then
        FOnChangeActive(Self);

      FStopActivateEvents := False;
      if FDWMClientShadowHitTest and FDWMClientShadow and (FDWMHitTestWnd <> nil) and
         FDWMHitTestWnd.Visible
      then
        FDWMHitTestWnd.UpdatePosition;
    end;

    WM_SIZE:
    begin
      if FDropDownForm then
      begin
        SendMessage(FForm.Handle, WM_NCPAINT, 0, 0);
        FForm.RePaint;
      end;
      if IsFluentUIEnabled and (FFluentUIBackground = scfuibAcrylic)
        {$IFNDEF VER230}  and (TOSVersion.Build >= 18362) {$ENDIF}
      then
        UpdateFluentWindow;
    end;

    WM_WINDOWPOSCHANGED:
    begin
      if FDWMClientShadowHitTest and FDWMClientShadow then
      begin
       if (WinAPi.Windows.IsIconic(FForm.Handle) or not FForm.Visible) or FDWMClientMaximized
        then
        begin
          if FDWMHitTestWnd <> nil then
            HideDWMHitTestWnd
        end
        else
        if (FDWMHitTestWnd = nil) or ((FDWMHitTestWnd <> nil) and not FDWMHitTestWnd.Visible) and FForm.Visible then
       begin
          ShowDWMHitTestWnd;
        end;

        if (FDWMHitTestWnd <> nil) and FDWMHitTestWnd.Visible then
          FDWMHitTestWnd.UpdatePosition;
      end;
    end;

    WM_MOVE:
    begin
      if IsFluentUIEnabled and (FFluentUIBackground = scfuibAcrylic)
        {$IFNDEF VER230}  and (TOSVersion.Build >= 18362) {$ENDIF}
      then
        UpdateFluentWindow;
    end;
  end;
end;

procedure TscStyledForm.CheckMDIChilds;
var
  I, Offset: Integer;
  R, R1: TRect;
  F: TForm;
  P: TPoint;
begin
  for I := 0 to THookForm(FForm).MDIChildCount - 1 do
  begin
    F := THookForm(FForm).MDIChildren[I];
    if F.Visible and (F.WindowState = wsMinimized) then
    begin
      GetWindowRect(F.Handle, R);
      GetWindowRect(THookForm(FForm).ClientHandle, R1);
      P := Point(0, 0);
      WinApi.Windows.ClientToScreen(THookForm(FForm).ClientHandle, P);
      OffsetRect(R, -P.X, -P.Y);
      if R.Bottom > R1.Height then
      begin
        Offset := R.Bottom - R1.Height;
        SetWindowPos(F.Handle, HWND_TOPMOST, R.Left, R.Top - Offset, 0, 0,
          SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
      end;
    end;
  end;
  if THookForm(FForm).WindowState = wsMaximized then
    SetWindowPos(THookForm(FForm).ClientHandle, 0, 0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_DRAWFRAME or
      SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);
end;

procedure TscStyledForm.SetClientWidth(Value: Integer);
begin
  if (Value <> FClientWidth) and (Value >= 0) then
  begin
    FClientWidth := Value;
    if FClientWidth > 0 then
      THookForm(FForm).ClientWidth := FClientWidth;
  end;
end;

procedure TscStyledForm.SetClientHeight(Value: Integer);
begin
  if (Value <> FClientWidth) and (Value >= 0) then
  begin
    FClientHeight := Value;
    if FClientHeight > 0 then
      THookForm(FForm).ClientHeight := FClientHeight;
  end;
end;

procedure TscStyledForm.SetStyleByIndex(AIndex: Integer);
var
  I: Integer;
  SName: String;
begin
  I := 0;
  SName := '';
  for SName in TStyleManager.StyleNames do
    if AIndex = I then Break else Inc(I);
  if SName <> '' then
    TStyleManager.SetStyle(SName);
end;

procedure TscStyledForm.Loaded;
begin
  inherited;
  if FShowStylesMenu then
     CreateSystemStyleMenu;
end;

procedure TscStyledForm.DeleteSystemStyleMenu;
begin
  if IsMenu(FStylesMenu) then
    while GetMenuItemCount(FStylesMenu) > 0 do
      DeleteMenu(FStylesMenu, 0, MF_BYPOSITION);
  FStyleCount := 0;    
end;
    
procedure TscStyledForm.SetShowStylesMenu(Value: Boolean);
var
  FSystemMenu: HMenu;
begin
  if FShowStylesMenu <> Value then
  begin
    FShowStylesMenu := Value;
    if FShowStylesMenu and not (csDesigning in ComponentState) and not
       (csLoading in ComponentState) and (FSubMenuIndex = -1) then
      CreateSystemStyleMenu
    else
    if not FShowStylesMenu and not (csDesigning in ComponentState) and not
       (csLoading in ComponentState) and (FSubMenuIndex <> -1) then
    begin   
      DeleteSystemStyleMenu;
      FSystemMenu := GetSystemMenu(FForm.Handle, False);
      DeleteMenu(FSystemMenu, FSubMenuIndex, MF_BYPOSITION);
      DeleteMenu(FSystemMenu, FSubMenuIndex, MF_BYPOSITION);
      FSubMenuIndex := -1;
    end;
  end;
end;

procedure TscStyledForm.SetStylesMenuCaption(Value: String);
var
  FMenuInfo: TMenuItemInfo;
  FSystemMenu: HMenu;
begin
  if FStylesMenuCaption <> Value then
  begin
    FStylesMenuCaption := Value;
    if FShowStylesMenu and not (csDesigning in ComponentState) and not
       (csLoading in ComponentState) and (FSubMenuIndex <> -1) then
    begin   
      FillChar(FMenuInfo, SizeOf(FMenuInfo), 0);
      FMenuInfo.cbSize := SizeOf(TMenuItemInfo);
      FMenuInfo.fMask := MIIM_SUBMENU or MIIM_FTYPE or  MIIM_ID or MIIM_BITMAP or MIIM_STRING;
      FMenuInfo.fType := MFT_STRING;
      FMenuInfo.wID := SystemStylesMenu;
      FMenuInfo.hSubMenu := FStylesMenu;
      FMenuInfo.dwTypeData := PWideChar(FStylesMenuCaption);
      FMenuInfo.cch := Length(FStylesMenuCaption);
      FSystemMenu := GetSystemMenu(FForm.Handle, False);
      SetMenuItemInfo(FSystemMenu, FSubMenuIndex, True, FMenuInfo);
    end;
  end;
end;

procedure TscStyledForm.CreateSystemStyleMenu;
var
  FSystemMenu: HMenu;
  SName: String;
  uIDNewItem: Integer;
  FItemCount: Integer;
  FMenuInfo: TMenuItemInfo;
  S, S2: TStringList;
  I, J: Integer;
begin
  FSystemMenu := GetSystemMenu(FForm.Handle, False);
  FItemCount := GetMenuItemCount(FSystemMenu);
  if FItemCount > 2 then
    FSubMenuIndex := FItemCount - 2
  else
    FSubMenuIndex := FItemCount;

  InsertMenu(FSystemMenu, FSubMenuIndex, MF_BYPOSITION or MF_SEPARATOR, 0, nil);
  Inc(FSubMenuIndex);

  FStylesMenu := CreatePopupMenu;

  SName := FStylesMenuCaption;

  uIDNewItem := SystemStylesMenu;

  FillChar(FMenuInfo, SizeOf(FMenuInfo), 0);
  FMenuInfo.cbSize := SizeOf(TMenuItemInfo);
  FMenuInfo.fMask := MIIM_SUBMENU or MIIM_FTYPE or MIIM_ID or MIIM_BITMAP or MIIM_STRING;
  FMenuInfo.fType := MFT_STRING;
  FMenuInfo.wID  := SystemStylesMenu;
  FMenuInfo.hSubMenu := FStylesMenu;
  FMenuInfo.dwTypeData := PWideChar(SName);
  FMenuInfo.cch := Length(SName);

  if FSubMenuIndex <> FItemCount then
    InsertMenuItem(FSystemMenu, GetMenuItemCount(FSystemMenu) - 2, True, FMenuInfo)
  else
    InsertMenuItem(FSystemMenu, GetMenuItemCount(FSystemMenu), True, FMenuInfo);

  Inc(uIDNewItem);
  FStyleCount := 0;
  J := uIDNewItem;
  if FStylesMenuSorted then
  begin
    S := TStringList.Create;
    S2 := TStringList.Create;
    for SName in TStyleManager.StyleNames do
    begin
      S.Add(SName);
      S2.Add(SName);
    end;
    S.Sort;
    if S.Count > 0 then
      for I := 0 to S.Count - 1 do
      begin
        SName := S[I];
        uIDNewItem := J + S2.IndexOf(SName);
        InsertStyleMenuItem(FStylesMenu, FStyleCount, uIDNewItem, PChar(SName), nil);
        if SameText(TStyleManager.ActiveStyle.Name, SName) then
          CheckMenuItem(FStylesMenu, FStyleCount, MF_BYPOSITION or MF_CHECKED);
        Inc(FStyleCount);
      end;
    S.Free;
    S2.Free;
  end
  else
  begin
    for SName in TStyleManager.StyleNames do
    begin
      InsertStyleMenuItem(FStylesMenu, FStyleCount, uIDNewItem, PChar(SName), nil);
      if SameText(TStyleManager.ActiveStyle.Name, SName) then
       CheckMenuItem(FStylesMenu, FStyleCount, MF_BYPOSITION or MF_CHECKED);
      Inc(uIDNewItem);
      Inc(FStyleCount);
    end;
  end;
end;

procedure TscStyledForm.UpDateFormNC;
begin
  if IsCustomStyle and not FUpdating then
    SendMessage(FForm.Handle, WM_NCPAINT, 0, 0);
end;

procedure TscStyledForm.DropDown(ADropDownForm: TCustomForm; ANCButton: TscNCButtonItem; X, Y: Integer; AIsRightToLeft: Boolean = False);
var
  WorkArea: TRect;
  F: TCustomForm;
  Button: TscNCButtonItem;
  I, AValue: Integer;
  B: Boolean;
  TickCount: Cardinal;
  AnimationStep: Integer;
  OldScaleFactor: Double;
  FScreenPPI: Integer;
begin
  if FActiveDropDownForm <> nil then
  begin
    F := FActiveDropDownForm;
    Button := FDropDownButtonItem;
    CheckDropDownForms(FActiveDropDownForm);
    if (F = FForm) and (Button = ANCButton) then Exit;
  end;
  FParentDropDownForm := ADropDownForm;
  FDropDownButtonItem := ANCButton;
  FActiveDropDownForm := FForm;
  FDropDownControl := nil;
  WorkArea := Screen.MonitorFromWindow(ADropDownForm.Handle).WorkAreaRect;

  if AIsRightToLeft then
  begin
    X := X + ANCButton.NCObject.ObjectRect.Width - FForm.Width;
  end;

  if X + FForm.Width > WorkArea.Right then
    X := WorkArea.Right - FForm.Width;
 if Y + FForm.Height > WorkArea.Bottom then
    Y := WorkArea.Bottom - FForm.Height;
  if X < WorkArea.Left then
    X := WorkArea.Left;
  if Y < WorkArea.Top then
    Y := WorkArea.Top;
  if Assigned(FOnDropDown) then
    FOnDropDown(FParentDropDownForm, FDropDownControl);

  B := THookForm(FForm).AlphaBlend;
  AValue := THookForm(FForm).AlphaBlendValue;
  if FDropDownAnimation then
  begin
    THookForm(FForm).AlphaBlendValue := 0;
    THookForm(FForm).AlphaBlend := True;
  end;
  //
  {$IFDEF VER300_UP}
  FScreenPPI := Screen.MonitorFromWindow(FForm.Handle).PixelsPerInch;
  {$ELSE}
  FScreenPPI := Screen.PixelsPerInch;
  {$ENDIF}
  if THookForm(FForm).Scaled and
    (THookForm(FForm).PixelsPerInch <> FScreenPPI) then
  begin
    OldScaleFactor := FScaleFactor;
    FScaleFactor :=  FScreenPPI / THookForm(FForm).PixelsPerInch;
    if FScaleFactor < 1 then FScaleFactor := 1;
    FScalePercent := Round(FScaleFactor * 100);
    if OldScaleFactor <> FScaleFactor then
    begin
      if Assigned(FOnChangeScale) then
       FOnChangeScale(FScaleFactor);
    end;
    FPPI := THookForm(FForm).PixelsPerInch;
  end;
  //
  SetWindowPos(FForm.Handle, HWND_TOPMOST, X, Y, 0, 0,
    SWP_SHOWWINDOW or SWP_NOSIZE);
  FForm.Realign;
  FForm.Visible := True;
  FForm.SetFocus;
  if FDropDownAnimation then
  begin
    Application.ProcessMessages;
    I := 0;
    TickCount := 0;
    AnimationStep := AValue div 10;
    if AnimationStep = 0 then AnimationStep := 1;
    repeat
      if (GetTickCount - TickCount > 5) then
      begin
        TickCount := GetTickCount;
        Inc(I, AnimationStep);
        if I > AValue then I := AValue;
        THookForm(FForm).AlphaBlendValue := I;
     end;
     Application.ProcessMessages;
   until i >= AValue;
   THookForm(FForm).AlphaBlend := B;
  end;
end;

procedure TscStyledForm.DropDown(ADropDownForm: TCustomForm; X, Y: Integer);
var
  WorkArea: TRect;
  I, AValue: Integer;
  B: Boolean;
  TickCount: Cardinal;
  AnimationStep: Integer;
  OldScaleFactor: Double;
  FScreenPPI: Integer;
begin
  if FActiveDropDownForm <> nil then
    CheckDropDownForms(FActiveDropDownForm);

  FParentDropDownForm := ADropDownForm;
  FDropDownControl := nil;
  FActiveDropDownForm := FForm;
  WorkArea := Screen.MonitorFromWindow(ADropDownForm.Handle).WorkAreaRect;
  if X + FForm.Width > WorkArea.Right then
    X := WorkArea.Right - FForm.Width;
 if Y + FForm.Height > WorkArea.Bottom then
    Y := WorkArea.Bottom - FForm.Height; 
  if X < WorkArea.Left then
    X := WorkArea.Left;
  if Y < WorkArea.Top then
    Y := WorkArea.Top;
  if Assigned(FOnDropDown) then
    FOnDropDown(FParentDropDownForm, FDropDownControl);
  B := THookForm(FForm).AlphaBlend;
  AValue := THookForm(FForm).AlphaBlendValue;
  if FDropDownAnimation then
  begin
    THookForm(FForm).AlphaBlendValue := 0;
    THookForm(FForm).AlphaBlend := True;
  end;
   //
  {$IFDEF VER300_UP}
  FScreenPPI := Screen.MonitorFromWindow(FForm.Handle).PixelsPerInch;
  {$ELSE}
  FScreenPPI := Screen.PixelsPerInch;
  {$ENDIF}
  if THookForm(FForm).Scaled and
    (THookForm(FForm).PixelsPerInch <> FScreenPPI) then
  begin
    OldScaleFactor := FScaleFactor;
    FScaleFactor :=  FScreenPPI / THookForm(FForm).PixelsPerInch;
    if FScaleFactor < 1 then FScaleFactor := 1;
    FScalePercent := Round(FScaleFactor * 100);
    if OldScaleFactor <> FScaleFactor then
    begin
      if Assigned(FOnChangeScale) then
       FOnChangeScale(FScaleFactor);
    end;
    FPPI := THookForm(FForm).PixelsPerInch;
  end;
  //
  SetWindowPos(FForm.Handle, HWND_TOPMOST, X, Y, 0, 0,
    SWP_SHOWWINDOW or SWP_NOSIZE);
  FForm.Realign;
  FForm.Visible := True;
  if FDropDownAnimation then
  begin
    Application.ProcessMessages;
    I := 0;
    TickCount := 0;
    AnimationStep := AValue div 10;
    if AnimationStep = 0 then AnimationStep := 1;
    repeat
      if (GetTickCount - TickCount > 5) then
      begin
        TickCount := GetTickCount;
        Inc(I, AnimationStep);
        if I > AValue then I := AValue;
        THookForm(FForm).AlphaBlendValue := I;
     end;
     Application.ProcessMessages;
   until i >= AValue;
   THookForm(FForm).AlphaBlend := B;
  end;
end;

procedure TscStyledForm.DropDown(ADropDownControl: TControl; X, Y: Integer);
var
  WorkArea: TRect;
  I, AValue: Integer;
  B: Boolean;
  TickCount: Cardinal;
  AnimationStep: Integer;
  OldScaleFactor: Double;
  FScreenPPI: Integer;
begin
  if FActiveDropDownForm <> nil then
    CheckDropDownForms(FActiveDropDownForm);

  FDropDownControl := ADropDownControl;
  if FDropDownControl <> nil then
  begin
    FActiveDropDownForm := FForm;
    FDropDownControl := ADropDownControl;
    FParentDropDownForm := GetParentForm(FDropDownControl);
    WorkArea := Screen.MonitorFromWindow(FParentDropDownForm.Handle).WorkAreaRect;
    if X + FForm.Width > WorkArea.Right then
      X := WorkArea.Right - FForm.Width;
    if Y + FForm.Height > WorkArea.Bottom then
      Y := WorkArea.Bottom - FForm.Height;
    if X < WorkArea.Left then
      X := WorkArea.Left;
    if Y < WorkArea.Top then
      Y := WorkArea.Top;
    if Assigned(FOnDropDown) then
      FOnDropDown(FParentDropDownForm, FDropDownControl);

    B := THookForm(FForm).AlphaBlend;
    AValue := THookForm(FForm).AlphaBlendValue;
    if FDropDownAnimation then
    begin
      THookForm(FForm).AlphaBlendValue := 0;
      THookForm(FForm).AlphaBlend := True;
    end;
     //
  {$IFDEF VER300_UP}
  FScreenPPI := Screen.MonitorFromWindow(FForm.Handle).PixelsPerInch;
  {$ELSE}
  FScreenPPI := Screen.PixelsPerInch;
  {$ENDIF}
  if THookForm(FForm).Scaled and
    (THookForm(FForm).PixelsPerInch <> FScreenPPI) then
  begin
    OldScaleFactor := FScaleFactor;
    FScaleFactor :=  FScreenPPI / THookForm(FForm).PixelsPerInch;
    if FScaleFactor < 1 then FScaleFactor := 1;
    FScalePercent := Round(FScaleFactor * 100);
    if OldScaleFactor <> FScaleFactor then
    begin
      if Assigned(FOnChangeScale) then
       FOnChangeScale(FScaleFactor);
    end;
    FPPI := THookForm(FForm).PixelsPerInch;
  end;
  //
    SetWindowPos(FForm.Handle, HWND_TOPMOST, X, Y, 0, 0,
      SWP_SHOWWINDOW or SWP_NOSIZE);
    FForm.Realign;
    FForm.Visible := True;
    if FDropDownAnimation then
    begin
      Application.ProcessMessages;
      I := 0;
      TickCount := 0;
      AnimationStep := AValue div 10;
      if AnimationStep = 0 then AnimationStep := 1;
      repeat
        if (GetTickCount - TickCount > 5) then
        begin
          TickCount := GetTickCount;
          Inc(I, AnimationStep);
          if I > AValue then I := AValue;
          THookForm(FForm).AlphaBlendValue := I;
       end;
       Application.ProcessMessages;
     until i >= AValue;
     THookForm(FForm).AlphaBlend := B;
    end;
  end;
end;

procedure TscStyledForm.DropDown(ADropDownControl: TControl; AFromRight: Boolean = False; AIsRightToLeft: Boolean = False);
var
  P: TPoint;
  WorkArea: TRect;
  I, AValue: Integer;
  B: Boolean;
  TickCount: Cardinal;
  AnimationStep: Integer;
   OldScaleFactor: Double;
  FScreenPPI: Integer;
begin
  if FActiveDropDownForm <> nil then
    CheckDropDownForms(FActiveDropDownForm);

  FDropDownControl := ADropDownControl;
  FActiveDropDownForm := FForm;
  FParentDropDownForm := GetParentForm(FDropDownControl);
  WorkArea := Screen.MonitorFromWindow(FParentDropDownForm.Handle).WorkAreaRect;
  if not AIsRightToLeft then
  begin
    if AFromRight then
    begin
      if FDropDownControl.Parent <> nil then
        P := FDropDownControl.Parent.ClientToScreen(Point(FDropDownControl.Left + FDropDownControl.Width,
          FDropDownControl.Top))
      else
        P := FDropDownControl.ClientToScreen(Point(FDropDownControl.Left + FDropDownControl.Width, FDropDownControl.Top));
    end
    else
    begin
      if FDropDownControl.Parent <> nil then
        P := FDropDownControl.Parent.ClientToScreen(Point(FDropDownControl.Left,
          FDropDownControl.Top + FDropDownControl.Height))
      else
        P := FDropDownControl.ClientToScreen(Point(0, FDropDownControl.Height));
    end;
  end
  else
  begin
    if AFromRight then
    begin
      if FDropDownControl.Parent <> nil then
        P := FDropDownControl.Parent.ClientToScreen(Point(FDropDownControl.Left - FForm.Width,
          FDropDownControl.Top))
      else
        P := FDropDownControl.ClientToScreen(Point(FDropDownControl.Left - FForm.Width, FDropDownControl.Top));
    end
    else
    begin
      if FDropDownControl.Parent <> nil then
        P := FDropDownControl.Parent.ClientToScreen(Point(FDropDownControl.Left + FDropDownControl.Width - FForm.Width,
          FDropDownControl.Top + FDropDownControl.Height))
      else
        P := FDropDownControl.ClientToScreen(Point(-FForm.Width, FDropDownControl.Height));
    end;
  end;

  if P.Y + FForm.Height > WorkArea.Bottom then
  begin
    P := FDropDownControl.ClientToScreen(Point(0, 0));
    P.Y := P.Y - FForm.Height;
  end;
  if P.X + FForm.Width > WorkArea.Right then
  begin
    if AFromRight then
      P.X := P.X - FForm.Width - FDropDownControl.Width
    else
      P.X := WorkArea.Right - FForm.Width;
  end;

  if P.X < WorkArea.Left then
    P.X := WorkArea.Left;
  if P.Y < WorkArea.Top then
    P.Y := WorkArea.Top;
  if Assigned(FOnDropDown) then
      FOnDropDown(FParentDropDownForm, FDropDownControl);

   B := THookForm(FForm).AlphaBlend;
   AValue := THookForm(FForm).AlphaBlendValue;
   if FDropDownAnimation then
   begin
    THookForm(FForm).AlphaBlendValue := 0;
    THookForm(FForm).AlphaBlend := True;
  end;
     //
  {$IFDEF VER300_UP}
  FScreenPPI := Screen.MonitorFromWindow(FForm.Handle).PixelsPerInch;
  {$ELSE}
  FScreenPPI := Screen.PixelsPerInch;
  {$ENDIF}
  if THookForm(FForm).Scaled and
    (THookForm(FForm).PixelsPerInch <> FScreenPPI) then
  begin
    OldScaleFactor := FScaleFactor;
    FScaleFactor :=  FScreenPPI / THookForm(FForm).PixelsPerInch;
    if FScaleFactor < 1 then FScaleFactor := 1;
    FScalePercent := Round(FScaleFactor * 100);
    if OldScaleFactor <> FScaleFactor then
    begin
      if Assigned(FOnChangeScale) then
       FOnChangeScale(FScaleFactor);
    end;
    FPPI := THookForm(FForm).PixelsPerInch;
  end;
  //
  SetWindowPos(FForm.Handle, HWND_TOPMOST, P.X, P.Y, 0, 0,
     SWP_SHOWWINDOW or SWP_NOSIZE);
  FForm.Realign;
  FForm.Visible := True;
  if FDropDownAnimation then
  begin
    Application.ProcessMessages;
    I := 0;
    TickCount := 0;
    AnimationStep := AValue div 10;
    if AnimationStep = 0 then AnimationStep := 1;
    repeat
      if (GetTickCount - TickCount > 5) then
      begin
        TickCount := GetTickCount;
        Inc(I, AnimationStep);
        if I > AValue then I := AValue;
        THookForm(FForm).AlphaBlendValue := I;
      end;
      Application.ProcessMessages;
    until i >= AValue;
    THookForm(FForm).AlphaBlend := B;
  end;
end;

procedure TscStyledForm.CloseUp(AAcceptChanges: Boolean);
var
  C: TControl;
  F: TCustomForm;
begin
  F := FParentDropDownForm;
  if FActiveDropDownForm = FForm then
  begin
    FParentDropDownForm := nil;
    FActiveDropDownForm := nil;
  end;
  SetWindowPos(FForm.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  FForm.Visible := False;
  C := FDropDownControl;
  FDropDownControl := nil;
  if (FDropDownButtonItem <> nil) then
  begin
    FDropDownButtonItem.NCObject.CloseUp(AAcceptChanges);
    FDropDownButtonItem := nil;
  end;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(F, C, AAcceptChanges);
end;

procedure TscStyledForm.SetCaptionWallpaperLeftMargin(Value: Integer);
begin
  if FCaptionWallpaperLeftMargin <> Value then
  begin
    FCaptionWallpaperLeftMargin := Value;
    if (FCaptionWallpapers <> nil) and not (csDesigning in ComponentState) and
      not (csLoading in ComponentState) then
         UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetCaptionWallpaperTopMargin(Value: Integer);
begin
  if FCaptionWallpaperTopMargin <> Value then
  begin
    FCaptionWallpaperTopMargin := Value;
    if (FCaptionWallpapers <> nil) and not (csDesigning in ComponentState) and
      not (csLoading in ComponentState) then
         UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetCaptionWallpaperRightMargin(Value: Integer);
begin
  if FCaptionWallpaperRightMargin <> Value then
  begin
    FCaptionWallpaperRightMargin := Value;
    if (FCaptionWallpapers <> nil) and not (csDesigning in ComponentState) and
      not (csLoading in ComponentState) then
         UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetCaptionWallpaperBottomMargin(Value: Integer);
begin
  if FCaptionWallpaperBottomMargin <> Value then
  begin
    FCaptionWallpaperBottomMargin := Value;
    if (FCaptionWallpapers <> nil) and not (csDesigning in ComponentState) and
      not (csLoading in ComponentState) then
         UpdateFormNC;
  end;
end;



procedure TscStyledForm.SetCaptionWallpaperIndex(Value: Integer);
begin
  if FCaptionWallpaperIndex <> Value then
  begin
    FCaptionWallpaperIndex := Value;
    if (FCaptionWallpapers <> nil) and not (csDesigning in ComponentState) and
      not (csLoading in ComponentState) then
     UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetCaptionWallpapers(Value: TscCustomImageCollection);
begin
  if FCaptionWallpapers <> Value then
  begin
    FCaptionWallpapers := Value;
    if (FCaptionWallpaperIndex <> -1) and not (csDesigning in ComponentState) and
      not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
end;

procedure TscStyledForm.SetButtonFont(Value: TFont);
begin
  FButtonFont.Assign(Value);
end;

procedure TscStyledForm.SetTabFont(Value: TFont);
begin
  FTabFont.Assign(Value);
end;

procedure TscStyledForm.SetCaptionAlignment;
begin
  if FCaptionAlignment <> Value then
  begin
    FCaptionAlignment := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetShowButtons(Value: Boolean);
begin
  if FShowButtons <> Value then
  begin
    FShowButtons := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetShowInactiveTab(Value: Boolean);
begin
  if FShowInactiveTab <> Value then
  begin
    FShowInactiveTab := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;
    
procedure TscStyledForm.SetShowTabs(Value: Boolean);
begin
  if FShowTabs <> Value then
  begin
    FShowTabs := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetTabIndex(Value: Integer);
begin
  if FTabIndex <> Value then
  begin
    FTabIndex := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetTabsPosition(Value: TscNCTabsPosition);
begin
  if FTabsPosition <> Value then
  begin
    FTabsPosition := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;

procedure TscStyledForm.SetButtons(Value: TscNCButtonItems);
begin
  FButtons.Assign(Value);
end;

procedure TscStyledForm.SetTabs(Value: TscNCTabItems);
begin
  FTabs.Assign(Value);
end;

procedure TscStyledForm.SetShowIcon(Value: Boolean);
begin
  if FShowIcon <> Value then
  begin
    FShowIcon := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      UpdateFormNC;
  end;
end;

constructor TscNCButtonItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  NCObject := TscNCButtonObject.Create(Self);
  FDown := False;
  FCustomDropDown := False;
  FGroupIndex := 0;
  FAllowAllUp := False;
  FCaption := '';
  FImageIndex := -1;
  FHotImageIndex := -1;
  FPressedImageIndex := -1;
  FEnabled := True;
  FVisible := True;
  FPopupMenu := nil;
  FSplitButton := False;
  FStyle := scncToolButtonTransparent;
  FHeight := 0;
  FWidth := 0;
  FMarginLeft := 0;
  FMarginRight := 0;
  FMarginTop := 2;
  FMarginBottom := 2;
  FSpacing := 5;                
  FMargin := -1;
  FContentMargin := 0;
  FHint := '';
end;

destructor TscNCButtonItem.Destroy;
begin
  NCObject.Free;
  inherited;
end;

procedure TscNCButtonItem.SetSpacing(Value: Integer);
begin
  if (Value <> FSpacing) and (Value >= 0) then
  begin
    FSpacing := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetContentMargin(Value: Integer);
begin
  if (Value <> FContentMargin) and (Value >= 0) then
  begin
    FContentMargin := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetPosition(Value: TscNCButtonPosition);
begin
  if Value <> FPosition then
  begin
    FPosition := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetWidth(Value: Integer);
begin
  if (Value >= 0) and (Value <> FWidth) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetHeight(Value: Integer);
begin
  if (Value >= 0) and (Value <> FHeight) then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetMarginLeft(Value: Integer);
begin
  if Value <> FMarginLeft then
  begin
    FMarginLeft := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetMarginRight(Value: Integer);
begin
  if Value <> FMarginRight then
  begin
    FMarginRight := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetMarginTop(Value: Integer);
begin
  if Value <> FMarginTop then
  begin
    FMarginTop := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetMarginBottom(Value: Integer);
begin
  if Value <> FMarginBottom then
  begin
    FMarginBottom := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetSplitButton(Value: Boolean);
begin
  if FSplitButton <> Value then
  begin
    FSplitButton := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetStyle(Value: TscNCButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetPopupMenu(Value: TPopupMenu);
begin
  if Value <> FPopupMenu then
  begin
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(TscNCButtonItems(Collection).StyledForm);

    FPopupMenu := Value;

    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(TscNCButtonItems(Collection).StyledForm);
  end;
end;

procedure TscNCButtonItem.Assign(Source: TPersistent);
begin
  if Source is TscNCButtonItem then
  begin
    FImageIndex := TscNCButtonItem(Source).ImageIndex;
    FHotImageIndex := TscNCButtonItem(Source).HotImageIndex;
    FPressedImageIndex := TscNCButtonItem(Source).PressedImageIndex;
    FEnabled := TscNCButtonItem(Source).Enabled;
    FPopupMenu := TscNCButtonItem(Source).PopupMenu;
    FVisible := TscNCButtonItem(Source).Visible;
    FSplitButton := TscNCButtonItem(Source).SplitButton;
    FCaption := TscNCButtonItem(Source).Caption;
    FStyle := TscNCButtonItem(Source).Style;
    FHeight := TscNCButtonItem(Source).Height;
    FWidth := TscNCButtonItem(Source).Width;
    FMarginLeft := TscNCButtonItem(Source).MarginLeft;
    FMarginTop := TscNCButtonItem(Source).MarginTop;
    FMarginRight := TscNCButtonItem(Source).MarginRight;
    FMarginBottom := TscNCButtonItem(Source).MarginBottom;
    FPosition := TscNCButtonItem(Source).Position;
    FSpacing := TscNCButtonItem(Source).Spacing;
    FMargin := TscNCButtonItem(Source).Margin;
    FHint := TscNCButtonItem(Source).Hint;
    FCustomDropDown :=  TscNCButtonItem(Source).CustomDropDown;
  end
  else
    inherited Assign(Source);
end;

procedure TscNCButtonItem.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetDown(Value: Boolean);
var
  StyledForm: TscStyledForm;
  I: Integer;
begin
  if FDown <> Value  then
  begin
    FDown := Value;
    StyledForm := TscNCButtonItems(Collection).StyledForm;
    if StyledForm <> nil then
    begin
      for I := 0 to StyledForm.Buttons.Count - 1 do
        if (I <> Index) and
           (StyledForm.Buttons[I].GroupIndex = GroupIndex)
        then
          StyledForm.Buttons[I].FDown := False;
    end;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value  then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TscNCButtonItem.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

constructor TscNCButtonItems.Create;
begin
  inherited Create(TscNCButtonItem);
  StyledForm := AStyledForm;
end;

function TscNCButtonItems.GetOwner: TPersistent;
begin
  Result := StyledForm;
end;

procedure TscNCButtonItems.Update(Item: TCollectionItem);
begin
  if StyledForm = nil then Exit;
  if not (csDesigning in StyledForm.ComponentState) and
     not (csLoading in StyledForm.ComponentState)
  then
    StyledForm.UpdateFormNC;
end;

function TscNCButtonItems.GetItem(Index: Integer):  TscNCButtonItem;
begin
  Result := TscNCButtonItem(inherited GetItem(Index));
end;

procedure TscNCButtonItems.SetItem(Index: Integer; Value:  TscNCButtonItem);
begin
  inherited SetItem(Index, Value);
end;

function TscNCButtonItems.Add: TscNCButtonItem;
begin
  Result := TscNCButtonItem(inherited Add);
end;

function TscNCButtonItems.Insert(Index: Integer): TscNCButtonItem;
begin
  Result := TscNCButtonItem(inherited Insert(Index));
end;

procedure TscNCButtonItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TscNCButtonItems.Clear;
begin
  inherited Clear;
end;

constructor TscNCTabItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  NCObject := TscNCTabObject.Create(Self);
  FImageIndex := -1;
  FCaption := '';
  FEnabled := True;
  FVisible := True;
  FHeight := 0;
  FWidth := 0;
  FMarginTop := 3;
  FSpacing := 5;
  FContentMargin := 0;
  FHint := '';
end;

destructor TscNCTabItem.Destroy;
begin
  NCObject.Free;
  inherited;
end;

procedure TscNCTabItem.SetSpacing(Value: Integer);
begin
  if (Value <> FSpacing) and (Value >= 0) then
  begin
    FSpacing := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.Assign(Source: TPersistent);
begin
  if Source is TscNCTabItem then
  begin
    FImageIndex := TscNCTabItem(Source).ImageIndex;
    FCaption := TscNCTabItem(Source).Caption;
    FVisible := TscNCTabItem(Source).Visible;
    FEnabled := TscNCTabItem(Source).Enabled;
    FHeight := TscNCTabItem(Source).Height;
    FWidth := TscNCTabItem(Source).Width;
    FMarginTop := TscNCTabItem(Source).MarginTop;
    FSpacing := TscNCTabItem(Source).Spacing;
    FHint := TscNCTabItem(Source).Hint;
  end
  else
    inherited Assign(Source);
end;

procedure TscNCTabItem.SetContentMargin(Value: Integer);
begin
  if Value <> FContentMargin then
  begin
    FContentMargin := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.SetMarginTop(Value: Integer);
begin
  if Value <> FMarginTop then
  begin
    FMarginTop := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TscNCTabItem.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

constructor TscNCTabItems.Create;
begin
  inherited Create(TscNCTabItem);
  StyledForm := AStyledForm;
end;

function TscNCTabItems.GetOwner: TPersistent;
begin
  Result := StyledForm;
end;

procedure TscNCTabItems.Update(Item: TCollectionItem);
begin
  if StyledForm = nil then Exit;
  if not (csDesigning in StyledForm.ComponentState) and
     not (csLoading in StyledForm.ComponentState)
  then
    StyledForm.UpdateFormNC;
end;

function TscNCTabItems.GetItem(Index: Integer):  TscNCTabItem;
begin
  Result := TscNCTabItem(inherited GetItem(Index));
end;

procedure TscNCTabItems.SetItem(Index: Integer; Value:  TscNCTabItem);
begin
  inherited SetItem(Index, Value);
end;

function TscNCTabItems.Add: TscNCTabItem;
begin
  Result := TscNCTabItem(inherited Add);
end;

function TscNCTabItems.Insert(Index: Integer): TscNCTabItem;
begin
  Result := TscNCTabItem(inherited Insert(Index));
end;

procedure TscNCTabItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TscNCTabItems.Clear;
begin
  inherited Clear;
end;

constructor TscNCObject.Create(AParent: TCollectionItem);
begin
  Parent := AParent;
  ObjectRect := Rect(0, 0, 0, 0);
  FMouseIn := False;
  FDown := False;
end;

function TscNCObject.GetObjectWidth(AWidth: Integer; ACanvas: TCanvas): Integer;
begin
  Result := AWidth;
end;

function TscNCObject.GetStyledForm: TscStyledForm;
begin
  Result := nil;
end;

procedure TscNCObject.MouseDown(X, Y: Integer);
begin
  FDown := True;
end;

procedure TscNCObject.MouseUp(X, Y: Integer);
begin
  FDown := False;
end;

procedure TscNCObject.MouseMove(X, Y: Integer);
begin
  if not FMouseIn then
    FMouseIn := True;
end;

procedure TscNCObject.MouseEnter;
begin
  FMouseIn := True;
end;

procedure TscNCObject.MouseLeave;
begin
  FMouseIn := False;
end;

procedure TscNCObject.Draw(ACanvas: TCanvas);
begin
end;

type
  TCollectionItemClass = class(TCollectionItem);

procedure TscNCObject.Invalidate;
begin
  TCollectionItemClass(Parent).Changed(False);
end;

constructor TscNCButtonObject.Create(AParent: TCollectionItem);
begin
  inherited;
  ButtonItem := TscNCButtonItem(Parent);
  FDroppedDown := False;
end;

function TscNCButtonObject.GetStyledForm: TscStyledForm;
begin
  Result := TscNCButtonItems(ButtonItem.Collection).StyledForm;
end;

function TscNCButtonObject.GetObjectWidth(AWidth: Integer; ACanvas: TCanvas): Integer;
var
  W: Integer;
begin
  Result := AWidth;
  if ButtonItem = nil then Exit;
  if StyledForm = nil then Exit;

  if (ButtonItem.ImageIndex <> -1) and (StyledForm.ButtonImages <> nil) then
    W := StyledForm.ButtonImages.Width
  else
    W := 0;

  if ButtonItem.Caption <> '' then
  begin
    if W = 0 then
      W := ACanvas.TextWidth(ButtonItem.Caption) + 6
    else
      W := 6 + W + ButtonItem.Spacing + ACanvas.TextWidth(ButtonItem.Caption);
  end
  else
  if (W <> 0) and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
  begin
    Inc(W, 6);
  end;

  if  ButtonItem.ContentMargin > 0 then
    Inc(W, Round(ButtonItem.ContentMargin * 2 * StyledForm.BorderScaleFactor));

  if ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
  if ButtonItem.SplitButton then
    Inc(W, Round(StyledForm.BorderScaleFactor * 17))
  else
    Inc(W, Round(StyledForm.BorderScaleFactor * 13));

  if W > Result then
   Result := W;
end;

procedure TscNCButtonObject.MouseDown(X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if (StyledForm <> nil) and StyledForm.FMenuTracking then
  begin
    FDown := False;
    FDroppedDown := False;
    FMouseIn := True;
    Exit;
  end;

  FAllowAllUpCheck := False;
  if ButtonItem.CustomDropDown then
  begin
    if not ButtonItem.SplitButton then
      TrackCustomWindow
    else
    begin
      R := ObjectRect;
      R.Left := R.Right - Round(StyledForm.BorderScaleFactor * 17);
      if R.Contains(Point(X, Y)) then
        TrackCustomWindow;
    end;
  end;
  if ButtonItem.PopupMenu <> nil then
  begin
    if not ButtonItem.SplitButton then
      TrackPopupMenu
    else
    begin
      R := ObjectRect;
      R.Left := R.Right - Round(StyledForm.BorderScaleFactor * 17);
      if R.Contains(Point(X, Y)) then
        TrackPopupMenu;
    end;
  end;
  if (ButtonItem.GroupIndex > 0) and not ButtonItem.Down then
  begin
    ButtonItem.Down := True;

    if Assigned(ButtonItem.OnMouseDown) then
     ButtonItem.OnMouseDown(ButtonItem.Index, X, Y);

    if Assigned(ButtonItem.OnClick) then
      ButtonItem.OnClick(Self);
    Exit;
  end;
  if (ButtonItem.GroupIndex > 0) and ButtonItem.Down and ButtonItem.AllowAllUp then
    FAllowAllUpCheck := True;
  if not FDroppedDown then Invalidate;

   if Assigned(ButtonItem.OnMouseDown) then
     ButtonItem.OnMouseDown(ButtonItem.Index, X, Y);
end;

procedure TscNCButtonObject.MouseUp(X, Y: Integer);
begin
  inherited;
  if (StyledForm <> nil) and StyledForm.FMenuTracking then
    Exit;

  if ButtonItem.GroupIndex > 0 then
  begin
    if ButtonItem.AllowAllUp and FAllowAllUpCheck then
    begin
      FAllowAllUpCheck := False;
      ButtonItem.FDown := False;
      Invalidate;
      if Assigned(ButtonItem.OnClick) then
        ButtonItem.OnClick(Self);
    end;
    Exit;
  end;
  if not FDroppedDown then Invalidate;

  if Assigned(ButtonItem.OnMouseUp) then
     ButtonItem.OnMouseUp(ButtonItem.Index, X, Y);

  if Assigned(ButtonItem.OnClick) then
    ButtonItem.OnClick(Self);
end;

procedure TscNCButtonObject.MouseMove(X, Y: Integer);
begin
  inherited;
  if Assigned(ButtonItem.OnMouseMove) then
     ButtonItem.OnMouseMove(ButtonItem.Index, X, Y);
end;

procedure TscNCButtonObject.MouseEnter;
begin
  inherited;
  if (ButtonItem <> nil) and (ButtonItem.Hint <> '') and
     (StyledForm <> nil) and (StyledForm.HintComponent <> nil) and
      StyledForm.ShowHints
  then
    StyledForm.HintComponent.ActivateHint(ButtonItem.Hint);
  if not FDroppedDown then Invalidate;
  if Assigned(ButtonItem.OnMouseEnter) then
     ButtonItem.OnMouseEnter(ButtonItem.Index);
end;

procedure TscNCButtonObject.MouseLeave;
begin
  inherited;
  if (ButtonItem <> nil) and (ButtonItem.Hint <> '') and
     (StyledForm <> nil) and (StyledForm.HintComponent <> nil) and
     StyledForm.ShowHints
  then
    StyledForm.HintComponent.HideHint;
  FDown := False;
  if not FDroppedDown then Invalidate;
  if Assigned(ButtonItem.OnMouseLeave) then
    ButtonItem.OnMouseLeave(ButtonItem.Index);
end;

procedure TscNCButtonObject.TrackCustomWindow;
var
  MX, MY: Integer;
  R: TRect;
begin
  MX := StyledForm.FForm.Left + ObjectRect.Left;
  MY := StyledForm.FForm.Top + ObjectRect.Bottom;
  if (THookForm(StyledForm.FForm).FormStyle = fsMDIChild) and (Application.MainForm <> nil) then
  begin
    GetWindowRect(Application.MainForm.ClientHandle, R);
    Inc(MX, R.Left);
    Inc(MY, R.Top);
  end;
  FDroppedDown := True;
  Invalidate;
  if Assigned(ButtonItem.OnClick) and not ButtonItem.SplitButton then
    ButtonItem.OnClick(Self);
  if Assigned(ButtonItem.OnDropDown) then
  begin
    ButtonItem.OnDropDown(ButtonItem.Index, MX, MY);
  end;
end;

procedure TscNCButtonObject.CloseUp;
var
  P: TPoint;
  R: TRect;
begin
  FDown := False;
  FDroppedDown := False;
  GetCursorPos(P);
  R := ObjectRect;
  OffsetRect(R, StyledForm.FForm.Left, StyledForm.FForm.Top);
  FMouseIn := R.Contains(P);
  Invalidate;
  if Assigned(ButtonItem.OnCloseUp) then
    ButtonItem.OnCloseUp(ButtonItem.Index, AAcceptChanges);
end;

procedure TscNCButtonObject.TrackPopupMenu;
var
  MX, MY: Integer;
  R: TRect;
begin
  MX := StyledForm.FForm.Left + ObjectRect.Left;
  MY := StyledForm.FForm.Top + ObjectRect.Bottom;
  if (THookForm(StyledForm.FForm).FormStyle = fsMDIChild) and (Application.MainForm <> nil) then
  begin
    GetWindowRect(Application.MainForm.ClientHandle, R);
    Inc(MX, R.Left);
    Inc(MY, R.Top);
  end;
  FDroppedDown := True;
  Invalidate;
  if Assigned(ButtonItem.OnClick) and not ButtonItem.SplitButton then
    ButtonItem.OnClick(Self);

  if StyledForm <> nil then
    StyledForm.FMenuTracking := True;

  ButtonItem.PopupMenu.Popup(MX, MY);
  FDown := False;
  FDroppedDown := False;
  FMouseIn := False;

  if StyledForm <> nil then
  begin
    StyledForm.FUpdating := True;
    SetTimer(StyledForm.FForm.Handle, 22, 10, nil);
  end
  else
    Invalidate;
end;

procedure TscNCButtonObject.Draw(ACanvas: TCanvas);
var
  FState: TscsCtrlState;
  R: TRect;
  TextColor, ArrowColor: TColor;
  IIndex: Integer;
  SaveIndex: Integer;
  FHandled: Boolean;
begin
  inherited;
  if StyledForm = nil then Exit;
  if Assigned(ButtonItem.OnPaint) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      ButtonItem.OnPaint(ButtonItem.Index,
        ACanvas, ObjectRect, FMouseIn, FDown, FDroppedDown);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    Exit;
  end;
  IIndex := ButtonItem.ImageIndex;
  if FDown or FDroppedDown or ButtonItem.Down then
  begin
    FState := scsPressed;
    IIndex := ButtonItem.PressedImageIndex;
  end
  else  
  if FMouseIn then
  begin
    FState := scsHot;
    IIndex := ButtonItem.HotImageIndex;
  end
  else
  if StyledForm.IsNCObjectEnabled(Self) 
  then
    FState := scsNormal
  else
    FState := scsDisabled;
  if IIndex = -1 then 
    IIndex := ButtonItem.ImageIndex;

  TextColor := GetStyleColor(clBtnText);
  ArrowColor := TextColor;

  case ButtonItem.Style of
    scncPushButton:
    begin
      if ButtonItem.SplitButton and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
        DrawSplitButton(ACanvas, ObjectRect, FState, FDroppedDown, Round(StyledForm.BorderScaleFactor * 17))
      else
        DrawButton(ACanvas, ObjectRect, FState, False);

      if ButtonItem.SplitButton and (ButtonItem.PopupMenu <> nil) and FDroppedDown then
      begin
        TextColor := scDrawUtils.GetButtonTextColor(scsHot);
        ArrowColor := scDrawUtils.GetButtonTextColor(scsPressed);
      end
      else
      begin
        TextColor := scDrawUtils.GetButtonTextColor(FState);
        ArrowColor := TextColor;
      end;
    end;
    scncToolButton:
    begin
      if ButtonItem.SplitButton and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
        DrawSplitToolButton(ACanvas, ObjectRect, FState, FDroppedDown, Round(StyledForm.BorderScaleFactor * 17))
      else
        DrawToolButton(ACanvas, ObjectRect, FState);

      if ButtonItem.SplitButton and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) and FDroppedDown then
      begin
        TextColor := scDrawUtils.GetToolButtonTextColor(scsHot);
        ArrowColor := scDrawUtils.GetToolButtonTextColor(scsPressed);
      end
      else
      begin
        TextColor := scDrawUtils.GetToolButtonTextColor(FState);
        ArrowColor := TextColor;
      end;
    end;
    scncPushButtonTransparent:
    begin
      if (FState = scsHot) or (FState = scsPressed) then
      begin
        if ButtonItem.SplitButton and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
          DrawSplitButton(ACanvas, ObjectRect, FState, FDroppedDown, Round(StyledForm.BorderScaleFactor * 17))
        else
          DrawButton(ACanvas, ObjectRect, FState, False);

        if ButtonItem.SplitButton and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) and FDroppedDown then
        begin
          TextColor := scDrawUtils.GetButtonTextColor(scsHot);
          ArrowColor := scDrawUtils.GetButtonTextColor(scsPressed);
        end
        else
        begin
          TextColor := scDrawUtils.GetButtonTextColor(FState);
          ArrowColor := TextColor;
        end;
      end
      else
        TextColor := GetCaptionTextColor(FState);
    end;
    scncToolButtonTransparent:
    begin
      if (FState = scsHot) or (FState = scsPressed) then
      begin
        if ButtonItem.SplitButton and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
          DrawSplitToolButton(ACanvas, ObjectRect, FState, FDroppedDown, Round(StyledForm.BorderScaleFactor * 17))
        else
          DrawToolButton(ACanvas, ObjectRect, FState);
        if ButtonItem.SplitButton and ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) and FDroppedDown then
        begin
          TextColor := scDrawUtils.GetToolButtonTextColor(scsHot);
          ArrowColor := scDrawUtils.GetToolButtonTextColor(scsPressed);
        end
        else
        begin
          TextColor := scDrawUtils.GetToolButtonTextColor(FState);
          ArrowColor := TextColor;
        end;
      end
      else
      begin
        TextColor := scDrawUtils.GetCaptionTextColor(FState);
        ArrowColor := TextColor;
      end;
    end;
    scncTransparent:
    begin
      TextColor := scDrawUtils.GetCaptionTextColor(FState);
      ArrowColor := TextColor;
    end;
    scncLink:
    begin
      TextColor := scDrawUtils.GetCaptionTextColor(FState);
      ArrowColor := TextColor;
    end;
  end;
  R := ObjectRect;
  if ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
    if ButtonItem.SplitButton then
      Dec(R.Right, Round(StyledForm.BorderScaleFactor * 17))
    else
      Dec(R.Right, Round(StyledForm.BorderScaleFactor * 13));

  ACanvas.Font.Color := TextColor;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    SetTextColor(ACanvas.Handle, TextColor);

    if FMouseIn and (ButtonItem.Style = scncLink) then
      ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderLine];

    FHandled := False;

    if Assigned(ButtonItem.OnGetPaintParams) then
      ButtonItem.OnGetPaintParams(ButtonItem.Index,
      ACanvas, ObjectRect, FMouseIn, FDown, FDroppedDown, FHandled);

    if not FHandled then
    begin
      DrawImageAndTextLeft2(ACanvas, R, ButtonItem.Margin, ButtonItem.Spacing,
         ButtonItem.Caption, IIndex, StyledForm.ButtonImages,
         ButtonItem.Enabled, StyledForm.FForm.BiDiMode = bdRightToLeft, True);
    end;

  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
  if ((ButtonItem.PopupMenu <> nil) or ButtonItem.CustomDropDown) then
  begin
    R := ObjectRect;
    R.Left := R.Right - Round(StyledForm.BorderScaleFactor * 17);
    if SC_MODERNARROWS then
      scDrawUtils.DrawButtonModernArrowImage(ACanvas, R, ArrowColor, StyledForm.BorderScaleFactor)
    else
      scDrawUtils.DrawButtonArrowImage(ACanvas, R, ArrowColor, StyledForm.BorderScaleFactor);
    if ((ButtonItem.Style = scncTransparent) or
       (ButtonItem.Style = scncLink)) and ButtonItem.SplitButton
    then
    begin
      Inc(R.Top, 2);
      Dec(R.Bottom, 2);
      DrawVertSplitter(ACanvas, R);
    end;
  end;
end;

constructor TscNCTabObject.Create(AParent: TCollectionItem);
begin
  inherited;
  TabItem := TscNCTabItem(Parent);
end;

function TscNCTabObject.GetObjectWidth(AWidth: Integer; ACanvas: TCanvas): Integer;
var
  W: Integer;
begin
  Result := AWidth;
  if (TabItem = nil) or (StyledForm = nil ) then Exit;

  if (TabItem.ImageIndex <> -1) and (StyledForm.ButtonImages <> nil) then
    W := StyledForm.ButtonImages.Width
  else
    W := 0;

  if TabItem.Caption <> '' then
  begin
    if W = 0 then
      W := ACanvas.TextWidth(TabItem.Caption) + 14 +
        Round(TabItem.ContentMargin * 2 * StyledForm.BorderScaleFactor)
    else
      W := 7 + W + TabItem.Spacing + ACanvas.TextWidth(TabItem.Caption) + 7 +
        TabItem.ContentMargin * 2;
  end;

  if W > Result then
   Result := W;
end;

function TscNCTabObject.GetStyledForm: TscStyledForm;
begin
  Result := TscNCTabItems(TabItem.Collection).StyledForm;
end;

procedure TscNCTabObject.MouseDown(X, Y: Integer);
begin
  inherited;
  if (StyledForm <> nil) and (StyledForm.TabIndex <> TabItem.Index) then
  begin
    StyledForm.TabIndex := TabItem.Index;
    if Assigned(TabItem.OnClick) then
       TabItem.OnClick(Self);
    if Assigned(StyledForm.OnTabChanged) then
       StyledForm.OnTabChanged(TabItem);
  end;
  if Assigned(TabItem.OnMouseDown) then
    TabItem.OnMouseDown(TabItem.Index, X, Y);
end;

procedure TscNCTabObject.MouseUp(X, Y: Integer);
begin
  inherited;
  if Assigned(TabItem.OnMouseUp) then
    TabItem.OnMouseUp(TabItem.Index, X, Y);
end;

procedure TscNCTabObject.MouseMove(X, Y: Integer);
begin
  inherited;
  if Assigned(TabItem.OnMouseMove) then
    TabItem.OnMouseMove(TabItem.Index, X, Y);
end;

procedure TscNCTabObject.MouseEnter;
begin
  inherited;
  if (TabItem <> nil) and (TabItem.Hint <> '') and
     (StyledForm <> nil) and (StyledForm.HintComponent <> nil) and
      StyledForm.ShowHints
  then
    StyledForm.HintComponent.ActivateHint(TabItem.Hint);

  Invalidate;
  
  if Assigned(TabItem.OnMouseEnter) then
    TabItem.OnMouseEnter(TabItem.Index);
end;

procedure TscNCTabObject.MouseLeave;
begin
  inherited;
  if (TabItem <> nil) and (TabItem.Hint <> '') and
     (StyledForm <> nil) and (StyledForm.HintComponent <> nil) and
     StyledForm.ShowHints
  then
    StyledForm.HintComponent.HideHint;
  Invalidate;

  if Assigned(TabItem.OnMouseLeave) then
    TabItem.OnMouseLeave(TabItem.Index);
end;

procedure TscNCTabObject.Draw(ACanvas: TCanvas);
var
  FState: TscsCtrlState;
  R: TRect;
  FHandled: Boolean;
  SaveIndex: Integer;
  TextColor: TColor;
begin
  if StyledForm = nil then Exit;
  if Assigned(TaBItem.OnPaint) then
  begin
    TabItem.OnPaint(TabItem.Index,
      ACanvas, ObjectRect, FMouseIn, StyledForm.TabIndex = TabItem.Index);
    Exit;
  end;
  R := ObjectRect;
  if StyledForm.TabIndex = TabItem.Index then
    FState := scsFocused
  else
  if FMouseIn then
    FState := scsHot
  else
  if StyledForm.IsNCObjectEnabled(Self) 
  then
    FState := scsNormal
  else
    FState := scsDisabled;
  // draw tab
  if not (((FState = scsNormal) or (FState = scsDisabled)) and
     not StyledForm.ShowInactiveTab)  then
    DrawSimpleTab(ACanvas, R, FState);
  // draw text and image
  R := ObjectRect;
  if ((FState = scsNormal) or (FState = scsDisabled)) and not StyledForm.ShowInactiveTab then
    TextColor := GetCaptionTextColor(FState)
  else
    TextColor := scDrawUtils.GetTabTextColor(FState);
  ACanvas.Font.Color := TextColor;
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    SetTextColor(ACanvas.Handle, TextColor);

    FHandled := False;
    if Assigned(TabItem.OnGetPaintParams) then
      TabItem.OnGetPaintParams(TabItem.Index,
      ACanvas, ObjectRect, FMouseIn, StyledForm.TabIndex = TabItem.Index, FHandled);
    Inc(R.Bottom);
    if not FHandled then
       DrawImageAndTextLeft2(ACanvas, R, -1, TabItem.Spacing,
       TabItem.Caption, TabItem.ImageIndex, StyledForm.TabImages,
       TabItem.Enabled, StyledForm.FForm.BiDiMode = bdRightToLeft, True);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

constructor TscClientInActivePanel.Create(AOwner: TComponent);
begin
  inherited;
  Buffer := nil;
  Visible := False;
end;

destructor TscClientInActivePanel.Destroy;
begin
  if Buffer <> nil then Buffer.Free;
  Buffer := nil;
  inherited;
end;

procedure TscClientInActivePanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TscClientInActivePanel.Paint;
begin
  if Buffer <> nil then
    Canvas.Draw(0, 0, Buffer);
end;

procedure TscClientInActivePanel.ShowPanel(AForm: TCustomForm; AColor: TColor;
  AColorAlpha: Byte;
  ABlurAmount: TscBlurAmount);
var
  C: TCanvas;
  PS: TPaintStruct;
begin
  if (AForm = nil) or not AForm.Visible then Exit;
  if (AForm.ClientWidth = 0) or (AForm.ClientHeight = 0) then Exit;
  Visible := False;
  Parent := AForm;
  SetBounds(0, 0, AForm.ClientWidth, AForm.ClientHeight);
  Buffer := TBitmap.Create;
  Buffer.PixelFormat := pf32Bit;
  Buffer.SetSize(AForm.ClientWidth, AForm.ClientHeight);
  Bitmap_ClearAlpha(Buffer, 255);
  C := TCanvas.Create;
  C.Handle := BeginPaint(AForm.Handle, PS);
  Buffer.Canvas.CopyRect(Rect(0, 0, Width, height), C, BoundsRect);
  EndPaint(AForm.Handle, PS);
  C.Handle := 0;
  C.Free;
  if ABlurAmount > 0 then
    Bitmap_Blur(Buffer, ABlurAmount);
  Buffer.Canvas.Brush.Color := AColor;
  scDrawUtils.FillRectWithAlpha(Buffer.Canvas, Rect(0, 0, Width, Height),
    AColorAlpha);
  BringToFront;
  Visible := True;
  RePaint;
end;

procedure TscClientInActivePanel.HidePanel;
begin
  Visible := False;
  Buffer.Free;
  Buffer := nil;
end;


initialization

  StyledFormHookList := TStyledFormHooksList.Create;

finalization

  StyledFormHookList.Free;

end.


