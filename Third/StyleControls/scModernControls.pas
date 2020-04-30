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

unit scModernControls;

{$I scdefine.inc}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
    Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Graphics, Vcl.Themes, Vcl.ImgList,
    scDrawUtils, scImageCollection, scControls, scExtControls,
    Vcl.Buttons, Vcl.ExtCtrls;

const
  DefaultSplitViewCompactWidth = 50;
  DefaultSplitViewCompactHeight = 20;
  DefaultSplitViewOpenedWidth = 250;
  DefaultSplitViewOpenedHeight = 50;
  DefaultSplitViewAnimationStep = 25;
  DefaultSwitchFrameColor = clBtnText;
  DefaultSwitchFrameOnColor = clHighLight;
  DefaultSwitchFramePressedColor = clBtnShadow;
  DefaultSwitchThumbColor = clBtnText;
  DefaultSwitchThumbOnColor = clHighLightText;
  DefaultSwitchThumbPressedColor = clBtnText;
  DefaultSwitchHeight = 20;
  DefaultSwitchWidth = 40;
  DefaultSwitchThumbWidth = 20;
  DefaultAniIndicatorWidth = 64;
  DefaultAniIndicatorSmallWidth = 32;
  DefaultAniIndicatorFrameCount = 61;

type
  TscPageViewer = class;
  TscPageViewerItem = class;
  TscPageViewerPage = class;

  TscPageViewerItem = class(TCollectionItem)
  protected
    FPage: TscPageViewerPage;
    procedure SetPage(const Value: TscPageViewerPage);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Page: TscPageViewerPage read FPage write SetPage;
  end;

  TscPageViewerItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscPageViewerItem;
    procedure SetItem(Index: Integer; Value:  TscPageViewerItem);
  protected
    PageViewer: TscPageViewer;
    DestroyPage: TscPageViewerPage;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(APageViewer: TscPageViewer);
    function Add: TscPageViewerItem;
    function Insert(Index: Integer): TscPageViewerItem;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TscPageViewerItem read GetItem write SetItem; default;
  end;

  TscPageViewerPage = class(TscScrollBox)
  public
    PageViewer: TscPageViewer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  end;

  TscPageViewer = class(TscCustomControl)
  private
    FPageIndex: Integer;
    FPages: TscPageViewerItems;
    FActivePage: TscPageViewerPage;
    FOnChangePage: TNotifyEvent;
    FOnChangingPage: TNotifyEvent;
    FOnCanChangePage: TscCanChangePageEvent;
    procedure SetPages(AValue: TscPageViewerItems);
    procedure SetActivePage(const Value: TscPageViewerPage);
    procedure SetPageIndex(Value: Integer);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    function GetPageIndex(Value: TscPageViewerPage): Integer;
    function GetPageBoundsRect: TRect;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    function CreatePage: TscPageViewerPage;
  published
    property Align;
    property Font;
    property Color;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
    property Pages: TscPageViewerItems read FPages write SetPages;
    property ActivePage: TscPageViewerPage read FActivePage write SetActivePage;
    property OnChangePage: TNotifyEvent read FOnChangePage write FOnChangePage;
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

  TscSplitViewPlacement = (scsvpLeft, scsvpRight, scsvpTop, scsvpBottom);
  TscSplitViewState = (scsvsOpened, scsvsOpening, scsvsClosed, scsvsClosing);
  TscSplitViewDisplayMode = (scsvmOverlay, scsvmDocked);
  TscSplitViewAnimationType = (scsvaInertial, scsvaLinear);

  TscCustomSplitView = class;

  TscSplitViewBackControl = class(TCustomControl)
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    procedure Paint; override;
  end;



  TscCustomSplitView = class(TscPanel)
  private
    FAnimationType: TscSplitViewAnimationType;
    FCompactWidth: Integer;
    FCompactHeight: Integer;
    FOpenedWidth: Integer;
    FOpenedHeight: Integer;
    FPlacement: TscSplitViewPlacement;
    FDisplayMode: TscSplitViewDisplayMode;
    FOnOpening: TNotifyEvent;
    FOnOpened: TNotifyEvent;
    FOnClosing: TNotifyEvent;
    FOnClosed: TNotifyEvent;

    FAnimationStep: Word;
    FAnimation: Boolean;
    FHideControls: Boolean;
    FAnimationTimer: TTimer;
    FGripSize: Integer;

    FOpenedMinWidth: Integer;
    FOpenedMaxWidth: Integer;
    FOpenedMinHeight: Integer;
    FOpenedMaxHeight: Integer;

    procedure OnAnimationTimer(Sender: TObject);
    function GetGripSize: Integer;
    procedure SetCompactWidth(Value: Integer);
    procedure SetCompactHeight(Value: Integer);
    function GetOpened: Boolean;
    procedure SetOpened(Value: Boolean);
    procedure SetOpenedWidth(Value: Integer);
    procedure SetOpenedHeight(Value: Integer);
    procedure SetPlacement(Value: TscSplitViewPlacement);
    procedure SetDisplayMode(Value: TscSplitViewDisplayMode);
    procedure SetGripSize(Value: Integer);
  protected
    FState: TscSplitViewState;
    FBackControl: TscSplitViewBackControl;
    FGripDown: Boolean;
    FVisibleList: TList;
    FGripDownPos: TPoint;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;

    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure ShowAllControls;
    procedure HideAllControls;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetState(Value: TscSplitViewState); virtual;
    procedure DoClosing; dynamic;
    procedure DoClosed; dynamic;
    procedure DoOpening; dynamic;
    procedure DoOpened; dynamic;
    property AnimationStep: Word read FAnimationStep write FAnimationStep;
    property AnimationType: TscSplitViewAnimationType
      read FAnimationType write FAnimationType;
    property CompactWidth: Integer read FCompactWidth write SetCompactWidth;
    property CompactHeight: Integer read FCompactHeight write SetCompactHeight;
    property Opened: Boolean read GetOpened write SetOpened;
    property OpenedWidth: Integer read FOpenedWidth write SetOpenedWidth;
    property OpenedHeight: Integer read FOpenedHeight write SetOpenedHeight;
    property Placement: TscSplitViewPlacement read FPlacement write SetPlacement;
    property DisplayMode: TscSplitViewDisplayMode read
      FDisplayMode write SetDisplayMode;
    property Animation: Boolean read FAnimation write FAnimation;
    property HideControls: Boolean read FHideControls write FHideControls;
    property GripSize: Integer
      read FGripSize write SetGripSize;

    property OpenedMinWidth: Integer
      read FOpenedMinWidth write FOpenedMinWidth;
    property OpenedMaxWidth: Integer
      read FOpenedMaxWidth write FOpenedMaxWidth;
    property OpenedMinHeight: Integer
      read FOpenedMinHeight write FOpenedMinHeight;
    property OpenedMaxHeight: Integer
      read FOpenedMaxHeight write FOpenedMaxHeight;

    property OnClosed: TNotifyEvent read FOnClosed write FOnClosed;
    property OnClosing: TNotifyEvent read FOnClosing write FOnClosing;
    property OnOpened: TNotifyEvent read FOnOpened write FOnOpened;
    property OnOpening: TNotifyEvent read FOnOpening write FOnOpening;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure Open;
    procedure Close;
  end;

  TscSplitView = class(TscCustomSplitView)
  published
    property AnimationStep;
    property AnimationType;
    property BiDiMode;
    property Color;
    property CompactWidth;
    property CompactHeight;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property GripSize;
    property Font;
    property Opened;
    property OpenedWidth;
    property OpenedHeight;
    property OpenedMinWidth;
    property OpenedMaxWidth;
    property OpenedMinHeight;
    property OpenedMaxHeight;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property Placement;
    property HideControls;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Animation;
    property Visible;
    property Width;
    property StyleElements;
    property DisplayMode;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnClosed;
    property OnClosing;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnOpened;
    property OnOpening;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TscSplitViewHistoryItem = class(TCollectionItem)
  private
    FPageIndex: Integer;
    FButton: TscButton;
  public
    constructor Create(Collection: TCollection); override;
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property Button: TscButton read FButton write FButton;
  end;

  TscSplitViewHistoryItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscSplitViewHistoryItem;
    procedure SetItem(Index: Integer; Value:  TscSplitViewHistoryItem);
  protected
    function GetOwner: TPersistent; override;
  public
    FSplitView: TscCustomSplitView;
    constructor Create(ASplitView: TscCustomSplitView);
    function Add: TscSplitViewHistoryItem;
    procedure AddMRUItem(APageIndex: Integer; AButton: TscButton);
    procedure GetLastMRUItem(var APageIndex: Integer; var AButton: TscButton);
    procedure DeleteMRUItem(APageIndex: Integer; AButton: TscButton);
    function Insert(Index: Integer): TscSplitViewHistoryItem;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TscSplitViewHistoryItem
      read GetItem write SetItem; default;
  end;

  TscModernSplitView = class(TscCustomSplitView)
  private
    FPageViewer: TscPageViewer;
    FBottomSplitView: TscSplitView;
    FLastButton: TscButton;
    FOnShowBackButton: TNotifyEvent;
    FOnHideBackButton: TNotifyEvent;
    FOnShowPageViewer: TNotifyEvent;
    FOnHidePageViewer: TNotifyEvent;
    procedure SetPageViewer(Value: TscPageViewer);
    procedure SetBottomSplitView(Value: TscSplitView);
  protected
    FHistoryItems: TscSplitViewHistoryItems;
    procedure SetState(Value: TscSplitViewState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenPage(APageIndex: Integer; AButton: TscButton);
    procedure Back;
    procedure CloseAll;
  published
    property PageViewer: TscPageViewer read
      FPageViewer write SetPageViewer;
    property BottomSplitView: TscSplitView read
      FBottomSplitView write SetBottomSplitView;
    property AnimationStep;
    property BiDiMode;
    property Color;
    property CompactWidth;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Opened;
    property OpenedWidth;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Animation;
    property Visible;
    property Width;
    property StyleElements;

    property OnShowBackButton: TNotifyEvent
      read FOnShowBackButton write FOnShowBackButton;
    property OnHideBackButton: TNotifyEvent
      read FOnHideBackButton write FOnHideBackButton;
    property OnShowPageViewer: TNotifyEvent
      read FOnShowPageViewer write FOnShowPageViewer;
    property OnHidePageViewer: TNotifyEvent
      read FOnHidePageViewer write FOnHidePageViewer;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnClosed;
    property OnClosing;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnOpened;
    property OnOpening;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TscSwitchState = (scswOff, scswOn);
  TscSwitchStyleKind = (scswsStyled, scswsColor, scswsCustomImage);

  TscCustomSwitch = class(TscCustomActiveControl)
  private
    FLoading: Boolean;
    FReadOnly: Boolean;
    FState: TscSwitchState;
    FFrameColor: TColor;
    FFrameOnColor: TColor;
    FThumbColor: TColor;
    FThumbOnColor: TColor;
    FFramePressedColor: TColor;
    FThumbPressedColor: TColor;
    FAnimation: Boolean;
    FCustomImages: TscCustomImageCollection;
    FFrameImageIndex: Integer;
    FFrameOnImageIndex: Integer;
    FFramePressedImageIndex: Integer;
    FThumbImageIndex: Integer;
    FThumbOnImageIndex: Integer;
    FThumbPressedImageIndex: Integer;
    FStyleKind: TscSwitchStyleKind;
    FOnChangeState: TNotifyEvent;
    procedure SetStyleKind(Value: TscSwitchStyleKind);
    procedure SetFrameImageIndex(Value: Integer);
    procedure SetFrameOnImageIndex(Value: Integer);
    procedure SetFramePressedImageIndex(Value: Integer);
    procedure SetThumbImageIndex(Value: Integer);
    procedure SetThumbOnImageIndex(Value: Integer);
    procedure SetThumbPressedImageIndex(Value: Integer);
    procedure OnAnimationTimer(Sender: TObject);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameOnColor(Value: TColor);
    procedure SetFramePressedColor(Value: TColor);
    procedure SetState(Value: TscSwitchState);
    procedure SetThumbColor(Value: TColor);
    procedure SetThumbOnColor(Value: TColor);
    procedure SetThumbPressedColor(Value: TColor);
    procedure SetThumbWidth(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
  protected
    FPressed: Boolean;
    FOldX, FMouseDownX: Integer;
    FThumbRect: TRect;
    FMouseDown: Boolean;
    FResImages: TscImageCollection;
    FThumbWidth: Integer;
    FThumbMoving: Boolean;
    FAnimationTimer: TTimer;
    FCancelAction: Boolean;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure InitResImages; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure ChangeState; virtual;
    procedure DrawSwitch(Canvas: TCanvas); virtual;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure DoChange;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property State: TscSwitchState read FState write SetState;
    property ThumbColor: TColor read FThumbColor write SetThumbColor;
    property ThumbOnColor: TColor read FThumbOnColor write SetThumbOnColor;
    property ThumbPressedColor: TColor read FThumbPressedColor write SetThumbPressedColor;
    property ThumbWidth: Integer read FThumbWidth write SetThumbWidth;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameOnColor: TColor read FFrameOnColor write SetFrameOnColor;
    property FramePressedColor: TColor read FFramePressedColor write SetFramePressedColor;
    property Color;
    property ParentColor default False;
    property Animation: Boolean read FAnimation write FAnimation;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property FrameImageIndex: Integer read FFrameImageIndex write SetFrameImageIndex;
    property FrameOnImageIndex: Integer read FFrameOnImageIndex write SetFrameOnImageIndex;
    property FramePressedImageIndex: Integer
      read FFramePressedImageIndex write SetFramePressedImageIndex;
    property ThumbImageIndex: Integer read FThumbImageIndex write SetThumbImageIndex;
    property ThumbOnImageIndex: Integer read FThumbOnImageIndex write SetThumbOnImageIndex;
    property ThumbPressedImageIndex: Integer
      read FThumbPressedImageIndex write SetThumbPressedImageIndex;
    property StyleKind: TscSwitchStyleKind read FStyleKind write SetStyleKind;
  
    property OnChangeState: TNotifyEvent read FOnChangeState write FOnChangeState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    function IsOn: Boolean;
  end;

  TscSwitch = class(TscCustomSwitch)
  published
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property CustomImages;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FrameColor;
    property FrameOnColor;
    property FramePressedColor;
    property FrameImageIndex;
    property FrameOnImageIndex;
    property FramePressedImageIndex;
    property Height;
    property HelpContext;
    property Hint;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property State;
    property StyleKind;
    property StyleElements;
    property TabOrder;
    property TabStop;
    property ThumbColor;
    property ThumbOnColor;
    property ThumbPressedColor;
    property ThumbImageIndex;
    property ThumbOnImageIndex;
    property ThumbPressedImageIndex;
    property ThumbWidth;
    property Visible;
    property Width;

    property OnChangeState;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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

  TscCustomToggleSwitch = class(TscCustomActiveControl)
  private
    FCanFocused: Boolean;
    FCaptionOn: String;
    FCaptionOff: String;
    FLoading: Boolean;
    FFrameColor: TColor;
    FFrameOnColor: TColor;
    FThumbColor: TColor;
    FThumbOnColor: TColor;
    FFramePressedColor: TColor;
    FThumbPressedColor: TColor;
    FAnimation: Boolean;
    FCustomImages: TscCustomImageCollection;
    FFrameImageIndex: Integer;
    FFrameOnImageIndex: Integer;
    FFramePressedImageIndex: Integer;
    FThumbImageIndex: Integer;
    FThumbOnImageIndex: Integer;
    FThumbPressedImageIndex: Integer;
    FStyleKind: TscSwitchStyleKind;
    FOnChangeState: TNotifyEvent;
    FUseFontColorToStyleColor: Boolean;
    FShowCaption: Boolean;
    FKeyDown: Boolean;
    procedure SetCanFocused(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
    procedure SetCaptionOn(Value: String);
    procedure SetCaptionOff(Value: String);
    procedure SetStyleKind(Value: TscSwitchStyleKind);
    procedure SetFrameImageIndex(Value: Integer);
    procedure SetFrameOnImageIndex(Value: Integer);
    procedure SetFramePressedImageIndex(Value: Integer);
    procedure SetThumbImageIndex(Value: Integer);
    procedure SetThumbOnImageIndex(Value: Integer);
    procedure SetThumbPressedImageIndex(Value: Integer);
    procedure OnAnimationTimer(Sender: TObject);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameOnColor(Value: TColor);
    procedure SetFramePressedColor(Value: TColor);
    procedure SetThumbColor(Value: TColor);
    procedure SetThumbOnColor(Value: TColor);
    procedure SetThumbPressedColor(Value: TColor);
    procedure SetThumbWidth(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
  protected
    FReadOnly: Boolean;
    FSwitchWidth: Integer;
    FSwitchHeight: Integer;
    FFixedMinSize: Boolean;
    FState: TscSwitchState;
    FPressed: Boolean;
    FOldX, FMouseDownX: Integer;
    FThumbRect, FSWRect: TRect;
    FMouseDown: Boolean;
    FResImages: TscImageCollection;
    FThumbWidth: Integer;
    FThumbMoving: Boolean;
    FAnimationTimer: TTimer;
    FCancelAction: Boolean;

    procedure SetState(Value: TscSwitchState); virtual;

    procedure SetSwitchWidth(Value: Integer); virtual;
    procedure SetSwitchHeight(Value: Integer); virtual;

    procedure WMSetFocus(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure WndProc(var Message: TMessage); override;

    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure InitResImages; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure ChangeState; virtual;
    procedure DrawSwitch(Canvas: TCanvas); virtual;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure DoChange;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property CanFocused: Boolean read FCanFocused write SetCanFocused;
    property CaptionOn: String read FCaptionOn write SetCaptionOn;
    property CaptionOff: String read FCaptionOff write SetCaptionOff;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property State: TscSwitchState read FState write SetState;
    property ThumbColor: TColor read FThumbColor write SetThumbColor;
    property ThumbOnColor: TColor read FThumbOnColor write SetThumbOnColor;
    property ThumbPressedColor: TColor read FThumbPressedColor write SetThumbPressedColor;
    property ThumbWidth: Integer read FThumbWidth write SetThumbWidth;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameOnColor: TColor read FFrameOnColor write SetFrameOnColor;
    property FramePressedColor: TColor read FFramePressedColor write SetFramePressedColor;
    property Color;
    property ParentColor default False;
    property Animation: Boolean read FAnimation write FAnimation;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property FrameImageIndex: Integer read FFrameImageIndex write SetFrameImageIndex;
    property FrameOnImageIndex: Integer read FFrameOnImageIndex write SetFrameOnImageIndex;
    property FramePressedImageIndex: Integer
      read FFramePressedImageIndex write SetFramePressedImageIndex;
    property ThumbImageIndex: Integer read FThumbImageIndex write SetThumbImageIndex;
    property ThumbOnImageIndex: Integer read FThumbOnImageIndex write SetThumbOnImageIndex;
    property ThumbPressedImageIndex: Integer
      read FThumbPressedImageIndex write SetThumbPressedImageIndex;
    property ShowCaption: Boolean
      read FShowCaption write SetShowCaption;
    property SwitchWidth: Integer
      read FSwitchWidth write SetSwitchWidth;
    property SwitchHeight: Integer
      read FSwitchHeight write SetSwitchHeight;
    property StyleKind: TscSwitchStyleKind read FStyleKind write SetStyleKind;
    property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
    property OnChangeState: TNotifyEvent read FOnChangeState write FOnChangeState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    function IsOn: Boolean;
  end;


  TscToggleSwitch = class(TscCustomToggleSwitch)
  published
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    property BiDiMode;
    property CaptionOn;
    property CaptionOff;
    property Color;
    property CanFocused;
    property Constraints;
    property CustomImages;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FrameColor;
    property FrameOnColor;
    property FramePressedColor;
    property FrameImageIndex;
    property FrameOnImageIndex;
    property FramePressedImageIndex;
    property Height;
    property HelpContext;
    property Hint;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;

    property SwitchWidth;
    property SwitchHeight;

    property State;
    property StyleKind;
    property StyleElements;
    property ShowCaption;

    property TabOrder;
    property TabStop;
    property ThumbColor;
    property ThumbOnColor;
    property ThumbPressedColor;
    property ThumbImageIndex;
    property ThumbOnImageIndex;
    property ThumbPressedImageIndex;
    property ThumbWidth;
    property Visible;
    property Width;
    property UseFontColorToStyleColor;

    property OnChangeState;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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

  TscRelativePanel = class;
  scERelativePanelException = class(Exception);
  TscRelativePanelControlItem = class(TCollectionItem)
  private
    FControl: TControl;
    FControlWidth: Integer;
    FControlHeight: Integer;
    FAbove: TControl;
    FAlignBottomWith: TControl;
    FAlignBottomWithPanel: Boolean;
    FAlignHorizontalCenterWith: TControl;
    FAlignHorizontalCenterWithPanel: Boolean;
    FAlignLeftWith: TControl;
    FAlignLeftWithPanel: Boolean;
    FAlignRightWith: TControl;
    FAlignRightWithPanel: Boolean;
    FAlignTopWith: TControl;
    FAlignTopWithPanel: Boolean;
    FAlignVerticalCenterWith: TControl;
    FAlignVerticalCenterWithPanel: Boolean;
    FBelow: TControl;
    FLeftOf: TControl;
    FRightOf: TControl;
    procedure SetControl(Value: TControl);
    procedure SetAbove(Value: TControl);
    procedure SetAlignBottomWith(Value: TControl);
    procedure SetAlignBottomWithPanel(Value: Boolean);
    procedure SetAlignHorizontalCenterWith(Value: TControl);
    procedure SetAlignHorizontalCenterWithPanel(Value: Boolean);
    procedure SetAlignLeftWith(Value: TControl);
    procedure SetAlignLeftWithPanel(Value: Boolean);
    procedure SetAlignRightWith(Value: TControl);
    procedure SetAlignRightWithPanel(Value: Boolean);
    procedure SetAlignTopWith(Value: TControl);
    procedure SetAlignTopWithPanel(Value: Boolean);
    procedure SetAlignVerticalCenterWith(Value: TControl);
    procedure SetAlignVerticalCenterWithPanel(Value: Boolean);
    procedure SetBelow(Value: TControl);
    procedure SetLeftOf(Value: TControl);
    procedure SetRightOf(Value: TControl);
    function GetRelativePanel: TscRelativePanel;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure CheckControl(Value: TControl);
    property RelativePanel: TscRelativePanel read GetRelativePanel;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure RestoreControlWidth;
    procedure RestoreControlHeight;
    property ControlWidth: Integer read FControlWidth write FControlWidth;
    property ControlHeight: Integer read FControlHeight write FControlHeight;
  published
    property Control: TControl read FControl write SetControl;
    property Above: TControl read FAbove write SetAbove;
    property AlignBottomWith: TControl read FAlignBottomWith write SetAlignBottomWith;
    property AlignBottomWithPanel: Boolean read FAlignBottomWithPanel write SetAlignBottomWithPanel;
    property AlignHorizontalCenterWith: TControl read FAlignHorizontalCenterWith write SetAlignHorizontalCenterWith;
    property AlignHorizontalCenterWithPanel: Boolean read FAlignHorizontalCenterWithPanel write SetAlignHorizontalCenterWithPanel;
    property AlignLeftWith: TControl read FAlignLeftWith write SetAlignLeftWith;
    property AlignLeftWithPanel: Boolean read FAlignLeftWithPanel write SetAlignLeftWithPanel;
    property AlignRightWith: TControl read FAlignRightWith write SetAlignRightWith;
    property AlignRightWithPanel: Boolean read FAlignRightWithPanel write SetAlignRightWithPanel;
    property AlignTopWith: TControl read FAlignTopWith write SetAlignTopWith;
    property AlignTopWithPanel: Boolean read FAlignTopWithPanel write SetAlignTopWithPanel;
    property AlignVerticalCenterWith: TControl read FAlignVerticalCenterWith write SetAlignVerticalCenterWith;
    property AlignVerticalCenterWithPanel: Boolean read FAlignVerticalCenterWithPanel write SetAlignVerticalCenterWithPanel;
    property Below: TControl read FBelow write SetBelow;
    property LeftOf: TControl read FLeftOf write SetLeftOf;
    property RightOf: TControl read FRightOf write SetRightOf;
  end;

  TscRelativePanelControlCollection = class(TOwnedCollection)
  protected
    function GetControl(Index: Integer): TControl;
    function GetItem(Index: Integer): TscRelativePanelControlItem;
    procedure SetControl(Index: Integer; Value: TControl);
    procedure SetItem(Index: Integer; Value: TscRelativePanelControlItem);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TscRelativePanelControlItem;
    procedure AddControl(AControl: TControl);
    procedure RemoveControl(AControl: TControl);
    function IndexOf(AControl: TControl): Integer;
    function Owner: TscRelativePanel;
    property Controls[Index: Integer]: TControl read GetControl write SetControl;
    property Items[Index: Integer]: TscRelativePanelControlItem read GetItem write SetItem; default;
  end;

  TscCustomRelativePanel = class(TscPanel)
  private
    FControlCollection: TscRelativePanelControlCollection;
    procedure SetControlCollection(const Value: TscRelativePanelControlCollection);
    procedure CMControlChange(var Msg: TCMControlChange); message CM_CONTROLCHANGE;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetControlIndex(AControl: TControl): Integer;
    procedure SetControlIndex(AControl: TControl; Index: Integer);
  published
    property ControlCollection: TscRelativePanelControlCollection read FControlCollection write SetControlCollection;
  end;

  TscRelativePanel = class(TscCustomRelativePanel)
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property StyleElements;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TscActivityIndicatorKind = (scaikPoints, scaikCircle, scaikSectors);

  TscActivityIndicator = class(TscCustomControl)
  private
    FIndicatorColor: TColor;
    FActive: Boolean;
    FKind: TscActivityIndicatorKind;
    FScaled: Boolean;
    function GetFrameCount: Integer;
    procedure SetIndicatorColor(Value: TColor);
    procedure SetKind(Value: TscActivityIndicatorKind);
    procedure SetActive(Value: Boolean);
    procedure OnAnimationTimer(Sender: TObject);
  protected
    FResImages: TscImageCollection;
    FLoading: Boolean;
    FAnimationTimer: TTimer;
    FAnimationFrame: Integer;
    procedure InitResImages;
    procedure Loaded; override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property IndicatorColor: TColor read FIndicatorColor write SetIndicatorColor;
    property Color;
    property Active: Boolean read FActive write SetActive;
    property Kind: TscActivityIndicatorKind read FKind write SetKind;
    property Scaled: Boolean read FScaled write FScaled;
  end;

  TscSizeStyle = (ssAbsolute, ssPercent, ssAuto);

  TscCustomGridPanel = class;
  scEGridPanelException = class(Exception);

  TscCellItem = class(TCollectionItem)
  private
    FSizeStyle: TscSizeStyle;
    FValue: Double;
    FSize: Integer;
    FAutoAdded: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetSizeStyle(Value: TscSizeStyle);
    procedure SetValue(Value: Double);
    property Size: Integer read FSize write FSize;
    property AutoAdded: Boolean read FAutoAdded write FAutoAdded;
  public
    constructor Create(Collection: TCollection); override;
  published
    property SizeStyle: TscSizeStyle read FSizeStyle write SetSizeStyle default ssPercent;
    property Value: Double read FValue write SetValue;
  end;

  TscRowItem = class(TscCellItem);

  TscColumnItem = class(TscCellItem);

  TscCellCollection = class(TOwnedCollection)
  protected
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    function GetItem(Index: Integer): TscCellItem;
    procedure SetItem(Index: Integer; Value: TscCellItem);
    procedure Update(Item: TCollectionItem); override;
  public
    function Owner: TscCustomGridPanel;
    property Items[Index: Integer]: TscCellItem read GetItem write SetItem; default;
  end;

  TscCellSpan = 1..MaxInt;

  TscRowCollection = class(TscCellCollection)
  protected
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    procedure Notify(Item: TCollectionItem; Action: System.Classes.TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TscRowItem;
  end;

  TscColumnCollection = class(TscCellCollection)
  protected
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    procedure Notify(Item: TCollectionItem; Action: System.Classes.TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TscColumnItem;
  end;

  TscControlItem = class(TCollectionItem)
  private
    FControl: TControl;
    FColumn, FRow: Integer;
    FColumnSpan, FRowSpan: TscCellSpan;
    FPushed: Integer;
    function GetGridPanel: TscCustomGridPanel;
    function GetPushed: Boolean;
    procedure SetColumn(Value: Integer);
    procedure SetColumnSpan(Value: TscCellSpan);
    procedure SetControl(Value: TControl);
    procedure SetRow(Value: Integer);
    procedure SetRowSpan(Value: TscCellSpan);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InternalSetLocation(AColumn, ARow: Integer; APushed: Boolean; MoveExisting: Boolean);
    property GridPanel: TscCustomGridPanel read GetGridPanel;
    property Pushed: Boolean read GetPushed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure SetLocation(AColumn, ARow: Integer; APushed: Boolean = False);
  published
    property Column: Integer read FColumn write SetColumn;
    property ColumnSpan: TscCellSpan read FColumnSpan write SetColumnSpan default 1;
    property Control: TControl read FControl write SetControl;
    property Row: Integer read FRow write SetRow;
    property RowSpan: TscCellSpan read FRowSpan write SetRowSpan default 1;
  end;

  TscControlCollection = class(TOwnedCollection)
  protected
    function GetControl(AColumn, ARow: Integer): TControl;
    function GetControlItem(AColumn, ARow: Integer): TscControlItem;
    function GetItem(Index: Integer): TscControlItem;
    procedure SetControl(AColumn, ARow: Integer; Value: TControl);
    procedure SetItem(Index: Integer; Value: TscControlItem);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TscControlItem;
    procedure AddControl(AControl: TControl; AColumn: Integer = -1; ARow: Integer = -1);
    procedure RemoveControl(AControl: TControl);
    function IndexOf(AControl: TControl): Integer;
    function Owner: TscCustomGridPanel;
    property Controls[AColumn, ARow: Integer]: TControl read GetControl write SetControl;
    property ControlItems[AColumn, ARow: Integer] : TscControlItem read GetControlItem;
    property Items[Index: Integer]: TscControlItem read GetItem write SetItem; default;
  end;

  TscExpandStyle = (emAddRows, emAddColumns, emFixedSize);

  TscCustomGridPanel = class(TscPanel)
  private
    FRowCollection: TscRowCollection;
    FColumnCollection: TscColumnCollection;
    FControlCollection: TscControlCollection;
    FRecalcCellSizes: Boolean;
    FExpandStyle: TscExpandStyle;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    function GetCellCount: Integer;
    function GetCellSizes(AColumn, ARow: Integer): TPoint;
    function GetCellRect(AColumn, ARow: Integer): TRect;
    function GetColumnSpanIndex(AColumn, ARow: Integer): Integer;
    function GetRowSpanIndex(AColumn, ARow: Integer): Integer;
    procedure SetColumnCollection(const Value: TscColumnCollection);
    procedure SetControlCollection(const Value: TscControlCollection);
    procedure SetRowCollection(const Value: TscRowCollection);
    procedure RecalcCellDimensions(const Rect: TRect);
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
  protected
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function AutoAddColumn: TscColumnItem;
    function AutoAddRow: TscRowItem;
    function CellToCellIndex(AColumn, ARow: Integer): Integer;
    procedure CellIndexToCell(AIndex: Integer; var AColumn, ARow: Integer);
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Loaded; override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure RemoveEmptyAutoAddColumns;
    procedure RemoveEmptyAutoAddRows;
    procedure UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function IsColumnEmpty(AColumn: Integer): Boolean;
    function IsRowEmpty(ARow: Integer): Boolean;
    procedure UpdateControlsColumn(AColumn: Integer);
    procedure UpdateControlsRow(ARow: Integer);
    property ColumnSpanIndex[AColumn, ARow: Integer]: Integer read GetColumnSpanIndex;
    property CellCount: Integer read GetCellCount;
    property CellSize[AColumn, ARow: Integer]: TPoint read GetCellSizes;
    property CellRect[AColumn, ARow: Integer]: TRect read GetCellRect;
    property ColumnCollection: TscColumnCollection read FColumnCollection write SetColumnCollection;
    property ControlCollection: TscControlCollection read FControlCollection write SetControlCollection;
    property ExpandStyle: TscExpandStyle read FExpandStyle write FExpandStyle default emAddRows;
    property RowCollection: TscRowCollection read FRowCollection write SetRowCollection;
    property RowSpanIndex[AColumn, ARow: Integer]: Integer read GetRowSpanIndex;
  end;

  TscGridPanel = class(TscCustomGridPanel)
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property ColumnCollection;
    property Constraints;
    property ControlCollection;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExpandStyle;
    property Font;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCollection;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property StyleElements;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  System.Math, Vcl.ActnList, scGPUtils;

{$R scModernControls.res}

const
  WM_POSTSTYLECHANGED = WM_USER + 200;
  SInvalidRelativePanelControlItem = 'ControlItem.Control cannot be set to owning RelativePanel';
  SInvalidRelativePanelControl = 'Control is not on this panel';
  SInvalidRelativePanelRelativeItself = 'Control cannot be positioned relative to itself';
  SCantDeleteColumn = 'Can''t delete column with control!';

constructor TscPageViewerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPage := nil;
  if (TscPageViewerItems(Collection).PageViewer <> nil) and
     (csDesigning in  TscPageViewerItems(Collection).PageViewer.ComponentState) and
      not (csLoading in TscPageViewerItems(Collection).PageViewer.ComponentState)
  then
  begin
    FPage := TscPageViewerItems(Collection).PageViewer.CreatePage;
    TscPageViewerItems(Collection).PageViewer.ActivePage := FPage;
  end;
end;

destructor TscPageViewerItem.Destroy;
begin
  if (TscPageViewerItems(Collection).PageViewer <> nil)
     and (csDesigning in  TscPageViewerItems(Collection).PageViewer.ComponentState)
     and not (csLoading in  TscPageViewerItems(Collection).PageViewer.ComponentState)
     and (FPage <> nil)
     and not (csDestroying in TscPageViewerItems(Collection).PageViewer.ComponentState)
  then
    TscPageViewerItems(Collection).DestroyPage := FPage;
  inherited;
end;

procedure TscPageViewerItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TscPageViewerItem
  then
    begin
      FPage := TscPageViewerItem(Source).Page;
    end
end;

procedure TscPageViewerItem.SetPage(const Value: TscPageViewerPage);
begin
  if FPage <> Value then
  begin
    FPage := Value;
    if (FPage <> nil) and (FPage.PageViewer <> nil) then
      FPage.PageViewer.ActivePage := FPage;
  end;
end;

constructor TscPageViewerItems.Create;
begin
  inherited Create(TscPageViewerItem);
  PageViewer := APageViewer;
  DestroyPage := nil;
end;

function TscPageViewerItems.GetOwner: TPersistent;
begin
  Result := PageViewer;
end;

function TscPageViewerItems.Add: TscPageViewerItem;
begin
  Result := TscPageViewerItem(inherited Add);
  if (PageViewer <> nil)
     and not (csDesigning in PageViewer.ComponentState)
     and not (csLoading in PageViewer.ComponentState)
  then
  begin
    Result.Page := PageViewer.CreatePage;
    PageViewer.ActivePage := Result.Page;
  end;

end;

function TscPageViewerItems.Insert(Index: Integer): TscPageViewerItem;
begin
  Result := TscPageViewerItem(inherited Insert(Index));
  if (PageViewer <> nil)
     and not (csDesigning in PageViewer.ComponentState)
     and not (csLoading in PageViewer.ComponentState)
  then
    Result.Page := PageViewer.CreatePage;
end;

procedure TscPageViewerItems.Delete(Index: Integer);
begin
   if (PageViewer <> nil)
      and not (csDesigning in PageViewer.ComponentState)
      and not (csLoading in PageViewer.ComponentState)
      and (Items[Index].FPage <> nil)
  then
    FreeAndNil(Items[Index].FPage);
  inherited Delete(Index);
end;

procedure TscPageViewerItems.Update(Item: TCollectionItem);
begin
  inherited;
  if PageViewer = nil then
    Exit;
  if (DestroyPage <> nil) and
     (csDesigning in PageViewer.ComponentState) and
     not (csLoading in  PageViewer.ComponentState) and
     not (csDestroying in PageViewer.ComponentState)
  then
  begin
    FreeAndNil(DestroyPage);
    GetParentForm(PageViewer).Designer.Modified;
  end;
end;

function TscPageViewerItems.GetItem(Index: Integer):  TscPageViewerItem;
begin
  Result := TscPageViewerItem(inherited GetItem(Index));
end;

procedure TscPageViewerItems.SetItem(Index: Integer; Value:  TscPageViewerItem);
begin
  inherited SetItem(Index, Value);
end;

constructor TscPageViewerPage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  BorderStyle := bsNone;
  ParentFont := False;
  ParentColor := False;
  Align := alClient;
end;

destructor TscPageViewerPage.Destroy;
var
  i, j: Integer;
  B: Boolean;
begin
  if (PageViewer <> nil)
     and not (csLoading in PageViewer.ComponentState)
     and not (csDestroying in PageViewer.ComponentState)
  then
    begin
      j := PageViewer.GetPageIndex(Self);
      if j <> -1
      then
        begin
          PageViewer.Pages[j].Page := nil;
          PageViewer.Pages.Delete(j);
          if PageViewer.PageIndex = j
          then
            begin
              B := False;
              if j < PageViewer.Pages.Count then
              for i := j to PageViewer.Pages.Count - 1 do
              begin
                if (i >= 0) and (i < PageViewer.Pages.Count) then
                begin
                  PageViewer.FPageIndex := -1;
                  PageViewer.PageIndex := i;
                  B := True;
                  Break;
                end;
              end;
              if not B and (j >= 0)
              then
                for i := j downto 0 do
                begin
                  if (i >= 0) and (i < PageViewer.Pages.Count) then
                  begin
                    PageViewer.FPageIndex := -1;
                    PageViewer.PageIndex := i;
                    Break;
                  end;
                end;
            end;
        end
      else
        begin
          if PageViewer.PageIndex > PageViewer.Pages.Count - 1
          then
            PageViewer.PageIndex := PageViewer.Pages.Count - 1
          else
            PageViewer.PageIndex := PageViewer.PageIndex;
        end;
    end;
  inherited;
end;

procedure TscPageViewerPage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if Align = alClient then
    inherited
  else
  begin
    if PageViewer = nil then
      if (Parent <> nil) and (Parent is TscPageViewer) then
        PageViewer := TscPageViewer(Parent);
     if (Parent <> nil) and (PageViewer <> nil)
     then
      begin
         R := PageViewer.GetPageBoundsRect;
         inherited SetBounds(R.Left, R.Top, R.Right, R.Bottom);
      end
    else
      inherited;
  end;
end;

constructor TscPageViewer.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := False;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  FPages := TscPageViewerItems.Create(Self);
  FPageIndex := -1;
  Width := 300;
  Height := 300;
end;

destructor TscPageViewer.Destroy;
begin
  FPages.Free;
  FPages := nil;
  inherited;
end;

procedure TscPageViewer.Paint;
var
  R: TRect;
begin
  if Pages.Count <> 0 then Exit;
  R := Rect(0, 0, Width, Height);
  with Canvas do
  begin
    if seClient in StyleElements then
      Brush.Color := scDrawUtils.GetStyleColor(Color)
    else
      Brush.Color := Color;
    FillRect(R);
    if seClient in StyleElements then
      Brush.Color := scDrawUtils.GetStyleColor(Color)
    else
      Brush.Color := Color;
  end;
end;

procedure TscPageViewer.SetPageIndex;
var
  LPage: TscPageViewerPage;
  LPrevPageIndex: Integer;
  B: Boolean;
begin
  if (Value < 0) or (Value > Pages.Count - 1) then
    Exit;

  if Assigned(FOnCanChangePage) and not (csLoading in ComponentState) then
  begin
    B := True;
    FOnCanChangePage(Value, B);
    if not B then Exit;
  end;

  if (FPageIndex <> Value) and (Value >= 0) and (Value < Pages.Count)
  then
    begin
      LPrevPageIndex := FPageIndex;
      FPageIndex := Value;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangingPage) then FOnChangingPage(Self);
      if (FPageIndex > -1) and (FPages[FPageIndex].Page <> nil)
      then
        begin
          LPage := FPages[FPageIndex].Page;
          LPage.Parent := Self;
          LPage.Visible := True;
          LPage.BringToFront;
          FActivePage := LPage;
          LPage.SetBounds(LPage.Left, LPage.Top, LPage.Width, LPage.Height);
        end;
      if (LPrevPageIndex > -1) and (FPages.Count > LPrevPageIndex) and
         (FPages[LPrevPageIndex].Page <> nil) and
         (FPages[LPrevPageIndex].Page <> nil)
      then
        FPages[LPrevPageIndex].Page.Visible := False;
      if not (csLoading in ComponentState) then
        if Assigned(FOnChangePage) then FOnChangePage(Self);
    end
  else
    begin
      if Pages[Value].FPage <> nil
      then
        if not Pages[Value].FPage.Visible then Pages[Value].FPage.Visible := True;
    end;
end;

procedure TscPageViewer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscPageViewer.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FActivePage) then
    FActivePage := nil;
end;

procedure TscPageViewer.SetPages(AValue: TscPageViewerItems);
begin
  FPages.Assign(AValue);
end;

procedure TscPageViewer.SetActivePage(const Value: TscPageViewerPage);
begin
  if Value <> nil then
    PageIndex := GetPageIndex(Value);
end;

function TscPageViewer.GetPageIndex(Value: TscPageViewerPage): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pages.Count - 1 do
    if Pages[i].Page = Value
    then
       begin
         Result := i;
         Break;
       end;
end;

procedure TscPageViewer.Loaded;
var
  i: Integer;
begin
  inherited;
  if Pages.Count > 0 then
    for i := 0 to Pages.Count - 1 do
    if Pages[i].Page <> nil then
    begin
      Pages[i].Page.PageViewer := Self;
      Pages[i].Page.Visible := Pages[i].Page = FActivePage;
    end;
end;

function TscPageViewer.GetPageBoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

procedure TscPageViewer.WMSIZE(var Message: TWMSIZE);
begin
  inherited;
  try
    if (ActivePage <> nil) and (ActivePage.Align <> alClient) and (Pages.Count > 0) then
      with ActivePage do
        SetBounds(Left, Top, Width, Height);
  except
    on e: EAccessViolation do
    begin
    end
    else
      raise;
  end;
end;

function TscPageViewer.CreatePage: TscPageViewerPage;

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
  LPage: TscPageViewerPage;
  R: TRect;
begin
  LPage := TscPageViewerPage.Create(GetParentForm(Self));
  LPage.Parent := Self;
  LPage.PageViewer := Self;
  R := GetPageBoundsRect;
  LPage.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
  LPage.Name := GetUniqueName('scPageViewerPage%d', GetParentForm(Self));
  ActivePage := LPage;
  Result := LPage;
end;

procedure TscSplitViewBackControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TscSplitViewBackControl.Paint;
begin
end;

constructor TscCustomSplitView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption, csParentBackground] +
    [csOpaque, csCaptureMouse];

  FCanEmpty := False;
  FGripSize := 0;
  FGripDown := False;
  FGripDownPos.X := 0;
  FGripDownPos.Y := 0;
  FOpenedMinWidth := 0;
  FOpenedMaxWidth := 0;
  FOpenedMinHeight := 0;
  FOpenedMaxHeight := 0;

  FAnimationType := scsvaInertial;

  FVisibleList := TList.Create;
  FState := scsvsOpened;
  FDisplayMode := scsvmOverlay;
  FBackControl := nil;
  FHideControls := False;
  FCompactWidth := DefaultSplitViewCompactWidth;
  FOpenedWidth := DefaultSplitViewOpenedWidth;
  FCompactHeight := DefaultSplitViewCompactHeight;
  FOpenedHeight := DefaultSplitViewOpenedHeight;

  Placement := scsvpLeft;

  FAnimationStep := DefaultSplitViewAnimationStep;
  FAnimation := True;

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 15;
  FAnimationTimer.OnTimer := OnAnimationTimer;
end;

destructor TscCustomSplitView.Destroy;
begin
  FAnimationTimer.Free;
  FVisibleList.Free;
  inherited;
end;

function TscCustomSplitView.GetGripSize: Integer;
begin
  Result := Round(FGripSize * FScaleFactor);
end;

procedure TscCustomSplitView.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FAnimationStep := MulDiv(FAnimationStep, M, D);
  CompactWidth := MulDiv(CompactWidth, M, D);
  CompactHeight := MulDiv(CompactHeight, M, D);
  OpenedWidth := MulDiv(OpenedWidth, M, D);
  OpenedHeight := MulDiv(OpenedHeight, M, D);
  OpenedMinWidth := MulDiv(OpenedMinWidth, M, D);
  OpenedMinHeight := MulDiv(OpenedMinHeight, M, D);
  OpenedMaxWidth := MulDiv(OpenedMaxWidth, M, D);
  OpenedMaxHeight := MulDiv(OpenedMaxHeight, M, D);
end;

procedure TscCustomSplitView.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBackControl <> nil then
  begin
    FBackControl.Visible := Visible;
    if Visible then
    begin
      FBackControl.SendToBack;
      BringToFront;
    end;
  end
end;

procedure TscCustomSplitView.WMMouseMove(var Message: TWMMouse);
var
  P: TPoint;
  Offset: Integer;
  W, H: Integer;
begin
  P.X := Message.XPos;
  P.Y := Message.YPos;
  if (GetGripSize > 0) and not (csDesigning in ComponentState) and Opened then
  case FPlacement of
    scsvpLeft:
    begin
      if (P.X >= Width - GetGripSize) and (Cursor <> crHSplit) then
        Cursor := crHSplit
      else
      if (P.X < Width - GetGripSize) and (Cursor <> crDefault) then
        Cursor := crDefault;
      if FGripDown then
      begin
        Offset := P.X - FGripDownPos.X;
        FGripDownPos.X := P.X;
        W := OpenedWidth + Offset;
       if W < Max(FCompactWidth, FOpenedMinWidth)
          then W := Max(FCompactWidth, FOpenedMinWidth);
        if (FOpenedMaxWidth > 0) and (FOpenedMaxWidth > FOpenedMinWidth)
           and (W > FOpenedMaxWidth) then
          W := FOpenedMaxWidth;
        if OpenedWidth <> W then
           OpenedWidth := W;
      end;
    end;
    scsvpRight:
    begin
       if (P.X <= GetGripSize) and (Cursor <> crHSplit) then
        Cursor := crHSplit
      else
      if (P.X > GetGripSize) and (Cursor <> crDefault) then
        Cursor := crDefault;
      if FGripDown then
      begin
        Offset := FGripDownPos.X - P.X;
        W := OpenedWidth + Offset;
        if W < Max(FCompactWidth, FOpenedMinWidth)
          then W := Max(FCompactWidth, FOpenedMinWidth);
        if (FOpenedMaxWidth > 0) and (FOpenedMaxWidth > FOpenedMinWidth) and
           (W > FOpenedMaxWidth) then
          W := FOpenedMaxWidth;
        if OpenedWidth <> W then
           OpenedWidth := W;
      end;
    end;
    scsvpTop:
    begin
      if (P.Y >= Height - GetGripSize) and (Cursor <> crVSplit) then
        Cursor := crVSplit
      else
      if (P.Y < Height - GetGripSize) and (Cursor <> crDefault) then
        Cursor := crDefault;
      if FGripDown then
      begin
        Offset := P.Y - FGripDownPos.Y;
        FGripDownPos.Y := P.Y;
        H := OpenedHeight + Offset;
        if H < Max(FCompactHeight, FOpenedMinHeight)
          then H := Max(FCompactHeight, FOpenedMinHeight);
        if (FOpenedMaxHeight > 0) and (FOpenedMaxHeight > FOpenedMinHeight) and
           (H > FOpenedMaxHeight) then
          H := FOpenedMaxHeight;
        if OpenedHeight <> H then
           OpenedHeight := H;
      end;
    end;
    scsvpBottom:
    begin
      if (P.Y <= GetGripSize) and (Cursor <> crVSplit) then
        Cursor := crVSplit
      else
      if (P.Y > GetGripSize) and (Cursor <> crDefault) then
        Cursor := crDefault;
      if FGripDown then
      begin
        Offset := FGripDownPos.Y - P.Y;
        H := OpenedHeight + Offset;
        if H < Max(FCompactHeight, FOpenedMinHeight)
          then H := Max(FCompactHeight, FOpenedMinHeight);
        if (FOpenedMaxHeight > 0) and (FOpenedMaxHeight > FOpenedMinHeight) and
           (H > FOpenedMaxHeight) then
          H := FOpenedMaxHeight;
        if OpenedHeight <> H then
           OpenedHeight := H;
      end;
    end;
  end;
  inherited;
end;

procedure TscCustomSplitView.WMLButtonUp(var Message: TWMMouse);
begin
  if FGripDown then
  begin
    FGripDown := False;
    ReleaseCapture;
  end
  else
    inherited;
end;

procedure TscCustomSplitView.WMLButtonDown(var Message: TWMMouse);
var
  X, Y: Integer;
begin
  X := Message.XPos;
  Y := Message.YPos;
  FGripDown := False;
  if (GetGripSize > 0) and not (csDesigning in ComponentState) and Opened then
  case FPlacement of
    scsvpLeft:
      FGripDown := X >= Width - GetGripSize;
    scsvpRight:
      FGripDown := X <= GetGripSize;
    scsvpTop:
      FGripDown := Y >= Height - GetGripSize;
    scsvpBottom:
      FGripDown := Y <= GetGripSize;
  end;
  if not FGripDown then
    inherited
  else
  begin
    SetCapture(Handle);
    FGripDownPos.X := X;
    FGripDownPos.Y := Y;
  end;
end;


procedure TscCustomSplitView.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_POSTSTYLECHANGED:
      BringToFront;
    WM_STYLECHANGED:
      PostMessage(Handle, WM_POSTSTYLECHANGED, 0, 0);
  end;
end;

procedure TscCustomSplitView.ShowAllControls;
var
  I: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FVisibleList.Count = 0 then Exit;
    DisableAlign;
    for I := 0 to FVisibleList.Count - 1 do
      TControl(FVisibleList[I]).Visible := True;
    EnableAlign;
    Realign;
    FVisibleList.Clear;
  end;
end;

procedure TscCustomSplitView.HideAllControls;
var
  I: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FVisibleList.Count > 0 then Exit;
    DisableAlign;
    for I := 0 to ControlCount - 1 do
    begin
      if Controls[I].Visible then
      begin
        Controls[I].Visible := False;
        FVisibleList.Add(Pointer(Controls[I]));
      end;
    end;
    EnableAlign;
    ReAlign;
  end;
end;

procedure TscCustomSplitView.Open;
begin
  if FAnimation then
    SetState(scsvsOpening)
  else
    SetState(scsvsOpened);
end;

procedure TscCustomSplitView.Close;
begin
  if FAnimation then
    SetState(scsvsClosing)
  else
    SetState(scsvsClosed);
end;

function TscCustomSplitView.GetOpened: Boolean;
begin
  Result := FState = scsvsOpened;
end;

procedure TscCustomSplitView.SetOpened(Value: Boolean);
begin
  if Value and (FState in [scsvsOpening, scsvsOpened]) then Exit;
  if not Value and (FState in [scsvsClosing, scsvsClosed]) then Exit;
  if FAnimation and not(csDesigning in ComponentState) and not(csLoading in ComponentState) then
  begin
    if Value then
      SetState(scsvsOpening)
    else
      SetState(scsvsClosing);
  end
  else
  begin
    if Value then
      SetState(scsvsOpened)
    else
      SetState(scsvsClosed);
  end;
end;

procedure TscCustomSplitView.SetOpenedWidth(Value: Integer);
begin
  if FOpenedWidth <> Value then
  begin
    FOpenedWidth := Value;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Width, Height);
    if (FDisplayMode = scsvmDocked) and (FBackControl <> nil) then
    begin
      if Opened then
         FBackControl.Width := FOpenedWidth
       else
         FBackControl.Width := FCompactWidth;
    end;
  end;
end;

procedure TscCustomSplitView.SetOpenedHeight(Value: Integer);
begin
  if FOpenedHeight <> Value then
  begin
    FOpenedHeight := Value;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Width, Height);
    if (FDisplayMode = scsvmDocked) and (FBackControl <> nil) then
    begin
      if Opened then
         FBackControl.Height := FOpenedHeight
       else
         FBackControl.Height := FCompactHeight;
    end;
  end;
end;

procedure TscCustomSplitView.SetGripSize(Value: Integer);
begin
  if Value >= 0 then
    FGripSize := Value;
end;

procedure TscCustomSplitView.SetState(Value: TscSplitViewState);
begin
  FState := Value;
  BringToFront;
  if FHideControls then
  if FState in [scsvsClosed, scsvsClosing] then
    HideAllControls
   else
   if FState = scsvsOpened then
    ShowAllControls;

  if FAnimation and not(csDesigning in ComponentState)
     and not(csLoading in ComponentState) then
  begin
    FAnimationTimer.Enabled := FState in [scsvsOpening, scsvsClosing];
    case FState of
      scsvsOpened:
      begin
        UpdateControls;
        DoOpened;
      end;
      scsvsOpening: DoOpening;
      scsvsClosed:  DoClosed;
      scsvsClosing: DoClosing;
    end;
  end
  else
  if (FPlacement = scsvpTop) or (FPlacement = scsvpBottom) then
  begin
    if FState = scsvsOpened then
    begin
      if (FCompactHeight = 0) and not Enabled then
         Enabled := True;

      Height := FOpenedHeight;
      if FPlacement = scsvpBottom then
        Top := Top - FOpenedHeight + FCompactHeight;
      if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
        FBackControl.Height := Height;
      UpdateControls;
      DoOpened;
    end
    else
    begin
      if FPlacement = scsvpBottom then
        Top := Top + Height - FCompactHeight;
      Height := FCompactHeight;
      if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
        FBackControl.Height := Height;

      if (FCompactHeight = 0) and Enabled then
         Enabled := False;

      DoClosed;
    end;
  end
  else
  begin
    if FState = scsvsOpened then
    begin
      if (FCompactWidth = 0) and not Enabled then
         Enabled := True;
      Width := FOpenedWidth;
      if FPlacement = scsvpRight then
        Left := Left - FOpenedWidth + FCompactWidth;
      if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
        FBackControl.Width := Width;
      UpdateControls;
      DoOpened;
    end
    else
    begin
      if FPlacement = scsvpRight then
        Left := Left + Width - FCompactWidth;
      Width := FCompactWidth;
      if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
        FBackControl.Width := Width;

      if (FCompactWidth = 0) and Enabled then
         Enabled := False;

      DoClosed;
    end;
  end;
end;

procedure TscCustomSplitView.DoOpened;
begin
  if Assigned(FOnOpened) and not (csDestroying in ComponentState)
    and not (csLoading in ComponentState)
  then
    FOnOpened(Self);
end;

procedure TscCustomSplitView.OnAnimationTimer(Sender: TObject);
var
  W, NewWidth, H, NewHeight: Integer;
  FAniStep: Word;
begin
  if FState in [scsvsOpened, scsvsClosed] then
  begin
    FAnimationTimer.Enabled := False;
    Exit;
  end;

  FAniStep := FAnimationStep;

  if (FPlacement = scsvpTop) or (FPlacement = scsvpBottom) then
  begin
    if FState = scsvsOpening then
    begin
      if (FCompactHeight = 0) and not Enabled then
         Enabled := True;


        if AnimationType = scsvaInertial then
        begin
          if Height < FOpenedHeight div 3 then
            FAniStep := FOpenedHeight div 2
          else
            FAniStep := Abs(FOpenedHeight - Height) div 6;
          FAniStep := Max(FAniStep, Trunc(FScaleFactor));
        end;

        NewHeight := Height + FAniStep;
        H := Min(NewHeight, FOpenedHeight);
        if FPlacement = scsvpTop then
          SetBounds(Left, Top, Width, H)
        else
          SetBounds(Left, BoundsRect.Bottom - H, Width, H);

        if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
          FBackControl.Height := H;

        if H = FOpenedHeight then
        begin
          if (FCompactHeight = 0) and not Enabled then
             Enabled := True;
          SetState(scsvsOpened);
        end;
    end
  else
  begin
    if AnimationType = scsvaInertial then
    begin
      if Height > FOpenedHeight div 3 then
         FAniStep := (Height - FCompactHeight) div 3
       else
        FAniStep := (Height - FCompactHeight) div 6;
        FAniStep := Max(FAniStep, Trunc(FScaleFactor));
     end;

     NewHeight := Height - FAniStep;
     H := Max(FCompactHeight, Min(NewHeight, FOpenedHeight));
     if FPlacement = scsvpTop then
       SetBounds(Left, Top, Width, H)
     else
       SetBounds(Left, BoundsRect.Bottom - H, Width, H);

     if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
       FBackControl.Height := H;

     if H = FCompactHeight then
      begin
        if (FCompactHeight = 0) and Enabled then
           Enabled := False;
        SetState(scsvsClosed);
      end;
    end;
  end
  else
  begin
    if FState = scsvsOpening then
    begin
      if (FCompactWidth = 0) and not Enabled then
         Enabled := True;

        if AnimationType = scsvaInertial then
        begin
          if Width < FOpenedWidth div 3 then
            FAniStep := FOpenedWidth div 2
          else
            FAniStep := Abs(FOpenedWidth - Width) div 6;
          FAniStep := Max(FAniStep, Trunc(FScaleFactor));
        end;

        NewWidth := Width + FAniStep;
        W := Min(NewWidth, FOpenedWidth);
        if FPlacement = scsvpLeft then
          SetBounds(Left, Top, W, Height)
        else
          SetBounds(BoundsRect.Right - W, Top, W, Height);
        if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
          FBackControl.Width := W;

        if W = FOpenedWidth then
        begin
          if (FCompactWidth = 0) and not Enabled then
             Enabled := True;
          SetState(scsvsOpened);
        end;
    end
    else
    begin
      if AnimationType = scsvaInertial then
      begin
        if Width > FOpenedWidth div 3 then
          FAniStep := (Width - FCompactWidth) div 3
        else
          FAniStep := (Width - FCompactWidth) div 6;
        FAniStep := Max(FAniStep, Trunc(FScaleFactor));
      end;

      NewWidth := Width - FAniStep;
      W := Max(FCompactWidth, Min(NewWidth, FOpenedWidth));

      if FPlacement = scsvpLeft then
        SetBounds(Left, Top, W, Height)
      else
        SetBounds(BoundsRect.Right - W, Top, W, Height);

      if (DisplayMode = scsvmDocked) and (FBackControl <> nil) then
         FBackControl.Width := W;
      if W = FCompactWidth then
      begin
        if (FCompactWidth = 0) and Enabled then
             Enabled := False;
          SetState(scsvsClosed);
        end;
    end;
  end;
end;

procedure TscCustomSplitView.DoOpening;
begin
  if Assigned(FOnOpening) and not (csDestroying in ComponentState)
    and not (csLoading in ComponentState)
  then
    FOnOpening(Self);
end;

procedure TscCustomSplitView.DoClosed;
begin
  if Assigned(FOnClosed) and not (csDestroying in ComponentState)
    and not (csLoading in ComponentState)
  then
    FOnClosed(Self);
end;

procedure TscCustomSplitView.DoClosing;
begin
  if Assigned(FOnClosing) and not (csDestroying in ComponentState)
    and not (csLoading in ComponentState)
  then
    FOnClosing(Self);
end;

procedure TscCustomSplitView.SetCompactWidth(Value: Integer);
begin
  if FCompactWidth <> Value then
  begin
    FCompactWidth := Value;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Width, Height);

    if FBackControl <> nil then
      if (FDisplayMode = scsvmDocked) then
      begin
        if Opened then
           FBackControl.Width := FOpenedWidth
         else
          FBackControl.Width := FCompactWidth;
      end
      else
        FBackControl.Width := FCompactWidth;
  end;
end;

procedure TscCustomSplitView.SetCompactHeight(Value: Integer);
begin
  if FCompactHeight <> Value then
  begin
    FCompactHeight := Value;
    if not (csLoading in ComponentState) then
       SetBounds(Left, Top, Width, Height);
    if FBackControl <> nil then
      if (FDisplayMode = scsvmDocked) then
      begin
        if Opened then
           FBackControl.Height := FOpenedHeight
         else
           FBackControl.Height := FCompactHeight;
      end
      else
        FBackControl.Height := FCompactHeight;
  end;
end;

procedure TscCustomSplitView.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FBackControl <> nil then
  begin
    FBackControl.Parent := AParent;
    FBackControl.SendToBack;
  end;
end;

procedure TscCustomSplitView.SetDisplayMode(Value: TscSplitViewDisplayMode);
begin
  if FDisplayMode <> Value then
  begin
    FDisplayMode := Value;
    Placement := FPlacement;
  end;
end;

procedure TscCustomSplitView.SetPlacement(Value: TscSplitViewPlacement);
begin
  FPlacement := Value;
  Align := alNone;
  if (FPlacement = scsvpTop) or (FPlacement = scsvpBottom) then
  begin
    if FPlacement = scsvpTop then
    begin
      if FBackControl = nil then
        FBackControl := TscSplitViewBackControl.Create(Self);
      if FBackControl <> nil then
      begin
        FBackControl.Parent := Parent;
        FBackControl.SendToBack;
        FBackControl.Align := alTop;
        if FDisplayMode = scsvmOverlay then
          FBackControl.Height := FCompactHeight
        else
        begin
          if Opened then
            FBackControl.Height := FOpenedHeight
          else
            FBackControl.Height := FCompactHeight;
        end;
      end;
      SetBounds(Left, Top, Width, Height);
      Anchors := [akLeft, akTop, akRight];
    end
    else
    begin
      if FBackControl = nil then
        FBackControl := TscSplitViewBackControl.Create(Self);
      if FBackControl <> nil then
      begin
        FBackControl.Parent := Parent;
        FBackControl.SendToBack;
        FBackControl.Align := alBottom;
        if FDisplayMode = scsvmOverlay then
          FBackControl.Height := FCompactHeight
        else
        begin
          if Opened then
            FBackControl.Height := FOpenedHeight
          else
            FBackControl.Height := FCompactHeight;
        end;
      end;
      SetBounds(Left, Top, Width, Height);
      Anchors := [akLeft, akRight, akBottom];
    end;
   end
   else
   if FPlacement = scsvpLeft then
   begin
     if FBackControl = nil then
       FBackControl := TscSplitViewBackControl.Create(Self);
     if FBackControl <> nil then
     begin
       FBackControl.Parent := Parent;
       FBackControl.SendToBack;
       FBackControl.Align := alLeft;
       if FDisplayMode = scsvmOverlay then
          FBackControl.Width := FCompactWidth
        else
        begin
          if Opened then
            FBackControl.Width := FOpenedWidth
          else
            FBackControl.Width := FCompactWidth;
        end;
     end;
     SetBounds(Left, Top, Width, Height);
     Anchors := [akLeft, akTop, akBottom];
   end
   else
   begin
     if FBackControl = nil then
       FBackControl := TscSplitViewBackControl.Create(Self);
     if FBackControl <> nil then
     begin
       FBackControl.Parent := Parent;
       FBackControl.SendToBack;
       FBackControl.Align := alRight;
       if FDisplayMode = scsvmOverlay then
          FBackControl.Width := FCompactWidth
        else
        begin
          if Opened then
            FBackControl.Width := FOpenedWidth
          else
            FBackControl.Width := FCompactWidth;
        end;
     end;
     SetBounds(Left, Top, Width, Height);
     Anchors := [akRight, akTop, akBottom];
   end;
end;

procedure TscCustomSplitView.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
  AHeight: Integer);
begin
  if (FState = scsvsOpening) or (FState = scsvsClosing) or (FBackControl = nil)
  then
    inherited SetBounds(ALeft, ATop, AWidth, AHeight)
  else
  case FPlacement of
    scsvpTop:
    begin
      if FState = scsvsOpened then
        inherited SetBounds(FBackControl.Left, FBackControl.Top,
          FBackControl.Width, FOpenedHeight)
      else
        inherited SetBounds(FBackControl.Left, FBackControl.Top,
          FBackControl.Width, FCompactHeight);
    end;
    scsvpBottom:
    begin
      if FState = scsvsOpened then
        inherited SetBounds(FBackControl.Left,
          FBackControl.Top + FBackControl.Height - FOpenedHeight,
          FBackControl.Width, FOpenedHeight)
      else
        inherited SetBounds(FBackControl.Left, FBackControl.Top,
          FBackControl.Width, FCompactHeight);
    end;
    scsvpLeft:
    begin
     if FState = scsvsOpened then
      inherited SetBounds(FBackControl.Left, FBackControl.Top,
        FOpenedWidth, FBackControl.Height)
     else
      inherited SetBounds(FBackControl.Left, FBackControl.Top,
        FCompactWidth, FBackControl.Height);
    end;
    scsvpRight:
    begin
      if FState = scsvsOpened then
      inherited SetBounds(FBackControl.Left + FBackControl.Width - FOpenedWidth,
        FBackControl.Top,
        FOpenedWidth,
        FBackControl.Height)
     else
      inherited SetBounds(FBackControl.Left, FBackControl.Top,
        FCompactWidth, FBackControl.Height);
    end;
  end;
end;

procedure TscCustomSplitView.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if FHideControls then
    begin
      FVisibleList.Clear;
      if FState = scsvsClosed then
        HideAllControls
      else
        ShowAllControls
    end;
    if Parent <> nil then
      Parent.Realign;
  end;
end;

constructor TscModernSplitView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageViewer := nil;
  FBottomSplitView := nil;
  FLastButton := nil;
  FHistoryItems := TscSplitViewHistoryItems.Create(Self);
end;

destructor TscModernSplitView.Destroy;
begin
  FHistoryItems.Free;
  inherited;
end;

procedure TscModernSplitView.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPageViewer) then
    FPageViewer := nil;
  if (Operation = opRemove) and (AComponent = FBottomSplitView) then
    FBottomSplitView := nil;
  if (Operation = opRemove) and (AComponent = FBackControl) then
    FBackControl := nil;  
end;

procedure TscModernSplitView.SetPageViewer(Value: TscPageViewer);
begin
  FPageViewer := Value;
  if (FPageViewer <> nil) and not (csDesigning in ComponentState) then
  begin
    FPageViewer.Parent := Self.Parent;
    FPageViewer.Visible := False;
    FPageViewer.BringToFront;
  end;
end;

procedure TscModernSplitView.SetBottomSplitView(Value: TscSplitView);
begin
  FBottomSplitView := Value;
  if not (csDesigning in ComponentState) and (FBottomSplitView  <> nil) then
  begin
    FBottomSplitView.Parent := Self.Parent;
    FBottomSplitView.Opened := False;
  end;
end;

procedure TscModernSplitView.SetState(Value: TscSplitViewState);
begin
  if (FBottomSplitView <> nil) and FBottomSplitView.Opened then
    FBottomSplitView.Opened := False;
  inherited SetState(Value);
  if Value = scsvsOpened then
    FHistoryItems.AddMRUItem(-1, nil)
  else
  if Value = scsvsClosed then
    FHistoryItems.DeleteMRUItem(-1, nil);
end;

procedure TscModernSplitView.OpenPage(APageIndex: Integer; AButton: TscButton);
begin
  if FPageViewer = nil then Exit;

  FPageViewer.PageIndex := APageIndex;
  if FPageViewer.Visible and Assigned(FOnShowPageViewer) then
     FOnShowPageViewer(Self);
  FPageViewer.Visible := True;
  FHistoryItems.AddMRUItem(APageIndex, AButton);
  FLastButton := AButton;

  if Assigned(FOnShowBackButton) then
    FOnShowBackButton(Self);
end;

procedure TscModernSplitView.Back;
var
  FPageIndex: Integer;
  FButton: TscButton;
begin
  if FPageViewer = nil then
  begin
    if Opened then
      Opened := False;
    if Assigned(FOnHideBackButton) then
      FOnHideBackButton(Self);
    Exit;
  end;
  if FHistoryItems.Count = 0 then
  begin
    if Assigned(FOnHideBackButton) then
      FOnHideBackButton(Self);
    if FPageViewer.Visible then
    begin
      FPageViewer.Visible := False;
      if Assigned(FOnHidePageViewer) then
        FOnHidePageViewer(Self);
      if FLastButton <> nil then
      begin
        FLastButton.DisableClick := True;
        FLastButton.Down := False;
        FLastButton.DisableClick := False;
        FLastButton := nil;
      end;
    end
    else
    if Opened then
      Opened := False;
  end
  else
  begin
    FHistoryItems.GetLastMRUItem(FPageIndex, FButton);
    if FPageIndex = FPageViewer.PageIndex then
    begin
      Back;
      Exit;
    end;
    if FPageIndex = -1 then
    begin
      if Opened then
        Opened := False;
      if not FPageViewer.Visible then
        if Assigned(FOnHideBackButton) then
          FOnHideBackButton(Self); 
    end
    else
    begin
      if (FPageIndex >= 0) and (FPageIndex < FPageViewer.Pages.Count) then
        FPageViewer.PageIndex := FPageIndex;
      if (FButton <> nil) and (FButton.GroupIndex > 0) then
      begin
        FButton.DisableClick := True;
        FButton.Down := True;
        FButton.DisableClick := False;
        FLastButton := FButton;
      end;
    end;
  end;
end;

procedure TscModernSplitView.CloseAll;
begin
  if Assigned(FOnHideBackButton) then
    FOnHideBackButton(Self);
  if (FPageViewer <> nil) and FPageViewer.Visible then
  begin
    FPageViewer.Visible := False;
    if Assigned(FOnHidePageViewer) then
        FOnHidePageViewer(Self);
  end;
  if FBottomSplitView <> nil then
    FBottomSplitView.Opened := False;
  Opened := False;
  FHistoryItems.Clear;
  if FLastButton <> nil then
  begin
    FLastButton.DisableClick := True;
    FLastButton.Down := False;
    FLastButton.DisableClick := False;
    FLastButton := nil;
  end;
end;

constructor TscSplitViewHistoryItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPageIndex := -1;
  FButton := nil;
end;

constructor TscSplitViewHistoryItems.Create;
begin
  inherited Create(TscSplitViewHistoryItem);
  FSplitView := ASplitView;
end;

function TscSplitViewHistoryItems.GetOwner: TPersistent;
begin
  Result := FSplitView;
end;

function TscSplitViewHistoryItems.Add: TscSplitViewHistoryItem;
begin
  Result := TscSplitViewHistoryItem(inherited Add);
end;

function TscSplitViewHistoryItems.Insert(Index: Integer): TscSplitViewHistoryItem;
begin
  Result := TscSplitViewHistoryItem(inherited Insert(Index));
end;

procedure TscSplitViewHistoryItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TscSplitViewHistoryItems.GetItem(Index: Integer): TscSplitViewHistoryItem;
begin
  Result := TscSplitViewHistoryItem(inherited GetItem(Index));
end;

procedure TscSplitViewHistoryItems.SetItem(Index: Integer; Value: TscSplitViewHistoryItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TscSplitViewHistoryItems.AddMRUItem(APageIndex: Integer; AButton: TscButton);
var
  I, J: Integer;
  Item: TscSplitViewHistoryItem;
begin
  J := - 1;
  for I := 0 to Count - 1 do
  begin
    if APageIndex = Items[I].PageIndex then
    begin
      J := I;
      Break;
    end;
  end;
  if J <> -1 then Delete(J);
  Item := Insert(0);
  Item.PageIndex := APageIndex;
  Item.Button := AButton;
end;

procedure TscSplitViewHistoryItems.DeleteMRUItem(APageIndex: Integer; AButton: TscButton);
var
  I, J: Integer;
begin
  J := - 1;
  for I := 0 to Count - 1 do
  begin
    if APageIndex = Items[I].PageIndex then
    begin
      J := I;
      Break;
    end;
  end;
  if J <> -1 then Delete(J);
end;

procedure TscSplitViewHistoryItems.GetLastMRUItem(var APageIndex: Integer; var AButton: TscButton);
begin
  if Count > 0 then
  begin
    APageIndex := Items[0].PageIndex;
    AButton := Items[0].Button;
    Delete(0);
  end
  else
  begin
    APageIndex := -1;
    AButton := nil;
  end;
end;

constructor TscCustomSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCancelAction := False;
  FLoading := False;
  FResImages := TscImageCollection.Create(Self);

  FCustomImages := nil;
  FFrameImageIndex :=- 1;
  FFrameOnImageIndex := -1;
  FFramePressedImageIndex := -1;
  FThumbImageIndex := -1;
  FThumbOnImageIndex := -1;
  FThumbPressedImageIndex := -1;

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 15;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimation := True;

  TabStop := False;
  FTransparentBackground := True;
  DrawOnBackground := True;

  FMouseDown := False;

  FReadOnly := False;
  FState := scswOff;

  Width := DefaultSwitchWidth;
  Height := DefaultSwitchHeight;
  FThumbWidth := 0;

  FFrameColor := DefaultSwitchFrameColor;
  FFrameOnColor := DefaultSwitchFrameOnColor;
  FFramePressedColor :=  DefaultSwitchFramePressedColor;
  FThumbColor := DefaultSwitchThumbColor;
  FThumbOnColor := DefaultSwitchThumbOnColor;
  FThumbPressedColor := DefaultSwitchThumbPressedColor;
  InitResImages;
end;

destructor TscCustomSwitch.Destroy;
begin
  FResImages.Free;
  FResImages := nil;
  FAnimationTimer.Free;
  inherited;
end;

procedure TscCustomSwitch.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  ThumbWidth := MulDiv(ThumbWidth, M, D);
  InitResImages;
end;

procedure TscCustomSwitch.DoChange;
begin
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    if Assigned(FOnChangeState) then
      FOnChangeState(Self);
end;

procedure TscCustomSwitch.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer);
begin
  if FStyleKind <> scswsCustomImage then
  begin
    if AHeight < 20 then AHeight := 20;
    if AWidth < 40 then AWidth := 40;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscCustomSwitch.InitResImages;
var
  Item: TscImageCollectionItem;
  Scale: String;
  Margin: Integer;
  ScaleF: Double;
begin
  if FResImages = nil then Exit;
  FLoading := True;
  FResImages.Images.Clear;
  ScaleF := 1;
  Scale := '';
  if FScaleFactor > 2 then
  begin
    ScaleF := FScaleFactor / 2;
    Scale := '_200';
  end
  else
  if (FScaleFactor = 2) then
  begin
    ScaleF := 1;
    Scale := '_200';
  end
  else
  if (FScaleFactor > 1.5) and (FScaleFactor < 2) then
  begin
    ScaleF := FScaleFactor / 1.5;
    Scale := '_150';
  end
  else
  if (FScaleFactor = 1.5) then
  begin
    ScaleF := 1;
    Scale := '_150';
  end
  else
  if (FScaleFactor > 1.25) and (FScaleFactor < 1.5) then
  begin
    ScaleF := FScaleFactor / 1.25;
    Scale := '_125';
  end
  else
  if FScaleFactor = 1.25 then
  begin
    Scale := '_125';
    ScaleF := 1;
  end;
  Margin := Round(10 * FScaleFactor);
  Item := FResImages.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SWITCH_FRAME_MASK_OFF' + Scale, ScaleF);
  Item.BitmapOptions.LeftMargin := Margin;
  Item.BitmapOptions.TopMargin := Margin;
  Item.BitmapOptions.BottomMargin := Margin;
  Item.BitmapOptions.RightMargin := Margin;
  Item.DrawStyle := idsStretch;
  Item.CheckBitmapOptions;
  Item.BitmapOptions.DrawOnlyBorder := True;
  Item.Color := FFrameColor;
  Item := FResImages.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SWITCH_FRAME_MASK_ON' + Scale, ScaleF);
  Item.BitmapOptions.LeftMargin := Margin;
  Item.BitmapOptions.TopMargin := Margin;
  Item.BitmapOptions.BottomMargin := Margin;
  Item.BitmapOptions.RightMargin := Margin;
  Item.DrawStyle := idsStretch;
  Item.CheckBitmapOptions;
  Item.Color := FFrameOnColor;
  Item := FResImages.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SWITCH_THUMB_MASK' + Scale, ScaleF);
  Item.BitmapOptions.LeftMargin := Margin;
  Item.BitmapOptions.TopMargin := Margin;
  Item.BitmapOptions.BottomMargin := Margin;
  Item.BitmapOptions.RightMargin := Margin;
  Item.DrawStyle := idsStretch;
  Item.CheckBitmapOptions;
  Item.Color := FThumbColor;
  FLoading := False;
end;

procedure TscCustomSwitch.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscCustomSwitch.SetFrameImageIndex(Value: Integer);
begin
  if FFrameImageIndex <> Value then
  begin
    FFrameImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetFrameOnImageIndex(Value: Integer);
begin
  if FFrameOnImageIndex <> Value then
  begin
    FFrameOnImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetFramePressedImageIndex(Value: Integer);
begin
  if FFramePressedImageIndex <> Value then
  begin
    FFramePressedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetThumbImageIndex(Value: Integer);
begin
  if FThumbImageIndex <> Value then
  begin
    FThumbImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetThumbOnImageIndex(Value: Integer);
begin
  if FThumbOnImageIndex <> Value then
  begin
    FThumbOnImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetThumbPressedImageIndex(Value: Integer);
begin
  if FThumbPressedImageIndex <> Value then
  begin
    FThumbPressedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetStyleKind(Value: TscSwitchStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.OnAnimationTimer(Sender: TObject);
var
  FAnimationStep: Integer;
  FCanChangeState: Boolean;
begin
  if FCancelAction then
  begin
    if (FState = scswOn) then
    begin
      if FThumbRect.Right < Width - Width div 3 then
        FAnimationStep := (Width - FThumbRect.Width) div 3
      else
        FAnimationStep := (Width - FThumbRect.Right) div 6;
    end
    else
    begin
      if FThumbRect.Left > Width div 3 then
        FAnimationStep := (Width - FThumbRect.Width) div 3
      else
        FAnimationStep := FThumbRect.Left div 6
    end;
    if FAnimationStep < 1 then
      FAnimationStep := 1;
    if FState = scswOn then
      OffsetRect(FThumbRect, FAnimationStep, 0)
    else
      OffsetRect(FThumbRect, -FAnimationStep, 0);
    if FThumbRect.Left < 0 then
    begin
      OffsetRect(FThumbRect, -FThumbRect.Left, 0);
      FAnimationTimer.Enabled := False;
      FThumbMoving := False;
      FCancelAction := False;
    end
    else
    if FThumbRect.Right > Width then
    begin
      OffsetRect(FThumbRect, -(FThumbRect.Right - Width), 0);
      FAnimationTimer.Enabled := False;
      FThumbMoving := False;
      FCancelAction := False;
    end;
    RePaintControl;
    Exit;
  end;
  if (FState = scswOff) then
  begin
    if FThumbRect.Right < Width - Width div 3 then
      FAnimationStep := (Width - FThumbRect.Width) div 3
    else
      FAnimationStep := (Width - FThumbRect.Right) div 6;
  end
  else
  begin
    if FThumbRect.Left > Width div 3 then
      FAnimationStep := (Width - FThumbRect.Width) div 3
    else
      FAnimationStep := FThumbRect.Left div 6
  end;
  if FAnimationStep < 1 then
    FAnimationStep := 1;
  FCanChangeState := False;
  if FState = scswOff then
    OffsetRect(FThumbRect, FAnimationStep, 0)
  else
    OffsetRect(FThumbRect, -FAnimationStep, 0);
  if FThumbRect.Left < 0 then
  begin
    OffsetRect(FThumbRect, -FThumbRect.Left, 0);
    FAnimationTimer.Enabled := False;
    FCanChangeState := True;
    FThumbMoving := False;
  end
  else
  if FThumbRect.Right > Width then
  begin
    OffsetRect(FThumbRect, -(FThumbRect.Right - Width), 0);
    FAnimationTimer.Enabled := False;
    FCanChangeState := True;
    FThumbMoving := False;
  end;
  if FCanChangeState then
  begin
    if FState = scswOff then 
      State := scswOn
    else 
      State := scswOff;
  end
  else
    RePaintControl;
end;

procedure TscCustomSwitch.CreateWnd;
begin
  inherited;
end;

procedure TscCustomSwitch.Loaded;
begin
  inherited;
end;

procedure TscCustomSwitch.DrawSwitch(Canvas: TCanvas);
var
  R: TRect;
  {$IFDEF VER300_UP}
  Details: TThemedElementDetails;
  {$ENDIF}
  ThumbR: TRect;
  FrmColor, ThmbColor: TColor;
  FrmIndex, ThmbIndex: Integer;
  TW: Integer;
  TempState: TscSwitchState;
begin
  R := Rect(0, 0, Width, Height);

  TW := FThumbWidth;
  if TW = 0 then TW := Width div 2;

  if FThumbMoving and (FThumbRect.Width > 0) then
    ThumbR := FThumbRect
  else
  begin
    ThumbR := R;
    if FState = scswOff then
      ThumbR.Right := ThumbR.Left + TW
    else
      ThumbR.Left := ThumbR.Right - TW;
    FThumbRect := ThumbR;
  end;

  TempState := FState;
  if (FAnimationTimer <> nil) and (FAnimationTimer.Enabled) and not FPressed and not FCancelAction then
  begin
    if TempState = scswOff then
      TempState := scswOn
    else
      TempState := scswOff;
  end;

  if (FStyleKind = scswsCustomImage) and (FCustomImages <> nil) then
  begin
    if FPressed and (FFramePressedImageIndex >= 0) then
      FrmIndex := FFramePressedImageIndex
    else
    if TempState = scswOn then
      FrmIndex := FFrameOnImageIndex
    else
      FrmIndex:= FFrameImageIndex;
    if FCustomImages.IsIndexAvailable(FrmIndex) then
      FCustomImages.Draw(Canvas, R, FrmIndex, FScaleFactor);
    if FPressed and (FThumbPressedImageIndex >= 0) and FThumbMoving then
      ThmbIndex := FThumbPressedImageIndex
    else
    if TempState = scswOn then
      ThmbIndex := FThumbOnImageIndex
    else
      ThmbIndex:= FThumbImageIndex;
    if FCustomImages.IsIndexAvailable(ThmbIndex) then
      FCustomImages.Draw(Canvas, ThumbR, ThmbIndex, FScaleFactor);
    Exit;
  end;

  {$IFDEF VER300_UP}
  if TStyleManager.IsCustomStyleActive and (FStyleKind = scswsStyled) then
  begin
    // Draw Track
    case TempState of
      scswOff:
        begin
          if Enabled then
            Details := StyleServices.GetElementDetails(ttsTrackOffNormal)
          else
            Details := StyleServices.GetElementDetails(ttsTrackOffDisabled);
        end;

      scswOn:
        begin
          if Enabled then
            Details := StyleServices.GetElementDetails(ttsTrackOnNormal)
          else
            Details := StyleServices.GetElementDetails(ttsTrackOnDisabled);
        end;
    end;

    StyleServices.DrawElement(Canvas.Handle, Details, R);

    // Draw Thumb
    if Enabled then
      Details := StyleServices.GetElementDetails(ttsThumbNormal)
    else
      Details := StyleServices.GetElementDetails(ttsThumbDisabled);

    StyleServices.DrawElement(Canvas.Handle, Details, ThumbR);
  end
  else
  {$ENDIF}
  begin
    if (FResImages = nil) or (FResImages.Images.Count = 0) then Exit;
    if FPressed and (FFramePressedColor <> clNone) then
    begin
      FrmColor := scDrawUtils.GetStyleColor(FFramePressedColor);
      FResImages.Images[1].Color := FrmColor;
      FResImages.DrawBitmap(Canvas, R, 1);
    end
    else
    if TempState = scswOn then
    begin
      FrmColor := scDrawUtils.GetStyleColor(FFrameOnColor);
      FResImages.Images[1].Color := FrmColor;
      FResImages.DrawBitmap(Canvas, R, 1);
    end
    else
    begin
      FrmColor := scDrawUtils.GetStyleColor(FFrameColor);
      FResImages.Images[0].Color := FrmColor;
      FResImages.DrawBitmap(Canvas, R, 0);
    end;

    if FPressed and (FThumbPressedColor <> clNone) then
      ThmbColor := scDrawUtils.GetStyleColor(FThumbPressedColor)
    else
    if TempState = scswOn then
      ThmbColor := scDrawUtils.GetStyleColor(FThumbOnColor)
    else
      ThmbColor := scDrawUtils.GetStyleColor(FThumbColor);
    FResImages.Images[2].Color := ThmbColor;
    FResImages.DrawBitmap(Canvas, ThumbR, 2);
  end;
end;

procedure TscCustomSwitch.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
begin
  if FLoading then Exit;
  DrawSwitch(ACanvas);
end;

procedure TscCustomSwitch.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Offset: Integer;
begin
  inherited;
  if Self.ReadOnly then Exit;

  if FThumbMoving and not FAnimationTimer.Enabled then
  begin
    Offset := X - FOldX;
    FOldX := X;
    OffsetRect(FThumbRect, Offset, 0);
    if FThumbRect.Left < 0 then
      OffsetRect(FThumbRect, -FThumbRect.Left, 0)
    else
      if FThumbRect.Right > Width then
        OffsetRect(FThumbRect, -(FThumbRect.Right - Width), 0);
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Self.ReadOnly then Exit;

  if (Button <> mbLeft) or not Enabled then Exit;
  if FThumbMoving and (FAnimationTimer <> nil) and FAnimationTimer.Enabled then Exit;

  if FStyleKind = scswsCustomImage then
    FPressed := True
  else
    FPressed := FFramePressedColor <> clNone;

  if FThumbRect.Contains(Point(X, Y)) then
  begin
    FThumbMoving := True;
    FOldX := X;
    FMouseDownX := X;
    RePaintControl;
  end
  else
  begin
    RePaintControl;
    FMouseDown := True;
  end;
end;

procedure TscCustomSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if Self.ReadOnly then Exit;

  FPressed := False;
  if FThumbMoving and (FAnimationTimer <> nil) and FAnimationTimer.Enabled then Exit;

  if FThumbMoving and (FMouseDownX <> X) then
  begin
    FThumbMoving := False;
    if (FThumbRect.Left + FThumbRect.Width div 2 >= Width div 2) and (State = scswOff) then
    begin
      ChangeState;
      FMouseDown := False;
      //State := scswOn;
    end
    else
    if (FThumbRect.Left + FThumbRect.Width div 2 <= Width div 2) and (State = scswOn) then
    begin
      ChangeState;
      FMouseDown := False;
      //State := scswOff;
    end
    else
    if FAnimation then
    begin
      FCancelAction := True;
      FThumbMoving := True;
      FAnimationTimer.Enabled := True;
      FMouseDown := False;
    end
    else
      RePaintControl;
  end
  else
  if FMouseDown or (FThumbMoving and (FMouseDownX = X))  then
  begin
    R := Rect(0, 0, Width, Height);
    FThumbMoving := False;
    if R.Contains(Point(X, Y)) then
      ChangeState;
    FMouseDown := False;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.ChangeState;
begin
  if FMouseDown and FReadOnly then
    Exit;
  if FAnimation and not(csDesigning in ComponentState)
     and not(csLoading in ComponentState) then
  begin
    FThumbMoving := True;
    FAnimationTimer.Enabled := True;
  end
  else
  case State of
    scswOff: State := scswOn;
    scswOn:  State := scswOff;
  end;
end;

procedure TscCustomSwitch.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    if FFrameColor = clNone then
      FFrameColor := DefaultSwitchFrameColor;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetFrameOnColor(Value: TColor);
begin
  if FFrameOnColor <> Value then
  begin
    FFrameOnColor := Value;
    if FFrameOnColor = clNone then
      FFrameOnColor := DefaultSwitchFrameColor;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetFramePressedColor(Value: TColor);
begin
  if FFramePressedColor <> Value then
  begin
    FFramePressedColor := Value;
    RePaintControl;
  end;
end;

function TscCustomSwitch.IsOn: Boolean;
begin
  Result := FState = scswOn;
end;

procedure TscCustomSwitch.SetState(Value: TscSwitchState);
begin
  if FState <> Value then
  begin
    FState := Value;
    RePaintControl;
    DoChange;
  end;
end;

procedure TscCustomSwitch.SetThumbColor(Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    if FThumbColor = clNone then
      FThumbColor := DefaultSwitchThumbColor;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetThumbOnColor(Value: TColor);
begin
  if FThumbOnColor <> Value then
  begin
    FThumbOnColor := Value;
    if FThumbOnColor = clNone then
      FThumbOnColor := DefaultSwitchThumbColor;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetThumbPressedColor(Value: TColor);
begin
  if FThumbPressedColor <> Value then
  begin
    FThumbPressedColor := Value;
    RePaintControl;
  end;
end;

procedure TscCustomSwitch.SetThumbWidth(Value: Integer);
begin
  if FThumbWidth <> Value then
  begin
    FThumbWidth := Value;
    RePaintControl;
  end;
end;

constructor TscRelativePanelControlItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TscRelativePanelControlItem.Destroy;
var
  FCtrl: TControl;
begin
  if Assigned(FControl) and
     not (csLoading in RelativePanel.ComponentState) and
     not (csUpdating in RelativePanel.ComponentState) and
     not (csDestroying in RelativePanel.ComponentState) then
  begin
    FCtrl := FControl;
    FControl := nil;
    RelativePanel.RemoveControl(FCtrl);
    FCtrl.Free;
  end;
  inherited;
end;

procedure TscRelativePanelControlItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TscRelativePanelControlItem then
  begin
    TscRelativePanelControlItem(Dest).FControl := Self.Control;
    TscRelativePanelControlItem(Dest).FAbove := Self.Above;
    TscRelativePanelControlItem(Dest).FBelow := Self.Below;
    TscRelativePanelControlItem(Dest).FLeftOf := Self.LeftOf;
    TscRelativePanelControlItem(Dest).FRightOf := Self.RightOf;
    TscRelativePanelControlItem(Dest).FAlignBottomWith := Self.AlignBottomWith;
    TscRelativePanelControlItem(Dest).FAlignBottomWithPanel := Self.AlignBottomWithPanel;
    TscRelativePanelControlItem(Dest).FAlignHorizontalCenterWith := Self.AlignHorizontalCenterWith;
    TscRelativePanelControlItem(Dest).FAlignHorizontalCenterWithPanel := Self.AlignHorizontalCenterWithPanel;
    TscRelativePanelControlItem(Dest).FAlignLeftWith := Self.AlignLeftWith;
    TscRelativePanelControlItem(Dest).FAlignLeftWithPanel := Self.AlignLeftWithPanel;
    TscRelativePanelControlItem(Dest).FAlignRightWith := Self.AlignRightWith;
    TscRelativePanelControlItem(Dest).FAlignRightWithPanel := Self.AlignRightWithPanel;
    TscRelativePanelControlItem(Dest).FAlignTopWith := Self.AlignTopWith;
    TscRelativePanelControlItem(Dest).FAlignTopWithPanel := Self.AlignTopWithPanel;
    TscRelativePanelControlItem(Dest).FAlignVerticalCenterWith := Self.AlignVerticalCenterWith;
    TscRelativePanelControlItem(Dest).FAlignVerticalCenterWithPanel := Self.AlignVerticalCenterWithPanel;
    TscRelativePanelControlItem(Dest).Changed(False);
  end;
end;

function TscRelativePanelControlItem.GetDisplayName: string;
begin
  if (FControl <> nil) and (FControl.Name <> '') then
    Result := FControl.Name
  else
    Result := inherited;
end;

function TscRelativePanelControlItem.GetRelativePanel: TscRelativePanel;
var
  Owner: TscRelativePanelControlCollection;
begin
  Owner := TscRelativePanelControlCollection(GetOwner);
  if Owner <> nil then
    Result := Owner.Owner
  else
    Result := nil;
end;

procedure TscRelativePanelControlItem.RestoreControlWidth;
begin
  if FControlWidth <> 0 then
  begin
    FControl.Width := FControlWidth;
    FControlWidth := 0;
  end;
end;

procedure TscRelativePanelControlItem.RestoreControlHeight;
begin
  if FControlHeight <> 0 then
  begin
    FControl.Height := FControlHeight;
    FControlHeight := 0;
  end;
end;

procedure TscRelativePanelControlItem.SetControl(Value: TControl);
begin
  if FControl <> Value then
  begin
    if Value = RelativePanel then
      raise scERelativePanelException.Create(SInvalidRelativePanelControlItem);
    FControl := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.CheckControl(Value: TControl);
begin
  if (Value <> nil) and (Value.Parent <> nil) then
  begin
    if (Value.Parent <> RelativePanel) then
      raise scERelativePanelException.Create(SInvalidRelativePanelControl)
    else if (Value = FControl) then
      raise scERelativePanelException.Create(SInvalidRelativePanelRelativeItself);
  end;
end;

procedure TscRelativePanelControlItem.SetAbove(Value: TControl);
begin
  if FAbove <> Value then
  begin
    CheckControl(Value);
    FAbove := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignBottomWith(Value: TControl);
begin
  if FAlignBottomWith <> Value then
  begin
    CheckControl(Value);
    FAlignBottomWith := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignBottomWithPanel(Value: Boolean);
begin
  if FAlignBottomWithPanel <> Value then
  begin
    FAlignBottomWithPanel := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignHorizontalCenterWith(Value: TControl);
begin
  if FAlignHorizontalCenterWith <> Value then
  begin
    CheckControl(Value);
    FAlignHorizontalCenterWith := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignHorizontalCenterWithPanel(Value: Boolean);
begin
  if FAlignHorizontalCenterWithPanel <> Value then
  begin
    FAlignHorizontalCenterWithPanel := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignLeftWith(Value: TControl);
begin
  if FAlignLeftWith <> Value then
  begin
    CheckControl(Value);
    FAlignLeftWith := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignLeftWithPanel(Value: Boolean);
begin
  if FAlignLeftWithPanel <> Value then
  begin
    FAlignLeftWithPanel := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignRightWith(Value: TControl);
begin
  if FAlignRightWith <> Value then
  begin
    CheckControl(Value);
    FAlignRightWith := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignRightWithPanel(Value: Boolean);
begin
  if FAlignRightWithPanel <> Value then
  begin
    FAlignRightWithPanel := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignTopWith(Value: TControl);
begin
  if FAlignTopWith <> Value then
  begin
    CheckControl(Value);
    FAlignTopWith := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignTopWithPanel(Value: Boolean);
begin
  if FAlignTopWithPanel <> Value then
  begin
    FAlignTopWithPanel := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignVerticalCenterWith(Value: TControl);
begin
  if FAlignVerticalCenterWith <> Value then
  begin
    CheckControl(Value);
    FAlignVerticalCenterWith := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetAlignVerticalCenterWithPanel(Value: Boolean);
begin
  if FAlignVerticalCenterWithPanel <> Value then
  begin
    FAlignVerticalCenterWithPanel := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetBelow(Value: TControl);
begin
  if FBelow <> Value then
  begin
    CheckControl(Value);
    FBelow := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetLeftOf(Value: TControl);
begin
  if FLeftOf <> Value then
  begin
    CheckControl(Value);
    FLeftOf := Value;
    Changed(False);
  end;
end;

procedure TscRelativePanelControlItem.SetRightOf(Value: TControl);
begin
  if FRightOf <> Value then
  begin
    CheckControl(Value);
    FRightOf := Value;
    Changed(False);
  end;
end;

constructor TscRelativePanelControlCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TscRelativePanelControlItem);
end;

function TscRelativePanelControlCollection.Add: TscRelativePanelControlItem;
begin
  Result := TscRelativePanelControlItem(inherited Add);
end;

procedure TscRelativePanelControlCollection.AddControl(AControl: TControl);
var
  Item: TscRelativePanelControlItem;
begin
  if IndexOf(AControl) < 0 then
  begin
    Item := Add;
    Item.Control := AControl;
  end;
end;

procedure TscRelativePanelControlCollection.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].Above = AControl then
      Items[I].Above := nil;
    if Items[I].AlignBottomWith = AControl then
      Items[I].AlignBottomWith := nil;
    if Items[I].AlignHorizontalCenterWith = AControl then
      Items[I].AlignHorizontalCenterWith := nil;
    if Items[I].AlignLeftWith = AControl then
      Items[I].AlignLeftWith := nil;
    if Items[I].AlignRightWith = AControl then
      Items[I].AlignRightWith := nil;
    if Items[I].AlignTopWith = AControl then
      Items[I].AlignTopWith := nil;
    if Items[I].AlignVerticalCenterWith = AControl then
      Items[I].AlignVerticalCenterWith := nil;
    if Items[I].Below = AControl then
      Items[I].Below := nil;
    if Items[I].LeftOf = AControl then
      Items[I].LeftOf := nil;
    if Items[I].RightOf = AControl then
      Items[I].RightOf := nil;
  end;

  for I := Count - 1 downto 0 do
  begin
    if Items[I].Control = AControl then
    begin
      Items[I].Control := nil;
      Delete(I);
      Exit;
    end;
  end;
end;

function TscRelativePanelControlCollection.GetItem(Index: Integer): TscRelativePanelControlItem;
begin
  Result := TscRelativePanelControlItem(inherited GetItem(Index));
end;

function TscRelativePanelControlCollection.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if TscRelativePanelControlItem(Items[Result]).Control = AControl then
      Exit;
  end;
  Result := -1;
end;

function TscRelativePanelControlCollection.Owner: TscRelativePanel;
begin
  Result := TscRelativePanel(GetOwner);
end;

function TscRelativePanelControlCollection.GetControl(Index: Integer): TControl;
var
  Item: TscRelativePanelControlItem;
begin
  Item := Items[Index];
  if Item <> nil then
    Result := Item.Control
  else
    Result := nil;
end;

procedure TscRelativePanelControlCollection.SetControl(Index: Integer; Value: TControl);
var
  Item: TscRelativePanelControlItem;
begin
  if Owner <> nil then
  begin
    Item := Items[Index];
    Item.Control := Value;
  end;
end;

procedure TscRelativePanelControlCollection.SetItem(Index: Integer; Value: TscRelativePanelControlItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TscRelativePanelControlCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner <> nil then
  begin
    Owner.RePaintControl;
    Owner.Realign;
  end;
end;

constructor TscCustomRelativePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanEmpty := False;
  FControlCollection := TscRelativePanelControlCollection.Create(Self);
end;

destructor TscCustomRelativePanel.Destroy;
begin
  FControlCollection.Free;
  inherited;
end;

procedure TscCustomRelativePanel.AlignControls(AControl: TControl; var Rect: TRect);
var
  I: Integer;
  Item: TscRelativePanelControlItem;

  procedure AlignControlWithPanel;
  var
    X, Y, W, H: Integer;
  begin
    if Item.AlignLeftWithPanel then
    begin
      if Item.AlignRightWithPanel then
      begin
        Item.ControlWidth := Item.Control.Width;
        W := Rect.Width - Item.Control.Margins.Left - Item.Control.Margins.Right;
        Item.Control.SetBounds(Rect.Left + Item.Control.Margins.Left,
          Item.Control.Top, W, Item.Control.Height);
      end
      else
      begin
        Item.RestoreControlWidth;
        Item.Control.SetBounds(Rect.Left + Item.Control.Margins.Left, Item.Control.Top,
          Item.Control.Width, Item.Control.Height);
      end;
    end
    else if Item.AlignRightWithPanel then
    begin
      Item.RestoreControlWidth;
      Item.Control.SetBounds(Rect.Right - Item.Control.Width - Item.Control.Margins.Right, Item.Control.Top,
        Item.Control.Width, Item.Control.Height);
    end;

    if Item.AlignTopWithPanel then
    begin
      if Item.AlignBottomWithPanel then
      begin
        Item.ControlHeight := Item.Control.Height;
        H := Rect.Height - Item.Control.Margins.Top - Item.Control.Margins.Bottom;
        Item.Control.SetBounds(Item.Control.Left, Rect.Top + Item.Control.Margins.Top,
          Item.Control.Width, H);
      end
      else
        Item.RestoreControlHeight;
      Item.Control.SetBounds(Item.Control.Left, Rect.Top + Item.Control.Margins.Top,
        Item.Control.Width,  Item.Control.Height);
    end
    else if Item.AlignBottomWithPanel then
    begin
      Item.RestoreControlHeight;
      Item.Control.SetBounds(Item.Control.Left, Rect.Bottom - Item.Control.Height - Item.Control.Margins.Bottom,
        Item.Control.Width, Item.Control.Height);
    end;

    if Item.AlignHorizontalCenterWithPanel then
    begin
      X := (Rect.Width - Item.Control.Width) div 2;
      Item.Control.SetBounds(X, Item.Control.Top, Item.Control.Width, Item.Control.Height);
    end;

    if Item.AlignVerticalCenterWithPanel then
    begin
      Y := (Rect.Height - Item.Control.Height) div 2;
      Item.Control.SetBounds(Item.Control.Left, Y, Item.Control.Width, Item.Control.Height);
    end;
  end;

  procedure AlignControlWithControl;
  var
    X, Y, Margin, W, H: Integer;
    R: TRect;
  begin
    if Item.AlignLeftWith <> nil then
    begin
      if Item.AlignRightWith <> nil then
      begin
        R := Item.AlignRightWith.BoundsRect;
        W := R.Right - Item.AlignLeftWith.Left;
        Item.Control.SetBounds(Item.AlignLeftWith.Left, Item.Control.Top, W, Item.Control.Height);
      end
      else
      begin
        Item.Control.SetBounds(Item.AlignLeftWith.Left, Item.Control.Top, Item.Control.Width, Item.Control.Height);
      end;
    end
    else if Item.AlignRightWith <> nil then
    begin
      R := Item.AlignRightWith.BoundsRect;
      X := R.Right - Item.Control.Width;
      Item.Control.SetBounds(X, Item.Control.Top, Item.Control.Width, Item.Control.Height);
    end;

    if Item.AlignTopWith <> nil then
    begin
      if Item.AlignBottomWith <> nil then
      begin
        R := Item.AlignBottomWith.BoundsRect;
        H := R.Bottom - Item.AlignTopWith.Top;
        Item.Control.SetBounds(Item.Control.Left, Item.AlignTopWith.Top, Item.Control.Width, H);
      end
      else
      begin
        Item.Control.SetBounds(Item.Control.Left, Item.AlignTopWith.Top, Item.Control.Width, Item.Control.Height);
      end;
    end
    else if Item.AlignBottomWith <> nil then
    begin
      R := Item.AlignBottomWith.BoundsRect;
      Y := R.Bottom - Item.Control.Height;
      Item.Control.SetBounds(Item.Control.Left, Y, Item.Control.Width, Item.Control.Height);
    end;

    if Item.LeftOf <> nil then
    begin
      Margin := Item.LeftOf.Margins.Left + Item.Control.Margins.Right;
      Item.Control.SetBounds(Item.LeftOf.Left - Item.Control.Width - Margin, Item.Control.Top, Item.Control.Width,
        Item.Control.Height);
    end
    else if Item.RightOf <> nil then
    begin
      Margin := Item.RightOf.Margins.Right + Item.Control.Margins.Left;
      Item.Control.SetBounds(Item.RightOf.BoundsRect.Right + Margin, Item.Control.Top, Item.Control.Width,
        Item.Control.Height);
    end;

    if Item.Above <> nil then
    begin
      Margin := Item.Above.Margins.Top + Item.Control.Margins.Bottom;
      Item.Control.SetBounds(Item.Control.Left, Item.Above.Top - Item.Control.Height - Margin, Item.Control.Width,
        Item.Control.Height);
    end
    else if Item.Below <> nil then
    begin
      Margin := Item.Below.Margins.Bottom + Item.Control.Margins.Top;
      Item.Control.SetBounds(Item.Control.Left, Item.Below.BoundsRect.Bottom + Margin, Item.Control.Width,
        Item.Control.Height);
    end;

    if Item.AlignHorizontalCenterWith <> nil then
    begin
      R := Item.AlignHorizontalCenterWith.BoundsRect;
      X := R.Left + (R.Width div 2);
      Item.Control.SetBounds(X - (Item.Control.Width div 2), Item.Control.Top, Item.Control.Width, Item.Control.Height);
    end;

    if Item.AlignVerticalCenterWith <> nil then
    begin
      R := Item.AlignVerticalCenterWith.BoundsRect;
      Y := R.Top + (R.Height div 2);
      Item.Control.SetBounds(Item.Control.Left, Y - (Item.Control.Height div 2), Item.Control.Width, Item.Control.Height);
    end;
  end;

begin
  if csLoading in ComponentState then Exit;

  AdjustClientRect(Rect);

  FForceDraw := True;

  for I := 0 to FControlCollection.Count - 1 do
  begin
    Item := FControlCollection[I];
    if Item.Control is TscCustomControl then
        TscCustomControl(Item.Control).FForceDraw := True;
    AlignControlWithPanel;
   if Item.Control is TscCustomControl then
        TscCustomControl(Item.Control).FForceDraw := False;
  end;

  for I := 0 to FControlCollection.Count - 1 do
  begin
    Item := FControlCollection[I];
    if Item.Control is TscCustomControl then
      TscCustomControl(Item.Control).FForceDraw := True;
    AlignControlWithControl;
    if Item.Control is TscCustomControl then
      TscCustomControl(Item.Control).FForceDraw := False;
  end;

  FForceDraw := False;
end;

procedure TscCustomRelativePanel.CMControlChange(var Msg: TCMControlChange);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    DisableAlign;
    try
      if Msg.Inserting and (Msg.Control.Parent = Self) then
      begin
        Msg.Control.Anchors := [];
        FControlCollection.AddControl(Msg.Control);
      end
      else
        FControlCollection.RemoveControl(Msg.Control);
    finally
      EnableAlign;
    end;
  end;
end;

procedure TscCustomRelativePanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  ControlItem: TscRelativePanelControlItem;
begin
  for I := 0 to FControlCollection.Count - 1 do
  begin
    ControlItem := TscRelativePanelControlItem(FControlCollection.Items[I]);
    if (ControlItem.Control <> nil) and (ControlItem.Control.Owner = Root) then
      Proc(ControlItem.Control);
  end;
end;

function TscCustomRelativePanel.GetControlIndex(AControl: TControl): Integer;
begin
  Result := FControlCollection.IndexOf(AControl);
end;

procedure TscCustomRelativePanel.SetControlIndex(AControl: TControl; Index: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetControlIndex(AControl);
  if (CurIndex > -1) and (CurIndex <> Index) and (Index < FControlCollection.Count) then
  begin
    FControlCollection[CurIndex].Index := Index;
    Realign;
  end;
end;

procedure TscCustomRelativePanel.SetControlCollection(const Value: TscRelativePanelControlCollection);
begin
  FControlCollection.Assign(Value);
end;

constructor TscActivityIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaled := True;
  FResImages := TscImageCollection.Create(Self);
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 50;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationFrame := 0;
  FIndicatorColor := clBtnText;
  FTransparentBackground := True;
  FLoading := False;
  DrawOnBackground := True;
  FKind := scaikPoints;
  InitResImages;
  Width := DefaultAniIndicatorWidth;
  Height := Width;
end;

destructor TscActivityIndicator.Destroy;
begin
  FResImages.Free;
  FResImages := nil;
  FAnimationTimer.Free;
  inherited;
end;

procedure TscActivityIndicator.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer);
begin
  if not FScaled then
  begin
   if AWidth > DefaultAniIndicatorWidth then
      AWidth := DefaultAniIndicatorWidth;
    if AWidth < DefaultAniIndicatorSmallWidth then
      AWidth := DefaultAniIndicatorSmallWidth;
  end;
  if (Align = alNone) then
    AWidth := AHeight;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscActivityIndicator.SetKind(Value: TscActivityIndicatorKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    InitResImages;
    SetBounds(Left, Top, Width, Height);
  end;
end;

function TscActivityIndicator.GetFrameCount: Integer;
begin
  Result := DefaultAniIndicatorFrameCount;
  case FKind of
    scaikCircle: Result := 20;
    scaikSectors: Result := 12;
  end;
end;

procedure TscActivityIndicator.InitResImages;
var
  Item: TscImageCollectionItem;
begin
  if FResImages = nil then Exit;
  FLoading := True;
  FResImages.Images.Clear;
  Item := FResImages.Images.Add;
  case FKind of
    scaikPoints:
      Item.LoadPngFromResourceName(HInstance, 'ANIINDICATOR_MASK');
    scaikCircle:
      Item.LoadPngFromResourceName(HInstance, 'ANIINDICATOR_MASK2');
    scaikSectors:
      Item.LoadPngFromResourceName(HInstance, 'ANIINDICATOR_MASK3');
  end;
  if FActive then
    FAnimationFrame := 1
  else
    FAnimationFrame := 0;
  Item.Color := FIndicatorColor;
  FLoading := False;
end;

procedure TscActivityIndicator.OnAnimationTimer(Sender: TObject);
begin
  Inc(FAnimationFrame);
  if FAnimationFrame > GetFrameCount then
     FAnimationFrame := 1;
  RePaintControl;
end;

procedure TscActivityIndicator.Loaded;
begin
  inherited;
end;

procedure TscActivityIndicator.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  FrameRect, DrawRect: TRect;
  FrameWidth, FrameHeight: Integer;
  XCount, X, Y: Integer;
  Buffer: TBitmap;
begin
  if csDesigning in ComponentState then
  begin
    DrawRect := Rect(0, 0, Width, Height);
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(DrawRect);
    Exit;
  end;

  if FLoading then Exit;
  
  if not FActive or (FResImages = nil) or (FAnimationFrame = 0) then
    Exit;
  if FAnimationFrame > GetFrameCount then
    FAnimationFrame := 1;

  FResImages.Images[0].Color := scDrawUtils.GetStyleColor(FIndicatorColor);
  FrameWidth := DefaultAniIndicatorWidth;
  FrameHeight := FrameWidth;
  DrawRect := Rect(0, 0, Width, Height);
  XCount := FResImages.Images[0].Bitmap.Width div FrameWidth;
  Y := (FAnimationFrame - 1) div XCount;
  X := (FAnimationFrame - 1) - XCount * Y;
  FrameRect := Rect(X * FrameWidth, Y * FrameHeight,
    X * FrameWidth + FrameWidth, Y * FrameHeight + FrameHeight);
  if Width = FrameWidth then
  begin
    FResImages.DrawBitmapRect(ACanvas, DrawRect, FrameRect, 0);
  end
  else
  begin
    Buffer := TBitmap.Create;
    Buffer.PixelFormat := pf32bit;
    Buffer.SetSize(FrameRect.Width + 2, FrameRect.Height + 2);
    try
      Buffer.AlphaFormat := afIgnored;
      Bitmap_ClearAlpha(Buffer, 0);
      Buffer.AlphaFormat := afPremultiplied;
      FResImages.DrawBitmapRect(Buffer.Canvas,
        Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1), FrameRect, 0);
      Buffer.AlphaFormat := afIgnored;
      GPDrawBitmapSmooth(ACanvas.Handle, DrawRect, Buffer);
    finally
      Buffer.Free;
    end;
  end;
end;

procedure TscActivityIndicator.SetIndicatorColor(Value: TColor);
begin
  if FIndicatorColor <> Value then
  begin
    FIndicatorColor := Value;
    if FIndicatorColor = clNone then
      FIndicatorColor := clBtnText;
    RePaintControl;
  end;
end;

procedure TscActivityIndicator.SetActive(Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
  begin
    FAnimationFrame := 0;
    FAnimationTimer.Enabled := FActive;
    if not FAnimationTimer.Enabled then
      RePaintControl;
  end;
end;


constructor TscCustomToggleSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFixedMinSize := True;
  FCanFocused := True;

  FShowCaption := True;
  FCaptionOn := 'On';
  FCaptionOff := 'Off';

  FSwitchWidth := 40;
  FSwitchHeight := 20;

  FCancelAction := False;
  FLoading := False;
  FResImages := TscImageCollection.Create(Self);

  FCustomImages := nil;
  FFrameImageIndex :=- 1;
  FFrameOnImageIndex := -1;
  FFramePressedImageIndex := -1;
  FThumbImageIndex := -1;
  FThumbOnImageIndex := -1;
  FThumbPressedImageIndex := -1;

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.Interval := 15;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimation := True;

  TabStop := True;
  FTransparentBackground := True;
  DrawOnBackground := True;

  FMouseDown := False;

  FReadOnly := False;
  FState := scswOff;

  Width := DefaultSwitchWidth;
  Height := DefaultSwitchHeight;
  FThumbWidth := 0;

  FFrameColor := DefaultSwitchFrameColor;
  FFrameOnColor := DefaultSwitchFrameOnColor;
  FFramePressedColor :=  DefaultSwitchFramePressedColor;
  FThumbColor := DefaultSwitchThumbColor;
  FThumbOnColor := DefaultSwitchThumbOnColor;
  FThumbPressedColor := DefaultSwitchThumbPressedColor;
  InitResImages;

  Width := 80;
  Height := 35;
end;

destructor TscCustomToggleSwitch.Destroy;
begin
  FResImages.Free;
  FResImages := nil;
  FAnimationTimer.Free;
  inherited;
end;

procedure TscCustomToggleSwitch.SetCanFocused(Value: Boolean);
begin
  if FCanFocused <> Value then
  begin
    FCanFocused := Value;
    TabStop := FCanFocused;
  end;
end;

procedure TscCustomToggleSwitch.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.WndProc(var Message: TMessage);
begin
  inherited;
  if FCanFocused then
   case Message.Msg of
      WM_KEYDOWN:
      if Focused and not FReadOnly and
         ((TWMKeyDown(Message).CharCode = VK_SPACE) or
         (TWMKeyDown(Message).CharCode = VK_RETURN)) then
      begin
        FKeyDown := True;
      end;
      WM_KEYUP:
        if Focused and FKeyDown and not FReadOnly and
           ((TWMKeyDown(Message).CharCode = VK_SPACE) or
           (TWMKeyDown(Message).CharCode = VK_RETURN)) then
        begin
          FKeyDown := False;
          ChangeState;
        end;
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        if not (csDesigning in ComponentState) and not Focused then
          SetFocus;
      end;
   end;
end;

procedure TscCustomToggleSwitch.WMSetFocus(var Message: TWMSETFOCUS);
begin
  inherited;
  if FCanFocused then
    if DrawTextMode = scdtmGDIPlus then
      Invalidate
    else
      RePaint;
end;

procedure TscCustomToggleSwitch.WMKillFocus(var Message: TWMKILLFOCUS);
begin
  inherited;
  if FCanFocused then
   RePaint;
end;

procedure TscCustomToggleSwitch.SetCaptionOn(Value: String);
begin
  if FCaptionOn <> Value then
  begin
    FCaptionOn := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetCaptionOff(Value: String);
begin
  if FCaptionOff <> Value then
  begin
    FCaptionOff := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetSwitchWidth(Value: Integer);
begin
  if (Value <> FSwitchWidth) and (Value >=40) then
  begin
    FSwitchWidth := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetSwitchHeight(Value: Integer);
begin
  if (Value <> FSwitchHeight) and (Value >=20) then
  begin
    FSwitchHeight := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FSwitchWidth := MulDiv(FSwitchWidth, M, D);
  if (FSwitchWidth < 40) and FFixedMinSize then
    FSwitchWidth := 40;
  FSwitchHeight := MulDiv(FSwitchHeight, M, D);
  if (FSwitchHeight < 20) and FFixedMinSize then
    FSwitchHeight := 20;
  ThumbWidth := MulDiv(ThumbWidth, M, D);
  InitResImages;
end;

procedure TscCustomToggleSwitch.DoChange;
begin
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    if Assigned(FOnChangeState) then
      FOnChangeState(Self);
end;

procedure TscCustomToggleSwitch.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TscCustomToggleSwitch.InitResImages;
var
  Item: TscImageCollectionItem;
  Scale: String;
  Margin: Integer;
  ScaleF: Double;
begin
  if FResImages = nil then Exit;
  FLoading := True;
  FResImages.Images.Clear;
  ScaleF := 1;
  Scale := '';
  if FScaleFactor > 2 then
  begin
    ScaleF := FScaleFactor / 2;
    Scale := '_200';
  end
  else
  if (FScaleFactor = 2) then
  begin
    ScaleF := 1;
    Scale := '_200';
  end
  else
  if (FScaleFactor > 1.5) and (FScaleFactor < 2) then
  begin
    ScaleF := FScaleFactor / 1.5;
    Scale := '_150';
  end
  else
  if (FScaleFactor = 1.5) then
  begin
    ScaleF := 1;
    Scale := '_150';
  end
  else
  if (FScaleFactor > 1.25) and (FScaleFactor < 1.5) then
  begin
    ScaleF := FScaleFactor / 1.25;
    Scale := '_125';
  end
  else
  if FScaleFactor = 1.25 then
  begin
    Scale := '_125';
    ScaleF := 1;
  end;
  Margin := Round(10 * FScaleFactor);
  Item := FResImages.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SWITCH_FRAME_MASK_OFF' + Scale, ScaleF);
  Item.BitmapOptions.LeftMargin := Margin;
  Item.BitmapOptions.TopMargin := Margin;
  Item.BitmapOptions.BottomMargin := Margin;
  Item.BitmapOptions.RightMargin := Margin;
  Item.DrawStyle := idsStretch;
  Item.CheckBitmapOptions;
  Item.BitmapOptions.DrawOnlyBorder := True;
  Item.Color := FFrameColor;
  Item := FResImages.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SWITCH_FRAME_MASK_ON' + Scale, ScaleF);
  Item.BitmapOptions.LeftMargin := Margin;
  Item.BitmapOptions.TopMargin := Margin;
  Item.BitmapOptions.BottomMargin := Margin;
  Item.BitmapOptions.RightMargin := Margin;
  Item.DrawStyle := idsStretch;
  Item.CheckBitmapOptions;
  Item.Color := FFrameOnColor;
  Item := FResImages.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SWITCH_THUMB_MASK' + Scale, ScaleF);
  Item.BitmapOptions.LeftMargin := Margin;
  Item.BitmapOptions.TopMargin := Margin;
  Item.BitmapOptions.BottomMargin := Margin;
  Item.BitmapOptions.RightMargin := Margin;
  Item.DrawStyle := idsStretch;
  Item.CheckBitmapOptions;
  Item.Color := FThumbColor;
  FLoading := False;
end;

procedure TscCustomToggleSwitch.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscCustomToggleSwitch.SetFrameImageIndex(Value: Integer);
begin
  if FFrameImageIndex <> Value then
  begin
    FFrameImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetFrameOnImageIndex(Value: Integer);
begin
  if FFrameOnImageIndex <> Value then
  begin
    FFrameOnImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetFramePressedImageIndex(Value: Integer);
begin
  if FFramePressedImageIndex <> Value then
  begin
    FFramePressedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetThumbImageIndex(Value: Integer);
begin
  if FThumbImageIndex <> Value then
  begin
    FThumbImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetThumbOnImageIndex(Value: Integer);
begin
  if FThumbOnImageIndex <> Value then
  begin
    FThumbOnImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetThumbPressedImageIndex(Value: Integer);
begin
  if FThumbPressedImageIndex <> Value then
  begin
    FThumbPressedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetStyleKind(Value: TscSwitchStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.OnAnimationTimer(Sender: TObject);
var
  FAnimationStep: Integer;
  FCanChangeState: Boolean;
begin
  if FCancelAction then
  begin
    if (FState = scswOn) then
    begin
      if FThumbRect.Right < FSWRect.Right - FSWRect.Width div 3 then
        FAnimationStep := (FSWRect.Width - FThumbRect.Width) div 3
      else
        FAnimationStep := (FSWRect.Right - FThumbRect.Right) div 6;
    end
    else
    begin
      if FThumbRect.Left > FSWRect.Left + FSWRect.Width div 3 then
        FAnimationStep := (FSWRect.Width - FThumbRect.Width) div 3
      else
        FAnimationStep := (FThumbRect.Left - FSWRect.Left) div 6
    end;
    if FAnimationStep < 1 then
      FAnimationStep := 1;
    if FState = scswOn then
      OffsetRect(FThumbRect, FAnimationStep, 0)
    else
      OffsetRect(FThumbRect, -FAnimationStep, 0);
    if FThumbRect.Left < FSWRect.Left then
    begin
      OffsetRect(FThumbRect, - (FSWRect.Left - FThumbRect.Left), 0);
      FAnimationTimer.Enabled := False;
      FThumbMoving := False;
      FCancelAction := False;
    end
    else
    if FThumbRect.Right > FSWRect.Right then
    begin
      OffsetRect(FThumbRect, -(FThumbRect.Right - FSWRect.Right), 0);
      FAnimationTimer.Enabled := False;
      FThumbMoving := False;
      FCancelAction := False;
    end;
    RePaintControl;
    Exit;
  end;
  if (FState = scswOff) then
  begin
    if FThumbRect.Right < FSWRect.Right - Width div 3 then
      FAnimationStep := (FSWRect.Width - FThumbRect.Width) div 3
    else
      FAnimationStep := (FSWRect.Right - FThumbRect.Right) div 6;
  end
  else
  begin
    if FThumbRect.Left > FSWRect.Left + FSWRect.Width div 3 then
      FAnimationStep := (FSWRect.Width - FThumbRect.Width) div 3
    else
      FAnimationStep := (FThumbRect.Left - FSWRect.Left) div 6
  end;
  if FAnimationStep < 1 then
    FAnimationStep := 1;
  FCanChangeState := False;
  if FState = scswOff then
    OffsetRect(FThumbRect, FAnimationStep, 0)
  else
    OffsetRect(FThumbRect, -FAnimationStep, 0);
  if FThumbRect.Left < FSWRect.Left then
  begin
    OffsetRect(FThumbRect, -(FSWRect.Left - FThumbRect.Left), 0);
    FAnimationTimer.Enabled := False;
    FCanChangeState := True;
    FThumbMoving := False;
  end
  else
  if FThumbRect.Right > FSWRect.Right then
  begin
    OffsetRect(FThumbRect, -(FThumbRect.Right - FSWRect.Right), 0);
    FAnimationTimer.Enabled := False;
    FCanChangeState := True;
    FThumbMoving := False;
  end;
  if FCanChangeState then
  begin
    if FState = scswOff then
      State := scswOn
    else
      State := scswOff;
  end
  else
    RePaintControl;
end;

procedure TscCustomToggleSwitch.CreateWnd;
begin
  inherited;
end;

procedure TscCustomToggleSwitch.Loaded;
begin
  inherited;
end;

procedure TscCustomToggleSwitch.DrawSwitch(Canvas: TCanvas);
var
  R: TRect;
  {$IFDEF VER300_UP}
  Details: TThemedElementDetails;
  {$ENDIF}
  ThumbR: TRect;
  FrmColor, ThmbColor: TColor;
  FrmIndex, ThmbIndex: Integer;
  TW: Integer;
  TempState: TscSwitchState;
begin
  R := Rect(0, 0, FSwitchWidth, FSwitchHeight);

  R.Top := Height div 2  - R.Height div 2;
  R.Bottom := R.Top + FSwitchHeight;
  
  if BidiMode = bdRightToLeft  then
  begin
    R.Left := Width - FSwitchWidth;
    R.Right := Width;
  end;

  if BidiMode = bdRightToLeft then
    OffsetRect(R, -2, 0)
  else
    OffsetRect(R, 2, 0);

  FSWRect := R;

  TW := FThumbWidth;
  if TW = 0 then TW := R.Width div 2;

  if FThumbMoving and (FThumbRect.Width > 0) then
    ThumbR := FThumbRect
  else
  begin
    ThumbR := R;
    if FState = scswOff then
      ThumbR.Right := ThumbR.Left + TW
    else
      ThumbR.Left := ThumbR.Right - TW;
    FThumbRect := ThumbR;
  end;

  TempState := FState;
  if (FAnimationTimer <> nil) and (FAnimationTimer.Enabled) and not FPressed and not FCancelAction then
  begin
    if TempState = scswOff then
      TempState := scswOn
    else
      TempState := scswOff;
  end;

  if (FStyleKind = scswsCustomImage) and (FCustomImages <> nil) then
  begin
    if FPressed and (FFramePressedImageIndex >= 0) then
      FrmIndex := FFramePressedImageIndex
    else
    if TempState = scswOn then
      FrmIndex := FFrameOnImageIndex
    else
      FrmIndex:= FFrameImageIndex;
    if FCustomImages.IsIndexAvailable(FrmIndex) then
      FCustomImages.Draw(Canvas, R, FrmIndex, FScaleFactor);
    if FPressed and (FThumbPressedImageIndex >= 0) and FThumbMoving then
      ThmbIndex := FThumbPressedImageIndex
    else
    if TempState = scswOn then
      ThmbIndex := FThumbOnImageIndex
    else
      ThmbIndex:= FThumbImageIndex;
    if FCustomImages.IsIndexAvailable(ThmbIndex) then
      FCustomImages.Draw(Canvas, ThumbR, ThmbIndex, FScaleFactor);
    Exit;
  end;

  {$IFDEF VER300_UP}
  if TStyleManager.IsCustomStyleActive and (FStyleKind = scswsStyled) then
  begin
    // Draw Track
    case TempState of
      scswOff:
        begin
          if Enabled then
            Details := StyleServices.GetElementDetails(ttsTrackOffNormal)
          else
            Details := StyleServices.GetElementDetails(ttsTrackOffDisabled);
        end;

      scswOn:
        begin
          if Enabled then
            Details := StyleServices.GetElementDetails(ttsTrackOnNormal)
          else
            Details := StyleServices.GetElementDetails(ttsTrackOnDisabled);
        end;
    end;

    StyleServices.DrawElement(Canvas.Handle, Details, R);

    // Draw Thumb
    if Enabled then
      Details := StyleServices.GetElementDetails(ttsThumbNormal)
    else
      Details := StyleServices.GetElementDetails(ttsThumbDisabled);

    StyleServices.DrawElement(Canvas.Handle, Details, ThumbR);
  end
  else
  {$ENDIF}
  begin
    if (FResImages = nil) or (FResImages.Images.Count = 0) then Exit;
    if FPressed and (FFramePressedColor <> clNone) then
    begin
      FrmColor := scDrawUtils.GetStyleColor(FFramePressedColor);
      FResImages.Images[1].Color := FrmColor;
      FResImages.DrawBitmap(Canvas, R, 1);
    end
    else
    if TempState = scswOn then
    begin
      FrmColor := scDrawUtils.GetStyleColor(FFrameOnColor);
      FResImages.Images[1].Color := FrmColor;
      FResImages.DrawBitmap(Canvas, R, 1);
    end
    else
    begin
      FrmColor := scDrawUtils.GetStyleColor(FFrameColor);
      FResImages.Images[0].Color := FrmColor;
      FResImages.DrawBitmap(Canvas, R, 0);
    end;

    if FPressed and (FThumbPressedColor <> clNone) then
      ThmbColor := scDrawUtils.GetStyleColor(FThumbPressedColor)
    else
    if TempState = scswOn then
      ThmbColor := scDrawUtils.GetStyleColor(FThumbOnColor)
    else
      ThmbColor := scDrawUtils.GetStyleColor(FThumbColor);
    FResImages.Images[2].Color := ThmbColor;
    FResImages.DrawBitmap(Canvas, ThumbR, 2);
  end;
end;

procedure TscCustomToggleSwitch.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  TextColor: TColor;
  S: String;
  TR: TRect;
  R: TRect;
begin
  if FLoading then Exit;
  DrawSwitch(ACanvas);

  if (ACtrlState <> scsDisabled) and FUseFontColorToStyleColor
  then
    TextColor := scDrawUtils.GetStyleColor(Self.Font.Color)
  else
    TextColor := GetCheckBoxTextColor(ACtrlState);
  ACanvas.Font.Assign(Font);
  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
    ACanvas.Font.Color := TextColor;
  if State = scswOff then
    S := FCaptionOff
  else
    S := FCaptionOn;

  if (S <> '') and FShowCaption then
  begin
    ACanvas.Font := Self.Font;
    ACanvas.Font.Color := TextColor;
    TR := Rect(0, 0, 0, 0);
    DrawText(ACanvas.Handle, PChar(S), Length(S), TR,
      DT_LEFT or DT_CALCRECT or DT_NOPREFIX);
    if BidiMode = bdRightToLeft then
    begin
      R := Rect(2, 2, FSWRect.Left - Round(10 * FScaleFactor), Height - 2);
      TR := Rect(R.Right - TR.Width, R.Top + R.Height div 2 - TR.Height div 2,
        R.Right, R.Top + R.Height div 2 - TR.Height div 2 + TR.Height);
    end
    else
    begin
      R := Rect(FSWRect.Right + Round(10 * FScaleFactor), 2, Width - 2, Height - 2);
      TR := Rect(R.Left, R.Top + R.Height div 2 - TR.Height div 2,
        R.Left + TR.Width, R.Top + R.Height div 2 - TR.Height div 2 + TR.Height);
    end;
    ACanvas.Brush.Style := bsClear;
    OffsetRect(TR, 0, -1);
    scDrawText(ACanvas, S, TR, IsRightToLeft, True);
    if Focused then
    begin
      InflateRect(TR, 3, 3);
      scDrawFocusRect(ACanvas, TR, FScaleFactor);
    end;
  end
  else
   if Focused and FCanFocused then
    begin
      ACanvas.Font.Color := TextColor;
      R := FSWRect;
      InflateRect(R, 2, 2);
      scDrawFocusRect(ACanvas, R, FScaleFactor);
    end;
end;

procedure TscCustomToggleSwitch.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Offset: Integer;
begin
  inherited;
  if Self.FReadOnly then Exit;

  if FThumbMoving and not FAnimationTimer.Enabled then
  begin
    Offset := X - FOldX;
    FOldX := X;
    OffsetRect(FThumbRect, Offset, 0);
    if FThumbRect.Left < FSWRect.Left then
      OffsetRect(FThumbRect, -(FThumbRect.Left - FSWRect.Left), 0)
    else
      if FThumbRect.Right > FSWRect.Right then
        OffsetRect(FThumbRect, -(FThumbRect.Right - FSWRect.Right), 0);
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Self.FReadOnly then Exit;

  if (Button <> mbLeft) or not Enabled then Exit;
  if FThumbMoving and (FAnimationTimer <> nil) and FAnimationTimer.Enabled then Exit;

  if FStyleKind = scswsCustomImage then
    FPressed := True
  else
    FPressed := FFramePressedColor <> clNone;

  if FThumbRect.Contains(Point(X, Y)) then
  begin
    FThumbMoving := True;
    FOldX := X;
    FMouseDownX := X;
    RePaintControl;
  end
  else
  begin
    RePaintControl;
    FMouseDown := True;
  end;
end;

procedure TscCustomToggleSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if Self.FReadOnly then Exit;

  FPressed := False;
  if FThumbMoving and (FAnimationTimer <> nil) and FAnimationTimer.Enabled then Exit;

  if FThumbMoving and (FMouseDownX <> X) then
  begin
    FThumbMoving := False;
    if (FThumbRect.Left + FThumbRect.Width div 2 >= FSWRect.Left + FSWRect.Width div 2) and (State = scswOff) then
    begin
      ChangeState;
      FMouseDown := False;
      //State := scswOn;
    end
    else
    if (FThumbRect.Left + FThumbRect.Width div 2 <= FSWRect.Left + FSWRect.Width div 2) and (State = scswOn) then
    begin
      ChangeState;
      FMouseDown := False;
      //State := scswOff;
    end
    else
    if FAnimation then
    begin
      FCancelAction := True;
      FThumbMoving := True;
      FAnimationTimer.Enabled := True;
      FMouseDown := False;
    end
    else
      RePaintControl;
  end
  else
  if FMouseDown or (FThumbMoving and (FMouseDownX = X))  then
  begin
    R := Rect(0, 0, Width, Height);
    FThumbMoving := False;
    if R.Contains(Point(X, Y)) then
      ChangeState;
    FMouseDown := False;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.ChangeState;
begin
  if FMouseDown and FReadOnly then
    Exit;
  if FAnimation and not(csDesigning in ComponentState)
     and not(csLoading in ComponentState) then
  begin
    FThumbMoving := True;
    FAnimationTimer.Enabled := True;
  end
  else
  case State of
    scswOff: State := scswOn;
    scswOn:  State := scswOff;
  end;
end;

procedure TscCustomToggleSwitch.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    if FFrameColor = clNone then
      FFrameColor := DefaultSwitchFrameColor;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetFrameOnColor(Value: TColor);
begin
  if FFrameOnColor <> Value then
  begin
    FFrameOnColor := Value;
    if FFrameOnColor = clNone then
      FFrameOnColor := DefaultSwitchFrameColor;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetFramePressedColor(Value: TColor);
begin
  if FFramePressedColor <> Value then
  begin
    FFramePressedColor := Value;
    RePaintControl;
  end;
end;

function TscCustomToggleSwitch.IsOn: Boolean;
begin
  Result := FState = scswOn;
end;

procedure TscCustomToggleSwitch.SetState(Value: TscSwitchState);
begin
  if FState <> Value then
  begin
    FState := Value;
    RePaintControl;
    DoChange;
  end;
end;

procedure TscCustomToggleSwitch.SetThumbColor(Value: TColor);
begin
  if FThumbColor <> Value then
  begin
    FThumbColor := Value;
    if FThumbColor = clNone then
      FThumbColor := DefaultSwitchThumbColor;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetThumbOnColor(Value: TColor);
begin
  if FThumbOnColor <> Value then
  begin
    FThumbOnColor := Value;
    if FThumbOnColor = clNone then
      FThumbOnColor := DefaultSwitchThumbColor;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetThumbPressedColor(Value: TColor);
begin
  if FThumbPressedColor <> Value then
  begin
    FThumbPressedColor := Value;
    RePaintControl;
  end;
end;

procedure TscCustomToggleSwitch.SetThumbWidth(Value: Integer);
begin
  if FThumbWidth <> Value then
  begin
    FThumbWidth := Value;
    RePaintControl;
  end;
end;

// GridPanel

procedure TscCustomGridPanel.AlignControls(AControl: TControl; var Rect: TRect);

  procedure ArrangeControlInCell(AControl: TControl; CellRect: TRect; const AlignInfo: TAlignInfo);
  var
    NewBounds: TRect;
    AnchorSubset: TAnchors;
  begin
    if AControl.Align <> alNone then
        ArrangeControl(AControl, Point(CellRect.Right - CellRect.Left, CellRect.Bottom - CellRect.Top),
          AControl.Align, AlignInfo, CellRect, True)
    else
    begin
      AnchorSubset := AControl.Anchors * [akLeft, akRight];
      if AnchorSubset = [akLeft] then
        NewBounds.Left := CellRect.Left
      else if AnchorSubset = [akRight] then
        NewBounds.Left := Max(CellRect.Left, CellRect.Right - AControl.Margins.ExplicitWidth)
      else
        NewBounds.Left := Max(CellRect.Left, CellRect.Left + ((CellRect.Right - CellRect.Left) - AControl.Margins.ControlWidth) div 2);
      NewBounds.Right := NewBounds.Left + Min(CellRect.Right - CellRect.Left, AControl.Margins.ExplicitWidth);
      AnchorSubset := AControl.Anchors * [akTop, akBottom];
      if AnchorSubset = [akTop] then
        NewBounds.Top := CellRect.Top
      else if AnchorSubset = [akBottom] then
        NewBounds.Top := Max(CellRect.Top, CellRect.Bottom - AControl.Margins.ExplicitHeight)
      else
        NewBounds.Top := Max(CellRect.Top, CellRect.Top + ((CellRect.Bottom - CellRect.Top) - AControl.Margins.ControlHeight) div 2);
      NewBounds.Bottom := NewBounds.Top + Min(CellRect.Bottom - CellRect.Top, AControl.Margins.ExplicitHeight);
      AControl.Margins.SetControlBounds(NewBounds, True);
    end;
  end;

  procedure AdjustCellRect(var Rect: TRect);
  begin
    Inc(Rect.Left, Padding.Left);
    Inc(Rect.Top, Padding.Top);
    Dec(Rect.Right, Padding.Right);
    Dec(Rect.Bottom, Padding.Bottom);
  end;

  procedure ArrangeControls;
  var
    I, J, K: Integer;
    CellRect: TRect;
    SpanRect: TRect;
    ControlItem: TscControlItem;
    AlignInfo: TAlignInfo;
  begin
    AlignInfo.ControlIndex := 0;
    AlignInfo.AlignList := TList.Create;
    try
      CellRect.Top := Rect.Top;
      for J := 0 to FRowCollection.Count - 1 do
      begin
        CellRect.Left := Rect.Left;
        CellRect.Bottom := CellRect.Top + FRowCollection[J].Size;
        for I := 0 to FColumnCollection.Count - 1 do
        begin
          ControlItem := FControlCollection.ControlItems[I, J];
          CellRect.Right := CellRect.Left + FColumnCollection[I].Size;
          if (ControlItem <> nil) and (ControlItem.Control <> nil) and
             (ControlItem.Column = I) and (ControlItem.Row = J) then
          begin
            AlignInfo.AlignList.Clear;
            AlignInfo.AlignList.Add(ControlItem.Control);
            AlignInfo.Align := ControlItem.Control.Align;
            SpanRect := CellRect;
            if ControlItem.ColumnSpan > 1 then
              for K := I + 1 to Min(I + ControlItem.ColumnSpan - 1, FColumnCollection.Count - 1) do
                Inc(SpanRect.Right, FColumnCollection[K].Size);
            if ControlItem.RowSpan > 1 then
              for K := J + 1 to Min(J + ControlItem.RowSpan - 1, FRowCollection.Count - 1 ) do
                Inc(SpanRect.Bottom, FRowCollection[K].Size);
            AdjustCellRect(SpanRect);
            ArrangeControlInCell(ControlItem.Control, SpanRect, AlignInfo);
          end;
          CellRect.Left := CellRect.Right;
        end;
        CellRect.Top := CellRect.Bottom;
      end;
    finally
      AlignInfo.AlignList.Free;
    end;
  end;

begin
  AdjustClientRect(Rect);
  if FRecalcCellSizes then
    RecalcCellDimensions(Rect);
  if ControlCount > 0 then
    ArrangeControls;
end;

function TscCustomGridPanel.AutoAddColumn: TscColumnItem;
begin
  Result := FColumnCollection.Add;
  Result.SizeStyle := ssAuto;
  Result.AutoAdded := True;
end;

function TscCustomGridPanel.AutoAddRow: TscRowItem;
begin
  Result := FRowCollection.Add;
  Result.SizeStyle := ssAuto;
  Result.AutoAdded := True;
end;

procedure TscCustomGridPanel.CellIndexToCell(AIndex: Integer; var AColumn, ARow: Integer);
begin
  if FExpandStyle in [emAddColumns, emFixedSize] then
  begin
    AColumn := AIndex div FRowCollection.Count;
    ARow := AIndex mod FRowCollection.Count;
  end else
  begin
    ARow := AIndex div FColumnCollection.Count;
    AColumn := AIndex mod FColumnCollection.Count;
  end;
end;

function TscCustomGridPanel.CellToCellIndex(AColumn, ARow: Integer): Integer;
begin
  if FExpandStyle in [emAddColumns, emFixedSize] then
    Result := ColumnSpanIndex[AColumn, ARow]
  else
    Result := RowSpanIndex[AColumn, ARow];
end;

constructor TscCustomGridPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRowCollection := TscRowCollection.Create(Self);
  FColumnCollection := TscColumnCollection.Create(Self);
  FControlCollection := TscControlCollection.Create(Self);
  FRecalcCellSizes := True;
  FRowCollection.Add;
  FRowCollection.Add;
  FColumnCollection.Add;
  FColumnCollection.Add;
end;

procedure TscCustomGridPanel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  I: Integer;
begin
  inherited;
  if M <> D then
  begin
    for I := 0 to FRowCollection.Count - 1 do
      if FRowCollection.Items[I].FSizeStyle = ssAbsolute then
        FRowCollection.Items[I].Value := FRowCollection.Items[I].Value * M / D;
    for I := 0 to FColumnCollection.Count - 1 do
      if FColumnCollection.Items[I].FSizeStyle = ssAbsolute then
        FColumnCollection.Items[I].Value := FColumnCollection.Items[I].Value * M / D;
  end;
end;

procedure TscCustomGridPanel.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if not (csLoading in ComponentState) then
    if Message.Inserting and (Message.Control.Parent = Self) then
    begin
      DisableAlign;
      try
        Message.Control.Anchors := [];
        FControlCollection.AddControl(Message.Control);
      finally
        EnableAlign;
      end;
    end else
      FControlCollection.RemoveControl(Message.Control);
end;

destructor TscCustomGridPanel.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRowCollection);
  FreeAndNil(FColumnCollection);
  FreeAndNil(FControlCollection);
end;

function TscCustomGridPanel.GetCellCount: Integer;
begin
  Result := FRowCollection.Count * FColumnCollection.Count;
end;

function TscCustomGridPanel.GetCellRect(AColumn, ARow: Integer): TRect;
var
  I: Integer;
begin
  Result.Left := 0;
  Result.Top := 0;
  for I := 0 to AColumn - 1 do
    Inc(Result.Left, FColumnCollection[I].Size);
  for I := 0 to ARow - 1 do
    Inc(Result.Top, FRowCollection[I].Size);
  Result.BottomRight := CellSize[AColumn, ARow];
  Inc(Result.Bottom, Result.Top);
  Inc(Result.Right, Result.Left);
end;

function TscCustomGridPanel.GetCellSizes(AColumn, ARow: Integer): TPoint;
begin
  Result := Point(FColumnCollection[AColumn].Size, FRowCollection[ARow].Size);
end;

procedure TscCustomGridPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  ControlItem: TscControlItem;
begin
  for I := 0 to FControlCollection.Count - 1 do
  begin
    ControlItem := TscControlItem(FControlCollection.Items[I]);
    if (ControlItem.Control <> nil) and (ControlItem.Control.Owner = Root) then
      Proc(ControlItem.Control);
  end;
end;

function TscCustomGridPanel.GetColumnSpanIndex(AColumn, ARow: Integer): Integer;
begin
  Result := AColumn + (ARow * FColumnCollection.Count);
end;

function TscCustomGridPanel.GetRowSpanIndex(AColumn, ARow: Integer): Integer;
begin
  Result := ARow + (AColumn * FRowCollection.Count);
end;

function TscCustomGridPanel.IsColumnEmpty(AColumn: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (AColumn > -1) and (AColumn < FColumnCollection.Count) then
  begin
    for I := 0 to FRowCollection.Count -1 do
      if ControlCollection.Controls[AColumn, I] <> nil then
        Exit;
    Result := True;
  end;
end;

function TscCustomGridPanel.IsRowEmpty(ARow: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ARow > -1) and (ARow < FRowCollection.Count) then
  begin
    for I := 0 to FColumnCollection.Count -1 do
      if ControlCollection.Controls[I, ARow] <> nil then
        Exit;
    Result := True;
  end;
end;

procedure TscCustomGridPanel.Loaded;
var
  LItem: TCollectionItem;
begin
  inherited;
  for LItem in ControlCollection do
    with TscControlItem(LItem) do
      TscControlItem(LItem).InternalSetLocation(Column, Row, False, True);
end;

procedure TscCustomGridPanel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  I: Integer;
  LinePos, Size: Integer;
  ClientRect: TRect;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    LinePos := 0;
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Color := clBlack;
    ClientRect := GetClientRect;
    for I := 0 to FColumnCollection.Count - 2 do
    begin
      Size := FColumnCollection[I].Size;
      ACanvas.MoveTo(LinePos + Size, ClientRect.Top);
      ACanvas.LineTo(LinePos + Size, ClientRect.Bottom);
      Inc(LinePos, Size);
    end;
    LinePos := 0;
    for I := 0 to FRowCollection.Count - 2 do
    begin
      Size := FRowCollection[I].Size;
      ACanvas.MoveTo(ClientRect.Left, LinePos + Size);
      ACanvas.LineTo(ClientRect.Right, LinePos + Size);
      Inc(LinePos, Size);
    end;
  end;
end;

procedure TscCustomGridPanel.RecalcCellDimensions(const Rect: TRect);
var
  I, J: Integer;
  LSize, XSize, YSize, RemainingX, RemainingY: Integer;
  MaxSize, PercentXCount, PercentYCount: Integer;
  PercentX, PercentY: Double;
  ControlItem: TscControlItem;
begin
  XSize := Rect.Right - Rect.Left;
  YSize := Rect.Bottom - Rect.Top;
  PercentX := 0.0;
  PercentY := 0.0;
  PercentXCount := 0;
  for I := 0 to FColumnCollection.Count - 1 do
    with FColumnCollection[I] do
      if SizeStyle = ssAbsolute then
        Dec(XSize, Trunc(Value))
      else if SizeStyle = ssPercent then
      begin
        PercentX := PercentX + Value;
        Inc(PercentXCount);
      end
      else
      begin
        MaxSize := 0;
        for J := 0 to FRowCollection.Count - 1 do
        begin
          ControlItem := FControlCollection.ControlItems[I, J];
          if (ControlItem <> nil) and (ControlItem.Control <> nil) and
             (ControlItem.Column = I) and (ControlItem.Row = J) then
          begin
            LSize := ControlItem.Control.Margins.ControlWidth + Self.Padding.Left + Self.Padding.Right;
            if LSize > MaxSize then
              MaxSize := LSize;
          end;
        end;
        Dec(XSize, MaxSize);
        Size := MaxSize;
      end;
  PercentYCount := 0;
  for I := 0 to FRowCollection.Count - 1 do
    with FRowCollection[I] do
      if SizeStyle = ssAbsolute then
        Dec(YSize, Trunc(Value))
      else if SizeStyle = ssPercent then
      begin
        PercentY := PercentY + Value;
        Inc(PercentYCount);
      end else
      begin
        MaxSize := 0;
        for J := 0 to FColumnCollection.Count - 1 do
        begin
          ControlItem := FControlCollection.ControlItems[J, I];
          if (ControlItem <> nil) and (ControlItem.Control <> nil) and
             (ControlItem.Column = J) and (ControlItem.Row = I) then
          begin
            LSize := ControlItem.Control.Margins.ControlHeight + Self.Padding.Top + Self.Padding.Bottom;
            if LSize > MaxSize then
              MaxSize := LSize;
          end;
        end;
        Dec(YSize, MaxSize);
        Size := MaxSize;
      end;
  // Finally Calculate the size of the percentage-based columns and rows based on the remaining
  // X and Y sizes
  RemainingX := XSize;
  RemainingY := YSize;
  for I := 0 to FColumnCollection.Count - 1 do
    with FColumnCollection[I] do
      if SizeStyle = ssPercent then
      begin
        if IsZero(PercentX) then
          Value := 100.0 / PercentXCount
        else
          Value := (Value / PercentX) * 100.0;
        Size := Trunc(XSize * (Value / 100.0));
        Dec(RemainingX, Size);
        if (RemainingX > 0) and (I = FColumnCollection.Count - 1) then
          Size := Size + RemainingX;
      end;
  for I := 0 to FRowCollection.Count - 1 do
    with FRowCollection[I] do
      if SizeStyle = ssPercent then
      begin
        if IsZero(PercentY) then
          Value := 100.0 / PercentYCount
        else
          Value := (Value / PercentY) * 100.0;
        Size := Trunc(YSize * (Value / 100.0));
        Dec(RemainingY, Size);
        if (RemainingY > 0) and (I = FRowCollection.Count - 1) then
          Size := Size + RemainingY;
      end;
  FRecalcCellSizes := False;
end;

procedure TscCustomGridPanel.RemoveEmptyAutoAddColumns;
var
  I: Integer;
begin
  for I := FColumnCollection.Count - 1 downto 0 do
    if FColumnCollection[I].AutoAdded and IsColumnEmpty(I) then
      FColumnCollection.Delete(I)
    else
      Exit;
end;

procedure TscCustomGridPanel.RemoveEmptyAutoAddRows;
var
  I: Integer;
begin
  for I := FRowCollection.Count - 1 downto 0 do
    if FRowCollection[I].AutoAdded and IsRowEmpty(I) then
      FRowCollection.Delete(I)
    else
      Exit;
end;

procedure TscCustomGridPanel.SetControlCollection(const Value: TscControlCollection);
begin
  FControlCollection.Assign(Value);
end;

procedure TscCustomGridPanel.SetRowCollection(const Value: TscRowCollection);
begin
  FRowCollection.Assign(Value);
end;

procedure TscCustomGridPanel.SetColumnCollection(const Value: TscColumnCollection);
begin
  FColumnCollection.Assign(Value);
end;

procedure TscCustomGridPanel.UpdateControlsColumn(AColumn: Integer);
var
  I, J: Integer;
  AControlItem: TscControlItem;
begin
  for I := AColumn + 1 to FColumnCollection.Count - 1 do
    for J := 0 to FRowCollection.Count - 1 do
    begin
      AControlItem := FControlCollection.ControlItems[I, J];
      if (AControlItem <> nil) and (AControlItem.Column = I) and
         (AControlItem.Row = J) then
        AControlItem.SetColumn(AControlItem.Column - 1);
    end;
end;

procedure TscCustomGridPanel.UpdateControlsRow(ARow: Integer);
var
  I, J: Integer;
  AControlItem: TscControlItem;
begin
  for I := 0 to FColumnCollection.Count - 1 do
    for J := ARow + 1 to FRowCollection.Count - 1 do
    begin
      AControlItem := FControlCollection.ControlItems[I, J];
      if (AControlItem <> nil) and (AControlItem.Column = I) and
         (AControlItem.Row = J) then
        AControlItem.SetRow(AControlItem.Row - 1);
    end;
end;

procedure TscCustomGridPanel.UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint);
var
  Rect: TRect;
  Index: Integer;
  ControlItem: TscControlItem;
begin
  if FRecalcCellSizes and HandleAllocated then
  begin
    Rect := GetClientRect;
    AdjustClientRect(Rect);
    RecalcCellDimensions(Rect);
  end;
  Index := FControlCollection.IndexOf(AControl);
  if Index > -1 then
  begin
    ControlItem := FControlCollection[Index];
    AOriginalParentSize := CellSize[ControlItem.Column, ControlItem.Row]
  end else
    inherited UpdateControlOriginalParentSize(AControl, AOriginalParentSize);
end;

procedure TscCustomGridPanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  FRecalcCellSizes := True;
end;

{ TscCellItem }

procedure TscCellItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TscCellItem then
    with TscCellItem(Dest) do
    begin
      FSizeStyle := Self.FSizeStyle;
      FValue := Self.FValue;
      FSize := Self.FSize;
    end;
end;

constructor TscCellItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSizeStyle := ssPercent;
end;

procedure TscCellItem.SetSizeStyle(Value: TscSizeStyle);
begin
  if Value <> FSizeStyle then
  begin
    FSizeStyle := Value;
    Changed(False);
  end;
end;

procedure TscCellItem.SetValue(Value: Double);
begin
  if Value <> FValue then
  begin
    if FSizeStyle = ssAbsolute then
    begin
      FSize := Trunc(Value);
      FValue := FSize;
    end else
      FValue := Value;
    Changed(False);
  end;
end;

{ TscCellCollection }

function TscCellCollection.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := 'Member';
    1: Result := 'Size Type';
    2: Result := 'Value';
  else
    Result := '';
  end;
end;

function TscCellCollection.GetAttrCount: Integer;
begin
  Result := 3;
end;

function TscCellCollection.GetItem(Index: Integer): TscCellItem;
begin
  Result := TscCellItem(inherited GetItem(Index));
end;

function TscCellCollection.GetItemAttr(Index, ItemIndex: Integer): string;

  function GetSizeStyleString(Index: Integer): string;
  begin
    with Items[Index] do
      case SizeStyle of
        ssAbsolute: Result := 'Absolute';
        ssPercent: Result := 'Percent';
        ssAuto: Result := 'Auto';
      else
        Result := '';
      end;
  end;

  function GetValueString(Index: Integer): string;
  begin
    with Items[Index] do
      if SizeStyle = ssAbsolute then
        Result := IntToStr(Trunc(Value))
      else if SizeStyle = ssPercent then
        Result := Format('%3.2f%%', [Value]) // do not localize
      else Result := 'Auto';
  end;

begin
  case Index of
    1: Result := GetSizeStyleString(ItemIndex);
    2: Result := GetValueString(ItemIndex);
  else
    Result := '';
  end;
end;

function TscCellCollection.Owner: TscCustomGridPanel;
begin
  Result := GetOwner as TscCustomGridPanel;
end;

procedure TscCellCollection.SetItem(Index: Integer; Value: TscCellItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TscCellCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner <> nil then
    with Owner do
    begin
      FRecalcCellSizes := True;
      Invalidate;
      Realign;
    end;
end;

{ TscRowCollection }

constructor TscRowCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRowItem);
end;

function TscRowCollection.Add: TscRowItem;
begin
  Result := TscRowItem(inherited Add);
end;

function TscRowCollection.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  if Index = 0 then
    Result := Format('Row%d', [ItemIndex])
  else
    Result := inherited GetItemAttr(Index, ItemIndex);
end;

procedure TscRowCollection.Notify(Item: TCollectionItem;
  Action: System.Classes.TCollectionNotification);
begin
  inherited;
  if (Action = System.Classes.TCollectionNotification.cnExtracting) and not (csDestroying in Owner.ComponentState) and
     not (csUpdating in Owner.ComponentState) then
    if Owner.IsRowEmpty(Item.Index) then
      Owner.UpdateControlsRow(Item.Index);
end;

{ TscColumnCollection }

constructor TscColumnCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TscColumnItem);
end;

function TscColumnCollection.Add: TscColumnItem;
begin
  Result := TscColumnItem(inherited Add);
end;

function TscColumnCollection.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  if Index = 0 then
    Result := Format('Column%d', [ItemIndex])
  else
    Result := inherited GetItemAttr(Index, ItemIndex);
end;

procedure TscColumnCollection.Notify(Item: TCollectionItem;
  Action: System.Classes.TCollectionNotification);
begin
  inherited;
  if (Action = System.Classes.TCollectionNotification.cnExtracting) and not (csDestroying in Owner.ComponentState) and
     not (csUpdating in Owner.ComponentState) then
    if not Owner.IsColumnEmpty(Item.Index) and not (csReading in Owner.ComponentState) then
    begin
      raise scEGridPanelException.Create(SCantDeleteColumn);
    end
    else
      Owner.UpdateControlsColumn(Item.Index);
end;

{ TscControlCollection }

function TscControlCollection.Add: TscControlItem;
begin
  Result := TscControlItem(inherited Add);
end;

procedure TscControlCollection.AddControl(AControl: TControl; AColumn, ARow: Integer);

  procedure PlaceInCell(ControlItem: TscControlItem);
  var
    I, J: Integer;
  begin
    with ControlItem do
    try
      Control := AControl;
      FRow := -1;
      FColumn := -1;
      if (ARow = -1) and (AColumn > -1) then
      begin
        for I := 0 to Owner.RowCollection.Count - 1 do
          if Controls[AColumn, I] = nil then
          begin
            Row := I;
            Column := AColumn;
            Exit;
          end;
        AColumn := -1;
      end;
      if (AColumn = -1) and (ARow > -1) then
      begin
        for I := 0 to Owner.ColumnCollection.Count - 1 do
          if Controls[I, ARow] = nil then
          begin
            Column := I;
            Row := ARow;
            Exit;
          end;
        ARow := -1;
      end;
      if (AColumn > -1) and (ARow > -1) then
      begin
        if Controls[AColumn, ARow] = nil then
        begin
          Column := AColumn;
          Row := ARow;
          Exit;
        end;
        AColumn := -1;
        ARow := -1;
      end;
      if (ARow = -1) and (AColumn = -1) then
      begin
        for J := 0 to Owner.RowCollection.Count - 1 do
          for I := 0 to Owner.ColumnCollection.Count - 1 do
            if Controls[I, J] = nil then
            begin
              Row := J;
              Column := I;
              Exit;
            end;
      end;
      if (Row = -1) or (Column = -1) then
        if (Owner <> nil) and (Owner.ExpandStyle <> emFixedSize) then
        begin
          if Owner.ExpandStyle = emAddRows then
            Owner.AutoAddRow
          else
            Owner.AutoAddColumn;
          PlaceInCell(ControlItem);
        end;
    except
      Control := nil;
      Free;
      raise;
    end;
  end;

begin
  if IndexOf(AControl) < 0 then
    PlaceInCell(Add);
end;

constructor TscControlCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TscControlItem);
end;

function TscControlCollection.GetControl(AColumn, ARow: Integer): TControl;
var
  ControlItem: TscControlItem;
begin
  ControlItem := GetControlItem(AColumn, ARow);
  if ControlItem <> nil then
    Result := ControlItem.Control
  else
    Result := nil;
end;

function TscControlCollection.GetControlItem(AColumn, ARow: Integer): TscControlItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TscControlItem(Items[I]);
    if (ARow >= Result.Row) and (ARow <= Result.Row + Result.RowSpan - 1) and
      (AColumn >= Result.Column) and (AColumn <= Result.Column + Result.ColumnSpan - 1) then
      Exit;
  end;
  Result := nil;
end;

function TscControlCollection.GetItem(Index: Integer): TscControlItem;
begin
  Result := TscControlItem(inherited GetItem(Index));
end;

function TscControlCollection.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
    if TscControlItem(Items[Result]).Control = AControl then
      Exit;
  Result := -1;
end;

function TscControlCollection.Owner: TscCustomGridPanel;
begin
  Result := TscCustomGridPanel(GetOwner);
end;

procedure TscControlCollection.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Control = AControl then
    begin
      Items[I].Control := nil;
      Delete(I);
      Exit;
    end;
end;

procedure TscControlCollection.SetControl(AColumn, ARow: Integer; Value: TControl);
var
  Index: Integer;
  ControlItem: TscControlItem;
begin
  if Owner <> nil then
  begin
    Index := IndexOf(Value);
    if Index > -1 then
    begin
      ControlItem := Items[Index];
      ControlItem.FColumn := AColumn;
      ControlItem.FRow := ARow;
    end else
      AddControl(Value, AColumn, ARow);
  end;
end;

procedure TscControlCollection.SetItem(Index: Integer; Value: TscControlItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TscControlCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner <> nil then
  begin
    Owner.FRecalcCellSizes := True;
    Owner.Invalidate;
    Owner.Realign;
  end;
end;

{ TscControlItem }

procedure TscControlItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TscControlItem then
    with TscControlItem(Dest) do
    begin
      FControl := Self.Control;
      FRow := Self.Row;
      FColumn := Self.Column;
      Changed(False);
    end;
end;

constructor TscControlItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FRowSpan := 1;
  FColumnSpan := 1;
end;

destructor TscControlItem.Destroy;
var
  LControl: TControl;
begin
  if Assigned(FControl) and not (csLoading in GridPanel.ComponentState) and
     not (csUpdating in GridPanel.ComponentState) and
     not (csDestroying in GridPanel.ComponentState) then
  begin
    LControl := FControl;
    FControl := nil;
    GridPanel.RemoveControl(LControl);
    LControl.Free;
  end;
  inherited;
end;

function TscControlItem.GetGridPanel: TscCustomGridPanel;
var
  Owner: TscControlCollection;
begin
  Owner := TscControlCollection(GetOwner);
  if Owner <> nil then
    Result := Owner.Owner
  else
    Result := nil;
end;

procedure TscControlItem.SetControl(Value: TControl);
begin
  if FControl <> Value then
  begin
    FControl := Value;
    Changed(False);
  end;
end;

procedure TscControlItem.SetColumn(Value: Integer);
begin
  if FColumn <> Value then
  begin
    if not (csLoading in GridPanel.ComponentState) then
    begin
      InternalSetLocation(Value, FRow, False, True);
    end
    else
      FColumn := Value;
  end;
end;

procedure TscControlItem.SetRow(Value: Integer);
begin
  if FRow <> Value then
  begin
    if not (csLoading in GridPanel.ComponentState) then
    begin
      InternalSetLocation(FColumn, Value, False, True);
    end
    else
      FRow := Value;
  end;
end;

type
  TNewLocationRec = record
    ControlItem: TscControlItem;
    NewColumn, NewRow: Integer;
    Pushed: Boolean;
  end;

  TNewLocationRecs = array of TNewLocationRec;

  TNewLocations = class
  private
    FNewLocations: TNewLocationRecs;
    FCount: Integer;
  public
    function AddNewLocation(AControlItem: TscControlItem; ANewColumn, ANewRow: Integer; APushed: Boolean = False): Integer;
    procedure ApplyNewLocations;
    property Count: Integer read FCount;
    property NewLocations: TNewLocationRecs read FNewLocations;
  end;

function TNewLocations.AddNewLocation(AControlItem: TscControlItem; ANewColumn, ANewRow: Integer; APushed: Boolean): Integer;
begin
  if FCount = Length(FNewLocations) then
    SetLength(FNewLocations, Length(FNewLocations) + 10);
  Result := FCount;
  Inc(FCount);
  with FNewLocations[Result] do
  begin
    ControlItem := AControlItem;
    NewColumn := ANewColumn;
    NewRow := ANewRow;
    Pushed := APushed;
  end;
end;

procedure TNewLocations.ApplyNewLocations;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FNewLocations[I] do
      if ControlItem <> nil then
        ControlItem.InternalSetLocation(NewColumn, NewRow, Pushed, False);
end;

procedure TscControlItem.SetRowSpan(Value: TscCellSpan);
var
  I, Delta: Integer;
  Collection: TscControlCollection;
  ControlItem: TscControlItem;
  NumToAdd, NumRows, MoveBy, LColumn, LRow, IndexDelta: Integer;
  Span: TscCellSpan;
  NewLocations: TNewLocations;
begin
  if FRowSpan <> Value then
  begin
    Collection := TscControlCollection(GetOwner);
    if Collection = nil then Exit;
    GridPanel.DisableAlign;
    try
      NewLocations := TNewLocations.Create;
      try
        if FRowSpan > Value then
        begin
          Delta := FRowSpan - Value;
          FRowSpan := Value;
          if GridPanel.ExpandStyle in [emAddRows, emFixedSize] then
          begin
            NumRows := GridPanel.RowCollection.Count;

            for I := FRow + FRowSpan + Delta to NumRows - 1 do
            begin
              ControlItem := Collection.ControlItems[FColumn, I];
              if ControlItem <> nil then
                if ControlItem.Pushed then
                  NewLocations.AddNewLocation(ControlItem, FColumn, I - Delta, False)
                else
                  Break;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddRows;
          end else
          begin
            for I := GridPanel.RowSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if ControlItem <> nil then
              begin
                if ControlItem.Pushed then
                begin
                  if (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
                  begin
                    GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    if (LRow > 0) and (LRow + ControlItem.RowSpan > GridPanel.RowCollection.Count) then
                    begin
                      Inc(Delta, (LRow + ControlItem.RowSpan) - GridPanel.RowCollection.Count);
                      GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    end;
                    NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
                  end;
                end else if ControlItem <> Self then
                  Break
                else
                  NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
              end;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddRows;
          end;
        end else
        begin
          NumRows := GridPanel.RowCollection.Count;
          Delta := Value - FRowSpan;
          for I := Min(FRow + FRowSpan, NumRows) to Min(FRow + Value - 1, NumRows - 1) do
            if Collection.Controls[FColumn, I] = nil then
              Dec(Delta)
            else
              Break;
          MoveBy := Delta;
          for I := NumRows - 1 downto NumRows - MoveBy do
            if Collection.Controls[FColumn, I] = nil then
              Dec(Delta)
            else
              Break;
          NumToAdd := Delta;
          if GridPanel.ExpandStyle in [emAddRows, emFixedSize] then
          begin
            while NumToAdd > 0 do
            begin
              GridPanel.AutoAddRow;
              Dec(NumToAdd);
            end;
            NumRows := GridPanel.RowCollection.Count;
            for I := NumRows - 1 downto NumRows - Delta do
            begin
              ControlItem := Collection.ControlItems[FColumn, I - MoveBy];
              if (ControlItem <> nil) and (ControlItem <> Self) then
                NewLocations.AddNewLocation(ControlItem, FColumn, I, True);
            end;
            NewLocations.ApplyNewLocations;
          end else if (NumToAdd + MoveBy) > 0 then
          begin
            IndexDelta := Max(NumToAdd, Min(MoveBy, NumRows));
            for I := GridPanel.RowSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if (ControlItem <> nil) and (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
              begin
                if ControlItem = Self then
                begin
                  Span := Value;
                  LColumn := FColumn;
                  LRow := FRow;
                end else
                begin
                  Span := ControlItem.RowSpan;
                  GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                end;
                if LRow + Span > GridPanel.RowCollection.Count then
                begin
                  if LRow > 0 then
                  begin
                    Inc(IndexDelta, GridPanel.RowCollection.Count - LRow);
                    GridPanel.CellIndexToCell(I + IndexDelta - NumToAdd, LColumn, LRow);
                  end else if ControlItem <> Self then
                  begin
                    Inc(IndexDelta, Min(Span, GridPanel.RowCollection.Count));
                    GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                  end else if (ControlItem = Self) and (LRow = 0) then
                    Exit;
                end;
                NumToAdd := 0;
                NewLocations.AddNewLocation(ControlItem, LColumn, LRow, True);
              end;
            end;
            for I := 0 to NewLocations.Count - 1 do
              if NewLocations.NewLocations[I].NewColumn > GridPanel.ColumnCollection.Count - 1 then
                GridPanel.AutoAddColumn;
            NewLocations.ApplyNewLocations;
          end;
          FRowSpan := Value;
        end;
        Changed(False);
      finally
        NewLocations.Free;
      end;
    finally
      GridPanel.EnableAlign;
    end;
  end;
end;

procedure TscControlItem.SetColumnSpan(Value: TscCellSpan);
var
  I, Delta: Integer;
  Collection: TscControlCollection;
  ControlItem: TscControlItem;
  NumToAdd, NumColumns, MoveBy, LColumn, LRow, IndexDelta: Integer;
  Span: TscCellSpan;
  NewLocations: TNewLocations;
begin
  if FColumnSpan <> Value then
  begin
    Collection := TscControlCollection(GetOwner);
    if Collection = nil then Exit;
    GridPanel.DisableAlign;
    try
      NewLocations := TNewLocations.Create;
      try
        if FColumnSpan > Value then
        begin
          Delta := FColumnSpan - Value;
          FColumnSpan := Value;
          if GridPanel.ExpandStyle in [emAddColumns, emFixedSize] then
          begin
            NumColumns := GridPanel.ColumnCollection.Count;
            for I := FColumn + FColumnSpan + Delta to NumColumns - 1 do
            begin
              ControlItem := Collection.ControlItems[I, FRow];
              if ControlItem <> nil then
                if ControlItem.Pushed then
                  NewLocations.AddNewLocation(ControlItem, I - Delta, FRow, False)
                else
                  Break;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddColumns;
          end else
          begin
            for I := GridPanel.ColumnSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if ControlItem <> nil then
              begin
                if ControlItem.Pushed then
                begin
                  if (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
                  begin
                    GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    if (LColumn > 0) and (LColumn + ControlItem.ColumnSpan > GridPanel.ColumnCollection.Count) then
                    begin
                      Inc(Delta, (LColumn + ControlItem.ColumnSpan) - GridPanel.ColumnCollection.Count);
                      GridPanel.CellIndexToCell(I - Delta, LColumn, LRow);
                    end;
                    NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
                  end;
                end else if ControlItem <> Self then
                  Break
                else
                  NewLocations.AddNewLocation(ControlItem, LColumn, LRow, False);
              end;
            end;
            NewLocations.ApplyNewLocations;
            GridPanel.RemoveEmptyAutoAddRows;
          end;
        end else
        begin
          NumColumns := GridPanel.ColumnCollection.Count;
          Delta := Value - FColumnSpan;
          for I := Min(FColumn + FColumnSpan, NumColumns) to Min(FColumn + Value - 1, NumColumns - 1) do
            if Collection.Controls[I, FRow] = nil then
              Dec(Delta)
            else
              Break;
          MoveBy := Delta;
          for I := NumColumns - 1 downto NumColumns - MoveBy do
            if Collection.Controls[I, FRow] = nil then
              Dec(Delta)
            else
              Break;
          NumToAdd := Delta;
          if GridPanel.ExpandStyle in [emAddColumns, emFixedSize] then
          begin
            while NumToAdd > 0 do
            begin
              GridPanel.AutoAddColumn;
              Dec(NumToAdd);
            end;
            NumColumns := GridPanel.ColumnCollection.Count;
            for I := NumColumns - 1 downto NumColumns - Delta do
            begin
              ControlItem := Collection.ControlItems[I - MoveBy, FRow];
              if (ControlItem <> nil) and (ControlItem <> Self) then
                NewLocations.AddNewLocation(ControlItem, I, FRow, True);
            end;
            NewLocations.ApplyNewLocations;
          end else if (NumToAdd + MoveBy) > 0 then
          begin
            IndexDelta := Max(NumToAdd, Min(MoveBy, NumColumns));
            for I := GridPanel.ColumnSpanIndex[FColumn, FRow] to GridPanel.CellCount - 1 do
            begin
              GridPanel.CellIndexToCell(I, LColumn, LRow);
              ControlItem := Collection.ControlItems[LColumn, LRow];
              if (ControlItem <> nil) and (ControlItem.Column = LColumn) and (ControlItem.Row = LRow) then
              begin
                if ControlItem = Self then
                begin
                  Span := Value;
                  LColumn := FColumn;
                  LRow := FRow;
                end else
                begin
                  Span := ControlItem.ColumnSpan;
                  GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                end;
                if LColumn + Span > GridPanel.ColumnCollection.Count then
                begin
                  if LColumn > 0 then
                  begin
                    Inc(IndexDelta, GridPanel.ColumnCollection.Count - LColumn);
                    GridPanel.CellIndexToCell(I + IndexDelta - NumToAdd, LColumn, LRow);
                  end else if ControlItem <> Self then
                  begin
                    Inc(IndexDelta, Min(Span, GridPanel.ColumnCollection.Count));
                    GridPanel.CellIndexToCell(I + IndexDelta, LColumn, LRow);
                  end else if (ControlItem = Self) and (LColumn = 0) then
                    Exit;
                end;
                NumToAdd := 0;
                NewLocations.AddNewLocation(ControlItem, LColumn, LRow, True);
              end;
            end;
            for I := 0 to NewLocations.Count - 1 do
              if NewLocations.NewLocations[I].NewRow > GridPanel.RowCollection.Count - 1 then
                GridPanel.AutoAddRow;
            NewLocations.ApplyNewLocations;
          end;
          FColumnSpan := Value;
        end;
        Changed(False);
      finally
        NewLocations.Free;
      end;
    finally
      GridPanel.EnableAlign;
    end;
  end;
end;

procedure TscControlItem.InternalSetLocation(AColumn, ARow: Integer; APushed, MoveExisting: Boolean);
var
  Collection: TscControlCollection;
  CurrentItem: TscControlItem;
begin
  if (AColumn <> FColumn) or (ARow <> FRow) then
  begin
    if MoveExisting then
    begin
      Collection := TscControlCollection(GetOwner);
      if Collection <> nil then
        CurrentItem := Collection.ControlItems[AColumn, ARow]
      else
        CurrentItem := nil;
      if CurrentItem <> nil then
        CurrentItem.InternalSetLocation(FColumn, FRow, False, False);
    end;
    FColumn := AColumn;
    FRow := ARow;
    if APushed then
      Inc(FPushed)
    else if FPushed > 0 then
      Dec(FPushed);
    Changed(False);
  end;
end;

procedure TscControlItem.SetLocation(AColumn, ARow: Integer; APushed: Boolean);
begin
  InternalSetLocation(AColumn, ARow, APushed, True);
end;

function TscControlItem.GetPushed: Boolean;
begin
  Result := FPushed > 0;
end;

initialization

  TCustomStyleEngine.RegisterStyleHook(TscPageViewerPage, TscScrollBoxStyleHook);

finalization

  {$IFNDEF VER230}
  TCustomStyleEngine.UnRegisterStyleHook(TscPageViewerPage, TscScrollBoxStyleHook);
  {$ENDIF}

end.

