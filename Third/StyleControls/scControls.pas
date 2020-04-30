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

unit scControls;

{$I scdefine.inc}
{$R-}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types,
    Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.Themes, Vcl.ImgList, WinApi.CommCtrl,
    Vcl.StdCtrls, Vcl.ExtCtrls, scDrawUtils, System.SysUtils, Vcl.Buttons,
    Vcl.CheckLst, Vcl.ToolWin, Vcl.GraphUtil,
    Vcl.Menus, Vcl.ComCtrls, scImageCollection, Vcl.Mask, WinApi.RichEdit, scHint;

  const
    WM_CHECKPARENTBG = WM_USER + 260;
    WM_MOUSEHOOKCANCELMODE = WM_USER + 270;

  type

    TscDrawTextMode = (scdtmGDI, scdtmGDIPlus);

    TscCustomControl = class(TCustomControl)
    private
      {$IFDEF VER230}
      FStyleElements: TStyleElements;
      {$ENDIF}
      FStopEraseBackground: Boolean;
      FromWMPaint: Boolean;
    protected
      FUseOnlyParentBackground: Boolean;
      ParentBGBuffer: TBitmap;
      FBlurBackground: Boolean;
      FBlurBackgroundAmount: Integer;

      FStorePaintBuffer: Boolean;
      FDrawOnBackground: Boolean;
      FTransparentBackground: Boolean;
      FDrawInClientRect: Boolean;
      {$IFNDEF VER330_UP}
      FScaleFactor: Double;
      FScalePercent: Integer;
      {$ENDIF}
      FOnChangeScale: TNotifyEvent;
      FUpdateParentBuffer: Boolean;
      FBackgroundLeft: Integer;
      FBackgroundTop: Integer;

      FFluentUIOpaque: Boolean;
      FDrawTextMode: TscDrawTextMode;

      procedure SetDrawTextMode(Value: TscDrawTextMode);

      procedure SetFluentUIOpaque(Value: Boolean);
      function IsFluentUIOpaque: Boolean;

      procedure SetBlurBackground(Value: Boolean);
      procedure SetBlurBackgroundAmount(Value: Integer);

      procedure SetTransparentBackground(Value: Boolean); virtual;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      function CanDrawOnBackground: Boolean; virtual;
      procedure GetParentBG; virtual;
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
      {$IFNDEF VER230}
      procedure UpdateStyleElements; override;
      {$ENDIF}
      function GetCtrlState: TscsCtrlState; virtual;
      procedure UpdateControls; virtual;
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
      procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
      procedure WMMove(var Msg: TMessage); message WM_MOVE;
      procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
      procedure DrawBackground(ACanvas: TCanvas); virtual;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); virtual;

      procedure WndProc(var Message: TMessage); override;
      procedure CreateWnd; override;
      procedure CreateParams(var Params: TCreateParams); override;

      function IsRightToLeft: Boolean;
      property TransparentBackground: Boolean read FTransparentBackground write SetTransparentBackground;
      property CtrlState: TscsCtrlState read GetCtrlState;
      property DrawOnBackground: Boolean read FDrawOnBackground write FDrawOnBackground;
      property BlurBackground: Boolean
        read FBlurBackground write SetBlurBackground;
      property BlurBackgroundAmount: Integer
        read FBlurBackgroundAmount write SetBlurBackgroundAmount;
      property DrawTextMode: TscDrawTextMode
        read FDrawTextMode write SetDrawTextMode;
    public
      FForceDraw: Boolean;
      FGetControlBG: Boolean;
      FPaintBuffer: TBitmap;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Paint; override;
      {$IFDEF VER310_UP}
      procedure ScaleForPPI(NewPPI: Integer); override;
      {$ENDIF}
      procedure RePaintControl; virtual;
      procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

      procedure SetRedraw(AValue: Boolean; AUpdate: Boolean = True);
      {$IFNDEF VER330_UP}
      property ScaleFactor: Double read FScaleFactor;
      {$ENDIF}
      property StorePaintBuffer: Boolean
        read FStorePaintBuffer write FStorePaintBuffer;
    published
      property Align;
      property Anchors;
      property BiDiMode;
      property Constraints;
      property DockSite;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      property Font;
      property Padding;
      property ParentBiDiMode;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Touch;
      property Visible;
     {$IFNDEF VER230}
      property StyleElements;
     {$ELSE}
      property StyleElements: TStyleElements
        read  FStyleElements write FStyleElements;
     {$ENDIF}
      property OnAlignInsertBefore;
      property OnAlignPosition;
      property OnClick;
      property OnCanResize;
      property OnContextPopup;
      property OnChangeScale: TNotifyEvent
        read FOnChangeScale write FOnChangeScale;
      property OnDblClick;
      property OnDragDrop;
      property OnDockDrop;
      property OnDockOver;
      property OnDragOver;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnGesture;
      property OnResize;
      property OnGetSiteInfo;
      property OnMouseActivate;
      property OnMouseDown;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDock;
      property OnStartDrag;
      property OnUnDock;
    end;

    TscCustomActiveControl = class(TscCustomControl)
    private
      FMouseTimer: TTimer;
      FCanAnimate: Boolean;
      FAnimation: Boolean;
      procedure StartMouseTimer;
      procedure StopMouseTimer;
    protected
      FMouseIn: Boolean;
      FOldCtrlState: TscsCtrlState;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      procedure DoMouseTimer(Sender: TObject); virtual;
      procedure DoMouseEnter; virtual;
      procedure DoMouseLeave; virtual;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      function GetCtrlState: TscsCtrlState; override;
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
      procedure WMMove(var Msg: TMessage); message WM_MOVE;
      procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
      procedure StartAnimationRendering;
      function CanAnimate: Boolean; virtual;
      function CanAnimateFocusedState: Boolean; virtual;
      function AnimationAvailable: Boolean;
      procedure DrawToAnimationDC(ADC: HDC; AState: TscsCtrlState; ARendering: Boolean);
      function StartAnimation(ADC: HDC): Boolean; virtual;
      procedure StopAnimation;
      property Animation: Boolean read FAnimation write FAnimation;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Paint; override;
    end;

  TscPanelStyleKind = (scpsPanel, scpsToolBar, scpsHeader, scpsFormBackground,
    scpsTransparent, scpsEmpty, scpsEdit, scpsTabSheet);

  TscPanelBorderStyle = (scpbsNone, scpbsFlat, scpbsRaised, scpbsLowered, scpbsLoweredBevel,
    scpbsRaisedBevel,
    scpbsTopBevel, scpbsBottomBevel,
    scpbsTopLightLine, scpbsBottomLightLine,
    scpbsTopShadowLine, scpbsBottomShadowLine,
    scpbsLeftBevel, scpbsRightBevel,
    scpbsLeftLightLine, scpbsRightLightLine,
    scpbsLeftShadowLine, scpbsRightShadowLine,
    scpbsHorzCenterBevel, scpbsVertCenterBevel);

  TscPanelPaintEvent = procedure(ACanvas: TCanvas; ARect: TRect) of object;

  TscPanel = class(TscCustomControl)
  protected
    FStyleKind: TscPanelStyleKind;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomImageIndex: Integer;
    FBorderStyle: TscPanelBorderStyle;
    FLightBorderColor: TColor;
    FShadowBorderColor: TColor;
    FShowCaption: Boolean;
    FCaptionGlowEffect: TscGlowEffect;
    FAlignment: TAlignment;
    FGlowBuffer: TBitmap;
    FStoredCaption: String;
    FStoredGlowColor: TColor;
    FStoredFontSize: Integer;
    FStoredFontName: String;
    FStoredFontStyle: TFontStyles;
    FUpdateGlowBuffer: Boolean;
    FOnPanelPaint: TscPanelPaintEvent;
    FCanEmpty: Boolean;
    FPopupMode: Boolean;
    FDragForm: Boolean;
    FDragTopForm: Boolean;
    FForm: TCustomForm;
    FDown: Boolean;
    FDownP: TPoint;

    function CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
    procedure SetShowCaption(Value: Boolean);
    procedure SetLightBorderColor(Value: TColor);
    procedure SetShadowBorderColor(Value: TColor);
    procedure SetBorderStyle(Value: TscPanelBorderStyle);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetStyleKind(Value: TscPanelStyleKind);
    procedure SetAlignment(Value: TAlignment);
    procedure CustomUpdateControl; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnGlowEffectChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function GetCaptionText: String; virtual;
    function GetCaptionColor: TColor; virtual;
    procedure DrawAdditionalContent(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; var ADrawCaption: Boolean); virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomImageIndex: Integer read FCustomImageIndex write SetCustomImageIndex;
    property DragForm: Boolean read FDragForm write FDragForm;
    property DragTopForm: Boolean read FDragTopForm write FDragTopForm;
    property StyleKind: TscPanelStyleKind read FStyleKind write SetStyleKind;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property BorderStyle: TscPanelBorderStyle read FBorderStyle write SetBorderStyle;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property LightBorderColor: TColor read FLightBorderColor write SetLightBorderColor;
    property ShadowBorderColor: TColor read FShadowBorderColor write SetShadowBorderColor;
    property CaptionGlowEffect: TscGlowEffect read FCaptionGlowEffect write FCaptionGlowEffect;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Color;
    property Caption;
    property StorePaintBuffer;
    property OnPanelPaint: TscPanelPaintEvent
      read FOnPanelPaint write FOnPanelPaint;
  end;

  TscGalleryMenu = class;

  TscMenuHeaderStyle = (scmhsDefault, scmhsBottomLine);

  TscGalleryMenuItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: String;
    FOnClick: TNotifyEvent;
    FButton: Boolean;
    FHeader: Boolean;
    FHint: String;
    FEnabled: Boolean;
  protected
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetCaption(const Value: String); virtual;
  public
    ItemRect: TRect;
    FColor: TColor;
    FShowColor: Boolean;
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Button: Boolean read FButton write FButton;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Header: Boolean read FHeader write FHeader;
    property Caption: String read FCaption write SetCaption;
    property Hint: String read FHint write FHint;
    property ImageIndex: Integer read FImageIndex
      write SetImageIndex default -1;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TscGalleryMenuItems = class(TCollection)
  private
    function GetItem(Index: Integer):  TscGalleryMenuItem;
    procedure SetItem(Index: Integer; Value:  TscGalleryMenuItem);
  protected
    function GetOwner: TPersistent; override;
  public
    GalleryMenu: TscGalleryMenu;
    constructor Create(AGalleryMenu: TscGalleryMenu);
    property Items[Index: Integer]:  TscGalleryMenuItem read GetItem write SetItem; default;
  end;

  TscGalleryMenuPopupWindow = class(TscPanel)
  private
    OldAppMessage: TMessageEvent;
    GalleryMenu: TscGalleryMenu;
    MouseInItem, OldMouseInItem: Integer;
    FDown: Boolean;
    FItemDown: Boolean;
    procedure AssignItemRects;
    procedure CreateMenu;
    procedure HookApp;
    procedure UnHookApp;
    procedure NewAppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
    procedure DrawButtonItem(Index: Integer; Cnvs: TCanvas; AActive, ADown: Boolean);
    procedure DrawItem(Index: Integer; Cnvs: TCanvas; AActive, ASelected: Boolean);
    procedure DrawItems(ActiveIndex, SelectedIndex: Integer; C: TCanvas);
    function GetItemRect(Index: Integer): TRect;
    function GetItemFromPoint(P: TPoint): Integer;
    procedure TestActive(X, Y: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMEraseBkGrnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;
    procedure ProcessKey(KeyCode: Integer);
    procedure FindLeft;
    procedure FindRight;
    procedure FindUp;
    procedure FindDown;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(PopupRect: TRect);
    procedure ShowExt(PopupRect: TRect; AFromTop, AFromRight: Boolean);
    procedure Hide(AProcessEvents: Boolean);
 end;

  TscGalleryMenuBGStyle = (scgmPanel, scgmToolBar, scgmFormBackground);

  TscGalleryMenu = class(TComponent)
  private
    FBackgroundStyle: TscGalleryMenuBGStyle;
    FImages: TCustomImageList;
    FItems: TscGalleryMenuItems;
    FColumnsCount: Integer;
    FOnItemClick: TNotifyEvent;
    FPopupWindow: TscGalleryMenuPopupWindow;
    FShowSelectedItem: Boolean;
    FOldItemIndex: Integer;
    FOnChange: TNotifyEvent;
    FOnInternalChange: TNotifyEvent;
    FOnMenuClose: TNotifyEvent;
    FOnMenuPopup: TNotifyEvent;
    FOnInternalMenuClose: TNotifyEvent;
    FItemFont: TFont;
    FHeaderFont: TFont;
    FHeaderHeight: Integer;
    FHeaderStyle: TscMenuHeaderStyle;
    FHintComponent: TscHint;
    FShowHints: Boolean;
    FButtonLeftAlignment: Boolean;
    FButtonGlyphLeftAlignment: Boolean;
    procedure SetItemFont(Value: TFont);
    procedure SetHeaderFont(Value: TFont);
    procedure SetItems(Value: TscGalleryMenuItems);
    procedure SetImages(Value: TCustomImageList);
    procedure SetColumnsCount(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    function GetSelectedItem: TscGalleryMenuItem;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ProcessEvents(ACanProcess: Boolean);
  public
    FItemIndex: Integer;
    FScaleFactor: Double;
    FScalePercent: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(X, Y: Integer);
    procedure PopupFromRect(R: TRect);
    procedure PopupExt(X, Y: Integer; AFromTop, AFromRight: Boolean);
    procedure PopupFromRectExt(R: TRect; AFromTop, AFromRight: Boolean);
    procedure Hide;
    property SelectedItem: TscGalleryMenuItem read GetSelectedItem;
    property OnInternalChange: TNotifyEvent read FOnInternalChange write FOnInternalChange;
    property OnInternalMenuClose: TNotifyEvent read FOnInternalMenuClose write FOnInternalMenuClose;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TscGalleryMenuItems read FItems write SetItems;
    property HeaderStyle: TscMenuHeaderStyle
      read FHeaderStyle write FHeaderStyle;
    property HeaderHeight: Integer
      read  FHeaderHeight write FHeaderHeight;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property HintComponent: TscHint
      read FHintComponent write FHintComponent;
    property BackgroundStyle: TscGalleryMenuBGStyle read
      FBackgroundStyle write FBackgroundStyle;
    property ItemFont: TFont read FItemFont write SetItemFont;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ColumnsCount: Integer read FColumnsCount write SetColumnsCount;
    property ShowSelectedItem: Boolean read FShowSelectedItem write FShowSelectedItem;
    property ShowHints: Boolean read FShowHints write FShowHints;
    property ButtonLeftAlignment: Boolean
      read FButtonLeftAlignment write FButtonLeftAlignment;
    property ButtonGlyphLeftAlignment: Boolean
      read FButtonGlyphLeftAlignment write FButtonGlyphLeftAlignment;
    property OnItemClick: TNotifyEvent read FOnItemClick write FOnItemClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMenuPopup: TNotifyEvent read FOnMenuPopup write FOnMenuPopup;
    property OnMenuClose: TNotifyEvent read FOnMenuClose write FOnMenuClose;
  end;

  TscButtonStyleKind = (scbsPushButton, scbsToolButton,
    scbsPushButtonTransparent, scbsToolButtonTransparent, scbsTransparent,
    scbsLink, scbsHeaderSection, scbsUpSpinButton, scbsDownSpinButton,
    scbsLeftSpinButton, scbsRightSpinButton, scbsDropDownButton,
    scbsColorButton, scbsCustomImage, scbsCustomImageOverContent,
    scbsTab, scbsTabTransparent, scbsSegmentedLeft, scbsSegmentedMiddle,
      scbsSegmentedRight, scbsSegmentedToolLeft, scbsSegmentedToolMiddle,
      scbsSegmentedToolRight, scbsDropDownCombo);

    TscArrowPosition = (scapRight, scapBottom);
    TscArrowDirection = (scadDefault, scadRight);

    TscButtonCloseUpEvent = procedure(Sender: TObject; AAcceptChanges: Boolean) of object;

    TscCustomButtonControl = class(TscCustomActiveControl)
    private
      FLayout: TButtonLayout;
      FImages: TCustomImageList;
      FImageIndex: Integer;
      FRepeatClick: Boolean;
      FRepeatClickInterval: Integer;
      FRepeatClickTimer: TTimer;
      FDropDownMenu: TPopupMenu;
      FSplitButton: Boolean;
      FShowFocusRect: Boolean;
      FGroupIndex: Integer;
      FAllowAllUp: Boolean;
      FMustDoUp: Boolean;
      FGlowEffect: TscButtonGlowEffect;
      FImageGlow: Boolean;
      FGalleryMenu: TscGalleryMenu;
      FShowGalleryMenuFromTop: Boolean;
      FShowGalleryMenuFromRight: Boolean;
      FShowMenuArrow: Boolean;
      FWordWrap: Boolean;
      FUseImagesFromAction: Boolean;
      FUseImageIndexFromAction: Boolean;
      FCustomDropDown: Boolean;
      FOnDropDown: TNotifyEvent;
      FOnCloseUp: TscButtonCloseUpEvent;
      procedure SetArrowDirection(Value: TscArrowDirection);
      procedure SetCustomDropDown(Value: Boolean);
      procedure SetWordWrap(Value: Boolean);
      procedure SetImageGlow(Value: Boolean);
      procedure SetCanFocused(Value: Boolean);
      procedure SetMargin(Value: Integer);
      procedure SetSpacing(Value: Integer);
      procedure SetImageIndex(Value: Integer);
      procedure SetImages(Value: TCustomImageList);
      procedure SetLayout(Value: TButtonLayout);
      procedure RepeatClickTimerProc(Sender: TObject);
      procedure WaitClickTimerProc(Sender: TObject);
      procedure StartRepeatClick;
      procedure StopRepeatClick;
      procedure SetDropDownMenu(Value: TPopupMenu);
      procedure SetGalleryMenu(Value: TscGalleryMenu);
      procedure SetSplitButton(Value: Boolean);
      procedure SetShowMenuArrow(Value: Boolean);
    protected
      FCanFocused: Boolean;
      FMargin: Integer;
      FSpacing: Integer;
      FGlowBuffer: TBitmap;
      FGlowImageBuffer: TBitmap;
      FStoredCaption: String;
      FStoredWidth, FStoredHeight: Integer;
      FStoredGlowColor: TColor;
      FStoredGlowSize: Integer;
      FStoredGlowAlphaValue: Integer;
      FStoredRect: TRect;
      FStoredFontSize: Integer;
      FStoredFontName: String;
      FStoredFontStyle: TFontStyles;
      FStoredImageIndex: Integer;
      FStoredSpacing: Integer;
      FStoredMargin: Integer;
      FStoredLayout: TButtonLayout;
      FStoredImageList: TCustomImageList;
      FStoredImageListW,
      FStoredImageListH: Integer;
      FStoredWordWrap: Boolean;
      FUpdateGlowBuffer: Boolean;
      FDown: Boolean;
      FPressed, FIsDown, FMenuDroppedDown, FMenuTracking: Boolean;
      FDisableClick: Boolean;
      FSplitWidth: Integer;
      FArrowPosition: TscArrowPosition;
      FArrowDirection: TscArrowDirection;
      FDropDownCall: Boolean;
      FOnClick: TNotifyEvent;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure SetArrowPosition(Value: TscArrowPosition);
      function CanUpdateGlowBuffer(ACanvas: TCanvas;
        AGlowColor: TColor; AGlowSize, AGlowAlpha: Byte; ARect: TRect;
        AImageIndex: Integer; AImageList: TCustomImageList): Boolean;
      procedure Click; override;
      procedure OnGalleryMenuChanged(Sender: TObject);
      procedure OnGalleryMenuClose(Sender: TObject);
      function CanAnimate: Boolean; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure OnGlowEffectChange(Sender: TObject);
      function GetGlowParams(ACtrlState: TscsCtrlState;
         var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean; virtual;
      procedure CheckGroupIndex; virtual;
      procedure SetDown(Value: Boolean);
      procedure DoDropDownMenu;
      procedure TrackMenu(ADropDownPressed: Boolean);
      procedure TrackGalleryMenu(ADropDownPressed: Boolean);
      procedure TrackCustomWindow(ADropDownPressed: Boolean);
      function GetCtrlState: TscsCtrlState; override;
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
      procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
      procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure WMSetFocus(var Message: TWMSETFOCUS); message WM_SETFOCUS;
      procedure WMKillFocus(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
      procedure WndProc(var Message: TMessage); override;
      procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
      procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
      procedure DoDialogChar; virtual;
      procedure ButtonClick; virtual;
      procedure DoMouseEnter; override;
      procedure DoMouseLeave; override;
      property CustomDropDown: Boolean
        read FCustomDropDown write SetCustomDropDown;
      property RepeatClick: Boolean read FRepeatClick write FRepeatClick;
      property RepeatClickInterval: Integer read FRepeatClickInterval write FRepeatClickInterval;
      property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
      property SplitButton: Boolean read FSplitButton write SetSplitButton;
      property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
      property Down: Boolean Read FDown write SetDown;
      property GroupIndex: Integer read FGroupIndex write FGroupIndex;
      property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp;
      property GlowEffect: TscButtonGlowEffect read FGlowEffect write FGlowEffect;
      property ImageGlow: Boolean read FImageGlow write SetImageGlow;
      property GalleryMenu: TscGalleryMenu read FGalleryMenu write SetGalleryMenu;
      property ShowGalleryMenuFromTop: Boolean
       read FShowGalleryMenuFromTop write FShowGalleryMenuFromTop;
      property ShowGalleryMenuFromRight: Boolean read FShowGalleryMenuFromRight
        write FShowGalleryMenuFromRight;
      property ShowMenuArrow: Boolean read FShowMenuArrow write SetShowMenuArrow;
     property ArrowDirection: TscArrowDirection
        read FArrowDirection write SetArrowDirection default scadDefault;
      property ArrowPosition: TscArrowPosition
        read FArrowPosition write SetArrowPosition default scapRight;
      property WordWrap: Boolean
        read FWordWrap write SetWordWrap default True;
      property UseImagesFromAction: Boolean
        read FUseImagesFromAction write FUseImagesFromAction default True;
      property UseImageIndexFromAction: Boolean
        read FUseImageIndexFromAction write FUseImageIndexFromAction default True;
      property OnDropDown: TNotifyEvent read
        FOnDropDown write FOnDropDown;
      property OnCloseUp: TscButtonCloseUpEvent read
        FOnCloseUp write FOnCloseUp;
    public
      property DisableClick: Boolean read FDisableClick write FDisableClick;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure CloseUp(AAcceptChanges: Boolean);
      property CanFocused: Boolean read FCanFocused write SetCanFocused;
      property Margin: Integer read FMargin write SetMargin;
      property Spacing: Integer read FSpacing write SetSpacing;
      property Layout: TButtonLayout read FLayout write SetLayout;
      property Images: TCustomImageList read FImages write SetImages;
      property ImageIndex: Integer read FImageIndex write SetImageIndex;
      property OnClick: TNotifyEvent read FOnClick write FOnClick;
    end;

    TscButtonColorOptions = class(TPersistent)
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
      FTitleFontNormalColor: TColor;
      FTitleFontHotColor: TColor;
      FTitleFontPressedColor: TColor;
      FTitleFontFocusedColor: TColor;
      FTitleFontDisabledColor: TColor;

      FStyleColors: Boolean;
      FState: TscsCtrlState;
      FOnChange: TNotifyEvent;

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

      function GetTitleFontNormalColor: TColor;
      function GetTitleFontHotColor: TColor;
      function GetTitleFontPressedColor: TColor;
      function GetTitleFontFocusedColor: TColor;
      function GetTitleFontDisabledColor: TColor;

      function GetColor: TColor;
      function GetFrameColor: TColor;
      function GetFontColor: TColor;
      function GetTitleFontColor: TColor;

      procedure SetNormalColor(Value: TColor);
      procedure SetHotColor(Value: TColor);
      procedure SetPressedColor(Value: TColor);
      procedure SetFocusedColor(Value: TColor);
      procedure SetDisabledColor(Value: TColor);
      procedure SetFrameNormalColor(Value: TColor);
      procedure SetFrameHotColor(Value: TColor);
      procedure SetFramePressedColor(Value: TColor);
      procedure SetFrameFocusedColor(Value: TColor);
      procedure SetFrameDisabledColor(Value: TColor);

      procedure SetFontNormalColor(Value: TColor);
      procedure SetFontHotColor(Value: TColor);
      procedure SetFontPressedColor(Value: TColor);
      procedure SetFontFocusedColor(Value: TColor);
      procedure SetFontDisabledColor(Value: TColor);

      procedure SetTitleFontNormalColor(Value: TColor);
      procedure SetTitleFontHotColor(Value: TColor);
      procedure SetTitleFontPressedColor(Value: TColor);
      procedure SetTitleFontFocusedColor(Value: TColor);
      procedure SetTitleFontDisabledColor(Value: TColor);

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
      property TitleFontColor: TColor read GetTitleFontColor;
    published
      property NormalColor: TColor read GetNormalColor write SetNormalColor;
      property HotColor: TColor read GetHotColor write SetHotColor;
      property PressedColor: TColor read GetPressedColor write SetPressedColor;
      property FocusedColor: TColor read GetFocusedColor write SetFocusedColor;
      property DisabledColor: TColor read GetDisabledColor write SetDisabledColor;

      property FrameNormalColor: TColor read GetFrameNormalColor write SetFrameNormalColor;
      property FrameHotColor: TColor read GetFrameHotColor write SetFrameHotColor;
      property FramePressedColor: TColor read GetFramePressedColor write SetFramePressedColor;
      property FrameFocusedColor: TColor read GetFrameFocusedColor write SetFrameFocusedColor;
      property FrameDisabledColor: TColor read GetFrameDisabledColor write SetFrameDisabledColor;
      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

      property FontNormalColor: TColor read GetFontNormalColor write SetFontNormalColor;
      property FontHotColor: TColor read GetFontHotColor write SetFontHotColor;
      property FontPressedColor: TColor read GetFontPressedColor write SetFontPressedColor;
      property FontFocusedColor: TColor read GetFontFocusedColor write SetFontFocusedColor;
      property FontDisabledColor: TColor read GetFontDisabledColor write SetFontDisabledColor;

      property TitleFontNormalColor: TColor read GetTitleFontNormalColor write SetTitleFontNormalColor;
      property TitleFontHotColor: TColor read GetTitleFontHotColor write SetTitleFontHotColor;
      property TitleFontPressedColor: TColor read GetTitleFontPressedColor write SetTitleFontPressedColor;
      property TitleFontFocusedColor: TColor read GetTitleFontFocusedColor write SetTitleFontFocusedColor;
      property TitleFontDisabledColor: TColor read GetTitleFontDisabledColor write SetTitleFontDisabledColor;

      property StyleColors: Boolean read FStyleColors write SetStyleColors;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TscPaintButtonEvent = procedure(ACanvas: TCanvas; ARect: TRect;
      AState: TscsCtrlState) of object;

    TscPaintControlEvent = TscPaintButtonEvent;

    TscButton = class(TscCustomButtonControl)
    private
      FColorOptions: TscButtonColorOptions;
      FTitle: String;
      FTitleFont: TFont;
      FHotImageIndex: Integer;
      FFocusedImageIndex: Integer;
      FPressedImageIndex: Integer;
      FStyleKind: TscButtonStyleKind;
      FUseGalleryMenuImage: Boolean;
      FUseGalleryMenuCaption: Boolean;
      FCustomImages: TscCustomImageCollection;
      FCustomImageNormalIndex: Integer;
      FCustomImageHotIndex: Integer;
      FCustomImagePressedIndex: Integer;
      FCustomImageFocusedIndex: Integer;
      FCustomImageDisabledIndex: Integer;
      FOnPaintContent: TscPaintButtonEvent;
      FScaleMarginAndSpacing: Boolean;
      FWidthWithCaption: Integer;
      FWidthWithoutCaption: Integer;
      FShowCaption: Boolean;
      FActive: Boolean;
      FDefault: Boolean;
      FCancel: Boolean;
      FModalResult: TModalResult;
      FModalSetting: Boolean;
      FImageMargin: Integer;
      FUseFontColorToStyleColor: Boolean;
      procedure SetImageMargin(Value: Integer);
      procedure SetShowCaption(Value: Boolean);
      procedure SetCustomImageNormalIndex(Value: Integer);
      procedure SetCustomImageHotIndex(Value: Integer);
      procedure SetCustomImagePressedIndex(Value: Integer);
      procedure SetCustomImageFocusedIndex(Value: Integer);
      procedure SetCustomImageDisabledIndex(Value: Integer);
      procedure SetCustomImages(Value: TscCustomImageCollection);
      procedure SetUseGalleryMenuImage(Value: Boolean);
      procedure SetUseGalleryMenuCaption(Value: Boolean);
      procedure SetTitle(Value: String);
      procedure SetTitleFont(Value: TFont);
      procedure OnTitleFontChange(Sender: TObject);
      procedure OnColorOptionsChange(Sender: TObject);
      function GetCurrentImageIndex(ACtrlState: TscsCtrlState): Integer;
      procedure SetStyleKind(Value: TscButtonStyleKind);
    protected
      FToolPushButtonStyle: Boolean;
      function GetCtrlState: TscsCtrlState; override;
      procedure DoDialogChar; override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      function CanAnimateFocusedState: Boolean; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
      procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
      procedure CheckGroupIndex; override;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure ButtonClick; override;
    published
      property Action;
      property ArrowDirection;
      property ArrowPosition;
      property Animation;
      property Caption;
      property CanFocused;
      property CustomDropDown;
      property Margin;
      property Spacing;
      property Layout;
      property Images;
      property ImageIndex;
      property ImageMargin: Integer
        read FImageMargin write SetImageMargin;
      property TransparentBackground;
      property Default: Boolean
        read FDefault write FDefault default False;
      property Cancel: Boolean
        read FCancel write FCancel default False;
      property ColorOptions: TscButtonColorOptions
        read FColorOptions write FColorOptions;
      property Title: String read FTitle write SetTitle;
      property TitleFont: TFont read FTitleFont write SetTitleFont;
      property HotImageIndex: Integer read FHotImageIndex write FHotImageIndex;
      property ModalResult: TModalResult read
        FModalResult write FModalResult default mrNone;
      property ModalSetting: Boolean read
        FModalSetting write FModalSetting default False;
      property FocusedImageIndex: Integer read FFocusedImageIndex write FFocusedImageIndex;
      property PressedImageIndex: Integer read FPressedImageIndex write FPressedImageIndex;
      property StyleKind: TscButtonStyleKind read FStyleKind write SetStyleKind;
      property UseGalleryMenuImage: Boolean read
          FUseGalleryMenuImage write SetUseGalleryMenuImage;
      property UseGalleryMenuCaption: Boolean
        read FUseGalleryMenuCaption write SetUseGalleryMenuCaption;
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
      property ScaleMarginAndSpacing: Boolean read
        FScaleMarginAndSpacing write FScaleMarginAndSpacing;
      property WidthWithCaption: Integer read FWidthWithCaption write FWidthWithCaption;
      property WidthWithoutCaption: Integer read FWidthWithoutCaption write FWidthWithoutCaption;
      property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
      property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
      property RepeatClick;
      property RepeatClickInterval;
      property GlowEffect;
      property ImageGlow;
      property DropDownMenu;
      property GalleryMenu;
      property ShowGalleryMenuFromTop;
      property ShowGalleryMenuFromRight;
      property ShowMenuArrow;
      property SplitButton;
      property ShowFocusRect;
      property Down;
      property GroupIndex;
      property AllowAllUp;
      property WordWrap;
      property UseImagesFromAction;
      property UseImageIndexFromAction;
      property OnDropDown;
      property OnCloseUp;
      property OnPaintContent: TscPaintButtonEvent
        read FOnPaintContent write FOnPaintContent;
      property OnClick;
    end;

    TscGroupBoxFramePosition = (scgfpDefault, scgfpOverText, scgfpUnderText, scgfpNone);

    TscGroupBox = class(TscCustomControl)
    private
      FFramePosition: TscGroupBoxFramePosition;
      FImages: TCustomImageList;
      FImageIndex: Integer;
      FAlignment: TAlignment;
      FShowCheckBox: Boolean;
      FChecked: Boolean;
      FAutoEnabledControls: Boolean;
      FOnChecked: TNotifyEvent;
      FCaptionRect: TRect;
      FGlowEffect: TscGlowEffect;
      FImageGlow: Boolean;
      FCaptionMouseIn, FCaptionMouseDown: Boolean;
      FCustomImages: TscCustomImageCollection;
      FCustomImageIndex: Integer;
      FCustomImageDisabledIndex: Integer;
      FCustomCheckedImageIndex: Integer;
      FCustomCheckedImageHotIndex: Integer;
      FCustomCheckedImagePressedIndex: Integer;
      FCustomCheckedImageDisabledIndex: Integer;
      FCustomUnCheckedImageIndex: Integer;
      FCustomUnCheckedImageHotIndex: Integer;
      FCustomUnCheckedImagePressedIndex: Integer;
      FCustomUnCheckedImageDisabledIndex: Integer;

      FGlowBuffer: TBitmap;
      FGlowImageBuffer: TBitmap;
      FStoredCaption: String;
      FStoredWidth, FStoredHeight: Integer;
      FStoredGlowColor: TColor;
      FStoredFontSize: Integer;
      FStoredFontName: String;
      FStoredFontStyle: TFontStyles;
      FStoredImageIndex: Integer;
      FStoredAlignment: TAlignment;
      FStoredImageList: TCustomImageList;
      FUpdateGlowBuffer: Boolean;
      function CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;

      procedure SetCustomCheckedImageIndex(Value: Integer);
      procedure SetCustomCheckedImageHotIndex(Value: Integer);
      procedure SetCustomCheckedImagePressedIndex(Value: Integer);
      procedure SetCustomCheckedImageDisabledIndex(Value: Integer);
      procedure SetCustomUnCheckedImageIndex(Value: Integer);
      procedure SetCustomUnCheckedImageHotIndex(Value: Integer);
      procedure SetCustomUnCheckedImagePressedIndex(Value: Integer);
      procedure SetCustomUnCheckedImageDisabledIndex(Value: Integer);
      procedure SetCustomImages(Value: TscCustomImageCollection);
      procedure SetCustomImageIndex(Value: Integer);
      procedure SetCustomImageDisabledIndex(Value: Integer);
      procedure SetImageGlow(Value: Boolean);
      procedure SetImages(Value: TCustomImageList);
      procedure SetImageIndex(Value: Integer);
      procedure SetAlignment(Value: TAlignment);
      procedure SetShowCheckBox(Value: Boolean);
      procedure SetChecked(Value: Boolean);
      procedure SetFramePosition(Value: TscGroupBoxFramePosition);
      function GetCaptionHeight(ACanvas: TCanvas): Integer;
      procedure DrawCustomCheckBox(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
      function GetCustomCheckBoxSize: TPoint;
    protected
      procedure OnGlowEffectChange(Sender: TObject);
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
      procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure AdjustClientRect(var Rect: TRect); override;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property FramePosition: TscGroupBoxFramePosition
        read FFramePosition write SetFramePosition;
      property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
      property CustomImageIndex: Integer read FCustomImageIndex write SetCustomImageIndex;
      property CustomImageDisabledIndex: Integer read FCustomImageDisabledIndex write SetCustomImageDisabledIndex;
      property CustomCheckedImageIndex: Integer
        read FCustomCheckedImageIndex write SetCustomCheckedImageIndex;
      property CustomCheckedImageHotIndex: Integer
        read FCustomCheckedImageHotIndex write SetCustomCheckedImageHotIndex;
      property CustomCheckedImagePressedIndex: Integer
        read FCustomCheckedImagePressedIndex write SetCustomCheckedImagePressedIndex;
      property CustomCheckedImageDisabledIndex: Integer
        read FCustomCheckedImageDisabledIndex write SetCustomCheckedImageDisabledIndex;
      property CustomUnCheckedImageIndex: Integer
        read FCustomUnCheckedImageIndex write SetCustomUnCheckedImageIndex;
      property CustomUnCheckedImageHotIndex: Integer
        read FCustomUnCheckedImageHotIndex write SetCustomUnCheckedImageHotIndex;
      property CustomUnCheckedImagePressedIndex: Integer
        read FCustomUnCheckedImagePressedIndex write SetCustomUnCheckedImagePressedIndex;
      property CustomUnCheckedImageDisabledIndex: Integer
        read FCustomUnCheckedImageDisabledIndex write SetCustomUnCheckedImageDisabledIndex;
      property Alignment: TAlignment read FAlignment write SetAlignment;
      property Caption;
      property Images: TCustomImageList read FImages write SetImages;
      property ImageIndex: Integer read FImageIndex write SetImageIndex;
      property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox;
      property Checked: Boolean read FChecked write SetChecked;
      property AutoEnabledControls: Boolean read FAutoEnabledControls write FAutoEnabledControls;
      property GlowEffect: TscGlowEffect read FGlowEffect write FGlowEffect;
      property ImageGlow: Boolean read FImageGlow write SetImageGlow;
      property StorePaintBuffer;
      property OnChecked: TNotifyEvent read FOnChecked write FOnChecked;
    end;

    TscCheckBox = class(TscCustomButtonControl)
    private
      FUseFontColorToStyleColor: Boolean;
      FAllowGrayed: Boolean;
      FCustomImages: TscCustomImageCollection;
      FCustomCheckedImageIndex: Integer;
      FCustomCheckedImageHotIndex: Integer;
      FCustomCheckedImagePressedIndex: Integer;
      FCustomCheckedImageDisabledIndex: Integer;
      FCustomUnCheckedImageIndex: Integer;
      FCustomUnCheckedImageHotIndex: Integer;
      FCustomUnCheckedImagePressedIndex: Integer;
      FCustomUnCheckedImageDisabledIndex: Integer;
      FCustomGrayedImageIndex: Integer;
      FCustomGrayedImageHotIndex: Integer;
      FCustomGrayedImagePressedIndex: Integer;
      FCustomGrayedImageDisabledIndex: Integer;
      procedure SetCustomImages(Value: TscCustomImageCollection);
      procedure SetCustomCheckedImageIndex(Value: Integer);
      procedure SetCustomCheckedImageHotIndex(Value: Integer);
      procedure SetCustomCheckedImagePressedIndex(Value: Integer);
      procedure SetCustomCheckedImageDisabledIndex(Value: Integer);
      procedure SetCustomUnCheckedImageIndex(Value: Integer);
      procedure SetCustomUnCheckedImageHotIndex(Value: Integer);
      procedure SetCustomUnCheckedImagePressedIndex(Value: Integer);
      procedure SetCustomUnCheckedImageDisabledIndex(Value: Integer);
      procedure SetCustomGrayedImageIndex(Value: Integer);
      procedure SetCustomGrayedImageHotIndex(Value: Integer);
      procedure SetCustomGrayedImagePressedIndex(Value: Integer);
      procedure SetCustomGrayedImageDisabledIndex(Value: Integer);
      function GetChecked: Boolean;
      procedure SetChecked(Value: Boolean);
      procedure DrawCustomCheckBox(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
      function GetCustomCheckBoxSize: TPoint;
    protected
      FState: TCheckBoxState;
      FClickDisabled: Boolean;
      procedure DoDialogChar; override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure SetState(Value: TCheckBoxState); virtual;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
      procedure ButtonClick; override;
      function GetCtrlState: TscsCtrlState; override;
      function GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property ClickDisabled: Boolean read FClickDisabled write FClickDisabled;
    published
      property CustomImages: TscCustomImageCollection
        read FCustomImages write SetCustomImages;
      property CustomCheckedImageIndex: Integer
        read FCustomCheckedImageIndex write SetCustomCheckedImageIndex;
      property CustomCheckedImageHotIndex: Integer
        read FCustomCheckedImageHotIndex write SetCustomCheckedImageHotIndex;
      property CustomCheckedImagePressedIndex: Integer
        read FCustomCheckedImagePressedIndex write SetCustomCheckedImagePressedIndex;
      property CustomCheckedImageDisabledIndex: Integer
        read FCustomCheckedImageDisabledIndex write SetCustomCheckedImageDisabledIndex;
      property CustomUnCheckedImageIndex: Integer
        read FCustomUnCheckedImageIndex write SetCustomUnCheckedImageIndex;
      property CustomUnCheckedImageHotIndex: Integer
        read FCustomUnCheckedImageHotIndex write SetCustomUnCheckedImageHotIndex;
      property CustomUnCheckedImagePressedIndex: Integer
        read FCustomUnCheckedImagePressedIndex write SetCustomUnCheckedImagePressedIndex;
      property CustomUnCheckedImageDisabledIndex: Integer
        read FCustomUnCheckedImageDisabledIndex write SetCustomUnCheckedImageDisabledIndex;
      property CustomGrayedImageIndex: Integer
        read FCustomGrayedImageIndex write SetCustomGrayedImageIndex;
      property CustomGrayedImageHotIndex: Integer
        read FCustomGrayedImageHotIndex write SetCustomGrayedImageHotIndex;
      property CustomGrayedImagePressedIndex: Integer
        read FCustomGrayedImagePressedIndex write SetCustomGrayedImagePressedIndex;
      property CustomGrayedImageDisabledIndex: Integer
        read FCustomGrayedImageDisabledIndex write SetCustomGrayedImageDisabledIndex;
      property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
      property Action;
      property Animation;
      property Caption;
      property CanFocused;
      property Spacing;
      property Layout;
      property Images;
      property ImageIndex;
      property GlowEffect;
      property ImageGlow;
      property Checked: Boolean read GetChecked write SetChecked;
      property State: TCheckBoxState read FState write SetState default cbUnchecked;
      property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
      property ShowFocusRect;
      property WordWrap;
      property UseImagesFromAction;
      property UseImageIndexFromAction;

      property OnClick;
    end;

    TscRadioButton = class(TscCustomButtonControl)
    private
      FUseFontColorToStyleColor: Boolean;
      FChecked: Boolean;
      FCustomImages: TscCustomImageCollection;
      FCustomCheckedImageIndex: Integer;
      FCustomCheckedImageHotIndex: Integer;
      FCustomCheckedImagePressedIndex: Integer;
      FCustomCheckedImageDisabledIndex: Integer;
      FCustomUnCheckedImageIndex: Integer;
      FCustomUnCheckedImageHotIndex: Integer;
      FCustomUnCheckedImagePressedIndex: Integer;
      FCustomUnCheckedImageDisabledIndex: Integer;
      procedure SetCustomImages(Value: TscCustomImageCollection);
      procedure SetCustomCheckedImageIndex(Value: Integer);
      procedure SetCustomCheckedImageHotIndex(Value: Integer);
      procedure SetCustomCheckedImagePressedIndex(Value: Integer);
      procedure SetCustomCheckedImageDisabledIndex(Value: Integer);
      procedure SetCustomUnCheckedImageIndex(Value: Integer);
      procedure SetCustomUnCheckedImageHotIndex(Value: Integer);
      procedure SetCustomUnCheckedImagePressedIndex(Value: Integer);
      procedure SetCustomUnCheckedImageDisabledIndex(Value: Integer);
      procedure SetChecked(Value: Boolean);
      procedure DrawCustomCheckBox(ACanvas: TCanvas; ARect: TRect;
        ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
      function GetCustomCheckBoxSize: TPoint;
    protected
      FClickDisabled: Boolean;
      procedure DoDialogChar; override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
      procedure ButtonClick; override;
      function GetCtrlState: TscsCtrlState; override;
      function GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean; override;
    public
      constructor Create(AOwner: TComponent); override;
      property ClickDisabled: Boolean read FClickDisabled write FClickDisabled;
    published
       property CustomImages: TscCustomImageCollection
        read FCustomImages write SetCustomImages;
      property CustomCheckedImageIndex: Integer
        read FCustomCheckedImageIndex write SetCustomCheckedImageIndex;
      property CustomCheckedImageHotIndex: Integer
        read FCustomCheckedImageHotIndex write SetCustomCheckedImageHotIndex;
      property CustomCheckedImagePressedIndex: Integer
        read FCustomCheckedImagePressedIndex write SetCustomCheckedImagePressedIndex;
      property CustomCheckedImageDisabledIndex: Integer
        read FCustomCheckedImageDisabledIndex write SetCustomCheckedImageDisabledIndex;
      property CustomUnCheckedImageIndex: Integer
        read FCustomUnCheckedImageIndex write SetCustomUnCheckedImageIndex;
      property CustomUnCheckedImageHotIndex: Integer
        read FCustomUnCheckedImageHotIndex write SetCustomUnCheckedImageHotIndex;
      property CustomUnCheckedImagePressedIndex: Integer
        read FCustomUnCheckedImagePressedIndex write SetCustomUnCheckedImagePressedIndex;
      property CustomUnCheckedImageDisabledIndex: Integer
        read FCustomUnCheckedImageDisabledIndex write SetCustomUnCheckedImageDisabledIndex;
      property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
      property Action;
      property Animation;
      property Caption;
      property CanFocused;
      property Spacing;
      property Layout;
      property Images;
      property ImageIndex;
      property GlowEffect;
      property ImageGlow;
      property Checked: Boolean read FChecked write SetChecked;
      property ShowFocusRect;
      property WordWrap;
      property UseImagesFromAction;
      property UseImageIndexFromAction;
      property OnClick;
    end;

    TscSelectionStyle = (scstStyled, scstColor);

    TscDrawItemEvent = procedure(AIndex: Integer; AState: TOwnerDrawState;
      ACanvas: TCanvas; ARect: TRect) of object;

    TscGetDrawItemParamsEvent = procedure(AIndex: Integer; AState: TOwnerDrawState;
      ACanvas: TCanvas; var AImages: TCustomImageList; var AImageIndex: Integer) of object;

    TscCustomListBox = class(TCustomListBox)
    private
      FImages: TCustomImageList;
      FImageIndex: Integer;
      FWordBreak: Boolean;
      FOnDrawItem: TscDrawItemEvent;
      FOnGetDrawItemParams: TscGetDrawItemParamsEvent;
      FTitleDivider: String;
      FShowLines: Boolean;
      FLineColor: TColor;
      FSelectionStyle: TscSelectionStyle;
      FShowFocusRect: Boolean;
      FSelectionColor: TColor;
      FSelectionTextColor: TColor;
      FTabWidths: TStrings;

      procedure SetTabWidths(Value: TStrings);
      procedure SetSelectionColor(Value: TColor);
      procedure SetSelectionTextColor(Value: TColor);
      procedure SetSelectionStyle(Value: TscSelectionStyle);
      procedure SetLineColor(Value: TColor);
      procedure SetShowLines(Value: Boolean);
      procedure SetImages(Value: TCustomImageList);
      procedure SetImageIndex(Value: Integer);
      procedure SetWordBreak(Value: Boolean);
      procedure SetTitleDivider(Value: String);
    protected
      {$IFNDEF VER330_UP}
      FScaleFactor: Double;
      FScalePercent: Integer;
      {$ENDIF}
      // Windows 10 Fluient UI
      FFluentUIOpaque: Boolean;
      procedure SetFluentUIOpaque(Value: Boolean);
      function IsFluentUIOpaque: Boolean;
      //
      procedure CreateWnd; override;
      procedure WndProc(var Message: TMessage); override;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      function CanDrawFocusRect: Boolean;
      procedure DrawItemContent(ACanvas: TCanvas; ARect:
        TRect; AIndex: Integer; AState: TOwnerDrawState); virtual;
      procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
      procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      property Images: TCustomImageList read FImages write SetImages;
      property ImageIndex: Integer read FImageIndex write SetImageIndex;
      property WordBreak: Boolean read FWordBreak write SetWordBreak;
      property TitleDivider: String read FTitleDivider write SetTitleDivider;
      property LineColor: TColor read FLineColor write SetLineColor;
      property ShowLines: Boolean read FShowLines write SetShowLines;
      property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
      property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
      property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
      property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
      property TabWidths: TStrings read FTabWidths write SetTabWidths;
      property OnDrawItem: TscDrawItemEvent read FOnDrawItem write FOnDrawItem;
      property OnGetDrawItemParams: TscGetDrawItemParamsEvent read FOnGetDrawItemParams write FOnGetDrawItemParams;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
    end;

    TscListBox = class(TscCustomListBox)
    public
      procedure AddMRUItem(Value: String);
    published
      property Items;
      property ItemIndex;
      property MultiSelect;
      property Columns;
      property Images;
      property ImageIndex;
      property WordBreak;
      property TitleDivider;
      property LineColor;
      property ShowLines;
      property Sorted;
      property SelectionStyle;
      property ShowFocusRect;
      property SelectionColor;
      property SelectionTextColor;
      property TabWidths;
      property Align;
      property AutoComplete;
      property Anchors;
      property BevelEdges;
      property BevelInner;
      property BevelKind;
      property BevelOuter;
      property BorderStyle;
      property BiDiMode;
      property Color;
      property Constraints;
      property Ctl3D;
      property DoubleBuffered;
      property Enabled;
      property Font;
      property ItemHeight;
      property ParentBiDiMode;
      property ParentColor;
      property ParentCtl3D;
      property ParentDoubleBuffered;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Touch;
      property Visible;
      {$IFNDEF VER230}
      property StyleElements;
      {$ENDIF}
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

      property OnDrawItem;
      property OnGetDrawItemParams;
    end;

    TscCheckListBox = class(TCheckListBox)
    private
      FImages: TCustomImageList;
      FImageIndex: Integer;
      FWordBreak: Boolean;
      FOnDrawItem: TscDrawItemEvent;
      FOnGetDrawItemParams: TscGetDrawItemParamsEvent;
      FTitleDivider: String;
      FShowLines: Boolean;
      FLineColor: TColor;
      FSelectionStyle: TscSelectionStyle;
      FShowFocusRect: Boolean;
      FSelectionColor: TColor;
      FSelectionTextColor: TColor;
      FTabWidths: TStrings;
      FCheckBoxWidth: Integer;
      procedure SetTabWidths(Value: TStrings);
      procedure SetSelectionColor(Value: TColor);
      procedure SetSelectionTextColor(Value: TColor);
      function CanDrawFocusRect: Boolean;
      procedure SetSelectionStyle(Value: TscSelectionStyle);
      procedure SetLineColor(Value: TColor);
      procedure SetShowLines(Value: Boolean);
      procedure SetImages(Value: TCustomImageList);
      procedure SetImageIndex(Value: Integer);
      procedure SetWordBreak(Value: Boolean);
      procedure SetTitleDivider(Value: String);
    protected
      {$IFNDEF VER330_UP}
      FScaleFactor: Double;
      FScalePercent: Integer;
      {$ENDIF}
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure DrawItemContent(ACanvas: TCanvas; ARect: TRect;
        AIndex: Integer; AState: TOwnerDrawState); virtual;
      procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure KeyPress(var Key: Char); override;
      procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property Items;
      property ItemIndex;
      property MultiSelect;
      property Images: TCustomImageList read FImages write SetImages;
      property ImageIndex: Integer read FImageIndex write SetImageIndex;
      property WordBreak: Boolean read FWordBreak write SetWordBreak;
      property TitleDivider: String read FTitleDivider write SetTitleDivider;
      property LineColor: TColor read FLineColor write SetLineColor;
      property ShowLines: Boolean read FShowLines write SetShowLines;
      property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
      property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
      property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
      property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
      property TabWidths: TStrings read FTabWidths write SetTabWidths;
      property Align;
      property AutoComplete;
      property Anchors;
      property BevelEdges;
      property BevelInner;
      property BevelKind;
      property BevelOuter;
      property BiDiMode;
      property Color;
      property Constraints;
      property Ctl3D;
      property DoubleBuffered;
      property Enabled;
      property Font;
      property ItemHeight;
      property ParentBiDiMode;
      property ParentColor;
      property ParentCtl3D;
      property ParentDoubleBuffered;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Touch;
      property Visible;
      {$IFNDEF VER230}
      property StyleElements;
      {$ENDIF}
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
      property OnDrawItem: TscDrawItemEvent read FOnDrawItem write FOnDrawItem;
      property OnGetDrawItemParams: TscGetDrawItemParamsEvent read FOnGetDrawItemParams write FOnGetDrawItemParams;
    end;

    TscComboStyleKind = (scscbDefault, scscbPushButton);

    TscCustomComboBox = class(TCustomComboBox)
    private
      FImages: TCustomImageList;
      FImageIndex: Integer;
      FWordBreak: Boolean;
      FOnDrawItem: TscDrawItemEvent;
      FOnGetDrawItemParams: TscGetDrawItemParamsEvent;
      FTitleDivider: String;
      FSelectionStyle: TscSelectionStyle;
      FItemHeight: Integer;
      FSelectionColor: TColor;
      FSelectionTextColor: TColor;
      FStyleKind: TscComboStyleKind;
      FShowFocusRect: Boolean;
      FTempItemIndex: Integer;
      procedure SetSelectionColor(Value: TColor);
      procedure SetSelectionTextColor(Value: TColor);
      procedure SetWordBreak(Value: Boolean);
      procedure SetTitleDivider(Value: String);
      procedure SetSelectionStyle(Value: TscSelectionStyle);
      procedure SetImages(Value: TCustomImageList);
      procedure SetImageIndex(Value: Integer);
      procedure SetStyleKind(Value: TscComboStyleKind);
    protected
      {$IFNDEF VER330_UP}
      FScaleFactor: Double;
      FScalePercent: Integer;
      {$ENDIF}
      // Windows 10 Fluient UI
      FFluentUIOpaque: Boolean;
      procedure SetFluentUIOpaque(Value: Boolean);
      function IsFluentUIOpaque: Boolean;
      //
      procedure CreateWnd; override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); overload; override;
      function IsCustomDraw: Boolean;
      function IsFocused: Boolean;
      procedure SetStyle(Value: TComboBoxStyle); override;
      procedure SetItemHeight(Value: Integer); override;
      procedure PaintCustomCombo(ACanvas: TCanvas);
      procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
      procedure DrawItemContent(ACanvas: TCanvas; ARect: TRect;
        AIndex: Integer; AState: TOwnerDrawState; AComboItem: Boolean); virtual;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
      procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
      procedure CNCommand(var Message: TWMCOMMAND); message CN_COMMAND;
      procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
      procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
      procedure CMSEPaint(var Message: TMessage); message CM_SEPAINT;
      procedure WndProc(var Message: TMessage); override;
      property WordBreak: Boolean read FWordBreak write SetWordBreak;
      property TitleDivider: String read FTitleDivider write SetTitleDivider;
      property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
      property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
      property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
      property StyleKind: TscComboStyleKind read FStyleKind write SetStyleKind;
      property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;

      property Images: TCustomImageList read FImages write SetImages;
      property ImageIndex: Integer read FImageIndex write SetImageIndex;
      property ItemHeight: Integer read FItemHeight write SetItemHeight;

      property OnDrawItem: TscDrawItemEvent read FOnDrawItem write FOnDrawItem;
      property OnGetDrawItemParams: TscGetDrawItemParamsEvent read FOnGetDrawItemParams write FOnGetDrawItemParams;
    public
      procedure PaintToDC(DC: HDC; X, Y: Integer);
      constructor Create(AOwner: TComponent); override;
    published
      property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
    end;

    TscComboBox = class(TscCustomComboBox)
    public
      procedure AddMRUItem(Value: String);
    published
      property Style;
      property Sorted;
      property ItemIndex;
      property Items;

      property WordBreak;
      property TitleDivider;
      property SelectionStyle;
      property SelectionColor;
      property SelectionTextColor;

      property Images;
      property ImageIndex;
      property ItemHeight;

      property Align;
      property AutoComplete;
      property Anchors;
      property BevelEdges;
      property BevelInner;
      property BevelKind;
      property BevelOuter;
      property BiDiMode;
      property Color;
      property Constraints;
      property Ctl3D;
      property DoubleBuffered;
      property DropDownCount;
      property Enabled;
      property Font;
      property ParentBiDiMode;
      property ParentColor;
      property ParentCtl3D;
      property ParentDoubleBuffered;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Touch;
      property Text;
      property Visible;
      property StyleKind;
      property ShowFocusRect;
      {$IFNDEF VER230}
      property StyleElements;
      {$ENDIF}
      property OnClick;
      property OnCloseUp;
      property OnChange;
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
      property OnSelect;
      property OnDrawItem;
      property OnGetDrawItemParams;
    end;

   TscScrollingStyleHook = class(TMouseTrackControlStyleHook)
    strict private type
      TscScrollWindow = class(TWinControl)
    strict private
      FStyleHook: TscScrollingStyleHook;
      FVertical: Boolean;
      procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
      procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
      procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    strict protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure WndProc(var Message: TMessage); override;
    public
      constructor Create(AOwner: TComponent); override;
      procedure PaintToDC(DC: HDC);
      property StyleHook: TscScrollingStyleHook read FStyleHook write FStyleHook;
      property Vertical: Boolean read FVertical write FVertical;
    end;
  strict private
    FHorzDownState: TThemedScrollBar;
    FHorzScrollWnd: TscScrollWindow;
    FHorzSliderState: TThemedScrollBar;
    FHorzUpState: TThemedScrollBar;
    FLeftButtonDown: Boolean;
    FListPos: Single;
    FPrevScrollPos: Integer;
    FScrollPos: Single;
    FVertDownState: TThemedScrollBar;
    FVertScrollWnd: TscScrollWindow;
    FVertSliderState: TThemedScrollBar;
    FVertUpState: TThemedScrollBar;
    FInitingScrollBars: Boolean;
    FCallNCDefPaint: Boolean;
    function GetHorzDownButtonRect: TRect;
    function GetHorzScrollRect: TRect;
    function GetHorzSliderRect: TRect;
    function GetHorzTrackRect: TRect;
    function GetHorzUpButtonRect: TRect;
    function GetParentBounds: TRect;
    function GetVertDownButtonRect: TRect;
    function GetVertScrollRect: TRect;
    function GetVertSliderRect: TRect;
    function GetVertTrackRect: TRect;
    function GetVertUpButtonRect: TRect;
    function IsPopupWindow: Boolean;
    procedure InitScrollBars;
    procedure InitScrollState;
    procedure UpdateScroll;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMLButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Msg: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseMove(var Msg: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonUp(var Msg: TWMMouse); message WM_NCLBUTTONUP;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMNCLButtonDblClk(var Msg: TWMMouse); message WM_NCLBUTTONDBLCLK;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMShowWindow(var Msg: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMClose(var Msg: TWMCLOSE); message WM_CLOSE;
    procedure WMNCPAINT(var Message: TWMNCPAINT); message WM_NCPAINT;
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
  strict protected
    FIsPopupWindow: Boolean;
    FIsMouseInScrolls: Boolean;
    FNeedUpdateNCArea: Boolean;
    procedure DrawBorder; virtual;
    procedure DrawBorderToDC(DC: HDC); virtual;
    procedure DrawHorzScroll(DC: HDC); virtual;
    procedure DrawVertScroll(DC: HDC); virtual;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure PaintScroll; virtual;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
    property HorzDownButtonRect: TRect read GetHorzDownButtonRect;
    property HorzDownState: TThemedScrollBar read FHorzDownState write FHorzDownState;
    property HorzScrollRect: TRect read GetHorzScrollRect;
    property HorzSliderRect: TRect read GetHorzSliderRect;
    property HorzSliderState: TThemedScrollBar read FHorzSliderState write FHorzSliderState;
    property HorzTrackRect: TRect read GetHorzTrackRect;
    property HorzUpButtonRect: TRect read GetHorzUpButtonRect;
    property HorzUpState: TThemedScrollBar read FHorzUpState write FHorzUpState;
    property LeftButtonDown: Boolean read FLeftButtonDown;
    property ListPos: Single read FListPos write FListPos;
    property ParentBounds: TRect read GetParentBounds;
    property PrevScrollPos: Integer read FPrevScrollPos write FPrevScrollPos;
    property ScrollPos: Single read FScrollPos write FScrollPos;
    property VertDownButtonRect: TRect read GetVertDownButtonRect;
    property VertDownState: TThemedScrollBar read FVertDownState write FVertDownState;
    property VertScrollRect: TRect read GetVertScrollRect;
    property VertSliderRect: TRect read GetVertSliderRect;
    property VertSliderState: TThemedScrollBar read FVertSliderState write FVertSliderState;
    property VertTrackRect: TRect read GetVertTrackRect;
    property VertUpButtonRect: TRect read GetVertUpButtonRect;
    property VertUpState: TThemedScrollBar read FVertUpState write FVertUpState;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

  TscTreeViewStyleHook = class(TscScrollingStyleHook)
  strict private
    procedure TVMSetBkColor(var Message: TMessage); message TVM_SETBKCOLOR;
    procedure TVMSetTextColor(var Message: TMessage); message TVM_SETTEXTCOLOR;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
  strict protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;


    TscExpandButtonStyle = (scebsArrow, scebsButton, scebsModernArrow);
    TscTreeViewCheckChangedEvent = procedure(Sender: TObject; Node: TTreeNode; NewState: Boolean) of object;
    TscTreeViewCheckChangingEvent = procedure(Sender: TObject; Node: TTreeNode; NewState: Boolean;
      var AllowChanges: Boolean) of object;
    TscTreeViewCustomDrawItemTextEvent = procedure(Sender: TCustomTreeView; ACanvas: TCanvas; ARect: TRect;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean) of object;

    TscCustomTreeView = class(TCustomTreeView)
    protected
      {$IFNDEF VER330_UP}
      FScaleFactor: Double;
      FScalePercent: Integer;
      {$ENDIF}
      FThirdImages, FThirdImageWidth, FThirdImageHeight,
      FThirdImagesCount: Integer;
      FButtonSize: Integer;
      FButtonImages: TCustomImageList;
      FButtonCollapseImageIndex: Integer;
      FButtonExpandImageIndex: Integer;
      FSelectionStyle: TscSelectionStyle;
      FShowFocusRect: Boolean;
      FButtonStyle: TscExpandButtonStyle;
      FDefaultDraw: Boolean;
      FButtonColor: TColor;
      FButtonGlyphColor: TColor;
      FSelectionColor: TColor;
      FSelectionTextColor: TColor;
      FCheckingTree: boolean;

      FCheckBoxes: Boolean;
      FOnNodeCheckedChanged: TscTreeViewCheckChangedEvent;
      FOnNodeCheckedChanging: TscTreeViewCheckChangingEvent;
      FOnCustomDrawItemText: TscTreeViewCustomDrawItemTextEvent;
      FCheckHierarchy: Boolean;
      FCheckSetting: Boolean;

       // Windows 10 Fluient UI
      FFluentUIOpaque: Boolean;
      procedure SetFluentUIOpaque(Value: Boolean);
      function IsFluentUIOpaque: Boolean;
      //
      procedure CreateWnd; override;
      procedure CreateParams(var Params: TCreateParams); override;

      procedure SetCheckBoxes(Value: Boolean);
      function GetChecked(Node: TTreeNode): Boolean;
      procedure SetChecked(Node: TTreeNode; Value: Boolean);
      procedure UpdateCheckNodes(Node: TTreeNode);

      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure WndProc(var Message: TMessage); override;
      procedure SetButtonColor(Value: TColor);
      procedure SetButtonGlyphColor(Value: TColor);
      procedure SetSelectionColor(Value: TColor);
      procedure SetSelectionTextColor(Value: TColor);


      procedure DoNodeCheckedChanged(Node:TTreeNode; NewState: Boolean); virtual;
      procedure DoNodeCheckedChanging(Node:TTreeNode; NewState: Boolean; var Allow: Boolean); virtual;
      procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
      procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
      procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
      procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;

      function CanDrawFocusRect: Boolean;
      procedure SetDefaultDraw(Value: Boolean);
      procedure SetButtonStyle(Value: TscExpandButtonStyle);
      procedure SetSelectionStyle(Value: TscSelectionStyle);
      procedure SetButtonImages(Value: TCustomImageList);
      procedure SetButtonCollapseImageIndex(Value: Integer);
      procedure SetButtonExpandImageIndex(Value: Integer);
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure HookAdvancedCustomDrawItem(
        Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
        Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
      procedure DrawButton(ACanvas: TCanvas; AColor: TColor; ARect: TRect; ANode: TTreeNode; ALineColor: TColor);
      procedure DrawImage(ACanvas: TCanvas; ANodeRect: TRect; ANode: TTreeNode; AImageIndex: Integer);
      property ButtonImages: TCustomImageList read FButtonImages write SetButtonImages;
      property ButtonCollapseImageIndex: Integer read FButtonCollapseImageIndex write SetButtonCollapseImageIndex;
      property ButtonExpandImageIndex: Integer read FButtonExpandImageIndex write SetButtonExpandImageIndex;
      property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
      property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
      property ButtonStyle: TscExpandButtonStyle read  FButtonStyle write SetButtonStyle;
      property DefaultDraw: Boolean read FDefaultDraw write SetDefaultDraw;
      property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
      property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
      property ButtonColor: TColor read FButtonColor write SetButtonColor;
      property ButtonGlyphColor: TColor read FButtonGlyphColor write SetButtonGlyphColor;
      property CheckBoxes: Boolean read FCheckBoxes write SetCheckBoxes;
      property CheckHierarchy: Boolean read FCheckHierarchy write FCheckHierarchy default True;
      property OnNodeCheckedChanged: TscTreeViewCheckChangedEvent read FOnNodeCheckedChanged write FOnNodeCheckedChanged;
      property OnNodeCheckedChanging: TscTreeViewCheckChangingEvent read FOnNodeCheckedChanging write FOnNodeCheckedChanging;
      property OnCustomDrawItemText: TscTreeViewCustomDrawItemTextEvent
        read FOnCustomDrawItemText write FOnCustomDrawItemText;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure FullRedraw;
      property Checked[Node: TTreeNode]: Boolean read GetChecked write SetChecked;
    published
      property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
    end;

    TscTreeView = class(TscCustomTreeView)
    published
      property Align;
      property Anchors;
      property AutoExpand;
      property BevelEdges;
      property BevelInner;
      property BevelOuter;
      property BevelKind default bkNone;
      property BevelWidth;
      property BiDiMode;
      property BorderStyle;
      property BorderWidth;
      property ChangeDelay;
      property CheckBoxes;
      property CheckHierarchy;
      property Color;
      property Ctl3D;
      property Constraints;
      property DoubleBuffered;
      property DragKind;
      property DragCursor;
      property DragMode;
      property Enabled;
      property Font;
      property HideSelection;
      property HotTrack;
      property Images;
      property Indent;
      property MultiSelect;
      property MultiSelectStyle;
      property ParentBiDiMode;
      property ParentColor default False;
      property ParentCtl3D;
      property ParentDoubleBuffered;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ReadOnly;
      property RightClickSelect;
      property ShowButtons;
      property ShowHint;
      property ShowRoot;
      property ShowLines;
      property SortType;
      property TabOrder;
      property TabStop default True;
      property ToolTips;
      property Touch;
      property Visible;
      {$IFNDEF VER230}
      property StyleElements;
      {$ENDIF}
      property Items;
      property ButtonImages;
      property ButtonCollapseImageIndex;
      property ButtonExpandImageIndex;
      property SelectionStyle;
      property SelectionColor;
      property SelectionTextColor;
      property ButtonColor;
      property ButtonGlyphColor;
      property ShowFocusRect;
      property DefaultDraw;
      property ButtonStyle;
      property OnAddition;
      property OnCancelEdit;
      property OnCustomDrawItemText;
      property OnChange;
      property OnChanging;
      property OnClick;
      property OnCollapsed;
      property OnCollapsing;
      property OnCompare;
      property OnContextPopup;
      property OnCreateNodeClass;
      property OnCustomDraw;
      property OnCustomDrawItem;
      property OnDblClick;
      property OnDeletion;
      property OnDragDrop;
      property OnDragOver;
      property OnEdited;
      property OnEditing;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnExpanding;
      property OnExpanded;
      property OnGesture;
      property OnGetImageIndex;
      property OnGetSelectedIndex;
      property OnHint;
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
      property OnNodeCheckedChanged;
      property OnNodeCheckedChanging;
    end;

    TscDrawSubItemEvent = procedure(Sender: TCustomListView;
     Item: TListItem; SubItemIndex: Integer; State: TCustomDrawState;
       ACanvas: TCanvas; ADrawRect: TRect) of object;

    TscDrawItemImageEvent = procedure(AItem: TListItem;
      ACanvas: TCanvas; ARect: TRect) of object;

    TscListViewColumnType = (scctDefault, scctProgress, scctCheck);

    TscCustomListView = class(TCustomListView)
    private
      FDefaultDraw: Boolean;
      FSelectionStyle: TscSelectionStyle;
      FShowFocusRect: Boolean;
      FThirdImages, FThirdImageWidth, FThirdImageHeight,
      FThirdImagesCount: Integer;
      FOnDrawSubItem: TscDrawSubItemEvent;
      FAlternateRow: Boolean;
      FGridLines: Boolean;
      FSelectionColor: TColor;
      FSelectionTextColor: TColor;
      FOnDrawItemImage: TscDrawItemImageEvent;
      FExtendedColumnDraw: Boolean;
      procedure SetExtendedColumnDraw(Value: Boolean);
      procedure SetSelectionColor(Value: TColor);
      procedure SetSelectionTextColor(Value: TColor);
      function CanDrawFocusRect: Boolean;
      procedure SetDefaultDraw(Value: Boolean);
      procedure SetGridLines(Value: Boolean);
      procedure SetAlternateRow(Value: Boolean);
      procedure SetSelectionStyle(Value: TscSelectionStyle);
    protected
      {$IFNDEF VER330_UP}
      FScaleFactor: Double;
      FScalePercent: Integer;
      {$ENDIF}
       // Windows 10 Fluient UI
      FFluentUIOpaque: Boolean;
      procedure SetFluentUIOpaque(Value: Boolean);
      function IsFluentUIOpaque: Boolean;
      //
      procedure CreateParams(var Params: TCreateParams); override;
      procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
      procedure WndProc(var Message: TMessage); override;
      procedure Loaded; override;
      procedure HookAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
        State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);  virtual;
      procedure CreateWnd; override;
      property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
      property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
      property AlternateRow: Boolean read FAlternateRow write SetAlternateRow;
      property GridLines: Boolean read FGridLines write SetGridLines;
      property DefaultDraw: Boolean read FDefaultDraw write SetDefaultDraw;
      property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
      property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
      property ExtendedColumnDraw: Boolean
        read FExtendedColumnDraw write SetExtendedColumnDraw;
      property OnDrawSubItem: TscDrawSubItemEvent
        read FOnDrawSubItem write FOnDrawSubItem;
      property OnDrawItemImage: TscDrawItemImageEvent read
         FOnDrawItemImage write FOnDrawItemImage;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure FullRedraw;
    published
      property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
    end;

  TscListViewStyleHook = class(TscScrollingStyleHook)
  strict private
    FHeaderHandle: HWnd;
    FHeaderInstance: Pointer;
    FDefHeaderProc: Pointer;
    FHotSection, FPrevHotSection, FPressedSection: Integer;
    FHeaderLBtnDown: Boolean;
    procedure LVMSetBkColor(var Message: TMessage); message LVM_SETBKCOLOR;
    procedure LVMSetTextBkColor(var Message: TMessage); message LVM_SETTEXTBKCOLOR;
    procedure LVMSetTextColor(var Message: TMessage); message LVM_SETTEXTCOLOR;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
  strict protected
    procedure DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: Integer;
      const Text: string; IsHot, IsPressed, IsBackground: Boolean); virtual;
    procedure HeaderWndProc(var Message: TMessage); virtual;
    procedure PaintHeader(DC: HDC); virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure CMSEPaint(var Message: TMessage); message CM_SEPAINT;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

    TscListView = class(TscCustomListView)
    published
      property Action;
      property Align;
      property AllocBy;
      property Anchors;
      property BevelEdges;
      property BevelInner;
      property BevelOuter;
      property BevelKind default bkNone;
      property BevelWidth;
      property BiDiMode;
      property BorderStyle;
      property BorderWidth;
      property Checkboxes;
      property Color;
      property Columns;
      property ColumnClick;
      property Constraints;
      property Ctl3D;
      property DragCursor;
      property DragKind;
      property DragMode;
      property DoubleBuffered;
      property Enabled;
      property ExtendedColumnDraw;
      property Font;
      property FlatScrollBars;
      property HideSelection;
      property HotTrack;
      property HotTrackStyles;
      property HoverTime;
      property IconOptions;
      property Items;
      property LargeImages;
      property MultiSelect;
      property OwnerData;
      property OwnerDraw;
      property ReadOnly default False;
      property RowSelect;
      property ParentBiDiMode;
      property ParentColor default False;
      property ParentDoubleBuffered;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowColumnHeaders;
      property ShowWorkAreas;
      property ShowHint;
      property SmallImages;
      property SortType;
      property StateImages;
      property TabOrder;
      property TabStop default True;
      property Touch;
      property ViewStyle;
      property Visible;

      property SelectionStyle;
      property ShowFocusRect;
      property AlternateRow;
      property GridLines;
      property DefaultDraw;
      property SelectionColor;
      property SelectionTextColor;

      {$IFNDEF VER230}
      property StyleElements;
      {$ENDIF}

      property OnChange;
      property OnChanging;
      property OnClick;
      property OnColumnClick;
      property OnColumnDragged;
      property OnColumnRightClick;
      property OnCompare;
      property OnContextPopup;
      property OnCreateItemClass;
      property OnData;
      property OnDataFind;
      property OnDataHint;
      property OnDataStateChange;
      property OnDblClick;
      property OnDeletion;
      property OnDrawItem;
      property OnEdited;
      property OnEditing;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnGesture;
      property OnGetImageIndex;
      property OnGetSubItemImage;
      property OnDragDrop;
      property OnDragOver;
      property OnInfoTip;
      property OnInsert;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseActivate;
      property OnMouseDown;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseMove;
      property OnMouseUp;
      property OnResize;
      property OnSelectItem;
      property OnItemChecked;
      property OnStartDock;
      property OnStartDrag;
      property OnDrawSubItem;
      property OnDrawItemImage;
    end;

  TscVertAlignment = (scvtaTop, scvtaCenter, scvtaBottom);

  TscLabel = class(TscCustomControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FVertAlignment: TscVertAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FGlowEffect: TscGlowEffect;
    FForm: TCustomForm;
    FDown: Boolean;
    FDownP: TPoint;
    FShowEllipsis: Boolean;

    FGlowBuffer: TBitmap;
    FStoredCaption: String;
    FStoredWidth, FStoredHeight: Integer;
    FStoredGlowColor: TColor;
    FStoredFontSize: Integer;
    FStoredFontName: String;
    FStoredFontStyle: TFontStyles;
    FUpdateGlowBuffer: Boolean;
    FStoredAlignment: TAlignment;
    FStoredShowAccelChar: Boolean;
    FStoredLayout: TTextLayout;
    FStoredWordWrap: Boolean;
    FStoredShowEllipsis: Boolean;
    FDragForm: Boolean;
    FDragTopForm: Boolean;

    FUseFontColorToStyleColor: Boolean;

    function CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;

    procedure SetShowEllipsis(Value: Boolean);
    procedure SetVertAlignment(Value: TscVertAlignment);
    procedure SetAlignment(Value: TAlignment);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);
    procedure SetWordWrap(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure Loaded; override;
    procedure OnGlowEffectChange(Sender: TObject);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure AdjustBounds; dynamic;
    procedure DoDrawText(ACanvas: TCanvas; var ARect: TRect; AFlags: Longint); dynamic;
    function GetLabelText: string; virtual;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure SetLabelAutoSize(Value: Boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DragForm: Boolean
      read FDragForm write FDragForm;
    property DragTopForm: Boolean read FDragTopForm write FDragTopForm;
    property GlowEffect: TscGlowEffect read FGlowEffect write FGlowEffect;
    property AutoSize: Boolean read FAutoSize write SetLabelAutoSize;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property VertAlignment: TscVertAlignment
      read FVertAlignment write SetVertAlignment default scvtaTop;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
     property ShowEllipsis: Boolean
      read FShowEllipsis write SetShowEllipsis default False;
    property UseFontColorToStyleColor: Boolean
      read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
    property Caption;
  end;


  TscTabStyleKind = (sctsTabSheet, sctsPanel, sctsToolBar, sctsFormBackground);

  TscTabSheet = class(TTabSheet)
  private
    {$IFDEF VER230}
    FStyleElements: TStyleElements;
    {$ENDIF}
    FStyleKind: TscTabStyleKind;
    FWallpaperIndex: Integer;
    FCustomBackgroundImageIndex: Integer;
    FDrawTabsWallpaper: Boolean;
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetStyleKind(Value: TscTabStyleKind);
  protected
    CloseButtonRect: TRect;
    CloseButtonMouseIn, CloseButtonMouseDown:Boolean;
    procedure Draw(ACanvas: TCanvas);
    procedure UpdateControls;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    {$IFNDEF VER230}
    property StyleElements;
    {$ELSE}
    property StyleElements: TStyleElements
      read  FStyleElements write FStyleElements;
    {$ENDIF}
    property Color;
    property StyleKind: TscTabStyleKind read FStyleKind write SetStyleKind;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property CustomBackgroundImageIndex: Integer read FCustomBackgroundImageIndex
      write SetCustomBackgroundImageIndex;
    property DrawTabsWallpaper: Boolean read FDrawTabsWallpaper write FDrawTabsWallpaper;
  end;

  TscDrawTabEvent = procedure(TabIndex: Integer; const Rct: TRect; Active,
    MouseIn: Boolean; Cnvs: TCanvas) of object;

  TscTabCloseEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;

  TscUpDownStyleHook = class(TMouseTrackControlStyleHook)
  strict private
    FLeftPressed, FRightPressed: Boolean;
    FMouseOnLeft, FMouseOnRight: Boolean;
    function GetOrientation: TUDOrientation;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  strict protected
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure MouseLeave; override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TscUpDown = class(TUpDown);

  TscPageControl = class(TPageControl)
  private
    {$IFDEF VER230}
    FStyleElements: TStyleElements;
    {$ENDIF}
    FScrollChecking: Boolean;
    FTabGlowEffect: TscButtonGlowEffect;
    FTabImageGlow: Boolean;
    FShowButtonsDivider: Boolean;
    FShowFocusRect: Boolean;
    FShowInActiveTab: Boolean;
    FMouseWheelSupport: Boolean;
    FTabExtendedDraw: Boolean;
    FTabMargin: Integer;
    FTabSpacing: Integer;
    FTabLayout: TButtonLayout;
    FOldTabHeight: Integer;
    FCloseSize: Integer;
    FCloseButtonSize: Integer;
    FOnClose: TscTabCloseEvent;
    FOnAfterClose: TNotifyEvent;
    FFreeOnClose: Boolean;
    FShowCloseButtons: Boolean;
    FActiveTab, FOldActiveTab: Integer;
    FActiveTabIndex, FOldActiveTabIndex: Integer;
    FOnDrawSkinTab: TscDrawTabEvent;
    FTabsWallpaperIndex: Integer;
    FImages: TCustomImageList;
    FTempImages: TCustomImageList;
    FTabsInCenter: Boolean;
    FTabsOffset: Integer;
    FWallpapers: TscCustomImageCollection;
    FCustomImages: TscCustomImageCollection;
    FHideBorder: Boolean;
    FHideTabs: Boolean;
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetHideBorder(Value: Boolean);
    procedure SetHideTabs(Value: Boolean);
    procedure SetTabsInCenter(Value: Boolean);
    procedure SetTabsOffset(Value: Integer);
    procedure SetTabsWallpaperIndex(Value: Integer);
    procedure SetImages(value: TCustomImageList);
    procedure DrawCloseButton(Cnvs: TCanvas; R: TRect; I: Integer; AColor: TColor);
    procedure SetTabExtendedDraw(Value: Boolean);
    procedure SetTabLayout(Value : TButtonLayout);
    procedure SetTabMargin(Value: Integer);
    procedure SetTabSpacing(Value: Integer);
    procedure SetShowInActiveTab(Value: Boolean);
    procedure SetShowCloseButtons(Value: Boolean);
    function GetPosition: Integer;
    function  GetInVisibleItemCount: Integer;
    procedure OnUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure DrawTabs(Cnvs: TCanvas);
    procedure DrawButtons(Cnvs: TCanvas);
    procedure DrawStyleTab(TI: Integer; const Rct: TRect;
      Active, MouseIn: Boolean; Cnvs: TCanvas; First, Last: Boolean);
    procedure DrawStyleButton(TI: Integer; const Rct: TRect;
      Active, MouseIn: Boolean; Cnvs: TCanvas);
    procedure DrawStyleFlatButton(TI: Integer; const Rct: TRect;
      Active, MouseIn: Boolean; Cnvs: TCanvas);
    function GetItemRect(index: integer): TRect;
    procedure CheckScroll;
    procedure ShowUpDown;
    procedure HideUpDown;
    procedure TestActive(X, Y: Integer);
  protected
    FUpDownPanel: TscPanel;
    FUpDown: TscUpDown;
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function GetGlowParams(ACtrlState: TscsCtrlState;
      var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean;
    function GetItemOffset: Integer;
    function CanDrawWithOffset: Boolean;
    procedure Change; override;
    procedure UpdateTab(AIndex: Integer);
    procedure Change2;
    procedure OnTabGlowEffectChange(Sender: TObject);
    procedure FindNextPage;
    procedure FindPriorPage;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WMHSCROLL(var Msg: TWMEraseBkGnd); message WM_HSCROLL;
    procedure PaintStyleWindow(Cnvs: TCanvas);
    procedure PaintWindow(DC: HDC); override;
    procedure WndProc(var Message:TMessage); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
    procedure WMCHANGE(var Message: TMessage); message TCM_SETCURSEL;
    procedure DoClose;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure UpDateTabs;
    procedure ReDrawTabs;
  published
   {$IFDEF VER230}
    property StyleElements: TStyleElements
      read  FStyleElements write FStyleElements;
   {$ENDIF}
    property TabGlowEffect: TscButtonGlowEffect read FTabGlowEffect write FTabGlowEffect;
    property TabImageGlow: Boolean read FTabImageGlow write FTabImageGlow;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property HideBorder: Boolean read FHideBorder write SetHideBorder;
    property HideTabs: Boolean read FHideTabs write SetHideTabs;
    property TabsOffset: Integer read FTabsOffset write SetTabsOffset;
    property TabsInCenter: Boolean read FTabsInCenter write SetTabsInCenter;
    property ShowButtonsDivider: Boolean read
       FShowButtonsDivider write FShowButtonsDivider;
    property TabsWallpaperIndex: Integer
      read FTabsWallpaperIndex write SetTabsWallpaperIndex;
    property ShowInActiveTab: Boolean
      read FShowInActiveTab write SetShowInActiveTab;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
    property MouseWheelSupport: Boolean
      read FMouseWheelSupport write FMouseWheelSupport;
    property TabExtededDraw: Boolean
     read FTabExtendedDraw write SetTabExtendedDraw;
    property TabMargin: Integer read FTabMargin write SetTabMargin default -1;
    property TabSpacing: Integer read FTabSpacing write SetTabSpacing default 2;
    property TabLayout: TButtonLayout read FTabLayout write SetTabLayout default blGlyphLeft;
    property FreeOnClose: Boolean read FFreeOnClose write FFreeOnClose;
    property ShowCloseButtons: Boolean read FShowCloseButtons write SetShowCloseButtons;
    property Images: TCustomImageList read FImages write SetImages;
    property Color;
    property ActivePage;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property TabHeight;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClose: TscTabCloseEvent read FOnClose write FOnClose;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnChange;
    property OnDrawTab: TscDrawTabEvent
      read FOnDrawSkinTab write FOnDrawSkinTab;
    property OnChanging;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TscMemo = class(TMemo)
  private
    {$IFDEF VER230}
    FStyleElements: TStyleElements;
    {$ENDIF}
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FTransparent: Boolean;
    ParentBGBuffer: TBitmap;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageNormalIndex: Integer;
    FCustomBackgroundImageHotIndex: Integer;
    FCustomBackgroundImageDisabledIndex: Integer;
    FSelLen, FSelPos: Integer;
    FUseFontColorToStyleColor: Boolean;
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetCustomBackgroundImageNormalIndex(Value: Integer);
    procedure SetCustomBackgroundImageHotIndex(Value: Integer);
    procedure SetCustomBackgroundImageDisabledIndex(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetRedraw(Value: Boolean);
    procedure GetParentBG;
  protected
    FStopDraw: Boolean;
    FStopGetParentBG: Boolean;
    FStopRedraw: Boolean;
    FSizeChanging: Boolean;
    // Windows 10 Fluient UI
    FFluentUIOpaque: Boolean;
    procedure SetFluentUIOpaque(Value: Boolean);
    function IsFluentUIOpaque: Boolean;
    //
    procedure CreateWnd; override;
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
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure DrawBackGround(ACanvas: TCanvas);
    procedure DoPaint;
    procedure DoPaint2(DC: HDC);
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
     procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFDEF VER230}
    property StyleElements: TStyleElements
      read  FStyleElements write FStyleElements;
    {$ENDIF}
    property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageNormalIndex: Integer
      read FCustomBackgroundImageNormalIndex write SetCustomBackgroundImageNormalIndex;
    property CustomBackgroundImageHotIndex: Integer
      read FCustomBackgroundImageHotIndex write SetCustomBackgroundImageHotIndex;
    property CustomBackgroundImageDisabledIndex: Integer
      read FCustomBackgroundImageDisabledIndex write SetCustomBackgroundImageDisabledIndex;
    property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
  end;

  TscEditBorderKind = (scebFrame, scebBottomLine, scebBottomActiveLine,
    scebColorFrame, scebColorFrame2);

  TscEditButton = class(TPersistent)
  private
    FRepeatClick: Boolean;
    FRepeatClickInterval: Integer;
    FEnabled: Boolean;
    FVisible: Boolean;
    FStyleKind: TscButtonStyleKind;
    FWidth: Integer;
    FImageIndex: Integer;
    FImageHotIndex: Integer;
    FImagePressedIndex: Integer;
    FOnChange: TNotifyEvent;
    FOnVisibleChange: TNotifyEvent;
    FShowEllipses: Boolean;
    FComboButton: Boolean;
    FDropDownMenu: TPopupMenu;
    FCustomImageNormalIndex: Integer;
    FCustomImageHotIndex: Integer;
    FCustomImagePressedIndex: Integer;
    FCustomImageDisabledIndex: Integer;
    FHint: String;
    FShowHint: Boolean;
    function GetWidth: Integer;
    procedure SetComboButton(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetShowEllipses(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetStyleKind(Value: TscButtonStyleKind);
    procedure SetWidth(Value: Integer);
    procedure SetImageIndex(Value: Integer);
    procedure SetCustomImageNormalIndex(Value: Integer);
    procedure SetCustomImageHotIndex(Value: Integer);
    procedure SetCustomImagePressedIndex(Value: Integer);
    procedure SetCustomImageDisabledIndex(Value: Integer);
  protected
    procedure Changed;
  public
    FDropDown: Boolean;
    ButtonRect: TRect;
    MouseIn: Boolean;
    Down: Boolean;
    {$IFDEF VER330}
    ParentControl: TWinControl;
    {$ENDIF}
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property ComboButton: Boolean read FComboButton write SetComboButton;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    property Hint: String read FHint write FHint;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property ShowEllipses: Boolean read FShowEllipses write SetShowEllipses;
    property StyleKind: TscButtonStyleKind read FStyleKind write SetStyleKind;
    property Width: Integer read GetWidth write SetWidth;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property ImageHotIndex: Integer read FImageHotIndex write FImageHotIndex;
    property ImagePressedIndex: Integer read FImagePressedIndex write FImagePressedIndex;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property RepeatClick: Boolean read FRepeatClick write FRepeatClick;
    property RepeatClickInterval: Integer read FRepeatClickInterval write FRepeatClickInterval;
    property CustomImageNormalIndex: Integer read
        FCustomImageNormalIndex write SetCustomImageNormalIndex;
    property CustomImageHotIndex: Integer read
        FCustomImageHotIndex write SetCustomImageHotIndex;
    property CustomImagePressedIndex: Integer read
        FCustomImagePressedIndex write SetCustomImagePressedIndex;
    property CustomImageDisabledIndex: Integer read
        FCustomImageDisabledIndex write SetCustomImageDisabledIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnVisibleChange: TNotifyEvent read FOnVisibleChange write FOnVisibleChange;
  end;

  TscCustomEdit = class(TCustomMaskEdit)
  private
   {$IFDEF VER230}
    FStyleElements: TStyleElements;
   {$ENDIF}
    FRepeatClickTimer: TTimer;
    FHintComponent: TscHint;
    FTextColor: TColor;
    FLeftButton: TscEditButton;
    FRightButton: TscEditButton;
    FMouseIn: Boolean;
    FTransparent: Boolean;
    ParentBGBuffer: TBitmap;
    FBorderKind: TscEditBorderKind;
    FFrameColor: TColor;
    FFrameActiveColor: TColor;
    FButtonImages: TCustomImageList;
    FOnLeftButtonClick: TNotifyEvent;
    FOnRightButtonClick: TNotifyEvent;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FContentMarginLeft: Integer;
    FContentMarginRight: Integer;
    FContentMarginTop: Integer;
    FContentMarginBottom: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageNormalIndex: Integer;
    FCustomBackgroundImageHotIndex: Integer;
    FCustomBackgroundImageDisabledIndex: Integer;
    FUseFontColorToStyleColor: Boolean;
    FPromptText: String;
    FPromptTextColor: TColor;
    FHidePromptTextIfFocused: Boolean;
    FOnDrawBackgroundEvent: TscPaintControlEvent;
    FCustomDraw: Boolean;
    // Windows 10 Fluient UI
    FFluentUIOpaque: Boolean;
    procedure SetFluentUIOpaque(Value: Boolean);
    //
    procedure SetPromptText(Value: String);
    procedure SetPromptTextColor(Value: TColor);
    procedure SetContentMarginLeft(Value: Integer);
    procedure SetContentMarginTop(Value: Integer);
    procedure SetContentMarginRight(Value: Integer);
    procedure SetContentMarginBottom(Value: Integer);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetButtonImages(Value: TCustomImageList);
    procedure SetBorderKind(Value: TscEditBorderKind);
    procedure SetTransparent(Value: Boolean);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameActiveColor(Value: TColor);
    procedure SetCustomBackgroundImageNormalIndex(Value: Integer);
    procedure SetCustomBackgroundImageHotIndex(Value: Integer);
    procedure SetCustomBackgroundImageDisabledIndex(Value: Integer);
  protected
    FStopDraw: Boolean;
    FInheritedKeys: Boolean;
    FStopGetParentBG: Boolean;
    FButtonsSystemSize: Boolean;
    FMenuTracking: Boolean;
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}

    function IsFluentUIOpaque: Boolean;

    procedure DoCustomPaint(ACanvas: TCanvas);
    function CanScaleButtons: Boolean; virtual;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure DrawEditBackground(ACanvas: TCanvas);
    procedure DrawBorder(DC: HDC);
    procedure RepeatClickTimerProc(Sender: TObject);
    procedure WaitClickTimerProc(Sender: TObject);
    procedure StartRepeatClick;
    procedure StopRepeatClick;
    function IsCustomDraw(ADC: HDC): Boolean; virtual;
    function GetTextColor: TColor;
    procedure OnButtonChange(Sender: TObject);
    procedure OnButtonVisibleChange(Sender: TObject);
    procedure DrawEditButton(ACanvas: TCanvas; AButton: TscEditButton);
    procedure DrawPromptText(ACanvas: TCanvas);
    procedure UpdateComboButton;
    function GetTextRect: TRect; virtual;
    procedure AdjustTextRect;
    procedure GetParentBG;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSetFocus(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMTimer(var Message: TWMTimer); message WM_Timer;
    procedure WMCHAR(var Message:TWMCHAR); message WM_CHAR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
    procedure WMNCPAINT(var Message: TWMNCPAINT); message WM_NCPAINT;
    procedure WMNCCALCSIZE(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCLBUTTONDOWN(var Message: TWMNCLBUTTONDOWN); message WM_NCLBUTTONDOWN;
    procedure WMNCLBUTTONDBCLK(var Message: TWMNCLBUTTONDOWN); message WM_NCLBUTTONDBLCLK;
    procedure WMNCLBUTTONUP(var Message: TWMNCLBUTTONUP); message WM_NCLBUTTONUP;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure WndProc(var Message: TMessage); override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMNCHITTEST(var Message: TWMNCHITTEST); message WM_NCHITTEST;
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoPaint;
    procedure DoPaint2(DC: HDC);
    procedure UpdateBorder;
    procedure EditMouseEnter;
    procedure EditMouseLeave;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Change; override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property LeftButton: TscEditButton read FLeftButton write FLeftButton;
    property RightButton: TscEditButton read FRightButton write FRightButton;
    property BorderKind: TscEditBorderKind read FBorderKind write SetBorderKind;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameActiveColor: TColor read FFrameActiveColor write SetFrameActiveColor;
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
  published
    property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
    {$IFDEF VER230}
    property StyleElements: TStyleElements
      read  FStyleElements write FStyleElements;
    {$ENDIF}
    property HintComponent: TscHint
      read FHintComponent write FHintComponent;
    property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
    property ContentMarginLeft: Integer read FContentMarginLeft write SetContentMarginLeft;
    property ContentMarginRight: Integer read FContentMarginRight write SetContentMarginRight;
    property ContentMarginTop: Integer read FContentMarginTop write SetContentMarginTop;
    property ContentMarginBottom: Integer read FContentMarginBottom write SetContentMarginBottom;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageNormalIndex: Integer
      read FCustomBackgroundImageNormalIndex write SetCustomBackgroundImageNormalIndex;
    property CustomBackgroundImageHotIndex: Integer
      read FCustomBackgroundImageHotIndex write SetCustomBackgroundImageHotIndex;
    property CustomBackgroundImageDisabledIndex: Integer
      read FCustomBackgroundImageDisabledIndex write SetCustomBackgroundImageDisabledIndex;
    property PromptText: String read FPromptText write SetPromptText;
    property PromptTextColor: TColor read FPromptTextColor write SetPromptTextColor;
    property HidePromptTextIfFocused: Boolean
      read FHidePromptTextIfFocused write FHidePromptTextIfFocused;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property OnDrawBackground: TscPaintControlEvent
      read FOnDrawBackgroundEvent write FOnDrawBackgroundEvent;
  end;

  TscEdit = class(TscCustomEdit)
  published
    property LeftButton;
    property RightButton;
    property Transparent;
    property BorderKind;
    property CustomDraw;
    property FrameColor;
    property FrameActiveColor;
    property ButtonImages;
    property EditMask;
    property Text;
    property Align;
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
    property OnLeftButtonClick;
    property OnRightButtonClick;
  end;

  TscValueType = (scvtInteger, scvtFloat);
  TscNumEditDisplayType = (scedtNumeric, scedtCurrency);

  TscNumericEdit = class(TscCustomEdit)
  private
    FDisplayType: TscNumEditDisplayType;
    StopCheck, FromEdit: Boolean;
    FMinValue, FMaxValue, FValue: Double;
    FDecimal: Byte;
    FValueType: TscValueType;
    FIncrement: Double;
    FSupportUpDownKeys: Boolean;
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
    function IsCustomDraw(ADC: HDC): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsNumText(AText: String): Boolean;
    property ValueAsInt: Integer read GetValueAsInt;
  published
    property CurrencyString: String
      read FCurrencyString write SetCurrencyString;
    property Increment: Double read FIncrement write FIncrement;
    property DisplayFormat: String
      read  FDisplayFormat write SetDisplayFormat;
    property SupportUpDownKeys: Boolean
      read FSupportUpDownKeys write FSupportUpDownKeys;
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
    property BorderKind;
    property ButtonImages;
    property EditMask;
    property Align;
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
    property OnLeftButtonClick;
    property OnRightButtonClick;
  end;

  TscUpDownKind = (scupkDefault, scupkLeftRight);

  TscSpinEdit = class(TscCustomEdit)
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
    function CanScaleButtons: Boolean; override;
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
    property Transparent;
    property BorderKind;
    property ButtonImages;
    property EditMask;
    property Align;
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


  TscCustomComboBoxEx = class(TCustomComboBoxEx)
  private
    FSelectionStyle: TscSelectionStyle;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FDrawSelectionWithStyles: Boolean;
    FShowFocusRect: Boolean;
    FComboBoxHandle: HWnd;
    FComboBoxInstance: Pointer;
    FDefComboBoxProc: Pointer;
    FMouseIn: Boolean;
    FStyleKind: TscComboStyleKind;
    procedure SetStyleKind(Value: TscComboStyleKind);
    procedure InitComboBoxWnd;
    procedure SetSelectionColor(Value: TColor);
    procedure SetSelectionTextColor(Value: TColor);
  protected
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
    procedure CMSEPaint(var Message: TMessage); message CM_SEPAINT;
    procedure DrawPushButtonCombo(ACanvas: TCanvas); virtual;
    function IsCustomDraw: Boolean;
    function IsCustomDrawItem: Boolean;
    procedure DrawComboBox(DC: HDC);
    procedure DrawListBoxItem(ADC: HDC; ARect: TRect; AIndex: Integer; ASelected: Boolean);
    procedure CreateWnd; override;
    procedure ComboBoxWndProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property StyleKind: TscComboStyleKind read FStyleKind write SetStyleKind;
    property SelectionStyle: TscSelectionStyle read FSelectionStyle write FSelectionStyle;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
    property DrawSelectionWithStyles: Boolean
      read FDrawSelectionWithStyles write FDrawSelectionWithStyles;
    property ShowFocusRect: Boolean
      read FShowFocusRect write FShowFocusRect;
  end;

  TscComboBoxEx = class(TscCustomComboBoxEx)
  published
    property Align;
    property AutoCompleteOptions default [acoAutoAppend];
    property ItemsEx;
    property Style;
    property StyleEx;
    property Action;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Touch;
    property Visible;
    {$IFNDEF VER230}
    property StyleElements;
    {$ENDIF}
    property SelectionStyle;
    property OnBeginEdit;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndEdit;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Images;
    property DropDownCount;
  end;

  TscCustomRadioGroup = class(TscGroupBox)
  private
    FButtonsImages: TCustomImageList;
    FButtonsGlowEffect: TscButtonGlowEffect;
    FShowFocusRect: Boolean;
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FOnButtonClick: TNotifyEvent;
    FButtonsAnimation: Boolean;
    procedure SetButtonsAnimation(Value: Boolean);
    procedure SetButtonsImages(Value: TCustomImageList);
    procedure ArrangeButtons;
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure ButtonClick(Sender: TObject); virtual;
    procedure OnButtonsGlowEffectChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateButtons;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
    property ButtonsImages: TCustomImageList read FButtonsImages write SetButtonsImages;
    property ButtonsGlowEffect: TscButtonGlowEffect read FButtonsGlowEffect write FButtonsGlowEffect;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
    property ButtonsAnimation: Boolean read FButtonsAnimation write SetButtonsAnimation;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure FlipChildren(AllLevels: Boolean); override;
  end;

  TscRadioGroup = class(TscCustomRadioGroup)
  published
    property Columns;
    property ItemIndex;
    property Items;
    property ButtonsImages;
    property ButtonsGlowEffect;
    property ButtonsAnimation;
    property ShowFocusRect;
    property OnButtonClick;
  end;

  TscCustomCheckGroup = class(TscGroupBox)
  private
    FButtonsImages: TCustomImageList;
    FButtonsGlowEffect: TscButtonGlowEffect;
    FButtonsAnimation: Boolean;
    FShowFocusRect: Boolean;
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FOnButtonClick: TNotifyEvent;
    procedure SetButtonsAnimation(Value: Boolean);
    procedure SetButtonsImages(Value: TCustomImageList);
    procedure SetShowFocusRect(Value: Boolean);
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    function GetCheckedStatus(Index: Integer): Boolean;
    procedure SetCheckedStatus(Index: Integer; Value: Boolean);
    procedure OnButtonsGlowEffectChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateButtons;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    property Columns: Integer read FColumns write SetColumns default 1;
    property Items: TStrings read FItems write SetItems;
    property ButtonsImages: TCustomImageList read FButtonsImages write SetButtonsImages;
    property ButtonsGlowEffect: TscButtonGlowEffect read FButtonsGlowEffect write FButtonsGlowEffect;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;
    property ButtonsAnimation: Boolean read FButtonsAnimation write SetButtonsAnimation;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property ItemIndex: Integer read FItemIndex;
    property ItemChecked[Index: Integer]: Boolean read GetCheckedStatus write SetCheckedStatus;
  end;

  TscCheckGroup = class(TscCustomCheckGroup)
  published
    property Columns;
    property Items;
    property ButtonsImages;
    property ButtonsGlowEffect;
    property ButtonsAnimation;
    property ShowFocusRect;
    property OnButtonClick;
  end;

  TscPasswordKind = (pkPasswordChar, pkRoundRect, pkRect, pkTriangle);

  TscCustomEditBorderKind = (sccebFrame, sccebBottomLine,
    sccebNone, sccebBottomActiveLine, sccebColorFrame, sccebColorFrame2);

  TscPasswordEdit = class(TscCustomControl)
  private
    FPasswordCharImages: TCustomImageList;
    FPasswordCharImageIndex: Integer;
    FPasswordCharSelectedImageIndex: Integer;
    FFrameColor: TColor;
    FFrameActiveColor: TColor;
    FTransparent: Boolean;
    FBorderKind: TscCustomEditBorderKind;
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
    FPasswordKind: TscPasswordKind;
    FTextAlignment: TAlignment;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageNormalIndex: Integer;
    FCustomBackgroundImageHotIndex: Integer;
    FCustomBackgroundImageDisabledIndex: Integer;
    FContentMarginLeft: Integer;
    FContentMarginRight: Integer;
    FContentMarginTop: Integer;
    FContentMarginBottom: Integer;
    FUseFontColorToStyleColor: Boolean;
    FPromptText: String;
    FPromptTextColor: TColor;
    FShowingText: Boolean;
    FHidePromptTextIfFocused: Boolean;
    procedure SetPromptText(Value: String);
    procedure SetPromptTextColor(Value: TColor);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameActiveColor(Value: TColor);
    procedure SetContentMarginLeft(Value: Integer);
    procedure SetContentMarginTop(Value: Integer);
    procedure SetContentMarginRight(Value: Integer);
    procedure SetContentMarginBottom(Value: Integer);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetCustomBackgroundImageNormalIndex(Value: Integer);
    procedure SetCustomBackgroundImageHotIndex(Value: Integer);
    procedure SetCustomBackgroundImageDisabledIndex(Value: Integer);
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
    procedure SetCursor(const Value: TCursor);
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetPasswordKind(const Value: TscPasswordKind);
    procedure SetBorderKind(Value: TscCustomEditBorderKind);
    procedure SetTransparent(Value: Boolean);
    procedure SetPasswordCharImages(Value: TCustomImageList);
    procedure SetPasswordCharImageIndex(Value: Integer);
    procedure SetPasswordCharSelectedImageIndex(Value: Integer);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetEditRect: TRect; virtual;
    function GetPasswordFigureWidth: Integer;
    function GetCharX(A: Integer): Integer;
    function GetCPos(x: Integer): Integer;
    function GetSelRect: TRect; virtual;
    function GetAlignmentFlags: Integer;
    procedure PaintText(Cnv: TCanvas);
    procedure PaintSelectedText(Cnv: TCanvas);
    procedure DrawPasswordChar(SymbolRect: TRect; Selected: Boolean; Cnv: TCanvas);
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
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property Constraints;
    property Color default clWindow;
    property Cursor write SetCursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameActiveColor: TColor read FFrameActiveColor write SetFrameActiveColor;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property BorderKind: TscCustomEditBorderKind read FBorderKind write SetBorderKind;
    property ContentMarginLeft: Integer read FContentMarginLeft write SetContentMarginLeft;
    property ContentMarginRight: Integer read FContentMarginRight write SetContentMarginRight;
    property ContentMarginTop: Integer read FContentMarginTop write SetContentMarginTop;
    property ContentMarginBottom: Integer read FContentMarginBottom write SetContentMarginBottom;
    property PromptText: String read FPromptText write SetPromptText;
    property PromptTextColor: TColor read FPromptTextColor write SetPromptTextColor;
    property HidePromptTextIfFocused: Boolean
      read FHidePromptTextIfFocused write FHidePromptTextIfFocused;
    property PasswordCharImages: TCustomImageList read FPasswordCharImages write SetPasswordCharImages;
    property PasswordCharImageIndex: Integer read FPasswordCharImageIndex write SetPasswordCharImageIndex;
    property PasswordCharSelectedImageIndex: Integer read FPasswordCharSelectedImageIndex
      write SetPasswordCharSelectedImageIndex;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageNormalIndex: Integer
      read FCustomBackgroundImageNormalIndex write SetCustomBackgroundImageNormalIndex;
    property CustomBackgroundImageHotIndex: Integer
      read FCustomBackgroundImageHotIndex write SetCustomBackgroundImageHotIndex;
    property CustomBackgroundImageDisabledIndex: Integer
      read FCustomBackgroundImageDisabledIndex write SetCustomBackgroundImageDisabledIndex;
     property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property ParentFont;
    property ParentShowHint;
    property PasswordKind: TscPasswordKind read FPasswordKind write SetPasswordKind default pkPasswordChar;
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

  TscColorListBox = class(TColorListBox)
  private
    FSelectionStyle: TscSelectionStyle;
    FShowFocusRect: Boolean;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    procedure SetSelectionColor(Value: TColor);
    procedure SetSelectionTextColor(Value: TColor);
    procedure SetSelectionStyle(Value: TscSelectionStyle);
  protected
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function CanDrawFocusRect: Boolean;
    procedure DrawItemContent(ACanvas: TCanvas; ARect: TRect;
      AIndex: Integer; AState: TOwnerDrawState); virtual;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
  end;

  TscColorBox = class(TColorBox)
  private
    FSelectionStyle: TscSelectionStyle;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FStyleKind: TscComboStyleKind;
    FShowFocusRect: Boolean;
    FTempItemIndex: Integer;
    procedure SetSelectionColor(Value: TColor);
    procedure SetSelectionTextColor(Value: TColor);
    procedure SetSelectionStyle(Value: TscSelectionStyle);
    procedure SetStyleKind(Value: TscComboStyleKind);
  protected
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function IsCustomDraw: Boolean;
    function IsFocused: Boolean;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Message: TWMCOMMAND); message CN_COMMAND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure DrawItemContent(ACanvas: TCanvas; ARect: TRect;
      AIndex: Integer; AState: TOwnerDrawState; AComboItem: Boolean); virtual;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write SetSelectionTextColor;
    property StyleKind: TscComboStyleKind read FStyleKind write SetStyleKind;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
  end;

  TscTrackStyleKind = (sctsStyled, sctsColor, sctsCustomImage);

  TscTrackBarTrackOptions = class(TPersistent)
  private
    FSize: Integer;
    FOffset: Integer;
    FStyleKind: TscTrackStyleKind;
    FColor: TColor;
    FProgressColor: TColor;
    FStyleColors: Boolean;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FCustomImageIndex: Integer;
    FCustomImageProgressIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure SetSize(Value: Integer);
    procedure SetOffset(Value: Integer);
    procedure SetStyleKind(Value: TscTrackStyleKind);
    procedure SetColor(Value: TColor);
    procedure SetProgressColor(Value: TColor);
    procedure SetStyleColors(Value: Boolean);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameWidth(Value: Integer);
    procedure SetCustomImageIndex(Value: Integer);
    procedure SetCustomImageProgressIndex(Value: Integer);
    procedure Changed;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    function GetColor: TColor;
    function GetProgressColor: TColor;
    function GetFrameColor: TColor;
  published
    property Size: Integer read FSize write SetSize;
    property Offset: Integer read FOffset write SetOffset;
    property StyleKind: TscTrackStyleKind read FStyleKind write SetStyleKind;
    property Color: TColor read FColor write SetColor;
    property ProgressColor: TColor read FProgressColor write SetProgressColor;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
    property CustomImageIndex: Integer
      read FCustomImageIndex write SetCustomImageIndex;
    property CustomImageProgressIndex: Integer
      read FCustomImageProgressIndex write SetCustomImageProgressIndex;
    property StyleColors: Boolean read FStyleColors write SetStyleColors;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscTrackBarThumbOptions = class(TPersistent)
  private
    FStyleKind: TscTrackStyleKind;
    FStyleColors: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FNormalColor: TColor;
    FHotColor: TColor;
    FPressedColor: TColor;
    FDisabledColor: TColor;
    FCustomImageNormalIndex: Integer;
    FCustomImageHotIndex: Integer;
    FCustomImagePressedIndex: Integer;
    FCustomImageDisabledIndex: Integer;
    FOnChange: TNotifyEvent;
    FCursor: TCursor;
    FUseCursor: Boolean;
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetStyleKind(Value: TscTrackStyleKind);
    procedure SetNormalColor(Value: TColor);
    procedure SetHotColor(Value: TColor);
    procedure SetPressedColor(Value: TColor);
    procedure SetDisabledColor(Value: TColor);
    procedure SetStyleColors(Value: Boolean);
    procedure SetCustomImageNormalIndex(Value: Integer);
    procedure SetCustomImageHotIndex(Value: Integer);
    procedure SetCustomImagePressedIndex(Value: Integer);
    procedure SetCustomImageDisabledIndex(Value: Integer);
    procedure Changed;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    function GetNormalColor: TColor;
    function GetHotColor: TColor;
    function GetPressedColor: TColor;
    function GetDisabledColor: TColor;
  published
    property StyleKind: TscTrackStyleKind read FStyleKind write SetStyleKind;
    property StyleColors: Boolean read FStyleColors write SetStyleColors;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property UseCursor: Boolean read FUseCursor write FUseCursor;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property NormalColor: TColor read FNormalColor write SetNormalColor;
    property HotColor: TColor read FHotColor write SetHotColor;
    property PressedColor: TColor read FPressedColor write SetPressedColor;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor;
    property CustomImageNormalIndex: Integer
      read FCustomImageNormalIndex write SetCustomImageNormalIndex;
    property CustomImageHotIndex: Integer
      read FCustomImageHotIndex write SetCustomImageHotIndex;
    property CustomImagePressedIndex: Integer
      read FCustomImagePressedIndex write SetCustomImagePressedIndex;
    property CustomImageDisabledIndex: Integer
      read FCustomImageDisabledIndex write SetCustomImageDisabledIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TscCustomTrackBar = class(TscCustomControl)
  protected
    FTrackOptions: TscTrackBarTrackOptions;
    FThumbOptions: TscTrackBarThumbOptions;
    FClicksDisabled: Boolean;
    FShowFocusRect: Boolean;
    FCanFocused: Boolean;
    Offset1, Offset2, BOffset: Integer;
    ThumbR, TrackR: TRect;
    FMinValue, FMaxValue, FValue: Integer;
    FVertical: Boolean;
    FMouseSupport, FDown, FMouseOver: Boolean;
    OMPos: Integer;
    OldBOffset: Integer;
    FOnChange: TNotifyEvent;
    FOnLastChange: TNotifyEvent;
    FOnStartDragButton: TNotifyEvent;
    FJumpWhenClick: Boolean;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageIndex: Integer;
    FReadOnly: Boolean;
    FSaveCursor: TCursor;
    FMouseWheelSupport: Boolean;
    FMouseWheelOpposite: Boolean;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure OnTrackOptionsChange(Sender: TObject); virtual;
    function IsFocused: Boolean;
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetCanFocused(Value: Boolean);
    procedure SetVertical(AValue: Boolean);
    procedure SetMinValue(AValue: Integer);
    procedure SetMaxValue(AValue: Integer);
    procedure SetValue(AValue: Integer);
    function CalcThumbRect(R: TRect): TRect; virtual;
    function CalcValue(AOffset: Integer): Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMSETFOCUS(var Message: TWMSETFOCUS); message WM_SETFOCUS;
    procedure WMKILLFOCUS(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; virtual;

    property TrackOptions: TscTrackBarTrackOptions read FTrackOptions write FTrackOptions;
    property ThumbOptions: TscTrackBarThumbOptions read FThumbOptions write FThumbOptions;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageIndex: Integer
      read FCustomBackgroundImageIndex write SetCustomBackgroundImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TransparentBackground;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;
    property JumpWhenClick: Boolean read FJumpWhenClick write FJumpWhenClick;
    property MouseWheelSupport: Boolean read FMouseWheelSupport write FMouseWheelSupport;
    property MouseWheelOpposite: Boolean read FMouseWheelOpposite write FMouseWheelOpposite;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property CanFocused: Boolean read FCanFocused write SetCanFocused;
    property MouseSupport: Boolean read FMouseSupport write FMouseSupport;
    property MinValue: Integer read FMinValue write SetMinValue;
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property Value: Integer read FValue write SetValue;
    property Vertical: Boolean read FVertical write SetVertical;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Align;
    property Color;
    property Enabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnLastChange: TNotifyEvent read FOnLastChange write FOnLastChange;
    property OnStartDragButton: TNotifyEvent
      read FOnStartDragButton write FOnStartDragButton;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscTrackBar = class(TscCustomTRackBar)
  published
    property TrackOptions;
    property ThumbOptions;
    property CustomImages;
    property CustomBackgroundImageIndex;
  end;

  TscTrackEdit = class;

  TscPopupTrackBar = class(TscTrackBar)
  protected
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    TrackEdit: TscTrackEdit;
    constructor Create(AOwner: TComponent); override;
  end;

  TscTrackBarPopupPos = (sctbpRight, sctbpLeft);

  TscTrackEdit = class(TscCustomEdit)
  private
    FIncrement: Integer;
    FSupportUpDownKeys: Boolean;
    FPopupKind: TscTrackBarPopupPos;
    FTrackBarWidth: Integer;
    FTrackBarHeight: Integer;
    StopCheck, FromEdit: Boolean;
    FMinValue, FMaxValue, FValue: Integer;
    FPopupTrackBar: TscPopupTrackBar;
    FDblClickShowTrackBar: Boolean;
    function GetJumpWhenClick: Boolean;
    procedure SetJumpWhenClick(Value: Boolean);
    procedure SetValue(AValue: Integer);
    procedure SetMinValue(AValue: Integer);
    procedure SetMaxValue(AValue: Integer);
    procedure TrackBarChange(Sender: TObject);
    procedure DropDown;
    procedure CloseUp;
  protected
    function CanScaleButtons: Boolean; override;
    function CheckValue(NewValue: Integer): Integer;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
     procedure WMCHAR(var Message:TWMCHAR); message WM_CHAR;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function IsValidChar(Key: Char): Boolean;
    procedure Change; override;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    property Text;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsNumText(AText: String): Boolean;
    procedure ButtonClick(Sender: TObject);
  published
    property Increment: Integer read FIncrement write FIncrement;
    property DblClickShowTrackBar: Boolean
      read FDblClickShowTrackBar write FDblClickShowTrackBar;
    property SupportUpDownKeys: Boolean
      read FSupportUpDownKeys write FSupportUpDownKeys;
    property Alignment;
    property PopupKind: TscTrackBarPopupPos read FPopupKind write FPopupKind;
    property JumpWhenClick: Boolean
     read GetJumpWhenClick write SetJumpWhenClick;
    property TrackBarWidth: Integer
      read FTrackBarWidth write FTrackBarWidth;
    property TrackBarHeight: Integer
      read FTrackBarHeight write FTrackBarHeight;
    property MinValue: Integer read FMinValue write SetMinValue;
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property Value: Integer read FValue write SetValue;
    property LeftButton;
    property RightButton;
    property Transparent;
    property BorderKind;
    property ButtonImages;
    property EditMask;
    property Align;
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

  TscProgressBar = class(TscCustomControl)
  protected
    FrameR: TRect;
    FAnimationTimer: TTimer;
    FAnimationTimerInterval: Integer;
    FAnimationRect: TRect;
    FOptions: TscTrackBarTrackOptions;
    FMinValue, FMaxValue, FValue: Integer;
    FVertical: Boolean;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageIndex: Integer;
    function CalcProgressRect(R: TRect): TRect;
    procedure ProcessAnimation;
    procedure OnProgressOptionsChange(Sender: TObject);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetVertical(AValue: Boolean);
    procedure SetMinValue(AValue: Integer);
    procedure SetMaxValue(AValue: Integer);
    procedure SetValue(AValue: Integer);
    procedure DrawBackground(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure SetTransparentBackground(Value: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnAnimationTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartAnimation;
    procedure StopAnimation;
  published
    property Options: TscTrackBarTrackOptions read FOptions write FOptions;
    property AnimationTimerInterval: Integer
      read FAnimationTimerInterval write FAnimationTimerInterval;
    property TransparentBackground;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageIndex: Integer
      read FCustomBackgroundImageIndex write SetCustomBackgroundImageIndex;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property MinValue: Integer read FMinValue write SetMinValue;
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property Value: Integer read FValue write SetValue;
    property Vertical: Boolean read FVertical write SetVertical;
    property Align;
    property Color;
    property Enabled;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TscListBoxStyleHook = class(TscScrollingStyleHook)
  strict private
    procedure UpdateColors;
  strict protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WndProc(var Message: TMessage); override;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TscComboBoxStyleHook = class(TMouseTrackControlStyleHook)
  protected
    FDownPos, FMovePos: TPoint;
    FDownSliderPos: Integer;
    FOldIdx, FInvsibleCount, FSliderSize: Integer;
    FVSliderState, FVUpState, FVDownState: TThemedScrollBar;
    FIgnoreStyleChanged: Boolean;
    FMouseOnButton: Boolean;
    FListHandle, FEditHandle: HWnd;
    FListBoxInstance: Pointer;
    FDefListBoxProc: Pointer;
    FListBoxTimerCode: Integer;
    FListBoxUpBtnDown, FListBoxDownBtnDown,
    FListBoxTrackUpDown, FListBoxTrackDownDown: Boolean;
    FListBoxDown: Boolean;
    FTempItemIndex: Integer;
    procedure DrawListBoxVertScroll(DC: HDC);
    procedure DrawListBoxBorder;
    function DroppedDown: Boolean;
    function GetButtonRect: TRect; virtual;
    function Style: TComboBoxStyle;
    function ListBoxBoundsRect: TRect;
    function ListBoxClientRect: TRect;
    procedure ListBoxSetTimer(ATimerCode: Integer);
    procedure ListBoxStopTimer;
    function ListBoxVertScrollRect: TRect;
    function ListBoxVertDownButtonRect: TRect;
    function ListBoxVertUpButtonRect: TRect;
    function ListBoxVertScrollArea: TRect;
    function ListBoxVertSliderRect: TRect;
    function ListBoxVertTrackRect: TRect;
    function ListBoxVertTrackRectUp: TRect;
    function ListBoxVertTrackRectDown: TRect;
    procedure PaintListBoxBorder(Canvas: TCanvas; const R: TRect);
    procedure UpdateColors;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMParentNotify(var Message: TMessage); message WM_PARENTNOTIFY;
    {$IFNDEF VER230}
    function AcceptMessage(var Message: TMessage): Boolean; override;
    function IsChildHandle(AHandle: HWnd): Boolean; override;
    {$ENDIF}
    procedure DrawItem(Canvas: TCanvas; Index: Integer;
      const R: TRect; Selected: Boolean); virtual;
    procedure HookListBox(AListHandle: HWND);
    property ListBoxInstance: Pointer read FListBoxInstance;
    procedure ListBoxWndProc(var Msg: TMessage); virtual;
    property ListHandle: HWND read FListHandle;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure PaintBorder(Canvas: TCanvas); virtual;
    procedure WndProc(var Message: TMessage); override;
    property ButtonRect: TRect read GetButtonRect;
    property MouseOnButton: Boolean read FMouseOnButton write FMouseOnButton;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

  TscComboBoxExStyleHook = class(TscComboBoxStyleHook)
  strict protected
    FDroppedDown: Boolean;
    FComboBoxHandle: HWnd;
    FComboBoxInstance: Pointer;
    FDefComboBoxProc: Pointer;
    FTempItemIndex: Integer;
    FPaintCopy: Boolean;
    procedure InitComboBoxWnd;
    procedure PaintComboBoxWnd;
    procedure DrawPushButtonCombo(ACanvas: TCanvas); virtual;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPaint;
    procedure WMParentNotify(var Message: TMessage); message WM_ParentNotify;
    procedure ComboBoxWndProc(var Msg: TMessage); virtual;
    procedure DoHotTrackTimer(Sender: TObject); override;
    procedure DrawComboBox(DC: HDC); virtual;
    procedure DrawListBoxItem(ADC: HDC; ARect: TRect; AIndex: Integer; ASelected: Boolean);
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
    {$IFNDEF VER230}
    function IsChildHandle(AHandle: HWnd): Boolean; override;
    {$ENDIF}
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
    procedure CMSEPaint(var Message: TMessage); message CM_SEPAINT;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

  TscScrollGrip = class(TscPanel);

  TscScrollBar = class(TScrollBar)
  protected
    procedure WMGestureNotify(var Message: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscScrollBarStyleHook = class(TMouseTrackControlStyleHook)
  public type
    TscScrollWindow = class(TWinControl)
    strict private
      FSizeBox: Boolean;
      FStyleHook: TscScrollBarStyleHook;
      FVertical: Boolean;
      procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
      procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    strict protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure WndProc(var Message: TMessage); override;
    public
      constructor Create(AOwner: TComponent); override;
      procedure PaintToDC(DC: HDC);
      property SizeBox: Boolean read FSizeBox write FSizeBox;
      property StyleHook: TscScrollBarStyleHook read FStyleHook write FStyleHook;
      property Vertical: Boolean read FVertical write FVertical;
    end;
  strict private
    FScrollWnd: TscScrollWindow;
    FVSliderState, FVUpState, FVDownState: TThemedScrollBar;
    FHSliderState, FHUpState, FHDownState: TThemedScrollBar;
    function ControlBounds: TRect;
    function Horizontal: Boolean;
    function HorzDownButtonRect: TRect;
    function HorzScrollRect: TRect;
    function HorzSliderRect: TRect;
    function HorzTrackRect: TRect;
    function HorzUpButtonRect: TRect;
    procedure InitScrollBar;
    procedure PaintScrollBar;
    function VertDownButtonRect: TRect;
    function VertScrollRect: TRect;
    function VertSliderRect: TRect;
    function VertTrackRect: TRect;
    function VertUpButtonRect: TRect;
    procedure UpdateScrollBar;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TMessage); message WM_KEYUP;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure WMMove(var Message: TMessage); message WM_MOVE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGED;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure CMSENCPaint(var Message: TMessage); message CM_SENCPAINT;
    procedure CMSEPaint(var Message: TMessage); message CM_SEPAINT;
  strict protected
    {$IFNDEF VER230}
    function AcceptMessage(var Message: TMessage): Boolean; override;
    {$ENDIF}
    function HasBorder: Boolean; override;
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
protected
    procedure VertDrawScroll(DC: HDC);
    procedure HorzDrawScroll(DC: HDC);
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy;  override;
  end;

  TscStatusBarStyleHook = class(TStyleHook)
  strict protected
    FFromWMPaint: Boolean;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TscStatusBar = class(TStatusBar)
  protected
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscToolBarStyleHook = class(TMouseTrackControlStyleHook)
  strict private
    FArrowDownButtonIndex: Integer;
    FHotButtonIndex: Integer;
    FImages: TCustomImageList;
    FRuntimeThemesEnabled: Boolean;
    FFromWMPaint: Boolean;
    procedure ApplyImageList;
    function GetButtonCount: Integer;
    function GetItemInfo(Index: Integer; Text: PChar; TextLen: Integer): TTBButtonInfo;
    function GetItemRect(Index: Integer): TRect;
  strict protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

  TscToolBar = class(TToolBar);

  TscCoolBar = class(TCoolBar)
  protected
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TscRichEdit = class(TCustomRichEdit)
  private
    FStyleColors: Boolean;
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StyleColors: Boolean
      read FStyleColors write FStyleColors default True;
    {$IFNDEF VER230}
    property StyleElements;
    {$ENDIF}
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property Constraints;
    property Lines;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Touch;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
    {$IFDEF VER270_UP}
    property Zoom;
    {$ENDIF}
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscRichEditStyleHook = class(TscScrollingStyleHook)
  strict private
    procedure EMSetBkgndColor(var Message: TMessage); message EM_SETBKGNDCOLOR;
    procedure EMSetCharFormat(var Message: TMessage); message EM_SETCHARFORMAT;
  strict protected
    function CanApplyStyleColors: Boolean;
    procedure WndProc(var Message: TMessage); override;
  end;

  TscCoolBarStyleHook = class(TStyleHook)
  strict private
    function GetBandText(Index: Integer): string;
    function GetBandRect(Index: Integer): TRect;
    function GetBandBorder(Index: Integer): TRect;
    function GetBandCount: Integer;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  strict protected
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TscbbButtonStyle = (scbbPushButton, scbbHeaderSection);

  TscScrollBoxStyleHook = class(TscScrollingStyleHook)
  strict protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TscsbBackgroundStyle = (scsbsFormBackground, scsbsPanel, scsbsTabSheet);
  TscCustomScrollBox = class(TScrollBox)
  private
    FBackgroundStyle: TscsbBackgroundStyle;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageIndex: Integer;
    FFullUpdate: Boolean;
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    FStorePaintBuffer: Boolean;
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetBackgroundStyle(Value: TscsbBackgroundStyle);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
  protected
    FIgnoreAutoScroll: Boolean;
    FFluentUIOpaque: Boolean;
    procedure SetFluentUIOpaque(Value: Boolean);
    function IsFluentUIOpaque: Boolean;
    //
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AutoScrollInView(AControl: TControl); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    property BackgroundStyle: TscsbBackgroundStyle
      read FBackgroundStyle write SetBackgroundStyle;
  public
    FPaintBuffer: TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    procedure ScrollInView(AControl: TControl);
    procedure FullRedraw;
  published
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageIndex: Integer
      read FCustomBackgroundImageIndex write SetCustomBackgroundImageIndex;
    property FullUpdate: Boolean
      read FFullUpdate write FFullUpdate;
    property FluentUIOpaque: Boolean
        read FFluentUIOpaque write SetFluentUIOpaque;
     property StorePaintBuffer: Boolean
        read FStorePaintBuffer write FStorePaintBuffer;
  end;

  TscScrollBox = class(TscCustomScrollBox)
  published
    property Wallpapers;
    property WallpaperIndex;
    property CustomImages;
    property CustomBackgroundImageIndex;
    property BackgroundStyle;
    property FullUpdate;
  end;


  function SC_MouseHookProc(Code: Integer; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;
  procedure SC_HookMouseMessages(AControl: TWinControl);
  procedure SC_UnHookMouseMessages;

  procedure SC_SetDrawTextModeInControl(AControl: TControl; ADrawTextMode: TscDrawTextMode);

implementation

  uses Winapi.DwmApi, Vcl.ActnList, System.UITypes, Vcl.Clipbrd, System.Math;

const
  AnimationInterval = 150;
  HTEDITBUTTONL = HTSIZE + 100;
  HTEDITBUTTONR = HTSIZE + 101;
  HTUPDOWN = HTSIZE + 102;
  TAB_CLOSE_SIZE = 16;
  TV_UNCHECKED = 1;
  TV_CHECKED = 2;
  PASSWORD_CHAR = 'l';

var
  SC_MouseHookControl: TWinControl = nil;
  SC_MouseHook: HHOOK = 0;

type
  TWinControlClass = class(TWinControl);

procedure SC_SetDrawTextModeInControl(AControl: TControl; ADrawTextMode: TscDrawTextMode);
var
  I: Integer;
begin
  if AControl is TscCustomControl then
     TscCustomControl(AControl).FDrawTextMode := ADrawTextMode;
  if AControl is TWinControl then
   for I := 0 to TWinControl(AControl).ControlCount - 1 do
      SC_SetDrawTextModeInControl(TWinControl(AControl).Controls[I], ADrawTextMode);
end;

function SC_MouseHookProc(Code: Integer; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;
var
  P: TPoint;
  FWnd: HWnd;
begin
  if (Code = HC_ACTION) and
     ((wParam = WM_LBUTTONDOWN) or (wParam = WM_RBUTTONDOWN)) then
  begin
    GetCursorPos(P);
    FWnd := WindowFromPoint(P);
    if (SC_MouseHookControl <> nil) then
      SendMessage(SC_MouseHookControl.Handle, WM_MOUSEHOOKCANCELMODE, FWnd, 0);
  end;
  Result := CallNextHookEx(SC_MouseHook, Code, wParam, lParam);
end;

procedure SC_HookMouseMessages(AControl: TWinControl);
begin
  if SC_MouseHook <> 0 then UnhookWindowsHookEx(SC_MouseHook);
  SC_MouseHookControl := AControl;
  SC_MouseHook := SetWindowsHookEx(WH_MOUSE_LL, @SC_MouseHookProc, HInstance, 0);
end;

function IsIntPlusText(AText: String): Boolean;
const
  NumChars = '01234567890';
var
  i: Integer;
  S: String;
begin
  Result := True;

  S := NumChars;
  if AText = '' then
  begin
    Result := False;
    Exit;
  end;

  for i := 1 to Length(AText) do
  begin
    if Pos(AText[i], S) = 0 then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure SC_UnHookMouseMessages;
begin
  SC_MouseHookControl := nil;
  if SC_MouseHook <> 0 then
  begin
    UnhookWindowsHookEx(SC_MouseHook);
    SC_MouseHook := 0;
  end;
end;

constructor TscScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csFramed];
  TabStop := False;
  Ctl3D := False;
end;

procedure TscScrollBar.WMGestureNotify(var Message: TWMGestureNotify);
var
  Configs: array of TGestureConfig;
begin
  SetLength(Configs, 1);
  Configs[0].dwID := 0;
  Configs[0].dwWant := 0;
  Configs[0].dwBlock := GC_ALLGESTURES;
  SetGestureConfig(Handle, 0, 1, @Configs[0], SizeOf(TGestureConfig));
  Message.Result := 1;
end;

procedure TscScrollBar.WndProc(var Message: TMessage);
begin
  inherited;
end;

constructor TscCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  FUseOnlyParentBackground := False;
  FDrawTextMode := scdtmGDI;
  FBlurBackground := False;
  FFluentUIOpaque := False;
  FBlurBackgroundAmount := 5;
  FStorePaintBuffer := False;
  FUpdateParentBuffer := False;
  FPaintBuffer := nil;
  FBackgroundLeft := 0;
  FBackgroundTop := 0;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  ParentBGBuffer := TBitmap.Create;
  FTransparentBackground := True;
  FStopEraseBackground:= False;
  FDrawOnBackground := True;
  FForceDraw := False;
  FGetControlBG := False;
  FDrawInClientRect := False;
  {$IFDEF VER230}
  FStyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
end;

destructor TscCustomControl.Destroy;
begin
  ParentBGBuffer.Free;
  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Free;
    FPaintBuffer := nil;
  end;
  inherited;
end;

procedure TscCustomControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if IsFluentUIOpaque then
      ExStyle := Exstyle or WS_EX_LAYERED;
  end;
end;

procedure TscCustomControl.SetDrawTextMode(Value: TscDrawTextMode);
begin
  if FDrawTextMode <> Value then
  begin
    FDrawTextMode := Value;
    RePaintControl;
  end;
end;

function TscCustomControl.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and
  IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF};
end;

procedure TscCustomControl.CreateWnd;
begin
  inherited;
  if IsFluentUIOpaque then
  begin
    FTransparentBackground := False;
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
      SetTimer(Handle, 101, 10, nil);
  end;
end;

procedure TscCustomControl.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_TIMER:
    if (TWMTimer(Message).TimerID = 101) and IsFluentUIOpaque then
    begin
      KillTimer(Handle, 101);
      RedrawWindow(Handle, nil, 0,
        RDW_INVALIDATE + RDW_ERASE + RDW_FRAME + RDW_ALLCHILDREN + RDW_UPDATENOW);
    end;
  end;
end;

procedure TscCustomControl.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TscCustomControl.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  OldScaleFactor: Double;
begin
  {$IFDEF VER330_UP}
  OldScaleFactor := FScaleFactor;
  {$ENDIF}
  inherited;
  {$IFNDEF VER330_UP}
  OldScaleFactor := FScaleFactor;
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
  if  (OldScaleFactor <> FScaleFactor) and Assigned(FOnChangeScale) then
    FOnChangeScale(Self);
end;

{$IFDEF VER310_UP}
procedure TscCustomControl.ScaleForPPI(NewPPI: Integer);
begin
  DisableAlign;
  try
    inherited ScaleForPPI(NewPPI);
  finally
    EnableAlign;
  end;
end;
{$ENDIF}

{$IFNDEF VER230}
procedure TscCustomControl.UpdateStyleElements;
begin
  RePaintControl;
end;
{$ENDIF}

procedure TscCustomControl.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[i] is TWinControl) and Controls[i].Visible
    then
      SendMessage(TWinControl(Controls[I]).Handle, WM_CHECKPARENTBG, 0, 0)
    else
    if Controls[i] is TGraphicControl and Controls[i].Visible
     then
       TGraphicControl(Controls[I]).Perform(WM_CHECKPARENTBG, 0, 0);
  end;
end;


procedure TscCustomControl.SetRedraw(AValue: Boolean; AUpdate: Boolean = True);
begin
  if AValue then
  begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    if AUpdate then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or
        RDW_ALLCHILDREN or RDW_UPDATENOW);
  end
  else
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TscCustomControl.SetTransparentBackground(Value: Boolean);
begin
  FTransparentBackground := Value;
end;

procedure TscCustomControl.SetBlurBackground(Value: Boolean);
begin
  if FBlurBackground <> Value then
  begin
    FBlurBackground := Value;
    if FTransparentBackground then
    begin
      GetParentBG;
      RePaintControl;
      UpdateControls;
    end;
  end;
end;

procedure TscCustomControl.SetBlurBackgroundAmount(Value: Integer);
begin
  if (FBlurBackgroundAmount <> Value) and (Value >= 1) and (Value <= 20) then
  begin
    FBlurBackgroundAmount := Value;
    if FTransparentBackground and FBlurBackground then
    begin
      GetParentBG;
      RePaintControl;
      UpdateControls;
    end;
  end;
end;

procedure TscCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  I: Integer;
begin
  if csAcceptsControls in ControlStyle then
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TscCustomControl then
        TscCustomControl(Controls[I]).FForceDraw := True;

  FForceDraw := True;
  inherited;
  FForceDraw := False;

  if csAcceptsControls in ControlStyle then
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TscCustomControl then
        TscCustomControl(Controls[I]).FForceDraw := False;
end;

procedure TscCustomControl.WMSIZE(var Msg: TMessage);
begin
  inherited;
  if FTransparentBackground or FStorePaintBuffer then
  begin
    if FStorePaintBuffer then
      RePaint;
    UpdateControls;
  end;
end;

procedure TscCustomControl.WMMove(var Msg: TMessage);
begin
  inherited;
  if FTransparentBackground then
  begin
    GetParentBG;
    RepaintControl;
    UpdateControls;
  end;
end;

procedure TscCustomControl.GetParentBG;
var
  R: TRect;
  ParentControl: TWinControl;
begin
  if FTransparentBackground then
  begin
     if FUseOnlyParentBackground and (Parent.Parent <> nil) then
       ParentControl := Parent.Parent
     else
       ParentControl := Parent;

     if FBlurBackground and (ParentBGBuffer.PixelFormat <> pf32bit) then
       ParentBGBuffer.PixelFormat := pf32bit;

    if Width * Height <> 0 then
    begin
      if (ParentBGBuffer.Width <> Width) or (ParentBGBuffer.Height <> Height) then
      begin
        ParentBGBuffer.Width := Width;
        ParentBGBuffer.Height := Height;
      end;

      if (ParentControl <> nil) and (ParentControl is TscCustomControl) then
        TscCustomControl(ParentControl).FGetControlBG := True;

      try
        if (ParentControl <> nil) and (ParentControl is TscCustomScrollBox) and
           TscCustomScrollBox(ParentControl).FStorePaintBuffer and
           (TscCustomScrollBox(ParentControl).FPaintBuffer <> nil) and
           (TscCustomScrollBox(ParentControl).FPaintBuffer.Width > 0) and
           (TscCustomScrollBox(ParentControl).FPaintBuffer.Height > 0) and
           (TscCustomScrollBox(ParentControl).FPaintBuffer.Width = TscCustomScrollBox(ParentControl).ClientWidth) and
           (TscCustomScrollBox(ParentControl).FPaintBuffer.Height = TscCustomScrollBox(ParentControl).ClientHeight)
        then
        begin
          R.Left := Self.Left;
          R.Top := Self.Top;
          R.Right := R.Left + Self.Width;
          R.Bottom := R.Top + Self.Height;
          ParentBGBuffer.Canvas.CopyRect(Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height),
            TscCustomScrollBox(ParentControl).FPaintBuffer.Canvas, R);

          if FBlurBackground then
            scDrawUtils.Bitmap_Blur(ParentBGBuffer, FBlurBackgroundAmount);
        end
        else
        if not FUpdateParentBuffer and
           (ParentControl is TscCustomControl) and TscCustomControl(ParentControl).FStorePaintBuffer and
           (TscCustomControl(ParentControl).FPaintBuffer <> nil) and
           (TscCustomControl(ParentControl).FPaintBuffer.Width > 0) and
           (TscCustomControl(ParentControl).FPaintBuffer.Height > 0) and
           (TscCustomControl(ParentControl).FPaintBuffer.Width = TscCustomControl(ParentControl).Width) and
           (TscCustomControl(ParentControl).FPaintBuffer.Height = TscCustomControl(ParentControl).Height)
        then
        begin
          if FUseOnlyParentBackground and (Parent.Parent <> nil) then
          begin
            R.Left := Parent.Left;
            R.Top := Parent.Top;
            R.Right := R.Left + Parent.Width;
            R.Bottom := R.Top + Parent.Height;
          end
          else
          begin
            R.Left := Self.Left;
            R.Top := Self.Top;
            R.Right := R.Left + Self.Width;
            R.Bottom := R.Top + Self.Height;
          end;

          ParentBGBuffer.Canvas.CopyRect(Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height),
            TscCustomControl(ParentControl).FPaintBuffer.Canvas, R);

          if FBlurBackground then
            scDrawUtils.Bitmap_Blur(ParentBGBuffer, FBlurBackgroundAmount);

        end
        else
        begin
          if FUseOnlyParentBackground and (Parent.Parent <> nil) then
            DrawParentBackground(Parent, ParentBGBuffer.Canvas)
          else
            DrawParentBackground(Self, ParentBGBuffer.Canvas);

          if FBlurBackground then
            scDrawUtils.Bitmap_Blur(ParentBGBuffer, FBlurBackgroundAmount);
        end;

        FUpdateParentBuffer := False;
      finally
        if (ParentControl <> nil) and (ParentControl is TscCustomControl) then
         TscCustomControl(ParentControl).FGetControlBG := False;
      end;
    end;
  end
  else
  begin
    FUpdateParentBuffer := False;
    if ParentBGBuffer.Width * ParentBGBuffer.Height <> 0 then
      ParentBGBuffer.SetSize(0, 0);
  end;
end;

procedure TscCustomControl.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  if FTransparentBackground then
  begin
    GetParentBG;
    RePaintControl;
    UpdateControls;
  end;
end;

function TscCustomControl.IsRightToLeft: Boolean;
begin
  Result := BidiMode = bdRightToLeft;
end;

procedure TscCustomControl.Paint;
var
  W, H: Integer;
begin
  {$IFDEF MACOS}
  if not FTransparentBackground then
    DrawBackground(Canvas);
  Draw(Canvas, CtrlState);
  {$ELSE}
  if FDrawInClientRect then
  begin
    W := ClientWidth;
    H := ClientHeight;
  end
  else
  begin
    W := Width;
    H := Height;
  end;
  if (W > 0) and (H > 0) then
  begin
    if not FStorePaintBuffer or (FPaintBuffer = nil) then
      FPaintBuffer := TBitmap.Create;
    try
      FPaintBuffer.SetSize(W, H);
      if FTransparentBackground and not FStopEraseBackground then
        GetParentBG;
      DrawBackground(FPaintBuffer.Canvas);
      Draw(FPaintBuffer.Canvas, CtrlState);
      Canvas.Draw(0, 0, FPaintBuffer);
    finally
      if not FStorePaintBuffer then
      begin
        FPaintBuffer.Free;
        FPaintBuffer := nil;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TscCustomControl.RePaintControl;
begin
  FStopEraseBackground := True;
  FForceDraw := True;
  RePaint;
  FForceDraw := False;
  FStopEraseBackground := False;
end;

procedure TscCustomControl.WMPaint(var Message: TWMPaint);
begin
  FromWMPaint := True;
  inherited;
  FromWMPaint := False;
end;

function TscCustomControl.CanDrawOnBackground: Boolean;
begin
  Result := not FForceDraw and not FromWMPaint and FDrawOnBackground and
    not (csPaintCopy in ControlState);
end;

procedure TscCustomControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if CanDrawOnBackground
  then
    PaintWindow(Message.DC)
  else
    Message.Result := 1;
end;

procedure TscCustomControl.DrawBackground(ACanvas: TCanvas);
var
  ParentControl: TWinControl;
begin
  if FTransparentBackground then
  begin
    if (ParentBGBuffer.Width <> Width) or (ParentBGBuffer.Height <> Height) then
      GetParentBG;

    if FUseOnlyParentBackground and (Parent.Parent <> nil) then
      ParentControl := Parent.Parent
    else
      ParentControl := Parent;

    if (ParentControl <> nil) and (((ParentControl is TscCustomControl) and
       TscCustomControl(ParentControl).StorePaintBuffer) or ((ParentControl is TscCustomScrollBox) and
      TscCustomScrollBox(ParentControl).StorePaintBuffer))
    then
      ACanvas.Draw(FBackgroundLeft, FBackgroundTop, ParentBGBuffer)
    else
      ACanvas.Draw(0, 0, ParentBGBuffer);
  end;
end;

procedure TscCustomControl.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
begin
end;

function TscCustomControl.GetCtrlState: TscsCtrlState;
begin
  if Enabled then
    Result := scsNormal
  else
    Result := scsDisabled;
end;

procedure TscCustomControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  RePaintControl;
end;

constructor TscCustomActiveControl.Create(AOwner: TComponent);
begin
  inherited;
  FMouseIn := False;
  FMouseTimer := nil;
  FOldCtrlState := scsNormal;
  FAnimation := False;
end;

destructor TscCustomActiveControl.Destroy;
begin
   if FMouseTimer <> nil then StopMouseTimer;
  inherited;
end;

procedure TscCustomActiveControl.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  inherited;
  if FTransparentBackground then
    FCanAnimate := False;
end;

procedure TscCustomActiveControl.WMMove(var Msg: TMessage);
begin
  if FTransparentBackground then
    FCanAnimate := False;
  inherited;
end;

procedure TscCustomActiveControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if not FromWMPaint then FCanAnimate := False;
  inherited;
end;

procedure TscCustomActiveControl.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 1 then
  begin
    KillTimer(Handle, 1);
    StopAnimation;
  end;
end;

procedure TscCustomActiveControl.StartAnimationRendering;
begin
  FCanAnimate := True;
  KillTimer(Handle, 1);
  SetTimer(Handle, 1, 350, nil);
end;

procedure TscCustomActiveControl.Paint;
begin
  if FCanAnimate and CanAnimate then
  begin
    if not SC_BufferedPaintRenderAnimation(Handle, Canvas.Handle) then
      StartAnimation(Canvas.Handle);
  end
  else
    inherited;
end;

function TscCustomActiveControl.CanAnimateFocusedState: Boolean;
begin
  Result := False;
end;

function TscCustomActiveControl.CanAnimate: Boolean;
begin
  Result := not (csDesigning in ComponentState) and Enabled and Visible and
            FAnimation and ((CtrlState = scsNormal) or
            ((CtrlState = scsFocused) and CanAnimateFocusedState) or
            (CtrlState = scsHot)) and AnimationAvailable;
end;

function TscCustomActiveControl.StartAnimation(ADC: HDC): Boolean;
var
  FromDC, ToDC: HDC;
  P: TscBPAnimationParams;
  R: TRect;
  AnimationBuffer: SC_HANIMATIONBUFFER;
  FRendering: Boolean;
begin
  Result := False;
  if not AnimationAvailable then Exit;
  FillChar(P, SizeOf(P), #0);
  P.cbSize := SizeOf(P);
  P.style := SC_BPAS_LINEAR;
  if FOldCtrlState <> CtrlState then
    P.dwDuration := AnimationInterval;
  R := ClientRect;
  AnimationBuffer := SC_BeginBufferedAnimation(Handle, ADC, R,
    SC_BPBF_COMPATIBLEBITMAP, nil, P, FromDC, ToDC);
  if AnimationBuffer <> 0 then
  begin
    Result := True;
    FRendering := P.dwDuration > 0;
    if FromDC <> 0 then
      DrawToAnimationDC(FromDC, FOldCtrlState, FRendering);
    if ToDC <> 0 then
      DrawToAnimationDC(ToDC, CtrlState, FRendering);
    SC_EndBufferedAnimation(AnimationBuffer, True);
  end;
  FOldCtrlState := CtrlState;
end;

procedure TscCustomActiveControl.StopAnimation;
begin
  FCanAnimate := False;
  if AnimationAvailable then SC_BufferedPaintStopAllAnimations(Handle);
end;

function TscCustomActiveControl.AnimationAvailable: Boolean;
begin
  Result := CheckWin32Version(6, 0) and (@SC_BeginBufferedAnimation <> nil);
end;

procedure TscCustomActiveControl.DrawToAnimationDC(ADC: HDC; AState: TscsCtrlState; ARendering: Boolean);
var
  FBuffer: TBitmap;
  BF: TBlendFunction;
  DstRect, SrcRect: TRect;
  FCanvas: TCanvas;
begin
  if (Width = 0) or (Height = 0) then Exit;

  if not ARendering then
  begin
    FCanvas := TCanvas.Create;
    FCanvas.Handle := ADC;
    try
      DrawBackground(FCanvas);
      Draw(FCanvas, AState);
    finally
      FCanvas.Handle := 0;
      FCanvas.Free;
    end;
    Exit;
  end;

  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FBuffer.SetSize(Width, Height);
  try
    DrawBackground(FBuffer.Canvas);
    Draw(FBuffer.Canvas, AState);
    Bitmap_ClearAlpha(FBuffer, 255);
    BF.BlendOp := AC_SRC_OVER;
    BF.BlendFlags := 0;
    BF.SourceConstantAlpha := 255;
    BF.AlphaFormat := AC_SRC_ALPHA;
    DstRect := Rect(0, 0, FBuffer.Width, FBuffer.Height);
    SrcRect := DstRect;
    Winapi.Windows.AlphaBlend(ADC,
      DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      FBuffer.Canvas.Handle,
      SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
      BF);
  finally
    FBuffer.Free;
  end;
end;

procedure TscCustomActiveControl.StartMouseTimer;
begin
  if FMouseTimer <> nil then
    StopMouseTimer;
  FMouseTimer := TTimer.Create(nil);
  FMouseTimer.Interval := 100;
  FMouseTimer.OnTimer := DoMouseTimer;
  FMouseTimer.Enabled := True;
end;

procedure TscCustomActiveControl.StopMouseTimer;
begin
  if FMouseTimer <> nil then
  begin
    FMouseTimer.Enabled := False;
    FreeAndNil(FMouseTimer);
  end;
end;

procedure TscCustomActiveControl.DoMouseTimer(Sender: TObject);
var
  P: TPoint;
  FWindowHandle: HWnd;
begin
  GetCursorPos(P);
  FWindowHandle := WindowFromPoint(P);
  if FWindowHandle <> Handle then
  begin
    StopMouseTimer;
    FMouseIn := False;
    DoMouseLeave;
  end;
end;

function TscCustomActiveControl.GetCtrlState: TscsCtrlState;
begin
  if not Enabled then
    Result := scsDisabled else
  if FMouseIn then
    Result := scsHot
  else
    Result := scsNormal;
end;

procedure TscCustomActiveControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseIn then
  begin
    StopMouseTimer;
    FMouseIn := False;
    DoMouseLeave;
    FOldCtrlState := CtrlState;
  end;
end;

procedure TscCustomActiveControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseIn then
  begin
    FMouseIn := True;
    StartMouseTimer;
    DoMouseEnter;
  end;
end;

procedure TscCustomActiveControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not FMouseIn and
     (X >= 0) and (Y >= 0) and (X <= Width) and (Y <= Height) then
  begin
    FMouseIn := True;
    StartMouseTimer;
    DoMouseEnter;
  end
  else
  if FMouseIn and
    ((X < 0) or (Y < 0) or (X > Width) or (Y > Height)) then
  begin
    StopMouseTimer;
    FMouseIn := False;
    DoMouseLeave;
  end;
end;

procedure TscCustomActiveControl.DoMouseEnter;
begin
  if FAnimation and CanAnimate then StartAnimationRendering;
  RePaintControl;
end;

procedure TscCustomActiveControl.DoMouseLeave;
begin
  if FAnimation and CanAnimate then StartAnimationRendering;
  RePaintControl;
end;

constructor TscCustomButtonControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
  FGlowEffect := TscButtonGlowEffect.Create;
  FCustomDropDown := False;
  FDropDownCall := False;
  FArrowPosition := scapRight;
  FArrowDirection := scadDefault;
  FWordWrap := True;
  FSplitWidth := 19;
  FGlowEffect.OnChange := OnGlowEffectChange;
  FDisableClick := False;
  FImageGlow := True;
  FPressed := False;
  FIsDown := False;
  FCanFocused := True;
  TabStop := True;
  FMargin := -1;
  FSpacing := 1;
  FLayout := blGlyphLeft;
  FImages := nil;
  FImageIndex := -1;
  FRepeatClickTimer := nil;
  FRepeatClick := False;
  FRepeatClickInterval := 100;
  FDropDownMenu := nil;
  FGalleryMenu := nil;
  FShowGalleryMenuFromTop := False;
  FShowGalleryMenuFromRight := False;
  FShowMenuArrow := True;
  FSplitButton := False;
  FMenuDroppedDown := False;
  FMenuTracking := False;
  FShowFocusRect := True;
  FDown := False;
  FGroupIndex := 0;
  FAllowAllUp := False;
  FMustDoUp := False;
  Width := 100;
  Height := 35;

  FGlowBuffer := nil;
  FGlowImageBuffer := nil;
  FStoredCaption := '';
  FStoredWordWrap := FWordWrap;
  FStoredWidth := 0;
  FStoredHeight := 0;
  FStoredGlowColor := clNone;
  FStoredFontSize := 0;
  FStoredFontStyle := [];
  FUpdateGlowBuffer := True;
  FStoredFontName := '';
  FStoredImageIndex := -1;
  FStoredGlowSize := 0;
  FStoredGlowAlphaValue := 0;
  FStoredRect := Rect(0, 0, 0, 0);
  FStoredSpacing := -1;
  FStoredMargin := -2;
  FStoredLayout := blGlyphLeft;

  FUseImagesFromAction := True;
  FUseImageIndexFromAction := True;
end;

destructor TscCustomButtonControl.Destroy;
begin
   if FGlowBuffer <> nil then
    FGlowBuffer.Free;
  if FGlowImageBuffer <> nil then
    FGlowImageBuffer.Free;
  if FRepeatClickTimer <> nil then StopRepeatClick;
  FGlowEffect.Free;
  inherited;
end;

procedure TscCustomButtonControl.SetCustomDropDown(Value: Boolean);
begin
  if FCustomDropDown <> Value then
  begin
    FCustomDropDown := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FSplitWidth := MulDiv(FSplitWidth, M, D);
end;

procedure TscCustomButtonControl.SetArrowDirection(Value: TscArrowDirection);
begin
  if Value <> FArrowDirection then
  begin
    FArrowDirection := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.SetArrowPosition(Value: TscArrowPosition);
begin
  if Value <> FArrowPosition then
  begin
    FArrowPosition := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FMouseIn := False;
end;

function TscCustomButtonControl.CanUpdateGlowBuffer(ACanvas: TCanvas;
   AGlowColor: TColor; AGlowSize, AGlowAlpha: Byte;
   ARect: TRect; AImageIndex: Integer; AImageList: TCustomImageList): Boolean;
begin
  Result := FUpdateGlowBuffer;
  if not Result then
    Result := (FStoredCaption <> Caption) or
              (FStoredWidth <> Width) or
              (FStoredHeight <> Height) or
              (FStoredGlowColor <> AGlowColor) or
              (FStoredGlowSize <> AGlowSize) or
              (FStoredGlowAlphaValue <> AGlowAlpha) or
              (FStoredFontName <> ACanvas.Font.Name) or
              (FStoredFontSize <> ACanvas.Font.Size) or
              (FStoredFontStyle <> ACanvas.Font.Style) or
              (FStoredImageIndex <> AImageIndex) or
              (FStoredRect <> ARect) or
              (FStoredSpacing <> FSpacing) or
              (FStoredMargin <> FMargin) or
              (FStoredLayout <> FLayout) or
              (FStoredImageList <> AImageList) or
              (FStoredWordWrap <> FWordWrap) or
              ((AImageList <> nil) and
              ((FStoredImageListW <> AImageList.Width) or (FStoredImageListH <> AImageList.Height)));

  FStoredCaption := Caption;
  FStoredWordWrap := FWordWrap;
  FStoredWidth := Width;
  FStoredHeight := Height;
  FStoredGlowColor := AGlowColor;
  FStoredGlowAlphaValue := AGlowAlpha;
  FStoredGlowSize := AGlowSize;
  FStoredFontName := ACanvas.Font.Name;
  FStoredFontSize := ACanvas.Font.Size;
  FStoredFontStyle := ACanvas.Font.Style;
  FStoredImageIndex := AImageIndex;
  FStoredRect := ARect;
  FStoredSpacing := FSpacing;
  FStoredMargin := FMargin;
  FStoredLayout := FLayout;
  FStoredImageList := AImageList;
  if (FStoredImageList <> nil) then
  begin
    FStoredImageListW := AImageList.Width;
    FStoredImageListH := AImageList.Height;
  end
  else
  begin
    FStoredImageListW := 0;
    FStoredImageListH := 0;
  end;
  FUpdateGlowBuffer := False;
end;

procedure TscCustomButtonControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RePaintControl;
end;

function TscCustomButtonControl.CanAnimate: Boolean;
begin
  Result := inherited CanAnimate;
  if Result then
    Result := not FPressed and not FDown
      and not FMenuDroppedDown and not FMenuTracking;
end;

procedure TscCustomButtonControl.SetImageGlow(Value: Boolean);
begin
  if FImageGlow <> Value then
  begin
    FImageGlow := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.OnGlowEffectChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    RePaintControl;
end;

function TscCustomButtonControl.GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean;
begin
  Result := False;
  if not (ACtrlState in FGlowEffect.States) then
    Exit;
  case ACtrlState of
    scsNormal: AColor := FGlowEffect.Color;
    scsHot: AColor := FGlowEffect.HotColor;
    scsPressed: AColor := FGlowEffect.PressedColor;
    scsFocused: AColor := FGlowEffect.FocusedColor;
    scsDisabled: AColor := FGlowEffect.Color;
  end;
  if AColor = clNone then AColor := FGlowEffect.Color;
  if ACtrlState = scsPressed then
  begin
    ASize := FGlowEffect.PressedGlowSize;
    AAlphaValue := FGlowEffect.PressedAlphaValue;
  end
  else
  begin
    ASize := FGlowEffect.GlowSize;
    if ACtrlState = scsDisabled then
      AAlphaValue := FGlowEffect.AlphaValue div 2
    else
      AAlphaValue := FGlowEffect.AlphaValue;
  end;
  Result := True;
end;

procedure TscCustomButtonControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then
    FDropDownMenu := nil;
  if (Operation = opRemove) and (AComponent = FGalleryMenu) then
    FGalleryMenu := nil;
end;

procedure TscCustomButtonControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
      begin
        if FUseImagesFromAction then
          Self.Images := Images;
        if FUseImageIndexFromAction then
          Self.ImageIndex := ImageIndex;
        RePaintControl;
      end;
    end;
end;

procedure TscCustomButtonControl.CheckGroupIndex;
begin
  if Parent = nil then Exit;
end;

procedure TscCustomButtonControl.SetDown(Value: Boolean);
begin
  if FDown <> Value then
  begin
    if FAnimation then StopAnimation;
    FDown := Value;
    RePaintControl;
    FOldCtrlState := CtrlState;
    if (FGroupIndex > 0) and FDown then CheckGroupIndex;
    if (FDown or FMustDoUp) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
        ButtonClick;
  end;
end;

procedure TscCustomButtonControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (FDropDownMenu <> nil) and ((Key = VK_UP) or (Key = VK_DOWN)) then
    TrackMenu(FSplitButton);
  if (FGalleryMenu <> nil) and ((Key = VK_UP) or (Key = VK_DOWN)) then
    TrackGalleryMenu(FSplitButton);
end;

procedure TscCustomButtonControl.SetShowMenuArrow(Value: Boolean);
begin
  if FShowMenuArrow <> Value then
  begin
    FShowMenuArrow := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.SetSplitButton(Value: Boolean);
begin
  if FSplitButton <> Value then
  begin
    FSplitButton := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.DoDropDownMenu;
var
  P: TPoint;
begin
  if (FDropDownMenu.Alignment = paCenter) then
    P := ClientToScreen(Point(Width div 2, Height))
  else
  if IsRightToLeft or (FDropDownMenu.Alignment = paRight) then
    P := ClientToScreen(Point(Width, Height))
  else
  begin
    if FArrowDirection = scadDefault then
      P := ClientToScreen(Point(0, Height))
    else
      P := ClientToScreen(Point(Width, 0));
  end;
  FDropDownMenu.Popup(P.X, P.Y);
end;

procedure TscCustomButtonControl.SetGalleryMenu(Value: TscGalleryMenu);
var
  Update: Boolean;
begin
  Update := Value <> FGalleryMenu;
  if (FGalleryMenu <> nil) and (Value <> FGalleryMenu) then
  begin
    FGalleryMenu.OnInternalChange := nil;
    FGalleryMenu.OnInternalMenuClose := nil;
  end;
  FGalleryMenu := Value;
  if FGalleryMenu <> nil
  then
  begin
    FGalleryMenu.OnInternalChange := OnGalleryMenuChanged;
    FGalleryMenu.OnInternalMenuClose := OnGalleryMenuClose;
  end;
  if Update then RePaintControl;
end;

procedure TscCustomButtonControl.SetDropDownMenu;
begin
  if FDropDownMenu <> Value then
  begin
    FDropDownMenu := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.StartRepeatClick;
begin
  if FRepeatClickTimer <> nil then FreeAndNil(FRepeatClickTimer);
  FRepeatClickTimer := TTimer.Create(Self);
  FRepeatClickTimer.Enabled := False;
  FRepeatClickTimer.OnTimer := WaitClickTimerProc;
  FRepeatClickTimer.Interval := 500;
  FRepeatClickTimer.Enabled := True;
end;

procedure TscCustomButtonControl.StopRepeatClick;
begin
  if FRepeatClickTimer <> nil then
  begin
    FRepeatClickTimer.Enabled := False;
    FreeAndNil(FRepeatClickTimer);
  end;
end;

procedure TscCustomButtonControl.RepeatClickTimerProc(Sender: TObject);
begin
  ButtonClick;
end;

procedure TscCustomButtonControl.WaitClickTimerProc(Sender: TObject);
begin
  FRepeatClickTimer.Enabled := False;
  FRepeatClickTimer.OnTimer := RepeatClickTimerProc;
  FRepeatClickTimer.Interval := FRepeatClickInterval;
  FRepeatClickTimer.Enabled := True;
end;

procedure TscCustomButtonControl.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.SetMargin(Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    RePaintControl;
  end;
end;

procedure TscCustomButtonControl.DoDialogChar;
begin
end;

procedure TscCustomButtonControl.SetCanFocused(Value: Boolean);
begin
  if FCanFocused <> Value then
  begin
    FCanFocused := Value;
    TabStop := FCanFocused;
  end;
end;

procedure TscCustomButtonControl.Click;
begin
end;

procedure TscCustomButtonControl.ButtonClick;
begin
  if FDisableClick then Exit;

  if (csDesigning in ComponentState) or (csLoading in ComponentState) then Exit;
  if Assigned(FOnClick) and (Action <> nil) and
     DelegatesEqual(@FOnClick, @Action.OnExecute)
  then
    FOnClick(Self)
  else
    if not (csDesigning in ComponentState) and (ActionLink <> nil)
    then
      ActionLink.Execute(Self)
     else
       if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TscCustomButtonControl.WndProc(var Message: TMessage);
begin
   case Message.Msg of
      WM_CREATE:
        begin
          FMenuTracking := False;
        end;
      WM_TIMER:
       if (TWMTimer(Message).TimerID = 21) and FMenuTracking then
       begin
         KillTimer(Handle, 21);
         FMenuTracking := False;
         if not (csDestroying in ComponentState) then
            RePaintControl;
          FOldCtrlState := CtrlState;
       end;
      WM_KEYDOWN:
        if FCanFocused then
          if (TWMKeyDown(Message).CharCode = VK_SPACE) and
             (not FDown or ((FGroupIndex > 0) and FAllowAllUp)) then
          begin
            if FGroupIndex > 0 then
            begin
              if FAllowAllUp and FDown then FMustDoUp := True;
              Down := True;
             end
            else
            if not FIsDown then
            begin
              if FCustomDropDown and not FSplitButton then
              begin
                Application.ProcessMessages;
                TrackCustomWindow(False);
                ButtonClick;
              end
              else
              if (FDropDownMenu <> nil) and not FSplitButton then
              begin
                Application.ProcessMessages;
                TrackMenu(False);
                ButtonClick;
              end
              else
              if (FGalleryMenu <> nil) and not FSplitButton then
              begin
                Application.ProcessMessages;
                TrackGalleryMenu(False);
                ButtonClick;
              end
              else
              begin
                FIsDown := True;
                RePaintControl;
              end;
            end;
            if FRepeatClick and (FRepeatClickTimer = nil) then StartRepeatClick;
          end
          else
          if (TWMKeyDown(Message).CharCode = VK_RETURN) then
          begin
            if FGroupIndex > 0 then
            begin
              if FAllowAllUp and FDown then
                FMustDoUp := True
              else
              if not FDown then
                Down := True;
            end
            else
            begin
              if not FMenuTracking and FCustomDropDown and not FSplitButton then
              begin
                Application.ProcessMessages;
                TrackCustomWindow(False);
              end
              else
              if not FMenuTracking and (FDropDownMenu <> nil) and not FSplitButton then
              begin
                Application.ProcessMessages;
                TrackMenu(False);
              end
              else
              if not FMenuTracking and (FGalleryMenu <> nil) and not FSplitButton then
              begin
                Application.ProcessMessages;
                TrackGalleryMenu(False);
              end;
              if not FMenuTracking then ButtonClick;
            end;
          end;
      WM_KEYUP:
        if FCanFocused and (TWMKeyUp(Message).CharCode = VK_RETURN) and
           FDown and (FGroupIndex > 0) and FAllowAllUp and FMustDoUp then
        begin
          Down := False;
          FMustDoUp := False;
        end
        else
        if FCanFocused and (TWMKeyUp(Message).CharCode = VK_SPACE) and
           (not FDown or ((FGroupIndex > 0) and FAllowAllUp)) then
        begin
          if (FGroupIndex > 0) and FMustDoUp then
          begin
            Down := False;
            FMustDoUp := False;
          end
          else
          begin
            if (FGalleryMenu = nil) or ((FGalleryMenu <> nil) and FSplitButton) then
            begin
              FIsDown := False;
              RePaintControl;
              if FRepeatClick and (FRepeatClickTimer <> nil) then StopRepeatClick;
              ButtonClick;
            end;
          end;
        end;
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        if FCanFocused and not (csDesigning in ComponentState) and not Focused then
          SetFocus;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TscCustomButtonControl.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if FCanFocused and ((FDropDownMenu <> nil) or (FGalleryMenu <> nil))  then
    case Msg.CharCode of
      VK_UP, VK_DOWN: Msg.Result := 1;
    end;
end;

procedure TscCustomButtonControl.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      if FCanFocused then
        SetFocus;
      DoDialogChar;
      Result := 1;
    end
  else
    inherited;
end;

procedure TscCustomButtonControl.WMSetFocus(var Message: TWMSETFOCUS);
begin
  inherited;
  if FCanFocused then
  begin
    FUpdateParentBuffer := True;
    if DrawTextMode = scdtmGDIPlus then
      Invalidate
    else
      RePaint;
  end;
  if FRepeatClick and (FRepeatClickTimer <> nil) then
    StopRepeatClick;
  FOldCtrlState := CtrlState;
end;

procedure TscCustomButtonControl.WMKillFocus(var Message: TWMKILLFOCUS);
begin
  inherited;
  if FCanFocused then
    RePaint;
  FOldCtrlState := CtrlState;
end;

procedure TscCustomButtonControl.DoMouseEnter;
begin
  inherited;
  if FPressed and FRepeatClick then StartRepeatClick;
end;

procedure TscCustomButtonControl.DoMouseLeave;
begin
  inherited;
  if FRepeatClick and (FRepeatClickTimer <> nil) then StopRepeatClick;
end;

function TscCustomButtonControl.GetCtrlState: TscsCtrlState;
begin
  if FDown then
    Result := scsPressed
  else
  if not Enabled then
    Result := scsDisabled else
  if FMenuDroppedDown then
    Result := scsHot
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn then
    Result := scsHot
  else
  if Focused and FCanFocused then
    Result := scsFocused
  else
    Result := scsNormal;
end;

procedure TscCustomButtonControl.OnGalleryMenuChanged(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscCustomButtonControl.OnGalleryMenuClose(Sender: TObject);
begin
  FMenuTracking := False;
  FMenuDroppedDown := False;
  FIsDown := False;
  FPressed := False;
  RePaintControl;
  FOldCtrlState := CtrlState;
end;

procedure TscCustomButtonControl.TrackCustomWindow(ADropDownPressed: Boolean);
begin
  if ADropDownPressed then FMenuDroppedDown := True;
  if not FSplitButton then FIsDown := True;
  RePaintControl;
  if not FSplitButton or ADropDownPressed then
  begin
    FMenuTracking := True;
    FDropDownCall := True;
    RePaintControl;
    if Assigned(FOnDropDown) then
      FOnDropDown(Self);
  end;
end;

procedure TscCustomButtonControl.CloseUp(AAcceptChanges: Boolean);
begin
  FMenuTracking := False;
  FMenuDroppedDown := False;
  FIsDown := False;
  FPressed := False;
  RePaintControl;
  FOldCtrlState := CtrlState;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self, AAcceptChanges);
end;

procedure TscCustomButtonControl.TrackGalleryMenu(ADropDownPressed: Boolean);
var
  R: TRect;
  P: TPoint;
begin
  if ADropDownPressed then FMenuDroppedDown := True;
  if not FSplitButton then FIsDown := True;
  RePaintControl;
  if not FSplitButton or ADropDownPressed  then
  begin
    FMenuTracking := True;
    FDropDownCall := True;
    P := ClientToScreen(Point(0, 0));
    R := Rect(P.X, P.Y, P.X + Width, P.Y + Height);
    FGalleryMenu.FScaleFactor := Self.FScaleFactor;
    if FShowGalleryMenuFromTop or FShowGalleryMenuFromRight then
      FGalleryMenu.PopupFromRectExt(R, FShowGalleryMenuFromTop, FShowGalleryMenuFromRight)
    else
      FGalleryMenu.PopupFromRect(R);
  end;
end;

procedure TscCustomButtonControl.TrackMenu(ADropDownPressed: Boolean);
begin
  if ADropDownPressed then FMenuDroppedDown := True;
  if not FSplitButton then FIsDown := True;
  RePaintControl;
  if not FSplitButton or ADropDownPressed  then
  begin
    FMenuTracking := True;
    FDropDownCall := True;
    DoDropDownMenu;
    FMenuDroppedDown := False;
    FIsDown := False;
    FPressed := False;
    SetTimer(Handle, 21, 50, nil);
    FOldCtrlState := CtrlState;
  end;
end;

procedure TscCustomButtonControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);

var
  FDropButtonPressed: Boolean;
begin
  inherited;
  if FMenuTracking then
  begin
    FMouseIn := True;
    FOldCtrlState := scsHot;
    Exit;
  end;

  FDropDownCall := False;

  if (Button = mbLeft) and (FGroupIndex > 0) then
  begin
    if FAllowAllUp and FDown then FMustDoUp := True;
    Down := True;
  end
  else
  if Button = mbLeft then
  begin
    FPressed := True;
    FDropButtonPressed := False;

    if (FDropDownMenu <> nil) or (FGalleryMenu <> nil ) or FCustomDropDown then
    begin
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          FDropButtonPressed := FSplitButton and
            PtInRect(Rect(Width - FSplitWidth, 0, Width, Height), Point(X, Y))
        else
          FDropButtonPressed := FSplitButton and
            PtInRect(Rect(0, 0, FSplitWidth, Height), Point(X, Y));
      end
      else
        FDropButtonPressed := FSplitButton and
          PtInRect(Rect(0, Height - FSplitWidth, Width, Height), Point(X, Y));
    end;

    if FDropDownMenu <> nil then
      TrackMenu(FDropButtonPressed)
    else
    if FGalleryMenu <> nil then
      TrackGalleryMenu(FDropButtonPressed)
    else
    if FCustomDropDown then
      TrackCustomWindow(FDropButtonPressed)
    else
      RePaintControl;

    if FRepeatClick then StartRepeatClick;
  end;
end;

procedure TscCustomButtonControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
begin
  if FMenuTracking then
  begin
    inherited;
    Exit;
  end;

  if (Button = mbLeft) and (FGroupIndex > 0) and FMustDoUp then
  begin
    Down := False;
    FMustDoUp := False;
  end
  else
  if (Button = mbLeft) and FPressed then
  begin
    FPressed := False;
    RePaintControl;
    if FRepeatClick and (FRepeatClickTimer <> nil) and not FDown then StopRepeatClick;
    if FMouseIn and not FDown then ButtonClick;
  end;

  inherited;
end;

constructor TscGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FStorePaintBuffer := True;
  FGlowEffect := TscGlowEffect.Create;
  FGlowEffect.OnChange := OnGlowEffectChange;
  FImageGlow := True;
  FImages := nil;
  FImageIndex := -1;
  FShowCheckBox := False;
  FChecked := False;
  FAutoEnabledControls := False;
  FAlignment := taLeftJustify;
  Width := 150;
  Height := 130;
  FCaptionMouseIn := False;
  FCaptionMouseDown := False;
  FCustomImages := nil;
  FCustomImageIndex := -1;
  FCustomImageDisabledIndex := -1;
  FCustomCheckedImageIndex := -1;
  FCustomCheckedImageHotIndex := -1;
  FCustomCheckedImagePressedIndex := -1;
  FCustomCheckedImageDisabledIndex := -1;
  FCustomUnCheckedImageIndex := -1;
  FCustomUnCheckedImageHotIndex := -1;
  FCustomUnCheckedImagePressedIndex := -1;
  FCustomUnCheckedImageDisabledIndex := -1;

  FGlowBuffer := nil;
  FGlowImageBuffer := nil;
  FStoredCaption := '';
  FStoredWidth := 0;
  FStoredHeight := 0;
  FStoredGlowColor := clNone;
  FStoredFontSize := 0;
  FStoredFontStyle := [];
  FUpdateGlowBuffer := True;
  FStoredFontName := '';
  FStoredImageIndex := -1;
  FStoredAlignment := taLeftJustify;
  FStoredImageList := nil;
end;

destructor TscGroupBox.Destroy;
begin
  if FGlowBuffer <> nil then
    FGlowBuffer.Free;
  if FGlowImageBuffer <> nil then
    FGlowImageBuffer.Free;
  FGlowEffect.Free;
  inherited;
end;

function TscGroupBox.CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
begin
  Result := FUpdateGlowBuffer;
  if not Result then
    Result := (FStoredCaption <> Caption) or
              (FStoredWidth <> Width) or
              (FStoredHeight <> Height) or
              (FStoredGlowColor <> FGlowEffect.Color) or
              (FStoredFontName <> ACanvas.Font.Name) or
              (FStoredFontSize <> ACanvas.Font.Size) or
              (FStoredImageIndex <> FImageIndex) or
              (FStoredFontStyle <> ACanvas.Font.Style) or
              (FStoredAlignment <> FAlignment) or
              (FStoredImageList <> FImages);

  FStoredCaption := Caption;
  FStoredWidth := Width;
  FStoredHeight := Height;
  FStoredGlowColor := FGlowEffect.Color;
  FStoredFontName := ACanvas.Font.Name;
  FStoredFontSize := ACanvas.Font.Size;
  FStoredFontStyle := ACanvas.Font.Style;
  FStoredImageIndex := FImageIndex;
  FStoredAlignment := FAlignment;
  FStoredImageList := FImages;
  FUpdateGlowBuffer := False;
end;

procedure TscGroupBox.SetFramePosition(Value: TscGroupBoxFramePosition);
begin
  if FFramePosition <> Value then
  begin
    FFramePosition := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomCheckedImageIndex(Value: Integer);
begin
  if FCustomCheckedImageIndex <> Value then
  begin
    FCustomCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomCheckedImageHotIndex(Value: Integer);
begin
  if FCustomCheckedImageHotIndex <> Value then
  begin
    FCustomCheckedImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomCheckedImagePressedIndex(Value: Integer);
begin
  if FCustomCheckedImagePressedIndex <> Value then
  begin
    FCustomCheckedImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomCheckedImageDisabledIndex(Value: Integer);
begin
  if FCustomCheckedImageDisabledIndex <> Value then
  begin
    FCustomCheckedImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomUnCheckedImageIndex(Value: Integer);
begin
  if FCustomUnCheckedImageIndex <> Value then
  begin
    FCustomUnCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomUnCheckedImageHotIndex(Value: Integer);
begin
  if FCustomUnCheckedImageHotIndex <> Value then
  begin
    FCustomUnCheckedImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomUnCheckedImagePressedIndex(Value: Integer);
begin
  if FCustomUnCheckedImagePressedIndex <> Value then
  begin
    FCustomUnCheckedImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomUnCheckedImageDisabledIndex(Value: Integer);
begin
  if FCustomUnCheckedImageDisabledIndex <> Value then
  begin
    FCustomUnCheckedImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomImageDisabledIndex(Value: Integer);
begin
  if FCustomImageDisabledIndex <> Value then
  begin
    FCustomImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomImageIndex(Value: Integer);
begin
  if FCustomImageIndex <> Value then
  begin
    FCustomImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetImageGlow(Value: Boolean);
begin
  if FImageGlow <> Value then
  begin
    FImageGlow := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.OnGlowEffectChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    FUpdateGlowBuffer := True;
    RePaintControl;
    ReAlign;
  end;
end;

function TscGroupBox.GetCaptionHeight(ACanvas: TCanvas): Integer;
var
  S: TPoint;
begin
  Result := ACanvas.TextHeight('Wq');
  if FImages <> nil then
    Result := Max(Result, FImages.Height);
  if FShowCheckBox then
    Result := Max(Result, GetCheckBoxSize(ACanvas, FScaleFactor) + 7);
  if FGlowEffect.Enabled then
    Inc(Result, FGlowEffect.GlowSize * 2 + FGlowEffect.Offset);
  if FShowCheckBox then
  begin
    S := GetCustomCheckBoxSize;
    if S.Y + 2 > Result then Result := S.Y + 2;
  end;
end;

function TscGroupBox.GetCustomCheckBoxSize: TPoint;
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

procedure TscGroupBox.DrawCustomCheckBox(ACanvas: TCanvas;
  ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
var
  CIndex: Integer;
  A: Byte;
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
    scsHot:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageHotIndex;
        cbChecked: CIndex := FCustomCheckedImageHotIndex;
      end;
    end;
    scsPressed:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImagePressedIndex;
        cbChecked: CIndex := FCustomCheckedImagePressedIndex;
      end;
    end;
    scsDisabled:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageDisabledIndex;
        cbChecked: CIndex := FCustomCheckedImageDisabledIndex;
      end;
    end;
  end;
  A := 255;
  if CIndex = -1 then
  begin
    case ACheckBoxState of
      cbUnChecked: CIndex := FCustomUnCheckedImageIndex;
      cbChecked: CIndex := FCustomCheckedImageIndex;
    end;
    if ACtrlState = scsDisabled then
      A := 120;
  end;
  if (FCustomImages <> nil) and (FCustomImages.IsIndexAvailable(CIndex))
  then
  begin
    if (FCustomImages is TscImageCollection) and not (TscImageCollection(FCustomImages).Images[CIndex].Bitmap.Empty) then
      TscImageCollection(FCustomImages).DrawBitmap(ACanvas, ARect, CIndex, A)
    else
      FCustomImages.Draw(ACanvas, ARect, CIndex, FScaleFactor);
  end;
end;

procedure TscGroupBox.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);

function GetCaptionWidth: Integer;
var
  R: TRect;
  S: TPoint;
begin
  R := Rect(0, 0, 0, 0);
  DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R,
             DT_LEFT or DT_CALCRECT);
  Result := R.Width + 2;
  if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
    Inc(Result, FImages.Width + 2);
  if FShowCheckBox then 
  begin
    S := GetCustomCheckBoxSize;
    if S.X > 0 then
      Inc(Result, S.X + 5)
    else
    begin
      S.X := GetCheckBoxSize(ACanvas, FScaleFactor) + 7;
      Inc(Result, S.X);
    end;
  end;
  if FGlowEffect.Enabled then
    Inc(Result, FGlowEffect.GlowSize * 2 + FGlowEffect.Offset);
end;

function GetCheckBoxState: TscsCtrlState;
begin
  if Enabled then
  begin
    if FCaptionMouseDown and FCaptionMouseIn then
      Result := scsPressed
    else
    if FCaptionMouseIn then
      Result := scsHot
    else
      Result := scsNormal;
  end
  else
    Result := scsDisabled;
end;

var
  FrameRect, R: TRect;
  W, H: Integer;
  SaveIndex: Integer;
  CIIndex: Integer;
  S: TPoint;
  FUpdateGlow: Boolean;
begin
  ACanvas.Font.Assign(Font);
  if (seFont in StyleElements) and IsCustomStyle then
    ACanvas.Font.Color := GetGroupBoxTextColor(CtrlState);
  W := GetCaptionWidth;
  H := GetCaptionHeight(ACanvas);
  case FFramePosition of
    scgfpDefault, scgfpNone: FrameRect := Rect(0, H div 2, Width, Height);
    scgfpOverText: FrameRect := Rect(0, 0, Width, Height);
    scgfpUnderText: FrameRect := Rect(0, H, Width, Height);
  end;
  if BiDiMode = bdRightToLeft then
   case FAlignment of
      taRightJustify: FCaptionRect := Rect(10, 0, 10 + W + 4, H);
      taLeftJustify: FCaptionRect := Rect(Width - 10 - W, 0, Width - 10, H);
      taCenter: FCaptionRect := Rect(Width div 2 - W div 2, 0,
         Width div 2 + W div 2, H);
    end
  else
    case FAlignment of
      taLeftJustify: FCaptionRect := Rect(10, 0, 10 + W, H);
      taRightJustify: FCaptionRect := Rect(Width - 10 - W, 0, Width - 10, H);
      taCenter: FCaptionRect := Rect(Width div 2 - W div 2, 0,
         Width div 2 + W div 2, H);
    end;
  if FCaptionRect.Left < 10 then FCaptionRect.Left := 10;
  if FCaptionRect.Right > Width - 10 then FCaptionRect.Right := Width - 10;

  if FFramePosition <> scgfpNone then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      if FFramePosition = scgfpDefault then
        ExcludeClipRect(ACanvas.Handle, FCaptionRect.Left, FCaptionRect.Top,
         FCaptionRect.Right, FCaptionRect.Bottom);

      CIIndex := FCustomImageIndex;
      if (ACtrlState = scsDisabled) and (FCustomImageDisabledIndex >= 0) then
        CIIndex := FCustomImageDisabledIndex;

      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIIndex) then
      begin
        if (FCustomImages is TscImageCollection) and
           not TscImageCollection(FCustomImages).Images[CIIndex].Bitmap.Empty
         then
          TscImageCollection(FCustomImages).Draw(ACanvas, FrameRect, CIIndex)
        else
          FCustomImages.Draw(ACanvas, FrameRect, CIIndex, FScaleFactor);
       end
       else
         DrawGroupBoxFrame(ACanvas, FrameRect);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  SaveIndex := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, FCaptionRect.Left, FCaptionRect.Top,
      FCaptionRect.Right, FCaptionRect.Bottom);
    R := FCaptionRect;
  
    if FShowCheckBox then
    begin
      S := GetCustomCheckBoxSize;
      Inc(R.Top, 2);
      if S.X = 0 then
      begin
        S.X := GetCheckBoxSize(ACanvas, FScaleFactor) + 5;
        if BiDiMode = bdRightToLeft then
          R.Left := R.Right - S.X
        else
          R.Right := R.Left + S.X;
        if FChecked then
          DrawCheckBox(ACanvas, R, GetCheckBoxState, cbChecked, FScaleFactor)
        else
          DrawCheckBox(ACanvas, R, GetCheckBoxState, cbUnChecked, FScaleFactor);
        R := FCaptionRect;
        if BiDiMode = bdRightToLeft then
          Dec(R.Right, S.X)
        else
          Inc(R.Left, S.X);
      end
      else
      begin
        if BiDiMode = bdRightToLeft then
          R.Left := R.Right - S.X - 5
        else
          R.Right := R.Left + S.X + 5;
        if FChecked then
          DrawCustomCheckBox(ACanvas, R, GetCheckBoxState, cbChecked)
        else
          DrawCustomCheckBox(ACanvas, R, GetCheckBoxState, cbUnChecked);
        R := FCaptionRect;
        if BiDiMode = bdRightToLeft then
          Dec(R.Right, S.X + 5)
        else
          Inc(R.Left, S.X + 5); 
      end;
    end;
    ACanvas.Brush.Style := bsClear;
    if FGlowEffect.Enabled then
    begin
      if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count)  then
      begin
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
        if BiDiMode = bdRightToLeft  then
          DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
            ACanvas, R, 0, FGlowEffect.GlowSize + 4,
            blGlyphRight, Caption, FImageIndex, FImages,
            CtrlState = scsNormal, False, clBlack,
              FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
              FGlowEffect.Intensive, FGlowEffect.AlphaValue, FImageGlow, False, IsRightToLeft, False, FScaleFactor)
        else
           DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
             ACanvas, R, 0, FGlowEffect.GlowSize + 4,
             blGlyphLeft, Caption, FImageIndex, FImages,
             CtrlState = scsNormal, False, clBlack,
              FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
               FGlowEffect.Intensive, FGlowEffect.AlphaValue, FImageGlow, False, IsRightToLeft, False, FScaleFactor)
      end
      else
      begin
        if BidiMode = bdRightToLeft then
          Dec(R.Right, FGlowEffect.GlowSize)
        else
          Inc(R.Left, FGlowEffect.GlowSize);
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
        DrawTextWithGlowBuffer(FGlowBuffer, FUpdateGlow,
          ACanvas, R, Caption, DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_NOCLIP,
          FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
          FGlowEffect.Intensive, FGlowEffect.AlphaValue, IsRightToLeft, False);
      end;
    end
    else
    if (FImages <> nil) and (FImageIndex >= 0) and (FImageIndex < FImages.Count)  then
    begin
      if BiDiMode = bdRightToLeft  then
        DrawImageAndText(ACanvas, R, 0, 4,
         blGlyphRight, Caption, FImageIndex, FImages,
          CtrlState = scsNormal, False, clBlack, False, IsRightToLeft, False, FScaleFactor)
      else
         DrawImageAndText(ACanvas, R, 0, 4,
         blGlyphLeft, Caption, FImageIndex, FImages,
          CtrlState = scsNormal, False, clBlack, False, IsRightToLeft, False, FScaleFactor);
    end
    else
      scDrawText(ACanvas, Caption, R, IsRightToLeft, False);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscGroupBox.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscGroupBox.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    RePaintControl;
  end;
end;


procedure TscGroupBox.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetShowCheckBox(Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    RePaintControl;
  end;
end;

procedure TscGroupBox.SetChecked(Value: Boolean);
var
  I: Integer;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    RePaintControl;
    if FAutoEnabledControls then
      for I := 0 to ControlCount -1 do
        Controls[i].Enabled := FChecked;
    if Assigned(FOnChecked) then FOnChecked(Self);
  end;
end;

procedure TscGroupBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FShowCheckBox and FCaptionMouseIn then
  begin
    FCaptionMouseIn := False;
    RePaintControl;
  end;
end;

procedure TscGroupBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TscGroupBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FShowCheckBox then
  begin
    if PtInRect(FCaptionRect, Point(X, Y)) and not FCaptionMouseIn then
    begin
      FCaptionMouseIn := True;
      RePaintControl;
    end
    else
    if not PtInRect(FCaptionRect, Point(X, Y)) and FCaptionMouseIn then
    begin
      FCaptionMouseIn := False;
      RePaintControl;
    end;
  end
end;

procedure TscGroupBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FShowCheckBox and (Button = mbLeft) then
  begin
    if PtInRect(FCaptionRect, Point(X, Y)) and not FCaptionMouseDown then
    begin
      FCaptionMouseDown := True;
      RePaintControl;
    end
    else
    if not PtInRect(FCaptionRect, Point(X, Y)) and FCaptionMouseDown then
    begin
      FCaptionMouseDown := False;
      RePaintControl;
    end;
  end
end;

procedure TscGroupBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FShowCheckBox and (Button = mbLeft) then
  begin
    FCaptionMouseDown := False;
    if PtInRect(FCaptionRect, Point(X, Y)) then
      Checked := not Checked;
   end;
end;

procedure TscGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      if FShowCheckBox and not FChecked then
       Checked := True;
      SelectFirst;
      Result := 1;
    end else
      inherited;
end;

procedure TscGroupBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RePaintControl;
  ReAlign;
end;

procedure TscGroupBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Canvas.Font := Font;
  Rect.Top := GetCaptionHeight(Canvas);
  InflateRect(Rect, -3, -3);
end;

procedure TscGroupBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

constructor TscButtonColorOptions.Create;
begin
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
  FTitleFontNormalColor := clBtnText;
  FTitleFontHotColor := clBtnText;
  FTitleFontPressedColor := clBtnText;
  FTitleFontFocusedColor := clBtnText;
  FTitleFontDisabledColor := clBtnShadow;
  FStyleColors := True;
  FOnChange := nil;
  FState := scsNormal;
end;

procedure TscButtonColorOptions.Assign(Source: TPersistent);
begin
  if Source is TscButtonColorOptions then
  begin
    FNormalColor := TscButtonColorOptions(Source).FNormalColor;
    FHotColor := TscButtonColorOptions(Source).FHotColor;
    FPressedColor := TscButtonColorOptions(Source).FPressedColor;
    FFocusedColor := TscButtonColorOptions(Source).FFocusedColor;
    FDisabledColor := TscButtonColorOptions(Source).FDisabledColor;
    FFrameNormalColor := TscButtonColorOptions(Source).FFrameNormalColor;
    FFrameHotColor := TscButtonColorOptions(Source).FFrameHotColor;
    FFramePressedColor := TscButtonColorOptions(Source).FFramePressedColor;
    FFrameFocusedColor := TscButtonColorOptions(Source).FFrameFocusedColor;
    FFrameDisabledColor := TscButtonColorOptions(Source).FFrameDisabledColor;
    FFrameWidth := TscButtonColorOptions(Source).FFrameWidth;
    FFontNormalColor := TscButtonColorOptions(Source).FFontNormalColor;
    FFontHotColor := TscButtonColorOptions(Source).FFontHotColor;
    FFontPressedColor := TscButtonColorOptions(Source).FFontPressedColor;
    FFontFocusedColor := TscButtonColorOptions(Source).FFontFocusedColor;
    FFontDisabledColor := TscButtonColorOptions(Source).FFontDisabledColor;
    FTitleFontNormalColor := TscButtonColorOptions(Source).FTitleFontNormalColor;
    FTitleFontHotColor := TscButtonColorOptions(Source).FTitleFontHotColor;
    FTitleFontPressedColor := TscButtonColorOptions(Source).FTitleFontPressedColor;
    FTitleFontFocusedColor := TscButtonColorOptions(Source).FTitleFontFocusedColor;
    FTitleFontDisabledColor := TscButtonColorOptions(Source).FTitleFontDisabledColor;
    FStyleColors := TscButtonColorOptions(Source).FStyleColors;
  end
  else
    inherited Assign(Source);
end;

function TscButtonColorOptions.GetColor: TColor;
begin
  Result := NormalColor;
  case FState of
    scsHot: Result := HotColor;
    scsPressed: Result := PressedColor;
    scsFocused: Result := FocusedColor;
    scsDisabled: Result := DisabledColor;
  end;
end;

function TscButtonColorOptions.GetFrameColor: TColor;
begin
  Result := FrameNormalColor;
  case FState of
    scsHot: Result := FrameHotColor;
    scsPressed: Result := FramePressedColor;
    scsFocused: Result := FrameFocusedColor;
    scsDisabled: Result := FrameDisabledColor;
  end;
end;

function TscButtonColorOptions.GetFontColor: TColor;
begin
  Result := FontNormalColor;
  case FState of
    scsHot: Result := FontHotColor;
    scsPressed: Result := FontPressedColor;
    scsFocused: Result := FontFocusedColor;
    scsDisabled: Result := FontDisabledColor;
  end;
end;

function TscButtonColorOptions.GetTitleFontColor: TColor;
begin
  Result := TitleFontNormalColor;
  case FState of
    scsHot: Result := TitleFontHotColor;
    scsPressed: Result := TitleFontPressedColor;
    scsFocused: Result := TitleFontFocusedColor;
    scsDisabled: Result := TitleFontDisabledColor;
  end;
end;

function TscButtonColorOptions.GetNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscButtonColorOptions.GetHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscButtonColorOptions.GetPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscButtonColorOptions.GetFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFocusedColor)
  else
    Result := FFocusedColor;
end;

function TscButtonColorOptions.GetDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

function TscButtonColorOptions.GetFrameNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameNormalColor)
  else
    Result := FFrameNormalColor;
end;

function TscButtonColorOptions.GetFrameHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameHotColor)
  else
    Result := FFrameHotColor;
end;

function TscButtonColorOptions.GetFramePressedColor: TColor;
begin
 if FStyleColors then
    Result := GetStyleColor(FFramePressedColor)
  else
    Result := FFramePressedColor;
end;

function TscButtonColorOptions.GetFrameFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameFocusedColor)
  else
    Result := FFrameFocusedColor;
end;

function TscButtonColorOptions.GetFrameDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFrameDisabledColor)
  else
    Result := FFrameDisabledColor;
end;

function TscButtonColorOptions.GetFontNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontNormalColor)
  else
    Result := FFontNormalColor;
end;

function TscButtonColorOptions.GetFontHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontHotColor)
  else
    Result := FFontHotColor;
end;

function TscButtonColorOptions.GetFontPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontPressedColor)
  else
    Result := FFontPressedColor;
end;

function TscButtonColorOptions.GetFontFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontFocusedColor)
  else
    Result := FFontFocusedColor;
end;

function TscButtonColorOptions.GetFontDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FFontDisabledColor)
  else
    Result := FFontDisabledColor;
end;

function TscButtonColorOptions.GetTitleFontNormalColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FTitleFontNormalColor)
  else
    Result := FTitleFontNormalColor;
end;

function TscButtonColorOptions.GetTitleFontHotColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FTitleFontHotColor)
  else
    Result := FTitleFontHotColor;
end;

function TscButtonColorOptions.GetTitleFontPressedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FTitleFontPressedColor)
  else
    Result := FTitleFontPressedColor;
end;

function TscButtonColorOptions.GetTitleFontFocusedColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FTitleFontFocusedColor)
  else
    Result := FTitleFontFocusedColor;
end;

function TscButtonColorOptions.GetTitleFontDisabledColor: TColor;
begin
  if FStyleColors then
    Result := GetStyleColor(FTitleFontDisabledColor)
  else
    Result := FTitleFontDisabledColor;
end;

procedure TscButtonColorOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFocusedColor(Value: TColor);
begin
  if FFocusedColor <> Value then
  begin
    FFocusedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFrameNormalColor(Value: TColor);
begin
  if FFrameNormalColor <> Value then
  begin
    FFrameNormalColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFrameHotColor(Value: TColor);
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFramePressedColor(Value: TColor);
begin
  if FFramePressedColor <> Value then
  begin
    FFramePressedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFrameFocusedColor(Value: TColor);
begin
  if FFrameFocusedColor <> Value then
  begin
    FFrameFocusedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFrameDisabledColor(Value: TColor);
begin
  if FFrameDisabledColor <> Value then
  begin
    FFrameDisabledColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFontNormalColor(Value: TColor);
begin
  if FFontNormalColor <> Value then
  begin
    FFontNormalColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFontHotColor(Value: TColor);
begin
  if FFontHotColor <> Value then
  begin
    FFontHotColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFontPressedColor(Value: TColor);
begin
  if FFontPressedColor <> Value then
  begin
    FFontPressedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFontFocusedColor(Value: TColor);
begin
  if FFontFocusedColor <> Value then
  begin
    FFontFocusedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFontDisabledColor(Value: TColor);
begin
  if FFontDisabledColor <> Value then
  begin
    FFontDisabledColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetTitleFontNormalColor(Value: TColor);
begin
  if FTitleFontNormalColor <> Value then
  begin
    FTitleFontNormalColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetTitleFontHotColor(Value: TColor);
begin
  if FTitleFontHotColor <> Value then
  begin
    FTitleFontHotColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetTitleFontPressedColor(Value: TColor);
begin
  if FTitleFontPressedColor <> Value then
  begin
    FTitleFontPressedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetTitleFontFocusedColor(Value: TColor);
begin
  if FTitleFontFocusedColor <> Value then
  begin
    FTitleFontFocusedColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetTitleFontDisabledColor(Value: TColor);
begin
  if FTitleFontDisabledColor <> Value then
  begin
    FTitleFontDisabledColor := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetFrameWidth(Value: Integer);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscButtonColorOptions.Changed;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscButton.Create(AOwner: TComponent);
begin
  inherited;
  FColorOptions := TscButtonColorOptions.Create;
  FColorOptions.OnChange := OnColorOptionsChange;
  FToolPushButtonStyle := False;
  FUseFontColorToStyleColor := False;
  FImageMargin := 0;
  FActive := False;
  FCancel := False;
  FDefault := False;
  FShowCaption := True;
  FModalResult := mrNone;
  FModalSetting := False;
  FWidthWithCaption := 0;
  FWidthWithoutCaption := 0;
  FScaleMarginAndSpacing := False;
  FGlowBuffer := nil;
  FGlowImageBuffer := nil;
  FTitle := '';
  FTitleFont := TFont.Create;
  FTitleFont.Assign(Font);
  FTitleFont.Style := [fsBold];
  FTitleFont.OnChange := OnTitleFontChange;
  FHotImageIndex := -1;
  FFocusedImageIndex := -1;
  FPressedImageIndex := -1;
  FUseGalleryMenuCaption := False;
  FUseGalleryMenuImage := False;
  FCustomImageNormalIndex := -1;
  FCustomImageHotIndex := -1;
  FCustomImagePressedIndex := -1;
  FCustomImageFocusedIndex := -1;
  FCustomImageDisabledIndex := -1;
end;

destructor TscButton.Destroy;
begin
  FTitleFont.Free;
  FColorOptions.Free;
  inherited;
end;

function TscButton.GetCtrlState: TscsCtrlState;
begin
  if FDown then
    Result := scsPressed
  else
  if not Enabled then
    Result := scsDisabled else
  if FMenuDroppedDown then
    Result := scsHot
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn or (FActive and FDefault and FCanFocused) then
    Result := scsHot
  else
  if Focused and FCanFocused then
    Result := scsFocused
  else
    Result := scsNormal;
end;

procedure TscButton.CMFocusChanged(var Message: TCMFocusChanged);
var
  OldActive: Boolean;
begin
  if FCanFocused and FDefault then
  begin
    OldActive := FActive;
    with Message do
    if Sender is TscButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
    if OldActive <> FActive then
      RePaintControl;
  end;
  inherited;
end;

procedure TscButton.CMDialogKey(var Message: TCMDialogKey);
begin
  if not FCanFocused then
  begin
    inherited;
    Exit;
  end;
  with Message do
   if FActive and (CharCode = VK_RETURN) and Enabled
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
   if (CharCode = VK_ESCAPE) and FCancel and FCanFocused and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus
   then
     begin
       ButtonClick;
       Result := 1;
     end
   else
     inherited;
end;

procedure TscButton.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscButton.ButtonClick;
var
  Form: TCustomForm;
begin
  if FDisableClick then Exit;
  if FModalSetting then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := FModalResult;
  end;
  inherited;
end;

procedure TscButton.SetImageMargin(Value: Integer);
begin
  if (Value >= 0) and (FImageMargin <> Value) then
  begin
    FImageMargin := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetShowCaption(Value: Boolean);
begin
  if Value <> FShowCaption then
  begin
    FShowCaption := Value;
    if (FWidthWithCaption > 0) and FShowCaption then
      Width := FWidthWithCaption
    else
    if (FWidthWithoutCaption > 0) and not FShowCaption then
      Width := FWidthWithoutCaption
    else
      RePaintControl;
  end;
end;

procedure TscButton.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FTitleFont.Height := MulDiv(FTitleFont.Height, M, D);
  if FImageMargin > 0 then
    FImageMargin := MulDiv(FImageMargin, M, D);
  if FScaleMarginAndSpacing then
  begin
    if FMargin > 0 then FMargin := MulDiv(FMargin, M, D);
    if FSpacing > 0 then FSpacing := MulDiv(FSpacing, M, D);
  end;
  if FWidthWithCaption > 0 then
    FWidthWithCaption := MulDiv(FWidthWithCaption, M, D);
  if FWidthWithoutCaption > 0 then
    FWidthWithoutCaption := MulDiv(FWidthWithoutCaption, M, D);
end;

function TscButton.CanAnimateFocusedState: Boolean;
begin
  Result := FCanFocused;
end;

procedure TscButton.CheckGroupIndex;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Parent.ControlCount - 1 do
   if (Parent.Controls[I] is TscButton) and (GroupIndex = TscButton(Parent.Controls[I]).GroupIndex)
     and TscButton(Parent.Controls[I]).Down and (Parent.Controls[I] <> Self)  then
       TscButton(Parent.Controls[I]).Down := False;
end;

procedure TscButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscButton.SetCustomImageNormalIndex(Value: Integer);
begin
  if FCustomImageNormalIndex <> Value then
  begin
    FCustomImageNormalIndex := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetCustomImageHotIndex(Value: Integer);
begin
  if FCustomImageHotIndex <> Value then
  begin
    FCustomImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetCustomImagePressedIndex(Value: Integer);
begin
  if FCustomImagePressedIndex <> Value then
  begin
    FCustomImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetCustomImageDisabledIndex(Value: Integer);
begin
  if FCustomImageDisabledIndex <> Value then
  begin
    FCustomImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetCustomImageFocusedIndex(Value: Integer);
begin
  if FCustomImageFocusedIndex <> Value then
  begin
    FCustomImageFocusedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetStyleKind(Value: TscButtonStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    RePaintControl;
  end;
end;

procedure TscButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, TR, R1: TRect;
  TextColor, ArrowColor: TColor;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
  SaveIndex: Integer;
  FSplitFocusRect: Boolean;
  S: String;
  IIndex, CIIndex, FTitleOffset: Integer;
  IL: TCustomImageList;
  FMarginLeft, FMarginTop, FMarginRight, FMarginBottom: Integer;
  FUpdateGlow: Boolean;
  FState: TscsCtrlState;
  XMargin, XSpacing: Integer;
  FInternalLayout: TButtonLayout;
begin
  R := Rect(0, 0, Width, Height);
  FMarginLeft := 0;
  FMarginTop := 0;
  FMarginRight := 0;
  FMarginBottom := 0;
  if not TransparentBackground then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;

  FInternalLayout := FLayout;
  if BidiMode = bdRightToLeft then
    if FInternalLayout = blGlyphLeft then
      FInternalLayout := blGlyphRight
    else
    if FInternalLayout = blGlyphRight then
      FInternalLayout := blGlyphLeft;

  case FStyleKind of
   scbsDropDownButton:
     begin
       DrawDropDownButton(ACanvas, R, ACtrlState, False, False, FScaleFactor);
       Exit;
     end;
    scbsUpSpinButton:
    begin
      DrawUpSpinButton(ACanvas, R, ACtrlState, FScaleFactor);
      Exit;
    end;
    scbsLeftSpinButton:
    begin
      DrawLeftSpinButton(ACanvas, R, ACtrlState, FScaleFactor);
      Exit;
    end;
    scbsDownSpinButton:
    begin
      DrawDownSpinButton(ACanvas, R, ACtrlState, FScaleFactor);
      Exit;
    end;
    scbsRightSpinButton:
    begin
      DrawRightSpinButton(ACanvas, R, ACtrlState, FScaleFactor);
      Exit;
    end;
  end;

  SaveIndex := 0;

  if StyleServices.Enabled and FMenuDroppedDown and (StyleKind <> scbsTransparent) and
     (StyleKind <> scbsColorButton) and (StyleKind <> scbsLink) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    if FArrowPosition = scapRight then
    begin
      if BidiMode <> bdRightToLeft then
        IntersectClipRect(ACanvas.Handle, 0, 0,  Width - FSplitWidth, Height)
      else
        IntersectClipRect(ACanvas.Handle, FSplitWidth, 0,  Width, Height);
    end
    else
      IntersectClipRect(ACanvas.Handle, 0, 0, Width, Height - FSplitWidth);
  end;

  FSplitFocusRect := ((FDropDownMenu <> nil) or (FGalleryMenu <> nil) or FCustomDropDown) and SplitButton;

  case FStyleKind of
    scbsCustomImage, scbsCustomImageOverContent:
    begin
      CIIndex := FCustomImageNormalIndex;
      case ACtrlState of
        scsHot: CIIndex := FCustomImageHotIndex;
        scsPressed: CIIndex := FCustomImagePressedIndex;
        scsFocused: CIIndex := FCustomImageFocusedIndex;
        scsDisabled: CIIndex := FCustomImageDisabledIndex;
      end;
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIIndex)
      then
      begin
        if FStyleKind = scbsCustomImage then
          FCustomImages.Draw(ACanvas, R, CIIndex, FScaleFactor);
        R1 := FCustomImages.GetContentMargins(CIIndex, FScaleFactor);
        FMarginLeft := R1.Left;
        FMarginTop := R1.Top;
        FMarginRight := R1.Right;
        FMarginBottom := R1.Bottom;
      end;
      FColorOptions.State := ACtrlState;
      TextColor := FColorOptions.FontColor;
    end;
    scbsColorButton:
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
         ACanvas.Pen.Color := FColorOptions.GetFrameColor;
         Frame3D(ACanvas, R1, FColorOptions.GetFrameColor, FColorOptions.GetFrameColor,
           FColorOptions.FrameWidth);
       end;
       TextColor := FColorOptions.FontColor;
    end;
    scbsTabTransparent:
    begin
      if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
      begin
        DrawSimpleTab(ACanvas, R, ACtrlState);
        TextColor := scDrawUtils.GetSimpleTabTextColor(ACtrlState);
      end
      else
      begin
        if FUseFontColorToStyleColor and Enabled then
          TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          TextColor := GetCheckBoxTextColor(ACtrlState);
      end;
    end;
    scbsTab:
    begin
      DrawSimpleTab(ACanvas, R, ACtrlState);
      TextColor := scDrawUtils.GetSimpleTabTextColor(ACtrlState);
    end;
    scbsHeaderSection:
    begin
      DrawHeaderSection(ACanvas, R, ACtrlState);
      TextColor := scDrawUtils.GetHeaderTextColor(ACtrlState);
    end;
    scbsSegmentedLeft:
    begin
      DrawSegmentedButton(ACanvas, R, ACtrlState,
         FCanFocused and Focused and (FShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, 1,
        FScaleFactor);
      TextColor := GetButtonTextColor(ACtrlState);
    end;
    scbsSegmentedMiddle:
    begin
      DrawSegmentedButton(ACanvas, R, ACtrlState,
         FCanFocused and Focused and (FShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, 2,
        FScaleFactor);
      TextColor := GetButtonTextColor(ACtrlState);
    end;
    scbsSegmentedRight:
    begin
      DrawSegmentedButton(ACanvas, R, ACtrlState,
         FCanFocused and Focused and (FShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, 3,
        FScaleFactor);
      TextColor := GetButtonTextColor(ACtrlState);
    end;
    scbsSegmentedToolLeft:
    begin
      DrawSegmentedToolButton(ACanvas, R, ACtrlState, 1, FScaleFactor);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scbsSegmentedToolMiddle:
    begin
      DrawSegmentedToolButton(ACanvas, R, ACtrlState, 2, FScaleFactor);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scbsSegmentedToolRight:
    begin
      DrawSegmentedToolButton(ACanvas, R, ACtrlState, 3, FScaleFactor);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scbsDropDownCombo:
    begin
      if ACtrlState = scsFocused then
        ACtrlState := scsHot;
      DrawButton(ACanvas, R, ACtrlState,
        False, FScaleFactor);
        TextColor := GetButtonTextColor(ACtrlState);
    end;
    scbsPushButton:
    begin
      if FToolPushButtonStyle and IsCustomStyle then
      begin
        DrawToolButton(ACanvas, R, ACtrlState);
        TextColor := GetToolButtonTextColor(ACtrlState);
      end
      else
      begin
        DrawButton(ACanvas, R, ACtrlState,
          FCanFocused and Focused and (FShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, FScaleFactor);
        TextColor := GetButtonTextColor(ACtrlState);
      end;
    end;
    scbsToolButton:
    begin
      DrawToolButton(ACanvas, R, ACtrlState);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scbsPushButtonTransparent:
    begin
      if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
      begin
        DrawButton(ACanvas, R, ACtrlState,
          FCanFocused and Focused and (FShowFocusRect or not StyleServices.Enabled) and not FSplitFocusRect, FScaleFactor);
        TextColor := GetButtonTextColor(ACtrlState);
      end
      else
      begin
        if FUseFontColorToStyleColor and (ACtrlState <> scsDisabled) then
          TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          TextColor := GetCheckBoxTextColor(ACtrlState);
      end;
    end;
    scbsToolButtonTransparent:
    begin
      if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
      begin
        DrawToolButton2(ACanvas, R, ACtrlState);
        TextColor := GetToolButtonTextColor(ACtrlState);
      end
      else
      begin
        if FUseFontColorToStyleColor and (ACtrlState <> scsDisabled) then
          TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          TextColor := GetCheckBoxTextColor(ACtrlState);
      end;
    end;
    scbsTransparent, scbsLink:
    begin
      if FUseFontColorToStyleColor and (ACtrlState <> scsDisabled) then
        TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
      else
        TextColor := GetCheckBoxTextColor(ACtrlState);
      end;
    else
    begin
      if FUseFontColorToStyleColor and (ACtrlState <> scsDisabled) then
        TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
      else
        TextColor := GetCheckBoxTextColor(ACtrlState);
    end;
  end;

  if SaveIndex <> 0 then
  begin
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;

  ACanvas.Brush.Style := bsClear;
  if FTitle <> '' then
  begin
    ACanvas.Font.Assign(FTitleFont);
    if StyleKind = scbsColorButton then
      ACanvas.Font.Color := FColorOptions.TitleFontColor
    else
    if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
      ACanvas.Font.Color := TextColor;

   if FMarginLeft > 0 then
     FTitleOffset := FMarginLeft
   else
     FTitleOffset := 5;

   if FScaleMarginAndSpacing then
     FTitleOffset := Round(FTitleOffset * FScaleFactor);

   if (FDropDownMenu <> nil) or (FGalleryMenu <> nil) or FCustomDropDown then
   begin
     if FArrowPosition = scapRight then
     begin
       if BidiMode <> bdRightToLeft then
         TR := Rect(FTitleOffset, 5, Width - FSplitWidth - 1, 5 + ACanvas.TextHeight('Wq'))
       else
         TR := Rect(FSplitWidth + 1, 5, Width - FTitleOffset, 5 + ACanvas.TextHeight('Wq'))
     end
     else
       TR := Rect(FTitleOffset, 5, Width - 5, 5 + ACanvas.TextHeight('Wq'));
   end
   else
     TR := Rect(FTitleOffset, 5, Width - 5, 5 + ACanvas.TextHeight('Wq'));

    if FScaleFactor > 1.25 then
    begin
      Inc(TR.Left, 2);
      Dec(TR.Right, 2);
    end;
    scDrawText(ACanvas, FTitle, TR, IsRightToLeft, False);
    Inc(R.Top, TR.Height);
  end;

  ACanvas.Font.Assign(Font);

  if StyleKind = scbsColorButton then
    ACanvas.Font.Color := FColorOptions.FontColor
  else
  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
     ACanvas.Font.Color := TextColor
  else
  if not Enabled then
    ACanvas.Font.Color := clGrayText;

  ArrowColor := ACanvas.Font.Color;
  if (FStyleKind = scbsLink) and (ACtrlState in [scsHot, scsPressed, scsFocused]) then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderLine];


  case FStyleKind of
    scbsDropDownCombo:
    begin
      if FCanFocused and Focused and FShowFocusRect then
        if BidiMode <> bdRightToLeft then
          scDrawFocusRect(ACanvas, Rect(4, 4, Width - FSplitWidth - 1, Height - 4), FScaleFactor)
        else
          scDrawFocusRect(ACanvas, Rect(FSplitWidth + 1, 4, Width - 4, Height - 4), FScaleFactor)
    end;
    scbsPushButton,
    scbsSegmentedLeft, scbsSegmentedRight, scbsSegmentedMiddle:
    begin
      if FCanFocused and Focused and FShowFocusRect and not IsCustomStyle and FSplitFocusRect then
      begin
        if FArrowPosition = scapRight then
        begin
          if BidiMode <> bdRightToLeft then
            scDrawFocusRect(ACanvas, Rect(3, 3, Width - FSplitWidth - 1, Height - 3), FScaleFactor)
          else
            scDrawFocusRect(ACanvas, Rect(FSplitWidth + 1, 3, Width - 3, Height - 3), FScaleFactor)
        end
        else
          scDrawFocusRect(ACanvas, Rect(3, 3, Width - 3, Height - FSplitWidth - 1), FScaleFactor)
      end;
    end;
    scbsPushButtonTransparent:
    begin
      if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
      begin
        if FCanFocused and Focused and FShowFocusRect and not IsCustomStyle and FSplitFocusRect then
        begin
          if FArrowPosition = scapRight then
          begin
            if BidiMode <> bdRightToLeft then
              scDrawFocusRect(ACanvas, Rect(3, 3, Width - FSplitWidth - 1, Height - 3), FScaleFactor)
            else
              scDrawFocusRect(ACanvas, Rect(FSplitWidth + 1, 3, Width - 3, Height - 3), FScaleFactor)
          end
          else
            scDrawFocusRect(ACanvas, Rect(3, 3, Width - 3, Height - FSplitWidth - 1), FScaleFactor);
        end;
      end;
    end;
    scbsTransparent, scbsLink:
    begin
      if FCanFocused and Focused and FShowFocusRect then
      begin
        if FSplitFocusRect then
        begin
          if FArrowPosition = scapRight then
          begin
            if BidiMode <> bdRightToLeft then
              scDrawFocusRect(ACanvas, Rect(0, 0, Width - FSplitWidth - 1, Height), FScaleFactor)
            else
              scDrawFocusRect(ACanvas, Rect(FSplitWidth + 1, 0, Width, Height), FScaleFactor)
          end
          else
            scDrawFocusRect(ACanvas, Rect(0, 0, Width, Height - FSplitWidth - 1), FScaleFactor);
        end
        else
          scDrawFocusRect(ACanvas, Rect(0, 0, Width, Height), FScaleFactor);
      end;
    end;
  end;

  if (FDropDownMenu <> nil) or (FGalleryMenu <> nil) or FCustomDropDown then
  begin
    if FSplitButton then
    begin
     if FArrowPosition = scapRight then
     begin
       if BidiMode <> bdRightToLeft then
         Dec(R.Right, FSplitWidth - 2)
       else
         Inc(R.Left, FSplitWidth + 2);
     end
     else
       Dec(R.Bottom, FSplitWidth - 2);
    end
    else
    if FShowMenuArrow then
    begin
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          Dec(R.Right, FSplitWidth - 5)
        else
          Inc(R.Left, FSplitWidth + 5);
      end
      else
        Dec(R.Bottom, FSplitWidth - 5);
    end;
    if FMenuDroppedDown and (StyleKind <> scbsTransparent) and (StyleKind <> scbsLink) then
    begin
      if StyleServices.Enabled and (FStyleKind <> scbsColorButton) then
      begin
        SaveIndex := SaveDC(ACanvas.Handle);
        R1 := Rect(0, 0, Width, Height);
      end
      else
      begin
        if FArrowPosition = scapRight then
        begin
          if BidiMode <> bdRightToLeft then
            R1 := Rect(Width - FSplitWidth, 0, Width, Height)
          else
            R1 := Rect(0, 0, FSplitWidth, Height)
        end
        else
          R1 := Rect(0, Height - FSplitWidth, Width, Height);
      end;
      try
        if StyleServices.Enabled and (FStyleKind <> scbsColorButton) then
        begin
          if FArrowPosition = scapRight then
          begin
            if BidiMode <> bdRightToLeft then
              IntersectClipRect(ACanvas.Handle, Width - FSplitWidth, 0, Width, Height)
            else
              IntersectClipRect(ACanvas.Handle, 0, 0, FSplitWidth, Height);
          end
          else
            IntersectClipRect(ACanvas.Handle, 0, Height - FSplitWidth, Width, Height);
        end;
         case FStyleKind of
           scbsCustomImage:
           begin
             FColorOptions.State := scsPressed;
             CIIndex := FCustomImagePressedIndex;
             if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIIndex)
             then
               FCustomImages.Draw(ACanvas, R1, CIIndex, FScaleFactor);
             ArrowColor := FColorOptions.FontColor;
           end;
           scbsColorButton:
           begin
             FColorOptions.State := scsPressed;
             if FColorOptions.Color <> clNone then
             begin
               ACanvas.Brush.Color := FColorOptions.Color;
               ACanvas.FillRect(R1);
             end;
             if (FColorOptions.FrameColor <> clNone) and
                (FColorOptions.FrameWidth > 0) then
             begin
               ACanvas.Pen.Color := FColorOptions.FrameColor;
               Frame3D(ACanvas, R1, FColorOptions.FrameColor, FColorOptions.FrameColor,
                 FColorOptions.FrameWidth);
             end;
             ArrowColor := FColorOptions.FontColor;
           end;
           scbsTab, scbsTabTransparent:
           begin
             DrawSimpleTab(ACanvas, R1, scsPressed);
             ArrowColor := GetSimpleTabTextColor(ACtrlState);
           end;
           scbsHeaderSection:
           begin
             DrawHeaderSection(ACanvas, R1, scsPressed);
             ArrowColor := GetHeaderTextColor(ACtrlState);
           end;
           scbsPushButton, scbsPushButtonTransparent, scbsDropDownCombo:
             begin
               DrawButton(ACanvas, R1, scsPressed, False);
               ArrowColor := GetButtonTextColor(scsPressed);
             end;
           scbsToolButton, scbsToolButtonTransparent:
             begin
               if not StyleServices.Enabled then
                 InflateRect(R1, -1, -1);
               DrawToolButton(ACanvas, R1, scsPressed);
               ArrowColor := GetToolButtonTextColor(scsPressed);
             end;
           scbsSegmentedLeft:
           begin
             DrawSegmentedButton(ACanvas, R1, scsPressed,
             False, 1,FScaleFactor);
             ArrowColor := GetButtonTextColor(scsPressed);
           end;
           scbsSegmentedMiddle:
           begin
             DrawSegmentedButton(ACanvas, R1, scsPressed,
             False, 2,FScaleFactor);
             ArrowColor := GetButtonTextColor(scsPressed);
           end;
           scbsSegmentedRight:
           begin
             DrawSegmentedButton(ACanvas, R1, scsPressed,
             False, 3,FScaleFactor);
             ArrowColor := GetButtonTextColor(scsPressed);
           end;
           scbsSegmentedToolLeft:
           begin
             DrawSegmentedToolButton(ACanvas, R1, scsPressed, 1, FScaleFactor);
             ArrowColor := GetToolButtonTextColor(scsPressed);
           end;
           scbsSegmentedToolMiddle:
           begin
             DrawSegmentedToolButton(ACanvas, R1, scsPressed, 2, FScaleFactor);
             ArrowColor := GetToolButtonTextColor(scsPressed);
           end;
           scbsSegmentedToolRight:
           begin
             DrawSegmentedToolButton(ACanvas, R1, scsPressed, 3, FScaleFactor);
             ArrowColor := GetToolButtonTextColor(scsPressed);
           end;
         end;
      finally
        if StyleServices.Enabled and (FStyleKind <> scbsColorButton) then
          RestoreDC(ACanvas.Handle, SaveIndex);
      end;
    end
    else
      ArrowColor := ACanvas.Font.Color;
    if FArrowPosition = scapRight then
    begin
      if BidiMode <> bdRightToLeft then
        TR := Rect(Width - FSplitWidth, 2, Width, Height - 2)
      else
        TR := Rect(0, 2, FSplitWidth, Height - 2);
    end
    else
      TR := Rect(2, Height - FSplitWidth, Width - 2, Height);

    if (FMarginRight > 0) and (FArrowPosition = scapRight) then
    begin
      if BidiMode <> bdRightToLeft then
        Dec(TR.Right, FMarginRight div 2)
      else
        Inc(TR.Left, FMarginRight div 2);
    end;

    if (FMarginBottom > 0) and (FArrowPosition = scapBottom) then
      Dec(TR.Bottom, FMarginBottom div 2);

    if FShowMenuArrow or FSplitButton then
    begin
      if FStyleKind = scbsDropDownCombo then
      begin
        Dec(TR.Right, 2);
        if IsCustomStyle or not StyleServices.Enabled or IsWindowsXP then
          DrawComboArrowImage(ACanvas, TR, ArrowColor, FScaleFactor)
        else
        begin
          if Enabled then
            FState := scsNormal
         else
            FState := scsDisabled;
          DrawDropDownButton(ACanvas, TR, FState, True, False, FScaleFactor);
        end;
      end
      else
      begin
        if SC_MODERNARROWS and IsCustomStyle then
        begin
          if FArrowDirection = scadRight then
            DrawModernMenuArrowImage(ACanvas, TR, ArrowColor, 1, FScaleFactor)
          else
            DrawModernArrowImage(ACanvas, TR, ArrowColor, FScaleFactor);
        end
        else
        begin
          if FArrowDirection = scadRight then
            DrawArrowImage(ACanvas, TR, ArrowColor, 2, FScaleFactor)
          else
            DrawArrowImage(ACanvas, TR, ArrowColor, 4, FScaleFactor);
        end;
      end;
    end;

    if FSplitButton and (StyleKind = scbsColorButton) then
    begin
      FColorOptions.State := ACtrlState;
      with ACanvas do
      begin
        if FColorOptions.FrameColor <> clNone then
          Pen.Color := FColorOptions.FrameColor
        else
          Pen.Color := FColorOptions.PressedColor;
        if FArrowPosition = scapRight then
        begin
          if BidiMode <> bdRightToLeft then
          begin
            MoveTo(TR.Left, TR.Top);
            LineTo(TR.Left, TR.Bottom);
          end
          else
          begin
            MoveTo(TR.Right, TR.Top);
            LineTo(TR.Right, TR.Bottom);
          end;
        end
        else
        begin
          MoveTo(TR.Left, TR.Top);
          LineTo(TR.Right, TR.Top);
        end;
      end;
    end
    else
    if FSplitButton and ((StyleKind = scbsPushButton) or
     ((ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled))) then
    begin
      if FArrowPosition = scapRight then
        DrawVertSplitter(ACanvas, TR, BidiMode = bdRightToLeft)
      else
        DrawHorzSplitter(ACanvas, TR);
    end;
  end;

  if (FMarginLeft > 0) or (FMarginTop > 0) or (FMarginRight > 0) or (FMarginBottom > 0)
  then
  begin
    Inc(R.Left, FMarginLeft);
    Inc(R.Top, FMarginTop);
    if not ((FDropDownMenu <> nil) or (FGalleryMenu <> nil)) then
    begin
      Dec(R.Right, FMarginRight);
      Dec(R.Bottom, FMarginBottom);
    end
    else
    begin
      if FArrowPosition = scapRight then
      begin
        if BidiMode <> bdRightToLeft then
          Dec(R.Right, FMarginRight div 2)
        else
          Inc(R.Left, FMarginRight div 2);
        Dec(R.Bottom, FMarginBottom);
      end
      else
      begin
        Dec(R.Bottom, FMarginBottom div 2);
        Dec(R.Right, FMarginRight);
      end;
    end;
  end
  else
    InflateRect(R, -3, -3);

  if FScaleFactor > 1.25 then
  begin
    InflateRect(R, -2, -2);
  end;

  if FGlowEffect.Enabled then
    InflateRect(R, -FGlowEffect.GlowSize div 2, -FGlowEffect.GlowSize div 2);

  S := Caption;
  IIndex := GetCurrentImageIndex(ACtrlState);
  IL := FImages;

  if (ACtrlState <> scsPressed) and (ACtrlState <> scsFocused) and (FStyleKind = scbsTab)
     and not StyleServices.Enabled
  then
    Inc(R.Top, 2);

  if (FGalleryMenu <> nil) and FUseGalleryMenuCaption and (FGalleryMenu.ItemIndex >= 0) and
     (FGalleryMenu.ItemIndex < FGalleryMenu.Items.Count)
  then
    S := FGalleryMenu.Items[FGalleryMenu.ItemIndex].Caption;

  if (FGalleryMenu <> nil) and FUseGalleryMenuImage and (FGalleryMenu.ItemIndex >= 0) and
     (FGalleryMenu.ItemIndex < FGalleryMenu.Items.Count) and (FGalleryMenu.Images <> nil)
  then
    begin
      IL := FGalleryMenu.Images;
      IIndex := FGalleryMenu.Items[FGalleryMenu.ItemIndex].ImageIndex;
    end;

  if not FShowCaption then S := '';  

  XMargin := FMargin;
  XSpacing := FSpacing;
  if (FImageMargin > 0) and (IL <> nil) then
  begin
    XMargin := FImageMargin div 2 - IL.Width div 2 - 3;
    if FGlowEffect.Enabled then
      XMargin := XMargin - FGlowEffect.GlowSize div 2;
    XSpacing := FImageMargin - XMargin - IL.Width + FSpacing;
    if FGlowEffect.Enabled then
      XSpacing := XSpacing - FGlowEffect.GlowSize div 2;
  end;

  if Assigned(FOnPaintContent) then
     FOnPaintContent(ACanvas, R, ACtrlState)
  else
  begin
    if FGlowEffect.Enabled and GetGlowParams(ACtrlState, FGlowColor, FGlowSize, FGlowAlpha) then
    begin
      if (FLayout = blGlyphLeft) or (FLayout = blGlyphRight) then
      begin
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, IIndex, IL);
        DrawImageAndTextWithGlowBuffer2(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
          ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack,
          FGlowEffect.Offset, FGlowColor, FGlowSize, FGlowEffect.Intensive, FGlowAlpha,
          FImageGlow, False, IsRightToLeft, False, FScaleFactor, FWordWrap)
      end
      else
      begin
        FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, IIndex, IL);
        DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
          ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack,
          FGlowEffect.Offset, FGlowColor, FGlowSize, FGlowEffect.Intensive, FGlowAlpha,
          FImageGlow, False, IsRightToLeft, False, FScaleFactor, FWordWrap);
      end;
    end
    else
      if (FLayout = blGlyphLeft) or (FLayout = blGlyphRight) then
        DrawImageAndText2(ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack, False,
          IsRightToLeft, False, FScaleFactor, FWordWrap)
      else
        DrawImageAndText(ACanvas, R, XMargin, XSpacing,
          FInternalLayout, S, IIndex, IL,
          ACtrlState <> scsDisabled, False, clBlack, False,
          IsRightToLeft, False, FScaleFactor, FWordWrap);
  end;

  case FStyleKind of
    scbsCustomImageOverContent:
    begin
      CIIndex := FCustomImageNormalIndex;
      R := Rect(0, 0, Width, Height);
      case ACtrlState of
        scsHot: CIIndex := FCustomImageHotIndex;
        scsPressed: CIIndex := FCustomImagePressedIndex;
        scsFocused: CIIndex := FCustomImageFocusedIndex;
        scsDisabled: CIIndex := FCustomImageDisabledIndex;
      end;
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(CIIndex)
      then
        FCustomImages.Draw(ACanvas, R, CIIndex, FScaleFactor);
    end;
  end;
end;

procedure TscButton.SetUseGalleryMenuImage(Value: Boolean);
begin
  if FUseGalleryMenuImage <> Value then
  begin
    FUseGalleryMenuImage := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetUseGalleryMenuCaption(Value: Boolean);
begin
  if FUseGalleryMenuCaption <> Value then
  begin
    FUseGalleryMenuCaption := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetTitle(Value: String);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    RePaintControl;
  end;
end;

procedure TscButton.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
  if FTitle <> '' then RePaintControl;
end;

procedure TscButton.OnColorOptionsChange(Sender: TObject);
begin
  if StyleKind = scbsColorButton then
    RePaintControl;
end;

procedure TscButton.OnTitleFontChange(Sender: TObject);
begin
  RePaintControl;
end;

function TscButton.GetCurrentImageIndex(ACtrlState: TscsCtrlState): Integer;
begin
  Result := FImageIndex;
  case ACtrlState of
    scsHot: if FHotImageIndex <> -1 then Result := FHotImageIndex;
    scsPressed: if FPressedImageIndex <> -1 then
      Result := FPressedImageIndex else Result := FHotImageIndex;
    scsFocused: if FFocusedImageIndex <> -1 then Result := FFocusedImageIndex
      else Result := FHotImageIndex;
  end;
  if Result = -1 then Result := FImageIndex;
end;


constructor TscCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FUseFontColorToStyleColor := False;
  FCustomImages := nil;
  FCustomCheckedImageIndex := -1;
  FCustomCheckedImageHotIndex := -1;
  FCustomCheckedImagePressedIndex := -1;
  FCustomCheckedImageDisabledIndex := -1;
  FCustomUnCheckedImageIndex := -1;
  FCustomUnCheckedImageHotIndex := -1;
  FCustomUnCheckedImagePressedIndex := -1;
  FCustomUnCheckedImageDisabledIndex := -1;
  FCustomGrayedImageIndex := -1;
  FCustomGrayedImageHotIndex := -1;
  FCustomGrayedImagePressedIndex := -1;
  FCustomGrayedImageDisabledIndex := -1;
end;

destructor TscCheckBox.Destroy;
begin
  inherited;
end;

procedure TscCheckBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FSpacing := MulDiv(FSpacing, M, D);
  if FMargin > 0 then
    FMargin := MulDiv(FMargin, M, D);
end;

procedure TscCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

function TscCheckBox.GetCtrlState: TscsCtrlState;
begin
  if not Enabled then
    Result := scsDisabled
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn then
    Result := scsHot
  else
    Result := scsNormal;
end;

function TscCheckBox.GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean;
begin
  if (ACtrlState = scsNormal) and (CanFocused and Focused) then ACtrlState := scsFocused;
  Result := inherited GetGlowParams(ACtrlState, Acolor, ASize, AAlphaValue);
end;

function TscCheckBox.GetCustomCheckBoxSize: TPoint;
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

procedure TscCheckBox.DrawCustomCheckBox(ACanvas: TCanvas;
  ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
var
  CIndex: Integer;
  A: Byte;
begin
  CIndex := -1;
  case ACtrlState of
    scsNormal:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageIndex;
        cbChecked: CIndex := FCustomCheckedImageIndex;
        cbGrayed: CIndex := FCustomGrayedImageIndex;
      end;
    end;
    scsHot:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageHotIndex;
        cbChecked: CIndex := FCustomCheckedImageHotIndex;
        cbGrayed: CIndex := FCustomGrayedImageHotIndex;
      end;
    end;
    scsPressed:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImagePressedIndex;
        cbChecked: CIndex := FCustomCheckedImagePressedIndex;
        cbGrayed: CIndex := FCustomGrayedImagePressedIndex;
      end;
    end;
    scsDisabled:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageDisabledIndex;
        cbChecked: CIndex := FCustomCheckedImageDisabledIndex;
        cbGrayed: CIndex := FCustomGrayedImageDisabledIndex;
      end;
    end;
  end;
  A := 255;
  if CIndex = -1 then
  begin
    case ACheckBoxState of
      cbUnChecked: CIndex := FCustomUnCheckedImageIndex;
      cbChecked: CIndex := FCustomCheckedImageIndex;
      cbGrayed: CIndex := FCustomGrayedImageIndex;
    end;
    if ACtrlState = scsDisabled then
      A := 120;
  end;
  if (FCustomImages <> nil) and (FCustomImages.IsIndexAvailable(CIndex))
  then
  begin
    if (FCustomImages is TscImageCollection) and not (TscImageCollection(FCustomImages).Images[CIndex].Bitmap.Empty) then
      TscImageCollection(FCustomImages).DrawBitmap(ACanvas, ARect, CIndex, A)
    else
      FCustomImages.Draw(ACanvas, ARect, CIndex, FScaleFactor);
  end;
end;

procedure TscCheckBox.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, TR: TRect;
  TextColor: TColor;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
  S: TPoint;
  FUpdateGlow: Boolean;
begin
  R := Rect(0, 0, Width, Height);
  if (ACtrlState <> scsDisabled) and FUseFontColorToStyleColor
  then
    TextColor := GetStyleColor(Self.Font.Color)
  else
    TextColor := GetCheckBoxTextColor(ACtrlState);
  ACanvas.Font.Assign(Font);
  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
    ACanvas.Font.Color := TextColor;

  TR := R;
  S := GetCustomCheckBoxSize;
  if S.X = 0 then
  begin
    S.X := GetCheckBoxSize(ACanvas, FScaleFactor) + 5;
    if BiDiMode = bdRightToLeft then
    begin
      TR.Left := TR.Right - S.X;
      Dec(R.Right, S.X);
    end
    else
    begin
      TR.Right := TR.Left + S.X;
      Inc(R.Left, S.X);
    end;
    Inc(TR.Top, 1);
    DrawCheckBox(ACanvas, TR, ACtrlState, FState, FScaleFactor);
  end
  else
  begin
    if BiDiMode = bdRightToLeft then
    begin
      TR.Left := TR.Right - S.X - 5;
      Dec(R.Right, S.X + 7);
    end
    else
    begin
      TR.Right := TR.Left + S.X + 5;
      Inc(R.Left, S.X + 7);
    end;
    DrawCustomCheckBox(ACanvas, TR, ACtrlState, FState);
  end;
  
  ACanvas.Brush.Style := bsClear;

  if GlowEffect.Enabled then
    InflateRect(R, -GlowEffect.GlowSize div 2, -GlowEffect.GlowSize div 2)
  else
  if ShowFocusRect then
  begin
    if (Layout = blGlyphLeft) and IsRightToLeft then
      OffsetRect(R, 2, 0)
    else
    if (Layout = blGlyphRight) and not IsRightToLeft then
      OffsetRect(R, -2, 0);
  end;

  FMargin := 0;
  if (FLayout = blGlyphTop) or (Layout = blGlyphBottom) then
    FMargin := -1;

  if FGlowEffect.Enabled and GetGlowParams(ACtrlState, FGlowColor, FGlowSize, FGlowAlpha) then
  begin
    if (FLayout = blGlyphLeft) or (FLayout = blGlyphRight) then
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, FImageIndex, FImages);
      DrawImageAndTextWithGlowBuffer2(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
       ACanvas, R, FMargin, FSpacing,
       FLayout, Caption, FImageIndex, FImages,
        ACtrlState <> scsDisabled, False, clBlack,
        FGlowEffect.Offset, FGlowColor, FGlowSize, FGlowEffect.Intensive, FGlowAlpha, FImageGlow,
        FCanFocused and Focused and FShowFocusRect, IsRightToLeft, False, FScaleFactor, FWordWrap)
    end
    else
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, FImageIndex, FImages);
      DrawImageAndTextWithGlowBuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
        ACanvas, R, FMargin, FSpacing,
        FLayout, Caption, FImageIndex, FImages,
        ACtrlState <> scsDisabled, False, clBlack,
        FGlowEffect.Offset, FGlowColor, FGlowSize, FGlowEffect.Intensive, FGlowAlpha, FImageGlow,
        FCanFocused and Focused and FShowFocusRect, IsRightToLeft, False, FScaleFactor, FWordWrap);
    end;
  end
  else
    if (FLayout = blGlyphLeft) or (FLayout = blGlyphRight) then
      DrawImageAndText2(ACanvas, R, FMargin, FSpacing,
       FLayout, Caption, FImageIndex, FImages,
        ACtrlState <> scsDisabled, False, clBlack, FCanFocused and Focused and FShowFocusRect, IsRightToLeft, False,
        FScaleFactor, FWordWrap)
    else
      DrawImageAndText(ACanvas, R, FMargin, FSpacing,
       FLayout, Caption, FImageIndex, FImages,
       ACtrlState <> scsDisabled, False, clBlack, FCanFocused and Focused and FShowFocusRect, IsRightToLeft, False,
       FScaleFactor, FWordWrap);
end;

function TscCheckBox.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

procedure TscCheckBox.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomCheckedImageIndex(Value: Integer);
begin
  if FCustomCheckedImageIndex <> Value then
  begin
    FCustomCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomCheckedImageHotIndex(Value: Integer);
begin
  if FCustomCheckedImageHotIndex <> Value then
  begin
    FCustomCheckedImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomCheckedImagePressedIndex(Value: Integer);
begin
  if FCustomCheckedImagePressedIndex <> Value then
  begin
    FCustomCheckedImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomCheckedImageDisabledIndex(Value: Integer);
begin
  if FCustomCheckedImageDisabledIndex <> Value then
  begin
    FCustomCheckedImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomUnCheckedImageIndex(Value: Integer);
begin
  if FCustomUnCheckedImageIndex <> Value then
  begin
    FCustomUnCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomUnCheckedImageHotIndex(Value: Integer);
begin
  if FCustomUnCheckedImageHotIndex <> Value then
  begin
    FCustomUnCheckedImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomUnCheckedImagePressedIndex(Value: Integer);
begin
  if FCustomUnCheckedImagePressedIndex <> Value then
  begin
    FCustomUnCheckedImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomUnCheckedImageDisabledIndex(Value: Integer);
begin
  if FCustomUnCheckedImageDisabledIndex <> Value then
  begin
    FCustomUnCheckedImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomGrayedImageIndex(Value: Integer);
begin
  if FCustomGrayedImageIndex <> Value then
  begin
    FCustomGrayedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomGrayedImageHotIndex(Value: Integer);
begin
  if FCustomGrayedImageHotIndex <> Value then
  begin
    FCustomGrayedImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomGrayedImagePressedIndex(Value: Integer);
begin
  if FCustomGrayedImagePressedIndex <> Value then
  begin
    FCustomGrayedImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetCustomGrayedImageDisabledIndex(Value: Integer);
begin
  if FCustomGrayedImageDisabledIndex <> Value then
  begin
    FCustomGrayedImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCheckBox.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;

procedure TscCheckBox.SetState(Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    RePaintControl;
    if not FClickDisabled then
    begin
      FClickDisabled := True;
      ButtonClick;
      FClickDisabled := False;
    end;
  end;
end;

procedure TscCheckBox.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscCheckBox.ButtonClick;
begin
  if not FClickDisabled then
  begin
    FClickDisabled := True;
    case FState of
      cbUnChecked: if FAllowGrayed then State := cbGrayed else State := cbChecked;
      cbGrayed: State := cbChecked;
      cbChecked: State := cbUnchecked;
    end;
    FClickDisabled := False;
  end;
  inherited;
end;

constructor TscRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FUseFontColorToStyleColor := False;
  FCustomImages := nil;
  FCustomCheckedImageIndex := -1;
  FCustomCheckedImageHotIndex := -1;
  FCustomCheckedImagePressedIndex := -1;
  FCustomCheckedImageDisabledIndex := -1;
  FCustomUnCheckedImageIndex := -1;
  FCustomUnCheckedImageHotIndex := -1;
  FCustomUnCheckedImagePressedIndex := -1;
  FCustomUnCheckedImageDisabledIndex := -1;
end;

procedure TscRadioButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscRadioButton.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FSpacing := MulDiv(FSpacing, M, D);
  if FMargin > 0 then
    FMargin := MulDiv(FMargin, M, D);
end;

procedure TscRadioButton.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomCheckedImageIndex(Value: Integer);
begin
  if FCustomCheckedImageIndex <> Value then
  begin
    FCustomCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomCheckedImageHotIndex(Value: Integer);
begin
  if FCustomCheckedImageHotIndex <> Value then
  begin
    FCustomCheckedImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomCheckedImagePressedIndex(Value: Integer);
begin
  if FCustomCheckedImagePressedIndex <> Value then
  begin
    FCustomCheckedImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomCheckedImageDisabledIndex(Value: Integer);
begin
  if FCustomCheckedImageDisabledIndex <> Value then
  begin
    FCustomCheckedImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomUnCheckedImageIndex(Value: Integer);
begin
  if FCustomUnCheckedImageIndex <> Value then
  begin
    FCustomUnCheckedImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomUnCheckedImageHotIndex(Value: Integer);
begin
  if FCustomUnCheckedImageHotIndex <> Value then
  begin
    FCustomUnCheckedImageHotIndex := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomUnCheckedImagePressedIndex(Value: Integer);
begin
  if FCustomUnCheckedImagePressedIndex <> Value then
  begin
    FCustomUnCheckedImagePressedIndex := Value;
    RePaintControl;
  end;
end;

procedure TscRadioButton.SetCustomUnCheckedImageDisabledIndex(Value: Integer);
begin
  if FCustomUnCheckedImageDisabledIndex <> Value then
  begin
    FCustomUnCheckedImageDisabledIndex := Value;
    RePaintControl;
  end;
end;

function TscRadioButton.GetCtrlState: TscsCtrlState;
begin
  if not Enabled then
    Result := scsDisabled
  else
  if (FMouseIn and FPressed) or FIsDown then
    Result := scsPressed
  else
  if FMouseIn then
    Result := scsHot
  else
    Result := scsNormal;
end;

function TscRadioButton.GetGlowParams(ACtrlState: TscsCtrlState;
        var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean;
begin
  if (ACtrlState = scsNormal) and (CanFocused and Focused) then ACtrlState := scsFocused;
  Result := inherited GetGlowParams(ACtrlState, Acolor, ASize, AAlphaValue);
end;

function TscRadioButton.GetCustomCheckBoxSize: TPoint;
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

procedure TscRadioButton.DrawCustomCheckBox(ACanvas: TCanvas;
  ARect: TRect; ACtrlState: TscsCtrlState; ACheckBoxState: TCheckBoxState);
var
  CIndex: Integer;
  A: Byte;
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
    scsHot:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageHotIndex;
        cbChecked: CIndex := FCustomCheckedImageHotIndex;
      end;
    end;
    scsPressed:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImagePressedIndex;
        cbChecked: CIndex := FCustomCheckedImagePressedIndex;
      end;
    end;
    scsDisabled:
    begin
      case ACheckBoxState of
        cbUnChecked: CIndex := FCustomUnCheckedImageDisabledIndex;
        cbChecked: CIndex := FCustomCheckedImageDisabledIndex;
      end;
    end;
  end;
  A := 255;
  if CIndex = -1 then
  begin
    case ACheckBoxState of
      cbUnChecked: CIndex := FCustomUnCheckedImageIndex;
      cbChecked: CIndex := FCustomCheckedImageIndex;
    end;
    if ACtrlState = scsDisabled then
      A := 120;
  end;
  if (FCustomImages <> nil) and (FCustomImages.IsIndexAvailable(CIndex))
  then
  begin
    if (FCustomImages is TscImageCollection) and not (TscImageCollection(FCustomImages).Images[CIndex].Bitmap.Empty) then
      TscImageCollection(FCustomImages).DrawBitmap(ACanvas, ARect, CIndex, A)
    else
      FCustomImages.Draw(ACanvas, ARect, CIndex, FScaleFactor);
  end;
end;

procedure TscRadioButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, TR: TRect;
  TextColor: TColor;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
  S: TPoint;
  FUpdateGlow: Boolean;
begin
  R := Rect(0, 0, Width, Height);
  TR := R;

  S := GetCustomCheckBoxSize;
  if S.X = 0 then
  begin
    S.X := GetCheckBoxSize(ACanvas, FScaleFactor) + 5;
    if BiDiMode = bdRightToLeft then
    begin
      TR.Left := TR.Right - S.X;
      Dec(R.Right, S.X);
    end
    else
    begin
      TR.Right := TR.Left + S.X;
      Inc(R.Left, S.X);
    end;
    Inc(TR.Top, 1);
    if FChecked then
      DrawRadioButton(ACanvas, TR, ACtrlState, cbChecked, FScaleFactor)
    else
      DrawRadioButton(ACanvas, TR, ACtrlState, cbUnChecked, FScaleFactor)
  end
  else
  begin
    if BiDiMode = bdRightToLeft then
    begin
      TR.Left := TR.Right - S.X - 5;
      Dec(R.Right, S.X + 5);
    end
    else
    begin
      TR.Right := TR.Left + S.X + 5;
      Inc(R.Left, S.X + 5);
    end;
    if FChecked then
      DrawCustomCheckBox(ACanvas, TR, ACtrlState, cbChecked)
    else
      DrawCustomCheckBox(ACanvas, TR, ACtrlState, cbUnChecked);
  end;

   if (ACtrlState <> scsDisabled) and FUseFontColorToStyleColor
  then
    TextColor := GetStyleColor(Self.Font.Color)
  else
    TextColor := GetCheckBoxTextColor(ACtrlState);

  ACanvas.Font.Assign(Font);
  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
    ACanvas.Font.Color := TextColor;
  ACanvas.Brush.Style := bsClear;

   if GlowEffect.Enabled then
    InflateRect(R, -GlowEffect.GlowSize div 2, -GlowEffect.GlowSize div 2)
  else
  if ShowFocusRect then
  begin
    if (Layout = blGlyphLeft) and IsRightToLeft then
      OffsetRect(R, 2, 0)
    else
    if (Layout = blGlyphRight) and not IsRightToLeft then
      OffsetRect(R, -2, 0);
  end;

  FMargin := 0;
  if (FLayout = blGlyphTop) or (Layout = blGlyphBottom) then
    FMargin := -1;

  if FGlowEffect.Enabled and GetGlowParams(ACtrlState, FGlowColor, FGlowSize, FGlowAlpha) then
  begin
    if (FLayout = blGlyphLeft) or (FLayout = blGlyphRight) then
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, FImageIndex, FImages);
      DrawImageAndTextWithGlowBuffer2(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
        ACanvas, R, FMargin, FSpacing,
        FLayout, Caption, FImageIndex, FImages,
        ACtrlState <> scsDisabled, False, clBlack,
        FGlowEffect.Offset, FGlowColor, FGlowSize, FGlowEffect.Intensive, FGlowAlpha,FImageGlow,
        FCanFocused and Focused and FShowFocusRect, IsRightToLeft, False, FScaleFactor, FWordWrap)
    end
    else
    begin
      FUpdateGlow := CanUpdateGlowBuffer(ACanvas, FGlowColor, FGlowSize, FGlowAlpha, R, FImageIndex, FImages);
      DrawImageAndTextWithGlowbuffer(FGlowBuffer, FGlowImageBuffer, FUpdateGlow,
        ACanvas, R, FMargin, FSpacing,
        FLayout, Caption, FImageIndex, FImages,
        ACtrlState <> scsDisabled, False, clBlack,
        FGlowEffect.Offset, FGlowColor, FGlowSize, FGlowEffect.Intensive, FGlowAlpha,FImageGlow,
        FCanFocused and Focused and FShowFocusRect, IsRightToLeft, False, FScaleFactor, FWordWrap);
    end;
  end
  else
    if (FLayout = blGlyphLeft) or (FLayout = blGlyphRight) then
      DrawImageAndText2(ACanvas, R, FMargin, FSpacing,
       FLayout, Caption, FImageIndex, FImages,
        ACtrlState <> scsDisabled, False, clBlack, FCanFocused and Focused and FShowFocusRect,
        IsRightToLeft, False, FScaleFactor, FWordWrap)
    else
      DrawImageAndText(ACanvas, R, FMargin, FSpacing,
       FLayout, Caption, FImageIndex, FImages,
       ACtrlState <> scsDisabled, False, clBlack, FCanFocused and Focused and FShowFocusRect,
       IsRightToLeft, False, FScaleFactor, FWordWrap);
end;

procedure TscRadioButton.SetChecked(Value: Boolean);
var
  I: Integer;
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if FChecked and (Parent <> nil) then
    begin
     for I := 0 to Parent.ControlCount - 1 do
      if (Parent.Controls[I] is TscRadioButton)
       and (Parent.Controls[I] <> Self) and TscRadioButton(Parent.Controls[I]).Checked then
       begin
         TscRadioButton(Parent.Controls[I]).FClickDisabled := True;
         TscRadioButton(Parent.Controls[I]).Checked := False;
         TscRadioButton(Parent.Controls[I]).FClickDisabled := False;
       end;
    end;
    RePaintControl;
    if not FClickDisabled then
    begin
      FClickDisabled := True;
      ButtonClick;
      FClickDisabled := False;
    end;
  end;
end;

procedure TscRadioButton.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscRadioButton.ButtonClick;
begin
  if not Checked and not FClickDisabled then
  begin
    FClickDisabled := True;
    Checked := True;
    FClickDisabled := False;
    inherited;
  end;
end;

constructor TscCustomListBox.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FFluentUIOpaque := False;
  Style := lbOwnerDrawFixed;
  FImages := nil;
  FImageIndex := -1;
  FWordBreak := False;
  FTitleDivider := '';
  FShowLines := False;
  FLineColor := clBtnFace;
  FShowFocusRect := True;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
  FTabWidths := TStringList.Create;
end;

destructor TscCustomListBox.Destroy;
begin
  FTabWidths.Free;
  inherited;
end;

procedure TscCustomListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if IsFluentUIOpaque then
      ExStyle := Exstyle or WS_EX_LAYERED;
  end;
end;

function TscCustomListBox.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscCustomListBox.CreateWnd;
begin
  inherited;
  if IsFluentUIOpaque then
  begin
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
      SetTimer(Handle, 101, 10, nil);
  end;
end;

procedure TscCustomListBox.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_TIMER:
    if (TWMTimer(Message).TimerID = 101) and IsFluentUIOpaque then
    begin
      KillTimer(Handle, 101);
      RedrawWindow(Handle, nil, 0,
        RDW_INVALIDATE + RDW_ERASE + RDW_FRAME + RDW_ALLCHILDREN + RDW_UPDATENOW);
    end;
  end;
end;

procedure TscCustomListBox.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TscCustomListBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  IH: Integer;
begin
  IH := MulDiv(ItemHeight, M, D);
  inherited;
  ItemHeight := IH;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscCustomListBox.SetTabWidths(Value: TStrings);
begin
  if FTabWidths <> Value then
  begin
    FTabWidths.Assign(Value);
    Invalidate;
  end;
end;

procedure TscCustomListBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);

procedure PaintBG(DC: HDC);
var
  FCanvas: TCanvas;
  I, J, W, H, IH: Integer;
  R: TRect;
  DrawCount: Integer;
begin
  J := 0;
  W := ClientWidth;
  H := ClientHeight;
  if (Count > 0) and (ItemHeight > 0) then
  begin
    IH := ItemHeight;
    DrawCount := H div ItemHeight;
    I := TopIndex;
    J := I + DrawCount;
    if J > Count - 1 then
      J := Count - 1;
    J := J - TopIndex + 1;
    if J < 0 then J := 0;
  end
  else
    IH := 0;
  R := Rect(0, j * IH, W, H);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := DC;
  try
    FCanvas.Brush.Color := Self.Brush.Color;
    FCanvas.FillRect(R);
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
end;

begin
  if not IsCustomStyle and (Columns = 0) then
  begin
    PaintBG(Message.DC);
    Message.Result := 1;
  end
  else
    inherited;
end;


procedure TscCustomListBox.WMSIZE(var Msg: TMessage);
begin
  inherited;
  RePaint;
end;

procedure TscCustomListBox.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

function TscCustomListBox.CanDrawFocusRect: Boolean;
begin
  Result := FShowFocusRect;
end;

procedure TscCustomListBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TscCustomListBox.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.SetShowLines(Value: Boolean);
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.SetTitleDivider(Value: String);
begin
  if FTitleDivider <> Value then
  begin
    FTitleDivider := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.SetWordBreak(Value: Boolean);
begin
  if FWordBreak <> Value then
  begin
    FWordBreak := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TscCustomListBox.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TscCustomListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  R, FR: TRect;
  Buffer: TBitmap;
  IIndex: Integer;
  C: TColor;
begin
  with Message.DrawItemStruct^ do
  begin
    if rcItem.Height * rcItem.Width = 0 then
    begin
      Message.Result := 1;
      Exit;
    end;
    IIndex := itemID;
    State := TOwnerDrawState(LoWord(itemState));
    if (csPaintCopy in ControlState) then
    begin
      if not MultiSelect and (IIndex = ItemIndex) and not (odSelected in State) then
        Include(State, odSelected)
      else
      if MultiSelect and (IIndex >= 0) and (IIndex < Items.Count) and
         Selected[IIndex] and not (odSelected in State)
      then
        Include(State, odSelected);
    end;
    Buffer := TBitmap.Create;
    try
      Canvas.Handle := hDC;
      Buffer.SetSize(rcItem.Width, rcItem.Height);
      Buffer.Canvas.Font := Font;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      with Buffer.Canvas do
      begin
        if (FSelectionStyle = scstColor) and
           (odSelected in State)
        then
        begin
          if FSelectionColor <> clNone then
          begin
            Font.Color := FSelectionTextColor;
            Brush.Color := FSelectionColor;
          end
          else
          begin
            Font.Color := GetStyleColor(clHighLightText);
            Brush.Color := GetStyleColor(clHighLight);
          end;
        end
        else
        begin
          if {$IFNDEF VER230}(seClient in StyleElements) and {$ENDIF} IsCustomStyle then
          begin
            if Enabled then
            begin
              {$IFNDEF VER230}
              if seFont in StyleElements then
                Font.Color := GetEditTextColor(scsNormal)
              else
                Font.Color := Self.Font.Color;
              {$ELSE}
              Font.Color := GetEditTextColor(scsNormal);
              {$ENDIF}
            end
            else
            begin
              Font.Color := GetEditTextColor(scsDisabled);
            end;    
            Brush.Color := GetEditBrushColor(scsNormal);
          end
          else
          begin
            Font.Color := Self.Font.Color;
            Brush.Color := Self.Color;
            if not Enabled then
              if IsCustomStyle then
                Font.Color := GetEditTextColor(scsDisabled)
              else
                Font.Color := clGrayText;
          end;
        end;
        if (SelectionStyle = scstColor) and (odSelected in State)
           and not Focused and not FShowFocusRect then
        begin
          C := Brush.Color;
          Brush.Color := Self.Color;
          FillRect(R);
          Brush.Color := C;
          FillRectWithAlpha(Buffer.Canvas, R, 200);
        end
        else
          FillRect(R);
        if FShowLines and not (odSelected in State) then
        begin
          Pen.Color := GetStyleColor(FLineColor);
          MoveTo(0, Buffer.Height - 1);
          LineTo(Buffer.Width, Buffer.Height -1);
        end;
      end;
      if (odSelected in State) then
      begin
        if FSelectionStyle = scstStyled then
        begin
          Buffer.Canvas.Font.Color := GetSelectionTextColor;
          DrawSelection(Buffer.Canvas, R, Focused and (odFocused in State), FShowFocusRect);
        end;
      end;
      if Focused and (odFocused in State) then
      begin
        if CanDrawFocusRect or (IIndex < 0) then
        begin
          FR := R;
          if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
             and (FSelectionStyle = scstStyled) then
            InflateRect(FR, -1, -1);
          scDrawFocusRect(Buffer.Canvas, FR, FScaleFactor);
        end;
      end;
      Buffer.Canvas.Brush.Style := bsClear;
      if FScaleFactor >= 1.5 then
      begin
        Inc(R.Left);
        Dec(R.Right);
      end;
      if IIndex >= 0 then
        if Assigned(FOnDrawItem) then
          FOnDrawItem(IIndex, State, Buffer.Canvas, R)
        else
          DrawItemContent(Buffer.Canvas, R, IIndex, State);
      Canvas.Draw(rcItem.Left, rcItem.Top, Buffer);
    finally
      Buffer.Free;
      Canvas.Handle := 0;
    end;
  end;
  Message.Result := 1;
end;

procedure TscCustomListBox.DrawItemContent(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AState: TOwnerDrawState);
var
  R, R1: TRect;
  IIndex: Integer;
  X, Y: Integer;
  S, S1: String;
  IL: TCustomImageList;
begin
  IL := Images;
  IIndex := -1;
  if Assigned(FOnGetDrawItemParams) then
    FOnGetDrawItemParams(AIndex, AState, ACanvas, IL, IIndex);
  R := ARect;
  Inc(R.Left, 2);
  Dec(R.Right, 2);
  Dec(R.Bottom);
  if IL <> nil then
  begin
    if BidiMode = bdRightToLeft then
    begin
      X := R.Right - FImages.Width;
      Y := R.Top + R.Height div 2 - FImages.Height div 2;
    end
    else
    begin
      X := R.Left;
      Y := R.Top + R.Height div 2 - FImages.Height div 2;
    end;
    if IIndex = -1 then
      if FImageIndex = -1 then
        IIndex := AIndex
      else
        IIndex := FImageIndex;
    if (IIndex >= 0) and (IIndex < FImages.Count) then
    if not Self.Enabled then
      DrawBitmapFromImageList(ACanvas, X, Y, FImages, IIndex, DisabledImageAlphaValue)
    else
      FImages.Draw(ACanvas, X, Y, IIndex, True);
    if BidiMode = bdRightToLeft then
      Dec(R.Right, FImages.Width + 5)
    else
      Inc(R.Left, FImages.Width + 5);
  end;
  S := Items[AIndex];
  if FTitleDivider <> '' then
  begin
    X := Pos(FTitleDivider, S);
    if X <> 0 then
    begin
      S1 := Copy(S, 1, X - 1);
      Delete(S, 1, X);
      R1 := R;
      Inc(R1.Top, 2);
      R1.Bottom := R1.Top + ACanvas.TextHeight('Wq');
      ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
      scDrawText(ACanvas, S1, R1, IsRightToLeft, True);
      ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
      R.Top := R1.Bottom;
      Inc(R.Left, 2);
    end;
  end;

  if FTabWidths.Count > 0 then
    scDrawUtils.DrawTabbedString(S, FTabWidths, ACanvas, R, 0)
  else
  if not FWordBreak
  then
    scDrawText(ACanvas, S, R, IsRightToLeft, True)
  else
    begin
      R1 := Rect(0, 0, R.Width, R.Height);
      DrawText(ACanvas.Handle,
        PChar(S), Length(S), R1, DT_VCENTER or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX or DT_CALCRECT);
      X := R.Left;
      Y := R.Top + R.Height div 2 - R1.Height div 2;
      R := Rect(X, Y, X + R1.Width, Y + R1.Height);
      DrawText(ACanvas.Handle,
        PChar(S), Length(S), R,
        scDrawTextBidiModeFlags(DT_VCENTER or DT_LEFT or DT_WORDBREAK
           or DT_NOPREFIX or DT_NOCLIP, BidiMode = bdRightToLeft));
    end;
end;

constructor TscCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FFluentUIOpaque := False;
  FWordBreak := False;
  FTitleDivider := '';
  FImages := nil;
  FImageIndex := -1;
  FTempItemIndex := -1;
  FItemHeight := 16;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
  FShowFocusRect := True;
end;

procedure TscCustomComboBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  IH: Integer;
  {$IFDEF VER320_UP}
  OldH: Integer;
  {$ENDIF}
begin
  {$IFDEF VER320_UP}
  OldH := Height;
  {$ENDIF}
  IH := MulDiv(ItemHeight, M, D);
  inherited;
  if ((Style <> csSimple) and (Style <> csDropDown))
     {$IFDEF VER320_UP} or ((OldH = Height) and (csLoading in ComponentState)) {$ENDIF} then
    ItemHeight := IH;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

function TscCustomComboBox.IsFocused: Boolean;
begin
  Result := (Handle <> 0) and (GetFocus = Handle);
end;

function TscCustomComboBox.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscCustomComboBox.CreateWnd;
begin
  inherited;
  if IsFluentUIOpaque then
  begin
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
      SetTimer(Handle, 101, 10, nil);
  end;
end;


procedure TscCustomComboBox.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TscCustomComboBox.WndProc(var Message: TMessage);
begin
  if IsCustomDraw then
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
      if Visible then
      begin
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
      end;
  end;
  inherited;
  if IsCustomDraw then
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
      if Visible then
      begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
      end;
  end;

  case Message.Msg of
    WM_TIMER:
       if (TWMTimer(Message).TimerID = 101) and IsFluentUIOpaque then
        begin
          KillTimer(Handle, 101);
          RedrawWindow(Handle, nil, 0,
            RDW_INVALIDATE + RDW_ERASE + RDW_FRAME + RDW_ALLCHILDREN + RDW_UPDATENOW);
        end;
  end;
end;

procedure TscCustomComboBox.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE)  then
  begin
    FTempItemIndex := ItemIndex;
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  end;
  inherited;
end;

procedure TscCustomComboBox.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  if IsCustomDraw then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
end;

function TscCustomComboBox.IsCustomDraw: Boolean;
begin
  Result := ((StyleKind <> scscbDefault) and (Style <> csDropDown) and (Style <> csSimple)) or
   ((Style = csDropDownList) and not IsWindowsXP and TStyleManager.SystemStyle.Enabled);
end;

procedure TscCustomComboBox.CMSENCPaint(var Message: TMessage);
begin
  Message.Result := SE_RESULT;
end;

procedure TscCustomComboBox.CMSEPaint(var Message: TMessage);
var
  DC: HDC;
  SaveIndex: Integer;
begin
  DC := Message.WParam;
  if FEditHandle <> 0 then
  begin
    SaveIndex := SaveDC(DC);
    MoveWindowOrg(Message.WParam, 3, 3);
    SendMessage(FEditHandle, WM_PAINT, Message.WParam, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TscCustomComboBox.PaintToDC(DC: HDC; X, Y: Integer);
var
  SaveIndex: Integer;
begin
  ControlState := ControlState + [csPaintCopy];
  SaveIndex := SaveDC(DC);
  try
    MoveWindowOrg(DC, X, Y);
    IntersectClipRect(DC, 0, 0, Width, Height);
    Perform(WM_PAINT, DC, 0);
  finally
    RestoreDC(DC, SaveIndex);
  end;
  ControlState := ControlState - [csPaintCopy];
end;

procedure TscCustomComboBox.PaintCustomCombo(ACanvas: TCanvas);
var
  R, R1, BR: TRect;
  Buffer: TBitmap;
  FState: TscsCtrlState;
  BW: Integer;
  FC: TColor;
  IIndex: Integer;
begin
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Width, Height);
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    if (Parent <> nil) and (Parent is TscCustomControl) then
      TscCustomControl(Parent).FGetControlBG := True;
    try
      DrawParentBackground(Self, Buffer.Canvas);
    finally
      if (Parent <> nil) and (Parent is TscCustomControl) then
        TscCustomControl(Parent).FGetControlBG := False;
    end;
    if DroppedDown then
      FState := scsPressed
    else
    if IsFocused and not FShowFocusRect and IsCustomStyle then
      FState := scsFocused
    else
    if MouseInClient and not (csDesigning in ComponentState) then
      FState := scsHot
    else
    if Enabled then
      FState := scsNormal
    else
      FState := scsDisabled;
    DrawButton(Buffer.Canvas, R, FState, False);
    Buffer.Canvas.Font := Self.Font;
    FC := GetButtonTextColor(FState);
    Buffer.Canvas.Font.Color := FC;
    // draw item
    InflateRect(R, -2, -2);
    BW := GetSystemMetrics(SM_CXVSCROLL);
    if BidiMode = bdRightToLeft then
    begin
      Inc(R.Left, BW);
      BR := Rect(2, R.Top, R.Left, R.Bottom);
    end
    else
    begin
      Dec(R.Right, BW);
      BR := Rect(R.Right, R.Top, Width - 2, R.Bottom);
    end;
    R1 := R;
    Inc(R.Left, 2);
    Dec(R.Right, 2);
    Buffer.Canvas.Brush.Style := bsClear;
    if Self.DroppedDown then
      IIndex := FTempItemIndex
    else
      IIndex := ItemIndex;
    if IIndex <> -1 then
    begin
      if Assigned(FOnDrawItem) then
        FOnDrawItem(IIndex, [], Buffer.Canvas, R)
      else
        DrawItemContent(Buffer.Canvas, R, IIndex, [], True);
    end;
    if IsFocused and (FState <> scsPressed) and (FShowFocusRect or not IsCustomStyle) then
    begin
      InflateRect(R1, -1, -1);
      scDrawFocusRect(Buffer.Canvas, R1, FScaleFactor);
    end;
    if Enabled then
      FState := scsNormal
    else
      FState := scsDisabled;
    if IsCustomStyle or not StyleServices.Enabled or IsWindowsXP then
      DrawComboArrowImage(Buffer.Canvas, BR, FC, FScaleFactor)
    else
      DrawDropDownButton(Buffer.Canvas, BR, FState, True, False, FScaleFactor);
    ACanvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TscCustomComboBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  Canvas: TCanvas;
begin
  if IsCustomDraw then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := Message.DC;
    try
      PaintCustomCombo(Canvas);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
  end
  else
    inherited;
end;

procedure TscCustomComboBox.WMPaint(var Message: TWMPaint);
var
  Canvas: TCanvas;
  PS: TPaintStruct;
  DC: HDC;
begin
  if IsCustomDraw then
  begin
    DC := Message.DC;
    Canvas := TCanvas.Create;
    if DC = 0 then
      Canvas.Handle := BeginPaint(Handle, PS)
    else
      Canvas.Handle := DC;
    try
      PaintCustomCombo(Canvas);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
      if DC = 0 then
        EndPaint(Handle, PS);
    end;
  end
  else
    inherited;
end;

procedure TscCustomComboBox.SetStyleKind(Value: TscComboStyleKind);
begin
  if (Style = csDropDown) or (Style = csSimple) then
    Value := scscbDefault;
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TscCustomComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  R: TRect;
  Buffer: TBitmap;
begin
  with Message.DrawItemStruct^ do
  begin
    if (rcItem.Height * rcItem.Width = 0) or (Integer(itemID) < 0) then
    begin
      State := TOwnerDrawState(LoWord(itemState));
      if (Integer(itemID) < 0) and (odFocused in State) then
      begin
        Canvas.Handle := hDC;
        if {$IFNDEF VER230}(seFont in StyleElements) and {$ENDIF} IsCustomStyle then
          scDrawColorFocusRect(Canvas, rcItem, GetStyleColor(clWindowText))
        else
          scDrawColorFocusRect(Canvas, rcItem, Font.Color);
        Canvas.Handle := 0;
      end;
      Message.Result := 1;
      Exit;
    end;
    State := TOwnerDrawState(LoWord(itemState));
    Buffer := TBitmap.Create;
    try
      Canvas.Handle := hDC;
      Buffer.SetSize(rcItem.Width, rcItem.Height);
      Buffer.Canvas.Font := Font;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      with Buffer.Canvas do
      begin
        if (FSelectionStyle = scstColor) and
           (odSelected in State)
        then
        begin
          if FSelectionColor <> clNone then
          begin
            Font.Color := FSelectionTextColor;
            Brush.Color := FSelectionColor;
          end
          else
          begin
            Font.Color := GetStyleColor(clHighLightText);
            Brush.Color := GetStyleColor(clHighLight);
          end;
        end
        else
        begin
          if {$IFNDEF VER230}(seClient in StyleElements) and {$ENDIF}IsCustomStyle then
          begin
            {$IFNDEF VER230}
            if seFont in StyleElements then
              Font.Color := GetEditTextColor(scsNormal)
            else
              Font.Color := Self.Font.Color;
            {$ELSE}
            Font.Color := GetEditTextColor(scsNormal);
            {$ENDIF}
            if not Enabled then
              Font.Color := GetEditTextColor(scsDisabled);
            Brush.Color := GetEditBrushColor(scsNormal);
          end
          else
          begin
            Font.Color := Self.Font.Color;
            Brush.Color := Self.Color;
            if not Enabled then
              if IsCustomStyle then
                Font.Color := GetEditTextColor(scsDisabled)
              else
                Font.Color := clGrayText;
          end;
        end;
        FillRect(R);
      end;
      if (odSelected in State) then
      begin
        if FSelectionStyle = scstStyled then
        begin
          Buffer.Canvas.Font.Color := GetSelectionTextColor;
          DrawSelection(Buffer.Canvas, R, (odFocused in State) and not DroppedDown, True);
        end;
      end;
      Buffer.Canvas.Brush.Style := bsClear;
      if Assigned(FOnDrawItem) then
        FOnDrawItem(itemID, State, Buffer.Canvas, R)
      else
        DrawItemContent(Buffer.Canvas, R, itemID, State, False);
      Canvas.Draw(rcItem.Left, rcItem.Top, Buffer);
    finally
      Buffer.Free;
      Canvas.Handle := 0;
    end;
  end;
  Message.Result := 1;
end;

procedure TscCustomComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or CBS_OWNERDRAWVARIABLE;
    if IsFluentUIOpaque then
      ExStyle := Exstyle or WS_EX_LAYERED;
  end;
end;

procedure TscCustomComboBox.DrawItemContent(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AState: TOwnerDrawState; AComboItem: Boolean);
var
  R, R1: TRect;
  IIndex: Integer;
  X, Y: Integer;
  S, S1: String;
  IL: TCustomImageList;
begin
  IL := Images;
  IIndex := -1;
  if Assigned(FOnGetDrawItemParams) then
    FOnGetDrawItemParams(AIndex, AState, ACanvas, IL, IIndex);
  R := ARect;
  Inc(R.Left, 2);
  Dec(R.Right, 2);
  if IL <> nil then
  begin
    if BidiMode = bdRightToLeft then
    begin
      X := R.Right - FImages.Width;
      Y := R.Top + R.Height div 2 - FImages.Height div 2;
    end
    else
    begin
      X := R.Left;
      Y := R.Top + R.Height div 2 - FImages.Height div 2;
    end;
    if IIndex = -1 then
      if FImageIndex = -1 then
        IIndex := AIndex
      else
        IIndex := FImageIndex;
    if (IIndex >= 0) and (IIndex < FImages.Count) then

    if not Self.Enabled then
      DrawBitmapFromImageList(ACanvas, X, Y, FImages, IIndex, DisabledImageAlphaValue)
    else
      FImages.Draw(ACanvas, X, Y, IIndex, True);

    if BidiMode = bdRightToLeft then
      Dec(R.Right, FImages.Width + 5)
    else
      Inc(R.Left, FImages.Width + 5);
  end;
  S := Items[AIndex];
  if FTitleDivider <> '' then
  begin
    X := Pos(FTitleDivider, S);
    if X <> 0 then
    begin
      S1 := Copy(S, 1, X - 1);
      Delete(S, 1, X);
      R1 := R;
      Inc(R1.Top, 2);
      R1.Bottom := R1.Top + ACanvas.TextHeight('Wq');
      ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
      scDrawText(ACanvas, S1, R1, IsRightToLeft, True);
      ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
      R.Top := R1.Bottom;
      Inc(R.Left, 2);
    end;
  end;

  if not FWordBreak
  then
    scDrawText(ACanvas, S, R, IsRightToLeft, True)
  else
    begin
      R1 := Rect(0, 0, R.Width, R.Height);
      DrawText(ACanvas.Handle,
        PChar(S), Length(S), R1, DT_VCENTER or DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
      X := R.Left;
      Y := R.Top + R.Height div 2 - R1.Height div 2;
      R := Rect(X, Y, X + R1.Width, Y + R1.Height);
      DrawText(ACanvas.Handle,
        PChar(S), Length(S), R, scDrawTextBidiModeFlags(DT_VCENTER or DT_LEFT or DT_WORDBREAK or DT_NOCLIP, BidiMode = bdRightToLeft));
    end;
end;

procedure TscCustomComboBox.SetTitleDivider(Value: String);
begin
  if FTitleDivider <> Value then
  begin
    FTitleDivider := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.SetWordBreak(Value: Boolean);
begin
  if FWordBreak <> Value then
  begin
    FWordBreak := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TscCustomComboBox.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    RecreateWnd;
  end;
end;

procedure TscCustomComboBox.CNMeasureItem(var Message: TWMMeasureItem);
var
  Height: Integer;
  MeasureItemStruct: PMeasureItemStruct;
begin
  MeasureItemStruct := Message.MeasureItemStruct;
  with MeasureItemStruct^ do
  begin
    itemHeight := FItemHeight;
    Height := FItemHeight;
    MeasureItem(itemID, Height);
    itemHeight := Height;
  end;
end;

procedure TscCustomComboBox.SetStyle(Value: TComboBoxStyle);
begin
  inherited;
  if Style = csDropDown then
  begin
    Canvas.Font := Font;
    ItemHeight := Canvas.TextHeight('Wq') + 2;
    FStyleKind := scscbDefault;
  end
  else
  if Style = csSimple then
    FStyleKind := scscbDefault;
end;


procedure TscCustomComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Style = csDropDown then
  begin
    Canvas.Font := Font;
    ItemHeight := Canvas.TextHeight('Wq') + 2;
  end;
end;

procedure TscComboBox.AddMRUItem(Value: String);
var
  I: Integer;
begin
  if Value = '' then Exit;
  I := Items.IndexOf(Value);
  if I <> -1
  then
    Items.Move(I, 0)
  else
    Items.Insert(0, Value);
end;

constructor TscCustomTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FCheckSetting := False;
  FFluentUIOpaque := False;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FCheckBoxes := False;
  FCheckingTree := False;
  FCheckHierarchy := True;
  OnAdvancedCustomDrawItem := HookAdvancedCustomDrawItem;
  FThirdImages := 0;
  FThirdImageWidth := 0;
  FThirdImageHeight := 0;
  FThirdImagesCount := 0;
  FButtonSize := 5;
  FButtonImages := nil;
  FButtonCollapseImageIndex := 0;
  FButtonExpandImageIndex := 1;
  FShowFocusRect := True;
  FSelectionStyle := scstStyled;
  FDefaultDraw := False;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
  FButtonColor := clNone;
  FButtonGlyphColor := clBtnFace;
end;

destructor TscCustomTreeView.Destroy;
begin
  inherited;
end;

procedure TscCustomTreeView.WMKeyDown(var Message: TWMKeyDown);
var
  B: Boolean;
  Node1, Node2: TTreeNode;
begin
  if not FCheckBoxes or (Message.CharCode <> VK_SPACE) then
  begin
    inherited;
    Exit;
  end;

  B := False;
  Node1 := Selected;
  if Node1 <> nil then
    B := GetChecked(Node1);

  inherited;

  Node2 := Selected;
  if (Node2 <> nil) and (Node1 = Node2) and (B <> GetChecked(Node2)) then
    DoNodeCheckedChanged(Node2, not B);
end;

procedure TscCustomTreeView.WMLButtonDblClk(var Message: TWMLButtonDown);
var
  B: Boolean;
  Node1, Node2: TTreeNode;
begin
  if not FCheckBoxes then
  begin
    inherited;
    Exit;
  end;

  Node1 := GetNodeAt(Message.XPos, Message.YPos);

  if Node1 <> nil then
    B := GetChecked(Node1)
  else
    B := False;

  inherited;

  Node2 := GetNodeAt(Message.XPos, Message.YPos);
  if (Node2 <> nil) and (Node1 = Node2) and (B <> GetChecked(Node2)) then
    DoNodeCheckedChanged(Node2, not B);
end;


procedure TscCustomTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  B: Boolean;
  Node1, Node2: TTreeNode;
begin
  if not FCheckBoxes then
  begin
    inherited;
    Exit;
  end;

  Node1 := GetNodeAt(Message.XPos, Message.YPos);

  if Node1 <> nil then
    B := GetChecked(Node1)
  else
    B := False;

  inherited;

  Node2 := GetNodeAt(Message.XPos, Message.YPos);
  if (Node2 <> nil) and (Node1 = Node2) and (B <> GetChecked(Node2)) then
    DoNodeCheckedChanged(Node2, not B);
end;

function TscCustomTreeView.GetChecked(Node: TTreeNode): Boolean;
var
  Item: TTVItem;
begin
  Result := False;
  Item.hItem := Node.ItemId;
  Item.Mask := TVIF_STATE;
  Item.StateMask := TVIS_STATEIMAGEMASK;
  if TreeView_GetItem(Handle, Item) then
    Result := ((Integer(Item.State) and
      IndexToStateImageMask(TV_CHECKED)) = IndexToStateImageMask(TV_CHECKED));
  if (Node.StateIndex = TV_CHECKED) and not Result then
  begin
    Item.State := IndexToStateImageMask(TV_CHECKED);
    FCheckSetting := True;
    TreeView_SetItem(Handle, Item);
    FCheckSetting := False;
  end;
end;

procedure TscCustomTreeView.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TscCustomTreeView.SetChecked(Node: TTreeNode; Value: Boolean);
begin
  if Value = GetChecked(Node) then
    Exit;

  if Value then
    Node.StateIndex := TV_CHECKED
  else
    Node.StateIndex := TV_UNCHECKED;

  UpdateCheckNodes(Node);
end;

procedure TscCustomTreeView.DoNodeCheckedChanged(Node: TTreeNode;
  NewState: Boolean);
begin
 if NewState then
   Node.StateIndex := TV_CHECKED
 else
   Node.StateIndex := TV_UNCHECKED;
  UpdateCheckNodes(Node);
  if Assigned(FOnNodeCheckedChanged) then
    FOnNodeCheckedChanged(Self, Node, NewState);
end;

procedure TscCustomTreeView.DoNodeCheckedChanging(Node: TTreeNode;
  NewState: Boolean; var Allow: boolean);
begin
  if Assigned(FOnNodeCheckedChanging) then
    FOnNodeCheckedChanging(Self, Node, NewState, Allow);
end;

procedure TscCustomTreeView.UpdateCheckNodes(Node: TTreeNode);

procedure ChildNodesSetChecked(Node: TTreeNode; AValue: Boolean);
var
  Index: Integer;
begin
  SetChecked(Node, AValue);
  for Index := 0 to Node.Count  - 1 do
    ChildNodesSetChecked(Node[Index], AValue);
end;

var
  B: Boolean;
  I: integer;
begin
  if FCheckingTree then
    Exit;

  FCheckingTree := True;

  B := GetChecked(Node);

  if FCheckHierarchy then
    for I := 0 to Node.Count -1 do
      ChildNodesSetChecked(Node[I], B);

  FCheckingTree := False;
end;

procedure TscCustomTreeView.SetCheckBoxes(Value: Boolean);
begin
  if Value <> FCheckBoxes then
  begin
    FCheckBoxes := Value;
    RecreateWnd;
  end;
end;

procedure TscCustomTreeView.CNNotify(var Message: TWMNotify);
var
  B, B1, AllowChanges: Boolean;
  Node: TTreeNode;
  NInfo: TAGNMTVITEMCHANGE;
begin
  if FCheckBoxes and (Message.NMHdr^.code = TVN_ITEMCHANGING) then
  begin
    NInfo := PNMTVITEMCHANGE(Pointer(Message.nmhdr))^;
    if (NInfo.uChanged = TVIF_STATE) and not FCheckSetting then
    begin
      B := (NInfo.uStateNew and
        UINT(IndexToStateImageMask(TV_CHECKED)) = UINT(IndexToStateImageMask(TV_CHECKED)));
      B1 := (NInfo.uStateOld and
        UINT(IndexToStateImageMask(TV_CHECKED)) = UINT(IndexToStateImageMask(TV_CHECKED)));
      if B <> B1 then
      begin
        Node := Items.GetNode(NInfo.hItem);
        if Node <> nil then
        begin
          AllowChanges := True;
          DoNodeCheckedChanging(Node, B, AllowChanges);
          if not AllowChanges then
          begin
            Message.Result := 1;
            Exit;
          end;
        end;
      end;
    end;
  end;
  inherited;
end;

procedure TscCustomTreeView.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  {$IFNDEF VER330_UP}
  Indent := MulDiv(Indent, M, D);
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscCustomTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FCheckBoxes then
    Params.Style := Params.Style or TVS_CHECKBOXES;
  if IsFluentUIOpaque then
    Params.ExStyle := Params.Exstyle or WS_EX_LAYERED;
end;

procedure TscCustomTreeView.FullRedraw;
begin
  RedrawWindow(Handle, nil, 0,
    RDW_FRAME or RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW);
end;

procedure TscCustomTreeView.SetButtonColor(Value: TColor);
begin
 if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomTreeView.SetButtonGlyphColor(Value: TColor);
begin
  if FButtonGlyphColor <> Value then
  begin
    FButtonGlyphColor := Value;
    Invalidate;
  end;
end;
      
procedure TscCustomTreeView.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomTreeView.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

function TscCustomTreeView.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscCustomTreeView.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_STYLECHANGED:
    begin
      {$IFNDEF VER230}
      if (seClient in StyleElements) then
      {$ENDIF}
      Brush.Color := GetStyleColor(Color);
      UpdateWindow(Handle);
    end;
    CM_COLORCHANGED:
     {$IFNDEF VER230}
     if (seClient in StyleElements) then
     {$ENDIF} 
     begin
       Brush.Color := GetStyleColor(Color);
       Invalidate;
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

procedure TscCustomTreeView.CreateWnd;
begin
  inherited;
  {$IFNDEF VER230}
  if (seClient in StyleElements) then
  {$ENDIF}
  Brush.Color := GetStyleColor(Color);
  if IsCustomStyle or not FDefaultDraw then
    OnAdvancedCustomDrawItem := HookAdvancedCustomDrawItem
  else
    OnAdvancedCustomDrawItem := nil;
  if IsFluentUIOpaque then
  begin
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
      SetTimer(Handle, 101, 10, nil);
  end;
end;

function TscCustomTreeView.CanDrawFocusRect: Boolean;
begin
  Result := FShowFocusRect;
end;

procedure TscCustomTreeView.SetDefaultDraw(Value: Boolean);
begin
  if FDefaultDraw <> Value then
  begin
    FDefaultDraw := Value;
    if  (csDesigning in ComponentState) or (not IsCustomStyle) then
    begin
      if not DefaultDraw then
        OnAdvancedCustomDrawItem := HookAdvancedCustomDrawItem
      else
        OnAdvancedCustomDrawItem := nil;
      Invalidate;
    end;
  end;
end;

procedure TscCustomTreeView.SetButtonStyle(Value: TscExpandButtonStyle);
begin
  if Value <> FButtonStyle then
  begin
    FButtonStyle := Value;
    Invalidate;
  end;
end;

procedure TscCustomTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FButtonImages) then
    FButtonImages := nil;
end;

procedure TscCustomTreeView.SetButtonImages(Value: TCustomImageList);
begin
  if FButtonImages <> Value then
  begin
    FButtonImages := Value;
    Invalidate;
  end;
end;

procedure TscCustomTreeView.SetButtonCollapseImageIndex(Value: Integer);
begin
  if FButtonCollapseImageIndex <> Value then
  begin
    FButtonCollapseImageIndex := Value;
    Invalidate;
  end;
end;

procedure TscCustomTreeView.SetButtonExpandImageIndex(Value: Integer);
begin
  if FButtonExpandImageIndex <> Value then
  begin
    FButtonExpandImageIndex := Value;
    Invalidate;
  end;
end;

procedure TscCustomTreeView.HookAdvancedCustomDrawItem(
   Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
     Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);


function GetImageIndex: Integer;
begin
  Result := Node.ImageIndex;
  if Images = nil then Exit;

  if Node.Selected and
    (Node.SelectedIndex >= 0) and (Node.SelectedIndex < Images.Count) then
      Result := Node.SelectedIndex;
  if Node.Expanded and
    (Node.ExpandedImageIndex >= 0) and (Node.ExpandedImageIndex < Images.Count) then
      Result := Node.ExpandedImageIndex;
end;


var
  NodeRect, TempNodeRect, R, R1, R2: TRect;
  ButtonColor: TColor;
  SI: TScrollInfo;
  Buffer: TBitmap;
  FLineColor: TColor;
  C: TColor;
  FCheckSize: Integer;
  LDefaultDraw: Boolean;
begin
  if Self.DefaultDraw and not IsCustomStyle then
  begin
    DefaultDraw := True;
    Exit;
  end
  else
    DefaultDraw := False;

  if not TreeView_GetItemRect(Handle, Node.ItemId, NodeRect, False) then Exit;
  if NodeRect.Width * NodeRect.Height = 0 then Exit;

  FCheckSize := Round(13 * FScaleFactor);

  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_ALL;
  if GetScrollInfo(Handle, SB_HORZ, SI)
  then
    OffsetRect(NodeRect, -SI.nPos, 0);
  Canvas.Font := Font;
  with Canvas do
  begin
    if {$IFNDEF VER230}(seFont in StyleElements) and {$ENDIF} IsCustomStyle then
      Font.Color := GetEditTextColor(scsNormal);
    if  {$IFNDEF VER230} (seClient in StyleElements) and {$ENDIF} IsCustomStyle then
    begin
      Brush.Color := GetStyleColor(Self.Color);
      ButtonColor := GetStyleColor(clWindowText);
    end
    else
    begin
      Brush.Color := Self.Color;
      ButtonColor := Self.Font.Color;
    end;
    if not Enabled then
    begin
      if IsCustomStyle then
        Font.Color := GetEditTextColor(scsDisabled)
      else
        Font.Color := clGrayText;  
    end;
    FLineColor := MiddleColor(Font.Color, Brush.Color);
    FillRect(NodeRect);
    if (cdsSelected in State) then
    begin
      if SelectionStyle = scstStyled then
        Font.Color := GetSelectionTextColor
      else
        Font.Color := GetStyleColor(clHighLightText);
    end
  end;
  TempNodeRect := NodeRect;
  if ShowRoot then
    TempNodeRect.Left := TempNodeRect.Left + (Node.Level  * Indent)
  else
    TempNodeRect.Left := TempNodeRect.Left + ((Node.Level - 1) * Indent);

  DrawButton(Canvas, ButtonColor, TempNodeRect, Node, FLineColor);
  // checkbox
  R1 := TempNodeRect;
  TempNodeRect.Left := TempNodeRect.Left + Indent + FButtonSize;
  if FCheckBoxes then
  begin
    R1.Left := R1.Left + Indent + Round(5 * FScaleFactor);
    R1.Right := R1.Left + FCheckSize;
    R2 := R1;
    OffsetRect(R1, Round(-5 * FScaleFactor) + 3, 0);
    if GetChecked(Node) then
      scDrawUtils.DrawTreeCheckBox(Canvas, R1, scsNormal, cbChecked, FScaleFactor)
    else
      scDrawUtils.DrawTreeCheckBox(Canvas, R1, scsNormal, cbUnChecked, FScaleFactor);
    TempNodeRect.Left := R2.Right + 5;
  end;

  DrawImage(Canvas, TempNodeRect, Node, GetImageIndex);

  Treeview_GetItemRect(Self.Handle, Node.ItemId, TempNodeRect, True);
  TempNodeRect.Right := TempNodeRect.Left + Canvas.TextWidth(Node.Text) + 2;
  if TempNodeRect.Width > 0 then
  begin
    Inc(TempNodeRect.Top);
    Dec(TempNodeRect.Bottom);
    if (cdsSelected in State) then
    begin
      Buffer := TBitmap.Create;
      Buffer.SetSize(TempNodeRect.Width, TempNodeRect.Height);
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      Buffer.Canvas.Font := Self.Font;
      if FSelectionStyle = scstColor
      then
      begin
        if FSelectionColor <> clNone then
        begin
          Buffer.Canvas.Font.Color  := FSelectionTextColor;
          Buffer.Canvas.Brush.Color := FSelectionColor;
        end
        else
        begin
          Buffer.Canvas.Brush.Color := GetStyleColor(clHighLight);
          Buffer.Canvas.Font.Color := GetStyleColor(clHighLighttext);
        end;
        if not Focused and not FShowFocusRect then
        begin
          C := Buffer.Canvas.Brush.Color;
          Buffer.Canvas.Brush.Color := Self.Canvas.Brush.Color;
          Buffer.Canvas.FillRect(R);
          Buffer.Canvas.Brush.Color := C;
          FillRectWithAlpha(Buffer.Canvas, R, 200);
        end
        else
        Buffer.Canvas.FillRect(R);
      end
      else
      begin
        Buffer.Canvas.Brush.Color := Canvas.Brush.Color;
        Buffer.Canvas.Font.Color := GetSelectionTextColor;
        Buffer.Canvas.FillRect(R);
        DrawSelection(Buffer.Canvas, R, Focused and (cdsFocused in State), FShowFocusRect);
      end;
      Buffer.Canvas.Brush.Style := bsClear;

      if Focused and (cdsFocused in State) and CanDrawFocusRect then
        scDrawFocusRect(Buffer.Canvas, R, FScaleFactor);

      LDefaultDraw := True;
      if Assigned(FOnCustomDrawItemText) then
        FOnCustomDrawItemText(Self, Buffer.Canvas, R, Node, State, LDefaultDraw);

      if LDefaultDraw then
        scDrawText(Buffer.Canvas, Node.Text, Rect(R.Left + 1, R.Top,
          R.Right, R.Bottom), BidiMode = bdRightToLeft, True);

      Canvas.Draw(TempNodeRect.Left, TempNodeRect.Top, Buffer);
      Buffer.Free;
    end
    else
    begin
      SetBkMode(Canvas.Handle, TRANSPARENT);

      LDefaultDraw := True;
      if Assigned(FOnCustomDrawItemText) then
      begin
        FOnCustomDrawItemText(Self, Canvas, TempNodeRect, Node, State, LDefaultDraw);
        if LDefaultDraw then
          SetTextColor(Canvas.Handle, ColorToRGB(Canvas.Font.Color));
      end;

      if LDefaultDraw then
        scDrawText(Canvas, Node.Text, Rect(TempNodeRect.Left + 1, TempNodeRect.Top,
          TempNodeRect.Right, TempNodeRect.Bottom), BidiMode = bdRightToLeft, True);
    end;
  end;
end;

procedure TscCustomTreeView.DrawButton(ACanvas: TCanvas; AColor: TColor; ARect: TRect; ANode: TTreeNode; ALineColor: TColor);
var
  cx, cy: Integer;
  R: TRect;
  C, C1: TColor;
begin
  if (FButtonImages <> nil) then
  begin
    if (FButtonImages.Width >= FButtonImages.Height)
    then
      FButtonSize := FButtonImages.Width div 2
    else
      FButtonSize := FButtonImages.Height div 2;
  end
  else
    FButtonSize := Round(5 * FScaleFactor);

  cx := ARect.Left + Indent div 2;
  cy := ARect.Top + (ARect.Bottom - ARect.Top) div 2;

  if ShowLines then
  begin
    if ANode.HasChildren and ShowRoot then
    begin
      DrawPixelLine(Canvas, cx + FButtonSize - 1, cy, ARect.Left + Indent + FButtonSize - 5, cy, ALineColor)
    end
    else
    begin
      if (ARect.Bottom - cy + 2) div 2 = (ARect.Bottom - cy + 2) / 2 then
        DrawPixelLine(Canvas, cx - 1, cy, ARect.Left + Indent, cy, ALineColor)
      else
        DrawPixelLine(Canvas, cx, cy, ARect.Left + Indent, cy, ALineColor);
    end;

    if ANode.AbsoluteIndex <> 0 then
    begin
      DrawPixelLine(Canvas,cx - 1, ARect.Top, cx - 1,  cy - 1, ALineColor);
    end;

    if ((ANode.GetNextVisible <> nil) and (ANode.GetNextVisible.Level = ANode.Level))
       or (ANode.GetNextSibling <> nil) then
    begin
      if (ARect.Bottom - cy + 2) div 2 = (ARect.Bottom - cy + 2) / 2 then
        DrawPixelLine(Canvas,cx - 1, cy, cx - 1, ARect.Bottom + 1, ALineColor)
      else
        DrawPixelLine(Canvas,cx - 1, cy + 1, cx - 1, ARect.Bottom, ALineColor)
    end;
  end;

  if ANode.HasChildren and (FButtonImages = nil) and ShowButtons then
  with ACanvas do
  begin
    if FButtonStyle = scebsButton then
      R := Rect(cx - FButtonSize, cy - FButtonSize, cx + FButtonSize - 1, cy + FButtonSize - 1)
    else
      R := Rect(cx - FButtonSize, cy - FButtonSize, cx + FButtonSize, cy + FButtonSize);

    FillRect(R);

    if FButtonStyle = scebsButton then
    begin
      if FButtonColor = clNone then
        DrawTreeExpandImage(ACanvas, R, ANode.Expanded, ALineColor, FScaleFactor)
      else
        DrawTreeExpandImageColor(ACanvas, R, ANode.Expanded, FButtonColor, FButtonGlyphColor, FScaleFactor);
    end   
    else
    begin
      if FButtonColor = clNone then
      begin
        C := ALineColor;
        if not ANode.Expanded then
        begin
          C := MiddleColor(C, GetStyleColor(clWindow));
          if IsWindows10 then
            C := MiddleColor(C, ALineColor);
        end
        else
        if IsWindows10 then
          C := MiddleColor(C, GetStyleColor(clWindowText));
        C1 := MiddleColor(GetStyleColor(clBtnText), GetStyleColor(clBtnFace));
        if IsDarkStyle and not IsDarkWindowStyle
        then
          C1 := C
        else
          C1 := MiddleColor(GetStyleColor(clBtnText), C1);
      end
      else
      begin
        C := FButtonColor;
        C1 := FButtonColor;
      end;  
      if ANode.Expanded then
        DrawTreeArrowImage(ACanvas, R, C, C1, 2, FScaleFactor, (ButtonStyle = scebsModernArrow) or IsWindowsModernStyle)
      else
        DrawTreeArrowImage(ACanvas, R, C, C1, 1, FScaleFactor, (ButtonStyle = scebsModernArrow) or IsWindowsModernStyle);
    end;
  end
  else
  if ANode.HasChildren and (FButtonImages <> nil) and ShowButtons then
  begin
    R := Rect(cx - FButtonSize, cy - FButtonSize,
              cx + FButtonSize, cy + FButtonSize);
    if not ANode.Expanded then
    begin
      if (FButtonCollapseImageIndex >= 0) and
         (FButtonCollapseImageIndex < FButtonImages.Count)
      then
        ButtonImages.Draw(Canvas, cx - FButtonSize, cy - FButtonSize,
          ButtonCollapseImageIndex, True);
    end
    else
      begin
        if (FButtonExpandImageIndex >= 0) and
           (FButtonExpandImageIndex < FButtonImages.Count)
        then
          FButtonImages.Draw(Canvas, cx - FButtonSize, cy - FButtonSize,
            ButtonExpandImageIndex, True);
      end;
   end;

   if ShowLines then
   begin
     ANode := ANode.Parent;
     while ANode <> nil do
     begin
       cx := cx - Indent;
       if ANode.GetNextSibling <> nil then
       begin
         if (ARect.Bottom - ARect.Top) div 2 = (ARect.Bottom + 1 - ARect.Top) / 2 then
           DrawPixelLine(Canvas, cx - 1, ARect.Top, cx - 1,  ARect.Bottom + 1, ALineColor)
         else
           DrawPixelLine(Canvas, cx - 1, ARect.Top, cx - 1,  ARect.Bottom - 1, ALineColor);
       end;
       ANode := ANode.Parent;
     end;
   end;
end;

procedure TscCustomTreeView.DrawImage(ACanvas: TCanvas; ANodeRect: TRect; ANode: TTreeNode; AImageIndex: Integer);
var
  X, Y: Integer;
  R: TRect;
begin
  if Images = nil then
  begin
    FThirdImages := TreeView_GetImageList(Self.Handle, TVSIL_NORMAL);
    if FThirdImages <> 0 then
    begin
      ImageList_GetIconSize(FThirdImages, FThirdImageWidth, FThirdImageHeight);
      FThirdImagesCount := ImageList_GetImageCount(FThirdImages);
      if (AImageIndex >= 0) and (AImageIndex < FThirdImagesCount) then
      begin
        X := ANodeRect.Left - FButtonSize;
        if FCheckBoxes then
        begin
          Treeview_GetItemRect(Self.Handle, ANode.ItemId, R, True);
          X := X + (R.Left - X - FThirdImageWidth) div 2;
        end;
        Y := ANodeRect.Top + (ANodeRect.Bottom - ANodeRect.Top) div 2;
        ImageList_DrawEx(FThirdImages, AImageIndex, Canvas.Handle,
          X,  Y - FThirdImageHeight div 2,
          FThirdImageWidth, FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
      end;
     end;
    Exit;
  end;
  X := ANodeRect.Left - FButtonSize;
  if FCheckBoxes then
  begin
    Treeview_GetItemRect(Self.Handle, ANode.ItemId, R, True);
    X := X + (R.Left - X - Images.Width) div 2;
  end;
  Y := ANodeRect.Top + (ANodeRect.Bottom - ANodeRect.Top) div 2;
  if (Images <> nil) and (AImageIndex >= 0) and (AImageIndex < Images.Count) then
      Images.Draw(Canvas, X, Y - Images.Height div 2,
        AImageIndex, True);
end;

procedure TscCustomTreeView.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

{ TscListViewStyleHook }

constructor TscListViewStyleHook.Create;
var
  LColor: TColor;
begin
  inherited;
  OverrideEraseBkgnd := False;
  FHeaderInstance := MakeObjectInstance(HeaderWndProc);
  FHotSection := -1;
  FPrevHotSection := -1;
  FPressedSection := -1;
  FHeaderHandle := 0;
  with StyleServices do
  begin
    if not GetElementColor(GetElementDetails(ttItemNormal), ecTextColor, LColor) or
       (LColor = clNone) then
      LColor := GetSystemColor(clWindowText);
  end;
  {$IFNDEF VER230}
  if seFont in Control.StyleElements then
    FontColor := LColor
  else
    FontColor := TWinControlClass(Control).Font.Color;
  if seClient in Control.StyleElements then
    Brush.Color := StyleServices.GetStyleColor(scListView)
  else
    Brush.Color := TWinControlClass(Control).Color;
  {$ELSE}
    FontColor := LColor;
    Brush.Color := StyleServices.GetStyleColor(scListView);
  {$ENDIF}

end;

destructor TscListViewStyleHook.Destroy;
begin
  if FHeaderHandle <> 0 then
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, IntPtr(FDefHeaderProc));
  FreeObjectInstance(FHeaderInstance);
  inherited;
end;

procedure TscListViewStyleHook.CMSEPaint(var Message: TMessage);
begin
  inherited;
  PaintHeader(Message.WParam);
end;

procedure TscListViewStyleHook.WMNotify(var Message: TWMNotify);
var
  H: HWnd;
begin
  H := GetWindow(Handle, GW_CHILD);
  if (H <> 0) and (FHeaderHandle = 0) then
  begin
    FHeaderHandle := H;
    FDefHeaderProc := Pointer(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, IntPtr(FHeaderInstance));
  end;
end;

procedure TscListViewStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_ERASEBKGND:
      PaintScroll;
  end;
end;

procedure TscListViewStyleHook.HeaderWndProc(var Message: TMessage);
var
  Info: THDHitTestInfo;
begin
  with Message do
  begin
    case Msg of
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        begin
          Info.Point.X := TWMMouse(Message).XPos;
          Info.Point.Y := TWMMouse(Message).YPos;
          SendMessage(FHeaderHandle, HDM_HITTEST, 0, IntPtr(@Info));

          if (Info.Flags and HHT_ONDIVIDER = 0) and
             (Info.Flags and HHT_ONDIVOPEN = 0) then
            FPressedSection := Info.Item
          else
            FPressedSection := -1;
          RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
          FHeaderLBtnDown := True;
        end;
      WM_LBUTTONUP, WM_RBUTTONUP:
        begin
          FPressedSection := -1;
          RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
          FHeaderLBtnDown := False;
          PaintScroll;
        end;
      WM_MOUSEMOVE:
        if (FPressedSection = - 1) and FHeaderLBtnDown then
          PaintScroll
        else
        if FPressedSection = -1 then
        begin
          Info.Point.X := TWMMouse(Message).XPos;
          Info.Point.Y := TWMMouse(Message).YPos;
          SendMessage(FHeaderHandle, HDM_HITTEST, 0, IntPtr(@Info));
          if (Info.Flags and HHT_ONDIVIDER = 0) and
             (Info.Flags and HHT_ONDIVOPEN = 0) then
            FHotSection := Info.Item
          else
            FHotSection := -1;
          if FPrevHotSection <> FHotSection then
          begin
            FPrevHotSection := FHotSection;
            RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          end;
        end;
      WM_MOUSELEAVE:
      begin
        if FHotSection <> -1 then
        begin
          FHotSection := -1;
          FPrevHotSection := FHotSection;
          RedrawWindow(FHeaderHandle, nil, 0, RDW_INVALIDATE);
        end;
      end;
      WM_PAINT:
        begin
          PaintHeader(0);
          Exit;
        end;
      WM_ERASEBKGND:
        begin
          Result := 1;
          Exit;
        end;
      WM_NCDESTROY:
        begin
          Result := CallWindowProc(FDefHeaderProc, FHeaderHandle, Msg, WParam, LParam);
          FHeaderHandle := 0;
          FDefHeaderProc := nil;
          Exit;
        end;
    end;
    Result := CallWindowProc(FDefHeaderProc, FHeaderHandle, Msg, WParam, LParam);
  end;
end;

procedure TscListViewStyleHook.LVMSetBkColor(var Message: TMessage);
begin
  Message.LParam := LPARAM(ColorToRGB(Brush.Color));
  Handled := False;
end;

procedure TscListViewStyleHook.LVMSetTextBkColor(var Message: TMessage);
begin
  Message.LParam := LPARAM(ColorToRGB(Brush.Color));
  Handled := False;
end;

procedure TscListViewStyleHook.LVMSetTextColor(var Message: TMessage);
begin
  Message.LParam := LPARAM(ColorToRGB(FontColor));
  Handled := False;
end;

procedure TscListViewStyleHook.PaintHeader(DC: HDC);
var
  Canvas: TCanvas;
  R, HeaderR: TRect;
  PS: TPaintStruct;
  HeaderDC: HDC;
  I, ColumnIndex, RightOffset: Integer;
  SectionOrder: array of Integer;
  Item: THDItem;
  Buffer: array [0..255] of Char;
begin
  if DC = 0 then
    HeaderDC := BeginPaint(FHeaderHandle, PS)
  else
    HeaderDC := DC;
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := HeaderDC;
      RightOffset := 0;

      for I := 0 to Header_GetItemCount(FHeaderHandle) - 1 do
      begin
        SetLength(SectionOrder, Header_GetItemCount(FHeaderHandle));
        Header_GetOrderArray(FHeaderHandle, Header_GetItemCount(FHeaderHandle),
          Pointer(SectionOrder));
        ColumnIndex := SectionOrder[I];
        Header_GETITEMRECT(FHeaderHandle, ColumnIndex, @R);
        FillChar(Item, SizeOf(Item), 0);
        Item.Mask := HDI_TEXT;
        Item.pszText := @Buffer;
        Item.cchTextMax := Length(Buffer);
        Header_GetItem(FHeaderHandle, ColumnIndex, Item);
        DrawHeaderSection(Canvas, R, ColumnIndex, Item.pszText,
          FHotSection = ColumnIndex,
          FPressedSection = ColumnIndex, False);

        if RightOffset < R.Right then
          RightOffset := R.Right;
      end;

      GetWindowRect(FHeaderHandle, HeaderR);
      R := Rect(RightOffset, 0, HeaderR.Width + 2, HeaderR.Height);
      if not IsRectEmpty(R) then
        DrawHeaderSection(Canvas, R, -1, '', False, False, True);

      if DC <> 0 then
        ReleaseDC(FHeaderHandle, DC);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
  finally
    if DC = 0  then
      EndPaint(FHeaderHandle, PS)
  end;
end;

procedure TscListViewStyleHook.DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: Integer;
  const Text: string; IsHot, IsPressed, IsBackground: Boolean);
var
  Flags, TextWidth: Integer;
  Item: THDItem;
  ImageList: HIMAGELIST;
  DrawState: TThemedHeader;
  IconWidth, IconHeight: Integer;
  Details: TThemedElementDetails;
  TempR: TRect;
  FColor: TColor;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Mask := HDI_FORMAT;
  Header_GetItem(FHeaderHandle, Index, Item);
  if IsBackground then
    DrawState := thHeaderItemNormal
  else if IsPressed then
    DrawState := thHeaderItemPressed
  else
  if IsHot then
    DrawState := thHeaderItemHot
  else
    DrawState := thHeaderItemNormal;

  Details := StyleServices.GetElementDetails(DrawState);
  StyleServices.DrawElement(Canvas.Handle, Details, R);

  ImageList := SendMessage(FHeaderHandle, HDM_GETIMAGELIST, 0, 0);
  Item.Mask := HDI_FORMAT or HDI_IMAGE;
  InflateRect(R, -2, -2);
  Inc(R.Left, 3);
  Dec(R.Right, 3);
  IconWidth := 0;
  Flags := DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS;
  TempR := Rect(0, 0, 0, 0);
  DrawText(Canvas.Handle, PChar(Text), Length(Text),
    TempR, DT_SINGLELINE or DT_CALCRECT);

  TextWidth := TempR.Width;

  if Item.fmt and HDF_RIGHT = HDF_RIGHT then
    Flags := Flags or DT_RIGHT
  else
  if Item.fmt and HDF_CENTER = HDF_CENTER then
    Flags := Flags or DT_CENTER
  else
    Flags := Flags or DT_LEFT;

  if (ImageList <> 0) and Header_GetItem(FHeaderHandle, Index, Item) then
  begin
    if Item.fmt and HDF_IMAGE = HDF_IMAGE then
    begin
      ImageList_GetIconSize(ImageList, IconWidth, IconHeight);
      TempR := R;
      TempR.Top := R.Top + R.Height div 2 - IconHeight div 2;
      TempR.Bottom := TempR.Top + IconHeight;
      if Flags and DT_RIGHT = DT_RIGHT then
      begin
        TempR.Left := R.Right - TextWidth - IconWidth - 10;
        if TempR.Left < R.Left then
        begin
          TempR.Left := R.Left;
          Inc(R.Left, IconWidth + 10);
        end;
      end
      else
      if Flags and DT_CENTER = DT_CENTER then
      begin
        TempR.Left := R.Left + R.Width div 2 - (TextWidth + IconWidth + 10) div 2;
        if TempR.Left < R.Left then
        begin
          TempR.Left := R.Left;
          Inc(R.Left, IconWidth + 10)
        end
        else
          R.Left := TempR.Left + IconWidth + 10;
      end
      else
        Inc(R.Left, IconWidth + 10);
      ImageList_Draw(ImageList, Item.iImage, Canvas.Handle,
        TempR.Left, TempR.Top, ILD_TRANSPARENT);
    end;
  end;

  if IconWidth = 0 then Inc(R.Left, 4);

  if not StyleServices.GetElementColor(Details, ecTextColor, FColor) then
    FColor := clBtnText;
  if Canvas.Font <> TWinControlClass(Control).Font then
    Canvas.Font := TWinControlClass(Control).Font;
  SetTextColor(Canvas.Handle, FColor);
  SetBkMode(Canvas.Handle, Transparent);
  DrawText(Canvas.Handle, PChar(Text), Length(Text), R,
     DT_VCENTER or DT_LEFT or  DT_SINGLELINE or DT_END_ELLIPSIS);
end;

procedure TscListViewStyleHook.WMMouseMove(var Message: TWMMouse);
var
  SF: TScrollInfo;
  SPos: Integer;
  R: TRect;
  P1, P2: TPoint;
  TrackR: TRect;
begin
  if VertSliderState = tsThumbBtnVertPressed then
  begin
    TrackR := VertTrackRect;
    P1 := TrackR.TopLeft;
    P2 := TrackR.BottomRight;
    ClientToScreen(Handle, P1);
    ClientToScreen(Handle, P2);

    if (Mouse.CursorPos.Y < P1.Y) and (PrevScrollPos <= Mouse.CursorPos.Y) then
    begin
      PrevScrollPos := P1.Y;
      Exit;
    end;

    if (Mouse.CursorPos.Y > P2.Y) and (PrevScrollPos >= Mouse.CursorPos.Y) then
    begin
      PrevScrollPos := P2.Y;
      Exit;
    end;

    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.Y - PrevScrollPos) / TrackR.Height);

    PrevScrollPos := Mouse.CursorPos.Y;

    if Control is TCustomListView then
    begin
      PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(ScrollPos))), 0);
      if TscCustomListView(Control).ViewStyle = vsReport then
      begin
        if (Abs(ScrollPos - ListPos) >= 1) or
        ((ScrollPos = SF.nMin) and (ListPos <> ScrollPos)) or
        ((ScrollPos = SF.nMax) and (ListPos <> ScrollPos)) then
        begin
          if TscCustomListView(Control).GroupView then
          begin
            SPos := Round(ScrollPos - ListPos);
            if SF.nPos + SPos < 0 then SPos := -SF.nPos;
          end
          else
            begin
              ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
              SPos := Round((ScrollPos - ListPos) * R.Height);
            end;
          ListView_Scroll(Handle, 0, SPos);
          ListPos := ScrollPos;
          if Control.DoubleBuffered then UpdateWindow(Handle);
        end;
      end
      else
      begin
        if Abs(ScrollPos - ListPos) >= 1 then
        begin
          ListView_Scroll(Handle, 0, Round((ScrollPos - ListPos)));
          ListPos := ScrollPos;
        end;
      end;
    end
    else
      PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(ScrollPos))), 0);
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if HorzSliderState = tsThumbBtnHorzPressed then
  begin
    TrackR := HorzTrackRect;
    P1 := TrackR.TopLeft;
    P2 := TrackR.BottomRight;
    ClientToScreen(Handle, P1);
    ClientToScreen(Handle, P2);

    if (Mouse.CursorPos.X < P1.X) and (PrevScrollPos <= Mouse.CursorPos.X) then
    begin
      PrevScrollPos := P1.X;
      Exit;
    end;

    if (Mouse.CursorPos.X > P2.X) and (PrevScrollPos >= Mouse.CursorPos.X) then
    begin
      PrevScrollPos := P2.X;
      Exit;
    end;

    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.X - PrevScrollPos) / TrackR.Width);
    if ScrollPos < SF.nMin then
      ScrollPos := SF.nMin;
    if ScrollPos > SF.nMax then
      ScrollPos := SF.nMax;
    PrevScrollPos := Mouse.CursorPos.X;
    if TscCustomListView(Control).ViewStyle = vsReport then
    begin
      if Abs(ScrollPos - ListPos) >= 1 then
      begin
        ListView_Scroll(Handle, Round(ScrollPos - ListPos), 0);
        ListPos := ScrollPos;
        if Control.DoubleBuffered then
          RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
      end;
    end
    else
    begin
      if Abs(Round(ScrollPos - ListPos)) >= 1 then
      begin
        if TscCustomListView(Control).ViewStyle = vsList then
        begin
          ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
          if R.Width > 0 then
             ListView_Scroll(Handle, Round(ScrollPos - ListPos) * R.Width, 0)
          else
            ListView_Scroll(Handle, Round(ScrollPos - ListPos), 0);
        end
        else
          ListView_Scroll(Handle, Round(ScrollPos - ListPos), 0);
        ListPos := ScrollPos;
      end;
    end;
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if (HorzSliderState <> tsThumbBtnHorzPressed) and (HorzSliderState = tsThumbBtnHorzHot) then
  begin
    HorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
  end;

  if (VertSliderState <> tsThumbBtnVertPressed) and (VertSliderState = tsThumbBtnVertHot) then
  begin
    VertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
  end;

  if (HorzUpState <> tsArrowBtnLeftPressed) and (HorzUpState = tsArrowBtnLeftHot) then
  begin
    HorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if (HorzDownState <> tsArrowBtnRightPressed) and (HorzDownState =tsArrowBtnRightHot) then
  begin
    HorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  if (VertUpState <> tsArrowBtnUpPressed) and (VertUpState = tsArrowBtnUpHot) then
  begin
    VertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if (VertDownState <> tsArrowBtnDownPressed) and (VertDownState = tsArrowBtnDownHot) then
  begin
    VertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Message));
  if LeftButtonDown then
    PaintScroll;
  Handled := True;
end;

constructor TscCustomListView.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FFluentUIOpaque := False;
  FExtendedColumnDraw := False;
  FSelectionStyle := scstStyled;
  OnAdvancedCustomDrawItem := HookAdvancedCustomDrawItem;
  FShowFocusRect := True;
  FAlternateRow := False;
  FThirdImages := 0;
  FThirdImageWidth := 0;
  FThirdImageHeight := 0;
  FThirdImagesCount := 0;
  FGridLines := False;
  FDefaultDraw := False;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
end;

destructor TscCustomListView.Destroy;
begin
  inherited;
end;

function TscCustomListView.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscCustomListView.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TscCustomListView.SetExtendedColumnDraw(Value: Boolean);
begin
  if FExtendedColumnDraw <> Value then
  begin
    FExtendedColumnDraw := Value;
    Invalidate;
  end;
end;

procedure TscCustomListView.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscCustomListView.Loaded;
begin
  inherited;
  if IsWindowsXP and DoubleBuffered then DoubleBuffered := False;
end;

procedure TscCustomListView.FullRedraw;
begin
  RedrawWindow(Handle, nil, 0,
    RDW_FRAME or RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW);
end;

procedure TscCustomListView.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomListView.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomListView.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_COLORCHANGED:
      {$IFNDEF VER230}
      if (seClient in StyleElements) then
      {$ENDIF}
      begin
        Brush.Color := GetStyleColor(Color);
        Invalidate;
      end;
    CM_STYLECHANGED:
    if  not (csLoading in ComponentState) then
    begin
      if ViewStyle = vsReport then
        UpdateColumns;
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

procedure TscCustomListView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if IsFluentUIOpaque then
    Params.ExStyle := Params.Exstyle or WS_EX_LAYERED;
end;

procedure TscCustomListView.CreateWnd;
begin
  inherited;
  {$IFNDEF VER230}
  if (seClient in StyleElements) then
  {$ENDIF} 
  Brush.Color := GetStyleColor(Color);
  if IsCustomStyle or not FDefaultDraw then
    OnAdvancedCustomDrawItem := HookAdvancedCustomDrawItem
  else
    OnAdvancedCustomDrawItem := nil;
  if IsFluentUIOpaque then
  begin
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
      SetTimer(Handle, 101, 10, nil);
  end;
end;

function TscCustomListView.CanDrawFocusRect: Boolean;
begin
  Result := FShowFocusRect;
end;

procedure TscCustomListView.SetDefaultDraw(Value: Boolean);
begin
  if FDefaultDraw <> Value then
  begin
    FDefaultDraw := Value;
    if (csDesigning in ComponentState) or (not IsCustomStyle)  then
    begin
      if not DefaultDraw then
        OnAdvancedCustomDrawItem := HookAdvancedCustomDrawItem
      else
        OnAdvancedCustomDrawItem := nil;
      Invalidate;
    end;
  end;
end;

procedure TscCustomListView.SetGridLines(Value: Boolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    if ViewStyle = vsReport then
      Invalidate;
  end;
end;

procedure TscCustomListView.SetAlternateRow(Value: Boolean);
begin
  if FAlternateRow <> Value then
  begin
    FAlternateRow := Value;
    if Self.ViewStyle = vsReport then
      Invalidate;
  end;
end;

procedure TscCustomListView.HookAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
        State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);

var
  SelectBounds, TextBounds, IconBounds, ItemBounds, R, R1, CR, FR: TRect;
  IL: TCustomImageList;
  Buffer: TBitmap;
  S: String;
  w1, w2: Integer;
  CX, CY, I: Integer;
  FBGColor, FBGAlternateColor: TColor;
  FLineColor: TColor;
  SaveIndex: Integer;
  FFullSelection: Boolean;
  C: TColor;
  FCheckSize: Integer;
  FPercent: Integer;
begin
  if Stage <> cdPostPaint then Exit;

  DefaultDraw := False;

  FCheckSize := Round(13 * FScaleFactor);

  with Canvas do
  begin
    if {$IFNDEF VER230}(seFont in StyleElements) and {$ENDIF} IsCustomStyle then
      Font.Color := GetEditTextColor(scsNormal);
    if not Enabled then
    begin
      if IsCustomStyle then
        Font.Color := GetEditTextColor(scsDisabled)
      else
        Font.Color := clGrayText;
    end;
    if {$IFNDEF VER230}(seClient in StyleElements) and {$ENDIF} IsCustomStyle then
      Brush.Color := GetStyleColor(Self.Color)
    else
      Brush.Color := Color;
    FBGColor := Brush.Color;
    FBGAlternateColor := AlternateColor(Brush.Color);
    FLineColor := MiddleColor(Font.Color, FBGColor);
    FLineColor := MiddleColor(FLineColor, FBGColor);
    FLineColor := MiddleColor(FLineColor, FBGColor);
  end;
  if (SmallImages = nil) and (LargeImages = nil) then
  begin
    if ViewStyle = vsIcon  then
      FThirdImages := ListView_GetImageList(Self.Handle, LVSIL_NORMAL)
    else
      FThirdImages := ListView_GetImageList(Self.Handle, LVSIL_SMALL);

    if FThirdImages <> 0 then
    begin
      ImageList_GetIconSize(FThirdImages, FThirdImageWidth, FThirdImageHeight);
      FThirdImagesCount := ImageList_GetImageCount(FThirdImages);
    end;
  end;

 if (ViewStyle <> vsList)
  then
    TextBounds :=  Item.DisplayRect(drLabel)
  else
    begin
      TextBounds :=  Item.DisplayRect(drLabel);
      R := Rect(0, 0, 0, 0);
      DrawText(Canvas.Handle, PChar(Item.Caption), Length(Item.Caption), R,
        DT_CALCRECT or DT_LEFT or DT_NOPREFIX);
      TextBounds.Right := TextBounds.Left + R.Width + 2;
    end;

  IconBounds := Item.DisplayRect(drIcon);

  if ViewStyle = vsIcon
  then
    begin
      OffsetRect(TextBounds, 2, 0);
      IL := LargeImages;
    end
  else
    IL := SmallImages;
  ItemBounds := Item.DisplayRect(drBounds);
  Canvas.FillRect(ItemBounds);
  FFullSelection := False;

  if (ViewStyle = vsIcon)
  then
  begin
    if not IsCustomStyle and StyleServices.Enabled and not IsWindowsXP and
      (SelectionStyle <> scstColor)
    then
    begin
      SelectBounds := ItemBounds;
      FFullSelection := Item.Selected;
    end
    else
      SelectBounds := Item.DisplayRect(drLabel);
  end
  else
    begin
      SelectBounds := Item.DisplayRect(drSelectBounds);
      if (ViewStyle = vsList)
      then
        begin
          SelectBounds := TextBounds;
          if Item.Caption = ''
          then
            Inc(SelectBounds.Right, 20)
          else
            Inc(SelectBounds.Right, 4);
        end
      else
        SelectBounds.Left := TextBounds.Left;
    end;

  if Item.Selected and (SelectBounds.Width > 0) then
  begin
    if (ViewStyle = vsReport) and GridLines then
    begin
      Dec(SelectBounds.Bottom);
      Dec(SelectBounds.Right);
    end;
    Buffer := TBitmap.Create;
    Buffer.SetSize(SelectBounds.Width, SelectBounds.Height);
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    Buffer.Canvas.Font := Self.Font;
    if FSelectionStyle = scstColor
    then
    begin
      if FSelectionColor <> clNone then
      begin
        Buffer.Canvas.Font.Color := GetStyleColor(FSelectionTextColor);
        Buffer.Canvas.Brush.Color := GetStyleColor(FSelectionColor);
      end
      else
      begin
        Buffer.Canvas.Brush.Color := GetStyleColor(clHighLight);
        Buffer.Canvas.Font.Color := GetStyleColor(clHighLightText);
      end;
      if not Focused and not FShowFocusRect then
      begin
        C := Buffer.Canvas.Brush.Color;
        Buffer.Canvas.Brush.Color := FBGColor;
        Buffer.Canvas.FillRect(R);
        Buffer.Canvas.Brush.Color := C;
        FillRectWithAlpha(Buffer.Canvas, R, 200);
      end
      else
        Buffer.Canvas.FillRect(R);
    end
    else
    begin
      Buffer.Canvas.Brush.Color := Canvas.Brush.Color;
      Buffer.Canvas.Font.Color := GetSelectionTextColor;
      Buffer.Canvas.FillRect(R);
      if not MultiSelect then
        DrawSelection(Buffer.Canvas, R, Focused, FShowFocusRect)
      else
        DrawSelection(Buffer.Canvas, R, Focused and (cdsFocused in State), FShowFocusRect);
    end;
    if Focused and ((cdsFocused in State) or not MultiSelect) and CanDrawFocusRect then
    begin
      FR := R;
      if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
         and (FSelectionStyle = scstStyled) then
        InflateRect(FR, -1, -1);
      scDrawFocusRect(Buffer.Canvas, FR, FScaleFactor);
    end;
    Buffer.Canvas.Brush.Style := bsClear;
    R := TextBounds;
    OffsetRect(R, -SelectBounds.Left, -SelectBounds.Top);
    if (ViewStyle = vsReport) and GridLines then
      Dec(R.Bottom);
    S := Item.Caption;
    if ViewStyle = vsList then
      CorrectTextbyWidth(Canvas, S, TextBounds.Width)
    else
      CorrectTextbyWidth(Canvas, S, TextBounds.Width - 2);
    if IconOptions.WrapText and (ViewStyle = vsIcon) then
    begin
      Dec(R.Right, 4);
      DrawTextCenterMultiLine(Buffer.Canvas, Item.Caption, R, True);
    end
    else
      begin
        if ViewStyle = vsReport then
        begin
          if FExtendedColumnDraw and (Columns.Count > 0) and
            (Columns[0].Tag >= 1) and (Columns[0].Tag <= 2) then
          begin
            if Item.Caption <> '-' then
            case TscListViewColumnType(Column[0].Tag) of
              scctProgress:
                 begin
                   if IsIntPlusText(Item.Caption) then
                   begin
                     FPercent := StrToInt(Item.Caption);
                     if FPercent > 100 then
                        FPercent := 100;
                   end
                   else
                     FPercent := 0;
                   InflateRect(R, -2, -2);
                   scDrawUtils.DrawProgressBar(Buffer.Canvas, R, 0, 100, FPercent);
                 end;
              scctCheck:
                 begin
                   if Item.Caption = '1' then
                      scDrawUtils.DrawCheckBoxInCenter(Buffer.Canvas, R, scsNormal, cbChecked, FScaleFactor)
                   else
                     scDrawUtils.DrawCheckBoxInCenter(Buffer.Canvas, R, scsNormal, cbUnChecked, FScaleFactor);
                 end;
             end;
          end
          else
          begin
            Inc(R.Left, 3);
            DrawTextLeftMultiLine(Buffer.Canvas, Item.Caption, R);
          end;
         end
        else
        begin
          Inc(R.Left, 2);
          scDrawText(Buffer.Canvas, S, R, BidiMode = bdRightToLeft, True);
        end;
      end;

    if (ViewStyle = vsReport) and (Item.SubItems.Count > 0) and RowSelect then
    begin
      w2 := ListView_GetColumnWidth(Handle, 0);
      for I := 0 to Item.SubItems.Count - 1 do
        if Columns.Count > I + 1 then
        begin
          w1 := ListView_GetColumnWidth(Handle, I + 1);
          R := Rect(ItemBounds.Left + w2, 0, ItemBounds.Left + w1 + w2, Buffer.Height);
          OffsetRect(R, -SelectBounds.Left, 0);
          Inc(R.Left, 3);
          Dec(R.Right, 3);
          if (ViewStyle = vsReport) and GridLines then
            Dec(R.Bottom);
          if Assigned(FOnDrawSubItem)  then
             FOnDrawSubItem(Self, Item, I, State, Buffer.Canvas, R)
          else
            begin
              if not FExtendedColumnDraw or (Columns[i + 1].Tag = 0) or (Columns[i + 1].Tag > 2) then
              begin
                if (SmallImages <> nil) and (Item.SubItemImages[i] >= 0) and
                   (Item.SubItemImages[I] < SmallImages.Count) then
                begin
                  SmallImages.Draw(Buffer.Canvas, R.Left, R.Top, Item.SubItemImages[i], True);
                  Inc(R.Left, SmallImages.Width + 3);
                end;
                S := Item.SubItems[i];
                CorrectTextbyWidth(Buffer.Canvas, S, R.Width);
                Buffer.Canvas.Brush.Style := bsClear;
                DrawTextAlignmentNoPrefix(Buffer.Canvas, S, R, Columns[i + 1].Alignment, BidiMode = bdRightToLeft);
              end
              else
              begin
                S := Item.SubItems[i];
                if S <> '-' then
                case TscListViewColumnType(Column[i + 1].Tag) of
                  scctProgress:
                    begin
                      if IsIntPlusText(S) then
                      begin
                        FPercent := StrToInt(S);
                        if FPercent > 100 then
                          FPercent := 100;
                      end
                      else
                       FPercent := 0;
                      InflateRect(R, -2, -2);
                      Dec(R.Left);
                      if GridLines then
                        Inc(R.Bottom);
                      scDrawUtils.DrawProgressBar(Buffer.Canvas, R, 0, 100, FPercent);
                    end;
                  scctCheck:
                    begin
                      if S = '1' then
                        scDrawUtils.DrawCheckBoxInCenter(Buffer.Canvas, R, scsNormal, cbChecked, FScaleFactor)
                      else
                        scDrawUtils.DrawCheckBoxInCenter(Buffer.Canvas, R, scsNormal, cbUnChecked, FScaleFactor);
                   end;
                end;
            end;
            end;
          w2 := w2 + w1;
        end;
    end;
    if FAlternateRow and (ViewStyle = vsReport) then
    begin
      R1 := ItemBounds;
      R1.Right := Self.Width;
      if Odd(Item.Index) then
        Canvas.Brush.Color := FBGAlternateColor
      else
        Canvas.Brush.Color := FBGColor;
     Canvas.FillRect(R1);
    end;
    Canvas.Draw(SelectBounds.Left, SelectBounds.Top, Buffer);
    Buffer.Free;
  end
  else
  begin
    R := ItemBounds;
    Dec(R.Bottom);
    R1 := R;

    SaveIndex := SaveDC(Canvas.Handle);
    try
     if FAlternateRow and (ViewStyle = vsReport) then
     begin
       if not GridLines then
         Inc(R1.Bottom);

       R1.Right := Self.Width;
       if Odd(Item.Index) then
         Canvas.Brush.Color := FBGAlternateColor
       else
         Canvas.Brush.Color := FBGColor;
      end;
      Canvas.FillRect(R1);
      SetBkMode(Canvas.Handle, Transparent);
      if IconOptions.WrapText and (ViewStyle = vsIcon) then
      begin
        R := TextBounds;
        Dec(R.Right, 4);
        DrawTextCenterMultiLine(Canvas, Item.Caption, R, False);
      end
      else
      begin
        R := TextBounds;
        if ViewStyle = vsReport then
        begin
          if FExtendedColumnDraw and (Columns.Count > 0) and
            (Columns[0].Tag >= 1) and (Columns[0].Tag <= 2) then
          begin
            if Item.Caption <> '-' then
            case TscListViewColumnType(Column[0].Tag) of
              scctProgress:
                 begin
                   if IsIntPlusText(Item.Caption) then
                   begin
                     FPercent := StrToInt(Item.Caption);
                     if FPercent > 100 then
                        FPercent := 100;
                   end
                   else
                     FPercent := 0;
                   InflateRect(R, -2, -2);
                   Dec(R.Left);
                   scDrawUtils.DrawProgressBar(Canvas, R, 0, 100, FPercent);
                 end;
              scctCheck:
                 begin
                   if Item.Caption = '1' then
                      scDrawUtils.DrawCheckBoxInCenter(Canvas, R, scsNormal, cbChecked, FScaleFactor)
                   else
                     scDrawUtils.DrawCheckBoxInCenter(Canvas, R, scsNormal, cbUnChecked, FScaleFactor);
                 end;
             end;
          end
          else
          begin
            Inc(R.Left, 3);
            if (ViewStyle = vsReport) and GridLines then
              Dec(R.Bottom);
            DrawTextLeftMultiLine(Canvas, Item.Caption, R);
          end;
        end
        else
        begin
          S := Item.Caption;
          CorrectTextbyWidth(Canvas, S, TextBounds.Width - 2);
          Inc(R.Left, 2);
          scDrawText(Canvas, S, R, BidiMode = bdRightToLeft, True);
        end;
      end;
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
   end;

  if Checkboxes then
    case ViewStyle of
      vsIcon:
      begin
        if not IsWindowsXP then
          OffsetRect(IconBounds, FCheckSize div 2 + 1, 0);
      end;
      vsSmallIcon:
      begin
        if not IsWindowsXP then
          OffsetRect(IconBounds, FCheckSize + 3, 0);
      end;
    end;

  CX := 0;

  if Assigned(FOnDrawItemImage) then
  begin
    FOnDrawItemImage(Item, Canvas, IconBounds);
  end
  else
  if FThirdImages <> 0 then
  begin
    if ViewStyle = vsIcon then
    begin
      CX := IconBounds.Left + IconBounds.Width div 2 - FThirdImageWidth div 2;
      CY := IconBounds.Top + 2;
      if not FFullSelection then
        Canvas.FillRect(Rect(CX, CY, CX + FThirdImageWidth, CY + FThirdImageHeight));
      if (Item.ImageIndex >= 0) and (Item.ImageIndex < FThirdImagesCount) then
        ImageList_DrawEx(FThirdImages, Item.ImageIndex, Canvas.Handle,
       CX,  CY, FThirdImageWidth, FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
      if CheckBoxes then
         IconBounds.Left := CX;
    end
    else
      begin
        if not FFullSelection then
          Canvas.FillRect(Rect(IconBounds.Left, IconBounds.Top,
            IconBounds.Left + FThirdImageWidth, IconBounds.Top + FThirdImageHeight));
        ImageList_DrawEx(FThirdImages, Item.ImageIndex, Canvas.Handle,
        IconBounds.Left, IconBounds.Top,
        FThirdImageWidth, FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
      end;
   end
   else
   if (IL <> nil) and (Item.ImageIndex >= 0) and
      (Item.ImageIndex < IL.Count) then
   begin
     if ViewStyle = vsIcon then
     begin
       CX := IconBounds.Left + IconBounds.Width div 2 - IL.Width div 2;
       CY := IconBounds.Top + 2;
       IL.Draw(Canvas, CX, CY, Item.ImageIndex, True);
       if CheckBoxes then  IconBounds.Left := CX;
     end
     else
     begin
       CX := IconBounds.Left;
       CY := IconBounds.Top + IconBounds.Height div 2 - IL.Height div 2;
       IL.Draw(Canvas, CX, CY, Item.ImageIndex, True);
     end;
   end;

  if Checkboxes then
  begin
    if ViewStyle = vsIcon then
    begin
      if CX <> 0 then
      begin
        if IsWindowsXP then
          CR := Rect(CX - FCheckSize - 3, IconBounds.Bottom - FCheckSize - 3,
            CX - 3, IconBounds.Bottom)
        else
          CR := Rect(CX - FCheckSize - 3,
            IconBounds.Top, CX - 3, IconBounds.Bottom);
      end
      else
      begin
        CR := ItemBounds;
        CR.Bottom := TextBounds.Top;
        CR.Left := CR.Left + CR.Width div 2 - FCheckSize - 2;
        CR.Right := CR.Left + FCheckSize + 3;
      end;
    end
    else
      CR := Rect(IconBounds.Left - FCheckSize - 2, ItemBounds.Top, IconBounds.Left - 1, ItemBounds.Bottom);
    if ViewStyle = vsReport then
    begin
      OffsetRect(CR, -2, 0);
      if CR.Left < 0 then
        CR.Left := 0;
      if Item.Checked then
        DrawCheckBoxInCenter(Canvas, CR, scsNormal, cbChecked, FScaleFactor)
      else
        DrawCheckBoxInCenter(Canvas, CR, scsNormal, cbUnChecked, FScaleFactor);
    end
    else
    begin
      if Item.Checked then
        DrawCheckBox(Canvas, CR, scsNormal, cbChecked, FScaleFactor)
      else
        DrawCheckBox(Canvas, CR, scsNormal, cbUnChecked, FScaleFactor);
    end;
  end;

  if (ViewStyle = vsReport) and (Item.SubItems.Count > 0) and (not Item.Selected or not RowSelect) then
  begin
    w2 := ListView_GetColumnWidth(Handle, 0);
    for i := 0 to Item.SubItems.Count -1 do
      if Columns.Count > i + 1 then
      begin
        w1 := ListView_GetColumnWidth(Handle, i + 1);
        R := Rect(ItemBounds.Left + w2, ItemBounds.Top, ItemBounds.Left + w1 + w2, ItemBounds.Bottom);
        CR := R;
        Canvas.FillRect(R);
        Inc(R.Left, 3);
        Dec(R.Right, 3);
        if (ViewStyle = vsReport) and GridLines then
            Dec(R.Bottom);
        if Assigned(FOnDrawSubItem) then
          FOnDrawSubItem(Self, Item, i, State, Canvas, R)
        else
          begin
            if not FExtendedColumnDraw or (Columns[i + 1].Tag = 0) or (Columns[i + 1].Tag > 2) then
            begin
              if (SmallImages <> nil) and
                 (Item.SubItemImages[i] >= 0) and
                 (Item.SubItemImages[i] < SmallImages.Count) then
              begin
                SmallImages.Draw(Canvas, R.Left, R.Top, Item.SubItemImages[i], True);
                Inc(R.Left, SmallImages.Width + 3);
              end;
              S := Item.SubItems[i];
              CorrectTextbyWidth(Canvas, S, R.Width);
              SaveIndex := SaveDC(Canvas.Handle);
              try
                SetBkMode(Canvas.Handle, Transparent);
                DrawTextAlignmentNoPrefix(Canvas, S, R, Columns[i + 1].Alignment, BidiMode = bdRightToLeft);
              finally
                RestoreDC(Canvas.Handle, SaveIndex);
              end;
            end
            else
            begin
              S := Item.SubItems[i];
              if S <> '-' then
              case TscListViewColumnType(Column[i + 1].Tag) of
                scctProgress:
                  begin
                    if IsIntPlusText(S) then
                    begin
                      FPercent := StrToInt(S);
                      if FPercent > 100 then
                        FPercent := 100;
                    end
                    else
                      FPercent := 0;
                    InflateRect(R, -2, -2);
                    Dec(R.Left);
                    scDrawUtils.DrawProgressBar(Canvas, R, 0, 100, FPercent);
                  end;
                scctCheck:
                  begin
                    if S = '1' then
                      scDrawUtils.DrawCheckBoxInCenter(Canvas, R, scsNormal, cbChecked, FScaleFactor)
                    else
                      scDrawUtils.DrawCheckBoxInCenter(Canvas, R, scsNormal, cbUnChecked, FScaleFactor);
                  end;
              end;
            end;
          end;
        w2 := w2 + w1;
      end;
  end;

  w2 := 0;

  if (ViewStyle = vsReport) and (Columns.Count > 0) and FGridLines then
    for i := 0 to Columns.Count - 1 do
    begin
      w1 := ListView_GetColumnWidth(Handle, i);
      Canvas.Pen.Color := FLineColor;
      Canvas.MoveTo(ItemBounds.Left + w1 + w2 - 1, ItemBounds.Top);
      Canvas.LineTo(ItemBounds.Left + w1 + w2 - 1, ItemBounds.Bottom);
      w2 := w2 + w1;
    end;

  if (ViewStyle = vsReport) and FGridLines then
  begin
    R := ItemBounds;
    R.Right := Width;
    Dec(R.Bottom);
    Canvas.Pen.Color := FLineColor;
    Canvas.MoveTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Right, R.Bottom);
    if Item.Index = 0 then
    begin
      Canvas.MoveTo(R.Left, R.Top - 1);
      Canvas.LineTo(R.Right, R.Top - 1);
    end;
  end;

end;

procedure TscCustomListView.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

constructor TscPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csDoubleClicks, csReplicatable, csPannable, csGestures];
  FCanEmpty := True;
  FFluentUIOpaque := False;
  FDown := False;
  FPopupMode := False;
  FDragForm := False;
  FDragTopForm := True;
  FGlowBuffer := nil;
  FStoredCaption := '';
  FStoredGlowColor := clNone;
  FStoredFontSize := 0;
  FStoredFontStyle := [];
  FUpdateGlowBuffer := True;
  FStoredFontName := '';
  FCaptionGlowEffect := TscGlowEffect.Create;
  FCaptionGlowEffect.OnChange := OnGlowEffectChange;
  FCaptionGlowEffect.Color := clBtnShadow;
  FTransparentBackground := False;
  FAlignment := taCenter;
  FWallpapers := nil;
  FCustomImages := nil;
  FCustomImageIndex := -1;
  FWallpaperIndex := -1;
  Color := clBtnFace;
  FLightBorderColor := clBtnHighLight;
  FShadowBorderColor := clBtnShadow;
  FShowCaption := False;
end;

destructor TscPanel.Destroy;
begin
  FCaptionGlowEffect.Free;
  if FGlowBuffer <> nil then
    FGlowBuffer.Free;
  inherited;
end;

procedure TscPanel.DrawAdditionalContent(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; var ADrawCaption: Boolean);
begin
end;

function TscPanel.CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
begin
  Result := FUpdateGlowBuffer;
  if not Result then
    Result := (FStoredCaption <> Caption) or
              (FStoredGlowColor <> FCaptionGlowEffect.Color) or
              (FStoredFontName <> ACanvas.Font.Name) or
              (FStoredFontSize <> ACanvas.Font.Size) or
              (FStoredFontStyle <> ACanvas.Font.Style);
  FStoredCaption := Caption;
  FStoredGlowColor := FCaptionGlowEffect.Color;
  FStoredFontName := ACanvas.Font.Name;
  FStoredFontSize := ACanvas.Font.Size;
  FStoredFontStyle := ACanvas.Font.Style;
  FUpdateGlowBuffer := False;
end;

procedure TscPanel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if FShowCaption then RePaintControl;
end;

procedure TscPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscPanel.OnGlowEffectChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) and FShowCaption then
  begin
    RePaintControl;
  end;
end;

procedure TscPanel.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    RePaintControl;
  end;
end;

procedure TscPanel.SetLightBorderColor(Value: TColor);
begin
  if FLightBorderColor <> Value then
  begin
    FLightBorderColor := Value;
    RePaintControl;
  end;
end;

procedure TscPanel.SetShadowBorderColor(Value: TColor);
begin
  if FShadowBorderColor <> Value then
  begin
    FShadowBorderColor := Value;
    RePaintControl;
  end;
end;

procedure TscPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if FStyleKind = scpsEdit then
  begin
    InflateRect(Rect, -2, -2);
  end
  else
  if FStyleKind in [scpsPanel, scpsFormBackground,
    scpsTransparent] then
  case FBorderStyle of
    scpbsLoweredBevel,
    scpbsRaisedBevel:
    begin
      InflateRect(Rect, -2, -2);
    end;
    scpbsFlat, scpbsRaised, scpbsLowered:
    begin
      InflateRect(Rect, -1, -1);
    end;
    scpbsTopBevel:
    begin
      Inc(Rect.Top, 2)
    end;
    scpbsBottomBevel:
    begin
      Dec(Rect.Bottom, 2);
    end;
    scpbsTopLightLine, scpbsTopShadowLine:
    begin
      Inc(Rect.Top, 1);
    end;
    scpbsBottomLightLine, scpbsBottomShadowLine:
    begin
      Dec(Rect.Bottom, 1);
    end;
    scpbsLeftBevel:
    begin
      Inc(Rect.Left, 2);
    end;
    scpbsRightBevel:
    begin
     Dec(Rect.Right, 2);
    end;
    scpbsLeftLightLine, scpbsLeftShadowLine:
    begin
      Inc(Rect.Left, 1);
    end;
    scpbsRightLightLine, scpbsRightShadowLine:
    begin
      Dec(Rect.Right, 1);
    end;
  end;
end;

procedure TscPanel.SetBorderStyle(Value: TscPanelBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscPanel.SetStyleKind(Value: TscPanelStyleKind);
begin
  if FStyleKind <> Value then
  begin
    if FStyleKind = scpsTransparent then
    begin
      FTransparentBackground := False;
      GetParentBG;
    end;
    FStyleKind := Value;
    if FStyleKind = scpsTransparent then
    begin
      FTransparentBackground := True;
      GetParentBG;
    end;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if FDragForm and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    FForm := GetParentForm(Self, FDragTopForm);
    if (FForm <> nil) and (FForm.WindowState <> wsMaximized) then
    begin
      FDown := True;
      GetCursorPos(FDownP);
    end;
  end;
end;

procedure TscPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  FDown := False;
  FForm := nil;
end;

procedure TscPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  L, T: Integer;
  P: TPoint;
begin
  inherited;
  if FDragForm and FDown and (FForm <> nil) then
  begin
    GetCursorPos(P);
    L := FForm.Left + (P.X - FDownP.X);
    T := FForm.Top + (P.Y - FDownP.Y);
    FForm.SetBounds(L, T, FForm.Width, FForm.Height);
    FDownP.X := P.X;
    FDownP.Y := P.Y;
  end;
end;

procedure TscPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not
      (CS_HREDRAW or CS_VREDRAW);
end;

function TscPanel.GetCaptionColor: TColor;
begin
  Result := Self.Font.Color;
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    Result := GetCheckBoxTextColor(CtrlState);
    case FStyleKind of
      scpsHeader:
        Result := GetHeaderTextColor(scsNormal);
      scpsEdit:
        Result := scDrawUtils.GetEditTextColor(scsNormal);
    end;
  end;
end;

function TscPanel.GetCaptionText: String;
begin
  Result := Caption;
end;

procedure TscPanel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);

procedure DrawBorder;
var
  LColor, SColor: TColor;
  R: TRect;
begin
  if (seClient in StyleElements) and IsCustomStyle then
  begin
    LColor := GetStyleColor(FLightBorderColor);
    SColor := GetStyleColor(FShadowBorderColor);
  end
  else
  begin
    LColor := FLightBorderColor;
    SColor := FShadowBorderColor;
  end;
  R := Rect(0, 0, Width, Height);
  case FBorderStyle of
    scpbsLoweredBevel:
    begin
      Frame3D(ACanvas, R, SColor, LColor, 1);
      Frame3D(ACanvas, R, LColor, SColor, 1);
    end;
    scpbsRaisedBevel:
    begin
      Frame3D(ACanvas, R, LColor, SColor, 1);
      Frame3D(ACanvas, R, SColor, LColor, 1);
    end;
    scpbsFlat:
    begin
      Frame3D(ACanvas, R, SColor, SColor, 1);
    end;
    scpbsRaised:
    begin
      Frame3D(ACanvas, R, LColor, SColor, 1);
    end;
    scpbsLowered:
    begin
      Frame3D(ACanvas, R, SColor, LColor, 1);
    end;
    scpbsTopBevel:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Top);
      Pen.Color := LColor;
      MoveTo(R.Left, R.Top + 1);
      LineTo(R.Right, R.Top + 1);
    end;
    scpbsBottomBevel:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Left, R.Bottom - 2);
      LineTo(R.Right, R.Bottom - 2);
      Pen.Color := LColor;
      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;
    scpbsTopLightLine:
    with ACanvas do
    begin
      Pen.Color := LColor;
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Top);
    end;
    scpbsTopShadowLine:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Top);
    end;
    scpbsBottomLightLine:
    with ACanvas do
    begin
      Pen.Color := LColor;
      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;
    scpbsBottomShadowLine:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;
    scpbsLeftBevel:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
      Pen.Color := LColor;
      MoveTo(R.Left + 1, R.Top);
      LineTo(R.Left + 1, R.Bottom);
    end;
    scpbsRightBevel:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Right - 2, R.Top);
      LineTo(R.Right - 2, R.Bottom);
      Pen.Color := LColor;
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right - 1, R.Bottom);
    end;
    scpbsLeftLightLine:
    with ACanvas do
    begin
      Pen.Color := LColor;
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
    end;
    scpbsRightLightLine:
    with ACanvas do
    begin
      Pen.Color := LColor;
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right - 1, R.Bottom);
    end;
    scpbsLeftShadowLine:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
    end;
    scpbsRightShadowLine:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right - 1, R.Bottom);
    end;
    scpbsHorzCenterBevel:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.Left, R.CenterPoint.Y - 1);
      LineTo(R.Right, R.CenterPoint.Y - 1);
      Pen.Color := LColor;
      MoveTo(R.Left, R.CenterPoint.Y);
      LineTo(R.Right, R.CenterPoint.Y);
    end;
    scpbsVertCenterBevel:
    with ACanvas do
    begin
      Pen.Color := SColor;
      MoveTo(R.CenterPoint.X - 1, R.Top);
      LineTo(R.CenterPoint.X - 1, R.Bottom);
      Pen.Color := LColor;
      MoveTo(R.CenterPoint.X, R.Top);
      LineTo(R.CenterPoint.X, R.Bottom);
    end;
  end;
end;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  R, R1, TR: TRect;
  FUpdateGlow: Boolean;
  S: String;
  Flags: Longint;
  SaveIndex: Integer;
  LDrawCaption: Boolean;
begin
  R := Rect(0, 0, Width, Height);
  if (FWallpapers <> nil) and FWallpapers.IsSolidDrawing(FWallpaperIndex) then
  begin
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
    if (FCustomImages <> nil) and (FCustomImages.IsIndexAvailable(FCustomImageIndex))
    then
      FCustomImages.Draw(ACanvas, R, FCustomImageIndex, FScaleFactor);
    DrawBorder;
  end
  else
  begin
  case FStyleKind of
    scpsTabSheet:
      begin
        R1 := R;
        InflateRect(R1, 4, 4);
        scDrawUtils.DrawTabFrame(ACanvas, R1);
      end;
    scpsEdit:
      begin
        if FPopupMode then
        with ACanvas do
        begin
          if (seClient in StyleElements) and IsCustomStyle then
            Brush.Color := GetStyleColor(clWindow)
          else
            Brush.Color := clWindow;
          Brush.Style := bsSolid;
          FillRect(R);
          DrawBorder;
        end
        else
        begin
          with ACanvas do
          begin
            Brush.Color := GetStyleColor(clBtnFace);
            Brush.Style := bsSolid;
            FillRect(R);
          end;
          SaveIndex := SaveDC(ACanvas.Handle);
          DrawEditBorder(ACanvas,R, scsNormal);
          RestoreDC(ACanvas.Handle, SaveIndex);
          InflateRect(R, -2, -2);
          with ACanvas do
          begin
            Brush.Color := scDrawUtils.GetEditBrushColor(scsNormal);
            Brush.Style := bsSolid;
            FillRect(R);
          end;
        end;
      end;
    scpsPanel, scpsEmpty:
      with ACanvas do
      begin
        if (seClient in StyleElements) and IsCustomStyle then
          Brush.Color := GetStyleColor(Self.Color)
        else
          Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(R);
        DrawBorder;
      end;
    scpsToolBar, scpsHeader:
      begin
        SaveIndex := SaveDC(ACanvas.Handle);
        if IsCustomStyle then
          with ACanvas do
          begin
            Brush.Color := GetStyleColor(clBtnFace);
            Brush.Style := bsSolid;
            FillRect(R);
          end;
        if FStyleKind = scpsToolBar then
          DrawToolBar(ACanvas, R)
        else
          DrawHeaderSection(ACanvas, R, scsNormal);
        RestoreDC(ACanvas.Handle, SaveIndex);
        DrawBorder;
      end;
    scpsFormBackground:
    begin
      SaveIndex := SaveDC(ACanvas.Handle);
      DrawFormBackground(ACanvas, R);
      RestoreDC(ACanvas.Handle, SaveIndex);
      DrawBorder;
    end;
    scpsTransparent:
    begin
      DrawBorder;
    end;
  end;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomImageIndex) then
  begin
    FCustomImages.Draw(ACanvas, R, FCustomImageIndex, FScaleFactor);
    if (FStyleKind <> scpsHeader) and (FStyleKind <> scpsEdit) then
      DrawBorder;
  end;
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
  begin
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
    if (FStyleKind <> scpsHeader) and (FStyleKind <> scpsEdit) then
      DrawBorder;
  end;
  // for inherired controls
  R1 := R;
  if Self.BorderStyle <> scpbsNone then
    InflateRect(R1, -2, -2);
  LDrawCaption := False;
  DrawAdditionalContent(ACanvas, R1, ACtrlState, LDrawCaption);
  // custom paint event
  if Assigned(FOnPanelPaint) then
     FOnPanelPaint(ACanvas, R1);
  //
  if FShowCaption and (GetCaptionText <> '') and not LDrawCaption then
  begin
    S := GetCaptionText;
    InflateRect(R, -3, -3);
    ACanvas.Font := Self.Font;
    ACanvas.Font.Color := GetCaptionColor;
    ACanvas.Brush.Style := bsClear;
    Flags := DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or Alignments[FAlignment];
    if FCaptionGlowEffect.Enabled then
    begin
     FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
     R1 := Rect(0, 0, 0, 0);
     DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R1, DT_LEFT or
        DT_CALCRECT or DT_NOPREFIX);
     R1.Right := R1.Right + FCaptionGlowEffect.GlowSize + FCaptionGlowEffect.Offset;
     R1.Bottom := R1.Bottom + FCaptionGlowEffect.GlowSize + FCaptionGlowEffect.Offset;
     TR := R1;
     case Self.FAlignment of
       taLeftJustify:
       begin
         TR := Rect(R.Left, R.Top + R.Height div 2 - R1.Height div 2,
           R.Left + R1.Width, R.Top + R.Height div 2 - R1.Height div 2 + R1.Height);
       end;
       taRightJustify:
       begin
         TR := Rect(R.Right - R1.Width, R.Top + R.Height div 2 - R1.Height div 2,
           R.Right, R.Top + R.Height div 2 - R1.Height div 2 + R1.Height);
       end;
       taCenter:
       begin
         TR := Rect(R.Left + R.Width div 2 - R1.Width div 2, R.Top + R.Height div 2 - R1.Height div 2,
           R.Left + R.Width div 2 - R1.Width div 2 + R1.Width,
           R.Top + R.Height div 2 - R1.Height div 2 + R1.Height);
       end;
     end;
     DrawTextWithGlowBuffer(FGlowBuffer, FUpdateGlow,
      ACanvas, TR, S, DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_CENTER,
        FCaptionGlowEffect.Offset, FCaptionGlowEffect.Color, FCaptionGlowEffect.GlowSize,
           FCaptionGlowEffect.Intensive, FCaptionGlowEffect.AlphaValue, IsRightToLeft, True);
    end
    else
      begin
        DrawText(ACanvas.Handle,
          PChar(S), Length(S), R, Flags);
      end;
    end;
  end;
end;

procedure TscPanel.Paint;
begin
  if (FStyleKind = scpsEmpty) and FCanEmpty then Exit;
  inherited;
end;

procedure TscPanel.SetCustomImageIndex(Value: Integer);
begin
  if FCustomImageIndex <> Value then
  begin
    FCustomImageIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscPanel.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscPanel.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscPanel.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscPanel.CustomUpdateControl;
begin

end;

procedure TscPanel.WMSIZE(var Msg: TMessage);
begin
  inherited;
  if not FStorePaintBuffer then
    RePaint;
  if not FTransparentBackground and not FStorePaintBuffer then
    if ((FWallpapers <> nil) and FWallpapers.NeedFullUpdate(FWallpaperIndex)) or
       ((FCustomImages <> nil) and FCustomImages.NeedFullUpdate(FCustomImageIndex))
    then
      UpdateControls
    else
      CustomUpdateControl;
end;

constructor TscLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FVertAlignment := scvtaTop;
  FUseFontColorToStyleColor := False;
  FDragForm := False;
  FDragTopForm := True;
  FGlowBuffer := nil;
  FStoredCaption := '';
  FStoredWidth := 0;
  FStoredHeight := 0;
  FStoredGlowColor := clNone;
  FStoredFontSize := 0;
  FStoredFontStyle := [];
  FUpdateGlowBuffer := True;
  FStoredFontName := '';
  FStoredAlignment := taLeftJustify;
  FStoredShowAccelChar := False;
  FStoredLayout := tlCenter;
  FStoredWordWrap := False;
  FStoredShowEllipsis := False;
  FShowEllipsis := False;
  FForm := nil;
  FDown := False;
  Width := 65;
  Height := 17;
  FTransparentBackground := True;
  FGlowEffect := TscGlowEffect.Create;
  FGlowEffect.OnChange := OnGlowEffectChange;
  FGlowEffect.Color := clBtnShadow;
  FAutoSize := True;
  FShowAccelChar := True;
end;

destructor TscLabel.Destroy;
begin
  if FGlowBuffer <> nil then
    FGlowBuffer.Free;
  FGlowEffect.Free;
  inherited;
end;

procedure TscLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  if FDragForm and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    FForm := GetParentForm(Self, FDragTopForm);
    if (FForm <> nil) and (FForm.WindowState <> wsMaximized) then
    begin
      FDown := True;
      GetCursorPos(FDownP);
    end;
  end;
end;

procedure TscLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  inherited;
  FDown := False;
  FForm := nil;
end;

procedure TscLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  L, T: Integer;
  P: TPoint;
begin
  inherited;
  if FDragForm and FDown and (FForm <> nil) then
  begin
    GetCursorPos(P);
    L := FForm.Left + (P.X - FDownP.X);
    T := FForm.Top + (P.Y - FDownP.Y);
    FForm.SetBounds(L, T, FForm.Width, FForm.Height);
    FDownP.X := P.X;
    FDownP.Y := P.Y;
  end;
end;

procedure TscLabel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  if FAutoSize then
    AdjustBounds;
end;

function TscLabel.CanUpdateGlowBuffer(ACanvas: TCanvas): Boolean;
begin
  Result := FUpdateGlowBuffer;
  if not Result then
    Result := (FStoredCaption <> Caption) or
              (FStoredWidth <> Width) or
              (FStoredHeight <> Height) or
              (FStoredGlowColor <> FGlowEffect.Color) or
              (FStoredFontName <> ACanvas.Font.Name) or
              (FStoredFontSize <> ACanvas.Font.Size) or
              (FStoredFontStyle <> ACanvas.Font.Style) or
              (FStoredAlignment <> FAlignment) or
              (FStoredShowAccelChar <> FShowAccelChar) or
              (FStoredLayout <> FLayout) or
              (FStoredWordWrap <> FWordWrap) or
              (FStoredShowEllipsis <> FShowEllipsis);

  FStoredCaption := Caption;
  FStoredWidth := Width;
  FStoredHeight := Height;
  FStoredGlowColor := FGlowEffect.Color;
  FStoredFontName := ACanvas.Font.Name;
  FStoredFontSize := ACanvas.Font.Size;
  FStoredFontStyle := ACanvas.Font.Style;
  FStoredAlignment := FAlignment;
  FStoredShowAccelChar := FShowAccelChar;
  FStoredLayout := FLayout;
  FStoredWordWrap := FWordWrap;
  FStoredShowEllipsis := FShowEllipsis;
  FUpdateGlowBuffer := False;
end;

procedure TscLabel.OnGlowEffectChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    FUpdateGlowBuffer := True;
    RePaintControl;
    AdjustBounds;
  end;
end;

function TscLabel.GetLabelText: string;
begin
  Result := Caption;
end;

procedure TscLabel.DoDrawText(ACanvas: TCanvas; var ARect: TRect; AFlags: Longint);
var
  S: string;
begin
  S := GetLabelText;
  if (AFlags and DT_CALCRECT <> 0) and ((S = '') or ShowAccelChar and
    (S[1] = '&') and (S[2] = #0)) then S := S + ' ';
  if not ShowAccelChar then AFlags := AFlags or DT_NOPREFIX;
//  AFlags := DrawTextBiDiModeFlags(AFlags);
  DrawText(ACanvas.Handle, PChar(S), Length(S), ARect, AFlags);
end;

procedure TscLabel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  S: string;
  Flags: Longint;
  R, R1: TRect;
  FUpdateGlow: Boolean;
begin
  ACanvas.Font.Assign(Font);
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if (CtrlState = scsNormal) and FUseFontColorToStyleColor then
      ACanvas.Font.Color := GetStyleColor(Self.Font.Color)
    else
      ACanvas.Font.Color := GetCheckBoxTextColor(CtrlState);
  end;
  ACanvas.Brush.Style := bsClear;
  S := GetLabelText;
  if FGlowEffect.Enabled then
    R := Rect(FGlowEffect.GlowSize - FGlowEffect.Offset, FGlowEffect.GlowSize - FGlowEffect.Offset,
              Width - FGlowEffect.GlowSize - FGlowEffect.Offset, Height - FGlowEffect.GlowSize - FGlowEffect.Offset)
  else
    R := Rect(0, 0, Width, Height);
  if R.Left < 0 then R.Left := 0;
  if R.Top < 0 then R.Top := 0;
  Flags := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
  if (Flags and DT_CALCRECT <> 0) and ((S = '') or ShowAccelChar and
    (S[1] = '&') and (S[2] = #0)) then S := S + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);

  if not FAutoSize and FShowEllipsis then
    Flags := Flags + DT_END_ELLIPSIS;

    case FVertAlignment of
      scvtaCenter:
        begin
          R1 := R;
          DoDrawText(ACanvas, R1, Flags or DT_CALCRECT);
          R.Top := (Height - R1.Height) div 2;
          R.Bottom := R.Top + R1.Height;
        end;
      scvtaBottom:
        begin
          R1 := R;
          DoDrawText(ACanvas, R1, Flags or DT_CALCRECT);
          R.Top := Height - R1.Height;
          R.Bottom := R.Top + R1.Height;
          if FGlowEffect.Enabled then
            OffsetRect(R, 0, - FGlowEffect.GlowSize - FGlowEffect.Offset);
        end;
    end;

  if FGlowEffect.Enabled then
  begin
    FUpdateGlow := CanUpdateGlowBuffer(ACanvas);
    DrawTextWithGlowBuffer(FGlowBuffer, FUpdateGlow, ACanvas, R, S, Flags,
      FGlowEffect.Offset, FGlowEffect.Color, FGlowEffect.GlowSize,
        FGlowEffect.Intensive, FGlowEffect.AlphaValue, IsRightToLeft, ShowAccelChar)
  end
  else
    DoDrawText(ACanvas, R, Flags);
end;

procedure TscLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TscLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  X: Integer;
  R: TRect;
  AAlignment: TAlignment;
  Buffer: TBitmap;
begin
  if not (csReading in ComponentState) and FAutoSize and
   (Align <> alTop) and (Align <> alBottom) and (Align <> alClient)  then
  begin
    Buffer := TBitmap.Create;
    Buffer.Canvas.Font := Self.Font;
    R := Rect(0, 0, Width, Height);
    DoDrawText(Buffer.Canvas, R, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
    Buffer.Free;
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - R.Right);
    if FGlowEffect.Enabled then
    begin
      R.Right := R.Right + FGlowEffect.Offset + FGlowEffect.GlowSize * 2;
      R.Bottom := R.Bottom + FGlowEffect.Offset + FGlowEffect.GlowSize * 2;
    end;
    SetBounds(X, Top, R.Right, R.Bottom);
  end;
end;

procedure TscLabel.SetShowEllipsis(Value: Boolean);
begin
  if FShowEllipsis <> Value then
  begin
    FShowEllipsis := Value;
    RePaintControl;
  end;
end;

procedure TscLabel.SetVertAlignment(Value: TscVertAlignment);
begin
  if Value <> FVertAlignment then
  begin
    FVertAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RePaintControl;
  end;
end;

procedure TscLabel.SetLabelAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

procedure TscLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TscLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    RePaintControl;
  end;
end;

procedure TscLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    RePaintControl;
  end;
end;

procedure TscLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    RePaintControl;
  end;
end;

procedure TscLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TscLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  RePaintControl;
end;

procedure TscLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  RePaintControl;
end;

procedure TscLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;


constructor TscTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint] - [csParentBackground];
  FWallpaperIndex := -1;
  FCustomBackgroundImageIndex := -1;
  {$IFDEF VER230}
  FStyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
end;

destructor TscTabSheet.Destroy;
begin
  inherited;
end;

procedure TscTabSheet.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaint;
    UpdateControls;
  end;
end;

procedure TscTabSheet.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaint;
    UpdateControls;
  end;
end;

procedure TscTabSheet.SetStyleKind(Value: TscTabStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    RePaint;
    UpdateControls;
  end;
end;

procedure TscTabSheet.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if Controls[i] is TWinControl
    then
      SendMessage(TWinControl(Controls[I]).Handle, WM_CHECKPARENTBG, 0, 0)
    else
    if Controls[i] is TGraphicControl
     then
       TGraphicControl(Controls[I]).Perform(WM_CHECKPARENTBG, 0, 0);
  end;
end;

procedure TscTabSheet.WMSIZE(var Msg: TMessage);
begin
  inherited;
  RePaint;
  if (PageControl <> nil) and (PageControl is TscPageControl) then
  begin
    with TscPageControl(PageControl) do
    begin
      if (Wallpapers <> nil) and FDrawTabsWallpaper and Wallpapers.NeedFullUpdate(TabsWallpaperIndex) then
      begin
        Self.UpdateControls;
      end
      else
      if ((FWallpapers <> nil) and FWallpapers.NeedFullUpdate(FWallpaperIndex)) or
         ((FCustomImages <> nil) and FCustomImages.NeedFullUpdate(FCustomBackgroundImageIndex))
      then
      begin
        Self.UpdateControls;
      end;
    end;
  end;
end;

procedure TscTabSheet.Draw(ACanvas: TCanvas);
var
  R, R1: TRect;
begin
  R := Rect(0, 0, Width, Height);
  if (PageControl <> nil) and (PageControl is TscPageControl) then
  with TscPageControl(PageControl) do
  begin
    if (FWallpapers <> nil) and FWallpapers.IsSolidDrawing(FWallpaperIndex) then
    begin
      FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
      Exit;
    end
    else
    if (FWallpapers <> nil) and DrawTabsWallpaper and FWallpapers.IsStretchTileDrawing(FTabsWallpaperIndex) then
    begin
      FWallpapers.Draw(ACanvas, R, FTabsWallpaperIndex, FScaleFactor);
      Exit;
    end;
  end;
  case FStyleKind of
    sctsTabSheet:
      begin
        R1 := R;
        InflateRect(R1, 3, 3);
        DrawTabFrame(ACanvas, R1);
      end;
    sctsPanel:
      with ACanvas do
      begin
        if (seClient in StyleElements) and IsCustomStyle then
          Brush.Color := GetStyleColor(Self.Color)
        else
          Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(R);
      end;
    sctsFormBackground:
      begin
        DrawFormBackground(ACanvas, R);
      end;
    sctsToolBar:
      begin
        if IsCustomStyle then
          with ACanvas do
          begin
            Brush.Color := GetStyleColor(clBtnFace);
            Brush.Style := bsSolid;
            FillRect(R);
          end;
        DrawToolBar(ACanvas, R);
      end;
  end;

  if (PageControl <> nil) and (PageControl is TscPageControl) then
    with TscPageControl(PageControl) do
    begin
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex) then
        FCustomImages.Draw(ACanvas, R, FCustomBackgroundImageIndex);
    end;

  if (PageControl <> nil) and (PageControl is TscPageControl) then
    with TscPageControl(PageControl) do
    begin
      if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
        FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
      if (FWallpapers <> nil) and DrawTabsWallpaper and FWallpapers.IsIndexAvailable(FTabsWallpaperIndex) then
      begin
        R := Rect(0, 0, Self.Width, Self.Height);
        Dec(R.Left, Self.Left);
        Dec(R.Top, Self.Top);
        R.Right := R.Left + PageControl.Width;
        R.Bottom := R.Top + PageControl.Height;
        FWallpapers.Draw(ACanvas, R, FTabsWallpaperIndex, FScaleFactor);
      end;
    end;
end;

procedure TscTabSheet.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TscCustomControl then
      TscCustomControl(Controls[I]).FForceDraw := True;
  inherited;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TscCustomControl then
      TscCustomControl(Controls[I]).FForceDraw := False;
end;

procedure TscTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  FCanvas: TCanvas;
  Buffer: TBitmap;
begin
  if Width * Height = 0 then Exit;
  FCanvas := TCanvas.Create;
  FCanvas.Handle := Message.DC;
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Width, Height);
    Draw(Buffer.Canvas);
    FCanvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
end;

constructor TscPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  {$IFDEF VER230}
  FStyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FWallpapers := nil;
  FCustomImages := nil;
  FScrollChecking := False;
  FTabGlowEffect := TscButtonGlowEffect.Create;
  FTabGlowEffect.OnChange := OnTabGlowEffectChange;
  FTabImageGlow := True;
  FHideBorder := False;
  FHideTabs := False;
  FMouseWheelSupport := False;
  FTabsInCenter := False;
  FTabsOffset := 0;
  FTabsWallpaperIndex := -1;
  FShowButtonsDivider := True;
  FShowFocusRect := True;
  FShowInActiveTab := True;
  FOldTabHeight := -1;
  FFreeOnClose := False;
  FImages := nil;
  FTempImages := TCustomImageList.Create(self);
  FTempImages.Width := 1;
  FTempImages.Height := 1;

  inherited Images := FTempImages;

  FTabExtendedDraw := False;
  FTabMargin := -1;
  FTabSpacing := 1;
  FTabLayout := blGlyphLeft;

  Ctl3D := False;
  FUpDown := nil;
  FUpDownPanel := nil;
  FActiveTab := -1;
  FOldActiveTab := -1;
  FActiveTabIndex := -1;
  FOldActiveTabIndex := -1;
  FCloseButtonSize := TAB_CLOSE_SIZE;
  FCloseSize := FCloseButtonSize + 10;
end;

destructor TscPageControl.Destroy;
begin
  FTabGlowEffect.Free;
  FTempImages.Free;
  inherited Destroy;
end;

procedure TscPageControl.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
  TabsOffset := MulDiv(FTabsOffset, M, D);
  {$IFNDEF VER300_UP}
  if TabWidth <> 0 then
    TabWidth := MulDiv(TabWidth, M, D);
  if TabHeight <> 0 then
    TabHeight := MulDiv(TabHeight, M, D);
  {$ENDIF}
  FCloseButtonSize := MulDiv(FCloseButtonSize, M, D);
  FCloseSize := FCloseButtonSize + 10;
end;

function TscPageControl.GetGlowParams(ACtrlState: TscsCtrlState;
   var AColor: TColor; var ASize: Byte; var AAlphaValue: Byte): Boolean;
begin
  Result := False;
  if Style = tsTabs then
  begin
    if Focused and (ACtrlState = scsFocused) then
       ACtrlState := scsPressed;
  end
  else
  begin
    if Focused and (ACtrlState = scsPressed) then
      ACtrlState := scsFocused;
  end;

  if not (ACtrlState in FTabGlowEffect.States) then
    Exit;

  case ACtrlState of
    scsNormal: AColor := FTabGlowEffect.Color;
    scsHot: AColor := FTabGlowEffect.HotColor;
    scsPressed: AColor := FTabGlowEffect.PressedColor;
    scsFocused: AColor := FTabGlowEffect.FocusedColor;
    scsDisabled: AColor := FTabGlowEffect.Color;
  end;
  if AColor = clNone then AColor := FTabGlowEffect.Color;
  if ACtrlState = scsPressed then
  begin
    ASize := FTabGlowEffect.PressedGlowSize;
    AAlphaValue := FTabGlowEffect.PressedAlphaValue;
  end
  else
  begin
    ASize := FTabGlowEffect.GlowSize;
    if ACtrlState = scsDisabled then
      AAlphaValue := FTabGlowEffect.AlphaValue div 2
    else
      AAlphaValue := FTabGlowEffect.AlphaValue;
  end;
  Result := True;
end;

procedure TscPageControl.OnTabGlowEffectChange(Sender: TObject);
begin
  if FTabExtendedDraw then
   Invalidate;
end;


procedure TscPageControl.SetHideBorder(Value: Boolean);
begin
  if FHideBorder <> Value then
  begin
    FHideBorder := Value;
    ReAlign;
  end;
end;

procedure TscPageControl.SetHideTabs(Value: Boolean);
var
  I: Integer;
  FSavePage: TTabSheet;
begin
  if FHideTabs <> Value then
  begin
    FHideTabs := Value;
    if FHideTabs then
    begin
      FSavePage := ActivePage;
      for I := 0 to PageCount - 1 do
        Pages[I].TabVisible := False;
      ActivePage := FSavePage;
    end
    else
    begin
      FSavePage := ActivePage;
      for I := 0 to PageCount - 1 do
        Pages[I].TabVisible := True;
      ActivePage := FSavePage;
    end;
  end;
end;

procedure TscPageControl.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    if ActivePage <> nil then
    begin
      ActivePage.Repaint;
      if ActivePage is TscTabSheet then
        TscTabSheet(ActivePage).UpdateControls;
    end;
  end;
end;

procedure TscPageControl.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    if ActivePage <> nil then
    begin
      ActivePage.Repaint;
      if ActivePage is TscTabSheet then
        TscTabSheet(ActivePage).UpdateControls;
    end;
  end;
end;

function TscPageControl.CanDrawWithOffset: Boolean;
begin
  Result := (FTabsInCenter  or (FTabsOffset > 0)) and (TabPosition = tpTop);
end;

function TscPageControl.GetItemOffset: Integer;
var
  W, i, AddW: Integer;
begin
  if FTabsOffset > 0 then
  begin
    Result := FTabsOffset;
    Exit;
  end;
  if TabWidth = 0 then
  begin
    Result := 0;
    Exit;
  end;
  W := 0;
  AddW := 0;
  case Style of
    tsButtons: AddW := 5;
    tsFlatButtons: AddW := 10;
  end;
  for i := 0 to PageCount - 1 do
    if Pages[i].TabVisible then W := W + TabWidth + AddW;
  Result := Width div 2 - W div 2 - 2;
  if Result < 0 then Result := 0;
end;

procedure TscPageControl.SetTabsInCenter;
begin
  if TabWidth = 0 then
  begin
    FTabsInCenter := False;
    Exit;
  end;
  if FTabsInCenter <> Value then
  begin
    FTabsInCenter := Value;
    if FTabsInCenter and (TabPosition <> tpTop) then
      TabPosition := tpTop;
    if FTabsInCenter and MultiLine then
      MultiLine := False;
    Invalidate;
  end;
end;

procedure TscPageControl.SetTabsOffset;
begin
  if FTabsOffset <> Value then
  begin
    FTabsOffset := Value;
    if (FTabsOffset > 0) and (TabPosition <> tpTop) then
      TabPosition := tpTop;
    if (FTabsOffset > 0) and MultiLine then
      MultiLine := False;
    Invalidate;
  end;
end;

procedure TscPageControl.SetTabsWallpaperIndex(Value: Integer);
begin
  if FTabsWallpaperIndex <> Value then
  begin
    FTabsWallpaperIndex := Value;
    Invalidate;
  end;
end;

procedure TscPageControl.CreateWnd;
begin
  inherited;
  CheckScroll;
end;

procedure TscPageControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TscPageControl.WMCHANGE(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then  Invalidate;
end;

procedure TscPageControl.WMMOUSEWHEEL(var Message: TMessage);
begin
  inherited;
  if FMouseWheelSupport and Focused then
    if TWMMOUSEWHEEL(Message).WheelDelta < 0 then FindNextPage else FindPriorPage;
end;

procedure TscPageControl.FindNextPage;
var
  i, j, k: Integer;
begin
  j := -1;
  for i := 0 to PageCount - 1 do
    if Pages[i] = ActivePage then
  begin
    j := i;
    Break;
  end;
  if (j = -1) or (j = PageCount - 1) then Exit;
  k := -1;
  for i := j + 1 to PageCount - 1 do
  begin
    if Pages[i].TabVisible then
    begin
      k := i;
      Break;
    end;
  end;
  if k <> -1 then ActivePage := Pages[k];
end;

procedure TscPageControl.FindPriorPage;
var
  i, j, k: Integer;
begin
  j := -1;
  for i := 0 to PageCount - 1 do
    if Pages[i] = ActivePage then
  begin
    j := i;
    Break;
  end;
  if (j = -1) or (j = 0) then Exit;
  k := -1;
  for i := j - 1 downto 0 do
  begin
    if Pages[i].TabVisible then
    begin
      k := i;
      Break;
    end;
  end;
 if k <> -1 then ActivePage := Pages[k];
end;

procedure TscPageControl.SetTabExtendedDraw;
begin
  if FTabExtendedDraw <> Value then
  begin
    FTabExtendedDraw := Value;
    Invalidate;
  end;
end;

procedure TscPageControl.SetTabLayout;
begin
  if FTabLayout <> Value then
  begin
    FTabLayout := Value;
    RePaint;
  end;
end;

procedure TscPageControl.SetTabSpacing;
begin
  if Value <> FTabSpacing then
  begin
    FTabSpacing := Value;
    RePaint;
  end;
end;

procedure TscPageControl.SetTabMargin;
begin
  if (Value <> FTabMargin) and (Value >= -1) then
  begin
    FTabMargin := Value;
    RePaint;
  end;
end;

procedure TscPageControl.DoClose;
var
  I: TTabSheet;
  CanClose: Boolean;
  P: TPoint;
begin
  I := ActivePage;
  CanClose := True;
  if Assigned(FOnClose) then FOnClose(I, CanClose);
  if CanClose then
  begin
    I.TabVisible := False;
    if FFreeOnClose then I.Free;
    if Assigned(FOnAfterClose) then FOnAfterClose(Self);
  end;
  if CanClose = False then
  begin
    GetCursorPos(P);
    if WinApi.Windows.WindowFromPoint(P) <> Self.Handle then
    begin
      TscTabSheet(I).CloseButtonMouseIn := False;
      TscTabSheet(I).CloseButtonMouseDown := False;
      RePaint;
    end;
  end;
end;

procedure TscPageControl.DrawCloseButton(Cnvs: TCanvas; R: TRect; I: Integer; AColor: TColor);
var
  X, Y: Integer;
  ButtonR: TRect;
  FC: TColor;
begin
  X := R.Left + R.Width div 2 - FCloseButtonSize div 2;
  Y := R.Top + R.Height div 2 - FCloseButtonSize div 2;
  ButtonR := Rect(X, Y, X + FCloseButtonSize + 2, Y + FCloseButtonSize + 1);
  TscTabSheet(Pages[I]).CloseButtonRect := ButtonR;
  FC := AColor;
  if TscTabSheet(Pages[I]).CloseButtonMouseDown then
  begin
    DrawToolButton(Cnvs, ButtonR, scsPressed);
    FC := GetToolButtonTextColor(scsPressed);
  end
  else
  if TscTabSheet(Pages[I]).CloseButtonMouseIn then
  begin
    DrawToolButton(Cnvs, ButtonR, scsHot);
    FC := GetToolButtonTextColor(scsHot);
  end;
  DrawTabCloseImage(Cnvs, ButtonR, FC, FScaleFactor);
end;


procedure TscPageControl.SetShowInActiveTab(Value: Boolean);
begin
  if FShowInActiveTab <> Value then
  begin
    FShowInActiveTab := Value;
    ReDrawTabs;
  end;
end;

procedure TscPageControl.SetShowCloseButtons(Value: Boolean);
begin
  if FShowCloseButtons <> Value then
  begin
    FShowCloseButtons := Value;
    if FShowCloseButtons then
    begin
      if TabPosition in [tpTop, tpBottom]
      then
        FTempImages.Width := FTempImages.Width + FCloseSize
      else
        FTempImages.Height := FTempImages.Height + FCloseSize;
    end
    else
    begin
      if TabPosition in [tpTop, tpBottom] then
        FTempImages.Width := FTempImages.Width - FCloseSize
      else
        FTempImages.Height := FTempImages.Height - FCloseSize;
    end;
    Invalidate;
    ReDrawTabs;
  end;
end;

procedure TscPageControl.SetImages(value: TCustomImageList);
begin
  if FImages <> nil then
  begin
    if TabPosition in [tpTop, tpBottom] then
      FTempImages.Width := FTempImages.Width - FImages.Width
    else
      FTempImages.Height := FTempImages.Height - FImages.Height;
  end;

  FImages := Value;

  if FImages <> nil then
  begin
    if TabPosition in [tpTop, tpBottom] then
      FTempImages.Width := FTempImages.Width + FImages.Width
    else
      FTempImages.Height := FTempImages.Height + FImages.Height;
  end;

  Invalidate;
end;

procedure TscPageControl.WMCHECKPARENTBG;
begin
  RePaint;
end;

procedure TscPageControl.UpDateTabs;
begin
  if MultiLine and (FUpDown <> nil) then
    HideUpDown;
  ReAlign;
end;

procedure TscPageControl.CMMouseLeave;
begin
  inherited;

  if HideTabs then Exit;

  TestActive(-1, -1);

  if (FOldActiveTabIndex <> - 1) and (FOldActiveTabIndex <> TabIndex) and
     (FOldActiveTabIndex < PageCount) then
  begin
    FOldActiveTabIndex := -1;
    FOldActiveTab := -1;
    RePaint;
  end;

  if (FActiveTabIndex <> - 1) and (FActiveTabIndex <> TabIndex) and
     (FActiveTabIndex < PageCount) then
  begin
    FActiveTabIndex := -1;
    FActiveTab := -1;
    RePaint;
  end;
end;

procedure TscPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
              X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if HideTabs then Exit;

  if (Button = mbLeft) and not (csDesigning in ComponentState)
  then
    if CanDrawWithOffset then
      TestActive(X + GetItemOffset, Y)
    else
      TestActive(X, Y);

  if (FActiveTabIndex <> -1) and FShowCloseButtons and (Button = mbLeft)
  then
    with TscTabSheet(Pages[FActiveTab]) do
    begin
      R := CloseButtonRect;
      if CanDrawWithOffset then
        OffsetRect(R, - GetItemOffset, 0);
      if PtInRect(R, Point(X, Y))
      then
        begin
          CloseButtonMouseIn := True;
          CloseButtonMouseDown := False;
          UpdateTab(FActiveTabIndex);
          DoClose;
        end
      else
      if not PtInRect(R, Point(X, Y))
      then
        begin
          CloseButtonMouseIn := False;
          CloseButtonMouseDown := False;
        end;
    end;

end;

procedure TscPageControl.MouseDown;
var
  R: TRect;
begin
  inherited;

  if HideTabs then Exit;

  if (Button = mbLeft) and not (csDesigning in ComponentState)
  then
    if CanDrawWithOffset then
      TestActive(X + GetItemOffset, Y)
    else
      TestActive(X, Y);

  if (FActiveTabIndex <> -1) and FShowCloseButtons and (Button = mbLeft)
  then
    with TscTabSheet(Pages[FActiveTab]) do
    begin
      R := CloseButtonRect;
      if CanDrawWithOffset then
        OffsetRect(R, - GetItemOffset, 0);
      if PtInRect(R, Point(X, Y)) then
      begin
        CloseButtonMouseIn := True;
        CloseButtonMouseDown := True;
        UpdateTab(FActiveTabIndex);
      end
      else
      if not PtInRect(R, Point(X, Y)) then
      begin
        CloseButtonMouseIn := False;
        CloseButtonMouseDown := False;
      end;
   end;
end;

procedure TscPageControl.MouseMove;
begin
 inherited;
 if HideTabs then Exit;
 if  not (csDesigning in ComponentState) then
   TestActive(X, Y);
end;

procedure TscPageControl.OnUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  FUpDown.Max := GetInVisibleItemCount;
  if FUpDown.Position > FUpDown.Max then FUpDown.Position := FUpDown.Max;
  if Button = btNext then
  begin
    if FUpDown.Position + 1 <= FUpDown.Max then
      FUpDown.Position := FUpDown.Position + 1
  end
  else
  begin
    if FUpDown.Position - 1 >= 0 then
      FUpDown.Position := FUpDown.Position - 1;
  end;
  SendMessage(Handle, WM_HSCROLL,
    MakeWParam(SB_THUMBPOSITION, FUpDown.Position), 0);
end;


function TscPageControl.GetPosition: Integer;
var
  i, j, k: Integer;
  R: TRect;
begin
  j := 0;
  k := -1;
  for i := 0 to PageCount - 1 do
  if Pages[i].TabVisible then
  begin
    inc(k);
    R := GetItemRect(k);
    if R.Left <= 0 then inc(j);
  end;
  Result := j;
end;

function TscPageControl.GetInVisibleItemCount;
var
  i, j, k: Integer;
  R: TRect;
  Limit: Integer;
begin
  Limit := Width - 1;
  if not FScrollChecking then
    if FUpDown <> nil then Limit := Width - FUpDown.Width;
  j := 0;
  k := -1;
  for i := 0 to PageCount - 1 do
  if Pages[i].TabVisible
  then
  begin
    inc(k);
    R := GetItemRect(k);
    if (R.Right > Limit) or (R.Left <= 0)
    then
      inc(j);
  end;
  Result := j;
end;

procedure TscPageControl.WMParentNotify(var Message: TWMParentNotify);
var
  Wnd: HWnd;
begin
  inherited;
  if (FTabsOffset > 0) and CanDrawWithOffset then
  begin
    Wnd := FindWindowEx(Handle, 0, 'msctls_updown32', nil);
    if Wnd <> 0 then DestroyWindow(Wnd);
  end;
end;

procedure TscPageControl.CheckScroll;
var
  InVCount: Integer;
  Wnd: HWnd;
begin
  if csDesigning in ComponentState then Exit;

  if not ((FTabsOffset > 0) and CanDrawWithOffset) then
  begin
    Wnd := FindWindowEx(Handle, 0, 'msctls_updown32', nil);
    if Wnd <> 0 then DestroyWindow(Wnd);
  end;
  FScrollChecking := True;
  InVCount := GetInVisibleItemCount;
  FScrollChecking := False;
  if ((InVCount = 0) or MultiLine or (CanDrawWithOffset and (GetItemOffset > 0))) and (FUpDown <> nil) then
    HideUpDown
  else
  if (InVCount > 0) and (FUpDown = nil) and not MultiLine and
    not (CanDrawWithOffset and (GetItemOffset > 0)) then
    ShowUpDown;
  if FUpDownPanel <> nil then
  begin
    FUpDown.Max := GetInVisibleItemCount;
    if TabPosition = tpTop then
      FUpDownPanel.SetBounds(Width - FUpDownPanel.Width,
       DisplayRect.Top - FUpDownPanel.Height - 6,
       FUpDownPanel.Width, FUpDownPanel.Height)
    else
      FUpDownPanel.SetBounds(Width - FUpDownPanel.Width, DisplayRect.Bottom + 6,
        FUpDownPanel.Width, FUpDownPanel.Height);
    if FUpDownPanel.Top < 0 then FUpDownPanel.Top := 0;
  end;
end;

procedure TscPageControl.ShowUpDown;
begin
  if HideTabs then Exit;
  if FUpDownPanel <> nil then Exit;
  FUpDownPanel := TscPanel.Create(Self);
  FUpDownPanel.DrawOnBackground := False;
  FUpDownPanel.TransparentBackground := False;
  FUpDownPanel.Width := GetSystemMetrics(SM_CYHSCROLL) * 2;
  FUpDownPanel.Height := GetSystemMetrics(SM_CYHSCROLL);
  FUpDownPanel.Visible := False;
  FUpDownPanel.Parent := Self;

  if TabPosition = tpTop then
    FUpDownPanel.SetBounds(Width - FUpDownPanel.Width,
     DisplayRect.Top - FUpDownPanel.Height - 6,
     FUpDownPanel.Width, FUpDownPanel.Height)
  else
    FUpDownPanel.SetBounds(Width - FUpDownPanel.Width, DisplayRect.Bottom + 6,
     FUpDownPanel.Width, FUpDownPanel.Height);

  FUpDown := TscUpDown.Create(Self);
  FUpDown.Orientation := udHorizontal;
  FUpDown.Parent := FUpDownPanel;
  FUpDown.Visible := True;
  FUpDown.Width := GetSystemMetrics(SM_CYHSCROLL) * 2;
  FUpDown.Height := GetSystemMetrics(SM_CYHSCROLL);
  FUpDown.Min := 0;
  FUpDown.Max := GetInVisibleItemCount;
  FUpDown.Position := GetPosition;
  FUpDown.Increment := 1;
  FUpDown.OnClick := OnUpDownClick;
  FUpDown.Top := 0;
  FUpDown.Left := 0;

  FUpDownPanel.Visible := True;
end;

procedure TscPageControl.HideUpDown;
begin
  if FUpDownPanel <> nil then
  begin
    FUpDownPanel.Free;
    FUpDown := nil;
    FUpDownPanel := nil;
  end;
end;

procedure TscPageControl.WMHSCROLL;
begin
  inherited;
  RePaint;
end;

procedure TscPageControl.ReDrawTabs;
var
  I: Integer;
begin
  if HideTabs then Exit;
  for I := 0 to Tabs.Count - 1 do
    UpdateTab(I);
end;

procedure TscPageControl.Change;
begin
  if FUpDown <> nil then
    FUpDown.Position := GetPosition;
  inherited;
  RePaint;
  RedrawTabs;
  if ActivePage <> nil then
    ActivePage.Invalidate;
end;

procedure TscPageControl.Change2;
begin
  if FUpDown <> nil then
    FUpDown.Position := GetPosition;
  Invalidate;
end;

procedure TscPageControl.Notification;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscPageControl.PaintStyleWindow;
var
  R, R1: TRect;
  SaveIndex: Integer;
  FBGDraw: Boolean;
begin
  R := DisplayRect;
  FBGDraw := False;
  SaveIndex := SaveDC(Cnvs.Handle);
  try
    if R.Width * R.Height > 0 then
      ExcludeClipRect(Cnvs.Handle, R.Left, R.Top, R.Right, R.Bottom);
    if not ((FWallpapers <> nil) and FWallpapers.IsSolidDrawing(FTabsWallpaperIndex)) then
    begin
      DrawParentBackground(Self, Cnvs);
      FBGDraw := True;
    end;
  finally
    RestoreDC(Cnvs.Handle, SaveIndex);
  end;

  if FBGDraw then
    with Cnvs do
    begin
      InflateRect(R, 4, 4);
      if (not HideBorder) or (Tabs.Count = 0) then
        DrawTabFrame(Cnvs, R);
    end;

  if (Tabs.Count > 0) and
     (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FTabsWallpaperIndex) then
  begin
    R := DisplayRect;
    R1 := Rect(0, 0, Width, Height);
    case TabPosition of
      tpTop: R1.Bottom := R.Top;
      tpBottom: R1.Top := R.Bottom;
      tpLeft: R1.Right := R.Left;
      tpRight: R1.Left := R.Right;
    end;
    FWallpapers.Draw(Cnvs, R1, FTabsWallpaperIndex, FScaleFactor);
  end;

  if not FBGDraw then
    with Cnvs do
    begin
      InflateRect(R, 4, 4);
      if (not HideBorder) or (Tabs.Count = 0) then
        DrawTabFrame(Cnvs, R);
    end;

  case Style of
    tsTabs: DrawTabs(Cnvs);
    tsButtons, tsFlatButtons: DrawButtons(Cnvs);
  end;
end;

procedure TscPageControl.Loaded;
begin
  inherited Loaded;
  ReAlign;
  if FShowCloseButtons and (FScaleFactor > 1) then
  begin
    ShowCloseButtons := False;
    ShowCloseButtons := True;
  end;
  CheckScroll;
  if FUpDown <> nil then
    FUpDown.Position := GetPosition;
end;

procedure TscPageControl.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if Message.DC <> 0 then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := BeginPaint(Handle, PS);
    MemBitmap := CreateCompatibleBitmap(DC, PS.rcPaint.Right - PS.rcPaint.Left,
      PS.rcPaint.Bottom - PS.rcPaint.Top);
     MemDC := CreateCompatibleDC(DC);
    try
      OldBitmap := SelectObject(MemDC, MemBitmap);
      try
        SetWindowOrgEx(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
        Perform(WM_ERASEBKGND, MemDC, MemDC);
        Message.DC := MemDC;
        if TStyleManager.IsCustomStyleActive then
          WndProc(TMessage(Message))
        else
          WMPaint(Message);
        Message.DC := 0;
          BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
            PS.rcPaint.Right - PS.rcPaint.Left,
            PS.rcPaint.Bottom - PS.rcPaint.Top,
            MemDC,
            PS.rcPaint.Left, PS.rcPaint.Top,
            SRCCOPY);
      finally
        SelectObject(MemDC, OldBitmap);
      end;
    finally
       EndPaint(Handle, PS);
       DeleteDC(MemDC);
       DeleteObject(MemBitmap);
    end;
  end;
end;

procedure TscPageControl.WndProc(var Message:TMessage);
begin
  if (Message.Msg = WM_SETFOCUS) and CanDrawWithOffset
  then
    begin
      inherited WndProc(Message);
      Invalidate;
    end
  else
  if (Message.Msg = WM_KILLFOCUS) and CanDrawWithOffset
  then
    begin
      inherited WndProc(Message);
      Invalidate;
    end
  else
  if (Message.Msg = WM_LButtonDblClk) and CanDrawWithOffset
  then
    begin
      TWMLButtonDblClk(Message).Pos.X := TWMLButtonDblClk(Message).Pos.X - GetItemOffset;
      inherited WndProc(Message);
    end
  else
  if (Message.Msg = WM_LButtonUp) and CanDrawWithOffset
  then
    begin
      TWMLButtonUp(Message).Pos.X := TWMLButtonUp(Message).Pos.X - GetItemOffset;
      inherited WndProc(Message);
    end
  else
  if (Message.Msg = WM_LButtonDown) and CanDrawWithOffset
  then
    begin
      TWMLButtonDown(Message).Pos.X := TWMLButtonDown(Message).Pos.X - GetItemOffset;
      inherited WndProc(Message);
    end
  else
  if (Message.Msg = WM_NCHITTEST) and CanDrawWithOffset
  then
    begin
      TWMNCHITTEST(Message).Pos.X := TWMNCHITTEST(Message).Pos.X - GetItemOffset;
      inherited WndProc(Message);
    end
  else
  if Message.Msg = TCM_GETITEMRECT
  then
    begin
      inherited WndProc(Message);
      if Style = tsTabs then
         case TabPosition of
           tpLeft:
             PRect(Message.LParam)^.Right := PRect(Message.LParam)^.Right - 2;
           tpRight:
             begin
               if (not TStyleManager.SystemStyle.Enabled) or IsCustomStyle then
                 PRect(Message.LParam)^.Left := PRect(Message.LParam)^.Left + 2;
             end;
           tpBottom:
             begin
               if TStyleManager.SystemStyle.Enabled then
                 PRect(Message.LParam)^.Top := PRect(Message.LParam)^.Top + 1
               else
                 PRect(Message.LParam)^.Top := PRect(Message.LParam)^.Top + 2;
             end;
         end;
      if CanDrawWithOffset then
        OffsetRect(PRect(Message.LParam)^, GetItemOffset, 0);
    end
  else
    inherited WndProc(Message);

  if (Message.Msg = TCM_ADJUSTRECT) and FHideBorder then
  begin
    PRect(Message.LParam)^.Left := PRect(Message.LParam)^.Left - 4;
    PRect(Message.LParam)^.Right := PRect(Message.LParam)^.Right + 4;
    PRect(Message.LParam)^.Top := PRect(Message.LParam)^.Top - 4;
    PRect(Message.LParam)^.Bottom := PRect(Message.LParam)^.Bottom + 4;
  end;

  if (Message.Msg = CN_NOTIFY) and (Style <> tsTabs) and
      not (csDesigning in ComponentState) and
     (TWMNotify(Message).NMHdr.code = TCN_FOCUSCHANGE) and ((FUpDown <> nil) or CanDrawWithOffset) then
    Invalidate;

  if (Message.Msg = WM_SIZE) and (not MultiLine) and
     not (csDesigning in ComponentState) then
    CheckScroll;
end;

function TscPageControl.GetItemRect(index: integer): TRect;
var
  R: TRect;
begin
  SendMessage(Handle, TCM_GETITEMRECT, index, Integer(@R));
  Result := R;
end;

procedure TscPageControl.PaintWindow(DC: HDC);
var
  SaveIndex: Integer;
begin
  if (Width <= 0) or (Height <=0) then Exit;
  SaveIndex := SaveDC(DC);
  try
    Canvas.Handle := DC;
    PaintStyleWindow(Canvas);
    Canvas.Handle := 0;
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TscPageControl.UpdateTab(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Self.Tabs.Count) then
    Tabs[AIndex] := Tabs[AIndex];
end;

procedure TscPageControl.TestActive(X, Y: Integer);
var
  i, j, k, t: Integer;
  R: TRect;
begin
  if HideTabs then Exit;

  FOldActiveTab := FActiveTab;
  FOldActiveTabIndex := FActiveTabIndex;
  FActiveTab := -1;
  FActiveTabIndex := -1;
  k := -1;
  j := -1;
  t := -1;
  for i := 0 to PageCount - 1 do
  if Pages[i].TabVisible then
  begin
    Inc(k);
    R := GetItemRect(k);
    if PtInRect(R, Point(X, Y)) then
    begin
      j := k;
      t := i;
      Break;
    end;
  end;

  FActiveTab := t;
  FActiveTabIndex := j;

  if (FOldActiveTabIndex <> FActiveTabIndex) then
  begin
    if (FOldActiveTabIndex <> - 1) and (FOldActiveTabIndex <> TabIndex) and
       (FOldActiveTabIndex < PageCount) then
    begin
      R := GetItemRect(FOldActiveTabIndex);
      TscTabSheet(Pages[FOldActiveTab]).CloseButtonMouseIn := False;
      TscTabSheet(Pages[FOldActiveTab]).CloseButtonMouseDown := False;
      UpdateTab(FOldActiveTabIndex);
    end;
    if (FActiveTabIndex <> -1) and (FActiveTabIndex <> TabIndex) and
       (FActiveTabIndex < PageCount) then
    begin
      RePaint;
    end;
  end;

  if (FActiveTabIndex <> -1) and FShowCloseButtons then
  with TscTabSheet(Pages[FActiveTab]) do
  begin
    if PtInRect(CloseButtonRect, Point(X, Y)) and not CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := True;
      UpdateTab(FActiveTabIndex);
    end
    else
    if not PtInRect(CloseButtonRect, Point(X, Y)) and CloseButtonMouseIn then
    begin
      CloseButtonMouseIn := False;
      CloseButtonMouseDown := False;
      UpdateTab(FActiveTabIndex);
    end;
  end;
end;

procedure TscPageControl.DrawButtons(Cnvs: TCanvas);
var
  i, j: integer;
  R: TRect;
begin
  if HideTabs then Exit;
  if (PageCount = 0) then Exit;
  j := -1;
  for i := 0 to PageCount-1 do
    if Pages[i].TabVisible then
    begin
      inc(j);
      R := GetItemRect(j);
      if Style = tsButtons then
        DrawStyleButton(i, R, j = TabIndex, j = FActiveTabIndex, Cnvs)
      else
      begin
        DrawStyleFlatButton(i, R, j = TabIndex, j = FActiveTabIndex, Cnvs);
        if not Multiline and FShowButtonsDivider then
        begin
          R.Left := R.Right + 4;
          R.Right := R.Left + 10;
          scDrawUtils.DrawVertSplitter(Cnvs, R);
        end;
      end;
    end;
end;

procedure TscPageControl.DrawTabs;
var
  i, j, k: Integer;
  R: TRect;
  FFirst, FLast: Boolean;
  Offset: Integer;
begin
  if HideTabs then Exit;
  if (PageCount = 0) then Exit;
  j := -1;
  k := 0;
  Offset := 0;
  if CanDrawWithOffset then
    Offset := GetItemOffset;
  for i := 0 to PageCount - 1 do
    if Pages[i].TabVisible then
    begin
      inc(j);
      R := GetItemRect(j);
      if (j <> TabIndex) and not ((Offset > 0) and (R.Left < Offset)) then
        DrawStyleTab(i, R, False, j = FActiveTabIndex, Cnvs, False, False)
      else
        k := i;
    end;
  if TabIndex >= 0 then
  begin
    R := GetItemRect(TabIndex);
    InflateRect(R, 2, 2);
    FFirst := R.Left = 0;
    FLast := R.Right >= Width - 4;
    if FLast and (TabPosition = tpBottom) then
      Dec(R.Right, 2);
    case TabPosition of
      tpTop:
        begin
          if not TStyleManager.SystemStyle.Enabled and not IsCustomStyle then
            Dec(R.Bottom);
        end;
      tpLeft:
        begin
          if not IsCustomStyle then
            Dec(R.Right);
          FFirst := R.Top = 0;
          FLast := R.Bottom >= Height - 4;
          if FLast then Dec(R.Bottom);
        end;
      tpRight:
        begin
          if not IsCustomStyle then
            Inc(R.Left);
          FFirst := R.Top = 0;
          FLast := R.Bottom >= Height - 4;
          if FLast then Dec(R.Bottom);
          if not TStyleManager.SystemStyle.Enabled then
            Dec(R.Left);
        end;
      tpBottom:
        begin
          if TStyleManager.SystemStyle.Enabled then
            Inc(R.Top);
        end;
    end;
    if not ((Offset > 0) and (R.Left < Offset)) then
      DrawStyleTab(k, R, True, Pages[k].TabIndex = FActiveTabIndex,
        Cnvs, FFirst, FLast);
  end;
end;

procedure TscPageControl.DrawStyleTab;
var
  TabState: TscsCtrlState;
  B, B1: TBitmap;
  R: TRect;
  TR: TRect;
  FImageIndex, FImageWidth, FImageHeight, FImageOffset: Integer;
  ImageR: TRect;
  TX, TY: Integer;
  S: String;
  FC: TColor;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
begin
  if HideTabs then Exit;

  if MouseIn then
    TabState := scsHot
  else
    TabState := scsNormal;
  if Active then TabState := scsFocused;

  if not ((TabState = scsNormal) and not FShowInActiveTab) then

  if (TabPosition = tpTop) or not StyleServices.Enabled then
    scDrawUtils.DrawTab (Cnvs, Rct, TabState, TabPosition, First, Last)
  else
  begin
    B := TBitmap.Create;
    B.PixelFormat := pf32bit;
    B.SetSize(40, 40);
    with B.Canvas do
    begin
      Brush.Color := GetStyleColor(clBtnFace);
      FillRect(Rect(0, 0, B.Width, B.Height));
    end;
    scDrawUtils.DrawTab(B.Canvas, Rect(0, 0, B.Width, B.Height + 1),
      TabState, tpTop, False, False);
    B1 := TBitmap.Create;
    B1.PixelFormat := pf32bit;
    B1.SetSize(40, 40);
    if TabPosition = tpLeft then
    begin
      Bitmap_Rotate90_1(B, B1);
      DrawStrecthBitmap(10, 10, 10, 10, B1, Cnvs, Rct);
    end
    else
    if TabPosition = tpRight then
    begin
      Bitmap_Rotate90_2(B, B1);
      DrawStrecthBitmap(10, 10, 10, 10, B1, Cnvs, Rct);
    end
    else
    begin
      Bitmap_FlipVert(B);
      DrawStrecthBitmap(10, 10, 10, 10, B, Cnvs, Rct);
    end;
    B.Free;
    B1.Free;
  end;
  Cnvs.Font.Assign(Self.Font);
  FC := ColorToRGB(Cnvs.Font.Color);
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if (TabState = scsNormal) and not FShowInActiveTab then
      FC := ColorToRGB(GetCheckBoxTextColor(TabState))
    else
      FC := ColorToRGB(GetTabTextColor(TabState));
    SetTextColor(Cnvs.Handle, FC);
  end;
  TR := Rct;
  if Active then
  if FTabExtendedDraw then
  begin
    InflateRect(TR, -2, -2);
    OffsetRect(TR, 0, -1);
  end
  else
  case TabPosition of
    tpTop:
      begin
        InflateRect(TR, -2, -2);
        OffsetRect(TR, 0, -1);
      end;
    tpBottom:
      begin
        InflateRect(TR, -2, -2);
        OffsetRect(TR, 0, 1);
      end;
    tpLeft:
      begin
        InflateRect(TR, -2, -2);
        OffsetRect(TR, -1, 0);
      end;
    tpRight:
      begin
        InflateRect(TR, -2, -2);
        OffsetRect(TR, 1, 0);
      end;
  end;

  if FShowCloseButtons then
  begin
    R := TR;
    if FTabExtendedDraw then
    begin
      R.Left := R.Right - FCloseSize;
      TR.Right := R.Left + 2;
      Dec(R.Right, 2);
    end
    else
    case TabPosition of
      tpTop, tpBottom:
        begin
          R.Left := R.Right - FCloseSize;
          TR.Right := R.Left + 2;
          Dec(R.Right, 2);
        end;
      tpLeft:
        begin
          R.Bottom := R.Top + FCloseSize;
          TR.Top := R.Bottom + 2;
        end;
      tpRight:
        begin
          R.Top := R.Bottom - FCloseSize;
          TR.Bottom := R.Top + 2;
        end;
    end;
    if not MouseIn then
       TscTabSheet(Pages[TI]).CloseButtonMouseIn := False;
    if FScaleFactor > 1 then
      OffsetRect(R, -1, 0);
    DrawCloseButton(Cnvs, R, TI, FC);
  end;
  if Assigned(OnDrawTab) then
  begin
    SetBkMode(Cnvs.Handle, TRANSPARENT);
    SetTextColor(Cnvs.Handle, FC);
    OnDrawTab(TI, TR, Active, MouseIn, Cnvs);
    if Focused and Active and FShowFocusRect then
    begin
      R := Rct;
      InflateRect(R, -4, -4);
      scDrawFocusRect2(Cnvs, R, FC, FScaleFactor);
    end;
    Exit;
  end;
  if FTabExtendedDraw then
  begin
    SetBkMode(Cnvs.Handle, TRANSPARENT);
    SetTextColor(Cnvs.Handle, FC);
    S := Pages[TI].Caption;
    if FTabGlowEffect.Enabled and GetGlowParams(TabState, FGlowColor, FGlowSize, FGlowAlpha) then
      DrawImageAndTextWithGlow(Cnvs, TR, FTabMargin, FTabSpacing, FTabLayout,
        S, Pages[TI].ImageIndex, Images,
        Pages[TI].Enabled, False, clBlack,
        FTabGlowEffect.Offset, FGlowColor, FGlowSize, FTabGlowEffect.Intensive,
          FGlowAlpha, FTabImageGlow, False, IsRightToLeft, False, FScaleFactor)
    else
      DrawImageAndText(Cnvs, TR, FTabMargin, FTabSpacing, FTabLayout,
        S, Pages[TI].ImageIndex, Images,
          Pages[TI].Enabled, False, clBlack, False, IsRightToLeft, False, FScaleFactor);
    if Focused and Active and FShowFocusRect then
    begin
      R := Rct;
      InflateRect(R, -4, -4);
      scDrawFocusRect2(Cnvs, R, FC, FScaleFactor);
    end;
    Exit;
  end;
  FImageIndex := Pages[TI].ImageIndex;
  if (Images <> nil) and (FImageIndex >= 0) and (FImageIndex < Images.Count) then
  begin
    FImageWidth := Images.Width;
    FImageHeight := Images.Height;
    FImageOffset := 3;
  end
  else
  begin
    FImageWidth := 0;
    FImageHeight := 0;
    FImageOffset := 0;
  end;
  if (Images <> nil) and (FImageWidth > 0) then
  begin
    ImageR := TR;
    case TabPosition of
      tpTop, tpBottom:
        begin
          ImageR.Left := ImageR.Left + FImageOffset;
          ImageR.Right := ImageR.Left + FImageWidth;
          TR.Left := ImageR.Right;
          ImageR.Top := ImageR.Top + (ImageR.Bottom - ImageR.Top) div 2 - FImageHeight div 2;
        end;
      tpLeft:
        begin
          ImageR.Bottom := ImageR.Bottom - FImageOffset;
          ImageR.Top := ImageR.Bottom - FImageHeight;
          TR.Bottom := ImageR.Top;
          ImageR.Left := ImageR.Left + (ImageR.Right - ImageR.Left) div 2 - FImageWidth div 2;
        end;
      tpRight:
        begin
          ImageR.Top := ImageR.Top + FImageOffset;
          ImageR.Bottom := ImageR.Top + FImageHeight;
          TR.Top := ImageR.Bottom;
          ImageR.Left := ImageR.Left + (ImageR.Right - ImageR.Left) div 2 - FImageWidth div 2;
        end;
    end;
    Images.Draw(Cnvs, ImageR.Left, ImageR.Top, FImageIndex, True);
  end;
  SetBkMode(Cnvs.Handle, TRANSPARENT);
  SetTextColor(Cnvs.Handle, FC);
  S := Pages[TI].Caption;
  if TabPosition = tpLeft then
  begin
    TX := TR.Left + (TR.Right - TR.Left) div 2 -
       Cnvs.TextHeight(S) div 2;
    TY := TR.Top + (TR.Bottom - TR.Top) div 2 +
     Cnvs.TextWidth(S) div 2;
    AngleDrawText(Cnvs, 900, TX, TY, S, FC);
  end
  else
  if TabPosition = tpRight then
  begin
    TX := TR.Left + (TR.Right - TR.Left) div 2 +
      Cnvs.TextHeight(S) div 2;
    TY := TR.Top + (TR.Bottom - TR.Top) div 2 -
      Cnvs.TextWidth(S) div 2;
    AngleDrawText(Cnvs, -900, TX, TY, S, FC);
  end
  else
  begin
    DrawText(Cnvs.Handle, PChar(S), Length(S), TR,
     scDrawTextBidiModeFlags(DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_NOCLIP,
      BidiMode = bdRightToLeft));
  end;
  if Focused and Active and FShowFocusRect then
  begin
    R := Rct;
    InflateRect(R, -4, -4);
    scDrawFocusRect2(Cnvs, R, FC);
  end;
end;

procedure TscPageControl.DrawStyleButton(TI: Integer; const Rct: TRect;
      Active, MouseIn: Boolean; Cnvs: TCanvas);
var
  ButtonState: TscsCtrlState;
  R, TR: TRect;
  FC: TColor;
  S: String;
  FImageIndex, FImageWidth, FImageHeight, FImageOffset: Integer;
  ImageR: TRect;
  FocusIndex: Integer;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
begin
  FocusIndex := SendMessage(Handle, TCM_GETCURFOCUS, 0, 0);
  if FocusIndex = Pages[TI].TabIndex then
    ButtonState := scsHot
  else
  if MouseIn then
    ButtonState := scsHot
  else
    ButtonState := scsNormal;
  if Active then ButtonState := scsPressed;
  if not ((ButtonState = scsNormal) and not FShowInActiveTab) then
    scDrawUtils.DrawButton(Cnvs, Rct, ButtonState, False);
  Cnvs.Font.Assign(Self.Font);
  FC := ColorToRGB(Cnvs.Font.Color);
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if (ButtonState = scsNormal) and not FShowInActiveTab then
      FC := ColorToRGB(GetCheckBoxTextColor(ButtonState))
    else
      FC := ColorToRGB(GetButtonTextColor(ButtonState));
    SetTextColor(Cnvs.Handle, FC);
  end;
  TR := Rct;
  if FShowCloseButtons then
  begin
    R := TR;
    R.Left := R.Right - FCloseSize - 2;
    Dec(R.Right, 2);
    TR.Right := R.Left + 2;
    if not MouseIn then
      TscTabSheet(Pages[TI]).CloseButtonMouseIn := False;
    DrawCloseButton(Cnvs, R, TI, FC);
  end;
  InflateRect(TR, -1, -1);
  if Assigned(OnDrawTab) then
  begin
    SetBkMode(Cnvs.Handle, TRANSPARENT);
    SetTextColor(Cnvs.Handle, FC);
    OnDrawTab(TI, TR, Active, MouseIn, Cnvs);
    if Focused and Active and FShowFocusRect then
    begin
      R := Rct;
      InflateRect(R, -4, -4);
      scDrawFocusRect2(Cnvs, R, FC);
    end;
    Exit;
  end;
  if FTabExtendedDraw then
  begin
    SetBkMode(Cnvs.Handle, TRANSPARENT);
    SetTextColor(Cnvs.Handle, FC);
    S := Pages[TI].Caption;
    if FTabGlowEffect.Enabled and GetGlowParams(ButtonState, FGlowColor, FGlowSize, FGlowAlpha) then
      DrawImageAndTextWithGlow(Cnvs, TR, FTabMargin, FTabSpacing, FTabLayout,
        S, Pages[TI].ImageIndex, Images,
        Pages[TI].Enabled, False, clBlack,
        FTabGlowEffect.Offset, FGlowColor, FGlowSize, FTabGlowEffect.Intensive,
          FGlowAlpha, FTabImageGlow, False, IsRightToLeft, False, FScaleFactor)
    else
      DrawImageAndText(Cnvs, TR, FTabMargin, FTabSpacing, FTabLayout,
       S, Pages[TI].ImageIndex, Images,
        Pages[TI].Enabled, False, clBlack, False, IsRightToLeft, False, FScaleFactor);
    if Focused and Active and FShowFocusRect then
    begin
      R := Rct;
      InflateRect(R, -4, -4);
      scDrawFocusRect2(Cnvs, R, FC, FScaleFactor);
    end;
    Exit;
  end;
  FImageIndex := Pages[TI].ImageIndex;
  if (Images <> nil) and (FImageIndex >= 0) and (FImageIndex < Images.Count) then
  begin
    FImageWidth := Images.Width;
    FImageHeight := Images.Height;
    FImageOffset := 3;
  end
  else
  begin
    FImageWidth := 0;
    FImageHeight := 0;
    FImageOffset := 0;
  end;
  if (Images <> nil) and (FImageWidth > 0) then
  begin
    ImageR := TR;
    ImageR.Left := ImageR.Left + FImageOffset;
    ImageR.Right := ImageR.Left + FImageWidth;
    TR.Left := ImageR.Right;
    ImageR.Top := ImageR.Top + (ImageR.Bottom - ImageR.Top) div 2 - FImageHeight div 2;
    Images.Draw(Cnvs, ImageR.Left, ImageR.Top, FImageIndex, True);
  end;
  SetBkMode(Cnvs.Handle, TRANSPARENT);
  SetTextColor(Cnvs.Handle, FC);
  S := Pages[TI].Caption;
  DrawText(Cnvs.Handle, PChar(S), Length(S), TR,
   scDrawTextBidiModeFlags(DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_NOCLIP,
      BidiMode = bdRightToLeft));
  if Focused and Active and FShowFocusRect then
  begin
    R := Rct;
    InflateRect(R, -4, -4);
    scDrawFocusRect2(Cnvs, R, FC, FScaleFactor);
  end;
end;

procedure TscPageControl.DrawStyleFlatButton(TI: Integer; const Rct: TRect;
      Active, MouseIn: Boolean; Cnvs: TCanvas);
var
  ButtonState: TscsCtrlState;
  R, TR: TRect;
  FC: TColor;
  S: String;
  FImageIndex, FImageWidth, FImageHeight, FImageOffset: Integer;
  ImageR: TRect;
  FocusIndex: Integer;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
begin
  FocusIndex := SendMessage(Handle, TCM_GETCURFOCUS, 0, 0);
  if FocusIndex = Pages[TI].TabIndex then
    ButtonState := scsHot
  else
  if MouseIn then
    ButtonState := scsHot
  else
    ButtonState := scsNormal;
  if Active then ButtonState := scsPressed;
  if ButtonState <> scsNormal then
    scDrawUtils.DrawToolButton(Cnvs, Rct, ButtonState);
  Cnvs.Font.Assign(Self.Font);
  FC := ColorToRGB(Cnvs.Font.Color);
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if ButtonState = scsNormal then
      FC := ColorToRGB(GetCheckBoxTextColor(ButtonState))
    else
      FC := ColorToRGB(GetToolButtonTextColor(ButtonState));
    SetTextColor(Cnvs.Handle, FC);
  end;
  TR := Rct;
  if FShowCloseButtons then
  begin
    R := TR;
    R.Left := R.Right - FCloseSize - 2;
    Dec(R.Right, 2);
    TR.Right := R.Left + 2;
    if not MouseIn then
      TscTabSheet(Pages[TI]).CloseButtonMouseIn := False;
    DrawCloseButton(Cnvs, R, TI, FC);
  end;
  InflateRect(TR, -1, -1);
  if Assigned(OnDrawTab) then
  begin
    SetBkMode(Cnvs.Handle, TRANSPARENT);
    SetTextColor(Cnvs.Handle, FC);
    OnDrawTab(TI, TR, Active, MouseIn, Cnvs);
    if Focused and Active and FShowFocusRect then
    begin
      R := Rct;
      InflateRect(R, -4, -4);
      scDrawFocusRect2(Cnvs, R, FC, FScaleFactor);
    end;
    Exit;
  end;
  if FTabExtendedDraw then
  begin
    SetBkMode(Cnvs.Handle, TRANSPARENT);
    SetTextColor(Cnvs.Handle, FC);
    S := Pages[TI].Caption;
    if FTabGlowEffect.Enabled and GetGlowParams(ButtonState, FGlowColor, FGlowSize, FGlowAlpha) then
      DrawImageAndTextWithGlow(Cnvs, TR, FTabMargin, FTabSpacing, FTabLayout,
        S, Pages[TI].ImageIndex, Images,
        Pages[TI].Enabled, False, clBlack,
        FTabGlowEffect.Offset, FGlowColor, FGlowSize, FTabGlowEffect.Intensive,
          FGlowAlpha, FTabImageGlow, False, IsRightToLeft, False, FScaleFactor)
    else
     DrawImageAndText(Cnvs, TR, FTabMargin, FTabSpacing, FTabLayout,
       S, Pages[TI].ImageIndex, Images,
        Pages[TI].Enabled, False, clBlack, False, IsRightToLeft, False, FScaleFactor);
    if Focused and Active and FShowFocusRect then
    begin
      R := Rct;
      InflateRect(R, -4, -4);
      scDrawFocusRect2(Cnvs, R, FC, FScaleFactor);
    end;
    Exit;
  end;
  FImageIndex := Pages[TI].ImageIndex;
  if (Images <> nil) and (FImageIndex >= 0) and (FImageIndex < Images.Count) then
  begin
    FImageWidth := Images.Width;
    FImageHeight := Images.Height;
    FImageOffset := 3;
  end
  else
  begin
    FImageWidth := 0;
    FImageHeight := 0;
    FImageOffset := 0;
  end;
  if (Images <> nil) and (FImageWidth > 0) then
  begin
    ImageR := TR;
    ImageR.Left := ImageR.Left + FImageOffset;
    ImageR.Right := ImageR.Left + FImageWidth;
    TR.Left := ImageR.Right;
    ImageR.Top := ImageR.Top + (ImageR.Bottom - ImageR.Top) div 2 - FImageHeight div 2;
    Images.Draw(Cnvs, ImageR.Left, ImageR.Top, FImageIndex, True);
  end;
  SetBkMode(Cnvs.Handle, TRANSPARENT);
  SetTextColor(Cnvs.Handle, FC);
  S := Pages[TI].Caption;
  DrawText(Cnvs.Handle, PChar(S), Length(S), TR,
   scDrawTextBidiModeFlags(DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_NOCLIP,
      BidiMode = bdRightToLeft));
  if Focused and Active and FShowFocusRect then
  begin
    R := Rct;
    InflateRect(R, -4, -4);
    scDrawFocusRect2(Cnvs, R, FC, FScaleFactor);
  end;
end;

procedure TscListBox.AddMRUItem(Value: String);
var
  I: Integer;
begin
  if Value = '' then Exit;
  I := Items.IndexOf(Value);
  if I <> -1
  then
    Items.Move(I, 0)
  else
    Items.Insert(0, Value);
end;

constructor TscCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  Style := lbOwnerDrawFixed;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FCheckBoxWidth := 13;
  FImages := nil;
  FImageIndex := -1;
  FWordBreak := False;
  FTitleDivider := '';
  FShowLines := False;
  FLineColor := clBtnFace;
  FShowFocusRect := True;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
  FTabWidths := TStringList.Create;
end;

destructor TscCheckListBox.Destroy;
begin
  FTabWidths.Free;
  inherited;
end;

procedure TscCheckListBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  IH: Integer;
begin
  IH := MulDiv(ItemHeight, M, D);
  inherited;
  ItemHeight := IH;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscCheckListBox.KeyPress(var Key: Char);
var
  R: TRect;
  K: Char;
begin
  K := Key;
  inherited KeyPress(Key);
  if IsWindowsXP and (K = ' ') and (ItemIndex <> -1) then
  begin
    R := ItemRect(ItemIndex);
    InvalidateRect(Handle, R, False);
  end;
end;

procedure TscCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
  R: TRect;
begin
  if (FCheckBoxWidth > 13) then
  begin
    Index := ItemAtPos(Point(X, Y),True);
    if not UseRightToLeftAlignment then
    begin
      if X < FCheckBoxWidth + 4 then
        X := 10;
    end
    else
    begin
      if X > ItemRect(Index).Right - FCheckBoxWidth - 4 then
        X := ItemRect(Index).Right - 10;
    end;
  end;
  inherited;
  if IsWindowsXP and (Button = mbLeft) then
  begin
    Index := ItemAtPos(Point(X,Y),True);
    R := ItemRect(Index);
    InvalidateRect(Handle, R, False);
  end;
end;

procedure TscCheckListBox.SetTabWidths(Value: TStrings);
begin
  if FTabWidths <> Value then
  begin
    FTabWidths.Assign(Value);
    Invalidate;
  end;
end;

procedure TscCheckListBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscCheckListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);

procedure PaintBG(DC: HDC);
var
  FCanvas: TCanvas;
  I, J, W, H, IH: Integer;
  R: TRect;
  DrawCount: Integer;
begin
  J := 0;
  W := ClientWidth;
  H := ClientHeight;
  if (Count > 0) and (ItemHeight > 0) then
  begin
    IH := ItemHeight;
    DrawCount := H div ItemHeight;
    I := TopIndex;
    J := I + DrawCount;
    if J > Count - 1 then
      J := Count - 1;
    J := J - TopIndex + 1;
    if J < 0 then J := 0;
  end
  else
    IH := 0;
  R := Rect(0, j * IH, W, H);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := DC;
  try
    FCanvas.Brush.Color := Self.Brush.Color;
    FCanvas.FillRect(R);
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
end;

begin
  if not IsCustomStyle and (Columns = 0) then
  begin
    PaintBG(Message.DC);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TscCheckListBox.WMSIZE(var Msg: TMessage);
begin
  inherited;
  RePaint;
end;

procedure TscCheckListBox.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;


procedure TscCheckListBox.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

function TscCheckListBox.CanDrawFocusRect: Boolean;
begin
  Result := FShowFocusRect;
end;

procedure TscCheckListBox.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TscCheckListBox.SetShowLines(Value: Boolean);
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    Invalidate;
  end;
end;

procedure TscCheckListBox.SetTitleDivider(Value: String);
begin
  if FTitleDivider <> Value then
  begin
    FTitleDivider := Value;
    Invalidate;
  end;
end;

procedure TscCheckListBox.SetWordBreak(Value: Boolean);
begin
  if FWordBreak <> Value then
  begin
    FWordBreak := Value;
    Invalidate;
  end;
end;

procedure TscCheckListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TscCheckListBox.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TscCheckListBox.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TscCheckListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  R, R1, FR: TRect;
  Buffer: TBitmap;
  IIndex: Integer;
  C: TColor;
begin
  with Message.DrawItemStruct^ do
  begin
    if rcItem.Height * rcItem.Width = 0 then
    begin
      Message.Result := 1;
      Exit;
    end;
    IIndex := itemID;
    State := TOwnerDrawState(LoWord(itemState));
    if (csPaintCopy in ControlState) then
    begin
      if not MultiSelect and (IIndex = ItemIndex) and not (odSelected in State) then
        Include(State, odSelected)
      else
      if MultiSelect and (IIndex >= 0) and (IIndex < Items.Count) and
         Selected[IIndex] and not (odSelected in State)
      then
        Include(State, odSelected);
    end;
    Buffer := TBitmap.Create;
    try
      FCheckBoxWidth := GetCheckBoxSize(Buffer.Canvas, FScaleFactor);
      Canvas.Handle := hDC;
      Buffer.SetSize(rcItem.Width, rcItem.Height);
      Buffer.Canvas.Font := Font;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      {$IFNDEF VER230}
      if (seClient in StyleElements) and IsCustomStyle then
        Buffer.Canvas.Brush.Color := GetEditBrushColor(scsNormal)
      else
        Buffer.Canvas.Brush.Color := Self.Color;
      {$ELSE}
      if IsCustomStyle then
        Buffer.Canvas.Brush.Color := GetEditBrushColor(scsNormal)
      else
        Buffer.Canvas.Brush.Color := Self.Color;
      {$ENDIF}
      Buffer.Canvas.FillRect(R);
      R1 := R;
      if Bidimode = bdRightToLeft then
      begin
        Dec(R1.Right, 2);
        R1.Left := R1.Right - FCheckBoxWidth - 2;
        R.Right := R1.Left - 2;
      end
      else
      begin
        R1.Left := 2;
        R1.Right := R1.Left + FCheckBoxWidth + 2;
        R.Left := R1.Right + 2;
      end;

      if (IIndex >= 0) and (IIndex < Items.Count) then
        if Checked[IIndex] then
          DrawCheckBoxInCenter(Buffer.Canvas, R1, scsNormal, cbChecked, FScaleFactor)
        else
          DrawCheckBoxInCenter(Buffer.Canvas, R1, scsNormal, cbUnChecked, FScaleFactor);

      with Buffer.Canvas do
      begin
        if (FSelectionStyle = scstColor) and
           (odSelected in State)
        then
        begin
          if FSelectionColor <> clNone then
          begin
            Font.Color := FSelectionTextColor;
            Brush.Color := FSelectionColor;
          end
          else
          begin
            Font.Color := GetStyleColor(clHighLightText);
            Brush.Color := GetStyleColor(clHighLight);
          end;
        end
        else
        begin
          if {$IFNDEF VER230}(seClient in StyleElements) and {$ENDIF} IsCustomStyle then
          begin
            if Enabled then
            begin
              {$IFNDEF VER230}
              if seFont in StyleElements then
                Font.Color := GetEditTextColor(scsNormal)
              else
                Font.Color := Self.Font.Color;
              {$ELSE}
              Font.Color := GetEditTextColor(scsNormal);
              {$ENDIF}
            end
            else
            begin
              Font.Color := GetEditTextColor(scsDisabled);
            end;
            Brush.Color := GetEditBrushColor(scsNormal);
          end
          else
          begin
            Font.Color := Self.Font.Color;
            Brush.Color := Self.Color;
            if not Enabled then
              if IsCustomStyle then
                Font.Color := GetEditTextColor(scsDisabled)
              else
                Font.Color := clGrayText;
          end;
        end;
         if (SelectionStyle = scstColor) and (odSelected in State)
           and not Focused and not FShowFocusRect then
        begin
          C := Brush.Color;
          Brush.Color := Self.Color;
          FillRect(R);
          Brush.Color := C;
          FillRectWithAlpha(Buffer.Canvas, R, 200);
        end
        else
          FillRect(R);
        if FShowLines and not (odSelected in State) then
        begin
          Pen.Color := GetStyleColor(FLineColor);
          MoveTo(0, Buffer.Height - 1);
          LineTo(Buffer.Width, Buffer.Height -1);
        end;
      end;
      if (odSelected in State) then
      begin
        if FSelectionStyle = scstStyled then
        begin
          Buffer.Canvas.Font.Color := GetSelectionTextColor;
          DrawSelection(Buffer.Canvas, R, Focused and (odFocused in State), FShowFocusRect);
        end;
      end;
      if Focused and (odFocused in State) then
      begin
        if CanDrawFocusRect or (IIndex < 0) then
        begin
          FR := R;
          if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
             and (FSelectionStyle = scstStyled) then
            InflateRect(FR, -1, -1);
          scDrawFocusRect(Buffer.Canvas, FR, FScaleFactor);
        end;
      end;
      Buffer.Canvas.Brush.Style := bsClear;
      Inc(R.Left);
      Dec(R.Right);
      if FScaleFactor >= 1.5 then
      begin
        Inc(R.Left);
        Dec(R.Right);
      end;
      if IIndex >= 0 then
        if Assigned(FOnDrawItem) then
          FOnDrawItem(IIndex, State, Buffer.Canvas, R)
        else
          DrawItemContent(Buffer.Canvas, R, IIndex, State);
      Canvas.Draw(rcItem.Left, rcItem.Top, Buffer);
    finally
      Buffer.Free;
      Canvas.Handle := 0;
    end;
  end;
  Message.Result := 1;
end;

procedure TscCheckListBox.DrawItemContent(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AState: TOwnerDrawState);
var
  R, R1: TRect;
  IIndex: Integer;
  X, Y: Integer;
  S, S1: String;
  IL: TCustomImageList;
begin
  FCheckBoxWidth := GetCheckBoxSize(ACanvas, FScaleFactor);
  IL := Images;
  IIndex := -1;
  if Assigned(FOnGetDrawItemParams) then
    FOnGetDrawItemParams(AIndex, AState, ACanvas, IL, IIndex);
  R := ARect;
  Inc(R.Left, 2);
  Dec(R.Right, 2);
  Dec(R.Bottom);
  if IL <> nil then
  begin
    if BidiMode = bdRightToLeft then
    begin
      X := R.Right - FImages.Width;
      Y := R.Top + R.Height div 2 - FImages.Height div 2;
    end
    else
    begin
      X := R.Left;
      Y := R.Top + R.Height div 2 - FImages.Height div 2;
    end;
    if IIndex = -1 then
      if FImageIndex = -1 then
        IIndex := AIndex
      else
        IIndex := FImageIndex;
    if (IIndex >= 0) and (IIndex < FImages.Count) then
      FImages.Draw(ACanvas, X, Y, IIndex, True);
    if BidiMode = bdRightToLeft then
      Dec(R.Right, FImages.Width + 5)
    else
      Inc(R.Left, FImages.Width + 5);
  end;
  S := Items[AIndex];
  if FTitleDivider <> '' then
  begin
    X := Pos(FTitleDivider, S);
    if X <> 0 then
    begin
      S1 := Copy(S, 1, X - 1);
      Delete(S, 1, X);
      R1 := R;
      Inc(R1.Top, 2);
      R1.Bottom := R1.Top + ACanvas.TextHeight('Wq');
      ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
      scDrawText(ACanvas, S1, R1, IsRightToLeft, True);
      ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
      R.Top := R1.Bottom;
      Inc(R.Left, 2);
    end;
  end;

  if FTabWidths.Count > 0 then
    scDrawUtils.DrawTabbedString(S, FTabWidths, ACanvas, R, 0)
  else
  if not FWordBreak
  then
    scDrawText(ACanvas, S, R, IsRightToLeft, True)
  else
    begin
      R1 := Rect(0, 0, R.Width, R.Height);
      DrawText(ACanvas.Handle,
        PChar(S), Length(S), R1, DT_VCENTER or DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
      X := R.Left;
      Y := R.Top + R.Height div 2 - R1.Height div 2;
      R := Rect(X, Y, X + R1.Width, Y + R1.Height);
      DrawText(ACanvas.Handle,
        PChar(S), Length(S), R, scDrawTextBidiModeFlags(DT_VCENTER or DT_LEFT or DT_WORDBREAK, BidiMode = bdRightToLeft));
    end;
end;

constructor TscMemo.Create(AOwner: TComponent);
begin
  inherited;
  FSizeChanging := False;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FFluentUIOpaque := False;
  FStopRedraw := False;
  FWallpaperIndex := -1;
  FStopGetParentBG := False;
  FSelLen := 0;
  FSelPos := 0;
  ParentBGBuffer := nil;
  FWallPapers := nil;
  FCustomImages := nil;
  FCustomBackgroundImageNormalIndex := -1;
  FCustomBackgroundImageHotIndex := -1;
  FCustomBackgroundImageDisabledIndex := -1;
  {$IFDEF VER230}
  FStyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
end;

destructor TscMemo.Destroy;
begin
  if ParentBGBuffer <> nil then
    ParentBGBuffer.Free;
  inherited;
end;

procedure TscMemo.CreateWnd;
begin
  inherited;
  if IsFluentUIOpaque then
  begin
    FTransparent := False;
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
       SetTimer(Handle, 101, 10, nil);
  end;
end;

procedure TscMemo.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

function TscMemo.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscMemo.WMSIZE(var Msg: TMessage);
begin
  if FStopRedraw then
  begin
    FStopRedraw := False;
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
  end;
  FSizeChanging := True;
  inherited;
  FSizeChanging := False;
end;

procedure TscMemo.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscMemo.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    Invalidate;
  end;
end;

procedure TscMemo.SetCustomBackgroundImageNormalIndex(Value: Integer);
begin
  if FCustomBackgroundImageNormalIndex <> Value then
  begin
    FCustomBackgroundImageNormalIndex := Value;
    Invalidate;
  end;
end;

procedure TscMemo.SetCustomBackgroundImageHotIndex(Value: Integer);
begin
  if FCustomBackgroundImageHotIndex <> Value then
  begin
    FCustomBackgroundImageHotIndex := Value;
    Invalidate;
  end;
end;

procedure TscMemo.SetCustomBackgroundImageDisabledIndex(Value: Integer);
begin
  if FCustomBackgroundImageDisabledIndex <> Value then
  begin
    FCustomBackgroundImageDisabledIndex := Value;
    Invalidate;
  end;
end;

procedure TscMemo.GetParentBG;
var
  R: TRect;
  P1, P2: TPoint;
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
          P1 := Point(0, 0);
          P2 := Point(Left, Top);
          P1 := ClientToScreen(P1);
          P2 := Parent.ClientToScreen(P2);
          P1.X := P1.X - P2.X;
          P1.Y := P1.Y - P2.Y;
          R.Left := Self.Left + P1.X;
          R.Top := Self.Top + P1.Y;
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
          P1 := Point(0, 0);
          P2 := Point(Left, Top);
          P1 := ClientToScreen(P1);
          P2 := Parent.ClientToScreen(P2);
          P1.X := P1.X - P2.X;
          P1.Y := P1.Y - P2.Y;
          R.Left := Self.Left + P1.X;
          R.Top := Self.Top + P1.Y;
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

procedure TscMemo.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    FStopGetParentBG := False;
    GetParentBG;
    DoPaint;
  end;
end;

procedure TscMemo.SetRedraw(Value: Boolean);
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

procedure TscMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
   FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
   FCustomImages := nil;
end;

procedure TscMemo.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    Invalidate;
  end;
end;

procedure TscMemo.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    Invalidate;
  end;
end;

procedure TscMemo.WMMove(var Msg: TMessage);
begin
  inherited;
  if FTransparent then
  begin
    FStopGetParentBG := False;
    DoPaint;
  end;
end;

procedure TscMemo.WMEraseBkgnd(var Message: TWMEraseBkgnd);
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

procedure TscMemo.DoPaint;
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

procedure TscMemo.DoPaint2(DC: HDC);
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

procedure TscMemo.DrawBackGround(ACanvas: TCanvas);
var
  IIndex: Integer;
  R: TRect;
begin
  with ACanvas do
  begin
    if (seClient in StyleElements) and IsCustomStyle then
      Brush.Color := GetStyleColor(clWindow)
    else
      Brush.Color := Self.Color;
    if FTransparent then
    begin
      GetParentBG;
      if ParentBGBuffer <> nil then
        ACanvas.Draw(0, 0, ParentBGBuffer);
    end
    else
      FillRect(ClientRect);
  end;
  if (FCustomImages <> nil) then
  begin
    if not Self.Enabled then
      IIndex := FCustomBackgroundImageDisabledIndex
    else
      if Focused then
        IIndex := FCustomBackgroundImageHotIndex
      else
        IIndex := FCustomBackgroundImageNormalIndex;
    if FCustomImages.IsIndexAvailable(IIndex)
    then
    begin
      R := ClientRect;
      FCustomImages.Draw(ACanvas, R, IIndex, FScaleFactor);
    end;
  end;
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
     FWallpapers.Draw(ACanvas, ClientRect, FWallpaperIndex, FScaleFactor);
end;

procedure TscMemo.WMPaint(var Message: TWMPaint);
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
      R := ClientRect;
      InflateRect(R, -1, -1);
      Inc(R.Left, 3);
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

procedure TscMemo.CreateParams(var Params: TCreateParams);
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

procedure TscMemo.WMVSCROLL(var Message: TWMVScroll);
begin
  SetRedraw(False);
  inherited;
  SetRedraw(True);
  DoPaint;
end;

procedure TscMemo.WMHSCROLL(var Message: TWMHScroll);
begin
  SetRedraw(False);
  inherited;
  SetRedraw(True);
  DoPaint;
end;

procedure TscMemo.Change;
begin
  inherited;
  SetRedraw(True);
  FStopGetParentBG := True;
  DoPaint;
  FStopGetParentBG := False;
end;

procedure TscMemo.WMCOMMAND;
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

procedure TscMemo.WndProc(var Message: TMessage);
var
  WParam: Integer;
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
        if FUseFontColorToStyleColor and Enabled then
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
        if FUseFontColorToStyleColor and Enabled then
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
    WM_LBUTTONUP:
    begin
      FSelPos := 0;
      FSelLen := 0;
    end;
    WM_MOUSEMOVE:
    begin
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

procedure TscMemo.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  if FTransparent then
  begin
    FStopGetParentBG := False;
    DoPaint;
  end;
end;

constructor TscEditButton.Create;
begin
  {$IFDEF VER330_UP}
  ParentControl := nil;
  {$ENDIF}
  FEnabled := True;
  FVisible := False;
  FHint := '';
  FShowHint := False;
  FImageIndex := -1;
  FImageHotIndex := -1;
  FImagePressedIndex := -1;
  FDropDownMenu := nil;
  FWidth := 18;
  FShowEllipses := False;
  FComboButton := False;
  FRepeatClick := False;
  FRepeatClickInterval := 200;
  FCustomImageNormalIndex := -1;
  FCustomImageHotIndex := -1;
  FCustomImagePressedIndex := -1;
  FCustomImageDisabledIndex := -1;
end;

procedure TscEditButton.SetCustomImageNormalIndex(Value: Integer);
begin
  if FCustomImageNormalIndex <> Value then
  begin
    FCustomImageNormalIndex := Value;
    Changed;
  end;
end;

procedure TscEditButton.SetCustomImageHotIndex(Value: Integer);
begin
  if FCustomImageHotIndex <> Value then
  begin
    FCustomImageHotIndex := Value;
    Changed;
  end;
end;

procedure TscEditButton.SetCustomImagePressedIndex(Value: Integer);
begin
  if FCustomImagePressedIndex <> Value then
  begin
    FCustomImagePressedIndex := Value;
    Changed;
  end;
end;

procedure TscEditButton.SetCustomImageDisabledIndex(Value: Integer);
begin
  if FCustomImageDisabledIndex <> Value then
  begin
    FCustomImageDisabledIndex := Value;
    Changed;
  end;
end;

procedure TscEditButton.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TscEditButton.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnVisibleChange) then FOnVisibleChange(Self);
  end;
end;

procedure TscEditButton.SetStyleKind(Value: TscButtonStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    Changed;
  end;
end;

function TscEditButton.GetWidth: Integer;
begin
  Result := FWidth;
  {$IFDEF VER330_UP}
  if ComboButton and (ParentControl <> nil) then
    Result := ParentControl.GetSystemMetrics(SM_CXVSCROLL);
  {$ELSE}
  if ComboButton then
    Result := GetSystemMetrics(SM_CXVSCROLL);
  {$ENDIF}
  if StyleServices.Enabled and ComboButton and not
    IsCustomStyle and not IsWindowsXP
  then
    Dec(Result, 2);
end;

procedure TscEditButton.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value > 10) then
  begin
    FWidth := Value;
    if FVisible then
      if Assigned(FOnVisibleChange) then FOnVisibleChange(Self);
  end;
end;

procedure TscEditButton.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TscEditButton.SetComboButton(Value: Boolean);
begin
  if FComboButton <> Value then
  begin
    FComboButton := Value;
    Changed;
  end;
end;

procedure TscEditButton.SetShowEllipses(Value: Boolean);
begin
  if FShowEllipses <> Value then
  begin
    FShowEllipses := Value;
    Changed;
  end;
end;

procedure TscEditButton.Assign(Source: TPersistent);
begin
  if Source is TscEditButton then
  begin
    FEnabled := TscEditButton(Source).Enabled;
    FVisible := TscEditButton(Source).Visible;
    FImageIndex := TscEditButton(Source).ImageIndex;
    FImageHotIndex := TscEditButton(Source).ImageHotIndex;
    FImagePressedIndex := TscEditButton(Source).ImagePressedIndex;
    FShowEllipses := TscEditButton(Source).ShowEllipses;
    FWidth := TscEditButton(Source).Width;
    FDropDownMenu := TscEditButton(Source).DropDownMenu;
    FCustomImageNormalIndex := TscEditButton(Source).CustomImageNormalIndex;
    FCustomImageHotIndex := TscEditButton(Source).CustomImageHotIndex;
    FCustomImagePressedIndex := TscEditButton(Source).CustomImagePressedIndex;
    FCustomImageDisabledIndex := TscEditButton(Source).CustomImageDisabledIndex;
    FHint := TscEditButton(Source).Hint;
    FShowHint := TscEditButton(Source).ShowHint;
  end
  else
    inherited Assign(Source);
end;

procedure TscEditButton.Changed;
begin
  if FVisible then
    if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscCustomEdit.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FCustomDraw := False;
  FMenuTracking := False;
  FButtonsSystemSize := False;
  FFluentUIOpaque := False;
  FUseFontColorToStyleColor := False;
  FPromptText := '';
  FHidePromptTextIfFocused := False;
  FPromptTextColor := clNone;
  FInheritedKeys := False;
  FStopGetParentBG := False;
  FLeftButton := TscEditButton.Create;
  {$IFDEF VER330_UP}
  FLeftButton.ParentControl := Self;
  {$ENDIF}
  FLeftButton.OnChange := OnButtonChange;
  FLeftButton.OnVisibleChange := OnButtonVisibleChange;
  FRightButton := TscEditButton.Create;
  {$IFDEF VER330_UP}
  FRightButton.ParentControl := Self;
  {$ENDIF}
  FRightButton.OnChange := OnButtonChange;
  FRightButton.OnVisibleChange := OnButtonVisibleChange;
  ParentBGBuffer := nil;
  FFrameColor := clBtnShadow;
  FFrameActiveColor := clHighLight;
  FRepeatClickTimer := nil;
  FWallpapers := nil;
  FWallpaperIndex := -1;
  FContentMarginLeft := 0;
  FContentMarginRight := 0;
  FContentMarginTop := 0;
  FContentMarginBottom := 0;
  FCustomImages := nil;
  FCustomBackgroundImageNormalIndex := -1;
  FCustomBackgroundImageHotIndex := -1;
  FCustomBackgroundImageDisabledIndex := -1;
  {$IFDEF VER230}
  FStyleElements := [seFont, seClient, seBorder];
  {$ENDIF}
end;

procedure TscCustomEdit.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    Invalidate;
  end;
end;

function TscCustomEdit.CanScaleButtons: Boolean;
begin
  Result := True;
end;

procedure TscCustomEdit.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
  if CanScaleButtons then
  begin
    LeftButton.Width := MulDiv(LeftButton.Width, M, D);
    RightButton.Width := MulDiv(RightButton.Width, M, D);
  end
  else
  if FButtonsSystemSize then
  begin
    {$IFDEF VER330_UP}
    LeftButton.Width := GetSystemMetrics(SM_CXVSCROLL);
    RightButton.Width := LeftButton.Width;
    {$ENDIF}
  end;
  FContentMarginLeft := MulDiv(FContentMarginLeft, M, D);
  FContentMarginTop := MulDiv(FContentMarginTop, M, D);
  FContentMarginRight := MulDiv(FContentMarginRight, M, D);
  FContentMarginBottom := MulDiv(FContentMarginBottom, M, D);
  inherited;
  if not (csLoading in ComponentState) then
    SetTimer(Handle, 100, 10, nil);
end;

procedure TscCustomEdit.SetPromptText(Value: String);
begin
  if FPromptText <> Value then
  begin
    FPromptText := Value;
    Invalidate;
  end;
end;

procedure TscCustomEdit.SetPromptTextColor(Value: TColor);
begin
  if FPromptTextColor <> Value then
  begin
    FPromptTextColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomEdit.WMCHAR;
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

procedure TscCustomEdit.UpdateComboButton;
begin
  if not IsWindowsXP and RightButton.ComboButton and not IsCustomStyle and
     StyleServices.Enabled and (BorderKind = scebFrame) and
     (BorderStyle <> bsNone)
  then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TscCustomEdit.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    Invalidate;
  end;
end;


procedure TscCustomEdit.SetCustomBackgroundImageNormalIndex(Value: Integer);
begin
  if FCustomBackgroundImageNormalIndex <> Value then
  begin
    FCustomBackgroundImageNormalIndex := Value;
    Invalidate;
  end;
end;

procedure TscCustomEdit.SetCustomBackgroundImageHotIndex(Value: Integer);
begin
  if FCustomBackgroundImageHotIndex <> Value then
  begin
    FCustomBackgroundImageHotIndex := Value;
    Invalidate;
  end;
end;

procedure TscCustomEdit.SetCustomBackgroundImageDisabledIndex(Value: Integer);
begin
  if FCustomBackgroundImageDisabledIndex <> Value then
  begin
    FCustomBackgroundImageDisabledIndex := Value;
    Invalidate;
  end;
end;

procedure TscCustomEdit.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    Invalidate;
  end;
end;

destructor TscCustomEdit.Destroy;
begin
  if FRepeatClickTimer <> nil then
    StopRepeatClick;
  FLeftButton.Free;
  FRightButton.Free;
  if ParentBGBuffer <> nil then
    ParentBGBuffer.Free;
  inherited;
end;

procedure TscCustomEdit.RepeatClickTimerProc(Sender: TObject);
begin
  if LeftButton.Visible and LeftButton.Down then
    if Assigned(FOnLeftButtonClick) then FOnLeftButtonClick(Self);
  if RightButton.Visible and RightButton.Down then
    if Assigned(FOnRightButtonClick) then FOnRightButtonClick(Self);
end;

procedure TscCustomEdit.WaitClickTimerProc(Sender: TObject);
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

procedure TscCustomEdit.StartRepeatClick;
begin
  if FRepeatClickTimer <> nil then FreeAndNil(FRepeatClickTimer);
  FRepeatClickTimer := TTimer.Create(Self);
  FRepeatClickTimer.Enabled := False;
  FRepeatClickTimer.OnTimer := WaitClickTimerProc;
  FRepeatClickTimer.Interval := 500;
  FRepeatClickTimer.Enabled := True;
end;

procedure TscCustomEdit.StopRepeatClick;
begin
  if FRepeatClickTimer <> nil then
  begin
    FRepeatClickTimer.Enabled := False;
    FreeAndNil(FRepeatClickTimer);
  end;
end;

procedure TscCustomEdit.WMSetFocus(var Message: TWMSETFOCUS);
begin
  inherited;
  if AutoSelect then SelectAll;
end;

function TscCustomEdit.IsCustomDraw(ADC: HDC): Boolean;
begin
  Result := FCustomDraw;
end;

procedure TscCustomEdit.SetContentMarginLeft(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginLeft <> Value) then
  begin
    FContentMarginLeft := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscCustomEdit.SetContentMarginTop(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginTop <> Value) then
  begin
    FContentMarginTop := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscCustomEdit.SetContentMarginRight(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginRight <> Value) then
  begin
    FContentMarginRight := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscCustomEdit.SetContentMarginBottom(Value: Integer);
begin
  if (Value >= 0) and (FContentMarginBottom <> Value) then
  begin
    FContentMarginBottom := Value;
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscCustomEdit.OnButtonVisibleChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    AdjustTextRect;
    Invalidate;
  end;
end;

procedure TscCustomEdit.OnButtonChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TscCustomEdit.CreateWnd;
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

procedure TscCustomEdit.WMSIZE(var Msg: TMessage);
begin
  FStopGetParentBG := False;
  inherited;
  AdjustTextRect;
  if BorderStyle <> bsNone then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TscCustomEdit.Loaded;
begin
  inherited;
  AdjustTextRect;
end;

procedure TscCustomEdit.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RedrawWindow(Handle, nil, 0, RDW_FRAME);
  end;
end;

procedure TscCustomEdit.SetFrameActiveColor(Value: TColor);
begin
  if FFrameActiveColor <> Value then
  begin
    FFrameActiveColor := Value;
    RedrawWindow(Handle, nil, 0, RDW_FRAME);
  end;
end;

procedure TscCustomEdit.GetParentBG;
var
  R: TRect;
  IIndex: Integer;
  P1, P2: TPoint;
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
          P1 := Point(0, 0);
          P2 := Point(Left, Top);
          P1 := ClientToScreen(P1);
          P2 := Parent.ClientToScreen(P2);
          P1.X := P1.X - P2.X;
          P1.Y := P1.Y - P2.Y;
          R.Left := Self.Left + P1.X;
          R.Top := Self.Top + P1.Y;
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
          P1 := Point(0, 0);
          P2 := Point(Left, Top);
          P1 := ClientToScreen(P1);
          P2 := Parent.ClientToScreen(P2);
          P1.X := P1.X - P2.X;
          P1.Y := P1.Y - P2.Y;
          R.Left := Self.Left + P1.X;
          R.Top := Self.Top + P1.Y;
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
    if (FCustomImages <> nil) then
    begin
      if not Self.Enabled then
        IIndex := FCustomBackgroundImageDisabledIndex
      else
        if Focused or FMouseIn then
          IIndex := FCustomBackgroundImageHotIndex
        else
          IIndex := FCustomBackgroundImageNormalIndex;
      if FCustomImages.IsIndexAvailable(IIndex)
      then
      begin
        R := Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height);
        FCustomImages.Draw(ParentBGBuffer.Canvas, R, IIndex, FScaleFactor);
      end;
    end;
    if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex)
    then
    begin
      R := Rect(0, 0, ParentBGBuffer.Width, ParentBGBuffer.Height);
      FWallpapers.Draw(ParentBGBuffer.Canvas, R, FWallpaperIndex, FScaleFactor);
    end;
  end;
end;

procedure TscCustomEdit.SetBorderKind(Value: TscEditBorderKind);
begin
  if FBorderKind <> Value then
  begin
    FBorderKind := Value;
    RecreateWnd;
  end;
end;

procedure TscCustomEdit.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    GetParentBG;
    Invalidate;
  end;
end;

procedure TscCustomEdit.WMMove(var Msg: TMessage);
begin
  inherited;
  if FTransparent then
  begin
    FStopGetParentBG := False;
    DoPaint;
  end;
end;

procedure TscCustomEdit.DoPaint;
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

procedure TscCustomEdit.DoPaint2(DC: HDC);
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

procedure TscCustomEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustTextRect;
end;

procedure TscCustomEdit.CMSENCPaint(var Message: TMessage);
begin
  if BorderStyle <> bsNone then
  begin
    Message.Result := SE_RESULT;
    DrawBorder(Message.WParam);
  end;
end;

procedure TscCustomEdit.PaintToDC(DC: HDC; X, Y: Integer);
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
    if Self.BorderStyle <> bsNone then
    begin
      Perform(WM_NCPAINT, DC, 0);
      case FBorderKind of
        scebFrame, scebColorFrame2:
          MoveWindowOrg(DC, 2, 2);
        scebColorFrame:
          MoveWindowOrg(DC, 1, 1);
      end;
    end;
    IntersectClipRect(DC, 0, 0, ClientWidth, ClientHeight);
    Perform(WM_PAINT, DC, 0);
    FMouseIn := FStoredMouseIn;
  finally
    RestoreDC(DC, SaveIndex);
  end;
  ControlState := ControlState - [csPaintCopy];
end;

procedure TscCustomEdit.WMPaint(var Message: TWMPaint);
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

procedure TscCustomEdit.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

function TscCustomEdit.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscCustomEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or ES_MULTILINE;
    if IsFluentUIOpaque then
      ExStyle := Exstyle or WS_EX_LAYERED;
  end;
end;

procedure TscCustomEdit.DoCustomPaint(ACanvas: TCanvas);
var
  CtrlState: TscsCtrlState;
begin
  if not Enabled then
     CtrlState := scsDisabled
   else
   if Focused then
     CtrlState := scsFocused
   else
   if FMouseIn then
     CtrlState := scsHot
   else
     CtrlState := scsNormal;
  FOnDrawBackgroundEvent(ACanvas, GetTextRect, CtrlState);
end;

procedure TscCustomEdit.DrawEditBackground(ACanvas: TCanvas);
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
    DrawEditButton(ACanvas, FLeftButton);
    DrawEditButton(ACanvas, FRightButton);
    if Assigned(FOnDrawBackgroundEvent) then
      DoCustomPaint(ACanvas);
    DrawPromptText(ACanvas);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TscCustomEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
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
          DrawEditButton(FCanvas, FLeftButton);
          DrawEditButton(FCanvas, FRightButton);
          if Assigned(FOnDrawBackgroundEvent) then
            DoCustomPaint(FCanvas);
          DrawPromptText(FCanvas);
        finally
          FCanvas.Handle := 0;
          FCanvas.Free;
        end;
      end;
    end;
end;

function TscCustomEdit.GetTextColor: TColor;
begin
  Result := Self.Font.Color;
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if FUseFontColorToStyleColor and Enabled then
      Result := ColorToRGB(GetStyleColor(Self.Font.Color))
    else
    if FTransparent then
    begin
      if Enabled then
        Result := ColorToRGB(GetCheckBoxTextColor(scsNormal))
      else
        Result := ColorToRGB(GetCheckBoxTextColor(scsDisabled));
    end
    else
     begin
       if Enabled then
         Result := ColorToRGB(GetEditTextColor(scsNormal))
        else
         Result := ColorToRGB(GetEditTextColor(scsDisabled));
     end;
  end
  else
  if not Enabled then
    Result := clGrayText;
end;

procedure TscCustomEdit.KeyPress(var Key: Char);
begin
  inherited;
end;

procedure TscCustomEdit.WndProc(var Message: TMessage);
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

procedure TscCustomEdit.Change;
begin
  inherited;
  FStopGetParentBG := True;
  DoPaint;
  FStopGetParentBG := False;
end;

procedure TscCustomEdit.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  FStopGetParentBG := False;
  DoPaint;
end;

procedure TscCustomEdit.WMNCCALCSIZE(var Message: TWMNCCalcSize);
begin
  if (BorderStyle = bsSingle) and ((BorderKind = scebBottomLine) or (BorderKind = scebBottomActiveLine))  then
    with Message.CalcSize_Params^.rgrc[0] do
    begin
      Dec(Bottom, 1);
    end
  else
  if (BorderStyle = bsSingle) and (BorderKind = scebColorFrame) then
    with Message.CalcSize_Params^.rgrc[0] do
    begin
      Inc(Left, 1);
      Inc(Top, 1);
      Dec(Right, 1);
      Dec(Bottom, 1);
    end
  else
    inherited;
end;

procedure TscCustomEdit.DrawBorder(DC: HDC);
var
  FCanvas: TCanvas;
  FHasFocus: Boolean;
  C: TColor;
  R: TRect;
  ButtonState: TscsCtrlState;
begin
  FHasFocus := Focused;
  if csPaintCopy in ControlState then
    FHasFocus := False;
  FCanvas := TCanvas.Create;
  FCanvas.Handle := DC;
  try
    if (BorderStyle = bsSingle) and ((BorderKind = scebBottomLine) or (BorderKind = scebBottomActiveLine)) then
     begin
       if (FMouseIn or FHasFocus) and (BorderKind = scebBottomActiveLine) then
         C := GetStyleColor(FFrameActiveColor)
       else
       if (seBorder in StyleElements) and IsCustomStyle then
       begin
         C := MiddleColor(GetStyleColor(clBtnFace), GetCheckBoxTextColor(scsNormal));
         C := MiddleColor(GetStyleColor(clBtnFace), C);
       end
       else
         C := FFrameColor;
       FCanvas.Pen.Color := C;
       FCanvas.MoveTo(0, Height - 1); FCanvas.LineTo(Width, Height - 1);
     end
     else
     if (BorderStyle = bsSingle) and ((BorderKind = scebColorFrame) or (BorderKind = scebColorFrame2)) then
     begin
       if FMouseIn or FHasFocus then
         C := GetStyleColor(FFrameActiveColor)
       else
         C := GetStyleColor(FFrameColor);
       R := Rect(0, 0, Width, Height);
       Frm3D(FCanvas, R, C, C);
       if BorderKind = scebColorFrame2 then
         Frm3D(FCanvas, R, C, C);
     end
     else
     begin
       ExcludeClipRect(FCanvas.Handle, 2, 2, Width - 2, Height - 2);
       if FHasFocus then
         DrawEditBorder(FCanvas, Rect(0, 0, Width, Height), scsFocused)
       else
       if FMouseIn then
         DrawEditBorder(FCanvas, Rect(0, 0, Width, Height), scsHot)
       else
         DrawEditBorder(FCanvas, Rect(0, 0, Width, Height), scsNormal);
       if RightButton.Visible and RightButton.ComboButton and not IsCustomStyle
          and StyleServices.Enabled and not IsWindowsXP
       then
       begin
         if BidiMode = bdRightToLeft then
           R := Rect(1, 1, RightButton.Width + 2, Height -1)
         else
           R := Rect(Width - RightButton.Width - 2, 1, Width - 1, Height -1);
         if RightButton.Down or  RightButton.FDropDown then
           ButtonState := scsPressed
         else
         if RightButton.MouseIn then
           ButtonState := scsHot
         else
         if RightButton.Enabled then
           ButtonState := scsNormal
         else
           ButtonState := scsDisabled;
         DrawDropDownButton(FCanvas, R, ButtonState, True, BidiMode = bdRightToLeft, FScaleFactor);
       end;
     end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
end;

procedure TscCustomEdit.WMNCPAINT(var Message: TWMNCPAINT);
var
  DC: HDC;
  SaveIndex: Integer;
begin
  if (BorderStyle = bsSingle) then
  begin
    if (csPaintCopy in ControlState) and (TMessage(Message).WParam <> 0) then
      DC := HDC(TMessage(Message).WParam)
    else
      DC := GetWindowDC(Handle);
    SaveIndex := SaveDC(DC);
    try
      DrawBorder(DC);
    finally
      RestoreDC(DC, SaveIndex);
      if not ((csPaintCopy in ControlState) and (TMessage(Message).WParam <> 0))
      then
        ReleaseDC(Handle, DC);
    end;
  end
  else
    inherited;
end;

function TscCustomEdit.GetTextRect: TRect;
var
  R: TRect;
begin
  Result := ClientRect;
  if not AutoSize then
  begin
    Inc(Result.Left, FContentMarginLeft);
    Inc(Result.Top, FContentMarginTop);
    Dec(Result.Right, FContentMarginRight);
    Dec(Result.Bottom, FContentMarginBottom);
  end;
  if not Assigned(FLeftButton) or not Assigned(FRightButton) then Exit;
  R := Result;
  FLeftButton.ButtonRect := Rect(0, 0, 0, 0);
  FRightButton.ButtonRect := Rect(0, 0, 0, 0);
  if BidiMode = bdRightToLeft then
  begin
   if FRightButton.Visible
    then
       begin
         FRightButton.ButtonRect := Rect(R.Left, R.Top, R.Left + FRightButton.Width, R.Bottom);
         Inc(R.Left, FRightButton.ButtonRect.Width + 2);
       end;
    if FLeftButton.Visible
    then
      begin
        FLeftButton.ButtonRect := Rect(R.Right - FLeftButton.Width, R.Top, R.Right, R.Bottom);
        Dec(R.Right, FLeftButton.ButtonRect.Width + 2);
      end;
  end
  else
  begin
    if FLeftButton.Visible
    then
       begin
         FLeftButton.ButtonRect := Rect(R.Left, R.Top, R.Left + FLeftButton.Width, R.Bottom);
         Inc(R.Left, LeftButton.ButtonRect.Width + 2);
       end;
    if FRightButton.Visible
    then
      begin
        FRightButton.ButtonRect := Rect(R.Right - FRightButton.Width, R.Top, R.Right, R.Bottom);
        Dec(R.Right, RightButton.ButtonRect.Width + 2);
      end;
  end;
  if (BorderStyle = bsNone) then
  begin
    Inc(R.Top, 3);
    Inc(R.Left, 2);
    Dec(R.Right, 2);
  end
  else
  if (BorderStyle = bsSingle) and ((BorderKind = scebBottomLine) or (BorderKind = scebBottomActiveLine)) then
    Inc(R.Top)
  else
  if (BorderStyle = bsSingle) and (BorderKind = scebColorFrame) then
  begin
    Inc(R.Top, 1);
    Inc(R.Left, 1);
    Dec(R.Bottom, 1);
    Dec(R.Right, 1);
  end;
  Result := R;
end;

procedure TscCustomEdit.AdjustTextRect;
var
  R: TRect;
begin
  if not HandleAllocated then Exit;
  R := GetTextRect;
  Perform(EM_SETRECTNP, 0, Longint(@R));
end;

procedure TscCustomEdit.SetButtonImages(Value: TCustomImageList);
begin
  if FButtonImages <> Value then
  begin
    FButtonImages := Value;
    Invalidate;
  end;
end;

procedure TscCustomEdit.Notification;
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
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
 if (Operation = opRemove) and (AComponent = FCustomImages) then
   FCustomImages := nil;
  if (Operation = opRemove) and (AComponent = FHintComponent) then
    FHintComponent := nil;
end;

procedure TscCustomEdit.DrawPromptText(ACanvas: TCanvas);
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

procedure TscCustomEdit.DrawEditButton(ACanvas: TCanvas; AButton: TscEditButton);
var
  ButtonState: TscsCtrlState;
  R: TRect;
  TextColor: TColor;
  X, Y, IX, IIndex: Integer;
begin
  if not AButton.Visible then Exit;

  if csPaintCopy in ControlState then
  begin
    if AButton.Enabled then
      ButtonState := scsNormal
    else
      ButtonState := scsDisabled;
  end
  else
  if AButton.Down or AButton.FDropDown then
    ButtonState := scsPressed
  else
  if AButton.MouseIn then
    ButtonState := scsHot
  else
  if AButton.Enabled then
    ButtonState := scsNormal
  else
    ButtonState := scsDisabled;

  R := AButton.ButtonRect;

  if (AButton.FComboButton) and not
     ((FButtonImages <> nil) and
      (AButton.ImageIndex >= 0) and (AButton.ImageIndex < FButtonImages.Count))  then
  begin
    if (FTransparent and not (BorderKind = scebFrame)) or (BorderKind <> scebFrame) then
    begin
      TextColor := GetTextColor;
      if AButton.Enabled then
        DrawComboArrowImage(ACanvas, R, TextColor, FScaleFactor)
      else
        DrawComboArrowImage(ACanvas, R, MiddleColor(TextColor, GetStyleColor(clBtnFace)), FScaleFactor);
    end
    else
    begin
      if not IsCustomStyle and StyleServices.Enabled and not IsWindowsXP
      then
      begin
        Dec(R.Top);
        Inc(R.Bottom);
        if BidiMode = bdRightToLeft then
          Dec(R.Left)
        else
          Inc(R.Right);
      end;
      if IsCustomStyle then
        if BidiMode = bdRightToLeft then
          Dec(R.Right)
        else
          Inc(R.Left);
      DrawDropDownButton(ACanvas, R, ButtonState, True, BidiMode = bdRightToLeft, FScaleFactor);
    end;
    Exit;
  end;
  TextColor := Self.Font.Color;
  case AButton.StyleKind of
    scbsDropDownButton:
    begin
      DrawDropDownButton(ACanvas, R, ButtonState, False, False, FScaleFactor);
    end;
    scbsUpSpinButton:
    begin
      DrawUpSpinButton(ACanvas, R, ButtonState, FScaleFactor);
    end;
    scbsLeftSpinButton:
    begin
      DrawLeftSpinButton(ACanvas, R, ButtonState, FScaleFactor);
    end;
    scbsDownSpinButton:
    begin
      DrawDownSpinButton(ACanvas, R, ButtonState, FScaleFactor);
    end;
    scbsRightSpinButton:
    begin
      DrawRightSpinButton(ACanvas, R, ButtonState, FScaleFactor);
    end;
    scbsHeaderSection:
    begin
      DrawHeaderSection(ACanvas, R, ButtonState);
      TextColor := GetHeaderTextColor(ButtonState);
    end;
    scbsCustomImageOverContent:
    begin
      TextColor := GetCheckBoxTextColor(ButtonState);
    end;
    scbsCustomImage:
    begin
      IIndex := AButton.CustomImageNormalIndex;
      case ButtonState of
        scsHot: IIndex := AButton.CustomImageHotIndex;
        scsPressed: IIndex := AButton.CustomImagePressedIndex;
        scsDisabled: IIndex := AButton.CustomImageDisabledIndex;
      end;
      TextColor := GetButtonTextColor(ButtonState);
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(IIndex)
      then
        FCustomImages.Draw(ACanvas, R, IIndex, FScaleFactor);
    end;
    scbsPushButton:
    begin
      DrawButton(ACanvas, R, ButtonState, False);
      TextColor := GetButtonTextColor(ButtonState);
    end;
    scbsToolButton:
    begin
      DrawToolButton(ACanvas, R, ButtonState);
      TextColor := GetToolButtonTextColor(ButtonState);
    end;
    scbsPushButtonTransparent:
    begin
      if (ButtonState <> scsNormal) and (ButtonState <> scsDisabled) then
      begin
        DrawButton(ACanvas, R, ButtonState, False);
        TextColor := GetButtonTextColor(ButtonState);
      end
      else
        TextColor := GetCheckBoxTextColor(ButtonState);
    end;
    scbsToolButtonTransparent:
    begin
      if (ButtonState <> scsNormal) and (ButtonState <> scsDisabled) then
      begin
        DrawToolButton(ACanvas, R, ButtonState);
        TextColor := GetToolButtonTextColor(ButtonState);
      end
      else
        TextColor := GetCheckBoxTextColor(ButtonState);
    end;
    scbsTransparent, scbsLink:
    begin
      TextColor := GetCheckBoxTextColor(ButtonState);
    end;
    else
      TextColor := GetCheckBoxTextColor(ButtonState);
  end;

 if AButton.ShowEllipses and
    (AButton.StyleKind <> scbsDropDownButton) and
    (AButton.StyleKind <> scbsUpSpinButton) and
    (AButton.StyleKind <> scbsDownSpinButton) and
    (AButton.StyleKind <> scbsLeftSpinButton) and
    (AButton.StyleKind <> scbsRightSpinButton)
  then
  begin
    ACanvas.Font := Self.Font;
    ACanvas.Font.Color := TextColor;
    if (AButton.StyleKind = scbsTransparent) or ((ButtonState = scsNormal)
       and ((AButton.StyleKind = scbsToolButtonTransparent) or
            (AButton.StyleKind = scbsPushButtonTransparent))) or
       (not IsCustomStyle and StyleServices.Enabled and
       ((AButton.StyleKind = scbsToolButton) or
         (AButton.StyleKind = scbsToolButtonTransparent)))
    then
    begin
      ACanvas.Font.Color := FTextColor;
    end;
    ACanvas.Brush.Style := bsClear;
    DrawTextAlignment(ACanvas, '...', AButton.ButtonRect, taCenter);
  end;

  if FButtonImages = nil then Exit;

  if (AButton.ImageIndex >= 0) and (AButton.ImageIndex < FButtonImages.Count)
  then
    begin
      Y := AButton.ButtonRect.Top + AButton.ButtonRect.Height div 2 -
          FButtonImages.Height div 2;
      // if Y < AButton.ButtonRect.Top then Y := AButton.ButtonRect.Top;
      X := AButton.ButtonRect.Left + AButton.ButtonRect.Width div 2 -
          FButtonImages.Width div 2;
      // if X < AButton.ButtonRect.Left then X := AButton.ButtonRect.Left;
      IX := AButton.ImageIndex;
      if AButton.Down and AButton.MouseIn and
         (AButton.ImagePressedIndex >= 0) and (AButton.ImagePressedIndex < FButtonImages.Count)
      then
        IX := AButton.ImagePressedIndex
      else
      if AButton.MouseIn and (AButton.ImageHotIndex >= 0) and
        (AButton.ImageHotIndex < FButtonImages.Count)
      then
        IX := AButton.ImageHotIndex;
      if Enabled and AButton.Enabled then
      begin
        if True then
        if AButton.Down and AButton.MouseIn and
          (AButton.ImagePressedIndex = AButton.ImageIndex)
        then
          DrawBitmapFromImageList(ACanvas, X, Y, FButtonImages, IX, 200)
        else
          FButtonImages.Draw(ACanvas, X, Y, IX, Enabled)
      end
      else
        DrawBitmapFromImageList(ACanvas, X, Y, FButtonImages, IX, DisabledImageAlphaValue);
    end;
   case AButton.StyleKind of
     scbsCustomImageOverContent:
     begin
       IIndex := AButton.CustomImageNormalIndex;
       case ButtonState of
         scsHot: IIndex := AButton.CustomImageHotIndex;
         scsPressed: IIndex := AButton.CustomImagePressedIndex;
         scsDisabled: IIndex := AButton.CustomImageDisabledIndex;
      end;
      if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(IIndex)
      then
        FCustomImages.Draw(ACanvas, R, IIndex, FScaleFactor);
    end;
  end;
end;

procedure TscCustomEdit.WMNCHITTEST(var Message: TWMNCHITTEST);
var
  P: TPoint;
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
        UpdateComboButton;
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
    UpdateComboButton;
  end;

  if Message.Result <> HTUPDOWN then
    inherited;
end;

procedure TscCustomEdit.WMNCLBUTTONDBCLK;
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
      if FLeftButton.DropDownMenu.Alignment = paCenter then
        P := ClientToScreen(Point(FLeftButton.ButtonRect.CenterPoint.X, Self.Height))
      else
      if FLeftButton.DropDownMenu.Alignment = paRight then
        P := ClientToScreen(Point(FLeftButton.ButtonRect.Right, Self.Height))
      else
        P := ClientToScreen(Point(0, Self.Height));
      FLeftButton.DropDownMenu.Popup(P.X, P.Y);
      FLeftButton.Down := False;
      //Invalidate;
      SetTimer(Handle, 22, 50, nil);
    end;
  end
  else
  if FRightButton.Visible and FRightButton.Enabled and (Message.HitTest = HTEDITBUTTONR) and not FMenuTracking then
  begin
    FRightButton.Down := True;
    if not Focused then SetFocus;
    Invalidate;
    UpdateComboButton;

    if RightButton.RepeatClick then StartRepeatClick;
     if FRightButton.FComboButton then
       if Assigned(FOnRightButtonClick) then FOnRightButtonClick(Self);
    if FRightButton.DropDownMenu <> nil then
    begin
      FMenuTracking := True;
      if FRightButton.DropDownMenu.Alignment = paCenter then
        P := ClientToScreen(Point(FRightButton.ButtonRect.CenterPoint.X, Self.Height))
      else
      if FRightButton.DropDownMenu.Alignment = paRight then
        P := ClientToScreen(Point(FRightButton.ButtonRect.Right, Self.Height))
      else
        P := ClientToScreen(Point(0, Self.Height));
      FRightButton.DropDownMenu.Popup(P.X, P.Y);
      FRightButton.Down := False;
//      Invalidate;
      SetTimer(Handle, 22, 50, nil);
    end;
  end
  else
    inherited;
end;

procedure TscCustomEdit.WMNCLBUTTONDOWN;
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
     // Invalidate;
      SetTimer(Handle, 22, 10, nil);
    end;
  end
  else
  if FRightButton.Visible and FRightButton.Enabled and (Message.HitTest = HTEDITBUTTONR) and not FMenuTracking then
  begin
    FRightButton.Down := True;
    if not Focused then SetFocus;
    Invalidate;
    UpdateComboButton;

    if RightButton.RepeatClick then StartRepeatClick;
     if FRightButton.FComboButton then
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

procedure TscCustomEdit.WMNCLBUTTONUP;
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
   if not LeftButton.FComboButton then
    if Assigned(FOnLeftButtonClick) then FOnLeftButtonClick(Self);
  end
  else
  if FRightButton.Visible and FRightButton.Enabled and (Message.HitTest = HTEDITBUTTONR) and RightButton.Down then
  begin
    RightButton.Down := False;
    Invalidate;
    UpdateComboButton;

    if RightButton.RepeatClick then StopRepeatClick;
    if not RightButton.FComboButton then
      if Assigned(FOnRightButtonClick) then FOnRightButtonClick(Self);
  end
  else
    inherited;
end;

procedure TscCustomEdit.WMTimer(var Message: TWMTimer);
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

procedure TscCustomEdit.UpdateBorder;
begin
  if not Focused and (BorderStyle <> bsNone) and
    (BorderKind <> scebBottomLine) then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  if (FCustomBackgroundImageHotIndex <> -1) and (FCustomImages <> nil) then
    DoPaint;
end;

procedure TscCustomEdit.CMMouseLeave(var Message: TMessage);
var
  P: TPoint;
begin
  inherited;
  GetCursorPos(P);
  if WindowFromPoint(P) <> Handle then
    EditMouseLeave;
end;

procedure TscCustomEdit.EditMouseEnter;
begin
  FMouseIn := True;
  UpdateBorder;
  SetTimer(Handle, 1, 200, nil);
end;

procedure TscCustomEdit.EditMouseLeave;
begin
  FMouseIn := False;

  if (FHintComponent <> nil)  then
     FHintComponent.HideHint;

  if FLeftButton.Visible and LeftButton.MouseIn then
  begin
    LeftButton.MouseIn := False;
    if LeftButton.Down and (LeftButton.DropDownMenu = nil) then
      LeftButton.Down := False;
    DoPaint;
  end;
  if FRightButton.Visible and RightButton.MouseIn then
  begin
    RightButton.MouseIn := False;
    if RightButton.Down and (RightButton.DropDownMenu = nil) then
      RightButton.Down := False;
    DoPaint;
  end;
  UpdateComboButton;
  UpdateBorder;
end;

constructor TscNumericEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCurrencyString := '';
  FDisplayFormat := '';
  FSupportUpdownKeys := False;
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
end;

destructor TscNumericEdit.Destroy;
begin
  inherited;
end;

function TscNumericEdit.IsCustomDraw(ADC: HDC): Boolean;
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

procedure TscNumericEdit.SetCurrencyString(Value: String);
begin
  if FCurrencyString <> Value then
  begin
    FCurrencyString := Value;
    Invalidate;
  end;
end;

procedure TscNumericEdit.SetDisplayFormat(Value: String);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Invalidate;
  end;
end;

procedure TscNumericEdit.SetDisplayType(Value: TscNumEditDisplayType);
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

procedure TscNumericEdit.WMKILLFOCUS(var Message: TMessage);
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

function TscNumericEdit.GetValueAsInt: Integer;
begin
  Result := Round(FValue);
end;

procedure TscNumericEdit.SetValueType(NewType: TscValueType);
begin
  if FValueType <> NewType then
  begin
    FValueType := NewType;
  end;
end;

procedure TscNumericEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then
  begin
    FDecimal := NewValue;
  end;
end;

function TscNumericEdit.CheckValue;
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

procedure TscNumericEdit.SetMinValue;
begin
  FMinValue := AValue;
end;

procedure TscNumericEdit.SetMaxValue;
begin
  FMaxValue := AValue;
end;

function TscNumericEdit.IsNumText;

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

procedure TscNumericEdit.Change;
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

procedure TscNumericEdit.SetValue;
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

procedure TscNumericEdit.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TscNumericEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key)
  then
    begin
      Key := #0;
      MessageBeep(0)
    end;
  inherited KeyPress(Key);
end;

function TscNumericEdit.IsValidChar(Key: Char): Boolean;
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

constructor TscSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  FMouseWheelSupport := True;
  FButtonsSystemSize := True;
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

destructor TscSpinEdit.Destroy;
begin
  inherited;
end;

function  TscSpinEdit.IsModified: Boolean;
begin
  Result := Modified or FIsModified;
end;

function TscSpinEdit.CanScaleButtons: Boolean;
begin
  Result := False;
end;

procedure TscSpinEdit.WMMOUSEWHEEL(var Message: TMessage);
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

procedure TscSpinEdit.SetUpDownKind(Value: TscUpDownKind);
begin
  if FUpDownKind <> Value then
  begin
    FUpDownKind := Value;
    ShowUpDownButtons;
    AdjustTextRect;
  end;
end;

procedure TscSpinEdit.UpButtonClick(Sender: TObject);
begin
  FIsModified := True;
  Value := Value + FIncrement;
  FIsModified := False;
end;

procedure TscSpinEdit.DownButtonClick(Sender: TObject);
begin
  FIsModified := True;
  Value := Value - FIncrement;
  FIsModified := False;
end;

function TscSpinEdit.GetTextRect: TRect;
var
  R: TRect;
begin
  if FUpDownKind = scupkLeftRight then
  begin
    Result := inherited GetTextRect;
    Exit;
  end;
  Result := ClientRect;
  if not Assigned(LeftButton) or not Assigned(RightButton) then Exit;
  if not LeftButton.Visible and not RightButton.Visible then Exit;
  R := Result;
  LeftButton.ButtonRect := Rect(0, 0, 0, 0);
  RightButton.ButtonRect := Rect(0, 0, 0, 0);
  if BidiMode = bdRightToLeft then
  begin
    if RightButton.Visible then
      RightButton.ButtonRect := Rect(R.Left, R.Top,
           R.Left + RightButton.Width, R.Top + R.Height div 2);
    if LeftButton.Visible then
      LeftButton.ButtonRect := Rect(R.Left, R.Top + R.Height div 2 + 1,
          R.Left + RightButton.Width, R.Bottom);
    Inc(R.Left, LeftButton.ButtonRect.Width + 2);
  end
  else
  begin
    if RightButton.Visible then
      RightButton.ButtonRect := Rect(R.Right - RightButton.Width, R.Top,
          R.Right, R.Top + R.Height div 2);
    if LeftButton.Visible then
      LeftButton.ButtonRect := Rect(R.Right - RightButton.Width, R.Top + R.Height div 2 + 1,
           R.Right, R.Bottom);
    Dec(R.Right, RightButton.ButtonRect.Width + 2);
  end;

  if (BorderStyle = bsNone) then
  begin
    Inc(R.Top, 3);
    Inc(R.Left, 2);
    Dec(R.Right, 2);
  end
  else
  if (BorderStyle = bsSingle) and ((BorderKind = scebBottomLine) or (BorderKind = scebBottomActiveLine)) then
    Inc(R.Top)
  else
  if (BorderStyle = bsSingle) and (BorderKind = scebColorFrame) then
  begin
    Inc(R.Top);
    Inc(R.Left);
    Dec(R.Bottom);
    Dec(R.Right);
  end;
  Result := R;
end;

procedure TscSpinEdit.ShowUpDownButtons;
begin
  if FUpDownKind = scupkLeftRight then
    LeftButton.StyleKind := scbsLeftSpinButton
  else
    LeftButton.StyleKind := scbsDownSpinButton;
  LeftButton.Width := GetSystemMetrics(SM_CYHSCROLL);
  LeftButton.Visible := True;
  LeftButton.RepeatClick := True;
  LeftButton.RepeatClickInterval := 100;
  if FUpDownKind = scupkLeftRight then
    RightButton.StyleKind := scbsRightSpinButton
  else
    RightButton.StyleKind := scbsUpSpinButton;
  RightButton.Width := GetSystemMetrics(SM_CYHSCROLL);
  RightButton.Visible := True;
  RightButton.RepeatClick := True;
  RightButton.RepeatClickInterval := 100;
  OnRightButtonClick := UpButtonClick;
  OnLeftButtonClick := DownButtonClick;
end;

function TscSpinEdit.IsCustomDraw(ADC: HDC): Boolean;
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

procedure TscSpinEdit.SetDisplayType(Value: TscNumEditDisplayType);
begin
  if FDisplayType <> Value then
  begin
    FDisplayType := Value;
    if FDisplayType = scedtCurrency then
      FDecimal := 2;
    Invalidate;
  end;
end;

procedure TscSpinEdit.WMKILLFOCUS(var Message: TMessage);
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

function TscSpinEdit.GetValueAsInt: Integer;
begin
  Result := Round(FValue);
end;

procedure TscSpinEdit.SetValueType(NewType: TscValueType);
begin
  if FValueType <> NewType then
  begin
    FValueType := NewType;
  end;
end;

procedure TscSpinEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then
  begin
    FDecimal := NewValue;
  end;
end;

function TscSpinEdit.CheckValue;
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

procedure TscSpinEdit.SetMinValue;
begin
  FMinValue := AValue;
end;

procedure TscSpinEdit.SetMaxValue;
begin
  FMaxValue := AValue;
end;

function TscSpinEdit.IsNumText;

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

procedure TscSpinEdit.Change;
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

procedure TscSpinEdit.SetValue;
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

procedure TscSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TscSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key)
  then
    begin
      Key := #0;
      MessageBeep(0)
    end;
  inherited KeyPress(Key);
end;

function TscSpinEdit.IsValidChar(Key: Char): Boolean;
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

constructor TscCustomComboBoxEx.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
  FDrawSelectionWithStyles := False;
  FShowFocusRect := True;
  FComboBoxHandle := 0;
  FComboBoxInstance := MakeObjectInstance(ComboBoxWndProc);
  FDefComboBoxProc := nil;
end;

destructor TscCustomComboBoxEx.Destroy;
begin
  if FComboBoxHandle  <> 0 then
    SetWindowLong(FComboBoxHandle, GWL_WNDPROC, IntPtr(FDefComboBoxProc));
  FreeObjectInstance(FComboBoxInstance);
  inherited;
end;

procedure TscCustomComboBoxEx.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TscCustomComboBoxEx.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscCustomComboBoxEx.SetStyleKind(Value: TscComboStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBoxEx.CMSENCPaint(var Message: TMessage);
begin
  if IsCustomDraw then
    Message.Result := SE_RESULT
  else
    Message.Result := 0;
end;

procedure TscCustomComboBoxEx.CMSEPaint(var Message: TMessage);
begin
  DrawComboBox(Message.WParam);
end;

procedure TscCustomComboBoxEx.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  if IsCustomDraw then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
end;

procedure TscCustomComboBoxEx.DrawPushButtonCombo(ACanvas: TCanvas);
var
  Buffer: TBitmap;
  R, R1, BR: TRect;
  FState: TscsCtrlState;
  BW: Integer;
  FC: TColor;
  IIndex: Integer;
  FThirdImageWidth, FThirdImageHeight, FThirdImages, FThirdImagesCount: Integer;
  IconX, IconY: Integer;
  LCaption: String;
  FCanDrawFocusRect: Boolean;
begin
  Buffer := TBitmap.Create;
  try
    Buffer.Width := Width;
    Buffer.Height := Height;
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    if (Parent <> nil) and (Parent is TscCustomControl) then
       TscCustomControl(Parent).FGetControlBG := True;
    try
      DrawParentBackground(Self, Buffer.Canvas);
    finally
      if (Parent <> nil) and (Parent is TscCustomControl) then
        TscCustomControl(Parent).FGetControlBG := False;
    end;
    FCanDrawFocusRect := FShowFocusRect;
    if DroppedDown then
      FState := scsPressed
    else
    if not FCanDrawFocusRect and Focused then
       FState := scsFocused
    else
    if FMouseIn then
       FState := scsHot
    else
    if Enabled then
      FState := scsNormal
    else
      FState := scsDisabled;
    DrawButton(Buffer.Canvas, R, FState, False);
    Buffer.Canvas.Font := Font;
    FC := GetButtonTextColor(FState);
    Buffer.Canvas.Font.Color := FC;
    // draw item
    InflateRect(R, -2, -2);
    BW := GetSystemMetrics(SM_CXVSCROLL);
    if BidiMode = bdRightToLeft then
    begin
      Inc(R.Left, BW);
      BR := Rect(2, R.Top, R.Left, R.Bottom);
    end
    else
      begin
        Dec(R.Right, BW);
        BR := Rect(R.Right, R.Top, Width - 2, R.Bottom);
      end;
    R1 := R;
    Inc(R.Left, 2);
    Dec(R.Right, 2);
    Buffer.Canvas.Brush.Style := bsClear;
    // draw item
    if ItemIndex <> -1 then
    begin
       FThirdImages := SendMessage(Handle, CBEM_GETIMAGELIST, 0, 0);
        if (Images <> nil) and
           (ItemIndex <> -1) then
          begin
            if BidiMode <> bdRightToLeft then
              IconX := 5
            else
              IconX := R.Right - Images.Width;

            IconY := R.Top + R.Height div 2 - Images.Height div 2;
            if IconY < R.Top then IconY := R.Top;

            if (ItemsEx[ItemIndex].ImageIndex >= 0) and
               (ItemsEx[ItemIndex].ImageIndex < Images.Count) then
              Images.Draw(Buffer.Canvas, IconX, IconY,
                ItemsEx[ItemIndex].ImageIndex, Enabled);
            if BidiMode <> bdRightToLeft then
              R.Left := IconX + IMages.Width + 5
            else
              R.Right := R.Right - IMages.Width - 5;
          end
        else
        if (FThirdImages > 0) and (ItemIndex <> -1) then
          begin
            IIndex := ItemsEx[ItemIndex].ImageIndex;
            ImageList_GetIconSize(FThirdImages, FThirdImageWidth, FThirdImageHeight);
            FThirdImagesCount := ImageList_GetImageCount(FThirdImages);
            if (IIndex >= 0) and (IIndex < FThirdImagesCount) then
            begin
              if BidiMode <> bdRightToLeft then
                IconX := 5
               else
                IconX := R.Right - FThirdImageWidth;

              IconY := R.Top + R.Height div 2 - FThirdImageHeight div 2;
              if IconY < R.Top then IconY := R.Top;
              ImageList_DrawEx(FThirdImages, IIndex, Buffer.Canvas.Handle,
                IconX,  IconY,  FThirdImageWidth,
                 FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
              if BidiMode <> bdRightToLeft then
                R.Left := IconX + FThirdImageWidth + 5
              else
                R.Right := R.Right - FThirdImageWidth - 5;
            end;
          end
        else
        begin
          if BidiMode <> bdRightToLeft then
            Inc(R.Left, 5)
          else
            Dec(R.Right, 5);
        end;

        if (ItemIndex <> -1) then
        begin
          LCaption := ItemsEx[ItemIndex].Caption;
          if LCaption <> '' then
            DrawText(Buffer.Canvas.Handle, PWideChar(LCaption), Length(LCaption),
              R, scDrawTextBidiModeFlags(DT_LEFT OR DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft));
        end;
    end;
    //
    if Focused and (FState <> scsPressed) and FCanDrawFocusRect then
    begin
      InflateRect(R1, -1, -1);
      scDrawFocusRect(Buffer.Canvas, R1, FScaleFactor);
    end;
    if Enabled then
      FState := scsNormal
    else
      FState := scsDisabled;
    if IsCustomStyle or not StyleServices.Enabled or IsWindowsXP then
      DrawComboArrowImage(Buffer.Canvas, BR, FC, FScaleFactor)
    else
      DrawDropDownButton(Buffer.Canvas, BR, FState, True, False, FScaleFactor);
    ACanvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

function TscCustomComboBoxEx.IsCustomDrawItem: Boolean;
begin 
  Result := IsCustomDraw;
  if not Result then
  begin
    Result := TStyleManager.SystemStyle.Enabled and not IsWindowsXP;
  end;
end;

function TscCustomComboBoxEx.IsCustomDraw: Boolean;
begin
  Result := (Style = csExDropDownList) and (FStyleKind = scscbPushButton);
  if not Result then
    Result := (Style = csExDropDownList) and not IsWindowsXP and
      TStyleManager.SystemStyle.Enabled;
end;

procedure TscCustomComboBoxEx.DrawComboBox(DC: HDC);
var
  C: TCanvas;
begin
  C := TCanvas.Create;
  C.Handle := DC;
  DrawPushButtonCombo(C);
  C.Handle := 0;
  C.Free;
end;

procedure TscCustomComboBoxEx.DrawListBoxItem(ADC: HDC; ARect: TRect; AIndex: Integer; ASelected: Boolean);
var
  Canvas: TCanvas;
  Offset: Integer;
  IconX, IconY, IIndex: Integer;
  Buffer: TBitMap;
  LCaption: String;
  R: TRect;
  FThirdImageWidth, FThirdImageHeight, FThirdImages, FThirdImagesCount: Integer;
begin
  if (AIndex < 0) or (AIndex >= ItemsEx.Count) then Exit;
  Canvas := TCanvas.Create;
  Canvas.Handle := ADC;
  Buffer := TBitmap.Create;
  Buffer.Width := ARect.Width;
  Buffer.Height := ARect.Height;
  try
    Buffer.Canvas.Font.Assign(Font);
    with Buffer.Canvas do
    begin
      Brush.Style := bsSolid;
      if ASelected and not (SelectionStyle = scstStyled) then
      begin
        if SelectionColor <> clNone
        then
        begin
          Brush.Color := SelectionColor;
          Font.Color := SelectionTextColor;
        end
        else
        begin
          Brush.Color := GetStyleColor(clHighLight);
          Font.Color := GetStyleColor(clHighLightText);
        end;
      end
      else
      begin
        {$IFNDEF VER230}
        if seClient in StyleElements then
          Brush.Color := StyleServices.GetStyleColor(scComboBox)
        else
          Brush.Color := Color;
        {$ELSE}
        Brush.Color := StyleServices.GetStyleColor(scComboBox);
        {$ENDIF}
        {$IFNDEF VER230}
        if seFont in StyleElements then
        begin
          if ASelected then
            Font.Color := GetSelectionTextColor
          else
            Font.Color := StyleServices.GetStyleFontColor(sfComboBoxItemNormal);
        end
        else
          Font.Color := Font.Color;
        {$ELSE}
        if ASelected then
          Font.Color := GetSelectionTextColor
        else
          Font.Color := StyleServices.GetStyleFontColor(sfComboBoxItemNormal);
        {$ENDIF}
      end;
      FillRect(Rect(0, 0, Buffer.Width, Buffer.Height));
      if ASelected and (SelectionStyle = scstStyled) then
        DrawSelection(Buffer.Canvas, Rect(0, 0, Buffer.Width, Buffer.Height), False, True);
      Offset := TComboExItem(ItemsEx[AIndex]).Indent;
      if Offset > 0 then Offset := (Offset * 10) + 5
      else Offset := 5;

      FThirdImages := SendMessage(Self.Handle, CBEM_GETIMAGELIST, 0, 0);
      if Images <> nil then
        begin
          if BidiMode <> bdRightToLeft then
            IconX := Offset
          else
           IconX := Buffer.Width - Offset - Images.Width;
          IconY := Buffer.Height div 2 - Images.Height div 2;
          if IconY < 0 then IconY := 0;
          if (ItemsEx[AIndex].ImageIndex >= 0) and
             (ItemsEx[AIndex].ImageIndex < Images.Count) then
            Images.Draw(Buffer.Canvas, IconX, IconY,
              ItemsEx[AIndex].ImageIndex, True);
          Offset := Offset + Images.Width + 5;
        end
      else
        if FThirdImages > 0 then
          begin
            ImageList_GetIconSize(FThirdImages, FThirdImageWidth, FThirdImageHeight);
            FThirdImagesCount := ImageList_GetImageCount(FThirdImages);
            IIndex := ItemsEx[AIndex].ImageIndex;
            if (IIndex >= 0) and (IIndex < FThirdImagesCount) then
            begin
              if BidiMode <> bdRightToLeft then
                IconX := Offset
              else
                IconX := Buffer.Width - Offset - FThirdImageWidth;
              IconY := Buffer.Height div 2 - FThirdImageHeight div 2;
              if IconY < 0 then IconY := 0;
              ImageList_DrawEx(FThirdImages, IIndex, Buffer.Canvas.Handle,
                IconX,  IconY,  FThirdImageWidth,
                FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
              Offset := Offset  + FThirdImageWidth + 5;
            end;
          end;

      if BidiMode <> bdRightToLeft then
        R := Rect(Offset, 0, Buffer.Width, Buffer.Height)
      else
        R := Rect(5, 0, Buffer.Width - Offset, Buffer.Height);

      Buffer.Canvas.Brush.Style := bsClear;
      LCaption := ItemsEx[AIndex].Caption;
      if LCaption <> '' then
         DrawText(Buffer.Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
           scDrawTextBidiModeFlags(DT_LEFT OR DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft));
    end;
    Canvas.Draw(ARect.Left, ARect.Top, Buffer);
  finally
    Buffer.Free;
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TscCustomComboBoxEx.ComboBoxWndProc(var Msg: TMessage);
var
  FHandled: Boolean;
  DC, PaintDC: HDC;
  PS: TPaintStruct;
begin
  FHandled := False;
  if not IsCustomStyle then
  case Msg.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
    if (FComboBoxHandle <> 0) and Visible and (Style = csExDropDownList) then
    begin
      SendMessage(FComboBoxHandle, WM_SETREDRAW, 0, 0);
    end;
   WM_MOUSELEAVE:
     FMouseIn := False;
   WM_MOUSEFIRST..WM_MOUSELAST:
     if not FMouseIn then
       FMouseIn := True;
   WM_DRAWITEM:
    if IsCustomDrawItem then
    begin
      with TWMDrawItem(Msg) do
      begin
        DrawListBoxItem(DrawItemStruct.hDC,
          DrawItemStruct.rcItem,
          DrawItemStruct.itemID,
          DrawItemStruct.itemState and ODS_SELECTED <> 0);
      end;
      FHandled := True;
    end;
     WM_ERASEBKGND:
     if IsCustomDraw then
       begin
         DrawComboBox(Msg.WParam);
         Msg.Result := 1;
         FHandled := True;
       end;
     WM_PAINT:
      if IsCustomDraw then
      begin
        DC := HDC(Msg.WParam);
        if DC <> 0 then
          PaintDC := DC
        else
          PaintDC := BeginPaint(FComboBoxHandle, PS);
        try
          DrawComboBox(PaintDC);
        finally
          if DC = 0 then
            EndPaint(FComboBoxHandle, PS);
        end;
        FHandled := True;
      end;
  end;

  if not FHandled then
    Msg.Result := CallWindowProc(FDefComboBoxProc, FComboBoxHandle,
      Msg.Msg, Msg.WParam, Msg.LParam);

  if not IsCustomStyle then
  case Msg.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
    if (FComboBoxHandle <> 0) and Visible and (Style = csExDropDownList) then
    begin
      SendMessage(FComboBoxHandle, WM_SETREDRAW, 1, 0);
      RedrawWindow(FComboBoxHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
    end;
  end;
end;

procedure TscCustomComboBoxEx.CreateWnd;
begin
  inherited;
  InitComboBoxWnd;
end;

procedure TscCustomComboBoxEx.InitComboBoxWnd;
var
  OldComboHandle: HWnd;
begin
  if IsWindowsXP then  Exit;

  if FComboBoxHandle = 0 then
  begin
    FComboBoxHandle := SendMessage(Handle, CBEM_GETCOMBOCONTROL, 0, 0);
    if FComboBoxHandle <> 0 then
    begin
      FDefComboBoxProc := Pointer(GetWindowLong(FComboBoxHandle, GWL_WNDPROC));
      SetWindowLong(FComboBoxHandle, GWL_WNDPROC, IntPtr(FComboBoxInstance));
    end;
  end
  else
  begin
    OldComboHandle := FComboBoxHandle;
    FComboBoxHandle := SendMessage(Handle, CBEM_GETCOMBOCONTROL, 0, 0);
    if OldComboHandle <> FComboBoxHandle then
    begin
      FDefComboBoxProc := Pointer(GetWindowLong(FComboBoxHandle, GWL_WNDPROC));
      SetWindowLong(FComboBoxHandle, GWL_WNDPROC, IntPtr(FComboBoxInstance));
    end;
  end;
end;

procedure TscCustomComboBoxEx.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomComboBoxEx.BeginUpdate;
var
  FComboHWND: HWND;
begin
  if not Visible then Exit;
  FComboHWND := SendMessage(Handle, CBEM_GETCOMBOCONTROL, 0, 0);
  if FComboHWND <> 0 then
    SendMessage(FComboHWND, WM_SETREDRAW, 0, 0);
end;

procedure TscCustomComboBoxEx.EndUpdate;
var
  FComboHWND: HWND;
begin
  if not Visible then Exit;
  FComboHWND := SendMessage(Handle, CBEM_GETCOMBOCONTROL, 0, 0);
  if FComboHWND <> 0 then
  begin
    SendMessage(FComboHWND, WM_SETREDRAW, 1, 0);
    RedrawWindow(FComboHWND, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  end;
end;

procedure TscCustomComboBoxEx.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

constructor TscComboBoxExStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  FPaintCopy := False;
  OverridePaint := False;
  OverridePaintNC := False;
  FDroppedDown := False;
  FTempItemIndex := -1;
  FComboBoxHandle := 0;
  FComboBoxInstance := MakeObjectInstance(ComboBoxWndProc);
  FDefComboBoxProc := nil;
end;

destructor TscComboBoxExStyleHook.Destroy;
begin
  if FComboBoxHandle <> 0 then
    SetWindowLong(FComboBoxHandle, GWL_WNDPROC, IntPtr(FDefComboBoxProc));
  FreeObjectInstance(FComboBoxInstance);
  inherited;
end;

procedure TscComboBoxExStyleHook.CMSEPaint(var Message: TMessage);
begin
  if FEditHandle <> 0 then
  begin
    FPaintCopy := True;
    DrawComboBox(Message.WParam);
    FPaintCopy := False;
  end;
  Handled := True;
end;

procedure TscComboBoxExStyleHook.CMSENCPaint(var Message: TMessage);
begin
  Message.Result := SE_RESULT;
  Handled := True;
end;

procedure TscComboBoxExStyleHook.WMParentNotify(var Message: TMessage);
begin
  if TStyleManager.SystemStyle.Enabled then inherited;
end;

procedure TscComboBoxExStyleHook.DrawPushButtonCombo(ACanvas: TCanvas);
var
  Buffer: TBitmap;
  R, R1, BR: TRect;
  FState: TscsCtrlState;
  BW: Integer;
  FC: TColor;
  IIndex: Integer;
  FItemIndex: Integer;
  FThirdImageWidth, FThirdImageHeight, FThirdImages, FThirdImagesCount: Integer;
  IconX, IconY: Integer;
  LCaption: String;
  FCanDrawFocusRect: Boolean;
  FScaleFactor: Double;
begin
  FScaleFactor := 1;
  if Control is TscCustomComboBoxEx then
    FScaleFactor :=  TscCustomComboBoxEx(Control).FScaleFactor;
  Buffer := TBitmap.Create;
  try
    Buffer.Width := Control.Width;
    Buffer.Height := Control.Height;
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    if (Control.Parent <> nil) and (Control.Parent is TscCustomControl) then
       TscCustomControl(Control.Parent).FGetControlBG := True;
    try
      DrawParentBackground(Control, Buffer.Canvas);
    finally
      if (Control.Parent <> nil) and (Control.Parent is TscCustomControl) then
        TscCustomControl(Control.Parent).FGetControlBG := False;
    end;
    FCanDrawFocusRect := (Control is TscCustomComboBoxEx) and
      (TscCustomComboBoxEx(Control).ShowFocusRect);
    if FDroppedDown then
      FState := scsPressed
    else
    if not FCanDrawFocusRect and Control.Focused then
       FState := scsFocused
    else
    if MouseInControl then
       FState := scsHot
    else
    if Control.Enabled then
      FState := scsNormal
    else
      FState := scsDisabled;
    DrawButton(Buffer.Canvas, R, FState, False, FScaleFactor);
    Buffer.Canvas.Font := TWinControlClass(Control).Font;
    FC := GetButtonTextColor(FState);
    Buffer.Canvas.Font.Color := FC;
    // draw item
    InflateRect(R, -2, -2);
    BW := GetSystemMetrics(SM_CXVSCROLL);
    if Control.BidiMode = bdRightToLeft then
    begin
      Inc(R.Left, BW);
      BR := Rect(2, R.Top, R.Left, R.Bottom);
    end
    else
      begin
        Dec(R.Right, BW);
        BR := Rect(R.Right, R.Top, Control.Width - 2, R.Bottom);
      end;
    R1 := R;
    Inc(R.Left, 2);
    Dec(R.Right, 2);
    Buffer.Canvas.Brush.Style := bsClear;
    if FDroppedDown then
      FItemIndex := FTempItemIndex
    else
      FItemIndex := TCustomComboBoxEx(Control).ItemIndex;
    // draw item
    if FItemIndex <> -1 then
    begin
       FThirdImages := SendMessage(Handle, CBEM_GETIMAGELIST, 0, 0);
        if (TscCustomComboBoxEx(Control).Images <> nil) and
           (FItemIndex <> -1) then
          with TscCustomComboBoxEx(Control) do
          begin
            IconX := 5;
            IconY := R.Top + R.Height div 2 - Images.Height div 2;
            if IconY < R.Top then IconY := R.Top;

            if (ItemsEx[FItemIndex].ImageIndex >= 0) and
               (ItemsEx[FItemIndex].ImageIndex < Images.Count) then
              Images.Draw(Buffer.Canvas, IconX, IconY,
                ItemsEx[FItemIndex].ImageIndex, Control.Enabled);

            R.Left := IconX + IMages.Width + 5;
          end
        else
        if (FThirdImages > 0) and (FItemIndex <> -1) then
          with TscCustomComboBoxEx(Control) do
          begin
            IIndex := ItemsEx[FItemIndex].ImageIndex;
            ImageList_GetIconSize(FThirdImages, FThirdImageWidth, FThirdImageHeight);
            FThirdImagesCount := ImageList_GetImageCount(FThirdImages);
            if (IIndex >= 0) and (IIndex < FThirdImagesCount) then
            begin
              IconX := 5;
              IconY := R.Top + R.Height div 2 - FThirdImageHeight div 2;
              if IconY < R.Top then IconY := R.Top;
              ImageList_DrawEx(FThirdImages, IIndex, Buffer.Canvas.Handle,
                IconX,  IconY,  FThirdImageWidth,
                 FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
              R.Left := IconX + FThirdImageWidth + 5;
            end;
          end
        else
          Inc(R.Left, 5);

        if (FItemIndex <> -1) then
        begin
          LCaption := TscCustomComboBoxEx(Control).ItemsEx[FItemIndex].Caption;
          if LCaption <> '' then
            DrawText(Buffer.Canvas.Handle, PWideChar(LCaption), Length(LCaption),
              R, DT_LEFT OR DT_VCENTER or DT_SINGLELINE);
        end;
    end;
    //
    if Control.Focused and (FState <> scsPressed) and FCanDrawFocusRect then
    begin
      InflateRect(R1, -1, -1);
      scDrawFocusRect(Buffer.Canvas, R1, FScaleFactor);
    end;
    if Control.Enabled then
      FState := scsNormal
    else
      FState := scsDisabled;
    if IsCustomStyle or not StyleServices.Enabled or IsWindowsXP then
      DrawComboArrowImage(Buffer.Canvas, BR, FC, FScaleFactor)
    else
      DrawDropDownButton(Buffer.Canvas, BR, FState, True, False, FScaleFactor);
    ACanvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TscComboBoxExStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{$IFNDEF VER230}
function TscComboBoxExStyleHook.IsChildHandle(AHandle: HWnd): Boolean;
var
  FComboEditHandle: HWnd;
begin
  Result := inherited IsChildHandle(AHandle);
  if not Result then
  begin
    FComboEditHandle := SendMessage(Handle, CBEM_GETEDITCONTROL, 0, 0);
    Result := (FComboEditHandle <> 0) and (AHandle = FComboEditHandle);
  end;
end;
{$ENDIF}

procedure TscComboBoxExStyleHook.DoHotTrackTimer;
var
  P: TPoint;
  FWindowHandle: HWnd;
begin
  GetCursorPos(P);
  FWindowHandle := WindowFromPoint(P);
  if (FWindowHandle <> FComboBoxHandle) {$IFNDEF VER230 }and not IsChildHandle(FWindowHandle) {$ENDIF} then
  begin
    StopHotTrackTimer;
    MouseInControl := False;
    MouseLeave;
  end;
end;

procedure TscComboBoxExStyleHook.MouseLeave;
begin
  MouseOnButton := False;
  PaintComboBoxWnd;
end;

procedure TscComboBoxExStyleHook.DrawListBoxItem(ADC: HDC; ARect: TRect; AIndex: Integer; ASelected: Boolean);
var
  Canvas: TCanvas;
  Offset: Integer;
  IconX, IconY, IIndex: Integer;
  Buffer: TBitMap;
  LCaption: String;
  R: TRect;
  FThirdImageWidth, FThirdImageHeight, FThirdImages, FThirdImagesCount: Integer;
begin
  if (AIndex < 0) or (AIndex >= TscCustomComboBoxEx(Control).ItemsEx.Count) then Exit;
  Canvas := TCanvas.Create;
  Canvas.Handle := ADC;
  Buffer := TBitmap.Create;
  Buffer.Width := ARect.Width;
  Buffer.Height := ARect.Height;
  try
    Buffer.Canvas.Font.Assign(TscCustomComboBoxEx(Control).Font);
    with Buffer.Canvas do
    begin
      Brush.Style := bsSolid;
      if ASelected and not ((Control is TscCustomComboBoxEx) and
          (TscCustomComboBoxEx(Control).SelectionStyle = scstStyled)) then
      begin
        if (Control is TscCustomComboBoxEx) and
           (TscCustomComboBoxEx(Control).SelectionColor <> clNone)
        then
        begin
          Brush.Color := TscCustomComboBoxEx(Control).SelectionColor;
          Font.Color := TscCustomComboBoxEx(Control).SelectionTextColor;
        end
        else
        begin
          Brush.Color := GetStyleColor(clHighLight);
          Font.Color := GetStyleColor(clHighLightText);
        end;
      end
      else
      begin
        {$IFNDEF VER230}
        if seClient in Control.StyleElements then
          Brush.Color := StyleServices.GetStyleColor(scComboBox)
        else
          Brush.Color := TWinControlClass(Control).Color;
        {$ELSE}
        Brush.Color := StyleServices.GetStyleColor(scComboBox);
        {$ENDIF}

        {$IFNDEF VER230}
        if seFont in Control.StyleElements then
        begin
          if ASelected then
            Font.Color := GetSelectionTextColor
          else
            Font.Color := StyleServices.GetStyleFontColor(sfComboBoxItemNormal);
        end
        else
          Font.Color := TWinControlClass(Control).Font.Color;
        {$ELSE}
        if ASelected then
          Font.Color := GetSelectionTextColor
        else
          Font.Color := StyleServices.GetStyleFontColor(sfComboBoxItemNormal);
        {$ENDIF}
      end;
      FillRect(Rect(0, 0, Buffer.Width, Buffer.Height));
      if ASelected and (Control is TscCustomComboBoxEx) and
         (TscCustomComboBoxEx(Control).SelectionStyle = scstStyled) then
        DrawSelection(Buffer.Canvas, Rect(0, 0, Buffer.Width, Buffer.Height), False, True);
      Offset := TComboExItem(TscCustomComboBoxEx(Control).ItemsEx[AIndex]).Indent;
      if Offset > 0 then Offset := (Offset * 10) + 5
      else Offset := 5;

      FThirdImages := SendMessage(Self.Handle, CBEM_GETIMAGELIST, 0, 0);
      if (TscCustomComboBoxEx(Control).Images <> nil) then
        with TscCustomComboBoxEx(Control) do
        begin
          IconX := Offset;
          IconY := Buffer.Height div 2 - Images.Height div 2;
          if IconY < 0 then IconY := 0;
          if (ItemsEx[AIndex].ImageIndex >= 0) and
             (ItemsEx[AIndex].ImageIndex < Images.Count) then
            Images.Draw(Buffer.Canvas, IconX, IconY,
              ItemsEx[AIndex].ImageIndex, True);
          Offset := Offset + Images.Width + 5;
        end
      else
        if FThirdImages > 0 then
          with TscCustomComboBoxEx(Control) do
          begin
            ImageList_GetIconSize(FThirdImages, FThirdImageWidth, FThirdImageHeight);
            FThirdImagesCount := ImageList_GetImageCount(FThirdImages);
            IIndex := ItemsEx[AIndex].ImageIndex;
            if (IIndex >= 0) and (IIndex < FThirdImagesCount) then
            begin
              IconX := Offset;
              IconY := Buffer.Height div 2 - FThirdImageHeight div 2;
              if IconY < 0 then IconY := 0;
              ImageList_DrawEx(FThirdImages, IIndex, Buffer.Canvas.Handle,
                IconX,  IconY,  FThirdImageWidth,
                FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
              Offset := Offset  + FThirdImageWidth + 5;
            end;
          end;
      R := Rect(Offset, 0, Buffer.Width, Buffer.Height);
      Buffer.Canvas.Brush.Style := bsClear;
      LCaption := TscCustomComboBoxEx(Control).ItemsEx[AIndex].Caption;
      if LCaption <> '' then
         DrawText(Buffer.Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
           DT_LEFT OR DT_VCENTER or DT_SINGLELINE);
    end;
    Canvas.Draw(ARect.Left, ARect.Top, Buffer);
  finally
    Buffer.Free;
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TscComboBoxExStyleHook.ComboBoxWndProc(var Msg: TMessage);
var
  FCallOldProc: Boolean;
  PS: TPaintStruct;
  DC, PaintDC: HDC;
  P: TPoint;
  FOldMouseOnButton: Boolean;
  R: TRect;
begin
  FCallOldProc := True;
  case Msg.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
    if (FComboBoxHandle <> 0) and Control.Visible then
    begin
      SendMessage(FComboBoxHandle, WM_SETREDRAW, 0, 0);
    end;
    WM_CTLCOLORLISTBOX:
     if (ListHandle = 0) and (Msg.LParam <> 0) and (ListBoxInstance = nil) then
       HookListBox(Msg.LParam);
     WM_ERASEBKGND:
       begin
         DrawComboBox(Msg.WParam);
         FCallOldProc := False;
       end;
    WM_DRAWITEM:
    begin
      with TWMDrawItem(Msg) do
      begin
        DrawListBoxItem(DrawItemStruct.hDC,
        DrawItemStruct.rcItem,
        DrawItemStruct.itemID,
        DrawItemStruct.itemState and ODS_SELECTED <> 0);
      end;
      Msg.Result := 1;
      FCallOldProc := False;
    end;
    WM_PAINT:
      begin
        DC := HDC(Msg.WParam);
        if DC <> 0 then
          PaintDC := DC
        else
          PaintDC := BeginPaint(FComboBoxHandle, PS);
        try
          DrawComboBox(PaintDC);
        finally
          if DC = 0 then
            EndPaint(FComboBoxHandle, PS);
        end;
        FCallOldProc := False;
      end;
    WM_MOUSEMOVE:
      begin
        if not MouseInControl then
        begin
          MouseInControl := True;
          StartHotTrackTimer;
          MouseEnter;
        end;
        P := Point(TWMMouse(Msg).XPos, TWMMouse(Msg).YPos);
        FOldMouseOnButton := MouseOnButton;
        R := ButtonRect;
        if R.Contains(P) then
          MouseOnButton := True
        else
          MouseOnButton := False;
        if FOldMouseOnButton <> MouseOnButton then
          InvalidateRect(FComboBoxHandle, @R, False);
     end;
    WM_NCPAINT:
    if IsWindowsXP then
    begin
      FCallOldProc := False;
      Msg.Result := 1;
    end;
    WM_NCCALCSIZE:
    if IsWindowsXP then
    begin
      FCallOldProc := False;
      Msg.Result := WVR_VALIDRECTS;
    end;
  end;

  if FCallOldProc then
    with Msg do
      Result := CallWindowProc(FDefComboBoxProc, FComboBoxHandle, Msg, WParam, LParam);

  case Msg.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
    if (FComboBoxHandle <> 0) and Control.Visible then
    begin
      SendMessage(FComboBoxHandle, WM_SETREDRAW, 1, 0);
      RedrawWindow(FComboBoxHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
    end;
  end;
end;

procedure TscComboBoxExStyleHook.PaintComboBoxWnd;
begin
  if not IsWindowsXP then
    RedrawWindow(FComboBoxHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW)
  else
    RedrawWindow(FComboBoxHandle, nil, 0, RDW_ALLCHILDREN or RDW_INVALIDATE);
end;

procedure TscComboBoxExStyleHook.WMNCPaint(var Message: TMessage);
begin
  if FComboBoxHandle = 0 then
  begin
    InitComboBoxWnd;
    if IsWindowsXP and (FComboBoxHandle <> 0) then
    begin
      SetWindowPos(FComboBoxHandle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    end;
  end;
end;

procedure TscComboBoxExStyleHook.WMCommand(var Message: TWMCommand);
begin
  CallDefaultProc(TMessage(Message));
  case Message.NotifyCode of
    CBN_SELCHANGE:
    begin
      if FDroppedDown then
         FTempItemIndex := TCustomComboBoxEx(Control).ItemIndex;
      PaintComboBoxWnd;
    end;
    CBN_SELENDOK,
    CBN_SETFOCUS, CBN_KILLFOCUS:
      PaintComboBoxWnd;
    CBN_DROPDOWN:
      begin
        FTempItemIndex := TCustomComboBoxEx(Control).ItemIndex;
        FDroppedDown := True;
        PaintComboBoxWnd;
      end;
     CBN_CLOSEUP:
      begin
        FDroppedDown := False;
        PaintComboBoxWnd;
      end;
  end;
  Handled := True;
end;

procedure TscComboBoxExStyleHook.DrawComboBox(DC: HDC);
var
  Canvas: TCanvas;
  DrawState: TThemedComboBox;
  Details: TThemedElementDetails;
  R: TRect;
  BtnDrawState: TThemedComboBox;
  IconX, IconY, IIndex: Integer;
  LCaption: string;
  Buffer: TBitmap;
  FThirdImageWidth, FThirdImageHeight, FThirdImages, FThirdImagesCount: Integer;
  FItemIndex: Integer;
begin
  if not StyleServices.Available or (Control.Width * Control.Height = 0) then
    Exit;

  if (Control is TscCustomComboBoxEx) and (not TscCustomComboBoxEx(Control).DrawSelectionWithStyles) and
     (TscCustomComboBoxEx(Control).Style = csExDropDownList) and TStyleManager.SystemStyle.Enabled and not
     IsWindowsXP
  then
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      DrawPushButtonCombo(Canvas);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
    Handled := True;
    Exit;
  end;

  if FDroppedDown then
    FItemIndex := FTempItemIndex
  else
    FItemIndex := TCustomComboBoxEx(Control).ItemIndex;

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    Buffer := TBitMap.Create;
    try
      Buffer.Width := Control.Width;
      Buffer.Height := Control.Height;
      if not Control.Enabled then
        DrawState := tcBorderDisabled
      else
      if Control.Focused then
        DrawState := tcBorderFocused
      else if MouseInControl then
        DrawState := tcBorderHot
      else
        DrawState := tcBorderNormal;

      R := Rect(0, 0, Control.Width, Control.Height);
      Details := StyleServices.GetElementDetails(DrawState);
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);
      {$IFNDEF VER230}
      if not (seClient in Control.StyleElements) then
      begin
        R := Control.ClientRect;
        InflateRect(R, -3, -3);
        R.Right := ButtonRect.Left - 1;
        with Buffer.Canvas do
        begin
          Brush.Color := TWinControlClass(Control).Color;
          FillRect(R);
        end;
      end;
      {$ENDIF}
      if not Control.Enabled then
        BtnDrawState := tcDropDownButtonDisabled
      else if FDroppedDown then
        BtnDrawState := tcDropDownButtonPressed
      else if MouseOnButton then
        BtnDrawState := tcDropDownButtonHot
      else
        BtnDrawState := tcDropDownButtonNormal;

      if TCustomComboBoxEx(Control).Style <> csExSimple then
      begin
        Details := StyleServices.GetElementDetails(BtnDrawState);
        StyleServices.DrawElement(Buffer.Canvas.Handle, Details, ButtonRect);
      end;

      R := Control.ClientRect;
      InflateRect(R, -3, -3);
      R.Right := ButtonRect.Left - 1;
      Buffer.Canvas.Font.Assign(TscCustomComboBoxEx(Control).Font);
      {$IFNDEF VER230}
      if seFont in Control.StyleElements then
      {$ENDIF}
        if Control.Enabled then
          Buffer.Canvas.Font.Color := StyleServices.GetStyleFontColor(sfComboBoxItemNormal)
        else
          Buffer.Canvas.Font.Color := StyleServices.GetStyleFontColor(sfComboBoxItemDisabled);
      if TscCustomComboBoxEx(Control).Style = csExDropDownList then
      begin
         if TscCustomComboBoxEx(Control).Focused then
           with Buffer.Canvas do
           begin
             if FItemIndex <> -1 then
             begin
               if (Control is TscCustomComboBoxEx) and
                  (TscCustomComboBoxEx(Control).SelectionStyle = scstStyled) then
               begin
                 DrawSelection(Buffer.Canvas, R, False, True);
                 Font.Color := GetSelectionTextColor;
               end
               else
               begin
                 if (Control is TscCustomComboBoxEx) and
                    (TscCustomComboBoxEx(Control).SelectionColor <> clNone)
                 then
                 begin
                   Brush.Color := TscCustomComboBoxEx(Control).SelectionColor;
                   Font.Color := TscCustomComboBoxEx(Control).SelectionTextColor;
                 end
                 else
                 begin
                   Brush.Color := GetStyleColor(clHighLight);
                   Font.Color := GetStyleColor(clHighLightText);
                 end;
                 Brush.Style := bsSolid;
                 FillRect(R);
               end;
             end;
             if FItemIndex = -1 then
               scDrawUtils.scDrawFocusRect(Buffer.Canvas, R);
           end
         else
           with Buffer.Canvas do
           begin
             Brush.Color := Self.Brush.Color;
             Brush.Style := bsSolid;
             FillRect(R);
           end;
      end;

      if TscCustomComboBoxEx(Control).Style <> csExSimple then
      begin
        FThirdImages := SendMessage(Handle, CBEM_GETIMAGELIST, 0, 0);
        if (TscCustomComboBoxEx(Control).Images <> nil) and
           (FItemIndex <> -1) then
          with TscCustomComboBoxEx(Control) do
          begin
            IconX := 5;
            IconY := R.Top + R.Height div 2 - Images.Height div 2;
            if IconY < R.Top then IconY := R.Top;

            if (ItemsEx[FItemIndex].ImageIndex >= 0) and
               (ItemsEx[FItemIndex].ImageIndex < Images.Count) then
              Images.Draw(Buffer.Canvas, IconX, IconY,
                ItemsEx[FItemIndex].ImageIndex, Control.Enabled);

            R.Left := IconX + IMages.Width + 5;
          end
        else
        if (FThirdImages > 0) and (FItemIndex <> -1) then
          with TscCustomComboBoxEx(Control) do
          begin
            IIndex := ItemsEx[FItemIndex].ImageIndex;
            ImageList_GetIconSize(FThirdImages, FThirdImageWidth, FThirdImageHeight);
            FThirdImagesCount := ImageList_GetImageCount(FThirdImages);
            if (IIndex >= 0) and (IIndex < FThirdImagesCount) then
            begin
              IconX := 5;
              IconY := R.Top + R.Height div 2 - FThirdImageHeight div 2;
              if IconY < R.Top then IconY := R.Top;
              ImageList_DrawEx(FThirdImages, IIndex, Buffer.Canvas.Handle,
                IconX,  IconY,  FThirdImageWidth,
                 FThirdImageHeight, CLR_NONE, CLR_NONE, ILD_NORMAL);
              R.Left := IconX + FThirdImageWidth + 5;
            end;
          end
        else
          Inc(R.Left, 5);
        if (FItemIndex <> -1) then
        begin
          Buffer.Canvas.Brush.Style := bsClear;
          LCaption := TscCustomComboBoxEx(Control).ItemsEx[FItemIndex].Caption;
          if (FEditHandle <> 0) and FPaintCopy then
          begin
            if (TscCustomComboBoxEx(Control).Images <> nil) then
            begin
              Inc(R.Top);
              Dec(R.Left, 2);
            end
            else
            if (FThirdImages > 0) and (FThirdImageWidth > 0) then
            begin
              Inc(R.Top);
              Dec(R.Left, 2);
            end
            else
            begin
              Dec(R.Left, 4);
              Inc(R.Top, 2);
            end;
          end;
          if LCaption <> '' then
            DrawText(Buffer.Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
              DT_LEFT OR DT_VCENTER or DT_SINGLELINE);
        end
        else
        if (FEditHandle <> 0) and (Text <> '') and (FPaintCopy) then
        begin
          Buffer.Canvas.Brush.Style := bsClear;
          LCaption := Text;
          if (FEditHandle <> 0) and
             not ((TscCustomComboBoxEx(Control).Images <> nil) or (FThirdImages > 0)) then
          begin
            Dec(R.Left, 4);
            Inc(R.Top, 2);
          end
          else
          begin
            if (TscCustomComboBoxEx(Control).Images <> nil) then
            begin
              Inc(R.Top);
              Inc(R.Left, TscCustomComboBoxEx(Control).Images.Width);
            end
            else
            if (FThirdImages > 0) and (FThirdImageWidth > 0) then
            begin
              Inc(R.Top);
              Inc(R.Left, FThirdImageWidth);
            end;
          end;
         DrawText(Buffer.Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
            DT_LEFT OR DT_VCENTER or DT_SINGLELINE);
        end;
      end;

      Canvas.Draw(0, 0, Buffer);
    finally
      Buffer.Free;
    end;
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;
  Handled := True;
end;

procedure TscComboBoxExStyleHook.InitComboBoxWnd;
begin
  if FComboBoxHandle = 0 then
  begin
    FComboBoxHandle := SendMessage(Handle, CBEM_GETCOMBOCONTROL, 0, 0);
    if FComboBoxHandle <> 0 then
    begin
      FDefComboBoxProc := Pointer(GetWindowLong(FComboBoxHandle, GWL_WNDPROC));
      SetWindowLong(FComboBoxHandle, GWL_WNDPROC, IntPtr(FComboBoxInstance));
    end;
  end;
end;

type
  TscGroupButton = class(TscRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(RadioGroup: TscCustomRadioGroup);
    destructor Destroy; override;
  end;

constructor TscGroupButton.InternalCreate(RadioGroup: TscCustomRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
  CanFocused := True;
  FMargin := 0;
end;

destructor TscGroupButton .Destroy;
begin
  TscCustomRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TscGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TscCustomRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TscGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TscCustomRadioGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TscCustomRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TscGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TscCustomRadioGroup(Parent).KeyDown(Key, Shift);
end;

constructor TscCustomRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonsGlowEffect := TscButtonGlowEffect.Create;
  FButtonsGlowEffect.OnChange := OnButtonsGlowEffectChange;
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
  FShowFocusRect := True;
end;

procedure TscCustomRadioGroup.OnButtonsGlowEffectChange(Sender: TObject);
var
  I: Integer;
begin
  if FButtons.Count > 0
  then
    for I := 0 to FButtons.Count - 1 do
      with TscGroupButton(FButtons[I]) do
      begin
        GlowEffect.Assign(FButtonsGlowEffect);
        RepaintControl;
      end;
end;

procedure TscCustomRadioGroup.SetButtonsAnimation(Value: Boolean);
var
  I: Integer;
begin
  if FButtonsAnimation <> Value then
  begin
    FButtonsAnimation := Value;
    if FButtons.Count > 0 then
     for I := 0 to FButtons.Count - 1 do
       with TscGroupButton (FButtons[I]) do
         Animation := FButtonsAnimation;
  end;
end;

procedure TscCustomRadioGroup.SetButtonsImages(Value: TCustomImageList);
var
  I: Integer;
begin
  FButtonsImages := Value;
  if FButtons.Count > 0
  then
    for I := 0 to FButtons.Count - 1 do
      with TscGroupButton (FButtons[I]) do
        Images := ButtonsImages;
end;

procedure TscCustomRadioGroup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FButtonsImages then FButtonsImages := nil;
  end;
end;

destructor TscCustomRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  FButtonsGlowEffect.Free;
  inherited Destroy;
end;

procedure TscCustomRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DeferHandle: THandle;
  ALeft: Integer;
  ButtonsRect: TRect;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    ButtonsRect := Rect(0, 0, Width, Height);
    AdjustClientRect(ButtonsRect);
    InflateRect(ButtonsRect, -5, -5);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := ButtonsRect.Width div FColumns - 2;
    I := ButtonsRect.Height;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := ButtonsRect.Top;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TscGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + ButtonsRect.Left + 1;
          if UseRightToLeftAlignment then
            ALeft := ButtonsRect.Width - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TscCustomRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    if not (csLoading in ComponentState) then
      if Assigned(FOnButtonClick) then FOnButtonClick(Self);
  end;
end;

procedure TscCustomRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TscCustomRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TscCustomRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TscCustomRadioGroup.SetButtonCount(Value: Integer);
var
  i: Integer;
begin
  while FButtons.Count < Value do TscGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do TscGroupButton(FButtons.Last).Free;
  if FButtons.Count > 0
  then
   for I := 0 to FButtons.Count - 1 do
     with TscGroupButton(FButtons[I]) do
     begin
       FImageIndex := I;
       FSpacing := 5;
       FAnimation := FButtonsAnimation;
       FShowFocusRect := Self.ShowFocusRect;
       GlowEffect.Assign(FButtonsGlowEffect);
     end;
end;

procedure TscCustomRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TscCustomRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then FItemIndex := Value else
  begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        TscGroupButton (FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        TscGroupButton (FButtons[FItemIndex]).Checked := True;
    end;
  end;
end;

procedure TscCustomRadioGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TscCustomRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    TscGroupButton (FButtons[I]).Caption := FItems[I];
  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TscGroupButton (FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
  RePaintControl;
  if not (csLoading in ComponentState) then
    ArrangeButtons;
end;

procedure TscCustomRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TscGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TscCustomRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TscCustomRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TscCustomRadioGroup.CanModify: Boolean;
begin
  Result := True;
end;

procedure TscCustomRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TscCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
end;

// check group
type

  TscCheckGroupButton = class(TscCheckBox)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(CheckGroup: TscCustomCheckGroup);
    destructor Destroy; override;
  end;

constructor TscCheckGroupButton.InternalCreate(CheckGroup: TscCustomCheckGroup);
begin
  inherited Create(CheckGroup);
  CheckGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := CheckGroup.Enabled;
  ParentShowHint := False;
  OnClick := CheckGroup.ButtonClick;
  Parent := CheckGroup;
  CanFocused := True;
  FMargin := 0;
end;

destructor TscCheckGroupButton.Destroy;
begin
  TscCustomRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TscCheckGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TscCustomRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TscCheckGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TscCustomRadioGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TscCustomRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TscCheckGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TscCustomRadioGroup(Parent).KeyDown(Key, Shift);
end;

constructor TscCustomCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonsGlowEffect := TscButtonGlowEffect.Create;
  FButtonsGlowEffect.OnChange := OnButtonsGlowEffectChange;
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FColumns := 1;
  FItemIndex := -1;
  FShowFocusRect := True;
end;

procedure TscCustomCheckGroup.OnButtonsGlowEffectChange(Sender: TObject);
var
  I: Integer;
begin
  if FButtons.Count > 0
  then
    for I := 0 to FButtons.Count - 1 do
      with TscCheckGroupButton(FButtons[I]) do
      begin
        GlowEffect.Assign(FButtonsGlowEffect);
        RepaintControl;
      end;
end;

procedure TscCustomCheckGroup.SetButtonsAnimation(Value: Boolean);
var
  I: Integer;
begin
  if FButtonsAnimation <> Value then
  begin
    FButtonsAnimation := Value;
    if FButtons.Count > 0 then
     for I := 0 to FButtons.Count - 1 do
       with TscCheckGroupButton (FButtons[I]) do
         Animation := FButtonsAnimation;
  end;
end;

procedure TscCustomCheckGroup.SetShowFocusRect(Value: Boolean);
var
  I: Integer;
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    if FButtons.Count > 0 then
     for I := 0 to FButtons.Count - 1 do
       with TscCheckGroupButton (FButtons[I]) do
         ShowFocusRect := Self.FShowFocusRect;
  end;
end;

procedure TscCustomCheckGroup.SetButtonsImages(Value: TCustomImageList);
var
  I: Integer;
begin
  FButtonsImages := Value;
  if FButtons.Count > 0
  then
    for I := 0 to FButtons.Count - 1 do
      with TscCheckGroupButton (FButtons[I]) do
        Images := ButtonsImages;
end;

procedure TscCustomCheckGroup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FButtonsImages then FButtonsImages := nil;
  end;
end;

destructor TscCustomCheckGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  FButtonsGlowEffect.Free;
  inherited Destroy;
end;

procedure TscCustomCheckGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DeferHandle: THandle;
  ALeft: Integer;
  ButtonsRect: TRect;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    ButtonsRect := Rect(0, 0, Width, Height);
    AdjustClientRect(ButtonsRect);
    InflateRect(ButtonsRect, -5, -5);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := ButtonsRect.Width div FColumns - 2;
    I := ButtonsRect.Height;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := ButtonsRect.Top;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TscCheckGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + ButtonsRect.Left + 1;
          if UseRightToLeftAlignment then
            ALeft := ButtonsRect.Width - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TscCustomCheckGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    if Assigned(FOnButtonClick) then FOnButtonClick(Self);
  end;
end;

procedure TscCustomCheckGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    UpdateButtons;
  end;
end;

procedure TscCustomCheckGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TscCustomCheckGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TscCustomCheckGroup.SetButtonCount(Value: Integer);
var
  i: Integer;
begin
  while FButtons.Count < Value do TscCheckGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do TscCheckGroupButton(FButtons.Last).Free;
  if FButtons.Count > 0
  then
   for I := 0 to FButtons.Count - 1 do
     with TscCheckGroupButton(FButtons[I]) do
     begin
       FImageIndex := I;
       FSpacing := 5;
       FShowFocusRect := Self.ShowFocusRect;
       FAnimation := FButtonsAnimation;
       GlowEffect.Assign(FButtonsGlowEffect);
     end;
end;

procedure TscCustomCheckGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TscCustomCheckGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TscCustomCheckGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    TscCheckGroupButton (FButtons[I]).Caption := FItems[I];
  RePaintControl;
  if not (csLoading in ComponentState) then
    ArrangeButtons;
end;

procedure TscCustomCheckGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TscCheckGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TscCustomCheckGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TscCustomCheckGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TscCustomCheckGroup.CanModify: Boolean;
begin
  Result := True;
end;

function TscCustomCheckGroup.GetCheckedStatus(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < FButtons.Count)
  then
    Result := TscCheckBox(FButtons[Index]).Checked
  else
    Result := False;
end;

procedure TscCustomCheckGroup.SetCheckedStatus(Index: Integer; Value: Boolean);
begin
  if (Index >= 0) and (Index < FButtons.Count)
  then
    TscCheckBox(FButtons[Index]).Checked := Value;
end;

procedure TscCustomCheckGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TscCustomCheckGroup.FlipChildren(AllLevels: Boolean);
begin
end;

constructor TscPasswordEdit.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse] - [csSetCaption];
  FFrameColor := clBtnShadow;
  FShowingText := False;
  FPromptText := '';
  FHidePromptTextIfFocused := False;
  FPromptTextColor := clNone;
  FUseFontColorToStyleColor := False;
  FFrameActiveColor := clHighLight;
  FPasswordKind := pkPasswordChar;
  TransparentBackground := False;
  ParentColor := False;
  Color := clWindow;
  Text := '';
  FMouseIn := False;
  Width := 121;
  Height := 21;
  TabStop := True;
  FTextAlignment := taLeftJustify;
  FAutoSelect := True;
  FCharCase := ecNormal;
  FHideSelection := True;
  FMaxLength := 0;
  FReadOnly := False;
  FLMouseSelecting := False;
  FCaretPosition := 0;
  FSelStart := 0;
  FSelLength := 0;
  FFVChar := 1;
  FTransparent := False;
  Cursor := Cursor;
  FPasswordCharImageIndex := -1;
  FPasswordCharSelectedImageIndex := -1;
  FWallpapers := nil;
  FWallpaperIndex := -1;
  FCustomImages := nil;
  FCustomBackgroundImageNormalIndex := -1;
  FCustomBackgroundImageHotIndex := -1;
  FCustomBackgroundImageDisabledIndex := -1;
  FContentMarginLeft := 0;
  FContentMarginRight := 0;
  FContentMarginTop := 0;
  FContentMarginBottom := 0;
end;

destructor TscPasswordEdit.Destroy;
begin
  inherited;
end;

procedure TscPasswordEdit.SetPromptTextColor(Value: TColor);
begin
  if FPromptTextColor <> Value then
  begin
    FPromptTextColor := Value;
    RePaintControl;
  end;
end;

procedure TscPasswordEdit.SetPromptText(Value: String);
begin
  if FPromptText <> Value then
  begin
    FPromptText := Value;
    RePaintControl;
  end;
end;

procedure TscPasswordEdit.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RedrawWindow(Handle, nil, 0, RDW_FRAME);
  end;
end;

procedure TscPasswordEdit.SetFrameActiveColor(Value: TColor);
begin
  if FFrameActiveColor <> Value then
  begin
    FFrameActiveColor := Value;
    RedrawWindow(Handle, nil, 0, RDW_FRAME);
  end;
end;

procedure TscPasswordEdit.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetContentMarginLeft(Value: Integer);
begin
  if (FContentMarginLeft >= 0) and (FContentMarginLeft <> Value) then
  begin
    FContentMarginLeft := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetContentMarginTop(Value: Integer);
begin
  if (FContentMarginTop >= 0) and (FContentMarginTop <> Value) then
  begin
    FContentMarginTop := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetContentMarginRight(Value: Integer);
begin
  if (FContentMarginRight >= 0) and (FContentMarginRight <> Value) then
  begin
    FContentMarginRight := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetContentMarginBottom(Value: Integer);
begin
  if (FContentMarginBottom >= 0) and (FContentMarginBottom <> Value) then
  begin
    FContentMarginBottom := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetCustomBackgroundImageNormalIndex(Value: Integer);
begin
  if FCustomBackgroundImageNormalIndex <> Value then
  begin
    FCustomBackgroundImageNormalIndex := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetCustomBackgroundImageHotIndex(Value: Integer);
begin
  if FCustomBackgroundImageHotIndex <> Value then
  begin
    FCustomBackgroundImageHotIndex := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetCustomBackgroundImageDisabledIndex(Value: Integer);
begin
  if FCustomBackgroundImageDisabledIndex <> Value then
  begin
    FCustomBackgroundImageDisabledIndex := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    Invalidate;
  end;
end;

procedure TscPasswordEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPasswordCharImages) then
    FPasswordCharImages := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscPasswordEdit.SetPasswordCharImages(Value: TCustomImageList);
begin
  if FPasswordCharImages <> Value then
  begin
    FPasswordCharImages := Value;
    RePaintControl;
  end;
end;

procedure TscPasswordEdit.SetPasswordCharImageIndex(Value: Integer);
begin
  if FPasswordCharImageIndex <> Value then
  begin
    FPasswordCharImageIndex := Value;
    if FPasswordCharImages <> nil then
      RePaintControl;
  end;
end;

procedure TscPasswordEdit.SetPasswordCharSelectedImageIndex(Value: Integer);
begin
  if FPasswordCharSelectedImageIndex <> Value then
    FPasswordCharSelectedImageIndex := Value;
end;

procedure TscPasswordEdit.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    FTransparentBackground := FTransparent;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscPasswordEdit.SetBorderKind(Value: TscCustomEditBorderKind);
begin
  if FBorderKind <> Value then
  begin
    FBorderKind := Value;
    RePaintControl;
  end;
end;

function TscPasswordEdit.GetPaintText;
begin
  Result := Text;
end;

procedure TscPasswordEdit.PasteFromClipboard;
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

procedure TscPasswordEdit.ShowPasswordText(AShow: Boolean);
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

procedure TscPasswordEdit.DrawPasswordText(ACanvas: TCanvas);
var
  R: TRect;
  C: TColor;
begin
  R := GetEditRect;
  InflateRect(R, -1, -1);
  Dec(R.Bottom);
  ACanvas.Font := Self.Font;
  if (seFont in StyleElements) and IsCustomStyle then
  begin
    if FUseFontColorToStyleColor and Enabled then
      C := ColorToRGB(GetStyleColor(Self.Font.Color))
    else
    if FTransparent then
      C := scDrawUtils.GetCheckBoxTextColor(scsNormal)
    else
      C := scDrawUtils.GetEditTextColor(scsNormal);
  end
  else
    C := Font.Color;
  ACanvas.Font.Color := C;
  ACanvas.Brush.Style := bsClear;
  scDrawUtils.DrawTextAlignmentNoPrefix(ACanvas, Text, R,
    taLeftJustify, IsRightToLeft);
end;

procedure TscPasswordEdit.DrawPromptText(ACanvas: TCanvas);
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
    taLeftJustify, IsRightToLeft);
end;

procedure TscPasswordEdit.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, ClientR: TRect;
  SaveIndex, IIndex: Integer;
  C: TColor;
begin
  R := Rect(0, 0, Width, Height);
  ClientR := R;
  if not FTransparent
  then
  begin
    if (seClient in StyleElements) and IsCustomStyle then
       ACanvas.Brush.Color := GetEditBrushColor(scsNormal)
    else
      ACanvas.Brush.Color := Color;
    ACanvas.FillRect(ClientR);
  end;

  if (FCustomImages <> nil) then
  begin
    if not Self.Enabled then
      IIndex := FCustomBackgroundImageDisabledIndex
    else
      if FMouseIn or Focused then
        IIndex := FCustomBackgroundImageHotIndex
       else
        IIndex := FCustomBackgroundImageNormalIndex;
    if FCustomImages.IsIndexAvailable(IIndex)
    then
      FCustomImages.Draw(ACanvas, R, IIndex, FScaleFactor);
  end;
  if (FWallpapers <> nil) and FWallPapers.IsIndexAvailable(FWallpaperIndex)
  then
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);

  if BorderKind = sccebFrame then
  begin
    InflateRect(ClientR, -2, -2);
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      ExcludeClipRect(ACanvas.Handle, ClientR.Left, ClientR.Top,
        ClientR.Right, ClientR.Bottom);
      if Focused then
        DrawEditBorder(ACanvas, R, scsFocused)
      else
      if FMouseIn then
        DrawEditBorder(ACanvas, R, scsHot)
      else
        DrawEditBorder(ACanvas, R, scsNormal);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end;

  if (BorderKind = sccebColorFrame) or (BorderKind = sccebColorFrame2)  then
  begin
    if FMouseIn or Focused then
      C := GetStyleColor(FFrameActiveColor)
    else
      C := GetStyleColor(FFrameColor);
    R := Rect(0, 0, Width, Height);
    Frm3D(ACanvas, R, C, C);
    if BorderKind = sccebColorFrame2 then
      Frm3D(ACanvas, R, C, C);
  end;

  if (BorderKind = sccebBottomLine) or (BorderKind = sccebBottomActiveLine) then
  begin
    if (FMouseIn or Focused) and (BorderKind = sccebBottomActiveLine) then
      C := GetStyleColor(FFrameActiveColor)
    else
    if (seBorder in StyleElements) and IsCustomStyle then
    begin
      C := MiddleColor(GetStyleColor(clBtnFace), GetCheckBoxTextColor(scsNormal));
      C := MiddleColor(GetStyleColor(clBtnFace), C);
    end
    else
      C := FFrameColor;
    ACanvas.Pen.Color := C;
    ACanvas.MoveTo(0, R.Bottom - 1);
    ACanvas.LineTo(R.Right, R.Bottom - 1);
  end;

  if (FPromptText <> '') and (Text = '') then
    DrawPromptText(ACanvas);

  if FShowingText then
    DrawPasswordText(ACanvas)
  else
  begin
    PaintText(ACanvas);
    if Focused or not HideSelection
    then
      with ACanvas do
      begin
        Brush.Color := clHighLight;
        FillRect(GetSelRect);
      end;
    if Focused or not HideSelection then
      PaintSelectedText(ACanvas);
  end;

end;

procedure TscPasswordEdit.Loaded;
begin
  inherited;
end;

procedure TscPasswordEdit.WMSIZE(var Msg: TMessage);
begin
  inherited;
  UpdateCarete;
end;

procedure TscPasswordEdit.WMSETFOCUS(var Message: TWMSETFOCUS);
begin
  inherited;
  UpdateCarete;
  CaretPosition := 0;
  if AutoSelect and not FLMouseSelecting then
    SelectAll;
end;

procedure TscPasswordEdit.WMKILLFOCUS(var Message: TWMKILLFOCUS);
begin
  inherited;
  DestroyCaret;
  RePaintControl;
end;

function TscPasswordEdit.GetCharX(a: Integer): Integer;
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

function TscPasswordEdit.GetCPos(x: Integer): Integer;
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

function TscPasswordEdit.GetEditRect: TRect;
begin
  Result := Rect(2, 2, Width - 2, Height - 2);
  Inc(Result.Left, FContentMarginLeft);
  Inc(Result.Top, FContentMarginTop);
  Dec(Result.Right, FContentMarginRight);
  Dec(Result.Bottom, FContentMarginBottom);
end;

function TscPasswordEdit.GetAlignmentFlags: Integer;
begin
  case FTextAlignment of
    taCenter: Result := DT_CENTER;
    taRightJustify: Result := DT_RIGHT;
  else
    Result := DT_LEFT;
  end;
end;

procedure TscPasswordEdit.KeyDown(var Key: word; Shift: TShiftState);
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

procedure TscPasswordEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Ord(Key) >= 32) and not ReadOnly then InsertChar(Key);
end;

procedure TscPasswordEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
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
end;

procedure TscPasswordEdit.UpdateFVC;
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

procedure TscPasswordEdit.MouseMove(Shift: TShiftState; x, y: Integer);
var
  OldCaretPosition: Integer;
  TmpNewPosition : Integer;
begin
  inherited;
  if FLMouseSelecting
  then
    begin
      TmpNewPosition := GetCPos(x);
      OldCaretPosition := CaretPosition;
      if (x > GetEditRect.Right)
      then
        CaretPosition := TmpNewPosition +1
      else
        CaretPosition := TmpNewPosition;
      if SelLength = 0 then FSelStart := OldCaretPosition;
      FSelStart := CaretPosition;
      FSelLength := FSelLength - (CaretPosition - OldCaretPosition);
    end;
end;

procedure TscPasswordEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: Integer);
begin
  inherited;
  FLMouseSelecting := False;
end;

procedure TscPasswordEdit.PaintText;
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
      DrawPasswordChar(CR, False, Cnv);
  end;
end;

procedure TscPasswordEdit.PaintSelectedText;
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
      DrawPasswordChar(CR, True, Cnv);
  end;
end;

function TscPasswordEdit.GetVisibleSelText: String;
begin
  if SelStart + 1 >= FFVChar
  then Result := SelText
  else Result := Copy(SelText, FFVChar - SelStart, Length(SelText) - (FFVChar - SelStart) + 1);
end;

function TscPasswordEdit.GetNextWPos(StartPosition: Integer): Integer;
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

function TscPasswordEdit.GetPrivWPos(StartPosition: Integer): Integer;
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

procedure TscPasswordEdit.ClearSelection;
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

procedure TscPasswordEdit.SelectAll;
begin
  SetCaretPosition(Length(Text));
  SelStart := 0;
  SelLength := Length(Text);
  RePaintControl;
end;

procedure TscPasswordEdit.DrawPasswordChar(SymbolRect: TRect; Selected: Boolean; Cnv: TCanvas);
var
  R: TRect;
  C: TColor;
  IX, IY, IIndex: Integer;
begin
  if (FPasswordCharImages <> nil) and (FPasswordCharImageIndex >= 0) and
     (FPasswordCharImageIndex < FPasswordCharImages.Count) then
  begin
    IX := SymbolRect.Left + SymbolRect.Width div 2 - FPasswordCharImages.Width div 2;
    if IX < SymbolRect.Left then IX := SymbolRect.Left;
    IY := SymbolRect.Top + SymbolRect.Height div 2 - FPasswordCharImages.Height div 2;
    if IY < SymbolRect.Top then IY := SymbolRect.Top;
    IIndex := FPasswordCharImageIndex;
    if Selected and (FPasswordCharSelectedImageIndex >= 0) and
      (FPasswordCharSelectedImageIndex < FPasswordCharImages.Count)
    then
      IIndex := FPasswordCharSelectedImageIndex;
    FPasswordCharImages.Draw(Cnv, IX, IY, IIndex, True);
    Exit;
  end;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FPasswordCharImageIndex)
  then
  begin
    IIndex := FPasswordCharImageIndex;
    if Selected and FCustomImages.IsIndexAvailable(FPasswordCharSelectedImageIndex)
    then
      IIndex := FPasswordCharSelectedImageIndex;
    FCustomImages.Draw(Cnv, SymbolRect, IIndex, FScaleFactor);
    Exit;
  end;
  if not Enabled then
  begin
    if not IsCustomStyle then
      C := clGrayText
    else
      C := GetEditTextColor(scsDisabled);
    end
  else
  if Selected
  then
    C := clHighLightText
  else
    if (seFont in StyleElements) and IsCustomStyle then
    begin
      if FUseFontColorToStyleColor and Enabled then
        C := ColorToRGB(GetStyleColor(Self.Font.Color))
      else
      if FTransparent then
        C := scDrawUtils.GetCheckBoxTextColor(scsNormal)
      else
        C := scDrawUtils.GetEditTextColor(scsNormal);
    end
    else
      C := Font.Color;
  R := SymbolRect;
  InflateRect(R, -2, - (R.Height - R.Width) div 2 - 2);
  with Cnv do
  case FPasswordKind of
    pkPasswordChar:
      begin
        R := SymbolRect;
        InflateRect(R, -R.Width div 4, -R.Width div 4);
        if Font.Name <> 'Wingdings' then
          Font.Name := 'Wingdings';
        if Font.Height <> R.Height - 2 then
          Font.Height := R.Height - 2;
        if Font.Color <> C then
          Font.Color := C;
        Brush.Style := bsClear;
        R := Rect(R.Left + R.Width div 2 - TextWidth(PASSWORD_CHAR) div 2,
          R.Top + R.Height div 2 - TextHeight(PASSWORD_CHAR) div 2,
          R.Left + R.Width div 2 + TextWidth(PASSWORD_CHAR),
          R.Top + R.Height div 2 + TextHeight(PASSWORD_CHAR));
        Inc(R.Top);
        TextOut(R.Left, R.Top, PASSWORD_CHAR);
      end;
    pkRect:
      begin
        Brush.Color := C;
        FillRect(R);
      end;
    pkRoundRect:
      begin
        Brush.Color := C;
        Pen.Color := C;
        RoundRect (R.Left, R.Top + 1, R.Right, R.Bottom, R.Width div 2, R.Width div 2);
      end;
    pkTriangle:
      begin
        R := Rect(0, 0, R.Width, R.Height);
        if not Odd(R.Width) then R.Right := R.Right + 1;
        RectToCenter(R, SymbolRect);
        Pen.Color := C;
        Brush.Color := C;
        Polygon([
          Point(R.Left + R.Width div 2 + 1, R.Top),
          Point(R.Right, R.Bottom),
          Point(R.Left, R.Bottom)]);
      end;
    end;
end;

procedure TscPasswordEdit.SelectWord;
begin
  SelStart := GetPrivWPos(CaretPosition);
  SelLength := GetNextWPos(SelStart) - SelStart;
  CaretPosition := SelStart + SelLength;
end;

procedure TscPasswordEdit.UpdateCarete;
begin
  CreateCaret(Handle, 0, 0, Height - 6);
  CaretPosition := FCaretPosition;
  ShowCaret;
end;

procedure TscPasswordEdit.HideCaret;
begin
  WinApi.Windows.HideCaret(Handle);
end;

procedure TscPasswordEdit.ShowCaret;
begin
  if not (csDesigning in ComponentState) and Focused
  then
    WinApi.Windows.ShowCaret(Handle);
end;

function TscPasswordEdit.GetPasswordFigureWidth: Integer;
begin
  if FPasswordCharImages <> nil then
    Result := FPasswordCharImages.Width + 2
  else
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FPasswordCharImageIndex)
  then
  begin
    Result := FCustomImages.GetWidth(FPasswordCharImageIndex, FScaleFactor) + 2;
  end
  else
  if PasswordKind = pkPasswordChar then
    Result := GetEditRect.Height div 2
  else
  if PasswordKind = pkRect then
    Result := GetEditRect.Height div 2 + 3
  else
    Result := GetEditRect.Height div 2 + 2;
end;

procedure TscPasswordEdit.Change;
begin
  inherited Changed;
  if Enabled and HandleAllocated then SetCaretPosition(CaretPosition);
  if Assigned(FOnChange) then  FOnChange(Self);
end;

procedure TscPasswordEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TscPasswordEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  FLMouseSelecting := False;
  SelectWord;
end;

procedure TscPasswordEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Font.Assign(Font);
  UpdateCarete;
end;

function TscPasswordEdit.GetText: String;
begin
  Result := FText;
end;

procedure TscPasswordEdit.SetText(const Value: String);
var
  S, S1: String;
begin
  if not ValidText(Value) then Exit;
  S := Value;
  S1 := Text;
  if (Value <> '') and (CharCase <> ecNormal)
  then
    case CharCase of
      ecUpperCase: FText := AnsiUpperCase(S);
      ecLowerCase: FText := AnsiLowerCase(S);
    end
  else
    FText := S;
  RePaintControl;
  if S <> S1 then Change;
end;

procedure TscPasswordEdit.SetCaretPosition(const Value: Integer);
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
  if Focused then SetCaretPos(GetCharX(FCaretPosition), GetEditRect.Top + 1);
end;

procedure TscPasswordEdit.SetSelLength(const Value: Integer);
begin
  if FSelLength <> Value
  then
    begin
      FSelLength := Value;
      RePaintControl;
    end;
end;

procedure TscPasswordEdit.SetSelStart(const Value: Integer);
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

procedure TscPasswordEdit.SetAutoSelect(const Value: Boolean);
begin
  if FAutoSelect <> Value then FAutoSelect := Value;
end;

function TscPasswordEdit.GetSelStart: Integer;
begin
  if FSelLength > 0
  then
    Result := FSelStart
  else
    if FSelLength < 0
    then Result := FSelStart + FSelLength
    else Result := CaretPosition;
end;

function TscPasswordEdit.GetSelRect: TRect;
begin
  Result := GetEditRect;
  Result.Left := GetCharX(SelStart);
  Result.Right := GetCharX(SelStart + SelLength);
  IntersectRect(Result, Result, GetEditRect);
  Inc(Result.Top);
  Dec(Result.Bottom);
end;

function TscPasswordEdit.GetSelLength: Integer;
begin
  Result := Abs(FSelLength);
end;

function TscPasswordEdit.GetSelText: String;
begin
  Result := Copy(Text, SelStart + 1, SelLength);
end;

procedure TscPasswordEdit.SetCharCase(const Value: TEditCharCase);
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
            ecUpperCase: Text := AnsiUpperCase(S);
            ecLowerCase: Text := AnsiLowerCase(S);
          end;
        end;
    end;
end;

procedure TscPasswordEdit.SetHideSelection(const Value: Boolean);
begin
  if FHideSelection <> Value
  then
    begin
      FHideSelection := Value;
      RePaintControl;
    end;
end;

procedure TscPasswordEdit.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then FMaxLength := Value;
end;

procedure TscPasswordEdit.SetCursor(const Value: TCursor);
begin
  if Value = crDefault
  then inherited Cursor := crIBeam
  else inherited Cursor := Value;
end;

function TscPasswordEdit.ValidText(NewText: String): Boolean;
begin
  Result := True;
end;

procedure TscPasswordEdit.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value
  then
    begin
      FTextAlignment := Value;
      RePaintControl;
    end;
end;

procedure TscPasswordEdit.UpdateCaretePosition;
begin
  SetCaretPosition(CaretPosition);
end;

procedure TscPasswordEdit.InsertText(AText: String);
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

procedure TscPasswordEdit.InsertChar(Ch: Char);
begin
  if ReadOnly then Exit;
  InsertText(Ch);
end;

procedure TscPasswordEdit.InsertAfter(Position: Integer; S: String;
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

procedure TscPasswordEdit.DeleteFrom(Position, Length: Integer; MoveCaret : Boolean);
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

procedure TscPasswordEdit.SetPasswordKind(const Value: TscPasswordKind);
begin
  if FPasswordKind <> Value
  then
    begin
      FPasswordKind := Value;
      RePaintControl;
    end;
end;

procedure TscPasswordEdit.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  FText := inherited Text;
  SelLength := 0;
  RePaintControl;
end;

procedure TscPasswordEdit.Clear;
begin
  Text := '';
end;

procedure TscPasswordEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if HandleAllocated then RePaintControl;
end;

procedure TscPasswordEdit.CMMouseEnter;
begin
  inherited;
  FMouseIn := True;
  if (not Focused) then RePaintControl;
end;

procedure TscPasswordEdit.CMMouseLeave;
begin
  inherited;
  FMouseIn := False;
  if not Focused then RePaintControl;
end;

constructor TscColorListBox.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FShowFocusRect := True;
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
end;

procedure TscColorListBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  IH: Integer;
begin
  IH := MulDiv(ItemHeight, M, D);
  inherited;
  ItemHeight := IH;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscColorListBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscColorListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);

procedure PaintBG(DC: HDC);
var
  FCanvas: TCanvas;
  I, J, W, H, IH: Integer;
  R: TRect;
  DrawCount: Integer;
begin
  J := 0;
  W := ClientWidth;
  H := ClientHeight;
  if (Count > 0) and (ItemHeight > 0) then
  begin
    IH := ItemHeight;
    DrawCount := H div ItemHeight;
    I := TopIndex;
    J := I + DrawCount;
    if J > Count - 1 then
      J := Count - 1;
    J := J - TopIndex + 1;
    if J < 0 then J := 0;
  end
  else
    IH := 0;
  R := Rect(0, j * IH, W, H);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := DC;
  try
    FCanvas.Brush.Color := Self.Brush.Color;
    FCanvas.FillRect(R);
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
end;

begin
  if not IsCustomStyle then
  begin
    PaintBG(Message.DC);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TscColorListBox.WMSIZE(var Msg: TMessage);
begin
  inherited;
  RePaint;
end;

procedure TscColorListBox.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

function TscColorListBox.CanDrawFocusRect: Boolean;
begin
  Result := FShowFocusRect;
end;

procedure TscColorListBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TscColorListBox.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

procedure TscColorListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  R, FR: TRect;
  Buffer: TBitmap;
  IIndex: Integer;
  C: TColor;
begin
  with Message.DrawItemStruct^ do
  begin
    if rcItem.Height * rcItem.Width = 0 then
    begin
      Message.Result := 1;
      Exit;
    end;
    IIndex := itemID;
    State := TOwnerDrawState(LoWord(itemState));
    if (csPaintCopy in ControlState) then
    begin
      if (IIndex = ItemIndex) and not (odSelected in State) then
        Include(State, odSelected);
    end;
    Buffer := TBitmap.Create;
    try
      Canvas.Handle := hDC;
      Buffer.SetSize(rcItem.Width, rcItem.Height);
      Buffer.Canvas.Font := Font;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      with Buffer.Canvas do
      begin
        if (FSelectionStyle = scstColor) and
           (odSelected in State)
        then
        begin
          if FSelectionColor <> clNone then
          begin
            Font.Color := FSelectionTextColor;
            Brush.Color := FSelectionColor;
          end
          else
          begin
            Font.Color := GetStyleColor(clHighLightText);
            Brush.Color := GetStyleColor(clHighLight);
          end;
        end
        else
        begin
          if {$IFNDEF VER230}(seClient in StyleElements) and {$ENDIF} IsCustomStyle then
          begin
            if Enabled then
            begin
              {$IFNDEF VER230}
              if seFont in StyleElements then
                Font.Color := GetEditTextColor(scsNormal)
              else
                Font.Color := Self.Font.Color;
              {$ELSE}
              Font.Color := GetEditTextColor(scsNormal);
              {$ENDIF}
            end
            else
            begin
              Font.Color := GetEditTextColor(scsDisabled);
            end;
            Brush.Color := GetEditBrushColor(scsNormal);
          end
          else
          begin
            Font.Color := Self.Font.Color;
            Brush.Color := Self.Color;
            if not Enabled then
              if IsCustomStyle then
                Font.Color := GetEditTextColor(scsDisabled)
              else
                Font.Color := clGrayText;
          end;
        end;
         if (SelectionStyle = scstColor) and (odSelected in State)
           and not Focused and not FShowFocusRect then
        begin
          C := Brush.Color;
          Brush.Color := Self.Color;
          FillRect(R);
          Brush.Color := C;
          FillRectWithAlpha(Buffer.Canvas, R, 200);
        end
        else
          FillRect(R);
      end;
      if (odSelected in State) then
      begin
        if FSelectionStyle = scstStyled then
        begin
          Buffer.Canvas.Font.Color := GetSelectionTextColor;
          DrawSelection(Buffer.Canvas, R, Focused and (odFocused in State), FShowFocusRect);
        end;
      end;
      if Focused and (odFocused in State) then
      begin
        if CanDrawFocusRect or (IIndex < 0) then
        begin
          FR := R;
          if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
          and (FSelectionStyle = scstStyled) then
            InflateRect(FR, -1, -1);
          scDrawFocusRect(Buffer.Canvas, FR, FScaleFactor);
        end;
      end;
      Buffer.Canvas.Brush.Style := bsClear;
      if FScaleFactor >= 1.5 then
      begin
        Inc(R.Left);
        Dec(R.Right);
      end;
      if IIndex >= 0 then
        DrawItemContent(Buffer.Canvas, R, IIndex, State);
      Canvas.Draw(rcItem.Left, rcItem.Top, Buffer);
    finally
      Buffer.Free;
      Canvas.Handle := 0;
    end;
  end;
  Message.Result := 1;
end;

procedure TscColorListBox.DrawItemContent(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AState: TOwnerDrawState);
var
  R, R1: TRect;
  S: String;
begin
  R := ARect;
  InflateRect(R, -1, -1);
  Inc(R.Left);
  Dec(R.Right);
  Dec(R.Bottom);
  R1 := R;
  Inc(R1.Top);
  R1.Right := R1.Left + R1.Height;
  with ACanvas do
  begin
    Brush.Color := Colors[AIndex];
    FillRect(R1);
    Frame3D(ACanvas, R1, ACanvas.Font.Color, ACanvas.Font.Color, 1);
  end;
  R.Left := R1.Right + 5;
  if AIndex >= 0 then
  begin
    S := Items[AIndex];
    ACanvas.Brush.Style := bsClear;
    scDrawText(ACanvas, S, R, False, True);
  end;
end;

constructor TscColorBox.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FSelectionColor := clNone;
  FSelectionTextColor := clHighLightText;
  FShowFocusRect := True;
  FTempItemIndex := -1;
end;

procedure TscColorBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  IH: Integer;
begin
  IH := MulDiv(ItemHeight, M, D);
  inherited;
  ItemHeight := IH;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscColorBox.WndProc(var Message: TMessage);
begin
  if IsCustomDraw then
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
      if Visible then
      begin
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
      end;
  end;
  inherited;
  if IsCustomDraw then
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, CB_SETCURSEL:
      if Visible then
      begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
      end;
  end;
end;

function TscColorBox.IsFocused: Boolean;
begin
  Result := (Handle <> 0) and (GetFocus = Handle);
end;

procedure TscColorBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

function TscColorBox.IsCustomDraw: Boolean;
begin
  Result := StyleKind <> scscbDefault;
end;


procedure TscColorBox.WMPaint(var Message: TWMPaint);
var
  Canvas: TCanvas;
  PS: TPaintStruct;
  DC: HDC;
  R, R1, BR: TRect;
  Buffer: TBitmap;
  FState: TscsCtrlState;
  BW: Integer;
  FC: TColor;
  IIndex: Integer;
begin
  if IsCustomDraw then
  begin
    DC := Message.DC;
    Canvas := TCanvas.Create;
    Buffer := TBitmap.Create;
    try
      if DC = 0 then
        Canvas.Handle := BeginPaint(Handle, PS)
      else
        Canvas.Handle := DC;
      Buffer.SetSize(Width, Height);
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      if (Parent <> nil) and (Parent is TscCustomControl) then
        TscCustomControl(Parent).FGetControlBG := True;
      try
        DrawParentBackground(Self, Buffer.Canvas);
      finally
        if (Parent <> nil) and (Parent is TscCustomControl) then
          TscCustomControl(Parent).FGetControlBG := False;
      end;
      if DroppedDown then
        FState := scsPressed
      else
      if IsFocused and not FShowFocusRect and IsCustomStyle then
        FState := scsFocused
      else
      if MouseInClient and not (csDesigning in ComponentState) then
        FState := scsHot
      else
      if Enabled then
        FState := scsNormal
      else
        FState := scsDisabled;
      DrawButton(Buffer.Canvas, R, FState, False);
      Buffer.Canvas.Font := Self.Font;
      FC := GetButtonTextColor(FState);
      Buffer.Canvas.Font.Color := FC;
      // draw item
      InflateRect(R, -2, -2);
      BW := GetSystemMetrics(SM_CXVSCROLL);
      if BidiMode = bdRightToLeft then
      begin
        Inc(R.Left, BW);
        BR := Rect(2, R.Top, R.Left, R.Bottom);
      end
      else
      begin
        Dec(R.Right, BW);
        BR := Rect(R.Right, R.Top, Width - 2, R.Bottom);
      end;
      R1 := R;
      Inc(R.Left, 2);
      Dec(R.Right, 2);
      Buffer.Canvas.Brush.Style := bsClear;
      if Self.DroppedDown then
        IIndex := FTempItemIndex
      else
        IIndex := ItemIndex;
      if IIndex <> -1 then
        DrawItemContent(Buffer.Canvas, R, IIndex, [], True);
      if IsFocused and (FState <> scsPressed) and (FShowFocusRect or not IsCustomStyle) then
      begin
        InflateRect(R1, -2, -2);
        scDrawFocusRect(Buffer.Canvas, R1, FScaleFactor);
      end;
      if Enabled then
        FState := scsNormal
      else
        FState := scsDisabled;
      if IsCustomStyle or not StyleServices.Enabled or IsWindowsXP then
        DrawComboArrowImage(Buffer.Canvas, BR, FC, FScaleFactor)
      else
        DrawDropDownButton(Buffer.Canvas, BR, FState, True, False, FScaleFactor);
      Canvas.Draw(0, 0, Buffer);
    finally
      Buffer.Free;
      Canvas.Handle := 0;
      Canvas.Free;
      if DC = 0 then
        EndPaint(Handle, PS);
    end;
  end
  else
    inherited;
end;

procedure TscColorBox.SetStyleKind(Value: TscComboStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    Invalidate;
  end;
end;

procedure TscColorBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscColorBox.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

procedure TscColorBox.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE) then
  begin
    FTempItemIndex := ItemIndex;
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  end;
  inherited;
end;

procedure TscColorBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  R: TRect;
  Buffer: TBitmap;
begin
  with Message.DrawItemStruct^ do
  begin
    if (rcItem.Height * rcItem.Width = 0) or (Integer(itemID) < 0) then
    begin
      State := TOwnerDrawState(LoWord(itemState));
      if (Integer(itemID) < 0) and (odFocused in State) then
      begin
        Canvas.Handle := hDC;
        if {$IFNDEF VER230} (seFont in StyleElements) and {$ENDIF} IsCustomStyle then
          scDrawColorFocusRect(Canvas, rcItem, GetStyleColor(clWindowText))
        else
          scDrawColorFocusRect(Canvas, rcItem, Font.Color);
        Canvas.Handle := 0;
      end;
      Message.Result := 1;
      Exit;
    end;
    State := TOwnerDrawState(LoWord(itemState));
    Buffer := TBitmap.Create;
    try
      Canvas.Handle := hDC;
      Buffer.SetSize(rcItem.Width, rcItem.Height);
      Buffer.Canvas.Font := Font;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      with Buffer.Canvas do
      begin
        if (FSelectionStyle = scstColor) and
           (odSelected in State)
        then
        begin
         if FSelectionColor <> clNone then
          begin
            Font.Color := FSelectionTextColor;
            Brush.Color := FSelectionColor;
          end
          else
          begin
            Font.Color := GetStyleColor(clHighLightText);
            Brush.Color := GetStyleColor(clHighLight);
          end;
        end
        else
        begin
          if {$IFNDEF VER230}(seClient in StyleElements) and {$ENDIF} IsCustomStyle then
          begin
            {$IFNDEF VER230}
            if seFont in StyleElements then
              Font.Color := GetEditTextColor(scsNormal)
            else
              Font.Color := Self.Font.Color;
            {$ELSE}
            Font.Color := GetEditTextColor(scsNormal);
            {$ENDIF}
            if not Enabled then
              Font.Color := GetEditTextColor(scsDisabled);
            Brush.Color := GetEditBrushColor(scsNormal);
          end
          else
          begin
            Font.Color := Self.Font.Color;
            Brush.Color := Self.Color;
            if not Enabled then
              if IsCustomStyle then
                Font.Color := GetEditTextColor(scsDisabled)
              else
                Font.Color := clGrayText;
          end;
        end;
        FillRect(R);
      end;
      if (odSelected in State) then
      begin
        if FSelectionStyle = scstStyled then
        begin
          Buffer.Canvas.Font.Color := GetSelectionTextColor;
          DrawSelection(Buffer.Canvas, R, (odFocused in State) and not DroppedDown, True);
        end;
      end;
      Buffer.Canvas.Brush.Style := bsClear;
      DrawItemContent(Buffer.Canvas, R, itemID, State, False);
      Canvas.Draw(rcItem.Left, rcItem.Top, Buffer);
    finally
      Buffer.Free;
      Canvas.Handle := 0;
    end;
  end;
  Message.Result := 1;
end;

procedure TscColorBox.DrawItemContent(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AState: TOwnerDrawState; AComboItem: Boolean);
var
  R, R1: TRect;
  S: String;
begin
  R := ARect;
  InflateRect(R, -1, -1);
  Inc(R.Left);
  Dec(R.Right);
  Dec(R.Bottom);
  R1 := R;
  Inc(R1.Top);
  R1.Right := R1.Left + R1.Height;
  with ACanvas do
  begin
    if (StyleKind <> scscbDefault) and AComboItem then
     InflateRect(R1, -1, -1);
    Brush.Color := Colors[AIndex];
    FillRect(R1);
    Frame3D(ACanvas, R1, ACanvas.Font.Color, ACanvas.Font.Color, 1);
  end;
  R.Left := R1.Right + 5;
  if AIndex >= 0 then
  begin
    S := Items[AIndex];
    ACanvas.Brush.Style := bsClear;
    scDrawText(ACanvas, S, R, False, True);
  end;
end;

procedure TscColorBox.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

constructor TscComboBoxStyleHook.Create;
begin
  inherited;
  if Style = csSimple then
    OverrideEraseBkgnd := True;
  FMouseOnButton := False;
  FEditHandle := 0;
  FListHandle := 0;
  FListBoxInstance := nil;
  FIgnoreStyleChanged := False;
  FVSliderState := tsThumbBtnVertNormal;
  FVUpState := tsArrowBtnUpNormal;
  FVDownState := tsArrowBtnDownNormal;
  FSliderSize := 0;
  FListBoxTimerCode := 0;
  FListBoxUpBtnDown := False;
  FListBoxDownBtnDown := False;
  FListBoxTrackUpDown := False;
  FListBoxTrackDownDown := False;
  FListBoxDown := False;
  FTempItemIndex := -1;
  UpdateColors;
end;

destructor TscComboBoxStyleHook.Destroy;
begin
  if (FListHandle <> 0) and (FListBoxInstance <> nil) then
  begin
    SetWindowLong(FListHandle, GWL_WNDPROC, IntPtr(FDefListBoxProc));
    FreeObjectInstance(FListBoxInstance);
    FListBoxInstance := nil;
  end;
  if FListBoxTimerCode <> 0 then
    ListBoxStopTimer;
  inherited;
end;

{$IFNDEF VER230}
function TscComboBoxStyleHook.IsChildHandle(AHandle: HWnd): Boolean;
begin
  Result := (FEditHandle <> 0) and (FEditHandle = AHandle);
end;

function TscComboBoxStyleHook.AcceptMessage(var Message: TMessage): Boolean;
begin
  Result := seBorder in Control.StyleElements;
end;
{$ENDIF}

procedure TscComboBoxStyleHook.ListBoxSetTimer(ATimerCode: Integer);
begin
  if FListBoxTimerCode <> 0 then ListBoxStopTimer;
  FListBoxTimerCode := ATimerCode;
  if ATimerCode < 4 then
    SetTimer(FListHandle, 1, 300, nil)
  else
    SetTimer(FListHandle, 1, 50, nil);
end;

procedure TscComboBoxStyleHook.ListBoxStopTimer;
begin
  FListBoxTimerCode := -1;
  KillTimer(FListHandle, 1);
end;

function TscComboBoxStyleHook.DroppedDown: Boolean;
begin
  if (Control <> nil) and (Control is TCustomComboBox) then
    Result := TCustomComboBox(Control).DroppedDown
  else if Handle <> 0 then
    Result := LongBool(SendMessage(Handle, CB_GETDROPPEDSTATE, 0, 0))
  else
    Result := False;
end;

function TscComboBoxStyleHook.GetButtonRect: TRect;
begin
  Result := Control.ClientRect;
  InflateRect(Result, -2, -2);
  if Control.BiDiMode <> bdRightToLeft then
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL) + 1
  else
    Result.Right := Result.Left + GetSystemMetrics(SM_CXVSCROLL) - 1;
end;


type
  THookComboBoxClass = class(TCustomComboBox);

function TscComboBoxStyleHook.Style: TComboBoxStyle;
const
  ComboBoxStyles: array[TComboBoxStyle] of DWORD = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWFIXED,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE);
var
  LStyle: Cardinal;
begin
  if (Control <> nil) and (Control is TCustomComboBox) then
    Result :=  THookComboBoxClass(Control).Style
  else if Handle <> 0 then
  begin
    LStyle := GetWindowLong(Handle, GWL_STYLE);
    Result := csDropDown;
    if LStyle and ComboBoxStyles[csDropDown] = ComboBoxStyles[csDropDown] then
      Result := csDropDown;
    if LStyle and ComboBoxStyles[csSimple] = ComboBoxStyles[csSimple] then
      Result := csSimple;
    if LStyle and ComboBoxStyles[csDropDownList] = ComboBoxStyles[csDropDownList] then
      Result := csDropDownList;
    if LStyle and ComboBoxStyles[csOwnerDrawFixed] = ComboBoxStyles[csOwnerDrawFixed] then
      Result := csOwnerDrawFixed;
    if LStyle and ComboBoxStyles[csOwnerDrawVariable] = ComboBoxStyles[csOwnerDrawVariable] then
      Result := csOwnerDrawVariable;
  end
  else
    Result := csDropDown;
end;

procedure TscComboBoxStyleHook.UpdateColors;
const
  ColorStates: array[Boolean] of TStyleColor = (scComboBoxDisabled, scComboBox);
  FontColorStates: array[Boolean] of TStyleFont = (sfComboBoxItemDisabled, sfComboBoxItemNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  {$IFNDEF VER230}
  if seClient in Control.StyleElements then
    Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled])
  else
    Brush.Color := TWinControlClass(Control).Color;
  {$ELSE}
    Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
  {$ENDIF}
  {$IFNDEF VER230}
  if seFont in Control.StyleElements then
    FontColor := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled])
  else
    FontColor := TWinControlClass(Control).Font.Color;
  {$ELSE}
    Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
  {$ENDIF}
end;

procedure TscComboBoxStyleHook.PaintBorder(Canvas: TCanvas);
var
  R, ControlRect, EditRect, ListRect: TRect;
  DrawState: TThemedComboBox;
  BtnDrawState: TThemedComboBox;
  Details: TThemedElementDetails;
  Buffer: TBitmap;
begin
  if not StyleServices.Available then Exit;

  if not Control.Enabled then
    BtnDrawState := tcDropDownButtonDisabled
  else if DroppedDown then
    BtnDrawState := tcDropDownButtonPressed
  else if FMouseOnButton then
    BtnDrawState := tcDropDownButtonHot
  else
    BtnDrawState := tcDropDownButtonNormal;

  if not Control.Enabled then
    DrawState := tcBorderDisabled
  else
  if Control.Focused then
    DrawState := tcBorderFocused
  else if MouseInControl then
    DrawState := tcBorderHot
  else
    DrawState := tcBorderNormal;

  Buffer := TBitMap.Create;
  Buffer.SetSize(Control.Width, Control.Height);
  try
    R := Rect(0, 0, Buffer.Width, Buffer.Height);

    Details := StyleServices.GetElementDetails(DrawState);
    if (Style = csSimple) and (FListHandle <> 0) then
    begin
      GetWindowRect(FListHandle, ListRect);
      GetWindowRect(Handle, ControlRect);
      R.Bottom := ListRect.Top - ControlRect.Top;
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);
      R := Rect(0, Control.Height - (ControlRect.Bottom - ListRect.Bottom),
        Control.Width, Control.Height);
      with Buffer.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := StyleServices.GetSystemColor(clBtnFace);
        FillRect(R);
      end;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      R.Bottom := ListRect.Top - ControlRect.Top;
    end
    else
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);

    {$IFNDEF VER230}
    if not (seClient in Control.StyleElements) and (FEditHandle = 0) then
    begin
      R := Control.ClientRect;
      InflateRect(R, -3, -3);
      R.Right := ButtonRect.Left - 2;
      with Buffer.Canvas do
      begin
        Brush.Color := TWinControlClass(Control).Color;
        FillRect(R);
      end;
    end;
    {$ENDIF}

    if Style <> csSimple then
    begin
      Details := StyleServices.GetElementDetails(BtnDrawState);
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, ButtonRect);
    end;

    if (SendMessage(Handle, CB_GETCURSEL, 0, 0) >= 0) and (FEditHandle = 0) then
    begin
      R := Control.ClientRect;
      InflateRect(R, -3, -3);
      R.Right := ButtonRect.Left - 2;
      ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    end
    else
    if FEditHandle <> 0 then
    begin
      GetWindowRect(Handle, R);
      GetWindowRect(FEditHandle, EditRect);
      OffsetRect(EditRect, -R.Left, -R.Top);
      with EditRect do
        ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    end;
    // draw buffer
    Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TscComboBoxStyleHook.DrawItem(Canvas: TCanvas;
  Index: Integer; const R: TRect; Selected: Boolean);
var
  DIS: TDrawItemStruct;
begin
  FillChar(DIS, SizeOf(DIS), 0);
  DIS.CtlType := ODT_COMBOBOX;
  DIS.CtlID := GetDlgCtrlID(Handle);
  DIS.itemAction := ODA_DRAWENTIRE;
  DIS.hDC := Canvas.Handle;
  DIS.hwndItem := Handle;
  DIS.rcItem := R;
  DIS.itemID := Index;
  DIS.itemData := SendMessage(FListHandle, LB_GETITEMDATA, 0, 0);
  if Selected then
    DIS.itemState := DIS.itemState or ODS_FOCUS or ODS_SELECTED;

  SendMessage(Handle, WM_DRAWITEM, Handle, LPARAM(@DIS));
end;

procedure TscComboBoxStyleHook.WMParentNotify(var Message: TMessage);
begin
  if (FListHandle = 0) and (LoWord(Message.WParam) = WM_CREATE) then
  begin
    if (Message.LParam <> 0) and (FListBoxInstance = nil) then
      HookListBox(Message.LParam);
  end
  else if (FEditHandle = 0) and (LoWord(Message.WParam) = WM_CREATE) then
    FEditHandle := Message.LParam;
end;

procedure TscComboBoxStyleHook.WndProc(var Message: TMessage);
const
  States: array[Boolean] of TStyleColor = (scEditDisabled, scComboBox);
begin
  case Message.Msg of
    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC,
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        {$IFNDEF VER230}
        if seClient in Control.StyleElements then
          Brush.Color := StyleServices.GetStyleColor(States[Control.Enabled])
        else
          Brush.Color := TWinControlClass(Control).Color;
        {$ELSE}
         Brush.Color := StyleServices.GetStyleColor(States[Control.Enabled]);
        {$ENDIF}
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
        Handled := True;
      end;
    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        Handled := False; // Allow control to handle message
      end;
    CM_FOCUSCHANGED:
      begin
        Invalidate;
        Handled := False; // Allow control to handle message
      end;
  else
    inherited WndProc(Message);
  end;
end;

procedure TscComboBoxStyleHook.MouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TscComboBoxStyleHook.MouseLeave;
begin
  inherited;
  if not DroppedDown and FMouseOnButton then
    FMouseOnButton := False;
  Invalidate;
end;

procedure TscComboBoxStyleHook.WMMouseMove(var Message: TWMMouse);
var
  P: TPoint;
  R: TRect;
  FOldMouseOnButton: Boolean;
begin
  CallDefaultProc(TMessage(Message));
  inherited;

  P := Point(Message.XPos, Message.YPos);
  FOldMouseOnButton := FMouseOnButton;
  R := ButtonRect;
  if R.Contains(P) then
    FMouseOnButton := True
  else
    FMouseOnButton := False;

  if FOldMouseOnButton <> FMouseOnButton then
    InvalidateRect(Handle, @R, False);

  Handled := True;
end;

procedure TscComboBoxStyleHook.CNDrawItem(var Message: TWMDrawItem);
begin
  WMDrawItem(Message);
  Handled := True;
end;

procedure TscComboBoxStyleHook.WMDrawItem(var Message: TWMDrawItem);
begin
  CallDefaultProc(TMessage(Message));
  Handled := True;
end;

procedure TscComboBoxStyleHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if (Control is TscCustomComboBox) and not TscCustomComboBox(Control).IsCustomDraw and
     (TscCustomComboBox(Control).Style <> csDropDown) and
     (TscCustomComboBox(Control).Style <> csSimple) then
  begin
    WMPaint(TMessage(Message));
    Message.Result := 1;
    Handled := True;
  end
  else
   inherited;
end;

procedure TscComboBoxStyleHook.WMPaint(var Message: TMessage);
var
  R: TRect;
  Canvas: TCanvas;
  PS: TPaintStruct;
  SaveIndex: Integer;
  DC: HDC;
begin
  if (Control is TscCustomComboBox) and TscCustomComboBox(Control).IsCustomDraw then
  begin
    Handled := False;
    Exit;
  end;
  if (Control is TscColorBox) and TscColorBox(Control).IsCustomDraw then
  begin
    Handled := False;
    Exit;
  end;
  DC := Message.WParam;
  Canvas := TCanvas.Create;
  try
    if DC = 0 then
      Canvas.Handle := BeginPaint(Handle, PS)
    else
      Canvas.Handle := DC;

    SaveIndex := SaveDC(Canvas.Handle);
    try
      PaintBorder(Canvas);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;

    if (Style <> csSimple) and (FEditHandle = 0) then
    begin
      R := Control.ClientRect;
      InflateRect(R, -3, -3);
      if Control.BiDiMode <> bdRightToLeft then
        R.Right := ButtonRect.Left - 1
      else
        R.Left := ButtonRect.Right + 1;
      SaveIndex := SaveDC(Canvas.Handle);
      try
        IntersectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        if not DroppedDown then
          DrawItem(Canvas, TComboBox(Control).ItemIndex, R, Focused)
        else
          DrawItem(Canvas, FTempItemIndex, R, Focused)
      finally
        RestoreDC(Canvas.Handle, SaveIndex);
      end;
    end;

  finally
    Canvas.Handle := 0;
    Canvas.Free;
    if DC = 0 then
      EndPaint(Handle, PS);
  end;

  Handled := True;
end;

procedure TscComboBoxStyleHook.WMCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or (Message.NotifyCode = CBN_SELENDOK) or
     (Message.NotifyCode = CBN_CLOSEUP) or (Message.NotifyCode = CBN_DROPDOWN) or
     (Message.NotifyCode = CBN_SELCHANGE) then
  begin
    if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE) then
      FTempItemIndex := TComboBox(Control).ItemIndex;

    if FListBoxTimerCode <> 0 then
      ListBoxStopTimer;
    FMouseOnButton := False;
    Invalidate;
  end;
end;

procedure TscComboBoxStyleHook.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or (Message.NotifyCode = CBN_SELENDOK) or
     (Message.NotifyCode = CBN_CLOSEUP) or (Message.NotifyCode = CBN_DROPDOWN) or
     (Message.NotifyCode = CBN_SELCHANGE)  then
  begin
    if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE) then
      FTempItemIndex := TComboBox(Control).ItemIndex;

    if FListBoxTimerCode <> 0 then ListBoxStopTimer;
    FMouseOnButton := False;
    Invalidate;
  end;
end;

procedure TscComboBoxStyleHook.HookListBox(AListHandle: HWND);
begin
  if (AListHandle <> 0) and (FListBoxInstance = nil) then
  begin
    FListHandle := AListHandle;
    FListBoxInstance := MakeObjectInstance(ListBoxWndProc);
    FDefListBoxProc := Pointer(GetWindowLong(FListHandle, GWL_WNDPROC));
    SetWindowLong(FListHandle, GWL_WNDPROC, IntPtr(FListBoxInstance));
  end;
end;


function TscComboBoxStyleHook.ListBoxBoundsRect: TRect;
begin
  GetWindowRect(FListHandle, Result);
end;

function TscComboBoxStyleHook.ListBoxClientRect: TRect;
begin
   GetClientRect(FListHandle, Result);
end;

function TscComboBoxStyleHook.ListBoxVertScrollRect: TRect;
begin
  Result := ListBoxBoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  InflateRect(Result, -1, -1);
  OffsetRect(Result, 1, 1);
  if Control.BiDiMode <> bdRightToLeft then
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL)
  else
    Result.Right := Result.Left + GetSystemMetrics(SM_CXVSCROLL);
  if ListBoxBoundsRect.Height > 30 then OffsetRect(Result, -1, -1);
end;

function TscComboBoxStyleHook.ListBoxVertDownButtonRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Height > 0 then
    Result.Top := Result.Bottom - Min(GetSystemMetrics(SM_CYVTHUMB), Result.Height div 2)
  else
    Result := TRect.Empty;
 end;

function TscComboBoxStyleHook.ListBoxVertUpButtonRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Height > 0 then
    Result.Bottom := Result.Top + Min(GetSystemMetrics(SM_CYVTHUMB), Result.Height div 2)
  else
    Result := TRect.Empty;
end;

function TscComboBoxStyleHook.ListBoxVertScrollArea: TRect;
begin
  if GetWindowLong(FListHandle, GWL_STYLE) and WS_VSCROLL = 0 then
  begin
    Result := TRect.Empty;
    Exit;
  end;
  Result := ListBoxBoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  if Control.BiDiMode <> bdRightToLeft then
     Result.Left := Result.Right - GetSystemMetrics(SM_CYVSCROLL) - 1
   else
     Result.Right := Result.Left + GetSystemMetrics(SM_CYVSCROLL);
end;

function TscComboBoxStyleHook.ListBoxVertSliderRect: TRect;
var
  I, LVisibleHeight, LTotalHeight, LSize, LTotalSize,
  LFinalHeight, LItemHeight, LBoundsHeight, LBorderHeight,
  LTopIndex, LItemCount: Integer;
  MinW: Integer;
begin
  Result := ListBoxVertScrollRect;
  Result.Top := ListBoxVertUpButtonRect.Bottom;
  Result.Bottom := ListBoxVertDownButtonRect.Top;
  LSize := Result.Bottom - Result.Top;
  LTopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
  LItemCount := SendMessage(FListHandle, LB_GETCOUNT, 0, 0);
  LTotalSize := LItemCount * LSize;

  if LTotalSize = 0 then
    Exit;

  Result.Top := Result.Top + Round(LTopIndex / LItemCount * LSize);

  LTotalHeight := 1;
  FInvsibleCount := 0;
  LBoundsHeight := ListBoxBoundsRect.Height;
  LItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
  for I := 0 to LItemCount - 1 do
  begin
    LTotalHeight := LTotalHeight + LItemHeight;
    if (LTotalHeight > LBoundsHeight) and (FInvsibleCount = 0) then
      FInvsibleCount := LItemCount - I;
  end;

  LVisibleHeight := 0;
  for I := LTopIndex to LItemCount - 1 do
  begin
    LVisibleHeight := LVisibleHeight + LItemHeight;
    if Style <> csSimple then LBorderHeight := 2 else
      LBorderHeight := 4;
    if LVisibleHeight >= ListBoxBoundsRect.Height - LBorderHeight then
      Break;
  end;

  MinW := GetSystemMetrics(SM_CXVSCROLL) div 2;

  Result.Bottom := Result.Top + Round(LVisibleHeight / LTotalHeight * LSize);
  if Result.Height < MinW then
  begin
    Dec(LSize, MinW - Result.Height + 1);
    Result.Top := ListBoxVertUpButtonRect.Bottom +
      Round(LTopIndex / LItemCount * LSize);
    Result.Bottom := Result.Top + MinW;
  end;

  if (I = LItemCount - 1) and
     (Result.Bottom <> ListBoxVertDownButtonRect.Top) then
  begin
    LFinalHeight := Result.Height;
    Result.Bottom := ListBoxVertDownButtonRect.Top;
    Result.Top := Result.Bottom - LFinalHeight;
  end;
  FSliderSize := Result.Height;
end;

function TscComboBoxStyleHook.ListBoxVertTrackRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
  end
  else
    Result := TRect.Empty;
end;

function TscComboBoxStyleHook.ListBoxVertTrackRectUp: TRect;
begin
  Result := ListBoxVertTrackRect;
  if (Result.Width > 0) and (ListBoxVertSliderRect.Height > 0) then
    Result.Bottom := ListBoxVertSliderRect.Top;
end;

function TscComboBoxStyleHook.ListBoxVertTrackRectDown: TRect;
begin
  Result := ListBoxVertTrackRect;
  if (Result.Width > 0) and (ListBoxVertSliderRect.Height > 0) then
    Result.Top := ListBoxVertSliderRect.Bottom;
end;

procedure TscComboBoxStyleHook.PaintListBoxBorder(Canvas: TCanvas; const R: TRect);
begin
  with Canvas do
  begin
    Brush.Color := StyleServices.GetSystemColor(clWindowFrame);
    FillRect(R);
  end;
end;

procedure TscComboBoxStyleHook.DrawListBoxVertScroll(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  Canvas: TCanvas;
  R: TRect;
begin
  if not Self.DroppedDown then Exit;

  if GetWindowLong(FListHandle, GWL_STYLE) and WS_VSCROLL = 0 then
    Exit;

  Canvas := TCanvas.Create;
  try
    if DC <> 0 then
      Canvas.Handle := DC
    else
      Canvas.Handle := GetWindowDC(FListHandle);
    if ListBoxVertScrollRect.Width > 0 then
    begin
      B := TBitmap.Create;
      try
        B.Width := ListBoxVertScrollRect.Width;
        B.Height := ListBoxVertScrollRect.Height;
        MoveWindowOrg(B.Canvas.Handle, -ListBoxVertScrollRect.Left, -ListBoxVertScrollRect.Top);

        if StyleServices.Available then
        begin
          R := ListBoxVertScrollRect;
          R.Top := ListBoxVertUpButtonRect.Bottom;
          R.Bottom := ListBoxVertDownButtonRect.Top;
          if R.Height > 0 then
          begin
            Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
            StyleServices.DrawElement(B.Canvas.Handle, Details, R);
          end;
          Details := StyleServices.GetElementDetails(FVSliderState);
          StyleServices.DrawElement(B.Canvas.Handle, Details, ListBoxVertSliderRect);
          Details := StyleServices.GetElementDetails(FVUpState);
          StyleServices.DrawElement(B.Canvas.Handle, Details, ListBoxVertUpButtonRect);
          Details := StyleServices.GetElementDetails(FVDownState);
          StyleServices.DrawElement(B.Canvas.Handle, Details, ListBoxVertDownButtonRect);
        end;

        MoveWindowOrg(B.Canvas.Handle, ListBoxVertScrollRect.Left, ListBoxVertScrollRect.Top);
        Canvas.Draw(ListBoxVertScrollRect.Left, ListBoxVertScrollRect.Top,  B);
      finally
        B.Free;
      end;
    end;
  finally
    if DC <> 0 then
      Canvas.Handle := 0
    else
    begin
      ReleaseDC(FListHandle, Canvas.Handle);
      Canvas.Handle := 0;
    end;
    Canvas.Free;
  end;
end;

procedure TscComboBoxStyleHook.DrawListBoxBorder;
var
  R: TRect;
  Canvas: TCanvas;
  SaveIdx: Integer;
  P: TPoint;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetWindowDC(FListHandle);
    P := Point(0, 0);
    ClientToScreen(FListHandle, P);
    GetWindowRect(FListHandle, R);
    P.X := P.X - R.Left;
    P.Y := P.Y - R.Top;
    if (R.Width < 5000) and (R.Height < 5000) then
    begin
      GetClientRect(FListHandle, R);
      ExcludeClipRect(Canvas.Handle, P.X, P.Y, R.Right - R.Left + P.X, R.Bottom - R.Top + P.Y);
      GetWindowRect(FListHandle, R);
      OffsetRect(R, -R.Left, -R.Top);
      SaveIdx := SaveDC(Canvas.Handle);
      try
        PaintListBoxBorder(Canvas, R);
      finally
        RestoreDC(Canvas.Handle, SaveIdx);
      end;
      DrawListBoxVertScroll(Canvas.Handle);
    end;
  finally
    ReleaseDC(FListHandle, Canvas.Handle);
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TscComboBoxStyleHook.ListBoxWndProc(var Msg: TMessage);
var
  MsgHandled: Boolean;

  procedure WMNCCalcSize(var Msg: TWMNCCalcSize);
  var
    LCalcSizeParams: PNCCalcSizeParams;
    LWindowPos: PWindowPos;
    LLeft, LRight, LTop, LBottom: Integer;
    LStyle, LNewStyle: Integer;
  begin
    LStyle := GetWindowLong(FListHandle, GWL_STYLE);
    if ((LStyle and WS_VSCROLL = WS_VSCROLL) or (LStyle and WS_HSCROLL = WS_HSCROLL)) then
    begin
      LNewStyle := LStyle and not WS_VSCROLL and not WS_HSCROLL;
      FIgnoreStyleChanged := True;
      SetWindowLong(FListHandle, GWL_STYLE, LNewStyle);
      Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
        TMessage(Msg).Msg, TMessage(Msg).WParam, TMessage(Msg).LParam);
      SetWindowLong(FListHandle, GWL_STYLE, LStyle);
      FIgnoreStyleChanged := False;
    end
    else
      Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
       TMessage(Msg).Msg, TMessage(Msg).WParam, TMessage(Msg).LParam);

    if (Msg.CalcValidRects) then
    begin
      LCalcSizeParams := Msg.CalcSize_Params;
      if Control.BiDiMode <> bdRightToLeft then
      begin
        LLeft := 1;
         if LStyle and WS_VSCROLL = WS_VSCROLL then
           LRight := ListBoxVertScrollRect.Width + 1
         else
          LRight := 1;
        end
      else
      begin
        LRight := 1;
        if LStyle and WS_VSCROLL = WS_VSCROLL then
         LLeft := ListBoxVertScrollRect.Width + 1
       else
         LLeft := 1;
      end;

      LTop := 1;
      LBottom := 1;
      LWindowPos := LCalcSizeParams.lppos;
      with LCalcSizeParams^.rgrc[0] do
      begin
        left := LWindowPos^.x;
        top := LWindowPos^.y;
        right := LWindowPos^.x + LWindowPos^.cx;
        bottom := LWindowPos^.y + LWindowPos^.cy;
        left := left + LLeft;
        top := top + LTop;
        right := right - LRight;
        bottom := bottom - LBottom;
      end;
      LCalcSizeParams^.rgrc[1] := LCalcSizeParams^.rgrc[0];
      Msg.CalcSize_Params := LCalcSizeParams;
      Msg.Result := WVR_VALIDRECTS;
    end;
    Msg.Result := 0;
    MsgHandled := True;
  end;

  procedure WMMouseWheel(var Msg: TWMMouseWheel);
  var
    Index: Integer;
    R: TRect;
  begin
    SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
    Index := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
    if Msg.WheelDelta < 0 then
      Inc(Index)
    else
      Dec(Index);
    SendMessage(FListHandle, LB_SETTOPINDEX, Index, 0);
    SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
    R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
    RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
    DrawListBoxVertScroll(0);
    MsgHandled := True;
  end;

  procedure WMNCLButtonDblClk(var Msg: TWMMouse);
  var
    R: TRect;
    P: TPoint;
  begin
    P := Point(Msg.XPos, Msg.YPos);
    if ListBoxVertScrollArea.Contains(P) then
    begin
      if ListBoxVertUpButtonRect.Contains(Point(Msg.XPos, Msg.YPos)) then
      begin
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
        DrawListBoxVertScroll(0);
        Exit;
      end;

      if ListBoxVertDownButtonRect.Contains(Point(Msg.XPos, Msg.YPos)) then
      begin
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
        DrawListBoxVertScroll(0);
        Exit;
      end;
    end;
    MsgHandled := True;
  end;

  procedure WMLButtonDown(var Msg: TWMMouse);
  var
    P: TPoint;
    R: TRect;
    ItemHeight, VisibleCount, TopIndex: Integer;
  begin
    MsgHandled := False;
    P := Point(Msg.XPos, Msg.YPos);
    if Control.BiDiMode = bdRightToLeft then P.X := - P.X;
    FDownPos := P;
    FListBoxDown := True;
    if ListBoxVertScrollArea.Contains(P) then
    begin
      if Style = csSimple then
        SetCapture(FListHandle);
      FDownPos := P;
      if ListBoxVertDownButtonRect.Contains(P) then
      begin
        FListBoxDownBtnDown := True;
        FVDownState := tsArrowBtnDownPressed;
        DrawListBoxVertScroll(0);

        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);

        ListBoxSetTimer(2);
      end
      else if ListBoxVertUpButtonRect.Contains(P) then
      begin
        FListBoxUpBtnDown := True;
        FVUpState := tsArrowBtnUpPressed;
        DrawListBoxVertScroll(0);

        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);

        ListBoxSetTimer(1);
      end
      else if ListBoxVertSliderRect.Contains(P) then
      begin
        FVSliderState := tsThumbBtnVertPressed;
        FDownSliderPos := FDownPos.Y - ListBoxVertSliderRect.Top;
        DrawListBoxVertScroll(0);
      end
      else
      if ListBoxVertTrackRectUp.Contains(P) then
      begin
        ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
        if ItemHeight > 0 then
          VisibleCount := ListBoxClientRect.Height div ItemHeight
        else
          VisibleCount := 0;
        TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - VisibleCount + 1;
        if TopIndex < 0 then TopIndex := 0;
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
        DrawListBoxVertScroll(0);
        ListBoxSetTimer(3);
      end
      else if ListBoxVertTrackRectDown.Contains(P) then
      begin
        ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
        if ItemHeight > 0 then
          VisibleCount := ListBoxClientRect.Height div ItemHeight
        else
          VisibleCount := 0;
        TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + VisibleCount - 1;
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
        DrawListBoxVertScroll(0);
        ListBoxSetTimer(4);
      end;
      MsgHandled := True;
    end
    else
    begin
      if (FVSliderState <> tsThumbBtnVertNormal) or
         (FVUpState <> tsArrowBtnUpNormal) or (FVDownState <> tsArrowBtnDownNormal) then
      begin
        FVSliderState := tsArrowBtnUpNormal;
        FVUpState := tsArrowBtnUpNormal;
        FVDownState := tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
    end;
    FOldIdx := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
  end;

  procedure WMMouseMove(var Msg: TWMMouse);
  var
    P: TPoint;
    NewIndex, Index: Integer;
    Dist: Integer;
    R, LR: TRect;
  begin
    P := Point(Msg.XPos, Msg.YPos);
    if Control.BiDiMode = bdRightToLeft then
      P.X := - P.X;
    WinApi.Windows.GetWindowRect(FListHandle, LR);
    FMovePos := P;
    if (FVSliderState = tsThumbBtnVertPressed) then
    begin
      Index := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
      Dist := (ListBoxVertScrollRect.Height - ListBoxVertUpButtonRect.Height - ListBoxVertDownButtonRect.Height - ListBoxVertSliderRect.Height);
      if Dist > 0 then
      begin
        NewIndex := round((((FMovePos.y - FDownSliderPos - ListBoxVertUpButtonRect.Bottom) / Dist) * FInvsibleCount));
        if NewIndex <> Index then
        begin
          if NewIndex < 0 then NewIndex := 0;
          if NewIndex >= SendMessage(FListHandle, LB_GETCOUNT, 0, 0) then NewIndex := SendMessage(FListHandle, LB_GETCOUNT, 0, 0) - 1;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, NewIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          DrawListBoxVertScroll(0);
        end;
      end;
      MsgHandled := True;
      Exit;
    end;

    if FListBoxUpBtnDown and not ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpPressed)
    then
    begin
      FVUpState := tsArrowBtnUpNormal;
      DrawListBoxVertScroll(0);
      ListBoxStopTimer;
      Exit;
    end;

    if FListBoxUpBtnDown and ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpNormal)
    then
    begin
      FVUpState := tsArrowBtnUpPressed;
      DrawListBoxVertScroll(0);
      ListBoxSetTimer(5);
      Exit;
    end;

    if FListBoxDownBtnDown and not ListBoxVertDownButtonRect.Contains(P) and (FVDownState = tsArrowBtnDownPressed)
    then
    begin
      FVDownState := tsArrowBtnDownNormal;
      DrawListBoxVertScroll(0);
      ListBoxStopTimer;
      Exit;
    end;

    if FListBoxDownBtnDown and ListBoxVertDownButtonRect.Contains(P) and (FVDownState = tsArrowBtnDownNormal)
    then
    begin
      FVDownState := tsArrowBtnDownPressed;
      DrawListBoxVertScroll(0);
      ListBoxSetTimer(6);
      Exit;
    end;

    if ListBoxVertScrollArea.Contains(P) then
    begin
      if ListBoxVertSliderRect.Contains(P) and (FVSliderState = tsThumbBtnVertNormal) then
      begin
        FVSliderState := tsThumbBtnVertHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertSliderRect.Contains(P) and (FVSliderState = tsThumbBtnVertHot) then
      begin
        FVSliderState := tsThumbBtnVertNormal;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpNormal) then
      begin
        FVUpState := tsArrowBtnUpHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpHot) then
      begin
        FVUpState := tsArrowBtnUpNormal;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertDownButtonRect.Contains(P) and (FVDownState = tsArrowBtnDownNormal) then
      begin
        FVDownState :=  tsArrowBtnDownHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertDownButtonRect.Contains(P) and (FVDownState =  tsArrowBtnDownHot) then
      begin
        FVDownState :=  tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
      MsgHandled := True;
    end
    else
    begin
      if (FVSliderState <> tsThumbBtnVertNormal) or (FVUpState <> tsArrowBtnUpNormal) or
         (FVUpState <> tsArrowBtnDownNormal) then
      begin
        if FListBoxTimerCode <> 0 then ListBoxStopTimer;
        FVSliderState := tsThumbBtnVertNormal;
        FVUpState := tsArrowBtnUpNormal;
        FVDownState := tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
    end;
  end;

  procedure WMLButtonUp(var Msg: TWMMouse);
  var
    P: TPoint;
  begin
    FListBoxUpBtnDown := False;
    FListBoxDownBtnDown := False;
    FListBoxTrackUpDown := False;
    FListBoxTrackDownDown := False;
    FListBoxDown := False;
    P := Point(Msg.XPos, Msg.YPos);
    if Control.BiDiMode = bdRightToLeft then P.X := - P.X;

    if (Style = csSimple) and ListBoxVertScrollArea.Contains(FDownPos)
    then
      ReleaseCapture;


    if ListBoxVertSliderRect.Contains(P) then
      FVSliderState := tsThumbBtnVertHot
    else
      FVSliderState := tsThumbBtnVertNormal;

    if ListBoxVertUpButtonRect.Contains(P) then
      FVUpState := tsArrowBtnUpHot
    else
      FVUpState := tsArrowBtnUpNormal;

    if ListBoxVertDownButtonRect.Contains(P) then
      FVDownState := tsArrowBtnDownHot
    else
      FVDownState := tsArrowBtnDownNormal;

    DrawListBoxVertScroll(0);

    if FListBoxTimerCode <> 0 then
      ListBoxStopTimer;

    MsgHandled := ListBoxVertScrollArea.Contains(P);
  end;

  procedure WMNCLButtonDown(var Msg: TWMMouse);
  var
    P: TPoint;
  begin
    if Style <> csSimple then
      SetCapture(FListHandle);
    P := Point(Msg.XPos, Msg.YPos);
    ScreenToClient(FListHandle, P);
    with P do
    begin
      Msg.XPos := X;
      Msg.YPos := Y;
    end;
    WMLButtonDown(Msg);
    MsgHandled := True;
  end;

  procedure WMPrint(var Msg: TMessage);
  var
    SaveIndex: Integer;
    Canvas: TCanvas;
    R: TRect;
  begin
    Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle, Msg.Msg, Msg.WParam, Msg.LParam);

    if (Msg.LParam and PRF_NONCLIENT = PRF_NONCLIENT) and
       (Msg.wParam > 0) then
    begin
      SaveIndex := 0;
      Canvas := TCanvas.Create;
      try
        SaveIndex := SaveDC(Msg.WParam);
        Canvas.Handle := Msg.WParam;
        GetWindowRect(FListHandle, R);
        OffsetRect(R, -R.Left, -R.Top);
        ExcludeClipRect(Canvas.Handle, R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
        PaintListBoxBorder(Canvas, R);
      finally
        if SaveIndex <> 0 then
          RestoreDC(Canvas.Handle, SaveIndex);
        Canvas.Handle := 0;
        Canvas.Free;
      end;
      DrawListBoxVertScroll(Msg.wParam);
    end;
    MsgHandled := True;
  end;

  procedure WMPrint2(var Msg: TMessage);
  var
    SaveIndex: Integer;
    Canvas: TCanvas;
    R: TRect;
  begin
    Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle, Msg.Msg, Msg.WParam, Msg.LParam);

    if (Msg.LParam and PRF_NONCLIENT = PRF_NONCLIENT) and
       (Msg.wParam > 0) then
    begin
      SaveIndex := 0;
      Canvas := TCanvas.Create;
      try
        SaveIndex := SaveDC(Msg.WParam);
        Canvas.Handle := Msg.WParam;
        GetWindowRect(FListHandle, R);
        OffsetRect(R, -R.Left, -R.Top);
        ExcludeClipRect(Canvas.Handle, R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
        PaintListBoxBorder(Canvas, R);
      finally
        if SaveIndex <> 0 then
          RestoreDC(Canvas.Handle, SaveIndex);
        Canvas.Handle := 0;
        Canvas.Free;
      end;
    end;
    MsgHandled := True;
  end;

  procedure WMTimer(var Msg: TMessage);
  var
    R: TRect;
    ItemHeight, VisibleCount, TopIndex: Integer;
  begin
    case FListBoxTimerCode of
      1: ListBoxSetTimer(5);
      2: ListBoxSetTimer(6);
      3: ListBoxSetTimer(7);
      4: ListBoxSetTimer(8);
      5:
        begin
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX,
            SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - 1, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          DrawListBoxVertScroll(0);
        end;
      6:
        begin
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX,
            SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + 1, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          DrawListBoxVertScroll(0);
        end;
      7:
        begin
          if ListBoxVertSliderRect.Contains(FMovePos) or (FMovePos.Y > ListBoxVertSliderRect.Bottom) then
          begin
            ListBoxStopTimer;
            Exit;
          end;
          ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
          if ItemHeight > 0 then
            VisibleCount := ListBoxClientRect.Height div ItemHeight
          else
            VisibleCount := 0;
          TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - VisibleCount + 1;
          if TopIndex < 0 then TopIndex := 0;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          DrawListBoxVertScroll(0);
        end;
      8:
        begin
          if ListBoxVertSliderRect.Contains(FMovePos) or (FMovePos.Y < ListBoxVertSliderRect.Top) then
          begin
            ListBoxStopTimer;
            Exit;
          end;
          ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
          if ItemHeight > 0 then
            VisibleCount := ListBoxClientRect.Height div ItemHeight
          else
            VisibleCount := 0;
          TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + VisibleCount - 1;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          DrawListBoxVertScroll(0);
        end;
    end;
  end;

begin
  MsgHandled := False;
  if ListBoxVertScrollArea.Height = 0 then
  begin
    case Msg.Msg of
      WM_PRINT:
        WMPrint2(Msg);
      WM_NCCALCSIZE:
        WMNCCalcSize(TWMNCCalcSize(Msg));
      WM_NCPAINT:
         begin
           DrawListBoxBorder;
           MsgHandled := True;
         end;
    end;
  end
  else
    case Msg.Msg of
      WM_NCHITTEST:
        if Style = csSimple then
        begin
          Msg.Result := HTCLIENT;
          MsgHandled := True;
        end;
      WM_MOUSELEAVE, WM_NCMOUSELEAVE:
        if Style = csSimple then
        begin
          FVSliderState := tsThumbBtnVertNormal;
          FVUpState := tsArrowBtnUpNormal;
          FVDownState := tsArrowBtnDownNormal;
          DrawListBoxVertScroll(0);
        end;
      
      WM_TIMER: WMTimer(Msg);
      WM_UpdateUIState: MsgHandled := True;
      WM_NCCALCSIZE: WMNCCalcSize(TWMNCCalcSize(Msg));
      WM_MOUSEWHEEL: WMMouseWheel(TWMMouseWheel(Msg));
      WM_NCLButtonDblClk: WMNCLButtonDblClk(TWMMouse(Msg));
      WM_LButtonDown: WMLButtonDown(TWMMouse(Msg));
      WM_MOUSEMOVE: WMMouseMove(TWMMouse(Msg));
      WM_LBUTTONUP: WMLButtonUp(TWMMouse(Msg));
      WM_NCLButtonDown: WMNCLButtonDown(TWMMouse(Msg));
      WM_NCLButtonUp, WM_NCMouseMove: MsgHandled := True;
      WM_PRINT:
        WMPrint(Msg);
      WM_KEYDOWN, WM_KEYUP:
      if Self.DroppedDown then
        begin
          if IsWindowsXP then
            SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
            Msg.Msg, Msg.WParam, Msg.LParam);
          if IsWindowsXP then
          begin
            SendMessage(FListHandle, WM_SETREDRAW, 1, 0);  
            RedrawWindow(FListHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          end;
          DrawListBoxVertScroll(0);
          MsgHandled := True;
        end;
      WM_NCPAINT:
       begin
         DrawListBoxBorder;
         DrawListBoxVertScroll(0);
         MsgHandled := True;
       end;
      LB_SETTOPINDEX:
      if Self.DroppedDown then
        begin
          if IsWindowsXP then
            SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
            Msg.Msg, Msg.WParam, Msg.LParam);
          if IsWindowsXP then
          begin
            SendMessage(FListHandle, WM_SETREDRAW, 1, 0);  
            RedrawWindow(FListHandle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          end;  
          DrawListBoxVertScroll(0);
          MsgHandled := True;
        end;
      WM_STYLECHANGED, WM_STYLECHANGING:
       if FIgnoreStyleChanged then
       begin
         Msg.Result := 0;
         MsgHandled := True;
       end;
    end;
  if not MsgHandled then
    Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
      Msg.Msg, Msg.WParam, Msg.LParam);

  if IsWindowsXP and FListBoxDown then
  case Msg.Msg of
    WM_ERASEBKGND, LB_SETCURSEL, LB_SETSEL:
    begin
      DrawListBoxVertScroll(0);
    end;
  end;    
end;

constructor TscGalleryMenuItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FCaption := '';
  FHint := '';
  FButton := False;
  FHeader := False;
  FEnabled := True;
  FShowColor := False;
end;

procedure TscGalleryMenuItem.Assign(Source: TPersistent);
begin
  if Source is TscGalleryMenuItem then
  begin
    FImageIndex := TscGalleryMenuItem(Source).ImageIndex;
    FCaption := TscGalleryMenuItem(Source).Caption;
    FButton := TscGalleryMenuItem(Source).Button;
    FHeader := TscGalleryMenuItem(Source).Header;
    Hint := TscGalleryMenuItem(Source).Hint;
  end
  else
    inherited Assign(Source);
end;

procedure TscGalleryMenuItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

procedure TscGalleryMenuItem.SetCaption(const Value: String);
begin
  FCaption := Value;
end;

constructor TscGalleryMenuItems.Create;
begin
  inherited Create(TscGalleryMenuItem);
  GalleryMenu := AGalleryMenu;
end;

function TscGalleryMenuItems.GetOwner: TPersistent;
begin
  Result := GalleryMenu;
end;

function TscGalleryMenuItems.GetItem(Index: Integer): TscGalleryMenuItem;
begin
  Result := TscGalleryMenuItem(inherited GetItem(Index));
end;

procedure TscGalleryMenuItems.SetItem(Index: Integer; Value:  TscGalleryMenuItem);
begin
  inherited SetItem(Index, Value);
end;

constructor TscGalleryMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaleFactor := 1;
  FItemFont := TFont.Create;
  FHeaderFont := TFont.Create;
  FHeaderHeight := 25;
  FHintComponent := nil;
  FShowHints := True;
  FItems := TscGalleryMenuItems.Create(Self);
  FItemIndex := -1;
  FColumnsCount := 1;
  FOnItemClick := nil;
  FPopupWindow := nil;
  FShowSelectedItem := True;
  FButtonLeftAlignment := False;
  FButtonGlyphLeftAlignment := False;
end;

function TscGalleryMenu.GetSelectedItem;
begin
  if (ItemIndex >=0) and (ItemIndex < FItems.Count)
  then
    Result := FItems[ItemIndex]
  else
    Result := nil;
end;

destructor TscGalleryMenu.Destroy;
begin
  if FPopupWindow <> nil then FPopupWindow.Free;
  FItems.Free;
  FItemFont.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

procedure TscGalleryMenu.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TscGalleryMenu.SetItemIndex(Value: Integer);
var
  FOldValue: Integer;
begin
  if FItemIndex <> Value then
  begin
    FOldValue := FItemIndex;
    FItemIndex := Value;
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count)
       and (FItems[FItemIndex].Header)
    then
      FItemIndex := FOldValue
    else
    if (FItemIndex > FItems.Count - 1)
    then
      FItemIndex := FOldValue;
    if (FItemIndex <> FOldValue) and not (csLoading in ComponentState) then
    begin
      if Assigned(FOnInternalChange) then FOnInternalChange(Self);
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

procedure TscGalleryMenu.SetItemFont;
begin
  FItemFont.Assign(Value);
end;

procedure TscGalleryMenu.SetColumnsCount(Value: Integer);
begin
  if (Value > 0) and (Value < 51) then
    FColumnsCount := Value;
end;

procedure TscGalleryMenu.SetItems(Value: TscGalleryMenuItems);
begin
  FItems.Assign(Value);
end;

procedure TscGalleryMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  if (Operation = opRemove) and (AComponent = FHintComponent) then
    FHintComponent := nil;
end;

procedure TscGalleryMenu.SetImages(Value: TCustomImageList);
begin
  FImages := Value;
end;

procedure TscGalleryMenu.Popup(X, Y: Integer);
begin
  if FPopupWindow <> nil then FPopupWindow.Hide(False);
  if Assigned(FOnMenuPopup) then FOnMenuPopup(Self);
  if (FImages = nil) or (FImages.Count = 0) then Exit;
  FOldItemIndex := ItemIndex;
  FPopupWindow := TscGalleryMenuPopupWindow.Create(Self);
  FPopupWindow.FScaleFactor := Self.FScaleFactor;
  FPopupWindow.Show(Rect(X, Y, X, Y));
end;

procedure TscGalleryMenu.PopupExt(X, Y: Integer; AFromTop, AFromRight: Boolean);
begin
  if FPopupWindow <> nil then FPopupWindow.Hide(False);
  if Assigned(FOnMenuPopup) then FOnMenuPopup(Self);
  if (FImages = nil) or (FImages.Count = 0) then Exit;
  FOldItemIndex := ItemIndex;
  FPopupWindow := TscGalleryMenuPopupWindow.Create(Self);
  FPopupWindow.FScaleFactor := Self.FScaleFactor;
  FPopupWindow.ShowExt(Rect(X, Y, X, Y), AFromTop, AFromRight);
end;

procedure TscGalleryMenu.PopupFromRectExt(R: TRect; AFromTop, AFromRight: Boolean);
begin
  if FPopupWindow <> nil then FPopupWindow.Hide(False);
  if Assigned(FOnMenuPopup) then FOnMenuPopup(Self);
  if (FImages = nil) or (FImages.Count = 0) then Exit;
  FOldItemIndex := ItemIndex;
  FPopupWindow := TscGalleryMenuPopupWindow.Create(Self);
  FPopupWindow.FScaleFactor := Self.FScaleFactor;
  FPopupWindow.ShowExt(R, AFromTop, AFromRight);
end;

procedure TscGalleryMenu.PopupFromRect(R: TRect);
begin
  if FPopupWindow <> nil then FPopupWindow.Hide(False);
  if Assigned(FOnMenuPopup) then FOnMenuPopup(Self);
  if (FImages = nil) or (FImages.Count = 0) then Exit;
  FOldItemIndex := ItemIndex;
  FPopupWindow := TscGalleryMenuPopupWindow.Create(Self);
  FPopupWindow.FScaleFactor := Self.FScaleFactor;
  FPopupWindow.Show(R);
end;

procedure TscGalleryMenu.ProcessEvents;
begin
  if FPopupWindow = nil then Exit;
  FPopupWindow.Free;
  FPopupWindow := nil;

  if Assigned(FOnInternalMenuClose) then
    FOnInternalMenuClose(Self);

  if Assigned(FOnMenuClose) then
    FOnMenuClose(Self);

  if ACanProcess and (ItemIndex <> -1) then
  begin
    if Assigned(FItems[ItemIndex].OnClick) then
       FItems[ItemIndex].OnClick(Self);
    if Assigned(FOnItemClick) then FOnItemClick(Self);
    if (FOldItemIndex <> ItemIndex) and
      Assigned(FOnInternalChange) then FOnInternalChange(Self);
    if (FOldItemIndex <> ItemIndex) and
       Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TscGalleryMenu.Hide;
begin
  if FPopupWindow <> nil then FPopupWindow.Hide(False);
end;

constructor TscGalleryMenuPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaleFactor := 1;
  DrawOnBackground := False;
  Visible := False;
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  GalleryMenu := TscGalleryMenu(AOwner);
  case GalleryMenu.BackgroundStyle of
    scgmToolBar: StyleKind := scpsToolBar;
    scgmFormBackground: StyleKind := scpsFormBackground;
  end;
  BorderStyle := scpbsFlat;
  MouseInItem := -1;
  OldMouseInItem := -1;
  FDown := False;
  FItemDown := False;
end;

destructor TscGalleryMenuPopupWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TscGalleryMenuPopupWindow.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
  SelectedIndex: Integer;
begin
  inherited;
  R := Rect(5, 5, Width - 5, Height - 5);
  if GalleryMenu.ShowSelectedItem then
    SelectedIndex := GalleryMenu.ItemIndex
  else
    SelectedIndex := -1;
  if GalleryMenu.BackgroundStyle = scgmToolBar then
  begin
    DrawSingleBorder(ACanvas, Rect(0, 0, Width, Height));
  end;
  DrawItems(MouseInItem, SelectedIndex, ACanvas);
end;

procedure TscGalleryMenuPopupWindow.FindUp;
var
  I: Integer;
begin
  if Self.MouseInItem = -1 then
  begin
    MouseInItem := GalleryMenu.Items.Count  - 1;
    RePaintControl;
    Exit;
  end;
  for I := MouseInItem - 1 downto 0 do
  if not GalleryMenu.Items[I].Header and GalleryMenu.Items[I].Enabled
  then
   begin
   if  (GalleryMenu.Items[I].ItemRect.Top <
        GalleryMenu.Items[MouseInItem].ItemRect.Top) and
       (GalleryMenu.Items[I].Button or
       (GalleryMenu.Items[I].ItemRect.Left =
        GalleryMenu.Items[MouseInItem].ItemRect.Left))
    then
      begin
        MouseInItem := I;
        RePaintControl;
        Break;
      end;
  end;
end;

procedure TscGalleryMenuPopupWindow.FindDown;
var
  I: Integer;
begin
  if Self.MouseInItem = -1 then
  begin
    MouseInItem := 0;
    if (GalleryMenu.Items[MouseInItem].Header)
    then
      Inc(MouseInItem);
    RePaintControl;
    Exit;
  end;

  for I := MouseInItem + 1 to GalleryMenu.Items.Count - 1 do
    if not GalleryMenu.Items[I].Header and GalleryMenu.Items[I].Enabled then
    begin
      if (GalleryMenu.Items[I].ItemRect.Top >
          GalleryMenu.Items[MouseInItem].ItemRect.Top) and
         (GalleryMenu.Items[I].Button or
         (GalleryMenu.Items[I].ItemRect.Left =
         GalleryMenu.Items[MouseInItem].ItemRect.Left)) then
      begin
        MouseInItem := I;
        RePaintControl;
        Break;
      end;
    end;
end;

procedure TscGalleryMenuPopupWindow.FindRight;
var
  I: Integer;
begin
  if Self.MouseInItem = -1 then
  begin
    MouseInItem := 0;
    if (GalleryMenu.Items[MouseInItem].Header)
    then
      Inc(MouseInItem);
    RePaintControl;
    Exit;
  end
  else
  if MouseInItem = GalleryMenu.Items.Count  - 1
  then
    begin
      MouseInItem := 0;
      if (GalleryMenu.Items[MouseInItem].Header)
      then
        Inc(MouseInItem);
      RePaintControl;
      Exit;
    end;
  for I := MouseInItem + 1 to GalleryMenu.Items.Count - 1 do
  if not GalleryMenu.Items[I].Header and GalleryMenu.Items[I].Enabled
  then
  begin
    MouseInItem := I;
    RePaintControl;
    Break;
  end;
end;

procedure TscGalleryMenuPopupWindow.FindLeft;
var
  I: Integer;
begin
  if Self.MouseInItem = -1 then
  begin
    MouseInItem := GalleryMenu.Items.Count  - 1;
    RePaintControl;
    Exit;
  end
  else
  if (MouseInItem = 0) or ((GalleryMenu.Items[0].Header) and
     (GalleryMenu.Items.Count > 1) and (MouseInItem = 1))
  then
    begin
      MouseInItem := GalleryMenu.Items.Count  - 1;
      RePaintControl;
      Exit;
    end;
  for I := MouseInItem - 1 downto 0 do
  if not GalleryMenu.Items[I].Header and GalleryMenu.Items[I].Enabled
  then
  begin
    MouseInItem := I;
    RePaintControl;
    Break;
  end;
end;

procedure TscGalleryMenuPopupWindow.ProcessKey(KeyCode: Integer);
begin
  case KeyCode of
   VK_ESCAPE: Self.Hide(False);
   VK_RETURN, VK_SPACE:
    begin
      if MouseInItem <> -1
      then
        GalleryMenu.ItemIndex := MouseInItem;
      Self.Hide(True);
    end;
    VK_RIGHT: FindRight;
    VK_LEFT: FindLeft;
    VK_UP: FindUp;
    VK_DOWN: FindDown;
  end;
end;

procedure TscGalleryMenuPopupWindow.WMEraseBkGrnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

function TscGalleryMenuPopupWindow.GetItemFromPoint;
var
  I: Integer;
  R: TRect;
begin
  Result := -1;
  for I := 0 to GalleryMenu.Items.Count - 1 do
  if not GalleryMenu.Items[I].Header and GalleryMenu.Items[I].Enabled then
  begin
    R := GetItemRect(I);
    if PointInRect(R, P)
    then
      begin
        Result := i;
        Break;
      end;
  end;
end;

procedure TscGalleryMenuPopupWindow.AssignItemRects;
var
  I, W, X, Y,StartX, StartY: Integer;
  ItemWidth, ItemHeight: Integer;
begin
  ItemWidth := GalleryMenu.FImages.Width + Round(10 * FScaleFactor);
  ItemHeight := GalleryMenu.FImages.Height + Round(10 * FScaleFactor);
  W := ItemWidth * GalleryMenu.ColumnsCount;
  StartX := 2;
  StartY := 2;
  X := StartX;
  Y := StartY;
  for I := 0 to GalleryMenu.Items.Count - 1 do
  with GalleryMenu.Items[I] do
  begin
   if Header
   then
      begin
        if X <> StartX then Inc(Y, ItemHeight + 1);
        ItemRect := Rect(StartX, Y, StartX + W, Y + Round(GalleryMenu.HeaderHeight * FScaleFactor));
        Inc(Y, Round(GalleryMenu.HeaderHeight * FScaleFactor) + 1);
        X := StartX;
      end
    else
    if Button
    then
      begin
        if X <> StartX then Inc(Y, ItemHeight + 1);
        ItemRect := Rect(StartX, Y, StartX + W, Y + ItemHeight);
        Inc(Y, ItemHeight + 1);
        X := StartX;
      end
    else
      begin
        ItemRect := Rect(X, Y, X + ItemWidth, Y + ItemHeight);
        X := X + ItemWidth;
        if X + ItemWidth > StartX + W
        then
          begin
            X := StartX;
            Inc(Y, ItemHeight + 1);
          end;
      end;
  end;
end;

function TscGalleryMenuPopupWindow.GetItemRect(Index: Integer): TRect;
begin
  Result := GalleryMenu.Items[Index].ItemRect;
end;

procedure TscGalleryMenuPopupWindow.TestActive(X, Y: Integer);
begin
  MouseInItem := GetItemFromPoint(Point(X, Y));
  if (GalleryMenu <> nil) and (GalleryMenu.HintComponent <> nil) and (MouseInItem = -1) then
    GalleryMenu.HintComponent.HideHint;
  if (MouseInItem <> OldMouseInItem)
  then
    begin
      OldMouseInItem := MouseInItem;
      RePaintControl;
      if (GalleryMenu <> nil) and GalleryMenu.ShowHints and 
         (MouseInItem <> -1) and (GalleryMenu.HintComponent <> nil)
      then
        begin
          GalleryMenu.HintComponent.HideHint;
          with GalleryMenu.Items[MouseInItem] do
          begin
            if Hint <> '' then
              GalleryMenu.HintComponent.ActivateHint(Hint);
          end;
        end;
    end;
end;

procedure TscGalleryMenuPopupWindow.ShowExt(PopupRect: TRect; AFromTop, AFromRight: Boolean);

procedure CorrectMenuPos(var X, Y: Integer);
var
  WorkArea: TRect;
begin
  if (Screen.ActiveCustomForm <> nil)
  then
    WorkArea := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).WorkAreaRect
  else
  if (Application.MainForm <> nil) and (Application.MainForm.Visible)
  then
    WorkArea := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkAreaRect
  else
    WorkArea := Screen.WorkAreaRect;

  if AFromTop then
  begin
    Y := PopupRect.Top - Height;
    if Y < WorkArea.Top then
      Y := PopupRect.Bottom;
  end;

  if AFromRight then
  begin
    X := PopupRect.Right - Width;
    if X < WorkArea.Left then
      X := PopupRect.Left;
  end;

  if Y + Height > WorkArea.Bottom
  then
    Y := PopupRect.Top - Height;

  if X + Width > WorkArea.Right
  then
    X := X - ((X + Width) - WorkArea.Right);

  if X < WorkArea.Left then
    X := WorkArea.Left;

  if Y < WorkArea.Top then
    Y := WorkArea.Top;
end;

var
  ShowX, ShowY: Integer;
begin
  CreateMenu;
  ShowX := PopupRect.Left;
  ShowY := PopupRect.Bottom;
  CorrectMenuPos(ShowX, ShowY);
  SetWindowPos(Handle, HWND_TOPMOST, ShowX, ShowY, 0, 0,
               SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
  RePaintControl;
  HookApp;
  SetCapture(Handle);
end;

procedure TscGalleryMenuPopupWindow.Show(PopupRect: TRect);

procedure CorrectMenuPos(var X, Y: Integer);
var
  WorkArea: TRect;
begin
  if (Screen.ActiveCustomForm <> nil)
  then
    WorkArea := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).WorkAreaRect
  else
  if (Application.MainForm <> nil) and (Application.MainForm.Visible)
  then
    WorkArea := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkAreaRect
  else
    WorkArea := Screen.WorkAreaRect;


  if X + Width > WorkArea.Right
  then
    X := X - ((X + Width) - WorkArea.Right);
  if X < WorkArea.Left then X := WorkArea.Left;
  if Y < WorkArea.Top then Y := WorkArea.Top;
end;

var
  ShowX, ShowY: Integer;
begin
  CreateMenu;
  ShowX := PopupRect.Left;
  ShowY := PopupRect.Bottom;
  CorrectMenuPos(ShowX, ShowY);
  SetWindowPos(Handle, HWND_TOPMOST, ShowX, ShowY, 0, 0,
               SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
  RePaintControl;
  HookApp;
  SetCapture(Handle);
end;

procedure TscGalleryMenuPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
  if GalleryMenu.ShowHints and (GalleryMenu.HintComponent <> nil) then
    GalleryMenu.HintComponent.HideHint;
  UnHookApp;
  if GetCapture = Handle then ReleaseCapture;
  GalleryMenu.ProcessEvents(AProcessEvents);
end;

procedure TscGalleryMenuPopupWindow.DrawItem(Index: Integer; Cnvs: TCanvas;
  AActive, ASelected: Boolean);
var
  R: TRect;
  ButtonState: TscsCtrlState;
  IX, IY: Integer;
begin
  R := GalleryMenu.Items[Index].ItemRect;
  if ASelected then
    ButtonState := scsPressed
  else
  if AActive then
    ButtonState := scsHot
  else
    ButtonState := scsNormal;
  case ButtonState of
    scsHot, scsPressed:
      DrawToolButton(Cnvs, R, ButtonState);
  end;
  if GalleryMenu.Images <> nil  then
  begin
    IX := R.Left + R.Width div 2 - GalleryMenu.Images.Width div 2;
    IY := R.Top + R.Height div 2 - GalleryMenu.Images.Height div 2;
    if not GalleryMenu.Items[Index].Enabled and not GalleryMenu.Items[Index].FShowColor then
    begin
      if GalleryMenu.Images is TscCustomImageList then
        GalleryMenu.Images.Draw(Cnvs, IX, IY, GalleryMenu.Items[Index].ImageIndex, False)
      else
        scDrawUtils.DrawBitmapFromImageList(Cnvs, IX, IY,
          GalleryMenu.Images, GalleryMenu.Items[Index].ImageIndex, 100)
    end
    else
      GalleryMenu.Images.Draw(Cnvs, IX, IY,
        GalleryMenu.Items[Index].ImageIndex, True);
  end;
end;

procedure TscGalleryMenuPopupWindow.DrawButtonItem
  (Index: Integer; Cnvs: TCanvas; AActive, ADown: Boolean);
var
  R: TRect;
  R1: TRect;
  C: TColor;
  ButtonState: TscsCtrlState;
  IX, IY: Integer;
begin
  R := GalleryMenu.Items[Index].ItemRect;
  if not GalleryMenu.Items[Index].Enabled then
    ButtonState := scsDisabled
  else
  if ADown then
    ButtonState := scsPressed
  else
  if AActive then
    ButtonState := scsHot
  else
    ButtonState := scsNormal;
  C := GetStyleColor(clBtnText);
  case ButtonState of
    scsHot, scsPressed, scsDisabled:
      begin
        C := GetToolButtonTextColor(ButtonState);
        DrawToolButton(Cnvs, R, ButtonState);
      end;
  end;
  Cnvs.Font.Assign(GalleryMenu.ItemFont);
  Cnvs.Font.Height := Round(Cnvs.Font.Height * FScaleFactor);
  Cnvs.Font.Color := C;
  SetBkMode(Cnvs.Handle, TRANSPARENT);
  InflateRect(R, -2, -2);
  if GalleryMenu.ButtonGlyphLeftAlignment and not GalleryMenu.ButtonLeftAlignment then
  begin
    if (GalleryMenu.Images <> nil) and (GalleryMenu.Items[Index].ImageIndex >= 0) and
       (GalleryMenu.Items[Index].ImageIndex < GalleryMenu.Images.Count)
    then
    begin
      R1 := GalleryMenu.Items[Index].ItemRect;
      R1.Right := R1.Left + R1.Height;
      IX := R1.Left + R1.Width div 2 - GalleryMenu.Images.Width div 2;
      IY := R1.Top + R1.Height div 2 - GalleryMenu.Images.Height div 2;
      GalleryMenu.Images.Draw(Cnvs, IX, IY,
        GalleryMenu.Items[Index].ImageIndex, True);
    end;
    scDrawUtils.DrawTextAlignmentNoPrefix(Cnvs,
       GalleryMenu.Items[Index].Caption, R, taCenter, False);
  end
  else
  if GalleryMenu.ButtonLeftAlignment then
    DrawImageAndText (Cnvs, R, 5, 5,
      blGlyphLeft, GalleryMenu.Items[Index].Caption, GalleryMenu.Items[Index].ImageIndex,
      GalleryMenu.Images, GalleryMenu.Items[Index].Enabled, False, clBlack, False, False, True, FScaleFactor)
  else
    DrawImageAndText(Cnvs, R, -1, 5,
      blGlyphLeft, GalleryMenu.Items[Index].Caption, GalleryMenu.Items[Index].ImageIndex,
      GalleryMenu.Images, GalleryMenu.Items[Index].Enabled, False, clBlack, False, False, True, FScaleFactor);
end;

procedure TscGalleryMenuPopupWindow.DrawHeaderItem(Index: Integer; Cnvs: TCanvas);
var
  R, R1: TRect;
  C: TColor;
  FOldPen: HPEN;
begin
  R := GalleryMenu.Items[Index].ItemRect;
  if GalleryMenu.HeaderStyle = scmhsDefault then
  begin
    DrawHeaderSection(Cnvs, GalleryMenu.Items[Index].ItemRect, scsNormal);
    Cnvs.Font.Assign(GalleryMenu.HeaderFont);
    Cnvs.Font.Height := Round(Cnvs.Font.Height * FScaleFactor);
    Cnvs.Font.Color := GetHeaderTextColor(scsNormal);
  end
  else
  begin
    Cnvs.Font.Assign(GalleryMenu.HeaderFont);
    Cnvs.Font.Height := Round(Cnvs.Font.Height * FScaleFactor);
    if IsCustomStyle then
    begin
      C := GetStyleColor(clBtnText);
      C := MiddleColor(C, GetStyleColor(clBtnFace));
      Cnvs.Font.Color := C;
    end
    else
      C := MiddleColor(clBtnText, clBtnFace);
    FOldPen := SelectObject(Cnvs.Handle, GetStockObject(DC_PEN));
    SetDCPenColor(Cnvs.Handle, ColorToRGB(C));
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 1);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 1);
    Cnvs.MoveTo(R.Left + 2, R.Bottom - 2);
    Cnvs.LineTo(R.Right - 2, R.Bottom - 2);
    SelectObject(Cnvs.Handle, FOldPen);
  end;
  Inc(R.Left, 5);
  Dec(R.Right, 5);
  SetBkMode(Cnvs.Handle, TRANSPARENT);
  if GalleryMenu.HeaderStyle = scmhsDefault then
    DrawText(Cnvs.Handle, PChar(GalleryMenu.Items[Index].Caption),
      Length(GalleryMenu.Items[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft))
  else
    begin
      R1 := Rect(0, 0, 0, 0);
      DrawText(Cnvs.Handle, PChar(GalleryMenu.Items[Index].Caption), Length(GalleryMenu.Items[Index].Caption), R1,
       DT_LEFT or DT_SINGLELINE or DT_CALCRECT);
      R.Top := R.Bottom - R1.Height - 6;
      DrawText(Cnvs.Handle, PChar(GalleryMenu.Items[Index].Caption), Length(GalleryMenu.Items[Index].Caption), R,
       scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft));
    end;
end;

procedure TscGalleryMenuPopupWindow.DrawItems(ActiveIndex, SelectedIndex: Integer; C: TCanvas);
var
  I: Integer;
  R: TRect;
  isActive, isDown: Boolean;
  SaveIndex: Integer;
begin
  for I := 0 to GalleryMenu.Items.Count - 1 do
  begin
    R := GetItemRect(I);
    SaveIndex := SaveDC(C.Handle);
    if GalleryMenu.Items[I].Header then
    begin
      DrawHeaderItem(I, C);
    end
    else
    begin
      isDown := False;
      isActive := False;
      if (I = SelectedIndex) and not FItemDown then
        isDown := True
      else
      if I = ActiveIndex then
      begin
        if FItemDown then isDown := True else isActive := True;
      end;
      if GalleryMenu.Items[I].Button then
        DrawButtonItem(I, C, isActive, isDown)
      else
        DrawItem(I, C, isActive, isDown);
    end;
    RestoreDC(C.Handle, SaveIndex);
  end;
end;

procedure TscGalleryMenuPopupWindow.CreateMenu;
var
  ItemsWidth, ItemsHeight: Integer;
  ItemsR: TRect;
begin
  AssignItemRects;
  ItemsWidth := (GalleryMenu.Images.Width + Round(10 * FScaleFactor)) *
    GalleryMenu.ColumnsCount;
  ItemsR := Rect(GalleryMenu.Items[0].ItemRect.Left,
                 GalleryMenu.Items[0].ItemRect.Top,
                 GalleryMenu.Items[0].ItemRect.Left + ItemsWidth,
                 GalleryMenu.Items[GalleryMenu.Items.Count - 1].ItemRect.Bottom);
  ItemsHeight := RectHeight(ItemsR);
  Width := ItemsWidth + 4;
  Height := ItemsHeight + 4;
end;

procedure TscGalleryMenuPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
  end;
end;

procedure TscGalleryMenuPopupWindow.CMMouseLeave(var Message: TMessage);
begin
  MouseInItem := -1;
  OldMouseInItem := -1;
  RePaintControl;
end;

procedure TscGalleryMenuPopupWindow.CMMouseEnter(var Message: TMessage);
begin
end;

procedure TscGalleryMenuPopupWindow.MouseDown(Button: TMouseButton; Shift: TShiftState;
   X, Y: Integer);
begin
  inherited;
  FDown := True;
  if GalleryMenu.ShowHints and (GalleryMenu.HintComponent <> nil) then
     GalleryMenu.HintComponent.HideHint;
  if GetItemFromPoint(Point(X, Y)) <> -1 then
    FItemDown := True
  else
    FItemDown := False;
  RePaintControl;
end;

procedure TscGalleryMenuPopupWindow.MouseUp(Button: TMouseButton; Shift: TShiftState;
   X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if not FDown then
  begin
    if GetCapture = Handle then ReleaseCapture;
    SetCapture(Handle);
  end
  else
  begin
    I := GetItemFromPoint(Point(X, Y));
    if I <> -1 then GalleryMenu.ItemIndex := I;
    if I <> -1 then
      Hide(I <> -1)
    else
    begin
      if GetCapture = Handle then ReleaseCapture;
      SetCapture(Handle);
    end;
  end;
end;

procedure TscGalleryMenuPopupWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TestActive(X, Y);
end;

procedure TscGalleryMenuPopupWindow.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscGalleryMenuPopupWindow.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
    begin
      if WinApi.Windows.WindowFromPoint(Mouse.CursorPos) <> Self.Handle then
         Hide(False);
    end;
  end;
end;

procedure TscGalleryMenuPopupWindow.HookApp;
begin
  OldAppMessage := Application.OnMessage;
  Application.OnMessage := NewAppMessage;
end;

procedure TscGalleryMenuPopupWindow.UnHookApp;
begin
  Application.OnMessage := OldAppMessage;
end;

procedure TscGalleryMenuPopupWindow.NewAppMessage;
begin
  if (Msg.message = WM_KEYDOWN) then
  begin
    ProcessKey(Msg.wParam);
    Msg.message := 0;
  end
  else
  if (Msg.message = WM_MOUSEWHEEL) then
  begin
    Msg.message := 0;
  end
  else
  case Msg.message of
    WM_MOUSEACTIVATE, WM_ACTIVATE,
    WM_RBUTTONDOWN, WM_MBUTTONDOWN,
    WM_NCLBUTTONDOWN, WM_NCMBUTTONDOWN, WM_NCRBUTTONDOWN,
    WM_KILLFOCUS, WM_MOVE, WM_SIZE, WM_CANCELMODE, WM_PARENTNOTIFY,
    WM_SYSKEYDOWN, WM_SYSCHAR:
      Hide(False);
  end;
end;

constructor TscUpDownStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaint := True;
  DoubleBuffered := True;
end;

procedure TscUpDownStyleHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  FCanvas: TCanvas;
begin
  FCanvas := TCanvas.Create;
  FCanvas.Handle := Message.DC;
  try
    Paint(FCanvas);
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
  Handled := True;
end;

function TscUpDownStyleHook.GetOrientation: TUDOrientation;
begin
  if (Control <> nil) and (Control is TUpDown) then
    Result := TUpDown(Control).Orientation
  else if Control.HandleAllocated and (Handle <> 0) then
  begin
    if GetWindowLong(Handle, GWL_STYLE) and UDS_HORZ = UDS_HORZ then
      Result := udHorizontal
    else
      Result := udVertical;
  end
  else
    Result := udVertical;
end;

procedure TscUpDownStyleHook.PaintBackground(Canvas: TCanvas);
begin
  Paint(Canvas);
end;

procedure TscUpDownStyleHook.Paint(Canvas: TCanvas);
var
  R: TRect;
  DrawState: TThemedScrollBar;
  Details: TThemedElementDetails;
begin
  if not Control.HandleAllocated then Exit;
  Canvas.Brush.Color := GetStyleColor(clBtnFace);
  Canvas.FillRect(Rect(0, 0, Control.Width, Control.Height));
  if GetOrientation = udHorizontal then
  begin
    R := Control.ClientRect;
    R.Right := R.Left + R.Width div 2;
    if FLeftPressed then
      DrawState := tsArrowBtnLeftPressed
    else if FMouseOnLeft and MouseInControl then
      DrawState := tsArrowBtnLeftHot
    else
      DrawState := tsArrowBtnLeftNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);

    R := Control.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if FRightPressed then
      DrawState := tsArrowBtnRightPressed
    else if FMouseOnRight and MouseInControl then
      DrawState :=  tsArrowBtnRightHot
    else
      DrawState :=  tsArrowBtnRightNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end
  else
  begin
    R := Control.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if FLeftPressed then
      DrawState := tsArrowBtnUpPressed
    else if FMouseOnLeft and MouseInControl then
      DrawState := tsArrowBtnUpHot
    else
      DrawState := tsArrowBtnUpNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);

    R := Control.ClientRect;
    R.Top := R.Bottom - R.Height div 2;

    if FRightPressed then
      DrawState := tsArrowBtnDownPressed
    else if FMouseOnRight and MouseInControl  then
      DrawState := tsArrowBtnDownHot
    else
      DrawState := tsArrowBtnDownNormal;

    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
end;

procedure TscUpDownStyleHook.WMLButtonDblClk(var Message: TWMMouse);
var
  R: TRect;
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  if GetOrientation = udHorizontal then
  begin
    R := Control.ClientRect;
    R.Right := R.Left +  R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := Control.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end
  else
  begin
    R := Control.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := Control.ClientRect;
    R.Top := R.Bottom - R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end;
  Invalidate;
  Handled := True;
end;

procedure TscUpDownStyleHook.WMLButtonDown(var Message: TWMMouse);
var
  R: TRect;
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);

  if GetOrientation = udHorizontal then
  begin
    R := Control.ClientRect;
    R.Right := R.Left + R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := Control.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end
  else
  begin
    R := Control.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FLeftPressed := True
    else
      FLeftPressed := False;

    R := Control.ClientRect;
    R.Top := R.Bottom - R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FRightPressed := True
    else
      FRightPressed := False;
  end;

  Invalidate;
  Handled := True;
end;

procedure TscUpDownStyleHook.WMLButtonUp(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);

  FLeftPressed := False;
  FRightPressed := False;
  Invalidate;

  Handled := True;
end;

procedure TscUpDownStyleHook.WMMouseMove(var Message: TWMMouse);
var
  R: TRect;
  FOldMouseOnLeft, FOldMouseOnRight: Boolean;
begin
  inherited;
  CallDefaultProc(TMessage(Message));

  FOldMouseOnLeft := FMouseOnLeft;
  FOldMouseOnRight := FMouseOnRight;

  if GetOrientation = udHorizontal then
  begin
    R := Control.ClientRect;
    R.Right := R.Left + R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnLeft := True
    else
      FMouseOnLeft := False;

    R := Control.ClientRect;
    R.Left := R.Right - R.Width div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnRight := True
    else
      FMouseOnRight := False;
  end
  else
  begin
    R := Control.ClientRect;
    R.Bottom := R.Top + R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnLeft := True
    else
      FMouseOnLeft := False;

    R := Control.ClientRect;
    R.Top := R.Bottom - R.Height div 2;
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FMouseOnRight := True
    else
      FMouseOnRight := False;
  end;

  if (FOldMouseOnLeft <> FMouseOnLeft) and (FOldMouseOnRight <> FMouseOnRight) then
    Invalidate;

  Handled := True;
end;

procedure TscUpDownStyleHook.MouseLeave;
begin
  FMouseOnLeft := False;
  FMouseOnRight := False;
  Invalidate;
end;


constructor TscTrackBarThumbOptions.Create;
begin
  FStyleKind := sctsStyled;
  FStyleColors := True;
  FWidth := 0;
  FHeight := 0;
  FNormalColor := clHighLight;
  FHotColor := clHighLight;
  FPressedColor := clHighLight;;
  FDisabledColor := clGray;
  FCustomImageNormalIndex := -1;
  FCustomImageHotIndex := -1;
  FCustomImagePressedIndex := -1;
  FCustomImageDisabledIndex := -1;
  FCursor := crDefault;
  FUseCursor := False;
end;

procedure TscTrackBarThumbOptions.Assign(Source: TPersistent);
begin
  if Source is TscTrackBarThumbOptions then
  begin
    FStyleKind := TscTrackBarThumbOptions(Source).FStyleKind;
    FStyleColors := TscTrackBarThumbOptions(Source).FStyleColors;
    FNormalColor := TscTrackBarThumbOptions(Source).FNormalColor;
    FHotColor := TscTrackBarThumbOptions(Source).FHotColor;
    FPressedColor := TscTrackBarThumbOptions(Source).FPressedColor;
    FDisabledColor := TscTrackBarThumbOptions(Source).FDisabledColor;
    FCustomImageNormalIndex := TscTrackBarThumbOptions(Source).FCustomImageNormalIndex;
    FCustomImageHotIndex := TscTrackBarThumbOptions(Source).FCustomImageHotIndex;
    FCustomImagePressedIndex := TscTrackBarThumbOptions(Source).FCustomImagePressedIndex;
    FCustomImageDisabledIndex := TscTrackBarThumbOptions(Source).FCustomImageDisabledIndex;
    FCursor := TscTrackBarThumbOptions(Source).FCursor;
    FUseCursor := TscTrackBarThumbOptions(Source).FUseCursor;
  end
  else
    inherited Assign(Source);
end;

function TscTrackBarThumbOptions.GetNormalColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FNormalColor)
  else
    Result := FNormalColor;
end;

function TscTrackBarThumbOptions.GetHotColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FHotColor)
  else
    Result := FHotColor;
end;

function TscTrackBarThumbOptions.GetPressedColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FPressedColor)
  else
    Result := FPressedColor;
end;

function TscTrackBarThumbOptions.GetDisabledColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FDisabledColor)
  else
    Result := FDisabledColor;
end;

procedure TscTrackBarThumbOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (FWidth >= 0) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetHeight(Value: Integer);
begin
  if (FHeight <> Value) and (FHeight >= 0) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetStyleKind(Value: TscTrackStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetNormalColor(Value: TColor);
begin
  if FNormalColor <> Value then
  begin
    FNormalColor := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetHotColor(Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetDisabledColor(Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetPressedColor(Value: TColor);
begin
  if FPressedColor <> Value then
  begin
    FPressedColor := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetCustomImageNormalIndex(Value: Integer);
begin
  if FCustomImageNormalIndex <> Value then
  begin
    FCustomImageNormalIndex := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetCustomImageHotIndex(Value: Integer);
begin
  if FCustomImageHotIndex <> Value then
  begin
    FCustomImageHotIndex := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetCustomImagePressedIndex(Value: Integer);
begin
  if FCustomImagePressedIndex <> Value then
  begin
    FCustomImagePressedIndex := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.SetCustomImageDisabledIndex(Value: Integer);
begin
  if FCustomImageDisabledIndex <> Value then
  begin
    FCustomImageDisabledIndex := Value;
    Changed;
  end;
end;

procedure TscTrackBarThumbOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;


constructor TscTrackBarTrackOptions.Create;
begin
  FSize := 10;
  FOffset := 5;
  FStyleKind := sctsStyled;
  FColor := clBtnShadow;
  FProgressColor := clHighLight;
  FStyleColors := True;
  FFrameColor := clNone;
  FFrameWidth := 1;
  FCustomImageIndex := -1;
  FCustomImageProgressIndex := -1;
end;

procedure TscTrackBarTrackOptions.Assign(Source: TPersistent);
begin
  if Source is TscTrackBarTrackOptions then
  begin
    FSize := TscTrackBarTrackOptions(Source).FSize;
    FOffset := TscTrackBarTrackOptions(Source).FOffset;
    FStyleKind := TscTrackBarTrackOptions(Source).FStyleKind;
    FColor := TscTrackBarTrackOptions(Source).FColor;
    FProgressColor := TscTrackBarTrackOptions(Source).FProgressColor;
    FFrameColor := TscTrackBarTrackOptions(Source).FFrameColor;
    FFrameWidth := TscTrackBarTrackOptions(Source).FFrameWidth;
    FCustomImageIndex := TscTrackBarTrackOptions(Source).FCustomImageIndex;
    FCustomImageProgressIndex := TscTrackBarTrackOptions(Source).FCustomImageProgressIndex;
    FStyleColors := TscTrackBarTrackOptions(Source).FStyleColors;
  end
  else
    inherited Assign(Source);
end;

function TscTrackBarTrackOptions.GetColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FColor)
  else
    Result := FColor;
end;

function TscTrackBarTrackOptions.GetProgressColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FProgressColor)
  else
    Result := FProgressColor;
end;

function TscTrackBarTrackOptions.GetFrameColor: TColor;
begin
  if FStyleColors and IsCustomStyle then
    Result := GetStyleColor(FFrameColor)
  else
    Result := FFrameColor;
end;

procedure TscTrackBarTrackOptions.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetOffset(Value: Integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetStyleKind(Value: TscTrackStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetProgressColor(Value: TColor);
begin
  if FProgressColor <> Value then
  begin
    FProgressColor := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetStyleColors(Value: Boolean);
begin
  if FStyleColors <> Value then
  begin
    FStyleColors := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetFrameWidth(Value: Integer);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetCustomImageIndex(Value: Integer);
begin
  if FCustomImageIndex <> Value then
  begin
    FCustomImageIndex := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.SetCustomImageProgressIndex(Value: Integer);
begin
  if FCustomImageProgressIndex <> Value then
  begin
    FCustomImageProgressIndex := Value;
    Changed;
  end;
end;

procedure TscTrackBarTrackOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TscCustomTrackBar.Create;
begin
  inherited;
  FReadOnly := False;
  FTrackOptions := TscTrackBarTrackOptions.Create;
  FTrackOptions.OnChange := OnTrackOptionsChange;
  FThumbOptions := TscTrackBarThumbOptions.Create;
  FThumbOptions.OnChange := OnTrackOptionsChange;
  FMouseWheelSupport := True;
  FMouseWheelOpposite := False;
  FJumpWhenClick := False;
  FCanFocused := True;
  FShowFocusRect := True;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 50;
  FVertical := False;
  Width := 100;
  Height := 20;
  FMouseSupport := True;
  FDown := False;
  FMouseOver := False;
  FCustomImages := nil;
  FCustomBackgroundImageIndex := -1;
  FSaveCursor := crDefault;
end;

destructor TscCustomTrackBar.Destroy;
begin
  FTrackOptions.Free;
  FThumbOptions.Free;
  inherited;
end;

procedure TscCustomTrackBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  TrackOptions.Size := MulDiv(TrackOptions.Size, M, D);
  ThumbOptions.Width := MulDiv(ThumbOptions.Width, M, D);
  ThumbOptions.Height := MulDiv(ThumbOptions.Height, M, D);
end;

procedure TscCustomTrackBar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscCustomTrackBar.OnTrackOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscCustomTrackBar.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscCustomTrackBar.WndProc(var Message: TMessage);
begin
  if FCanFocused and not FReadOnly then
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

procedure TscCustomTrackBar.KeyDown;
begin
  inherited KeyDown(Key, Shift);
  if FCanFocused and not FReadOnly then
  case Key of
    VK_UP, VK_RIGHT:
      begin
        Value := Value + 1;
        if Assigned(FOnLastChange) then FOnLastChange(Self);
      end;
    VK_DOWN, VK_LEFT:
     begin
       Value := Value - 1;
       if Assigned(FOnLastChange) then FOnLastChange(Self);
     end;
  end;
end;

procedure TscCustomTrackBar.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseOver and not FReadOnly then
  begin
    FMouseOver := False;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
  then
    FCustomImages.Draw(ACanvas, Rect(0, 0, Width, Height), FCustomBackgroundImageIndex, FScaleFactor);
end;

procedure TscCustomTrackBar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  PR, R, R1: TRect;
  SaveIndex, IIndex: Integer;
  C: TColor;
  Offset: Integer;
begin
  TrackR := Rect(0, 0, Width, Height);
  InflateRect(TrackR, -2, -2);
  if IsCustomStyle and SC_SCALESTYLES and (FScaleFactor >= 1.25) and
     (FTrackOptions.StyleKind = sctsStyled)
  then
  begin
   if FTrackOptions.Offset > 0 then
      Offset := Round(FTrackOptions.Offset * FScaleFactor)
    else
      Offset := Round(5 * FScaleFactor);
    if FVertical then
    begin
      TrackR.Top := Offset;
      TrackR.Bottom := Height - Offset;
    end
    else
    begin
      TrackR.Left := Offset;
      TrackR.Right := Width - Offset;
    end;
  end;
  // track
  if FTrackOptions.Offset > 0 then
    if FVertical then
    begin
      Inc(TrackR.Top, FTrackOptions.Offset);
      Dec(TrackR.Bottom, FTrackOptions.Offset);
    end
    else
    begin
      Inc(TrackR.Left, FTrackOptions.Offset);
      Dec(TrackR.Right, FTrackOptions.Offset);
    end;
  if FTrackOptions.Size <> 0 then 
  begin
    if FVertical then
    begin
      TrackR.Left := TrackR.Left + TrackR.Width div 2 - FTrackOptions.Size div 2;
      TrackR.Right := TrackR.Left + FTrackOptions.Size;
    end
    else
    begin
      TrackR.Top := TrackR.Top + TrackR.Height div 2 - FTrackOptions.Size div 2;
      TrackR.Bottom := TrackR.Top + FTrackOptions.Size;
    end;
  end;
  // thumb
  ThumbR := CalcThumbRect(TrackR);
  if (FTrackOptions.StyleKind = sctsColor) and
     (FThumbOptions.StyleKind = sctsColor) and
     (FTrackOptions.FrameColor <> clNone)
  then
  begin
    if FVertical then
    begin
      Inc(ThumbR.Top, FTrackOptions.FrameWidth);
      Dec(ThumbR.Bottom, FTrackOptions.FrameWidth);
    end
    else
    begin
      Inc(ThumbR.Left, FTrackOptions.FrameWidth);
      Dec(ThumbR.Right, FTrackOptions.FrameWidth);
    end;
  end;
  //
  PR := TrackR;
  if FVertical then
    PR.Top := ThumbR.Top + ThumbR.Height div 2
  else
    PR.Right := ThumbR.Left + ThumbR.Width div 2;
  case FTrackOptions.StyleKind of
    sctsStyled:
    begin
      R1 := TrackR;
      if IsCustomStyle and (ThumbOptions.StyleKind = sctsStyled) then
        if FVertical then
        begin
          Inc(R1.Top, ThumbR.Height div 2);
          Dec(R1.Bottom, ThumbR.Height div 2);
        end
        else
        begin
          Inc(R1.Left, ThumbR.Width div 2);
          Dec(R1.Right, ThumbR.Width div 2);
        end;
      DrawTrackBarTrack(ACanvas, R1, FVertical);
    end;
    sctsColor:
      begin
        if FTrackOptions.Color <> clNone then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := TrackOptions.GetColor;
          ACanvas.FillRect(TrackR);
        end;
        if FTrackOptions.ProgressColor <> clNone then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := FTrackOptions.GetProgressColor;
          ACanvas.FillRect(PR);
        end;
        if FTrackOptions.FrameColor <> clNone then
        begin
          ACanvas.Pen.Color := FTrackOptions.FrameColor;
          Frame3D(ACanvas, TrackR, FTrackOptions.FrameColor,
            FTrackOptions.FrameColor,
            FTrackOptions.FrameWidth);
        end;
      end;
    sctsCustomImage:
      if FCustomImages <> nil then
      begin
        if FCustomImages.IsIndexAvailable(FTrackOptions.FCustomImageIndex)
        then
          FCustomImages.Draw(ACanvas, TrackR, FTrackOptions.FCustomImageIndex, FScaleFactor);
        if FCustomImages.IsIndexAvailable(FTrackOptions.FCustomImageProgressIndex)
        then
        begin
          SaveIndex := SaveDC(ACanvas.Handle);
          try
            R := TrackR;
            if FVertical then
              R.Top := PR.Top
            else
              R.Right := PR.Right;
            IntersectClipRect(ACanvas.Handle,
              R.Left, R.Top, R.Right, R.Bottom);
            FCustomImages.Draw(ACanvas, TrackR, FTrackOptions.FCustomImageProgressIndex);
          finally
            RestoreDC(ACanvas.Handle, SaveIndex);
          end;
        end;
      end;
  end;
  case FThumbOptions.StyleKind of
    sctsStyled:
    begin
      if not Enabled then
        DrawTrackBarThumb(ACanvas, ThumbR, FVertical, scsDisabled, FScaleFactor)
      else
      if FDown then
        DrawTrackBarThumb(ACanvas, ThumbR, FVertical, scsPressed, FScaleFactor)
      else
      if FMouseOver then
        DrawTrackBarThumb(ACanvas, ThumbR, FVertical, scsHot, FScaleFactor)
      else
        DrawTrackBarThumb(ACanvas, ThumbR, FVertical, scsNormal, FScaleFactor);
    end;
    sctsColor:
    begin
      if not Enabled then
        C := FThumbOptions.GetDisabledColor
      else
      if FDown then
        C := FThumbOptions.GetPressedColor
      else
      if FMouseOver then
        C := FThumbOptions.GetHotColor
      else
        C := FThumbOptions.GetNormalColor;
       ACanvas.Brush.Style := bsSolid;
       ACanvas.Brush.Color := C;
       ACanvas.FillRect(ThumbR);
     end;
    sctsCustomImage:
    if FCustomImages <> nil then
    begin
      if not Enabled then
        IIndex := FThumbOptions.FCustomImageDisabledIndex
      else
      if FDown then
        IIndex := FThumbOptions.FCustomImagePressedIndex
      else
      if FMouseOver then
        IIndex := FThumbOptions.FCustomImageHotIndex
      else
        IIndex := FThumbOptions.FCustomImageNormalIndex;
      if FCustomImages.IsIndexAvailable(IIndex)
      then
        FCustomImages.Draw(ACanvas, ThumbR, IIndex);
    end;
  end;
  if IsFocused and FShowFocusRect then
  begin
    ACanvas.Font.Color := GetCheckBoxTextColor(scsNormal);
    scDrawFocusRect(ACanvas, Rect(0, 0, Width, Height), FScaleFactor);
  end;
end;

procedure TscCustomTrackBar.WMMOUSEWHEEL;
var
  IncDirection: Boolean;
begin
  if FReadOnly or not FMouseWheelSupport then
  begin
    inherited;
    Exit;
  end;

  if not FMouseWheelOpposite then
    IncDirection := (TWMMOUSEWHEEL(Message).WheelDelta > 0)
  else
    IncDirection := (TWMMOUSEWHEEL(Message).WheelDelta < 0);

  if Vertical then
  begin
    if IncDirection then
      Value := Value + 1
    else
      Value := Value - 1;
  end
  else
  begin
    if IncDirection then
      Value := Value - 1
    else
      Value := Value + 1;
  end;
  if Assigned(FOnLastChange) then FOnLastChange(Self)
end;

procedure TscCustomTrackBar.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if FCanFocused and not FReadOnly then
    case Msg.CharCode of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: Msg.Result := 1;
    end;
end;

function TscCustomTrackBar.IsFocused;
begin
  Result := Focused and FCanFocused;
end;

procedure TscCustomTrackBar.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.SetCanFocused;
begin
  if FCanFocused <> Value then
  begin
    FCanFocused := Value;
    if FCanFocused then TabStop := True else TabStop := False;
  end;
end;

procedure TscCustomTrackBar.WMSETFOCUS;
begin
  inherited;
  if FCanFocused and not FReadOnly then
  begin
    FUpdateParentBuffer := True;
    if DrawTextMode = scdtmGDIPlus then
      Invalidate
    else
      RePaint;
  end;
end;

procedure TscCustomTrackBar.WMKILLFOCUS;
begin
  inherited;
  if FCanFocused and not FReadOnly then
    RePaint;
end;

function TscCustomTrackBar.CalcValue;
var
  kf: Double;
begin
  if (Offset2 - Offset1) <= 0
  then kf := 0
  else kf := AOffset / (Offset2 - Offset1);
  if kf > 1 then kf := 1 else
  if kf < 0 then kf := 0;
  Result := FMinValue + Round((FMaxValue - FMinValue) * kf);
end;

function TscCustomTrackBar.CalcThumbRect;
var
  kf: Double;
  BW, BH: Integer;
  DefDraw: Boolean;
begin
  if FMinValue = FMaxValue
  then
    Kf := 0
  else
    kf := (FValue - FMinValue) / (FMaxValue - FMinValue);
  DefDraw := False;
  if (FThumbOptions.StyleKind = sctsCustomImage) and
     (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FThumbOptions.CustomImageNormalIndex)
  then
  begin
    BW := FCustomImages.GetWidth(FThumbOptions.CustomImageNormalIndex, FScaleFactor);
    BH := FCustomImages.GetHeight(FThumbOptions.CustomImageNormalIndex, FScaleFactor);
  end
  else
  begin
    if (FThumbOptions.Width * FThumbOptions.Height > 0) then
    begin
      BW := FThumbOptions.Width;
      BH := FThumbOptions.Height;
    end
    else
    if FVertical then
    begin
      BW := Width - 4;
      BH := BW div 2;
      DefDraw := ThumbOptions.FStyleKind = sctsStyled;
    end
    else
    begin
      BH := Height - 4;
      BW := BH div 2;
      DefDraw := ThumbOptions.FStyleKind = sctsStyled;
    end;
  end;
  if FVertical then
  begin
    Offset1 := R.Top + BH div 2;
    Offset2 := R.Bottom - BH div 2;
    if ThumbOptions.StyleKind = sctsStyled  then
    begin
      Dec(Offset1);
      Inc(Offset2);
    end;
    BOffset := Round((Offset2 - Offset1) * Kf);
    Result := Rect(R.Left + R.Width div 2 - BW div 2,
      Offset2 - BOffset - BH div 2,
      R.Left + R.Width div 2 - BW div 2 + BW,
      Offset2 - BOffset - BH div 2 + BH);
    if DefDraw and StyleServices.Enabled and not Odd(Width) then
      Inc(Result.Right);
  end
  else
  begin
    Offset1 := R.Left + BW div 2;
    Offset2 := R.Right - BW div 2;
    if ThumbOptions.StyleKind = sctsStyled  then
    begin
      Dec(Offset1);
      Inc(Offset2);
    end;
    BOffset := Round((Offset2 - Offset1) * kf);
    Result := Rect(Offset1 + BOffset - BW div 2,
      R.Top + R.Height div 2 - BH div 2,
      Offset1 + BOffset - BW div 2 + BW,
      R.Top + R.Height div 2 - BH div 2 + BH);
    if DefDraw and StyleServices.Enabled and not Odd(Height) then
      Inc(Result.Bottom);
  end;
end;


procedure TscCustomTrackBar.MouseDown;
begin
  inherited;
  if FReadOnly then Exit;
  
  if FMouseSupport and
      PtInRect(Rect(ThumbR.Left, ThumbR.Top, ThumbR.Right, ThumbR.Bottom), Point(X, Y))
  then
    begin
      if FVertical then OMPos := Y else OMPos := X;
      OldBOffset := BOffset;
      FDown := True;
      RePaintControl;
      if Assigned(FOnStartDragButton) then FOnStartDragButton(Self);
    end;
end;

procedure TscCustomTrackBar.MouseUp;
var
  Off: Integer;
  Off2: Integer;
begin
  inherited;
  if FReadOnly then Exit;

  if FMouseSupport and FDown
  then
    begin
      FDown := False;
      FMouseOver := PtInRect(Rect(ThumbR.Left, ThumbR.Top, ThumbR.Right, ThumbR.Bottom), Point(X, Y));
      RePaintControl;
      if FThumbOptions.UseCursor and not FMouseOver then
        Cursor := FSaveCursor;
      if Assigned(FOnLastChange) then FOnLastChange(Self);
    end
  else
  if FMouseSupport and not FDown and FJumpWhenClick then
  begin
    Off2 := 2;
    if FVertical then
      Off := Height - Y - RectHeight(ThumbR) div 2 - Off2
    else
      Off := X - RectWidth(ThumbR) div 2 - Off2;
    Value := CalcValue(Off);
  end;
end;

procedure TscCustomTrackBar.MouseMove;
var
  Off: Integer;
begin
  if FReadOnly then Exit;

  if FMouseSupport and FDown then
  begin
    if Vertical then
    begin
      Off := OMPos - Y;
      Off := OldBOffset + Off;
    end
    else
    begin
      Off := X - OMPos;
      Off := OldBOffset + Off;
    end;
    Value := CalcValue(Off);
  end
  else
  if FMouseSupport then
  begin
    if PtInRect(Rect(ThumbR.Left, ThumbR.Top, ThumbR.Right, ThumbR.Bottom), Point(X, Y))
       and not FMouseOver then
    begin
      FMouseOver := True;
      RePaintControl;
      if FThumbOptions.UseCursor then
      begin
        FSaveCursor := Cursor;
        Cursor := FThumbOptions.Cursor;
      end;
    end
    else
    if not PtInRect(Rect(ThumbR.Left, ThumbR.Top, ThumbR.Right, ThumbR.Bottom), Point(X, Y))
       and FMouseOver then
    begin
      FMouseOver := False;
      RePaintControl;
      if FThumbOptions.UseCursor then
        Cursor := FSaveCursor;
    end;
  end;
  inherited;
end;

procedure TscCustomTrackBar.SetVertical;
begin
  if FVertical <> AValue then
  begin
    FVertical := AValue;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscCustomTrackBar.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
    Change;
  end;
end;

// TscProgressBar

constructor TscProgressBar.Create;
begin
  inherited;
  FOptions := TscTrackBarTrackOptions.Create;
  FOptions.Size := 0;
  FOptions.Offset := 0;
  FOptions.OnChange := OnProgressOptionsChange;
  FAnimationTimerInterval := 50;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  FVertical := False;
  Width := 100;
  Height := 20;
  FCustomImages := nil;
  FCustomBackgroundImageIndex := -1;
  FAnimationTimer := nil;
  FAnimationRect := Rect(0, 0, 0, 0);
end;

destructor TscProgressBar.Destroy;
begin
  if FAnimationTimer <> nil then
  begin
    FAnimationTimer.Free;
    FAnimationTimer := nil;
  end;
  FOptions.Free;
  inherited;
end;

procedure TscProgressBar.ProcessAnimation;
var
  R, MR: TRect;
  FSize, FOffset: Integer;
begin
  if FVertical then
    FSize := FrameR.Height div 3
  else
    FSize := FrameR.Width div 3;

  MR := Rect(0, 0, 0, 0);
  if (FCustomImages <> nil) and (FOptions.StyleKind = sctsCustomImage) and
     FCustomImages.IsIndexAvailable(FOptions.FCustomImageProgressIndex) then
  MR := FCustomImages.GetMargins(FOptions.FCustomImageProgressIndex, FScaleFactor);

  if (FCustomImages <> nil) and (FOptions.StyleKind = sctsCustomImage) and
     FCustomImages.IsIndexAvailable(FOptions.FCustomImageProgressIndex) and
     (MR.Left = 0) and (MR.Top = 0) and (MR.Right = 0) and (MR.Bottom = 0) then
   if FVertical then
     FSize := FCustomImages.GetHeight(FOptions.FCustomImageProgressIndex, FScaleFactor)
   else
     FSize := FCustomImages.GetWidth(FOptions.FCustomImageProgressIndex, FScaleFactor);

  if FAnimationRect.Width * FAnimationRect.Height = 0 then
  begin
    FAnimationRect := FrameR;
    if FVertical then
    begin
      FAnimationRect.Top := FrameR.Bottom - FSize;
      R := FAnimationRect;
      OffsetRect(FAnimationRect, 0, R.Height);
    end
    else
    begin
      FAnimationRect.Right := FrameR.Left + FSize;
      R := FAnimationRect;
      OffsetRect(FAnimationRect, - R.Width, 0);
    end;
  end
  else
  begin
    if FVertical then
      OffsetRect(FAnimationRect, 0, -FrameR.Height div 20)
    else
      OffsetRect(FAnimationRect, FrameR.Width div 20, 0);
  end;
  if FVertical then
  begin
   if FAnimationRect.Bottom < FrameR.Top then
     FAnimationRect := Rect(0, 0, 0, 0);
  end
  else
  begin
    if (FCustomImages <> nil) and (FOptions.StyleKind = sctsCustomImage) and
        FCustomImages.IsIndexAvailable(FOptions.FCustomImageIndex)
    then
      begin
        MR := FCustomImages.GetContentMargins(FOptions.FCustomImageIndex, FScaleFactor);
        if FVertical then
          FOffset := MR.Top
        else
          FOffset := MR.Right;
       end
     else
       FOffset := 0;
    if FVertical then
    begin
      if FAnimationRect.Bottom < FrameR.Top + FOffset then
        FAnimationRect := Rect(0, 0, 0, 0);
    end
    else
      if FAnimationRect.Left > FrameR.Right - FOffset then
         FAnimationRect := Rect(0, 0, 0, 0);
  end;
  RePaintControl;
end;

procedure TscProgressBar.OnAnimationTimer(Sender: TObject);
begin
  ProcessAnimation;
end;

procedure TscProgressBar.StartAnimation;
begin
  if FAnimationTimer <> nil then FAnimationTimer.Free;
  FAnimationRect := Rect(0, 0, 0, 0);
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimer;
  FAnimationTimer.Interval := FAnimationTimerInterval;
  FAnimationTimer.Enabled := True;
end;

procedure TscProgressBar.StopAnimation;
begin
  if FAnimationTimer <> nil then
  begin
    FAnimationTimer.Free;
    FAnimationTimer := nil;
  end;
  RePaintControl;
end;

procedure TscProgressBar.OnProgressOptionsChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscProgressBar.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscProgressBar.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscProgressBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscProgressBar.DrawBackground(ACanvas: TCanvas);
begin
  if FTransparentBackground then
    inherited
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := GetStyleColor(Color);
    ACanvas.FillRect(Rect(0, 0, Width, Height));
  end;
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
  then
    FCustomImages.Draw(ACanvas, Rect(0, 0, Width, Height), FCustomBackgroundImageIndex, FScaleFactor);
end;

function TscProgressBar.CalcProgressRect(R: TRect): TRect;
var
  kf: Double;
  Offset: Integer;
begin
   if FMinValue = FMaxValue
  then
    Kf := 0
  else
    kf := (FValue - FMinValue) / (FMaxValue - FMinValue);
  if FVertical
  then
    begin
      Offset := Trunc(R.Height * kf);
      R.Top := R.Bottom - Offset;
      Result := R;
    end
  else
    begin
      Offset := Trunc(R.Width * kf);
      R.Right := R.Left + Offset;
      Result := R;
    end;
end;

procedure TscProgressBar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  PR, R, MR: TRect;
begin
  FrameR := Rect(0, 0, Width, Height);
  if FOptions.Offset > 0 then
    if FVertical then
    begin
      Inc(FrameR.Top, FOptions.Offset);
      Dec(FrameR.Bottom, FOptions.Offset);
    end
    else
    begin
      Inc(FrameR.Left, FOptions.Offset);
      Dec(FrameR.Right, FOptions.Offset);
    end;

  if FOptions.Size <> 0 then
  begin
    if FVertical then
    begin
      FrameR.Left := FrameR.Left + FrameR.Width div 2 - FOptions.Size div 2;
      FrameR.Right := FrameR.Left + FOptions.Size;
    end
    else
    begin
      FrameR.Top := FrameR.Top + FrameR.Height div 2 - FOptions.Size div 2;
      FrameR.Bottom := FrameR.Top + FOptions.Size;
    end;
  end;

  if (FAnimationTimer <> nil) then
  begin
    PR := FAnimationRect;
    if FVertical then
    begin
      if PR.Bottom > FrameR.Bottom then PR.Bottom := FrameR.Bottom;
      if PR.Top < FrameR.Top then PR.Top := FrameR.Top;
      PR.Left := FrameR.Left;
      PR.Right := FrameR.Right;
    end
    else
    begin
      if PR.Left < FrameR.Left then PR.Left := FrameR.Left;
      if PR.Right > FrameR.Right then PR.Right := FrameR.Right;
      PR.Top := FrameR.Top;
      PR.Bottom := FrameR.Bottom;
    end;
  end
  else
    PR := CalcProgressRect(FrameR);

  case FOptions.StyleKind of
    sctsStyled:
    begin
      DrawProgressBarBorder(ACanvas, FrameR, FVertical);
      InflateRect(PR, -1, -1);
      if PR.Width * PR.Height > 0 then
        DrawProgressBarChunk(ACanvas, PR, FVertical);
    end;
    sctsColor:
      begin
        if FOptions.Color <> clNone then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := FOptions.GetColor;
          ACanvas.FillRect(FrameR);
        end;
        if FOptions.ProgressColor <> clNone then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := FOptions.GetProgressColor;
          ACanvas.FillRect(PR);
        end;
        if FOptions.FrameColor <> clNone then
        begin
          ACanvas.Pen.Color := FOptions.GetFrameColor;
          R := FrameR;
          Frame3D(ACanvas, R, FOptions.GetFrameColor,
            FOptions.GetFrameColor,
            FOptions.FrameWidth);
        end;
      end;
    sctsCustomImage:
      if FCustomImages <> nil then
      begin
        if FCustomImages.IsIndexAvailable(FOptions.FCustomImageIndex)
        then
          FCustomImages.Draw(ACanvas, FrameR, FOptions.FCustomImageIndex, FScaleFactor);
        if  (PR.Width * PR.Height > 0) and FCustomImages.IsIndexAvailable(FOptions.FCustomImageIndex) then
        begin
          MR := FCustomImages.GetMargins(FOptions.FCustomImageProgressIndex, FScaleFactor);
          if (FAnimationTimer <> nil) and
             (MR.Left = 0) and (MR.Top = 0) and (MR.Bottom = 0) and (MR.Right = 0)
          then
            PR := FAnimationRect;
          FCustomImages.Draw(ACanvas, PR, FOptions.FCustomImageProgressIndex, FScaleFactor);
        end;
      end;
  end;
end;

procedure TscProgressBar.SetTransparentBackground(Value: Boolean);
begin
  if FTransparentBackground <> Value then
  begin
    FTransparentBackground := Value;
    GetParentBG;
    RePaintControl;
  end;
end;

procedure TscProgressBar.SetVertical;
begin
  if FVertical <> AValue then
  begin
    FVertical := AValue;
    RePaintControl;
  end;
end;

procedure TscProgressBar.SetMinValue;
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then FValue := FMinValue;
    RePaintControl;
  end;
end;

procedure TscProgressBar.SetMaxValue;
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then FValue := FMaxValue;
    RePaintControl;
  end;
end;

procedure TscProgressBar.SetValue;
begin
  if AValue > MaxValue then AValue := MaxValue else
    if AValue < MinValue then AValue := MinValue;
  if AValue <> FValue then
  begin
    FValue := AValue;
    RePaintControl;
  end;
end;

constructor TscScrollBarStyleHook.TscScrollWindow.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
end;

procedure TscScrollBarStyleHook.TscScrollWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
end;

procedure TscScrollBarStyleHook.TscScrollWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if not FSizeBox then
     Message.Result := HTTRANSPARENT;
end;

procedure TscScrollBarStyleHook.TscScrollWindow.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TscScrollBarStyleHook.TscScrollWindow.PaintToDC(DC: HDC);
var
  Canvas: TCanvas;
begin
  if FStyleHook <> nil then
  begin
    if FVertical then
      FStyleHook.VertDrawScroll(DC)
    else
      FStyleHook.HorzDrawScroll(DC);
  end
  else if FSizeBox then
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      if StyleServices.Enabled then
         with Canvas do
         begin
           Brush.Style := bsSolid;
           Brush.Color := StyleServices.GetSystemColor(clBtnFace);
           FillRect(Rect(0, 0, Width, Height));
         end;
     finally
       Canvas.Handle := 0;
       Canvas.Free;
     end;
   end;
end;

procedure TscScrollBarStyleHook.TscScrollWindow.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
  Canvas: TCanvas;
begin
  BeginPaint(Handle, PS);
  try
    if FStyleHook <> nil then
    begin
      DC := GetWindowDC(Handle);
      try
        if FVertical then
          FStyleHook.VertDrawScroll(DC)
        else
          FStyleHook.HorzDrawScroll(DC);
      finally
        ReleaseDC(Handle, DC);
      end;
    end
    else if FSizeBox then
    begin
      Canvas := TCanvas.Create;
      try
        Canvas.Handle := GetWindowDC(Handle);
        if StyleServices.Enabled then
          with Canvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := StyleServices.ColorToRGB(clBtnFace);
            FillRect(Rect(0, 0, Width, Height));
          end;
      finally
        ReleaseDC(Handle, Canvas.Handle);
        Canvas.Handle := 0;
        Canvas.Free;
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

procedure TscScrollBarStyleHook.TscScrollWindow.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TscScrollBarStyleHook }

constructor TscScrollBarStyleHook.Create;
begin
  inherited;
  FScrollWnd := nil;
  FVSliderState := tsThumbBtnVertNormal;
  FVUpState := tsArrowBtnUpNormal;
  FVDownState := tsArrowBtnDownNormal;
  FHSliderState := tsThumbBtnHorzNormal;
  FHUpState := tsArrowBtnLeftNormal;
  FHDownState := tsArrowBtnRightNormal;
end;

destructor TscScrollBarStyleHook.Destroy;
begin
  if FScrollWnd <> nil then
  begin
    FScrollWnd.StyleHook := nil;
    FreeAndNil(FScrollWnd);
  end;
  inherited;
end;

{$IFNDEF VER230}
function TscScrollBarStyleHook.AcceptMessage(var Message: TMessage): Boolean;
begin
  Result := seClient in Control.StyleElements;
end;
{$ENDIF}

function TscScrollBarStyleHook.HasBorder: Boolean;
begin
  Result := False;
end;

function TscScrollBarStyleHook.ControlBounds: TRect;
var
  R: TRect;
begin
  R := Control.BoundsRect;
  if HasBorder then InflateRect(R, 1, 1);
  Result := R;
end;

procedure TscScrollBarStyleHook.CMVisibleChanged(var Message: TMessage);
begin
  if (FScrollWnd <> nil) and FScrollWnd.HandleAllocated then
  begin
    if Control.Visible then
      ShowWindow(FScrollWnd.Handle, SW_SHOW)
    else
      ShowWindow(FScrollWnd.Handle, SW_HIDE);
  end;
end;

procedure TscScrollBarStyleHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if (FScrollWnd <> nil) and FScrollWnd.HandleAllocated then
    PaintScrollBar;
end;

procedure TscScrollBarStyleHook.WMPaint(var Message: TMessage);
begin
  if (FScrollWnd <> nil) and FScrollWnd.HandleAllocated then
    PaintScrollBar;
end;

procedure TscScrollBarStyleHook.WMShowWindow(var Message: TWMShowWindow);
begin
  CallDefaultProc(TMessage(Message));
  if (FScrollWnd <> nil) and FScrollWnd.HandleAllocated then
  begin
    if Message.Show then
      ShowWindow(FScrollWnd.Handle, SW_SHOW)
    else
      ShowWindow(FScrollWnd.Handle, SW_HIDE);
  end;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WMWindowPosChanged;
begin
  CallDefaultProc(TMessage(Message));
  UpdateScrollBar;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WMMove(var Message: TMessage);
begin
  CallDefaultProc(TMessage(Message));
  UpdateScrollBar;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WMSize(var Message: TMessage);
begin
  CallDefaultProc(TMessage(Message));
  UpdateScrollBar;
  Handled := True;
end;

procedure TscScrollBarStyleHook.InitScrollBar;
var
  R: TRect;
  P: HWnd;
begin
  P := GetParent(Control.Handle);
  FScrollWnd := TscScrollWindow.CreateParented(P);
  FScrollWnd.StyleHook := Self;
  FScrollWnd.Vertical := not Horizontal;
  FScrollWnd.SizeBox := False;
  R := ControlBounds;
  SetWindowPos(FScrollWnd.Handle, HWND_TOP, R.Left, R.Top,
    R.Width, R.Height, SWP_NOREDRAW);
  if Control.Visible then
    ShowWindow(FScrollWnd.Handle, SW_SHOW);
end;

procedure TscScrollBarStyleHook.UpdateScrollBar;
var
  R: TRect;
begin
  if (FScrollWnd <> nil) and not (FScrollWnd.HandleAllocated) then
  begin
    FScrollWnd.Free;
    FScrollWnd := nil;
    InitScrollBar;
    Exit;
  end;
  if Control.Visible and (FScrollWnd <> nil) and FScrollWnd.HandleAllocated then
  begin
    R := ControlBounds;
    SetWindowPos(FScrollWnd.Handle, HWND_TOP, R.Left,
        R.Top, R.Width, R.Height, SWP_SHOWWINDOW);
  end;
end;

procedure TscScrollBarStyleHook.PaintScrollBar;
begin
  if FScrollWnd <> nil then
    FScrollWnd.Repaint;
end;

procedure TscScrollBarStyleHook.CMSEPaint(var Message: TMessage);
begin
  if FScrollWnd <> nil then
    FScrollWnd.PaintToDC(Message.WParam);
  Handled := True;
end;

procedure TscScrollBarStyleHook.CMSENCPaint(var Message: TMessage);
begin
  Message.Result := SE_RESULT;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

procedure TscScrollBarStyleHook.CNHScroll(var Message: TWMHScroll);
begin
  PaintScrollBar;
end;

procedure TscScrollBarStyleHook.CNVScroll(var Message: TWMVScroll);
begin
  PaintScrollBar;
end;

function TscScrollBarStyleHook.Horizontal: Boolean;
begin
  Result := GetWindowLong(Handle, GWL_STYLE) and SBS_VERT = 0;
end;

function TscScrollBarStyleHook.VertScrollRect: TRect;
begin
  Result := TRect.Create(0, 0, Control.Width, Control.Height);
end;

function TscScrollBarStyleHook.VertSliderRect: TRect;
var
  Info: TScrollInfo;
begin
  if not Control.Enabled then
    Result := TRect.Create(0, 0, 0, 0)
  else
  begin
    Result := Control.ClientRect;
    Info.fMask := SIF_ALL;
    Info.cbSize := SizeOf(Info);
    GetScrollInfo(Handle, SB_CTL, Info);
    if Info.nMax - Info.nMin = 0 then
      Exit(TRect.Empty);

    if Info.nPage = 0 then
    begin
      Result.Top := VertTrackRect.Top + Round(((Info.nPos - Info.nMin) / (Info.nMax - Info.nMin)) * (VertTrackRect.Height - GetSystemMetrics(SM_CYVTHUMB)));
      Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    end
    else
    begin
      Result.Top := VertTrackRect.Top + Round(((Info.nPos - Info.nMin) / (Info.nMax - Info.nMin)) * VertTrackRect.Height);
      Result.Bottom := VertTrackRect.Top + Round(((Info.nPos + Integer(Info.nPage) - Info.nMin - 1) / (Info.nMax - Info.nMin)) * VertTrackRect.Height);
      if Result.Bottom - Result.Top < GetSystemMetrics(SM_CYVTHUMB) then
      begin
        if Info.nMax - Info.nMin - Integer(Info.nPage) = 0 then
          Result.Top := VertTrackRect.Top
        else
          Result.Top := VertTrackRect.Top + Round(((Info.nPos - Info.nMin) / (Info.nMax - Info.nMin - Integer(Info.nPage))) * (VertTrackRect.Height - GetSystemMetrics(SM_CYVTHUMB)));
        Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
      end;
    end;
    if Result.Bottom > VertDownButtonRect.Top then
        Result.Bottom := VertDownButtonRect.Top;
    if VertDownButtonRect.Top - VertUpButtonRect.Bottom < GetSystemMetrics(SM_CYVTHUMB) then
        Result := Rect(0, 0, 0, 0);
  end;
end;

function TscScrollBarStyleHook.VertTrackRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
  end
  else
    Result := TRect.Empty;
end;

function TscScrollBarStyleHook.VertUpButtonRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    if VertScrollRect.Height < GetSystemMetrics(SM_CYVTHUMB) * 2 then
    Dec(Result.Bottom, (GetSystemMetrics(SM_CYVTHUMB) * 2 - VertScrollRect.Height) div 2);
    if Result.Bottom - Result.Top < GetSystemMetrics(SM_CYVTHUMB) div 2 then
      Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVTHUMB) div 2;
  end
  else
    Result := TRect.Empty;
end;

function TscScrollBarStyleHook.VertDownButtonRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
    if VertScrollRect.Height < GetSystemMetrics(SM_CYVTHUMB) * 2 then
    Inc(Result.Top, (GetSystemMetrics(SM_CYVTHUMB) * 2 - VertScrollRect.Height) div 2);
    if Result.Bottom - Result.Top < GetSystemMetrics(SM_CYVTHUMB) div 2 then
      Result.Top := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB) div 2;
  end
  else
    Result := TRect.Empty;
end;

function TscScrollBarStyleHook.HorzScrollRect: TRect;
begin
  Result := TRect.Create(0, 0, Control.Width, Control.Height);
end;

function TscScrollBarStyleHook.HorzSliderRect: TRect;
var
  Info: TScrollInfo;
begin
  if not Control.Enabled then
    Result := TRect.Create(0, 0, 0, 0)
  else
  begin
    Result := Control.ClientRect;
    Info.fMask := SIF_ALL;
    Info.cbSize := SizeOf(Info);
    GetScrollInfo(Handle, SB_CTL, Info);
    if Info.nMax - Info.nMin = 0 then
      Exit(TRect.Empty);

    if Info.nPage = 0 then
    begin
      Result.Left := HorzTrackRect.Left + Round(((Info.nPos - Info.nMin) / (Info.nMax - Info.nMin)) * (HorzTrackRect.Width - GetSystemMetrics(SM_CXHTHUMB)));
      Result.Right := Result.Left + GetSystemMetrics(SM_CXHTHUMB);
    end
    else
    begin
      Result.Left := HorzTrackRect.Left + Round(((Info.nPos - Info.nMin) / (Info.nMax - Info.nMin)) * HorzTrackRect.Width);
      Result.Right := HorzTrackRect.Left + Round(((Info.nPos + Integer(Info.nPage) - Info.nMin - 1) / (Info.nMax - Info.nMin)) * HorzTrackRect.Width);
      if Result.Right - Result.Left < GetSystemMetrics(SM_CXHTHUMB) then
      begin
        if Info.nMax - Info.nMin - Integer(Info.nPage) = 0 then
          Result.Left := HorzTrackRect.Left
        else
          Result.Left := HorzTrackRect.Left + Round(((Info.nPos - Info.nMin) / (Info.nMax - Info.nMin - Integer(Info.nPage))) * (HorzTrackRect.Width - GetSystemMetrics(SM_CXHTHUMB)));
        Result.Right := Result.Left + GetSystemMetrics(SM_CXHTHUMB);
      end;
    end;
    if Result.Right > HorzDownButtonRect.Left then
        Result.Right := HorzDownButtonRect.Left;
    if HorzDownButtonRect.Left - HorzUpButtonRect.Right < GetSystemMetrics(SM_CXHTHUMB) then
        Result := Rect(0, 0, 0, 0);
  end;
end;

function TscScrollBarStyleHook.HorzTrackRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Width > 0 then
  begin
    Result.Left := Result.Left + GetSystemMetrics(SM_CXHTHUMB);
    Result.Right := Result.Right - GetSystemMetrics(SM_CXHTHUMB);
  end
  else
    Result := TRect.Empty;
end;

function TscScrollBarStyleHook.HorzUpButtonRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Height > 0 then
  begin
    Result.Right := Result.Left + GetSystemMetrics(SM_CXHTHUMB);

    if HorzScrollRect.Width < GetSystemMetrics(SM_CXHTHUMB) * 2 then
    Dec(Result.Right, (GetSystemMetrics(SM_CXHTHUMB) * 2 - HorzScrollRect.Width) div 2);
    if Result.Right - Result.Left < GetSystemMetrics(SM_CXHTHUMB) div 2 then
      Result.Right := Result.Left + GetSystemMetrics(SM_CXHTHUMB) div 2;
  end
  else
    Result := TRect.Empty;
end;

function TscScrollBarStyleHook.HorzDownButtonRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Height > 0 then
  begin
    Result.Left := Result.Right - GetSystemMetrics(SM_CXHTHUMB);
    if HorzScrollRect.Width < GetSystemMetrics(SM_CXHTHUMB) * 2 then
    Inc(Result.Left, (GetSystemMetrics(SM_CXHTHUMB) * 2 - HorzScrollRect.Width) div 2);
    if Result.Right - Result.Left < GetSystemMetrics(SM_CXHTHUMB) div 2 then
      Result.Left := Result.Right - GetSystemMetrics(SM_CXHTHUMB) div 2;
  end
  else
    Result := TRect.Empty;
end;

procedure TscScrollBarStyleHook.VertDrawScroll(DC: HDC);
var
  LRect: TRect;
  LHandle: HDC;
  LBitmap: TBitmap;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
begin
  if (Handle = 0) or (DC = 0) or (ControlBounds.Width = 0) or (ControlBounds.Height = 0) then
    Exit;

  LStyle := StyleServices;
  if (VertScrollRect.Width > 0) and LStyle.Available then
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(ControlBounds.Width, ControlBounds.Height);
      LBitmap.Canvas.Brush.Color := StyleServices.ColorToRGB(clBtnFace);
      LBitmap.Canvas.FillRect(Rect(0, 0, LBitmap.Width, LBitmap.Height));

      LRect := VertScrollRect;
      LRect.Top := VertUpButtonRect.Bottom;
      LRect.Bottom := VertDownButtonRect.Top;
      if LRect.Height > 0 then
      begin
        LDetails := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
        StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, LRect);
      end;

      LRect := VertSliderRect;
      LHandle := LBitmap.Canvas.Handle;

      if Control.Enabled then
      begin
        LDetails := StyleServices.GetElementDetails(FVSliderState);
        StyleServices.DrawElement(LHandle, LDetails, LRect);
      end;

      if Control.Enabled then
        LDetails := StyleServices.GetElementDetails(FVUpState)
      else
        LDetails := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);

      StyleServices.DrawElement(LHandle, LDetails, VertUpButtonRect);

      if Control.Enabled then
        LDetails := StyleServices.GetElementDetails(FVDownState)
      else
        LDetails := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);

      StyleServices.DrawElement(LHandle, LDetails, VertDownButtonRect);

      BitBlt(DC, 0, 0, LBitmap.Width, LBitmap.Height, LHandle, 0, 0, SRCCOPY);
    finally
      LBitmap.Free;
    end;
  end;
end;

procedure TscScrollBarStyleHook.HorzDrawScroll(DC: HDC);
var
  LRect: TRect;
  LHandle: HDC;
  LBitmap: TBitmap;
  LDetails: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  if (Handle = 0) or (DC = 0) or (ControlBounds.Width = 0) or (ControlBounds.Height = 0) then
    Exit;

  LStyle := StyleServices;
  if (HorzScrollRect.Height > 0) and LStyle.Available then
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(ControlBounds.Width, ControlBounds.Height);
      LBitmap.Canvas.Brush.Color := LStyle.ColorToRGB(clBtnFace);
      LBitmap.Canvas.FillRect(Rect(0, 0, LBitmap.Width, LBitmap.Height));

      LRect := HorzScrollRect;
      LRect.Left := HorzUpButtonRect.Right;
      LRect.Right := HorzDownButtonRect.Left;
      if LRect.Height > 0 then
      begin
        LDetails := LStyle.GetElementDetails(tsUpperTrackHorzNormal);
        LStyle.DrawElement(LBitmap.Canvas.Handle, LDetails, LRect);
      end;

      LRect := HorzSliderRect;
      LHandle := LBitmap.Canvas.Handle;

      if Control.Enabled then
      begin
        LDetails := LStyle.GetElementDetails(FHSliderState);
        LStyle.DrawElement(LHandle, LDetails, LRect);
      end;

      if Control.Enabled then
        LDetails := LStyle.GetElementDetails(FHUpState)
      else
        LDetails := LStyle.GetElementDetails(tsArrowBtnLeftDisabled);
      LStyle.DrawElement(LHandle, LDetails, HorzUpButtonRect);

      if Control.Enabled then
        LDetails := LStyle.GetElementDetails(FHDownState)
      else
        LDetails := LStyle.GetElementDetails(tsArrowBtnRightDisabled);
      LStyle.DrawElement(LHandle, LDetails, HorzDownButtonRect);

      BitBlt(DC, 0, 0, LBitmap.Width, LBitmap.Height, LHandle, 0, 0, SRCCOPY);
    finally
      LBitmap.Free;
    end;
  end;
end;

procedure TscScrollBarStyleHook.WMNCPaint(var Message: TMessage);
begin
  if FScrollWnd = nil then
    InitScrollBar;
  UpdateScrollBar;
end;

procedure TscScrollBarStyleHook.WMMouseWheel(var Message: TMessage);
begin
  CallDefaultProc(TMessage(Message));
  Invalidate;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WMLButtonDown(var Message: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Message.XPos, Message.YPos);
  if not Horizontal then
  begin
    if VertUpButtonRect.Contains(P) then
    begin
      FVUpState := tsArrowBtnUpPressed;
      PaintScrollBar;
    end
    else if VertDownButtonRect.Contains(P) then
    begin
      FVDownState := tsArrowBtnDownPressed;
      PaintScrollBar;
    end
    else if VertSliderRect.Contains(P) then
    begin
      FVSliderState := tsThumbBtnVertPressed;
      PaintScrollBar;
    end;
  end
  else
  begin
    if HorzUpButtonRect.Contains(P) then
    begin
      FHUpState := tsArrowBtnLeftPressed;
      PaintScrollBar;
    end
    else if HorzDownButtonRect.Contains(P) then
    begin
      FHDownState := tsArrowBtnRightPressed;
      PaintScrollBar;
    end
    else if HorzSliderRect.Contains(P) then
    begin
      FHSliderState := tsThumbBtnHorzPressed;
      PaintScrollBar;
    end;
  end;
end;

procedure TscScrollBarStyleHook.WMLButtonUp(var Message: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Message.XPos, Message.YPos);

  if not Horizontal then
  begin
    if FVSliderState = tsThumbBtnVertPressed then
    begin
      if VertSliderRect.Contains(P) then
        FVSliderState := tsThumbBtnVertHot
      else
        FVSliderState := tsThumbBtnVertNormal;
      PaintScrollBar;
    end
    else if FVUpState = tsArrowBtnUpPressed then
    begin
      if VertUpButtonRect.Contains(P) then
        FVUpState := tsArrowBtnUpHot
      else
        FVUpState := tsArrowBtnUpNormal;
      PaintScrollBar;
    end
    else if FVDownState = tsArrowBtnDownPressed then
    begin
      if VertDownButtonRect.Contains(P) then
        FVDownState := tsArrowBtnDownHot
      else
        FVDownState := tsArrowBtnDownNormal;
      PaintScrollBar;
    end;
  end
  else
  begin
    if FHSliderState = tsThumbBtnHorzPressed then
    begin
      if HorzSliderRect.Contains(P) then
        FHSliderState := tsThumbBtnHorzHot
      else
        FHSliderState := tsThumbBtnHorzNormal;
      PaintScrollBar;
    end
    else if FHUpState = tsArrowBtnLeftPressed then
    begin
      if HorzUpButtonRect.Contains(P) then
        FHUpState := tsArrowBtnLeftHot
      else
        FHUpState:= tsArrowBtnLeftNormal;
      PaintScrollBar;
    end
    else if FHDownState = tsArrowBtnRightPressed then
    begin
       if HorzDownButtonRect.Contains(P) then
        FHDownState := tsArrowBtnRightHot
      else
        FHDownState:= tsArrowBtnRightNormal;
      PaintScrollBar;
    end;
  end;
end;

procedure TscScrollBarStyleHook.WMMouseMove(var Message: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
begin
  inherited;
  CallDefaultProc(TMessage(Message));
  P := Point(Message.XPos, Message.YPos);
  MustUpdateScroll := False;

  if Horizontal then
  begin
    if HorzSliderRect.Width > 0 then
      if PtInRect(HorzSliderRect, P) and (FHSliderState = tsThumbBtnHorzNormal) then
      begin
        FHSliderState := tsThumbBtnHorzHot;
        MustUpdateScroll := True;
      end
      else if not PtInRect(HorzSliderRect, P) and (FHSliderState = tsThumbBtnHorzHot) then
      begin
        FHSliderState := tsThumbBtnHorzNormal;
        MustUpdateScroll := True;
      end;

    if HorzSliderRect.Width > 0 then
      if PtInRect(HorzDownButtonRect, P) and (FHDownState = tsArrowBtnRightNormal) then
      begin
        FHDownState := tsArrowBtnRightHot;
        MustUpdateScroll := True;
      end
      else if not PtInRect(HorzDownButtonRect, P) and (FHDownState = tsArrowBtnRightHot) then
      begin
        FHDownState := tsArrowBtnRightNormal;
        MustUpdateScroll := True;
      end;

    if HorzSliderRect.Width > 0 then
      if PtInRect(HorzUpButtonRect, P) and (FHUpState = tsArrowBtnLeftNormal) then
      begin
        FHUpState := tsArrowBtnLeftHot;
        MustUpdateScroll := True;
      end
      else if not PtInRect(HorzUpButtonRect, P) and (FHUpState = tsArrowBtnLeftHot) then
      begin
        FHUpState := tsArrowBtnLeftNormal;
        MustUpdateScroll := True;
      end;
  end
  else
  begin
    if VertSliderRect.Height > 0 then
      if PtInRect(VertSliderRect, P) and (FVSliderState = tsThumbBtnVertNormal) then
      begin
        FVSliderState := tsThumbBtnVertHot;
        MustUpdateScroll := True;
      end
      else if not PtInRect(VertSliderRect, P) and (FVSliderState = tsThumbBtnVertHot) then
      begin
        FVSliderState := tsThumbBtnVertNormal;
        MustUpdateScroll := True;
      end;

    if VertSliderRect.Height > 0 then
      if PtInRect(VertDownButtonRect, P) and (FVDownState = tsArrowBtnDownNormal) then
      begin
        FVDownState := tsArrowBtnDownHot;
        MustUpdateScroll := True;
      end
      else if not PtInRect(VertDownButtonRect, P) and (FVDownState = tsArrowBtnDownHot) then
      begin
        FVDownState := tsArrowBtnDownNormal;
        MustUpdateScroll := True;
      end;

    if VertSliderRect.Height > 0 then
      if PtInRect(VertUpButtonRect, P) and (FVUpState = tsArrowBtnUpNormal) then
      begin
        FVUpState := tsArrowBtnUpHot;
        MustUpdateScroll := True;
      end
      else if not PtInRect(VertUpButtonRect, P) and (FVUpState = tsArrowBtnUpHot) then
      begin
        FVUpState := tsArrowBtnUpNormal;
        MustUpdateScroll := True;
      end;
  end;

  if MustUpdateScroll then
    PaintScrollBar;

  Handled := True;
end;

procedure TscScrollBarStyleHook.WMLButtonDblClk(var Message: TWMMouse);
begin
  WMLButtonDown(Message);
end;

procedure TscScrollBarStyleHook.WMSetFocus(var Message: TMessage);
begin
  CallDefaultProc(TMessage(Message));
  PaintScrollBar;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WMKillFocus(var Message: TMessage);
begin
  CallDefaultProc(TMessage(Message));
  PaintScrollBar;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WMKeyDown(var Message: TMessage);
begin
  CallDefaultProc(TMessage(Message));
  PaintScrollBar;
  Handled := True;
end;

procedure TscScrollBarStyleHook.WMKeyUp(var Message: TMessage);
begin
  CallDefaultProc(TMessage(Message));
  PaintScrollBar;
  Handled := True;
end;

procedure TscScrollBarStyleHook.MouseLeave;
begin
  inherited;

  if FVSliderState = tsThumbBtnVertHot then
    FVSliderState := tsThumbBtnVertNormal;

  if FHSliderState = tsThumbBtnHorzHot then
    FHSliderState := tsThumbBtnHorzNormal;

  if FVUpState = tsArrowBtnUpHot then
    FVUpState := tsArrowBtnUpNormal;

  if FVDownState = tsArrowBtnDownHot then
    FVDownState := tsArrowBtnDownNormal;

  if FHUpState = tsArrowBtnLeftHot then
    FHUpState := tsArrowBtnLeftNormal;

  if FHDownState = tsArrowBtnRightHot then
    FHDownState := tsArrowBtnRightNormal;

  PaintScrollBar;
end;

procedure TscScrollBarStyleHook.WMCaptureChanged(var Message: TMessage);
var
  MustUpdateScroll: Boolean;
begin
  MustUpdateScroll := False;

  if FVUpState = tsArrowBtnUpPressed then
  begin
    FVUpState := tsArrowBtnUpNormal;
    MustUpdateScroll := True;
  end;

  if FVDownState = tsArrowBtnDownPressed then
  begin
    FVDownState := tsArrowBtnDownNormal;
    MustUpdateScroll := True;
  end;

  if FHUpState = tsArrowBtnLeftPressed then
  begin
    FHUpState := tsArrowBtnLeftNormal;
    MustUpdateScroll := True;
  end;

  if FHDownState = tsArrowBtnRightPressed then
  begin
    FHDownState := tsArrowBtnRightNormal;
    MustUpdateScroll := True;
  end;

  if (FVSliderState = tsThumbBtnVertPressed) and (NativeInt(Message.LParam) <> NativeInt(Handle))
  then
  begin
    FVSliderState := tsThumbBtnVertNormal;
    MustUpdateScroll := True;
  end;

  if (FHSliderState = tsThumbBtnHorzPressed) and (NativeInt(Message.LParam) <> NativeInt(Handle))
  then
  begin
    FHSliderState := tsThumbBtnHorzNormal;
    MustUpdateScroll := True;
  end;

  if  MustUpdateScroll then PaintScrollBar;
end;

{ TScrollingStyleHook.TScrollWindow }

constructor TscScrollingStyleHook.TscScrollWindow.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FStyleHook := nil;
end;

procedure TscScrollingStyleHook.TscScrollWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
  Params.WindowClass.style := Params.WindowClass.style;
  if (FStyleHook <> nil) and FStyleHook.IsPopupWindow then
    Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
end;

procedure TscScrollingStyleHook.TscScrollWindow.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
  if (FStyleHook <> nil) and not FStyleHook.FIsMouseInScrolls and not FStyleHook.IsPopupWindow then
  begin
    FStyleHook.FIsMouseInScrolls := True;
    if IsWindowsXP then
    begin
      FStyleHook.FNeedUpdateNCArea := True;
      SendMessage(FStyleHook.Control.Handle, WM_NCPAINT, 0, 0);
    end
    else
      SetWindowPos(FStyleHook.Control.Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
       SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

procedure TscScrollingStyleHook.TscScrollWindow.WMPaint(var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  BeginPaint(Handle, PS);
  try
    if (FStyleHook <> nil) and
       (FStyleHook.Control.Width > 0) and
       (FStyleHook.Control.Height > 0) then
    begin
      DC := GetWindowDC(Handle);
      try
        if FVertical then
        begin
          with FStyleHook.VertScrollRect do
            MoveWindowOrg(DC, -Left, -Top);
          FStyleHook.DrawVertScroll(DC);
        end
        else
        begin
          with FStyleHook.HorzScrollRect do
            MoveWindowOrg(DC, -Left, -Top);
          FStyleHook.DrawHorzScroll(DC);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

procedure TscScrollingStyleHook.TscScrollWindow.PaintToDC(DC: HDC);
begin
  if (FStyleHook <> nil) and
     (FStyleHook.Control.Width > 0) and
     (FStyleHook.Control.Height > 0) then
  begin
    if FVertical then
      FStyleHook.DrawVertScroll(DC)
    else
      FStyleHook.DrawHorzScroll(DC);
  end;
end;

procedure TscScrollingStyleHook.TscScrollWindow.WndProc(var Message: TMessage);
begin
  inherited;
end;

procedure TscScrollingStyleHook.TscScrollWindow.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

{ TScrollingStyleHook }

constructor TscScrollingStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  FIsMouseInScrolls := False;
  OverridePaintNC := True;
  FNeedUpdateNCArea := False;
  FCallNCDefPaint := False;
  FVertScrollWnd := nil;
  FHorzScrollWnd := nil;
  FInitingScrollBars := False;
  FIsPopupWindow := False;
end;

destructor TscScrollingStyleHook.Destroy;
begin
  FInitingScrollBars := True;
  if FVertScrollWnd <> nil then
  begin
    FVertScrollWnd.StyleHook := nil;
    FreeAndNil(FVertScrollWnd);
  end;
  if FHorzScrollWnd <> nil then
  begin
    FHorzScrollWnd.StyleHook := nil;
    FreeAndNil(FHorzScrollWnd);
  end;
  FInitingScrollBars := False;
  inherited;
end;

function TscScrollingStyleHook.IsPopupWindow;
begin
  Result := (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) or
            (GetWindowLong(Handle, GWL_STYLE) and WS_POPUP = WS_POPUP);
end;

procedure TscScrollingStyleHook.InitScrollBars;
var
  R: TRect;
  WR: TRect;
begin
  if FInitingScrollBars then Exit;

  FInitingScrollBars := True;

  InitScrollState;
  FIsPopupWindow := IsPopupWindow;

  FVertScrollWnd := TscScrollWindow.CreateParented(GetParent(Control.Handle));
  FVertScrollWnd.StyleHook := Self;
  FVertScrollWnd.Vertical := True;
  R := VertScrollRect;
  if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(R) then
  begin
    OffsetRect(R, -R.Left, 0);
    if HasBorder then
      if HasClientEdge then
        OffsetRect(R, 2, 0)
      else
        OffsetRect(R, 1, 0);
  end;
  with R do
   if FIsPopupWindow then
   begin
     WinApi.Windows.GetWindowRect(Control.Handle, WR);
     SetWindowPos(FVertScrollWnd.Handle, HWND_TOPMOST,
        WR.Left + Left, WR.Top + Top,
        Right - Left, Bottom - Top, SWP_NOREDRAW);
   end
   else
     SetWindowPos(FVertScrollWnd.Handle, HWND_TOP, Control.Left + Left, Control.Top + Top,
       Right - Left, Bottom - Top, SWP_NOREDRAW);

  if IsRectEmpty(VertScrollRect) or (not Control.Visible and not FIsPopupWindow) then
    ShowWindow(FVertScrollWnd.Handle, SW_HIDE)
  else
    ShowWindow(FVertScrollWnd.Handle, SW_SHOW);

  FHorzScrollWnd := TscScrollWindow.CreateParented(GetParent(Control.Handle));
  FHorzScrollWnd.StyleHook := Self;
  FHorzScrollWnd.Vertical := False;

  R := HorzScrollRect;
  if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(VertScrollRect) then
    OffsetRect(R, VertScrollRect.Width, 0);
  with R do
    if FIsPopupWindow then
    begin
      WinApi.Windows.GetWindowRect(Control.Handle, WR);
      SetWindowPos(FHorzScrollWnd.Handle, HWND_TOPMOST,
        WR.Left + Left, WR.Top + Top,
          Right - Left, Bottom - Top, SWP_NOREDRAW);
    end
    else
      SetWindowPos(FHorzScrollWnd.Handle, HWND_TOP, Control.Left + Left, Control.Top + Top,
        Right - Left, Bottom - Top, SWP_NOREDRAW);

  if IsRectEmpty(HorzScrollRect) or (not Control.Visible and not FIsPopupWindow) then
    ShowWindow(FHorzScrollWnd.Handle, SW_HIDE)
  else
    ShowWindow(FHorzScrollWnd.Handle, SW_SHOW);

  FInitingScrollBars := False;
end;

procedure TscScrollingStyleHook.MouseLeave;
begin
  inherited;

  if not FLeftButtonDown then
    FIsMouseInScrolls := False;

  if VertSliderState = tsThumbBtnVertHot then
    FVertSliderState := tsThumbBtnVertNormal;

  if FHorzSliderState = tsThumbBtnHorzHot then
    FHorzSliderState := tsThumbBtnHorzNormal;

  if FVertUpState = tsArrowBtnUpHot then
    FVertUpState := tsArrowBtnUpNormal;

  if FVertDownState = tsArrowBtnDownHot then
    FVertDownState := tsArrowBtnDownNormal;

  if FHorzUpState = tsArrowBtnLeftHot then
    FHorzUpState := tsArrowBtnLeftNormal;

  if FHorzDownState = tsArrowBtnRightHot then
    FHorzDownState := tsArrowBtnRightNormal;

  PaintScroll;
end;

procedure TscScrollingStyleHook.PaintNC(Canvas: TCanvas);
begin
  if FInitingScrollBars then Exit;
  inherited;
  DrawBorder;
  if FVertScrollWnd = nil then
    InitScrollBars;
  UpdateScroll;
  PaintScroll;
end;

procedure TscScrollingStyleHook.InitScrollState;
begin
  FVertSliderState := tsThumbBtnVertNormal;
  FVertUpState := tsArrowBtnUpNormal;
  FVertDownState := tsArrowBtnDownNormal;
  FHorzSliderState := tsThumbBtnHorzNormal;
  FHorzUpState := tsArrowBtnLeftNormal;
  FHorzDownState := tsArrowBtnRightNormal;
end;

procedure TscScrollingStyleHook.UpdateScroll;
var
  R: TRect;
  WR: TRect;
begin
  if ((FVertScrollWnd <> nil) and not FVertScrollWnd.HandleAllocated) or
     ((FHorzScrollWnd <> nil) and not FHorzScrollWnd.HandleAllocated)
  then
  begin
    if FVertScrollWnd <> nil then
    begin
      FVertScrollWnd.Free;
      FVertScrollWnd := nil;
    end;
    if FHorzScrollWnd <> nil then
    begin
      FHorzScrollWnd.Free;
      FHorzScrollWnd := nil;
    end;
    InitScrollBars;
    Exit;
  end;

 if not Control.Visible and not FIsPopupWindow then
 begin
   Exit;
 end;

 if (FVertScrollWnd <> nil) and (FVertScrollWnd.HandleAllocated) then
  begin
    R := VertScrollRect;

    if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(R) then
    begin
      OffsetRect(R, -R.Left, 0);
      if HasBorder then
        if HasClientEdge then
          OffsetRect(R, 2, 0)
        else
          OffsetRect(R, 1, 0);
    end;

    if IsRectEmpty(R) then
      ShowWindow(FVertScrollWnd.Handle, SW_HIDE)
    else
    begin
      ShowWindow(FVertScrollWnd.Handle, SW_SHOW);
      with R do
        if FIsPopupWindow then
        begin
          WinApi.Windows.GetWindowRect(Control.Handle, WR);
          SetWindowPos(FVertScrollWnd.Handle, HWND_TOPMOST,
            WR.Left + Left, WR.Top + Top,
              Right - Left, Bottom - Top, SWP_SHOWWINDOW)
        end
        else
          SetWindowPos(FVertScrollWnd.Handle, HWND_TOP, Control.Left + Left,
           Control.Top + Top, Right - Left, Bottom - Top, SWP_SHOWWINDOW);
    end
  end;
  if (FHorzScrollWnd <> nil) and (FHorzScrollWnd.HandleAllocated) then
  begin
    R := HorzScrollRect;

    if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(VertScrollRect) then
      OffsetRect(R, VertScrollRect.Width, 0);
    if IsRectEmpty(R) then
      ShowWindow(FHorzScrollWnd.Handle, SW_HIDE)
    else
    begin
      ShowWindow(FHorzScrollWnd.Handle, SW_SHOW);
      with R do
        if FIsPopupWindow then
        begin
          WinApi.Windows.GetWindowRect(Control.Handle, WR);
          SetWindowPos(FHorzScrollWnd.Handle, HWND_TOPMOST, WR.Left + Left,
            WR.Top + Top, Right - Left, Bottom - Top, SWP_SHOWWINDOW);
        end
        else
          SetWindowPos(FHorzScrollWnd.Handle, HWND_TOP, Control.Left + Left,
            Control.Top + Top, Right - Left, Bottom - Top, SWP_SHOWWINDOW);
    end;
  end;
end;

procedure TscScrollingStyleHook.DrawVertScroll(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if Handle = 0 then Exit;
  if DC = 0 then Exit;

  if (VertScrollRect.Width > 0) and (VertScrollRect.Height > 0) then
  begin
    B := TBitmap.Create;
    try
      B.Width := VertScrollRect.Width;
      B.Height := VertScrollRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -VertScrollRect.Left, -VertScrollRect.Top);
      if StyleServices.Available then
      begin
        R := VertScrollRect;
        R.Top := VertUpButtonRect.Bottom;
        R.Bottom := VertDownButtonRect.Top;
        if (R.Height > 0) and (R.Width > 0) then
        begin
          Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
          StyleServices.DrawElement(B.Canvas.Handle, Details, R);
        end;

        if (VertSliderRect.Height > 0) and (VertSliderRect.Width > 0) then
        begin
          Details := StyleServices.GetElementDetails(VertSliderState);
          StyleServices.DrawElement(B.Canvas.Handle, Details, VertSliderRect);
        end;

        if VertSliderRect.Height <> 0 then
          Details := StyleServices.GetElementDetails(FVertUpState)
        else
          Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
        StyleServices.DrawElement(B.Canvas.Handle, Details, VertUpButtonRect);

        if VertSliderRect.Height <> 0 then
          Details := StyleServices.GetElementDetails(FVertDownState)
        else
          Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
        StyleServices.DrawElement(B.Canvas.Handle, Details, VertDownButtonRect);
      end;
      MoveWindowOrg(B.Canvas.Handle, VertScrollRect.Left, VertScrollRect.Top);
      with VertScrollRect do
        BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

procedure TscScrollingStyleHook.DrawHorzScroll(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if Handle = 0 then Exit;
  if DC = 0 then Exit;

  if (HorzScrollRect.Height > 0) and (HorzScrollRect.Width > 0) then
  begin
    B := TBitmap.Create;
    try
      B.Width := HorzScrollRect.Width;
      B.Height := HorzScrollRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -HorzScrollRect.Left, -HorzScrollRect.Top);
      if StyleServices.Available then
      begin
        R := HorzScrollRect;
        R.Left := HorzUpButtonRect.Right;
        R.Right := HorzDownButtonRect.Left;
        if (R.Height > 0) and (R.Width > 0) then
        begin
          Details := StyleServices.GetElementDetails(tsUpperTrackHorzNormal);
          StyleServices.DrawElement(B.Canvas.Handle, Details, R);
        end;

        if (HorzSliderRect.Height > 0) and (HorzSliderRect.Width > 0) then
        begin
          Details := StyleServices.GetElementDetails(FHorzSliderState);
          StyleServices.DrawElement(B.Canvas.Handle, Details, HorzSliderRect);
        end;

        if HorzSliderRect.Height > 0 then
          Details := StyleServices.GetElementDetails(FHorzUpState)
        else
          Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
        StyleServices.DrawElement(B.Canvas.Handle, Details, HorzUpButtonRect);

        if HorzSliderRect.Height > 0 then
          Details := StyleServices.GetElementDetails(FHorzDownState)
        else
          Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
        StyleServices.DrawElement(B.Canvas.Handle, Details, HorzDownButtonRect);
      end;
      MoveWindowOrg(B.Canvas.Handle, HorzScrollRect.Left, HorzScrollRect.Top);
      with HorzScrollRect do
        BitBlt(DC, Left, Top, B.Width, B.Height,
          B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

function TscScrollingStyleHook.GetVertDownButtonRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Height > 0 then
    Result.Top := Result.Bottom - Min(GetSystemMetrics(SM_CYVTHUMB), Result.Height div 2)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TscScrollingStyleHook.GetVertScrollRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
  if STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0 then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    {$IFDEF VER330_UP}
    P.X := Result.Left + GetSystemMetrics(SM_CXVSCROLL);
    {$ENDIF}
    Result.BottomRight := P;
    if HasBorder then
      if HasClientEdge then
         OffsetRect(Result, 2, 2)
       else
         OffsetRect(Result, 1, 1);
  end;
end;

{$IFDEF VER330_UP}
function TscScrollingStyleHook.GetVertSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
  ThumbSize, ScrollSize, MinSize: Integer;
  SizeRatio: Single;
  SF: TScrollInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
  if (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0) or
     (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    ThumbSize := GetSystemMetrics(SM_CXHTHUMB);
    P.X := Result.Left + ThumbSize;
    Result.BottomRight := P;
    Result.Top := BarInfo.xyThumbTop + 0;
    Result.Bottom := BarInfo.xyThumbBottom + 0;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);

    if ThumbSize <> BarInfo.rcScrollBar.Width then
    begin
      SF.fMask := SIF_ALL;
      SF.cbSize := SizeOf(SF);
      GetScrollInfo(Handle, SB_VERT, SF);
      if SF.nPage = 0 then
      begin
        ScrollSize := VertScrollRect.Height - BarInfo.rcScrollBar.Width * 3;
        if ScrollSize > 0 then
        begin
          SizeRatio := (Result.Top - VertScrollRect.Top - BarInfo.rcScrollBar.Width) / ScrollSize;
          Result.Top := VertTrackRect.Top + Trunc((VertTrackRect.Height - ThumbSize) * SizeRatio);
          Result.Bottom := Result.Top + ThumbSize;
        end;
      end
      else
      begin
        ScrollSize := VertScrollRect.Height - BarInfo.rcScrollBar.Width * 2;
        if ScrollSize > 0 then
        begin
          SizeRatio := VertTrackRect.Height / ScrollSize;
          ThumbSize := Trunc(Result.Height * SizeRatio);
          MinSize := GetSystemMetrics(SM_CXHTHUMB) div 2;
          if ThumbSize < MinSize then
          begin
            ThumbSize := MinSize;
            ScrollSize := ScrollSize - Result.Height;
            if ScrollSize > 0 then
            begin
              SizeRatio := (Result.Top - VertScrollRect.Top - BarInfo.rcScrollBar.Width) / ScrollSize;
              Result.Top := VertTrackRect.Top + Trunc((VertTrackRect.Height - ThumbSize) * SizeRatio);
              Result.Bottom := Result.Top + ThumbSize;
            end;
          end
          else
          begin
            SizeRatio := (Result.Top - VertScrollRect.Top - BarInfo.rcScrollBar.Width) / ScrollSize;
            Result.Top := VertTrackRect.Top + Trunc(VertTrackRect.Height * SizeRatio);
            Result.Bottom := Result.Top + ThumbSize;
          end;
        end;
      end;
    end;

  end;
end;
{$ELSE}
function TscScrollingStyleHook.GetVertSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
  if (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0) or
     (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Top := BarInfo.xyThumbTop + 0;
    Result.Bottom := BarInfo.xyThumbBottom + 0;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;
{$ENDIF}

function TscScrollingStyleHook.GetVertTrackRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TscScrollingStyleHook.GetVertUpButtonRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Height > 0 then
    Result.Bottom := Result.Top + Min(GetSystemMetrics(SM_CYVTHUMB), Result.Height div 2)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TscScrollingStyleHook.GetHorzDownButtonRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Width > 0 then
    Result.Left := Result.Right - Min(GetSystemMetrics(SM_CXHTHUMB), Result.Width div 2)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TscScrollingStyleHook.GetHorzScrollRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
  if STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0 then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    {$IFDEF VER330_UP}
    P.Y := Result.Top + GetSystemMetrics(SM_CYHSCROLL);
    {$ENDIF}
    Result.BottomRight := P;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;

{$IFDEF VER330_UP}
function TscScrollingStyleHook.GetHorzSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
  ThumbSize, ScrollSize, MinSize: Integer;
  SizeRatio: Single;
  SF: TScrollInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
  if (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0) or
     (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0) then
    Result := TRect.Create(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    ThumbSize := GetSystemMetrics(SM_CYVTHUMB);
    P.Y := Result.Top + ThumbSize;
    Result.BottomRight := P;
    Result.Left := BarInfo.xyThumbTop + 0;
    Result.Right := BarInfo.xyThumbBottom + 0;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);

    if ThumbSize <> BarInfo.rcScrollBar.Height then
    begin
      SF.fMask := SIF_ALL;
      SF.cbSize := SizeOf(SF);
      GetScrollInfo(Handle, SB_HORZ, SF);
      if SF.nPage = 0 then
      begin
        ScrollSize := HorzScrollRect.Width - BarInfo.rcScrollBar.Height * 3;
        if ScrollSize > 0 then
        begin
          SizeRatio := (Result.Left - HorzScrollRect.Left - BarInfo.rcScrollBar.Height) / ScrollSize;
          Result.Left := HorzTrackRect.Left + Trunc((HorzTrackRect.Width - ThumbSize) * SizeRatio);
          Result.Right := Result.Left + ThumbSize;
        end;
      end
      else
      begin
        ScrollSize := HorzScrollRect.Width - BarInfo.rcScrollBar.Height * 2;
        if ScrollSize > 0 then
        begin
          SizeRatio := HorzTrackRect.Width / ScrollSize;
          ThumbSize := Trunc(Result.Width * SizeRatio);
          MinSize := GetSystemMetrics(SM_CYVTHUMB) div 2;
          if ThumbSize < MinSize then
          begin
            ThumbSize := MinSize;
            ScrollSize := ScrollSize - Result.Width;
            if ScrollSize > 0 then
            begin
              SizeRatio := (Result.Left - HorzScrollRect.Left - BarInfo.rcScrollBar.Height) / ScrollSize;
              Result.Left := HorzTrackRect.Left + Trunc((HorzTrackRect.Width - ThumbSize) * SizeRatio);
              Result.Right := Result.Left + ThumbSize;
            end;
          end
          else
          begin
            SizeRatio := (Result.Left - HorzScrollRect.Left - BarInfo.rcScrollBar.Height) / ScrollSize;
            Result.Left := HorzTrackRect.Left + Trunc(HorzTrackRect.Width * SizeRatio);
            Result.Right := Result.Left + ThumbSize;
          end;
        end;
      end;
    end;

  end;
end;
{$ELSE}
function TscScrollingStyleHook.GetHorzSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
  if (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0) or
     (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0)
  then
    Result := TRect.Create(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Left := BarInfo.xyThumbTop + 0;
    Result.Right := BarInfo.xyThumbBottom + 0;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;
{$ENDIF}

function TscScrollingStyleHook.GetHorzTrackRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Width > 0 then
  begin
    Result.Left := Result.Left + GetSystemMetrics(SM_CXHTHUMB);
    Result.Right := Result.Right - GetSystemMetrics(SM_CXHTHUMB);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TscScrollingStyleHook.GetHorzUpButtonRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Width > 0 then
    Result.Right := Result.Left + Min(GetSystemMetrics(SM_CXHTHUMB), Result.Width div 2)
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TscScrollingStyleHook.DrawBorderToDC(DC: HDC);
var
  ExStyle, Style: Integer;
  H, W: Integer;
  DrawRect: TRect;
  C: TCanvas;
  Details: TThemedElementDetails;
begin
  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  Style := GetWindowLong(Handle, GWL_STYLE);
  DrawRect := Rect(0, 0, Control.Width, Control.Height);
  if ExStyle and WS_EX_CLIENTEDGE <> 0 then
  begin
    Details := StyleServices.GetElementDetails(teEditTextNormal);
    StyleServices.DrawElement(DC, Details, DrawRect);
    InflateRect(DrawRect, -2, -2);
  end
  else
  if (Style and WS_BORDER) <> 0 then
  begin
    C := TCanvas.Create;
    C.Handle := DC;
    C.Brush.Style := bsClear;
    C.Pen.Color := GetStyleColor(clWindowFrame);
    C.Rectangle(DrawRect);
    C.Handle := 0;
    C.Free;
  end;
  if ((Style and WS_HSCROLL) <> 0) and ((Style and WS_VSCROLL) <> 0) then
  begin
    W := GetSystemMetrics(SM_CXVSCROLL);
    H := GetSystemMetrics(SM_CYHSCROLL);
    if Control.UseRightToLeftScrollBar then
      DrawRect := Rect(DrawRect.Left,
        DrawRect.Bottom - H, DrawRect.Left + W, DrawRect.Bottom)
    else
      DrawRect := Rect(DrawRect.Right - W,
        DrawRect.Bottom - H, DrawRect.Right, DrawRect.Bottom);
    C := TCanvas.Create;
    C.Handle := DC;
    C.Brush.Color := GetStyleColor(clBtnFace);
    C.FillRect(DrawRect);
    C.Handle := 0;
    C.Free;
  end;
end;

procedure TscScrollingStyleHook.DrawBorder;
var
  ExStyle, Style: Integer;
  H, W: Integer;
  LFontRecall: TGDIHandleRecall;
  DrawRect: TRect;
  DC: HDC;
  C: TCanvas;
begin
  if StyleServices.Available then
    StyleServices.PaintBorder(Control, True);
  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  Style := GetWindowLong(Handle, GWL_STYLE);
  if not (((ExStyle and WS_EX_CLIENTEDGE) <> 0) or ((Style and WS_BORDER) <> 0))
     and (((Style and WS_HSCROLL) <> 0) and ((Style and WS_VSCROLL) <> 0))
  then
  begin
    GetWindowRect(Handle, DrawRect);
    OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
    W := GetSystemMetrics(SM_CXVSCROLL);
    H := GetSystemMetrics(SM_CYHSCROLL);
    DC := GetWindowDC(Control.Handle);
    with DrawRect do
      if Control.UseRightToLeftScrollBar then
        DrawRect := Rect(Left, Bottom - H, Left + W, Bottom)
      else
        DrawRect := Rect(Right - W, Bottom - H, Right, Bottom);
    LFontRecall := TGDIHandleRecall.Create(DC, OBJ_FONT);
    try
      C := LFontRecall.Canvas;
      C.Brush.Color := GetStyleColor(clBtnFace);
      C.FillRect(DrawRect);
    finally
      LFontRecall.Free;
      ReleaseDC(Control.Handle, DC);
    end;
  end;
end;

procedure TscScrollingStyleHook.PaintScroll;
begin
  if FInitingScrollBars then Exit;
  if FVertScrollWnd <> nil then
    FVertScrollWnd.Repaint;
  if FHorzScrollWnd <> nil then
    FHorzScrollWnd.Repaint;
end;

procedure TscScrollingStyleHook.WMHScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.WMVScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.WMMouseWheel(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.WMCaptureChanged(var Msg: TMessage);
var
  P: TPoint;
begin

  GetCursorPos(P);
  if Control <> nil then
    P := Control.ScreenToClient(P);

  if FVertUpState = tsArrowBtnUpPressed then
  begin
    if VertUpButtonRect.Contains(P) then
      FVertUpState := tsArrowBtnUpHot
    else
      FVertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if FVertDownState = tsArrowBtnDownPressed then
  begin
    if VertDownButtonRect.Contains(P) then
      FVertDownState := tsArrowBtnDownHot
    else
      FVertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  if FHorzUpState = tsArrowBtnLeftPressed then
  begin
     if HorzUpButtonRect.Contains(P) then
      FHorzUpState := tsArrowBtnLeftHot
    else
      FHorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if FHorzDownState = tsArrowBtnRightPressed then
  begin
     if HorzDownButtonRect.Contains(P) then
      FHorzDownState := tsArrowBtnRightHot
    else
      FHorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  Handled := True;
end;

procedure TscScrollingStyleHook.WMNCLButtonDown(var Msg: TWMMouse);
var
  P: TPoint;
  SF: TScrollInfo;
begin

  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if (VertTrackRect.Height > 0) and VertSliderRect.Contains(P) then
  begin
    FLeftButtonDown := True;
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    FListPos := SF.nPos;
    FScrollPos := SF.nPos;
    FPrevScrollPos := Mouse.CursorPos.Y;
    FVertSliderState := tsThumbBtnVertPressed;
    PaintScroll;
    SetCapture(Handle);
    Handled := True;
    Exit;
  end;

  if (HorzTrackRect.Width > 0) and HorzSliderRect.Contains(P) then
  begin
    FLeftButtonDown := True;
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    FListPos := SF.nPos;
    FScrollPos := SF.nPos;
    FPrevScrollPos := Mouse.CursorPos.X;
    FHorzSliderState :=  tsThumbBtnHorzPressed;
    PaintScroll;
    SetCapture(Handle);
    Handled := True;
    Exit;
  end;

  if VertDownButtonRect.Contains(P) and (VertSliderRect.Height > 0) then
    FVertDownState := tsArrowBtnDownPressed;

  if VertUpButtonRect.Contains(P) and (VertSliderRect.Height > 0) then
    FVertUpState := tsArrowBtnUpPressed;

  if HorzDownButtonRect.Contains(P) and (HorzSliderRect.Width > 0)  then
    FHorzDownState := tsArrowBtnRightPressed;

  if HorzUpButtonRect.Contains(P) and (HorzSliderRect.Width > 0) then
    FHorzUpState := tsArrowBtnLeftPressed;

  PaintScroll;
end;

procedure TscScrollingStyleHook.WMNCLButtonDblClk(var Msg: TWMMouse);
begin
  WMNCLButtonDown(Msg);
end;

procedure TscScrollingStyleHook.WMNCLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if not MouseInControl then
    FIsMouseInScrolls := False;

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if VertSliderState =  tsThumbBtnVertPressed then
  begin
    FLeftButtonDown := False;
    FVertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if FHorzSliderState = tsThumbBtnHorzPressed then
  begin
    FLeftButtonDown := False;
    FHorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if VertSliderRect.Height > 0 then
    if VertDownButtonRect.Contains(P) then
      FVertDownState := tsArrowBtnDownHot
    else
      FVertDownState := tsArrowBtnDownNormal;

  if VertSliderRect.Height > 0 then
    if VertUpButtonRect.Contains(P) then
      FVertUpState := tsArrowBtnUpHot
    else
      FVertUpState := tsArrowBtnUpNormal;

  if HorzSliderRect.Width > 0 then
    if HorzDownButtonRect.Contains(P) then
      FHorzDownState := tsArrowBtnRightHot
    else
      FHorzDownState := tsArrowBtnRightNormal;

  if HorzSliderRect.Width > 0 then
    if HorzUpButtonRect.Contains(P) then
      FHorzUpState := tsArrowBtnLeftHot
    else
      FHorzUpState := tsArrowBtnLeftNormal;

  CallDefaultProc(TMessage(Msg));
  if (HorzSliderRect.Width > 0) or (VertSliderRect.Height > 0) then
    PaintScroll;
  Handled := True;
end;


procedure TscScrollingStyleHook.WMNCMouseMove(var Msg: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
begin
  inherited;
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  MustUpdateScroll := False;

  if VertSliderRect.Height > 0 then
  if VertSliderRect.Contains(P) and (VertSliderState = tsThumbBtnVertNormal) then
  begin
    FVertSliderState := tsThumbBtnVertHot;
    MustUpdateScroll := True;
  end
  else
    if not VertSliderRect.Contains(P) and (VertSliderState = tsThumbBtnVertHot) then
    begin
      FVertSliderState := tsThumbBtnVertNormal;
      MustUpdateScroll := True;
    end;

  if HorzSliderRect.Width > 0 then
    if HorzSliderRect.Contains(P) and (FHorzSliderState = tsThumbBtnHorzNormal) then
    begin
      FHorzSliderState := tsThumbBtnHorzHot;
      MustUpdateScroll := True;
    end
    else
      if not HorzSliderRect.Contains(P) and (FHorzSliderState = tsThumbBtnHorzHot) then
      begin
        FHorzSliderState := tsThumbBtnHorzNormal;
        MustUpdateScroll := True;
      end;

  if VertSliderRect.Height > 0 then
    if VertDownButtonRect.Contains(P) and (FVertDownState = tsArrowBtnDownNormal) then
    begin
      FVertDownState := tsArrowBtnDownHot;
      MustUpdateScroll := True;
    end
    else
      if not VertDownButtonRect.Contains(P) and (FVertDownState = tsArrowBtnDownHot) then
      begin
        FVertDownState := tsArrowBtnDownNormal;
        MustUpdateScroll := True;
      end;

  if VertSliderRect.Height > 0 then
    if VertUpButtonRect.Contains(P) and (FVertUpState = tsArrowBtnUpNormal) then
    begin
      FVertUpState := tsArrowBtnUpHot;
      MustUpdateScroll := True;
    end
    else if not VertUpButtonRect.Contains(P) and (FVertUpState = tsArrowBtnUpHot) then
    begin
      FVertUpState := tsArrowBtnUpNormal;
      MustUpdateScroll := True;
    end;

  if HorzSliderRect.Width > 0 then
    if HorzDownButtonRect.Contains(P) and (FHorzDownState = tsArrowBtnRightNormal) then
    begin
      FHorzDownState := tsArrowBtnRightHot;
      MustUpdateScroll := True;
    end
    else if not HorzDownButtonRect.Contains(P) and (FHorzDownState = tsArrowBtnRightHot) then
    begin
      FHorzDownState := tsArrowBtnRightNormal;
      MustUpdateScroll := True;
    end;

  if HorzSliderRect.Width > 0 then
    if HorzUpButtonRect.Contains(P) and (FHorzUpState = tsArrowBtnLeftNormal) then
    begin
      FHorzUpState := tsArrowBtnLeftHot;
      MustUpdateScroll := True;
    end
    else if not HorzUpButtonRect.Contains(P) and (FHorzUpState = tsArrowBtnLeftHot) then
    begin
      FHorzUpState := tsArrowBtnLeftNormal;
      MustUpdateScroll := True;
    end;

  if MustUpdateScroll then
    PaintScroll;
end;

procedure TscScrollingStyleHook.WMLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  if not MouseInControl and FLeftButtonDown then
    FIsMouseInScrolls := False;

  GetCursorPos(P);
  if Control <> nil then
    P := Control.ScreenToClient(P);

  if VertSliderState = tsThumbBtnVertPressed then
  begin
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(FScrollPos))), 0);
    FLeftButtonDown := False;
    if VertSliderRect.Contains(P) then
      FVertSliderState := tsThumbBtnVertHot
    else
      FVertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
    Handled := True;
    ReleaseCapture;
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
    Exit;
  end;

  if FHorzSliderState = tsThumbBtnHorzPressed then
  begin
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(FScrollPos))), 0);
    FLeftButtonDown := False;
    if HorzSliderRect.Contains(P) then
      FHorzSliderState  := tsThumbBtnHorzHot
    else
      FHorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
    Handled := True;
    ReleaseCapture;
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
    Exit;
  end;

  if FVertUpState = tsArrowBtnUpPressed then
  begin
    if VertUpButtonRect.Contains(P) then
      FVertUpState := tsArrowBtnUpHot
    else
      FVertUpState := tsArrowBtnUpNormal;
  end;

  if FVertDownState = tsArrowBtnDownPressed then
  begin
     if VertDownButtonRect.Contains(P) then
      FVertDownState := tsArrowBtnDownHot
    else
      FVertDownState := tsArrowBtnDownNormal;
  end;

  if FHorzUpState = tsArrowBtnLeftPressed then
  begin
    if HorzUpButtonRect.Contains(P) then
      FHorzUpState := tsArrowBtnLeftHot
    else
      FHorzUpState := tsArrowBtnLeftNormal;
  end;

  if FHorzDownState = tsArrowBtnRightPressed then
  begin
     if HorzDownButtonRect.Contains(P) then
      FHorzDownState := tsArrowBtnRightHot
    else
      FHorzDownState := tsArrowBtnRightNormal;
  end;

  FLeftButtonDown := False;
  PaintScroll;
end;

procedure TscScrollingStyleHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
  FOldPos: Integer;
  P1, P2: TPoint;
  TrackR: TRect;
begin
  inherited;

  if VertSliderState = tsThumbBtnVertPressed then
  begin
    if VertTrackRect.Height = 0 then Exit;


    TrackR := VertTrackRect;
    P1 := TrackR.TopLeft;
    P2 := TrackR.BottomRight;
    ClientToScreen(Handle, P1);
    ClientToScreen(Handle, P2);

    if (Mouse.CursorPos.Y < P1.Y) and (FPrevScrollPos <= Mouse.CursorPos.Y) then
    begin
      FPrevScrollPos := P1.Y;
      Exit;
    end;

    if (Mouse.CursorPos.Y > P2.Y) and (FPrevScrollPos >= Mouse.CursorPos.Y) then
    begin
      FPrevScrollPos := P2.Y;
      Exit;
    end;


    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    if SF.nPos <> Round(FScrollPos) then FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.Y - FPrevScrollPos) / TrackR.Height);
    if FScrollPos < SF.nMin then FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.Y;

    FOldPos := SF.nPos;
    SF.nPos := Round(FScrollPos);

    if FOldPos <> SF.nPos then
    begin
      SetScrollInfo(Handle, SB_VERT, SF, False);
      PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(FScrollPos))), 0);
      PaintScroll;
    end;

    Handled := True;
    Exit;
  end;

  if FHorzSliderState = tsThumbBtnHorzPressed then
  begin
    if HorzTrackRect.Width = 0 then Exit;

    TrackR := HorzTrackRect;
    P1 := TrackR.TopLeft;
    P2 := TrackR.BottomRight;
    ClientToScreen(Handle, P1);
    ClientToScreen(Handle, P2);

    if (Mouse.CursorPos.X < P1.X) and (FPrevScrollPos <= Mouse.CursorPos.X) then
    begin
      FPrevScrollPos := P1.X;
      Exit;
    end;

    if (Mouse.CursorPos.X > P2.X) and (FPrevScrollPos >= Mouse.CursorPos.X) then
    begin
      FPrevScrollPos := P2.X;
      Exit;
    end;

    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    if SF.nPos <> Round(FScrollPos) then FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.X - FPrevScrollPos) / TrackR.Width);
    if FScrollPos < SF.nMin then FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.X;

    FOldPos := SF.nPos;
    SF.nPos := Round(FScrollPos);

    if FOldPos <> SF.nPos then
    begin
      SetScrollInfo(Handle, SB_HORZ, SF, False);
      PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(FScrollPos))), 0);
      PaintScroll;
    end;

    Handled := True;
    Exit;
  end;

  if (FHorzSliderState <> tsThumbBtnHorzPressed) and (FHorzSliderState = tsThumbBtnHorzHot) then
  begin
    FHorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
  end;

  if (VertSliderState <> tsThumbBtnVertPressed) and (VertSliderState = tsThumbBtnVertHot) then
  begin
    FVertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
  end;

  if (FHorzUpState <> tsArrowBtnLeftPressed) and (FHorzUpState = tsArrowBtnLeftHot) then
  begin
    FHorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if (FHorzDownState <> tsArrowBtnRightPressed) and (FHorzDownState =tsArrowBtnRightHot) then
  begin
    FHorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  if (FVertUpState <> tsArrowBtnUpPressed) and (FVertUpState = tsArrowBtnUpHot) then
  begin
    FVertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if (FVertDownState <> tsArrowBtnDownPressed) and (FVertDownState = tsArrowBtnDownHot) then
  begin
    FVertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  if FLeftButtonDown then
    PaintScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.WMKeyDown(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.WMKeyUp(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.WMLButtonDown(var Msg: TWMMouse);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.Paint(Canvas: TCanvas);
begin
  PaintScroll;
end;

procedure TscScrollingStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited PaintBackground(Canvas);
  PaintScroll;
end;

function TscScrollingStyleHook.GetParentBounds: TRect;
begin
  if (Control <> nil) and (Control.Parent <> nil) then
    Result := Control.Parent.BoundsRect
  else if Handle <> 0 then
    GetWindowRect(Control.ParentWindow, Result)
  else
    Result := TRect.Empty;
end;

procedure TscScrollingStyleHook.WMClose(var Msg: TWMCLOSE);
begin
  Handled := True;
end;

procedure TscScrollingStyleHook.WMShowWindow(var Msg: TWMShowWindow);
begin
  CallDefaultProc(TMessage(Msg));

  if (FVertScrollWnd <> nil) and FVertScrollWnd.HandleAllocated then
    if Msg.Show then
      ShowWindow(FVertScrollWnd.Handle, SW_SHOW)
    else
      ShowWindow(FVertScrollWnd.Handle, SW_HIDE);

  if (FHorzScrollWnd <> nil) and FHorzScrollWnd.HandleAllocated then
    if Msg.Show then
      ShowWindow(FHorzScrollWnd.Handle, SW_SHOW)
    else
      ShowWindow(FHorzScrollWnd.Handle, SW_HIDE);

  Handled := True;
end;

procedure TscScrollingStyleHook.WMWindowPosChanged;
begin
  CallDefaultProc(TMessage(Msg));
  if Msg.WindowPos.Flags and SWP_HIDEWINDOW = SWP_HIDEWINDOW then
  begin
    if FVertScrollWnd <> nil then
      ShowWindow(FVertScrollWnd.Handle, SW_HIDE);
    if FHorzScrollWnd <> nil then
      ShowWindow(FHorzScrollWnd.Handle, SW_HIDE);
  end
  else
    UpdateScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.CMSENCPaint(var Message: TMessage);
var
  DC: HDC;
begin
  Message.Result := SE_RESULT;
  Handled := True;
  DC := Message.WParam;
  // draw frame
  DrawBorderToDC(DC);
  // draw scrollbars
  if FInitingScrollBars then Exit;
  if FVertScrollWnd <> nil then
    FVertScrollWnd.PaintToDC(DC);
  if FHorzScrollWnd <> nil then
    FHorzScrollWnd.PaintToDC(DC);
end;

procedure TscScrollingStyleHook.WMNCPAINT(var Message: TWMNCPAINT);
begin
  if IsWindowsXP and FNeedUpdateNCArea then
  begin
    if FCallNCDefPaint then Exit;
    FCallNCDefPaint := True;
    try
      CallDefaultProc(TMessage(Message));
    finally
      FCallNCDefPaint := False;
      FNeedUpdateNCArea := False;
    end;
  end;
  inherited;
end;

procedure TscScrollingStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
end;

procedure TscScrollingStyleHook.CMVisibleChanged(var Msg: TMessage);
begin
  if Control.HandleAllocated then
  begin
    if FVertScrollWnd <> nil then
      if (Control.Visible) then
        ShowWindow(FVertScrollWnd.Handle, SW_SHOW)
      else
        ShowWindow(FVertScrollWnd.Handle, SW_HIDE);

    if FHorzScrollWnd <> nil then
      if (Control.Visible) then
        ShowWindow(FHorzScrollWnd.Handle, SW_SHOW)
      else
        ShowWindow(FHorzScrollWnd.Handle, SW_HIDE);
  end;
  Handled := False;
end;

procedure TscScrollingStyleHook.WMMove(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  if (FVertScrollWnd <> nil) or (FHorzScrollWnd <> nil) then
    SetWindowPos(Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  UpdateScroll;
  Handled := True;
end;

procedure TscScrollingStyleHook.WMSize(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  UpdateScroll;
  Handled := True;
end;

constructor TscTreeViewStyleHook.Create(AControl: TWinControl);
var
  LColor: TColor;
begin
  inherited;
  OverrideEraseBkgnd := False;
  with StyleServices do
  begin
    if not GetElementColor(GetElementDetails(ttItemNormal), ecTextColor, LColor) or
       (LColor = clNone) then
      LColor := GetSystemColor(clWindowText);
  end;
  {$IFNDEF VER230}
  if seFont in Control.StyleElements then
    FontColor := LColor
  else
    FontColor := TWinControlClass(Control).Font.Color;
  if seClient in Control.StyleElements then
    Brush.Color := StyleServices.GetStyleColor(scTreeView)
  else
    Brush.Color := TWinControlClass(Control).Color;
  {$ELSE}
    FontColor := LColor;
    Brush.Color := StyleServices.GetStyleColor(scTreeView);
  {$ENDIF}
end;

procedure TscTreeViewStyleHook.TVMSetBkColor(var Message: TMessage);
begin
  Message.LParam := LPARAM(ColorToRGB(Brush.Color));
  Handled := False;
end;

procedure TscTreeViewStyleHook.TVMSetTextColor(var Message: TMessage);
begin
  Message.LParam := LPARAM(ColorToRGB(FontColor));
  Handled := False;
end;

procedure TscTreeViewStyleHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
begin
  if VertSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.Y - PrevScrollPos) / VertTrackRect.Height);
    if ScrollPos < SF.nMin then
      ScrollPos := SF.nMin;
    if ScrollPos > SF.nMax then
      ScrollPos := SF.nMax;

    PrevScrollPos := Mouse.CursorPos.Y;
    if Control is TCustomTreeView then
    begin
      PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(ScrollPos))), 0);
      SF.nPos := Round(ScrollPos);
      SF.nTrackPos := Round(ScrollPos);
      SetScrollInfo(Handle, SB_VERT, SF, True);
    end
    else
      PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(ScrollPos))), 0);
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if HorzSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.X - PrevScrollPos) / HorzTrackRect.Width);
    if ScrollPos < SF.nMin then
      ScrollPos := SF.nMin;
    if ScrollPos > SF.nMax then
      ScrollPos := SF.nMax;

    PrevScrollPos := Mouse.CursorPos.X;

    if Control is TCustomTreeView then
    begin
      PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(ScrollPos))), 0);
      SF.nPos := Round(ScrollPos);
      SF.nTrackPos := Round(ScrollPos);
      SetScrollInfo(Handle, SB_HORZ, SF, True);
    end
    else
      PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(ScrollPos))), 0);
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if (HorzSliderState <> tsThumbBtnHorzPressed) and (HorzSliderState = tsThumbBtnHorzHot) then
  begin
    HorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
  end;

  if (VertSliderState <> tsThumbBtnVertPressed) and (VertSliderState = tsThumbBtnVertHot) then
  begin
    VertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
  end;

  if (HorzUpState <> tsArrowBtnLeftPressed) and (HorzUpState = tsArrowBtnLeftHot) then
  begin
    HorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if (HorzDownState <> tsArrowBtnRightPressed) and (HorzDownState =tsArrowBtnRightHot) then
  begin
    HorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  if (VertUpState <> tsArrowBtnUpPressed) and (VertUpState = tsArrowBtnUpHot) then
  begin
    VertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if (VertDownState <> tsArrowBtnDownPressed) and (VertDownState = tsArrowBtnDownHot) then
  begin
    VertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  if LeftButtonDown then
    PaintScroll;
  Handled := True;
end;


procedure TscTreeViewStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_ERASEBKGND:
      PaintScroll;
    WM_HSCROLL:
      if Control.DoubleBuffered then
        Invalidate;
  end;
end;

constructor TscStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  UseSystemFont := False;
end;

procedure TscStatusBar.WMSize(var Message: TWMSize);
begin
  inherited;
  if ControlCount > 0 then
    ReAlign;
end;

procedure TscStatusBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  I, J, W: Integer;
  FScale: Boolean;
  {$IFDEF VER330_UP}
  H: Integer;
  {$ENDIF}
begin
  J := -1;
  W := 0;
  if Panels.Count > 0 then
  begin
    for I := 0 to Panels.Count - 1 do
      if Panels[I].Width > 0 then
      begin
        J := I;
        W := Panels[I].Width;
        Break;
      end;
  end;
  {$IFDEF VER330_UP}
  H := Font.Height;
  {$ENDIF}
  inherited;
  if Panels.Count > 0 then
  begin
    if J <> -1 then
      FScale := W = Panels[J].Width
    else
      FScale := True;
    if FScale then
      for I := 0 to Panels.Count - 1 do
        Panels[I].Width := MulDiv(Panels[I].Width, M, D);
  end;
  {$IFDEF VER330_UP}
  if UseSystemFont and (H = Font.Height) then
     Font.Height := MulDiv(Screen.HintFont.Height, FCurrentPPI, Screen.PixelsPerInch);
  {$ENDIF}
end;

constructor TscStatusBarStyleHook.Create;
begin
  inherited;
  OverridePaint := True;
  DoubleBuffered := True;
  FFromWMPaint := False;
end;

procedure TscStatusBarStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_PAINT:
      FFromWMPaint := True;
  end;
  inherited;
  case Message.Msg of
    WM_PAINT:
      FFromWMPaint := False;
  end;
end;

procedure TscStatusBarStyleHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  Buffer: TBitmap;
  FCanvas: TCanvas;
begin
  if FFromWMPaint then
  begin
    Message.Result := 1;
    Handled := True;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  FCanvas.Handle := Message.DC;
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Control.Width, Control.Height);
    Paint(Buffer.Canvas);
    FCanvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
  Message.Result := 1;
  Handled := True;
end;

procedure TscStatusBarStyleHook.Paint(Canvas: TCanvas);
const
  AlignStyles: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R, R1: TRect;
  Res, Count, I: Integer;
  Idx, Flags: Cardinal;
  Details: TThemedElementDetails;
  LText: string;
  Borders: array [0..2] of Integer;
  FForm: TCustomForm;
begin
  if not StyleServices.Available then
    Exit;

  if (Control is TCustomStatusBar) and (Control.Parent is TCustomForm) then
    FForm := TCustomForm(TCustomStatusBar(Control).Parent)
  else
    FForm := nil;

  Details := StyleServices.GetElementDetails(tsStatusRoot);
  StyleServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, Control.Width, Control.Height));

  if SendMessage(Handle, SB_ISSIMPLE, 0, 0) > 0 then
  begin
    R := Control.ClientRect;
    FillChar(Borders, SizeOf(Borders), 0);
    SendMessage(Handle, SB_GETBORDERS, 0, IntPtr(@Borders));
    R.Left := Borders[0] + Borders[2];
    R.Top := Borders[1];
    R.Bottom := R.Bottom - Borders[1];
    R.Right := R.Right - Borders[2];

    R1 := R;
    R1.Right := Control.ClientWidth + 5;
    Details := StyleServices.GetElementDetails(tsPane);
    StyleServices.DrawElement(Canvas.Handle, Details, R1);

    R1 := Control.ClientRect;
    R1.Left := R1.Right - 17;
    R1.Top := R1.Bottom - 17;
    if (FForm <> nil) and not IsZoomed(FForm.Handle) and (TCustomStatusBar(Control).SizeGrip) then
    begin
      Details := StyleServices.GetElementDetails(tsGripper);
      StyleServices.DrawElement(Canvas.Handle, Details, R1);
    end;
    Details := StyleServices.GetElementDetails(tsPane);
    if Control is TStatusBar then
    begin
      LText := TStatusBar(Control).SimpleText;
      Flags := Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER);
      DrawControlText(Canvas, Details, LText, R, Flags);
    end
    else
    begin
      SetLength(LText, Word(SendMessage(Handle, SB_GETTEXTLENGTH, 0, 0)));
      if Length(LText) > 0 then
      begin
       SendMessage(Handle, SB_GETTEXT, 0, IntPtr(@LText[1]));
       Flags := Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER);
       DrawControlText(Canvas, Details, LText, R, Flags);
      end;
    end;
  end
  else
  begin
    if Control is TStatusBar then
      Count := TStatusBar(Control).Panels.Count
    else
      Count := SendMessage(Handle, SB_GETPARTS, 0, 0);
    for I := 0 to Count - 1 do
    begin
      R := Rect(0, 0, 0, 0);
      SendMessage(Handle, SB_GETRECT, I, IntPtr(@R));
      if IsRectEmpty(R) then
        Continue;

      R1 := R;
      if I = Count - 1 then
        R1.Right := Control.ClientWidth + 5;
      Details := StyleServices.GetElementDetails(tsPane);
      StyleServices.DrawElement(Canvas.Handle, Details, R1);
      if I = Count - 1 then
      begin
        R1 := Control.ClientRect;
        R1.Left := R1.Right - 17;
        R1.Top := R1.Bottom - 17;
        if (FForm <> nil) and not IsZoomed(FForm.Handle) and (TCustomStatusBar(Control).SizeGrip) then
        begin
          Details := StyleServices.GetElementDetails(tsGripper);
          StyleServices.DrawElement(Canvas.Handle, Details, R1);
        end;
      end;
      Details := StyleServices.GetElementDetails(tsPane);
      InflateRect(R, -1, -1);
      if Control is TCustomStatusBar then
        Flags := Control.DrawTextBiDiModeFlags(AlignStyles[TCustomStatusBar(Control).Panels[I].Alignment] or DT_VCENTER)
      else
        Flags := Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER);
      Idx := I;
      SetLength(LText, Word(SendMessage(Handle, SB_GETTEXTLENGTH, Idx, 0)));
      if (Control is TCustomStatusBar) then
      begin
        if (TCustomStatusBar(Control).Panels[I].Style <> psOwnerDraw) and
           (TCustomStatusBar(Control).Panels[I].Text <> '')
        then
          DrawControlText(Canvas, Details,
          TCustomStatusBar(Control).Panels[I].Text, R, Flags)
        else
         if Assigned(TCustomStatusBar(Control).OnDrawPanel) then
         begin
           TCustomStatusBar(Control).Canvas.Lock;
           TCustomStatusBar(Control).Canvas.Handle := Canvas.Handle;
           try
             TCustomStatusBar(Control).OnDrawPanel(TCustomStatusBar(Control),
               TCustomStatusBar(Control).Panels[I], R);
           finally
             TCustomStatusBar(Control).Canvas.Handle := 0;
             TCustomStatusBar(Control).Canvas.Unlock;
           end;
         end;
      end
      else
      if Length(LText) > 0 then
      begin
        Res := SendMessage(Handle, SB_GETTEXT, Idx, IntPtr(@LText[1]));
        if (Res and SBT_OWNERDRAW = 0) then
          DrawControlText(Canvas, Details, LText, R, Flags)
        else
        if (Control is TCustomStatusBar) and
           Assigned(TCustomStatusBar(Control).OnDrawPanel) then
        begin
          TCustomStatusBar(Control).Canvas.Lock;
          TCustomStatusBar(Control).Canvas.Handle := Canvas.Handle;
          try
            TCustomStatusBar(Control).OnDrawPanel(TCustomStatusBar(Control),
              TCustomStatusBar(Control).Panels[I], R);
          finally
            TCustomStatusBar(Control).Canvas.Handle := 0;
            TCustomStatusBar(Control).Canvas.Unlock;
          end;
        end;
      end;
    end;
  end;
end;


constructor TscToolBarStyleHook.Create;
begin
  inherited;
  FImages := nil;
  OverridePaint := True;
  OverridePaintNC := True;
  DoubleBuffered := True;
  FHotButtonIndex := -1;
  FArrowDownButtonIndex := -1;
  FRuntimeThemesEnabled := TStyleManager.SystemStyle.Enabled;
end;

destructor TscToolBarStyleHook.Destroy;
begin
  inherited;
  if FImages <> nil then
    FreeAndNil(FImages);
end;

procedure TscToolBarStyleHook.MouseLeave;
begin
  if FHotButtonIndex <> -1 then
  begin
    FHotButtonIndex := -1;
    Invalidate;
  end;
end;

function TscToolBarStyleHook.GetButtonCount: Integer;
begin
  Result := SendMessage(Handle, TB_BUTTONCOUNT, 0, 0);
end;

function TscToolBarStyleHook.GetItemRect(Index: Integer): TRect;
begin
  Result := TRect.Empty;
  if not BOOL(SendMessage(Handle, TB_GETITEMRECT, Index, LPARAM(@Result))) then
    Result := TRect.Empty;
end;

function TscToolBarStyleHook.GetItemInfo(Index: Integer; Text: PChar; TextLen: Integer): TTBButtonInfo;
var
  TB: TTBButton;
begin
  FillChar(TB, SizeOf(TB), 0);
  SendMessage(Handle, TB_GETBUTTON, Index, IntPtr(@TB));
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(TTBButtonInfo);
  Result.dwMask := TBIF_STATE or TBIF_STYLE or TBIF_IMAGE or TBIF_TEXT;
  Result.cchText := TextLen;
  Result.pszText := Text;
  SendMessage(Handle, TB_GETBUTTONINFO, TB.idCommand, LPARAM(@Result));
  Result.fsStyle := TB.fsStyle;
  if not FRuntimeThemesEnabled then
    SendMessage(Handle, TB_GETBUTTONTEXT, TB.idCommand, LPARAM(Result.pszText));
end;

procedure TscToolBarStyleHook.ApplyImageList;
var
  H: HIMAGELIST;
begin
  H := HIMAGELIST(SendMessage(Handle, TB_GETIMAGELIST, 0, 0));
  if (H <> 0) and (FImages = nil) then
  begin
    FImages := TImageList.Create(nil);
    FImages.ShareImages := True;
    FImages.Handle := H;
  end;
end;

procedure TscToolBarStyleHook.WMSize(var Message: TWMSize);
begin
  if TToolBar(Control).EdgeBorders <> [] then InvalidateNC;
end;

procedure TscToolBarStyleHook.PaintNC(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  R: TRect;
  SaveIndex: Integer;
begin
  if TToolBar(Control).BorderWidth <> 0 then
  begin
    SaveIndex := SaveDC(Canvas.Handle);
    try
      R := Rect(0, 0, Control.Width, Control.Height);
      InflateRect(R, -TToolBar(Control).BorderWidth * 2, -TToolBar(Control).BorderWidth * 2);
      if ebLeft in TToolBar(Control).EdgeBorders then
        Inc(R.Left, 2);
      if ebTop in TToolBar(Control).EdgeBorders then
        Inc(R.Top, 2);
      if ebRight in TToolBar(Control).EdgeBorders then
        Dec(R.Right, 2);
      if ebBottom in TToolBar(Control).EdgeBorders then
        Dec(R.Bottom, 2);
      ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace);
      R := Rect(0, 0, Control.Width, Control.Height);
      Canvas.FillRect(R);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end;
  if TToolBar(Control).EdgeBorders <> [] then
  with Canvas do
  begin
    Details.Part := 0;
    Details.State := 0;
    Details.Element := teToolBar;
    R := Rect(0, 0, Control.Width, Control.Height);
    StyleServices.DrawEdge(Canvas.Handle, Details, R, [eeSunken], [efRect]);
    InflateRect(R, -1, -1);
    StyleServices.DrawEdge(Canvas.Handle, Details, R, [eeRaised], [efRect]);
  end;
end;

procedure TscToolBarStyleHook.Paint(Canvas: TCanvas);

  function DropDownWidth(AButtonIndex: Integer): Integer;
  var
    R: TRect;
  begin
    if BOOL(SendMessage(Handle, TB_GETITEMDROPDOWNRECT, AButtonIndex, LPARAM(@R))) then
      Result := R.Right - R.Left
    else
      Result := 15;
  end;

const
  BufSize = 255;
var
  Details, DropButtonDetails: TThemedElementDetails;
  R, R1, iRect, sRect: TRect;
  I: Integer;
  Info: TTBButtonInfo;
  ButtonState, DropButtonState: TThemedToolBar;
  ImageDrawed: Boolean;
  WStyle: Cardinal;
  LColor, LEndColor: TColor;
  DrawButtonFace: Boolean;
  Buffer: array [0..BufSize - 1] of Char;
  LStyle: TCustomStyleServices;
  LDropDownWidth: Integer;
begin
  LStyle := StyleServices;
  if not LStyle.Available then
    Exit;
  WStyle := GetWindowLong(Handle, GWL_STYLE);
  R := Rect(0, 0, Control.Width, Control.Height);
  {draw body}
  if TToolBar(Control).DrawingStyle = TTBdrawingStyle.dsNormal then
  begin
    Details.Element := teToolBar;
    Details.Part := 0;
    Details.State := 0;
    if LStyle.HasTransparentParts(Details) then
      LStyle.DrawParentBackground(Handle, Canvas.Handle, Details, False);
    LStyle.DrawElement(Canvas.Handle, Details, R);
  end
  else
  begin
    LColor := LStyle.GetStyleColor(scToolBarGradientBase);
    LEndColor := LStyle.GetStyleColor(scToolBarGradientEnd);
    GradientFillCanvas(Canvas, LColor, LEndColor, R, gdVertical);
  end;
  {draw buttons}
  ApplyImageList;
  for I := 0 to GetButtonCount - 1 do
  begin
    Info := GetItemInfo(I, @Buffer, BufSize);

    if Info.fsState and TBSTATE_HIDDEN = TBSTATE_HIDDEN then
      Continue;
    R := GetItemRect(I);

    if Info.fsStyle and BTNS_WHOLEDROPDOWN = BTNS_WHOLEDROPDOWN then
    begin
      ButtonState := ttbDropDownButtonNormal;
      if Info.fsState and TBSTATE_ENABLED = 0 then
        ButtonState := ttbDropDownButtonDisabled
      else if Info.fsState and TBSTATE_PRESSED = TBSTATE_PRESSED then
        ButtonState := ttbDropDownButtonPressed
      else if Info.fsState and TBSTATE_CHECKED = TBSTATE_CHECKED then
      begin
        if Info.fsState and TBSTATE_PRESSED = TBSTATE_PRESSED then
          ButtonState := ttbDropDownButtonCheckedHot
        else if (FHotButtonIndex = I) and MouseInControl then
          ButtonState := ttbDropDownButtonCheckedHot
        else
          ButtonState := ttbDropDownButtonChecked
      end
      else if (FHotButtonIndex = I) and MouseInControl then
        ButtonState := ttbDropDownButtonHot;
    end
    else
    begin
      ButtonState := ttbButtonNormal;
      if Info.fsState and TBSTATE_ENABLED = 0 then
        ButtonState := ttbButtonDisabled
      else if (Info.fsState and TBSTATE_PRESSED = TBSTATE_PRESSED) and not
              ((Info.fsStyle and TBSTYLE_DROPDOWN = TBSTYLE_DROPDOWN) and
              (FArrowDownButtonIndex = I)) then
        ButtonState := ttbButtonPressed
      else if Info.fsState and TBSTATE_CHECKED = TBSTATE_CHECKED then
      begin
        if Info.fsState and TBSTATE_PRESSED = TBSTATE_PRESSED then
          ButtonState := ttbButtonCheckedHot
        else if ((FHotButtonIndex = I) and MouseInControl) then
          ButtonState := ttbButtonCheckedHot
        else
          ButtonState := ttbButtonChecked
       end
       else if ((FHotButtonIndex = I) and MouseInControl)  or
               ((Info.fsStyle and TBSTYLE_DROPDOWN = TBSTYLE_DROPDOWN) and (FArrowDownButtonIndex = I)) then
         ButtonState := ttbButtonHot;
    end;

    DrawButtonFace := ((TToolBar(Control).DrawingStyle = TTBDrawingStyle.dsNormal) and not TToolBar(Control).Flat) or
      (Info.fsState and TBSTATE_PRESSED = TBSTATE_PRESSED) or
      (Info.fsState and TBSTATE_CHECKED = TBSTATE_CHECKED) or
      (MouseInControl and (FHotButtonIndex = I)) or (FArrowDownButtonIndex = I);

    { Face }
    Details := LStyle.GetElementDetails(ButtonState);
    ImageDrawed := True;
    if Info.fsStyle and TBSTYLE_CHECK = TBSTYLE_CHECK then
    begin
      if DrawButtonFace then
        LStyle.DrawElement(Canvas.Handle, Details, R);
    end
    else if Info.fsStyle and BTNS_WHOLEDROPDOWN = BTNS_WHOLEDROPDOWN then
      LStyle.DrawElement(Canvas.Handle, Details, R)
    else if Info.fsStyle and TBSTYLE_DROPDOWN = TBSTYLE_DROPDOWN then
    begin
      LDropDownWidth := DropDownWidth(I);
      R1 := R;
      R1.Right := R1.Right - LDropDownWidth;
      if DrawButtonFace then
        LStyle.DrawElement(Canvas.Handle, Details, R1);
      R1 := R;
      R1.Left := R1.Right - LDropDownWidth;

      if (FArrowDownButtonIndex = I) or (ButtonState = ttbButtonPressed) then
        DropButtonState := ttbSplitButtonDropDownPressed
      else if ButtonState = ttbButtonDisabled then
        DropButtonState := ttbSplitButtonDropDownDisabled
      else if ButtonState = ttbButtonHot then
        DropButtonState := ttbSplitButtonDropDownHot
      else
      if DrawButtonFace then
        DropButtonState := ttbSplitButtonDropDownNormal
      else
        DropButtonState := ttbDropDownButtonGlyphNormal;

      if TToolBar(Control).DrawingStyle = dsGradient then
        case DropButtonState of
          ttbSplitButtonDropDownNormal,
          ttbSplitButtonDropDownDisabled,
          ttbButtonDisabled:
            begin
              DropButtonState := ttbDropDownButtonGlyphNormal;
              R1.Offset(-1, 0);
            end;
        end;

      DropButtonDetails := LStyle.GetElementDetails(DropButtonState);
      LStyle.DrawElement(Canvas.Handle, DropButtonDetails, R1);
      R.Right := R.Right - LDropDownWidth;
      if DropButtonState = ttbDropDownButtonGlyphNormal then
        R1.Offset(1, 0);
    end
    else if Info.fsStyle and TBSTYLE_SEP = TBSTYLE_SEP then
    begin
      R1 := R;
      if TStyleManager.SystemStyle.Enabled then
        R1.Right := R1.Left + 4;
      if (Control is TToolBar) and not (TToolBar(Control).Wrapable) and
         (TToolBar(Control).Buttons[I].Wrap) then
      begin
        R1 := Rect(0, R1.CenterPoint.Y, Control.ClientWidth, R1.CenterPoint.Y + 2);
        Frame3D(Canvas, R1,
          StyleServices.GetSystemColor(clBtnShadow), StyleServices.GetSystemColor(clBtnHighLight), 1);
      end
      else
      begin
        Details := LStyle.GetElementDetails(ttbSeparatorNormal);
        LStyle.DrawElement(Canvas.Handle, Details, R1);
      end;
      ImageDrawed := False;
    end
    else if DrawButtonFace then
      LStyle.DrawElement(Canvas.Handle, Details, R);

    { Text and Image }
    if FImages <> nil then
    begin
      iRect := Rect(0, 0, FImages.Width, FImages.Height);
      RectCenter(iRect, R);
    end
    else
      iRect := Rect(0, 0, 0, 0);

    if (StrLen(Info.pszText) > 0) and not (Info.fsStyle and TBSTYLE_SEP = TBSTYLE_SEP) and
       (not (Control is TToolBar) or
       ((Control is TToolBar) and
       ((TToolBar(Control).ShowCaptions) or (TToolBar(Control).AllowTextButtons and
       (TToolBar(Control).Buttons[I].Style = tbsTextButton))))) then
    begin
      if (WStyle and TBSTYLE_LIST = TBSTYLE_LIST) then
      begin
        sRect := R;
        Canvas.Font := TToolBar(Control).Font;
        DrawText(Canvas.Handle, PChar(Info.pszText),
          Length(Info.pszText), sRect, DT_CENTER or DT_WORDBREAK or DT_CALCRECT);
        RectCenter(sRect, R);
        if ImageDrawed and (Info.iImage >= 0) and (FImages <> nil) then
        begin
          iRect := Rect(5, iRect.Top, FImages.Width + 5, iRect.Bottom);
          OffsetRect(iRect, R.Left, 0);
          sRect := Rect(iRect.Right + 3, sRect.Top, R.Right, sRect.Bottom);
        end;
        if DrawButtonFace then
          DrawControlText(Canvas, Details, Info.pszText, sRect, DT_LEFT or DT_WORDBREAK)
        else
        begin
          Canvas.Font.Color := StyleServices.GetSystemColor(clBtnText);
          Canvas.Brush.Style := bsClear;
          DrawText(Canvas.Handle, Info.pszText, Length(Info.pszText), sRect,
            DT_LEFT or DT_WORDBREAK);
        end;
      end
      else
      begin
        Canvas.Font := TToolBar(Control).Font;
        sRect := R;
        DrawText(Canvas.Handle, PChar(Info.pszText),
          Length(Info.pszText), sRect, DT_CENTER or DT_WORDBREAK or DT_CALCRECT);
        RectCenter(sRect, R);
        if ImageDrawed and (Info.iImage >= 0) and (FImages <> nil) then
        begin
          OffsetRect(sRect, 0, iRect.Height div 2);
          OffsetRect(iRect, 0, -sRect.Height div 2);
        end;
        if DrawButtonFace then
          DrawControlText(Canvas, Details, Info.pszText, sRect, DT_CENTER or DT_WORDBREAK)
        else
        begin
          Canvas.Font.Color := StyleServices.GetSystemColor(clBtnText);
          Canvas.Brush.Style := bsClear;
          DrawText(Canvas.Handle, Info.pszText, Length(Info.pszText), sRect,
            DT_CENTER or DT_WORDBREAK);
        end;
      end;
    end;

    if ImageDrawed and (Info.iImage >= 0) and (FImages <> nil) then
    begin
      if Control is TToolBar then
      begin
        if (Info.fsState and TBSTATE_ENABLED = 0) and (TToolBar(Control).DisabledImages <> nil) and
           (Info.iImage < TToolBar(Control).DisabledImages.Count) then
          TToolBar(Control).DisabledImages.Draw(Canvas, iRect.Left, iRect.Top, Info.iImage)
        else
        if (FHotButtonIndex = I) and (TToolBar(Control).HotImages <> nil) and
           (Info.iImage < TToolBar(Control).HotImages.Count) then
          TToolBar(Control).HotImages.Draw(Canvas, iRect.Left, iRect.Top, Info.iImage)
        else
          TToolBar(Control).Images.Draw(Canvas, iRect.Left, iRect.Top, Info.iImage,
            Info.fsState and TBSTATE_ENABLED = TBSTATE_ENABLED);
      end
      else
        ImageList_Draw(FImages.Handle, Info.iImage, Canvas.Handle, iRect.Left, iRect.Top, ILD_TRANSPARENT);
    end;
  end;

end;

procedure TscToolBarStyleHook.WMNotify(var Message: TWMNotify);
begin
  if (Message.NMHdr.Code = TBN_DROPDOWN) and (LongWord(Message.IDCtrl) = Handle) then
  begin
    FArrowDownButtonIndex := PNMToolBar(Message.NMHdr).iItem;
    Invalidate;
    FHotButtonIndex := -1;
    CallDefaultProc(TMessage(Message));
    FArrowDownButtonIndex := -1;
    Invalidate;
    Handled := True;
  end;
end;

procedure TscToolBarStyleHook.WMMouseMove(var Message: TWMMouse);
const
  BufSize = 255;
var
  I: Integer;
  Info: TTBButtonInfo;
  Buffer: array [0..BufSize - 1] of Char;
begin
  inherited;
  for I := 0 to GetButtonCount - 1 do
  begin
    if GetItemRect(I).Contains(Point(Message.XPos, Message.YPos)) then
    begin
      Info := GetItemInfo(I, @Buffer, BufSize);
      if Info.fsState and TBSTATE_ENABLED = TBSTATE_ENABLED then
      begin
        if FHotButtonIndex <> I then
          Invalidate;
        FHotButtonIndex := I;
        Exit;
      end;
    end;
  end;
  if FHotButtonIndex >= 0 then
  begin
    FHotButtonIndex := -1;
    Invalidate;
  end;
end;


procedure TscToolBarStyleHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  Buffer: TBitmap;
  FCanvas: TCanvas;
begin
  if FFromWMPaint then
  begin
    Message.Result := 1;
    Handled := True;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  FCanvas.Handle := Message.DC;
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Control.Width, Control.Height);
    Paint(Buffer.Canvas);
    FCanvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
  Message.Result := 1;
  Handled := True;
  inherited;
end;

procedure TscToolBarStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_PAINT:
      FFromWMPaint := True;
  end;
  inherited;
  case Message.Msg of
    WM_PAINT:
      FFromWMPaint := False;
    TB_SETIMAGELIST:
      if FImages <> nil then
      begin
        FreeAndNil(FImages);
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE);
      end;
  end;
end;

function TscRichEditStyleHook.CanApplyStyleColors: Boolean;
begin
  if Control is TscRichEdit then
    Result := TscRichEdit(Control).StyleColors
  else
  begin
    {$IFNDEF VER230}
    Result := seClient in Control.StyleElements;
    {$ELSE}
    Result := True;
    {$ENDIF}
  end;
end;

procedure TscRichEditStyleHook.EMSetBkgndColor(var Message: TMessage);
begin
  if CanApplyStyleColors then
  begin
    {$IFNDEF VER230}
    if seClient in Control.StyleElements then
    {$ENDIF}
    Message.LParam := ColorToRGB(StyleServices.GetStyleColor(scEdit));
    Handled := False;
  end;
end;

procedure TscRichEditStyleHook.EMSetCharFormat(var Message: TMessage);
type
  PCharFormat2 = ^TCharFormat2;
const
  TextColor: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
  BkColor: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
var
  Format: PCharFormat2;
begin
  Format := PCharFormat2(Message.LParam);
  if CanApplyStyleColors and
     (Format.dwMask and CFM_COLOR = CFM_COLOR) then
  begin
    {$IFNDEF VER230}
    if seFont in Control.StyleElements then
    {$ENDIF}
    Format.crTextColor := ColorToRGB(StyleServices.GetStyleFontColor(TextColor[Control.Enabled]));
    Format.crBackColor := ColorToRGB(StyleServices.GetStyleColor(BkColor[Control.Enabled]));
    Format.dwEffects := Format.dwEffects and not CFE_AUTOCOLOR;
  end;
  Handled := False;
end;

procedure TscRichEditStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_WINDOWPOSCHANGED:
    begin
      if TWMWindowPosChanged(Message).WindowPos^.flags and SWP_NOSIZE = 0 then
        PaintScroll;
    end;
    CN_NOTIFY:
      begin
        if TWMNotifyRE(Message).NMHdr.code = EN_SELCHANGE then
          PaintScroll;
      end;
  end;
end;

constructor TscRichEdit.Create(AOwner: TComponent);
begin
  inherited;
  FStyleColors := True;
end;

procedure TscRichEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    WindowClass.style := WindowClass.style or CS_HREDRAW or CS_VREDRAW;
end;

procedure TscRichEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TscRichEdit.WndProc(var Message: TMessage);
var
  Format: TCharFormat2;
begin
  inherited;
  case Message.Msg of
    CM_STYLECHANGED:
    if not (csLoading in ComponentState) then
    begin
      FillChar(Format, SizeOf(Format), 0);
      Format.cbSize := SizeOf(Format);
      Format.dwMask := CFM_COLOR;
      Format.crTextColor := ColorToRGB(Font.Color);
      Perform(EM_SETCHARFORMAT, SCF_ALL, LongInt(@Format));
    end;
  end;
end;

constructor TscListBoxStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
  UpdateColors;
end;

type
  TCustomListBoxClass = class(TCustomListBox);

procedure TscListBoxStyleHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);

procedure PaintBG(DC: HDC);
var
  FCanvas: TCanvas;
  I, J, W, H: Integer;
  R: TRect;
  DrawCount, ItemHeight: Integer;
begin
  ItemHeight := TCustomListBoxClass(Control).ItemHeight;
  J := 0;
  W := Control.ClientWidth;
  H := Control.ClientHeight;
  if (TCustomListBox(Control).Count > 0) and (ItemHeight > 0) then
  begin
    DrawCount := H div ItemHeight;
    I := TCustomListBox(Control).TopIndex;
    J := I + DrawCount;
    if J > TCustomListBox(Control).Count - 1 then
      J := TCustomListBox(Control).Count - 1;
    J := J - TCustomListBox(Control).TopIndex + 1;
    if J < 0 then J := 0;
  end
  else
    ItemHeight := 0;
  R := Rect(0, j * ItemHeight, W, H);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := DC;
  try
    FCanvas.Brush.Color := Self.Brush.Color;
    FCanvas.FillRect(R);
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
  end;
end;

begin
  if ((Control is TCustomListBox) and (TCustomListBoxClass(Control).Columns <> 0)) or
     ((Control is TCheckListBox) and (TCheckListBox(Control).Columns <> 0))
  then
  begin
    inherited;
  end
  else
  begin
    PaintBG(Message.DC);
    Message.Result := 1;
    Handled := True;
  end;
end;

procedure TscListBoxStyleHook.WMSetFocus(var Message: TMessage);
begin
  inherited;
  CallDefaultProc(Message);
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  Handled := True;
end;

procedure TscListBoxStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
        Handled := True;
      end;
    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        Handled := False;
      end
  else
    inherited WndProc(Message);
  end;
end;

procedure TscListBoxStyleHook.UpdateColors;
const
  ColorStates: array[Boolean] of TStyleColor = (scListBoxDisabled, scListBox);
  FontColorStates: array[Boolean] of TStyleFont = (sfListItemTextDisabled, sfListItemTextNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
  {$IFNDEF VER230}
  if seFont in Control.StyleElements then
    FontColor := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled])
  else
    FontColor := TWinControlClass(Control).Font.Color;
  {$ELSE}
  FontColor := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled]);
  {$ENDIF}
end;

procedure TscListBoxStyleHook.WMKillFocus(var Message: TMessage);
begin
  inherited;
  CallDefaultProc(Message);
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  Handled := True;
end;

constructor TscTrackEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDblClickShowTrackBar := False;
  FSupportUpdownKeys := True;
  FIncrement := 1;
  FPopupKind := sctbpRight;
  FTrackBarWidth := 0;
  FTrackBarHeight := 0;
  FRightButton.Visible := True;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  StopCheck := True;
  Text := '0';
  StopCheck := False;
  FromEdit := False;
  OnRightButtonClick := ButtonClick;
  FPopupTrackBar := TscPopupTrackBar.Create(Self);
  FPopupTrackBar.Visible := False;
  FPopupTrackBar.TrackEdit := Self;
  FPopupTrackBar.Parent := Self;
  FPopupTrackBar.OnChange := TrackBarChange;
end;

destructor TscTrackEdit.Destroy;
begin
  FPopupTrackBar.Free;
  inherited;
end;

function TscTrackEdit.CanScaleButtons: Boolean;
begin
  Result := True;
end;

procedure TscTrackEdit.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  if FTrackBarWidth <> 0 then
    FTrackBarWidth := MulDiv(FTrackBarWidth, M, D);
  if FTrackBarHeight <> 0 then
    FTrackBarHeight := MulDiv(FTrackBarHeight, M, D);
end;

function TscTrackEdit.GetJumpWhenClick: Boolean;
begin
  Result := FPopupTrackBar.JumpWhenClick;
end;

procedure TscTrackEdit.SetJumpWhenClick(Value: Boolean);
begin
  FPopupTrackBar.JumpWhenClick := Value;
end;

procedure TscTrackEdit.WMMOUSEWHEEL;
begin
  if not FPopupTrackBar.Visible
  then
    begin
      if TWMMOUSEWHEEL(Message).WheelDelta > 0
      then
        Value := Value - FIncrement
      else
        Value := Value + FIncrement;
    end
  else
    begin
      if TWMMOUSEWHEEL(Message).WheelDelta > 0
      then
        FPopupTrackBar.Value := FPopupTrackBar.Value - FIncrement
      else
        FPopupTrackBar.Value := FPopupTrackBar.Value + FIncrement;
    end;
end;


procedure TscTrackEdit.CMCancelMode(var Message: TCMCancelMode);
begin
 if (Message.Sender <> FPopupTrackBar) then CloseUp;
end;

procedure TscTrackEdit.CloseUp;
begin
  if FPopupTrackbar.Visible
  then
    begin
      SetWindowPos(FPopupTrackBar.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
                   SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
      FPopupTrackBar.Visible := False;
    end;
end;

procedure TscTrackEdit.DropDown;
var
  P: TPoint;
  Y: Integer;
  R: TRect;
begin
  with FPopupTrackBar do
  begin
    if FTrackBarWidth = 0
    then
      Width := Self.Width
    else
      Width := FTrackBarWidth;
    if FTrackBarHeight = 0
    then
    begin
      if IsCustomStyle then
        Height := Round(Self.Height + 10 * Self.FScaleFactor)
      else
        Height := Round(Self.Height + 5 * Self.FScaleFactor);
    end
    else
      Height := FTrackBarHeight;
    MinValue := Self.MinValue;
    MaxValue := Self.MaxValue;
    Value := Self.Value;
  end;

  if (PopupKind = sctbpRight) or (FPopupTrackBar.Width = Self.Width)
  then
    P := Parent.ClientToScreen(Point(Left, Top))
  else
    P := Parent.ClientToScreen(Point(Left + Width - FPopupTrackBar.Width, Top));

  Y := P.Y + Height;

  if (Screen.ActiveCustomForm <> nil)
  then
    R := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).WorkAreaRect
  else
  if (Application.MainForm <> nil) and (Application.MainForm.Visible)
  then
    R := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkAreaRect
  else
    R := Screen.WorkAreaRect;

  if P.X + FPopupTrackBar.Width > R.Right
  then
    P.X := P.X - ((P.X + FPopupTrackBar.Width) - R.Right)
  else
  if P.X < R.Left then P.X := R.Left;

  if Y + FPopupTrackBar.Height > R.Bottom
  then
    Y := P.Y - FPopupTrackBar.Height;

  SetWindowPos(FPopupTrackBar.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);

  FPopupTrackBar.Visible := True;
end;

procedure TscTrackEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  if FDblClickShowTrackBar then
    if not FPopupTrackBar.Visible then DropDown;
end;

procedure TscTrackEdit.ButtonClick(Sender: TObject);
begin
  if not FPopupTrackBar.Visible then DropDown else CloseUp;
end;

function TscTrackEdit.CheckValue;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue)
  then
    begin
      if NewValue < FMinValue then
      Result := FMinValue
      else if NewValue > FMaxValue then
      Result := FMaxValue;
    end;
end;

procedure TscTrackEdit.SetMinValue;
begin
  FMinValue := AValue;
end;

procedure TscTrackEdit.SetMaxValue;
begin
  FMaxValue := AValue;
end;

function TscTrackEdit.IsNumText;

function GetMinus: Boolean;
var
  i: Integer;
  S: String;
begin
  S := AText;
  i := Pos('-', S);
  if i > 1
  then
    Result := False
  else
    begin
      Delete(S, i, 1);
      Result := Pos('-', S) = 0;
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
end;

procedure TscTrackEdit.Change;
var
  NewValue, TmpValue: Integer;

function CheckInput: Boolean;
begin
  if (NewValue < 0) and (TmpValue < 0)
  then
    Result := NewValue > TmpValue
  else
    Result := NewValue < TmpValue;

  if not Result and ( ((FMinValue > 0) and (TmpValue < 0))
    or ((FMinValue < 0) and (TmpValue > 0)))
  then
    Result := True;
end;

begin
  if FromEdit then Exit;
  if not StopCheck and IsNumText(Text)
  then
    begin
      TmpValue := StrToInt(Text);
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
           Text := IntToStr(Round(NewValue));
           FromEdit := False;
        end;
    end;
  inherited;
end;

procedure TscTrackEdit.SetValue;
begin
  FValue := CheckValue(AValue);
  StopCheck := True;
  Text := IntToStr(Round(CheckValue(AValue)));
  if FPopupTrackBar.Visible then FPopupTrackBar.Value := FValue;
  StopCheck := False;
end;

procedure TscTrackEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FSupportUpDownKeys then
  if not FPopupTrackBar.Visible
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
  else
    begin
       if Key = VK_UP
      then
        FPopupTrackBar.Value := FPopupTrackBar.Value + FIncrement
      else
      if Key = VK_DOWN
      then
        FPopupTrackBar.Value := FPopupTrackBar.Value - FIncrement
    end;
end;

procedure TscTrackEdit.WMCHAR(var Message: TWMCHAR);
begin
  if Message.CharCode in [VK_ESCAPE] then
  begin
    if FPopupTrackBar.Visible then CloseUp;
  end;
  inherited;
end;

procedure TscTrackEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  inherited KeyPress(Key);
end;

function TscTrackEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := CharInSet(Key, ['-', '0'..'9']) or
            ((Key < #32) and (Key <> Chr(VK_RETURN)));

  if (Key = '-') and (Pos('-', Text) <> 0)
  then
    Result := False;

  if ReadOnly and Result and ((Key >= #32) or
     (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)))
  then
    Result := False;
end;

procedure TscTrackEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp;
  StopCheck := True;
  Text := IntToStr(FValue);
  StopCheck := False;
end;

procedure TscTrackEdit.TrackBarChange(Sender: TObject);
begin
  if Value <> FPopupTrackBar.Value
  then
    Value := FPopupTrackBar.Value;
end;

constructor TscPopupTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FTransparentBackground := False;
  FCanFocused := False;
  Color := clBtnFace;
end;

procedure TscPopupTrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
  end;
end;

procedure TscPopupTrackBar.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscPopupTrackBar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  C: TColor;
  R: TRect;
begin
  inherited;
  C := GetStyleCOlor(clBtnShadow);
  R := Rect(0, 0, Width, Height);
  Frm3D(ACanvas, R, C, C);
end;

{ TscCoolBarStyleHook }

constructor TscCoolBarStyleHook.Create;
begin
  inherited;
  OverridePaint := True;
  OverridePaintNC := True;
end;

procedure TscCoolBarStyleHook.PaintNC(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
begin
  if not StyleServices.Available then Exit;
  if (Control is TCoolBar) and (TCoolBar(Control).EdgeBorders = []) then Exit;

  ExcludeClipRect(Canvas.Handle,
    2, 2, Control.Width - 2, Control.Height - 2);
  Canvas.Brush.Color := StyleServices.ColorToRGB(clBtnFace);
  Canvas.FillRect(Rect(0, 0, Control.Width, Control.Height));
  Details.Element := teToolBar;
  Details.Part := 0;
  StyleServices.DrawElement(Canvas.Handle,
    Details, Rect(0, 0, Control.Width, Control.Height));
end;

procedure TscCoolBarStyleHook.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
  Handled := True;
end;

function TscCoolBarStyleHook.GetBandCount: Integer;
begin
  Result := SendMessage(Handle, RB_GETBANDCOUNT, 0, 0);
end;

function TscCoolBarStyleHook.GetBandText(Index: Integer): string;
begin
  if (Control is TCoolBar) and (Index < TCoolBar(Control).Bands.Count) then
    Result := TCoolBar(Control).Bands[Index].Text
  else
    Result := '';
end;

function TscCoolBarStyleHook.GetBandRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  SendMessage(Handle, RB_GETRECT, Index, IntPtr(@Result));
  if (Control is TCoolBar) and TCoolBar(Control).Vertical then
  with TCoolBar(Control)  do
  begin
    Result.Top := Bands[Index].Control.Top;
    Result.Left :=  Bands[Index].Control.Left;
    Result.Bottom := Result.Top + Bands[Index].Control.Height;
    Result.Right := Result.Left + Bands[Index].Control.Width;
    Result.Top := Result.Top - GetBandBorder(Index).Top + 4;
  end;
end;

function TscCoolBarStyleHook.GetBandBorder(Index: Integer): TRect;
begin
  SendMessage(Handle, RB_GETBANDBORDERS, Index, IntPtr(@Result));
end;

procedure TscCoolBarStyleHook.Paint(Canvas: TCanvas);
var
  I, IX, IY: integer;
  R, Margin, LTextRect: TRect;
  S: string;
  Details: TThemedElementDetails;
begin
  { Background }
  R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
  InflateRect(R, 2, 2);
  Details.Element := teToolBar;
  Details.Part := 0;
  Canvas.Brush.Color := GetStyleColor(clBtnFace);
  Canvas.FillRect(R);
  { Bands }
  for I := 0 to GetBandCount - 1 do
  begin
    R := GetBandRect(I);
    Margin := GetBandBorder(I);
    InflateRect(R, 1, 1);
    if R.Top < 0 then R.Top := 0;
    if R.Left < 0 then R.Left := 0;
    if R.Right > Control.ClientRect.Right then
      R.Right := Control.ClientRect.Right;
    if R.Bottom > Control.ClientRect.Bottom then
      R.Bottom := Control.ClientRect.Bottom;
    {draw text and image}
    if (Control is TCoolBar) and TCoolBar(Control).Vertical then
      LTextRect := Rect(R.Left, R.Top, R.Right, R.Top + Margin.Top)
    else
      LTextRect := Rect(R.Left + 10, R.Top, R.Left + Margin.Left, R.Bottom);

    if Control is TCoolBar then
      with TCoolBar(Control) do
      begin
        if (Images <> nil) and (Bands[I].ImageIndex < Images.Count) and
           (Bands[I].ImageIndex >= 0) then
        begin
          if not Vertical then
          begin
            IX := LTextRect.Left;
            IY := LTextRect.Top + LTextRect.Height div 2 - Images.Height div 2;
            Inc(LTextRect.Left, Images.Width + 3);
          end
          else
          begin
            IX := LTextRect.Left + LTextRect.Width div 2 - Images.Width div 2;
            IY := LTextRect.Top + 10;
            Inc(LTextRect.Top, Images.Height + 5);
          end;
          ImageList_Draw(Images.Handle, Bands[I].ImageIndex, Canvas.Handle,
            IX, IY, ILD_TRANSPARENT);
        end;
      end;

    S := GetBandText(I);
    if S <> '' then
    begin
      Canvas.Font := TWinControlClass(Control).Font;
      Canvas.Font.Color := ColorToRGB(GetStyleColor(clBtnText));
      Canvas.Brush.Style := bsClear;
      scDrawText(Canvas, S, LTextRect, False, False);
    end;

    {draw gripper}
    if (Control is TCoolBar) and TCoolBar(Control).Vertical then
    begin
      if RBS_BANDBORDERS and GetWindowLong(Handle, GWL_STYLE) = 0 then
        R := Rect(R.Left, R.Top, R.Right, R.Top + 4)
      else
        R := Rect(R.Left, R.Top + 3, R.Right, R.Top + 7);
      Details := StyleServices.GetElementDetails(trGripperVert);
    end
    else
    begin
      R := Rect(R.Left + 2, R.Top + 2, R.Left + 6, R.Bottom - 2);
      Details := StyleServices.GetElementDetails(trGripper);
    end;
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
end;

procedure TscCoolBarStyleHook.WMSize(var Message: TMessage);
begin
  CallDefaultProc(Message);
  Invalidate;
  Handled := True;
end;

procedure TscCoolBarStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
end;

constructor TscCoolBar.Create(AOwner: TComponent);
begin
  inherited;
  BevelEdges := [];
  BandBorderStyle := bsNone;
  EdgeBorders := [];
  EdgeInner := esNone;
  EdgeOuter := esNone;
end;

destructor TscCoolBar.Destroy;
begin
  inherited;
end;

procedure TscCoolBar.WMSIZE(var Msg: TMessage);
begin
  inherited;
  if not IsCustomStyle then
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW or
      RDW_ALLCHILDREN);
end;


constructor TscCustomScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FFluentUIOpaque := False;
  FWallpapers := nil;
  FPaintBuffer := nil;
  FWallpaperIndex := -1;
  FCustomImages := nil;
  FCustomBackgroundImageIndex := -1;
  HorzScrollBar.Tracking := True;
  VertScrollBar.Tracking := True;
  FIgnoreAutoScroll := False;
  FFullUpdate := True;
  FStorePaintBuffer := False;
end;

destructor TscCustomScrollBox.Destroy;
begin
  if FPaintBuffer <> nil then
  begin
    FPaintBuffer.Free;
    FPaintBuffer := nil;
  end;
  inherited;
end;

procedure TscCustomScrollBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if IsFluentUIOpaque then
      ExStyle := Exstyle or WS_EX_LAYERED;
  end;
end;

procedure TscCustomScrollBox.CreateWnd;
begin
  inherited;
  if IsFluentUIOpaque then
  begin
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
    if Visible then
      SetTimer(Handle, 101, 10, nil);
  end;
end;

function TscCustomScrollBox.IsFluentUIOpaque: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FFluentUIOpaque and IsWindows10;
end;

procedure TscCustomScrollBox.SetFluentUIOpaque(Value: Boolean);
begin
  if FFluentUIOpaque <> Value then
  begin
    FFluentUIOpaque := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
       not (csDestroying in ComponentState) and IsWindows10{$IFDEF VER300_UP} and (TOSVersion.Build >= 17134) {$ENDIF} then
      Perform(CM_RECREATEWND, 0, 0);
  end;
end;

procedure TscCustomScrollBox.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
  if not (csLoading in ComponentState) and Visible then
    SetTimer(Handle, 100, 10, nil);
end;

procedure TscCustomScrollBox.SetCustomBackgroundImageIndex(Value: Integer);
begin
  if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.AutoScrollInView(AControl: TControl);
var
  P, P1: TPoint;
  R, R1: TRect;
begin
  if (not AutoScroll and not FIgnoreAutoScroll) or not FFullUpdate then
  begin
    inherited;
    Exit;
  end;
  P := Point(0, 0);
  P := AControl.ClientToScreen(P);
  P := ScreenToClient(P);
  R1 := AControl.ClientRect;
  P1 := Point(P.X + R1.Width, P.Y + R1.Height);
  R := ClientRect;
  InflateRect(R, 2, 2);
  if R.Contains(P) and R.Contains(P1) then
  begin
    inherited;
    Exit;
  end;
  if (AControl <> nil) and not (csLoading in AControl.ComponentState) and
    not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
    Perform(WM_SETREDRAW, 0, 0);
  inherited;
  if (AControl <> nil) and not (csLoading in AControl.ComponentState) and
    not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
  begin
    Perform(WM_SETREDRAW, 1, 0);
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.ScrollInView(AControl: TControl);
var
  P, P1: TPoint;
  R, R1: TRect;
begin
  if (not AutoScroll and not FIgnoreAutoScroll) or not FFullUpdate then
  begin
    inherited;
    Exit;
  end;
  P := Point(0, 0);
  P := AControl.ClientToScreen(P);
  P := ScreenToClient(P);
  R1 := AControl.ClientRect;
  P1 := Point(P.X + R1.Width, P.Y + R1.Height);
  R := ClientRect;
  InflateRect(R, 2, 2);
  if R.Contains(P) and R.Contains(P1) then
  begin
    inherited;
    Exit;
  end;
  if (AControl <> nil) and not (csLoading in AControl.ComponentState) and
    not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
  Perform(WM_SETREDRAW, 0, 0);
  inherited;
  if (AControl <> nil) and not (csLoading in AControl.ComponentState) and
    not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
  begin
    Perform(WM_SETREDRAW, 1, 0);
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.ScrollBy(DeltaX, DeltaY: Integer);
begin
  if FFullUpdate then
  begin
    Perform(WM_SETREDRAW, 0, 0);
  end;
  inherited;
  if FFullUpdate then
  begin
    Perform(WM_SETREDRAW, 1, 0);
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.FullRedraw;
begin
  if Parent = nil then Exit;
  if not HandleAllocated then Exit;
  RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or
     RDW_ALLCHILDREN or RDW_UPDATENOW);
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TscCustomScrollBox.SetBackgroundStyle(Value: TscsbBackgroundStyle);
begin
  if FBackgroundStyle <> Value then
  begin
    FBackgroundStyle := Value;
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    FullRedraw;
  end;
end;

procedure TscCustomScrollBox.WndProc(var Message: TMessage);
var
  FCanvas: TCanvas;
  R, R1, R2: TRect;
  Buffer: TBitmap;
  FStopDraw: Boolean;
begin
  FStopDraw := False;
  case Message.Msg of
    WM_SIZE:
    if FFullUpdate and Visible then
    begin
      if ((FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex)) or
         ((FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustombackgroundImageIndex)) or
         ((FBackgroundStyle <> scsbsPanel) and IsCustomStyle and
          ((GetScrollPos(Handle, SB_HORZ) > 0) or (GetScrollPos(Handle, SB_VERT) > 0)))
      then
      begin
        FStopDraw := True;
        Perform(WM_SETREDRAW, 0, 0);
      end;
    end;
    WM_HSCROLL, WM_VSCROLL, 45138:
    if FFullUpdate and (AutoScroll or FIgnoreAutoScroll) and Visible then
    begin
      FStopDraw := True;
      Perform(WM_SETREDRAW, 0, 0);
    end;
    WM_ERASEBKGND:
    begin
      FCanvas := TCanvas.Create;
      FCanvas.Handle := TWMERASEBKGND(Message).DC;
      try
        R := ClientRect;
        if not FStorePaintBuffer and (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) and
            FWallpapers.IsSolidDrawing(FWallpaperIndex)
        then
        begin
          FWallpapers.Draw(FCanvas, R, FWallpaperIndex, FScaleFactor);
          if (FCustomImages <> nil) and
             FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
          then
            FCustomImages.Draw(FCanvas, R, FCustomBackgroundImageIndex, FScaleFactor);
        end
        else
        if (R.Width * R.Height > 0) and not FFullUpdate and not FStorePaintBuffer and
           ((FBackgroundStyle = scsbsPanel) or not AutoScroll) then
        begin
          R1 := Rect(0, 0, R.Width, R.Height);
          case Self.BackgroundStyle of
            scsbsFormBackground:
              begin
                DrawFormBackground(FCanvas, R1);
              end;
            scsbsPanel:
              begin
                FCanvas.Brush.Color := GetStyleColor(Self.Color);
                FCanvas.FillRect(R1);
              end;
            scsbsTabSheet:
              begin
                R2 := R1;
                InflateRect(R2, 4, 4);
                scDrawUtils.DrawTabFrame(FCanvas, R2);
              end;
          end;
        end
        else
        if (R.Width * R.Height > 0) then
        begin
          if FStorePaintBuffer then
          begin
            if FPaintBuffer = nil then
              FPaintBuffer := TBitmap.Create;
          end;
          if FStorePaintBuffer then
            Buffer := FPaintBuffer
          else
            Buffer := TBitmap.Create;
          try
            Buffer.SetSize(R.Width, R.Height);
            R1 := Rect(0, 0, R.Width, R.Height);
            if FBackgroundStyle = scsbsFormBackground then
              DrawFormBackground(Buffer.Canvas, R1)
            else
            if FBackgroundStyle = scsbsTabSheet then
            begin
              R2 := R1;
              InflateRect(R2, 4, 4);
              scDrawUtils.DrawTabFrame(Buffer.Canvas, R2);
            end
            else
            with Buffer.Canvas do
            begin
              Brush.Color := GetStyleColor(Self.Color);
              FillRect(R1);
            end;
            if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
            then
              FCustomImages.Draw(Buffer.Canvas, R1, FCustomBackgroundImageIndex, FScaleFactor);
            if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex)
            then
              FWallpapers.Draw(Buffer.Canvas, R1, FWallpaperIndex, FScaleFactor);
            FCanvas.Draw(R.Left, R.Top, Buffer);
          finally
            if not FStorePaintBuffer then
              Buffer.Free;
          end;
        end;
      finally
        FCanvas.Handle := 0;
        FCanvas.Free;
      end;
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_SIZE, WM_VSCROLL, WM_HSCROLL, 45138:
    if FStopDraw then
    begin
      Perform(WM_SETREDRAW, 1, 0);
      FullRedraw;
    end;
    WM_TIMER:
    if TWMTimer(Message).TimerID = 100 then
    begin
      KillTimer(Handle, 100);
      FullReDraw;
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

procedure TscCustomScrollBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
   FCustomImages := nil;
end;


{ TscScrollBoxStyleHook }

type
  TWinControlClassHook = class(TWinControl);

constructor TscScrollBoxStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
  {$IFNDEF VER230}
  if seClient in Control.StyleElements then
    Brush.Color := StyleServices.GetStyleColor(scPanel)
  else
    Brush.Color :=  TWinControlClassHook(Control).Color;
  {$ELSE}
    Brush.Color := StyleServices.GetStyleColor(scPanel);
  {$ENDIF}
end;

procedure TscScrollBoxStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_VSCROLL, WM_HSCROLL:
     if TWMScroll(Message).ScrollCode = SB_ENDSCROLL then
       SetWindowPos(Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
         SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;


initialization

  {$IFDEF REPLACEDEFSTYLEHOOKS}
  TCustomStyleEngine.UnRegisterStyleHook(TCoolBar, TCoolBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TCoolBar, TscCoolBarStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TToolBar, TToolBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TToolBar, TscToolBarStyleHook);
  {$ENDIF}

  TCustomStyleEngine.RegisterStyleHook(TscStatusBar, TscStatusBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscCoolBar, TscCoolBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscToolBar, TscToolBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscMemo, TscScrollingStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscCustomListBox, TscListBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscListBox, TscListBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscCheckListBox, TscListBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscColorListBox, TscListBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscColorBox, TscComboBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscCustomComboBoxEx, TscComboBoxExStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscComboBoxEx, TscComboBoxExStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscCustomListView, TscListViewStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscListView, TscListViewStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscTreeView, TscTreeViewStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscUpDown, TscUpDownStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscScrollBar, TscScrollBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscRichEdit, TscRichEditStyleHook);

finalization

  if SC_MouseHook <> 0 then
    SC_UnHookMouseMessages;

  {$IFDEF REPLACEDEFSTYLEHOOKS}
  TCustomStyleEngine.UnRegisterStyleHook(TCoolBar, TscCoolBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TCoolBar, TCoolBarStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TToolBar, TscToolBarStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TToolBar, TToolBarStyleHook);
  {$ENDIF}


  {$IFNDEF VER230}

  TCustomStyleEngine.UnRegisterStyleHook(TscStatusBar, TscStatusBarStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscToolBar, TscToolBarStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscCoolBar, TscCoolBarStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscMemo, TscScrollingStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscCustomListBox, TscListBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscListBox, TscListBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscCheckListBox, TscListBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscColorListBox, TscListBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscColorBox, TscComboBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscCustomComboBoxEx, TscComboBoxExStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscComboBoxEx, TscComboBoxExStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscCustomListView, TscListViewStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscTreeView, TscTreeViewStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscListView, TscListViewStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscUpDown, TscUpDownStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscScrollBar, TscScrollBarStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscRichEdit, TscRichEditStyleHook);
  {$ENDIF}

end.

