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

unit scExtControls;

{$R-}
{$I scdefine.inc}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types,
    Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.Themes, Vcl.ImgList,
    Vcl.Buttons, Vcl.ComCtrls, WinApi.CommCtrl, Vcl.Dialogs,
    scDrawUtils, scImageCollection, scControls, scColorDialog;

const
  DefCalcPrecision = 15;

type
  TscFontDevice = (fdScreen, fdPrinter, fdBoth);
  TscFontListOption = (foAnsiOnly, foTrueTypeOnly, foFixedPitchOnly,
    foNoOEMFonts, foOEMFontsOnly, foScalableOnly, foNoSymbolFonts);
  TscFontListOptions = set of TscFontListOption;

  TscFontListBox = class(TscCustomListBox)
  private
    FDevice: TscFontDevice;
    FUpdate: Boolean;
    FOptions: TscFontListOptions;
    procedure SetFontName(const NewFontName: TFontName);
    function GetFontName: TFontName;
    function GetTrueTypeOnly: Boolean;
    procedure SetDevice(Value: TscFontDevice);
    procedure SetOptions(Value: TscFontListOptions);
    procedure SetTrueTypeOnly(Value: Boolean);
  protected
    procedure DrawItemContent(ACanvas: TCanvas; ARect:
        TRect; AIndex: Integer; AState: TOwnerDrawState); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopulateList;
    procedure Reset;
  published
    property ItemIndex;
    property Sorted;

    property Device: TscFontDevice read FDevice write SetDevice default fdScreen;
    property FontName: TFontName read GetFontName write SetFontName;
    property Options: TscFontListOptions read FOptions write SetOptions default [];
    property TrueTypeOnly: Boolean read GetTrueTypeOnly write SetTrueTypeOnly
      stored False;

    property LineColor;
    property ShowLines;
    property SelectionStyle;
    property ShowFocusRect;
    property SelectionColor;
    property SelectionTextColor;

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
  end;

  TscFontComboBox = class(TscCustomComboBox)
  private
    FDevice: TscFontDevice;
    FUpdate: Boolean;
    FOptions: TscFontListOptions;
    procedure SetFontName(const NewFontName: TFontName);
    function GetFontName: TFontName;
    function GetTrueTypeOnly: Boolean;
    procedure SetDevice(Value: TscFontDevice);
    procedure SetOptions(Value: TscFontListOptions);
    procedure SetTrueTypeOnly(Value: Boolean);
  protected
    procedure DrawItemContent(ACanvas: TCanvas; ARect:
        TRect; AIndex: Integer; AState: TOwnerDrawState; AComboItem: Boolean); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopulateList;
    procedure Reset;
  published
    property ItemIndex;
    property Sorted;

    property Device: TscFontDevice read FDevice write SetDevice default fdScreen;
    property FontName: TFontName read GetFontName write SetFontName;
    property Options: TscFontListOptions read FOptions write SetOptions default [];
    property TrueTypeOnly: Boolean read GetTrueTypeOnly write SetTrueTypeOnly
      stored False;

    property SelectionStyle;
    property SelectionColor;
    property SelectionTextColor;
    property StyleKind;
    property ShowFocusRect;

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
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

   TscFontSizeComboBox = class(TscCustomComboBox)
   private
     PixelsPerInch: Integer;
     FFontName: TFontName;
     procedure SetFontName(const Value: TFontName);
     procedure Build;
     function GetSizeValue: Integer;
     function IsValidChar(Key: Char): Boolean;
   protected
     procedure KeyPress(var Key: Char); override;
   public
     constructor Create(AOwner: TComponent); override;
     property FontName: TFontName read FFontName write SetFontName;
     property SizeValue: Integer read GetSizeValue;
   published
      property Style;

      property SelectionStyle;
      property SelectionColor;
      property SelectionTextColor;

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
      property Text;
      property Touch;
      property Visible;
      property StyleKind;
      property ShowFocusRect;
      {$IFNDEF VER230}
      property StyleElements;
      {$ENDIF}
      property OnClick;
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
      property OnSelect;
      property OnStartDock;
      property OnStartDrag;
  end;

  TscCalcState = (csFirst, csValid, csError);

  TscCalcEdit = class;

  TscPopupCalculatorForm = class(TscPanel)
  protected
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
  public
    CalcEdit: TscCalcEdit;
    FCalcPanel: TscPanel;
    FDisplayLabel: TscPanel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(X, Y: Integer);
    procedure Hide;
  end;

  TscCalcBackgroundStyle = (sccbsPanel, sccbsFormBackground);
  TscCalcButtonStyle = (sccbsPushButton, sccbsToolButton);

  TscCalcEdit = class(TscCustomEdit)
  private
    FDisplayType: TscNumEditDisplayType;
    FMemory: Double;
    FPrecision: Byte;
    FCalc: TscPopupCalculatorForm;
    StopCheck, FromEdit: Boolean;
    FDecimal: Byte;
    FMinValue, FMaxValue, FIncrement: Double;
    FValueType: TscValueType;
    FValue: Double;
    FCalcButtonAnimation: Boolean;
    FCalcBackgroundStyle: TscCalcBackgroundStyle;
    FCalcWallpapers: TscCustomImageCollection;
    FCalcWallpaperIndex: Integer;
    FCalcButtonStyle: TscCalcButtonStyle;
    function GetCalcFont: TFont;
    procedure SetDisplayType(Value: TscNumEditDisplayType);
    procedure SetCalcFont(Value: TFont);
    procedure SetValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetValueType(NewType: TscValueType);
    procedure SetDecimal(NewValue: Byte);
    procedure DropDown;
    procedure CloseUp;
  protected
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function IsCustomDraw(ADC: HDC): Boolean; override;
    function CheckValue(NewValue: Double): Double;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMMouseHookCancelMode(var Message: TMessage); message WM_MOUSEHOOKCANCELMODE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(Key: Char): Boolean;
    procedure Change; override;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Text;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsNumText(AText: String): Boolean;
    function IsPopupVisible: Boolean;
    property Memory: Double read FMemory;
    procedure ButtonClick(Sender: TObject);
  published
    property LeftButton;
    property RightButton;
    property Transparent;
    property BorderKind;
    property FrameColor;
    property FrameActiveColor;
    property ButtonImages;
    property Alignment;
    property BorderStyle;
    property DisplayType: TscNumEditDisplayType
      read FDisplayType write SetDisplayType;
    property CalcButtonStyle: TscCalcButtonStyle
      read FCalcButtonStyle write FCalcButtonStyle;
    property CalcBackgroundStyle: TscCalcBackgroundStyle
      read FCalcBackgroundStyle write FCalcBackgroundStyle;
    property CalcWallpapers: TscCustomImageCollection
      read FCalcWallpapers write FCalcWallpapers;
    property CalcWallpaperIndex: Integer
      read FCalcWallpaperIndex write FCalcWallpaperIndex;
    property CalcButtonAnimation: Boolean
      read FCalcButtonAnimation write FCalcButtonAnimation;
    property CalcFont: TFont read GetCalcFont write SetCalcFont;
    property Precision: Byte read FPrecision write FPrecision default DefCalcPrecision;
    property ValueType: TscValueType read FValueType write SetValueType;
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property Increment: Double read FIncrement write FIncrement;
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
    property OnRightButtonClick;
  end;

  TscColorButtonStyleKind = (scclbPushButton, scclbToolButton,
    scclbPushButtonTransparent, scclbToolButtonTransparent, scclbTransparent,
      scclbSegmentedLeft, scclbSegmentedMiddle,
      scclbSegmentedRight, scclbSegmentedToolLeft, scclbSegmentedToolMiddle,
      scclbSegmentedToolRight);

  TscColorGridType = (scgsExtended, scgsSimple);

  TscCustomColorButtonValues = array[1..8] of TColor;

  TscColorButton = class(TscCustomButtonControl)
  private
    FColorGridType: TscColorGridType;
    FMenuBGStyle: TscGalleryMenuBGStyle;
    FStyleKind: TscColorButtonStyleKind;
    FColorMenu: TscGalleryMenu;
    FColorImages: TCustomImageList;
    FAutoColor: TColor;
    FColorValue: TColor;
    FOnChangeColor: TNotifyEvent;
    FShowAutoColor: Boolean;
    FShowMoreColor: Boolean;
    FColorDialog: TColorDialog;
    FColorDialogCustom: TscColorDialog;
    FMoreCaption: String;
    FAutoCaption: String;
    FWidthWithCaption: Integer;
    FWidthWithoutCaption: Integer;
    FShowCaption: Boolean;
    FUseFontColorToStyleColor: Boolean;
    FShowCustomColors: Boolean;
    FCustomColorCursor: Integer;
    function GetShowPopupColorsFromRight: Boolean;
    procedure SetShowPopupColorsFromRight(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
    procedure SetColorValue(Value: TColor);
    procedure OnGalleryMenuClick(Sender: TObject);
    procedure OnGalleryMenuPopup(Sender: TObject);
    function GetMenuFont: TFont;
    procedure SetMenuFont(Value: TFont);
    procedure SetShowAutoColor(Value: Boolean);
    procedure SetShowMoreColor(Value: Boolean);
    procedure SetShowCustomColors(Value: Boolean);
    procedure SetStyleKind(Value: TscColorButtonStyleKind);
    procedure AddCustomColor(Value: TColor);
    procedure AddCustomColorIndex(Index: Integer; Value: TColor);
  protected
    procedure DoDialogChar; override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function CanAnimateFocusedState: Boolean; override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
  public
    CustomColors: TscCustomColorButtonValues;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitColors;
    procedure InitSimpleColors;
  published
    property WidthWithCaption: Integer read FWidthWithCaption write FWidthWithCaption;
    property WidthWithoutCaption: Integer read FWidthWithoutCaption write FWidthWithoutCaption;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowPopupColorsFromRight: Boolean
      read GetShowPopupColorsFromRight write SetShowPopupColorsFromRight;
    property SplitButton;
    property Action;
    property ArrowPosition;
    property Animation;
    property Caption;
    property CanFocused;
    property Margin;
    property Spacing;
    property Layout;
    property Images;
    property ImageIndex;
    property GlowEffect;
    property ImageGlow;
    property AutoColor: TColor read FAutoColor write FAutoColor;
    property ColorValue: TColor read FColorValue write SetColorValue;
    property ShowAutoColor: Boolean read FShowAutoColor write SetShowAutoColor;
    property ShowMoreColor: Boolean read FShowMoreColor write SetShowMoreColor;
    property ShowCustomColors: Boolean read FShowCustomColors write SetShowCustomColors;
    property ColorDialog: TColorDialog read FColorDialog write FColorDialog;
    property ColorDialogCustom: TscColorDialog read FColorDialogCustom write FColorDialogCustom;
    property MenuFont: TFont read GetMenuFont write SetMenuFont;
    property MoreCaption: String read FMoreCaption write FMoreCaption;
    property AutoCaption: String read FAutoCaption write FAutoCaption;
    property StyleKind: TscColorButtonStyleKind read FStyleKind write SetStyleKind;
    property UseFontColorToStyleColor: Boolean
        read FUseFontColorToStyleColor write FUseFontColorToStyleColor;
    property MenuBGStyle: TscGalleryMenuBGStyle
      read FMenuBGStyle write FMenuBGStyle;
    property ColorGridType: TscColorGridType
      read FColorGridType write FColorGridType;
    property OnChangeColor: TNotifyEvent read FOnChangeColor write FOnChangeColor;
    property OnClick;
  end;

  TscFrameBarItems = class;
  TscFrameBarItemButton = class;
  TscCreateFrameEvent = procedure(Sender: TObject; var AFrame: TCustomFrame) of object;
  TscFrameDestroyEvent = procedure(Sender: TObject; var AFrame: TCustomFrame; var ACanDestroy: Boolean) of object;
  TscFrameState = (scfsFrameClosed, scfsFrameOpened, scfsFrameCloseAction, scfsFrameOpenAction);

  TscFrameAdapter = class(TComponent)
  protected
    FFrame: TFrame;
    OldWindowProc: TWndMethod;
    procedure NewWndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TscFrameBarItem = class(TCollectionItem)
  private
    FFrameBarItems: TscFrameBarItems;
    FCaption: String;
    FVisible: Boolean;
    FData: TCustomData;
    FImageIndex: Integer;
    FEnabled: Boolean;
    FFreezeState: Boolean;
    FOnFrameCreate: TscCreateFrameEvent;
    FOnFrameDestroy: TscFrameDestroyEvent;
    FOnFrameShow: TNotifyEvent;
    FOnFrameClose: TNotifyEvent;
    FOnClick: TNotifyEvent;
    procedure SetEnabled(Value: Boolean);
    procedure SetCaption(Value: String);
    procedure SetVisible(Value: Boolean);
    procedure FrameButtonClick(Sender: TObject);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure SetIndex(Value: Integer); override;
    procedure SetData(const Value: TCustomData); virtual;
  public
    Frame: TCustomFrame;
    FrameAdapter: TscFrameAdapter;
    FrameButton: TscFrameBarItemButton;
    FrameState: TscFrameState;
    FrameHeight: Integer;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
    property Data: TCustomData read FData write SetData;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Caption: String read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Visible: Boolean read FVisible write SetVisible default True;
    property FreezeState: Boolean read FFreezeState write FFreezeState;
    property OnFrameCreate: TscCreateFrameEvent read FOnFrameCreate write FOnFrameCreate;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnFrameDestroy: TscFrameDestroyEvent read FOnFrameDestroy write FOnFrameDestroy;
    property OnFrameShow: TNotifyEvent read FOnFrameShow write FOnFrameShow;
    property OnFrameClose: TNotifyEvent read FOnFrameClose write FOnFrameClose;
  end;

  TscFrameBar = class;

  TscFrameBarItems = class(TCollection)
  private
    FFrameBar: TscFrameBar;
  protected
    function GetItem(Index: Integer): TscFrameBarItem;
    procedure SetItem(Index: Integer; Value: TscFrameBarItem);
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TscFrameBar);
    destructor Destroy; override;
    property Items[Index: Integer]: TscFrameBarItem read GetItem write SetItem; default;
  end;

  TscOnFrameChangeEvent = procedure(Sender: TObject; FrameItem: TscFrameBarItem) of object;

  TscFrameBar = class(TscScrollBox)
  private
    FItems: TscFrameBarItems;
    FButtonStyle: TscbbButtonStyle;
    FAnimation: Boolean;
    FCanChange: Boolean;
    FScalePercent: Integer;
    FButtonHeight: Integer;
    FButtonImages: TCustomImageList;
    FButtonSpacing: Integer;
    FButtonMargin: Integer;
    FButtonFont: TFont;
    FButtonAnimation: Boolean;
    FButtonTransparentBackground: Boolean;

    FCanCloseAll: Boolean;
    FCanOpenAll: Boolean;

    FOnFrameChange: TscOnFrameChangeEvent;
    FOnFrameChanging: TscOnFrameChangeEvent;

    FActiveFrameIndex: Integer;
    procedure SetButtonTransparentBackground(Value: Boolean);
    procedure SetButtonAnimation(Value: Boolean);
    procedure SetButtonStyle(Value: TscbbButtonStyle);
    procedure SetButtonFont(Value: TFont);
    procedure SetActiveFrameIndex(Value: Integer);
    procedure SetItems(const Value: TscFrameBarItems);
    procedure SetButonHeight(const Value: Integer);
    procedure SetButtonImages(const Value: TCustomImageList);
    procedure SetButtonSpacing(const Value: Integer);
    procedure SetButtonMargin(const Value: Integer);

    function CalcClientRect: TRect;
    procedure AdjustSizes;
    function AdjustFrameBounds(AIndex, APos,
      AWidth, AHeight: Integer; AOpen: Boolean): Boolean;

    function GetScrollOffset: Integer;
    procedure OnButtonFontChange(Sender: TObject);
  protected
    Adjusting: Boolean;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AdjustControls(ACanAnimate: Boolean);
    procedure OpenFrame(Index: Integer);
    procedure CloseFrame(Index: Integer);
    procedure OpenAllFrames;
    procedure CloseAllFrames;
  published
    property Align;
    property ActiveFrameIndex: Integer read FActiveFrameIndex write SetActiveFrameIndex;
    property Animation: Boolean read FAnimation write FAnimation default True;
    property ButtonFont: TFont read FButtonFont write SetButtonFont;
    property ButtonImages: TCustomImageList read FButtonImages write SetButtonImages;
    property ButtonSpacing: Integer read FButtonSpacing write SetButtonSpacing;
    property ButtonMargin: Integer read FButtonMargin write SetButtonMargin;
    property ButonHeight: Integer read FButtonHeight write SetButonHeight;
    property ButtonStyle: TscbbButtonStyle read FButtonStyle
       write SetButtonStyle;
    property ButtonTransparentBackground: Boolean
      read FButtonTransparentBackground write SetButtonTransparentBackground;
    property ButtonAnimation: Boolean read FButtonAnimation write SetButtonAnimation;
    property Items: TscFrameBarItems read FItems write SetItems;
    property CanCloseAll: Boolean read FCanCloseAll write FCanCloseAll;
    property CanOpenAll: Boolean read FCanOpenAll write FCanOpenAll;
    property OnFrameChange: TscOnFrameChangeEvent read FOnFrameChange write FOnFrameChange;
    property OnFrameChanging: TscOnFrameChangeEvent read FOnFrameChanging write FOnFrameChanging;
  end;

  TscFrameBarItemButton = class(TscButton)
  protected
    FrameBar: TscFrameBar;
    FrameItem: TscFrameBarItem;
  public
    constructor CreateEx(AFrameBar: TscFrameBar);
    destructor Destroy; override;
    property OnClick;
  end;

  TscTimeFormat = (sctf24Hour, sctf12Hour);

  TscTimeEdit = class(TscCustomEdit)
  private
    FShowMSec: Boolean;
    FShowSec: Boolean;
    FShowUpDown: Boolean;
    FTimeFormat: TscTimeFormat;
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
    function CanScaleButtons: Boolean; override;
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
    property Transparent;
    property BorderKind;
    property FrameColor;
    property FrameActiveColor;
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

  TscControlButton = record
    Rect: TRect;
    MouseIn: Boolean;
    Down: Boolean;
    Visible: Boolean;
    Enabled: Boolean;
  end;

  TscScrollType = (scstHorizontal, scstVertical);
  TscScrollPanelStyleKind = (scspsPanel, scspsToolBar,
    scspsFormBackground, scspsTabSheet, scspsTransparent);

  TscScrollPanelHorzDirection = (scspssdLeftToRight, scspssdRightToLeft);
  TscScrollPanelVertDirection = (scspssdTopToBottom, scspssdBottomToTop);

  TscScrollPanel = class(TscCustomControl)
  private
    FStyleKind: TscScrollPanelStyleKind;
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

    FHorzScrollDirection: TscScrollPanelHorzDirection;
    FVertScrollDirection: TscScrollPanelVertDirection;

    procedure SetHorzScrollDirection(Value: TscScrollPanelHorzDirection);
    procedure SetVertScrollDirection(Value: TscScrollPanelVertDirection);

    procedure SetScrollButtonWidth(Value: Integer);

    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetCustomImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetStyleKind(Value: TscScrollPanelStyleKind);

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
    property Color;
    property StyleKind: TscScrollPanelStyleKind read FStyleKind write SetStyleKind;
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
    property StorePaintBuffer;
    property HorzScrollDirection: TscScrollPanelHorzDirection
      read FHorzScrollDirection write SetHorzScrollDirection;
    property VertScrollDirection: TscScrollPanelVertDirection
      read FVertScrollDirection write SetVertScrollDirection;
    property TouchScroll: Boolean read
      FTouchScroll write SetTouchScroll;
  end;

  TscExPanelRollKind = (scrkRollHorizontal, scrkRollVertical);
  TscExPanelBackgroundStyle =
    (scexbgsPanel, scexbgsFormBackground, scexbgsTransparent);
  TscExPanelHeaderStyle =
    (scexphsHeader, scexphsColor, scexphsCategoryHeader);

  TscExPanelHorzRollButtonPosition = (scrbpRight, scrbpLeft);

  TscExPanel = class(TscCustomControl)
  private
    FHorzRollButtonPosition: TscExPanelHorzRollButtonPosition;
    FBorderWidth: Integer;
    FBackgroundStyle: TscExPanelBackgroundStyle;
    FHeaderStyle: TscExPanelHeaderStyle;
    FHeaderColor: TColor;
    FButtonGlyphColor: TColor;
    FCaptionCaptured: Boolean;
    FMoveable, FSizeable: Boolean;
    FFrameColor: TColor;
    FCMaxWidth, FCMinWidth,
    FCMaxHeight, FCMinHeight: Integer;
    FSpacing: Integer;
    FOnChangingRollUpState: TNotifyEvent;
    FOnChangeRollUpState: TNotifyEvent;
    FOnClose: TNotifyEvent;
    StopCheckSize: Boolean;
    FRollUpState: Boolean;
    FRollKind: TscExPanelRollKind;
    FCaptionHeight: Integer;
    FChangeRollStateWithCaptionClick: Boolean;
    FRealWidth, FRealHeight: Integer;
    FShowRollButton: Boolean;
    FShowCloseButton: Boolean;
    FCaptionImages: TCustomImageList;
    FCaptionImageIndex: Integer;
    FVisibleList: TList;
    FShowFrame: Boolean;
    FCaptionRect: TRect;
    FOnCaptionClick, FOnCaptionDblClick: TNotifyEvent;
    FMovingInParentClientBounds: Boolean;
    FHideControlsInRollUpState: Boolean;
    procedure SetHorzRollButtonPosition(Value: TscExPanelHorzRollButtonPosition);
    procedure SetFrameColor(Value: TColor);
    procedure SetBorderWidth(Value: Integer);
    procedure SetShowFrame(Value: Boolean);
    procedure SetHeaderStyle(Value: TscExPanelHeaderStyle);
    procedure SetHeaderColor(Value: TColor);
    procedure SetButtonGlyphColor(Value: TColor);
    procedure SetCaptionImages(Value: TCustomImageList);
    procedure SetBackgroundStyle(Value: TscExPanelBackgroundStyle);
    procedure SetCaptionImageIndex(Value: Integer);
    function GetRollWidth: Integer;
    function GetRollHeight: Integer;
    procedure SetShowRollButton(Value: Boolean);
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetSpacing(Value: Integer);
  protected
    Buttons: array[0..1] of TscControlButton;
    OldActiveButton, ActiveButton, CaptureButton: Integer;
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMCAPTURECHANGED(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure SetCaptionHeight(Value: Integer);
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure HideControls;
    procedure ShowControls;
    procedure SetRollUpState(Value: Boolean);
    procedure SetRollKind(Value: TscExPanelRollKind);

    procedure ButtonDown(I: Integer; X, Y: Integer);
    procedure ButtonUp(I: Integer; X, Y: Integer);
    procedure ButtonEnter(I: Integer);
    procedure ButtonLeave(I: Integer);
    procedure DrawButton(ACanvas: TCanvas; I: Integer);
    procedure TestActive(X, Y: Integer);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure WMSIZING(var Message: TWMSIZE); message WM_SIZING;
    procedure WMMOVING(var Message: TWMMOVING); message WM_MOVING;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure Loaded; override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Close;
  published
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property BackgroundStyle: TscExPanelBackgroundStyle read FBackgroundStyle write SetBackgroundStyle;
    property FrameColor: TColor
      read FFrameColor write SetFrameColor;
    property MovingInParentClientBounds: Boolean
      read FMovingInParentClientBounds write FMovingInParentClientBounds default True;
    property HeaderColor: TColor
      read FHeaderColor write SetHeaderColor;
     property ButtonGlyphColor: TColor
      read FButtonGlyphColor write SetButtonGlyphColor;
    property HeaderStyle: TscExPanelHeaderStyle
      read FHeaderStyle write SetHeaderStyle;
    property CaptionImages: TCustomImageList read FCaptionImages write SetCaptionImages;
    property CaptionImageIndex: Integer read FCaptionImageIndex write SetCaptionImageIndex;
    property ChangeRollStateWithCaptionClick: Boolean
      read FChangeRollStateWithCaptionClick write FChangeRollStateWithCaptionClick;
    property Color;
    property HorzRollButtonPosition: TscExPanelHorzRollButtonPosition
      read FHorzRollButtonPosition write SetHorzRollButtonPosition;

    property Spacing: Integer read FSpacing write SetSpacing;

    property ShowFrame: Boolean read FShowFrame write SetShowFrame;

    property RealWidth: Integer read FRealWidth write FRealWidth;
    property RealHeight: Integer read FRealHeight write FRealHeight;

    property ShowRollButton: Boolean
      read FShowRollButton write SetShowRollButton;
    property ShowCloseButton: Boolean
      read FShowCloseButton write SetShowCloseButton;
    property CaptionHeight: Integer
      read FCaptionHeight write SetCaptionHeight;
    property RollKind: TscExPanelRollKind read FRollKind write SetRollKind;
    property RollUpState: Boolean read FRollUpState write SetRollUpState;

    property Moveable: Boolean read FMoveable write FMoveable;
    property Sizeable: Boolean read FSizeable write FSizeable;

    property HideControlsInRollUpState: Boolean
      read  FHideControlsInRollUpState write FHideControlsInRollUpState;
    property StorePaintBuffer;

    property Align;
    property Caption;
    property DockSite;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnCaptionClick: TNotifyEvent read
      FOnCaptionClick write FOnCaptionClick;
     property OnCaptionDblClick: TNotifyEvent read
      FOnCaptionDblClick write FOnCaptionDblClick;
    property OnChangingRollUpState: TNotifyEvent
      read FOnChangingRollUpState write FOnChangingRollUpState;
    property OnChangeRollUpState: TNotifyEvent
      read FOnChangeRollUpState write FOnChangeRollUpState;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TscImage = class(TscPanel)
  private
    FPicture: TPicture;
    FOnProgress: TProgressEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FDrawing: Boolean;
    FProportional: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);
  protected
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Center: Boolean read FCenter write SetCenter default False;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
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

  TscListGroupPanel = class(TscCustomControl)
  private
    FGroupColor: TColor;
    FGroupColorAlpha: Byte;
    FHeaderHeight: Integer;
    FRowCount: Integer;
    FHeaderFont: TFont;
    FRowLineMargin: Integer;
    FRowHeight: Integer;
    FHeaderMargin: Integer;
    FHeaderAutoColor: Boolean;
    FAutoSize: Boolean;
    procedure SetRowHeight(Value: Integer);
    procedure SetPanelAutoSize(Value: Boolean);
    procedure SetGroupColorAlpha(Value: Byte);
    procedure SetHeaderFont(Value: TFont);
    procedure SetRowCount(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetGroupColor(Value: TColor);
    procedure SetRowLineMargin(Value: Integer);
    procedure SetHeaderMargin(Value: Integer);
    procedure SetHeaderAutoColor(Value: Boolean);
  protected
    procedure OnControlChange(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure DrawBackground(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property AutoSize: Boolean
      read FAutoSize write SetPanelAutoSize;
    property RowCount: Integer read FRowCount write SetRowCount;
    property RowLineMargin: Integer read
      FRowLineMargin write SetRowLineMargin;
    property RowHeight: Integer
      read FRowHeight write SetRowHeight;
    property GroupColor: TColor read FGroupColor write SetGroupColor;
    property GroupColorAlpha: Byte read
      FGroupColorAlpha write SetGroupColorAlpha;
    property Color;
    property Caption;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HeaderHeight: Integer
      read FHeaderHeight write SetHeaderHeight;
    property HeaderMargin: Integer
      read FHeaderMargin write SetHeaderMargin;
    property HeaderAutoColor: Boolean
      read FHeaderAutoColor write SetHeaderAutoColor;
    property TransparentBackground;
  end;

  TscControlBar = class(TCustomControlBar)
  protected
    FBackgroundStyle: TscExPanelBackgroundStyle;
    FWallpapers: TscCustomImageCollection;
    FWallpaperIndex: Integer;
    FScaleFactor: Double;
    FScalePercent: Integer;
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    procedure SetBackgroundStyle(Value: TscExPanelBackgroundStyle);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMCHECKPARENTBG(var Msg: TWMEraseBkgnd); message WM_CHECKPARENTBG;
    procedure RePaintControl;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure WMSIZE(var Msg: TMessage); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AutoDock;
    property AutoDrag;
    property AutoSize;
    property BackgroundStyle: TscExPanelBackgroundStyle read FBackgroundStyle write SetBackgroundStyle;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderWidth;
    property Color nodefault;
    property Constraints;
    property CornerEdge;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawingStyle;
    property Enabled;
    property GradientDirection;
    property GradientEndColor;
    property GradientStartColor;
    property ParentBackground default True;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowSize;
    property RowSnap;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnBandDrag;
    property OnBandInfo;
    property OnBandMove;
    property OnBandPaint;
    property OnBeginBandMove;
    property OnEndBandMove;
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
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation
 uses
   System.UITypes, System.SysUtils, Vcl.Printers, System.Math,
   Vcl.Clipbrd, System.StrUtils;

 const
   WRITABLE_FONTTYPE = 256;
   HTBUTTON1 = HTOBJECT + 100;
   HTBUTTON2 = HTOBJECT + 101;
   ScrollButtonSize = 15;

procedure DrawTT(Cnvs: TCanvas; X, Y: Integer; C: TColor);
begin
  with Cnvs do
  begin
    Pen.Color := C;
    MoveTo(X, Y);
    LineTo(X + 7, Y);
    LineTo(X + 7, Y + 3);
    MoveTo(X, Y);
    LineTo(X, Y + 3);
    MoveTo(X + 1, Y);
    LineTo(X + 1, Y + 1);
    MoveTo(X + 6, Y);
    LineTo(X + 6, Y + 1);
    MoveTo(X + 3, Y);
    LineTo(X + 3, Y + 8);
    MoveTo(X + 4, Y);
    LineTo(X + 4, Y + 8);
    MoveTo(X + 2, Y + 8);
    LineTo(X + 6, Y + 8);
  end;
end;

function IsValidFontLB(Box: TscFontListBox; LogFont: TLogFont;
  FontType: Integer): Boolean;
begin
  Result := True;
  if (foAnsiOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if (foTrueTypeOnly in Box.Options) then
    Result := Result and (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE);
  if (foFixedPitchOnly in Box.Options) then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH);
  if (foOEMFontsOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if (foNoOEMFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if (foNoSymbolFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> SYMBOL_CHARSET);
  if (foScalableOnly in Box.Options) then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

function EnumFontsProcLB(var EnumLogFont: TEnumLogFont;
  var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
var
  FaceName: string;
begin
  FaceName := StrPas(EnumLogFont.elfLogFont.lfFaceName);
  with TscFontListBox(Data) do
    if (Items.IndexOf(FaceName) < 0) and
      IsValidFontLB(TscFontListBox(Data), EnumLogFont.elfLogFont, FontType) then
    begin
      if EnumLogFont.elfLogFont.lfCharSet <> SYMBOL_CHARSET then
        FontType := FontType or WRITABLE_FONTTYPE;
      Items.AddObject(FaceName, TObject(FontType));
    end;
  Result := 1;
end;

function IsValidFontCB(Box: TscFontComboBox; LogFont: TLogFont;
  FontType: Integer): Boolean;
begin
  Result := True;
  if (foAnsiOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if (foTrueTypeOnly in Box.Options) then
    Result := Result and (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE);
  if (foFixedPitchOnly in Box.Options) then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH);
  if (foOEMFontsOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if (foNoOEMFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if (foNoSymbolFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> SYMBOL_CHARSET);
  if (foScalableOnly in Box.Options) then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

function EnumFontsProcCB(var EnumLogFont: TEnumLogFont;
  var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
var
  FaceName: string;
begin
  FaceName := StrPas(EnumLogFont.elfLogFont.lfFaceName);
  with TscFontComboBox(Data) do
    if (Items.IndexOf(FaceName) < 0) and
      IsValidFontCB(TscFontComboBox(Data), EnumLogFont.elfLogFont, FontType) then
    begin
      if EnumLogFont.elfLogFont.lfCharSet <> SYMBOL_CHARSET then
        FontType := FontType or WRITABLE_FONTTYPE;
      Items.AddObject(FaceName, TObject(FontType));
    end;
  Result := 1;
end;

constructor TscFontListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDevice := fdScreen;
  Sorted := True;
end;

procedure TscFontListBox.CreateWnd;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    FUpdate := True;
    try
      PopulateList;
    finally
      FUpdate := False;
    end;
  end
  else
    Reset;
end;

procedure TscFontListBox.Reset;
var
  SaveName: TFontName;
begin
  if HandleAllocated then
  begin
    FUpdate := True;
    try
      SaveName := FontName;
      PopulateList;
      FontName := SaveName;
    finally
      FUpdate := False;
      if FontName <> SaveName then
      begin
        if not (csReading in ComponentState) then
        if not FUpdate and Assigned(OnClick) then OnClick(Self);
       end;
    end;
  end;
end;

procedure TscFontListBox.SetFontName(const NewFontName: TFontName);
var
  Item: Integer;
begin
  if FontName <> NewFontName then
  begin
    if not (csLoading in ComponentState) then
    begin
      HandleNeeded;
      for Item := 0 to Items.Count - 1 do
        if AnsiCompareText(Items[Item], NewFontName) = 0 then
        begin
          ItemIndex := Item;
          if not (csReading in ComponentState) then
          if not FUpdate and Assigned(OnClick) then OnClick(Self);
          Exit;
        end;
      end;
    if not (csReading in ComponentState) then
      if not FUpdate and Assigned(OnClick) then OnClick(Self);
  end;
end;

function TscFontListBox.GetFontName: TFontName;
begin
  if ItemIndex <> -1
  then
    Result := Items[ItemIndex]
  else
    Result := '';
end;

function TscFontListBox.GetTrueTypeOnly: Boolean;
begin
  Result := foTrueTypeOnly in FOptions;
end;

procedure TscFontListBox.SetOptions;
begin
  if Value <> Options then
  begin
    FOptions := Value;
    Reset;
  end;
end;

procedure TscFontListBox.SetTrueTypeOnly(Value: Boolean);
begin
  if Value <> TrueTypeOnly then
  begin
    if Value then FOptions := FOptions + [foTrueTypeOnly]
    else FOptions := FOptions - [foTrueTypeOnly];
    Reset;
  end;
end;

procedure TscFontListBox.SetDevice;
begin
  if Value <> FDevice then
  begin
    FDevice := Value;
    Reset;
  end;
end;

procedure TscFontListBox.DrawItemContent(ACanvas: TCanvas; ARect:
  TRect; AIndex: Integer; AState: TOwnerDrawState);
var
  R: TRect;
  X, Y: Integer;
  C1, C2: TColor;
begin
  R := ARect;
  if (Integer(Items.Objects[AIndex]) and TRUETYPE_FONTTYPE) <> 0 then
  begin
    if IsRightToLeft then
      X := R.Right - 15
    else
      X := R.Left + 2;
    Y := R.Top + R.Height div 2 - 6;
    C1 := GetStyleColor(clWindowText);
    C2 := MiddleColor(GetStyleColor(clWindow), C1);
    DrawTT(ACanvas, X, Y, C2);
    DrawTT(ACanvas, X + 4, Y + 4, C1);
  end;
  if IsRightToLeft then
    Dec(R.Right, 17)
  else
    Inc(R.Left, 17);
  with ACanvas do
  begin
    Font.Name := Items[AIndex];
    Font.Style := [];
    scDrawText(ACanvas, Items[AIndex], R, IsRightToLeft, True);
  end;
end;

procedure TscFontListBox.Loaded;
begin
  inherited;
  FUpdate := True;
  try
    PopulateList;
  finally
    FUpdate := False;
  end;
end;

procedure TscFontListBox.PopulateList;
var
  DC: HDC;
  OldItemIndex: Integer;
begin
  if not HandleAllocated then Exit;
  OldItemIndex := ItemIndex;
  Items.BeginUpdate;
  try
    Items.Clear;
    DC := GetDC(0);
    try
      if (FDevice = fdScreen) or (FDevice = fdBoth) then
        EnumFontFamilies(DC, nil, @EnumFontsProcLB, Longint(Self));
      if (FDevice = fdPrinter) or (FDevice = fdBoth) then
      try
        EnumFontFamilies(Printer.Handle, nil, @EnumFontsProcLB, Longint(Self));
      except
      end;
    finally
      ReleaseDC(0, DC);
    end;
  finally
    Items.EndUpdate;
  end;
  ItemIndex := OldItemIndex;
end;

constructor TscFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FDevice := fdScreen;
  Sorted := True;
end;

procedure TscFontComboBox.CreateWnd;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    FUpdate := True;
    try
      PopulateList;
    finally
      FUpdate := False;
    end;
  end
  else
    Reset;
end;

procedure TscFontComboBox.Reset;
var
  SaveName: TFontName;
begin
  if HandleAllocated then
  begin
    FUpdate := True;
    try
      SaveName := FontName;
      PopulateList;
      FontName := SaveName;
    finally
      FUpdate := False;
      if FontName <> SaveName then
      begin
        if not (csReading in ComponentState) then
        if not FUpdate and Assigned(OnClick) then OnClick(Self);
       end;
    end;
  end;
end;

procedure TscFontComboBox.SetFontName(const NewFontName: TFontName);
var
  Item: Integer;
begin
  if FontName <> NewFontName then
  begin
    if not (csLoading in ComponentState) then
    begin
      HandleNeeded;
      for Item := 0 to Items.Count - 1 do
        if AnsiCompareText(Items[Item], NewFontName) = 0 then
        begin
          ItemIndex := Item;
          if not (csReading in ComponentState) then
          if not FUpdate and Assigned(OnClick) then OnClick(Self);
          Exit;
        end;
      end;
    if not (csReading in ComponentState) then
      if not FUpdate and Assigned(OnClick) then OnClick(Self);
  end;
end;

function TscFontComboBox.GetFontName: TFontName;
begin
  if ItemIndex <> -1
  then
    Result := Items[ItemIndex]
  else
    Result := '';
end;

function TscFontComboBox.GetTrueTypeOnly: Boolean;
begin
  Result := foTrueTypeOnly in FOptions;
end;

procedure TscFontComboBox.SetOptions;
begin
  if Value <> Options then
  begin
    FOptions := Value;
    Reset;
  end;
end;

procedure TscFontComboBox.SetTrueTypeOnly(Value: Boolean);
begin
  if Value <> TrueTypeOnly then
  begin
    if Value then FOptions := FOptions + [foTrueTypeOnly]
    else FOptions := FOptions - [foTrueTypeOnly];
    Reset;
  end;
end;

procedure TscFontComboBox.SetDevice;
begin
  if Value <> FDevice then
  begin
    FDevice := Value;
    Reset;
  end;
end;

procedure TscFontComboBox.DrawItemContent(ACanvas: TCanvas; ARect:
  TRect; AIndex: Integer; AState: TOwnerDrawState; AComboItem: Boolean);
var
  R: TRect;
  X, Y: Integer;
  C1, C2: TColor;
begin
  R := ARect;
  if (Integer(Items.Objects[AIndex]) and TRUETYPE_FONTTYPE) <> 0 then
  begin
    if IsRightToLeft then
      X := R.Right - 15
    else
      X := R.Left + 2;
    Y := R.Top + R.Height div 2 - 6;
    C1 := GetStyleColor(clWindowText);
    C2 := MiddleColor(GetStyleColor(clWindow), C1);
    DrawTT(ACanvas, X, Y, C2);
    DrawTT(ACanvas, X + 4, Y + 4, C1);
  end;
  if IsRightToLeft then
    Dec(R.Right, 17)
  else
    Inc(R.Left, 17);
  with ACanvas do
  begin
    Font.Name := Items[AIndex];
    Font.Style := [];
    scDrawText(ACanvas, Items[AIndex], R, IsRightToLeft, True);
  end;
end;

procedure TscFontComboBox.Loaded;
begin
  inherited;
  FUpdate := True;
  try
    PopulateList;
  finally
    FUpdate := False;
  end;
end;

procedure TscFontComboBox.PopulateList;
var
  DC: HDC;
  OldItemIndex: Integer;
begin
  if not HandleAllocated then Exit;
  OldItemIndex := ItemIndex;
  Items.BeginUpdate;
  try
    Items.Clear;
    DC := GetDC(0);
    try
      if (FDevice = fdScreen) or (FDevice = fdBoth) then
        EnumFontFamilies(DC, nil, @EnumFontsProcCB, Longint(Self));
      if (FDevice = fdPrinter) or (FDevice = fdBoth) then
      try
        EnumFontFamilies(Printer.Handle, nil, @EnumFontsProcCB, Longint(Self));
      except
      end;
    finally
      ReleaseDC(0, DC);
    end;
  finally
    Items.EndUpdate;
  end;
  ItemIndex := OldItemIndex;
end;

procedure TscFontComboBox.WMFontChange(var Message: TMessage);
begin
  inherited;
  Reset;
end;


function EnumFontSizes(var EnumLogFont: TEnumLogFont;
  PTextMetric: PNewTextMetric; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
var
  s: String;
  i, v, v2: Integer;
begin
  if (FontType and TRUETYPE_FONTTYPE) <> 0 then
  begin
    TscFontSizeComboBox(Data).Items.Add('8');
    TscFontSizeComboBox(Data).Items.Add('9');
    TscFontSizeComboBox(Data).Items.Add('10');
    TscFontSizeComboBox(Data).Items.Add('11');
    TscFontSizeComboBox(Data).Items.Add('12');
    TscFontSizeComboBox(Data).Items.Add('14');
    TscFontSizeComboBox(Data).Items.Add('16');
    TscFontSizeComboBox(Data).Items.Add('18');
    TscFontSizeComboBox(Data).Items.Add('20');
    TscFontSizeComboBox(Data).Items.Add('22');
    TscFontSizeComboBox(Data).Items.Add('24');
    TscFontSizeComboBox(Data).Items.Add('26');
    TscFontSizeComboBox(Data).Items.Add('28');
    TscFontSizeComboBox(Data).Items.Add('36');
    TscFontSizeComboBox(Data).Items.Add('48');
    TscFontSizeComboBox(Data).Items.Add('72');
    Result := 0;
  end
  else
  begin
    v := Round((EnumLogFont.elfLogFont.lfHeight - PTextMetric.tmInternalLeading)
      * 72 / TscFontSizeComboBox(Data).PixelsPerInch);
    s := IntToStr(v);
    Result := 1;
    for i := 0 to TscFontSizeComboBox(Data).Items.Count - 1 do
    begin
      v2 := StrToInt(TscFontSizeComboBox(Data).Items[i]);
      if v2 = v then
        Exit;
      if v2 > v then
      begin
        TscFontSizeComboBox(Data).Items.Insert(i, s);
        Exit;
      end;
    end;
    TscFontSizeComboBox(Data).Items.Add(s);
  end;
end;

constructor TscFontSizeComboBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
end;

function TscFontSizeComboBox.IsValidChar(Key: Char): Boolean;
begin
  Result := CharInSet(Key, ['0'..'9']) or
   ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if (Key = '0') and (Text = '') then
    Result := False;
end;

procedure TscFontSizeComboBox.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;    
  inherited KeyPress(Key);
end;

procedure TscFontSizeComboBox.Build;
var
  DC: HDC;
  OC: TNotifyEvent;
begin
  DC := GetDC(0);
  Items.BeginUpdate;
  try
    Items.Clear;
    if FontName <> '' then
    begin
      PixelsPerInch := GetDeviceCaps(DC, LOGPIXELSY);
      EnumFontFamilies(DC, PChar(FontName), @EnumFontSizes, Longint(Self));
      OC := OnClick;
      OnClick := nil;
      ItemIndex := Items.IndexOf(Text);
      OnClick := OC;
      if Assigned(OnClick) then
        OnClick(Self);
    end;
  finally
    Items.EndUpdate;
    ReleaseDC(0, DC);
  end;
end;

procedure TscFontSizeComboBox.SetFontName(const Value: TFontName);
begin
  FFontName := Value;
  Build;
end;

function TscFontSizeComboBox.GetSizeValue: Integer;

  function IsNumText(AText: String): Boolean;

    function GetMinus: Boolean;
    var
      i: Integer;
      s: String;
    begin
      s := AText;
      i := Pos('-', s);
      if i > 1 then
        Result := False
      else
      begin
        Delete(s, i, 1);
        Result := Pos('-', s) = 0;
      end;
    end;

  const
    EditChars = '01234567890-';
  var
    i: Integer;
    s: String;
  begin
    s := EditChars;
    Result := True;
    if (Text = '') or (Text = '-') then
    begin
      Result := False;
      Exit;
    end;

    for i := 1 to Length(Text) do
    begin
      if Pos(Text[i], s) = 0 then
      begin
        Result := False;
        Break;
      end;
    end;

    Result := Result and GetMinus;
  end;

begin
  if Style <> csDropDown then
  begin
    if ItemIndex = -1 then
      Result := 0
    else if Items[ItemIndex] <> '' then
      Result := StrToInt(Items[ItemIndex])
    else
      Result := 0;
  end
  else
  begin
    if (Text <> '') and (IsNumText(Text)) then
      Result := StrToInt(Text)
    else
      Result := 0;
  end;
end;

const
  BtnOffset = 5;

type
  TscCalcBtnKind =
   (cbNone, cbNum0, cbNum1, cbNum2, cbNum3, cbNum4, cbNum5, cbNum6,
    cbNum7, cbNum8, cbNum9, cbSgn, cbDcm, cbDiv, cbMul, cbSub,
    cbAdd, cbSqr, cbPcnt, cbRev, cbEql, cbBck, cbClr, cbMP,
    cbMS, cbMR, cbMC, cbOk, cbCancel);

{ TCalcButton }

type
  TCalcButton = class(TscButton)
  private
    FKind: TscCalcBtnKind;
  protected
  public
    constructor CreateKind(AOwner: TComponent; AKind: TscCalcBtnKind);
    property Kind: TscCalcBtnKind read FKind;
  end;

constructor TCalcButton.CreateKind(AOwner: TComponent; AKind: TscCalcBtnKind);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  DrawOnBackground := False;
  CanFocused := False;
  FKind := AKind;
  if FKind in [cbNum0..cbClr] then Tag := Ord(Kind) - 1
  else Tag := -1;
end;

const
  BtnPos: array[TscCalcBtnKind] of TPoint =
  ((X: -1; Y: -1), (X: 38; Y: 120), (X: 38; Y: 92), (X: 71; Y: 92),
    (X: 104; Y: 92), (X: 38; Y: 64), (X: 71; Y: 64), (X: 104; Y: 64),
    (X: 38; Y: 36), (X: 71; Y: 36), (X: 104; Y: 36), (X: 71; Y: 120),
    (X: 104; Y: 120), (X: 137; Y: 36), (X: 137; Y: 64), (X: 137; Y: 92),
    (X: 137; Y: 120), (X: 170; Y: 36), (X: 170; Y: 64), (X: 170; Y: 92),
    (X: 170; Y: 120), (X: 104; Y: 6), (X: 154; Y: 6), (X: 5; Y: 120),
    (X: 5; Y: 92), (X: 5; Y: 64), (X: 5; Y: 36),
    (X: 38; Y: 6), (X: 71; Y: 6));

   ResultKeys = [#13, '=', '%'];

function CreateCalcBtn(AParent: TWinControl; AKind: TscCalcBtnKind;
  AOnClick: TNotifyEvent): TCalcButton;
const
  BtnCaptions: array[cbSgn..cbMC] of PChar =
   ('+/-', ',', '/', '*', '-', '+', 'sqrt', '%', '1/x', '=', '<', 'C',
    'MP', 'MS', 'MR', 'MC');
begin
  Result := TCalcButton.CreateKind(AParent, AKind);
  with Result do
  try
    if Kind in [cbNum0..cbNum9] then Caption := IntToStr(Tag)
    else if Kind = cbDcm then Caption := FormatSettings.DecimalSeparator
    else if Kind in [cbSgn..cbMC] then Caption := StrPas(BtnCaptions[Kind]);
    Left := BtnPos[Kind].X + BtnOffset;
    Top := BtnPos[Kind].Y;
    Width := 30;
    Height := 23;
    OnClick := AOnClick;
    Parent := AParent;
  except
    Free;
    raise;
  end;
end;

{ TCalculatorPanel }

type
  TCalculatorPanel = class(TscPanel)
  private
    FText: string;
    FStatus: TscCalcState;
    FOperator: Char;
    FOperand: Double;
    FMemory: Double;
    FPrecision: Byte;
    FBeepOnError: Boolean;
    FMemoryLabel: TscLabel;
    FOnError: TNotifyEvent;
    FOnOk: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnResult: TNotifyEvent;
    FOnTextChange: TNotifyEvent;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TNotifyEvent;
    FControl: TControl;
    FButtonAnimation: Boolean;
    procedure SetText(const Value: string);
    procedure CheckFirst;
    procedure CalcKey(Key: Char);
    procedure Clear;
    procedure Error;
    procedure SetDisplay(R: Double);
    function GetDisplay: Double;
    procedure UpdateMemoryLabel;
    function FindButton(Key: Char): TscButton;
    procedure BtnClick(Sender: TObject);
  protected
    procedure TextChanged; virtual;
  public
    constructor CreateLayout(AOwner: TComponent);
    procedure CalcKeyPress(Sender: TObject; var Key: Char);
    procedure Copy;
    procedure Paste;
    property ButtonAnimation: Boolean
      read FButtonAnimation write FButtonAnimation;
    property DisplayValue: Double read GetDisplay write SetDisplay;
    property Text: string read FText;
    property OnOkClick: TNotifyEvent read FOnOk write FOnOk;
    property OnCancelClick: TNotifyEvent read FOnCancel write FOnCancel;
    property OnResultClick: TNotifyEvent read FOnResult write FOnResult;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnCalcKey: TKeyPressEvent read FOnCalcKey write FOnCalcKey;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
  end;

constructor TCalculatorPanel.CreateLayout(AOwner: TComponent);
var
  I: TscCalcBtnKind;
const
    BtnCaptions: array[cbSgn..cbCancel] of PChar =
    ('+/-', ',', '/', '*', '-', '+', 'sqrt', '%', '1/x', '=', '<<', 'C',
    'MP', 'MS', 'MR', 'MC', 'OK', 'X');
begin
  inherited Create(AOwner);
  DrawOnBackground := False;
  Height := 150;
  Width := Round(210 * FScaleFactor) + BtnOffset;
  FMemoryLabel := TscLabel.Create(Self);
  with FMemoryLabel do
  begin
    Left := 15;
    Top := 8;
    Parent := Self;
  end;
  try
    for I := cbNum0 to cbCancel do begin
      if BtnPos[I].X > 0 then
        with CreateCalcBtn(Self, I, BtnClick) do
        begin
          if (Kind in [cbBck, cbClr]) then Width := 46;
          if (Kind in [cbSgn..cbCancel]) then Caption := BtnCaptions[Kind];
        end;
    end;
  finally
  end;
  FText := '0';
  FMemory := 0.0;
  FPrecision := DefCalcPrecision;
  FBeepOnError := True;
end;

procedure TCalculatorPanel.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    TextChanged;
  end;
end;

procedure TCalculatorPanel.TextChanged;
begin
  if Assigned(FControl) then TLabel(FControl).Caption := FText;
  if Assigned(FOnTextChange) then FOnTextChange(Self);
end;

procedure TCalculatorPanel.Error;
begin
  FStatus := csError;
  SetText('Error');
  if FBeepOnError then MessageBeep(0);
  if Assigned(FOnError) then FOnError(Self);
end;

procedure TCalculatorPanel.SetDisplay(R: Double);
var
  S: string;
begin
  S := FloatToStrF(R, ffGeneral, Max(2, FPrecision), 0);
  if FText <> S then begin
    SetText(S);
    if Assigned(FOnDisplayChange) then FOnDisplayChange(Self);
  end;
end;

function TCalculatorPanel.GetDisplay: Double;
begin
  if FStatus = csError then Result := 0.0
  else Result := StrToFloat(Trim(FText));
end;

procedure TCalculatorPanel.CheckFirst;
begin
  if FStatus = csFirst then begin
    FStatus := csValid;
    SetText('0');
  end;
end;

procedure TCalculatorPanel.UpdateMemoryLabel;
begin
  if FMemoryLabel <> nil then
    if FMemory <> 0.0 then FMemoryLabel.Caption := 'M'
    else FMemoryLabel.Caption := '';
end;

procedure TCalculatorPanel.CalcKey(Key: Char);
var
  R: Double;
begin
  Key := UpCase(Key);
  if (FStatus = csError) and (Key <> 'C') then Key := #0;
  if Assigned(FOnCalcKey) then FOnCalcKey(Self, Key);
  if CharInSet(Key, [FormatSettings.DecimalSeparator, '.', ',']) then begin
    CheckFirst;
    if Pos(FormatSettings.DecimalSeparator, FText) = 0 then
      SetText(FText + FormatSettings.DecimalSeparator);
    Exit;
  end;
  case Key of
    'R':
      if FStatus in [csValid, csFirst] then begin
        FStatus := csFirst;
        if GetDisplay = 0 then Error else SetDisplay(1.0 / GetDisplay);
      end;
    'Q':
      if FStatus in [csValid, csFirst] then begin
        FStatus := csFirst;
        if GetDisplay < 0 then Error else SetDisplay(Sqrt(GetDisplay));
      end;
    '0'..'9':
      begin
        CheckFirst;
        if FText = '0' then SetText('');
        if Pos('E', FText) = 0 then begin
          if Length(FText) < Max(2, FPrecision) + Ord(Boolean(Pos('-', FText))) then
            SetText(FText + Key)
          else if FBeepOnError then MessageBeep(0);
        end;
      end;
    #8:
      begin
        CheckFirst;
        if (Length(FText) = 1) or ((Length(FText) = 2) and (FText[1] = '-')) then
          SetText('0')
        else
          SetText(System.Copy(FText, 1, Length(FText) - 1));
      end;
    '_': SetDisplay(-GetDisplay);
    '+', '-', '*', '/', '=', '%', #13:
      begin
        if FStatus = csValid then begin
          FStatus := csFirst;
          R := GetDisplay;
          if Key = '%' then
            case FOperator of
              '+', '-': R := FOperand * R / 100.0;
              '*', '/': R := R / 100.0;
            end;
          case FOperator of
            '+': SetDisplay(FOperand + R);
            '-': SetDisplay(FOperand - R);
            '*': SetDisplay(FOperand * R);
            '/': if R = 0 then Error else SetDisplay(FOperand / R);
          end;
        end;
        FOperator := Key;
        FOperand := GetDisplay;
        if CharInSet(Key, ResultKeys) then
          if Assigned(FOnResult) then FOnResult(Self);
      end;
    #27, 'C': Clear;
    ^C: Copy;
    ^V: Paste;
  end;
end;

procedure TCalculatorPanel.Clear;
begin
  FStatus := csFirst;
  SetDisplay(0.0);
  FOperator := '=';
end;

type
  TscButtonClass = class(TscButton);

procedure TCalculatorPanel.CalcKeyPress(Sender: TObject; var Key: Char);
var
  Btn: TscButton;
begin
  Btn := FindButton(Key);
  if Btn <> nil then TscButtonClass(Btn).ButtonClick
  else CalcKey(Key);
end;

function TCalculatorPanel.FindButton(Key: Char): TscButton;
const
  ButtonChars = '0123456789_./*-+Q%R='#8'C';
var
  I: Integer;
  BtnTag: Longint;
begin
  if CharInSet(Key, [FormatSettings.DecimalSeparator, '.', ',']) then Key := '.'
  else if Key = #13 then Key := '='
  else if Key = #27 then Key := 'C';
  BtnTag := Pos(UpCase(Key), ButtonChars) - 1;
  if BtnTag >= 0 then
    for I := 0 to ControlCount - 1 do begin
      if Controls[I] is TscButton then begin
        Result := TscButton(Controls[I]);
        if Result.Tag = BtnTag then Exit;
      end;
    end;
  Result := nil;
end;

procedure TCalculatorPanel.BtnClick(Sender: TObject);
begin
  case TCalcButton(Sender).Kind of
    cbNum0..cbNum9: CalcKey(Char(TComponent(Sender).Tag + Ord('0')));
    cbSgn: CalcKey('_');
    cbDcm: CalcKey(FormatSettings.DecimalSeparator);
    cbDiv: CalcKey('/');
    cbMul: CalcKey('*');
    cbSub: CalcKey('-');
    cbAdd: CalcKey('+');
    cbSqr: CalcKey('Q');
    cbPcnt: CalcKey('%');
    cbRev: CalcKey('R');
    cbEql: CalcKey('=');
    cbBck: CalcKey(#8);
    cbClr: CalcKey('C');
    cbMP:
      if FStatus in [csValid, csFirst] then begin
        FStatus := csFirst;
        FMemory := FMemory + GetDisplay;
        UpdateMemoryLabel;
      end;
    cbMS:
      if FStatus in [csValid, csFirst] then begin
        FStatus := csFirst;
        FMemory := GetDisplay;
        UpdateMemoryLabel;
      end;
    cbMR:
      if FStatus in [csValid, csFirst] then begin
        FStatus := csFirst;
        CheckFirst;
        SetDisplay(FMemory);
      end;
    cbMC:
      begin
        FMemory := 0.0;
        UpdateMemoryLabel;
      end;
    cbOk:
      begin
        if FStatus <> csError then begin
          CalcKey('=');
          DisplayValue := DisplayValue; { to raise exception on error }
          if Assigned(FOnOk) then FOnOk(Self);
        end
        else if FBeepOnError then MessageBeep(0);
      end;
    cbCancel: if Assigned(FOnCancel) then FOnCancel(Self);
  end;
end;

procedure TCalculatorPanel.Copy;
begin
  Clipboard.AsText := FText;
end;

procedure TCalculatorPanel.Paste;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    try
      SetDisplay(StrToFloat(Trim(ReplaceStr(Clipboard.AsText,
        FormatSettings.CurrencyString, ''))));
    except
      SetText('0');
    end;
end;

constructor TscPopupCalculatorForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := scpbsFlat;
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  CalcEdit := nil;
  { DisplayPanel }
  FDisplayLabel := TscPanel.Create(Self);
  with FDisplayLabel do begin
    Align := alTop;
    StyleKind := scpsEdit;
    ShowCaption := True;
    Parent := Self;
    AutoSize := False;
    Alignment := taRightJustify;
    Caption := '0';
    Height := 25;
    Visible := True;
  end;
  { CalcPanel }
  FCalcPanel := TCalculatorPanel.CreateLayout(Self);
  with TCalculatorPanel(FCalcPanel) do begin
    Align := alTop;
    Parent := Self;
    FControl := FDisplayLabel;
    OnOkClick := OkClick;
    OnCancelClick := CancelClick;
    Visible := True;
  end;
end;

destructor TscPopupCalculatorForm.Destroy;
begin
  FDisplayLabel.Free;
  FCalcPanel.Free;
  inherited;
end;


procedure TscPopupCalculatorForm.Show(X, Y: Integer);
begin
  SetWindowPos(Handle, HWND_TOP, X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  Visible := True;
end;

procedure TscPopupCalculatorForm.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TscPopupCalculatorForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscPopupCalculatorForm.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TscPopupCalculatorForm.OkClick(Sender: TObject);
begin
  if CalcEdit <> nil
  then
    begin
      CalcEdit.Value := TCalculatorPanel(FCalcPanel).DisplayValue;
      CalcEdit.CloseUp;
    end;
end;

procedure TscPopupCalculatorForm.CancelClick(Sender: TObject);
begin
  if CalcEdit <> nil then CalcEdit.CloseUp;
end;

constructor TscCalcEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCalc := TscPopupCalculatorForm.Create(Self);
  FCalc.Visible := False;
  FCalc.CalcEdit := Self;
  if not (csDesigning in ComponentState) then
   FCalc.Parent := Self;
  FCalcButtonAnimation := False;
  RightButton.Visible := True;
  RightButton.ShowEllipses := True;
  FCalcWallpaperIndex := -1;
  FCalcWallpapers := nil;
  FValue := 0;
  ValueType := scvtFloat;
  FIncrement := 1;
  FDecimal := 2;
  StopCheck := True;
  Text := '0';
  FMemory := 0.0;
  FPrecision := DefCalcPrecision;
  StopCheck := False;
  FromEdit := False;
  OnRightButtonClick := ButtonClick;
  Width := 120;
  Height := 20;
end;

destructor TscCalcEdit.Destroy;
begin
  FCalc.Free;
  FCalc := nil;
  inherited;
end;

function TscCalcEdit.IsPopupVisible: Boolean;
begin
  Result := FCalc.Visible;
end;

procedure TscCalcEdit.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
end;

procedure TscCalcEdit.SetDisplayType(Value: TscNumEditDisplayType);
begin
  if FDisplayType <> Value then
  begin
    FDisplayType := Value;
    if FDisplayType = scedtCurrency then
      FDecimal := 2;
    Invalidate;
  end;
end;

function TscCalcEdit.IsCustomDraw(ADC: HDC): Boolean;
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

procedure TscCalcEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCalcWallpapers) then
  begin
    FCalcWallpapers := nil;
    if FCalc <> nil then FCalc.FCalcPanel.Wallpapers := nil;
  end;
end;

function TscCalcEdit.GetCalcFont;
begin
  Result := FCalc.Font;
end;

procedure TscCalcEdit.SetCalcFont;
begin
  FCalc.Font.Assign(Value);
end;

procedure TscCalcEdit.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (GetParent(Message.wParam) <> FCalc.Handle) and
     (GetParent(Message.wParam) <> FCalc.FCalcPanel.Handle)
  then
    CloseUp;
end;

procedure TscCalcEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> FCalc) and
     not FCalc.ContainsControl(Message.Sender)
  then
    CloseUp;
end;

procedure TscCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and FCalc.Visible then CloseUp;
  inherited;
end;

procedure TscCalcEdit.CloseUp;
begin
  SC_UnHookMouseMessages;
  if FCalc.Visible then FCalc.Hide;
end;

procedure TscCalcEdit.DropDown;
var
  I, Y, X: Integer;
  P: TPoint;
  WorkArea: TRect;
begin
 with FCalc do
  begin
    FCalcPanel.Wallpapers := Self.FCalcWallpapers;
    FCalcPanel.WallpaperIndex := Self.FCalcWallpaperIndex;
    case Self.FCalcBackgroundStyle  of
      sccbsPanel: FCalcPanel.StyleKind := scpsPanel;
      sccbsFormBackground: FCalcPanel.StyleKind := scpsFormBackground;
    end;
    FCalc.StyleKind := FCalcPanel.StyleKind;
    FDisplayLabel.Font.Assign(FCalc.Font);
    for I := 0 to FCalcPanel.ControlCount - 1 do
    if FCalcPanel.Controls[I] is TscButton then
    with TscButton(FCalcPanel.Controls[I]) do
    begin
      case FCalcButtonStyle of
        sccbsPushButton: StyleKind := scbsPushButton;
        sccbsToolButton: StyleKind := scbsToolButtonTransparent;
      end;
      Font.Assign(FCalc.Font);
      Animation := FCalcButtonAnimation;
    end
    else
    if FCalcPanel.Controls[I] is TscLabel then
    with TscLabel(FCalcPanel.Controls[I]) do
    begin
      Font.Assign(FCalc.Font);
    end;
    TCalculatorPanel(FCalcPanel).FMemory := Self.FMemory;
    TCalculatorPanel(FCalcPanel).UpdateMemoryLabel;
    TCalculatorPanel(FCalcPanel).FPrecision := Max(2, Self.Precision);
    TCalculatorPanel(FCalcPanel).FBeepOnError := False;
    if Self.FValue <> 0 then begin
      TCalculatorPanel(FCalcPanel).DisplayValue := Self.FValue;
      TCalculatorPanel(FCalcPanel).FStatus := csFirst;
      TCalculatorPanel(FCalcPanel).FOperator := '=';
    end;
    Width := Round(210 * FScaleFactor) + BtnOffset * 2;
    Height := FCalcPanel.Height + FDisplayLabel.Height + 2;
    P := Self.Parent.ClientToScreen(Point(Self.Left, Self.Top));
    X := P.X;
    Y := P.Y + Self.Height;
    WorkArea := Screen.MonitorFromWindow(Self.Handle).WorkAreaRect;
    if Y + FCalc.Height > WorkArea.Bottom then
      Y := Y - Self.Height - FCalc.Height;
    if X + FCalc.Width > WorkArea.Right then
      Dec(X, X + FCalc.Width - WorkArea.Right);
    if X < WorkArea.Left then
      X := WorkArea.Left;
    FCalc.Left := X;
    FCalc.Top := Y;
    FCalc.Show(X, Y);
    SC_HookMouseMessages(Self);
  end;
end;

procedure TscCalcEdit.ButtonClick(Sender: TObject);
begin
  if FCalc.Visible then CloseUp else DropDown;
end;

procedure TscCalcEdit.SetValueType(NewType: TscValueType);
begin
  if FValueType <> NewType
  then
    begin
      FValueType := NewType;
      if FValueType = scvtInteger
      then
        begin
          FIncrement := Round(FIncrement);
          if FIncrement = 0 then FIncrement := 1;
        end;
  end;
end;

procedure TscCalcEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then begin
    FDecimal := NewValue;
  end;
end;

function TscCalcEdit.CheckValue;
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

procedure TscCalcEdit.SetMinValue;
begin
  FMinValue := AValue;
end;

procedure TscCalcEdit.SetMaxValue;
begin
  FMaxValue := AValue;
end;

function TscCalcEdit.IsNumText;

function GetMinus: Boolean;
var
  I: Integer;
  S: String;
begin
  S := AText;
  I := Pos('-', S);
  if I > 1
  then
    Result := False
  else
    begin
      Delete(S, I, 1);
      Result := Pos('-', S) = 0;
    end;
end;

function GetP: Boolean;
var
  I: Integer;
  S: String;
begin
  S := AText;
  I := Pos(FormatSettings.DecimalSeparator, S);
  if I = 1
  then
    Result := False
  else
    begin
      Delete(S, I, 1);
      Result := Pos(FormatSettings.DecimalSeparator, S) = 0;
    end;
end;

const
  EditChars = '01234567890-';
var
  I: Integer;
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

  for I := 1 to Length(Text) do
  begin
    if Pos(Text[I], S) = 0
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

procedure TscCalcEdit.Change;
var
  NewValue, TmpValue: Double;
begin
  if FromEdit then Exit;
  if not StopCheck and IsNumText(Text)
  then
    begin
      if ValueType = scvtFloat
      then TmpValue := StrToFloat(Text)
      else TmpValue := StrToInt(Text);
      NewValue := CheckValue(TmpValue);
      if NewValue <> FValue
      then
        begin
          FValue := NewValue;
        end;
      if NewValue <> TmpValue
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

procedure TscCalcEdit.CMTextChanged;
begin
  inherited;
end;

procedure TscCalcEdit.SetValue;
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

procedure TscCalcEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then
  if FCalc.Visible
  then
    CloseUp
  else
    inherited KeyPress(Key);
end;

function TscCalcEdit.IsValidChar(Key: Char): Boolean;
var
  S: String;
  DecPos: Integer;
begin
  if ValueType = scvtInteger
  then
    Result := CharInSet(Key, ['-', '0'..'9']) or
     ((Key < #32) and (Key <> Chr(VK_RETURN)))
  else
  Result := CharInSet(Key, [FormatSettings.DecimalSeparator, '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));

  if ReadOnly and Result and ((Key >= #32) or
     (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)))
  then
    Result := False;

  if (Key = FormatSettings.DecimalSeparator) and (Pos(FormatSettings.DecimalSeparator, Text) <> 0)
  then
    Result := False
  else
  if (Key = '-') and (Pos('-', Text) <> 0)
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

procedure TscCalcEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp;
end;

constructor TscColorButton.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FCustomColorCursor := 0;
  FColorDialog := nil;
  FUseFontColorToStyleColor := False;
  FColorDialogCustom := nil;
  FColorValue := clBlack;
  FShowCaption := True;
  FWidthWithCaption := 0;
  FWidthWithoutCaption := 0;
  FColorImages := TCustomImageList.Create(Self);
  FColorMenu := TscGalleryMenu.Create(Self);
  FColorMenu.ButtonGlyphLeftAlignment := True;
  FColorMenu.Images := FColorImages;
  FColorMenu.OnItemClick := OnGalleryMenuClick;
  FColorMenu.OnMenuPopup := OnGalleryMenuPopup;
  FAutoColor := clBlack;
  FShowAutoColor := True;
  FShowMoreColor := True;
  FMoreCaption := 'More colors...';
  FAutoCaption := 'Automatic';
  for I := 1 to 8 do
    CustomColors[I] := clWhite;
  InitColors;
end;

procedure TscColorButton.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  InitColors;
end;

function TscColorButton.GetShowPopupColorsFromRight: Boolean;
begin
  Result := ShowGalleryMenuFromRight;
end;

procedure TscColorButton.SetShowPopupColorsFromRight(Value: Boolean);
begin
  ShowGalleryMenuFromRight := Value;
end;

procedure TscColorButton.SetShowCaption(Value: Boolean);
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

procedure TscColorButton.SetStyleKind(Value: TscColorButtonStyleKind);
begin
  if FStyleKind <> Value then
  begin
    FStyleKind := Value;
    RePaintControl;
  end;
end;

function TscColorButton.CanAnimateFocusedState: Boolean;
begin
  Result := CanFocused;
end;

procedure TscColorButton.SetShowAutoColor(Value: Boolean);
begin
  if FShowAutoColor <> Value
  then
    begin
      FShowAutoColor := Value;
      if not (csDesigning in ComponentState) and
         not (csLoading in ComponentState)
      then
        InitColors;
    end;
end;

procedure TscColorButton.SetShowCustomColors(Value: Boolean);
begin
  if FShowCustomColors <> Value
  then
    begin
      FShowCustomColors := Value;
      if not (csDesigning in ComponentState) and
         not (csLoading in ComponentState)
      then
        InitColors;
    end;
end;

procedure TscColorButton.SetShowMoreColor(Value: Boolean);
begin
  if FShowMoreColor <> Value
  then
    begin
      FShowMoreColor := Value;
      if not (csDesigning in ComponentState) and
         not (csLoading in ComponentState)
      then
        InitColors;
    end;
end;


procedure TscColorButton.SetColorValue;
begin
  FColorValue := Value;
  RePaintControl;
  if not (csDesigning in ComponentState) and
     not (csLoading in ComponentState)
  then
    if Assigned(FOnChangeColor) then FOnChangeColor(Self);
end;

procedure TscColorButton.InitSimpleColors;
var
  B: TBitMap;
  Item: TscGalleryMenuItem;
  I, J: Integer;
begin
  GalleryMenu := FColorMenu;
  DropDownMenu := nil;
  if FColorImages.Count <> 0 then FColorImages.Clear;
  if FColorMenu.Items.Count <> 0 then FColorMenu.Items.Clear;
  // init menu
  FColorMenu.ColumnsCount := 8;
  FColorMenu.ItemIndex := 0;
  // 1
  if FShowAutoColor
  then
    begin
      Item := TscGalleryMenuItem(FColorMenu.Items.Add);
      with Item do
      begin
        ImageIndex := 0;
        FColor := FAutoColor;
        Caption := FAutoCaption;
        Button := True;
      end;
    end;
  // 2
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 1;
    FColor := clBlack;
  end;
  // 3
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 2;
    FColor := clGray;
  end;
  // 4
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 3;
    FColor := clMaroon;
  end;
  // 5
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 4;
    FColor := clOlive;
  end;
  // 6
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 5;
    FColor := clGreen;
  end;
  // 7
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 6;
    FColor := clNavy;
  end;
  // 8
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 7;
    FColor := clTeal;
  end;
  // 9
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 8;
    FColor := clPurple;
  end;
  // 10
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 9;
    FColor := clWhite;
  end;
  // 11
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 10;
    FColor := clSilver;
  end;
  // 12
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 11;
    FColor := clRed;
  end;
  // 13
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 12;
    FColor := clYellow;
  end;
  // 14
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 13;
    FColor := clLime;
  end;
  // 15
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 14;
    FColor := clAqua;
  end;
  // 16
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 15;
    FColor := clBlue;
  end;
  // 17
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 16;
    FColor := clFuchsia;
  end;
  // init images
  FColorImages.Clear;
  FColorImages.Width := Round(12 * FScaleFactor);
  FColorImages.Height := Round(12 * FScaleFactor);
  B := TBitMap.Create;
  B.Width := Round(12 * FScaleFactor);
  B.Height := Round(12 * FScaleFactor);

  if not Self.FShowAutoColor
  then
    begin
      with B.Canvas do
      begin
        Brush.Color := clBlack;
        FillRect(Rect(0, 0, B.Width, B.Height));
      end;
      FColorImages.Add(B, nil);
    end;

  for I := 0 to FColorMenu.Items.Count - 1 do
  begin
    with B.Canvas do
    begin
      Brush.Color := FColorMenu.Items[I].FColor;
      FillRect(Rect(0, 0, B.Width, B.Height));
    end;
    FColorImages.Add(B, nil);
  end;

  if FShowMoreColor
  then
    begin
      Item := TscGalleryMenuItem(FColorMenu.Items.Add);
      with Item do
      begin
        ImageIndex := -1;
        FColor := clNone;
        Caption := FMoreCaption;
        Button := True;
      end;

      if FShowCustomColors then
      begin
        for J := 1 to 8 do
        begin
          Item := TscGalleryMenuItem(FColorMenu.Items.Add);
          with Item do
          begin
            FShowColor := True;
            ImageIndex := 16 + J;
            FColor := CustomColors[J];
            with B.Canvas do
            begin
              Brush.Color := FColor;
              FillRect(Rect(0, 0, B.Width, B.Height));
            end;
            Enabled := FColor <> clWhite;
          end;
          FColorImages.Add(B, nil);
        end;
      end;

    end;
  B.Free;
end;

procedure TscColorButton.InitColors;
var
  B: TBitMap;
  Item: TscGalleryMenuItem;
  I, J: Integer;
begin
  if FColorGridType = scgsSimple then
  begin
    InitSimpleColors;
    Exit;
  end;
  GalleryMenu := FColorMenu;
  DropDownMenu := nil;
  if FColorImages.Count <> 0 then FColorImages.Clear;
  if FColorMenu.Items.Count <> 0 then FColorMenu.Items.Clear;
  // init menu
  FColorMenu.ColumnsCount := 8;
  FColorMenu.ItemIndex := 0;
  if FShowAutoColor
  then
    begin
      Item := TscGalleryMenuItem(FColorMenu.Items.Add);
      with Item do
      begin
        ImageIndex := 0;
        FColor := FAutoColor;
        Caption := FAutoCaption;
        Button := True;
      end;
    end;
  // 2
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 1;
    FColor := clBlack;
  end;
  // 3
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 2;
    FColor := $00003399;;
  end;
  // 4
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 3;
    FColor := $00003333;
  end;
  // 5
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 4;
    FColor := $00003300;
  end;
  // 6
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 5;
    FColor := $00663300;
  end;
  // 7
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 6;
    FColor := clNavy;
  end;
  // 8
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 7;
    FColor := $00353333;
  end;
  // 9
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 8;
    FColor := $00333333;
  end;
  // 10
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 9;
    FColor := RGB(128, 0, 0);
  end;
  // 11
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 10;
    FColor := RGB(255, 102, 0);
  end;
  // 12
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 11;
    FColor := RGB(128, 128, 0);
  end;
  // 13
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 12;
    FColor := RGB(0, 128, 0);
  end;
  // 14
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 13;
    FColor := RGB(0, 128, 128);
  end;
  // 15
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 14;
    FColor := RGB(0, 0, 255);
  end;
  // 16
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 15;
    FColor := RGB(102, 102, 153);
  end;
  // 17
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 16;
    FColor := RGB(128, 128, 128);
  end;
  // 18
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 17;
    FColor :=  RGB(255, 0, 0);
  end;
  // 19
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 18;
    FColor :=  RGB(255, 153, 0);
  end;
  // 20
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 19;
    FColor :=  RGB(153, 204, 0);
  end;
  // 21
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 20;
    FColor := RGB(51, 153, 102);
  end;
  // 22
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 21;
    FColor := RGB(51, 204, 204);
  end;
  // 23
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 22;
    FColor := RGB(51, 102, 255);
  end;
  // 24
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 23;
    FColor := RGB(128, 0, 128);
  end;
  // 25
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 24;
    FColor := RGB(153, 153, 153);
  end;
  // 26
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 25;
    FColor := RGB(255, 0, 255);
  end;
  // 27
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 26;
    FColor := RGB(255, 204, 0);
  end;
  // 28
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 27;
    FColor := RGB(255, 255, 0);
  end;
  // 29
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 28;
    FColor := RGB(0, 255, 0);
  end;
  // 30
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 29;
    FColor := RGB(0, 255, 255);
  end;
  // 31
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 30;
    FColor := RGB(0, 204, 255);
  end;
  // 32
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 31;
    FColor := RGB(153, 51, 102);
  end;
  // 33
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 32;
    FColor := RGB(192, 192, 192);
  end;
  // 34
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 33;
    FColor := RGB(255, 153, 204);
  end;
  // 35
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 34;
    FColor := RGB(255, 204, 153);
  end;
  // 36
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 35;
    FColor := RGB(255, 255, 153);
  end;
  // 37
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 36;
    FColor := RGB(204, 255, 204);
  end;
  // 38
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 37;
    FColor := RGB(204, 255, 255);
  end;
  // 39
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 38;
    FColor := RGB(153, 204, 255);
  end;
  // 40
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 39;
    FColor := RGB(204, 153, 255);
  end;
  // 41
  Item := TscGalleryMenuItem(FColorMenu.Items.Add);
  with Item do
  begin
    ImageIndex := 40;
    FColor := RGB(255, 255, 255);
  end;

  // init images
  FColorImages.Width := Round(12 * FScaleFactor);
  FColorImages.Height := Round(12 * FScaleFactor);
  B := TBitMap.Create;
  B.Width := Round(12 * FScaleFactor);
  B.Height := Round(12 * FScaleFactor);

  if not Self.FShowAutoColor
  then
    begin
      with B.Canvas do
      begin
        Brush.Color := clBlack;
        FillRect(Rect(0, 0, B.Width, B.Height));
      end;
      FColorImages.Add(B, nil);
    end;

  for I := 0 to FColorMenu.Items.Count - 1 do
  begin
    with B.Canvas do
    begin
      Brush.Color := FColorMenu.Items[I].FColor;
      FillRect(Rect(0, 0, B.Width, B.Height));
    end;
    FColorImages.Add(B, nil);
  end;


  if FShowMoreColor
  then
    begin
      Item := TscGalleryMenuItem(FColorMenu.Items.Add);
      with Item do
      begin
        ImageIndex := -1;
        FColor := clNone;
        Caption := FMoreCaption;
        Button := True;
      end;

      if FShowCustomColors then
      begin
        for J := 1 to 8 do
        begin
          Item := TscGalleryMenuItem(FColorMenu.Items.Add);
          with Item do
          begin
            FShowColor := True;
            ImageIndex := 40 + J;
            FColor := CustomColors[J];
            with B.Canvas do
            begin
              Brush.Color := FColor;
              FillRect(Rect(0, 0, B.Width, B.Height));
            end;
            Enabled := FColor <> clWhite;
          end;
          FColorImages.Add(B, nil);
        end;
      end;

    end;
   B.Free;
end;

destructor TscColorButton.Destroy;
begin
  FColorImages.Clear;
  FColorImages.Free;
  FColorMenu.Free;
  inherited;
end;

procedure TscColorButton.Notification;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FColorDialog)
  then
    FColorDialog := nil;
  if (Operation = opRemove) and (AComponent = FColorDialogCustom)
  then
    FColorDialogCustom := nil;
end;

procedure TscColorButton.DoDialogChar;
begin
  ButtonClick;
end;

procedure TscColorButton.Loaded;
begin
  inherited;
  InitColors;
end;

procedure TscColorButton.OnGalleryMenuPopup(Sender: TObject);
var
  I, J: Integer;
begin
  FColorMenu.BackgroundStyle := FMenuBGStyle;

  if FShowMoreColor and FShowCustomColors then
  begin
    FCustomColorCursor := 0;
    for J := 1 to 8 do
    begin
      AddCustomColorIndex(J - 1, CustomColors[J]);
      if CustomColors[J] <> clWhite then
        Inc(FCustomColorCursor);
      if FCustomColorCursor > 7 then
        FCustomColorCursor := 0;
    end;
  end;

  J := -1;
  for I := 0 to FColorMenu.Items.Count - 1 do
  with FColorMenu.Items[I] do
  if FColor <> clNone then
  begin
    if ColorToRGB(FColor) = ColorToRGB(Self.ColorValue)
    then
      begin
        J := I;
        Break;
     end;
  end;

  FColorMenu.ItemIndex := J;

  if FShowAutoColor then
    FColorMenu.Items[0].Caption := FAutoCaption;
  if FShowMoreColor then
  begin
    if FShowCustomColors then
      FColorMenu.Items[FColorMenu.Items.Count - 9].Caption := FMoreCaption
    else
      FColorMenu.Items[FColorMenu.Items.Count - 1].Caption := FMoreCaption;
  end;

end;

procedure TscColorButton.AddCustomColorIndex(Index: Integer; Value: TColor);
var
  I: Integer;
  B: TBitmap;
begin
  I := FColorMenu.Items.Count - 8 + Index;
  B := TBitMap.Create;
  B.Width := Round(12 * FScaleFactor);
  B.Height := Round(12 * FScaleFactor);
  with B.Canvas do
  begin
    Brush.Color := Value;
    FillRect(Rect(0, 0, B.Width, B.Height));
  end;
  FColorMenu.Items[I].FColor := Value;
  FColorMenu.Items[I].Enabled := Value <> clWhite;
  if not FShowAutoColor then
    FColorImages.Replace(I, B, nil)
  else
    FColorImages.Replace(I - 1, B, nil);
  B.Free;
end;

procedure TscColorButton.AddCustomColor(Value: TColor);
var
  I, J: Integer;
  B: TBitmap;
begin
  J := -1;
  for I := 0 to FColorMenu.Items.Count - 1 do
  with FColorMenu.Items[I] do
  if FColor <> clNone then
  begin
    if ColorToRGB(FColor) = ColorToRGB(Self.ColorValue)
    then
      begin
        J := I;
        Break;
     end;
  end;
  if J = -1 then
  begin
    I := FColorMenu.Items.Count - 8 + FCustomColorCursor;
    // replace bitmap
    B := TBitMap.Create;
    B.Width := Round(12 * FScaleFactor);
    B.Height := Round(12 * FScaleFactor);
    with B.Canvas do
    begin
      Brush.Color := Value;
      FillRect(Rect(0, 0, B.Width, B.Height));
    end;
    FColorMenu.Items[I].FColor := Value;
    FColorMenu.Items[I].Enabled := Value <> clWhite;
    if not FShowAutoColor then
      FColorImages.Replace(I, B, nil)
    else
      FColorImages.Replace(I - 1, B, nil);
    B.Free;
    //
    Inc(FCustomColorCursor);
    CustomColors[FCustomColorCursor] := Value;
    if FCustomColorCursor > 7 then
      FCustomColorCursor := 0;
  end;
end;

procedure TscColorButton.OnGalleryMenuClick(Sender: TObject);
var
  CD: TColorDialog;
begin
  if FColorMenu.ItemIndex = -1 then Exit;
  if ((FColorMenu.ItemIndex = FColorMenu.Items.Count - 1) and
      FShowMoreColor and not FShowCustomColors) or
     ((FColorMenu.ItemIndex = FColorMenu.Items.Count - 9) and
      FShowMoreColor and FShowCustomColors)
  then
    begin
      if (FColorDialog = nil) and (FColorDialogCustom = nil)
      then
        begin
          CD := TColorDialog.Create(Self);
          CD.Color := ColorValue;
          try
          if CD.Execute then
          begin
            ColorValue := CD.Color;
            if FShowCustomColors then
              AddCustomColor(ColorValue);
          end;
          finally
            CD.Free;
          end;
        end
      else
      if (FColorDialog <> nil) then
      begin
        FColorDialog.Color := ColorValue;
        if FColorDialog.Execute then
        begin
          ColorValue := ColorDialog.Color;
          if FShowCustomColors then
            AddCustomColor(ColorValue);
        end;
      end
      else
      if (FColorDialogCustom <> nil) then
      begin
        FColorDialogCustom.Color := ColorValue;
        if FColorDialogCustom.Execute then
        begin
          ColorValue := FColorDialogCustom.Color;
          if FShowCustomColors then
            AddCustomColor(ColorValue);
        end;
      end;
    end
  else
    ColorValue := FColorMenu.SelectedItem.FColor;
end;

function TscColorButton.GetMenuFont: TFont;
begin
  Result := FColorMenu.ItemFont;
end;

procedure TscColorButton.SetMenuFont(Value: TFont);
begin
  FColorMenu.ItemFont.Assign(Value);
end;

procedure TscColorButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);

var
  R, TR, R1: TRect;
  TextColor, ArrowColor: TColor;
  FGlowColor: TColor;
  FGlowSize, FGlowAlpha: Byte;
  SaveIndex: Integer;
  FSplitFocusRect: Boolean;
  S: String;
  IIndex: Integer;
  IL: TCustomImageList;
  FInternalLayout: TButtonLayout;
begin
  R := Rect(0, 0, Width, Height);
  if TransparentBackground = False then
  begin
    ACanvas.Brush.Color := GetStyleColor(clBtnFace);
    ACanvas.FillRect(R);
  end;

  FInternalLayout := Layout;
  if BidiMode = bdRightToLeft then
    if FInternalLayout = blGlyphLeft then
      FInternalLayout := blGlyphRight
    else
    if FInternalLayout = blGlyphRight then
      FInternalLayout := blGlyphLeft;

  SaveIndex := 0;

  if FMenuDroppedDown and (StyleKind <> scclbTransparent) then
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

  FSplitFocusRect := SplitButton;

  case FStyleKind of
    scclbSegmentedLeft:
    begin
      DrawSegmentedButton(ACanvas, R, ACtrlState,
        CanFocused and Focused and (ShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, 1, FScaleFactor);
      TextColor := GetButtonTextColor(ACtrlState);
    end;
    scclbSegmentedMiddle:
    begin
      DrawSegmentedButton(ACanvas, R, ACtrlState,
        CanFocused and Focused and (ShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, 2, FScaleFactor);
      TextColor := GetButtonTextColor(ACtrlState);
    end;
    scclbSegmentedRight:
    begin
      DrawSegmentedButton(ACanvas, R, ACtrlState,
        CanFocused and Focused and (ShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, 2, FScaleFactor);
      TextColor := GetButtonTextColor(ACtrlState);
    end;
    scclbPushButton:
    begin
      DrawButton(ACanvas, R, ACtrlState,
        CanFocused and Focused and (ShowFocusRect or not StyleServices.Enabled)
          and not FSplitFocusRect, FScaleFactor);
      TextColor := GetButtonTextColor(ACtrlState);
    end;
    scclbSegmentedToolLeft:
    begin
      DrawSegmentedToolButton(ACanvas, R, ACtrlState, 1, FScaleFactor);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scclbSegmentedToolMiddle:
    begin
      DrawSegmentedToolButton(ACanvas, R, ACtrlState, 2, FScaleFactor);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scclbSegmentedToolRight:
    begin
      DrawSegmentedToolButton(ACanvas, R, ACtrlState, 3, FScaleFactor);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scclbToolButton:
    begin
      DrawToolButton(ACanvas, R, ACtrlState);
      TextColor := GetToolButtonTextColor(ACtrlState);
    end;
    scclbPushButtonTransparent:
    begin
      if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
      begin
        DrawButton(ACanvas, R, ACtrlState,
          CanFocused and Focused and (ShowFocusRect or not StyleServices.Enabled) and not FSplitFocusRect, FScaleFactor);
        TextColor := GetButtonTextColor(ACtrlState);
      end
      else
      begin
        if FUseFontColorToStyleColor and Enabled then
          TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          TextColor := GetCheckBoxTextColor(ACtrlState);
      end;
    end;
    scclbToolButtonTransparent:
    begin
      if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
      begin
        DrawToolButton(ACanvas, R, ACtrlState);
        TextColor := GetToolButtonTextColor(ACtrlState);
      end
      else
      begin
        if FUseFontColorToStyleColor and Enabled then
          TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          TextColor := GetCheckBoxTextColor(ACtrlState);
      end;
    end;
    scclbTransparent:
      begin
        if FUseFontColorToStyleColor and Enabled then
          TextColor := ColorToRGB(GetStyleColor(Self.Font.Color))
        else
          TextColor := GetCheckBoxTextColor(ACtrlState);
      end;
    else
      begin
        if FUseFontColorToStyleColor and Enabled then
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
  ACanvas.Font.Assign(Font);

  if (seFont in StyleElements) and (IsCustomStyle or (ACtrlState = scsDisabled)) then
     ACanvas.Font.Color := TextColor
  else
  if not Enabled then
    ACanvas.Font.Color := clGrayText;

  ArrowColor := ACanvas.Font.Color;

  case FStyleKind of
    scclbPushButton, scclbSegmentedLeft,
      scclbSegmentedRight, scclbSegmentedMiddle:
    begin
      if CanFocused and Focused and ShowFocusRect and not IsCustomStyle and FSplitFocusRect then
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
    scclbPushButtonTransparent:
    begin
      if (ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled) then
      begin
        if CanFocused and Focused and ShowFocusRect and not IsCustomStyle and FSplitFocusRect then
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
    scclbTransparent:
    begin
      if CanFocused and Focused and ShowFocusRect then
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

  if GalleryMenu <> nil then
  begin
    if SplitButton then
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

    if FMenuDroppedDown and (StyleKind <> scclbTransparent) then
    begin
      if StyleServices.Enabled then
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
            R1 := Rect(0, 0, FSplitWidth, Height);
        end
        else
          R1 := Rect(0, Height - FSplitWidth, Width, Height);
      end;
      try
        if StyleServices.Enabled then
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
           scclbSegmentedLeft:
           begin
             DrawSegmentedButton(ACanvas, R1, scsPressed,
             False, 1,FScaleFactor);
             ArrowColor := GetButtonTextColor(scsPressed);
           end;
           scclbSegmentedMiddle:
           begin
             DrawSegmentedButton(ACanvas, R1, scsPressed,
             False, 2,FScaleFactor);
             ArrowColor := GetButtonTextColor(scsPressed);
           end;
           scclbSegmentedRight:
           begin
             DrawSegmentedButton(ACanvas, R1, scsPressed,
             False, 3,FScaleFactor);
             ArrowColor := GetButtonTextColor(scsPressed);
           end;
           scclbSegmentedToolLeft:
           begin
             DrawSegmentedToolButton(ACanvas, R1, scsPressed, 1, FScaleFactor);
             ArrowColor := GetToolButtonTextColor(scsPressed);
           end;
           scclbSegmentedToolMiddle:
           begin
             DrawSegmentedToolButton(ACanvas, R1, scsPressed, 2, FScaleFactor);
             ArrowColor := GetToolButtonTextColor(scsPressed);
           end;
           scclbSegmentedToolRight:
           begin
             DrawSegmentedToolButton(ACanvas, R1, scsPressed, 3, FScaleFactor);
             ArrowColor := GetToolButtonTextColor(scsPressed);
           end;
           scclbPushButton, scclbPushButtonTransparent:
             begin
               DrawButton(ACanvas, R1, scsPressed, False);
               ArrowColor := GetButtonTextColor(scsPressed);
             end;
           scclbToolButton, scclbToolButtonTransparent:
             begin
               DrawToolButton(ACanvas, R1, scsPressed);
               ArrowColor := GetToolButtonTextColor(scsPressed);
             end;
         end;
      finally
       if StyleServices.Enabled then
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

    if (SC_MODERNARROWS and IsCustomStyle) then
      DrawModernArrowImage(ACanvas, TR, ArrowColor, FScaleFactor)
    else
      DrawArrowImage(ACanvas, TR, ArrowColor, 4, FScaleFactor);

    if SplitButton and ((StyleKind = scclbPushButton) or
     ((ACtrlState <> scsNormal) and (ACtrlState <> scsDisabled))) then
    begin
      if FArrowPosition = scapRight then
        DrawVertSplitter(ACanvas, TR, BidiMode = bdRightToLeft)
      else
        DrawHorzSplitter(ACanvas, TR);
    end;
  end;

  InflateRect(R, -3, -3);

  if GlowEffect.Enabled then
    InflateRect(R, -GlowEffect.GlowSize div 2, -GlowEffect.GlowSize div 2);

  S := Caption;
  IIndex := ImageIndex;
  IL := Images;

  if not FShowCaption then S := '';

  if GlowEffect.Enabled and GetGlowParams(ACtrlState, FGlowColor, FGlowSize, FGlowAlpha) then
  begin
    if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
      DrawImageAndTextWithGlow2(ACanvas, R, Margin, Spacing,
       FInternalLayout, S, IIndex, IL,
        ACtrlState <> scsDisabled, True, FColorValue,
        GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha, ImageGlow, False, IsRightToLeft,
        False, FScaleFactor)
    else
      DrawImageAndTextWithGlow(ACanvas, R, Margin, Spacing,
       FInternalLayout, S, IIndex, IL,
        ACtrlState <> scsDisabled, True, FColorValue,
        GlowEffect.Offset, FGlowColor, FGlowSize, GlowEffect.Intensive, FGlowAlpha, ImageGlow, False, IsRightToLeft,
        False, FScaleFactor);
  end
  else
    if (Layout = blGlyphLeft) or (Layout = blGlyphRight) then
      DrawImageAndText2(ACanvas, R, Margin, Spacing,
       FInternalLayout, S, IIndex, IL,
        ACtrlState <> scsDisabled, True, FColorValue, False, IsRightToLeft,
        False,
        FScaleFactor)
    else
      DrawImageAndText(ACanvas, R, Margin, Spacing,
       FInternalLayout, S, IIndex, IL,
       ACtrlState <> scsDisabled, True, FColorValue, False, IsRightToLeft,
       False,
       FScaleFactor);
end;

constructor TscFrameBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Frame := nil;
  FrameAdapter := nil;
  FFreezeState := False;
  FFrameBarItems := TscFrameBarItems(Collection);
  FrameButton := TscFrameBarItemButton.CreateEx(FFrameBarItems.FFrameBar);
  FrameButton.FrameItem := Self;
  FrameButton.OnClick := FrameButtonClick;
  FVisible := True;
  FImageIndex := -1;
  FEnabled := True;
  FrameState := scfsFrameClosed;
  FFrameBarItems.FFrameBar.AdjustControls(False);
end;

destructor TscFrameBarItem.Destroy;
begin
 if not (csDestroying in FFrameBarItems.FFrameBar.ComponentState) and
    (FrameButton <> nil)
 then
 begin
   FrameButton.Visible := False;
   FreeAndNil(FrameButton);
   if Frame <> nil then FreeAndNil(Frame);
 end;
 inherited Destroy;
 if not (csDestroying in FFrameBarItems.FFrameBar.ComponentState) then
   FFrameBarItems.FFrameBar.AdjustControls(False);
end;

procedure TscFrameBarItem.Assign(Source: TPersistent);
begin
  if Source <> nil then
  begin
    Caption := TscFrameBarItem(Source).Caption;
    ImageIndex := TscFrameBarItem(Source).ImageIndex;
    Visible := TscFrameBarItem(Source).Visible;
  end
  else
    inherited;
end;

procedure TscFrameBarItem.SetData(const Value: TCustomData);
begin
  FData := Value;
end;

procedure TscFrameBarItem.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  FrameButton.Enabled := Value;
end;

procedure TscFrameBarItem.SetCaption(Value: String);
begin
  FCaption := Value;
  FrameButton.Caption := Value;
end;

procedure TscFrameBarItem.SetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
  FrameButton.ImageIndex := Value;
end;

procedure TscFrameBarItem.SetIndex(Value: Integer);
begin
  inherited;
  FFrameBarItems.FFrameBar.AdjustControls(True);
end;

procedure TscFrameBarItem.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if FVisible then
      FrameButton.Parent := FFrameBarItems.FFrameBar
    else
      FrameButton.Parent := nil;
    FFrameBarItems.FFrameBar.AdjustControls(True);
  end;
end;

procedure TscFrameBarItem.FrameButtonClick;
var
  I, J: Integer;
  FUpdateItems: array of Integer;
begin
  if csDesigning in FFrameBarItems.FFrameBar.ComponentState then Exit;

  if Assigned(FrameButton) and Assigned(FOnClick) then
    FOnClick(FrameButton);

  if FFreezeState then Exit;

  case FrameState of
    scfsFrameOpened:
    begin
      if Assigned(FFrameBarItems.FFrameBar.FOnFrameChanging) then
       FFrameBarItems.FFrameBar.FOnFrameChanging(FFrameBarItems.FFrameBar, Self);

      if FFrameBarItems.FFrameBar.CanCloseAll then
      begin
        FFrameBarItems[Index].FrameState := scfsFrameCloseAction;
        if Assigned(FFrameBarItems[Index].FOnFrameClose) then
        begin
          J := Length(FUpdateItems);
          SetLength(FUpdateItems, J + 1);
          FUpdateItems[J] := Index;
        end;
      end;
    end;

    scfsFrameClosed:
    begin
      FrameState := scfsFrameOpenAction;
      if Assigned(FFrameBarItems.FFrameBar.FOnFrameChanging) then
        FFrameBarItems.FFrameBar.FOnFrameChanging(FFrameBarItems.FFrameBar, Self);

      if not FFrameBarItems.FFrameBar.CanOpenAll then
        for I := 0 to FFrameBarItems.Count - 1 do
          if FFrameBarItems[I].FrameState = scfsFrameOpened then
          begin
            FFrameBarItems[I].FrameState := scfsFrameCloseAction;
            if (I <> Index) and Assigned(FFrameBarItems[I].FOnFrameClose) then
            begin
              J := Length(FUpdateItems);
              SetLength(FUpdateItems, J + 1);
              FUpdateItems[J] := I;
            end;
          end;
    end;
  end;

  FFrameBarItems.FFrameBar.AdjustControls(True);

  if (FrameState = scfsFrameOpened) and Assigned(FOnFrameShow) then
     FOnFrameShow(Self);

  for I := 0 to Length(FUpdateItems) - 1 do
    if FUpdateItems[I] < FFrameBarItems.Count then
      if Assigned(FFrameBarItems[I].FOnFrameClose) then
        FFrameBarItems[FUpdateItems[I]].FOnFrameClose(FFrameBarItems[FUpdateItems[I]]);
end;

constructor TscFrameBarItems.Create(AOwner: TscFrameBar);
begin
  inherited Create(TscFrameBarItem);
  FFrameBar := AOwner;
end;

destructor TscFrameBarItems.Destroy;
begin
  inherited Destroy;
  FFrameBar := nil;
end;

function TscFrameBarItems.GetItem(Index: Integer): TscFrameBarItem;
begin
  Result := TscFrameBarItem(inherited GetItem(Index));
end;

function TscFrameBarItems.GetOwner: TPersistent;
begin
  Result := FFrameBar;
end;

procedure TscFrameBarItems.SetItem(Index: Integer; Value: TscFrameBarItem);
begin
  inherited SetItem(Index, Value);
  FFrameBar.AdjustControls(False);
end;

procedure TscFrameBarItems.Update(Item: TCollectionItem);
begin
  inherited;
  if UpdateCount = 0 then
    FFrameBar.AdjustControls(False);
end;

constructor TscFrameBarItemButton.CreateEx(AFrameBar: TscFrameBar);
begin
  inherited Create(AFrameBar);
  FrameBar := AFrameBar;
  TransparentBackground := FrameBar.ButtonTransparentBackground;
  CanFocused := False;
  Font.Assign(FrameBar.ButtonFont);
  Spacing := FrameBar.ButtonSpacing;
  Margin := FrameBar.ButtonMargin;
  Images := FrameBar.ButtonImages;
  Animation := FrameBar.ButtonAnimation;
  if FrameBar.ButtonStyle = scbbPushButton then
    StyleKind := scbsPushButton
  else
    StyleKind := scbsHeaderSection;
  FrameBar.AdjustControls(False);
end;

destructor TscFrameBarItemButton.Destroy;
begin
  inherited;
  FrameBar.AdjustControls(False);
end;

constructor TscFrameBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScalePercent := 100;
  FIgnoreAutoScroll := True;
  AutoScroll := False;
  HorzScrollBar.Visible := False;
  FItems := TscFrameBarItems.Create(Self);
  FButtonTransparentBackground := False;
  FButtonFont := TFont.Create;
  FButtonFont.OnChange := OnButtonFontChange;
  FButtonAnimation := False;
  FCanCloseAll := True;
  FCanOpenAll := True;
  FActiveFrameIndex := -1;
  FButtonHeight := 25;
  FButtonMargin := 10;
  FButtonSpacing := 5;
  FButtonAnimation := False;
  FAnimation := True;
  FCanChange := False;
end;

destructor TscFrameBar.Destroy;
begin
  FItems.Free;
  FItems := nil;
  FButtonFont.Free;
  inherited Destroy;
end;

procedure TscFrameBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FScalePercent := MulDiv(FScalePercent, M, D);
  FButtonFont.Height := MulDiv(FButtonFont.Height, M, D);
  FButtonHeight := MulDiv(FButtonHeight, M, D);
end;

function TscFrameBar.CalcClientRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -2, -2);
end;

procedure TscFrameBar.CloseFrame(Index: Integer);
begin
  Items[Index].FrameState := scfsFrameCloseAction;
  AdjustControls(True);
  if Assigned(Items[Index].FOnFrameClose) then
    Items[Index].FOnFrameClose(Items[Index]);
end;


procedure TscFrameBar.CloseAllFrames;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
     Items[I].FrameState := scfsFrameCloseAction;
  AdjustControls(True);
end;

procedure TscFrameBar.OpenAllFrames;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].FrameState := scfsFrameOpenAction;
  AdjustControls(True);
end;

procedure TscFrameBar.Loaded;
begin
  inherited;
  AdjustControls(True);
end;

procedure TscFrameBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = ButtonImages) then
    ButtonImages := nil;
end;

function TscFrameBar.GetScrollOffset: Integer;
var
  SI: TScrollInfo;
begin
  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_POS;
  if GetScrollInfo(Handle, SB_VERT, SI) then
    Result := SI.nPos
  else
    Result := 0;
end;

procedure TscFrameBar.OpenFrame(Index: Integer);
var
  l, I: Integer;
  ClosedItems: array of Integer;
begin
  if (Index < 0) or (Index > Items.Count - 1) then Exit;

  FActiveFrameIndex := Index;
  Items[Index].FrameState := scfsFrameOpenAction;

  if not CanOpenAll then begin
    for I := 0 to Items.Count - 1 do
      if Items[I].FrameState = scfsFrameOpened then begin
        Items[I].FrameState := scfsFrameCloseAction;
        if (I <> Index) and Assigned(Items[Index].FOnFrameClose) then begin
          l := Length(ClosedItems);
          SetLength(ClosedItems, l + 1);
          ClosedItems[l] := i;
        end;
      end;

    Items[Index].FrameState := scfsFrameOpened;
  end;

  AdjustControls(True);

  if Assigned(Items[Index].FOnFrameShow) then
    Items[Index].FOnFrameShow(Items[Index]);

  for I := 0 to Length(ClosedItems) - 1 do
    if ClosedItems[I] < Items.Count then
      if Assigned(Items[Index].FOnFrameClose) then
        Items[ClosedItems[I]].FOnFrameClose(Items[ClosedItems[I]]);
end;

procedure TscFrameBar.AdjustControls;
var
  I, H, W: Integer;
  ClRect: TRect;
  AnimationStep, AnimationCount, AnimationOffset: Integer;
  FCanDestroy: Boolean;
  TickCount: DWord;
begin
 if (csDestroying in ComponentState) or (csReading in ComponentState) or (Items.Count = 0) or
     not Visible or Adjusting then
  begin
    FActiveFrameIndex := -1;
    Exit;
  end;

  if Items.UpdateCount > 0 then
    Exit;

  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
      FAnimation and ACanAnimate then
    AnimationCount := 6
  else
    AnimationCount := 0;

  ClRect := CalcClientRect;
  Adjusting := True;
  H := 0;
  FCanChange := True;
  for AnimationStep := 0 to AnimationCount do
  begin
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
    H := 2;
    W := ClRect.Width;

    for I := 0 to Items.Count - 1 do
      if Items[I].Visible and (Items[I].FrameButton <> nil) then
      begin
        Items[I].FrameButton.SetBounds(ClRect.Left, H - GetScrollOffset, W, FButtonHeight);
        if Items[I].FrameButton.Parent <> Self then
          Items[I].FrameButton.Parent := Self;

        Inc(H, FButtonHeight);
        if (AnimationStep = 0) and (Items[I].Frame <> nil) then
          Items[I].FrameHeight := Items[I].Frame.Height;

        AnimationOffset := Items[I].FrameHeight;
        if (AnimationOffset = 0) and (Items[I].FrameState = scfsFrameOpenAction)
          and not Animation then
          Items[I].FrameState := scfsFrameOpened;

        case Items[I].FrameState of
          scfsFrameOpenAction:
          begin
            if AnimationStep = AnimationCount then
            begin
              if Items[I].Frame <> nil then
                AnimationOffset := Items[I].Frame.Height
            end
            else
            begin
              if AnimationCount <> 0 then
                AnimationOffset := Round((AnimationOffset / AnimationCount) * AnimationStep);
            end;

            TickCount := GetTickCount;

            if AdjustFrameBounds(I, H - GetScrollOffset, W, AnimationOffset, False) then
            begin
              if (AnimationStep = AnimationCount) then
                Items[I].FrameState := scfsFrameOpened;
              if AnimationCount > 0 then
                while TickCount + 1 > GetTickCount do;
            end;
          end;

         scfsFrameCloseAction:
         begin
            TickCount := GetTickCount;

            if AnimationCount <> 0 then
              AnimationOffset := Round((AnimationOffset / AnimationCount) *
                (AnimationCount - AnimationStep))
            else
              AnimationOffset := 0;

            if (AnimationStep = AnimationCount) then
            begin
              FCanDestroy := True;
              if Assigned(Items[I].FOnFrameDestroy) then
                 Items[I].FOnFrameDestroy(Items[I], Items[I].Frame, FCanDestroy);
              if FCanDestroy then
              begin
                FreeAndNil(Items[I].Frame);
                Items[I].FrameAdapter := nil;
              end;

              Items[I].FrameHeight := 0;
              AnimationOffset := 0;
              Items[I].FrameState := scfsFrameClosed;
              if Items[I].Frame <> nil then
                AdjustFrameBounds(I, H - GetScrollOffset, W, AnimationOffset, False);

              Continue;
            end;

            AdjustFrameBounds(I, H - GetScrollOffset, W, AnimationOffset, False);

              if AnimationCount > 0 then
                while TickCount + 1 > GetTickCount do;
          end;

          scfsFrameOpened:
          begin
            FActiveFrameIndex := i;
            AdjustFrameBounds(I, H - GetScrollOffset, W, 0, True);
            if (AnimationOffset = 0) and (Items[I].Frame <> nil) then
              AnimationOffset := Items[I].Frame.Height;
          end;

          scfsFrameClosed:
          begin
            if FActiveFrameIndex = I then
              FActiveFrameIndex := -1;

            if Items[I].Frame <> nil then begin
              FCanDestroy := True;
              if Assigned(Items[I].FOnFrameDestroy) then
                Items[I].FOnFrameDestroy(Items[I], Items[I].Frame, FCanDestroy);

              if FCanDestroy then
              begin
                FreeAndNil(Items[I].Frame);
                Items[I].FrameAdapter := nil;
              end;

              Items[I].FrameHeight := 0;
              AnimationOffset := 0;
              if Items[I].Frame <> nil then
                AdjustFrameBounds(I, H - GetScrollOffset, W, AnimationOffset, False);

              Items[I].FrameHeight := 0;
            end;
          end;
        end;
        if (Items[I].Frame <> nil) and (Items[I].FrameState <> scfsFrameClosed) then
        begin
          if Items[I].Frame.Parent = nil then
            Items[I].Frame.Parent := Self;
          Inc(H, AnimationOffset);
        end;
        if (Items[I].Frame <> nil) and (Items[I].FrameState = scfsFrameOpened) then
          SetWindowRgn(Items[I].Frame.Handle, 0, False);
      end;

    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    FullRedraw;
  end;

  FCanChange := False;

  if not AutoScroll then
  begin
    Inc(H, 2);
    if VertScrollBar.Range <> H then
      VertScrollBar.Range := H;
  end;

  Adjusting := False;
  AdjustSizes;
  if Showing then FullRedraw;
end;

procedure TscFrameBar.SetActiveFrameIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < Items.Count) then
  begin
    OpenFrame(Value);
    FActiveFrameIndex := Value;
  end
  else
    FActiveFrameIndex := -1;
end;

procedure TscFrameBar.OnButtonFontChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if Items[I].FrameButton <> nil then
      Items[I].FrameButton.Font.Assign(FButtonFont);
end;

procedure TscFrameBar.SetButtonFont;
begin
  FButtonFont.Assign(Value);
  OnButtonFontChange(Self);
end;

procedure TscFrameBar.SetButtonTransparentBackground(Value: Boolean);
var
  I: Integer;
begin
  if FButtonTransparentBackground <> Value then
  begin
     FButtonTransparentBackground := Value;
    for I := 0 to Items.Count - 1 do
      Items[I].FrameButton.TransparentBackground := FButtonTransparentBackground;
  end;
end;

procedure TscFrameBar.SetButtonStyle(Value: TscbbButtonStyle);
var
  I: Integer;
begin
  if FButtonStyle <> Value then
  begin
    FButtonStyle := Value;
    for I := 0 to Items.Count - 1 do
      if Items[I].FrameButton <> nil then
        if FButtonStyle = scbbPushButton then
          Items[I].FrameButton.StyleKind := scbsPushButton
        else
          Items[I].FrameButton.StyleKind := scbsHeaderSection;
  end;
end;

procedure TscFrameBar.SetButtonAnimation(Value: Boolean);
var
  I: Integer;
begin
  if FButtonAnimation <> Value then
  begin
    FButtonAnimation := Value;
    for I := 0 to Items.Count - 1 do
      if Items[I].FrameButton <> nil then
        Items[I].FrameButton.Animation := FButtonAnimation;
  end;
end;

procedure TscFrameBar.SetButtonImages(const Value: TCustomImageList);
var
  I: Integer;
begin
  if FButtonImages <> Value then
  begin
    FButtonImages := Value;
    for I := 0 to Items.Count - 1 do
      if Items[I].FrameButton <> nil then
        Items[I].FrameButton.Images := FButtonImages;
  end;
end;

procedure TscFrameBar.SetItems(const Value: TscFrameBarItems);
begin
  FItems.Assign(Value);
end;

procedure TscFrameBar.SetButtonMargin(const Value: Integer);
var
  I: Integer;
begin
  if FButtonMargin <> Value then
  begin
    FButtonMargin := Value;
    for I := 0 to Items.Count - 1 do
      if Items[I].FrameButton.Visible then
        Items[I].FrameButton.Margin := FButtonMargin;
  end;
end;

procedure TscFrameBar.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].FrameButton <> nil then
    begin
      if Enabled then
        Items[I].FrameButton.Enabled := Items[I].Enabled
       else
        Items[I].FrameButton.Enabled := Enabled;
     end;
    if Items[I].Frame <> nil then
      Items[I].Frame.Enabled := Enabled;
  end;
end;

procedure TscFrameBar.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Showing then AdjustControls(False);
end;

procedure TscFrameBar.SetButtonSpacing(const Value: Integer);
var
  I: Integer;
begin
  if FButtonSpacing <> Value then
  begin
    FButtonSpacing := Value;
    for I := 0 to Items.Count - 1 do
      if Items[I].FrameButton.Visible then
         Items[I].FrameButton.Spacing := FButtonSpacing;
  end;
end;

procedure TscFrameBar.SetButonHeight(const Value: Integer);
begin
  if FButtonHeight <> Value then begin
    FButtonHeight := Value;
    if not (csLoading in ComponentState) then
      AdjustControls(False);
  end;
end;

function TscFrameBar.AdjustFrameBounds(AIndex, APos, AWidth, AHeight: Integer; AOpen: Boolean): Boolean;
var
  FRgn: HRGN;
begin
  if Items.Count <= AIndex then Exit(False);

  Result := False;

  if (Items[AIndex].Frame = nil) and not (csDesigning in ComponentState) then
  begin
    if Assigned(Items[AIndex].OnFrameCreate) then
    begin
      Items[AIndex].OnFrameCreate(Items[AIndex], Items[AIndex].Frame);
      {$IFNDEF VER310_UP}
      if (FScalePercent > 100) and (Items[AIndex].Frame <> nil) then
        Items[AIndex].Frame.ScaleBy(FScalePercent, 100);
      {$ENDIF}
      if Items[AIndex].FrameAdapter = nil then
        Items[AIndex].FrameAdapter := TscFrameAdapter.Create(Items[AIndex].Frame);
    end
    else
      Items[AIndex].Frame := nil;

    if Items[AIndex].Frame <> nil then
    begin
      if FCanChange and Assigned(FOnFrameChange) then
        FOnFrameChange(Self, Items[AIndex]);
      FCanChange := False;
    end;
  end;

  if Items[AIndex].Frame <> nil then
  begin
    Result := True;

    if (Items[AIndex].FrameHeight = 0) then
      Items[AIndex].FrameHeight := Items[AIndex].Frame.Height;

    if AOpen then
    begin
      AHeight := Items[AIndex].FrameHeight;
      Items[AIndex].Frame.Height := Items[AIndex].FrameHeight;
    end;

    if AHeight = 0 then
    begin
      FRgn := CreateNullRgn;
      SetWindowRgn(Items[AIndex].Frame.Handle, FRgn, False);
      Items[AIndex].Frame.Visible := False;
    end
    else
      if AHeight = Items[AIndex].Frame.Height then
      begin
        SetWindowRgn(Items[AIndex].Frame.Handle, 0, False);
        Items[AIndex].Frame.Visible := True;
      end
      else
      begin
        FRgn := CreateRectRgn(0, Items[AIndex].Frame.Height - AHeight,
          AWidth, Items[AIndex].Frame.Height);
        SetWindowRgn(Items[AIndex].Frame.Handle, FRgn, False);
        Items[AIndex].Frame.Visible := True;
      end;

    Items[AIndex].Frame.SetBounds(Items[AIndex].FrameButton.Left, APos -
      (Items[AIndex].Frame.Height - AHeight), AWidth, Items[AIndex].Frame.Height);
  end;
end;

procedure TscFrameBar.AdjustSizes;
var
  I, W: Integer;
begin
  if Items.UpdateCount > 0 then Exit;
  Adjusting := True;
  W := CalcClientRect.Width;
  for I := 0 to Items.Count - 1 do
    if (Items[I].FrameButton <> nil) and Items[I].FrameButton.Visible and
        Items[I].Visible then
    begin
      Items[I].FrameButton.Width := W;
      if Items[I].Frame <> nil then
         Items[I].Frame.Width := W;
    end;
  Adjusting := False;
end;

procedure TscFrameBar.WMSIZE(var Msg: TMessage);
begin
  inherited;
  AdjustSizes;
end;


constructor TscTimeEdit.Create(AOwner: TComponent);
begin
  inherited;
  FShowUpDown := False;
  CharCase := ecUpperCase;
  FShowMSec := False;
  FShowSec := True;
  FTimeFormat := sctf24Hour;
  EditMask := '!90:00:00;1; ';
  Text := '00' + FormatSettings.TimeSeparator + '00' + FormatSettings.TimeSeparator + '00';
  FButtonsSystemSize := True;
end;

destructor TscTimeEdit.Destroy;
begin
  inherited;
end;

function TscTimeEdit.CanScaleButtons: Boolean;
begin
  Result := False;
end;

function TscTimeEdit.GetTextRect: TRect;
var
  R: TRect;
begin
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
      LeftButton.ButtonRect := Rect(R.Left, R.Top + R.Height div 2,
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

procedure TscTimeEdit.WMMOUSEWHEEL(var Message: TMessage);
begin
  if TWMMOUSEWHEEL(Message).WheelDelta > 0
  then
    UpButtonClick(Self)
  else
    DownButtonClick(Self);
end;

function TscTimeEdit.GetIncIndex: Integer;
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


procedure TscTimeEdit.UpButtonClick(Sender: TObject);
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

procedure TscTimeEdit.DownButtonClick(Sender: TObject);
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

procedure TscTimeEdit.ShowUpDownButtons;
begin
  LeftButton.StyleKind := scbsDownSpinButton;
  LeftButton.Width := GetSystemMetrics(SM_CYHSCROLL);
  LeftButton.Visible := True;
  LeftButton.RepeatClick := True;
  LeftButton.RepeatClickInterval := 100;
  RightButton.StyleKind := scbsUpSpinButton;
  RightButton.Width := GetSystemMetrics(SM_CYHSCROLL);
  RightButton.Visible := True;
  RightButton.RepeatClick := True;
  RightButton.RepeatClickInterval := 100;
  OnRightButtonClick := UpButtonClick;
  OnLeftButtonClick := DownButtonClick;
end;

procedure TscTimeEdit.HideUpDownButtons;
begin
  LeftButton.Visible := False;
  RightButton.Visible := False;
  OnRightButtonClick := nil;
  OnLeftButtonClick := nil;
end;

procedure TscTimeEdit.SetTimeFormat(Value: TscTimeFormat);
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

procedure TscTimeEdit.SetShowUpDown;
begin
  FShowUpDown := Value;
  if FShowUpDown then ShowUpDownButtons else HideUpDownButtons;
end;

procedure TscTimeEdit.ValidateEdit;
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

procedure TscTimeEdit.CheckSpace(var S: String);
var
  i: Integer;
begin
  for i := 0 to Length(S) do
  begin
    if S[i] = ' ' then S[i] := '0';
  end;
end;

procedure TscTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      UpButtonClick(Self);
    VK_DOWN:
      DownButtonClick(Self);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TscTimeEdit.KeyPress(var Key: Char);
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

procedure TscTimeEdit.SetShowSeconds(const Value: Boolean);
begin
  if FShowSec <> Value
  then
    begin
      FShowSec := Value;
      SetTimeFormat(FTimeFormat);
    end;
end;

procedure TscTimeEdit.SetShowMilliseconds(const Value: Boolean);
begin
   if FShowMSec <> Value
   then
     begin
       FShowMSec := Value;
       SetTimeFormat(FTimeFormat);
     end;
end;

procedure TscTimeEdit.SetMilliseconds(const Value: Integer);
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

function TscTimeEdit.GetMilliseconds: Integer;
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

procedure TscTimeEdit.SetTime(const Value: string);
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

function TscTimeEdit.GetTime: string;
begin
  Result := Text;
end;

function TscTimeEdit.IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  Result := ((AHour < 24) and (AMinute < 60) and
            (ASecond < 60) and (AMilliSecond < 1000));
end;

function TscTimeEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := CharInSet(Key, ['0'..'9']);
end;

procedure TscTimeEdit.SetValidTime(var H, M, S, MS: Word);
begin
  if H > 23 then H := 23;
  if M > 59 then M := 59;
  if S > 59 then S := 59;
  if MS > 999 then MS := 999;
end;

function TscTimeEdit.ValidateParameter;
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

function TscTimeEdit.GetTimeValue: TTime;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Hour, Min, Sec, MSec);
  Result := System.SysUtils.EncodeTime(Hour, Min, Sec, MSec);
end;

procedure TscTimeEdit.SetTimeValue(Value: TTime);
var
  Hour, Min, Sec, MSec: Word;
begin
  System.SysUtils.DecodeTime(Value, Hour, Min, Sec, MSec);
  Self.EncodeTime(Hour, Min, Sec, MSec);
end;

procedure TscTimeEdit.DecodeTime(var Hour, Min, Sec, MSec: Word);
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


procedure TscTimeEdit.EncodeTime(Hour, Min, Sec, MSec: Word);
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


{TscScrollPanel}

constructor TscScrollPanel.Create;
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FHorzScrollDirection := scspssdLeftToRight;
  FVertScrollDirection := scspssdTopToBottom;
  FCanScroll := True;
  FScrollButtonWidth := ScrollButtonSize;
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

procedure TscScrollPanel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  ScrollButtonWidth := MulDiv(FScrollButtonWidth , M, D);
  if FScrollOffset > 0 then
    FScrollOffset := MulDiv(FScrollOffset , M, D);
end;

procedure TscScrollPanel.InitTouch;
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

procedure TscScrollPanel.DrawBackground(ACanvas: TCanvas);
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

procedure TscScrollPanel.SetTouchScroll(Value: Boolean);
begin
  if FTouchScroll <> Value then
  begin
    FTouchScroll := Value;
    InitTouch;
  end;
end;

procedure TscScrollPanel.SetHorzScrollDirection(Value: TscScrollPanelHorzDirection);
begin
  if Value <> FHorzScrollDirection then
  begin
    FHorzScrollDirection := Value;
    GetScrollInfo;
    UpdateNC;
    RePaintControl;
  end;
end;

procedure TscScrollPanel.SetVertScrollDirection(Value: TscScrollPanelVertDirection);
begin
  if Value <> FVertScrollDirection then
  begin
    FVertScrollDirection := Value;
    GetScrollInfo;
    UpdateNC;
    RePaintControl;
  end;
end;

procedure TscScrollPanel.WMDESTROY(var Message: TMessage);
begin
  if TimerMode <> 0 then
    KillTimer(Handle, 1);
  if FMouseTimerActive then
    KillTimer(Handle, 2);
  inherited;
end;

procedure TscScrollPanel.AlignControls(AControl: TControl; var ARect: TRect);
begin
  inherited;
  if not FUpdatingScrollInfo then
    GetScrollInfo;
end;

procedure TscScrollPanel.WMSETCURSOR(var Message: TWMSetCursor);
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

procedure TscScrollPanel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, R1: TRect;
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
    case FStyleKind of
      scspsTabSheet:
      begin
        R1 := R;
        InflateRect(R1, 4, 4);
        scDrawUtils.DrawTabFrame(ACanvas, R1);
      end;
      scspsPanel:
      with ACanvas do
      begin
        if (seClient in StyleElements) and IsCustomStyle then
           Brush.Color := GetStyleColor(Self.Color)
        else
          Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(R);
      end;
      scspsToolBar:
      begin
        if IsCustomStyle then
          with ACanvas do
          begin
            Brush.Color := GetStyleColor(clBtnFace);
            Brush.Style := bsSolid;
            FillRect(R);
          end;
        DrawToolBar(ACanvas, R)
      end;
      scspsFormBackground:
      begin
        DrawFormBackground(ACanvas, R);
      end;
    end;
  end;

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomImageIndex)
  then
    FCustomImages.Draw(ACanvas, R, FCustomImageIndex, FScaleFactor);

  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex)
  then
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);
end;

procedure TscScrollPanel.SetScrollButtonWidth(Value: Integer);
begin
  if Value >= ScrollButtonSize then
  begin
    FScrollButtonWidth := Value;
    UpdateNC;
    RePaintControl;
  end;
end;

procedure TscScrollPanel.SetCustomImageIndex(Value: Integer);
begin
  if FCustomImageIndex <> Value then
  begin
    FCustomImageIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscScrollPanel.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscScrollPanel.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscScrollPanel.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscScrollPanel.CMSENCPaint(var Message: TMessage);
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

procedure TscScrollPanel.CMGesture(var Message: TCMGesture);
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
      if FVertScrollDirection = scspssdBottomToTop then
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
      if FHorzScrollDirection = scspssdRightToLeft then
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

procedure TscScrollPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscScrollPanel.SetStyleKind(Value: TscScrollPanelStyleKind);
begin
  if FStyleKind <> Value then
  begin
    if FStyleKind = scspsTransparent then
    begin
      FTransparentBackground := False;
      GetParentBG;
    end;
    FStyleKind := Value;
    if FStyleKind = scspsTransparent then
    begin
      FTransparentBackground := True;
      GetParentBG;
    end;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscScrollPanel.SetPosition(const Value: integer);
begin
  if Value <> SPosition
  then
    begin
      SPosition := Value;
      GetScrollInfo;
    end;
end;

procedure TscScrollPanel.Loaded;
begin
  inherited Loaded;
  if FAutoSize then UpDateSize;
  if FCanScroll then
     GetScrollInfo;
end;

destructor TscScrollPanel.Destroy;
begin
  inherited;
end;

procedure TscScrollPanel.UpDateSize;
begin
  SetBounds(Left, Top, Width, Height);
end;

procedure TscScrollPanel.StartTimer;
begin
  KillTimer(Handle, 1);
  if not FStartScroll and not FHotScroll then
    SetTimer(Handle, 1, 300, nil)
  else
    SetTimer(Handle, 1, Self.ScrollTimerInterval, nil);
end;

procedure TscScrollPanel.StopTimer;
begin
  KillTimer(Handle, 1);
  TimerMode := 0;
end;

procedure TscScrollPanel.WMTimer;
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

procedure TscScrollPanel.AdjustClientRect(var Rect: TRect);
var
  RLeft, RTop, VMax, HMax: Integer;
begin
  if FScrollType = scstHorizontal then
  begin
    RTop := 0;
    if FHorzScrollDirection = scspssdLeftToRight then
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
    if FVertScrollDirection = scspssdTopToBottom then
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

procedure TscScrollPanel.VScrollControls(AOffset: Integer);
begin
 if FVertScrollDirection = scspssdTopToBottom then
    ScrollBy(0, -AOffset)
  else
    ScrollBy(0, AOffset);
end;

procedure TscScrollPanel.ScrollBy(DeltaX, DeltaY: Integer);
begin
  FUpdatingScrollInfo := True;
  Perform(WM_SETREDRAW, 0, 0);
  inherited;
  Perform(WM_SETREDRAW, 1, 0);
  RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or
    RDW_ALLCHILDREN or RDW_UPDATENOW or RDW_FRAME);
  FUpdatingScrollInfo := False;
end;

procedure TscScrollPanel.HScrollControls(AOffset: Integer);
begin
  if FHorzScrollDirection = scspssdLeftToRight then
    ScrollBy(-AOffset, 0)
  else
    ScrollBy(AOffset, 0);
end;

procedure TscScrollPanel.SetBounds;
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

procedure TscScrollPanel.GetHRangeRTL;
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

procedure TscScrollPanel.GetHRange;
var
  i, FMax, W, MaxRight: Integer;
begin
  if FHorzScrollDirection = scspssdRightToLeft then
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

procedure TscScrollPanel.GetVRangeBTT;
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

procedure TscScrollPanel.GetVRange;
var
  i, MaxBottom, H: Integer;
  FMax: Integer;
begin
  if FVertScrollDirection = scspssdBottomToTop then
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

procedure TscScrollPanel.GetScrollInfo;
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

procedure TscScrollPanel.UpdateNC;
begin
  SetWindowPos(Handle, 0, Self.Left, Self.Top, Self.Width, Self.Height,
    SWP_FRAMECHANGED or SWP_DRAWFRAME or
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);
end;

procedure TscScrollPanel.SetButtonsVisible;
begin
  if Buttons[0].Visible <> AVisible
  then
    begin
      Buttons[0].Visible := AVisible;
      Buttons[1].Visible := AVisible;
      UpdateNC;
    end;
end;

procedure TscScrollPanel.ButtonDown(I: Integer);
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

procedure TscScrollPanel.ButtonUp(I: Integer);
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

procedure TscScrollPanel.ButtonClick;
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

   if ((FScrollType = scstHorizontal) and(FHorzScrollDirection = scspssdRightToLeft)) or
     ((FScrollType = scstVertical) and(FVertScrollDirection = scspssdBottomToTop))
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

procedure TscScrollPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  DoMouseLeave;
end;

procedure TscScrollPanel.DoMouseLeave;
begin
  if Buttons[0].MouseIn and Buttons[0].Visible then
  begin
    if TimerMode <> 0 then StopTimer;
    Buttons[0].MouseIn := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end
  else
  if Buttons[1].MouseIn and Buttons[1].Visible then
  begin
    if TimerMode <> 0 then StopTimer;
    Buttons[1].MouseIn := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TscScrollPanel.WndProc;
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

procedure TscScrollPanel.SetScrollOffset;
begin
  if Value >= 0 then FScrollOffset := Value;
end;

procedure TscScrollPanel.SetScrollType(Value: TscScrollType);
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

procedure TscScrollPanel.WMNCCALCSIZE;
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

procedure TscScrollPanel.WMNCPaint;
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

procedure TscScrollPanel.WMSIZE;
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

procedure TscScrollPanel.SetScrollTimerInterval;
begin
  if Value > 0 then FScrollTimerInterval := Value;
end;

procedure TscScrollPanel.DrawButton(Cnvs: TCanvas; I: Integer);
var
  ButtonState: TscsCtrlState;
  B: TBitmap;
  R: TRect;
begin
  if not Buttons[I].Visible then Exit;
  if ((HorzScrollDirection = scspssdRightToLeft) and (ScrollType = scstHorizontal)) or
     ((VertScrollDirection = scspssdBottomToTop) and (ScrollType = scstVertical))
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
    ButtonState := scsNormal;
    if Buttons[I].MouseIn and Buttons[I].Down then
      ButtonState := scsPressed
    else
     if Buttons[I].MouseIn then
        ButtonState := scsHot;
  end
  else
  begin
    if FCapturedButton = I + 1 then
      FCapturedButton := 0;
    Buttons[I].MouseIn := False;
    Buttons[I].Down := False;
    ButtonState := scsDisabled;
  end;
  B := TBitmap.Create;
  B.Width := Buttons[i].Rect.Width;
  B.Height := Buttons[i].Rect.Height;
  R := Rect(0, 0, B.Width, B.Height);
  with B.Canvas do
  begin
    Brush.Color := GetStyleColor(clBtnFace);
    FillRect(R);
  end;
  if Self.ScrollType = scstHorizontal then
  begin
    if I = 0 then
      scDrawUtils.DrawLeftSpinButton(B.Canvas, R, ButtonState, FScaleFactor)
    else
      scDrawUtils.DrawRightSpinButton(B.Canvas, R, ButtonState, FScaleFactor);
  end
  else
  begin
    if I = 0 then
      scDrawUtils.DrawUpSpinButton(B.Canvas, R, ButtonState, FScaleFactor)
    else
      scDrawUtils.DrawDownSpinButton(B.Canvas, R, ButtonState, FScaleFactor);
  end;
  Cnvs.Draw(Buttons[i].Rect.Left, Buttons[i].Rect.Top, B);
  B.Free;
end;

constructor TscFrameAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TFrame then
    FFrame := TFrame(AOwner)
  else
    FFrame := nil;
  if (FFrame <> nil) and not (csDesigning in ComponentState) then
  begin
    OldWindowProc := FFrame.WindowProc;
    FFrame.WindowProc := NewWndProc;
  end;
end;

destructor TscFrameAdapter.Destroy;
begin
  if not (csDesigning in ComponentState) and (FFrame <> nil)
  then
    FFrame.WindowProc := OldWindowProc;
  inherited;
end;

procedure TscFrameAdapter.NewWndProc(var Message: TMessage);
var
  FHandled: Boolean;
  FCanvas: TCanvas;
begin
  FHandled := False;
  case Message.Msg of
    WM_ERASEBKGND:
    begin
      if not StyleServices.Enabled then
      begin
        FCanvas := TCanvas.Create;
        try
          FCanvas.Handle := TWMERASEBKGND(Message).DC ;
          DrawParentBackground(FFrame, FCanvas);
        finally
          FCanvas.Handle := 0;
          FCanvas.Free;
        end;
        FHandled := True;
      end;
    end;
  end;
  if not FHandled then
    OldWindowProc(Message);
end;

// TscExPanel

constructor TscExPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  Font.Color := clBtnText;
  FHorzRollButtonPosition := scrbpRight;
  FBorderWidth := 1;
  FFrameColor := clBtnShadow;
  ButtonGlyphColor := clBtnText;
  FChangeRollStateWithCaptionClick := False;
  FCaptionRect := Rect(0, 0, 0, 0);
  FHideControlsInRollUpState := True;
  FHeaderColor := clBtnFace;
  FHeaderStyle := scexphsHeader;
  FMovingInParentClientBounds := True;
  FVisibleList := TList.Create;
  FTransparentBackground := False;
  FBackgroundStyle := scexbgsPanel;
  FShowFrame := True;
  FMoveable := False;
  FSizeable := False;
  FSpacing := 5;
  FCaptionHeight := 24;
  Width := 150;
  Height := 100;
  FRollKind := scrkRollVertical;
  ActiveButton := -1;
  OldActiveButton := -1;
  CaptureButton := -1;
  FShowRollButton := True;
  FShowCloseButton := True;
  FRollUpState := False;
  FRealWidth := 0;
  FRealHeight := 0;
  StopCheckSize := False;
  FCMaxWidth := 0;
  FCMinWidth := 0;
  FCMaxHeight := 0;
  FCMinHeight := 0;
  FCaptionImages := nil;
  FCaptionImageIndex := -1;
end;

destructor TscExPanel.Destroy;
begin
  FVisibleList.Free;
  inherited;
end;

procedure TscExPanel.SetHorzRollButtonPosition(Value: TscExPanelHorzRollButtonPosition);
begin
  if FHorzRollButtonPosition <> Value then
  begin
    FHorzRollButtonPosition := Value;
    if FShowRollButton then
      RePaintControl;
  end;
end;

procedure TscExPanel.SetBorderWidth(Value: Integer);
begin
  if (FBorderWidth <> Value) and (Value > 0) then
  begin
    FBorderWidth := Value;
    ReAlign;
  end;
end;

procedure TscExPanel.SetHeaderStyle(Value: TscExPanelHeaderStyle);
begin
  if FHeaderStyle <> Value then
  begin
    FHeaderStyle := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.SetHeaderColor(Value: TColor);
begin
  if FHeaderColor <> Value then
  begin
    FHeaderColor := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.SetButtonGlyphColor(Value: TColor);
begin
  if FButtonGlyphColor <> Value then
  begin
    FButtonGlyphColor := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  CaptionHeight := MulDiv(FCaptionHeight, M, D);
  if FRealWidth <> 0 then
    FRealWidth := MulDiv(FRealWidth, M, D);
  if FRealHeight <> 0 then
    FRealHeight:= MulDiv(FRealHeight, M, D);
end;

procedure TscExPanel.Loaded;
begin
  inherited;
  if FRollUpState then
    HideControls;
end;

procedure TscExPanel.SetBackgroundStyle(Value: TscExPanelBackgroundStyle);
begin
  if FBackgroundStyle <> Value then
  begin
    if FBackgroundStyle = scexbgsTransparent then
    begin
      TransparentBackground := False;
      GetParentBG;
    end;
    FBackgroundStyle := Value;
    if FBackgroundStyle = scexbgsTransparent then
    begin
      TransparentBackground := True;
      GetParentBG;
    end;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscExPanel.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  X, Y: Integer;
begin
  if FMoveable and (Align = alNone) and FMovingInParentClientBounds and
    (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
  begin
    X := Message.WindowPos^.x;
    Y := Message.WindowPos^.y;
    if X < 0 then X := 0;
    if Y < 0 then Y := 0;
   if X + Width > Parent.ClientWidth then
      X := Parent.ClientWidth - Width;
    if Y + Height > Parent.ClientHeight then
      Y := Parent.ClientHeight - Height;
    Message.WindowPos^.x := X;
    Message.WindowPos^.y := Y;
  end;

  if FSizeable and (Align <> alClient) and
    (Message.WindowPos^.flags and SWP_NOSIZE = 0) then
  begin
    X := Message.WindowPos^.cx;
    Y := Message.WindowPos^.cy;

    if Align = alRight then
    begin
      if (Constraints.MaxWidth <> 0) and (X > Constraints.MaxWidth) then
        Message.WindowPos^.x := Self.Left;
      if (Constraints.MinWidth <> 0) and (X < Constraints.MinWidth) then
        Message.WindowPos^.x := Self.Left;
    end;

    if Align = alBottom then
    begin
      if (Constraints.MaxHeight <> 0) and (Y > Constraints.MaxHeight) then
        Message.WindowPos^.y := Self.Top;
      if (Constraints.MinHeight <> 0) and (Y < Constraints.MinHeight) then
        Message.WindowPos^.y := Self.Top;
    end;
  end;

  inherited;
end;


procedure TscExPanel.WMMOVING(var Message: TWMMOVING);
begin
  inherited;
  if Self.FMoveable then
  begin
    if (Align = alNone) and (Parent <> nil) and (Parent is TScrollBox) then
      Parent.Realign;
  end;
end;

procedure TscExPanel.WMSIZING(var Message: TWMSIZE);
var
  R: TRect;
begin
  inherited;
  if FSizeable then
  begin
    R := ClientRect;
    AdjustClientRect(R);
    ReAlign;
    if (Align <> alNone) and (Align <> alClient) and (Parent <> nil) then
      Parent.Realign;
  end;
end;

procedure TscExPanel.WMSIZE(var Message: TWMSIZE);
var
  R: TRect;
begin
  inherited;
  if FSizeable and (Align = alNone) then
  begin
    R := ClientRect;
    AdjustClientRect(R);
    ReAlign;
  end;
end;

procedure TscExPanel.WndProc(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnCaptionDblClick) and FMoveable and (Message.Msg = WM_NCLBUTTONDBLCLK) and
    (TWMNCLBUTTONDBLCLK(Message).HitTest = HTCAPTION)
  then
    FOnCaptionDblClick(Self);
end;

procedure TscExPanel.WMNCHitTest(var Message: TWMNCHitTest);
var
  RBRect: TRect;
  RRect: TRect;
  BRect: TRect;
  LRect: TRect;
  TpRect: TRect;
  CR: TRect;
  P: TPoint;
  Offset: Integer;
begin
  if FCaptionHeight = 0 then
  begin
    inherited;
    Exit;
  end;
  If (FSizeable or FMoveable) and not (csDesigning in ComponentState) and ((Align = alNone) or (Align <> alClient))
  then
    begin
      Offset := FBorderWidth + 1;
      P := ScreenToClient(SmallPointToPoint(TWMNCHitTest(Message).Pos));
      CR := Rect (2, 2, Width - 2, FCaptionHeight - 1);
      RBRect := Rect(Width - Offset, Height - Offset, Width, Height);
      RRect := Rect(Width - Offset, CR.Bottom, Width, Height - Offset);
      BRect := Rect(0, Height - Offset, Width - Offset, Height);
      LRect := Rect(0, CR.Bottom, Offset, Height);
      TpRect := Rect(0, 0, Width, 2);
      if FMoveable and
        PointInRect(CR, P) and
        not (FShowCloseButton and PointInRect(Buttons[0].Rect, P)) and
        not (FShowRollButton and PointInRect(Buttons[1].Rect, P))
      then
        begin
          Message.Result := HTCAPTION;
          if FCaptionCaptured then
          begin
            FCaptionCaptured := False;
            if Assigned(FOnCaptionClick) then
              FOnCaptionClick(Self);
          end;
          if ActiveButton <> -1
          then
            begin
              ActiveButton := -1;
              Buttons[0].MouseIn := False;
              Buttons[1].MouseIn := False;
              RePaint;
            end;
        end
      else
      if FSizeable and PtInRect(RBRect, P) and not FRollUpState and (Align = alNone)
      then
        Message.Result := HTBOTTOMRIGHT
      else
      if FSizeable and PtInRect(BRect, P) and not FRollUpState and ((Align = alNone) or (Align = alTop))
      then
        Message.Result := HTBOTTOM
      else
      if FSizeable and PtInRect(RRect, P) and not FRollUpState and ((Align = alNone) or (Align = alLeft))
      then
        Message.Result := HTRIGHT
      else
      if FSizeable and PtInRect(LRect, P) and not FRollUpState and (Align = alRight)
      then
        Message.Result := HTLEFT
      else
      if FSizeable and PtInRect(TpRect, P) and not FRollUpState and (Align = alBottom)
      then
        Message.Result := HTTOP
      else
        inherited;
    end
  else
    inherited;
end;

procedure TscExPanel.Notification;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCaptionImages) then
    FCaptionImages := nil;
end;

procedure TscExPanel.SetShowFrame(Value: Boolean);
begin
  if FShowFrame <> Value then
  begin
    FShowFrame := Value;
    RePaintControl;
    ReAlign;
  end;
end;

procedure TscExPanel.SetCaptionImageIndex(Value: Integer);
begin
  if FCaptionImageIndex <> Value then
  begin
    FCaptionImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.SetCaptionImages(Value: TCustomImageList);
begin
  if FCaptionImages <> Value then
  begin
    FCaptionImages := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.SetSpacing;
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.Close;
begin
  Visible := False;
  if not (csDesigning in ComponentState) and
    Assigned(FOnClose)
  then
    FOnClose(Self);
  if (Parent <> nil) and (Parent is TScrollBox) and (Align <> alNone) and
    not (csLoading in ComponentState)
  then
    TScrollBox(Parent).Realign;
end;

procedure TscExPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FRollUpState and not StopCheckSize
  then
    begin
      if (FRollKind = scrkRollHorizontal) and (AWidth <> GetRollWidth)
      then
        AWidth := GetRollWidth
      else
      if (FRollKind = scrkRollVertical) and (AHeight <> GetRollHeight)
      then
        AHeight := GetRollHeight
    end;
  inherited;
end;

procedure TscExPanel.CMTextChanged;
begin
  inherited;
  if FCaptionHeight > 0 then
    RePaintControl;
end;

procedure TscExPanel.SetShowRollButton(Value: Boolean);
begin
  FShowRollButton := Value;
  if FCaptionHeight > 0 then
    RePaintControl;
end;

procedure TscExPanel.SetShowCloseButton(Value: Boolean);
begin
  FShowCloseButton := Value;
  if FCaptionHeight > 0 then
    RePaintControl;
end;

function TscExPanel.GetRollWidth: Integer;
begin
  Result := FCaptionHeight;
end;

function TscExPanel.GetRollHeight: Integer;
begin
  Result := FCaptionHeight;
end;

procedure TscExPanel.SetRollKind(Value: TscExPanelRollKind);
begin
  if FRollKind <> Value then
  begin
    FRollKind := Value;
    RePaintControl;
  end;
end;

procedure TscExPanel.SetCaptionHeight;
begin
  if (FCaptionHeight <> Value) and (Value >= 0) then
  begin
    FCaptionHeight := Value;
    if FRollUpState and not (csLoading in ComponentState) then
    begin
      case FRollKind of
        scrkRollHorizontal:
           Width := FCaptionHeight;
        scrkRollVertical:
           Height := FCaptionHeight;
        end;
      RePaintControl;
    end
    else
    if not FRollUpState then
    begin
      if not (csLoading in ComponentState) then
        RePaintControl;
      ReAlign;
    end;
  end;
end;

procedure TscExPanel.Draw;
var
  R, CR: TRect;
  BW, CROffset, TX, TY, GX, GY: Integer;
  B, B1: TBitmap;
  TextColor: TColor;
begin
  // draw background
  R := Rect(0, 0, Width, Height);
  case FBackgroundStyle of
    scexbgsFormBackground:
    begin
      scDrawUtils.DrawFormBackground(ACanvas, R);
    end;
    scexbgsPanel:
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := GetStyleColor(Self.Color);
      ACanvas.FillRect(R);
    end;
  end;

  if not FGetControlBG then
  begin
    if FShowFrame then
      CR := Rect(0, 1, Width, FCaptionHeight)
    else
      CR := Rect(0, 0, Width, FCaptionHeight);
    FCaptionRect := CR;

    if FCaptionHeight > 0 then
    begin
      if FRollUpState and (FRollKind = scrkRollHorizontal) then
       begin
         if FShowFrame then
           CR := Rect(1, 0, FCaptionHeight, Height)
         else
           CR := Rect(0, 0, FCaptionHeight, Height);
         FCaptionRect := CR;

         if FHeaderStyle = scexphsHeader then
         begin
           B := TBitmap.Create;
           B.PixelFormat := pf32bit;
           B.SetSize(40, 40);
           B1 := TBitmap.Create;
           B1.PixelFormat := pf32bit;
           B1.SetSize(40, 40);
           try
             with B.Canvas do
             begin
               Brush.Color := GetStyleColor(clBtnFace);
               FillRect(Rect(0, 0, B.Width, B.Height));
             end;
             scDrawUtils.DrawHeaderSection(B.Canvas,
             Rect(0, 0, B.Width, B.Height), scsNormal);
             Bitmap_Rotate90_1(B, B1);
             DrawStrecthBitmap(10, 10, 10, 10, B1, ACanvas, CR);
           finally
             B.Free;
             B1.Free;
           end;
         end
         else
         if FHeaderStyle = scexphsCategoryHeader then
         begin
           B := TBitmap.Create;
           B.PixelFormat := pf32bit;
           B.SetSize(40, 40);
           B1 := TBitmap.Create;
           B1.PixelFormat := pf32bit;
           B1.SetSize(40, 40);
           try
             with B.Canvas do
             begin
               Brush.Color := GetStyleColor(clBtnFace);
               FillRect(Rect(0, 0, B.Width, B.Height));
             end;
             scDrawUtils.DrawCategoryHeader(B.Canvas,
             Rect(0, 0, B.Width, B.Height));
             Bitmap_Rotate90_1(B, B1);
             DrawStrecthBitmap(10, 10, 10, 10, B1, ACanvas, CR);
           finally
             B.Free;
             B1.Free;
           end;
         end
         else
         begin
           ACanvas.Brush.Style := bsSolid;
           ACanvas.Brush.Color := GetStyleColor(FHeaderColor);
           ACanvas.FillRect(CR);
         end;
       end
      else
        if FHeaderStyle = scexphsHeader then
          DrawHeaderSection(ACanvas, CR, scsNormal)
        else
        if FHeaderStyle = scexphsCategoryHeader then
          DrawCategoryHeader(ACanvas, CR)
        else
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := GetStyleColor(FHeaderColor);
          ACanvas.FillRect(CR);
        end;
    end;
  end;
  // draw frame
  if FShowFrame then
    Frm3D(ACanvas, R, GetStyleColor(FFrameColor),
      GetStyleColor(FFrameColor));
  //
  if FCaptionHeight = 0 then Exit;
  //
  BW := FCaptionHeight - 6;
  ACanvas.Font.Assign(Self.Font);
  if IsCustomStyle and (seFont in StyleElements) then
  begin
    if FHeaderStyle = scexphsHeader then
      ACanvas.Font.Color := scDrawUtils.GetHeaderTextColor(scsNormal)
    else
    if FHeaderStyle = scexphsCategoryHeader then
      ACanvas.Font.Color := scDrawUtils.GetCategoryHeaderTextColor;
  end;
  if not Self.Enabled then
    ACanvas.Font.Color := GetCheckBoxTextColor(scsDisabled);
  TextColor := ColorToRGB(ACanvas.Font.Color);
  if FRollUpState and (FRollKind = scrkRollHorizontal)
  then
    with ACanvas do
    begin
      CR := R;
      if FShowCloseButton
      then
        begin
          begin
            Buttons[0].Rect := Rect(3, 3, 3 + BW, 3 + BW);
          end;
        end
      else
        Buttons[0].Rect := Rect(0, 0, 0, 3);

      if FShowRollButton
      then
        begin
          Buttons[1].Rect := Rect(3, Buttons[0].Rect.Bottom, 3 + BW,
            Buttons[0].Rect.Bottom + BW);
        end
      else
        Buttons[1].Rect := Rect(0, 0, 0, 0);
      TX := CR.Left + RectWidth(CR) div 2 - TextHeight(Caption) div 2 - 2;
      TY := CR.Bottom - 3;
      if (FCaptionImages <> nil) and (FCaptionImageIndex >= 0) and
       (FCaptionImageIndex < FCaptionImages.Count)
       then
         begin
           GX := CR.Left + RectWidth(CR) div 2 - FCaptionImages.Width div 2;
           GY := CR.Bottom - FCaptionImages.Height - 2;
           FCaptionImages.Draw(ACanvas, GX, GY, FCaptionImageIndex, Enabled);
           TY := TY - FCaptionImages.Height - FSpacing - 2;
         end;
      Brush.Style := bsClear;
      AngleDrawText(ACanvas, 900, TX, TY, Caption, TextColor);
    end
  else
    with ACanvas do
    begin
      CR := Rect(2, 0, Width - 2, FCaptionHeight);
      CROffset := 0;
      if FShowCloseButton then
      begin
        Buttons[0].Rect := Rect(Width - BW - 3, 3, Width - 3, 3 + BW);
        CROffset := CROffset + RectWidth(Buttons[1].Rect);
      end
      else
        Buttons[0].Rect := Rect(Width - 3, 0, 0, 0);
      if FShowRollButton then
      begin
        if FHorzRollButtonPosition = scrbpRight then
        begin
          Buttons[1].Rect := Rect(Buttons[0].Rect.Left - BW, 3,
            Buttons[0].Rect.Left, 3 + BW);
          CROffset := CROffset + RectWidth(Buttons[1].Rect);
        end
        else
        begin
          Buttons[1].Rect := Rect(3, 3, 3 + BW, 3 + BW);
          CR.Left := 4 + RectWidth(Buttons[1].Rect);
        end;
      end
      else
        Buttons[1].Rect := Rect(0, 0, 0, 0);
      Inc(CR.Left, 2);
      Dec(CR.Right, CROffset + 2);
      FCaptionRect := CR;
      if (FCaptionImages <> nil) and (FCaptionImageIndex >= 0) and
         (FCaptionImageIndex < FCaptionImages.Count) then
      begin
        GX := CR.Left;
        GY := CR.Top + RectHeight(CR) div 2 - FCaptionImages.Height div 2;
        FCaptionImages.Draw(ACanvas, GX, GY, FCaptionImageIndex, Enabled);
          Inc(CR.Left, FCaptionImages.Width + FSpacing);
      end;
      Brush.Style := bsClear;
      SetBkMode(ACanvas.Handle, TRANSPARENT);
      SetTextColor(ACanvas.Handle, TextColor);
      scDrawText(ACanvas, Caption, CR, IsRightToLeft, True);
    end;

  if FShowCloseButton then
    DrawButton(ACanvas, 0);
  if FShowRollButton then
    DrawButton(ACanvas, 1);
end;

procedure TscExPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Rect.Top := Rect.Top + FCaptionHeight;
  if FShowFrame then
  begin
    Inc(Rect.Left, FBorderWidth);
    Dec(Rect.Right, FBorderWidth);
    Dec(Rect.Bottom, FBorderWidth);
  end;
end;

procedure TscExPanel.ShowControls;
var
  I: Integer;
begin
  if not FHideControlsInRollUpState then Exit;

  if not (csDesigning in ComponentState) then
  begin
    if FVisibleList.Count = 0 then Exit;
    for I := 0 to FVisibleList.Count - 1 do
      TControl(FVisibleList[I]).Visible := True;
    FVisibleList.Clear;
  end;

  if AlignDisabled then
    EnableAlign;

  ReAlign;
end;


procedure TscExPanel.HideControls;
var
  I: Integer;
begin
  if not FHideControlsInRollUpState then Exit;

  if not AlignDisabled then
    DisableAlign;

  if not (csDesigning in ComponentState) then
  begin
    if FVisibleList.Count > 0 then Exit;
    for I := 0 to ControlCount - 1 do
    if Controls[I].Visible then
    begin
      Controls[I].Visible := False;
      FVisibleList.Add(Pointer(Controls[I]));
    end;
  end;
end;

procedure TscExPanel.SetRollUpState;
begin
  if FRollUpState = Value then Exit;
  FRollUpState := Value;
  StopCheckSize := True;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState)
    and Assigned(FOnChangingRollUpState)
  then
    FOnChangingRollUpState(Self);

  if FRollUpState then
  begin
    FCMaxWidth := Constraints.MaxWidth;
    FCMinWidth := Constraints.MinWidth;
    FCMaxHeight := Constraints.MaxHeight;
    FCMinHeight := Constraints.MinHeight;
    Constraints.MaxWidth := 0;
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    Constraints.MinWidth := 0;

    HideControls;

    case FRollKind of
      scrkRollVertical:
        if FRealHeight = 0 then
        begin
          FRealHeight := Height;
          Height := GetRollHeight;
        end;
      scrkRollHorizontal:
        if FRealWidth = 0 then
        begin
          FRealWidth := Width;
          Width := GetRollWidth;
        end;
   end;
  end
  else
  begin
    Constraints.MaxWidth := FCMaxWidth;
    Constraints.MinWidth := FCMinWidth;
    Constraints.MaxHeight := FCMaxHeight;
    Constraints.MinHeight := FCMinHeight;
    Buttons[0].MouseIn := False;
    Buttons[1].MouseIn := False;
    case FRollKind of
      scrkRollVertical:
        begin
          Height := FRealHeight;
          FRealHeight := 0;
        end;
      scrkRollHorizontal:
        begin
          Width := FRealWidth;
          FRealWidth := 0;
        end;
    end;

    ShowControls;
  end;
  StopCheckSize := False;
  if (Parent <> nil) and (Parent is TScrollBox) and (Align <> alNone) and
    not (csLoading in ComponentState)
  then
    TScrollBox(Parent).Realign;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState)
    and Assigned(FOnChangeRollUpState)
  then
    FOnChangeRollUpState(Self);
end;

procedure TscExPanel.CMMouseEnter;
begin
  inherited;
  if (csDesigning in ComponentState) or (FCaptionHeight = 0) then
    Exit;
  TestActive(-1, -1);
end;

procedure TscExPanel.CMMouseLeave;
var
  I: Integer;
begin
  inherited;
  if (csDesigning in ComponentState) or (FCaptionHeight = 0) then
    Exit;
  for I := 0 to 1 do
    if Buttons[I].MouseIn then
    begin
      Buttons[I].MouseIn := False;
      RePaintControl;
    end;
end;

procedure TscExPanel.MouseDown;
begin
  if FCaptionHeight > 0 then
  begin
    TestActive(X, Y);
    if ActiveButton <> -1 then
    begin
      CaptureButton := ActiveButton;
      ButtonDown(ActiveButton, X, Y);
    end
    else
    if (ssDouble in Shift) and FCaptionRect.Contains(Point(X, Y)) and
       Assigned(FOnCaptionDblClick)
    then
      FOnCaptionDblClick(Self);
  end;
  inherited;
end;

procedure TscExPanel.WMCAPTURECHANGED;
var
  P: TPoint;
begin
  if Moveable then
    if FCaptionCaptured then
    begin
      FCaptionCaptured := False;
      if Assigned(FOnCaptionClick) then
         FOnCaptionClick(Self);
    end
    else
    begin
      GetCursorPos(P);
      P := ScreenToClient(P);
      if FCaptionRect.Contains(Point(P.X, P.Y)) then
         FCaptionCaptured := True;
    end;
  inherited;
end;

procedure TscExPanel.MouseUp;
begin
  inherited;
  if FCaptionHeight = 0 then Exit;
  if CaptureButton <> -1 then
  begin
    ButtonUp(CaptureButton, X, Y);
    CaptureButton := -1;
  end
  else
  if FCaptionRect.Contains(Point(X, Y)) and (Button = mbLeft) then
  begin
    if Assigned(FOnCaptionClick) then
      FOnCaptionClick(Self);
    if FChangeRollStateWithCaptionClick then
      RollUpState := not RollUpState;
  end;
end;

procedure TscExPanel.MouseMove;
begin
  inherited;
  if FCaptionHeight > 0 then
    TestActive(X, Y);
end;

procedure TscExPanel.TestActive(X, Y: Integer);
var
  I, J: Integer;
  i1, i2: Integer;
begin
  if FShowCloseButton then i1 := 0 else i1 := 1;
  if FShowRollButton then i2 := 1 else i2 := 0;

  if i1 > i2 then Exit;

  J := -1;
  OldActiveButton := ActiveButton;

  for I := i1 to i2 do
  begin
    if PtInRect(Buttons[I].Rect, Point(X, Y)) then
    begin
      J := I;
      Break;
    end;
  end;

  ActiveButton := J;

  if (CaptureButton <> -1) and
     (ActiveButton <> CaptureButton) and (ActiveButton <> -1)
  then
    ActiveButton := -1;

  if (OldActiveButton <> ActiveButton) then
  begin
    if OldActiveButton <> - 1 then
      ButtonLeave(OldActiveButton);
    if ActiveButton <> -1 then
      ButtonEnter(ActiveButton);
  end;
end;

procedure TscExPanel.ButtonDown(I: Integer; X, Y: Integer);
begin
  Buttons[I].MouseIn := True;
  Buttons[I].Down := True;
  RePaintControl;
end;

procedure TscExPanel.ButtonUp(I: Integer; X, Y: Integer);
begin
  Buttons[I].Down := False;
  if ActiveButton <> I then
  begin
    Buttons[I].MouseIn := False;
    RePaintControl;
  end;
  if Buttons[I].MouseIn then
  begin
    case I of
     0:
       begin
         Close;
       end;
     1:
       begin
         RollUpState := not RollUpState;
         if not Assigned(FOnChangingRollUpState) then
         begin
           ActiveButton := -1;
           TestActive(X, Y);
         end;
       end;
    end;
  end;
end;

procedure TscExPanel.ButtonEnter(I: Integer);
begin
  Buttons[I].MouseIn := True;
  RePaintControl;
end;

procedure TscExPanel.ButtonLeave(I: Integer);
begin
  Buttons[I].MouseIn := False;
  RePaintControl;
end;

procedure TscExPanel.DrawButton(ACanvas: TCanvas; I: Integer);
var
  GlyphColor: TColor;
begin
  if FHeaderStyle = scexphsHeader then
  begin
    if Enabled then
      GlyphColor := GetHeaderTextColor(scsNormal)
    else
      GlyphColor := GetCheckBoxTextColor(scsDisabled);
    if Buttons[I].Down and Buttons[I].MouseIn then
    begin
      GlyphColor := GetToolButtonTextColor(scsPressed);
      DrawToolButton(ACanvas, Buttons[I].Rect, scsPressed);
    end
    else
    if Buttons[I].MouseIn then
    begin
      GlyphColor := GetToolButtonTextColor(scsHot);
      DrawToolButton(ACanvas, Buttons[I].Rect, scsHot);
    end;
  end
  else
  if FHeaderStyle = scexphsCategoryHeader then
  begin
    if Enabled then
      GlyphColor := GetCategoryHeaderTextColor
    else
      GlyphColor := GetCheckBoxTextColor(scsDisabled);
    if Buttons[I].Down and Buttons[I].MouseIn then
    begin
      GlyphColor := GetButtonTextColor(scsPressed);
      scDrawUtils.DrawButton(ACanvas, Buttons[I].Rect, scsPressed, False);
    end
    else
    if Buttons[I].MouseIn then
    begin
      GlyphColor := GetButtonTextColor(scsHot);
      scDrawUtils.DrawButton(ACanvas, Buttons[I].Rect, scsHot, False);
    end;
  end
  else
  begin
    GlyphColor := ColorToRGB(GetStyleColor(FButtonGlyphColor));
    ACanvas.Brush.Color := GlyphColor;
    if Buttons[I].Down and Buttons[I].MouseIn then
      FillRectWithAlpha(ACanvas, Buttons[I].Rect, 80)
    else
    if Buttons[I].MouseIn then
      FillRectWithAlpha(ACanvas, Buttons[I].Rect, 60);
  end;

  case I of
    0:
    begin
      DrawTabCloseImage(ACanvas, Buttons[I].Rect, GlyphColor, FScaleFactor);
    end;
    1:
    begin
      if FRollKind = scrkRollVertical then
      begin
        if Align <> alBottom then
        begin
          if FRollUpState
          then
            DrawArrowImage(ACanvas, Buttons[I].Rect, GlyphColor, 4, FScaleFactor)
          else
            DrawArrowImage(ACanvas,Buttons[I].Rect, GlyphColor, 3, FScaleFactor);
        end
        else
        begin
          if FRollUpState
          then
            DrawArrowImage(ACanvas, Buttons[I].Rect, GlyphColor, 3, FScaleFactor)
          else
            DrawArrowImage(ACanvas,Buttons[I].Rect, GlyphColor, 4, FScaleFactor);
        end;
      end
      else
      begin
        if Align <> alRight then
        begin
          if FRollUpState
         then
            DrawArrowImage(ACanvas, Buttons[I].Rect, GlyphColor, 2, FScaleFactor)
          else
            DrawArrowImage(ACanvas, Buttons[I].Rect, GlyphColor, 1, FScaleFactor);
        end
        else
        begin
          if FRollUpState
          then
            DrawArrowImage(ACanvas, Buttons[I].Rect, GlyphColor, 1, FScaleFactor)
          else
            DrawArrowImage(ACanvas, Buttons[I].Rect, GlyphColor, 2, FScaleFactor);
        end;
      end;
    end;
  end;
end;


constructor TscImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanEmpty := False;
  ControlStyle := ControlStyle + [csReplicatable, csPannable];
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := Progress;
  Height := 105;
  Width := 105;
end;

destructor TscImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TscImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;

function TscImage.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditLinkID then
    Result := True;
end;

procedure TscImage.CustomUpdateControl;
begin
  if Picture.Graphic <> nil then
  begin
    if FStretch or FProportional or FCenter then
      UpdateControls;
  end;
end;

procedure TscImage.DrawAdditionalContent(ACanvas: TCanvas; ARect: TRect; ACtrlState: TscsCtrlState; var ADrawCaption: Boolean);
var
  Save: Boolean;
begin
  Save := FDrawing;
  FDrawing := True;
  try
    ACanvas.StretchDraw(DestRect(ARect), Picture.Graphic);
  finally
    FDrawing := Save;
  end;
end;

function TscImage.DestRect(ARect: TRect): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := Picture.Width;
  h := Picture.Height;
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
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
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

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);

  OffsetRect(Result, ARect.Left, ARect.Top);
end;

function TscImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := Picture.Graphic;
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

procedure TscImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
    if DoPaletteChange then Update
    else Paint;
  end;
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TscImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    PictureChanged(Self);
  end;
end;

procedure TscImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TscImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    PictureChanged(Self);
  end;
end;

procedure TscImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    PictureChanged(Self);
  end;
end;

procedure TscImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    PictureChanged(Self);
  end;
end;

procedure TscImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkEdit(Observers) then
      TLinkObservers.EditLinkModified(Observers);

  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
	SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := FTransparent;
    if DoPaletteChange and FDrawing then Update;
  end;

  if not FDrawing then
  begin
    RePaintControl;
    UpdateControls;
  end;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkIsEditing(Observers) then
      TLinkObservers.EditLinkUpdate(Observers);
end;

function TscImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;

procedure TscImage.CMStyleChanged(var Message: TMessage);
var
  G: TGraphic;
begin
  inherited;
  if Transparent then
  begin
    G := Picture.Graphic;
    if (G <> nil) and not ((G is TMetaFile) or (G is TIcon)) and G.Transparent then
    begin
      G.Transparent := False;
      G.Transparent := True;
    end;
  end;
end;

constructor TscListGroupPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csDoubleClicks, csReplicatable, csPannable, csGestures];
  FStorePaintBuffer := True;
  FHeaderFont := TFont.Create;
  FAutoSize := False;
  FRowHeight := 0;
  FGroupColorAlpha := 255;
  FHeaderFont.Assign(Font);
  FHeaderFont.Size := 11;
  FHeaderFont.Color := clBtnText;
  FHeaderFont.OnChange := OnControlChange;
  FHeaderHeight := 30;
  FHeaderAutoColor := True;
  FTransparentBackground := False;
  Color := clBtnFace;
  FGroupColor := clWindow;
  FRowLineMargin := 20;
  FRowCount := 1;
  FHeaderMargin := 10;
  Width := 200;
  Height := 230;
end;

destructor TscListGroupPanel.Destroy;
begin
  FHeaderFont.Free;
  inherited;
end;

procedure TscListGroupPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FAutoSize and (FRowHeight > 0) and (FRowCount > 0) and
    ((Align = alNone) or (Align = alTop) or (Align = alBottom)) then
  begin
    AHeight := FHeaderHeight + FRowHeight * FRowCount;
  end;
  inherited;
end;

procedure TscListGroupPanel.CMTextChanged;
begin
  inherited;
  RePaintControl;
end;

procedure TscListGroupPanel.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TscListGroupPanel.OnControlChange(Sender: TObject);
begin
  RePaintControl;
end;

procedure TscListGroupPanel.SetRowHeight(Value: Integer);
begin
  if FRowHeight <> Value then
  begin
    FRowHeight := Value;
    if FAutoSize then
      SetBounds(Left, Top, Width, Height)
    else
      RePaintControl;
  end;
end;

procedure TscListGroupPanel.SetPanelAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then
      SetBounds(Left, Top, Width, Height)
  end;
end;

procedure TscListGroupPanel.SetRowCount;
begin
  if (FRowCount <> Value) and (Value >= 1) then
  begin
    FRowCount := Value;
    if FAutoSize then
      SetBounds(Left, Top, Width, Height)
    else
      RePaintControl;
  end;
end;

procedure TscListGroupPanel.SetHeaderHeight(Value: Integer);
begin
  if (FHeaderHeight <> Value) and (Value >= 10) then
  begin
    FHeaderHeight := Value;
    if FAutoSize then
      SetBounds(Left, Top, Width, Height)
    else
      RePaintControl;
  end;
end;

procedure TscListGroupPanel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FHeaderFont.Height := MulDiv(FHeaderFont.Height, M, D);
  FRowHeight := MulDiv(FRowHeight, M, D);
  FHeaderHeight := MulDiv(FHeaderHeight, M, D);
  FHeaderMargin := MulDiv(FHeaderMargin, M, D);
  FRowLineMargin := MulDiv(FRowLineMargin, M, D);
end;

procedure TscListGroupPanel.SetHeaderAutoColor(Value: Boolean);
begin
  if (FHeaderAutoColor <> Value) then
  begin
    FHeaderAutoColor := Value;
    RePaintControl;
  end;
end;

procedure TscListGroupPanel.SetHeaderMargin(Value: Integer);
begin
  if (FHeaderMargin <> Value) and (Value >= 0) then
  begin
    FHeaderMargin := Value;
    RePaintControl;
  end;
end;

procedure TscListGroupPanel.SetRowLineMargin(Value: Integer);
begin
  if (FRowLineMargin <> Value) and (Value >= 0) then
  begin
    FRowLineMargin := Value;
    RePaintControl;
  end;
end;

procedure TscListGroupPanel.SetGroupColor(Value: TColor);
begin
  if FGroupColor <> Value then
  begin
    FGroupColor := Value;
    RePaintControl;
    UpdateControls;
  end;
end;

procedure TscListGroupPanel.SetGroupColorAlpha(Value: Byte);
begin
  if FGroupColorAlpha <> Value then
  begin
    FGroupColorAlpha := Value;
    RePaintControl;
    UpdateControls;
  end;
end;


procedure TscListGroupPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not
      (CS_HREDRAW or CS_VREDRAW);
end;

procedure TscListGroupPanel.DrawBackground(ACanvas: TCanvas);
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

procedure TscListGroupPanel.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
  C, C1, C2: TColor;
  I, IH, Y: Integer;
begin
  ACanvas.Font := FHeaderFont;
  if FHeaderAutoColor then
  begin
    C1 := GetStyleColor(clBtnFace);
    C2 := GetStyleColor(clBtnText);
    C := MiddleColor(C1, C2);
    if not IsCustomStyle then
      C := DarkerColor(C, 20)
    else
     if not IsDarkStyle then
        C := DarkerColor(C, 10);
  end
  else
    C := GetStyleColor(FHeaderFont.Color);
  ACanvas.Font.Color := C;
  R := Rect(FHeaderMargin, 0, Width - FHeaderMargin, FHeaderHeight);
  R.Top := Max(R.Bottom - ACanvas.TextHeight('Wq') - Round(5 * FScaleFactor), R.Top);
  ACanvas.Brush.Style := bsClear;
  DrawText(ACanvas.Handle,
    PChar(Caption), Length(Caption), R,
    scDrawTextBidiModeFlags(DT_LEFT or DT_VCENTER or DT_SINGLELINE, BidiMode = bdRightToLeft));
  R := Rect(0, FHeaderHeight, Width, Height);
  ACanvas.Brush.Color := ColorToRGB(GetStyleColor(FGroupColor));
  if FGroupColorAlpha = 255 then
    ACanvas.FillRect(R)
  else
  if FGroupColorAlpha > 0 then
    FillRectWithAlpha(ACanvas, R, FGroupColorAlpha);

  C1 := GetStyleColor(clBtnFace);
  C2 := GetStyleColor(clBtnText);
  C := MiddleColor(C1, C2);
  C2 := C;
  C := MiddleColor(C1, C2);
  ACanvas.Pen.Color := C;
  ACanvas.MoveTo(0, FHeaderHeight);
  ACanvas.LineTo(Width, FHeaderHeight);
  ACanvas.MoveTo(0, Height - 1);
  ACanvas.LineTo(Width, Height - 1);
  if FRowCount < 2 then Exit;
  if FRowHeight = 0 then
    IH := R.Height div FRowCount
  else
    IH := FRowHeight;
  Y := R.Top;
  C := MiddleColor(C, GetStyleColor(FGroupColor));
  ACanvas.Pen.Color := C;
  for I := 1 to FRowCount - 1 do
  begin
    Y := Y + IH;
    ACanvas.MoveTo(FRowLineMargin, Y);
    ACanvas.LineTo(Width, Y);
  end;
end;

procedure TscListGroupPanel.WMSIZE(var Msg: TMessage);
begin
  inherited;
  if not FStorePaintBuffer then
    RePaint;
end;

constructor TscControlBar.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundStyle := scexbgsPanel;
  FWallpapers := nil;
  FWallpaperIndex := -1;
  FScaleFactor := 1;
  FScalePercent := 100;
end;

destructor TscControlBar.Destroy;
begin
  inherited;
end;

procedure TscControlBar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
end;

procedure TscControlBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
end;

procedure TscControlBar.RePaintControl;
begin
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or
      RDW_ALLCHILDREN or RDW_UPDATENOW);
end;

procedure TscControlBar.SetBackgroundStyle(Value: TscExPanelBackgroundStyle);
begin
  if FBackgroundStyle <> Value then
  begin
    FBackgroundStyle := Value;
    RePaintControl;
  end;
end;

procedure TscControlBar.WMCHECKPARENTBG(var Msg: TWMEraseBkgnd);
begin
  if FBackgroundStyle = scexbgsTransparent then
    RePaintControl;
end;

procedure TscControlBar.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
  end;
end;

procedure TscControlBar.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
  end;
end;

procedure TscControlBar.WMSIZE(var Msg: TMessage);
begin
  inherited;
  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) and
    FWallpapers.NeedFullUpdate(FWallpaperIndex) then
    RePaintControl;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TscControlBar.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  Buffer: TBitmap;
  R: TRect;
  C: TCanvas;
begin
  R := ClientRect;
  if R.Width * R.Height = 0 then Exit;

  C := TCanvas.Create;
  C.Handle := Message.DC;

  Buffer := TBitmap.Create;
  Buffer.Width := R.Width;
  Buffer.Height := R.Height;

  try
    R := Rect(0, 0, Buffer.Width, Buffer.Height);

    case FBackgroundStyle of
      scexbgsPanel:
        begin
          Buffer.Canvas.Brush.Color := GetStyleColor(Color);
          Buffer.Canvas.FillRect(R);
        end;
      scexbgsFormBackground:
        begin
          scDrawUtils.DrawFormBackground(Buffer.Canvas, R);
        end;
      scexbgsTransparent:
        begin
          scDrawUtils.DrawParentBackground(Self, Buffer.Canvas);
        end;
    end;

    if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex) then
      FWallpapers.Draw(Buffer.Canvas, R, FWallpaperIndex, FScaleFactor);

    C.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
    C.Handle := 0;
    C.Free;
  end;

  Message.Result := 1;
end;

initialization

  TCustomStyleEngine.RegisterStyleHook(TscFontComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscFontSizeComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscFontListBox, TscListBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscScrollBox, TscScrollBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscFrameBar, TscScrollBoxStyleHook);

finalization

  {$IFNDEF VER230}
  TCustomStyleEngine.UnRegisterStyleHook(TscFontComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscFontSizeComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscFontListBox, TscListBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscScrollBox, TscScrollBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscFrameBar, TscScrollBoxStyleHook);
  {$ENDIF}

end.

