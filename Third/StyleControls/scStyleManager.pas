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

unit scStyleManager;

{$I scdefine.inc}
{$R-}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
    Vcl.Controls, Vcl.Graphics, Vcl.Themes, Vcl.ImgList, Vcl.Menus, Vcl.Forms,
    scDrawUtils, scImageCollection{$IFDEF VER270_UP},Vcl.SysStyles{$ENDIF}, Generics.Collections;

{$IFNDEF VER270_UP}
const
  MN_SELECTITEM = $01E5;
  MN_SETHMENU = $01E0;
  MN_GETHMENU = $01E1;
  MN_BUTTONDOWN = $01ED;
  MN_MOUSEMOVE = $01EE;
{$ENDIF}

type
  {$IFNDEF VER270_UP}
  TscSysBidiModeDirection = (scsbmLeftToRight, scsbmRightToLeft);
  TscSysControl = class
  private
    FFont: TFont;
    FParent: TscSysControl;
    FHandle: THandle;
    function GetParent: TscSysControl;
    function GetParentHandle: THandle;
    function GetText: String;
    function GetStyle: NativeInt;
    function GetExStyle: NativeInt;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetBorder: Boolean;
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    function GetClientRect: TRect;
    function GetWinRect: TRect;
    function GetClientEdge: Boolean;
    function GetControlClassName: String;
    function GetWndProc: NativeInt;
    procedure SetWndProc(Value: NativeInt);
    function GetBidiMode: TscSysBidiModeDirection;
    procedure SetExStyle(const Value: NativeInt);
    procedure SetStyle(const Value: NativeInt);
    function GetBoundsRect: TRect;
    function GetFont: TFont;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
  public
    constructor Create(AHandle: THandle); virtual;
    destructor Destroy; override;
    property Font: TFont read GetFont;
    property Parent: TscSysControl read GetParent;
    property ParentHandle: THandle read GetParentHandle;
    property Handle: THandle read FHandle write FHandle;
    property Text: String read GetText;
    property Style: NativeInt read GetStyle write SetStyle;
    property ExStyle: NativeInt read GetExStyle write SetExStyle;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft;
    property Top: Integer read GetTop;
    property HasBorder: Boolean read GetBorder;
    property Enabled: Boolean read GetEnabled;
    property Visible: Boolean read GetVisible;
    property ClientRect: TRect read GetClientRect;
    property WindowRect: TRect read GetWinRect;
    property HasClientEdge: Boolean read GetClientEdge;
    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property ControlClassName: string read GetControlClassName;
    property WndProc: NativeInt read GetWndProc write SetWndProc;
    property BidiMode: TscSysBidiModeDirection read GetBidiMode;
    property BoundsRect: TRect read GetBoundsRect;
    function DrawTextBiDiModeFlags(Flags: Longint): Longint;
    function UseRightToLeftAlignment: Boolean; dynamic;
    function DrawTextBiDiModeFlagsReadingOnly: Longint;
    function UseRightToLeftReading: Boolean;
    function Focused: Boolean; dynamic;
  end;

  TscSysStyleHook = class
  private
    FHandle: THandle;
    FProcInstance: Pointer;
    FSysControl: TscSysControl;
    FOrgWndProc: NativeInt;
    FOverrideEraseBkgnd: Boolean;
    FOverridePaint: Boolean;
    FOverridePaintNC: Boolean;
    FOverrideFont: Boolean;
    FDoubleBuffered: Boolean;
    FPaintOnEraseBkgnd: Boolean;
    FFontColor: TColor;
    FBrush: TBrush;
    FHandled: Boolean;
    FParentColor: Boolean;
    FStyleElements: TStyleElements;
    FColor: TColor;
    FFont: TFont;
    FText: string;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure SetStyleElements(Value: TStyleElements);
    function GetFontColor: TColor;
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetOverridePaint(Value: Boolean);
    function GetFocused: Boolean;
    function GetParentHandle: HWND;
    procedure SetFont(const Value: TFont);
    function UseLeftScrollBar: Boolean;
    function GetText: string;
  protected
    function GetBorderSize: TRect; virtual;
    function CheckIfParentBkGndPainted: Boolean; virtual;
    function CheckIfParentHooked: Boolean;
    procedure Paint(Canvas: TCanvas); virtual;
    procedure DrawParentBackground(DC: HDC); overload;
    procedure DrawParentBackground(DC: HDC; ARect: PRect); overload;
    procedure PaintBorder(Control: TscSysControl; EraseLRCorner: Boolean);
    procedure DrawBorder(Canvas: TCanvas); virtual;
    procedure PaintBackground(Canvas: TCanvas); virtual;
    procedure PaintNC(Canvas: TCanvas); virtual;
    function CallDefaultProc(var Msg: TMessage): LRESULT;
    procedure SetRedraw(Value: Boolean); overload;
    procedure SetRedraw(AHandle: HWND; Value: Boolean); overload; virtual;
    function StyleServicesEnabled: Boolean;
    procedure WndProc(var Message: TMessage); virtual;
    function InternalPaint(DC: HDC): Boolean; virtual;
    procedure UpdateColors; virtual;
  public
    constructor Create(AHandle: THandle); virtual;
    Destructor Destroy; override;
    procedure Invalidate; virtual;
    procedure InvalidateNC; virtual;
    procedure Refresh; virtual;
    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
      const S: string; var R: TRect; Flags: Cardinal);
    function DrawTextCentered(DC: HDC; Details: TThemedElementDetails;
      const R: TRect; S: String; Const Flags: DWORD = 0): Integer;
    function DrawText(DC: HDC; Details: TThemedElementDetails; S: String;
      var R: TRect; Const Flags: TTextFormat = []): Integer;
    property Handle: THandle read FHandle;
    property ParentHandle: HWND read GetParentHandle;
    property Handled: Boolean read FHandled write FHandled;
    property SysControl: TscSysControl read FSysControl write FSysControl;
    property StyleElements: TStyleElements read FStyleElements
      write SetStyleElements;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property OverridePaint: Boolean read FOverridePaint write SetOverridePaint;
    property OverridePaintNC: Boolean read FOverridePaintNC
      write FOverridePaintNC;
    property OverrideFont: Boolean read FOverrideFont write FOverrideFont;
    property OverrideEraseBkgnd: Boolean read FOverrideEraseBkgnd
      write FOverrideEraseBkgnd;
    property FontColor: TColor read GetFontColor write FFontColor;
    property Color: TColor read GetColor write SetColor;
    property Brush: TBrush read FBrush;
    property Font: TFont read FFont write SetFont;
    property Focused: Boolean read GetFocused;
    property ParentBkGndPainted: Boolean read CheckIfParentBkGndPainted;
    property ParentColor: Boolean read FParentColor write FParentColor;
    property Text: string read GetText;
  end;

  TscSysStyleHookClass = class of TscSysStyleHook;

  PscChildControlInfo = ^TscChildControlInfo;
  TscChildControlInfo = record
    Parent: HWND;
    ParentStyle: NativeInt;
    StyleHookClass: TscSysStyleHookClass;
  end;

  TscSysStyleManager = class(TComponent)
  strict protected type
    TscRegSysStylesList = TObjectDictionary<String, TscSysStyleHookClass>;
    TscSysStyleHookList = TObjectDictionary<HWND, TscSysStyleHook>;
    TscChildRegSysStylesList = TObjectDictionary<HWND, TscChildControlInfo>;
  private
  class var
    FEnabled: Boolean;
    FHook: HHook;
    FRegSysStylesList: TscRegSysStylesList;
    FSysStyleHookList: TscSysStyleHookList;
    FChildRegSysStylesList: TscChildRegSysStylesList;
    FMenuHookEnabled: Boolean;
  protected
    class function CheckSysClassName(const AClassName: String): Boolean;
    class procedure InstallHook;
    class procedure RemoveHook;
    class function HookCBProc(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT; stdcall; static;
  public
    class procedure RegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TscSysStyleHookClass);
    class procedure UnRegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TscSysStyleHookClass);
    class constructor Create;
    class destructor Destroy;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class property Enabled: Boolean read FEnabled write FEnabled;
    class property SysStyleHookList: TscSysStyleHookList read FSysStyleHookList;
    class property ChildRegSysStylesList: TscChildRegSysStylesList read FChildRegSysStylesList;
    class property MenuHookEnabled: Boolean read FMenuHookEnabled write FMenuHookEnabled;
  end;
  {$ENDIF}

  TscSysPopupItemState = set of (scisHot, scisDisabled, scisChecked, scisDefault);
  TscSysPopupItemStyle = (scisNormal, scisSep, scisDropDown);

  {$IFDEF VER270_UP}
  TscSysPopupStyleHook = class(TSysStyleHook)
  {$ELSE}
  TscSysPopupStyleHook = class(TscSysStyleHook)
  {$ENDIF}
  private type
    TscSysPopupItem = class
    private
      FIndex: Integer;
      FVCLMenuItem: TMenuItem;
      FMenu: HMENU;
      FHandle: HWND;
      {$IFDEF VER270_UP}
      FSysParent: TSysControl;
      {$ELSE}
      FSysParent: TscSysControl;
      {$ENDIF}
      function GetItemRect: TRect;
      function IsItemDisabled: Boolean;
      function IsItemContainsSubMenu: Boolean;
      function IsItemSeparator: Boolean;
      function IsItemChecked: Boolean;
      function IsItemSelected: Boolean;
      function IsItemDefault: Boolean;
      function GetItemText: String;
      function GetVCLParentMenuItem: TMenuItem;
      function GetItemBitmap: HBITMAP;
      function IsItemRadioCheck: Boolean;
    public
      constructor Create(SysParent: {$IFDEF VER270_UP}TSysControl{$ELSE}TscSysControl{$ENDIF};
        Index: Integer;
        Menu: HMENU); virtual;
      Destructor Destroy; override;
      property ItemRect: TRect read GetItemRect;
      property Disabled: Boolean read IsItemDisabled;
      property Separator: Boolean read IsItemSeparator;
      property HasSubMenu: Boolean read IsItemContainsSubMenu;
      property Checked: Boolean read IsItemChecked;
      property RadioCheck: Boolean read IsItemRadioCheck;
      property DefaultItem: Boolean read IsItemDefault;
      property Text: String read GetItemText;
      property VCLMenuItem: TMenuItem read FVCLMenuItem;
      property Bitmap: HBITMAP read GetItemBitmap;
    end;
  var
    FItemsPainted: Boolean;
    FLBDown, FMouseMove: Boolean;
    FParentSubItemPainted: Boolean;
    FPreviousHotItemIndex: Integer;
    FActiveItemIndex: Integer;
    FFromKeyboard: Boolean;
    FKeyIndex: Integer;
    FSysPopupItem: TscSysPopupItem;
    FCount: Integer;
    FMenu: HMENU;
    FAlphaBlend: Boolean;
    FCanSetRedraw: Boolean;
    function GetMenuFromHandle(AHandle: HWND): HMENU;
    function GetItemsCount: Integer;
    procedure MNSELECTITEM(var Message: TMessage); message MN_SELECTITEM;
    procedure WMPRINT(var Message: TMessage); message WM_PRINT;
    function GeTscSysPopupItem(Index: Integer): TscSysPopupItem;
    function GetRightToLeft: Boolean;
  protected
    {$IFDEF VER330_UP}
    function GetDevicePPI: Integer;
    {$ENDIF}
    procedure DoDrawItem(Canvas: TCanvas; Index: Integer);
    procedure DrawMenuHeader(ACanvas: TCanvas; ARect: TRect; ACaption: String);
    procedure DrawPopupWindow(Canvas: TCanvas);
    procedure DrawItem(Canvas: TCanvas; Index: Integer; ItemRect: TRect;
      ItemText: String; State: TscSysPopupItemState; Style: TscSysPopupItemStyle); Virtual;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
    property Menu: HMENU read FMenu;
    property Items[Index: Integer]: TscSysPopupItem read GeTscSysPopupItem;
    property Count: Integer read FCount;
    property RightToLeft: Boolean read GetRightToLeft;
  end;

  {$IFDEF VER300_UP}
  TscSysDialogStyleHook = class(TSysScrollingStyleHook)
  private
    FFrameActive: Boolean;
    FPressedButton: Integer;
    FHotButton: Integer;
    FIcon: TIcon;
    FIconHandle: HICON;
    FCaptionRect: TRect;
    FSysMenuButtonRect: TRect;
    FRegion: HRGN;
    FWidth, FHeight: Integer;
    FSysCloseButtonDisabled: Boolean;
    FScaleFactor: Double;
    FStyledCaptionHeight: Integer;
    FMinButtonRect,
    FMaxButtonRect,
    FHelpButtonRect,
    FCloseButtonRect: TRect;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCMouseMove(var Message: TWMNCHitMessage); message WM_NCMOUSEMOVE;
    procedure WMNCACTIVATE(var Message: TWMNCActivate); message WM_NCACTIVATE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMSIZE(var Message: TWMSize); message WM_SIZE;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
    function GetCaptionRect: TRect;
    function GetBorderStyle: TFormBorderStyle;
    function GetBorderIcons: TBorderIcons;
    function GetCloseButtonRect: TRect;
    function GetMaxButtonRect: TRect;
    function GetMinButtonRect: TRect;
    function GetHelpButtonRect: TRect;
    function GetSysMenuButtonRect: TRect;
    function GetWindowState: TWindowState;
    function UseSmallBorder: Boolean;
    function GetRegion: HRGN;
    function GetIcon: TIcon;
    function GetIconFast: TIcon;
    function NormalizePoint(const P: TPoint): TPoint;
    function GetHitTest(const P: TPoint): Integer;
    function IsSysCloseButtonDisabled: Boolean;
    function GetSysMenu: HMENU;
    function GetUpdateRegion: Boolean;
  protected
    procedure DrawBorder(Canvas: TCanvas); override;
    function GetBorderSize: TRect; override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure MouseLeave; override;
    procedure Close; virtual;
    procedure Help; virtual;
    procedure Maximize; virtual;
    procedure Minimize; virtual;
    procedure Restore; virtual;
    property  PressedButton: Integer read FPressedButton write FPressedButton;
    property  HotButton: Integer read FHotButton write FHotButton;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property CaptionRect: TRect read GetCaptionRect;
    property UpdateRegion: Boolean read GetUpdateRegion;
    property BorderStyle: TFormBorderStyle read GetBorderStyle;
    property BorderSize: TRect read GetBorderSize;
    property BorderIcons: TBorderIcons read GetBorderIcons;
    Property WindowState: TWindowState read GetWindowState;
    Property CloseButtonRect: TRect read GetCloseButtonRect;
    Property MaxButtonRect: TRect read GetMaxButtonRect;
    Property MinButtonRect: TRect read GetMinButtonRect;
    Property HelpButtonRect: TRect read GetHelpButtonRect;
    property SysMenuButtonRect: TRect read GetSysMenuButtonRect;
    property Icon: TIcon read GetIconFast;
    property SysMenu: HMENU read GetSysMenu;
    property SysCloseButtonDisabled: Boolean read FSysCloseButtonDisabled;
  end;

  TscSysTooltipsStyleHook = class(TSysStyleHook)
  private
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
  protected
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintHint(Canvas: TCanvas; TextRect: TRect);
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TscSysButtonStyleHook = class(TMouseTrackSysControlStyleHook)
  private
    FScaleFactor: Double;
    function GetCaptionRect(Canvas: TCanvas): TRect;
    function GetBoxRect: TRect;
    function IsCheckBox: Boolean;
    function IsRadioButton: Boolean;
    function IsGroupBox: Boolean;
    function IsPushButton: Boolean;
    function IsSplitButton: Boolean;
    function IsCommandButton: Boolean;
    function GetShowText: Boolean;
    function GetCheckBoxState: TSysCheckBoxState;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    function IsOwnerDraw: Boolean;
  protected
    procedure DrawCheckBoxText(DC: HDC; Text: String;
      LDetails: TThemedElementDetails; R: TRect); virtual;
    procedure PaintButton(Canvas: TCanvas); virtual;
    procedure PaintCheckBox(Canvas: TCanvas); virtual;
    procedure PaintRadioButton(Canvas: TCanvas); virtual;
    procedure PaintGroupBox(Canvas: TCanvas); virtual;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
  public
    constructor Create(AHandle: THandle); override;
    property CheckBox: Boolean read IsCheckBox;
    property CommandButton: Boolean read IsCommandButton;
    property RadioButton: Boolean read IsRadioButton;
    property GroupBox: Boolean read IsGroupBox;
    property PushButton: Boolean read IsPushButton;
    property SplitButton: Boolean read IsSplitButton;
    property CheckBoxState: TSysCheckBoxState read GetCheckBoxState;
    property ShowText: Boolean read GetShowText;
    property OwnerDraw: Boolean read IsOwnerDraw;
  end;

  {$ENDIF}

  TscStyleArrowType = (scsatDefault, scsatModern);
  TscMenuSelectionStyle = (scmssStyled, scmssColor);

  TscStyleManager = class(TComponent)
  private
    FMenuAlphaBlendValue: Byte;
    FMenuWallpaperIndex: Integer;
    FMenuBackgroundIndex: Integer;
    FMenuBackgroundOverContentIndex: Integer;
    FMenuImages: TscImageCollection;
    FMenuHeadersSupport: Boolean;
    FMenuHookEnabled: Boolean;
    FMenuSelectionStyle: TscMenuSelectionStyle;
    FStyleArrowType: TscStyleArrowType;
    function GetStyleArrowType: TscStyleArrowType;
    procedure SetStyleArrowType(Value: TscStyleArrowType);
    procedure SetMenuSelectionStyle(Value: TscMenuSelectionStyle);
    procedure SetMenuHeadersSupport(Value: Boolean);
    procedure SetMenuAlphaBlendValue(Value: Byte);
    procedure SetMenuWallpaperIndex(Value: Integer);
    procedure SetMenuBackgroundIndex(Value: Integer);
    procedure SetMenuBackgroundOverContentIndex(Value: Integer);
    procedure SetMenuImages(Value: TscImageCollection);
    procedure SetMenuHookEnabled(Value: Boolean);
    function GetScaleStyles: Boolean;
    procedure SetScaleStyles(Value: Boolean);
    function GetScaleThemes: Boolean;
    procedure SetScaleThemes(Value: Boolean);
    function GetScaleResources: Boolean;
    procedure SetScaleResources(Value: Boolean);
    function GetScaleFormBorder: Boolean;
    procedure SetScaleFormBorder(Value: Boolean);
    function GetRTLMode: Boolean;
    procedure SetRTLMode(Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ArrowsType: TscStyleArrowType
      read GetStyleArrowType write SetStyleArrowType;
    property MenuHookEnabled: Boolean read FMenuHookEnabled write SetMenuHookEnabled;
    property MenuAlphaBlendValue: Byte read FMenuAlphaBlendValue write
      SetMenuAlphaBlendValue;
    property MenuImages: TscImageCollection
      read FMenuImages write SetMenuImages;
    property MenuWallpaperIndex: Integer
      read FMenuWallpaperIndex write SetMenuWallpaperIndex;
    property MenuBackgroundIndex: Integer
      read FMenuBackgroundIndex write SetMenuBackgroundIndex;
    property MenuBackgroundOverContentIndex: Integer
      read FMenuBackgroundOverContentIndex write SetMenuBackgroundOverContentIndex;
    property MenuHeadersSupport: Boolean
      read FMenuHeadersSupport write SetMenuHeadersSupport;
    property MenuSelectionStyle: TscMenuSelectionStyle
      read FMenuSelectionStyle write SetMenuSelectionStyle;
    property ScaleStyles: Boolean
      read GetScaleStyles write SetScaleStyles;
    property ScaleThemes: Boolean
      read GetScaleThemes write SetScaleThemes;
    property ScaleResources: Boolean
      read GetScaleResources write SetScaleResources;
    property ScaleFormBorder: Boolean
      read GetScaleFormBorder write SetScaleFormBorder;
    property RTLMode: Boolean
      read GetRTLMode write SetRTLMode;
  end;

  {$IFNDEF VER270_UP}
  var
    SC_DwmIsCompositionEnabled: function(pfEnabled: PBool): HRESULT; stdcall;
  {$ENDIF}

implementation
  uses System.UITypes, System.Math, Winapi.UxTheme, Vcl.GraphUtil,
    Winapi.DwmApi, Winapi.CommCtrl;

const
  WM_ITEMSPAINTED = WM_USER + 201;
  WM_STYLEDLGDESTROY = WM_USER + 202;
  WM_NCDRAW = WM_USER + 203;
  WM_NEEDREDRAW = WM_USER + 204;


  BS_SPLITBUTTON = $0000000C;
 {$EXTERNALSYM BS_DEFSPLITBUTTON}
  BS_DEFSPLITBUTTON = $0000000D;
 {$EXTERNALSYM BS_COMMANDLINK}
  BS_COMMANDLINK = $0000000E;
 {$EXTERNALSYM BS_DEFCOMMANDLINK}
  BS_DEFCOMMANDLINK = $0000000F;

  {$IFNDEF VER270_UP}
  CM_BASE = WM_USER + $113;
  CM_CTLCOLORMSGBOX = CM_BASE + WM_CTLCOLORMSGBOX;
  CM_CTLCOLORSTATIC = CM_BASE + WM_CTLCOLORSTATIC;
  CM_PARENTHOOKED = CM_BASE + 360;
  CM_CONTROLHOOKED = CM_BASE + 361;
  CM_INITCHILDS = CM_BASE + 362;
  MARLETT_RESTORE_CHAR = Char(50);
  MARLETT_MINIMIZE_CHAR = Char(48);
  MARLETT_CLOSE_CHAR = Char(114);
  MARLETT_MAXIMIZE_CHAR = Char(49);
  {$ENDIF}

var
  FCallFromKeyboard: Boolean = False;
  // menus header
  SC_MenuHeadersSupport: Boolean = True;
  // menus wallpapers
  SC_MenuAlphaBlendValue: Byte = 255;
  SC_MenuImageCollection: TscImageCollection = nil;
  SC_MenuWallpaperIndex: Integer = -1;
  SC_MenuBackgroundIndex: Integer = -1;
  SC_MenuBackgroundOverContentIndex: Integer = -1;

{$IFNDEF VER270_UP}
function BmpToIcon(hBmp: HBITMAP): HICON;
var
  Bmp: BITMAP;
  hbmMask: HBITMAP;
  DC: HDC;
  IInfo: ICONINFO;
  Icon: HICON;
begin
  FillChar(Bmp, SizeOf(BITMAP), Char(0));
  GetObject(hBmp, SizeOf(BITMAP), @Bmp);
  DC := GetDC(0);
  hbmMask := CreateCompatibleBitmap(DC, Bmp.bmWidth, Bmp.bmHeight);
  IInfo.fIcon := True;
  IInfo.hbmColor := hBmp;
  IInfo.hbmMask := hbmMask;
  Icon := CreateIconIndirect(IInfo);
  DeleteObject(hbmMask);
  ReleaseDC(0, DC);
  Result := Icon;
end;

function GetBmpInfo(hBmp: HBITMAP): BITMAP;
begin
  ZeroMemory(@Result, SizeOf(BITMAP));
  GetObject(hBmp, SizeOf(Result), @Result);
end;

function GetBitmapHeight(hBmp: HBITMAP): Integer;
begin
  Result := GetBmpInfo(hBmp).bmHeight;
end;

function GetBitmapWidth(hBmp: HBITMAP): Integer;
begin
  Result := GetBmpInfo(hBmp).bmWidth;
end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;
{$ENDIF}

{$IFNDEF VER270_UP}
function DwmCompositionEnabled: Boolean;
var
  LEnabled: LongBool;
begin
  if (Win32MajorVersion >= 6) and (@SC_DwmIsCompositionEnabled <> nil)
  then
  begin
    LEnabled := False;
    SC_DwmIsCompositionEnabled(@LEnabled);
    Result := LEnabled;
  end
  else
    Result := False;
end;
{$ENDIF}

constructor TscSysPopupStyleHook.Create(AHandle: THandle);
begin
  inherited;
  StyleElements := [seFont, seClient, seBorder];
  FPreviousHotItemIndex := -1;
  FActiveItemIndex := -1;
  FKeyIndex := -1;
  FItemsPainted := False;
  FSysPopupItem := nil;
  if not SC_StopSetMenuRedraw then
  begin
    FCanSetRedraw := DwmCompositionEnabled or
      scDrawUtils.IsWindows8 or scDrawUtils.IsWindows10;
    if FCanSetRedraw and (Screen.PixelsPerInch > 120) then
      FCanSetRedraw := False;
  end
  else
  begin
    FCanSetRedraw := True;
    if (Screen.PixelsPerInch > 120) and not
       (IsWindows7 and not DwmCompositionEnabled)
    then
      FCanSetRedraw := False;
  end;
end;

procedure TscSysPopupStyleHook.DoDrawItem(Canvas: TCanvas; Index: Integer);
var
  LItemRect, R: TRect;
  State: TscSysPopupItemState;
  Style: TscSysPopupItemStyle;
  LText: String;
  SaveIndex: Integer;
  Item: TscSysPopupItem;
begin
  if (Index < 0) or (Index > Count - 1) then Exit;
  Item := Items[Index];
  LItemRect := Item.ItemRect;
  GetWindowRect(Handle, R);
  OffsetRect(LItemRect, -R.Left - 3, - R.Top - 3);
  { Item State }
  State := [];
  if Item.IsItemSelected then
  begin
    Include(State, scisHot);
    FActiveItemIndex := Index;
  end;
  if Item.Disabled then
    Include(State, scisDisabled);
  if Item.Checked then
    Include(State, scisChecked);
  if Item.DefaultItem then
    Include(State, scisDefault);
  { Item Style }
  Style := scisNormal;
  if Item.Separator then
    Style := scisSep;
  if Item.HasSubMenu then
    Style := scisDropDown;

  LText := '';
  if Style <> scisSep then
    LText := Item.Text;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    Self.DrawItem(Canvas, Index, LItemRect, LText, State, Style);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

procedure TscSysPopupStyleHook.DrawItem(Canvas: TCanvas; Index: Integer;
  ItemRect: TRect; ItemText: String; State: TscSysPopupItemState;
  Style: TscSysPopupItemStyle);
var
  Detail: TThemedMenu;
  LDetails: TThemedElementDetails;
  LTextFormat: TTextFormat;
  DC: HDC;
  LSize: TSize;
  MI: TMenuItem;
  MIImages: TCustomImageList;
  ImageIndex: Integer;
  LImageRect, R: TRect;
  LTextRect: TRect;
  hBmp: HBITMAP;
  BmpHeight, BmpWidth: Integer;
  Icon: HICON;
  DisplayCheckedGlyph: Boolean;
  Sign: Char;
  SysItem: TscSysPopupItem;
  sShortCut: String;
  Bmp: TBitmap;
  VCLItemState: TOwnerDrawState;
  C: TColor;
  FState: TscsCtrlState;
  FDrawSelColorText: Boolean;

procedure DrawSelectionText(S: String; R: TRect; Flags: TTextFormat);
var
  DrawFlags: Cardinal;
  SaveIndex: Integer;
  sColor: TColor;
begin
  SaveIndex := SaveDC(Canvas.Handle);
  try
    SetBkMode(Canvas.Handle, TRANSPARENT);
    sColor := GetStyleColor(clHighLightText);
    SetTextColor(Canvas.Handle, sColor);
    DrawFlags := TTextFormatFlags(Flags);
    Winapi.Windows.DrawText(Canvas.Handle, S, -1, R, DrawFlags);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

procedure DrawSubMenu(const ItemRect: TRect);
  var
    LSubMenuDetails: TThemedElementDetails;
    SubMenuSize: TSize;
    LSubMenuRect: TRect;
    FState: TscsCtrlState;
    C: TColor;
    Y: Integer;
    FScaleFactor: Double;
  begin
    LSubMenuRect := Rect(0, 0, 0, 0);
    FState := scsNormal;
    if scisDisabled in State then
      FState := scsDisabled
    else
    if scisHot in State then
      FState := scsHot;

    LSubMenuDetails := StyleServices.GetElementDetails(tmPopupSubMenuNormal);
    StyleServices.GetElementSize(DC, LSubMenuDetails, esActual, SubMenuSize);

    Y := ItemRect.Top + ItemRect.Height div 2 - SubMenuSize.cy div 2;

    if not RightToLeft then
      LSubMenuRect := Rect(ItemRect.Right - SubMenuSize.cx, Y - 1,
        ItemRect.Right, Y + SubMenuSize.cy - 1)
    else
      LSubMenuRect := Rect(ItemRect.Left + 4, Y - 1,
        ItemRect.Left + 4 + SubMenuSize.Width, Y + SubMenuSize.cy - 1);

    C := scDrawUtils.GetPopupItemTextColor(FState);

    {$IFNDEF VER330_UP}
      FScaleFactor := Screen.PixelsPerInch / 96;
    {$ELSE}
      FScaleFactor := GetDevicePPI / 96;
    {$ENDIF}

     if SC_MODERNARROWS or IsWindowsModernStyle then
     begin
       if FScaleFactor = 1.25 then
         FScaleFactor := 1.1
       else
       if FScaleFactor >= 1.5 then
         FScaleFactor := FScaleFactor - 0.3;
     end;

     if FScaleFactor < 1 then
        FScaleFactor := 1;

    if RightToLeft or (SysControl.BidiMode = {$IFDEF VER270_UP}sbmRightToLeft{$ELSE}scsbmRightToLeft{$ENDIF}) then
    begin
      if SC_MODERNARROWS or IsWindowsModernStyle then
        scDrawUtils.DrawModernMenuArrowImage(Canvas, LSubMenuRect, C, 2, FScaleFactor)
      else
        scDrawUtils.DrawMenuArrowImage(Canvas, LSubMenuRect, C, 1, FScaleFactor)
    end
    else
    begin
      if SC_MODERNARROWS or IsWindowsModernStyle then
        scDrawUtils.DrawModernMenuArrowImage(Canvas, LSubMenuRect, C, 1, FScaleFactor)
      else
        scDrawUtils.DrawMenuArrowImage(Canvas, LSubMenuRect, C, 2, FScaleFactor);
    end;

    Dec(LTextRect.Right, LSubMenuRect.Width);
  end;

  procedure DrawVisualSysBitmap(Bmp: HBITMAP);
  var
    Theme: HTHEME;
    LRect, R: TRect;
    iPart, iState: Integer;
    LSize: TSize;
  begin
    Theme := OpenThemeData(0, 'Menu');
    iPart := MENU_SYSTEMCLOSE;
    case hBmp of
      HBMMENU_POPUP_RESTORE:
        iPart := MENU_SYSTEMRESTORE;
      HBMMENU_POPUP_MINIMIZE:
        iPart := MENU_SYSTEMMINIMIZE;
      HBMMENU_POPUP_MAXIMIZE:
        iPart := MENU_SYSTEMMAXIMIZE;
      HBMMENU_POPUP_CLOSE:
        iPart := MENU_SYSTEMCLOSE;

    end;
    iState := Integer(scisDisabled in State) + 1;

    Winapi.UxTheme.GetThemePartSize(Theme, DC, iPart, iState, nil,
      TS_TRUE, LSize);
    LRect := Rect(0, 0, LSize.Width, LSize.Height);
    R := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + 30, ItemRect.Bottom);
    RectCenter(LRect, R);
    Winapi.UxTheme.DrawThemeBackground(Theme, DC, iPart, iState, LRect, nil);
    CloseThemeData(Theme);
  end;

  procedure DrawSpecialChar(DC: HDC; Sign: Char; DestRect: TRect;
    const Bold: Boolean = False; const Disabled: Boolean = False);
  var
    LogFont: TLogFont;
    pOldFont: HGDIOBJ;
    AFont: HFONT;
    oldColor: COLORREF;
    OldMode: Integer;
    LColor: TColor;
  begin

    LogFont.lfHeight := DestRect.Height;
    LogFont.lfWidth := 0;
    LogFont.lfEscapement := 0;
    LogFont.lfOrientation := 0;
    if Bold then
      LogFont.lfWeight := FW_BOLD
    else
      LogFont.lfWeight := FW_NORMAL;
    LogFont.lfItalic := 0;
    LogFont.lfUnderline := 0;
    LogFont.lfStrikeOut := 0;
    LogFont.lfCharSet := DEFAULT_CHARSET;
    LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
    LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    LogFont.lfQuality := DEFAULT_QUALITY;
    LogFont.lfPitchAndFamily := DEFAULT_PITCH;
    LogFont.lfFaceName := 'Marlett';
    AFont := CreateFontIndirect(LogFont);

    oldColor := 0;
    if Disabled then
      oldColor := GetSysColor(COLOR_GRAYTEXT);

    oldColor := SetTextColor(DC, oldColor);
    pOldFont := SelectObject(DC, AFont);
    OldMode := SetBkMode(DC, Transparent);

    if StyleServices.GetElementColor(LDetails, ecTextColor, LColor) then
      SetTextColor(DC, LColor);

    Winapi.Windows.DrawText(DC, Sign, 1, DestRect, DT_LEFT or DT_SINGLELINE);
    SetBkMode(DC, OldMode);
    SelectObject(DC, pOldFont);
    SelectObject(DC, oldColor);
    DeleteObject(AFont);
  end;

begin
  FDrawSelColorText := False;
  DisplayCheckedGlyph := True;
  LTextRect := ItemRect;
  SysItem := Items[Index];
  DC := Canvas.Handle;
  R := ItemRect;
  Detail := tmPopupItemNormal;
  if scisHot in State then
    Detail := tmPopupItemHot;
  if scisDisabled in State then
    Detail := tmPopupItemDisabled;

  if Style = scisSep then
    begin
      Detail := tmPopupSeparator;
      Inc(R.Left, 2);
      Dec(R.Right, 2);
    end;

  LDetails := StyleServices.GetElementDetails(Detail);

  if (scisDisabled in State) and (scisHot in State) then
  begin
    if SC_MENUCOLORSELECTION and (Detail <> tmPopupSeparator) then
    begin
      with Canvas do
      begin
        Brush.Color := GetStyleColor(clHighLight);
        Brush.Style := bsSolid;
        FillRectWithAlpha(Canvas, R, 50);
      end
    end
    else
      scDrawUtils.DrawPopupSelectionWithAlpha(Canvas, R, 50);
  end
  else
  if (Detail <> tmPopupItemNormal) and (Detail <> tmPopupItemDisabled) then
  begin
    if SC_MENUCOLORSELECTION and (Detail <> tmPopupSeparator) then
    begin
      with Canvas do
      begin
        Brush.Color := GetStyleColor(clHighLight);
        Brush.Style := bsSolid;
        FillRect(R);
        FDrawSelColorText := True;
      end
    end
    else
      StyleServices.DrawElement(DC, LDetails, R);
  end;

  if Style = scisDropDown then
    DrawSubMenu(ItemRect);

  MI := SysItem.VCLMenuItem;
  MIImages := nil;

  if (MI <> nil) and Assigned(MI.OnDrawItem) then
  begin
    MI.OnDrawItem(MI, Canvas, ItemRect, scisHot in State);
    Exit;
  end
  else
  if (MI <> nil) and (Length(MI.Caption) > 1) and
     (MI.Caption[1] = '-') and (MI.Caption[Length(MI.Caption)] = '-') and
     not MI.Enabled and SC_MenuHeadersSupport then
  begin
    DrawMenuHeader(Canvas, ItemRect, MI.Caption);
    Exit;
  end;

  if (MI <> nil) and Assigned(MI.OnAdvancedDrawItem) then
  begin
    VCLItemState := [];
    if scisDisabled in State then
      VCLItemState := VCLItemState + [odDisabled];
    if scisHot in State then
      VCLItemState := VCLItemState + [odSelected];
    MI.OnAdvancedDrawItem(MI, Canvas, ItemRect, VCLItemState);
    Exit;
  end;

  if MI <> nil then
    begin
      { Draw Vcl PopupMenu Bitmap }
      ImageIndex := MI.ImageIndex;
      MIIMages := MI.GetImageList;
        with MI.GetParentMenu do
          begin
            if (ImageIndex < 0) and (MI.Bitmap <> nil) then
              begin
                Bmp := MI.Bitmap;
                LImageRect := Rect(0, 0, Bmp.Width, Bmp.Height);
                RectVCenter(LImageRect, ItemRect);
                if TStyleManager.SystemStyle.Enabled then
                begin
                  if not RightToLeft then
                   OffsetRect(LImageRect, 4, 0)
                  else
                    begin
                      LImageRect.Left := ItemRect.Right - Bmp.Width - 4;
                      LImageRect.Right := ItemRect.Right;
                    end;
                end
                else
                begin
                  if not RightToLeft then
                   OffsetRect(LImageRect, 2, 0)
                  else
                    begin
                      LImageRect.Left := ItemRect.Right - Bmp.Width - 2;
                      LImageRect.Right := ItemRect.Right;
                    end;
                end;
                Canvas.Draw(LImageRect.Left, LImageRect.Top, Bmp);
              end
            else
              if (MIImages <> nil) and (ImageIndex > -1) then
              begin
                DisplayCheckedGlyph := False;
                LImageRect := Rect(0, 0, MIImages.Width, MIImages.Height);
                RectVCenter(LImageRect, ItemRect);
                if TStyleManager.SystemStyle.Enabled then
                begin
                  if not RightToLeft then
                    OffsetRect(LImageRect, 4, 0)
                  else
                    begin
                      LImageRect.Left := ItemRect.Right - Images.Width - 4;
                      LImageRect.Right := ItemRect.Right;
                    end;
                end
                else
                begin
                  if not RightToLeft then
                    OffsetRect(LImageRect, 2, 0)
                  else
                    begin
                      LImageRect.Left := ItemRect.Right - Images.Width - 2;
                      LImageRect.Right := ItemRect.Right;
                    end;
                end;
                MIImages.Draw(Canvas, LImageRect.Left, LImageRect.Top,
                  ImageIndex, MI.Enabled);
              end;
          end;
    end
  else if SysItem.Bitmap > 0 then
    begin
      hBmp := SysItem.Bitmap;
      if hBmp < HBMMENU_POPUP_MINIMIZE + 1 then
        begin
          { Draw System PopupMenu Bitmap }
          DisplayCheckedGlyph := False;
          // DrawVisualSysBitmap(hBmp);
          case hBmp of
            HBMMENU_POPUP_RESTORE:
              Sign := MARLETT_RESTORE_CHAR;
            HBMMENU_POPUP_MINIMIZE, HBMMENU_MBAR_MINIMIZE_D:
              Sign := MARLETT_MINIMIZE_CHAR;
            HBMMENU_POPUP_MAXIMIZE:
              Sign := MARLETT_MAXIMIZE_CHAR;
            HBMMENU_POPUP_CLOSE, HBMMENU_MBAR_CLOSE_D:
              Sign := MARLETT_CLOSE_CHAR;
          else
            Sign := Char(0);
          end;
          if Sign <> #0 then
            begin
              LImageRect := Rect(0, 0, ItemRect.Height div 2, ItemRect.Height div 2);

              if TStyleManager.SystemStyle.Enabled then
                R := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + ItemRect.Height,
                  ItemRect.Bottom)
              else
                R := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + ItemRect.Height,
                  ItemRect.Bottom);

              RectCenter(LImageRect, ItemRect);

              if TStyleManager.SystemStyle.Enabled then
              begin
                if not RightToLeft then
                begin
                  LImageRect.Left := ItemRect.Left + 10;
                end
                else
                  begin
                    LImageRect.Left := ItemRect.Right - 10 - 4;
                    LImageRect.Right := ItemRect.Right;
                  end;
              end
              else
              begin
                if not RightToLeft then
                  LImageRect.Left := ItemRect.Left + 5
                else
                  begin
                    LImageRect.Left := ItemRect.Right - 5 - 4;
                    LImageRect.Right := ItemRect.Right;
                  end;
              end;
              DrawSpecialChar(DC, Sign, LImageRect, False,
                (scisDisabled in State));
            end;
        end
      else
        begin
          { Draw PopupMenu Bitmap }
          DisplayCheckedGlyph := False;
          BmpWidth := GetBitmapWidth(hBmp);
          BmpHeight := GetBitmapHeight(hBmp);
          LImageRect := Rect(0, 0, BmpWidth, BmpHeight);
          RectVCenter(LImageRect, ItemRect);
          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
            begin
              LImageRect.Left := ItemRect.Right - BmpWidth - 4;
              LImageRect.Right := ItemRect.Right;
            end;
          Icon := BmpToIcon(hBmp);
          DrawIconEX(DC, LImageRect.Left, LImageRect.Top, Icon, BmpWidth,
            BmpHeight, 0, 0, DI_NORMAL);
          DestroyIcon(Icon);
        end;
    end;

  if (SysItem.Checked) and (DisplayCheckedGlyph) then
   begin
      FState := scsNormal;
      if scisDisabled in State then
        FState := scsDisabled
      else
      if scisHot in State then
        FState := scsHot;

      if SysItem.RadioCheck then
        LDetails := StyleServices.GetElementDetails(tmPopupCheckNormal)
      else
        LDetails := StyleServices.GetElementDetails(tmPopupBulletNormal);
      StyleServices.GetElementSize(DC, LDetails, esActual, LSize);
      LImageRect := Rect(0, 0, LSize.Width, LSize.Height);
      RectVCenter(LImageRect, ItemRect);
      if not RightToLeft then
        OffsetRect(LImageRect, 4, 0)
      else
        begin
          LImageRect.Left := ItemRect.Right - LSize.Width - 4;
          LImageRect.Right := ItemRect.Right;
        end;
      C := scDrawUtils.GetPopupItemTextColor(FState);
      if SysItem.RadioCheck then
      begin
        scDrawUtils.DrawMenuRadioImage(Canvas, LImageRect, C);
      end
      else
      begin
        scDrawUtils.DrawMenuCheckImage(Canvas, LImageRect, C);
      end;
    end;

  { Draw Text }
  LTextFormat := [tfLeft, tfVerticalCenter, tfSingleLine, tfExpandTabs];

  if not RightToLeft then
  begin
    if (MIImages <> nil) and (MIImages.Width > 16) then
    begin
      if TStyleManager.SystemStyle.Enabled then
        Inc(LTextRect.Left, MIImages.Width + 10)
      else
        Inc(LTextRect.Left, MIImages.Width + 7);
    end
    else
      if TStyleManager.SystemStyle.Enabled then
        Inc(LTextRect.Left, Round(ItemRect.Height * 1.3))
      else
        Inc(LTextRect.Left, ItemRect.Height);
  end
  else
    begin
      LTextRect.Left := ItemRect.Left;
      if (MIImages <> nil) and (MIImages.Width > 16) then
      begin
        if TStyleManager.SystemStyle.Enabled then
          Dec(LTextRect.Right, MIImages.Width + 10)
        else
          Dec(LTextRect.Right, MIImages.Width + 7);
      end
      else
        if TStyleManager.SystemStyle.Enabled then
          Dec(LTextRect.Right, Round(ItemRect.Height * 1.3))
        else
          Dec(LTextRect.Right, ItemRect.Height);
      Exclude(LTextFormat, tfLeft);
      Include(LTextFormat, tfRtlReading);
      Include(LTextFormat, tfRight);
    end;

  LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
  if scisHot in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
  if scisDisabled in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemDisabled);

  if SysItem.DefaultItem then
    Canvas.Font.Style := [fsBold];

  if FDrawSelColorText then
    DrawSelectionText(ItemText, LTextRect, LTextFormat)
  else
    DrawText(Canvas.Handle, LDetails, ItemText, LTextRect, LTextFormat);

  { Draw ShortCut Text . }
  if MI <> nil then
    begin
      if MI.ShortCut <> 0 then
        begin
          sShortCut := ShortCutToText(MI.ShortCut);
          LTextRect := ItemRect;
          if RightToLeft then
            begin
              LTextRect.Left := ItemRect.Left + 14;
              LTextRect.Right := LTextRect.Left + Canvas.TextWidth(sShortCut);
            end
          else
            begin
              LTextRect.Left := ItemRect.Right - 14 -
                Canvas.TextWidth(sShortCut);
              LTextRect.Right := ItemRect.Right;
            end;
          if FDrawSelColorText then
            DrawSelectionText(sShortCut, LTextRect, LTextFormat)
          else
            DrawText(Canvas.Handle, LDetails, sShortCut, LTextRect, LTextFormat);
        end;
    end;
end;

function TscSysPopupStyleHook.GetItemsCount: Integer;
begin
  Result := GetMenuItemCount(FMenu);
end;

function TscSysPopupStyleHook.GetMenuFromHandle(AHandle: HWND): HMENU;
begin
  Result := SendMessage(AHandle, MN_GETHMENU, 0, 0);
end;

function TscSysPopupStyleHook.GetRightToLeft: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_TYPE;
  GetMenuItemInfo(FMenu, 0, True, info);
  Result := ((info.fType and MFT_RIGHTORDER) = MFT_RIGHTORDER) or
    ((info.fType and MFT_RIGHTJUSTIFY) = MFT_RIGHTJUSTIFY);
end;

function TscSysPopupStyleHook.GeTscSysPopupItem(Index: Integer): TscSysPopupItem;
begin
  Result := nil;
  if (Index > -1) and (Index <= Count) then
    begin
      if Assigned(FSysPopupItem) then
        FreeAndNil(FSysPopupItem);
      FSysPopupItem := TscSysPopupItem.Create(SysControl, Index, FMenu);
      Result := FSysPopupItem;
    end;
end;

procedure TscSysPopupStyleHook.PaintBackground(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  R: TRect;
begin
  R := SysControl.ClientRect;
  InflateRect(R, 3, 3);
  LDetails := StyleServices.GetElementDetails(tmPopupBorders);
  StyleServices.DrawElement(Canvas.Handle, LDetails, R);
  if (SC_MenuImageCollection <> nil) and (SC_MenuBackgroundIndex >= 0) and
     (SC_MenuBackgroundIndex < SC_MenuImageCollection.Images.Count)
  then
    SC_MenuImageCollection.Draw(Canvas, SysControl.ClientRect, SC_MenuBackgroundIndex);
  if (SC_MenuImageCollection <> nil) and (SC_MenuWallpaperIndex >= 0) and
     (SC_MenuWallpaperIndex < SC_MenuImageCollection.Images.Count)
  then
    SC_MenuImageCollection.Draw(Canvas, SysControl.ClientRect, SC_MenuWallpaperIndex);
end;

procedure TscSysPopupStyleHook.UpdateColors;
begin
  inherited;
end;

type
  TSubMenuItemInfo = record
    Menu: HMENU;
    WindowHandle: HWND;
    ItemIndex: Integer;
  end;

var
  SubMenuItemInfoArray: array of TSubMenuItemInfo;

destructor TscSysPopupStyleHook.Destroy;
begin
  if Assigned(FSysPopupItem) then
    FreeAndNil(FSysPopupItem);
  SetLength(SubMenuItemInfoArray, 0);
  SubMenuItemInfoArray := nil;
  inherited;
end;

{$IFDEF VER330_UP}
function TscSysPopupStyleHook.GetDevicePPI: Integer;
begin
  if CheckPerMonitorV2SupportForWindow(Self.Handle) then
    Result := GetDPIForWindow(Self.Handle)
  else
    Result := Screen.PixelsPerInch;
end;
{$ENDIF}

procedure TscSysPopupStyleHook.MNSELECTITEM(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  Index: Integer;
  i: Word;
  L: Integer;
  ParentItem: Integer;
  ParentPopup: HWND;
  LMenu: HMENU;
begin
  ParentPopup := 0;
  ParentItem := -1;
  Handled := False;

  DC := GetDC(Handle);
  Canvas := TCanvas.Create;

  try
    Canvas.Handle := DC;
    {$IFDEF VER330_UP}
    Canvas.Font := Screen.MenuFont;
    Canvas.Font.Height := MulDiv(Canvas.Font.Height, GetDevicePPI, Screen.PixelsPerInch);
    {$ENDIF}

    Index := Integer(Message.WParam);

    if not FItemsPainted then
    begin
      if (Index = FCount - 1) then
      begin
        DrawPopupWindow(Canvas);
        PostMessage(Handle, WM_ITEMSPAINTED, 0, 0);
      end;
      Handled := True;
      Exit;
    end;

    L := Length(SubMenuItemInfoArray);
    if L <> 0 then
      begin
        for i := 0 to L - 1 do
          begin
            { Look for SubMenu Parent }
            LMenu := SubMenuItemInfoArray[i].Menu;
            if LMenu = FMenu then
              begin
                ParentPopup := SubMenuItemInfoArray[i].WindowHandle;
                ParentItem := SubMenuItemInfoArray[i].ItemIndex;
                Break;
              end;
          end;
      end;

    if (Index > FCount - 1) or (Index < 0) then
    begin
      // send parent popup to update
      if (ParentPopup <> 0) and (ParentPopup <> Handle) and not FFromKeyboard then
        SendMessage(ParentPopup, WM_NEEDREDRAW, 1, 0);
      //
      if FItemsPainted then
      begin
        if not FAlphaBlend and FCanSetRedraw then
          SetRedraw(False)
        else
          LockWindowUpdate(Handle);
      end;
      Message.Result := CallDefaultProc(Message);
      if FItemsPainted then
      begin
        if not FAlphaBlend and FCanSetRedraw then
          SetRedraw(True)
        else
          LockWindowUpdate(0);
      end;
      FPreviousHotItemIndex := -1;
      DrawPopupWindow(Canvas);
      Handled := True;
      Exit;
    end;

    if ((ParentPopup <> Handle) and (FItemsPainted) and (ParentPopup <> 0))
    then
      begin
        if (not FParentSubItemPainted) and (ParentItem > -1) then
          begin
            SendMessage(ParentPopup, MN_SELECTITEM, ParentItem, 0);
            FParentSubItemPainted := True;
          end;
        if Integer(Message.WParam) <> (Self.FActiveItemIndex) then
        begin
          PostMessage(Handle, WM_NEEDREDRAW, 0, 0);
        end;  
      end;

    if Items[Index].HasSubMenu then
      begin
        L := Length(SubMenuItemInfoArray);
        if L = 0 then
          SetLength(SubMenuItemInfoArray, 1);
        for i := 0 to L do
          if SubMenuItemInfoArray[i].Menu <> GetMenuFromHandle(Handle) then
            begin
              Inc(L);
              SetLength(SubMenuItemInfoArray, L);
              SubMenuItemInfoArray[L - 1].Menu := GetSubMenu(FMenu, Index);
              SubMenuItemInfoArray[L - 1].WindowHandle := Handle;
              SubMenuItemInfoArray[L - 1].ItemIndex := Index;
              Break;
            end;
      end;

      if FItemsPainted then
      begin
        // send parent popup to update
        if (ParentPopup <> 0) and (ParentPopup <> Handle) and
           (FPreviousHotItemIndex <> Index) and not
           FFromKeyboard
        then
           SendMessage(ParentPopup, WM_NEEDREDRAW, 1, 0);
        //
        if not FAlphaBlend and FCanSetRedraw then
          SetRedraw(False)
        else
          LockWindowUpdate(Handle);
        try
          Message.Result := CallDefaultProc(Message);
        finally
          if not FAlphaBlend and FCanSetRedraw then
            SetRedraw(True)
          else
            LockWindowUpdate(0);
        end;
        if FPreviousHotItemIndex <> Index then
        begin
          FPreviousHotItemIndex := Index;
          DrawPopupWindow(Canvas);
        end;
      end;

  finally
    Canvas.Free;
    ReleaseDC(Handle, DC);
  end;
  Handled := True;
end;

procedure TscSysPopupStyleHook.DrawMenuHeader(ACanvas: TCanvas; ARect: TRect; ACaption: String);
var
  TextColor: TColor;
  S: String;
  I: Integer;
begin
  DrawHeaderSection(ACanvas, ARect, scsNormal);
  TextColor := scDrawUtils.GetHeaderTextColor(scsNormal);
  SetTextColor(ACanvas.Handle, TextColor);
  SetBkMode(ACanvas.Handle, TRANSPARENT);
  S := ACaption;
  Delete(S, 1, 1);
  Delete(S, Length(S), 1);
  I := Pos('&', S);
  if I <> 0 then
    Delete(S, I, 1);
  DrawTextAlignment(ACanvas, S, ARect, taCenter);
end;

procedure TscSysPopupStyleHook.DrawPopupWindow(Canvas: TCanvas);
var
  R, R1: TRect;
  I: Integer;
  Buffer: TBitmap;
begin
  Canvas.Font := Screen.MenuFont;
  {$IFDEF VER330_UP}
  Canvas.Font.Height := MulDiv(Canvas.Font.Height, GetDevicePPI, Screen.PixelsPerInch);
  {$ENDIF}

  GetWindowRect(Handle, R);
  if R.Width * R.Height = 0 then Exit;
  Buffer := TBitmap.Create;
  FActiveItemIndex := -1;
  try
    Buffer.SetSize(R.Width, R.Height);
    R1 := Rect(0, 0, Buffer.Width, Buffer.Height);
    PaintBackground(Buffer.Canvas);
    Buffer.Canvas.Font := Canvas.Font;
    for I := 0 to Count - 1 do
      DoDrawItem(Buffer.Canvas, I);
    if (SC_MenuImageCollection <> nil) and (SC_MenuBackgroundOverContentIndex >= 0) and
       (SC_MenuBackgroundOverContentIndex < SC_MenuImageCollection.Images.Count)
    then
      SC_MenuImageCollection.Draw(Buffer.Canvas,
        R1, SC_MenuBackgroundOverContentIndex);
    Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TscSysPopupStyleHook.WMPRINT(var Message: TMessage);
var
  DC: HDC;
  i: Integer;
  Canvas: TCanvas;
  R: TRect;
  LDetails: TThemedElementDetails;
begin
  FMenu := GetMenuFromHandle(Handle);
  FCount := GetItemsCount;

  if Message.WParam <> 0 then
    DC := HDC(Message.WParam)
  else
    DC := GetDC(Handle);

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    if (Message.LParam and PRF_NONCLIENT = PRF_NONCLIENT) and
       (Message.wParam > 0) then
    begin
      R := Rect(0, 0, SysControl.Width, SysControl.Height);
      LDetails := StyleServices.GetElementDetails(tmPopupBorders);
      StyleServices.DrawElement(Canvas.Handle, LDetails, R);
    end
    else
      PaintBackground(Canvas);
  finally
    Canvas.Free;
    if DC <> HDC(Message.WParam) then
      ReleaseDC(Handle, DC);
  end;

  if (Count > -1) and not FItemsPainted then
  begin
    for i := 0 to Count - 1 do
      PostMessage(Handle, MN_SELECTITEM, i, 0);
  end
  else
    if (Count > -1) and (FPreviousHotItemIndex > - 1) and
       (FPreviousHotItemIndex < Count - 1)
    then
      PostMessage(Handle, MN_SELECTITEM, FPreviousHotItemIndex, 0);

  Handled := True;
end;

function IsItemSeparator(Menu: HMENU; ItemIndex: Integer): Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  GetMenuItemInfo(Menu, ItemIndex, True, info);
  Result := (info.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
end;

procedure TscSysPopupStyleHook.WndProc(var Message: TMessage);
var
  i: Integer;
  DC: HDC;
  Canvas: TCanvas;
  R, R1: TRect;
  Buffer, Buffer2: TBitmap;
  LDetails: TThemedElementDetails;
  P: TPoint;
begin

  case Message.Msg of

    WM_NEEDREDRAW:
    begin
      DC := GetDC(Handle);
      Canvas := TCanvas.Create;
      Canvas.Handle := DC;
      DrawPopupWindow(Canvas);
      Canvas.Handle := 0;
      Canvas.Free;
      ReleaseDC(Handle, DC);
      if Message.WParam = 0 then
      begin
        if FAlphaBlend then
          SetLayeredWindowAttributes(Handle, clNone,
            SC_MenuAlphaBlendValue, ULW_ALPHA);
      end;
    end;

    WM_ITEMSPAINTED:
      begin
        if (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0) and
           (SC_MenuAlphaBlendValue < 255) then
        begin
         SetWindowLong(Handle, GWL_EXSTYLE,
         GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
         FAlphaBlend := True;
        end;
        if FAlphaBlend then
          SetLayeredWindowAttributes(Handle, clNone,
            SC_MenuAlphaBlendValue, ULW_ALPHA);
        FItemsPainted := True;
      end;

    MN_MOUSEMOVE:
      begin
        FMouseMove := True;
      end;

    WM_PRINT:
    begin
      if (not OverridePaint) or (not OverridePaintNC) then
      begin
        Message.Result := CallDefaultProc(Message);
        Exit;
      end;
    end;

    MN_BUTTONDOWN:
    begin
      FLBDown := True;
    end;

    MN_SELECTITEM:
      begin
        if (GetKeyState(VK_LBUTTON) < 0)
           and FItemsPainted and not FLBDown and FMouseMove
        then
        begin
          GetCursorPos(P);
          if WinApi.Windows.WindowFromPoint(P) = Handle then
          begin
            FLBDown := True;
            Sendmessage(Handle, MN_BUTTONDOWN, 0, 0);
          end;
        end;
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
       end;

    WM_WINDOWPOSCHANGED:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;

      if FCallFromKeyboard then
          SetTimer(Handle, $93, 10, nil);
      end;

      WM_TIMER:
      begin
        if (FItemsPainted) and (Message.WParam = $93) then
        begin
          FCallFromKeyboard := False;
          PostMessage(Handle, WM_KEYDOWN, VK_DOWN, 0);
          KillTimer(Handle, $93);
        end;
      end;

     WM_KEYDOWN:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        FMenu := GetMenuFromHandle(Handle);
        if FPreviousHotItemIndex <> -1 then
          FKeyIndex := FPreviousHotItemIndex;

        if (Message.WParam = VK_RETURN) or (Message.WParam = VK_LEFT) or
           (Message.WParam = VK_RIGHT)
        then
          begin
            if (FPreviousHotItemIndex <> -1) and
                Items[FPreviousHotItemIndex].HasSubMenu
            then
              FCallFromKeyboard := True;
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;

        case Message.WParam of
          VK_DOWN:
              begin
                if FKeyIndex >= GetMenuItemCount(Menu) - 1 then
                  FKeyIndex := -1;
                Inc(FKeyIndex);
                if IsItemSeparator(Menu, FKeyIndex) then
                  for i := FKeyIndex to GetMenuItemCount(Menu) - 1 do
                    if (not IsItemSeparator(Menu, i)) then
                      begin
                        FKeyIndex := i;
                        Break;
                      end;
                if Count = 1 then FPreviousHotItemIndex := -1;
                FFromKeyboard := True;
                SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
                FFromKeyboard := False;
                Message.Result := 0;
              end;
          VK_UP:
            begin
              if FKeyIndex <= 0 then
                FKeyIndex := GetMenuItemCount(Menu);

              Dec(FKeyIndex);
              if IsItemSeparator(Menu, FKeyIndex) then
                for i := FKeyIndex downto 0 do
                  if not IsItemSeparator(Menu, i) then
                    begin
                      FKeyIndex := i;
                      Break;
                    end;
              FFromKeyboard := True;
              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              FFromKeyboard := False;
              Message.Result := 0;
            end;
        else
          Message.Result := CallDefaultProc(Message);
        end;
        Exit;
      end;

    WM_ERASEBKGND:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        if not FItemsPainted then
          SendMessage(Handle, WM_PRINT, Message.wParam, PRF_CLIENT);
        Message.Result := 1;
        Exit;
      end;

    WM_NCPAINT:
    begin
      DC := GetWindowDC(Handle);
      Canvas := TCanvas.Create;
      Canvas.Handle := DC;
      Buffer := TBitmap.Create;
      Buffer2 := TBitMap.Create;
      R := Rect(0, 0, SysControl.Width, SysControl.Height);
      Buffer.SetSize(R.Width, R.Height);
      R1 := R;
      InflateRect(R1, -3, -3);
      Buffer2.SetSize(R1.Width, R1.Height);
      try
        LDetails := StyleServices.GetElementDetails(tmPopupBorders);
        StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails, R);
        DrawPopupWindow(Buffer2.Canvas);
        Buffer.Canvas.Draw(R1.Left, R1.Top, Buffer2);
        Canvas.Draw(0, 0, Buffer);
      finally
        Buffer2.Free;
        Buffer.Free;
        Canvas.Handle := 0;
        Canvas.Free;
        ReleaseDC(Handle, DC);
      end;
      Exit;
    end;
  end;
  inherited;
end;

{ TscSysPopupItem }
constructor TscSysPopupStyleHook.TscSysPopupItem.Create(SysParent: {$IFDEF VER270_UP}TSysControl{$ELSE}TscSysControl{$ENDIF};
  Index: Integer; Menu: HMENU);
var
  I, J: Integer;
  FParentVCLItem: TMenuItem;
begin
  FMenu := Menu;
  FHandle := SysParent.Handle;
  FSysParent := SysParent;
  FIndex := Index;
  FVCLMenuItem := nil;
  FParentVCLItem := GetVCLParentMenuItem;
  if FParentVCLItem <> nil then
  begin
    J := -1;
    for I := 0 to FParentVCLItem.Count - 1 do
    begin
      if FParentVCLItem[I].Visible then Inc(J);
      if FIndex = J then
      begin
        FVCLMenuItem := FParentVCLItem[I];
        Break;
      end;
    end;
  end;
end;

destructor TscSysPopupStyleHook.TscSysPopupItem.Destroy;
begin
  inherited;
end;

function TscSysPopupStyleHook.TscSysPopupItem.GetItemBitmap: HBITMAP;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_CHECKMARKS or MIIM_BITMAP;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := info.hbmpItem;
  if Result = 0 then
    Result := info.hbmpUnchecked;
end;

function TscSysPopupStyleHook.TscSysPopupItem.GetItemRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (FMenu > 0) and (FIndex > -1) then
    GetMenuItemRect(0, FMenu, FIndex, Result);
end;

function TscSysPopupStyleHook.TscSysPopupItem.GetItemText: String;
var
  Buffer: PChar;
  StrSize: Integer;
  info: MenuItemInfo;
begin
  if VCLMenuItem <> nil then begin
    Result := VCLMenuItem.Caption;
    Exit;
  end;

  Result := '';

  FillChar(info, SizeOf(MenuItemInfo), 0);
  info.cbSize := SizeOf(MenuItemInfo);
  info.fMask := MIIM_STRING or MIIM_FTYPE;
  info.dwTypeData := nil;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  StrSize := info.cch * 2 + 2;
  GetMem(Buffer, StrSize);
  try
   info.dwTypeData := Buffer;
   Inc(info.cch);
   GetMenuItemInfo(FMenu, FIndex, True, info);
   Result := String(Buffer);
  finally
    FreeMem(Buffer, StrSize);
  end;
end;

function TscSysPopupStyleHook.TscSysPopupItem.GetVCLParentMenuItem: TMenuItem;

function GetVisibleCount(AItem: TMenuItem): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AItem.Count - 1 do
    if AItem[I].Visible then
      Inc(Result);
end;

function FindItemInMenu(AMenuItem: TMenuItem): TMenuItem;
var
  I: Integer;
  MI: TMenuItem;
begin
  Result := nil;
  for I := 0 to AMenuItem.Count - 1 do
  begin
    MI := TMenuItem(AMenuItem.Items[I]);
    if MI.Handle = FMenu then
    begin
      if FIndex < GetVisibleCount(MI) then
        Exit(MI)
      else
        Exit(nil);
    end
    else
    begin
      Result := FindItemInMenu(MI);
      if Result <> nil then
        Exit;
    end;
  end;
end;

function FindItemInFrame(AFrame: TCustomFrame): TMenuItem;
var
  I: Integer;
  MI: TMenuItem;
  PopupMenu: TPopupMenu;
begin
  Result := nil;
  for I := 0 to AFrame.ComponentCount - 1 do
  begin
    if AFrame.Components[I] is TMenuItem then
    begin
      MI := TMenuItem(AFrame.Components[I]);
      if (MI.Handle = FMenu) then
      begin
        if FIndex < GetVisibleCount(MI) then
          Exit(MI)
        else
          Exit(nil);
      end
      else
      begin
        Result := FindItemInMenu(MI);
        if Result <> nil then
          Exit;
      end;
    end
    else
    if AFrame.Components[I] is TPopupMenu then
    begin
      PopupMenu := TPopupMenu(AFrame.Components[I]);
      if PopupMenu.Handle = FMenu then
        Exit(PopupMenu.Items)
      else
      begin
        Result := FindItemInMenu(PopupMenu.Items);
        if Result <> nil then
          Exit;
      end;
    end
    else
    if AFrame.Components[I] is TCustomFrame then
    begin
      Result := FindItemInFrame(TCustomFrame(AFrame.Components[I]));
      if Result <> nil then
        Exit;
    end;
  end;
end;

var
  I, J: Integer;
  PopupMenu: TPopupMenu;
  Form: TForm;
  MI: TMenuItem;
begin
  Result := nil;

  for I := 0 to PopupList.Count - 1 do
   if TPopupMenu(PopupList[I]).Handle = FMenu then
      Exit(TPopupMenu(PopupList[I]).Items)
    else
    begin
      Result := FindItemInMenu(TPopupMenu(PopupList[I]).Items);
      if Result <> nil then
        Exit;
    end;

  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    for J := 0 to Form.ComponentCount - 1 do
    begin
      if Form.Components[J] is TMenuItem then
      begin
        MI := TMenuItem(Form.Components[J]);
        if (MI.Handle = FMenu) then
        begin
          if FIndex < GetVisibleCount(MI) then
            Exit(MI)
          else
            Exit(nil);
        end
        else
        begin
          Result := FindItemInMenu(MI);
          if Result <> nil then
            Exit;
        end;
      end
      else if Form.Components[J] is TPopupMenu then
      begin
        PopupMenu := TPopupMenu(Form.Components[J]);
        if PopupMenu.Handle = FMenu then
          Exit(PopupMenu.Items)
        else
        begin
          Result := FindItemInMenu(PopupMenu.Items);
          if Result <> nil then
            Exit;
        end;
      end
      else
      if Form.Components[J] is TCustomFrame then
      begin
        Result := FindItemInFrame(TCustomFrame(Form.Components[J]));
        if Result <> nil then
          Exit;
      end;
    end;
  end;
end;

function TscSysPopupStyleHook.TscSysPopupItem.IsItemDisabled: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_DISABLED = MFS_DISABLED) or
    (info.fState and MF_DISABLED = MF_DISABLED) or
    (info.fState and MF_GRAYED = MF_GRAYED);
end;

function TscSysPopupStyleHook.TscSysPopupItem.IsItemRadioCheck: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fType and MFT_RADIOCHECK) = MFT_RADIOCHECK;
end;

function TscSysPopupStyleHook.TscSysPopupItem.IsItemSelected: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_HILITE) = MFS_HILITE;
end;

function TscSysPopupStyleHook.TscSysPopupItem.IsItemChecked: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_CHECKED) = MFS_CHECKED;
end;

function TscSysPopupStyleHook.TscSysPopupItem.IsItemContainsSubMenu: Boolean;
begin
  Result := (GetSubMenu(FMenu, FIndex) > 0);
end;

function TscSysPopupStyleHook.TscSysPopupItem.IsItemDefault: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_DEFAULT) = MFS_DEFAULT;
end;

function TscSysPopupStyleHook.TscSysPopupItem.IsItemSeparator: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), 0);
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  Result := False;
  if (FIndex > -1) and (FIndex < GetMenuItemCount(FMenu) - 1) then
    begin
      GetMenuItemInfo(FMenu, FIndex, True, info);
      Result := (info.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
    end;
end;

constructor TscStyleManager.Create(AOwner: TComponent);
begin
  inherited;
  FStyleArrowType := scsatDefault;
  FMenuImages := nil;
  FMenuAlphaBlendValue := 255;
  FMenuWallpaperIndex := -1;
  FMenuBackgroundIndex := -1;
  FMenuBackgroundOverContentIndex := -1;
  FMenuHeadersSupport := True;
  FMenuHookEnabled := True;
end;

destructor TscStyleManager.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    SC_MenuImageCollection := nil;
    SC_MenuWallPaperIndex := -1;
    SC_MenuBackgroundIndex := -1;
    SC_MenuBackgroundOverContentIndex := -1;
    SC_MenuHeadersSupport := True;
  end;
  inherited;
end;

function TscStyleManager.GetRTLMode: Boolean;
begin
  Result := SC_RTLMODE;
end;

procedure TscStyleManager.SetRTLMode(Value: Boolean);
begin
  SC_RTLMODE := Value;
end;

function TscStyleManager.GetStyleArrowType: TscStyleArrowType;
begin
  if SC_MODERNARROWS then
    Result := scsatModern
  else
    Result := scsatDefault;
end;

procedure TscStyleManager.SetStyleArrowType(Value: TscStyleArrowType);
begin
  if Value = scsatDefault then
    scDrawUtils.SC_MODERNARROWS := False
  else
    scDrawUtils.SC_MODERNARROWS := True;
end;

function TscStyleManager.GetScaleFormBorder: Boolean;
begin
  Result := scDrawUtils.SC_SCALEFORMBORDER;
end;

procedure TscStyleManager.SetScaleFormBorder(Value: Boolean);
begin
  scDrawUtils.SC_SCALEFORMBORDER := Value;
end;

function TscStyleManager.GetScaleStyles: Boolean;
begin
  Result := scDrawUtils.SC_SCALESTYLES;
end;

procedure TscStyleManager.SetScaleStyles(Value: Boolean);
begin
  scDrawUtils.SC_SCALESTYLES := Value;
end;

function TscStyleManager.GetScaleThemes: Boolean;
begin
  Result := scDrawUtils.SC_SCALETHEMES;
end;

procedure TscStyleManager.SetScaleThemes(Value: Boolean);
begin
  scDrawUtils.SC_SCALETHEMES := Value;
end;

function TscStyleManager.GetScaleResources: Boolean;
begin
  Result := scDrawUtils.SC_SCALERESOURCES;
end;

procedure TscStyleManager.SetScaleResources(Value: Boolean);
begin
  scDrawUtils.SC_SCALERESOURCES := Value;
end;

procedure TscStyleManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FMenuImages) then
    MenuImages := nil;
end;

procedure TscStyleManager.SetMenuHookEnabled(Value: Boolean);
begin
  FMenuHookEnabled := Value;
  {$IFNDEF VER270_UP}
  TscSysStyleManager.MenuHookEnabled := FMenuHookEnabled;
  {$ENDIF}
end;

procedure TscStyleManager.SetMenuSelectionStyle(Value: TscMenuSelectionStyle);
begin
  FMenuSelectionStyle := Value;
  if not (csDesigning in ComponentState) then
    case FMenuSelectionStyle of
      scmssStyled: SC_MENUCOLORSELECTION := False;
      scmssColor: SC_MENUCOLORSELECTION := True;
    end;
end;

procedure TscStyleManager.SetMenuHeadersSupport(Value: Boolean);
begin
  FMenuHeadersSupport := Value;
  if not (csDesigning in ComponentState) then
    SC_MenuHeadersSupport := FMenuHeadersSupport;
end;

procedure TscStyleManager.SetMenuAlphaBlendValue(Value: Byte);
begin
  FMenuAlphaBlendValue := Value;
  if not (csDesigning in ComponentState) then
    SC_MenuAlphaBlendValue := FMenuAlphaBlendValue;
end;

procedure TscStyleManager.SetMenuWallpaperIndex(Value: Integer);
begin
  FMenuWallpaperIndex := Value;
  if not (csDesigning in ComponentState) then
     SC_MenuWallPaperIndex := FMenuWallpaperIndex;
end;

procedure TscStyleManager.SetMenuBackgroundIndex(Value: Integer);
begin
  FMenuBackgroundIndex := Value;
  if not (csDesigning in ComponentState) then
     SC_MenuBackgroundIndex := FMenuBackgroundIndex;
end;

procedure TscStyleManager.SetMenuBackgroundOverContentIndex(Value: Integer);
begin
  FMenuBackgroundOverContentIndex := Value;
  if not (csDesigning in ComponentState) then
     SC_MenuBackgroundOverContentIndex := FMenuBackgroundOverContentIndex;
end;

procedure TscStyleManager.SetMenuImages(Value: TscImageCollection);
begin
  FMenuImages := Value;
  if not (csDesigning in ComponentState) then
    SC_MenuImageCollection := FMenuImages;
end;

{$IFDEF VER300_UP}
const
  TTM_ADJUSTRECT = WM_USER + 31;

constructor TscSysButtonStyleHook.Create(AHandle: THandle);
begin
  inherited;
  FScaleFactor := 1;
  if SC_SCALESTYLES then
  begin
    FScaleFactor := Screen.PixelsPerInch / 96;
    if FScaleFactor < 1 then
      FScaleFactor := 1;
  end;
  ParentColor := True;
  StyleElements := [seFont, seClient, seBorder];
  Color := StyleServices.GetStyleColor(scWindow);
end;

procedure TscSysButtonStyleHook.DrawCheckBoxText(DC: HDC; Text: String;
  LDetails: TThemedElementDetails; R: TRect);
var
  TextFormat: TTextFormat;
begin
  if ShowText then
  begin
    TextFormat := [tfVerticalCenter, tfHidePrefix];
    if (SysControl.Style and BS_MULTILINE = BS_MULTILINE) then
      include(TextFormat, tfWordBreak)
    else
      include(TextFormat, tfSingleLine);
    if (SysControl.Style and BS_LEFT = BS_LEFT) then
      include(TextFormat, tfLeft)
    else if (SysControl.Style and BS_RIGHT = BS_RIGHT) then
      include(TextFormat, tfRight)
    else if (SysControl.Style and BS_CENTER = BS_CENTER) then
      include(TextFormat, tfCenter);
    DrawText(DC, LDetails, SysControl.Text, R, TextFormat);
  end;
end;

function TscSysButtonStyleHook.GetCheckBoxState: TSysCheckBoxState;
var
  LState: DWORD;
begin
  LState := SendMessage(Handle, BM_GETCHECK, 0, 0);
  Result := TSysCheckBoxState(LState)
end;

function TscSysButtonStyleHook.GetShowText: Boolean;
begin
  Result := (SysControl.Style and BS_TEXT = BS_TEXT);
end;

function TscSysButtonStyleHook.IsCheckBox: Boolean;
begin
  with SysControl do
    Result := (Style and BS_CHECKBOX = BS_CHECKBOX) or
      (Style and BS_AUTOCHECKBOX = BS_AUTOCHECKBOX);
end;

function TscSysButtonStyleHook.IsCommandButton: Boolean;
begin
  Result := (SysControl.Style and BS_COMMANDLINK = BS_COMMANDLINK) or
    (SysControl.Style and BS_DEFCOMMANDLINK = BS_DEFCOMMANDLINK);
end;

function TscSysButtonStyleHook.IsGroupBox: Boolean;
begin
  Result := (SysControl.Style and BS_GROUPBOX = BS_GROUPBOX);
end;

function TscSysButtonStyleHook.IsOwnerDraw: Boolean;
begin
  Result := (SysControl.Style and BS_OWNERDRAW = BS_OWNERDRAW);
end;

function TscSysButtonStyleHook.IsPushButton: Boolean;
begin
  with SysControl do
    Result := (Style and BS_PUSHBUTTON = BS_PUSHBUTTON) or
      (not CheckBox and not RadioButton and not GroupBox and not CommandButton);
end;

function TscSysButtonStyleHook.IsRadioButton: Boolean;
begin
  with SysControl do
    Result := (Style and BS_RADIOBUTTON = BS_RADIOBUTTON) or
      (Style and BS_AUTORADIOBUTTON = BS_AUTORADIOBUTTON);
end;

function TscSysButtonStyleHook.IsSplitButton: Boolean;
begin
  Result := (SysControl.Style and BS_SPLITBUTTON = BS_SPLITBUTTON) or
    (SysControl.Style and BS_DEFSPLITBUTTON = BS_DEFSPLITBUTTON);

end;

procedure TscSysButtonStyleHook.MouseEnter;
begin
  // Invalidate;
end;

procedure TscSysButtonStyleHook.MouseLeave;
begin
  Invalidate;
end;

procedure TscSysButtonStyleHook.Paint(Canvas: TCanvas);
begin
  //OutputDebugString(PChar('Paint '+IntToHex(SysControl.Handle, 8)));
  if not GroupBox or CommandButton then
    PaintBackground(Canvas)
  else
    Exit;

  if CommandButton then
    PaintButton(Canvas)
  else
  if CheckBox then
    PaintCheckBox(Canvas)
  else
  if RadioButton then
    PaintRadioButton(Canvas)
  else
  if PushButton then
    PaintButton(Canvas);

end;

procedure TscSysButtonStyleHook.PaintBackground(Canvas: TCanvas);
begin
  if not GroupBox then
    inherited;
end;

procedure TscSysButtonStyleHook.PaintButton(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
  Detail: TThemedButton;
  X, Y, i: Integer;
  IW, IH, IY: Integer;
  TextFormat: TTextFormat;
  IL: BUTTON_IMAGELIST;
  LText: string;
  DrawRect: TRect;
  ThemeTextColor: TColor;
  Buffer: string;
  BufferLength: Integer;
begin
  LText := SysControl.Text;
  LRect := SysControl.ClientRect;

  if SysControl.Enabled then
    Detail := tbPushButtonNormal
  else
    Detail := tbPushButtonDisabled;

  if MouseDown then
    Detail := tbPushButtonPressed
  else
  if MouseInControl then
    Detail := tbPushButtonHot
  else
  if Focused then
    Detail := tbPushButtonDefaulted;

  LDetails := StyleServices.GetElementDetails(Detail);
  DrawRect := SysControl.ClientRect;
  StyleServices.DrawElement(Canvas.Handle, LDetails, LRect);


  if Button_GetImageList(handle, IL) and (IL.himl <> 0) and
     ImageList_GetIconSize(IL.himl, IW, IH) then
  begin
    if (GetWindowLong(Handle, GWL_STYLE) and BS_COMMANDLINK) = BS_COMMANDLINK then
      IY := DrawRect.Top + 15
    else
      IY := DrawRect.Top + (DrawRect.Height - IH) div 2;
    ImageList_Draw(IL.himl, 0, Canvas.Handle, DrawRect.Left + 3, IY, ILD_NORMAL);
    Inc(DrawRect.Left, IW + 3);
  end;

  if CommandButton then
  begin
      if IL.himl = 0 then
        Inc(DrawRect.Left, 35);
      Inc(DrawRect.Top, 15);
      Inc(DrawRect.Left, 5);
      Canvas.Font := SysControl.Font;
      TextFormat := TTextFormatFlags(DT_LEFT);
      if StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
         Canvas.Font.Color := ThemeTextColor;
      StyleServices.DrawText(Canvas.Handle, LDetails, LText, DrawRect, TextFormat, Canvas.Font.Color);
      SetLength(Buffer, Button_GetNoteLength(Handle) + 1);
      if Length(Buffer) <> 0 then
      begin
        BufferLength := Length(Buffer);
        if Button_GetNote(Handle, PChar(Buffer), BufferLength) then
        begin
          TextFormat := TTextFormatFlags(DT_LEFT or DT_WORDBREAK);
          Inc(DrawRect.Top, Canvas.TextHeight('Wq') + 2);
          Canvas.Font.Size := 8;
          StyleServices.DrawText(Canvas.Handle, LDetails, Buffer, DrawRect,
            TextFormat, Canvas.Font.Color);
        end;
      end;
      if IL.himl = 0 then
      begin
        if MouseDown then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphPressed)
        else if MouseInControl then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphHot)
        else if SysControl.Enabled then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphNormal)
        else
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphDisabled);
        DrawRect.Right := 35;
        DrawRect.Left := 3;
        DrawRect.Top := 10;
        DrawRect.Bottom := DrawRect.Top + 32;
        StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);
      end;

  end
  else
  if SplitButton then
    with Canvas, SysControl do
    begin
      { draw vertical line }
      Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
      MoveTo(Width - 15, 3);
      LineTo(Width - 15, Height - 3);
      if Enabled then
        Pen.Color := StyleServices.GetSystemColor(clBtnHighLight)
      else
        Pen.Color := Font.Color;
      MoveTo(Width - 14, 3);
      LineTo(Width - 14, Height - 3);
      { Draw arrow }
      Pen.Color := Font.Color;
      X := Width - 8;
      Y := Height div 2 + 1;
      for i := 3 downto 0 do
      begin
        MoveTo(X - i, Y - i);
        LineTo(X + i + 1, Y - i);
      end;
    end;

  if ShowText and not IsCommandButton then
  begin
    TextFormat := [tfCenter, tfVerticalCenter, tfSingleLine, tfHidePrefix];
    if (SysControl.Style and BS_MULTILINE = BS_MULTILINE) then
    begin
      Exclude(TextFormat, tfSingleLine);
      include(TextFormat, tfWordBreak)
    end;


    DrawText(Canvas.Handle, LDetails, SysControl.Text, LRect, TextFormat);
  end;
end;

function TscSysButtonStyleHook.GetBoxRect: TRect;
var
  DC: HDC;
  sSize: TSize;
begin
  DC := GetDC(Handle);
  with SysControl do
  begin
    GetTextExtentPoint32(DC, Text, Length(Text) - 1, sSize);
    Result := Rect(0, sSize.Height div 2 + 1, Width - 0, Height - 0);
  end;
  ReleaseDC(Handle, DC);
  DeleteDC(DC);
end;

function TscSysButtonStyleHook.GetCaptionRect(Canvas: TCanvas): TRect;
const
  FCaptionMargin = 12;
begin
  with SysControl do
    if BiDiMode <> sbmRightToLeft then
      Result := Rect(FCaptionMargin, 0, FCaptionMargin + Canvas.TextWidth(Text),
        Canvas.TextHeight(Text))
    else
      Result := Rect(Width - Canvas.TextWidth(Text) - FCaptionMargin, 0,
        Width - FCaptionMargin, Canvas.TextHeight(Text));
end;

procedure TscSysButtonStyleHook.PaintRadioButton(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  DC: HDC;
  LRect: TRect;
  Detail: TThemedButton;
  TxtRect, BoxRect, FRect, R: TRect;
  LState: TSysCheckBoxState;
  Size: TSize;
  Buffer: TBitmap;
  BoxSize: TSize;
begin
  DC := Canvas.Handle;
  LRect := SysControl.ClientRect;
  LState := CheckBoxState;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(LRect);
  if SysControl.Enabled then
    Detail := tbRadioButtonUncheckedNormal
  else
    Detail := tbRadioButtonUncheckedDisabled;
  if MouseDown then
    Detail := tbRadioButtonUncheckedPressed
  else if MouseInControl then
    Detail := tbRadioButtonUncheckedHot;

  if LState = cbChecked then
    Detail := TThemedButton(Integer(Detail) + 4);

  if FScaleFactor > 1 then
  begin
    Size.cx := GetSystemMetrics(SM_CXMENUCHECK);
    Size.cy := GetSystemMetrics(SM_CYMENUCHECK);
  end
  else
  begin
    if not StyleServices.GetElementSize(DC,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, esActual, Size) then
     begin
       Size.cx := 13;
       Size.cy := 13;
     end;
  end;

  LDetails := StyleServices.GetElementDetails(Detail);
  BoxRect := Rect(0, 0, Size.cx, Size.cy);
  RectVCenter(BoxRect, LRect);

  if (SysControl.Style and BS_LEFTTEXT = BS_LEFTTEXT) then
  begin
    BoxRect.Left := LRect.Right - BoxRect.Width - 2;
    BoxRect.Right := LRect.Right;
    TxtRect := Rect(LRect.Left + 1, LRect.Top, BoxRect.Left, LRect.Bottom);
  end
  else
  begin
    OffsetRect(BoxRect, 1, 0);
    TxtRect := Rect(BoxRect.Right + 2, LRect.Top, LRect.Right, LRect.Bottom);
  end;

   if FScaleFactor > 1 then
  begin
    if not StyleServices.GetElementSize(DC,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, esActual, BoxSize) then
    begin
      BoxSize.cx := 13;
      BoxSize.cy := 13;
    end;
    Inc(BoxSize.cx, 2);
    Inc(BoxSize.cy, 2);
    Buffer := TBitmap.Create;
    Buffer.Width := BoxSize.cx;
    Buffer.Height := BoxSize.cy;
    Buffer.PixelFormat := pf32bit;
    try
      Bitmap_ClearAlpha(Buffer, 0);
      Buffer.AlphaFormat := afPremultiplied;
      StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails,
         Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
      Bitmap_DrawScaleAlpha_XY(Buffer, Canvas, BoxRect.Left, BoxRect.Top, 255,
       BoxRect.Width, BoxRect.Height);
    finally
      Buffer.Free;
    end;
  end
  else
    StyleServices.DrawElement(DC, LDetails, BoxRect);

  Inc(TxtRect.Left);
  Dec(TxtRect.Right);

  DrawCheckBoxText(DC, SysControl.Text, LDetails, TxtRect);

  if Focused then
  begin
    R := Rect(0, 0, 0, 0);
    Winapi.Windows.DrawText(DC, SysControl.Text, -1, R, DT_LEFT or DT_CALCRECT);
    FRect := TxtRect;
    FRect.Right := FRect.Left + R.Width + 2;
    Dec(FRect.Left, 2);
    FRect.Top := FRect.Top + FRect.Height div 2 - R.Height div 2 - 2;
    if FRect.Top < TxtRect.Top then
      FRect.Top := TxtRect.Top;
    FRect.Bottom := FRect.Top + R.Height + 4;
    if FRect.Bottom > TxtRect.Bottom then
      FRect.Bottom := TxtRect.Bottom;
    Canvas.Brush.Color := clWhite;
    Canvas.DrawFocusRect(FRect);
  end;

end;

procedure TscSysButtonStyleHook.PaintCheckBox(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  DC: HDC;
  LRect: TRect;
  Detail: TThemedButton;
  TxtRect, BoxRect, FRect, R: TRect;
  LState: TSysCheckBoxState;
  Size: TSize;
  Buffer: TBitmap;
  BoxSize: TSize;
begin
  DC := Canvas.Handle;
  LRect := SysControl.ClientRect;
  LState := CheckBoxState;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(LRect);
  if SysControl.Enabled then
    Detail := tbCheckBoxUncheckedNormal
  else
    Detail := tbCheckBoxUncheckedDisabled;
  if MouseDown then
    Detail := tbCheckBoxUncheckedPressed
  else if MouseInControl then
    Detail := tbCheckBoxUncheckedHot;

  if LState = cbChecked then
    Detail := TThemedButton(Integer(Detail) + 4);
  if LState = cbGrayed then
    Detail := TThemedButton(Integer(Detail) + 8);

  if FScaleFactor > 1 then
  begin
    Size.cx := GetSystemMetrics(SM_CXMENUCHECK);
    Size.cy := GetSystemMetrics(SM_CYMENUCHECK);
  end
  else
  begin
    if not StyleServices.GetElementSize(DC,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, esActual, Size) then
     begin
       Size.cx := 13;
       Size.cy := 13;
     end;
  end;

  LDetails := StyleServices.GetElementDetails(Detail);
  BoxRect := Rect(0, 0, Size.cx, Size.cy);
  BoxRect := RectVCenter(BoxRect, LRect);

  if (SysControl.Style and BS_LEFTTEXT = BS_LEFTTEXT) then
  begin
    BoxRect.Left := LRect.Right - BoxRect.Width - 2;
    BoxRect.Right := LRect.Right;
    TxtRect := Rect(LRect.Left + 1, LRect.Top, BoxRect.Left, LRect.Bottom);
  end
  else
  begin
    OffsetRect(BoxRect, 1, 0);
    TxtRect := Rect(BoxRect.Right + 2, LRect.Top, LRect.Right, LRect.Bottom);
  end;

  if FScaleFactor > 1 then
  begin
    if not StyleServices.GetElementSize(DC,
       StyleServices.GetElementDetails(tbCheckBoxCheckedNormal),
         R, esActual, BoxSize) then
    begin
      BoxSize.cx := 13;
      BoxSize.cy := 13;
    end;
    Inc(BoxSize.cx, 2);
    Inc(BoxSize.cy, 2);
    Buffer := TBitmap.Create;
    Buffer.Width := BoxSize.cx;
    Buffer.Height := BoxSize.cy;
    Buffer.PixelFormat := pf32bit;
    try
      Bitmap_ClearAlpha(Buffer, 0);
      Buffer.AlphaFormat := afPremultiplied;
      StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails,
         Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1));
      Bitmap_DrawScaleAlpha_XY(Buffer, Canvas, BoxRect.Left, BoxRect.Top, 255,
       BoxRect.Width, BoxRect.Height);
    finally
      Buffer.Free;
    end;
  end
  else
    StyleServices.DrawElement(DC, LDetails, BoxRect);

  Inc(TxtRect.Left);
  Dec(TxtRect.Right);

  DrawCheckBoxText(DC, SysControl.Text, LDetails, TxtRect);

  if Focused then
  begin
    R := Rect(0, 0, 0, 0);
    Winapi.Windows.DrawText(DC, SysControl.Text, -1, R, DT_LEFT or DT_CALCRECT);
    FRect := TxtRect;
    FRect.Right := FRect.Left + R.Width + 2;
    Dec(FRect.Left, 2);
    FRect.Top := FRect.Top + FRect.Height div 2 - R.Height div 2 - 2;
    if FRect.Top < TxtRect.Top then
      FRect.Top := TxtRect.Top;
    FRect.Bottom := FRect.Top + R.Height + 4;
    if FRect.Bottom > TxtRect.Bottom then
      FRect.Bottom := TxtRect.Bottom;
    Canvas.Brush.Color := clWhite;
    Canvas.DrawFocusRect(FRect);
  end;

end;

procedure TscSysButtonStyleHook.PaintGroupBox(Canvas: TCanvas);
var
  R, CaptionRect: TRect;
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
  procedure DoDrawParentBackground(DC: HDC; ARect: TRect);
  begin
    if SysControl.ParentHandle > 0 then
      DrawParentBackground(DC, @ARect)
    else
    begin
      Canvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
      Canvas.FillRect(ARect);
    end;
  end;

begin
  CaptionRect := GetCaptionRect(Canvas);
  R := GetBoxRect;

  if SysControl.Enabled then
    LDetails := StyleServices.GetElementDetails(tbGroupBoxNormal)
  else
    LDetails := StyleServices.GetElementDetails(tbGroupBoxDisabled);

  { Clean caption area }
  DoDrawParentBackground(Canvas.Handle, CaptionRect);
  ExcludeClipRect(Canvas.Handle, R.Left + 4, CaptionRect.Height + 2,
    R.Right - 4, R.Height - 2);
  { Clean GroupBox corners area }
  DoDrawParentBackground(Canvas.Handle, R);

  SaveIndex := SaveDC(Canvas.Handle);

  try
    ExcludeClipRect(Canvas.Handle, CaptionRect.Left, CaptionRect.Top,
      CaptionRect.Right, CaptionRect.Bottom);
    StyleServices.DrawElement(Canvas.Handle, LDetails, R);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;

  Inc(CaptionRect.Top, 3);
  { Paint Text }
  StyleServices.DrawText(Canvas.Handle, LDetails, SysControl.Text, CaptionRect,
    [tfSingleLine, tfVerticalCenter, tfLeft, tfHidePrefix]);

end;

procedure TscSysButtonStyleHook.PaintNC(Canvas: TCanvas);
begin
  if GroupBox then
    PaintGroupBox(Canvas);
end;

procedure TscSysButtonStyleHook.UpdateColors;
begin
  inherited;
end;

procedure TscSysButtonStyleHook.WMEraseBkgnd(var Message: TMessage);
begin
  if not OwnerDraw and not GroupBox and ParentBkGndPainted then
  begin
    Message.Result := 1;
    Handled := True;
  end
  else
    Handled := False;
end;

procedure TscSysButtonStyleHook.WMNCPaint(var Message: TMessage);
begin
  if not OwnerDraw and ParentBkGndPainted then
  begin
    inherited;
    Handled := True;
  end
  else
    Handled := False;
end;

procedure TscSysButtonStyleHook.WMPaint(var Message: TMessage);
begin
  if not OwnerDraw and ParentBkGndPainted then
  begin
    inherited;
    Handled := True;
  end
  else
    Handled := False;
end;

procedure TscSysButtonStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ENABLE:
      begin
        { Check first if Window is visible
          if you dont check ..the InVisible window will be visible .
        }
        if SysControl.Visible then
          Invalidate;
      end;

    WM_STYLECHANGING, WM_STYLECHANGED:
      begin
        Invalidate;
      end;

    WM_SETTEXT:
      begin
        SetRedraw(False);
        CallDefaultProc(Message);
        SetRedraw(True);
        Invalidate;
      end;

    WM_SETFOCUS, WM_KILLFOCUS:
      begin
        inherited;
        Invalidate;
      end;

  else
    inherited;
  end;

end;

procedure TscSysDialogStyleHook.Close;
begin
  if (Handle <> 0) and not(FSysCloseButtonDisabled) then
    PostMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

constructor TscSysDialogStyleHook.Create(AHandle: THandle);
begin
  inherited;
  FRegion := 0;
  FStyledCaptionHeight := 0;
  FCloseButtonRect := Rect(0, 0, 0, 0);
  FMinButtonRect := Rect(0, 0, 0, 0);
  FMaxButtonRect := Rect(0, 0, 0, 0);
  FHelpButtonRect := Rect(0, 0, 0, 0);
  StyleElements := [seFont, seClient, seBorder];
  FScaleFactor := 1;
  if SC_SCALEFORMBORDER then
  begin
    FScaleFactor := Screen.PixelsPerInch / 96;
    if FScaleFactor < 1 then
      FScaleFactor := 1;
  end;
  OverrideEraseBkgnd := True;
  FPressedButton := 0;
  FHotButton := 0;
  FIconHandle := 0;
  FIcon := nil;
  FWidth := SysControl.Width;
  FHeight := SysControl.Height;
  FSysMenuButtonRect := Rect(0, 0, 0, 0);
end;

destructor TscSysDialogStyleHook.Destroy;
begin
   if Application.DialogHandle = Handle then
  begin
    Application.DialogHandle := 0;
    SendMessage(Handle, WM_STYLEDLGDESTROY, 0, 0);
  end;

  if FRegion <> 0 then
    DeleteObject(FRegion);
  if Assigned(FIcon) then
    FreeAndNil(FIcon);
  inherited;
end;

procedure TscSysDialogStyleHook.DrawBorder(Canvas: TCanvas);
begin
  // do nothing
end;

function TscSysDialogStyleHook.GetCaptionRect: TRect;
var
  LDetails: TThemedElementDetails;
  ElementSize: TSize;
  CaptionHeight: Integer;
begin
  Result := Rect(0, 0, FWidth, 0);
  if BorderStyle = bsNone then
    Exit;

  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionInActive);
  end;
  StyleServices.GetElementSize(0, LDetails, esActual, ElementSize);
  CaptionHeight := Round(ElementSize.Height * FScaleFactor);
  Result := Rect(0, 0, FWidth, CaptionHeight);

end;

function TscSysDialogStyleHook.GetCloseButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biSystemMenu in BorderIcons) then
  begin
    if not UseSmallBorder then
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else if FFrameActive then
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
      else if FFrameActive then
        FButtonState := twSmallCloseButtonNormal
      else
        FButtonState := twSmallCloseButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;
end;

function TscSysDialogStyleHook.GetHelpButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biHelp in BorderIcons) and (biSystemMenu in BorderIcons) and ((not(biMaximize in BorderIcons) and not(biMinimize in BorderIcons)) or (BorderStyle = bsDialog)) then
  begin
    if (FPressedButton = HTHELP) and (FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else if FFrameActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;
end;

function TscSysDialogStyleHook.GetHitTest(const P: TPoint): Integer;
begin
  Result := HTCAPTION;
  if FCloseButtonRect.Contains(P) then
    Result := HTCLOSE;
  if FMaxButtonRect.Contains(P) then
    Result := HTMAXBUTTON;
  if FMinButtonRect.Contains(P) then
    Result := HTMINBUTTON;
  if FHelpButtonRect.Contains(P) then
    Result := HTHELP;

  if Result <> HTCAPTION then
  begin
    if FHotButton <> Result then
    begin
      FHotButton := Result;
      InvalidateNC;
    end;
    Exit;
  end
  else
  begin
    if FHotButton <> 0 then
    begin
      FHotButton := 0;
      InvalidateNC;
    end;
  end;
end;

function TscSysDialogStyleHook.GetMaxButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biMaximize in BorderIcons) and (biSystemMenu in BorderIcons) and (BorderStyle <> bsDialog) and (BorderStyle <> bsToolWindow) and (BorderStyle <> bsSizeToolWin) then
  begin
    if WindowState = wsMaximized then
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else if FFrameActive then
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
      else if FFrameActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;
end;

function TscSysDialogStyleHook.GetMinButtonRect: TRect;
var
  FButtonState: TThemedWindow;
  LDetails: TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);
  if (biMinimize in BorderIcons) and (biSystemMenu in BorderIcons) and (BorderStyle <> bsDialog) and (BorderStyle <> bsToolWindow) and (BorderStyle <> bsSizeToolWin) then
  begin
    if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else if FFrameActive then
      FButtonState := twMinButtonNormal
    else
      FButtonState := twMinButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, CaptionRect, Result) then
      Result := Rect(0, 0, 0, 0);
  end;

end;

function TscSysDialogStyleHook.GetWindowState: TWindowState;
begin
  Result := wsNormal;
  if IsZoomed(Handle) then
    Result := wsMaximized;
  if IsIconic(Handle) then
    Result := wsMinimized;
end;

procedure TscSysDialogStyleHook.Help;
begin
  PostMessage(Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0)
end;

function TscSysDialogStyleHook.IsSysCloseButtonDisabled: Boolean;
var
  i, ID: Integer;
begin
  Result := True;
  if SysMenu > 0 then
  begin
    for i := 0 to GetMenuItemCount(SysMenu) - 1 do
    begin
      ID := GetMenuItemID(SysMenu, i);
      if ID = SC_CLOSE then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure TscSysDialogStyleHook.Maximize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;

    if IsZoomed(Handle) then
      PostMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      PostMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  end;
end;

procedure TscSysDialogStyleHook.Minimize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;
    if IsIconic(Handle) then
      PostMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      PostMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  end;
end;

procedure TscSysDialogStyleHook.Paint(Canvas: TCanvas);
begin
  inherited;
  PaintBackground(Canvas);
end;

procedure TscSysDialogStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited;
end;

function TscSysDialogStyleHook.GetBorderIcons: TBorderIcons;
begin
  Result := [];
  with SysControl do
  begin
    if (Style and WS_SYSMENU = WS_SYSMENU) then
      Include(Result, biSystemMenu);
    if (Style and WS_MAXIMIZEBOX = WS_MAXIMIZEBOX) then
      Include(Result, biMaximize);
    if (Style and WS_MINIMIZEBOX = WS_MINIMIZEBOX) then
      Include(Result, biMinimize);
    if (ExStyle and WS_EX_CONTEXTHELP = WS_EX_CONTEXTHELP) and (not(biMaximize in Result)) and (not(biMinimize in Result)) then
      Include(Result, biHelp);
  end;
end;

function TscSysDialogStyleHook.GetBorderSize: TRect;
var
  Size: TSize;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  {
    Result.Left = Left border width
    Result.Top = Caption height
    Result.Right = Right border width
    Result.Bottom = Bottom border height
  }
  Result := Rect(0, 0, 0, 0);
  if BorderStyle = bsNone then
    Exit;

  if not StyleServices.Available then
    Exit;
  { Caption height }
  if not UseSmallBorder then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  FStyledCaptionHeight := Size.cy;
  Size.cy := Round(Size.cy * FScaleFactor);
  Result.Top := Size.cy;
  { Left border width }
  if not UseSmallBorder then
    Detail := twFrameLeftActive
  else
    Detail := twSmallFrameLeftActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Left := Size.cx;
  { Right border width }
  if not UseSmallBorder then
    Detail := twFrameRightActive
  else
    Detail := twSmallFrameRightActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Right := Size.cx;
  { Bottom border height }
  if not UseSmallBorder then
    Detail := twFrameBottomActive
  else
    Detail := twSmallFrameBottomActive;
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Bottom := Size.cy;
end;

function TscSysDialogStyleHook.GetBorderStyle: TFormBorderStyle;
begin
  Result := bsNone;
  if not UpdateRegion then
    Exit(bsNone);
  with SysControl do
  begin
    if (Style and WS_OVERLAPPED = WS_OVERLAPPED) or (Style and WS_OVERLAPPEDWINDOW = WS_OVERLAPPEDWINDOW) or (Style and WS_CAPTION = WS_CAPTION) or
      (ExStyle and WS_EX_OVERLAPPEDWINDOW = WS_EX_OVERLAPPEDWINDOW) and (ExStyle and WS_EX_TOOLWINDOW <> WS_EX_TOOLWINDOW) then
    begin
      if (Style and WS_SIZEBOX <> WS_SIZEBOX) and ((Style and WS_MINIMIZEBOX = WS_MAXIMIZE) or (Style and WS_MINIMIZEBOX = WS_MINIMIZEBOX)) then
        Result := bsSingle;

      if (Style and WS_SIZEBOX <> WS_SIZEBOX) and (Style and WS_MINIMIZEBOX <> WS_MAXIMIZE) and (Style and WS_MINIMIZEBOX <> WS_MINIMIZEBOX) then
        Result := bsDialog;

      if (Style and WS_SIZEBOX = WS_SIZEBOX) then
        Result := bsSizeable;
    end
    else if (ExStyle and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) then
    begin
      if (Style and WS_SIZEBOX = WS_SIZEBOX) then
        Result := bsSizeToolWin
      else
        Result := bsToolWindow;
    end
    else
      Result := bsNone;
  end;
end;

function TscSysDialogStyleHook.UseSmallBorder: Boolean;
begin
  Result := (BorderStyle = bsToolWindow) or (BorderStyle = bsSizeToolWin);
end;

function TscSysDialogStyleHook.GetRegion: HRGN;
var
  R: TRect;
  LDetails: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  Result := 0;
  if not StyleServices.Available then
    Exit;
  { Get Window Region }
  R := Rect(0, 0, FWidth, FHeight);
  if not UseSmallBorder then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;

  DeleteObject(FRegion);
  LDetails := StyleServices.GetElementDetails(Detail);
  if not StyleServices.GetElementRegion(LDetails, R, Result) then
    FRegion := 0;
end;

function TscSysDialogStyleHook.GetSysMenu: HMENU;
begin
  Result := GetSystemMenu(Handle, False);
end;

function TscSysDialogStyleHook.GetSysMenuButtonRect: TRect;
var
  LBorderIcons: TBorderIcons;
  LBorderStyle: TBorderStyle;
  IconDetails: TThemedElementDetails;
  ButtonRect, R, R2: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  LBorderStyle := BorderStyle;
  LBorderIcons := BorderIcons;
  if (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    R2 := ButtonRect;
    R2.Right := R2.Left + Round(ButtonRect.Width * FScaleFactor);
    R2.Bottom := R2.Top + Round(ButtonRect.Height * FScaleFactor);
    ButtonRect := R2;
    R := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R, ButtonRect);
    Result := ButtonRect;
  end;

end;

function TscSysDialogStyleHook.GetUpdateRegion: Boolean;
begin
  with SysControl do
    Result := not((Style and WS_CAPTION <> WS_CAPTION) and (Style and WS_SYSMENU <> WS_SYSMENU) and (Style and WS_SIZEBOX <> WS_SIZEBOX));
end;

function TscSysDialogStyleHook.GetIconFast: TIcon;
begin
  if (FIcon = nil) or (FIconHandle = 0) then
    Result := GetIcon
  else
    Result := FIcon;
end;

function TscSysDialogStyleHook.GetIcon: TIcon;
var
  IconX, IconY: Integer;
  TmpHandle: THandle;
  Info: TWndClassEx;
  Buffer: array [0 .. 255] of Char;
begin
  TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_SMALL, 0));
  if TmpHandle = 0 then
    TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_BIG, 0));

  if TmpHandle = 0 then
    TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_SMALL2, 0));

  if TmpHandle = 0 then
  begin
    { Get instance }
    GetClassName(Handle, @Buffer, sizeof(Buffer));
    FillChar(Info, sizeof(Info), 0);
    Info.cbSize := sizeof(Info);

    if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then
    begin
      TmpHandle := Info.hIconSm;
      if TmpHandle = 0 then
        TmpHandle := Info.HICON;
    end
  end;

  if FIcon = nil then
    FIcon := TIcon.Create;
  if TmpHandle <> 0 then
  begin
    IconX := GetSystemMetrics(SM_CXSMICON);
    if IconX = 0 then
      IconX := GetSystemMetrics(SM_CXSIZE);
    IconY := GetSystemMetrics(SM_CYSMICON);
    if IconY = 0 then
      IconY := GetSystemMetrics(SM_CYSIZE);
    FIcon.Handle := CopyImage(TmpHandle, IMAGE_ICON, IconX, IconY, 0);
    FIconHandle := TmpHandle;
  end;

  Result := FIcon;
end;

function GetMenuItemPos(const Menu: HMENU; const ID: Integer): Integer;
var
  I: Integer;
  MIInfo: MENUITEMINFO;
begin
  Result := -1;
  if Menu = 0 then
    Exit;
  for I := 0 to GetMenuItemCount(Menu) do
  begin
    FillChar(MIInfo, sizeof(MIInfo), Char(0));
    MIInfo.cbSize := sizeof(MIInfo);
    MIInfo.fMask := MIIM_ID;
    if GetMenuItemInfo(Menu, I, True, MIInfo) and
       (MIInfo.wID = Cardinal(ID)) then
      Exit(I);
  end;
end;

function IsItemDisabled(const Menu: HMENU; const Index: Integer): Boolean;
var
  Info: TMenuItemInfo;
begin
  Result := False;
  if (Menu = 0) or (Index < 0) then
    Exit;

  FillChar(Info, sizeof(Info), Char(0));
  Info.cbSize := sizeof(TMenuItemInfo);
  Info.fMask := MIIM_STATE;
  GetMenuItemInfo(Menu, Index, True, Info);
  Result := (Info.fState and MFS_DISABLED = MFS_DISABLED) or (Info.fState and MF_DISABLED = MF_DISABLED) or (Info.fState and MF_GRAYED = MF_GRAYED);
end;

procedure TscSysDialogStyleHook.PaintNC(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  CaptionBmp, Buffer: TBitmap;
  DC: HDC;
  FButtonState: TThemedWindow;
  LCaptionRect, LBorderSize, R, R1, R2: TRect;
  ButtonRect, TextRect: TRect;
  TextTopOffset, IX, IY, Margin: Integer;
  IconDetails: TThemedElementDetails;
  LBorderIcons: TBorderIcons;
  LBorderStyle: TFormBorderStyle;
  CaptionDetails: TThemedElementDetails;
  TextFormat: TTextFormat;
  LText: String;
  nPos: Integer;
  SysMenu: HMENU;
  ItemDisabled: Boolean;
  SaveIndex: Integer;
begin
  LBorderStyle := BorderStyle;
  if (LBorderStyle = bsNone) or (WindowState = wsMinimized) then // (WindowState=wsMinimized) avoid bug in windows 8.1  and increase performance
    Exit;

  LBorderIcons := BorderIcons;
  LCaptionRect := CaptionRect;
  CaptionBmp := TBitmap.Create;
  CaptionBmp.SetSize(LCaptionRect.Width, LCaptionRect.Height);
  DC := CaptionBmp.Canvas.Handle;
  TextTopOffset := 0;
  TextRect := Rect(0, 0, CaptionBmp.Width, CaptionBmp.Height);
  ButtonRect := Rect(0, 0, 0, 0);
  FCaptionRect := Rect(0, 0, 0, 0);
  R := Rect(0, 0, 0, 0);

  { Caption }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twCaptionInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallCaptionInActive);
  end;
  CaptionDetails := LDetails;

  if FScaleFactor > 1 then
  begin
    R1 := Rect(0, 0, CaptionBmp.Width, FStyledCaptionHeight);
    Buffer := TBitmap.Create;
    try
      Buffer.Width := R1.Width;
      Buffer.Height := R1.Height;
      Buffer.Canvas.Brush.Color := GetStyleColor(clBtnShadow);
      Buffer.Canvas.FillRect(R1);
      StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails, R1);
      Margin := R1.Height div 3;
      DrawStrecthBitmap(Margin, Margin, Margin, Margin, Buffer,
        CaptionBmp.Canvas,
        Rect(0, 0, CaptionBmp.Width, CaptionBmp.Height));
    finally
      Buffer.Free;
    end;
  end
  else
    StyleServices.DrawElement(DC, LDetails, LCaptionRect, nil);

  { Draw icon }

  if (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, LCaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if FStyledCaptionHeight > 0 then
    begin
      ButtonRect.Top := 0;
      ButtonRect.Bottom := FStyledCaptionHeight;
    end;
    if FScaleFactor > 1 then
    begin
      R2 := ButtonRect;
      R2.Right := R2.Left + Round(ButtonRect.Width * FScaleFactor);
      R2.Bottom := R2.Top + Round(ButtonRect.Height * FScaleFactor);
      ButtonRect := R2;
    end;

    IX := GetSystemMetrics(SM_CXSMICON);
    IY :=  GetSystemMetrics(SM_CYSMICON);
    R := Rect(0, 0, IX, IY);

    IY := ButtonRect.Top + ButtonRect.Height div 2 - R.Height div 2;
    IX := ButtonRect.Left + ButtonRect.Width div 2 - R.Width div 2;

    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBmp.Canvas.Handle, IX, IY,
        GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);

    Inc(TextRect.Left, ButtonRect.Width + Max(10, Round(5 * FScaleFactor)));

    FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, 10);

  { Draw buttons }
  SysMenu := GetSystemMenu(Handle, False);
  nPos := GetMenuItemPos(SysMenu, SC_CLOSE);
  ItemDisabled := IsItemDisabled(SysMenu, nPos);
  if (biSystemMenu in LBorderIcons) and (not ItemDisabled) then
  begin
    if not UseSmallBorder then
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else if FFrameActive then
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
      else if FFrameActive then
        FButtonState := twSmallCloseButtonNormal
      else
        FButtonState := twSmallCloseButtonDisabled;
    end;
    if FSysCloseButtonDisabled then
    begin
      if UseSmallBorder then
        LDetails := StyleServices.GetElementDetails(twSmallCloseButtonNormal)
      else
        LDetails := StyleServices.GetElementDetails(twCloseButtonNormal);
    end
    else
      LDetails := StyleServices.GetElementDetails(FButtonState);

    ButtonRect := CloseButtonRect;

    if (ButtonRect.Width > 0) then
      if FScaleFactor > 1 then
      begin
        R1 := ButtonRect;
        R1.Left := R1.Right - Round(ButtonRect.Width * FScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * FScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBmp.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails, R2);
          if R1.Left - 1 + Round(Buffer.Width * FScaleFactor) >
            CaptionBmp.Width - 1
          then
            IntersectClipRect(CaptionBmp.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * FScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBmp.Canvas,
            R1.Left - 1, R1.Top - 1, 255, FScaleFactor);
        finally
          RestoreDC(CaptionBmp.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);

    FCloseButtonRect := ButtonRect;

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  if (biMaximize in LBorderIcons) and (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    if WindowState = wsMaximized then
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else if FFrameActive then
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
      else if FFrameActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    ButtonRect := MaxButtonRect;

    if ButtonRect.Width > 0 then
     if FScaleFactor > 1 then
      begin
        R1 := ButtonRect;
        R1.Left := R1.Right - Round(ButtonRect.Width * FScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * FScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBmp.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails, R2);
          if R1.Left - 1 + Round(Buffer.Width * FScaleFactor) >
            CaptionBmp.Width - 1
          then
            IntersectClipRect(CaptionBmp.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * FScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBmp.Canvas,
            R1.Left - 1, R1.Top - 1, 255, FScaleFactor);
        finally
          RestoreDC(CaptionBmp.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);

    FMaxButtonRect := ButtonRect;

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  if (biMinimize in LBorderIcons) and (biSystemMenu in LBorderIcons) and (LBorderStyle <> bsDialog) and (LBorderStyle <> bsToolWindow) and (LBorderStyle <> bsSizeToolWin) then
  begin
    if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else if FFrameActive then
      FButtonState := twMinButtonNormal
    else
      FButtonState := twMinButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);
    ButtonRect := MinButtonRect;

    if ButtonRect.Width > 0 then
      if FScaleFactor > 1 then
      begin
        R1 := ButtonRect;
        R1.Left := R1.Right - Round(ButtonRect.Width * FScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * FScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBmp.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails, R2);
          if R1.Left - 1 + Round(Buffer.Width * FScaleFactor) >
            CaptionBmp.Width - 1
          then
            IntersectClipRect(CaptionBmp.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * FScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBmp.Canvas,
            R1.Left - 1, R1.Top - 1, 255, FScaleFactor);
        finally
          RestoreDC(CaptionBmp.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);

    FMinButtonRect := ButtonRect;

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  if (biHelp in LBorderIcons) and (biSystemMenu in LBorderIcons) and ((not(biMaximize in LBorderIcons) and not(biMinimize in LBorderIcons)) or (LBorderStyle = bsDialog)) then
  begin
    if (FPressedButton = HTHELP) and (FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else if FFrameActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, LCaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    if ButtonRect.Width > 0 then
       if FScaleFactor > 1 then
      begin
        R1 := ButtonRect;
        R1.Left := R1.Right - Round(ButtonRect.Width * FScaleFactor);
        R1.Bottom := R1.Top + Round(ButtonRect.Height * FScaleFactor);
        Buffer := TBitmap.Create;
        SaveIndex := SaveDC(CaptionBmp.Canvas.Handle);
        try
          Buffer.Width := ButtonRect.Width + 2;
          Buffer.Height := ButtonRect.Height + 2;
          Buffer.PixelFormat := pf32bit;
          Bitmap_ClearAlpha(Buffer, 0);
          R2 := Rect(1, 1, Buffer.Width - 1, Buffer.Height - 1);
          Buffer.AlphaFormat := afPremultiplied;
          StyleServices.DrawElement(Buffer.Canvas.Handle, LDetails, R2);
          if R1.Left - 1 + Round(Buffer.Width * FScaleFactor) >
            CaptionBmp.Width - 1
          then
            IntersectClipRect(CaptionBmp.Canvas.Handle,
              R1.Left, R1.Top - 1, R1.Right,
              R1.Top - 1 + Round(Buffer.Height * FScaleFactor));
          Bitmap_DrawScaleAlpha_XY(Buffer, CaptionBmp.Canvas,
            R1.Left - 1, R1.Top - 1, 255, FScaleFactor);
        finally
          RestoreDC(CaptionBmp.Canvas.Handle, SaveIndex);
          Buffer.Free;
        end;
        ButtonRect := R1;
      end
      else
        StyleServices.DrawElement(CaptionBmp.Canvas.Handle, LDetails, ButtonRect);

    FHelpButtonRect := ButtonRect;

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
  end;

  { draw text }
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if SysControl.BidiMode = sbmRightToLeft then
    Include(TextFormat, tfRtlReading);

  LText := SysControl.Text;

  if FScaleFactor > 1 then
  begin
    TextTopOffset := (CaptionBmp.Height - FStyledCaptionHeight) div 2;
    MoveWindowOrg(CaptionBmp.Canvas.Handle, 0, TextTopOffset);
    StyleServices.DrawText(CaptionBmp.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
    MoveWindowOrg(CaptionBmp.Canvas.Handle, 0, -TextTopOffset);
  end
  else
  if (TextTopOffset <> 0) and (biSystemMenu in LBorderIcons) then
  begin
    Inc(TextRect.Left, R.Left);
    MoveWindowOrg(CaptionBmp.Canvas.Handle, 0, TextTopOffset);
    StyleServices.DrawText(CaptionBmp.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
    MoveWindowOrg(CaptionBmp.Canvas.Handle, 0, -TextTopOffset);
  end
  else
    StyleServices.DrawText(CaptionBmp.Canvas.Handle,
      CaptionDetails, LText, TextRect, TextFormat);

  FCaptionRect := TextRect;

  Canvas.Draw(0, 0, CaptionBmp);
  CaptionBmp.Free;

  DC := Canvas.Handle;
  LBorderSize := BorderSize;

  { Left Border }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameLeftActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameLeftActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameLeftInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameLeftInActive);
  end;

  R := Rect(0, LCaptionRect.Height, LBorderSize.Left, FHeight);
  if FWidth > LBorderSize.Left then
    StyleServices.DrawElement(DC, LDetails, R);

  { Right Border }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameRightActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameRightActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameRightInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameRightInActive);
  end;
  R := Rect(FWidth - LBorderSize.Right, LCaptionRect.Height, FWidth, FHeight);
  if FWidth > LBorderSize.Right then
    StyleServices.DrawElement(DC, LDetails, R);

  { Bottom Border }
  if FFrameActive then
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameBottomActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameBottomActive);
  end
  else
  begin
    if not UseSmallBorder then
      LDetails := StyleServices.GetElementDetails(twFrameBottomInActive)
    else
      LDetails := StyleServices.GetElementDetails(twSmallFrameBottomInActive);
  end;
  R := Rect(0, FHeight - LBorderSize.Bottom, FWidth, FHeight);
  StyleServices.DrawElement(DC, LDetails, R);
end;

procedure TscSysDialogStyleHook.Restore;
begin
  FPressedButton := 0;
  FHotButton := 0;
  if Handle <> 0 then
    SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TscSysDialogStyleHook.MouseLeave;
begin
  FPressedButton := 0;
  if FHotButton <> 0 then
  begin
    FHotButton := 0;
    InvalidateNC;
  end;
end;

procedure TscSysDialogStyleHook.WMNCACTIVATE(var Message: TWMNCActivate);
begin
  Handled := False;
  if not StyleServicesEnabled then
    Exit;

  if not OverridePaintNC then
    Exit;

  FFrameActive := Message.Active;
  InvalidateNC;
  Message.Result := 1;
  Handled := True;
end;

procedure TscSysDialogStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;
  if (BorderStyle = bsNone) or (not UpdateRegion) then
    Exit;
  inherited;
end;

function TscSysDialogStyleHook.NormalizePoint(const P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
begin
  { Convert the point from the screen to the client window . }
  WindowPos := Point(SysControl.Left, SysControl.Top);
  ClientPos := Point(0, 0);
  ClientToScreen(Handle, ClientPos);
  Result := P;
  ScreenToClient(Handle, Result);
  Inc(Result.X, ClientPos.X - WindowPos.X);
  Inc(Result.Y, ClientPos.Y - WindowPos.Y);
end;

procedure TscSysDialogStyleHook.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;

  if OverridePaintNC then
  begin
    P := Point(Message.XPos, Message.YPos);
    P := NormalizePoint(P);
    Message.Result := GetHitTest(P);
    if ((Message.Result <> HTCLOSE) and (Message.Result <> HTMAXBUTTON) and (Message.Result <> HTMINBUTTON) and (Message.Result <> HTHELP)) then
    begin
      // Message.Result := CallDefaultProc(TMessage(Message));
      { Check if form can be scrolled . }
      inherited;
      { We need to correct the result after calling the default message . }
      if ((Message.Result = HTCLOSE) or (Message.Result = HTMAXBUTTON) or (Message.Result = HTMINBUTTON) or (Message.Result = HTHELP)) then
        Message.Result := HTCLIENT;
    end;
    Handled := True;
  end;
end;

procedure TscSysDialogStyleHook.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;

  if (Message.HitTest = HTCLOSE) or (Message.HitTest = HTMAXBUTTON) or
     (Message.HitTest = HTMINBUTTON) or (Message.HitTest = HTHELP) then
  begin
    FPressedButton := Message.HitTest;
    InvalidateNC;
    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end;
end;

procedure TscSysDialogStyleHook.WMNCLButtonUp(var Message: TWMNCLButtonUp);
var
  FWasPressedButton: Integer;
begin
  FWasPressedButton := FPressedButton;

  if FPressedButton <> 0 then
  begin
    FPressedButton := 0;
    InvalidateNC;
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
    begin
      Close
    end
    else if (Message.HitTest = HTMAXBUTTON) then
    begin
      if WindowState <> wsMaximized then
        Maximize
      else
        Restore;
    end
    else if (Message.HitTest = HTMINBUTTON)  then
    begin
      if WindowState <> wsMinimized then
        Minimize
      else
        Restore;
    end
    else if (Message.HitTest = HTHELP) then
      Help;

  Message.Result := 0;
  Message.Msg := WM_NULL;
  Handled := True;
end;

procedure TscSysDialogStyleHook.WMNCMouseMove(var Message: TWMNCHitMessage);
begin
  { Reserved for potential updates . }
  if not MouseInControl then
  begin
    MouseInControl := True;
    StartHotTrackTimer;
    MouseEnter;
  end;
  Handled := False;
end;


function IsWindowMsgBox(Handle: HWND): Boolean;
begin
  Result := ((FindWindowEx(Handle, 0, 'Edit', nil) = 0) and (GetDlgItem(Handle, $FFFF) <> 0)) and (GetWindowLongPtr(Handle, GWL_USERDATA) <> 0);
end;

procedure TscSysDialogStyleHook.WMPaint(var Message: TMessage);
begin
  if IsWindowMsgBox(Handle) and OverridePaint then
  begin
    inherited;
    Exit;
  end;
  Message.Result := CallDefaultProc(Message);
  Handled := True;
end;

procedure TscSysDialogStyleHook.WMSetText(var Message: TMessage);
var
  FRedraw: Boolean;
  LBorderStyle : TFormBorderStyle;
begin
  LBorderStyle := BorderStyle;
  if (LBorderStyle = bsNone) or (WindowState = wsMinimized) or (StyleServices.IsSystemStyle) then
  begin
    Handled := False;
    Exit;
  end;

  FRedraw := True;

  if  IsWindowVisible(Handle) then
  begin
    //Application.ProcessMessages;
    FRedraw := False;
    SetRedraw(False);
  end;

  CallDefaultProc(Message);

  if not FRedraw then
  begin
    SetRedraw(True);
    InvalidateNC;
  end;
  Handled := True;
end;


procedure TscSysDialogStyleHook.WMSIZE(var Message: TWMSize);
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;

  Message.Result := CallDefaultProc(TMessage(Message));

  FWidth := SysControl.Width;
  FHeight := SysControl.Height;

  Handled := True;
end;

procedure TscSysDialogStyleHook.WndProc(var Message: TMessage);
var
  DFBW: Integer;
  BorderSize: TRect;
  Changed: Boolean;
  MinW, MinH: Integer;
begin
  case Message.msg of
    WM_WINDOWPOSCHANGED:
      begin
        FSysCloseButtonDisabled := IsSysCloseButtonDisabled;
        if TWMWindowPosChanged(Message).WindowPos^.flags and SWP_NOSIZE = 0 then
        begin
          FWidth :=  TWMWindowPosChanged(Message).WindowPos^.cx;
          FHeight := TWMWindowPosChanged(Message).WindowPos^.cy;
        end;
      end;

    WM_CREATE:
      begin
        Message.Result := CallDefaultProc(Message);
        DFBW := GetSystemMetrics(SM_CXBORDER);
        MinW := GetSystemMetrics(SM_CXMIN);
        MinH := GetSystemMetrics(SM_CYMIN);
        BorderSize := GetBorderSize;
        FWidth := SysControl.Width;
        FHeight := SysControl.Height;
        if (FWidth >= MinW) and (FHeight >= MinH) then
          SetWindowPos(Handle, 0, 0, 0, FWidth + DFBW + 5, FHeight + DFBW + Round(10 * FScaleFactor),
            SWP_NOMOVE or SWP_NOZORDER or SWP_FRAMECHANGED or SWP_NOOWNERZORDER or SWP_NOACTIVATE);
        Exit;
      end;

    WM_SHOWWINDOW:
      if (Message.WParam > 0) and (FRegion = 0) and
         (SysControl.Width > 0) and (SysControl.Height > 0) then
      begin
        FWidth := SysControl.Width;
        FHeight := SysControl.Height;
        FRegion := GetRegion;
        if (FRegion <> 0) and (BorderStyle <> bsNone) and UpdateRegion then
          SetWindowRgn(Handle, FRegion, True);
      end;

  end;
  inherited;
  case Message.Msg of
    WM_WINDOWPOSCHANGING:
    begin
      Changed := False;
      if (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0) then
      begin
        Changed := ((TWMWindowPosChanging(Message).WindowPos^.cx <> FWidth) or
                    (TWMWindowPosChanging(Message).WindowPos^.cy <> FHeight)) and
                   (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0);

        FWidth :=  TWMWindowPosChanging(Message).WindowPos^.cx;
        FHeight := TWMWindowPosChanging(Message).WindowPos^.cy;
      end;

      if Changed then
      begin;
        FRegion := GetRegion;
        if (FRegion <> 0) and (BorderStyle <> bsNone) and UpdateRegion then
          SetWindowRgn(Handle, FRegion, True);
        if BorderStyle <> bsNone then
          InvalidateNC;
      end;
    end;
  end;
end;

procedure TscSysTooltipsStyleHook.PaintHint(Canvas: TCanvas; TextRect: TRect);
var
  DC: HDC;
  LDetails: TThemedElementDetails;
  BkColor, GradientStartColor, GradientEndColor, TextColor, LColor: TColor;
  Brush: HBRUSH;
  AText: PChar;
begin
  Canvas.Font := SysControl.Font;
  DC := Canvas.Handle;
  BkColor := $00767676;
  GradientStartColor := clWhite;
  GradientEndColor := $EFE4E3;
  TextColor := $00575757;
  if StyleServices.Enabled then
  begin
    LDetails := StyleServices.GetElementDetails(thHintBalloon);
    if StyleServices.GetElementColor(LDetails, ecBorderColor, LColor) and (LColor <> clNone) then
      BkColor := LColor;
    if StyleServices.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
      GradientStartColor := LColor;
    if StyleServices.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
      GradientEndColor := LColor;
    if StyleServices.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
      TextColor := LColor;
  end;
  { Draw Tooltips Face }
  GradientFillCanvas(Canvas, GradientStartColor,
    GradientEndColor, SysControl.ClientRect, gdVertical);
  { Draw Tooltips Border }
  Brush := CreateSolidBrush(BkColor);
  FrameRect(DC, SysControl.ClientRect, Brush);
  DeleteObject(Brush);
  { Draw Tooltips Text }
  SetBkMode(DC, TRANSPARENT);
  SetTextColor(DC, TextColor);
  AText := PChar(SysControl.Text);
  scDrawUtils.DrawWordWrapText(DC, AText, TextRect);
end;

procedure TscSysTooltipsStyleHook.WMPaint(var Message: TMessage);
const
  TTS_BALLOON = $40;
begin
  CallDefaultProc(Message);
  if not ((GetWindowLong(Handle, GWL_STYLE) and TTS_BALLOON) = TTS_BALLOON) then
    inherited;
end;

procedure TscSysTooltipsStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

constructor TscSysTooltipsStyleHook.Create(AHandle: THandle);
begin
  inherited;
  StyleElements := [seClient];
end;

procedure TscSysTooltipsStyleHook.Paint(Canvas: TCanvas);
Var
  TextRect: TRect;
begin
  TextRect := SysControl.ClientRect;
  SendMessage(Handle, TTM_ADJUSTRECT, 0, UINT_PTR(@TextRect));
  PaintHint(Canvas, TextRect);
end;
{$ENDIF}

{$IFNDEF VER270_UP}

function GetSysWindowClassName(Window: HWND): string;
var
  sClassName: array[0..256] of Char;
begin
  SetString(Result, sClassName, Winapi.Windows.GetClassName(Window, sClassName, Length(sClassName)));
end;

function GetSysWindowText(Window: HWND): string;
var
  Text: array[0..256] of Char;
begin
  SetString(Result, Text, Winapi.Windows.GetWindowText(Window, Text, Length(Text)));
end;

constructor TscSysControl.Create(AHandle: THandle);
begin
  FFont := nil;
  FParent := nil;
  Handle := AHandle;
end;

destructor TscSysControl.Destroy;
begin
  if Assigned(FParent) then
    FreeAndNil(FParent);
  if FFont <> nil then
    FFont.Free;
  inherited;
end;

function TscSysControl.DrawTextBiDiModeFlags(Flags: Integer): Longint;
begin
  Result := Flags;
  { do not change center alignment }
  if UseRightToLeftAlignment then
    if Result and DT_RIGHT = DT_RIGHT then
      Result := Result and not DT_RIGHT { removing DT_RIGHT, makes it DT_LEFT }
    else if not(Result and DT_CENTER = DT_CENTER) then
      Result := Result or DT_RIGHT;
  Result := Result or DrawTextBiDiModeFlagsReadingOnly;
end;

function TscSysControl.DrawTextBiDiModeFlagsReadingOnly: Longint;
begin
  if UseRightToLeftReading then
    Result := DT_RTLREADING
  else
    Result := 0;
end;

function TscSysControl.Focused: Boolean;
begin
  Result := (Handle <> 0) and (GetFocus = Handle);
end;

function TscSysControl.GetBidiMode: TscSysBidiModeDirection;
begin
  Result := scsbmLeftToRight;
  if Style <> 0 then
    if (ExStyle and WS_EX_RIGHT = WS_EX_RIGHT) or
      (ExStyle and WS_EX_RTLREADING = WS_EX_RTLREADING) or
      (ExStyle and WS_EX_LAYOUTRTL = WS_EX_LAYOUTRTL) then
      Result := scsbmRightToLeft;
end;

function TscSysControl.GetBorder: Boolean;
begin
  Result := (Style and WS_BORDER = WS_BORDER) or
    (ExStyle and WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE);
end;

function TscSysControl.GetBoundsRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TscSysControl.GetClientEdge: Boolean;
begin
  Result := ExStyle and WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE;
end;

function TscSysControl.GetClientHeight: Integer;
begin
  Result := ClientRect.Bottom;
end;

function TscSysControl.GetClientWidth: Integer;
begin
  Result := ClientRect.Right;
end;

function TscSysControl.GetClientRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  Winapi.Windows.GetClientRect(Handle, Result);
end;

function TscSysControl.GetControlClassName: String;
begin
  Result := GetSysWindowClassName(Handle);
end;

function TscSysControl.GetEnabled: Boolean;
begin
  Result := False;
  if Handle > 0 then
    Result := IsWindowEnabled(Handle);
end;

function TscSysControl.GetHeight: Integer;
begin
  Result := WindowRect.Height;
end;

function TscSysControl.GetLeft: Integer;
begin
  Result := WindowRect.Left;
end;

function TscSysControl.GetParent: TscSysControl;
begin
  Result := nil;
  if Assigned(FParent) then
    FreeAndNil(FParent);
  if ParentHandle <> 0 then
    begin
      FParent := TscSysControl.Create(ParentHandle);
      Result := FParent;
    end;
end;

function TscSysControl.GetParentHandle: THandle;
begin
  Result := Winapi.Windows.GetParent(Handle);
end;

function TscSysControl.GetStyle: NativeInt;
begin
  Result := GetWindowLongPtr(Handle, GWL_STYLE);
end;

function TscSysControl.GetExStyle: NativeInt;
begin
  Result := GetWindowLongPtr(Handle, GWL_EXSTYLE);
end;

function TscSysControl.GetFont: TFont;
var
  LogFont: TLogFont;
  hFont: HGDIOBJ;
begin
  if FFont <> nil then
    Exit(FFont);

  hFont := HGDIOBJ(SendMessage(Handle, WM_GETFONT, 0, 0));
  Result := TFont.Create;
  FillChar(LogFont, SizeOf(LogFont), 0);
  GetObject(hFont, SizeOf(LogFont), @LogFont);
  Result.Name := StrPas(LogFont.lffaceName);
  Result.Height := LogFont.lfHeight;
  if LogFont.lfWeight >= FW_MEDIUM then
    Result.Style := Result.Style + [fsBold];
  if LogFont.lfItalic <> 0 then
    Result.Style := Result.Style + [fsItalic];
  if LogFont.lfUnderline <> 0 then
    Result.Style := Result.Style + [fsUnderline];
  if LogFont.lfStrikeout <> 0 then
    Result.Style := Result.Style + [fsStrikeout];
  case (LogFont.lfPitchAndFamily and 3) of
    VARIABLE_PITCH:
      Result.Pitch := fpVariable;
    FIXED_PITCH:
      Result.Pitch := fpFixed;
  end;

  FFont := Result;
end;

function TscSysControl.GetText: String;
begin
  Result := GetSysWindowText(Handle);
end;

function TscSysControl.GetTop: Integer;
begin
  Result := WindowRect.Top;
end;

function TscSysControl.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

function TscSysControl.GetWidth: Integer;
begin
  Result := WindowRect.Width;
end;

function TscSysControl.GetWinRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  GetWindowRect(Handle, Result);
end;

function TscSysControl.GetWndProc: NativeInt;
begin
  Result := GetWindowLongPtr(Handle, GWL_WNDPROC);
end;

procedure TscSysControl.SetExStyle(const Value: NativeInt);
begin
  SetWindowLongPtr(Handle, GWL_EXSTYLE, Value);
end;

procedure TscSysControl.SetStyle(const Value: NativeInt);
begin
  SetWindowLongPtr(Handle, GWL_STYLE, Value);
end;

procedure TscSysControl.SetWndProc(Value: NativeInt);
begin
  if Value <> WndProc then
    begin
      SetWindowLongPtr(Handle, GWL_WNDPROC, Value);
    end;
end;

function TscSysControl.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and
    (BidiMode = TscSysBidiModeDirection.scsbmRightToLeft);
end;

function TscSysControl.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and
    (BidiMode <> TscSysBidiModeDirection.scsbmLeftToRight);
end;

function IsControlHooked(Handle: HWND): Boolean;
begin
  Result := False;
  if Handle > 0 then
    Result := (SendMessage(Handle, CM_CONTROLHOOKED, 0, 0) = $77);
end;

{ TscSysStyleHook }

constructor TscSysStyleHook.Create(AHandle: THandle);
begin
  FHandled := False;
  FSysControl := nil;
  FHandle := AHandle;
  FOrgWndProc := 0;
  FProcInstance := nil;
  FBrush := nil;
  FFont := TFont.Create;
  StyleElements := [];
  FParentColor := False;
  FDoubleBuffered := False;
  FPaintOnEraseBkgnd := False;
  OverridePaint := False;
  OverridePaintNC := False;
  OverrideEraseBkgnd := False;
  OverrideFont := False;
  if AHandle > 0 then
    begin
      FProcInstance := MakeObjectInstance(WndProc);
      FSysControl := TscSysControl.Create(AHandle);
      FOrgWndProc := FSysControl.WndProc;
      if FOrgWndProc > 0 then
        begin
          FSysControl.WndProc := LONG_PTR(FProcInstance);
          FBrush := TBrush.Create;
          UpdateColors;
        end;
    end;
end;

destructor TscSysStyleHook.Destroy;
begin
  if Assigned(FProcInstance) then
    FreeObjectInstance(FProcInstance);

  if Assigned(FSysControl) then
    begin
      FSysControl.WndProc := FOrgWndProc;
      FreeAndNil(FSysControl);
    end;

  if Assigned(FBrush) then
    FreeAndNil(FBrush);

  if Assigned(FFont) then
    FreeAndNil(FFont);

  inherited;
end;

function TscSysStyleHook.CallDefaultProc(var Msg: TMessage): LRESULT;
begin
  Result := CallWindowProc(Pointer(FOrgWndProc), Handle, Msg.Msg, Msg.wParam,
    Msg.lParam);
end;

procedure TscSysStyleHook.DrawBorder(Canvas: TCanvas);
var
  BorderSize: TRect;
begin
  BorderSize := GetBorderSize;
  with BorderSize do
    if (Left > 0) and (Right > 0) and (Top > 0) and (Bottom > 0) then
      PaintBorder(SysControl, True);
end;

procedure TscSysStyleHook.DrawControlText(Canvas: TCanvas;
  Details: TThemedElementDetails; const S: string; var R: TRect;
  Flags: Cardinal);
var
  ThemeTextColor: TColor;
  TextFormat: TTextFormatFlags;
begin
  Canvas.Font := SysControl.Font;
  TextFormat := TTextFormatFlags(Flags);
  if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
    begin
      Canvas.Font.Color := ThemeTextColor;
      StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat,
        Canvas.Font.Color);
    end
  else
    begin
      Canvas.Refresh;
      StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
    end;
end;

procedure TscSysStyleHook.DrawParentBackground(DC: HDC; ARect: PRect);
var
  Bmp: TBitmap;
  P: TPoint;
begin
  P := Point(0, 0);
  if ARect <> nil then
    P := Point(ARect.Left, ARect.Top);

  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(SysControl.Parent.Width, SysControl.Parent.Height);
    SendMessage(ParentHandle, WM_ERASEBKGND, Bmp.Canvas.Handle, $93);
    ClientToScreen(Handle, P);
    ScreenToClient(ParentHandle, P);
    if ARect <> nil then
      BitBlt(DC, ARect.Left, ARect.Top, ARect.Width, ARect.Height,
        Bmp.Canvas.Handle, P.X, P.Y, SRCCOPY)
    else
      BitBlt(DC, 0, 0, SysControl.Width, SysControl.Height, Bmp.Canvas.Handle,
        P.X, P.Y, SRCCOPY);
  finally
    Bmp.Free;
  end;

end;

function TscSysStyleHook.DrawText(DC: HDC; Details: TThemedElementDetails;
  S: String; var R: TRect; const Flags: TTextFormat): Integer;
var
  DrawFlags: Cardinal;
  SaveIndex: Integer;
  sColor: TColor;
begin
  SaveIndex := SaveDC(DC);
  try
    SetBkMode(DC, TRANSPARENT);
    if not StyleServices.GetElementColor(Details, ecTextColor, sColor) then
      sColor := FontColor;
    if not OverrideFont then
      sColor := FontColor;
    SetTextColor(DC, sColor);
    DrawFlags := TTextFormatFlags(Flags);
    Result := Winapi.Windows.DrawText(DC, S, -1, R, DrawFlags);
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

function TscSysStyleHook.DrawTextCentered(DC: HDC; Details: TThemedElementDetails;
  const R: TRect; S: String; Const Flags: DWORD = 0): Integer;
var
  DrawRect: TRect;
  DrawFlags: Cardinal;
  DrawParams: TDrawTextParams;
  SaveIndex: Integer;
  sColor: TColor;
begin
  SaveIndex := SaveDC(DC);
  try
    SetBkMode(DC, TRANSPARENT);
    if not StyleServices.GetElementColor(Details, ecTextColor, sColor) then
      sColor := FontColor;
    if not OverrideFont then
      sColor := FontColor;
    SetTextColor(DC, sColor);
    DrawRect := R;
    DrawFlags := DT_END_ELLIPSIS or DT_WORDBREAK or DT_EDITCONTROL or DT_CENTER;
    if DrawFlags <> 0 then
      DrawFlags := DrawFlags or Flags;

    Winapi.Windows.DrawText(DC, PChar(S), -1, DrawRect,
      DrawFlags or DT_CALCRECT);
    DrawRect.Right := R.Right;
    if DrawRect.Bottom < R.Bottom then
      OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
    else
      DrawRect.Bottom := R.Bottom;
    ZeroMemory(@DrawParams, SizeOf(DrawParams));
    DrawParams.cbSize := SizeOf(DrawParams);
    DrawTextEx(DC, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
    Result := DrawParams.uiLengthDrawn;
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

function TscSysStyleHook.GetFocused: Boolean;
begin
  Result := (GetFocus = Handle);
end;

function TscSysStyleHook.GetBorderSize: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TscSysStyleHook.GetColor: TColor;
begin
  // if OverrideEraseBkgnd then
  // Result := StyleServices.GetStyleColor(scWindow)
  // else
  Result := FColor;
end;

function TscSysStyleHook.GetFontColor: TColor;
begin
  // if OverrideFont then
  // Result := StyleServices.GetSystemColor(clWindowText)
  // else
  Result := FFontColor;
end;

function TscSysStyleHook.GetParentHandle: HWND;
begin
  Result := GetParent(Handle);
end;

function TscSysStyleHook.GetText: string;
var
  Buffer: array [0 .. 255] of Char;
begin
  if (Handle <> 0) then
    SetString(Result, Buffer, Winapi.Windows.GetWindowText(Handle, Buffer,
      Length(Buffer)));
  FText := Result;
end;

function TscSysStyleHook.InternalPaint(DC: HDC): Boolean;
begin
  Result := False;
end;

procedure TscSysStyleHook.SetColor(Value: TColor);
begin
  if (Value <> FColor) or ((FBrush <> nil) and (Value <> FBrush.Color)) then
    begin
      FColor := Value;
      if Assigned(FBrush) then
        FBrush.Color := Value;
    end;
end;

procedure TscSysStyleHook.SetFont(const Value: TFont);
begin
  if Value <> FFont then
    FFont.Assign(Value);
end;

procedure TscSysStyleHook.SetOverridePaint(Value: Boolean);
begin
  if Value then
    OverrideEraseBkgnd := Value;
  FOverridePaint := Value;
end;

procedure TscSysStyleHook.SetRedraw(AHandle: HWND; Value: Boolean);
begin
  SendMessage(AHandle, WM_SETREDRAW, wParam(Value), 0);
end;

procedure TscSysStyleHook.SetRedraw(Value: Boolean);
begin
  SetRedraw(Handle, Value);
end;

procedure TscSysStyleHook.SetStyleElements(Value: TStyleElements);
begin
  if Value <> FStyleElements then
    begin
      FStyleElements := Value;
      OverridePaint := (seClient in FStyleElements);
      OverridePaintNC := (seBorder in FStyleElements);
      OverrideFont := (seFont in FStyleElements);
    end;
end;

function TscSysStyleHook.StyleServicesEnabled: Boolean;
begin
  Result := (StyleServices.Available) and not(StyleServices.IsSystemStyle) and
    (FindControl(Handle) = nil);
end;

procedure TscSysStyleHook.UpdateColors;
begin
  if (OverrideEraseBkgnd) or (OverridePaint) then
    Color := StyleServices.GetStyleColor(scWindow)
  else
    Color := clBtnFace;
  if OverrideFont then
    FontColor := StyleServices.GetSystemColor(clWindowText)
  else
    FontColor := clBlack;
end;

function TscSysStyleHook.UseLeftScrollBar: Boolean;
begin
  Result := (SysControl.ExStyle and WS_EX_LEFTSCROLLBAR = WS_EX_LEFTSCROLLBAR)
end;

procedure TscSysStyleHook.Invalidate;
begin
  if FOverridePaintNC then
    InvalidateNC;
  InvalidateRect(Handle, nil, False);
end;

procedure TscSysStyleHook.InvalidateNC;
begin
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TscSysStyleHook.Paint(Canvas: TCanvas);
begin

end;

procedure TscSysStyleHook.PaintBackground(Canvas: TCanvas);
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(SysControl.ClientRect);
end;

procedure TscSysStyleHook.PaintBorder(Control: TscSysControl;
  EraseLRCorner: Boolean);
var
  EmptyRect, DrawRect: TRect;
  DC: HDC;
  H, W: Integer;
  AStyle: Integer;
  Details: TThemedElementDetails;
  BorderSize: TRect;
begin
  BorderSize := GetBorderSize;
  with Control do
    begin
      ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
      if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
        begin
          GetWindowRect(Handle, DrawRect);
          OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
          DC := GetWindowDC(Handle);
          try
            EmptyRect := DrawRect;
            if EraseLRCorner then
              begin
                AStyle := GetWindowLong(Handle, GWL_STYLE);
                if ((AStyle and WS_HSCROLL) <> 0) and
                  ((AStyle and WS_VSCROLL) <> 0) then
                  begin
                    W := GetSystemMetrics(SM_CXVSCROLL);
                    H := GetSystemMetrics(SM_CYHSCROLL);
                    InflateRect(EmptyRect, -2, -2);
                    with EmptyRect do
                      if not UseLeftScrollBar then
                        EmptyRect := Rect(Left, Bottom - H, Left + W, Bottom)
                      else
                        EmptyRect := Rect(Right - W, Bottom - H, Right, Bottom);
                    FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_BTNFACE));
                  end;
              end;
            with DrawRect do
              ExcludeClipRect(DC, Left + BorderSize.Left, Top + BorderSize.Top,
                Right - BorderSize.Right, Bottom - BorderSize.Bottom);
            Details := StyleServices.GetElementDetails(teEditTextNormal);
            StyleServices.DrawElement(DC, Details, DrawRect);
          finally
            ReleaseDC(Handle, DC);
          end;
        end;
    end;
end;

procedure TscSysStyleHook.PaintNC(Canvas: TCanvas);
begin

end;

procedure TscSysStyleHook.DrawParentBackground(DC: HDC);
begin
  DrawParentBackground(DC, nil);
end;

procedure TscSysStyleHook.Refresh;
begin
  SendMessage(Handle, WM_PAINT, 0, 0);
end;

procedure TscSysStyleHook.WMEraseBkgnd(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  SaveIndex: Integer;
begin
  Handled := False;

  if not StyleServicesEnabled then
    Exit;

  UpdateColors;

  if FOverrideEraseBkgnd then
    begin
      if not FDoubleBuffered then
        begin
          DC := HDC(Message.wParam);

          SaveIndex := 0;
          if DC = 0 then
            DC := GetDC(Handle)
          else
            SaveIndex := SaveDC(DC);

          Canvas := TCanvas.Create;
          try
            Canvas.Handle := DC;
            if Assigned(FFont) then
              Canvas.Font.Assign(FFont);

            if (FParentColor) and (ParentHandle > 0) then
              DrawParentBackground(Canvas.Handle)
            else
              PaintBackground(Canvas);

            if (FPaintOnEraseBkgnd) and (Message.lParam <> $93) then
              Paint(Canvas);
          finally
            Canvas.Handle := 0;
            Canvas.Free;
            if Message.wParam = 0 then
              ReleaseDC(Handle, DC)
            else if SaveIndex <> 0 then
              RestoreDC(DC, SaveIndex);
          end;
        end;
      Handled := True;
      Message.Result := 1;
    end;
end;

function TscSysStyleHook.CheckIfParentBkGndPainted: Boolean;
var
  Test: Integer;
  PTest: PInteger;
  ParentHandle: HWND;
begin
  Test := $93;
  PTest := @Test;
  Result := False;
  ParentHandle := GetParent(Handle);
  if ParentHandle > 0 then
    begin
      SendMessage(ParentHandle, WM_ERASEBKGND, 0, lParam(PTest));
      Result := (PTest^ = $11);
    end;
end;

function TscSysStyleHook.CheckIfParentHooked: Boolean;
begin
  Result := (SendMessage(ParentHandle, CM_PARENTHOOKED, 0, 0) = $77);
end;

procedure TscSysStyleHook.WMNCPaint(var Message: TMessage);
var
  Canvas: TCanvas;
begin
  Handled := False;
  if not StyleServicesEnabled then
    Exit;

  if FOverridePaintNC then
    begin
      Canvas := TCanvas.Create;
      try
        Canvas.Handle := GetWindowDC(SysControl.Handle);
        if Assigned(FFont) then
          Canvas.Font.Assign(FFont);
        DrawBorder(Canvas);
        PaintNC(Canvas);
      finally
        ReleaseDC(Handle, Canvas.Handle);
        Canvas.Handle := 0;
        Canvas.Free;
      end;
      Handled := True;
    end;
end;

procedure TscSysStyleHook.WMPaint(var Message: TMessage);
var
  DC: HDC;
  Buffer: TBitmap;
  Canvas: TCanvas;
  PS: TPaintStruct;
begin
  Handled := False;
  if not StyleServicesEnabled then
    Exit;

  if OverridePaint then
    begin
      DC := HDC(Message.wParam);
      Canvas := TCanvas.Create;
      try
        BeginPaint(SysControl.Handle, PS);
        if DC <> 0 then
          Canvas.Handle := DC
        else
          begin
            DC := GetDC(Handle);
            Canvas.Handle := DC;
          end;
        if Assigned(FFont) then
          Canvas.Font.Assign(FFont);

        if not InternalPaint(Canvas.Handle) then
          if FDoubleBuffered and (DC = 0) then
            begin
              Buffer := TBitmap.Create;
              try
                Buffer.SetSize(SysControl.Width, SysControl.Height);
                PaintBackground(Buffer.Canvas);
                Paint(Buffer.Canvas);
                Canvas.Draw(0, 0, Buffer);
              finally
                Buffer.Free;
              end;
            end
          else
            Paint(Canvas);

        if DC = 0 then
        begin
          ReleaseDC(SysControl.Handle, DC)
        end;
      finally
        Canvas.Handle := 0;
        Canvas.Free;
        EndPaint(SysControl.Handle, PS);
      end;
      Handled := True;
    end;

end;

procedure TscSysStyleHook.WndProc(var Message: TMessage);
var
  TempResult: LRESULT;
  ChildHandle: HWND;
begin
  case Message.Msg of
    CM_INITCHILDS:
      begin
        Message.Result := 0;
        with TscSysStyleManager do
        begin
          for ChildHandle in FChildRegSysStylesList.Keys do
            if (not IsControlHooked(ChildHandle)) and
              (FChildRegSysStylesList[ChildHandle].Parent = Handle) then
              begin
                if not FSysStyleHookList.ContainsKey(ChildHandle) then
                  begin
                     FSysStyleHookList.Add(ChildHandle,
                       FChildRegSysStylesList[ChildHandle].StyleHookClass.Create(ChildHandle));
                     RedrawWindow(ChildHandle, nil, 0,
                       RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or
                       RDW_INVALIDATE);
                     SetWindowPos(ChildHandle, 0, 0, 0, 0, 0,
                       SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
                       SWP_FRAMECHANGED);
                    Message.Result := 1;
                  end;
              end;
          end;
        Exit;
      end;

    WM_CHANGEUISTATE, WM_PARENTNOTIFY:
      begin
        SendMessage(Handle, CM_INITCHILDS, 0, 0);
      end;

    WM_ERASEBKGND:
      begin
        if (Message.lParam > 0) and (Message.wParam = 0) and
          (FOverrideEraseBkgnd or FOverridePaint or FPaintOnEraseBkgnd) then
          if PInteger(Message.lParam)^ = $93 then
            begin
              PInteger(Message.lParam)^ := $11;
              Exit;
            end;
      end;

    CM_PARENTHOOKED, CM_CONTROLHOOKED:
      begin
        Message.Result := $77;
        Exit;
      end;

    WM_SETREDRAW:
      begin
        Message.Result := CallDefaultProc(Message);
        Dispatch(Message);
        Exit;
      end;

    WM_CTLCOLORMSGBOX .. WM_CTLCOLORSTATIC:
      begin
        if not StyleServicesEnabled then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        TempResult := SendMessage(Handle, CM_BASE + Message.Msg, Message.wParam,
          Message.lParam);
        Message.Result := SendMessage(Message.lParam, CM_BASE + Message.Msg,
          Message.wParam, Message.lParam);
        if Message.Result = 0 then
          Message.Result := TempResult;
        Exit;
      end;

    CM_CTLCOLORMSGBOX .. CM_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.wParam, ColorToRGB(FontColor));
        SetBkColor(Message.wParam, ColorToRGB(FBrush.Color));
        Message.Result := LRESULT(FBrush.Handle);
        Exit;
      end;

  end;

  Dispatch(Message);
  if not Handled then
    Message.Result := CallDefaultProc(Message)
  else
    Handled := False;
end;

class constructor TscSysStyleManager.Create;
begin
  FEnabled := True;
  FMenuHookEnabled := True;
  FSysStyleHookList := TscSysStyleHookList.Create;
  FRegSysStylesList := TscRegSysStylesList.Create;
  FChildRegSysStylesList := TscChildRegSysStylesList.Create;
  InstallHook;
end;

class destructor TscSysStyleManager.Destroy;
begin
  RemoveHook;
  FRegSysStylesList.Free;
  FSysStyleHookList.Free;
  FChildRegSysStylesList.Free;
  inherited;
end;

constructor TscSysStyleManager.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TscSysStyleManager.Destroy;
begin
  inherited;
end;

class function TscSysStyleManager.HookCBProc(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT;
var
  CBTSturct: TCBTCreateWnd;
  sClassName: string;
  Parent: HWND;
  Style, ParentStyle: NativeInt;

  procedure AddChildControl(Handle: HWND);
  var
    Info: TscChildControlInfo;
  begin
    ZeroMemory(@Info, SizeOf(TscChildControlInfo));
    Info.Parent := Parent;
    Info.ParentStyle := ParentStyle;
    Info.StyleHookClass := FRegSysStylesList[sClassName];
    if FChildRegSysStylesList.ContainsKey(Handle) then
      FChildRegSysStylesList.Remove(Handle);
    FChildRegSysStylesList.Add(Handle, Info);
  end;

  procedure AddControl(Handle: HWND);
  begin
    if FSysStyleHookList.ContainsKey(Handle) then
      FSysStyleHookList.Remove(Handle);
    FSysStyleHookList.Add(Handle, FRegSysStylesList[sClassName].Create(Handle));
  end;

begin
  Result := CallNextHookEx(FHook, nCode, wParam, lParam);

  if not Enabled or not TStyleManager.IsCustomStyleActive then Exit;

  if (nCode = HCBT_CREATEWND) and not (StyleServices.IsSystemStyle) then
  begin
    CBTSturct := PCBTCreateWnd(lParam)^;
    sClassName := GetSysWindowClassName(wParam);
    sClassName := LowerCase(sClassName);
    Parent := CBTSturct.lpcs.hwndParent;
    Style := CBTSturct.lpcs.Style;
    ParentStyle := 0;
    if FRegSysStylesList.ContainsKey(sClassName) and CheckSysClassName(sClassName) then
    begin
      if Parent > 0 then
        ParentStyle := GetWindowLongPtr(Parent, GWL_STYLE);
      if (Style and DS_CONTROL = DS_CONTROL) then
      begin
        AddControl(wParam);
        PostMessage(wParam, CM_INITCHILDS, 0, 0);
      end
      else
        if (Style and WS_POPUP = WS_POPUP) then
        begin
          AddControl(wParam);
        end
        else
        if (Style and WS_CHILD = WS_CHILD) then
        begin
          if FSysStyleHookList.ContainsKey(Parent) then
              AddChildControl(wParam)
          else
            AddControl(wParam);
        end
         else
           AddControl(wParam);
    end;
  end;

  if nCode = HCBT_DESTROYWND then
  begin
    if FSysStyleHookList.ContainsKey(wParam) then
    begin
      FSysStyleHookList[wParam].Free;
      FSysStyleHookList.Remove(wParam);
    end;
  end;

end;

class function TscSysStyleManager.CheckSysClassName(const AClassName: String): Boolean;
begin
  Result := True;
  if not FMenuHookEnabled and (AClassName = '#32768') then
    Result := False;
end;

class procedure TscSysStyleManager.InstallHook;
begin
  FHook := SetWindowsHookEx(WH_CBT, @HookCBProc, 0, GetCurrentThreadId);
end;

class procedure TscSysStyleManager.RegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TscSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
  FRegSysStylesList.Add(LowerCase(SysControlClass), SysStyleHookClass);
end;

class procedure TscSysStyleManager.RemoveHook;
begin
  if FHook <> 0 then
    UnhookWindowsHookEx(FHook);
end;

class procedure TscSysStyleManager.UnRegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TscSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
end;

{$ENDIF}

{$IFNDEF VER270_UP}
var
  hDWMDLL:HModule;

procedure SetupDWM;
begin
  hDWMDLL := LoadLibrary('dwmapi.dll');
  if hDWMDLL <> 0 then
  begin
    @SC_DwmIsCompositionEnabled := GetProcAddress(hDWMDLL, 'DwmIsCompositionEnabled');
  end;
end;
{$ENDIF}

initialization

  {$IFDEF VER270_UP}
  TCustomStyleEngine.UnRegisterSysStyleHook('#32768', TSysPopupStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('#32768', TscSysPopupStyleHook);
  {$ELSE}
  TscSysStyleManager.RegisterSysStyleHook('#32768', TscSysPopupStyleHook);
  SetupDWM;
  {$ENDIF}
  {$IFDEF VER300_UP}
  TCustomStyleEngine.UnRegisterSysStyleHook('#32770', TSysDialogStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('#32770', TscSysDialogStyleHook);
  TCustomStyleEngine.UnRegisterSysStyleHook('Button', TSysButtonStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('Button', TscSysButtonStyleHook);
  TCustomStyleEngine.UnRegisterSysStyleHook('tooltips_class32', TSysTooltipsStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('tooltips_class32', TscSysTooltipsStyleHook);
  {$ENDIF}

finalization

  {$IFDEF VER270_UP}
  TCustomStyleEngine.UnRegisterSysStyleHook('#32768', TscSysPopupStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('#32768', TSysPopupStyleHook);
  {$ELSE}
  TscSysStyleManager.UnRegisterSysStyleHook('#32768', TscSysPopupStyleHook);

  if hDWMDLL <> 0 then FreeLibrary(hDWMDLL);
  {$ENDIF}

  {$IFDEF VER300_UP}
  TCustomStyleEngine.UnRegisterSysStyleHook('#32770', TscSysDialogStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('#32770', TSysDialogStyleHook);
  TCustomStyleEngine.UnRegisterSysStyleHook('Button', TscSysButtonStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('Button', TSysButtonStyleHook);
  TCustomStyleEngine.UnRegisterSysStyleHook('tooltips_class32', TscSysTooltipsStyleHook);
  TCustomStyleEngine.RegisterSysStyleHook('tooltips_class32', TSysTooltipsStyleHook);
  {$ENDIF}

end.