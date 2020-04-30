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

unit scGrids;

{$R-,T-,H+,X+}

{$IFDEF CPUX64}
  {$DEFINE PUREPASCAL}
{$ENDIF CPUX64}
{$I scdefine.inc}

interface

uses
  Winapi.Messages, Winapi.Windows, System.SysUtils, System.Classes, System.Variants,
  System.Types, Vcl.Graphics, Vcl.Menus, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Mask,
  System.UITypes, scControls, scDrawUtils, scImageCollection;

const
  SC_MaxGridSize = MaxInt div 16;
  SC_MaxShortInt = High(ShortInt);
  SC_MaxCustomExtents = SC_MaxGridSize;

type
  scEInvalidGridOperation = class(Exception);

  TscGetExtentsFunc = function(Index: Longint): Integer of object;

  TscGridAxisDrawInfo = record
    EffectiveLineWidth: Integer;
    FixedBoundary: Integer;
    GridBoundary: Integer;
    GridExtent: Integer;
    LastFullVisibleCell: Longint;
    FullVisBoundary: Integer;
    FixedCellCount: Integer;
    FirstGridCell: Integer;
    GridCellCount: Integer;
    GetExtent: TscGetExtentsFunc;
  end;

  TscGridDrawInfo = record
    Horz, Vert: TscGridAxisDrawInfo;
  end;

  TscGridState = (scgsNormal, scgsSelecting, scgsRowSizing, scgsColSizing,
    scgsRowMoving, scgsColMoving);
  TscGridMovement = scgsRowMoving..scgsColMoving;

  TscCustomGrid = class;

  TscInplaceEdit = class(TscCustomEdit)
  private
    FGrid: TscCustomGrid;
    FClickTime: Longint;
    procedure InternalMove(const Loc: TRect; Redraw: Boolean);
    procedure SetGrid(Value: TscCustomGrid);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
  protected
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure BoundsChanged; virtual;
    procedure UpdateContents; virtual;
    procedure WndProc(var Message: TMessage); override;
    property  Grid: TscCustomGrid read FGrid;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Deselect;
    procedure Hide;
    procedure Invalidate; reintroduce;
    procedure Move(const Loc: TRect);
    function PosEqual(const Rect: TRect): Boolean;
    procedure SetFocus; reintroduce;
    procedure UpdateLoc(const Loc: TRect);
    function Visible: Boolean;
    procedure InvalidateGrid;
  end;

  { TscCustomGrid }

  TscGridOption = (scgoFixedVertLine, scgoFixedHorzLine, scgoVertLine, scgoHorzLine,
    scgoRangeSelect, scgoDrawFocusSelected, scgoRowSizing, scgoColSizing, scgoRowMoving,
    scgoColMoving, scgoEditing, scgoTabs, scgoRowSelect, scgoAlwaysShowEditor,
    scgoThumbTracking, scgoFixedColClick, scgoFixedRowClick, scgoFixedHotTrack);
  TscGridOptions = set of TscGridOption;
  TscGridDrawState = set of (scgdSelected, scgdFocused, scgdFixed, scgdRowSelected,
    scgdHotTrack, scgdPressed);
  TscGridScrollDirection = set of (scsdLeft, scsdRight, scsdUp, scsdDown);

  TscGridCoord = record
    X: Longint;
    Y: Longint;
  end;

  TscHotTrackCellInfo = record
    Coord: TscGridCoord;
    Pressed: Boolean;
    Button: TMouseButton;
  end;

  TscGridRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Longint);
      1: (TopLeft, BottomRight: TscGridCoord);
    end;

  TscGridEditStyle =  (esSimple, esEllipsis, esPickList);
  TscGetCellParamEvent = procedure (Sender: TObject; ACol, ARow: Longint; AState: TscGridDrawState;
    var ABGColor: TColor; var AFillBG: Boolean; var AFontColor: TColor) of object;

  TscSelectCellEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var CanSelect: Boolean) of object;
  TscDrawCellEvent = procedure (Sender: TObject; ACanvas: TCanvas; ACol, ARow: Longint;
    Rect: TRect; State: TscGridDrawState) of object;
  TscFixedCellClickEvent = procedure (Sender: TObject; ACol, ARow: Longint) of object;

  TscGridDrawingStyle = (scgdsClassic, scgdsThemed, scgdsGradient);

  TscGridBackgroundStyle = (scgbColor, scbgbFormBackground, scgbTransparent);

  TscCustomGrid = class(TscCustomControl)
  private
    FAnchor: TscGridCoord;
    FBorderStyle: TBorderStyle;
    FAlternateRow: Boolean;
    FCanEditModify: Boolean;
    FColCount: Longint;
    FCurrent: TscGridCoord;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FDrawingStyle: TscGridDrawingStyle;
    FFixedCols: Integer;
    FFixedRows: Integer;
    FFixedColor: TColor;
    FGradientEndColor: TColor;
    FGradientStartColor: TColor;
    FGridLineWidth: Integer;
    FOptions: TscGridOptions;
    FPanPoint: TPoint;
    FRowCount: Longint;
    FScrollBars: {$IFNDEF VER230}System.UITypes.{$ENDIF}TScrollStyle;
    FTopLeft: TscGridCoord;
    FSizingIndex: Longint;
    FSizingPos, FSizingOfs: Integer;
    FMoveIndex, FMovePos: Longint;
    FHitTest: TPoint;
    FInplaceEdit: TscInplaceEdit;
    FColOffset: Integer;
    FDefaultDrawing: Boolean;
    FEditorMode: Boolean;
    FColWidths: Pointer;
    FRowHeights: Pointer;
    FTabStops: Pointer;
    FOnFixedCellClick: TscFixedCellClickEvent;
    FWallpapers: TscCustomImageCollection;
    FCustomImages: TscCustomImageCollection;
    FCustomBackgroundImageIndex: Integer;
    FCustomOverContentImageIndex: Integer;
    FWallpaperIndex: Integer;
    FBackgroundStyle: TscGridBackgroundStyle;
    FTransparentEditor: Boolean;
    FShowFocusRect: Boolean;
    FLinesColor: TColor;
    FSelectionStyle: TscSelectionStyle;
    procedure SetAlternateRow(Value: Boolean);
    procedure SetSelectionStyle(Value: TscSelectionStyle);
    procedure SetCustomOverContentImageIndex(Value: Integer);
    procedure SetCustomBackgroundImageIndex(Value: Integer);
    procedure SetCustomImages(Value: TscCustomImageCollection);
    procedure SetLinesColor(Value: TColor);
    procedure SetBackgroundStyle(Value: TscGridBackgroundStyle);
    procedure SetWallpaperIndex(Value: Integer);
    procedure SetWallpapers(Value: TscCustomImageCollection);
    function CalcCoordFromPoint(X, Y: Integer;
      const DrawInfo: TscGridDrawInfo): TscGridCoord;
    procedure CalcDrawInfoXY(var DrawInfo: TscGridDrawInfo;
      UseWidth, UseHeight: Integer);
    function CalcMaxTopLeft(const Coord: TscGridCoord;
      const DrawInfo: TscGridDrawInfo): TscGridCoord;
    procedure CancelMode;
    procedure ChangeSize(NewColCount, NewRowCount: Longint);
    procedure ClampInView(const Coord: TscGridCoord);
    procedure DrawSizingLine(const DrawInfo: TscGridDrawInfo);
    procedure DrawMove;
    procedure GridRectToScreenRect(GridRect: TscGridRect;
      var ScreenRect: TRect; IncludeLine: Boolean);
    procedure Initialize;
    procedure InvalidateRect(ARect: TscGridRect);
    procedure ModifyScrollBar(ScrollBar, ScrollCode, Pos: Cardinal);
    procedure MoveAdjust(var CellPos: Longint; FromIndex, ToIndex: Longint);
    procedure MoveAnchor(const NewAnchor: TscGridCoord);
    procedure MoveAndScroll(Mouse, CellHit: Integer; var DrawInfo: TscGridDrawInfo;
      var Axis: TscGridAxisDrawInfo; Scrollbar: Integer; const MousePt: TPoint);
    procedure MoveCurrent(ACol, ARow: Longint; MoveAnchor, Show: Boolean);
    procedure MoveTopLeft(ALeft, ATop: Longint);
    procedure ResizeCol(Index: Longint; OldSize, NewSize: Integer);
    procedure ResizeRow(Index: Longint; OldSize, NewSize: Integer);
    procedure ScrollDataInfo(DX, DY: Integer; var DrawInfo: TscGridDrawInfo);
    procedure TopLeftMoved(const OldTopLeft: TscGridCoord);
    procedure UpdateScrollPos;
    procedure UpdateScrollRange;
    function GetColWidths(Index: Longint): Integer;
    function GetRowHeights(Index: Longint): Integer;
    function GetSelection: TscGridRect;
    function GetTabStops(Index: Longint): Boolean;
    function GetVisibleColCount: Integer;
    function GetVisibleRowCount: Integer;
    function IsActiveControl: Boolean;
    function IsGradientEndColorStored: Boolean;
    procedure ReadColWidths(Reader: TReader);
    procedure ReadRowHeights(Reader: TReader);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCol(Value: Longint);
    procedure SetColCount(Value: Longint);
    procedure SetColWidths(Index: Longint; Value: Integer);
    procedure SetDefaultColWidth(Value: Integer);
    procedure SetDefaultRowHeight(Value: Integer);
    procedure SetDrawingStyle(const Value: TscGridDrawingStyle);
    procedure SetEditorMode(Value: Boolean);
    procedure SetFixedColor(Value: TColor);
    procedure SetFixedCols(Value: Integer);
    procedure SetFixedRows(Value: Integer);
    procedure SetGradientEndColor(Value: TColor);
    procedure SetGradientStartColor(Value: TColor);
    procedure SetGridLineWidth(Value: Integer);
    procedure SetLeftCol(Value: Longint);
    procedure SetOptions(Value: TscGridOptions);
    procedure SetRow(Value: Longint);
    procedure SetRowCount(Value: Longint);
    procedure SetRowHeights(Index: Longint; Value: Integer);
    procedure SetScrollBars(Value: {$IFNDEF VER230}System.UITypes.{$ENDIF}TScrollStyle);
    procedure SetSelection(Value: TscGridRect);
    procedure SetTabStops(Index: Longint; Value: Boolean);
    procedure SetTopRow(Value: Longint);
    procedure UpdateEdit;
    procedure UpdateText;
    procedure WriteColWidths(Writer: TWriter);
    procedure WriteRowHeights(Writer: TWriter);
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCancelMode(var Msg: TWMCancelMode); message WM_CANCELMODE;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    FFullRePaint: Boolean;
    FGridState: TscGridState;
    FSaveCellExtents: Boolean;
    DesignOptionsBoost: TscGridOptions;
    VirtualView: Boolean;
    FInternalColor: TColor;
    FInternalDrawingStyle: TscGridDrawingStyle;
    FHotTrackCell: TscHotTrackCellInfo;
    FInplaceCol, FInplaceRow: Longint;
    FStopUpdateColums: Boolean;
    FTextColor: TColor;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanObserve(const ID: Integer): Boolean; override;
    procedure ObserverAdded(const ID: Integer; const Observer: IObserver); override;
    procedure ObserverToggle(const AObserver: IObserver; const Value: Boolean);
    procedure ChangeGridOrientation(ACanvas: TCanvas; ARightToLeftOrientation: Boolean);
    function ObserverCurrent: TVarRec;
    procedure CalcDrawInfo(var DrawInfo: TscGridDrawInfo);
    procedure CalcFixedInfo(var DrawInfo: TscGridDrawInfo);
    procedure CalcSizingState(X, Y: Integer; var State: TscGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TscGridDrawInfo); virtual;
    function CreateEditor: TscInplaceEdit; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure AdjustSize(Index, Amount: Longint; Rows: Boolean); reintroduce; dynamic;
    function BoxRect(ALeft, ATop, ARight, ABottom: Longint): TRect;
    procedure DoExit; override;
    function CellRect(ACol, ARow: Longint): TRect;
    function CanEditAcceptKey(Key: Char): Boolean; dynamic;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; dynamic;
    function CanEditModify: Boolean; dynamic;
    function CanEditShow: Boolean; virtual;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure FixedCellClick(ACol, ARow: Longint); dynamic;
    procedure FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
    function GetEditText(ACol, ARow: Longint): string; dynamic;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); dynamic;
    function GetEditLimit: Integer; dynamic;
    function GetEditMask(ACol, ARow: Longint): string; dynamic;
    function GetEditStyle(ACol, ARow: Longint): TscGridEditStyle; dynamic;
    function GetGridWidth: Integer;
    function GetGridHeight: Integer;
    procedure HideEdit;
    procedure HideEditor;
    procedure ShowEditor;
    procedure ShowEditorChar(Ch: Char);
    procedure InvalidateEditor;
    procedure InvalidateGrid; inline;
    procedure MoveColumn(FromIndex, ToIndex: Longint);
    procedure ColumnMoved(FromIndex, ToIndex: Longint); dynamic;
    procedure MoveRow(FromIndex, ToIndex: Longint);
    procedure RowMoved(FromIndex, ToIndex: Longint); dynamic;
    procedure SelectionMoved(const OldSel: TscGridRect); virtual;
    procedure DrawCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect;
      AState: TscGridDrawState); virtual; abstract;
    procedure DrawCellBackground(ACanvas: TCanvas; const ARect: TRect; AColor: TColor;
      AState: TscGridDrawState; ACol, ARow: Integer); virtual;
    procedure DrawCellHighlight(ACanvas: TCanvas; const ARect: TRect;
      AState: TscGridDrawState; ACol, ARow: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure MoveColRow(ACol, ARow: Longint; MoveAnchor, Show: Boolean);
    function SelectCell(ACol, ARow: Longint): Boolean; virtual;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); dynamic;
    function Sizing(X, Y: Integer): Boolean;
    procedure ScrollData(DX, DY: Integer);
    procedure InvalidateCell(ACol, ARow: Longint);
    procedure InvalidateCol(ACol: Longint);
    procedure InvalidateRow(ARow: Longint);
    function IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; override;
    procedure TopLeftChanged; dynamic;
    procedure TimedScroll(Direction: TscGridScrollDirection); dynamic;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure ColWidthsChanged; dynamic;
    procedure RowHeightsChanged; dynamic;
    procedure DeleteColumn(ACol: Longint); virtual;
    procedure DeleteRow(ARow: Longint); virtual;
    procedure UpdateDesigner;
    function BeginColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function BeginRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function CheckRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function EndColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    function EndRowDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; dynamic;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Col: Longint read FCurrent.X write SetCol;
    property Color default clWindow;
    property ColCount: Longint read FColCount write SetColCount default 5;
    property ColWidths[Index: Longint]: Integer read GetColWidths write SetColWidths;
    property DefaultColWidth: Integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default True;
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default 24;
    property DrawingStyle: TscGridDrawingStyle read FDrawingStyle write SetDrawingStyle default scgdsThemed;
    property EditorMode: Boolean read FEditorMode write SetEditorMode;
    property FixedColor: TColor read FFixedColor write SetFixedColor default clBtnFace;
    property FixedCols: Integer read FFixedCols write SetFixedCols default 1;
    property FixedRows: Integer read FFixedRows write SetFixedRows default 1;
    property GradientEndColor: TColor read FGradientEndColor
      write SetGradientEndColor stored IsGradientEndColorStored;
    property GradientStartColor: TColor read FGradientStartColor
      write SetGradientStartColor default clWhite;
    property GridHeight: Integer read GetGridHeight;
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default 1;
    property GridWidth: Integer read GetGridWidth;
    property HitTest: TPoint read FHitTest;
    property InplaceEditor: TscInplaceEdit read FInplaceEdit;
    property LeftCol: Longint read FTopLeft.X write SetLeftCol;
    property Options: TscGridOptions read FOptions write SetOptions
      default [scgoFixedVertLine, scgoFixedHorzLine, scgoVertLine, scgoHorzLine,
      scgoRangeSelect];
    property ParentColor default False;
    property Row: Longint read FCurrent.Y write SetRow;
    property RowCount: Longint read FRowCount write SetRowCount default 5;
    property RowHeights[Index: Longint]: Integer read GetRowHeights write SetRowHeights;
    property ScrollBars: {$IFNDEF VER230}System.UITypes.{$ENDIF}TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Selection: TscGridRect read GetSelection write SetSelection;
    property TabStops[Index: Longint]: Boolean read GetTabStops write SetTabStops;
    property TopRow: Longint read FTopLeft.Y write SetTopRow;
    property VisibleColCount: Integer read GetVisibleColCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property OnFixedCellClick: TscFixedCellClickEvent read FOnFixedCellClick write FOnFixedCellClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseCoord(X, Y: Integer): TscGridCoord;
  published
    property TabStop default True;
    property AlternateRow: Boolean
      read FAlternateRow write SetAlternateRow;
    property SelectionStyle: TscSelectionStyle read FSelectionStyle write SetSelectionStyle;
    property TransparentEditor: Boolean
      read FTransparentEditor write FTransparentEditor;
    property Wallpapers: TscCustomImageCollection read FWallpapers write SetWallpapers;
    property WallpaperIndex: Integer read FWallpaperIndex write SetWallpaperIndex;
    property BackgroundStyle: TscGridBackgroundStyle read FBackgroundStyle write SetBackgroundStyle;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
    property LinesColor: TColor read FLinesColor write SetLinesColor;
    property CustomImages: TscCustomImageCollection read FCustomImages write SetCustomImages;
    property CustomBackgroundImageIndex: Integer
      read FCustomBackgroundImageIndex write SetCustomBackgroundImageIndex;
    property CustomOverContentImageIndex: Integer
      read FCustomOverContentImageIndex write SetCustomOverContentImageIndex;
  end;

  { TscCustomDrawGrid }

  TscGetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; var Value: string) of object;
  TscSetEditEvent = procedure (Sender: TObject; ACol, ARow: Longint; const Value: string) of object;
  TscMovedEvent = procedure (Sender: TObject; FromIndex, ToIndex: Longint) of object;

  TscCustomDrawGrid = class(TscCustomGrid)
  private
    FOnColumnMoved: TscMovedEvent;
    FOnDrawCell: TscDrawCellEvent;
    FOnGetEditMask: TscGetEditEvent;
    FOnGetEditText: TscGetEditEvent;
    FOnRowMoved: TscMovedEvent;
    FOnSelectCell: TscSelectCellEvent;
    FOnSetEditText: TscSetEditEvent;
    FOnTopLeftChanged: TNotifyEvent;
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect;
      AState: TscGridDrawState); override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure TopLeftChanged; override;
    property OnColumnMoved: TscMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnDrawCell: TscDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnGetEditMask: TscGetEditEvent read FOnGetEditMask write FOnGetEditMask;
    property OnGetEditText: TscGetEditEvent read FOnGetEditText write FOnGetEditText;
    property OnRowMoved: TscMovedEvent read FOnRowMoved write FOnRowMoved;
    property OnSelectCell: TscSelectCellEvent read FOnSelectCell write FOnSelectCell;
    property OnSetEditText: TscSetEditEvent read FOnSetEditText write FOnSetEditText;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
  public
    function CellRect(ACol, ARow: Longint): TRect;
    function SelectionRect: TRect;
    function SelectionScreenRect: TRect;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    property Canvas;
    property Col;
    property ColWidths;
    property DrawingStyle;
    property EditorMode;
    property GridHeight;
    property GridWidth;
    property LeftCol;
    property Selection;
    property Row;
    property RowHeights;
    property TabStops;
    property TopRow;
  end;

  { TscDrawGrid }

  TscDrawGrid = class(TscCustomDrawGrid)
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property ColCount;
    property Constraints;
    property Ctl3D;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawingStyle;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property RowCount;
    property FixedRows;
    property Font;
    property GradientEndColor;
    property GradientStartColor;
    property GridLineWidth;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Touch;
    property Visible;
    property StyleElements;
    property VisibleColCount;
    property VisibleRowCount;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFixedCellClick;
    property OnGesture;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;

  { TscStringGrid }

  TscStringGrid = class;

  TscStringGridStrings = class(TStrings)
  private
    FGrid: TscStringGrid;
    FIndex: Integer;
    procedure CalcXY(Index: Integer; var X, Y: Integer);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AGrid: TscStringGrid; AIndex: Longint);
    function Add(const S: string): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  TscStringGrid = class(TscDrawGrid)
  private
    FUpdating: Boolean;
    FNeedsUpdating: Boolean;
    FEditUpdate: Integer;
    FData: TCustomData;
    FRows: TCustomData;
    FCols: TCustomData;
    FOnGetCellParam: TscGetCellParamEvent;
    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    procedure Initialize;
    procedure Update(ACol, ARow: Integer); reintroduce;
    procedure SetUpdateState(Updating: Boolean);
    function GetCells(ACol, ARow: Integer): string;
    function GetCols(Index: Integer): TStrings;
    function GetObjects(ACol, ARow: Integer): TObject;
    function GetRows(Index: Integer): TStrings;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    procedure SetCols(Index: Integer; Value: TStrings);
    procedure SetObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetRows(Index: Integer; Value: TStrings);
    function EnsureColRow(Index: Integer; IsCol: Boolean): TscStringGridStrings;
    function EnsureDataRow(ARow: Integer): TCustomData;
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect;
      AState: TscGridDrawState); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CellRect(ACol, ARow: Longint): TRect;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;
  published
    property OnGetCellParam: TscGetCellParamEvent
      read FOnGetCellParam write FOnGetCellParam;
  end;

  TscOnGetPickListItems = procedure(ACol, ARow: Integer; Items: TStrings) of Object;

  TscInplaceEditList = class(TscInplaceEdit)
  private
    FPickList: TscCustomListbox;
    FActiveList: TWinControl;
    FEditStyle: TscGridEditStyle;
    FDropDownRows: Integer;
    FListVisible: Boolean;
    FPressed: Boolean;
    FPickListLoaded: Boolean;
    FOnGetPickListitems: TscOnGetPickListItems;
    FOnEditButtonClick: TNotifyEvent;
    function GetPickList: TscCustomListbox;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CancelMode;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFocus;
  protected
    procedure OnButtonClick(Sender: TObject);
    procedure CloseUp(Accept: Boolean); dynamic;
    procedure DblClick; override;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditButtonClick; virtual;
    procedure DoGetPickListItems; dynamic;
    procedure DropDown; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure RestoreContents;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows;
    property EditStyle: TscGridEditStyle read FEditStyle;
    property ListVisible: Boolean read FListVisible write FListVisible;
    property PickList: TscCustomListbox read GetPickList;
    property PickListLoaded: Boolean read FPickListLoaded write FPickListLoaded;
    property Pressed: Boolean read FPressed;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write FOnEditButtonClick;
    property OnGetPickListitems: TscOnGetPickListItems read FOnGetPickListitems
      write FOnGetPickListitems;
  end;

implementation

uses
  System.Math, Vcl.Themes, System.RTLConsts, Vcl.Consts, Vcl.GraphUtil;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..SC_MaxCustomExtents] of Integer;

procedure InvalidOp(const id: string);
begin
  raise scEInvalidGridOperation.Create(id);
end;

function GridRect(Coord1, Coord2: TscGridCoord): TscGridRect;
begin
  with Result do
  begin
    Left := Coord2.X;
    if Coord1.X < Coord2.X then Left := Coord1.X;
    Right := Coord1.X;
    if Coord1.X < Coord2.X then Right := Coord2.X;
    Top := Coord2.Y;
    if Coord1.Y < Coord2.Y then Top := Coord1.Y;
    Bottom := Coord1.Y;
    if Coord1.Y < Coord2.Y then Bottom := Coord2.Y;
  end;
end;

function PointInGridRect(Col, Row: Longint; const Rect: TscGridRect): Boolean;
begin
  Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top)
    and (Row <= Rect.Bottom);
end;

type
  TXorRects = array[0..3] of TRect;

procedure XorRects(const R1, R2: TRect; var XorRects: TXorRects);
var
  Intersect, Union: TRect;

  function PtInRect(X, Y: Integer; const Rect: TRect): Boolean;
  begin
    with Rect do Result := (X >= Left) and (X <= Right) and (Y >= Top) and
      (Y <= Bottom);
  end;


  function Includes(const P1: TPoint; var P2: TPoint): Boolean;
  begin
    with P1 do
    begin
      Result := PtInRect(X, Y, R1) or PtInRect(X, Y, R2);
      if Result then P2 := P1;
    end;
  end;

  function Build(var R: TRect; const P1, P2, P3: TPoint): Boolean;
  begin
    Build := True;
    with R do
      if Includes(P1, TopLeft) then
      begin
        if not Includes(P3, BottomRight) then BottomRight := P2;
      end
      else if Includes(P2, TopLeft) then BottomRight := P3
      else Build := False;
  end;

begin
  FillChar(XorRects, SizeOf(XorRects), 0);
  if not IntersectRect(Intersect, R1, R2) then
  begin
    XorRects[0] := R1;
    XorRects[1] := R2;
  end
  else
  begin
    UnionRect(Union, R1, R2);
    if Build(XorRects[0],
      Point(Union.Left, Union.Top),
      Point(Union.Left, Intersect.Top),
      Point(Union.Left, Intersect.Bottom)) then
      XorRects[0].Right := Intersect.Left;
    if Build(XorRects[1],
      Point(Intersect.Left, Union.Top),
      Point(Intersect.Right, Union.Top),
      Point(Union.Right, Union.Top)) then
      XorRects[1].Bottom := Intersect.Top;
    if Build(XorRects[2],
      Point(Union.Right, Intersect.Top),
      Point(Union.Right, Intersect.Bottom),
      Point(Union.Right, Union.Bottom)) then
      XorRects[2].Left := Intersect.Right;
    if Build(XorRects[3],
      Point(Union.Left, Union.Bottom),
      Point(Intersect.Left, Union.Bottom),
      Point(Intersect.Right, Union.Bottom)) then
      XorRects[3].Top := Intersect.Bottom;
  end;
end;

procedure ModifyExtents(var Extents: Pointer; Index, Amount: Longint;
  Default: Integer);
var
  LongSize, OldSize: LongInt;
  NewSize: Integer;
  I: Integer;
begin
  if Amount <> 0 then
  begin
    if not Assigned(Extents) then OldSize := 0
    else OldSize := PIntArray(Extents)^[0];
    if (Index < 0) or (OldSize < Index) then InvalidOp(SIndexOutOfRange);
    LongSize := OldSize + Amount;
    if LongSize < 0 then InvalidOp(STooManyDeleted)
    else if LongSize >= SC_MaxGridSize - 1 then InvalidOp(SGridTooLarge);
    NewSize := Cardinal(LongSize);
    if NewSize > 0 then Inc(NewSize);
    ReallocMem(Extents, NewSize * SizeOf(Integer));
    if Assigned(Extents) then
    begin
      I := Index + 1;
      while I < NewSize do
      begin
        PIntArray(Extents)^[I] := Default;
        Inc(I);
      end;
      PIntArray(Extents)^[0] := NewSize-1;
    end;
  end;
end;


procedure UpdateExtents(var Extents: Pointer; NewSize: Longint;
  Default: Integer);
var
  OldSize: Integer;
begin
  OldSize := 0;
  if Assigned(Extents) then OldSize := PIntArray(Extents)^[0];
  ModifyExtents(Extents, OldSize, NewSize - OldSize, Default);
end;

procedure MoveExtent(var Extents: Pointer; FromIndex, ToIndex: Longint);
var
  Extent: Integer;
begin
  if Assigned(Extents) then
  begin
    Extent := PIntArray(Extents)^[FromIndex];
    if FromIndex < ToIndex then
      Move(PIntArray(Extents)^[FromIndex + 1], PIntArray(Extents)^[FromIndex],
        (ToIndex - FromIndex) * SizeOf(Integer))
    else if FromIndex > ToIndex then
      Move(PIntArray(Extents)^[ToIndex], PIntArray(Extents)^[ToIndex + 1],
        (FromIndex - ToIndex) * SizeOf(Integer));
    PIntArray(Extents)^[ToIndex] := Extent;
  end;
end;


function CompareExtents(E1, E2: Pointer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if E1 <> nil then
  begin
    if E2 <> nil then
    begin
      for I := 0 to PIntArray(E1)^[0] do
        if PIntArray(E1)^[I] <> PIntArray(E2)^[I] then Exit;
      Result := True;
    end
  end
  else Result := E2 = nil;
end;

function LongMulDiv(Mult1, Mult2, Div1: Longint): Longint; stdcall;
  external 'kernel32.dll' name 'MulDiv';

procedure KillMessage(Wnd: HWnd; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

constructor TscInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInheritedKeys := True;
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
end;

procedure TscInplaceEdit.SetGrid(Value: TscCustomGrid);
begin
  FGrid := Value;
end;

procedure TscInplaceEdit.CMShowingChanged(var Message: TMessage);
begin
end;

procedure TscInplaceEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if scgoTabs in Grid.Options then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TscInplaceEdit.WMPaste(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  inherited
end;

procedure TscInplaceEdit.WMClear(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TscInplaceEdit.WMCut(var Message: TMessage);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TscInplaceEdit.DblClick;
begin
  Grid.DblClick;
end;

function TscInplaceEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := Grid.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TscInplaceEdit.EditCanModify: Boolean;
begin
  Result := Grid.CanEditModify;
end;

procedure TscInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    Grid.KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    GridKeyDown := Grid.OnKeyDown;
    if Assigned(GridKeyDown) then GridKeyDown(Grid, Key, Shift);
  end;

  function ForwardMovement: Boolean;
  begin
    Result := scgoAlwaysShowEditor in Grid.Options;
  end;

  function Ctrl: Boolean;
  begin
    Result := ssCtrl in Shift;
  end;

  function Selection: TSelection;
  begin
    SendMessage(Handle, EM_GETSEL, WPARAM(@Result.StartPos), LPARAM(@Result.EndPos));
  end;

  function CaretPos: Integer;
  var
    P: TPoint;
  begin
    Winapi.Windows.GetCaretPos(P);
    Result := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLong(P.X, P.Y));
  end;

  function RightSide: Boolean;
  begin
    with Selection do
      Result := (CaretPos = GetTextLen) and
        ((StartPos = 0) or (EndPos = StartPos)) and (EndPos = GetTextLen);
   end;

  function LeftSide: Boolean;
  begin
    with Selection do
      Result := (CaretPos = 0) and (StartPos = 0) and
        ((EndPos = 0) or (EndPos = GetTextLen));
  end;

begin
  case Key of
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_ESCAPE: SendToParent;
    VK_INSERT:
      if Shift = [] then SendToParent
      else if (Shift = [ssShift]) and not Grid.CanEditModify then Key := 0;
    VK_LEFT: if ForwardMovement and (Ctrl or LeftSide) then SendToParent;
    VK_RIGHT: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_HOME: if ForwardMovement and (Ctrl or LeftSide) then SendToParent;
    VK_END: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_F2:
      begin
        ParentEvent;
        if Key = VK_F2 then
        begin
          Deselect;
          Exit;
        end;
      end;
    VK_TAB: if not (ssAlt in Shift) then SendToParent;
    VK_DELETE:
      if Ctrl then
        SendToParent
      else
        if not Grid.CanEditModify then Key := 0;
  end;
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TscInplaceEdit.KeyPress(var Key: Char);
var
  Selection: TSelection;
begin
  Grid.KeyPress(Key);
  if (Key >= #32) and not Grid.CanEditAcceptKey(Key) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
  case Key of
    #9, #27: Key := #0;
    #13:
      begin
        SendMessage(Handle, EM_GETSEL,
          WPARAM(@Selection.StartPos), LPARAM(@Selection.EndPos));
        if (Selection.StartPos = 0) and (Selection.EndPos = GetTextLen) then
          Deselect else
          SelectAll;
        Key := #0;
      end;
    ^H, ^V, ^X, #32..High(Char):
      if not Grid.CanEditModify then Key := #0;
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TscInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Grid.KeyUp(Key, Shift);
end;

procedure TscInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(Grid) then
          Dispatch(Message);
        Exit;
      end;
    WM_LBUTTONDOWN:
      begin
        if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TscInplaceEdit.Deselect;
begin
  SendMessage(Handle, EM_SETSEL, $7FFFFFFF, LPARAM($FFFFFFFF));
end;

procedure TscInplaceEdit.Invalidate;
begin
  FStopGetParentBG := True;
  DoPaint;
  FStopGetParentBG := False;
end;

procedure TscInplaceEdit.InvalidateGrid;
var
  Cur: TRect;
begin
  WinApi.Windows.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, Grid.Handle, Cur, 2);
  ValidateRect(Grid.Handle, @Cur);
  InvalidateRect(Grid.Handle, @Cur, False);
end;

procedure TscInplaceEdit.Hide;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    InvalidateGrid;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
      SWP_NOREDRAW);
    if Focused then
      Winapi.Windows.SetFocus(Grid.Handle);
  end;
end;

function TscInplaceEdit.PosEqual(const Rect: TRect): Boolean;
var
  Cur: TRect;
begin
  GetWindowRect(Handle, Cur);
  MapWindowPoints(HWND_DESKTOP, Grid.Handle, Cur, 2);
  Result := EqualRect(Rect, Cur);
end;
                                    
procedure TscInplaceEdit.InternalMove(const Loc: TRect; Redraw: Boolean);
begin
  if IsRectEmpty(Loc) then Hide
  else
  begin
    CreateHandle;
    Redraw := Redraw or not IsWindowVisible(Handle);
    Invalidate;
    with Loc do
      SetWindowPos(Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top,
        SWP_SHOWWINDOW or SWP_NOREDRAW);
    BoundsChanged;
    if Redraw then Invalidate;
    if Grid.Focused then
      Winapi.Windows.SetFocus(Handle);
  end;
end;

procedure TscInplaceEdit.BoundsChanged;
begin
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TscInplaceEdit.UpdateLoc(const Loc: TRect);
begin
  InternalMove(Loc, False);
end;

function TscInplaceEdit.Visible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TscInplaceEdit.Move(const Loc: TRect);
begin
  InternalMove(Loc, True);
end;

procedure TscInplaceEdit.SetFocus;
begin
  if IsWindowVisible(Handle) then
    Winapi.Windows.SetFocus(Handle);
end;

procedure TscInplaceEdit.UpdateContents;
begin
  Text := '';
  EditMask := Grid.GetEditMask(Grid.Col, Grid.Row);
  Text := Grid.GetEditText(Grid.Col, Grid.Row);
  MaxLength := Grid.GetEditLimit;
end;

{ TscCustomGrid }

const
  GradientEndColorBase = $F0F0F0;

constructor TscCustomGrid.Create(AOwner: TComponent);
const
  GridStyle = [csCaptureMouse, csOpaque, csDoubleClicks,
                csNeedsBorderPaint, csPannable, csGestures];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := GridStyle
  else
    ControlStyle := GridStyle + [csFramed];
  FAlternateRow := False;
  FStopUpdateColums := True;
  FTransparentBackground := False;
  FSelectionStyle := scstStyled;
  FDrawInClientRect := True;
  FFullRePaint := True;
  FDrawOnBackground := False;
  FShowFocusRect := True;
  FTransparentEditor := False;
  FLinesColor := clNone;
  FTextColor := clBlack;
  FWallpapers := nil;
  FWallpaperIndex := -1;
  FCustomImages := nil;
  FCustomBackgroundImageIndex := -1;
  FCustomOverContentImageIndex := -1;
  FCanEditModify := True;
  FColCount := 5;
  FRowCount := 5;
  FFixedCols := 1;
  FFixedRows := 1;
  FGridLineWidth := 1;
  FOptions := [scgoFixedVertLine, scgoFixedHorzLine, scgoVertLine, scgoHorzLine,
    scgoRangeSelect];
  DesignOptionsBoost := [scgoColSizing, scgoRowSizing];
  FFixedColor := clBtnFace;
  FScrollBars := ssBoth;
  FBorderStyle := bsSingle;
  FDefaultColWidth := 64;
  FDefaultRowHeight := 24;
  FDefaultDrawing := True;
  FDrawingStyle := scgdsThemed;
  FGradientEndColor := GetShadowColor(GradientEndColorBase, -25);
  FGradientStartColor := clWhite;
  FSaveCellExtents := True;
  FEditorMode := False;
  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  SetBounds(Left, Top, FColCount * FDefaultColWidth,
    FRowCount * FDefaultRowHeight);
  FHotTrackCell.Coord.X := -1;
  FHotTrackCell.Coord.Y := -1;
  FHotTrackCell.Pressed := False;
  Touch.InteractiveGestures := [igPan, igPressAndTap];
  Touch.InteractiveGestureOptions := [igoPanInertia,
    igoPanSingleFingerHorizontal, igoPanSingleFingerVertical,
    igoPanGutter, igoParentPassthrough];
  Initialize;
end;

destructor TscCustomGrid.Destroy;
begin
  FInplaceEdit.Free;
  FInplaceEdit := nil;
  inherited Destroy;
  FreeMem(FColWidths);
  FreeMem(FRowHeights);
  FreeMem(FTabStops);
end;

procedure TscCustomGrid.ChangeGridOrientation(ACanvas: TCanvas; ARightToLeftOrientation: Boolean);
var
  Org: TPoint;
  Ext: TPoint;
begin
  if ARightToLeftOrientation then
  begin
    Org := Point(ClientWidth,0);
    Ext := Point(-1,1);
    SetMapMode(ACanvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(ACanvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(ACanvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(ACanvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end
  else
  begin
    Org := Point(0,0);
    Ext := Point(1,1);
    SetMapMode(ACanvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(ACanvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(ACanvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(ACanvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end;
end;

procedure TscCustomGrid.SetAlternateRow(Value: Boolean);
begin
  if FAlternateRow <> Value then
  begin
    FAlternateRow := Value;
    Invalidate;
  end;
end;

procedure TscCustomGrid.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

procedure TscCustomGrid.SetLinesColor(Value: TColor);
begin
  if FLinesColor <> Value then
  begin
    FLinesColor := Value;
    RePaintControl;
  end;
end;

procedure TscCustomGrid.SetCustomOverContentImageIndex(Value: Integer);
begin
   if FCustomOverContentImageIndex <> Value then
  begin
    FCustomOverContentImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomGrid.SetCustomBackgroundImageIndex(Value: Integer);
begin
   if FCustomBackgroundImageIndex <> Value then
  begin
    FCustomBackgroundImageIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomGrid.SetWallpaperIndex(Value: Integer);
begin
  if FWallpaperIndex <> Value then
  begin
    FWallpaperIndex := Value;
    RePaintControl;
  end;
end;

procedure TscCustomGrid.SetCustomImages(Value: TscCustomImageCollection);
begin
  if FCustomImages <> Value then
  begin
    FCustomImages := Value;
    RePaintControl;
  end;
end;

procedure TscCustomGrid.SetWallpapers(Value: TscCustomImageCollection);
begin
  if FWallpapers <> Value then
  begin
    FWallpapers := Value;
    RePaintControl;
  end;
end;

procedure TscCustomGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWallpapers) then
    FWallpapers := nil;
  if (Operation = opRemove) and (AComponent = FCustomImages) then
    FCustomImages := nil;
end;

procedure TscCustomGrid.SetBackgroundStyle(Value: TscGridBackgroundStyle);
begin
  if FBackgroundStyle <> Value then
  begin
    if FBackgroundStyle = scgbTransparent then
    begin
      TransparentBackground := False;
      GetParentBG;
    end;
    FBackgroundStyle := Value;
    if FBackgroundStyle = scgbTransparent then
    begin
      TransparentBackground := True;
      GetParentBG;
    end;
    RePaintControl;
  end;
end;

procedure TscCustomGrid.AdjustSize(Index, Amount: Longint; Rows: Boolean);
var
  NewCur: TscGridCoord;
  OldRows, OldCols: Longint;
  MovementX, MovementY: Longint;
  MoveRect: TscGridRect;
  ScrollArea: TRect;
  AbsAmount: Longint;

  function DoSizeAdjust(var Count: Longint; var Extents: Pointer;
    DefaultExtent: Integer; var Current: Longint): Longint;
  var
    I: Integer;
    NewCount: Longint;
  begin
    NewCount := Count + Amount;
    if NewCount < Index then InvalidOp(STooManyDeleted);
    if (Amount < 0) and Assigned(Extents) then
    begin
      Result := 0;
      for I := Index to Index - Amount - 1 do
        Inc(Result, PIntArray(Extents)^[I]);
    end
    else
      Result := Amount * DefaultExtent;
    if Extents <> nil then
      ModifyExtents(Extents, Index, Amount, DefaultExtent);
    Count := NewCount;
    if Current >= Index then
      if (Amount < 0) and (Current < Index - Amount) then Current := Index
      else Inc(Current, Amount);
  end;

begin
  if Amount = 0 then Exit;
  NewCur := FCurrent;
  OldCols := ColCount;
  OldRows := RowCount;
  MoveRect.Left := FixedCols;
  MoveRect.Right := ColCount - 1;
  MoveRect.Top := FixedRows;
  MoveRect.Bottom := RowCount - 1;
  MovementX := 0;
  MovementY := 0;
  AbsAmount := Amount;
  if AbsAmount < 0 then AbsAmount := -AbsAmount;
  if Rows then
  begin
    MovementY := DoSizeAdjust(FRowCount, FRowHeights, DefaultRowHeight, NewCur.Y);
    MoveRect.Top := Index;
    if Index + AbsAmount <= TopRow then MoveRect.Bottom := TopRow - 1;
  end
  else
  begin
    MovementX := DoSizeAdjust(FColCount, FColWidths, DefaultColWidth, NewCur.X);
    MoveRect.Left := Index;
    if Index + AbsAmount <= LeftCol then MoveRect.Right := LeftCol - 1;
  end;
  GridRectToScreenRect(MoveRect, ScrollArea, True);
  if not IsRectEmpty(ScrollArea) then
  begin
    ScrollWindow(Handle, MovementX, MovementY,
      @ScrollArea, @ScrollArea);
    UpdateWindow(Handle);
  end;
  SizeChanged(OldCols, OldRows);
  if (NewCur.X <> FCurrent.X) or (NewCur.Y <> FCurrent.Y) then
    MoveCurrent(NewCur.X, NewCur.Y, True, True);
end;

function TscCustomGrid.BoxRect(ALeft, ATop, ARight, ABottom: Longint): TRect;
var
  GridRect: TscGridRect;
begin
  GridRect.Left := ALeft;
  GridRect.Right := ARight;
  GridRect.Top := ATop;
  GridRect.Bottom := ABottom;
  GridRectToScreenRect(GridRect, Result, False);
end;

procedure TscCustomGrid.DoExit;
begin
  if Observers.IsObserving(TObserverMapping.EditGridLinkID) then
    if TLinkObservers.EditGridLinkIsEditing(Observers) then
    begin
      try
        TLinkObservers.EditGridLinkUpdate(Observers);
      except
        TLinkObservers.EditGridLinkReset(Observers);
        SetFocus;
        raise;
      end;
    end;

  inherited DoExit;
  if not (scgoAlwaysShowEditor in Options) then HideEditor;
end;

function TscCustomGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := BoxRect(ACol, ARow, ACol, ARow);
end;

function TscCustomGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  Result := True;
end;

function TscCustomGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function TscCustomGrid.CanEditModify: Boolean;
begin
  Result := FCanEditModify;
end;

function TscCustomGrid.CanEditShow: Boolean;
begin
  Result := ([scgoRowSelect, scgoEditing] * Options = [scgoEditing]) and
    FEditorMode and not (csDesigning in ComponentState) and HandleAllocated and
    ((scgoAlwaysShowEditor in Options) or IsActiveControl);
end;

procedure TscCustomGrid.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  if FStopUpdateColums then
  begin
    FDefaultColWidth := MulDiv(FDefaultColWidth, M, D);
    FDefaultRowHeight := MulDiv(FDefaultRowHeight, M, D);
  end
  else
  begin
    DefaultColWidth := MulDiv(DefaultColWidth, M, D);
    DefaultRowHeight := MulDiv(DefaultRowHeight, M, D);
  end;
end;

function TscCustomGrid.IsActiveControl: Boolean;
var
  H: Hwnd;
  ParentForm: TCustomForm;
begin
  Result := False;
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
    Result := (ParentForm.ActiveControl = Self) and (ParentForm = Screen.ActiveCustomForm)
  else
  begin
    H := GetFocus;
    while IsWindow(H) and not Result do
    begin
      if H = WindowHandle then
        Result := True
      else
        H := GetParent(H);
    end;
  end;
end;

function TscCustomGrid.IsGradientEndColorStored: Boolean;
begin
  Result := FGradientEndColor <> GetShadowColor(GradientEndColorBase, -25);
end;

function TscCustomGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
end;

function TscCustomGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
end;

procedure TscCustomGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
end;

function TscCustomGrid.GetEditLimit: Integer;
begin
  Result := 0;
end;

function TscCustomGrid.GetEditStyle(ACol, ARow: Longint): TscGridEditStyle;
begin
  Result := esSimple;
end;

procedure TscCustomGrid.HideEditor;
begin
  FEditorMode := False;
  HideEdit;
end;

procedure TscCustomGrid.ShowEditor;
begin
  FEditorMode := True;
  UpdateEdit;
end;

procedure TscCustomGrid.ShowEditorChar(Ch: Char);
begin
  ShowEditor;
  if FInplaceEdit <> nil then
    PostMessage(FInplaceEdit.Handle, WM_CHAR, Ord(Ch), 0);
end;

procedure TscCustomGrid.InvalidateEditor;
begin
  FInplaceCol := -1;
  FInplaceRow := -1;
  UpdateEdit;
end;

procedure TscCustomGrid.ReadColWidths(Reader: TReader);
var
  I: Integer;
begin
  with Reader do
  begin
    ReadListBegin;
    for I := 0 to ColCount - 1 do ColWidths[I] := ReadInteger;
    ReadListEnd;
  end;
end;

procedure TscCustomGrid.ReadRowHeights(Reader: TReader);
var
  I: Integer;
begin
  with Reader do
  begin
    ReadListBegin;
    for I := 0 to RowCount - 1 do RowHeights[I] := ReadInteger;
    ReadListEnd;
  end;
end;

procedure TscCustomGrid.WriteColWidths(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to ColCount - 1 do WriteInteger(ColWidths[I]);
    WriteListEnd;
  end;
end;

procedure TscCustomGrid.WriteRowHeights(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to RowCount - 1 do WriteInteger(RowHeights[I]);
    WriteListEnd;
  end;
end;

procedure TscCustomGrid.DefineProperties(Filer: TFiler);

  function DoColWidths: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not CompareExtents(TscCustomGrid(Filer.Ancestor).FColWidths, FColWidths)
    else
      Result := FColWidths <> nil;
  end;

  function DoRowHeights: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not CompareExtents(TscCustomGrid(Filer.Ancestor).FRowHeights, FRowHeights)
    else
      Result := FRowHeights <> nil;
  end;

begin
  inherited DefineProperties(Filer);
  if FSaveCellExtents then
    with Filer do
    begin
      DefineProperty('ColWidths', ReadColWidths, WriteColWidths, DoColWidths);
      DefineProperty('RowHeights', ReadRowHeights, WriteRowHeights, DoRowHeights);
    end;
end;

procedure TscCustomGrid.MoveColumn(FromIndex, ToIndex: Longint);
var
  Rect: TscGridRect;
begin
  if FromIndex = ToIndex then Exit;
  if Assigned(FColWidths) then
  begin
    MoveExtent(FColWidths, FromIndex + 1, ToIndex + 1);
    MoveExtent(FTabStops, FromIndex + 1, ToIndex + 1);
  end;
  MoveAdjust(FCurrent.X, FromIndex, ToIndex);
  MoveAdjust(FAnchor.X, FromIndex, ToIndex);
  MoveAdjust(FInplaceCol, FromIndex, ToIndex);
  Rect.Top := 0;
  Rect.Bottom := VisibleRowCount;
  if FromIndex < ToIndex then
  begin
    Rect.Left := FromIndex;
    Rect.Right := ToIndex;
  end
  else
  begin
    Rect.Left := ToIndex;
    Rect.Right := FromIndex;
  end;
  InvalidateRect(Rect);
  ColumnMoved(FromIndex, ToIndex);
  if Assigned(FColWidths) then
    ColWidthsChanged;
  UpdateEdit;
end;

procedure TscCustomGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
end;

procedure TscCustomGrid.MoveRow(FromIndex, ToIndex: Longint);
begin
  if Assigned(FRowHeights) then
    MoveExtent(FRowHeights, FromIndex + 1, ToIndex + 1);
  MoveAdjust(FCurrent.Y, FromIndex, ToIndex);
  MoveAdjust(FAnchor.Y, FromIndex, ToIndex);
  MoveAdjust(FInplaceRow, FromIndex, ToIndex);
  RowMoved(FromIndex, ToIndex);
  if Assigned(FRowHeights) then
    RowHeightsChanged;
  UpdateEdit;
end;

procedure TscCustomGrid.RowMoved(FromIndex, ToIndex: Longint);
begin
end;

function TscCustomGrid.MouseCoord(X, Y: Integer): TscGridCoord;
var
  DrawInfo: TscGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := CalcCoordFromPoint(X, Y, DrawInfo);
  if Result.X < 0 then Result.Y := -1
  else if Result.Y < 0 then Result.X := -1;
end;

procedure TscCustomGrid.MoveColRow(ACol, ARow: Longint; MoveAnchor,
  Show: Boolean);
begin
  MoveCurrent(ACol, ARow, MoveAnchor, Show);
end;

function TscCustomGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
end;

procedure TscCustomGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
end;

function TscCustomGrid.Sizing(X, Y: Integer): Boolean;
var
  DrawInfo: TscGridDrawInfo;
  State: TscGridState;
  Index: Longint;
  Pos, Ofs: Integer;
begin
  State := FGridState;
  if State = scgsNormal then
  begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end;
  Result := State <> scgsNormal;
end;

procedure TscCustomGrid.TopLeftChanged;
begin
  if FEditorMode and (FInplaceEdit <> nil) then
    FInplaceEdit.UpdateLoc(CellRect(Col, Row));
end;

procedure FillDWord(var Dest; Count, Value: Integer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
  P: PInteger;
begin
  P := PInteger(@Dest);
  for I := 0 to Count - 1 do
  begin
    P^ := Value;
    Inc(P);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
  XCHG  EDX, ECX
  PUSH  EDI
  MOV   EDI, EAX
  MOV   EAX, EDX
  REP   STOSD
  POP   EDI
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

function StackAlloc(Size: Integer): Pointer; {$IFNDEF PUREPASCAL} register; {$ENDIF}
{$IFDEF PUREPASCAL}
begin
  GetMem(Result, Size);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure StackFree(P: Pointer); {$IFNDEF PUREPASCAL}register; {$ENDIF}
{$IFDEF PUREPASCAL}
begin
  FreeMem(P);
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
  POP   ECX                     { return address }
  MOV   EDX, DWORD PTR [ESP]
  SUB   EAX, 8
  CMP   EDX, ESP                { sanity check #1 (SP = [SP]) }
  JNE   @@1
  CMP   EDX, EAX                { sanity check #2 (P = this stack block) }
  JNE   @@1
  MOV   ESP, DWORD PTR [ESP+4]  { restore previous SP  }
@@1:
  PUSH  ECX                     { return to caller }
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}

procedure TscCustomGrid.Draw;
var
  LStyle: TCustomStyleServices;
  LColor, C: TColor;
  LineColor: TColor;
  LFixedColor: TColor;
  LFixedBorderColor: TColor;
  DrawInfo: TscGridDrawInfo;
  Sel: TscGridRect;
  UpdateRect: TRect;
  AFocRect, FocRect: TRect;
  PointsList: PIntArray;
  StrokeList: PIntArray;
  MaxStroke: Integer;
  FrameFlags1, FrameFlags2: DWORD;
  R: TRect;

  procedure DrawLines(DoHorz, DoVert: Boolean; Col, Row: Longint;
    const CellBounds: array of Integer; OnColor, OffColor: TColor);
  const
    FlatPenStyle = PS_Geometric or PS_Solid or PS_EndCap_Flat or PS_Join_Miter;

    procedure DrawAxisLines(const AxisInfo: TscGridAxisDrawInfo;
      Cell, MajorIndex: Integer; UseOnColor: Boolean);
    var
      Line: Integer;
      LogBrush: TLOGBRUSH;
      Index: Integer;
      Points: PIntArray;
      StopMajor, StartMinor, StopMinor, StopIndex: Integer;
      LineIncr: Integer;
    begin
      with ACanvas, AxisInfo do
      begin
        if EffectiveLineWidth <> 0 then
        begin
          Pen.Width := GridLineWidth;
          if UseOnColor then
            Pen.Color := OnColor
          else
            Pen.Color := OffColor;
          if Pen.Width > 1 then
          begin
            LogBrush.lbStyle := BS_Solid;
            LogBrush.lbColor := Pen.Color;
            LogBrush.lbHatch := 0;
            Pen.Handle := ExtCreatePen(FlatPenStyle, Pen.Width, LogBrush, 0, nil);
          end;
          Points := PointsList;
          Line := CellBounds[MajorIndex] + (EffectiveLineWidth shr 1) +
            AxisInfo.GetExtent(Cell);
          if (BidiMode = bdRightToLeft) and (MajorIndex = 0) then Inc(Line);
          StartMinor := CellBounds[MajorIndex xor 1];
          StopMinor := CellBounds[2 + (MajorIndex xor 1)];
          if BidiMode = bdRightToLeft then Inc(StopMinor);
          StopMajor := CellBounds[2 + MajorIndex] + EffectiveLineWidth;
          StopIndex := MaxStroke * 4;
          Index := 0;
          repeat
            Points^[Index + MajorIndex] := Line;
            Points^[Index + (MajorIndex xor 1)] := StartMinor;
            Inc(Index, 2);
            Points^[Index + MajorIndex] := Line;
            Points^[Index + (MajorIndex xor 1)] := StopMinor;
            Inc(Index, 2);
            repeat
              Inc(Cell);
              LineIncr := AxisInfo.GetExtent(Cell) + EffectiveLineWidth;
            until (LineIncr > 0) or (Cell > LastFullVisibleCell);
            Inc(Line, LineIncr);
          until (Line > StopMajor) or (Cell > LastFullVisibleCell) or (Index > StopIndex);
          PolyPolyLine(ACanvas.Handle, Points^, StrokeList^, Index shr 2);
        end;
      end;
    end;

  begin
    if (CellBounds[0] = CellBounds[2]) or (CellBounds[1] = CellBounds[3]) then
      Exit;
    if not DoHorz then
    begin
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
    end
    else
    begin
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
    end;
  end;

  procedure DrawCells(ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer;
    AColor: TColor; IncludeDrawState: TscGridDrawState);
  var
    CurCol, CurRow: Longint;
    AWhere, Where, TempRect: TRect;
    DrawState: TscGridDrawState;
    Focused: Boolean;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while (Where.Top < StopY) and (CurRow < RowCount) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + RowHeights[CurRow];
      while (Where.Left < StopX) and (CurCol < ColCount) do
      begin
        Where.Right := Where.Left + ColWidths[CurCol];
        if (Where.Right > Where.Left) and RectVisible(ACanvas.Handle, Where) then
        begin
          DrawState := IncludeDrawState;
          if (CurCol = FHotTrackCell.Coord.X) and (CurRow = FHotTrackCell.Coord.Y) then
          begin
            if (scgoFixedHotTrack in Options) then
              Include(DrawState, scgdHotTrack);
            if FHotTrackCell.Pressed then
              Include(DrawState, scgdPressed);
          end;
          Focused := IsActiveControl;
          if Focused and (CurRow = Row) and (CurCol = Col)  then
          begin
            SetCaretPos(Where.Left, Where.Top);
            Include(DrawState, scgdFocused);
          end;
          if PointInGridRect(CurCol, CurRow, Sel) then
            Include(DrawState, scgdSelected);
          if not (scgdFocused in DrawState) or not (scgoEditing in Options) or
            not FEditorMode or (csDesigning in ComponentState) then
          begin
            if DefaultDrawing or (csDesigning in ComponentState) then
            begin
              ACanvas.Font := Self.Font;
              if not (FEditorMode and (FInplaceCol = CurCol) and (FInplaceRow = CurRow))
              then
                if (scgdSelected in DrawState) and
                   (not (scgdFocused in DrawState) or
                   ([scgoDrawFocusSelected, scgoRowSelect] * Options <> [])) then
                  DrawCellHighlight(ACanvas, Where, DrawState, CurCol, CurRow)
                else
                  DrawCellBackground(ACanvas, Where, AColor, DrawState, CurCol, CurRow);
            end;
            AWhere := Where;
            if (scgdPressed in DrawState) then
            begin
              Inc(AWhere.Top);
              Inc(AWhere.Left);
            end;
            if not (FEditorMode and (FInplaceCol = CurCol) and (FInplaceRow = CurRow))
            then
              DrawCell(ACanvas, CurCol, CurRow, AWhere, DrawState);
            if DefaultDrawing and (scgdFixed in DrawState) and Ctl3D and
              ((FrameFlags1 or FrameFlags2) <> 0) and
              (FInternalDrawingStyle = scgdsClassic) and not (scgdPressed in DrawState) then
            begin
              TempRect := Where;
              if (FrameFlags1 and BF_RIGHT) = 0 then
                Inc(TempRect.Right, DrawInfo.Horz.EffectiveLineWidth)
              else if (FrameFlags1 and BF_BOTTOM) = 0 then
                Inc(TempRect.Bottom, DrawInfo.Vert.EffectiveLineWidth);
              if not TStyleManager.IsCustomStyleActive then
              begin
                DrawEdge(ACanvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags1);
                DrawEdge(ACanvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags2);
              end;
            end;

            if DefaultDrawing and not (csDesigning in ComponentState)
               and (scgdFocused in DrawState) and
               ([scgoEditing, scgoAlwaysShowEditor] * Options <> [scgoEditing, scgoAlwaysShowEditor]) and
               not (scgoRowSelect in Options) then
            begin
              TempRect := Where;
              if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
                 and (FSelectionStyle = scstStyled)
              then
                InflateRect(TempRect, -1, -1);
              ACanvas.Brush.Style := bsSolid;
              if TStyleManager.IsCustomStyleActive then
              begin
                if FShowFocusRect then
                  scDrawUtils.scDrawFocusRect(ACanvas, TempRect, FScaleFactor);
              end
              else
              begin
                if FShowFocusRect then
                  scDrawUtils.scDrawFocusRect(ACanvas, TempRect, FScaleFactor);
              end;
            end;
          end;
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc(CurCol);
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc(CurRow);
    end;
  end;

begin
  Canvas.Font := Self.Font;
  if BiDiMode = bdRightToLeft then
    ChangeGridOrientation(ACanvas, True);

  R := ClientRect;
  // fill bg
  case FBackgroundStyle of
    scgbColor:
      with ACanvas do
      begin
        if (seClient in StyleElements) and IsCustomStyle then
          Brush.Color :=  GetEditBrushColor(scsNormal)
        else
          Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(R);
      end;
    scbgbFormBackground:
      begin
        scDrawUtils.DrawFormBackground(ACanvas, R);
      end;
  end;

  // draw wallpaper and custom background

  if (FWallpapers <> nil) and FWallpapers.IsIndexAvailable(FWallpaperIndex)
  then
    FWallpapers.Draw(ACanvas, R, FWallpaperIndex, FScaleFactor);

  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomBackgroundImageIndex)
  then
    FCustomImages.Draw(ACanvas, R, FCustomBackgroundImageIndex, FScaleFactor);

  // draw content

  FInternalColor := Color;
  LStyle := StyleServices;
  if (FInternalDrawingStyle = scgdsThemed) then
  begin
    LStyle.GetElementColor(LStyle.GetElementDetails(tgCellNormal), ecBorderColor, LineColor);
    if seClient in StyleElements then
      LStyle.GetElementColor(LStyle.GetElementDetails(tgCellNormal), ecFillColor, FInternalColor);
    LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecBorderColor, LFixedBorderColor);
    LStyle.GetElementColor(LStyle.GetElementDetails(tgFixedCellNormal), ecFillColor, LFixedColor);
  end
  else
  begin
    if FInternalDrawingStyle = scgdsGradient then
    begin
      LineColor := $F0F0F0;
      LFixedColor := Color;
      LFixedBorderColor := GetShadowColor($F0F0F0, -45);

      if LStyle.Enabled then
      begin
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientCellNormal),
           ecBorderColor, LColor) and (LColor <> clNone) then
          LineColor := LColor;
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientCellNormal),
           ecFillColor, LColor) and (LColor <> clNone) then
          FInternalColor := LColor;
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientFixedCellNormal),
           ecBorderColor, LColor) and (LColor <> clNone) then
          LFixedBorderColor := LColor;
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgGradientFixedCellNormal),
           ecFillColor, LColor) and (LColor <> clNone) then
          LFixedColor := LColor;
      end;
    end
    else
    begin
      LineColor := clSilver;
      LFixedColor := FixedColor;
      LFixedBorderColor := clBlack;

      if LStyle.Enabled then
      begin
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicCellNormal),
           ecBorderColor, LColor) and (LColor <> clNone) then
          LineColor := LColor;
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicCellNormal),
           ecFillColor, LColor) and (LColor <> clNone) then
          FInternalColor := LColor;
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicFixedCellNormal),
           ecBorderColor, LColor) and (LColor <> clNone) then
          LFixedBorderColor := LColor;
        if LStyle.GetElementColor(LStyle.GetElementDetails(tgClassicFixedCellNormal),
           ecFillColor, LColor) and (LColor <> clNone) then
          LFixedColor := LColor;
      end;
    end;
  end;

  UpdateRect := ACanvas.ClipRect;
  CalcDrawInfo(DrawInfo);

  if FLinesColor <> clNone then
  begin
    LineColor := GetStyleColor(FLinesColor);
  end
  else
  if FBackgroundStyle <> scgbColor then
  begin
    LineColor := scDrawUtils.GetCheckBoxTextColor(scsNormal);
    C := scDrawutils.GetStylecolor(clBtnFace);
    LineColor := scDrawUtils.MiddleColor(LineColor, C);
    LineColor := scDrawUtils.MiddleColor(LineColor, C);
    LineColor := scDrawUtils.MiddleColor(LineColor, C);
  end;

  // draw lines
  with DrawInfo do
  begin
    if (Horz.EffectiveLineWidth > 0) or (Vert.EffectiveLineWidth > 0) then
    begin
      MaxStroke := Max(Horz.LastFullVisibleCell - LeftCol + FixedCols,
        Vert.LastFullVisibleCell - TopRow + FixedRows) + 3;
      PointsList := StackAlloc(MaxStroke * sizeof(TPoint) * 2);
      StrokeList := StackAlloc(MaxStroke * sizeof(Integer));
      FillDWord(StrokeList^, MaxStroke, 2);

      if ColorToRGB(FInternalColor) = clSilver then
        LineColor := clGray;
      DrawLines(scgoFixedHorzLine in Options, scgoFixedVertLine in Options,
        0, 0, [0, 0, Horz.FixedBoundary, Vert.FixedBoundary], LFixedBorderColor, LFixedColor);
      DrawLines(scgoFixedHorzLine in Options, scgoFixedVertLine in Options,
        LeftCol, 0, [Horz.FixedBoundary, 0, Horz.GridBoundary,
        Vert.FixedBoundary], LFixedBorderColor, LFixedColor);
      DrawLines(scgoFixedHorzLine in Options, scgoFixedVertLine in Options,
        0, TopRow, [0, Vert.FixedBoundary, Horz.FixedBoundary,
        Vert.GridBoundary], LFixedBorderColor, LFixedColor);
      DrawLines(scgoHorzLine in Options, scgoVertLine in Options, LeftCol,
        TopRow, [Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridBoundary,
        Vert.GridBoundary], LineColor, FInternalColor);

      StackFree(StrokeList);
      StackFree(PointsList);
    end;

    Sel := Selection;
    FrameFlags1 := 0;
    FrameFlags2 := 0;
    if scgoFixedVertLine in Options then
    begin
      FrameFlags1 := BF_RIGHT;
      FrameFlags2 := BF_LEFT;
    end;
    if scgoFixedHorzLine in Options then
    begin
      FrameFlags1 := FrameFlags1 or BF_BOTTOM;
      FrameFlags2 := FrameFlags2 or BF_TOP;
    end;
    // draw fixed cells
    DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, LFixedColor,
      [scgdFixed]);
    DrawCells(LeftCol, 0, Horz.FixedBoundary - FColOffset, 0, Horz.GridBoundary,
      Vert.FixedBoundary, LFixedColor, [scgdFixed]);
    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary,
      Vert.GridBoundary, LFixedColor, [scgdFixed]);

    // draw cells
    DrawCells(LeftCol, TopRow, Horz.FixedBoundary - FColOffset,
      Vert.FixedBoundary, Horz.GridBoundary, Vert.GridBoundary, FInternalColor, []);

    // draw focus
    if not (csDesigning in ComponentState) and
       (scgoRowSelect in Options) and Focused then
    begin
      GridRectToScreenRect(GetSelection, FocRect, False);
      ACanvas.Brush.Style := bsSolid;
      if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
        and (FSelectionStyle = scstStyled)
      then
        InflateRect(FocRect, -1, -1);
      AFocRect := FocRect;
      if FShowFocusRect then
      begin
        if FSelectionStyle = scstStyled then
          ACanvas.Font.Color := GetSelectionTextColor
        else
          ACanvas.Font.Color := GetStyleColor(clHighLightText);
        scDrawFocusRect(ACanvas, AFocRect, FScaleFactor);
      end;
    end;
  end;

  // image over content
  if (FCustomImages <> nil) and FCustomImages.IsIndexAvailable(FCustomOverContentImageIndex)
  then
    FCustomImages.Draw(ACanvas, R, FCustomOverContentImageIndex, FScaleFactor);

  if BiDiMode = bdRightToLeft then
    ChangeGridOrientation(ACanvas, False);
end;

function TscCustomGrid.CalcCoordFromPoint(X, Y: Integer;
  const DrawInfo: TscGridDrawInfo): TscGridCoord;

  function DoCalc(const AxisInfo: TscGridAxisDrawInfo; N: Integer): Integer;
  var
    I, Start, Stop: Longint;
    Line: Integer;
  begin
    with AxisInfo do
    begin
      if N < FixedBoundary then
      begin
        Start := 0;
        Stop :=  FixedCellCount - 1;
        Line := 0;
      end
      else
      begin
        Start := FirstGridCell;
        Stop := GridCellCount - 1;
        Line := FixedBoundary;
      end;
      Result := -1;
      for I := Start to Stop do
      begin
        Inc(Line, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if N < Line then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
  end;

  function DoCalcRightToLeft(const AxisInfo: TscGridAxisDrawInfo; N: Integer): Integer;
  begin
    N := ClientWidth - N;
    Result := DoCalc(AxisInfo, N);
  end;

begin
  if not UseRightToLeftAlignment then
    Result.X := DoCalc(DrawInfo.Horz, X)
  else
    Result.X := DoCalcRightToLeft(DrawInfo.Horz, X);
  Result.Y := DoCalc(DrawInfo.Vert, Y);
end;

function TscCustomGrid.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditGridLinkID then
    Result := True
  else if ID = TObserverMapping.PositionLinkID then
    Result := True;
end;

procedure TscCustomGrid.ObserverAdded(const ID: Integer;
  const Observer: IObserver);
var
  LGridLinkObserver: IEditGridLinkObserver;
begin
  if ID = TObserverMapping.EditGridLinkID then
    Observer.OnObserverToggle := ObserverToggle;
  if Supports(Observer, IEditGridLinkObserver, LGridLinkObserver) then
    LGridLinkObserver.OnObserverCurrent := ObserverCurrent;
end;

function TscCustomGrid.ObserverCurrent: TVarRec;
begin
  Result.VType := vtInteger;
  //Expects 0 based index
  Result.VInteger := Col;
end;

procedure TscCustomGrid.ObserverToggle(const AObserver: IObserver;
  const Value: Boolean);
begin
  // Code to use observers removed.  Observers
  // do not affect goEditing option of a grid.
end;

procedure TscCustomGrid.CalcDrawInfo(var DrawInfo: TscGridDrawInfo);
begin
  CalcDrawInfoXY(DrawInfo, ClientWidth, ClientHeight);
end;

procedure TscCustomGrid.CalcDrawInfoXY(var DrawInfo: TscGridDrawInfo;
  UseWidth, UseHeight: Integer);

  procedure CalcAxis(var AxisInfo: TscGridAxisDrawInfo; UseExtent: Integer);
  var
    I: Integer;
  begin
    with AxisInfo do
    begin
      GridExtent := UseExtent;
      GridBoundary := FixedBoundary;
      FullVisBoundary := FixedBoundary;
      LastFullVisibleCell := FirstGridCell;
      for I := FirstGridCell to GridCellCount - 1 do
      begin
        Inc(GridBoundary, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if GridBoundary > GridExtent + EffectiveLineWidth then
        begin
          GridBoundary := GridExtent;
          Break;
        end;
        LastFullVisibleCell := I;
        FullVisBoundary := GridBoundary;
      end;
    end;
  end;

begin
  CalcFixedInfo(DrawInfo);
  CalcAxis(DrawInfo.Horz, UseWidth);
  CalcAxis(DrawInfo.Vert, UseHeight);
end;

procedure TscCustomGrid.CalcFixedInfo(var DrawInfo: TscGridDrawInfo);

  procedure CalcFixedAxis(var Axis: TscGridAxisDrawInfo; LineOptions: TscGridOptions;
    FixedCount, FirstCell, CellCount: Integer; GetExtentFunc: TscGetExtentsFunc);
  var
    I: Integer;
  begin
    with Axis do
    begin
      if LineOptions * Options = [] then
        EffectiveLineWidth := 0
      else
        EffectiveLineWidth := GridLineWidth;

      FixedBoundary := 0;
      for I := 0 to FixedCount - 1 do
        Inc(FixedBoundary, GetExtentFunc(I) + EffectiveLineWidth);

      FixedCellCount := FixedCount;
      FirstGridCell := FirstCell;
      GridCellCount := CellCount;
      GetExtent := GetExtentFunc;
    end;
  end;

begin
  CalcFixedAxis(DrawInfo.Horz, [scgoFixedVertLine, scgoVertLine], FixedCols,
    LeftCol, ColCount, GetColWidths);
  CalcFixedAxis(DrawInfo.Vert, [scgoFixedHorzLine, scgoHorzLine], FixedRows,
    TopRow, RowCount, GetRowHeights);
end;

function TscCustomGrid.CalcMaxTopLeft(const Coord: TscGridCoord;
  const DrawInfo: TscGridDrawInfo): TscGridCoord;

  function CalcMaxCell(const Axis: TscGridAxisDrawInfo; Start: Integer): Integer;
  var
    Line: Integer;
    I, Extent: Longint;
  begin
    Result := Start;
    with Axis do
    begin
      Line := GridExtent + EffectiveLineWidth;
      for I := Start downto FixedCellCount do
      begin
        Extent := GetExtent(I);
        if Extent > 0 then
        begin
          Dec(Line, Extent);
          Dec(Line, EffectiveLineWidth);
          if Line < FixedBoundary then
          begin
            if (Result = Start) and (GetExtent(Start) <= 0) then
              Result := I;
            Break;
          end;
          Result := I;
        end;
      end;
    end;
  end;

begin
  Result.X := CalcMaxCell(DrawInfo.Horz, Coord.X);
  Result.Y := CalcMaxCell(DrawInfo.Vert, Coord.Y);
end;

procedure TscCustomGrid.CalcSizingState(X, Y: Integer; var State: TscGridState;
  var Index: Longint; var SizingPos, SizingOfs: Integer;
  var FixedInfo: TscGridDrawInfo);

  procedure CalcAxisState(const AxisInfo: TscGridAxisDrawInfo; Pos: Integer;
    NewState: TscGridState);
  var
    I, Line, Back, Range: Integer;
  begin
    if (NewState = scgsColSizing) and (BidiMode = bdRightToLeft) then
      Pos := ClientWidth - Pos;
    with AxisInfo do
    begin
      Line := FixedBoundary;
      Range := EffectiveLineWidth;
      Back := 0;
      if Range < 7 then
      begin
        Range := 7;
        Back := (Range - EffectiveLineWidth) shr 1;
      end;
      for I := FirstGridCell to GridCellCount - 1 do
      begin
        Inc(Line, AxisInfo.GetExtent(I));
        if Line > GridBoundary then Break;
        if (Pos >= Line - Back) and (Pos <= Line - Back + Range) then
        begin
          State := NewState;
          SizingPos := Line;
          SizingOfs := Line - Pos;
          Index := I;
          Exit;
        end;
        Inc(Line, EffectiveLineWidth);
      end;
      if (GridBoundary = GridExtent) and (Pos >= GridExtent - Back)
        and (Pos <= GridExtent) then
      begin
        State := NewState;
        SizingPos := GridExtent;
        SizingOfs := GridExtent - Pos;
        Index := LastFullVisibleCell + 1;
      end;
    end;
  end;

  function XOutsideHorzFixedBoundary: Boolean;
  begin
    with FixedInfo do
      if BidiMode <> bdRightToLeft then
        Result := X > Horz.FixedBoundary
      else
        Result := X < ClientWidth - Horz.FixedBoundary;
  end;

  function XOutsideOrEqualHorzFixedBoundary: Boolean;
  begin
    with FixedInfo do
      if BidiMode <> bdRightToLeft then
        Result := X >= Horz.FixedBoundary
      else
        Result := X <= ClientWidth - Horz.FixedBoundary;
  end;


var
  EffectiveOptions: TscGridOptions;
begin
  State := scgsNormal;
  Index := -1;
  EffectiveOptions := Options;
  if csDesigning in ComponentState then
    EffectiveOptions := EffectiveOptions + DesignOptionsBoost;
  if [scgoColSizing, scgoRowSizing] * EffectiveOptions <> [] then
    with FixedInfo do
    begin
      Vert.GridExtent := ClientHeight;
      Horz.GridExtent := ClientWidth;
      if (XOutsideHorzFixedBoundary) and (scgoColSizing in EffectiveOptions) then
      begin
        if Y >= Vert.FixedBoundary then Exit;
        CalcAxisState(Horz, X, scgsColSizing);
      end
      else if (Y > Vert.FixedBoundary) and (scgoRowSizing in EffectiveOptions) then
      begin
        if XOutsideOrEqualHorzFixedBoundary then Exit;
        CalcAxisState(Vert, Y, scgsRowSizing);
      end;
    end;
end;

procedure TscCustomGrid.ChangeSize(NewColCount, NewRowCount: Longint);
var
  OldColCount, OldRowCount: Longint;
  OldDrawInfo: TscGridDrawInfo;

  procedure MinRedraw(const OldInfo, NewInfo: TscGridAxisDrawInfo; Axis: Integer);
  var
    R: TRect;
    First: Integer;
  begin
    First := Min(OldInfo.LastFullVisibleCell, NewInfo.LastFullVisibleCell);
    // Get the rectangle around the leftmost or topmost cell in the target range.
    R := CellRect(First and not Axis, First and Axis);
    R.Bottom := Height;
    R.Right := Width;
    Winapi.Windows.InvalidateRect(Handle, R, False);
  end;

  procedure DoChange;
  var
    Coord: TscGridCoord;
    NewDrawInfo: TscGridDrawInfo;
  begin
    if FColWidths <> nil then
      UpdateExtents(FColWidths, ColCount, DefaultColWidth);
    if FTabStops <> nil then
      UpdateExtents(FTabStops, ColCount, Integer(True));
    if FRowHeights <> nil then
      UpdateExtents(FRowHeights, RowCount, DefaultRowHeight);
    Coord := FCurrent;
    if Row >= RowCount then Coord.Y := RowCount - 1;
    if Col >= ColCount then Coord.X := ColCount - 1;
    if (FCurrent.X <> Coord.X) or (FCurrent.Y <> Coord.Y) then
      MoveCurrent(Coord.X, Coord.Y, True, True);
    if (FAnchor.X <> Coord.X) or (FAnchor.Y <> Coord.Y) then
      MoveAnchor(Coord);
    if VirtualView or
      (LeftCol <> OldDrawInfo.Horz.FirstGridCell) or
      (TopRow <> OldDrawInfo.Vert.FirstGridCell) then
      InvalidateGrid
    else if HandleAllocated then
    begin
      CalcDrawInfo(NewDrawInfo);
      MinRedraw(OldDrawInfo.Horz, NewDrawInfo.Horz, 0);
      MinRedraw(OldDrawInfo.Vert, NewDrawInfo.Vert, -1);
    end;
    UpdateScrollRange;
    SizeChanged(OldColCount, OldRowCount);
  end;

begin
  if HandleAllocated then
    CalcDrawInfo(OldDrawInfo);
  OldColCount := FColCount;
  OldRowCount := FRowCount;
  FColCount := NewColCount;
  FRowCount := NewRowCount;
  if FixedCols > NewColCount then FFixedCols := NewColCount - 1;
  if FixedRows > NewRowCount then FFixedRows := NewRowCount - 1;
  try
    DoChange;
  except
    FColCount := OldColCount;
    FRowCount := OldRowCount;
    DoChange;
    InvalidateGrid;
    raise;
  end;
end;

procedure TscCustomGrid.ClampInView(const Coord: TscGridCoord);
var
  DrawInfo: TscGridDrawInfo;
  MaxTopLeft: TscGridCoord;
  OldTopLeft: TscGridCoord;
begin
  if not HandleAllocated then Exit;
  CalcDrawInfo(DrawInfo);
  with DrawInfo, Coord do
  begin
    if (X > Horz.LastFullVisibleCell) or
      (Y > Vert.LastFullVisibleCell) or (X < LeftCol) or (Y < TopRow) then
    begin
      OldTopLeft := FTopLeft;
      MaxTopLeft := CalcMaxTopLeft(Coord, DrawInfo);
      Update;
      if X < LeftCol then FTopLeft.X := X
      else if X > Horz.LastFullVisibleCell then FTopLeft.X := MaxTopLeft.X;
      if Y < TopRow then FTopLeft.Y := Y
      else if Y > Vert.LastFullVisibleCell then FTopLeft.Y := MaxTopLeft.Y;
      TopLeftMoved(OldTopLeft);
    end;
  end;
end;

procedure TscCustomGrid.DrawSizingLine(const DrawInfo: TscGridDrawInfo);
var
  OldPen: TPen;
begin
  OldPen := TPen.Create;
  try
    with Canvas, DrawInfo do
    begin
      OldPen.Assign(Pen);
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Width := 1;
      try
        if FGridState = scgsRowSizing then
        begin
          if UseRightToLeftAlignment then
          begin
            MoveTo(Horz.GridExtent, FSizingPos);
            LineTo(Horz.GridExtent - Horz.GridBoundary, FSizingPos);
          end
          else
          begin
            MoveTo(0, FSizingPos);
            LineTo(Horz.GridBoundary, FSizingPos);
          end;
        end
        else
        begin
          MoveTo(FSizingPos, 0);
          LineTo(FSizingPos, Vert.GridBoundary);
        end;
      finally
        Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TscCustomGrid.DrawCellHighlight(ACanvas: TCanvas; const ARect: TRect; AState: TscGridDrawState;
  ACol, ARow: Integer);
const
  CSelected: array[TscGridDrawingStyle] of TThemedGrid = (
    tgClassicCellSelected, tgCellSelected, tgGradientCellSelected);
  CRowSelectedLeft: array[TscGridDrawingStyle] of TThemedGrid = (
    tgClassicCellRowSelectedLeft, tgCellRowSelectedLeft, tgGradientCellRowSelectedLeft);
  CRowSelectedCenter: array[TscGridDrawingStyle] of TThemedGrid = (
    tgClassicCellRowSelectedCenter, tgCellRowSelectedCenter, tgGradientCellRowSelectedCenter);
  CRowSelectedRight: array[TscGridDrawingStyle] of TThemedGrid = (
    tgClassicCellRowSelectedRight, tgCellRowSelectedRight, tgGradientCellRowSelectedRight);
var
  LRect: TRect;
  LGridPart: TThemedGrid;
  LStyle: TCustomStyleServices;
  LColor, LEndColor, LTextColor, LColorRef: TColor;
  SaveIndex: Integer;
begin
  LStyle := StyleServices;
  if (scgoRowSelect in Options) then
    Include(AState, scgdRowSelected);
  LRect := ARect;

  LGridPart := CSelected[FInternalDrawingStyle];
  if LStyle.Enabled and (scgdRowSelected in AState) then
  begin
    if (ACol >= FixedCols + 1) and (ACol < ColCount - 1) then
    begin
      LGridPart := CRowSelectedCenter[FInternalDrawingStyle];
      Inc(LRect.Right, 4);
      Dec(LRect.Left, 4);
    end
    else if ACol = FixedCols then
    begin
      LGridPart := CRowSelectedLeft[FInternalDrawingStyle];
      Inc(LRect.Right, 4);
    end
    else if ACol = (ColCount - 1) then
    begin
      LGridPart := CRowSelectedRight[FInternalDrawingStyle];
      Dec(LRect.Left, 4);
    end;
  end;

  if (FInternalDrawingStyle = scgdsThemed) then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, ARect.Left,
        ARect.Top, ARect.Right, ARect.Bottom);

      if TStyleManager.IsCustomStyleActive and (BidiMode = bdRightToLeft) then
        OffsetRect(LRect, 1, 0);

      if FSelectionStyle = scstStyled then
      begin
        scDrawUtils.DrawSelection(ACanvas, LRect, Focused, FShowFocusRect);
        LColor := scDrawUtils.GetSelectionTextColor;
      end
      else
      begin
        ACanvas.Brush.Color := GetStyleColor(clHighLight);
        if not Focused and not FShowFocusRect then
           FillRectWithAlpha(ACanvas, LRect, 200)
        else
           ACanvas.FillRect(LRect);
        LColor := GetStyleColor(clHighLightText);
      end;
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    ACanvas.Font.Color := LColor;
    ACanvas.Brush.Style := bsClear;
  end
  else
  begin
    if FInternalDrawingStyle = scgdsGradient then
    begin
      LRect := ARect;
      ACanvas.Brush.Color := LStyle.GetSystemColor(clHighlight);
      ACanvas.FrameRect(LRect);
      if (scgdRowSelected in AState) then
      begin
        InflateRect(LRect, 0, -1);
        if ACol = FixedCols then
          Inc(LRect.Left)
        else if ACol = (ColCount - 1) then
          Dec(LRect.Right)
      end
      else
        InflateRect(LRect, -1, -1);

      LColor := GetShadowColor(clHighlight, 45);
      LEndColor := GetShadowColor(clHighlight, 10);
      LTextColor := clHighlightText;

      if LStyle.Enabled then
      begin
        if LStyle.GetElementColor(LStyle.GetElementDetails(LGridPart), ecGradientColor1, LColorRef) and
           (LColorRef <> clNone) then
          LColor := LColorRef;
        if LStyle.GetElementColor(LStyle.GetElementDetails(LGridPart), ecGradientColor2, LColorRef) and
           (LColorRef <> clNone) then
          LEndColor := LColorRef;
        if LStyle.GetElementColor(LStyle.GetElementDetails(LGridPart), ecTextColor, LColorRef) and
           (LColorRef <> clNone) then
          LTextColor := LColorRef;
      end;
      GradientFillCanvas(ACanvas, LColor, LEndColor, LRect, gdVertical);
      ACanvas.Font.Color := LTextColor;
      ACanvas.Brush.Style := bsClear;
    end
    else
    begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText;
      if LStyle.Enabled then
      begin
        if LStyle.GetElementColor(LStyle.GetElementDetails(LGridPart), ecFillColor, LColor) and
           (LColor <> clNone) then
          ACanvas.Brush.Color := LColor;
        if LStyle.GetElementColor(LStyle.GetElementDetails(LGridPart), ecTextColor, LColor) and
           (LColor <> clNone) then
          ACanvas.Font.Color := LColor;
      end;
      if not Focused and not FShowFocusRect then
        FillRectWithAlpha(ACanvas, ARect, 200)
      else
        ACanvas.FillRect(ARect);
    end;
  end;
end;

procedure TscCustomGrid.DrawCellBackground(ACanvas: TCanvas; const ARect: TRect; AColor: TColor;
  AState: TscGridDrawState; ACol, ARow: Integer);
const
  CFixedStates: array[Boolean, Boolean] of TThemedGrid = (
    (tgFixedCellNormal, tgFixedCellPressed),
    (tgFixedCellHot, tgFixedCellPressed));
  CFixedGradientStates: array[Boolean, Boolean] of TThemedGrid = (
    (tgGradientFixedCellNormal, tgGradientFixedCellPressed),
    (tgGradientFixedCellHot, tgGradientFixedCellPressed));
  CFixedClassicStates: array[Boolean, Boolean] of TThemedGrid = (
    (tgClassicFixedCellNormal, tgClassicFixedCellPressed),
    (tgClassicFixedCellHot, tgClassicFixedCellPressed));
  CNormalStates: array[Boolean] of TThemedGrid = (
    tgCellNormal, tgCellSelected);
  CNormalGradientStates: array[Boolean] of TThemedGrid = (
    tgGradientCellNormal, tgGradientCellSelected);
  CNormalClassicStates: array[Boolean] of TThemedGrid = (
    tgClassicCellNormal, tgClassicCellSelected);
var
  LStyle: TCustomStyleServices;
  LRect, ClipRect: TRect;
  LDetails: TThemedElementDetails;
  LColor, LEndColor, LStartColor: TColor;
  SaveIndex: Integer;
  FAlternateColor: TColor;
begin
  LRect := ARect;
  LStyle := StyleServices;
  FTextColor := ACanvas.Font.Color;
  if (FInternalDrawingStyle = scgdsThemed) and (scgdFixed in AState) then
  begin
    ClipRect := LRect;
    Inc(LRect.Bottom);
    Inc(LRect.Right);
    if TStyleManager.IsCustomStyleActive and (BidiMode = bdRightToLeft) then
      OffsetRect(LRect, 1, 0);

    LDetails := LStyle.GetElementDetails(CFixedStates[(scgdHotTrack in AState),
      (scgdPressed  in AState)]);
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      LStyle.DrawElement(ACanvas.Handle, LDetails, LRect, ClipRect);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;

    ACanvas.Brush.Style := bsClear;

    if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
      ACanvas.Font.Color := LColor;
  end
  else
  begin
    if (FInternalDrawingStyle = scgdsGradient) and (scgdFixed in AState) then
    begin
      if not (scgoFixedVertLine in Options) then
        Inc(LRect.Right);
      if not (scgoFixedHorzLine in Options) then
        Inc(LRect.Bottom);

      if (scgdHotTrack in AState) or (scgdPressed  in AState) then
      begin
        if (scgdPressed  in AState) then
        begin
          LStartColor := FGradientEndColor;
          LEndColor := FGradientStartColor;
        end
        else
        begin
          LStartColor := GetHighlightColor(FGradientStartColor);
          LEndColor := GetHighlightColor(FGradientEndColor);
        end;
      end
      else
      begin
        LStartColor := FGradientStartColor;
        LEndColor := FGradientEndColor;
      end;

      if LStyle.Enabled then
      begin
        LDetails := LStyle.GetElementDetails(CFixedGradientStates[(scgdHotTrack in AState), (scgdPressed  in AState)]);
        if LStyle.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
          LStartColor := LColor;
        if LStyle.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
          LEndColor := LColor;
        if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
          ACanvas.Font.Color := LColor;
      end;
      GradientFillCanvas(ACanvas, LStartColor, LEndColor, LRect, gdVertical);
      ACanvas.Brush.Style := bsClear;
    end
    else
    begin
      if LStyle.Enabled then
      begin
        case FInternalDrawingStyle of
          scgdsClassic:
            if (scgdFixed in AState) then
              LDetails := LStyle.GetElementDetails(CFixedClassicStates[(scgdHotTrack in AState), (scgdPressed  in AState)])
            else
              LDetails := LStyle.GetElementDetails(CNormalClassicStates[(scgdSelected in AState) and (scgoDrawFocusSelected in FOptions)]);
          scgdsThemed:
            LDetails := LStyle.GetElementDetails(CNormalStates[(scgdSelected in AState) and (scgoDrawFocusSelected in FOptions)]);
          scgdsGradient:
            LDetails := LStyle.GetElementDetails(CNormalGradientStates[(scgdSelected in AState) and (scgoDrawFocusSelected in FOptions)]);
        end;
        if seClient in StyleElements then
          if LStyle.GetElementColor(LDetails, ecFillColor, LColor) and (LColor <> clNone) then
            AColor := LColor;
      end;
      // alternate color
      ACanvas.Brush.Color := AColor;
      if scgdFixed in AState then
        ACanvas.FillRect(LRect)
      else
      if FAlternateRow and not Odd(ARow) then
      begin
        FAlternateColor := scdrawUtils.AlternateColor(AColor);
        ACanvas.Brush.Color := FAlternateColor;
        ACanvas.FillRect(LRect);
      end;
      //
      if (scgdPressed  in AState) then
      begin
        if TStyleManager.IsCustomStyleActive then
        begin
          DrawStyleEdge(ACanvas, LRect, [eeSunkenInner], [efTopLeft]);
          DrawStyleEdge(ACanvas, LRect, [eeSunkenInner], [efBottomRight]);
        end
        else
        begin
          Dec(LRect.Right);
          Dec(LRect.Bottom);
          DrawEdge(ACanvas.Handle, LRect, BDR_SUNKENINNER, BF_TOPLEFT);
          DrawEdge(ACanvas.Handle, LRect, BDR_SUNKENINNER, BF_BOTTOMRIGHT);
        end;
        ACanvas.Brush.Style := bsClear;
      end;
      FTextColor := ACanvas.Font.Color;
      if LStyle.Enabled then
      begin
        if (seFont in StyleElements) and (FBackgroundStyle <> scgbColor) then
        begin
          LColor := scDrawUtils.GetCheckBoxTextColor(scsNormal);
          ACanvas.Font.Color := LColor;
          FTextColor := LColor;
        end
        else
        if (seFont in StyleElements) and LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
        begin
          ACanvas.Font.Color := LColor;
          FTextColor := LColor;
        end;
      end;
    end;
  end;
end;

procedure TscCustomGrid.DrawMove;
var
  OldPen: TPen;
  Pos: Integer;
  R: TRect;
begin
  OldPen := TPen.Create;
  try
    with Canvas do
    begin
      OldPen.Assign(Pen);
      try
        Pen.Style := psDot;
        Pen.Mode := pmXor;
        Pen.Width := 5;
        if FGridState = scgsRowMoving then
        begin
          R := CellRect(0, FMovePos);
          if FMovePos > FMoveIndex then
            Pos := R.Bottom else
            Pos := R.Top;
          MoveTo(0, Pos);
          LineTo(ClientWidth, Pos);
        end
        else
        begin
          R := CellRect(FMovePos, 0);
          if FMovePos > FMoveIndex then
            if not UseRightToLeftAlignment then
              Pos := R.Right
            else
              Pos := R.Left
          else
            if not UseRightToLeftAlignment then
              Pos := R.Left
            else
              Pos := R.Right;
          MoveTo(Pos, 0);
          LineTo(Pos, ClientHeight);
        end;
      finally
        Canvas.Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TscCustomGrid.FixedCellClick(ACol, ARow: Integer);
begin
  if Assigned(FOnFixedCellClick) then
    FOnFixedCellClick(Self, ACol, ARow);
end;

procedure TscCustomGrid.FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
begin
  MoveCurrent(ACol, ARow, MoveAnchor, True);
  UpdateEdit;
  Click;
end;

procedure TscCustomGrid.GridRectToScreenRect(GridRect: TscGridRect;
  var ScreenRect: TRect; IncludeLine: Boolean);

  function LinePos(const AxisInfo: TscGridAxisDrawInfo; Line: Integer): Integer;
  var
    Start, I: Longint;
  begin
    with AxisInfo do
    begin
      Result := 0;
      if Line < FixedCellCount then
        Start := 0
      else
      begin
        if Line >= FirstGridCell then
          Result := FixedBoundary;
        Start := FirstGridCell;
      end;
      for I := Start to Line - 1 do
      begin
        Inc(Result, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if Result > GridExtent then
        begin
          Result := 0;
          Exit;
        end;
      end;
    end;
  end;

  function CalcAxis(const AxisInfo: TscGridAxisDrawInfo;
    GridRectMin, GridRectMax: Integer;
    var ScreenRectMin, ScreenRectMax: Integer): Boolean;
  begin
    Result := False;
    with AxisInfo do
    begin
      if (GridRectMin >= FixedCellCount) and (GridRectMin < FirstGridCell) then
        if GridRectMax < FirstGridCell then
        begin
          ScreenRect := Rect(0, 0, 0, 0);
          Exit;
        end
        else
          GridRectMin := FirstGridCell;
      if GridRectMax > LastFullVisibleCell then
      begin
        GridRectMax := LastFullVisibleCell;
        if GridRectMax < GridCellCount - 1 then Inc(GridRectMax);
        if LinePos(AxisInfo, GridRectMax) = 0 then
          Dec(GridRectMax);
      end;

      ScreenRectMin := LinePos(AxisInfo, GridRectMin);
      ScreenRectMax := LinePos(AxisInfo, GridRectMax);
      if ScreenRectMax = 0 then
        ScreenRectMax := ScreenRectMin + AxisInfo.GetExtent(GridRectMin)
      else
        Inc(ScreenRectMax, AxisInfo.GetExtent(GridRectMax));
      if ScreenRectMax > GridExtent then
        ScreenRectMax := GridExtent;
      if IncludeLine then Inc(ScreenRectMax, EffectiveLineWidth);
    end;
    Result := True;
  end;

var
  DrawInfo: TscGridDrawInfo;
  Hold: Integer;
begin
  ScreenRect := Rect(0, 0, 0, 0);
  if (GridRect.Left > GridRect.Right) or (GridRect.Top > GridRect.Bottom) then
    Exit;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if GridRect.Left > Horz.LastFullVisibleCell + 1 then Exit;
    if GridRect.Top > Vert.LastFullVisibleCell + 1 then Exit;

    if CalcAxis(Horz, GridRect.Left, GridRect.Right, ScreenRect.Left,
      ScreenRect.Right) then
    begin
      CalcAxis(Vert, GridRect.Top, GridRect.Bottom, ScreenRect.Top,
        ScreenRect.Bottom);
    end;
  end;
  if (BidiMode = bdRightToLeft) and (Canvas.CanvasOrientation = coLeftToRight) then
  begin
    Hold := ScreenRect.Left;
    ScreenRect.Left := ClientWidth - ScreenRect.Right;
    ScreenRect.Right := ClientWidth - Hold;
  end;
end;

procedure TscCustomGrid.Initialize;
begin
  FTopLeft.X := FixedCols;
  FTopLeft.Y := FixedRows;
  FCurrent := FTopLeft;
  FAnchor := FCurrent;
  if scgoRowSelect in Options then
    FAnchor.X := ColCount - 1;
end;

procedure TscCustomGrid.InvalidateCell(ACol, ARow: Longint);
var
  Rect: TscGridRect;
begin
  if FEditorMode and (FInplaceCol = ACol) and (FInplaceRow = ARow)
  then
    Exit
  else
  if FFullRePaint then
    RePaintControl
  else
  begin
    Rect.Top := ARow;
    Rect.Left := ACol;
    Rect.Bottom := ARow;
    Rect.Right := ACol;
    InvalidateRect(Rect);
  end;
end;

procedure TscCustomGrid.InvalidateCol(ACol: Longint);
var
  Rect: TscGridRect;
begin
  if not HandleAllocated then Exit;
  Rect.Top := 0;
  Rect.Left := ACol;
  Rect.Bottom := VisibleRowCount+1;
  Rect.Right := ACol;
  InvalidateRect(Rect);
end;

procedure TscCustomGrid.InvalidateRow(ARow: Longint);
var
  Rect: TscGridRect;
begin
  if not HandleAllocated then Exit;
  Rect.Top := ARow;
  Rect.Left := 0;
  Rect.Bottom := ARow;
  Rect.Right := VisibleColCount+1;
  InvalidateRect(Rect);
end;

procedure TscCustomGrid.InvalidateGrid;
begin
  if FFullRePaint then
    RePaintControl
  else
    Invalidate;
end;

procedure TscCustomGrid.InvalidateRect(ARect: TscGridRect);
var
  InvalidRect: TRect;
begin
  if not HandleAllocated then Exit;
  GridRectToScreenRect(ARect, InvalidRect, True);
  Winapi.Windows.InvalidateRect(Handle, InvalidRect, False);
end;

function TscCustomGrid.IsTouchPropertyStored(AProperty: TTouchProperty): Boolean;
begin
  Result := inherited IsTouchPropertyStored(AProperty);
  case AProperty of
    tpInteractiveGestures:
      Result := Touch.InteractiveGestures <> [igPan, igPressAndTap];
    tpInteractiveGestureOptions:
      Result := Touch.InteractiveGestureOptions <> [igoPanInertia,
        igoPanSingleFingerHorizontal, igoPanSingleFingerVertical,
        igoPanGutter, igoParentPassthrough];
  end;
end;

procedure TscCustomGrid.ModifyScrollBar(ScrollBar, ScrollCode, Pos: Cardinal);
var
  NewTopLeft, MaxTopLeft: TscGridCoord;
  DrawInfo: TscGridDrawInfo;
  RTLFactor: Integer;

  function Min: Longint;
  begin
    if ScrollBar = SB_HORZ then Result := FixedCols
    else Result := FixedRows;
  end;

  function Max: Longint;
  begin
    if ScrollBar = SB_HORZ then Result := MaxTopLeft.X
    else Result := MaxTopLeft.Y;
  end;

  function PageUp: Longint;
  var
    MaxTopLeft: TscGridCoord;
  begin
    MaxTopLeft := CalcMaxTopLeft(FTopLeft, DrawInfo);
    if ScrollBar = SB_HORZ then
      Result := FTopLeft.X - MaxTopLeft.X else
      Result := FTopLeft.Y - MaxTopLeft.Y;
    if Result < 1 then Result := 1;
  end;

  function PageDown: Longint;
  var
    DrawInfo: TscGridDrawInfo;
  begin
    CalcDrawInfo(DrawInfo);
    with DrawInfo do
      if ScrollBar = SB_HORZ then
        Result := Horz.LastFullVisibleCell - FTopLeft.X else
        Result := Vert.LastFullVisibleCell - FTopLeft.Y;
    if Result < 1 then Result := 1;
  end;

  function CalcScrollBar(Value, ARTLFactor: Longint): Longint;
  begin
    Result := Value;
    case ScrollCode of
      SB_LINEUP:
        Dec(Result, ARTLFactor);
      SB_LINEDOWN:
        Inc(Result, ARTLFactor);
      SB_PAGEUP:
        Dec(Result, PageUp * ARTLFactor);
      SB_PAGEDOWN:
        Inc(Result, PageDown * ARTLFactor);
      SB_THUMBPOSITION, SB_THUMBTRACK:
        if (scgoThumbTracking in Options) or (ScrollCode = SB_THUMBPOSITION) then
        begin
          if (BidiMode <> bdRightToLeft) or (ARTLFactor = 1) then
            Result := Min + LongMulDiv(Pos, Max - Min, SC_MaxShortInt)
          else
            Result := Max - LongMulDiv(Pos, Max - Min, SC_MaxShortInt);
        end;
      SB_BOTTOM:
        Result := Max;
      SB_TOP:
        Result := Min;
    end;
  end;

  procedure ModifyPixelScrollBar(Code, Pos: Cardinal);
  var
    NewOffset: Integer;
    OldOffset: Integer;
    R: TscGridRect;
    GridSpace, ColWidth: Integer;
  begin
    NewOffset := FColOffset;
    ColWidth := ColWidths[DrawInfo.Horz.FirstGridCell];
    GridSpace := ClientWidth - DrawInfo.Horz.FixedBoundary;
    case Code of
      SB_LINEUP: Dec(NewOffset, Canvas.TextWidth('0') * RTLFactor);
      SB_LINEDOWN: Inc(NewOffset, Canvas.TextWidth('0') * RTLFactor);
      SB_PAGEUP: Dec(NewOffset, GridSpace * RTLFactor);
      SB_PAGEDOWN: Inc(NewOffset, GridSpace * RTLFactor);
      SB_THUMBPOSITION,
      SB_THUMBTRACK:
        if (scgoThumbTracking in Options) or (Code = SB_THUMBPOSITION) then
        begin
          if BidiMode <> bdRightToLeft then
            NewOffset := Pos
          else
            NewOffset := Max - Integer(Pos);
        end;
      SB_BOTTOM: NewOffset := 0;
      SB_TOP: NewOffset := ColWidth - GridSpace;
    end;
    if NewOffset < 0 then
      NewOffset := 0
    else if NewOffset >= ColWidth - GridSpace then
      NewOffset := ColWidth - GridSpace;
    if NewOffset <> FColOffset then
    begin
      OldOffset := FColOffset;
      FColOffset := NewOffset;
      ScrollData(OldOffset - NewOffset, 0);
      FillChar(R, SizeOf(R), 0);
      R.Bottom := FixedRows;
      InvalidateRect(R);
      Update;
      UpdateScrollPos;
    end;
  end;

var
  Temp: Longint;
begin
  if BidiMode <> bdRightToLeft then
    RTLFactor := 1
  else
    RTLFactor := -1;

  if Visible and CanFocus and TabStop and not (csDesigning in ComponentState) then
    SetFocus;
  CalcDrawInfo(DrawInfo);
  if (ScrollBar = SB_HORZ) and (ColCount = 1) then
  begin
    ModifyPixelScrollBar(ScrollCode, Pos);
    Exit;
  end;
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  NewTopLeft := FTopLeft;
  if ScrollBar = SB_HORZ then
    repeat
      Temp := NewTopLeft.X;
      NewTopLeft.X := CalcScrollBar(NewTopLeft.X, RTLFactor);
    until (NewTopLeft.X <= FixedCols) or (NewTopLeft.X >= MaxTopLeft.X)
      or (ColWidths[NewTopLeft.X] > 0) or (Temp = NewTopLeft.X)
  else
    repeat
      Temp := NewTopLeft.Y;
      NewTopLeft.Y := CalcScrollBar(NewTopLeft.Y, 1);
    until (NewTopLeft.Y <= FixedRows) or (NewTopLeft.Y >= MaxTopLeft.Y)
      or (RowHeights[NewTopLeft.Y] > 0) or (Temp = NewTopLeft.Y);
  NewTopLeft.X := System.Math.Max(FixedCols, System.Math.Min(MaxTopLeft.X, NewTopLeft.X));
  NewTopLeft.Y := System.Math.Max(FixedRows, System.Math.Min(MaxTopLeft.Y, NewTopLeft.Y));
  if (NewTopLeft.X <> FTopLeft.X) or (NewTopLeft.Y <> FTopLeft.Y) then
    MoveTopLeft(NewTopLeft.X, NewTopLeft.Y);
end;

procedure TscCustomGrid.MoveAdjust(var CellPos: Longint; FromIndex, ToIndex: Longint);
var
  Min, Max: Longint;
begin
  if CellPos = FromIndex then CellPos := ToIndex
  else
  begin
    Min := FromIndex;
    Max := ToIndex;
    if FromIndex > ToIndex then
    begin
      Min := ToIndex;
      Max := FromIndex;
    end;
    if (CellPos >= Min) and (CellPos <= Max) then
      if FromIndex > ToIndex then
        Inc(CellPos) else
        Dec(CellPos);
  end;
end;

procedure TscCustomGrid.MoveAnchor(const NewAnchor: TscGridCoord);
var
  OldSel: TscGridRect;
begin
  if [scgoRangeSelect, scgoEditing] * Options = [scgoRangeSelect] then
  begin
    OldSel := Selection;
    FAnchor := NewAnchor;
    if scgoRowSelect in Options then FAnchor.X := ColCount - 1;
    ClampInView(NewAnchor);
    SelectionMoved(OldSel);
  end
  else MoveCurrent(NewAnchor.X, NewAnchor.Y, True, True);
end;

procedure TscCustomGrid.MoveCurrent(ACol, ARow: Longint; MoveAnchor,
  Show: Boolean);
var
  OldSel: TscGridRect;
  OldCurrent: TscGridCoord;
begin
  if (ACol < 0) or (ARow < 0) or (ACol >= ColCount) or (ARow >= RowCount) then
    InvalidOp(SIndexOutOfRange);
  if SelectCell(ACol, ARow) then
  begin
    OldSel := Selection;
    OldCurrent := FCurrent;
    FCurrent.X := ACol;
    FCurrent.Y := ARow;
    if not (scgoAlwaysShowEditor in Options) then HideEditor;
    if MoveAnchor or not (scgoRangeSelect in Options) then
    begin
      FAnchor := FCurrent;
      if scgoRowSelect in Options then FAnchor.X := ColCount - 1;
    end;
    if scgoRowSelect in Options then FCurrent.X := FixedCols;
    if Show then ClampInView(FCurrent);
    SelectionMoved(OldSel);
    with OldCurrent do InvalidateCell(X, Y);
    with FCurrent do InvalidateCell(ACol, ARow);
  end;
end;

procedure TscCustomGrid.MoveTopLeft(ALeft, ATop: Longint);
var
  OldTopLeft: TscGridCoord;
begin
  if (ALeft = FTopLeft.X) and (ATop = FTopLeft.Y) then Exit;
  Update;
  OldTopLeft := FTopLeft;
  FTopLeft.X := ALeft;
  FTopLeft.Y := ATop;
  TopLeftMoved(OldTopLeft);
end;

procedure TscCustomGrid.ResizeCol(Index: Longint; OldSize, NewSize: Integer);
begin
  InvalidateGrid;
end;

procedure TscCustomGrid.ResizeRow(Index: Longint; OldSize, NewSize: Integer);
begin
  InvalidateGrid;
end;

procedure TscCustomGrid.SelectionMoved(const OldSel: TscGridRect);
var
  OldRect, NewRect: TRect;
  AXorRects: TXorRects;
  I: Integer;
begin
  if not HandleAllocated then Exit;
  GridRectToScreenRect(OldSel, OldRect, True);
  GridRectToScreenRect(Selection, NewRect, True);
  XorRects(OldRect, NewRect, AXorRects);
  for I := Low(AXorRects) to High(AXorRects) do
    Winapi.Windows.InvalidateRect(Handle, AXorRects[I], False);
end;

procedure TscCustomGrid.ScrollDataInfo(DX, DY: Integer;
  var DrawInfo: TscGridDrawInfo);
var
  ScrollArea: TRect;
  ScrollFlags: Integer;
begin
  with DrawInfo do
  begin
    ScrollFlags := SW_INVALIDATE;
    if not DefaultDrawing then
      ScrollFlags := ScrollFlags or SW_ERASE;
    if DY = 0 then
    begin
      if BidiMode <> bdRightToLeft then
        ScrollArea := Rect(Horz.FixedBoundary, 0, Horz.GridExtent, Vert.GridExtent)
      else
      begin
        ScrollArea := Rect(ClientWidth - Horz.GridExtent, 0, ClientWidth - Horz.FixedBoundary, Vert.GridExtent);
        DX := -DX;
      end;
      ScrollWindowEx(Handle, DX, 0, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
    end
    else if DX = 0 then
    begin
      ScrollArea := Rect(0, Vert.FixedBoundary, Horz.GridExtent, Vert.GridExtent);
      ScrollWindowEx(Handle, 0, DY, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
    end
    else
    begin
      ScrollArea := Rect(Horz.FixedBoundary, 0, Horz.GridExtent, Vert.FixedBoundary);
      ScrollWindowEx(Handle, DX, 0, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
      ScrollArea := Rect(0, Vert.FixedBoundary, Horz.FixedBoundary, Vert.GridExtent);
      ScrollWindowEx(Handle, 0, DY, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
      ScrollArea := Rect(Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridExtent,
        Vert.GridExtent);
      ScrollWindowEx(Handle, DX, DY, ScrollArea, ScrollArea, 0, nil, ScrollFlags);
    end;
  end;
  if scgoRowSelect in Options then
    InvalidateRect(Selection);
end;

procedure TscCustomGrid.ScrollData(DX, DY: Integer);
var
  DrawInfo: TscGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Perform(WM_SETREDRAW, 0, 0);
  ScrollDataInfo(DX, DY, DrawInfo);
  Perform(WM_SETREDRAW, 1, 0);
  RePaintControl;
end;

procedure TscCustomGrid.TopLeftMoved(const OldTopLeft: TscGridCoord);

  function CalcScroll(const AxisInfo: TscGridAxisDrawInfo;
    OldPos, CurrentPos: Integer; var Amount: Longint): Boolean;
  var
    Start, Stop: Longint;
    I: Longint;
  begin
    Result := False;
    with AxisInfo do
    begin
      if OldPos < CurrentPos then
      begin
        Start := OldPos;
        Stop := CurrentPos;
      end
      else
      begin
        Start := CurrentPos;
        Stop := OldPos;
      end;
      Amount := 0;
      for I := Start to Stop - 1 do
      begin
        Inc(Amount, AxisInfo.GetExtent(I) + EffectiveLineWidth);
        if Amount > (GridBoundary - FixedBoundary) then
        begin
          InvalidateGrid;
          Exit;
        end;
      end;
      if OldPos < CurrentPos then Amount := -Amount;
    end;
    Result := True;
  end;

var
  DrawInfo: TscGridDrawInfo;
  Delta: TscGridCoord;
begin
  UpdateScrollPos;
  CalcDrawInfo(DrawInfo);
  if CalcScroll(DrawInfo.Horz, OldTopLeft.X, FTopLeft.X, Delta.X) and
    CalcScroll(DrawInfo.Vert, OldTopLeft.Y, FTopLeft.Y, Delta.Y) then
  begin
    RePaintControl;
  end;
  TopLeftChanged;
end;

procedure TscCustomGrid.UpdateScrollPos;
var
  DrawInfo: TscGridDrawInfo;
  MaxTopLeft: TscGridCoord;
  GridSpace, ColWidth: Integer;

  procedure SetScroll(Code: Word; Value: Integer);
  begin
    if (BidiMode = bdRightToLeft) and (Code = SB_HORZ) then
      if ColCount <> 1 then Value := SC_MaxShortInt - Value else
      Value := (ColWidth - GridSpace) - Value;
    if GetScrollPos(Handle, Code) <> Value then
      SetScrollPos(Handle, Code, Value, True);
  end;

begin
  if (not HandleAllocated) or (ScrollBars = ssNone) then Exit;
  CalcDrawInfo(DrawInfo);
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  if ScrollBars in [ssHorizontal, ssBoth] then
    if ColCount = 1 then
    begin
      ColWidth := ColWidths[DrawInfo.Horz.FirstGridCell];
      GridSpace := ClientWidth - DrawInfo.Horz.FixedBoundary;
      if (FColOffset > 0) and (GridSpace > (ColWidth - FColOffset)) then
        ModifyScrollbar(SB_HORZ, SB_THUMBPOSITION, ColWidth - GridSpace)
      else
        SetScroll(SB_HORZ, FColOffset)
    end
    else
      SetScroll(SB_HORZ, LongMulDiv(FTopLeft.X - FixedCols, SC_MaxShortInt,
        MaxTopLeft.X - FixedCols));
  if ScrollBars in [ssVertical, ssBoth] then
    SetScroll(SB_VERT, LongMulDiv(FTopLeft.Y - FixedRows, SC_MaxShortInt,
      MaxTopLeft.Y - FixedRows));
end;

procedure TscCustomGrid.UpdateScrollRange;
var
  MaxTopLeft, OldTopLeft: TscGridCoord;
  DrawInfo: TscGridDrawInfo;
  OldScrollBars: {$IFNDEF VER230}System.UITypes.{$ENDIF}TScrollStyle;
  Updated: Boolean;

  procedure DoUpdate;
  begin
    if not Updated then
    begin
      Update;
      Updated := True;
    end;
  end;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Min, Max: Integer;
  begin
    Result := False;
    if (ScrollBars = ssBoth) or
      ((Code = SB_HORZ) and (ScrollBars = ssHorizontal)) or
      ((Code = SB_VERT) and (ScrollBars = ssVertical)) then
    begin
      GetScrollRange(Handle, Code, Min, Max);
      Result := Min <> Max;
    end;
  end;

  procedure CalcSizeInfo;
  begin
    CalcDrawInfoXY(DrawInfo, DrawInfo.Horz.GridExtent, DrawInfo.Vert.GridExtent);
    MaxTopLeft.X := ColCount - 1;
    MaxTopLeft.Y := RowCount - 1;
    MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  end;

  procedure SetAxisRange(var Max, Old, Current: Longint; Code: Word;
    Fixeds: Integer);
  begin
    CalcSizeInfo;
    if Fixeds < Max then
      SetScrollRange(Handle, Code, 0, SC_MaxShortInt, True)
    else
      SetScrollRange(Handle, Code, 0, 0, True);
    if Old > Max then
    begin
      DoUpdate;
      Current := Max;
    end;
  end;

  procedure SetHorzRange;
  var
    Range: Integer;
  begin
    if OldScrollBars in [ssHorizontal, ssBoth] then
      if ColCount = 1 then
      begin
        Range := ColWidths[0] - ClientWidth;
        if Range < 0 then Range := 0;
        SetScrollRange(Handle, SB_HORZ, 0, Range, True);
      end
      else
        SetAxisRange(MaxTopLeft.X, OldTopLeft.X, FTopLeft.X, SB_HORZ, FixedCols);
  end;

  procedure SetVertRange;
  begin
    if OldScrollBars in [ssVertical, ssBoth] then
      SetAxisRange(MaxTopLeft.Y, OldTopLeft.Y, FTopLeft.Y, SB_VERT, FixedRows);
  end;

begin
  if (ScrollBars = ssNone) or not HandleAllocated or not Showing then Exit;
  with DrawInfo do
  begin
    Horz.GridExtent := ClientWidth;
    Vert.GridExtent := ClientHeight;
    if ScrollBarVisible(SB_HORZ) then
      Inc(Vert.GridExtent, GetSystemMetrics(SM_CYHSCROLL));
    if ScrollBarVisible(SB_VERT) then
      Inc(Horz.GridExtent, GetSystemMetrics(SM_CXVSCROLL));
  end;
  OldTopLeft := FTopLeft;
  OldScrollBars := FScrollBars;
  FScrollBars := ssNone;
  Updated := False;
  try
    SetHorzRange;
    DrawInfo.Vert.GridExtent := ClientHeight;
    SetVertRange;
    if DrawInfo.Horz.GridExtent <> ClientWidth then
    begin
      DrawInfo.Horz.GridExtent := ClientWidth;
      SetHorzRange;
    end;
  finally
    FScrollBars := OldScrollBars;
  end;
  UpdateScrollPos;
  if (FTopLeft.X <> OldTopLeft.X) or (FTopLeft.Y <> OldTopLeft.Y) then
    TopLeftMoved(OldTopLeft);
end;

function TscCustomGrid.CreateEditor: TscInplaceEdit;
begin
  Result := TscInplaceEdit.Create(Self);
end;

procedure TscCustomGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or WS_CLIPCHILDREN;
    Style := Style and not (CS_HREDRAW or CS_VREDRAW);
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
    WindowClass.style := CS_DBLCLKS;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TscCustomGrid.CreateWnd;
begin
  inherited;
  FInternalDrawingStyle := FDrawingStyle;
  if (FDrawingStyle = scgdsThemed) and not ThemeControl(Self) then
    FInternalDrawingStyle := scgdsClassic;
end;

procedure TscCustomGrid.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
const
  VertScrollFlags: array[Boolean] of Integer = (SB_LINEDOWN, SB_LINEUP);
  HorizScrollFlags: array[Boolean] of Integer = (SB_LINERIGHT, SB_LINELEFT);
var
  I, LColWidth, LCols, LRowHeight, LRows, DeltaX, DeltaY: Integer;
begin
  if EventInfo.GestureID = igiPan then
  begin
    Handled := True;
    if gfBegin in EventInfo.Flags then
      FPanPoint := EventInfo.Location
    else if not (gfEnd in EventInfo.Flags) then
    begin
      // Vertical panning
      DeltaY := EventInfo.Location.Y - FPanPoint.Y;
      if Abs(DeltaY) > 1 then
      begin
        LRowHeight := RowHeights[TopRow];
        LRows := Abs(DeltaY) div LRowHeight;
        if (Abs(DeltaY) mod LRowHeight = 0) or (LRows > 0) then
        begin
          for I := 0 to LRows - 1 do
            ModifyScrollBar(SB_VERT, VertScrollFlags[DeltaY > 0], 0);
          FPanPoint := EventInfo.Location;
          Inc(FPanPoint.Y, DeltaY mod LRowHeight);
        end;
      end
      else
      begin
        // Horizontal panning
        DeltaX := EventInfo.Location.X - FPanPoint.X;
        if Abs(DeltaX) > 1 then
        begin
          LColWidth := ColWidths[LeftCol];
          LCols := Abs(DeltaX) div LColWidth;
          if (Abs(DeltaX) mod LColWidth = 0) or (LCols > 0) then
          begin
            for I := 0 to LCols - 1 do
              ModifyScrollBar(SB_HORZ, HorizScrollFlags[DeltaX > 0], 0);
            FPanPoint := EventInfo.Location;
            Inc(FPanPoint.X, DeltaX mod LColWidth);
          end;
        end;
      end;
                                            
    end;
  end;
end;

procedure TscCustomGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewTopLeft, NewCurrent, MaxTopLeft: TscGridCoord;
  DrawInfo: TscGridDrawInfo;
  PageWidth, PageHeight: Integer;
  RTLFactor: Integer;
  NeedsInvalidating: Boolean;
  LPosChanged: Boolean;

  procedure CalcPageExtents;
  begin
    CalcDrawInfo(DrawInfo);
    PageWidth := DrawInfo.Horz.LastFullVisibleCell - LeftCol;
    if PageWidth < 1 then PageWidth := 1;
    PageHeight := DrawInfo.Vert.LastFullVisibleCell - TopRow;
    if PageHeight < 1 then PageHeight := 1;
  end;

  procedure Restrict(var Coord: TscGridCoord; MinX, MinY, MaxX, MaxY: Longint);
  begin
    with Coord do
    begin
      if X > MaxX then X := MaxX
      else if X < MinX then X := MinX;
      if Y > MaxY then Y := MaxY
      else if Y < MinY then Y := MinY;
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  if Observers.IsObserving(TObserverMapping.EditGridLinkID) then
    if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
      if TLinkObservers.EditGridLinkEdit(Observers) then
        TLinkObservers.EditGridLinkModified(Observers);

  NeedsInvalidating := False;
  if not CanGridAcceptKey(Key, Shift) then Key := 0;

  if BidiMode <> bdRightToLeft then
    RTLFactor := 1
  else
    RTLFactor := -1;

  NewCurrent := FCurrent;
  NewTopLeft := FTopLeft;
  CalcPageExtents;
  if ssCtrl in Shift then
    case Key of
      VK_UP: Dec(NewTopLeft.Y);
      VK_DOWN: Inc(NewTopLeft.Y);
      VK_LEFT:
        if not (scgoRowSelect in Options) then
        begin
          Dec(NewCurrent.X, PageWidth * RTLFactor);
          Dec(NewTopLeft.X, PageWidth * RTLFactor);
        end;
      VK_RIGHT:
        if not (scgoRowSelect in Options) then
        begin
          Inc(NewCurrent.X, PageWidth * RTLFactor);
          Inc(NewTopLeft.X, PageWidth * RTLFactor);
        end;
      VK_PRIOR: NewCurrent.Y := TopRow;
      VK_NEXT: NewCurrent.Y := DrawInfo.Vert.LastFullVisibleCell;
      VK_HOME:
        begin
          NewCurrent.X := FixedCols;
          NewCurrent.Y := FixedRows;
          NeedsInvalidating := BidiMode = bdRightToLeft;
        end;
      VK_END:
        begin
          NewCurrent.X := ColCount - 1;
          NewCurrent.Y := RowCount - 1;
          NeedsInvalidating := BidiMode = bdRightToLeft;
        end;
    end
  else
    case Key of
      VK_UP: Dec(NewCurrent.Y);
      VK_DOWN: Inc(NewCurrent.Y);
      VK_LEFT:
        if scgoRowSelect in Options then
          Dec(NewCurrent.Y, RTLFactor) else
          Dec(NewCurrent.X, RTLFactor);
      VK_RIGHT:
        if scgoRowSelect in Options then
          Inc(NewCurrent.Y, RTLFactor) else
          Inc(NewCurrent.X, RTLFactor);
      VK_NEXT:
        begin
          Inc(NewCurrent.Y, PageHeight);
          Inc(NewTopLeft.Y, PageHeight);
        end;
      VK_PRIOR:
        begin
          Dec(NewCurrent.Y, PageHeight);
          Dec(NewTopLeft.Y, PageHeight);
        end;
      VK_HOME:
        if scgoRowSelect in Options then
          NewCurrent.Y := FixedRows else
          NewCurrent.X := FixedCols;
      VK_END:
        if scgoRowSelect in Options then
          NewCurrent.Y := RowCount - 1 else
          NewCurrent.X := ColCount - 1;
      VK_TAB:
        if not (ssAlt in Shift) then
        repeat
          if ssShift in Shift then
          begin
            Dec(NewCurrent.X);
            if NewCurrent.X < FixedCols then
            begin
              NewCurrent.X := ColCount - 1;
              Dec(NewCurrent.Y);
              if NewCurrent.Y < FixedRows then NewCurrent.Y := RowCount - 1;
            end;
            Shift := [];
          end
          else
          begin
            Inc(NewCurrent.X);
            if NewCurrent.X >= ColCount then
            begin
              NewCurrent.X := FixedCols;
              Inc(NewCurrent.Y);
              if NewCurrent.Y >= RowCount then NewCurrent.Y := FixedRows;
            end;
          end;
        until TabStops[NewCurrent.X] or (NewCurrent.X = FCurrent.X);
      VK_F2: EditorMode := True;
    end;
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  Restrict(NewTopLeft, FixedCols, FixedRows, MaxTopLeft.X, MaxTopLeft.Y);
  if (NewTopLeft.X <> LeftCol) or (NewTopLeft.Y <> TopRow) then
    MoveTopLeft(NewTopLeft.X, NewTopLeft.Y);
  Restrict(NewCurrent, FixedCols, FixedRows, ColCount - 1, RowCount - 1);

  if Observers.IsObserving(TObserverMapping.EditGridLinkID) then
    if TLinkObservers.EditGridLinkIsEditing(Observers) then
    begin
      try
        TLinkObservers.EditGridLinkUpdate(Observers);
      except
        TLinkObservers.EditGridLinkReset(Observers);
        SetFocus;
        raise;
      end;
    end;

  if (NewCurrent.X <> Col) or (NewCurrent.Y <> Row) then
  begin
     LPosChanged := NewCurrent.Y <> Row;
     {$IFDEF VER250_UP}
     if LPosChanged then
       TLinkObservers.PositionLinkPosChanging(Observers);
     {$ENDIF}
     FocusCell(NewCurrent.X, NewCurrent.Y, not (ssShift in Shift));
     if LPosChanged then
       TLinkObservers.PositionLinkPosChanged(Observers);
  end;
  if NeedsInvalidating then RePaintControl;
end;

procedure TscCustomGrid.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FEditorMode and Observers.IsObserving(TObserverMapping.EditGridLinkID) then
  begin
    if (Key >= #32) and
      not TLinkObservers.EditGridLinkIsValidChar(Observers, Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^V, ^X, #32..High(Char):
        if not TLinkObservers.EditGridLinkEdit(Observers) then
          Key := #0
        else
          TLinkObservers.EditGridLinkModified(Observers);
      #27:
        begin
          if TLinkObservers.EditGridLinkIsEditing(Observers) then
          begin
            TLinkObservers.EditGridLinkReset(Observers);
          end;
          Key := #0;
        end;
      #$D: //vkReturn
        begin
          if TLinkObservers.EditGridLinkIsEditing(Observers) then
          begin
            try
              TLinkObservers.EditGridLinkUpdate(Observers);
            except
              TLinkObservers.EditGridLinkReset(Observers);
              SetFocus;
              raise;
            end;
          end;
        end;
    end;
  end;

  if not (scgoAlwaysShowEditor in Options) and (Key = #13) then
  begin
    if FEditorMode then
      HideEditor else
      ShowEditor;
    Key := #0;
  end;
end;

procedure TscCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CellHit: TscGridCoord;
  DrawInfo: TscGridDrawInfo;
  MoveDrawn: Boolean;
  LPosChanged: Boolean;
begin
  MoveDrawn := False;
  if FEditorMode and (FInplaceEdit <> nil) and
     FInplaceEdit.Transparent
  then
    begin
    end
  else
    HideEdit;
  if not (csDesigning in ComponentState) and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    SetFocus;
    if not IsActiveControl then
    begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if (Button = mbLeft) and (ssDouble in Shift) then
    DblClick
  else if Button = mbLeft then
  begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, FGridState, FSizingIndex, FSizingPos, FSizingOfs,
      DrawInfo);
    if FGridState <> scgsNormal then
    begin
      if (FGridState = scgsColSizing) and (BidiMode = bdRightToLeft) then
        FSizingPos := ClientWidth - FSizingPos;
      DrawSizingLine(DrawInfo);
      Exit;
    end;
    CellHit := CalcCoordFromPoint(X, Y, DrawInfo);
    if (CellHit.X >= FixedCols) and (CellHit.Y >= FixedRows) then
    begin
      if scgoEditing in Options then
      begin
        if (CellHit.X = FCurrent.X) and (CellHit.Y = FCurrent.Y) then
          ShowEditor
        else
        begin
          LPosChanged := CellHit.Y <> FCurrent.Y;
          if Observers.IsObserving(TObserverMapping.EditGridLinkID) then
            if TLinkObservers.EditGridLinkIsEditing(Observers) then
              try
                TLinkObservers.EditGridLinkUpdate(Observers);
              except
                TLinkObservers.EditGridLinkReset(Observers);
                raise;
              end;
          {$IFDEF VER250_UP}
          if LPosChanged then
            TLinkObservers.PositionLinkPosChanging(Observers);
          {$ENDIF}
          MoveCurrent(CellHit.X, CellHit.Y, True, True);
          UpdateEdit;
          if LPosChanged then
            TLinkObservers.PositionLinkPosChanged(Observers);
        end;
        Click;
      end
      else
      begin
        FGridState := scgsSelecting;
        SetTimer(Handle, 1, 60, nil);
        if ssShift in Shift then
          MoveAnchor(CellHit)
        else
        begin
          if Observers.IsObserving(TObserverMapping.EditGridLinkID) then
            if TLinkObservers.EditGridLinkIsEditing(Observers) then
              try
                TLinkObservers.EditGridLinkUpdate(Observers);
              except
                TLinkObservers.EditGridLinkReset(Observers);
                raise;
              end;
          LPosChanged := CellHit.Y <> FCurrent.Y;
          {$IFDEF VER250_UP}
          if LPosChanged then
            TLinkObservers.PositionLinkPosChanging(Observers);
          {$ENDIF}
          MoveCurrent(CellHit.X, CellHit.Y, True, True);
          if LPosChanged then
            TLinkObservers.PositionLinkPosChanged(Observers);
        end;
      end;
    end
    else
    begin
      if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
      begin
        FHotTrackCell.Pressed := True;
        FHotTrackCell.Button := Button;
        InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
      end;

      if (scgoRowMoving in Options) and (CellHit.X >= 0) and
        (CellHit.X < FixedCols) and (CellHit.Y >= FixedRows) then
      begin
        FMoveIndex := CellHit.Y;
        FMovePos := FMoveIndex;
        if BeginRowDrag(FMoveIndex, FMovePos, Point(X,Y)) then
        begin
          FGridState := scgsRowMoving;
          Update;
          DrawMove;
          MoveDrawn := True;
          SetTimer(Handle, 1, 60, nil);
        end;
      end
      else if (scgoColMoving in Options) and (CellHit.Y >= 0) and
        (CellHit.Y < FixedRows) and (CellHit.X >= FixedCols) then
      begin
        FMoveIndex := CellHit.X;
        FMovePos := FMoveIndex;
        if BeginColumnDrag(FMoveIndex, FMovePos, Point(X,Y)) then
        begin
          FGridState := scgsColMoving;
          Update;
          DrawMove;
          MoveDrawn := True;
          SetTimer(Handle, 1, 60, nil);
        end;
      end;
    end;
  end;
  try
    inherited MouseDown(Button, Shift, X, Y);
  except
    if MoveDrawn then DrawMove;
  end;
end;

procedure TscCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  DrawInfo: TscGridDrawInfo;
  CellHit: TscGridCoord;
begin
  CalcDrawInfo(DrawInfo);
  case FGridState of
    scgsSelecting, scgsColMoving, scgsRowMoving:
      begin
        CellHit := CalcCoordFromPoint(X, Y, DrawInfo);
        if (CellHit.X >= FixedCols) and (CellHit.Y >= FixedRows) and
          (CellHit.X <= DrawInfo.Horz.LastFullVisibleCell+1) and
          (CellHit.Y <= DrawInfo.Vert.LastFullVisibleCell+1) then
          case FGridState of
            scgsSelecting:
              if ((CellHit.X <> FAnchor.X) or (CellHit.Y <> FAnchor.Y)) then
                MoveAnchor(CellHit);
            scgsColMoving:
              MoveAndScroll(X, CellHit.X, DrawInfo, DrawInfo.Horz, SB_HORZ, Point(X,Y));
            scgsRowMoving:
              MoveAndScroll(Y, CellHit.Y, DrawInfo, DrawInfo.Vert, SB_VERT, Point(X,Y));
          end;
      end;
    scgsRowSizing, scgsColSizing:
      begin
        DrawSizingLine(DrawInfo);
        if FGridState = scgsRowSizing then
          FSizingPos := Y + FSizingOfs else
          FSizingPos := X + FSizingOfs;
        DrawSizingLine(DrawInfo);
      end;
    else
    begin
      if (csDesigning in ComponentState) then
        Exit;
      CellHit := CalcCoordFromPoint(X, Y, DrawInfo);
      if ((scgoFixedRowClick in FOptions) and (CellHit.Y < FixedRows)) or
         ((scgoFixedColClick in FOptions) and (CellHit.X < FixedCols)) then
      begin
        if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
          InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
        if (CellHit.X <> FHotTrackCell.Coord.X) or (CellHit.Y <> FHotTrackCell.Coord.Y) then
        begin
          FHotTrackCell.Coord := CellHit;
          FHotTrackCell.Pressed := False;
          InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
        end;
      end
      else
        if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
        begin
          FHotTrackCell.Coord.X := -1;
          FHotTrackCell.Coord.Y := -1;
          FHotTrackCell.Pressed := False;
          RePaintControl;
        end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TscCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DrawInfo: TscGridDrawInfo;
  NewSize: Integer;
  Cell: TscGridCoord;

  function ResizeLine(const AxisInfo: TscGridAxisDrawInfo): Integer;
  var
    I: Integer;
  begin
    with AxisInfo do
    begin
      Result := FixedBoundary;
      for I := FirstGridCell to FSizingIndex - 1 do
        Inc(Result, AxisInfo.GetExtent(I) + EffectiveLineWidth);
      Result := FSizingPos - Result;
    end;
  end;

begin
  try
    case FGridState of
      scgsSelecting:
        begin
          MouseMove(Shift, X, Y);
          KillTimer(Handle, 1);
          UpdateEdit;
          Click;
        end;
      scgsRowSizing, scgsColSizing:
        begin
          CalcDrawInfo(DrawInfo);
          DrawSizingLine(DrawInfo);
          if (FGridState = scgsColSizing) and (BidiMode = bdRightToLeft) then
            FSizingPos := ClientWidth - FSizingPos;
          if FGridState = scgsColSizing then
          begin
            NewSize := ResizeLine(DrawInfo.Horz);
            if NewSize > 1 then
            begin
              ColWidths[FSizingIndex] := NewSize;
              UpdateDesigner;
            end;
          end
          else
          begin
            NewSize := ResizeLine(DrawInfo.Vert);
            if NewSize > 1 then
            begin
              RowHeights[FSizingIndex] := NewSize;
              UpdateDesigner;
            end;
          end;
        end;
      scgsColMoving:
        begin
          DrawMove;
          KillTimer(Handle, 1);
          if EndColumnDrag(FMoveIndex, FMovePos, Point(X,Y))
            and (FMoveIndex <> FMovePos) then
          begin
            MoveColumn(FMoveIndex, FMovePos);
            UpdateDesigner;
          end;
          UpdateEdit;
        end;
      scgsRowMoving:
        begin
          DrawMove;
          KillTimer(Handle, 1);
          if EndRowDrag(FMoveIndex, FMovePos, Point(X,Y))
            and (FMoveIndex <> FMovePos) then
          begin
            MoveRow(FMoveIndex, FMovePos);
            UpdateDesigner;
          end;
          UpdateEdit;
        end;
    else
      UpdateEdit;
      Cell := MouseCoord(X, Y);
      if (Button = mbLeft) and (FHotTrackCell.Coord.X <> -1) and (FHotTrackCell.Coord.Y <> -1) and
         (((scgoFixedColClick in FOptions) and (Cell.X < FFixedCols) and (Cell.X >= 0)) or
         ((scgoFixedRowClick in FOptions) and (Cell.Y < FFixedRows) and (Cell.Y >= 0))) then
        FixedCellClick(Cell.X, Cell.Y);
    end;
    inherited MouseUp(Button, Shift, X, Y);
  finally
    FGridState := scgsNormal;
    FHotTrackCell.Pressed := False;
    InvalidateCell(FHotTrackCell.Coord.X, FHotTrackCell.Coord.Y);
  end;
end;

procedure TscCustomGrid.MoveAndScroll(Mouse, CellHit: Integer;
  var DrawInfo: TscGridDrawInfo; var Axis: TscGridAxisDrawInfo;
  ScrollBar: Integer; const MousePt: TPoint);
begin
  if (BidiMode = bdRightToLeft) and (ScrollBar = SB_HORZ) then
    Mouse := ClientWidth - Mouse;

  if (CellHit <> FMovePos) and
    not((FMovePos = Axis.FixedCellCount) and (Mouse < Axis.FixedBoundary)) and
    not((FMovePos = Axis.GridCellCount-1) and (Mouse > Axis.GridBoundary)) then
  begin
    DrawMove;   // hide the drag line
    if (Mouse < Axis.FixedBoundary) then
    begin
      if (FMovePos > Axis.FixedCellCount) then
      begin
        ModifyScrollbar(ScrollBar, SB_LINEUP, 0);
        Update;
        CalcDrawInfo(DrawInfo);    // this changes contents of Axis var
      end;
      CellHit := Axis.FirstGridCell;
    end
    else if (Mouse >= Axis.FullVisBoundary) then
    begin
      if (FMovePos = Axis.LastFullVisibleCell) and
        (FMovePos < Axis.GridCellCount -1) then
      begin
        ModifyScrollBar(Scrollbar, SB_LINEDOWN, 0);
        Update;
        CalcDrawInfo(DrawInfo);    // this changes contents of Axis var
      end;
      CellHit := Axis.LastFullVisibleCell;
    end
    else if CellHit < 0 then CellHit := FMovePos;
    if ((FGridState = scgsColMoving) and CheckColumnDrag(FMoveIndex, CellHit, MousePt))
      or ((FGridState = scgsRowMoving) and CheckRowDrag(FMoveIndex, CellHit, MousePt)) then
      FMovePos := CellHit;
    DrawMove;
  end;
end;

function TscCustomGrid.GetColWidths(Index: Longint): Integer;
begin
  if (FColWidths = nil) or (Index >= ColCount) then
    Result := DefaultColWidth
  else
    Result := PIntArray(FColWidths)^[Index + 1];
end;

function TscCustomGrid.GetRowHeights(Index: Longint): Integer;
begin
  if (FRowHeights = nil) or (Index >= RowCount) then
    Result := DefaultRowHeight
  else
    Result := PIntArray(FRowHeights)^[Index + 1];
end;

function TscCustomGrid.GetGridWidth: Integer;
var
  DrawInfo: TscGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Horz.GridBoundary;
end;

function TscCustomGrid.GetGridHeight: Integer;
var
  DrawInfo: TscGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Vert.GridBoundary;
end;

function TscCustomGrid.GetSelection: TscGridRect;
begin
  Result := GridRect(FCurrent, FAnchor);
end;

function TscCustomGrid.GetTabStops(Index: Longint): Boolean;
begin
  if FTabStops = nil then Result := True
  else Result := Boolean(PIntArray(FTabStops)^[Index + 1]);
end;

function TscCustomGrid.GetVisibleColCount: Integer;
var
  DrawInfo: TscGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Horz.LastFullVisibleCell - LeftCol + 1;
end;

function TscCustomGrid.GetVisibleRowCount: Integer;
var
  DrawInfo: TscGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Vert.LastFullVisibleCell - TopRow + 1;
end;

procedure TscCustomGrid.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TscCustomGrid.SetCol(Value: Longint);
begin
  if Col <> Value then FocusCell(Value, Row, True);
end;

procedure TscCustomGrid.SetColCount(Value: Longint);
begin
  if FColCount <> Value then
  begin
    if Value < 1 then Value := 1;
    if Value <= FixedCols then FixedCols := Value - 1;
    ChangeSize(Value, RowCount);
    if scgoRowSelect in Options then
    begin
      FAnchor.X := ColCount - 1;
      Invalidate;
    end;
  end;
end;

procedure TscCustomGrid.SetColWidths(Index: Longint; Value: Integer);
begin
  if FColWidths = nil then
    UpdateExtents(FColWidths, ColCount, DefaultColWidth);

  if Index >= ColCount then InvalidOp(SIndexOutOfRange);

  if Value <> PIntArray(FColWidths)^[Index + 1] then
  begin
    ResizeCol(Index, PIntArray(FColWidths)^[Index + 1], Value);
    PIntArray(FColWidths)^[Index + 1] := Value;
    ColWidthsChanged;
  end;
end;

procedure TscCustomGrid.SetDefaultColWidth(Value: Integer);
begin
  UpdateExtents(FColWidths, 0, 0);
  FDefaultColWidth := Value;
  ColWidthsChanged;
  InvalidateGrid;
end;

procedure TscCustomGrid.SetDefaultRowHeight(Value: Integer);
begin
  if FRowHeights <> nil then
    UpdateExtents(FRowHeights, 0, 0);
  FDefaultRowHeight := Value;
  RowHeightsChanged;
  InvalidateGrid;
end;

procedure TscCustomGrid.SetDrawingStyle(const Value: TscGridDrawingStyle);
begin
  if Value <> FDrawingStyle then
  begin
    FDrawingStyle := Value;
    FInternalDrawingStyle := FDrawingStyle;
    if (FDrawingStyle = scgdsThemed) and not ThemeControl(Self) then
      FInternalDrawingStyle := scgdsClassic;
    Repaint;
  end;
end;

procedure TscCustomGrid.SetFixedColor(Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    InvalidateGrid;
  end;
end;

procedure TscCustomGrid.SetFixedCols(Value: Integer);
begin
  if FFixedCols <> Value then
  begin
    if Value < 0 then InvalidOp(SIndexOutOfRange);
    if Value >= ColCount then InvalidOp(SFixedColTooBig);
    FFixedCols := Value;
    Initialize;
    InvalidateGrid;
  end;
end;

procedure TscCustomGrid.SetFixedRows(Value: Integer);
begin
  if FFixedRows <> Value then
  begin
    if Value < 0 then InvalidOp(SIndexOutOfRange);
    if Value >= RowCount then InvalidOp(SFixedRowTooBig);
    FFixedRows := Value;
    Initialize;
    InvalidateGrid;
  end;
end;

procedure TscCustomGrid.SetEditorMode(Value: Boolean);
begin
  if not Value then
    HideEditor
  else
  begin
    ShowEditor;
    if FInplaceEdit <> nil then FInplaceEdit.Deselect;
  end;
end;

procedure TscCustomGrid.SetGradientEndColor(Value: TColor);
begin
  if Value <> FGradientEndColor then
  begin
    FGradientEndColor := Value;
    if HandleAllocated then
      Repaint;
  end;
end;

procedure TscCustomGrid.SetGradientStartColor(Value: TColor);
begin
  if Value <> FGradientStartColor then
  begin
    FGradientStartColor := Value;
    if HandleAllocated then
      Repaint;
  end;
end;

procedure TscCustomGrid.SetGridLineWidth(Value: Integer);
begin
  if FGridLineWidth <> Value then
  begin
    FGridLineWidth := Value;
    InvalidateGrid;
  end;
end;

procedure TscCustomGrid.SetLeftCol(Value: Longint);
begin
  if FTopLeft.X <> Value then MoveTopLeft(Value, TopRow);
end;

procedure TscCustomGrid.SetOptions(Value: TscGridOptions);
begin
  if FOptions <> Value then
  begin
    if scgoRowSelect in Value then
      Exclude(Value, scgoAlwaysShowEditor);
    FOptions := Value;
    if not FEditorMode then
      if scgoAlwaysShowEditor in Value then
        ShowEditor else
        HideEditor;
    if scgoRowSelect in Value then MoveCurrent(Col, Row,  True, False);
    InvalidateGrid;
  end;
end;

procedure TscCustomGrid.SetRow(Value: Longint);
begin
  if Row <> Value then FocusCell(Col, Value, True);
end;

procedure TscCustomGrid.SetRowCount(Value: Longint);
begin
  if FRowCount <> Value then
  begin
    if Value < 1 then Value := 1;
    if Value <= FixedRows then FixedRows := Value - 1;
    ChangeSize(ColCount, Value);
  end;
end;

procedure TscCustomGrid.SetRowHeights(Index: Longint; Value: Integer);
begin
  if FRowHeights = nil then
    UpdateExtents(FRowHeights, RowCount, DefaultRowHeight);
  if Index >= RowCount then InvalidOp(SIndexOutOfRange);
  if Value <> PIntArray(FRowHeights)^[Index + 1] then
  begin
    ResizeRow(Index, PIntArray(FRowHeights)^[Index + 1], Value);
    PIntArray(FRowHeights)^[Index + 1] := Value;
    RowHeightsChanged;
  end;
end;

procedure TscCustomGrid.SetScrollBars(Value: {$IFNDEF VER230}System.UITypes.{$ENDIF}TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TscCustomGrid.SetSelection(Value: TscGridRect);
var
  OldSel: TscGridRect;
begin
  OldSel := Selection;
  FAnchor.X := Value.Left;
  FAnchor.Y := Value.Top;
  FCurrent.X := Value.Right;
  FCurrent.Y := Value.Bottom;
  SelectionMoved(OldSel);
end;

procedure TscCustomGrid.SetTabStops(Index: Longint; Value: Boolean);
begin
  if FTabStops = nil then
    UpdateExtents(FTabStops, ColCount, Integer(True));
  if Index >= ColCount then InvalidOp(SIndexOutOfRange);
  PIntArray(FTabStops)^[Index + 1] := Integer(Value);
end;

procedure TscCustomGrid.SetTopRow(Value: Longint);
begin
  if FTopLeft.Y <> Value then MoveTopLeft(LeftCol, Value);
end;

procedure TscCustomGrid.HideEdit;
begin
  if FInplaceEdit <> nil then
    try
      UpdateText;
    finally
      FInplaceCol := -1;
      FInplaceRow := -1;
      FInplaceEdit.Hide;
    end;
end;

procedure TscCustomGrid.UpdateEdit;

  procedure UpdateEditor;
  begin
    FInplaceCol := Col;
    FInplaceRow := Row;
    FInplaceEdit.UpdateContents;
    if FInplaceEdit.MaxLength = -1 then FCanEditModify := False
    else FCanEditModify := True;
    FInplaceEdit.Color := Self.Color;
    FInplaceEdit.StyleElements := StyleElements;
    FInplaceEdit.Font := Self.Font;
    FInplaceEdit.Transparent := Self.FTransparentEditor;
    if FInplaceEdit.Transparent then
    begin
      FInplaceEdit.StyleElements := StyleElements - [seFont];
      FInplaceEdit.Font.Color := ColorToRGB(FTextColor);
    end;
    //
    FInplaceEdit.SelectAll;
  end;

begin
  if CanEditShow then
  begin
    if FInplaceEdit = nil then
    begin
      FInplaceEdit := CreateEditor;
      FInplaceEdit.SetGrid(Self);
      FInplaceEdit.Parent := Self;
      UpdateEditor;
    end
    else
    begin
      if (Col <> FInplaceCol) or (Row <> FInplaceRow) then
      begin
        HideEdit;
        UpdateEditor;
      end;
    end;
    if CanEditShow then FInplaceEdit.Move(CellRect(Col, Row));
  end
  else
    FEditorMode := False;
end;

procedure TscCustomGrid.UpdateText;
begin
  if (FInplaceCol <> -1) and (FInplaceRow <> -1) then
    SetEditText(FInplaceCol, FInplaceRow, FInplaceEdit.Text);
end;

procedure TscCustomGrid.WMChar(var Msg: TWMChar);
begin
  if (scgoEditing in Options) and (CharInSet(Char(Msg.CharCode), [^H]) or
     (Char(Msg.CharCode) >= #32)) then
    ShowEditorChar(Char(Msg.CharCode))
  else
    inherited;
end;

procedure TscCustomGrid.WMCommand(var Message: TWMCommand);
begin
  with Message do
  begin
    if (FInplaceEdit <> nil) and (Ctl = FInplaceEdit.Handle) then
      case NotifyCode of
        EN_CHANGE: UpdateText;
      end;
  end;
end;

procedure TscCustomGrid.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
  if scgoRowSelect in Options then Exit;
  if scgoTabs in Options then Msg.Result := Msg.Result or DLGC_WANTTAB;
  if scgoEditing in Options then Msg.Result := Msg.Result or DLGC_WANTCHARS;
end;

procedure TscCustomGrid.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  DestroyCaret;  
  InvalidateRect(Selection);
  if (FInplaceEdit <> nil) and (Msg.FocusedWnd <> FInplaceEdit.Handle) then
    HideEdit;
end;

procedure TscCustomGrid.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if FInplaceEdit <> nil then FInplaceEdit.FClickTime := GetMessageTime;
end;

procedure TscCustomGrid.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  FHitTest := ScreenToClient(SmallPointToPoint(Msg.Pos));
end;

procedure TscCustomGrid.WMSetCursor(var Msg: TWMSetCursor);
var
  DrawInfo: TscGridDrawInfo;
  State: TscGridState;
  Index: Longint;
  Pos, Ofs: Integer;
  Cur: HCURSOR;
begin
  Cur := 0;
  with Msg do
  begin
    if HitTest = HTCLIENT then
    begin
      if FGridState = scgsNormal then
      begin
        CalcDrawInfo(DrawInfo);
        CalcSizingState(FHitTest.X, FHitTest.Y, State, Index, Pos, Ofs,
          DrawInfo);
      end else State := FGridState;
      if State = scgsRowSizing then
        Cur := Screen.Cursors[crVSplit]
      else if State = scgsColSizing then
        Cur := Screen.Cursors[crHSplit]
    end;
  end;
  if Cur <> 0 then SetCursor(Cur)
  else inherited;
end;

procedure TscCustomGrid.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  CreateCaret(Handle, 0, 0, 0);
  if (FInplaceEdit = nil) or (Msg.FocusedWnd <> FInplaceEdit.Handle) then
  begin
    InvalidateRect(Selection);
    UpdateEdit;
  end;
end;

procedure TscCustomGrid.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateScrollRange;
  RePaintControl;
  UpdateControls;
end;

procedure TscCustomGrid.WMVScroll(var Msg: TWMVScroll);
begin
  ModifyScrollBar(SB_VERT, Msg.ScrollCode, Msg.Pos);
  RePaintControl;
end;

procedure TscCustomGrid.WMHScroll(var Msg: TWMHScroll);
begin
  ModifyScrollBar(SB_HORZ, Msg.ScrollCode, Msg.Pos);
  RePaintControl;
end;

procedure TscCustomGrid.CancelMode;
var
  DrawInfo: TscGridDrawInfo;
begin
  try
    case FGridState of
      scgsSelecting:
        KillTimer(Handle, 1);
      scgsRowSizing, scgsColSizing:
        begin
          CalcDrawInfo(DrawInfo);
          DrawSizingLine(DrawInfo);
        end;
      scgsColMoving, scgsRowMoving:
        begin
          DrawMove;
          KillTimer(Handle, 1);
        end;
    end;
  finally
    FGridState := scgsNormal;
  end;
end;

procedure TscCustomGrid.WMCancelMode(var Msg: TWMCancelMode);
begin
  inherited;
  CancelMode;
end;

procedure TscCustomGrid.CMCancelMode(var Msg: TCMCancelMode);
begin
  if Assigned(FInplaceEdit) then
    FInplaceEdit.WndProc(TMessage(Msg));
  inherited;
  CancelMode;
end;

procedure TscCustomGrid.CMFontChanged(var Message: TMessage);
begin
  if FInplaceEdit <> nil then FInplaceEdit.Font := Font;
  inherited;
end;

procedure TscCustomGrid.CMMouseLeave(var Message: TMessage);
var
  X, Y: Integer;
begin
  inherited;
  if (FHotTrackCell.Coord.X <> -1) or (FHotTrackCell.Coord.Y <> -1) then
  begin
    X := FHotTrackCell.Coord.X;
    Y := FHotTrackCell.Coord.Y;
    FHotTrackCell.Coord.X := -1;
    FHotTrackCell.Coord.Y := -1;
    InvalidateCell(X, Y);
  end;
end;

procedure TscCustomGrid.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure TscCustomGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := LRESULT(BOOL(Sizing(Msg.Pos.X, Msg.Pos.Y)));
end;

procedure TscCustomGrid.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (scgoEditing in Options) and (Char(Msg.CharCode) = #13) then Msg.Result := 1;
end;

procedure TscCustomGrid.TimedScroll(Direction: TscGridScrollDirection);
var
  MaxAnchor, NewAnchor: TscGridCoord;
begin
  NewAnchor := FAnchor;
  MaxAnchor.X := ColCount - 1;
  MaxAnchor.Y := RowCount - 1;
  if (scsdLeft in Direction) and (FAnchor.X > FixedCols) then Dec(NewAnchor.X);
  if (scsdRight in Direction) and (FAnchor.X < MaxAnchor.X) then Inc(NewAnchor.X);
  if (scsdUp in Direction) and (FAnchor.Y > FixedRows) then Dec(NewAnchor.Y);
  if (scsdDown in Direction) and (FAnchor.Y < MaxAnchor.Y) then Inc(NewAnchor.Y);
  if (FAnchor.X <> NewAnchor.X) or (FAnchor.Y <> NewAnchor.Y) then
    MoveAnchor(NewAnchor);
end;

procedure TscCustomGrid.WMTimer(var Msg: TWMTimer);
var
  Point: TPoint;
  DrawInfo: TscGridDrawInfo;
  ScrollDirection: TscGridScrollDirection;
  CellHit: TscGridCoord;
  LeftSide: Integer;
  RightSide: Integer;
begin
  if not (FGridState in [scgsSelecting, scgsRowMoving, scgsColMoving]) then Exit;
  GetCursorPos(Point);
  Point := ScreenToClient(Point);
  CalcDrawInfo(DrawInfo);
  ScrollDirection := [];
  with DrawInfo do
  begin
    CellHit := CalcCoordFromPoint(Point.X, Point.Y, DrawInfo);
    case FGridState of
      scgsColMoving:
        MoveAndScroll(Point.X, CellHit.X, DrawInfo, Horz, SB_HORZ, Point);
      scgsRowMoving:
        MoveAndScroll(Point.Y, CellHit.Y, DrawInfo, Vert, SB_VERT, Point);
      scgsSelecting:
      begin
        if BidiMode <> bdRightToLeft then
        begin
          if Point.X < Horz.FixedBoundary then Include(ScrollDirection, scsdLeft)
          else if Point.X > Horz.FullVisBoundary then Include(ScrollDirection, scsdRight);
        end
        else
        begin
          LeftSide := ClientWidth - Horz.FullVisBoundary;
          RightSide := ClientWidth - Horz.FixedBoundary;
          if Point.X < LeftSide then Include(ScrollDirection, scsdRight)
          else if Point.X > RightSide then Include(ScrollDirection, scsdLeft);
        end;
        if Point.Y < Vert.FixedBoundary then Include(ScrollDirection, scsdUp)
        else if Point.Y > Vert.FullVisBoundary then Include(ScrollDirection, scsdDown);
        if ScrollDirection <> [] then  TimedScroll(ScrollDirection);
      end;
    end;
  end;
end;

procedure TscCustomGrid.ColWidthsChanged;
begin
  UpdateScrollRange;
  UpdateEdit;
end;

procedure TscCustomGrid.RowHeightsChanged;
begin
  UpdateScrollRange;
  UpdateEdit;
end;

procedure TscCustomGrid.DeleteColumn(ACol: Longint);
begin
  MoveColumn(ACol, ColCount-1);
  ColCount := ColCount - 1;
end;

procedure TscCustomGrid.DeleteRow(ARow: Longint);
begin
  MoveRow(ARow, RowCount - 1);
  RowCount := RowCount - 1;
end;

procedure TscCustomGrid.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and HandleAllocated and
    not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

function TscCustomGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    if Row < RowCount - 1 then
    begin
      Row := Row + 1;
      TLinkObservers.PositionLinkPosChanged(Observers);
    end;
    Result := True;
  end;
end;

function TscCustomGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    if Row > FixedRows then
    begin
      Row := Row - 1;
      TLinkObservers.PositionLinkPosChanged(Observers);
    end;
    Result := True;
  end;
end;

function TscCustomGrid.CheckColumnDrag(var Origin,
  Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TscCustomGrid.CheckRowDrag(var Origin,
  Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TscCustomGrid.BeginColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TscCustomGrid.BeginRowDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TscCustomGrid.EndColumnDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

function TscCustomGrid.EndRowDrag(var Origin, Destination: Integer; const MousePt: TPoint): Boolean;
begin
  Result := True;
end;

procedure TscCustomGrid.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then UpdateScrollRange;
end;

{ TscCustomDrawGrid }

function TscCustomDrawGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

function TscCustomDrawGrid.SelectionScreenRect: TRect;
var
  P: TPoint;
begin
  P := ClientToScreen(Point(0, 0));
  Result := SelectionRect;
  OffsetRect(Result, P.X, P.Y);
end;

function TscCustomDrawGrid.SelectionRect: TRect;
var
  X, Y: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  X := Selection.Left;
  Y := Selection.Bottom;
  Result := CellRect(X, Y);
end;

procedure TscCustomDrawGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TscGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TscCustomDrawGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

function TscCustomDrawGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
  if Assigned(FOnGetEditMask) then FOnGetEditMask(Self, ACol, ARow, Result);
end;

function TscCustomDrawGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TscCustomDrawGrid.RowMoved(FromIndex, ToIndex: Longint);
begin
  if Assigned(FOnRowMoved) then FOnRowMoved(Self, FromIndex, ToIndex);
end;

function TscCustomDrawGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
  if Assigned(FOnSelectCell) then FOnSelectCell(Self, ACol, ARow, Result);
end;

procedure TscCustomDrawGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  if Assigned(FOnSetEditText) then FOnSetEditText(Self, ACol, ARow, Value);
end;

procedure TscCustomDrawGrid.DrawCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect;
  AState: TscGridDrawState);
var
  Hold: Integer;
begin
  if Assigned(FOnDrawCell) then
  begin
    if BidiMode = bdRightToLeft then
    begin
      ARect.Left := ClientWidth - ARect.Left;
      ARect.Right := ClientWidth - ARect.Right;
      Hold := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := Hold;
      ChangeGridOrientation(ACanvas, False);
    end;
    FOnDrawCell(Self, ACanvas, ACol, ARow, ARect, AState);
    if BidiMode = bdRightToLeft then ChangeGridOrientation(ACanvas, True);
  end;
end;

procedure TscCustomDrawGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

type
  PStrItem = ^TStrItem;
  TStrItem = record
    FObject: TObject;
    FString: string;
  end;

  TStrItemType = PStrItem;

function NewStrItem(const AString: string; AObject: TObject): TStrItemType;
begin
  New(Result);
  Result.FObject := AObject;
  Result.FString := AString;
end;

procedure DisposeStrItem(P: PStrItem);
begin
  Dispose(P);
end;

type
{ Exception classes }

  EStringSparseListError = class(Exception);

{ TSparsePointerArray class}

  TSPAApply = TFunc<Integer, Pointer, Integer>;

  TSecDir = array[0..4095] of Pointer;
  PSecDir = ^TSecDir;
  TSecDirType = PSecDir;
  TSPAQuantum = (SPASmall, SPALarge);

  TSparsePointerArray = class(TObject)
  private
    secDir: TSecDirType;
    slotsInDir: Cardinal;
    indexMask, secShift: Word;
    FHighBound: Integer;
    FSectionSize: Word;
    cachedIndex: Integer;
    cachedValue: TCustomData;
    function GetAt(Index: Integer): TCustomData;
    procedure PutAt(Index: Integer; Item: TCustomData);
    function MakeAt(Index: Integer): PPointer;
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor Destroy; override;
    function  ForAll(ApplyFunction: TSPAApply): Integer;
    procedure ResetHighBound;
    property HighBound: Integer read FHighBound;
    property SectionSize: Word read FSectionSize;
    property Items[Index: Integer]: TCustomData read GetAt write PutAt; default;
  end;

{ TSparseList class }

  TSparseList = class(TObject)
  private
    FList: TSparsePointerArray;
    FCount: Integer;
    FQuantum: TSPAQuantum;
    procedure NewList(Quantum: TSPAQuantum);
  protected
    function  Get(Index: Integer): TCustomData;
    procedure Put(Index: Integer; Item: TCustomData);
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; Item: TCustomData);
    procedure Move(CurIndex, NewIndex: Integer);
    function ForAll(ApplyFunction: TSPAApply): Integer;
    property Count: Integer read FCount;
    property Items[Index: Integer]: TCustomData read Get write Put; default;
  end;

{ TStringSparseList class }

  TStringSparseList = class(TStrings)
  private
    FList: TSparseList;
    FOnChange: TNotifyEvent;
  protected
    function  Get(Index: Integer): String; override;
    function  GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure Changed;
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Clear; override;
    property List: TSparseList read FList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TSparsePointerArray }

const
  SPAIndexMask: array[TSPAQuantum] of Byte = (15, 255);
  SPASecShift: array[TSPAQuantum] of Byte = (4, 8);

function  ExpandDir(secDir: TSecDirType; var slotsInDir: Cardinal;
  newSlots: Cardinal): TSecDirType;
begin
  Result := secDir;
  ReallocMem(Result, newSlots * SizeOf(Pointer));
  FillChar(Result^[slotsInDir], (newSlots - slotsInDir) * SizeOf(Pointer), 0);
  slotsInDir := newSlots;
end;

function  MakeSec(SecIndex: Integer; SectionSize: Word): Pointer;
var
  SecP: Pointer;
  Size: Word;
begin
  Size := SectionSize * SizeOf(Pointer);
  GetMem(secP, size);
  FillChar(secP^, size, 0);
  MakeSec := SecP
end;

constructor TSparsePointerArray.Create(Quantum: TSPAQuantum);
begin
  SecDir := nil;
  SlotsInDir := 0;
  FHighBound := -1;
  FSectionSize := Word(SPAIndexMask[Quantum]) + 1;
  IndexMask := Word(SPAIndexMask[Quantum]);
  SecShift := Word(SPASecShift[Quantum]);
  CachedIndex := -1
end;

destructor TSparsePointerArray.Destroy;
var
  i:  Cardinal;
  size: Word;
begin
  i := 0;
  size := FSectionSize * SizeOf(Pointer);
  while i < slotsInDir do begin
    if secDir^[i] <> nil then
      FreeMem(secDir^[i], size);
    Inc(i)
  end;
  if secDir <> nil then
    FreeMem(secDir, slotsInDir * SizeOf(Pointer));
end;

function  TSparsePointerArray.GetAt(Index: Integer): TCustomData;
var
  byteP: PByte;
  secIndex: Cardinal;
begin
  if Index = cachedIndex then
    Result := cachedValue
  else begin
    secIndex := Index shr secShift;
    if secIndex >= slotsInDir then
      byteP := nil
    else begin
      byteP := secDir^[secIndex];
      if byteP <> nil then
      begin
        Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
      end
    end;
    if byteP = nil then Result := nil else Result := PPointer(byteP)^;
    cachedIndex := Index;
    cachedValue := Result
  end;
end;

function  TSparsePointerArray.MakeAt(Index: Integer): PPointer;
var
  dirP: PSecDir;
  p: Pointer;
  byteP: PByte;
  secIndex: Cardinal;
begin
  secIndex := Index shr secShift;
  if secIndex >= slotsInDir then
    dirP := expandDir(secDir, slotsInDir, secIndex + 1)
  else
    dirP := secDir;
  secDir := dirP;
  p := dirP^[secIndex];
  if p = nil then
  begin
    p := makeSec(secIndex, FSectionSize);
    dirP^[secIndex] := p
  end;
  byteP := p;
  Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
  if Index > FHighBound then
    FHighBound := Index;
  Result := PPointer(byteP);
  cachedIndex := -1
end;

procedure TSparsePointerArray.PutAt(Index: Integer; Item: TCustomData);
begin
  if (Item <> nil) or (GetAt(Index) <> nil) then
  begin
    MakeAt(Index)^ := Item;
    if Item = nil then
      ResetHighBound
  end
end;

function  TSparsePointerArray.ForAll(ApplyFunction: TSPAApply):
  Integer;
var
  itemP: PByte;
  item: Pointer;
  i: Cardinal;
  j, index: Integer;
begin
  Result := 0;
  i := 0;
  while (i < slotsInDir) and (Result = 0) do begin
    itemP := secDir^[i];
    if itemP <> nil then
    begin
      j := 0;
      index := i shl SecShift;
      while (j < FSectionSize) and (Result = 0) do begin
        item := PPointer(itemP)^;
        if item <> nil then
        begin
          Result := ApplyFunction(index, item);
        end;
        Inc(itemP, SizeOf(Pointer));
        Inc(j);
        Inc(index)
      end
    end;
    Inc(i)
  end;
end;

procedure TSparsePointerArray.ResetHighBound;
var
  NewHighBound: Integer;
begin
  NewHighBound := -1;
  ForAll(
    function(TheIndex: Integer; TheItem: Pointer): Integer
    begin
      if TheIndex > FHighBound then
        Result := 1
      else
      begin
        Result := 0;
        if TheItem <> nil then NewHighBound := TheIndex
      end
    end);
  FHighBound := NewHighBound
end;

constructor TSparseList.Create(Quantum: TSPAQuantum);
begin
  inherited Create;
  NewList(Quantum)
end;

destructor TSparseList.Destroy;
begin
  if FList <> nil then
    FList.Destroy
end;

procedure TSparseList.Clear;
begin
  FList.Destroy;
  NewList(FQuantum);
  FCount := 0
end;

procedure TSparseList.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then Exit;
  for I := Index to FCount - 1 do
    FList[I] := FList[I + 1];
  FList[FCount] := nil;
  Dec(FCount);
end;

procedure TSparseList.Exchange(Index1, Index2: Integer);
var
  Temp: TCustomData;
begin
  Temp := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Temp);
end;

function TSparseList.ForAll(ApplyFunction: TSPAApply): Integer;
begin
  Result := FList.ForAll(ApplyFunction);
end;

function TSparseList.Get(Index: Integer): TCustomData;
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  Result := FList[Index]
end;

procedure TSparseList.Insert(Index: Integer; Item: TCustomData);
var
  i: Integer;
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  I := FCount;
  while I > Index do
  begin
    FList[i] := FList[i - 1];
    Dec(i)
  end;
  FList[Index] := Item;
  if Index > FCount then FCount := Index;
  Inc(FCount)
end;

procedure TSparseList.Move(CurIndex, NewIndex: Integer);
var
  Item: TCustomData;
begin
  if CurIndex <> NewIndex then
  begin
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

procedure TSparseList.NewList(Quantum: TSPAQuantum);
begin
  FQuantum := Quantum;
  FList := TSparsePointerArray.Create(Quantum)
end;

procedure TSparseList.Put(Index: Integer; Item: TCustomData);
begin
  if Index < 0 then TList.Error(SListIndexError, Index);
  FList[Index] := Item;
  FCount := FList.HighBound + 1
end;

{ TStringSparseList }

constructor TStringSparseList.Create(Quantum: TSPAQuantum);
begin
  inherited Create;
  FList := TSparseList.Create(Quantum)
end;

destructor  TStringSparseList.Destroy;
begin
  if FList <> nil then
  begin
    Clear;
    FList.Destroy
  end
end;

procedure TStringSparseList.ReadData(Reader: TReader);
var
  i: Integer;
begin
  with Reader do begin
    i := Integer(ReadInteger);
    while i > 0 do begin
      InsertObject(Integer(ReadInteger), ReadString, nil);
      Dec(i)
    end
  end
end;


procedure TStringSparseList.WriteData(Writer: TWriter);
var
  itemCount: Integer;
begin
  with Writer do
  begin
    itemCount := 0;
    FList.ForAll(
      function(TheIndex: Integer; TheItem: Pointer): Integer
      begin
        Inc(itemCount);
        Result := 0
      end);
    WriteInteger(itemCount);
    FList.ForAll(
      function(TheIndex: Integer; TheItem: Pointer): Integer
      begin
        with Writer do
        begin
          WriteInteger(TheIndex);
          WriteString(PStrItem(TheItem)^.FString);
        end;
        Result := 0
      end);
  end
end;

procedure TStringSparseList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('List', ReadData, WriteData, True);
end;

function  TStringSparseList.Get(Index: Integer): String;
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p = nil then Result := '' else Result := p.FString
end;

function  TStringSparseList.GetCount: Integer;
begin
  Result := FList.Count
end;

function  TStringSparseList.GetObject(Index: Integer): TObject;
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p = nil then Result := nil else Result := p.FObject
end;

procedure TStringSparseList.Put(Index: Integer; const S: String);
var
  p: TStrItemType;
  obj: TObject;
begin
  p := TStrItemType(FList[Index]);
  if p = nil then obj := nil else obj := p.FObject;
  if (S = '') and (obj = nil) then
    FList[Index] := nil
  else
    FList[Index] := NewStrItem(S, obj);
  if p <> nil then DisposeStrItem(p);
  Changed
end;

procedure TStringSparseList.PutObject(Index: Integer; AObject: TObject);
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p <> nil then
    p.FObject := AObject
  else if AObject <> nil then
    FList[Index] := NewStrItem('',AObject);
  Changed
end;

procedure TStringSparseList.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self)
end;

procedure TStringSparseList.Delete(Index: Integer);
var
  p: TStrItemType;
begin
  p := TStrItemType(FList[Index]);
  if p <> nil then DisposeStrItem(p);
  FList.Delete(Index);
  Changed
end;

procedure TStringSparseList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TStringSparseList.Insert(Index: Integer; const S: String);
begin
  FList.Insert(Index, NewStrItem(S, nil));
  Changed
end;


procedure TStringSparseList.Clear;
begin
  FList.ForAll(
    function(TheIndex: Integer; TheItem: Pointer): Integer
    begin
      DisposeStrItem(PStrItem(TheItem));
      Result := 0
    end);
  FList.Clear;
  Changed
end;

constructor TscStringGridStrings.Create(AGrid: TscStringGrid; AIndex: Longint);
begin
  inherited Create;
  FGrid := AGrid;
  FIndex := AIndex;
end;

procedure TscStringGridStrings.Assign(Source: TPersistent);
var
  I, Max: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    Max := TStrings(Source).Count - 1;
    if Max >= Count then Max := Count - 1;
    try
      for I := 0 to Max do
      begin
        Put(I, TStrings(Source).Strings[I]);
        PutObject(I, TStrings(Source).Objects[I]);
      end;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TscStringGridStrings.CalcXY(Index: Integer; var X, Y: Integer);
begin
  if FIndex = 0 then
  begin
    X := -1; Y := -1;
  end else if FIndex > 0 then
  begin
    X := Index;
    Y := FIndex - 1;
  end else
  begin
    X := -FIndex - 1;
    Y := Index;
  end;
end;

function TscStringGridStrings.Add(const S: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Strings[I] = '' then
    begin
      if S = '' then
        Strings[I] := ' '
      else
        Strings[I] := S;
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TscStringGridStrings.Clear;
var
  SSList: TStringSparseList;
  I: Integer;
begin
  if FIndex > 0 then
  begin
    SSList := TStringSparseList(TSparseList(FGrid.FData)[FIndex - 1]);
    if SSList <> nil then
      SSList.List.ForAll(
        function(TheIndex: Integer; TheItem: Pointer): Integer
        begin
          Objects[TheIndex] := nil;
          Strings[TheIndex] := '';
          Result := 0;
        end);
  end
  else if FIndex < 0 then
    for I := Count - 1 downto 0 do
    begin
      Objects[I] := nil;
      Strings[I] := '';
    end;
end;

procedure TscStringGridStrings.Delete(Index: Integer);
begin
  InvalidOp(sInvalidStringGridOp);
end;

function TscStringGridStrings.Get(Index: Integer): string;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then Result := '' else Result := FGrid.Cells[X, Y];
end;

function TscStringGridStrings.GetCount: Integer;
begin
  if FIndex = 0 then Result := 0
  else if FIndex > 0 then Result := Integer(FGrid.ColCount)
  else Result := Integer(FGrid.RowCount);
end;

function TscStringGridStrings.GetObject(Index: Integer): TObject;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then Result := nil else Result := FGrid.Objects[X, Y];
end;

procedure TscStringGridStrings.Insert(Index: Integer; const S: string);
begin
  InvalidOp(sInvalidStringGridOp);
end;

procedure TscStringGridStrings.Put(Index: Integer; const S: string);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FGrid.Cells[X, Y] := S;
end;

procedure TscStringGridStrings.PutObject(Index: Integer; AObject: TObject);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FGrid.Objects[X, Y] := AObject;
end;

procedure TscStringGridStrings.SetUpdateState(Updating: Boolean);
begin
  FGrid.SetUpdateState(Updating);
end;

{ TscStringGrid }

constructor TscStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Initialize;
end;

destructor TscStringGrid.Destroy;
var
  FreeCallBack: TSPAApply;

begin
  FreeCallBack :=
  function(TheIndex: Integer; TheItem: Pointer): Integer
  begin
    TObject(TheItem).Free;
    Result := 0;
  end;
  if FRows <> nil then
  begin
    TSparseList(FRows).ForAll(FreeCallBack);
    TSparseList(FRows).Free;
  end;
  if FCols <> nil then
  begin
    TSparseList(FCols).ForAll(FreeCallBack);
    TSparseList(FCols).Free;
  end;
  if FData <> nil then
  begin
    TSparseList(FData).ForAll(FreeCallBack);
    TSparseList(FData).Free;
  end;
  inherited Destroy;
end;

function TscStringGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

procedure TscStringGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  TSparseList(FData).ForAll(
  function(Index: Integer; ARow: Pointer): Integer
  begin
    TStringSparseList(ARow).Move(FromIndex, ToIndex);
    Result := 0;
  end);
  Invalidate;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

procedure TscStringGrid.RowMoved(FromIndex, ToIndex: Longint);
begin
  TSparseList(FData).Move(FromIndex, ToIndex);
  Invalidate;
  inherited RowMoved(FromIndex, ToIndex);
end;

function TscStringGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := Cells[ACol, ARow];
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TscStringGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  DisableEditUpdate;
  try
    if Value <> Cells[ACol, ARow] then
      Cells[ACol, ARow] := Value;
  finally
    EnableEditUpdate;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TscStringGrid.DrawCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect;
  AState: TscGridDrawState);
const
  CCellNormal: array[TscGridDrawingStyle] of TThemedGrid =
    (tgClassicCellNormal, tgCellNormal, tgGradientCellNormal);
  CCellSelected: array[TscGridDrawingStyle] of TThemedGrid =
    (tgClassicCellSelected, tgCellSelected, tgGradientCellSelected);
  CFixedNormal: array[TscGridDrawingStyle] of TThemedGrid =
    (tgClassicFixedCellNormal, tgFixedCellNormal, tgGradientFixedCellNormal);
  CFixedHot: array[TscGridDrawingStyle] of TThemedGrid =
    (tgClassicFixedCellHot, tgFixedCellHot, tgGradientFixedCellHot);
  CFixedPressed: array[TscGridDrawingStyle] of TThemedGrid =
    (tgClassicFixedCellPressed, tgFixedCellPressed, tgGradientFixedCellPressed);
var
  LText: string;
  LDetails: TThemedElementDetails;
  AFontColor: TColor;
  ABGColor: TColor;
  AFillBG: Boolean;
  Hold: Integer;
begin
  if DefaultDrawing then
  begin
    if Assigned(FOnGetCellParam)
      then
        with ACanvas do
        begin
          AFontColor := Font.Color;
          ABGColor := Canvas.Brush.Color;
          AFillBG := False;
          FOnGetCellParam(Self, ACol, ARow, AState, ABGColor, AFillBG, AFontColor);
          Font.Color := AFontColor;
          if AFillBG
          then
            begin
              Brush.Style := bsSolid;
              Brush.Color := ABGColor;
              FillRect(ARect);
            end;
        end;

    if StyleServices.Enabled then
    begin
      ARect.Left := ARect.Left + 4;

      if (scgdFixed in AState) then
      begin
        if scgdHotTrack in AState then
          LDetails := StyleServices.GetElementDetails(CFixedHot[FInternalDrawingStyle])
        else if scgdPressed in AState then
          LDetails := StyleServices.GetElementDetails(CFixedPressed[FInternalDrawingStyle])
        else
          LDetails := StyleServices.GetElementDetails(CFixedNormal[FInternalDrawingStyle])
      end
      else
      begin
        if (scgdSelected in AState) or (scgdRowSelected in AState) then
          LDetails := StyleServices.GetElementDetails(CCellSelected[FInternalDrawingStyle])
        else
          LDetails := StyleServices.GetElementDetails(CCellNormal[FInternalDrawingStyle]);
      end;

      LText := Cells[ACol, ARow];
      ACanvas.Brush.Style := bsClear;
      if LText <> '' then
      begin
        if BidiMode = bdRightToLeft then
        begin
          ARect.Left := ClientWidth - ARect.Left;
          ARect.Right := ClientWidth - ARect.Right;
          Hold := ARect.Left;
          ARect.Left := ARect.Right;
          ARect.Right := Hold;
          ChangeGridOrientation(ACanvas, False);
        end;

        scDrawClipText(ACanvas, LText, ARect, BidiMode = bdRightToLeft, True);

        if BidiMode = bdRightToLeft then
          ChangeGridOrientation(ACanvas, True);
      end;
    end
    else
    begin
      LText := Cells[ACol, ARow];
      ACanvas.Brush.Style := bsClear;
      ARect.Left := ARect.Left + 4;

      if BidiMode = bdRightToLeft then
      begin
        ARect.Left := ClientWidth - ARect.Left;
        ARect.Right := ClientWidth - ARect.Right;
        Hold := ARect.Left;
        ARect.Left := ARect.Right;
        ARect.Right := Hold;
        ChangeGridOrientation(ACanvas, False);
      end;

      scDrawClipText(ACanvas, LText, ARect, BidiMode = bdRightToLeft, True);

      if BidiMode = bdRightToLeft then
          ChangeGridOrientation(ACanvas, True);
    end;
  end;
  inherited DrawCell(ACanvas, ACol, ARow, ARect, AState);
end;

procedure TscStringGrid.DisableEditUpdate;
begin
  Inc(FEditUpdate);
end;

procedure TscStringGrid.EnableEditUpdate;
begin
  Dec(FEditUpdate);
end;

procedure TscStringGrid.Initialize;
var
  quantum: TSPAQuantum;
begin
  if FCols = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    FCols := TSparseList.Create(quantum);
  end;
  if RowCount > 256 then quantum := SPALarge else quantum := SPASmall;
  if FRows = nil then FRows := TSparseList.Create(quantum);
  if FData = nil then FData := TSparseList.Create(quantum);
end;

procedure TscStringGrid.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if not Updating and FNeedsUpdating then
  begin
    InvalidateGrid;
    FNeedsUpdating := False;
  end;
end;

procedure TscStringGrid.Update(ACol, ARow: Integer);
begin
  if not FUpdating then InvalidateCell(ACol, ARow)
  else FNeedsUpdating := True;
  if (ACol = Col) and (ARow = Row) and (FEditUpdate = 0) then InvalidateEditor;
end;

function  TscStringGrid.EnsureColRow(Index: Integer; IsCol: Boolean):
  TscStringGridStrings;
var
  RCIndex: Integer;
  PList: ^TSparseList;
begin
  if IsCol then PList := @FCols else PList := @FRows;
  Result := TscStringGridStrings(PList^[Index]);
  if Result = nil then
  begin
    if IsCol then RCIndex := -Index - 1 else RCIndex := Index + 1;
    Result := TscStringGridStrings.Create(Self, RCIndex);
    PList^[Index] := Result;
  end;
end;

function  TscStringGrid.EnsureDataRow(ARow: Integer): TCustomData;
var
  quantum: TSPAQuantum;
begin
  Result := TStringSparseList(TSparseList(FData)[ARow]);
  if Result = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    Result := TStringSparseList.Create(quantum);
    TSparseList(FData)[ARow] := Result;
  end;
end;

function TscStringGrid.GetCells(ACol, ARow: Integer): string;
var
  ssl: TStringSparseList;
begin
  ssl := TStringSparseList(TSparseList(FData)[ARow]);
  if ssl = nil then Result := '' else Result := ssl[ACol];
end;

function TscStringGrid.GetCols(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, True);
end;

function TscStringGrid.GetObjects(ACol, ARow: Integer): TObject;
var
  ssl: TStringSparseList;
begin
  ssl := TStringSparseList(TSparseList(FData)[ARow]);
  if ssl = nil then Result := nil else Result := ssl.Objects[ACol];
end;

function TscStringGrid.GetRows(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, False);
end;

procedure TscStringGrid.SetCells(ACol, ARow: Integer; const Value: string);
begin
  TscStringGridStrings(EnsureDataRow(ARow))[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
  Update(ACol, ARow);
end;

procedure TscStringGrid.SetCols(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, True).Assign(Value);
end;

procedure TscStringGrid.SetObjects(ACol, ARow: Integer; Value: TObject);
begin
  TscStringGridStrings(EnsureDataRow(ARow)).Objects[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
  Update(ACol, ARow);
end;

procedure TscStringGrid.SetRows(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, False).Assign(Value);
end;

type

{ TscGridPopupListbox }

  TscGridPopupListbox = class(TscCustomListbox)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

procedure TscGridPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS or CS_DROPSHADOW;
  end;
end;

procedure TscGridPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Winapi.Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TscGridPopupListbox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..High(Char):
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then
          FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then
          FSearchText := FSearchText + Key;
        SendTextMessage(Handle, LB_SelectString, WORD(-1), FSearchText);
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TscGridPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TscInplaceEditList(Owner).CloseUp((X >= 0) and (Y >= 0) and
      (X < Width) and (Y < Height));
end;

{ TscInplaceEditList }

constructor TscInplaceEditList.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FEditStyle := esSimple;
end;

procedure TscInplaceEditList.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if ListVisible and (ActiveList = FPickList) then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if PickList.ItemIndex <> -1 then
      ListValue := PickList.Items[PickList.ItemIndex];
    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept then
      if (not VarIsEmpty(ListValue) or VarIsNull(ListValue))
         and (VarToStr(ListValue) <> Text) then
      begin
        Perform(WM_SETTEXT, 0, LPARAM(string(ListValue)));
        Modified := True;
        with Grid do
          SetEditText(Col, Row, VarToStr(ListValue));
      end;
  end;
end;

procedure TscInplaceEditList.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if ListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if ListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TscInplaceEditList.DoEditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Grid);
end;

procedure TscInplaceEditList.DoGetPickListItems;
begin
  if not PickListLoaded then
  begin
    if Assigned(OnGetPickListItems) then
      OnGetPickListItems(Grid.Col, Grid.Row, PickList.Items);
    PickListLoaded := (PickList.Items.Count > 0);
  end;
end;

function TscInplaceEditList.GetPickList: TscCustomListbox;
var
  PopupListbox: TscGridPopupListbox;
begin
  if not Assigned(FPickList) then
  begin
    PopupListbox := TscGridPopupListbox.Create(Self);
    PopupListbox.Visible := False;
    PopupListbox.Parent := Self;
    PopupListbox.OnMouseUp := ListMouseUp;
    PopupListbox.IntegralHeight := True;
    PopupListbox.ItemHeight := Self.Height - 2;
    FPickList := PopupListBox;
  end;
  Result := FPickList;
end;

procedure TscInplaceEditList.OnButtonClick(Sender: TObject);
begin
  DoEditButtonClick;
  if FEditStyle = esPickList then
  begin
    if ListVisible
    then
      CloseUp(False)
    else
      if Assigned(ActiveList) then
        DropDown;
  end;
end;

procedure TscInplaceEditList.DropDown;
var
  P: TPoint;
  I,J,Y: Integer;
begin
  if not ListVisible then
  begin
    ActiveList.Width := Width;
    if ActiveList = FPickList then
    begin
      DoGetPickListItems;
      TscGridPopupListbox(PickList).Color := Color;
      TscGridPopupListbox(PickList).Font := Font;
      TscGridPopupListbox(PickList).SelectionStyle := Grid.SelectionStyle;
      TscGridPopupListbox(PickList).ItemHeight := FGrid.DefaultRowHeight;
      {$IFNDEF VER230}
      if Grid <> nil then
        TscGridPopupListbox(PickList).StyleElements := Self.Grid.StyleElements;
      {$ENDIF}
      if (DropDownRows > 0) and (PickList.Items.Count >= DropDownRows) then
        PickList.Height := DropDownRows * TscGridPopupListbox(PickList).ItemHeight + 4
      else
        PickList.Height := PickList.Items.Count * TscGridPopupListbox(PickList).ItemHeight + 4;
      if Text = '' then
        PickList.ItemIndex := -1
      else
        PickList.ItemIndex := PickList.Items.IndexOf(Text);
      J := PickList.ClientWidth;
      for I := 0 to PickList.Items.Count - 1 do
      begin
        Y := PickList.Canvas.TextWidth(PickList.Items[I]);
        if Y > J then J := Y;
      end;
      PickList.ClientWidth := J;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + ActiveList.Height > Screen.Height then Y := P.Y - ActiveList.Height;
    SetWindowPos(ActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Winapi.Windows.SetFocus(Handle);
  end;
end;

procedure TscInplaceEditList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    DoEditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TscInplaceEditList.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(ActiveList.ClientRect.Contains(Point(X, Y)));
end;

procedure TscInplaceEditList.UpdateContents;
begin
  ActiveList := nil;
  PickListLoaded := False;
  FEditStyle := Grid.GetEditStyle(Grid.Col, Grid.Row);
  OnRightButtonClick := OnButtonClick;
  if EditStyle = esPickList then
    ActiveList := PickList;
  case FEditStyle of
    esSimple:
      begin
        RightButton.Visible := False;
      end;
    esEllipsis:
    begin
      RightButton.StyleKind := scbsPushButton;
      RightButton.ShowEllipses := True;
      RightButton.Visible := True;
    end;
    esPickList:
    begin
      RightButton.StyleKind := scbsDropDownButton;
      RightButton.ShowEllipses := False;
      RightButton.Visible := True;
      RightButton.Width := GetSystemMetrics(SM_CXVSCROLL);
    end;
  end;
  inherited UpdateContents;
end;

procedure TscInplaceEditList.RestoreContents;
begin
  Reset;
  Grid.UpdateText;
end;

procedure TscInplaceEditList.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> ActiveList) then
    CloseUp(False);
end;

procedure TscInplaceEditList.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
end;

procedure TscInplaceEditList.WMKillFocus(var Message: TWMKillFocus);
begin
  if not SysLocale.FarEast then
  begin
    inherited;
  end else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if HWND(Message.FocusedWnd) <> Grid.Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  CloseUp(False);
end;

procedure TscInplaceEditList.WndProc(var Message: TMessage);
var
  TheChar: Word;
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle = esPickList then
      with TWMKey(Message) do
      begin
        TheChar := CharCode;
        DoDropDownKeys(TheChar, KeyDataToShiftState(KeyData));
        CharCode := TheChar;
        if (CharCode <> 0) and ListVisible then
        begin
          with Message do
            SendMessage(ActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end;
  end;
  inherited;
end;

procedure TscInplaceEditList.DblClick;
var
  Index: Integer;
  ListValue: string;
begin
  if (EditStyle = esSimple) or Assigned(Grid.OnDblClick) then
    inherited
  else if (EditStyle = esPickList) and (ActiveList = PickList) then
  begin
    DoGetPickListItems;
    if PickList.Items.Count > 0 then
    begin
      Index := PickList.ItemIndex + 1;
      if Index >= PickList.Items.Count then
        Index := 0;
      PickList.ItemIndex := Index;
      ListValue := PickList.Items[PickList.ItemIndex];
      Perform(WM_SETTEXT, 0, LPARAM(ListValue));
      Modified := True;
      with Grid do
        SetEditText(Col, Row, ListValue);
      SelectAll;
    end;
  end
  else if EditStyle = esEllipsis then
    DoEditButtonClick;
end;

initialization

  TCustomStyleEngine.RegisterStyleHook(TscGridPopupListBox, TscListBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscDrawGrid, TscScrollingStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscStringGrid, TscScrollingStyleHook);

finalization

  TCustomStyleEngine.UnRegisterStyleHook(TscGridPopupListBox, TscListBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscDrawGrid, TscScrollingStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscStringGrid, TscScrollingStyleHook);

end.
