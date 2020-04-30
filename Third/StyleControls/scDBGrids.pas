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


unit scDBGrids;

{$R-}
{$I scdefine.inc}

interface

uses System.Variants, Winapi.Windows, System.SysUtils, Winapi.Messages,
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics,
  Data.DB, Vcl.DBCtrls, Vcl.Menus, Vcl.ImgList, scControls, scGrids, scDBControls,
  scImageCollection, scDialogs;

type
  TscColumnValue = (sccvColor, sccvWidth, sccvFont, sccvAlignment,
    sccvReadOnly, sccvTitleColor,
    sccvTitleCaption, sccvTitleAlignment, sccvTitleFont, sccvImeMode, sccvImeName);
  TscColumnValues = set of TscColumnValue;

const
  SC_ColumnTitleValues = [sccvTitleColor..sccvTitleFont];
  CM_DEFERLAYOUT = WM_USER + 100;

type
  TscColumn = class;
  TscCustomDBGrid = class;

  TscColumnTitle = class(TPersistent)
  private
    FColumn: TscColumn;
    FCaption: string;
    FFont: TFont;
    FColor: TColor;
    FAlignment: TAlignment;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetCaption: string;
    function GetFont: TFont;
    function IsAlignmentStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsCaptionStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string); virtual;
  protected
    procedure RefreshDefaultFont;
  public
    constructor Create(Column: TscColumn);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    function DefaultCaption: string;
    procedure RestoreDefaults; virtual;
    property Column: TscColumn read FColumn;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
  end;

  TscColumnButtonStyle = (sccbsAuto, sccbsEllipsis, sccbsNone);

  TscColumn = class(TCollectionItem)
  private
    FField: TField;
    FFieldName: string;
    FColor: TColor;
    FWidth: Integer;
    FTitle: TscColumnTitle;
    FFont: TFont;
    FImeMode: TImeMode;
    FImeName: TImeName;
    FPickList: TStrings;
    FPopupMenu: TPopupMenu;
    FDropDownRows: Cardinal;
    FButtonStyle: TscColumnButtonStyle;
    FAlignment: TAlignment;
    FReadonly: Boolean;
    FAssignedValues: TscColumnValues;
    FVisible: Boolean;
    FExpanded: Boolean;
    FStored: Boolean;
    procedure FontChanged(Sender: TObject);
    function  GetAlignment: TAlignment;
    function  GetColor: TColor;
    function  GetExpanded: Boolean;
    function  GetField: TField;
    function  GetFont: TFont;
    function  GetImeMode: TImeMode;
    function  GetImeName: TImeName;
    function  GetParentColumn: TscColumn;
    function  GetPickList: TStrings;
    function  GetReadOnly: Boolean;
    function  GetShowing: Boolean;
    function  GetWidth: Integer;
    function  GetVisible: Boolean;
    function  IsAlignmentStored: Boolean;
    function  IsColorStored: Boolean;
    function  IsFontStored: Boolean;
    function  IsImeModeStored: Boolean;
    function  IsImeNameStored: Boolean;
    function  IsReadOnlyStored: Boolean;
    function  IsWidthStored: Boolean;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetButtonStyle(Value: TscColumnButtonStyle);
    procedure SetColor(Value: TColor);
    procedure SetExpanded(Value: Boolean);
    procedure SetField(Value: TField); virtual;
    procedure SetFieldName(const Value: string);
    procedure SetFont(Value: TFont);
    procedure SetImeMode(Value: TImeMode); virtual;
    procedure SetImeName(Value: TImeName); virtual;
    procedure SetPickList(Value: TStrings);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetTitle(Value: TscColumnTitle);
    procedure SetWidth(Value: Integer); virtual;
    procedure SetVisible(Value: Boolean);
    function GetExpandable: Boolean;
  protected
    function  CreateTitle: TscColumnTitle; virtual;
    function  GetGrid: TscCustomDBGrid;
    function GetDisplayName: string; override;
    procedure RefreshDefaultFont;
    procedure SetIndex(Value: Integer); override;
    property IsStored: Boolean read FStored write FStored default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  DefaultAlignment: TAlignment;
    function  DefaultColor: TColor;
    function  DefaultFont: TFont;
    function  DefaultImeMode: TImeMode;
    function  DefaultImeName: TImeName;
    function  DefaultReadOnly: Boolean;
    function  DefaultWidth: Integer;
    function  Depth: Integer;
    procedure RestoreDefaults; virtual;
    property  Grid: TscCustomDBGrid read GetGrid;
    property  AssignedValues: TscColumnValues read FAssignedValues;
    property  Expandable: Boolean read GetExpandable;
    property  Field: TField read GetField write SetField;
    property  ParentColumn: TscColumn read GetParentColumn;
    property  Showing: Boolean read GetShowing;
  published
    property  Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property  ButtonStyle: TscColumnButtonStyle read FButtonStyle write SetButtonStyle
      default sccbsAuto;
    property  Color: TColor read GetColor write SetColor stored IsColorStored;
    property  DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 7;
    property  Expanded: Boolean read GetExpanded write SetExpanded default True;
    property  FieldName: string read FFieldName write SetFieldName;
    property  Font: TFont read GetFont write SetFont stored IsFontStored;
    property  ImeMode: TImeMode read GetImeMode write SetImeMode stored IsImeModeStored;
    property  ImeName: TImeName read GetImeName write SetImeName stored IsImeNameStored;
    property  PickList: TStrings read GetPickList write SetPickList;
    property  PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property  ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
    property  Title: TscColumnTitle read FTitle write SetTitle;
    property  Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property  Visible: Boolean read GetVisible write SetVisible;
  end;

  TscColumnClass = class of TscColumn;

  TscDBGridColumnsState = (sccsDefault, sccsCustomized);

  TscDBGridColumns = class(TCollection)
  private
    FGrid: TscCustomDBGrid;
    function GetColumn(Index: Integer): TscColumn;
    function InternalAdd: TscColumn;
    procedure SetColumn(Index: Integer; Value: TscColumn);
    procedure SetState(NewState: TscDBGridColumnsState);
    function GetState: TscDBGridColumnsState;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TscCustomDBGrid; ColumnClass: TscColumnClass);
    function  Add: TscColumn;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure RestoreDefaults;
    procedure RebuildColumns;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream);
    property State: TscDBGridColumnsState read GetState write SetState;
    property Grid: TscCustomDBGrid read FGrid;
    property Items[Index: Integer]: TscColumn read GetColumn write SetColumn; default;
  end;

  TscGridDataLink = class(TDataLink)
  private
    FGrid: TscCustomDBGrid;
    FFieldCount: Integer;
    FFieldMap: array of Integer;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FSparseMap: Boolean;
    function GetDefaultFields: Boolean;
    function GetFields(I: Integer): TField;
  protected
    procedure ActiveChanged; override;
    procedure BuildAggMap;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure EditingChanged; override;
    function IsAggRow(Value: Integer): Boolean; virtual;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    function  GetMappedIndex(ColIndex: Integer): Integer;
  public
    constructor Create(AGrid: TscCustomDBGrid);
    destructor Destroy; override;
    function AddMapping(const FieldName: string): Boolean;
    procedure ClearMapping;
    procedure Modified;
    procedure Reset;
    property DefaultFields: Boolean read GetDefaultFields;
    property FieldCount: Integer read FFieldCount;
    property Fields[I: Integer]: TField read GetFields;
    property SparseMap: Boolean read FSparseMap write FSparseMap;
    property Grid: TscCustomDBGrid read FGrid;
  end;

  TscBookmarkList = class
  private
    FList: array of TBookmark;
    FGrid: TscCustomDBGrid;
    FCache: TBookmark;
    FCacheIndex: Integer;
    FCacheFind: Boolean;
    FLinkActive: Boolean;
    function GetCount: Integer;
    function GetCurrentRowSelected: Boolean;
    function GetItem(Index: Integer): TBookmark;
    procedure InsertItem(Index: Integer; Item: TBookmark);
    procedure DeleteItem(Index: Integer);
    procedure SetCurrentRowSelected(Value: Boolean);
    procedure DataChanged(Sender: TObject);
  protected
    function CurrentRow: TBookmark;
    function Compare(const Item1, Item2: TBookmark): Integer;
    procedure LinkActive(Value: Boolean);
  public
    constructor Create(AGrid: TscCustomDBGrid);
    destructor Destroy; override;
    procedure Clear;
    procedure Delete;
    function  Find(const Item: TBookmark; var Index: Integer): Boolean;
    function  IndexOf(const Item: TBookmark): Integer;
    function  Refresh: Boolean;
    property Count: Integer read GetCount;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected
      write SetCurrentRowSelected;
    property Items[Index: Integer]: TBookmark read GetItem; default;
  end;

  TscDBGridOption = (scdgEditing, scdgAlwaysShowEditor, scdgTitles, scdgIndicator,
    scdgColumnResize, scdgColLines, scdgRowLines, scdgTabs, scdgRowSelect,
    scdgAlwaysShowSelection, scdgConfirmDelete, scdgCancelOnExit, scdgMultiSelect,
    scdgTitleClick, scdgTitleHotTrack);
  TscDBGridOptions = set of TscDBGridOption;

  TscDrawDataCellEvent = procedure (Sender: TObject; ACanvas: TCanvas; const Rect: TRect; Field: TField;
    State: TscGridDrawState) of object;

  TDrawColumnCellEvent = procedure (Sender: TObject; ACanvas: TCanvas; const Rect: TRect;
    DataCol: Integer; Column: TscColumn; State: TscGridDrawState) of object;
  TscDBGridClickEvent = procedure (Column: TscColumn) of object;

  TscCustomDBGrid = class(TscCustomGrid)
  private
    FTouchBegin, FTouchEnd: Integer;
    FNeedRestoreImeName: Boolean;
    FIndicators: TscImageCollection;
    FIndicatorsLoading: Boolean;
    FTitleFont: TFont;
    FReadOnly: Boolean;
    FOriginalImeName: TImeName;
    FOriginalImeMode: TImeMode;
    FUserChange: Boolean;
    FIsESCKey: Boolean;
    FLayoutFromDataset: Boolean;
    FOptions: TscDBGridOptions;
    FTitleOffset, FIndicatorOffset: Byte;
    FUpdateLock: Byte;
    FLayoutLock: Byte;
    FInColExit: Boolean;
    FDefaultDrawing: Boolean;
    FSelfChangingTitleFont: Boolean;
    FSelecting: Boolean;
    FSelRow: Integer;
    FDataLink: TscGridDataLink;
    FOnColEnter: TNotifyEvent;
    FOnColExit: TNotifyEvent;
    FOnDrawDataCell: TscDrawDataCellEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FEditText: string;
    FColumns: TscDBGridColumns;
    FVisibleColumns: TList;
    FBookmarks: TscBookmarkList;
    FSelectionAnchor: TBookmark;
    FOnEditButtonClick: TNotifyEvent;
    FOnColumnMoved: TscMovedEvent;
    FOnCellClick: TscDBGridClickEvent;
    FOnTitleClick:TscDBGridClickEvent;
    FDragCol: TscColumn;
    FOldScrollBarVisible: Boolean;
    function AcquireFocus: Boolean;
    procedure DataChanged;
    procedure EditingChanged;
    function GetDataSource: TDataSource;
    function GetFieldCount: Integer;
    function GetFields(FieldIndex: Integer): TField;
    function GetSelectedField: TField;
    function GetSelectedIndex: Integer;
    procedure InternalLayout;
    procedure MoveCol(RawCol, Direction: Integer);
    function PtInExpandButton(X,Y: Integer; var MasterCol: TscColumn): Boolean;
    procedure ReadColumns(Reader: TReader);
    procedure RecordChanged(Field: TField);
    procedure SetIme;
    procedure SetColumns(Value: TscDBGridColumns);
    procedure SetDataSource(Value: TDataSource);
    procedure SetOptions(Value: TscDBGridOptions);
    procedure SetSelectedField(Value: TField);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetTitleFont(Value: TFont);
    procedure TitleFontChanged(Sender: TObject);
    procedure UpdateData;
    procedure UpdateActive;
    procedure UpdateIme;
    procedure UpdateRowCount;
    procedure WriteColumns(Writer: TWriter);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMDeferLayout(var Message); message CM_DEFERLAYOUT;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMIMEStartComp(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SetFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  protected
    FUpdateFields: Boolean;
    FAcquireFocus: Boolean;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure InitResImages;
    function  RawToDataColumn(ACol: Integer): Integer;
    function  DataToRawColumn(ACol: Integer): Integer;
    function  AcquireLayoutLock: Boolean;
    procedure BeginLayout;
    procedure BeginUpdate;
    procedure CalcSizingState(X, Y: Integer; var State: TscGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TscGridDrawInfo); override;
    procedure CancelLayout;
    function  CanEditAcceptKey(Key: Char): Boolean; override;
    function  CanEditModify: Boolean; override;
    function  CanEditShow: Boolean; override;
    procedure CellClick(Column: TscColumn); dynamic;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    function CalcTitleRect(Col: TscColumn; ARow: Integer;
      var MasterCol: TscColumn): TRect;
    function ColumnAtDepth(Col: TscColumn; ADepth: Integer): TscColumn;
    procedure CMGesture(var Message: TCMGesture); message CM_GESTURE;
    procedure ColEnter; dynamic;
    procedure ColExit; dynamic;
    procedure ColWidthsChanged; override;
    function  CreateColumns: TscDBGridColumns; dynamic;
    function  CreateEditor: TscInplaceEdit; override;
    function  CreateDataLink: TscGridDataLink; dynamic;
    procedure CreateWnd; override;
    procedure DeferLayout;
    procedure DefineFieldMap; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect; AState: TscGridDrawState); override;
    procedure DrawCellBackground(ACanvas: TCanvas; const ARect: TRect; AColor: TColor;
      AState: TscGridDrawState; ACol, ARow: Integer); override;
    procedure DrawCellHighlight(ACanvas: TCanvas; const ARect: TRect;
      AState: TscGridDrawState; ACol, ARow: Integer); override;
    procedure DrawDataCell(ACanvas: TCanvas; const Rect: TRect; Field: TField;
      State: TscGridDrawState); dynamic;
    procedure DrawColumnCell(ACanvas: TCanvas; const Rect: TRect; DataCol: Integer;
      Column: TscColumn; State: TscGridDrawState); dynamic;
    procedure EditButtonClick; dynamic;
    procedure EndLayout;
    procedure EndUpdate;
    function  GetColField(DataCol: Integer): TField;
    function  GetEditLimit: Integer; override;
    function  GetEditMask(ACol, ARow: Longint): string; override;
    function  GetEditStyle(ACol, ARow: Longint): TscGridEditStyle; override;
    function  GetEditText(ACol, ARow: Longint): string; override;
    function  GetFieldValue(ACol: Integer): string;
    function  HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TscGridDrawState): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure InvalidateTitles;
    procedure LayoutChanged; virtual;
    procedure LinkActive(Value: Boolean); virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Scroll(Distance: Integer); virtual;
    procedure SetColumnAttributes; virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function  StoreColumns: Boolean;
    procedure TimedScroll(Direction: TscGridScrollDirection); override;
    procedure TitleClick(Column: TscColumn); dynamic;
    procedure TopLeftChanged; override;
    procedure UpdateScrollBar; virtual;
    function UseRightToLeftAlignmentForField(const AField: TField;
      Alignment: TAlignment): Boolean;
    function BeginColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function EndColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    property Columns: TscDBGridColumns read FColumns write SetColumns;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default True;
    property DataLink: TscGridDataLink read FDataLink;
    property IndicatorOffset: Byte read FIndicatorOffset;
    property LayoutLock: Byte read FLayoutLock;
    property Options: TscDBGridOptions read FOptions write SetOptions
      default [scdgEditing, scdgTitles, scdgIndicator, scdgColumnResize, scdgColLines,
      scdgRowLines, scdgTabs, scdgConfirmDelete, scdgCancelOnExit, scdgTitleClick,
      scdgTitleHotTrack];
    property ParentColor default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property SelectedRows: TscBookmarkList read FBookmarks;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property UpdateLock: Byte read FUpdateLock;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnDrawDataCell: TscDrawDataCellEvent read FOnDrawDataCell
      write FOnDrawDataCell;
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell
      write FOnDrawColumnCell;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write FOnEditButtonClick;
    property OnColumnMoved: TscMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnCellClick: TscDBGridClickEvent read FOnCellClick write FOnCellClick;
    property OnTitleClick: TscDBGridClickEvent read FOnTitleClick write FOnTitleClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawDataCell(ACanvas: TCanvas; const Rect: TRect; Field: TField;
      State: TscGridDrawState);
    procedure DefaultDrawColumnCell(ACanvas: TCanvas; const Rect: TRect; DataCol: Integer;
      Column: TscColumn; State: TscGridDrawState);
    procedure DefaultHandler(var Msg); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ShowPopupEditor(Column: TscColumn; X: Integer = Low(Integer);
      Y: Integer = Low(Integer)); dynamic;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function ValidFieldIndex(FieldIndex: Integer): Boolean;
    function CellRect(ACol, ARow: Longint): TRect;
    function SelectionRect: TRect;
    function SelectionScreenRect: TRect;
    property Selection;
    property EditorMode;
    property FieldCount: Integer read GetFieldCount;
    property Fields[FieldIndex: Integer]: TField read GetFields;
    property SelectedField: TField read GetSelectedField write SetSelectedField;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TscDBGrid = class(TscCustomDBGrid)
  public
    property Canvas;
    property SelectedRows;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns stored False;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawingStyle;
    property Enabled;
    property FixedColor;
    property GradientEndColor;
    property GradientStartColor;
    property Font;
    property ImeMode;
    property ImeName;
    property Options;
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
    property TitleFont;
    property Touch;
    property Visible;
    property StyleElements;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawDataCell;
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
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
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;

const
  IndicatorWidth = 11;

implementation

uses System.Math, System.Types, System.UITypes, Data.DBConsts, Vcl.VDBConsts,
  Vcl.Dialogs, System.RTLConsts, Vcl.Themes, scDrawUtils;

{$R scDBGrids.res}

const
  bmArrow = 'SCDBGARROW';
  bmEdit = 'SCDBEDIT';
  bmInsert = 'SCDBINSERT';
  bmMultiDot = 'SCDBMULTIDOT';
  bmMultiArrow = 'SCDBMULTIARROW';

  MaxMapSize = (MaxInt div 2) div SizeOf(Integer);


procedure RaiseGridError(const S: string);
begin
  raise scEInvalidGridOperation.Create(S);
end;

procedure KillMessage(Wnd: HWnd; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

{ TscDBGridInplaceEdit }

type

  TscDBGridInplaceEdit = class(TscInplaceEditList)
  private
    FDataList: TscDBLookupListBox;
    FUseDataList: Boolean;
    FLookupSource: TDatasource;
  protected
    procedure CloseUp(Accept: Boolean); override;
    procedure DoEditButtonClick; override;
    procedure DropDown; override;
    procedure UpdateContents; override;
  public
    constructor Create(Owner: TComponent); override;
    property  DataList: TscDBLookupListBox read FDataList;
  end;

constructor TscDBGridInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FLookupSource := TDataSource.Create(Self);
end;

procedure TscDBGridInplaceEdit.CloseUp(Accept: Boolean);
var
  MasterField: TField;
  ListValue: Variant;
begin
  if ListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if ActiveList = DataList then
      ListValue := DataList.KeyValue
    else
      if PickList.ItemIndex <> -1 then
        ListValue := PickList.Items[Picklist.ItemIndex];
    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    ListVisible := False;
    if Assigned(FDataList) then
      FDataList.ListSource := nil;
    FLookupSource.Dataset := nil;
    Invalidate;
    if Accept then
      if ActiveList = DataList then
        with TscCustomDBGrid(Grid), Columns[SelectedIndex].Field do
        begin
          MasterField := DataSet.FieldByName(KeyFields);
          if MasterField.CanModify and FDataLink.Edit then
            MasterField.Value := ListValue;
        end
      else
        if (not VarIsNull(ListValue)) and EditCanModify then
          with TscCustomDBGrid(Grid) do
          begin
            SetEditText(Row, Col, ListValue);
            Columns[SelectedIndex].Field.Text := ListValue;
            InvalidateEditor;
          end;
  end;
end;

procedure TscDBGridInplaceEdit.DoEditButtonClick;
begin
  TscCustomDBGrid(Grid).EditButtonClick;
end;

procedure TscDBGridInplaceEdit.DropDown;
var
  Column: TscColumn;
begin
  if not ListVisible then
  begin
    with TscCustomDBGrid(Grid) do
      Column := Columns[SelectedIndex];
    if ActiveList = FDataList then
      with Column.Field do
      begin
        FDataList.Color := Color;
        FDataList.Font := Font;
        {$IFNDEF VER230}
        FDataList.StyleElements := TscCustomDBGrid(Grid).StyleElements;
        {$ENDIF}
        FDataList.RowCount := Column.DropDownRows;
        FLookupSource.DataSet := LookupDataSet;
        FDataList.KeyField := LookupKeyFields;
        FDataList.ListField := LookupResultField;
        FDataList.KeyValue := Null;
        FDataList.ListSource := FLookupSource;
        FDataList.KeyValue := DataSet.FieldByName(KeyFields).Value;
        FDataList.SelectionStyle := Grid.SelectionStyle;
      end
    else if ActiveList = PickList then
    begin
      PickList.Items.Assign(Column.PickList);
      DropDownRows := Column.DropDownRows;
    end;
  end;
  inherited DropDown;
end;

procedure TscDBGridInplaceEdit.UpdateContents;
var
  Column: TscColumn;
begin
  inherited UpdateContents;
  if FUseDataList then
  begin
    if FDataList = nil then
    begin
      FDataList := TscPopupDataList.Create(Self);
      FDataList.Visible := False;
      FDataList.Parent := Self;
      FDataList.OnMouseUp := ListMouseUp;
    end;
    ActiveList := FDataList;
  end;
  with TscCustomDBGrid(Grid) do
    Column := Columns[SelectedIndex];
  Self.ReadOnly := Column.ReadOnly;
  Font.Assign(Column.Font);
  ImeMode := Column.ImeMode;
  ImeName := Column.ImeName;
end;

{ TscGridDataLink }

type
  TIntArray = array[0..MaxMapSize] of Integer;
  PIntArray = ^TIntArray;

constructor TscGridDataLink.Create(AGrid: TscCustomDBGrid);
begin
  inherited Create;
  FGrid := AGrid;
  VisualControl := True;
end;

destructor TscGridDataLink.Destroy;
begin
  ClearMapping;
  inherited Destroy;
end;

function TscGridDataLink.GetDefaultFields: Boolean;
var
  I: Integer;
begin
  Result := True;
  {$IFDEF VER270_UP}
  if DataSet <> nil then Result := lcAutomatic in DataSet.Fields.LifeCycles;
  {$ELSE}
  if DataSet <> nil then Result := DataSet.DefaultFields;
  {$ENDIF}
  if Result and SparseMap then
  for I := 0 to FFieldCount-1 do
    if FFieldMap[I] < 0 then
    begin
      Result := False;
      Exit;
    end;
end;

function TscGridDataLink.GetFields(I: Integer): TField;
begin
  if (0 <= I) and (I < FFieldCount) and (FFieldMap[I] >= 0) then
    Result := DataSet.FieldList[FFieldMap[I]]
  else
    Result := nil;
end;

function TscGridDataLink.AddMapping(const FieldName: string): Boolean;
var
  Field: TField;
  NewSize: Integer;
begin
  Result := True;
  if FFieldCount >= MaxMapSize then RaiseGridError(STooManyColumns);
  if SparseMap then
    Field := DataSet.FindField(FieldName)
  else
    Field := DataSet.FieldByName(FieldName);

  if FFieldCount = Length(FFieldMap) then
  begin
    NewSize := Length(FFieldMap);
    if NewSize = 0 then
      NewSize := 8
    else
      Inc(NewSize, NewSize);
    if (NewSize < FFieldCount) then
      NewSize := FFieldCount + 1;
    if (NewSize > MaxMapSize) then
      NewSize := MaxMapSize;
    SetLength(FFieldMap, NewSize);
  end;
  if Assigned(Field) then
  begin
    FFieldMap[FFieldCount] := Dataset.FieldList.IndexOfObject(Field);
    Field.FreeNotification(FGrid);
  end
  else
    FFieldMap[FFieldCount] := -1;
  Inc(FFieldCount);
end;

procedure TscGridDataLink.ActiveChanged;
begin
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        DatabaseError(SDataSetUnidirectional);
  FGrid.LinkActive(Active);
  FModified := False;
end;

procedure TscGridDataLink.ClearMapping;
begin
  FFieldMap := nil;
  FFieldCount := 0;
end;

procedure TscGridDataLink.Modified;
begin
  FModified := True;
end;

procedure TscGridDataLink.DataSetChanged;
begin
  FGrid.DataChanged;
  FModified := False;
end;

procedure TscGridDataLink.DataSetScrolled(Distance: Integer);
begin
  FGrid.Scroll(Distance);
end;

procedure TscGridDataLink.LayoutChanged;
var
  SaveState: Boolean;
begin
  SaveState := FGrid.FLayoutFromDataset;
  FGrid.FLayoutFromDataset := True;
  try
    FGrid.LayoutChanged;
  finally
    FGrid.FLayoutFromDataset := SaveState;
  end;
  inherited LayoutChanged;
end;

procedure TscGridDataLink.FocusControl(Field: TFieldRef);
begin
  if Assigned(Field) and Assigned(Field^) then
  begin
    FGrid.SelectedField := Field^;
    if (FGrid.SelectedField = Field^) and FGrid.AcquireFocus then
    begin
      Field^ := nil;
      FGrid.ShowEditor;
    end;
  end;
end;

procedure TscGridDataLink.EditingChanged;
begin
  FGrid.EditingChanged;
end;

procedure TscGridDataLink.RecordChanged(Field: TField);
begin
  if FModified and Assigned(Field) and (Field.FieldKind = fkData) and
     (FGrid.SelectedField <> Field) and not FInUpdateData then
    UpdateData;
  FGrid.RecordChanged(Field);
  FModified := False;
end;

procedure TscGridDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified then FGrid.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

function TscGridDataLink.GetMappedIndex(ColIndex: Integer): Integer;
begin
  if (0 <= ColIndex) and (ColIndex < FFieldCount) then
    Result := FFieldMap[ColIndex]
  else
    Result := -1;
end;

procedure TscGridDataLink.Reset;
begin
  if FModified then RecordChanged(nil) else Dataset.Cancel;
end;

function TscGridDataLink.IsAggRow(Value: Integer): Boolean;
begin
  Result := False;
end;

procedure TscGridDataLink.BuildAggMap;
begin
end;

{ TscColumnTitle }
constructor TscColumnTitle.Create(Column: TscColumn);
begin
  inherited Create;
  FColumn := Column;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
end;

destructor TscColumnTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TscColumnTitle.Assign(Source: TPersistent);
begin
  if Source is TscColumnTitle then
  begin
    if sccvTitleAlignment in TscColumnTitle(Source).FColumn.FAssignedValues then
      Alignment := TscColumnTitle(Source).Alignment;
    if sccvTitleColor in TscColumnTitle(Source).FColumn.FAssignedValues then
      Color := TscColumnTitle(Source).Color;
    if sccvTitleCaption in TscColumnTitle(Source).FColumn.FAssignedValues then
      Caption := TscColumnTitle(Source).Caption;
    if sccvTitleFont in TscColumnTitle(Source).FColumn.FAssignedValues then
      Font := TscColumnTitle(Source).Font;
  end
  else
    inherited Assign(Source);
end;

function TscColumnTitle.DefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TscColumnTitle.DefaultColor: TColor;
var
  Grid: TscCustomDBGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TscColumnTitle.DefaultFont: TFont;
var
  Grid: TscCustomDBGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.TitleFont
  else
    Result := FColumn.Font;
end;

function TscColumnTitle.DefaultCaption: string;
var
  Field: TField;
begin
  Field := FColumn.Field;
  if Assigned(Field) then
    Result := Field.DisplayName
  else
    Result := FColumn.FieldName;
end;

procedure TscColumnTitle.FontChanged(Sender: TObject);
begin
  Include(FColumn.FAssignedValues, sccvTitleFont);
  FColumn.Changed(True);
end;

function TscColumnTitle.GetAlignment: TAlignment;
begin
  if sccvTitleAlignment in FColumn.FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TscColumnTitle.GetColor: TColor;
begin
  if sccvTitleColor in FColumn.FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TscColumnTitle.GetCaption: string;
begin
  if sccvTitleCaption in FColumn.FAssignedValues then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TscColumnTitle.GetFont: TFont;
var
  Save: TNotifyEvent;
  Def: TFont;
begin
  if not (sccvTitleFont in FColumn.FAssignedValues) then
  begin
    Def := DefaultFont;
    if (FFont.Handle <> Def.Handle) or (FFont.Color <> Def.Color) then
    begin
      Save := FFont.OnChange;
      FFont.OnChange := nil;
      FFont.Assign(DefaultFont);
      FFont.OnChange := Save;
    end;
  end;
  Result := FFont;
end;

function TscColumnTitle.IsAlignmentStored: Boolean;
begin
  Result := (sccvTitleAlignment in FColumn.FAssignedValues) and
    (FAlignment <> DefaultAlignment);
end;

function TscColumnTitle.IsColorStored: Boolean;
begin
  Result := (sccvTitleColor in FColumn.FAssignedValues) and
    (FColor <> DefaultColor);
end;

function TscColumnTitle.IsFontStored: Boolean;
begin
  Result := (sccvTitleFont in FColumn.FAssignedValues);
end;

function TscColumnTitle.IsCaptionStored: Boolean;
begin
  Result := (sccvTitleCaption in FColumn.FAssignedValues) and
    (FCaption <> DefaultCaption);
end;

procedure TscColumnTitle.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if (sccvTitleFont in FColumn.FAssignedValues) then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TscColumnTitle.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := sccvTitleFont in FColumn.FAssignedValues;
  FColumn.FAssignedValues := FColumn.FAssignedValues - SC_ColumnTitleValues;
  FCaption := '';
  RefreshDefaultFont;
  FColumn.Changed(FontAssigned);
end;

procedure TscColumnTitle.SetAlignment(Value: TAlignment);
begin
  if (sccvTitleAlignment in FColumn.FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FColumn.FAssignedValues, sccvTitleAlignment);
  FColumn.Changed(False);
end;

procedure TscColumnTitle.SetColor(Value: TColor);
begin
  if (sccvTitleColor in FColumn.FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FColumn.FAssignedValues, sccvTitleColor);
  FColumn.Changed(False);
end;

procedure TscColumnTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TscColumnTitle.SetCaption(const Value: string);
var
  Grid: TscCustomDBGrid;
begin
  if Column.IsStored then
  begin
    if (sccvTitleCaption in FColumn.FAssignedValues) and (Value = FCaption) then Exit;
    FCaption := Value;
    Include(Column.FAssignedValues, sccvTitleCaption);
    Column.Changed(False);
  end
  else
  begin
    Grid := Column.GetGrid;
    if Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Column.Field) then
      Column.Field.DisplayLabel := Value;
  end;
end;

{ TscColumn }

constructor TscColumn.Create(Collection: TCollection);
var
  Grid: TscCustomDBGrid;
begin
  Grid := nil;
  if Assigned(Collection) and (Collection is TscDBGridColumns) then
    Grid := TscDBGridColumns(Collection).Grid;
  if Assigned(Grid) then Grid.BeginLayout;
  try
    inherited Create(Collection);
    FDropDownRows := 7;
    FButtonStyle := sccbsAuto;
    FFont := TFont.Create;
    FFont.Assign(DefaultFont);
    FFont.OnChange := FontChanged;
    FImeMode := imDontCare;
    FImeName := Screen.DefaultIme;
    FTitle := CreateTitle;
    FVisible := True;
    FExpanded := True;
    FStored := True;
  finally
    if Assigned(Grid) then Grid.EndLayout;
  end;
end;

destructor TscColumn.Destroy;
begin
  FTitle.Free;
  FFont.Free;
  FPickList.Free;
  inherited Destroy;
end;

procedure TscColumn.Assign(Source: TPersistent);
begin
  if Source is TscColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      RestoreDefaults;
      FieldName := TscColumn(Source).FieldName;
      if sccvColor in TscColumn(Source).AssignedValues then
        Color := TscColumn(Source).Color;
      if sccvWidth in TscColumn(Source).AssignedValues then
        Width := TscColumn(Source).Width;
      if sccvFont in TscColumn(Source).AssignedValues then
        Font := TscColumn(Source).Font;
      if sccvImeMode in TscColumn(Source).AssignedValues then
        ImeMode := TscColumn(Source).ImeMode;
      if sccvImeName in TscColumn(Source).AssignedValues then
        ImeName := TscColumn(Source).ImeName;
      if sccvAlignment in TscColumn(Source).AssignedValues then
        Alignment := TscColumn(Source).Alignment;
      if sccvReadOnly in TscColumn(Source).AssignedValues then
        ReadOnly := TscColumn(Source).ReadOnly;
      Title := TscColumn(Source).Title;
      DropDownRows := TscColumn(Source).DropDownRows;
      ButtonStyle := TscColumn(Source).ButtonStyle;
      PickList := TscColumn(Source).PickList;
      PopupMenu := TscColumn(Source).PopupMenu;
      FVisible := TscColumn(Source).FVisible;
      FExpanded := TscColumn(Source).FExpanded;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TscColumn.CreateTitle: TscColumnTitle;
begin
  Result := TscColumnTitle.Create(Self);
end;

function TscColumn.DefaultAlignment: TAlignment;
begin
  if Assigned(Field) then
    Result := FField.Alignment
  else
    Result := taLeftJustify;
end;

function TscColumn.DefaultColor: TColor;
var
  Grid: TscCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.FInternalColor
  else
    Result := clWindow;
end;

function TscColumn.DefaultFont: TFont;
var
  Grid: TscCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TscColumn.DefaultImeMode: TImeMode;
var
  Grid: TscCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeMode
  else
    Result := FImeMode;
end;

function TscColumn.DefaultImeName: TImeName;
var
  Grid: TscCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeName
  else
    Result := FImeName;
end;

function TscColumn.DefaultReadOnly: Boolean;
var
  Grid: TscCustomDBGrid;
begin
  Grid := GetGrid;
  Result := (Assigned(Grid) and Grid.ReadOnly) or
    (Assigned(Field) and FField.ReadOnly);
end;

function TscColumn.DefaultWidth: Integer;
var
  W: Integer;
  RestoreCanvas: Boolean;
  TempDc: HDC;
  TM: TTextMetric;
begin
  if GetGrid = nil then
  begin
    Result := 64;
    Exit;
  end;
  with GetGrid do
  begin
    if Assigned(Field) then
    begin
      RestoreCanvas := not HandleAllocated;
      if RestoreCanvas then
        Canvas.Handle := GetDC(0);
      try
        Canvas.Font := Self.Font;
        GetTextMetrics(Canvas.Handle, TM);
        Result := Field.DisplayWidth * (Canvas.TextWidth('0') - TM.tmOverhang)
          + TM.tmOverhang + 4;
        if scdgTitles in Options then
        begin
          Canvas.Font := Title.Font;
          W := Canvas.TextWidth(Title.Caption) + 4;
          if Result < W then
            Result := W;
        end;
      finally
        if RestoreCanvas then
        begin
          TempDc := Canvas.Handle;
          Canvas.Handle := 0;
          ReleaseDC(0, TempDc);
        end;
      end;
    end
    else
      Result := DefaultColWidth;
  end;
end;

procedure TscColumn.FontChanged;
begin
  Include(FAssignedValues, sccvFont);
  Title.RefreshDefaultFont;
  Changed(False);
end;

function TscColumn.GetAlignment: TAlignment;
begin
  if sccvAlignment in FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TscColumn.GetColor: TColor;
begin
  if sccvColor in FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TscColumn.GetExpanded: Boolean;
begin
  Result := FExpanded and Expandable;
end;

function TscColumn.GetField: TField;
var
  Grid: TscCustomDBGrid;
begin
  Grid := GetGrid;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(Grid) and
    Assigned(Grid.DataLink.DataSet) then
  with Grid.Datalink.Dataset do
    {$IFDEF VER270_UP}
    if Active or (lcPersistent in Fields.LifeCycles) then
    {$ELSE}
    if Active or (not DefaultFields) then
    {$ENDIF}
      SetField(FindField(FieldName));
  Result := FField;
end;

function TscColumn.GetFont: TFont;
var
  Save: TNotifyEvent;
begin
  if not (sccvFont in FAssignedValues) and (FFont.Handle <> DefaultFont.Handle) then
  begin
    Save := FFont.OnChange;
    FFont.OnChange := nil;
    FFont.Assign(DefaultFont);
    FFont.OnChange := Save;
  end;
  Result := FFont;
end;

function TscColumn.GetGrid: TscCustomDBGrid;
begin
  if Assigned(Collection) and (Collection is TscDBGridColumns) then
    Result := TscDBGridColumns(Collection).Grid
  else
    Result := nil;
end;

function TscColumn.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TscColumn.GetImeMode: TImeMode;
begin
  if sccvImeMode in FAssignedValues then
    Result := FImeMode
  else
    Result := DefaultImeMode;
end;

function TscColumn.GetImeName: TImeName;
begin
  if sccvImeName in FAssignedValues then
    Result := FImeName
  else
    Result := DefaultImeName;
end;

function TscColumn.GetParentColumn: TscColumn;
var
  Col: TscColumn;
  Fld: TField;
  I: Integer;
begin
  Result := nil;
  Fld := Field;
  if (Fld <> nil) and (Fld.ParentField <> nil) and (Collection <> nil) then
    for I := Index - 1 downto 0 do
    begin
      Col := TscColumn(Collection.Items[I]);
      if Fld.ParentField = Col.Field then
      begin
        Result := Col;
        Exit;
      end;
    end;
end;

function TscColumn.GetPickList: TStrings;
begin
  if FPickList = nil then
    FPickList := TStringList.Create;
  Result := FPickList;
end;

function TscColumn.GetReadOnly: Boolean;
begin
  if sccvReadOnly in FAssignedValues then
    Result := FReadOnly
  else
    Result := DefaultReadOnly;
end;

function TscColumn.GetShowing: Boolean;
var
  Col: TscColumn;
begin
  Result := not Expanded and Visible;
  if Result then
  begin
    Col := Self;
    repeat
      Col := Col.ParentColumn;
    until (Col = nil) or not Col.Expanded;
    Result := Col = nil;
  end;
end;

function TscColumn.GetVisible: Boolean;
var
  Col: TscColumn;
begin
  Result := FVisible;
  if Result then
  begin
    Col := ParentColumn;
    Result := Result and ((Col = nil) or Col.Visible);
  end;
end;

function TscColumn.GetWidth: Integer;
begin
  if not Showing then
    Result := -1
  else if sccvWidth in FAssignedValues then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

function TscColumn.IsAlignmentStored: Boolean;
begin
  Result := (sccvAlignment in FAssignedValues) and (FAlignment <> DefaultAlignment);
end;

function TscColumn.IsColorStored: Boolean;
begin
  Result := (sccvColor in FAssignedValues) and (FColor <> DefaultColor);
end;

function TscColumn.IsFontStored: Boolean;
begin
  Result := (sccvFont in FAssignedValues);
end;

function TscColumn.IsImeModeStored: Boolean;
begin
  Result := (sccvImeMode in FAssignedValues) and (FImeMode <> DefaultImeMode);
end;

function TscColumn.IsImeNameStored: Boolean;
begin
  Result := (sccvImeName in FAssignedValues) and (FImeName <> DefaultImeName);
end;

function TscColumn.IsReadOnlyStored: Boolean;
begin
  Result := (sccvReadOnly in FAssignedValues) and (FReadOnly <> DefaultReadOnly);
end;

function TscColumn.IsWidthStored: Boolean;
begin
  Result := (sccvWidth in FAssignedValues) and (FWidth <> DefaultWidth);
end;

procedure TscColumn.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if sccvFont in FAssignedValues then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TscColumn.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := sccvFont in FAssignedValues;
  FTitle.RestoreDefaults;
  FAssignedValues := [];
  RefreshDefaultFont;
  FPickList.Free;
  FPickList := nil;
  ButtonStyle := sccbsAuto;
  Changed(FontAssigned);
end;

procedure TscColumn.SetAlignment(Value: TAlignment);
var
  Grid: TscCustomDBGrid;
begin
  if IsStored then
  begin
    if (sccvAlignment in FAssignedValues) and (Value = FAlignment) then Exit;
    FAlignment := Value;
    Include(FAssignedValues, sccvAlignment);
    Changed(False);
  end
  else
  begin
    Grid := GetGrid;
    if Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Field) then
      Field.Alignment := Value;
  end;
end;

procedure TscColumn.SetButtonStyle(Value: TscColumnButtonStyle);
begin
  if Value = FButtonStyle then Exit;
  FButtonStyle := Value;
  Changed(False);
end;

procedure TscColumn.SetColor(Value: TColor);
begin
  if (sccvColor in FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FAssignedValues, sccvColor);
  Changed(False);
end;

procedure TscColumn.SetField(Value: TField);
begin
  if FField = Value then Exit;
  if Assigned(FField) and (GetGrid <> nil) then
    FField.RemoveFreeNotification(GetGrid);
  if Assigned(Value) and (csDestroying in Value.ComponentState) then
    Value := nil;
  if FField = Value then Exit;
  FField := Value;
  if Assigned(Value) then
  begin
    if GetGrid <> nil then
      FField.FreeNotification(GetGrid);
    FFieldName := Value.FullName;
  end;
  if not IsStored then
  begin
    if Value = nil then
      FFieldName := '';
    RestoreDefaults;
  end;
  Changed(False);
end;

procedure TscColumn.SetFieldName(const Value: string);
var
  AField: TField;
  Grid: TscCustomDBGrid;
begin
  AField := nil;
  Grid := GetGrid;
  if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) and
    not (csLoading in Grid.ComponentState) and (Length(Value) > 0) then
      AField := Grid.DataLink.DataSet.FindField(Value);
  FFieldName := Value;
  SetField(AField);
  Changed(False);
end;

procedure TscColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, sccvFont);
  Changed(False);
end;

procedure TscColumn.SetImeMode(Value: TImeMode);
begin
  if (sccvImeMode in FAssignedValues) or (Value <> DefaultImeMode) then
  begin
    FImeMode := Value;
    Include(FAssignedValues, sccvImeMode);
  end;
  Changed(False);
end;

procedure TscColumn.SetImeName(Value: TImeName);
begin
  if (sccvImeName in FAssignedValues) or (Value <> DefaultImeName) then
  begin
    FImeName := Value;
    Include(FAssignedValues, sccvImeName);
  end;
  Changed(False);
end;

procedure TscColumn.SetIndex(Value: Integer);
var
  Grid: TscCustomDBGrid;
  Fld: TField;
  I, OldIndex: Integer;
  Col: TscColumn;
begin
  OldIndex := Index;
  Grid := GetGrid;

  if IsStored then
  begin
    Grid.BeginLayout;
    try
      I := OldIndex + 1;
      while (I < Collection.Count) and (TscColumn(Collection.Items[I]).ParentColumn = Self) do
        Inc(I);
      Dec(I);
      if OldIndex > Value then
      begin
        while I > OldIndex do
        begin
          Collection.Items[I].Index := Value;
          Inc(OldIndex);
        end;
        inherited SetIndex(Value);
      end
      else
      begin
        inherited SetIndex(Value);
        while I > OldIndex do
        begin
          Collection.Items[OldIndex].Index := Value;
          Dec(I);
        end;
      end;
    finally
      Grid.EndLayout;
    end;
  end
  else
  begin
    if (Grid <> nil) and Grid.Datalink.Active then
    begin
      if Grid.AcquireLayoutLock then
      try
        Col := Grid.ColumnAtDepth(Grid.Columns[Value], Depth);
        if (Col <> nil) then
        begin
          Fld := Col.Field;
          if Assigned(Fld) then
            Field.Index := Fld.Index;
        end;
      finally
        Grid.EndLayout;
      end;
    end;
    inherited SetIndex(Value);
  end;
end;

procedure TscColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
  begin
    FPickList.Free;
    FPickList := nil;
    Exit;
  end;
  PickList.Assign(Value);
end;

procedure TscColumn.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then Value.FreeNotification(GetGrid);
end;

procedure TscColumn.SetReadOnly(Value: Boolean);
var
  Grid: TscCustomDBGrid;
begin
  Grid := GetGrid;
  if not IsStored and Assigned(Grid) and Grid.Datalink.Active and Assigned(Field) then
    Field.ReadOnly := Value
  else
  begin
    if (sccvReadOnly in FAssignedValues) and (Value = FReadOnly) then Exit;
    FReadOnly := Value;
    Include(FAssignedValues, sccvReadOnly);
    Changed(False);
  end;
end;

procedure TscColumn.SetTitle(Value: TscColumnTitle);
begin
  FTitle.Assign(Value);
end;

procedure TscColumn.SetWidth(Value: Integer);
var
  Grid: TscCustomDBGrid;
  TM: TTextMetric;
  DoSetWidth: Boolean;
begin
  DoSetWidth := IsStored;
  if not DoSetWidth then
  begin
    Grid := GetGrid;
    if Assigned(Grid) then
    begin
      if Grid.HandleAllocated and Assigned(Field) and Grid.FUpdateFields then
      with Grid do
      begin
        Canvas.Font := Self.Font;
        GetTextMetrics(Canvas.Handle, TM);
        Field.DisplayWidth := (Value + (TM.tmAveCharWidth div 2) - TM.tmOverhang - 3)
          div TM.tmAveCharWidth;
      end;
      if (not Grid.FLayoutFromDataset) or (sccvWidth in FAssignedValues) then
        DoSetWidth := True;
    end
    else
      DoSetWidth := True;
  end;
  if DoSetWidth then
  begin
    if (not (sccvWidth in FAssignedValues) or (Value <> DefaultWidth))
      and (Value <> -1) then
    begin
      FWidth := Value;
      Include(FAssignedValues, sccvWidth);
    end;
    Changed(False);
  end;
end;

procedure TscColumn.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

procedure TscColumn.SetExpanded(Value: Boolean);
const
  Direction: array [Boolean] of ShortInt = (-1,1);
var
  Grid: TscCustomDBGrid;
  WasShowing: Boolean;
begin
  if Value <> FExpanded then
  begin
    Grid := GetGrid;
    WasShowing := (Grid <> nil) and Grid.Columns[Grid.SelectedIndex].Showing;
    FExpanded := Value;
    Changed(True);
    if (Grid <> nil) and WasShowing then
    begin
      if not Grid.Columns[Grid.SelectedIndex].Showing then
        Grid.MoveCol(Grid.Col, Direction[FExpanded]);
    end;
  end;
end;

function TscColumn.Depth: Integer;
var
  Col: TscColumn;
begin
  Result := 0;
  Col := ParentColumn;
  if Col <> nil then Result := Col.Depth + 1;
end;

function TscColumn.GetExpandable: Boolean;
var
  Fld: TField;
begin
  Fld := Field;
  Result := (Fld <> nil) and (Fld.DataType in [ftADT, ftArray]);
end;

{ TscDBGridColumns }

constructor TscDBGridColumns.Create(Grid: TscCustomDBGrid; ColumnClass: TscColumnClass);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TscDBGridColumns.Add: TscColumn;
begin
  Result := TscColumn(inherited Add);
end;

function TscDBGridColumns.GetColumn(Index: Integer): TscColumn;
begin
  Result := TscColumn(inherited Items[Index]);
end;

function TscDBGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TscDBGridColumns.LoadFromFile(const Filename: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

type
  TscColumnsWrapper = class(TComponent)
  private
    FColumns: TscDBGridColumns;
  published
    property Columns: TscDBGridColumns read FColumns write FColumns;
  end;

procedure TscDBGridColumns.LoadFromStream(S: TStream);
var
  Wrapper: TscColumnsWrapper;
begin
  Wrapper := TscColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := FGrid.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;
end;

procedure TscDBGridColumns.RestoreDefaults;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count-1 do
      Items[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TscDBGridColumns.RebuildColumns;

  procedure AddFields(Fields: TFields; Depth: Integer);
  var
    I: Integer;
  begin
    Inc(Depth);
    for I := 0 to Fields.Count-1 do
    begin
      Add.FieldName := Fields[I].FullName;
      if Fields[I].DataType in [ftADT, ftArray] then
        AddFields((Fields[I] as TObjectField).Fields, Depth);
    end;
  end;

begin
  if Assigned(FGrid) and Assigned(FGrid.DataSource) and
    Assigned(FGrid.Datasource.Dataset) then
  begin
    FGrid.BeginLayout;
    try
      Clear;
      AddFields(FGrid.Datasource.Dataset.Fields, 0);
    finally
      FGrid.EndLayout;
    end
  end
  else
    Clear;
end;

procedure TscDBGridColumns.SaveToFile(const Filename: string);
var
  S: TStream;
begin
  S := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TscDBGridColumns.SaveToStream(S: TStream);
var
  Wrapper: TscColumnsWrapper;
begin
  Wrapper := TscColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;

procedure TscDBGridColumns.SetColumn(Index: Integer; Value: TscColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TscDBGridColumns.SetState(NewState: TscDBGridColumnsState);
begin
  if NewState = State then Exit;
  if NewState = sccsDefault then
    Clear
  else
    RebuildColumns;
end;

procedure TscDBGridColumns.Update(Item: TCollectionItem);
var
  Raw: Integer;
begin
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;
  if Item = nil then
  begin
    FGrid.LayoutChanged;
  end
  else
  begin
    Raw := FGrid.DataToRawColumn(Item.Index);
    FGrid.InvalidateCol(Raw);
    FGrid.ColWidths[Raw] := TscColumn(Item).Width;
  end;
end;

function TscDBGridColumns.InternalAdd: TscColumn;
begin
  Result := Add;
  Result.IsStored := False;
end;

function TscDBGridColumns.GetState: TscDBGridColumnsState;
begin
  Result := TscDBGridColumnsState((Count > 0) and Items[0].IsStored);
end;

{ TscBookmarkList }

constructor TscBookmarkList.Create(AGrid: TscCustomDBGrid);
begin
  inherited Create;
  SetLength(FList, 0);
  FGrid := AGrid;
end;

destructor TscBookmarkList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TscBookmarkList.Clear;
begin
  if Length(FList) = 0 then Exit;
  SetLength(FList, 0);
  FGrid.Invalidate;
end;

function TscBookmarkList.Compare(const Item1, Item2: TBookmark): Integer;
begin
  with FGrid.Datalink.Datasource.Dataset do
    Result := CompareBookmarks(TBookmark(Item1), TBookmark(Item2));
end;

function TscBookmarkList.CurrentRow: TBookmark;
begin
  if not FLinkActive then RaiseGridError(sDataSetClosed);
  Result := FGrid.Datalink.Datasource.Dataset.Bookmark;
end;

function TscBookmarkList.GetCurrentRowSelected: Boolean;
var
  Index: Integer;
begin
  Result := Find(CurrentRow, Index);
end;

function TscBookmarkList.Find(const Item: TBookmark; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  if (Item = FCache) and (FCacheIndex >= 0) then
  begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := Length(FList) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(FList[I], Item);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
  FCache := Item;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TscBookmarkList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TscBookmarkList.GetItem(Index: Integer): TBookmark;
begin
  Result := FList[Index];
end;

function TscBookmarkList.IndexOf(const Item: TBookmark): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

procedure TscBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;
  FLinkActive := Value;
end;

procedure TscBookmarkList.Delete;
var
  I: Integer;
begin
  with FGrid.Datalink.Datasource.Dataset do
  begin
    DisableControls;
    try
      for I := Length(FList)-1 downto 0 do
      begin
        Bookmark := FList[I];
        Delete;
        DeleteItem(I);
      end;
    finally
      EnableControls;
    end;
  end;
end;

function TscBookmarkList.Refresh: Boolean;
var
  I: Integer;
begin
  Result := False;
  with FGrid.DataLink.Datasource.Dataset do
  try
    CheckBrowseMode;
    for I := Length(FList) - 1 downto 0 do
      if not BookmarkValid(TBookmark(FList[I])) then
      begin
        Result := True;
        DeleteItem(I);
      end;
  finally
    UpdateCursorPos;
    if Result then FGrid.Invalidate;
  end;
end;

procedure TscBookmarkList.DeleteItem(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.Create(SListIndexError);
  Temp := FList[Index];
  FList[Index] := nil;
  if Index < Count-1 then
  begin
    System.Move(FList[Index + 1], FList[Index],
      (Count - Index - 1) * SizeOf(Pointer));
    PPointer(@FList[Count-1])^ := nil;
  end;
  SetLength(FList, Count-1);
  DataChanged(Temp);
end;

procedure TscBookmarkList.InsertItem(Index: Integer; Item: TBookmark);
begin
  if (Index < 0) or (Index > Count) then
    raise EListError.Create(SListIndexError);
  SetLength(FList, Count + 1);
  if Index < Count - 1 then
  begin
    Move(FList[Index], FList[Index + 1],
      (Count - Index - 1) * SizeOf(Pointer));
    PPointer(@FList[Index])^ := nil;
  end;
  FList[Index] := Item;
  DataChanged(TObject(Item));
end;

procedure TscBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index: Integer;
  Current: TBookmark;
begin
  Current := CurrentRow;
  if (Length(Current) = 0) or (Find(Current, Index) = Value) then Exit;
  if Value then
    InsertItem(Index, Current)
  else
    DeleteItem(Index);
  FGrid.InvalidateRow(FGrid.Row);
end;

procedure TscBookmarkList.DataChanged(Sender: TObject);
begin
  FCache := nil;
  FCacheIndex := -1;
end;


{ TscCustomDBGrid }

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; ARightToLeft: Boolean);
var
  Left: Integer;
begin
  if ARightToLeft then
    ChangeBiDiModeAlignment(Alignment);
  case Alignment of
    taLeftJustify:
      Left := ARect.Left + DX;
    taRightJustify:
      Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
   else
     Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
       - (ACanvas.TextWidth(Text) shr 1);
  end;
  if ACanvas.Brush.Style <> bsClear then
    ACanvas.Brush.Style := bsClear;
  ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
end;

constructor TscCustomDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  FOldScrollBarVisible := False;
  FFullRePaint := False;
  FAcquireFocus := True;
  FIndicators := TscImageCollection.Create(Self);
  InitResImages;
  FTouchBegin := 0;
  FTouchEnd := 0;
  FTitleOffset := 1;
  FIndicatorOffset := 1;
  FUpdateFields := True;
  FOptions := [scdgEditing, scdgTitles, scdgIndicator, scdgColumnResize,
    scdgColLines, scdgRowLines, scdgTabs, scdgConfirmDelete, scdgCancelOnExit,
    scdgTitleClick, scdgTitleHotTrack];
  if SysLocale.PriLangID = LANG_KOREAN then
    Include(FOptions, scdgAlwaysShowEditor);
  DesignOptionsBoost := [scgoColSizing];
  VirtualView := True;
  ScrollBars := ssHorizontal;
  inherited Options := [scgoFixedHorzLine, scgoFixedVertLine, scgoHorzLine,
    scgoVertLine, scgoColSizing, scgoColMoving, scgoTabs, scgoEditing, scgoFixedRowClick,
    scgoFixedHotTrack];
  FColumns := CreateColumns;
  FVisibleColumns := TList.Create;
  inherited RowCount := 2;
  inherited ColCount := 2;
  FDataLink := CreateDataLink;
  Color := clWindow;
  ParentColor := False;
  FTitleFont := TFont.Create;
  FTitleFont.OnChange := TitleFontChanged;
  FSaveCellExtents := False;
  FUserChange := True;
  FDefaultDrawing := True;
  FBookmarks := TscBookmarkList.Create(Self);
  FNeedRestoreImeName := False;
  HideEditor;
end;

destructor TscCustomDBGrid.Destroy;
begin
  FColumns.Free;
  FColumns := nil;
  FVisibleColumns.Free;
  FVisibleColumns := nil;
  FDataLink.Free;
  FDataLink := nil;
  FIndicators.Free;
  FTitleFont.Free;
  FTitleFont := nil;
  FBookmarks.Free;
  FBookmarks := nil;
  inherited Destroy;
end;

procedure TscCustomDBGrid.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
var
  I: Integer;
begin
  FStopUpdateColums := True;
  inherited;
  InitResImages;
  for I := 0 to FColumns.Count -1 do
    Columns[I].Width := MulDiv(FColumns[I].Width, M, D);
  FStopUpdateColums := False;
  FTitleFont.Height := MulDiv(FTitleFont.Height, M, D);
end;

procedure TscCustomDBGrid.InitResImages;
var
  ScaleF: Double;
  Scale: String;
  Item: TscImageCollectionItem;
begin
  if FIndicators = nil then Exit;
  FIndicatorsLoading := True;
  FIndicators.Images.Clear;

  ScaleF := 1;
  Scale := '';

  if (FScaleFactor > 2.5) then
  begin
    Scale := '_200';
    if SC_SCALERESOURCES then
      ScaleF := FScaleFactor / 2;
  end
  else
  if (FScaleFactor >= 2) and (FScaleFactor <= 2.5) then
  begin
    Scale := '_200';
    ScaleF := 1;
  end
  else
  if FScaleFactor >= 1.5 then
  begin
    Scale := '_150';
    ScaleF := 1;
  end;

  Item := FIndicators.Images.Add;
  Item.LoadPngFromResourceName(HInstance, bmArrow + Scale, ScaleF);
  Item.DrawStyle := idsCenter;
  Item.CheckBitmapOptions;

  Item := FIndicators.Images.Add;
  Item.LoadPngFromResourceName(HInstance, bmEdit + Scale, ScaleF);
  Item.DrawStyle := idsCenter;
  Item.CheckBitmapOptions;

  Item := FIndicators.Images.Add;
  Item.LoadPngFromResourceName(HInstance, bmInsert + Scale, ScaleF);
  Item.DrawStyle := idsCenter;
  Item.CheckBitmapOptions;

  Item := FIndicators.Images.Add;
  Item.LoadPngFromResourceName(HInstance, bmMultiDot + Scale, ScaleF);
  Item.DrawStyle := idsCenter;
  Item.CheckBitmapOptions;

  Item := FIndicators.Images.Add;
  Item.LoadPngFromResourceName(HInstance, bmMultiArrow + Scale, ScaleF);
  Item.DrawStyle := idsCenter;
  Item.CheckBitmapOptions;

  FIndicatorsLoading := False;
end;

function TscCustomDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or (InplaceEditor <> nil) and InplaceEditor.Focused;
  end;
end;

function TscCustomDBGrid.RawToDataColumn(ACol: Integer): Integer;
begin
  Result := ACol - FIndicatorOffset;
end;

function TscCustomDBGrid.DataToRawColumn(ACol: Integer): Integer;
begin
  Result := ACol + FIndicatorOffset;
end;

function TscCustomDBGrid.AcquireLayoutLock: Boolean;
begin
  Result := (FUpdateLock = 0) and (FLayoutLock = 0);
  if Result then BeginLayout;
end;

procedure TscCustomDBGrid.BeginLayout;
begin
  BeginUpdate;
  if FLayoutLock = 0 then Columns.BeginUpdate;
  Inc(FLayoutLock);
end;

procedure TscCustomDBGrid.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TscCustomDBGrid.CancelLayout;
begin
  if FLayoutLock > 0 then
  begin
    if FLayoutLock = 1 then
      Columns.EndUpdate;
    Dec(FLayoutLock);
    EndUpdate;
  end;
end;

function TscCustomDBGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  with Columns[SelectedIndex] do
    Result := FDatalink.Active and Assigned(Field) and Field.IsValidChar(Key);
end;

function TscCustomDBGrid.CanEditModify: Boolean;
begin
  Result := False;
  if not ReadOnly and FDatalink.Active and not FDatalink.Readonly then
  with Columns[SelectedIndex] do
    if (not ReadOnly) and Assigned(Field) and Field.CanModify
      and (not (Field.DataType in ftNonTextTypes) or Assigned(Field.OnSetText)) then
    begin
      FDatalink.Edit;
      Result := FDatalink.Editing;
      if Result then FDatalink.Modified;
    end;
end;

function TscCustomDBGrid.CanEditShow: Boolean;
begin
  Result := (LayoutLock = 0) and inherited CanEditShow;
end;

procedure TscCustomDBGrid.CellClick(Column: TscColumn);
begin
  if Assigned(FOnCellClick) then FOnCellClick(Column);
end;

procedure TscCustomDBGrid.ColEnter;
begin
  UpdateIme;
  if Assigned(FOnColEnter) then FOnColEnter(Self);
end;

procedure TscCustomDBGrid.ColExit;
begin
  if Assigned(FOnColExit) then FOnColExit(Self);
end;

procedure TscCustomDBGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  FromIndex := RawToDataColumn(FromIndex);
  ToIndex := RawToDataColumn(ToIndex);
  Columns[FromIndex].Index := ToIndex;
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

procedure TscCustomDBGrid.ColWidthsChanged;
var
  I: Integer;
begin
  inherited ColWidthsChanged;
  if (FDatalink.Active or (FColumns.State = sccsCustomized)) and
    AcquireLayoutLock then
  try
    for I := FIndicatorOffset to ColCount - 1 do
      FColumns[I - FIndicatorOffset].Width := ColWidths[I];
  finally
    EndLayout;
  end;
end;

function TscCustomDBGrid.CreateColumns: TscDBGridColumns;
begin
  Result := TscDBGridColumns.Create(Self, TscColumn);
end;

function TscCustomDBGrid.CreateDataLink: TscGridDataLink;
begin
  Result := TscGridDataLink.Create(Self);
end;

function TscCustomDBGrid.CreateEditor: TscInplaceEdit;
begin
  Result := TscDBGridInplaceEdit.Create(Self);
end;

procedure TscCustomDBGrid.CreateWnd;
begin
  BeginUpdate;
  try
    inherited CreateWnd;
  finally
    EndUpdate;
  end;
  UpdateRowCount;
  UpdateActive;
  UpdateScrollBar;
  FOriginalImeName := ImeName;
  FOriginalImeMode := ImeMode;
end;

procedure TscCustomDBGrid.DataChanged;
begin
  if not HandleAllocated then Exit;
  UpdateRowCount;
  UpdateScrollBar;
  UpdateActive;
  InvalidateEditor;
  ValidateRect(Handle, nil);
  Invalidate;
end;

function TscCustomDBGrid.SelectionRect: TRect;
var
  X, Y: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  X := Selection.Left;
  Y := Selection.Bottom;
  Result := CellRect(X, Y);
end;

function TscCustomDBGrid.SelectionScreenRect: TRect;
var
  P: TPoint;
begin
  P := ClientToScreen(Point(0, 0));
  Result := SelectionRect;
  OffsetRect(Result, P.X, P.Y);
end;

function TscCustomDBGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

procedure TscCustomDBGrid.DefaultHandler(var Msg);
var
  P: TPopupMenu;
  Cell: TscGridCoord;
begin
  inherited DefaultHandler(Msg);
  if TMessage(Msg).Msg = wm_RButtonUp then
    with TWMRButtonUp(Msg) do
    begin
      Cell := MouseCoord(XPos, YPos);
      if (Cell.X < FIndicatorOffset) or (Cell.Y < 0) then Exit;
      P := Columns[RawToDataColumn(Cell.X)].PopupMenu;
      if (P <> nil) and P.AutoPopup then
      begin
        SendCancelMode(nil);
        P.PopupComponent := Self;
        with ClientToScreen(SmallPointToPoint(Pos)) do
          P.Popup(X, Y);
        Result := 1;
      end;
    end;
end;

procedure TscCustomDBGrid.DeferLayout;
var
  M: TMsg;
begin
  if HandleAllocated and
    not PeekMessage(M, Handle, CM_DEFERLAYOUT, CM_DEFERLAYOUT, pm_NoRemove) then
    PostMessage(Handle, CM_DEFERLAYOUT, 0, 0);
  CancelLayout;
end;

procedure TscCustomDBGrid.DefineFieldMap;
var
  I: Integer;
begin
  if FColumns.State = sccsCustomized then
  begin
    DataLink.SparseMap := True;
    for I := 0 to FColumns.Count-1 do
      FDataLink.AddMapping(FColumns[I].FieldName);
  end
  else
  begin
    FDataLink.SparseMap := False;
    with Datalink.Dataset do
      for I := 0 to FieldList.Count - 1 do
        with FieldList[I] do if Visible then Datalink.AddMapping(FullName);
  end;
end;

function TscCustomDBGrid.UseRightToLeftAlignmentForField(const AField: TField;
  Alignment: TAlignment): Boolean;
begin
  Result := False;
  if BidiMode = bdRightToLeft then
    Result := OkToChangeFieldAlignment(AField, Alignment);
end;

procedure TscCustomDBGrid.DefaultDrawDataCell(ACanvas: TCanvas; const Rect: TRect; Field: TField;
  State: TscGridDrawState);
var
  Alignment: TAlignment;
  Value: string;
begin
  Alignment := taLeftJustify;
  Value := '';
  if Assigned(Field) then
  begin
    Alignment := Field.Alignment;
    Value := Field.DisplayText;
  end;
  WriteText(ACanvas, Rect, 2, 2, Value, Alignment,
    UseRightToLeftAlignmentForField(Field, Alignment));
end;

procedure TscCustomDBGrid.DefaultDrawColumnCell(ACanvas: TCanvas; const Rect: TRect;
  DataCol: Integer; Column: TscColumn; State: TscGridDrawState);
var
  Value: string;
begin
  Value := '';
  if Assigned(Column.Field) then
    Value := Column.Field.DisplayText;
  WriteText(ACanvas, Rect, 2, 2, Value, Column.Alignment,
    UseRightToLeftAlignmentForField(Column.Field, Column.Alignment));
end;

procedure TscCustomDBGrid.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;

procedure TscCustomDBGrid.WriteColumns(Writer: TWriter);
begin
  if Columns.State = sccsCustomized then
    Writer.WriteCollection(Columns)
  else
    Writer.WriteCollection(nil);
end;

procedure TscCustomDBGrid.DefineProperties(Filer: TFiler);
var
  StoreIt: Boolean;
  vState: TscDBGridColumnsState;
begin
  vState := Columns.State;
  if Filer.Ancestor = nil then
    StoreIt := vState = sccsCustomized
  else
    if vState <> TscCustomDBGrid(Filer.Ancestor).Columns.State then
      StoreIt := True
    else
      StoreIt := (vState = sccsCustomized) and
        (not CollectionsEqual(Columns, TscCustomDBGrid(Filer.Ancestor).Columns, Self, TscCustomDBGrid(Filer.Ancestor)));

  Filer.DefineProperty('Columns', ReadColumns, WriteColumns, StoreIt);
end;

function TscCustomDBGrid.ColumnAtDepth(Col: TscColumn; ADepth: Integer): TscColumn;
begin
  Result := Col;
  while (Result <> nil) and (Result.Depth > ADepth) do
    Result := Result.ParentColumn;
end;

function TscCustomDBGrid.CalcTitleRect(Col: TscColumn; ARow: Integer;
  var MasterCol: TscColumn): TRect;
var
  I,J: Integer;
  DrawInfo: TscGridDrawInfo;
  InBiDiMode: Boolean;
begin
  MasterCol := ColumnAtDepth(Col, ARow);
  if MasterCol = nil then Exit;

  I := DataToRawColumn(MasterCol.Index);
  if I >= LeftCol then
    J := MasterCol.Depth
  else
  begin
    I := LeftCol;
    if Col.Depth > ARow then
      J := ARow
    else
      J := Col.Depth;
  end;

  Result := CellRect(I, J);
  InBiDiMode := BidiMode = bdRightToLeft;

  for I := Col.Index to Columns.Count-1 do
  begin
    if ColumnAtDepth(Columns[I], ARow) <> MasterCol then Break;
    if not InBiDiMode then
    begin
      J := CellRect(DataToRawColumn(I), ARow).Right;
      if J = 0 then Break;
      Result.Right := Max(Result.Right, J);
    end
    else
    begin
      J := CellRect(DataToRawColumn(I), ARow).Left;
      if J >= ClientWidth then Break;
      Result.Left := J;
    end;
  end;
  J := Col.Depth;
  if (J <= ARow) and (J < FixedRows-1) then
  begin
    CalcFixedInfo(DrawInfo);
    Result.Bottom := DrawInfo.Vert.FixedBoundary - DrawInfo.Vert.EffectiveLineWidth;
  end;
end;

procedure TscCustomDBGrid.DrawCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect; AState: TscGridDrawState);
var
  FrameOffs: Byte;

  function RowIsMultiSelected: Boolean;
  var
    Index: Integer;
  begin
    Result := (scdgMultiSelect in Options) and Datalink.Active and
      FBookmarks.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  end;

  procedure DrawTitleCell(ACol, ARow: Integer; Column: TscColumn; var AState: TscGridDrawState);
  const
    ScrollArrows: array [Boolean, Boolean] of Integer =
      ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    MasterCol: TscColumn;
    TitleRect, TextRect, ButtonRect: TRect;
    I: Integer;
    LFrameOffs: Byte;
  begin
    TitleRect := CalcTitleRect(Column, ARow, MasterCol);

    if MasterCol = nil then
    begin
      ACanvas.FillRect(ARect);
      Exit;
    end;

    ACanvas.Font := MasterCol.Title.Font;
    ACanvas.Brush.Color := MasterCol.Title.Color;
    if [scdgRowLines, scdgColLines] * Options = [scdgRowLines, scdgColLines] then
      InflateRect(TitleRect, -1, -1);
    TextRect := TitleRect;
    I := GetSystemMetrics(SM_CXHSCROLL);
    if ((TextRect.Right - TextRect.Left) > I) and MasterCol.Expandable then
    begin
      Dec(TextRect.Right, I);
      ButtonRect := TitleRect;
      ButtonRect.Left := TextRect.Right;
      I := SaveDC(ACanvas.Handle);
      try
        ACanvas.FillRect(ButtonRect);
        InflateRect(ButtonRect, -1, -1);
        IntersectClipRect(ACanvas.Handle, ButtonRect.Left,
          ButtonRect.Top, ButtonRect.Right, ButtonRect.Bottom);
        InflateRect(ButtonRect, 1, 1);
        DrawFrameControl(ACanvas.Handle, ButtonRect, DFC_SCROLL,
          ScrollArrows[False, MasterCol.Expanded] or DFCS_FLAT);
      finally
        RestoreDC(ACanvas.Handle, I);
      end;
    end;

    DrawCellBackground(ACanvas, TitleRect, FixedColor, AState, ACol, ARow - FTitleOffset);

    LFrameOffs := FrameOffs;
    if (scgdPressed in AState) then
      Inc(LFrameOffs);
    with MasterCol.Title do
    begin
      WriteText(ACanvas, TextRect, 2, LFrameOffs, Caption, Alignment,
        BidiMode = bdRightToLeft);
    end;
    if ([scdgRowLines, scdgColLines] * Options = [scdgRowLines, scdgColLines]) and
       (FInternalDrawingStyle = scgdsClassic) and
       not (scgdPressed in AState) then
    begin
      InflateRect(TitleRect, 1, 1);
      if not TStyleManager.IsCustomStyleActive then
      begin
        DrawEdge(ACanvas.Handle, TitleRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
        DrawEdge(ACanvas.Handle, TitleRect, BDR_RAISEDINNER, BF_TOPLEFT);
      end;
    end;
    AState := AState - [scgdFixed];
  end;

var
  OldActive: Integer;
  Indicator: TThemedGrid;
  Value: string;
  DrawColumn: TscColumn;
  MultiSelected: Boolean;
  IsCustomStyle: Boolean;
  Style: TCustomStyleServices;
  CurCol, CurRow: Longint;
  Buffer: TBitmap;
  R1: TRect;
  IX, IY: Integer;
  FAlternateColor: TColor;
  Hold: Integer;
begin
  CurCol := ACol;
  CurRow := ARow;
  Style := StyleServices;
  IsCustomStyle := TStyleManager.IsCustomStyleActive;
  if csLoading in ComponentState then
  begin
    if IsCustomStyle and (seClient in StyleElements) then
      ACanvas.Brush.Color := Style.GetStyleColor(scGrid)
    else
      ACanvas.Brush.Color := Color;
    ACanvas.FillRect(ARect);
    Exit;
  end;

  Dec(ARow, FTitleOffset);
  Dec(ACol, FIndicatorOffset);

  if (scgdFixed in AState) and ([scdgRowLines, scdgColLines] * Options =
    [scdgRowLines, scdgColLines]) then
  begin
    InflateRect(ARect, -1, -1);
    FrameOffs := 1;
  end
  else
    FrameOffs := 2;

  if (BidiMode = bdRightToLeft) and not ((scgdFixed in AState) and (ACol < 0)) then
  begin
    ARect.Left := ClientWidth - ARect.Left;
    ARect.Right := ClientWidth - ARect.Right;
    Hold := ARect.Left;
    ARect.Left := ARect.Right;
    ARect.Right := Hold;
    ChangeGridOrientation(ACanvas, False);
  end;

  if (scgdFixed in AState) and (ACol < 0) then
  begin
    DrawCellBackground(ACanvas, ARect, FixedColor, AState, ACol, ARow);
    if Assigned(DataLink) and DataLink.Active  then
    begin
      MultiSelected := False;
      if ARow >= 0 then
      begin
        OldActive := FDataLink.ActiveRecord;
        try
          FDatalink.ActiveRecord := ARow;
          MultiSelected := RowIsMultiselected;
        finally
          FDatalink.ActiveRecord := OldActive;
        end;
      end;
      if (ARow = FDataLink.ActiveRecord) or MultiSelected then
      begin
        Indicator := tgIndicatorArrow;
        if FDataLink.DataSet <> nil then
          case FDataLink.DataSet.State of
            dsEdit: Indicator := tgIndicatorEdit;
            dsInsert: Indicator := tgIndicatorInsert;
            dsBrowse:
              if MultiSelected then
                if (ARow <> FDatalink.ActiveRecord) then
                  Indicator := tgIndicatorMultiDot
                else
                  Indicator := tgIndicatorMultiArrow;
          end;

        if IsCustomStyle then
        begin
          if (FScaleFactor < 1.5) or not SC_SCALESTYLES then
          begin
            Style.DrawElement(ACanvas.Handle,
              Style.GetElementDetails(Indicator), ARect)
          end
          else
          begin
            R1 := Rect(0, 0, 20, 20);
            IX := ARect.Left + ARect.Width div 2 - Round(R1.Width * FScaleFactor) div 2;
            IY := ARect.Top + ARect.Height div 2 - Round(R1.Height * FScaleFactor) div 2;
            Buffer := TBitmap.Create;
            Buffer.PixelFormat := pf32bit;
            Buffer.Width := R1.Width;
            Buffer.Height := R1.Height;
            Bitmap_ClearAlpha(Buffer, 0);
            try
              Buffer.AlphaFormat := afPremultiplied;
              StyleServices.DrawElement(Buffer.Canvas.Handle,
              Style.GetElementDetails(Indicator),
              Rect(2, 2, Buffer.Width - 2, Buffer.Height - 2));
              Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas,
                IX, IY, 255, FScaleFactor);
            finally
              Buffer.Free;
            end;
          end;
        end
        else
        if not FIndicatorsLoading then
          FIndicators.Draw(ACanvas, ARect,
            Integer(Indicator) - Integer(tgIndicatorArrow));

        if ARow = FDatalink.ActiveRecord then
          FSelRow := ARow + FTitleOffset;
      end;
    end;
  end
  else with ACanvas do
  begin
    DrawColumn := Columns[ACol];
    if not DrawColumn.Showing then Exit;
    if not (scgdFixed in AState) then
    begin
      Font := DrawColumn.Font;
      Brush.Color := DrawColumn.Color;
      if AlternateRow and Odd(ARow) then
      begin
        FAlternateColor := scdrawUtils.AlternateColor(DrawColumn.Color);
        ACanvas.Brush.Color := FAlternateColor;
        ACanvas.FillRect(ARect);
      end;
      if IsCustomStyle then
      begin
        if seFont in StyleElements then
        begin
          if  Enabled then
          case Self.BackgroundStyle of
            scgbColor:
              Font.Color := Style.GetStyleFontColor(sfGridItemNormal);
            scbgbFormBackground, scgbTransparent:
              Font.Color := scDrawUtils.GetCheckBoxTextColor(scsNormal);
          end
          else
          case Self.BackgroundStyle of
            scgbColor:
              Font.Color := scDrawUtils.GetEditTextColor(scsDisabled);
            scbgbFormBackground, scgbTransparent:
              Font.Color := scDrawUtils.GetCheckBoxTextColor(scsDisabled);
          end
        end
        else
        begin
          Font.Color := Self.Font.Color;
          if not Enabled then
             Font.Color := clGrayText;
        end;
        if seClient in StyleElements then
          Brush.Color := Style.GetStyleColor(scGrid)
        else
          Brush.Color := Color;
        FTextColor := Font.Color;
      end;
    end;
    if ARow < 0 then
      DrawTitleCell(ACol, ARow + FTitleOffset, DrawColumn, AState)
    else if (FDataLink = nil) or not FDataLink.Active then
    begin
      //FillRect(ARect)
    end
    else
    begin
      Value := '';
      OldActive := FDataLink.ActiveRecord;
      try
        FDataLink.ActiveRecord := ARow;
        if Assigned(DrawColumn.Field) then
          Value := DrawColumn.Field.DisplayText;
        if (FInplaceCol = CurCol) and (FInplaceRow = CurRow) then
        begin

        end
        else
        begin
        if HighlightCell(ACol, ARow, Value, AState) and DefaultDrawing then
          DrawCellHighlight(ACanvas, ARect, AState, ACol, ARow);
        if FDefaultDrawing then
        begin
          if Assigned(DrawColumn.FField) and (DrawColumn.FField.DataType = ftBoolean) then
          begin
            if DrawColumn.FField.IsNull then
              scDrawUtils.DrawCheckBoxInCenter(ACanvas, ARect, scsNormal, cbGrayed, FScaleFactor)
            else
            if DrawColumn.FField.AsBoolean then
              scDrawUtils.DrawCheckBoxInCenter(ACanvas, ARect, scsNormal, cbChecked, FScaleFactor)
            else
              scDrawUtils.DrawCheckBoxInCenter(ACanvas, ARect, scsNormal, cbUnChecked, FScaleFactor);
          end
          else
            WriteText(ACanvas, ARect, 3, 2, Value, DrawColumn.Alignment,
              UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment));
        end;
        end;
        if Columns.State = sccsDefault then
          DrawDataCell(ACanvas, ARect, DrawColumn.Field, AState);
        DrawColumnCell(ACanvas, ARect, ACol, DrawColumn, AState);
      finally
        FDataLink.ActiveRecord := OldActive;
      end;
      ACanvas.Brush.Style := bsSolid;
      if FDefaultDrawing and (scgdSelected in AState)
        and ((scdgAlwaysShowSelection in Options) or Focused)
        and not (csDesigning in ComponentState)
        and not (scdgRowSelect in Options)
        and (UpdateLock = 0)
        and (ValidParentForm(Self).ActiveControl = Self) then
      begin
        if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
        and (SelectionStyle = scstStyled)
        then
          InflateRect(ARect, -1, -1);
        if ShowFocusRect then
          scDrawFocusRect(ACanvas, ARect, FScaleFactor);
      end;
    end;
  end;
  if (scgdFixed in AState) and ([scdgRowLines, scdgColLines] * Options =
     [scdgRowLines, scdgColLines]) and (FInternalDrawingStyle = scgdsClassic) and
     not (scgdPressed in AState) then
  begin
    InflateRect(ARect, 1, 1);
    if not IsCustomStyle then
    begin
      DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
      DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
    end;
  end;

  if (BidiMode = bdRightToLeft) and not ((scgdFixed in AState) and (ACol < 0)) then
    ChangeGridOrientation(ACanvas, True);
end;

procedure TscCustomDBGrid.DrawDataCell(ACanvas: TCanvas; const Rect: TRect; Field: TField;
  State: TscGridDrawState);
begin
  if Assigned(FOnDrawDataCell) then FOnDrawDataCell(Self, ACanvas, Rect, Field, State);
end;

procedure TscCustomDBGrid.DrawCellBackground(ACanvas: TCanvas; const ARect: TRect; AColor: TColor;
  AState: TscGridDrawState; ACol, ARow: Integer);
var
  LRect: TRect;
begin
  LRect := ARect;
  if not (csDesigning in ComponentState) then
    InflateRect(LRect, 1, 1);
  inherited DrawCellBackground(ACanvas, LRect, AColor, AState, ACol + FIndicatorOffset,
    ARow + FTitleOffset);
end;

procedure TscCustomDBGrid.DrawCellHighlight(ACanvas: TCanvas; const ARect: TRect;
  AState: TscGridDrawState; ACol, ARow: Integer);
begin
  if (scdgMultiSelect in Options) and Datalink.Active and (FInternalDrawingStyle <> scgdsClassic) then
    Include(AState, scgdRowSelected);
  if (Datalink <> nil) and (DataLink.Active) then
    inherited DrawCellHighlight(ACanvas, ARect, AState, ACol + FIndicatorOffset, ARow + FTitleOffset);
end;

procedure TscCustomDBGrid.DrawColumnCell(ACanvas: TCanvas; const Rect: TRect; DataCol: Integer;
  Column: TscColumn; State: TscGridDrawState);
begin
  if Assigned(OnDrawColumnCell) then
    OnDrawColumnCell(Self, ACanvas, Rect, DataCol, Column, State);
end;

procedure TscCustomDBGrid.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Self)
  else
    ShowPopupEditor(Columns[SelectedIndex]);
end;

procedure TscCustomDBGrid.EditingChanged;
begin
  if scdgIndicator in Options then InvalidateCell(0, FSelRow);
end;

procedure TscCustomDBGrid.EndLayout;
begin
  if FLayoutLock > 0 then
  begin
    try
      try
        if FLayoutLock = 1 then
          InternalLayout;
      finally
        if FLayoutLock = 1 then
          FColumns.EndUpdate;
      end;
    finally
      Dec(FLayoutLock);
      EndUpdate;
    end;
  end;
end;

procedure TscCustomDBGrid.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
end;

function TscCustomDBGrid.GetColField(DataCol: Integer): TField;
begin
  Result := nil;
  if (DataCol >= 0) and FDatalink.Active and (DataCol < Columns.Count) then
    Result := Columns[DataCol].Field;
end;

function TscCustomDBGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TscCustomDBGrid.GetEditLimit: Integer;
begin
  Result := 0;
  if Assigned(SelectedField) and (SelectedField.DataType in [ftString, ftWideString]) then
    Result := SelectedField.Size;
end;

function TscCustomDBGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
  if FDatalink.Active then
  with Columns[RawToDataColumn(ACol)] do
    if Assigned(Field) then
      Result := Field.EditMask;
end;

function TscCustomDBGrid.GetEditStyle(ACol, ARow: Integer): TscGridEditStyle;
var
  Column: TscColumn;
  MasterField: TField;
begin
  TscDBGridInplaceEdit(InplaceEditor).FUseDataList := False;
  Column := Columns[SelectedIndex];
  Result := esSimple;
  case Column.ButtonStyle of
   sccbsEllipsis:
     Result := esEllipsis;
   sccbsAuto:
     if Assigned(Column.Field) then
     with Column.Field do
     begin
       if FieldKind = fkLookup then
       begin
         MasterField := Dataset.FieldByName(KeyFields);
         if Assigned(MasterField) and MasterField.CanModify and
           not ((sccvReadOnly in Column.AssignedValues) and Column.ReadOnly) then
           if not ReadOnly and DataLink.Active and not Datalink.ReadOnly then
           begin
             Result := esPickList;
             TscDBGridInplaceEdit(InplaceEditor).FUseDataList := True;
           end;
       end
       else
       if Assigned(Column.Picklist) and (Column.PickList.Count > 0) and
         not Column.Readonly then
         Result := esPickList
       else if DataType in [ftDataset, ftReference] then
         Result := esEllipsis;
     end;
  end;
end;

function TscCustomDBGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
  if FDatalink.Active then
    with Columns[RawToDataColumn(ACol)] do
      if Assigned(Field) then
        Result := Field.Text;
  FEditText := Result;
end;

function TscCustomDBGrid.GetFieldCount: Integer;
begin
  Result := FDatalink.FieldCount;
end;

function TscCustomDBGrid.GetFields(FieldIndex: Integer): TField;
begin
  Result := FDatalink.Fields[FieldIndex];
end;

function TscCustomDBGrid.GetFieldValue(ACol: Integer): string;
var
  Field: TField;
begin
  Result := '';
  Field := GetColField(ACol);
  if Field <> nil then Result := Field.DisplayText;
end;

function TscCustomDBGrid.GetSelectedField: TField;
var
  Index: Integer;
begin
  Index := SelectedIndex;
  if Index <> -1 then
    Result := Columns[Index].Field
  else
    Result := nil;
end;

function TscCustomDBGrid.GetSelectedIndex: Integer;
begin
  Result := RawToDataColumn(Col);
end;

function TscCustomDBGrid.HighlightCell(DataCol, DataRow: Integer;
  const Value: string; AState: TscGridDrawState): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (scdgMultiSelect in Options) and Datalink.Active then
    Result := FBookmarks.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  if not Result then
    Result := (scgdSelected in AState)
      and ((scdgAlwaysShowSelection in Options) or Focused)
      and ((UpdateLock = 0) or (scdgRowSelect in Options));
end;

procedure TscCustomDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

  procedure ClearSelection;
  begin
    if (scdgMultiSelect in Options) then
    begin
      FBookmarks.Clear;
      FSelecting := False;
    end;
  end;

  procedure DoSelection(Select: Boolean; Direction: Integer);
  var
    AddAfter: Boolean;
  begin
    AddAfter := False;
    BeginUpdate;
    try
      if (scdgMultiSelect in Options) and FDatalink.Active then
        if Select and (ssShift in Shift) then
        begin
          if not FSelecting then
          begin
            FSelectionAnchor := FBookmarks.CurrentRow;
            FBookmarks.CurrentRowSelected := True;
            FSelecting := True;
            AddAfter := True;
          end
          else
          with FBookmarks do
          begin
            AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
            if not AddAfter then
              CurrentRowSelected := False;
          end
        end
        else
          ClearSelection;
      FDatalink.MoveBy(Direction);
      if AddAfter then FBookmarks.CurrentRowSelected := True;
    finally
      EndUpdate;
    end;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with FDatalink.Dataset do
    begin
      if (State = dsInsert) and not Modified and not FDatalink.FModified then
        if FDataLink.EOF then Exit else Cancel
      else
        DoSelection(Select, 1);
      if FDataLink.EOF and CanModify and (not ReadOnly) and (scdgEditing in Options) then
        Append;
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    with FDatalink.Dataset do
      if (State = dsInsert) and not Modified and FDataLink.EOF and
        not FDatalink.FModified then
        Cancel
      else
        DoSelection(Select, -1);
  end;

  procedure Tab(GoForward: Boolean);
  var
    ACol, Original: Integer;
  begin
    ACol := Col;
    Original := ACol;
    BeginUpdate;
    try
      while True do
      begin
        if GoForward then
          Inc(ACol) else
          Dec(ACol);
        if ACol >= ColCount then
        begin
          NextRow(False);
          ACol := FIndicatorOffset;
        end
        else if ACol < FIndicatorOffset then
        begin
          PriorRow(False);
          ACol := ColCount - FIndicatorOffset;
        end;
        if ACol = Original then Exit;
        if TabStops[ACol] then
        begin
          MoveCol(ACol, 0);
          Exit;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;

  function DeletePrompt: Boolean;
  var
    Msg: string;
  begin
    if (FBookmarks.Count > 1) then
      Msg := SDeleteMultipleRecordsQuestion
    else
      Msg := SDeleteRecordQuestion;
    Result := not (scdgConfirmDelete in Options) or
      (scMessageDlg(Msg, mtConfirmation, mbOKCancel, 0) <> idCancel);
  end;

const
  RowMovementKeys = [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END];

begin
  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then KeyDownEvent(Self, Key, Shift);
  if not FDatalink.Active or not CanGridAcceptKey(Key, Shift) then Exit;
  if BidiMode = bdRightToLeft then
    if Key = VK_LEFT then
      Key := VK_RIGHT
    else if Key = VK_RIGHT then
      Key := VK_LEFT;

  with FDatalink.DataSet do
    if ssCtrl in Shift then
    begin
      if (Key in RowMovementKeys) then ClearSelection;
      case Key of
        VK_UP, VK_PRIOR: FDataLink.MoveBy(-FDatalink.ActiveRecord);
        VK_DOWN, VK_NEXT: FDataLink.MoveBy(FDatalink.BufferCount - FDatalink.ActiveRecord - 1);
        VK_LEFT: MoveCol(FIndicatorOffset, 1);
        VK_RIGHT: MoveCol(ColCount - 1, -1);
        VK_HOME: First;
        VK_END: Last;
        VK_DELETE:
          if (not ReadOnly) and not IsEmpty
            and CanModify and DeletePrompt then
          if FBookmarks.Count > 0 then
            FBookmarks.Delete
          else
            Delete;
      end
    end
    else
      case Key of
        VK_UP: PriorRow(True);
        VK_DOWN: NextRow(True);
        VK_LEFT:
          if scdgRowSelect in Options then
            PriorRow(False) else
            MoveCol(Col - 1, -1);
        VK_RIGHT:
          if scdgRowSelect in Options then
            NextRow(False) else
            MoveCol(Col + 1, 1);
        VK_HOME:
          if (ColCount = FIndicatorOffset+1)
            or (scdgRowSelect in Options) then
          begin
            ClearSelection;
            First;
          end
          else
            MoveCol(FIndicatorOffset, 1);
        VK_END:
          if (ColCount = FIndicatorOffset+1)
            or (scdgRowSelect in Options) then
          begin
            ClearSelection;
            Last;
          end
          else
            MoveCol(ColCount - 1, -1);
        VK_NEXT:
          begin
            ClearSelection;
            FDataLink.MoveBy(VisibleRowCount);
          end;
        VK_PRIOR:
          begin
            ClearSelection;
            FDataLink.MoveBy(-VisibleRowCount);
          end;
        VK_INSERT:
          if CanModify and (not ReadOnly) and (scdgEditing in Options) then
          begin
            ClearSelection;
            Insert;
          end;
        VK_TAB: if not (ssAlt in Shift) then Tab(not (ssShift in Shift));
        VK_ESCAPE:
          begin
            if SysLocale.PriLangID = LANG_KOREAN then
              FIsESCKey := True;
            FDatalink.Reset;
            ClearSelection;
            if not (scdgAlwaysShowEditor in Options) then HideEditor;
          end;
        VK_F2: EditorMode := True;
      end;
end;

procedure TscCustomDBGrid.WMChar;
var
  CellIndex: Integer;
begin
  if scdgIndicator in Options
  then
    CellIndex := Col - 1
  else
    CellIndex := Col;
  if (CellIndex >= 0) and (Columns.Count > 0) and (Columns[CellIndex].Field <> nil)
     and (Columns[CellIndex].Field.DataType = ftBoolean)
  then
    begin
      if (Msg.CharCode = VK_RETURN) or (Msg.CharCode = VK_SPACE)
      then
        if not ReadOnly
        then
          begin
            FDatalink.Edit;
            FDatalink.Modified;
            if Columns[CellIndex ].Field.AsBoolean = True
            then
              Columns[CellIndex ].Field.AsBoolean := False
            else
             Columns[CellIndex ].Field.AsBoolean := True;
          end;
    end
  else
    inherited;
end;

procedure TscCustomDBGrid.KeyPress(var Key: Char);
begin
  FIsESCKey := False;
  if not (scdgAlwaysShowEditor in Options) and (Key = #13) then
    FDatalink.UpdateData;
  inherited KeyPress(Key);
end;

procedure TscCustomDBGrid.InternalLayout;

  function FieldIsMapped(F: TField): Boolean;
  var
    X: Integer;
  begin
    Result := False;
    if F = nil then Exit;
    for X := 0 to FDatalink.FieldCount-1 do
      if FDatalink.Fields[X] = F then
      begin
        Result := True;
        Exit;
      end;
  end;

  procedure CheckForPassthroughs;
  var
    SeenPassthrough: Boolean;
    I, J: Integer;
    Column: TscColumn;
  begin
    SeenPassthrough := False;
    for I := 0 to FColumns.Count-1 do
      if not FColumns[I].IsStored then
        SeenPassthrough := True
      else if SeenPassthrough then
      begin
        for J := FColumns.Count-1 downto 0 do
        begin
          Column := FColumns[J];
          if not Column.IsStored then
            Column.Free;
        end;
        Exit;
      end;
  end;

  procedure ResetColumnFieldBindings;
  var
    I, J, K: Integer;
    Fld: TField;
    Column: TscColumn;
  begin
    if FColumns.State = sccsDefault then
    begin
      if (not FDataLink.Active) and (FDatalink.DefaultFields) then
        FColumns.Clear
      else
        for J := FColumns.Count-1 downto 0 do
          with FColumns[J] do
          if not Assigned(Field)
            or not FieldIsMapped(Field) then Free;
      I := FDataLink.FieldCount;
      if (I = 0) and (FColumns.Count = 0) then Inc(I);
      for J := 0 to I-1 do
      begin
        Fld := FDatalink.Fields[J];
        if Assigned(Fld) then
        begin
          K := J;
          while (K < FColumns.Count) and (FColumns[K].Field <> Fld) do
            Inc(K);
          if K < FColumns.Count then
            Column := FColumns[K]
          else
          begin
            Column := FColumns.InternalAdd;
            Column.Field := Fld;
          end;
        end
        else
          Column := FColumns.InternalAdd;
        Column.Index := J;
      end;
    end
    else
    begin
      for I := 0 to FColumns.Count-1 do
        FColumns[I].Field := nil;
    end;
  end;

  procedure MeasureTitleHeights;
  var
    I, J, K, D, B: Integer;
    RestoreCanvas: Boolean;
    Heights: array of Integer;
    TempDc: HDC;
  begin
    RestoreCanvas := not HandleAllocated;
    if RestoreCanvas then
      Canvas.Handle := GetDC(0);
    try
      Canvas.Font := Font;
      K := Canvas.TextHeight('Wg') + 3;
      if scdgRowLines in Options then
        Inc(K, GridLineWidth);
      DefaultRowHeight := K;
      B := GetSystemMetrics(SM_CYHSCROLL);
      if scdgTitles in Options then
      begin
        SetLength(Heights, FTitleOffset+1);
        for I := 0 to FColumns.Count-1 do
        begin
          Canvas.Font := FColumns[I].Title.Font;
          D := FColumns[I].Depth;
          if D <= High(Heights) then
          begin
            J := Canvas.TextHeight('Wg') + 4;
            if FColumns[I].Expandable and (B > J) then
              J := B;
            Heights[D] := Max(J, Heights[D]);
          end;
        end;
        if Heights[0] = 0 then
        begin
          Canvas.Font := FTitleFont;
          Heights[0] := Canvas.TextHeight('Wg') + 4;
        end;
        for I := 0 to High(Heights)-1 do
        begin
          RowHeights[I] := Heights[I];
        end;
      end;
    finally
      if RestoreCanvas then
      begin
        TempDc := Canvas.Handle;
        Canvas.Handle := 0;
        ReleaseDC(0,TempDc);
      end;
    end;
  end;

var
  I, J: Integer;
begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then Exit;

  if HandleAllocated then KillMessage(Handle, CM_DEFERLAYOUT);

  CheckForPassthroughs;
  FIndicatorOffset := 0;
  if scdgIndicator in Options then
    Inc(FIndicatorOffset);
  FDatalink.ClearMapping;
  if FDatalink.Active then DefineFieldMap;
  ResetColumnFieldBindings;
  FVisibleColumns.Clear;
  for I := 0 to FColumns.Count-1 do
    if FColumns[I].Showing then FVisibleColumns.Add(FColumns[I]);
  ColCount := FColumns.Count + FIndicatorOffset;
  inherited FixedCols := FIndicatorOffset;
  FTitleOffset := 0;
  if scdgTitles in Options then
  begin
    FTitleOffset := 1;
    if (FDatalink <> nil) and (FDatalink.Dataset <> nil)
      and FDatalink.Dataset.ObjectView then
    begin
      for I := 0 to FColumns.Count-1 do
      begin
        if FColumns[I].Showing then
        begin
          J := FColumns[I].Depth;
          if J >= FTitleOffset then FTitleOffset := J+1;
        end;
      end;
    end;
  end;
  UpdateRowCount;
  MeasureTitleHeights;
  SetColumnAttributes;
  UpdateActive;
  Invalidate;
end;

procedure TscCustomDBGrid.LayoutChanged;
begin
  if AcquireLayoutLock then
    EndLayout;
end;

procedure TscCustomDBGrid.LinkActive(Value: Boolean);
var
  Comp: TComponent;
  I: Integer;
begin
  if not Value then HideEditor;
  FBookmarks.LinkActive(Value);
  try
    LayoutChanged;
  finally
    for I := ComponentCount-1 downto 0 do
    begin
      Comp := Components[I];
      if (Comp is TscCustomDBGrid)
        and (TscCustomDBGrid(Comp).DragKind = dkDock) then
        Comp.Free;
    end;
    UpdateScrollBar;
    if Value and (scdgAlwaysShowEditor in Options) then ShowEditor;
  end;
end;

procedure TscCustomDBGrid.Loaded;
begin
  inherited Loaded;
  if FColumns.Count > 0 then
    ColCount := FColumns.Count;
  LayoutChanged;
end;

function TscCustomDBGrid.PtInExpandButton(X,Y: Integer; var MasterCol: TscColumn): Boolean;
var
  Cell: TscGridCoord;
  R: TRect;
begin
  MasterCol := nil;
  Result := False;
  Cell := MouseCoord(X,Y);
  if (Cell.Y < FTitleOffset) and FDatalink.Active
    and (Cell.X >= FIndicatorOffset)
    and (RawToDataColumn(Cell.X) < Columns.Count) then
  begin
    R := CalcTitleRect(Columns[RawToDataColumn(Cell.X)], Cell.Y, MasterCol);
    if BidiMode <> bdRightToLeft then
      R.Left := R.Right - GetSystemMetrics(SM_CXHSCROLL)
    else
      R.Right := R.Left + GetSystemMetrics(SM_CXHSCROLL);
    Result := MasterCol.Expandable and R.Contains(Point(X,Y));
  end;
end;

procedure TscCustomDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TscGridCoord;
  OldCol,OldRow: Integer;
  MasterCol: TscColumn;
  CellIndex: Integer;
begin
  if not AcquireFocus then Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;

  if Sizing(X, Y) then
  begin
    FDatalink.UpdateData;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  Cell := MouseCoord(X, Y);
  if (Cell.X < 0) and (Cell.Y < 0) then
  begin
    if (FDataLink <> nil) and (FDataLink.Editing) then
      FDataLink.UpdateData;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  if (DragKind = dkDock) and (Cell.X < FIndicatorOffset) and
    (Cell.Y < FTitleOffset) and (not (csDesigning in ComponentState)) then
  begin
    BeginDrag(false);
    Exit;
  end;

  if PtInExpandButton(X,Y, MasterCol) then
  begin
    MasterCol.Expanded := not MasterCol.Expanded;
    ReleaseCapture;
    UpdateDesigner;
    Exit;
  end;

  if ((csDesigning in ComponentState) or (scdgColumnResize in Options)) and
    (Cell.Y < FTitleOffset) then
  begin
    FDataLink.UpdateData;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  if FDatalink.Active then
    with Cell do
    begin
      BeginUpdate;
      try
        FDatalink.UpdateData;
        HideEditor;
        OldCol := Col;
        OldRow := Row;
        if (Y >= FTitleOffset) and (Y - Row <> 0) then
          FDatalink.MoveBy(Y - Row);
        if X >= FIndicatorOffset then
          MoveCol(X, 0);
        if (Button = mbLeft) and (scdgMultiSelect in Options) and FDatalink.Active then
          with FBookmarks do
          begin
            FSelecting := False;
            if ssCtrl in Shift then
              CurrentRowSelected := not CurrentRowSelected
            else
            begin
              Clear;
              CurrentRowSelected := True;
            end;
          end;

        if scdgIndicator in Options
        then
          CellIndex := Cell.X - 1
        else
          CellIndex := Cell.X;

        if CellIndex >= 0 then

        if (Columns.Count > 0) and (Columns[CellIndex].Field <> nil) and
            (Columns[CellIndex].Field.DataType = ftBoolean)
        then
          begin
            if not ReadOnly and ((X = OldCol) and (Y = OldRow))
            then
              begin
                FDatalink.Edit;
                FDatalink.Modified;
                if Columns[CellIndex].Field.AsBoolean = True
                then
                  Columns[CellIndex].Field.AsBoolean := False
                else
                  Columns[CellIndex].Field.AsBoolean := True;
              end;
          end
        else
        if (Button = mbLeft) and
          (((X = OldCol) and (Y = OldRow)) or (scdgAlwaysShowEditor in Options)) then
          ShowEditor
        else
          InvalidateEditor;
      finally
        EndUpdate;
      end;
    end;
end;

procedure TscCustomDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TscGridCoord;
  SaveState: TscGridState;
begin
  SaveState := FGridState;
  inherited MouseUp(Button, Shift, X, Y);
  if (SaveState = scgsRowSizing) or (SaveState = scgsColSizing) or
    ((InplaceEditor <> nil) and (InplaceEditor.Visible) and
     InplaceEditor.BoundsRect.Contains(Point(X,Y))) then Exit;
  Cell := MouseCoord(X,Y);
  if (Button = mbLeft) and (Cell.X >= FIndicatorOffset) and (Cell.Y >= 0) then
    if Cell.Y < FTitleOffset then
      TitleClick(Columns[RawToDataColumn(Cell.X)])
    else
      CellClick(Columns[SelectedIndex]);
end;

function TscCustomDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;

  if (scdgMultiSelect in Options) and (FBookmarks.Count > 0) then
    begin
      FBookmarks.Clear;
      FSelecting := False;
    end;

  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if (not Result) and (FDataLink.Active) then
  begin
    FDataLink.MoveBy(1);
    Result := True;
  end;
end;

function TscCustomDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;

   if (scdgMultiSelect in Options) and (FBookmarks.Count > 0) then
    begin
      FBookmarks.Clear;
      FSelecting := False;
    end;

  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if (not Result) and (FDataLink.Active) then
  begin
    FDataLink.MoveBy(-1);
    Result := True;
  end;
end;

procedure TscCustomDBGrid.MoveCol(RawCol, Direction: Integer);
var
  OldCol: Integer;
begin
  FDatalink.UpdateData;
  if RawCol >= ColCount then
    RawCol := ColCount - 1;
  if RawCol < FIndicatorOffset then RawCol := FIndicatorOffset;
  if Direction <> 0 then
  begin
    while (RawCol < ColCount) and (RawCol >= FIndicatorOffset) and
      (ColWidths[RawCol] <= 0) do
      Inc(RawCol, Direction);
    if (RawCol >= ColCount) or (RawCol < FIndicatorOffset) then Exit;
  end;
  OldCol := Col;
  if RawCol <> OldCol then
  begin
    if not FInColExit then
    begin
      FInColExit := True;
      try
        ColExit;
      finally
        FInColExit := False;
      end;
      if Col <> OldCol then Exit;
    end;
    if not (scdgAlwaysShowEditor in Options) then HideEditor;
    Col := RawCol;
    ColEnter;
  end;
end;

procedure TscCustomDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
  NeedLayout: Boolean;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent is TPopupMenu) then
    begin
      for I := 0 to Columns.Count-1 do
        if Columns[I].PopupMenu = AComponent then
          Columns[I].PopupMenu := nil;
    end
    else if (FDataLink <> nil) then
      if (AComponent = DataSource)  then
        DataSource := nil
      else if (AComponent is TField) then
      begin
        NeedLayout := False;
        BeginLayout;
        try
          for I := 0 to Columns.Count-1 do
            with Columns[I] do
              if Field = AComponent then
              begin
                Field := nil;
                NeedLayout := True;
              end;
        finally
          if NeedLayout and Assigned(FDatalink.Dataset)
            and not (csDestroying in FDatalink.DataSet.ComponentState) 
            and not FDatalink.Dataset.ControlsDisabled then
            EndLayout
          else
            DeferLayout;
        end;
      end;
  end;
end;

procedure TscCustomDBGrid.RecordChanged(Field: TField);
var
  I: Integer;
  CField: TField;
begin
  if not HandleAllocated then Exit;
  if Field = nil then
    Invalidate
  else
  begin
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Field = Field then
        InvalidateCol(DataToRawColumn(I));
  end;
  CField := SelectedField;
  if ((Field = nil) or (CField = Field)) and
    (Assigned(CField) and (CField.Text <> FEditText) and
    ((SysLocale.PriLangID <> LANG_KOREAN) or FIsESCKey)) then
  begin
    InvalidateEditor;
    if InplaceEditor <> nil then InplaceEditor.Deselect;
  end;
end;

procedure TscCustomDBGrid.Scroll(Distance: Integer);
var
  OldRect, NewRect: TRect;
  DrawInfo: TscGridDrawInfo;
  R: TRect;
begin
  if not HandleAllocated then Exit;
  OldRect := BoxRect(0, Row, ColCount - 1, Row);
  if (FDataLink.ActiveRecord >= RowCount - FTitleOffset) then UpdateRowCount;
  UpdateScrollBar;
  UpdateActive;
  NewRect := BoxRect(0, Row, ColCount - 1, Row);
  ValidateRect(Handle, @OldRect);
  InvalidateRect(Handle, @OldRect, False);
  InvalidateRect(Handle, @NewRect, False);
  if Distance <> 0 then
  begin
    HideEditor;
    try
      if Abs(Distance) > VisibleRowCount then
      begin
        Invalidate;
        Exit;
      end
      else
      begin
        if scdgIndicator in Options then
        begin
          OldRect := BoxRect(0, FSelRow, ColCount - 1, FSelRow);
          InvalidateRect(Handle, @OldRect, False);
        end;
        if scdgIndicator in Options then
        begin
          NewRect := BoxRect(0, Row, ColCount - 1, Row);
          InvalidateRect(Handle, @NewRect, False);
        end;
      end;
    finally
      if scdgAlwaysShowEditor in Options then ShowEditor;
    end;
  end;
  if UpdateLock = 0 then Update;

  if (Distance <> 0)
  then
    begin
      CalcDrawInfo(DrawInfo);
      with DrawInfo do
      begin
        R.Left := Horz.FixedBoundary;
        R.Top := Vert.FixedBoundary;
        R.Right := ClientRect.Right;
        R.Bottom := ClientRect.Bottom;
        WinApi.Windows.InvalidateRect(Handle, @R, True);
        R.Left := GridWidth;
        if R.Left >= Width then R.Left := Width - 1;
        R.Top := 0;
        R.Right := ClientWidth;
        R.Bottom := Vert.FixedBoundary;
        WinApi.Windows.InvalidateRect(Handle, @R, True);
      end;
    end;

end;

procedure TscCustomDBGrid.SetColumns(Value: TscDBGridColumns);
begin
  Columns.Assign(Value);
end;

function ReadOnlyField(Field: TField): Boolean;
var
  MasterField: TField;
begin
  Result := Field.ReadOnly;
  if not Result and (Field.FieldKind = fkLookup) then
  begin
    Result := True;
    if Field.DataSet = nil then Exit;
    MasterField := Field.Dataset.FindField(Field.KeyFields);
    if MasterField = nil then Exit;
    Result := MasterField.ReadOnly;
  end;
end;

procedure TscCustomDBGrid.SetColumnAttributes;
var
  I: Integer;
begin
  for I := 0 to FColumns.Count-1 do
  with FColumns[I] do
  begin
    TabStops[I + FIndicatorOffset] := Showing and not ReadOnly and DataLink.Active and
      Assigned(Field) and not (Field.FieldKind = fkCalculated) and not ReadOnlyField(Field);
    ColWidths[I + FIndicatorOffset] := Width;
  end;
  if (scdgIndicator in Options) then
    ColWidths[0] := Round(IndicatorWidth * FScaleFactor);
end;

procedure TscCustomDBGrid.SetDataSource(Value: TDataSource);
begin
  if Value = FDatalink.Datasource then Exit;
  if Assigned(Value) then
    if Assigned(Value.DataSet) then
      if Value.DataSet.IsUnidirectional then
        DatabaseError(SDataSetUnidirectional);
  FBookmarks.Clear;
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TscCustomDBGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  FEditText := Value;
end;

procedure TscCustomDBGrid.SetOptions(Value: TscDBGridOptions);
const
  LayoutOptions = [scdgEditing, scdgAlwaysShowEditor, scdgTitles, scdgIndicator,
    scdgColLines, scdgRowLines, scdgRowSelect, scdgAlwaysShowSelection];
var
  NewGridOptions: TscGridOptions;
  ChangedOptions: TscDBGridOptions;
begin
  if FOptions <> Value then
  begin
    NewGridOptions := [];
    if scdgColLines in Value then
      NewGridOptions := NewGridOptions + [scgoFixedVertLine, scgoVertLine];
    if scdgRowLines in Value then
      NewGridOptions := NewGridOptions + [scgoFixedHorzLine, scgoHorzLine];
    if scdgColumnResize in Value then
      NewGridOptions := NewGridOptions + [scgoColSizing, scgoColMoving];
    if scdgTabs in Value then Include(NewGridOptions, scgoTabs);
    if scdgRowSelect in Value then
    begin
      Include(NewGridOptions, scgoRowSelect);
      Exclude(Value, scdgAlwaysShowEditor);
      Exclude(Value, scdgEditing);
    end;
    if scdgEditing in Value then Include(NewGridOptions, scgoEditing);
    if scdgAlwaysShowEditor in Value then Include(NewGridOptions, scgoAlwaysShowEditor);
    if scdgTitleClick in Value then Include(NewGridOptions, scgoFixedRowClick);
    if scdgTitleHotTrack in Value then Include(NewGridOptions, scgoFixedHotTrack);
    inherited Options := NewGridOptions;
    if scdgMultiSelect in (FOptions - Value) then FBookmarks.Clear;
    ChangedOptions := (FOptions + Value) - (FOptions * Value);
    FOptions := Value;
    if ChangedOptions * LayoutOptions <> [] then LayoutChanged;
  end;
end;

procedure TscCustomDBGrid.SetSelectedField(Value: TField);
var
  I: Integer;
begin
  if Value = nil then Exit;
  for I := 0 to Columns.Count - 1 do
    if Columns[I].Field = Value then
      MoveCol(DataToRawColumn(I), 0);
end;

procedure TscCustomDBGrid.SetSelectedIndex(Value: Integer);
begin
  MoveCol(DataToRawColumn(Value), 0);
end;

procedure TscCustomDBGrid.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
  if scdgTitles in Options then LayoutChanged;
end;

function TscCustomDBGrid.StoreColumns: Boolean;
begin
  Result := Columns.State = sccsCustomized;
end;

procedure TscCustomDBGrid.TimedScroll(Direction: TscGridScrollDirection);
begin
  if FDatalink.Active then
  begin
    with FDatalink do
    begin
      if scsdUp in Direction then
      begin
        FDataLink.MoveBy(-ActiveRecord - 1);
        Exclude(Direction, scsdUp);
      end;
      if scsdDown in Direction then
      begin
        FDataLink.MoveBy(RecordCount - ActiveRecord);
        Exclude(Direction, scsdDown);
      end;
    end;
    if Direction <> [] then inherited TimedScroll(Direction);
  end;
end;

procedure TscCustomDBGrid.TitleClick(Column: TscColumn);
begin
  if (scdgTitleClick in FOptions) and Assigned(FOnTitleClick) then
    FOnTitleClick(Column);
end;

procedure TscCustomDBGrid.TitleFontChanged(Sender: TObject);
begin
  if (not FSelfChangingTitleFont) and not (csLoading in ComponentState) then
    ParentFont := False;
  if scdgTitles in Options then LayoutChanged;
end;

procedure TscCustomDBGrid.UpdateActive;
var
  NewRow: Integer;
  Field: TField;
begin
  if FDatalink.Active and HandleAllocated and not (csLoading in ComponentState) then
  begin
    NewRow := FDatalink.ActiveRecord + FTitleOffset;
    if Row <> NewRow then
    begin
      if not (scdgAlwaysShowEditor in Options) then HideEditor;
      MoveColRow(Col, NewRow, False, False);
      InvalidateEditor;
    end;
    Field := SelectedField;
    if Assigned(Field) and (Field.Text <> FEditText) then
      InvalidateEditor;
  end;
end;

procedure TscCustomDBGrid.UpdateData;
var
  Field: TField;
begin
  Field := SelectedField;
  if Assigned(Field) and (Field.Text <> FEditText) then
    Field.Text := FEditText;
end;

procedure TscCustomDBGrid.UpdateRowCount;
var
  OldRowCount: Integer;
begin
  OldRowCount := RowCount;
  if RowCount <= FTitleOffset then RowCount := FTitleOffset + 1;
  FixedRows := FTitleOffset;
  with FDataLink do
    if not Active or (RecordCount = 0) or not HandleAllocated then
      RowCount := 1 + FTitleOffset
    else
    begin
      RowCount := 1000;
      FDataLink.BufferCount := VisibleRowCount;
      RowCount := RecordCount + FTitleOffset;
      if scdgRowSelect in Options then TopRow := FixedRows;
      UpdateActive;
    end;
  if OldRowCount <> RowCount then Invalidate;
end;

procedure TscCustomDBGrid.UpdateScrollBar;
var
  SIOld, SINew: TScrollInfo;
  ScrollBarVisible: Boolean;
begin
  if FDatalink.Active and HandleAllocated then
    with FDatalink.DataSet do
    begin
      SIOld.cbSize := sizeof(SIOld);
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SIOld);
      SINew := SIOld;
      if IsSequenced then
      begin
        ScrollBarVisible := RecordCount > 1;
        if ScrollBarVisible then
        begin
          SINew.nMin := 1;
          SINew.nPage := Self.VisibleRowCount;
          SINew.nMax := Integer(DWORD(RecordCount) + SINew.nPage - 1);
          if State in [dsInactive, dsBrowse, dsEdit] then
            SINew.nPos := RecNo;
        end;
      end
      else
      begin
        ScrollBarVisible := True;
        SINew.nMin := 0;
        SINew.nPage := 0;
        SINew.nMax := 4;
        if FDataLink.BOF then SINew.nPos := 0
        else if FDataLink.EOF then SINew.nPos := 4
        else SINew.nPos := 2;
      end;
      ShowScrollBar(Self.Handle, SB_VERT, ScrollBarVisible);
      if ScrollBarVisible then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);

      if (Win32MajorVersion >= 6) and (ScrollBarVisible <> FOldScrollBarVisible) then
         SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
           SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING or SWP_FRAMECHANGED);

      FOldScrollBarVisible := ScrollBarVisible;

      if TStyleManager.IsCustomStyleActive then
          SendMessage(Handle, WM_NCPAINT, 0, 0);
    end;

  if not FDatalink.Active and HandleAllocated and FOldScrollBarVisible then
  begin
    FOldScrollBarVisible := False;
    ShowScrollBar(Self.Handle, SB_VERT, False);
    if (Win32MajorVersion >= 6) then
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
        SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING or SWP_FRAMECHANGED);
  end;
end;

function TscCustomDBGrid.ValidFieldIndex(FieldIndex: Integer): Boolean;
begin
  Result := DataLink.GetMappedIndex(FieldIndex) >= 0;
end;

procedure  TscCustomDBGrid.CMGesture(var Message: TCMGesture);
var
  Offset: Integer;
  rh: Integer;
begin
  inherited;
  if gfBegin in Message.Info^.Flags
  then
    FTouchBegin := Message.Info^.Location.Y
  else
  begin
    FTouchEnd := Message.Info^.Location.Y;
    rh := ClientHeight div VisibleRowCount;
    if rh = 0 then rh := 1;
    Offset := (FTouchEnd - FTouchBegin) div rh;
    if Abs(Offset) > 0 then
    begin
      FDataLink.MoveBy(-Offset);
      FTouchBegin := FTouchEnd;
    end;
  end;
end;

procedure TscCustomDBGrid.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if ParentFont then
  begin
    FSelfChangingTitleFont := True;
    try
      TitleFont := Font;
    finally
      FSelfChangingTitleFont := False;
    end;
    LayoutChanged;
  end;
end;

procedure TscCustomDBGrid.CMBiDiModeChanged(var Message: TMessage);
var
  Loop: Integer;
begin
  inherited;
  for Loop := 0 to ComponentCount - 1 do
    if Components[Loop] is TscCustomDBGrid then
      with Components[Loop] as TscCustomDBGrid do
        if Parent <> nil then
          Parent.BiDiMode := Self.BiDiMode;
end;

procedure TscCustomDBGrid.CMExit(var Message: TMessage);
begin
  try
    if FDatalink.Active then
      with FDatalink.Dataset do
        if (scdgCancelOnExit in Options) and (State = dsInsert) and
          not Modified and not FDatalink.FModified then
          Cancel else
          FDataLink.UpdateData;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscCustomDBGrid.CMFontChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  BeginLayout;
  try
    for I := 0 to Columns.Count-1 do
      Columns[I].RefreshDefaultFont;
  finally
    EndLayout;
  end;
end;

procedure TscCustomDBGrid.CMDeferLayout(var Message);
begin
  if AcquireLayoutLock then
    EndLayout
  else
    DeferLayout;
end;

procedure TscCustomDBGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  MasterCol: TscColumn;
begin
  inherited;
  if (Msg.Result = 1) and ((FDataLink = nil) or
    ((Columns.State = sccsDefault) and
     (FDataLink.DefaultFields or (not FDataLink.Active)))) then
    Msg.Result := 0
  else if (Msg.Result = 0) and (FDataLink <> nil) and (FDataLink.Active)
    and (Columns.State = sccsCustomized)
    and PtInExpandButton(Msg.XPos, Msg.YPos, MasterCol) then
    Msg.Result := 1;
end;

procedure TscCustomDBGrid.WMSetCursor(var Msg: TWMSetCursor);
begin
  if (csDesigning in ComponentState) and
      ((FDataLink = nil) or
       ((Columns.State = sccsDefault) and
        (FDataLink.DefaultFields or not FDataLink.Active))) then
    Winapi.Windows.SetCursor(LoadCursor(0, IDC_ARROW))
  else inherited;
end;

procedure TscCustomDBGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  if UpdateLock = 0 then UpdateRowCount;
  UpdateScrollBar;
end;

procedure TscCustomDBGrid.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
begin
  if not AcquireFocus then Exit;
  if FDatalink.Active then
    with Message, FDataLink.DataSet do
      case ScrollCode of
        SB_LINEUP: FDataLink.MoveBy(-FDatalink.ActiveRecord - 1);
        SB_LINEDOWN: FDataLink.MoveBy(FDatalink.RecordCount - FDatalink.ActiveRecord);
        SB_PAGEUP: FDataLink.MoveBy(-VisibleRowCount);
        SB_PAGEDOWN: FDataLink.MoveBy(VisibleRowCount);
        SB_THUMBPOSITION:
          begin
            if IsSequenced then
            begin
              SI.cbSize := sizeof(SI);
              SI.fMask := SIF_ALL;
              GetScrollInfo(Self.Handle, SB_VERT, SI);
              if SI.nTrackPos <= 1 then First
              else if SI.nTrackPos >= RecordCount then Last
              else RecNo := SI.nTrackPos;
            end
            else
              case Pos of
                0: First;
                1: FDataLink.MoveBy(-VisibleRowCount);
                2: Exit;
                3: FDataLink.MoveBy(VisibleRowCount);
                4: Last;
              end;
          end;
        SB_BOTTOM: Last;
        SB_TOP: First;
      end;
  RePaintControl;
end;

procedure TscCustomDBGrid.SetIme;
var
  Column: TscColumn;
begin
  if Columns.Count = 0 then Exit;

  ImeName := FOriginalImeName;
  ImeMode := FOriginalImeMode;
  Column := Columns[SelectedIndex];
  if Column.IsImeNameStored then ImeName := Column.ImeName;
  if Column.IsImeModeStored then ImeMode := Column.ImeMode;

  FNeedRestoreImeName := ImeName <> '';

  if InplaceEditor <> nil then
  begin
    TscDBGridInplaceEdit(InplaceEditor).ImeName := ImeName;
    TscDBGridInplaceEdit(InplaceEditor).ImeMode := ImeMode;
  end;
end;

procedure TscCustomDBGrid.UpdateIme;
begin
  SetIme;
  SetImeName(ImeName);
  SetImeMode(Handle, ImeMode);
end;

procedure TscCustomDBGrid.WMIMEStartComp(var Message: TMessage);
begin
  inherited;
  ShowEditor;
end;

procedure TscCustomDBGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  FNeedRestoreImeName := False;
  if not ((InplaceEditor <> nil) and
    (Message.FocusedWnd = InplaceEditor.Handle)) then SetIme;
  inherited;
end;

procedure TscCustomDBGrid.WMKillFocus(var Message: TMessage);
begin
  ImeName := Screen.DefaultIme;
  ImeMode := imDontCare;
  inherited;
  if FNeedRestoreImeName then
    if not ((InplaceEditor <> nil) and
      (HWND(Message.WParam) = InplaceEditor.Handle)) then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
end;

function TscCustomDBGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil) and DataLink.ExecuteAction(Action);
end;

function TscCustomDBGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil) and DataLink.UpdateAction(Action);
end;

procedure TscCustomDBGrid.ShowPopupEditor(Column: TscColumn; X, Y: Integer);
var
  SubGrid: TscCustomDBGrid;
  DS: TDataSource;
  I: Integer;
  FloatRect: TRect;
  Cmp: TControl;
begin
  if not ((Column.Field <> nil) and (Column.Field is TDataSetField)) then  Exit;

  for I := 0 to ComponentCount-1 do
    if Components[I] is TscCustomDBGrid then
    begin
      SubGrid := TscCustomDBGrid(Components[I]);
      if (SubGrid.DataSource <> nil) and
        (SubGrid.DataSource.DataSet = (Column.Field as TDatasetField).NestedDataset) and
        SubGrid.CanFocus then
      begin
        SubGrid.Parent.Show;
        SubGrid.SetFocus;
        Exit;
      end;
    end;

  SubGrid := TscCustomDBGrid(TComponentClass(Self.ClassType).Create(Self));
  try
    DS := TDataSource.Create(SubGrid);
    DS.Dataset := (Column.Field as TDatasetField).NestedDataset;
    DS.DataSet.CheckBrowseMode;
    SubGrid.DataSource := DS;
    SubGrid.Columns.State := Columns.State;
    SubGrid.Columns[0].Expanded := True;
    SubGrid.Visible := False;
    SubGrid.FloatingDockSiteClass := TCustomDockForm;
    FloatRect.TopLeft := ClientToScreen(CellRect(Col, Row).BottomRight);
    if X > Low(Integer) then FloatRect.Left := X;
    if Y > Low(Integer) then FloatRect.Top := Y;
    FloatRect.Right := FloatRect.Left + Width;
    FloatRect.Bottom := FloatRect.Top + Height;
    SubGrid.ManualFloat(FloatRect);

    SubGrid.Parent.BiDiMode := Self.BiDiMode;
    I := SubGrid.CellRect(SubGrid.ColCount-1, 0).Right;
    if (I > 0) and (I < Screen.Width div 2) then
      SubGrid.Parent.ClientWidth := I
    else
      SubGrid.Parent.Width := Screen.Width div 4;
    SubGrid.Parent.Height := Screen.Height div 4;
    SubGrid.Align := alClient;
    SubGrid.DragKind := dkDock;
    SubGrid.Color := Color;
    SubGrid.Ctl3D := Ctl3D;
    SubGrid.Cursor := Cursor;
    SubGrid.Enabled := Enabled;
    SubGrid.FixedColor := FixedColor;
    SubGrid.Font := Font;
    SubGrid.HelpContext := HelpContext;
    SubGrid.IMEMode := IMEMode;
    SubGrid.IMEName := IMEName;
    SubGrid.Options := Options;
    Cmp := Self;
    while (Cmp <> nil) and (TscCustomDBGrid(Cmp).PopupMenu = nil) do
      Cmp := Cmp.Parent;
    if Cmp <> nil then
      SubGrid.PopupMenu := TscCustomDBGrid(Cmp).PopupMenu;
    SubGrid.TitleFont := TitleFont;
    SubGrid.Visible := True;
    SubGrid.Parent.Show;
  except
    SubGrid.Free;
    raise;
  end;
end;

procedure TscCustomDBGrid.CalcSizingState(X, Y: Integer;
  var State: TscGridState; var Index, SizingPos, SizingOfs: Integer;
  var FixedInfo: TscGridDrawInfo);
var
  R: TscGridCoord;
begin
  inherited CalcSizingState(X, Y, State, Index, SizingPos, SizingOfs, FixedInfo);
  if (State = scgsColSizing) and (FDataLink <> nil)
    and (FDatalink.Dataset <> nil) and FDataLink.Dataset.ObjectView then
  begin
    R := MouseCoord(X, Y);
    R.X := RawToDataColumn(R.X);
    if (R.X >= 0) and (R.X < Columns.Count) and (Columns[R.X].Depth > R.Y) then
      State := scgsNormal;
  end;
end;

function TscCustomDBGrid.CheckColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
var
  I, ARow: Integer;
  DestCol: TscColumn;
begin
  Result := inherited CheckColumnDrag(Origin, Destination, MousePt);
  if Result and (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView then
  begin
    assert(FDragCol <> nil);
    ARow := FDragCol.Depth;
    if Destination <> Origin then
    begin
      DestCol := ColumnAtDepth(Columns[RawToDataColumn(Destination)], ARow);
      if DestCol.ParentColumn <> FDragCol.ParentColumn then
        if Destination < Origin then
          DestCol := Columns[FDragCol.ParentColumn.Index+1]
        else
        begin
          I := DestCol.Index;
          while DestCol.ParentColumn <> FDragCol.ParentColumn do
          begin
            Dec(I);
            DestCol := Columns[I];
          end;
        end;
      if (DestCol.Index > FDragCol.Index) then
      begin
        I := DestCol.Index + 1;
        while (I < Columns.Count) and (ColumnAtDepth(Columns[I],ARow) = DestCol) do
          Inc(I);
        DestCol := Columns[I-1];
      end;
      Destination := DataToRawColumn(DestCol.Index);
    end;
  end;
end;

function TscCustomDBGrid.BeginColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
var
  I, ARow: Integer;
begin
  Result := inherited BeginColumnDrag(Origin, Destination, MousePt);
  if Result and (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView then
  begin
    ARow := MouseCoord(MousePt.X, MousePt.Y).Y;
    FDragCol := ColumnAtDepth(Columns[RawToDataColumn(Origin)], ARow);
    if FDragCol = nil then Exit;
    I := DataToRawColumn(FDragCol.Index);
    if Origin <> I then Origin := I;
    Destination := Origin;
  end;
end;

function TscCustomDBGrid.EndColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := inherited EndColumnDrag(Origin, Destination, MousePt);
  FDragCol := nil;
end;

procedure TscCustomDBGrid.InvalidateTitles;
var
  R: TRect;
  DrawInfo: TscGridDrawInfo;
begin
  if HandleAllocated and (scdgTitles in Options) then
  begin
    CalcFixedInfo(DrawInfo);
    R := Rect(0, 0, Width, DrawInfo.Vert.FixedBoundary);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TscCustomDBGrid.TopLeftChanged;
begin
  InvalidateTitles;
  inherited TopLeftChanged;
end;

initialization

  TCustomStyleEngine.RegisterStyleHook(TscDBGrid, TscScrollingStyleHook);

finalization

  TCustomStyleEngine.UnRegisterStyleHook(TscDBGrid, TscScrollingStyleHook);

end.
