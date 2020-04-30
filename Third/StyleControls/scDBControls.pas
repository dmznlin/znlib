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

unit scDBControls;

{$I scdefine.inc}
{$R-}

interface
  uses System.Variants, Winapi.Windows, System.SysUtils, Winapi.Messages,
     Vcl.Controls, System.Classes, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.Themes,
     Vcl.Mask, Data.DB, VCl.DBCtrls, scDrawUtils, scControls, scExtControls,
     scCalendar, WinApi.RichEdit, scAdvancedControls, scImageCollection,
     scDialogs, scModernControls;

type
  TscDBText = class(TscLabel)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldText: string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function GetLabelText: string; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TscDBEdit = class(TscEdit)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure ResetMaxLength;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
  protected
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  TscDBMemo = class(TscMemo)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FForceDataChange: Boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadMemo; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property ForceDataChange: Boolean read FForceDataChange write FForceDataChange;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  TscDBComboBox = class(TscComboBox)
  private
    FDataLink: TFieldDataLink;
    FEnableValues: Boolean;
    FValues: TStrings;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetComboText(const Value: string);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
  protected
    procedure Change; override;
    procedure Click; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); override;
    procedure CreateWnd; override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetItems(const Value: TStrings); override;
    procedure WndProc(var Message: TMessage); override;
    function GetComboText: string;
    function GetPaintText: string;
    property ComboText: string read GetComboText write SetComboText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Text;
  published
    property Style;
    property Align;
    property Anchors;
    property AutoComplete;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property EnableValues: Boolean read FEnableValues write SetEnableValues;  // AGGIUNTA
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items write SetItems;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Values: TStrings read FValues write SetValues;
    property Touch;
    property Visible;
    {$IFNDEF VER230}
    property StyleElements;
    {$ENDIF}
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBListBox = class(TscListBox)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetItems(Value: TStrings);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Anchors;
    property AutoComplete;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items write SetItems;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property Style;
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
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBCheckBox = class(TscCheckBox)
  private
    FDataLink: TFieldDataLink;
    FValueCheck: string;
    FValueUncheck: string;
    FInDataChange: Boolean;
    FInChange: Boolean;
    FClickOnDataChange: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldState: TCheckBoxState;
    function GetReadOnly: Boolean;
    function IsValueChecked: Boolean;
    function IsValueUnchecked: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    procedure UpdateData(Sender: TObject);
    function ValueMatch(const ValueList, Value: string): Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure SetState(Value: TCheckBoxState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Checked;
    property Field: TField read GetField;
    property State;
  published
    property Action;
    property Align;
    property AllowGrayed;
    property Anchors;
    property BiDiMode;
    property Caption;
    property ClickOnDataChange: Boolean
      read FClickOnDataChange write FClickOnDataChange;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValueChecked: string read FValueCheck write SetValueCheck stored IsValueChecked nodefault;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck stored IsValueUnchecked nodefault;
    property Visible;
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

  TscDBRadioGroup = class(TscCustomRadioGroup)
  private
    FInClick: Boolean;
    FDataLink: TFieldDataLink;
    FValue: string;
    FValues: TStrings;
    FInSetValue: Boolean;
    FOnChange: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetButtonValue(Index: Integer): string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValue(const Value: string);
    procedure SetItems(Value: TStrings);
    procedure SetValues(Value: TStrings);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure ButtonClick(Sender: TObject); override;
    procedure Change; dynamic;
    procedure KeyPress(var Key: Char); override;
    function CanModify: Boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property DataLink: TFieldDataLink read FDataLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property ItemIndex;
    property Value: string read FValue write SetValue;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Columns;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Items write SetItems;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Values: TStrings read FValues write SetValues;
    property Visible;
    property ButtonsImages;
    property ButtonsGlowEffect;
    property ButtonsAnimation;
    property ShowFocusRect;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnButtonClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBNumericEdit = class(TscNumericEdit)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBSpinEdit = class(TscSpinEdit)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBCalcEdit = class(TscCalcEdit)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
    FPopupWasVisible: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBDateEdit = class(TscDateEdit)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    FAllowNullData: boolean;
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    procedure DropDown; override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    published
    property AllowNullData: Boolean read FAllowNullData write FAllowNullData;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBTimeEdit = class(TscTimeEdit)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBPasswordEdit = class(TscPasswordEdit)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    function GetPaintText: String; override;
     procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBProgressBar = class(TscProgressBar)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TscDBTrackBar = class(TscTrackBar)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TscDBRichEdit = class(TscRichEdit)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FDataSave: string;
    FCreatingWnd: Integer;
    function BeginEditing: Boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure EMSetCharFormat(var Message: TMessage); message EM_SETCHARFORMAT;
    procedure EMSetParaFormat(var Message: TMessage); message EM_SETPARAFORMAT;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure CreateWnd; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadMemo; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont default False;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
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
    property OnResizeRequest;
    property OnSelectionChange;
    property OnProtectChange;
    property OnSaveClipboard;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBImage = class(TscPanel)
  private
    FDataLink: TFieldDataLink;
    FPicture: TPicture;
    FAutoDisplay: Boolean;
    FStretch: Boolean;
    FCenter: Boolean;
    FPictureLoaded: Boolean;
    FProportional: Boolean;
    FQuickDraw: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetCenter(Value: Boolean);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetPicture(Value: TPicture);
    procedure SetProportional(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
  protected
    function DestRect(APicture: TPicture): TRect;
    function GetPalette: HPALETTE; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    function GetCaptionText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadPicture;
    procedure PasteFromClipboard;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
    property Picture: TPicture read FPicture write SetPicture;
  published
    property Align;
    property Anchors;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property Center: Boolean read FCenter write SetCenter default True;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Proportional: Boolean read FProportional write SetProportional default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property QuickDraw: Boolean read FQuickDraw write FQuickDraw default True;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property TabOrder;
    property TabStop default True;
    property Touch;
    property Visible;
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

  TscDBAdvancedComboEdit = class(TscAdvancedComboEdit)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
    FPopupWasVisible: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBAdvancedComboBox = class(TscAdvancedComboBox)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;

    FEnableValues: Boolean;
    FValues: TStrings;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;

    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
  protected
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    function GetComboText: string;
    procedure SetComboText(const Value: string);
    property ComboText: string read GetComboText write SetComboText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableValues: Boolean read FEnableValues write SetEnableValues;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Values: TStrings read FValues write SetValues;
    property Visible;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscDBAdvancedListBox = class(TscAdvancedListBox)
  private
    FInDataChange: Boolean;
    FInChange: Boolean;
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMGotFocus); message CM_EXIT;
    function GetListText: string;
    procedure SetListText(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
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

  TscNavButton = class;
  TscNavDataLink = class;

  TscDBNavigator = class (TscPanel)
  private
    FDataLink: TscNavDataLink;
    FVisibleButtons: TNavButtonSet;
    FHints: TStrings;
    FDefHints: TStrings;
    ButtonWidth: Integer;
    ButtonHeight: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: ENavClick;
    FBeforeAction: ENavClick;
    FocusedButton: TNavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    FAnimation: Boolean;
    FMaxErrors: Integer;
    FKind: TDBNavigatorKind;

    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitHints;
    procedure SetDataSource(Value: TDataSource);
    procedure SetFlat(Value: Boolean);
    procedure SetAnimation(Value: Boolean);
    procedure SetHints(Value: TStrings);
    procedure SetKind(Value: TDBNavigatorKind);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(Value: TNavButtonSet);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure ApplyUpdates;
    function CanApplyUpdates: Boolean;
    procedure CancelUpdates;
    function CanCancelUpdates: Boolean;
    procedure InitResImages;
  protected
    procedure ActiveChanged;
    procedure CalcMinSize(var W, H: Integer);
    procedure DataChanged;
    procedure EditingChanged;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    FResImages: TscImageCollection;
    Buttons: array[TNavigateBtn] of TscNavButton;
    FLoadingImages: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure BtnClick(Index: TNavigateBtn); virtual;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TNavButtonSet read FVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property MaxErrors: Integer read FMaxErrors write FMaxErrors default -1;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Animation: Boolean read FAnimation write SetAnimation default False;
    property Hints: TStrings read GetHints write SetHints;
    property Kind: TDBNavigatorKind read FKind write SetKind default dbnHorizontal;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property BeforeAction: ENavClick read FBeforeAction write FBeforeAction;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscNavButton = class(TscButton)
  private
    FIndex: TNavigateBtn;
  protected
    FNavigator: TscDBNavigator;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Index: TNavigateBtn read FIndex write FIndex;
  end;

  TscNavDataLink = class(TDataLink)
  private
    FNavigator: TscDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TscDBNavigator);
    destructor Destroy; override;
  end;


  { TscDBLookupControl }

  TscDBLookupControl = class;

  TscDataSourceLink = class(TDataLink)
  private
    FDBLookupControl: TscDBLookupControl;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create;
  end;

  TscListSourceLink = class(TDataLink)
  private
    FDBLookupControl: TscDBLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
  public
    constructor Create;
  end;

  TscDBLookupControl = class(TCustomControl)
  private
    FLookupSource: TDataSource;
    FDataLink: TscDataSourceLink;
    FListLink: TscListSourceLink;
    FDataFieldName: string;
    FKeyFieldName: string;
    FListFieldName: string;
    FListFieldIndex: Integer;
    FDataField: TField;
    FMasterField: TField;
    FKeyField: TField;
    FListField: TField;
    FListFields: TList;
    FKeyValue: Variant;
    FSearchText: string;
    FLookupMode: Boolean;
    FListActive: Boolean;
    FHasFocus: Boolean;
    FListDataChanging: Integer;
    FNullValueKey: TShortCut;
    procedure CheckNotCircular;
    procedure CheckNotLookup;
    procedure DataLinkRecordChanged(Field: TField);
    function GetDataSource: TDataSource;
    function GetKeyFieldName: string;
    function GetListSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
    procedure SetLookupMode(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function CanModify: Boolean; virtual;
    function GetBorderSize: Integer; virtual;
    function GetTextHeight: Integer; virtual;
    procedure KeyValueChanged; virtual;
    procedure ListLinkDataChanged; virtual;
    function LocateKey: Boolean; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ProcessSearchKey(Key: Char); virtual;
    procedure SelectKeyValue(const Value: Variant); virtual;
    procedure UpdateDataFields; virtual;
    procedure UpdateListFields; virtual;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataLink: TscDataSourceLink read FDataLink;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property HasFocus: Boolean read FHasFocus;
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property KeyValue: Variant read FKeyValue write SetKeyValue;
    property ListActive: Boolean read FListActive;
    property ListField: string read FListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListFields: TList read FListFields;
    property ListLink: TscListSourceLink read FListLink;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property NullValueKey: TShortCut read FNullValueKey write FNullValueKey default 0;
    property ParentColor default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchText: string read FSearchText write FSearchText;
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read FDataField;
  end;

  TscDBLookupListBox = class(TscDBLookupControl)
  private
    FRecordIndex: Integer;
    FRecordCount: Integer;
    FRowCount: Integer;
    FBorderStyle: TBorderStyle;
    FPopup: Boolean;
    FKeySelected: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FLockPosition: Boolean;
    FMousePos: Integer;
    FSelectedItem: string;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FSelectionStyle: TscSelectionStyle;
    FShowFocusRect: Boolean;
    FLineColor: TColor;
    FRowCountFixed: Integer;
    function GetKeyIndex: Integer;
    procedure SelectCurrent;
    procedure SelectItemAt(X, Y: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetRowCount(Value: Integer);
    procedure StopTimer;
    procedure StopTracking;
    procedure TimerScroll;
    procedure UpdateScrollBar;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMTimer(var Message: TMessage); message WM_TIMER;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure SetSelectionStyle(Value: TscSelectionStyle);
    procedure SetSelectionColor(Value: TColor);
    procedure SetSelectionTextColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure SetRowCountFixed(Value: Integer);
  protected
    {$IFNDEF VER330_UP}
    FScaleFactor: Double;
    FScalePercent: Integer;
    {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyValueChanged; override;
    procedure ListLinkDataChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure UpdateListFields; override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    function GetTextHeight: Integer; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property KeyValue;
    property SelectedItem: string read FSelectedItem;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property NullValueKey;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowCount: Integer read FRowCount write SetRowCount stored False;
    property ShowHint;
     property SelectionStyle: TscSelectionStyle
      read FSelectionStyle write SetSelectionStyle;
    property SelectionColor: TColor
      read FSelectionColor write SetSelectionColor;
    property SelectionTextColor: TColor
      read FSelectionTextColor write SetSelectionTextColor;
    property ShowFocusRect: Boolean
      read FShowFocusRect write FShowFocusRect;
    property LineColor: TColor read
      FLineColor write SetLineColor;
    property RowCountFixed: Integer
      read FRowCountFixed write SetRowCountFixed;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
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

  TscPopupDataList = class(TscDBLookupListBox)
  private
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscCustomDBLookupComboBox = class(TscDBLookupControl)
  private
    FDataList: TscPopupDataList;
    FButtonWidth: Integer;
    FButtonRect: TRect;
    FText: string;
    FDropDownRows: Integer;
    FDropDownWidth: Integer;
    FDropDownAlign: TDropDownAlign;
    FListVisible: Boolean;
    FPressed: Boolean;
    FTracking: Boolean;
    FAlignment: TAlignment;
    FLookupMode: Boolean;
    FMouseInControl, FMouseInButton: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FSelectionStyle: TscSelectionStyle;
    FShowFocusRect: Boolean;
    FLineColor: TColor;
    procedure SetSelectionStyle(Value: TscSelectionStyle);
    procedure SetSelectionColor(Value: TColor);
    procedure SetSelectionTextColor(Value: TColor);
    procedure SetLineColor(Value: TColor);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMKillFocus); message WM_SETFOCUS;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure  DrawCombo(ACanvas: TCanvas; ARect: TRect; AText: String; ASelected: Boolean;
      AAlignment: TAlignment);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyValueChanged; override;
    procedure ListLinkDataChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UpdateListFields; override;
    {$IFNDEF VER230}
    procedure UpdateStyleElements; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure DropDown; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property DropDownAlign: TDropDownAlign read FDropDownAlign write FDropDownAlign default daLeft;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows default 7;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property KeyValue;
    property ListVisible: Boolean read FListVisible;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property Text: string read FText;
    property SelectionStyle: TscSelectionStyle
      read FSelectionStyle write SetSelectionStyle;
    property SelectionColor: TColor
      read FSelectionColor write SetSelectionColor;
    property SelectionTextColor: TColor
      read FSelectionTextColor write SetSelectionTextColor;
    property ShowFocusRect: Boolean
      read FShowFocusRect write FShowFocusRect;
    property LineColor: TColor read
      FLineColor write SetLineColor;
  end;

  TscDBLookupComboBox = class(TscCustomDBLookupComboBox)
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownAlign;
    property DropDownRows;
    property DropDownWidth;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property NullValueKey;
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
    property Touch;
    property Visible;
    {$IFNDEF VER230}
    property StyleElements;
    {$ENDIF}
    property SelectionStyle;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
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


  TscDBToggleSwitch = class(TscToggleSwitch)
  private
    FDataLink: TFieldDataLink;
    FValueCheck: string;
    FValueUncheck: string;
    FInDataChange: Boolean;
    FInChange: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldState: TscSwitchState;
    function GetReadOnly: Boolean;
    function IsValueChecked: Boolean;
    function IsValueUnchecked: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    procedure UpdateData(Sender: TObject);
    function ValueMatch(const ValueList, Value: string): Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure ChangeState; override;
    procedure SetState(Value: TscSwitchState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property State;
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValueChecked: string read FValueCheck write SetValueCheck stored IsValueChecked nodefault;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck stored IsValueUnchecked nodefault;
    property Visible;
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

implementation

{$R scDBControls.res}

  Uses System.Types, System.Math, System.UITypes, Vcl.Clipbrd,
    Vcl.Dialogs, Vcl.VDBConsts, Data.DBConsts, Vcl.Menus;

constructor TscDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  AutoSize := False;
  ShowAccelChar := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TscDBText.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBText.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBText.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TscDBText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBText.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBText.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBText.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBText.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TscDBText.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then Result := Name else Result := '';
end;

procedure TscDBText.DataChange(Sender: TObject);
begin
  Caption := GetFieldText;
end;

function TscDBText.GetLabelText: string;
begin
  if csPaintCopy in ControlState then
    Result := GetFieldText else
    Result := Caption;
end;

procedure TscDBText.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscDBText.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBText.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TscDBEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

constructor TscDbEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TscDbEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TscDbEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then
    DataChange(Self)
  else
  if DataSource = nil then
    EditText := '';
end;

procedure TscDbEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDbEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscDbEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TscDbEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

function TscDbEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TscDbEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscDbEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscDbEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TscDbEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDbEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDbEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDbEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TscDbEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDbEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDbEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDbEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TscDbEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FAlignment <> FDataLink.Field.Alignment then
    begin
      EditText := '';
      FAlignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      if FDataLink.Editing and FDataLink.CanModify then
      Modified := True;
    end;
  end else
  begin
    FAlignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
    begin
      if Text <> '' then
        EditText := Name;
    end
    else
      EditText := '';
  end;
end;

procedure TscDbEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TscDbEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TscDbEdit.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscDbEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscDbEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscDbEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify
  then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and
      not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDbEdit.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TscDbEdit.WMPaint(var Message: TWMPaint);
const
  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));

function IsPaintCopy: Boolean;
begin
  Result := csPaintCopy in ControlState;
end;

var
  Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  ExStyle: DWORD;
  C: TColor;
begin
  AAlignment := FAlignment;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  if ((AAlignment = taLeftJustify) or FFocused) and
    not IsPaintCopy then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    begin
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;
    inherited;
    Exit;
  end;
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      DrawEditBackground(FCanvas);
      R := ClientRect;
      if IsPaintCopy and (FDataLink.Field <> nil) and not FStopDraw then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      R := GetTextRect;
      case AAlignment of
        taLeftJustify: Left := R.Left + 1;
        taRightJustify: Left := R.Right - TextWidth(S) - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then UpdateTextFlags;
      Brush.Style := bsClear;
      C := GetTextColor;
      Font.Color := C;
      TextRect(R, Left, R.Top + 1, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TscDbEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDbEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDbEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  ForceDataChange := True;
end;

destructor TscDBMemo.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBMemo.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TscDBMemo.Loaded;
begin
  inherited Loaded;
end;

procedure TscDBMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBMemo.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscDBMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
      FDataLink.Edit;
  end;
end;

procedure TscDBMemo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if CharInSet(Key, [#32..#255]) and (FDataLink.Field <> nil) and
      not FDataLink.Field.IsValidChar(Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..#255:
        FDataLink.Edit;
      #27:
        FDataLink.Reset;
    end;
  end else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;
end;

procedure TscDBMemo.Change;
begin
  if FMemoLoaded then FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TscDBMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBMemo.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBMemo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBMemo.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBMemo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBMemo.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBMemo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBMemo.LoadMemo;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
  begin
    try
      Lines.Text := FDataLink.Field.AsString;
      FMemoLoaded := True;
    except
      { Memo too large }
      on E:EInvalidOperation do
        Lines.Text := Format('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TscDBMemo.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FDataLink.Field.IsBlob then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FMemoLoaded := False;
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
    end
  else
  begin
    if csDesigning in ComponentState then Text := Name else Text := '';
    FMemoLoaded := False;
  end;

 if FForceDataChange then
 begin
   FStopDraw := True;
   if HandleAllocated then
     DoPaint;
 end
end;

procedure TscDBMemo.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TscDBMemo.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsString := Text;
end;

procedure TscDBMemo.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TscDBMemo.WndProc(var Message: TMessage);
begin
  inherited;
end;

procedure TscDBMemo.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;

  if (FDataLink.DataSource <> nil) and not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBMemo.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;

procedure TscDBMemo.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TscDBMemo.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TscDBMemo.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscDBMemo.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscDBMemo.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscDBMemo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBMemo.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBMemo.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];

  FValues := TStringList.Create;
  TStringList(FValues).OnChange := ValuesChanged;
  EnableValues := False;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
end;

destructor TscDBComboBox.Destroy;
begin
  TStringList(FValues).OnChange := nil;
  FValues.Free;

  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
end;

procedure TscDBComboBox.DataChange(Sender: TObject);
begin
  if not (Style = csSimple) and DroppedDown then Exit;
  if FDataLink.Field <> nil then
    ComboText := FDataLink.Field.Text
  else
    if csDesigning in ComponentState then
      ComboText := Name
    else
      ComboText := '';
end;

procedure TscDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetComboText;
end;

procedure TscDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> GetComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then
          I := -1
        else
          Begin
            if FEnableValues then
              I := Values.IndexOf(Value)
            else
              I := Items.IndexOf(Value);
          End;
        if I >= Items.Count then I := -1;
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then
    begin
      Text := Value;
      if Value = '' then
        ItemIndex := -1
      else
        if FEnableValues then
          ItemIndex := Values.IndexOf(Value)
        else
          ItemIndex := Items.IndexOf(Value);
    end;
  end;
end;

function TscDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if (Style in [csDropDown, csSimple]) and (not FEnableValues) then
    Result := Text
  else
    begin
      I := ItemIndex;
      if (I < 0) or (FEnableValues and (FValues.Count < I + 1)) then
        Result := ''
      else if FEnableValues then Result := FValues[I]
      else Result := Items[I];
    end;
end;

procedure TscDBComboBox.Change;
begin
  FDataLink.Edit;
  inherited Change;
  FDataLink.Modified;
end;

procedure TscDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

procedure TscDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
    begin
      FEnableValues := Value;
      DataChange(Self);
    end;
end;

procedure TscDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

function TscDBComboBox.GetPaintText: string;
var
  I: Integer;
begin
  Result := '';
  if FDataLink.Field <> nil then begin
    if FEnableValues then begin
      I := Values.IndexOf(FDataLink.Field.Text);
      if I >= 0 then Result := Items.Strings[I]
    end
    else Result := FDataLink.Field.Text;
  end;
end;

procedure TscDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

procedure TscDBComboBox.DropDown;
begin
  inherited DropDown;
end;

function TscDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBComboBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
  begin
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
  end;
end;

procedure TscDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key >= #32) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..High(Char):
      if not FDataLink.Edit then
        Key := #0;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TscDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TscDBComboBox.SetEditReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

procedure TscDBComboBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_COMMAND:
        if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
          if not FDataLink.Edit then
          begin
            if Style <> csSimple then
              PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
            Exit;
          end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then FDataLink.Edit else
          if not FDataLink.Editing then DataChange(Self); {Restore text}
    end;
  inherited WndProc(Message);
end;

procedure TscDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if (Style = csSimple) and (ComboWnd <> EditHandle) then
          if not FDataLink.Edit then
            Exit;
      WM_PASTE, WM_CUT, WM_UNDO, WM_CLEAR:
         if not FDataLink.Edit then
           Exit;
    end;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
end;

procedure TscDBComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if FDataLink.CanModify then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(False), 0);
end;

procedure TscDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscDBComboBox.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  DataChange(Self);
end;

function TscDBComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscDBComboBox.CMGetDatalink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscDBComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TscDBListBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscDBListBox.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    ItemIndex := Items.IndexOf(FDataLink.Field.Text) else
    ItemIndex := -1;
end;

procedure TscDBListBox.UpdateData(Sender: TObject);
begin
  if ItemIndex >= 0 then
    FDataLink.Field.Text := Items[ItemIndex] else
    FDataLink.Field.Text := '';
end;

procedure TscDBListBox.Click;
begin
  if FDataLink.Edit then
  begin
    inherited Click;
    FDataLink.Modified;
  end;
end;

function TscDBListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBListBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBListBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBListBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBListBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBListBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP,
    VK_RIGHT, VK_DOWN] then
    if not FDataLink.Edit then
      Key := 0;
end;

procedure TscDBListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..High(Char):
      if not FDataLink.Edit then
        Key := #0;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TscDBListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if FDataLink.Edit then inherited
  else
  begin
    SetFocus;
    with Message do
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
  end;
end;

procedure TscDBListBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscDBListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

procedure TscDBListBox.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

function TscDBListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  State := cbUnchecked;
  FInDataChange := False;
  FClickOnDataChange := True;
  FInChange := False;
  FValueCheck := 'True';
  FValueUncheck := 'False';
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TscDBCheckBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBCheckBox.SetState(Value: TCheckBoxState);
begin
  FInChange := True;
  if FState <> Value then
  begin
    if not FInDataChange and (FDataLink <> nil) and
       not ReadOnly and FDataLink.CanModify and FDataLink.Edit
    then
      begin
        inherited;
        FDataLink.Modified;
      end
    else
    if not ((ReadOnly or not FDataLink.Edit) and not FInDataChange) then
    begin
      FState := Value;
      RePaintControl;
    end;
  end;
  FInChange := False;
end;

procedure TscDBCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBCheckBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TscDBCheckBox.GetFieldState: TCheckBoxState;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := cbGrayed
    else if FDataLink.Field.DataType = ftBoolean then
      if FDataLink.Field.AsBoolean then
        Result := cbChecked
      else
        Result := cbUnchecked
    else
    begin
      Result := cbGrayed;
      Text := FDataLink.Field.Text;
      if ValueMatch(FValueCheck, Text) then Result := cbChecked else
        if ValueMatch(FValueUncheck, Text) then Result := cbUnchecked;
    end
  else
    Result := cbUnchecked;
end;

procedure TscDBCheckBox.DataChange(Sender: TObject);
var
  B: TCheckBoxState;
begin
  FInDataChange := True;
  if not FInChange then
  begin
    B := State;
    State := GetFieldState;
    if FClickOnDataChange and Assigned(FOnClick) and (B <> State) and not (csLoading in ComponentState)
       and not (csDesigning in ComponentState)
    then
      FOnClick(Self);
  end;
  FInDataChange := False;
end;

procedure TscDBCheckBox.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
  if State = cbGrayed then
    FDataLink.Field.Clear
  else
    if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean := Checked
    else
    begin
      if Checked then S := FValueCheck else S := FValueUncheck;
      Pos := 1;
      FDataLink.Field.Text := ExtractFieldName(S, Pos);
    end;
end;

function TscDBCheckBox.ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    begin
      Result := True;
      Break;
    end;
end;

function TscDBCheckBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBCheckBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBCheckBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBCheckBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBCheckBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TscDBCheckBox.IsValueChecked: Boolean;
begin
  Result := not SameText(FValueCheck, 'True');
end;

function TscDBCheckBox.IsValueUnchecked: Boolean;
begin
  Result := not SameText(FValueUncheck, 'False');
end;

procedure TscDBCheckBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBCheckBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBCheckBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TscDBCheckBox.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TscDBCheckBox.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

procedure TscDBCheckBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscDBCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscDBCheckBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBCheckBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FValues := TStringList.Create;
  FInClick := False;
end;

destructor TscDBRadioGroup .Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;
  inherited Destroy;
end;

procedure TscDBRadioGroup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBRadioGroup .UseRightToLeftAlignment: Boolean;
begin
  Result := inherited UseRightToLeftAlignment;
end;

procedure TscDBRadioGroup.DataChange(Sender: TObject);
begin
  if not FInClick then
  if FDataLink.Field <> nil then
    Value := FDataLink.Field.Text else
    Value := '';
end;

procedure TscDBRadioGroup.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil then FDataLink.Field.Text := Value;
end;

function TscDBRadioGroup.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBRadioGroup.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBRadioGroup.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBRadioGroup.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBRadioGroup.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBRadioGroup.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBRadioGroup.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TscDBRadioGroup.GetButtonValue(Index: Integer): string;
begin
  if (Index < FValues.Count) and (FValues[Index] <> '') then
    Result := FValues[Index]
  else if Index < Items.Count then
    Result := Items[Index]
  else
    Result := '';
end;

procedure TscDBRadioGroup.SetValue(const Value: string);
var
  I, Index: Integer;
begin
  if FValue <> Value then
  begin
    FInSetValue := True;
    try
      Index := -1;
      for I := 0 to Items.Count - 1 do
        if Value = GetButtonValue(I) then
        begin
          Index := I;
          Break;
        end;
      ItemIndex := Index;
    finally
      FInSetValue := False;
    end;
    FValue := Value;
    Change;
  end;
end;

procedure TscDBRadioGroup.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    if ItemIndex >= 0 then
      TscRadioButton(Controls[ItemIndex]).SetFocus else
      TscRadioButton(Controls[0]).SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscDBRadioGroup.ButtonClick;
begin
  if not FInSetValue then
  begin
    inherited ButtonClick(Sender);
    FInClick := True;
    if ItemIndex >= 0
    then Value := GetButtonValue(ItemIndex);
    if not ReadOnly  and not FDataLink.Editing then FDataLink.Edit;
    if FDataLink.Editing
    then FDataLink.Modified;
    FInClick := False;
  end;
end;

procedure TscDBRadioGroup.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TscDBRadioGroup.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
  DataChange(Self);
end;

procedure TscDBRadioGroup.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscDBRadioGroup.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ': FDataLink.Edit;
    #27: FDataLink.Reset;
  end;
end;

function TscDBRadioGroup.CanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

function TscDBRadioGroup.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataLink <> nil) and
    DataLink.ExecuteAction(Action);
end;

function TscDBRadioGroup.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataLink <> nil) and
    DataLink.UpdateAction(Action);
end;

constructor TscDBNumericEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBNumericEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBNumericEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TscDBNumericEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscDBNumericEdit.WMPaint(var Message: TWMPaint);

function IsPaintCopy: Boolean;
begin
  Result := csPaintCopy in ControlState;
end;

var
  Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  C: TColor;
  FCanvas: TCanvas;
begin
  if not IsPaintCopy or FFocused then
  begin
    inherited;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      DrawEditBackground(FCanvas);
      R := ClientRect;
      if IsPaintCopy and (FDataLink.Field <> nil) and not FStopDraw then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      R := GetTextRect;
      case Alignment of
        taLeftJustify: Left := R.Left + 1;
        taRightJustify: Left := R.Right - TextWidth(S) - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      Brush.Style := bsClear;
      C := GetTextColor;
      Font.Color := C;
      TextRect(R, Left, R.Top + 1, S);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TscDBNumericEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBNumericEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBNumericEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscDBNumericEdit.Change;
begin
  inherited;
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
    end;
  FInChange := False;
end;

function TscDBNumericEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBNumericEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBNumericEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBNumericEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBNumericEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBNumericEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBNumericEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBNumericEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') and IsNumText(FDataLink.Field.Text)
      then
        Text := FDataLink.Field.Text
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
  FInDataChange := False;
end;

procedure TscDBNumericEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscDBNumericEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify
  then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and
      not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBNumericEdit.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;


procedure TscDBNumericEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBNumericEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBNumericEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBSpinEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBSpinEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TscDBSpinEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscDBSpinEdit.WMPaint(var Message: TWMPaint);

function IsPaintCopy: Boolean;
begin
  Result := csPaintCopy in ControlState;
end;

var
  Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  C: TColor;
  FCanvas: TCanvas;
begin
  if not IsPaintCopy or FFocused then
  begin
    inherited;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      DrawEditBackground(FCanvas);
      R := ClientRect;
      if IsPaintCopy and (FDataLink.Field <> nil) and not FStopDraw then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      R := GetTextRect;
      case Alignment of
        taLeftJustify: Left := R.Left + 1;
        taRightJustify: Left := R.Right - TextWidth(S) - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      Brush.Style := bsClear;
      C := GetTextColor;
      Font.Color := C;
      TextRect(R, Left, R.Top + 1, S);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TscDBSpinEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBSpinEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscDBSpinEdit.Change;
begin
  inherited;
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
    end;
  FInChange := False;
end;

function TscDBSpinEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBSpinEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBSpinEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBSpinEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBSpinEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBSpinEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBSpinEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') and IsNumText(FDataLink.Field.Text)
      then
        Text := FDataLink.Field.Text
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
  FInDataChange := False;
end;

procedure TscDBSpinEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscDBSpinEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify
  then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and
      not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBSpinEdit.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TscDBSpinEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBSpinEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBSpinEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
  FPopupWasVisible := False;
end;

destructor TscDBCalcEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
     FPopupWasVisible := IsPopupVisible;
  inherited;
end;

procedure TscDBCalcEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      if not FPopupWasVisible then
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
  FPopupWasVisible := False;
end;

procedure TscDBCalcEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscDBCalcEdit.WMPaint(var Message: TWMPaint);

function IsPaintCopy: Boolean;
begin
  Result := csPaintCopy in ControlState;
end;

var
  Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  C: TColor;
  FCanvas: TCanvas;
begin
  if not IsPaintCopy or FFocused then
  begin
    inherited;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      DrawEditBackground(FCanvas);
      R := ClientRect;
      if IsPaintCopy and (FDataLink.Field <> nil) and not FStopDraw then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      R := GetTextRect;
      case Alignment of
        taLeftJustify: Left := R.Left + 1;
        taRightJustify: Left := R.Right - TextWidth(S) - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      Brush.Style := bsClear;
      C := GetTextColor;
      Font.Color := C;
      TextRect(R, Left, R.Top + 1, S);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TscDBCalcEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBCalcEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscDBCalcEdit.Change;
begin
  inherited;
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
    end;
  FInChange := False;
end;

function TscDBCalcEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBCalcEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBCalcEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBCalcEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBCalcEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBCalcEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBCalcEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBCalcEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') and IsNumText(FDataLink.Field.Text)
      then
        Text := FDataLink.Field.Text
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
  FInDataChange := False;
end;

procedure TscDBCalcEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscDBCalcEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify
  then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and
      not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBCalcEdit.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TscDBCalcEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBCalcEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBCalcEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
  FAllowNullData := False;
end;

destructor TscDBDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBDateEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TscDBDateEdit.DropDown;
begin
  if ReadOnly then
    Exit;

  try
    if FDataLink <> nil then
      if FDataLink.Field <> nil then
        if FDataLink.Field.IsNull then
          FMonthCalendar.Date := Now;
  finally
    inherited DropDown;
  end;
end;

procedure TscDBDateEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBDateEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscDBDateEdit.Change;
begin
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify and not (csDesigning in ComponentState)
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
      inherited Change;
    end;
  FInChange := False;
end;

function TscDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBDateEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBDateEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBDateEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
  inherited ReadOnly := Value;
end;

function TscDBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBDateEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '')
      then
        Date := FDataLink.Field.AsDateTime
      else
        begin
          if ToDayDefault then Date := Now;
          Text := '';
        end;
    end;
  FInDataChange := False;
end;

procedure TscDBDateEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscDBDateEdit.UpdateData(Sender: TObject);
begin
  if not (csDesigning in ComponentState)
  then
    FDataLink.Field.AsDateTime := Date;
end;

procedure TscDBDateEdit.WMPaint(var Message: TWMPaint);

function IsPaintCopy: Boolean;
begin
  Result := csPaintCopy in ControlState;
end;

var
  Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  C: TColor;
  FCanvas: TCanvas;
begin
  if not IsPaintCopy or FFocused then
  begin
    inherited;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      DrawEditBackground(FCanvas);
      R := ClientRect;
      if IsPaintCopy and (FDataLink.Field <> nil) and not FStopDraw then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      R := GetTextRect;
      case Alignment of
        taLeftJustify: Left := R.Left + 1;
        taRightJustify: Left := R.Right - TextWidth(S) - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      Brush.Style := bsClear;
      C := GetTextColor;
      Font.Color := C;
      TextRect(R, Left, R.Top + 1, S);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TscDBDateEdit.CMEnter;
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBDateEdit.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing) and
     not Self.IsDateInput and FAllowNullData
  then
    FDataLink.Field.Value := Null;

  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
  SetFocused(False);
  CheckCursor;
end;

procedure TscDBDateEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBDateEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBDateEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBTimeEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBTimeEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TscDBTimeEdit.WMPaint(var Message: TWMPaint);

function IsPaintCopy: Boolean;
begin
  Result := csPaintCopy in ControlState;
end;

var
  Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  C: TColor;
  FCanvas: TCanvas;
begin
  if not IsPaintCopy or FFocused then
  begin
    inherited;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      DrawEditBackground(FCanvas);
      R := ClientRect;
      if IsPaintCopy and (FDataLink.Field <> nil) and not FStopDraw then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      R := GetTextRect;
      case Alignment of
        taLeftJustify: Left := R.Left + 1;
        taRightJustify: Left := R.Right - TextWidth(S) - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      Brush.Style := bsClear;
      C := GetTextColor;
      Font.Color := C;
      TextRect(R, Left, R.Top + 1, S);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TscDBTimeEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBTimeEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBTimeEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscDBTimeEdit.Change;
begin
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
      inherited Change;
    end;
  FInChange := False;
end;

function TscDBTimeEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBTimeEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscDBTimeEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBTimeEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBTimeEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBTimeEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBTimeEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBTimeEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBTimeEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if FDataLink.Field.Text <> '' then
        TimeValue := FDataLink.Field.AsDateTime
      else
        Text := '';
    end;
  FInDataChange := False;
end;

procedure TscDBTimeEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsDateTime := TimeValue;
end;

procedure TscDBTimeEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify
  then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and
      not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBTimeEdit.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TscDBTimeEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBTimeEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBTimeEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBPasswordEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBPasswordEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBPasswordEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

function TscDBPasswordEdit.GetPaintText;
begin
  if csPaintCopy in ControlState
  then
    begin
      if FDataLink.Field <> nil
      then
        Result := FDataLink.Field.AsString
      else
        inherited GetPaintText;
    end
  else
    inherited GetPaintText;
end;

procedure TscDBPasswordEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBPasswordEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBPasswordEdit.Change;
begin
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
      inherited Change;
    end;
  FInChange := False;
end;

function TscDBPasswordEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBPasswordEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBPasswordEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBPasswordEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBPasswordEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBPasswordEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBPasswordEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBPasswordEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    Text := FDataLink.Field.Text;
  FInDataChange := False;
end;

procedure TscDBPasswordEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscDBPasswordEdit.CMEnter;
begin
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBPasswordEdit.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
end;

procedure TscDBPasswordEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBPasswordEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBPasswordEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBProgressBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBProgressBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBProgressBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBProgressBar.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBProgressBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBProgressBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function IsNumText(AText: String): Boolean;

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
  S := S + FormatSettings.DecimalSeparator;
  if (AText = '') or (AText = '-')
  then
    begin
      Result := False;
      Exit;
    end;

  for i := 1 to Length(AText) do
  begin
    if Pos(AText[i], S) = 0
    then
      begin
        Result := False;
        Break;
      end;
  end;

  Result := Result and GetMinus and GetP;
end;


procedure TscDBProgressBar.DataChange(Sender: TObject);
var
  D: Double;
begin
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') and IsNumText(FDataLink.Field.Text)
      then
        begin
          if Pos(FormatSettings.DecimalSeparator, FDataLink.Field.Text) <> 0
          then
            begin
              D := StrToFloat(FDataLink.Field.Text);
              Value := Trunc(D);
            end
          else
            Value := StrToInt(FDataLink.Field.Text)
        end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
end;

procedure TscDBProgressBar.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBProgressBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBProgressBar.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBTrackBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBTrackBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBTrackBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBTrackBar.Change;
begin
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
      inherited Change;
    end;
  FInChange := False;
end;

function TscDBTrackBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBTrackBar.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBTrackBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBTrackBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBTrackBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBTrackBar.DataChange(Sender: TObject);
var
  D: Double;
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') and IsNumText(FDataLink.Field.Text)
      then
        begin
          if Pos(FormatSettings.DecimalSeparator, FDataLink.Field.Text) <> 0
          then
            begin
              D := StrToFloat(FDataLink.Field.Text);
              Value := Trunc(D);
            end
          else
            Value := StrToInt(FDataLink.Field.Text)
        end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
  FInDataChange := False;
end;

procedure TscDBTrackBar.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := IntToStr(Value);
end;

procedure TscDBTrackBar.CMEnter;
begin
  inherited;
end;

procedure TscDBTrackBar.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
end;

procedure TscDBTrackBar.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBTrackBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBTrackBar.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

procedure TscDBRichEdit.CreateWnd;
begin
  inc(FCreatingWnd);
  try
    inherited;
  finally
    dec(FCreatingWnd);
  end;
end;

destructor TscDBRichEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBRichEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBRichEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBRichEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TscDBRichEdit.BeginEditing: Boolean;
begin
  Result := FDataLink.Editing;
  if not FDataLink.Editing then
  try
    if (FDataLink.Field <> nil) and (FDataLink.Field.IsBlob) then
      FDataSave := FDataLink.Field.AsString;
    Result := FDataLink.Edit;
  finally
    FDataSave := '';
  end;
end;

procedure TscDBRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or (Key = VK_BACK) or
      ((Key = VK_INSERT) and (ssShift in Shift)) or
      (((Key = Ord('V')) or (Key = Ord('X'))) and (ssCtrl in Shift)) then
    begin
      if not BeginEditing then
        Key := 0;
    end;
  end;
end;

procedure TscDBRichEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if (Key >= #32) and (FDataLink.Field <> nil) and
      not FDataLink.Field.IsValidChar(Key) then
    begin
      MessageBeep(0);
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..High(Char):
        begin
          if not BeginEditing then
            Key := #0;
        end;
      #27:
        FDataLink.Reset;
    end;
  end else
  begin
    if Key = #13 then LoadMemo;
    Key := #0;
  end;
end;

procedure TscDBRichEdit.Change;
begin
  if FMemoLoaded then FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TscDBRichEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBRichEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBRichEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBRichEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBRichEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBRichEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBRichEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBRichEdit.LoadMemo;
const
  RTFHeader = '{\rtf';
  URTFHeader = '{urtf';
var
  Data: string;
  Stream: TStringStream;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
  begin
    try
      Data := FDataLink.Field.AsString;
      if (Pos(RTFHeader, Data) = 1) or (Pos(URTFHeader, Data) = 1) then
      begin
        Stream := TStringStream.Create(Data);
        try
          Lines.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;
      end
      else
        Text := Data;
      FMemoLoaded := True;
    except
      on E:EOutOfResources do
        Lines.Text := Format('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  end;
end;

procedure TscDBRichEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FDataLink.Field.IsBlob then
    begin
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then
      begin
        if (FDataSave <> '') and (FDataSave = FDataLink.Field.AsString) then Exit;
        FMemoLoaded := False;
        LoadMemo;
      end else
      begin
        Text := Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FMemoLoaded := False;
      end;
    end else
    begin
      if FFocused and FDataLink.CanModify then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
    end
  else
  begin
    if csDesigning in ComponentState then Text := Name else Text := '';
    FMemoLoaded := False;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TscDBRichEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TscDBRichEdit.EMSetCharFormat(var Message: TMessage);
begin
  if FCreatingWnd=0 then
    BeginEditing;
  inherited;
end;

procedure TscDBRichEdit.EMSetParaFormat(var Message: TMessage);
begin
  BeginEditing;
  inherited;
end;

procedure TscDBRichEdit.UpdateData(Sender: TObject);
var
  Stream: TStringStream;
begin
  if FDataLink.Field.IsBlob then
  begin
    Stream := TStringStream.Create('');
    try
      Lines.SaveToStream(Stream);
      FDataLink.Field.AsString := Stream.DataString;
    finally
      Stream.Free;
    end;
  end else
    FDataLink.Field.AsString := Text;
end;

procedure TscDBRichEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TscDBRichEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TscDBRichEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;

procedure TscDBRichEdit.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TscDBRichEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TscDBRichEdit.WMCut(var Message: TMessage);
begin
  if BeginEditing then
    inherited;
end;

procedure TscDBRichEdit.WMPaste(var Message: TMessage);
begin
  if BeginEditing then
    inherited;
end;

procedure TscDBRichEdit.WMClear(var Message: TMessage);
begin
  if BeginEditing then
    inherited;
end;

procedure TscDBRichEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscDBRichEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBRichEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csNeedsBorderPaint];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  Width := 105;
  Height := 105;
  TabStop := True;
  ParentColor := False;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FBorderStyle := scpbsLoweredBevel;
  FAutoDisplay := True;
  FCenter := True;
  FProportional := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FQuickDraw := True;
end;

destructor TscDBImage.Destroy;
begin
  FPicture.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TscDBImage.DestRect(APicture: TPicture): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := APicture.Width;
  h := APicture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
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
end;

function TscDBImage.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBImage.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBImage.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBImage.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBImage.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBImage.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBImage.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TscDBImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic is TBitmap then
    Result := TBitmap(FPicture.Graphic).Palette;
end;

procedure TscDBImage.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadPicture;
  end;
end;

procedure TscDBImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    RePaintControl;
  end;
end;

procedure TscDBImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TscDBImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    RePaintControl;
  end;
end;

procedure TscDBImage.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    RePaintControl;
  end;
end;

function TscDBImage.GetCaptionText: String;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayLabel
  else
    Result := Name;
  Result := '(' + Result + ')';
end;

procedure TscDBImage.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R: TRect;
  DrawPict: TPicture;
  Pal: HPalette;
  Form: TCustomForm;
begin
  // draw caption
  if not (FPictureLoaded or (csPaintCopy in ControlState)) then
  begin
    FShowCaption := True;
  end;
  inherited;
  if not (FPictureLoaded or (csPaintCopy in ControlState)) then
  begin
    FShowCaption := False;
  end;
  // draw image
  with ACanvas do
  begin
    if FPictureLoaded or (csPaintCopy in ControlState) then
    begin
      DrawPict := TPicture.Create;
      Pal := 0;
      try
        if (csPaintCopy in ControlState) and
          Assigned(FDataLink.Field) and FDataLink.Field.IsBlob then
        begin
          DrawPict.Assign(FDataLink.Field);
          if DrawPict.Graphic is TBitmap then
            DrawPict.Bitmap.IgnorePalette := QuickDraw;
        end
        else
        begin
          DrawPict.Assign(Picture);
          if Focused and (DrawPict.Graphic <> nil) and (DrawPict.Graphic.Palette <> 0) then
          begin
            Pal := SelectPalette(Handle, DrawPict.Graphic.Palette, False);
            RealizePalette(Handle);
          end;
        end;
        if (DrawPict.Graphic = nil) or DrawPict.Graphic.Empty then
        begin
        end
        else
        begin
          R := DestRect(DrawPict);
          StretchDraw(R, DrawPict.Graphic);
        end;
      finally
        if Pal <> 0 then
          SelectPalette(Handle, Pal, True);
        DrawPict.Free;
      end;
    end;
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.ActiveControl = Self) and
      not (csDesigning in ComponentState) and
      not (csPaintCopy in ControlState) then
    begin
      R := Rect(0, 0, Width, Height);
      InflateRect(R, -1, -1);
      ACanvas.Font.Color := GetCaptionColor;
      scDrawFocusRect(ACanvas, R);
    end;
  end;
end;

procedure TscDBImage.PictureChanged(Sender: TObject);
begin
  if FPictureLoaded then FDataLink.Modified;
  FPictureLoaded := True;
  Invalidate;
end;

procedure TscDBImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBImage.LoadPicture;
begin
  if not FPictureLoaded and (not Assigned(FDataLink.Field) or
    FDataLink.Field.IsBlob) then
    Picture.Assign(FDataLink.Field);
end;

procedure TscDBImage.DataChange(Sender: TObject);
begin
  Picture.Graphic := nil;
  FPictureLoaded := False;
  if FAutoDisplay then LoadPicture;
end;

procedure TscDBImage.UpdateData(Sender: TObject);
begin
  if Picture.Graphic is TBitmap then
     FDataLink.Field.Assign(Picture.Graphic) else
     FDataLink.Field.Clear;
end;

procedure TscDBImage.CopyToClipboard;
begin
  if Picture.Graphic <> nil then Clipboard.Assign(Picture);
end;

procedure TscDBImage.CutToClipboard;
begin
  if Picture.Graphic <> nil then
    if FDataLink.Edit then
    begin
      CopyToClipboard;
      Picture.Graphic := nil;
    end;
end;

procedure TscDBImage.PasteFromClipboard;
begin
  if Clipboard.HasFormat(CF_BITMAP) and FDataLink.Edit then
    Picture.Bitmap.Assign(Clipboard);
end;

procedure TscDBImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      if ssShift in Shift then PasteFromClipBoard else
        if ssCtrl in Shift then CopyToClipBoard;
    VK_DELETE:
      if ssShift in Shift then CutToClipBoard;
  end;
end;

procedure TscDBImage.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ^X: CutToClipBoard;
    ^C: CopyToClipBoard;
    ^V: PasteFromClipBoard;
    #13: LoadPicture;
    #27: FDataLink.Reset;
  end;
end;

procedure TscDBImage.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

procedure TscDBImage.CMEnter(var Message: TCMEnter);
begin
  RePaintControl;
  inherited;
end;

procedure TscDBImage.CMExit(var Message: TCMExit);
begin
  try
    if Assigned(DataSource) and Assigned(DataSource.DataSet) and
       (DataSource.DataSet.State in [dsInsert, dsEdit]) then
      FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  RePaintControl;
  inherited;
end;

procedure TscDBImage.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if not FPictureLoaded then Invalidate;
end;

procedure TscDBImage.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if TabStop and CanFocus then SetFocus;
  inherited;
end;

procedure TscDBImage.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture;
  inherited;
end;

procedure TscDBImage.WMCut(var Message: TMessage);
begin
  CutToClipboard;
end;

procedure TscDBImage.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TscDBImage.WMPaste(var Message: TMessage);
begin
  PasteFromClipboard;
end;

function TscDBImage.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBImage.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBAdvancedComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
  FPopupWasVisible := False;
end;

destructor TscDBAdvancedComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBAdvancedComboEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscDBAdvancedComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
     FPopupWasVisible := IsPopupVisible;
  inherited;
end;

procedure TscDBAdvancedComboEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      if not FPopupWasVisible then
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
  FPopupWasVisible := False;
end;

procedure TscDBAdvancedComboEdit.WMPaint(var Message: TWMPaint);

function IsPaintCopy: Boolean;
begin
  Result := csPaintCopy in ControlState;
end;

var
  Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  C: TColor;
  FCanvas: TCanvas;
begin
  if not IsPaintCopy or FFocused then
  begin
    inherited;
    Exit;
  end;
  FCanvas := TCanvas.Create;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      DrawEditBackground(FCanvas);
      R := ClientRect;
      if IsPaintCopy and (FDataLink.Field <> nil) and not FStopDraw then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := EditText;
      R := GetTextRect;
      case Alignment of
        taLeftJustify: Left := R.Left + 1;
        taRightJustify: Left := R.Right - TextWidth(S) - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      Brush.Style := bsClear;
      C := GetTextColor;
      Font.Color := C;
      TextRect(R, Left, R.Top + 1, S);
    end;
  finally
    FCanvas.Handle := 0;
    FCanvas.Free;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TscDBAdvancedComboEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBAdvancedComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBAdvancedComboEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscDBAdvancedComboEdit.Change;
begin
  inherited;
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
    end;
  FInChange := False;
end;

function TscDBAdvancedComboEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBAdvancedComboEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBAdvancedComboEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBAdvancedComboEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBAdvancedComboEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBAdvancedComboEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBAdvancedComboEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBAdvancedComboEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    Text := FDataLink.Field.Text;
  FInDataChange := False;
end;

procedure TscDBAdvancedComboEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscDBAdvancedComboEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify
  then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and
      not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscDBAdvancedComboEdit.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TscDBAdvancedComboEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBAdvancedComboEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBAdvancedComboEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscDBAdvancedComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];

  FValues := TStringList.Create;
  TStringList(FValues).OnChange := ValuesChanged;
  EnableValues := False;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBAdvancedComboBox.Destroy;
begin
  TStringList(FValues).OnChange := nil;
  FValues.Free;

  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TscDBAdvancedComboBox.GetComboText: string;
var
  I: Integer;
begin
  I := ItemIndex;
  if (I < 0) or (FEnableValues and (FValues.Count < I + 1))
  then
    Result := ''
  else
  if FEnableValues then Result := FValues[I]
  else
    Result := Items[I].Caption;
end;

procedure TscDBAdvancedComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
    begin
      FEnableValues := Value;
      DataChange(Self);
    end;
end;

procedure TscDBAdvancedComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

procedure TscDBAdvancedComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

procedure TscDBAdvancedComboBox.SetComboText(const Value: string);
var
  I: Integer;
begin
  if Value = '' then I := -1 else
  begin
    if FEnableValues then
      I := Values.IndexOf(Value)
    else
      I := IndexOfCaption(Value);
  end;
  ItemIndex := I;
end;

procedure TscDBAdvancedComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBAdvancedComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBAdvancedComboBox.Change;
begin
  inherited;
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
    end;
  FInChange := False;
end;

function TscDBAdvancedComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBAdvancedComboBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBAdvancedComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBAdvancedComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBAdvancedComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBAdvancedComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBAdvancedComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBAdvancedComboBox.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  begin
    if Self.IsPopupVisible then Exit;
    if FDataLink.Field <> nil then
      ComboText := FDataLink.Field.Text
    else
    if csDesigning in ComponentState then
      ComboText := Name
    else
      ComboText := '';
  end;
  FInDataChange := False;
end;

procedure TscDBAdvancedComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetComboText;
end;

procedure TscDBAdvancedComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TscDBAdvancedComboBox.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
end;

procedure TscDBAdvancedComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBAdvancedComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBAdvancedComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


constructor TscDBAdvancedListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscDBAdvancedListBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TscDBAdvancedListBox.GetListText: string;
var
  I: Integer;
begin
  I := ItemIndex;
  if I < 0 then Result := '' else Result := Items[I].Caption;
end;

procedure TscDBAdvancedListBox.SetListText(const Value: string);
var
  I: Integer;
begin
  if Value = '' then I := -1 else I := IndexOfCaption(Value);
  ItemIndex := I;
end;

procedure TscDBAdvancedListBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscDBAdvancedListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBAdvancedListBox.Change;
begin
  inherited;
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     not ReadOnly and FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
    end;
  FInChange := False;
end;

function TscDBAdvancedListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBAdvancedListBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBAdvancedListBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBAdvancedListBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBAdvancedListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscDBAdvancedListBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscDBAdvancedListBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBAdvancedListBox.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  begin
    if FDataLink.Field <> nil then
      SetListText(FDataLink.Field.Text)
    else
    if csDesigning in ComponentState then
      SetListText(Name)
    else
      SetListText('');
  end;
  FInDataChange := False;
end;

procedure TscDBAdvancedListBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetListText;
end;

procedure TscDBAdvancedListBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TscDBAdvancedListBox.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
end;

procedure TscDBAdvancedListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscDBAdvancedListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBAdvancedListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscNavButton.Create(AOwner: TComponent);
begin
  inherited;
  FToolPushButtonStyle := True;
  if AOwner is TscDBNavigator then
    FNavigator := TscDBNavigator(AOwner)
  else
    FNavigator := nil;
  CanFocused := False;
end;

procedure TscNavButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
const
  NormalButtons: array[TNavigateBtn] of TThemedDataNavButtons = (tdnbFirstNormal,
    tdnbPriorNormal, tdnbNextNormal, tdnbLastNormal, tdnbInsertNormal,
    tdnbDeleteNormal, tdnbEditNormal, tdnbPostNormal, tdnbCancelNormal,
    tdnbRefreshNormal, tdnbApplyUpdatesNormal, tdnbCancelUpdatesNormal);
  HotButtons: array[TNavigateBtn] of TThemedDataNavButtons = (tdnbFirstHot,
    tdnbPriorHot, tdnbNextHot, tdnbLastHot, tdnbInsertHot,
    tdnbDeleteHot, tdnbEditHot, tdnbPostHot, tdnbCancelHot,
    tdnbRefreshHot, tdnbApplyUpdatesHot, tdnbCancelUpdatesHot);
  DisabledButtons: array[TNavigateBtn] of TThemedDataNavButtons = (tdnbFirstDisabled,
    tdnbPriorDisabled, tdnbNextDisabled, tdnbLastDisabled, tdnbInsertDisabled,
    tdnbDeleteDisabled, tdnbEditDisabled, tdnbPostDisabled, tdnbCancelDisabled,
    tdnbRefreshDisabled, tdnbApplyUpdatesDisabled, tdnbCancelUpdatesDisabled);
  PressedButtons: array[TNavigateBtn] of TThemedDataNavButtons = (tdnbFirstPressed,
    tdnbPriorPressed, tdnbNextPressed, tdnbLastPressed, tdnbInsertPressed,
    tdnbDeletePressed, tdnbEditPressed, tdnbPostPressed, tdnbCancelPressed,
    tdnbRefreshPressed, tdnbApplyUpdatesPressed, tdnbCancelUpdatesPressed);

var
  R, R1: TRect;
  LButton: TThemedDataNavButtons;
  I: Integer;
  Buffer: TBitmap;
  IX, IY: Integer;
begin
  inherited;
  R := Bounds(0, 0, Width, Height);

  if IsCustomStyle then
  begin
    if ACtrlState = scsDisabled then
      LButton := DisabledButtons[Index]
    else if ACtrlState = scsPressed then
      LButton := PressedButtons[Index]
    else if ACtrlState = scsHot then
      LButton := HotButtons[Index]
    else
      LButton := NormalButtons[Index];
    if (FScaleFactor = 1) or not SC_SCALESTYLES then
    begin
      StyleServices.DrawElement(ACanvas.Handle,
        StyleServices.GetElementDetails(LButton), R);
    end
    else
    begin
      R1 := Rect(0, 0, 31, 31);
      IX := R.Left + R.Width div 2 - Round(R1.Width * FScaleFactor) div 2;
      IY := R.Top + R.Height div 2 - Round(R1.Height * FScaleFactor) div 2;
      Buffer := TBitmap.Create;
      Buffer.PixelFormat := pf32bit;
      Buffer.Width := R1.Width;
      Buffer.Height := R1.Height;
      Bitmap_ClearAlpha(Buffer, 0);
      try
        Buffer.AlphaFormat := afPremultiplied;
        StyleServices.DrawElement(Buffer.Canvas.Handle,
           StyleServices.GetElementDetails(LButton),
           Rect(0, 0, Buffer.Width, Buffer.Height));
        Bitmap_DrawScaleAlpha_XY(Buffer, ACanvas,
          IX, IY, 255, FScaleFactor);
      finally
        Buffer.Free;
      end;
    end;
  end
  else
  if (FNavigator <> nil) and not FNavigator.FLoadingImages then
  begin
    I := Ord(Index);
    Inc(R.Top);
    if not Self.Enabled then
      FNavigator.FResImages.DrawBitmap(ACanvas, R, I, 125)
    else
      FNavigator.FResImages.DrawBitmap(ACanvas, R, I);
  end;

  if (FNavigator <> nil) and (Parent <> nil) and (GetFocus = Parent.Handle) and
     (FIndex = FNavigator.FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    scDrawFocusRect(ACanvas, R, FScaleFactor);
  end;
end;

constructor TscNavDataLink.Create(ANav: TscDBNavigator);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;

destructor TscNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TscNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TscNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TscNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

{ TscDBNavigator }
var
  BtnTypeName: array[TNavigateBtn] of PChar = ('FIRST', 'PRIOR', 'NEXT',
    'LAST', 'INSERT', 'DELETE', 'EDIT', 'POST', 'CANCEL', 'REFRESH', 'APPLYUPDATES',
    'CANCELUPDATES');

  BtnHintId: array[TNavigateBtn] of String = (SFirstRecord, SPriorRecord,
    SNextRecord, SLastRecord, SInsertRecord, SDeleteRecord, SEditRecord,
    SPostEdit, SCancelEdit, SRefreshRecord, SApplyUpdates, SCancelUpdates);

constructor TscDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption, csGestures];
  FResImages := TscImageCollection.Create(Self);
  InitResImages;
  FDataLink := TscNavDataLink.Create(Self);
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  InitButtons;
  InitHints;
  MaxErrors := -1;
  Kind := dbnHorizontal;
  Width := 241;
  Height := 25;
  ButtonWidth := 0;
  ButtonHeight := 0;
  FocusedButton := nbFirst;
  FConfirmDelete := True;
end;

destructor TscDBNavigator.Destroy;
begin
  FResImages.Free;
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBNavigator.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
  end;
end;

procedure TscDBNavigator.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  InitResImages;
end;

procedure TscDBNavigator.InitResImages;
var
  Item: TscImageCollectionItem;
  Scale: String;
  I: TNavigateBtn;
  ScaleF: Double;
  ResName: String;
begin
  if FResImages = nil then Exit;
  FLoadingImages := True;
  FResImages.Images.Clear;

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

  for I := Low(Buttons) to High(Buttons) do
  begin
    FmtStr(ResName, 'SCDBN_%s', [BtnTypeName[I]]);
    Item := FResImages.Images.Add;
    Item.LoadPngFromResourceName(HInstance, ResName + Scale, ScaleF);
    Item.DrawStyle := idsCenter;
    Item.CheckBitmapOptions;
  end;

  FLoadingImages := False;
end;

procedure TscDBNavigator.InitButtons;
var
  I: TNavigateBtn;
  Btn: TscNavButton;
  X, Y: Integer;
begin
  MinBtnSize := Point(20, 18);
  X := 0;
  Y := 0;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Btn := TscNavButton.Create(Self);
    if FFlat then
      Btn.StyleKind := scbsToolButtonTransparent
    else
      Btn.StyleKind := scbsPushButton;
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.Animation := FAnimation;
    Btn.Enabled := True;
    Btn.SetBounds (X, Y, MinBtnSize.X, MinBtnSize.Y);
    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Buttons[I] := Btn;
    if Kind = dbnHorizontal then
      X := X + MinBtnSize.X
    else
      Y := Y + MinBtnSize.Y;
  end;
  Buttons[nbPrior].RepeatClick := True;
  Buttons[nbPrior].RepeatClickInterval := 100;
  Buttons[nbNext].RepeatClick := True;
  Buttons[nbNext].RepeatClickInterval := 100;
end;

procedure TscDBNavigator.InitHints;
var
  I: Integer;
  J: TNavigateBtn;
begin
  if not Assigned(FDefHints) then
  begin
    FDefHints := TStringList.Create;
    for J := Low(Buttons) to High(Buttons) do
      FDefHints.Add(BtnHintId[J]);
  end;
  for J := Low(Buttons) to High(Buttons) do
    Buttons[J].Hint := FDefHints[Ord(J)];
  J := Low(Buttons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then Buttons[J].Hint := FHints.Strings[I];
    if J = High(Buttons) then Exit;
    Inc(J);
  end;
end;

procedure TscDBNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TscDBNavigator.SetAnimation(Value: Boolean);
var
  I: TNavigateBtn;
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;
    for I := Low(Buttons) to High(Buttons) do
      Buttons[I].Animation := FAnimation;
  end;
end;

procedure TscDBNavigator.SetFlat(Value: Boolean);
var
  I: TNavigateBtn;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for I := Low(Buttons) to High(Buttons) do
    begin
      if FFlat then
        Buttons[I].StyleKind := scbsToolButtonTransparent
      else
        Buttons[I].StyleKind := scbsPushButton;
    end;
  end;
end;

procedure TscDBNavigator.SetHints(Value: TStrings);
begin
  if Value.Text = FDefHints.Text then
    FHints.Clear else
    FHints.Assign(Value);
end;

function TscDBNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefHints else
    Result := FHints;
end;

procedure TscDBNavigator.SetKind(Value: TDBNavigatorKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width);
    Invalidate;
  end;
end;

procedure TscDBNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TscDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscDBNavigator.SetVisible(Value: TNavButtonSet);
var
  I: TNavigateBtn;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;
  for I := Low(Buttons) to High(Buttons) do
    Buttons[I].Visible := I in FVisibleButtons;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  Invalidate;
end;

procedure TscDBNavigator.CalcMinSize(var W, H: Integer);
var
  Count: Integer;
  I: TNavigateBtn;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);
  if Kind = dbnHorizontal then
  begin
    W := Max(W, Count * MinBtnSize.X);
    H := Max(H, MinBtnSize.Y);
    if Align = alNone then W := (W div Count) * Count;
  end
  else
  begin
    W := Max(W, MinBtnSize.X);
    H := Max(H, Count * MinBtnSize.Y);
    if Align = alNone then H := (H div Count) * Count;
  end;
end;

procedure TscDBNavigator.SetSize(var W: Integer; var H: Integer);
var
  Count: Integer;
  I: TNavigateBtn;
  Space, Temp, Remain: Integer;
  X, Y: Integer;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;

  CalcMinSize(W, H);

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);

  if Kind = dbnHorizontal then
  begin
    ButtonWidth := W div Count;
    ButtonHeight := H;
    Temp := Count * ButtonWidth;
    if Align = alNone then W := Temp;
    Remain := W - Temp;
  end
  else
  begin
    ButtonWidth := W;
    ButtonHeight := H div Count;
    Temp := Count * ButtonHeight;
    if Align = alNone then H := Temp;
    Remain := H - Temp;
  end;

  X := 0;
  Y := 0;
  Temp := Count div 2;

  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec(Temp, Remain);
        if Temp < 0 then
        begin
          Inc(Temp, Count);
          Space := 1;
        end;
      end;
      if Kind = dbnHorizontal then
      begin
        Buttons[I].SetBounds(X, Y, ButtonWidth + Space, Height);
        Inc(X, ButtonWidth + Space);
      end
      else
      begin
        Buttons[I].SetBounds(X, Y, ButtonWidth, ButtonHeight + Space);
        Inc(Y, ButtonHeight + Space);
      end;
    end
    else
      if Kind = dbnHorizontal then
        Buttons[I].SetBounds(Width + 1, 0, ButtonWidth, Height)
      else
        Buttons[I].SetBounds(0, Height + 1, ButtonWidth, ButtonHeight);
  end;
end;

procedure TscDBNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then SetSize(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TscDBNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

procedure TscDBNavigator.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if (SWP_NOSIZE and Message.WindowPos.Flags) = 0 then
    CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;

procedure TscDBNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick (TscNavButton (Sender).Index);
end;

procedure TscDBNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TscNavButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    Buttons[OldFocus].RePaintControl;
    Buttons[FocusedButton].RePaintControl;
  end;
end;

procedure TscDBNavigator.ApplyUpdates;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Intf.ExecuteCommand(sApplyUpdatesDataSetCommand, [MaxErrors])
end;

function TscDBNavigator.CanApplyUpdates: Boolean;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Result := dcEnabled in Intf.GetCommandStates(sApplyUpdatesDataSetCommand)
  else
    Result := False;
end;

procedure TscDBNavigator.CancelUpdates;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Intf.ExecuteCommand(sCancelUpdatesDataSetCommand, [MaxErrors])
end;

function TscDBNavigator.CanCancelUpdates: Boolean;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Result := dcEnabled in Intf.GetCommandStates(sCancelUpdatesDataSetCommand)
  else
    Result := False;
end;

procedure TscDBNavigator.BtnClick(Index: TNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
        nbPrior: Prior;
        nbNext: Next;
        nbFirst: First;
        nbLast: Last;
        nbInsert: Insert;
        nbEdit: Edit;
        nbCancel: Cancel;
        nbPost: Post;
        nbRefresh: Refresh;
        nbDelete:
          if not FConfirmDelete or
            (scMessageDlg(SDeleteRecordQuestion, mtConfirmation,
            mbOKCancel, 0) <> idCancel) then Delete;
        nbApplyUpdates: Self.ApplyUpdates;
        nbCancelUpdates: Self.CancelUpdates;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

procedure TscDBNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TscDBNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TscDBNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TNavigateBtn;
  OldFocus: TNavigateBtn;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        if OldFocus < High(Buttons) then
        begin
          NewFocus := OldFocus;
          repeat
            NewFocus := Succ(NewFocus);
          until (NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible);
          if Buttons[NewFocus].Visible then
          begin
            FocusedButton := NewFocus;
            Buttons[OldFocus].RePaintControl;
            Buttons[NewFocus].RePaintControl;
          end;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(Buttons) then
            NewFocus := Pred(NewFocus);
        until (NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].RePaintControl;
          Buttons[FocusedButton].RePaintControl;
        end;
      end;
    VK_SPACE:
      begin
        if Buttons[FocusedButton].Enabled then
          Buttons[FocusedButton].ButtonClick;
      end;
  end;
end;

procedure TscDBNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TscDBNavigator.DataChanged;
var
  UpEnable, DnEnable: Boolean;
  CanModify{$IFDEF VER270_UP}, CanRefresh{$ENDIF}: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  {$IFDEF VER270_UP}
  CanRefresh := Enabled and FDataLink.Active and FDataLink.DataSet.CanRefresh;
  {$ENDIF}
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  Buttons[nbFirst].Enabled := UpEnable;
  Buttons[nbPrior].Enabled := UpEnable;
  Buttons[nbNext].Enabled := DnEnable;
  Buttons[nbLast].Enabled := DnEnable;
  Buttons[nbDelete].Enabled := CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  {$IFDEF VER270_UP}
  Buttons[nbRefresh].Enabled := CanRefresh;
  {$ENDIF}
  Buttons[nbApplyUpdates].Enabled := CanModify and Self.CanApplyUpdates;
  Buttons[nbCancelUpdates].Enabled := CanModify and Self.CanCancelUpdates;
end;

procedure TscDBNavigator.EditingChanged;
var
  CanModify{$IFDEF VER270_UP}, CanRefresh{$ENDIF}: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  {$IFDEF VER270_UP}
  CanRefresh := Enabled and FDataLink.Active and FDataLink.DataSet.CanRefresh;
  {$ENDIF}
  Buttons[nbInsert].Enabled := CanModify;
  Buttons[nbEdit].Enabled := CanModify and not FDataLink.Editing;
  Buttons[nbPost].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbCancel].Enabled := CanModify and FDataLink.Editing;
  {$IFDEF VER270_UP}
  Buttons[nbRefresh].Enabled := CanRefresh;
  {$ELSE}
  Buttons[nbRefresh].Enabled := CanModify;
  {$ENDIF}
  Buttons[nbApplyUpdates].Enabled := CanModify and Self.CanApplyUpdates;
  Buttons[nbCancelUpdates].Enabled := CanModify and Self.CanCancelUpdates;
end;

procedure TscDBNavigator.ActiveChanged;
var
  I: TNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
    for I := Low(Buttons) to High(Buttons) do
      Buttons[I].Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TscDBNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TscDBNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBNavigator.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  InitHints;
  ActiveChanged;
end;


{ TscDataSourceLink }

constructor TscDataSourceLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TscDataSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.UpdateDataFields;
end;

procedure TscDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FDBLookupControl.Field) and
    (FDBLookupControl <> nil) and FDBLookupControl.CanFocus then
  begin
    Field^ := nil;
    FDBLookupControl.SetFocus;
  end;
end;

procedure TscDataSourceLink.LayoutChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.UpdateDataFields;
end;

procedure TscDataSourceLink.RecordChanged(Field: TField);
begin
  if FDBLookupControl <> nil then FDBLookupControl.DataLinkRecordChanged(Field);
end;

{ TscListSourceLink }

constructor TscListSourceLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TscListSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.UpdateListFields;
end;

procedure TscListSourceLink.DataSetChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.ListLinkDataChanged;
end;

procedure TscListSourceLink.LayoutChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.UpdateListFields;
end;

{ TscDBLookupControl }

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

var
  SearchTickCount: Integer = 0;

constructor TscDBLookupControl.Create(AOwner: TComponent);
const
  LookupStyle = [csOpaque, csNeedsBorderPaint];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := LookupStyle
  else
    ControlStyle := LookupStyle + [csFramed];
  ParentColor := False;
  TabStop := True;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TscDataSourceLink.Create;
  FDataLink.FDBLookupControl := Self;
  FListLink := TscListSourceLink.Create;
  FListLink.FDBLookupControl := Self;
  FListFields := TList.Create;
  FKeyValue := Null;
end;

destructor TscDBLookupControl.Destroy;
begin
  inherited Destroy;
  FListFields.Free;
  FListFields := nil;
  if FListLink <> nil then
    FListLink.FDBLookupControl := nil;
  FListLink.Free;
  FListLink := nil;
  if FDataLink <> nil then
    FDataLink.FDBLookupControl := nil;
  FDataLink.Free;
  FDataLink := nil;
end;

function TscDBLookupControl.CanModify: Boolean;
begin
  Result := FListActive and not ReadOnly and ((FDataLink.DataSource = nil) or
    (FMasterField <> nil) and FMasterField.CanModify);
end;

procedure TscDBLookupControl.CheckNotCircular;
begin
  if FListLink.Active and FListLink.DataSet.IsLinkedTo(DataSource) then
    DatabaseError(SCircularDataLink);
end;

procedure TscDBLookupControl.CheckNotLookup;
begin
  if FLookupMode then DatabaseError(SPropDefByLookup);
  if FDataLink.DataSourceFixed then DatabaseError(SDataSourceFixed);
end;

procedure TscDBLookupControl.UpdateDataFields;
begin
  FDataField := nil;
  FMasterField := nil;
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := GetFieldProperty(FDataLink.DataSet, Self, FDataFieldName);
    if FDataField.FieldKind = fkLookup then
      FMasterField := GetFieldProperty(FDataLink.DataSet, Self, FDataField.KeyFields)
    else
      FMasterField := FDataField;
  end;
  SetLookupMode((FDataField <> nil) and (FDataField.FieldKind = fkLookup));
  DataLinkRecordChanged(nil);
end;

procedure TscDBLookupControl.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FMasterField) then
    if FMasterField <> nil then
      SetKeyValue(FMasterField.Value) else
      SetKeyValue(Null);
end;

function TscDBLookupControl.GetBorderSize: Integer;
var
  Params: TCreateParams;
  R: TRect;
begin
  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  Result := R.Bottom - R.Top;
end;

function TscDBLookupControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TscDBLookupControl.GetKeyFieldName: string;
begin
  if FLookupMode then Result := '' else Result := FKeyFieldName;
end;

function TscDBLookupControl.GetListSource: TDataSource;
begin
  if FLookupMode then Result := nil else Result := FListLink.DataSource;
end;

function TscDBLookupControl.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TscDBLookupControl.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

procedure TscDBLookupControl.KeyValueChanged;
begin
end;

procedure TscDBLookupControl.UpdateListFields;
var
  DataSet: TDataSet;
  ResultField: TField;

procedure GetFieldList(List: TList; const FieldNames: string);
var
  Pos: Integer;
  Field: TField;
  Len: Integer;
begin
  {$IFNDEF VER230}
  Len := FieldNames.Length;
  {$ELSE}
  Len := Length(FieldNames);
  {$ENDIF}
  Pos := 1;
  while Pos <= Len do
  begin
    Field := DataSet.FieldByName(ExtractFieldName(FieldNames, Pos));
    if Assigned(List) then List.Add(Field);
  end;
end;


begin
  FListActive := False;
  FKeyField := nil;
  FListField := nil;
  FListFields.Clear;
  if FListLink.Active and (FKeyFieldName <> '') then
  begin
    CheckNotCircular;
    DataSet := FListLink.DataSet;
    FKeyField := GetFieldProperty(DataSet, Self, FKeyFieldName);
    try
      GetFieldList(FListFields, FListFieldName);
    except
      DatabaseErrorFmt(SFieldNotFound, [Self.Name, FListFieldName]);
    end;
    if FLookupMode then
    begin
      ResultField := GetFieldProperty(DataSet, Self, FDataField.LookupResultField);
      if FListFields.IndexOf(ResultField) < 0 then
        FListFields.Insert(0, ResultField);
      FListField := ResultField;
    end else
    begin
      if FListFields.Count = 0 then FListFields.Add(FKeyField);
      if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
        FListField := FListFields[FListFieldIndex] else
        FListField := FListFields[0];
    end;
    FListActive := True;
  end;
end;

procedure TscDBLookupControl.ListLinkDataChanged;
begin
end;

function TscDBLookupControl.LocateKey: Boolean;
var
  KeySave: Variant;
begin
  Result := False;
  try
    Inc(FListDataChanging);
    try
      KeySave := FKeyValue;
      if not VarIsNull(FKeyValue) and FListLink.DataSet.Active and
        FListLink.DataSet.Locate(FKeyFieldName, FKeyValue, []) then
      begin
        Result := True;
        FKeyValue := KeySave;
      end;
    except
    end;
  finally
    Dec(FListDataChanging);
  end;
end;

procedure TscDBLookupControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
    if (FListLink <> nil) and (AComponent = ListSource) then ListSource := nil;
  end;
end;

procedure TscDBLookupControl.ProcessSearchKey(Key: Char);
var
  TickCount: Integer;
  S: string;
begin
  if (FListField <> nil) and (FListField.FieldKind in [fkData, fkInternalCalc]) and
    (FListField.DataType in [ftString, ftWideString]) then
    case Key of
      #8, #27: SearchText := '';
      #32..High(Char):
        if CanModify then
        begin
          TickCount := GetTickCount;
          if TickCount - SearchTickCount > 2000 then SearchText := '';
          SearchTickCount := TickCount;
          if Length(SearchText) < 32 then
          begin
            S := SearchText + Key;
            try
              if FListLink.DataSet.Locate(FListField.FieldName, S,
                [loCaseInsensitive, loPartialKey]) then
              begin
                SelectKeyValue(FKeyField.Value);
                SearchText := S;
              end;
            except
              { If you attempt to search for a string larger than what the field
                can hold, and exception will be raised.  Just trap it and
                reset the SearchText back to the old value. }
              SearchText := S;
            end;
          end;
        end;
    end;
end;

procedure TscDBLookupControl.SelectKeyValue(const Value: Variant);
begin
  if FMasterField <> nil then
  begin
    if FDataLink.Edit then
      FMasterField.Value := Value;
  end else
    SetKeyValue(Value);
  Repaint;
  Click;
end;

procedure TscDBLookupControl.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
  begin
    FDataFieldName := Value;
    UpdateDataFields;
  end;
end;

procedure TscDBLookupControl.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TscDBLookupControl.SetKeyFieldName(const Value: string);
begin
  CheckNotLookup;
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TscDBLookupControl.SetKeyValue(const Value: Variant);
begin
  if not VarEquals(FKeyValue, Value) then
  begin
    FKeyValue := Value;
    KeyValueChanged;
  end;
end;

procedure TscDBLookupControl.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TscDBLookupControl.SetListSource(Value: TDataSource);
begin
  CheckNotLookup;
  FListLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TscDBLookupControl.SetLookupMode(Value: Boolean);
begin
  if FLookupMode <> Value then
    if Value then
    begin
      FMasterField := GetFieldProperty(FDataField.DataSet, Self, FDataField.KeyFields);
      FLookupSource.DataSet := FDataField.LookupDataSet;
      FKeyFieldName := FDataField.LookupKeyFields;
      FLookupMode := True;
      FListLink.DataSource := FLookupSource;
    end else
    begin
      FListLink.DataSource := nil;
      FLookupMode := False;
      FKeyFieldName := '';
      FLookupSource.DataSet := nil;
      FMasterField := FDataField;
    end;
end;

procedure TscDBLookupControl.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TscDBLookupControl.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TscDBLookupControl.WMKillFocus(var Message: TMessage);
begin
  FHasFocus := False;
  inherited;
  Invalidate;
end;

procedure TscDBLookupControl.WMSetFocus(var Message: TMessage);
begin
  SearchText := '';
  FHasFocus := True;
  inherited;
  Invalidate;
end;

procedure TscDBLookupControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TscDBLookupControl.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscDBLookupControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBLookupControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TscDBLookupControl.WMKeyDown(var Message: TWMKeyDown);
begin
  if (FNullValueKey <> 0) and CanModify and (FNullValueKey = ShortCut(Message.CharCode,
     KeyDataToShiftState(Message.KeyData))) then
  begin
    SelectKeyValue(Null);
    Message.CharCode := 0;
  end;
  inherited;
end;

constructor TscDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csDoubleClicks];
  Width := 121;
  FBorderStyle := bsSingle;
  RowCount := 7;
  FRowCountFixed := 0;
  {$IFNDEF VER330_UP}
  FScaleFactor := 1;
  FScalePercent := 100;
  {$ENDIF}
  FSelectionColor := clHighLight;
  FSelectionTextColor := clHighLightText;
  FSelectionStyle := scstStyled;
  FShowFocusRect := True;
  FLineColor := clBtnFace;
end;

procedure TscDBLookupListBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  {$IFNDEF VER330_UP}
  FScalePercent := MulDiv(FScalePercent, M, D);
  FScaleFactor := FScalePercent / 100;
  if FScaleFactor < 1 then FScaleFactor := 1;
  {$ENDIF}
end;

procedure TscDBLookupListBox.SetRowCountFixed(Value: Integer);
begin
  if (Value >= 0) and (FRowCountFixed <> Value) then
  begin
    FRowCountFixed := Value;
    SetBounds(Left, Top, Width, Height);
  end;
end;

function TscDBLookupListBox.GetTextHeight: Integer;
begin
  Result := inherited GetTextHeight;
  Inc(Result, 2);
end;

procedure TscDBLookupListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TscDBLookupListBox.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

procedure TscDBLookupListBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscDBLookupListBox.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TscDBLookupListBox.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

procedure TscDBLookupListBox.Paint;
var
  I, J, W, X, TextWidth, TextHeight, LastFieldIndex: Integer;
  S: string;
  R: TRect;
  Selected: Boolean;
  Field: TField;
  AAlignment: TAlignment;
  Buffer: TBitmap;
  C, BGColor: TColor;
begin
  R := ClientRect;
  Buffer := TBitmap.Create;
  Buffer.Width := R.Width;
  Buffer.Height := R.Height;
  Canvas.Font := Font;
  Buffer.Canvas.Font := Font;
  TextWidth := Canvas.TextWidth('0');
  TextHeight := Self.GetTextHeight;
  LastFieldIndex := ListFields.Count - 1;
  // fill client
  if {$IFNDEF VER230}(seClient in StyleElements) and {$ENDIF} TStyleManager.IsCustomStyleActive then
    BGColor :=  GetStyleColor(Self.Color)
  else
    BGColor := Self.Color;
  Buffer.Canvas.Brush.Color := BGColor;
  Buffer.Canvas.Brush.Style := bsSolid;
  Buffer.Canvas.FillRect(Rect(0, 0, Buffer.Width, Buffer.Height));
  // draw items
  for I := 0 to RowCount - 1 do
  begin
    Selected := not FKeySelected and (I = 0);
    R.Top := I * TextHeight;
    R.Bottom := R.Top + TextHeight;
    R.Right := Buffer.Width;
    R.Left := 0;
    if I < FRecordCount then
    begin
      ListLink.ActiveRecord := I;
      if not VarIsNull(FKeyValue) and
        VarEquals(FKeyField.Value, KeyValue) then
        Selected := True;
        // draw item
         with Buffer.Canvas do
         begin
           if (FSelectionStyle = scstColor) and Selected then
           begin
             if FSelectionColor <> clNone then
             begin
               Font.Color := GetStyleColor(FSelectionTextColor);
               Brush.Color := GetStyleColor(FSelectionColor);
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
        if (SelectionStyle = scstColor) and Selected
           and not Focused and not FShowFocusRect then
        begin
          C := Brush.Color;
          Brush.Color := BGColor;
          FillRect(R);
          Brush.Color := C;
          FillRectWithAlpha(Buffer.Canvas, R, 200);
        end
        else
          FillRect(R);
      end;
      if Selected then
      begin
        if FSelectionStyle = scstStyled then
        begin
          Buffer.Canvas.Font.Color := GetSelectionTextColor;
          DrawSelection(Buffer.Canvas, R, Self.Focused, FShowFocusRect);
        end;
      end;
      R.Right := 0;
      for J := 0 to LastFieldIndex do
      begin
        Field := ListFields[J];
        if J < LastFieldIndex then
          W := Field.DisplayWidth * TextWidth + 4 else
          W := ClientWidth - R.Right;
        S := Field.DisplayText;
        X := 2;
        AAlignment := Field.Alignment;
        if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
        case AAlignment of
          taRightJustify: X := W - Buffer.Canvas.TextWidth(S) - 3;
          taCenter: X := (W - Buffer.Canvas.TextWidth(S)) div 2;
        end;
        R.Left := R.Right;
        R.Right := R.Right + W;
        if SysLocale.MiddleEast then
          TControlCanvas(Canvas).UpdateTextFlags;
      // draw text
      Buffer.Canvas.Brush.Style := bsClear;
      Buffer.Canvas.TextRect(R, R.Left + X, R.Top, S);
      //
      if J < LastFieldIndex then
      begin
        C := GetStyleColor(FLineColor);
        if ColorToRGB(C) = ColorToRGB(BGColor) then
          C :=  GetStyleColor(clBtnShadow);
        Buffer.Canvas.Pen.Color := C;
        Buffer.Canvas.MoveTo(R.Right, R.Top);
        Buffer.Canvas.LineTo(R.Right, R.Bottom);
        Inc(R.Right);
        if R.Right >= ClientWidth then Break;
       end;
      end;
    end;
    R.Left := 0;
    R.Right := ClientWidth;
    // draw focus
    if Selected and HasFocus and ShowFocusRect then
    begin
      if StyleServices.Enabled and not IsWindowsXP and not IsCustomStyle
         and (FSelectionStyle = scstStyled)
      then
        InflateRect(R, -1, -1);
      scDrawFocusRect(Buffer.Canvas, R, FScaleFactor);
    end;
  end;
  Canvas.Draw(0, 0, Buffer);
  Buffer.Free;
  if FRecordCount <> 0 then ListLink.ActiveRecord := FRecordIndex;
end;

procedure TscDBLookupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
end;

procedure TscDBLookupListBox.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;

function TscDBLookupListBox.GetKeyIndex: Integer;
var
  FieldValue: Variant;
begin
  if not VarIsNull(FKeyValue) then
    for Result := 0 to FRecordCount - 1 do
    begin
      ListLink.ActiveRecord := Result;
      FieldValue := FKeyField.Value;
      ListLink.ActiveRecord := FRecordIndex;
      if VarEquals(FieldValue, FKeyValue) then Exit;
    end;
  Result := -1;
end;

procedure TscDBLookupListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta, KeyIndex: Integer;
begin
  inherited KeyDown(Key, Shift);
  if CanModify then
  begin
    Delta := 0;
    case Key of
      VK_UP, VK_LEFT: Delta := -1;
      VK_DOWN, VK_RIGHT: Delta := 1;
      VK_PRIOR: Delta := 1 - FRowCount;
      VK_NEXT: Delta := FRowCount - 1;
      VK_HOME: Delta := -Maxint;
      VK_END: Delta := Maxint;
    end;
    if Delta <> 0 then
    begin
      SearchText := '';
      if Delta = -Maxint then ListLink.DataSet.First else
        if Delta = Maxint then ListLink.DataSet.Last else
        begin
          KeyIndex := GetKeyIndex;
          if KeyIndex >= 0 then
            ListLink.DataSet.MoveBy(KeyIndex - FRecordIndex)
          else
          begin
            KeyValueChanged;
            Delta := 0;
          end;
          ListLink.DataSet.MoveBy(Delta);
        end;
      SelectCurrent;
    end;
  end;
end;

procedure TscDBLookupListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ProcessSearchKey(Key);
end;

procedure TscDBLookupListBox.KeyValueChanged;
begin
  if ListActive and not FLockPosition then
    if not LocateKey then ListLink.DataSet.First;
  if FListField <> nil then
    FSelectedItem := FListField.DisplayText else
    FSelectedItem := '';
end;

procedure TscDBLookupListBox.UpdateListFields;
begin
  try
    inherited;
  finally
    if ListActive then KeyValueChanged else ListLinkDataChanged;
  end;
end;

procedure TscDBLookupListBox.ListLinkDataChanged;
begin
  if ListActive then
  begin
    FRecordIndex := ListLink.ActiveRecord;
    FRecordCount := ListLink.RecordCount;
    FKeySelected := not VarIsNull(FKeyValue) or
      not ListLink.DataSet.BOF;
  end else
  begin
    FRecordIndex := 0;
    FRecordCount := 0;
    FKeySelected := False;
  end;
  if HandleAllocated then
  begin
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TscDBLookupListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SearchText := '';
    if not FPopup then
    begin
      SetFocus;
      if not HasFocus then Exit;
    end;
    if CanModify then
      if ssDouble in Shift then
      begin
        if FRecordIndex = Y div GetTextHeight then DblClick;
      end else
      begin
        MouseCapture := True;
        FTracking := True;
        SelectItemAt(X, Y);
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TscDBLookupListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
  begin
    SelectItemAt(X, Y);
    FMousePos := Y;
    TimerScroll;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TscDBLookupListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FTracking then
  begin
    StopTracking;
    SelectItemAt(X, Y);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;


procedure TscDBLookupListBox.SelectCurrent;
begin
  FLockPosition := True;
  try
    SelectKeyValue(FKeyField.Value);
  finally
    FLockPosition := False;
  end;
end;

procedure TscDBLookupListBox.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  Inc(FListDataChanging);
  try
    if Y < 0 then Y := 0;
    if Y >= ClientHeight then Y := ClientHeight - 1;
    Delta := Y div GetTextHeight - FRecordIndex;
    ListLink.DataSet.MoveBy(Delta);
    SelectCurrent;
  finally
    Dec(FListDataChanging);
  end;
end;

procedure TscDBLookupListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
    RowCount := RowCount;
  end;
end;

procedure TscDBLookupListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  BorderSize, TextHeight, Rows: Integer;
begin
  BorderSize := GetBorderSize;
  TextHeight := GetTextHeight;
  if FRowCountFixed > 0 then
    AHeight := FRowCountFixed * TextHeight + BorderSize;
  Rows := (AHeight - BorderSize) div TextHeight;
  if Rows < 1 then Rows := 1;
  FRowCount := Rows;
  if ListLink.BufferCount <> Rows then
  begin
    ListLink.BufferCount := Rows;
    ListLinkDataChanged;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, Rows * TextHeight + BorderSize);
end;

function TscDBLookupListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscDBLookupListBox.SetRowCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;
  Height := Value * GetTextHeight + GetBorderSize;
end;

procedure TscDBLookupListBox.StopTimer;
begin
  if FTimerActive then
  begin
    KillTimer(Handle, 1);
    FTimerActive := False;
  end;
end;

procedure TscDBLookupListBox.StopTracking;
begin
  if FTracking then
  begin
    StopTimer;
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TscDBLookupListBox.TimerScroll;
var
  Delta, Distance, Interval: Integer;
begin
  Delta := 0;
  Distance := 0;
  if FMousePos < 0 then
  begin
    Delta := -1;
    Distance := -FMousePos;
  end;
  if FMousePos >= ClientHeight then
  begin
    Delta := 1;
    Distance := FMousePos - ClientHeight + 1;
  end;
  if Delta = 0 then StopTimer else
  begin
    Inc(FListDataChanging);
    try
      if ListLink.DataSet.MoveBy(Delta) <> 0 then SelectCurrent;
      Interval := 200 - Distance * 15;
      if Interval < 0 then Interval := 0;
      SetTimer(Handle, 1, Interval, nil);
      FTimerActive := True;
    finally
      Dec(FListDataChanging);
    end;
  end;
end;

procedure TscDBLookupListBox.UpdateScrollBar;
var
  Pos, Max: Integer;
  ScrollInfo: TScrollInfo;
begin
  Pos := 0;
  Max := 0;
  if FRecordCount = FRowCount then
  begin
    Max := 4;
    if not ListLink.DataSet.BOF then
      if not ListLink.DataSet.EOF then Pos := 2 else Pos := 4;
  end;
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_POS or SIF_RANGE;
  if not GetScrollInfo(Handle, SB_VERT, ScrollInfo) or
    (ScrollInfo.nPos <> Pos) or (ScrollInfo.nMax <> Max) then
  begin
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := Max;
    ScrollInfo.nPos := Pos;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;

procedure TscDBLookupListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
  begin
    RecreateWnd;
    RowCount := RowCount;
  end;
  inherited;
end;

procedure TscDBLookupListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Height := Height;
end;

procedure TscDBLookupListBox.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TscDBLookupListBox.WMTimer(var Message: TMessage);
begin
  TimerScroll;
end;

procedure TscDBLookupListBox.WMVScroll(var Message: TWMVScroll);
begin
  SearchText := '';
  if ListLink.DataSet = nil then
    Exit;
  Inc(FListDataChanging);
  try
    with Message, ListLink.DataSet do
      case ScrollCode of
        SB_LINEUP: MoveBy(-FRecordIndex - 1);
        SB_LINEDOWN: MoveBy(FRecordCount - FRecordIndex);
        SB_PAGEUP: MoveBy(-FRecordIndex - FRecordCount + 1);
        SB_PAGEDOWN: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
        SB_THUMBPOSITION:
          begin
            case Pos of
              0: First;
              1: MoveBy(-FRecordIndex - FRecordCount + 1);
              2: Exit;
              3: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
              4: Last;
            end;
          end;
        SB_BOTTOM: Last;
        SB_TOP: First;
      end;
  finally
    Dec(FListDataChanging);
  end;
end;

function TscDBLookupListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBLookupListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscPopupDataList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  FPopup := True;
end;

procedure TscPopupDataList.CreateWnd;
begin
  inherited CreateWnd;
  if TStyleManager.IsCustomStyleActive then
  begin
    Winapi.Windows.SetParent(Handle, 0);
    CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TscPopupDataList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if TStyleManager.IsCustomStyleActive then
    begin
      Style := Style or WS_BORDER;
      ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    end
    else
      begin
        Style := WS_POPUP or WS_BORDER;
        ExStyle := WS_EX_TOOLWINDOW;
      end;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS or CS_DROPSHADOW;
  end;
end;

procedure TscPopupDataList.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

{ TDBLookupComboBox }

constructor TscCustomDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable] + [csOverrideStylePaint] - [csParentBackground];
  Width := 145;
  Height := 0;
  FDataList := TscPopupDataList.Create(Self);
  FDataList.Visible := False;
  FDataList.Parent := Self;
  FDataList.OnMouseUp := ListMouseUp;
  FSelectionColor := clHighLight;
  FSelectionTextColor := clHighLightText;
  FSelectionStyle := scstStyled;
  FShowFocusRect := True;
  FLineColor := clBtnFace;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FDropDownRows := 7;
end;

procedure TscCustomDBLookupComboBox.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  if FDropDownWidth > 0 then
    FDropDownWidth := MulDiv(FDropDownWidth, M, D);
end;

procedure TscCustomDBLookupComboBox.SetSelectionStyle(Value: TscSelectionStyle);
begin
  if FSelectionStyle <> Value then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

procedure TscCustomDBLookupComboBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomDBLookupComboBox.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TscCustomDBLookupComboBox.SetSelectionTextColor(Value: TColor);
begin
  if FSelectionTextColor <> Value then
  begin
    FSelectionTextColor := Value;
    Invalidate;
  end;
end;

{$IFNDEF VER230}
procedure TscCustomDBLookupComboBox.UpdateStyleElements;
begin
  Invalidate;
end;
{$ENDIF}

procedure TscCustomDBLookupComboBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TscCustomDBLookupComboBox.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    ListValue := FDataList.KeyValue;
    SetWindowPos(FDataList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);

    FListVisible := False;
    FDataList.ListSource := nil;
    Invalidate;
    SearchText := '';
    if Accept and CanModify then SelectKeyValue(ListValue);
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  end;
end;

procedure TscCustomDBLookupComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  FDataList.BiDiMode := BiDiMode;
end;

procedure TscCustomDBLookupComboBox.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Message.CharCode in [VK_RETURN, VK_ESCAPE]) and FListVisible then
  begin
    CloseUp(Message.CharCode = VK_RETURN);
    Message.Result := 1;
  end else
    inherited;
end;

procedure TscCustomDBLookupComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TscCustomDBLookupComboBox.DropDown;
var
  P: TPoint;
  I, Y: Integer;
  S: string;
  ADropDownAlign: TDropDownAlign;
begin
  Inc(FListDataChanging);
  try
    if not FListVisible and ListActive then
    begin
      if Assigned(FOnDropDown) then FOnDropDown(Self);
      FDataList.Color := Color;
      FDataList.Font := Font;
      FDataList.SelectionStyle := Self.SelectionStyle;
      FDataList.SelectionColor := Self.SelectionColor;
      FDataList.SelectionTextColor := Self.SelectionTextColor;
      FDataList.LineColor := Self.LineColor;
      {$IFNDEF VER230}
      FDataList.StyleElements := StyleElements;
      {$ENDIF}
      if FDropDownWidth > 0 then
        FDataList.Width := FDropDownWidth
      else
        FDataList.Width := Width;
      FDataList.ReadOnly := not CanModify;
      if (ListLink.DataSet.RecordCount > 0) and
         (FDropDownRows > ListLink.DataSet.RecordCount) then
        FDataList.RowCount := ListLink.DataSet.RecordCount else
        FDataList.RowCount := FDropDownRows;
      FDataList.KeyField := FKeyFieldName;
      for I := 0 to ListFields.Count - 1 do
        S := S + TField(ListFields[I]).FieldName + ';';
      FDataList.ListField := S;
      FDataList.ListFieldIndex := ListFields.IndexOf(FListField);
      FDataList.ListSource := ListLink.DataSource;
      FDataList.KeyValue := KeyValue;
      P := Parent.ClientToScreen(Point(Left, Top));
      Y := P.Y + Height;
      if Y + FDataList.Height > Screen.Height then Y := P.Y - FDataList.Height;
      ADropDownAlign := FDropDownAlign;
      { This alignment is for the ListField, not the control }
      if DBUseRightToLeftAlignment(Self, FListField) then
      begin
        if ADropDownAlign = daLeft then
          ADropDownAlign := daRight
        else if ADropDownAlign = daRight then
          ADropDownAlign := daLeft;
      end;
      case ADropDownAlign of
        daRight: Dec(P.X, FDataList.Width - Width);
        daCenter: Dec(P.X, (FDataList.Width - Width) div 2);
      end;
      FListVisible := True;
      RePaint;
      SetWindowPos(FDataList.Handle, HWND_TOP, P.X, Y, 0, 0,
        SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    end;
  finally
    Dec(FListDataChanging);
  end;
end;

procedure TscCustomDBLookupComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  Inc(FListDataChanging);
  try
    inherited KeyDown(Key, Shift);
    if ListActive and ((Key = VK_UP) or (Key = VK_DOWN)) then
      if ssAlt in Shift then
      begin
        if FListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end else
        if not FListVisible then
        begin
          if not LocateKey then
            ListLink.DataSet.First
          else
          begin
            if Key = VK_UP then Delta := -1 else Delta := 1;
            ListLink.DataSet.MoveBy(Delta);
          end;
          SelectKeyValue(FKeyField.Value);
          Key := 0;
        end;
    if (Key <> 0) and FListVisible then FDataList.KeyDown(Key, Shift);
  finally
    Dec(FListDataChanging);
  end;
end;

procedure TscCustomDBLookupComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FListVisible then
    if CharInSet(Key, [#13, #27]) then
      CloseUp(Key = #13)
    else
      FDataList.KeyPress(Key)
  else
    ProcessSearchKey(Key);
end;

procedure TscCustomDBLookupComboBox.KeyValueChanged;
begin
  if FLookupMode then
  begin
    FText := FDataField.DisplayText;
    FAlignment := FDataField.Alignment;
  end else
  if ListActive and LocateKey then
  begin
    FText := FListField.DisplayText;
    FAlignment := FListField.Alignment;
  end else
  begin
    FText := '';
    FAlignment := taLeftJustify;
  end;
  Invalidate;
end;

procedure TscCustomDBLookupComboBox.UpdateListFields;
begin
  inherited;
  KeyValueChanged;
end;

procedure TscCustomDBLookupComboBox.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(FDataList.ClientRect.Contains(Point(X, Y)));
end;

procedure TscCustomDBLookupComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocus;
    if not HasFocus then Exit;
    if FListVisible then CloseUp(False) else
      if ListActive then
      begin
        MouseCapture := True;
        FTracking := True;
        TrackButton(X, Y);
        DropDown;
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TscCustomDBLookupComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  NewState: Boolean;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FDataList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if FDataList.ClientRect.Contains(ListPos) then
      begin
        StopTracking;
        SendMessage(FDataList.Handle, WM_LBUTTONDOWN, 0, MakeLParam(ListPos.X, ListPos.Y));
        Exit;
      end;
    end;
  end
  else
  begin
    NewState := FButtonRect.Contains(Point(X, Y));
    if FMouseInButton <> NewState then
    begin
      FMouseInButton := NewState;
      Repaint;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TscCustomDBLookupComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TscCustomDBLookupComboBox.DrawCombo(ACanvas: TCanvas; ARect: TRect; AText: String; ASelected: Boolean;
   AAlignment: TAlignment);
var
  FState, FButtonState: TscsCtrlState;
  FC, BC: TColor;
  IR: TRect;
begin
  ACanvas.Font := Self.Font;
  if not Enabled then
    FState := scsDisabled
  else
  if ASelected then
    FState := scsFocused
  else
  if FMouseInControl then
    FState := scsHot
  else
    FState := scsNormal;
  if not Enabled then
    FC := clGrayText
  else
    FC := Font.Color;
  BC := GetStyleColor(clHighLight);
  if IsCustomStyle {$IFNDEF VER230} and (seFont in StyleElements){$ENDIF} then
  begin
    if not Enabled then
      FState := scsDisabled
    else
      FState := scsNormal;
    FC := GetEditTextColor(FState);
  end;
  if ASelected then
    case SelectionStyle of
      scstStyled:
        FC := GetSelectionTextColor;
      scstColor:
        if FSelectionColor <> clNone then
        begin
          FC := GetStyleColor(FSelectionTextColor);
          BC := GetStyleColor(FSelectionColor);
        end
        else
        begin
          FC := GetStyleColor(clHighLightText);
          BC := GetStyleColor(clHighLight);
        end;
    end;
  ACanvas.Brush.Color := GetStyleColor(clWindow);
  ACanvas.FillRect(ARect);
  // draw frame
  DrawEditBorder(ACanvas, ARect, FState);
  //
  FButtonRect := Rect(0, 0, Width, Height);
   if IsCustomStyle then
    InflateRect(FButtonRect, -2, -2)
  else
  if StyleServices.Enabled then
    InflateRect(FButtonRect, -1, -1)
  else
    InflateRect(FButtonRect, -2, -2);
  if BiDiMode <> bdRightToLeft then
  begin
    FButtonRect.Left := FButtonRect.Right - FButtonWidth + 1;
    IR := Rect(3, 3, FButtonRect.Left - 1 , Height - 3);
  end
  else
  begin
    FButtonRect.Right := FButtonRect.Left + FButtonWidth - 1;
    IR := Rect(FButtonRect.Right + 1, 3, Width - 3, Height - 3);
  end;
  // draw button
  if not (ListActive and Enabled) then
    FButtonState := scsDisabled
  else
   if FPressed or (Self.FListVisible) then
     FButtonState := scsPressed
   else
     if FMouseInControl and FMouseInButton then
       FButtonState := scsHot
     else
       FButtonState := scsNormal;
  DrawDropDownButton(ACanvas, FButtonRect, FButtonState, True,
    BidiMode = bdRightToLeft{$IFDEF VER330_UP}, FScaleFactor{$ENDIF});
  // draw selection
  if ASelected then
  case SelectionStyle of
    scstStyled:
    begin
      DrawSelection(ACanvas, IR, True, FShowFocusRect);
    end;
    scstColor:
    begin
      ACanvas.Brush.Color := BC;
      ACanvas.FillRect(IR);
    end;
  end;
  // draw text
  ACanvas.Font.Color := FC;
  ACanvas.Brush.Style := bsClear;
  Inc(IR.Left, 3);
  Dec(IR.Right, 3);
  scDrawUtils.DrawTextAlignmentNoPrefix(ACanvas,
    AText, IR, AAlignment, BidiMode = bdRightToLeft);
end;


procedure TscCustomDBLookupComboBox.Paint;
const
  StyleColor: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
  StyleFontColor: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
var
  Text: string;
  AAlignment: TAlignment;
  Selected: Boolean;
  R: TRect;
  Buffer: TBitmap;
begin
  Selected := HasFocus and not FListVisible and not (csPaintCopy in ControlState);
  if (csPaintCopy in ControlState) and (FDataField <> nil) and
    (FDataField.Lookup) then
  begin
    Text := FDataField.DisplayText;
    AAlignment := FDataField.Alignment;
  end else
  begin
    if (csDesigning in ComponentState) and (FDataField = nil) then
      Text := Name else
      Text := FText;
    AAlignment := FAlignment;
  end;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  R := Rect(0, 0, Width, Height);
  Buffer := TBitmap.Create;
  try
    Buffer.Width := Width;
    Buffer.Height := Height;
    DrawCombo(Buffer.Canvas, R, Text, Selected, AAlignment);
    Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TscCustomDBLookupComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, GetTextHeight + 8);
end;

function TscCustomDBLookupComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscCustomDBLookupComboBox.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TscCustomDBLookupComboBox.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
begin
  NewState := FButtonRect.Contains(Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    Repaint;
  end;
end;

procedure TscCustomDBLookupComboBox.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FDataList) then
    CloseUp(False);
end;

procedure TscCustomDBLookupComboBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then
  begin
    RecreateWnd;
    Height := 0;
  end;
  inherited;
end;

procedure TscCustomDBLookupComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Height := 0;
end;

procedure TscCustomDBLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(DataLink);
end;

procedure TscCustomDBLookupComboBox.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TscCustomDBLookupComboBox.WMSetFocus(var Message: TWMKillFocus);
begin
  inherited;
  if (TStyleManager.IsCustomStyleActive)
     {$IFNDEF VER230}and (seBorder in StyleElements) {$ENDIF}
  then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TscCustomDBLookupComboBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
  if (TStyleManager.IsCustomStyleActive)
     {$IFNDEF VER230}and (seBorder in StyleElements){$ENDIF}
  then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

function TscCustomDBLookupComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataLink <> nil) and
    DataLink.ExecuteAction(Action);
end;

function TscCustomDBLookupComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataLink <> nil) and
    DataLink.UpdateAction(Action);
end;

procedure TscCustomDBLookupComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and not (FMouseInControl) and
    not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TscCustomDBLookupComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and FMouseInControl then
  begin
    FMouseInButton := False;
    FMouseInControl := False;
    Invalidate;
  end;
end;

procedure TscCustomDBLookupComboBox.ListLinkDataChanged;
begin
  if (FDataField = nil) and ListActive and (FListDataChanging = 0) and
     (FDataList.FListDataChanging = 0) and ListLink.DataSet.BOF then
  begin
    SetKeyValue(Null);
  end;
end;


constructor TscDBToggleSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FInDataChange := False;
  FInChange := False;
  FValueCheck := 'True';
  FValueUncheck := 'False';
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TscDBToggleSwitch.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscDBToggleSwitch.ChangeState;
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TscDBToggleSwitch.SetState(Value: TscSwitchState);
begin
  FInChange := True;
  if FState <> Value then
  begin
    if not FInDataChange and (FDataLink <> nil) and
       not ReadOnly and FDataLink.CanModify and FDataLink.Edit
    then
      begin
        inherited;
        FDataLink.Modified;
      end
    else
    if not (ReadOnly and not FInDataChange) then
    begin
      FState := Value;
      RePaintControl;
    end;
  end;
  FInChange := False;
end;

procedure TscDBToggleSwitch.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscDBToggleSwitch.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TscDBToggleSwitch.GetFieldState: TscSwitchState;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := scswOff
    else if FDataLink.Field.DataType = ftBoolean then
      if FDataLink.Field.AsBoolean then
        Result := scswOn
      else
        Result := scswOff
    else
    begin
      Result := scswOff;
      Text := FDataLink.Field.Text;
      if ValueMatch(FValueCheck, Text) then Result := scswOn else
        if ValueMatch(FValueUncheck, Text) then Result := scswOff;
    end
  else
    Result := scswOff;
end;

procedure TscDBToggleSwitch.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
    State := GetFieldState;
  FInDataChange := False;
end;

procedure TscDBToggleSwitch.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
   if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean := IsOn
    else
    begin
      if IsOn then S := FValueCheck else S := FValueUncheck;
      Pos := 1;
      FDataLink.Field.Text := ExtractFieldName(S, Pos);
    end;
end;

function TscDBToggleSwitch.ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    begin
      Result := True;
      Break;
    end;
end;

function TscDBToggleSwitch.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscDBToggleSwitch.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscDBToggleSwitch.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscDBToggleSwitch.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscDBToggleSwitch.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
  FReadOnly := Result;
end;

function TscDBToggleSwitch.IsValueChecked: Boolean;
begin
  Result := not SameText(FValueCheck, 'True');
end;

function TscDBToggleSwitch.IsValueUnchecked: Boolean;
begin
  Result := not SameText(FValueUncheck, 'False');
end;

procedure TscDBToggleSwitch.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
  FReadOnly := FDataLink.ReadOnly;
end;

function TscDBToggleSwitch.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscDBToggleSwitch.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TscDBToggleSwitch.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TscDBToggleSwitch.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

procedure TscDBToggleSwitch.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscDBToggleSwitch.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscDBToggleSwitch.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscDBToggleSwitch.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


initialization

  TCustomStyleEngine.RegisterStyleHook(TscDBMemo, TscScrollingStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscDBLookupListBox, TscScrollingStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscPopupDataList, TscScrollingStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscDBComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscDBRichEdit, TscRichEditStyleHook);

finalization

  {$IFNDEF VER230}
  TCustomStyleEngine.UnRegisterStyleHook(TscDBMemo, TscScrollingStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscPopupDataList, TscScrollingStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscDBLookupListBox, TscScrollingStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscDBComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscDBRichEdit, TscRichEditStyleHook);
  {$ENDIF}

end.

