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

unit scGPDBControls;

{$I scdefine.inc}
{$R-}

interface
  uses System.Variants, Winapi.Windows, System.SysUtils, Winapi.Messages,
     Vcl.Controls, System.Classes, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.Themes,
     Vcl.Mask, Data.DB, VCl.DBCtrls, scDrawUtils, scGPControls, scGPMeters,
     scModernControls, scGPExtControls;

type
  TscGPDBCheckBox = class(TscGPCheckBox)
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

  TscGPDBProgressBar = class(TscGPProgressBar)
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

  TscGPDBCircledProgressBar = class(TscGPCircledProgressBar)
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

  TscGPDBTrackBar = class(TscGPTrackBar)
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

  TscGPDBHVMeter = class(TscGPHVMeter)
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

  TscGPDBMeter = class(TscGPMeter)
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

  TscGPDBMeter120 = class(TscGPMeter120)
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

  TscGPDBSlider = class(TscGPSlider)
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

  TscGPDBDial = class(TscGPDial)
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

  TscGPDBGearDial = class(TscGPGearDial)
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

  TscGPDBEdit = class(TscGPEdit)
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

  TscGPDBComboEdit = class(TscGPComboEdit)
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

  TscGPDBListBox = class(TscGPListBox)
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

  TscGPDBComboBox = class(TscGPComboBox)
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

  TscGPDBNumericEdit = class(TscGPNumericEdit)
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

  TscGPDBSpinEdit = class(TscGPSpinEdit)
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

  TscGPDBTimeEdit = class(TscGPTimeEdit)
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

  TscGPDBPasswordEdit = class(TscGPPasswordEdit)
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

  TscGPDBDateEdit = class(TscGPDateEdit)
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

  TscGPDBMemo = class(TscGPMemo)
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

  TscGPDBText = class(TscGPLabel)
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

  TscGPDBToggleSwitch = class(TscGPToggleSwitch)
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

  Uses System.Types, System.Math, System.UITypes, Vcl.Clipbrd,
    Vcl.Dialogs, Vcl.VDBConsts, Data.DBConsts, Vcl.Menus;


  constructor TscGPDBCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FClickOnDataChange := True;
  State := cbUnchecked;
  FInDataChange := False;
  FInChange := False;
  FValueCheck := 'True';
  FValueUncheck := 'False';
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TscGPDBCheckBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBCheckBox.SetState(Value: TCheckBoxState);
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

procedure TscGPDBCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBCheckBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TscGPDBCheckBox.GetFieldState: TCheckBoxState;
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

procedure TscGPDBCheckBox.DataChange(Sender: TObject);
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

procedure TscGPDBCheckBox.UpdateData(Sender: TObject);
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

function TscGPDBCheckBox.ValueMatch(const ValueList, Value: string): Boolean;
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

function TscGPDBCheckBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBCheckBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBCheckBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBCheckBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBCheckBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TscGPDBCheckBox.IsValueChecked: Boolean;
begin
  Result := not SameText(FValueCheck, 'True');
end;

function TscGPDBCheckBox.IsValueUnchecked: Boolean;
begin
  Result := not SameText(FValueUncheck, 'False');
end;

procedure TscGPDBCheckBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBCheckBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBCheckBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TscGPDBCheckBox.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TscGPDBCheckBox.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

procedure TscGPDBCheckBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscGPDBCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscGPDBCheckBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBCheckBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscGPDBProgressBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBProgressBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBProgressBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBProgressBar.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBProgressBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBProgressBar.GetField: TField;
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

procedure TscGPDBProgressBar.DataChange(Sender: TObject);
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

procedure TscGPDBProgressBar.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBProgressBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBProgressBar.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


constructor TscGPDBCircledProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscGPDBCircledProgressBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBCircledProgressBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBCircledProgressBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBCircledProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBCircledProgressBar.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBCircledProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBCircledProgressBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBCircledProgressBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBCircledProgressBar.DataChange(Sender: TObject);
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

procedure TscGPDBCircledProgressBar.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBCircledProgressBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBCircledProgressBar.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBTrackBar.Create(AOwner: TComponent);
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

destructor TscGPDBTrackBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBTrackBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBTrackBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBTrackBar.Change;
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

function TscGPDBTrackBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBTrackBar.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBTrackBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBTrackBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBTrackBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBTrackBar.DataChange(Sender: TObject);
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

procedure TscGPDBTrackBar.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := IntToStr(Value);
end;

procedure TscGPDBTrackBar.CMEnter;
begin
  inherited;
end;

procedure TscGPDBTrackBar.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
end;

procedure TscGPDBTrackBar.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBTrackBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBTrackBar.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBHVMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscGPDBHVMeter.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBHVMeter.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBHVMeter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBHVMeter.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBHVMeter.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBHVMeter.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBHVMeter.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBHVMeter.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBHVMeter.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') then
      begin
        case Field.DataType of
          ftSmallint, ftInteger, ftWord:
            Value := FDataLink.Field.AsInteger;
          ftFloat:
            Value := FDataLink.Field.AsFloat;
          ftSingle:
            Value := FDataLink.Field.AsSingle;
          else
            Value := MinValue;
        end;
      end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
end;

procedure TscGPDBHVMeter.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBHVMeter.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBHVMeter.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscGPDBMeter.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBMeter.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBMeter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBMeter.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBMeter.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBMeter.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBMeter.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBMeter.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBMeter.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') then
      begin
        case Field.DataType of
          ftSmallint, ftInteger, ftWord:
            Value := FDataLink.Field.AsInteger;
          ftFloat:
            Value := FDataLink.Field.AsFloat;
          ftSingle:
            Value := FDataLink.Field.AsSingle;
          else
            Value := MinValue;
        end;
      end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
end;

procedure TscGPDBMeter.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBMeter.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBMeter.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBMeter120.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FInChange := False;
  FInDataChange := False;
end;

destructor TscGPDBMeter120.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBMeter120.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBMeter120.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBMeter120.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBMeter120.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBMeter120.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBMeter120.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBMeter120.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBMeter120.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') then
      begin
        case FDataLink.Field.DataType of
          ftSmallint, ftInteger, ftWord:
            Value := FDataLink.Field.AsInteger;
          ftFloat:
            Value := FDataLink.Field.AsFloat;
          ftSingle:
            Value := FDataLink.Field.AsSingle;
          else
            Value := MinValue;
        end;
      end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
end;

procedure TscGPDBMeter120.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBMeter120.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBMeter120.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBSlider.Create(AOwner: TComponent);
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

destructor TscGPDBSlider.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBSlider.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBSlider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBSlider.Change;
begin
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
      inherited Change;
    end;
  FInChange := False;
end;

function TscGPDBSlider.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBSlider.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBSlider.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBSlider.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBSlider.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBSlider.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') then
      begin
        case FDataLink.Field.DataType of
          ftSmallint, ftInteger, ftWord:
            Value := FDataLink.Field.AsInteger;
          ftFloat:
            Value := FDataLink.Field.AsFloat;
          ftSingle:
            Value := FDataLink.Field.AsSingle;
          else
            Value := MinValue;
        end;
      end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
  FInDataChange := False;
end;

procedure TscGPDBSlider.UpdateData(Sender: TObject);
begin
  case FDataLink.Field.DataType of
    ftSmallint, ftInteger, ftWord:
      FDataLink.Field.AsInteger := Trunc(Value);
    ftFloat:
      FDataLink.Field.AsFloat := Value;
    ftSingle:
      FDataLink.Field.AsSingle := Value;
  end;
end;

procedure TscGPDBSlider.CMEnter;
begin
  inherited;
end;

procedure TscGPDBSlider.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
end;

procedure TscGPDBSlider.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBSlider.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBSlider.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


constructor TscGPDBDial.Create(AOwner: TComponent);
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

destructor TscGPDBDial.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBDial.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBDial.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBDial.Change;
begin
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
      inherited Change;
    end;
  FInChange := False;
end;

function TscGPDBDial.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBDial.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBDial.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBDial.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBDial.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBDial.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') then
      begin
        case FDataLink.Field.DataType of
          ftSmallint, ftInteger, ftWord:
            Value := FDataLink.Field.AsInteger;
          ftFloat:
            Value := FDataLink.Field.AsFloat;
          ftSingle:
            Value := FDataLink.Field.AsSingle;
          else
            Value := MinValue;
        end;
      end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
  FInDataChange := False;
end;

procedure TscGPDBDial.UpdateData(Sender: TObject);
begin
  case FDataLink.Field.DataType of
    ftSmallint, ftInteger, ftWord:
      FDataLink.Field.AsInteger := Trunc(Value);
    ftFloat:
      FDataLink.Field.AsFloat := Value;
    ftSingle:
      FDataLink.Field.AsSingle := Value;
  end;
end;

procedure TscGPDBDial.CMEnter;
begin
  inherited;
end;

procedure TscGPDBDial.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
end;

procedure TscGPDBDial.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBDial.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBDial.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;



constructor TscGPDBGearDial.Create(AOwner: TComponent);
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

destructor TscGPDBGearDial.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBGearDial.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBGearDial.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBGearDial.Change;
begin
  FInChange := True;
  if not FInDataChange and (FDataLink <> nil) and
     FDataLink.CanModify
  then
    begin
      if not FDataLink.Editing then FDataLink.Edit;
      FDataLink.Modified;
      inherited Change;
    end;
  FInChange := False;
end;

function TscGPDBGearDial.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBGearDial.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBGearDial.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBGearDial.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBGearDial.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBGearDial.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    begin
      if (FDataLink.Field.Text <> '') then
      begin
        case FDataLink.Field.DataType of
          ftSmallint, ftInteger, ftWord:
            Value := FDataLink.Field.AsInteger;
          ftFloat:
            Value := FDataLink.Field.AsFloat;
          ftSingle:
            Value := FDataLink.Field.AsSingle;
          else
            Value := MinValue;
        end;
      end
      else
        Value := MinValue;
    end
  else
    Value := MinValue;
  FInDataChange := False;
end;

procedure TscGPDBGearDial.UpdateData(Sender: TObject);
begin
  case FDataLink.Field.DataType of
    ftSmallint, ftInteger, ftWord:
      FDataLink.Field.AsInteger := Trunc(Value);
    ftFloat:
      FDataLink.Field.AsFloat := Value;
    ftSingle:
      FDataLink.Field.AsSingle := Value;
  end;
end;

procedure TscGPDBGearDial.CMEnter;
begin
  inherited;
end;

procedure TscGPDBGearDial.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
end;

procedure TscGPDBGearDial.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBGearDial.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBGearDial.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDbEdit.Create(AOwner: TComponent);
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

destructor TscGPDbEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure  TscGPDbEdit.ResetMaxLength;
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

procedure TscGPDbEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if (csDesigning in ComponentState) then
    DataChange(Self)
  else
  if DataSource = nil then
    EditText := '';
end;

procedure TscGPDbEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDbEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscGPDbEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TscGPDbEdit.KeyPress(var Key: Char);
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

function TscGPDbEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TscGPDbEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscGPDbEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscGPDbEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TscGPDbEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDbEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDbEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDbEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TscGPDbEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDbEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDbEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDbEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TscGPDbEdit.DataChange(Sender: TObject);
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

procedure TscGPDbEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TscGPDbEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TscGPDbEdit.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscGPDbEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscGPDbEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscGPDbEdit.CMEnter(var Message: TCMEnter);
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

procedure TscGPDbEdit.CMExit(var Message: TCMExit);
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

procedure TscGPDbEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDbEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDbEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBComboEdit.Create(AOwner: TComponent);
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

destructor TscGPDBComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
     FPopupWasVisible := IsPopupVisible;
  inherited;
end;

procedure TscGPDBComboEdit.KeyPress(var Key: Char);
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

procedure TscGPDBComboEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscGPDBComboEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self)
  else
  if DataSource = nil then
    EditText := '';
end;

procedure TscGPDBComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBComboEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscGPDBComboEdit.Change;
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

function TscGPDBComboEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBComboEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBComboEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBComboEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBComboEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBComboEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBComboEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBComboEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    Text := FDataLink.Field.Text;
  FInDataChange := False;
end;

procedure TscGPDBComboEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscGPDBComboEdit.CMEnter(var Message: TCMEnter);
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

procedure TscGPDBComboEdit.CMExit(var Message: TCMExit);
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

procedure TscGPDBComboEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBComboEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBComboEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBListBox.Create(AOwner: TComponent);
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

destructor TscGPDBListBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TscGPDBListBox.GetListText: string;
var
  I: Integer;
begin
  I := ItemIndex;
  if I < 0 then Result := '' else Result := Items[I].Caption;
end;

procedure TscGPDBListBox.SetListText(const Value: string);
var
  I: Integer;
begin
  if Value = '' then I := -1 else I := IndexOfCaption(Value);
  ItemIndex := I;
end;

procedure TscGPDBListBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBListBox.Change;
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

function TscGPDBListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBListBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBListBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBListBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBListBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBListBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBListBox.DataChange(Sender: TObject);
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

procedure TscGPDBListBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetListText;
end;

procedure TscGPDBListBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TscGPDBListBox.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
end;

procedure TscGPDBListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBComboBox.Create(AOwner: TComponent);
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

destructor TscGPDBComboBox.Destroy;
begin
  TStringList(FValues).OnChange := nil;
  FValues.Free;

  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TscGPDBComboBox.GetComboText: string;
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

procedure TscGPDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
    begin
      FEnableValues := Value;
      DataChange(Self);
    end;
end;

procedure TscGPDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

procedure TscGPDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

procedure TscGPDBComboBox.SetComboText(const Value: string);
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

procedure TscGPDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBComboBox.Change;
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

function TscGPDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBComboBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBComboBox.DataChange(Sender: TObject);
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

procedure TscGPDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetComboText;
end;

procedure TscGPDBComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TscGPDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Editing then FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
end;

procedure TscGPDBComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


constructor TscGPDBNumericEdit.Create(AOwner: TComponent);
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

destructor TscGPDBNumericEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBNumericEdit.KeyPress(var Key: Char);
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

procedure TscGPDBNumericEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscGPDBNumericEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBNumericEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBNumericEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscGPDBNumericEdit.Change;
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

function TscGPDBNumericEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBNumericEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBNumericEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBNumericEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBNumericEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBNumericEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBNumericEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBNumericEdit.DataChange(Sender: TObject);
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

procedure TscGPDBNumericEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscGPDBNumericEdit.CMEnter(var Message: TCMEnter);
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

procedure TscGPDBNumericEdit.CMExit(var Message: TCMExit);
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


procedure TscGPDBNumericEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBNumericEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBNumericEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBSpinEdit.Create(AOwner: TComponent);
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

destructor TscGPDBSpinEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBSpinEdit.KeyPress(var Key: Char);
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

procedure TscGPDBSpinEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscGPDBSpinEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBSpinEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscGPDBSpinEdit.Change;
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

function TscGPDBSpinEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBSpinEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBSpinEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBSpinEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBSpinEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBSpinEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBSpinEdit.DataChange(Sender: TObject);
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

procedure TscGPDBSpinEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscGPDBSpinEdit.CMEnter(var Message: TCMEnter);
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

procedure TscGPDBSpinEdit.CMExit(var Message: TCMExit);
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


procedure TscGPDBSpinEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBSpinEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBSpinEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBTimeEdit.Create(AOwner: TComponent);
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

destructor TscGPDBTimeEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBTimeEdit.KeyPress(var Key: Char);
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

procedure TscGPDBTimeEdit.WMPaint(var Message: TWMPaint);

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

procedure TscGPDBTimeEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBTimeEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBTimeEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscGPDBTimeEdit.Change;
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

function TscGPDBTimeEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBTimeEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscGPDBTimeEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBTimeEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBTimeEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBTimeEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBTimeEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBTimeEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBTimeEdit.DataChange(Sender: TObject);
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

procedure TscGPDBTimeEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsDateTime := TimeValue;
end;

procedure TscGPDBTimeEdit.CMEnter(var Message: TCMEnter);
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

procedure TscGPDBTimeEdit.CMExit(var Message: TCMExit);
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

procedure TscGPDBTimeEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBTimeEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBTimeEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBPasswordEdit.Create(AOwner: TComponent);
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

destructor TscGPDBPasswordEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBPasswordEdit.KeyPress(var Key: Char);
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

function TscGPDBPasswordEdit.GetPaintText;
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

procedure TscGPDBPasswordEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBPasswordEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBPasswordEdit.Change;
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

function TscGPDBPasswordEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBPasswordEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBPasswordEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBPasswordEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBPasswordEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBPasswordEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBPasswordEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBPasswordEdit.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
  if FDataLink.Field <> nil
  then
    Text := FDataLink.Field.Text;
  FInDataChange := False;
end;

procedure TscGPDBPasswordEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

procedure TscGPDBPasswordEdit.CMEnter;
begin
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscGPDBPasswordEdit.CMExit;
begin
  inherited;
  if (FDataLink <> nil) and (FDataLink.Editing)
  then
    FDataLink.UpdateRecord;
end;

procedure TscGPDBPasswordEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBPasswordEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBPasswordEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

constructor TscGPDBDateEdit.Create(AOwner: TComponent);
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

destructor TscGPDBDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBDateEdit.KeyPress(var Key: Char);
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

procedure TscGPDBDateEdit.DropDown;
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

procedure TscGPDBDateEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TscGPDBDateEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TscGPDBDateEdit.Change;
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

function TscGPDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBDateEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBDateEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBDateEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBDateEdit.DataChange(Sender: TObject);
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

procedure TscGPDBDateEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TscGPDBDateEdit.UpdateData(Sender: TObject);
begin
  if not (csDesigning in ComponentState)
  then
    FDataLink.Field.AsDateTime := Date;
end;

procedure TscGPDBDateEdit.CMEnter;
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
  if (FDataLink.DataSource <> nil) and not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscGPDBDateEdit.CMExit;
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

procedure TscGPDBDateEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBDateEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBDateEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


constructor TscGPDBMemo.Create(AOwner: TComponent);
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

destructor TscGPDBMemo.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBMemo.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TscGPDBMemo.Loaded;
begin
  inherited Loaded;
end;

procedure TscGPDBMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBMemo.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TscGPDBMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
      FDataLink.Edit;
  end;
end;

procedure TscGPDBMemo.KeyPress(var Key: Char);
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

procedure TscGPDBMemo.Change;
begin
  if FMemoLoaded then FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TscGPDBMemo.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBMemo.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBMemo.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBMemo.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBMemo.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TscGPDBMemo.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TscGPDBMemo.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBMemo.LoadMemo;
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

procedure TscGPDBMemo.DataChange(Sender: TObject);
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

procedure TscGPDBMemo.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TscGPDBMemo.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsString := Text;
end;

procedure TscGPDBMemo.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TscGPDBMemo.WndProc(var Message: TMessage);
begin
  inherited;
end;

procedure TscGPDBMemo.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;

  if (FDataLink.DataSource <> nil) and not FDataLink.DataSource.AutoEdit and not FDataLink.Editing
  then
    inherited ReadOnly := True;
end;

procedure TscGPDBMemo.CMExit(var Message: TCMExit);
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

procedure TscGPDBMemo.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadMemo;
  end;
end;

procedure TscGPDBMemo.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not FMemoLoaded then LoadMemo else inherited;
end;

procedure TscGPDBMemo.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscGPDBMemo.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscGPDBMemo.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TscGPDBMemo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TscGPDBMemo.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBMemo.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


constructor TscGPDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  AutoSize := False;
  ShowAccelChar := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TscGPDBText.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBText.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TscGPDBText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBText.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TscGPDBText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBText.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBText.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBText.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBText.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TscGPDBText.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then Result := Name else Result := '';
end;

procedure TscGPDBText.DataChange(Sender: TObject);
begin
  Caption := GetFieldText;
end;

function TscGPDBText.GetLabelText: string;
begin
  if csPaintCopy in ControlState then
    Result := GetFieldText else
    Result := Caption;
end;

procedure TscGPDBText.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscGPDBText.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBText.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;


constructor TscGPDBToggleSwitch.Create(AOwner: TComponent);
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

destructor TscGPDBToggleSwitch.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TscGPDBToggleSwitch.ChangeState;
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TscGPDBToggleSwitch.SetState(Value: TscSwitchState);
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

procedure TscGPDBToggleSwitch.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TscGPDBToggleSwitch.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TscGPDBToggleSwitch.GetFieldState: TscSwitchState;
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

procedure TscGPDBToggleSwitch.DataChange(Sender: TObject);
begin
  FInDataChange := True;
  if not FInChange then
    State := GetFieldState;
  FInDataChange := False;
end;

procedure TscGPDBToggleSwitch.UpdateData(Sender: TObject);
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

function TscGPDBToggleSwitch.ValueMatch(const ValueList, Value: string): Boolean;
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

function TscGPDBToggleSwitch.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TscGPDBToggleSwitch.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TscGPDBToggleSwitch.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TscGPDBToggleSwitch.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TscGPDBToggleSwitch.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
  FReadOnly := Result;
end;

function TscGPDBToggleSwitch.IsValueChecked: Boolean;
begin
  Result := not SameText(FValueCheck, 'True');
end;

function TscGPDBToggleSwitch.IsValueUnchecked: Boolean;
begin
  Result := not SameText(FValueUncheck, 'False');
end;

procedure TscGPDBToggleSwitch.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
  FReadOnly := FDataLink.ReadOnly;
end;

function TscGPDBToggleSwitch.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TscGPDBToggleSwitch.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TscGPDBToggleSwitch.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TscGPDBToggleSwitch.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;

procedure TscGPDBToggleSwitch.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TscGPDBToggleSwitch.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TscGPDBToggleSwitch.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TscGPDBToggleSwitch.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

end.

