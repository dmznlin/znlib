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

unit scShellControls;

{$R-}
{$I scdefine.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Winapi.CommCtrl, Winapi.ShlObj, Winapi.ActiveX, Vcl.StdCtrls, Vcl.ImgList, scControls, Vcl.Themes;

const
  SC_IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';

type
  TscRoot = type string;

  TscRootFolder = (rfDesktop, rfMyComputer, rfNetwork, rfRecycleBin, rfAppData,
    rfCommonDesktopDirectory, rfCommonPrograms, rfCommonStartMenu, rfCommonStartup,
    rfControlPanel, rfDesktopDirectory, rfFavorites, rfFonts, rfInternet, rfPersonal,
    rfPrinters, rfPrintHood, rfPrograms, rfRecent, rfSendTo, rfStartMenu, rfStartup,
    rfTemplates);

  TscShellFolderCapability = (fcCanCopy, fcCanDelete, fcCanLink, fcCanMove, fcCanRename,
                   fcDropTarget, fcHasPropSheet);
  TscShellFolderCapabilities = set of TscShellFolderCapability;

  TscShellFolderProperty = (fpCut, fpIsLink, fpReadOnly, fpShared, fpFileSystem,
                     fpFileSystemAncestor, fpRemovable, fpValidate);
  TscShellFolderProperties = set of TscShellFolderProperty;

  TscShellObjectType = (otFolders, otNonFolders, otHidden);
  TscShellObjectTypes = set of TscShellObjectType;

  EInvalidPath = class(Exception);

  IShellCommandVerb = interface
    ['{7D2A7245-2376-4D33-8008-A130935A2E8B}']
    procedure ExecuteCommand(Verb: string; var Handled: boolean);
    procedure CommandCompleted(Verb: string; Succeeded: boolean);
  end;

  TscShellFolder = class;
  TscCustomShellListView = class;
  TscThumbnailState = set of (scthmbIconLoaded, scthmbThumbnailLoaded);
  TscThumbnailsThread = class(TThread)
  protected
    FLV: TscCustomShellListView;
    FStartIndex, FStep: Integer;
    FIconOnly: Boolean;
    procedure Execute; override;
    procedure LoadThumbnails;
    procedure LoadThumbnail(AFolder: TscShellFolder; AIndex: Integer);
    procedure LoadIcon(AFolder: TscShellFolder; AIndex: Integer);
  public
    constructor CreateEx(AFLV: TscCustomShellListView; AStartIndex, AStep: Integer; AIconOnly: Boolean);
  end;

  TscShellFolder = class
  private
    FPIDL,
    FFullPIDL: PItemIDList;
    FParent: TscShellFolder;
    FIShellFolder: IShellFolder;
    FIShellFolder2: IShellFolder2;
    FIShellDetails: IShellDetails;
    FDetailInterface: IInterface;
    FLevel: Integer;
    FViewHandle: THandle;
    FDetails: TStrings;
    FImageIndex: Integer;
    FLargeImageIndex: Integer;
    FDisplayName: String;
    FDetailsEx: Boolean;
    function GetDetailInterface: IInterface;
    function GetShellDetails: IShellDetails;
    function GetShellFolder2: IShellFolder2;
    function GetDetails(Index: integer): string;
    procedure SetDetails(Index: integer; const Value: string);
    procedure LoadColumnDetails(RootFolder: TscShellFolder; Handle: THandle; ColumnCount: integer);
  public
    FThumbnailHBitmap: HBITMAP;
    FIconHBitmap: HBITMAP;
    FThumbnail: TBitmap;
    FThumbnailState: TscThumbnailState;
    constructor Create(AParent: TscShellFolder; ID: PItemIDList; SF: IShellFolder); virtual;
    destructor Destroy; override;
    function Capabilities: TscShellFolderCapabilities;
    function DisplayName: string;
    function ExecuteDefault: Integer;
    function ImageIndex(LargeIcon: Boolean): Integer;
    function IsFolder: Boolean;
    function ParentShellFolder: IShellFolder;
    function PathName: string;
    function FullObjectName: String;
    function Properties: TscShellFolderProperties;
    function Rename(const NewName: WideString): boolean;
    function SubFolders: Boolean;
    property AbsoluteID: PItemIDLIst read FFullPIDL;
    property Details[Index: integer] : string read GetDetails write SetDetails;
    property Level: Integer read FLevel;
    property Parent: TscShellFolder read FParent;
    property RelativeID: PItemIDList read FPIDL;
    property ShellFolder: IShellFolder read FIShellFolder;
    property ShellFolder2: IShellFolder2 read GetShellFolder2;
    property ShellDetails: IShellDetails read GetShellDetails;
    property ViewHandle: THandle read FViewHandle write FViewHandle;
  end;

  TscNotifyFilter = (nfFileNameChange, nfDirNameChange, nfAttributeChange,
    nfSizeChange, nfWriteChange, nfSecurityChange);
  TscNotifyFilters = set of TscNotifyFilter;

  TscShellChangeThread = class(TThread)
  private
    FMutex,
    FWaitHandle: THandle;
    FChangeEvent: TThreadMethod;
    FDirectory: string;
    FWatchSubTree: Boolean;
    FWaitChanged: Boolean;
    FNotifyOptionFlags: DWORD;
  protected
    procedure Execute; override;
  public
    constructor Create(ChangeEvent: TThreadMethod); virtual;
    destructor Destroy; override;
    procedure SetDirectoryOptions(Directory: String; WatchSubTree: Boolean;
      NotifyOptionFlags: DWORD);
    property ChangeEvent: TThreadMethod read FChangeEvent write FChangeEvent;
  end;

  TscCustomShellChangeNotifier = class(TComponent)
  private
    FFilters: TscNotifyFilters;
    FWatchSubTree: Boolean;
    FRoot: TscRoot;
    FThread: TscShellChangeThread;
    FOnChange: TThreadMethod;
    procedure SetRoot(const Value: TscRoot);
    procedure SetWatchSubTree(const Value: Boolean);
    procedure SetFilters(const Value: TscNotifyFilters);
    procedure SetOnChange(const Value: TThreadMethod);
  protected
    procedure Change;
    procedure Start;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NotifyFilters: TscNotifyFilters read FFilters write SetFilters;
    property Root: TscRoot read FRoot write SetRoot;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
    property OnChange: TThreadMethod read FOnChange write SetOnChange;
  end;

  TscShellChangeNotifier = class(TscCustomShellChangeNotifier)
  published
    property NotifyFilters;
    property Root;
    property WatchSubTree;
    property OnChange;
  end;

  TscCustomShellComboBox = class;

  TAddFolderEvent = procedure(Sender: TObject; AFolder: TscShellFolder;
    var CanAdd: Boolean) of object;
  TGetImageIndexEvent = procedure(Sender: TObject; Index: Integer;
     var ImageIndex: Integer) of object;

  TscCustomShellTreeView = class(TscCustomTreeView, IShellCommandVerb)
  private
    FPath: String;
    FRoot,
    FOldRoot : TscRoot;
    FRootFolder: TscShellFolder;
    FObjectTypes: TscShellObjectTypes;
    FLoadingRoot,
    FAutoContext,
    FUpdating: Boolean;
    FListView: TscCustomShellListView;
    FComboBox: TscCustomShellComboBox;
    FAutoRefresh,
    FImageListChanging,
    FUseShellImages: Boolean;
    FNotifier: TscShellChangeNotifier;
    FOnAddFolder: TAddFolderEvent;
    FSavePath: string;
    FNodeToMonitor: TTreeNode;
    function FolderExists(FindID: PItemIDList; InNode: TTreeNode): TTreeNode;
    function GetFolder(Index: Integer): TscShellFolder;
    function GetPath: string;
    procedure SetComboBox(Value: TscCustomShellComboBox);
    procedure SetListView(const Value: TscCustomShellListView);
    procedure SetPath(const Value: string);
    procedure SetPathFromID(ID: PItemIDList);
    procedure SetRoot(const Value: TscRoot);
    procedure SetUseShellImages(const Value: Boolean);
    procedure SetAutoRefresh(const Value: boolean);
  protected
    function CanChange(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure CreateRoot;
    procedure CreateWnd; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure Edit(const Item: TTVItem); override;
    procedure GetImageIndex(Node: TTreeNode); override;
    procedure GetSelectedIndex(Node: TTreeNode); override;
    procedure InitNode(NewNode: TTreeNode; ID: PItemIDList; ParentNode: TTreeNode);
    procedure Loaded; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Delete(Node: TTreeNode); override;
    function NodeFromAbsoluteID(StartNode: TTreeNode; ID: PItemIDList): TTreeNode;
    function NodeFromRelativeID(ParentNode: TTreeNode; ID: PItemIDList): TTreeNode;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopulateNode(Node: TTreeNode);
    procedure RootChanged;
    procedure SetObjectTypes(Value: TscShellObjectTypes); virtual;
    procedure WMDestroy(var Message: TWMDestroy); virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure ClearItems;
    procedure RefreshEvent;
  public
    FImages: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh(Node: TTreeNode);
    procedure ExpandMyComputer;
    function SelectedFolder: TscShellFolder;
    property AutoRefresh: boolean read FAutoRefresh write SetAutoRefresh;
    property Folders[Index: Integer]: TscShellFolder read GetFolder; default;
    property Items;
    property Path: string read GetPath write SetPath;
    property AutoContextMenus: Boolean read FAutoContext write FAutoContext default True;
    property ObjectTypes: TscShellObjectTypes read FObjectTypes write SetObjectTypes;
    property Root: TscRoot read FRoot write SetRoot;
    property ShellListView: TscCustomShellListView read FListView write SetListView;
    property ShellComboBox: TscCustomShellComboBox read FComboBox write SetComboBox;
    property UseShellImages: Boolean read FUseShellImages write SetUseShellImages;
    property OnAddFolder: TAddFolderEvent read FOnAddFolder write FOnAddFolder;
    procedure CommandCompleted(Verb: String; Succeeded: Boolean);
    procedure ExecuteCommand(Verb: String; var Handled: Boolean);
  end;

  TscShellTreeView = class(TscCustomShellTreeView)
  published
    {$IFNDEF VER230}
    property StyleElements;
    {$ENDIF}
    property ButtonImages;
    property ButtonCollapseImageIndex;
    property ButtonExpandImageIndex;
    property SelectionStyle;
    property SelectionColor;
    property SelectionTextColor;
    property ShowFocusRect;
    property DefaultDraw;
    property ButtonStyle;
    property AutoContextMenus;
    property ObjectTypes;
    property Root;
    property ShellComboBox;
    property ShellListView;
    property UseShellImages;
    property OnAddFolder;
    property Align;
    property Anchors;
    property AutoRefresh;
    property BorderStyle;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Cursor;
    property DoubleBuffered;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Images;
    property Indent;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightClickSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChanging;
    property OnChange;
    property OnExpanding;
    property OnCollapsing;
    property OnCollapsed;
    property OnExpanded;
    property OnEditing;
    property OnEdited;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
  end;

  TscCustomShellComboBox = class(TscCustomComboBoxEx)
  private
    FImages,
    FImageHeight,
    FImageWidth: Integer;
    FImageList: TCustomImageList;
    FOldRoot: TscRoot;
    FRoot: TscRoot;
    FRootFolder: TscShellFolder;
    FTreeView: TscCustomShellTreeView;
    FListView: TscCustomShellListView;
    FObjectTypes: TscShellObjectTypes;
    FUseShellImages,
    FUpdating: Boolean;
    FOnGetImageIndex: TGetImageIndexEvent;
    procedure ClearItems;
    function GetFolder(Index: Integer): TscShellFolder;
    function GetPath: string;
    procedure SetPath(const Value: string);
    procedure SetRoot(const Value: TscRoot);
    procedure SetTreeView(Value: TscCustomShellTreeView);
    procedure SetListView(Value: TscCustomShellListView);
    procedure SetUseShellImages(const Value: Boolean);
    function GetShellImageIndex(AFolder: TscShellFolder): integer;
  protected
    procedure AddItems(Index: Integer; ParentFolder: TscShellFolder);
    procedure Change; override;
    procedure Click; override;
    procedure CreateRoot;
    procedure CreateWnd; override;
    function IndexFromID(AbsoluteID: PItemIDList): Integer;
    procedure Init; virtual;
    function InitItem(ParentFolder: TscShellFolder; ID: PItemIDList): TscShellFolder;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RootChanged;
    procedure TreeUpdate(NewPath: PItemIDList);
    procedure SetObjectTypes(Value: TscShellObjectTypes); virtual;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetPathFromID(ID: PItemIDList);
    property Items;
    property Path: string read GetPath write SetPath;
    property Folders[Index: Integer]: TscShellFolder read GetFolder;
    property Root: TscRoot read FRoot write SetRoot;
    property ObjectTypes: TscShellObjectTypes read FObjectTypes write SetObjectTypes;
    property ShellTreeView: TscCustomShellTreeView read FTreeView write SetTreeView;
    property ShellListView: TscCustomShellListView read FListView write SetListView;
    property UseShellImages: Boolean read FUseShellImages write SetUseShellImages;
    property OnGetImageIndex: TGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
  end;

  TscShellComboBox = class(TscCustomShellComboBox)
  published
    property Align;
    property Images;
    property Root;
    property ShellTreeView;
    property ShellListView;
    property UseShellImages;
    property OnGetImageIndex;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
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
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TscThumbnailOptions = class(TPersistent)
  private
    FHeight: Integer;
    FEnabled: Boolean;
    FWidth: Integer;
    FOnChange: TNotifyEvent;
    FOnEnabledChange: TNotifyEvent;
    FDrawSelectionFrame: Boolean;
    procedure SetHeight(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    procedure Changed;
    procedure DoChange;
    procedure DoEnabledChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnabledChange: TNotifyEvent read FOnEnabledChange write FOnEnabledChange;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure SetValues(AEnabled: Boolean; AWidth, AHeight: Integer);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Width: Integer read FWidth write SetWidth default 96;
    property Height: Integer read FHeight write SetHeight default 96;
    property DrawSelectionFrame: Boolean read FDrawSelectionFrame write FDrawSelectionFrame default True;
  end;

  TscCustomShellListView = class(TscCustomListView, IShellCommandVerb)
  private
    FStopFetch: Boolean;
    FIsVistaOrLater: Boolean;
    FThumbnailOptions: TscThumbnailOptions;
    FIsThumbnailView: Boolean;
    FTempThumbnailImages: TCustomImageList;
    FOldSortColumn: Integer;
    FSortColumn: Integer;
    FSortDirection: Boolean;
    FRootLoaded: Boolean;
    FOldRoot: TscRoot;
    FRoot: TscRoot;
    FRootFolder: TscShellFolder;
    FAutoContext,
    FAutoRefresh,
    FAutoNavigate,
    FAutoExecute,
    FSorted,
    FUpdating: Boolean;
    FObjectTypes: TscShellObjectTypes;
    FLargeImages,
    FSmallImages: Integer;
    FOnAddFolder: TAddFolderEvent;
    FFolders: TList;
    FTreeView: TscCustomShellTreeView;
    FComboBox: TscCustomShelLComboBox;
    FNotifier: TscShellChangeNotifier;
    FOnEditing: TLVEditingEvent;
    FSettingRoot: boolean;
    FSavePath: string;
    FMask: String;
    FOnPathChanged: TNotifyEvent;
    FThumbsThreadList: TList;
    procedure ClearThreads;
    function GetThreadCount: Integer;
    procedure DrawThumbnailImage(AItem: TListItem;
      ACanvas: TCanvas; ARect: TRect);
    procedure OnThumbnailOptionsChange(Sender: TObject);
    procedure OnThumbnailOptionsEnabledChange(Sender: TObject);
    procedure SetMask(const Value: String);
    procedure EnumColumns;
    function GetFolder(Index: Integer): TscShellFolder;
    procedure SetAutoRefresh(const Value: Boolean);
    procedure SetSorted(const Value: Boolean);
    procedure SetTreeView(Value: TscCustomShellTreeView);
    procedure SetComboBox(Value: TscCustomShellComboBox);
    procedure SynchPaths;
    function GetPath: string;
    procedure SetPath(const Value: string);
  protected
    procedure HookAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
        State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean); override;
    procedure UpdateThumbnail(AFolder: TscShellFolder);
    procedure ColClick(Column: TListColumn); override;
    procedure ClearItems;
    procedure CreateRoot;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure EditText;
    procedure Edit(const Item: TLVItem); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean; override;
    function OwnerDataFind(Find: TItemFind; const FindString: string;
      const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer;
      Direction: TSearchDirection; Wrap: Boolean): Integer; override;
    procedure Populate; virtual;
    procedure SetObjectTypes(Value: TscShellObjectTypes);
    procedure SetRoot(const Value: TscRoot);
    procedure SetViewStyle(Value: TViewStyle); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateThumbnails;
    procedure TreeUpdate(NewRoot: PItemIDList);
    procedure RootChanged;
    function IsThumbnailView: Boolean;
    procedure Back;
    procedure Refresh;
    procedure SetPathFromID(ID: PItemIDList);
    procedure SetRootDirectly(const Value: TscRoot);
    function GetSelectedPath: String;
    function GetSelectedFile: String;
    procedure GetSelectedFiles(AFiles: TStrings);
    function SelectedFolder: TscShellFolder;
    property Folders[Index: Integer]: TscShellFolder read GetFolder;
    property RootFolder: TscShellFolder read FRootFolder;
    property Path: string read GetPath write SetPath;
    property Items;
    property Columns;
    property Mask: String read FMask write SetMask;
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute default False;
    property AutoContextMenus: Boolean read FAutoContext write FAutoContext default True;
    property AutoRefresh: Boolean read FAutoRefresh write SetAutoRefresh default False;
    property AutoNavigate: Boolean read FAutoNavigate write FAutoNavigate default True;
    property ObjectTypes: TscShellObjectTypes read FObjectTypes write SetObjectTypes;
    property Root: TscRoot read FRoot write SetRoot;
    property ShellTreeView: TscCustomShellTreeView read FTreeView write SetTreeView;
    property ShellComboBox: TscCustomShellComboBox read FComboBox write SetComboBox;
    property Sorted: Boolean read FSorted write SetSorted;
    property ThumbnailOptions: TscThumbnailOptions read
      FThumbnailOptions write FThumbnailOptions;
    property OnAddFolder: TAddFolderEvent read FOnAddFolder write FOnAddFolder;
    property OnEditing: TLVEditingEvent read FOnEditing write FOnEditing;
    procedure CommandCompleted(Verb: String; Succeeded: Boolean);
    procedure ExecuteCommand(Verb: String; var Handled: Boolean);
    property OnPathChanged: TNotifyEvent read FOnPathChanged write FOnPathChanged;
  end;

  TscShellListView = class(TscCustomShellListView)
  published
    property ThumbnailOptions;
    property Mask;
    property SelectionStyle;
    property SelectionColor;
    property SelectionTextColor;
    property ShowFocusRect;
    property AlternateRow;
    property GridLines;
    property DefaultDraw;
    property AutoContextMenus;
    property AutoRefresh;
    property AutoNavigate;
    property AutoExecute;
    property ObjectTypes;
    property Root;
    property ShellTreeView;
    property ShellComboBox;
    property Sorted;
    property OnAddFolder;
    property Align;
    property DoubleBuffered;
    property Anchors;
    property BorderStyle;
    property Color;
    property ColumnClick;
    property OnClick;
    property OnDblClick;
    property Ctl3D;
    property DragMode;
    property ReadOnly default True;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property IconOptions;
    property AllocBy;
    property MultiSelect;
    property RowSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnDragDrop;
    property OnDragOver;
    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property ViewStyle;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnEditing;
  end;

  TscFilterComboBox = class(TscComboBox)
  private
    FFilter: string;
    MaskList: TStringList;
    function IsFilterStored: Boolean;
    function GetMask: string;
    procedure SetFilter(const NewFilter: string);
  protected
    procedure CreateWnd; override;
    procedure Click; override;
    procedure BuildList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Mask: string read GetMask;
    property Text;
  published
    property Anchors;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Filter: string read FFilter write SetFilter stored IsFilterStored;
    property Font;
    property ImeName;
    property ImeMode;
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
  end;

  procedure InvokeContextMenu(Owner: TWinControl; AFolder: TscShellFolder; X, Y: Integer);
  function GetIdList_Libraries: pItemIdList;
  procedure GetRootInfo(Folder: TscRootFolder; var ACaption: String; var AImageIndex: Integer);
  function CopyPIDL(IDList: PItemIDList): PItemIDList;

  var
    SC_SHCreateItemFromIDList: function(pidl: PItemIdList; const riid: TIID; out ppvOut): HResult; stdcall;
    SC_SHGetImageList: function (iImageList: integer; const riid: TGUID; var ppv: Pointer): hResult; stdcall;
    SC_SHGetKnownFolderIDList: function(const rfid: TGUID; dwFlags: DWORD; hToken: THandle; var ppidl: PItemIDList ): HResult; stdcall;

     // from comdlg32.dll
    SC_S_Folder: String = 'Look &in:';
    SC_S_Save: String = '&Save';
    SC_S_Open: String = '&Open';
    SC_S_Cancel: String = 'Cancel';
    SC_S_FileType: String = 'File &type';
    SC_S_FileName: String = 'File &name';
    SC_S_SelectDir: String = 'Select Directory';
    SC_S_FileOpen: String = 'Open';
    SC_S_FileSave: String = 'Save';
    SC_S_GoToLastFolderVisited_Hint: String = 'Go To Last Folder Visited';
    SC_S_UpOneLevel_Hint: String = 'Up One Level';
    SC_S_CreateNewFolder_Hint: String  = 'Create New Folder';
    SC_S_ViewMenu_Hint: String = 'View Menu';
    SC_S_Replace: String = '%s already exists. Do you want replace it?';
    // from shell32.dll
    SC_S_Create: String = 'Create';
    // new folder
    SC_S_NewFolder: String = 'New Folder';


implementation

uses Winapi.ShellAPI, System.Win.ComObj, scDrawUtils,
  System.TypInfo, Vcl.Menus, Vcl.Consts, System.Math, System.Masks, System.MaskUtils,
  scDlgStrs;

const
  SShellDefaultNameStr = 'Name';
  SShellDefaultSizeStr = 'Size';
  SShellDefaultTypeStr = 'Type';
  SShellDefaultModifiedStr = 'Modified';
  SShellNoDetails = 'Unable to retrieve folder details for "%s". Error code $%x';
  SCallLoadDetails = '%s: Missing call to LoadColumnDetails';
  SPalletePage = 'Samples';
  SPropertyName = 'Root';
  SRenamedFailedError = 'Rename to %s failed';
  SRFDesktop = 'rfDesktop';
  SCmdVerbOpen = 'open';
  SCmdVerbRename = 'rename';
  SCmdVerbDelete = 'delete';
  SCmdVerbPaste = 'paste';
  FOLDERID_Libraries : TGUID ='{1B3EA5DC-B587-4786-B4EF-BD1DC332AEAE}';

  nFolder: array[TscRootFolder] of Integer =
    (CSIDL_DESKTOP, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_BITBUCKET, CSIDL_APPDATA,
    CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTMENU,
    CSIDL_COMMON_STARTUP, CSIDL_CONTROLS, CSIDL_DESKTOPDIRECTORY, CSIDL_FAVORITES,
    CSIDL_FONTS, CSIDL_INTERNET, CSIDL_PERSONAL, CSIDL_PRINTERS, CSIDL_PRINTHOOD,
    CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU, CSIDL_STARTUP,
    CSIDL_TEMPLATES);


var
  cmvProperties: PAnsiChar = 'properties';
  ICM: IContextMenu = nil;
  ICM2: IContextMenu2 = nil;
  DesktopFolder: TscShellFolder = nil;
  CS: TRTLCriticalSection;

function GetIdList_Libraries: pItemIdList;
begin
  if @SC_SHCreateItemFromIDList <> nil then
    SC_SHGetKnownFolderIDList(FOLDERID_LIBRARIES, 0, 0, Result)
  else
    Result := nil;
end;

function MapColumnToSCID(AShellFolder2: IShellFolder2; AColumnID: UINT;
    var pscid: TShColumnID): Boolean;
var
  HR: HRESULT;
begin
  HR := AShellFolder2.MapColumnToSCID(AColumnID, pscid);
  Result := HR = S_OK;
end;

procedure debug(Comp:TComponent; msg:string);
begin
  ShowMessage(Comp.Name + ':' + msg);
end;

function CreatePIDL(Size: Integer): PItemIDList;
var
  Malloc: IMalloc;
begin
  OleCheck(SHGetMalloc(Malloc));

  Result := Malloc.Alloc(Size);
  if Assigned(Result) then
    FillChar(Result^, Size, 0);
end;

function NextPIDL(IDList: PItemIDList): PItemIDList;
begin
  Result := IDList;
  Inc(PByte(Result), IDList^.mkid.cb);
end;

procedure StripLastID(IDList: PItemIDList);
var
  MarkerID: PItemIDList;
begin
  MarkerID := IDList;
  if Assigned(IDList) then
  begin
    while IDList.mkid.cb <> 0 do
    begin
      MarkerID := IDList;
      IDList := NextPIDL(IDList);
    end;
    MarkerID.mkid.cb := 0;
  end;
end;

function GetItemCount(IDList: PItemIDList): Integer;
begin
  Result := 0;
  while IDList^.mkid.cb <> 0 do
  begin
    Inc(Result);
    IDList := NextPIDL(IDList);
  end;
end;

function GetPIDLSize(IDList: PItemIDList): Integer;
begin
  Result := 0;
  if Assigned(IDList) then
  begin
    Result := SizeOf(IDList^.mkid.cb);
    while IDList^.mkid.cb <> 0 do
    begin
      Result := Result + IDList^.mkid.cb;
      IDList := NextPIDL(IDList);
    end;
  end;
end;

function CopyPIDL(IDList: PItemIDList): PItemIDList;
var
  Size: Integer;
begin
  Size := GetPIDLSize(IDList);
  Result := CreatePIDL(Size);
  if Assigned(Result) then
    CopyMemory(Result, IDList, Size);
end;

function ConcatPIDLs(IDList1, IDList2: PItemIDList): PItemIDList;
var
  Cb1, Cb2: Integer;
begin
  if Assigned(IDList1) then
    Cb1 := GetPIDLSize(IDList1) - SizeOf(IDList1^.mkid.cb)
  else
    Cb1 := 0;

  Cb2 := GetPIDLSize(IDList2);

  Result := CreatePIDL(Cb1 + Cb2);
  if Assigned(Result) then
  begin
    if Assigned(IDList1) then
      CopyMemory(Result, IDList1, Cb1);
    CopyMemory(PByte(Result) + Cb1, IDList2, Cb2);
  end;
end;

procedure DisposePIDL(PIDL: PItemIDList);
var
  MAlloc: IMAlloc;
begin
  OLECheck(SHGetMAlloc(MAlloc));
  MAlloc.Free(PIDL);
end;

function RelativeFromAbsolute(AbsoluteID: PItemIDList): PItemIDList;
begin
  Result := AbsoluteID;
  while GetItemCount(Result) > 1 do
     Result := NextPIDL(Result);
  Result := CopyPIDL(Result);
end;

function CreatePIDLList(ID: PItemIDList): TList;
var
  TempID: PItemIDList;
begin
  Result := TList.Create;
  TempID := ID;
  while TempID.mkid.cb <> 0 do
  begin
    TempID := CopyPIDL(TempID);
    Result.Insert(0, TempID);
    StripLastID(TempID);
  end;
end;

procedure DestroyPIDLList(List: TList);
var
  I: Integer;
begin
  If List = nil then Exit;
  for I := 0 to List.Count-1 do
    DisposePIDL(List[I]);
  List.Free;
end;

procedure NoFolderDetails(AFolder: TscShellFolder; HR: HResult);
begin
  Raise EInvalidPath.CreateFmt(SShellNoDetails, [AFolder.DisplayName, HR]);
end;

function DesktopShellFolder: IShellFolder;
begin
  OleCheck(SHGetDesktopFolder(Result));
end;

procedure CreateDesktopFolder;
var
  DesktopPIDL: PItemIDList;
begin
  SHGetSpecialFolderLocation(0, nFolder[rfDesktop], DesktopPIDL);
  if DesktopPIDL <> nil then
  begin
    DesktopFolder := TscShellFolder.Create(nil, DesktopPIDL, DesktopShellFolder);
    DisposePIDL(DesktopPIDL);
  end;
end;

function SamePIDL(ID1, ID2: PItemIDList): boolean;
begin
  Result := DesktopShellFolder.CompareIDs(0, ID1, ID2) = 0;
end;

function DesktopPIDL: PItemIDList;
begin
  OleCheck(SHGetSpecialFolderLocation(0, nFolder[rfDesktop], Result));
end;

function GetCSIDLType(const Value: string): TscRootFolder;
begin
{$R+}
  Result := TscRootFolder(GetEnumValue(TypeInfo(TscRootFolder), Value))
{$R-}
end;

function IsElement(Element, Flag: Integer): Boolean;
begin
  Result := Element and Flag <> 0;
end;

function GetShellImage(PIDL: PItemIDList; Large, Open: Boolean): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
begin
  Flags := SHGFI_PIDL or SHGFI_SYSICONINDEX;
  if Open then Flags := Flags or SHGFI_OPENICON;
  if Large then Flags := Flags or SHGFI_LARGEICON
  else Flags := Flags or SHGFI_SMALLICON;
  SHGetFileInfo(PChar(PIDL),
                0,
                FileInfo,
                SizeOf(FileInfo),
                Flags);
  Result := FileInfo.iIcon;
end;

function GetCaps(ParentFolder: IShellFolder; PIDL: PItemIDList): TscShellFolderCapabilities;
var
  Flags: LongWord;
begin
  Result := [];
  Flags := SFGAO_CAPABILITYMASK;
  ParentFolder.GetAttributesOf(1, PIDL, Flags);
  if IsElement(SFGAO_CANCOPY, Flags) then Include(Result, fcCanCopy);
  if IsElement(SFGAO_CANDELETE, Flags) then Include(Result, fcCanDelete);
  if IsElement(SFGAO_CANLINK, Flags) then Include(Result, fcCanLink);
  if IsElement(SFGAO_CANMOVE, Flags) then Include(Result, fcCanMove);
  if IsElement(SFGAO_CANRENAME, Flags) then Include(Result, fcCanRename);
  if IsElement(SFGAO_DROPTARGET, Flags) then Include(Result, fcDropTarget);
  if IsElement(SFGAO_HASPROPSHEET, Flags) then Include(Result, fcHasPropSheet);
end;

function GetProperties(ParentFolder: IShellFolder; PIDL: PItemIDList): TscShellFolderProperties;
var
  Flags: LongWord;
begin
  Result := [];
  if ParentFolder = nil then Exit;
  Flags := SFGAO_DISPLAYATTRMASK;
  ParentFolder.GetAttributesOf(1, PIDL, Flags);
  if IsElement(SFGAO_GHOSTED, Flags) then Include(Result, fpCut);
  if IsElement(SFGAO_LINK, Flags) then Include(Result, fpIsLink);
  if IsElement(SFGAO_READONLY, Flags) then Include(Result, fpReadOnly);
  if IsElement(SFGAO_SHARE, Flags) then Include(Result, fpShared);

  Flags := 0;
  ParentFolder.GetAttributesOf(1, PIDL, Flags);
  if IsElement(SFGAO_FILESYSTEM, Flags) then Include(Result, fpFileSystem);
  if IsElement(SFGAO_FILESYSANCESTOR, Flags) then Include(Result, fpFileSystemAncestor);
  if IsElement(SFGAO_REMOVABLE, Flags) then Include(Result, fpRemovable);
  if IsElement(SFGAO_VALIDATE, Flags) then Include(Result, fpValidate);
end;

function GetIsFolder(Parentfolder: IShellFolder; PIDL: PItemIDList): Boolean;
var
  Flags: LongWord;
begin
  Flags := SFGAO_FOLDER;
  ParentFolder.GetAttributesOf(1, PIDL, Flags);
  Result := SFGAO_FOLDER and Flags <> 0;
end;

function GetHasSubFolders(Parentfolder: IShellFolder; PIDL: PItemIDList): Boolean;
var
  Flags: LongWord;
begin
  Flags := SFGAO_CONTENTSMASK;
  ParentFolder.GetAttributesOf(1, PIDL, Flags);
  Result := SFGAO_HASSUBFOLDER and Flags <> 0;
end;

function GetHasSubItems(ShellFolder: IShellFolder; Flags: Integer): Boolean;
var
  ID: PItemIDList;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  HR: HResult;
  ErrMode: Integer;
begin
  Result := False;
  if ShellFolder = nil then Exit;
  ErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    HR := ShellFolder.EnumObjects(0,
                                Flags,
                                EnumList);
    if HR <> S_OK then Exit;
    Result := EnumList.Next(1, ID, NumIDs) = S_OK;
  finally
    SetErrorMode(ErrMode);
  end;
end;

function StrRetToString(PIDL: PItemIDList; StrRet: TStrRet; Flag:string=''): string;
var
  P: PAnsiChar;
begin
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLenA(StrRet.cStr));
    STRRET_OFFSET:
      begin
        P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
        SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
      end;
    STRRET_WSTR:
      if Assigned(StrRet.pOleStr) then
        Result := StrRet.pOleStr
      else
        Result := '';
  end;
  if (Length(Result) > 1) and (Result[1] = '?') and CharInSet(Result[2], ['0'..'9']) then
    Result := StringReplace(Result,'?','',[rfReplaceAll]);
end;

function GetDisplayName(Parentfolder: IShellFolder; PIDL: PItemIDList;
                        Flags: DWORD): string;
var
  StrRet: TStrRet;
begin
  Result := '';
  if ParentFolder = nil then
  begin
    Result := 'parentfolder = nil';
    exit;
  end;
  FillChar(StrRet, SizeOf(StrRet), 0);
  ParentFolder.GetDisplayNameOf(PIDL, Flags, StrRet);
  Result := StrRetToString(PIDL, StrRet);
  if (Pos('::{', Result) = 1) then
    Result := GetDisplayName(ParentFolder, PIDL, SHGDN_NORMAL);
end;

function ObjectFlags(ObjectTypes: TscShellObjectTypes): Integer;
begin
  Result := 0;
  if otFolders in ObjectTypes then Inc(Result, SHCONTF_FOLDERS);
  if otNonFolders in ObjectTypes then Inc(Result, SHCONTF_NONFOLDERS);
  if otHidden in ObjectTypes then Inc(Result, SHCONTF_INCLUDEHIDDEN);
end;

procedure GetRootInfo(Folder: TscRootFolder; var ACaption: String; var AImageIndex: Integer);
var
  NewPIDL: PItemIDList;
begin
  if (Folder = rfPersonal) and not IsWindowsXP then
  begin
    NewPIDL := GetIdList_Libraries;
    if NewPIDL = nil then
      SHGetSpecialFolderLocation(0, nFolder[Folder], NewPIDL);
  end
  else
    SHGetSpecialFolderLocation(0, nFolder[Folder], NewPIDL);
  ACaption := GetDisplayName(DesktopShellFolder, NewPIDL, SHGDN_NORMAL);
  AImageIndex := GetShellImage(newPIDL, True, False);
end;

procedure InvokeContextMenu(Owner: TWinControl; AFolder: TscShellFolder; X, Y: Integer);
var
  PIDL: PItemIDList;
  CM: IContextMenu;
  Menu: HMenu;
  ICI: TCMInvokeCommandInfo;
  P: TPoint;
  Command: LongBool;
  ICmd: integer;
  ZVerb: array[0..255] of AnsiChar;
  Verb: string;
  Handled: boolean;
  SCV: IShellCommandVerb;
  HR: HResult;
begin
  if AFolder = nil then Exit;
  PIDL := AFolder.RelativeID;
  AFolder.ParentShellFolder.GetUIObjectOf(Owner.Handle, 1, PIDL, IID_IContextMenu, nil, CM);
  if CM = nil then Exit;
  P.X := X;
  P.Y := Y;

  Winapi.Windows.ClientToScreen(Owner.Handle, P);
  Menu := CreatePopupMenu;
  try
    CM.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME);
    CM.QueryInterface(IID_IContextMenu2, ICM2);
    try
      Command := TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or
        TPM_RETURNCMD, P.X, P.Y, 0, Owner.Handle, nil);
    finally
      ICM2 := nil;
    end;

    if Command then
    begin
      ICmd := LongInt(Command) - 1;
      HR := CM.GetCommandString(ICmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb));
      Verb := String(ZVerb);
      Handled := False;
      if Supports(Owner, IShellCommandVerb, SCV) then
      begin
        HR := 0;
        SCV.ExecuteCommand(Verb, Handled);
      end;

      if not Handled then
      begin
        FillChar(ICI, SizeOf(ICI), #0);
        with ICI do
        begin
          cbSize := SizeOf(ICI);
          hWND := Owner.Handle;
          lpVerb := MakeIntResourceA(ICmd);
          nShow := SW_SHOWNORMAL;
        end;
        HR := CM.InvokeCommand(ICI);
      end;

      if Assigned(SCV) then
        SCV.CommandCompleted(Verb, HR = S_OK);
    end;
  finally
    DestroyMenu(Menu);
  end;
end;

procedure DoContextMenuVerb(AFolder: TscShellFolder; Verb: PAnsiChar);
var
  ICI: TCMInvokeCommandInfo;
  CM: IContextMenu;
  PIDL: PItemIDList;
begin
  if AFolder = nil then Exit;
  FillChar(ICI, SizeOf(ICI), #0);
  with ICI do
  begin
    cbSize := SizeOf(ICI);
    fMask := CMIC_MASK_ASYNCOK;
    hWND := 0;
    lpVerb := Verb;
    nShow := SW_SHOWNORMAL;
  end;
  PIDL := AFolder.RelativeID;
  AFolder.ParentShellFolder.GetUIObjectOf(0, 1, PIDL, IID_IContextMenu, nil, CM);
  CM.InvokeCommand(ICI);
end;

function GetIShellFolder(IFolder: IShellFolder; PIDL: PItemIDList;
  Handle: THandle = 0): IShellFolder;
var
  HR: HResult;
begin
  if Assigned(IFolder) then
  begin
    HR := IFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(Result));
    if HR <> S_OK then
      IFolder.GetUIObjectOf(Handle, 1, PIDL, IID_IShellFolder, nil, Pointer(Result));
    if HR <> S_OK then
      IFolder.CreateViewObject(Handle, IID_IShellFolder, Pointer(Result));
  end;
  if not Assigned(Result) then
    DesktopShellFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(Result));
end;

function GetIShellDetails(IFolder: IShellFolder; PIDL: PItemIDList;
  Handle: THandle = 0): IShellDetails;
var
  HR: HResult;
begin
  if Assigned(IFolder) then
  begin
    HR := IFolder.BindToObject(PIDL, nil, IID_IShellDetails, Pointer(Result));
    if HR <> S_OK then
      IFolder.GetUIObjectOf(Handle, 1, PIDL, IID_IShellDetails, nil, Pointer(Result));
    if HR <> S_OK then
      IFolder.CreateViewObject(Handle, IID_IShellDetails, Pointer(Result));
  end;
  if not Assigned(Result) then
    DesktopShellFolder.BindToObject(PIDL, nil, IID_IShellDetails, Pointer(Result));
end;

function GetIShellFolder2(IFolder: IShellFolder; PIDL: PItemIDList;
  Handle: THandle = 0): IShellFolder2;
var
  HR: HResult;
begin
  if (Win32MajorVersion >= 5) then
  begin
    HR := DesktopShellFolder.BindToObject(PIDL, nil, IID_IShellFolder2, Pointer(Result));
    if HR <> S_OK then
      IFolder.GetUIObjectOf(Handle, 1, PIDL, IID_IShellFolder2, nil, Pointer(Result));
    if (HR <> S_OK) and (IFolder <> nil) then
      IFolder.BindToObject(PIDL, nil, IID_IShellFolder2, Pointer(Result));
  end
  else
    Result := nil;
end;

function CreateRootFromPIDL(Value: PItemIDList): TscShellFolder;
var
  SF: IShellFolder;
begin
  SF := GetIShellFolder(DesktopShellFolder, Value);
  if SF = NIL then SF := DesktopShellFolder;
  Result := TscShellFolder.Create(DesktopFolder, Value, SF);
end;

function CreateRootFolder(RootFolder: TscShellFolder; OldRoot: TscRoot;
  var NewRoot: TscRoot): TscShellFolder;
var
  P: PWideChar;
  NewPIDL: PItemIDList;
  NumChars,
  Flags,
  HR: LongWord;
  ErrorMsg: string;
begin
  HR := S_FALSE;
  if GetEnumValue(TypeInfo(TscRootFolder), NewRoot) >= 0 then
  begin
    HR := SHGetSpecialFolderLocation(
            0,
            nFolder[GetCSIDLType(NewRoot)],
            NewPIDL);
  end
  else if Length(NewRoot) > 0 then
  begin
    if NewRoot[Length(NewRoot)] = ':' then NewRoot := NewRoot + '\';
    NumChars := Length(NewRoot);
    Flags := 0;
    P := StringToOleStr(NewRoot);
    HR := DesktopShellFolder.ParseDisplayName(0, nil, P, NumChars, NewPIDL, Flags);
  end;

  if HR <> S_OK then
  begin
    ErrorMsg := Format(SErrorSettingPath, [NewRoot]);
    NewRoot := OldRoot;
    raise Exception.Create(ErrorMsg);
  end;

  Result := CreateRootFromPIDL(NewPIDL);
  if Assigned(RootFolder) then RootFolder.Free;
end;

constructor TscThumbnailsThread.CreateEx(AFLV: TscCustomShellListView; AStartIndex, AStep: Integer; AIconOnly: Boolean);
begin
  inherited Create(True);
  FIconOnly := AIconOnly;
  Priority := tpIdle;
  FStartIndex := AStartIndex;
  FStep := AStep;
  FLV := AFLV;
  FreeOnTerminate := False;
end;

procedure TscThumbnailsThread.Execute;
begin
  LoadThumbnails;
end;

procedure TscThumbnailsThread.LoadThumbnails;
var
  I: Integer;
  AFolder: TscShellFolder;
  ASucceeded: Boolean;
begin
  ASucceeded := CoInitializeEx(nil, COINIT_APARTMENTTHREADED) = S_OK;
  if ASucceeded then
  begin
    I := FStartIndex;
    if I < FLV.FFolders.Count then
    repeat
      if I < FLV.FFolders.Count then
      begin
        AFolder := FLV.FFolders[I];
        if AFolder <> nil then
         if FIconOnly
         then
           LoadIcon(AFolder, I)
         else
           LoadThumbnail(AFolder, I);
      end;
      Inc(I, FStep);
    until (I > FLV.FFolders.Count - 1) or Terminated;
  end;
  if ASucceeded then CoUninitialize;
end;

procedure TscThumbnailsThread.LoadThumbnail(AFolder: TscShellFolder; AIndex: Integer);
var
  FThumbSize: TSize;
  FHBitmap: HBitmap;
  FShellItem: IShellItem;
  FItemImageFactory: IShellItemImageFactory;
begin
  if FLV.FStopFetch then Exit;
  FThumbSize.cx := FLV.FThumbnailOptions.Width;
  FThumbSize.cy := FLV.FThumbnailOptions.Height;
  if FLV.FIsVistaOrLater and
    (@SC_SHCreateItemFromIDList <> nil) and
    (SC_SHCreateItemFromIDList(AFolder.FFullPIDL, IID_IShellItem, FShellItem) = S_Ok) and
    (FShellItem.QueryInterface(IID_IShellItemImageFactory, FItemImageFactory) = S_OK) and
    (FItemImageFactory.GetImage(FThumbSize, 0, FHBitmap) = S_OK) then
  begin
    AFolder.FThumbnailHBitmap := FHBitmap;
    if FLV.HandleAllocated then
      ListView_ReDrawItems(FLV.Handle, AIndex, AIndex);
  end;
end;

procedure TscThumbnailsThread.LoadIcon(AFolder: TscShellFolder; AIndex: Integer);
var
  FThumbSize: TSize;
  FHBitmap: HBitmap;
  FShellItem: IShellItem;
  FItemImageFactory: IShellItemImageFactory;

const
  SIIGBF_ICONONLY = $00000004;

begin
  if FLV.FStopFetch or (scthmbThumbnailLoaded in AFolder.FThumbnailState) or
     (AFolder.FThumbnailHBitmap <> 0) then
    Exit;
  FThumbSize.cx := FLV.FThumbnailOptions.Width;
  FThumbSize.cy := FLV.FThumbnailOptions.Height;
  if FLV.FIsVistaOrLater and
    (@SC_SHCreateItemFromIDList <> nil) and
    (SC_SHCreateItemFromIDList(AFolder.FFullPIDL, IID_IShellItem, FShellItem) = S_Ok) and
    (FShellItem.QueryInterface(IID_IShellItemImageFactory, FItemImageFactory) = S_OK) and
    (FItemImageFactory.GetImage(FThumbSize, SIIGBF_ICONONLY, FHBitmap) = S_OK) then
  begin
    AFolder.FIconHBitmap := FHBitmap;
    if FLV.HandleAllocated then
      ListView_ReDrawItems(FLV.Handle, AIndex, AIndex);
  end;
end;

constructor TscShellFolder.Create(AParent: TscShellFolder; ID: PItemIDList; SF: IShellFolder);
var
  DesktopID: PItemIDList;
begin
  inherited Create;
  FThumbnailHBitmap := 0;
  FIconHBitmap := 0;
  FLevel := 0;
  FDetails := TStringList.Create;
  FImageIndex := -1;
  FLargeImageIndex := -1;
  FDisplayName := '/';
  FIShellFolder := SF;
  FIShellFolder2 := nil;
  FIShellDetails := nil;
  FParent := AParent;
  FPIDL := CopyPIDL(ID);
  FThumbnail := TBitmap.Create;
  FThumbnailState := [];
  FThumbnailHBitmap := 0;
  if FParent <> nil then
    FFullPIDL := ConcatPIDLs(AParent.FFullPIDL, ID)
  else
  begin
    DesktopID := DesktopPIDL;
    try
      FFullPIDL := ConcatPIDLs(DesktopID, ID);
    finally
      DisposePIDL(DesktopID);
    end;
  end;
  if FParent = nil then
    FParent := DesktopFolder;
  while AParent <> nil do
  begin
    AParent := AParent.Parent;
    if AParent <> nil then Inc(FLevel);
  end;
end;

destructor TscShellFolder.Destroy;
begin
   FThumbnailState := [];
  if FThumbnailHBitmap <> 0 then
    DeleteObject(FThumbnailHBitmap);
  FThumbnailHBitmap := 0;
  if FIconHBitmap <> 0 then
    DeleteObject(FIconHBitmap);
  FIconHBitmap := 0;

  FThumbnail.Free;
  FThumbnail := nil;

  if Assigned(FDetails) then
    FDetails.Free;
  FDetails := nil;
  if Assigned(FPIDL) then
    DisposePIDL(FPIDL);
  if Assigned(FFullPIDL) then
    DisposePIDL(FFullPIDL);
  inherited Destroy;
end;

function TscShellFolder.GetDetailInterface: IInterface;
begin
  if (not Assigned(FDetailInterface)) and Assigned(FIShellFolder) then
  begin
    FIShellDetails := GetIShellDetails(FIShellFolder, FFullPIDL, FViewHandle);
    if (not Assigned(FIShellDetails)) and (Win32MajorVersion >= 5) then
    begin
      FIShellFolder2 := GetIShellFolder2(FIShellFolder, FFullPIDL, FViewHandle);
      if not Assigned(FIShellFolder2) then
          FIShellFolder2 := IShellFolder2(FIShellFolder);
    end;
    if Assigned(FIShellFolder2) then
      Result := IInterface(FIShellFolder2)
    else
      Result := IInterface(FIShellDetails);
    FDetailInterface := Result;
  end
  else
    Result := FDetailInterface;
end;

function TscShellFolder.GetShellDetails: IShellDetails;
begin
  if not Assigned(FDetailInterface) then
    GetDetailInterface;
  Result := FIShellDetails;
end;

function TscShellFolder.GetShellFolder2: IShellFolder2;
begin
  if not Assigned(FDetailInterface) then
    GetDetailInterface;
  Result := FIShellFolder2;
end;

procedure TscShellFolder.LoadColumnDetails(RootFolder: TscShellFolder;
  Handle: THandle; ColumnCount: integer);

  procedure GetDetailsOf(AFolder: TscShellFolder; var Details: TWin32FindData);
  var
    szPath: array[ 0 .. MAX_PATH] of char;
    Path: string;
    Handle: THandle;
  begin
    FillChar(Details, SizeOf(Details), 0);
    FillChar(szPath,MAX_PATH,0);
    Path := AFolder.PathName;
    Handle := WinApi.Windows.FindFirstFile(PChar(Path), Details);
    try
      if Handle = INVALID_HANDLE_VALUE then
        NoFolderDetails(AFolder, WinApi.Windows.GetLastError);
    finally
      WinApi.Windows.FindClose(Handle);
    end;
  end;

  function CalcFileSize(FindData: TWin32FindData): int64;
  begin
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      Result := FindData.nFileSizeHigh * MAXDWORD + FindData.nFileSizeLow
    else
      Result := -1;
  end;

  function CalcModifiedDate(FindData: TWin32FindData): TDateTime;
  var
    LocalFileTime: TFileTime;
    Age : integer;
  begin
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Age).Hi,
        LongRec(Age).Lo) then
      begin
        Result := FileDateToDateTime(Age);
        Exit;
      end;
    end;
    Result := -1;
  end;

  function DefaultDetailColumn(FindData: TWin32FindData; Col: integer): string;
  begin
    case Col of
      1 : Result := IntToStr(CalcFileSize(FindData)); // Size
      2 : Result := ExtractFileExt(FindData.cFileName); // Type
      3 : Result := DateTimeToStr(CalcModifiedDate(FindData)); // Modified
      4 : Result := IntToStr(FindData.dwFileAttributes);
    end;
  end;

  procedure AddDetail(HR: HResult; PIDL: PItemIDList; SD: TShellDetails);
  var
    S: String;
  begin
    if HR = S_OK then
    begin
      S := StrRetToString(PIDL, SD.str);
      FDetails.Add(S);
    end
    else
      FDetails.Add('');
  end;

  function FormatSizeStr(AStr: string): string;
  begin
    Result := FormatMaskText('!### ### ### KB;0;*', AStr);
  end;

var
  SF2: IShellFolder2;
  ISD: IShellDetails;
  J, I: Integer;
  SD: TShellDetails;
  HR: HResult;
  FindData: TWin32FindData;
  V: OleVariant;
  AShColumnID: SHCOLUMNID;
  S: String;
begin
  if not Assigned(FDetails) or (FDetails.Count >= ColumnCount) then Exit;
  FDetails.Clear;
  FViewHandle := Handle;
  SF2 := RootFolder.ShellFolder2;
  if Assigned(SF2) then
  begin
    FDetailsEx := False;
    for J := 1 to ColumnCount do
    begin
      HR := SF2.GetDetailsOf(FPIDL, J, SD);
      if (HR <> S_OK) and MapColumnToSCID(SF2, J, AShColumnID) then
      begin
        HR := SF2.GetDetailsEx(FPIDL, AShColumnID, @V);
        if HR = S_OK then
        begin
          FDetailsEx := True;
          S := V;
          if AShColumnID.pid = 12 then
          begin
            I := V;
            S := FormatSizeStr(IntToStr(Ceil(I/1024)));
          end;
          FDetails.Add(S);
        end
        else
          FDetails.Add('');
      end
      else
        AddDetail(HR, FPIDL, SD);
    end;
  end
  else
  begin
    ISD := RootFolder.ShellDetails;
    if Assigned(ISD) then
    begin
      for J := 1 to ColumnCount do
      begin
        HR := ISD.GetDetailsOf(FPIDL, J, SD);
        AddDetail(HR, FPIDL, SD);
      end;
    end
    else if (fpFileSystem in RootFolder.Properties) then
    begin
      GetDetailsOf(Self, FindData);
      for J := 1 to ColumnCount do
        FDetails.Add(DefaultDetailColumn(FindData, J));
    end;
  end;
end;


function TscShellFolder.GetDetails(Index: integer): string;
begin
  if FDetails.Count > 0 then
    Result := FDetails[Index-1]
  else
    Raise Exception.CreateFmt(SCallLoadDetails, [ Self.DisplayName ] );
end;

procedure TscShellFolder.SetDetails(Index: integer; const Value: string);
begin
  if Index < FDetails.Count then
    FDetails[Index - 1] := Value
  else
    FDetails.Insert(Index - 1, Value);
end;

function TscShellFolder.ParentShellFolder: IShellFolder;
begin
  if FParent <> nil then
    Result := FParent.ShellFolder
  else
    OLECheck(SHGetDesktopFolder(Result));
end;

function TscShellFolder.Properties: TscShellFolderProperties;
begin
  Result := GetProperties(ParentShellFolder, FPIDL);
end;

function TscShellFolder.Capabilities: TscShellFolderCapabilities;
begin
  Result := GetCaps(ParentShellFolder, FPIDL);
end;

function TscShellFolder.SubFolders: Boolean;
begin
  Result := GetHasSubFolders(ParentShellFolder, FPIDL);
end;

function TscShellFolder.IsFolder: Boolean;
var
  S: String;
begin
  Result := GetIsFolder(ParentShellFolder, FPIDL);
  if Result
  then
    begin
      S := FullObjectName;
      Result := UpperCase(ExtractFileExt(S)) <> '.ZIP';
    end;
end;

function TscShellFolder.PathName: string;
begin
  Result := GetDisplayName(DesktopShellFolder, FFullPIDL, SHGDN_FORPARSING);
end;

function TscShellFolder.FullObjectName: String;
var
  Tmp: array [0..MAX_PATH] of Char;
begin
  Result := '';
  if SHGetPathFromIdList(FFullPIDL, @Tmp) then Result := Tmp;
end;

function TscShellFolder.DisplayName: string;
var
  ParentFolder: IShellFolder;
begin
  if FDisplayName = '/' then
  begin
    if Parent <> nil then
      ParentFolder := ParentShellFolder
    else
      ParentFolder := DesktopShellFolder;
    FDisplayName := GetDisplayName(ParentFolder, FPIDL, SHGDN_INFOLDER);
  end;
  Result := FDisplayName;
end;

function TscShellFolder.Rename(const NewName: Widestring): boolean;
var
  NewPIDL: PItemIDList;
begin
  Result := False;
  if not (fcCanRename in Capabilities) then Exit;

  Result := ParentShellFolder.SetNameOf(
       0,
       FPIDL,
       PWideChar(NewName),
       SHGDN_NORMAL,
       NewPIDL) = S_OK;
  if Result then
  begin
    DisposePIDL(FPIDL);
    DisposePIDL(FFullPIDL);
    FPIDL := NewPIDL;
    if (FParent <> nil) then
      FFullPIDL := ConcatPIDLs(FParent.FPIDL, NewPIDL)
    else
      FFullPIDL := CopyPIDL(NewPIDL);
  end
  else
    Raise Exception.Create(Format(SRenamedFailedError,[NewName]));
end;

function TscShellFolder.ImageIndex(LargeIcon: Boolean): Integer;
begin
  if (FImageIndex = -1) and not LargeIcon then
    FImageIndex := GetShellImage(AbsoluteID, False, False);
  if (FLargeImageIndex = -1) and LargeIcon then
    FLargeImageIndex := GetShellImage(AbsoluteID, True, False);
  if LargeIcon then
    Result := FLargeImageIndex
  else
    Result := FImageIndex;
end;

function TscShellFolder.ExecuteDefault: Integer;
var
  SEI: TShellExecuteInfo;
begin
  FillChar(SEI, SizeOf(SEI), 0);
  with SEI do
  begin
    cbSize := SizeOf(SEI);
    wnd := Application.Handle;
    fMask := SEE_MASK_INVOKEIDLIST;
    lpIDList := AbsoluteID;
    nShow := SW_SHOW;
  end;
  Result := Integer(ShellExecuteEx(@SEI));
end;

procedure TscCustomShellChangeNotifier.Change;

  function NotifyOptionFlags: DWORD;
  begin
    Result := 0;
    if nfFileNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
    if nfDirNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
    if nfSizeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SIZE;
    if nfAttributeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
    if nfWriteChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
    if nfSecurityChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
  end;

begin
  if Assigned(FThread) then
  begin
    FThread.SetDirectoryOptions(Root, LongBool(FWatchSubTree),
      NotifyOptionFlags);
  end;
end;

constructor TscCustomShellChangeNotifier.Create(AOwner: TComponent);
begin
  inherited;
  FRoot := 'C:\';
  FWatchSubTree := True;
  FFilters := [nfFilenameChange, nfDirNameChange];
  Start;
end;

destructor TscCustomShellChangeNotifier.Destroy;
var
  Temp: TscShellChangeThread;
begin
  if Assigned(FThread) then
  begin
    Temp := FThread;
    FThread := nil;
    Temp.Terminate;
    ReleaseMutex(Temp.FMutex);
  end;
  inherited;
end;

procedure TscCustomShellChangeNotifier.SetRoot(const Value: TscRoot);
begin
  if not SameText(FRoot, Value) then
  begin
    FRoot := Value;
    Change;
  end;
end;

procedure TscCustomShellChangeNotifier.SetFilters(const Value: TscNotifyFilters);
begin
  FFilters := Value;
  Change;
end;

procedure TscCustomShellChangeNotifier.SetOnChange(const Value: TThreadMethod);
begin
  FOnChange := Value;
  if Assigned(FThread) then
    FThread.ChangeEvent := FOnChange
  else
    Start;
end;

procedure TscCustomShellChangeNotifier.SetWatchSubTree(const Value: Boolean);
begin
  FWatchSubTree := Value;
  Change;
end;

procedure TscCustomShellChangeNotifier.Start;

  function NotifyOptionFlags: DWORD;
  begin
    Result := 0;
    if nfFileNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
    if nfDirNameChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
    if nfSizeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SIZE;
    if nfAttributeChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
    if nfWriteChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
    if nfSecurityChange in FFilters then
      Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
  end;

begin
  if Assigned(FOnChange) then
  begin
    FThread := TscShellChangeThread.Create(FOnChange);
    FThread.SetDirectoryOptions(FRoot,
      LongBool(FWatchSubTree), NotifyOptionFlags);
    FThread.Start;
  end;
end;


constructor TscShellChangeThread.Create(ChangeEvent: TThreadMethod);
begin
  FreeOnTerminate := True;
  FChangeEvent := ChangeEvent;
  FMutex := CreateMutex(nil, True, nil);
  WaitForSingleObject(FMutex, INFINITE);
  FWaitChanged := false;
  inherited Create(True);
end;

destructor TscShellChangeThread.Destroy;
begin
  if FWaitHandle <> ERROR_INVALID_HANDLE then
    FindCloseChangeNotification(FWaitHandle);
  CloseHandle(FMutex);
  inherited Destroy;
end;

procedure TscShellChangeThread.Execute;
var
  Obj: DWORD;
  Handles: array[0..1] of THandle;
begin
  EnterCriticalSection(CS);
  FWaitHandle := FindFirstChangeNotification(PChar(FDirectory),
     LongBool(FWatchSubTree), FNotifyOptionFlags);
  LeaveCriticalSection(CS);
  if FWaitHandle = ERROR_INVALID_HANDLE then Exit;
  while not Terminated do
  begin
    Handles[0] := FWaitHandle;
    Handles[1] := FMutex;
    Obj := WaitForMultipleObjects(2, @Handles, False, INFINITE);
    case Obj of
      WAIT_OBJECT_0:
        begin
          Synchronize(FChangeEvent);
          FindNextChangeNotification(FWaitHandle);
        end;
      WAIT_OBJECT_0 + 1:
        ReleaseMutex(FMutex);
      WAIT_FAILED:
        Exit;
    end;
    EnterCriticalSection(CS);
    if FWaitChanged then
    begin
      FWaitHandle := FindFirstChangeNotification(PChar(FDirectory),
         LongBool(FWatchSubTree), FNotifyOptionFlags);
      FWaitChanged := false;
    end;
    LeaveCriticalSection(CS);
  end;
end;

procedure TscShellChangeThread.SetDirectoryOptions(Directory: String;
  WatchSubTree: Boolean; NotifyOptionFlags: DWORD);
begin
  EnterCriticalSection(CS);
  FDirectory := Directory;
  FWatchSubTree := WatchSubTree;
  FNotifyOptionFlags := NotifyOptionFlags;

  FindCloseChangeNotification(FWaitHandle);
  FWaitChanged := true;
  LeaveCriticalSection(CS);
end;

constructor TscCustomShellTreeView.Create(AOwner: TComponent);
var
  FileInfo: TSHFileInfo;
begin
  inherited Create(AOwner);
  FPath := '';
  FRootFolder := nil;
  ShowRoot := False;
  FObjectTypes := [otFolders];
  HideSelection := False;
  RightClickSelect := True;
  FAutoContext := True;
  FUpdating := False;
  FListView := nil;
  FComboBox := nil;
  FImageListChanging := False;
  FUseShellImages := True;
  FImages := SHGetFileInfo('C:\',
    0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  ImageList_SetBkColor(FImages, CLR_NONE);
  FNotifier := TscShellChangeNotifier.Create(Self);
  FNotifier.FComponentStyle := FNotifier.FComponentStyle + [ csSubComponent ];
  FRoot := SRFDesktop;
  FLoadingRoot := False;
end;

procedure TscCustomShellTreeView.ExpandMyComputer;
var
  ID: PItemIDList;
begin
  SHGetSpecialFolderLocation(0, nFolder[rfMyComputer], ID);
  SetPathFromID(ID);
  if Selected <> nil
  then
    begin
      Selected.Expand(False);
    end;
end;

procedure TscCustomShellTreeView.ClearItems;
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    for I := 0 to Items.Count-1 do
    begin
      if Assigned(Folders[i]) then
        Folders[I].Free;
      Items[I].Data := nil;
    end;
    Items.Clear;
  finally
    Items.EndUpdate;
  end;
end;

procedure TscCustomShellTreeView.CreateWnd;
begin
  inherited CreateWnd;
  if (Items.Count = 0) and not FLoadingRoot then
  begin
    CreateRoot;
    if FPath <> '' then SetPath(FPath);
  end;
  if not Assigned(Images) then
    SetUseShellImages(FUseShellImages);
end;

destructor TscCustomShellTreeView.Destroy;
begin
  if Assigned(FRootFolder) then FRootFolder.Free;
  inherited;
end;

procedure TscCustomShellTreeView.CommandCompleted(Verb: String;
  Succeeded: Boolean);
var
  Fldr : TscShellFolder;
begin
  if not Succeeded then Exit;
  if Assigned(Selected) then
  begin
    if SameText(Verb, SCmdVerbDelete) then
    begin
      Fldr := TscShellFolder(Selected.Data);
      if not FileExists(Fldr.PathName) then
      begin
        Selected.Data := nil;
        Selected.Delete;
        FreeAndNil(Fldr);
      end;
    end
    else if SameText(Verb, SCmdVerbPaste) then
      Refresh(Selected)
    else if SameText(Verb, SCmdVerbOpen) then
      SetCurrentDirectory(PChar(FSavePath));
  end;
end;

procedure TscCustomShellTreeView.ExecuteCommand(Verb: String;
  var Handled: Boolean);
var
  szPath: array[0..MAX_PATH] of char;
begin
  if SameText(Verb, SCmdVerbRename) and Assigned(Selected) then
  begin
    Selected.EditText;
    Handled := True;
  end
  else if SameText(Verb, SCmdVerbOpen) then
  begin
    GetCurrentDirectory(MAX_PATH, szPath);
    FSavePath := StrPas(szPath);
    StrPCopy(szPath, ExtractFilePath(TscShellFolder(Selected.Data).PathName));
    SetCurrentDirectory(szPath);
  end;

end;

function TreeSortFunc(Node1, Node2: TTreeNode; lParam: Integer): Integer; stdcall;
begin
  Result := SmallInt(TscShellFolder(Node1.Data).ParentShellFolder.CompareIDs(
       0, TscShellFolder(Node1.Data).RelativeID, TscShellFolder(Node2.Data).RelativeID));
end;

procedure TscCustomShellTreeView.InitNode(NewNode: TTreeNode; ID: PItemIDList; ParentNode: TTreeNode);
var
  CanAdd: Boolean;
  NewFolder: IShellFolder;
  AFolder: TscShellFolder;
  S: String;
begin
  AFolder := TscShellFolder(ParentNode.Data);
  NewFolder := GetIShellFolder(AFolder.ShellFolder, ID);
  NewNode.Data := TscShellFolder.Create(AFolder, ID, NewFolder);
  with TscShellFolder(NewNode.Data) do
  begin
    S := PathName;
    NewNode.Text := DisplayName;
    if FUseShellImages and not Assigned(Images) then
    begin
      NewNode.ImageIndex := GetShellImage(AbsoluteID, False, False);
      NewNode.SelectedIndex := GetShellImage(AbsoluteID, False, True);
    end;
    if NewNode.SelectedIndex = 0 then NewNode.SelectedIndex := NewNode.ImageIndex;
    NewNode.HasChildren := SubFolders;
    if (otNonFolders in ObjectTypes) and (ShellFolder <> nil) then
      NewNode.HasChildren := GetHasSubItems(ShellFolder, ObjectFlags(FObjectTypes));
  end;

  CanAdd := True;
  if Assigned(FOnAddFolder) then FOnAddFolder(Self, TscShellFolder(NewNode.Data), CanAdd);
  if not CanAdd then
    NewNode.Delete;
end;

procedure TscCustomShellTreeView.PopulateNode(Node: TTreeNode);
var
  ID: PItemIDList;
  EnumList: IEnumIDList;
  NewNode: TTreeNode;
  NumIDs: LongWord;
  SaveCursor: TCursor;
  HR: HResult;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Items.BeginUpdate;
  try
    try
      HR := TscShellFolder(Node.Data).ShellFolder.EnumObjects(Application.Handle,
                     ObjectFlags(FObjectTypes),
                     EnumList);
      if HR <> 0 then Exit;
    except on E:Exception do end;

    while EnumList.Next(1, ID, NumIDs) = S_OK do
    begin
      NewNode := Items.AddChild(Node, '');
      InitNode(NewNode, ID, Node);
    end;

    Node.CustomSort(@TreeSortFunc, 0);
  finally
    Items.EndUpdate;
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TscCustomShellTreeView.SetObjectTypes(Value: TscShellObjectTypes);
begin
  FObjectTypes := Value;
  RootChanged;
end;

procedure TscCustomShellTreeView.CreateRoot;
var
  RootNode: TTreeNode;
  ErrorMsg: string;
begin
  if (csLoading in ComponentState) then Exit;
  try
    FRootFolder := CreateRootFolder(FRootFolder, FOldRoot, FRoot);
    ErrorMsg := '';
  except
    on E : Exception do ErrorMsg := E.Message;
  end;

  if Assigned(FRootFolder) then
  begin
    FLoadingRoot := true;
    try
      if Items.Count > 0 then
        ClearItems;
      RootNode := Items.Add(nil, '');
      with RootNode do
      begin
        Data := TscShellFolder.Create(nil, FRootFolder.AbsoluteID, FRootFolder.ShellFolder);

        Text := GetDisplayName(DesktopShellFolder,
                               TscShellFolder(Data).AbsoluteID,
                               SHGDN_NORMAL);

        if FUseShellImages and not Assigned(Images) then
        begin
          RootNode.ImageIndex := GetShellImage(TscShellFolder(RootNode.Data).AbsoluteID, False, False);
          RootNode.SelectedIndex := GetShellImage(TscShellFolder(RootNode.Data).AbsoluteID, False, True);
        end;
        RootNode.HasChildren := TscShellFolder(RootNode.Data).SubFolders;
      end;
      RootNode.Expand(False);
      Selected := RootNode;
    finally
      FLoadingRoot := False;
    end;
  end;
  if ErrorMsg <> '' then
    Raise Exception.Create( ErrorMsg );
end;

function TscCustomShellTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  Fldr: TscShellFolder;
begin
  Result := True;
  Fldr := TscShellFolder(Node.Data);
  if (csDesigning in ComponentState) and (Node.Level > 0) then Exit;
  if Assigned(OnExpanding) then OnExpanding(Self, Node, Result);
  if Result then
    if Fldr.IsFolder and (Node.HasChildren) and (Node.Count = 0) then
      PopulateNode(Node)
    else if not Fldr.IsFolder then
    begin
      ShellExecute(Handle, nil, PChar(Fldr.PathName), nil,
        PChar(ExtractFilePath(Fldr.PathName)), 0);
    end;
  Node.HasChildren := Node.Count > 0;
end;

procedure TscCustomShellTreeView.Edit(const Item: TTVItem);
var
  S: string;
  Node: TTreeNode;
begin
  with Item do
    if pszText <> nil then
    begin
      S := pszText;
      Node := Items.GetNode(Item.hItem);
         if Assigned(OnEdited) then OnEdited(Self, Node, S);
      if ( Node <> nil ) and TscShellFolder(Node.Data).Rename(S) then
        Node.Text := S;
      Self.Refresh(Node.Parent);
    end;
end;

procedure TscCustomShellTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

function TscCustomShellTreeView.NodeFromRelativeID(ParentNode: TTreeNode; ID: PItemIDList): TTreeNode;
var
  HR: HResult;
begin
  Result := ParentNode.GetFirstChild;
  while (Result <> nil) do
  begin
    HR := TscShellFolder(ParentNode.Data).ShellFolder.CompareIDs(0, ID, TscShellFolder(Result.Data).RelativeID);
    if HR = 0 then Exit;
    Result := ParentNode.GetNextChild(Result);
  end;
end;

function TscCustomShellTreeView.NodeFromAbsoluteID(StartNode: TTreeNode; ID: PItemIDList): TTreeNode;
var
  HR: HResult;
begin
  Result := StartNode;
  while Result <> nil do
  begin
    HR := DesktopShellFolder.CompareIDs(0, ID, TscShellFolder(Result.Data).AbsoluteID);
    if HR = 0 then Exit;
    Result := Result.GetNext;
  end;
end;

procedure TscCustomShellTreeView.Delete(Node: TTreeNode);
begin
  if Assigned(Node.Data) then
  begin
    TscShellFolder(Node.Data).Free;
    Node.Data := nil;
  end;
  inherited Delete(Node);
end;

procedure TscCustomShellTreeView.RootChanged;
begin
  if FUpdating then Exit;
  FUpdating := True;
  try
    CreateRoot;
    if Assigned(FListView) then
      FListView.SetRoot(FRoot);
  finally
    FUpdating := False;
  end;
end;

function TscCustomShellTreeView.FolderExists(FindID: PItemIDList; InNode: TTreeNode): TTreeNode;
var
  ALevel: Integer;
begin
  Result := nil;
  ALevel := InNode.Level;
  repeat
    if DesktopShellFolder.CompareIDs(
      0,
      FindID,
      TscShellFolder(InNode.Data).AbsoluteID) = 0 then
    begin
      Result := InNode;
      Exit;
    end else
      InNode := InNode.GetNext;
  until (InNode = nil) or (InNode.Level <= ALevel);
end;

procedure TscCustomShellTreeView.RefreshEvent;
begin
  if Assigned(Selected) then
    Refresh(Selected);
end;

procedure TscCustomShellTreeView.Refresh(Node: TTreeNode);
var
  NewNode, OldNode, Temp: TTreeNode;
  OldFolder, NewFolder: TscShellFolder;
  ThisLevel: Integer;
  SaveCursor: TCursor;
  TopID, SelID: PItemIDList;
  ParentFolder: TscShellFolder;
begin
  if TscShellFolder(Node.Data).ShellFolder = nil then Exit;
  SaveCursor := Screen.Cursor;
  ParentFolder := nil;
  TopID := CopyPIDL(TscShellFolder(TopItem.Data).RelativeID);
  if TscShellFolder(TopItem.Data).Parent <> nil then
    TopID := ConcatPIDLs(TscShellFolder(TopItem.Data).Parent.AbsoluteID, TopID);
  SelID := nil;
  if (Selected <> nil) and (Selected.Data <> nil) then
  begin
    SelID := CopyPIDL(TscShellFolder(Selected.Data).RelativeID);
    if TscShellFolder(Selected.Data).Parent <> nil then
      SelID := ConcatPIDLs(TscShellFolder(Selected.Data).Parent.AbsoluteID, SelID);
  end;

  Items.BeginUpdate;
  try
    Screen.Cursor := crHourglass;
    OldFolder := Node.Data;
    NewNode := Items.Insert(Node, '');
    if Node.Parent <> nil then
      ParentFolder := TscShellFolder(Node.Parent.Data);
    NewNode.Data := TscShellFolder.Create(ParentFolder,
                                   OldFolder.RelativeID,
                                   OldFolder.ShellFolder);
    PopulateNode(NewNode);
    with NewNode do
    begin
      NewFolder := Data;
      ImageIndex := GetShellImage(NewFolder.AbsoluteID, False, False);
      SelectedIndex := GetShellImage(NewFolder.AbsoluteID, False, True);
      HasChildren := NewFolder.SubFolders;
      Text := NewFolder.DisplayName;
    end;

    ThisLevel := Node.Level;
    OldNode := Node;
    repeat
      Temp := FolderExists(TscShellFolder(OldNode.Data).AbsoluteID, NewNode);
      if (Temp <> nil) and OldNode.Expanded then
        Temp.Expand(False);
      OldNode := OldNode.GetNext;
    until (OldNode = nil) or (OldNode.Level = ThisLevel);

    if Assigned(Node.Data) then
    begin
      TscShellFolder(Node.Data).Free;
      Node.Data := nil;
    end;
    Node.Delete;
    if SelID <> nil then
    begin
      Temp := FolderExists(SelID, Items[0]);
      Selected := Temp;
    end;
    Temp := FolderExists(TopID, Items[0]);
    TopItem := Temp;
  finally
    Items.EndUpdate;
    DisposePIDL(TopID);
    if SelID <> nil then DisposePIDL(SelID);
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TscCustomShellTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FListView) then
      FListView := nil else
    if (AComponent = FComboBox) then
      FComboBox := nil
  end;
end;

function TscCustomShellTreeView.CanChange(Node: TTreeNode): Boolean;
var
  Fldr: TscShellFolder;
  StayFresh: boolean;
begin
  Result := inherited CanChange(Node);
  if Result and (not FUpdating) and Assigned(Node) then
  begin
    Fldr := TscShellFolder(Node.Data);
    StayFresh := FAutoRefresh;
    AutoRefresh := False;
    if not Fldr.IsFolder then
      Fldr := Fldr.Parent;
    FUpdating := True;
    try
     if Assigned(FComboBox) then
       FComboBox.TreeUpdate(Fldr.AbsoluteID);

     if Assigned(FListView) then
        FListView.TreeUpdate(Fldr.AbsoluteID);
    finally
      FUpdating := False;
    end;
    FNodeToMonitor := Node;
    try
      AutoRefresh := StayFresh;
    finally
      FNodeToMonitor := nil;
    end;
  end;
end;

function TscCustomShellTreeView.GetFolder(Index: Integer): TscShellFolder;
begin
  if Items.Count = 0
  then
    Result := nil
  else
    Result := TscShellFolder(Items[Index].Data);
end;

function TscCustomShellTreeView.SelectedFolder: TscShellFolder;
begin
  Result := nil;
  if Selected <> nil then Result := TscShellFolder(Selected.Data);
end;

function TscCustomShellTreeView.GetPath: String;
begin
  if SelectedFolder <> nil then
    Result := SelectedFolder.PathName
  else
    Result := '';
end;

procedure TscCustomShellTreeView.SetPath(const Value: string);
var
  P: PWideChar;
  NewPIDL: PItemIDList;
  Flags,
  NumChars: LongWord;
begin
  FPath := Value;
  NumChars := Length(Value);
  Flags := 0;
  P := StringToOleStr(Value);
  try
    OLECheck(DesktopShellFolder.ParseDisplayName(
        0,
        nil,
        P,
        NumChars,
        NewPIDL,
        Flags)
     );
    SetPathFromID(NewPIDL);
    if Selected <> nil
    then
      begin
        Selected.Expand(False);
        CanChange(Selected);
      end;
    FPath := Value;
  except on EOleSysError do
    raise EInvalidPath.CreateFmt(SErrorSettingPath, [Value]);
  end;
end;

procedure TscCustomShellTreeView.SetPathFromID(ID: PItemIDList);
var
  I: Integer;
  Pidls: TList;
  Temp, Node: TTreeNode;
begin
  if (csLoading in ComponentState) or
     ((SelectedFolder <> nil) and SamePIDL(SelectedFolder.AbsoluteID, ID)) then Exit;
  FUpdating := True;
  Items.BeginUpdate;
  try
    Pidls := CreatePIDLList(ID);
    try
      Node := Items[0];
      for I := 0 to Pidls.Count-1 do
      begin
        Temp := FolderExists(Pidls[I], Node);
        if Temp <> nil then
        begin
          Node := Temp;
          Node.Expand(False);
        end;
      end;
      Node := FolderExists(ID, Node);
      Selected := Node;
      if Assigned(Node) then
      begin
        if Assigned(FListView) then
          FListView.TreeUpdate(TscShellFolder(Node.Data).AbsoluteID);
        if Assigned(FComboBox) then
          FComboBox.TreeUpdate(TscShellFolder(Node.Data).AbsoluteID);
      end;
    finally
      DestroyPIDLList(Pidls);
    end;
  finally
    Items.EndUpdate;
    FUpdating := False;
  end;
end;

procedure TscCustomShellTreeView.SetRoot(const Value: TscRoot);
begin
  if not SameText(FRoot, Value) then
  begin
    FOldRoot := FRoot;
    FRoot := Value;
    RootChanged;
  end;
end;

procedure TscCustomShellTreeView.GetImageIndex(Node: TTreeNode);
begin
  if Assigned(Images) then
    inherited GetImageIndex(Node);
end;

procedure TscCustomShellTreeView.GetSelectedIndex(Node: TTreeNode);
begin
  if Assigned(Images) then
    inherited GetSelectedIndex(Node);
end;

procedure TscCustomShellTreeView.WndProc(var Message: TMessage);
var
  ImageListHandle: THandle;
begin
  case Message.Msg of
    WM_INITMENUPOPUP,
    WM_DRAWITEM,
    WM_MENUCHAR,
    WM_MEASUREITEM:
      if Assigned(ICM2) then
      begin
        ICM2.HandleMenuMsg(Message.Msg, Message.wParam, Message.lParam);
        Message.Result := 0;
      end;

    TVM_SETIMAGELIST:
      if not FImageListChanging then
      begin
        FImageListChanging := True;
        try
         if not Assigned(Images) then
           if FUseShellImages then
             ImageListHandle := FImages
           else
             ImageListHandle := 0
         else
           ImageListHandle := Images.Handle;

           SendMessage(Self.Handle, TVM_SETIMAGELIST, TVSIL_NORMAL, ImageListHandle);
        finally
          FImageListChanging := False;
        end;
      end
      else inherited;
  else
    inherited WndProc(Message);
  end;
end;

procedure TscCustomShellTreeView.SetUseShellImages(const Value: Boolean);
var
  ImageListHandle: THandle;
begin
  FUseShellImages := Value;
  if not Assigned(Images) then
    if FUseShellImages then
      ImageListHandle := FImages
    else
      ImageListHandle := 0
  else
    ImageListHandle := Images.Handle;
  SendMessage(Handle, TVM_SETIMAGELIST, TVSIL_NORMAL, ImageListHandle);
end;

procedure TscCustomShellTreeView.WMDestroy(var Message: TWMDestroy);
begin
  ClearItems;
  inherited;
end;

procedure TscCustomShellTreeView.Loaded;
begin
  inherited Loaded;
  CreateRoot;
end;

procedure TscCustomShellTreeView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  if AutoContextMenus and not (Assigned(PopupMenu) and PopupMenu.AutoPopup) then
    InvokeContextMenu(Self, SelectedFolder, MousePos.X, MousePos.Y)
  else
    inherited;
end;

procedure TscCustomShellTreeView.SetComboBox(Value: TscCustomShellComboBox);
begin
  if Value = FComboBox then Exit;
  if Value <> nil then
  begin
    Value.Root := Root;
    Value.FTreeView := Self;
  end else
    if FComboBox <> nil then
      FComboBox.FTreeView := nil;

  if FComboBox <> nil then
    FComboBox.FreeNotification(Self);
  FComboBox := Value;
end;

procedure TscCustomShellTreeView.SetListView(const Value: TscCustomShellListView);
begin
  if Value = FListView then Exit;
  if Value <> nil then
  begin
    Value.Root := Root;
    Value.FTreeView := Self;
  end else
    if FListView <> nil then
      FListView.FTreeView := nil;

  if FListView <> nil then
    FListView.FreeNotification(Self);
  FListView := Value;
end;

procedure TscCustomShellTreeView.SetAutoRefresh(const Value: boolean);
begin
  FAutoRefresh := Value;
  if not (csLoading in ComponentState) then
  begin
    if FAutoRefresh then
    begin
      if Assigned(FNotifier) then
        FreeAndNil(FNotifier);
      FNotifier := TscShellChangeNotifier.Create(Self);
      FNotifier.FComponentStyle := FNotifier.FComponentStyle + [ csSubComponent ];
      FNotifier.WatchSubTree := False;
      if Assigned(FNodeToMonitor) then
        FNotifier.Root := TscShellFolder(FNodeToMonitor.Data).PathName
      else
        FNotifier.Root := FRootFolder.PathName;
      FNotifier.OnChange := Self.RefreshEvent;
    end
    else if Assigned(FNotifier) then
      FreeAndNil(FNotifier);
  end;
end;

constructor TscCustomShellComboBox.Create(AOwner: TComponent);
var
  FileInfo: TSHFileInfo;
begin
  inherited Create(AOwner);
  FRootFolder := nil;
  FImages := SHGetFileInfo('C:\',
    0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  ImageList_GetIconSize(FImages, FImageWidth, FImageHeight);
  FUpdating := False;
  DropDownCount := 20;
  FObjectTypes := [otFolders];
  FRoot := SRFDesktop;
  FUseShellImages := True;
end;

procedure TscCustomShellComboBox.WMCommand(var Message: TWMCommand);
begin
  inherited;
  case Message.NotifyCode of
     CBN_CLOSEUP:
      begin
        Change;
      end;
  end;
end;

procedure TscCustomShellComboBox.ClearItems;
var
  I: Integer;
begin
  ItemsEx.BeginUpdate;
  try
    for I := 0 to ItemsEx.Count-1 do
    begin
      if Assigned(Folders[I]) then
        Folders[I].Free;
      ItemsEx[I].Data := nil;
    end;
    ItemsEx.Clear;
  finally
    ItemsEx.EndUpdate;
  end;
end;

procedure TscCustomShellComboBox.CreateRoot;
var
  AFolder: TscShellFolder;
  Text: string;
  ImageIndex: integer;
begin
  if (csLoading in ComponentState) then Exit;
  ItemsEx.BeginUpdate;
  try
    ClearItems;
    FRootFolder := CreateRootFolder(FRootFolder, FOldRoot, FRoot);
    AFolder := TscShellFolder.Create(nil,
                              FRootFolder.AbsoluteID,
                              FRootFolder.ShellFolder);
    Text := AFolder.DisplayName;

    ImageIndex := GetShellImageIndex(AFolder);
    ItemsEx.AddItem(Text, ImageIndex, ImageIndex,
      -1, 0, AFolder);
    Init;
    ItemIndex := 0;
    if FUseShellImages then
    begin
      SetUseShellImages(False);
      SetUseShellImages(True);
    end;
  finally
    ItemsEx.EndUpdate;
  end;
end;

procedure TscCustomShellComboBox.CreateWnd;
begin
  inherited CreateWnd;

  if Style <> csExDropDownList then
    Style := csExDropDownList;
    
  if FImages <> 0 then
    SendMessage(Handle, CBEM_SETIMAGELIST, 0, FImages);
  SetUseShellImages(FUseShellImages);

  if ItemsEx.Count = 0 then
    CreateRoot;
end;

procedure TscCustomShellComboBox.SetObjectTypes(Value: TscShellObjectTypes);
begin
  FObjectTypes := Value;
  RootChanged;
end;

procedure TscCustomShellComboBox.TreeUpdate(NewPath: PItemIDList);
begin
  if FUpdating or ((ItemIndex > -1)
    and SamePIDL(Folders[ItemIndex].AbsoluteID, NewPath)) then Exit;
  FUpdating := True;
  try
    SetPathFromID(NewPath);
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  finally
    FUpdating := False;
  end;
end;

procedure TscCustomShellComboBox.SetTreeView(Value: TscCustomShellTreeView);
begin
  if Value = FTreeView then Exit;
  if Value <> nil then
  begin
    Value.Root := Root;
    Value.FComboBox := Self;
  end else
    if FTreeView <> nil then
      FTreeView.FComboBox := nil;

  if FTreeView <> nil then
    FTreeView.FreeNotification(Self);
  FTreeView := Value;
end;

procedure TscCustomShellComboBox.SetListView(Value: TscCustomShellListView);
begin
  if Value = FListView then Exit;
  if Value <> nil then
  begin
    Value.Root := Root;
    Value.FComboBox := Self;
  end else
    if FListView <> nil then
      FListView.FComboBox := nil;

  if FListView <> nil then
    FListView.FreeNotification(Self);
  FListView := Value;
end;

procedure TscCustomShellComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FTreeView) then
      FTreeView := nil
    else if (AComponent = FListView) then
      FListView := nil
    else if (AComponent = FImageList) then
      FImageList := nil;
  end;
end;

function TscCustomShellComboBox.GetFolder(Index: Integer): TscShellFolder;
begin
  if Index > ItemsEx.Count - 1 then
    Index := ItemsEx.Count - 1;
  Result := TscShellFolder(ItemsEx[Index].Data);
end;

function TscCustomShellComboBox.InitItem(ParentFolder: TscShellFolder; ID: PItemIDList): TscShellFolder;
var
  SF: IShellFolder;
begin
  SF := GetIShellFolder(ParentFolder.ShellFolder, ID);
  Result := TscShellFolder.Create(ParentFolder, ID, SF);
end;

var
  CompareFolder: TscShellFolder = nil;

function ComboSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if CompareFolder = nil then Exit;
  Result := SmallInt(CompareFolder.ShellFolder.CompareIDs(0,
    PItemIDList(Item1), PItemIDList(Item2)));
end;

procedure TscCustomShellComboBox.AddItems(Index: Integer; ParentFolder: TscShellFolder);
var
  EnumList: IEnumIDList;
  ID: PItemIDList;
  ImageIndex: integer;
  Item: TComboExItem;
  NumIDs: integer;
  List: TList;
  ItemText: string;
  AFolder: TscShellFolder;
begin
  OLECheck(ParentFolder.ShellFolder.EnumObjects(0, ObjectFlags(FObjectTypes), EnumList));
  CompareFolder := ParentFolder;
  List := nil;
  ItemsEx.BeginUpdate;
  try
    List := TList.Create;
    while EnumList.Next(1, ID, LongWord(NumIDs)) = S_OK do
      List.Add(ID);
    List.Sort(ComboSortFunc);

    for NumIDs := 0 to List.Count-1 do
    begin
      AFolder := InitItem(ParentFolder, List[NumIDs]);
      ItemText := AFolder.DisplayName;
      Item := ItemsEx.Insert(NumIDs+1);
      Item.Caption := ItemText;
      Item.Data := AFolder;
      Item.Indent := AFolder.Level;
      Item.ImageIndex := GetShellImageIndex(AFolder);
      Item.SelectedImageIndex := Item.ImageIndex;
      if Assigned(FOnGetImageIndex) and (Assigned(FImageList) or FUseShellImages) then
      begin
        ImageIndex := ItemsEx[NumIDs+1].ImageIndex;
        FOnGetImageIndex(Self, NumIDs+1, ImageIndex);
        ItemsEx[NumIDs+1].ImageIndex := ImageIndex;
      end;
    end;

  finally
    CompareFolder := nil;
    List.Free;
    ItemsEx.EndUpdate;
  end;
end;

procedure TscCustomShellComboBox.Init;
var
  MyComputer: PItemIDList;
  Index: Integer;
begin
  ItemsEx.BeginUpdate;
  try
    AddItems(0, FRootFolder);

    if Root = SRFDesktop then
    begin
      SHGetSpecialFolderLocation(0, CSIDL_DRIVES, MyComputer);
      Index := IndexFromID(MyComputer);
      if Index <> -1 then
        AddItems(Index, Folders[Index]);
    end;
  finally
    ItemsEx.EndUpdate;
  end;
end;

function TscCustomShellComboBox.IndexFromID(AbsoluteID: PItemIDList): Integer;
begin
  Result := ItemsEx.Count-1;
  while Result >= 0 do
  begin
    if DesktopShellFolder.CompareIDs(
      0,
      AbsoluteID,
      Folders[Result].AbsoluteID) = 0 then Exit;
    Dec(Result);
  end;
end;

procedure TscCustomShellComboBox.SetRoot(const Value: TscRoot);
begin
  if not SameText(FRoot, Value) then
  begin
    FOldRoot := FRoot;
    FRoot := Value;
    RootChanged;
  end;
end;

procedure TscCustomShellComboBox.RootChanged;
begin
  FUpdating := True;
  try
    ClearItems;
    CreateRoot;
    if Assigned(FTreeView) then
      FTreeView.SetRoot(FRoot);
    if Assigned(FListView) then
      FListView.SetRoot(FRoot);
  finally
    FUpdating := False;
  end;
end;

function TscCustomShellComboBox.GetPath: string;
var
  Folder: TscShellFolder;
begin
  Result := '';
  if ItemIndex > -1 then
  begin
    Folder := Folders[ItemIndex];
    if Assigned(Folder) then
      Result := Folder.PathName
    else
      Result := '';
  end;
end;

procedure TscCustomShellComboBox.SetPath(const Value: string);
var
  P: PWideChar;
  NewPIDL: PItemIDList;
  Flags,
  NumChars: LongWord;
begin
  NumChars := Length(Value);
  Flags := 0;
  P := StringToOleStr(Value);
  try
    OLECheck(DesktopShellFolder.ParseDisplayName(
        0,
        nil,
        P,
        NumChars,
        NewPIDL,
        Flags)
     );
    SetPathFromID(NewPIDL);
  except on EOleSysError do
    raise EInvalidPath.CreateFmt(SErrorSettingPath, [Value]);
  end;
end;

procedure TscCustomShellComboBox.SetPathFromID(ID: PItemIDList);
var
  Pidls: TList;
  I, Item, Temp: Integer;
  AFolder: TscShellFolder;
  RelID: PItemIDList;

  procedure InsertItemObject(Position: integer; Text: string; AFolder: TscShellFolder);

  var
    Item: TComboExItem;
  begin
    Item := ItemsEx.Insert(Position);
    Item.Caption := Text;
    Item.Indent := AFolder.Level;
    Item.Data := AFolder;
    if AFolder = nil then
      Item.Data := AFolder;
    Item.ImageIndex := GetShellImageIndex(AFolder);
  end;

begin
  Item := -1;
  BeginUpdate;
  ItemsEx.BeginUpdate;
  try
    CreateRoot;
    Pidls := CreatePIDLList(ID);
    try
      I := Pidls.Count-1;
      while I >= 0 do
      begin
        Item := IndexFromID(Pidls[I]);
        if Item <> -1 then Break;
        Dec(I);
      end;

      if I < 0 then Exit;

      while I < Pidls.Count-1 do
      begin
        Inc(I);
        RelID := RelativeFromAbsolute(Pidls[I]);
        AFolder := InitItem(Folders[Item], RelID);
        InsertItemObject(Item+1, AFolder.DisplayName, AFolder);
        Inc(Item);
      end;

      Temp := IndexFromID(ID);
      if Temp < 0 then
      begin
        RelID := RelativeFromAbsolute(ID);
        AFolder := InitItem(Folders[Item], RelID);
        Temp := Item + 1;
        InsertItemObject(Item+1, AFolder.DisplayName, AFolder);
      end;
      ItemIndex := Temp;
    finally
      DestroyPIDLList(Pidls);
    end;
  finally
    ItemsEx.EndUpdate;
    EndUpdate;
  end;
end;

function TscCustomShellComboBox.GetShellImageIndex(
  AFolder: TscShellFolder): integer;
begin
  if FUseShellImages then
    Result := AFolder.ImageIndex(False)
  else
    Result := -1;
end;

procedure TscCustomShellComboBox.SetUseShellImages(const Value: Boolean);
var
  ImageListHandle: THandle;
begin
  FUseShellImages := Value;
  if not Assigned(Images) then
    if FUseShellImages then
      ImageListHandle := FImages
    else
      ImageListHandle := 0
  else
    ImageListHandle := Images.Handle;
  SendMessage(Handle, CBEM_SETIMAGELIST, 0, ImageListHandle);

  if FUseShellImages and not Assigned(FImageList) then
    ImageList_GetIconSize(FImages, FImageWidth, FImageHeight)
  else
    if not Assigned(FImageList) then
    begin
      FImageWidth := 16;
      FImageHeight := 16;
    end
    else
    begin
      FImageWidth := FImageList.Width;
      FImageHeight := FImageList.Height;
    end;
end;

destructor TscCustomShellComboBox.Destroy;
var
  I: Integer;
begin
  for I := 0 to ItemsEx.Count - 1 do
  begin
    if Assigned(Folders[I]) then
      Folders[I].Free;
  end;
  if Assigned(FRootFolder) then FRootFolder.Free;
  inherited Destroy;
  if Assigned(FImageList) then FImageList.Free;
end;

procedure TscCustomShellComboBox.Loaded;
begin
  inherited Loaded;
  CreateRoot;
end;

type
  TAccessItemUpdateCount = class(TComboExItems);

procedure TscCustomShellComboBox.Change;
var
  Node: TscShellFolder;
begin
  if TAccessItemUpdateCount(ItemsEx).UpdateCount > 0 then Exit;
  inherited Change;
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  if (ItemIndex > -1) and (not FUpdating) and (not DroppedDown) then
  begin
    FUpdating := True;
    try
      Node := Folders[ItemIndex];
      if Assigned(Node) then
      begin
        if Assigned(FTreeView) then
          FTreeView.SetPathFromID(Node.AbsoluteID);
        if Assigned(FListView) then
          FListView.TreeUpdate(Node.AbsoluteID);
      end;
    finally
      FUpdating := False;
    end;
  end;
end;

procedure TscCustomShellComboBox.Click;
var
  Temp: PItemIDList;
begin
  if DroppedDown then Exit;

  FUpdating := True;
  try
    Temp := CopyPIDL(Folders[ItemIndex].AbsoluteID);
    try
      SetPathFromID(Temp);
      inherited;
    finally
     DisposePIDL(Temp);
    end;
  finally
    FUpdating := False;
  end;
  inherited;
end;

constructor TscThumbnailOptions.Create(AOwner: TPersistent);
begin
  inherited Create;
  FDrawSelectionFrame := True;
  FEnabled := False;
  FWidth := 96;
  FHeight := 96;
end;

procedure TscThumbnailOptions.SetValues(AEnabled: Boolean; AWidth, AHeight: Integer);
begin
  FEnabled := AEnabled;
  FWidth := AWidth;
  FHeight := AHeight;
  DoEnabledChange;
end;

procedure TscThumbnailOptions.Assign(Source: TPersistent);
begin
  if Source is TscThumbnailOptions then
    with TscThumbnailOptions(Source) do
    begin
      Self.FHeight := Height;
      Self.FWidth := Width;
      Self.FEnabled := Enabled;
      Self.FDrawSelectionFrame := DrawSelectionFrame;
    end
  else
    inherited Assign(Source);
end;

procedure TscThumbnailOptions.Changed;
begin
  DoChange;
end;

procedure TscThumbnailOptions.DoChange;
begin
  if Assigned(FOnChange) and FEnabled then
    FOnChange(Self);
end;

procedure TscThumbnailOptions.DoEnabledChange;
begin
  if Assigned(FOnEnabledChange) then
    FOnEnabledChange(Self);
end;

procedure TscThumbnailOptions.SetHeight(const Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TscThumbnailOptions.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    DoEnabledChange;
  end;
end;

procedure TscThumbnailOptions.SetWidth(const Value: Integer);
begin
  if Value <> FWidth then
  begin
    FWidth := Value;
    Changed;
  end;
end;

constructor TscCustomShellListView.Create(AOwner: TComponent);
var
  FileInfo: TSHFileInfo;
begin
  inherited Create(AOwner);
  FStopFetch := False;
  FThumbsThreadList := TList.Create;
  FIsVistaOrLater := CheckWin32Version(6);
  FThumbnailOptions := TscThumbnailOptions.Create(Self);
  FThumbnailOptions.OnChange := OnThumbnailOptionsChange;
  FThumbnailOptions.OnEnabledChange := OnThumbnailOptionsEnabledChange;
  FTempThumbnailImages := TImageList.Create(nil);
  FRootLoaded := False;
  FMask := '*.*|*.*';
  FRootFolder := nil;
  OwnerData := True;
  FSorted := True;
  FObjectTypes := [otFolders, otNonFolders];
  FAutoContext := True;
  FAutoNavigate := True;
  FAutoRefresh := False;
  FFolders := TList.Create;
  FTreeView := nil;
  FUpdating := False;
  FSettingRoot := False;
  FSmallImages := SHGetFileInfo('C:\',
    0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FLargeImages := SHGetFileInfo('C:\',
    0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  FRoot := SRFDesktop;
  HideSelection := False;
  FSortDirection := False;
  FSortColumn := -1;
  FOldSortColumn := -1;
end;

destructor TscCustomShellListView.Destroy;
begin
  ClearThreads;
  FThumbsThreadList.Free;
  if Assigned(FRootFolder) then FRootFolder.Free;
  FThumbnailOptions.Free;
  FTempThumbnailImages.Free;
  ClearItems;
  FFolders.Free;
  inherited;
end;

procedure TscCustomShellListView.ClearThreads;
var
  I: Integer;
begin
  FStopFetch := True;
  if FThumbsThreadList.Count > 0 then
  begin
    for I := 0 to FThumbsThreadList.Count - 1 do
    begin
      TscThumbnailsThread(FThumbsThreadList[I]).Terminate;
      TscThumbnailsThread(FThumbsThreadList[I]).WaitFor;
      TscThumbnailsThread(FThumbsThreadList[I]).Free;
    end;
    FThumbsThreadList.Clear;
  end;
  FStopFetch := False;
end;

function TscCustomShellListView.IsThumbnailView: Boolean;
begin
  Result := (ViewStyle = vsIcon) and FThumbnailOptions.Enabled and not IsWindowsXP;
end;

procedure TscCustomShellListView.OnThumbnailOptionsEnabledChange(Sender: TObject);
begin
  if FThumbnailOptions.Enabled and (ViewStyle <> vsIcon) then
    ViewStyle := vsIcon;
  UpdateThumbnails;
  RootChanged;
end;

procedure TscCustomShellListView.OnThumbnailOptionsChange(Sender: TObject);
begin
  UpdateThumbnails;
  RootChanged;
end;

procedure TscCustomShellListView.HookAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Item.Index > Self.FFolders.Count - 1 then Exit;
  inherited;
end;

procedure TscCustomShellListView.UpdateThumbnail(AFolder: TscShellFolder);
begin
  if AFolder <> nil then
  begin
    if (AFolder.FThumbnailHBitmap > 0) and not (scthmbThumbnailLoaded in AFolder.FThumbnailState)
    then
    begin
      if AFolder.FThumbnail.Width * AFolder.FThumbnail.Height <> 0 then
      begin
        AFolder.FThumbnail.Free;
        AFolder.FThumbnail := TBitmap.Create;
      end;
      AFolder.FThumbnail.PixelFormat := pf32Bit;
      AFolder.FThumbnail.AlphaFormat := afIgnored;
      AFolder.FThumbnail.Handle := AFolder.FThumbnailHBitmap;
      AFolder.FThumbnailHBitmap := 0;
      AFolder.FThumbnail.AlphaFormat := afPremultiplied;
      AFolder.FThumbnailState := AFolder.FThumbnailState + [scthmbThumbnailLoaded];
    end
    else
    if (AFolder.FIconHBitmap > 0) and not (scthmbIconLoaded in AFolder.FThumbnailState) and
       not (scthmbThumbnailLoaded in AFolder.FThumbnailState)  then
    begin
      AFolder.FThumbnail.PixelFormat := pf32Bit;
      AFolder.FThumbnail.AlphaFormat := afIgnored;
      AFolder.FThumbnail.Handle := AFolder.FIconHBitmap;
      AFolder.FIconHBitmap := 0;
      AFolder.FThumbnail.AlphaFormat := afPremultiplied;
      AFolder.FThumbnailState := AFolder.FThumbnailState + [scthmbIconLoaded];
    end;
  end;
end;

procedure TscCustomShellListView.DrawThumbnailImage(AItem: TListItem;
  ACanvas: TCanvas; ARect: TRect);
var
  AFolder: TscShellFolder;
  CX, CY: Integer;
  R: TRect;
begin
  if FStopFetch then Exit;
  if IsWindowsXP then Exit;
  AFolder := Folders[AItem.Index];
  if AFolder <> nil then
  with AFolder do
  begin
    UpdateThumbnail(AFolder);
    if (FThumbnailState <> []) and (FThumbnail <> nil) and
       (FThumbnail.Width * FThumbnail.Width > 0)
    then
    begin
      if AItem.Selected and FThumbnailOptions.DrawSelectionFrame and
        (IsCustomStyle or not StyleServices.Enabled or (SelectionStyle = scstColor)) then
      with ACanvas do
      begin
        Brush.Style := bsClear;
        Pen.Color := scDrawUtils.GetStyleColor(clHighLight);
        Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      end;
      CX := ARect.Left + ARect.Width div 2 - FThumbnail.Width div 2;
      CY := ARect.Top + ARect.Height div 2 - FThumbnail.Height div 2;
      R := Rect(CX, CY, CX + FThumbnail.Width, CY + FThumbnail.Height);
      Bitmap_DrawAlpha(FThumbnail, ACanvas,
        Rect(0, 0, FThumbnail.Width, FThumbnail.Height), R, 255);
    end;
  end;
end;

procedure TscCustomShellListView.UpdateThumbnails;
begin
  if IsThumbnailView then
  begin
    DoubleBuffered := True;
    FIsThumbnailView := True;
    OnDrawItemImage := DrawThumbnailImage;
    FTempThumbnailImages.Width := ThumbnailOptions.Width;
    FTempThumbnailImages.Height := ThumbnailOptions.Height;
    LargeImages := FTempThumbnailImages;
    if DefaultDraw then DefaultDraw := False;
  end
  else
  begin
    DoubleBuffered := not IsWindowsXP;
    FIsThumbnailView := False;
    OnDrawItemImage := nil;
    SmallImages := nil;
    LargeImages := nil;
    if FSmallImages <> 0 then
      SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_SMALL, FSmallImages);
    if FLargeImages <> 0 then
      SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, FLargeImages);
  end;
end;

function TscCustomShellListView.GetPath: String;
begin
  if RootFolder <> nil then
    Result := RootFolder.PathName
  else
    Result := '';
end;

procedure TscCustomShellListView.SetPath(const Value: string);
var
  P: PWideChar;
  NewPIDL: PItemIDList;
  Flags,
  NumChars: LongWord;
begin
  NumChars := Length(Value);
  Flags := 0;
  P := StringToOleStr(Value);
  try
    OLECheck(DesktopShellFolder.ParseDisplayName(
        0,
        nil,
        P,
        NumChars,
        NewPIDL,
        Flags)
     );
    SetPathFromID(NewPIDL);
  except on EOleSysError do
    raise EInvalidPath.CreateFmt(SErrorSettingPath, [Value]);
  end;
end;

function TscCustomShellListView.GetSelectedPath: String;
const
  CSIDL_MYVIDEO = $0e;
  CSIDL_MYPICTURES = $27;
  CSIDL_MYMUSIC = $0d;
var
  FShellFolder: IShellFolder;
  SRet: _STRRET;
  FTempPidl: PItemIDList;
  S: String;
  S1: array[ 0..MAX_PATH] of Char;
begin
  S := '';
  S1 := '';
  FShellFolder := FRootFolder.ParentShellFolder;
  if FShellFolder <> nil then
  begin
    FTempPidl := CopyPidl(RootFolder.FFullPIDL);
    FShellFolder.GetDisplayNameOf(FTempPidl, SHGDN_FORPARSING, SRet);
    DisposePidl(FTempPidl);
    S := SRet.pOleStr;
  end
  else
  begin
    Result := GetPath;
    Exit;
  end;
  if Pos('Documents.library-ms', S) <> 0 then
    SHGetSpecialFolderPath (0, S1, CSIDL_PERSONAL, False)
  else
  if Pos('Videos.library-ms', S) <> 0 then
    SHGetSpecialFolderPath (0, S1, CSIDL_MYVIDEO, False)
  else
  if Pos('Pictures.library-ms', S) <> 0 then
    SHGetSpecialFolderPath (0, S1, CSIDL_MYPICTURES, False)
  else
  if Pos('Music.library-ms', S) <> 0 then
    SHGetSpecialFolderPath (0, S1, CSIDL_MYMUSIC, False);
  if S1 <> '' then
    Result := S1
  else
    Result := S;
  if Pos('::{', Result) <> 0 then Result := '';
end;

procedure TscCustomShellListView.GetSelectedFiles(AFiles: TStrings);
var
  I: Integer;
begin
  AFiles.Clear;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].Selected and not Folders[I].IsFolder
    then
      AFiles.Add(Folders[I].FullObjectName);
  end;
end;

function TscCustomShellListView.GetSelectedFile: String;
begin
  Result := '';
  if (SelectedFolder <> nil) and not SelectedFolder.IsFolder
  then
    Result := SelectedFolder.FullObjectName;
end;


procedure TscCustomShellListView.SetMask;
begin
  if FMask <> Value
  then
    begin
      FMask := Value;
      RootChanged;
    end;
end;

procedure TscCustomShellListView.ClearItems;
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
    Items.Count := 0;
  for I := 0 to FFolders.Count-1 do
    if Assigned(Folders[i]) then
      Folders[I].Free;

  FFolders.Clear;
end;

procedure TscCustomShellListView.CommandCompleted(Verb: String;
  Succeeded: Boolean);
begin
  if not Succeeded then Exit;
  if SameText(Verb, SCmdVerbDelete) or SameText(Verb, SCmdVerbPaste) then
    Refresh
  else if SameText(Verb, SCmdVerbOpen) then
    SetCurrentDirectory(PChar(FSavePath));
end;

procedure TscCustomShellListView.ExecuteCommand(Verb: String;
  var Handled: Boolean);
var
  szPath: array[0..MAX_PATH] of char;
begin
  if SameText(Verb, SCmdVerbRename) then
  begin
    EditText;
    Handled := True;
  end
  else if SameText(Verb, SCmdVerbOpen) then
  begin
    GetCurrentDirectory(MAX_PATH, szPath);
    FSavePath := StrPas(szPath);
    StrPCopy(szPath, ExtractFilePath(Folders[Selected.Index].PathName));
    SetCurrentDirectory(szPath);
  end;
end;

var
  ListViewHandle: Integer;
  CustomSorting: Boolean;
  SortDirection: Boolean;
  SortColumn: Integer;

function ListSortFunc(Item1, Item2: Pointer): Integer;


  function CalcModifiedDate(FindData: TWin32FindData): TDateTime;
  var
    LocalFileTime: TFileTime;
    Age : integer;
  begin
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    if FileTimeToDosDateTime(LocalFileTime, LongRec(Age).Hi,
      LongRec(Age).Lo) then
    begin
      Result := FileDateToDateTime(Age);
      Exit;
    end;
    Result := -1;
  end;

  function CalcFileSize(FindData: TWin32FindData): int64;
  begin
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      Result := FindData.nFileSizeHigh * MAXDWORD + FindData.nFileSizeLow
    else
      Result := -1;
  end;

  procedure GetDetailsOf(AFolder: TscShellFolder; var Details: TWin32FindData);
  var
    szPath: array[ 0 .. MAX_PATH] of char;
    Path: string;
    Handle: THandle;
  begin
    FillChar(Details, SizeOf(Details), 0);
    FillChar(szPath,MAX_PATH,0);
    Path := AFolder.PathName;
    Handle := WinApi.Windows.FindFirstFile(PChar(Path), Details);
    try
      if Handle = INVALID_HANDLE_VALUE then
        NoFolderDetails(AFolder, WinApi.Windows.GetLastError);
    finally
      WinApi.Windows.FindClose(Handle);
    end;
  end;

const
  R: array[Boolean] of Byte = (0, 1);
var
  S1, S2: String;
  Details1: TWin32FindData;
  Details2: TWin32FindData;
  Size1, Size2: Int64;
  DateTime1, DateTime2: TDateTime;
begin
  Result := 0;
  if (Item1 = nil) or (Item2 = nil) then Exit;

  Result := R[TscShellFolder(Item2).IsFolder] - R[TscShellFolder(Item1).IsFolder];
  if (Result = 0) and (TscShellFolder(Item1).ParentShellFolder <> nil) then
    Result := Smallint(
                  TscShellFolder(Item1).ParentShellFolder.CompareIDs(
                  0,
                  TscShellFolder(Item1).RelativeID,
                  TscShellFolder(Item2).RelativeID)
              );

  if CustomSorting
  then
    begin
      if SortColumn <> 0
      then
        begin
          TscShellFolder(Item1).LoadColumnDetails(CompareFolder, ListViewHandle, 4);
          TscShellFolder(Item2).LoadColumnDetails(CompareFolder, ListViewHandle, 4);
        end;
      if (SortColumn = 0) or
         ((SortColumn <> 0) and (Pos(FormatSettings.TimeSeparator, TscShellFolder(Item1).Details[4]) <> 0))
      then
      begin
      if TscShellFolder(Item1).FDetailsEx then
      begin
       case SortColumn of
      0:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
            Result := AnsiCompareText(TscShellFolder(Item1).DisplayName,
                                      TscShellFolder(Item2).DisplayName);
        end;
      2:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
            begin
              if TscShellFolder(Item1).FDetails.Count > 0
              then
                S1 := TscShellFolder(Item1).Details[2]
              else
                S1 := '';
              if TscShellFolder(Item2).FDetails.Count > 0
              then
                S2 := TscShellFolder(Item2).Details[2]
              else
                S2 := '';
              Result := AnsiCompareText(S1, S2);
            end;
        end;
      3:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
          if not TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            begin
              GetDetailsOf(TscShellFolder(Item1), Details1);
              GetDetailsOf(TscShellFolder(Item2), Details2);
              Size1 := CalcFileSize(Details1);
              Size2 := CalcFileSize(Details2);
              if Size1 = Size2 then Result := 0 else
                if Size1 > Size2 then Result := 1 else Result := -1;
            end;
        end;
      1:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
            begin
              GetDetailsOf(TscShellFolder(Item1), Details1);
              GetDetailsOf(TscShellFolder(Item2), Details2);
              DateTime1 := CalcModifiedDate(Details1);
              DateTime2 := CalcModifiedDate(Details2);
              if DateTime1 = DateTime2 then Result := 0 else
                if DateTime1 > DateTime2 then Result := 1 else Result := -1;
            end;
         end;
      end;
      end
      else
      case SortColumn of
      0:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
            Result := AnsiCompareText(TscShellFolder(Item1).DisplayName,
                                      TscShellFolder(Item2).DisplayName);
        end;
      2:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
            begin
              if TscShellFolder(Item1).FDetails.Count > 0
              then
                S1 := TscShellFolder(Item1).Details[2]
              else
                S1 := '';
              if TscShellFolder(Item2).FDetails.Count > 0
              then
                S2 := TscShellFolder(Item2).Details[2]
              else
                S2 := '';
              Result := AnsiCompareText(S1, S2);
            end;
        end;
      1:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
          if not TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            begin
              GetDetailsOf(TscShellFolder(Item1), Details1);
              GetDetailsOf(TscShellFolder(Item2), Details2);
              Size1 := CalcFileSize(Details1);
              Size2 := CalcFileSize(Details2);
              if Size1 = Size2 then Result := 0 else
                if Size1 > Size2 then Result := 1 else Result := -1;
            end;
        end;
      3:
        begin
          if TscShellFolder(Item1).IsFolder and not TscShellFolder(Item2).IsFolder
          then
            Result := -1
          else
          if TscShellFolder(Item2).IsFolder and not TscShellFolder(Item1).IsFolder
          then
            Result := 1
          else
            begin
              GetDetailsOf(TscShellFolder(Item1), Details1);
              GetDetailsOf(TscShellFolder(Item2), Details2);
              DateTime1 := CalcModifiedDate(Details1);
              DateTime2 := CalcModifiedDate(Details2);
              if DateTime1 = DateTime2 then Result := 0 else
                if DateTime1 > DateTime2 then Result := 1 else Result := -1;
            end;
         end;
      end;
      end;
      if SortDirection then Result := -Result;
    end;
end;

procedure  TscCustomShellListView.ColClick(Column: TListColumn);
begin
  inherited;
  if (Columns.Count < 4) and (Column.Index <> 0) then Exit;
  FOldSortColumn := FSortColumn;
  FSortColumn := Column.Index;
  if (FOldSortColumn = FSortColumn) and (FOldSortColumn <> -1)
  then FSortDirection := not FSortDirection
  else FSortDirection := False;
  CustomSorting := True;
  CompareFolder := FRootFolder;
  ListViewHandle := Self.Handle;
  SortColumn := FSortColumn;
  SortDirection := FSortDirection;
  Items.BeginUpdate;
  try
    FFolders.Sort(@ListSortFunc);
  finally
    Items.EndUpdate;
    CompareFolder := nil;
    CustomSorting := False;
  end;
end;

procedure TscCustomShellListView.CreateWnd;
begin
  inherited CreateWnd;
  if HandleAllocated then
    UpdateThumbnails;
  if not FRootLoaded and (Items.Count = 0) then
  begin
    CreateRoot;
  end;
  RootChanged;
end;

procedure TscCustomShellListView.SetObjectTypes(Value: TscShellObjectTypes);
begin
  FObjectTypes := Value;
  if not (csLoading in ComponentState) then
    RootChanged;
end;

procedure TscCustomShellListView.RootChanged;
var
  StayFresh: boolean;
begin
  if FUpdating then Exit;

  FUpdating := True;
  try
    StayFresh := FAutoRefresh;
    AutoRefresh := False;
    SynchPaths;
    Populate;
    if ViewStyle = vsReport then EnumColumns;
    AutoRefresh := StayFresh;
  finally
    FUpdating := False;
  end;
end;

procedure TscCustomShellListView.Populate;
var
  ID: PItemIDList;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  SaveCursor: TCursor;
  HR: HResult;
  CanAdd: Boolean;
  NewFolder: IShellFolder;
  Count: Integer;
  AFolder: TscShellFolder;
  S: String;
  W: Integer;
  R: TRect;
  I: Integer;
  FThumbsThread: TscThumbnailsThread;
  FThreadCount: Integer;

procedure CheckFile;
var
  TempMask: String;
  I: Integer;
  S: String;
begin
  TempMask := '';
  CanAdd := False;
  for I := 1 to Length(FMask) do
  begin
    if (FMask[I] = ';') or (FMask[I] = '|') or (I = Length(FMask))
    then
      begin
        if (I = Length(FMask)) and (FMask[I] <> ';') and (FMask[I] <> '|')
        then
          TempMask := TempMask + FMask[I];
        S := ExtractFileName(AFolder.FullObjectName);
        CanAdd := ((Pos('.', TempMask) <> 0) and MatchesMask(S, TempMask))
                  or (TempMask = '*');
        if CanAdd then Exit else TempMask := '';
      end
    else
      begin
        TempMask := TempMask + FMask[I];
      end;
  end;
end;

begin
  if (csLoading in ComponentState) and not HandleAllocated then Exit;

  ClearThreads;

  S := '';
  Items.BeginUpdate;
  try
    ClearItems;
    SendMessage(Handle, WM_VSCROLL, MakeWParam(SB_LEFT, 0), 0);
    SendMessage(Handle, WM_HSCROLL, MakeWParam(SB_LEFT, 0), 0);
    Count := 0;
    SaveCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourglass;
      HR := FRootFolder.ShellFolder.EnumObjects(Application.Handle,
         ObjectFlags(FObjectTypes), EnumList);

      if HR <> 0 then Exit;

      while EnumList.Next(1, ID, NumIDs) = S_OK do
      begin
        NewFolder := GetIShellFolder(FRootFolder.ShellFolder, ID);

        AFolder := TscShellFolder.Create(FRootFolder, ID, NewFolder);

        CanAdd := True;

        if not AFolder.IsFolder then CheckFile;

        if Assigned(FOnAddFolder) then FOnAddFolder(Self, AFolder, CanAdd);

        if CanAdd then
        begin
          Inc(Count);
          FFolders.Add(AFolder);
          if Length(S) < Length(AFolder.DisplayName)
          then
            S := AFolder.DisplayName;
        end else
          AFolder.Free;
      end;
      Items.Count := Count;
      if FSorted then
      begin
        CompareFolder := FRootFolder;
        try
          CustomSorting := False;
          FFolders.Sort(@ListSortFunc);
        finally
          CompareFolder := nil;
        end;
      end;
    finally
      Screen.Cursor := SaveCursor;
    end;
  finally
    Items.EndUpdate;
    if HandleAllocated and IsCustomStyle then
      RedrawWindow(Handle, nil, 0, RDW_FRAME);
  end;

  if (ViewStyle = vsList) and (S <> '')
  then
    begin
      Canvas.Font := Font;
      R := Rect(0, 0, 0, 0);
      DrawText(Canvas.Handle, PChar(S), Length(S), R,
               DT_CALCRECT or DT_LEFT);
      W := (R.Right - R.Left) + 32;
      ListView_SetColumnWidth(Handle, 0, W);
    end;

   if IsThumbnailView and (FFolders.Count > 0) then
   begin
    FThreadCount := GetThreadCount;
     for I := 0 to FThreadCount - 1 do
     begin
       FThumbsThread := TscThumbnailsThread.CreateEx(Self, I, FThreadCount, False);
       FThumbsThreadList.Add(FThumbsThread);
       FThumbsThread.Start;
     end;
     for I := 0 to FThreadCount - 1 do
     begin
       FThumbsThread := TscThumbnailsThread.CreateEx(Self, I, FThreadCount, True);
       FThumbsThreadList.Add(FThumbsThread);
       FThumbsThread.Start;
     end;
   end;
 end;

function TscCustomShellListView.GetThreadCount: Integer;
begin
  Result := 3;
  if Result > FFolders.Count then
    Result := FFolders.Count;
end;

procedure TscCustomShellListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FTreeView) then
      FTreeView := nil else
    if (AComponent = FComboBox) then
      FComboBox := nil;
  end;
end;

procedure TscCustomShellListView.DblClick;
begin
  if FAutoNavigate and (Selected <> nil) then
    with Folders[Selected.Index] do
      if IsFolder then
        SetPathFromID(AbsoluteID)
      else
      if FAutoExecute then
        ShellExecute(Handle, nil, PChar(PathName), nil, PChar(ExtractFilePath(PathName)), SW_SHOW);
  inherited DblClick;
end;

procedure TscCustomShellListView.EditText;
begin
  if Selected <> nil then
    ListView_EditLabel(Handle, Selected.Index);
end;

procedure TscCustomShellListView.Edit(const Item: TLVItem);
var
  S: string;
begin
  with Item do
  begin
    if iItem >= FFolders.Count then Exit;
    if pszText <> nil then
    begin
      S := pszText;
      TscShellFolder(FFolders[iItem]).Rename(S);
      ListView_RedrawItems(Handle, iItem, iItem);
      if Assigned(FTreeView) and (FTreeView.Selected <> nil)
      then
        FTreeView.Refresh(FTreeView.Selected);
    end;
  end;
end;

procedure TscCustomShellListView.SetAutoRefresh(const Value: Boolean);
begin
  FAutoRefresh := Value;
  if not (csLoading in ComponentState) then
  begin
    if FAutoRefresh then
    begin
      if Assigned(FNotifier) then
        FreeAndNil(FNotifier);
      FNotifier := TscShellChangeNotifier.Create(Self);
      FNotifier.FComponentStyle := FNotifier.FComponentStyle + [ csSubComponent ];
      FNotifier.WatchSubTree := False;
      FNotifier.Root := FRootFolder.PathName;
      FNotifier.OnChange := Self.Refresh;
    end
    else if Assigned(FNotifier) then
      FreeAndNil(FNotifier);
  end;
end;

procedure TscCustomShellListView.SetRoot(const Value: TscRoot);
begin
  if not SameText(Value, FRoot) then
  begin
    FOldRoot := FRoot;
    FRoot := Value;
    CreateRoot;
    FSettingRoot := True;
    RootChanged;
  end;
end;

function TscCustomShellListView.SelectedFolder: TscShellFolder;
begin
  Result := nil;
  if Selected <> nil then Result := Folders[Selected.Index];
end;

function TscCustomShellListView.OwnerDataFetch(Item: TListItem;
  Request: TItemRequest): Boolean;
var
  AFolder: TscShellFolder;
  J: Integer;
begin
  if FStopFetch or (Item.Index < 0) or (Item.Index > Items.Count - 1) or
     (Item.Index > FFolders.Count - 1) or (Item.Index < 0) then
  begin
    Result := False;
    Exit;
  end;
  AFolder := Folders[Item.Index];
  if not Assigned(AFolder) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  Item.Caption := AFolder.DisplayName;

   if not IsThumbnailView then
     Item.ImageIndex := AFolder.ImageIndex(ViewStyle = vsIcon);

  if ViewStyle <> vsReport then Exit;

  AFolder.LoadColumnDetails(FRootFolder, Self.Handle, Columns.Count);
  for J := 1 to Columns.Count - 1 do
    Item.SubItems.Add(AFolder.Details[J]);
end;

function TscCustomShellListView.GetFolder(Index: Integer): TscShellFolder;
begin
  if FFolders.Count = 0
  then
    Result := nil
  else
    Result := TscShellFolder(FFolders[Index]);
end;

function TscCustomShellListView.OwnerDataFind(Find: TItemFind;
  const FindString: string; const FindPosition: TPoint; FindData: Pointer;
  StartIndex: Integer; Direction: TSearchDirection;
  Wrap: Boolean): Integer;
var
  I: Integer;
  Found: Boolean;
begin
  Result := -1;
  I := StartIndex;
  if I <> -1 then
  if (Find = ifExactString) or (Find = ifPartialString) then
  begin
    repeat
      if (I = FFolders.Count) then
        if Wrap then I := 0 else Exit;
       if (I <= FFolders.Count - 1) and (I >= 0)
       then
         Found := Pos(UpperCase(FindString), UpperCase(Folders[I].DisplayName)) = 1
       else
         Found := False;
      Inc(I);
    until Found or (I = StartIndex) or (I > FFolders.Count);
    if Found then Result := I-1;
  end;
  if Result > FFolders.Count - 1 then Result := -1;
  if Result < 0 then Result := -1
end;

procedure TscCustomShellListView.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    Populate;
  end;
end;

procedure TscCustomShellListView.Loaded;
begin
  inherited Loaded;
  if not IsThumbnailView then Populate;
  if csLoading in ComponentState then
    inherited Loaded;
  SetAutoRefresh(FAutoRefresh);
end;

procedure TscCustomShellListView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  if AutoContextMenus and (SelectedFolder <> nil) then
  begin
    InvokeContextMenu(Self, SelectedFolder, MousePos.X, MousePos.Y);
    Handled := True;
    Refresh;
  end else
    inherited;
end;

procedure TscCustomShellListView.Back;
var
  RootPIDL: PItemIDList;
begin
  RootPIDL := CopyPIDL(FRootFolder.AbsoluteID);
  try
    StripLastID(RootPIDL);
    SetPathFromID(RootPIDL);
  finally
    DisposePIDL(RootPIDL);
  end;
end;

procedure TscCustomShellListView.EnumColumns;

var
  ColNames: TStringList;

  procedure UpdateColumn(AIndex: Integer; SD: TShellDetails);
  var
    ColName: string;
    PIDL: PItemIDList;
  begin
    PIDL := nil;
    ColName := StrRetToString(PIDL, SD.Str);
    with Columns[AIndex] do
    begin
      Caption := ColName;
      case SD.fmt of
         LVCFMT_CENTER: Alignment := taCenter;
         LVCFMT_LEFT: Alignment := taLeftJustify;
         LVCFMT_RIGHT: Alignment := taRightJustify;
       end;
      if AIndex = 0 then
        Width := SD.cxChar * Canvas.TextWidth('A1')
      else
        Width := SD.cxChar * Canvas.TextWidth('W');
    end;
  end;

  function AddColumn(SD: TShellDetails) : boolean;
  var
    PIDL: PItemIDList;
    ColName: string;

    function ColumnIsUnique(const Name: string): boolean;
    var
      i : integer;
    begin
      for i := 0 to ColNames.Count - 1 do
        if SameText(ColNames[i], Name) then
        begin
          Result := False;
          exit;
        end;
      Result := True;
    end;

  begin
    PIDL := nil;
    ColName := StrRetToString(PIDL, SD.Str);
    if ColName <> '' then
    begin
      Result := ColumnIsUnique(ColName);
      if Result then
        with Columns.Add do
        begin
          Caption := ColName;
          case SD.fmt of
            LVCFMT_CENTER: Alignment := taCenter;
            LVCFMT_LEFT: Alignment := taLeftJustify;
            LVCFMT_RIGHT: Alignment := taRightJustify;
          end;
          if Columns.Count = 1 then
            Width := SD.cxChar * Canvas.TextWidth('A1')
          else
            Width := SD.cxChar * Canvas.TextWidth('W');
          ColNames.Add(ColName);
        end;
    end
    else
      Result := True;
  end;

  procedure AddDefaultColumn(const ACaption: string; const AAlignment: TAlignment;
    AWidth: integer);
  begin
    with Columns.Add do
    begin
      Caption := ACaption;
      Alignment := AAlignment;
      if Columns.Count = 1 then
       Width := AWidth * Canvas.TextWidth('A1')
      else
       Width := AWidth * Canvas.TextWidth('W');
    end;
  end;

  procedure AddDefaultColumns(const ColCount: integer = 1);
  begin
    if ColCount > 0 then
      AddDefaultColumn(SShellDefaultNameStr, taLeftJustify, 25);
    if ColCount > 1 then
      AddDefaultColumn(SShellDefaultSizeStr, taRightJustify, 10);
    if ColCount > 2 then
      AddDefaultColumn(SShellDefaultTypeStr, taLeftJustify, 10);
    if ColCount > 3 then
      AddDefaultColumn(SShellDefaultModifiedStr, taLeftJustify, 14);
  end;


var
  Col, I: Integer;
  SD: TShellDetails;
  PIDL: PItemIDList;
  SF2: IShellFolder2;
  ISD: IShellDetails;
  ColFlags: LongWord;
  Default: Boolean;
  UpdateColums: Boolean;
begin
  if (not Assigned(FRootFolder)) or (not Assigned(FRootFolder.ShellFolder)) then Exit;

  ColNames := TStringList.Create;
  try
    Columns.BeginUpdate;
    try
      Col := 0;
      PIDL := nil;
      Default := False;
      FillChar(SD, SizeOf(SD), 0);
      FRootFolder.ViewHandle := Self.Handle;
      SF2 := FRootFolder.ShellFolder2;
      UpdateColums := True;
      if Assigned(SF2) then
      begin
        I := 0;
        while SF2.GetDetailsOf(PIDL, Col, SD) = S_OK do
        begin
          SF2.GetDefaultColumnState(Col, ColFlags);
          Default := Default or Boolean(ColFlags and SHCOLSTATE_ONBYDEFAULT);
          if Default and not Boolean(ColFlags and SHCOLSTATE_ONBYDEFAULT) then Break;
          Inc(I);
          Inc(Col);
        end;
        if I <> Columns.Count then
          Columns.Clear
        else
          UpdateColums := False;
      end
      else
        Columns.Clear;
      Col := 0;
      if Assigned(SF2) then
      begin
        while SF2.GetDetailsOf(PIDL, Col, SD) = S_OK do
        begin
          SF2.GetDefaultColumnState(Col, ColFlags);
          Default := Default or Boolean(ColFlags and SHCOLSTATE_ONBYDEFAULT);
          if Default and not Boolean(ColFlags and SHCOLSTATE_ONBYDEFAULT) then Exit;
          if UpdateColums then
            AddColumn(SD)
          else
            UpdateColumn(Col, SD);
          Inc(Col);
        end;
      end
      else
      begin
        ISD := FRootFolder.ShellDetails;
        if Assigned(ISD) then
        begin
          while (ISD.GetDetailsOf(nil, Col, SD) = S_OK) do
          begin
            if (AddColumn(SD)) then
              Inc(Col)
            else
              Break;
          end;
        end
        else
        begin
          if (fpFileSystem in FRootFolder.Properties) then
            AddDefaultColumns(4)
          else
            AddDefaultColumns(1);
        end;
      end;

    finally
      Columns.EndUpdate;
    end;
  finally
    ColNames.Free;
  end;
end;

procedure TscCustomShellListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FAutoNavigate then
    case Key of
      VK_RETURN:
        if ssAlt in Shift then
        begin
          DoContextMenuVerb(SelectedFolder, cmvProperties);
          Key := 0;
        end
        else if (SelectedFolder <> nil) then
          if SelectedFolder.IsFolder then
          begin
            SetPathFromID(SelectedFolder.AbsoluteID);
          end;
      VK_BACK: if not IsEditing then Back;
      VK_F5: Refresh;
    end;
end;

procedure TscCustomShellListView.SetViewStyle(Value: TViewStyle);
begin
  inherited;
  if (Value = vsReport) then EnumColumns;
end;

procedure TscCustomShellListView.SetComboBox(Value: TscCustomShellComboBox);
begin
  if Value = FComboBox then Exit;
  if Value <> nil then
  begin
    Value.Root := Root;
    Value.FListView := Self;
  end else
    if FComboBox <> nil then
      FComboBox.FListView := nil;

  if FComboBox <> nil then
    FComboBox.FreeNotification(Self);
  FComboBox := Value;
end;

procedure TscCustomShellListView.SetTreeView(Value: TscCustomShellTreeView);
begin
  if Value = FTreeView then Exit;
  if Value <> nil then
  begin
    Value.Root := Root;
    Value.FListView := Self;
  end else
    if FTreeView <> nil then
      FTreeView.FListView := nil;

  if FTreeView <> nil then
    FTreeView.FreeNotification(Self);
  FTreeView := Value;
end;

procedure TscCustomShellListView.TreeUpdate(NewRoot: PItemIDList);
begin
  if FUpdating or (Assigned(FRootFolder)
    and SamePIDL(FRootFolder.AbsoluteID, NewRoot)) then Exit;
  SetPathFromID(NewRoot);
end;

procedure TscCustomShellListView.WndProc(var Message: TMessage);
begin
  with Message do
  begin
    if ((Msg = WM_INITMENUPOPUP) or (Msg = WM_DRAWITEM) or (Msg = WM_MENUCHAR)
    or (Msg = WM_MEASUREITEM)) and Assigned(ICM2) then
    begin
      ICM2.HandleMenuMsg(Msg, wParam, lParam);
      Result := 0;
    end;
  end;
  inherited;
end;

procedure TscCustomShellListView.Refresh;
var
  SelectedIndex: Integer;
  RootPIDL: PItemIDList;
begin
  SelectedIndex := -1;
  if Selected <> nil then SelectedIndex := Selected.Index;
  Selected := nil;
  RootPIDL := CopyPIDL(FRootFolder.AbsoluteID);
  try
    FreeAndNil(FRootFolder);
    SetPathFromID(RootPIDL);
  finally
    DisposePIDL(RootPIDL);
  end;
  if (SelectedIndex > -1) and (SelectedIndex < Items.Count - 1) then
    Selected := Items[SelectedIndex];
end;

procedure TscCustomShellListView.SetPathFromID(ID: PItemIDList);
begin
  if FUpdating then Exit;

  if Assigned(FRootFolder) then
    if SamePIDL(FRootFolder.AbsoluteID, ID) then
      Exit
    else
      FRootFolder.Free;

  FSettingRoot := False;
  FRootFolder := CreateRootFromPIDL(ID);
  RootChanged;
  if Assigned(FOnPathChanged) then FOnPathChanged(Self);
end;

procedure TscCustomShellListView.SetRootDirectly(const Value: TscRoot);
begin
  FOldRoot := FRoot;
  FRoot := Value;
  CreateRoot;
  FSettingRoot := True;
  RootChanged;
end;

procedure TscCustomShellListView.CreateRoot;
begin
  FRootLoaded := True;
  FRootFolder := CreateRootFolder(FRootFolder, FOldRoot, FRoot);
end;

procedure TscCustomShellListView.SynchPaths;
begin
  try
    if FSettingRoot then
    begin
      if Assigned(FTreeView) then
        FTreeView.SetRoot(FRoot);
    end
    else
    if FRootFolder <> nil then
    begin
      if Assigned(FTreeView) then
        FTreeView.SetPathFromID(FRootFolder.AbsoluteID);
      if Assigned(FComboBox) then
        FComboBox.TreeUpdate(FRootFolder.AbsoluteID);
    end;
  finally
    FSettingRoot := False;
  end;
end;


constructor TscFilterComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := SDefaultFilter;
  MaskList := TStringList.Create;
end;

destructor TscFilterComboBox.Destroy;
begin
  MaskList.Free;
  inherited Destroy;
end;

procedure TscFilterComboBox.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
end;

function TscFilterComboBox.IsFilterStored: Boolean;
begin
  Result := SDefaultFilter <> FFilter;
end;

procedure TscFilterComboBox.SetFilter(const NewFilter: string);
begin
  if AnsiCompareFileName(NewFilter, FFilter) <> 0 then
  begin
    FFilter := NewFilter;
    if HandleAllocated then BuildList;
    Change;
  end;
end;

procedure TscFilterComboBox.Click;
begin
  inherited Click;
  Change;
end;

function TscFilterComboBox.GetMask: string;
begin
  if ItemIndex < 0 then
    ItemIndex := Items.Count - 1;

  if ItemIndex >= 0 then
  begin
     Result := MaskList[ItemIndex];
  end
  else
     Result := '*.*';
end;

procedure TscFilterComboBox.BuildList;
var
  AFilter, MaskName, Mask: string;
  BarPos: Integer;
begin
  Items.Clear;
  MaskList.Clear;
  AFilter := Filter;
  BarPos := AnsiPos('|', AFilter);
  while BarPos <> 0 do
  begin
    MaskName := Copy(AFilter, 1, BarPos - 1);
    Delete(AFilter, 1, BarPos);
    BarPos := AnsiPos('|', AFilter);
    if BarPos > 0 then
    begin
      Mask := Copy(AFilter, 1, BarPos - 1);
      Delete(AFilter, 1, BarPos);
    end
    else
    begin
      Mask := AFilter;
      AFilter := '';
    end;
    Items.Add(MaskName);
    MaskList.Add(Mask);
    BarPos := AnsiPos('|', AFilter);
  end;
  ItemIndex := 0;
end;

function NormalizeStringFormat(S: String): String;
var
  I: Integer;
begin
  Result := S;
  I := Pos('%', S);
  if (I <> 0) and (I + 1 <= Length(S)) then
  begin
    S[I + 1] := 's';
    Result := S;
  end;
end;

var
  Shell32DLL: HMODULE = 0;
  Lib: HModule = 0;

initialization

  Shell32DLL := LoadLibrary('Shell32');
  if Shell32DLL <> 0 then begin
    SC_SHCreateItemFromIDList := GetProcAddress(Shell32DLL, 'SHCreateItemFromIDList');
    SC_SHGetImageList := GetProcAddress(Shell32DLL, 'SHGetImageList');
    SC_SHGetKnownFolderIDList := GetProcAddress(Shell32DLL, 'SHGetKnownFolderIDList');
    SC_S_Create := LoadStringFromDll(Shell32DLL, 30315, SC_S_Create);
  end;

  Lib := LoadLibrary('comdlg32.dll');
  if Lib <> 0 then
  begin
    SC_S_Folder := LoadStringFromDll(Lib, 368, SC_S_Folder);
    SC_S_Save := LoadStringFromDll(Lib, 369, SC_S_Save);
    SC_S_Open := LoadStringFromDll(Lib, 370, SC_S_Open);
    if not IsWindowsXP then
      SC_S_Cancel := LoadStringFromDll(Lib, 372, SC_S_Cancel)
    else
      SC_S_Cancel := SC_S_Msg_Cancel;
    if not IsWindowsXP then
    begin
      SC_S_FileType := LoadStringFromDll(Lib, 432, SC_S_FileType);
      SC_S_FileName := LoadStringFromDll(Lib, 433, SC_S_FileName);
    end
    else
    begin
      SC_S_FileType := LoadStringFromDll(Lib, 412, SC_S_FileType);
      SC_S_FileName := LoadStringFromDll(Lib, 418, SC_S_FileName);
    end;
    SC_S_SelectDir := LoadStringFromDll(Lib, 439, SC_S_SelectDir);
    SC_S_FileOpen := LoadStringFromDll(Lib, 384, SC_S_FileOpen);
    SC_S_FileSave := LoadStringFromDll(Lib, 385, SC_S_FileSave);
    SC_S_GoToLastFolderVisited_Hint := LoadStringFromDll(Lib, 712, SC_S_GoToLastFolderVisited_Hint);
    SC_S_UpOneLevel_Hint := LoadStringFromDll(Lib, 702, SC_S_UpOneLevel_Hint);
    SC_S_CreateNewFolder_Hint := LoadStringFromDll(Lib, 703, SC_S_CreateNewFolder_Hint);
    SC_S_ViewMenu_Hint := LoadStringFromDll(Lib, 711, SC_S_ViewMenu_Hint);
    SC_S_Replace := NormalizeStringFormat(LoadStringFromDll(Lib, 257, SC_S_Replace));

    FreeLibrary(Lib);
  end;  
  
  TCustomStyleEngine.RegisterStyleHook(TscShellComboBox, TscComboBoxExStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscFilterComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscShellListView, TscListViewStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TscShellTreeView, TscTreeViewStyleHook);

  CreateDesktopFolder;
  InitializeCriticalSection(CS);
  OleInitialize(nil);

finalization
  if Shell32DLL > 0 then FreeLibrary(Shell32DLL);

  {$IFNDEF VER230}
  TCustomStyleEngine.UnRegisterStyleHook(TscShellComboBox, TscComboBoxExStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscFilterComboBox, TscComboBoxStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscShellListView, TscListViewStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TscShellTreeView, TscTreeViewStyleHook);
  {$ENDIF}

  if Assigned(DesktopFolder) then
    DesktopFolder.Free;
  DeleteCriticalSection(CS);
  OleUninitialize;
end.
