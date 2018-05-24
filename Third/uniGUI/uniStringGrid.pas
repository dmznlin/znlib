unit uniStringGrid;
{***@@@---------------------------------------------------------------------***}
{               uniGUI Web Application Framework for Delphi                    }
{                                                                              }
{ This source code is copyrighted material.                                    }
{ Copyright (c) FMSoft Co. Ltd. 2009-2017. All rights reserved.                }
{                                                                              }
{ See License.pdf under installation folder for details.                       }
{                                                                              }
{ Developed by: Farshad Mohajeri                                               }
{ Contact: farshad@fmsoft.net - info@fmsoft.net                                }
{ http://www.unigui.com                                                        }
{ http://www.fmsoft.net                                                        }
{------------------------------------------------------------------------------}

interface

uses
  Windows, Controls, SysUtils, Classes, Graphics, Grids, uniGUIConst, uniGUITypes,
  uniGUIInterfaces, uniGUIClasses, uniBasicGrid, ExtPascalUtils,
  uniGUIAbstractClasses;

const
  DEF_COL_WIDTH = 64;

type
  TUniGridColumn = class(TCollectionItem)
  private
    FTitle: TUniColumnTitle;
    FWidth : Integer;
    procedure SetTitle(const Value: TUniColumnTitle);
    procedure SetWidth(const Value: Integer);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Title: TUniColumnTitle read FTitle write SetTitle;
    property Width: Integer read FWidth write SetWidth default DEF_COL_WIDTH;
  end;

  TUniGridColumns = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TUniGridColumn;
    procedure SetItems(Index: Integer; const Value: TUniGridColumn);
  public
    property Items[Index: Integer]: TUniGridColumn read GetItems write SetItems; default;
  end;

  TUniStringGrid = class(TUniBasicGrid, IUniStringGrid)
  private
    FObjectData,
    FColData  : TList;
    FFixedColor : TColor;
    FColumnCount,
    FRowCount,
    FFixedRows,
    FFixedCols : Integer;
    FOptions  : TGridOptions;
    FDefaultColWidth,
    fDefaultRowHeight : Integer;
    FCustomizableCells,
    FConfigChanged, FUpdating, FUpdated,
    FInternalSet: Boolean;
    FColWidths : array of Integer;
    SelectedLine  : Integer;
    FOnDrawCell : TUniDrawCellEvent;
    FCellAttribs : TUniCellAttribs;
    FLockFixedCols: Boolean;
    FShowColumnTitles: Boolean;
    FHeaders: TUniGridColumns;
    procedure SetColumnAttribs(Col: TJSGridColumn; Index:Integer);
    procedure InitRowStrings;

    procedure SetJSCell(ACol, ARow: Integer; const Value: string; const Attr: string='');
    procedure DelJSRow(RowNo:Integer);
    procedure AddJSRow(RowNo:Integer);
    function GetJSONRow(ARowNo: Integer; AObjects: Boolean = False): string;

    procedure ReconfigureColumns;
    function RowIsEmpty(ARow: Integer): Boolean;
    function ProcessCellData(ColNo, RowNo: Integer; var Attr: string; var AValue: string): Boolean;
    procedure ReadColWidths(Reader: TReader);
    function GetColumns: TUniGridColumns;
    procedure SetColumns(const Value: TUniGridColumns);
    function GetGridData(AObjects: Boolean = False): string;
    function GetFieldDefs: string;
    function GetObjects(ACol, ARow: Integer): TObject;
    procedure SetObjects(ACol, ARow: Integer; const Value: TObject);
    procedure InitObjects;
  protected
    function GetCells(ACol, ARow: Integer): string; virtual;
    procedure SetCells(ACol, ARow: Integer; Value: string); virtual;
    function GetColWidths(Index: Integer): Integer; virtual;
    procedure SetColWidths(Index: Integer; Value: Integer); virtual;
    function GetRowHeights(Index: Integer): Integer; virtual;
    procedure SetRowHeights(Index: Integer; Value: Integer); virtual;
    function GetFixedColor: TColor; virtual;
    procedure SetFixedColor(Value: TColor); virtual;
    function GetFixedCols: Integer; virtual;
    procedure SetFixedCols(Value: Integer); virtual;
    function GetFixedRows: Integer; virtual;
    procedure SetFixedRows(Value: Integer); virtual;
    function GetRowCount: Integer; virtual;
    procedure SetRowCount(Value: Integer); virtual;
    function GetColCount: Integer; virtual;
    procedure SetColCount(Value: Integer); virtual;
    function GetDefaultColWidth: Integer; virtual;
    procedure SetDefaultColWidth(Value: Integer); virtual;
    function GetDefaultRowHeight: Integer; virtual;
    procedure SetDefaultRowHeight(Value: Integer); virtual;
    function GetOptions: TGridOptions; virtual;
    procedure SetOptions(Value: TGridOptions); virtual;
    function GetCol: Integer; virtual;
    procedure SetCol(Value: Integer); virtual;
    function GetRow: Integer; virtual;
    procedure SetRow(Value: Integer); virtual;
    function GetOnDrawCell: TUniDrawCellEvent; virtual;
    procedure SetOnDrawCell(Value: TUniDrawCellEvent); virtual;

//    procedure DoCellClick(const ARow: Integer; const AColumn: Integer); override;
    function RowSelect:Boolean; override;
    function CheckBoxSelectCheckOnly: Boolean; override;
    function CheckBoxSelect: Boolean; override;
    function RowMultiSelect:Boolean; override;
    procedure LoadCompleted; override;
    function Clickable(ACol, ARow:Integer):Boolean; override;
    procedure SetCellValue(ACol, ARow:Integer; OldValue, NewValue: Variant; AuxParam: string); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure NotifyAjax; override;

    function DataStoreClassName:string; override;

    procedure WebCreate; override;
    procedure WebDestroy; override;

    function VCLControlClassName: string; override;
  public
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property ColWidths[Index: Longint]: Integer read GetColWidths write SetColWidths;
    property RowHeights[Index: Longint]: Integer read GetRowHeights write SetRowHeights;
    property Col: Integer read GetCol write SetCol;
    property Row: Integer read GetRow write SetRow;

    procedure BeginUpdate; reintroduce;
    procedure EndUpdate; reintroduce;

    procedure RefreshRow(RowNo: Integer=-1);

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
  published
    property FixedColor: TColor read GetFixedColor write SetFixedColor default clBtnFace;
    property FixedCols: Integer read GetFixedCols write SetFixedCols default 1;
    property FixedRows: Integer read GetFixedRows write SetFixedRows default 1;
    property RowCount:Integer read GetRowCount write SetRowCount default 5;
    property ColCount:Integer read GetColCount write SetColCount default 5;
    property DefaultColWidth:Integer read GetDefaultColWidth write SetDefaultColWidth default 64;
    property DefaultRowHeight:Integer read GetDefaultRowHeight write SetDefaultRowHeight default 24;
    property Options: TGridOptions read GetOptions write SetOptions;
    property CustomizableCells: Boolean read FCustomizableCells write FCustomizableCells default True;
    property LockFixedCols: Boolean read FLockFixedCols write FLockFixedCols default False;
    property ShowColumnTitles: Boolean read FShowColumnTitles write FShowColumnTitles default False;
    property Columns: TUniGridColumns read GetColumns write SetColumns;
    property ClientEvents;
    property HeaderTitle;
    property HeaderTitleAlign;
    property Draggable;
    property CellCursor;

    // events
    property OnEndDrag;
    property OnDrawCell: TUniDrawCellEvent read GetOnDrawCell write SetOnDrawCell;
    property OnSelectCell;
    property OnDblClick;
    property OnClick;
    property OnBodyClick;
    property OnBodyDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnAjaxEvent;

    property LayoutConfig;
    property BorderStyle default ubsDefault;
    property TrackOver default False;
    property StripeRows default False;
    property Align;
    property Anchors;
    property TabOrder;
    property TabStop;
    property ParentFont;
    property Font;
    property ScreenMask;
  end;

implementation

uses
  Types, StrUtils, uniStrUtils, UniGUIJSUtils, uniGUIServer, uniGUIFont,
  Variants;

type
  TJSObjectHack = class(TJSObject);

function TUniStringGrid.DataStoreClassName:string;
begin
  Result := 'Ext.data.JsonStore';
end;

function TUniStringGrid.VCLControlClassName: string;
begin
  Result := 'TVCLStringGrid';
end;

constructor TUniStringGrid.Create(AOwner:TComponent);
begin
  inherited;
  FOptions := [goVertLine, goHorzLine];
  FCustomizableCells := True;
  SelectMarkOffest := Point(4, 4);
  DefaultColWidth := DEF_COL_WIDTH;
  DefaultRowHeight := 24;
  ColCount := 5;
  RowCount := 5;
  Width := 320;
  Height := 240;
  FixedRows := 1;
  FixedCols := 1;
  FixedColor := clBtnFace;
  Color := clWindow;
  TrackOver := False;
  StripeRows := False;
  FHeaders := TUniGridColumns.Create(Self, TUniGridColumn);
end;

procedure TUniStringGrid.WebCreate;
begin
  inherited;
  FColData := TList.Create;
  FCellAttribs := TUniCellAttribs.Create(True);
  FCellAttribs.DefaultColor := clWindow;

  JSConfig('storeId', [DataStore.JSName+'_id'], DataStore);
  JSAssign('grid', [JSControl], DataStore);

  JSConfig('store', [DataStore]);
  JSConfigObject('viewConfig', 'markDirty', [False]);

  SelectedLine := -1;
end;

procedure TUniStringGrid.WebDestroy;
var
  I : Integer;
begin
  if Assigned(FObjectData) then
  begin
    for I := 0 to FObjectData.Count - 1 do
      TObject(FObjectData[I]).Free;
    FObjectData.Free;
  end;

  for I := 0 to FColData.Count - 1 do
    TObject(FColData[I]).Free;

  FCellAttribs.Free;
  FColData.Free;
  inherited;
end;

function TUniStringGrid.RowIsEmpty(ARow: Integer): Boolean;
var
  I : Integer;
  Attr, sVal : string;
  Res : Boolean;
begin
  Result := True;
  for I := 0 to ColCount - 1 do
  begin
    sVal := Trim(Cells[I, ARow]);
    Res := ProcessCellData(I, ARow, Attr, sVal);
    if (sVal<>'') or Res then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TUniStringGrid.ProcessCellData(ColNo, RowNo: Integer; var Attr: string; var AValue: string): Boolean;
var
  S : string;
begin
  Attr := '';
  Result := False;
  if Assigned(FOnDrawCell) then
  begin
    FCellAttribs.Clear;
    S := AValue;
    FOnDrawCell(Self, ColNo, RowNo, AValue, FCellAttribs);
    Result := FCellAttribs.Modified;
    if Result then
      Attr := FCellAttribs.AsString
    else
      Result := (S<>AValue);
  end;
end;

function TUniStringGrid.GetJSONRow(ARowNo: Integer; AObjects: Boolean = False): string;
var
  EmptyRow, HasAttr : Boolean;
  Attr, aVal : string;
  J : Integer;
begin
  Result := '';
  EmptyRow := True;
  HasAttr := False;
  for J := 0 to ColCount - 1 do
  begin
    aVal := Cells[J, ARowNo];
    if ProcessCellData(J, ARowNo, Attr, aVal) then HasAttr := True;
    Attr := '"'+Attr+'"';

    if (Trim(aVal)<>'') or (Attr<>'""') then EmptyRow := False;

    if AObjects then
      Result := Result+IntToStr(J)+':'+StrToJS(aVal)+',_'+IntToStr(J)+':'+Attr
    else
      Result := Result+StrToJS(aVal)+','+Attr;

    if J<ColCount-1 then Result := Result+',';
  end;

  if EmptyRow and (not AObjects) then
    Result := '" "' // if not ExtJS will set row height to minimum (only first col of empty rows should be sent)
  else
  begin
    if AObjects then
      Result := '_x:'+IfThen(HasAttr, '1,', '0,')+Result
    else
      Result := IfThen(HasAttr, '1,', '0,')+Result;
  end;
end;

function TUniStringGrid.GetGridData(AObjects: Boolean = False): string;
var
  I : Integer;
begin
  Result := '';
  for I := 0 to RowCount - 1 do
  begin
    if AObjects then
      Result := Result + '{'+GetJSONRow(I, True)+'}'
    else
      Result := Result + '['+GetJSONRow(I)+']';
    if I < RowCount - 1 then Result := Result + ',';
  end;
end;

function TUniStringGrid.GetFieldDefs: string;
var
  I : Integer;
begin
  Result := '"_x"';
  for I := 0 to ColCount - 1 do
    Result := Result + ',"' + IntToStr(I)+'","_' + IntToStr(I)+'"';
end;

procedure TUniStringGrid.LoadCompleted;
var
  S : string;
begin
  if FLockFixedCols then JSConfig('enableLocking', [True]);
  if not FShowColumnTitles then JSConfig('hideHeaders', [True]);
  JSConfig('fxCols', [FixedCols]);
  JSConfig('fxRows', [FixedRows]);
  if FixedColor <> clBtnFace then
    JSConfig('fxColor', [uniColor2Web(FixedColor)]);

  if not (goHorzLine in FOptions) then JSConfig('rowLines', [False]);
  if not (goVertLine in FOptions) then JSConfig('columnLines', [False]);

  JSConfig('fields', [JSArray(GetFieldDefs)], DataStore);
  S := GetGridData;
  if S<>'' then
    JSConfig('data', [JSArray(S)], DataStore);

  FUpdated := False;
  FConfigChanged := False;
  inherited;
  ReconfigureColumns;
end;

procedure TUniStringGrid.NotifyAjax;
begin
  inherited;
  if FConfigChanged then
  begin
    FConfigChanged := False;
    ReconfigureColumns;
  end;

  if FUpdated then
  begin
    FUpdated := False;
    JSCall('model.setFields', [JSArray(GetFieldDefs())], DataStore);
    JSCall('loadData', [JSArray(GetGridData())], DataStore);
  end;
end;

function TUniStringGrid.Clickable(ACol, ARow:Integer):Boolean;
begin
  Result := (ACol >= FFixedCols) and (ARow >= FFixedRows);
end;

procedure TUniStringGrid.SetCellValue(ACol, ARow:Integer; OldValue, NewValue: Variant; AuxParam: string);
var
  Attr, S : string;
begin
  if (ACol<FColumnCount) and  (ARow<FRowCount) then
    try
      FInternalSet := True;
      S := Trim(NewValue);
      Cells[ACol, ARow] := S;
      if ProcessCellData(ACol, ARow, Attr, S) then
        SetJSCell(ACol, ARow, S, Attr);
    finally
      FInternalSet := False;
    end;
end;

function TUniStringGrid.GetCol: Integer;
begin
  Result := CurrCol;
end;

procedure TUniStringGrid.SetCol(Value: Integer);
begin
  CurrCol := Value;
end;

function TUniStringGrid.GetRow: Integer;
begin
  Result := CurrRow;
end;

procedure TUniStringGrid.SetRow(Value: Integer);
begin
  CurrRow := Value;
end;

function TUniStringGrid.GetOptions: TGridOptions;
begin
  Result := FOptions;
end;

procedure TUniStringGrid.SetOptions(Value: TGridOptions);
begin
  if FOptions<>Value then
  begin
    if ((goEditing in (Value - FOptions)) or (goEditing in (FOptions - Value)))
      and (not IsLoading) then
      FConfigChanged := True;

    FOptions := Value;
  end;
end;

function TUniStringGrid.CheckBoxSelectCheckOnly: Boolean;
begin
  Result := False;
end;

procedure TUniStringGrid.BeginUpdate;
begin
  FUpdating := True;
end;

function TUniStringGrid.CheckBoxSelect: Boolean;
begin
  Result := False;
end;

function TUniStringGrid.RowSelect:Boolean;
begin
  Result := goRowSelect in FOptions;
end;

function TUniStringGrid.RowMultiSelect:Boolean;
begin
  Result := goRangeSelect in FOptions;
end;

procedure TUniStringGrid.SetObjects(ACol, ARow: Integer; const Value: TObject);
var
  ColObjects  : TList;
begin
  if (ACol < FColumnCount) and  (ARow < FRowCount) then
  begin
    InitObjects;

    while FObjectData.Count <= ACol do
      FObjectData.Add(TList.Create);

    ColObjects := FObjectData[ACol];

    while ColObjects.Count <= ARow do
      ColObjects.Add(nil);

    ColObjects[ARow] := Value;
  end;
end;

function TUniStringGrid.GetObjects(ACol, ARow: Integer): TObject;
var
  ColObjects  : TList;
begin
  Result := nil;

  if (ACol < FColumnCount) and  (ARow < FRowCount) then
  begin
    InitObjects;
    if ACol < FObjectData.Count then
    begin
      ColObjects := FObjectData[ACol];
      if ARow < ColObjects.Count then
        Result := ColObjects[ARow];
    end;                                  
  end;
end;

function TUniStringGrid.GetCells(ACol, ARow: Integer): string;
var
  ColStrings  : TStringList;
begin
  if ACol < FColData.Count then
  begin
    ColStrings := FColData[ACol];
    Result := ColStrings[ARow];
  end
  else
    Result := '';
end;

procedure TUniStringGrid.SetCells(ACol, ARow: Integer; Value: string);
var
  ColStrings : TStringList;
begin
  if (ACol < FColumnCount) and (ARow < FRowCount) then
  begin
    ColStrings := FColData[ACol];

    if ColStrings[ARow] <> Value then
    begin
      ColStrings[ARow] := Value;
      if (not IsLoading) and (not FInternalSet) then
        SetJSCell(ACol, ARow, Value);
    end;
  end;
end;

procedure TUniStringGrid.SetJSCell(ACol, ARow: Integer; const Value: string; const Attr: string='');
var
  sValue, sAttr : string;
begin
  if not FUpdating then
  begin
    sValue := Value;
    if RowIsEmpty(ARow) then
      JSCode('_srd_('+DataStore.JSName+',"'+IntToStr(ARow)+ '","",null);')
    else
    begin
      sAttr := Attr;
      if sAttr='' then
        ProcessCellData(ACol, ARow, sAttr, sValue);

      JSCode('_srd_('+DataStore.JSName+','+
                      '"'+IntToStr(ARow)+ '",'+
                      '"'+IntToStr(ACol)+ '",'+
                      StrToJS(sValue)+','+
                      '"'+sAttr+'");');

    end;
  end;
end;

function TUniStringGrid.GetColWidths(Index: Longint): Integer;
begin
  Result := FColWidths[Index];
end;

function TUniStringGrid.GetOnDrawCell: TUniDrawCellEvent;
begin
  Result := FOnDrawCell;
end;

procedure TUniStringGrid.SetOnDrawCell(Value: TUniDrawCellEvent);
begin
  FOnDrawCell := Value;
end;

procedure TUniStringGrid.SetColWidths(Index: Longint; Value: Integer);
begin
  if Value<>FColWidths[Index] then
  begin
    FColWidths[Index] := Value;
    if not IsLoading then FConfigChanged := True;

    if Supports(VCLProxy, IUniStringGrid) then
      (VCLProxy as IUniStringGrid).ColWidths[Index] := Value;
  end;
end;

function TUniStringGrid.GetRowHeights(Index: Longint): Integer;
begin
  Result := 0;
//  Result := StrToIntDef(Cell[Index, 0].Height,0);
end;

procedure TUniStringGrid.SetRowHeights(Index: Longint; Value: Integer);
//var
//  I : Integer;
begin
//  for I := 0 to ColCount - 1 do
//    Cell[Index, I].Height := IntToStr(Value);
end;

function TUniStringGrid.GetDefaultColWidth: Integer;
begin
  Result := FDefaultColWidth;
end;

procedure TUniStringGrid.SetDefaultColWidth(Value:Integer);
var
  I : Integer;
begin
  if FDefaultColWidth <> Value then
  begin
    FDefaultColWidth := Value;

    for I := 0 to FColumnCount - 1 do FColWidths[I] := FDefaultColWidth;

    SetVCLProperty('DefaultColWidth', FDefaultColWidth);
  end;
end;

function TUniStringGrid.GetDefaultRowHeight: Integer;
begin
  Result := fDefaultRowHeight;
end;

procedure TUniStringGrid.SetDefaultRowHeight(Value:Integer);
begin
  fDefaultRowHeight := Value;

  SetVCLProperty('DefaultRowHeight', fDefaultRowHeight);
end;

function TUniStringGrid.GetFixedColor: TColor;
begin
  Result := fFixedColor;
end;

procedure TUniStringGrid.SetFixedColor(Value:TColor);
begin
  fFixedColor := Value;

  SetVCLProperty('FixedColor', fFixedColor);
end;

function TUniStringGrid.GetFixedRows: Integer;
begin
  Result := fFixedRows;
end;

function TUniStringGrid.GetColumns: TUniGridColumns;
begin
  Result := FHeaders;
end;

procedure TUniStringGrid.SetFixedRows(Value:Integer);
begin
  fFixedRows := Value;

  if SetVCLProperty('FixedRows', fFixedRows) then Exit;

  if fFixedRows>RowCount then fFixedRows := RowCount;
end;


procedure TUniStringGrid.SetColumns(const Value: TUniGridColumns);
begin
  FHeaders.Assign(Value);
end;

function TUniStringGrid.GetFixedCols: Integer;
begin
  Result := FFixedCols;
end;

procedure TUniStringGrid.SetFixedCols(Value:Integer);
begin
  FFixedCols := Value;

  if SetVCLProperty('FixedCols', FFixedCols) then Exit;

  if FFixedCols>ColCount then FFixedCols := ColCount;
end;

function TUniStringGrid.GetRowCount:Integer;
begin
  Result := FRowCount;
end;

procedure TUniStringGrid.SetRowCount(Value:Integer);
var
  I, pRow  : Integer;
begin
  if (Value<>FRowCount) and (Value>=0) then
  begin
    pRow := FRowCount;
    FRowCount := Value;

    if SetVCLProperty('RowCount', FRowCount) then Exit;

    InitRowStrings;

    if not IsLoading then
    begin
      for I := pRow to FRowCount-1 do
        AddJSRow(I);

      for I := pRow-1 downto FRowCount do
        DelJSRow(I);
    end;
  end;
end;

function TUniStringGrid.GetColCount:Integer;
begin
  Result := FColumnCount;
end;

procedure TUniStringGrid.RefreshRow(RowNo: Integer=-1);
begin
  if WebMode and (not IsLoading) then
  begin
    if RowNo=-1 then RowNo := Row;
    JSCode('var R='#1'.getAt('+IntToStr(RowNo)+'); if (R!=null) {R.data={'+GetJSONRow(RowNo, True)+'};R.commit();};', DataStore);
  end;
end;

procedure TUniStringGrid.SetColumnAttribs(Col: TJSGridColumn; Index:Integer);
var
  S, ed, Attr : string;
  Locked, Editable : Boolean;
begin
  Locked := Index < FixedCols;
  Editable := (goEditing in FOptions) and (not Locked);

  JSConfig('width', [FColWidths[Index]], Col);
  JSConfig('dataIndex', [IntToStr(Index)], Col);

  if Locked then JSConfig('locked', [True], Col);

  if FShowColumnTitles then
  begin
    JSConfig('menuDisabled', [True], Col);
    JSConfig('sortable', [False], Col);
    JSConfig('resizable', [goColSizing in FOptions], Col);

    if FHeaders.Count>Index then
    begin
      S := Trim(FHeaders[Index].Title.Caption);
      if S = '' then S := '&nbsp;';
      JSConfig('text', [S], Col);

      S := GetTitleAttribs(FHeaders[Index].Title, taLeftJustify);
      if S<>'' then
        JSCall('setElProp', [JSObject(S), NULL, 0, NULL, NULL, 'titleEl'], Col);
    end;
  end;

  ed := 'xtype:"textfield"';
  if not Self.Font.IsDefault then
  begin
    FCellAttribs.Font.Assign(Self.Font);
    Attr := FCellAttribs.AsString;
    if (Attr<>'') and (Attr<>'{}') then
      JSConfig('attr', [Attr], Col);
    ed := ed+',fieldStyle:'+StrToJS(Self.Font.ToString(True));
  end;
  if Editable then JSConfig('editor','{'+ed+'}', Col);
  JSConfig('renderer','_rndcll_', Col);
end;

procedure TUniStringGrid.InitObjects;
begin
  if FObjectData = nil then
    FObjectData := TList.Create;
end;

procedure TUniStringGrid.InitRowStrings;
var
  I, J : Integer;
  ColStrings : TStringList;
begin
  for I := 0 to FColumnCount - 1 do
  begin
    ColStrings := FColData[I];
    for J := ColStrings.Count-1 to FRowCount do
      ColStrings.Add('');
  end;
end;

procedure TUniStringGrid.SetColCount(Value:Integer);
var
  I : Integer;
begin
  if Value<>FColumnCount then
  begin
    FColumnCount := Value;

    if Length(FColWidths)<FColumnCount then
      SetLength(FColWidths, FColumnCount);

    if SetVCLProperty('ColCount', FColumnCount) then Exit;

    for I := FColData.Count to FColumnCount-1 do
    begin
      FColData.Add(TStringList.Create);
      FColWidths[I] := FDefaultColWidth;
    end;

    InitRowStrings;
    if not IsLoading then FConfigChanged := True;
  end;
end;

procedure TUniStringGrid.ReconfigureColumns;
var
  I  : Integer;
begin
  JSColumns.ResetColumns;

  for I  := 0 to FColumnCount-1 do
    SetColumnAttribs(JSColumns.AddColumn, I);

  JSColumns.InsertJSCode;
  JSColumns.ReConfigure;
end;

procedure TUniStringGrid.DelJSRow(RowNo:Integer);
begin
  if not FUpdating then
    JSCall('removeAt', [RowNo], DataStore);
end;

destructor TUniStringGrid.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TUniStringGrid.EndUpdate;
begin
  FUpdating := False;
  if not IsLoading then FUpdated := True;
end;

procedure TUniStringGrid.AddJSRow(RowNo:Integer);
var
  I : Integer;
begin
  if not FUpdating then
  begin
    JSCall('add', [JSObject([IntToStr(RowNo), ''])], DataStore);

    if not RowIsEmpty(RowNo) then
    begin
      for I := 0 to ColCount - 1 do
        SetJSCell(I, RowNo, Cells[I, RowNo]);
    end;
  end;
end;

procedure TUniStringGrid.ReadColWidths(Reader: TReader);
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

procedure TUniStringGrid.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  with Filer do
    DefineProperty('ColWidths', ReadColWidths, nil, Length(FColWidths)>0);
end;

{ TUniGridColumn }

constructor TUniGridColumn.Create(Collection: TCollection);
begin
  inherited;
  FTitle := TWebColumnTitle.Create(Collection);
  FWidth := DEF_COL_WIDTH;
end;

destructor TUniGridColumn.Destroy;
begin
  FTitle.Free;
  inherited;
end;

function TUniGridColumn.GetDisplayName: string;
begin
  Result := Self.ClassName + '-' + IntToStr(ID);
end;

procedure TUniGridColumn.SetTitle(const Value: TUniColumnTitle);
begin
  FTitle.Assign(Value);
end;

procedure TUniGridColumn.SetWidth(const Value: Integer);
var
  I : Integer;
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    I := (Collection.Owner as TUniStringGrid).ColCount;

    if Self.Index < I  then
      (Collection.Owner as TUniStringGrid).ColWidths[Self.Index] := Value;
  end;
end;

{ TUniGridColumns }

function TUniGridColumns.GetItems(Index: Integer): TUniGridColumn;
begin
  Result := (inherited Items[Index]) as TUniGridColumn;
end;

procedure TUniGridColumns.SetItems(Index: Integer;
  const Value: TUniGridColumn);
begin
  ((inherited Items[Index]) as TUniGridColumn).Assign(Value);
end;

end.
