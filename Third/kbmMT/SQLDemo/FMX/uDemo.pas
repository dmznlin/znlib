unit uDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  Data.DB, kbmMemTable, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Data.Bind.Components, Data.Bind.DBScope, FMX.Grid, Fmx.Bind.Editors,
  Data.Bind.DBLinks, Fmx.Bind.DBLinks, kbmMemSQL, kbmSQLMemTableAPI, kbmSQLStdFunc,
  kbmMemCSVStreamFormat, System.Rtti, System.Bindings.Outputs, FMX.StdCtrls,
  FMX.TabControl, Fmx.Bind.Grid, Data.Bind.Grid, FMX.TreeView, kbmSQLElements,
  kbmFMXMemo, kbmFMXGrid;

type
  TForm1 = class(TForm)
    mtTable1: TkbmMemTable;
    mtTable1fld1: TStringField;
    mtTable1fld2: TIntegerField;
    mtTable1fld3: TIntegerField;
    mtTable1fld4: TStringField;
    dsTable1: TDataSource;
    mtTable1fld5: TIntegerField;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    btnExecuteTop: TButton;
    dsResult: TDataSource;
    Panel3: TPanel;
    Memo2: TMemo;
    kbmCSVStreamFormat1: TkbmCSVStreamFormat;
    btnSaveResult: TButton;
    btnExecuteSelectedSQL: TButton;
    btnExecuteAllSQL: TButton;
    TabControl1: TTabControl;
    tsDataResult: TTabItem;
    tsParseTree: TTabItem;
    tsLog: TTabItem;
    TabControl2: TTabControl;
    tsTable1: TTabItem;
    tsTable2: TTabItem;
    tsTable3: TTabItem;
    mtTable2: TkbmMemTable;
    StringField1: TStringField;
    IntegerField1: TIntegerField;
    IntegerField2: TIntegerField;
    StringField2: TStringField;
    IntegerField3: TIntegerField;
    dsTable2: TDataSource;
    mtTable3: TkbmMemTable;
    StringField3: TStringField;
    IntegerField4: TIntegerField;
    IntegerField5: TIntegerField;
    StringField4: TStringField;
    IntegerField6: TIntegerField;
    dsTable3: TDataSource;
    mtTable1fld6: TIntegerField;
    mtTable1fld7: TIntegerField;
    mtTable1fld8: TFloatField;
    mtTable2fld6: TIntegerField;
    mtTable2fld7: TIntegerField;
    mtTable2fld8: TFloatField;
    mtTable3fld6: TIntegerField;
    mtTable3fld7: TIntegerField;
    mtTable3fld8: TFloatField;
    Layout1: TLayout;
    Layout2: TLayout;
    Splitter2: TSplitter;
    Label3: TLabel;
    Label4: TLabel;
    mLog: TMemo;
    tvParse: TTreeView;
    Layout3: TLayout;
    btnRefreshParseTree: TButton;
    mSQL: TkbmFMXMemo;
    kbmFMXDBGrid1: TkbmFMXDBGrid;
    kbmFMXDBGrid2: TkbmFMXDBGrid;
    kbmFMXDBGrid3: TkbmFMXDBGrid;
    kbmFMXDBGrid4: TkbmFMXDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExecuteTopClick(Sender: TObject);
    procedure btnSaveResultClick(Sender: TObject);
    procedure btnExecuteSelectedSQLClick(Sender: TObject);
    procedure btnExecuteAllSQLClick(Sender: TObject);
    procedure btnRefreshParseTreeClick(Sender: TObject);
  private
    { Private declarations }
    FCurrentLine:integer;
    FSQL:TkbmMemSQL;
    procedure AddSubElements(const AParentNode:TTreeViewItem; AElements:TkbmSQLNodes);
    procedure AddSubElement(const AParentNode:TTreeViewItem; AElement:TkbmSQLCustomNode);

    function OnGetVariableValue(const ANode:TkbmSQLCustomNode; const AVariableName:string; var AVariableValue:variant):boolean;
    function OnGetVariableMetaData(const ANode:TkbmSQLCustomNode; const AVariableName:string; var AVariableWidth:integer; var AVariableDataType:TFieldType):boolean;

  public
    { Public declarations }
    procedure BuildParseTree;
    procedure BuildSampleData(ATable:TkbmMemTable);
    procedure ExecuteSQL(const ASQL:string);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
     FSQL:=TkbmMemSQL.Create(nil);
     FSQL.OnGetVariableValue:=OnGetVariableValue;

     // This event is only necessary to set if the variable is going to be part of a field result.
     FSQL.OnGetVariableMetaData:=OnGetVariableMetaData;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     FSQL.Free;
end;

procedure TForm1.ExecuteSQL(const ASQL:string);
var
   i:integer;
   fld:TkbmSQLFieldNode;
   tbl:TkbmSQLTable;
begin
     mLog.Lines.Add('Executing: '+ASQL);

     BuildSampleData(mtTable1);
     BuildSampleData(mtTable2);
     BuildSampleData(mtTable3);

     // Add tables that the SQL is supposed to access.
     FSQL.Tables.Clear;
     FSQL.Tables.Add('table1',mtTable1);
     FSQL.Tables.Add('table2',mtTable2);
     FSQL.Tables.Add('table3',mtTable3);

     // Execute the SQL.
     try
        FSQL.ExecSQL(ASQL);
        dsResult.DataSet:=FSQL;
     finally
        // Dump our parse tree as we know it.
        with Memo2.Lines do
        begin
             Clear;
             Add('Operation');
             Add('----------');
             Add('');
             if FSQL.Parser.Operation<>nil then
             begin

                  case FSQL.Parser.Operation.OpType of
                       sotSELECT: Add('SELECT');
                       sotDELETE: Add('DELETE');
                       sotINSERT: Add('INSERT');
                       sotUPDATE: Add('UPDATE');
                  else
                       Add('Unknown');
                  end;

                  for i:=0 to FSQL.Parser.Operation.RefSourceTables.Count-1 do
                  begin
                       Add('Table '+Inttostr(i));
                       tbl:=FSQL.Parser.Operation.RefSourceTables[i];
                       Add('  '+tbl.Name+' as '+tbl.Alias);
                  end;

                  for i:=0 to FSQL.Parser.Operation.RefSourceFields.Count-1 do
                  begin
                       Add('Included field '+Inttostr(i));
                       fld:=FSQL.Parser.Operation.RefSourceFields.FieldNodes[i];
                       Add('  '+fld.FieldName+' as '+fld.Alias);
                  end;

                  for i:=0 to FSQL.Parser.Operation.RefSearchFields.Count-1 do
                  begin
                       Add('Searched field '+Inttostr(i));
                       fld:=FSQL.Parser.Operation.RefSearchFields.FieldNodes[i];
                       Add('  '+fld.FieldName+' as '+fld.Alias);
                  end;
             end
             else
                 Add('NOT DETERMINED');
        end;
     end;
end;

procedure TForm1.btnExecuteAllSQLClick(Sender: TObject);
var
   i:integer;
begin
     for i:=0 to mSQL.Lines.Count-1 do
         ExecuteSQL(mSQL.Lines[i]);
end;

procedure TForm1.btnExecuteSelectedSQLClick(Sender: TObject);
var
   i:integer;
begin
     i:=mSQL.CaretPos.Line;
     ExecuteSQL(mSQL.Lines[i]);
end;

procedure TForm1.btnExecuteTopClick(Sender: TObject);
begin
     ExecuteSQL(mSQL.Lines[0]);
end;

procedure TForm1.btnRefreshParseTreeClick(Sender: TObject);
begin
     BuildParseTree;
end;

procedure TForm1.btnSaveResultClick(Sender: TObject);
begin
     FSQL.SaveToFileViaFormat('resultCSV',kbmCSVStreamFormat1);
end;

procedure TForm1.BuildSampleData(ATable:TkbmMemTable);
var
   i:integer;
begin
     ATable.Close;
     ATable.CreateTable;
     ATable.DisableControls;
     try
        ATable.Open;
        for i:=0 to 1000 do
        begin
             ATable.Append;
             ATable.FieldByName('fld1').AsString:='STR'+inttostr(i div 3);
             ATable.FieldByName('fld2').AsInteger:=i;
             ATable.FieldByName('fld3').AsInteger:=999-i;
             ATable.FieldByName('fld4').AsString:='STR'+inttostr(999-i);
             ATable.FieldByName('fld5').AsInteger:=Random(2)+5;
             ATable.FieldByName('fld6').AsInteger:=Random(100);
             if random(100)>5 then
                ATable.FieldByName('fld7').AsInteger:=ATable.FieldByName('fld6').AsInteger;
             ATable.FieldByName('fld8').AsFloat:=ATable.FieldByName('fld3').AsInteger / (ATable.FieldByName('fld6').AsInteger+1);
             ATable.Post;
        end;
     finally
        ATable.EnableControls;
     end;
end;

procedure TForm1.AddSubElements(const AParentNode:TTreeViewItem; AElements:TkbmSQLNodes);
var
   i:integer;
   n:TkbmSQLCustomNode;
begin
     for i:=0 to AElements.Count-1 do
     begin
          n:=AElements.Node[i];
          AddSubElement(AParentNode,n);
     end;
end;

procedure TForm1.AddSubElement(const AParentNode:TTreeViewItem; AElement:TkbmSQLCustomNode);
var
   tn,tn1:TTreeViewItem;
   s:string;
begin
     if AElement is TkbmSQLBinaryNode then
     begin
          case TkbmSQLBinaryNode(AElement).Operator of
               ebAdd: s:='Add';
               ebConcat: s:='Concat';
               ebSub: s:='Sub';
               ebMul: s:='Mul';
               ebDiv: s:='Div';
               ebAnd: s:='And';
               ebOr: s:='Or';
               ebXor: s:='XOr';
               ebMod: s:='Mod';
               ebIDiv: s:='IDiv';
               ebLike: s:='LIKE';
               ebEqual: s:='=';
               ebNotEqual: s:='<>';
               ebLess: s:='<';
               ebGreater: s:='>';
               ebLessEqual: s:='<=';
               ebGreaterEqual: s:='>=';
          else
              s:='Unknown';
          end;

          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName+' '+s;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Left';
          tn1.Parent:=tn;
          AddSubElement(tn1,TkbmSQLBinaryNode(AElement).LeftNode);
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Right';
          tn1.Parent:=tn;
          AddSubElement(tn1,TkbmSQLBinaryNode(AElement).RightNode);
     end
     else if AElement is TkbmSQLUnaryNode then
     begin
          case TkbmSQLUnaryNode(AElement).Operator of
               euNot: s:='Not';
               euNegate: s:='Negate';
          else
              s:='Unknown';
          end;

          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Right';
          tn1.Parent:=tn;

          AddSubElement(tn1,TkbmSQLUnaryNode(AElement).RightNode);
     end
     else if AElement is TkbmSQLBetweenNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Left';
          tn1.Parent:=tn;
          AddSubElement(tn1,TkbmSQLBetweenNode(AElement).LeftNode);

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Range low';
          tn1.Parent:=tn;
          AddSubElement(tn1,TkbmSQLBetweenNode(AElement).RangeLow);

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Range high';
          tn1.Parent:=tn;
          AddSubElement(tn1,TkbmSQLBetweenNode(AElement).RangeHigh);
     end
     else if AElement is TkbmSQLInNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Left';
          tn1.Parent:=tn;
          AddSubElement(tn1,TkbmSQLInNode(AElement).LeftNode);

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='In';
          tn1.Parent:=tn;
          AddSubElements(tn1,TkbmSQLInNode(AElement).Nodes);
     end
     else if AElement is TkbmSQLFieldNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Type='+TkbmSQLFieldNode(AElement).ClassName;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Name='+TkbmSQLFieldNode(AElement).FieldName;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Alias='+TkbmSQLFieldNode(AElement).Alias;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Description='+TkbmSQLFieldNode(AElement).Description;
          tn1.Parent:=tn;
     end
     else if AElement is TkbmSQLVariableNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Type='+TkbmSQLVariableNode(AElement).ClassName;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='VariableName='+TkbmSQLVariableNode(AElement).VariableName;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Alias='+TkbmSQLVariableNode(AElement).Alias;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Description='+TkbmSQLVariableNode(AElement).Description;
          tn1.Parent:=tn;
     end
     else if AElement is TkbmSQLGroupFieldNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Name='+TkbmSQLGroupFieldNode(AElement).ClassName;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Alias='+TkbmSQLGroupFieldNode(AElement).Alias;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Description='+TkbmSQLGroupFieldNode(AElement).Description;
          tn1.Parent:=tn;
     end
     else if AElement is TkbmSQLAggregateNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          if TkbmSQLAggregateNode(AElement).Distinct then
             s:=' DISTINCT'
          else
              s:='';

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Aggregation='+TkbmSQLAggregateNode(AElement).FieldModifier+s;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Expression';
          tn1.Parent:=tn;
          AddSubElement(tn1,TkbmSQLAggregateNode(AElement).ExpressionNode);
     end
     else if AElement is TkbmSQLFunctionNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Function='+TkbmSQLFunctionNode(AElement).FunctionName;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Arguments';
          tn1.Parent:=tn;
          AddSubElements(tn1,TkbmSQLFunctionNode(AElement).Args);
     end
     else if AElement is TkbmSQLConstNode then
     begin
          tn:=TTreeViewItem.Create(tvParse);
          tn.Text:=AElement.ClassName;
          tn.Parent:=AParentNode;

          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Alias='+TkbmSQLConstNode(AElement).Alias;
          tn1.Parent:=tn;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Value='+String(TkbmSQLConstNode(AElement).Value);
          tn1.Parent:=tn;
     end;
end;

procedure TForm1.BuildParseTree;
var
   tn,tn1:TTreeViewItem;
   op:TkbmSQLCustomOperation;
begin
     tvParse.Clear;

     op:=FSQL.Parser.Operation;

     tn:=TTreeViewItem.Create(tvParse);
     tn.Text:=op.ClassName;
     tn.Parent:=tvParse;
     if op is TkbmSQLSelectOperation then
     begin
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Field expressions';
          tn1.Parent:=tn;
          AddSubElements(tn1,(TkbmSQLSelectOperation(op).Selection));
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Condition';
          tn1.Parent:=tn;
          AddSubElement(tn1,(TkbmSQLSelectOperation(op).Condition));
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Group expression';
          tn1.Parent:=tn;
          AddSubElements(tn1,(TkbmSQLSelectOperation(op).Group));
          if TkbmSQLSelectOperation(op).HavingCondition<>nil then
          begin
               tn1:=TTreeViewItem.Create(tvParse);
               tn1.Text:='Having expression';
               tn1.Parent:=tn;
               AddSubElement(tn1,(TkbmSQLSelectOperation(op).HavingCondition));
          end;
          tn1:=TTreeViewItem.Create(tvParse);
          tn1.Text:='Order expression';
          tn1.Parent:=tn;
          AddSubElements(tn1,(TkbmSQLSelectOperation(op).Order));
     end;
     tvParse.ExpandAll;
     tvParse.RealignContent;
end;

function TForm1.OnGetVariableValue(const ANode:TkbmSQLCustomNode; const AVariableName:string; var AVariableValue:variant):boolean;
var
   s:string;
begin
     s:=UpperCase(AVariableName);
     if s='VAR1' then
     begin
          AVariableValue:='The red fox';
          Result:=true;
          exit;
     end;
     if s='VAR2' then
     begin
          AVariableValue:=1234.567;
          Result:=true;
          exit;
     end;
     Result:=false;
end;

function TForm1.OnGetVariableMetaData(const ANode:TkbmSQLCustomNode; const AVariableName:string; var AVariableWidth:integer; var AVariableDataType:TFieldType):boolean;
var
   s:string;
begin
     s:=UpperCase(AVariableName);
     if s='VAR1' then
     begin
          AVariableWidth:=11;
          AVariableDataType:=ftString;
          Result:=true;
          exit;
     end;
     if s='VAR2' then
     begin
          AVariableWidth:=10;
          AVariableDataType:=ftFloat;
          Result:=true;
          exit;
     end;
     Result:=false;
end;


end.
