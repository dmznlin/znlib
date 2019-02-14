unit kbmSQLDBAPI;

// =========================================================================
// A kbmMemTable based SQL implementation.
//
// Copyright 2007-2015 Kim Bo Madsen/Components4Developers DK
// All rights reserved.
//
// Before using this file you must have read, understood and accepted the
// the license agreement which you find in the file license.txt.
// If that file is not part of the package then the package is not valid and
// must be removed immediately. A valid package can be downloaded from
// Components4Developers at www.components4developers.com

interface

{$include kbmMemTable.inc}

uses
    Classes,
{$IFDEF NEXTGEN}
    System.Generics.Collections,
{$ENDIF}
    DB,
    kbmSQLElements,
    kbmMemTable;

{$WARN UNSAFE_CAST OFF}

const
   KBMSQL_UNIQUE_INDEXNAME = '__KBMSQL_UNIQUE';
   KBMSQL_ORDERBY_INDEXNAME = '__KBMSQL_ORDERBY';

type
   TkbmSQLCustomDBAPI = class;
   TkbmSQLCustomDBAPIClass = class of TkbmSQLCustomDBAPI;

   TkbmSQLDBAPIRegistration = class
   private
      FDataset:TClass;
      FDatasetAPI:TkbmSQLCustomDBAPI;
   public
      constructor Create(const ADataset:TClass; const ADatasetAPIClass:TkbmSQLCustomDBAPIClass); virtual;
      destructor Destroy; override;
      property Dataset:TClass read FDataset;
      property DatasetAPI:TkbmSQLCustomDBAPI read FDatasetAPI;
   end;

   TkbmSQLDBAPIRegistrations = class
   private
{$IFDEF NEXTGEN}
      FList:TList<TkbmSQLDBAPIRegistration>;
{$ELSE}
      FList:TList;
{$ENDIF}
   public
      constructor Create; virtual;
      destructor Destroy; override;

      procedure Clear;
      procedure RegisterAPI(ADatasetClass:TClass; ADatasetAPIClass:TkbmSQLCustomDBAPIClass);
      function GetAPI(ADataset:TClass):TkbmSQLCustomDBAPI;
   end;

   TkbmSQLCustomDBAPI = class(TkbmSQLCustomDBAPI_)
   protected
      function CreateField(const AOwner:TComponent; const ADataType:TFieldType):TField; virtual;
      procedure CheckNonAggregateFieldsInGroupClause(const ASelection:TkbmSQLNodes; const AGroup:TkbmSQLNodes); virtual;
      procedure CreateSelectionFields(const ATable:TkbmCustomMemTable; const ASelection:TkbmSQLNodes; const ACreateGroupFields:boolean; const AUseAggregateSources:boolean); virtual;
      procedure CreateDistinctIndex(const ATable:TkbmCustomMemTable; const ADistinct:TkbmSQLNodes); virtual;
      procedure AppendResultRecord(const AResultTable:TkbmCustomMemTable; const ASelection:TkbmSQLNodes; const AIncludeGroupFields:boolean); overload; virtual;
      procedure ApplyGroupBy(const ASourceTable,AResultTable:TkbmCustomMemTable; const ASelection:TkbmSQLNodes; const AGroup:TkbmSQLNodes; const AAggregates:TkbmSQLNodes); virtual;
      procedure ApplyOrderBy(const AResultTable:TkbmCustomMemTable; const AOrder:TkbmSQLNodes); virtual;
      procedure ApplyOffsetAndLimit(const AResultTable:TkbmCustomMemTable; const AOffset:TkbmSQLCustomNode; const ALimit:TkbmSQLCustomNode); virtual;
      procedure DetermineUsableIndexesForRefFields(const AOperation:TkbmSQLCustomOperation); virtual;
      procedure SetupSourceFields(const ANodes:TkbmSQLNodes; const ASource:TkbmSQLCustomTableData); virtual;
      procedure SetupSourceField(const ANode:TkbmSQLCustomNode; const ASource:TkbmSQLCustomTableData); virtual;
      procedure Search(const ASourceTable:TkbmSQLCustomTableData; AResultTable:TkbmCustomMemTable; const AOperation:TkbmSQLCustomSelectOperation; const ASelection:TkbmSQLNodes; const ACondition:TkbmSQLCustomNode; const ACopyDirectlyFromSource:boolean); virtual;
      function GetIndexNameForField(const AField:TkbmSQLFieldNode):string; virtual; abstract;

   public
      procedure PrepareSubset(const ATable:TkbmSQLTable); virtual;
      function GetTableDataClass:TkbmSQLCustomTableDataClass; virtual;

      function LocateFirst(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean; virtual;
      function LocateNext(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean; virtual;

      function Insert(const ATable:TkbmSQLTable; const AOperation:TkbmSQLInsertOperation):boolean; virtual; abstract;
      function Delete(const ATable:TkbmSQLTable; const AOperation:TkbmSQLDeleteOperation):boolean; virtual; abstract;
      function Update(const ATable:TkbmSQLTable; const AOperation:TkbmSQLUpdateOperation):boolean; virtual; abstract;
      function Select(const AResultDataset:TkbmCustomMemTable; const AOperation:TkbmSQLCustomSelectOperation):boolean; virtual; abstract;
   end;

   TkbmSQLCustomDatasetAPI = class(TkbmSQLCustomDBAPI)
   public
      function Insert(const ATable:TkbmSQLTable; const AOperation:TkbmSQLInsertOperation):boolean; override;
      function Delete(const ATable:TkbmSQLTable; const AOperation:TkbmSQLDeleteOperation):boolean; override;
      function Update(const ATable:TkbmSQLTable; const AOperation:TkbmSQLUpdateOperation):boolean; override;
      function Select(const AResultDataset:TkbmCustomMemTable; const AOperation:TkbmSQLCustomSelectOperation):boolean; override;
   end;

// Initialization/Finalization
{$HPPEMIT '#pragma link "kbmSQLDBAPI"' }

  procedure RunInitialization;
  procedure RunFinalization;
  function IsInitialized:boolean;

var
   kbmSQLDBAPIRegistrations:TkbmSQLDBAPIRegistrations;

implementation

uses
    Variants,
    SysUtils,
    kbmMemSQL,
    kbmSQLMemTableAPI;

constructor TkbmSQLDBAPIRegistration.Create(const ADataset:TClass; const ADatasetAPIClass:TkbmSQLCustomDBAPIClass);
begin
     inherited Create;
     FDataset:=ADataset;
     FDatasetAPI:=ADatasetAPIClass.Create;
end;

destructor TkbmSQLDBAPIRegistration.Destroy;
begin
     FDatasetAPI.Free;
     inherited Destroy;
end;

constructor TkbmSQLDBAPIRegistrations.Create;
begin
     inherited Create;
{$IFDEF NEXTGEN}
     FList:=TList<TkbmSQLDBAPIRegistration>.Create;
{$ELSE}
     FList:=TList.Create;
{$ENDIF}
end;

destructor TkbmSQLDBAPIRegistrations.Destroy;
begin
     Clear;
     FList.Free;
     inherited Destroy;
end;

procedure TkbmSQLDBAPIRegistrations.Clear;
{$IFNDEF NEXTGEN}
var
   i:integer;
{$ENDIF}
begin
{$IFNDEF NEXTGEN}
     for i:=0 to FList.Count-1 do
         TkbmSQLCustomDBAPI(FList.Items[i]).Free;
{$ENDIF}
     FList.Clear;
end;

procedure TkbmSQLDBAPIRegistrations.RegisterAPI(ADatasetClass:TClass; ADatasetAPIClass:TkbmSQLCustomDBAPIClass);
begin
     if GetAPI(ADatasetClass)<>nil then
        raise Exception.Create('Database API already registered for '+ADatasetClass.ClassName);
     FList.Add(TkbmSQLDBAPIRegistration.Create(ADatasetClass,ADatasetAPIClass));
end;

function TkbmSQLDBAPIRegistrations.GetAPI(ADataset:TClass):TkbmSQLCustomDBAPI;
var
   i:integer;
   reg:TkbmSQLDBAPIRegistration;
begin
     for i:=0 to FList.Count-1 do
     begin
          reg:=TkbmSQLDBAPIRegistration(FList.Items[i]);
          if (ADataset=reg.FDataset) or (ADataset.InheritsFrom(reg.FDataset)) then
          begin
               Result:=reg.FDatasetAPI;
               exit;
          end;
     end;
     Result:=nil;
end;

procedure TkbmSQLCustomDBAPI.PrepareSubset(const ATable:TkbmSQLTable);
begin
     if ATable.Data.SubSetData<>nil then
        ATable.Data.CurrentData:=ATable.Data.SubSetData
     else
         ATable.Data.CurrentData:=ATable.Data.Data;
end;

function TkbmSQLCustomDBAPI.CreateField(const AOwner:TComponent; const ADataType:TFieldType):TField;
{
  const
     FTClass : array [TFieldType] of TFieldClass = (
       nil,              // ftUnknown,
       TStringField,     // ftString,
       TSmallIntField,   // ftSmallint,
       TIntegerField,    // ftInteger,
       TWordField,       // ftWord,
       TBooleanField,    // ftBoolean,
       TFloatField,      // ftFloat,
       TCurrencyField,   // ftCurrency,
       TBCDField,        // ftBCD,
       TDateField,       // ftDate,
       TTimeField,       // ftTime,
       TDateTimeField,   // ftDateTime,
       TBytesField,      // ftBytes,
       TVarBytesField,   // ftVarBytes,
       TAutoIncField,    // ftAutoInc,
       TBlobField,       // ftBlob,
       TMemoField,       // ftMemo,
       TGraphicField,    // ftGraphic,
       TFmtMemoField,    // ftFmtMemo,
       TParadoxOLEField, // ftParadoxOle,
       nil,              // ftDBaseOle,
       TTypedBinaryField,// ftTypedBinary,
       nil,              // ftCursor,
       TFixedCharField,  // ftFixedChar,
       TWideStringField, // ftWideString,
       TLargeIntField,   // ftLargeint,
       nil,              // ftADT,
       nil,              // ftArray,
       nil,              // ftReference,
       nil,              // ftDataSet,
       nil,              // ftOraBlob,
       TOraClobField,    // ftOraClob,
       nil,              // ftVariant,
       nil,              // ftInterface,
       nil,              // ftIDispatch,
       TGUIDField,       // ftGuid,
       TTimeStampField,  // ftTimeStamp,
       TFMTBCDField      // ftFMTBcd
     );
}
var
   fc:TFieldClass;
begin
     fc:=DefaultFieldClasses[ADataType];
     if fc=nil then
        raise Exception.Create('Unsupported field type ('+inttostr(ord(ADataType))+').');
     Result:=fc.Create(AOwner);
end;

procedure TkbmSQLCustomDBAPI.CheckNonAggregateFieldsInGroupClause(const ASelection:TkbmSQLNodes; const AGroup:TkbmSQLNodes);
begin
     // TODO.
end;

procedure TkbmSQLCustomDBAPI.CreateSelectionFields(const ATable:TkbmCustomMemTable; const ASelection:TkbmSQLNodes; const ACreateGroupFields:boolean; const AUseAggregateSources:boolean);
var
   i:integer;
   node:TkbmSQLCustomNode;
   fld:TkbmSQLFieldNode;
   ffld:TField;
   dt:TFieldType;
begin
     ATable.DeleteTable;

     for i:=0 to ASelection.Count-1 do
     begin
          node:=ASelection[i];

          if (node is TkbmSQLGroupFieldNode) and (not ACreateGroupFields) then
             continue
          else if (node is TkbmSQLAggregateNode) and ACreateGroupFields then
          begin
               if AUseAggregateSources then
                  dt:=TkbmSQLAggregateNode(node).ExpressionNode.DataType
               else
                   dt:=TkbmSQLAggregateNode(node).DataType;
               ffld:=CreateField(ATable,dt);
               ffld.FieldName:=node.GetUniqueName;
               ffld.FieldKind:=fkData;
               ffld.DisplayLabel:=node.Description;
               if (node.DataType in kbmStringTypes) or (node.DataType in kbmBlobTypes) then
                  ffld.Size:=node.Width;
               ffld.DataSet:=ATable;
               node.DestinationField:=ffld;
          end
          else if node is TkbmSQLFieldNode then
          begin
               ffld:=nil; // To satisfy compiler warning.
               fld:=TkbmSQLFieldNode(node);
               case fld.FieldType of
                  ntField:
                    ffld:=CreateField(ATable,fld.DataType);
                  ntRowID,
                  ntRecNo:
                    ffld:=CreateField(ATable,ftLargeint);
                  ntWildCard:
                    raise Exception.Create('Internal error 1');
               end;

               ffld.FieldName:=fld.UniqueName;
               ffld.FieldKind:=fkData;
               ffld.DisplayLabel:=node.Description;
               ffld.Size:=node.Width;
               ffld.DataSet:=ATable;
               node.DestinationField:=ffld;
          end
          else
          begin
               ffld:=CreateField(ATable,node.DataType);
               ffld.FieldName:=node.GetUniqueName;
               ffld.FieldKind:=fkData;
               ffld.DisplayLabel:=node.Description;
               if (node.DataType in kbmStringTypes) or (node.DataType in kbmBlobTypes) then
                  ffld.Size:=node.Width;
               ffld.DataSet:=ATable;
               node.DestinationField:=ffld;
          end;
     end;

     ATable.ResetAutoInc;
end;

procedure TkbmSQLCustomDBAPI.CreateDistinctIndex(const ATable:TkbmCustomMemTable; const ADistinct:TkbmSQLNodes);
var
   i:integer;
   s,a:string;
   n:TkbmSQLCustomNode;
begin
     s:='';
     a:='';
     for i:=0 to ADistinct.Count-1 do
     begin
          n:=ADistinct.Node[i];
          s:=s+a+n.GetUniqueName;
          a:=';';
     end;
     ATable.AddIndex(KBMSQL_UNIQUE_INDEXNAME,s,[ixUnique]);
end;

procedure TkbmSQLCustomDBAPI.DetermineUsableIndexesForRefFields(const AOperation:TkbmSQLCustomOperation);
var
   i:integer;
   n:TkbmSQLCustomNode;
   fld:TkbmSQLFieldNode;
begin
     for i:=0 to AOperation.RefSourceFields.Count-1 do
     begin
          n:=AOperation.RefSourceFields[i];
          if not (n is TkbmSQLFieldNode) then
             continue;

          fld:=TkbmSQLFieldNode(n);
          if fld.SourceFieldObject=nil then
             continue;
          fld.IndexName:=GetIndexNameForField(fld);
     end;
end;

procedure TkbmSQLCustomDBAPI.SetupSourceFields(const ANodes:TkbmSQLNodes; const ASource:TkbmSQLCustomTableData);
var
   i:integer;
begin
     for i:=0 to ANodes.Count-1 do
         SetupSourceField(ANodes.Node[i],ASource);
end;

procedure TkbmSQLCustomDBAPI.SetupSourceField(const ANode:TkbmSQLCustomNode; const ASource:TkbmSQLCustomTableData);
var
   fnode:TkbmSQLFieldNode;
   aggnode:TkbmSQLAggregateNode;
   s:string;
begin
     // Point aggregate nodes to result fields.
     if (ANode is TkbmSQLFieldNode) then
     begin
          fnode:=TkbmSQLFieldNode(ANode);
          s:=fnode.GetUniqueName;
          ANode.Source:=ASource;
          ANode.SourceFieldObject:=ASource.GetFieldObject(s);
          if ANode.SourceFieldObject=nil then
             ANode.SourceFieldObject:=ASource.GetFieldObject(fnode.FieldName);
          ANode.Source:=ASource;
     end
     else if (ANode is TkbmSQLAggregateNode) then
     begin
          aggnode:=TkbmSQLAggregateNode(ANode);
          s:=aggnode.GetUniqueName;
          ANode.SourceFieldObject:=ASource.GetFieldObject(s);
          ANode.Source:=ASource;
     end
     else if ANode is TkbmSQLUnaryNode then
        SetupSourceField(TkbmSQLUnaryNode(ANode).RightNode,ASource)
     else if ANode is TkbmSQLBinaryNode then
     begin
          SetupSourceField(TkbmSQLBinaryNode(ANode).LeftNode,ASource);
          SetupSourceField(TkbmSQLBinaryNode(ANode).RightNode,ASource);
     end
     else if ANode is TkbmSQLBetweenNode then
     begin
          SetupSourceField(TkbmSQLBetweenNode(ANode).LeftNode,ASource);
          SetupSourceField(TkbmSQLBetweenNode(ANode).RangeLow,ASource);
          SetupSourceField(TkbmSQLBetweenNode(ANode).RangeHigh,ASource);
     end
     else if ANode is TkbmSQLInNode then
     begin
          SetupSourceField(TkbmSQLInNode(ANode).LeftNode,ASource);
          SetupSourceFields(TkbmSQLInNode(ANode).Nodes,ASource);
     end
     else if ANode is TkbmSQLFunctionNode then
          SetupSourceFields(TkbmSQLFunctionNode(ANode).Args,ASource);
end;

procedure TkbmSQLCustomDBAPI.AppendResultRecord(const AResultTable:TkbmCustomMemTable; const ASelection:TkbmSQLNodes; const AIncludeGroupFields:boolean);
var
   i:integer;
   node:TkbmSQLCustomNode;
   fld:TField;
begin
     AResultTable.Append;
     for i:=0 to ASelection.Count-1 do
     begin
          node:=ASelection[i];
          if (node is TkbmSQLGroupFieldNode) and (not AIncludeGroupFields) then
             continue;
          fld:=node.DestinationField;
          if fld=nil then
             raise Exception.Create('Corrupted selection field reference: '+node.GetUniqueName);
          fld.Value:=node.Execute;
     end;
     try
        AResultTable.Post;
     except
        on E: EMemTableDupKey do
           AResultTable.Cancel;
        on E: Exception do raise;
     end;
end;

function TkbmSQLCustomDBAPI.GetTableDataClass:TkbmSQLCustomTableDataClass;
begin
     Result:=nil;
end;

function TkbmSQLCustomDBAPI.LocateFirst(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean;
begin
     Result:=false;
end;

function TkbmSQLCustomDBAPI.LocateNext(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean;
begin
     Result:=false;
end;

procedure TkbmSQLCustomDBAPI.Search(const ASourceTable:TkbmSQLCustomTableData; AResultTable:TkbmCustomMemTable; const AOperation:TkbmSQLCustomSelectOperation; const ASelection:TkbmSQLNodes; const ACondition:TkbmSQLCustomNode; const ACopyDirectlyFromSource:boolean);
var
   v:variant;
   exec:boolean;
begin
     ASourceTable.DisableControls;
     AResultTable.DisableControls;
     try
        ASourceTable.First;
        try
           while not ASourceTable.Eof do
           begin
                exec:=ACondition=nil;
                if not exec then
                begin
                     v:=ACondition.Execute;
                     exec:=(not VarIsNull(v)) and (v);
                end;
                if exec then
                begin
                     if ACopyDirectlyFromSource then
                        AppendResultRecord(AResultTable,ASelection,false)
                     else
                         AppendResultRecord(AResultTable,ASelection,AOperation.Group.Count>0);
                end;
                ASourceTable.Next;
           end;
        except
           AResultTable.Cancel;
           raise;
        end;
     finally
        AResultTable.EnableControls;
        ASourceTable.EnableControls;
     end;
end;

procedure TkbmSQLCustomDBAPI.ApplyGroupBy(const ASourceTable,AResultTable:TkbmCustomMemTable; const ASelection:TkbmSQLNodes; const AGroup:TkbmSQLNodes; const AAggregates:TkbmSQLNodes);
var
   i:integer;
   node:TkbmSQLCustomNode;
   fnode:TkbmSQLFieldNode;
   anode:TkbmSQLAggregateNode;
   sGroupFields,sFields:string;
   s,a:string;
   gbsrcflist,
   aggsrcflist,aggdstflist:TkbmFieldList;
begin
     CreateSelectionFields(AResultTable,ASelection,false,false);
     AResultTable.Open;

     // Build group fields.
     sGroupFields:='';
     a:='';
     for i:=0 to AGroup.Count-1 do
     begin
          node:=AGroup.Node[i];
          if node is TkbmSQLFieldNode then
          begin
               fnode:=TkbmSQLFieldNode(node);
               sGroupFields:=sGroupFields+a+fnode.GetUniqueName;
               a:=';';
          end;
     end;
     a:='';

     // Build aggregate fields.
     sFields:='';
     for i:=0 to ASelection.Count-1 do
     begin
          // Point aggregate nodes to result fields.
          node:=ASelection.Node[i];
          s:=node.GetUniqueName;
          if node is TkbmSQLAggregateNode then
          begin
               anode:=TkbmSQLAggregateNode(node);
               s:=s+':'+anode.FieldModifier;
          end;
          sFields:=sFields+a+s;
          a:=';';
     end;

     gbsrcflist:=TkbmFieldList.Create;
     aggsrcflist:=TkbmFieldList.Create;
     aggdstflist:=TkbmFieldList.Create;
     try
        // Build list of fields to group on (source).
        gbsrcflist.Build(ASourceTable,sGroupFields);

        // Build list of fields to aggregate on.
        aggsrcflist.Build(ASourceTable,sFields);
        aggdstflist.Build(AResultTable,sFields);

        ASourceTable.GroupBy(AResultTable,gbsrcflist,aggsrcflist,aggdstflist);
     finally
        gbsrcflist.Free;
        aggsrcflist.Free;
        aggdstflist.Free;
     end;
end;

procedure TkbmSQLCustomDBAPI.ApplyOrderBy(const AResultTable:TkbmCustomMemTable; const AOrder:TkbmSQLNodes);
var
   i:integer;
   node,node2:TkbmSQLCustomNode;
   fnode,fnode2:TkbmSQLFieldNode;
   s,a:string;
begin
     if AOrder.Count<1 then
        exit;
     s:='';
     a:='';
     for i:=0 to AOrder.Count-1 do
     begin
          node:=AOrder.Node[i];
          if node is TkbmSQLFieldNode then
          begin
               fnode:=TkbmSQLFieldNode(node);

               // Lookup referenced field.
               node2:=TkbmSQLSelectOperation(fnode.Operation).Selection.GetByFieldName(fnode.FieldName);
               if node2 is TkbmSQLFieldNode then
                  fnode2:=TkbmSQLFieldNode(node2)
               else
                   fnode2:=fnode;
               s:=s+a+fnode2.UniqueName;
               if fnode.Descending then
                  s:=s+':D';
               a:=';';
          end;
     end;
     AResultTable.AddIndex(KBMSQL_ORDERBY_INDEXNAME,s,[]);
     AResultTable.IndexName:=KBMSQL_ORDERBY_INDEXNAME;
end;

procedure TkbmSQLCustomDBAPI.ApplyOffsetAndLimit(const AResultTable:TkbmCustomMemTable; const AOffset:TkbmSQLCustomNode; const ALimit:TkbmSQLCustomNode);
var
   n:integer;
begin
     if (AOffset<>nil) or (ALimit<>nil) then
        AResultTable.DisableControls;
     try
        if AOffset<>nil then
        begin
             n:=trunc(double(AOffset.Execute));
             while (n>0) and (AResultTable.RecordCount>0) do
             begin
                  AResultTable.First;
                  AResultTable.Delete;
                  dec(n);
             end;
        end;
        if ALimit<>nil then
        begin
             n:=trunc(double(ALimit.Execute));
             while AResultTable.RecordCount>n do
             begin
                  AResultTable.Last;
                  AResultTable.Delete;
             end;
        end;
     finally
        if (AOffset<>nil) or (ALimit<>nil) then
           AResultTable.EnableControls;
     end;
end;

function TkbmSQLCustomDatasetAPI.Insert(const ATable:TkbmSQLTable; const AOperation:TkbmSQLInsertOperation):boolean;
var
   i:integer;
   fld:TkbmSQLFieldNode;
   td:TkbmSQLCustomTableData;
begin
     td:=ATable.Data;

     // Locate registered table.
     if AOperation.Fields.Count<>AOperation.Values.Count then
        raise Exception.Create('Values dont match fields.');

     Result:=false;
     td.DisableControls;
     try
        td.Insert;
        try
           for i:=0 to AOperation.Fields.Count-1 do
           begin
                fld:=AOperation.Fields.FieldNodes[i];
                fld.Table.Data.SetFieldValue(fld.FieldName,fld.SourceFieldObject,AOperation.Values.Node[i].Execute);
           end;
           td.Post;
           AOperation.Table.BaseTable.AffectedRows:=1;
           Result:=true;
        except
           td.Cancel;
           raise;
        end;
     finally
        td.EnableControls;
     end;
end;

function TkbmSQLCustomDatasetAPI.Delete(const ATable:TkbmSQLTable; const AOperation:TkbmSQLDeleteOperation):boolean;
var
   td:TkbmSQLCustomTableData;
   n:integer;
   exec:boolean;
   v:variant;
begin
     PrepareSubset(ATable);

     // Locate registered table.
     td:=ATable.Data;

     n:=0;
     Result:=false;
     td.DisableControls;
     try
        td.First;
        try
           while not td.Eof do
           begin
                exec:=AOperation.Condition=nil;
                if not exec then
                begin
                     v:=AOperation.Condition.Execute;
                     exec:=(not VarIsNull(v)) and (v);
                end;
                if exec then
                begin
                     td.Delete;
                     inc(n);
                end
                else
                    td.Next;
           end;
           Result:=true;
        except
           td.Cancel;
           raise;
        end;
     finally
        AOperation.Table.BaseTable.AffectedRows:=n;
        td.EnableControls;
     end;
end;

function TkbmSQLCustomDatasetAPI.Update(const ATable:TkbmSQLTable; const AOperation:TkbmSQLUpdateOperation):boolean;
var
   n:integer;
   i:integer;
   exec:boolean;
   td:TkbmSQLCustomTableData;
   fld:TkbmSQLFieldNode;
   v:variant;
begin
     PrepareSubset(ATable);

     // Locate registered table.
     td:=ATable.Data;

     if AOperation.Fields.Count<>AOperation.Values.Count then
        raise Exception.Create('Values dont match fields.');

     n:=0;
     Result:=false;
     td.DisableControls;
     try
        td.First;
        try
           while not td.Eof do
           begin
                exec:=AOperation.Condition=nil;
                if not exec then
                begin
                     v:=AOperation.Condition.Execute;
                     exec:=(not VarIsNull(v)) and (v);
                end;
                if exec then
                begin
                     td.Edit;
                     for i:=0 to AOperation.Fields.Count-1 do
                     begin
                          fld:=AOperation.Fields.FieldNodes[i];
                          fld.Table.Data.SetFieldValue(fld.FieldName,fld.SourceFieldObject,AOperation.Values.Node[i].Execute);
                     end;
                     td.Post;
                     inc(n);
                end;
                td.Next;
           end;
           Result:=true;
        except
           td.Cancel;
           raise;
        end;
     finally
        AOperation.Table.BaseTable.AffectedRows:=n;
        td.EnableControls;
     end;
end;

function TkbmSQLCustomDatasetAPI.Select(const AResultDataset:TkbmCustomMemTable; const AOperation:TkbmSQLCustomSelectOperation):boolean;
var
   tdTempResult,tdGroup,tdHaving:TkbmSQLMemTableData;
   tTempResult,tGroup,tHaving:TkbmMemTable;
   nAggSelection,nSelection:TkbmSQLNodes;
begin
     // Check if group by/aggregation. Then operate via intermediate table.
     if AOperation.IsAggregate or AOperation.IsGroupBy then
     begin
          tdTempResult:=TkbmSQLMemTableData.Create(nil,TkbmMemTable.Create(niL),true);
          tdGroup:=TkbmSQLMemTableData.Create(nil,TkbmMemTable.Create(niL),true);
          tdHaving:=TkbmSQLMemTableData.Create(nil,TkbmMemTable.Create(niL),true);
          tTempResult:=TkbmMemTable(tdTempResult.Data);
          tGroup:=TkbmMemTable(tdGroup.Data);
          tHaving:=TkbmMemTable(tdHaving.Data);

          nAggSelection:=TkbmSQLNodes.Create(false);
          nSelection:=TkbmSQLNodes.Create(false);
          try
             // Sanity check.
             CheckNonAggregateFieldsInGroupClause(AOperation.Group,AOperation.Selection);

             // Create list of selection nodes that are relevant in aggregation.
             nAggSelection.Add(AOperation.Aggregates,true);
             nAggSelection.Add(AOperation.Group,true);

             // Create group table.
             CreateSelectionFields(tGroup,nAggSelection,true,true);

             // Make distinct if requested.
             if AOperation.IsDistinctAggregate then
                CreateDistinctIndex(tGroup,AOperation.Aggregates);

             // Open result table.
             tGroup.Open;

             // Determine indexes matching referenced fields.
             DetermineUsableIndexesForRefFields(AOperation);

             // Look for records matching condition and output selection.
             Search(AOperation.Tables.Tables[0].Data,tGroup,AOperation,nAggSelection,AOperation.Condition,false);

             // Apply group by directly if no having condition.
             if AOperation.HavingCondition<>nil then
             begin
                  ApplyGroupBy(tGroup,tHaving,nAggSelection,AOperation.Group,AOperation.Aggregates);

                  // Create fields
                  CreateSelectionFields(tTempResult,AOperation.Selection,false,false);
                  tTempResult.Open;

                  // Apply having.
                  SetupSourceFields(AOperation.Selection,tdHaving);
                  SetupSourceField(AOperation.HavingCondition,tdHaving);
                  Search(tdHaving,tTempResult,AOperation,AOperation.Selection,AOperation.HavingCondition,false);
             end
             else
                 // Apply group by.
                 ApplyGroupBy(tGroup,tTempResult,nAggSelection,AOperation.Group,AOperation.Aggregates);

             // Do final calculations on aggregated data.
             CreateSelectionFields(AResultDataset,AOperation.Selection,true,false);
             AResultDataset.Open;
             SetupSourceFields(AOperation.Selection,tdTempResult);
             SetupSourceFields(AOperation.Aggregates,tdTempResult);
             Search(tdTempResult,AResultDataset,AOperation,AOperation.Selection,nil,true);

          finally
             nSelection.Free;
             nAggSelection.Free;
             tdGroup.Free;
             tdHaving.Free;
             tdTempResult.Free;
          end;
     end
     else
     begin
          // Create result table.
          CreateSelectionFields(AResultDataset,AOperation.Selection,false,false);

          // Make distinct if requested.
          if AOperation.Distinct then
             CreateDistinctIndex(AResultDataset,AOperation.Selection);

          // Open result table.
          AResultDataset.Open;

          // Determine indexes matching referenced fields.
          DetermineUsableIndexesForRefFields(AOperation);

          // Look for records matching condition and output selection.
          Search(AOperation.Tables[0].Data,AResultDataset,AOperation,AOperation.Selection,AOperation.Condition,false);
     end;

     // Add appropriate index to handle order by.
     ApplyOrderBy(AResultDataset,AOperation.Order);

     // Check if offset given, drop all before offset.
     ApplyOffsetAndLimit(AResultDataset,AOperation.Offset,AOperation.Limit);

     Result:=true;
end;

var
  __IsInitialized:boolean = false;

procedure RunInitialization;
begin
     if __IsInitialized then
        exit;
     __IsInitialized:=true;
     kbmSQLDBAPIRegistrations:=TkbmSQLDBAPIRegistrations.Create;
     kbmSQLDBAPIRegistrations.RegisterAPI(TkbmCustomMemTable,TkbmSQLMemTableAPI);
end;

procedure RunFinalization;
begin
     if not __IsInitialized then
        exit;
     __IsInitialized:=false;
     kbmSQLDBAPIRegistrations.Free;
     kbmSQLDBAPIRegistrations:=nil;
end;

function IsInitialized:boolean;
begin
     Result:=__IsInitialized;
end;

initialization
   RunInitialization;

finalization
   RunFinalization;

end.
