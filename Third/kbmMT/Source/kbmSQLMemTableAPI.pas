unit kbmSQLMemTableAPI;

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

uses
   DB,
   kbmMemTypes,
   kbmList,
   kbmMemTable,
   kbmSQLDBAPI,
   kbmSQLElements;

type
   TkbmSQLMemTableAPI = class(TkbmSQLCustomDatasetAPI)
   protected
      function GetIndexNameForField(const AField:TkbmSQLFieldNode):string; override;
   public
      function GetTableDataClass:TkbmSQLCustomTableDataClass; override;
      function LocateFirst(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean; override;
      function LocateNext(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean; override;
   end;

   TkbmSQLMemTableData = class(TkbmSQLCustomTableData)
   public
      function GetCurrentRecordObject:TObject; override;
      function ContainsField(const AFieldName:string):boolean; override;
      function GetFieldObject(const AFieldName:string):TObject; overload; override;
      function GetFieldObject(const AIndex:integer):TObject; overload; override;
      procedure DisableControls; override;
      procedure EnableControls; override;
      procedure First; override;
      procedure Next; override;
      procedure Edit; override;
      procedure Insert; override;
      procedure Delete; override;
      procedure Cancel; override;
      procedure Post; override;
      function Eof:boolean; override;
      function GetRowID:int64; override;
      function GetRecordNo:integer; override;
      function GetFieldCount:integer; override;
      function GetFieldName(const AFieldObject:TObject):string; override;
      function GetFieldDataType(const AFieldName:string; const AFieldObject:TObject):TFieldType; override;
      function GetFieldKind(const AFieldName:string; const AFieldObject:TObject):TFieldKind; override;
      function GetFieldValue(const AFieldName:string; const AFieldObject:TObject):variant; override;
      procedure SetFieldValue(const AFieldName:string; const AFieldObject:TObject; const AValue:variant); override;
      function GetFieldSize(const AFieldName:string; const AFieldObject:TObject):integer; override;
   end;


implementation

uses
   Classes,
   SysUtils,
   Math;


// --------------------------------------------------------
// TkbmSQLMemTableData
// --------------------------------------------------------

function TkbmSQLMemTableData.GetCurrentRecordObject:TObject;
begin
     Result:=nil;
end;

function TkbmSQLMemTableData.ContainsField(const AFieldName:string):boolean;
begin
     Result:=TkbmCustomMemTable(Data).FindField(AFieldName)<>nil;
end;

function TkbmSQLMemTableData.GetFieldObject(const AFieldName:string):TObject;
begin
     Result:=TkbmCustomMemTable(Data).FindField(AFieldName);
end;

function TkbmSQLMemTableData.GetFieldObject(const AIndex:integer):TObject;
begin
     Result:=TkbmCustomMemTable(Data).Fields[AIndex];
end;

procedure TkbmSQLMemTableData.DisableControls;
begin
     TkbmCustomMemTable(Data).DisableControls;
end;

procedure TkbmSQLMemTableData.EnableControls;
begin
     TkbmCustomMemTable(Data).EnableControls;
end;

procedure TkbmSQLMemTableData.First;
begin
     TkbmCustomMemTable(Data).First;
end;

procedure TkbmSQLMemTableData.Next;
begin
     TkbmCustomMemTable(Data).Next;
end;

procedure TkbmSQLMemTableData.Edit;
begin
     TkbmCustomMemTable(Data).Edit;
end;

procedure TkbmSQLMemTableData.Insert;
begin
     TkbmCustomMemTable(Data).Insert;
end;

procedure TkbmSQLMemTableData.Delete;
begin
     TkbmCustomMemTable(Data).Delete;
end;

procedure TkbmSQLMemTableData.Cancel;
begin
     TkbmCustomMemTable(Data).Cancel;
end;

procedure TkbmSQLMemTableData.Post;
begin
     TkbmCustomMemTable(Data).Post;
end;

function TkbmSQLMemTableData.Eof:boolean;
begin
     Result:=TkbmCustomMemTable(Data).Eof;
end;

function TkbmSQLMemTableData.GetRowID:int64;
begin
     Result:=PkbmRecord(TkbmCustomMemTable(Data).ActiveBuffer).UniqueRecordID;
end;

function TkbmSQLMemTableData.GetRecordNo:integer;
begin
     Result:=PkbmRecord(TkbmCustomMemTable(Data).ActiveBuffer).RecordNo;
end;

function TkbmSQLMemTableData.GetFieldCount:integer;
begin
     Result:=TkbmCustomMemTable(Data).FieldCount;
end;

function TkbmSQLMemTableData.GetFieldName(const AFieldObject:TObject):string;
begin
     if AFieldObject<>nil then
        Result:=TField(AFieldObject).FieldName
     else
         Result:=inherited;
end;

function TkbmSQLMemTableData.GetFieldDataType(const AFieldName:string; const AFieldObject:TObject):TFieldType;
begin
     if AFieldObject<>nil then
        Result:=TField(AFieldObject).DataType
     else
         Result:=inherited;
end;

function TkbmSQLMemTableData.GetFieldKind(const AFieldName:string; const AFieldObject:TObject):TFieldKind;
begin
     if AFieldObject<>nil then
        Result:=TField(AFieldObject).FieldKind
     else
         Result:=inherited;
end;

function TkbmSQLMemTableData.GetFieldValue(const AFieldName:string; const AFieldObject:TObject):variant;
begin
     if AFieldObject<>nil then
        Result:=TField(AFieldObject).Value
     else
         Result:=inherited;
end;

procedure TkbmSQLMemTableData.SetFieldValue(const AFieldName:string; const AFieldObject:TObject; const AValue:variant);
begin
     if AFieldObject<>nil then
        TField(AFieldObject).Value:=AValue;
end;

function TkbmSQLMemTableData.GetFieldSize(const AFieldName:string; const AFieldObject:TObject):integer;
begin
     if AFieldObject<>nil then
        Result:=TField(AFieldObject).Size
     else
         Result:=inherited;
end;

// --------------------------------------------------------
// TkbmSQLMemTableAPI
// --------------------------------------------------------

function TkbmSQLMemTableAPI.GetIndexNameForField(const AField:TkbmSQLFieldNode):string;
var
   mt:TkbmCustomMemTable;
   idx:TkbmIndex;
begin
     mt:=TkbmCustomMemTable(AField.Table.Data.Data);
     idx:=mt.Indexes.GetByFieldNames(AField.FieldName);
     if idx<>nil then
        Result:=idx.Name
     else
         Result:='';
end;

function TkbmSQLMemTableAPI.GetTableDataClass:TkbmSQLCustomTableDataClass;
begin
     Result:=TkbmSQLMemTableData;
end;

function TkbmSQLMemTableAPI.LocateFirst(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean;
var
   ds:TkbmCustomMemTable;
begin
     ds:=TkbmCustomMemTable(ATable.Data.Data);
     ds.First;
     if ACondition=nil then
     begin
          Result:=not ds.Eof;
          exit;
     end;

     Result:=false;
     while not ds.Eof do
     begin
          if ACondition.Execute=true then
          begin
               Result:=true;
               exit;
          end;
          ds.Next;
     end;
end;

function TkbmSQLMemTableAPI.LocateNext(const ATable:TkbmSQLTable; const ACondition:TkbmSQLCustomNode):boolean;
var
   ds:TkbmCustomMemTable;
begin
     ds:=TkbmCustomMemTable(ATable.Data.Data);
     ds.Next;
     if ACondition=nil then
     begin
          Result:=not ds.Eof;
          exit;
     end;

     Result:=false;
     while not ds.Eof do
     begin
          if ACondition.Execute=true then
          begin
               Result:=true;
               exit;
          end;
          ds.Next;
     end;
end;

end.
