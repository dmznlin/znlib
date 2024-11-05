{ Example 3

  Demo unit that shows how to import XML data into record structure.

  It is a "real-life" example written for one of our customers.

  It shows how to use the method "NodesByName", creating a temporary
  list of node elements with the same name.

  It uses NativeXml to handle the conversion.

  This was the original request for the conversion:

  " Data should be placed in a record with this structure:
  type 
    FieldRecord= record
      FieldId: String;
      FieldReq: String;
      FieldType: String;
      FieldCap: String;
      DependField: String;
      MetaType: String;
      MetaData: String;
    End;
 
  De enums and the hashtables must be stored in MetaData, 
  in the following manner:
 
  Enums: enumvalue_id1 + ',' + enum_caption1 + ";" + 
    enumvalue_id2 + ',' + enum_caption2 + ";" + etc.....

  HashTable: codeid1 + ',' + hashdata1 + ';'  + 
    codeid2 + ',' + hashdata2 + etc......
 
  If there's no MetaData the field should remain empty. "


  Author: Nils Haeck, Simdesign
  http://www.simdesign.nl

  More info at:
  http://www.simdesign.nl/xml.html

}
unit Example3Main;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, NativeXml, ComCtrls;

type

  // This is the record structure
  FieldRecord= record
    FieldId: String;
    FieldReq: String;
    FieldType: String;
    FieldCap: String;
    DependField: String;
    MetaType: String;
    MetaData: String;
  end;

  TForm1 = class(TForm)
    mmXML: TMemo;
    btnImportXML: TButton;
    lbStatus: TLabel;
    Label1: TLabel;
    lvTable: TListView;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnImportXMLClick(Sender: TObject);
    procedure lvTableData(Sender: TObject; Item: TListItem);
  private
  public
    Fields: array of FieldRecord; // This is a dynamic array of records
    procedure LoadFieldFromNode(var AField: FieldRecord; ANode: TXmlNode);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btnImportXMLClick(Sender: TObject);
// Read XML from TMemo and convert into record structure
var
  i: integer;
  ADoc: TNativeXml;
  ANode: TXmlNode;
  AList: TList;
begin
  ADoc := TNativeXml.Create;
  try
    // Read from memo
    ADoc.ReadFromString(mmXML.Text);

    // A temporary list to hold references to the record elements
    AList := TList.Create;
    try
      ANode := ADoc.Root.NodeByName('fields');
      if not assigned(ANode) then exit;

      // List of nodes that are named "field"
      ANode.NodesByName('field', AList);

      // Set dynamic array length
      SetLength(Fields, AList.Count);

      // Import each element
      for i := 0 to AList.Count - 1 do
        LoadFieldFromNode(Fields[i], AList[i]);

      // Show table
      lvTable.Items.Count := AList.Count;
      lvTable.Invalidate;

      // Status
      lbStatus.Caption := Format('Imported %d records', [AList.Count]);

    finally
      AList.Free;
    end;
  finally
    ADoc.Free;
  end;
end;

procedure TForm1.LoadFieldFromNode(var AField: FieldRecord;
  ANode: TXmlNode);
// Load one field from the XML element ANode
var
  i: integer;
  AMeta: TXmlNode;
  AEnum: TXmlNode;
  AList: TList;
begin
  with AField, ANode do begin
    // Initialize record
    FillChar(AField, SizeOf(AField), 0);

    // The flat data
    FieldId      := AttributeByName['id'];
    FieldReq     := AttributeByName['required'];
    FieldType    := AttributeByName['type'];
    FieldCap     := ReadString('caption');
    DependField  := AttributeByName['dependsfield'];

    // Meta data
    AMeta := NodeByName('metadata');
    if assigned(AMeta) then with AMeta do begin
      MetaType := AttributeByName['type'];

      // List that holds enumeration
      AList := TList.Create;
      try

        // "enum" metadata
        if MetaType = 'enum' then begin
          AEnum := NodeByName('enumeration');
          if assigned(AEnum) then
            AEnum.NodesByName('enumvalue', AList);
          // all enumeration values
          for i := 0 to AList.Count - 1 do with TXmlNode(AList[i]) do
            MetaData := MetaData +
              AttributeByName['id'] + ',' + ReadString('caption') + ';';
        end;

        // "hashtable" metadata
        if MetaType = 'hashtable' then begin
          AEnum := NodeByName('hashtable');
          if assigned(AEnum) then
            AEnum.NodesByName('code', AList);
          // all enumeration values
          for i := 0 to AList.Count - 1 do with TXmlNode(AList[i]) do
            MetaData := MetaData +
              AttributeByName['id'] + ',' + ValueAsString + ';';
        end;

      finally
        AList.Free;
      end;
    end;
  end;
end;

procedure TForm1.lvTableData(Sender: TObject; Item: TListItem);
// Add data to each item in the listview. We use the listview with OwnerData
begin
  if (Item.Index >= 0) and (Item.Index < Length(Fields)) then begin
    with Fields[Item.Index] do begin
      Item.Caption := FieldID;
      Item.SubItems.Add(FieldReq);
      Item.SubItems.Add(FieldType);
      Item.SubItems.Add(FieldCap);
      Item.SubItems.Add(DependField);
      Item.SubItems.Add(MetaType);
      Item.SubItems.Add(MetaData);
    end;
  end;
end;

end.
