{
  This simple example shows how to use NativeXml to load an XML file, and
  shows how to do this in a few different ways.

  This example also contains benchmarking tests.

  Copyright (c) 2003 - 2004 Nils Haeck, SimDesign B.V.

}
unit XmlTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, NativeXml;

type
  TForm1 = class(TForm)
    btnLoadUseEvents: TButton;
    Memo1: TMemo;
    btnLoad: TButton;
    btnLoad1st2nd: TButton;
    edXmlFileOpen: TEdit;
    btnSelectOpen: TButton;
    btnCreate: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edXmlFileSave: TEdit;
    btnSelectSave: TButton;
    btnAddStyleSheet: TButton;
    btnAbort: TButton;
    procedure btnLoadUseEventsClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnLoad1st2ndClick(Sender: TObject);
    procedure btnSelectOpenClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnSelectSaveClick(Sender: TObject);
    procedure btnAddStyleSheetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
  private
    FXml: TNativeXml;
    procedure DoNodeNew(Sender: TObject; Node: TXmlNode);
    procedure DoNodeLoaded(Sender: TObject; Node: TXmlNode);
  public
    property Xml: TNativeXml read FXml;
  end;

  // A bogus stream type, that just does not add any overhead, because using
  // a memory stream for testing adds a lot of overhead in the memory allocation
  TMeasureStream = class(TStream)
  private
    FPos: longint;
    FSize: longint;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function Indent(ACount: integer): string;
// Helper routine to make an indented string
begin
  while ACount > 0 do begin
    Result := Result + '  ';
    dec(ACount);
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create a TNativeXml instance
  FXml := TNativeXml.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Free TNativeXml instance
  FreeAndNil(FXml);
end;

procedure TForm1.btnLoadClick(Sender: TObject);
// Load an XML document the usual way
begin
  // Clear the memo first
  Memo1.Lines.Clear;
  // Clear Xml document
  FXml.Clear;
  // Load it from the filename in the edXmlFile editbox
  FXml.LoadFromFile(edXmlFileOpen.Text);
  // Set format to readable so we have a nice layout in the memo later
  FXml.XmlFormat := xfReadable;
  // Write to a string and assign to the memo
  Memo1.Lines.Text := FXml.WriteToString;
  //Memo1.Lines.Text := 'Finished!';
end;

procedure TForm1.btnLoadUseEventsClick(Sender: TObject);
// Load an XML document and show it through events. So for big documents you
// will see the progress of loading.
begin
  // Clear the memo first
  Memo1.Lines.Clear;
  // Clear Xml document
  FXml.Clear;
  // Set the events
  FXml.OnNodeNew    := DoNodeNew;
  FXml.OnNodeLoaded := DoNodeLoaded;
  // We will use an UTF8 encoding internally
  FXml.Utf8Encoded := True;
  // Load the file, and it will invoke the events for every node that it
  // reads in
  FXml.LoadFromFile(edXmlFileOpen.Text);
end;

procedure TForm1.DoNodeLoaded(Sender: TObject; Node: TXmlNode);
// The event which is called after a node is loaded completely (so also all its
// sub-nodes)
begin
  Memo1.Lines.Add(Format('Loaded: %sName=%s, Value=%s', [Indent(Node.TreeDepth), Node.Name, Node.ValueAsString]));
  if Node.TreeDepth > 0 then
    Node.Delete;
end;

procedure TForm1.DoNodeNew(Sender: TObject; Node: TXmlNode);
// The event which is called when a new node is read from the stream, only the
// first tag is read.
begin
  Memo1.Lines.Add(Format('New   : %sName=%s', [Indent(Node.TreeDepth), Node.Name]));
end;

procedure TForm1.btnLoad1st2ndClick(Sender: TObject);
// Load an XML document and show the nodes present by enumerating them. Here
// we do only levels 1 and 2. An iterative approach could show all levels deep
var
  i, j: integer;
  NodeLevel1, NodeLevel2: TXmlNode;
begin
  // Clear the memo and create instance
  Memo1.Lines.Clear;
  FXml.Clear;
  // Load the XML file
  FXml.LoadFromFile(edXmlFileOpen.Text);
  // The Root property contains the root node, we use it as a base
  if assigned(FXml.Root) then
  begin
    // Iterate through all the child nodes of Root (level 1)
    for i := 0 to FXml.Root.NodeCount - 1 do
    begin
      NodeLevel1 := FXml.Root.Nodes[i];
      // Add the name of each child to the memo
      Memo1.Lines.Add(NodeLevel1.Name);
      // Also iterate through the grandchilds (level 2)
      for j := 0 to NodeLevel1.NodeCount - 1 do
      begin
        NodeLevel2 := NodeLevel1.Nodes[j];
        // Add these names too, with an indent
        Memo1.Lines.Add(' ' + NodeLevel2.Name);
      end;
    end;
  end;
end;

procedure TForm1.btnSelectOpenClick(Sender: TObject);
// Open dialog to select an XML file
begin
  with TOpenDialog.Create(Application) do
    try
      Title := 'Select an XML file to open';
      Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
      if Execute then begin
        edXmlFileOpen.Text := FileName;
      end;
    finally
      Free;
    end;
end;

procedure TForm1.btnSelectSaveClick(Sender: TObject);
// Save dialog to select an XML file
begin
  with TSaveDialog.Create(Application) do
    try
      Title := 'Select an XML file to save';
      Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
      if Execute then begin
        edXmlFileSave.Text := FileName;
      end;
    finally
      Free;
    end;
end;

procedure TForm1.btnCreateClick(Sender: TObject);
// This procedure shows how to create an XML document. It also does some
// benchmarking, creating 20.000 nodes on the fly and shows how much time
// the creation and storage process takes.
var
  i, NodeCount: integer;
  Tick: dword;
  ADoc: TNativeXml;
  M: TStream;
  S: string;
begin
  // Set nodecount for testing
  NodeCount := 100000;

  Tick := GetTickCount;
  Memo1.Clear;
  Memo1.Lines.Add(Format('Creating an XML document with %d nodes...', [NodeCount]));
  // Create a new XML document with root named 'root'
  ADoc := TNativeXml.CreateName('root');
  try
    // Add NodeCount nodes to this root
    for i := 1 to NodeCount do
      // NodeNew will add the node with name 'NodeName' and returns a pointer to it
      // that can be used in a 'with' statement like here
      with ADoc.Root.NodeNew('NodeName') do
        // We set the text value of the node to 'NodeValue'
        ValueAsString := 'NodeValue';
    Memo1.Lines.Add(Format('Creation time: %3.3f sec', [(GetTickCount - Tick)/1000]));

    // Save the document to a file
    Tick := GetTickCount;
    ADoc.SaveToFile(edXmlFileSave.Text);
    Memo1.Lines.Add(Format('Storage time (file): %3.3f sec', [(GetTickCount - Tick)/1000]));
    // Save the document to a string
    Tick := GetTickCount;
    S := ADoc.WriteToString;
    Memo1.Lines.Add(Format('Storage time (string): %3.3f sec', [(GetTickCount - Tick)/1000]));
    S := '';

    // Save the document to a stream
    Tick := GetTickCount;
    M := TMeasureStream.Create;
    try
      ADoc.SaveToStream(M);
      Memo1.Lines.Add(Format('Storage time (stream): %3.3f sec', [(GetTickCount - Tick)/1000]));
      Memo1.Lines.Add(Format('File size: %d bytes', [M.Size]));
    finally
      M.Free;
    end;

    // Test clearing time
    Tick := GetTickCount;
    ADoc.Root.Clear;
    Memo1.Lines.Add(Format('Clearing time: %3.3f sec', [(GetTickCount - Tick)/1000]));

    // Test loading time (file)
    Tick := GetTickCount;
    ADoc.LoadFromFile(edXmlFileSave.Text);
    Memo1.Lines.Add(Format('Loading time (file): %3.3f sec', [(GetTickCount - Tick)/1000]));

    // Test loading time (stream)
    M := TMemoryStream.Create;
    try
      TMemoryStream(M).LoadFromFile(edXmlFileSave.Text);
      M.Position := 0;
      ADoc.Clear;
      Tick := GetTickCount;
      ADoc.LoadFromStream(M);
      Memo1.Lines.Add(Format('Loading time (stream): %3.3f sec', [(GetTickCount - Tick)/1000]));
    finally
      M.Free;
    end;

  finally
    // Don't forget to free the document once you're done. This is best done
    // using a try..finally..end construct like here.
    ADoc.Free;
  end;
  Memo1.Lines.Add('Done.');
end;

{ TMeasureStream }

function TMeasureStream.Read(var Buffer; Count: Integer): Longint;
begin
  inc(FPos, Count);
  Result := Count;
end;

function TMeasureStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
  soFromBeginning: FPos := Offset;
  soFromCurrent:   FPos := FPos + Offset;
  soFromEnd:       FPos := FSize + Offset;
  end;
  Result := FPos;
end;

function TMeasureStream.Write(const Buffer; Count: Integer): Longint;
begin
  inc(FPos, Count);
  if FSize < FPos then FSize := FPos;
  Result := Count;
end;

procedure TForm1.btnAddStyleSheetClick(Sender: TObject);
var
  ADoc: TNativeXml;
begin
  ADoc := TNativeXml.CreateName('root');
  try
    with ADoc.StyleSheetNode do begin
      AttributeAdd('type', 'text/css');
      AttributeAdd('href', 'styles.css');
    end;
    ADoc.SaveToFile('test.xml');
  finally
    ADoc.Free;
  end;
end;

procedure TForm1.btnAbortClick(Sender: TObject);
begin
  FXml.AbortParsing := True;
end;

end.
