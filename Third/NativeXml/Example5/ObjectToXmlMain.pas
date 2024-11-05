{ unit ObjectToXmlMain

  This demo unit illustrates how to use the NativeXmlObjectStorage unit in order
  to store any TObject descendant to an XML file or stream.

  Copyright (c) 2004 - 2006 Simdesign B.V., Author Nils Haeck M.Sc.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit ObjectToXmlMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, NativeXml, NativeXmlObjectStorage, Menus, ComCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    mmXml: TMemo;
    Label2: TLabel;
    btnSaveImage: TButton;
    PopupMenu1: TPopupMenu;
    mnuTest: TMenuItem;
    Image2: TImage;
    Image1: TImage;
    Label3: TLabel;
    btnLoadImage: TButton;
    StatusBar1: TStatusBar;
    btnSaveForm: TButton;
    btnLoadForm: TButton;
    btnClear: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnuLoad: TMenuItem;
    mnuSave: TMenuItem;
    mnuExit: TMenuItem;
    btnNewFormJustLabels: TButton;
    procedure btnSaveImageClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure mnuTestClick(Sender: TObject);
    procedure btnSaveFormClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnLoadFormClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuLoadClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnNewFormJustLabelsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  published
    // Used for testing
    property Image: TImage read Image1 write Image1;
    property StatusBar: TStatusbar read StatusBar1 write StatusBar1;
  end;

var
  Form1: TForm1;
  glFormCount: integer = 1;


implementation

{$R *.DFM}

procedure TForm1.btnSaveImageClick(Sender: TObject);
var
  ADoc: TNativeXml;
  AWriter: TsdXmlObjectWriter;
begin
  // Create XML document with root named "Root"
  ADoc := TNativeXml.CreateName('Root');
  try
    // We set the XML document to UTF8 so widestrings are preserved
    ADoc.Utf8Encoded := True;
    ADoc.EncodingString := 'UTF-8';
    // Create Object writer
    AWriter := TsdXmlObjectWriter.Create;
    try
      // Write the image object as child of the XML document's root node
      AWriter.WriteComponent(ADoc.Root, Image1, Self);
    finally
      AWriter.Free;
    end;
    // Display resulting XML in memo
    ADoc.XmlFormat := xfReadable;
    mmXml.Clear;
    mmXml.Lines.Text := ADoc.WriteToString;
  finally
    ADoc.Free;
  end;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  mmXML.Clear;
  mmXML.Lines.Add('Image clicked!');
end;

procedure TForm1.mnuTestClick(Sender: TObject);
begin
  mmXML.Clear;
  mmXML.Lines.Add('Popup test menu clicked!');
end;

procedure TForm1.btnSaveFormClick(Sender: TObject);
var
  ADoc: TNativeXml;
  AWriter: TsdXmlObjectWriter;
begin
  // You could use the highlevel call "FormSaveToXmlString" here, but this code
  // demonstrates how to do it manually

  // Create XML document
  ADoc := TNativeXml.CreateName('Root');
  try
    // We set the XML document to UTF8 so widestrings are preserved
    ADoc.Utf8Encoded := True;
    ADoc.EncodingString := 'UTF-8';
    // Create Object writer
    AWriter := TsdXmlObjectWriter.Create;
    try
      // Write the form object as child of the XML document's root node
      AWriter.WriteComponent(ADoc.Root, Self, Self);
    finally
      AWriter.Free;
    end;
    // Display in memo
    ADoc.XmlFormat := xfReadable;
    mmXml.Clear;
    mmXml.Lines.Text := ADoc.WriteToString;
  finally
    ADoc.Free;
  end;
end;

procedure TForm1.btnLoadImageClick(Sender: TObject);
var
  ADoc: TNativeXml;
  AReader: TsdXmlObjectReader;
  OldPos: TPoint;
begin
  // You could also call "ObjectLoadFromXmlString" here, but this code demonstrates
  // how to do it manually

  // Create XML document
  ADoc := TNativeXml.Create;
  try
    // Read the XML from the memo
    ADoc.ReadFromString(mmXml.Lines.Text);
    // Create Object reader
    AReader := TsdXmlObjectReader.Create;
    try
      // Read the image object as child of the XML document's root node
      OldPos := Point(Image2.Left, Image2.Top);
      AReader.ReadComponent(ADoc.Root, Image2, Self);
      // We must set the image to the location it was otherwise it exactly covers
      // Image1
      Image2.Left := OldPos.X;
      Image2.Top  := OldPos.Y;
    finally
      AReader.Free;
    end;
  finally
    ADoc.Free;
  end;
end;

procedure TForm1.btnLoadFormClick(Sender: TObject);
var
  ADoc: TNativeXml;
  AReader: TsdXmlObjectReader;
  AForm: TForm;
begin
  // Create XML document
  ADoc := TNativeXml.Create;
  try
    // Read the XML from the memo
    ADoc.ReadFromString(mmXml.Lines.Text);
    // Create Object reader
    AReader := TsdXmlObjectReader.Create;
    try
      // Read the form as child of the XML document's root node
      inc(glFormCount);
      AForm := TForm(AReader.CreateComponent(ADoc.Root, Application, nil,
        Format('Form%d', [glFormCount])));

      // Offset the form a bit
      AForm.Left := AForm.Left + 20;
      AForm.Top  := AForm.Top  + 20;
      AForm.Show;
    finally
      AReader.Free;
    end;
  finally
    ADoc.Free;
  end;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  mmXml.Clear;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.mnuLoadClick(Sender: TObject);
// What we do here is load form data from an XML file and create it on the fly
// as a new form. You can actually edit the XML data before with another application
// to create whatever fancy form you like.
var
  AForm: TForm;
begin
  // We must register the class we try to create..
  RegisterClass(TForm1);

  with TOpenDialog.Create(Application) do
    try
      Title := 'Select XML file to load';
      Filter := 'XML files (*.xml)|*.xml';
      if Execute then begin
        // Ensure unique name by counting up
        inc(glFormCount);
        AForm := FormCreateFromXmlFile(FileName, Application,
          Format('Form%d', [glFormCount]));

        // Offset the form a bit
        if assigned(AForm) then begin
          AForm.Left := AForm.Left + 20;
          AForm.Top  := AForm.Top  + 20;

          // And show it
          AForm.Show;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TForm1.mnuSaveClick(Sender: TObject);
// What we do here is create a copy of our form and put it in an XML file specified
// by the user.
begin
  with TSaveDialog.Create(Application) do
    try
      Title := 'Save form as XML';
      Filter := 'XML files (*.xml)|*.xml';
      DefaultExt := '.xml';
      if Execute then
        FormSaveToXmlFile(Self, FileName);
    finally
      Free;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // If using CreateComponent, it is neccesary that all used component classes
  // are registered
  RegisterClasses([TForm1, TImage, TLabel, TMemo, TButton, TStatusBar,
    TPopupMenu, TMenuItem, TCheckBox, TMainMenu]);
end;

procedure TForm1.btnNewFormJustLabelsClick(Sender: TObject);
var
  i: integer;
  NewForm: TForm;
  ADoc: TNativeXml;
  Comps: TXmlNode;
  AList: TList;
  ALabel: TLabel;
begin
  // Create new form
  NewForm := TForm.Create(Application);
  // Copy size and displace slightly
  NewForm.Left := Self.Left + 10;
  NewForm.Top := Self.Top + 10;
  NewForm.Width := Self.Width;
  NewForm.Height := Self.Height;
  // New XML document
  ADoc := TNativeXml.Create;
  try
    // Load from the memo
    ADoc.ReadFromString(mmXml.Lines.Text);
    if not assigned(ADoc.Root) then exit;
    // Check component list
    Comps := ADoc.Root.NodeByName('Components');
    if not assigned(Comps) then exit;
    // Find all TLabel objects
    AList := TList.Create;
    try
      Comps.NodesByName('TLabel', AList);
      // Loop through labels, and create each label from the XML node
      for i := 0 to AList.Count - 1 do begin
        ALabel := TLabel(ComponentCreateFromXmlNode(TXmlNode(AList[i]), NewForm,
          TXmlNode(AList[i]).AttributeByName['Name']));
        ALabel.Parent := NewForm;
      end;
    finally
      AList.Free;
    end;
  finally
    ADoc.Free;
  end;
  // Now show the form with just the labels copied from the other form's XML
  NewForm.Show;
end;

end.
