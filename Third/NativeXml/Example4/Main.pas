{
  Test program for the NativeXmlAppend unit

  The NativeXmlAppend unit implements a way to add XML data to *existing* XML files
  on disk *without* loading them completely. This way, one avoids the overhead
  of loading possibly huge XML databases, and one can simply add new records
  to the end.

  Copyright (c) 2003 by Nils Haeck, Simdesign

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, NativeXml, NativeXmlAppend;

type
  TfrmMain = class(TForm)
    Label1: TLabel;
    edFilename: TEdit;
    btnSelect: TButton;
    Label2: TLabel;
    mmAppend: TMemo;
    btnAppend: TButton;
    Label3: TLabel;
    edLevel: TEdit;
    udLevel: TUpDown;
    sbMain: TStatusBar;
    procedure btnSelectClick(Sender: TObject);
    procedure btnAppendClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.btnSelectClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      Title := 'Select XML file';
      Filter := 'XML files (*.xml)|*.xml|All files|*.*';
      if Execute then
        edFilename.Text := FileName;
    finally
      Free;
    end;
end;

procedure TfrmMain.btnAppendClick(Sender: TObject);
var
  AFilename: string;
  ADoc: TNativeXml;
  ALevel: integer;
begin
  // Filename
  AFilename := edFilename.Text;
  if not FileExists(AFilename) then begin
    ShowMessage('File not found');
    exit;
  end;
  Screen.Cursor := crHourglass;
  // Load XML to append
  ADoc := TNativeXml.Create;
  try
    // Load it from the memo
    ADoc.ReadFromString(mmAppend.Text);

    // Find level
    ALevel := udLevel.Position;

    sbMain.SimpleText := 'Appending...';

    // Call our function
    try
      XmlAppendToExistingFile(AFilename, ADoc.Root, ALevel);
      // Arriving here means all OK
      sbMain.SimpleText := 'Append performed OK';
    except
      // Something went wrong
      on E: Exception do begin
        // Display error in the status bar
        sbMain.SimpleText := E.Message;
        raise; //let the user know
      end;
    end;

  finally
    ADoc.Free;
    Screen.Cursor := crDefault;
  end;
end;

end.
