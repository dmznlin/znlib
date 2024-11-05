program XmlEditor;

uses
  FastMM,
  Forms,
  Main in 'Main.pas' {frmMain},
  NativeXml in '..\Source\NativeXml.pas',
  VirtualTrees in '..\..\..\lib\VirtualTreeview_443\Source\VirtualTrees.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
