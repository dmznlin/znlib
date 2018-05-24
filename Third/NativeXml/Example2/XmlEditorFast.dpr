program XmlEditorFast;

uses
  FastMM,
  Forms,
  XmlEditorMainFast in 'XmlEditorMainFast.pas' {frmMain},
  sdFastXml in '..\Source\sdFastXml.pas',
  sdStringTable in '..\..\lib\units\General\sdStringTable.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
