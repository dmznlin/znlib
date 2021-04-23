program ModLinkDemo;

{$I ModLink.inc}

//--------------------------------------------------------------------------------------------------

uses
  Forms,
  ModLinkDemoMain in 'ModLinkDemoMain.pas' {ModLinkDemoMainForm},
  ServerItemEditor in 'ServerItemEditor.pas' {ServerItemEditorForm},
  ServerItem in 'ServerItem.pas';

//--------------------------------------------------------------------------------------------------

{$R *.res}

//--------------------------------------------------------------------------------------------------

begin
  Application.Initialize;
  Application.Title := 'ModLink VCL Demo';
  Application.CreateForm(TModLinkDemoMainForm, ModLinkDemoMainForm);
  Application.Run;
end.
