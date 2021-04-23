program BasicClientDemo;

{$I ModLink.inc}

uses
  Forms,
  UMainForm in 'UMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Basic Client Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
