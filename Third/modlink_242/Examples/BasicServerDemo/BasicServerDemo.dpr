program BasicServerDemo;

{$I ModLink.inc}

uses
  Forms,
  UMainForm in 'UMainForm.pas' {MainForm},
  UModbusRegisterList in 'UModbusRegisterList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Basic Server Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
