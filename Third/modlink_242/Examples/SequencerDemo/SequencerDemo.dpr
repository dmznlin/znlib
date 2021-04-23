program SequencerDemo;

{$I ModLink.inc}

uses
  Forms,
  UMainForm in 'UMainForm.pas' {MainForm},
  UModbusSequencer in 'UModbusSequencer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Sequencer Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
