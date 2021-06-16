program MQTT;

uses
  FastMM4,
  Vcl.Forms,
  UFormMain in 'UFormMain.pas' {fFormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfFormMain, fFormMain);
  Application.Run;
end.
