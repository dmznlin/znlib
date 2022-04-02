program kafka;

uses
  FastMM4,
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {fFormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfFormMain, fFormMain);
  Application.Run;
end.
