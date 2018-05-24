program Example3;

uses
  Forms,
  Example3Main in 'Example3Main.pas' {Form1},
  NativeXml in '..\Source\NativeXml.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
