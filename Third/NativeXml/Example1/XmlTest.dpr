program XmlTest;

uses
  FastMM,
  Forms,
  NativeXml in '..\source\nativexml.pas',
  XmlTestMain in 'XmlTestMain.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
