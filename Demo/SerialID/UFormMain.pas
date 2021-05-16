unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfFormMain = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fFormMain: TfFormMain;

implementation

{$R *.dfm}

uses
  UDBFun, ULibFun;

procedure TfFormMain.Button1Click(Sender: TObject);
begin
  with Memo1.Lines,TDBCommand,TDateTimeHelper do
  begin
    Clear;
    Add('ServerNow: ' + #9 + DateTime2Str(ServerNow()));
    Add('SerialID: ' + #9 + SerialID('group', 'item'));
    Add('Snowflake: ' + #9 + SnowflakeID);
  end;
end;

end.
