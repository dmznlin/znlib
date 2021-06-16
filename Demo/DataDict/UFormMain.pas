unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfFormMain = class(TForm)
    Panel1: TPanel;
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
  ULibFun, UManagerGroup, UMgrDataDict, UMenuManager;

procedure TfFormMain.Button1Click(Sender: TObject);
begin
  gMG.FDBManager.InitDB('MAIN', Memo1.Lines);
end;

end.
