unit UFormMain;

interface

uses
  System.SysUtils, System.Variants, System.Classes, Vcl.Forms, Vcl.Controls,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfFormMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  UManagerGroup, ULibFun, UMgrPopedom;

procedure SystemPopedom(const nList: TList);
begin
  gMG.FPopedomManager.AddGroup(nList).
    AddPopdom('Form1', '´°ÌåA').
    AddPopdom('Form2', '´°ÌåB').
    AddPopdom('Form3', '´°ÌåC');

  gMG.FPopedomManager.AddGroup(nList, 'test', '²âÊÔ×é').
    AddPopdom('Frame1', '¿ò¼ÜA').
    AddPopdom('Frame2', '¿ò¼ÜB').
    AddPopdom('Frame3', '¿ò¼ÜC');
end;

procedure TfFormMain.FormCreate(Sender: TObject);
begin
  gMG.FLogManager.StartService();
  gMG.FPopedomManager.AddBuilder(SystemPopedom);
  gMG.FPopedomManager.AddTag('H', 'µ¥¼Û');
end;

procedure TfFormMain.Button1Click(Sender: TObject);
begin
  gMG.FDBManager.InitDB('MAIN', Memo1.Lines);
end;

procedure TfFormMain.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Add(gPopedomManager.FindTagName(sPopedom_Add));
  Memo1.Lines.Add(gPopedomManager.FindGroupName(sPopedom_Forms));
end;

procedure TfFormMain.Button3Click(Sender: TObject);
var i,nIdx: Integer;
    nList: TList;
    nGroup: PPopedomGroup;
begin
  nList := TList.Create;
  try
    gPopedomManager.GetPopedoms(nList);
    for nIdx := 0 to nList.Count-1 do
    begin
      nGroup := nList[nIdx];
      Memo1.Lines.Add(nGroup.FGroup + ' ' + nGroup.FName);

      for i := Low(nGroup.FItems) to High(nGroup.FItems) do
       with nGroup.FItems[i] do
        Memo1.Lines.Add('  ' + FItemID + ' ' + FName);
    end;
  finally
    gPopedomManager.ClearPopedoms(nList, True);
  end;
end;

end.
