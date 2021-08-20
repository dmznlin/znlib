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
    procedure Button4Click(Sender: TObject);
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
  UManagerGroup, UParameters;

const
  sParam1 = '0001';
  sParam2 = '0002';
  sParam3 = '0003';

procedure SystemParams(const nList: TList);
begin
  gMG.FParamsManager.Default.Init('System', '系统通用参数').
    SetEffect(etLower).
    SetOptionOnly([dtStr]);
  //设置参数默认值

  gMG.FParamsManager.AddParam(sParam1, '第一参数', nList).
    AddS('aaaa', '', True).
    AddS('bbbb').
    AddS('cccc').
    AddI(1111, '', True).
    AddI(2222).
    AddI(3333).SetEffect(etHigher);
  //first

  gMG.FParamsManager.AddParam(sParam2, '第二参数', nList).
    AddS('aaaa', '', True).
    AddS('bbbb').
    AddS('cccc').
    AddI(1111, '', True).
    AddI(2222).
    AddI(3333);
  //first
end;

procedure TfFormMain.FormCreate(Sender: TObject);
begin
  gMG.FLogManager.StartService();
  gMG.FParamsManager.AddBuilder(SystemParams);
end;

procedure TfFormMain.Button1Click(Sender: TObject);
begin
  gMG.FDBManager.InitDB('MAIN', Memo1.Lines);
  gMG.FParamsManager.InitParameters('dmzn', 'admin', Memo1.Lines);
end;


procedure TfFormMain.Button2Click(Sender: TObject);
var nP: TParamItem;
begin
  nP.Init('System', 'name').FID := sParam3;
  nP.FOwner := 'dmzn';
  nP.FRecord := '031B583720021000';

  nP.FValue.AddS('a');
  nP.FValue.AddS('b');
  nP.FValue.AddS('c');
  nP.FValue.AddI(3);
  nP.FValue.AddF(1.2);
  nP.FValue.AddD(Now);
  gMG.FParamsManager.SaveParam(@nP, 'admin');
end;

procedure TfFormMain.Button3Click(Sender: TObject);
var nP: TParamItem;
begin
  nP.Init('system', 'name');
  gMG.FParamsManager.DeleteParam(@nP);
end;

procedure TfFormMain.Button4Click(Sender: TObject);
begin
  gMG.GetManagersStatus(Memo1.Lines);
end;

end.
