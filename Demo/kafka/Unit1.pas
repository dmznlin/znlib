unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls;

type
  TfFormMain = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Btn1: TButton;
    Btn2: TButton;
    MemoTopic: TMemo;
    MemoMain: TMemo;
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure OnLog(const nEvent: string);
  public
    { Public declarations }
  end;

var
  fFormMain: TfFormMain;

implementation

{$R *.dfm}
uses
  UManagerGroup, Kafka.Lib, UKafkaTypes;

procedure TfFormMain.FormCreate(Sender: TObject);
var nIdx: Integer;
begin
  for nIdx := Memo1.Lines.Count-1 downto 0 do
    MemoMain.Lines.Add(Memo1.Lines[nIdx] + '=0');
  //xxxxx

  with gMG.FLogManager do
  begin
    SyncMainUI := True;
    SyncSimple := OnLog;
    StartService();
  end;
end;

procedure TfFormMain.OnLog(const nEvent: string);
begin
  Memo1.Lines.Add(nEvent);
end;

procedure TfFormMain.Btn1Click(Sender: TObject);
var nKcfg: TKafkaConfig;
    nTcfg: TKafkaTopicConfig;
    nItems: TKafkaHelper.TConfigItems;
begin
  with TKafkaHelper do
  begin
    nKcfg := ConfigNew();

    BuildConfig(MemoMain.Text, nItems);
    ConfigSet(nKcfg, nItems, False);
    ConfigDestroy(nKcfg);
  end;
end;

procedure TfFormMain.Btn2Click(Sender: TObject);
begin
  gMG.GetManagersStatus(Memo1.Lines);
  TKafkaHelper.Log('log test');
  TKafkaHelper.Log('log test', ltConsumer);
end;

end.
