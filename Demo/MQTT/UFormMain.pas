unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  Vcl.StdCtrls, System.Classes, Vcl.Controls, Vcl.Forms, UMosMQTT;

type
  TfFormMain = class(TForm)
    Edit1: TEdit;
    BtnSub: TButton;
    Edit2: TEdit;
    BtnPub: TButton;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    BtnUnsub: TButton;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    CheckBox2: TCheckBox;
    BtnStatus: TButton;
    BtnNewTopic: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnSubClick(Sender: TObject);
    procedure BtnPubClick(Sender: TObject);
    procedure BtnUnsubClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BtnStatusClick(Sender: TObject);
    procedure BtnNewTopicClick(Sender: TObject);
  private
    { Private declarations }
    FMQTTClient: TMQTTClient;
    procedure WriteLog(const nEvent: string);
    procedure DoMessageEvent(const nTopic,nPayload: string);
  public
    { Public declarations }
  end;

var
  fFormMain: TfFormMain;

implementation

{$R *.dfm}
uses
  UMosquitto, ULibFun, UManagerGroup, vcl.PostMsg, UMosMessager;

procedure DoMessage(const nTopic, nPayload: string);
begin
  fFormMain.WriteLog(nTopic + ',' + nPayload);
end;

procedure TfFormMain.FormCreate(Sender: TObject);
begin
  with TApplicationHelper do
  begin
    gPath := ExtractFilePath(Application.ExeName);

    gMG.FLogManager.SyncMainUI := True;
    gMG.FLogManager.SyncSimple := WriteLog;
    gMG.FLogManager.StartService(gPath + 'Logs\');
  end;

  FMQTTClient := gMG.FMessageCenter.Channel;
  with FMQTTClient do
  begin
    KeepAlive := 10;
    DetailLog := True;
    OnMessage := DoMessage;
    OnMessageEvent := DoMessageEvent;
  end;

  DoMessage(IntToStr(Ord(mtInfo)), '11');
end;

procedure TfFormMain.DoMessageEvent(const nTopic, nPayload: string);
begin
  Memo1.Lines.Add(nTopic + #9 + nPayload);
end;

procedure TfFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gMG.RunBeforApplicationHalt;
end;

procedure TfFormMain.WriteLog(const nEvent: string);
begin
  Memo1.Lines.Add(nEvent)
end;

procedure TfFormMain.BtnSubClick(Sender: TObject);
begin
  FMQTTClient.AddTopic(Edit1.Text);
end;

procedure TfFormMain.BtnPubClick(Sender: TObject);
begin
  FMQTTClient.Publish(edit1.Text, edit2.Text);
end;

procedure TfFormMain.BtnUnsubClick(Sender: TObject);
begin
    FMQTTClient.DelTopic(Edit1.Text);
end;

procedure TfFormMain.BtnStatusClick(Sender: TObject);
begin
  gMG.GetManagersStatus(Memo1.Lines);
end;

procedure TfFormMain.BtnNewTopicClick(Sender: TObject);
var nRecv: TMsgReceiver;
begin
  gMG.FMessageCenter.InitReceiver(@nRecv);
  Edit1.Text := gMG.FMessageCenter.MakeTopic(@nRecv);
end;

procedure TfFormMain.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  with FMQTTClient do
  begin
    ServerHost := Edit3.Text;
    ServerPort := StrToInt(Edit4.Text);
    UserName := Edit5.Text;
    Password := Edit6.Text;
    ConnectBroker;
  end else FMQTTClient.Disconnect;
end;

procedure TfFormMain.CheckBox2Click(Sender: TObject);
begin
  FMQTTClient.DetailLog := CheckBox2.Checked;
end;

end.
