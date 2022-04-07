unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  UKafkaManager;

type
  TfFormMain = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Btn1: TButton;
    Btn2: TButton;
    MemoTopic: TMemo;
    MemoMain: TMemo;
    Btn3: TButton;
    Btn4: TButton;
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Btn3Click(Sender: TObject);
    procedure Btn4Click(Sender: TObject);
  private
    { Private declarations }
    procedure OnLog(const nEvent: string);
    procedure OnMsg(const nConsumer: PKKConsumer; const nMsg: PKKMessage);
  public
    { Public declarations }
  end;

var
  fFormMain: TfFormMain;

implementation

{$R *.dfm}
uses
  UManagerGroup, ULibFun, Kafka.Lib, UKafkaTypes;

procedure TfFormMain.FormCreate(Sender: TObject);
var nIdx: Integer;
begin
  Caption := 'SDK: ' + TKafkaHelper.Version();
  MemoTopic.Clear();

  with TTopicConfig do
  for nIdx := Low(ConfigKeys) to High(ConfigKeys) do
    MemoTopic.Lines.Add(ConfigKeys[nIdx] + '=');
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

procedure TfFormMain.OnMsg(const nConsumer: PKKConsumer; const nMsg: PKKMessage);
begin
  Memo1.Lines.Add('Key: ' + TEncoding.UTF8.GetString(nMsg.FKey) + #13#10 +
                  'Data: ' + TEncoding.UTF8.GetString(nMsg.FData));
  //xxxxx
end;

procedure TfFormMain.Btn1Click(Sender: TObject);
var nKafka: TKKConfig;
    nTopic: TKKTopicConfig;
    nItems: TKafkaHelper.TConfigItems;
    nClient: TKKClient;
begin
  with TKafkaHelper do
  begin
    BuildConfig(MemoMain.Text, nItems);
    nKafka := ConfigNew(nItems);
    //1.创建kafka配置

    nClient := ClientNew(RD_KAFKA_CONSUMER, nItems);
    if ClientType(nClient) = RD_KAFKA_CONSUMER then
      Log('RD_KAFKA_CONSUMER OK.');
    //xxxxx

    TKafkaHelper.BuildConfig(MemoTopic.Text, nItems);
    nTopic := TKafkaHelper.TopicConfigNew(nItems);
    //2.创建kafka topic的配置


    if Assigned(nClient) then
      ClientClose(nClient);
    ConfigDestroy(nKafka);
  end;
end;

procedure TfFormMain.Btn2Click(Sender: TObject);
begin
  gMG.GetManagersStatus(Memo1.Lines);
  TKafkaHelper.Log('log test');
  TKafkaHelper.Log('log test', ltConsumer);
end;

procedure TfFormMain.Btn3Click(Sender: TObject);
var nConsumer: TKKConsumer;
begin
  nConsumer := gKafkaManager.ConsumerInit('test', Self);
  nConsumer.FOnMsgEventUI := OnMsg;
  gKafkaManager.ConsumerAdd(@nConsumer);
  Memo1.Lines.Add(nConsumer.Ident.toString);
end;

procedure TfFormMain.Btn4Click(Sender: TObject);
begin
  gKafkaManager.SaveDefault();
end;

end.
