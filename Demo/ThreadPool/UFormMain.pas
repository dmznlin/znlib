unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdTCPServer, Vcl.StdCtrls, Vcl.ExtCtrls, IdTCPConnection,
  IdTCPClient;

type
  TForm1 = class(TForm)
    IdTCPServer1: TIdTCPServer;
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    BtnStatus: TButton;
    BtnStart: TButton;
    BtnStop: TButton;
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure BtnStatusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
  private
    { Private declarations }
    FCounter: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
uses
  UBaseObject, UThreadPool, UManagerGroup;

procedure TForm1.FormCreate(Sender: TObject);
var nIdx: Integer;
    nWorker: TThreadWorkerConfig;
begin
  FCounter := 0;

  for nIdx := 0 to 20 do
  begin
    gMG.FThreadPool.WorkerInit(nWorker);
    with nWorker do
    begin
      FWorkerName := 'Client';
      FParentObj := Self;
      FParentDesc := 'Main';
      FCallInterval := 100;
      FCallMaxTake := 3000;
      FCallTimes := 0;
      FAutoStatus := True;

      FOnInit.WorkRefer := procedure (const nConfig: PThreadWorkerConfig;
        const nThread: TThread)
      begin
        nConfig.FDataObj[0] := TIdTCPClient.Create(nil);
        with TIdTCPClient(nConfig.FDataObj[0]) do
        begin
          ConnectTimeout := 3000;
          ReadTimeout := 3000;
        end;
      end;

      FOnWork.WorkRefer := procedure (const nConfig: PThreadWorkerConfig;
        const nThread: TThread)
      var nClient: TIdTCPClient;
      begin
        nClient := nConfig.FDataObj[0] as TIdTCPClient;
        if not nClient.Connected then
        begin
          nClient.Host := '127.0.0.1';
          nClient.Port := 8000;
          nClient.Connect;
        end;

        nClient.IOHandler.WriteLn('hello');
        if nClient.IOHandler.ReadLn() = 'hello' then
        begin
          Inc(FCounter);
          TThread.Synchronize(nThread,
            procedure
            begin
              Edit1.Text := FCounter.ToString;
            end);
        end;
      end;

      FOnStop.WorkRefer := procedure (const nConfig: PThreadWorkerConfig;
        const nThread: TThread)
      begin
        if Assigned(nConfig.FDataObj[0]) then
          (nConfig.FDataObj[0] as TIdTCPClient).Disconnect;
        //xxxxx
      end;

      FOnFree.WorkRefer := procedure (const nConfig: PThreadWorkerConfig;
        const nThread: TThread)
      begin
        if Assigned(nConfig.FDataObj[0]) then
          FreeAndNil(nConfig.FDataObj[0]);
        //xxxxx
      end;
    end;

    gMG.FThreadPool.WorkerAdd(@nWorker);
  end;
end;

procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
var nStr: string;
begin
  nStr := AContext.Connection.IOHandler.ReadLn();
  AContext.Connection.IOHandler.WriteLn(nStr);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gMG.FThreadPool.WorkerStop(Self);
end;

procedure TForm1.BtnStartClick(Sender: TObject);
begin
  gMG.FThreadPool.WorkerStart(Self)
end;

procedure TForm1.BtnStopClick(Sender: TObject);
begin
  gMG.FThreadPool.WorkerStop(Self);
end;

procedure TForm1.BtnStatusClick(Sender: TObject);
begin
  gMG.GetManagersStatus(Memo1.Lines);
end;

end.
