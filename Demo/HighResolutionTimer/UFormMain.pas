unit UFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin;

type
  TfFormMain = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    BtnDel: TButton;
    Edit3: TEdit;
    Memo1: TMemo;
    BtnAdd: TButton;
    Timer1: TTimer;
    BtnStop: TButton;
    BtnStart: TButton;
    SpinEdit1: TSpinEdit;
    procedure BtnDelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnAddClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
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
  UWaitItem, UManagerGroup, UThreadPool;

procedure TfFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gMG.FThreadPool.WorkerDelete(Self);
  //gMG.FThreadPool.RegistMe(False);
  //gMG.RegistAll(False);
end;

procedure TfFormMain.FormCreate(Sender: TObject);
begin
  gMG.FThreadPool.ThreadMin := 1;
  gMG.FThreadPool.ThreadMax := 5;
  //gMG.FThreadPool.NumRunSleep := 0;
  SpinEdit1.MaxValue := cThreadMax;
  SpinEdit1.MinValue := cThreadMin;
  SpinEdit1.Value := gMG.FThreadPool.ThreadMin;
end;

procedure TfFormMain.SpinEdit1Change(Sender: TObject);
begin
  gMG.FThreadPool.ThreadMin := SpinEdit1.Value;
end;

procedure TfFormMain.Timer1Timer(Sender: TObject);
begin
  Memo1.Lines.Clear;
  gMG.FSerialIDManager.GetStatus(Memo1.Lines);
  gMG.FThreadPool.GetStatus(Memo1.Lines);
end;

procedure TfFormMain.BtnDelClick(Sender: TObject);
begin
  gMG.FThreadPool.WorkerDelete(Self);
end;

procedure TfFormMain.BtnAddClick(Sender: TObject);
var nIdx: Integer;
    nInt,nInt2,nInt3: Int64;
    nWorker: TThreadWorkerConfig;
begin
  for nIdx := 0 to 2 do
  begin
    gMG.FThreadPool.WorkerInit(nWorker);
    with nWorker do
    begin
      FWorkerName := '测试高速计时器';
      FParentObj := Self;
      FParentDesc := '主窗口';
      FDataInt[0] := nIdx;
      FCallInterval := 1000;
      FAutoStatus := True;
      FWorkStatus := twsSleep;
      FCallMaxTake := 500;
      //FCallTimes := 100;

      FOnWork.WorkRefer := procedure (const nConfig: PThreadWorkerConfig;
       const nThread: TThread)
      begin
        TWaitTimer.StartHighResolutionTimer;
        sleep(1);
        TWaitTimer.StartHighResolutionTimer;
        sleep(2);
        TWaitTimer.StartHighResolutionTimer;
        Sleep(Random(3000));
        nInt := TWaitTimer.GetHighResolutionTimerResult;
        nInt2 := TWaitTimer.GetHighResolutionTimerResult;
        nInt3 := TWaitTimer.GetHighResolutionTimerResult;

        if not (csFixups in Self.ComponentState) then
        TThread.Synchronize(nThread, procedure
        begin
          case nConfig.FDataInt[0] of
           0: fFormMain.Edit1.Text := IntToStr(nInt) + '.' + IntToStr(nInt2) + '.' + IntToStr(nInt3);
           1: fFormMain.Edit2.Text := IntToStr(nInt) + '.' + IntToStr(nInt2) + '.' + IntToStr(nInt3);
           2: fFormMain.Edit3.Text := IntToStr(nInt) + '.' + IntToStr(nInt2) + '.' + IntToStr(nInt3);
          end;
        end);
      end;
    end;

    gMG.FThreadPool.WorkerAdd(@nWorker);
  end;
end;

procedure TfFormMain.BtnStopClick(Sender: TObject);
begin
  gMG.FThreadPool.WorkerStop(Self);
end;

procedure TfFormMain.BtnStartClick(Sender: TObject);
begin
  gMG.FThreadPool.WorkerStart(Self, 10);
end;

end.
