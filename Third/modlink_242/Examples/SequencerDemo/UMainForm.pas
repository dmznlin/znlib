unit UMainForm;

{$I ModLink.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ModLink, ExtCtrls, ActnList, StdCtrls, UModbusSequencer;

type
  TMySequenceState = (
    mssIdle,
    mssRead1,
    mssRead2,
    mssWrite1);

  TMainForm = class(TForm)
    ModbusConnection1: TModbusConnection;
    ModbusClient1: TModbusClient;
    ModbusClient2: TModbusClient;
    ActionList1: TActionList;
    StartAction: TAction;
    StopAction: TAction;
    StartButton: TButton;
    StopButton: TButton;
    Memo1: TMemo;
    procedure StartActionExecute(Sender: TObject);
    procedure StartActionUpdate(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSequencer: TModbusSequencer;
    FState: TMySequenceState;
    procedure AttachClients;
    procedure DetachClients;
    procedure LogString(const S: string);
    procedure SequencerAdvanceHandler(ASender: TModbusSequencer);
    procedure SequencerProcessHandler(ASender: TModbusSequencer; const AInfo: TTransactionInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{==============================================================================}
{ TMainForm                                                                    }
{==============================================================================}

procedure TMainForm.AttachClients;
begin
  FSequencer.AttachClient(ModbusClient1);
  FSequencer.AttachClient(ModbusClient2);
end;

{==============================================================================}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FSequencer := TModbusSequencer.Create;
  FSequencer.OnAdvance := SequencerAdvanceHandler;
  FSequencer.OnProcess := SequencerProcessHandler;
end;

{==============================================================================}

destructor TMainForm.Destroy;
begin
  FSequencer.Free;
  inherited;
end;

{==============================================================================}

procedure TMainForm.DetachClients;
begin
  FSequencer.DetachClient(ModbusClient1);
  FSequencer.DetachClient(ModbusClient2);
end;

{==============================================================================}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not StopAction.Enabled then Exit;
  FSequencer.Stop;
  FState := mssIdle;
  DetachClients;
  LogString('*** Sequencer stopped.');
end;

{==============================================================================}

procedure TMainForm.LogString(const S: string);
begin
  Memo1.Lines.Add(S);
end;

{==============================================================================}

procedure TMainForm.SequencerAdvanceHandler(ASender: TModbusSequencer);
begin
  case FState of
    mssRead1:
      begin
        ModbusClient1.ReadHoldingRegisters(100, 1);
        LogString('READ1 init. Waiting...');
      end;
    mssRead2:
      begin
        ModbusClient2.ReadHoldingRegisters(200, 1);
        LogString('READ2 init. Waiting...');
      end;
    mssWrite1:
      begin
        ModbusClient1.WriteMultipleRegisters(300, [1234]);
        LogString('WRITE1 init. Waiting...');
      end;
  end;
end;

{==============================================================================}

procedure TMainForm.SequencerProcessHandler(ASender: TModbusSequencer;
  const AInfo: TTransactionInfo);
const
  CStateMessages: array [Boolean] of string = (
    'failed',
    'succeeded'
  );
  CStatusFormat = '%s has %s.';
var
  LSuccess: Boolean;
begin
  LSuccess := AInfo.Reply = srNormalReply;
  case FState of
    mssRead1:
      begin
        LogString(Format(CStatusFormat, ['READ1', CStateMessages[LSuccess]]));
        FState := mssRead2;
      end;
    mssRead2:
      begin
        LogString(Format(CStatusFormat, ['READ2', CStateMessages[LSuccess]]));
        FState := mssWrite1;
      end;
    mssWrite1:
      begin
        LogString(Format(CStatusFormat, ['WRITE1', CStateMessages[LSuccess]]));
        FState := mssRead1;
      end;
  end;
end;

{==============================================================================}

procedure TMainForm.StartActionExecute(Sender: TObject);
begin
  LogString('*** Sequencer started.');
  AttachClients;
  FState := mssRead1;
  FSequencer.Start;
end;

{==============================================================================}

procedure TMainForm.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not FSequencer.Running;
end;

{==============================================================================}

procedure TMainForm.StopActionExecute(Sender: TObject);
begin
  FSequencer.Stop;
  FState := mssIdle;
  DetachClients;
  LogString('*** Sequencer stopped.');
end;

{==============================================================================}

procedure TMainForm.StopActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FSequencer.Running;
end;

{==============================================================================}

end.
