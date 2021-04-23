unit UModbusSequencer;

{==============================================================================}

{$I ModLink.inc}

{==============================================================================}

interface

{==============================================================================}

uses
  Classes,
  ModLink;

{==============================================================================}

type
  TModbusSequencer = class; { forward declaration }

  TModbusSequencerAdvanceEvent = procedure (ASender: TModbusSequencer) of object;

  TModbusSequencerProcessEvent = procedure (ASender: TModbusSequencer;
    const AInfo: TTransactionInfo) of object;

  TModbusSequencer = class
  private
    FClients: TStringList;
    FOnAdvance: TModbusSequencerAdvanceEvent;
    FOnProcess: TModbusSequencerProcessEvent;
    FRunning: Boolean;
    procedure TransactionProcessedHandler(ASender: TModbusClient;
      const AInfo: TTransactionInfo; ACommand: Byte; ACustom: Boolean);
  protected
    procedure DoAdvance; dynamic;
    procedure DoProcess(const AInfo: TTransactionInfo); dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AttachClient(AClient: TModbusClient);
    procedure DetachClient(AClient: TModbusClient);
    procedure Start;
    procedure Stop;
    property OnAdvance: TModbusSequencerAdvanceEvent read FOnAdvance write FOnAdvance;
    property OnProcess: TModbusSequencerProcessEvent read FOnProcess write FOnProcess;
    property Running: Boolean read FRunning;
  end;

{==============================================================================}

implementation

{==============================================================================}

uses
  SysUtils;

{==============================================================================}
{ Helper routine                                                               }
{==============================================================================}

function HashClientReference(AClient: TModbusClient): string;
begin
  Result := Format('%p', [Pointer(AClient)]);
end;

{==============================================================================}
{ TClientItem                                                                  }
{==============================================================================}

type
  TClientItem = class
  private
    FClient: TModbusClient;
    FOldHandler: TTransactionProcessedEvent;
  public
    constructor Create(AClient: TModbusClient;
      ANewHandler: TTransactionProcessedEvent);
    destructor Destroy; override;
    property Client: TModbusClient read FClient;
  end;

{==============================================================================}

constructor TClientItem.Create(AClient: TModbusClient;
  ANewHandler: TTransactionProcessedEvent);
begin
  FClient := AClient;
  FOldHandler := FClient.OnTransactionProcessed;
  FClient.OnTransactionProcessed := ANewHandler;
end;

{==============================================================================}

destructor TClientItem.Destroy;
begin
  FClient.OnTransactionProcessed := FOldHandler;
  inherited;
end;

{==============================================================================}
{ TModbusSequencer                                                             }
{==============================================================================}

procedure TModbusSequencer.AttachClient(AClient: TModbusClient);
var
  LItem: TClientItem;
begin
  LItem := TClientItem.Create(AClient, TransactionProcessedHandler);
  try
    FClients.AddObject(HashClientReference(AClient), LItem);
  except
    LItem.Free;
    raise;
  end;
end;

{==============================================================================}

constructor TModbusSequencer.Create;
begin
  FClients := TStringList.Create;
  FClients.Duplicates := dupError;
  FClients.Sorted := True;
  FClients.CaseSensitive := False;
end;

{==============================================================================}

destructor TModbusSequencer.Destroy;
begin
  FClients.Free;
  inherited;
end;

{==============================================================================}

procedure TModbusSequencer.DetachClient(AClient: TModbusClient);
var
  LIndex: Integer;
  LItem: TClientItem;
begin
  LIndex := FClients.IndexOf(HashClientReference(AClient));
  if LIndex >= 0 then
  begin
    LItem := TClientItem(FClients.Objects[LIndex]);
    try
      FClients.Delete(LIndex);
    finally
      LItem.Free;
    end;
  end;
end;

{==============================================================================}

procedure TModbusSequencer.DoAdvance;
begin
  if Assigned(FOnAdvance) then
    FOnAdvance(Self);
end;

{==============================================================================}

procedure TModbusSequencer.DoProcess(const AInfo: TTransactionInfo);
begin
  if Assigned(FOnProcess) then
    FOnProcess(Self, AInfo);
end;

{==============================================================================}

procedure TModbusSequencer.Start;
begin
  if FRunning then Exit;
  FRunning := True;
  DoAdvance;
end;

{==============================================================================}

procedure TModbusSequencer.Stop;
begin
  FRunning := False;
end;

{==============================================================================}

procedure TModbusSequencer.TransactionProcessedHandler(ASender: TModbusClient;
  const AInfo: TTransactionInfo; ACommand: Byte; ACustom: Boolean);
begin
  { Pending transaction has been processed. To determine whether it succeeded or
    failed, consult the AInfo parameter. This is the best place to adjust the
    the current state of the state machine. }
  DoProcess(AInfo);
  { If the sequencer is still running, let the application to initiate
    another transaction. }
  if FRunning then
    DoAdvance;
end;

{==============================================================================}

end.
