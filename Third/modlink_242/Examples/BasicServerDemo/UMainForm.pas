unit UMainForm;

{==============================================================================}

{$I ModLink.inc}

{==============================================================================}

interface

{==============================================================================}

uses
  { Delphi  } Windows, Messages, SysUtils,
              {$IFDEF COMPILER_6_UP} Variants, {$ENDIF COMPILER_6_UP}
              Classes, Graphics, Controls, Forms, Dialogs, ActnList, StdCtrls,
              ExtCtrls,
  { ModLink } ModLink,
              UModbusRegisterList;

{==============================================================================}

type
  TMainForm = class(TForm)
    ModbusConnection1: TModbusConnection;
    ActionList1: TActionList;
    ConnectionSettingsAction: TAction;
    ConnectionOpenAction: TAction;
    ConnectionCloseAction: TAction;
    Panel1: TPanel;
    SettingsButton: TButton;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    ModbusServer1: TModbusServer;
    Panel2: TPanel;
    ClearButton: TButton;
    Memo1: TMemo;
    procedure ConnectionSettingsActionExecute(Sender: TObject);
    procedure ConnectionSettingsActionUpdate(Sender: TObject);
    procedure ConnectionOpenActionExecute(Sender: TObject);
    procedure ConnectionOpenActionUpdate(Sender: TObject);
    procedure ConnectionCloseActionExecute(Sender: TObject);
    procedure ConnectionCloseActionUpdate(Sender: TObject);
    procedure ModbusServer1AcceptCommand(Sender: TModbusServer; Command: Byte;
      var Accept: Boolean);
    procedure ModbusServer1CanReadHoldingRegister(Sender: TModbusServer;
      RegAddr: Word; var Allow: Boolean);
    procedure ModbusServer1GetHoldingRegisterValue(Sender: TModbusServer;
      RegAddr: Word; var RegValue: Word);
    procedure ClearButtonClick(Sender: TObject);
    procedure ModbusServer1CanWriteHoldingRegister(Sender: TModbusServer;
      RegAddr, RegValue: Word; var Status: TItemWriteStatus);
    procedure ModbusServer1SetHoldingRegisterValue(Sender: TModbusServer;
      RegAddr, RegValue: Word);
  private
    FHoldingRegisters: TModbusRegisterList;
    procedure LogString(const S: string);
    procedure PopulateHoldingRegisterList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindReadableHoldingRegisterAt(AAddress: Word): TModbusRegister;
    function FindWritableHoldingRegisterAt(AAddress: Word; AValue: Word;
      out AStatus: TItemWriteStatus): TModbusRegister;
  end;

{==============================================================================}

var
  MainForm: TMainForm;

{==============================================================================}

implementation

{==============================================================================}

uses
  { Delphi  } SysConst, Consts, Math,
  { ModLink } ModbusConnectionEditor;

{==============================================================================}

{$R *.dfm}       

{==============================================================================}
{ TMainForm                                                                    }
{==============================================================================}

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  Memo1.Clear;
end;

{==============================================================================}

procedure TMainForm.ConnectionCloseActionExecute(Sender: TObject);
begin
  ModbusConnection1.Close;
end;

{==============================================================================}

procedure TMainForm.ConnectionCloseActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ModbusConnection1.Active;
end;

{==============================================================================}

procedure TMainForm.ConnectionOpenActionExecute(Sender: TObject);
begin
  ModbusConnection1.Open;
end;

{==============================================================================}

procedure TMainForm.ConnectionOpenActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not ModbusConnection1.Active;
end;

{==============================================================================}

procedure TMainForm.ConnectionSettingsActionExecute(Sender: TObject);
begin
  EditModbusConnection(ModbusConnection1, 'Modbus Connection Settings');
end;

{==============================================================================}

procedure TMainForm.ConnectionSettingsActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not ModbusConnection1.Active;
end;

{==============================================================================}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FHoldingRegisters := TModbusRegisterList.Create(True);
  PopulateHoldingRegisterList;
end;

{==============================================================================}

destructor TMainForm.Destroy;
begin
  FHoldingRegisters.Free;
  inherited;
end;

{==============================================================================}

function TMainForm.FindReadableHoldingRegisterAt(AAddress: Word): TModbusRegister;
var
  I: Integer;
begin
  for I := 0 to Pred(FHoldingRegisters.Count) do
  begin
    Result := FHoldingRegisters[I];
    if Result.Address = AAddress then
    begin
      if Result.Readable then
        Exit
      else
        Break;
    end;
  end;
  Result := nil;
end;

{==============================================================================}

function TMainForm.FindWritableHoldingRegisterAt(AAddress, AValue: Word;
  out AStatus: TItemWriteStatus): TModbusRegister;
var
  I: Integer;
begin
  AStatus := iwsIllegalAddress;
  for I := 0 to Pred(FHoldingRegisters.Count) do
  begin
    Result := FHoldingRegisters[I];
    if Result.Address = AAddress then
    begin
      if Result.Writable then
      begin
        if InRange(AValue, Result.MinValue, Result.MaxValue) then
        begin
          AStatus := iwsAllowWrite;
          Exit;
        end
        else
        begin
          AStatus := iwsIllegalValue;
          Break;
        end;
      end
      else
        Break;
    end;
  end;
  Result := nil;
end;

{==============================================================================}

procedure TMainForm.LogString(const S: string);

  {--------------------------------------------------------------------------}

  function EventTimeToStr(const ATime: TDateTime): string;
  begin
    Result := FormatDateTime('hh:nn:ss.zzz', ATime);
  end;

  {--------------------------------------------------------------------------}

var
  LTemp: string;
begin
  if S = EmptyStr then
    LTemp := ''
  else
    LTemp := Format('[%s] %s', [EventTimeToStr(Now), S]);

  Memo1.Lines.Add(LTemp);
end;

{==============================================================================}

procedure TMainForm.ModbusServer1AcceptCommand(Sender: TModbusServer;
  Command: Byte; var Accept: Boolean);

  {--------------------------------------------------------------------------}

  function RequestToStr(ACode: Byte): string;
  begin
    Result := Format('Request received (code $%.2x).', [ACode]);
  end;

  {--------------------------------------------------------------------------}
var
  S: string;
begin
  { Set Accept to True if your server application can handle a particular
    Modbus command. Otherwise, set Accept to False. In this example, our server
    can handle just two commands: ReadHoldingRegisters and WriteSingleRegister. }

  S := RequestToStr(Command);
  LogString(EmptyStr);
  case Command of
    Cmd_ReadHoldingRegisters:
      begin
        Accept := True;
        LogString(S + ' ' + 'Processing ReadHoldingRegisters request...');
      end;
    Cmd_WriteSingleRegister:
      begin
        Accept := True;
        LogString(S + ' ' + 'Processing WriteSingleRegister request...');
      end;
  else
    begin
      Accept := False;
      LogString(S + ' ' + 'Command not supported.');
    end;
  end;
end;

{==============================================================================}

procedure TMainForm.ModbusServer1CanReadHoldingRegister(Sender: TModbusServer;
  RegAddr: Word; var Allow: Boolean);
begin
  { This event is triggered while ReadHoldingRegisters request is being processed.
    You should set Allow param to True if:

    (1) this application exposes a holding register located at address given by
        RegAddr param and
    (2) the holding register in question can be read at the moment.

    Otherwise, set Allow to False. }

  Allow := Assigned(FindReadableHoldingRegisterAt(RegAddr));

  if Allow then
    LogString(Format('Register at address %d is readable', [RegAddr]))
  else
    LogString(Format('Register at address %d is not readable', [RegAddr]));
end;

{==============================================================================}

procedure TMainForm.ModbusServer1CanWriteHoldingRegister(Sender: TModbusServer;
  RegAddr, RegValue: Word; var Status: TItemWriteStatus);
begin
  { This event is triggered while WriteSingleRegister request is being processed.
    You should set Status param according to whether or not this application
    exposes a holding register located at address given by RegAddr param and
    a value given by RegValue param can be actually written into the holding
    register in question at the moment. }

  FindWritableHoldingRegisterAt(RegAddr, RegValue, Status);

  case Status of
    iwsIllegalAddress:
      LogString(Format('There is no register at address %d', [RegAddr]));
    iwsIllegalValue:
      LogString(Format('%d is not legal value for register at address %d', [RegValue, RegAddr]));
    iwsAllowWrite:
      LogString(Format('Register at address %d is writable', [RegAddr]));
  end;
end;

{==============================================================================}

procedure TMainForm.ModbusServer1GetHoldingRegisterValue(Sender: TModbusServer;
  RegAddr: Word; var RegValue: Word);
var
  LRegister: TModbusRegister;
begin
  { This event is triggered while ReadHoldingRegisters request is being processed.
    You should set RegValue param to the exact value of a holding register
    located at address given by RegAddr param. }

  LRegister := FindReadableHoldingRegisterAt(RegAddr);
  Assert(Assigned(LRegister)); { LRegister reference is expected to be valid! }
  RegValue := LRegister.Value;
  LogString(Format('Value %d has been read from register at address %d', [RegValue, RegAddr]));
end;

{==============================================================================}

procedure TMainForm.ModbusServer1SetHoldingRegisterValue(Sender: TModbusServer;
  RegAddr, RegValue: Word);
var
  LRegister: TModbusRegister;
  LStatus: TItemWriteStatus;
begin
  { This event is triggered while WriteSingleRegister request is being processed.
    You should set the value of a holding register located at address given by
    RegAddr param to the exact value given by RegValue param. }

  LRegister := FindWritableHoldingRegisterAt(RegAddr, RegValue, LStatus);
  Assert(Assigned(LRegister)); { LRegister reference is expected to be valid! }
  Assert(LStatus = iwsAllowWrite); { This has to be true as well. }
  LRegister.Value := RegValue;
  LogString(Format('Value %d has been written to register at address %d', [RegValue, RegAddr]));
end;

{==============================================================================}

procedure TMainForm.PopulateHoldingRegisterList;

  {----------------------------------------------------------------------------}

  procedure AddHoldingRegister(
    AAddress: Word;       { Address of this register.                       }
    AMinValue: Word;     { Lowes value this register is allowed to hold.   }
    AMaxValue: Word;     { Highest value this register is allowed to hold. }
    ACurValue: Word;     { Value this register currently holds.            }
    AReadable: Boolean;  { Can this register be read by remote client?     }
    AWritable: Boolean); { Can this register be written by remote client?  }
  var
    LRegister: TModbusRegister;
  begin
    LRegister := TModbusRegister.Create;
    try
      FHoldingRegisters.Add(LRegister);
      try
        LRegister.Address := AAddress;
        LRegister.MinValue := AMinValue;
        LRegister.MaxValue := AMaxValue;
        LRegister.Value := ACurValue;
        LRegister.Readable := AReadable;
        LRegister.Writable := AWritable;
      except
        FHoldingRegisters.Remove(LRegister);
        raise;
      end;
    except
      LRegister.Free;
      raise;
    end;
  end;

  {----------------------------------------------------------------------------}

begin
  { For the sake of the simplicity, let this Modbus server have just 5 holding
    registers. Lets create their descriptors right here. }

  AddHoldingRegister(
    { Address  } 100,
    { MinValue } $1233,
    { MaxValue } $1235,
    { CurValue } $1234,
    { Readable } True,
    { Writable } True); { This one has a restricted range of allowed values. }

  AddHoldingRegister(
    { Address  } 101,
    { MinValue } 0,
    { MaxValue } High(Word),
    { CurValue } $BEEF,
    { Readable } True,
    { Writable } True); { This one has no special restrictions. }

  AddHoldingRegister(
    { Address  } 102,
    { MinValue } 0,
    { MaxValue } High(Word),
    { CurValue } $C0DE,
    { Readable } True,
    { Writable } True); { This one has no special restrictions. }

  AddHoldingRegister(
    { Address  } 103,
    { MinValue } 0,
    { MaxValue } High(Word),
    { CurValue } $FACE,
    { Readable } True,
    { Writable } False); { This one can be read but not written. }

  AddHoldingRegister(
    { Address  } 104,
    { MinValue } 0,
    { MaxValue } High(Word),
    { CurValue } $FEED,
    { Readable } False,
    { Writable } True); { This one can be written but not read. }
end;

{==============================================================================}

end.
