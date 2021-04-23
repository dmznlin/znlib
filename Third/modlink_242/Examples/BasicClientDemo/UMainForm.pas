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
  { ModLink } ModLink;

{==============================================================================}

type
  TMainForm = class(TForm)
    ModbusConnection1: TModbusConnection;
    ModbusClient1: TModbusClient;
    ActionList1: TActionList;
    ConnectionSettingsAction: TAction;
    ConnectionOpenAction: TAction;
    ConnectionCloseAction: TAction;
    Panel1: TPanel;
    SettingsButton: TButton;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    GroupBox1: TGroupBox;
    ReadAddressLabel: TLabel;
    ReadAddressEdit: TEdit;
    ReadButton: TButton;
    ReadStatusLabel: TLabel;
    ReadStatusEdit: TEdit;
    GroupBox2: TGroupBox;
    WriteAddressLabel: TLabel;
    WriteStatusLabel: TLabel;
    WriteAddressEdit: TEdit;
    WriteButton: TButton;
    WriteStatusEdit: TEdit;
    WriteValueLabel: TLabel;
    WriteValueEdit: TEdit;
    RegisterReadAction: TAction;
    RegisterWriteAction: TAction;
    procedure ConnectionSettingsActionExecute(Sender: TObject);
    procedure ConnectionSettingsActionUpdate(Sender: TObject);
    procedure ConnectionOpenActionExecute(Sender: TObject);
    procedure ConnectionOpenActionUpdate(Sender: TObject);
    procedure ConnectionCloseActionExecute(Sender: TObject);
    procedure ConnectionCloseActionUpdate(Sender: TObject);
    procedure RegisterReadActionExecute(Sender: TObject);
    procedure RegisterReadActionUpdate(Sender: TObject);
    procedure RegisterWriteActionExecute(Sender: TObject);
    procedure RegisterWriteActionUpdate(Sender: TObject);
    procedure ModbusClient1HoldingRegistersRead(Sender: TModbusClient;
      const Info: TTransactionInfo; StartReg, RegCount: Word;
      const RegValues: TRegValues);
    procedure ModbusClient1SingleRegisterWrite(Sender: TModbusClient;
      const Info: TTransactionInfo; RegAddr, RegValue: Word);
  private
    function GetStatusText(const AInfo: TTransactionInfo): string;
  public
    { Public declarations }
  end;

{==============================================================================}

var
  MainForm: TMainForm;

{==============================================================================}

implementation

{==============================================================================}

uses
  { Delphi  } SysConst, Consts,
  { ModLink } ModbusConnectionEditor;

{==============================================================================}

{$R *.dfm}

{==============================================================================}

const
  CServerReplies: array [TServerReply] of string = (
    { srNone            } 'Timed out (no reply)',
    { srInvalidFrame    } 'Timed out (reply frame is corrupted)',
    { srUnexpectedReply } 'Timed out (reply from unexpected server)',
    { srUnmatchedReply  } 'Failed (reply does not match request)',
    { srExceptionReply  } 'Failed (server returned exception reply)',
    { srNormalReply     } 'Transaction succeeded'
  );

{==============================================================================}
{ Helper routines                                                              }
{==============================================================================}

procedure ValidateNumberInEditBox(AEdit: TEdit; MinValue, MaxValue: Int64);

  {--------------------------------------------------------------------------}

  procedure Error(ResStringRec: PResStringRec; const Args: array of const);
  begin
    {$IFDEF COMPILER_5_UP}
    raise Exception.CreateResFmt(ResStringRec, Args);
    {$ELSE}
    raise Exception.CreateFmt(LoadResString(ResStringRec), Args);
    {$ENDIF COMPILER_5_UP}
  end;

  procedure InvalidInteger;
  begin
    if AEdit.CanFocus then AEdit.SetFocus;
    Error(@SInvalidInteger, [AEdit.Text]);
  end;

  procedure InvalidRange;
  begin
    if AEdit.CanFocus then AEdit.SetFocus;
    Error(@SOutOfRange, [MinValue, MaxValue]);
  end;

  {--------------------------------------------------------------------------}

var
  I: Int64;
begin
  try
    I := StrToInt64(AEdit.Text);
    if (MinValue <> MaxValue) and ((I < MinValue) or (I > MaxValue)) then
      InvalidRange;
  except
    on E: Exception do
      if E is EConvertError then
      begin
        InvalidInteger;
      end
      else
        raise;
  end;
end;

{==============================================================================}
{ TMainForm                                                                    }
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

function TMainForm.GetStatusText(const AInfo: TTransactionInfo): string;
begin
  Result := CServerReplies[AInfo.Reply];
  if AInfo.Reply = srExceptionReply then
    Result := Result + ': ' + ModbusClient1.ExceptionCodeToStr(AInfo.ExceptionCode);
end;

{==============================================================================}

procedure TMainForm.ModbusClient1HoldingRegistersRead(Sender: TModbusClient;
  const Info: TTransactionInfo; StartReg, RegCount: Word;
  const RegValues: TRegValues);
begin
  if Info.Reply = srNormalReply then
  begin
    ReadStatusEdit.Text := Format('Value %d has been read from holding register %d',
    [RegValues[0], StartReg]);
  end
  else
    ReadStatusEdit.Text := GetStatusText(Info);
end;

{==============================================================================}

procedure TMainForm.ModbusClient1SingleRegisterWrite(Sender: TModbusClient;
  const Info: TTransactionInfo; RegAddr, RegValue: Word);
begin
  if Info.Reply = srNormalReply then
  begin
    WriteStatusEdit.Text := Format('Value %d has been written to holding register %d',
    [RegValue, RegAddr]);
  end
  else
    WriteStatusEdit.Text := GetStatusText(Info);
end;

{==============================================================================}

procedure TMainForm.RegisterReadActionExecute(Sender: TObject);
var
  LAddress: Word;
begin
  ValidateNumberInEditBox(ReadAddressEdit, Low(Word), High(Word));
  LAddress := StrToInt(ReadAddressEdit.Text);
  ModbusClient1.ReadHoldingRegisters(LAddress, 1);
  ReadStatusEdit.Text := 'Read transaction is being processed...';
end;

{==============================================================================}

procedure TMainForm.RegisterReadActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ModbusConnection1.Active and
    (ModbusConnection1.CountPendingTransactions(nil) = 0);
end;

{==============================================================================}

procedure TMainForm.RegisterWriteActionExecute(Sender: TObject);
var
  LAddress, LValue: Word;
begin
  ValidateNumberInEditBox(WriteAddressEdit, Low(Word), High(Word));
  ValidateNumberInEditBox(WriteValueEdit, Low(Word), High(Word));
  LAddress := StrToInt(WriteAddressEdit.Text);
  LValue := StrToInt(WriteValueEdit.Text);
  ModbusClient1.WriteSingleRegister(LAddress, LValue);
  WriteStatusEdit.Text := 'Write transaction is being processed...';
end;

{==============================================================================}

procedure TMainForm.RegisterWriteActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ModbusConnection1.Active and
    (ModbusConnection1.CountPendingTransactions(nil) = 0);
end;

{==============================================================================}

end.
