{*****************************************************************}
{                                                                 }
{ ModLink                                                         }
{ Copyright (C) 2002 - 2013 Ing. Ivo Bauer                        }
{ All Rights Reserved.                                            }
{                                                                 }
{ Web site: http://www.ozm.cz/ivobauer/modlink/                   }
{ E-mail:   bauer@ozm.cz                                          }
{                                                                 }
{ For a detailed information regarding the distribution and use   }
{ of this software product, please refer to the License Agreement }
{ embedded in the accompanying online documentation (ModLink.chm) }
{                                                                 }
{*****************************************************************}

unit ModbusConnectionEditor;

{$I ModLink.inc}

interface

//--------------------------------------------------------------------------------------------------

uses
  { Windows } Windows, Messages,
  { Delphi  } SysUtils, {$IFDEF COMPILER_6_UP} Variants , {$ENDIF COMPILER_6_UP} Classes, Graphics,
              Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  { ModLink } ModLink;

//--------------------------------------------------------------------------------------------------

type
  TModbusConnectionEditForm = class(TForm)
    PageControl1: TPageControl;
    SerialCommTabSheet: TTabSheet;
    TransactionMgmtTabSheet: TTabSheet;
    Label1: TLabel;
    PortComboBox: TComboBox;
    Label2: TLabel;
    BaudRateComboBox: TComboBox;
    CustomBaudRateEdit: TEdit;
    DataBitsRadioGroup: TRadioGroup;
    ParitySchemeRadioGroup: TRadioGroup;
    StopBitsRadioGroup: TRadioGroup;
    FlowControlRadioGroup: TRadioGroup;
    TransmissionModeRadioGroup: TRadioGroup;
    Label3: TLabel;
    SilentIntervalEdit: TEdit;
    Label4: TLabel;
    RefetchDelayEdit: TEdit;
    Label5: TLabel;
    SendTimeoutEdit: TEdit;
    Label6: TLabel;
    ReceiveTimeoutEdit: TEdit;
    Label7: TLabel;
    MaxRetriesEdit: TEdit;
    Label8: TLabel;
    TurnaroundDelayEdit: TEdit;
    ThreadPriorityRadioGroup: TRadioGroup;
    RTUDefaultsButton: TButton;
    ASCIIDefaultsButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    ConnectionModeRadioGroup: TRadioGroup;
    EnabledLinesGroupBox: TGroupBox;
    DTREnabledCheckBox: TCheckBox;
    RTSEnabledCheckBox: TCheckBox;
    EchoCheckBox: TCheckBox;
    Label9: TLabel;
    RTSHoldDelayEdit: TEdit;
    Bevel1: TBevel;
    procedure FormShow(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure BaudRateComboBoxClick(Sender: TObject);
    procedure TransmissionModeRadioGroupClick(Sender: TObject);
    procedure RTUDefaultsButtonClick(Sender: TObject);
    procedure ASCIIDefaultsButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ConnectionModeRadioGroupClick(Sender: TObject);
    procedure FlowControlRadioGroupClick(Sender: TObject);
  private
    procedure ValidateSerialCommTabSheet;
    procedure ValidateTransactionMgmtTabSheet;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//--------------------------------------------------------------------------------------------------

var
  ModbusConnectionEditForm: TModbusConnectionEditForm;

//--------------------------------------------------------------------------------------------------

function EditModbusConnection(AConnection: TModbusConnection; const ACaption: string): Boolean;

//--------------------------------------------------------------------------------------------------

implementation

//--------------------------------------------------------------------------------------------------

uses
  { Delphi } SysConst, Consts;

//--------------------------------------------------------------------------------------------------

{$R *.dfm}

//--------------------------------------------------------------------------------------------------

procedure ChangeEnabledState(AEdit: TEdit; AEnabled: Boolean);
const
  Colors: array [Boolean] of TColor = (clBtnFace, clWindow);
begin
  AEdit.Enabled := AEnabled;
  AEdit.Color := Colors[AEnabled];
end;

//--------------------------------------------------------------------------------------------------

procedure ValidateNumberInEditBox(AEdit: TEdit; MinValue, MaxValue: Int64);

  // begin of local block --------------------------------------------------------------------------

  procedure Error(ResStringRec: PResStringRec; const Args: array of const);
  begin
    {$IFDEF COMPILER_5_UP}
    raise EModLinkError.CreateResFmt(ResStringRec, Args);
    {$ELSE}
    raise EModLinkError.CreateFmt(LoadResString(ResStringRec), Args);
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

  // end of local block ----------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function EditModbusConnection(AConnection: TModbusConnection; const ACaption: string): Boolean;
var
  Form: TModbusConnectionEditForm;
  ExplicitClosure: Boolean;
  IsClientMode, IsClientOrServerMode: Boolean;

  // begin of local block --------------------------------------------------------------------------

  procedure SelectBaudRate;
  begin
    if AConnection.BaudRate in [Low(TBaudRate)..High(TBaudRate)] then
      Form.BaudRateComboBox.ItemIndex := Ord(AConnection.BaudRate)
    else
      Form.BaudRateComboBox.ItemIndex := Ord(br19200);
  end;

  procedure SelectDataBits;
  begin
    if AConnection.DataBits in [Low(TDataBits)..High(TDataBits)] then
      Form.DataBitsRadioGroup.ItemIndex := Ord(AConnection.DataBits)
    else
      Form.DataBitsRadioGroup.ItemIndex := Ord(db8);
  end;

  procedure SelectParityScheme;
  begin
    if AConnection.Parity in [Low(TParityScheme)..High(TParityScheme)] then
      Form.ParitySchemeRadioGroup.ItemIndex := Ord(AConnection.Parity)
    else
      Form.ParitySchemeRadioGroup.ItemIndex := Ord(psEven);
  end;

  procedure SelectStopBits;
  begin
    if AConnection.StopBits in [Low(TStopBits)..High(TStopBits)] then
      Form.StopBitsRadioGroup.ItemIndex := Ord(AConnection.StopBits)
    else
      Form.StopBitsRadioGroup.ItemIndex := Ord(sb1);
  end;

  procedure SelectFlowControl;
  begin
    if AConnection.FlowControl in [Low(TFlowControl)..High(TFlowControl)] then
      Form.FlowControlRadioGroup.ItemIndex := Ord(AConnection.FlowControl)
    else
      Form.FlowControlRadioGroup.ItemIndex := Ord(fcNone);
  end;

  procedure SelectTransmissionMode;
  begin
    if AConnection.TransmissionMode in [Low(TTransmissionMode)..High(TTransmissionMode)] then
      Form.TransmissionModeRadioGroup.ItemIndex := Ord(AConnection.TransmissionMode)
    else
      Form.TransmissionModeRadioGroup.ItemIndex := Ord(tmRTU);
  end;

  procedure SelectConnectionMode;
  begin
    if AConnection.ConnectionMode in [Low(TConnectionMode)..High(TConnectionMode)] then
      Form.ConnectionModeRadioGroup.ItemIndex := Ord(AConnection.ConnectionMode)
    else
      Form.ConnectionModeRadioGroup.ItemIndex := Ord(cmClient);
  end;

  procedure SelectThreadPriority;
  begin
    if AConnection.ThreadPriority in [Low(TThreadPriority)..High(TThreadPriority)] then
      Form.ThreadPriorityRadioGroup.ItemIndex := Ord(AConnection.ThreadPriority)
    else
      Form.ThreadPriorityRadioGroup.ItemIndex := Ord(tpNormal);
  end;

  function ConnectionNeedsExplicitClosure: Boolean;
  begin
    with Form do
      Result := AConnection.Active and not (csDesigning in AConnection.ComponentState) and
        ((PortComboBox.Text <> AConnection.Port) or
        (TFlowControl(FlowControlRadioGroup.ItemIndex) <> AConnection.FlowControl) or
        (DTREnabledCheckBox.Checked <> AConnection.DTREnabled) or
        (RTSEnabledCheckBox.Checked <> AConnection.RTSEnabled) or
        (TTransmissionMode(TransmissionModeRadioGroup.ItemIndex) <> AConnection.TransmissionMode) or
        (TConnectionMode(ConnectionModeRadioGroup.ItemIndex) <> AConnection.ConnectionMode));
  end;

  // end of local block ----------------------------------------------------------------------------

begin
  Form := TModbusConnectionEditForm.Create(nil);
  with Form do
    try
      Caption := ACaption;

      // Synchronize the controls with properties of supplied AConnection.

      // SerialComm TabSheet
      PortComboBox.Text := AConnection.Port;
      SelectBaudRate;
      CustomBaudRateEdit.Text := IntToStr(Int64(AConnection.CustomBaudRate));
      EchoCheckBox.Checked := AConnection.EchoQueryBeforeReply;
      SelectDataBits;
      SelectParityScheme;
      SelectStopBits;
      SelectFlowControl;
      RTSHoldDelayEdit.Text := IntToStr(Int64(AConnection.RTSHoldDelay));
      DTREnabledCheckBox.Checked := AConnection.DTREnabled;
      RTSEnabledCheckBox.Checked := AConnection.RTSEnabled;
      SelectTransmissionMode;
      SilentIntervalEdit.Text := IntToStr(Int64(AConnection.SilentInterval));

      // TransactionMgmt TabSheet
      SelectConnectionMode;
      SelectThreadPriority;
      RefetchDelayEdit.Text := IntToStr(Int64(AConnection.RefetchDelay));
      SendTimeoutEdit.Text := IntToStr(Int64(AConnection.SendTimeout));
      ReceiveTimeoutEdit.Text := IntToStr(Int64(AConnection.ReceiveTimeout));
      MaxRetriesEdit.Text := IntToStr(Int64(AConnection.MaxRetries));
      TurnaroundDelayEdit.Text := IntToStr(Int64(AConnection.TurnaroundDelay));

      // Display the editor.
      Result := ShowModal = mrOk;

      // Assign a new property values to supplied AConnection in case of positive modal result.
      if Result then
      begin
        // Some of TModbusConnection properties cannot be changed while the connection is active
        // (this is due to by design). I do a check here whether or not to close the connection
        // explicitly. When done with assigning new property values, connection will be reopened
        // again if applicable.
        ExplicitClosure := ConnectionNeedsExplicitClosure;

        if ExplicitClosure then AConnection.Close;
        try
          // Apply the changes to the supplied AConnection.
          // SerialComm TabSheet
          AConnection.Port := PortComboBox.Text;
          AConnection.BaudRate := TBaudRate(BaudRateComboBox.ItemIndex);
          if TBaudRate(BaudRateComboBox.ItemIndex) = brCustom then
            AConnection.CustomBaudRate := Cardinal(StrToInt64(CustomBaudRateEdit.Text));
          AConnection.EchoQueryBeforeReply := EchoCheckBox.Checked;
          AConnection.DataBits := TDataBits(DataBitsRadioGroup.ItemIndex);
          AConnection.Parity := TParityScheme(ParitySchemeRadioGroup.ItemIndex);
          AConnection.StopBits := TStopBits(StopBitsRadioGroup.ItemIndex);
          AConnection.FlowControl := TFlowControl(FlowControlRadioGroup.ItemIndex);
          AConnection.RTSHoldDelay := Cardinal(StrToInt64(RTSHoldDelayEdit.Text));
          AConnection.DTREnabled := DTREnabledCheckBox.Checked;
          AConnection.RTSEnabled := RTSEnabledCheckBox.Checked;
          AConnection.TransmissionMode := TTransmissionMode(TransmissionModeRadioGroup.ItemIndex);
          AConnection.SilentInterval := Cardinal(StrToInt64(SilentIntervalEdit.Text));

          // TransactionMgmt TabSheet
          IsClientMode := ConnectionModeRadioGroup.ItemIndex = Ord(cmClient);
          IsClientOrServerMode := ConnectionModeRadioGroup.ItemIndex in [Ord(cmClient), Ord(cmServer)];

          AConnection.ConnectionMode := TConnectionMode(ConnectionModeRadioGroup.ItemIndex);
          AConnection.ThreadPriority := TThreadPriority(ThreadPriorityRadioGroup.ItemIndex);

          if IsClientMode then
            AConnection.RefetchDelay := Cardinal(StrToInt64(RefetchDelayEdit.Text));

          if IsClientOrServerMode then
            AConnection.SendTimeout := Cardinal(StrToInt64(SendTimeoutEdit.Text));

          if IsClientMode then
            AConnection.ReceiveTimeout := Cardinal(StrToInt64(ReceiveTimeoutEdit.Text));

          if IsClientMode then
            AConnection.MaxRetries := Cardinal(StrToInt64(MaxRetriesEdit.Text));

          if IsClientMode then
            AConnection.TurnaroundDelay := Cardinal(StrToInt64(TurnaroundDelayEdit.Text));
        finally
          // Finally, reopen the connection.
          if ExplicitClosure then AConnection.Open;
        end;
      end;
    finally
      Free;
    end;
end;

//--------------------------------------------------------------------------------------------------
// TConnectionOptionsForm class
//--------------------------------------------------------------------------------------------------

constructor TModbusConnectionEditForm.Create(AOwner: TComponent);

  // begin of local block --------------------------------------------------------------------------

  procedure EnumBaudRates;
  const
    CBaudRates: array [TBaudRate] of string = (
      '110',
      '300',
      '600',
      '1200',
      '2400',
      '4800',
      '9600',
      '14400',
      '19200',
      '38400',
      '56000',
      '57600',
      '115200',
      '128000',
      '256000',
      'Custom'
    );
  var
    I: TBaudRate;
  begin
    with BaudRateComboBox do
    begin
      Items.Clear;
      for I := Low(I) to High(I) do
        Items.Add(CBaudRates[I]);
    end;
  end;

  // end of local block ----------------------------------------------------------------------------

begin
  inherited;
  EnumSerialPorts(PortComboBox.Items);
  EnumBaudRates;
  PageControl1.ActivePage := SerialCommTabSheet;
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.ValidateSerialCommTabSheet;
begin
  if TBaudRate(BaudRateComboBox.ItemIndex) = brCustom then
    ValidateNumberInEditBox(CustomBaudRateEdit, 0, High(Cardinal));
  EchoCheckBox.Enabled := ConnectionModeRadioGroup.ItemIndex = Ord(cmClient);
  ValidateNumberInEditBox(RTSHoldDelayEdit, 0, High(Cardinal));
  ValidateNumberInEditBox(SilentIntervalEdit, 0, High(Cardinal));
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.ValidateTransactionMgmtTabSheet;
var
  IsClientMode: Boolean;
begin
  IsClientMode := ConnectionModeRadioGroup.ItemIndex = Ord(cmClient);
  if IsClientMode then ValidateNumberInEditBox(RefetchDelayEdit, 0, High(Cardinal));
  ValidateNumberInEditBox(SendTimeoutEdit, 0, High(Cardinal));
  ValidateNumberInEditBox(ReceiveTimeoutEdit, 0, High(Cardinal));
  if IsClientMode then ValidateNumberInEditBox(MaxRetriesEdit, 1, High(Cardinal));
  if IsClientMode then ValidateNumberInEditBox(TurnaroundDelayEdit, 0, High(Cardinal));
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.FormShow(Sender: TObject);
begin
  BaudRateComboBoxClick(nil);
  FlowControlRadioGroupClick(nil);
  TransmissionModeRadioGroupClick(nil);
  PageControl1Change(nil);
  ConnectionModeRadioGroupClick(nil);
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  try
    if PageControl1.ActivePage = SerialCommTabSheet then
      ValidateSerialCommTabSheet
    else
      ValidateTransactionMgmtTabSheet;
  except
    AllowChange := False;
    {$IFDEF COMPILER_6_UP}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
    {$ELSE}
    Application.HandleException(Self);
    {$ENDIF COMPILER_6_UP}
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.PageControl1Change(Sender: TObject);
begin
  RTUDefaultsButton.Enabled := PageControl1.ActivePage = SerialCommTabSheet;
  ASCIIDefaultsButton.Enabled := PageControl1.ActivePage = SerialCommTabSheet;
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.BaudRateComboBoxClick(Sender: TObject);
begin
  CustomBaudRateEdit.Visible := BaudRateComboBox.ItemIndex = Ord(brCustom);
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.TransmissionModeRadioGroupClick(
  Sender: TObject);
const
  LabelFmt = 'Silent &Interval [%s]:';
begin
  if TransmissionModeRadioGroup.ItemIndex = Ord(tmRTU) then
    Label3.Caption := Format(LabelFmt, ['character times'])
  else
    Label3.Caption := Format(LabelFmt, ['ms'])
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.RTUDefaultsButtonClick(Sender: TObject);
begin
  DataBitsRadioGroup.ItemIndex := Ord(db8);
  ParitySchemeRadioGroup.ItemIndex := Ord(psEven);
  StopBitsRadioGroup.ItemIndex := Ord(sb1);
  TransmissionModeRadioGroup.ItemIndex := Ord(tmRTU);
  SilentIntervalEdit.Text := '4';
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.ASCIIDefaultsButtonClick(Sender: TObject);
begin
  DataBitsRadioGroup.ItemIndex := Ord(db7);
  ParitySchemeRadioGroup.ItemIndex := Ord(psEven);
  StopBitsRadioGroup.ItemIndex := Ord(sb1);
  TransmissionModeRadioGroup.ItemIndex := Ord(tmASCII);
  SilentIntervalEdit.Text := '1000';
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.OKButtonClick(Sender: TObject);
begin
  try
    ValidateSerialCommTabSheet;
    ValidateTransactionMgmtTabSheet;
  except
    ModalResult := mrNone;
    {$IFDEF COMPILER_6_UP}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
    {$ELSE}
    Application.HandleException(Self);
    {$ENDIF COMPILER_6_UP}
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.ConnectionModeRadioGroupClick(
  Sender: TObject);
var
  IsClientMode, IsClientOrServerMode: Boolean;
begin
  IsClientMode := ConnectionModeRadioGroup.ItemIndex = Ord(cmClient);
  IsClientOrServerMode := ConnectionModeRadioGroup.ItemIndex in [Ord(cmClient), Ord(cmServer)];

  Label4.Enabled := IsClientMode;
  ChangeEnabledState(RefetchDelayEdit, IsClientMode);

  Label5.Enabled := IsClientOrServerMode;
  ChangeEnabledState(SendTimeoutEdit, IsClientOrServerMode);

  Label6.Enabled := IsClientMode;
  ChangeEnabledState(ReceiveTimeoutEdit, IsClientMode);

  Label7.Enabled := IsClientMode;
  ChangeEnabledState(MaxRetriesEdit, IsClientMode);

  Label8.Enabled := IsClientMode;
  ChangeEnabledState(TurnaroundDelayEdit, IsClientMode);
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditForm.FlowControlRadioGroupClick(
  Sender: TObject);
var
  IsRtsToggle: Boolean;
begin
  IsRtsToggle := FlowControlRadioGroup.ItemIndex = Ord(fcRtsToggle);

  Label9.Enabled := IsRtsToggle;
  ChangeEnabledState(RTSHoldDelayEdit, IsRtsToggle);

  DTREnabledCheckBox.Enabled := FlowControlRadioGroup.ItemIndex <> Ord(fcDtrDsr);

  RTSEnabledCheckBox.Enabled := not (FlowControlRadioGroup.ItemIndex in
    [Ord(fcRtsToggle), Ord(fcRtsCts)]);
end;

//--------------------------------------------------------------------------------------------------

end.
