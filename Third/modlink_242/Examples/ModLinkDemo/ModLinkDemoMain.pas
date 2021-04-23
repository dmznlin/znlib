unit ModLinkDemoMain;

{$I ModLink.inc}

interface

//--------------------------------------------------------------------------------------------------

uses
  { Windows } Windows, Messages,
  { Delphi  } SysUtils, {$IFDEF COMPILER_6_UP} Variants , {$ENDIF COMPILER_6_UP} Classes, Graphics,
              Controls, Forms, Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls, Buttons, IniFiles,
  { ModLink } ModLink,
  { Project } ServerItem, ServerItemEditor;

//--------------------------------------------------------------------------------------------------

type
  TModLinkDemoMainForm = class(TForm)
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    ToolsMenu: TMenuItem;
    HelpMenu: TMenuItem;
    FileExitItem: TMenuItem;
    ToolsConnectionOptionsItem: TMenuItem;
    ToolsClientOptionsItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    ModbusConnection1: TModbusConnection;
    ModbusClient1: TModbusClient;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    LogMemo: TMemo;
    IntroductionTabSheet: TTabSheet;
    DiscreteAccessTabSheet: TTabSheet;
    RegisterAccessTabSheet: TTabSheet;
    DiagnosticsTabSheet: TTabSheet;
    Panel1: TPanel;
    DiagnosticActionRadioGroup: TRadioGroup;
    DiagnosticsButton: TButton;
    N2: TMenuItem;
    ToolsClearTransactionLogItem: TMenuItem;
    DiscreteReadGroupBox: TGroupBox;
    Label3: TLabel;
    Label2: TLabel;
    ReadStartBitEdit: TEdit;
    ReadBitCountEdit: TEdit;
    ReadCoilsButton: TButton;
    ReadDiscreteInputsButton: TButton;
    DiscreteWriteGroupBox: TGroupBox;
    DiscreteListView: TListView;
    WriteSingleCoilButton: TButton;
    WriteMultipleCoilsButton: TButton;
    Label1: TLabel;
    WriteStartBitEdit: TEdit;
    Label7: TLabel;
    WriteBitCountEdit: TEdit;
    RegisterReadGroupBox: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    ReadStartRegEdit: TEdit;
    ReadRegCountEdit: TEdit;
    ReadHoldingRegistersButton: TButton;
    ReadInputRegistersButton: TButton;
    RegisterWriteGroupBox: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    RegisterListView: TListView;
    RegisterBroadcastCheckBox: TCheckBox;
    WriteStartRegEdit: TEdit;
    WriteRegCountEdit: TEdit;
    WriteSingleRegisterButton: TButton;
    WriteMultipleRegistersButton: TButton;
    RegisterMaskWriteGroupBox: TGroupBox;
    Label11: TLabel;
    MaskWriteRegAddrEdit: TEdit;
    Label6: TLabel;
    AndMaskEdit: TEdit;
    Label8: TLabel;
    OrMaskEdit: TEdit;
    MaskWriteSingleRegisterButton: TButton;
    DiscreteBroadcastCheckBox: TCheckBox;
    IntroductionRichEdit: TRichEdit;
    Timer1: TTimer;
    ToolsDiscardPendingTransactionsItem: TMenuItem;
    ServerMapTabSheet: TTabSheet;
    ModbusServer1: TModbusServer;
    ToolsServerOptionsItem: TMenuItem;
    ServerItemsListView: TListView;
    AddButton: TBitBtn;
    RemoveButton: TButton;
    RemoveAllButton: TButton;
    PopupMenu1: TPopupMenu;
    AddCoilItem: TMenuItem;
    AddDiscreteInputItem: TMenuItem;
    AddHoldingRegisterItem: TMenuItem;
    AddInputRegisterItem: TMenuItem;
    EditButton: TButton;
    ReadExceptionStatusButton: TButton;
    ReportServerIDButton: TButton;
    Bevel1: TBevel;
    RegisterReadWriteGroupBox: TGroupBox;
    ReadWriteMultipleRegistersButton: TButton;
    ToolsLogTransactionsToFileItem: TMenuItem;
    procedure FileExitItemClick(Sender: TObject);
    procedure ToolsConnectionOptionsItemClick(Sender: TObject);
    procedure ToolsClientOptionsItemClick(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
    procedure ReadCoilsButtonClick(Sender: TObject);
    procedure ReadDiscreteInputsButtonClick(Sender: TObject);
    procedure WriteSingleCoilButtonClick(Sender: TObject);
    procedure WriteMultipleCoilsButtonClick(Sender: TObject);
    procedure ModbusClient1CoilsRead(Sender: TModbusClient;
      const Info: TTransactionInfo; BitStart, BitCount: Word;
      const BitValues: TBitValues);
    procedure ModbusClient1DiscreteInputsRead(Sender: TModbusClient;
      const Info: TTransactionInfo; BitStart, BitCount: Word;
      const BitValues: TBitValues);
    procedure ModbusClient1SingleCoilWrite(Sender: TModbusClient;
      const Info: TTransactionInfo; BitAddr: Word; BitValue: Boolean);
    procedure ModbusClient1MultipleCoilsWrite(Sender: TModbusClient;
      const Info: TTransactionInfo; BitStart, BitCount: Word;
      const BitValues: TBitValues);
    procedure ModbusConnection1FrameSend(Sender: TModbusConnection;
      const Data: TFrameData);
    procedure ModbusConnection1FrameReceive(Sender: TModbusConnection;
      const Data: TFrameData);
    procedure ReadHoldingRegistersButtonClick(Sender: TObject);
    procedure ReadInputRegistersButtonClick(Sender: TObject);
    procedure WriteSingleRegisterButtonClick(Sender: TObject);
    procedure WriteMultipleRegistersButtonClick(Sender: TObject);
    procedure RegisterListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RegisterListViewDblClick(Sender: TObject);
    procedure ModbusClient1HoldingRegistersRead(Sender: TModbusClient;
      const Info: TTransactionInfo; StartReg, RegCount: Word;
      const RegValues: TRegValues);
    procedure ModbusClient1InputRegistersRead(Sender: TModbusClient;
      const Info: TTransactionInfo; StartReg, RegCount: Word;
      const RegValues: TRegValues);
    procedure ModbusClient1SingleRegisterWrite(Sender: TModbusClient;
      const Info: TTransactionInfo; RegAddr, RegValue: Word);
    procedure ModbusClient1MultipleRegistersWrite(Sender: TModbusClient;
      const Info: TTransactionInfo; StartReg, RegCount: Word;
      const RegValues: TRegValues);
    procedure MaskWriteSingleRegisterButtonClick(Sender: TObject);
    procedure ModbusClient1SingleRegisterMaskWrite(Sender: TModbusClient;
      const Info: TTransactionInfo; RegAddr, AndMask, OrMask: Word);
    procedure DiagnosticsButtonClick(Sender: TObject);
    procedure ModbusClient1Diagnostics(Sender: TModbusClient;
      const Info: TTransactionInfo; Action: TDiagnosticAction;
      Result: Word);
    procedure ToolsClearTransactionLogItemClick(Sender: TObject);
    procedure WriteStartBitEditExit(Sender: TObject);
    procedure WriteBitCountEditExit(Sender: TObject);
    procedure WriteStartRegEditExit(Sender: TObject);
    procedure WriteRegCountEditExit(Sender: TObject);
    procedure RegisterListViewEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure ModbusConnection1BeforeOpen(Sender: TObject);
    procedure ModbusConnection1AfterOpen(Sender: TObject);
    procedure ModbusConnection1BeforeClose(Sender: TObject);
    procedure ModbusConnection1AfterClose(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolsDiscardPendingTransactionsItemClick(Sender: TObject);
    procedure ToolsServerOptionsItemClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure RemoveAllButtonClick(Sender: TObject);
    procedure AddServerItemClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ServerItemsListViewDeletion(Sender: TObject;
      Item: TListItem);
    procedure ServerItemsListViewDblClick(Sender: TObject);
    procedure ServerItemsListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ServerItemsListViewCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ModbusServer1AcceptCommand(Sender: TModbusServer;
      Command: Byte; var Accept: Boolean);
    procedure ModbusServer1CanReadCoil(Sender: TModbusServer;
      BitAddr: Word; var Allow: Boolean);
    procedure ModbusServer1CanReadDiscreteInput(Sender: TModbusServer;
      BitAddr: Word; var Allow: Boolean);
    procedure ModbusServer1CanReadHoldingRegister(Sender: TModbusServer;
      RegAddr: Word; var Allow: Boolean);
    procedure ModbusServer1CanReadInputRegister(Sender: TModbusServer;
      RegAddr: Word; var Allow: Boolean);
    procedure ModbusServer1CanWriteCoil(Sender: TModbusServer;
      BitAddr: Word; BitValue: Boolean; var Status: TItemWriteStatus);
    procedure ModbusServer1CanWriteHoldingRegister(Sender: TModbusServer;
      RegAddr, RegValue: Word; var Status: TItemWriteStatus);
    procedure ModbusServer1GetCoilValue(Sender: TModbusServer;
      BitAddr: Word; var BitValue: Boolean);
    procedure ModbusServer1GetDiscreteInputValue(Sender: TModbusServer;
      BitAddr: Word; var BitValue: Boolean);
    procedure ModbusServer1GetHoldingRegisterValue(Sender: TModbusServer;
      RegAddr: Word; var RegValue: Word);
    procedure ModbusServer1GetInputRegisterValue(Sender: TModbusServer;
      RegAddr: Word; var RegValue: Word);
    procedure ModbusServer1SetCoilValue(Sender: TModbusServer;
      BitAddr: Word; BitValue: Boolean);
    procedure ModbusServer1SetHoldingRegisterValue(Sender: TModbusServer;
      RegAddr, RegValue: Word);
    procedure ReadExceptionStatusButtonClick(Sender: TObject);
    procedure ModbusClient1ExceptionStatusRead(Sender: TModbusClient;
      const Info: TTransactionInfo;
      const StatusValues: TExceptionStatusValues);
    procedure ReportServerIDButtonClick(Sender: TObject);
    procedure ModbusClient1ServerIdentificationReport(
      Sender: TModbusClient; const Info: TTransactionInfo; Count: Integer;
      const Data: TServerIdentificationData);
    procedure ReadWriteMultipleRegistersButtonClick(Sender: TObject);
    procedure ModbusClient1MultipleRegistersReadWrite(
      Sender: TModbusClient; const Info: TTransactionInfo; StartRegToRead,
      RegCountToRead: Word; const RegValuesToRead: TRegValues;
      StartRegToWrite, RegCountToWrite: Word;
      const RegValuesToWrite: TRegValues);
    procedure ToolsLogTransactionsToFileItemClick(Sender: TObject);
    procedure ModbusConnection1InspectCapturedFrame(
      Sender: TModbusConnection; ServerAddress, CommandCode: Byte;
      const CommandData: TFrameData);
  private
    fLogTransactionsToFile: Boolean;
    fLogCounter: Cardinal;
    fLogFile: TFileStream;
    procedure BeginLogTransactions;
    procedure ClearTransactionLog;
    procedure ConnectionModeChanged;
    procedure EditSelectedServerItem;
    procedure EndLogTransactions;
    function FindServerItemByAddress(Address: Word; ItemKind: TItemKind): PServerItem;
    procedure LogBroadcast;
    procedure LogDone(ID: Cardinal; const CmdDesc: string);
    procedure LogExceptionStatusBit(BitIndex: Word; BitValue: Boolean);
    procedure LogFrame(const Data: TFrameData; Send: Boolean);
    procedure LogIdentificationData(const Data: TServerIdentificationData);
    procedure LogInit(ID: Cardinal; const CmdDesc: string);
    procedure LogProcessedBits(BitCount: Word; Coils: Boolean);
    procedure LogProcessedRegs(RegCount: Word; Holding: Boolean);
    procedure LogServerReadError(ItemKind: TItemKind; ItemAddr: Word);
    procedure LogServerWriteError(ItemKind: TItemKind; ItemAddr: Word;
      ItemValue: Word; Status: TItemWriteStatus);
    procedure LogSingleBit(BitIndex: Word; BitValue: Boolean; Coil: Boolean);
    procedure LogSingleRegister(RegIndex: Word; RegValue: Word; Holding: Boolean);
    procedure LogStatus(const Info: TTransactionInfo);
    procedure LogString(const S: string);
    procedure LogStringToFile(const S: string);
    procedure UpdateConnectionStatus;
    procedure UpdateDiscreteListView;
    procedure UpdateRegisterListView;
    procedure UpdateServerItem(Item: TListItem);
    procedure ValidateRegisterMaskWriteGroupBox;
    procedure ValidateDiscreteReadGroupBox;
    procedure ValidateDiscreteWriteGroupBox;
    procedure ValidateRegisterReadGroupBox;
    procedure ValidateRegisterWriteGroupBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadServerItems(const aFileName: string);
    procedure LoadSettings(aIniFile: TCustomIniFile);
    procedure SaveServerItems(const aFileName: string);
    procedure SaveSettings(aIniFile: TCustomIniFile);
  end;

//--------------------------------------------------------------------------------------------------

var
  ModLinkDemoMainForm: TModLinkDemoMainForm;

//--------------------------------------------------------------------------------------------------

implementation

//--------------------------------------------------------------------------------------------------

uses SysConst, Consts, Math, ModbusConnectionEditor, ModLinkAboutBox;

//--------------------------------------------------------------------------------------------------

{$R *.dfm}

//--------------------------------------------------------------------------------------------------

const
  gcGeneral               = 'General';
  gcModbusConnection1     = 'ModbusConnection1';
  gcModbusClient1         = 'ModbusClient1';
  gcModbusServer1         = 'ModbusServer1';
  // ---
  gcAddress               = 'Address';
  gcBaudRate              = 'BaudRate';
  gcConnectionMode        = 'ConnectionMode';
  gcCustomBaudRate        = 'CustomBaudRate';
  gcDataBits              = 'DataBits';
  gcDTREnabled            = 'DTREnabled';
  gcEchoQryBeforeRpy      = 'EchoQueryBeforeReply';
  gcFlowControl           = 'FlowControl';
  gcLogTransactionsToFile = 'LogTransactionsToFile';
  gcMaxRetries            = 'MaxRetries';
  gcParity                = 'Parity';
  gcPort                  = 'Port';
  gcReceiveTimeout        = 'ReceiveTimeout';
  gcRefetchDelay          = 'RefetchDelay';
  gcRTSEnabled            = 'RTSEnabled';
  gcRTSHoldDelay          = 'RTSHoldDelay';
  gcSendTimeout           = 'SendTimeout';
  gcServerAddress         = 'ServerAddress';
  gcSilentInterval        = 'SilentInterval';
  gcStopBits              = 'StopBits';
  gcThreadPriority        = 'ThreadPriority';
  gcTransmissionMode      = 'TransmissionMode';
  gcTurnaroundDelay       = 'TurnaroundDelay';

//--------------------------------------------------------------------------------------------------

type
  TAccessSettingsProc = procedure (aIniFile: TCustomIniFile) of object;

//--------------------------------------------------------------------------------------------------

procedure AccessSettings(const IniFileName: string; AccessProc: TAccessSettingsProc);
var
  IniFile: TCustomIniFile;
begin
  IniFile := TIniFile.Create(IniFileName);
  try
    AccessProc(IniFile);
  finally
    IniFile.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

var
  SettingsFileName: TFileName = '';

//--------------------------------------------------------------------------------------------------

function GetSettingsFileName: TFileName;
begin
  if SettingsFileName = EmptyStr then
    SettingsFileName := ExtractFilePath(ParamStr(0)) + 'ModLinkDemo.dat';
  Result := SettingsFileName;
end;

//--------------------------------------------------------------------------------------------------

var
  ServerItemsFileName: TFileName = '';

//--------------------------------------------------------------------------------------------------

function GetServerItemsFileName: TFileName;
begin
  if ServerItemsFileName = EmptyStr then
    ServerItemsFileName := ExtractFilePath(ParamStr(0)) + 'ServerItems.dat';
  Result := ServerItemsFileName;
end;

//--------------------------------------------------------------------------------------------------

var
  TransactionLogFileName: TFileName = '';

//--------------------------------------------------------------------------------------------------

function GetTransactionLogFileName: TFileName;
begin
  if TransactionLogFileName = EmptyStr then
    TransactionLogFileName := ExtractFilePath(ParamStr(0)) + 'TransactionLog.txt';
  Result := TransactionLogFileName;
end;

//--------------------------------------------------------------------------------------------------

const
  ServerReplies: array [TServerReply] of string = (
    { srNone            } 'FAILURE: Response timeout expired',
    { srInvalidFrame    } 'FAILURE: Response frame corrupted',
    { srUnexpectedReply } 'FAILURE: Mistargeted response',
    { srUnmatchedReply  } 'FAILURE: Unmatched response',
    { srExceptionReply  } 'FAILURE: Modbus exception response',
    { srNormalReply     } 'SUCCESS: Normal response'
  );

//--------------------------------------------------------------------------------------------------

procedure ValidateNumberInEditBox(AEdit: TEdit; MinValue, MaxValue: Int64);

  // begin of local block --------------------------------------------------------------------------

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

{$IFNDEF COMPILER_5_UP}

// FreeAndNil doesn't exist in Delphi 4.
procedure FreeAndNil(var Obj);
var
  TempObj: TObject;
begin
  TempObj := TObject(Obj);
  Pointer(Obj) := nil;
  TempObj.Free;
end;

{$ENDIF COMPILER_5_UP}

//--------------------------------------------------------------------------------------------------
// TModLinkDemoMainForm class
//--------------------------------------------------------------------------------------------------

constructor TModLinkDemoMainForm.Create(AOwner: TComponent);
const
  SIntroductionFileName = 'Introduction.rtf';
begin
  inherited;
  AccessSettings(GetSettingsFileName, LoadSettings);
  ToolsLogTransactionsToFileItem.Checked := fLogTransactionsToFile;

  LogString('');
  if fLogTransactionsToFile then
    LogString('*** Transactions are now being logged to both screen and file.')
  else
    LogString('*** Transactions are now being logged to screen only.');
  LogString('');

  LogString('*** Welcome to ModLink VCL Demo!');
  LogString(Format('*** Running on ModLink version %s', [ModLinkVersion]));
  LogString('');
  LogString('*** To specify in which mode the demo should operate:');
  LogString('*** [1] Select ''Tools -> Modbus Connection Options...'' from the menu.');
  LogString('*** [2] A dialog window will appear.');
  LogString('*** [3] Go to ''Modbus Transaction Management'' page.');
  LogString('*** [4] Locate ''Connection Mode'' group box.');
  LogString('*** [5] Select ''Client'' radio button to operate in client mode.');
  LogString('*** [6] Select ''Server'' radio button to operate in server mode.');
  LogString('*** [7] Select ''Monitor'' radio button to monitor serial network.');
  LogString('*** [8] Click OK to close the dialog window and apply the changes.');
  LogString('');

  LoadServerItems(GetServerItemsFileName);

  try
    try
      ModbusConnection1.Open;
    except
      on E: Exception do
      begin
        LogString(Format('Failed to open Modbus connection. %s'#13#10, [E.Message]));

        {$IFDEF COMPILER_6_UP}
        if Assigned(ApplicationHandleException) then
          ApplicationHandleException(Self);
        {$ELSE}
        Application.HandleException(Self);
        {$ENDIF}
      end;
    end;
  finally
    ConnectionModeChanged;
    UpdateConnectionStatus;
  end;

  Application.HintHidePause := 20000;

  PageControl1.ActivePage := IntroductionTabSheet;
  RegisterListView.Column[0].Index := 1;
  UpdateDiscreteListView;
  UpdateRegisterListView;

  try
    IntroductionRichEdit.Lines.LoadFromFile(ExtractFilePath(ParamStr(0)) + SIntroductionFileName);
  except
    {$IFDEF COMPILER_6_UP}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
    {$ELSE}
    Application.HandleException(Self);
    {$ENDIF}
  end;
end;

//--------------------------------------------------------------------------------------------------

destructor TModLinkDemoMainForm.Destroy;
begin
  fLogFile.Free;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.BeginLogTransactions;
var
  OpenMode: Word;
begin
  if not Assigned(fLogFile) then
  begin
    if FileExists(GetTransactionLogFileName) then
      OpenMode := fmOpenReadWrite or fmShareDenyWrite
    else
      OpenMode := fmCreate or fmShareDenyWrite;
    fLogFile := TFileStream.Create(GetTransactionLogFileName, OpenMode);
    fLogFile.Seek(0, soFromEnd);
  end;
  Inc(fLogCounter);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ClearTransactionLog;
begin
  LogMemo.Clear;
  if fLogTransactionsToFile then
  begin
    BeginLogTransactions;
    try
      Assert(Assigned(fLogFile));
      fLogFile.Size := 0;
    finally
      EndLogTransactions;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ConnectionModeChanged;
const
  ConnectionModes: array [TConnectionMode] of string = ('Client', 'Server', 'Monitor');
begin
  Caption := Format('%s [%s Mode]', [Application.Title,
    ConnectionModes[ModbusConnection1.ConnectionMode]]);

  ToolsClientOptionsItem.Visible := ModbusConnection1.ConnectionMode = cmClient;
  ToolsServerOptionsItem.Visible := ModbusConnection1.ConnectionMode = cmServer;
  ToolsDiscardPendingTransactionsItem.Visible := ModbusConnection1.ConnectionMode = cmClient;

  RegisterAccessTabSheet.TabVisible := ModbusConnection1.ConnectionMode = cmClient;
  DiscreteAccessTabSheet.TabVisible := ModbusConnection1.ConnectionMode = cmClient;
  DiagnosticsTabSheet.TabVisible := ModbusConnection1.ConnectionMode = cmClient;
  ServerMapTabSheet.TabVisible := ModbusConnection1.ConnectionMode = cmServer;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.EditSelectedServerItem;
var
  ListItem: TListItem;
begin
  with ServerItemsListView do
  begin
    ListItem := Selected;
    if Assigned(ListItem) then
    begin
      if EditServerItem(PServerItem(ListItem.Data), False) then
      begin
        SaveServerItems(GetServerItemsFileName);
        UpdateServerItem(ListItem);
      end;
    end
    else
      Beep;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.EndLogTransactions;
begin
  if fLogCounter > 0 then
    Dec(fLogCounter);
  if fLogCounter = 0 then
    FreeAndNil(fLogFile);
end;

//--------------------------------------------------------------------------------------------------

function TModLinkDemoMainForm.FindServerItemByAddress(Address: Word; ItemKind: TItemKind): PServerItem;
var
  I: Integer;
  ServerItem: PServerItem;
begin
  Result := nil;
  for I := 0 to ServerItemsListView.Items.Count - 1 do
  begin
    ServerItem := PServerItem(ServerItemsListView.Items[I].Data);
    if (ServerItem^.Addr = Address) and (ServerItem^.Kind = ItemKind) then
    begin
      Result := ServerItem;
      Break;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LoadServerItems(const aFileName: string);
var
  vStream: TFileStream;
  vCount, I, X: Integer;
  vServerItem: PServerItem;
  vListItem: TListItem;
begin
  ServerItemsListView.Items.Clear;
  if FileExists(aFileName) then
  begin
    vStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    try
      ServerItemsListView.Items.BeginUpdate;
      try
        { Obtain the number of server items to be read from the stream }
        vCount := vStream.Size div SizeOf(TServerItem);
        for I := 0 to Pred(vCount) do
        begin
          New(vServerItem);
          try
            LoadServerItem(vServerItem, vStream);
            vListItem := ServerItemsListView.Items.Add;
            try
              for X := 0 to 4 do
                vListItem.SubItems.Add('');
              vListItem.Data := Pointer(vServerItem);
              UpdateServerItem(vListItem);
            except
              ServerItemsListView.Items.Delete(vListItem.Index);
              raise;
            end;
          except
            Dispose(vServerItem);
            raise;
          end;
        end;
      finally
        ServerItemsListView.Items.EndUpdate;
      end;
    finally
      vStream.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LoadSettings(aIniFile: TCustomIniFile);
const
  cDefaultPort = 'COM1';
begin
  fLogTransactionsToFile := aIniFile.ReadBool(gcGeneral, gcLogTransactionsToFile, False);
  with ModbusConnection1 do
  begin
    BaudRate := TBaudRate(aIniFile.ReadInteger(gcModbusConnection1, gcBaudRate, Ord(br19200)));

    if BaudRate = brCustom then
      CustomBaudRate := Cardinal(aIniFile.ReadInteger(gcModbusConnection1, gcCustomBaudRate, 19200));

    ConnectionMode := TConnectionMode(aIniFile.ReadInteger(gcModbusConnection1, gcConnectionMode, Ord(cmClient)));
    DataBits := TDataBits(aIniFile.ReadInteger(gcModbusConnection1, gcDataBits, Ord(db8)));
    DTREnabled := aIniFile.ReadBool(gcModbusConnection1, gcDTREnabled, True);
    EchoQueryBeforeReply := aIniFile.ReadBool(gcModbusConnection1, gcEchoQryBeforeRpy, False);
    FlowControl := TFlowControl(aIniFile.ReadInteger(gcModbusConnection1, gcFlowControl, Ord(fcNone)));
    MaxRetries := TMaxRetries(aIniFile.ReadInteger(gcModbusConnection1, gcMaxRetries, 1));
    Parity := TParityScheme(aIniFile.ReadInteger(gcModbusConnection1, gcParity, Ord(psEven)));
    Port := aIniFile.ReadString(gcModbusConnection1, gcPort, cDefaultPort);
    ReceiveTimeout := Cardinal(aIniFile.ReadInteger(gcModbusConnection1, gcReceiveTimeout, 1000));
    RefetchDelay := Cardinal(aIniFile.ReadInteger(gcModbusConnection1, gcRefetchDelay, 0));
    RTSEnabled := aIniFile.ReadBool(gcModbusConnection1, gcRTSEnabled, True);
    RTSHoldDelay := Cardinal(aIniFile.ReadInteger(gcModbusConnection1, gcRTSHoldDelay, 0));
    SendTimeout := Cardinal(aIniFile.ReadInteger(gcModbusConnection1, gcSendTimeout, 1000));
    SilentInterval := Cardinal(aIniFile.ReadInteger(gcModbusConnection1, gcSilentInterval, 4));
    StopBits := TStopBits(aIniFile.ReadInteger(gcModbusConnection1, gcStopBits, Ord(sb1)));
    ThreadPriority := TThreadPriority(aIniFile.ReadInteger(gcModbusConnection1, gcThreadPriority, Ord(tpNormal)));
    TransmissionMode := TTransmissionMode(aIniFile.ReadInteger(gcModbusConnection1, gcTransmissionMode, Ord(tmRTU)));
    TurnaroundDelay := Cardinal(aIniFile.ReadInteger(gcModbusConnection1, gcTurnaroundDelay, 100));
  end;
  ModbusClient1.ServerAddress := aIniFile.ReadInteger(gcModbusClient1, gcServerAddress, 1);
  ModbusServer1.Address := aIniFile.ReadInteger(gcModbusServer1, gcAddress, 1);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogBroadcast;
begin
  LogString('Broadcasting mode: No reply is expected to be returned from a remote server(s).');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogDone(ID: Cardinal; const CmdDesc: string);
begin
  LogString(Format('DONE: %s [ID: %d]', [CmdDesc, ID]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogExceptionStatusBit(BitIndex: Word; BitValue: Boolean);
const
  BitStates: array [Boolean] of string = ('OFF', 'ON');
begin
  LogString(Format('Exception status bit %d is %s.', [BitIndex, BitStates[BitValue]]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogFrame(const Data: TFrameData; Send: Boolean);
var
  S: string;
  I: Integer;
begin
  if Send then
    S := 'TX: '
  else
    S := 'RX: ';

  if Length(Data) = 0 then
    S := S + '<empty>'
  else
    for I := 0 to High(Data) do
      S := S + IntToHex(Data[I], 2) + ' ';

  LogString(S);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogIdentificationData(const Data: TServerIdentificationData);
var
  S: string;
  I: Integer;
begin
  if Length(Data) = 0 then
    S := '<empty>'
  else
  begin
    S := '';
    for I := 0 to High(Data) do
      S := S + IntToHex(Data[I], 2) + ' ';
  end;

  LogString(S);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogInit(ID: Cardinal; const CmdDesc: string);
begin
  LogString(Format('INIT: %s [ID: %d]', [CmdDesc, ID]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogProcessedBits(BitCount: Word; Coils: Boolean);
var
  Temp: string;
begin
  if BitCount > 1 then
    if Coils then
      Temp := 'coils were'
    else
      Temp := 'discrete inputs were'
  else
    if Coils then
      Temp := 'coil was'
    else
      Temp := 'discrete input was';

  LogString(Format('%d %s processed.', [BitCount, Temp]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogProcessedRegs(RegCount: Word; Holding: Boolean);
var
  Temp: string;
begin
  if RegCount > 1 then
    if Holding then
      Temp := 'holding registers were'
    else
      Temp := 'input registers were'
  else
    if Holding then
      Temp := 'holding register was'
    else
      Temp := 'input register was';

  LogString(Format('%d %s processed.', [RegCount, Temp]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogServerReadError(ItemKind: TItemKind; ItemAddr: Word);
const
  ItemKinds: array [TItemKind] of string = (
    'coil',
    'discrete input',
    'holding register',
    'input register'
  );
begin
  LogString(Format('[Read Request Error] No %s found at local server address %d.',
    [ItemKinds[ItemKind], ItemAddr]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogServerWriteError(ItemKind: TItemKind; ItemAddr: Word;
  ItemValue: Word; Status: TItemWriteStatus);
const
  ItemKinds: array [TItemKind] of string = (
    'coil',
    'discrete input',
    'holding register',
    'input register'
  );
begin
  if ItemKind in [ikCoil, ikHoldingRegister] then
  begin
    case Status of
      iwsIllegalAddress:
        LogString(Format('[Write Request Error] No writeable %s found at local server address %d.',
          [ItemKinds[ItemKind], ItemAddr]));
      iwsIllegalValue:
        LogString(Format('[Write Request Error] Illegal value (%d) for %s at local server address %d.',
          [ItemValue, ItemKinds[ItemKind], ItemAddr]));
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogSingleBit(BitIndex: Word; BitValue: Boolean; Coil: Boolean);
const
  BitStates: array [Boolean] of string = ('OFF', 'ON');
var
  Temp: string;
begin
  if Coil then
    Temp := 'Coil'
  else
    Temp := 'Discrete input';

  LogString(Format('%s %d is %s.', [Temp, BitIndex, BitStates[BitValue]]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogSingleRegister(RegIndex: Word; RegValue: Word; Holding: Boolean);
const
  RegNames: array [Boolean] of string = ('input', 'holding');
begin
  LogString(Format('Value of %s register %d is %d',
    [RegNames[Holding], RegIndex, RegValue]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogStatus(const Info: TTransactionInfo);
begin
  LogString(ServerReplies[Info.Reply]);
  if Info.Reply = srExceptionReply then
    LogString('Server exception: ' + ModbusClient1.ExceptionCodeToStr(Info.ExceptionCode));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.LogString(const S: string);

  function FormatLoggingTime(const aTime: TDateTime): string;
  begin
    Result := FormatDateTime('hh:nn:ss.zzz', aTime);
  end;

var
  vStringToLog: string;
begin
  if S <> '' then
    vStringToLog := Format('[%s] %s', [FormatLoggingTime(Now), S])
  else
    vStringToLog := '';

  LogMemo.Lines.Add(vStringToLog);
  if fLogTransactionsToFile then
    LogStringToFile(vStringToLog);
end;

//--------------------------------------------------------------------------------------------------

{$IFNDEF COMPILER_5_UP}
const
  sLineBreak = AnsiString(#13#10);
{$ENDIF ~COMPILER_5_UP}

procedure TModLinkDemoMainForm.LogStringToFile(const S: string);
var
  vLength: Integer;
  {$IFDEF UNICODE} vBytes: TBytes; {$ENDIF UNICODE}
begin
  BeginLogTransactions;
  try
    Assert(Assigned(fLogFile));
    {$IFDEF UNICODE}
      vBytes := TEncoding.UTF8.GetBytes(S);
      vLength := Length(vBytes);
      if vLength > 0 then
        fLogFile.WriteBuffer(vBytes[0], vLength);
    {$ELSE ~UNICODE}
      vLength := Length(S);
      if vLength > 0 then
        fLogFile.WriteBuffer(Pointer(S)^, vLength);
    {$ENDIF UNICODE}
    vLength := Length(sLineBreak);
    fLogFile.WriteBuffer(sLineBreak, vLength);
  finally
    EndLogTransactions;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.SaveServerItems(const aFileName: string);
var
  vStream: TFileStream;
  I: Integer;
  vItem: PServerItem;
begin
  vStream := TFileStream.Create(aFileName, fmCreate);
  try
    for I := 0 to Pred(ServerItemsListView.Items.Count) do
    begin
      vItem := PServerItem(ServerItemsListView.Items[I].Data);
      SaveServerItem(vItem, vStream);
    end;
  finally
    vStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.SaveSettings(aIniFile: TCustomIniFile);
begin
  aIniFile.WriteBool(gcGeneral, gcLogTransactionsToFile, fLogTransactionsToFile);
  with ModbusConnection1 do
  begin
    aIniFile.WriteInteger(gcModbusConnection1, gcBaudRate, Ord(BaudRate));

    if BaudRate = brCustom then
      aIniFile.WriteInteger(gcModbusConnection1, gcCustomBaudRate, CustomBaudRate);

    aIniFile.WriteInteger(gcModbusConnection1, gcConnectionMode, Ord(ConnectionMode));
    aIniFile.WriteInteger(gcModbusConnection1, gcDataBits, Ord(DataBits));
    aIniFile.WriteBool(gcModbusConnection1, gcDTREnabled, DTREnabled);
    aIniFile.WriteBool(gcModbusConnection1, gcEchoQryBeforeRpy, EchoQueryBeforeReply);
    aIniFile.WriteInteger(gcModbusConnection1, gcFlowControl, Ord(FlowControl));
    aIniFile.WriteInteger(gcModbusConnection1, gcMaxRetries, MaxRetries);
    aIniFile.WriteInteger(gcModbusConnection1, gcParity, Ord(Parity));
    aIniFile.WriteString(gcModbusConnection1, gcPort, Port);
    aIniFile.WriteInteger(gcModbusConnection1, gcReceiveTimeout, ReceiveTimeout);
    aIniFile.WriteInteger(gcModbusConnection1, gcRefetchDelay, RefetchDelay);
    aIniFile.WriteBool(gcModbusConnection1, gcRTSEnabled, RTSEnabled);
    aIniFile.WriteInteger(gcModbusConnection1, gcRTSHoldDelay, RTSHoldDelay);
    aIniFile.WriteInteger(gcModbusConnection1, gcSendTimeout, SendTimeout);
    aIniFile.WriteInteger(gcModbusConnection1, gcSilentInterval, SilentInterval);
    aIniFile.WriteInteger(gcModbusConnection1, gcStopBits, Ord(StopBits));
    aIniFile.WriteInteger(gcModbusConnection1, gcThreadPriority, Ord(ThreadPriority));
    aIniFile.WriteInteger(gcModbusConnection1, gcTransmissionMode, Ord(TransmissionMode));
    aIniFile.WriteInteger(gcModbusConnection1, gcTurnaroundDelay, TurnaroundDelay);
  end;
  aIniFile.WriteInteger(gcModbusClient1, gcServerAddress, ModbusClient1.ServerAddress);
  aIniFile.WriteInteger(gcModbusServer1, gcAddress, ModbusServer1.Address);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.UpdateConnectionStatus;
begin
  if ModbusConnection1.Active then
    StatusBar1.Panels[0].Text := Format('Modbus connection: Opened [%s]', [ModbusConnection1.Port])
  else
    StatusBar1.Panels[0].Text := 'Modbus connection: Closed';

  case ModbusConnection1.ConnectionMode of
    cmClient:
      StatusBar1.Panels[1].Text := Format('Remote server address: %d', [ModbusClient1.ServerAddress]);
    cmServer:
      StatusBar1.Panels[1].Text := Format('Local server address: %d', [ModbusServer1.Address]);
    cmMonitor:
      StatusBar1.Panels[1].Text := 'Monitoring serial network...';
  else
    StatusBar1.Panels[1].Text := '';
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.UpdateDiscreteListView;
var
  StartBit, BitCount: Word;
  I: Integer;
begin
  ValidateDiscreteWriteGroupBox;
  
  StartBit := Word(StrToInt(WriteStartBitEdit.Text));
  BitCount := Word(StrToInt(WriteBitCountEdit.Text));

  with DiscreteListView.Items do
  begin
    BeginUpdate;
    try
      if Count < BitCount then
        while Count < BitCount do
          Add
      else if Count > BitCount then
        while (Count > 0) and (Count > BitCount) do
          Delete(Count - 1);

      for I := 0 to Count - 1 do
        Item[I].Caption := Format('Coil %d', [StartBit + I]);
    finally
      EndUpdate;
      WriteSingleCoilButton.Enabled := Count = 1;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.UpdateRegisterListView;
var
  StartReg, RegCount: Word;
  I: Integer;
  Temp: TListItem;
begin
  ValidateRegisterWriteGroupBox;

  StartReg := Word(StrToInt(WriteStartRegEdit.Text));
  RegCount := Word(StrToInt(WriteRegCountEdit.Text));

  with RegisterListView.Items do
  begin
    BeginUpdate;
    try
      if Count < RegCount then
        while Count < RegCount do
        begin
          Temp := Add;
          Temp.Caption := '0';
          Temp.SubItems.Add('');
        end
      else if Count > RegCount then
        while (Count > 0) and (Count > RegCount) do
          Delete(Count - 1);

      for I := 0 to Count - 1 do
        Item[I].SubItems[0] := Format('Register %d', [StartReg + I]);
    finally
      EndUpdate;
      WriteSingleRegisterButton.Enabled := (Count = 1);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.UpdateServerItem(Item: TListItem);
const
  ItemKinds: array [TItemKind] of string = (
    'Coil',
    'Discrete Input',
    'Holding Register',
    'Input Register'
  );
  Booleans: array [Boolean] of string = (
    'Off',
    'On'
  );
  StatusMessages: array [Boolean] of string = (
    'Read-Only',
    'Read/Write'
  );
begin
  with Item do
  begin
    Caption := IntToStr(PServerItem(Data)^.Addr);
    SubItems[0] := ItemKinds[PServerItem(Data)^.Kind];
    if PServerItem(Data)^.Kind in [ikCoil, ikDiscreteInput] then
    begin
      SubItems[1] := Booleans[Boolean(PServerItem(Data)^.Value)];
      SubItems[2] := 'N/A';
      SubItems[3] := 'N/A';
    end
    else
    begin
      SubItems[1] := IntToStr(PServerItem(Data)^.Value);
      SubItems[2] := IntToStr(PServerItem(Data)^.MinValue);
      SubItems[3] := IntToStr(PServerItem(Data)^.MaxValue);
    end;
    if PServerItem(Data)^.Kind in [ikCoil, ikHoldingRegister] then
        SubItems[4] := StatusMessages[PServerItem(Data)^.Writeable]
      else
        SubItems[4] := StatusMessages[False];
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ValidateRegisterMaskWriteGroupBox;
begin
  ValidateNumberInEditBox(MaskWriteRegAddrEdit, 0, High(Word));
  ValidateNumberInEditBox(AndMaskEdit, 0, High(Word));
  ValidateNumberInEditBox(OrMaskEdit, 0, High(Word));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ValidateDiscreteReadGroupBox;
begin
  ValidateNumberInEditBox(ReadStartBitEdit, 0, High(Word));
  ValidateNumberInEditBox(ReadBitCountEdit, 1, 2008);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ValidateDiscreteWriteGroupBox;
begin
  ValidateNumberInEditBox(WriteStartBitEdit, 0, High(Word));
  ValidateNumberInEditBox(WriteBitCountEdit, 1, 1976);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ValidateRegisterReadGroupBox;
begin
  ValidateNumberInEditBox(ReadStartRegEdit, 0, High(Word));
  ValidateNumberInEditBox(ReadRegCountEdit, 1, 125);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ValidateRegisterWriteGroupBox;
begin
  ValidateNumberInEditBox(WriteStartRegEdit, 0, High(Word));
  ValidateNumberInEditBox(WriteRegCountEdit, 1, 123);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ToolsConnectionOptionsItemClick(Sender: TObject);
begin
  try
    try
      if EditModbusConnection(ModbusConnection1, 'Modbus Connection Options') then
      begin
        AccessSettings(GetSettingsFileName, SaveSettings);
        ModbusConnection1.Open;
      end;
    except
      on E: Exception do
      begin
        LogString(Format('Failed to open Modbus connection. %s'#13#10, [E.Message]));
        raise;
      end;
    end;
  finally
    ConnectionModeChanged;
    UpdateConnectionStatus;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ToolsClientOptionsItemClick(Sender: TObject);
const
  SCaptionFmt = 'Modbus Client Options';
  SPrompt = 'Enter the address of a remote server ' +
    '(acceptable values are 1 through 247):';
var
  S: string;
  NewAddress: Byte;
begin
  S := IntToStr(ModbusClient1.ServerAddress);
  if InputQuery(SCaptionFmt, SPrompt, S) then
  begin
    try
      NewAddress := Byte(StrToInt(S));
    except
      on E: EConvertError do
      begin
        E.Message := Format('''%s'' is not a valid server address.', [S]);
        raise;
      end
      else raise;
    end;
    ModbusClient1.ServerAddress := NewAddress;
    AccessSettings(GetSettingsFileName, SaveSettings);
    UpdateConnectionStatus;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.HelpAboutItemClick(Sender: TObject);
begin
  ShowModLinkAboutBox;
end;


//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ReadCoilsButtonClick(Sender: TObject);
var
  StartBit, BitCount: Word;
  ID: Cardinal;
begin
  ValidateDiscreteReadGroupBox;
  StartBit := Word(StrToInt(ReadStartBitEdit.Text));
  BitCount := Word(StrToInt(ReadBitCountEdit.Text));
  ID := ModbusClient1.ReadCoils(StartBit, BitCount);
  LogInit(ID, 'Read Coils (code $01)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ReadDiscreteInputsButtonClick(Sender: TObject);
var
  StartBit, BitCount: Word;
  ID: Cardinal;
begin
  ValidateDiscreteReadGroupBox;
  StartBit := Word(StrToInt(ReadStartBitEdit.Text));
  BitCount := Word(StrToInt(ReadBitCountEdit.Text));
  ID := ModbusClient1.ReadDiscreteInputs(StartBit, BitCount);
  LogInit(ID, 'Read Discrete Inputs (code $02)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteSingleCoilButtonClick(Sender: TObject);
var
  S: string;
  BitAddr: Word;
  BitValue: Boolean;
  ID: Cardinal;
begin
  with DiscreteListView.Items[0] do
  begin
    S := Caption;
    System.Delete(S, 1, Length('Coil '));
    BitAddr := Word(StrToInt(S));
    BitValue := Checked;
  end;

  if DiscreteBroadcastCheckBox.Checked then
  begin
    ID := ModbusConnection1.WriteSingleCoil(BitAddr, BitValue);
    LogInit(ID, 'Write Single Coil (code $05)');
    LogBroadcast;
  end
  else
  begin
    ID := ModbusClient1.WriteSingleCoil(BitAddr, BitValue);
    LogInit(ID, 'Write Single Coil (code $05)');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteMultipleCoilsButtonClick(Sender: TObject);
var
  S: string;
  StartBit, BitCount: Word;
  BitValues: TBitValues;
  I: Integer;
  ID: Cardinal;
begin
  with DiscreteListView.Items[0] do
  begin
    S := Caption;
    System.Delete(S, 1, Length('Coil '));
    StartBit := Word(StrToInt(S));
  end;

  BitCount := DiscreteListView.Items.Count;
  SetLength(BitValues, BitCount);

  try
    for I := 0 to BitCount - 1 do
      BitValues[I] := DiscreteListView.Items[I].Checked;

    if DiscreteBroadcastCheckBox.Checked then
    begin
      ID := ModbusConnection1.WriteMultipleCoils(StartBit, BitValues);
      LogInit(ID, 'Write Multiple Coils (code $0F)');
      LogBroadcast;
    end
    else
    begin
      ID := ModbusClient1.WriteMultipleCoils(StartBit, BitValues);
      LogInit(ID, 'Write Multiple Coils (code $0F)');
    end;
  finally
    Finalize(BitValues);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1CoilsRead(Sender: TModbusClient;
  const Info: TTransactionInfo; BitStart, BitCount: Word;
  const BitValues: TBitValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Read Coils (code $01)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedBits(BitCount, True);
    for I := 0 to BitCount - 1 do
      LogSingleBit(BitStart + I, BitValues[I], True);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1DiscreteInputsRead(
  Sender: TModbusClient; const Info: TTransactionInfo; BitStart,
  BitCount: Word; const BitValues: TBitValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Read Discrete Inputs (code $02)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedBits(BitCount, False);
    for I := 0 to BitCount - 1 do
      LogSingleBit(BitStart + I, BitValues[I], False);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1SingleCoilWrite(Sender: TModbusClient;
  const Info: TTransactionInfo; BitAddr: Word; BitValue: Boolean);
begin
  LogDone(Info.ID, 'Write Single Coil (code $05)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedBits(1, True);
    LogSingleBit(BitAddr, BitValue, True);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1MultipleCoilsWrite(
  Sender: TModbusClient; const Info: TTransactionInfo; BitStart,
  BitCount: Word; const BitValues: TBitValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Write Multiple Coils (code $0F)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedBits(BitCount, True);
    for I := 0 to BitCount - 1 do
      LogSingleBit(BitStart + I, BitValues[I], True);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusConnection1FrameSend(
  Sender: TModbusConnection; const Data: TFrameData);
begin
  LogFrame(Data, True);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusConnection1FrameReceive(
  Sender: TModbusConnection; const Data: TFrameData);
begin
  LogFrame(Data, False);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ReadHoldingRegistersButtonClick(Sender: TObject);
var
  StartReg, RegCount: Word;
  ID: Cardinal;
begin
  ValidateRegisterReadGroupBox;
  StartReg := Word(StrToInt(ReadStartRegEdit.Text));
  RegCount := Word(StrToInt(ReadRegCountEdit.Text));
  ID := ModbusClient1.ReadHoldingRegisters(StartReg, RegCount);
  LogInit(ID, 'Read Holding Registers (code $03)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ReadInputRegistersButtonClick(Sender: TObject);
var
  StartReg, RegCount: Word;
  ID: Cardinal;
begin
  ValidateRegisterReadGroupBox;
  StartReg := Word(StrToInt(ReadStartRegEdit.Text));
  RegCount := Word(StrToInt(ReadRegCountEdit.Text));
  ID := ModbusClient1.ReadInputRegisters(StartReg, RegCount);
  LogInit(ID, 'Read Input Registers (code $04)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteSingleRegisterButtonClick(Sender: TObject);
var
  S: string;
  RegAddr, RegValue: Word;
  ID: Cardinal;
begin
  with RegisterListView.Items[0] do
  begin
    S := SubItems[0];
    System.Delete(S, 1, Length('Register '));
    RegAddr := Word(StrToInt(S));

    S := Caption;
    RegValue := Word(StrToInt(S));
  end;

  if RegisterBroadcastCheckBox.Checked then
  begin
    ID := ModbusConnection1.WriteSingleRegister(RegAddr, RegValue);
    LogInit(ID, 'Write Single Register (code $06)');
    LogBroadcast;
  end
  else
  begin
    ID := ModbusClient1.WriteSingleRegister(RegAddr, RegValue);
    LogInit(ID, 'Write Single Register (code $06)');
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteMultipleRegistersButtonClick(Sender: TObject);
var
  S: string;
  StartReg: Word;
  RegValues: TRegValues;
  RegIndex, I: Integer;
  ID: Cardinal;
begin
  with RegisterListView.Items[0] do
  begin
    S := SubItems[0];
    System.Delete(S, 1, Length('Register '));
    StartReg := Word(StrToInt(S));
  end;

  SetLength(RegValues, RegisterListView.Items.Count);

  try
    RegIndex := 0;
    for I := 0 to RegisterListView.Items.Count - 1 do
    begin
      S := RegisterListView.Items[I].Caption;
      RegValues[RegIndex] := Word(StrToInt(S));
      Inc(RegIndex);
    end;

    if RegisterBroadcastCheckBox.Checked then
    begin
      ID := ModbusConnection1.WriteMultipleRegisters(StartReg, RegValues);
      LogInit(ID, 'Write Multiple Registers (code $10)');
      LogBroadcast;
    end
    else
    begin
      ID := ModbusClient1.WriteMultipleRegisters(StartReg, RegValues);
      LogInit(ID, 'Write Multiple Registers (code $10)');
    end;
  finally
    Finalize(RegValues);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.RegisterListViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  with Sender as TListView do
    if (Key = VK_RETURN) and (not IsEditing) and (Selected <> nil) then
    begin
      Selected.EditCaption;
      Key := 0;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.RegisterListViewDblClick(Sender: TObject);
var
  P: TPoint;
  Item: TListItem;
begin
  P := Mouse.CursorPos;
  with Sender as TListView do
  begin
    P := ScreenToClient(P);
    Item := GetItemAt(P.X, P.Y);
    if Assigned(Item) then Item.EditCaption;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1HoldingRegistersRead(
  Sender: TModbusClient; const Info: TTransactionInfo; StartReg,
  RegCount: Word; const RegValues: TRegValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Read Holding Registers (code $03)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedRegs(RegCount, True);
    for I := 0 to RegCount - 1 do
      LogSingleRegister(StartReg + I, RegValues[I], True);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1InputRegistersRead(
  Sender: TModbusClient; const Info: TTransactionInfo; StartReg,
  RegCount: Word; const RegValues: TRegValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Read Input Registers (code $04)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedRegs(RegCount, False);
    for I := 0 to RegCount - 1 do
      LogSingleRegister(StartReg + I, RegValues[I], False);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1SingleRegisterWrite(
  Sender: TModbusClient; const Info: TTransactionInfo; RegAddr,
  RegValue: Word);
begin
  LogDone(Info.ID, 'Write Single Register (code $06)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedRegs(1, True);
    LogSingleRegister(RegAddr, RegValue, True);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1MultipleRegistersWrite(
  Sender: TModbusClient; const Info: TTransactionInfo; StartReg,
  RegCount: Word; const RegValues: TRegValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Write Multiple Registers (code $10)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedRegs(RegCount, True);
    for I := 0 to RegCount - 1 do
      LogSingleRegister(StartReg + I, RegValues[I], True);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.MaskWriteSingleRegisterButtonClick(Sender: TObject);
var
  RegAddr, AndMask, OrMask: Word;
  ID: Cardinal;
begin
  ValidateRegisterMaskWriteGroupBox;
  RegAddr := Word(StrToInt(MaskWriteRegAddrEdit.Text));
  AndMask := Word(StrToInt(AndMaskEdit.Text));
  OrMask := Word(StrToInt(OrMaskEdit.Text));
  ID := ModbusClient1.MaskWriteSingleRegister(RegAddr, AndMask, OrMask);
  LogInit(ID, 'Mask Write Register (code $16)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1SingleRegisterMaskWrite(
  Sender: TModbusClient; const Info: TTransactionInfo; RegAddr, AndMask,
  OrMask: Word);
begin
  LogDone(Info.ID, 'Mask Write Register (code $16)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogProcessedRegs(1, True);
    LogString(Format('Register: %d | AND mask: %d | OR mask: %d',
      [RegAddr, AndMask, OrMask]));
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.DiagnosticsButtonClick(Sender: TObject);
var
  Action: TDiagnosticAction;
  ID: Cardinal;
begin
  Action := TDiagnosticAction(DiagnosticActionRadioGroup.ItemIndex);
  ID := ModbusClient1.Diagnostics(Action);
  LogInit(ID, 'Diagnostics (code $08)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1Diagnostics(Sender: TModbusClient;
  const Info: TTransactionInfo; Action: TDiagnosticAction; Result: Word);
begin
  LogDone(Info.ID, 'Diagnostics (code $08)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    case Action of
      daReturnQueryData:
        LogString('"Return Query Data" action was performed by the server. It appears to be responding.');
      daRestartCommsOption:
        LogString('"Restart Comms Option" action was performed by the server.');
      daRestartCommsOptionAndClearEventLog:
        LogString('"Restart Comms Option And Clear Event Log" action was performed by the server.');
      daReturnDiagnosticRegister:
        LogString(Format('"Return Diagnostic Register" action returned the value %d.', [Result]));
      daForceListenOnlyMode:
        LogString('"Force Listen Only Mode" action was performed by the server. It should never go here.');
      daClearCountersAndDiagnosticRegister:
        LogString('"Clear Counters And Diagnostic Register" action was performed by the server.');
      daReturnBusMessageCount:
        LogString(Format('"Return Bus Message Count" action returned the value %d.', [Result]));
      daReturnBusCommErrorCount:
        LogString(Format('"Return Bus Comm Error Count" action returned the value %d.', [Result]));
      daReturnBusExceptionErrorCount:
        LogString(Format('"Return Bus Exception Error Count" action returned the value %d.', [Result]));
      daReturnServerMessageCount:
        LogString(Format('"Return Server Message Count" action returned the value %d.', [Result]));
      daReturnServerNoReplyCount:
        LogString(Format('"Return Server No Reply Count" action returned the value %d.', [Result]));
      daReturnServerNegativeAcknowledgeCount:
        LogString(Format('"Return Server Negative Acknowledge Count" action returned the value %d.', [Result]));
      daReturnServerBusyCount:
        LogString(Format('"Return Server Busy Count" action returned the value %d.', [Result]));
      daReturnBusCharacterOverrunCount:
        LogString(Format('"Return Bus Character Overrun Count" action returned the value %d.', [Result]));
      daClearOverrunCounterAndFlag:
        LogString('"Clear Overrun Counter And Flag" action was performed by the server.');
    end;
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ToolsClearTransactionLogItemClick(Sender: TObject);
begin
  ClearTransactionLog;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteStartBitEditExit(Sender: TObject);
begin
  UpdateDiscreteListView;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteBitCountEditExit(Sender: TObject);
begin
  UpdateDiscreteListView;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteStartRegEditExit(Sender: TObject);
begin
  UpdateRegisterListView;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.WriteRegCountEditExit(
  Sender: TObject);
begin
  UpdateRegisterListView;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.RegisterListViewEdited(Sender: TObject;
  Item: TListItem; var S: String);
begin
  try
    S := IntToStr(Word(StrToInt(S)));
  except
    on E: EConvertError do
    begin
      S := Item.Caption;
      E.Message := Format('You''ve entered an invalid value for ''%s''', [Item.SubItems[0]]);
      raise;
    end;
  else
    raise;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusConnection1BeforeOpen(
  Sender: TObject);
begin
  LogString(Format('Opening Modbus connection on %s...', [ModbusConnection1.Port]));
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusConnection1AfterOpen(
  Sender: TObject);
begin
  LogString('Modbus connection has been successfully opened.'#13#10);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusConnection1BeforeClose(
  Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    LogString('Closing Modbus connection...');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusConnection1AfterClose(
  Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    LogString('Modbus Connection has been closed.'#13#10);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.Timer1Timer(Sender: TObject);
var
  PendingCount: Integer;
begin
  if ModbusConnection1.ConnectionMode = cmClient then
  begin
    PendingCount := ModbusConnection1.CountPendingTransactions;
    with StatusBar1.Panels[2] do
      if PendingCount > 1 then
        Text := Format('There are %d pending transactions.', [PendingCount])
      else if PendingCount = 1 then
        Text := 'There is 1 pending transaction.'
      else
        Text := 'Transaction queue is empty.';
  end
  else
    StatusBar1.Panels[2].Text := '';
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ToolsDiscardPendingTransactionsItemClick(
  Sender: TObject);
begin
  ModbusConnection1.DiscardPendingTransactions;
  LogString('');
  LogString('*** All pending transactions have been discarded.');
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ToolsServerOptionsItemClick(Sender: TObject);
const
  SCaptionFmt = 'Modbus Server Options';
  SPrompt = 'Enter the address of a local server ' +
    '(acceptable values are 1 through 247):';
var
  S: string;
  NewAddress: Byte;
begin
  S := IntToStr(ModbusServer1.Address);
  if InputQuery(SCaptionFmt, SPrompt, S) then
  begin
    try
      NewAddress := Byte(StrToInt(S));
    except
      on E: EConvertError do
      begin
        E.Message := Format('''%s'' is not a valid server address.', [S]);
        raise;
      end
      else raise;
    end;
    ModbusServer1.Address := NewAddress;
    AccessSettings(GetSettingsFileName, SaveSettings);
    UpdateConnectionStatus;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.AddButtonClick(Sender: TObject);
var
  P: TPoint;
begin
  PopupMenu1.PopupComponent := Sender as TComponent;
  with Sender as TBitBtn do
  begin
    P.X := Left;
    P.Y := Top + Height;
    with ServerMapTabSheet.ClientToScreen(P) do
      PopupMenu1.Popup(X, Y);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.RemoveButtonClick(Sender: TObject);
var
  ListItem: TListItem;
begin
  with ServerItemsListView do
  begin
    ListItem := Selected;
    if Assigned(ListItem) then
    begin
      if Application.MessageBox('Remove selected entry from the item map?', PChar(Application.Title),
        MB_YESNO or MB_ICONQUESTION) = ID_YES then
      begin
        Items.Delete(ListItem.Index);
        SaveServerItems(GetServerItemsFileName);
      end;
    end
    else
      Beep;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.RemoveAllButtonClick(Sender: TObject);
begin
  with ServerItemsListView do
  begin
    if Items.Count > 0 then
    begin
      if Application.MessageBox('Remove all entries from the item map?', PChar(Application.Title),
        MB_YESNO or MB_ICONQUESTION) = ID_YES then
      begin
        Items.BeginUpdate;
        try
          Items.Clear;
          SaveServerItems(GetServerItemsFileName);
        finally
          Items.EndUpdate;
        end;
      end;
    end
    else
      Beep;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.AddServerItemClick(Sender: TObject);
var
  ServerItem, ExistingItem: PServerItem;
  ListItem: TListItem;
  I: Integer;
begin
  ServerItem := CreateServerItem(0, TItemKind((Sender as TMenuItem).Tag), 0, 0, 65535, True);
  try
    if EditServerItem(ServerItem, True) then
    begin
      ExistingItem := FindServerItemByAddress(ServerItem^.Addr, ServerItem^.Kind);
      if Assigned(ExistingItem) then
      begin
        // There is an item already existing at the specified address. In this case don't create
        // a new list item, but overwrite the data record associated with that list item with
        // the new values.
        ExistingItem^ := ServerItem^;
        // No Assigned check is needed here since FindData method below should never return nil.
        UpdateServerItem(ServerItemsListView.FindData(0, Pointer(ExistingItem), True, False));
      end
      else
      begin
        ListItem := ServerItemsListView.Items.Add;
        try
          for I := 0 to 4 do
            ListItem.SubItems.Add('');
          ListItem.Data := Pointer(ServerItem);
          UpdateServerItem(ListItem);
        except
          ServerItemsListView.Items.Delete(ListItem.Index);
          raise;
        end;
      end;
      SaveServerItems(GetServerItemsFileName);
    end;
  except
    DestroyServerItem(ServerItem);
    raise;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.EditButtonClick(Sender: TObject);
begin
  EditSelectedServerItem;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ServerItemsListViewDeletion(Sender: TObject;
  Item: TListItem);
var
  ServerItem: PServerItem;
begin
  if Assigned(Item) then
  begin
    ServerItem := PServerItem(Item.Data);
    DestroyServerItem(ServerItem);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ServerItemsListViewDblClick(
  Sender: TObject);
var
  P: TPoint;
  Item: TListItem;
begin
  P := Mouse.CursorPos;
  with Sender as TListView do
  begin
    P := ScreenToClient(P);
    Item := GetItemAt(P.X, P.Y);
    if Assigned(Item) then
    begin
      Selected := Item;
      EditSelectedServerItem;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ServerItemsListViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  with Sender as TListView do
    if (Key = VK_RETURN) and (Selected <> nil) then
    begin
      EditSelectedServerItem;
      Key := 0;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ServerItemsListViewCompare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ServerItem1, ServerItem2: PServerItem;
begin
  ServerItem1 := PServerItem(Item1.Data);
  ServerItem2 := PServerItem(Item2.Data);

  if (ServerItem1 = nil) or (ServerItem2 = nil) then Exit;

  if ServerItem1^.Kind = ServerItem2^.Kind then
  begin
    if ServerItem1^.Addr = ServerItem2^.Addr then
      Compare := 0
    else
      if ServerItem1^.Addr < ServerItem2^.Addr then
        Compare := -1
      else
        Compare := 1;
  end
  else
    if ServerItem1^.Kind < ServerItem2^.Kind then
      Compare := -1
    else
      Compare := 1;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1AcceptCommand(
  Sender: TModbusServer; Command: Byte; var Accept: Boolean);
begin
  // In ModLinkDemo, this local Modbus server is able to handle all Modbus commands
  // as supported by ModLink.
  Accept := Command in [
    Cmd_ReadCoils,
    Cmd_ReadDiscreteInputs,
    Cmd_ReadHoldingRegisters,
    Cmd_ReadInputRegisters,
    Cmd_WriteSingleCoil,
    Cmd_WriteSingleRegister,
    Cmd_WriteMultipleCoils,
    Cmd_WriteMultipleRegisters];
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1CanReadCoil(
  Sender: TModbusServer; BitAddr: Word; var Allow: Boolean);
begin
  Allow := FindServerItemByAddress(BitAddr, ikCoil) <> nil;
  if not Allow then
    LogServerReadError(ikCoil, BitAddr);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1CanReadDiscreteInput(
  Sender: TModbusServer; BitAddr: Word; var Allow: Boolean);
begin
  Allow := FindServerItemByAddress(BitAddr, ikDiscreteInput) <> nil;
  if not Allow then
    LogServerReadError(ikDiscreteInput, BitAddr);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1CanReadHoldingRegister(
  Sender: TModbusServer; RegAddr: Word; var Allow: Boolean);
begin
  Allow := FindServerItemByAddress(RegAddr, ikHoldingRegister) <> nil;
  if not Allow then
    LogServerReadError(ikHoldingRegister, RegAddr);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1CanReadInputRegister(
  Sender: TModbusServer; RegAddr: Word; var Allow: Boolean);
begin
  Allow := FindServerItemByAddress(RegAddr, ikInputRegister) <> nil;
  if not Allow then
    LogServerReadError(ikInputRegister, RegAddr);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1CanWriteCoil(
  Sender: TModbusServer; BitAddr: Word; BitValue: Boolean;
  var Status: TItemWriteStatus);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(BitAddr, ikCoil);
  if Assigned(ServerItem) and ServerItem^.Writeable then
    Status := iwsAllowWrite
  else
    Status := iwsIllegalAddress;
  LogServerWriteError(ikCoil, BitAddr, Ord(BitValue), Status);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1CanWriteHoldingRegister(
  Sender: TModbusServer; RegAddr, RegValue: Word;
  var Status: TItemWriteStatus);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(RegAddr, ikHoldingRegister);
  if Assigned(ServerItem) and ServerItem^.Writeable then
    if (RegValue >= ServerItem^.MinValue) and (RegValue <= ServerItem^.MaxValue) then
      Status := iwsAllowWrite
    else
      Status := iwsIllegalValue
  else
    Status := iwsIllegalAddress;
  LogServerWriteError(ikHoldingRegister, RegAddr, RegValue, Status);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1GetCoilValue(
  Sender: TModbusServer; BitAddr: Word; var BitValue: Boolean);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(BitAddr, ikCoil);
  if Assigned(ServerItem) then
    BitValue := Boolean(ServerItem^.Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1GetDiscreteInputValue(
  Sender: TModbusServer; BitAddr: Word; var BitValue: Boolean);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(BitAddr, ikDiscreteInput);
  if Assigned(ServerItem) then
    BitValue := Boolean(ServerItem^.Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1GetHoldingRegisterValue(
  Sender: TModbusServer; RegAddr: Word; var RegValue: Word);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(RegAddr, ikHoldingRegister);
  if Assigned(ServerItem) then
    RegValue := ServerItem^.Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1GetInputRegisterValue(
  Sender: TModbusServer; RegAddr: Word; var RegValue: Word);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(RegAddr, ikInputRegister);
  if Assigned(ServerItem) then
    RegValue := ServerItem^.Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1SetCoilValue(
  Sender: TModbusServer; BitAddr: Word; BitValue: Boolean);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(BitAddr, ikCoil);
  if Assigned(ServerItem) then
  begin
    ServerItem^.Value := Ord(BitValue);
    // No Assigned check is needed here since FindData method below should never return nil.
    UpdateServerItem(ServerItemsListView.FindData(0, Pointer(ServerItem), True, False));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusServer1SetHoldingRegisterValue(
  Sender: TModbusServer; RegAddr, RegValue: Word);
var
  ServerItem: PServerItem;
begin
  ServerItem := FindServerItemByAddress(RegAddr, ikHoldingRegister);
  if Assigned(ServerItem) then
  begin
    ServerItem^.Value := RegValue;
    // No Assigned check is needed here since FindData method below should never return nil.
    UpdateServerItem(ServerItemsListView.FindData(0, Pointer(ServerItem), True, False));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ReadExceptionStatusButtonClick(
  Sender: TObject);
var
  ID: Cardinal;
begin
  ID := ModbusClient1.ReadExceptionStatus;
  LogInit(ID, 'Read Exception Status (code $07)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1ExceptionStatusRead(
  Sender: TModbusClient; const Info: TTransactionInfo;
  const StatusValues: TExceptionStatusValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Read Exception Status (code $07)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogString('8 exception status bits were processed.');
    for I := 0 to High(StatusValues) do
      LogExceptionStatusBit(I, StatusValues[I]);
  end;
  LogString('');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ReportServerIDButtonClick(Sender: TObject);
var
  ID: Cardinal;
begin
  ID := ModbusClient1.ReportServerID;
  LogInit(ID, 'Report Server ID (code $11)');
end;

//--------------------------------------------------------------------------------------------------

procedure TModLinkDemoMainForm.ModbusClient1ServerIdentificationReport(
  Sender: TModbusClient; const Info: TTransactionInfo; Count: Integer;
  const Data: TServerIdentificationData);
begin
  LogDone(Info.ID, 'Report Server ID (code $11)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogString(Format('Size of server identification info is %d bytes.', [Count]));
    LogIdentificationData(Data);
  end;
  LogString('');
end;

procedure TModLinkDemoMainForm.ReadWriteMultipleRegistersButtonClick(
  Sender: TObject);
var
  StartRegToRead, RegCountToRead, StartRegToWrite: Word;
  S: string;
  RegValuesToWrite: TRegValues;
  RegIndex, I: Integer;
  ID: Cardinal;
begin
  ValidateRegisterReadGroupBox;
  StartRegToRead := Word(StrToInt(ReadStartRegEdit.Text));
  RegCountToRead := Word(StrToInt(ReadRegCountEdit.Text));

  with RegisterListView.Items[0] do
  begin
    S := SubItems[0];
    System.Delete(S, 1, Length('Register '));
    StartRegToWrite := Word(StrToInt(S));
  end;

  SetLength(RegValuesToWrite, RegisterListView.Items.Count);
  try
    RegIndex := 0;
    for I := 0 to RegisterListView.Items.Count - 1 do
    begin
      S := RegisterListView.Items[I].Caption;
      RegValuesToWrite[RegIndex] := Word(StrToInt(S));
      Inc(RegIndex);
    end;
    ID := ModbusClient1.ReadWriteMultipleRegisters(StartRegToRead,
      RegCountToRead, StartRegToWrite, RegValuesToWrite);
    LogInit(ID, 'Read/Write Multiple Registers (code $17)');
  finally
    Finalize(RegValuesToWrite);
  end;
end;

procedure TModLinkDemoMainForm.ModbusClient1MultipleRegistersReadWrite(
  Sender: TModbusClient; const Info: TTransactionInfo; StartRegToRead,
  RegCountToRead: Word; const RegValuesToRead: TRegValues; StartRegToWrite,
  RegCountToWrite: Word; const RegValuesToWrite: TRegValues);
var
  I: Integer;
begin
  LogDone(Info.ID, 'Read/Write Multiple Registers (code $17)');
  LogStatus(Info);
  if Info.Reply = srNormalReply then
  begin
    LogString('WRITE OPERATION:');
    LogProcessedRegs(RegCountToWrite, True);
    for I := 0 to RegCountToWrite - 1 do
      LogSingleRegister(StartRegToWrite + I, RegValuesToWrite[I], True);

    LogString('READ OPERATION:');
    LogProcessedRegs(RegCountToRead, True);
    for I := 0 to RegCountToRead - 1 do
      LogSingleRegister(StartRegToRead + I, RegValuesToRead[I], True);
  end;
  LogString('');
end;

procedure TModLinkDemoMainForm.ToolsLogTransactionsToFileItemClick(
  Sender: TObject);
begin
  if fLogTransactionsToFile then
  begin
    LogString('');
    LogString('*** Transactions are now being logged to screen only.');
    LogString('');
  end;

  fLogTransactionsToFile := not fLogTransactionsToFile;
  AccessSettings(GetSettingsFileName, SaveSettings);
  ToolsLogTransactionsToFileItem.Checked := fLogTransactionsToFile;

  if fLogTransactionsToFile then
  begin
    LogString('');
    LogString('*** Transactions are now being logged to both screen and file.');
    LogString('');
  end;
end;

procedure TModLinkDemoMainForm.ModbusConnection1InspectCapturedFrame(
  Sender: TModbusConnection; ServerAddress, CommandCode: Byte;
  const CommandData: TFrameData);

  function CommandCodeToStr(ACommandCode: Byte): string;
  begin
    case ACommandCode of
      $01: Result := 'Read Coils';
      $02: Result := 'Read Discrete Inputs';
      $03: Result := 'Read Holding Registers';
      $04: Result := 'Read Input Registers';
      $05: Result := 'Write Single Coil';
      $06: Result := 'Write Single Register';
      $07: Result := 'Read Exception Status';
      $08: Result := 'Diagnostics';
      $0F: Result := 'Write Multiple Coils';
      $10: Result := 'Write Multiple Registers';
      $11: Result := 'Report Server ID';
      $14: Result := 'Read File Record';
      $15: Result := 'Write File Record';
      $16: Result := 'Mask Write Register';
      $17: Result := 'Read Write Multiple Registers';
    else
      Result := 'Uknown command';
    end;
  end;

  function FrameDataToStr(const AFrameData: TFrameData): string;
  var
    I: Integer;
  begin
    if Length(AFrameData) = 0 then
      Result := '<empty>'
    else
    begin
      Result := '';
      for I := 0 to High(AFrameData) do
        Result := Result + IntToHex(AFrameData[I], 2) + ' ';
    end;
  end;

var
  LCommandName, LCommandData: string;
begin
  LCommandName := CommandCodeToStr(CommandCode);
  LCommandData := FrameDataToStr(CommandData);

  LogString('Inspecting captured frame:');
  LogString(Format('Server address = %d', [ServerAddress]));
  LogString(Format('Command code = $%.2x (%s)', [CommandCode, LCommandName]));
  LogString(Format('Command data = %s', [LCommandData]));
  LogString('');
end;

end.
