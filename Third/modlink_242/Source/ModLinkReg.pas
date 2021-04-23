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

unit ModLinkReg;

{$I ModLink.inc}

interface

//--------------------------------------------------------------------------------------------------

procedure Register;

//--------------------------------------------------------------------------------------------------

implementation

uses
  { Delphi  } SysUtils, Classes,
              {$IFDEF COMPILER_6_UP}
              DesignIntf, DesignEditors,
              {$ELSE}
              DsgnIntf,
              {$ENDIF COMPILER_6_UP}
  { ModLink } ModLink, ModbusConnectionEditor, ModLinkAboutBox;

//--------------------------------------------------------------------------------------------------

{$R ModLink.dcr}

//--------------------------------------------------------------------------------------------------

type

  TModLinkEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TModbusConnectionEditor = class(TModLinkEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TUnsortedEnumProperty = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TSerialPortProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {$IFDEF COMPILER_5}

  TSerialCommCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TTransactionMgmtCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TDiscreteAccessCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TRegisterAccessCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TFileRecordAccessCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  TDiagnosticsCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

  {$ENDIF}

  {$IFDEF COMPILER_5_UP}

  resourcestring
    RsSerialCommCategory = 'Serial Communication';
    RsTransactionMgmtCategory = 'Modbus Transaction Management';
    RsDiscreteAccessCategory = 'Modbus Discrete Access';
    RsRegisterAccessCategory = 'Modbus Register Access';
    RsFileRecordAccessCategory = 'Modbus File Record Access';
    RsDiagnosticsCategory = 'Modbus Diagnostics';

  {$ENDIF COMPILER_5_UP}

//--------------------------------------------------------------------------------------------------

procedure TModLinkEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    ShowModLinkAboutBox;
end;

//--------------------------------------------------------------------------------------------------

function TModLinkEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := '&About ModLink...'
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

function TModLinkEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditor.Edit;
begin
  if GetVerbCount > 1 then ExecuteVerb(1);
end;

//--------------------------------------------------------------------------------------------------

procedure TModbusConnectionEditor.ExecuteVerb(Index: Integer);
const
  EditorCaptionFmt = 'Modbus Connection Editor (%s)';
var
  Connection: TModbusConnection;
begin
  if Index = 1 then
  begin
    Connection := Component as TModbusConnection;
    if EditModbusConnection(Connection, Format(EditorCaptionFmt, [Connection.Name])) then
      Designer.Modified;
  end
  else
    inherited;
end;

//--------------------------------------------------------------------------------------------------

function TModbusConnectionEditor.GetVerb(Index: Integer): string;
begin
  if Index = 1 then
    Result := '&Connection Editor...'
  else
    Result := inherited GetVerb(Index);
end;

//--------------------------------------------------------------------------------------------------

function TModbusConnectionEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

//--------------------------------------------------------------------------------------------------

function TUnsortedEnumProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paSortList];
end;

//--------------------------------------------------------------------------------------------------

function TSerialPortProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList];
end;

//--------------------------------------------------------------------------------------------------

procedure TSerialPortProperty.GetValues(Proc: TGetStrProc);
var
  Temp: TStringList;
  I: Integer;
begin
  Temp := TStringList.Create;
  try
    EnumSerialPorts(Temp);
    for I := 0 to Temp.Count - 1 do
      Proc(Temp[I]);
  finally
    Temp.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF COMPILER_5}

class function TSerialCommCategory.Name: string;
begin
  Result := RsSerialCommCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TSerialCommCategory.Description: string;
begin
  Result := RsSerialCommCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TTransactionMgmtCategory.Name: string;
begin
  Result := RsTransactionMgmtCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TTransactionMgmtCategory.Description: string;
begin
  Result := RsTransactionMgmtCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TDiscreteAccessCategory.Name: string;
begin
  Result := RsDiscreteAccessCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TDiscreteAccessCategory.Description: string;
begin
  Result := RsDiscreteAccessCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TRegisterAccessCategory.Name: string;
begin
  Result := RsRegisterAccessCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TRegisterAccessCategory.Description: string;
begin
  Result := RsRegisterAccessCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TFileRecordAccessCategory.Name: string;
begin
  Result := RsFileRecordAccessCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TFileRecordAccessCategory.Description: string;
begin
  Result := RsFileRecordAccessCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TDiagnosticsCategory.Name: string;
begin
  Result := RsDiagnosticsCategory;
end;

//--------------------------------------------------------------------------------------------------

class function TDiagnosticsCategory.Description: string;
begin
  Result := RsDiagnosticsCategory;
end;

{$ENDIF}

//--------------------------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('ModLink', [TModbusConnection, TModbusClient, TModbusServer]);

  RegisterComponentEditor(TModbusConnection, TModbusConnectionEditor);
  RegisterComponentEditor(TModbusClient, TModLinkEditor);
  RegisterComponentEditor(TModbusServer, TModLinkEditor);

  RegisterPropertyEditor(TypeInfo(TBaudRate), TModbusConnection, 'BaudRate', TUnsortedEnumProperty);
  RegisterPropertyEditor(TypeInfo(TConnectionMode), TModbusConnection, 'ConnectionMode', TUnsortedEnumProperty);
  RegisterPropertyEditor(TypeInfo(TDataBits), TModbusConnection, 'DataBits', TUnsortedEnumProperty);
  RegisterPropertyEditor(TypeInfo(TFlowControl), TModbusConnection, 'FlowControl', TUnsortedEnumProperty);
  RegisterPropertyEditor(TypeInfo(TParityScheme), TModbusConnection, 'Parity', TUnsortedEnumProperty);
  RegisterPropertyEditor(TypeInfo(TSerialPort), TModbusConnection, 'Port', TSerialPortProperty);
  RegisterPropertyEditor(TypeInfo(TStopBits), TModbusConnection, 'StopBits', TUnsortedEnumProperty);
  RegisterPropertyEditor(TypeInfo(TThreadPriority), TModbusConnection, 'ThreadPriority', TUnsortedEnumProperty);
  RegisterPropertyEditor(TypeInfo(TTransmissionMode), TModbusConnection, 'TransmissionMode', TUnsortedEnumProperty);

  {$IFDEF COMPILER_5_UP}

  // TModbusConnection class

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TSerialCommCategory {$ELSE} RsSerialCommCategory {$ENDIF COMPILER_5} ,
    TModbusConnection, [
    'BaudRate',
    'CustomBaudRate',
    'DataBits',
    'DTREnabled',
    'EchoQueryBeforeReply',
    'FlowControl',
    'HookExistingPort',
    'InputBufferSize',
    'OnGetHookedPortHandle',
    'OutputBufferSize',
    'Parity',
    'Port',
    'RTSEnabled',
    'RTSHoldDelay',
    'StopBits']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TTransactionMgmtCategory {$ELSE} RsTransactionMgmtCategory {$ENDIF COMPILER_5} ,
    TModbusConnection, [
    'Active',
    'ConnectionMode',
    'MaxRetries',
    'On*Close',
    'On*Frame*',
    'On*Open',
    'ReceiveTimeout',
    'RefetchDelay',
    'SendTimeout',
    'SilentInterval',
    'ThreadPriority',
    'TransmissionMode',
    'TurnaroundDelay']);

  // TModbusClient class

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TTransactionMgmtCategory {$ELSE} RsTransactionMgmtCategory {$ENDIF COMPILER_5} ,
    TModbusClient, [
    'MaxConsecutiveTimeouts',
    'OnConsecutiveTimeoutLimitExceed',
    'OnCustomTransactionComplete',
    'OnTransactionProcessed',
    'ServerAddress']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TLocalizableCategory {$ELSE} SLocalizableCategoryName {$ENDIF COMPILER_5} ,
    TModbusClient, [
    'OnTranslateExceptionCode']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TDiscreteAccessCategory {$ELSE} RsDiscreteAccessCategory {$ENDIF COMPILER_5} ,
    TModbusClient, [
    'OnCoilsRead',
    'OnDiscreteInputsRead',
    'OnMultipleCoilsWrite',
    'OnSingleCoilWrite']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TRegisterAccessCategory {$ELSE} RsRegisterAccessCategory {$ENDIF COMPILER_5} ,
    TModbusClient, [
    'On*RegistersRead',
    'OnMultipleRegistersReadWrite',
    'OnMultipleRegistersWrite',
    'OnSingleRegisterMaskWrite',
    'OnSingleRegisterWrite']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TFileRecordAccessCategory {$ELSE} RsFileRecordAccessCategory {$ENDIF COMPILER_5} ,
    TModbusClient, [
    'OnFileRecord*']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TDiagnosticsCategory {$ELSE} RsDiagnosticsCategory {$ENDIF COMPILER_5} ,
    TModbusClient, [
    'OnDiagnostics',
    'OnExceptionStatusRead',
    'OnServerIdentificationReport']);
    
  // TModbusServer class

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TTransactionMgmtCategory {$ELSE} RsTransactionMgmtCategory {$ENDIF COMPILER_5} ,
    TModbusServer, [
    'Address',
    'OnAcceptCommand',
    'OnCommandHandlerException']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TDiscreteAccessCategory {$ELSE} RsDiscreteAccessCategory {$ENDIF COMPILER_5} ,
    TModbusServer, [
    'OnCanReadCoil',
    'OnCanReadDiscreteInput',
    'OnCanWriteCoil',
    'OnGetCoilValue',
    'OnGetDiscreteInputValue',
    'OnSetCoilValue']);

  RegisterPropertiesInCategory(
    {$IFDEF COMPILER_5} TRegisterAccessCategory {$ELSE} RsRegisterAccessCategory {$ENDIF COMPILER_5} ,
    TModbusServer, [
    'OnCanRead*Register',
    'OnCanWriteHoldingRegister',
    'OnGet*RegisterValue',
    'OnSetHoldingRegisterValue']);

  {$ENDIF COMPILER_5_UP}

end;

//--------------------------------------------------------------------------------------------------

end.
