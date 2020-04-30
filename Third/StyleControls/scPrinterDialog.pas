{*******************************************************************}
{                                                                   }
{       Almediadev Visual Component Library                         }
{       StyleControls                                               }
{       Version 4.62                                                }
{                                                                   }
{       Copyright (c) 2014-2020 Almediadev                          }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{       Home:  http://www.almdev.com                                }
{       Support: support@almdev.com                                 }
{                                                                   }
{*******************************************************************}

unit scPrinterDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, scControls, Vcl.StdCtrls,
  scStyledForm, Vcl.Mask, Printers, WinSpool, scDrawUtils, Vcl.ExtCtrls,
  scImageCollection, scExtControls, scAdvancedPager, scDialogs;

type
  TscPaperInfo = class
  private
    FDMPaper: Integer;
    FName: string;
    FSize: TPoint;
    function GetSize(Index: Integer): Integer;
    procedure SetSize(Index: Integer; Value: Integer);
  public
    procedure Assign(Source: TscPaperInfo);
    function IsEqual(Source: TscPaperInfo): Boolean;
    property DMPaper: Integer read FDMPaper;
    property Height: Integer index 1 read GetSize write SetSize;
    property Name: string read FName;
    property Size: TPoint read FSize;
    property Width: Integer index 0 read GetSize write SetSize;
  end;

  TscPageMeasureUnits = (scpmMillimeters, scpmInches);

  TscPrintDlgFrm = class(TForm)
    OKButton: TscButton;
    CancelButton: TscButton;
    scImageCollection1: TscImageCollection;
    scStyledForm1: TscStyledForm;
    Pager: TscAdvancedPager;
    scAdvancedPagerPage1: TscAdvancedPagerPage;
    scAdvancedPagerPage2: TscAdvancedPagerPage;
    PrinterGroupBox: TscGroupBox;
    PrinterBox: TscComboBox;
    PropertyButton: TscButton;
    L1: TscLabel;
    L2: TscLabel;
    L3: TscLabel;
    PrintToFileCheckBox: TscCheckBox;
    CopyCountGroupBox: TscGroupBox;
    CollectPreviewPanel: TscPanel;
    CopiesSpinEdit: TscSpinEdit;
    CollateCheckBox: TscCheckBox;
    PageCountGroupBox: TscGroupBox;
    AllRadioButton: TscRadioButton;
    PagerRadioButton: TscRadioButton;
    PageFromSpinEdit: TscSpinEdit;
    scLabel1: TscLabel;
    PageToSpinEdit: TscSpinEdit;
    PagePreview: TscPanel;
    PageSizeLabel: TscLabel;
    PageSourceLabel: TscLabel;
    OrienationLabel: TscLabel;
    MarginsGroupBox: TscGroupBox;
    LeftMEdit: TscSpinEdit;
    TopMEdit: TscSpinEdit;
    RightMEdit: TscSpinEdit;
    BottomMEdit: TscSpinEdit;
    PaperSizeBox: TscComboBox;
    PaperSourceBox: TscComboBox;
    OrientationBox: TscComboBox;
    procedure FormCreate(Sender: TObject);
    procedure PrinterBoxClick(Sender: TObject);
    procedure PagerRadioButtonClick(Sender: TObject);
    procedure PageToSpinEditEnter(Sender: TObject);
    procedure PageFromSpinEditEnter(Sender: TObject);
    procedure PropertyButtonClick(Sender: TObject);
    procedure CollateCheckBoxClick(Sender: TObject);
    procedure CopiesSpinEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaperSizeBoxClick(Sender: TObject);
    procedure PaperSourceBoxClick(Sender: TObject);
    procedure OrientationBoxClick(Sender: TObject);
    procedure TopMEditChange(Sender: TObject);
    procedure RightMEditChange(Sender: TObject);
    procedure LeftMEditChange(Sender: TObject);
    procedure BottomMEditChange(Sender: TObject);
    procedure PagePreviewPanelPaint(ACanvas: TCanvas; ARect: TRect);
    procedure scStyledForm1ChangeScale(AScaleFactor: Double);
  protected
    StopCheck: Boolean;
    Bins, Papers: TStrings;
    procedure ClearPapersAndBins;
    procedure LoadPapersAndBins;
    procedure LoadCurrentPaperAndBin;
    procedure SaveCurrentPaperAndBin;
  public
    FUnits: TscPageMeasureUnits;
    LeftMargin, RightMargin,
    TopMargin, BottomMargin: Integer;
    PageWidth, PageHeight: Integer;
  end;

  TscPrintDialogOption = (
    scpsoDisablePrinter,
    scpsoDisablePrintToFile,
    scpsoDisablePagesCount,
    scpsoDisableCopiesCount,
    scpsoDisablePaperPage,
    scpsoDisablePaperSize,
    scpsoDisablePaperSource,
    scpsoDisableOrientation,
    scpsoDisableMargins);
  TscPageSetupDialogOptions = set of TscPrintDialogOption;

  TscPrintRange = (scprAllPages, scprPageNums);

  TscPrintDialog = class(TComponent)
  private
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FTitle: String;
    FDlgFrm: TscPrintDlgFrm;
    FScaled: Boolean;
    FAnimationOnControls: Boolean;
    FUnits: TscPageMeasureUnits;
    FOptions: TscPageSetupDialogOptions;

    FMinMarginLeft: Integer;
    FMinMarginTop: Integer;
    FMinMarginRight: Integer;
    FMinMarginBottom: Integer;

    FMaxMarginLeft: Integer;
    FMaxMarginTop: Integer;
    FMaxMarginRight: Integer;
    FMaxMarginBottom: Integer;

    FMarginLeft: Integer;
    FMarginTop: Integer;
    FMarginRight: Integer;
    FMarginBottom: Integer;
    FPageWidth, FPageHeight: Integer;

    FPrintRange: TscPrintRange;
    FPrintToFile: Boolean;
    FCollate: Boolean;

    FFromPage: Integer;
    FToPage: Integer;
    FMinPage: Integer;
    FMaxPage: Integer;
    FCopies: Integer;
    FPrintToFileText: String;
    procedure SetCopies(Value: Integer);
  protected
    procedure SetAnimation;
    procedure Change;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;

    property PageWidth: Integer read FPageWidth write FPageWidth;
    property PageHeight: Integer read FPageHeight write FPageHeight;
    property Collate: Boolean read FCollate write FCollate default False;

  published
    property MinMarginLeft: Integer read FMinMarginLeft write FMinMarginLeft;
    property MinMarginTop: Integer read FMinMarginTop write FMinMarginTop;
    property MinMarginRight: Integer read FMinMarginRight write FMinMarginRight;
    property MinMarginBottom: Integer read FMinMarginBottom write FMinMarginBottom;

    property MaxMarginLeft: Integer read FMaxMarginLeft write FMaxMarginLeft;
    property MaxMarginTop: Integer read FMaxMarginTop write FMaxMarginTop;
    property MaxMarginRight: Integer read FMaxMarginRight write FMaxMarginRight;
    property MaxMarginBottom: Integer read FMaxMarginBottom write FMaxMarginBottom;

    property MarginLeft: Integer read FMarginLeft write FMarginLeft;
    property MarginTop: Integer read FMarginTop write FMarginTop;
    property MarginRight: Integer read FMarginRight write FMarginRight;
    property MarginBottom: Integer read FMarginBottom write FMarginBottom;

    property Copies: Integer read FCopies write SetCopies default 1;
    property FromPage: Integer read FFromPage write FFromPage default 1;
    property ToPage: Integer read FToPage write FToPage default 1;
    property MinPage: Integer read FMinPage write FMinPage default 1;
    property MaxPage: Integer read FMaxPage write FMaxPage default 1;

    property PrintToFileText: String
      read FPrintToFileText write FPrintToFileText;
    property PrintRange: TscPrintRange
      read FPrintRange write FPrintRange default scprAllPages;
    property PrintToFile: Boolean
      read FPrintToFile write FPrintToFile default False;
    property Options: TscPageSetupDialogOptions read FOptions write FOptions
      default [];
    property Units: TscPageMeasureUnits
      read FUnits write FUnits default scpmMillimeters;
    property Title: String read FTitle write FTitle;
    property Scaled: Boolean read FScaled write FScaled;
    property AnimationOnControls: Boolean
      read FAnimationOnControls write FAnimationOnControls;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
  Uses System.UITypes, scDlgStrs;

{$R *.dfm}
{$R *.res}

var
  SC_S_PRNSTATUS_Paused: String = 'Paused';
  SC_S_PRNSTATUS_PendingDeletion: String = 'Pending Deletion';
  SC_S_PRNSTATUS_Busy: String = 'Busy';
  SC_S_PRNSTATUS_DoorOpen: String = 'Door Open';
  SC_S_PRNSTATUS_Error: String = 'Error';
  SC_S_PRNSTATUS_Initializing: String = 'Initializing';
  SC_S_PRNSTATUS_IOActive: String = 'IO Active';
  SC_S_PRNSTATUS_ManualFeed: String = 'Manual Feed';
  SC_S_PRNSTATUS_NoToner: String = 'No Toner';
  SC_S_PRNSTATUS_NotAvailable: String = 'Not Available';
  SC_S_PRNSTATUS_OFFLine: String = 'Offline';
  SC_S_PRNSTATUS_OutOfMemory: String = 'Out of Memory';
  SC_S_PRNSTATUS_OutBinFull: String = 'Output Bin Full';
  SC_S_PRNSTATUS_PagePunt: String = 'Page Punt';
  SC_S_PRNSTATUS_PaperJam: String = 'Paper Jam';
  SC_S_PRNSTATUS_PaperOut: String = 'Paper Out';
  SC_S_PRNSTATUS_PaperProblem: String = 'Paper Problem';
  SC_S_PRNSTATUS_Printing: String = 'Printing';
  SC_S_PRNSTATUS_Processing: String = 'Processing';
  SC_S_PRNSTATUS_TonerLow: String = 'Toner Low';
  SC_S_PRNSTATUS_UserIntervention: String = 'User Intervention';
  SC_S_PRNSTATUS_Waiting: String = 'Waiting';
  SC_S_PRNSTATUS_WarningUp: String = 'Warming Up';
  SC_S_PRNSTATUS_Ready: String = 'Ready';
  SC_S_PRNSTATUS_PrintingAndWaiting: String = 'Printing: %d document(s) waiting';

  SC_S_PRN_Status: String = 'Status';
  SC_S_PRN_Type: String = 'Type';
  SC_S_PRN_Where: String = 'Where';
  SC_S_PRN_Comment: String = 'Comment';

  SC_S_PRN_Print: String = 'Print';
  SC_S_PRN_Priner: String = 'Printer';

  SC_S_PRN_Settings: String = 'Settings';
  SC_S_PRN_PageSettings: String = 'Page Setup';
  SC_S_PRN_Properties: String = 'Properties';
  SC_S_PRN_PrintToFile: String = 'Print to file';
  SC_S_PRN_PageCount: String = 'Count of pages';
  SC_S_PRN_CopyCount: String = 'Count of copies';
  SC_S_PRN_Collate: String = 'Collate';
  SC_S_PRN_PageSize: String = 'Page Size';
  SC_S_PRN_PageSource: String = 'Page Source';
  SC_S_PRN_AllPages: String = 'All';
  SC_S_PRN_Orientation: String = 'Orientation';
  SC_S_PRN_Portrait: String = 'Portrait';
  SC_S_PRN_Landscape: String = 'Landscape';

  SC_S_PRN_MarginsMM: String = 'Margins (millimeters)';
  SC_S_PRN_MarginsIn: String = 'Margins (inches)';

  SC_S_PRN_Warning: String = 'There are no printers in your system!';

function InchToMM(Inches: Double): Double;
begin
  Result := Inches * 25.4;
end;

function MMtoInch(MM: Double): Double;
begin
  Result := MM / 25.4;
end;

function MMtoInch2(MM: Double): Double;
begin
  Result := MM / 10;
  Result := Round(MMToInch(Result) * 1000) div 10;
end;

procedure GetPaperSizeInMM(var X, Y: Integer; PageWidth, PageHeight: Integer);
begin
  X := PageWidth * 10;
  Y := PageHeight * 10;
end;

procedure GetPaperSizeInInches(var X, Y: Integer; PageWidth, PageHeight: Integer);
begin
  GetPaperSizeInMM(X, Y, PageWidth, PageHeight);
  X := X div 10;
  Y := Y div 10;
  X := Round(MMToInch(X) * 1000) div 10;
  Y := Round(MMToInch(Y) * 1000) div 10;
end;

function GetStatusString(Status: DWORD): string;
begin
  case Status of
    0:
      Result := SC_S_PRNSTATUS_Ready;
    PRINTER_STATUS_PAUSED:
      Result := SC_S_PRNSTATUS_Paused;
    PRINTER_STATUS_PENDING_DELETION:
      Result := SC_S_PRNSTATUS_PendingDeletion;
    PRINTER_STATUS_BUSY:
      Result := SC_S_PRNSTATUS_Busy;
    PRINTER_STATUS_DOOR_OPEN:
      Result := SC_S_PRNSTATUS_DoorOpen;
    PRINTER_STATUS_ERROR:
      Result := SC_S_PRNSTATUS_Error;
    PRINTER_STATUS_INITIALIZING:
      Result := SC_S_PRNSTATUS_Initializing;
    PRINTER_STATUS_IO_ACTIVE:
      Result := SC_S_PRNSTATUS_IOActive;
    PRINTER_STATUS_MANUAL_FEED:
      Result := SC_S_PRNSTATUS_ManualFeed;
    PRINTER_STATUS_NO_TONER:
      Result := SC_S_PRNSTATUS_NoToner;
    PRINTER_STATUS_NOT_AVAILABLE:
      Result := SC_S_PRNSTATUS_NotAvailable;
    PRINTER_STATUS_OFFLINE:
      Result := SC_S_PRNSTATUS_OFFLine;
    PRINTER_STATUS_OUT_OF_MEMORY:
      Result := SC_S_PRNSTATUS_OutOfMemory;
    PRINTER_STATUS_OUTPUT_BIN_FULL:
      Result := SC_S_PRNSTATUS_OutBinFull;
    PRINTER_STATUS_PAGE_PUNT:
      Result := SC_S_PRNSTATUS_PagePunt;
    PRINTER_STATUS_PAPER_JAM:
      Result := SC_S_PRNSTATUS_PaperJam;
    PRINTER_STATUS_PAPER_OUT:
      Result := SC_S_PRNSTATUS_PaperOut;
    PRINTER_STATUS_PAPER_PROBLEM:
      Result := SC_S_PRNSTATUS_PaperProblem;
    PRINTER_STATUS_PRINTING:
      Result := SC_S_PRNSTATUS_Printing;
    PRINTER_STATUS_PROCESSING:
      Result := SC_S_PRNSTATUS_Processing;
    PRINTER_STATUS_TONER_LOW:
      Result := SC_S_PRNSTATUS_TonerLow;
    PRINTER_STATUS_USER_INTERVENTION:
      Result := SC_S_PRNSTATUS_UserIntervention;
    PRINTER_STATUS_WAITING:
      Result := SC_S_PRNSTATUS_Waiting;
    PRINTER_STATUS_WARMING_UP:
      Result := SC_S_PRNSTATUS_WarningUp;
  else
    Result := '';
  end;
end;

procedure CallDocumentPropertiesDialog(H: HWND);
var
  PPrinterDevMode: PDevMode;
  DevModeHandle: THandle;
  hPrinter: THandle;
  PrinterName, Driver, Port: array[0..79] of Char;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
  if not OpenPrinter(PrinterName, hPrinter, nil)
  then
    raise EPrinter.Create(SysErrorMessage(GetLastError ));
  PPrinterDevMode := GlobalLock(DevModeHandle);
  DocumentProperties(H, hPrinter, PrinterName, PPrinterDevMode^, PPrinterDevMode^,
    DM_OUT_BUFFER or DM_IN_BUFFER or DM_IN_PROMPT);
  GlobalUnlock(DevModeHandle);
  ClosePrinter(hPrinter);
end;

procedure SetCollate(Value: Boolean);
var
  PPrinterDevMode: PDevMode;
  DevModeHandle: THandle;
  hPrinter: THandle;
  PrinterName, Driver, Port: array[0..79] of Char;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
  if not OpenPrinter(PrinterName, hPrinter, nil)
  then
    raise EPrinter.Create(SysErrorMessage(GetLastError ));
  PPrinterDevMode := GlobalLock(DevModeHandle);
  if Value
  then PPrinterDevMode^.dmCollate := 1
  else PPrinterDevMode^.dmCollate := 0;
  DocumentProperties(0, hPrinter, PrinterName, PPrinterDevMode^, PPrinterDevMode^,
    DM_OUT_BUFFER or DM_IN_BUFFER);
  GlobalUnlock(DevModeHandle);
  ClosePrinter(hPrinter);
end;

function GetCollate: Boolean;
var
  PPrinterDevMode: PDevMode;
  DevModeHandle: THandle;
  hPrinter: THandle;
  PrinterName, Driver, Port: array[0..79] of Char;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
  if not OpenPrinter(PrinterName, hPrinter, nil)
  then
    raise EPrinter.Create(SysErrorMessage(GetLastError ));
  PPrinterDevMode := GlobalLock(DevModeHandle);
  DocumentProperties(0, hPrinter, PrinterName, PPrinterDevMode^, PPrinterDevMode^, DM_OUT_BUFFER or DM_IN_BUFFER);
  Result := PPrinterDevMode^.dmCollate > 0;
  GlobalUnlock(DevModeHandle);
  ClosePrinter(hPrinter);
end;

procedure RestoreDocumentProperties;
var
  PPrinterDevMode: PDevMode;
  DevModeHandle: THandle;
  hPrinter: THandle;
  PrinterName, Driver, Port: array[0..79] of Char;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
  if not OpenPrinter(PrinterName, hPrinter, nil)
  then
    raise EPrinter.Create(SysErrorMessage(GetLastError ));
  PPrinterDevMode := GlobalLock(DevModeHandle);
  DocumentProperties(0, hPrinter, PrinterName, PPrinterDevMode^, PPrinterDevMode^, DM_OUT_BUFFER);
  GlobalUnlock(DevModeHandle);
  ClosePrinter(hPrinter);
end;

procedure GetPrinterInfo(var AStatus, AType, APort: String);
var
  Flags, ACount, NumInfo: DWORD;
  Buffer, PInfo: PAnsiChar;
  PrinterName, Driver, Port: array[0..79] of Char;
  DevModeHandle: THandle;
  I: Integer;
  S1, S2: String;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);

  Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
  ACount := 0;
  EnumPrinters(Flags, nil, 2, nil, 0, ACount, NumInfo);
  if ACount = 0 then Exit;
  GetMem(Buffer, ACount);
  if not EnumPrinters(Flags, nil, 2, PByte(Buffer), ACount, ACount, NumInfo)
  then
    begin
      FreeMem(Buffer, ACount);
      Exit;
    end;

  PInfo := Buffer;

  S1 := PrinterName;
  for i := 0 to NumInfo - 1 do
  begin
    S2 := PPrinterInfo2(PInfo)^.pPrinterName;
    if S1 = S2
    then
      Break
    else
      Inc(PInfo, Sizeof(TPrinterInfo2));
  end;

  AStatus := GetStatusString(PPrinterInfo2(PInfo)^.Status);
  AType := PPrinterInfo2(PInfo)^.pDriverName;
  APort := PPrinterInfo2(PInfo)^.pPortName;

  FreeMem(Buffer, ACount);
end;


procedure GetPapers(APapers: TStrings);
const
  bsPaperNameLength = 64;
  bsPaperValueLength = SizeOf(Word);
  bsPaperSizeLength = SizeOf(TPoint);
type
  TscPaperSize = TPoint;
  TscPaperSizes = array[0..0] of TscPaperSize;
  PscPaperSizes = ^TscPaperSizes;
  TscPaperValue = Word;
  TscPaperValues = array [0..0] of TscPaperValue;
  PscPaperValues = ^TscPaperValues;
  TscPaperName = array[0..bsPaperNameLength - 1] of char;
  TscPaperNames = array [0..0] of TscPaperName;
  PscPaperNames = ^TscPaperNames;
var
  APaperNames: PscPaperNames;
  APaperValues: PscPaperValues;
  APaperSizes: PscPaperSizes;
  ACount: Integer;
  I: Integer;
  APaper: TscPaperInfo;
  ACapability: UINT;
  PrinterName, Driver, Port: array[0..79] of Char;
  DevModeHandle: THandle;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
  if APapers <> nil then
  try
    APapers.Clear;
    ACapability := DC_PAPERNAMES;
    ACount := WinSpool.DeviceCapabilities(PrinterName, Port, ACapability, nil, nil);
    if ACount > 0 then
      begin
        GetMem(APaperNames, ACount * Sizeof( TscPapername ));
        try
          if WinSpool.DeviceCapabilities(PrinterName, Port, ACapability, PChar(APaperNames), nil) <> -1 then
          begin
            ACapability := DC_PAPERS;
            GetMem(APaperValues, ACount * Sizeof( TscPaperValue ));
           try
              if WinSpool.DeviceCapabilities(PrinterName, Port, ACapability, PChar(APaperValues), nil) <> -1 then
              begin
                ACapability := DC_PAPERSIZE;
                GetMem(APaperSizes, bsPaperSizeLength * ACount);
                try
                  if WinSpool.DeviceCapabilities(PrinterName, Port, ACapability, PChar(APaperSizes), nil) <> -1 then
                  begin
                    for I := 0 to ACount - 1 do
                    begin
                      APaper := TscPaperInfo.Create;
                      with APaper do
                      begin
                        FSize := APaperSizes^[I];
                        FDMPaper :=  APaperValues^[I];
                        FName := APaperNames^[I];
                      end;
                      if APaper.Name <> '' then
                        APapers.AddObject(APaper.Name, APaper)
                      else
                        APaper.Free;
                    end;
                  end;
                finally
                  FreeMem(APaperSizes, bsPaperSizeLength * ACount);
                end;
              end;
            finally
              FreeMem(APaperValues, bsPaperValueLength * ACount);
            end;
          end;
        finally
          FreeMem(APaperNames, bsPaperNameLength * ACount);
        end;
    end;
  except
    raise;
  end;
end;

procedure GetBins(sl: TStrings);
 type
   TBinName      = array [0..23] of Char;
   TBinNameArray = array [0..0] of TBinName;
   PBinnameArray = ^TBinNameArray;
   TBinArray     = array [0..0] of Word;
   PBinArray     = ^TBinArray;
 var
   Device, Driver, Port: array [0..255] of Char;
   hDevMode: THandle;
   i, numBinNames, numBins, temp: Integer;
   pBinNames: PBinnameArray;
   pBins: PBinArray;
 begin
   Printer.GetPrinter(Device, Driver, Port, hDevmode);
   numBinNames := WinSpool.DeviceCapabilities(Device, Port, DC_BINNAMES, nil, nil);
   numBins     := WinSpool.DeviceCapabilities(Device, Port, DC_BINS, nil, nil);
   if numBins <> numBinNames then
   begin
     raise Exception.Create('DeviceCapabilities reports different number of bins and bin names!');
   end;
   if numBinNames > 0 then
   begin
     GetMem(pBinNames, numBinNames * SizeOf(TBinname));
     GetMem(pBins, numBins * SizeOf(Word));
     try
       WinSpool.DeviceCapabilities(Device, Port, DC_BINNAMES, PChar(pBinNames), nil);
       WinSpool.DeviceCapabilities(Device, Port, DC_BINS, PChar(pBins), nil);
       sl.Clear;
       for i := 0 to numBinNames - 1 do
       if pBinNames^[i] <> '' then
       begin
         temp := pBins^[i];
         sl.addObject(pBinNames^[i], TObject(temp));
       end;
     finally
       FreeMem(pBinNames);
       if pBins <> nil then
         FreeMem(pBins);
     end;
   end;
 end;


function TscPaperInfo.IsEqual(Source: TscPaperInfo): Boolean;
begin
  Result := (DMPaper = Source.DMPaper) and (FName = Source.Name) and
    EqPoints(Size, Source.Size);
end;

procedure TscPaperInfo.Assign(Source: TscPaperInfo);
begin
  FDMPaper := Source.FDMPaper;
  FName := Source.FName;
  FSize := Source.FSize;
end;

function TscPaperInfo.GetSize(Index: Integer): Integer;
begin
  if Index = 0
  then
    Result := FSize.X
  else
    Result := FSize.Y;
end;

procedure TscPaperInfo.SetSize(Index: Integer; Value: Integer);
begin
  if DMPaper < DMPAPER_USER then Exit;
  if Index = 0
  then
    FSize.X := Value
  else
    FSize.Y := Value;
end;

procedure TscPrintDlgFrm.CollateCheckBoxClick(Sender: TObject);
begin
  if not StopCheck then
    SetCollate(CollateCheckBox.Checked);
  if CollateCheckBox.Checked then
    CollectPreviewPanel.WallpaperIndex := 1
  else
    CollectPreviewPanel.WallpaperIndex := 0;
end;

procedure TscPrintDlgFrm.CopiesSpinEditChange(Sender: TObject);
begin
  Printer.Copies := Round(CopiesSpinEdit.Value);
  CollateCheckBox.Enabled := CopiesSpinEdit.Value > 1;
end;

procedure TscPrintDlgFrm.FormCreate(Sender: TObject);
var
  Item: TscImageCollectionItem;
begin
  if SC_RTLMODE then
    BidiMode := bdRightToLeft;

  Item := scImageCollection1.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SC_NOCOLLATE');
  Item.DrawStyle := idsCenter;
  Item.CheckBitmapOptions;
  Item := scImageCollection1.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SC_COLLATE');
  Item.DrawStyle := idsCenter;
  Item.CheckBitmapOptions;
  Item := scImageCollection1.Images.Add;
  Item.LoadPngFromResourceName(HInstance, 'SC_PAPERSHADOW');
  Item.BitmapOptions.LeftMargin := 10;
  Item.BitmapOptions.TopMargin := 10;
  Item.BitmapOptions.BottomMargin := 10;
  Item.BitmapOptions.RightMargin := 10;
  Item.BitmapOptions.StretchBorder := True;
  Item.BitmapOptions.DrawOnlyBorder := True;
  Item.BitmapOptions.Stretch := True;
  Item.CheckBitmapOptions;

  LeftMargin := 0;
  TopMargin := 0;
  BottomMargin := 0;
  RightMargin := 0;
  PageWidth := 0;
  PageHeight := 0;

  Bins := TStringList.Create;
  Papers := TStringList.Create;

  // Captions set
  Self.Caption := SC_S_PRN_Print;
  Pager.Tabs[0].Caption := SC_S_PRN_Settings;
  Pager.Tabs[1].Caption := SC_S_PRN_PageSettings;
  Self.PrinterGroupBox.Caption := SC_S_PRN_Priner;
  Self.PropertyButton.Caption := SC_S_PRN_Properties + '...';
  Self.PrintToFileCheckBox.Caption := SC_S_PRN_PrintToFile;
  Self.PageCountGroupBox.Caption := SC_S_PRN_PageCount;
  Self.CopyCountGroupBox.Caption := SC_S_PRN_CopyCount;
  Self.CollateCheckBox.Caption := SC_S_PRN_Collate;
  Self.PageSizeLabel.Caption := SC_S_PRN_PageSize + ':';
  Self.PageSourceLabel.Caption := SC_S_PRN_PageSource + ':';
  Self.AllRadioButton.Caption := SC_S_PRN_AllPages;
  Self.OrienationLabel.Caption := SC_S_PRN_Orientation + ':';
  Self.OrientationBox.Items[0] := SC_S_PRN_Portrait;
  Self.OrientationBox.Items[1] := SC_S_PRN_Landscape;
  Self.MarginsGroupBox.Caption := SC_S_PRN_MarginsMM;

  Self.OkButton.Caption := SC_S_Msg_OK;
  Self.CancelButton.Caption := SC_S_Msg_Cancel;

  PrinterBox.Items.Assign(Printer.Printers);
  PrinterBox.ItemIndex := Printer.PrinterIndex;
  PrinterBoxClick(Self);
end;

procedure TscPrintDlgFrm.FormDestroy(Sender: TObject);
begin
  ClearPapersAndBins;
  Papers.Free;
  Bins.Free;
end;

procedure TscPrintDlgFrm.BottomMEditChange(Sender: TObject);
begin
  if FUnits = scpmMillimeters
  then
    BottomMargin := Round(BottomMEdit.Value * 100)
  else
    BottomMargin := Round(InchToMM(BottomMEdit.Value) * 100);
  PagePreview.RePaintControl;
end;

procedure TscPrintDlgFrm.ClearPapersAndBins;
var
  I: Integer;
begin
  if Papers.Count = 0 then Exit;
  for I := 0 to Papers.Count - 1 do
    TscPaperInfo(Papers.Objects[I]).Free;
  Papers.Clear;
  Bins.Clear;
end;

procedure TscPrintDlgFrm.LeftMEditChange(Sender: TObject);
begin
  if FUnits = scpmMillimeters
  then
    LeftMargin := Round(LeftMEdit.Value * 100)
  else
    LeftMargin := Round(InchToMM(LeftMEdit.Value) * 100);
  PagePreview.RePaintControl;
end;

procedure TscPrintDlgFrm.LoadCurrentPaperAndBin;
var
  PPrinterDevMode: PDevMode;
  DevModeHandle: THandle;
  hPrinter: THandle;
  PrinterName, Driver, Port: array[0..79] of Char;
  dm_Size: Integer;
  dm_Source: Integer;
  I, J: Integer;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
  if not OpenPrinter(PrinterName, hPrinter, nil)
  then
    raise EPrinter.Create(SysErrorMessage(GetLastError ));
  PPrinterDevMode := GlobalLock(DevModeHandle);
  DocumentProperties(0, hPrinter, PrinterName, PPrinterDevMode^, PPrinterDevMode^, DM_OUT_BUFFER or DM_IN_BUFFER);
  dm_Size := PPrinterDevMode^.dmPaperSize;
  dm_Source := PPrinterDevMode^.dmDefaultSource;
  GlobalUnlock(DevModeHandle);
  ClosePrinter(hPrinter);
  J := 0;
  for I := 0 to PaperSizeBox.Items.Count - 1 do
  begin
    if TscPaperInfo(Papers.Objects[I]).DMPaper = dm_Size then
    begin
      J := I;
      Break;
    end;
  end;
  PaperSizeBox.ItemIndex := J;
  J := 0;
  for I := 0 to PaperSourceBox.Items.Count - 1 do
  begin
    if Integer(Bins.Objects[I]) = dm_Source then
    begin
      J := I;
      Break;
    end;
  end;
  PaperSourceBox.ItemIndex := J;
  if I <> -1
  then
    begin
      GetPaperSizeInMM(PageWidth, PageHeight,
                       TscPaperInfo(Papers.Objects[I]).Size.X,
                       TscPaperInfo(Papers.Objects[I]).Size.Y);

      if FUnits = scpmMillimeters
      then
        begin
          LeftMargin := Round(LeftMEdit.Value * 100);
          TopMargin := Round(TopMEdit.Value * 100);
          RightMargin := Round(RightMEdit.Value * 100);
          BottomMargin := Round(BottomMEdit.Value * 100);
        end
      else
        begin
          LeftMargin := Round(InchToMM(LeftMEdit.Value) * 100);
          TopMargin := Round(InchToMM(TopMEdit.Value) * 100);
          RightMargin := Round(InchToMM(RightMEdit.Value) * 100);
          BottomMargin := Round(InchToMM(BottomMEdit.Value) * 100);
        end;

      PagePreview.RepaintControl;
    end;
end;

procedure TscPrintDlgFrm.SaveCurrentPaperAndBin;
var
  PPrinterDevMode: PDevMode;
  DevModeHandle: THandle;
  hPrinter: THandle;
  PrinterName, Driver, Port: array[0..79] of Char;
  I: Integer;
begin
  Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
  if not OpenPrinter(PrinterName, hPrinter, nil)
  then
    raise EPrinter.Create(SysErrorMessage(GetLastError ));
  PPrinterDevMode := GlobalLock(DevModeHandle);
  I := PaperSizeBox.ItemIndex;
  if I <> -1 then
    PPrinterDevMode^.dmPaperSize := TscPaperInfo(Papers.Objects[I]).DMPaper;
  I := PaperSourceBox.ItemIndex;
  if I <> -1 then
    PPrinterDevMode^.dmDefaultSource := Integer(Bins.Objects[I]);
  DocumentProperties(0, hPrinter, PrinterName, PPrinterDevMode^, PPrinterDevMode^, DM_OUT_BUFFER or DM_IN_BUFFER);
  Printer.SetPrinter(PrinterName, Driver, Port, DevModeHandle);
  GlobalUnlock(DevModeHandle);
  ClosePrinter(hPrinter);
end;

procedure TscPrintDlgFrm.scStyledForm1ChangeScale(AScaleFactor: Double);
begin
  scImageCollection1.Images[0].LoadPngFromResourceName(HInstance, 'SC_NOCOLLATE', AScaleFactor);
  scImageCollection1.Images[1].LoadPngFromResourceName(HInstance, 'SC_COLLATE', AScaleFactor);
end;

procedure TscPrintDlgFrm.LoadPapersAndBins;
begin
  ClearPapersAndBins;
  GetPapers(Papers);
  GetBins(Bins);
  StopCheck := True;
  PaperSizeBox.Items.Assign(Papers);
  PaperSourceBox.Items.Assign(Bins);
  LoadCurrentPaperAndBin;
  StopCheck := False;
end;

procedure TscPrintDlgFrm.TopMEditChange(Sender: TObject);
begin
  if FUnits = scpmMillimeters
  then
    TopMargin := Round(TopMEdit.Value * 100)
  else
    TopMargin := Round(InchToMM(TopMEdit.Value) * 100);
  PagePreview.RePaintControl;
end;

procedure TscPrintDlgFrm.OrientationBoxClick(Sender: TObject);
begin
  if OrientationBox.ItemIndex > 0 then
    Printer.Orientation := poLandscape
  else
    Printer.Orientation := poPortrait;
  PagePreview.RePaintControl;
end;

procedure TscPrintDlgFrm.PageFromSpinEditEnter(Sender: TObject);
begin
  PagerRadioButton.Checked := True;
end;

procedure TscPrintDlgFrm.PagePreviewPanelPaint(ACanvas: TCanvas; ARect: TRect);
var
  TempPageWidth,  TempPageHeight,
  TempLeftMargin, TempTopMargin,
  TempRightMargin, TempBottomMargin: Integer;
  PR, TR, SR: TRect;
  kf: Double;
begin
  if (PageWidth = 0) or (PageHeight = 0) then Exit;
  InflateRect(ARect, -10, -10);
  if Printer.Orientation = poPortrait
  then
    begin
      kf := ARect.Width / PageHeight;
      TempPageWidth  := Round(PageWidth * kf);
      TempPageheight  := Round(PageHeight * kf);
      TempLeftMargin := Round(LeftMargin * kf);
      TempTopMargin := Round(TopMargin * kf);
      TempRightMargin := Round(RightMargin * kf);
      TempBottomMargin := Round(BottomMargin * kf);
      PR := Rect(ARect.Left + ARect.Width div 2 - TempPageWidth div 2,
                 ARect.Top + ARect.Height div 2 - TempPageHeight div 2,
                 ARect.Left + ARect.Width div 2 - TempPageWidth div 2 + TempPageWidth,
                 ARect.Top +  ARect.Height div 2 - TempPageHeight div 2 + TempPageHeight);
    end
  else
    begin
      kf := ARect.Width / PageHeight;
      TempPageWidth := Round(PageHeight * kf);
      TempPageHeight  := Round(PageWidth * kf);
      TempLeftMargin := Round(LeftMargin * kf);
      TempTopMargin := Round(TopMargin * kf);
      TempRightMargin := Round(RightMargin * kf);
      TempBottomMargin := Round(BottomMargin * kf);
      PR := Rect(ARect.Left + ARect.Width div 2 - TempPageWidth div 2,
                 ARect.Top + ARect.Height div 2 - TempPageHeight div 2,
                 ARect.Left + ARect.Width div 2 - TempPageWidth div 2 + TempPageWidth,
                 ARect.Top + ARect.Height div 2 - TempPageHeight div 2 + TempPageHeight);
    end;

  with ACanvas do
  begin
    Pen.Color := GetStyleColor(clBtnShadow);
    if PagePreview.ScaleFactor >= 2 then
      Pen.Width := 2;
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Rectangle(PR.Left, PR.Top, PR.Right, PR.Bottom);
    //
    SR := PR;
    InflateRect(SR, 5, 5);
    scImageCollection1.DrawBitmap(ACanvas, SR, 2);
    //
    if MarginsGroupBox.Visible then
    begin
      TR := PR;
      Inc(TR.Left, TempLeftMargin + 1);
      Inc(TR.Top, TempTopMargin + 1);
      Dec(TR.Right, TempRightMargin + 1);
      Dec(TR.Bottom, TempBottomMargin + 1);
      Pen.Color := clGray;
      Pen.Style := psDot;
      Brush.Color := $00F5F5F5;
      Brush.Style := bsBDiagonal;

      if TR.Left < PR.Left then TR.Left := PR.Left;
      if TR.Left > PR.Right then TR.Left := PR.Right;
      if TR.Right < PR.Left then TR.Right := PR.Left;
      if TR.Right > PR.Right then TR.Right := PR.Right;
      if TR.Top < PR.Top then TR.Top := PR.Top;
      if TR.Top > PR.Bottom then TR.Top := PR.Bottom;
      if TR.Bottom < PR.Top then TR.Bottom := PR.Top;
      if TR.Bottom > PR.Bottom then TR.Bottom := PR.Bottom;
      Rectangle(TR.Left, TR.Top, TR.Right, TR.Bottom);
    end;
  end;

end;

procedure TscPrintDlgFrm.PagerRadioButtonClick(Sender: TObject);
begin
  if not Self.Visible then Exit;
  if not PagerRadioButton.HandleAllocated then Exit;
  if not PageFromSpinEdit.HandleAllocated then Exit;
  if not PageToSpinEdit.HandleAllocated then Exit;

  if not PageFromSpinEdit.Focused and not PageToSpinEdit.Focused then
    PageFromSpinEdit.SetFocus;
end;

procedure TscPrintDlgFrm.PageToSpinEditEnter(Sender: TObject);
begin
  PagerRadioButton.Checked := True;
end;

procedure TscPrintDlgFrm.PaperSizeBoxClick(Sender: TObject);
var
  I: Integer;
begin
  if StopCheck then Exit;
  SaveCurrentPaperAndBin;
  I := PaperSizeBox.ItemIndex;
  if I <> -1 then
  begin
    GetPaperSizeInMM(PageWidth, PageHeight,
                     TscPaperInfo(Papers.Objects[I]).Size.X,
                     TscPaperInfo(Papers.Objects[I]).Size.Y);
    PagePreview.RepaintControl;
  end;
end;

procedure TscPrintDlgFrm.PaperSourceBoxClick(Sender: TObject);
begin
  if StopCheck then Exit;
  SaveCurrentPaperAndBin;
end;

procedure TscPrintDlgFrm.PrinterBoxClick(Sender: TObject);
var
  S1, S2, S3: String;
begin
  Printer.PrinterIndex := PrinterBox.ItemIndex;
  GetPrinterInfo(S1, S2, S3);
  L1.Caption := S1;
  L2.Caption := S2;
  L3.Caption := S3;
  StopCheck := True;
  CopiesSpinEdit.Value := Printer.Copies;
  CollateCheckBox.Checked := GetCollate;
  StopCheck := False;
  if Pager.Tabs[1].Visible then
  begin
    LoadPapersAndBins;
    LoadCurrentPaperAndBin;
    if Printer.Orientation = poPortrait
    then
      OrientationBox.ItemIndex := 0
    else
      OrientationBox.ItemIndex := 1;
  end;
end;

procedure TscPrintDlgFrm.PropertyButtonClick(Sender: TObject);
begin
  CallDocumentPropertiesDialog(Self.Handle);
  StopCheck := True;
  CopiesSpinEdit.Value :=  Printer.Copies;
  CollateCheckBox.Checked := GetCollate;
  StopCheck := False;
end;

procedure TscPrintDlgFrm.RightMEditChange(Sender: TObject);
begin
  if FUnits = scpmMillimeters
  then
    RightMargin := Round(RightMEdit.Value * 100)
  else
    RightMargin := Round(InchToMM(RightMEdit.Value) * 100);
  PagePreview.RePaintControl;
end;


constructor TscPrintDialog.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationOnControls := False;
  FScaled := True;
  FTitle := '';
  FUnits := scpmMillimeters;
  FPrintToFileText := 'Print to file';

  FMinMarginLeft := 0;
  FMinMarginTop := 0;
  FMinMarginRight := 0;
  FMinMarginBottom := 0;

  FMaxMarginLeft := 2500;
  FMaxMarginTop := 2500;
  FMaxMarginRight := 2500;
  FMaxMarginBottom := 2500;

  FMarginLeft := 2500;
  FMarginTop := 2500;
  FMarginRight := 2500;
  FMarginBottom := 2500;

  FPageWidth := 0;
  FPageHeight := 0;

  FPrintRange := scprAllPages;
  FPrintToFile := False;
  FCollate := False;
  FFromPage := 1;
  FToPage := 1;
  FMinPage := 1;
  FMaxPage := 1;
  FOptions := [];
end;

procedure TscPrintDialog.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscPrintDialog.SetCopies(Value: Integer);
begin
  FCopies := Value;
  Printer.Copies := Value;
end;

procedure TscPrintDialog.SetAnimation;
var
  I: Integer;
begin
  if FDlgFrm = nil then Exit;
  for I := 0 to FDlgFrm.ComponentCount - 1 do
  begin
    if FDlgFrm.Components[I] is TscButton then
      TscButton(FDlgFrm.Components[I]).Animation := FAnimationOnControls
    else
    if FDlgFrm.Components[I] is TscCheckBox then
      TscCheckBox(FDlgFrm.Components[I]).Animation := FAnimationOnControls
    else
    if FDlgFrm.Components[I] is TscRadioButton then
      TscRadioButton(FDlgFrm.Components[I]).Animation := FAnimationOnControls;
  end;
end;

function TscPrintDialog.Execute: Boolean;
var
  IsChange: Boolean;
  OldPrinterIndex: Integer;
  FDiv: Integer;
begin
  if (Printer = nil) or (Printer.Printers.Count = 0) then
  begin
    scMessageDlg(SC_S_PRN_Warning, mtError, [mbOk], 0);
    Result := False;
    Exit;
  end;

  if Assigned(FOnShow) then FOnShow(Self);
  FDlgFrm := TscPrintDlgFrm.Create(Application);
  SetAnimation;
  if FTitle <> '' then
    FDlgFrm.Caption := FTitle;
  // apply options
  FDlgFrm.FUnits := Self.FUnits;
  if not (scpsoDisableMargins in FOptions) then
  begin
    if FUnits = scpmMillimeters then
      FDiv := 100
    else
    begin
      FDiv := 1000;
      FDlgFrm.MarginsGroupBox.Caption := SC_S_PRN_MarginsIn;
    end;
    FDlgFrm.LeftMEdit.MinValue := FMinMarginLeft / FDiv;
    FDlgFrm.LeftMEdit.MaxValue := FMaxMarginLeft / FDiv;
    FDlgFrm.LeftMEdit.Value := FMarginLeft / FDiv;
    FDlgFrm.RightMEdit.MinValue := FMinMarginRight / FDiv;
    FDlgFrm.RightMEdit.MaxValue := FMaxMarginRight / FDiv;
    FDlgFrm.RightMEdit.Value := FMarginRight / FDiv;
    FDlgFrm.TopMEdit.MinValue := FMinMarginTop / FDiv;
    FDlgFrm.TopMEdit.MaxValue := FMaxMarginTop / FDiv;
    FDlgFrm.TopMEdit.Value := FMarginTop / FDiv;
    FDlgFrm.BottomMEdit.MinValue := FMinMarginBottom / FDiv;
    FDlgFrm.BottomMEdit.MaxValue := FMaxMarginBottom / FDiv;
    FDlgFrm.BottomMEdit.Value := FMarginBottom / FDiv;
  end
  else
    FDlgFrm.MarginsGroupBox.Visible := False;

   if FPrintToFileText <> ''  then
     FDlgFrm.PrintToFileCheckBox.Caption := FPrintToFileText;  

  if scpsoDisablePrinter in Options then
    FDlgFrm.PrinterBox.Enabled := False;

  if scpsoDisablePrintToFile in Options then
    FDlgFrm.PrintToFileCheckBox.Visible := False;

  if scpsoDisablePagesCount in Options then
  begin
    FDlgFrm.AllRadioButton.Enabled := False;
    FDlgFrm.PagerRadioButton.Enabled := False;
    FDlgFrm.PageFromSpinEdit.Enabled := False;
    FDlgFrm.PageToSpinEdit.Enabled := False;
  end
  else
  begin
    if FPrintRange = scprAllPages then
      FDlgFrm.AllRadioButton.Checked := True
    else
      FDlgFrm.PagerRadioButton.Checked := True;
  end;

  if scpsoDisableCopiesCount in Options then
  begin
    FDlgFrm.CopiesSpinEdit.Enabled := False;
    FDlgFrm.CollateCheckBox.Enabled := False;
  end;

  if scpsoDisablePaperPage in Options then
    FDlgFrm.Pager.Tabs[1].Visible := False;

  if scpsoDisablePaperSize in Options then
    FDlgFrm.PaperSizeBox.Enabled := False;

  if scpsoDisablePaperSource in Options then
    FDlgFrm.PaperSourceBox.Enabled := False;

  if scpsoDisableOrientation in Options then
    FDlgFrm.OrientationBox.Enabled := False;

  // set init values
  FDlgFrm.CopiesSpinEdit.Value := Self.Copies;

  FDlgFrm.PageFromSpinEdit.MinValue := MinPage;
  FDlgFrm.PageFromSpinEdit.MaxValue := MaxPage;
  FDlgFrm.PageFromSpinEdit.Value := FromPage;

  FDlgFrm.PageToSpinEdit.MinValue := MinPage;
  FDlgFrm.PageToSpinEdit.MaxValue := MaxPage;
  FDlgFrm.PageToSpinEdit.Value := ToPage;

  //
  IsChange := False;
  try
    OldPrinterIndex := Printer.PrinterIndex;
    Result := (FDlgFrm.ShowModal = mrOk);
    if Result then
    begin
      IsChange := True;
      // save options
      FCopies := Printer.Copies;
      if FUnits = scpmMillimeters then
      begin
        if not (scpsoDisableMargins in FOptions) then
        begin
          FMarginLeft := FDlgFrm.LeftMargin;
          FMarginTop := FDlgFrm.TopMargin;
          FMarginRight := FDlgFrm.RightMargin;
          FMarginBottom := FDlgFrm.BottomMargin;
        end;
        FPageWidth := FDlgFrm.PageWidth;
        FPageHeight := FDlgFrm.PageHeight;
      end
      else
      begin
        if not (scpsoDisableMargins in FOptions) then
        begin
          FMarginLeft := Round(MMToInch2(FDlgFrm.LeftMargin));
          FMarginTop := Round(MMToInch2(FDlgFrm.TopMargin));
          FMarginRight := Round(MMToInch2(FDlgFrm.RightMargin));
          FMarginBottom := Round(MMToInch2(FDlgFrm.BottomMargin));
        end;
        FPageWidth := Round(MMToInch2(FDlgFrm.PageWidth));
        FPageHeight := Round(MMToInch2(FDlgFrm.PageHeight));
      end;

      if not (scpsoDisableCopiesCount in Options) then
      begin
        FCopies := Round(FDlgFrm.CopiesSpinEdit.Value);
        FCollate := FDlgFrm.CollateCheckBox.Checked;
      end;

      if not (scpsoDisablePagesCount in Options) then
      begin
        if FDlgFrm.PagerRadioButton.Checked then
        begin
          FPrintRange := scprPageNums;
          FFromPage := Round(FDlgFrm.PageFromSpinEdit.Value);
          FToPage := Round(FDlgFrm.PageToSpinEdit.Value);
        end
        else
          FPrintRange := scprAllPages;
      end;

      if not (scpsoDisablePrintToFile in Options) then
        FPrintToFile := FDlgFrm.PrintToFileCheckBox.Checked;

    end
    else
    begin
      RestoreDocumentProperties;
      if Printer.PrinterIndex <> OldPrinterIndex then
         Printer.PrinterIndex := OldPrinterIndex;
    end;
  finally
     FDlgFrm.Free;
     if IsChange then
       Change;
     if Assigned(FOnClose) then
       FOnClose(Self);
  end;
end;

procedure RemoveAccelChar(var S: String);
var
  I: Integer;
begin
  I := Pos('&', S);
  if I > 0 then
    Delete(S, I, 1);
end;

var
  Lib: HModule = 0;
  PLib: HModule = 0;

initialization

  Lib := LoadLibrary('compstui.dll');
  if Lib <> 0 then
  begin
    SC_S_PRN_Settings := LoadStringFromDll(Lib, 64721, SC_S_PRN_Settings);
    SC_S_PRN_PageSettings := LoadStringFromDll(Lib, 64719, SC_S_PRN_PageSettings);
    SC_S_PRN_Priner := LoadStringFromDll(Lib, 64717, SC_S_PRN_Priner);
    SC_S_PRN_Properties := LoadStringFromDll(Lib, 64713, SC_S_PRN_Properties);
    SC_S_PRN_Collate := LoadStringFromDll(Lib, 64756, SC_S_PRN_Collate);
    SC_S_PRN_CopyCount := LoadStringFromDll(Lib, 64740, SC_S_PRN_CopyCount);
    SC_S_PRN_PageCount := LoadStringFromDll(Lib, 64855, SC_S_PRN_PageCount);
    RemoveAccelChar(SC_S_PRN_CopyCount);
    RemoveAccelChar(SC_S_PRN_PageCount);
    SC_S_PRN_PageSize := LoadStringFromDll(Lib, 64747, SC_S_PRN_PageSize);
    SC_S_PRN_PageSource := LoadStringFromDll(Lib, 64741, SC_S_PRN_PageSource);

    SC_S_PRN_Orientation := LoadStringFromDll(Lib, 64738, SC_S_PRN_Orientation);
    SC_S_PRN_Portrait := LoadStringFromDll(Lib, 64753, SC_S_PRN_Portrait);
    RemoveAccelChar(SC_S_PRN_Portrait);
    SC_S_PRN_Landscape := LoadStringFromDll(Lib, 64754, SC_S_PRN_Landscape);
    RemoveAccelChar(SC_S_PRN_Landscape);
    SC_S_PRN_AllPages := LoadStringFromDll(Lib, 64841, SC_S_PRN_AllPages);

    FreeLibrary(Lib);
  end;

  Lib := LoadLibrary('comdlg32.dll');
  if Lib <> 0 then
  begin
    SC_S_PRN_Print := LoadStringFromDll(Lib, 1124, SC_S_PRN_Print);
    SC_S_PRN_MarginsMM := LoadStringFromDll(Lib, 1586, SC_S_PRN_MarginsMM);
    SC_S_PRN_MarginsIn := LoadStringFromDll(Lib, 1585, SC_S_PRN_MarginsIn);

    SC_S_PRN_Warning := LoadStringFromDll(Lib, 1113, SC_S_PRN_MarginsIn);

    SC_S_PRNSTATUS_Ready := LoadStringFromDll(Lib, 1536, SC_S_PRNSTATUS_Ready);

    FreeLibrary(Lib);
  end;

end.
