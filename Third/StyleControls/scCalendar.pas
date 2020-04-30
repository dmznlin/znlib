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


unit scCalendar;

{$R-}
{$I scdefine.inc}

interface

uses Winapi.Windows, Winapi.Messages, System.Classes,
     System.Types, System.UITypes, Vcl.Menus, Vcl.Forms,
     Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls, scDrawUtils, scControls,
     scImageCollection;

type
  TscDaysOfWeek = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

  TscCalButton = class(TscButton)
  protected
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  public
    DrawCode: Integer;
  end;

  TscMonthCalendar = class(TscPanel)
  protected
    FShowMonthMenu: Boolean;
    FMenuPopup: Boolean;
    FMonthMenu: TPopupMenu;
    FIsChina: Boolean;
    FWeekNumbers: Boolean;
    FShowToday: Boolean;
    FTodayDefault: Boolean;
    BevelTop, CellW, CellH, BottomOffset: Integer;
    FBtns: array[0..3] of TscCalButton;
    FDate: TDate;
    FFirstDayOfWeek: TscDaysOfWeek;
    CalFontColor: TColor;
    CalActiveFontColor: TColor;
    FOnNumberClick: TNotifyEvent;
    FBoldDays: Boolean;
    FTodayR: TRect;
    FInTodayR: Boolean;
    procedure MonthMenuClick(Sender: TObject);
    procedure Loaded; override;
    procedure CreateMonthMenu;
    procedure SetTodayDefault(Value: Boolean);
    procedure OffsetMonth(AOffset: Integer);
    procedure OffsetYear(AOffset: Integer);
    procedure SetFirstDayOfWeek(Value: TscDaysOfWeek);
    procedure UpdateCalendar;
    procedure ArangeControls;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
    procedure SetDate(Value: TDate);
    procedure DrawCalendar(Cnvs: TCanvas);
    function DaysThisMonth: Integer;
    function GetMonthOffset: Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DayNumFromPoint(X, Y: Integer): Word;
    procedure NextMButtonClick(Sender: TObject);
    procedure PriorMButtonClick(Sender: TObject);
    procedure NextYButtonClick(Sender: TObject);
    procedure PriorYButtonClick(Sender: TObject);
    procedure SetBoldDays(Value: Boolean);
    procedure SetWeekNumbers(Value: Boolean);
    procedure SetShowToday(Value: Boolean);
    procedure DrawFrame(R: TRect; C: TCanvas);
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Date: TDate read FDate write SetDate;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers;
    property ShowToday: Boolean read FShowToday write SetShowToday;
    property TodayDefault: Boolean read FTodayDefault write SetTodayDefault;
    property FirstDayOfWeek: TscDaysOfWeek read FFirstDayOfWeek write SetFirstDayOfWeek;
    property OnNumberClick: TNotifyEvent
      read FOnNumberClick write FOnNumberClick;
    property BoldDays: Boolean read FBoldDays write SetBoldDays;
    property ShowMonthMenu: Boolean read FShowMonthMenu write FShowMonthMenu;
  end;

  TscPopupMonthCalendar = class(TscMonthCalendar)
  protected
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscCalBackgroundStyle = (sccasPanel, sccasFormBackground, sccasEdit);
  TscDateOrder = (scdoMDY, scdoDMY, scdoYMD);
  TscDateEdit = class(TscCustomEdit)
  private
    StopCheck: Boolean;
    FTodayDefault: Boolean;
    FBlanksChar: Char;
    FDateSelected: Boolean;
    FCalendarBackgroundStyle: TscCalBackgroundStyle;
    FCalendarWallpapers: TscCustomImageCollection;
    FCalendarWallpaperIndex: Integer;
  protected
    FMonthCalendar: TscPopupMonthCalendar;
    FOnDateChange: TNotifyEvent;
    FOldDateValue: TDateTime;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMMouseHookCancelMode(var Message: TMessage); message WM_MOUSEHOOKCANCELMODE;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function FourDigitYear: Boolean;
    function GetDateOrder(const DateFormat: string): TscDateOrder;
    function DefDateFormat(FourDigitYear: Boolean): string;
    function DefDateMask(BlanksChar: Char; FourDigitYear: Boolean): string;
    function MonthFromName(const S: string; MaxLen: Byte): Byte;
    procedure ExtractMask(const Format, S: string; Ch: Char; Cnt: Integer;
       var I: Integer; Blank, Default: Integer);
    function ScanDateStr(const Format, S: string; var D, M, Y: Integer): Boolean;
    function CurrentYear: Word;
    function ExpandYear(Year: Integer): Integer;
    function IsValidDate(Y, M, D: Word): Boolean;
    function ScanDate(const S, DateFormat: string; var Pos: Integer;
      var Y, M, D: Integer): Boolean;
    function GetDateMask: String;
    procedure Loaded; override;

    function GetShowToday: Boolean;
    procedure SetShowToday(Value: Boolean);
    function GetWeekNumbers: Boolean;
    procedure SetWeekNumbers(Value: Boolean);

    procedure SetBlanksChar(Value: Char);

    procedure SetTodayDefault(Value: Boolean);
    function GetCalendarFont: TFont;
    procedure SetCalendarFont(Value: TFont);
    function GetCalendarWidth: Integer;
    procedure SetCalendarWidth(Value: Integer);

    function GetCalendarBoldDays: Boolean;
    procedure SetCalendarBoldDays(Value: Boolean);

    function GetCalendarHeight: Integer;
    procedure SetCalendarHeight(Value: Integer);

    function GetDate: TDate;
    procedure SetDate(Value: TDate);
    procedure DropDown; virtual;
    procedure CloseUp(AcceptValue: Boolean);
    procedure CalendarClick(Sender: TObject);
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WndProc(var Message: TMessage); override;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CheckValidDate;
    procedure Change; override;

    function GetFirstDayOfWeek: TscDaysOfWeek;
    procedure SetFirstDayOfWeek(Value: TscDaysOfWeek);
    function IsValidText(S: String): Boolean;
    function IsOnlyNumbers(S: String): Boolean;
    function MyStrToDate(S: String): TDate;
    function MyDateToStr(Date: TDate): String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsDateInput: Boolean;
    procedure ValidateEdit; override;
    procedure ButtonClick(Sender: TObject);
    function IsCalendarVisible: Boolean;
  published
    property BlanksChar: Char read FBlanksChar write SetBlanksChar;
    property Color;
    property Date: TDate read GetDate write SetDate;
    property TodayDefault: Boolean read FTodayDefault write SetTodayDefault;

    property CalendarWidth: Integer read GetCalendarWidth write SetCalendarWidth;
    property CalendarHeight: Integer read GetCalendarHeight write SetCalendarHeight;
    property CalendarFont: TFont read GetCalendarFont write SetCalendarFont;
    property CalendarBoldDays: Boolean read GetCalendarBoldDays write SetCalendarBoldDays;
    property CalendarBackgroundStyle: TscCalBackgroundStyle
      read FCalendarBackgroundStyle write FCalendarBackgroundStyle;
    property CalendarWallpapers: TscCustomImageCollection
      read FCalendarWallpapers write FCalendarWallpapers;
    property CalendarWallpaperIndex: Integer
      read FCalendarWallpaperIndex write FCalendarWallpaperIndex;

    property FirstDayOfWeek: TscDaysOfWeek
      read GetFirstDayOfWeek write SetFirstDayOfWeek;


    property WeekNumbers: Boolean
      read GetWeekNumbers write SetWeekNumbers;

    property ShowToday: Boolean
      read GetShowToday write SetShowToday;

    property OnDateChange: TNotifyEvent
      read FOnDateChange write FOnDateChange;

    property LeftButton;
    property RightButton;
    property Transparent;
    property BorderKind;
    property FrameColor;
    property FrameActiveColor;
    property ButtonImages;
    property BorderStyle;

    property Align;
    property ReadOnly;
    property Font;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property CharCase;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StyleElements;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnLeftButtonClick;
  end;

  function ExtractDay(ADate: TDateTime): Word;
  function ExtractMonth(ADate: TDateTime): Word;
  function ExtractYear(ADate: TDateTime): Word;
  function IsLeapYear(AYear: Integer): Boolean;
  function DaysPerMonth(AYear, AMonth: Integer): Integer;

  procedure SCScanBlanks(const S: string; var Pos: Integer);
  function SCScanNumber(const S: string; MaxLength: Integer; var Pos: Integer;
  var Number: Longint): Boolean;
  function SCScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;

implementation

  Uses System.SysUtils;

const
  BSize = 23;
  RepeatInt = 100;
  CenturyOffset = 60;

procedure SCScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  Pos := I;
end;

function SCScanNumber(const S: string; MaxLength: Integer; var Pos: Integer;
  var Number: Longint): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  SCScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (Longint(I - Pos) < MaxLength) and
    CharInSet(S[I], ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then begin
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function SCScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
begin
  Result := False;
  SCScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then begin
    Inc(Pos);
    Result := True;
  end;
end;

function ExtractDay(ADate: TDateTime): Word;
var
  M, Y: Word;
begin
  DecodeDate(ADate, Y, M, Result);
end;

function ExtractMonth(ADate: TDateTime): Word;
var
  D, Y: Word;
begin
  DecodeDate(ADate, Y, Result, D);
end;

function ExtractYear(ADate: TDateTime): Word;
var
  D, M: Word;
begin
  DecodeDate(ADate, Result, M, D);
end;

function IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result);
end;

procedure TscCalButton.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
var
  R, R1, R2: TRect;
begin
  inherited;
  R := Rect(0, 0, Width, Height);
  R1 := R;
  R1.Right := R.Width div 2;
  R1.Left := 5;
  R2 := R;
  R2.Left := R.Width div 2;
  R2.Right := R2.Right - 5;
  case DrawCode of
    0:
    begin
      scDrawUtils.DrawArrowImage(ACanvas, R, ACanvas.Font.Color, 1, FScaleFactor);
    end;
    1:
    begin
      scDrawUtils.DrawArrowImage(ACanvas, R, ACanvas.Font.Color, 2, FScaleFactor);
    end;
    2:
    begin
      scDrawUtils.DrawArrowImage(ACanvas, R1, ACanvas.Font.Color, 1, FScaleFactor);
      scDrawUtils.DrawArrowImage(ACanvas, R2, ACanvas.Font.Color, 1, FScaleFactor);
    end;
    3:
    begin
      scDrawUtils.DrawArrowImage(ACanvas, R1, ACanvas.Font.Color, 2, FScaleFactor);
      scDrawUtils.DrawArrowImage(ACanvas, R2, ACanvas.Font.Color, 2, FScaleFactor);
    end;
  end;
end;

constructor TscMonthCalendar.Create;
begin
  inherited;
  FCanEmpty := False;
  FInTodayR := False;
  FTodayR := Rect(0, 0, 0, 0);
  FIsChina := (GetSystemDefaultLangID and $3FF) = $04;
  FMenuPopup := False;
  FWeekNumbers := False;
  FMonthMenu := TPopupMenu.Create(Self);
  FShowToday := False;
  BottomOffset := 0;
  FBtns[0] := TscCalButton.Create(Self);
  FShowMonthMenu := True;
  with FBtns[0] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt;
    Width := BSize;
    Height := BSize;
    DrawCode := 0;
    OnClick := PriorMButtonClick;
    Parent := Self;
  end;

  FBtns[1] := TscCalButton.Create(Self);
  with FBtns[1] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt;
    Width := BSize;
    Height := BSize;
    DrawCode := 1;
    OnClick := NextMButtonClick;
    Parent := Self;
  end;

  FBtns[2] := TscCalButton.Create(Self);
  with FBtns[2] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt - 50;
    Width := BSize;
    Height := BSize;
    DrawCode := 2;
    OnClick := PriorYButtonClick;
    Parent := Self;
  end;

  FBtns[3] := TscCalButton.Create(Self);
  with FBtns[3] do
  begin
    CanFocused := False;
    RepeatClick := True;
    RepeatClickInterval := RepeatInt - 50;
    Width := BSize;
    Height := BSize;
    DrawCode := 3;
    OnClick := NextYButtonClick;
    Parent := Self;
  end;

  Width := 200;
  Height := 150;

  Date := Now;
  FTodayDefault := False;
  FBoldDays := False;
end;

destructor TscMonthCalendar.Destroy;
begin
  FMonthMenu.Free;
  inherited;
end;

procedure TscMonthCalendar.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  ArangeControls;
end;

procedure TscMonthCalendar.Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState);
begin
  inherited;
  DrawCalendar(ACanvas);
end;

procedure TscMonthCalendar.DrawFrame;
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(C.Handle);
  if StyleKind = scpsEdit then
  begin
    scDrawUtils.DrawSelection(C, R, False, True);
    RestoreDC(C.Handle, SaveIndex);
    C.Font.Color := scDrawUtils.GetSelectionTextColor;
  end
  else
  begin
    scDrawUtils.DrawToolButton(C, R, scsPressed);
    RestoreDC(C.Handle, SaveIndex);
    C.Font.Color := scDrawUtils.GetToolButtonTextColor(scsPressed);
  end;
end;

procedure TscMonthCalendar.SetWeekNumbers(Value: Boolean);
begin
  if FWeekNumbers <> Value
  then
    begin
      FWeekNumbers := Value;
      RePaintControl;
    end;
end;

procedure TscMonthCalendar.SetShowToday(Value: Boolean);
begin
  if FShowToday <> Value
  then
    begin
      FShowToday := Value;
      RePaintControl;
    end;
end;

procedure TscMonthCalendar.SetBoldDays(Value: Boolean);
begin
  FBoldDays := Value;
  RePaintControl;
end;

procedure TscMonthCalendar.SetTodayDefault;
begin
  FTodayDefault := Value;
  if FTodayDefault then Date := Now;
end;

procedure TscMonthCalendar.NextMButtonClick(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  if AMonth = 12 then OffsetYear(1);
  OffsetMonth(1);
  Click;
end;

procedure TscMonthCalendar.PriorMButtonClick(Sender: TObject);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  if AMonth = 1 then OffsetYear(-1);
  OffsetMonth(-1);
  Click;
end;

procedure TscMonthCalendar.NextYButtonClick(Sender: TObject);
begin
  OffsetYear(1);
  Click;
end;

procedure TscMonthCalendar.PriorYButtonClick(Sender: TObject);
begin
  OffsetYear(-1);
  Click;
end;

procedure TscMonthCalendar.OffsetMonth(AOffset: Integer);
var
  AYear, AMonth, ADay: Word;
  TempDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  AMonth := AMonth + AOffset;
  if AMonth > 12 then AMonth := 1 else
  if AMonth <= 0 then AMonth := 12;
  if ADay > DaysPerMonth(AYear, AMonth)
  then ADay := DaysPerMonth(AYear, AMonth);
  TempDate := EncodeDate(AYear, AMonth, ADay);
  Date := TempDate;
end;

procedure TscMonthCalendar.OffsetYear(AOffset: Integer);
var
  AYear, AMonth, ADay: Word;
  TempDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  AYear := AYear + AOffset;
  if AYear <= 1760 then Exit else
    if AYear > 9999 then Exit;
  if ADay > DaysPerMonth(AYear, AMonth)
  then ADay := DaysPerMonth(AYear, AMonth);
  TempDate := EncodeDate(AYear, AMonth, ADay);
  Date := TempDate;
end;

procedure TscMonthCalendar.SetFirstDayOfWeek(Value: TscDaysOfWeek);
begin
  FFirstDayOfWeek := Value;
  UpdateCalendar;
end;

procedure TscMonthCalendar.ArangeControls;
var
  R: TRect;
  BSizeScale: Integer;
begin
  R := Rect(0, 0, Width, Height);
  InflateRect(R, -1, -1);
  BSizeScale := Round(BSize * FScaleFactor);
  if FBtns[0] = nil then Exit;
  with FBtns[2] do SetBounds(R.Left + 2, R.Top + 2, Width, Height);
  with FBtns[0] do SetBounds(FBtns[2].Left + FBtns[2].Width + 1,
    R.Top + 2, Width, Height);
  with FBtns[3] do SetBounds(R.Right - BSizeScale - 2, R.Top + 2, Width, Height);
  with FBtns[1] do SetBounds(FBtns[3].Left - FBtns[3].Width - 1 , R.Top + 2, Width, Height);
end;

procedure TscMonthCalendar.WMSIZE;
begin
  inherited;
  ArangeControls;
  UpdateControls;
end;

procedure TscMonthCalendar.SetDate(Value: TDate);
begin
  FDate := Value;
  UpdateCalendar;
  RePaintControl;
end;

procedure TscMonthCalendar.UpdateCalendar;
begin
  RePaintControl;
end;

function TscMonthCalendar.GetMonthOffset: Integer;
var
  AYear, AMonth, ADay: Word;
  FirstDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  FirstDate := EncodeDate(AYear, AMonth, 1);
  Result := 2 - ((DayOfWeek(FirstDate) - Ord(FirstDayOfWeek) + 7) mod 7);
  if Result = 2 then Result := -5;
end;

procedure TscMonthCalendar.DrawCalendar(Cnvs: TCanvas);

function WeekOfTheYear(Dat: TDateTime): Word;
var
  Day, Month, Year: Word;
  FirstDate: TDateTime;
  DateDiff: Integer;
begin
  Day := DayOfWeek(Dat) - 1;
  Dat := Dat + 3 -((6 + day) mod 7);
  DecodeDate(Dat, Year, Month, Day);
  FirstDate := EncodeDate(Year,1,1);
  DateDiff := Trunc(Dat - FirstDate);
  Result := 1 + (DateDiff div 7);
end;

var
  R, LR: TRect;
  I, J: Integer;
  FMonthOffset, X, Y, X2, Y2: Integer;
  S: String;
  ADay, DayNum: Integer;
  CDate: TDateTime;
  AYear, AMonth, ADay_: Word;
  Week, OldWeek: Integer;
  CalGrayFontColor: TColor;
begin
  Cnvs.Brush.Style := bsClear;
  if Self.StyleKind <> scpsEdit then
    CalFontColor := scDrawUtils.GetCheckBoxTextColor(scsNormal)
  else
    CalFontColor := scDrawUtils.GetEditTextColor(scsNormal);
  CalActiveFontColor := scDrawUtils.GetActiveTextColor;
  if IsLightColor(CalFontColor) then
    CalGrayFontColor := DarkerColor(CalFontColor, 40)
  else
    CalGrayFontColor := LighterColor(CalFontColor, 40);
  R := Rect(0, 0, Width, Height);
  if FWeekNumbers
  then
    begin
      Inc(R.Left, Width div 8);
    end;
  with Cnvs do
  begin
    Font := Self.Font;
    if FShowToday
    then
      begin
        BottomOffset := TextHeight('Wq') + 7;
        Dec(R.Bottom, BottomOffset);
      end
    else
      BottomOffset := 0;
    Brush.Style := bsClear;
    // draw caption
    S := FormatDateTime('MMMM, YYYY', FDate);
    Y := R.Top + 5;
    Font.Style := [fsBold];
    X := Self.Width div 2 - TextWidth(S) div 2;
    Font.Color := CalFontColor;
    TextOut(X, Y, S);
    Font.Color := CalFontColor;
    CellW := (R.Width - 2) div 7;
    // draw week days
    X := R.Left + 1;
    Y := R.Top + Round(BSize * FScaleFactor) + 10;
    for I := 0 to 6 do
    begin
      S := FormatSettings.ShortDayNames[(Ord(FirstDayOfWeek) + I) mod 7 + 1];
      if FIsChina
      then
        begin
          if Length(S) > 2 then S := Copy(S, 3, 1)
        end
      else
        if Length(S) > 4 then S := Copy(S, 1, 4);
      X2 := X + CellW div 2 - TextWidth(S) div 2;
      TextOut(X2, Y, S);
      X := X + CellW;
    end;
    // draw bevel
    BevelTop := Y + TextHeight('Wq') + 1;
    LR := Rect(R.Left + 5, BevelTop, R.Left + R.Width - 5, BevelTop + 1);
    Cnvs.Brush.Color := CalFontColor;
    scDrawUtils.FillRectWithAlpha(Cnvs, LR, 50);
    if FWeekNumbers
    then
      begin
        LR := Rect(R.Left - 1, BevelTop + 5,
          R.Left, R.Bottom - 5);
        scDrawUtils.FillRectWithAlpha(Cnvs, LR, 50);
      end;
    Cnvs.Brush.Style := bsClear;
    if FBoldDays then Font.Style := [fsBold] else Font.Style := [];
    // draw today
    if FShowToday
    then
      begin
        if WeekNumbers then
          X := R.Left + 10
        else
          X := R.Left + 15;
        Y := R.Bottom + 2;
        S := DateToStr(Now);
        if FInTodayR then
          Font.Color := CalActiveFontColor
        else
          Font.Color := CalFontColor;
        TextOut(X, Y, S);
        FTodayR := Rect(X, Y, X + TextWidth(S), Y + TextHeight(S));
        InflateRect(FTodayR, Round(FScaleFactor), Round(FScaleFactor));
      end;
    // draw month numbers
    CellH := (R.Bottom - BevelTop - 4) div 6;
    Font.Color := CalFontColor;
    FMonthOffset := GetMonthOffset;
    ADay := ExtractDay(FDate);
    Y := BevelTop + 3;
    OldWeek := -2;
    for J := 0 to 6 do
    begin
      X := R.Left + 1;
      if FWeekNumbers
      then
        begin
          Week := -1;
          for I := 0 to 6 do
          begin
            DayNum := FMonthOffset + I + (J - 1) * 7;
            if (DayNum > 0) and (DayNum <= DaysThisMonth)
            then
              begin
                DecodeDate(FDate, AYear, AMonth, ADay_);
                CDate := EncodeDate(AYear, AMonth, DayNum);
                Week := WeekOfTheYear(CDate);
                if FirstDayOfWeek <> Sun
                then
                  Break;
              end;
          end;
          if Week <> -1
          then
            begin
              if (OldWeek = Week)
              then
                begin
                  Week := OldWeek + 1;
                  if Week > 52 then Week := 52;
                end;
              OldWeek := Week;
              S := IntToStr(Week);
              X2 := X + CellW div 2 - TextWidth(S) div 2 - CellW - 2;
              Y2 := Y - CellH div 2 - TextHeight(S) div 2;
              Font.Color := CalGrayFontColor;
              Font.Style := Font.Style - [fsBold];
              TextOut(X2, Y2, S);
              Font.Color := CalFontColor;
            end;
         end;
      //
      for I := 0 to 6 do
      begin
        DayNum := FMonthOffset + I + (J - 1) * 7;
        if (DayNum < 1) or (DayNum > DaysThisMonth) then S := ''
        else S := IntToStr(DayNum);
        if DayNum = ADay
        then
          Font.Style := Font.Style + [fsBold]
        else
          Font.Style := Font.Style - [fsBold];
        X2 := X + CellW div 2 - TextWidth(S) div 2;
        Y2 := Y - CellH div 2 - TextHeight(S) div 2;
        if DayNum = ADay
        then
          DrawFrame(Rect(X, Y - CellH, X + CellW, Y + 1), Cnvs);
        if S <> '' then TextOut(X2, Y2, S);
        Font.Color := CalFontColor;
        X := X + CellW;
      end;
      Y := Y + CellH;
    end;
  end;
end;

function TscMonthCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(ExtractYear(FDate), ExtractMonth(FDate));
end;

function TscMonthCalendar.DayNumFromPoint;
var
  R, R1: TRect;
  FMonthOffset, X1, Y1, I, J: Integer;
begin
  Result := 0;
  R := Rect(0, 0, Width, Height);
  if FWeekNumbers
  then
    begin
      Inc(R.Left, Width div 8);
    end;
  if FShowToday
  then
    begin
      Dec(R.Bottom, BottomOffset);
    end;
  if not PtInRect(R, Point(X, Y)) then Exit;
  FMonthOffset := GetMonthOffset;
  Y1 := BevelTop + 3;
  for J := 0 to 6 do
  begin
    X1 := R.Left + 1;
    for I := 0 to 6 do
    begin
      R1 := Rect(X1, Y1 - CellH, X1 + CellW, Y1);
      if PtInRect(R1, Point(X, Y))
      then
        begin
          Result := FMonthOffset + I + (J - 1) * 7;
          if (Result < 1) or (Result > DaysThisMonth) then Result := 0;
          Break;
        end;
      X1 := X1 + CellW;
    end;
    Y1 := Y1 + CellH;
  end;
end;

procedure TscMonthCalendar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FInTodayR then
  begin
    FInTodayR := False;
    RePaintControl;
  end;
end;

procedure TscMonthCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FShowToday then
  begin
    if FTodayR.Contains(Point(X, Y)) and not FInTodayR then
    begin
      FInTodayR := True;
      RePaintControl;
    end
    else
    if not FTodayR.Contains(Point(X, Y)) and FInTodayR then
    begin
      FInTodayR := False;
      RePaintControl;
    end;
  end;
end;



procedure TscMonthCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  DayNum, AYear, AMonth, ADay: Word;
  TempDate: TDate;
begin
  inherited;
  if Button <> mbLeft then Exit;
  if FShowMonthMenu and (X > FBtns[0].Left + FBtns[0].Width) and (X < Self.FBtns[1].Left) and
     (Y > 2) and (Y <= Self.FBtns[0].Top + Self.FBtns[0].Height) then
  begin
    GetCursorPos(P);
    CreateMonthMenu;
    FMenuPopup := True;
    FMonthMenu.Popup(P.X, P.Y);
    FMenuPopup := False;
  end
  else
  begin
    DayNum := DayNumFromPoint(X, Y);
    if DayNum <> 0
    then
    begin
      DecodeDate(FDate, AYear, AMonth, ADay);
      ADay := DayNum;
      TempDate := EncodeDate(AYear, AMonth, ADay);
      Date := TempDate;
      if Assigned(FOnNumberClick) then FOnNumberClick(Self);
    end
    else
    if FShowToday and FTodayR.Contains(Point(X, Y)) then
    begin
      Date := Now;
      if Assigned(FOnNumberClick) then FOnNumberClick(Self);
    end;
  end;
end;

procedure  TscMonthCalendar.MonthMenuClick(Sender: TObject);
var
  AYear, AMonth, ADay, DaysPM: Word;
  TempDate: TDate;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  AMonth := TMenuItem(Sender).MenuIndex + 1;
  DaysPM := DaysPerMonth(AYear, AMonth);
  if ADay > DaysPM then ADay := DaysPM;
  TempDate := EncodeDate(AYear, AMonth, ADay);
  Date := TempDate;
end;

procedure TscMonthCalendar.CreateMonthMenu;
var
  I: Integer;
  MI: TMenuItem;
begin
  if FMonthMenu.Items.Count > 0 then Exit;
  for I := 1 to 12 do
  begin
    MI := TMenuItem.Create(Self);
    MI.Caption := FormatSettings.LongMonthNames[I];
    MI.OnClick := MonthMenuClick;
    FMonthMenu.Items.Add(MI);
  end;
end;

procedure TscMonthCalendar.Loaded;
begin
  inherited;
  ArangeControls;
  if FTodayDefault then Date := Now;
end;

constructor TscPopupMonthCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  DrawOnBackground := False;
  FPopupMode := True;
  StyleKind := scpsPanel;
  BorderStyle := scpbsFlat;
  if FBtns[0] <> nil then
  begin
    FBtns[0].DrawOnBackground := False;
    FBtns[1].DrawOnBackground := False;
    FBtns[2].DrawOnBackground := False;
    FBtns[3].DrawOnBackground := False;
  end;
end;

procedure  TscPopupMonthCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_SAVEBITS;
    WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TscPopupMonthCalendar.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

constructor TscDateEdit.Create(AOwner: TComponent);
begin
  inherited;
  FBlanksChar := ' ';
  EditMask := GetDateMask;
  FMonthCalendar := TscPopupMonthCalendar.Create(Self);
  if not (csDesigning in ComponentState) then
    FMonthCalendar.Parent := Self;
  FMonthCalendar.Visible := False;
  FMonthCalendar.OnNumberClick := CalendarClick;
  FMonthCalendar.OnMouseUp := CalendarMouseUp;
  FMonthCalendar.Date := 0;
  OnRightButtonClick := ButtonClick;
  FTodayDefault := False;
  RightButton.Visible := True;
  RightButton.ShowEllipses := True;
  FCalendarWallpaperIndex := -1;
  FCalendarWallpapers := nil;
end;

destructor TscDateEdit.Destroy;
begin
  FMonthCalendar.Free;
  FMonthCalendar := nil;
  inherited;
end;

procedure TscDateEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCalendarWallpapers) then
  begin
    FCalendarWallpapers := nil;
    if FMonthCalendar <> nil then FMonthCalendar.Wallpapers := nil;
  end;
end;

function TscDateEdit.GetShowToday: Boolean;
begin
  Result := FMonthCalendar.ShowToday;
end;

procedure TscDateEdit.SetBlanksChar(Value: Char);
begin
  if Value <> FBlanksChar then
  begin
    FBlanksChar := Value;
    EditMask := GetDateMask;
  end;
end;

procedure TscDateEdit.SetShowToday(Value: Boolean);
begin
  FMonthCalendar.ShowToday := Value;
end;

function TscDateEdit.GetWeekNumbers: Boolean;
begin
  Result := FMonthCalendar.WeekNumbers;
end;

procedure TscDateEdit.SetWeekNumbers(Value: Boolean);
begin
  FMonthCalendar.WeekNumbers := Value;
end;

function TscDateEdit.GetCalendarBoldDays: Boolean;
begin
  Result := FMonthCalendar.BoldDays;
end;

procedure TscDateEdit.SetCalendarBoldDays(Value: Boolean);
begin
  FMonthCalendar.BoldDays := Value;
end;

procedure TscDateEdit.ValidateEdit;
var
  Str: string;
  Pos: Integer;
begin
  Str := EditText;
  if IsMasked and Modified
  then
    begin
      if not Validate(Str, Pos) then
      begin
      end;
    end;
end;

function TscDateEdit.IsDateInput: Boolean;
begin
  Result := IsValidText(Text);
end;

function TscDateEdit.MonthFromName(const S: string; MaxLen: Byte): Byte;
begin
  if Length(S) > 0 then
    for Result := 1 to 12 do begin
      if (Length(FormatSettings.LongMonthNames[Result]) > 0) and
        (CompareText(Copy(S, 1, MaxLen),
        Copy(FormatSettings.LongMonthNames[Result], 1, MaxLen)) = 0) then Exit;
    end;
  Result := 0;
end;

function MakeStr(C: Char; N: Integer): String;
var
  S: String;
begin
  if N < 1 then Result := ''
  else
  begin
    S := StringOfChar(C, N);
    Result := S;
  end;
end;

procedure TscDateEdit.ExtractMask(const Format, S: string; Ch: Char; Cnt: Integer;
  var I: Integer; Blank, Default: Integer);
var
  Tmp: string;
  J, L: Integer;
  S1: String;
begin
  I := Default;
  Ch := UpCase(Ch);
  L := Length(Format);
  if Length(S) < L then L := Length(S)
  else if Length(S) > L then Exit;
  S1 := MakeStr(Ch, Cnt);
  J := Pos(S1, UpperCase(Format));
  if J <= 0 then Exit;
  Tmp := '';
  while (UpCase(Format[J]) = Ch) and (J <= L) do begin
    if S[J] <> ' ' then Tmp := Tmp + S[J];
    Inc(J);
  end;
  if Tmp = '' then I := Blank
  else if Cnt > 1 then begin
    I := MonthFromName(Tmp, Length(Tmp));
    if I = 0 then I := -1;
  end
  else I := StrToIntDef(Tmp, -1);
end;

function TscDateEdit.CurrentYear: Word;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := SystemTime.wYear;
end;

function TscDateEdit.ExpandYear(Year: Integer): Integer;
var
  N: Longint;
begin
  Result := Year;
  if Result < 100 then begin
    N := CurrentYear - CenturyOffset;
    Inc(Result, N div 100 * 100);
    if (CenturyOffset > 0) and (Result < N) then
      Inc(Result, 100);
  end;
end;

function TscDateEdit.IsValidDate(Y, M, D: Word): Boolean;
begin
  Result := (Y >= 1) and (Y <= 9999) and (M >= 1) and (M <= 12) and
    (D >= 1) and (D <= DaysPerMonth(Y, M));
end;

function TscDateEdit.ScanDate(const S, DateFormat: string; var Pos: Integer;
  var Y, M, D: Integer): Boolean;
var
  DateOrder: TscDateOrder;
  N1, N2, N3: Longint;
begin
  Result := False;
  Y := 0; M := 0; D := 0;
  DateOrder := GetDateOrder(DateFormat);
  if not (SCScanNumber(S, MaxInt, Pos, N1) and SCScanChar(S, Pos, FormatSettings.DateSeparator) and
    SCScanNumber(S, MaxInt, Pos, N2)) then Exit;
  if SCScanChar(S, Pos, FormatSettings.DateSeparator) then begin
    if not SCScanNumber(S, MaxInt, Pos, N3) then Exit;
    case DateOrder of
      scdoMDY: begin Y := N3; M := N1; D := N2; end;
      scdoDMY: begin Y := N3; M := N2; D := N1; end;
      scdoYMD: begin Y := N1; M := N2; D := N3; end;
    end;
    Y := ExpandYear(Y);
  end
  else begin
    Y := CurrentYear;
    if DateOrder = scdoDMY then begin
      D := N1; M := N2;
    end
    else begin
      M := N1; D := N2;
    end;
  end;
  SCScanChar(S, Pos, FormatSettings.DateSeparator);
  SCScanBlanks(S, Pos);
  Result := IsValidDate(Y, M, D) and (Pos > Length(S));
end;

function TscDateEdit.ScanDateStr(const Format, S: string; var D, M, Y: Integer): Boolean;
var
  Pos: Integer;
begin
  ExtractMask(Format, S, 'm', 3, M, -1, 0);
  if M = 0 then ExtractMask(Format, S, 'm', 1, M, -1, 0);
  ExtractMask(Format, S, 'd', 1, D, -1, 1);
  ExtractMask(Format, S, 'y', 1, Y, -1, CurrentYear);
  Y := ExpandYear(Y);
  Result := IsValidDate(Y, M, D);
  if not Result then
  begin
    Pos := 1;
    Result := ScanDate(S, Format, Pos, Y, M, D);
  end;
end;

function TscDateEdit.MyStrToDate(S: String): TDate;
var
  D, M, Y: Integer;
  B: Boolean;
begin
  Result := 0;
  if S <> '' then
  begin
    B := ScanDateStr(DefDateFormat(FourDigitYear), S, D, M, Y);
    if B then
    try
      Result := EncodeDate(Y, M, D);
    except
      Result := 0;
    end;
  end;
end;

function TscDateEdit.MyDateToStr(Date: TDate): String;
begin
  Result := FormatDateTime(DefDateFormat(FourDigitYear), Date);
end;

function TscDateEdit.IsOnlyNumbers;
const
  DateSymbols = '0123456789';
var
  i: Integer;
  S1: String;
begin
  Result := True;
  S1 := DateSymbols;
  S1 := S1 + FBlanksChar;
  for i := 1 to Length(S) do
  begin
    if (Pos(S[i], S1) = 0) and (S[i] <> FormatSettings.DateSeparator)
    then
      begin
        Result := False;
        Break;
      end;
  end;
end;

function TscDateEdit.FourDigitYear: Boolean;
begin
  Result := Pos('YYYY', UpperCase(FormatSettings.ShortDateFormat)) > 0;
end;

function TscDateEdit.GetDateOrder(const DateFormat: string): TscDateOrder;
var
  I: Integer;
begin
  Result := scdoMDY;
  I := 1;
  while I <= Length(DateFormat) do begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'Y': Result := scdoYMD;
      'M': Result := scdoMDY;
      'D': Result := scdoDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
end;


function TscDateEdit.DefDateFormat(FourDigitYear: Boolean): string;
begin
  if FourDigitYear then begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY: Result := 'MM/DD/YYYY';
      scdoDMY: Result := 'DD/MM/YYYY';
      scdoYMD: Result := 'YYYY/MM/DD';
    end;
  end
  else begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY: Result := 'MM/DD/YY';
      scdoDMY: Result := 'DD/MM/YY';
      scdoYMD: Result := 'YY/MM/DD';
    end;
  end;
end;

function TscDateEdit.DefDateMask(BlanksChar: Char; FourDigitYear: Boolean): string;
begin
  if FourDigitYear then begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY, scdoDMY: Result := '!99/99/9999;1;';
      scdoYMD: Result := '!9999/99/99;1;';
    end;
  end
  else begin
    case GetDateOrder(FormatSettings.ShortDateFormat) of
      scdoMDY, scdoDMY: Result := '!99/99/99;1;';
      scdoYMD: Result := '!99/99/99;1;';
    end;
  end;
  if Result <> '' then Result := Result + BlanksChar;
end;

function TscDateEdit.GetDateMask: String;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

procedure TscDateEdit.Loaded;
begin
  inherited;
  EditMask := GetDateMask;
  if FTodayDefault then Date := Now;
end;

procedure TscDateEdit.SetTodayDefault;
begin
  FTodayDefault := Value;
  if FTodayDefault then Date := Now;
end;

function TscDateEdit.GetCalendarFont;
begin
  Result := FMonthCalendar.Font;
end;

procedure TscDateEdit.SetCalendarFont;
begin
  FMonthCalendar.Font.Assign(Value);
end;

function TscDateEdit.GetCalendarWidth: Integer;
begin
  Result := FMonthCalendar.Width;
end;

procedure TscDateEdit.SetCalendarWidth(Value: Integer);
begin
  FMonthCalendar.Width := Value;
end;

function TscDateEdit.GetCalendarHeight: Integer;
begin
  Result := FMonthCalendar.Height;
end;

procedure TscDateEdit.SetCalendarHeight(Value: Integer);
begin
  FMonthCalendar.Height := Value;
end;

function TscDateEdit.GetDate: TDate;
begin
  Result := FMonthCalendar.Date;
end;

procedure TscDateEdit.SetDate(Value: TDate);
begin
  FMonthCalendar.Date := Value;
  StopCheck := True;
  if not (csLoading in ComponentState) or FTodayDefault
  then
    begin
      Text := MyDateToStr(Value);
    end;
  StopCheck := False;
  if Assigned(FOnDateChange) then FOnDateChange(Self);
end;

function TscDateEdit.IsValidText;
var
  D, M, Y: Integer;
  DF: String;
begin
  Result := IsOnlyNumbers(S);
  if Result
  then
    begin
      DF := DefDateFormat(FourDigitYear);
      Result := ScanDateStr(DF, S, D, M, Y);
    end;
end;

procedure TscDateEdit.Change;
begin
  inherited;
  if not StopCheck
  then
    if IsValidText(Text)
    then CheckValidDate;
end;

procedure TscDateEdit.CheckValidDate;
begin
  if FMonthCalendar = nil then Exit;
  FMonthCalendar.Date := MyStrToDate(Text);
  if Assigned(FOnDateChange) then FOnDateChange(Self);
end;

procedure TscDateEdit.CMCancelMode;
begin
 if (Message.Sender <> FMonthCalendar) and (FMonthCalendar <> nil) and
     not FMonthCalendar.ContainsControl(Message.Sender)
 then
   CloseUp(False);
end;

procedure TscDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if not IsCalendarVisible then
  case Key of
    VK_DOWN, VK_RIGHT:
      if ssAlt in Shift then
        DropDown;
  end
  else
  case Key of
    VK_UP, VK_LEFT:
      if ssAlt in Shift then
        CloseUP(False);
  end;
end;

procedure TscDateEdit.WndProc;
var
  Offset: Integer;
begin
  case message.Msg of
    WM_KILLFOCUS:
      begin
        inherited;
        if FMonthCalendar <> nil then
        begin
          if not FMonthCalendar.Visible and FTodayDefault then
          begin
            StopCheck := True;
            Text := MyDateToStr(FMonthCalendar.Date);
            StopCheck := False;
          end
          else if message.wParam <> FMonthCalendar.Handle then
            CloseUp(False);
        end;
      end;

    WM_CHAR:
      if not IsCalendarVisible then
        inherited;

    WM_KEYUP:
      begin
        inherited;
        if IsCalendarVisible then
          case TWMKeyUp(message).CharCode of
            VK_SPACE:
              FMonthCalendar.Date := Now;
          end;
      end;

    WM_KEYDOWN:
      if IsCalendarVisible then
      begin
        Offset := 0;
        case TWMKeyDown(message).CharCode of
          VK_LEFT:
            Offset := -1;
          VK_RIGHT:
            Offset := 1;
          VK_UP:
            Offset := -7;
          VK_DOWN:
            Offset := 7;
          VK_RETURN:
            CloseUp(True);
          VK_ESCAPE:
            CloseUp(False);
          VK_NEXT:
             FMonthCalendar.NextMButtonClick(Self);
          VK_PRIOR:
             FMonthCalendar.PriorMButtonClick(Self);
        end;

        if Offset <> 0 then
          FMonthCalendar.Date := FMonthCalendar.Date + Offset;
      end
      else
        inherited;
  else
    inherited;
  end;
end;

function TscDateEdit.IsCalendarVisible: Boolean;
begin
  Result := (FMonthCalendar <> nil) and FMonthCalendar.Visible;
end;

procedure TscDateEdit.DropDown;
var
  P: TPoint;
  X, Y: Integer;
  WorkArea: TRect;
begin
  if ReadOnly then Exit;

  FDateSelected := False;
  FMonthCalendar.Wallpapers := Self.FCalendarWallpapers;
  FMonthCalendar.WallpaperIndex := Self.FCalendarWallpaperIndex;
  case Self.FCalendarBackgroundStyle  of
    sccasPanel: FMonthCalendar.StyleKind := scpsPanel;
    sccasFormBackground: FMonthCalendar.StyleKind := scpsFormBackground;
    sccasEdit: FMonthCalendar.StyleKind := scpsEdit;
  end;
  if not FTodayDefault
  then
    begin
      FOldDateValue := FMonthCalendar.Date;
      if (FMonthCalendar.Date = 0) and (Pos(' ', Text) <> 0) then FMonthCalendar.Date := Now;
    end;
  P := Parent.ClientToScreen(Point(Left, Top));
  X := P.X;
  Y := P.Y + Height;
  WorkArea := Screen.MonitorFromWindow(Handle).WorkAreaRect;
  if Y + FMonthCalendar.Height > WorkArea.Bottom then
    Y := Y - Height - FMonthCalendar.Height;
  if X + FMonthCalendar.Width > WorkArea.Right then
    Dec(X, X + FMonthCalendar.Width - WorkArea.Right);
  if X < WorkArea.Left then
    X := WorkArea.Left;
  SetWindowPos(FMonthCalendar.Handle, HWND_TOP, X, Y,
   0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FMonthCalendar.Visible := True;
  SC_HookMouseMessages(Self);
end;

procedure TscDateEdit.WMMouseHookCancelMode(var Message: TMessage);
begin
  if (Message.wParam <> Handle) and
     (Message.wParam <> FMonthCalendar.Handle) and
     (GetParent(Message.wParam) <> FMonthCalendar.Handle) and
     not FMonthCalendar.FMenuPopup
  then
    CloseUp(False);
end;

procedure TscDateEdit.CloseUp(AcceptValue: Boolean);
begin
  SC_UnHookMouseMessages;
  if (FMonthCalendar <> nil) and FMonthCalendar.Visible
  then
    begin
      SetWindowPos(FMonthCalendar.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
      FMonthCalendar.Visible := False;
      if AcceptValue
      then
        begin
          StopCheck := True;
          Text := MyDateToStr(FMonthCalendar.Date);
          if Assigned(FOnDateChange) then FOnDateChange(Self);
          StopCheck := False;
        end
      else
        if not FTodayDefault then FMonthCalendar.Date := FOldDateValue;
   end;
end;

procedure TscDateEdit.ButtonClick(Sender: TObject);
begin
  if FMonthCalendar.Visible
  then
    CloseUp(False)
  else
    DropDown;
end;

procedure TscDateEdit.CalendarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDateSelected then CloseUp(True);
end;

procedure TscDateEdit.CalendarClick;
begin
  FDateSelected := True;
end;

function TscDateEdit.GetFirstDayOfWeek: TscDaysOfWeek;
begin
  Result := FMonthCalendar.FirstDayOfWeek;
end;

procedure TscDateEdit.SetFirstDayOfWeek(Value: TscDaysOfWeek);
begin
  FMonthCalendar.FirstDayOfWeek := Value;
end;

end.
