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

unit scHint;

{$I scdefine.inc}

interface
  uses Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
    Vcl.Controls, Vcl.Graphics, Vcl.Themes, Vcl.ImgList, Vcl.Menus, Vcl.Forms,
    Vcl.ExtCtrls;

type

 {$IFNDEF VER310_UP}
  TscCustomHintWindow = class(TCustomHintWindow)
  protected
    procedure CheckPosition;
    procedure WndProc(var Message: TMessage); override;
  public
    FShowAtCursor: Boolean;
    procedure PositionAt(Point: TPoint); overload;
    procedure PositionAt(Rect: TRect); overload;
    procedure PositionAtCursor;
  end;
  {$ENDIF}

  TscBalloonHint = class(TBalloonHint){$IFDEF VER310_UP};{$ENDIF}
  {$IFNDEF VER310_UP}
  public
    procedure ShowHint; overload;
    procedure ShowHint(Point: TPoint); overload;
    procedure ShowHint(Rect: TRect); overload;
    procedure ShowHint(Control: TControl); overload;
  end;
  {$ENDIF}

  TscHint = class;
  TscHintWindow = class(THintWindow)
  private
    FOldAlphaBlend: Boolean;
    FAnimationStep: Integer;
    FAnimationAlpha: Integer;
    procedure CalcHintSize(Cnvs: TCanvas; S: String; var W, H: Integer);
    procedure CalcHintSizeEx(Cnvs: TCanvas; AHint, AHintTitle: String;
      AImageIndex: Integer; AImageList: TCustomImageList;
      var W, H: Integer);
    procedure CheckText(var S: String);
  protected
    FHint: TscHint;
    AppHint: Boolean;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure PaintEx;
    function FindHintComponent: TscHint;
  public
    FExtendedStyle: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintFromRect(ARect: TRect; const AHint: string; ATop: Boolean);
    procedure ActivateHintEx(Rect: TRect;
      const AHintTitle, AHint: string; AImageIndex: Integer; AImageList: TCustomImageList);
  end;

  TscHint = class(TComponent)
  private
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FAlphaBlendAnimation: Boolean;
    HintTimer: TTimer;
    HintText: String;
    FLineSeparator: String;
    FromRect: TRect;
    Top: Boolean;
    HW: TscHintWindow;
    FActiveForApplication: Boolean;
    procedure SetActiveForApplication(Value: Boolean);
    procedure HintTime1(Sender: TObject);
    procedure HintTime1x(Sender: TObject);
    procedure HintTimeEx1(Sender: TObject);
    procedure HintTime2(Sender: TObject);
  protected
    FHintTitle: String;
    FHintImageIndex: Integer;
    FHintImageList: TCustomImageList;
    FCursorHeight: Integer;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(P: TPoint; const AHint: string); overload;
    procedure ActivateHint(const AHint: string); overload;
    procedure ActivateHint(AFromRect: TRect; const AHint: string; ATop: Boolean); overload;
    procedure ActivateHintEx(P: TPoint;
      const AHintTitle, AHint: string;
      AImageIndex: Integer; AImageList: TCustomImageList); overload;
    procedure ActivateHintEx(const AHintTitle, AHint: string;
     AImageIndex: Integer; AImageList: TCustomImageList); overload;
    function IsVisible: Boolean;
    procedure HideHint;
  published
    property ActiveForApplication: Boolean
      read FActiveForApplication write SetActiveForApplication;
    property LineSeparator: String read FLineSeparator write FLineSeparator;
    property AlphaBlend: Boolean read FAlphaBlend write FAlphaBlend;
    property AlphaBlendValue: Byte read FAlphaBlendValue write FAlphaBlendValue;
    property AlphaBlendAnimation: Boolean
      read FAlphaBlendAnimation write FAlphaBlendAnimation;
  end;

implementation
  uses System.UITypes, System.Math, Winapi.UxTheme, Vcl.GraphUtil,
    scDrawUtils;

const
  AnimationInterval = 30;

{$IFNDEF VER310_UP}
type
  TscHintWindowHelper = class Helper for TCustomHintWindow
  protected
    procedure _SetTitle(Value: String);
    procedure _SetDescription(Value: String);
    procedure _SetImageIndex(Value: Integer);
    procedure _SetPopAbove(Value: Boolean);
  end;

  TscHintHelper = class Helper for TCustomHint
  protected
    procedure _SetShow(Value: Boolean);
    procedure _SetWorkComplete(Value: Boolean);
    function _AnimateThread: TCustomHintShowHideThread;
    procedure _SetAnimateThread(Value: TCustomHintShowHideThread);
    procedure _SetLatestHintControl(Value: TControl);
  end;

  TscHintAnimationThread = class Helper for TCustomHintShowHideThread
  protected
    procedure _QueHintWindow(Value: TCustomHintWindow);
  end;

procedure TscHintWindowHelper._SetPopAbove(Value: Boolean);
begin
  Self.FPopAbove := Value;
end;

procedure TscHintWindowHelper._SetTitle(Value: String);
begin
  Self.FTitle := Value;
end;

procedure TscHintWindowHelper._SetDescription(Value: String);
begin
  Self.FDescription := Value;
end;

procedure TscHintWindowHelper._SetImageIndex(Value: Integer);
begin
  Self.FImageIndex := Value;
end;

procedure TscHintHelper._SetShow(Value: Boolean);
begin
  Self.FShow := Value;
end;

procedure TscHintHelper._SetWorkComplete(Value: Boolean);
begin
  Self.FWorkComplete := Value;
end;

function TscHintHelper._AnimateThread: TCustomHintShowHideThread;
begin
  Result := Self.FAnimateThread;
end;

procedure TscHintHelper._SetAnimateThread(Value: TCustomHintShowHideThread);
begin
  Self.FAnimateThread := Value;
end;

procedure TscHintHelper._SetLatestHintControl(Value: TControl);
begin
  Self.FLatestHintControl := Value;
end;

procedure TscHintAnimationThread._QueHintWindow(Value: TCustomHintWindow);
begin
  Self.QueHintWindow(Value);
end;

procedure TscCustomHintWindow.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SHOWWINDOW:
    begin
      if FShowAtCursor and (Message.wParam > 0) then
        PositionAtCursor
      else
        FShowAtCursor := False;
    end;
  end;
  inherited;
end;

procedure TscCustomHintWindow.CheckPosition;
var
  R: TRect;
  X, Y: Integer;
  P: TPoint;
begin
  GetCursorPos(P);
  R := Screen.MonitorFromPoint(P).WorkareaRect;
  if R.Width = 0 then
    R := Screen.WorkAreaRect;
  X := Left + Width;
  Y := Top + Height;
  // left position
  if X > R.Right then
    Left := Left - (X - R.Right);
  if Left < R.Left then Left := R.Left;
  // top position
  if Self.FShowAtCursor then
  begin
    if Y > R.Bottom then
      Top := P.Y - Height;
  end
  else
  begin
    if Y > R.Bottom then
      Top := Top - (Y - R.Bottom);
  end;
  if Top < R.Top then Top := R.Top;
end;

procedure TscCustomHintWindow.PositionAt(Point: TPoint);
begin
  if FShowAtCursor then
  begin
    AutoSize;
    Left := Point.X;
    Top := Point.Y;
  end
  else
    PositionAt(Rect(Point.X, Point.Y, Point.X, Point.Y));
end;

procedure TscCustomHintWindow.PositionAt(Rect: TRect);
var
  B: Boolean;
begin
    AutoSize;

  Top := Rect.Bottom;
  Left := Rect.Left + RectWidth(Rect) div 2 - (Width) div 2;

  if HintParent.Style = bhsBalloon then
    Left := Left + cBalloonStemHeight;

  B := Top > Screen.Height div 2;
  _SetPopAbove(B);
  if B then
    Top := Top - Height - RectHeight(Rect);

  CheckPosition;
end;

procedure TscCustomHintWindow.PositionAtCursor;
var
  Pos: TPoint;
  Offset: Integer;
begin
  Offset := scDrawUtils.GetCursorHeightMargin;
  GetCursorPos(Pos);
  Pos.Y := Pos.Y + Offset;
  FShowAtCursor := True;
  PositionAt(Pos);
  CheckPosition;
end;

procedure TscBalloonHint.ShowHint;
var
  Hint: TscCustomHintWindow;
  HT: TCustomHintShowHideThread;
begin
  _SetShow(True);
  Hint := TscCustomHintWindow.Create(nil);
  Hint.HintParent := Self;
  Hint.HandleNeeded;
  Hint._SetTitle(Title);
  Hint._SetDescription(Description);
  Hint._SetImageIndex(ImageIndex);
  Hint.PositionAtCursor;
  _SetWorkComplete(False);
  if _AnimateThread = nil then
  begin
    HT := TCustomHintShowHideThread.Create(Hint, Self);
    _SetAnimateThread(HT);
  end
  else
  begin
    _AnimateThread._QueHintWindow(Hint);
    _AnimateThread.ResumeWork;
  end;
end;

procedure TscBalloonHint.ShowHint(Point: TPoint);
begin
  ShowHint(Rect(Point.X, Point.Y, Point.X, Point.Y));
end;

procedure TscBalloonHint.ShowHint(Rect: TRect);
var
  Hint: TscCustomHintWindow;
  HT: TCustomHintShowHideThread;
begin
  _SetShow(True);
  Hint := TscCustomHintWindow.Create(nil);
  Hint.HintParent := Self;
  Hint.HandleNeeded;
  Hint._SetTitle(Title);
  Hint._SetDescription(Description);
  Hint._SetImageIndex(ImageIndex);
  Hint.PositionAt(Rect);

  _SetWorkComplete(False);
  if _AnimateThread = nil then
  begin
    HT := TCustomHintShowHideThread.Create(Hint, Self);
    _SetAnimateThread(HT);
  end
  else
  begin
    _AnimateThread._QueHintWindow(Hint);
    _AnimateThread.ResumeWork;
  end;
end;

procedure TscBalloonHint.ShowHint(Control: TControl);
var
  Pos: TPoint;
  Index: Integer;
begin
  if Control = nil then
    Exit;

  if Control.CustomHint = Self then
  begin
    if Control.Hint = '' then
      Exit;

    Index := AnsiPos('|', Control.Hint); //Do Not Localize
    Title := GetShortHint(Control.Hint);
    if Index <> 0 then
      Description := GetLongHint(Control.Hint)
    else
      Description := '';

    Index := AnsiPos('|', Description); //Do Not Localize
    if Index <> 0 then
    begin
      ImageIndex := StrToInt(Copy(Description, Index + 1, MaxInt));
      Description := Copy(Description, 0, Index - 1);
    end
    else
      ImageIndex := -1;

    ShowHint;
  end
  else
  begin
    Pos := Control.ClientToScreen(Point(0, Control.Height));
    ShowHint(Pos);
  end;
end;

{$ENDIF}

// TscHintWindow
constructor TscHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldAlphaBlend := False;
  AppHint := False;
end;

destructor TscHintWindow.Destroy;
begin
  inherited Destroy;
end;

function TscHintWindow.FindHintComponent;
var
  i: Integer;
begin
  Result := nil;
  if (Application.MainForm <> nil) and
     (Application.MainForm.ComponentCount > 0)
  then
    with Application.MainForm do
      for i := 0 to ComponentCount - 1 do
       if (Components[i] is TscHint) and
          (TscHint(Components[i]).ActiveForApplication)
       then
         begin
           Result := TscHint(Components[i]);
           Break;
         end;
end;

procedure TscHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style - WS_BORDER;
end;

procedure TscHintWindow.WMNCPaint(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TscHintWindow.CheckText(var S: String);
var
  I: Integer;
begin
  while Pos(FHint.LineSeparator, S) <> 0 do
  begin
    I := Pos(FHint.LineSeparator, S);
    Delete(S, I, Length(FHint.LineSeparator));
    Insert(#10, S, I);
    Insert(#13, S, I + 1);
  end;
end;

procedure TscHintWindow.ActivateHintEx(Rect: TRect;
   const AHintTitle, AHint: string; AImageIndex: Integer;
   AImageList: TCustomImageList);
var
  HintWidth, HintHeight: Integer;
  ABV: Integer;
  S: String;
  WorkArea: TRect;
  {$IFDEF VER300_UP}
  M, D: Integer;
  {$ENDIF}
begin
  FExtendedStyle := True;
  if FHint = nil then Exit;
  //
  if FHint.AlphaBlend or (not FHint.AlphaBlend and FOldAlphaBlend) then
     SetWindowLong(Handle, GWL_EXSTYLE,
     GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);
  //
  FOldAlphaBlend := FHint.AlphaBlend;
  // check font
  Canvas.Font := Screen.HintFont;
  {$IFDEF VER300_UP}
  if Screen.ActiveCustomForm <> nil then
  begin
    D := Screen.PixelsPerInch;
    M := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).PixelsPerInch;
    Canvas.Font.Height := Muldiv(Canvas.Font.Height, M, D);
  end;
  {$ENDIF}
  //
  S := AHint;
  CheckText(S);
  Caption := S;
  CalcHintSizeEx(Canvas, Caption, AHintTitle, AImageIndex, AImageList,
    HintWidth, HintHeight);
  Rect.Right := Rect.Left + HintWidth;
  Rect.Bottom := Rect.Top + HIntHeight;
  //
  if (Screen.ActiveCustomForm <> nil)
  then
    begin
      WorkArea := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).WorkareaRect;
      if (Rect.Right > WorkArea.Right) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > WorkArea.Bottom) then OffsetRect(Rect, 0, -HintHeight - 2);
    end
   else
  if (Application.MainForm <> nil) and (Application.MainForm.Visible)
  then
    begin
      WorkArea := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkareaRect;
      if (Rect.Right > WorkArea.Right) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > WorkArea.Bottom) then OffsetRect(Rect, 0, -HintHeight - 2);
    end
  else
    begin
      if (Rect.Right > Screen.Width) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > Screen.Height) then OffsetRect(Rect, 0, -HintHeight - 2);
    end;
  //
  BoundsRect := Rect;
  //
  if FHint.AlphaBlend
  then
  begin
    SetWindowLong(Handle, GWL_EXSTYLE,
      GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    if FHint.AlphaBlendAnimation
    then
      SetLayeredWindowAttributes(Handle, 0, 0, LWA_ALPHA)
    else
      SetLayeredWindowAttributes(Handle, 0, FHint.AlphaBlendValue, LWA_ALPHA);
  end;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0,
    0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
  Visible := True;
  Self.RePaint;
  if FHint.AlphaBlend and FHint.AlphaBlendAnimation
  then
    begin
      FAnimationAlpha := 0;
      ABV := FHint.AlphaBlendValue;
      FAnimationStep := ABV div 7;
      if FAnimationStep = 0 then FAnimationStep := 1;
      SetTimer(Handle, 10, AnimationInterval, nil);
    end;
end;

procedure TscHintWindow.CalcHintSizeEx(Cnvs: TCanvas; AHint, AHintTitle: String;
             AImageIndex: Integer; AImageList: TCustomImageList;
             var W, H: Integer);
var
  R: TRect;
  TH: Integer;
  FS: TFontStyles;
begin
  R := Rect(0, 0, 0, 0);
  DrawText(Cnvs.Handle, PChar(AHint), -1, R, DT_CALCRECT or DT_LEFT or DT_NOPREFIX);
  W := RectWidth(R);
  H := RectHeight(R);
  TH := 0;
  if AHintTitle <> ''
  then
    begin
      R := Rect(0, 0, 0, 0);
      FS := Cnvs.Font.Style;
      Cnvs.Font.Style := Cnvs.Font.Style + [fsBold];
      DrawText(Cnvs.Handle, PChar(AHintTitle), -1, R, DT_CALCRECT or DT_LEFT or DT_NOPREFIX);
      Cnvs.Font.Style := FS;
      H := H + RectHeight(R) + 10;
      if RectWidth(R) > W then
        W := RectWidth(R);
      TH := RectHeight(R);
    end;
  if (AImageList <> nil) and (AImageIndex >= 0) and (AImageIndex < AImageList.Count)
  then
    begin
      W := W + AImageList.Width + 10;
      if AImageList.Height + TH + 5 > H then
        H := AImageList.Height + TH + 5;
    end
  else
    Inc(W, 4);
  Inc(W, 6);
  Inc(H, 4);
end;

procedure TscHintWindow.CalcHintSize(Cnvs: TCanvas; S: String; var W, H: Integer);
var
  R: TRect;
begin
  R := Rect(0, 0, 0, 0);
  DrawText(Cnvs.Handle, PChar(S), -1, R, DT_CALCRECT or DT_LEFT or DT_NOPREFIX);
  W := RectWidth(R);
  H := RectHeight(R);
  Inc(W, 6);
  Inc(H, 4);
end;

procedure TscHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  HintWidth, HintHeight: Integer;
  ABV: Integer;
  S: String;
  WorkArea: TRect;
  {$IFDEF VER300_UP}
  M, D: Integer;
  {$ENDIF}
  P: TPoint;
begin
  FExtendedStyle := False;
  if FHint = nil then
  begin
    FHint := FindHintComponent;
    AppHint := True;
  end;
  if FHint = nil then Exit;
  //
  if AppHint then
  begin
    GetCursorPos(P);
    P.Y := P.Y + GetCursorHeightMargin;
    Rect.Top := P.Y;
  end;
  if not AppHint and ((FHint.FHintTitle <> '') or (FHint.FHintImageList <> nil))
  then
    begin
      ActivateHintEx(Rect, FHint.FHintTitle, AHint,
        FHint.FHintImageIndex, FHint.FHintImageList);
      Exit;
    end;

  if FHint.AlphaBlend or (not FHint.AlphaBlend and FOldAlphaBlend) then
     SetWindowLong(Handle, GWL_EXSTYLE,
       GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);
   FOldAlphaBlend := FHint.AlphaBlend;

  Canvas.Font := Screen.HintFont;
  {$IFDEF VER300_UP}
  if Screen.ActiveCustomForm <> nil then
  begin
    D := Screen.PixelsPerInch;
    M := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).PixelsPerInch;
    Canvas.Font.Height := Muldiv(Canvas.Font.Height, M, D);
  end;
  {$ENDIF}
  S := AHint;
  CheckText(S);
  Caption := S;
  CalcHintSize(Canvas, Caption, HintWidth, HintHeight);
  Rect.Right := Rect.Left + HintWidth;
  Rect.Bottom := Rect.Top + HIntHeight;
  //
  if (Screen.ActiveCustomForm <> nil)
  then
    begin
      WorkArea := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).WorkareaRect;
      if (Rect.Right > WorkArea.Right) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > WorkArea.Bottom) then OffsetRect(Rect, 0, -HintHeight - 2);
    end
   else
  if (Application.MainForm <> nil) and (Application.MainForm.Visible)
  then
    begin
      WorkArea := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkareaRect;
      if (Rect.Right > WorkArea.Right) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > WorkArea.Bottom) then OffsetRect(Rect, 0, -HintHeight - 2);
    end
  else
    begin
      if (Rect.Right > Screen.Width) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > Screen.Height) then OffsetRect(Rect, 0, -HintHeight - 2);
    end;
  //
  BoundsRect := Rect;
  //
  if FHint.AlphaBlend
  then
  begin
    SetWindowLong(Handle, GWL_EXSTYLE,
      GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    if FHint.AlphaBlendAnimation
    then
      SetLayeredWindowAttributes(Handle, 0, 0, LWA_ALPHA)
    else
      SetLayeredWindowAttributes(Handle, 0, FHint.AlphaBlendValue, LWA_ALPHA);
  end;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0,
    0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
  Visible := True;
  Self.RePaint;
  if FHint.AlphaBlend and FHint.AlphaBlendAnimation
  then
    begin
      FAnimationAlpha := 0;
      ABV := FHint.AlphaBlendValue;
      FAnimationStep := ABV div 7;
      if FAnimationStep = 0 then FAnimationStep := 1;
      SetTimer(Handle, 10, AnimationInterval, nil);
    end;
end;

procedure TscHintWindow.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 10 then
  begin
    Inc(FAnimationAlpha, FAnimationStep);
    if FAnimationAlpha > FHint.AlphaBlendValue then
      FAnimationAlpha := FHint.AlphaBlendValue;
    Self.RePaint;
    SetLayeredWindowAttributes(Handle, 0, FAnimationAlpha, LWA_ALPHA);
    if (FAnimationAlpha >= FHint.AlphaBlendValue) or not Visible then
      KillTimer(Handle, 10);
  end;
end;

procedure TscHintWindow.ActivateHintFromRect;
var
  HintWidth, HintHeight: Integer;
  ABV: Integer;
  S: String;
  WorkArea: TRect;
  Rect: TRect;
  {$IFDEF VER300_UP}
  M, D: Integer;
  {$ENDIF}
begin
  if FHint = nil then Exit;
  //
  if FHint.AlphaBlend or (not FHint.AlphaBlend and FOldAlphaBlend) then
     SetWindowLong(Handle, GWL_EXSTYLE,
     GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_LAYERED);
  FOldAlphaBlend := FHint.AlphaBlend;
  //
  Canvas.Font := Screen.HintFont;
  {$IFDEF VER300_UP}
  if Screen.ActiveCustomForm <> nil then
  begin
    D := Screen.PixelsPerInch;
    M := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).PixelsPerInch;
    Canvas.Font.Height := Muldiv(Canvas.Font.Height, M, D);
  end;
  {$ENDIF}
  //
  S := AHint;
  CheckText(S);
  Caption := S;
  CalcHintSize(Canvas, Caption, HintWidth, HintHeight);
  if ATop
  then
    begin
      Rect.Left := ARect.Left + RectWidth(ARect) div 2 - HintWidth div 2;
      Rect.Top := ARect.Top - HintHeight;
    end
  else
    begin
      Rect.Left := ARect.Left + RectWidth(ARect) div 2 - HintWidth div 2;
      Rect.Top := ARect.Bottom;
    end;

  Rect.Right := Rect.Left + HintWidth;
  Rect.Bottom := Rect.Top + HintHeight;

  //
  if (Screen.ActiveCustomForm <> nil)
  then
    begin
      WorkArea := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).WorkareaRect;
      if (Rect.Right > WorkArea.Right) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > WorkArea.Bottom) then OffsetRect(Rect, 0, -HintHeight - 2);
    end
   else
  if (Application.MainForm <> nil) and (Application.MainForm.Visible)
  then
    begin
      WorkArea := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkareaRect;
      if (Rect.Right > WorkArea.Right) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > WorkArea.Bottom) then OffsetRect(Rect, 0, -HintHeight - 2);
    end
  else
    begin
      if (Rect.Right > Screen.Width) then OffsetRect(Rect, -HintWidth - 2, 0);
      if (Rect.Bottom > Screen.Height) then OffsetRect(Rect, 0, -HintHeight - 2);
    end;
  //
  BoundsRect := Rect;
  //
 if FHint.AlphaBlend
  then
  begin
    SetWindowLong(Handle, GWL_EXSTYLE,
      GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    if FHint.AlphaBlendAnimation
    then
      SetLayeredWindowAttributes(Handle, 0, 0, LWA_ALPHA)
    else
      SetLayeredWindowAttributes(Handle, 0, FHint.AlphaBlendValue, LWA_ALPHA);
  end;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0,
    0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
  Visible := True;
  Self.RePaint;
  if FHint.AlphaBlend and FHint.AlphaBlendAnimation
  then
    begin
      FAnimationAlpha := 0;
      ABV := FHint.AlphaBlendValue;
      FAnimationStep := ABV div 7;
      if FAnimationStep = 0 then FAnimationStep := 1;
      SetTimer(Handle, 10, AnimationInterval, nil);
    end;
end;

procedure TscHintWindow.PaintEx;
var
  Buffer: TBitmap;
  R, ClipRect, ClRect: TRect;
  LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  LGradientStart, LGradientEnd, LTextColor: TColor;
  W, H, X, Y, OffsetX, OffsetY: Integer;
  Details: TThemedElementDetails;
  {$IFDEF VER300_UP}
  M, D: Integer;
  {$ENDIF}
begin
  R := ClientRect;
  Buffer := TBitmap.Create;
  Buffer.Width := R.Width;
  Buffer.Height := R.Height;
  // draw client
  ClRect := R;
  LStyle := StyleServices;
  LTextColor := Screen.HintFont.Color;
  Buffer.Canvas.Font := Screen.HintFont;
  {$IFDEF VER300_UP}
  if Screen.ActiveCustomForm <> nil then
  begin
    D := Screen.PixelsPerInch;
    M := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).PixelsPerInch;
    Buffer.Canvas.Font.Height := Muldiv(Buffer.Canvas.Font.Height, M, D);
  end;
  {$ENDIF}
  if LStyle.Enabled then
  begin
    ClipRect := R;
    InflateRect(R, 4, 4);
    if TOSVersion.Check(6) and LStyle.IsSystemStyle then
    begin
      LStyle.DrawElement(Buffer.Canvas.Handle,
        LStyle.GetElementDetails(tttStandardNormal), R, ClipRect);
    end
    else
    begin
      LDetails := LStyle.GetElementDetails(thHintNormal);
      if LStyle.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
        LGradientStart := LColor
      else
        LGradientStart := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
        LGradientEnd := LColor
      else
        LGradientEnd := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
        LTextColor := LColor
      else
        LTextColor := Screen.HintFont.Color;
      GradientFillCanvas(Buffer.Canvas, LGradientStart, LGradientEnd, R, gdVertical);
    end;
    R := ClipRect;
  end;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Buffer.Canvas.Font.Color := LTextColor;
  Buffer.Canvas.Brush.Style := bsClear;
  // draw title
  OffsetY := 0;
  OffsetX := 0;
  if FHint.FHintTitle <> '' then
  begin
    R := Rect(0, 0, 0, 0);
    Buffer.Canvas.Font.Style := [fsBold];
    DrawText(Buffer.Canvas.Handle, PChar(FHint.FHintTitle), -1, R,
      DT_CALCRECT or DT_LEFT or DT_NOPREFIX);
    W := R.Width;
    H := R.Height;
    X := ClRect.Left + 5;
    Y := ClRect.Top + 3;
    R := Rect(X, Y, X + W, Y + H);
    DrawText(Buffer.Canvas.Handle, PChar(FHint.FHintTitle), -1, R, DT_LEFT or DT_NOPREFIX);
    Buffer.Canvas.Font.Style := [];
    OffsetY := H + 7;
  end;
  Inc(ClRect.Top, OffsetY);
  // draw image
  if (FHint.FHintImageList <> nil) and (FHint.FHintImageIndex >= 0) and
     (FHint.FHintImageIndex < FHint.FHintImageList.Count) then
  begin
    X := ClRect.Left + 5;
    Y := ClRect.Top + ClRect.Height div 2 -
         FHint.FHintImageList.Height div 2;
    FHint.FHintImageList.Draw(Buffer.Canvas,
         X, Y, FHint.FHintImageIndex);
    OffsetX := X + FHint.FHintImageList.Width + 5;
  end;
  // draw text
  R := Rect(0, 0, 0, 0);
  DrawText(Buffer.Canvas.Handle, PChar(Caption), -1, R,
    DT_CALCRECT or DT_LEFT or DT_NOPREFIX);
  W := R.Width;
  H := R.Height;
  if OffsetX = 0
  then
    X := ClRect.Left + 5
  else
    X := OffsetX;
  Y := ClRect.Top + ClRect.Height div 2 - H div 2;
  if OffsetY > 0 then Dec(Y, 2);

  R := Rect(X, Y, X + W, Y + H);
  DrawText(Buffer.Canvas.Handle,
    PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX);
  // draw border
  R := ClientRect;
  if not StyleServices.Enabled then
    Winapi.Windows.DrawEdge(Buffer.Canvas.Handle, R, BDR_RAISEDOUTER, BF_RECT)
  else
  begin
    Details := StyleServices.GetElementDetails(twWindowRoot);
    StyleServices.DrawEdge(Buffer.Canvas.Handle, Details, R, [eeRaisedOuter], [efRect]);
  end;
  Canvas.Draw(0, 0, Buffer);
  Buffer.Free;
end;

procedure TscHintWindow.Paint;
var
  Buffer: TBitmap;
  R, ClipRect: TRect;
  LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  LGradientStart, LGradientEnd, LTextColor: TColor;
  Details: TThemedElementDetails;
  {$IFDEF VER300_UP}
  M, D: Integer;
  {$ENDIF}
begin
  if FExtendedStyle then
  begin
    PaintEx;
    Exit;
  end;
  R := ClientRect;
  Buffer := TBitmap.Create;
  Buffer.Width := R.Width;
  Buffer.Height := R.Height;
  // draw client
  LStyle := StyleServices;
  LTextColor := Screen.HintFont.Color;
  Buffer.Canvas.Font := Screen.HintFont;
  {$IFDEF VER300_UP}
  if Screen.ActiveCustomForm <> nil then
  begin
    D := Screen.PixelsPerInch;
    M := Screen.MonitorFromWindow(Screen.ActiveCustomForm.Handle).PixelsPerInch;
    Buffer.Canvas.Font.Height := Muldiv(Buffer.Canvas.Font.Height, M, D);
  end;
  {$ENDIF}
  if LStyle.Enabled then
  begin
    ClipRect := R;
    InflateRect(R, 4, 4);
    if TOSVersion.Check(6) and LStyle.IsSystemStyle then
    begin
      LStyle.DrawElement(Buffer.Canvas.Handle,
        LStyle.GetElementDetails(tttStandardNormal), R, ClipRect);
    end
    else
    begin
      LDetails := LStyle.GetElementDetails(thHintNormal);
      if LStyle.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
        LGradientStart := LColor
      else
        LGradientStart := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
        LGradientEnd := LColor
      else
        LGradientEnd := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
        LTextColor := LColor
      else
        LTextColor := Screen.HintFont.Color;
      GradientFillCanvas(Buffer.Canvas, LGradientStart, LGradientEnd, R, gdVertical);
    end;
    R := ClipRect;
  end;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Buffer.Canvas.Font.Color := LTextColor;
  Buffer.Canvas.Brush.Style := bsClear;
  DrawText(Buffer.Canvas.Handle, Caption, -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  // draw border
  R := ClientRect;
  if not StyleServices.Enabled then
    Winapi.Windows.DrawEdge(Buffer.Canvas.Handle, R, BDR_RAISEDOUTER, BF_RECT)
  else
  begin
    Details := StyleServices.GetElementDetails(twWindowRoot);
    StyleServices.DrawEdge(Buffer.Canvas.Handle, Details, R, [eeRaisedOuter], [efRect]);
  end;
  Canvas.Draw(0, 0, Buffer);
  Buffer.Free;
end;

procedure TscHintWindow.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

// TscHint

constructor TscHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveForApplication := False;
  FAlphaBlendAnimation := False;
  FLineSeparator := '@@';
  FAlphaBlend := False;
  FAlphaBlendValue := 255;
  HintTimer := nil;
  HW := TscHintWindow.Create(Self);
  HW.FHint := Self;
  HW.Visible := False;
  FCursorHeight := 0;
end;

destructor TscHint.Destroy;
begin
  HW.Free;
  if HintTimer <> nil then HintTimer.Free;
  inherited Destroy;
end;

procedure TscHint.SetActiveForApplication(Value: Boolean);
begin
  FActiveForApplication := Value;
  if FActiveForApplication and not (csDesigning in ComponentState) then
    HintWindowClass := TscHintWindow;
end;

procedure TscHint.Notification;
begin
  inherited Notification(AComponent, Operation);
end;

procedure TscHint.HintTimeEx1(Sender: TObject);
var
  R: TRect;
  P: TPoint;
begin
  if HintTimer = nil then Exit;
  GetCursorPos(P);
  FCursorHeight := GetCursorHeightMargin;
  P.Y := P.Y + FCursorHeight;
  R := Rect(P.X, P.Y, P.X, P.Y);
  HW.ActivateHintEx(R, FHintTitle, HintText, FHintImageIndex, FHintImageList);
  HW.Visible := True;
  HintTimer.Enabled := False;
  HintTimer.Interval := Application.HintHidePause;
  HintTimer.OnTimer := HintTime2;
  HintTimer.Enabled := True;
end;

procedure TscHint.ActivateHintEx(P: TPoint;
      const AHintTitle, AHint: string;
      AImageIndex: Integer; AImageList: TCustomImageList);
var
  R: TRect;
begin
  R := Rect(P.X, P.Y, P.X, P.Y);
  HW.ActivateHintEx(R, AHintTitle, AHint, AImageIndex, AImageList);
  HW.Visible := True;
end;

procedure TscHint.ActivateHintEx(const AHintTitle, AHint: string;
   AImageIndex: Integer; AImageList: TCustomImageList);
begin
  if HintTimer <> nil
  then
    begin
      HintTimer.Enabled := False;
      HintTimer.Free;
      HintTimer := nil;
    end;
  FHintTitle := AHintTitle;
  FHintImageIndex := AImageIndex;
  FHintImageList := AImageList;
  HintText := AHint;
  HintTimer := TTimer.Create(Self);
  HintTimer.Enabled := False;
  HintTimer.Interval := Application.HintPause;
  HintTimer.OnTimer := HintTimeEx1;
  HintTimer.Enabled := True;
end;

procedure TscHint.HintTime1(Sender: TObject);
var
  R: TRect;
  P: TPoint;
begin
  GetCursorPos(P);
  FCursorHeight := GetCursorHeightMargin;
  P.Y := P.Y + FCursorHeight;
  R := Rect(P.X, P.Y, P.X, P.Y);
  HW.ActivateHint(R, HintText);
  HW.Visible := True;
  HintTimer.Enabled := False;
  HintTimer.Interval := Application.HintHidePause;
  HintTimer.OnTimer := HintTime2;
  HintTimer.Enabled := True;
end;

procedure TscHint.HintTime1x(Sender: TObject);
begin
  HW.ActivateHintFromRect(FromRect, HintText, Top);
  HW.Visible := True;
  HintTimer.Enabled := False;
  HintTimer.Interval := Application.HintHidePause;
  HintTimer.OnTimer := HintTime2;
  HintTimer.Enabled := True;
end;

procedure TscHint.HintTime2(Sender: TObject);
begin
  HideHint;
end;

procedure TscHint.ActivateHint(AFromRect: TRect; const AHint: string; ATop: Boolean);
begin
  FromRect := AFromRect;
  Top := ATop;
  if HintTimer <> nil then HintTimer.Free;
  HintText := AHint;
  Self.FHintTitle := '';
  Self.FHintImageList := nil;
  Self.FHintImageIndex := 0;
  HintTimer := TTimer.Create(Self);
  HintTimer.Enabled := False;
  HintTimer.Interval := Application.HintPause;
  HintTimer.OnTimer := HintTime1x;
  HintTimer.Enabled := True;
end;

procedure TscHint.ActivateHint(const AHint: string);
begin
  if HintTimer <> nil then HintTimer.Free;
  HintText := AHint;
  Self.FHintTitle := '';
  Self.FHintImageList := nil;
  Self.FHintImageIndex := 0;
  HintTimer := TTimer.Create(Self);
  HintTimer.Enabled := False;
  HintTimer.Interval := Application.HintPause;
  HintTimer.OnTimer := HintTime1;
  HintTimer.Enabled := True;
end;

procedure TscHint.ActivateHint(P: TPoint; const AHint: string);
var
  R: TRect;
begin
  Self.FHintTitle := '';
  Self.FHintImageList := nil;
  Self.FHintImageIndex := 0;
  R := Rect(P.X, P.Y, P.X, P.Y);
  HW.ActivateHint(R, AHint);
  HW.Visible := True;
end;

function TscHint.IsVisible: Boolean;
begin
  Result := HW.Visible;
end;

procedure TscHint.HideHint;
begin
  if HintTimer <> nil
  then
    begin
      HintTimer.Enabled := False;
      HintTimer.Free;
      HintTimer := nil;
    end;
  if HW.Visible
  then
    begin
      HW.Visible := False;
      SetWindowPos(HW.Handle, HWND_TOPMOST, 0, 0, 0,
        0, SWP_HideWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
      Self.FHintTitle := '';
      Self.FHintImageList := nil;
      Self.FHintImageIndex := 0;
    end;
end;

end.

