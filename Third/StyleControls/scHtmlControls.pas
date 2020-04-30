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

unit scHtmlControls;

{$I scdefine.inc}

interface

uses
  System.Classes, System.UITypes, System.Types, System.SysUtils,
  Winapi.Windows, Winapi.Messages,
  Vcl.Controls, Vcl.Graphics, scControls, scDrawUtils;

type
  TOnLinkClick = procedure (Sender: TObject; LinkIndex: Integer;
    LinkText: String; LinkURL: String) of object;

  TscHTMLLabel = class(TscCustomControl)
  private
    FLinkFontNormal: TFont;
    FLinkFontHover: TFont;
    FAutoSizeWidth: Boolean;
    FAutoSizeHeight: Boolean;
    FTextAlignment: TAlignment;
    FTextHeight: Integer;
    FMaxWidth: Integer;
    FParsingText: Boolean;
    FBuildingLines: Boolean;
    FRebuildLines: Boolean;
    FOnHeightChanged: TNotifyEvent;
    FOnWidthChanged: TNotifyEvent;
    FOnLinkClick: TOnLinkClick;
    FLines: TList;
    FLinks: TList;
    FHtmlObjects: TList;
    FMouseDownMove: Boolean;
    FMouseDownIndex: Integer;
    FMouseWasDown: Boolean;
    FAutoExecuteURL: Boolean;
    procedure SetAutoSizeWidth(Value: Boolean);
    procedure SetAutoSizeHeight(Value: Boolean);
    procedure SetTextAlignment(Value: TAlignment);
    procedure SetMaxWidth(Value: Integer);
    procedure DoFontChange(Sender: TObject);
    procedure SetLinkFontNormal(Value: TFont);
    procedure SetLinkFontHover(Value: TFont);
    procedure SetLHeight(Value: Integer);
    function MouseInLink(LinkID: Integer): Boolean;
    function GetLinkText(LinkID: Integer): String;
    function GetLinkURL(LinkID: Integer): String;
    procedure ParseText;
    procedure BuildHtmlLines;
    procedure TextToHtmlObjects;
  protected
    procedure Loaded; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseLeave;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Draw(ACanvas: TCanvas; ACtrlState: TscsCtrlState); override;
  published
    property Align;
    property AutoSizeHeight: Boolean
      read FAutoSizeHeight write SetAutoSizeHeight;
    property AutoSizeWidth: Boolean
      read FAutoSizeWidth write SetAutoSizeWidth;
    property AutoExecuteURL: Boolean
      read FAutoExecuteURL write FAutoExecuteURL;
    property Caption;
    property Color;
    property Font;
    property LinkFontNormal: TFont read FLinkFontNormal write SetLinkFontNormal;
    property LinkFontHover: TFont read FLinkFontHover write SetLinkFontHover;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property ParentColor;
    property ParentFont;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment;
    property TextHeight: Integer read FTextHeight;
    property StyleElements;
    property Top;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnHeightChanged: TNotifyEvent read FOnHeightChanged write FOnHeightChanged;
    property OnLinkClick: TOnLinkClick read FOnLinkClick write FOnLinkClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnWidthChanged: TNotifyEvent read FOnWidthChanged write FOnWidthChanged;
  end;

implementation
  uses WinApi.ShellAPI;

type
  TTokenKind = (tkUnknow, tkText, tkBOn, tkBOff, tkIOn, tkIOff, tkUOn,
    tkUOff, tkSOn, tkSOff, tkFCOn, tkFSOn, tkFOn, tkFOff,
    tkAOn, tkAOff, tkBreak, tkBCOn);

  TTokenInfo = record
    Kind: TTokenKind;
    Text: String;
    Value: Integer;
    Url: String;
  end;

  THtmlObjectInfo = class
  protected
    FText: String;
    FRect: TRect;
    FFontStyle: TFontStyles;
    FLinkID: Integer;
    FLinkUrl: String;
    FFontColor: TColor;
    FBackColor: TColor;
    FSize: Integer;
    FWordHeight: Integer;
    FWordWidth: Integer;
  public
    constructor Create(AText: String;
      AFontStyle: TFontStyles; AFontColor: TColor;
      AFontSize: Integer; ABackColor: TColor; ALinkID: Integer;
      ALinkURL: String);
    procedure SetWidth(XPos: Integer; TextRC: TRect);
    procedure AdjustWidth(XPos: Integer);
    procedure SetLineHeight(LineTop, LineHeight: Integer);
    procedure SetWOffset(Offset: Integer);
    procedure Add(ws: String);
  end;

  TBreakInfo = class (THtmlObjectInfo)
    constructor Create;
  end;

  TLineInfo = class
    FHtmlObjects: TList;
    FLineWidth: Integer;
    FLineHeight: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TLinkInfo = class
    FRect: TRect;
    FMouseOver: Boolean;
    FLinkID: Integer;
    procedure SetData(ARect: TRect; ALinkID: Integer);
    constructor Create(ALinkID: Integer);
  end;

  TTokenState = (tsStart, tsTagStart, tsTagFont);

constructor THtmlObjectInfo.Create(AText: String; AFontStyle: TFontStyles; AFontColor: TColor;
   AFontSize: Integer; ABackColor: TColor; ALinkID: Integer; ALinkUrl: String);
begin
  FText := AText;
  FRect := Rect(0, 0, 0, 0);
  FFontStyle := AFontStyle;
  FLinkID := ALinkID;
  FLinkURL := ALinkURL;
  FFontColor := AFontColor;
  FSize := AFontSize;
  FBackColor := ABackColor;
end;

procedure THtmlObjectInfo.SetWidth(XPos: Integer; TextRC: TRect);
begin
  FWordWidth := TextRC.Right - TextRC.Left;
  FRect.Left := XPos;
  FRect.Right := XPos + FWordWidth;
  FRect.Top := 0;
  FRect.Bottom := TextRC.Bottom;
  FWordHeight := TextRC.Bottom;
end;

procedure THtmlObjectInfo.AdjustWidth(XPos: Integer);
begin
  FWordWidth := FRect.Right - FRect.Left;
  FRect.Left := XPos;
  FRect.Right := XPos + FWordWidth;
  FRect.Top := 0;
end;

procedure THtmlObjectInfo.SetLineHeight(LineTop, LineHeight: Integer);
begin
  FRect.Bottom := LineTop + LineHeight;
  FRect.Top := FRect.Bottom - FWordHeight;
end;

procedure THtmlObjectInfo.SetWOffset(Offset: Integer);
begin
  FRect.Left := FRect.Left + Offset;
  FRect.Right := FRect.Left + FWordWidth;
end;

procedure THtmlObjectInfo.Add(ws: String);
begin
  FText := FText + ws;
end;

constructor TBreakInfo.Create;
begin
  inherited Create('', [], clBlack, 0, clNone, 0, '');
end;

constructor TLineInfo.Create;
begin
  FHtmlObjects := TList.Create;
  FLineWidth := 0;
  FLineHeight := 0;
end;

destructor TLineInfo.Destroy;
begin
  FHtmlObjects.Free;
  inherited;
end;

constructor TLinkInfo.Create(ALinkID: Integer);
begin
  FRect := Rect(0, 0, 0, 0);
  FMouseOver := False;
  FLinkID := ALinkID;
end;

procedure TLinkInfo.SetData(ARect: TRect; ALinkID: Integer);
begin
  FRect := ARect;
  FLinkID := ALinkID;
end;

function EntityToString(S: String): String;
begin
  Result := S;
  case S[1] of
    'a':
      if S = 'amp' then
        Result := '&';
    'b':
      if S = 'bull' then
        Result := UTF8ToString(AnsiString(#$e2#$80#$a2));
    'c':
      if S = 'cent' then
        Result := UTF8ToString(AnsiString(#$C2#$A2))
      else if S = 'copy' then
        Result := UTF8ToString(AnsiString(#$C2#$A9));
    'e':
      if S = 'euro' then
        Result := '€';
    'g':
      if S = 'gt' then
        Result := '>';
    'l':
      if S = 'lt' then
        Result := '<';
    'n':
      if S = 'nbsp' then
        Result := ' '
      else if S = 'ndash' then
        Result := UTF8ToString(AnsiString(#$e2#$80#$93));
    'r':
      if S = 'reg' then
        Result := UTF8ToString(AnsiString(#$C2#$AE));
    't':
      if S = 'trade' then
       Result := UTF8ToString(AnsiString(#$e2#$84#$a2));
  end;
end;

function ReplaceHTMLEntities(S: String): String;
var
  I, J: Integer;
  C1, C2: Char;
  Entity: String;
begin
  if S = '' then Exit;
  I := Pos('&', S);
  if I < 1 then
    Result := S
  else
  begin
    Result := copy(S, 1, I - 1);
    while I <= Length(S) do
    begin
      C1 := S[i];
      if C1 = '&' then
      begin
        J := I + 1;
        while J <= Length(S) do
        begin
          C2 := S[J];
          if C2 = ';' then
          begin
            Entity := Copy(S, I + 1, J - I - 1);
            if Entity <> '' then
              Result := Result + EntityToString(LowerCase(Entity));
            I := J;
            Break;
          end
          else
          if C2 = '&' then
          begin
            Result := Result + Copy(S, I, J - I);
            I := J - 1;
            Break;
          end;
          Inc(J);
        end;
        if J > Length(S) then
        begin
          Result := Result + Copy(S, I, Length(S));
          Exit;
        end;
      end
      else
        Result := Result + C1;
      Inc(I);
    end;
  end;
end;


function GetHtmlToken(Line: String; var Index: Integer; var Token: TTokenInfo; var State: TTokenState): Boolean;
var
  C: Char;
  S, S1, SValue: String;
  I, J: Integer;
  B: Boolean;
begin
  Token.Kind := tkUnKnow;
  Token.Text := '';
  Token.Value := 0;
  while True do
  begin
    if Index > Length(Line) then
    begin
      Result := False;
      Token.Kind := tkUnKnow;
      Exit;
    end;
    C := Line[Index];
    Token.Text := Token.Text + C;
    Inc(Index);
    if (C = '>') then
    begin
      State := tsStart;
      Delete(Token.Text, Length(Token.Text), 1);
    end
    else
    if (C = '<') and (State = tsStart) then
    begin
      State := tsTagStart;
      Dec(Index);
      if Token.Text <> '<' then
      begin
        Delete(Token.Text, Length(Token.Text), 1);
        Token.Kind := tkText;
        Token.Text := ReplaceHTMLEntities(Token.Text);
        Result := True;
        Exit;
      end;
    end
    else
    if (State = tsTagFont) then
    begin
      I := Index - 1;
      B := False;
      repeat
        Inc(I);
        if CharInSet(Line[I], ['B', 'b']) then
        begin
          S1 := Copy(Line, I, 8);
          S1 := LowerCase(S1);
          if S1 = 'bgcolor=' then
          begin
            Inc(I, 8);
            SValue := '';
            J := I;
            repeat
              if Line[J] <> '>' then
                SValue := SValue + Line[J];
              Inc(J);
            until (J >= Length(Line)) or (Line[J] = '>') or (Line[J] = ' ');
            if SValue[1] = '#' then
            begin
              if HexStringToColor(SValue, Token.Value) then
                Token.Kind := tkBCOn;
            end
            else
            if CharInSet(SValue[1], ['0'..'9']) then
            begin
              if DecStringToColor(SValue, Token.Value) then
                Token.Kind := tkBCOn
            end
            else
            begin
              if NameStringToColor(SValue, Token.Value) then
                Token.Kind := tkBCOn;
            end;
            B := True;
            Inc(I, Length(SValue));
          end;
        end
        else
        if CharInSet(Line[I], ['C', 'c']) then
        begin
          S1 := Copy(Line, I, 6);
          S1 := LowerCase(S1);
          if S1 = 'color=' then
          begin
            Inc(I, 6);
            SValue := '';
            J := I;
            repeat
              if Line[J] <> '>' then
                SValue := SValue + Line[J];
              Inc(J);
            until (J >= Length(Line)) or (Line[J] = '>') or (Line[J] = ' ');
            if SValue[1] = '#' then
            begin
              if HexStringToColor(SValue, Token.Value) then
                Token.Kind := tkFCOn;
            end
            else
            if CharInSet(SValue[1], ['0'..'9']) then
            begin
              if DecStringToColor(SValue, Token.Value) then
                Token.Kind := tkFCOn
            end
            else
            begin
              if NameStringToColor(SValue, Token.Value) then
                Token.Kind := tkFCOn;
            end;
            B := True;
            Inc(I, Length(SValue));
          end;
        end
        else
        if CharInSet(Line[I], ['S', 's']) then
        begin
          S1 := Copy(Line, I, 5);
          S1 := LowerCase(S1);
          if S1 = 'size=' then
          begin
            Inc(I, 5);
            SValue := '';
            J := I;
            repeat
              if Line[J] <> '>' then
                SValue := SValue + Line[J];
              Inc(J);
            until (J >= Length(Line)) or (Line[J] = '>') or (Line[J] = ' ');
            if CharInSet(SValue[1], ['0'..'9']) then
            begin
              if DecStringToColor(SValue, Token.Value) then
               Token.Kind := tkFSOn;
            end;
            B := True;
            Inc(I, Length(SValue));
          end;
        end;
      until B or (I >= Length(Line)) or (Line[I] = '>');
      Index := I;
      if (Token.Kind = tkBCOn) and B then
      begin
        Result := True;
        Exit;
      end;
      if (Token.Kind = tkFSOn) and B then
      begin
        Result := True;
        Exit;
      end;
      if (Token.Kind = tkFCOn) and B then
      begin
        Result := True;
        Exit;
      end;
      if (I <= Length(Line)) and (Line[I] = '>') then
      begin
        State := tsStart;
      end;
    end
    else
    if (State = tsTagStart) then
    begin
      Token.Text := '';
      S := Copy(Line, Index, 6);
      if S = '/font>' then
      begin
        State := tsStart;
        Token.Kind := tkFOff;
        Inc(Index, 5);
        Result := True;
        Exit;
      end;
      S := LowerCase(S);
      S := Copy(Line, Index, 4);
      S := LowerCase(S);
      if S = 'font' then
      begin
        Inc(Index, 4);
        State := tsTagFont;
        Token.Kind := tkFOn;
        Result := True;
        Exit;
      end;
      S := Copy(Line, Index, 3);
      S := LowerCase(S);
      if S = 'br>' then
      begin
        Token.Kind := tkBreak;
        State := tsStart;
        Inc(Index, 2);
        Result := True;
        Exit;
      end
      else
      if S = '/b>' then
      begin
        Token.Kind := tkBOff;
        State := tsStart;
        Inc(Index, 2);
        Result := True;
        Exit;
      end
      else
      if S = '/i>' then
      begin
        Token.Kind := tkIOff;
        State := tsStart;
        Inc(Index, 2);
        Result := True;
        Exit;
      end
      else
      if S = '/s>' then
      begin
        Token.Kind := tkSOff;
        State := tsStart;
        Inc(Index, 2);
        Result := True;
        Exit;
      end
      else
      if S = '/u>' then
      begin
        Token.Kind := tkUOff;
        State := tsStart;
        Inc(Index, 2);
        Result := True;
        Exit;
      end
      else
      if S = '/a>' then
      begin
        Token.Kind := tkAOff;
        State := tsStart;
        Inc(Index, 2);
        Result := True;
        Exit;
      end;
      S := Copy(Line, Index, 2);
      S := LowerCase(S);
      if S = 'b>' then
      begin
        Token.Kind := tkBOn;
        State := tsStart;
        Inc(Index);
        Result := True;
        Exit;
      end
      else
      if S = 'i>' then
      begin
        Token.Kind := tkIOn;
        State := tsStart;
        Inc(Index);
        Result := True;
        Exit;
      end
      else
      if S = 's>' then
      begin
        Token.Kind := tkSOn;
        State := tsStart;
        Inc(Index);
        Result := True;
        Exit;
      end
      else
      if S = 'u>' then
      begin
        Token.Kind := tkUOn;
        State := tsStart;
        Inc(Index);
        Result := True;
        Exit;
      end
      else
      if (S = 'a>') then
      begin
        Token.Kind := tkAOn;
        Token.Url := '';
        State := tsStart;
        Inc(Index);
        Result := True;
        Exit;
      end
      else
      if (S = 'a ') then
      begin
        I := Index + 1;
        B := False;
        repeat
          Inc(I);
          S1 := Copy(Line, I, 6);
          S1 := LowerCase(S1);
          if S1 = 'href="' then
          begin
            J := I + 6;
            SValue := '';
            repeat
              SValue := SValue + Line[J];
              Inc(J);
            until (Line[J] = '"') or (J >= Length(Line)) or (Line[J] = '>');
            B := True;
            Index := J;
          end;
        until B or (I >= Length(Line)) or (Line[I] = '>');
        if Line[I] = '>' then Index := I;
        if B then
          Token.Url := SValue;
        Token.Kind := tkAOn;
        State := tsStart;
        Inc(Index);
        Result := True;
        Exit;
      end;
    end
    else
    if C = '>' then
    begin
      Result := True;
      State := tsStart;
      Exit;
    end
    else
    if Index = Length(Line) + 1 then
    begin
      Result := True;
      Token.Kind := tkText;
      State := tsStart;
      Exit;
    end;
  end;
end;

constructor TscHTMLLabel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];

  FAutoExecuteURL := True;

  FLines := TList.Create;
  FHtmlObjects := TList.Create;
  FLinks := TList.Create;

  FTransparentBackground := True;
  FDrawOnBackground := True;

  FLinkFontNormal := TFont.Create;
  FLinkFontNormal.Assign(Font);
  FLinkFontNormal.Color := clHighLight;
  FLinkFontNormal.Style := [];

  FLinkFontHover := TFont.Create;
  FLinkFontHover.Assign(Font);
  FLinkFontHover.Color := clHighLight;
  FLinkFontHover.Style := [fsUnderline];

  TabStop := False;
  FTextHeight := 0;

  FAutoSizeWidth := True;
  FAutoSizeHeight := True;
  FTextAlignment := taLeftJustify;
  FParsingText := False;
  FBuildingLines := False;
  FRebuildLines := False;
  FMaxWidth := 0;

  FLinkFontNormal.OnChange := DoFontChange;
  FLinkFontHover.OnChange := DoFontChange;

  FOnLinkClick := nil;
  FOnHeightChanged := nil;
  FOnWidthChanged := nil;

  FMouseDownMove := False;
  FMouseWasDown := False;
  FMouseDownIndex := -1;

  Width := 100;
  Height := 15;
end;

destructor TscHTMLLabel.Destroy;
var
  I: Integer;
begin
  FLinkFontNormal.Free;
  FLinkFontHover.Free;
  for I := 0 to FLines.Count - 1 do
    if FLines[I] <> nil then
      TLineInfo(FLines[I]).Free;
  FLines.Free;
  for I := 0 to FHtmlObjects.Count - 1 do
    if FHtmlObjects[I] <> nil then
      THtmlObjectInfo(FHtmlObjects[I]).Free;
  FHtmlObjects.Free;
  for I := 0 to FLinks.Count - 1 do
    if FLinks[I] <> nil then
      TLinkInfo(FLinks[I]).Free;
  FLinks.Free;
  inherited;
end;

procedure TscHTMLLabel.ChangeScale(M, D: Integer{$IFDEF VER310_UP}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FLinkFontNormal.Height := MulDiv(FLinkFontNormal.Height, M, D);
  FLinkFontHover.Height := MulDiv(FLinkFontHover.Height, M, D);
  ParseText;
end;

procedure TscHTMLLabel.Loaded;
begin
  inherited;
  ParseText;
end;

procedure TscHTMLLabel.TextToHtmlObjects;
var
  Token: TTokenInfo;
  I, Index: Integer;
  S: String;
  CurrentStyle: TFontStyles;
  FontColorList: TList;
  BackColorList: TList;
  SizeList: TList;
  LinkID, CurLinkID: Integer;
  LinkHtmlObject: Boolean;
  FontSize: Integer;
  FontColor: TColor;
  BackColor: TColor;
  State: TTokenState;
  FontColorChanged, FontSizeChanged, FontBGChanged: Boolean;
begin
   if (csLoading in ComponentState) then Exit;

  for I := 0 to FHtmlObjects.Count - 1 do
   if FHtmlObjects[I] <> nil then
     THtmlObjectInfo(FHtmlObjects[I]).Free;

  FHtmlObjects.Clear;

  S := Caption;
  if S = '' then Exit;

  FontColorList := TList.Create;
  BackColorList := TList.Create;
  SizeList := TList.Create;
  try
    FontColorList.Add(Pointer(Font.Color));
    BackColorList.Add(Pointer(clNone));
    SizeList.Add(Pointer(Font.Size));
    CurLinkID := 0;
    LinkHtmlObject := False;
    FontColorChanged := False;
    FontSizeChanged := False;
    FontBGChanged := False;
    CurrentStyle := Font.Style;
    State := tsStart;
    Token.Url := '';
    Index := 1;
    while GetHtmlToken(S, Index, Token, State) do begin
      if LinkHtmlObject then LinkID := CurLinkID
      else LinkID := 0;
      FontSize := Integer(SizeList[SizeList.Count - 1]);
      FontColor := TColor(FontColorList[FontColorList.Count - 1]);
      BackColor := TColor(BackColorList[BackColorList.Count - 1]);
      case Token.Kind of
        tkText:
          FHtmlObjects.Add(THtmlObjectInfo.Create(Token.Text, CurrentStyle,
            FontColor, FontSize, BackColor, LinkID, Token.URL));
        tkFOn:
        begin
          FontColorChanged := False;
          FontSizeChanged := False;
          FontBGChanged := False;
        end;
        tkFOff:
        begin
          if FontColorChanged then
            if FontColorList.Count > 1 then
              FontColorList.Delete(FontColorList.Count - 1);
          if FontSizeChanged then
            if SizeList.Count > 1 then
              SizeList.Delete(SizeList.Count - 1);
          if FontBGChanged then
            if BackColorList.Count > 1 then
              BackColorList.Delete(BackColorList.Count - 1);
          FontSizeChanged := False;
          FontColorChanged := False;
          FontBGChanged := False;
        end;
        tkAOn:
        begin
          inc(CurLinkID);
          LinkHtmlObject := True;
        end;
        tkAOff:
        begin
          Token.Url := '';
          LinkHtmlObject := False;
        end;
        tkFCOn:
        begin
          FontColorList.Add(Pointer(Token.Value));
          FontColorChanged := True;
        end;
        tkFSOn:
        begin
          SizeList.Add(Pointer(Token.Value));
          FontSizeChanged := True;
        end;
        tkBCOn:
        begin
          BackColorList.Add(Pointer(Token.Value));
          FontBGChanged := True;
        end;
        tkBOn: CurrentStyle := CurrentStyle + [fsBold];
        tkBOff: CurrentStyle := CurrentStyle - [fsBold];
        tkIOn: CurrentStyle := CurrentStyle + [fsItalic];
        tkIOff: CurrentStyle := CurrentStyle - [fsItalic];
        tkUOn: CurrentStyle := CurrentStyle + [fsUnderline];
        tkUOff: CurrentStyle := CurrentStyle - [fsUnderline];
        tkSOn: CurrentStyle := CurrentStyle + [fsStrikeOut];
        tkSOff: CurrentStyle := CurrentStyle - [fsStrikeOut];
        tkBreak: FHtmlObjects.Add(TBreakInfo.Create);
      end;
    end;
  finally
    FontColorList.Free;
    BackColorList.Free;
    SizeList.Free;
  end;
end;

procedure TscHTMLLabel.ParseText;
var
  i: Integer;
begin
  if (csLoading in ComponentState) then Exit;

  if FParsingText then Exit;

  FParsingText := True;
  try
    for i := 0 to FLinks.Count - 1 do if FLinks[i] <> nil then
      TLinkInfo(FLinks[i]).Free;
    FLinks.Clear;

    TextToHtmlObjects;
    for i := 0 to FHtmlObjects.Count - 1 do
    begin
      if THtmlObjectInfo(FHtmlObjects[i]).FLinkID > 0 then
        FLinks.Add(TLinkInfo.Create(THtmlObjectInfo(FHtmlObjects[i]).FLinkID));
    end;
    BuildHtmlLines;
  finally
    FParsingText := False;
  end;
end;

procedure TscHTMLLabel.BuildHtmlLines;
var
  I, J: Integer;
  S: String;
  LineTop: Integer;
  HtmlObjectInfo, TempInfo: THtmlObjectInfo;
  LineInfo: TLineInfo;
  LineWidth: Integer;
  MaxLineWidth: Integer;
  TextR: TRect;
  DefaultLineHeight: Integer;
  Offset: Integer;
  TabBreak: Integer;
  TempObjects: TList;
  OLeft, OLen: Integer;
  Buffer: TBitmap;

procedure GetTextSize(ACanvas: TCanvas; S: String; var R: TRect);
begin
  R := Rect(0, 0, 0, 0);
  DrawText(ACanvas.Handle, PChar(S), Length(S), R,
    DT_CALCRECT or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE or DT_NOCLIP);
end;

function GetTextHeight(ACanvas: TCanvas; S: String): Integer;
var
  R: TRect;
begin
  R := Rect(0, 0, 0, 0);
  DrawText(ACanvas.Handle, PChar(S), Length(S), R,
    DT_CALCRECT or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE or DT_NOCLIP);
  Result := R.Bottom - R.Top;
end;

function GetTextWidth(ACanvas: TCanvas; S: String): Integer;
var
  R: TRect;
begin
  R := Rect(0, 0, 0, 0);
  DrawText(ACanvas.Handle, PChar(S), Length(S), R,
    DT_CALCRECT or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE or DT_NOCLIP);
  Result := R.Right - R.Left;
end;

begin
  if (csLoading in ComponentState) then Exit;

  FBuildingLines := True;
  Buffer := TBitmap.Create;

  try
    for i := 0 to FLines.Count - 1 do
     if FLines[i] <> nil then
       TLineInfo(FLines[i]).Free;
    FLines.Clear;

    LineInfo := TLineInfo.Create;
    FLines.Add(LineInfo);
    LineWidth := 0;
    TabBreak := 0;

    Buffer.Canvas.Font.Assign(Font);
    DefaultLineHeight := GetTextHeight(Buffer.Canvas, ' ');

    for I := 0 to FHtmlObjects.Count - 1 do
    begin
      HtmlObjectInfo := THtmlObjectInfo(FHtmlObjects[I]);
      S := HtmlObjectInfo.FText;

      if HtmlObjectInfo is TBreakInfo then
      begin
        LineInfo.FLineWidth := LineWidth;
        LineInfo := TLineInfo.Create;
        FLines.Add(LineInfo);
        LineWidth := 0;
        TabBreak := 0;
      end

      else
      if S <> '' then
      begin
        if HtmlObjectInfo.FLinkID = 0 then
        begin
          Buffer.Canvas.Font.Assign(Font);
          Buffer.Canvas.Font.Color := HtmlObjectInfo.FFontColor;
          Buffer.Canvas.Font.Style := HtmlObjectInfo.FFontStyle;
          Buffer.Canvas.Font.Size := HtmlObjectInfo.FSize;
        end
        else
        begin
          if MouseInLink(HtmlObjectInfo.FLinkID) then
            Buffer.Canvas.Font.Assign(FLinkFontHover)
          else
            Buffer.Canvas.Font.Assign(FLinkFontNormal);
        end;

        GetTextSize(Buffer.Canvas, S, TextR);
        HtmlObjectInfo.SetWidth(LineWidth, TextR);
        if (HtmlObjectInfo.FRect.Right <= Width) or (FAutoSizeWidth and
           ((HtmlObjectInfo.FRect.Right <= FMaxWidth) or (FMaxWidth = 0))) then
        begin
          LineInfo.FHtmlObjects.Add(HtmlObjectInfo);
          LineWidth := HtmlObjectInfo.FRect.Right;
        end
        else
        begin
          TempObjects := TList.Create;
          try
            while LineInfo.FHtmlObjects.Count > 0 do
            begin
              TempInfo := THtmlObjectInfo(LineInfo.FHtmlObjects[LineInfo.FHtmlObjects.Count - 1]);
              TempObjects.Insert(0, TempInfo);
              LineInfo.FHtmlObjects.Delete(LineInfo.FHtmlObjects.Count - 1);
            end;

            if (Trim(S) <> '') then TempObjects.Add(HtmlObjectInfo);

            if LineInfo.FHtmlObjects.Count > 0 then
            begin
              LineInfo.FLineWidth := THtmlObjectInfo(LineInfo.FHtmlObjects[LineInfo.FHtmlObjects.Count - 1]).FRect.Right;
              if (TempObjects.Count > 0) then
              begin
                LineInfo := TLineInfo.Create;
                FLines.Add(LineInfo);
              end;
            end
            else LineInfo.FLineWidth := 0;

            if (TempObjects.Count > 0) then
            begin
              if (LineInfo.FHtmlObjects.Count = 0) then
                LineWidth := TabBreak
              else
                LineWidth := 0;
              for J := 0 to TempObjects.Count - 1 do
              begin
                HtmlObjectInfo := THtmlObjectInfo(TempObjects[J]);
                LineInfo.FHtmlObjects.Add(HtmlObjectInfo);
                HtmlObjectInfo.AdjustWidth(LineWidth);
                LineWidth := HtmlObjectInfo.FRect.Right;
              end;

              if FAutoSizeWidth and (FMaxWidth <> 0) and (LineWidth > FMaxWidth) then
              begin
                HtmlObjectInfo := THtmlObjectInfo(LineInfo.FHtmlObjects[LineInfo.FHtmlObjects.Count - 1]);
                if HtmlObjectInfo <> nil then
                begin
                  LineWidth := HtmlObjectInfo.FRect.Left;
                  S := HtmlObjectInfo.FText;
                  OLeft := HtmlObjectInfo.FRect.Left;
                  OLen := Length(S);
                  while OLeft + GetTextWidth(Buffer.Canvas, S + '...') > FMaxWidth do
                  begin
                    Dec(OLen);
                    S := copy (S, 1, OLen);
                  end;
                  HtmlObjectInfo.FText := S + '...';
                  GetTextSize(Buffer.Canvas, HtmlObjectInfo.FText, TextR);
                  HtmlObjectInfo.SetWidth(LineWidth, TextR);
                  LineWidth := HtmlObjectInfo.FRect.Right;
                end;
              end;
            end;
          finally
            TempObjects.Free;
          end;
        end;
      end;
    end;

    LineInfo.FLineWidth := LineWidth;

    MaxLineWidth := 0;
    LineTop := 0;
    for I := 0 to FLines.Count - 1 do
    begin
      LineInfo := TLineInfo(FLines[I]);
      if LineInfo.FLineWidth > MaxLineWidth then MaxLineWidth := LineInfo.FLineWidth;

      LineInfo.FLineHeight := DefaultLineHeight;
      for J := 0 to LineInfo.FHtmlObjects.Count - 1 do
      begin
        if THtmlObjectInfo(LineInfo.FHtmlObjects[J]).FWordHeight > LineInfo.FLineHeight
          then LineInfo.FLineHeight := THtmlObjectInfo(LineInfo.FHtmlObjects[J]).FWordHeight;
      end;
      for J := 0 to LineInfo.FHtmlObjects.Count - 1 do
      begin
        THtmlObjectInfo(LineInfo.FHtmlObjects[J]).SetLineHeight(LineTop, LineInfo.FLineHeight);
      end;
      LineTop := LineTop + LineInfo.FLineHeight;
    end;

    if FAutoSizeWidth and (MaxLineWidth <> Width) then
    begin
      if (MaxLineWidth > FMaxWidth) and (FMaxWidth <> 0) then MaxLineWidth := FMaxWidth;
      Width := MaxLineWidth;
      if Assigned(FOnWidthChanged) then FOnWidthChanged(self);
    end;
    SetLHeight(LineTop);

    if FAutoSizeHeight and (LineTop <> Height) then
    begin
      Height := LineTop;
      FTextHeight := LineTop;
      if Assigned(FOnHeightChanged) then FOnHeightChanged(self);
    end;

    for I := 0 to FLines.Count - 1 do
    begin
      LineInfo := TLineInfo(FLines[I]);
      case FTextAlignment of
        taRightJustify: Offset := Width - LineInfo.FLineWidth;
        taCenter: Offset := (Width - LineInfo.FLineWidth) div 2;
        else Offset := 0;
      end;
      if Offset <> 0 then
      begin
        for J := 0 to LineInfo.FHtmlObjects.Count - 1 do
        begin
          THtmlObjectInfo(LineInfo.FHtmlObjects[J]).SetWOffset(Offset);
        end;
      end;
    end;
  finally
    FBuildingLines := False;
    Buffer.Free;
  end;
end;

procedure TscHTMLLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  ParseText;
  RePaintControl;
end;

procedure TscHTMLLabel.SetLinkFontNormal(Value: TFont);
begin
  FLinkFontNormal.Assign(Value);
end;

procedure TscHTMLLabel.SetLinkFontHover(Value: TFont);
begin
  FLinkFontHover.Assign(Value);
end;

procedure TscHTMLLabel.SetLHeight(Value: Integer);
begin
  if FAutoSizeHeight then Exit;
  if Height = Value then Exit;
  Height := Value;
  if Assigned(FOnHeightChanged) then FOnHeightChanged(self);
end;

procedure TscHTMLLabel.SetAutoSizeWidth(Value: Boolean);
begin
  if FAutoSizeWidth = Value then Exit;
  FAutoSizeWidth := Value;
  BuildHtmlLines;
  Invalidate;
end;

procedure TscHTMLLabel.SetAutoSizeHeight(Value: Boolean);
begin
  if FAutoSizeHeight = Value then Exit;
  FAutoSizeHeight := Value;
  BuildHtmlLines;
  Invalidate;
end;

procedure TscHTMLLabel.SetTextAlignment(Value: TAlignment);
begin
  if FTextAlignment = Value then Exit;
  FTextAlignment := Value;
  BuildHtmlLines;
  RePaintControl;
end;

procedure TscHTMLLabel.SetMaxWidth(Value: Integer);
begin
  if FMaxWidth = Value then Exit;
  FMaxWidth := Value;
  BuildHtmlLines;
  Invalidate;
end;

procedure TscHTMLLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  I: Integer;
  LInfo: TLinkInfo;
  LinkText, LinkURL: String;
begin
  if Button <> mbLeft then
  begin
    inherited;
    Exit;
  end;
  for I := 0 to FLinks.Count - 1 do
  begin
    LInfo := TLinkInfo(FLinks[I]);
    R := LInfo.FRect;
    if R.Contains(Point(X, Y)) then
    begin
      if FMouseDownIndex = I then
      begin
        FMouseWasDown := False;
        LinkText := GetLinkText(LInfo.FLinkID);
        LinkURL := GetLinkURL(LInfo.FLinkID);
        if Assigned(FOnLinkClick) then
          FOnLinkClick(Self, LInfo.FLinkID, LinkText, LinkURL);
        if FAutoExecuteURL and (LinkURL <> '') then
        begin
          ShellExecute(Handle, 'open', PChar(LinkURL), nil, nil, 0);
        end;
        Exit;
      end;
    end;
  end;
  FMouseDownIndex := - 1;
  inherited;
end;

procedure TscHTMLLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  I: Integer;
  LInfo: TLinkInfo;
begin
  FMouseWasDown := True;
  FMouseDownIndex := - 1;
  if Button = mbLeft then
  begin
    for I := 0 to FLinks.Count - 1 do
    begin
      LInfo := TLinkInfo(FLinks[i]);
      R := LInfo.FRect;
      if R.Contains(Point(X, Y)) then
        FMouseDownIndex := I;
    end;
  end;
  inherited;
end;

procedure TscHTMLLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  I: Integer;
  LInfo: TLinkInfo;
  CursorHand: Boolean;
begin
  try
    FMouseDownMove := FMouseWasDown;
    if (Y > Height) or (Y < 0) or
       (X > Width) or (X < 0)
    then
    begin
      DoMouseLeave;
      Exit;
    end;
    CursorHand := False;
    for I := 0 to FLinks.Count - 1 do
    begin
      LInfo := TLinkInfo(FLinks[i]);
      R := LInfo.FRect;

      if R.Contains(Point(X, Y)) then
      begin
        CursorHand := True;
        if not LInfo.FMouseOver then
        begin
          LInfo.FMouseOver := True;
          BuildHtmlLines;
          RePaintControl;
        end;
      end
      else
      begin
        if LInfo.FMouseOver then
        begin
          LInfo.FMouseOver := False;
          BuildHtmlLines;
          RePaintControl;
        end;
      end;
    end;
    if CursorHand then
    begin
      if Cursor <> crHandPoint then
        Cursor := crHandPoint;
    end
    else
    if Cursor <> crDefault then
      Cursor := crDefault;
  finally
    inherited MouseMove(Shift, X, Y);
  end;
end;

procedure TscHTMLLabel.DoMouseLeave;
var
  I: Integer;
  R: TLinkInfo;
begin
  FMouseDownMove := False;
  FMouseWasDown := False;
  for I := 0 to FLinks.Count - 1 do
  begin
    R := TLinkInfo(FLinks[I]);
    if R.FMouseOver then
    begin
      R.FMouseOver := False;
      Cursor := crDefault;
      BuildHtmlLines;
      RePaintControl;
    end;
  end;
end;

procedure TscHTMLLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  DoMouseLeave;
end;

function TscHTMLLabel.MouseInLink(LinkID: Integer): Boolean;
var
  I: Integer;
  R: TLinkInfo;
begin
  Result := False;
  for I := 0 to FLinks.Count - 1 do
  begin
    R := TLinkInfo(FLinks[I]);
    if R.FLinkID = LinkID then
    begin
      if R.FMouseOver then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TscHTMLLabel.GetLinkURL(LinkID: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FHtmlObjects.Count - 1 do
  begin
    if THtmlObjectInfo(FHtmlObjects[I]).FLinkID = LinkID then
    begin
      Result := THtmlObjectInfo(FHtmlObjects[I]).FLinkUrl;
      Break;
    end;
  end;
end;

function TscHTMLLabel.GetLinkText(LinkID: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FHtmlObjects.Count - 1 do
  begin
    if THtmlObjectInfo(FHtmlObjects[i]).FLinkID = LinkID then
    begin
      Result := Result + THtmlObjectInfo(FHtmlObjects[I]).FText;
      Break;
    end;
  end;
end;

procedure TscHTMLLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  OldWidth: Integer;
begin
  OldWidth := Width;
  inherited;
  if (csLoading in ComponentState) then Exit;
  if FBuildingLines then
  begin
    FRebuildLines := True;
    Exit;
  end;
  if OldWidth <> AWidth then
  begin
    BuildHtmlLines;
    Invalidate;
    if Assigned(FOnWidthChanged) then FOnWidthChanged(self);
  end;
end;

procedure TscHTMLLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  DoFontChange(nil);
end;

procedure TscHTMLLabel.DoFontChange(Sender: TObject);
begin
  ParseText;
  Invalidate;
end;

procedure TscHTMLLabel.Draw;
var
  ws: String;
  i, Line: Integer;
  HtmlObjectInfo: THtmlObjectInfo;
  LineInfo: TLineInfo;
  LinkIndex: Integer;
begin

  if FParsingText or FBuildingLines then Exit;

  if FRebuildLines then begin
    FRebuildLines := False;
    BuildHtmlLines;
  end;

  ACanvas.Brush.Style := bsClear;

  LinkIndex := 0;

  for Line := 0 to FLines.Count - 1 do
  begin
    LineInfo := TLineInfo(FLines[Line]);
    for i := 0 to LineInfo.FHtmlObjects.Count - 1 do
    begin
      HtmlObjectInfo := THtmlObjectInfo(LineInfo.FHtmlObjects[i]);
      ws := HtmlObjectInfo.FText;
      if ws <> '' then
      begin
        if HtmlObjectInfo.FBackColor = clNone
        then
          ACanvas.Brush.Style := bsClear
        else
        begin
          ACanvas.Brush.Color := HtmlObjectInfo.FBackColor;
          ACanvas.Brush.Style := bsSolid;
        end;
        if HtmlObjectInfo.FLinkID = 0 then
        begin
          ACanvas.Font.Assign(Self.Font);
          ACanvas.Font.Color := HtmlObjectInfo.FFontColor;
          ACanvas.Font.Style := HtmlObjectInfo.FFontStyle;
          ACanvas.Font.Size := HtmlObjectInfo.FSize;
          if not Enabled then
            ACanvas.Font.Color := GetCheckBoxTextColor(scsDisabled)
          else  
          if seFont in StyleElements then
            ACanvas.Font.Color := GetStyleColor(ACanvas.Font.Color);
        end
        else
        begin
          if MouseInLink(HtmlObjectInfo.FLinkID) then
            ACanvas.Font.Assign(FLinkFontHover)
          else
            ACanvas.Font.Assign(FLinkFontNormal);
          if not Enabled then
            ACanvas.Font.Color := GetCheckBoxTextColor(scsDisabled)
          else    
          if seFont in StyleElements then
          begin
            if not MouseInLink(HtmlObjectInfo.FLinkID) and
               (ACanvas.Font.Color = clHighLight)
            then
            begin
              if IsCustomStyle and IsDarkStyle then
                ACanvas.Font.Color := GetStyleColor(ACanvas.Font.Color)
              else
                ACanvas.Font.Color := GetActiveTextColor
            end
            else
            begin
              if IsCustomStyle and IsDarkStyle then
                ACanvas.Font.Color := GetActiveTextColor
              else
                ACanvas.Font.Color := GetStyleColor(ACanvas.Font.Color);
            end;
          end;
        end;
        if HtmlObjectInfo.FLinkID > 0 then
        begin
          TLinkInfo(FLinks[LinkIndex]).SetData(HtmlObjectInfo.FRect, HtmlObjectInfo.FLinkID);
          Inc(LinkIndex);
        end;
        DrawText(ACanvas.Handle, PChar(ws), length(ws), HtmlObjectInfo.FRect,
          DT_NOPREFIX or DT_LEFT or DT_SINGLELINE or DT_NOCLIP);
      end;
    end;
  end;

end;

end.
