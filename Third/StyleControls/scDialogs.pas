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


unit scDialogs;

{$R-}
{$I scdefine.inc}

interface

uses Winapi.Windows, Winapi.Messages, System.Classes,
     System.Types, System.UITypes, Vcl.Menus, Vcl.Forms, Vcl.Dialogs,
     Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, scStyledForm, scDrawUtils, scControls;

function scMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

function scMessageDlgEx(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

function scMessageDlgEx(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; AButtonCaptions: array of string): Integer; overload;

procedure scShowMessage(const Msg: string);
procedure scShowMessageEx(const Title, Msg: string);

implementation
Uses scDlgStrs, System.StrUtils, System.SysUtils, Vcl.Consts, System.Math,
   scGPFontControls, scMsgDialog;

type
  TCustomFormClass = class(TCustomForm);

  TMessageForm = class(TscMsgDlgForm)
  private
    Message: TLabel;
    procedure HelpButtonClick(Sender: TObject);
  protected
    procedure CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WriteToClipBoard(Text: String);
    function GetFormText: String;
  end;

procedure TMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TMessageForm.CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Word('C')) then
    WriteToClipBoard(GetFormText);
end;

procedure TMessageForm.WriteToClipBoard(Text: String);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  if OpenClipBoard(0) then
  begin
    try
      Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, ByteLength(Text) + SizeOf(Char));
      try
        DataPtr := GlobalLock(Data);
        try
          Move(PChar(Text)^, DataPtr^, ByteLength(Text) + SizeOf(Char));
          EmptyClipBoard;
          SetClipboardData(CF_UNICODETEXT, Data);
        finally
          GlobalUnlock(Data);
        end;
      except
        GlobalFree(Data);
        raise;
      end;
    finally
      CloseClipBoard;
    end;
  end
  else
    raise Exception.CreateRes(@SCannotOpenClipboard);
end;

function TMessageForm.GetFormText: String;
var
  DividerLine, ButtonCaptions: string;
  I: integer;
begin
  DividerLine := StringOfChar('-', 27) + sLineBreak;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TButton then
      ButtonCaptions := ButtonCaptions + TButton(Components[I]).Caption +
        StringOfChar(' ', 3);
  ButtonCaptions := StringReplace(ButtonCaptions,'&','', [rfReplaceAll]);
  Result := Format('%s%s%s%s%s%s%s%s%s%s', [DividerLine, Caption, sLineBreak,
    DividerLine, Message.Caption, sLineBreak, DividerLine, ButtonCaptions,
    sLineBreak, DividerLine]);
end;

var
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help', 'Close');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);


function DlgCaption(AType: TMsgDlgType): Pointer;
begin
  Result := nil;
  case AType of
    mtWarning: Result := @SMsgDlgWarning;
    mtError: Result := @SMsgDlgError;
    TMsgDlgType.mtInformation: Result := @SMsgDlgInformation;
    TMsgDlgType.mtConfirmation: Result := @SMsgDlgConfirm;
  end;
end;

function ButtonCaption(AButton: TMsgDlgBtn): Pointer;
begin
  Result := nil;
  case AButton of
    mbYes: Result := @SMsgDlgYes;
    mbNo: Result := @SMsgDlgNo;
    mbOK: Result := @SMsgDlgOK;
    mbCancel: Result := @SMsgDlgCancel;
    mbAbort: Result := @SMsgDlgAbort;
    mbRetry: Result := @SMsgDlgRetry;
    mbIgnore: Result := @SMsgDlgIgnore;
    mbAll: Result := @SMsgDlgAll;
    mbNoToAll: Result := @SMsgDlgNoToAll;
    mbYesToAll: Result := @SMsgDlgYesToAll;
    mbHelp: Result :=  @SMsgDlgHelp;
    mbClose: Result := @SMsgDlgClose;
  end;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function scCreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; UseSystemCaptions: Boolean): TForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft: Integer;
  B, CancelButton: TMsgDlgBtn;
  TextRect: TRect;
  LButton: TButton;
  DefaultButton: TMsgDlgBtn;
  S: String;
  IconSize: Integer;
begin
  Result := TMessageForm.Create(Application);

  IconSize := TMessageForm(Result).Image.Width;

  if mbOk in Buttons then DefaultButton := mbOk else
    if mbYes in Buttons then DefaultButton := mbYes else
      DefaultButton := mbRetry;

  with Result do
  begin
    BiDiMode := Application.BiDiMode;
    Canvas.Font := Font;
    KeyPreview := True;
    PopupMode := pmAuto;
    OnKeyDown := TMessageForm(Result).CustomKeyDown;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);

    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if B in Buttons then
      begin
         S := LoadResString(ButtonCaption(B));
         if UseSystemCaptions then
          begin
            case B of
              TMsgDlgBtn.mbYes: S := SC_S_Msg_Yes;
              TMsgDlgBtn.mbNo:  S := SC_S_Msg_No;
              TMsgDlgBtn.mbCancel: S := SC_S_Msg_Cancel;
              TMsgDlgBtn.mbAbort: S := SC_S_Msg_Abort;
              TMsgDlgBtn.mbRetry: S := SC_S_Msg_Retry;
              TMsgDlgBtn.mbIgnore: S := SC_S_Msg_Ignore;
              TMsgDlgBtn.mbHelp: S := SC_S_Msg_Help;
              TMsgDlgBtn.mbClose: S := SC_S_Msg_Close;
            end;
          end;
          TextRect := Rect(0,0,0,0);
          Winapi.Windows.DrawText(Canvas.Handle,
            PChar(S), -1,
            TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
            DrawTextBiDiModeFlagsReadingOnly);
         with TextRect do if Right - Left + 8 > ButtonWidth then
           ButtonWidth := Right - Left + 8;
      end;
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, Msg, Length(Msg) + 1, TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);

    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if DlgType <> mtCustom then
    begin
      Inc(IconTextWidth, IconSize + HorzSpacing);
      if IconTextHeight < IconSize then IconTextHeight := IconSize;
    end;

    ButtonCount := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      Round(VertMargin * 2.1);
    if scDrawUtils.IsCustomStyle and SC_SCALEFORMBORDER then
      ClientHeight := ClientHeight + (IconSize - 42) div 2;

    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    if DlgType <> mtCustom then
      Caption := LoadResString(DlgCaption(DlgType)) else
      Caption := Application.Title;

    if DlgType = mtCustom then
      TMessageForm(Result).Image.Visible := False
    else
      with TMessageForm(Result).Image do
      begin
        case DlgType of
          mtWarning:
          begin
            ImageIndex := 106;
            if not scDrawUtils.IsCustomStyle then
              ImageColor := $000080FF;
          end;
          mtInformation:
          begin
            if not scDrawUtils.IsCustomStyle then
              ImageColor := $00D77800;
            ImageIndex := 90;
          end;
          mtError:
          begin
            ImageColor := $004040FF;
            ImageIndex := 87;
          end;
          mtConfirmation:
          begin
            if not scDrawUtils.IsCustomStyle then
              ImageColor := $00D77800;
            ImageIndex := 89;
          end;
        end;
        SetBounds(HorzMargin, VertMargin, IconSize, IconSize);
      end;

    TMessageForm(Result).Message := TLabel.Create(Result);
    with TMessageForm(Result).Message do
    begin
      Name := 'Message';
      Parent := Result;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := TextRect;
      BiDiMode := Result.BiDiMode;
      ALeft := IconTextWidth - TextRect.Right + HorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
      SetBounds(ALeft, VertMargin,
        TextRect.Width, TextRect.Height);
    end;
    if mbCancel in Buttons then CancelButton := mbCancel else
      if mbNo in Buttons then CancelButton := mbNo else
        CancelButton := mbOk;
    X := (ClientWidth - ButtonGroupWidth) div 2;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
      begin
        LButton := TButton.Create(Result);
        with LButton do
        begin
          Name := ButtonNames[B];
          Parent := Result;
          S := LoadResString(ButtonCaption(B));
          if UseSystemCaptions then
          begin
            case B of
              TMsgDlgBtn.mbYes: S := SC_S_Msg_Yes;
              TMsgDlgBtn.mbNo:  S := SC_S_Msg_No;
              TMsgDlgBtn.mbCancel: S := SC_S_Msg_Cancel;
              TMsgDlgBtn.mbAbort: S := SC_S_Msg_Abort;
              TMsgDlgBtn.mbRetry: S := SC_S_Msg_Retry;
              TMsgDlgBtn.mbIgnore: S := SC_S_Msg_Ignore;
              TMsgDlgBtn.mbHelp: S := SC_S_Msg_Help;
              TMsgDlgBtn.mbClose: S := SC_S_Msg_Close;
            end;
          end;
          Caption := S;
          ModalResult := ModalResults[B];
          if B = DefaultButton then
          begin
            Default := True;
            ActiveControl := LButton;
          end;
          if B = CancelButton then
            Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mbHelp then
            OnClick := TMessageForm(Result).HelpButtonClick;
        end;
      end;
  end;
end;

function scCreateMessageDialogCustomCaptions(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; AButtonCaptions: array of string): TForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft: Integer;
  B, CancelButton: TMsgDlgBtn;
  TextRect: TRect;
  LButton: TButton;
  DefaultButton: TMsgDlgBtn;
  S: String;
  FCaptionIndex: Integer;
  IconSize: Integer;
begin
  Result := TMessageForm.Create(Application);
  IconSize := TMessageForm(Result).Image.Width;

  if mbOk in Buttons then DefaultButton := mbOk else
    if mbYes in Buttons then DefaultButton := mbYes else
      DefaultButton := mbRetry;

  with Result do
  begin
    BiDiMode := Application.BiDiMode;
    Canvas.Font := Font;
    KeyPreview := True;
    PopupMode := pmAuto;
    OnKeyDown := TMessageForm(Result).CustomKeyDown;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    FCaptionIndex := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if B in Buttons then
      begin
        S := LoadResString(ButtonCaption(B));
        if FCaptionIndex <= High(AButtonCaptions) then
        begin
          S := AButtonCaptions[FCaptionIndex];
          Inc(FCaptionIndex);
        end;

        TextRect := Rect(0,0,0,0);
        Winapi.Windows.DrawText( canvas.handle,
          PChar(S), -1,
          TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
          DrawTextBiDiModeFlagsReadingOnly);

        with TextRect do if Right - Left + 8 > ButtonWidth then
           ButtonWidth := Right - Left + 8;
      end;
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, Msg, Length(Msg) + 1, TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);

    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if DlgType <> mtCustom then
    begin
      Inc(IconTextWidth, IconSize + HorzSpacing);
      if IconTextHeight < IconSize then IconTextHeight := IconSize;
    end;

    ButtonCount := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      Round(VertMargin * 2.1);
    if scDrawUtils.IsCustomStyle and SC_SCALEFORMBORDER then
      ClientHeight := ClientHeight + (IconSize - 42) div 2;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    if DlgType <> mtCustom then
      Caption := LoadResString(DlgCaption(DlgType)) else
      Caption := Application.Title;

     if DlgType = mtCustom then
       TMessageForm(Result).Image.Visible := False
     else
      with TMessageForm(Result).Image do
      begin
        case DlgType of
          mtWarning:
          begin
            ImageIndex := 106;
            if not scDrawUtils.IsCustomStyle then
              ImageColor := $000080FF;
          end;
          mtInformation:
          begin
            if not scDrawUtils.IsCustomStyle then
              ImageColor := $00D77800;
            ImageIndex := 90;
          end;
          mtError:
          begin
            ImageColor := $004040FF;
            ImageIndex := 87;
          end;
          mtConfirmation:
          begin
            if not scDrawUtils.IsCustomStyle then
              ImageColor := $00D77800;
            ImageIndex := 89;
          end;
        end;
        SetBounds(HorzMargin, VertMargin, IconSize, IconSize);
      end;

    TMessageForm(Result).Message := TLabel.Create(Result);
    with TMessageForm(Result).Message do
    begin
      Name := 'Message';
      Parent := Result;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := TextRect;
      BiDiMode := Result.BiDiMode;
      ALeft := IconTextWidth - TextRect.Right + HorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
      SetBounds(ALeft, VertMargin,
        TextRect.Width, TextRect.Height);
    end;
    if mbCancel in Buttons then CancelButton := mbCancel else
      if mbNo in Buttons then CancelButton := mbNo else
        CancelButton := mbOk;
    X := (ClientWidth - ButtonGroupWidth) div 2;
    FCaptionIndex := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
      begin
        LButton := TButton.Create(Result);
        with LButton do
        begin
          Name := ButtonNames[B];
          Parent := Result;
          S := LoadResString(ButtonCaption(B));
          if FCaptionIndex <= High(AButtonCaptions) then
          begin
            S := AButtonCaptions[FCaptionIndex];
            Inc(FCaptionIndex);
          end;
          Caption := S;
          ModalResult := ModalResults[B];
          if B = DefaultButton then
          begin
            Default := True;
            ActiveControl := LButton;
          end;
          if B = CancelButton then
            Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mbHelp then
            OnClick := TMessageForm(Result).HelpButtonClick;
        end;
      end;
  end;
end;


function scMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
  FMsgFrm: TForm;
begin
  if not scDrawUtils.IsCustomStyle then
  begin
    Result := MessageDlg(Msg, DlgType, Buttons, HelpCtx);
    Exit;
  end;
  FMsgFrm := scCreateMessageDialog(Msg, DlgType, Buttons, False);
  FMsgFrm.HelpContext := HelpCtx;
  FMsgFrm.Position := poScreenCenter;
  Result := FMsgFrm.ShowModal;
  FMsgFrm.Free;
end;

function scMessageDlgEx(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
var
  FMsgFrm: TForm;
begin
  FMsgFrm := scCreateMessageDialog(Msg, DlgType, Buttons, True);
  if Title <> '' then
    FMsgFrm.Caption := Title;
  FMsgFrm.Position := poScreenCenter;
  Result := FMsgFrm.ShowModal;
  FMsgFrm.Free;
end;

function scMessageDlgEx(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; AButtonCaptions: array of string): Integer;
var
  FMsgFrm: TForm;
 begin
  FMsgFrm := scCreateMessageDialogCustomCaptions(Msg, DlgType, Buttons, AButtonCaptions);
  if Title <> '' then
    FMsgFrm.Caption := Title;
  FMsgFrm.Position := poScreenCenter;
  Result := FMsgFrm.ShowModal;
  FMsgFrm.Free;
end;

procedure scShowMessage(const Msg: string);
begin
  scMessageDlg(Msg, mtCustom, [mbOK], -1);
end;

procedure scShowMessageEx(const Title, Msg: string);
begin
  scMessageDlgEx(Title, Msg, mtCustom, [mbOK], -1, []);
end;


end.
