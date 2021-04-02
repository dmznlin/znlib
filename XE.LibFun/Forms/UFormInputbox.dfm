object fFormInputBox: TfFormInputBox
  Left = 252
  Top = 289
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 99
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 12
  object BtnOK: TButton
    Left = 142
    Top = 66
    Width = 65
    Height = 22
    Caption = #30830#23450
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object BtnExit: TButton
    Left = 215
    Top = 66
    Width = 65
    Height = 22
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 3
  end
  object LabelHint: TcxLabel
    Left = 8
    Top = 12
    AutoSize = False
    Caption = #25552#31034':'
    ParentFont = False
    Properties.Alignment.Vert = taBottomJustify
    Properties.WordWrap = True
    Transparent = True
    Height = 30
    Width = 272
    AnchorY = 42
  end
  object EditValue: TcxTextEdit
    Left = 8
    Top = 42
    ParentFont = False
    TabOrder = 0
    Width = 272
  end
end
