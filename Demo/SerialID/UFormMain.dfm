object fFormMain: TfFormMain
  Left = 0
  Top = 0
  Caption = 'fFormMain'
  ClientHeight = 351
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 12
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 609
    Height = 297
    Align = alTop
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 251
    Top = 314
    Width = 105
    Height = 25
    Caption = 'check'
    TabOrder = 1
    OnClick = Button1Click
  end
end
