object fFormMain: TfFormMain
  Left = 0
  Top = 0
  Caption = 'data dict'
  ClientHeight = 367
  ClientWidth = 578
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 578
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = -6
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = 'initdb'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 89
      Top = 10
      Width = 75
      Height = 25
      Caption = 'initdict'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 170
      Top = 10
      Width = 75
      Height = 25
      Caption = 'getdict'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 251
      Top = 10
      Width = 75
      Height = 25
      Caption = 'status'
      TabOrder = 3
      OnClick = Button4Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 578
    Height = 326
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
