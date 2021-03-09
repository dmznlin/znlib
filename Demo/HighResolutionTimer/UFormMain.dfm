object fFormMain: TfFormMain
  Left = 0
  Top = 0
  Caption = #39640#24615#33021#35745#26102#22120
  ClientHeight = 591
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    558
    591)
  PixelsPerInch = 96
  TextHeight = 12
  object Edit1: TEdit
    Left = 8
    Top = 68
    Width = 185
    Height = 20
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 8
    Top = 37
    Width = 185
    Height = 20
    TabOrder = 1
  end
  object BtnDel: TButton
    Left = 199
    Top = 68
    Width = 75
    Height = 25
    Caption = 'delete'
    TabOrder = 2
    OnClick = BtnDelClick
  end
  object Edit3: TEdit
    Left = 8
    Top = 10
    Width = 185
    Height = 20
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 8
    Top = 112
    Width = 542
    Height = 467
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
    ExplicitWidth = 582
    ExplicitHeight = 394
  end
  object BtnAdd: TButton
    Left = 199
    Top = 37
    Width = 75
    Height = 25
    Caption = 'add'
    TabOrder = 5
    OnClick = BtnAddClick
  end
  object BtnStop: TButton
    Left = 280
    Top = 68
    Width = 75
    Height = 25
    Caption = 'stop'
    TabOrder = 6
    OnClick = BtnStopClick
  end
  object BtnStart: TButton
    Left = 280
    Top = 37
    Width = 75
    Height = 25
    Caption = 'start'
    TabOrder = 7
    OnClick = BtnStartClick
  end
  object SpinEdit1: TSpinEdit
    Left = 201
    Top = 9
    Width = 73
    Height = 21
    MaxValue = 32
    MinValue = 1
    TabOrder = 8
    Value = 1
    OnChange = SpinEdit1Change
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 376
    Top = 8
  end
end
