object fFormMain: TfFormMain
  Left = 0
  Top = 0
  ClientHeight = 491
  ClientWidth = 872
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  TextHeight = 12
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 567
    Height = 491
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 567
    Top = 0
    Width = 305
    Height = 491
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Btn1: TButton
      Left = 89
      Top = 45
      Width = 75
      Height = 25
      Caption = 'Btn1'
      TabOrder = 0
      OnClick = Btn1Click
    end
    object Btn2: TButton
      Left = 8
      Top = 14
      Width = 75
      Height = 25
      Caption = 'getStatus'
      TabOrder = 1
      OnClick = Btn2Click
    end
    object MemoTopic: TMemo
      Left = 0
      Top = 287
      Width = 305
      Height = 204
      Align = alBottom
      Lines.Strings = (
        'bootstrap.servers=123.56.150.117:8082')
      ScrollBars = ssBoth
      TabOrder = 2
    end
    object MemoMain: TMemo
      Left = 0
      Top = 87
      Width = 305
      Height = 200
      Align = alBottom
      Lines.Strings = (
        'bootstrap.servers=123.56.150.117:8082')
      ScrollBars = ssBoth
      TabOrder = 3
    end
    object Btn3: TButton
      Left = 8
      Top = 45
      Width = 75
      Height = 25
      Caption = 'client'
      TabOrder = 4
      OnClick = Btn3Click
    end
    object Btn4: TButton
      Left = 89
      Top = 14
      Width = 75
      Height = 25
      Caption = 'config'
      TabOrder = 5
      OnClick = Btn4Click
    end
  end
end
