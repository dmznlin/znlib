object fFormMain: TfFormMain
  Left = 596
  Top = 463
  Caption = 'Doc 2 Code'
  ClientHeight = 525
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 12
  object Splitter1: TSplitter
    Left = 0
    Top = 225
    Width = 754
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 762
  end
  object Edit1: TMemo
    Left = 0
    Top = 0
    Width = 754
    Height = 225
    Align = alTop
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 228
    Width = 754
    Height = 53
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Btn1: TButton
      Left = 359
      Top = 22
      Width = 75
      Height = 25
      Caption = #8595#36716#25442
      TabOrder = 0
      OnClick = Btn1Click
    end
    object Edit3: TLabeledEdit
      Left = 12
      Top = 25
      Width = 81
      Height = 20
      EditLabel.Width = 54
      EditLabel.Height = 12
      EditLabel.Caption = #31354#26684#23545#40784':'
      TabOrder = 1
      Text = '38'
    end
    object Edit4: TLabeledEdit
      Left = 98
      Top = 25
      Width = 81
      Height = 20
      EditLabel.Width = 54
      EditLabel.Height = 12
      EditLabel.Caption = #34892#23383#31526#25968':'
      TabOrder = 2
      Text = '76'
    end
    object Check1: TCheckBox
      Left = 192
      Top = 26
      Width = 97
      Height = 17
      Caption = 'KEY'#29420#31435#19968#34892'.'
      TabOrder = 3
    end
    object Check2: TCheckBox
      Left = 288
      Top = 26
      Width = 65
      Height = 17
      Caption = 'C'#24211#25991#26723
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object Edit2: TMemo
    Left = 0
    Top = 281
    Width = 754
    Height = 244
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
