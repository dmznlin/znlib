object Form1: TForm1
  Left = 278
  Top = 104
  Width = 783
  Height = 539
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 3
    Width = 97
    Height = 13
    Caption = 'Filename for loading:'
  end
  object Label2: TLabel
    Left = 8
    Top = 43
    Width = 94
    Height = 13
    Caption = 'Filename for saving:'
  end
  object btnLoadUseEvents: TButton
    Left = 504
    Top = 16
    Width = 129
    Height = 25
    Caption = 'Load using events'
    TabOrder = 0
    OnClick = btnLoadUseEventsClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 88
    Width = 761
    Height = 377
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object btnLoad: TButton
    Left = 368
    Top = 16
    Width = 129
    Height = 25
    Caption = 'Load w. LoadFromFile'
    TabOrder = 2
    OnClick = btnLoadClick
  end
  object btnLoad1st2nd: TButton
    Left = 640
    Top = 16
    Width = 129
    Height = 25
    Caption = 'Load and enumerate'
    TabOrder = 3
    OnClick = btnLoad1st2ndClick
  end
  object edXmlFileOpen: TEdit
    Left = 8
    Top = 18
    Width = 329
    Height = 21
    TabOrder = 4
    Text = 'email.xml'
  end
  object btnSelectOpen: TButton
    Left = 336
    Top = 18
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = btnSelectOpenClick
  end
  object btnCreate: TButton
    Left = 368
    Top = 56
    Width = 265
    Height = 25
    Caption = 'Create/Load/Save/Clear XML benchmark'
    TabOrder = 6
    OnClick = btnCreateClick
  end
  object edXmlFileSave: TEdit
    Left = 8
    Top = 58
    Width = 329
    Height = 21
    TabOrder = 7
    Text = 'test.xml'
  end
  object btnSelectSave: TButton
    Left = 336
    Top = 58
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 8
    OnClick = btnSelectSaveClick
  end
  object btnAddStyleSheet: TButton
    Left = 640
    Top = 56
    Width = 129
    Height = 25
    Caption = 'Add StyleSheet'
    TabOrder = 9
    OnClick = btnAddStyleSheetClick
  end
  object btnAbort: TButton
    Left = 8
    Top = 472
    Width = 121
    Height = 25
    Caption = 'Abort Parsing'
    TabOrder = 10
    OnClick = btnAbortClick
  end
end
