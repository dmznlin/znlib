object fFormMain: TfFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = #21442#25968#31649#29702#22120
  ClientHeight = 476
  ClientWidth = 667
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 667
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 649
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = 'init'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 89
      Top = 10
      Width = 75
      Height = 25
      Caption = 'saveParam'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 170
      Top = 10
      Width = 75
      Height = 25
      Caption = 'delParam'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 574
      Top = 10
      Width = 75
      Height = 25
      Caption = 'status'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 251
      Top = 10
      Width = 75
      Height = 25
      Caption = 'getParam'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 331
      Top = 10
      Width = 75
      Height = 25
      Caption = 'getParam2'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 412
      Top = 10
      Width = 75
      Height = 25
      Caption = 'loadFile'
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 493
      Top = 10
      Width = 75
      Height = 25
      Caption = 'saveFile'
      TabOrder = 7
      OnClick = Button8Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 667
    Height = 435
    Align = alClient
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 649
  end
end
