object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Thread Pool'
  ClientHeight = 545
  ClientWidth = 524
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
  PixelsPerInch = 96
  TextHeight = 12
  object Memo1: TMemo
    Left = 0
    Top = 57
    Width = 524
    Height = 488
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 22
      Width = 54
      Height = 12
      Caption = #25910#21457#35745#25968':'
    end
    object Edit1: TEdit
      Left = 72
      Top = 19
      Width = 121
      Height = 20
      TabOrder = 0
      Text = '0'
    end
    object BtnStatus: TButton
      Left = 363
      Top = 17
      Width = 75
      Height = 25
      Caption = #29366#24577
      TabOrder = 1
      OnClick = BtnStatusClick
    end
    object BtnStart: TButton
      Left = 201
      Top = 17
      Width = 75
      Height = 25
      Caption = #21551#21160
      TabOrder = 2
      OnClick = BtnStartClick
    end
    object BtnStop: TButton
      Left = 282
      Top = 17
      Width = 75
      Height = 25
      Caption = #20572#27490
      TabOrder = 3
      OnClick = BtnStopClick
    end
  end
  object IdTCPServer1: TIdTCPServer
    Active = True
    Bindings = <>
    DefaultPort = 8000
    OnExecute = IdTCPServer1Execute
    Left = 32
    Top = 80
  end
end
