object fFormMain: TfFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 438
  ClientWidth = 651
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
  object Edit1: TEdit
    Left = 10
    Top = 72
    Width = 228
    Height = 20
    TabOrder = 0
    Text = 'TXSrv/DL/YX/MQTT/IRC/aabb'
  end
  object BtnSub: TButton
    Left = 255
    Top = 71
    Width = 75
    Height = 25
    Caption = '2.sub'
    TabOrder = 1
    OnClick = BtnSubClick
  end
  object Edit2: TEdit
    Left = 8
    Top = 104
    Width = 230
    Height = 20
    TabOrder = 2
    Text = #20013#25991#27979#35797
  end
  object BtnPub: TButton
    Left = 255
    Top = 102
    Width = 75
    Height = 25
    Caption = '3.pub'
    TabOrder = 3
    OnClick = BtnPubClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 136
    Width = 651
    Height = 302
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object CheckBox1: TCheckBox
    Left = 255
    Top = 31
    Width = 86
    Height = 17
    Caption = '1.'#21551#21160#26381#21153
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object BtnUnsub: TButton
    Left = 336
    Top = 71
    Width = 75
    Height = 25
    Caption = '4.un-sub'
    TabOrder = 6
    OnClick = BtnUnsubClick
  end
  object Edit3: TEdit
    Left = 10
    Top = 5
    Width = 111
    Height = 20
    TabOrder = 7
    Text = '118.89.157.37'
  end
  object Edit4: TEdit
    Left = 127
    Top = 5
    Width = 111
    Height = 20
    TabOrder = 8
    Text = '8031'
  end
  object Edit5: TEdit
    Left = 10
    Top = 32
    Width = 111
    Height = 20
    TabOrder = 9
    Text = 'dmzn'
  end
  object Edit6: TEdit
    Left = 127
    Top = 32
    Width = 111
    Height = 20
    PasswordChar = '*'
    TabOrder = 10
    Text = 'dmzn'
  end
  object CheckBox2: TCheckBox
    Left = 255
    Top = 8
    Width = 75
    Height = 17
    Caption = '0.'#26174#31034#26085#24535
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = CheckBox2Click
  end
  object BtnStatus: TButton
    Left = 336
    Top = 102
    Width = 75
    Height = 25
    Caption = 'status'
    TabOrder = 12
    OnClick = BtnStatusClick
  end
  object BtnNewTopic: TButton
    Left = 417
    Top = 102
    Width = 75
    Height = 25
    Caption = 'make topic'
    TabOrder = 13
    OnClick = BtnNewTopicClick
  end
end
