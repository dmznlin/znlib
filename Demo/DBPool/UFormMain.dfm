object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #25968#25454#24211#36830#25509#27744
  ClientHeight = 574
  ClientWidth = 753
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
    Width = 753
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 988
    object Btn1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Init'
      TabOrder = 0
      OnClick = Btn1Click
    end
    object Edit1: TEdit
      Left = 96
      Top = 10
      Width = 641
      Height = 21
      TabOrder = 1
      Text = 
        'Provider=SQLOLEDB.1;Password=sa;Persist Security Info=True;User ' +
        'ID=sa;Initial Catalog=test;Data Source=127.0.0.1'
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 753
    Height = 533
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    ExplicitWidth = 988
    ExplicitHeight = 544
  end
end
