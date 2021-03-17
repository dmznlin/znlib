object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #25968#25454#24211#36830#25509#27744
  ClientHeight = 649
  ClientWidth = 773
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
    Width = 773
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      773
      65)
    object Label1: TLabel
      Left = 15
      Top = 13
      Width = 30
      Height = 12
      Caption = #36830#25509':'
    end
    object Label2: TLabel
      Left = 15
      Top = 37
      Width = 30
      Height = 12
      Caption = #39537#21160':'
    end
    object BtnStatus: TButton
      Left = 255
      Top = 37
      Width = 75
      Height = 25
      Caption = 'status'
      TabOrder = 0
      OnClick = BtnStatusClick
    end
    object Edit1: TEdit
      Left = 54
      Top = 10
      Width = 693
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 
        'Provider=SQLOLEDB.1;Password=sa;Persist Security Info=True;User ' +
        'ID=sa;Initial Catalog=test;Data Source=127.0.0.1'
    end
    object EditDrv: TComboBox
      Left = 51
      Top = 36
      Width = 179
      Height = 22
      Style = csOwnerDrawFixed
      TabOrder = 2
      OnChange = EditDrvChange
    end
    object BtnInit: TButton
      Left = 336
      Top = 37
      Width = 75
      Height = 25
      Caption = 'Init'
      TabOrder = 3
      OnClick = BtnInitClick
    end
    object BtnTrans: TButton
      Left = 417
      Top = 36
      Width = 75
      Height = 25
      Caption = 'trans'
      TabOrder = 4
      OnClick = BtnTransClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 65
    Width = 773
    Height = 584
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
