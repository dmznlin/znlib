object FontViewerForm: TFontViewerForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Char'
  ClientHeight = 424
  ClientWidth = 708
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 708
    Height = 372
    Align = alClient
    BorderStyle = bsNone
    Columns = <>
    DoubleBuffered = True
    LargeImages = ImageList1
    ParentDoubleBuffered = False
    TabOrder = 0
    OnAdvancedCustomDrawItem = ListView1AdvancedCustomDrawItem
    ExplicitWidth = 699
  end
  object Panel1: TPanel
    Left = 0
    Top = 372
    Width = 708
    Height = 52
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 699
    object Button1: TButton
      Left = 598
      Top = 17
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 507
      Top = 17
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object ImageList1: TImageList
    Height = 48
    Width = 32
    Left = 136
    Top = 296
  end
end
