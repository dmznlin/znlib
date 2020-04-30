object scBitmapEditorForm: TscBitmapEditorForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Image Editor'
  ClientHeight = 398
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 8
    Width = 408
    Height = 350
    OnPaint = PaintBoxPaint
  end
  object Button1: TButton
    Left = 8
    Top = 365
    Width = 81
    Height = 25
    Caption = 'Load...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 95
    Top = 365
    Width = 74
    Height = 25
    Caption = 'Save...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 260
    Top = 365
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 341
    Top = 365
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 175
    Top = 365
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = Button5Click
  end
  object OD: TOpenDialog
    DefaultExt = 'png'
    Filter = 'PNG Image|*.png'
    Left = 304
    Top = 24
  end
  object SD: TSaveDialog
    DefaultExt = 'png'
    Filter = 'PNG Image|*.png'
    Left = 352
    Top = 24
  end
end
