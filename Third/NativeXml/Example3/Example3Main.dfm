object Form1: TForm1
  Left = 279
  Top = 104
  Width = 822
  Height = 420
  Caption = 'XML Demo showing how to convert XML to dynamic array of records'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbStatus: TLabel
    Left = 136
    Top = 24
    Width = 3
    Height = 13
  end
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 100
    Height = 13
    Caption = 'XML (Cut and paste):'
  end
  object Label2: TLabel
    Left = 328
    Top = 56
    Width = 68
    Height = 13
    Caption = 'Imported data:'
  end
  object Label3: TLabel
    Left = 16
    Top = 376
    Width = 212
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Copyright (c) 2003 by Nils Haeck (Simdesign)'
  end
  object mmXML: TMemo
    Left = 16
    Top = 72
    Width = 297
    Height = 297
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      '<?xml version="1.0" encoding="UTF-8" ?>'
      '<!DOCTYPE field_info (View Source for full doctype...)>'
      '<field_info>'
      '<fields>'
      '<field id="F1" required="no" type="text">'
      '<caption>Text</caption>'
      '<metadata type="text" />'
      '</field>'
      '<field id="F2" required="always" type="text">'
      '<caption>Text Req</caption>'
      '<metadata type="text" />'
      '</field>'
      '<field id="F3" required="no" type="textarea">'
      '<caption>Text ML</caption>'
      '<metadata type="text" />'
      '</field>'
      '<field id="F4" required="always" type="textarea">'
      '<caption>Text ML Req</caption>'
      '<metadata type="text" />'
      '</field>'
      '<field id="F5" required="no" type="enum">'
      '<caption>DL</caption>'
      '<metadata type="enum">'
      '<enumeration>'
      '<enumvalue id="1">'
      '<caption>1</caption>'
      '</enumvalue>'
      '<enumvalue id="2">'
      '<caption>2</caption>'
      '</enumvalue>'
      '<enumvalue id="3">'
      '<caption>3</caption>'
      '</enumvalue>'
      '<enumvalue id="4">'
      '<caption>4</caption>'
      '</enumvalue>'
      '</enumeration>'
      '</metadata>'
      '</field>'
      '<field id="F6" required="always" type="enum">'
      '<caption>DL Req</caption>'
      '<metadata type="enum">'
      '<enumeration>'
      '<enumvalue id="1">'
      '<caption>1</caption>'
      '</enumvalue>'
      '<enumvalue id="2">'
      '<caption>2</caption>'
      '</enumvalue>'
      '<enumvalue id="3">'
      '<caption>3</caption>'
      '</enumvalue>'
      '<enumvalue id="4">'
      '<caption>4</caption>'
      '</enumvalue>'
      '</enumeration>'
      '</metadata>'
      '</field>'
      
        '<field dependsfield="F1" id="F7" required="always" type="passwor' +
        'd">'
      '<caption>PW Req</caption>'
      '<metadata type="hashtable">'
      '<hashtable>'
      '<code id="John">d4da4238b0b923820dcc509a6f75849b</code>'
      '<code id="Test">c61e738d9d4a2f636f067f89cc14862c</code>'
      '</hashtable>'
      '</metadata>'
      '</field>'
      '</fields>'
      '</field_info>')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnImportXML: TButton
    Left = 16
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Import XML'
    TabOrder = 1
    OnClick = btnImportXMLClick
  end
  object lvTable: TListView
    Left = 328
    Top = 72
    Width = 473
    Height = 297
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'ID'
      end
      item
        Caption = 'Req'
      end
      item
        Caption = 'Type'
      end
      item
        Caption = 'Caption'
      end
      item
        Caption = 'Depends'
      end
      item
        Caption = 'MetaType'
      end
      item
        Caption = 'MetaData'
      end>
    OwnerData = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnData = lvTableData
  end
end
