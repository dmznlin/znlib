object frmMain: TfrmMain
  Left = 279
  Top = 104
  Width = 470
  Height = 420
  Caption = 'Append XML data to an existing XML *file*'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 79
    Height = 13
    Caption = 'File to append to'
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 147
    Height = 13
    Caption = 'XML to append (cut and paste)'
  end
  object Label3: TLabel
    Left = 16
    Top = 288
    Width = 206
    Height = 13
    Caption = 'Append level (level 1 = first level under root)'
  end
  object edFilename: TEdit
    Left = 16
    Top = 24
    Width = 345
    Height = 21
    TabOrder = 0
    Text = 'test.xml'
  end
  object btnSelect: TButton
    Left = 368
    Top = 22
    Width = 73
    Height = 25
    Caption = 'Select...'
    TabOrder = 1
    OnClick = btnSelectClick
  end
  object mmAppend: TMemo
    Left = 16
    Top = 72
    Width = 425
    Height = 201
    Lines.Strings = (
      '<Bag>'
      '  <PFHeader>'
      '    <OriginStore>2023</OriginStore>'
      '    <BagNumber>20577601</BagNumber>'
      '    <PickupStore>2023</PickupStore>'
      '    <DestLabCode>LCL</DestLabCode>'
      '    <CustomerNum>20230167050310031432</CustomerNum>'
      '    <StatusCode>5</StatusCode>'
      '    <FileLetter></FileLetter>'
      '    <Phone></Phone>'
      '    <PromisedDateTime>12/30/99 00:00</PromisedDateTime>'
      '    <CreateDateTime>12/30/99 00:00</CreateDateTime>'
      '    <LastPriceDateTime>12/30/99 00:00</LastPriceDateTime>'
      '    <SaleDateTime>12/30/99 00:00</SaleDateTime>'
      '    <BagPrice>2.79</BagPrice>'
      '    <BagSource>CO</BagSource>'
      '  </PFHeader>'
      '  <PFJobHdr>'
      '    <OriginStore>2023</OriginStore>'
      '    <BagNumber>20577601</BagNumber>'
      '    <JobNumber>1</JobNumber>'
      '    <JobOrder>-32000</JobOrder>'
      '    <Quantity>1</Quantity>'
      '    <UnitPrice>2.79</UnitPrice>'
      '    <FlatPrice>0</FlatPrice>'
      '    <JobPrice>2.79</JobPrice>'
      '    <JobCost>0</JobCost>'
      '    <Remarks></Remarks>'
      '  </PFJobHdr>'
      '  <PFJob>'
      '    <OriginStore>2023</OriginStore>'
      '    <BagNumber>20577601</BagNumber>'
      '    <JobNumber>1</JobNumber>'
      '    <Parent>0</Parent>'
      '    <Child>29</Child>'
      '  </PFJob>'
      '  <PFEvent>'
      '    <OriginStore>2023</OriginStore>'
      '    <BagNumber>20577601</BagNumber>'
      '    <JobNumber>1</JobNumber>'
      '    <EventDateTime>08/31/03 17:56</EventDateTime>'
      '    <EventCode>150</EventCode>'
      '    <EmpNum>3472</EmpNum>'
      '    <CausingStore>2023</CausingStore>'
      '    <ReceivingStore>2023</ReceivingStore>'
      '  </PFEvent>'
      '</Bag>')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnAppend: TButton
    Left = 16
    Top = 336
    Width = 105
    Height = 25
    Caption = 'Append now!'
    TabOrder = 3
    OnClick = btnAppendClick
  end
  object edLevel: TEdit
    Left = 16
    Top = 304
    Width = 41
    Height = 21
    TabOrder = 4
    Text = '1'
  end
  object udLevel: TUpDown
    Left = 57
    Top = 304
    Width = 15
    Height = 21
    Associate = edLevel
    Min = 1
    Position = 1
    TabOrder = 5
    Wrap = False
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 374
    Width = 462
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
