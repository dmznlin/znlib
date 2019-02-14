object Form1: TForm1
  Left = 187
  Top = 98
  Caption = 
    'kbmMemTable demo. Created by Components4Developers (www.componen' +
    'ts4developers.com)'
  ClientHeight = 694
  ClientWidth = 938
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 289
    Top = 0
    Width = 649
    Height = 656
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object DBGrid1: TDBGrid
      Left = 0
      Top = 0
      Width = 649
      Height = 470
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      DataSource = DataSource1
      Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -13
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object DBNavigator1: TDBNavigator
      Left = 0
      Top = 629
      Width = 649
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DataSource = DataSource1
      Align = alBottom
      Kind = dbnHorizontal
      TabOrder = 1
    end
    object Panel2: TPanel
      Left = 0
      Top = 470
      Width = 649
      Height = 159
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object DBImage1: TDBImage
        Left = 0
        Top = 0
        Width = 170
        Height = 159
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alLeft
        DataSource = DataSource1
        Stretch = True
        TabOrder = 0
      end
      object DBMemo1: TDBMemo
        Left = 170
        Top = 0
        Width = 251
        Height = 159
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        TabOrder = 1
      end
      object Panel6: TPanel
        Left = 421
        Top = 0
        Width = 228
        Height = 159
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 2
        object Label34: TLabel
          Left = 10
          Top = 10
          Width = 64
          Height = 16
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'BytesField'
        end
        object DBEdit1: TDBEdit
          Left = 10
          Top = 30
          Width = 149
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 0
        end
        object Button22: TButton
          Left = 10
          Top = 69
          Width = 92
          Height = 31
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Clear Memo'
          TabOrder = 1
          OnClick = Button22Click
        end
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 656
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = TabSheet1
    Align = alLeft
    MultiLine = True
    TabOrder = 1
    object TabSheet1: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Basic functionality'
      object Label6: TLabel
        Left = 9
        Top = 9
        Width = 144
        Height = 32
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Define some fields programatically'
        WordWrap = True
      end
      object Label7: TLabel
        Left = 9
        Top = 64
        Width = 160
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Open the memorytable'
        WordWrap = True
      end
      object Label8: TLabel
        Left = 9
        Top = 105
        Width = 168
        Height = 32
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Close the memorytable'
        WordWrap = True
      end
      object Label9: TLabel
        Left = 9
        Top = 282
        Width = 152
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Generate sample data'
        WordWrap = True
      end
      object Label25: TLabel
        Left = 10
        Top = 138
        Width = 159
        Height = 40
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Number of records in sample data'
        WordWrap = True
      end
      object Label26: TLabel
        Left = 10
        Top = 532
        Width = 90
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Recordnumber'
      end
      object lRecNo: TLabel
        Left = 138
        Top = 532
        Width = 4
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '-'
      end
      object Label35: TLabel
        Left = 10
        Top = 502
        Width = 68
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Old VALUE'
      end
      object lOldValue: TLabel
        Left = 138
        Top = 502
        Width = 3
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '.'
      end
      object Button6: TButton
        Left = 192
        Top = 9
        Width = 75
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Def. Fields'
        TabOrder = 0
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 192
        Top = 64
        Width = 75
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Open'
        TabOrder = 1
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 192
        Top = 105
        Width = 75
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Close'
        TabOrder = 2
        OnClick = Button8Click
      end
      object Button1: TButton
        Left = 192
        Top = 272
        Width = 75
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Generate'
        TabOrder = 3
        OnClick = Button1Click
      end
      object eRecordCount: TEdit
        Left = 192
        Top = 138
        Width = 75
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 4
        Text = '100'
      end
      object chbEnableIndexes: TCheckBox
        Left = 10
        Top = 187
        Width = 168
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Update indexes'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = chbEnableIndexesClick
      end
      object chbRandomColor: TCheckBox
        Left = 10
        Top = 217
        Width = 119
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Random COLOR data'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object chbGenerateMemos: TCheckBox
        Left = 10
        Top = 246
        Width = 159
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Generate memos'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object Button13: TButton
        Left = 158
        Top = 311
        Width = 109
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'AppendRecord'
        TabOrder = 8
        OnClick = Button13Click
      end
      object Button10: TButton
        Left = 175
        Top = 364
        Width = 92
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Goto 10'
        TabOrder = 9
        OnClick = Button10Click
      end
      object Button25: TButton
        Left = 175
        Top = 407
        Width = 92
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Pack'
        TabOrder = 10
        OnClick = Button25Click
      end
      object Button26: TButton
        Left = 175
        Top = 444
        Width = 92
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Empty'
        TabOrder = 11
        OnClick = Button26Click
      end
    end
    object TabSheet2: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Load/Save'
      object Label19: TLabel
        Left = 16
        Top = 319
        Width = 258
        Height = 37
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 
          'Check this, press the save above, after always with this checked' +
          ' press Load'
        WordWrap = True
      end
      object lblLZH: TLabel
        Left = 15
        Top = 362
        Width = 5
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = ' '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object PageControl2: TPageControl
        Left = 0
        Top = 89
        Width = 281
        Height = 452
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ActivePage = TabSheet5
        Align = alClient
        TabOrder = 0
        TabPosition = tpBottom
        object TabSheet5: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Copy, Save, Load'
          object Label1: TLabel
            Left = 9
            Top = 113
            Width = 168
            Height = 32
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Copy structur and data from another datasource.'
            WordWrap = True
          end
          object Label2: TLabel
            Left = 9
            Top = 282
            Width = 168
            Height = 32
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Save contents in a file, incl. blobs'
            WordWrap = True
          end
          object Label3: TLabel
            Left = 9
            Top = 330
            Width = 168
            Height = 32
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Load data from a file'
            WordWrap = True
          end
          object Label20: TLabel
            Left = 7
            Top = 26
            Width = 258
            Height = 75
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 
              'Check Blob InMemory Compression before loading the table and the' +
              ' Blob fIelds will be transparently de/compressed using less memo' +
              'ry'
            WordWrap = True
          end
          object Button9: TButton
            Left = 192
            Top = 113
            Width = 75
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Copy'
            TabOrder = 0
            OnClick = Button9Click
          end
          object Button2: TButton
            Left = 192
            Top = 282
            Width = 75
            Height = 23
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Save'
            TabOrder = 1
            OnClick = Button2Click
          end
          object Button3: TButton
            Left = 192
            Top = 330
            Width = 75
            Height = 23
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Load'
            TabOrder = 2
            OnClick = Button3Click
          end
          object BlobCompression: TCheckBox
            Left = 7
            Top = -1
            Width = 251
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Blob InMemory Compression'
            TabOrder = 3
          end
          object BinarySave: TCheckBox
            Left = 9
            Top = 160
            Width = 176
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Save/Load as binary'
            TabOrder = 4
            OnClick = BinarySaveClick
          end
          object chbSaveIndexDef: TCheckBox
            Left = 10
            Top = 187
            Width = 247
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Save index definitions too'
            TabOrder = 5
          end
          object chbSaveDeltas: TCheckBox
            Left = 10
            Top = 217
            Width = 257
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Save deltas too (only if binary checked)'
            Enabled = False
            TabOrder = 6
          end
          object chbNoQuotes: TCheckBox
            Left = 10
            Top = 246
            Width = 119
            Height = 21
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'No quotes'
            TabOrder = 7
          end
        end
        object TabSheet6: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'CommaText'
          object Label4: TLabel
            Left = 9
            Top = 9
            Width = 136
            Height = 72
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 
              'Save contents in CSV format in the memo. Note that Blobs doesnt ' +
              'get saved.'
            WordWrap = True
          end
          object Label5: TLabel
            Left = 9
            Top = 97
            Width = 136
            Height = 40
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Load contents from CSV formatted memo.'
            WordWrap = True
          end
          object Button4: TButton
            Left = 153
            Top = 33
            Width = 112
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Get CommaText'
            TabOrder = 0
            OnClick = Button4Click
          end
          object Button5: TButton
            Left = 153
            Top = 105
            Width = 112
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Set CommaText'
            TabOrder = 1
            OnClick = Button5Click
          end
          object Memo1: TMemo
            Left = 0
            Top = 261
            Width = 273
            Height = 162
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alBottom
            TabOrder = 2
          end
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 281
        Height = 89
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label21: TLabel
          Left = 9
          Top = 28
          Width = 264
          Height = 53
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 
            'Remember to check this before loading from a compressed file or ' +
            'before saving to a file with compression.'
          WordWrap = True
        end
        object LZHCompressed: TCheckBox
          Left = 7
          Top = 4
          Width = 251
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '(De)Compress CSV file/CommaText'
          TabOrder = 0
        end
      end
    end
    object TabSheet3: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Sorting/Searching'
      OnEnter = TabSheet3Enter
      object Label10: TLabel
        Left = 9
        Top = 1
        Width = 152
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Sort the table on'
        WordWrap = True
      end
      object Label12: TLabel
        Left = 9
        Top = 123
        Width = 168
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'What to search for'
        WordWrap = True
      end
      object Label13: TLabel
        Left = 9
        Top = 155
        Width = 112
        Height = 32
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Locate for PERIOD'
        WordWrap = True
      end
      object Label14: TLabel
        Left = 9
        Top = 201
        Width = 112
        Height = 49
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Locate for VALUE. If sorted will do binary locate.'
        WordWrap = True
      end
      object Label15: TLabel
        Left = 9
        Top = 263
        Width = 112
        Height = 32
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Locate for CALC'
        WordWrap = True
      end
      object Label16: TLabel
        Left = 9
        Top = 378
        Width = 112
        Height = 33
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Lookup for PERIOD'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 9
        Top = 418
        Width = 168
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'What has been looked up'
        WordWrap = True
      end
      object Label24: TLabel
        Left = 10
        Top = 305
        Width = 121
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'FindNearest VALUE'
      end
      object Label47: TLabel
        Left = 10
        Top = 345
        Width = 96
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'FindKey VALUE'
      end
      object Button11: TButton
        Left = 187
        Top = 18
        Width = 75
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Sort'
        TabOrder = 0
        OnClick = Button11Click
      end
      object btnLocatePeriod: TButton
        Left = 137
        Top = 154
        Width = 125
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Locate Period'
        TabOrder = 5
        OnClick = btnLocatePeriodClick
      end
      object eSearch: TEdit
        Left = 192
        Top = 123
        Width = 70
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 4
      end
      object btnLocateValue: TButton
        Left = 137
        Top = 208
        Width = 125
        Height = 27
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Locate Value'
        TabOrder = 6
        OnClick = btnLocateValueClick
      end
      object chbCaseInsensitive: TCheckBox
        Left = 9
        Top = 66
        Width = 199
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'CaseInsensitive'
        TabOrder = 2
      end
      object chbPartialKey: TCheckBox
        Left = 9
        Top = 91
        Width = 119
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'PartialKey'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object btnLookupCalc: TButton
        Left = 137
        Top = 375
        Width = 125
        Height = 27
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Period->Calc'
        TabOrder = 8
        OnClick = btnLookupCalcClick
      end
      object btnLocateCalc: TButton
        Left = 137
        Top = 254
        Width = 125
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Locate CALC'
        TabOrder = 7
        OnClick = btnLocateCalcClick
      end
      object eResult: TEdit
        Left = 192
        Top = 410
        Width = 70
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 9
      end
      object chbDescending: TCheckBox
        Left = 9
        Top = 48
        Width = 96
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Descending'
        TabOrder = 1
      end
      object cbSortField: TComboBox
        Left = 9
        Top = 16
        Width = 112
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        TabOrder = 10
      end
      object btnFindNearest: TButton
        Left = 137
        Top = 295
        Width = 125
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'FindNearest'
        TabOrder = 11
        OnClick = btnFindNearestClick
      end
      object Button23: TButton
        Left = 137
        Top = 335
        Width = 125
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'FindKey'
        TabOrder = 12
        OnClick = Button23Click
      end
    end
    object TabSheet4: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Master/Detail'
      DesignSize = (
        281
        541)
      object DBGrid2: TDBGrid
        Left = -2
        Top = 137
        Width = 279
        Height = 302
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsMaster
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 281
        Height = 130
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        BevelOuter = bvLowered
        TabOrder = 1
        object Label18: TLabel
          Left = 9
          Top = 9
          Width = 264
          Height = 112
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 
            'This shows masterdetail relations, by using a TTable as master, ' +
            'and copying the contents of another TTable into the memory table' +
            ' and then specifying mastersource/fields and indexfields on the ' +
            'memory table.'
          WordWrap = True
        end
        object Button12: TButton
          Left = 47
          Top = 95
          Width = 184
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Open Master/Detail'
          TabOrder = 0
          OnClick = Button12Click
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 518
        Width = 281
        Height = 23
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alBottom
        BevelOuter = bvLowered
        TabOrder = 2
        object Label27: TLabel
          Left = 10
          Top = 5
          Width = 43
          Height = 16
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Rec.no'
        end
        object lMasterRecNo: TLabel
          Left = 108
          Top = 5
          Width = 4
          Height = 16
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '-'
        end
      end
      object DBNavigator2: TDBNavigator
        Left = 0
        Top = 487
        Width = 281
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataSource = dsMaster
        Align = alBottom
        Kind = dbnHorizontal
        TabOrder = 3
      end
      object Button30: TButton
        Left = 4
        Top = 447
        Width = 157
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Sort on detail EmpNo'
        TabOrder = 4
        OnClick = Button30Click
      end
    end
    object TabSheet7: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Bookmarks'
      ImageIndex = 4
      object Label11: TLabel
        Left = 128
        Top = 0
        Width = 149
        Height = 70
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 
          'Gets a bookmark which can be used to reposition to the same plac' +
          'e later'
        WordWrap = True
      end
      object Label29: TLabel
        Left = 128
        Top = 69
        Width = 149
        Height = 80
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Goto the bookmark retrieved by Get Bookmark'
        WordWrap = True
      end
      object btnGetBookmark: TButton
        Left = 10
        Top = 10
        Width = 109
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Get bookmark'
        TabOrder = 0
        OnClick = btnGetBookmarkClick
      end
      object btnGotoBookmark: TButton
        Left = 10
        Top = 69
        Width = 109
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Goto bookmark'
        TabOrder = 1
        OnClick = btnGotoBookmarkClick
      end
    end
    object TabSheet8: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Indexes'
      ImageIndex = 5
      OnEnter = TabSheet8Enter
      object Label30: TLabel
        Left = 128
        Top = 20
        Width = 139
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Rebuild all defined indexes.'
        WordWrap = True
      end
      object Label31: TLabel
        Left = 10
        Top = 69
        Width = 149
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Specify index to use.'
        WordWrap = True
      end
      object Label32: TLabel
        Left = 148
        Top = 233
        Width = 129
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Add COLOR index.'
        WordWrap = True
      end
      object Label33: TLabel
        Left = 148
        Top = 311
        Width = 129
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Delete COLOR index.'
        WordWrap = True
      end
      object Label48: TLabel
        Left = 148
        Top = 272
        Width = 129
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Add index Period<70'
        WordWrap = True
      end
      object btnRebuildIdx: TButton
        Left = 10
        Top = 20
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Rebuild'
        TabOrder = 0
        OnClick = btnRebuildIdxClick
      end
      object cbIndexes: TComboBox
        Left = 10
        Top = 89
        Width = 247
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        TabOrder = 1
        OnChange = cbIndexesChange
      end
      object btnAddIndex: TButton
        Left = 4
        Top = 226
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Add Index'
        TabOrder = 2
        OnClick = btnAddIndexClick
      end
      object btnDeleteIndex: TButton
        Left = 4
        Top = 305
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Delete Index'
        TabOrder = 3
        OnClick = btnDeleteIndexClick
      end
      object chbColorUnique: TCheckBox
        Left = 10
        Top = 128
        Width = 119
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Unique index'
        TabOrder = 4
      end
      object chbColorDescending: TCheckBox
        Left = 10
        Top = 156
        Width = 119
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Descending'
        TabOrder = 5
      end
      object Button24: TButton
        Left = 4
        Top = 266
        Width = 119
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Add filtered Index'
        TabOrder = 6
        OnClick = Button24Click
      end
      object chbNullIsFirst: TCheckBox
        Left = 10
        Top = 185
        Width = 247
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Null is first (MSSQL vs ORACLE style)'
        TabOrder = 7
      end
    end
    object TabSheet10: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Versioning'
      ImageIndex = 7
      object Label36: TLabel
        Left = 118
        Top = 20
        Width = 149
        Height = 70
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 
          'Define the base for versioning. Only changes from this checkpoin' +
          't will be versioned.'
        WordWrap = True
      end
      object Label37: TLabel
        Left = 118
        Top = 177
        Width = 159
        Height = 51
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Save versioning delta to the file '#39'c:\deltas.dat'#39'.'
        WordWrap = True
      end
      object Label38: TLabel
        Left = 118
        Top = 236
        Width = 159
        Height = 80
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 
          'If checked keep all versions of a record, otherwise keep only ne' +
          'west and original.'
        WordWrap = True
      end
      object Label39: TLabel
        Left = 118
        Top = 98
        Width = 149
        Height = 71
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Using the TDemoDeltaHandler resolves all changes.'
        WordWrap = True
      end
      object Button14: TButton
        Left = 10
        Top = 20
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Checkpoint'
        TabOrder = 0
        OnClick = Button14Click
      end
      object chbVersionAll: TCheckBox
        Left = 10
        Top = 246
        Width = 90
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 1
        OnClick = chbVersionAllClick
      end
      object Button15: TButton
        Left = 10
        Top = 177
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Save'
        TabOrder = 2
        OnClick = Button15Click
      end
      object Button16: TButton
        Left = 10
        Top = 98
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Resolve'
        TabOrder = 3
        OnClick = Button16Click
      end
    end
    object TabSheet11: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Snapshots'
      ImageIndex = 8
      object Label40: TLabel
        Left = 158
        Top = 20
        Width = 119
        Height = 60
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Get a snapshot into a variant variable.'
        WordWrap = True
      end
      object Label41: TLabel
        Left = 158
        Top = 89
        Width = 119
        Height = 60
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Set the table contents from a snapshot.'
        WordWrap = True
      end
      object Button17: TButton
        Left = 10
        Top = 20
        Width = 139
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Save to snapshot'
        TabOrder = 0
        OnClick = Button17Click
      end
      object Button18: TButton
        Left = 10
        Top = 89
        Width = 139
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Load from snapshot'
        TabOrder = 1
        OnClick = Button18Click
      end
    end
    object TabSheet12: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Transactions'
      ImageIndex = 9
      object Label42: TLabel
        Left = 10
        Top = 512
        Width = 103
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Transaction level'
      end
      object lTransactionLevel: TLabel
        Left = 158
        Top = 512
        Width = 23
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'N/A'
      end
      object Label43: TLabel
        Left = 148
        Top = 10
        Width = 129
        Height = 188
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 
          'Starts a new transaction (several transactions can be active at ' +
          'the same time but remember to commit or rollback as many times a' +
          's you have started a transaction.'
        WordWrap = True
      end
      object Label44: TLabel
        Left = 150
        Top = 207
        Width = 129
        Height = 80
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Keep the changes made since last start transaction.'
        WordWrap = True
      end
      object Label45: TLabel
        Left = 150
        Top = 295
        Width = 129
        Height = 71
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Discard all changes made since start transaction.'
        WordWrap = True
      end
      object Button19: TButton
        Left = 10
        Top = 10
        Width = 119
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Start transaction'
        TabOrder = 0
        OnClick = Button19Click
      end
      object Button20: TButton
        Left = 10
        Top = 207
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Commit'
        TabOrder = 1
        OnClick = Button20Click
      end
      object Button21: TButton
        Left = 10
        Top = 295
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Rollback'
        TabOrder = 2
        OnClick = Button21Click
      end
    end
    object TabSheet13: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Filtering/Ranges'
      ImageIndex = 10
      object Label28: TLabel
        Left = 10
        Top = 10
        Width = 29
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Filter'
      end
      object Label22: TLabel
        Left = 10
        Top = 89
        Width = 119
        Height = 40
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = False
        Caption = 'Set range where 50<Period<70'
        WordWrap = True
      end
      object Label23: TLabel
        Left = 10
        Top = 138
        Width = 80
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Cancel range'
      end
      object eFilter: TEdit
        Left = 64
        Top = 10
        Width = 198
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
        Text = 'PERIOD>10'
      end
      object TableFilteredCheckBox: TCheckBox
        Left = 9
        Top = 43
        Width = 101
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Use Filter'
        TabOrder = 1
        Visible = False
        OnClick = TableFilteredCheckBoxClick
      end
      object btnSetFilter: TButton
        Left = 143
        Top = 39
        Width = 119
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Set filter'
        TabOrder = 2
        OnClick = btnSetFilterClick
      end
      object btnSetRange: TButton
        Left = 167
        Top = 89
        Width = 93
        Height = 27
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Set range'
        TabOrder = 3
        OnClick = btnSetRangeClick
      end
      object btnCancelRange: TButton
        Left = 167
        Top = 128
        Width = 93
        Height = 27
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Cancel range'
        TabOrder = 4
        OnClick = btnCancelRangeClick
      end
    end
    object TabSheet9: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Group by'
      ImageIndex = 10
      DesignSize = (
        281
        541)
      object Label49: TLabel
        Left = 4
        Top = 49
        Width = 94
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Resulting group'
      end
      object Button27: TButton
        Left = 4
        Top = 10
        Width = 123
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Group by Color'
        TabOrder = 0
        OnClick = Button27Click
      end
      object DBGrid3: TDBGrid
        Left = 4
        Top = 65
        Width = 272
        Height = 408
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = DataSource2
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object Button28: TButton
        Left = 134
        Top = 10
        Width = 123
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Group global'
        TabOrder = 2
        OnClick = Button28Click
      end
      object Button29: TButton
        Left = 9
        Top = 487
        Width = 168
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Sort on Color_COUNT'
        TabOrder = 3
        OnClick = Button29Click
      end
    end
    object TabSheet14: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Aggregate'
      ImageIndex = 11
      object Button31: TButton
        Left = 4
        Top = 4
        Width = 272
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Aggregate Max on ColorField'
        TabOrder = 0
        OnClick = Button31Click
      end
      object Button32: TButton
        Left = 5
        Top = 42
        Width = 272
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Aggregate StdDev/Average on ColorField'
        TabOrder = 1
        OnClick = Button32Click
      end
    end
  end
  object Panel7: TPanel
    Left = 0
    Top = 656
    Width = 938
    Height = 38
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Label46: TLabel
      Left = 10
      Top = 10
      Width = 58
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Progress:'
    end
    object lProgress: TLabel
      Left = 79
      Top = 10
      Width = 23
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'N/A'
    end
  end
  object DataSource1: TDataSource
    DataSet = kbmMemTable1
    Left = 448
    Top = 264
  end
  object kbmMemTable1: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    AutoIncMinValue = -1
    FieldDefs = <>
    AutoReposition = True
    IndexDefs = <>
    SortOptions = []
    Performance = mtpfSmall
    PersistentFile = 'C:\test_org.csv'
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    LoadedCompletely = False
    SavedCompletely = False
    EnableVersioning = True
    VersioningMode = mtvmAllSinceCheckPoint
    FilterOptions = [foNoPartialCompare]
    Version = '7.73.00 Standard Edition'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    OnProgress = kbmMemTable1Progress
    OnCompressBlobStream = kbmMemTable1CompressBlobStream
    OnDecompressBlobStream = kbmMemTable1DecompressBlobStream
    AfterEdit = kbmMemTable1AfterEdit
    AfterPost = kbmMemTable1AfterScroll
    AfterScroll = kbmMemTable1AfterScroll
    OnCalcFields = MemTable1CalcFields
    OnFilterRecord = kbmMemTable1FilterRecord
    Left = 296
    Top = 384
  end
  object dsMaster: TDataSource
    DataSet = tMaster
    Left = 352
    Top = 216
  end
  object sfBinary: TkbmBinaryStreamFormat
    Version = '5.00'
    sfUsingIndex = [sfSaveUsingIndex]
    sfData = [sfSaveData, sfLoadData]
    sfCalculated = []
    sfLookup = []
    sfNonVisible = [sfSaveNonVisible, sfLoadNonVisible]
    sfBlobs = [sfSaveBlobs, sfLoadBlobs]
    sfDef = [sfSaveDef, sfLoadDef]
    sfIndexDef = [sfSaveIndexDef, sfLoadIndexDef]
    sfFiltered = [sfSaveFiltered]
    sfIgnoreRange = [sfSaveIgnoreRange]
    sfIgnoreMasterDetail = [sfSaveIgnoreMasterDetail]
    sfDeltas = []
    sfDontFilterDeltas = []
    sfAppend = []
    sfFieldKind = [sfSaveFieldKind]
    sfFromStart = [sfLoadFromStart]
    sfDataTypeHeader = [sfSaveDataTypeHeader, sfLoadDataTypeHeader]
    sfDisplayWidth = []
    BufferSize = 16384
    Left = 299
    Top = 415
  end
  object sfCSV: TkbmCSVStreamFormat
    CommentChar = #0
    EscapeChar = '%'
    DefaultStringFieldSize = 255
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    sfLocalFormat = [sfLoadAsASCII]
    sfQuoteOnlyStrings = []
    sfNoHeader = []
    Version = '3.10'
    sfData = [sfSaveData, sfLoadData]
    sfCalculated = []
    sfLookup = []
    sfNonVisible = [sfSaveNonVisible, sfLoadNonVisible]
    sfBlobs = [sfSaveBlobs, sfLoadBlobs]
    sfDef = [sfSaveDef, sfLoadDef]
    sfIndexDef = [sfSaveIndexDef, sfLoadIndexDef]
    sfPlaceHolders = []
    sfFiltered = [sfSaveFiltered]
    sfIgnoreRange = [sfSaveIgnoreRange]
    sfIgnoreMasterDetail = [sfSaveIgnoreMasterDetail]
    sfDeltas = []
    sfDontFilterDeltas = []
    sfAppend = []
    sfFieldKind = [sfSaveFieldKind]
    sfFromStart = [sfLoadFromStart]
    sfDisplayWidth = []
    sfAutoInc = []
    UseFieldDisplayName = False
    Left = 331
    Top = 415
  end
  object sfBinaryWithDeltas: TkbmBinaryStreamFormat
    Version = '5.00'
    sfUsingIndex = [sfSaveUsingIndex]
    sfData = [sfSaveData, sfLoadData]
    sfCalculated = []
    sfLookup = []
    sfNonVisible = [sfSaveNonVisible, sfLoadNonVisible]
    sfBlobs = [sfSaveBlobs, sfLoadBlobs]
    sfDef = [sfSaveDef, sfLoadDef]
    sfIndexDef = [sfSaveIndexDef, sfLoadIndexDef]
    sfFiltered = [sfSaveFiltered]
    sfIgnoreRange = [sfSaveIgnoreRange]
    sfIgnoreMasterDetail = [sfSaveIgnoreMasterDetail]
    sfDeltas = [sfSaveDeltas]
    sfDontFilterDeltas = []
    sfAppend = []
    sfFieldKind = [sfSaveFieldKind]
    sfFromStart = [sfLoadFromStart]
    sfDataTypeHeader = [sfSaveDataTypeHeader, sfLoadDataTypeHeader]
    sfDisplayWidth = []
    BufferSize = 16384
    Left = 363
    Top = 415
  end
  object kbmMemTable2: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    LoadedCompletely = False
    SavedCompletely = False
    FilterOptions = []
    Version = '7.73.00 Standard Edition'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    OnUserAggregate = kbmMemTable2UserAggregate
    Left = 171
    Top = 267
  end
  object DataSource2: TDataSource
    DataSet = kbmMemTable2
    Left = 172
    Top = 300
  end
  object tMaster: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <
      item
        Name = 'CustNo'
        DataType = ftFloat
      end
      item
        Name = 'Company'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Addr1'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Addr2'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'City'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'State'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Zip'
        DataType = ftString
        Size = 10
      end
      item
        Name = 'Country'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Phone'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'FAX'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'TaxRate'
        DataType = ftFloat
      end
      item
        Name = 'Contact'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'LastInvoiceDate'
        DataType = ftDateTime
      end>
    IndexDefs = <>
    SortOptions = []
    PersistentFile = 'C:\svn_c4d\kbmmemtable\trunk\demos\Demo\win32\customer.csv'
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    LoadedCompletely = True
    SavedCompletely = True
    FilterOptions = []
    Version = '7.73.00 Standard Edition'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 299
    Top = 216
  end
  object tDetailTemplate: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <
      item
        Name = 'OrderNo'
        DataType = ftFloat
      end
      item
        Name = 'CustNo'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'SaleDate'
        DataType = ftDateTime
      end
      item
        Name = 'ShipDate'
        DataType = ftDateTime
      end
      item
        Name = 'EmpNo'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'ShipToContact'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'ShipToAddr1'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'ShipToAddr2'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'ShipToCity'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'ShipToState'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'ShipToZip'
        DataType = ftString
        Size = 10
      end
      item
        Name = 'ShipToCountry'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'ShipToPhone'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'ShipVIA'
        DataType = ftString
        Size = 7
      end
      item
        Name = 'PO'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'Terms'
        DataType = ftString
        Size = 6
      end
      item
        Name = 'PaymentMethod'
        DataType = ftString
        Size = 7
      end
      item
        Name = 'ItemsTotal'
        DataType = ftCurrency
      end
      item
        Name = 'TaxRate'
        DataType = ftFloat
      end
      item
        Name = 'Freight'
        DataType = ftCurrency
      end
      item
        Name = 'AmountPaid'
        DataType = ftCurrency
      end>
    IndexDefs = <>
    SortOptions = []
    PersistentFile = 'C:\svn_c4d\kbmmemtable\trunk\demos\Demo\win32\orders.csv'
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    LoadedCompletely = False
    SavedCompletely = True
    FilterOptions = []
    Version = '7.73.00 Standard Edition'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 299
    Top = 264
    object tDetailTemplateOrderNo: TFloatField
      FieldName = 'OrderNo'
    end
    object tDetailTemplateCustNo: TFloatField
      FieldName = 'CustNo'
      Required = True
    end
    object tDetailTemplateSaleDate: TDateTimeField
      FieldName = 'SaleDate'
    end
    object tDetailTemplateShipDate: TDateTimeField
      FieldName = 'ShipDate'
    end
    object tDetailTemplateEmpNo: TIntegerField
      FieldName = 'EmpNo'
      Required = True
    end
    object tDetailTemplateShipToContact: TStringField
      FieldName = 'ShipToContact'
    end
    object tDetailTemplateShipToAddr1: TStringField
      FieldName = 'ShipToAddr1'
      Size = 30
    end
    object tDetailTemplateShipToAddr2: TStringField
      FieldName = 'ShipToAddr2'
      Size = 30
    end
    object tDetailTemplateShipToCity: TStringField
      FieldName = 'ShipToCity'
      Size = 15
    end
    object tDetailTemplateShipToState: TStringField
      FieldName = 'ShipToState'
    end
    object tDetailTemplateShipToZip: TStringField
      FieldName = 'ShipToZip'
      Size = 10
    end
    object tDetailTemplateShipToCountry: TStringField
      FieldName = 'ShipToCountry'
    end
    object tDetailTemplateShipToPhone: TStringField
      FieldName = 'ShipToPhone'
      Size = 15
    end
    object tDetailTemplateShipVIA: TStringField
      FieldName = 'ShipVIA'
      Size = 7
    end
    object tDetailTemplatePO: TStringField
      FieldName = 'PO'
      Size = 15
    end
    object tDetailTemplateTerms: TStringField
      FieldName = 'Terms'
      Size = 6
    end
    object tDetailTemplatePaymentMethod: TStringField
      FieldName = 'PaymentMethod'
      Size = 7
    end
    object tDetailTemplateItemsTotal: TCurrencyField
      FieldName = 'ItemsTotal'
    end
    object tDetailTemplateTaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object tDetailTemplateFreight: TCurrencyField
      FieldName = 'Freight'
    end
    object tDetailTemplateAmountPaid: TCurrencyField
      FieldName = 'AmountPaid'
    end
  end
  object tBiolife: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    LoadedCompletely = False
    SavedCompletely = False
    FilterOptions = []
    Version = '7.73.00 Standard Edition'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 299
    Top = 160
  end
end
