object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Basic Client Demo'
  ClientHeight = 280
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object SettingsButton: TButton
      Left = 8
      Top = 8
      Width = 81
      Height = 25
      Action = ConnectionSettingsAction
      TabOrder = 0
    end
    object ConnectButton: TButton
      Left = 96
      Top = 8
      Width = 81
      Height = 25
      Action = ConnectionOpenAction
      TabOrder = 1
    end
    object DisconnectButton: TButton
      Left = 184
      Top = 8
      Width = 81
      Height = 25
      Action = ConnectionCloseAction
      TabOrder = 2
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 497
    Height = 97
    Caption = 'Reading a Value of Holding Register'
    TabOrder = 1
    object ReadAddressLabel: TLabel
      Left = 8
      Top = 36
      Width = 85
      Height = 13
      Caption = 'R&egister address:'
      FocusControl = ReadAddressEdit
    end
    object ReadStatusLabel: TLabel
      Left = 8
      Top = 64
      Width = 82
      Height = 13
      Caption = 'Last read status:'
    end
    object ReadAddressEdit: TEdit
      Left = 120
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '<insert address here>'
    end
    object ReadButton: TButton
      Left = 248
      Top = 32
      Width = 81
      Height = 21
      Action = RegisterReadAction
      TabOrder = 1
    end
    object ReadStatusEdit: TEdit
      Left = 120
      Top = 64
      Width = 369
      Height = 21
      TabStop = False
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      Text = 'n/a'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 152
    Width = 497
    Height = 121
    Caption = 'Writing a Value of Holding Register'
    TabOrder = 2
    object WriteAddressLabel: TLabel
      Left = 8
      Top = 36
      Width = 85
      Height = 13
      Caption = 'Re&gister address:'
      FocusControl = WriteAddressEdit
    end
    object WriteStatusLabel: TLabel
      Left = 8
      Top = 88
      Width = 84
      Height = 13
      Caption = 'Last write status:'
    end
    object WriteValueLabel: TLabel
      Left = 8
      Top = 60
      Width = 70
      Height = 13
      Caption = '&Value to write:'
      FocusControl = WriteValueEdit
    end
    object WriteAddressEdit: TEdit
      Left = 120
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '<insert address here>'
    end
    object WriteButton: TButton
      Left = 248
      Top = 56
      Width = 81
      Height = 21
      Action = RegisterWriteAction
      TabOrder = 2
    end
    object WriteStatusEdit: TEdit
      Left = 120
      Top = 88
      Width = 369
      Height = 21
      TabStop = False
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 3
      Text = 'n/a'
    end
    object WriteValueEdit: TEdit
      Left = 120
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '<insert value here>'
    end
  end
  object ModbusConnection1: TModbusConnection
    ThreadPriority = tpHigher
    Left = 272
    Top = 8
  end
  object ModbusClient1: TModbusClient
    Connection = ModbusConnection1
    OnHoldingRegistersRead = ModbusClient1HoldingRegistersRead
    OnSingleRegisterWrite = ModbusClient1SingleRegisterWrite
    Left = 304
    Top = 8
  end
  object ActionList1: TActionList
    Left = 336
    Top = 8
    object ConnectionSettingsAction: TAction
      Category = 'Connection'
      Caption = '&Settings...'
      Hint = 'Edit Modbus connection settings'
      OnExecute = ConnectionSettingsActionExecute
      OnUpdate = ConnectionSettingsActionUpdate
    end
    object ConnectionOpenAction: TAction
      Category = 'Connection'
      Caption = '&Connect'
      Hint = 'Open Modbus connection'
      OnExecute = ConnectionOpenActionExecute
      OnUpdate = ConnectionOpenActionUpdate
    end
    object ConnectionCloseAction: TAction
      Category = 'Connection'
      Caption = '&Disconnect'
      Hint = 'Close Modbus connection'
      OnExecute = ConnectionCloseActionExecute
      OnUpdate = ConnectionCloseActionUpdate
    end
    object RegisterReadAction: TAction
      Category = 'Modbus commands'
      Caption = '&Read'
      OnExecute = RegisterReadActionExecute
      OnUpdate = RegisterReadActionUpdate
    end
    object RegisterWriteAction: TAction
      Category = 'Modbus commands'
      Caption = '&Write'
      OnExecute = RegisterWriteActionExecute
      OnUpdate = RegisterWriteActionUpdate
    end
  end
end
