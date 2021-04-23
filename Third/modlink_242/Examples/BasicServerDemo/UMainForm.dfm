object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Basic Server Demo'
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
  object Panel2: TPanel
    Left = 0
    Top = 239
    Width = 512
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ClearButton: TButton
      Left = 8
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 0
      OnClick = ClearButtonClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 512
    Height = 198
    Align = alClient
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ModbusConnection1: TModbusConnection
    ConnectionMode = cmServer
    ThreadPriority = tpHigher
    Left = 272
    Top = 8
  end
  object ModbusServer1: TModbusServer
    Connection = ModbusConnection1
    OnAcceptCommand = ModbusServer1AcceptCommand
    OnCanReadHoldingRegister = ModbusServer1CanReadHoldingRegister
    OnCanWriteHoldingRegister = ModbusServer1CanWriteHoldingRegister
    OnGetHoldingRegisterValue = ModbusServer1GetHoldingRegisterValue
    OnSetHoldingRegisterValue = ModbusServer1SetHoldingRegisterValue
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
  end
end
