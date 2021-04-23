object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Sequencer Demo'
  ClientHeight = 286
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object StartButton: TButton
    Left = 8
    Top = 8
    Width = 81
    Height = 25
    Action = StartAction
    TabOrder = 0
  end
  object StopButton: TButton
    Left = 96
    Top = 8
    Width = 81
    Height = 25
    Action = StopAction
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 409
    Height = 241
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ModbusConnection1: TModbusConnection
    Active = True
    ThreadPriority = tpHigher
    Left = 296
    Top = 8
  end
  object ModbusClient1: TModbusClient
    Connection = ModbusConnection1
    Left = 328
    Top = 8
  end
  object ModbusClient2: TModbusClient
    Connection = ModbusConnection1
    ServerAddress = 2
    Left = 360
    Top = 8
  end
  object ActionList1: TActionList
    Left = 392
    Top = 8
    object StartAction: TAction
      Caption = '&Start...'
      OnExecute = StartActionExecute
      OnUpdate = StartActionUpdate
    end
    object StopAction: TAction
      Caption = 'S&top...'
      OnExecute = StopActionExecute
      OnUpdate = StopActionUpdate
    end
  end
end
