inherited fFiles: TfFiles
  Width = 338
  Height = 281
  OnResize = FrameResize
  object sbSpecifedDir: TSpeedButton
    Left = 132
    Top = 156
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = sbSpecifedDirClick
  end
  object Label1: TLabel
    Left = 6
    Top = 160
    Width = 118
    Height = 13
    Caption = 'Select specified directory'
  end
  object lblBackupFileExt: TLabel
    Left = 144
    Top = 16
    Width = 101
    Height = 13
    Caption = 'Backup file extension'
  end
  object lblOutputFileExt: TLabel
    Left = 144
    Top = 40
    Width = 96
    Height = 13
    Caption = 'Output file extension'
  end
  object rgLogLevel: TRadioGroup
    Left = 6
    Top = 6
    Width = 113
    Height = 73
    Caption = 'Log file detail level'
    Items.Strings = (
      'Errors only'
      'File'
      'Token')
    TabOrder = 0
  end
  object rgLogDir: TRadioGroup
    Left = 6
    Top = 80
    Width = 267
    Height = 73
    Caption = 'Log file directory'
    Items.Strings = (
      'Temp'
      'Application'
      'Specified')
    TabOrder = 1
  end
  object btnViewLog: TButton
    Left = 6
    Top = 228
    Width = 89
    Height = 29
    Caption = 'View Log now'
    TabOrder = 2
    OnClick = btnViewLogClick
  end
  object cbViewLog: TCheckBox
    Left = 6
    Top = 188
    Width = 137
    Height = 17
    Caption = 'View log after each run'
    TabOrder = 3
  end
  object edtBackupExt: TEdit
    Left = 260
    Top = 12
    Width = 45
    Height = 21
    MaxLength = 3
    TabOrder = 4
  end
  object edtOutputExt: TEdit
    Left = 260
    Top = 40
    Width = 45
    Height = 21
    MaxLength = 3
    TabOrder = 5
  end
  object cbLogTime: TCheckBox
    Left = 6
    Top = 208
    Width = 133
    Height = 17
    Caption = 'Log time to process'
    TabOrder = 6
  end
end
