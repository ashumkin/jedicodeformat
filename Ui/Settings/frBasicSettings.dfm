inherited frBasic: TfrBasic
  Width = 456
  Height = 198
  OnResize = FrameResize
  object lblOutput: TLabel
    Left = 4
    Top = 172
    Width = 48
    Height = 13
    Caption = 'Output file'
  end
  object lblInput: TLabel
    Left = 4
    Top = 148
    Width = 40
    Height = 13
    Caption = 'Input file'
  end
  object sbOpen: TSpeedButton
    Left = 430
    Top = 144
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = sbOpenClick
  end
  object rgFileRecurse: TRadioGroup
    Left = 4
    Top = 64
    Width = 225
    Height = 73
    Caption = 'Files'
    ItemIndex = 0
    Items.Strings = (
      'Single file'
      'All files in directory'
      'All files in directory and all subdirectories')
    TabOrder = 0
    OnClick = rgFileRecurseClick
  end
  object rgBackup: TRadioGroup
    Left = 231
    Top = 64
    Width = 154
    Height = 73
    Caption = 'Backup'
    ItemIndex = 0
    Items.Strings = (
      'No backup'
      'Backup to seperate file'
      'Output to seperate file')
    TabOrder = 1
    OnClick = rgBackupClick
  end
  object edtInput: TEdit
    Left = 72
    Top = 144
    Width = 357
    Height = 21
    MaxLength = 255
    TabOrder = 2
    OnDragDrop = edtInputDragDrop
    OnDragOver = edtInputDragOver
    OnKeyUp = edtInputKeyUp
  end
  object edtOutput: TEdit
    Left = 72
    Top = 172
    Width = 381
    Height = 21
    Color = clBtnFace
    MaxLength = 255
    ReadOnly = True
    TabOrder = 3
  end
  object rgMode: TRadioGroup
    Left = 4
    Top = 4
    Width = 89
    Height = 57
    Caption = 'Mode'
    Items.Strings = (
      'Clarify'
      'Obfuscate')
    TabOrder = 4
    OnClick = rgModeClick
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '*.pas'
    Filter = 
      'Delphi Source|*.pas|Delphi Project Source|*.dpr|text file|*.txt|' +
      'All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a Delphi source file'
    Left = 244
    Top = 20
  end
end
