object fmRegistrySettings: TfmRegistrySettings
  Left = 219
  Top = 231
  BorderStyle = bsDialog
  Caption = 'JCF Notepad registry settings'
  ClientHeight = 205
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object sbFile: TSpeedButton
    Left = 446
    Top = 8
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = sbFileClick
  end
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 92
    Height = 13
    Caption = 'Convert settings file'
  end
  object Label2: TLabel
    Left = 8
    Top = 46
    Width = 74
    Height = 13
    Caption = 'MRU max items'
  end
  object btnOK: TBitBtn
    Left = 152
    Top = 170
    Width = 75
    Height = 28
    TabOrder = 0
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 244
    Top = 170
    Width = 75
    Height = 28
    TabOrder = 1
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object eSettingsFile: TEdit
    Left = 110
    Top = 8
    Width = 333
    Height = 21
    TabOrder = 2
    OnKeyUp = eSettingsFileKeyUp
  end
  object btnClearMRU: TButton
    Left = 156
    Top = 42
    Width = 75
    Height = 25
    Caption = 'Clear MRU'
    TabOrder = 3
    OnClick = btnClearMRUClick
  end
  object eMRUMaxItems: TJvIntegerEdit
    Left = 110
    Top = 42
    Width = 40
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 4
    Value = 0
    MaxValue = 12
    MinValue = 0
    HasMaxValue = False
    HasMinValue = False
  end
  object rgShowParseTree: TRadioGroup
    Left = 8
    Top = 72
    Width = 185
    Height = 89
    Caption = 'Show parse tree during parse'
    Items.Strings = (
      'Always'
      'On parse error'
      'Never')
    TabOrder = 5
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '*.cfg'
    Left = 380
    Top = 36
  end
end
