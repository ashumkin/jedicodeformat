object fmRegistrySettings: TfmRegistrySettings
  Left = 73
  Top = 151
  BorderStyle = bsDialog
  Caption = 'JCF Notepad registry settings'
  ClientHeight = 309
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pgPages: TPageControl
    Left = 0
    Top = 0
    Width = 478
    Height = 264
    ActivePage = tsLogFile
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = '&General'
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
        Top = 70
        Width = 74
        Height = 13
        Caption = 'MRU max items'
      end
      object eSettingsFile: TEdit
        Left = 110
        Top = 8
        Width = 333
        Height = 21
        TabOrder = 0
        OnKeyUp = eSettingsFileKeyUp
      end
      object eMRUMaxItems: TJvValidateEdit
        Left = 110
        Top = 66
        Width = 40
        Height = 21
        CheckChars = '01234567890'
        CriticalPoints.CheckPoints = cpNone
        CriticalPoints.ColorAbove = clBlue
        CriticalPoints.ColorBelow = clRed
        EditText = '0'
        MaxLength = 2
        MaxValue = 12.000000000000000000
        PasswordChar = #0
        TabOrder = 2
        Text = '0'
        Value = 0
      end
      object btnClearMRU: TButton
        Left = 156
        Top = 66
        Width = 75
        Height = 25
        Caption = 'Clear MRU'
        TabOrder = 3
        OnClick = btnClearMRUClick
      end
      object rgShowParseTree: TRadioGroup
        Left = 8
        Top = 96
        Width = 185
        Height = 89
        Caption = 'Show parse tree during parse'
        Items.Strings = (
          'Always'
          'On parse error'
          'Never')
        TabOrder = 4
      end
      object cbWriteSettingsFile: TCheckBox
        Left = 112
        Top = 36
        Width = 161
        Height = 17
        Caption = '&Write changes to settings file'
        TabOrder = 1
      end
    end
    object tsLogFile: TTabSheet
      Caption = '&Log file'
      ImageIndex = 1
      object sbSpecifedDir: TSpeedButton
        Left = 132
        Top = 156
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbSpecifedDirClick
      end
      object Label3: TLabel
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
        Width = 423
        Height = 73
        Caption = 'Log file directory'
        Items.Strings = (
          'Temp'
          'Application'
          'Specified')
        TabOrder = 3
      end
      object btnViewLog: TButton
        Left = 318
        Top = 194
        Width = 89
        Height = 29
        Caption = 'View Log now'
        TabOrder = 6
        OnClick = btnViewLogClick
      end
      object cbViewLog: TCheckBox
        Left = 6
        Top = 188
        Width = 137
        Height = 17
        Caption = 'View log after each run'
        TabOrder = 4
      end
      object edtBackupExt: TEdit
        Left = 260
        Top = 12
        Width = 45
        Height = 21
        MaxLength = 3
        TabOrder = 1
      end
      object edtOutputExt: TEdit
        Left = 260
        Top = 40
        Width = 45
        Height = 21
        MaxLength = 3
        TabOrder = 2
      end
      object cbLogTime: TCheckBox
        Left = 6
        Top = 208
        Width = 147
        Height = 17
        Caption = 'Log time taken to process'
        TabOrder = 5
      end
    end
    object tsExclusions: TTabSheet
      Caption = '&Exclusions'
      ImageIndex = 2
      OnResize = tsExclusionsResize
      object lblFilesCaption: TLabel
        Left = 8
        Top = 4
        Width = 225
        Height = 13
        Caption = 'Individual files to exclude from batch processing'
      end
      object lblDirsCaption: TLabel
        Left = 8
        Top = 118
        Width = 209
        Height = 13
        Caption = 'Directories to exclude from batch processing'
      end
      object mFiles: TJvMemo
        Left = 8
        Top = 20
        Width = 320
        Height = 93
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        TabOrder = 0
      end
      object mDirs: TJvMemo
        Left = 8
        Top = 134
        Width = 320
        Height = 97
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        TabOrder = 1
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 264
    Width = 478
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TBitBtn
      Left = 159
      Top = 9
      Width = 75
      Height = 30
      TabOrder = 0
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 247
      Top = 9
      Width = 75
      Height = 30
      TabOrder = 1
      OnClick = btnCancelClick
      Kind = bkCancel
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '*.cfg'
    Left = 380
    Top = 36
  end
end
