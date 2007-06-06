object fmRegistrySettings: TfmRegistrySettings
  Left = 73
  Top = 151
  BorderStyle = bsDialog
  Caption = 'JCF Registry Settings'
  ClientHeight = 285
  ClientWidth = 578
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object pgPages: TPageControl
    Left = 0
    Top = 0
    Width = 578
    Height = 244
    ActivePage = tsLogFile
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = '&General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object sbFile: TSpeedButton
        Left = 548
        Top = 8
        Width = 21
        Height = 21
        Caption = '...'
        OnClick = sbFileClick
      end
      object Label1: TLabel
        Left = 8
        Top = 9
        Width = 105
        Height = 15
        Caption = 'Convert settings file'
      end
      object Label2: TLabel
        Left = 8
        Top = 191
        Width = 83
        Height = 15
        Caption = 'MRU max items'
      end
      object eSettingsFile: TEdit
        Left = 120
        Top = 8
        Width = 424
        Height = 28
        TabOrder = 0
        OnKeyUp = eSettingsFileKeyUp
      end
      object eMRUMaxItems: TJvValidateEdit
        Left = 98
        Top = 187
        Width = 37
        Height = 23
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        EditText = '0'
        MaxLength = 2
        MaxValue = 12.000000000000000000
        TabOrder = 1
      end
      object btnClearMRU: TButton
        Left = 140
        Top = 185
        Width = 69
        Height = 23
        Caption = 'Clear MRU'
        TabOrder = 2
        OnClick = btnClearMRUClick
      end
      object rgShowParseTree: TRadioGroup
        Left = 8
        Top = 104
        Width = 171
        Height = 69
        Caption = 'Show parse &tree during parse'
        Items.Strings = (
          '&Always'
          '&On parse error'
          '&Never')
        TabOrder = 3
      end
      object rgWriteSettingsFile: TRadioGroup
        Left = 8
        Top = 29
        Width = 171
        Height = 71
        Caption = '&Write settings file'
        Items.Strings = (
          '&Always'
          '&Fail quietly'
          '&Never')
        TabOrder = 4
      end
      object cbCheckMultibyteChars: TCheckBox
        Left = 200
        Top = 41
        Width = 179
        Height = 15
        Caption = 'Check for &Multibyte chars'
        TabOrder = 5
      end
    end
    object tsLogFile: TTabSheet
      Caption = '&Log file'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object sbSpecifedDir: TSpeedButton
        Left = 135
        Top = 146
        Width = 22
        Height = 20
        Caption = '...'
        OnClick = sbSpecifedDirClick
      end
      object Label3: TLabel
        Left = 5
        Top = 148
        Width = 131
        Height = 15
        Caption = 'Select specified directory'
      end
      object lblBackupFileExt: TLabel
        Left = 133
        Top = 15
        Width = 111
        Height = 15
        Caption = 'Backup file extension'
      end
      object lblOutputFileExt: TLabel
        Left = 133
        Top = 37
        Width = 110
        Height = 15
        Caption = 'Output file extension'
      end
      object rgLogLevel: TRadioGroup
        Left = 5
        Top = 5
        Width = 116
        Height = 68
        Caption = 'Log file detail level'
        Items.Strings = (
          'Errors only'
          'File'
          'Token')
        TabOrder = 0
      end
      object rgLogDir: TRadioGroup
        Left = 5
        Top = 74
        Width = 560
        Height = 67
        Caption = 'Log file directory'
        Items.Strings = (
          'Temp'
          'Application'
          'Specified')
        TabOrder = 3
      end
      object btnViewLog: TButton
        Left = 293
        Top = 179
        Width = 83
        Height = 27
        Caption = 'View Log now'
        TabOrder = 6
        OnClick = btnViewLogClick
      end
      object cbViewLog: TCheckBox
        Left = 5
        Top = 173
        Width = 170
        Height = 16
        Caption = 'View log after each run'
        TabOrder = 4
      end
      object edtBackupExt: TEdit
        Left = 240
        Top = 11
        Width = 41
        Height = 28
        MaxLength = 3
        TabOrder = 1
      end
      object edtOutputExt: TEdit
        Left = 240
        Top = 37
        Width = 41
        Height = 28
        MaxLength = 3
        TabOrder = 2
      end
      object cbLogTime: TCheckBox
        Left = 5
        Top = 192
        Width = 170
        Height = 16
        Caption = 'Log time taken to process'
        TabOrder = 5
      end
    end
    object tsExclusions: TTabSheet
      Caption = '&Exclusions'
      ImageIndex = 2
      OnResize = tsExclusionsResize
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblFilesCaption: TLabel
        Left = 8
        Top = 4
        Width = 255
        Height = 15
        Caption = 'Individual files to exclude from batch processing'
      end
      object lblDirsCaption: TLabel
        Left = 8
        Top = 109
        Width = 235
        Height = 15
        Caption = 'Directories to exclude from batch processing'
      end
      object mFiles: TJvMemo
        Left = 8
        Top = 19
        Width = 557
        Height = 85
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        TabOrder = 0
      end
      object mDirs: TJvMemo
        Left = 2
        Top = 126
        Width = 563
        Height = 89
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        TabOrder = 1
      end
    end
    object tsIde: TTabSheet
      Caption = 'IDE'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object cbEditorIntegration: TCheckBox
        Left = 8
        Top = 11
        Width = 161
        Height = 16
        Caption = '&Editor Integration'
        TabOrder = 0
      end
      object cbFormatBeforeSave: TCheckBox
        Left = 8
        Top = 44
        Width = 161
        Height = 15
        Caption = 'Format before &Save'
        TabOrder = 1
        OnClick = cbFormatBeforeSaveClick
      end
      object cbFormatAfterLoad: TCheckBox
        Left = 8
        Top = 27
        Width = 161
        Height = 16
        Caption = 'Format after &Load'
        TabOrder = 2
        OnClick = cbFormatAfterLoadClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 244
    Width = 578
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TBitBtn
      Left = 225
      Top = 7
      Width = 69
      Height = 28
      TabOrder = 0
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 306
      Top = 7
      Width = 69
      Height = 28
      TabOrder = 1
      OnClick = btnCancelClick
      Kind = bkCancel
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '*.cfg'
    Left = 20
    Top = 340
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Left = 56
    Top = 344
  end
end
