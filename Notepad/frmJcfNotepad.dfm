object fmJCFNotepad: TfmJCFNotepad
  Left = 165
  Top = 173
  Caption = 'JCF Notepad'
  ClientHeight = 412
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object sb1: TStatusBar
    Left = 0
    Top = 393
    Width = 635
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 50
      end>
    ParentFont = True
    UseSystemFont = False
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 26
    Align = alTop
    TabOrder = 1
    object sbLoad: TSpeedButton
      Left = 2
      Top = 2
      Width = 45
      Height = 22
      Action = actOpen
      ParentShowHint = False
      ShowHint = True
    end
    object sbSave: TSpeedButton
      Left = 50
      Top = 2
      Width = 50
      Height = 22
      Action = actSave
    end
    object sbGo: TSpeedButton
      Left = 101
      Top = 2
      Width = 54
      Height = 22
      Action = actGo
    end
    object sbClear: TSpeedButton
      Left = 157
      Top = 2
      Width = 52
      Height = 22
      Action = actClear
    end
  end
  object pcPages: TPageControl
    Left = 0
    Top = 26
    Width = 635
    Height = 367
    ActivePage = tsInput
    Align = alClient
    TabOrder = 2
    OnChange = pcPagesChange
    object tsInput: TTabSheet
      Caption = 'Input'
      object mInput: TJvMemo
        Left = 0
        Top = 0
        Width = 627
        Height = 337
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        OnClick = mInputClick
        OnEnter = mInputEnter
        OnKeyDown = mInputKeyDown
        OnKeyUp = mInputKeyDown
        OnMouseUp = mInputMouseUp
      end
    end
    object tsOutput: TTabSheet
      Caption = 'Output'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblMessages: TLabel
        Left = 11
        Top = 155
        Width = 51
        Height = 15
        Caption = 'Messages'
      end
      object mOutput: TJvMemo
        Left = 11
        Top = 15
        Width = 171
        Height = 82
        AutoSize = False
        ClipboardCommands = [caCopy]
        MaxLines = 0
        HideCaret = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        OnClick = mOutputClick
        OnEnter = mOutputEnter
        OnKeyUp = mOutputKeyUp
      end
      object mMessages: TJvMemo
        Left = 11
        Top = 175
        Width = 171
        Height = 82
        AutoSize = False
        ClipboardCommands = [caCopy]
        MaxLines = 0
        HideCaret = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
  end
  object ActionList1: TActionList
    Left = 364
    Top = 17
    object actOpen: TAction
      Caption = '&Open...'
      Hint = 'Open an input file'
      OnExecute = actOpenExecute
    end
    object actSave: TAction
      Caption = '&Save...'
      Hint = 'Save the output'
      OnExecute = actSaveExecute
    end
    object actGo: TAction
      Caption = '&Go'
      Hint = 'Run the formatter on the input'
      OnExecute = actGoExecute
    end
    object actClear: TAction
      Caption = '&Clear'
      OnExecute = actClearExecute
    end
    object actCopy: TAction
      Caption = 'Copy'
      Hint = 'Copy output to clipboard'
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
    object actPaste: TAction
      Caption = '&Paste'
      Hint = 'Paste from clipboard to input'
      ShortCut = 16470
      OnExecute = actPasteExecute
    end
    object ActCut: TAction
      Caption = '&Cut'
      ShortCut = 16472
      OnExecute = ActCutExecute
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.pas'
    Left = 496
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    Left = 444
    Top = 20
  end
  object MainMenu1: TMainMenu
    Left = 400
    Top = 8
    object mnuFile: TMenuItem
      Caption = '&File'
      OnClick = actCopyExecute
      object mnuFileOpen: TMenuItem
        Action = actOpen
      end
      object mnuFileSaveOut: TMenuItem
        Action = actSave
        Caption = '&Save Output...'
      end
      object mnuFileSaveIn: TMenuItem
        Caption = 'Save &Input'
        OnClick = mnuFileSaveInClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnuExitClick
      end
    end
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      object mnuEditCut: TMenuItem
        Action = ActCut
      end
      object mnuEditCopy: TMenuItem
        Action = actCopy
        Hint = 'Copy to clipboard'
      end
      object mnuEditPaste: TMenuItem
        Action = actPaste
      end
      object mnuEditSelectAll: TMenuItem
        Caption = 'Select &All'
        ShortCut = 16449
        OnClick = mnuEditSelectAllClick
      end
      object mnuEditCopyOutput: TMenuItem
        Caption = 'Copy &Output'
        ShortCut = 16463
        OnClick = mnuEditCopyOutputClick
      end
      object mnuEditCopyMessages: TMenuItem
        Caption = 'Copy &Messages'
        ShortCut = 16461
        OnClick = mnuEditCopyMessagesClick
      end
    end
    object mnuFormat: TMenuItem
      Caption = '&Format'
      object mnuEditGo: TMenuItem
        Action = actGo
        ShortCut = 16455
      end
      object mnuEditClear: TMenuItem
        Action = actClear
        ShortCut = 16474
      end
    end
    object mnuSettings: TMenuItem
      Caption = '&Settings'
      object mnuShowRegSetting: TMenuItem
        Caption = '&Registry Settings...'
        GroupIndex = 1
        OnClick = mnuShowRegSettingClick
      end
      object mnuFormatSettings: TMenuItem
        Caption = '&Format Settings...'
        GroupIndex = 1
        OnClick = mnuFormatSettingsClick
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents'
        Enabled = False
        OnClick = Contents1Click
      end
      object mnuHelpAbout: TMenuItem
        Caption = '&About'
        OnClick = mnuHelpAboutClick
      end
    end
  end
  object mruFiles: TJvMRUManager
    Duplicates = dupIgnore
    AccelDelimiter = adSpace
    Capacity = 9
    RecentMenu = mnuFile
    OnClick = mruFilesClick
    Left = 540
    Top = 24
  end
  object JvFormStorage1: TJvFormStorage
    AppStoragePath = 'fmJCFNotepad\'
    StoredValues = <>
    Left = 576
    Top = 8
  end
end
