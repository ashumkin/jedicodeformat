object frmScratchpad: TfrmScratchpad
  Left = 192
  Top = 114
  Width = 696
  Height = 480
  Caption = 'Jedi Code format scratchpad'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sb1: TStatusBar
    Left = 0
    Top = 407
    Width = 688
    Height = 19
    Panels = <>
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 28
    Align = alTop
    TabOrder = 1
    object sbLoad: TSpeedButton
      Left = 2
      Top = 2
      Width = 49
      Height = 24
      Action = actOpen
      ParentShowHint = False
      ShowHint = True
    end
    object sbSave: TSpeedButton
      Left = 54
      Top = 2
      Width = 54
      Height = 24
      Action = actSave
    end
    object sbGo: TSpeedButton
      Left = 110
      Top = 2
      Width = 57
      Height = 24
      Action = actGo
    end
    object sbClear: TSpeedButton
      Left = 170
      Top = 2
      Width = 57
      Height = 24
      Action = actClear
    end
    object SpeedButton1: TSpeedButton
      Left = 290
      Top = 2
      Width = 57
      Height = 24
      Action = actCopy
    end
    object sbPaste: TSpeedButton
      Left = 230
      Top = 2
      Width = 57
      Height = 24
      Action = actPaste
    end
  end
  object pcPages: TPageControl
    Left = 0
    Top = 28
    Width = 688
    Height = 379
    ActivePage = tsInput
    Align = alClient
    TabOrder = 2
    OnChange = pcPagesChange
    object tsInput: TTabSheet
      Caption = 'Input'
      object mInput: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 351
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        OnKeyUp = mInputKeyUp
      end
    end
    object tsOutput: TTabSheet
      Caption = 'Output'
      ImageIndex = 1
      object lblMessages: TLabel
        Left = 16
        Top = 168
        Width = 48
        Height = 13
        Caption = 'Messages'
      end
      object mOutput: TMemo
        Left = 8
        Top = 16
        Width = 185
        Height = 89
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
      end
      object mMessages: TMemo
        Left = 32
        Top = 221
        Width = 185
        Height = 89
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
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.pas'
    Filter = 'Delphi source (*.pas, *.dpr)|*.pas; *.dpr|All files|*.*'
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
      object mnuFileSave: TMenuItem
        Action = actSave
      end
      object N2: TMenuItem
        Action = actPaste
      end
      object Copy1: TMenuItem
        Action = actCopy
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuGo: TMenuItem
        Action = actGo
      end
      object mnuClear: TMenuItem
        Action = actClear
      end
      object mnuExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnuExitClick
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object mnuAlwaysShowParseTree: TMenuItem
        AutoCheck = True
        Caption = '&Always show parse tree'
        GroupIndex = 1
        RadioItem = True
        OnClick = mnuAlwaysShowParseTreeClick
      end
      object mnuShowParseTreeonError: TMenuItem
        AutoCheck = True
        Caption = 'Show parse tree on &error'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = mnuShowParseTreeonErrorClick
      end
      object mnuNeverShowParseTree: TMenuItem
        AutoCheck = True
        Caption = '&Never show parse tree'
        GroupIndex = 1
        RadioItem = True
        OnClick = mnuNeverShowParseTreeClick
      end
    end
  end
end
