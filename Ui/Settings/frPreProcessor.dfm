inherited fPreProcessor: TfPreProcessor
  Height = 282
  OnResize = FrameResize
  object lblSymbols: TLabel
    Left = 4
    Top = 30
    Width = 202
    Height = 13
    Caption = 'Symbols defined for conditional compilation'
  end
  object lblCompilerOptions: TLabel
    Left = 8
    Top = 138
    Width = 240
    Height = 13
    Caption = 'Compiler options defined for conditional compilation'
  end
  object mSymbols: TJvMemo
    Left = 0
    Top = 50
    Width = 320
    Height = 79
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    ReadOnly = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object cbEnable: TCheckBox
    Left = 6
    Top = 6
    Width = 61
    Height = 17
    Caption = 'Enable'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object mOptions: TJvMemo
    Left = 0
    Top = 158
    Width = 320
    Height = 79
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    ReadOnly = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
