inherited frUnitNameCaps: TfrUnitNameCaps
  OnResize = FrameResize
  object Label1: TLabel
    Left = 76
    Top = 6
    Width = 178
    Height = 13
    Caption = 'Set capitalisation on these unit names'
  end
  object mWords: TJvMemo
    Left = 0
    Top = 50
    Width = 320
    Height = 190
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object cbEnableAnyWords: TCheckBox
    Left = 6
    Top = 6
    Width = 61
    Height = 17
    Caption = 'Enable'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
end
