inherited frAnyCapsSettings: TfrAnyCapsSettings
  Width = 366
  Height = 230
  OnResize = FrameResize
  object Label1: TLabel
    Left = 76
    Top = 6
    Width = 158
    Height = 13
    Caption = 'Set capitalisation on these words '
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
    OnClick = cbEnableAnyWordsClick
  end
  object mWords: TJvMemo
    Left = 0
    Top = 40
    Width = 366
    Height = 190
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Align = alBottom
    ReadOnly = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
