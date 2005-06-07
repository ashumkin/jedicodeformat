inherited fNotIdentifierCapsSettings: TfNotIdentifierCapsSettings
  Width = 366
  Height = 230
  OnResize = FrameResize
  object Label1: TLabel
    Left = 76
    Top = 6
    Width = 192
    Height = 13
    Caption = 'Set capitalisation on these non-identifiers'
  end
  object cbEnableAnyWords: TCheckBox
    Left = 6
    Top = 5
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
    Top = 32
    Width = 366
    Height = 198
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
