inherited frAnyCapsSettings: TfrAnyCapsSettings
  Width = 366
  Height = 230
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitWidth = 366
  ExplicitHeight = 230
  object Label1: TLabel
    Left = 108
    Top = 3
    Width = 222
    Height = 20
    Caption = 'Set capitalisation on these words '
  end
  object cbEnableAnyWords: TCheckBox
    Left = 8
    Top = 5
    Width = 83
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
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
