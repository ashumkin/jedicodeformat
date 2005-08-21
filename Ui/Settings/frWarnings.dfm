inherited fWarnings: TfWarnings
  OnResize = FrameResize
  object Label1: TLabel
    Left = 12
    Top = 52
    Width = 158
    Height = 13
    Caption = '&Ignore unused parameters named'
    FocusControl = mIgnoreUnusedParams
  end
  object cbWarningsOn: TCheckBox
    Left = 8
    Top = 8
    Width = 180
    Height = 17
    Caption = '&Warnings On'
    TabOrder = 0
  end
  object cbWarnUnusedParams: TCheckBox
    Left = 8
    Top = 28
    Width = 180
    Height = 17
    Caption = 'Warn &unused parameters'
    TabOrder = 1
  end
  object mIgnoreUnusedParams: TMemo
    Left = 8
    Top = 72
    Width = 213
    Height = 153
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
