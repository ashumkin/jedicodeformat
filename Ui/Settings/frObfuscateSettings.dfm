inherited fObfuscateSettings: TfObfuscateSettings
  Width = 275
  Height = 205
  object cbRemoveWhiteSpace: TCheckBox
    Left = 6
    Top = 104
    Width = 133
    Height = 17
    Caption = 'Remove white space'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object cbRemoveComments: TCheckBox
    Left = 6
    Top = 124
    Width = 125
    Height = 17
    Caption = 'Remove comments'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object rgObfuscateCaps: TRadioGroup
    Left = 6
    Top = 6
    Width = 125
    Height = 93
    Caption = 'Obfuscate word caps'
    ItemIndex = 0
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 0
  end
  object cbRebreak: TCheckBox
    Left = 6
    Top = 164
    Width = 125
    Height = 17
    Caption = 'Rebreak lines'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object cbRemoveIndent: TCheckBox
    Left = 6
    Top = 144
    Width = 125
    Height = 17
    Caption = 'Remove indent'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
end
