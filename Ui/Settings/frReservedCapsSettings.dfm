inherited frReservedCapsSettings: TfrReservedCapsSettings
  Width = 334
  Height = 297
  object cbEnable: TCheckBox
    Left = 6
    Top = 6
    Width = 145
    Height = 17
    Caption = 'Enable fix capitalisation'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = cbEnableClick
  end
  object rgReservedWords: TRadioGroup
    Left = 6
    Top = 28
    Width = 120
    Height = 81
    Caption = 'Reserved words'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 1
  end
  object rgOperators: TRadioGroup
    Left = 134
    Top = 28
    Width = 120
    Height = 81
    Caption = 'Operators'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 2
  end
  object rgTypes: TRadioGroup
    Left = 6
    Top = 196
    Width = 120
    Height = 81
    Caption = 'Types'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 5
  end
  object rgConstants: TRadioGroup
    Left = 134
    Top = 112
    Width = 120
    Height = 81
    Caption = 'Constants'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 4
  end
  object rgDirectives: TRadioGroup
    Left = 6
    Top = 112
    Width = 120
    Height = 81
    Caption = 'Directives'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 3
  end
end
