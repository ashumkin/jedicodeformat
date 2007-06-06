inherited frReservedCapsSettings: TfrReservedCapsSettings
  Width = 334
  Height = 375
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 334
  ExplicitHeight = 375
  object cbEnable: TCheckBox
    Left = 8
    Top = 6
    Width = 248
    Height = 17
    Caption = 'Enable fix capitalisation'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = cbEnableClick
  end
  object rgReservedWords: TRadioGroup
    Left = 8
    Top = 28
    Width = 150
    Height = 108
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
    Left = 164
    Top = 29
    Width = 150
    Height = 108
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
    Left = 8
    Top = 257
    Width = 150
    Height = 108
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
    Left = 164
    Top = 143
    Width = 150
    Height = 108
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
    Left = 8
    Top = 143
    Width = 150
    Height = 108
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
