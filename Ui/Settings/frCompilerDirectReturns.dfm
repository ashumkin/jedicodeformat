inherited fCompilerDirectReturns: TfCompilerDirectReturns
  Width = 399
  Height = 242
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 196
    Height = 13
    Caption = 'Use a new line before compiler directives:'
  end
  object Label2: TLabel
    Left = 4
    Top = 104
    Width = 187
    Height = 13
    Caption = 'Use a new line after compiler directives:'
  end
  object rgBeforeUses: TRadioGroup
    Left = 4
    Top = 24
    Width = 100
    Height = 73
    Caption = 'Uses clause'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgBeforeStatements: TRadioGroup
    Left = 114
    Top = 24
    Width = 100
    Height = 73
    Caption = 'Statements'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgBeforeGeneral: TRadioGroup
    Left = 222
    Top = 24
    Width = 100
    Height = 73
    Caption = 'Other places'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgAfterGeneral: TRadioGroup
    Left = 222
    Top = 124
    Width = 100
    Height = 73
    Caption = 'Other places'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgAfterStatements: TRadioGroup
    Left = 114
    Top = 124
    Width = 100
    Height = 73
    Caption = 'Statements'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
  object rgAfterUses: TRadioGroup
    Left = 4
    Top = 124
    Width = 100
    Height = 73
    Caption = 'Uses clause'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 5
  end
end
