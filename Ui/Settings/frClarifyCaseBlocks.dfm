inherited fClarifyCaseBlocks: TfClarifyCaseBlocks
  Width = 301
  Height = 286
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 142
    Height = 13
    Caption = 'Block styles: use a new line at'
  end
  object rgLabelBegin: TRadioGroup
    Left = 4
    Top = 24
    Width = 130
    Height = 73
    Caption = 'Label with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgLabel: TRadioGroup
    Left = 146
    Top = 24
    Width = 130
    Height = 73
    Caption = 'Label without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgCaseLabel: TRadioGroup
    Left = 146
    Top = 104
    Width = 130
    Height = 73
    Caption = 'Case without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgElseCase: TRadioGroup
    Left = 146
    Top = 184
    Width = 130
    Height = 73
    Caption = 'Else case without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 5
  end
  object rgCaseBegin: TRadioGroup
    Left = 4
    Top = 104
    Width = 130
    Height = 73
    Caption = 'Case with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgCaseElseBegin: TRadioGroup
    Left = 4
    Top = 184
    Width = 130
    Height = 73
    Caption = 'Else case with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
end
