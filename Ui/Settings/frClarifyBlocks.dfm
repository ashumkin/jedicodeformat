inherited fClarifyBlocks: TfClarifyBlocks
  Width = 356
  Height = 277
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 142
    Height = 13
    Caption = 'Block styles: use a new line at'
  end
  object rgBlockBegin: TRadioGroup
    Left = 4
    Top = 24
    Width = 125
    Height = 73
    Caption = 'Block with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgLabelBegin: TRadioGroup
    Left = 150
    Top = 24
    Width = 130
    Height = 73
    Caption = 'Label with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgLabel: TRadioGroup
    Left = 150
    Top = 104
    Width = 130
    Height = 73
    Caption = 'Label without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgBlock: TRadioGroup
    Left = 4
    Top = 104
    Width = 130
    Height = 73
    Caption = 'Block without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgEndElse: TRadioGroup
    Left = 4
    Top = 180
    Width = 130
    Height = 73
    Caption = 'between end and else'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
  object rgCaseLabel: TRadioGroup
    Left = 150
    Top = 180
    Width = 130
    Height = 73
    Caption = 'Case label without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 5
  end
end
