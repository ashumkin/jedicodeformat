inherited fClarifyBlocks: TfClarifyBlocks
  Width = 353
  Height = 344
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
    Width = 120
    Height = 73
    Caption = 'Block with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgLabelBegin: TRadioGroup
    Left = 134
    Top = 24
    Width = 110
    Height = 73
    Caption = 'Label with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgLabel: TRadioGroup
    Left = 134
    Top = 104
    Width = 110
    Height = 73
    Caption = 'Label without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgBlock: TRadioGroup
    Left = 4
    Top = 104
    Width = 120
    Height = 73
    Caption = 'Block without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgEndElse: TRadioGroup
    Left = 4
    Top = 180
    Width = 120
    Height = 73
    Caption = 'between end and else'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
  object rgCaseLabel: TRadioGroup
    Left = 4
    Top = 260
    Width = 120
    Height = 73
    Caption = 'Case without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 6
  end
  object rgElseIf: TRadioGroup
    Left = 132
    Top = 180
    Width = 110
    Height = 73
    Caption = 'between else and if'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 5
  end
  object rgElseCase: TRadioGroup
    Left = 132
    Top = 260
    Width = 90
    Height = 73
    Caption = 'Else case'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 7
  end
  object rgElseBegin: TRadioGroup
    Left = 252
    Top = 24
    Width = 90
    Height = 73
    Caption = 'Else begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 8
  end
end
