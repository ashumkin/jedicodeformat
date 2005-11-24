inherited fClarifyBlocks: TfClarifyBlocks
  Width = 329
  Height = 270
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
    Width = 130
    Height = 73
    Caption = 'Block with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgBlock: TRadioGroup
    Left = 146
    Top = 24
    Width = 130
    Height = 73
    Caption = 'Block without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgEndElse: TRadioGroup
    Left = 146
    Top = 104
    Width = 130
    Height = 73
    Caption = 'Between end and else'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgElseIf: TRadioGroup
    Left = 4
    Top = 104
    Width = 130
    Height = 73
    Caption = 'Between else and if'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgElseBegin: TRadioGroup
    Left = 4
    Top = 184
    Width = 130
    Height = 73
    Caption = 'Else begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
end
