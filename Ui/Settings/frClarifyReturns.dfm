inherited fClarifyReturns: TfClarifyReturns
  Width = 437
  Height = 369
  object Label1: TLabel
    Left = 8
    Top = 196
    Width = 199
    Height = 13
    Caption = 'Number of returns after the unit'#39's final end.'
  end
  object cbRemoveReturns: TCheckBox
    Left = 8
    Top = 30
    Width = 217
    Height = 17
    Caption = 'Remove returns in misc. bad places'
    TabOrder = 0
  end
  object cbRemoveVarBlankLines: TCheckBox
    Left = 8
    Top = 163
    Width = 237
    Height = 17
    Caption = 'Remove blank lines in procedure var section'
    TabOrder = 1
  end
  object cbRemovePropertyReturns: TCheckBox
    Left = 8
    Top = 52
    Width = 181
    Height = 17
    Caption = 'Remove returns in properties'
    TabOrder = 2
  end
  object cbRemoveExprReturns: TCheckBox
    Left = 8
    Top = 74
    Width = 181
    Height = 17
    Caption = 'Remove returns in expressions'
    TabOrder = 3
  end
  object cbInsertReturns: TCheckBox
    Left = 8
    Top = 8
    Width = 217
    Height = 17
    Caption = 'Insert returns in misc. good places'
    TabOrder = 4
  end
  object cbRemoveProcDefReturns: TCheckBox
    Left = 8
    Top = 96
    Width = 237
    Height = 17
    Caption = 'Remove returns in procedure definitions'
    TabOrder = 5
  end
  object cbRemoveVarReturns: TCheckBox
    Left = 8
    Top = 118
    Width = 237
    Height = 17
    Caption = 'Remove returns in variable declarations'
    TabOrder = 6
  end
  object eNumReturnsAfterFinalEnd: TJvIntegerEdit
    Left = 212
    Top = 194
    Width = 49
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 7
    Value = 0
    MaxValue = 255
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
  object cbRemoveBlankLinesAfterProcHeader: TCheckBox
    Left = 8
    Top = 145
    Width = 237
    Height = 17
    Caption = 'Remove blank lines after procedure header'
    TabOrder = 8
  end
  object cbRemoveBlockBlankLines: TCheckBox
    Left = 8
    Top = 140
    Width = 253
    Height = 17
    Caption = 'Remove blank lines at start and end of block'
    TabOrder = 9
  end
  object rgReturnChars: TRadioGroup
    Left = 8
    Top = 224
    Width = 301
    Height = 73
    Caption = 'Return chars'
    Items.Strings = (
      'Leave as is'
      'Convert to Carriage return (UNIX)'
      'Convert to Carriage-return + Linefeed (DOS/Windows)')
    TabOrder = 10
  end
end
