inherited fClarifyReturns: TfClarifyReturns
  Width = 362
  Height = 369
  object Label1: TLabel
    Left = 8
    Top = 216
    Width = 199
    Height = 13
    Caption = 'Number of returns after the unit'#39's final end.'
  end
  object eNumReturnsAfterFinalEnd: TJvIntegerEdit
    Left = 212
    Top = 214
    Width = 49
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 0
    Value = 0
    MaxValue = 255
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
  object rgReturnChars: TRadioGroup
    Left = 8
    Top = 240
    Width = 301
    Height = 73
    Caption = 'Return chars'
    Items.Strings = (
      'Leave as is'
      'Convert to Carriage return (UNIX)'
      'Convert to Carriage-return + Linefeed (DOS/Windows)')
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 157
    Height = 153
    Hint = 'bgRemove'
    Caption = 'Remove returns...'
    TabOrder = 2
    object cbRemoveProcDefReturns: TCheckBox
      Left = 8
      Top = 83
      Width = 140
      Height = 17
      Caption = 'In procedure definitions'
      TabOrder = 0
    end
    object cbRemoveVarReturns: TCheckBox
      Left = 8
      Top = 105
      Width = 140
      Height = 17
      Caption = 'In variable declarations'
      TabOrder = 1
    end
    object cbRemoveBlockBlankLines: TCheckBox
      Left = 8
      Top = 127
      Width = 140
      Height = 17
      Caption = 'At start and end of block'
      TabOrder = 2
    end
    object cbRemoveExprReturns: TCheckBox
      Left = 8
      Top = 61
      Width = 140
      Height = 17
      Caption = 'In expressions'
      TabOrder = 3
    end
    object cbRemovePropertyReturns: TCheckBox
      Left = 8
      Top = 39
      Width = 140
      Height = 17
      Caption = 'In properties'
      TabOrder = 4
    end
    object cbRemoveReturns: TCheckBox
      Left = 8
      Top = 18
      Width = 140
      Height = 17
      Caption = 'In misc. bad places'
      TabOrder = 5
    end
  end
  object cbRemoveBlankLinesAfterProcHeader: TCheckBox
    Left = 8
    Top = 169
    Width = 237
    Height = 17
    Caption = 'Remove blank lines after procedure header'
    TabOrder = 3
  end
  object cbRemoveVarBlankLines: TCheckBox
    Left = 8
    Top = 192
    Width = 237
    Height = 17
    Caption = 'Remove blank lines in procedure var section'
    TabOrder = 4
  end
  object gbInsert: TGroupBox
    Left = 176
    Top = 8
    Width = 173
    Height = 153
    Caption = 'Insert returns'
    TabOrder = 5
    object cbUsesClauseOnePerLine: TCheckBox
      Left = 4
      Top = 40
      Width = 165
      Height = 17
      Caption = 'Uses clause items one per line'
      TabOrder = 0
    end
    object cbInsertReturns: TCheckBox
      Left = 4
      Top = 18
      Width = 130
      Height = 17
      Caption = 'In misc. good places'
      TabOrder = 1
    end
  end
end
