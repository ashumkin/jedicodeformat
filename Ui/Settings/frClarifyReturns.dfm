inherited fClarifyReturns: TfClarifyReturns
  Width = 362
  Height = 284
  object rgReturnChars: TRadioGroup
    Left = 8
    Top = 160
    Width = 301
    Height = 73
    Caption = 'Return chars'
    Items.Strings = (
      'Leave as is'
      'Convert to Carriage return (UNIX)'
      'Convert to Carriage-return + Linefeed (DOS/Windows)')
    TabOrder = 2
  end
  object gbRemoveReturns: TGroupBox
    Left = 8
    Top = 8
    Width = 157
    Height = 145
    Hint = 'bgRemove'
    Caption = 'Remove returns'
    TabOrder = 0
    object cbRemoveProcDefReturns: TCheckBox
      Left = 8
      Top = 67
      Width = 140
      Height = 17
      Caption = 'In procedure definitions'
      TabOrder = 2
    end
    object cbRemoveVarReturns: TCheckBox
      Left = 8
      Top = 92
      Width = 140
      Height = 17
      Caption = 'In variable declarations'
      TabOrder = 3
    end
    object cbRemoveExprReturns: TCheckBox
      Left = 8
      Top = 117
      Width = 140
      Height = 17
      Caption = 'In expressions'
      TabOrder = 4
    end
    object cbRemovePropertyReturns: TCheckBox
      Left = 8
      Top = 42
      Width = 140
      Height = 17
      Caption = 'In properties'
      TabOrder = 1
    end
    object cbRemoveReturns: TCheckBox
      Left = 8
      Top = 18
      Width = 140
      Height = 17
      Caption = 'In misc. bad places'
      TabOrder = 0
    end
  end
  object gbInsert: TGroupBox
    Left = 176
    Top = 8
    Width = 173
    Height = 145
    Caption = 'Insert returns'
    TabOrder = 1
    object cbUsesClauseOnePerLine: TCheckBox
      Left = 8
      Top = 42
      Width = 160
      Height = 17
      Caption = 'One uses clause item per line'
      TabOrder = 1
    end
    object cbInsertReturns: TCheckBox
      Left = 8
      Top = 18
      Width = 130
      Height = 17
      Caption = 'In misc. good places'
      TabOrder = 0
    end
  end
end
