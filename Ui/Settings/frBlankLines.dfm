inherited fBlankLines: TfBlankLines
  Height = 382
  object Label1: TLabel
    Left = 4
    Top = 121
    Width = 199
    Height = 13
    Caption = 'Number of returns after the unit'#39's final end.'
  end
  object Label2: TLabel
    Left = 4
    Top = 168
    Width = 116
    Height = 13
    Caption = 'Max consecutive returns'
  end
  object eNumReturnsAfterFinalEnd: TJvIntegerEdit
    Left = 208
    Top = 117
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
  object cbRemoveConsecutiveReturns: TCheckBox
    Left = 4
    Top = 144
    Width = 201
    Height = 17
    Caption = 'Remove consecutive returns'
    TabOrder = 1
  end
  object edtMaxConsecutiveReturns: TJvIntegerEdit
    Left = 124
    Top = 165
    Width = 49
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 2
    Value = 0
    MaxValue = 255
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 221
    Height = 105
    Caption = 'Remove blank lines'
    TabOrder = 3
    object cbRemoveBlockBlankLines: TCheckBox
      Left = 8
      Top = 68
      Width = 197
      Height = 17
      Caption = 'at start and end of begin..end block'
      TabOrder = 0
    end
    object cbRemoveBlankLinesAfterProcHeader: TCheckBox
      Left = 8
      Top = 45
      Width = 193
      Height = 17
      Caption = 'after procedure header'
      TabOrder = 1
    end
    object cbRemoveVarBlankLines: TCheckBox
      Left = 8
      Top = 21
      Width = 181
      Height = 17
      Caption = 'in procedure var section'
      TabOrder = 2
    end
  end
end
