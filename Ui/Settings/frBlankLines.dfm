inherited fBlankLines: TfBlankLines
  Height = 242
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
    Width = 134
    Height = 13
    Caption = 'Max consecutive blank lines'
  end
  object Label3: TLabel
    Left = 4
    Top = 194
    Width = 109
    Height = 13
    Caption = 'Lines before procedure'
  end
  object eNumReturnsAfterFinalEnd: TJvValidateEdit
    Left = 208
    Top = 117
    Width = 49
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 255.000000000000000000
    TabOrder = 1
    Text = '0'
    Value = 0
  end
  object cbRemoveConsecutiveBlankLines: TCheckBox
    Left = 4
    Top = 144
    Width = 201
    Height = 17
    Caption = 'Remove consecutive blank lines'
    TabOrder = 2
  end
  object edtMaxConsecutiveBlankLines: TJvValidateEdit
    Left = 144
    Top = 165
    Width = 49
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 99.000000000000000000
    TabOrder = 3
    Text = '0'
    Value = 0
  end
  object gbRemoveBlankLines: TGroupBox
    Left = 4
    Top = 4
    Width = 221
    Height = 101
    Caption = 'Remove blank lines'
    TabOrder = 0
    object cbRemoveBlockBlankLines: TCheckBox
      Left = 8
      Top = 68
      Width = 197
      Height = 17
      Caption = 'at start and end of begin..end block'
      TabOrder = 2
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
      TabOrder = 0
    end
  end
  object edtLinesBeforeProcedure: TJvValidateEdit
    Left = 144
    Top = 192
    Width = 49
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 1
    MaxValue = 9.000000000000000000
    TabOrder = 4
    Text = '0'
    Value = 0
  end
end
