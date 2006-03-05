inherited fBlankLines: TfBlankLines
  Width = 291
  Height = 315
  ExplicitWidth = 291
  ExplicitHeight = 315
  object Label1: TLabel
    Left = 3
    Top = 161
    Width = 210
    Height = 13
    Caption = 'Number of returns after the unit'#39's final end.'
  end
  object Label2: TLabel
    Left = 3
    Top = 208
    Width = 132
    Height = 13
    Caption = 'Max consecutive blank lines'
  end
  object Label3: TLabel
    Left = 3
    Top = 234
    Width = 111
    Height = 13
    Caption = 'Lines before procedure'
  end
  object eNumReturnsAfterFinalEnd: TJvValidateEdit
    Left = 215
    Top = 156
    Width = 49
    Height = 21
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 255.000000000000000000
    TabOrder = 1
  end
  object cbRemoveConsecutiveBlankLines: TCheckBox
    Left = 3
    Top = 184
    Width = 201
    Height = 17
    Caption = 'Remove consecutive blank lines'
    TabOrder = 2
  end
  object edtMaxConsecutiveBlankLines: TJvValidateEdit
    Left = 143
    Top = 205
    Width = 49
    Height = 21
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 99.000000000000000000
    TabOrder = 3
  end
  object gbRemoveBlankLines: TGroupBox
    Left = 3
    Top = 3
    Width = 221
    Height = 126
    Caption = 'Remove blank lines'
    TabOrder = 0
    object Label4: TLabel
      Left = 8
      Top = 94
      Width = 132
      Height = 13
      Caption = 'Max consecutive blank lines'
    end
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
    object edtMaxBlankLinesInSection: TJvValidateEdit
      Left = 146
      Top = 91
      Width = 49
      Height = 21
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 3
    end
  end
  object edtLinesBeforeProcedure: TJvValidateEdit
    Left = 143
    Top = 232
    Width = 49
    Height = 21
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 1
    MaxValue = 9.000000000000000000
    TabOrder = 4
  end
end
