inherited fClarifyIndent: TfClarifyIndent
  Width = 310
  Height = 297
  object Label2: TLabel
    Left = 4
    Top = 80
    Width = 119
    Height = 13
    Caption = 'Block indentation spaces'
  end
  object edtIndentSpaces: TJvValidateEdit
    Left = 236
    Top = 78
    Width = 50
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 12.000000000000000000
    PasswordChar = #0
    TabOrder = 3
    Text = '0'
    Value = 0
  end
  object cbIndentBeginEnd: TCheckBox
    Left = 4
    Top = 108
    Width = 237
    Height = 17
    Caption = 'Extra indent for begin/end inside procedures'
    TabOrder = 4
    OnClick = cbIndentBeginEndClick
  end
  object eIndentBeginEndSpaces: TJvValidateEdit
    Left = 236
    Top = 106
    Width = 50
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 10.000000000000000000
    MinValue = -10.000000000000000000
    PasswordChar = #0
    TabOrder = 5
    Text = '0'
    Value = 0
  end
  object cbHasFirstLevelIndent: TCheckBox
    Left = 4
    Top = 136
    Width = 153
    Height = 17
    Caption = 'Different indent for first level'
    TabOrder = 6
    OnClick = cbHasFirstLevelIndentClick
  end
  object eFirstLevelIndent: TJvValidateEdit
    Left = 236
    Top = 134
    Width = 50
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    MaxLength = 3
    PasswordChar = #0
    TabOrder = 7
    Text = '0'
    Value = 0
  end
  object cbIndentGlobals: TCheckBox
    Left = 4
    Top = 8
    Width = 110
    Height = 17
    Caption = 'Indent globals'
    TabOrder = 0
  end
  object cbIndentProcedures: TCheckBox
    Left = 4
    Top = 30
    Width = 110
    Height = 17
    Caption = 'Indent procedures'
    TabOrder = 1
  end
  object cbIndentClasses: TCheckBox
    Left = 4
    Top = 52
    Width = 110
    Height = 17
    Caption = 'Indent classes'
    TabOrder = 2
  end
  object cbKeepWithInProc: TCheckBox
    Left = 4
    Top = 164
    Width = 265
    Height = 17
    Caption = 'Keep single-line comments with code in procedures'
    TabOrder = 8
  end
  object cbKeepWithInGlobals: TCheckBox
    Left = 4
    Top = 184
    Width = 305
    Height = 17
    Caption = 'Keep single-line comments with code in globals'
    TabOrder = 9
  end
  object cbKeepWithInClassDef: TCheckBox
    Left = 4
    Top = 204
    Width = 285
    Height = 17
    Caption = 'Keep single-line comments with code in class definitions'
    TabOrder = 10
  end
end
