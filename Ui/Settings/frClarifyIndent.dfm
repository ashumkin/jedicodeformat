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
  object edtIndentSpaces: TJvIntegerEdit
    Left = 236
    Top = 78
    Width = 50
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 0
    Value = 0
    MaxValue = 12
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
  object cbIndentBeginEnd: TCheckBox
    Left = 4
    Top = 108
    Width = 237
    Height = 17
    Caption = 'Extra indent for begin/end inside procedures'
    TabOrder = 1
    OnClick = cbIndentBeginEndClick
  end
  object eIndentBeginEndSpaces: TJvIntegerEdit
    Left = 236
    Top = 106
    Width = 50
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 2
    Value = 0
    MaxValue = 10
    MinValue = -10
    HasMaxValue = True
    HasMinValue = True
  end
  object cbHasFirstLevelIndent: TCheckBox
    Left = 4
    Top = 136
    Width = 153
    Height = 17
    Caption = 'Different indent for first level'
    TabOrder = 3
    OnClick = cbHasFirstLevelIndentClick
  end
  object eFirstLevelIndent: TJvIntegerEdit
    Left = 236
    Top = 134
    Width = 50
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 4
    Value = 0
    MaxValue = 0
    MinValue = 0
    HasMaxValue = False
    HasMinValue = False
  end
  object cbIndentGlobals: TCheckBox
    Left = 4
    Top = 8
    Width = 110
    Height = 17
    Caption = 'Indent globals'
    TabOrder = 5
  end
  object cbIndentProcedures: TCheckBox
    Left = 4
    Top = 30
    Width = 110
    Height = 17
    Caption = 'Indent procedures'
    TabOrder = 6
  end
  object cbIndentClasses: TCheckBox
    Left = 4
    Top = 52
    Width = 110
    Height = 17
    Caption = 'Indent classes'
    TabOrder = 7
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
  object cbBorlandCaseIndent: TCheckBox
    Left = 4
    Top = 228
    Width = 189
    Height = 17
    Caption = 'Borland style case indents'
    TabOrder = 11
  end
end
