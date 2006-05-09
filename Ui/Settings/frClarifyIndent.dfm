inherited fClarifyIndent: TfClarifyIndent
  Width = 321
  Height = 306
  object Label2: TLabel
    Left = 4
    Top = 80
    Width = 119
    Height = 13
    Caption = 'Block indentation spaces'
  end
  object edtIndentSpaces: TJvValidateEdit
    Left = 155
    Top = 78
    Width = 50
    Height = 21
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 12.000000000000000000
    TabOrder = 3
  end
  object cbIndentGlobals: TCheckBox
    Left = 4
    Top = 6
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
    Top = 54
    Width = 110
    Height = 17
    Caption = 'Indent classes'
    TabOrder = 2
  end
  object gbOptions: TGroupBox
    Left = 4
    Top = 105
    Width = 309
    Height = 192
    Caption = 'Options'
    TabOrder = 4
    object cbIndentBeginEnd: TCheckBox
      Left = 12
      Top = 20
      Width = 237
      Height = 17
      Caption = 'Extra indent for begin/end inside procedures'
      TabOrder = 0
      OnClick = cbIndentBeginEndClick
    end
    object eIndentBeginEndSpaces: TJvValidateEdit
      Left = 246
      Top = 18
      Width = 50
      Height = 21
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 10.000000000000000000
      MinValue = -10.000000000000000000
      TabOrder = 1
    end
    object cbHasFirstLevelIndent: TCheckBox
      Left = 12
      Top = 48
      Width = 189
      Height = 17
      Caption = 'Different indent for first level'
      TabOrder = 2
      OnClick = cbHasFirstLevelIndentClick
    end
    object eFirstLevelIndent: TJvValidateEdit
      Left = 246
      Top = 45
      Width = 50
      Height = 21
      EditText = '0'
      MaxLength = 3
      TabOrder = 3
    end
    object cbKeepWithInProc: TCheckBox
      Left = 12
      Top = 76
      Width = 265
      Height = 17
      Caption = 'Keep single-line comments with code in procedures'
      TabOrder = 4
    end
    object cbKeepWithInGlobals: TCheckBox
      Left = 12
      Top = 96
      Width = 265
      Height = 17
      Caption = 'Keep single-line comments with code in globals'
      TabOrder = 5
    end
    object cbKeepWithInClassDef: TCheckBox
      Left = 12
      Top = 116
      Width = 285
      Height = 17
      Caption = 'Keep single-line comments with code in class definitions'
      TabOrder = 6
    end
    object cbKeepWithElsewhere: TCheckBox
      Left = 11
      Top = 139
      Width = 253
      Height = 17
      Caption = 'Keep single-line comments with code elsewhere'
      TabOrder = 7
    end
    object cbIndentElse: TCheckBox
      Left = 11
      Top = 162
      Width = 238
      Height = 17
      Caption = 'Extra Indent for Else blocks'
      TabOrder = 8
    end
  end
end
