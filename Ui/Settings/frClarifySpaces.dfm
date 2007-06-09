inherited fClarifySpaces: TfClarifySpaces
  Width = 472
  Height = 359
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 472
  ExplicitHeight = 359
  object cbFixSpacing: TCheckBox
    Left = 8
    Top = 6
    Width = 241
    Height = 17
    Caption = 'Fix &spacing'
    TabOrder = 0
  end
  object cbSpaceClassHeritage: TCheckBox
    Left = 8
    Top = 29
    Width = 241
    Height = 17
    Caption = 'Space before class &heritage'
    TabOrder = 1
  end
  object gbColon: TGroupBox
    Left = 8
    Top = 50
    Width = 246
    Height = 189
    Caption = 'Spaces &before colon in'
    TabOrder = 2
    object Label2: TLabel
      Left = 6
      Top = 24
      Width = 175
      Height = 20
      Caption = '&Var and const declarations'
    end
    object Label4: TLabel
      Left = 6
      Top = 108
      Width = 96
      Height = 20
      Caption = '&Class variables'
    end
    object Label7: TLabel
      Left = 6
      Top = 80
      Width = 138
      Height = 20
      Caption = '&Function return types'
    end
    object Label8: TLabel
      Left = 6
      Top = 52
      Width = 146
      Height = 20
      Caption = '&Procedure parameters'
    end
    object Label5: TLabel
      Left = 6
      Top = 136
      Width = 68
      Height = 20
      Caption = 'Case l&abel'
    end
    object Label6: TLabel
      Left = 6
      Top = 164
      Width = 36
      Height = 20
      Caption = '&Label'
    end
    object eSpaceBeforeColonVar: TJvValidateEdit
      Left = 187
      Top = 21
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 0
    end
    object eSpaceBeforeColonParam: TJvValidateEdit
      Left = 187
      Top = 49
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 1
    end
    object eSpaceBeforeColonFn: TJvValidateEdit
      Left = 187
      Top = 73
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 2
    end
    object eSpacesBeforeColonClassVar: TJvValidateEdit
      Left = 187
      Top = 101
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 3
    end
    object eSpacesBeforeCaseLabel: TJvValidateEdit
      Left = 187
      Top = 129
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 4
    end
    object eSpacesBeforeLabel: TJvValidateEdit
      Left = 187
      Top = 157
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 5
    end
  end
  object gbTabs: TGroupBox
    Left = 8
    Top = 244
    Width = 457
    Height = 70
    Caption = '&Tab characters'
    TabOrder = 3
    object Label1: TLabel
      Left = 240
      Top = 20
      Width = 98
      Height = 20
      Caption = 'Spaces per tab'
    end
    object Label3: TLabel
      Left = 240
      Top = 42
      Width = 95
      Height = 20
      Caption = 'Spaces for tab'
    end
    object cbTabsToSpaces: TCheckBox
      Left = 6
      Top = 22
      Width = 175
      Height = 17
      Caption = 'Turn tabs to spaces'
      TabOrder = 0
      OnClick = cbTabsToSpacesClick
    end
    object cbSpacesToTabs: TCheckBox
      Left = 6
      Top = 44
      Width = 175
      Height = 17
      Caption = 'Turn spaces to tabs'
      TabOrder = 2
      OnClick = cbSpacesToTabsClick
    end
    object edtSpacesPerTab: TJvValidateEdit
      Left = 356
      Top = 18
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 2
      MaxValue = 12.000000000000000000
      TabOrder = 1
    end
    object edtSpacesForTab: TJvValidateEdit
      Left = 356
      Top = 42
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 2
      MaxValue = 12.000000000000000000
      TabOrder = 3
    end
  end
  object cbMaxSpaces: TCheckBox
    Left = 8
    Top = 324
    Width = 179
    Height = 17
    Caption = '&Max spaces in code'
    TabOrder = 4
    OnClick = cbMaxSpacesClick
  end
  object edtMaxSpacesInCode: TJvValidateEdit
    Left = 189
    Top = 320
    Width = 49
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 2
    MaxValue = 99.000000000000000000
    TabOrder = 5
  end
  object rgOperators: TRadioGroup
    Left = 260
    Top = 50
    Width = 202
    Height = 92
    Caption = 'Spaces around &operators'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 6
  end
  object GroupBoxInsertSpace: TGroupBox
    Left = 260
    Top = 146
    Width = 202
    Height = 92
    Caption = '&Insert space before bracket'
    TabOrder = 7
    object cbInsertSpaceBeforeBracketinFunctionDeclaration: TCheckBox
      Left = 3
      Top = 26
      Width = 181
      Height = 17
      Caption = 'In function &declaration'
      TabOrder = 0
    end
    object cbInsertSpaceBeforeBracketinFunctionCall: TCheckBox
      Left = 3
      Top = 47
      Width = 181
      Height = 17
      Caption = 'In function &call'
      TabOrder = 1
    end
    object cbBeforeOpenSquareBracketInExpression: TCheckBox
      Left = 3
      Top = 68
      Width = 182
      Height = 17
      Caption = 'Before [ in expression'
      TabOrder = 2
    end
  end
end
