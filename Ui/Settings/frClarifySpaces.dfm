inherited fClarifySpaces: TfClarifySpaces
  Width = 456
  Height = 394
  ExplicitWidth = 456
  ExplicitHeight = 394
  object cbFixSpacing: TCheckBox
    Left = 8
    Top = 6
    Width = 89
    Height = 17
    Caption = 'Fix &spacing'
    TabOrder = 0
  end
  object cbSpaceClassHeritage: TCheckBox
    Left = 8
    Top = 24
    Width = 161
    Height = 17
    Caption = 'Space before class &heritage'
    TabOrder = 1
  end
  object gbColon: TGroupBox
    Left = 2
    Top = 42
    Width = 213
    Height = 189
    Caption = 'Spaces &before colon in'
    TabOrder = 2
    object Label2: TLabel
      Left = 6
      Top = 24
      Width = 127
      Height = 13
      Caption = '&Var and const declarations'
    end
    object Label4: TLabel
      Left = 6
      Top = 108
      Width = 71
      Height = 13
      Caption = '&Class variables'
    end
    object Label7: TLabel
      Left = 6
      Top = 80
      Width = 104
      Height = 13
      Caption = '&Function return types'
    end
    object Label8: TLabel
      Left = 6
      Top = 52
      Width = 107
      Height = 13
      Caption = '&Procedure parameters'
    end
    object Label5: TLabel
      Left = 6
      Top = 136
      Width = 49
      Height = 13
      Caption = 'Case l&abel'
    end
    object Label6: TLabel
      Left = 6
      Top = 164
      Width = 25
      Height = 13
      Caption = '&Label'
    end
    object eSpaceBeforeColonVar: TJvValidateEdit
      Left = 142
      Top = 24
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 0
    end
    object eSpaceBeforeColonParam: TJvValidateEdit
      Left = 142
      Top = 52
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 1
    end
    object eSpaceBeforeColonFn: TJvValidateEdit
      Left = 142
      Top = 76
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 2
    end
    object eSpacesBeforeColonClassVar: TJvValidateEdit
      Left = 142
      Top = 104
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 3
    end
    object eSpacesBeforeCaseLabel: TJvValidateEdit
      Left = 142
      Top = 132
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 4
    end
    object eSpacesBeforeLabel: TJvValidateEdit
      Left = 142
      Top = 160
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 5
    end
  end
  object gbTabs: TGroupBox
    Left = 2
    Top = 232
    Width = 277
    Height = 72
    Caption = '&Tab characters'
    TabOrder = 3
    object Label1: TLabel
      Left = 136
      Top = 20
      Width = 72
      Height = 13
      Caption = 'Spaces per tab'
    end
    object Label3: TLabel
      Left = 136
      Top = 44
      Width = 70
      Height = 13
      Caption = 'Spaces for tab'
    end
    object cbTabsToSpaces: TCheckBox
      Left = 6
      Top = 20
      Width = 117
      Height = 17
      Caption = 'Turn tabs to spaces'
      TabOrder = 0
      OnClick = cbTabsToSpacesClick
    end
    object cbSpacesToTabs: TCheckBox
      Left = 6
      Top = 44
      Width = 117
      Height = 17
      Caption = 'Turn spaces to tabs'
      TabOrder = 2
      OnClick = cbSpacesToTabsClick
    end
    object edtSpacesPerTab: TJvValidateEdit
      Left = 212
      Top = 18
      Width = 49
      Height = 21
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
      Left = 212
      Top = 42
      Width = 49
      Height = 21
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
    Left = 4
    Top = 308
    Width = 129
    Height = 17
    Caption = '&Max spaces in code'
    TabOrder = 4
    OnClick = cbMaxSpacesClick
  end
  object edtMaxSpacesInCode: TJvValidateEdit
    Left = 216
    Top = 310
    Width = 49
    Height = 21
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
    Left = 221
    Top = 42
    Width = 141
    Height = 77
    Caption = 'Spaces around &operators'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 6
  end
  object GroupBoxInsertSpace: TGroupBox
    Left = 221
    Top = 125
    Width = 148
    Height = 70
    Caption = '&Insert space before bracket'
    TabOrder = 7
    object cbInsertSpaceBeforeBracketinFunctionDeclaration: TCheckBox
      Left = 4
      Top = 20
      Width = 129
      Height = 17
      Caption = 'In function &declaration'
      TabOrder = 0
    end
    object cbInsertSpaceBeforeBracketinFunctionCall: TCheckBox
      Left = 4
      Top = 40
      Width = 129
      Height = 17
      Caption = 'In function &call'
      TabOrder = 1
    end
  end
end
