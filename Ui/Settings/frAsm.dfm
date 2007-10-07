inherited fAsm: TfAsm
  Width = 175
  Height = 438
  ExplicitWidth = 175
  ExplicitHeight = 438
  object Label7: TLabel
    Left = 3
    Top = 151
    Width = 84
    Height = 13
    Caption = 'Breaks after label'
  end
  object cbEnabled: TCheckBox
    Left = 3
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Enabled'
    TabOrder = 0
  end
  object rgCaps: TRadioGroup
    Left = 3
    Top = 39
    Width = 150
    Height = 106
    Caption = 'Capitalization'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 1
  end
  object gbIndents: TGroupBox
    Left = 3
    Top = 182
    Width = 150
    Height = 234
    Caption = 'Indents'
    TabOrder = 3
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 41
      Height = 13
      Caption = 'Indent 1'
    end
    object Label2: TLabel
      Left = 16
      Top = 58
      Width = 41
      Height = 13
      Caption = 'Indent 2'
    end
    object Label3: TLabel
      Left = 16
      Top = 92
      Width = 41
      Height = 13
      Caption = 'Indent 3'
    end
    object Label4: TLabel
      Left = 16
      Top = 126
      Width = 41
      Height = 13
      Caption = 'Indent 4'
    end
    object Label5: TLabel
      Left = 16
      Top = 160
      Width = 41
      Height = 13
      Caption = 'Indent 5'
    end
    object Label6: TLabel
      Left = 16
      Top = 195
      Width = 41
      Height = 13
      Caption = 'Indent 6'
    end
    object edtIndent6: TJvValidateEdit
      Left = 63
      Top = 189
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 5
    end
    object edtIndent5: TJvValidateEdit
      Left = 63
      Top = 155
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 4
    end
    object edtIndent4: TJvValidateEdit
      Left = 63
      Top = 123
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 3
    end
    object edtIndent3: TJvValidateEdit
      Left = 63
      Top = 89
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 2
    end
    object edtIndent2: TJvValidateEdit
      Left = 63
      Top = 55
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 1
    end
    object edtIndent1: TJvValidateEdit
      Left = 63
      Top = 21
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 0
    end
  end
  object edtBreaksAfterLabel: TJvValidateEdit
    Left = 93
    Top = 148
    Width = 49
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 99.000000000000000000
    TabOrder = 2
  end
end
