inherited fAsm: TfAsm
  Width = 327
  Height = 262
  ExplicitWidth = 327
  ExplicitHeight = 262
  object rgCaps: TRadioGroup
    Left = 165
    Top = 3
    Width = 153
    Height = 106
    Caption = '&Capitalization'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 1
  end
  object gbIndents: TGroupBox
    Left = 9
    Top = 3
    Width = 150
    Height = 250
    Caption = '&Indents'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 43
      Width = 41
      Height = 13
      Caption = 'Indent 1'
    end
    object Label2: TLabel
      Left = 16
      Top = 77
      Width = 41
      Height = 13
      Caption = 'Indent 2'
    end
    object Label3: TLabel
      Left = 16
      Top = 111
      Width = 41
      Height = 13
      Caption = 'Indent 3'
    end
    object Label4: TLabel
      Left = 16
      Top = 145
      Width = 41
      Height = 13
      Caption = 'Indent 4'
    end
    object Label5: TLabel
      Left = 16
      Top = 179
      Width = 41
      Height = 13
      Caption = 'Indent 5'
    end
    object Label6: TLabel
      Left = 16
      Top = 214
      Width = 41
      Height = 13
      Caption = 'Indent 6'
    end
    object edtIndent6: TJvValidateEdit
      Left = 63
      Top = 208
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 6
    end
    object edtIndent5: TJvValidateEdit
      Left = 63
      Top = 174
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 5
    end
    object edtIndent4: TJvValidateEdit
      Left = 63
      Top = 142
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 4
    end
    object edtIndent3: TJvValidateEdit
      Left = 63
      Top = 108
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 3
    end
    object edtIndent2: TJvValidateEdit
      Left = 63
      Top = 74
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 2
    end
    object edtIndent1: TJvValidateEdit
      Left = 63
      Top = 40
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 1
    end
    object cbIndentsEnabled: TCheckBox
      Left = 16
      Top = 17
      Width = 97
      Height = 17
      Caption = '&Enabled'
      TabOrder = 0
    end
  end
  object gbBreaksAfterLabel: TGroupBox
    Left = 165
    Top = 115
    Width = 153
    Height = 80
    Caption = '&Breaks after label'
    TabOrder = 2
    object Label7: TLabel
      Left = 14
      Top = 41
      Width = 85
      Height = 13
      Caption = '&Number of breaks'
      FocusControl = edtBreaksAfterLabel
    end
    object cbBreaksAfterLabelEnabled: TCheckBox
      Left = 6
      Top = 18
      Width = 97
      Height = 17
      Caption = 'En&abled'
      TabOrder = 0
    end
    object edtBreaksAfterLabel: TJvValidateEdit
      Left = 100
      Top = 38
      Width = 40
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 1
      MaxValue = 9.000000000000000000
      TabOrder = 1
    end
  end
end
