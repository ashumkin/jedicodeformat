inherited fAsm: TfAsm
  Width = 375
  Height = 262
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 375
  ExplicitHeight = 262
  object rgCaps: TRadioGroup
    Left = 165
    Top = 3
    Width = 196
    Height = 109
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
      Left = 8
      Top = 50
      Width = 54
      Height = 20
      Caption = 'Indent 1'
    end
    object Label2: TLabel
      Left = 8
      Top = 84
      Width = 54
      Height = 20
      Caption = 'Indent 2'
    end
    object Label3: TLabel
      Left = 8
      Top = 118
      Width = 54
      Height = 20
      Caption = 'Indent 3'
    end
    object Label4: TLabel
      Left = 8
      Top = 152
      Width = 54
      Height = 20
      Caption = 'Indent 4'
    end
    object Label5: TLabel
      Left = 8
      Top = 186
      Width = 54
      Height = 20
      Caption = 'Indent 5'
    end
    object Label6: TLabel
      Left = 8
      Top = 221
      Width = 54
      Height = 20
      Caption = 'Indent 6'
    end
    object edtIndent6: TJvValidateEdit
      Left = 55
      Top = 215
      Width = 49
      Height = 28
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
      Left = 55
      Top = 181
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
    object edtIndent4: TJvValidateEdit
      Left = 55
      Top = 149
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
    object edtIndent3: TJvValidateEdit
      Left = 55
      Top = 115
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
    object edtIndent2: TJvValidateEdit
      Left = 55
      Top = 81
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
    object edtIndent1: TJvValidateEdit
      Left = 55
      Top = 47
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
    object cbIndentsEnabled: TCheckBox
      Left = 8
      Top = 25
      Width = 97
      Height = 17
      Caption = '&Enabled'
      TabOrder = 0
    end
  end
  object gbBreaksAfterLabel: TGroupBox
    Left = 165
    Top = 118
    Width = 196
    Height = 135
    Caption = '&Breaks after label'
    TabOrder = 2
    object Label7: TLabel
      Left = 8
      Top = 48
      Width = 119
      Height = 20
      Caption = '&Number of breaks'
      FocusControl = edtBreaksAfterLabel
    end
    object cbBreaksAfterLabelEnabled: TCheckBox
      Left = 8
      Top = 25
      Width = 97
      Height = 17
      Caption = 'En&abled'
      TabOrder = 0
    end
    object edtBreaksAfterLabel: TJvValidateEdit
      Left = 131
      Top = 45
      Width = 49
      Height = 28
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
