inherited fClarifyAlign: TfClarifyAlign
  Width = 181
  Height = 277
  object Label6: TLabel
    Left = 8
    Top = 208
    Width = 65
    Height = 13
    Caption = 'Max Variance'
  end
  object Label4: TLabel
    Left = 8
    Top = 180
    Width = 58
    Height = 13
    Caption = 'Max Column'
  end
  object Label5: TLabel
    Left = 8
    Top = 152
    Width = 55
    Height = 13
    Caption = 'Min Column'
  end
  object Label1: TLabel
    Left = 8
    Top = 236
    Width = 48
    Height = 13
    Caption = 'Unaligned'
  end
  object cbInterfaceOnly: TCheckBox
    Left = 8
    Top = 6
    Width = 97
    Height = 17
    Caption = 'Interface Only'
    TabOrder = 0
  end
  object edtMaxVariance: TJvValidateEdit
    Left = 80
    Top = 204
    Width = 57
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '1'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 999.000000000000000000
    MinValue = 1.000000000000000000
    PasswordChar = #0
    TabOrder = 4
    Text = '1'
    Value = 1
    OnExit = edtMaxColumnExit
  end
  object edtMaxColumn: TJvValidateEdit
    Left = 80
    Top = 176
    Width = 57
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 150.000000000000000000
    PasswordChar = #0
    TabOrder = 3
    Text = '0'
    Value = 0
    OnExit = edtMaxColumnExit
  end
  object edtMinColumn: TJvValidateEdit
    Left = 80
    Top = 148
    Width = 57
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 150.000000000000000000
    PasswordChar = #0
    TabOrder = 2
    Text = '0'
    Value = 0
    OnExit = edtMinColumnExit
  end
  object gbWhat: TGroupBox
    Left = 2
    Top = 24
    Width = 95
    Height = 117
    Caption = 'What to Align'
    TabOrder = 1
    object cbAlignAsign: TCheckBox
      Left = 6
      Top = 17
      Width = 80
      Height = 17
      Caption = 'Assign'
      TabOrder = 0
    end
    object cbAlignConst: TCheckBox
      Left = 6
      Top = 36
      Width = 71
      Height = 17
      Caption = 'Const'
      TabOrder = 1
    end
    object cbAlignVar: TCheckBox
      Left = 6
      Top = 55
      Width = 80
      Height = 17
      Caption = 'Var'
      TabOrder = 2
    end
    object cbAlignTypedef: TCheckBox
      Left = 6
      Top = 74
      Width = 75
      Height = 17
      Caption = 'Type defs'
      TabOrder = 3
    end
    object cbAlignComment: TCheckBox
      Left = 6
      Top = 94
      Width = 75
      Height = 17
      Caption = 'Comment'
      TabOrder = 4
    end
  end
  object eMaxUnaligned: TJvValidateEdit
    Left = 80
    Top = 232
    Width = 57
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 999.000000000000000000
    PasswordChar = #0
    TabOrder = 5
    Text = '0'
    Value = 0
  end
end
