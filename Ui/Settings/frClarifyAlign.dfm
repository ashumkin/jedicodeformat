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
  object edtMaxVariance: TJvIntegerEdit
    Left = 80
    Top = 204
    Width = 57
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 1
    OnExit = edtMaxColumnExit
    Value = 0
    MaxValue = 999
    MinValue = 1
    HasMaxValue = True
    HasMinValue = True
  end
  object edtMaxColumn: TJvIntegerEdit
    Left = 80
    Top = 176
    Width = 57
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 2
    OnExit = edtMaxColumnExit
    Value = 0
    MaxValue = 150
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
  object edtMinColumn: TJvIntegerEdit
    Left = 80
    Top = 148
    Width = 57
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 3
    OnExit = edtMinColumnExit
    Value = 0
    MaxValue = 150
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
  object gbWhat: TGroupBox
    Left = 2
    Top = 24
    Width = 95
    Height = 117
    Caption = 'What to Align'
    TabOrder = 4
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
  object eMaxUnaligned: TJvIntegerEdit
    Left = 80
    Top = 232
    Width = 57
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 5
    Value = 0
    MaxValue = 999
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
end
