inherited fClarifyLongLineBreaker: TfClarifyLongLineBreaker
  Width = 437
  Height = 523
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Max line length'
  end
  object edtMaxLineLength: TJvIntegerEdit
    Left = 88
    Top = 6
    Width = 49
    Height = 21
    Alignment = taRightJustify
    MaxLength = 3
    ReadOnly = False
    TabOrder = 0
    Value = 0
    MaxValue = 255
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
  end
  object rgRebreakLongLines: TRadioGroup
    Left = 8
    Top = 44
    Width = 309
    Height = 85
    Caption = '&Break lines that are longer than max line length'
    ItemIndex = 1
    Items.Strings = (
      '&Never'
      '&Sometimes, if a good place to break is found'
      '&Usually, unless there is no acceptable place to break')
    TabOrder = 1
  end
end
