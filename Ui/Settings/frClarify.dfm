inherited fClarify: TfClarify
  Width = 426
  Height = 292
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 113
    Height = 13
    Caption = 'File extensions to format'
  end
  object rgRunOnceOffs: TRadioGroup
    Left = 136
    Top = 12
    Width = 141
    Height = 85
    Caption = 'Run once-offs'
    Items.Strings = (
      'Do &not run'
      'Do &run'
      'Run &only these')
    TabOrder = 0
  end
  object mFileExtensions: TMemo
    Left = 8
    Top = 24
    Width = 109
    Height = 109
    TabOrder = 1
  end
end
