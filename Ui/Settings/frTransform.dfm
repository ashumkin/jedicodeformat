inherited fTransform: TfTransform
  Width = 360
  Height = 278
  object cbBlockEndSemicolons: TCheckBox
    Left = 4
    Top = 88
    Width = 253
    Height = 17
    Caption = 'Put &semicolons after last statement in a block'
    TabOrder = 0
  end
  object rbBeginEnd: TRadioGroup
    Left = 4
    Top = 4
    Width = 294
    Height = 77
    Caption = 'Add or Remove &begin and end from single statements'
    Items.Strings = (
      'Add begin and end to single statements'
      'Leave begin and end as is'
      'Remove begin and end from around single statements')
    TabOrder = 1
  end
  object bgSortUses: TGroupBox
    Left = 4
    Top = 110
    Width = 161
    Height = 131
    Caption = 'Sort &uses clauses'
    TabOrder = 2
    object cbSortInterfaceUses: TCheckBox
      Left = 8
      Top = 20
      Width = 121
      Height = 17
      Caption = 'Sort i&nterface uses'
      TabOrder = 0
    end
    object cbSortImplementationUses: TCheckBox
      Left = 8
      Top = 38
      Width = 145
      Height = 17
      Caption = 'Sort i&mplementation uses'
      TabOrder = 1
    end
    object cbBreakUsesSortOnComment: TCheckBox
      Left = 8
      Top = 80
      Width = 133
      Height = 17
      Caption = 'Break on &comment'
      TabOrder = 2
    end
    object cbBreakUsesSortOnReturn: TCheckBox
      Left = 8
      Top = 64
      Width = 109
      Height = 17
      Caption = 'Break on &return'
      TabOrder = 3
    end
    object cbNoComments: TCheckBox
      Left = 8
      Top = 104
      Width = 133
      Height = 17
      Caption = 'Only with no comments'
      TabOrder = 4
    end
  end
  object rgUsesSortOrder: TRadioGroup
    Left = 172
    Top = 110
    Width = 133
    Height = 97
    Caption = 'Uses sort &order'
    Items.Strings = (
      'Alphabetic'
      'Reverse Alphabetic'
      'Shortest to longest'
      'Longest to shortest')
    TabOrder = 3
  end
end
