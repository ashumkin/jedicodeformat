inherited fUses: TfUses
  Width = 366
  Height = 317
  object cbRemoveEnabled: TCheckBox
    Left = 4
    Top = 4
    Width = 129
    Height = 17
    Caption = 'Remove'
    TabOrder = 0
    OnClick = cbRemoveEnabledClick
  end
  object cbInsertInterface: TCheckBox
    Left = 4
    Top = 108
    Width = 137
    Height = 17
    Caption = 'Insert into  Interface'
    TabOrder = 2
    OnClick = cbInsertInterfaceClick
  end
  object cbInsertImplementation: TCheckBox
    Left = 176
    Top = 108
    Width = 141
    Height = 17
    Caption = 'Insert into Implementation'
    TabOrder = 4
    OnClick = cbInsertImplementationClick
  end
  object cbFindReplace: TCheckBox
    Left = 4
    Top = 212
    Width = 113
    Height = 17
    Caption = 'Replace'
    TabOrder = 6
    OnClick = cbFindReplaceClick
  end
  object mRemove: TMemo
    Left = 4
    Top = 24
    Width = 160
    Height = 70
    TabOrder = 1
  end
  object mInsertInterface: TMemo
    Left = 4
    Top = 128
    Width = 160
    Height = 70
    TabOrder = 3
  end
  object mFind: TMemo
    Left = 4
    Top = 232
    Width = 160
    Height = 70
    TabOrder = 7
  end
  object mInsertImplementation: TMemo
    Left = 176
    Top = 128
    Width = 160
    Height = 70
    Lines.Strings = (
      '')
    TabOrder = 5
  end
  object mReplace: TMemo
    Left = 176
    Top = 232
    Width = 160
    Height = 70
    TabOrder = 8
  end
end
