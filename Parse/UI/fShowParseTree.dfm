object frmShowParseTree: TfrmShowParseTree
  Left = 319
  Top = 116
  Width = 467
  Height = 421
  Caption = 'Parse Tree'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 368
    Width = 459
    Height = 19
    Panels = <>
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 459
    Height = 69
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblTreeCount: TLabel
      Left = 8
      Top = 24
      Width = 83
      Height = 13
      Caption = 'Tree has ? nodes'
    end
    object lblTreeDepth: TLabel
      Left = 8
      Top = 44
      Width = 121
      Height = 13
      Caption = 'Tree has max depth of ??'
    end
    object cbShowWhiteSpace: TCheckBox
      Left = 8
      Top = 4
      Width = 117
      Height = 17
      Caption = 'Show whitespace'
      TabOrder = 0
      OnClick = cbShowWhiteSpaceClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 327
    Width = 459
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblCurrent: TLabel
      Left = 8
      Top = 6
      Width = 37
      Height = 13
      Caption = 'Current:'
    end
    object lblDepth: TLabel
      Left = 8
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Depth:'
    end
    object lblTotalNodeCount: TLabel
      Left = 220
      Top = 24
      Width = 84
      Height = 13
      Caption = 'Total node count:'
    end
    object lblImmediateChildCount: TLabel
      Left = 220
      Top = 6
      Width = 106
      Height = 13
      Caption = 'Immediate child count:'
    end
  end
  object pcPages: TPageControl
    Left = 0
    Top = 69
    Width = 459
    Height = 258
    ActivePage = tsTokens
    Align = alClient
    TabOrder = 3
    object tsTokens: TTabSheet
      Caption = 'Tokens'
      object lvTokens: TListView
        Left = 0
        Top = 0
        Width = 451
        Height = 230
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
          end
          item
            Caption = 'Type'
            Width = 150
          end
          item
            Caption = 'Text'
            Width = 220
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvTokensDblClick
        OnSelectItem = lvTokensSelectItem
      end
    end
    object tsTree: TTabSheet
      Caption = 'Tree'
      ImageIndex = 1
      object tvParseTree: TTreeView
        Left = 0
        Top = 0
        Width = 451
        Height = 230
        Align = alClient
        HideSelection = False
        Indent = 15
        MultiSelectStyle = []
        ReadOnly = True
        TabOrder = 0
        OnChange = tvParseTreeChange
        OnDblClick = tvParseTreeDblClick
      end
    end
  end
end
