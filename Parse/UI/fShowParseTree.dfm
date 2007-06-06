object frmShowParseTree: TfrmShowParseTree
  Left = 319
  Top = 116
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'JCF Parse Tree'
  ClientHeight = 416
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object StatusBar1: TStatusBar
    Left = 0
    Top = 391
    Width = 436
    Height = 25
    Panels = <>
    ExplicitTop = 259
    ExplicitWidth = 383
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 64
    Align = alTop
    BevelOuter = bvNone
    Constraints.MinHeight = 50
    Constraints.MinWidth = 50
    TabOrder = 1
    ExplicitWidth = 383
    object lblTreeCount: TLabel
      Left = 8
      Top = 23
      Width = 87
      Height = 15
      Caption = 'Tree has ? nodes'
    end
    object lblTreeDepth: TLabel
      Left = 8
      Top = 41
      Width = 130
      Height = 15
      Caption = 'Tree has max depth of ??'
    end
    object cbShowWhiteSpace: TCheckBox
      Left = 8
      Top = 4
      Width = 108
      Height = 16
      Caption = 'Show whitespace'
      TabOrder = 0
      OnClick = cbShowWhiteSpaceClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 353
    Width = 436
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 221
    ExplicitWidth = 383
    object lblCurrent: TLabel
      Left = 8
      Top = 5
      Width = 43
      Height = 15
      Caption = 'Current:'
    end
    object lblDepth: TLabel
      Left = 8
      Top = 23
      Width = 35
      Height = 15
      Caption = 'Depth:'
    end
    object lblTotalNodeCount: TLabel
      Left = 203
      Top = 23
      Width = 94
      Height = 15
      Caption = 'Total node count:'
    end
    object lblImmediateChildCount: TLabel
      Left = 203
      Top = 5
      Width = 123
      Height = 15
      Caption = 'Immediate child count:'
    end
  end
  object pcPages: TPageControl
    Left = 0
    Top = 64
    Width = 436
    Height = 289
    ActivePage = tsTokens
    Align = alClient
    TabOrder = 3
    ExplicitWidth = 383
    ExplicitHeight = 157
    object tsTokens: TTabSheet
      Caption = 'Tokens'
      ExplicitWidth = 375
      ExplicitHeight = 127
      object lvTokens: TListView
        Left = 0
        Top = 0
        Width = 428
        Height = 259
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 47
          end
          item
            Caption = 'Type'
            Width = 139
          end
          item
            Caption = 'Text'
            Width = 203
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvTokensDblClick
        OnSelectItem = lvTokensSelectItem
        ExplicitWidth = 375
        ExplicitHeight = 127
      end
    end
    object tsTree: TTabSheet
      Caption = 'Tree'
      ImageIndex = 1
      object tvParseTree: TTreeView
        Left = 0
        Top = 0
        Width = 428
        Height = 259
        Align = alClient
        HideSelection = False
        Indent = 22
        MultiSelectStyle = []
        ReadOnly = True
        TabOrder = 0
        OnChange = tvParseTreeChange
        OnDblClick = tvParseTreeDblClick
      end
    end
  end
end
