object ExceptionDialog: TExceptionDialog
  Left = 294
  Top = 195
  BorderIcons = [biSystemMenu]
  Caption = 'JCF Exception'
  ClientHeight = 135
  ClientWidth = 315
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object btnOk: TButton
    Left = 116
    Top = 100
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = btnOkClick
  end
  object mExceptionMessage: TMemo
    Left = 0
    Top = 0
    Width = 307
    Height = 93
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
end
