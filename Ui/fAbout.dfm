object frmAboutBox: TfrmAboutBox
  Left = 156
  Top = 114
  BorderStyle = bsDialog
  Caption = 'About Jedi Code Format'
  ClientHeight = 305
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object bbOK: TBitBtn
    Left = 136
    Top = 274
    Width = 80
    Height = 30
    TabOrder = 0
    Kind = bkOK
  end
  inline FrameAbout: TFrameAbout
    Width = 352
    Height = 269
    Align = alTop
    TabOrder = 1
    inherited Panel1: TPanel
      Width = 352
      inherited mWarning: TMemo
        Width = 348
      end
      inherited lblMPL: TStaticText
        Cursor = crHandPoint
      end
    end
  end
end
