unit frUnitCaps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  { local} frmBaseSettingsFrame, StdCtrls, JvMemo;

type
  TfrUnitNameCaps = class(TfrSettingsFrame)
    mWords: TJvMemo;
    cbEnableAnyWords: TCheckBox;
    Label1: TLabel;
    procedure FrameResize(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

{$R *.dfm}

uses JcfHelp, JcfSettings;

constructor TfrUnitNameCaps.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TfrUnitNameCaps.Read;
begin
  inherited;
  with FormatSettings.UnitNameCaps do
  begin
    cbEnableAnyWords.Checked := Enabled;
    mWords.Lines.Assign(Words);
  end;

end;

procedure TfrUnitNameCaps.Write;
begin
  inherited;
  with FormatSettings.UnitNameCaps do
  begin
    Enabled := cbEnableAnyWords.Checked;
    Words.Assign(mWords.Lines);
  end;

end;

procedure TfrUnitNameCaps.FrameResize(Sender: TObject);
begin
  mWords.Height := ClientHeight -
    (cbEnableAnyWords.Top + cbEnableAnyWords.Height + GUI_PAD);
end;

end.
