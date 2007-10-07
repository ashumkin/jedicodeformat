unit frAsm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, 
  { local}
  frmBaseSettingsFrame, JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TfAsm = class(TfrSettingsFrame)
    cbEnabled: TCheckBox;
    rgCaps: TRadioGroup;
    gbIndents: TGroupBox;
    edtIndent6: TJvValidateEdit;
    edtIndent5: TJvValidateEdit;
    edtIndent4: TJvValidateEdit;
    edtIndent3: TJvValidateEdit;
    edtIndent2: TJvValidateEdit;
    edtIndent1: TJvValidateEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edtBreaksAfterLabel: TJvValidateEdit;
  private
  public
    procedure Read; override;
    procedure Write; override;

  end;

implementation

uses JcfHelp, JcfSettings, Capitalisation, SettingsTypes;

{$R *.dfm}

procedure TfAsm.Read;
var
  liIndex: integer;
begin
  cbEnabled.Checked := FormatSettings.SetAsm.Enabled;

  with FormatSettings.SetAsm do
  begin
    rgCaps.ItemIndex := Ord(Capitalisation);

    edtBreaksAfterLabel.Value := BreaksAfterLabel;

    edtIndent1.Value := Indents[0];
    edtIndent2.Value := Indents[1];
    edtIndent3.Value := Indents[2];
    edtIndent4.Value := Indents[3];
    edtIndent5.Value := Indents[4];
    edtIndent6.Value := Indents[5];

  end;

end;

procedure TfAsm.Write;
begin
  FormatSettings.SetAsm.Enabled := cbEnabled.Checked;

  with FormatSettings.SetAsm do
  begin
    Capitalisation := TCapitalisationType(rgCaps.ItemIndex);

    BreaksAfterLabel := edtBreaksAfterLabel.Value;

    Indents[0] := edtIndent1.Value;
    Indents[1] := edtIndent2.Value;
    Indents[2] := edtIndent3.Value;
    Indents[3] := edtIndent4.Value;
    Indents[4] := edtIndent5.Value;
    Indents[5] := edtIndent6.Value;
  end;
end;

end.
