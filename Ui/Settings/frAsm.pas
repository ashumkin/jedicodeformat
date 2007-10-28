unit frAsm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, 
  { local}
  frmBaseSettingsFrame, JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TfAsm = class(TfrSettingsFrame)
    rgCaps: TRadioGroup;
    gbStatementIndent: TGroupBox;
    edtStatementIndent: TJvValidateEdit;
    cbStatementIndent: TCheckBox;
    gbBreaksAfterLabel: TGroupBox;
    cbBreaksAfterLabelEnabled: TCheckBox;
    edtBreaksAfterLabel: TJvValidateEdit;
    Label7: TLabel;
    Label1: TLabel;
    gbParamsIndent: TGroupBox;
    Label2: TLabel;
    edtParamsIndent: TJvValidateEdit;
    cbParamsIndent: TCheckBox;
  private
  public
    procedure Read; override;
    procedure Write; override;

  end;

implementation

uses JcfHelp, JcfSettings, Capitalisation, SettingsTypes, SetAsm;

{$R *.dfm}

procedure TfAsm.Read;
begin

  with FormatSettings.SetAsm do
  begin

    cbStatementIndent.Checked := StatementIndentEnabled;
    edtStatementIndent.Value := StatementIndent;

    cbParamsIndent.Checked := ParamsIndentEnabled;
    edtParamsIndent.Value := ParamsIndent;

    cbBreaksAfterLabelEnabled.Checked := BreaksAfterLabelEnabled;
    edtBreaksAfterLabel.Value := BreaksAfterLabel;

    rgCaps.ItemIndex := Ord(Capitalisation);
  end;

end;

procedure TfAsm.Write;
begin

  with FormatSettings.SetAsm do
  begin
    StatementIndentEnabled := cbStatementIndent.Checked;
    StatementIndent := edtStatementIndent.Value;

    ParamsIndentEnabled := cbParamsIndent.Checked;
    ParamsIndent := edtParamsIndent.Value;

    BreaksAfterLabelEnabled := cbBreaksAfterLabelEnabled.Checked;
    BreaksAfterLabel := edtBreaksAfterLabel.Value;

    Capitalisation := TCapitalisationType(rgCaps.ItemIndex);
  end;
end;

end.
