unit frTransform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls,
  { local }
  frmBaseSettingsFrame;

type
  TfTransform = class(TfrSettingsFrame)
    cbBlockEndSemicolons: TCheckBox;
    rbBeginEnd: TRadioGroup;
    bgSortUses: TGroupBox;
    cbSortInterfaceUses: TCheckBox;
    cbSortImplementationUses: TCheckBox;
    cbBreakUsesSortOnComment: TCheckBox;
    cbBreakUsesSortOnReturn: TCheckBox;
    rgUsesSortOrder: TRadioGroup;
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

uses SettingsTypes, JcfSettings, JcfHelp, SetTransform;

{$R *.dfm}

constructor TfTransform.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_TRANSFORM;
end;

procedure TfTransform.Read;
begin
  with FormatSettings.Transform do
  begin
    rbBeginEnd.ItemIndex := Ord(BeginEndStyle);
    cbBlockEndSemicolons.Checked := AddBlockEndSemicolon;

    cbSortInterfaceUses.Checked := SortInterfaceUses;
    cbSortImplementationUses.Checked := SortImplementationUses;
    cbBreakUsesSortOnReturn.Checked := BreakUsesSortOnReturn;
    cbBreakUsesSortOnComment.Checked := BreakUsesSortOnComment;

    rgUsesSortOrder.ItemIndex := Ord(UsesSortOrder);
  end;

end;

procedure TfTransform.Write;
begin
  with FormatSettings.Transform do
  begin
    BeginEndStyle := TTriOptionStyle(rbBeginEnd.ItemIndex);
    AddBlockEndSemicolon := cbBlockEndSemicolons.Checked;

    SortInterfaceUses := cbSortInterfaceUses.Checked;
    SortImplementationUses := cbSortImplementationUses.Checked;
    BreakUsesSortOnReturn := cbBreakUsesSortOnReturn.Checked;
    BreakUsesSortOnComment := cbBreakUsesSortOnComment.Checked;

    UsesSortOrder := TUsesSortOrder(rgUsesSortOrder.ItemIndex);
  end;

end;

end.
