unit frUses;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  { local }
  frmBaseSettingsFrame;

type
  TfUses = class(TfrSettingsFrame)
    cbRemoveEnabled: TCheckBox;
    cbInsertInterface: TCheckBox;
    cbInsertImplementation: TCheckBox;
    cbFindReplace: TCheckBox;
    mRemove: TMemo;
    mInsertInterface: TMemo;
    mFind: TMemo;
    mInsertImplementation: TMemo;
    mReplace: TMemo;
    procedure cbInsertInterfaceClick(Sender: TObject);
    procedure cbInsertImplementationClick(Sender: TObject);
    procedure cbRemoveEnabledClick(Sender: TObject);
    procedure cbFindReplaceClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

{$R *.DFM}

uses
  { local } JcfHelp;

constructor TfUses.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_FIND_ANDREPLACE_USES;
end;


procedure TfUses.Read;
begin
  with Settings.UsesClause do
  begin
    cbRemoveEnabled.Checked := RemoveEnabled;
    cbInsertInterface.Checked := InsertInterfaceEnabled;
    cbInsertImplementation.Checked := InsertImplementationEnabled;
    cbFindReplace.Checked := FindReplaceEnabled;

    mRemove.Lines.Assign(Remove);
    mInsertInterface.Lines.Assign(InsertInterface);
    mInsertImplementation.Lines.Assign(InsertImplementation);
    mFind.Lines.Assign(Find);
    mReplace.Lines.Assign(Replace);
  end;

  cbInsertInterfaceClick(nil);
  cbInsertImplementationClick(nil);
  cbRemoveEnabledClick(nil);
  cbFindReplaceClick(nil);
end;

procedure TfUses.Write;
begin
  with Settings.UsesClause do
  begin
    RemoveEnabled := cbRemoveEnabled.Checked;
    InsertInterfaceEnabled := cbInsertInterface.Checked;
    InsertImplementationEnabled := cbInsertImplementation.Checked;
    FindReplaceEnabled := cbFindReplace.Checked;

    Remove.Assign(mRemove.Lines);
    InsertInterface.Assign(mInsertInterface.Lines);
    InsertImplementation.Assign(mInsertImplementation.Lines);
    Find.Assign(mFind.Lines);
    Replace.Assign(mReplace.Lines);
  end;

end;

procedure TfUses.cbInsertInterfaceClick(Sender: TObject);
begin
  mInsertInterface.Enabled := cbInsertInterface.Checked;
end;

procedure TfUses.cbInsertImplementationClick(Sender: TObject);
begin
  mInsertImplementation.Enabled := cbInsertImplementation.Checked;
end;

procedure TfUses.cbRemoveEnabledClick(Sender: TObject);
begin
  mRemove.Enabled := cbRemoveEnabled.Checked;
end;

procedure TfUses.cbFindReplaceClick(Sender: TObject);
begin
  mFind.Enabled := cbFindReplace.Checked;
  mReplace.Enabled := cbFindReplace.Checked;
end;

end.
