unit fRegistrySettings;

{ settings form for JCF notepad registy options }

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  { JCL }
  JvEdit, JvTypedEdit, JcfRegistrySettings;

type
  TfmRegistrySettings = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    eSettingsFile: TEdit;
    sbFile: TSpeedButton;
    Label1: TLabel;
    btnClearMRU: TButton;
    Label2: TLabel;
    eMRUMaxItems: TJvIntegerEdit;
    rgShowParseTree: TRadioGroup;
    dlgOpen: TOpenDialog;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnClearMRUClick(Sender: TObject);
    procedure eSettingsFileKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sbFileClick(Sender: TObject);
  private

    procedure ReadSettings;
    procedure WriteSettings;

  public
    procedure Execute;

  end;


implementation

uses
  { jcf }
  ConvertTypes, JcfSettings;

{$R *.dfm}

procedure TfmRegistrySettings.ReadSettings;
var
  lcSet: TJCFRegistrySettings;
begin
  lcSet := GetRegSettings;
  Assert(lcSet <> nil);

  eSettingsFile.Text := lcSet.FormatConfigFileName;
  eMRUMaxItems.Value := lcSet.MRUMaxItems;
  rgShowParseTree.ItemIndex := Ord(lcSet.ShowParseTreeOption);

  btnClearMRU.Enabled := GetRegSettings.CanClearMRU;
end;

procedure TfmRegistrySettings.WriteSettings;
var
  lcSet: TJCFRegistrySettings;
begin
  lcSet := GetRegSettings;
  Assert(lcSet <> nil);

  if lcSet.FormatConfigFileName <> eSettingsFile.Text then
  begin
    lcSet.FormatConfigFileName := eSettingsFile.Text;
    FormatSettings.ReadFromFile(eSettingsFile.Text);
  end;

  lcSet.MRUMaxItems := eMRUMaxItems.Value;
  lcSet.ShowParseTreeOption := TShowParseTreeOption(rgShowParseTree.ItemIndex);
end;

procedure TfmRegistrySettings.Execute;
begin
  ReadSettings;
  ShowModal;
end;

procedure TfmRegistrySettings.btnOKClick(Sender: TObject);
begin
  WriteSettings;
  Close;
end;

procedure TfmRegistrySettings.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmRegistrySettings.btnClearMRUClick(Sender: TObject);
begin
  GetRegSettings.ClearMRU;
  btnClearMRU.Enabled := False;
end;

procedure TfmRegistrySettings.eSettingsFileKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    sbFileClick(sender);
end;

procedure TfmRegistrySettings.sbFileClick(Sender: TObject);
begin
  dlgOpen.Filter := CONFIG_FILE_FILTERS;

  if dlgOpen.Execute then
    eSettingsFile.Text := dlgOpen.FileName;
end;

end.
