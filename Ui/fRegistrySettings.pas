unit fRegistrySettings;

{ settings form for JCF notepad registy options }

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, 
  { JCL }
  JvEdit, JvTypedEdit, JvMemo;

type
  TfmRegistrySettings = class(TForm)
    dlgOpen: TOpenDialog;
    pgPages: TPageControl;
    tsGeneral: TTabSheet;
    tsLogFile: TTabSheet;
    eSettingsFile: TEdit;
    sbFile: TSpeedButton;
    Label1: TLabel;
    eMRUMaxItems: TJvIntegerEdit;
    btnClearMRU: TButton;
    Label2: TLabel;
    rgShowParseTree: TRadioGroup;
    pnlBottom: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    sbSpecifedDir: TSpeedButton;
    Label3: TLabel;
    lblBackupFileExt: TLabel;
    lblOutputFileExt: TLabel;
    rgLogLevel: TRadioGroup;
    rgLogDir: TRadioGroup;
    btnViewLog: TButton;
    cbViewLog: TCheckBox;
    edtBackupExt: TEdit;
    edtOutputExt: TEdit;
    cbLogTime: TCheckBox;
    TabSheet1: TTabSheet;
    lblFilesCaption: TLabel;
    lblDirsCaption: TLabel;
    mFiles: TJvMemo;
    mDirs: TJvMemo;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnClearMRUClick(Sender: TObject);
    procedure eSettingsFileKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sbFileClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbSpecifedDirClick(Sender: TObject);
    procedure btnViewLogClick(Sender: TObject);
  private
    fsSpecifiedDirectory: string;

    procedure ShowDirs;

    procedure ReadSettings;
    procedure WriteSettings;

  public
    procedure Execute;

  end;


implementation

uses
  { delphi }
  FileCtrl,
  { jcl }
  JclFileUtils, JclShell, JclSysInfo,
  { jcf }
  ConvertTypes, JcfRegistrySettings, JcfSettings;

{$R *.dfm}

procedure TfmRegistrySettings.ReadSettings;
var
  lcSet: TJCFRegistrySettings;
begin
  lcSet := GetRegSettings;
  Assert(lcSet <> nil);

  { general }
  eSettingsFile.Text := lcSet.FormatConfigFileName;
  eMRUMaxItems.Value := lcSet.MRUMaxItems;
  rgShowParseTree.ItemIndex := Ord(lcSet.ShowParseTreeOption);

  { mru }
  btnClearMRU.Enabled := GetRegSettings.CanClearMRU;

  { log }
  rgLogLevel.ItemIndex := Ord(lcSet.LogLevel);
  rgLogDir.ItemIndex   := Ord(lcSet.LogPlace);
  fsSpecifiedDirectory := lcSet.SpecifiedDirectory;
  cbViewLog.Checked    := lcSet.ViewLogAfterRun;
  cbLogTime.Checked    := lcSet.LogTime;

  edtBackupExt.Text := lcSet.BackupExtension;
  edtOutputExt.Text := lcSet.OutputExtension;

  { exclusions}
  mFiles.Lines.Assign(lcSet.ExclusionsFiles);
  mDirs.Lines.Assign(lcSet.ExclusionsDirs);


  ShowDirs;
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

  { log files }
  lcSet.LogLevel := TLogLevel(rgLogLevel.ItemIndex);
  lcSet.LogPlace := TLogPlace(rgLogDir.ItemIndex);
  lcSet.SpecifiedDirectory := fsSpecifiedDirectory;
  lcSet.ViewLogAfterRun := cbViewLog.Checked;
  lcSet.LogTime  := cbLogTime.Checked;

  lcSet.BackupExtension := edtBackupExt.Text;
  lcSet.OutputExtension := edtOutputExt.Text;

  { exclusions }
  lcSet.ExclusionsFiles.Assign(mFiles.Lines);
  lcSet.ExclusionsDirs.Assign(mDirs.Lines);

end;

procedure TfmRegistrySettings.Execute;
begin
  ReadSettings;
  FormResize(nil);

  pgPages.ActivePage := tsGeneral;

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

procedure TfmRegistrySettings.FormResize(Sender: TObject);
const
  SPACING = 8;
  SMALL_SPACE = 4;
begin
  sbFile.Left := tsGeneral.ClientWidth - (sbFile.Width + SPACING);
  eSettingsFile.Width := tsGeneral.ClientWidth -
    (eSettingsFile.Left + sbFile.Width + SPACING + SMALL_SPACE);

  rgLogDir.Width := tsGeneral.ClientWidth - (rgLogDir.Left + SPACING);

  btnViewLog.Left := tsGeneral.ClientWidth - (btnViewLog.Width + SPACING);
end;

procedure TfmRegistrySettings.sbSpecifedDirClick(Sender: TObject);
var
  lsDir: string;
begin
  if SelectDirectory('select a directory', '', lsDir) then
  begin
    fsSpecifiedDirectory := PathAddSeparator(lsDir);
    ShowDirs;
  end;
end;

procedure TfmRegistrySettings.btnViewLogClick(Sender: TObject);
begin
  GetRegSettings.ViewLog;
end;

procedure TfmRegistrySettings.ShowDirs;
begin
  rgLogDir.Items[0] := 'Temp: ' + GetWindowsTempFolder;
  rgLogDir.Items[1] := 'Application: ' +  PathAddSeparator(ExtractFileDir(ParamStr(0)));
  rgLogDir.Items[2] := 'Specified: ' + fsSpecifiedDirectory;
end;

end.
