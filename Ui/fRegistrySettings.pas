unit fRegistrySettings;

{ settings form for JCF notepad registy options }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is fRegistrySettings, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.
------------------------------------------------------------------------------*)
{*)}

interface

uses
  { delphi }
  Windows, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  { JVCL }
  JvMemo, JvEdit, JvExStdCtrls, JvValidateEdit;

type
  TfmRegistrySettings = class(TForm)
    dlgOpen: TOpenDialog;
    pgPages: TPageControl;
    tsGeneral: TTabSheet;
    tsLogFile: TTabSheet;
    eSettingsFile: TEdit;
    sbFile: TSpeedButton;
    Label1: TLabel;
    eMRUMaxItems: TJvValidateEdit;
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
    tsExclusions: TTabSheet;
    lblFilesCaption: TLabel;
    lblDirsCaption: TLabel;
    mFiles: TJvMemo;
    mDirs: TJvMemo;
    rgWriteSettingsFile: TRadioGroup;
    cbCheckMultibyteChars: TCheckBox;
    tsIde: TTabSheet;
    cbEditorIntegration: TCheckBox;
    cbFormatBeforeSave: TCheckBox;
    cbFormatAfterLoad: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnClearMRUClick(Sender: TObject);
    procedure eSettingsFileKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure sbFileClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbSpecifedDirClick(Sender: TObject);
    procedure btnViewLogClick(Sender: TObject);
    procedure tsExclusionsResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure cbFormatAfterLoadClick(Sender: TObject);
    procedure cbFormatBeforeSaveClick(Sender: TObject);
  private
    fsSpecifiedDirectory: string;

    procedure ShowDirs;

    procedure ReadSettings;
    procedure WriteSettings;

  public
    procedure Execute;

  end;


implementation

{$WARN UNIT_PLATFORM OFF}
uses
  { delphi }
  FileCtrl,
  { jcl }
  JclSysInfo,
  { jcf }
  ConvertTypes, JcfRegistrySettings, JcfSettings, JCFHelp;

{$R *.dfm}

procedure TfmRegistrySettings.ReadSettings;
var
  lcSet: TJCFRegistrySettings;
begin
  lcSet := GetRegSettings;
  Assert(lcSet <> nil);

  { general }
  eSettingsFile.Text := lcSet.FormatConfigFileName;
  rgWriteSettingsFile.ItemIndex := Ord(lcSet.FormatFileWriteOption);

  eMRUMaxItems.Value := lcSet.MRUMaxItems;
  rgShowParseTree.ItemIndex := Ord(lcSet.ShowParseTreeOption);

  cbCheckMultibyteChars.Checked := lcSet.CheckMultiByteChars;

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

  { IDE }
  cbEditorIntegration.Checked := lcSet.EditorIntegration;
  cbFormatAfterLoad.Checked := lcSet.FormatAfterLoad;
  cbFormatBeforeSave.Checked := lcSet.FormatBeforeSave;

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

  lcSet.FormatFileWriteOption := TFormatFileWriteOption(rgWriteSettingsFile.ItemIndex);

  lcSet.MRUMaxItems := eMRUMaxItems.Value;
  lcSet.ShowParseTreeOption := TShowParseTreeOption(rgShowParseTree.ItemIndex);

  lcSet.CheckMultiByteChars := cbCheckMultibyteChars.Checked;

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

  { IDE }
  lcSet.EditorIntegration := cbEditorIntegration.Checked;
  lcSet.FormatAfterLoad := cbFormatAfterLoad.Checked;
  lcSet.FormatBeforeSave := cbFormatBeforeSave.Checked;

   lcSet.WriteAll;
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
  var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    sbFileClick(Sender);
end;

procedure TfmRegistrySettings.sbFileClick(Sender: TObject);
begin
  dlgOpen.Filter := CONFIG_FILE_FILTERS;

  if dlgOpen.Execute then
    eSettingsFile.Text := dlgOpen.FileName;
end;

procedure TfmRegistrySettings.FormResize(Sender: TObject);
const
  SPACING     = 8;
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
    fsSpecifiedDirectory := IncludeTrailingPathDelimiter(lsDir);
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
  rgLogDir.Items[1] := 'Application: ' + IncludeTrailingPathDelimiter(
    ExtractFileDir(ParamStr(0)));
  rgLogDir.Items[2] := 'Specified: ' + fsSpecifiedDirectory;
end;

procedure TfmRegistrySettings.tsExclusionsResize(Sender: TObject);
const
  SPACING = 8;
begin
  mFiles.Left  := SPACING;
  mFiles.Width := tsExclusions.ClientWidth - (SPACING * 2);

  mDirs.Left  := SPACING;
  mDirs.Width := tsExclusions.ClientWidth - (SPACING * 2);
end;

procedure TfmRegistrySettings.FormKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    Application.HelpContext(HELP_MAIN);

end;

procedure TfmRegistrySettings.cbFormatAfterLoadClick(Sender: TObject);
begin
  if cbFormatAfterLoad.Checked then
    cbEditorIntegration.Checked := True;
end;

procedure TfmRegistrySettings.cbFormatBeforeSaveClick(Sender: TObject);
begin
  if cbFormatBeforeSave.Checked then
    cbEditorIntegration.Checked := True;
end;

end.
