{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frLog.pas, released April 2000.
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

unit frFiles;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  { local } frmBaseSettingsFrame;

type
  TfFiles = class(TfrSettingsFrame)
    rgLogLevel: TRadioGroup;
    rgLogDir: TRadioGroup;
    sbSpecifedDir: TSpeedButton;
    Label1: TLabel;
    btnViewLog: TButton;
    cbViewLog: TCheckBox;
    lblBackupFileExt: TLabel;
    edtBackupExt: TEdit;
    lblOutputFileExt: TLabel;
    edtOutputExt: TEdit;
    cbLogTime: TCheckBox;
    procedure sbSpecifedDirClick(Sender: TObject);
    procedure btnViewLogClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    fsSpecifiedDirectory: string;

    procedure ShowDirs;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

uses
    { delphi }
    { jcl } JclSysInfo, JclFileUtils,
    { local } FileUtils, JcfRegistrySettings, JcfSettings, JCFHelp;

{$R *.DFM}

constructor TfFiles.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_LOGGING;
end;

procedure TfFiles.sbSpecifedDirClick(Sender: TObject);
var
  lsDir: string;
begin
  if SelectDirectory('select a directory', '', lsDir) then
  begin
    fsSpecifiedDirectory := PathAddSeparator(lsDir);
    ShowDirs;
  end;
end;

procedure TfFiles.ShowDirs;
begin
  rgLogDir.Items[0] := 'Temp: ' + GetWindowsTempFolder;
  rgLogDir.Items[1] := 'Application: ' +  PathAddSeparator(ExtractFileDir(ParamStr(0)));
  rgLogDir.Items[2] := 'Specified: ' + fsSpecifiedDirectory;
end;

procedure TfFiles.Read;
begin
  with GetRegSettings do
  begin
    rgLogLevel.ItemIndex := Ord(LogLevel);
    rgLogDir.ItemIndex   := Ord(LogPlace);
    fsSpecifiedDirectory := SpecifiedDirectory;
    cbViewLog.Checked    := ViewLogAfterRun;
    cbLogTime.Checked    := LogTime;
  end;

  with FormatSettings.FileSettings do
  begin
    edtBackupExt.Text := BackupExtension;
    edtOutputExt.Text := OutputExtension;
  end;

  ShowDirs;
end;

procedure TfFiles.Write;
begin
  with GetRegSettings do
  begin
    LogLevel := TLogLevel(rgLogLevel.ItemIndex);
    LogPlace := TLogPlace(rgLogDir.ItemIndex);
    SpecifiedDirectory := fsSpecifiedDirectory;
    ViewLogAfterRun := cbViewLog.Checked;
    LogTime  := cbLogTime.Checked;
  end;

  with FormatSettings.FileSettings do
  begin
    BackupExtension := edtBackupExt.Text;
    OutputExtension := edtOutputExt.Text;
  end;
end;

procedure TfFiles.btnViewLogClick(Sender: TObject);
begin
  GetRegSettings.ViewLog;
end;

procedure TfFiles.FrameResize(Sender: TObject);
begin
  inherited;
  rgLogDir.Width := ClientWidth - (rgLogDir.Left + GUI_PAD);
end;

end.