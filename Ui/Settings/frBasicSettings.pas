unit frBasicSettings;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frBasicSettings.pas, released April 2000.
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls,
  { local }
  ConvertTypes, frmBaseSettingsFrame;

type
  TfrBasic = class(TfrSettingsFrame)
    rgFileRecurse: TRadioGroup;
    rgBackup: TRadioGroup;
    edtInput: TEdit;
    edtOutput: TEdit;
    lblOutput: TLabel;
    lblInput: TLabel;
    sbOpen: TSpeedButton;
    rgMode: TRadioGroup;
    dlgOpen: TOpenDialog;
    procedure rgFileRecurseClick(Sender: TObject);
    procedure rgBackupClick(Sender: TObject);
    procedure sbOpenClick(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure edtInputDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure edtInputDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure edtInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    procedure DisplayOutputFile;
    function GetCurrentBackupMode: TBackupMode;
  protected

    procedure DragItemDropped(const piFormat: integer; const psItem: string); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

    function GetCurrentSourceMode: TSourceMode;
    function GetGoHint: string;
  end;

implementation

uses
 { delphi }
 { jcl } jclFileUtils,
 { local } FileUtils, frUses, JcfHelp;

{$R *.DFM}

constructor TfrBasic.Create(AOwner: TComponent);
var
  lbShowFileName: boolean;
begin
  inherited;
  IsDropActive := True;

  { the filename setting etc. is not relevant to the IDE pluggin }

  {$IFDEF IDEPLUGGIN}
  lbShowFileName := False;
  {$ELSE}
  lbShowFileName := True;
  {$ENDIF}
                                 
  lblInput.Visible  := lbShowFileName;
  lblOutput.Visible := lbShowFileName;
  edtInput.Visible  := lbShowFileName;
  edtOutput.Visible := lbShowFileName;
  sbOpen.Visible    := lbShowFileName;

  rgFileRecurse.Visible := lbShowFileName;
  rgBackup.Visible      := lbShowFileName;

  fiHelpContext := HELP_BASIC_SETTINGS;
end;

procedure TfrBasic.DragItemDropped(const piFormat: integer; const psItem: string);
begin
  // can only be from the input edit box
  edtInput.Text := psItem;
  DisplayOutputFile;
end;

function TfrBasic.GetCurrentBackupMode: TBackupMode;
begin
  Result := TBackupMode(rgBackup.ItemIndex);
end;

function TfrBasic.GetCurrentSourceMode: TSourceMode;
begin
  Result := TSourceMode(rgFileRecurse.ItemIndex);
end;


procedure TfrBasic.DisplayOutputFile;
var
  bShowOutput: boolean;
begin
  case GetCurrentBackupMode of
    cmInPlace:
      lblOutput.Caption := '';
    cmInPlaceWithBackup:
      lblOutput.Caption := 'Backup file';
    cmSeperateOutput:
      lblOutput.Caption := 'Output file';
    else
      raise Exception.Create('TfrmMain.DisplayOutputFile: bad backup group index');
  end;

  {$IFNDEF IDEPLUGGIN}
  if Settings = nil then
    edtOutput.Text := ''
  else
    edtOutput.Text :=
      Settings.FileSettings.GetOutputFileName(edtInput.Text, GetCurrentBackupMode);

  bShowOutput := (GetCurrentBackupMode <> cmInplace) and
    (GetCurrentSourceMode = fmSingleFIle);

  lblOutput.Visible := bShowOutput;
  edtOutput.Visible := bShowOutput;
  {$ENDIF}
end;

procedure TfrBasic.rgModeClick(Sender: TObject);
begin
  CallOnChange;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfrBasic.rgFileRecurseClick(Sender: TObject);
begin
  inherited;
  case GetCurrentSourceMode of
    fmSingleFile:
      lblInput.Caption := 'Input file';
    fmDirectory:
    begin
      lblInput.Caption := 'Directory';
      edtInput.Text    := PathAddSeparator(ExtractFileDir(edtInput.Text));
    end;
    fmDirectoryRecursive:
    begin
      lblInput.Caption := 'Start directory';
      edtInput.Text    := PathAddSeparator(ExtractFileDir(edtInput.Text));
    end;
  end;

  DisplayOutputFile;
  CallOnChange;
end;

procedure TfrBasic.rgBackupClick(Sender: TObject);
begin
  DisplayOutputFile;
  CallOnChange;
end;

procedure TfrBasic.sbOpenClick(Sender: TObject);
var
  lsDir: string;
begin
  lsDir := PathAddSeparator(ExtractFileDir(edtInput.Text)); // strip out the dir

  dlgOpen.InitialDir := lsDir;

  if GetCurrentSourceMode = fmSingleFile then
  begin
    if dlgOpen.Execute then
      edtInput.Text := dlgOpen.FileName;

    DisplayOutputFile;
  end
  else
  begin
    if SelectDirectory('select a directory', '', lsDir) then
      edtInput.Text := PathAddSeparator(lsDir);
  end;
end;

procedure TfrBasic.Read;
begin
  Assert(Settings <> nil);
  with Settings.FileSettings do
  begin
    rgFileRecurse.ItemIndex := Ord(SourceMode);
    rgBackup.ItemIndex := Ord(BackupMode);
    edtInput.Text := Input;
  end;

  if Settings.Obfuscate.Enabled then
    rgMode.ItemIndex := 1
  else
    rgMode.ItemIndex := 0;

  DisplayOutputFile;
end;

procedure TfrBasic.Write;
begin
  Assert(Settings <> nil);

  with Settings.FileSettings do
  begin
    SourceMode := GetCurrentSourceMode;
    BackupMode := GetCurrentBackupMode;
    Input      := edtInput.Text;
  end;

  Settings.Obfuscate.Enabled := (rgMode.ItemIndex <> 0);
end;

function TfrBasic.GetGoHint: string;
begin
  if rgMode.ItemIndex = 0 then
    Result := 'Format'
  else
    Result := 'Obfuscate';

  case GetCurrentSourceMode of
    fmSingleFile:
      Result := Result + ' file';
    fmDirectory:
      Result := Result + ' directory';
    fmDirectoryRecursive:
      Result := Result + ' directory heirarchy';
  end;

  case GetCurrentBackupMode of
    cmInPlace:
      Result := Result + ' in place';
    cmInPlaceWithBackup:
      Result := Result + ' with backup';
    cmSeperateOutput:
      Result := Result + ' to output';
  end;
end;

procedure TfrBasic.edtInputDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

procedure TfrBasic.edtInputDragDrop(Sender, Source: TObject; X, Y: integer);
begin
  HandleShellDragDrop(Source);
end;

procedure TfrBasic.edtInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  DisplayOutputFile;
end;

end.
