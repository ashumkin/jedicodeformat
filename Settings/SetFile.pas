{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetFile.pas, released April 2000.
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

unit SetFile;

{ settings to do with files
  AFS 29 Dec 1999
}

interface

uses
    { delphi  } classes,
    { local } JCFSetBase, TokenType, ConvertTypes, SettingsStream;

type

  TSetFile = class(TSetBase)
  private
    feBackupMode: TBackupMode;
    feSourceMode: TSourceMode;
    fsInput: string;

    fsBackupExtension, fsOutputExtension: string;

    fcExclusionsFiles: TStringList;
    fcExclusionsDirs: TStringList;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    function GetOutputFileName(const psIn: string): string; overload;
    function GetOutputFileName(const psIn: string; peMode: TBackupMode): string;
      overload;

    function Output: string;

    function FileIsExcluded(const psFile: string): boolean;
    function DirIsExcluded(const psDir: string): boolean;

    property BackupMode: TBackupMode read feBackupMode write feBackupMode;
    property SourceMode: TSourceMode read feSourceMode write feSourceMode;

    property Input: string read fsInput write fsInput;
    property BackupExtension: string read fsBackupExtension write fsBackupExtension;
    property OutputExtension: string read fsOutputExtension write fsOutputExtension;

    property ExclusionsFiles: TStringList read fcExclusionsFiles;
    property ExclusionsDirs: TStringList read fcExclusionsDirs;

  end;

implementation

uses
    { delphi } SysUtils,
    { jcl } JclStrings, JclFileUtils;

const
  REG_BACKUP_MODE = 'BackupMode';
  REG_SOURCE_MODE = 'SourceMode';
  REG_INPUT       = 'Input';
  REG_BACKUP_EXT  = 'BackupExt';
  REG_OUTPUT_EXT  = 'OutputExt';

  REG_EXCLUSIONS_FILES  = 'ExclusionsFiles';
  REG_EXCLUSIONS_DIRS   = 'ExclusionsDirs';

  { TSetFile }

constructor TSetFile.Create;
begin
  inherited;

  fcExclusionsFiles := TStringList.Create;
  fcExclusionsDirs := TStringList.Create;

  SetSection('File');
end;

destructor TSetFile.Destroy;
begin
  FreeAndNil(fcExclusionsFiles);
  FreeAndNil(fcExclusionsDirs);
  inherited;
end;

procedure TSetFile.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  feBackupMode := TBackupMode(pcStream.Read(REG_BACKUP_MODE, Ord(cmSeperateOutput)));
  feSourceMode := TSourceMode(pcStream.Read(REG_SOURCE_MODE, Ord(fmSingleFile)));
  fsInput      := pcStream.Read(REG_INPUT, '');

  fsBackupExtension := pcStream.Read(REG_BACKUP_EXT, 'bak');
  fsOutputExtension := pcStream.Read(REG_OUTPUT_EXT, 'out');

  pcStream.Read(REG_EXCLUSIONS_FILES, fcExclusionsFiles);
  pcStream.Read(REG_EXCLUSIONS_DIRS, fcExclusionsDirs);
end;

procedure TSetFile.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_BACKUP_MODE, Ord(feBackupMode));
  pcOut.Write(REG_SOURCE_MODE, Ord(feSourceMode));

  pcOut.Write(REG_INPUT, fsInput);
  pcOut.Write(REG_BACKUP_EXT, fsBackupExtension);
  pcOut.Write(REG_OUTPUT_EXT, fsOutputExtension);

  pcOut.Write(REG_EXCLUSIONS_FILES, fcExclusionsFiles);
  pcOut.Write(REG_EXCLUSIONS_DIRS, fcExclusionsDirs);
end;

{-------------------------------------------------------------------------------
  worker procs }

function TSetFile.Output: string;
begin
  Result := GetOutputFileName(Input);
end;

{ get an output file name based on the name passed in & the current file settings }
function TSetFile.GetOutputFileName(const psIn: string): string;
begin
  // use the currently selected mode
  Result := GetOutputFileName(psIn, BackupMode);
end;



function TSetFile.GetOutputFileName(const psIn: string;
  peMode: TBackupMode): string;
var
  lsExt: string;
  liMainFileNameLength: integer;
begin
  if PathExtractFileNameNoExt(psIn) = '' then
  begin
    Result := '';
    exit;
  end;

  if (peMode = cmInPlace) then
  begin
    Result := '';
  end
  else if peMode in [cmInPlaceWithBackup, cmSeperateOutput] then
  begin
    lsExt  := ExtractFileExt(psIn);
    liMainFileNameLength := Length(psIn) - Length(lsExt);
    Result := StrLeft(psIn, liMainFileNameLength);

    if peMode = cmInPlaceWithBackup then
      Result := Result + '.' + BackupExtension
    else
      Result := Result + '.' + OutputExtension;
  end
  else
    raise Exception.Create('TCodeFormatSettings.Output: bad backup mode ');
end;

function TSetFile.FileIsExcluded(const psFile: string): boolean;
begin
  { !!! check without paths & extensions ! }
  Result := (fcExclusionsFiles.IndexOf(psFile) >= 0);
end;

function TSetFile.DirIsExcluded(const psDir: string): boolean;
begin
  { !!! check without paths  }
  Result := (fcExclusionsDirs.IndexOf(psDir) >= 0);
end;

end.