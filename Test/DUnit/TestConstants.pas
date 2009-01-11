unit TestConstants;

{ constants used in tests }


{  the directories where the test files are found}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestConstants, released May 2003.
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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface


{ get the directory for the test files, relative to the exe dir, not hardcoded }
function GetBaseDir: string;
function GetExeFilesDir: string;
function GetTestFilesDir: string;
function GetObsOutFilesDir: string;
function GetRefOutFilesDir: string;

function GetTestSettingsFileName: string;

procedure InitTestSettings;

implementation

uses
  { delphi }
  SysUtils,
  Windows,
  { local }
  JcfStringUtils,
  JcfRegistrySettings, JcfSettings, ConvertTypes;

var
  msEXEFilesDir: string = '';
  msBaseDir:     string = '';

procedure GenerateDirs;
const
  OUTPUT_DIR: string      = '\Output\';
  OUTPUT_DIR_LEN: integer = 8;
var
  outputIndex: integer;
begin
  // calculate this once, read the app path
  msEXEFilesDir := ExtractFilePath(ParamStr(0));

  msBaseDir := msEXEFilesDir;
  { expect this to be, in my e.g. "C:\Code\JcfCheckout\CodeFormat\Jcf2\Output\"
    The base dir strips off the /output
  }
  outputIndex := Pos(OUTPUT_DIR, msBaseDir);
  if outputIndex > 0 then
  begin
    msBaseDir := StrLeft(msBaseDir, outputIndex);
  end;
end;

function GetBaseDir: string;
begin
  if msBaseDir = '' then
    GenerateDirs;

  Result := msBaseDir;
end;

function GetExeFilesDir: string;
begin
  if msEXEFilesDir = '' then
    GenerateDirs;

  Result := msEXEFilesDir;
end;

function GetTestFilesDir: string;
begin
  Result := GetBaseDir + 'Test\TestCases\';
end;

function GetObsOutFilesDir: string;
begin
  Result := GetTestFilesDir + 'ObfuscatedOut\';
end;

function GetRefOutFilesDir: string;
begin
  Result := GetTestFilesDir + 'Out\';
end;

function GetTestSettingsFileName: string;
begin
  Result := GetTestFilesDir + 'JCFTestSettings.cfg';
end;

procedure InitTestSettings;
var
  lsSettingsFileName: string;
begin
  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  { use clarify test settings }
  lsSettingsFileName := GetTestSettingsFileName;
  if not (FileExists(lsSettingsFileName)) then
    raise Exception.Create('Settings file ' + lsSettingsFileName + ' not found');

  GetRegSettings.FormatConfigFileName := lsSettingsFileName;
  FormatSettings; // create and read
  FormatSettings.Obfuscate.Enabled := False;

  { some registry settings can be painfull in automated tests }
  GetRegSettings.LogTime := False;
  GetRegSettings.ViewLogAfterRun := False;
  GetRegSettings.ShowParseTreeOption := eShowNever;
end;

end.
