unit TestSelfParse;


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestSelfParse, released August 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2003 Anthony Steele.
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

{
  AFS 24 August 2004
  test parsing own source }

uses
  TestFrameWork;

type
  TTestSelfParse = class(TTestCase)
  published
    procedure TestSelfParse;
  end;

implementation

uses
  { delphi }
  SysUtils, Windows, ShellApi, Forms,
  { jcl }
  JclSysInfo, JclFileUtils, JclStrings,
  { local }
  ConvertTypes, FileConverter, TestConstants,
  JcfSettings, JcfRegistrySettings;

function CopyDirs(const psSource, psDest: string): boolean;
var
  rFiles: TSHFileOpStruct;
begin
  FillChar(rFiles, Sizeof(rFiles), #0);

  rFiles.Wnd := Application.Handle;
  rFiles.wFunc := FO_COPY;
  rFiles.pFrom := PChar(psSource +  #0#0);
  rFiles.pTo := PChar(psDest + PathSeparator + #0#0);
  rFiles.lpszProgressTitle := 'Copying directory';
  rFiles.hNameMappings := nil;
  rFiles.fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR;

  Result := (SHFileOperation(rFiles) = 0);
end;

function GetParentDir(const ps: string): string;
var
  liPos: integer;
begin
  Result := ps;
  if StrRight(Result, 1) = PathSeparator then
    Result := StrChopRight(Result, 1);

  liPos := StrLastPos(PathSeparator, Result);
  if liPos > 0 then
    Result := StrLeft(Result, liPos);
end;

procedure TTestSelfParse.TestSelfParse;
var
  lsTempDir, lsSource, lsSettingsFileName: string;
  lcConverter: TFileConverter;
begin
  lsTempDir := GetWindowsTempFolder + '\jcfParseTest';

  lsSource := PathExtractFileDirFixed(Application.ExeName);
  // go one dir up
  lsSource := GetParentDir(lsSource);
  // this does not recurse
  lsSource :=  lsSource + '*.pas';

  // remove existing dir
  if DirectoryExists(lsTempDir) then
    DelTree(lsTempDir);

  // copy the files
  Check(CopyDirs(lsSource, lsTempDir));
  Check(DirectoryExists(lsTempDir));

  // convert the copy
  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll := True;
    lcConverter.GuiMessages := False;

    { see also TestFullClarify }
    lsSettingsFileName := GetTestFilesDir + 'JCFTestSettings.cfg';
    Check(FileExists(lsSettingsFileName), 'Settings file ' + lsSettingsFileName + ' not found');

    GetRegSettings.FormatConfigFileName := lsSettingsFileName;
    lcConverter.ShowParseTreeOption := eShowNever;

    FormatSettings;
    FormatSettings.Obfuscate.Enabled := False;

    lcConverter.SourceMode := fmDirectoryRecursive;
    lcConverter.BackupMode := cmSeperateOutput;
    GetRegSettings.OutputExtension := 'out';

    lcConverter.Input := lsTempDir;
    lcConverter.Convert;
  finally
    lcConverter.Free;
  end;

  DelTree(lsTempDir);
end;

initialization
 TestFramework.RegisterTest(TTestSelfParse.Suite);
end.
