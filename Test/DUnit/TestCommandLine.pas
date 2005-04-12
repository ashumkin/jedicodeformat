unit TestCommandLine;

{ AFS 26 April 03
  test the jcf.exe commandline }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestCommandLine, released May 2003.
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
  Classes,
  { jcf }
  TestFile, TestConstants;

type
  TTestCommandline = class(TTestFile)
  private
    fsJcfParams: string;
    fsRefDir: string;
    fsFileMask: string;
    fsOutputExt: string;

    fsFileNames: TStringList;

    procedure CompareFileToRef(const psFileName: string);

    procedure RunJcfCommandline;
    procedure GetOutFiles;

  protected
    procedure SetUp; override;
    procedure TearDown; override;


  published
    procedure TestFormatClarify;
    procedure TestFormatObfuscate;

  end;

implementation

uses
  { delphi }
  SysUtils,
  { jcl }
  JclShell, JclFileUtils, JcfRegistrySettings,
  TestFramework;

const
  EXPECTED_FILE_COUNT = 169;

procedure TTestCommandline.SetUp;
begin
  fsFileNames := TStringList.Create;

  InitTestSettings;
  GetRegSettings.WriteAll;
end;

procedure TTestCommandline.TearDown;
begin
  FreeAndNil(fsFileNames);
end;


procedure TTestCommandline.GetOutFiles;
begin
  fsFileNames.Clear;
  BuildFileList(GetTestFilesDir + fsFileMask, faAnyFile, fsFileNames);
end;

procedure TTestCommandline.CompareFileToRef(const psFileName: string);
begin
  TestFileContentsSame(GetTestFilesDir + psFileName, fsRefDir + psFileName);
  DeleteFile(GetTestFilesDir + psFileName);
end;

procedure TTestCommandline.RunJcfCommandline;
var
  lsJcfExe:  string;
  liLoop:    integer;
  lbRes:     boolean;
var
  lsSaveExt: string;
begin
  lsSaveExt := GetRegSettings.OutputExtension;

  try
    GetRegSettings.ReadAll;
    GetRegSettings.OutputExtension := fsOutputExt;
    GetRegSettings.WriteAll;

    Assert(fsJcfParams <> '');
    Assert(fsRefDir <> '');
    Assert(fsFileMask <> '');

    // delete the output files
    GetOutFiles;

    if fsFileNames.Count > 0 then
    begin
      for liLoop := 0 to fsFileNames.Count - 1 do
        DeleteFile(GetTestFilesDir + fsFileNames[liLoop]);

      // should be none left
      GetOutFiles;
    end;

    CheckEquals(0, fsFileNames.Count, 'file could not be deleted');


    // build them again
    lsJcfExe := GetExeFilesDir + 'jcf.exe';
    Check(FileExists(lsJcfExe), 'could not find program ' + lsJcfExe);

    lbRes := ShellExecAndWait('"' + lsJcfExe + '"', fsJcfParams);
    Check(lbRes, 'program execution failed');

    // should be back
    GetOutFiles;
    CheckEquals(EXPECTED_FILE_COUNT, fsFileNames.Count);

    // for each, compare to the reference versions
    GetOutFiles;

    for liLoop := 0 to fsFileNames.Count - 1 do
      CompareFileToRef(fsFileNames[liLoop]);

  finally
    GetRegSettings.OutputExtension := lsSaveExt;
    GetRegSettings.WriteAll;
  end;
end;



procedure TTestCommandline.TestFormatClarify;
begin
  fsOutputExt := 'out';
  fsJcfParams := ' -config="' + GetTestSettingsFileName +
    '" -out -D "' + GetTestFilesDir + '"';
  fsRefDir    := GetRefOutFilesDir;
  fsFileMask  := '*.out';

  RunJcfCommandline;
end;

procedure TTestCommandline.TestFormatObfuscate;
begin
  fsOutputExt := 'obs';
  fsJcfParams := ' -obfuscate -config="' + GetTestFilesDir +
    'JCFObfuscateSettings.cfg" ' +
    '-out -D "' + GetTestFilesDir + '"';
  fsRefDir    := GetObsOutFilesDir;
  fsFileMask  := '*.obs';

  RunJcfCommandline;
end;

initialization
  TestFramework.RegisterTest(TTestCommandline.Suite);
end.
