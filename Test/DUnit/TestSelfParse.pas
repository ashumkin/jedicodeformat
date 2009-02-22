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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{
  AFS 24 August 2004
  test parsing own source

  ie all files under the base directory, or it's subdirs, to depth 2 }

uses
  TestFrameWork;

type
  TTestSelfParse = class(TTestCase)
  private
    procedure TestParseFile(const psInFileName: string);
    procedure TestParseDir(const psDir: string; const piDepth: integer);
  published
    procedure TestSelfParse;
  end;

implementation

uses
  { delphi }
  SysUtils, Windows, Classes,
  { jcl }
  JclFileUtils,
  { local }
  ConvertTypes, FileConverter, TestConstants,
  JcfSettings, JcfRegistrySettings;

const
  MAX_DIR_DEPTH = 2;

procedure TTestSelfParse.TestParseDir(const psDir: string; const piDepth: integer);
var
  liLoop:    integer;
  lcFiles:   TStringList;
  lcSubdirs: TStringList;
begin
  { test parse all the .pas, .dpr, .dpk files in this dir }
  lcFiles := TStringList.Create;
  try
    // get files
    BuildFileList(psDir + '*.pas', 0, lcFiles);
    BuildFileList(psDir + '*.dpr', 0, lcFiles);
    BuildFileList(psDir + '*.dpk', 0, lcFiles);

    // parse them
    for liLoop := 0 to lcFiles.Count - 1 do
      TestParseFile(psDir + lcFiles.Strings[liLoop]);

  finally
    lcFiles.Free;
  end;


  { go further down? }
  if piDepth > 0 then
  begin
    lcSubdirs := TStringList.Create;
    try
      // get dirs
      BuildFileList(psDir + '*.*', faDirectory, lcSubdirs);

      // recurse - parse them
      for liLoop := 0 to lcSubdirs.Count - 1 do
        TestParseDir(psDir + lcSubdirs.Strings[liLoop] + DirDelimiter, piDepth - 1);
    finally
      lcSubdirs.Free;
    end;
  end;
end;

procedure TTestSelfParse.TestParseFile(const psInFileName: string);
var
  lcConverter:   TFileConverter;
  lsOutFileName: string;
begin

  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll      := True;
    lcConverter.GuiMessages := False;

    // init and read the format settings
    GetRegSettings.FormatConfigFileName := GetTestSettingsFileName;
    FormatSettings.Obfuscate.Enabled    := False;

    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeparateOutput;
    GetRegSettings.OutputExtension := 'out';

    GetRegSettings.ShowParseTreeOption := eShowNever;
    lcConverter.Input := psInFileName;

    lcConverter.Convert;
    lsOutFileName := lcConverter.OutFileName;

    Check( not lcConverter.ConvertError, 'Convert failed for ' +
      ExtractFileName(psInFileName));

    lsOutFileName := lcConverter.OutFileName;
    Check(lsOutFileName <> '', 'No output file');
    Check(FileExists(lsOutFileName), 'output file ' + lsOutFileName + ' not found');

    // clean up
    if FileExists(lsOutFileName) then
      SysUtils.DeleteFile(lsOutFileName);
  finally
    lcConverter.Free;
  end;
end;

procedure TTestSelfParse.TestSelfParse;
begin
  TestParseDir(GetBaseDir, MAX_DIR_DEPTH);
end;

initialization
  TestFramework.RegisterTest(TTestSelfParse.Suite);
end.
