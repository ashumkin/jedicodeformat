unit TestObfuscate;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestObfuscate, released May 2003.
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
 TestFrameWork;

type
  TTestObfuscate = class(TTestCase)
  private
    procedure TestObfuscateFile(const psInFileName,
      psRefObsOutput, psRefClearOutput: string); overload;
    procedure TestObfuscateFile(const psName: string); overload;

    procedure TestFileContentsSame(const psFileName1, psFileName2: string);

  protected
    procedure Setup; override;

 published

    procedure TestObfuscate_Empty1;
    procedure TestObfuscate_fFormTest;
    procedure TestObfuscate_LittleTest1;
    procedure TestObfuscate_LittleTest2;
    procedure TestObfuscate_LittleTest3;
    procedure TestObfuscate_LittleTest4;
    procedure TestObfuscate_LittleTest5;
    procedure TestObfuscate_LittleTest6;
    procedure TestObfuscate_LittleTest7;
    procedure TestObfuscate_LittleTest8;
    procedure TestObfuscate_LittleTest9;
    procedure TestObfuscate_LittleTest10;

    procedure TestObfuscate_TestAbsolute;
    procedure TestObfuscate_TestAlign;
    procedure TestObfuscate_TestArray;
    procedure TestObfuscate_TestAsm;
    procedure TestObfuscate_TestBlankLineRemoval;
    procedure TestObfuscate_TestBogusDirectives;
    procedure TestObfuscate_TestBogusTypes;
    procedure TestObfuscate_TestCaseBlock;
    procedure TestObfuscate_TestCast;
    procedure TestObfuscate_TestCastSimple;
    procedure TestObfuscate_TestCharLiterals;
    procedure TestObfuscate_TestClassLines;
    procedure TestObfuscate_TestCommentIndent;
    procedure TestObfuscate_TestConstRecords;
    procedure TestObfuscate_TestD6;
    procedure TestObfuscate_TestDeclarations;
    procedure TestObfuscate_TestDeclarations2;
    procedure TestObfuscate_TestDefaultParams;
    procedure TestObfuscate_TestDeref;
    procedure TestObfuscate_TestEmptyClass;
    procedure TestObfuscate_TestEsotericKeywords;
    procedure TestObfuscate_TestExclusion;
    procedure TestObfuscate_TestExclusionFlags;
    procedure TestObfuscate_TestExternal;
    procedure TestObfuscate_TestForward;
    procedure TestObfuscate_TestGoto;
    procedure TestObfuscate_TestInitFinal;
    procedure TestObfuscate_TestInterfaceImplements;
    procedure TestObfuscate_TestInterfaceMap;
    procedure TestObfuscate_TestInterfaces;
    procedure TestObfuscate_TestLayout;
    procedure TestObfuscate_TestLayoutBare;
    procedure TestObfuscate_TestLayoutBare2;
    procedure TestObfuscate_TestLayoutBare3;
    procedure TestObfuscate_TestLibExports;
    procedure TestObfuscate_TestLineBreaking;
    procedure TestObfuscate_TestLocalTypes;
    procedure TestObfuscate_TestLongStrings;
    procedure TestObfuscate_TestMarcoV;
    procedure TestObfuscate_TestMessages;
    procedure TestObfuscate_TestMixedModeCaps;
    procedure TestObfuscate_TestMVB;
    procedure TestObfuscate_TestNested;
    procedure TestObfuscate_TestNestedRecords;
    procedure TestObfuscate_TestOperators;
    procedure TestObfuscate_TestParams;
    procedure TestObfuscate_TestParamSpaces;
    procedure TestObfuscate_TestPointers;
    procedure TestObfuscate_TestProgram;
    procedure TestObfuscate_TestProperties;
    procedure TestObfuscate_TestPropertyLines;
    procedure TestObfuscate_TestPropertyInherited;
    procedure TestObfuscate_TestRecords;
    procedure TestObfuscate_TestReg;
    procedure TestObfuscate_TestReint;
    procedure TestObfuscate_TestReturnRemoval;
    procedure TestObfuscate_TestReturns;
    procedure TestObfuscate_TestRunOnConst;
    procedure TestObfuscate_TestRunOnDef;
    procedure TestObfuscate_TestRunOnLine;
    procedure TestObfuscate_TestTestMH;
    procedure TestObfuscate_TestTPObjects;
    procedure TestObfuscate_TestTry;
    procedure TestObfuscate_TestTypeDefs;
    procedure TestObfuscate_TestUses;
    procedure TestObfuscate_TestUsesChanges;
    procedure TestObfuscate_TestVarParam;
    procedure TestObfuscate_TestWarnings;
    procedure TestObfuscate_TestWith;

   procedure TestObfuscate_TestCases;
   procedure TestObfuscate_TestProcBlankLines;
end;

implementation

uses
  { delphi } SysUtils,
  JclStrings,
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;

procedure TTestObfuscate.Setup;
begin
  inherited;
  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;
end;

procedure TTestObfuscate.TestObfuscateFile(const psInFileName,
  psRefObsOutput, psRefClearOutput: string);
var
  lcConverter: TFileConverter;
  lsObsFileName: string;
  lsOutFileName: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  FormatSettings.Obfuscate.Enabled := True;
  GetRegSettings.OutputExtension := 'obs';

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll := True;
    lcConverter.GuiMessages := False;

    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeperateOutput;

    FormatSettings.Obfuscate.Enabled := True;

    lcConverter.Input := psInFileName;


    lcConverter.Convert;

    Check(not lcConverter.ConvertError, 'Obfuscate failed for ' +
      ExtractFileName(psInFileName) +
      ' : ' + lcConverter.ConvertErrorMessage);

    lsObsFileName := lcConverter.OutFileName;
    Check(lsObsFileName <> '', 'No obfuscated file');
    Check(FileExists(lsObsFileName), 'obfuscated file ' + lsObsFileName + ' not found');

    TestFileContentsSame(lsObsFileName, psRefObsOutput);

    // now deobfuscate
    FormatSettings.Obfuscate.Enabled := False;
    GetRegSettings.OutputExtension := 'out';

    lcConverter.Clear;
    lcConverter.YesAll := True;
    lcConverter.GuiMessages := False;
    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeperateOutput;

    lcConverter.Input := lsObsFileName;
    lcConverter.Convert;

    Check(not lcConverter.ConvertError, 'Reclarify failed for ' +
      ExtractFileName(lsObsFileName) +
      ' : ' + lcConverter.ConvertErrorMessage);

    lsOutFileName := lcConverter.OutFileName;
    Check(lsOutFileName <> '', 'No output file');
    Check(FileExists(lsOutFileName), 'output file ' + lsObsFileName + ' not found');

    TestFileContentsSame(lsOutFileName, psRefClearOutput);

    // clean up
    DeleteFile(lsOutFileName);
    DeleteFile(lsObsFileName);
  finally
    lcConverter.Free;
    FormatSettings.Obfuscate.Enabled := False;
  end;

end;

procedure TTestObfuscate.TestFileContentsSame(const psFileName1,
  psFileName2: string);
var
  lsFile1, lsFile2: string;
begin
  Check(FileExists(psFileName1), 'File ' + psFileName1 + ' does not exist');
  Check(FileExists(psFileName2), 'File ' + psFileName2 + ' does not exist');

  lsFile1 := FileToString(psFileName1);
  lsFile2 := FileToString(psFileName2);

  // check contents the same 
  if (lsFile1 <> lsFile2) then
    Fail('Files differ ' + psFileName1 + ' and ' + psFileName2);
end;

procedure TTestObfuscate.TestObfuscateFile(const psName: string);
var
  lsInName, lsObsFileName: string;
  lsRemadeFileName: string;
begin
  Assert(psName <> '');

  { does it have an file extension? }
  if Pos('.', psName) > 0 then
  begin
    lsInName := psName;
    lsObsFileName := StrBefore('.', psName) + '.obs';
    lsRemadeFileName := StrBefore('.', psName) + '.out';
  end
  else
  begin
    lsInName := psName + '.pas';
    lsObsFileName := psName + '.obs';
    lsRemadeFileName := psName + '.out';
  end;

  GetRegSettings.OutputExtension := 'obs';

  TestObfuscateFile(GetTestFilesDir + lsInName,
    GetObsOutFilesDir + lsObsFileName, GetObsOutFilesDir + lsRemadeFileName)

  {
    // test re-obfuscating 

  FormatSettings.FileSettings.OutputExtension := 'out';

  TestObfuscateFile(GetTestFilesDir + lsObsFileName,
    GetObsOutFilesDir + lsRemadeFileName)
  }
end;

procedure TTestObfuscate.TestObfuscate_Empty1;
begin
  TestObfuscateFile('EmptyTest1');
end;

procedure TTestObfuscate.TestObfuscate_fFormTest;
begin
  TestObfuscateFile('fFormTest');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest1;
begin
  TestObfuscateFile('LittleTest1');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest2;
begin
  TestObfuscateFile('LittleTest2');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest3;
begin
  TestObfuscateFile('LittleTest3');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest4;
begin
  TestObfuscateFile('LittleTest4');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest5;
begin
  TestObfuscateFile('LittleTest5');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest6;
begin
  TestObfuscateFile('LittleTest6');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest7;
begin
  TestObfuscateFile('LittleTest7');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest8;
begin
  TestObfuscateFile('LittleTest8');
end;

procedure TTestObfuscate.TestObfuscate_TestAbsolute;
begin
  TestObfuscateFile('TestAbsolute');
end;

procedure TTestObfuscate.TestObfuscate_TestAlign;
begin
  TestObfuscateFile('TestAlign');
end;


procedure TTestObfuscate.TestObfuscate_TestArray;
begin
  TestObfuscateFile('TestArray');
end;

procedure TTestObfuscate.TestObfuscate_TestAsm;
begin
  TestObfuscateFile('TestAsm');
end;

procedure TTestObfuscate.TestObfuscate_TestBlankLineRemoval;
begin
  TestObfuscateFile('TestBlankLineRemoval');
end;

procedure TTestObfuscate.TestObfuscate_TestBogusDirectives;
begin
  TestObfuscateFile('TestBogusDirectives');
end;

procedure TTestObfuscate.TestObfuscate_TestBogusTypes;
begin
  TestObfuscateFile('TestBogusTypes');
end;

procedure TTestObfuscate.TestObfuscate_TestCaseBlock;
begin
  TestObfuscateFile('TestCaseBlock');
end;

procedure TTestObfuscate.TestObfuscate_TestCast;
begin
  TestObfuscateFile('TestCast');
end;

procedure TTestObfuscate.TestObfuscate_TestCastSimple;
begin
  TestObfuscateFile('TestCastSimple');
end;

procedure TTestObfuscate.TestObfuscate_TestCharLiterals;
begin
  TestObfuscateFile('TestCharLiterals');
end;

procedure TTestObfuscate.TestObfuscate_TestClassLines;
begin
  TestObfuscateFile('TestClassLines');
end;

procedure TTestObfuscate.TestObfuscate_TestCommentIndent;
begin
  TestObfuscateFile('TestCommentIndent');
end;

procedure TTestObfuscate.TestObfuscate_TestConstRecords;
begin
  TestObfuscateFile('TestConstRecords');
end;

procedure TTestObfuscate.TestObfuscate_TestD6;
begin
  TestObfuscateFile('TestD6');
end;

procedure TTestObfuscate.TestObfuscate_TestDeclarations;
begin
  TestObfuscateFile('TestDeclarations');
end;


procedure TTestObfuscate.TestObfuscate_TestDeclarations2;
begin
  TestObfuscateFile('TestDeclarations2');
end;

procedure TTestObfuscate.TestObfuscate_TestDefaultParams;
begin
  TestObfuscateFile('TestDefaultParams');
end;

procedure TTestObfuscate.TestObfuscate_TestEmptyClass;
begin
  TestObfuscateFile('TestEmptyClass');
end;

procedure TTestObfuscate.TestObfuscate_TestEsotericKeywords;
begin
  TestObfuscateFile('TestEsotericKeywords');
end;

procedure TTestObfuscate.TestObfuscate_TestExclusion;
begin
  TestObfuscateFile('TestExclusion');
end;

procedure TTestObfuscate.TestObfuscate_TestExclusionFlags;
begin
  TestObfuscateFile('TestExclusionFlags');
end;

procedure TTestObfuscate.TestObfuscate_TestExternal;
begin
  TestObfuscateFile('TestExternal');
end;

procedure TTestObfuscate.TestObfuscate_TestForward;
begin
  TestObfuscateFile('TestForward');
end;

procedure TTestObfuscate.TestObfuscate_TestGoto;
begin
  TestObfuscateFile('TestGoto');
end;

procedure TTestObfuscate.TestObfuscate_TestInitFinal;
begin
  TestObfuscateFile('TestInitFinal');
end;

procedure TTestObfuscate.TestObfuscate_TestInterfaceImplements;
begin
  TestObfuscateFile('TestInterfaceImplements');
end;

procedure TTestObfuscate.TestObfuscate_TestInterfaceMap;
begin
  TestObfuscateFile('TestInterfaceMap');
end;

procedure TTestObfuscate.TestObfuscate_TestInterfaces;
begin
  TestObfuscateFile('TestInterfaces');
end;

procedure TTestObfuscate.TestObfuscate_TestLayout;
begin
  TestObfuscateFile('TestLayout');
end;

procedure TTestObfuscate.TestObfuscate_TestLayoutBare;
begin
  TestObfuscateFile('TestLayoutBare');
end;

procedure TTestObfuscate.TestObfuscate_TestLayoutBare2;
begin
  TestObfuscateFile('TestLayoutBare2');
end;

procedure TTestObfuscate.TestObfuscate_TestLayoutBare3;
begin
  TestObfuscateFile('TestLayoutBare3');
end;

procedure TTestObfuscate.TestObfuscate_TestLibExports;
begin
  TestObfuscateFile('TestLibExports');
end;

procedure TTestObfuscate.TestObfuscate_TestLineBreaking;
begin
  TestObfuscateFile('TestLineBreaking');
end;

procedure TTestObfuscate.TestObfuscate_TestLocalTypes;
begin
  TestObfuscateFile('TestLocalTypes');
end;

procedure TTestObfuscate.TestObfuscate_TestLongStrings;
begin
  TestObfuscateFile('TestLongStrings');
end;

procedure TTestObfuscate.TestObfuscate_TestMarcoV;
begin
  TestObfuscateFile('TestMarcoV');
end;

procedure TTestObfuscate.TestObfuscate_TestTestMH;
begin
  TestObfuscateFile('TestMH');
end;

procedure TTestObfuscate.TestObfuscate_TestMixedModeCaps;
begin
  TestObfuscateFile('TestMixedModeCaps');
end;

procedure TTestObfuscate.TestObfuscate_TestMVB;
begin
  TestObfuscateFile('TestMVB');
end;

procedure TTestObfuscate.TestObfuscate_TestNested;
begin
  TestObfuscateFile('TestNested');
end;

procedure TTestObfuscate.TestObfuscate_TestNestedRecords;
begin
  TestObfuscateFile('TestNestedRecords');
end;

procedure TTestObfuscate.TestObfuscate_TestOperators;
begin
  TestObfuscateFile('TestOperators');
end;

procedure TTestObfuscate.TestObfuscate_TestParams;
begin
  TestObfuscateFile('TestParams');
end;

procedure TTestObfuscate.TestObfuscate_TestParamSpaces;
begin
  TestObfuscateFile('TestParamSpaces');
end;

procedure TTestObfuscate.TestObfuscate_TestPointers;
begin
  TestObfuscateFile('TestPointers');
end;

procedure TTestObfuscate.TestObfuscate_TestProgram;
begin
  TestObfuscateFile('TestProgram');
end;

procedure TTestObfuscate.TestObfuscate_TestProperties;
begin
  TestObfuscateFile('TestProperties');
end;

procedure TTestObfuscate.TestObfuscate_TestPropertyLines;
begin
  TestObfuscateFile('TestPropertyLines');
end;

procedure TTestObfuscate.TestObfuscate_TestRecords;
begin
  TestObfuscateFile('TestRecords');
end;

procedure TTestObfuscate.TestObfuscate_TestReg;
begin
  TestObfuscateFile('TestReg');
end;

procedure TTestObfuscate.TestObfuscate_TestReint;
begin
  TestObfuscateFile('TestReint');
end;

procedure TTestObfuscate.TestObfuscate_TestReturnRemoval;
begin
  TestObfuscateFile('TestReturnRemoval');
end;

procedure TTestObfuscate.TestObfuscate_TestReturns;
begin
  TestObfuscateFile('TestReturns');
end;

procedure TTestObfuscate.TestObfuscate_TestRunOnConst;
begin
  TestObfuscateFile('TestRunOnConst');
end;

procedure TTestObfuscate.TestObfuscate_TestRunOnDef;
begin
  TestObfuscateFile('TestRunOnDef');
end;

procedure TTestObfuscate.TestObfuscate_TestRunOnLine;
begin
  TestObfuscateFile('TestRunOnLine');
end;

procedure TTestObfuscate.TestObfuscate_TestTPObjects;
begin
  TestObfuscateFile('TestTPObjects');
end;

procedure TTestObfuscate.TestObfuscate_TestTry;
begin
  TestObfuscateFile('TestTry');
end;

procedure TTestObfuscate.TestObfuscate_TestTypeDefs;
begin
  TestObfuscateFile('TestTypeDefs');
end;

procedure TTestObfuscate.TestObfuscate_TestUses;
begin
  TestObfuscateFile('TestUses');
end;

procedure TTestObfuscate.TestObfuscate_TestUsesChanges;
begin
  TestObfuscateFile('TestUsesChanges');
end;

procedure TTestObfuscate.TestObfuscate_TestVarParam;
begin
  TestObfuscateFile('TestVarParam');
end;

procedure TTestObfuscate.TestObfuscate_TestWarnings;
begin
  TestObfuscateFile('TestWarnings');
end;

procedure TTestObfuscate.TestObfuscate_TestWith;
begin
  TestObfuscateFile('TestWith');
end;

procedure TTestObfuscate.TestObfuscate_TestCases;
begin
  TestObfuscateFile('Testcases.dpr');
end;

procedure TTestObfuscate.TestObfuscate_TestProcBlankLines;
begin
  TestObfuscateFile('TestProcBlankLines.pas');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest9;
begin
  TestObfuscateFile('LittleTest9');
end;

procedure TTestObfuscate.TestObfuscate_TestDeref;
begin
  TestObfuscateFile('TestDeref');
end;

procedure TTestObfuscate.TestObfuscate_TestPropertyInherited;
begin
  TestObfuscateFile('TestPropertyInherited');
end;

procedure TTestObfuscate.TestObfuscate_TestMessages;
begin
  TestObfuscateFile('TestMessages');
end;

procedure TTestObfuscate.TestObfuscate_LittleTest10;
begin
  TestObfuscateFile('LittleTest10');
end;

initialization
 TestFramework.RegisterTest(TTestObfuscate.Suite);
end.