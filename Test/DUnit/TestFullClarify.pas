unit TestFullClarify;

{ test the full clarify - all processes }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestFullClarify, released May 2003.
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
 TestFile;

type
  TFullTestClarify = class(TTestFile)
  private
    procedure TestClarifyFile(const psInFileName, psRefOutput: string); overload;
    procedure TestClarifyFile(const psName: string); overload;

  protected
    procedure Setup; override;

 published
    { one test for each file}
    procedure TestClarify_EmptyTest1;
    procedure TestClarify_fFormTest;
    procedure TestClarify_LittleTest1;
    procedure TestClarify_LittleTest2;
    procedure TestClarify_LittleTest3;
    procedure TestClarify_LittleTest4;
    procedure TestClarify_LittleTest5;
    procedure TestClarify_LittleTest6;
    procedure TestClarify_LittleTest7;
    procedure TestClarify_LittleTest8;
    procedure TestClarify_LittleTest9;
    procedure TestClarify_LittleTest10;
    procedure TestClarify_LittleTest11;
    procedure TestClarify_LittleTest12;
    procedure TestClarify_LittleTest13;
    procedure TestClarify_LittleTest14;
    procedure TestClarify_LittleTest15;
    procedure TestClarify_LittleTest16;
    procedure TestClarify_LittleTest17;
    procedure TestClarify_LittleTest18;

    procedure TestClarify_TestAbsolute;
    procedure TestClarify_TestAlign;
    procedure TestClarify_TestArray;
    procedure TestClarify_TestAsm;

    procedure TestClarify_TestBlankLineRemoval;
    procedure TestClarify_TestBogusDirectives;
    procedure TestClarify_TestBogusTypes;
    procedure TestClarify_TestCaseBlock;
    procedure TestClarify_TestCast;
    procedure TestClarify_TestCastSimple;
    procedure TestClarify_TestCharLiterals;
    procedure TestClarify_TestClassLines;
    procedure TestClarify_TestCommentIndent;
    procedure TestClarify_TestConstRecords;
    procedure TestClarify_TestD6;
    procedure TestClarify_TestDeclarations2;
    procedure TestClarify_TestDeclarations;
    procedure TestClarify_TestDefaultParams;
    procedure TestClarify_TestDeref;
    procedure TestClarify_TestEmptyClass;
    procedure TestClarify_TestEsotericKeywords;
    procedure TestClarify_TestExclusion;
    procedure TestClarify_TestExclusionFlags;
    procedure TestClarify_TestExternal;
    procedure TestClarify_TestForward;
    procedure TestClarify_TestGoto;
    procedure TestClarify_TestInheritedExpr;
    procedure TestClarify_TestInitFinal;
    procedure TestClarify_TestInterfaceImplements;
    procedure TestClarify_TestInterfaceMap;
    procedure TestClarify_TestInterfaces;
    procedure TestClarify_TestLayout;
    procedure TestClarify_TestLayoutBare2;
    procedure TestClarify_TestLayoutBare3;
    procedure TestClarify_TestLayoutBare;
    procedure TestClarify_TestLibExports;

    procedure TestClarify_TestLineBreaking;
    procedure TestClarify_TestLocalTypes;
    procedure TestClarify_TestLongStrings;
    procedure TestClarify_TestMarcoV;
    procedure TestClarify_TestMessages;
    procedure TestClarify_TestMH;
    procedure TestClarify_TestMixedModeCaps;
    procedure TestClarify_TestMVB;
    procedure TestClarify_TestNested;
    procedure TestClarify_TestNestedRecords;
    procedure TestClarify_TestOleParams;
    procedure TestClarify_TestOperators;
    procedure TestClarify_TestParams;
    procedure TestClarify_TestParamSpaces;
    procedure TestClarify_TestPointers;
    procedure TestClarify_TestProgram;
    procedure TestClarify_TestProperties;
    procedure TestClarify_TestPropertyLines;
    procedure TestClarify_TestPropertyInherited;
    procedure TestClarify_TestRecords;
    procedure TestClarify_TestReg;
    procedure TestClarify_TestReint;
    procedure TestClarify_TestReturnRemoval;
    procedure TestClarify_TestReturns;
    procedure TestClarify_TestRunOnConst;
    procedure TestClarify_TestRunOnDef;
    procedure TestClarify_TestRunOnLine;
    procedure TestClarify_TestTPObjects;
    procedure TestClarify_TestTry;
    procedure TestClarify_TestTypeDefs;
    procedure TestClarify_TestUses;
    procedure TestClarify_TestUsesChanges;
    procedure TestClarify_TestVarParam;
    procedure TestClarify_TestWarnings;
    procedure TestClarify_TestWith;

    procedure TestClarify_TestPackage;
    procedure TestClarify_TestProcBlankLines;

    procedure TestClarify_TestCases;
end;


implementation

uses
  { delphi }
  SysUtils,
  { jcl }
  JclStrings,
  { DUnit}
  TestFrameWork,
  { JCF }
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;

{ TFullTestClarify }

procedure TFullTestClarify.Setup;
var
  lsSettingsFileName: string;
begin
  inherited;

  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  lsSettingsFileName := GetTestFilesDir + '\JCFTestSettings.cfg';
  Check(FileExists(lsSettingsFileName), 'Settings file ' + lsSettingsFileName + ' not found');

  FormatSettings.ReadFromFile(lsSettingsFileName);
  FormatSettings.Obfuscate.Enabled := False;

end;

procedure TFullTestClarify.TestClarifyFile(const psName: string);
var
  lsInName, lsClearFileName: string;
begin
  Assert(psName <> '');

  { does it have an file extension? }
  if Pos('.', psName) > 0 then
  begin
    lsInName := psName;
    lsClearFileName := StrBefore('.', psName) + '.out';
  end
  else
  begin
    lsInName := psName + '.pas';
    lsClearFileName := psName + '.out';
  end;


  TestClarifyFile(GetTestFilesDir + lsInName,
    GetRefOutFilesDir + lsClearFileName)
end;

procedure TFullTestClarify.TestClarifyFile(const psInFileName,
  psRefOutput: string);
var
  lcConverter: TFileConverter;
  lsOutFileName: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  FormatSettings.Obfuscate.Enabled := False;

  // Check(FileExists(psRefOutput), 'reference output file ' + psRefOutput + ' not found');

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll := True;
    lcConverter.GuiMessages := False;

    { see also TestFileParse }
    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeperateOutput;

    GetRegSettings.OutputExtension := 'out';
    lcConverter.Input := psInFileName;
    lcConverter.Convert;

    Check(not lcConverter.ConvertError, 'Convert failed for ' +
      ExtractFileName(psInFileName) +
      ' : ' + lcConverter.ConvertErrorMessage);

    lsOutFileName := lcConverter.OutFileName;
    Check(lsOutFileName <> '', 'No output file');
    Check(FileExists(lsOutFileName), 'output file ' + lsOutFileName + ' not found');

    TestFileContentsSame(lsOutFileName, psRefOutput);

    // clean up
    DeleteFile(lsOutFileName);

  finally
    lcConverter.Free;
    FormatSettings.Obfuscate.Enabled := False;
  end;

end;



procedure TFullTestClarify.TestClarify_EmptyTest1;
begin
  TestClarifyFile('EmptyTest1');
end;

procedure TFullTestClarify.TestClarify_fFormTest;
begin
  TestClarifyFile('fFormTest');
end;

procedure TFullTestClarify.TestClarify_LittleTest1;
begin
  TestClarifyFile('LittleTest1');
end;

procedure TFullTestClarify.TestClarify_LittleTest10;
begin
  TestClarifyFile('LittleTest10');
end;

procedure TFullTestClarify.TestClarify_LittleTest11;
begin
  TestClarifyFile('LittleTest11');
end;

procedure TFullTestClarify.TestClarify_LittleTest2;
begin
  TestClarifyFile('LittleTest2');
end;

procedure TFullTestClarify.TestClarify_LittleTest3;
begin
  TestClarifyFile('LittleTest3');
end;

procedure TFullTestClarify.TestClarify_LittleTest4;
begin
  TestClarifyFile('LittleTest4');
end;

procedure TFullTestClarify.TestClarify_LittleTest5;
begin
  TestClarifyFile('LittleTest5');
end;

procedure TFullTestClarify.TestClarify_LittleTest6;
begin
  TestClarifyFile('LittleTest6');
end;

procedure TFullTestClarify.TestClarify_LittleTest7;
begin
  TestClarifyFile('LittleTest7');
end;

procedure TFullTestClarify.TestClarify_LittleTest8;
begin
  TestClarifyFile('LittleTest8');
end;

procedure TFullTestClarify.TestClarify_LittleTest9;
begin
  TestClarifyFile('LittleTest9');
end;

procedure TFullTestClarify.TestClarify_TestAbsolute;
begin
  TestClarifyFile('TestAbsolute');
end;

procedure TFullTestClarify.TestClarify_TestAlign;
begin
  TestClarifyFile('TestAlign');
end;

procedure TFullTestClarify.TestClarify_TestArray;
begin
  TestClarifyFile('TestArray');
end;

procedure TFullTestClarify.TestClarify_TestAsm;
begin
  TestClarifyFile('TestAsm');
end;


procedure TFullTestClarify.TestClarify_TestBlankLineRemoval;
begin
  TestClarifyFile('TestBlankLineRemoval');
end;

procedure TFullTestClarify.TestClarify_TestBogusDirectives;
begin
  TestClarifyFile('TestBogusDirectives');
end;

procedure TFullTestClarify.TestClarify_TestBogusTypes;
begin
  TestClarifyFile('TestBogusTypes');
end;

procedure TFullTestClarify.TestClarify_TestCaseBlock;
begin
  TestClarifyFile('TestCaseBlock');
end;

procedure TFullTestClarify.TestClarify_TestCases;
begin
  TestClarifyFile('TestCases.dpr');
end;

procedure TFullTestClarify.TestClarify_TestCast;
begin
  TestClarifyFile('TestCast');
end;

procedure TFullTestClarify.TestClarify_TestCastSimple;
begin
  TestClarifyFile('TestCastSimple');
end;

procedure TFullTestClarify.TestClarify_TestCharLiterals;
begin
  TestClarifyFile('TestCharLiterals');
end;

procedure TFullTestClarify.TestClarify_TestClassLines;
begin
  TestClarifyFile('TestClassLines');
end;

procedure TFullTestClarify.TestClarify_TestCommentIndent;
begin
  TestClarifyFile('TestCommentIndent');
end;

procedure TFullTestClarify.TestClarify_TestConstRecords;
begin
  TestClarifyFile('TestConstRecords');
end;

procedure TFullTestClarify.TestClarify_TestD6;
begin
  TestClarifyFile('TestD6');
end;

procedure TFullTestClarify.TestClarify_TestDeclarations;
begin
  TestClarifyFile('TestDeclarations');
end;

procedure TFullTestClarify.TestClarify_TestDeclarations2;
begin
  TestClarifyFile('TestDeclarations2');
end;

procedure TFullTestClarify.TestClarify_TestDefaultParams;
begin
  TestClarifyFile('TestDefaultParams');
end;

procedure TFullTestClarify.TestClarify_TestDeref;
begin
  TestClarifyFile('TestDeref');
end;

procedure TFullTestClarify.TestClarify_TestEmptyClass;
begin
  TestClarifyFile('TestEmptyClass');
end;

procedure TFullTestClarify.TestClarify_TestEsotericKeywords;
begin
  TestClarifyFile('TestEsotericKeywords');
end;

procedure TFullTestClarify.TestClarify_TestExclusion;
begin
  TestClarifyFile('TestExclusion');
end;

procedure TFullTestClarify.TestClarify_TestExclusionFlags;
begin
  TestClarifyFile('TestExclusionFlags');
end;

procedure TFullTestClarify.TestClarify_TestExternal;
begin
  TestClarifyFile('TestExternal');
end;

procedure TFullTestClarify.TestClarify_TestForward;
begin
  TestClarifyFile('TestForward');
end;

procedure TFullTestClarify.TestClarify_TestGoto;
begin
  TestClarifyFile('TestGoto');
end;

procedure TFullTestClarify.TestClarify_TestInheritedExpr;
begin
  TestClarifyFile('TestInheritedExpr');
end;

procedure TFullTestClarify.TestClarify_TestInitFinal;
begin
  TestClarifyFile('TestInitFinal');
end;

procedure TFullTestClarify.TestClarify_TestInterfaceImplements;
begin
  TestClarifyFile('TestInterfaceImplements');
end;

procedure TFullTestClarify.TestClarify_TestInterfaceMap;
begin
  TestClarifyFile('TestInterfaceMap');
end;

procedure TFullTestClarify.TestClarify_TestInterfaces;
begin
  TestClarifyFile('TestInterfaces');
end;

procedure TFullTestClarify.TestClarify_TestLayout;
begin
  TestClarifyFile('TestLayout');
end;

procedure TFullTestClarify.TestClarify_TestLayoutBare;
begin
  TestClarifyFile('TestLayoutBare');
end;

procedure TFullTestClarify.TestClarify_TestLayoutBare2;
begin
  TestClarifyFile('TestLayoutBare2');
end;

procedure TFullTestClarify.TestClarify_TestLayoutBare3;
begin
  TestClarifyFile('TestLayoutBare3');
end;

procedure TFullTestClarify.TestClarify_TestLibExports;
begin
  TestClarifyFile('TestLibExports');
end;

procedure TFullTestClarify.TestClarify_TestLineBreaking;
begin
  TestClarifyFile('TestLineBreaking');
end;

procedure TFullTestClarify.TestClarify_TestLocalTypes;
begin
  TestClarifyFile('TestLocalTypes');
end;

procedure TFullTestClarify.TestClarify_TestLongStrings;
begin
  TestClarifyFile('TestLongStrings');
end;

procedure TFullTestClarify.TestClarify_TestMarcoV;
begin
  TestClarifyFile('TestMarcoV');
end;

procedure TFullTestClarify.TestClarify_TestMessages;
begin
  TestClarifyFile('TestMessages');
end;

procedure TFullTestClarify.TestClarify_TestMH;
begin
  TestClarifyFile('TestMH');
end;

procedure TFullTestClarify.TestClarify_TestMixedModeCaps;
begin
  TestClarifyFile('TestMixedModeCaps');
end;

procedure TFullTestClarify.TestClarify_TestMVB;
begin
  TestClarifyFile('TestMVB');
end;

procedure TFullTestClarify.TestClarify_TestNested;
begin
  TestClarifyFile('TestNested');
end;

procedure TFullTestClarify.TestClarify_TestNestedRecords;
begin
  TestClarifyFile('TestNestedRecords');
end;

procedure TFullTestClarify.TestClarify_TestOperators;
begin
  TestClarifyFile('TestOperators');
end;

procedure TFullTestClarify.TestClarify_TestOleParams;
begin
  TestClarifyFile('TestOleParams');
end;

procedure TFullTestClarify.TestClarify_TestPackage;
begin
  TestClarifyFile('TestMe.dpk');
end;

procedure TFullTestClarify.TestClarify_TestParams;
begin
  TestClarifyFile('TestParams');
end;

procedure TFullTestClarify.TestClarify_TestParamSpaces;
begin
  TestClarifyFile('TestParamSpaces');
end;

procedure TFullTestClarify.TestClarify_TestPointers;
begin
  TestClarifyFile('TestPointers');
end;

procedure TFullTestClarify.TestClarify_TestProcBlankLines;
begin
  TestClarifyFile('TestProcBlankLines');
end;

procedure TFullTestClarify.TestClarify_TestProgram;
begin
  TestClarifyFile('TestProgram');
end;

procedure TFullTestClarify.TestClarify_TestProperties;
begin
  TestClarifyFile('TestProperties');
end;

procedure TFullTestClarify.TestClarify_TestPropertyInherited;
begin
  TestClarifyFile('TestPropertyInherited');
end;

procedure TFullTestClarify.TestClarify_TestPropertyLines;
begin
  TestClarifyFile('TestPropertyLines');
end;

procedure TFullTestClarify.TestClarify_TestRecords;
begin
  TestClarifyFile('TestRecords');
end;

procedure TFullTestClarify.TestClarify_TestReg;
begin
  TestClarifyFile('TestReg');
end;

procedure TFullTestClarify.TestClarify_TestReint;
begin
  TestClarifyFile('TestReint');
end;

procedure TFullTestClarify.TestClarify_TestReturnRemoval;
begin
  TestClarifyFile('TestReturnRemoval');
end;

procedure TFullTestClarify.TestClarify_TestReturns;
begin
  TestClarifyFile('TestReturns');
end;

procedure TFullTestClarify.TestClarify_TestRunOnConst;
begin
  TestClarifyFile('TestRunOnConst');
end;

procedure TFullTestClarify.TestClarify_TestRunOnDef;
begin
  TestClarifyFile('TestRunOnDef');
end;

procedure TFullTestClarify.TestClarify_TestRunOnLine;
begin
  TestClarifyFile('TestRunOnLine');
end;

procedure TFullTestClarify.TestClarify_TestTPObjects;
begin
  TestClarifyFile('TestTPObjects');
end;

procedure TFullTestClarify.TestClarify_TestTry;
begin
  TestClarifyFile('TestTry');
end;

procedure TFullTestClarify.TestClarify_TestTypeDefs;
begin
  TestClarifyFile('TestTypeDefs');
end;

procedure TFullTestClarify.TestClarify_TestUses;
begin
  TestClarifyFile('TestUses');
end;

procedure TFullTestClarify.TestClarify_TestUsesChanges;
begin
  TestClarifyFile('TestUsesChanges');
end;

procedure TFullTestClarify.TestClarify_TestVarParam;
begin
  TestClarifyFile('TestVarParam');
end;

procedure TFullTestClarify.TestClarify_TestWarnings;
begin
  TestClarifyFile('TestWarnings');
end;

procedure TFullTestClarify.TestClarify_TestWith;
begin
  TestClarifyFile('TestWith');
end;

procedure TFullTestClarify.TestClarify_LittleTest12;
begin
  TestClarifyFile('LittleTest12');
end;

procedure TFullTestClarify.TestClarify_LittleTest13;
begin
  TestClarifyFile('LittleTest13');
end;

procedure TFullTestClarify.TestClarify_LittleTest14;
begin
  TestClarifyFile('LittleTest14');
end;

procedure TFullTestClarify.TestClarify_LittleTest15;
begin
  TestClarifyFile('LittleTest15');
end;

procedure TFullTestClarify.TestClarify_LittleTest16;
begin
  TestClarifyFile('LittleTest16');
end;

procedure TFullTestClarify.TestClarify_LittleTest17;
begin
  TestClarifyFile('LittleTest17');
end;

procedure TFullTestClarify.TestClarify_LittleTest18;
begin
  TestClarifyFile('LittleTest18');
end;

initialization
 TestFramework.RegisterTest(TFullTestClarify.Suite);
end.