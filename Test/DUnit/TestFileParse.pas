unit TestFileParse;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestFileParse, released May 2003.
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
 TTestFileParse = class(TTestCase)
 private
    procedure TestParseFile(psInFileName: string; const piTokenCount: integer); overload;

 protected
   procedure Setup; override;


 published
   procedure TestDirs;
   procedure TestCreate;


    { one proc for each file,
      as it's nice to have a tick for each test file }
    procedure TestParse_Empty1;
    procedure TestParse_fFormTest;
    procedure TestParse_LittleTest1;
    procedure TestParse_LittleTest2;
    procedure TestParse_LittleTest3;
    procedure TestParse_LittleTest4;
    procedure TestParse_LittleTest5;
    procedure TestParse_LittleTest6;
    procedure TestParse_LittleTest7;
    procedure TestParse_LittleTest8;
    procedure TestParse_LittleTest9;
    procedure TestParse_LittleTest10;
    procedure TestParse_LittleTest11;
    procedure TestParse_LittleTest12;
    procedure TestParse_LittleTest13;
    procedure TestParse_LittleTest14;
    procedure TestParse_LittleTest15;
    procedure TestParse_LittleTest16;
    procedure TestParse_LittleTest17;
    procedure TestParse_LittleTest18;
    procedure TestParse_LittleTest19;
    procedure TestParse_LittleTest20;
    procedure TestParse_LittleTest21;
    procedure TestParse_LittleTest22;
    procedure TestParse_LittleTest23;
    procedure TestParse_LittleTest24;
    procedure TestParse_LittleTest25;
    procedure TestParse_LittleTest26;
    procedure TestParse_LittleTest27;
    procedure TestParse_LittleTest28;
    procedure TestParse_LittleTest29;
    procedure TestParse_LittleTest30;
    procedure TestParse_LittleTest31;
    procedure TestParse_LittleTest32;
    procedure TestParse_LittleTest33;
    procedure TestParse_LittleTest34;
    procedure TestParse_LittleTest35;
    procedure TestParse_LittleTest36;
    procedure TestParse_LittleTest37;
    procedure TestParse_LittleTest38;
    procedure TestParse_LittleTest39;
    procedure TestParse_LittleTest40;
    procedure TestParse_LittleTest41;
    procedure TestParse_LittleTest42;
    procedure TestParse_LittleTest43;
    procedure TestParse_LittleTest44;
    procedure TestParse_LittleTest45;
    procedure TestParse_LittleTest46;

    procedure TestParse_TestAbsolute;
    procedure TestParse_TestAlign;
    procedure TestParse_TestArray;
    procedure TestParse_TestAsm;
    procedure TestParse_TestAsmStructs;
    procedure TestParse_TestAtExpr;
    procedure TestParse_TestBlankLineRemoval;
    procedure TestParse_TestBogusDirectives;
    procedure TestParse_TestBogusTypes;
    procedure TestParse_TestCaseBlock;
    procedure TestParse_TestCaseIfFormat;
    procedure TestParse_TestSimpleCast;
    procedure TestParse_TestCast;
    procedure TestParse_TestCharLiterals;
    procedure TestParse_TestClassLines;
    procedure TestParse_TestCommentIndent;
    procedure TestParse_TestConstRecords;
    procedure TestParse_TestD6;
    procedure TestParse_TestDeclarations;
    procedure TestParse_TestDeclarations2;
    procedure TestParse_TestDefaultParams;
    procedure TestParse_TestDefines;
    procedure TestParse_TestDeref;
    procedure TestParse_TestEmptyCase;
    procedure TestParse_TestEmptyClass;
    procedure TestParse_TestEmptySquareBrackets;
    procedure TestParse_TestEsotericKeywords;
    procedure TestParse_TestExclusion;
    procedure TestParse_TestExclusionFlags;
    procedure TestParse_TestExternal;
    procedure TestParse_TestForward;
    procedure TestParse_TestGoto;
    procedure TestParse_TestInheritedExpr;
    procedure TestParse_TestInitFinal;
    procedure TestParse_TestInterfaceImplements;
    procedure TestParse_TestInterfaceMap;
    procedure TestParse_TestInterfaces;
    procedure TestParse_TestLayout;
    procedure TestParse_TestLayoutBare;
    procedure TestParse_TestLayoutBare2;
    procedure TestParse_TestLayoutBare3;
    procedure TestParse_TestLibExports;
    procedure TestParse_TestLineBreaking;
    procedure TestParse_TestLocalTypes;
    procedure TestParse_TestLongStrings;
    procedure TestParse_TestMarcoV;
    procedure TestParse_TestMixedModeCaps;
    procedure TestParse_TestMessages;
    procedure TestParse_TestMVB;
    procedure TestParse_TestNested;
    procedure TestParse_TestNestedRecords;
    procedure TestParse_TestOleParams;
    procedure TestParse_TestOperators;
    procedure TestParse_TestParams;
    procedure TestParse_TestParamSpaces;
    procedure TestParse_TestPointers;
    procedure TestParse_TestProgram;
    procedure TestParse_TestProperties;
    procedure TestParse_TestPropertyLines;
    procedure TestParse_TestPropertyInherited;
    procedure TestParse_TestRaise;
    procedure TestParse_TestRecords;
    procedure TestParse_TestReg;
    procedure TestParse_TestReint;
    procedure TestParse_TestReturnRemoval;
    procedure TestParse_TestReturns;
    procedure TestParse_TestRunOnConst;
    procedure TestParse_TestRunOnDef;
    procedure TestParse_TestRunOnLine;

    procedure TestParse_TestSimpleIfDef;
    procedure TestParse_TestSimpleIfDef2;
    procedure TestParse_TestSimpleIfDef3;
    procedure TestParse_TestSimpleIfDef4;
    procedure TestParse_TestSimpleIfDef5;

    procedure TestParse_TestTestMH;
    procedure TestParse_TestTPObjects;
    procedure TestParse_TestTry;
    procedure TestParse_TestTypeDefs;

    procedure TestParse_TestUnitPlatform;
    procedure TestParse_TestUnitAllDirectives;
    procedure TestParse_TestUnitDeprecated;
    procedure TestParse_TestUnitLibrary;

    procedure TestParse_TestUses;
    procedure TestParse_TestUsesChanges;
    procedure TestParse_TestWarnings;
    procedure TestParse_TestVarParam;
    procedure TestParse_TestWith;

   procedure TestParse_TestCases;
   procedure TestParse_TestPackage;
   procedure TestParse_TestProcBlankLines;

 end;

implementation

uses
  { delphi } SysUtils,
  JclStrings, JclFileUtils,
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;


procedure TTestFileParse.Setup;
var
  lsSettingsFileName: string;
begin
  inherited;

  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  { use clarify test settings }
  lsSettingsFileName := GetTestSettingsFileName;
  Check(FileExists(lsSettingsFileName), 'Settings file ' + lsSettingsFileName + ' not found');

  GetRegSettings.FormatConfigFileName := lsSettingsFileName;
  FormatSettings; // create and read
  FormatSettings.Obfuscate.Enabled := False;
end;


procedure TTestFileParse.TestParseFile(psInFileName: string;
  const piTokenCount: integer);
var
  lcConverter: TFileConverter;
  lsSettingsFileName, lsOutFileName: string;
begin
  { does it have an file extension? }
  if Pos('.', psInFileName) <= 0 then
    psInFileName := psInFileName + '.pas';

  if Pos(PathSeparator, psInFileName) <= 0 then
    psInFileName := GetTestFilesDir + psInFileName;

  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll := True;
    lcConverter.GuiMessages := False;

    { see also TestFullClarify }
    lsSettingsFileName := GetTestSettingsFileName;
    Check(FileExists(lsSettingsFileName), 'Settings file ' + lsSettingsFileName + ' not found');

    FormatSettings.ReadFromFile(lsSettingsFileName);
    FormatSettings.Obfuscate.Enabled := False;

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

    CheckEquals(piTokenCount, lcConverter.TokenCount, 'wrong number of tokens');

    // clean up
    DeleteFile(lsOutFileName);

  finally
    lcConverter.Free;
  end;
end;


procedure TTestFileParse.TestCreate;
var
  lcConverter: TFileConverter;
begin
  lcConverter := TFileConverter.Create;
  lcConverter.Free;
end;

procedure TTestFileParse.TestDirs;
begin
  Check(DirectoryExists(GetTestFilesDir), 'Test files dir ' + GetTestFilesDir + ' not found');
  Check(DirectoryExists(GetRefOutFilesDir), 'Test files ref out dir ' + GetTestFilesDir + ' not found');
end;


procedure TTestFileParse.TestParse_Empty1;
begin
  TestParseFile('EmptyTest1', 18);
end;

procedure TTestFileParse.TestParse_fFormTest;
begin
  TestParseFile('fFormTest', 150);
end;

procedure TTestFileParse.TestParse_LittleTest1;
begin
  TestParseFile('LittleTest1', 28);
end;

procedure TTestFileParse.TestParse_LittleTest2;
begin
  TestParseFile('LittleTest2', 29);
end;

procedure TTestFileParse.TestParse_LittleTest3;
begin
  TestParseFile('LittleTest3', 42);
end;

procedure TTestFileParse.TestParse_LittleTest4;
begin
  TestParseFile('LittleTest4', 45);
end;

procedure TTestFileParse.TestParse_LittleTest5;
begin
  TestParseFile('LittleTest5', 58);
end;

procedure TTestFileParse.TestParse_LittleTest6;
begin
  TestParseFile('LittleTest6', 78);
end;

procedure TTestFileParse.TestParse_LittleTest7;
begin
  TestParseFile('LittleTest7', 109);
end;

procedure TTestFileParse.TestParse_LittleTest8;
begin
  TestParseFile('LittleTest8', 41);
end;

procedure TTestFileParse.TestParse_TestAbsolute;
begin
  TestParseFile('TestAbsolute', 86);
end;

procedure TTestFileParse.TestParse_TestAlign;
begin
  TestParseFile('TestAlign', 662);
end;


procedure TTestFileParse.TestParse_TestArray;
begin
  TestParseFile('TestArray', 220);
end;


procedure TTestFileParse.TestParse_TestAsm;
begin
  TestParseFile('TestAsm', 581);
end;

procedure TTestFileParse.TestParse_TestBlankLineRemoval;
begin
  TestParseFile('TestBlankLineRemoval', 373);
end;

procedure TTestFileParse.TestParse_TestBogusDirectives;
begin
  TestParseFile('TestBogusDirectives', 427);
end;

procedure TTestFileParse.TestParse_TestBogusTypes;
begin
  TestParseFile('TestBogusTypes', 230);
end;

procedure TTestFileParse.TestParse_TestCaseBlock;
begin
  TestParseFile('TestCaseBlock', 3041);
end;

procedure TTestFileParse.TestParse_TestCast;
begin
  TestParseFile('TestCast', 600);
end;

procedure TTestFileParse.TestParse_TestSimpleCast;
begin
  TestParseFile('TestCastSimple', 843);
end;

procedure TTestFileParse.TestParse_TestCharLiterals;
begin
  TestParseFile('TestCharLiterals', 177);
end;

procedure TTestFileParse.TestParse_TestClassLines;
begin
  TestParseFile('TestClassLines', 71);
end;

procedure TTestFileParse.TestParse_TestCommentIndent;
begin
  TestParseFile('TestCommentIndent', 549);
end;

procedure TTestFileParse.TestParse_TestConstRecords;
begin
  TestParseFile('TestConstRecords', 760);
end;

procedure TTestFileParse.TestParse_TestD6;
begin
  TestParseFile('TestD6', 959);
end;

procedure TTestFileParse.TestParse_TestDeclarations;
begin
  TestParseFile('TestDeclarations', 1003);
end;


procedure TTestFileParse.TestParse_TestDeclarations2;
begin
  TestParseFile('TestDeclarations2', 362);
end;

procedure TTestFileParse.TestParse_TestDefaultParams;
begin
  TestParseFile('TestDefaultParams', 698);
end;

procedure TTestFileParse.TestParse_TestEmptyClass;
begin
  TestParseFile('TestEmptyClass', 244);
end;

procedure TTestFileParse.TestParse_TestEsotericKeywords;
begin
  TestParseFile('TestEsotericKeywords', 258);
end;

procedure TTestFileParse.TestParse_TestExclusion;
begin
  TestParseFile('TestExclusion', 431);
end;

procedure TTestFileParse.TestParse_TestExclusionFlags;
begin
  TestParseFile('TestExclusionFlags', 723);
end;

procedure TTestFileParse.TestParse_TestExternal;
begin
  TestParseFile('TestExternal', 259);
end;

procedure TTestFileParse.TestParse_TestForward;
begin
  TestParseFile('TestForward', 332);
end;

procedure TTestFileParse.TestParse_TestGoto;
begin
  TestParseFile('TestGoto', 443);
end;

procedure TTestFileParse.TestParse_TestInitFinal;
begin
  TestParseFile('TestInitFinal', 170);
end;

procedure TTestFileParse.TestParse_TestInterfaceImplements;
begin
  TestParseFile('TestInterfaceImplements', 225);
end;

procedure TTestFileParse.TestParse_TestInterfaceMap;
begin
  TestParseFile('TestInterfaceMap', 397);
end;

procedure TTestFileParse.TestParse_TestInterfaces;
begin
  TestParseFile('TestInterfaces', 648);
end;

procedure TTestFileParse.TestParse_TestLayout;
begin
  TestParseFile('TestLayout', 1227);
end;

procedure TTestFileParse.TestParse_TestLayoutBare;
begin
  TestParseFile('TestLayoutBare', 1555);
end;

procedure TTestFileParse.TestParse_TestLayoutBare2;
begin
  TestParseFile('TestLayoutBare2', 1008);
end;

procedure TTestFileParse.TestParse_TestLayoutBare3;
begin
  TestParseFile('TestLayoutBare3', 1177);
end;

procedure TTestFileParse.TestParse_TestLibExports;
begin
  TestParseFile('TestLibExports', 119);
end;

procedure TTestFileParse.TestParse_TestLineBreaking;
begin
  TestParseFile('TestLineBreaking', 6108);
end;

procedure TTestFileParse.TestParse_TestLocalTypes;
begin
  TestParseFile('TestLocalTypes', 297);
end;

procedure TTestFileParse.TestParse_TestLongStrings;
begin
  TestParseFile('TestLongStrings', 163);
end;

procedure TTestFileParse.TestParse_TestMarcoV;
begin
  TestParseFile('TestMarcoV', 241);
end;

procedure TTestFileParse.TestParse_TestTestMH;
begin
  TestParseFile('TestMH', 2956);
end;

procedure TTestFileParse.TestParse_TestMixedModeCaps;
begin
  TestParseFile('TestMixedModeCaps', 123);
end;

procedure TTestFileParse.TestParse_TestMVB;
begin
  TestParseFile('TestMVB', 833);
end;

procedure TTestFileParse.TestParse_TestNested;
begin
  TestParseFile('TestNested', 658);
end;

procedure TTestFileParse.TestParse_TestNestedRecords;
begin
  TestParseFile('TestNestedRecords', 1189);
end;

procedure TTestFileParse.TestParse_TestOleParams;
begin
  TestParseFile('TestOleParams', 113);
end;

procedure TTestFileParse.TestParse_TestOperators;
begin
  TestParseFile('TestOperators', 1232);
end;

procedure TTestFileParse.TestParse_TestParams;
begin
  TestParseFile('TestParams', 218);
end;

procedure TTestFileParse.TestParse_TestParamSpaces;
begin
  TestParseFile('TestParamSpaces', 159);
end;

procedure TTestFileParse.TestParse_TestPointers;
begin
  TestParseFile('TestPointers', 193);
end;

procedure TTestFileParse.TestParse_TestProgram;
begin
  TestParseFile('TestProgram', 1246);
end;

procedure TTestFileParse.TestParse_TestProperties;
begin
  TestParseFile('TestProperties', 751);
end;

procedure TTestFileParse.TestParse_TestPropertyLines;
begin
  TestParseFile('TestPropertyLines', 1186);
end;

procedure TTestFileParse.TestParse_TestRecords;
begin
  TestParseFile('TestRecords', 1455);
end;

procedure TTestFileParse.TestParse_TestReg;
begin
  TestParseFile('TestReg', 85);
end;

procedure TTestFileParse.TestParse_TestReint;
begin
  TestParseFile('TestReint', 159);
end;

procedure TTestFileParse.TestParse_TestReturnRemoval;
begin
  TestParseFile('TestReturnRemoval', 256);
end;

procedure TTestFileParse.TestParse_TestReturns;
begin
  TestParseFile('TestReturns', 141);
end;

procedure TTestFileParse.TestParse_TestRunOnConst;
begin
  TestParseFile('TestRunOnConst', 465);
end;

procedure TTestFileParse.TestParse_TestRunOnDef;
begin
  TestParseFile('TestRunOnDef', 363);
end;

procedure TTestFileParse.TestParse_TestRunOnLine;
begin
  TestParseFile('TestRunOnLine', 3668);
end;

procedure TTestFileParse.TestParse_TestTPObjects;
begin
  TestParseFile('TestTPObjects', 126);
end;

procedure TTestFileParse.TestParse_TestTry;
begin
  TestParseFile('TestTry', 939);
end;

procedure TTestFileParse.TestParse_TestTypeDefs;
begin
  TestParseFile('TestTypeDefs', 793);
end;

procedure TTestFileParse.TestParse_TestUses;
begin
  TestParseFile('TestUses', 64);
end;

procedure TTestFileParse.TestParse_TestUsesChanges;
begin
  TestParseFile('TestUsesChanges', 56);
end;

procedure TTestFileParse.TestParse_TestWarnings;
begin
  TestParseFile('TestWarnings', 700);
end;

procedure TTestFileParse.TestParse_TestWith;
begin
  TestParseFile('TestWith', 735);
end;

procedure TTestFileParse.TestParse_TestCases;
begin
  TestParseFile('Testcases.dpr', 1071);
end;


procedure TTestFileParse.TestParse_TestPackage;
begin
  TestParseFile('TestMe.dpk', 684);
end;

procedure TTestFileParse.TestParse_TestProcBlankLines;
begin
  TestParseFile('TestProcBlankLines', 64);
end;

procedure TTestFileParse.TestParse_TestVarParam;
begin
  TestParseFile('TestVarParam', 116);
end;


procedure TTestFileParse.TestParse_LittleTest9;
begin
  TestParseFile('LittleTest9', 69);
end;

procedure TTestFileParse.TestParse_TestDeref;
begin
  TestParseFile('TestDeref', 584);
end;

procedure TTestFileParse.TestParse_TestPropertyInherited;
begin
  TestParseFile('TestPropertyInherited', 797);
end;

procedure TTestFileParse.TestParse_TestMessages;
begin
  TestParseFile('TestMessages', 130);
end;

procedure TTestFileParse.TestParse_LittleTest10;
begin
  TestParseFile('LittleTest10', 367);
end;

procedure TTestFileParse.TestParse_TestInheritedExpr;
begin
  TestParseFile('TestInheritedExpr', 301);
end;

procedure TTestFileParse.TestParse_LittleTest11;
begin
  TestParseFile('LittleTest11', 97);
end;

procedure TTestFileParse.TestParse_LittleTest12;
begin
  TestParseFile('LittleTest12', 54);
end;

procedure TTestFileParse.TestParse_LittleTest13;
begin
  TestParseFile('LittleTest13', 86);
end;

procedure TTestFileParse.TestParse_LittleTest14;
begin
  TestParseFile('LittleTest14', 37);
end;

procedure TTestFileParse.TestParse_LittleTest15;
begin
  TestParseFile('LittleTest15', 38);
end;

procedure TTestFileParse.TestParse_LittleTest16;
begin
  TestParseFile('LittleTest16', 101);
end;

procedure TTestFileParse.TestParse_LittleTest17;
begin
  TestParseFile('LittleTest17', 102);
end;

procedure TTestFileParse.TestParse_LittleTest18;
begin
  TestParseFile('LittleTest18', 102);
end;

procedure TTestFileParse.TestParse_TestAtExpr;
begin
  TestParseFile('TestAtExpr', 79);
end;

procedure TTestFileParse.TestParse_TestAsmStructs;
begin
  TestParseFile('TestAsmStructs', 358);
end;

procedure TTestFileParse.TestParse_TestUnitAllDirectives;
begin
  TestParseFile('TestUnitAllDirectives', 21);
end;

procedure TTestFileParse.TestParse_TestUnitDeprecated;
begin
  TestParseFile('TestUnitDeprecated', 17);
end;

procedure TTestFileParse.TestParse_TestUnitLibrary;
begin
  TestParseFile('TestUnitLibrary', 17);
end;

procedure TTestFileParse.TestParse_TestUnitPlatform;
begin
  TestParseFile('TestUnitPlatform', 17);
end;

procedure TTestFileParse.TestParse_LittleTest19;
begin
  TestParseFile('LittleTest19', 168);
end;

procedure TTestFileParse.TestParse_TestRaise;
begin
  TestParseFile('TestRaise', 519);
end;

procedure TTestFileParse.TestParse_LittleTest20;
begin
  TestParseFile('LittleTest20', 62);
end;

procedure TTestFileParse.TestParse_LittleTest21;
begin
  TestParseFile('LittleTest21', 37);
end;

procedure TTestFileParse.TestParse_LittleTest22;
begin
  TestParseFile('LittleTest22', 53);
end;

procedure TTestFileParse.TestParse_LittleTest23;
begin
  TestParseFile('LittleTest23', 74);
end;

procedure TTestFileParse.TestParse_LittleTest24;
begin
  TestParseFile('LittleTest24', 69);
end;

procedure TTestFileParse.TestParse_LittleTest25;
begin
  TestParseFile('LittleTest25', 42);
end;

procedure TTestFileParse.TestParse_LittleTest26;
begin
  TestParseFile('LittleTest26', 97);
end;

procedure TTestFileParse.TestParse_LittleTest27;
begin
  TestParseFile('LittleTest27', 89);
end;

procedure TTestFileParse.TestParse_TestEmptySquareBrackets;
begin
  TestParseFile('TestEmptySquareBrackets', 66);
end;

procedure TTestFileParse.TestParse_LittleTest28;
begin
  TestParseFile('LittleTest28', 71);
end;

procedure TTestFileParse.TestParse_LittleTest29;
begin
  TestParseFile('LittleTest29', 141);
end;

procedure TTestFileParse.TestParse_LittleTest30;
begin
  TestParseFile('LittleTest30', 60);
end;

procedure TTestFileParse.TestParse_LittleTest31;
begin
  TestParseFile('LittleTest31', 58);
end;

procedure TTestFileParse.TestParse_LittleTest32;
begin
  TestParseFile('LittleTest32', 91);
end;

procedure TTestFileParse.TestParse_LittleTest33;
begin
  TestParseFile('LittleTest33', 45);
end;

procedure TTestFileParse.TestParse_TestEmptyCase;
begin
  TestParseFile('TestEmptyCase', 128);
end;

procedure TTestFileParse.TestParse_TestCaseIfFormat;
begin
  TestParseFile('TestCaseIfFormat', 294);
end;

procedure TTestFileParse.TestParse_LittleTest34;
begin
  TestParseFile('LittleTest34', 78);
end;

procedure TTestFileParse.TestParse_LittleTest35;
begin
  TestParseFile('LittleTest35', 38);
end;

procedure TTestFileParse.TestParse_LittleTest36;
begin
  TestParseFile('LittleTest36', 77);
end;

procedure TTestFileParse.TestParse_LittleTest37;
begin
  TestParseFile('LittleTest37', 76);
end;

procedure TTestFileParse.TestParse_TestSimpleIfDef;
begin
  TestParseFile('TestSimpleIfDef', 54);
end;

procedure TTestFileParse.TestParse_TestSimpleIfDef2;
begin
  TestParseFile('TestSimpleIfDef2', 33);
end;

procedure TTestFileParse.TestParse_TestSimpleIfDef3;
begin
  TestParseFile('TestSimpleIfDef3', 83);
end;

procedure TTestFileParse.TestParse_TestSimpleIfDef4;
begin
  TestParseFile('TestSimpleIfDef4', 92);
end;

procedure TTestFileParse.TestParse_TestSimpleIfDef5;
begin
  TestParseFile('TestSimpleIfDef5', 39);
end;

procedure TTestFileParse.TestParse_LittleTest38;
begin
  TestParseFile('LittleTest38', 53);
end;

procedure TTestFileParse.TestParse_LittleTest39;
begin
  TestParseFile('LittleTest39', 56);

end;

procedure TTestFileParse.TestParse_LittleTest40;
begin
  TestParseFile('LittleTest40', 143);
end;

procedure TTestFileParse.TestParse_TestDefines;
begin
  TestParseFile('TestDefines', 262);
end;

procedure TTestFileParse.TestParse_LittleTest41;
begin
  TestParseFile('LittleTest41', 47);
end;

procedure TTestFileParse.TestParse_LittleTest42;
begin
  TestParseFile('LittleTest42', 112);
end;

procedure TTestFileParse.TestParse_LittleTest43;
begin
  TestParseFile('LittleTest43', 413);
end;

procedure TTestFileParse.TestParse_LittleTest44;
begin
  TestParseFile('LittleTest44', 285);
end;

procedure TTestFileParse.TestParse_LittleTest45;
begin
  TestParseFile('LittleTest45', 177);
end;

procedure TTestFileParse.TestParse_LittleTest46;
begin
  TestParseFile('LittleTest46', 96);
end;

initialization
 TestFramework.RegisterTest(TTestFileParse.Suite);
end.