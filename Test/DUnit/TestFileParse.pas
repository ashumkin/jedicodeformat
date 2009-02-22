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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ test that all test files can parse }

uses
  TestFrameWork;

type
  TTestFileParse = class(TTestCase)
  private
    procedure TestParseFile(psInFileName: string; const piTokenCount: integer); overload;

  protected
    procedure SetUp; override;


  published
    procedure TestDirs;
    procedure TestCreate;


    { one proc for each file,
      as it's nice to have a tick for each test file }
    procedure Empty1;
    procedure fFormTest;
    procedure fBracketProp;

    procedure LittleTest1;
    procedure LittleTest2;
    procedure LittleTest3;
    procedure LittleTest4;
    procedure LittleTest5;
    procedure LittleTest6;
    procedure LittleTest7;
    procedure LittleTest8;
    procedure LittleTest9;
    procedure LittleTest10;
    procedure LittleTest11;
    procedure LittleTest12;
    procedure LittleTest13;
    procedure LittleTest14;
    procedure LittleTest15;
    procedure LittleTest16;
    procedure LittleTest17;
    procedure LittleTest18;
    procedure LittleTest19;
    procedure LittleTest20;
    procedure LittleTest21;
    procedure LittleTest22;
    procedure LittleTest23;
    procedure LittleTest24;
    procedure LittleTest25;
    procedure LittleTest26;
    procedure LittleTest27;
    procedure LittleTest28;
    procedure LittleTest29;
    procedure LittleTest30;
    procedure LittleTest31;
    procedure LittleTest32;
    procedure LittleTest33;
    procedure LittleTest34;
    procedure LittleTest35;
    procedure LittleTest36;
    procedure LittleTest37;
    procedure LittleTest38;
    procedure LittleTest39;
    procedure LittleTest40;
    procedure LittleTest41;
    procedure LittleTest42;
    procedure LittleTest43;
    procedure LittleTest44;
    procedure LittleTest45;
    procedure LittleTest46;
    procedure LittleTest47;
    procedure LittleTest48;
    procedure LittleTest49;
    procedure LittleTest50;
    procedure LittleTest51;
    procedure LittleTest52;
    procedure LittleTest53;
    procedure LittleTest54;
    procedure LittleTest55;
    procedure LittleTest56;
    procedure LittleTest57;
    procedure LittleTest58;
    procedure LittleTest59;
    procedure LittleTest60;
    procedure LittleTest61;
    procedure LittleTest62;

    procedure TestAbsolute;
    procedure TestAlign;
    procedure TestArray;
    procedure TestAsm;
    procedure TestAsmStructs;
    procedure TestAtExpr;
    procedure TestBlankLineRemoval;
    procedure TestBogusDirectives;
    procedure TestBogusTypes;
    procedure TestCaseBlock;
    procedure TestCaseIfFormat;
    procedure TestSimpleCast;
    procedure TestCast;
    procedure TestCharLiterals;
    procedure TestClassLines;
    procedure TestCommentIndent;
    procedure TestCommentIndent2;
    procedure TestCondReturns;
    procedure TestConstRecords;
    procedure TestD6;
    procedure TestDeclarations;
    procedure TestDeclarations2;
    procedure TestDefaultParams;
    procedure TestDefines;
    procedure TestDeref;
    procedure TestEmptyCase;
    procedure TestEmptyClass;
    procedure TestEmptySquareBrackets;
    procedure TestEndElse;
    procedure TestEsotericKeywords;
    procedure TestExclusion;
    procedure TestExclusionFlags;
    procedure TestExternal;
    procedure TestForward;
    procedure TestGoto;
    procedure TestInheritedExpr;
    procedure TestInitFinal;
    procedure TestInline;
    procedure TestInterfaceImplements;
    procedure TestInterfaceMap;
    procedure TestInterfaces;
    procedure TestLayout;
    procedure TestLayoutBare;
    procedure TestLayoutBare2;
    procedure TestLayoutBare3;
    procedure TestLibExports;
    procedure TestLineBreaking;
    procedure TestLocalTypes;
    procedure TestLongStrings;
    procedure TestMarcoV;
    procedure TestMixedModeCaps;
    procedure TestMessages;
    procedure TestMVB;
    procedure TestNested;
    procedure TestNestedRecords;
    procedure TestOleParams;
    procedure TestOperators;
    procedure TestParams;
    procedure TestParamSpaces;
    procedure TestPointers;
    procedure TestProgram;
    procedure TestProperties;
    procedure TestPropertyLines;
    procedure TestPropertyInherited;
    procedure TestRaise;
    procedure TestRecords;
    procedure TestReg;
    procedure TestReint;
    procedure TestReturnRemoval;
    procedure TestReturns;
    procedure TestRunOnConst;
    procedure TestRunOnDef;
    procedure TestRunOnLine;

    procedure TestSimpleIfDef;
    procedure TestSimpleIfDef2;
    procedure TestSimpleIfDef3;
    procedure TestSimpleIfDef4;
    procedure TestSimpleIfDef5;
    procedure TestSimpleIfDef6;

    procedure TestTestMH;
    procedure TestTPObjects;
    procedure TestTry;
    procedure TestTypeDefs;

    procedure TestUnitPlatform;
    procedure TestUnitAllDirectives;
    procedure TestUnitDeprecated;
    procedure TestUnitLibrary;

    procedure TestUses;
    procedure TestUsesChanges;
    procedure TestWarnings;
    procedure TestWarnDestroy;
    procedure TestVarParam;
    procedure TestWith;
    procedure TestProcBlankLines;
    procedure TestCondCompBreaks;
    procedure TestCondCompBreaks2;
    procedure TestAsmLabel;

    procedure TestDephiNetUses;
    procedure TestForIn;

    procedure TestConstBug;
    procedure TestDottedName;
    procedure TestDelphiNetClass;
    procedure TestDelphiNetConst;
    procedure TestDelphiNetStatic;
    procedure TestTestDotNetForm1;
    procedure TestDelphiNetOperatorOverload;
    procedure TestDelphiNetHelperClass;
    procedure TestDelphiNetNestedType;
    procedure TestDelphiNetNestedType2;
    procedure TestDelphiNetRecordForward;

    procedure TestDelphiNetWebService;
    procedure TestDelphiNetWebService2;
    procedure TestDelphiNetAttributes;
    procedure TestDelphiNetKeywords;
    procedure TestDelphiNetClassVar;

    procedure TestDelphiNetSealedClass;
    procedure TestDelphiNetFinalMethod;
    procedure TestDelphiNetDottedType;
    procedure TestDelphiNetAmpersandMethod;
    procedure TestDelphiNetMulticast;
    procedure TestDelphiNetDynamicArray;
    procedure TestDelphiNetRecordProcs;

    procedure TestTryExceptRaise;
    procedure TestTrailingCommaParam;
    procedure TestDprNoBegin;
    procedure TestDLLIndex;
    procedure TestIncAt;
    procedure TestAsmAnd;
    procedure TestVarArgs;

    procedure TestCases;
    procedure TestPackage;

  end;

implementation

uses
  { delphi }
  SysUtils, Windows,
  { JCL }
  JclFileUtils,
  { local }
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;


procedure TTestFileParse.Setup;
begin
  inherited;

  InitTestSettings;
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
    lcConverter.YesAll      := True;
    lcConverter.GuiMessages := False;

    { see also TestFullClarify }
    lsSettingsFileName := GetTestSettingsFileName;
    Check(FileExists(lsSettingsFileName), 'Settings file ' +
      lsSettingsFileName + ' not found');

    FormatSettings.ReadFromFile(lsSettingsFileName, True);
    FormatSettings.Obfuscate.Enabled := False;

    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeparateOutput;
    GetRegSettings.OutputExtension := 'out';

    lcConverter.Input := psInFileName;

    lcConverter.Convert;

    Check( not lcConverter.ConvertError, 'Convert failed for ' +
      ExtractFileName(psInFileName));

    lsOutFileName := lcConverter.OutFileName;
    Check(lsOutFileName <> '', 'No output file');
    Check(FileExists(lsOutFileName), 'output file ' + lsOutFileName + ' not found');

    CheckEquals(piTokenCount, lcConverter.TokenCount, 'wrong number of tokens');

    // clean up
    SysUtils.DeleteFile(lsOutFileName);

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
  Check(DirectoryExists(GetTestFilesDir), 'Test files dir ' +
    GetTestFilesDir + ' not found');
  Check(DirectoryExists(GetRefOutFilesDir), 'Test files ref out dir ' +
    GetTestFilesDir + ' not found');
end;


procedure TTestFileParse.Empty1;
begin
  TestParseFile('EmptyTest1', 18);
end;

procedure TTestFileParse.fFormTest;
begin
  TestParseFile('fFormTest', 150);
end;

procedure TTestFileParse.LittleTest1;
begin
  TestParseFile('LittleTest1', 28);
end;

procedure TTestFileParse.LittleTest2;
begin
  TestParseFile('LittleTest2', 29);
end;

procedure TTestFileParse.LittleTest3;
begin
  TestParseFile('LittleTest3', 42);
end;

procedure TTestFileParse.LittleTest4;
begin
  TestParseFile('LittleTest4', 45);
end;

procedure TTestFileParse.LittleTest5;
begin
  TestParseFile('LittleTest5', 58);
end;

procedure TTestFileParse.LittleTest6;
begin
  TestParseFile('LittleTest6', 78);
end;

procedure TTestFileParse.LittleTest7;
begin
  TestParseFile('LittleTest7', 109);
end;

procedure TTestFileParse.LittleTest8;
begin
  TestParseFile('LittleTest8', 41);
end;

procedure TTestFileParse.TestAbsolute;
begin
  TestParseFile('TestAbsolute', 86);
end;

procedure TTestFileParse.TestAlign;
begin
  TestParseFile('TestAlign', 662);
end;


procedure TTestFileParse.TestArray;
begin
  TestParseFile('TestArray', 220);
end;


procedure TTestFileParse.TestAsm;
begin
  TestParseFile('TestAsm', 828);
end;

procedure TTestFileParse.TestBlankLineRemoval;
begin
  TestParseFile('TestBlankLineRemoval', 373);
end;

procedure TTestFileParse.TestBogusDirectives;
begin
  TestParseFile('TestBogusDirectives', 456);
end;

procedure TTestFileParse.TestBogusTypes;
begin
  TestParseFile('TestBogusTypes', 230);
end;

procedure TTestFileParse.TestCaseBlock;
begin
  TestParseFile('TestCaseBlock', 3041);
end;

procedure TTestFileParse.TestCast;
begin
  TestParseFile('TestCast', 654);
end;

procedure TTestFileParse.TestSimpleCast;
begin
  TestParseFile('TestCastSimple', 843);
end;

procedure TTestFileParse.TestCharLiterals;
begin
  TestParseFile('TestCharLiterals', 1035);
end;

procedure TTestFileParse.TestClassLines;
begin
  TestParseFile('TestClassLines', 71);
end;

procedure TTestFileParse.TestCommentIndent;
begin
  TestParseFile('TestCommentIndent', 549);
end;

procedure TTestFileParse.TestCommentIndent2;
begin
  TestParseFile('TestCommentIndent2', 361);
end;

procedure TTestFileParse.TestConstRecords;
begin
  TestParseFile('TestConstRecords', 945);
end;

procedure TTestFileParse.TestD6;
begin
  TestParseFile('TestD6', 959);
end;

procedure TTestFileParse.TestDeclarations;
begin
  TestParseFile('TestDeclarations', 1004);
end;


procedure TTestFileParse.TestDeclarations2;
begin
  TestParseFile('TestDeclarations2', 362);
end;

procedure TTestFileParse.TestDefaultParams;
begin
  TestParseFile('TestDefaultParams', 698);
end;

procedure TTestFileParse.TestEmptyClass;
begin
  TestParseFile('TestEmptyClass', 244);
end;

procedure TTestFileParse.TestEsotericKeywords;
begin
  TestParseFile('TestEsotericKeywords', 258);
end;

procedure TTestFileParse.TestExclusion;
begin
  TestParseFile('TestExclusion', 431);
end;

procedure TTestFileParse.TestExclusionFlags;
begin
  TestParseFile('TestExclusionFlags', 723);
end;

procedure TTestFileParse.TestExternal;
begin
  TestParseFile('TestExternal', 259);
end;

procedure TTestFileParse.TestForward;
begin
  TestParseFile('TestForward', 332);
end;

procedure TTestFileParse.TestGoto;
begin
  TestParseFile('TestGoto', 503);
end;

procedure TTestFileParse.TestInitFinal;
begin
  TestParseFile('TestInitFinal', 170);
end;

procedure TTestFileParse.TestInterfaceImplements;
begin
  TestParseFile('TestInterfaceImplements', 225);
end;

procedure TTestFileParse.TestInterfaceMap;
begin
  TestParseFile('TestInterfaceMap', 397);
end;

procedure TTestFileParse.TestInterfaces;
begin
  TestParseFile('TestInterfaces', 648);
end;

procedure TTestFileParse.TestLayout;
begin
  TestParseFile('TestLayout', 1227);
end;

procedure TTestFileParse.TestLayoutBare;
begin
  TestParseFile('TestLayoutBare', 1555);
end;

procedure TTestFileParse.TestLayoutBare2;
begin
  TestParseFile('TestLayoutBare2', 1013);
end;

procedure TTestFileParse.TestLayoutBare3;
begin
  TestParseFile('TestLayoutBare3', 1178);
end;

procedure TTestFileParse.TestLibExports;
begin
  TestParseFile('TestLibExports', 119);
end;

procedure TTestFileParse.TestLineBreaking;
begin
  TestParseFile('TestLineBreaking', 6108);
end;

procedure TTestFileParse.TestLocalTypes;
begin
  TestParseFile('TestLocalTypes', 297);
end;

procedure TTestFileParse.TestLongStrings;
begin
  TestParseFile('TestLongStrings', 163);
end;

procedure TTestFileParse.TestMarcoV;
begin
  TestParseFile('TestMarcoV', 241);
end;

procedure TTestFileParse.TestTestMH;
begin
  TestParseFile('TestMH', 2956);
end;

procedure TTestFileParse.TestMixedModeCaps;
begin
  TestParseFile('TestMixedModeCaps', 123);
end;

procedure TTestFileParse.TestMVB;
begin
  TestParseFile('TestMVB', 835);
end;

procedure TTestFileParse.TestNested;
begin
  TestParseFile('TestNested', 658);
end;

procedure TTestFileParse.TestNestedRecords;
begin
  TestParseFile('TestNestedRecords', 1189);
end;

procedure TTestFileParse.TestOleParams;
begin
  TestParseFile('TestOleParams', 160);
end;

procedure TTestFileParse.TestOperators;
begin
  TestParseFile('TestOperators', 1233);
end;

procedure TTestFileParse.TestParams;
begin
  TestParseFile('TestParams', 218);
end;

procedure TTestFileParse.TestParamSpaces;
begin
  TestParseFile('TestParamSpaces', 159);
end;

procedure TTestFileParse.TestPointers;
begin
  TestParseFile('TestPointers', 193);
end;

procedure TTestFileParse.TestProgram;
begin
  TestParseFile('TestProgram', 1246);
end;

procedure TTestFileParse.TestProperties;
begin
  TestParseFile('TestProperties', 751);
end;

procedure TTestFileParse.TestPropertyLines;
begin
  TestParseFile('TestPropertyLines', 1186);
end;

procedure TTestFileParse.TestRecords;
begin
  TestParseFile('TestRecords', 1455);
end;

procedure TTestFileParse.TestReg;
begin
  TestParseFile('TestReg', 85);
end;

procedure TTestFileParse.TestReint;
begin
  TestParseFile('TestReint', 159);
end;

procedure TTestFileParse.TestReturnRemoval;
begin
  TestParseFile('TestReturnRemoval', 256);
end;

procedure TTestFileParse.TestReturns;
begin
  TestParseFile('TestReturns', 141);
end;

procedure TTestFileParse.TestRunOnConst;
begin
  TestParseFile('TestRunOnConst', 465);
end;

procedure TTestFileParse.TestRunOnDef;
begin
  TestParseFile('TestRunOnDef', 363);
end;

procedure TTestFileParse.TestRunOnLine;
begin
  TestParseFile('TestRunOnLine', 3668);
end;

procedure TTestFileParse.TestTPObjects;
begin
  TestParseFile('TestTPObjects', 126);
end;

procedure TTestFileParse.TestTry;
begin
  TestParseFile('TestTry', 939);
end;

procedure TTestFileParse.TestTypeDefs;
begin
  TestParseFile('TestTypeDefs', 793);
end;

procedure TTestFileParse.TestUses;
begin
  TestParseFile('TestUses', 64);
end;

procedure TTestFileParse.TestUsesChanges;
begin
  TestParseFile('TestUsesChanges', 56);
end;

procedure TTestFileParse.TestWarnings;
begin
  TestParseFile('TestWarnings', 702);
end;

procedure TTestFileParse.TestWith;
begin
  TestParseFile('TestWith', 735);
end;

procedure TTestFileParse.TestCases;
begin
  TestParseFile('Testcases.dpr', 1301);
end;


procedure TTestFileParse.TestPackage;
begin
  TestParseFile('TestMe.dpk', 684);
end;

procedure TTestFileParse.TestProcBlankLines;
begin
  TestParseFile('TestProcBlankLines', 64);
end;

procedure TTestFileParse.TestVarArgs;
begin
  TestParseFile('TestVarArgs', 43);
end;

procedure TTestFileParse.TestVarParam;
begin
  TestParseFile('TestVarParam', 116);
end;


procedure TTestFileParse.LittleTest9;
begin
  TestParseFile('LittleTest9', 69);
end;

procedure TTestFileParse.TestDeref;
begin
  TestParseFile('TestDeref', 584);
end;

procedure TTestFileParse.TestPropertyInherited;
begin
  TestParseFile('TestPropertyInherited', 797);
end;

procedure TTestFileParse.TestMessages;
begin
  TestParseFile('TestMessages', 130);
end;

procedure TTestFileParse.LittleTest10;
begin
  TestParseFile('LittleTest10', 375);
end;

procedure TTestFileParse.TestInheritedExpr;
begin
  TestParseFile('TestInheritedExpr', 301);
end;

procedure TTestFileParse.LittleTest11;
begin
  TestParseFile('LittleTest11', 97);
end;

procedure TTestFileParse.LittleTest12;
begin
  TestParseFile('LittleTest12', 54);
end;

procedure TTestFileParse.LittleTest13;
begin
  TestParseFile('LittleTest13', 86);
end;

procedure TTestFileParse.LittleTest14;
begin
  TestParseFile('LittleTest14', 37);
end;

procedure TTestFileParse.LittleTest15;
begin
  TestParseFile('LittleTest15', 41);
end;

procedure TTestFileParse.LittleTest16;
begin
  TestParseFile('LittleTest16', 102);
end;

procedure TTestFileParse.LittleTest17;
begin
  TestParseFile('LittleTest17', 102);
end;

procedure TTestFileParse.LittleTest18;
begin
  TestParseFile('LittleTest18', 103);
end;

procedure TTestFileParse.TestAtExpr;
begin
  TestParseFile('TestAtExpr', 79);
end;

procedure TTestFileParse.TestAsmStructs;
begin
  TestParseFile('TestAsmStructs', 358);
end;

procedure TTestFileParse.TestUnitAllDirectives;
begin
  TestParseFile('TestUnitAllDirectives', 21);
end;

procedure TTestFileParse.TestUnitDeprecated;
begin
  TestParseFile('TestUnitDeprecated', 17);
end;

procedure TTestFileParse.TestUnitLibrary;
begin
  TestParseFile('TestUnitLibrary', 17);
end;

procedure TTestFileParse.TestUnitPlatform;
begin
  TestParseFile('TestUnitPlatform', 17);
end;

procedure TTestFileParse.LittleTest19;
begin
  TestParseFile('LittleTest19', 168);
end;

procedure TTestFileParse.TestRaise;
begin
  TestParseFile('TestRaise', 519);
end;

procedure TTestFileParse.LittleTest20;
begin
  TestParseFile('LittleTest20', 62);
end;

procedure TTestFileParse.LittleTest21;
begin
  TestParseFile('LittleTest21', 37);
end;

procedure TTestFileParse.LittleTest22;
begin
  TestParseFile('LittleTest22', 53);
end;

procedure TTestFileParse.LittleTest23;
begin
  TestParseFile('LittleTest23', 74);
end;

procedure TTestFileParse.LittleTest24;
begin
  TestParseFile('LittleTest24', 69);
end;

procedure TTestFileParse.LittleTest25;
begin
  TestParseFile('LittleTest25', 42);
end;

procedure TTestFileParse.LittleTest26;
begin
  TestParseFile('LittleTest26', 97);
end;

procedure TTestFileParse.LittleTest27;
begin
  TestParseFile('LittleTest27', 89);
end;

procedure TTestFileParse.TestEmptySquareBrackets;
begin
  TestParseFile('TestEmptySquareBrackets', 66);
end;

procedure TTestFileParse.LittleTest28;
begin
  TestParseFile('LittleTest28', 71);
end;

procedure TTestFileParse.LittleTest29;
begin
  TestParseFile('LittleTest29', 141);
end;

procedure TTestFileParse.LittleTest30;
begin
  TestParseFile('LittleTest30', 60);
end;

procedure TTestFileParse.LittleTest31;
begin
  TestParseFile('LittleTest31', 58);
end;

procedure TTestFileParse.LittleTest32;
begin
  TestParseFile('LittleTest32', 91);
end;

procedure TTestFileParse.LittleTest33;
begin
  TestParseFile('LittleTest33', 45);
end;

procedure TTestFileParse.TestEmptyCase;
begin
  TestParseFile('TestEmptyCase', 128);
end;

procedure TTestFileParse.TestCaseIfFormat;
begin
  TestParseFile('TestCaseIfFormat', 294);
end;

procedure TTestFileParse.LittleTest34;
begin
  TestParseFile('LittleTest34', 78);
end;

procedure TTestFileParse.LittleTest35;
begin
  TestParseFile('LittleTest35', 38);
end;

procedure TTestFileParse.LittleTest36;
begin
  TestParseFile('LittleTest36', 77);
end;

procedure TTestFileParse.LittleTest37;
begin
  TestParseFile('LittleTest37', 76);
end;

procedure TTestFileParse.TestSimpleIfDef;
begin
  TestParseFile('TestSimpleIfDef', 54);
end;

procedure TTestFileParse.TestSimpleIfDef2;
begin
  TestParseFile('TestSimpleIfDef2', 33);
end;

procedure TTestFileParse.TestSimpleIfDef3;
begin
  TestParseFile('TestSimpleIfDef3', 83);
end;

procedure TTestFileParse.TestSimpleIfDef4;
begin
  TestParseFile('TestSimpleIfDef4', 92);
end;

procedure TTestFileParse.TestSimpleIfDef5;
begin
  TestParseFile('TestSimpleIfDef5', 39);
end;

procedure TTestFileParse.LittleTest38;
begin
  TestParseFile('LittleTest38', 53);
end;

procedure TTestFileParse.LittleTest39;
begin
  TestParseFile('LittleTest39', 185);
end;

procedure TTestFileParse.LittleTest40;
begin
  TestParseFile('LittleTest40', 143);
end;

procedure TTestFileParse.TestDefines;
begin
  TestParseFile('TestDefines', 262);
end;

procedure TTestFileParse.LittleTest41;
begin
  TestParseFile('LittleTest41', 47);
end;

procedure TTestFileParse.LittleTest42;
begin
  TestParseFile('LittleTest42', 112);
end;

procedure TTestFileParse.LittleTest43;
begin
  TestParseFile('LittleTest43', 413);
end;

procedure TTestFileParse.LittleTest44;
begin
  TestParseFile('LittleTest44', 286);
end;

procedure TTestFileParse.LittleTest45;
begin
  TestParseFile('LittleTest45', 177);
end;

procedure TTestFileParse.LittleTest46;
begin
  TestParseFile('LittleTest46', 96);
end;

procedure TTestFileParse.LittleTest47;
begin
  TestParseFile('LittleTest47', 268);
end;

procedure TTestFileParse.TestWarnDestroy;
begin
  TestParseFile('TestWarnDestroy', 117);
end;

procedure TTestFileParse.LittleTest48;
begin
  TestParseFile('LittleTest48', 204);
end;

procedure TTestFileParse.LittleTest49;
begin
  TestParseFile('LittleTest49', 161);
end;

procedure TTestFileParse.LittleTest50;
begin
  TestParseFile('LittleTest50', 135);
end;

procedure TTestFileParse.LittleTest51;
begin
  TestParseFile('LittleTest51', 172);
end;

procedure TTestFileParse.LittleTest52;
begin
  TestParseFile('LittleTest52', 39);
end;

procedure TTestFileParse.TestSimpleIfDef6;
begin
  TestParseFile('TestSimpleIfDef6', 46);
end;

procedure TTestFileParse.LittleTest53;
begin
  TestParseFile('LittleTest53', 60);
end;

procedure TTestFileParse.LittleTest54;
begin
  TestParseFile('LittleTest54', 62);
end;

procedure TTestFileParse.LittleTest55;
begin
  TestParseFile('LittleTest55', 66);
end;

procedure TTestFileParse.LittleTest56;
begin
  TestParseFile('LittleTest56', 51);
end;

procedure TTestFileParse.LittleTest57;
begin
  TestParseFile('LittleTest57', 204);
end;

procedure TTestFileParse.LittleTest58;
begin
  TestParseFile('LittleTest58', 212);
end;

procedure TTestFileParse.LittleTest59;
begin
  TestParseFile('LittleTest59', 61);
end;

procedure TTestFileParse.LittleTest60;
begin
  TestParseFile('LittleTest60', 73);
end;

procedure TTestFileParse.LittleTest61;
begin
  TestParseFile('LittleTest61', 24);
end;

procedure TTestFileParse.LittleTest62;
begin
  TestParseFile('LittleTest62', 49);
end;

procedure TTestFileParse.TestInline;
begin
  TestParseFile('TestInline', 92);
end;

procedure TTestFileParse.fBracketProp;
begin
  TestParseFile('fBracketProp', 492);
end;

procedure TTestFileParse.TestEndElse;
begin
  TestParseFile('TestEndElse', 106);
end;

procedure TTestFileParse.TestCondReturns;
begin
  TestParseFile('TestCondReturns', 92);
end;


procedure TTestFileParse.TestDephiNetUses;
begin
  TestParseFile('TestDelphiNetUses', 146);
end;

procedure TTestFileParse.TestConstBug;
begin
  TestParseFile('TestConstBug', 157);
end;

procedure TTestFileParse.TestForIn;
begin
  TestParseFile('TestForIn', 76);
end;

procedure TTestFileParse.TestDottedName;
begin
  TestParseFile('test.dotted.name.pas', 23);
end;

procedure TTestFileParse.TestDelphiNetClass;
begin
  TestParseFile('TestDelphiNetClass', 148);
end;

procedure TTestFileParse.TestDelphiNetConst;
begin
  TestParseFile('TestDelphiNetConst', 125);
end;

procedure TTestFileParse.TestDelphiNetDottedType;
begin
  TestParseFile('TestDelphiNetDottedType', 245);
end;

procedure TTestFileParse.TestDelphiNetDynamicArray;
begin
  TestParseFile('TestDelphiNetDynamicArray', 797);
end;

procedure TTestFileParse.TestDelphiNetStatic;
begin
  TestParseFile('TestDelphiNetStatic', 324);
end;

procedure TTestFileParse.TestTestDotNetForm1;
begin
  TestParseFile('TestDotNetForm1', 356);
end;

procedure TTestFileParse.TestDelphiNetNestedType;
begin
  TestParseFile('TestDelphiNetNestedType', 117);
end;

procedure TTestFileParse.TestDelphiNetNestedType2;
begin
  TestParseFile('TestDelphiNetNestedType2', 174);
end;

procedure TTestFileParse.TestDelphiNetOperatorOverload;
begin
  TestParseFile('TestDelphiNetOperatorOverload', 252);
end;

procedure TTestFileParse.TestDelphiNetHelperClass;
begin
  TestParseFile('TestDelphiNetHelperClass', 158);
end;

procedure TTestFileParse.TestDelphiNetRecordForward;
begin
  TestParseFile('TestDelphiNetRecordForward', 142);
end;

procedure TTestFileParse.TestDelphiNetRecordProcs;
begin
  TestParseFile('TestDelphiNetRecordProcs', 698);
end;

procedure TTestFileParse.TestCondCompBreaks;
begin
  TestParseFile('TestCondCompBreaks', 89);
end;

procedure TTestFileParse.TestCondCompBreaks2;
begin
  TestParseFile('TestCondCompBreaks2', 88);
end;

procedure TTestFileParse.TestDelphiNetAmpersandMethod;
begin
  TestParseFile('TestDelphiNetAmpersandMethod', 166);
end;

procedure TTestFileParse.TestDelphiNetAttributes;
begin
  TestParseFile('TestDelphiNetAttributes', 247);
end;

procedure TTestFileParse.TestDelphiNetWebService;
begin
  TestParseFile('TestDelphiNetWebService', 356);
end;

procedure TTestFileParse.TestDelphiNetWebService2;
begin
  TestParseFile('TestDelphiNetWebService2', 432);
end;


procedure TTestFileParse.TestDelphiNetKeywords;
begin
  TestParseFile('TestDelphiNetKeywords', 95);
end;

procedure TTestFileParse.TestDelphiNetMulticast;
begin
  TestParseFile('TestDelphiNetMulticast', 65);
end;

procedure TTestFileParse.TestDelphiNetClassVar;
begin
  TestParseFile('TestDelphiNetClassVar', 314);
end;

procedure TTestFileParse.TestTrailingCommaParam;
begin
  TestParseFile('TestTrailingCommaParam', 189);
end;

procedure TTestFileParse.TestTryExceptRaise;
begin
  TestParseFile('TestTryExceptRaise', 155);
end;

procedure TTestFileParse.TestDprNoBegin;
begin
  TestParseFile('TestDprNoBegin.dpr', 168);
end;

procedure TTestFileParse.TestDLLIndex;
begin
  TestParseFile('TestDLLIndex', 120);
end;

procedure TTestFileParse.TestIncAt;
begin
  TestParseFile('TestIncAt', 53);
end;

procedure TTestFileParse.TestDelphiNetFinalMethod;
begin
  TestParseFile('TestDelphiNetFinalMethod', 140);
end;

procedure TTestFileParse.TestDelphiNetSealedClass;
begin
  TestParseFile('TestDelphiNetSealedClass', 200);
end;

procedure TTestFileParse.TestAsmAnd;
begin
  TestParseFile('TestAsmAnd', 142);
end;

procedure TTestFileParse.TestAsmLabel;
begin
  TestParseFile('TestAsmLabel', 52);
end;

initialization
  TestFramework.RegisterTest(TTestFileParse.Suite);
end.
