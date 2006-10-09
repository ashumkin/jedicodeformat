unit TestFullClarify;
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

{ test the full clarify - all processes }


uses
  TestFile;

type
  TFullTestClarify = class(TTestFile)
  private
    procedure TestClarifyFile(const psInFileName, psRefOutput: string;
      const piTokenCount: integer); overload;
    procedure TestClarifyFile(const psName: string; const piTokenCount: integer); overload;

  protected

  published
    { one test for each file}
    procedure EmptyTest1;
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
    procedure TestAsmOps;

    procedure TestBlankLineRemoval;
    procedure TestBogusDirectives;
    procedure TestBogusTypes;
    procedure TestCaseBlock;
    procedure TestCaseIfFormat;
    procedure TestCast;
    procedure TestCastSimple;
    procedure TestCharLiterals;
    procedure TestClassLines;
    procedure TestCommentIndent;
    procedure TestCommentIndent2;
    procedure TestCondReturns;
    procedure TestConstRecords;
    procedure TestD6;
    procedure TestDeclarations2;
    procedure TestDeclarations;
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
    procedure TestLayoutBare2;
    procedure TestLayoutBare3;
    procedure TestLayoutBare;
    procedure TestLibExports;

    procedure TestLineBreaking;
    procedure TestLocalTypes;
    procedure TestLongStrings;
    procedure TestMarcoV;
    procedure TestMessages;
    procedure TestMH;
    procedure TestMixedModeCaps;
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

    procedure TestTPObjects;
    procedure TestTry;
    procedure TestTypeDefs;
    procedure TestUses;
    procedure TestUsesChanges;

    procedure TestUnitAllDirectives;
    procedure TestUnitDeprecated;
    procedure TestUnitLibrary;
    procedure TestUnitPlatform;

    procedure TestVarParam;
    procedure TestWarnings;
    procedure TestWarnDestroy;
    procedure TestWith;

    procedure TestPackage;
    procedure TestProcBlankLines;
    procedure TestCondCompBreaks;
    procedure TestCondCompBreaks2;

    procedure TestDelphiNetUses;
    procedure TestConstBug;
    procedure TestForIn;
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
    procedure TestDelphiNetAttributes;
    procedure TestDelphiNetWebService;
    procedure TestDelphiNetWebService2;
    procedure TestDelphiNetKeywords;
    procedure TestDelphiNetClassVar;
    procedure TestDelphiNetSealedClass;
    procedure TestDelphiNetFinalMethod;
    procedure TestDelphiNetDottedType;
    procedure TestDelphiNetAmpersandMethod;
    procedure TestDelphiNetMulticast;
    procedure TestDelphiNetDynamicArray;
    procedure TestDelphiNetRecordProcs;
    procedure TestDelphiNetUnsafe;

    procedure TestTryExceptRaise;
    procedure TestTrailingCommaParam;
    procedure TestDprNoBegin;
    procedure TestDLLIndex;
    procedure TestIncAt;
    procedure TestAsmAnd;
    procedure TestAsmLabel;
    procedure TestVarArgs;
    procedure TestLabelKeyword;

    procedure TestCases;
  end;


implementation

uses
  { delphi }
  SysUtils, Windows,
  { jcl }
  JclStrings,
  { DUnit}
  TestFrameWork,
  { JCF }
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;

{ TFullTestClarify }


procedure TFullTestClarify.TestClarifyFile(const psName: string;
  const piTokenCount: integer);
var
  liLastDotPos: integer;
  lsInName, lsClearFileName: string;
begin
  Assert(psName <> '');

  { does it have an file extension? }
  liLastDotPos := StrLastPos('.', psName);
  if liLastDotPos > 0 then
  begin
    lsInName := psName;
    lsClearFileName := StrLeft(psName, liLastDotPos) + 'out';
  end
  else
  begin
    lsInName := psName + '.pas';
    lsClearFileName := psName + '.out';
  end;


  TestClarifyFile(GetTestFilesDir + lsInName,
    GetRefOutFilesDir + lsClearFileName, piTokenCount)
end;

procedure TFullTestClarify.TestClarifyFile(const psInFileName, psRefOutput: string;
  const piTokenCount: integer);
var
  lcConverter:   TFileConverter;
  lsOutFileName: string;
  //lsOutFileName2: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  FormatSettings.Obfuscate.Enabled := False;

  // Check(FileExists(psRefOutput), 'reference output file ' + psRefOutput + ' not found');

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll      := True;
    lcConverter.GuiMessages := False;

    { see also TestFileParse }
    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeperateOutput;

    GetRegSettings.OutputExtension := 'out';
    lcConverter.Input := psInFileName;
    lcConverter.Convert;

    Check( not lcConverter.ConvertError, 'Convert failed for ' +
      ExtractFileName(psInFileName));

    lsOutFileName := lcConverter.OutFileName;
    Check(lsOutFileName <> '', 'No output file');
    Check(FileExists(lsOutFileName), 'output file ' + lsOutFileName + ' not found');

    CheckEquals(piTokenCount, lcConverter.TokenCount, 'wrong number of tokens');

    TestFileContentsSame(lsOutFileName, psRefOutput);

    {
    // do it again - should the the same after a second round
    GetRegSettings.OutputExtension := 'out2';

    lcConverter.Input := lsOutFileName;
    lcConverter.Convert;

    Check( not lcConverter.ConvertError, 'Convert failed for ' +
      ExtractFileName(lsOutFileName));

    lsOutFileName2 := lcConverter.OutFileName;
    Check(lsOutFileName2 <> '', 'No output file');
    Check(FileExists(lsOutFileName2), 'output file ' + lsOutFileName + ' not found');

    // formatting twice should be the same as formatting once
    TestFileContentsSame(lsOutFileName, lsOutFileName2);
    }

    // clean up
    SysUtils.DeleteFile(lsOutFileName);
    //SysUtils.DeleteFile(lsOutFileName2);

  finally
    lcConverter.Free;
    FormatSettings.Obfuscate.Enabled := False;
  end;

end;



procedure TFullTestClarify.EmptyTest1;
begin
  TestClarifyFile('EmptyTest1', 18);
end;

procedure TFullTestClarify.fFormTest;
begin
  TestClarifyFile('fFormTest', 150);
end;

procedure TFullTestClarify.LittleTest1;
begin
  TestClarifyFile('LittleTest1', 28);
end;

procedure TFullTestClarify.LittleTest2;
begin
  TestClarifyFile('LittleTest2', 29);
end;

procedure TFullTestClarify.LittleTest3;
begin
  TestClarifyFile('LittleTest3', 42);
end;

procedure TFullTestClarify.LittleTest4;
begin
  TestClarifyFile('LittleTest4', 45);
end;

procedure TFullTestClarify.LittleTest5;
begin
  TestClarifyFile('LittleTest5', 58);
end;

procedure TFullTestClarify.LittleTest6;
begin
  TestClarifyFile('LittleTest6', 78);
end;

procedure TFullTestClarify.LittleTest7;
begin
  TestClarifyFile('LittleTest7', 109);
end;

procedure TFullTestClarify.LittleTest8;
begin
  TestClarifyFile('LittleTest8', 41);
end;

procedure TFullTestClarify.LittleTest9;
begin
  TestClarifyFile('LittleTest9', 69);
end;

procedure TFullTestClarify.LittleTest10;
begin
  TestClarifyFile('LittleTest10', 375);
end;

procedure TFullTestClarify.LittleTest11;
begin
  TestClarifyFile('LittleTest11', 97);
end;


procedure TFullTestClarify.TestAbsolute;
begin
  TestClarifyFile('TestAbsolute', 86);
end;

procedure TFullTestClarify.TestAlign;
begin
  TestClarifyFile('TestAlign', 662);
end;

procedure TFullTestClarify.TestArray;
begin
  TestClarifyFile('TestArray', 220);
end;

procedure TFullTestClarify.TestAsm;
begin
  TestClarifyFile('TestAsm', 828);
end;


procedure TFullTestClarify.TestBlankLineRemoval;
begin
  TestClarifyFile('TestBlankLineRemoval', 373);
end;

procedure TFullTestClarify.TestBogusDirectives;
begin
  TestClarifyFile('TestBogusDirectives', 456);
end;

procedure TFullTestClarify.TestBogusTypes;
begin
  TestClarifyFile('TestBogusTypes', 230);
end;

procedure TFullTestClarify.TestCaseBlock;
begin
  TestClarifyFile('TestCaseBlock', 3041);
end;

procedure TFullTestClarify.TestCases;
begin
  TestClarifyFile('TestCases.dpr', 1309);
end;

procedure TFullTestClarify.TestCast;
begin
  TestClarifyFile('TestCast', 654);
end;

procedure TFullTestClarify.TestCastSimple;
begin
  TestClarifyFile('TestCastSimple', 843);
end;

procedure TFullTestClarify.TestCharLiterals;
begin
  TestClarifyFile('TestCharLiterals', 1035);
end;

procedure TFullTestClarify.TestClassLines;
begin
  TestClarifyFile('TestClassLines', 71);
end;

procedure TFullTestClarify.TestCommentIndent;
begin
  TestClarifyFile('TestCommentIndent', 549);
end;

procedure TFullTestClarify.TestCommentIndent2;
begin
  TestClarifyFile('TestCommentIndent2', 361);
end;

procedure TFullTestClarify.TestConstRecords;
begin
  TestClarifyFile('TestConstRecords', 945);
end;

procedure TFullTestClarify.TestD6;
begin
  TestClarifyFile('TestD6', 959);
end;

procedure TFullTestClarify.TestDeclarations;
begin
  TestClarifyFile('TestDeclarations', 1004);
end;

procedure TFullTestClarify.TestDeclarations2;
begin
  TestClarifyFile('TestDeclarations2', 362);
end;

procedure TFullTestClarify.TestDefaultParams;
begin
  TestClarifyFile('TestDefaultParams', 698);
end;

procedure TFullTestClarify.TestDeref;
begin
  TestClarifyFile('TestDeref', 584);
end;

procedure TFullTestClarify.TestEmptyClass;
begin
  TestClarifyFile('TestEmptyClass', 244);
end;

procedure TFullTestClarify.TestEsotericKeywords;
begin
  TestClarifyFile('TestEsotericKeywords', 258);
end;

procedure TFullTestClarify.TestExclusion;
begin
  TestClarifyFile('TestExclusion', 431);
end;

procedure TFullTestClarify.TestExclusionFlags;
begin
  TestClarifyFile('TestExclusionFlags', 723);
end;

procedure TFullTestClarify.TestExternal;
begin
  TestClarifyFile('TestExternal', 259);
end;

procedure TFullTestClarify.TestForward;
begin
  TestClarifyFile('TestForward', 332);
end;

procedure TFullTestClarify.TestGoto;
begin
  TestClarifyFile('TestGoto', 503);
end;

procedure TFullTestClarify.TestInheritedExpr;
begin
  TestClarifyFile('TestInheritedExpr', 301);
end;

procedure TFullTestClarify.TestInitFinal;
begin
  TestClarifyFile('TestInitFinal', 170);
end;

procedure TFullTestClarify.TestInterfaceImplements;
begin
  TestClarifyFile('TestInterfaceImplements', 225);
end;

procedure TFullTestClarify.TestInterfaceMap;
begin
  TestClarifyFile('TestInterfaceMap', 397);
end;

procedure TFullTestClarify.TestInterfaces;
begin
  TestClarifyFile('TestInterfaces', 648);
end;

procedure TFullTestClarify.TestLabelKeyword;
begin
  TestClarifyFile('TestLabelKeyword', 60);
end;

procedure TFullTestClarify.TestLayout;
begin
  TestClarifyFile('TestLayout', 1227);
end;

procedure TFullTestClarify.TestLayoutBare;
begin
  TestClarifyFile('TestLayoutBare', 1555);
end;

procedure TFullTestClarify.TestLayoutBare2;
begin
  TestClarifyFile('TestLayoutBare2', 1013);
end;

procedure TFullTestClarify.TestLayoutBare3;
begin
  TestClarifyFile('TestLayoutBare3', 1178);
end;

procedure TFullTestClarify.TestLibExports;
begin
  TestClarifyFile('TestLibExports', 119);
end;

procedure TFullTestClarify.TestLineBreaking;
begin
  TestClarifyFile('TestLineBreaking', 6108);
end;

procedure TFullTestClarify.TestLocalTypes;
begin
  TestClarifyFile('TestLocalTypes', 297);
end;

procedure TFullTestClarify.TestLongStrings;
begin
  TestClarifyFile('TestLongStrings', 163);
end;

procedure TFullTestClarify.TestMarcoV;
begin
  TestClarifyFile('TestMarcoV', 241);
end;

procedure TFullTestClarify.TestMessages;
begin
  TestClarifyFile('TestMessages',130);
end;

procedure TFullTestClarify.TestMH;
begin
  TestClarifyFile('TestMH', 2956);
end;

procedure TFullTestClarify.TestMixedModeCaps;
begin
  TestClarifyFile('TestMixedModeCaps', 123);
end;

procedure TFullTestClarify.TestMVB;
begin
  TestClarifyFile('TestMVB', 835);
end;

procedure TFullTestClarify.TestNested;
begin
  TestClarifyFile('TestNested', 658);
end;

procedure TFullTestClarify.TestNestedRecords;
begin
  TestClarifyFile('TestNestedRecords', 1189);
end;

procedure TFullTestClarify.TestOperators;
begin
  TestClarifyFile('TestOperators', 1233);
end;

procedure TFullTestClarify.TestOleParams;
begin
  TestClarifyFile('TestOleParams', 160);
end;

procedure TFullTestClarify.TestPackage;
begin
  TestClarifyFile('TestMe.dpk', 684);
end;

procedure TFullTestClarify.TestParams;
begin
  TestClarifyFile('TestParams', 218);
end;

procedure TFullTestClarify.TestParamSpaces;
begin
  TestClarifyFile('TestParamSpaces', 159);
end;

procedure TFullTestClarify.TestPointers;
begin
  TestClarifyFile('TestPointers', 193);
end;

procedure TFullTestClarify.TestProcBlankLines;
begin
  TestClarifyFile('TestProcBlankLines', 64);
end;

procedure TFullTestClarify.TestProgram;
begin
  TestClarifyFile('TestProgram', 1246);
end;

procedure TFullTestClarify.TestProperties;
begin
  TestClarifyFile('TestProperties', 751);
end;

procedure TFullTestClarify.TestPropertyInherited;
begin
  TestClarifyFile('TestPropertyInherited', 797);
end;

procedure TFullTestClarify.TestPropertyLines;
begin
  TestClarifyFile('TestPropertyLines', 1186);
end;

procedure TFullTestClarify.TestRecords;
begin
  TestClarifyFile('TestRecords', 1455);
end;

procedure TFullTestClarify.TestReg;
begin
  TestClarifyFile('TestReg', 85);
end;

procedure TFullTestClarify.TestReint;
begin
  TestClarifyFile('TestReint', 159);
end;

procedure TFullTestClarify.TestReturnRemoval;
begin
  TestClarifyFile('TestReturnRemoval', 256);
end;

procedure TFullTestClarify.TestReturns;
begin
  TestClarifyFile('TestReturns', 141);
end;

procedure TFullTestClarify.TestRunOnConst;
begin
  TestClarifyFile('TestRunOnConst', 465);
end;

procedure TFullTestClarify.TestRunOnDef;
begin
  TestClarifyFile('TestRunOnDef', 363);
end;

procedure TFullTestClarify.TestRunOnLine;
begin
  TestClarifyFile('TestRunOnLine', 3668);
end;

procedure TFullTestClarify.TestTPObjects;
begin
  TestClarifyFile('TestTPObjects', 126);
end;

procedure TFullTestClarify.TestTry;
begin
  TestClarifyFile('TestTry', 939);
end;

procedure TFullTestClarify.TestTypeDefs;
begin
  TestClarifyFile('TestTypeDefs', 793);
end;

procedure TFullTestClarify.TestUses;
begin
  TestClarifyFile('TestUses', 64);
end;

procedure TFullTestClarify.TestUsesChanges;
begin
  TestClarifyFile('TestUsesChanges', 56);
end;

procedure TFullTestClarify.TestVarArgs;
begin
  TestClarifyFile('TestVarArgs', 43);
end;

procedure TFullTestClarify.TestVarParam;
begin
  TestClarifyFile('TestVarParam', 116);
end;

procedure TFullTestClarify.TestWarnings;
begin
  TestClarifyFile('TestWarnings', 702);
end;

procedure TFullTestClarify.TestWith;
begin
  TestClarifyFile('TestWith', 735);
end;

procedure TFullTestClarify.LittleTest12;
begin
  TestClarifyFile('LittleTest12', 54);
end;

procedure TFullTestClarify.LittleTest13;
begin
  TestClarifyFile('LittleTest13', 86);
end;

procedure TFullTestClarify.LittleTest14;
begin
  TestClarifyFile('LittleTest14', 37);
end;

procedure TFullTestClarify.LittleTest15;
begin
  TestClarifyFile('LittleTest15', 41);
end;

procedure TFullTestClarify.LittleTest16;
begin
  TestClarifyFile('LittleTest16', 102);
end;

procedure TFullTestClarify.LittleTest17;
begin
  TestClarifyFile('LittleTest17', 102);
end;

procedure TFullTestClarify.LittleTest18;
begin
  TestClarifyFile('LittleTest18', 103);
end;

procedure TFullTestClarify.TestAtExpr;
begin
  TestClarifyFile('TestAtExpr', 79);
end;

procedure TFullTestClarify.TestAsmOps;
begin
  TestClarifyFile('TestAsmOps', 93);
end;

procedure TFullTestClarify.TestAsmStructs;
begin
  TestClarifyFile('TestAsmStructs', 358);
end;

procedure TFullTestClarify.TestUnitAllDirectives;
begin
  TestClarifyFile('TestUnitAllDirectives', 21);
end;

procedure TFullTestClarify.TestUnitDeprecated;
begin
  TestClarifyFile('TestUnitDeprecated', 17);
end;

procedure TFullTestClarify.TestUnitLibrary;
begin
  TestClarifyFile('TestUnitLibrary', 17);
end;

procedure TFullTestClarify.TestUnitPlatform;
begin
  TestClarifyFile('TestUnitPlatform', 17);
end;

procedure TFullTestClarify.LittleTest19;
begin
  TestClarifyFile('LittleTest19', 168);
end;

procedure TFullTestClarify.TestRaise;
begin
  TestClarifyFile('TestRaise', 519);
end;

procedure TFullTestClarify.LittleTest20;
begin
  TestClarifyFile('LittleTest20', 62);
end;

procedure TFullTestClarify.LittleTest21;
begin
  TestClarifyFile('LittleTest21', 37);
end;

procedure TFullTestClarify.LittleTest22;
begin
  TestClarifyFile('LittleTest22', 53);
end;

procedure TFullTestClarify.LittleTest23;
begin
  TestClarifyFile('LittleTest23', 74);
end;

procedure TFullTestClarify.LittleTest24;
begin
  TestClarifyFile('LittleTest24', 69);
end;

procedure TFullTestClarify.LittleTest25;
begin
  TestClarifyFile('LittleTest25', 42);
end;

procedure TFullTestClarify.LittleTest26;
begin
  TestClarifyFile('LittleTest26', 97);
end;

procedure TFullTestClarify.LittleTest27;
begin
  TestClarifyFile('LittleTest27', 89);
end;

procedure TFullTestClarify.TestEmptySquareBrackets;
begin
  TestClarifyFile('TestEmptySquareBrackets', 66);
end;

procedure TFullTestClarify.LittleTest28;
begin
  TestClarifyFile('LittleTest28', 71);
end;

procedure TFullTestClarify.LittleTest29;
begin
  TestClarifyFile('LittleTest29', 141);
end;

procedure TFullTestClarify.LittleTest30;
begin
  TestClarifyFile('LittleTest30', 60);
end;

procedure TFullTestClarify.LittleTest31;
begin
  TestClarifyFile('LittleTest31', 58);
end;

procedure TFullTestClarify.LittleTest32;
begin
  TestClarifyFile('LittleTest32', 91);
end;

procedure TFullTestClarify.LittleTest33;
begin
  TestClarifyFile('LittleTest33', 45);
end;

procedure TFullTestClarify.TestEmptyCase;
begin
  TestClarifyFile('TestEmptyCase', 128);
end;

procedure TFullTestClarify.TestCaseIfFormat;
begin
  TestClarifyFile('TestCaseIfFormat', 294);
end;

procedure TFullTestClarify.LittleTest34;
begin
  TestClarifyFile('LittleTest34', 78);
end;

procedure TFullTestClarify.LittleTest35;
begin
  TestClarifyFile('LittleTest35', 38);
end;

procedure TFullTestClarify.LittleTest36;
begin
  TestClarifyFile('LittleTest36', 77);
end;

procedure TFullTestClarify.LittleTest37;
begin
  TestClarifyFile('LittleTest37', 76);
end;

procedure TFullTestClarify.TestSimpleIfDef;
begin
  TestClarifyFile('TestSimpleIfDef', 54);
end;

procedure TFullTestClarify.TestSimpleIfDef2;
begin
  TestClarifyFile('TestSimpleIfDef2', 33);
end;

procedure TFullTestClarify.TestSimpleIfDef3;
begin
  TestClarifyFile('TestSimpleIfDef3', 83);
end;

procedure TFullTestClarify.TestSimpleIfDef4;
begin
  TestClarifyFile('TestSimpleIfDef4', 92);
end;

procedure TFullTestClarify.TestSimpleIfDef5;
begin
  TestClarifyFile('TestSimpleIfDef5', 39);
end;

procedure TFullTestClarify.LittleTest38;
begin
  TestClarifyFile('LittleTest38', 53);
end;

procedure TFullTestClarify.LittleTest39;
begin
  TestClarifyFile('LittleTest39', 185);
end;

procedure TFullTestClarify.LittleTest40;
begin
  TestClarifyFile('LittleTest40', 143);
end;

procedure TFullTestClarify.TestDefines;
begin
  TestClarifyFile('TestDefines', 262);
end;

procedure TFullTestClarify.LittleTest41;
begin
  TestClarifyFile('LittleTest41', 47);
end;

procedure TFullTestClarify.LittleTest42;
begin
  TestClarifyFile('LittleTest42', 112);
end;

procedure TFullTestClarify.LittleTest43;
begin
  TestClarifyFile('LittleTest43', 413);
end;

procedure TFullTestClarify.LittleTest44;
begin
  TestClarifyFile('LittleTest44', 286);
end;

procedure TFullTestClarify.LittleTest45;
begin
  TestClarifyFile('LittleTest45', 177);
end;

procedure TFullTestClarify.LittleTest46;
begin
  TestClarifyFile('LittleTest46', 96);
end;

procedure TFullTestClarify.LittleTest47;
begin
  TestClarifyFile('LittleTest47', 268);
end;

procedure TFullTestClarify.TestWarnDestroy;
begin
  TestClarifyFile('TestWarnDestroy', 117);
end;

procedure TFullTestClarify.LittleTest48;
begin
  TestClarifyFile('LittleTest48', 204);
end;

procedure TFullTestClarify.LittleTest49;
begin
  TestClarifyFile('LittleTest49', 161);
end;

procedure TFullTestClarify.LittleTest50;
begin
  TestClarifyFile('LittleTest50', 135);
end;

procedure TFullTestClarify.LittleTest51;
begin
  TestClarifyFile('LittleTest51', 172);
end;

procedure TFullTestClarify.LittleTest52;
begin
  TestClarifyFile('LittleTest52', 39);
end;

procedure TFullTestClarify.TestSimpleIfDef6;
begin
  TestClarifyFile('TestSimpleIfDef6', 46);
end;

procedure TFullTestClarify.LittleTest53;
begin
  TestClarifyFile('LittleTest53', 60);
end;

procedure TFullTestClarify.LittleTest54;
begin
  TestClarifyFile('LittleTest54', 62);
end;

procedure TFullTestClarify.LittleTest55;
begin
  TestClarifyFile('LittleTest55', 66);
end;

procedure TFullTestClarify.LittleTest56;
begin
  TestClarifyFile('LittleTest56', 51);
end;

procedure TFullTestClarify.LittleTest57;
begin
  TestClarifyFile('LittleTest57', 204);
end;

procedure TFullTestClarify.LittleTest58;
begin
  TestClarifyFile('LittleTest58', 212);
end;

procedure TFullTestClarify.LittleTest59;
begin
  TestClarifyFile('LittleTest59', 61);
end;

procedure TFullTestClarify.LittleTest60;
begin
  TestClarifyFile('LittleTest60', 73);
end;

procedure TFullTestClarify.LittleTest61;
begin
  TestClarifyFile('LittleTest61', 24);
end;

procedure TFullTestClarify.LittleTest62;
begin
  TestClarifyFile('LittleTest62', 49);
end;

procedure TFullTestClarify.TestInline;
begin
  TestClarifyFile('TestInline', 92);
end;

procedure TFullTestClarify.fBracketProp;
begin
  TestClarifyFile('fBracketProp', 492);
end;

procedure TFullTestClarify.TestEndElse;
begin
  TestClarifyFile('TestEndElse', 106);
end;

procedure TFullTestClarify.TestCondReturns;
begin
  TestClarifyFile('TestCondReturns', 92);
end;

procedure TFullTestClarify.TestDelphiNetUnsafe;
begin
  TestClarifyFile('TestDelphiNetUnsafe', 74);
end;

procedure TFullTestClarify.TestDelphiNetUses;
begin
  TestClarifyFile('TestDelphiNetUses', 146);
end;

procedure TFullTestClarify.TestConstBug;
begin
  TestClarifyFile('TestConstBug', 157);
end;

procedure TFullTestClarify.TestForIn;
begin
  TestClarifyFile('TestForIn', 76);
end;

procedure TFullTestClarify.TestDottedName;
begin
  TestClarifyFile('test.dotted.name.pas', 23);
end;

procedure TFullTestClarify.TestDelphiNetClass;
begin
  TestClarifyFile('TestDelphiNetClass', 148);
end;

procedure TFullTestClarify.TestDelphiNetConst;
begin
  TestClarifyFile('TestDelphiNetConst', 206);
end;

procedure TFullTestClarify.TestDelphiNetDottedType;
begin
  TestClarifyFile('TestDelphiNetDottedType', 245);
end;

procedure TFullTestClarify.TestDelphiNetDynamicArray;
begin
  TestClarifyFile('TestDelphiNetDynamicArray', 797);
end;

procedure TFullTestClarify.TestDelphiNetStatic;
begin
  TestClarifyFile('TestDelphiNetStatic', 324);
end;

procedure TFullTestClarify.TestTestDotNetForm1;
begin
  TestClarifyFile('TestDotNetForm1', 356);
end;

procedure TFullTestClarify.TestDelphiNetNestedType;
begin
  TestClarifyFile('TestDelphiNetNestedType', 117);
end;

procedure TFullTestClarify.TestDelphiNetNestedType2;
begin
  TestClarifyFile('TestDelphiNetNestedType2', 174);

end;

procedure TFullTestClarify.TestDelphiNetOperatorOverload;
begin
  TestClarifyFile('TestDelphiNetOperatorOverload', 252);
end;

procedure TFullTestClarify.TestDelphiNetHelperClass;
begin
  TestClarifyFile('TestDelphiNetHelperClass', 158);
end;

procedure TFullTestClarify.TestDelphiNetRecordForward;
begin
  TestClarifyFile('TestDelphiNetRecordForward', 142);
end;

procedure TFullTestClarify.TestDelphiNetRecordProcs;
begin
  TestClarifyFile('TestDelphiNetRecordProcs', 698);
end;

procedure TFullTestClarify.TestCondCompBreaks;
begin
  TestClarifyFile('TestCondCompBreaks', 89);
end;

procedure TFullTestClarify.TestCondCompBreaks2;
begin
  TestClarifyFile('TestCondCompBreaks2', 88);
end;

procedure TFullTestClarify.TestDelphiNetAmpersandMethod;
begin
  TestClarifyFile('TestDelphiNetAmpersandMethod', 166);
end;

procedure TFullTestClarify.TestDelphiNetAttributes;
begin
  TestClarifyFile('TestDelphiNetAttributes', 247);
end;

procedure TFullTestClarify.TestDelphiNetWebService;
begin
  TestClarifyFile('TestDelphiNetWebService', 356);
end;

procedure TFullTestClarify.TestDelphiNetWebService2;
begin
  TestClarifyFile('TestDelphiNetWebService2', 432);
end;

procedure TFullTestClarify.TestDelphiNetKeywords;
begin
  TestClarifyFile('TestDelphiNetKeywords', 95);
end;

procedure TFullTestClarify.TestDelphiNetMulticast;
begin
  TestClarifyFile('TestDelphiNetMulticast', 65);
end;

procedure TFullTestClarify.TestDelphiNetClassVar;
begin
  TestClarifyFile('TestDelphiNetClassVar', 314);
end;

procedure TFullTestClarify.TestTrailingCommaParam;
begin
  TestClarifyFile('TestTrailingCommaParam', 189);
end;

procedure TFullTestClarify.TestTryExceptRaise;
begin
  TestClarifyFile('TestTryExceptRaise', 155);
end;

procedure TFullTestClarify.TestDprNoBegin;
begin
  TestClarifyFile('TestDprNoBegin.dpr', 168);
end;

procedure TFullTestClarify.TestDLLIndex;
begin
  TestClarifyFile('TestDLLIndex', 120);
end;

procedure TFullTestClarify.TestIncAt;
begin
  TestClarifyFile('TestIncAt', 53);
end;

procedure TFullTestClarify.TestDelphiNetFinalMethod;
begin
  TestClarifyFile('TestDelphiNetFinalMethod', 140);
end;

procedure TFullTestClarify.TestDelphiNetSealedClass;
begin
  TestClarifyFile('TestDelphiNetSealedClass', 200);
end;

procedure TFullTestClarify.TestAsmAnd;
begin
  TestClarifyFile('TestAsmAnd', 142);
end;

procedure TFullTestClarify.TestAsmLabel;
begin
  TestClarifyFile('TestAsmLabel', 52);
end;

initialization
  TestFramework.RegisterTest(TFullTestClarify.Suite);
end.
