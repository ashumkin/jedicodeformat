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
    procedure TestClarifyFile(const psInFileName, psRefOutput: string); overload;
    procedure TestClarifyFile(const psName: string); overload;

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
    procedure TestDelphiNetKeywords;
    procedure TestDelphiNetClassVar;

    procedure TestTryExceptRaise;
    procedure TestTrailingCommaParam;
    procedure TestDprNoBegin;
    procedure TestDLLIndex;

    procedure TestCases;
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


procedure TFullTestClarify.TestClarifyFile(const psName: string);
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
    GetRefOutFilesDir + lsClearFileName)
end;

procedure TFullTestClarify.TestClarifyFile(const psInFileName, psRefOutput: string);
var
  lcConverter:   TFileConverter;
  lsOutFileName: string;
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

    TestFileContentsSame(lsOutFileName, psRefOutput);

    // clean up
    DeleteFile(lsOutFileName);

  finally
    lcConverter.Free;
    FormatSettings.Obfuscate.Enabled := False;
  end;

end;



procedure TFullTestClarify.EmptyTest1;
begin
  TestClarifyFile('EmptyTest1');
end;

procedure TFullTestClarify.fFormTest;
begin
  TestClarifyFile('fFormTest');
end;

procedure TFullTestClarify.LittleTest1;
begin
  TestClarifyFile('LittleTest1');
end;

procedure TFullTestClarify.LittleTest10;
begin
  TestClarifyFile('LittleTest10');
end;

procedure TFullTestClarify.LittleTest11;
begin
  TestClarifyFile('LittleTest11');
end;

procedure TFullTestClarify.LittleTest2;
begin
  TestClarifyFile('LittleTest2');
end;

procedure TFullTestClarify.LittleTest3;
begin
  TestClarifyFile('LittleTest3');
end;

procedure TFullTestClarify.LittleTest4;
begin
  TestClarifyFile('LittleTest4');
end;

procedure TFullTestClarify.LittleTest5;
begin
  TestClarifyFile('LittleTest5');
end;

procedure TFullTestClarify.LittleTest6;
begin
  TestClarifyFile('LittleTest6');
end;

procedure TFullTestClarify.LittleTest7;
begin
  TestClarifyFile('LittleTest7');
end;

procedure TFullTestClarify.LittleTest8;
begin
  TestClarifyFile('LittleTest8');
end;

procedure TFullTestClarify.LittleTest9;
begin
  TestClarifyFile('LittleTest9');
end;

procedure TFullTestClarify.TestAbsolute;
begin
  TestClarifyFile('TestAbsolute');
end;

procedure TFullTestClarify.TestAlign;
begin
  TestClarifyFile('TestAlign');
end;

procedure TFullTestClarify.TestArray;
begin
  TestClarifyFile('TestArray');
end;

procedure TFullTestClarify.TestAsm;
begin
  TestClarifyFile('TestAsm');
end;


procedure TFullTestClarify.TestBlankLineRemoval;
begin
  TestClarifyFile('TestBlankLineRemoval');
end;

procedure TFullTestClarify.TestBogusDirectives;
begin
  TestClarifyFile('TestBogusDirectives');
end;

procedure TFullTestClarify.TestBogusTypes;
begin
  TestClarifyFile('TestBogusTypes');
end;

procedure TFullTestClarify.TestCaseBlock;
begin
  TestClarifyFile('TestCaseBlock');
end;

procedure TFullTestClarify.TestCases;
begin
  TestClarifyFile('TestCases.dpr');
end;

procedure TFullTestClarify.TestCast;
begin
  TestClarifyFile('TestCast');
end;

procedure TFullTestClarify.TestCastSimple;
begin
  TestClarifyFile('TestCastSimple');
end;

procedure TFullTestClarify.TestCharLiterals;
begin
  TestClarifyFile('TestCharLiterals');
end;

procedure TFullTestClarify.TestClassLines;
begin
  TestClarifyFile('TestClassLines');
end;

procedure TFullTestClarify.TestCommentIndent;
begin
  TestClarifyFile('TestCommentIndent');
end;

procedure TFullTestClarify.TestCommentIndent2;
begin
  TestClarifyFile('TestCommentIndent2');
end;

procedure TFullTestClarify.TestConstRecords;
begin
  TestClarifyFile('TestConstRecords');
end;

procedure TFullTestClarify.TestD6;
begin
  TestClarifyFile('TestD6');
end;

procedure TFullTestClarify.TestDeclarations;
begin
  TestClarifyFile('TestDeclarations');
end;

procedure TFullTestClarify.TestDeclarations2;
begin
  TestClarifyFile('TestDeclarations2');
end;

procedure TFullTestClarify.TestDefaultParams;
begin
  TestClarifyFile('TestDefaultParams');
end;

procedure TFullTestClarify.TestDeref;
begin
  TestClarifyFile('TestDeref');
end;

procedure TFullTestClarify.TestEmptyClass;
begin
  TestClarifyFile('TestEmptyClass');
end;

procedure TFullTestClarify.TestEsotericKeywords;
begin
  TestClarifyFile('TestEsotericKeywords');
end;

procedure TFullTestClarify.TestExclusion;
begin
  TestClarifyFile('TestExclusion');
end;

procedure TFullTestClarify.TestExclusionFlags;
begin
  TestClarifyFile('TestExclusionFlags');
end;

procedure TFullTestClarify.TestExternal;
begin
  TestClarifyFile('TestExternal');
end;

procedure TFullTestClarify.TestForward;
begin
  TestClarifyFile('TestForward');
end;

procedure TFullTestClarify.TestGoto;
begin
  TestClarifyFile('TestGoto');
end;

procedure TFullTestClarify.TestInheritedExpr;
begin
  TestClarifyFile('TestInheritedExpr');
end;

procedure TFullTestClarify.TestInitFinal;
begin
  TestClarifyFile('TestInitFinal');
end;

procedure TFullTestClarify.TestInterfaceImplements;
begin
  TestClarifyFile('TestInterfaceImplements');
end;

procedure TFullTestClarify.TestInterfaceMap;
begin
  TestClarifyFile('TestInterfaceMap');
end;

procedure TFullTestClarify.TestInterfaces;
begin
  TestClarifyFile('TestInterfaces');
end;

procedure TFullTestClarify.TestLayout;
begin
  TestClarifyFile('TestLayout');
end;

procedure TFullTestClarify.TestLayoutBare;
begin
  TestClarifyFile('TestLayoutBare');
end;

procedure TFullTestClarify.TestLayoutBare2;
begin
  TestClarifyFile('TestLayoutBare2');
end;

procedure TFullTestClarify.TestLayoutBare3;
begin
  TestClarifyFile('TestLayoutBare3');
end;

procedure TFullTestClarify.TestLibExports;
begin
  TestClarifyFile('TestLibExports');
end;

procedure TFullTestClarify.TestLineBreaking;
begin
  TestClarifyFile('TestLineBreaking');
end;

procedure TFullTestClarify.TestLocalTypes;
begin
  TestClarifyFile('TestLocalTypes');
end;

procedure TFullTestClarify.TestLongStrings;
begin
  TestClarifyFile('TestLongStrings');
end;

procedure TFullTestClarify.TestMarcoV;
begin
  TestClarifyFile('TestMarcoV');
end;

procedure TFullTestClarify.TestMessages;
begin
  TestClarifyFile('TestMessages');
end;

procedure TFullTestClarify.TestMH;
begin
  TestClarifyFile('TestMH');
end;

procedure TFullTestClarify.TestMixedModeCaps;
begin
  TestClarifyFile('TestMixedModeCaps');
end;

procedure TFullTestClarify.TestMVB;
begin
  TestClarifyFile('TestMVB');
end;

procedure TFullTestClarify.TestNested;
begin
  TestClarifyFile('TestNested');
end;

procedure TFullTestClarify.TestNestedRecords;
begin
  TestClarifyFile('TestNestedRecords');
end;

procedure TFullTestClarify.TestOperators;
begin
  TestClarifyFile('TestOperators');
end;

procedure TFullTestClarify.TestOleParams;
begin
  TestClarifyFile('TestOleParams');
end;

procedure TFullTestClarify.TestPackage;
begin
  TestClarifyFile('TestMe.dpk');
end;

procedure TFullTestClarify.TestParams;
begin
  TestClarifyFile('TestParams');
end;

procedure TFullTestClarify.TestParamSpaces;
begin
  TestClarifyFile('TestParamSpaces');
end;

procedure TFullTestClarify.TestPointers;
begin
  TestClarifyFile('TestPointers');
end;

procedure TFullTestClarify.TestProcBlankLines;
begin
  TestClarifyFile('TestProcBlankLines');
end;

procedure TFullTestClarify.TestProgram;
begin
  TestClarifyFile('TestProgram');
end;

procedure TFullTestClarify.TestProperties;
begin
  TestClarifyFile('TestProperties');
end;

procedure TFullTestClarify.TestPropertyInherited;
begin
  TestClarifyFile('TestPropertyInherited');
end;

procedure TFullTestClarify.TestPropertyLines;
begin
  TestClarifyFile('TestPropertyLines');
end;

procedure TFullTestClarify.TestRecords;
begin
  TestClarifyFile('TestRecords');
end;

procedure TFullTestClarify.TestReg;
begin
  TestClarifyFile('TestReg');
end;

procedure TFullTestClarify.TestReint;
begin
  TestClarifyFile('TestReint');
end;

procedure TFullTestClarify.TestReturnRemoval;
begin
  TestClarifyFile('TestReturnRemoval');
end;

procedure TFullTestClarify.TestReturns;
begin
  TestClarifyFile('TestReturns');
end;

procedure TFullTestClarify.TestRunOnConst;
begin
  TestClarifyFile('TestRunOnConst');
end;

procedure TFullTestClarify.TestRunOnDef;
begin
  TestClarifyFile('TestRunOnDef');
end;

procedure TFullTestClarify.TestRunOnLine;
begin
  TestClarifyFile('TestRunOnLine');
end;

procedure TFullTestClarify.TestTPObjects;
begin
  TestClarifyFile('TestTPObjects');
end;

procedure TFullTestClarify.TestTry;
begin
  TestClarifyFile('TestTry');
end;

procedure TFullTestClarify.TestTypeDefs;
begin
  TestClarifyFile('TestTypeDefs');
end;

procedure TFullTestClarify.TestUses;
begin
  TestClarifyFile('TestUses');
end;

procedure TFullTestClarify.TestUsesChanges;
begin
  TestClarifyFile('TestUsesChanges');
end;

procedure TFullTestClarify.TestVarParam;
begin
  TestClarifyFile('TestVarParam');
end;

procedure TFullTestClarify.TestWarnings;
begin
  TestClarifyFile('TestWarnings');
end;

procedure TFullTestClarify.TestWith;
begin
  TestClarifyFile('TestWith');
end;

procedure TFullTestClarify.LittleTest12;
begin
  TestClarifyFile('LittleTest12');
end;

procedure TFullTestClarify.LittleTest13;
begin
  TestClarifyFile('LittleTest13');
end;

procedure TFullTestClarify.LittleTest14;
begin
  TestClarifyFile('LittleTest14');
end;

procedure TFullTestClarify.LittleTest15;
begin
  TestClarifyFile('LittleTest15');
end;

procedure TFullTestClarify.LittleTest16;
begin
  TestClarifyFile('LittleTest16');
end;

procedure TFullTestClarify.LittleTest17;
begin
  TestClarifyFile('LittleTest17');
end;

procedure TFullTestClarify.LittleTest18;
begin
  TestClarifyFile('LittleTest18');
end;

procedure TFullTestClarify.TestAtExpr;
begin
  TestClarifyFile('TestAtExpr');
end;

procedure TFullTestClarify.TestAsmStructs;
begin
  TestClarifyFile('TestAsmStructs');
end;

procedure TFullTestClarify.TestUnitAllDirectives;
begin
  TestClarifyFile('TestUnitAllDirectives');
end;

procedure TFullTestClarify.TestUnitDeprecated;
begin
  TestClarifyFile('TestUnitDeprecated');
end;

procedure TFullTestClarify.TestUnitLibrary;
begin
  TestClarifyFile('TestUnitLibrary');
end;

procedure TFullTestClarify.TestUnitPlatform;
begin
  TestClarifyFile('TestUnitPlatform');
end;

procedure TFullTestClarify.LittleTest19;
begin
  TestClarifyFile('LittleTest19');
end;

procedure TFullTestClarify.TestRaise;
begin
  TestClarifyFile('TestRaise');
end;

procedure TFullTestClarify.LittleTest20;
begin
  TestClarifyFile('LittleTest20');
end;

procedure TFullTestClarify.LittleTest21;
begin
  TestClarifyFile('LittleTest21');
end;

procedure TFullTestClarify.LittleTest22;
begin
  TestClarifyFile('LittleTest22');
end;

procedure TFullTestClarify.LittleTest23;
begin
  TestClarifyFile('LittleTest23');
end;

procedure TFullTestClarify.LittleTest24;
begin
  TestClarifyFile('LittleTest24');
end;

procedure TFullTestClarify.LittleTest25;
begin
  TestClarifyFile('LittleTest25');
end;

procedure TFullTestClarify.LittleTest26;
begin
  TestClarifyFile('LittleTest26');
end;

procedure TFullTestClarify.LittleTest27;
begin
  TestClarifyFile('LittleTest27');
end;

procedure TFullTestClarify.TestEmptySquareBrackets;
begin
  TestClarifyFile('TestEmptySquareBrackets');
end;

procedure TFullTestClarify.LittleTest28;
begin
  TestClarifyFile('LittleTest28');
end;

procedure TFullTestClarify.LittleTest29;
begin
  TestClarifyFile('LittleTest29');
end;

procedure TFullTestClarify.LittleTest30;
begin
  TestClarifyFile('LittleTest30');
end;

procedure TFullTestClarify.LittleTest31;
begin
  TestClarifyFile('LittleTest31');
end;

procedure TFullTestClarify.LittleTest32;
begin
  TestClarifyFile('LittleTest32');
end;

procedure TFullTestClarify.LittleTest33;
begin
  TestClarifyFile('LittleTest33');
end;

procedure TFullTestClarify.TestEmptyCase;
begin
  TestClarifyFile('TestEmptyCase');
end;

procedure TFullTestClarify.TestCaseIfFormat;
begin
  TestClarifyFile('TestCaseIfFormat');
end;

procedure TFullTestClarify.LittleTest34;
begin
  TestClarifyFile('LittleTest34');
end;

procedure TFullTestClarify.LittleTest35;
begin
  TestClarifyFile('LittleTest35');
end;

procedure TFullTestClarify.LittleTest36;
begin
  TestClarifyFile('LittleTest36');
end;

procedure TFullTestClarify.LittleTest37;
begin
  TestClarifyFile('LittleTest37');
end;

procedure TFullTestClarify.TestSimpleIfDef;
begin
  TestClarifyFile('TestSimpleIfDef');
end;

procedure TFullTestClarify.TestSimpleIfDef2;
begin
  TestClarifyFile('TestSimpleIfDef2');
end;

procedure TFullTestClarify.TestSimpleIfDef3;
begin
  TestClarifyFile('TestSimpleIfDef3');
end;

procedure TFullTestClarify.TestSimpleIfDef4;
begin
  TestClarifyFile('TestSimpleIfDef4');
end;

procedure TFullTestClarify.TestSimpleIfDef5;
begin
  TestClarifyFile('TestSimpleIfDef5');
end;

procedure TFullTestClarify.LittleTest38;
begin
  TestClarifyFile('LittleTest38');
end;

procedure TFullTestClarify.LittleTest39;
begin
  TestClarifyFile('LittleTest39');
end;

procedure TFullTestClarify.LittleTest40;
begin
  TestClarifyFile('LittleTest40');
end;

procedure TFullTestClarify.TestDefines;
begin
  TestClarifyFile('TestDefines');
end;

procedure TFullTestClarify.LittleTest41;
begin
  TestClarifyFile('LittleTest41');
end;

procedure TFullTestClarify.LittleTest42;
begin
  TestClarifyFile('LittleTest42');
end;

procedure TFullTestClarify.LittleTest43;
begin
  TestClarifyFile('LittleTest43');
end;

procedure TFullTestClarify.LittleTest44;
begin
  TestClarifyFile('LittleTest44');
end;

procedure TFullTestClarify.LittleTest45;
begin
  TestClarifyFile('LittleTest45');
end;

procedure TFullTestClarify.LittleTest46;
begin
  TestClarifyFile('LittleTest46');
end;

procedure TFullTestClarify.LittleTest47;
begin
  TestClarifyFile('LittleTest47');
end;

procedure TFullTestClarify.TestWarnDestroy;
begin
  TestClarifyFile('TestWarnDestroy');
end;

procedure TFullTestClarify.LittleTest48;
begin
  TestClarifyFile('LittleTest48');
end;

procedure TFullTestClarify.LittleTest49;
begin
  TestClarifyFile('LittleTest49');
end;

procedure TFullTestClarify.LittleTest50;
begin
  TestClarifyFile('LittleTest50');
end;

procedure TFullTestClarify.LittleTest51;
begin
  TestClarifyFile('LittleTest51');
end;

procedure TFullTestClarify.LittleTest52;
begin
  TestClarifyFile('LittleTest52');
end;

procedure TFullTestClarify.TestSimpleIfDef6;
begin
  TestClarifyFile('TestSimpleIfDef6');
end;

procedure TFullTestClarify.LittleTest53;
begin
  TestClarifyFile('LittleTest53');
end;

procedure TFullTestClarify.LittleTest54;
begin
  TestClarifyFile('LittleTest54');
end;

procedure TFullTestClarify.LittleTest55;
begin
  TestClarifyFile('LittleTest55');
end;

procedure TFullTestClarify.LittleTest56;
begin
  TestClarifyFile('LittleTest56');
end;

procedure TFullTestClarify.LittleTest57;
begin
  TestClarifyFile('LittleTest57');
end;

procedure TFullTestClarify.LittleTest58;
begin
  TestClarifyFile('LittleTest58');
end;

procedure TFullTestClarify.LittleTest59;
begin
  TestClarifyFile('LittleTest59');
end;

procedure TFullTestClarify.LittleTest60;
begin
  TestClarifyFile('LittleTest60');
end;

procedure TFullTestClarify.LittleTest61;
begin
  TestClarifyFile('LittleTest61');
end;

procedure TFullTestClarify.LittleTest62;
begin
  TestClarifyFile('LittleTest62');
end;

procedure TFullTestClarify.TestInline;
begin
  TestClarifyFile('TestInline');
end;

procedure TFullTestClarify.fBracketProp;
begin
  TestClarifyFile('fBracketProp');
end;

procedure TFullTestClarify.TestEndElse;
begin
  TestClarifyFile('TestEndElse');
end;

procedure TFullTestClarify.TestCondReturns;
begin
  TestClarifyFile('TestCondReturns');
end;

procedure TFullTestClarify.TestDelphiNetUses;
begin
  TestClarifyFile('TestDelphiNetUses');
end;

procedure TFullTestClarify.TestConstBug;
begin
  TestClarifyFile('TestConstBug');
end;

procedure TFullTestClarify.TestForIn;
begin
  TestClarifyFile('TestForIn');
end;

procedure TFullTestClarify.TestDottedName;
begin
  TestClarifyFile('test.dotted.name.pas');
end;

procedure TFullTestClarify.TestDelphiNetClass;
begin
  TestClarifyFile('TestDelphiNetClass');
end;

procedure TFullTestClarify.TestDelphiNetConst;
begin
  TestClarifyFile('TestDelphiNetConst');
end;

procedure TFullTestClarify.TestDelphiNetStatic;
begin
  TestClarifyFile('TestDelphiNetStatic');
end;

procedure TFullTestClarify.TestTestDotNetForm1;
begin
  TestClarifyFile('TestDotNetForm1');
end;

procedure TFullTestClarify.TestDelphiNetNestedType;
begin
  TestClarifyFile('TestDelphiNetNestedType');
end;

procedure TFullTestClarify.TestDelphiNetNestedType2;
begin
  TestClarifyFile('TestDelphiNetNestedType2');

end;

procedure TFullTestClarify.TestDelphiNetOperatorOverload;
begin
  TestClarifyFile('TestDelphiNetOperatorOverload');
end;

procedure TFullTestClarify.TestDelphiNetHelperClass;
begin
  TestClarifyFile('TestDelphiNetHelperClass');
end;

procedure TFullTestClarify.TestDelphiNetRecordForward;
begin
  TestClarifyFile('TestDelphiNetRecordForward');
end;

procedure TFullTestClarify.TestCondCompBreaks;
begin
  TestClarifyFile('TestCondCompBreaks');
end;

procedure TFullTestClarify.TestCondCompBreaks2;
begin
  TestClarifyFile('TestCondCompBreaks2');
end;

procedure TFullTestClarify.TestDelphiNetAttributes;
begin
  TestClarifyFile('TestDelphiNetAttributes');
end;

procedure TFullTestClarify.TestDelphiNetWebService;
begin
  TestClarifyFile('TestDelphiNetWebService');
end;

procedure TFullTestClarify.TestDelphiNetKeywords;
begin
  TestClarifyFile('TestDelphiNetKeywords');
end;

procedure TFullTestClarify.TestDelphiNetClassVar;
begin
  TestClarifyFile('TestDelphiNetClassVar');
end;

procedure TFullTestClarify.TestTrailingCommaParam;
begin
  TestClarifyFile('TestTrailingCommaParam');
end;

procedure TFullTestClarify.TestTryExceptRaise;
begin
  TestClarifyFile('TestTryExceptRaise');
end;

procedure TFullTestClarify.TestDprNoBegin;
begin
  TestClarifyFile('TestDprNoBegin.dpr');
end;

procedure TFullTestClarify.TestDLLIndex;
begin
  TestClarifyFile('TestDLLIndex');
end;

initialization
  TestFramework.RegisterTest(TFullTestClarify.Suite);
end.
