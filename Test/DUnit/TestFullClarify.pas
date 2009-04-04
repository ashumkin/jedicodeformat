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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ test the full clarify - all processes }


uses
  TestFile;

type
  TFullTestClarify = class(TTestFile)
  private
    procedure TestSingleClarifyFile(const psInFileName, psRefOutput: string;
      const piTokenCount: integer);
    procedure TestDoubleClarifyFile(const psInFileName, psRefOutput: string);

    procedure TestFile(const psName: string; const piTokenCount: integer);

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
    procedure TestComplexAsm2;

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
    procedure TestNestedType;

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
    procedure TestDelphiNetRecordClassVars;

    procedure TestTryExceptRaise;
    procedure TestTrailingCommaParam;
    procedure TestDprNoBegin;
    procedure TestDLLIndex;
    procedure TestIncAt;
    procedure TestAsmAnd;
    procedure TestAsmLabel;
    procedure TestVarArgs;
    procedure TestLabelKeyword;

    procedure TestSubrangeType;
    procedure TestAmpersand;
    procedure TestAutomated;
    procedure TestClassMethods;
    procedure TestExports;

    procedure TestDelphiNetLibrary;

    procedure TestAsmCaps;
    procedure TestAsmOffsetKeyword;

    procedure TestGenerics;
    procedure TestGenerics2;

    procedure TestGenericArray;
    procedure TestGenericClassHelper;
    procedure TestGenericClassOperators;
    procedure TestGenericConstraintConstructor;
    procedure TestGenericConstraints;
    procedure TestGenericConstructorStatic;
    procedure TestGenericDelegates;
    procedure TestGenericFunctions;
    procedure TestGenericHeritage;
    procedure TestGenericInheritance;
    procedure TestGenericInterface;
    procedure TestGenericMethod;
    procedure TestGenericMethods1;
    procedure TestGenericOperatorAs;
    procedure TestGenericOperatorIs;
    procedure TestGenericTypeNullable;
    procedure TestPackedObject;
    procedure TestClassVarEmpty;
    procedure TestRecordWithClassFunction;
    procedure TestOutKeyword;
    procedure TestExit;
    procedure TestHexConstantElse;
    procedure TestClassOf;

    procedure TestDelphi2009Inherited;
    procedure TestDelphi2009Generics;
    procedure TestDelphi2009AnonymousMethod;
    procedure TestAnonFunctionInInitialization;

    procedure TestLibrary;

    procedure TestUnicode_ansi;
    procedure TestUnicode_be_ucs2;
    procedure TestUnicode_be_ucs4;
    procedure TestUnicode_le_ucs2;
    procedure TestUnicode_le_ucs4;
    procedure TestUnicode_utf8;

    procedure TestUnicodeStrings;
    procedure TestAssignments;

    procedure TestCases;
  end;


implementation

uses
  { delphi }
  SysUtils, Windows,
  { DUnit}
  TestFrameWork,
  { JCF }
  JcfStringUtils,
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;

{ TFullTestClarify }


procedure TFullTestClarify.TestFile(const psName: string;
  const piTokenCount: integer);
var
  liLastDotPos: integer;
  lsInName, lsClearFileName: string;
  lsFullIn, lsFullOut: string;
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


  lsFullIn := GetTestFilesDir + lsInName;
  lsFullOut := GetRefOutFilesDir + lsClearFileName;

  TestSingleClarifyFile(lsFullIn, lsFullOut, piTokenCount);

  //TestDoubleClarifyFile(lsFullIn, lsFullOut);
end;

procedure TFullTestClarify.TestSingleClarifyFile(const psInFileName, psRefOutput: string;
  const piTokenCount: integer);
var
  lcConverter:   TFileConverter;
  lsOutFileName: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  FormatSettings.Obfuscate.Enabled := False;

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll      := True;
    lcConverter.GuiMessages := False;

    { see also TestFileParse }
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

    TestFileContentsSame(lsOutFileName, psRefOutput);
  finally
    lcConverter.Free;
    FormatSettings.Obfuscate.Enabled := False;
  end;
end;

procedure TFullTestClarify.TestDoubleClarifyFile(const psInFileName, psRefOutput: string);
var
  lcConverter:   TFileConverter;
  lsOutFileName: string;
  lsOutFileName2: string;
  bSaveAlign: Array[0..5] of boolean;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  FormatSettings.Obfuscate.Enabled := False;

  bSaveAlign[0] := FormatSettings.Align.AlignAssign;
  bSaveAlign[1] := FormatSettings.Align.AlignConst;
  bSaveAlign[2] := FormatSettings.Align.AlignTypeDef;
  bSaveAlign[3] := FormatSettings.Align.AlignVar;
  bSaveAlign[4] := FormatSettings.Align.AlignComment;
  bSaveAlign[5] := FormatSettings.Align.AlignField;

  FormatSettings.Align.AlignAssign := False;
  FormatSettings.Align.AlignConst := False;
  FormatSettings.Align.AlignTypeDef := False;
  FormatSettings.Align.AlignVar := False;
  FormatSettings.Align.AlignComment := False;
  FormatSettings.Align.AlignField := False;

  lcConverter := TFileConverter.Create;

  try
    lcConverter.YesAll      := True;
    lcConverter.GuiMessages := False;

    { see also TestFileParse }
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

    // clean up
    SysUtils.DeleteFile(lsOutFileName);
    SysUtils.DeleteFile(lsOutFileName2);
  finally
    FormatSettings.Align.AlignAssign := bSaveAlign[0];
    FormatSettings.Align.AlignConst := bSaveAlign[1];
    FormatSettings.Align.AlignTypeDef := bSaveAlign[2];
    FormatSettings.Align.AlignVar := bSaveAlign[3];
    FormatSettings.Align.AlignComment := bSaveAlign[4];
    FormatSettings.Align.AlignField := bSaveAlign[5];


    lcConverter.Free;
    FormatSettings.Obfuscate.Enabled := False;
  end;
end;


procedure TFullTestClarify.EmptyTest1;
begin
  TestFile('EmptyTest1', 18);
end;

procedure TFullTestClarify.fFormTest;
begin
  TestFile('fFormTest', 150);
end;

procedure TFullTestClarify.LittleTest1;
begin
  TestFile('LittleTest1', 28);
end;

procedure TFullTestClarify.LittleTest2;
begin
  TestFile('LittleTest2', 29);
end;

procedure TFullTestClarify.LittleTest3;
begin
  TestFile('LittleTest3', 42);
end;

procedure TFullTestClarify.LittleTest4;
begin
  TestFile('LittleTest4', 45);
end;

procedure TFullTestClarify.LittleTest5;
begin
  TestFile('LittleTest5', 58);
end;

procedure TFullTestClarify.LittleTest6;
begin
  TestFile('LittleTest6', 78);
end;

procedure TFullTestClarify.LittleTest7;
begin
  TestFile('LittleTest7', 109);
end;

procedure TFullTestClarify.LittleTest8;
begin
  TestFile('LittleTest8', 41);
end;

procedure TFullTestClarify.LittleTest9;
begin
  TestFile('LittleTest9', 69);
end;

procedure TFullTestClarify.LittleTest10;
begin
  TestFile('LittleTest10', 375);
end;

procedure TFullTestClarify.LittleTest11;
begin
  TestFile('LittleTest11', 123);
end;


procedure TFullTestClarify.TestAbsolute;
begin
  TestFile('TestAbsolute', 129);
end;

procedure TFullTestClarify.TestAlign;
begin
  TestFile('TestAlign', 662);
end;

procedure TFullTestClarify.TestAmpersand;
begin
  TestFile('TestAmpersand', 87);
end;

procedure TFullTestClarify.TestAnonFunctionInInitialization;
begin
  TestFile('TestAnonFunctionInInitialization', 94);
end;

procedure TFullTestClarify.TestArray;
begin
  TestFile('TestArray', 220);
end;

procedure TFullTestClarify.TestAsm;
begin
  TestFile('TestAsm', 958);
end;


procedure TFullTestClarify.TestBlankLineRemoval;
begin
  TestFile('TestBlankLineRemoval', 373);
end;

procedure TFullTestClarify.TestBogusDirectives;
begin
  TestFile('TestBogusDirectives', 541);
end;

procedure TFullTestClarify.TestBogusTypes;
begin
  TestFile('TestBogusTypes', 230);
end;

procedure TFullTestClarify.TestCaseBlock;
begin
  TestFile('TestCaseBlock', 3041);
end;

procedure TFullTestClarify.TestCases;
begin
  TestFile('D11\TestCases.dpr', 1421);
end;

procedure TFullTestClarify.TestCast;
begin
  TestFile('TestCast', 654);
end;

procedure TFullTestClarify.TestCastSimple;
begin
  TestFile('TestCastSimple', 843);
end;

procedure TFullTestClarify.TestCharLiterals;
begin
  TestFile('TestCharLiterals', 1035);
end;

procedure TFullTestClarify.TestClassLines;
begin
  TestFile('TestClassLines', 71);
end;

procedure TFullTestClarify.TestClassMethods;
begin
  TestFile('TestClassMethods', 185);
end;

procedure TFullTestClarify.TestClassOf;
begin
  TestFile('TestClassOf', 53);
end;

procedure TFullTestClarify.TestCommentIndent;
begin
  TestFile('TestCommentIndent', 549);
end;

procedure TFullTestClarify.TestCommentIndent2;
begin
  TestFile('TestCommentIndent2', 361);
end;

procedure TFullTestClarify.TestComplexAsm2;
begin
  TestFile('TestComplexAsm2', 190);
end;

procedure TFullTestClarify.TestConstRecords;
begin
  TestFile('TestConstRecords', 945);
end;

procedure TFullTestClarify.TestD6;
begin
  TestFile('TestD6', 959);
end;

procedure TFullTestClarify.TestDelphi2009Generics;
begin
  TestFile('TestDelphi2009Generics', 302);
end;

procedure TFullTestClarify.TestDelphi2009AnonymousMethod;
begin
  TestFile('TestDelphi2009AnonymousMethod', 521);
end;

procedure TFullTestClarify.TestDelphi2009Inherited;
begin
  TestFile('TestDelphi2009Inherited', 192);
end;

procedure TFullTestClarify.TestDeclarations;
begin
  TestFile('TestDeclarations', 1019);
end;

procedure TFullTestClarify.TestDeclarations2;
begin
  TestFile('TestDeclarations2', 362);
end;

procedure TFullTestClarify.TestDefaultParams;
begin
  TestFile('TestDefaultParams', 698);
end;

procedure TFullTestClarify.TestDeref;
begin
  TestFile('TestDeref', 584);
end;

procedure TFullTestClarify.TestEmptyClass;
begin
  TestFile('TestEmptyClass', 244);
end;

procedure TFullTestClarify.TestClassVarEmpty;
begin
  TestFile('TestClassVarEmpty', 404);
end;

procedure TFullTestClarify.TestEsotericKeywords;
begin
  TestFile('TestEsotericKeywords', 258);
end;

procedure TFullTestClarify.TestExclusion;
begin
  TestFile('TestExclusion', 431);
end;

procedure TFullTestClarify.TestExclusionFlags;
begin
  TestFile('TestExclusionFlags', 723);
end;

procedure TFullTestClarify.TestExit;
begin
  TestFile('TestExit', 90);
end;

procedure TFullTestClarify.TestExports;
begin
  TestFile('TestExports', 43);
end;

procedure TFullTestClarify.TestExternal;
begin
  TestFile('TestExternal', 259);
end;

procedure TFullTestClarify.TestForward;
begin
  TestFile('TestForward', 332);
end;

procedure TFullTestClarify.TestGenericConstraintConstructor;
begin
  TestFile('TestGenericConstraintConstructor.dpr', 637);
end;

procedure TFullTestClarify.TestGenericConstraints;
begin
  TestFile('TestGenericConstraints.dpr', 673);
end;

procedure TFullTestClarify.TestGenerics;
begin
  TestFile('TestGenerics', 262);
end;

procedure TFullTestClarify.TestGenerics2;
begin
  TestFile('TestGenerics2', 277);
end;


procedure TFullTestClarify.TestGenericTypeNullable;
begin
  TestFile('TestGenericTypeNullable.dpr', 1426);
end;

procedure TFullTestClarify.TestGenericArray;
begin
  TestFile('TestGenericArray.dpr', 244);
end;

procedure TFullTestClarify.TestGenericClassHelper;
begin
  TestFile('TestGenericClassHelper.dpr', 187);
end;


procedure TFullTestClarify.TestGenericClassOperators;
begin
  TestFile('TestGenericClassOperators.dpr', 212);
end;

procedure TFullTestClarify.TestGenericConstructorStatic;
begin
  TestFile('TestGenericConstructorStatic.dpr', 553);
end;


procedure TFullTestClarify.TestGenericDelegates;
begin
  TestFile('TestGenericDelegates.dpr', 423);
end;



procedure TFullTestClarify.TestGenericFunctions;
begin
  TestFile('TestGenericFunctions.dpr', 790);
end;

procedure TFullTestClarify.TestGenericHeritage;
begin
  TestFile('TestGenericHeritage.dpr', 352);
end;

procedure TFullTestClarify.TestGenericInheritance;
begin
  TestFile('TestGenericInheritance.dpr', 291);
end;

procedure TFullTestClarify.TestGenericInterface;
begin
  TestFile('TestGenericInterface.dpr', 430);
end;

procedure TFullTestClarify.TestGenericMethod;
begin
  TestFile('TestGenericMethod.dpr', 567);
end;

procedure TFullTestClarify.TestGenericMethods1;
begin
  TestFile('TestGenericMethods1.dpr', 517);
end;

procedure TFullTestClarify.TestGenericOperatorAs;
begin
  TestFile('TestGenericOperatorAs.dpr', 447);
end;

procedure TFullTestClarify.TestGenericOperatorIs;
begin
  TestFile('TestGenericOperatorIs.dpr', 967);
end;

procedure TFullTestClarify.TestGoto;
begin
  TestFile('TestGoto', 503);
end;

procedure TFullTestClarify.TestHexConstantElse;
begin
  TestFile('TestHexConstantElse', 62);
end;

procedure TFullTestClarify.TestInheritedExpr;
begin
  TestFile('TestInheritedExpr', 301);
end;

procedure TFullTestClarify.TestInitFinal;
begin
  TestFile('TestInitFinal', 170);
end;

procedure TFullTestClarify.TestInterfaceImplements;
begin
  TestFile('TestInterfaceImplements', 225);
end;

procedure TFullTestClarify.TestInterfaceMap;
begin
  TestFile('TestInterfaceMap', 397);
end;

procedure TFullTestClarify.TestInterfaces;
begin
  TestFile('TestInterfaces', 648);
end;

procedure TFullTestClarify.TestLabelKeyword;
begin
  TestFile('TestLabelKeyword', 60);
end;

procedure TFullTestClarify.TestLayout;
begin
  TestFile('TestLayout', 1227);
end;

procedure TFullTestClarify.TestLayoutBare;
begin
  TestFile('TestLayoutBare', 1555);
end;

procedure TFullTestClarify.TestLayoutBare2;
begin
  TestFile('TestLayoutBare2', 1013);
end;

procedure TFullTestClarify.TestLayoutBare3;
begin
  TestFile('TestLayoutBare3', 1178);
end;

procedure TFullTestClarify.TestLibExports;
begin
  TestFile('TestLibExports', 119);
end;

procedure TFullTestClarify.TestLibrary;
begin
  TestFile('TestLibrary', 35);
end;

procedure TFullTestClarify.TestLineBreaking;
begin
  TestFile('TestLineBreaking', 6531);
end;

procedure TFullTestClarify.TestLocalTypes;
begin
  TestFile('TestLocalTypes', 297);
end;

procedure TFullTestClarify.TestLongStrings;
begin
  TestFile('TestLongStrings', 163);
end;

procedure TFullTestClarify.TestMarcoV;
begin
  TestFile('TestMarcoV', 241);
end;

procedure TFullTestClarify.TestMessages;
begin
  TestFile('TestMessages',130);
end;

procedure TFullTestClarify.TestMH;
begin
  TestFile('TestMH', 2956);
end;

procedure TFullTestClarify.TestMixedModeCaps;
begin
  TestFile('TestMixedModeCaps', 123);
end;

procedure TFullTestClarify.TestMVB;
begin
  TestFile('TestMVB', 835);
end;

procedure TFullTestClarify.TestNested;
begin
  TestFile('TestNested', 658);
end;

procedure TFullTestClarify.TestNestedRecords;
begin
  TestFile('TestNestedRecords', 1189);
end;

procedure TFullTestClarify.TestNestedType;
begin
  TestFile('TestNestedType', 194);
end;

procedure TFullTestClarify.TestOperators;
begin
  TestFile('TestOperators', 1233);
end;

procedure TFullTestClarify.TestOutKeyword;
begin
  TestFile('TestOutKeyword', 41);
end;

procedure TFullTestClarify.TestOleParams;
begin
  TestFile('TestOleParams', 160);
end;

procedure TFullTestClarify.TestPackage;
begin
  TestFile('TestMe.dpk', 684);
end;

procedure TFullTestClarify.TestPackedObject;
begin
  TestFile('TestPackedObject', 37);
end;

procedure TFullTestClarify.TestParams;
begin
  TestFile('TestParams', 218);
end;

procedure TFullTestClarify.TestParamSpaces;
begin
  TestFile('TestParamSpaces', 159);
end;

procedure TFullTestClarify.TestPointers;
begin
  TestFile('TestPointers', 193);
end;

procedure TFullTestClarify.TestProcBlankLines;
begin
  TestFile('TestProcBlankLines', 64);
end;

procedure TFullTestClarify.TestProgram;
begin
  TestFile('TestProgram', 1246);
end;

procedure TFullTestClarify.TestProperties;
begin
  TestFile('TestProperties', 751);
end;

procedure TFullTestClarify.TestPropertyInherited;
begin
  TestFile('TestPropertyInherited', 797);
end;

procedure TFullTestClarify.TestPropertyLines;
begin
  TestFile('TestPropertyLines', 1186);
end;

procedure TFullTestClarify.TestRecords;
begin
  TestFile('TestRecords', 1495);
end;

procedure TFullTestClarify.TestRecordWithClassFunction;
begin
  TestFile('TestRecordWithClassFunction', 130);
end;

procedure TFullTestClarify.TestReg;
begin
  TestFile('TestReg', 85);
end;

procedure TFullTestClarify.TestReint;
begin
  TestFile('TestReint', 159);
end;

procedure TFullTestClarify.TestReturnRemoval;
begin
  TestFile('TestReturnRemoval', 256);
end;

procedure TFullTestClarify.TestReturns;
begin
  TestFile('TestReturns', 141);
end;

procedure TFullTestClarify.TestRunOnConst;
begin
  TestFile('TestRunOnConst', 465);
end;

procedure TFullTestClarify.TestRunOnDef;
begin
  TestFile('TestRunOnDef', 363);
end;

procedure TFullTestClarify.TestRunOnLine;
begin
  TestFile('TestRunOnLine', 3668);
end;

procedure TFullTestClarify.TestTPObjects;
begin
  TestFile('TestTPObjects', 126);
end;

procedure TFullTestClarify.TestTry;
begin
  TestFile('TestTry', 939);
end;

procedure TFullTestClarify.TestTypeDefs;
begin
  TestFile('TestTypeDefs', 793);
end;

procedure TFullTestClarify.TestUses;
begin
  TestFile('TestUses', 64);
end;

procedure TFullTestClarify.TestUsesChanges;
begin
  TestFile('TestUsesChanges', 56);
end;

procedure TFullTestClarify.TestVarArgs;
begin
  TestFile('TestVarArgs', 43);
end;

procedure TFullTestClarify.TestVarParam;
begin
  TestFile('TestVarParam', 116);
end;

procedure TFullTestClarify.TestWarnings;
begin
  TestFile('TestWarnings', 702);
end;

procedure TFullTestClarify.TestWith;
begin
  TestFile('TestWith', 735);
end;

procedure TFullTestClarify.LittleTest12;
begin
  TestFile('LittleTest12', 54);
end;

procedure TFullTestClarify.LittleTest13;
begin
  TestFile('LittleTest13', 86);
end;

procedure TFullTestClarify.LittleTest14;
begin
  TestFile('LittleTest14', 37);
end;

procedure TFullTestClarify.LittleTest15;
begin
  TestFile('LittleTest15', 41);
end;

procedure TFullTestClarify.LittleTest16;
begin
  TestFile('LittleTest16', 102);
end;

procedure TFullTestClarify.LittleTest17;
begin
  TestFile('LittleTest17', 102);
end;

procedure TFullTestClarify.LittleTest18;
begin
  TestFile('LittleTest18', 103);
end;

procedure TFullTestClarify.TestAtExpr;
begin
  TestFile('TestAtExpr', 79);
end;

procedure TFullTestClarify.TestAutomated;
begin
  TestFile('TestAutomated', 167);
end;

procedure TFullTestClarify.TestAsmOffsetKeyword;
begin
  TestFile('TestAsmOffsetKeyword', 145);
end;

procedure TFullTestClarify.TestAsmOps;
begin
  TestFile('TestAsmOps', 93);
end;

procedure TFullTestClarify.TestAsmStructs;
begin
  TestFile('TestAsmStructs', 358);
end;

procedure TFullTestClarify.TestAssignments;
begin
  TestFile('TestAssignments', 81);
end;

procedure TFullTestClarify.TestUnicodeStrings;
begin
  TestFile('TestUnicodeStrings', 91);
end;

procedure TFullTestClarify.TestUnicode_ansi;
begin
  TestFile('TestUnicode_ansi', 89);
end;

procedure TFullTestClarify.TestUnicode_be_ucs2;
begin
  TestFile('TestUnicode_be_ucs2', 89);
end;

procedure TFullTestClarify.TestUnicode_be_ucs4;
begin
  TestFile('TestUnicode_be_ucs4', 89);
end;

procedure TFullTestClarify.TestUnicode_le_ucs2;
begin
  TestFile('TestUnicode_le_ucs2', 89);
end;

procedure TFullTestClarify.TestUnicode_le_ucs4;
begin
  TestFile('TestUnicode_le_ucs4', 89);
end;

procedure TFullTestClarify.TestUnicode_utf8;
begin
  TestFile('TestUnicode_utf8', 89);
end;

procedure TFullTestClarify.TestUnitAllDirectives;
begin
  TestFile('TestUnitAllDirectives', 21);
end;

procedure TFullTestClarify.TestUnitDeprecated;
begin
  TestFile('TestUnitDeprecated', 17);
end;

procedure TFullTestClarify.TestUnitLibrary;
begin
  TestFile('TestUnitLibrary', 20);
end;

procedure TFullTestClarify.TestUnitPlatform;
begin
  TestFile('TestUnitPlatform', 17);
end;

procedure TFullTestClarify.LittleTest19;
begin
  TestFile('LittleTest19', 168);
end;

procedure TFullTestClarify.TestRaise;
begin
  TestFile('TestRaise', 519);
end;

procedure TFullTestClarify.LittleTest20;
begin
  TestFile('LittleTest20', 62);
end;

procedure TFullTestClarify.LittleTest21;
begin
  TestFile('LittleTest21', 37);
end;

procedure TFullTestClarify.LittleTest22;
begin
  TestFile('LittleTest22', 53);
end;

procedure TFullTestClarify.LittleTest23;
begin
  TestFile('LittleTest23', 74);
end;

procedure TFullTestClarify.LittleTest24;
begin
  TestFile('LittleTest24', 69);
end;

procedure TFullTestClarify.LittleTest25;
begin
  TestFile('LittleTest25', 42);
end;

procedure TFullTestClarify.LittleTest26;
begin
  TestFile('LittleTest26', 153);
end;

procedure TFullTestClarify.LittleTest27;
begin
  TestFile('LittleTest27', 137);
end;

procedure TFullTestClarify.TestEmptySquareBrackets;
begin
  TestFile('TestEmptySquareBrackets', 66);
end;

procedure TFullTestClarify.LittleTest28;
begin
  TestFile('LittleTest28', 71);
end;

procedure TFullTestClarify.LittleTest29;
begin
  TestFile('LittleTest29', 141);
end;

procedure TFullTestClarify.LittleTest30;
begin
  TestFile('LittleTest30', 60);
end;

procedure TFullTestClarify.LittleTest31;
begin
  TestFile('LittleTest31', 71);
end;

procedure TFullTestClarify.LittleTest32;
begin
  TestFile('LittleTest32', 91);
end;

procedure TFullTestClarify.LittleTest33;
begin
  TestFile('LittleTest33', 45);
end;

procedure TFullTestClarify.TestEmptyCase;
begin
  TestFile('TestEmptyCase', 128);
end;

procedure TFullTestClarify.TestCaseIfFormat;
begin
  TestFile('TestCaseIfFormat', 294);
end;

procedure TFullTestClarify.LittleTest34;
begin
  TestFile('LittleTest34', 78);
end;

procedure TFullTestClarify.LittleTest35;
begin
  TestFile('LittleTest35', 38);
end;

procedure TFullTestClarify.LittleTest36;
begin
  TestFile('LittleTest36', 77);
end;

procedure TFullTestClarify.LittleTest37;
begin
  TestFile('LittleTest37', 76);
end;

procedure TFullTestClarify.TestSimpleIfDef;
begin
  TestFile('TestSimpleIfDef', 54);
end;

procedure TFullTestClarify.TestSimpleIfDef2;
begin
  TestFile('TestSimpleIfDef2', 33);
end;

procedure TFullTestClarify.TestSimpleIfDef3;
begin
  TestFile('TestSimpleIfDef3', 83);
end;

procedure TFullTestClarify.TestSimpleIfDef4;
begin
  TestFile('TestSimpleIfDef4', 95);
end;

procedure TFullTestClarify.TestSimpleIfDef5;
begin
  TestFile('TestSimpleIfDef5', 42);
end;

procedure TFullTestClarify.LittleTest38;
begin
  TestFile('LittleTest38', 53);
end;

procedure TFullTestClarify.LittleTest39;
begin
  TestFile('LittleTest39', 185);
end;

procedure TFullTestClarify.LittleTest40;
begin
  TestFile('LittleTest40', 143);
end;

procedure TFullTestClarify.TestDefines;
begin
  TestFile('TestDefines', 262);
end;

procedure TFullTestClarify.LittleTest41;
begin
  TestFile('LittleTest41', 47);
end;

procedure TFullTestClarify.LittleTest42;
begin
  TestFile('LittleTest42', 112);
end;

procedure TFullTestClarify.LittleTest43;
begin
  TestFile('LittleTest43', 413);
end;

procedure TFullTestClarify.LittleTest44;
begin
  TestFile('LittleTest44', 318);
end;

procedure TFullTestClarify.LittleTest45;
begin
  TestFile('LittleTest45', 177);
end;

procedure TFullTestClarify.LittleTest46;
begin
  TestFile('LittleTest46', 96);
end;

procedure TFullTestClarify.LittleTest47;
begin
  TestFile('LittleTest47', 268);
end;

procedure TFullTestClarify.TestWarnDestroy;
begin
  TestFile('TestWarnDestroy', 117);
end;

procedure TFullTestClarify.LittleTest48;
begin
  TestFile('LittleTest48', 204);
end;

procedure TFullTestClarify.LittleTest49;
begin
  TestFile('LittleTest49', 161);
end;

procedure TFullTestClarify.LittleTest50;
begin
  TestFile('LittleTest50', 135);
end;

procedure TFullTestClarify.LittleTest51;
begin
  TestFile('LittleTest51', 172);
end;

procedure TFullTestClarify.LittleTest52;
begin
  TestFile('LittleTest52', 39);
end;

procedure TFullTestClarify.TestSimpleIfDef6;
begin
  TestFile('TestSimpleIfDef6', 49);
end;

procedure TFullTestClarify.TestSubrangeType;
begin
  TestFile('TestSubrangeType', 74);
end;

procedure TFullTestClarify.LittleTest53;
begin
  TestFile('LittleTest53', 60);
end;

procedure TFullTestClarify.LittleTest54;
begin
  TestFile('LittleTest54', 62);
end;

procedure TFullTestClarify.LittleTest55;
begin
  TestFile('LittleTest55', 66);
end;

procedure TFullTestClarify.LittleTest56;
begin
  TestFile('LittleTest56', 51);
end;

procedure TFullTestClarify.LittleTest57;
begin
  TestFile('LittleTest57', 204);
end;

procedure TFullTestClarify.LittleTest58;
begin
  TestFile('LittleTest58', 212);
end;

procedure TFullTestClarify.LittleTest59;
begin
  TestFile('LittleTest59', 61);
end;

procedure TFullTestClarify.LittleTest60;
begin
  TestFile('LittleTest60', 76);
end;

procedure TFullTestClarify.LittleTest61;
begin
  TestFile('LittleTest61', 24);
end;

procedure TFullTestClarify.LittleTest62;
begin
  TestFile('LittleTest62', 49);
end;

procedure TFullTestClarify.TestInline;
begin
  TestFile('TestInline', 92);
end;

procedure TFullTestClarify.fBracketProp;
begin
  TestFile('fBracketProp', 492);
end;

procedure TFullTestClarify.TestEndElse;
begin
  TestFile('TestEndElse', 106);
end;

procedure TFullTestClarify.TestCondReturns;
begin
  TestFile('TestCondReturns', 98);
end;

procedure TFullTestClarify.TestDelphiNetUnsafe;
begin
  TestFile('TestDelphiNetUnsafe', 74);
end;

procedure TFullTestClarify.TestDelphiNetUses;
begin
  TestFile('TestDelphiNetUses', 146);
end;

procedure TFullTestClarify.TestConstBug;
begin
  TestFile('TestConstBug', 157);
end;

procedure TFullTestClarify.TestForIn;
begin
  TestFile('TestForIn', 76);
end;

procedure TFullTestClarify.TestDottedName;
begin
  TestFile('test.dotted.name.pas', 23);
end;

procedure TFullTestClarify.TestDelphiNetClass;
begin
  TestFile('TestDelphiNetClass', 312);
end;

procedure TFullTestClarify.TestDelphiNetConst;
begin
  TestFile('TestDelphiNetConst', 206);
end;

procedure TFullTestClarify.TestDelphiNetDottedType;
begin
  TestFile('TestDelphiNetDottedType', 245);
end;

procedure TFullTestClarify.TestDelphiNetDynamicArray;
begin
  TestFile('TestDelphiNetDynamicArray', 797);
end;

procedure TFullTestClarify.TestDelphiNetStatic;
begin
  TestFile('TestDelphiNetStatic', 324);
end;

procedure TFullTestClarify.TestTestDotNetForm1;
begin
  TestFile('TestDotNetForm1', 356);
end;

procedure TFullTestClarify.TestDelphiNetNestedType;
begin
  TestFile('TestDelphiNetNestedType', 117);
end;

procedure TFullTestClarify.TestDelphiNetNestedType2;
begin
  TestFile('TestDelphiNetNestedType2', 174);

end;

procedure TFullTestClarify.TestDelphiNetOperatorOverload;
begin
  TestFile('TestDelphiNetOperatorOverload', 252);
end;

procedure TFullTestClarify.TestDelphiNetHelperClass;
begin
  TestFile('TestDelphiNetHelperClass', 158);
end;

procedure TFullTestClarify.TestDelphiNetRecordClassVars;
begin
  TestFile('TestDelphiNetRecordClassVars', 240);
end;

procedure TFullTestClarify.TestDelphiNetRecordForward;
begin
  TestFile('TestDelphiNetRecordForward', 142);
end;

procedure TFullTestClarify.TestDelphiNetRecordProcs;
begin
  TestFile('TestDelphiNetRecordProcs', 698);
end;

procedure TFullTestClarify.TestCondCompBreaks;
begin
  TestFile('TestCondCompBreaks', 89);
end;

procedure TFullTestClarify.TestCondCompBreaks2;
begin
  TestFile('TestCondCompBreaks2', 88);
end;

procedure TFullTestClarify.TestDelphiNetAmpersandMethod;
begin
  TestFile('TestDelphiNetAmpersandMethod', 216);
end;

procedure TFullTestClarify.TestDelphiNetAttributes;
begin
  TestFile('TestDelphiNetAttributes', 247);
end;

procedure TFullTestClarify.TestDelphiNetWebService;
begin
  TestFile('TestDelphiNetWebService', 356);
end;

procedure TFullTestClarify.TestDelphiNetWebService2;
begin
  TestFile('TestDelphiNetWebService2', 432);
end;

procedure TFullTestClarify.TestDelphiNetKeywords;
begin
  TestFile('TestDelphiNetKeywords', 97);
end;

procedure TFullTestClarify.TestDelphiNetLibrary;
begin
  TestFile('TestDelphiNetLibrary', 445);
end;

procedure TFullTestClarify.TestDelphiNetMulticast;
begin
  TestFile('TestDelphiNetMulticast', 65);
end;

procedure TFullTestClarify.TestDelphiNetClassVar;
begin
  TestFile('TestDelphiNetClassVar', 314);
end;

procedure TFullTestClarify.TestTrailingCommaParam;
begin
  TestFile('TestTrailingCommaParam', 189);
end;

procedure TFullTestClarify.TestTryExceptRaise;
begin
  TestFile('TestTryExceptRaise', 155);
end;

procedure TFullTestClarify.TestDprNoBegin;
begin
  TestFile('TestDprNoBegin.dpr', 168);
end;

procedure TFullTestClarify.TestDLLIndex;
begin
  TestFile('TestDLLIndex', 120);
end;

procedure TFullTestClarify.TestIncAt;
begin
  TestFile('TestIncAt', 53);
end;

procedure TFullTestClarify.TestDelphiNetFinalMethod;
begin
  TestFile('TestDelphiNetFinalMethod', 140);
end;

procedure TFullTestClarify.TestDelphiNetSealedClass;
begin
  TestFile('TestDelphiNetSealedClass', 200);
end;

procedure TFullTestClarify.TestAsmAnd;
begin
  TestFile('TestAsmAnd', 142);
end;

procedure TFullTestClarify.TestAsmCaps;
begin
  TestFile('TestAsmCaps', 113);
end;

procedure TFullTestClarify.TestAsmLabel;
begin
  TestFile('TestAsmLabel', 52);
end;

initialization
  TestFramework.RegisterTest(TFullTestClarify.Suite);
end.
