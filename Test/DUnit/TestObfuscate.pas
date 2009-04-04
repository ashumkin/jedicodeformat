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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ obfuscate and reclarify all test files }

uses
  TestFrameWork;

type
  TTestObfuscate = class(TTestCase)
  private
    procedure TestFile(const psInFileName, psRefObsOutput,
      psRefClearOutput: string); overload;
    procedure TestFile(const psName: string); overload;

    procedure TestFileContentsSame(const psFileName1, psFileName2: string);

  protected
    procedure SetUp; override;

  published
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
    procedure TestCast;
    procedure TestCastSimple;
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
    procedure TestMessages;
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

    procedure TestSimpleIfdef;
    procedure TestSimpleIfdef2;
    procedure TestSimpleIfdef3;
    procedure TestSimpleIfdef4;
    procedure TestSimpleIfdef5;
    procedure TestSimpleIfdef6;

    procedure TestTestMH;
    procedure TestTPObjects;
    procedure TestTry;
    procedure TestTypeDefs;

    procedure TestUnitAllDirectives;
    procedure TestUnitDeprecated;
    procedure TestUnitLibrary;
    procedure TestUnitPlatform;

    procedure TestUses;
    procedure TestUsesChanges;
    procedure TestVarParam;
    procedure TestWarnings;
    procedure TestWarnDestroy;
    procedure TestWith;

    procedure TestCases;
    procedure TestProcBlankLines;
    procedure TestCondCompBreaks;
    procedure TestCondCompBreaks2;
    procedure TestAsmLabel;
    procedure TestAsmOps;
    procedure TestComplexAsm2;

    procedure TestDelphiNetUses;

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

    procedure TestForIn;
    procedure TestDprNoBegin;
    procedure TestDLLIndex;
    procedure TestIncAt;
    procedure TestAsmAnd;
    procedure TestVarArgs;
    procedure TestLabelKeyword;
    procedure TestSubrangeType;
    procedure TestAmpersand;
    procedure TestAutomated;
    procedure TestClassMethods;
    procedure TestExports;

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

    procedure TestDelphiNetLibrary;

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

  end;

implementation

uses
  { delphi }
  Windows, SysUtils, 
  { local }
  JcfStringUtils,
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;

procedure TTestObfuscate.Setup;
begin
  inherited;

  InitTestSettings;
end;

procedure TTestObfuscate.TestFile(
  const psInFileName, psRefObsOutput, psRefClearOutput: string);
var
  lcConverter:   TFileConverter;
  lsObsFileName: string;
  lsOutFileName: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  FormatSettings.Obfuscate.Enabled := True;
  GetRegSettings.OutputExtension   := 'obs';

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll      := True;
    lcConverter.GuiMessages := False;

    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeparateOutput;

    FormatSettings.Obfuscate.Enabled := True;

    lcConverter.Input := psInFileName;


    lcConverter.Convert;

    Check( not lcConverter.ConvertError, 'Obfuscate failed for ' +
      ExtractFileName(psInFileName));

    lsObsFileName := lcConverter.OutFileName;
    Check(lsObsFileName <> '', 'No obfuscated file');
    Check(FileExists(lsObsFileName), 'obfuscated file ' + lsObsFileName + ' not found');

    TestFileContentsSame(lsObsFileName, psRefObsOutput);

    // now deobfuscate
    FormatSettings.Obfuscate.Enabled := False;
    GetRegSettings.OutputExtension   := 'out';

    lcConverter.Clear;
    lcConverter.YesAll      := True;
    lcConverter.GuiMessages := False;
    lcConverter.SourceMode  := fmSingleFile;
    lcConverter.BackupMode  := cmSeparateOutput;

    lcConverter.Input := lsObsFileName;
    lcConverter.Convert;

    Check( not lcConverter.ConvertError, 'Reclarify failed for ' +
      ExtractFileName(lsObsFileName));

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

procedure TTestObfuscate.TestFileContentsSame(const psFileName1, psFileName2: string);
var
  lsFile1, lsFile2: string;
begin
  Check(FileExists(psFileName1), 'File ' + psFileName1 + ' does not exist');
  Check(FileExists(psFileName2), 'File ' + psFileName2 + ' does not exist');

  lsFile1 := string(FileToString(psFileName1));
  lsFile2 := string(FileToString(psFileName2));

  // check contents the same
  if (lsFile1 <> lsFile2) then
    Fail('Files differ ' + psFileName1 + ' and ' + psFileName2);
end;

procedure TTestObfuscate.TestFile(const psName: string);
var
  liLastDotPos: integer;
  lsInName, lsPrefix, lsObsFileName: string;
  lsRemadeFileName: string;
begin
  Assert(psName <> '');

  { does it have an file extension? }
  if Pos('.', psName) > 0 then
  begin
    liLastDotPos := StrLastPos('.', psName);
    lsInName      := psName;
    lsPrefix := StrLeft(psName, liLastDotPos);
    lsObsFileName := lsPrefix + 'obs';
    lsRemadeFileName := lsPrefix + 'out';
  end
  else
  begin
    lsInName      := psName + '.pas';
    lsObsFileName := psName + '.obs';
    lsRemadeFileName := psName + '.out';
  end;

  GetRegSettings.OutputExtension := 'obs';

  TestFile(GetTestFilesDir + lsInName,
    GetObsOutFilesDir + lsObsFileName, GetObsOutFilesDir + lsRemadeFileName)

  {
    // test re-obfuscating 

  FormatSettings.FileSettings.OutputExtension := 'out';

  TestObfuscateFile(GetTestFilesDir + lsObsFileName,
    GetObsOutFilesDir + lsRemadeFileName)
  }
end;

procedure TTestObfuscate.TestDelphiNetLibrary;
begin
   TestFile('TestDelphiNetLibrary');
end;

procedure TTestObfuscate.Empty1;
begin
  TestFile('EmptyTest1');
end;

procedure TTestObfuscate.fFormTest;
begin
  TestFile('fFormTest');
end;

procedure TTestObfuscate.LittleTest1;
begin
  TestFile('LittleTest1');
end;

procedure TTestObfuscate.LittleTest2;
begin
  TestFile('LittleTest2');
end;

procedure TTestObfuscate.LittleTest3;
begin
  TestFile('LittleTest3');
end;

procedure TTestObfuscate.LittleTest4;
begin
  TestFile('LittleTest4');
end;

procedure TTestObfuscate.LittleTest5;
begin
  TestFile('LittleTest5');
end;

procedure TTestObfuscate.LittleTest6;
begin
  TestFile('LittleTest6');
end;

procedure TTestObfuscate.LittleTest7;
begin
  TestFile('LittleTest7');
end;

procedure TTestObfuscate.LittleTest8;
begin
  TestFile('LittleTest8');
end;

procedure TTestObfuscate.TestAbsolute;
begin
  TestFile('TestAbsolute');
end;

procedure TTestObfuscate.TestAlign;
begin
  TestFile('TestAlign');
end;


procedure TTestObfuscate.TestAmpersand;
begin
  TestFile('TestAmpersand');
end;

procedure TTestObfuscate.TestAnonFunctionInInitialization;
begin
  TestFile('TestAnonFunctionInInitialization');
end;

procedure TTestObfuscate.TestArray;
begin
  TestFile('TestArray');
end;

procedure TTestObfuscate.TestAsm;
begin
  TestFile('TestAsm');
end;

procedure TTestObfuscate.TestBlankLineRemoval;
begin
  TestFile('TestBlankLineRemoval');
end;

procedure TTestObfuscate.TestBogusDirectives;
begin
  TestFile('TestBogusDirectives');
end;

procedure TTestObfuscate.TestBogusTypes;
begin
  TestFile('TestBogusTypes');
end;

procedure TTestObfuscate.TestCaseBlock;
begin
  TestFile('TestCaseBlock');
end;

procedure TTestObfuscate.TestCast;
begin
  TestFile('TestCast');
end;

procedure TTestObfuscate.TestCastSimple;
begin
  TestFile('TestCastSimple');
end;

procedure TTestObfuscate.TestCharLiterals;
begin
  TestFile('TestCharLiterals');
end;

procedure TTestObfuscate.TestClassLines;
begin
  TestFile('TestClassLines');
end;

procedure TTestObfuscate.TestClassMethods;
begin
  TestFile('TestClassMethods');
end;

procedure TTestObfuscate.TestClassOf;
begin
  TestFile('TestClassOf');
end;

procedure TTestObfuscate.TestClassVarEmpty;
begin
  TestFile('TestClassVarEmpty');
end;

procedure TTestObfuscate.TestCommentIndent;
begin
  TestFile('TestCommentIndent');
end;

procedure TTestObfuscate.TestCommentIndent2;
begin
  TestFile('TestCommentIndent2');
end;

procedure TTestObfuscate.TestComplexAsm2;
begin
  TestFile('TestComplexAsm2');
end;

procedure TTestObfuscate.TestConstRecords;
begin
  TestFile('TestConstRecords');
end;

procedure TTestObfuscate.TestD6;
begin
  TestFile('TestD6');
end;

procedure TTestObfuscate.TestDelphi2009AnonymousMethod;
begin
  TestFile('TestDelphi2009AnonymousMethod');
end;

procedure TTestObfuscate.TestDelphi2009Generics;
begin
  TestFile('TestDelphi2009Generics');
end;

procedure TTestObfuscate.TestDelphi2009Inherited;
begin
  TestFile('TestDelphi2009Inherited');
end;

procedure TTestObfuscate.TestDeclarations;
begin
  TestFile('TestDeclarations');
end;


procedure TTestObfuscate.TestDeclarations2;
begin
  TestFile('TestDeclarations2');
end;

procedure TTestObfuscate.TestDefaultParams;
begin
  TestFile('TestDefaultParams');
end;

procedure TTestObfuscate.TestEmptyClass;
begin
  TestFile('TestEmptyClass');
end;

procedure TTestObfuscate.TestEsotericKeywords;
begin
  TestFile('TestEsotericKeywords');
end;

procedure TTestObfuscate.TestExclusion;
begin
  TestFile('TestExclusion');
end;

procedure TTestObfuscate.TestExclusionFlags;
begin
  TestFile('TestExclusionFlags');
end;

procedure TTestObfuscate.TestExit;
begin
  TestFile('TestExit');
end;

procedure TTestObfuscate.TestExports;
begin
  TestFile('TestExports');
end;

procedure TTestObfuscate.TestExternal;
begin
  TestFile('TestExternal');
end;

procedure TTestObfuscate.TestForward;
begin
  TestFile('TestForward');
end;

procedure TTestObfuscate.TestGenericArray;
begin
  TestFile('TestGenericArray.dpr');
end;

procedure TTestObfuscate.TestGenericClassHelper;
begin
  TestFile('TestGenericClassHelper.dpr');
end;

procedure TTestObfuscate.TestGenericClassOperators;
begin
  TestFile('TestGenericClassOperators.dpr');
end;

procedure TTestObfuscate.TestGenericConstraintConstructor;
begin
  TestFile('TestGenericConstraintConstructor.dpr');
end;

procedure TTestObfuscate.TestGenericConstraints;
begin
  TestFile('TestGenericConstraints.dpr');
end;

procedure TTestObfuscate.TestGenericConstructorStatic;
begin
  TestFile('TestGenericConstructorStatic.dpr');
end;

procedure TTestObfuscate.TestGenericDelegates;
begin
  TestFile('TestGenericDelegates.dpr');
end;

procedure TTestObfuscate.TestGenericFunctions;
begin
  TestFile('TestGenericFunctions.dpr');
end;

procedure TTestObfuscate.TestGenericHeritage;
begin
  TestFile('TestGenericHeritage.dpr');
end;

procedure TTestObfuscate.TestGenericInheritance;
begin
  TestFile('TestGenericInheritance.dpr');
end;

procedure TTestObfuscate.TestGenericInterface;
begin
  TestFile('TestGenericInterface.dpr');
end;

procedure TTestObfuscate.TestGenericMethod;
begin
  TestFile('TestGenericMethod.dpr');
end;

procedure TTestObfuscate.TestGenericMethods1;
begin
  TestFile('TestGenericMethods1.dpr');
end;

procedure TTestObfuscate.TestGenericOperatorAs;
begin
 TestFile('TestGenericOperatorAs.dpr');
end;

procedure TTestObfuscate.TestGenericOperatorIs;
begin
 TestFile('TestGenericOperatorIs.dpr');
end;

procedure TTestObfuscate.TestGenerics;
begin
  TestFile('TestGenerics');
end;

procedure TTestObfuscate.TestGenerics2;
begin
  TestFile('TestGenerics2');
end;

procedure TTestObfuscate.TestGenericTypeNullable;
begin
  TestFile('TestGenericTypeNullable.dpr');
end;

procedure TTestObfuscate.TestGoto;
begin
  TestFile('TestGoto');
end;

procedure TTestObfuscate.TestHexConstantElse;
begin
  TestFile('TestHexConstantElse');
end;

procedure TTestObfuscate.TestInitFinal;
begin
  TestFile('TestInitFinal');
end;

procedure TTestObfuscate.TestInterfaceImplements;
begin
  TestFile('TestInterfaceImplements');
end;

procedure TTestObfuscate.TestInterfaceMap;
begin
  TestFile('TestInterfaceMap');
end;

procedure TTestObfuscate.TestInterfaces;
begin
  TestFile('TestInterfaces');
end;

procedure TTestObfuscate.TestLabelKeyword;
begin
  TestFile('TestLabelKeyword');
end;

procedure TTestObfuscate.TestLayout;
begin
  TestFile('TestLayout');
end;

procedure TTestObfuscate.TestLayoutBare;
begin
  TestFile('TestLayoutBare');
end;

procedure TTestObfuscate.TestLayoutBare2;
begin
  TestFile('TestLayoutBare2');
end;

procedure TTestObfuscate.TestLayoutBare3;
begin
  TestFile('TestLayoutBare3');
end;

procedure TTestObfuscate.TestLibExports;
begin
  TestFile('TestLibExports');
end;

procedure TTestObfuscate.TestLibrary;
begin
  TestFile('TestLibrary');
end;

procedure TTestObfuscate.TestLineBreaking;
begin
  TestFile('TestLineBreaking');
end;

procedure TTestObfuscate.TestLocalTypes;
begin
  TestFile('TestLocalTypes');
end;

procedure TTestObfuscate.TestLongStrings;
begin
  TestFile('TestLongStrings');
end;

procedure TTestObfuscate.TestMarcoV;
begin
  TestFile('TestMarcoV');
end;

procedure TTestObfuscate.TestTestMH;
begin
  TestFile('TestMH');
end;

procedure TTestObfuscate.TestMixedModeCaps;
begin
  TestFile('TestMixedModeCaps');
end;

procedure TTestObfuscate.TestMVB;
begin
  TestFile('TestMVB');
end;

procedure TTestObfuscate.TestNested;
begin
  TestFile('TestNested');
end;

procedure TTestObfuscate.TestNestedRecords;
begin
  TestFile('TestNestedRecords');
end;

procedure TTestObfuscate.TestNestedType;
begin
  TestFile('TestNestedType');
end;

procedure TTestObfuscate.TestOleParams;
begin
  TestFile('TestOleParams');
end;

procedure TTestObfuscate.TestOperators;
begin
  TestFile('TestOperators');
end;

procedure TTestObfuscate.TestOutKeyword;
begin
  TestFile('TestOutKeyword');
end;

procedure TTestObfuscate.TestPackedObject;
begin
  TestFile('TestPackedObject');
end;

procedure TTestObfuscate.TestParams;
begin
  TestFile('TestParams');
end;

procedure TTestObfuscate.TestParamSpaces;
begin
  TestFile('TestParamSpaces');
end;

procedure TTestObfuscate.TestPointers;
begin
  TestFile('TestPointers');
end;

procedure TTestObfuscate.TestProgram;
begin
  TestFile('TestProgram');
end;

procedure TTestObfuscate.TestProperties;
begin
  TestFile('TestProperties');
end;

procedure TTestObfuscate.TestPropertyLines;
begin
  TestFile('TestPropertyLines');
end;

procedure TTestObfuscate.TestRecords;
begin
  TestFile('TestRecords');
end;

procedure TTestObfuscate.TestRecordWithClassFunction;
begin
  TestFile('TestRecordWithClassFunction');
end;

procedure TTestObfuscate.TestReg;
begin
  TestFile('TestReg');
end;

procedure TTestObfuscate.TestReint;
begin
  TestFile('TestReint');
end;

procedure TTestObfuscate.TestReturnRemoval;
begin
  TestFile('TestReturnRemoval');
end;

procedure TTestObfuscate.TestReturns;
begin
  TestFile('TestReturns');
end;

procedure TTestObfuscate.TestRunOnConst;
begin
  TestFile('TestRunOnConst');
end;

procedure TTestObfuscate.TestRunOnDef;
begin
  TestFile('TestRunOnDef');
end;

procedure TTestObfuscate.TestRunOnLine;
begin
  TestFile('TestRunOnLine');
end;

procedure TTestObfuscate.TestTPObjects;
begin
  TestFile('TestTPObjects');
end;

procedure TTestObfuscate.TestTry;
begin
  TestFile('TestTry');
end;

procedure TTestObfuscate.TestTypeDefs;
begin
  TestFile('TestTypeDefs');
end;

procedure TTestObfuscate.TestUses;
begin
  TestFile('TestUses');
end;

procedure TTestObfuscate.TestUsesChanges;
begin
  TestFile('TestUsesChanges');
end;

procedure TTestObfuscate.TestVarArgs;
begin
  TestFile('TestVarArgs');
end;

procedure TTestObfuscate.TestVarParam;
begin
  TestFile('TestVarParam');
end;

procedure TTestObfuscate.TestWarnings;
begin
  TestFile('TestWarnings');
end;

procedure TTestObfuscate.TestWith;
begin
  TestFile('TestWith');
end;

procedure TTestObfuscate.TestCases;
begin
  TestFile('D11\Testcases.dpr');
end;

procedure TTestObfuscate.TestProcBlankLines;
begin
  TestFile('TestProcBlankLines.pas');
end;

procedure TTestObfuscate.LittleTest9;
begin
  TestFile('LittleTest9');
end;

procedure TTestObfuscate.TestDeref;
begin
  TestFile('TestDeref');
end;

procedure TTestObfuscate.TestPropertyInherited;
begin
  TestFile('TestPropertyInherited');
end;

procedure TTestObfuscate.TestMessages;
begin
  TestFile('TestMessages');
end;

procedure TTestObfuscate.LittleTest10;
begin
  TestFile('LittleTest10');
end;

procedure TTestObfuscate.TestInheritedExpr;
begin
  TestFile('TestInheritedExpr');
end;

procedure TTestObfuscate.LittleTest11;
begin
  TestFile('LittleTest11');
end;

procedure TTestObfuscate.LittleTest12;
begin
  TestFile('LittleTest12');
end;

procedure TTestObfuscate.LittleTest13;
begin
  TestFile('LittleTest13');
end;

procedure TTestObfuscate.LittleTest14;
begin
  TestFile('LittleTest14');
end;

procedure TTestObfuscate.LittleTest15;
begin
  TestFile('LittleTest15');
end;

procedure TTestObfuscate.LittleTest16;
begin
  TestFile('LittleTest16');
end;

procedure TTestObfuscate.LittleTest17;
begin
  TestFile('LittleTest17');
end;

procedure TTestObfuscate.LittleTest18;
begin
  TestFile('LittleTest18');
end;

procedure TTestObfuscate.TestAtExpr;
begin
  TestFile('TestAtExpr');
end;

procedure TTestObfuscate.TestAutomated;
begin
  TestFile('TestAutomated');
end;

procedure TTestObfuscate.TestAsmStructs;
begin
  TestFile('TestAsmStructs');
end;

procedure TTestObfuscate.TestAssignments;
begin
  TestFile('TestAssignments');
end;

procedure TTestObfuscate.TestUnicodeStrings;
begin
  TestFile('TestUnicodeStrings');
end;

procedure TTestObfuscate.TestUnicode_ansi;
begin
  TestFile('TestUnicode_ansi');
end;

procedure TTestObfuscate.TestUnicode_be_ucs2;
begin
  TestFile('TestUnicode_be_ucs2');
end;

procedure TTestObfuscate.TestUnicode_be_ucs4;
begin
  TestFile('TestUnicode_be_ucs4');
end;

procedure TTestObfuscate.TestUnicode_le_ucs2;
begin
  TestFile('TestUnicode_le_ucs2');
end;

procedure TTestObfuscate.TestUnicode_le_ucs4;
begin
  TestFile('TestUnicode_le_ucs4');
end;

procedure TTestObfuscate.TestUnicode_utf8;
begin
  TestFile('TestUnicode_utf8');
end;

procedure TTestObfuscate.TestUnitAllDirectives;
begin
  TestFile('TestUnitAllDirectives');
end;

procedure TTestObfuscate.TestUnitDeprecated;
begin
  TestFile('TestUnitDeprecated');
end;

procedure TTestObfuscate.TestUnitLibrary;
begin
  TestFile('TestUnitLibrary');
end;

procedure TTestObfuscate.TestUnitPlatform;
begin
  TestFile('TestUnitPlatform');
end;

procedure TTestObfuscate.LittleTest19;
begin
  TestFile('LittleTest19');
end;

procedure TTestObfuscate.TestRaise;
begin
  TestFile('TestRaise');
end;

procedure TTestObfuscate.LittleTest20;
begin
  TestFile('LittleTest20');
end;

procedure TTestObfuscate.LittleTest21;
begin
  TestFile('LittleTest21');
end;

procedure TTestObfuscate.LittleTest22;
begin
  TestFile('LittleTest22');
end;

procedure TTestObfuscate.LittleTest23;
begin
  TestFile('LittleTest23');
end;

procedure TTestObfuscate.LittleTest24;
begin
  TestFile('LittleTest24');
end;

procedure TTestObfuscate.LittleTest25;
begin
  TestFile('LittleTest25');
end;

procedure TTestObfuscate.LittleTest26;
begin
  TestFile('LittleTest26');
end;

procedure TTestObfuscate.LittleTest27;
begin
  TestFile('LittleTest27');
end;

procedure TTestObfuscate.TestEmptySquareBrackets;
begin
  TestFile('TestEmptySquareBrackets');
end;

procedure TTestObfuscate.LittleTest28;
begin
  TestFile('LittleTest28');
end;

procedure TTestObfuscate.LittleTest29;
begin
  TestFile('LittleTest29');
end;

procedure TTestObfuscate.LittleTest30;
begin
  TestFile('LittleTest30');
end;

procedure TTestObfuscate.LittleTest31;
begin
  TestFile('LittleTest31');
end;

procedure TTestObfuscate.LittleTest32;
begin
  TestFile('LittleTest32');
end;

procedure TTestObfuscate.LittleTest33;
begin
  TestFile('LittleTest33');
end;

procedure TTestObfuscate.TestCaseIfFormat;
begin
  TestFile('TestCaseIfFormat');
end;

procedure TTestObfuscate.TestEmptyCase;
begin
  TestFile('TestEmptyCase');
end;

procedure TTestObfuscate.LittleTest34;
begin
  TestFile('LittleTest34');
end;

procedure TTestObfuscate.LittleTest35;
begin
  TestFile('LittleTest35');
end;

procedure TTestObfuscate.LittleTest36;
begin
  TestFile('LittleTest36');
end;

procedure TTestObfuscate.LittleTest37;
begin
  TestFile('LittleTest37');
end;

procedure TTestObfuscate.LittleTest38;
begin
  TestFile('LittleTest38');
end;

procedure TTestObfuscate.LittleTest39;
begin
  TestFile('LittleTest39');
end;

procedure TTestObfuscate.LittleTest40;
begin
  TestFile('LittleTest40');
end;

procedure TTestObfuscate.TestSimpleIfdef;
begin
  TestFile('TestSimpleIfdef');
end;

procedure TTestObfuscate.TestSimpleIfdef2;
begin
  TestFile('TestSimpleIfdef2');
end;

procedure TTestObfuscate.TestSimpleIfdef3;
begin
  TestFile('TestSimpleIfdef3');
end;

procedure TTestObfuscate.TestSimpleIfdef4;
begin
  TestFile('TestSimpleIfdef4');
end;

procedure TTestObfuscate.TestSimpleIfdef5;
begin
  TestFile('TestSimpleIfdef5');
end;

procedure TTestObfuscate.TestDefines;
begin
  TestFile('TestDefines');
end;

procedure TTestObfuscate.LittleTest41;
begin
  TestFile('LittleTest41');
end;

procedure TTestObfuscate.LittleTest42;
begin
  TestFile('LittleTest42');
end;

procedure TTestObfuscate.LittleTest43;
begin
  TestFile('LittleTest43');
end;

procedure TTestObfuscate.LittleTest44;
begin
  TestFile('LittleTest44');
end;

procedure TTestObfuscate.LittleTest45;
begin
  TestFile('LittleTest45');
end;

procedure TTestObfuscate.LittleTest46;
begin
  TestFile('LittleTest46');
end;

procedure TTestObfuscate.LittleTest47;
begin
  TestFile('LittleTest47');
end;

procedure TTestObfuscate.TestWarnDestroy;
begin
  TestFile('TestWarnDestroy');
end;

procedure TTestObfuscate.LittleTest48;
begin
  TestFile('LittleTest48');
end;

procedure TTestObfuscate.LittleTest49;
begin
  TestFile('LittleTest49');
end;

procedure TTestObfuscate.LittleTest50;
begin
  TestFile('LittleTest50');
end;

procedure TTestObfuscate.LittleTest51;
begin
  TestFile('LittleTest51');
end;

procedure TTestObfuscate.LittleTest52;
begin
  TestFile('LittleTest52');
end;

procedure TTestObfuscate.TestSimpleIfdef6;
begin
  TestFile('TestSimpleIfdef6');
end;

procedure TTestObfuscate.TestSubrangeType;
begin
  TestFile('TestSubrangeType');
end;

procedure TTestObfuscate.LittleTest53;
begin
  TestFile('LittleTest53');
end;

procedure TTestObfuscate.LittleTest54;
begin
  TestFile('LittleTest54');
end;

procedure TTestObfuscate.LittleTest55;
begin
  TestFile('LittleTest55');
end;

procedure TTestObfuscate.LittleTest56;
begin
  TestFile('LittleTest56');
end;

procedure TTestObfuscate.LittleTest57;
begin
  TestFile('LittleTest57');
end;

procedure TTestObfuscate.LittleTest58;
begin
  TestFile('LittleTest58');
end;

procedure TTestObfuscate.LittleTest59;
begin
  TestFile('LittleTest59');
end;

procedure TTestObfuscate.LittleTest60;
begin
  TestFile('LittleTest60');
end;

procedure TTestObfuscate.LittleTest61;
begin
  TestFile('LittleTest61');
end;

procedure TTestObfuscate.LittleTest62;
begin
  TestFile('LittleTest62');
end;

procedure TTestObfuscate.TestInline;
begin
  TestFile('TestInline');
end;

procedure TTestObfuscate.fBracketProp;
begin
  TestFile('fBracketProp');
end;

procedure TTestObfuscate.TestEndElse;
begin
  TestFile('TestEndElse');
end;

procedure TTestObfuscate.TestCondReturns;
begin
  TestFile('TestCondReturns');
end;

procedure TTestObfuscate.TestDelphiNetUnsafe;
begin
  TestFile('TestDelphiNetUnsafe');
end;

procedure TTestObfuscate.TestDelphiNetUses;
begin
  TestFile('TestDelphiNetUses');
end;

procedure TTestObfuscate.TestConstBug;
begin
  TestFile('TestConstBug');
end;

procedure TTestObfuscate.TestForIn;
begin
  TestFile('TestForIn');
end;

procedure TTestObfuscate.TestDottedName;
begin
  TestFile('test.dotted.name.pas');
end;

procedure TTestObfuscate.TestDelphiNetClass;
begin
  TestFile('TestDelphiNetClass');
end;

procedure TTestObfuscate.TestDelphiNetConst;
begin
  TestFile('TestDelphiNetConst');
end;

procedure TTestObfuscate.TestDelphiNetDottedType;
begin
  TestFile('TestDelphiNetDottedType');
end;

procedure TTestObfuscate.TestDelphiNetDynamicArray;
begin
  TestFile('TestDelphiNetDynamicArray');
end;

procedure TTestObfuscate.TestDelphiNetStatic;
begin
  TestFile('TestDelphiNetStatic');
end;

procedure TTestObfuscate.TestTestDotNetForm1;
begin
  TestFile('TestDotNetForm1');
end;

procedure TTestObfuscate.TestDelphiNetHelperClass;
begin
  TestFile('TestDelphiNetHelperClass');
end;

procedure TTestObfuscate.TestDelphiNetNestedType;
begin
  TestFile('TestDelphiNetNestedType');
end;

procedure TTestObfuscate.TestDelphiNetNestedType2;
begin
  TestFile('TestDelphiNetNestedType2');
end;

procedure TTestObfuscate.TestDelphiNetOperatorOverload;
begin
  TestFile('TestDelphiNetOperatorOverload');
end;

procedure TTestObfuscate.TestDelphiNetRecordClassVars;
begin
  TestFile('TestDelphiNetRecordClassVars');
end;

procedure TTestObfuscate.TestDelphiNetRecordForward;
begin
  TestFile('TestDelphiNetRecordForward');
end;

procedure TTestObfuscate.TestDelphiNetRecordProcs;
begin
  TestFile('TestDelphiNetRecordProcs');
end;

procedure TTestObfuscate.TestCondCompBreaks;
begin
  TestFile('TestCondCompBreaks');
end;

procedure TTestObfuscate.TestCondCompBreaks2;
begin
  TestFile('TestCondCompBreaks2');
end;

procedure TTestObfuscate.TestDelphiNetAmpersandMethod;
begin
  TestFile('TestDelphiNetAmpersandMethod');
end;

procedure TTestObfuscate.TestDelphiNetAttributes;
begin
  TestFile('TestDelphiNetAttributes');
end;

procedure TTestObfuscate.TestDelphiNetWebService;
begin
  TestFile('TestDelphiNetWebService');
end;

procedure TTestObfuscate.TestDelphiNetWebService2;
begin
  TestFile('TestDelphiNetWebService2');
end;

procedure TTestObfuscate.TestDelphiNetKeywords;
begin
  TestFile('TestDelphiNetKeywords');
end;

procedure TTestObfuscate.TestDelphiNetMulticast;
begin
  TestFile('TestDelphiNetMulticast');
end;

procedure TTestObfuscate.TestDelphiNetClassVar;
begin
  TestFile('TestDelphiNetClassVar');
end;

procedure TTestObfuscate.TestTrailingCommaParam;
begin
  TestFile('TestTrailingCommaParam');
end;

procedure TTestObfuscate.TestTryExceptRaise;
begin
  TestFile('TestTryExceptRaise');
end;

procedure TTestObfuscate.TestDprNoBegin;
begin
   TestFile('TestDprNoBegin.dpr');
end;

procedure TTestObfuscate.TestDLLIndex;
begin
   TestFile('TestDLLIndex');
end;

procedure TTestObfuscate.TestIncAt;
begin
   TestFile('TestIncAt');
end;

procedure TTestObfuscate.TestDelphiNetFinalMethod;
begin
   TestFile('TestDelphiNetFinalMethod');
end;

procedure TTestObfuscate.TestDelphiNetSealedClass;
begin
   TestFile('TestDelphiNetSealedClass');
end;

procedure TTestObfuscate.TestAsmAnd;
begin
   TestFile('TestAsmAnd');
end;

procedure TTestObfuscate.TestAsmCaps;
begin
   TestFile('TestAsmCaps');
end;

procedure TTestObfuscate.TestAsmLabel;
begin
  TestFile('TestAsmLabel');
end;

procedure TTestObfuscate.TestAsmOffsetKeyword;
begin
  TestFile('TestAsmOffsetKeyword');
end;

procedure TTestObfuscate.TestAsmOps;
begin
  TestFile('TestAsmOps');
end;

initialization
  TestFramework.RegisterTest(TTestObfuscate.Suite);
end.
