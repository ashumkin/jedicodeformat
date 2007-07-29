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

{ obfuscate and reclarify all test files }

uses
  TestFrameWork;

type
  TTestObfuscate = class(TTestCase)
  private
    procedure TestObfuscateFile(const psInFileName, psRefObsOutput,
      psRefClearOutput: string); overload;
    procedure TestObfuscateFile(const psName: string); overload;

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

    procedure TestDelphiNetLibrary;

  end;

implementation

uses
  { delphi }
  Windows, SysUtils, 
  { JCL }
  JclStrings,
  { local }
  FileConverter, ConvertTypes, JcfSettings, JcfRegistrySettings,
  TestConstants;

procedure TTestObfuscate.Setup;
begin
  inherited;

  InitTestSettings;
end;

procedure TTestObfuscate.TestObfuscateFile(
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
    lcConverter.BackupMode := cmSeperateOutput;

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
    lcConverter.BackupMode  := cmSeperateOutput;

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

  lsFile1 := FileToString(psFileName1);
  lsFile2 := FileToString(psFileName2);

  // check contents the same 
  if (lsFile1 <> lsFile2) then
    Fail('Files differ ' + psFileName1 + ' and ' + psFileName2);
end;

procedure TTestObfuscate.TestObfuscateFile(const psName: string);
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

  TestObfuscateFile(GetTestFilesDir + lsInName,
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
   TestObfuscateFile('TestDelphiNetLibrary');
end;

procedure TTestObfuscate.Empty1;
begin
  TestObfuscateFile('EmptyTest1');
end;

procedure TTestObfuscate.fFormTest;
begin
  TestObfuscateFile('fFormTest');
end;

procedure TTestObfuscate.LittleTest1;
begin
  TestObfuscateFile('LittleTest1');
end;

procedure TTestObfuscate.LittleTest2;
begin
  TestObfuscateFile('LittleTest2');
end;

procedure TTestObfuscate.LittleTest3;
begin
  TestObfuscateFile('LittleTest3');
end;

procedure TTestObfuscate.LittleTest4;
begin
  TestObfuscateFile('LittleTest4');
end;

procedure TTestObfuscate.LittleTest5;
begin
  TestObfuscateFile('LittleTest5');
end;

procedure TTestObfuscate.LittleTest6;
begin
  TestObfuscateFile('LittleTest6');
end;

procedure TTestObfuscate.LittleTest7;
begin
  TestObfuscateFile('LittleTest7');
end;

procedure TTestObfuscate.LittleTest8;
begin
  TestObfuscateFile('LittleTest8');
end;

procedure TTestObfuscate.TestAbsolute;
begin
  TestObfuscateFile('TestAbsolute');
end;

procedure TTestObfuscate.TestAlign;
begin
  TestObfuscateFile('TestAlign');
end;


procedure TTestObfuscate.TestAmpersand;
begin
  TestObfuscateFile('TestAmpersand');
end;

procedure TTestObfuscate.TestArray;
begin
  TestObfuscateFile('TestArray');
end;

procedure TTestObfuscate.TestAsm;
begin
  TestObfuscateFile('TestAsm');
end;

procedure TTestObfuscate.TestBlankLineRemoval;
begin
  TestObfuscateFile('TestBlankLineRemoval');
end;

procedure TTestObfuscate.TestBogusDirectives;
begin
  TestObfuscateFile('TestBogusDirectives');
end;

procedure TTestObfuscate.TestBogusTypes;
begin
  TestObfuscateFile('TestBogusTypes');
end;

procedure TTestObfuscate.TestCaseBlock;
begin
  TestObfuscateFile('TestCaseBlock');
end;

procedure TTestObfuscate.TestCast;
begin
  TestObfuscateFile('TestCast');
end;

procedure TTestObfuscate.TestCastSimple;
begin
  TestObfuscateFile('TestCastSimple');
end;

procedure TTestObfuscate.TestCharLiterals;
begin
  TestObfuscateFile('TestCharLiterals');
end;

procedure TTestObfuscate.TestClassLines;
begin
  TestObfuscateFile('TestClassLines');
end;

procedure TTestObfuscate.TestClassMethods;
begin
  TestObfuscateFile('TestClassMethods');
end;

procedure TTestObfuscate.TestCommentIndent;
begin
  TestObfuscateFile('TestCommentIndent');
end;

procedure TTestObfuscate.TestCommentIndent2;
begin
  TestObfuscateFile('TestCommentIndent2');
end;

procedure TTestObfuscate.TestComplexAsm2;
begin
  TestObfuscateFile('TestComplexAsm2');
end;

procedure TTestObfuscate.TestConstRecords;
begin
  TestObfuscateFile('TestConstRecords');
end;

procedure TTestObfuscate.TestD6;
begin
  TestObfuscateFile('TestD6');
end;

procedure TTestObfuscate.TestDeclarations;
begin
  TestObfuscateFile('TestDeclarations');
end;


procedure TTestObfuscate.TestDeclarations2;
begin
  TestObfuscateFile('TestDeclarations2');
end;

procedure TTestObfuscate.TestDefaultParams;
begin
  TestObfuscateFile('TestDefaultParams');
end;

procedure TTestObfuscate.TestEmptyClass;
begin
  TestObfuscateFile('TestEmptyClass');
end;

procedure TTestObfuscate.TestEsotericKeywords;
begin
  TestObfuscateFile('TestEsotericKeywords');
end;

procedure TTestObfuscate.TestExclusion;
begin
  TestObfuscateFile('TestExclusion');
end;

procedure TTestObfuscate.TestExclusionFlags;
begin
  TestObfuscateFile('TestExclusionFlags');
end;

procedure TTestObfuscate.TestExports;
begin
  TestObfuscateFile('TestExports');
end;

procedure TTestObfuscate.TestExternal;
begin
  TestObfuscateFile('TestExternal');
end;

procedure TTestObfuscate.TestForward;
begin
  TestObfuscateFile('TestForward');
end;

procedure TTestObfuscate.TestGoto;
begin
  TestObfuscateFile('TestGoto');
end;

procedure TTestObfuscate.TestInitFinal;
begin
  TestObfuscateFile('TestInitFinal');
end;

procedure TTestObfuscate.TestInterfaceImplements;
begin
  TestObfuscateFile('TestInterfaceImplements');
end;

procedure TTestObfuscate.TestInterfaceMap;
begin
  TestObfuscateFile('TestInterfaceMap');
end;

procedure TTestObfuscate.TestInterfaces;
begin
  TestObfuscateFile('TestInterfaces');
end;

procedure TTestObfuscate.TestLabelKeyword;
begin
  TestObfuscateFile('TestLabelKeyword');
end;

procedure TTestObfuscate.TestLayout;
begin
  TestObfuscateFile('TestLayout');
end;

procedure TTestObfuscate.TestLayoutBare;
begin
  TestObfuscateFile('TestLayoutBare');
end;

procedure TTestObfuscate.TestLayoutBare2;
begin
  TestObfuscateFile('TestLayoutBare2');
end;

procedure TTestObfuscate.TestLayoutBare3;
begin
  TestObfuscateFile('TestLayoutBare3');
end;

procedure TTestObfuscate.TestLibExports;
begin
  TestObfuscateFile('TestLibExports');
end;

procedure TTestObfuscate.TestLineBreaking;
begin
  TestObfuscateFile('TestLineBreaking');
end;

procedure TTestObfuscate.TestLocalTypes;
begin
  TestObfuscateFile('TestLocalTypes');
end;

procedure TTestObfuscate.TestLongStrings;
begin
  TestObfuscateFile('TestLongStrings');
end;

procedure TTestObfuscate.TestMarcoV;
begin
  TestObfuscateFile('TestMarcoV');
end;

procedure TTestObfuscate.TestTestMH;
begin
  TestObfuscateFile('TestMH');
end;

procedure TTestObfuscate.TestMixedModeCaps;
begin
  TestObfuscateFile('TestMixedModeCaps');
end;

procedure TTestObfuscate.TestMVB;
begin
  TestObfuscateFile('TestMVB');
end;

procedure TTestObfuscate.TestNested;
begin
  TestObfuscateFile('TestNested');
end;

procedure TTestObfuscate.TestNestedRecords;
begin
  TestObfuscateFile('TestNestedRecords');
end;

procedure TTestObfuscate.TestOleParams;
begin
  TestObfuscateFile('TestOleParams');
end;

procedure TTestObfuscate.TestOperators;
begin
  TestObfuscateFile('TestOperators');
end;

procedure TTestObfuscate.TestParams;
begin
  TestObfuscateFile('TestParams');
end;

procedure TTestObfuscate.TestParamSpaces;
begin
  TestObfuscateFile('TestParamSpaces');
end;

procedure TTestObfuscate.TestPointers;
begin
  TestObfuscateFile('TestPointers');
end;

procedure TTestObfuscate.TestProgram;
begin
  TestObfuscateFile('TestProgram');
end;

procedure TTestObfuscate.TestProperties;
begin
  TestObfuscateFile('TestProperties');
end;

procedure TTestObfuscate.TestPropertyLines;
begin
  TestObfuscateFile('TestPropertyLines');
end;

procedure TTestObfuscate.TestRecords;
begin
  TestObfuscateFile('TestRecords');
end;

procedure TTestObfuscate.TestReg;
begin
  TestObfuscateFile('TestReg');
end;

procedure TTestObfuscate.TestReint;
begin
  TestObfuscateFile('TestReint');
end;

procedure TTestObfuscate.TestReturnRemoval;
begin
  TestObfuscateFile('TestReturnRemoval');
end;

procedure TTestObfuscate.TestReturns;
begin
  TestObfuscateFile('TestReturns');
end;

procedure TTestObfuscate.TestRunOnConst;
begin
  TestObfuscateFile('TestRunOnConst');
end;

procedure TTestObfuscate.TestRunOnDef;
begin
  TestObfuscateFile('TestRunOnDef');
end;

procedure TTestObfuscate.TestRunOnLine;
begin
  TestObfuscateFile('TestRunOnLine');
end;

procedure TTestObfuscate.TestTPObjects;
begin
  TestObfuscateFile('TestTPObjects');
end;

procedure TTestObfuscate.TestTry;
begin
  TestObfuscateFile('TestTry');
end;

procedure TTestObfuscate.TestTypeDefs;
begin
  TestObfuscateFile('TestTypeDefs');
end;

procedure TTestObfuscate.TestUses;
begin
  TestObfuscateFile('TestUses');
end;

procedure TTestObfuscate.TestUsesChanges;
begin
  TestObfuscateFile('TestUsesChanges');
end;

procedure TTestObfuscate.TestVarArgs;
begin
  TestObfuscateFile('TestVarArgs');
end;

procedure TTestObfuscate.TestVarParam;
begin
  TestObfuscateFile('TestVarParam');
end;

procedure TTestObfuscate.TestWarnings;
begin
  TestObfuscateFile('TestWarnings');
end;

procedure TTestObfuscate.TestWith;
begin
  TestObfuscateFile('TestWith');
end;

procedure TTestObfuscate.TestCases;
begin
  TestObfuscateFile('Testcases.dpr');
end;

procedure TTestObfuscate.TestProcBlankLines;
begin
  TestObfuscateFile('TestProcBlankLines.pas');
end;

procedure TTestObfuscate.LittleTest9;
begin
  TestObfuscateFile('LittleTest9');
end;

procedure TTestObfuscate.TestDeref;
begin
  TestObfuscateFile('TestDeref');
end;

procedure TTestObfuscate.TestPropertyInherited;
begin
  TestObfuscateFile('TestPropertyInherited');
end;

procedure TTestObfuscate.TestMessages;
begin
  TestObfuscateFile('TestMessages');
end;

procedure TTestObfuscate.LittleTest10;
begin
  TestObfuscateFile('LittleTest10');
end;

procedure TTestObfuscate.TestInheritedExpr;
begin
  TestObfuscateFile('TestInheritedExpr');
end;

procedure TTestObfuscate.LittleTest11;
begin
  TestObfuscateFile('LittleTest11');
end;

procedure TTestObfuscate.LittleTest12;
begin
  TestObfuscateFile('LittleTest12');
end;

procedure TTestObfuscate.LittleTest13;
begin
  TestObfuscateFile('LittleTest13');
end;

procedure TTestObfuscate.LittleTest14;
begin
  TestObfuscateFile('LittleTest14');
end;

procedure TTestObfuscate.LittleTest15;
begin
  TestObfuscateFile('LittleTest15');
end;

procedure TTestObfuscate.LittleTest16;
begin
  TestObfuscateFile('LittleTest16');
end;

procedure TTestObfuscate.LittleTest17;
begin
  TestObfuscateFile('LittleTest17');
end;

procedure TTestObfuscate.LittleTest18;
begin
  TestObfuscateFile('LittleTest18');
end;

procedure TTestObfuscate.TestAtExpr;
begin
  TestObfuscateFile('TestAtExpr');
end;

procedure TTestObfuscate.TestAutomated;
begin
  TestObfuscateFile('TestAutomated');
end;

procedure TTestObfuscate.TestAsmStructs;
begin
  TestObfuscateFile('TestAsmStructs');
end;

procedure TTestObfuscate.TestUnitAllDirectives;
begin
  TestObfuscateFile('TestUnitAllDirectives');
end;

procedure TTestObfuscate.TestUnitDeprecated;
begin
  TestObfuscateFile('TestUnitDeprecated');
end;

procedure TTestObfuscate.TestUnitLibrary;
begin
  TestObfuscateFile('TestUnitLibrary');
end;

procedure TTestObfuscate.TestUnitPlatform;
begin
  TestObfuscateFile('TestUnitPlatform');
end;

procedure TTestObfuscate.LittleTest19;
begin
  TestObfuscateFile('LittleTest19');
end;

procedure TTestObfuscate.TestRaise;
begin
  TestObfuscateFile('TestRaise');
end;

procedure TTestObfuscate.LittleTest20;
begin
  TestObfuscateFile('LittleTest20');
end;

procedure TTestObfuscate.LittleTest21;
begin
  TestObfuscateFile('LittleTest21');
end;

procedure TTestObfuscate.LittleTest22;
begin
  TestObfuscateFile('LittleTest22');
end;

procedure TTestObfuscate.LittleTest23;
begin
  TestObfuscateFile('LittleTest23');
end;

procedure TTestObfuscate.LittleTest24;
begin
  TestObfuscateFile('LittleTest24');
end;

procedure TTestObfuscate.LittleTest25;
begin
  TestObfuscateFile('LittleTest25');
end;

procedure TTestObfuscate.LittleTest26;
begin
  TestObfuscateFile('LittleTest26');
end;

procedure TTestObfuscate.LittleTest27;
begin
  TestObfuscateFile('LittleTest27');
end;

procedure TTestObfuscate.TestEmptySquareBrackets;
begin
  TestObfuscateFile('TestEmptySquareBrackets');
end;

procedure TTestObfuscate.LittleTest28;
begin
  TestObfuscateFile('LittleTest28');
end;

procedure TTestObfuscate.LittleTest29;
begin
  TestObfuscateFile('LittleTest29');
end;

procedure TTestObfuscate.LittleTest30;
begin
  TestObfuscateFile('LittleTest30');
end;

procedure TTestObfuscate.LittleTest31;
begin
  TestObfuscateFile('LittleTest31');
end;

procedure TTestObfuscate.LittleTest32;
begin
  TestObfuscateFile('LittleTest32');
end;

procedure TTestObfuscate.LittleTest33;
begin
  TestObfuscateFile('LittleTest33');
end;

procedure TTestObfuscate.TestCaseIfFormat;
begin
  TestObfuscateFile('TestCaseIfFormat');
end;

procedure TTestObfuscate.TestEmptyCase;
begin
  TestObfuscateFile('TestEmptyCase');
end;

procedure TTestObfuscate.LittleTest34;
begin
  TestObfuscateFile('LittleTest34');
end;

procedure TTestObfuscate.LittleTest35;
begin
  TestObfuscateFile('LittleTest35');
end;

procedure TTestObfuscate.LittleTest36;
begin
  TestObfuscateFile('LittleTest36');
end;

procedure TTestObfuscate.LittleTest37;
begin
  TestObfuscateFile('LittleTest37');
end;

procedure TTestObfuscate.LittleTest38;
begin
  TestObfuscateFile('LittleTest38');
end;

procedure TTestObfuscate.LittleTest39;
begin
  TestObfuscateFile('LittleTest39');
end;

procedure TTestObfuscate.LittleTest40;
begin
  TestObfuscateFile('LittleTest40');
end;

procedure TTestObfuscate.TestSimpleIfdef;
begin
  TestObfuscateFile('TestSimpleIfdef');
end;

procedure TTestObfuscate.TestSimpleIfdef2;
begin
  TestObfuscateFile('TestSimpleIfdef2');
end;

procedure TTestObfuscate.TestSimpleIfdef3;
begin
  TestObfuscateFile('TestSimpleIfdef3');
end;

procedure TTestObfuscate.TestSimpleIfdef4;
begin
  TestObfuscateFile('TestSimpleIfdef4');
end;

procedure TTestObfuscate.TestSimpleIfdef5;
begin
  TestObfuscateFile('TestSimpleIfdef5');
end;

procedure TTestObfuscate.TestDefines;
begin
  TestObfuscateFile('TestDefines');
end;

procedure TTestObfuscate.LittleTest41;
begin
  TestObfuscateFile('LittleTest41');
end;

procedure TTestObfuscate.LittleTest42;
begin
  TestObfuscateFile('LittleTest42');
end;

procedure TTestObfuscate.LittleTest43;
begin
  TestObfuscateFile('LittleTest43');
end;

procedure TTestObfuscate.LittleTest44;
begin
  TestObfuscateFile('LittleTest44');
end;

procedure TTestObfuscate.LittleTest45;
begin
  TestObfuscateFile('LittleTest45');
end;

procedure TTestObfuscate.LittleTest46;
begin
  TestObfuscateFile('LittleTest46');
end;

procedure TTestObfuscate.LittleTest47;
begin
  TestObfuscateFile('LittleTest47');
end;

procedure TTestObfuscate.TestWarnDestroy;
begin
  TestObfuscateFile('TestWarnDestroy');
end;

procedure TTestObfuscate.LittleTest48;
begin
  TestObfuscateFile('LittleTest48');
end;

procedure TTestObfuscate.LittleTest49;
begin
  TestObfuscateFile('LittleTest49');
end;

procedure TTestObfuscate.LittleTest50;
begin
  TestObfuscateFile('LittleTest50');
end;

procedure TTestObfuscate.LittleTest51;
begin
  TestObfuscateFile('LittleTest51');
end;

procedure TTestObfuscate.LittleTest52;
begin
  TestObfuscateFile('LittleTest52');
end;

procedure TTestObfuscate.TestSimpleIfdef6;
begin
  TestObfuscateFile('TestSimpleIfdef6');
end;

procedure TTestObfuscate.TestSubrangeType;
begin
  TestObfuscateFile('TestSubrangeType');
end;

procedure TTestObfuscate.LittleTest53;
begin
  TestObfuscateFile('LittleTest53');
end;

procedure TTestObfuscate.LittleTest54;
begin
  TestObfuscateFile('LittleTest54');
end;

procedure TTestObfuscate.LittleTest55;
begin
  TestObfuscateFile('LittleTest55');
end;

procedure TTestObfuscate.LittleTest56;
begin
  TestObfuscateFile('LittleTest56');
end;

procedure TTestObfuscate.LittleTest57;
begin
  TestObfuscateFile('LittleTest57');
end;

procedure TTestObfuscate.LittleTest58;
begin
  TestObfuscateFile('LittleTest58');
end;

procedure TTestObfuscate.LittleTest59;
begin
  TestObfuscateFile('LittleTest59');
end;

procedure TTestObfuscate.LittleTest60;
begin
  TestObfuscateFile('LittleTest60');
end;

procedure TTestObfuscate.LittleTest61;
begin
  TestObfuscateFile('LittleTest61');
end;

procedure TTestObfuscate.LittleTest62;
begin
  TestObfuscateFile('LittleTest62');
end;

procedure TTestObfuscate.TestInline;
begin
  TestObfuscateFile('TestInline');
end;

procedure TTestObfuscate.fBracketProp;
begin
  TestObfuscateFile('fBracketProp');
end;

procedure TTestObfuscate.TestEndElse;
begin
  TestObfuscateFile('TestEndElse');
end;

procedure TTestObfuscate.TestCondReturns;
begin
  TestObfuscateFile('TestCondReturns');
end;

procedure TTestObfuscate.TestDelphiNetUnsafe;
begin
  TestObfuscateFile('TestDelphiNetUnsafe');
end;

procedure TTestObfuscate.TestDelphiNetUses;
begin
  TestObfuscateFile('TestDelphiNetUses');
end;

procedure TTestObfuscate.TestConstBug;
begin
  TestObfuscateFile('TestConstBug');
end;

procedure TTestObfuscate.TestForIn;
begin
  TestObfuscateFile('TestForIn');
end;

procedure TTestObfuscate.TestDottedName;
begin
  TestObfuscateFile('test.dotted.name.pas');
end;

procedure TTestObfuscate.TestDelphiNetClass;
begin
  TestObfuscateFile('TestDelphiNetClass');
end;

procedure TTestObfuscate.TestDelphiNetConst;
begin
  TestObfuscateFile('TestDelphiNetConst');
end;

procedure TTestObfuscate.TestDelphiNetDottedType;
begin
  TestObfuscateFile('TestDelphiNetDottedType');
end;

procedure TTestObfuscate.TestDelphiNetDynamicArray;
begin
  TestObfuscateFile('TestDelphiNetDynamicArray');
end;

procedure TTestObfuscate.TestDelphiNetStatic;
begin
  TestObfuscateFile('TestDelphiNetStatic');
end;

procedure TTestObfuscate.TestTestDotNetForm1;
begin
  TestObfuscateFile('TestDotNetForm1');
end;

procedure TTestObfuscate.TestDelphiNetHelperClass;
begin
  TestObfuscateFile('TestDelphiNetHelperClass');
end;

procedure TTestObfuscate.TestDelphiNetNestedType;
begin
  TestObfuscateFile('TestDelphiNetNestedType');
end;

procedure TTestObfuscate.TestDelphiNetNestedType2;
begin
  TestObfuscateFile('TestDelphiNetNestedType2');
end;

procedure TTestObfuscate.TestDelphiNetOperatorOverload;
begin
  TestObfuscateFile('TestDelphiNetOperatorOverload');
end;

procedure TTestObfuscate.TestDelphiNetRecordClassVars;
begin
  TestObfuscateFile('TestDelphiNetRecordClassVars');
end;

procedure TTestObfuscate.TestDelphiNetRecordForward;
begin
  TestObfuscateFile('TestDelphiNetRecordForward');
end;

procedure TTestObfuscate.TestDelphiNetRecordProcs;
begin
  TestObfuscateFile('TestDelphiNetRecordProcs');
end;

procedure TTestObfuscate.TestCondCompBreaks;
begin
  TestObfuscateFile('TestCondCompBreaks');
end;

procedure TTestObfuscate.TestCondCompBreaks2;
begin
  TestObfuscateFile('TestCondCompBreaks2');
end;

procedure TTestObfuscate.TestDelphiNetAmpersandMethod;
begin
  TestObfuscateFile('TestDelphiNetAmpersandMethod');
end;

procedure TTestObfuscate.TestDelphiNetAttributes;
begin
  TestObfuscateFile('TestDelphiNetAttributes');
end;

procedure TTestObfuscate.TestDelphiNetWebService;
begin
  TestObfuscateFile('TestDelphiNetWebService');
end;

procedure TTestObfuscate.TestDelphiNetWebService2;
begin
  TestObfuscateFile('TestDelphiNetWebService2');
end;

procedure TTestObfuscate.TestDelphiNetKeywords;
begin
  TestObfuscateFile('TestDelphiNetKeywords');
end;

procedure TTestObfuscate.TestDelphiNetMulticast;
begin
  TestObfuscateFile('TestDelphiNetMulticast');
end;

procedure TTestObfuscate.TestDelphiNetClassVar;
begin
  TestObfuscateFile('TestDelphiNetClassVar');
end;

procedure TTestObfuscate.TestTrailingCommaParam;
begin
  TestObfuscateFile('TestTrailingCommaParam');
end;

procedure TTestObfuscate.TestTryExceptRaise;
begin
  TestObfuscateFile('TestTryExceptRaise');
end;

procedure TTestObfuscate.TestDprNoBegin;
begin
   TestObfuscateFile('TestDprNoBegin.dpr');
end;

procedure TTestObfuscate.TestDLLIndex;
begin
   TestObfuscateFile('TestDLLIndex');
end;

procedure TTestObfuscate.TestIncAt;
begin
   TestObfuscateFile('TestIncAt');
end;

procedure TTestObfuscate.TestDelphiNetFinalMethod;
begin
   TestObfuscateFile('TestDelphiNetFinalMethod');
end;

procedure TTestObfuscate.TestDelphiNetSealedClass;
begin
   TestObfuscateFile('TestDelphiNetSealedClass');
end;

procedure TTestObfuscate.TestAsmAnd;
begin
   TestObfuscateFile('TestAsmAnd');
end;

procedure TTestObfuscate.TestAsmLabel;
begin
  TestObfuscateFile('TestAsmLabel');
end;

procedure TTestObfuscate.TestAsmOps;
begin
  TestObfuscateFile('TestAsmOps');
end;

initialization
  TestFramework.RegisterTest(TTestObfuscate.Suite);
end.
