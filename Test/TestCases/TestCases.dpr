program TestCases;

uses
  Forms,
  EmptyTest1 in 'EmptyTest1.pas',
  fFormTest in 'fFormTest.pas' {FormTest},
  LittleTest1 in 'LittleTest1.pas',
  LittleTest2 in 'LittleTest2.pas',
  LittleTest3 in 'LittleTest3.pas',
  LittleTest4 in 'LittleTest4.pas',
  LittleTest5 in 'LittleTest5.pas',
  TestAbsolute in 'TestAbsolute.pas',
  TestAlign in 'TestAlign.pas',
  TestBlankLineRemoval in 'TestBlankLineRemoval.pas',
  TestBogusDirectives in 'TestBogusDirectives.pas',
  TestBogusTypes in 'TestBogusTypes.pas',
  TestCaseBlock in 'TestCaseBlock.pas',
  TestCast in 'TestCast.pas',
  TestCastSimple in 'TestCastSimple.pas',
  TestCharLiterals in 'TestCharLiterals.pas',
  TestClassLines in 'TestClassLines.pas',
  TestCommentIndent in 'TestCommentIndent.pas',
  TestConstRecords in 'TestConstRecords.pas',
  TestD6 in 'TestD6.pas',
  TestDeclarations2 in 'TestDeclarations2.pas',
  TestDeclarations in 'TestDeclarations.pas',
  TestDefaultParams in 'TestDefaultParams.pas',
  TestEmptyClass in 'TestEmptyClass.pas',
  TestEsotericKeywords in 'TestEsotericKeywords.pas',
  TestExclusion in 'TestExclusion.pas',
  TestExclusionFlags in 'TestExclusionFlags.pas',
  TestForward in 'TestForward.pas',
  TestGoto in 'TestGoto.pas',
  TestInitFinal in 'TestInitFinal.pas',
  TestInterfaceImplements in 'TestInterfaceImplements.pas',
  TestInterfaces in 'TestInterfaces.pas',
  TestLayout in 'TestLayout.pas',
  TestLayoutBare2 in 'TestLayoutBare2.pas',
  TestLayoutBare3 in 'TestLayoutBare3.pas',
  TestLayoutBare in 'TestLayoutBare.pas',
  TestLineBreaking in 'TestLineBreaking.pas',
  TestLocalTypes in 'TestLocalTypes.pas',
  TestLongStrings in 'TestLongStrings.pas',
  TestMarcoV in 'TestMarcoV.pas',
  TestMH in 'TestMH.pas',
  TestMixedModeCaps in 'TestMixedModeCaps.pas',
  TestNested in 'TestNested.pas',
  TestNestedRecords in 'TestNestedRecords.pas',
  TestOperators in 'TestOperators.pas',
  TestParams in 'TestParams.pas',
  TestParamSpaces in 'TestParamSpaces.pas',
  TestPointers in 'TestPointers.pas',
  TestProperties in 'TestProperties.pas',
  TestPropertyLines in 'TestPropertyLines.pas',
  TestRecords in 'TestRecords.pas',
  TestReg in 'TestReg.pas',
  TestReint in 'TestReint.pas',
  TestReturnRemoval in 'TestReturnRemoval.pas',
  TestReturns in 'TestReturns.pas',
  TestRunOnConst in 'TestRunOnConst.pas',
  TestRunOnDef in 'TestRunOnDef.pas',
  TestTry in 'TestTry.pas',
  TestTypeDefs in 'TestTypeDefs.pas',
  TestUses in 'TestUses.pas',
  TestWarnings in 'TestWarnings.pas',
  TestWith in 'TestWith.pas',
  LittleTest6 in 'LittleTest6.pas',
  TestArray in 'TestArray.pas',
  TestVarParam in 'TestVarParam.pas',
  LittleTest7 in 'LittleTest7.pas',
  LittleTest8 in 'LittleTest8.pas',
  TestDeref in 'TestDeref.pas',
  LittleTest9 in 'LittleTest9.pas',
  TestPropertyInherited in 'TestPropertyInherited.pas',
  TestMessages in 'TestMessages.pas',
  LittleTest10 in 'LittleTest10.pas',
  TestInheritedExpr in 'TestInheritedExpr.pas',
  LittleTest11 in 'LittleTest11.pas',
  LittleTest12 in 'LittleTest12.pas',
  LittleTest13 in 'LittleTest13.pas',
  TestOleParams in 'TestOleParams.pas',
  LittleTest14 in 'LittleTest14.pas',
  LittleTest15 in 'LittleTest15.pas',
  LittleTest16 in 'LittleTest16.pas',
  LittleTest17 in 'LittleTest17.pas',
  LittleTest18 in 'LittleTest18.pas',
  TestAtExpr in 'TestAtExpr.pas',
  TestAsmStructs in 'TestAsmStructs.pas',
  TestUnitPlatform in 'TestUnitPlatform.pas',
  TestUnitAllDirectives in 'TestUnitAllDirectives.pas',
  TestUnitDeprecated in 'TestUnitDeprecated.pas',
  TestUnitLibrary in 'TestUnitLibrary.pas',
  LittleTest19 in 'LittleTest19.pas',
  LittleTest20 in 'LittleTest20.pas',
  TestRaise in 'TestRaise.pas',
  LittleTest21 in 'LittleTest21.pas',
  LittleTest22 in 'LittleTest22.pas',
  LittleTest23 in 'LittleTest23.pas',
  TestEmptySquareBrackets in 'TestEmptySquareBrackets.pas',
  LittleTest24 in 'LittleTest24.pas',
  LittleTest25 in 'LittleTest25.pas',
  LittleTest26 in 'LittleTest26.pas',
  LittleTest27 in 'LittleTest27.pas',
  TestAsm in 'TestAsm.pas',
  testCaseIfFormat in 'testCaseIfFormat.pas',
  LittleTest28 in 'LittleTest28.pas',
  TestEmptyCase in 'TestEmptyCase.pas',
  LittleTest29 in 'LittleTest29.pas',
  LittleTest30 in 'LittleTest30.pas',
  LittleTest31 in 'LittleTest31.pas',
  LittleTest32 in 'LittleTest32.pas',
  LittleTest33 in 'LittleTest33.pas',
  LittleTest34 in 'LittleTest34.pas',
  TestSimpleIfdef in 'TestSimpleIfdef.pas',
  TestSimpleIfdef2 in 'TestSimpleIfdef2.pas',
  TestSimpleIfdef3 in 'TestSimpleIfdef3.pas',
  LittleTest35 in 'LittleTest35.pas',
  LittleTest36 in 'LittleTest36.pas',
  LittleTest37 in 'LittleTest37.pas',
  TestSimpleIfDef4 in 'TestSimpleIfDef4.pas',
  TestSimpleIfDef5 in 'TestSimpleIfDef5.pas',
  LittleTest38 in 'LittleTest38.pas',
  LittleTest39 in 'LittleTest39.pas',
  LittleTest40 in 'LittleTest40.pas',
  TestDefines in 'TestDefines.pas',
  LittleTest41 in 'LittleTest41.pas',
  LittleTest42 in 'LittleTest42.pas',
  LittleTest43 in 'LittleTest43.pas',
  LittleTest44 in 'LittleTest44.pas',
  LittleTest45 in 'LittleTest45.pas',
  LittleTest46 in 'LittleTest46.pas',
  LittleTest47 in 'LittleTest47.pas',
  TestWarnDestroy in 'TestWarnDestroy.pas',
  LittleTest48 in 'LittleTest48.pas',
  LittleTest49 in 'LittleTest49.pas',
  LittleTest50 in 'LittleTest50.pas',
  LittleTest51 in 'LittleTest51.pas',
  LittleTest52 in 'LittleTest52.pas',
  TestSimpleIfDef6 in 'TestSimpleIfDef6.pas',
  LittleTest53 in 'LittleTest53.pas',
  LittleTest54 in 'LittleTest54.pas',
  LittleTest55 in 'LittleTest55.pas',
  LittleTest56 in 'LittleTest56.pas',
  LittleTest57 in 'LittleTest57.pas',
  LittleTest58 in 'LittleTest58.pas',
  LittleTest59 in 'LittleTest59.pas',
  LittleTest60 in 'LittleTest60.pas',
  TestCommentIndent2 in 'TestCommentIndent2.pas',
  fBracketProp in 'fBracketProp.pas' {Form1},
  TestEndElse in 'TestEndElse.pas',
  TestCondReturns in 'TestCondReturns.pas',
  TestConstBug in 'TestConstBug.pas',
  test.dotted.name in 'test.dotted.name.pas',
  TestCondCompBreaks in 'TestCondCompBreaks.pas',
  TestDelphiNetKeywords in 'TestDelphiNetKeywords.pas',
  TestIncAt in 'TestIncAt.pas',
  TestDLLIndex in 'TestDLLIndex.pas',
  TestASMAnd in 'TestASMAnd.pas',
  TestAsmLabel in 'TestAsmLabel.pas',
  TestVarArgs in 'TestVarArgs.pas',
  TestAsmOps in 'TestAsmOps.pas',
  TestComplexAsm2 in 'TestComplexAsm2.pas',
  TestSubrangeType in 'TestSubrangeType.pas',
  TestAmpersand in 'TestAmpersand.pas',
  TestAutomated in 'TestAutomated.pas',
  TestClassMethods in 'TestClassMethods.pas',
  TestExports in 'TestExports.pas',
  TestAsmCaps in 'TestAsmCaps.pas',
  TestAsmOffsetKeyword in 'TestAsmOffsetKeyword.pas',
  TestClassVarEmpty in 'TestClassVarEmpty.pas',
  TestRecordWithClassFunction in 'TestRecordWithClassFunction.pas',
  TestOutKeyword in 'TestOutKeyword.pas',
  TestExit in 'TestExit.pas',
  TestHexConstantElse in 'TestHexConstantElse.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
