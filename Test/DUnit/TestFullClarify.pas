unit TestFullClarify;

{ test the full clarify - all processes }

interface

uses
 TestFile;

type
  TTestClarify = class(TTestFile)
  private
    procedure TestClarifyFile(const psInFileName, psRefOutput: string); overload;
    procedure TestClarifyFile(const psName: string); overload;


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

  procedure TestClarify_TestAbsolute;
  procedure TestClarify_TestAlign;
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
  procedure TestClarify_TestEmptyClass;
  procedure TestClarify_TestEsotericKeywords;
  procedure TestClarify_TestExclusion;
  procedure TestClarify_TestExclusionFlags;
  procedure TestClarify_TestExternal;
  procedure TestClarify_TestForward;
  procedure TestClarify_TestGoto;
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
  procedure TestClarify_TestMH;
  procedure TestClarify_TestMixedModeCaps;
  procedure TestClarify_TestMVB;
  procedure TestClarify_TestNested;
  procedure TestClarify_TestNestedRecords;
  procedure TestClarify_TestOperators;
  procedure TestClarify_TestParams;
  procedure TestClarify_TestParamSpaces;
  procedure TestClarify_TestPointers;
  procedure TestClarify_TestProgram;
  procedure TestClarify_TestProperties;
  procedure TestClarify_TestPropertyLines;
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
  procedure TestClarify_TestWarnings;
  procedure TestClarify_TestWith;

  procedure TestClarify_TestPackage;
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

{ TTestClarify }

procedure TTestClarify.TestClarifyFile(const psName: string);
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


  TestClarifyFile(TEST_FILES_DIR + lsInName,
    REF_OUT_FILES_DIR + lsClearFileName)
end;

procedure TTestClarify.TestClarifyFile(const psInFileName,
  psRefOutput: string);
var
  lcConverter: TFileConverter;
  lsOutFileName, lsSettingsFileName: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  FormatSettings.Obfuscate.Enabled := False;

  // Check(FileExists(psRefOutput), 'reference output file ' + psRefOutput + ' not found');

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll := True;
    lcConverter.GuiMessages := False;

    { see also TestFileParse }
    lsSettingsFileName := TEST_FILES_DIR + '\JCFTestSettings.cfg';
    Check(FileExists(lsSettingsFileName), 'Settings file ' + lsSettingsFileName + ' not found');

    FormatSettings.ReadFromFile(lsSettingsFileName);
    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeperateOutput;
    FormatSettings.Obfuscate.Enabled := False;

    GetRegSettings.OutputExtension := 'out';
    lcConverter.Input := psInFileName;
    lcConverter.Convert;

    Check(not lcConverter.ConvertError, 'Convert failed for ' +
      ExtractFileName(psInFileName) +
      ' : ' + lcConverter.ConvertErrorMessage);

    lsOutFileName := lcConverter.OutFileName;
    Check(lsOutFileName <> '', 'No output file');
    Check(FileExists(lsOutFileName), 'output file ' + lsOutFileName + ' not found');

  finally
    lcConverter.Free;
    FormatSettings.Obfuscate.Enabled := False;
  end;

  TestFileContentsSame(lsOutFileName, psRefOutput);
end;



procedure TTestClarify.TestClarify_EmptyTest1;
begin
  TestClarifyFile('EmptyTest1');
end;

procedure TTestClarify.TestClarify_fFormTest;
begin
  TestClarifyFile('fFormTest');
end;

procedure TTestClarify.TestClarify_LittleTest1;
begin
  TestClarifyFile('LittleTest1');
end;

procedure TTestClarify.TestClarify_LittleTest2;
begin
  TestClarifyFile('LittleTest2');
end;

procedure TTestClarify.TestClarify_LittleTest3;
begin
  TestClarifyFile('LittleTest3');
end;

procedure TTestClarify.TestClarify_LittleTest4;
begin
  TestClarifyFile('LittleTest4');
end;

procedure TTestClarify.TestClarify_LittleTest5;
begin
  TestClarifyFile('LittleTest5');
end;

procedure TTestClarify.TestClarify_LittleTest6;
begin
  TestClarifyFile('LittleTest6');
end;

procedure TTestClarify.TestClarify_TestAbsolute;
begin
  TestClarifyFile('TestAbsolute');
end;

procedure TTestClarify.TestClarify_TestAlign;
begin
  TestClarifyFile('TestAlign');
end;

procedure TTestClarify.TestClarify_TestAsm;
begin
  TestClarifyFile('TestAsm');
end;


procedure TTestClarify.TestClarify_TestBlankLineRemoval;
begin
  TestClarifyFile('TestBlankLineRemoval');
end;

procedure TTestClarify.TestClarify_TestBogusDirectives;
begin
  TestClarifyFile('TestBogusDirectives');
end;

procedure TTestClarify.TestClarify_TestBogusTypes;
begin
  TestClarifyFile('TestBogusTypes');
end;

procedure TTestClarify.TestClarify_TestCaseBlock;
begin
  TestClarifyFile('TestCaseBlock');
end;

procedure TTestClarify.TestClarify_TestCast;
begin
  TestClarifyFile('TestCast');
end;

procedure TTestClarify.TestClarify_TestCastSimple;
begin
  TestClarifyFile('TestCastSimple');
end;

procedure TTestClarify.TestClarify_TestCharLiterals;
begin
  TestClarifyFile('TestCharLiterals');
end;

procedure TTestClarify.TestClarify_TestClassLines;
begin
  TestClarifyFile('TestClassLines');
end;

procedure TTestClarify.TestClarify_TestCommentIndent;
begin
  TestClarifyFile('TestCommentIndent');
end;

procedure TTestClarify.TestClarify_TestConstRecords;
begin
  TestClarifyFile('TestConstRecords');
end;

procedure TTestClarify.TestClarify_TestD6;
begin
  TestClarifyFile('TestD6');
end;

procedure TTestClarify.TestClarify_TestDeclarations;
begin
  TestClarifyFile('TestDeclarations');
end;

procedure TTestClarify.TestClarify_TestDeclarations2;
begin
  TestClarifyFile('TestDeclarations2');
end;

procedure TTestClarify.TestClarify_TestDefaultParams;
begin
  TestClarifyFile('TestDefaultParams');
end;

procedure TTestClarify.TestClarify_TestEmptyClass;
begin
  TestClarifyFile('TestEmptyClass');
end;

procedure TTestClarify.TestClarify_TestEsotericKeywords;
begin
  TestClarifyFile('TestEsotericKeywords');
end;

procedure TTestClarify.TestClarify_TestExclusion;
begin
  TestClarifyFile('TestExclusion');
end;

procedure TTestClarify.TestClarify_TestExclusionFlags;
begin
  TestClarifyFile('TestExclusionFlags');
end;

procedure TTestClarify.TestClarify_TestExternal;
begin
  TestClarifyFile('TestExternal');
end;

procedure TTestClarify.TestClarify_TestForward;
begin
  TestClarifyFile('TestForward');
end;

procedure TTestClarify.TestClarify_TestGoto;
begin
  TestClarifyFile('TestGoto');
end;

procedure TTestClarify.TestClarify_TestInitFinal;
begin
  TestClarifyFile('TestInitFinal');
end;

procedure TTestClarify.TestClarify_TestInterfaceImplements;
begin
  TestClarifyFile('TestInterfaceImplements');
end;

procedure TTestClarify.TestClarify_TestInterfaceMap;
begin
  TestClarifyFile('TestInterfaceMap');
end;

procedure TTestClarify.TestClarify_TestInterfaces;
begin
  TestClarifyFile('TestInterfaces');
end;

procedure TTestClarify.TestClarify_TestLayout;
begin
  TestClarifyFile('TestLayout');
end;

procedure TTestClarify.TestClarify_TestLayoutBare;
begin
  TestClarifyFile('TestLayoutBare');
end;

procedure TTestClarify.TestClarify_TestLayoutBare2;
begin
  TestClarifyFile('TestLayoutBare2');
end;

procedure TTestClarify.TestClarify_TestLayoutBare3;
begin
  TestClarifyFile('TestLayoutBare3');
end;

procedure TTestClarify.TestClarify_TestLibExports;
begin
  TestClarifyFile('TestLibExports');
end;

procedure TTestClarify.TestClarify_TestLineBreaking;
begin
  TestClarifyFile('TestLineBreaking');
end;

procedure TTestClarify.TestClarify_TestLocalTypes;
begin
  TestClarifyFile('TestLocalTypes');
end;

procedure TTestClarify.TestClarify_TestLongStrings;
begin
  TestClarifyFile('TestLongStrings');
end;

procedure TTestClarify.TestClarify_TestMarcoV;
begin
  TestClarifyFile('TestMarcoV');
end;

procedure TTestClarify.TestClarify_TestMH;
begin
  TestClarifyFile('TestMH');
end;

procedure TTestClarify.TestClarify_TestMixedModeCaps;
begin
  TestClarifyFile('TestMixedModeCaps');
end;

procedure TTestClarify.TestClarify_TestMVB;
begin
  TestClarifyFile('TestMVB');
end;

procedure TTestClarify.TestClarify_TestNested;
begin
  TestClarifyFile('TestNested');
end;

procedure TTestClarify.TestClarify_TestNestedRecords;
begin
  TestClarifyFile('TestNestedRecords');
end;

procedure TTestClarify.TestClarify_TestOperators;
begin
  TestClarifyFile('TestOperators');
end;

procedure TTestClarify.TestClarify_TestPackage;
begin
  TestClarifyFile('TestMe.dpk');
end;

procedure TTestClarify.TestClarify_TestParams;
begin
  TestClarifyFile('TestParams');
end;

procedure TTestClarify.TestClarify_TestParamSpaces;
begin
  TestClarifyFile('TestParamSpaces');
end;

procedure TTestClarify.TestClarify_TestPointers;
begin
  TestClarifyFile('TestPointers');
end;

procedure TTestClarify.TestClarify_TestProgram;
begin
  TestClarifyFile('TestProgram');
end;

procedure TTestClarify.TestClarify_TestProperties;
begin
  TestClarifyFile('TestProperties');
end;

procedure TTestClarify.TestClarify_TestPropertyLines;
begin
  TestClarifyFile('TestPropertyLines');
end;

procedure TTestClarify.TestClarify_TestRecords;
begin
  TestClarifyFile('TestRecords');
end;

procedure TTestClarify.TestClarify_TestReg;
begin
  TestClarifyFile('TestReg');
end;

procedure TTestClarify.TestClarify_TestReint;
begin
  TestClarifyFile('TestReint');
end;

procedure TTestClarify.TestClarify_TestReturnRemoval;
begin
  TestClarifyFile('TestReturnRemoval');
end;

procedure TTestClarify.TestClarify_TestReturns;
begin
  TestClarifyFile('TestReturns');
end;

procedure TTestClarify.TestClarify_TestRunOnConst;
begin
  TestClarifyFile('TestRunOnConst');
end;

procedure TTestClarify.TestClarify_TestRunOnDef;
begin
  TestClarifyFile('TestRunOnDef');
end;

procedure TTestClarify.TestClarify_TestRunOnLine;
begin
  TestClarifyFile('TestRunOnLine');
end;

procedure TTestClarify.TestClarify_TestTPObjects;
begin
  TestClarifyFile('TestTPObjects');
end;

procedure TTestClarify.TestClarify_TestTry;
begin
  TestClarifyFile('TestTry');
end;

procedure TTestClarify.TestClarify_TestTypeDefs;
begin
  TestClarifyFile('TestTypeDefs');
end;

procedure TTestClarify.TestClarify_TestUses;
begin
  TestClarifyFile('TestUses');
end;

procedure TTestClarify.TestClarify_TestUsesChanges;
begin
  TestClarifyFile('TestUsesChanges');
end;

procedure TTestClarify.TestClarify_TestWarnings;
begin
  TestClarifyFile('TestWarnings');
end;

procedure TTestClarify.TestClarify_TestWith;
begin
  TestClarifyFile('TestWith');
end;

initialization
 TestFramework.RegisterTest(TTestClarify.Suite);
end.

