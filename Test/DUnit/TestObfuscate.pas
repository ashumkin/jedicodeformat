unit TestObfuscate;

interface

uses
 TestFrameWork;

type
  TTestObfuscate = class(TTestCase)
  private
    procedure TestParseFile(const psInFileName, psRefOutput: string); overload;
    procedure TestParseFile(const psName: string); overload;

    procedure TestFileContentsSame(const psFileName1, psFileName2: string);

 published

    procedure TestParse_Empty1;
    procedure TestParse_fFormTest;
    procedure TestParse_LittleTest1;
    procedure TestParse_LittleTest2;
    procedure TestParse_LittleTest3;
    procedure TestParse_LittleTest4;
    procedure TestParse_LittleTest5;
    procedure TestParse_TestAbsolute;
    procedure TestParse_TestAlign;
    procedure TestParse_TestAsm;
    procedure TestParse_TestBlankLineRemoval;
    procedure TestParse_TestBogusDirectives;
    procedure TestParse_TestBogusTypes;
    procedure TestParse_TestCaseBlock;
    procedure TestParse_TestCast;
    procedure TestParse_TestCharLiterals;
    procedure TestParse_TestClassLines;
    procedure TestParse_TestCommentIndent;
    procedure TestParse_TestConstRecords;
    procedure TestParse_TestD6;
    procedure TestParse_TestDeclarations;
    procedure TestParse_TestDeclarations2;
    procedure TestParse_TestDefaultParams;
    procedure TestParse_TestEmptyClass;
    procedure TestParse_TestEsotericKeywords;
    procedure TestParse_TestExclusion;
    procedure TestParse_TestExclusionFlags;
    procedure TestParse_TestExternal;
    procedure TestParse_TestForward;
    procedure TestParse_TestGoto;
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
    procedure TestParse_TestMVB;
    procedure TestParse_TestNested;
    procedure TestParse_TestNestedRecords;
    procedure TestParse_TestOperators;
    procedure TestParse_TestParams;
    procedure TestParse_TestParamSpaces;
    procedure TestParse_TestPointers;
    procedure TestParse_TestProgram;
    procedure TestParse_TestProperties;
    procedure TestParse_TestPropertyLines;
    procedure TestParse_TestRecords;
    procedure TestParse_TestReg;
    procedure TestParse_TestReint;
    procedure TestParse_TestReturnRemoval;
    procedure TestParse_TestReturns;
    procedure TestParse_TestRunOnConst;
    procedure TestParse_TestRunOnDef;
    procedure TestParse_TestRunOnLine;
    procedure TestParse_TestSimpleCast;
    procedure TestParse_TestTestMH;
    procedure TestParse_TestTPObjects;
    procedure TestParse_TestTry;
    procedure TestParse_TestTypeDefs;
    procedure TestParse_TestUses;
    procedure TestParse_TestUsesChanges;
    procedure TestParse_TestWarnings;

   procedure TestParse_TestCases;
end;

implementation

uses
  { delphi } SysUtils,
  JclStrings,
  FileConverter, ConvertTypes, JcfSettings;

const
  TEST_FILES_DIR = 'C:\Code\Delphi\JcfCheckout\CodeFormat\Jcf2\Test\TestCases\';
  OBS_OUT_FILES_DIR = 'C:\Code\Delphi\JcfCheckout\CodeFormat\Jcf2\Test\TestCases\ObfuscatedOut\';


procedure TTestObfuscate.TestParseFile(const psInFileName,
  psRefOutput: string);
var
  lcConverter: TFileConverter;
  lsOutFileName: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  Settings.Obfuscate.Enabled := True;

  // Check(FileExists(psRefOutput), 'reference output file ' + psRefOutput + ' not found');

  lcConverter := TFileConverter.Create;
  try
    lcConverter.YesAll := True;
    lcConverter.GuiMessages := False;

    lcConverter.SourceMode := fmSingleFile;
    lcConverter.BackupMode := cmSeperateOutput;

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
    Settings.Obfuscate.Enabled := False;
  end;

  TestFileContentsSame(lsOutFileName, psRefOutput);
end;

procedure TTestObfuscate.TestFileContentsSame(const psFileName1,
  psFileName2: string);
var
  lsFile1, lsFile2: string;
begin
  lsFile1 := FileToString(psFileName1);
  lsFile2 := FileToString(psFileName2);

  // check contents the same 
  if (lsFile1 <> lsFile2) then
    Fail('Files differ ' + psFileName1 + ' and ' + psFileName2);
end;

procedure TTestObfuscate.TestParseFile(const psName: string);
var
  lsInName, lsObsFileName, lsRemadeFileName: string;
begin
  { does it have an file extension? }
  if Pos('.', psName) > 0 then
  begin
    lsInName := psName;
    lsObsFileName := StrBefore('.', psName) + '.obs';
    lsRemadeFileName := StrBefore('.', psName) + '.out';
  end
  else
  begin
    lsInName := psName + '.pas';
    lsObsFileName := psName + '.obs';
    lsRemadeFileName := psName + '.out';
  end;

  Settings.FileSettings.OutputExtension := 'obs';

  TestParseFile(TEST_FILES_DIR + lsInName,
    OBS_OUT_FILES_DIR + lsObsFileName)

  {
    // test re-obfuscating 

  Settings.FileSettings.OutputExtension := 'out';

  TestParseFile(TEST_FILES_DIR + lsObsFileName,
    OBS_OUT_FILES_DIR + lsRemadeFileName)
  }
end;

procedure TTestObfuscate.TestParse_Empty1;
begin
  TestParseFile('EmptyTest1');
end;

procedure TTestObfuscate.TestParse_fFormTest;
begin
  TestParseFile('fFormTest');
end;

procedure TTestObfuscate.TestParse_LittleTest1;
begin
  TestParseFile('LittleTest1');
end;

procedure TTestObfuscate.TestParse_LittleTest2;
begin
  TestParseFile('LittleTest2');
end;

procedure TTestObfuscate.TestParse_LittleTest3;
begin
  TestParseFile('LittleTest3');
end;

procedure TTestObfuscate.TestParse_LittleTest4;
begin
  TestParseFile('LittleTest4');
end;

procedure TTestObfuscate.TestParse_LittleTest5;
begin
  TestParseFile('LittleTest5');
end;

procedure TTestObfuscate.TestParse_TestAbsolute;
begin
  TestParseFile('TestAbsolute');
end;

procedure TTestObfuscate.TestParse_TestAlign;
begin
  TestParseFile('TestAlign');
end;

procedure TTestObfuscate.TestParse_TestAsm;
begin
  TestParseFile('TestAsm');
end;

procedure TTestObfuscate.TestParse_TestBlankLineRemoval;
begin
  TestParseFile('TestBlankLineRemoval');
end;

procedure TTestObfuscate.TestParse_TestBogusDirectives;
begin
  TestParseFile('TestBogusDirectives');
end;

procedure TTestObfuscate.TestParse_TestBogusTypes;
begin
  TestParseFile('TestBogusTypes');
end;

procedure TTestObfuscate.TestParse_TestCaseBlock;
begin
  TestParseFile('TestCaseBlock');
end;

procedure TTestObfuscate.TestParse_TestCast;
begin
  TestParseFile('TestCast');
end;

procedure TTestObfuscate.TestParse_TestSimpleCast;
begin
  TestParseFile('TestCastSimple');
end;

procedure TTestObfuscate.TestParse_TestCharLiterals;
begin
  TestParseFile('TestCharLiterals');
end;

procedure TTestObfuscate.TestParse_TestClassLines;
begin
  TestParseFile('TestClassLines');
end;

procedure TTestObfuscate.TestParse_TestCommentIndent;
begin
  TestParseFile('TestCommentIndent');
end;

procedure TTestObfuscate.TestParse_TestConstRecords;
begin
  TestParseFile('TestConstRecords');
end;

procedure TTestObfuscate.TestParse_TestD6;
begin
  TestParseFile('TestD6');
end;

procedure TTestObfuscate.TestParse_TestDeclarations;
begin
  TestParseFile('TestDeclarations');
end;


procedure TTestObfuscate.TestParse_TestDeclarations2;
begin
  TestParseFile('TestDeclarations2');
end;

procedure TTestObfuscate.TestParse_TestDefaultParams;
begin
  TestParseFile('TestDefaultParams');
end;

procedure TTestObfuscate.TestParse_TestEmptyClass;
begin
  TestParseFile('TestEmptyClass');
end;

procedure TTestObfuscate.TestParse_TestEsotericKeywords;
begin
  TestParseFile('TestEsotericKeywords');
end;

procedure TTestObfuscate.TestParse_TestExclusion;
begin
  TestParseFile('TestExclusion');
end;

procedure TTestObfuscate.TestParse_TestExclusionFlags;
begin
  TestParseFile('TestExclusionFlags');
end;

procedure TTestObfuscate.TestParse_TestExternal;
begin
  TestParseFile('TestExternal');
end;

procedure TTestObfuscate.TestParse_TestForward;
begin
  TestParseFile('TestForward');
end;

procedure TTestObfuscate.TestParse_TestGoto;
begin
  TestParseFile('TestGoto');
end;

procedure TTestObfuscate.TestParse_TestInitFinal;
begin
  TestParseFile('TestInitFinal');
end;

procedure TTestObfuscate.TestParse_TestInterfaceImplements;
begin
  TestParseFile('TestInterfaceImplements');
end;

procedure TTestObfuscate.TestParse_TestInterfaceMap;
begin
  TestParseFile('TestInterfaceMap');
end;

procedure TTestObfuscate.TestParse_TestInterfaces;
begin
  TestParseFile('TestInterfaces');
end;

procedure TTestObfuscate.TestParse_TestLayout;
begin
  TestParseFile('TestLayout');
end;

procedure TTestObfuscate.TestParse_TestLayoutBare;
begin
  TestParseFile('TestLayoutBare');
end;

procedure TTestObfuscate.TestParse_TestLayoutBare2;
begin
  TestParseFile('TestLayoutBare2');
end;

procedure TTestObfuscate.TestParse_TestLayoutBare3;
begin
  TestParseFile('TestLayoutBare3');
end;

procedure TTestObfuscate.TestParse_TestLibExports;
begin
  TestParseFile('TestLibExports');
end;

procedure TTestObfuscate.TestParse_TestLineBreaking;
begin
  TestParseFile('TestLineBreaking');
end;

procedure TTestObfuscate.TestParse_TestLocalTypes;
begin
  TestParseFile('TestLocalTypes');
end;

procedure TTestObfuscate.TestParse_TestLongStrings;
begin
  TestParseFile('TestLongStrings');
end;

procedure TTestObfuscate.TestParse_TestMarcoV;
begin
  TestParseFile('TestMarcoV');
end;

procedure TTestObfuscate.TestParse_TestTestMH;
begin
  TestParseFile('TestMH');
end;

procedure TTestObfuscate.TestParse_TestMixedModeCaps;
begin
  TestParseFile('TestMixedModeCaps');
end;

procedure TTestObfuscate.TestParse_TestMVB;
begin
  TestParseFile('TestMVB');
end;

procedure TTestObfuscate.TestParse_TestNested;
begin
  TestParseFile('TestNested');
end;

procedure TTestObfuscate.TestParse_TestNestedRecords;
begin
  TestParseFile('TestNestedRecords');
end;

procedure TTestObfuscate.TestParse_TestOperators;
begin
  TestParseFile('TestOperators');
end;

procedure TTestObfuscate.TestParse_TestParams;
begin
  TestParseFile('TestParams');
end;

procedure TTestObfuscate.TestParse_TestParamSpaces;
begin
  TestParseFile('TestParamSpaces');
end;

procedure TTestObfuscate.TestParse_TestPointers;
begin
  TestParseFile('TestPointers');
end;

procedure TTestObfuscate.TestParse_TestProgram;
begin
  TestParseFile('TestProgram');
end;

procedure TTestObfuscate.TestParse_TestProperties;
begin
  TestParseFile('TestProperties');
end;

procedure TTestObfuscate.TestParse_TestPropertyLines;
begin
  TestParseFile('TestPropertyLines');
end;

procedure TTestObfuscate.TestParse_TestRecords;
begin
  TestParseFile('TestRecords');
end;

procedure TTestObfuscate.TestParse_TestReg;
begin
  TestParseFile('TestReg');
end;

procedure TTestObfuscate.TestParse_TestReint;
begin
  TestParseFile('TestReint');
end;

procedure TTestObfuscate.TestParse_TestReturnRemoval;
begin
  TestParseFile('TestReturnRemoval');
end;

procedure TTestObfuscate.TestParse_TestReturns;
begin
  TestParseFile('TestReturns');
end;

procedure TTestObfuscate.TestParse_TestRunOnConst;
begin
  TestParseFile('TestRunOnConst');
end;

procedure TTestObfuscate.TestParse_TestRunOnDef;
begin
  TestParseFile('TestRunOnDef');
end;

procedure TTestObfuscate.TestParse_TestRunOnLine;
begin
  TestParseFile('TestRunOnLine');
end;

procedure TTestObfuscate.TestParse_TestTPObjects;
begin
  TestParseFile('TestTPObjects');
end;

procedure TTestObfuscate.TestParse_TestTry;
begin
  TestParseFile('TestTry');
end;

procedure TTestObfuscate.TestParse_TestTypeDefs;
begin
  TestParseFile('TestTypeDefs');
end;

procedure TTestObfuscate.TestParse_TestUses;
begin
  TestParseFile('TestUses');
end;

procedure TTestObfuscate.TestParse_TestUsesChanges;
begin
  TestParseFile('TestUsesChanges');
end;

procedure TTestObfuscate.TestParse_TestWarnings;
begin
  TestParseFile('TestWarnings');
end;

procedure TTestObfuscate.TestParse_TestCases;
begin
  TestParseFile('Testcases.dpr');
end;

initialization
 TestFramework.RegisterTest(TTestObfuscate.Suite);
end.
