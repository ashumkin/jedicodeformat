unit TestFullClarify;

{ test the full clarify - all processes }

interface

uses
 TestFrameWork;

type
  TTestClarify = class(TTestCase)
  private
    procedure TestClarifyFile(const psInFileName, psRefOutput: string); overload;
    procedure TestClarifyFile(const psName: string); overload;

    procedure TestFileContentsSame(const psFileName1, psFileName2: string);

 published
  { one test for each file}
  procedure TestClarify_EmptyTest1;
  procedure TestClarify_fFormTest;
  procedure TestClarify_LittleTest1;
  procedure TestClarify_LittleTest2;
  procedure TestClarify_LittleTest3;
  procedure TestClarify_LittleTest4;
  procedure TestClarify_LittleTest5;

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
end;


implementation

uses
  { delphi } SysUtils,
  JclStrings,
  FileConverter, ConvertTypes, JcfSettings;

const
  TEST_FILES_DIR = 'C:\Code\JcfCheckout\CodeFormat\Jcf2\Test\TestCases\';
  CLEAR_OUT_FILES_DIR = 'C:\Code\JcfCheckout\CodeFormat\Jcf2\Test\TestCases\Out\';


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

  Settings.FileSettings.OutputExtension := 'out';

  TestClarifyFile(TEST_FILES_DIR + lsInName,
    CLEAR_OUT_FILES_DIR + lsClearFileName)
end;

procedure TTestClarify.TestClarifyFile(const psInFileName,
  psRefOutput: string);
var
  lcConverter: TFileConverter;
  lsOutFileName: string;
begin
  Check(FileExists(psInFileName), 'input file ' + psInFileName + ' not found');
  Settings.Obfuscate.Enabled := False;

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


procedure TTestClarify.TestFileContentsSame(const psFileName1,
  psFileName2: string);
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

initialization
 TestFramework.RegisterTest(TTestClarify.Suite);
end.

