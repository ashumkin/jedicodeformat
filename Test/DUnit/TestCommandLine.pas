unit TestCommandLine;

{ AFS 26 April 03
  test the jcf.exe commandline }

interface

uses
  { delphi }
  Classes,
  { jcf }
  TestFile, TestConstants;

type
  TTestCommandline = class(TTestFile)
  private
    fsJcfParams: string;
    fsRefDir: string;
    fsFileMask: string;

    fsFileNames: TStringList;

    procedure CompareFileToRef(const psFileName: string);

    procedure RunJcfCommandline;
    procedure GetOutFiles;

  protected
    procedure SetUp; override;
    procedure TearDown; override;


  published
    procedure TestFormatClarify;
    procedure TestFormatObfuscate;

end;

implementation

uses
  { delphi }
  SysUtils,
  { jcl }
  JclShell, JclFileUtils,
  TestFramework;

const
  EXPECTED_FILE_COUNT = 73;

procedure TTestCommandline.SetUp;
begin
  fsFileNames := TStringList.Create;
end;

procedure TTestCommandline.TearDown;
begin
  FreeAndNil(fsFileNames);
end;


procedure TTestCommandline.GetOutFiles;
begin
  fsFileNames.Clear;
  BuildFileList(TEST_FILES_DIR + fsFileMask, faAnyFile, fsFileNames);
end;

procedure TTestCommandline.CompareFileToRef(const psFileName: string);
begin
  TestFileContentsSame(TEST_FILES_DIR + psFileName,  fsRefDir + psFileName);
end;

procedure TTestCommandline.RunJcfCommandline;
var
  lsJcfExe: string;
  liLoop: integer;
  lbRes: boolean;
begin
  Assert(fsJcfParams <> '');
  Assert(fsRefDir <> '');
  Assert(fsFileMask <> '');

  // delete the output files
  GetOutFiles;

  if fsFileNames.Count > 0 then
  begin
    for liLoop := 0 to fsFileNames.Count - 1 do
      DeleteFile(TEST_FILES_DIR + fsFileNames[liLoop]);

    // should be none left
    GetOutFiles;
  end;

  CheckEquals(0, fsFileNames.Count);


  // build them again
  lsJcfExe := EXE_FILES_DIR + 'jcf.exe';
  Check(FileExists(lsJcfExe));

  lbRes := ShellExecAndWait(lsJcfExe, fsJcfParams);
  Check(lbRes);

  // should be back
  GetOutFiles;
  CheckEquals(EXPECTED_FILE_COUNT, fsFileNames.Count);

  // for each, compare to the reference versions
  GetOutFiles;

  for liLoop := 0 to fsFileNames.Count - 1 do
    CompareFileToRef(fsFileNames[liLoop]);
end;



procedure TTestCommandline.TestFormatClarify;
begin
  fsJcfParams := ' -config=' + TEST_FILES_DIR + 'JCFTestSettings.cfg -out -D ' + TEST_FILES_DIR;
  fsRefDir := REF_OUT_FILES_DIR;
  fsFileMask := '*.out';

  RunJcfCommandline;
end;

procedure TTestCommandline.TestFormatObfuscate;
begin
  fsJcfParams := ' -obfuscate -config=' + TEST_FILES_DIR + 'JCFObfuscateSettings.cfg -out -D ' + TEST_FILES_DIR;
  fsRefDir := OBS_OUT_FILES_DIR;
  fsFileMask := '*.obs';

  RunJcfCommandline;
end;

initialization
 TestFramework.RegisterTest(TTestCommandline.Suite);
end.
