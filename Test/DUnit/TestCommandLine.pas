unit TestCommandLine;

{ AFS 26 April 03
  test the jcf.exe commandline }

interface

uses
 TestFrameWork, TestConstants;

type
  TTestCommandline = class(TTestCase)
  private
    procedure CompareFileToRef(const psFileName: string);
  published
    procedure TestFormatClarify;

end;

implementation

uses
  { delphi }
  Classes, SysUtils, Dialogs,
  { jcl }
  JclShell, JclFileUtils;

procedure GetOutFiles(const pcStrings: TStrings);
begin
  pcStrings.Clear;
  BuildFileList(TEST_FILES_DIR + '*.out', faAnyFile, pcStrings);
end;

procedure TTestCommandline.CompareFileToRef(const psFileName: string);
begin
  ShowMessage(psFileName);
end;


procedure TTestCommandline.TestFormatClarify;
var
  lsJcfExe: string;
  lsFiles: TStringList;
  liLoop: integer;
  lbRes: boolean;
begin
  lsFiles := TStringList.Create;
  try

    // delete the output files
    GetOutFiles(lsFiles);

    if lsFiles.Count > 0 then
    begin
      for liLoop := 0 to lsFiles.Count - 1 do
        DeleteFile(TEST_FILES_DIR + lsFiles[liLoop]);

      // should be none left
      GetOutFiles(lsFiles);
    end;

    CheckEquals(0, lsFiles.Count);


    // build them again
    lsJcfExe := EXE_FILES_DIR + 'jcf.exe';
    Check(FileExists(lsJcfExe));

    lbRes := ShellExecAndWait(lsJcfExe,
      ' -config=' + TEST_FILES_DIR +
      'JCFTestSettings.cfg -out -D ' + TEST_FILES_DIR);
    Check(lbRes);

    // should be back
    GetOutFiles(lsFiles);
    CheckEquals(73, lsFiles.Count);

    // for each, compare to the reference versions
    GetOutFiles(lsFiles);

    for liLoop := 0 to lsFiles.Count - 1 do
      CompareFileToRef(lsFiles[liLoop]);

  finally
    lsFiles.Free;
  end;

end;

initialization
 TestFramework.RegisterTest(TTestCommandline.Suite);
end.
