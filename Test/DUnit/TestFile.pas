unit TestFile;

{ case class for a couple of testers that depend on
  format output files matching ref output }

interface

uses
 TestFrameWork;

type
  TTestFile = class(TTestCase)
  protected
    procedure TestFileContentsSame(const psFileName1, psFileName2: string);

end;

implementation

uses
  { delphi }
  SysUtils,
  { jcl }
  JclStrings;

procedure TTestFile.TestFileContentsSame(const psFileName1, psFileName2: string);
var
  lsFile1, lsFile2: string;
begin
  Check(FileExists(psFileName1), 'File ' + psFileName1 + ' does not exist');
  Check(FileExists(psFileName2), 'File ' + psFileName2 + ' does not exist');

  lsFile1 := FileToString(psFileName1);
  lsFile2 := FileToString(psFileName2);

  // first check lengths
  CheckEquals(Length(lsFile1), Length(lsFile2),
    'Files lengths differ, ' +
    IntToStr(Length(lsFile1)) + ' vs ' + IntToStr(Length(lsFile2)) +  ' ' +
    psFileName1 + ' and ' + psFileName2);


  // check contents the same
  if (lsFile1 <> lsFile2) then
    Fail('Files differ ' + psFileName1 + ' and ' + psFileName2);
end;

end.
