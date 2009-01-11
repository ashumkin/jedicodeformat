unit TestFile;

{ case class for a couple of testers that depend on
  format output files matching ref output }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestFile, released May 2003.
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

uses
  TestFrameWork;

type
  TTestFile = class(TTestCase)
  protected
    procedure SetUp; override;

    procedure TestFileContentsSame(const psFileName1, psFileName2: string);
  end;

implementation

uses
  { delphi }
  SysUtils,
  { jcf }
  JcfStringUtils,
  JcfUnicodeFiles, TestConstants;

procedure TTestFile.Setup;
begin
  inherited;

  InitTestSettings;
end;

procedure TTestFile.TestFileContentsSame(const psFileName1, psFileName2: string);
var
  lsFile1, lsFile2: WideString;
  leFileType1, leFileType2: TFileContentType;
begin
  Check(FileExists(psFileName1), 'File ' + psFileName1 + ' does not exist');
  Check(FileExists(psFileName2), 'File ' + psFileName2 + ' does not exist');

  ReadTextFile(psFileName1, lsFile1, leFileType1);
  ReadTextFile(psFileName2, lsFile2, leFileType2);

  // check types
  Check(leFileType1 = leFileType2, 'File types differ');

  // check lengths
  CheckEquals(Length(lsFile1), Length(lsFile2),
    'Files lengths differ, ' +
    IntToStr(Length(lsFile1)) + ' vs ' + IntToStr(Length(lsFile2)) + ' ' +
    psFileName1 + ' and ' + psFileName2);

  // check contents the same
  if (lsFile1 <> lsFile2) then
    Fail('Files differ ' + psFileName1 + ' and ' + psFileName2);
end;

end.
