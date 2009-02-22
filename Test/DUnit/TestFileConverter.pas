unit TestFileConverter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestFileConverter.pas, released December 2005.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2001 Anthony Steele.
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

{
  AFS 16 December 2005
  I introduced a bug in FileConverter in v 2.16
  My bad for not having test cases that would have caught it
  So, here they are }

uses
  SysUtils,
  TestFramework,
  FileConverter;

type
  TTestFileConverter = class(TTestCase)
  private
    fcFileCoverter: TFileConverter;
    fsFileName: string;
    fsBackFileName: string;
    fsOutFileName: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
  published
    procedure TestInPlace;
    procedure TestInPlaceWithBackup;
    procedure TestSeparateOutput;
  end;

implementation

uses
  Windows,
  JclFileUtils, JcfStringUtils,
  ConvertTypes;

const
  // a unit that needs formatting 
  UNIT_TEXT = 'unit TestUnit; interface implementation uses SysUtils; const aConst = 3; end.';

function TimesEqual(const prFileTime1, prFileTime2: TFileTime): boolean;
begin
  Result := (prFileTime1.dwLowDateTime = prFileTime2.dwLowDateTime) and
    (prFileTime1.dwHighDateTime = prFileTime2.dwHighDateTime);
end;

procedure TTestFileConverter.SetUp;
var
  lsBaseFileName: string;
begin
  inherited;

  fcFileCoverter := TFileConverter.Create;
  fcFileCoverter.SourceMode := fmSingleFile;
  fcFileCoverter.YesAll := True;

  // write the unit text to a file
  lsBaseFileName := PathGetTempPath + 'JcfTempUnit.';
  fsFileName := lsBaseFileName + 'pas';
  fsBackFileName := lsBaseFileName + 'bak';
  fsOutFileName := lsBaseFileName + 'out';

  if FileExists(fsFileName) then
    SysUtils.DeleteFile(fsFileName);
  if FileExists(fsBackFileName) then
    SysUtils.DeleteFile(fsBackFileName);
  if FileExists(fsOutFileName) then
    SysUtils.DeleteFile(fsOutFileName);

  StringToFile(fsFileName, UNIT_TEXT);

  fcFileCoverter.Input := fsFileName;
end;

procedure TTestFileConverter.TearDown;
begin
  inherited;

  FreeAndNil(fcFileCoverter);

  if FileExists(fsFileName) then
    SysUtils.DeleteFile(fsFileName);
  if FileExists(fsBackFileName) then
    SysUtils.DeleteFile(fsBackFileName);
  if FileExists(fsOutFileName) then
    SysUtils.DeleteFile(fsOutFileName);
end;

procedure TTestFileConverter.TestInPlace;
var
  lrFileTime1, lrFileTime2: TFileTime;
begin
  fcFileCoverter.BackupMode := cmInPlace;

  fcFileCoverter.Convert;
  Check(FileExists(fsFileName));

  { file is now formatted, so formatting it again should have no effect
    File time should not be updated }
  lrFileTime1 := GetFileLastWrite(fsFileName);
  fcFileCoverter.Convert;
  Check(FileExists(fsFileName));
  Check(not FileExists(fsBackFileName));
  Check(not FileExists(fsOutFileName));

  lrFileTime2 := GetFileLastWrite(fsFileName);
  Check(TimesEqual(lrFileTime1, lrFileTime2));
end;

procedure TTestFileConverter.TestInPlaceWithBackup;
var
  lrFileTime1, lrFileTime2: TFileTime;
begin
  fcFileCoverter.BackupMode := cmInPlaceWithBackup;

  fcFileCoverter.Convert;

  Check(FileExists(fsFileName));
  Check(FileExists(fsBackFileName));
  Check(not FileExists(fsOutFileName));


  // try again on the formatted file
  lrFileTime1 := GetFileLastWrite(fsFileName);
  fcFileCoverter.Convert;

  Check(FileExists(fsFileName));
  Check(FileExists(fsBackFileName));
  Check(not FileExists(fsOutFileName));

  lrFileTime2 := GetFileLastWrite(fsFileName);
  Check(TimesEqual(lrFileTime1, lrFileTime2));
end;

procedure TTestFileConverter.TestSeparateOutput;
begin
  fcFileCoverter.BackupMode := cmSeparateOutput;
  fcFileCoverter.Convert;

  Check(FileExists(fsFileName));
  Check(FileExists(fsOutFileName));
  Check(not FileExists(fsBackFileName));

  // move out to original and try again on the formatted file
  SysUtils.DeleteFile(fsFileName);
  RenameFile(fsOutFileName, fsFileName);

  fcFileCoverter.Convert;

  Check(FileExists(fsFileName));
  Check(FileExists(fsOutFileName));
  Check(not FileExists(fsBackFileName));
end;


initialization
  TestFramework.RegisterTest('Processes', TTestFileConverter.Suite);
end.
