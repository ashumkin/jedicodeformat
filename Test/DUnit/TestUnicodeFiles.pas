unit TestUnicodeFiles;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestUnicodeFiles, released March 2008.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2008 Anthony Steele.
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

uses
  TestFrameWork;

type
  TTestUnicodeFiles = class(TTestCase)
  private
    procedure CheckStart(const ws: WideString);

    // these are not passing yet
    procedure TestReadBeUcs4File;
    procedure TestReadLeUcs4File;

  published
    procedure TestReadAnsiFile;
    procedure TestReadUtf8File;
    procedure TestReadBeUcs2File;
    procedure TestReadLeUcs2File;
  end;

implementation

uses
  SysUtils,
  JcfUnicode;

const
  ANSI_FILE = '..\Test\TestCases\TestUnicode_ansi.pas';
  UTF8_FILE = '..\Test\TestCases\TestUnicode_utf8.pas';
  BE_UCS2_FILE = '..\Test\TestCases\TestUnicode_be_ucs2.pas';
  LE_UCS2_FILE = '..\Test\TestCases\TestUnicode_le_ucs2.pas';
  BE_UCS4_FILE = '..\Test\TestCases\TestUnicode_be_ucs4.pas';
  LE_UCS4_FILE = '..\Test\TestCases\TestUnicode_le_ucs4.pas';


{
  Check that the file starts with "unit"
  Important to see that leading chars that identify the charset
  have not wandered in
}
procedure TTestUnicodeFiles.CheckStart(const ws: WideString);
var
  lsWStart: WideString;
  lsStart: string;
begin
  lsWStart := Copy(ws, 0, 4);
  lsStart := lsWStart;

  Assert(lsStart = 'unit', 'start was ' + lsStart);
end;

procedure TTestUnicodeFiles.TestReadAnsiFile;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(ANSI_FILE, ls, le);

  Assert(Length(ls) > 0);
  Assert(le = e8Bit);
  CheckStart(ls);
end;

procedure TTestUnicodeFiles.TestReadUtf8File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(UTF8_FILE, ls, le);

  Assert(Length(ls) > 0);
  Assert(le = eUtf8);
  CheckStart(ls);
end;

procedure TTestUnicodeFiles.TestReadBeUcs2File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(BE_UCS2_FILE, ls, le);

  Assert(Length(ls) > 0);
  Assert(le = eUtf16BigEndian);
  CheckStart(ls);
end;

procedure TTestUnicodeFiles.TestReadBeUcs4File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(BE_UCS4_FILE, ls, le);

  Assert(Length(ls) > 0);
  Assert(le = eUtf16BigEndian);
  CheckStart(ls);
end;

procedure TTestUnicodeFiles.TestReadLeUcs2File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(LE_UCS2_FILE, ls, le);

  Assert(Length(ls) > 0);
  Assert(le = eUtf16LittleEndian);
  CheckStart(ls);
end;


procedure TTestUnicodeFiles.TestReadLeUcs4File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(LE_UCS4_FILE, ls, le);

  Assert(Length(ls) > 0);
  Assert(le = eUtf16LittleEndian);
  CheckStart(ls);
end;

initialization
  TestFramework.RegisterTest('Procs', TTestUnicodeFiles.Suite);
end.
