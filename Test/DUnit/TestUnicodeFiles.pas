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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{
 Test reading and writing files in unicode formats
 8-bit: Ansi, UTF-8
 16-bit: little-endian and big-endian
 32-bit: little-endian and big-endian
}

uses
  TestFrameWork;

type
  TTestUnicodeFiles = class(TTestCase)
  private
    procedure CheckStart(const ws: WideString);
    procedure CheckFileSame(const fileName1: string; const fileName2: string);
    procedure CheckReadWriteFile(const psFileName: string);


  published
    procedure TestAnsiFileSameAsFileToString;

    procedure TestTypeAnsiFile;
    procedure TestTypeUtf8File;

    procedure TestTypeBeUcs2File;
    procedure TestTypeLeUcs2File;

    procedure TestTypeBeUcs4File;
    procedure TestTypeLeUcs4File;

    procedure TestReadAnsiFile;
    procedure TestReadUtf8File;

    procedure TestReadBeUcs2File;
    procedure TestReadLeUcs2File;

    procedure TestReadBeUcs4File;
    procedure TestReadLeUcs4File;

    procedure TestReadWriteAnsiFile;
    procedure TestReadWriteUtf8File;

    procedure TestReadWriteBeUcs2File;
    procedure TestReadWriteLeUcs2File;

    procedure TestReadWriteBeUcs4File;
    procedure TestReadWriteLeUcs4File;
  end;

implementation

uses
  { Delphi }
  SysUtils,
  Windows,
  { local }
  JcfStringUtils,
  JcfUnicodeFiles;

const
  ANSI_FILE = '..\..\Test\TestCases\TestUnicode_ansi.pas';
  UTF8_FILE = '..\..\Test\TestCases\TestUnicode_utf8.pas';
  BE_UCS2_FILE = '..\..\Test\TestCases\TestUnicode_be_ucs2.pas';
  LE_UCS2_FILE = '..\..\Test\TestCases\TestUnicode_le_ucs2.pas';
  BE_UCS4_FILE = '..\..\Test\TestCases\TestUnicode_be_ucs4.pas';
  LE_UCS4_FILE = '..\..\Test\TestCases\TestUnicode_le_ucs4.pas';

  TEMP_FILE = '..\..\Test\TestCases\TempUnicodeFile.tmp';

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

  CheckEquals('unit', lsStart, 'Start text incorrect');
end;

procedure TTestUnicodeFiles.CheckFileSame(const fileName1, fileName2: string);
var
  ls1: WideString;
  le1: TFileContentType;
  ls2: WideString;
  le2: TFileContentType;
  liLoop: integer;
begin
  ReadTextFile(fileName1, ls1, le1);
  ReadTextFile(fileName2, ls2, le2);

  Check(le1 = le2, 'file types differ: ' + IntToStr(Ord(le1)) + ' vs ' + IntToStr(Ord(le2)));
  CheckEquals(Length(ls1), Length(ls2), 'file lengths differ');

  for liLoop := 1 to Length(ls1) do
  begin
    CheckEquals(ls1[liLoop], ls2[liLoop], ' Diffrence at ' + IntToStr(liLoop));
  end;
    
end;


procedure TTestUnicodeFiles.CheckReadWriteFile(const psFileName: string);
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(psFileName, ls, le);

  Check(Length(ls) > 0);

  WriteTextFile(TEMP_FILE, ls, le);
  try
    CheckFileSame(psFileName, TEMP_FILE);
  finally
    DeleteFile(TEMP_FILE);
  end;
end;

// test types

procedure TTestUnicodeFiles.TestTypeAnsiFile;
var
  le: TFileContentType;
begin
  le := TypeOfTextFile(ANSI_FILE);

  Check(le = e8Bit);
end;

procedure TTestUnicodeFiles.TestTypeBeUcs2File;
var
  le: TFileContentType;
begin
  le := TypeOfTextFile(BE_UCS2_FILE);

  Check(le = eUtf16BigEndian);
end;

procedure TTestUnicodeFiles.TestTypeBeUcs4File;
var
  le: TFileContentType;
begin
  le := TypeOfTextFile(BE_UCS4_FILE);

  Check(le = eUtf32BigEndian);
end;

procedure TTestUnicodeFiles.TestTypeLeUcs2File;
var
  le: TFileContentType;
begin
  le := TypeOfTextFile(LE_UCS2_FILE);

  Check(le = eUtf16LittleEndian);
end;

procedure TTestUnicodeFiles.TestTypeLeUcs4File;
var
  le: TFileContentType;
begin
  le := TypeOfTextFile(LE_UCS4_FILE);

  Check(le = eUtf32LittleEndian);
end;

procedure TTestUnicodeFiles.TestTypeUtf8File;
var
  le: TFileContentType;
begin
  le := TypeOfTextFile(UTF8_FILE);

  Check(le = eUtf8);
end;



/// test reading

procedure TTestUnicodeFiles.TestAnsiFileSameAsFileToString;
var
  lsReadTextFile: WideString;
  lsNarrow: string;
  le: TFileContentType;

  lsFileToString: string;
  lwsFileToString: WideString;
begin
  ReadTextFile(ANSI_FILE, lsReadTextFile, le);

  lsFileToString := string(FileToString(ANSI_FILE));
  lwsFileToString := lsFileToString;

  Check(lwsFileToString = lsReadTextFile);

  lsNarrow := lsReadTextFile;
  Check(lsFileToString = lsNarrow);
end;

procedure TTestUnicodeFiles.TestReadAnsiFile;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(ANSI_FILE, ls, le);

  Check(Length(ls) > 0);
  Check(le = e8Bit);
  CheckStart(ls);
end;

procedure TTestUnicodeFiles.TestReadUtf8File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(UTF8_FILE, ls, le);

  Check(Length(ls) > 0);
  Check(le = eUtf8);
  CheckStart(ls);
end;


procedure TTestUnicodeFiles.TestReadBeUcs2File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(BE_UCS2_FILE, ls, le);

  Check(Length(ls) > 0);
  Assert(le = eUtf16BigEndian);
  CheckStart(ls);
end;

procedure TTestUnicodeFiles.TestReadBeUcs4File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(BE_UCS4_FILE, ls, le);

  Check(Length(ls) > 0);
  Check(le = eUtf32BigEndian);
  CheckStart(ls);
end;

procedure TTestUnicodeFiles.TestReadLeUcs2File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(LE_UCS2_FILE, ls, le);

  Check(Length(ls) > 0);
  Check(le = eUtf16LittleEndian);
  CheckStart(ls);
end;


procedure TTestUnicodeFiles.TestReadLeUcs4File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile(LE_UCS4_FILE, ls, le);

  Check(Length(ls) > 0);
  Check(le = eUtf32LittleEndian);
  CheckStart(ls);
end;

// test writing

procedure TTestUnicodeFiles.TestReadWriteAnsiFile;
begin
  CheckReadWriteFile(ANSI_FILE);
end;

procedure TTestUnicodeFiles.TestReadWriteUtf8File;
begin
  CheckReadWriteFile(UTF8_FILE);
end;

procedure TTestUnicodeFiles.TestReadWriteBeUcs2File;
begin
  CheckReadWriteFile(BE_UCS2_FILE);
end;

procedure TTestUnicodeFiles.TestReadWriteLeUcs2File;
begin
  CheckReadWriteFile(LE_UCS2_FILE);
end;

procedure TTestUnicodeFiles.TestReadWriteLeUcs4File;
begin
  CheckReadWriteFile(LE_UCS4_FILE);
end;

procedure TTestUnicodeFiles.TestReadWriteBeUcs4File;
begin
  CheckReadWriteFile(BE_UCS4_FILE);
end;

initialization
  TestFramework.RegisterTest('Procs', TTestUnicodeFiles.Suite);
end.
