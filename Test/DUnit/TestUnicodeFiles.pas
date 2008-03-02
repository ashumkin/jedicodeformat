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
  public
  published
    procedure TestReadAnsiFile;
    procedure TestReadUtf8File;
    procedure TestReadBeUcs2File;
    procedure TestReadLeUcs2File;

  end;

implementation

uses JcfUnicode;


procedure TTestUnicodeFiles.TestReadAnsiFile;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile('..\Test\TestCases\TestUnicode_ansi.pas', ls, le);

  Assert(Length(ls) > 0);
  Assert(le = e8Bit);
end;

procedure TTestUnicodeFiles.TestReadUtf8File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile('..\Test\TestCases\TestUnicode_utf8.pas', ls, le);

  Assert(Length(ls) > 0);
  Assert(le = e8Bit);
end;

procedure TTestUnicodeFiles.TestReadBeUcs2File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile('..\Test\TestCases\TestUnicode_be_ucs2.pas', ls, le);

  Assert(Length(ls) > 0);
  Assert(le = eUtf16BigEndian);
end;

procedure TTestUnicodeFiles.TestReadLeUcs2File;
var
  ls: WideString;
  le: TFileContentType;
begin
  ReadTextFile('..\Test\TestCases\TestUnicode_le_ucs2.pas', ls, le);

  Assert(Length(ls) > 0);
  Assert(le = eUtf16LittleEndian);
end;


initialization
  TestFramework.RegisterTest('Procs', TTestUnicodeFiles.Suite);
end.
