unit TestJcfUnicode;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestJcfUnicode, released December 2008.
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

{ AFS 11 Dec 2008
  Tests on the Jcf Unicode functions }

uses
  TestFrameWork;

type
  TTestJcfUnicode = class(TTestCase)
  private

  published

    // test WideCharIsHexDigitDot
    procedure TestWideCharIsHexDigitDotSucessDigits;
    procedure TestWideCharIsHexDigitDotSucessUppercase;
    procedure TestWideCharIsHexDigitDotSucessLowercase;
    procedure TestWideCharIsHexDigitDotFail;
  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfUnicode;

procedure TTestJcfUnicode.TestWideCharIsHexDigitDotSucessDigits;
begin
  CheckTrue(WideCharIsHexDigitDot('0'));
  CheckTrue(WideCharIsHexDigitDot('1'));
  CheckTrue(WideCharIsHexDigitDot('2'));
  CheckTrue(WideCharIsHexDigitDot('3'));
  CheckTrue(WideCharIsHexDigitDot('4'));
  CheckTrue(WideCharIsHexDigitDot('5'));
  CheckTrue(WideCharIsHexDigitDot('6'));
  CheckTrue(WideCharIsHexDigitDot('7'));
  CheckTrue(WideCharIsHexDigitDot('8'));
  CheckTrue(WideCharIsHexDigitDot('9'));
  CheckTrue(WideCharIsHexDigitDot('0'));
  CheckTrue(WideCharIsHexDigitDot('.'));
end;

procedure TTestJcfUnicode.TestWideCharIsHexDigitDotSucessUppercase;
begin
  CheckTrue(WideCharIsHexDigitDot('A'));
  CheckTrue(WideCharIsHexDigitDot('B'));
  CheckTrue(WideCharIsHexDigitDot('C'));
  CheckTrue(WideCharIsHexDigitDot('D'));
  CheckTrue(WideCharIsHexDigitDot('E'));
  CheckTrue(WideCharIsHexDigitDot('F'));
end;

procedure TTestJcfUnicode.TestWideCharIsHexDigitDotFail;
begin
  CheckFalse(WideCharIsHexDigitDot('g'));
  CheckFalse(WideCharIsHexDigitDot('h'));
  CheckFalse(WideCharIsHexDigitDot('z'));
  CheckFalse(WideCharIsHexDigitDot(' '));
  CheckFalse(WideCharIsHexDigitDot('?'));
  CheckFalse(WideCharIsHexDigitDot('$'));
  CheckFalse(WideCharIsHexDigitDot(#0));
  CheckFalse(WideCharIsHexDigitDot(#123));
end;


procedure TTestJcfUnicode.TestWideCharIsHexDigitDotSucessLowercase;
begin
  CheckTrue(WideCharIsHexDigitDot('a'));
  CheckTrue(WideCharIsHexDigitDot('b'));
  CheckTrue(WideCharIsHexDigitDot('c'));
  CheckTrue(WideCharIsHexDigitDot('d'));
  CheckTrue(WideCharIsHexDigitDot('e'));
  CheckTrue(WideCharIsHexDigitDot('f'));
end;

initialization
  TestFramework.RegisterTest('Procs', TTestJcfUnicode.Suite);

end.
