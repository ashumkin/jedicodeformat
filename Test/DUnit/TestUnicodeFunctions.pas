unit TestUnicodeFunctions;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestUnicodeFunctions, released March 2008.
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

uses
  TestFrameWork;

type
  TTestUnicodeFunctions = class(TTestCase)

  published
    procedure TestWideCharIsReturnSuccess;
    procedure TestWideCharIsReturnFail;

    procedure TestWideCharIsDigitSuccess;
    procedure TestWideCharIsDigitFail;

    procedure TestWideCharIsAlphaSuccess;
    procedure TestWideCharIsAlphaFail;

    procedure TestWideCharIsAlphaNumSuccess;
    procedure TestWideCharIsAlphaNumFail;

    procedure TestWideCharIsPuncCharSuccess;
    procedure TestWideCharIsPuncCharFail;

    procedure TestWideCharIsHexDigitDotSuccess;
    procedure TestWideCharIsHexDigitDotFail;

    procedure TestWideCharIsWordCharSuccess;
    procedure TestWideCharIsWordCharFail;

    procedure TestWideCharIsWhiteSpaceNoReturnSuccess;
    procedure TestWideCharIsWhiteSpaceNoReturnFail;

    procedure TestStringRepeat;
  end;

implementation

uses
  JcfUnicode;

procedure TTestUnicodeFunctions.TestStringRepeat;
begin
  CheckTrue('' = WideStringRepeat('A', 0));
  CheckTrue('A' = WideStringRepeat('A', 1));
  CheckTrue('AA' = WideStringRepeat('A', 2));
  CheckTrue('AAA' = WideStringRepeat('A', 3));

  CheckTrue('' = WideStringRepeat('Foo', 0));
  CheckTrue('Foo' = WideStringRepeat('Foo', 1));
  CheckTrue('FooFoo' = WideStringRepeat('Foo', 2));

end;

procedure TTestUnicodeFunctions.TestWideCharIsAlphaFail;
begin
  CheckFalse(WideCharIsAlpha('1'));
  CheckFalse(WideCharIsAlpha(' '));
  CheckFalse(WideCharIsAlpha('£'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsAlphaNumFail;
begin
  CheckFalse(WideCharIsAlphaNum(' '));
  CheckFalse(WideCharIsAlphaNum('£'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsAlphaNumSuccess;
begin
  CheckTrue(WideCharIsAlphaNum('a'));
  CheckTrue(WideCharIsAlphaNum('b'));
  CheckTrue(WideCharIsAlphaNum('f'));
  CheckTrue(WideCharIsAlphaNum('t'));
  CheckTrue(WideCharIsAlphaNum('y'));
  CheckTrue(WideCharIsAlphaNum('z'));
  CheckTrue(WideCharIsAlphaNum('1'));
  CheckTrue(WideCharIsAlphaNum('2'));
  CheckTrue(WideCharIsAlphaNum('2'));
  CheckTrue(WideCharIsAlphaNum('3'));
  CheckTrue(WideCharIsAlphaNum('4'));
  CheckTrue(WideCharIsAlphaNum('5'));
  CheckTrue(WideCharIsAlphaNum('6'));
  CheckTrue(WideCharIsAlphaNum('7'));
  CheckTrue(WideCharIsAlphaNum('8'));
  CheckTrue(WideCharIsAlphaNum('9'));
  CheckTrue(WideCharIsAlphaNum('0'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsAlphaSuccess;
begin
  CheckTrue(WideCharIsAlpha('a'));
  CheckTrue(WideCharIsAlpha('b'));
  CheckTrue(WideCharIsAlpha('f'));
  CheckTrue(WideCharIsAlpha('t'));
  CheckTrue(WideCharIsAlpha('y'));
  CheckTrue(WideCharIsAlpha('z'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsDigitFail;
begin
  CheckFalse(WideCharIsDigit('a'));
  CheckFalse(WideCharIsDigit('z'));
  CheckFalse(WideCharIsDigit('!'));
  CheckFalse(WideCharIsDigit('*'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsDigitSuccess;
begin
  CheckTrue(WideCharIsDigit('0'));
  CheckTrue(WideCharIsDigit('1'));
  CheckTrue(WideCharIsDigit('2'));
  CheckTrue(WideCharIsDigit('3'));
  CheckTrue(WideCharIsDigit('4'));
  CheckTrue(WideCharIsDigit('5'));
  CheckTrue(WideCharIsDigit('6'));
  CheckTrue(WideCharIsDigit('7'));
  CheckTrue(WideCharIsDigit('8'));
  CheckTrue(WideCharIsDigit('9'));
  CheckTrue(WideCharIsDigit('0'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsHexDigitDotSuccess;
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
  CheckTrue(WideCharIsHexDigitDot('A'));
  CheckTrue(WideCharIsHexDigitDot('B'));
  CheckTrue(WideCharIsHexDigitDot('C'));
  CheckTrue(WideCharIsHexDigitDot('D'));
  CheckTrue(WideCharIsHexDigitDot('E'));
  CheckTrue(WideCharIsHexDigitDot('F'));
  CheckTrue(WideCharIsHexDigitDot('.'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsHexDigitDotFail;
begin
  CheckFalse(WideCharIsHexDigitDot('G'));
  CheckFalse(WideCharIsHexDigitDot(','));
  CheckFalse(WideCharIsHexDigitDot('!'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsPuncCharFail;
begin
  CheckFalse(WideCharIsPuncChar('0'));
  CheckFalse(WideCharIsPuncChar('a'));
  CheckFalse(WideCharIsPuncChar('b'));
  CheckFalse(WideCharIsPuncChar('l'));
  CheckFalse(WideCharIsPuncChar('o'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsPuncCharSuccess;
begin
  CheckTrue(WideCharIsPuncChar('.'));
  CheckTrue(WideCharIsPuncChar(','));
  CheckTrue(WideCharIsPuncChar(';'));
  CheckTrue(WideCharIsPuncChar('+'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsReturnFail;
begin
  CheckFalse(WideCharIsReturn('!'));
  CheckFalse(WideCharIsReturn(' '));
  CheckFalse(WideCharIsReturn('r'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsReturnSuccess;
begin
  CheckTrue(WideCharIsReturn(#13));
  CheckTrue(WideCharIsReturn(#10));
end;

procedure TTestUnicodeFunctions.TestWideCharIsWhiteSpaceNoReturnFail;
begin
  CheckFalse(WideCharIsWhiteSpaceNoReturn('a'));
  CheckFalse(WideCharIsWhiteSpaceNoReturn('_'));
  CheckFalse(WideCharIsWhiteSpaceNoReturn(#10));
  CheckFalse(WideCharIsWhiteSpaceNoReturn(#13));
end;

procedure TTestUnicodeFunctions.TestWideCharIsWhiteSpaceNoReturnSuccess;
begin
  CheckTrue(WideCharIsWhiteSpaceNoReturn(' '));
  CheckTrue(WideCharIsWhiteSpaceNoReturn(char(3)));
end;

procedure TTestUnicodeFunctions.TestWideCharIsWordCharFail;
begin
  CheckFalse(WideCharIsWordChar('!'));
  CheckFalse(WideCharIsWordChar('*'));
  CheckFalse(WideCharIsWordChar('1'));
  CheckFalse(WideCharIsWordChar('2'));
  CheckFalse(WideCharIsWordChar('0'));
end;

procedure TTestUnicodeFunctions.TestWideCharIsWordCharSuccess;
begin
  CheckTrue(WideCharIsWordChar('a'));
  CheckTrue(WideCharIsWordChar('b'));
  CheckTrue(WideCharIsWordChar('y'));
  CheckTrue(WideCharIsWordChar('z'));
  CheckTrue(WideCharIsWordChar('_'));
end;

initialization
  TestFramework.RegisterTest('Procs', TTestUnicodeFunctions.Suite);
end.
