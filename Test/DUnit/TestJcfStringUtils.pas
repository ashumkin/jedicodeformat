unit TestJcfStringUtils;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestJcfStringUtils, released December 2008.
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
  Tests on the Jcf String Utils unit }

uses
  TestFrameWork;

type
  TTestJcfStringUtils  = class(TTestCase)

  published
    procedure TestWideStringReplaceBlank;
    procedure TestWideStringReplaceMismatch;
    procedure TestWideStringReplaceMatch;
    procedure TestWideStringReplaceTwice;
    procedure TestWideStringReplaceTwiceReplaceAll;

    procedure TestStrSearchEmpty1;
    procedure TestStrSearchEmpty2;

    procedure TestStrSearchFound1;
    procedure TestStrSearchFound2;

    procedure TestStrSearchNotFound1;
    procedure TestStrSearchNotFound2;

    procedure TestStrPadLeftNumber;
    procedure TestStrPadLeftFiveChars;

    procedure TestStrIPos_Misses;
    procedure TestStrIPos_Match1;
    procedure TestStrIPos_Match4;
  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfStringUtils;


procedure TTestJcfStringUtils.TestStrPadLeftNumber;
begin
  // pad on the left with zeros until it's 3 chars long
  CheckEquals('000', StrPadLeft('', 3, '0'));
  CheckEquals('001', StrPadLeft('1', 3, '0'));
  CheckEquals('011', StrPadLeft('11', 3, '0'));
  CheckEquals('111', StrPadLeft('111', 3, '0'));

  // if it's longer already, left unchanged
  CheckEquals('1111', StrPadLeft('1111', 3, '0'));
end;

procedure TTestJcfStringUtils.TestStrIPos_Misses;
begin
  // should all return "not found"
  CheckEquals(0, StrIPos('foo', ''));
  CheckEquals(0, StrIPos('foo', 'bar'));
  CheckEquals(0, StrIPos('foo', 'goo'));
end;

procedure TTestJcfStringUtils.TestStrIPos_Match1;
begin
  // should all match at pos 1
  CheckEquals(1, StrIPos('foo', 'foo'));
  CheckEquals(1, StrIPos('foo', 'Foo'));
  CheckEquals(1, StrIPos('foo', 'FOO'));

  CheckEquals(1, StrIPos('Foo', 'foo'));
  CheckEquals(1, StrIPos('Foo', 'Foo'));
  CheckEquals(1, StrIPos('Foo', 'FOO'));

  CheckEquals(1, StrIPos('FOO', 'foo'));
  CheckEquals(1, StrIPos('FOO', 'Foo'));
  CheckEquals(1, StrIPos('FOO', 'FOO'));
end;

procedure TTestJcfStringUtils.TestStrIPos_Match4;
begin
  // should all match at pos 4
  CheckEquals(4, StrIPos('foo', '123foo'));
  CheckEquals(4, StrIPos('foo', '123Foo'));
  CheckEquals(4, StrIPos('foo', '123FOO'));

  CheckEquals(4, StrIPos('Foo', '123foo'));
  CheckEquals(4, StrIPos('Foo', '123Foo'));
  CheckEquals(4, StrIPos('Foo', '123FOO'));

  CheckEquals(4, StrIPos('FOO', '123foo'));
  CheckEquals(4, StrIPos('FOO', '123Foo'));
  CheckEquals(4, StrIPos('FOO', '123FOO'));
end;

procedure TTestJcfStringUtils.TestStrPadLeftFiveChars;
begin
  // pad on the left with zeros until it's 5 chars long
  CheckEquals('00000', StrPadLeft('', 5, '0'));
  CheckEquals('00001', StrPadLeft('1', 5, '0'));
  CheckEquals('00011', StrPadLeft('11', 5, '0'));
  CheckEquals('00111', StrPadLeft('111', 5, '0'));
end;


procedure TTestJcfStringUtils.TestStrSearchEmpty1;
var
  liIndex: integer;
begin
  liIndex := StrSearch('foo', '');
  CheckEquals(0, liIndex);
end;

procedure TTestJcfStringUtils.TestStrSearchEmpty2;
var
  liIndex: integer;
begin
  liIndex := StrSearch('foo', '', 5);
  CheckEquals(0, liIndex);
end;

procedure TTestJcfStringUtils.TestStrSearchFound1;
var
  liIndex: integer;
begin
  liIndex := StrSearch('foo', 'more food please');
  CheckEquals(6, liIndex);
end;

procedure TTestJcfStringUtils.TestStrSearchFound2;
var
  liIndex: integer;
begin
  liIndex := StrSearch('foo', 'more food please', 9);
  CheckEquals(0, liIndex);
end;

procedure TTestJcfStringUtils.TestStrSearchNotFound1;
var
  liIndex: integer;
begin
  liIndex := StrSearch('boo', 'more food please');
  CheckEquals(0, liIndex);
end;

procedure TTestJcfStringUtils.TestStrSearchNotFound2;
var
  liIndex: integer;
begin
  liIndex := StrSearch('boo', 'more food please', 9);
  CheckEquals(0, liIndex);
end;

procedure TTestJcfStringUtils.TestWideStringReplaceBlank;
var
  lsIn, lsOut: WideString;
begin
  lsIn := '';

  // replace on an empty string should return an empty string
  lsOut := WideStringReplace(lsIn, 'foo', 'bar', []);

  CheckEquals('', lsOut);
end;

procedure TTestJcfStringUtils.TestWideStringReplaceMismatch;
var
  lsIn, lsOut: WideString;
begin
  lsIn := 'fish';

  // no pattern match, input string should not be changed
  lsOut := WideStringReplace(lsIn, 'foo', 'bar', []);

  CheckEquals('fish', lsOut);
end;

procedure TTestJcfStringUtils.TestWideStringReplaceMatch;
var
  lsIn, lsOut: WideString;
begin
  lsIn := 'foo';

  // input string should be replaced
  lsOut := WideStringReplace(lsIn, 'foo', 'bar', []);

  CheckEquals('bar', lsOut);
end;

procedure TTestJcfStringUtils.TestWideStringReplaceTwice;
var
  lsIn, lsOut: WideString;
begin
  lsIn := 'foofoo';

  // input string should be replaced once
  lsOut := WideStringReplace(lsIn, 'foo', 'bar', []);

  CheckEquals('barfoo', lsOut);
end;

procedure TTestJcfStringUtils.TestWideStringReplaceTwiceReplaceAll;
var
  lsIn, lsOut: WideString;
begin
  lsIn := 'foofoo';

  // input string should be replaced twice
  lsOut := WideStringReplace(lsIn, 'foo', 'bar', [rfReplaceAll]);

  CheckEquals('barbar', lsOut);
end;

initialization
  TestFramework.RegisterTest('Procs', TTestJcfStringUtils.Suite);

end.
