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
  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfStringUtils;


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
